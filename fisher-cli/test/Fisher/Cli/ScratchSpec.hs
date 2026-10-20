module Fisher.Cli.ScratchSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (bracket)
import Data.Aeson (Value (..))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime)
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, emptyPermissions, findExecutable
  , getModificationTime, listDirectory, setModificationTime, setOwnerExecutable, setOwnerReadable, setPermissions )
import System.FileLock (SharedExclusive (..), lockFile, unlockFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (readFile')
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Cli.Scratch
import Fisher.ProjectCommand (CreateRequest (..))
import Fisher.Template (findTemplate, templateRevision)
import Fisher.Template.Run (Event (..), writeEvent)

-- | HOME and FISHER_HOME in a temporary directory, so the scratch cache and
-- the built-ins are this test's own.
withHomes :: (FilePath -> IO a) -> IO a
withHomes k = withSystemTempDirectory "fisher-scratch" $ \d0 -> do
  d <- canonicalizePath d0
  bracket save restore (const (mapM_ (uncurry setEnv) (kvs d) >> k d))
  where
    kvs d = [("HOME", d), ("FISHER_HOME", d </> "fh")]
    save = mapM (\n -> (,) n <$> lookupEnv n) ["HOME", "FISHER_HOME"]
    restore = mapM_ (\(n, v) -> maybe (unsetEnv n) (setEnv n) v)

-- | A create that writes what a built-in would (a sample under the project
-- directory, the manifest's template section, a ready event) after a short
-- wait, and counts its calls.
fakeCreate :: IORef [CreateRequest] -> Bool -> CreateRequest -> IO (Either Text FilePath)
fakeCreate calls ok r = do
  modifyIORef' calls (<> [r])
  threadDelay 200000
  let target = crTarget r
      project = maybe "Scratch" T.unpack (lookup "project" (crVars r))
      sample = project </> "Sample.lean"
  createDirectoryIfMissing True (target </> project)
  writeFile (target </> sample) "import Mathlib\n\ntheorem fisher_sample (a b : Nat) : a + b = b + a := by omega\n"
  createDirectoryIfMissing True (target </> ".fisher")
  writeFile (target </> ".fisher" </> "project.yaml")
    ("version: 1\ntemplate:\n  name: x\n  sample: " <> sample <> "\n  variables:\n    mathlib: v4.24.0\n    project: " <> project <> "\n")
  if ok
    then writeEvent target Ready >> pure (Right target)
    else writeEvent target (Failed "boom") >> pure (Left "step build failed (exit 1); see .fisher/create.log")

deps :: IORef [CreateRequest] -> Bool -> ScratchDeps
deps calls ok = ScratchDeps {sdCreate = fakeCreate calls ok, sdBuild = noBuild, sdConfig = Null, sdOs = "other", sdSay = const (pure ())}
  where
    noBuild _ p = pure (Left ("unexpected build of " <> T.pack p))

req :: ScratchRequest
req = ScratchRequest {srTemplate = "lean-mathlib", srVars = [], srFile = Nothing, srStatement = Nothing, srYes = False, srTrustRevision = Nothing}

spec :: Spec
spec = do
  describe "baseKey" $ do
    it "is 16 hex, stable, and changes with a variable or the revision" $ withHomes $ \_ -> do
      Right t <- findTemplate Null "lean"
      let k = baseKey t "rev1" [("lean", "v4.24.0"), ("project", "Scratch")]
      T.length k `shouldBe` 16
      T.all (`elem` ("0123456789abcdef" :: String)) k `shouldBe` True
      baseKey t "rev1" [("project", "Scratch"), ("lean", "v4.24.0")] `shouldBe` k
      baseKey t "rev1" [("lean", "v4.25.0"), ("project", "Scratch")] `shouldNotBe` k
      baseKey t "rev2" [("lean", "v4.24.0"), ("project", "Scratch")] `shouldNotBe` k

  describe "cloneCommand" $ do
    it "clones by copy-on-write where it can" $ do
      cloneCommand "darwin" "/b" "/t" `shouldBe` ["cp", "-c", "-R", "-p", "/b", "/t"]
      cloneCommand "linux" "/b" "/t" `shouldBe` ["cp", "--reflink=auto", "-R", "-p", "/b", "/t"]
      cloneCommand "freebsd" "/b" "/t" `shouldBe` ["cp", "-R", "-p", "/b", "/t"]

    it "keeps the base's file times in the clone" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right _ <- scratchWith (deps calls True) req
      [c] <- readIORef calls
      let sample = crTarget c </> "Scratch" </> "Sample.lean"
      now <- getCurrentTime
      setModificationTime sample (addUTCTime (-3600) now)
      Right (r, _) <- scratchWith (deps calls True) req
      t0 <- getModificationTime sample
      getModificationTime (r </> "Scratch" </> "Sample.lean") `shouldReturn` t0

  describe "scratch" $ do
    it "builds the base once, then only clones" $ withHomes $ \home -> do
      calls <- newIORef []
      Right (r1, f1) <- scratchWith (deps calls True) req
      Right (r2, f2) <- scratchWith (deps calls True) req
      length <$> readIORef calls `shouldReturn` 1
      r1 `shouldNotBe` r2
      takeDirectory r1 `shouldBe` home </> ".cache" </> "fisher" </> "scratch"
      f1 `shouldBe` r1 </> "Scratch" </> "Scratch.lean"
      f2 `shouldBe` r2 </> "Scratch" </> "Scratch.lean"
      doesFileExist (r2 </> "Scratch" </> "Sample.lean") `shouldReturn` True
      doesFileExist (r2 </> ".fisher" </> "project.yaml") `shouldReturn` True
      [c] <- readIORef calls
      crTemplate c `shouldBe` Just "lean-mathlib"
      crBuild c `shouldBe` True
      takeDirectory (crTarget c) `shouldBe` home </> ".cache" </> "fisher" </> "scratch" </> "bases"
      -- the id: date, time and 4 random hex
      let i = takeFileName r1
      length i `shouldBe` 20
      take 1 (drop 8 i) `shouldBe` "-"
      take 1 (drop 15 i) `shouldBe` "-"

    it "uses the given variables in the base's key" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right _ <- scratchWith (deps calls True) req
      Right _ <- scratchWith (deps calls True) req {srVars = [("mathlib", "latest")]}
      length <$> readIORef calls `shouldReturn` 1
      Right _ <- scratchWith (deps calls True) req {srVars = [("mathlib", "v4.24.0")]}
      length <$> readIORef calls `shouldReturn` 2
      Right t <- findTemplate Null "lean-mathlib"
      rev <- templateRevision t
      cs <- readIORef calls
      map (takeFileName . crTarget) cs
        `shouldBe` map T.unpack [baseKey t rev [("mathlib", "latest"), ("project", "Scratch")], baseKey t rev [("mathlib", "v4.24.0"), ("project", "Scratch")]]

    it "passes --yes through to the base's create" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right _ <- scratchWith (deps calls True) req {srYes = True}
      map crYes <$> readIORef calls `shouldReturn` [True]

    it "passes --trust-revision through to the base's create, and parses it" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right _ <- scratchWith (deps calls True) req {srTrustRevision = Just "abc123"}
      map crTrustRevision <$> readIORef calls `shouldReturn` [Just "abc123"]
      calls2 <- newIORef []
      out <- newIORef []
      scratchMainWith (deps calls2 True) (\l -> modifyIORef' out (<> [l])) ["--template", "lean", "--trust-revision", "r1", "--statement", "True"] `shouldReturn` 0
      map crTrustRevision <$> readIORef calls2 `shouldReturn` [Just "r1"]
      scratchMainWith (deps calls2 True) (\l -> modifyIORef' out (<> [l])) ["--trust-revision"] `shouldReturn` 2

    it "writes a statement as scratch_goal, with Mathlib for lean-mathlib" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right (_, f) <- scratchWith (deps calls True) req {srStatement = Just "∀ n : ℕ, n * 1 = n"}
      readFile' f `shouldReturn` "import Mathlib\n\ntheorem scratch_goal :\n    ∀ n : ℕ, n * 1 = n\n    := by\n  sorry\n"

    it "trims the statement, and refuses an empty one or a whole theorem before building" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right (_, f) <- scratchWith (deps calls True) req {srStatement = Just "  True -- why not\n "}
      readFile' f `shouldReturn` "import Mathlib\n\ntheorem scratch_goal :\n    True -- why not\n    := by\n  sorry\n"
      n <- length <$> readIORef calls
      scratchWith (deps calls True) req {srStatement = Just "  \n "} `shouldReturn` Left "--statement is empty"
      scratchWith (deps calls True) req {srStatement = Just "theorem foo : True"} `shouldReturn` Left "give the statement only, without theorem NAME :"
      scratchWith (deps calls True) req {srStatement = Just " lemma foo : True"} `shouldReturn` Left "give the statement only, without theorem NAME :"
      length <$> readIORef calls `shouldReturn` n

    it "takes the imports from the whole header, past comments and options" $ withHomes $ \_ -> do
      calls <- newIORef []
      let commented r = fakeCreate calls True r >>= \x -> do
            writeFile (crTarget r </> "Scratch" </> "Sample.lean") $
              "/- Copyright.\r\n   Two lines. -/\r\n-- a note\r\n\r\nset_option autoImplicit false\r\nimport Mathlib.Tactic\r\n/-! doc -/ import Aesop\r\nimport Batteries\r\n\r\ntheorem fisher_sample : True := trivial\r\nimport Late\r\n"
            pure x
      Right (_, f) <- scratchWith (deps calls True) {sdCreate = commented} req {srStatement = Just "True"}
      readFile' f `shouldReturn` "import Mathlib.Tactic\nimport Aesop\nimport Batteries\n\ntheorem scratch_goal :\n    True\n    := by\n  sorry\n"

    it "writes a statement without an import for lean" $ withHomes $ \_ -> do
      calls <- newIORef []
      let plain r = fakeCreate calls True r >>= \x -> do
            writeFile (crTarget r </> "Scratch" </> "Sample.lean") "theorem fisher_sample (n : Nat) : n + 0 = n := by simp\n"
            pure x
      Right (_, f) <- scratchWith (deps calls True) {sdCreate = plain} req {srTemplate = "lean", srStatement = Just "1 = 1"}
      readFile' f `shouldReturn` "theorem scratch_goal :\n    1 = 1\n    := by\n  sorry\n"

    it "copies a FILE in next to the sample" $ withHomes $ \home -> do
      calls <- newIORef []
      writeFile (home </> "Mine.lean") "theorem mine : True := trivial\n"
      Right (r, f) <- scratchWith (deps calls True) req {srFile = Just (home </> "Mine.lean")}
      f `shouldBe` r </> "Scratch" </> "Mine.lean"
      readFile' f `shouldReturn` "theorem mine : True := trivial\n"

    it "refuses a FILE that does not exist, and a FILE with a statement, before building" $ withHomes $ \home -> do
      calls <- newIORef []
      scratchWith (deps calls True) req {srFile = Just (home </> "nope.lean")} >>= (`shouldSatisfy` isLeft)
      scratchWith (deps calls True) req {srFile = Just (home </> "nope.lean"), srStatement = Just "True"} >>= (`shouldSatisfy` isLeft)
      length <$> readIORef calls `shouldReturn` 0

    it "refuses an unknown template" $ withHomes $ \_ -> do
      calls <- newIORef []
      scratchWith (deps calls True) req {srTemplate = "nope"} >>= (`shouldSatisfy` isLeft)
      length <$> readIORef calls `shouldReturn` 0

    it "builds the base once for two first scratches at once, and both get clones" $ withHomes $ \home -> do
      calls <- newIORef []
      a <- newEmptyMVar
      b <- newEmptyMVar
      _ <- forkIO (scratchWith (deps calls True) req >>= putMVar a)
      _ <- forkIO (scratchWith (deps calls True) req >>= putMVar b)
      Right (ra, _) <- takeMVar a
      Right (rb, _) <- takeMVar b
      length <$> readIORef calls `shouldReturn` 1
      ra `shouldNotBe` rb
      doesFileExist (ra </> "Scratch" </> "Sample.lean") `shouldReturn` True
      doesFileExist (rb </> "Scratch" </> "Sample.lean") `shouldReturn` True
      [c] <- readIORef calls
      doesFileExist (crTarget c <> ".lock") `shouldReturn` True
      takeDirectory (crTarget c) `shouldBe` home </> ".cache" </> "fisher" </> "scratch" </> "bases"

    it "clones from a built base while another scratch holds its shared lock" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right _ <- scratchWith (deps calls True) req
      [c] <- readIORef calls
      l <- lockFile (crTarget c <> ".lock") Shared
      -- the lock goes after 3 s, so a clone that waits for it fails rather than hangs
      _ <- forkIO (threadDelay 3000000 >> unlockFile l)
      t0 <- getCurrentTime
      Right _ <- scratchWith (deps calls True) req
      t1 <- getCurrentTime
      diffUTCTime t1 t0 `shouldSatisfy` (< 2.5)

    it "runs two clones of one base at once" $ withHomes $ \home -> do
      calls <- newIORef []
      Right _ <- scratchWith (deps calls True) req
      let bin = home </> "slowbin"
      createDirectoryIfMissing True bin
      cp <- maybe "/bin/cp" id <$> findExecutable "cp"
      writeFile (bin </> "cp") ("#!/bin/sh\nsleep 1\nexec " <> cp <> " \"$@\"\n")
      setPermissions (bin </> "cp") (setOwnerExecutable True (setOwnerReadable True emptyPermissions))
      path <- maybe "" id <$> lookupEnv "PATH"
      bracket (setEnv "PATH" (bin <> ":" <> path)) (const (setEnv "PATH" path)) $ \_ -> do
        a <- newEmptyMVar
        b <- newEmptyMVar
        t0 <- getCurrentTime
        _ <- forkIO (scratchWith (deps calls True) req >>= putMVar a)
        _ <- forkIO (scratchWith (deps calls True) req >>= putMVar b)
        Right _ <- takeMVar a
        Right _ <- takeMVar b
        t1 <- getCurrentTime
        diffUTCTime t1 t0 `shouldSatisfy` (< 1.8)
      length <$> readIORef calls `shouldReturn` 1

    it "removes a base whose build fails and returns the error" $ withHomes $ \_ -> do
      calls <- newIORef []
      scratchWith (deps calls False) req `shouldReturn` Left "step build failed (exit 1); see .fisher/create.log"
      [c] <- readIORef calls
      doesDirectoryExist (crTarget c) `shouldReturn` False
      -- the next one builds again
      Right _ <- scratchWith (deps calls True) req
      length <$> readIORef calls `shouldReturn` 2

    it "builds again over a base left half built" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right (r, _) <- scratchWith (deps calls True) req
      [c] <- readIORef calls
      writeFile (crTarget c </> ".fisher" </> "create.jsonl") "{\"event\":\"step start\",\"step\":\"cache\"}\n"
      Right _ <- scratchWith (deps calls True) req
      length <$> readIORef calls `shouldReturn` 2
      doesDirectoryExist r `shouldReturn` True

    it "keeps a base whose build was interrupted and resumes it with a build next time" $ withHomes $ \_ -> do
      calls <- newIORef []
      builds <- newIORef []
      let interrupted r = do
            _ <- fakeCreate calls True r
            -- what the create's runner leaves on Ctrl-C or Stop
            writeFile (crTarget r </> ".fisher" </> "create.jsonl") "{\"event\":\"step start\",\"step\":\"cache\"}\n"
            writeEvent (crTarget r) (Failed "interrupted")
            pure (Left ("Interrupted; resume with fisher project build " <> T.pack (crTarget r)))
          resume yes p = modifyIORef' builds (<> [(yes, p)]) >> writeEvent p Ready >> pure (Right ())
          failing _ p = writeEvent p (Failed "step cache failed (exit 1)") >> pure (Left "step cache failed (exit 1); see .fisher/create.log")
      said <- newIORef []
      let d = (deps calls True) {sdCreate = interrupted, sdSay = \l -> modifyIORef' said (<> [l])}
      scratchWith d req `shouldReturn` Left "the base build was interrupted; run fisher scratch again to resume"
      [c] <- readIORef calls
      doesDirectoryExist (crTarget c) `shouldReturn` True
      out <- newIORef []
      -- a resume interrupted again keeps it, with 130
      let again _ p = writeEvent p (Failed "interrupted") >> pure (Left ("Interrupted; resume with fisher project build " <> T.pack p))
      scratchMainWith d {sdBuild = again} (\l -> modifyIORef' out (<> [l])) [] `shouldReturn` 130
      readIORef out `shouldReturn` ["(>_<) the base build was interrupted; run fisher scratch again to resume"]
      -- the next scratch builds the kept base instead of a fresh create
      length <$> readIORef calls `shouldReturn` 1
      Right (r, _) <- scratchWith d {sdBuild = resume} req {srYes = True}
      readIORef builds `shouldReturn` [(True, crTarget c)]
      length <$> readIORef calls `shouldReturn` 1
      doesDirectoryExist r `shouldReturn` True
      readIORef said >>= (`shouldSatisfy` elem "  resuming its interrupted build")
      -- a resume that fails its preflight (no new event) reports that error with 1, the base kept
      writeEvent (crTarget c) (Failed "interrupted")
      let preflightFails _ _ = pure (Left "elan is not on PATH: install elan")
      scratchWith d {sdBuild = preflightFails} req `shouldReturn` Left "elan is not on PATH: install elan"
      out2 <- newIORef []
      scratchMainWith d {sdBuild = preflightFails} (\l -> modifyIORef' out2 (<> [l])) [] `shouldReturn` 1
      readIORef out2 `shouldReturn` ["(>_<) elan is not on PATH: install elan"]
      doesDirectoryExist (crTarget c) `shouldReturn` True
      -- a resume that fails (not interrupted) removes the base
      writeEvent (crTarget c) (Failed "interrupted")
      scratchWith d {sdBuild = failing} req `shouldReturn` Left "step cache failed (exit 1); see .fisher/create.log"
      doesDirectoryExist (crTarget c) `shouldReturn` False
      Right _ <- scratchWith d {sdCreate = fakeCreate calls True} req
      length <$> readIORef calls `shouldReturn` 2

  describe "listScratch and pruneScratch" $ do
    it "lists scratches and bases with age and size" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right (r, _) <- scratchWith (deps calls True) req
      [c] <- readIORef calls
      xs <- listScratch
      sort [p | (p, _, _) <- xs] `shouldBe` sort [r, crTarget c]
      all (\(_, age, bytes) -> age >= 0 && age < 1 && bytes > 0) xs `shouldBe` True
      baseRelease (crTarget c) `shouldReturn` "mathlib=v4.24.0"
      baseRelease r `shouldReturn` ""
      out <- newIORef []
      scratchMainWith (deps calls True) (\l -> modifyIORef' out (<> [l])) ["--list"] `shouldReturn` 0
      ls <- readIORef out
      filter ("mathlib=v4.24.0" `T.isSuffixOf`) ls `shouldSatisfy` ((== 1) . length)

    it "prunes only scratches older than the days, and bases only with the flag" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right (old, _) <- scratchWith (deps calls True) req
      Right (new, _) <- scratchWith (deps calls True) req
      [c] <- readIORef calls
      let base = crTarget c
      now <- getCurrentTime
      let tenDaysAgo = addUTCTime (-10 * 86400) now
      makeOld old tenDaysAgo
      setModificationTime base tenDaysAgo
      pruneScratch 5 False `shouldReturn` [old]
      doesDirectoryExist old `shouldReturn` False
      doesDirectoryExist new `shouldReturn` True
      doesDirectoryExist base `shouldReturn` True
      pruneScratch 5 True `shouldReturn` [base]
      doesDirectoryExist base `shouldReturn` False
      doesDirectoryExist new `shouldReturn` True

    it "keeps an old scratch whose work file was edited lately" $ withHomes $ \_ -> do
      calls <- newIORef []
      Right (old, f) <- scratchWith (deps calls True) req {srStatement = Just "True"}
      now <- getCurrentTime
      makeOld old (addUTCTime (-10 * 86400) now)
      setModificationTime f (addUTCTime (-60) now)
      pruneScratch 5 False `shouldReturn` []
      setModificationTime f (addUTCTime (-10 * 86400) now)
      pruneScratch 5 False `shouldReturn` [old]

    it "keeps a base in use (its lock held)" $ withHomes $ \home -> do
      calls <- newIORef []
      started <- newIORef False
      gate <- newEmptyMVar
      done <- newEmptyMVar
      let slow r = do
            atomicModifyIORef' started (const (True, ()))
            takeMVar gate
            fakeCreate calls True r
      _ <- forkIO (scratchWith (deps calls True) {sdCreate = slow} req >>= putMVar done)
      let waitStart = readIORef started >>= \s -> if s then pure () else threadDelay 10000 >> waitStart
      waitStart
      bases <- listDirectory (home </> ".cache" </> "fisher" </> "scratch" </> "bases")
      pruneScratch 0 True `shouldReturn` []
      putMVar gate ()
      Right _ <- takeMVar done
      length bases `shouldBe` 1

  describe "scratchMainWith" $ do
    it "prints the root and the file, and parses the flags" $ withHomes $ \home -> do
      calls <- newIORef []
      out <- newIORef []
      code <- scratchMainWith (deps calls True) (\l -> modifyIORef' out (<> [l]))
        ["--template", "lean-mathlib", "--var", "mathlib=v4.24.0", "--yes", "--statement", "True"]
      code `shouldBe` 0
      ls <- readIORef out
      [c] <- readIORef calls
      crVars c `shouldBe` [("mathlib", "v4.24.0"), ("project", "Scratch")]
      crYes c `shouldBe` True
      let scratchLines = filter ("Scratch: " `T.isPrefixOf`) ls
          fileLines = filter ("File: " `T.isPrefixOf`) ls
      length scratchLines `shouldBe` 1
      length fileLines `shouldBe` 1
      T.isPrefixOf (T.pack (home </> ".cache")) (T.drop 9 (head scratchLines)) `shouldBe` True

    it "lists, prunes, and refuses a misuse with 2" $ withHomes $ \_ -> do
      calls <- newIORef []
      out <- newIORef []
      let run = scratchMainWith (deps calls True) (\l -> modifyIORef' out (<> [l]))
      run ["--list"] `shouldReturn` 0
      run ["--prune", "3", "--bases"] `shouldReturn` 0
      run ["--prune"] `shouldReturn` 2
      run ["--prune", "x"] `shouldReturn` 2
      run ["--var", "novalue"] `shouldReturn` 2
      run ["a.lean", "--statement", "True"] `shouldReturn` 2
      run ["--bogus"] `shouldReturn` 2
      run ["--template", "nope"] `shouldReturn` 1
  where
    isLeft = either (const True) (const False)
    -- the root, its top-level entries and the work file at that time
    makeOld root t = do
      entries <- listDirectory root
      mapM_ (\e -> setModificationTime (root </> e) t) entries
      setModificationTime (root </> "Scratch" </> "Scratch.lean") t
      setModificationTime root t
