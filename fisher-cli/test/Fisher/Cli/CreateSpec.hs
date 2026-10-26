module Fisher.Cli.CreateSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, emptyPermissions
  , setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, hPutStr, readFile')
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (raiseSignal, sigINT, sigTERM, signalProcessGroup)
import System.Process (createPipe)
import Test.Hspec

import Fisher.Cli.Create
import Fisher.ProjectCommand (CreateRequest (..))
import Fisher.Prover (ProverKind (..))
import Fisher.Template (Origin (..), Template (..), findTemplate, templateRevision)
import Fisher.Template.Run (Event (..), doneSteps, readEvents)

withTmp :: (FilePath -> IO a) -> IO a
withTmp k = withSystemTempDirectory "fisher-create" (\d -> canonicalizePath d >>= k)

-- | HOME and FISHER_HOME in a scratch directory (the built-ins are written
-- under FISHER_HOME).
withHomes :: (FilePath -> IO a) -> IO a
withHomes k = withTmp $ \d -> bracket save restore (const (mapM_ (uncurry setEnv) (kvs d) >> k d))
  where
    kvs d = [("HOME", d), ("FISHER_HOME", d </> "fh")]
    save = mapM (\n -> (,) n <$> lookupEnv n) ["HOME", "FISHER_HOME"]
    restore = mapM_ (\(n, v) -> maybe (unsetEnv n) (setEnv n) v)

writeExe :: FilePath -> String -> IO ()
writeExe p body = do
  exists <- doesFileExist p
  when exists (setPermissions p (setOwnerWritable True (setOwnerReadable True emptyPermissions)))
  writeFile p ("#!/bin/sh\n" <> body)
  setPermissions p (setOwnerExecutable True (setOwnerWritable True (setOwnerReadable True emptyPermissions)))

-- | A bin directory with stand-ins for the named tools.
toolsDir :: FilePath -> [String] -> IO FilePath
toolsDir tmp tools = do
  let bin = tmp </> "bin-" <> concat tools
  createDirectoryIfMissing True bin
  mapM_ (\t -> writeExe (bin </> t) "exit 0\n") tools
  pure bin

data Fake = Fake {fSaid :: IORef [Text], fAsked :: IORef [Text]}

-- | Dependencies with the tools on PATH, a fixed release, a passing check
-- and the given answer to the trust question.
fakeDeps :: FilePath -> Value -> Bool -> IO (CreateDeps, Fake)
fakeDeps tmp cfg answer = do
  said <- newIORef []
  asked <- newIORef []
  bin <- toolsDir tmp ["elan", "lake"]
  path <- maybe "" id <$> lookupEnv "PATH"
  let d = CreateDeps
        { cdSay = \l -> modifyIORef' said (<> [l])
        , cdAsk = \q -> modifyIORef' asked (<> [q]) >> pure answer
        , cdConfig = cfg
        , cdEnv = [("PATH", bin <> ":" <> path)]
        , cdResolver = \w _ -> pure (Right (if w == "smt" then "abc1234" else "v4.24.0"))
        , cdCheck = \_ _ -> pure (Right ())
        , cdTrustFile = tmp </> "trusted.json"
        , cdScale = 60 }
  pure (d, Fake said asked)

request :: FilePath -> CreateRequest
request target = CreateRequest target Nothing Nothing [] True False Nothing Nothing

-- | A custom template whose steps write a file and print a line.
writeFake :: FilePath -> [String] -> IO ()
writeFake dir steps = do
  createDirectoryIfMissing True dir
  writeFile (dir </> "fisher-template.yaml") $ unlines $
    [ "name: fake", "prover: lean4", "description: A fake", "files: [lakefile.toml, Sample.lean]", "steps:" ]
      <> steps <> ["sample: Sample.lean"]
  writeFile (dir </> "lakefile.toml") "name = \"{{project}}\"\n"
  writeFile (dir </> "Sample.lean") "theorem fisher_sample : True := trivial\n"

goodSteps :: [String]
goodSteps =
  [ "  - name: one", "    run: [sh, -c, 'echo one > one.txt']"
  , "  - name: two", "    run: [sh, -c, 'echo two']" ]

configured :: FilePath -> Value
configured dir = object ["fisher" .= object ["templates" .= object ["fake" .= dir]]]

-- | Change the manifest's @template@ section.
editTemplate :: FilePath -> (KM.KeyMap Value -> KM.KeyMap Value) -> IO ()
editTemplate root f = do
  let mpath = root </> ".fisher" </> "project.yaml"
  Y.decodeFileThrow mpath >>= \case
    Object o | Just (Object t) <- KM.lookup "template" o -> Y.encodeFile mpath (Object (KM.insert "template" (Object (f t)) o))
    _ -> expectationFailure "no template section"

-- | The action with a directory first on this process's PATH.
withPath :: FilePath -> IO a -> IO a
withPath dir act = bracket (lookupEnv "PATH") (maybe (unsetEnv "PATH") (setEnv "PATH")) $ \old ->
  setEnv "PATH" (dir <> maybe "" (":" <>) old) >> act

manifestOf :: FilePath -> IO Value
manifestOf root = Y.decodeFileThrow (root </> ".fisher" </> "project.yaml")

field :: [Text] -> Value -> Value
field [] v = v
field (k : ks) (Object o) = maybe Null (field ks) (KM.lookup (K.fromText k) o)
field _ _ = Null

spec :: Spec
spec = do
  describe "createProjectWith" $ do
    it "--no-build writes the rendered files and the manifest and runs no step" $ withHomes $ \tmp -> do
      (d, f) <- fakeDeps tmp (object []) False
      let target = tmp </> "Demo"
      r <- createProjectWith d {cdEnv = [("PATH", "")]} (request target)
        { crTemplate = Just "lean", crVars = [("lean", "v4.24.0")], crBuild = False }
      r `shouldBe` Right target
      mapM_ (\p -> doesFileExist (target </> p) `shouldReturn` True) ["lakefile.toml", "Demo.lean", "Demo/Sample.lean"]
      TIO.readFile (target </> "lean-toolchain") >>= (`shouldSatisfy` T.isInfixOf "leanprover/lean4:v4.24.0")
      m <- manifestOf target
      field ["kind"] m `shouldBe` String "lean4"
      field ["template", "name"] m `shouldBe` String "lean"
      field ["template", "variables"] m `shouldBe` object ["project" .= ("Demo" :: Text), "lean" .= ("v4.24.0" :: Text)]
      field ["template", "revision"] m `shouldSatisfy` (\case String s -> T.length s == 64; _ -> False)
      field ["template", "sample"] m `shouldBe` String "Demo/Sample.lean"
      field ["source", "mode"] m `shouldBe` String "template"
      readEvents target `shouldReturn` []
      doesFileExist (target </> ".fisher" </> "create.log") `shouldReturn` False
      readIORef (fAsked f) `shouldReturn` []
      said <- readIORef (fSaid f)
      said `shouldContain` ["Warning: elan is not on PATH: install elan"]
      last said `shouldBe` ("Not built; build it with: fisher project build " <> T.pack target)
      field ["template", "origin"] m `shouldBe` String "builtin"

    it "refuses a target holding .git and accepts one holding only .DS_Store" $ withHomes $ \tmp -> do
      (d, _) <- fakeDeps tmp (object []) False
      let repo = tmp </> "Repo"
          mac = tmp </> "Mac"
          req t = (request t) {crTemplate = Just "lean", crVars = [("lean", "v4.24.0")], crBuild = False}
      createDirectoryIfMissing True (repo </> ".git")
      createDirectoryIfMissing True mac
      writeFile (mac </> ".DS_Store") ""
      createProjectWith d (req repo) `shouldReturn` Left ("Target is not empty: " <> T.pack repo)
      doesFileExist (repo </> "lakefile.toml") `shouldReturn` False
      createProjectWith d (req mac) `shouldReturn` Right mac
      doesFileExist (mac </> "lakefile.toml") `shouldReturn` True

    it "stops before writing when lake is not on PATH, with the elan fix" $ withHomes $ \tmp -> do
      (d, _) <- fakeDeps tmp (object []) False
      bin <- toolsDir tmp ["elan"]
      let target = tmp </> "NoLake"
      createProjectWith d {cdEnv = [("PATH", bin)]} (request target) {crTemplate = Just "lean", crVars = [("lean", "v4.24.0")]}
        `shouldReturn` Left "lake is not on PATH: install elan"
      doesDirectoryExist target `shouldReturn` False
      preflight [("PATH", tmp </> "nowhere")] `shouldReturn` Left "elan is not on PATH: install elan"
      -- offline and without the tools: the tools are named, not the release
      createProjectWith d {cdEnv = [("PATH", "")], cdResolver = \_ _ -> pure (Left "offline")} (request target) {crTemplate = Just "lean"}
        `shouldReturn` Left "elan is not on PATH: install elan"

    it "writes nothing when the latest release cannot be resolved" $ withHomes $ \tmp -> do
      (d, _) <- fakeDeps tmp (object []) False
      let target = tmp </> "Offline"
      createProjectWith d {cdResolver = \_ _ -> pure (Left "no network")} (request target) {crTemplate = Just "lean"}
        `shouldReturn` Left "could not resolve the latest release; pass --var lean=v4.x"
      doesDirectoryExist target `shouldReturn` False

    it "renders the smt revision the resolver gives into lean-mathlib's lakefile" $ withHomes $ \tmp -> do
      (d, _) <- fakeDeps tmp (object []) False
      calls <- newIORef []
      let target = tmp </> "Smt"
          resolver w done = do
            modifyIORef' calls (<> [(w, done)])
            pure (Right (if w == "smt" then "abc1234" else "v4.34.0"))
      createProjectWith d {cdResolver = resolver} (request target) {crTemplate = Just "lean-mathlib", crBuild = False}
        `shouldReturn` Right target
      lakefile <- readFile' (target </> "lakefile.toml")
      -- the smt block comes before Mathlib's, so Mathlib's pins take precedence
      lakefile `shouldSatisfy` isInfixOf "name = \"smt\"\nscope = \"ufmg-smite\"\nrev = \"abc1234\"\n\n[[require]]\nname = \"mathlib\"\nscope = \"leanprover-community\"\nrev = \"v4.34.0\"\n"
      readFile' (target </> "Smt" </> "Sample.lean") >>= (`shouldSatisfy` isInfixOf "import Smt\n")
      readIORef calls `shouldReturn` [("mathlib", [("project", "Smt")]), ("smt", [("project", "Smt"), ("mathlib", "v4.34.0")])]

    it "a resolver giving none renders no smt block and no import Smt" $ withHomes $ \tmp -> do
      (d, _) <- fakeDeps tmp (object []) False
      let target = tmp </> "NoSmt"
          resolver w _ = pure (Right (if w == "smt" then "none" else "v4.35.0"))
      createProjectWith d {cdResolver = resolver} (request target) {crTemplate = Just "lean-mathlib", crBuild = False}
        `shouldReturn` Right target
      lakefile <- readFile' (target </> "lakefile.toml")
      lakefile `shouldNotSatisfy` isInfixOf "smt"
      lakefile `shouldNotSatisfy` isInfixOf "{{"
      lakefile `shouldSatisfy` isInfixOf "name = \"mathlib\"\nscope = \"leanprover-community\"\nrev = \"v4.35.0\"\n"
      sample <- readFile' (target </> "NoSmt" </> "Sample.lean")
      sample `shouldNotSatisfy` isInfixOf "Smt"
      sample `shouldNotSatisfy` isInfixOf "by smt"

    it "--var smt=main renders main without a fetch" $ withHomes $ \tmp -> do
      (d, _) <- fakeDeps tmp (object []) False
      calls <- newIORef []
      let target = tmp </> "Main"
          resolver w _ = modifyIORef' calls (<> [w]) >> pure (Right "v4.34.0")
      createProjectWith d {cdResolver = resolver} (request target)
        { crTemplate = Just "lean-mathlib", crVars = [("smt", "main")], crBuild = False }
        `shouldReturn` Right target
      readFile' (target </> "lakefile.toml") >>= (`shouldSatisfy` isInfixOf "name = \"smt\"\nscope = \"ufmg-smite\"\nrev = \"main\"\n")
      readIORef calls `shouldReturn` ["mathlib"]

    it "resolves smt from Reservoir on the resolved mathlib, else none with a warning" $ do
      said <- newIORef []
      let say l = modifyIORef' said (<> [l])
          builds = Right [("c35", "leanprover/lean4:v4.35.0"), ("c34", "leanprover/lean4:v4.34.0")]
      templateResolver say (pure builds) "smt" [("project", "D"), ("mathlib", "v4.34.0")] `shouldReturn` Right "c34"
      readIORef said `shouldReturn` []
      templateResolver say (pure (Left "curl exited with 6")) "smt" [("mathlib", "v4.34.0")] `shouldReturn` Right "none"
      readIORef said `shouldReturn` ["Warning: could not read Reservoir's builds of lean-smt (curl exited with 6); add it later with fisher project add smt"]
      templateResolver say (pure builds) "smt" [("mathlib", "v4.20.0")] `shouldReturn` Right "none"
      readIORef said >>= (`shouldEndWith` ["Warning: lean-smt has no build for leanprover/lean4:v4.20.0; add it later with fisher project add smt"])

    it "runs the steps, checks, and ends Ready with the events ending ready" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          target = tmp </> "Good"
      writeFake tdir goodSteps
      (d, f) <- fakeDeps tmp (configured tdir) False
      r <- createProjectWith d (request target) {crTemplate = Just "fake", crYes = True}
      r `shouldBe` Right target
      doesFileExist (target </> "one.txt") `shouldReturn` True
      said <- readIORef (fSaid f)
      drop (length said - 2) said `shouldBe`
        [ "Ready: " <> T.pack target
        , "Next: fisher search " <> T.pack (target </> "Sample.lean") <> " or /prove " <> T.pack (target </> "Sample.lean") ]
      said `shouldContain` ["  step one (1/2): sh -c echo one > one.txt (output in .fisher/create.log)"]
      evs <- readEvents target
      evs `shouldBe` [StepStart "one", StepEnd "one" 0 [], StepStart "two", StepEnd "two" 0 ["two"], CheckEnd True [], Ready]
      doneSteps target `shouldReturn` Right ["one", "two"]
      m <- manifestOf target
      field ["template", "steps"] m `shouldSatisfy` (\case Array a -> length a == 2; _ -> False)

    it "prints a failing check's problems, ends the events failed, and build runs the check again" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          target = tmp </> "Bad"
      writeFake tdir goodSteps
      (d, f) <- fakeDeps tmp (configured tdir) False
      r <- createProjectWith d {cdCheck = \_ _ -> pure (Left ["Sample.lean:1:0: error: boom"])} (request target) {crTemplate = Just "fake", crYes = True}
      r `shouldBe` Left ("The sample check failed for Sample.lean:\n  Sample.lean:1:0: error: boom\n  see .fisher/create.log; rerun with fisher project build " <> T.pack target)
      evs <- readEvents target
      drop (length evs - 2) evs `shouldBe` [CheckEnd False ["Sample.lean:1:0: error: boom"], Failed "the sample check failed"]
      buildProjectWith d False target `shouldReturn` Right ()
      said <- readIORef (fSaid f)
      said `shouldContain` ["  step one (1/2): done before, skipped"]
      last <$> readEvents target `shouldReturn` Ready

    it "stops at a failing step and names it and the log" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          target = tmp </> "Step"
      writeFake tdir ["  - name: one", "    run: [sh, -c, 'exit 3']"]
      (d, _) <- fakeDeps tmp (configured tdir) False
      createProjectWith d (request target) {crTemplate = Just "fake", crYes = True}
        `shouldReturn` Left "step one failed (exit 3); see .fisher/create.log"
      last <$> readEvents target `shouldReturn` Failed "step one failed (exit 3); see .fisher/create.log"

    it "asks before an untrusted template's steps; no stops with nothing run or written" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          target = tmp </> "Asked"
      writeFake tdir goodSteps
      (d, f) <- fakeDeps tmp (configured tdir) False
      r <- createProjectWith d (request target) {crTemplate = Just "fake"}
      r `shouldBe` Left "Template `fake` was not trusted; nothing was written. Pass --yes to run its steps."
      doesDirectoryExist target `shouldReturn` False
      readIORef (fAsked f) `shouldReturn` ["Run these steps? [y/N] "]
      said <- readIORef (fSaid f)
      said `shouldBe`
        [ "Template `fake` (" <> T.pack tdir <> ") runs these steps in " <> T.pack target <> ":"
        , "  one: sh -c echo one > one.txt"
        , "  two: sh -c echo two" ]
      -- yes runs them and remembers; the next create does not ask
      (dy, fy) <- fakeDeps tmp (configured tdir) True
      createProjectWith dy (request target) {crTemplate = Just "fake"} `shouldReturn` Right target
      createProjectWith dy (request (tmp </> "Again")) {crTemplate = Just "fake"} `shouldReturn` Right (tmp </> "Again")
      readIORef (fAsked fy) `shouldReturn` ["Run these steps? [y/N] "]

    it "runs an untrusted template's steps with --trust-revision only at that revision, without asking" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          revOf = findTemplate (configured tdir) "fake" >>= either (fail . T.unpack) templateRevision
      writeFake tdir goodSteps
      (d, f) <- fakeDeps tmp (configured tdir) True
      shown <- revOf
      -- the template changes after its steps were confirmed
      appendFile (tdir </> "Sample.lean") "-- changed\n"
      now <- revOf
      now `shouldNotBe` shown
      r <- createProjectWith d (request (tmp </> "Changed")) {crTemplate = Just "fake", crTrustRevision = Just shown}
      r `shouldBe` Left ("Template `fake`: the template changed since it was confirmed (now at " <> now <> "); nothing was written. Confirm its steps again.")
      doesDirectoryExist (tmp </> "Changed") `shouldReturn` False
      doesFileExist (tmp </> "trusted.json") `shouldReturn` False
      -- confirmed at the revision it has: it runs, nothing is asked, trust is recorded
      createProjectWith d (request (tmp </> "Confirmed")) {crTemplate = Just "fake", crTrustRevision = Just now} `shouldReturn` Right (tmp </> "Confirmed")
      doesFileExist (tmp </> "Confirmed" </> "one.txt") `shouldReturn` True
      readIORef (fAsked f) `shouldReturn` []
      doesFileExist (tmp </> "trusted.json") `shouldReturn` True

    it "takes a --template-source with a manifest as a template, and one without as before" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          plain = tmp </> "plain"
      writeFake tdir goodSteps
      createDirectoryIfMissing True plain
      writeFile (plain </> "lakefile.lean") "-- lean project\n"
      (d, f) <- fakeDeps tmp (object []) True
      createProjectWith d (request (tmp </> "FromManifest")) {crSource = Just (T.pack tdir)}
        `shouldReturn` Right (tmp </> "FromManifest")
      readIORef (fAsked f) `shouldReturn` ["Run these steps? [y/N] "]
      m <- manifestOf (tmp </> "FromManifest")
      field ["source", "template_source"] m `shouldBe` String (T.pack tdir)
      (d2, f2) <- fakeDeps tmp (object []) False
      createProjectWith d2 (request (tmp </> "Copied")) {crSource = Just (T.pack plain), crName = Just "Copied Project"}
        `shouldReturn` Right (tmp </> "Copied")
      readFile (tmp </> "Copied" </> "lakefile.lean") `shouldReturn` "-- lean project\n"
      readIORef (fSaid f2) `shouldReturn`
        [ "(^_^)b Created Fisher project from template: Copied Project"
        , "  Root: " <> T.pack (tmp </> "Copied")
        , "  Template source: " <> T.pack plain ]
      readEvents (tmp </> "Copied") `shouldReturn` []

    it "turns Ctrl-C into a stop that build can resume" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          target = tmp </> "Slow"
      writeFake tdir ["  - name: slow", "    run: [sleep, '30']"]
      (d, _) <- fakeDeps tmp (configured tdir) False
      done <- newEmptyMVar
      _ <- forkIO (createProjectWith d (request target) {crTemplate = Just "fake", crYes = True} >>= putMVar done)
      let waitStart n = do
            evs <- readEvents target
            if StepStart "slow" `elem` evs || n > (200 :: Int) then pure () else threadDelay 50000 >> waitStart (n + 1)
      waitStart 0
      threadDelay 200000
      raiseSignal sigINT
      takeMVar done `shouldReturn` Left ("Interrupted; resume with fisher project build " <> T.pack target)
      last <$> readEvents target `shouldReturn` Failed "interrupted"
      doneSteps target `shouldReturn` Right []
      -- a build of something not made from a template says so
      createDirectoryIfMissing True (tmp </> "Other" </> ".fisher")
      writeFile (tmp </> "Other" </> ".fisher" </> "project.yaml") "name: Other\nkind: lean4\n"
      buildProjectWith d False (tmp </> "Other") `shouldReturn` Left (T.pack (tmp </> "Other") <> " was not made from a template; there is nothing to build")

    it "writes only failed interrupted when a stop kills the step before the signal arrives" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          target = tmp </> "Stopped"
      writeFake tdir ["  - name: slow", "    run: [sh, -c, 'echo $$ > step.pid; exec sleep 30']"]
      (d, _) <- fakeDeps tmp (configured tdir) False
      done <- newEmptyMVar
      _ <- forkIO (createProjectWith d (request target) {crTemplate = Just "fake", crYes = True} >>= putMVar done)
      let waitPid n = do
            ok <- doesFileExist (target </> "step.pid")
            txt <- if ok then readFile' (target </> "step.pid") else pure ""
            case reads txt of
              [(pid, _)] -> pure pid
              _ | n > (200 :: Int) -> expectationFailure "the step never started" >> pure 0
                | otherwise -> threadDelay 50000 >> waitPid (n + 1)
      pid <- waitPid 0
      -- the UI's stop: the step's group first, then this process
      signalProcessGroup sigTERM (fromIntegral (pid :: Int))
      threadDelay 300000
      raiseSignal sigTERM
      takeMVar done `shouldReturn` Left ("Interrupted; resume with fisher project build " <> T.pack target)
      evs <- readEvents target
      [e | e@(Failed _) <- evs] `shouldBe` [Failed "interrupted"]
      [n | StepEnd "slow" n _ <- evs] `shouldBe` [-15]

    it "turns Ctrl-C during the check into the same stop" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          target = tmp </> "SlowCheck"
      writeFake tdir goodSteps
      (d0, _) <- fakeDeps tmp (configured tdir) False
      checking <- newEmptyMVar
      let d = d0 {cdCheck = \_ _ -> putMVar checking () >> threadDelay 30000000 >> pure (Right ())}
      done <- newEmptyMVar
      _ <- forkIO (createProjectWith d (request target) {crTemplate = Just "fake", crYes = True} >>= putMVar done)
      takeMVar checking
      threadDelay 100000
      raiseSignal sigINT
      takeMVar done `shouldReturn` Left ("Interrupted; resume with fisher project build " <> T.pack target)
      last <$> readEvents target `shouldReturn` Failed "interrupted"
      doneSteps target `shouldReturn` Right ["one", "two"]

    it "build asks about a custom template's steps as create does, and --yes skips the question" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          target = tmp </> "Later"
      writeFake tdir goodSteps
      (d, _) <- fakeDeps tmp (configured tdir) False
      createProjectWith d (request target) {crTemplate = Just "fake", crYes = True, crBuild = False} `shouldReturn` Right target
      -- another user (an empty trust file) builds it
      (dn, fn) <- fakeDeps (tmp </> "other") (configured tdir) False
      buildProjectWith dn False target
        `shouldReturn` Left "The steps recorded for `fake` are not trusted; nothing was run. Pass --yes to run them."
      readIORef (fAsked fn) `shouldReturn` ["Run these steps? [y/N] "]
      readEvents target `shouldReturn` []
      buildProjectWith dn True target `shouldReturn` Right ()
      last <$> readEvents target `shouldReturn` Ready
      -- now trusted there: no question
      buildProjectWith dn False target `shouldReturn` Right ()
      readIORef (fAsked fn) `shouldReturn` ["Run these steps? [y/N] "]

    it "build does not trust a manifest claiming a built-in whose steps differ" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
          target = tmp </> "Claims"
      writeFake tdir goodSteps
      (d, _) <- fakeDeps tmp (configured tdir) False
      createProjectWith d (request target) {crTemplate = Just "fake", crYes = True, crBuild = False} `shouldReturn` Right target
      editTemplate target (KM.insert "name" (String "lean") . KM.insert "origin" (String "builtin"))
      (dn, fn) <- fakeDeps (tmp </> "other") (object []) False
      buildProjectWith dn False target `shouldReturn` Left "The steps recorded for `lean` are not trusted; nothing was run. Pass --yes to run them."
      readIORef (fAsked fn) `shouldReturn` ["Run these steps? [y/N] "]
      readEvents target `shouldReturn` []

    it "build runs a built-in project's matching steps without asking, origin recorded or not" $ withHomes $ \tmp -> do
      let target = tmp </> "Plain"
      (d, f) <- fakeDeps tmp (object []) False
      createProjectWith d (request target) {crTemplate = Just "lean", crVars = [("lean", "v4.24.0")], crBuild = False}
        `shouldReturn` Right target
      editTemplate target (KM.delete "origin")
      bin <- toolsDir tmp ["elan", "lake"]
      withPath bin (buildProjectWith d False target) `shouldReturn` Right ()
      readIORef (fAsked f) `shouldReturn` []
      last <$> readEvents target `shouldReturn` Ready

    it "build asks about a hand-edited step, once" $ withHomes $ \tmp -> do
      let target = tmp </> "Edited"
      (d, f) <- fakeDeps tmp (object []) False
      createProjectWith d (request target) {crTemplate = Just "lean", crVars = [("lean", "v4.24.0")], crBuild = False}
        `shouldReturn` Right target
      editTemplate target $ KM.insert "steps"
        (toJSON [object ["name" .= ("build" :: Text), "run" .= (["sh", "-c", "echo edited"] :: [Text]), "timeout_minutes" .= (30 :: Int)]])
      buildProjectWith d False target `shouldReturn` Left "The steps recorded for `lean` are not trusted; nothing was run. Pass --yes to run them."
      readIORef (fAsked f) `shouldReturn` ["Run these steps? [y/N] "]
      said <- readIORef (fSaid f)
      said `shouldContain` ["  build: sh -c echo edited"]
      buildProjectWith d True target `shouldReturn` Right ()
      buildProjectWith d False target `shouldReturn` Right ()
      readIORef (fAsked f) `shouldReturn` ["Run these steps? [y/N] "]

    it "records trust only once the project is written" $ withHomes $ \tmp -> do
      let tdir = tmp </> "tpl"
      writeFake tdir goodSteps
      -- no lakefile: the manifest cannot be written for a Lean project
      writeFile (tdir </> "fisher-template.yaml") . T.unpack . T.replace "[lakefile.toml, Sample.lean]" "[Sample.lean]" . T.pack
        =<< readFile' (tdir </> "fisher-template.yaml")
      (d, _) <- fakeDeps tmp (configured tdir) True
      r <- createProjectWith d (request (tmp </> "NoLake")) {crTemplate = Just "fake"}
      r `shouldSatisfy` either (T.isInfixOf "is missing") (const False)
      doesFileExist (tmp </> "trusted.json") `shouldReturn` False

  describe "askOn" $ do
    let answer s = do
          (r, w) <- createPipe
          hPutStr w s
          hClose w
          askOn r "Run these steps? [y/N] "
    it "is yes only for y or yes, and no for a closed input" $ do
      answer "n\n" `shouldReturn` False
      answer "y\n" `shouldReturn` True
      answer " YES \n" `shouldReturn` True
      answer "" `shouldReturn` False

  describe "checkSampleWith" $ do
    let sample = Template "fake" Lean4 "" [] [] [] "Sample.lean" (Source "x") "x"
        withLake body k = withTmp $ \tmp -> do
          let root = tmp </> "p"
          createDirectoryIfMissing True (root </> ".fisher")
          writeFile (root </> "Sample.lean") "theorem fisher_sample : True := trivial\n"
          writeExe (tmp </> "lake") body
          k (tmp </> "lake") root
        leanSays out = "if [ \"$1\" = build ]; then echo built; exit 0; fi\n" <> out
    it "passes a sample with no errors, no sorry and standard axioms" $
      withLake (leanSays "echo \"'fisher_sample' depends on axioms: [propext]\"\n") $ \lake root -> do
        -- a probe a killed check left behind goes first
        writeFile (root </> "_fisher_search_Old.lean") "theorem stale : False := sorry\n"
        checkSampleWith lake 30 sample root `shouldReturn` Right ()
        doesFileExist (root </> "_fisher_search_Old.lean") `shouldReturn` False
        doesFileExist (root </> "Sample.lean") `shouldReturn` True
        logText <- readFile (root </> ".fisher" </> "create.log")
        logText `shouldSatisfy` isInfixOf "== check: lake build\nbuilt\n"
        logText `shouldSatisfy` isInfixOf "== check: probe of Sample.lean\n'fisher_sample' depends on axioms: [propext]\n"
    it "reports errors, sorry and other axioms" $
      withLake (leanSays "echo 'Sample.lean:1:8: warning: declaration uses '\"'\"'sorry'\"'\"''\necho \"'fisher_sample' depends on axioms: [sorryAx]\"\n") $ \lake root ->
        checkSampleWith lake 30 sample root `shouldReturn` Left ["`sorry` at line 1", "`fisher_sample` depends on sorryAx"]
    it "reports a failing lake build" $
      withLake "echo 'x.lean:1:0: error: bad'; echo 'no errors here'; echo 'error: build failed'; echo '  an error: inside'; exit 1\n" $ \lake root ->
        checkSampleWith lake 30 sample root `shouldReturn`
          Left ["lake build failed (exit 1); see .fisher/create.log", "x.lean:1:0: error: bad", "error: build failed"]

  describe "sampleTheorems" $ do
    it "takes the names of theorem and lemma lines" $
      sampleTheorems "-- c\ntheorem a (n : Nat) : n = n := rfl\n  theorem b: True := trivial\nlemma c : True := trivial\n"
        `shouldBe` ["a", "b", "c"]
    it "qualifies names by their namespaces and reads attributes and modifiers" $
      sampleTheorems (T.unlines
        [ "namespace Demo", "@[simp] theorem one : 1 = 1 := rfl", "section S", "private lemma two : True := trivial"
        , "end S", "namespace Inner", "@[simp, grind] protected theorem three : True := trivial", "end Inner"
        , "theorem _root_.four : True := trivial", "end Demo", "theorem five : True := trivial" ])
        `shouldBe` ["Demo.one", "Demo.two", "Demo.Inner.three", "four", "five"]
