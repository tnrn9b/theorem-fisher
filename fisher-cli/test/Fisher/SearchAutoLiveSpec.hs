-- | The automation against real Lean and Mathlib: a temporary Lake project
-- on the toolchain of the TWISTED bench clone, with the clone's packages
-- as path dependencies (and a manifest written beforehand, so Lake runs
-- no update and no Mathlib hook), one file with four holes, the real
-- language server through 'openLsp' and the real kernel check, and a
-- scripted backend in place of the models. Pending without the clone's
-- Mathlib (~/.cache/fisher/bench/c3/twisted/lean/.lake/packages/mathlib).
module Fisher.SearchAutoLiveSpec (spec) where

import Control.Exception (finally)
import Control.Monad (forM, forM_, when)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, getHomeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process (CreateProcess (..), StdStream (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Cli.SearchLsp (openLsp)
import Fisher.Search.Auto (AutoConfig (..), Diag (..), Lsp (..), Trial (..), cutAtHole, defaultAutoConfig, holesIn, suggestion, winner, wrapSnippet)
import Fisher.Search.Lean (declarations, findDecl)
import Fisher.Search.Check (AxiomPolicy (..), lakeLeanRun, standardAxioms)
import Fisher.Search.Disprove (Disproof (..))
import Fisher.Search.Report (eventLine)
import Fisher.Search.Orchestrate (realOrchestrator)
import Fisher.Search.Scheduler

cloneLean :: FilePath -> FilePath
cloneLean home = home </> ".cache" </> "fisher" </> "bench" </> "c3" </> "twisted" </> "lean"

packages :: [String]
packages = ["mathlib", "plausible", "LeanSearchClient", "importGraph", "proofwidgets", "aesop", "Qq", "batteries", "Cli"]

header :: T.Text
header = "import Mathlib.Tactic.NormNum\nimport Plausible\n"

-- | Goals the search tactics close, with a Try this suggestion.
fileTry :: T.Text
fileTry = T.intercalate "\n\n"
  [ "import Mathlib.Tactic.NormNum\nimport Plausible\nimport Aesop"
  , "theorem e1 (n : Nat) : n + 0 = n := by sorry"
  , "theorem a1 (n m : Nat) (h : n ≤ m) : n ≤ m + 1 := by sorry"
  , "theorem s1 (p q : Prop) (hp : p) (hq : q) : p ∧ q := by sorry" ] <> "\n"

t1, t2, t3, t4 :: T.Text
t1 = "theorem t1 (n : Nat) : n + 0 = n := by sorry"
t2 = "theorem t2 (xs : List Nat) : (xs ++ []).length = xs.length := by sorry"
t3 = "theorem t3 (n : Nat) : n < 3 := by sorry"
t4 = "theorem t4 : (2 : Nat) + 2 = 5 := by sorry"

file4, file2 :: T.Text
file4 = T.intercalate "\n\n" [header, t1, t2, t3, t4] <> "\n"
file2 = T.intercalate "\n\n" [header, t2, t3] <> "\n"

-- | The project in @d@: the clone's toolchain, each of its packages by
-- path, and the manifest Lake would write for them.
makeProject :: FilePath -> FilePath -> T.Text -> IO ()
makeProject clone d file = do
  copyFile (clone </> "lean-toolchain") (d </> "lean-toolchain")
  let pkg p = clone </> ".lake" </> "packages" </> p
  writeFile (d </> "lakefile.toml") $
    "name = \"autolive\"\n" <> concat ["\n[[require]]\nname = \"" <> p <> "\"\npath = \"" <> pkg p <> "\"\n" | p <- packages]
  entries <- forM packages $ \p -> do
    lean <- doesFileExist (pkg p </> "lakefile.lean")
    pure $ object
      [ "type" .= ("path" :: T.Text), "scope" .= ("" :: T.Text), "name" .= p, "manifestFile" .= ("lake-manifest.json" :: T.Text)
      , "inherited" .= False, "dir" .= pkg p, "configFile" .= (if lean then "lakefile.lean" else "lakefile.toml" :: T.Text) ]
  BL.writeFile (d </> "lake-manifest.json") . A.encode $ object
    [ "version" .= ("1.2.0" :: T.Text), "packagesDir" .= (".lake/packages" :: T.Text), "packages" .= entries
    , "name" .= ("autolive" :: T.Text), "lakeDir" .= (".lake" :: T.Text), "fixedToolchain" .= False ]
  BS.writeFile (d </> "A.lean") (TE.encodeUtf8 file)

-- | Whether the fixture's imports are built in the clone: @lake build
-- --no-build@ of them in the project (it exits non-zero rather than build
-- anything).
importsBuilt :: FilePath -> IO Bool
importsBuilt d = do
  (code, _, _) <- readProcessWithExitCodeIn d "lake" ["build", "--no-build", "Mathlib.Tactic.NormNum", "Plausible"]
  pure (code == ExitSuccess)

readProcessWithExitCodeIn :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
readProcessWithExitCodeIn d cmd args = readCreateProcessWithExitCode (proc cmd args) {cwd = Just d, std_in = NoStream} ""

-- | The files under the clone's packages changed since the marker.
changedSince :: FilePath -> FilePath -> IO [String]
changedSince clone marker = do
  (_, out, _) <- readProcessWithExitCode "find" [clone </> ".lake" </> "packages", "-newer", marker] ""
  pure (lines out)

-- | A live spec in a fresh project: pending when the clone's imports are
-- not built; after it, nothing under the clone's packages is newer than
-- the start.
withFixture :: FilePath -> T.Text -> (FilePath -> IO ()) -> IO ()
withFixture clone file act = withSystemTempDirectory "auto-live" $ \d -> do
  let marker = d </> "started"
  writeFile marker ""
  makeProject clone d file
  built <- importsBuilt d
  if not built
    then pendingWith "clone needs a build (lake build --no-build of the fixture's imports failed)"
    else do
      act d
      changedSince clone marker `shouldReturn` []

-- | The run: one slot, two scripted backends, two attempts per goal
-- (Disprove runs after a goal's second failure).
searchIn :: FilePath -> T.Text -> AutoConfig -> (AttemptSpec -> IO AttemptResult) -> IO (SearchReport, [Event])
searchIn d file ac run = do
  lsp <- openLsp 600 d "A.lean" >>= either (fail . ("the server did not start: " <>) . T.unpack) pure
  events <- newIORef []
  orch <- realOrchestrator run (realToFrac <$> getPOSIXTime)
            (\e -> TIO.putStrLn ("      " <> eventLine e) >> modifyIORef events (e :))
            (\_ -> pure Nothing)
  let env = SearchEnv orch (lakeLeanRun 600) (AxiomPolicy standardAxioms False) d "A.lean"
              (\_ -> pure ()) (Just (AutoEnv ac lsp Nothing)) Nothing
      cfg = defaultSearchConfig {scBackends = ["scripted", "scripted2"], scSlots = 1, scMaxAttempts = 2, scBudget = 1500, scParallelPerGoal = False}
  r <- timeout 1700000000 (runSearch cfg env file) `finally` lspClose lsp
  es <- reverse <$> readIORef events
  case r of
    Nothing -> fail "the search did not end"
    Just (Left e) -> fail (T.unpack e)
    Just (Right rep) -> pure (rep, es)

unchanged :: AttemptSpec -> AttemptResult
unchanged sp = plainResult (Just (asStart sp)) Nothing ""

goal :: T.Text -> SearchReport -> GoalState
goal g rep = maybe (error ("no goal " <> T.unpack g)) id (lookup g (srGoals rep))

-- | The language server's process group (it runs in its own session, as
-- a child of this process), killed.
killServer :: IO Int
killServer = do
  me <- getProcessID
  (code, out, _) <- readProcessWithExitCode "pgrep" ["-P", show me, "-f", "lake serve"] ""
  let pids = if code == ExitSuccess then [read p :: Int | p <- lines out, not (null p)] else []
  forM_ pids $ \p -> signalProcessGroup sigKILL (fromIntegral p)
  pure (length pids)

spec :: Spec
spec = describe "the automation on real Lean and Mathlib" $ do
  home <- runIO getHomeDirectory
  let clone = cloneLean home
  have <- runIO (doesDirectoryExist (clone </> ".lake" </> "packages" </> "mathlib"))
  let live name act = it name (if have then act else pendingWith ("no Mathlib under " <> clone))
  live "Auto closes t1, the model t2, t3 is suspect and t4 disproved" $
    withFixture clone file4 $ \d -> do
      let file = file4
          proved = T.replace "by sorry" "by simp" t2
          run sp
            | asGoal sp == "t2" = pure (plainResult (Just (T.replace t2 proved (asStart sp))) Nothing "")
            | otherwise = pure (unchanged sp)
      (rep, es) <- searchIn d file defaultAutoConfig {acTactics = ["rfl"]} run
      gsClosedBy (goal "t1" rep) `shouldBe` Just "auto"
      gsStatus (goal "t1" rep) `shouldBe` Closed
      gsClosedBy (goal "t2" rep) `shouldBe` Just "model"
      gsStatus (goal "t2" rep) `shouldBe` Closed
      -- plausible draws at random: any n of 3 or more is a counterexample
      gsDisproof (goal "t3" rep) `shouldSatisfy` (\case Just (DSuspect c) -> "< 3 does not hold" `T.isInfixOf` c; _ -> False)
      gsStatus (goal "t3" rep) `shouldNotBe` Closed
      gsDisproof (goal "t4" rep) `shouldBe` Just (DFalse "decide")
      gsStatus (goal "t4" rep) `shouldBe` Disproved
      [() | AutoOff {} <- es] `shouldBe` []
      srBase rep `shouldSatisfy` T.isInfixOf "theorem t1 (n : Nat) : n + 0 = n := by set_option maxHeartbeats 200000 in rfl"
  live "the server killed mid-run: one AutoOff, and the search finishes" $
    withFixture clone file2 $ \d -> do
      let file = file2
          -- a proof that fails, so Repair (the first call after the kill) runs
          broken = T.replace "by sorry" "by omega" t2
      killed <- newIORef Nothing
      let run sp = do
            n <- readIORef killed
            when (n == Nothing) (killServer >>= writeIORef killed . Just)
            pure (if asGoal sp == "t2" then plainResult (Just (T.replace t2 broken (asStart sp))) Nothing "" else unchanged sp)
      (rep, es) <- searchIn d file defaultAutoConfig {acTactics = ["rfl"], acDisprove = False} run
      -- the lake serve process found and its group killed
      (fmap (>= 1) <$> readIORef killed) `shouldReturn` Just True
      [() | AutoOff {} <- es] `shouldBe` [()]
      map fst (srGoals rep) `shouldBe` ["t2", "t3"]
      srClosed rep `shouldBe` 0
  live "Lean 4.33's Try this suggestions from exact?, apply? and aesop? are read and win in place of the search" $
    withFixture clone fileTry $ \d -> do
      lsp <- openLsp 600 d "A.lean" >>= either (fail . ("the server did not start: " <>) . T.unpack) pure
      flip finally (lspClose lsp) $ forM_ [("e1", "exact?"), ("a1", "apply?"), ("s1", "aesop?")] $ \(g, tac) -> do
        let dcl = maybe (error ("no " <> T.unpack g)) id (findDecl g (declarations fileTry))
            h = head (holesIn fileTry dcl)
        r <- lspAttempts lsp (cutAtHole fileTry h) h [wrapSnippet 200000 tac]
        case r of
          Right [tr] -> do
            -- the messages as the server sends them, for the record
            forM_ (tDiags tr) $ \dg -> TIO.putStrLn ("      " <> g <> " " <> T.pack (show (dgSeverity dg)) <> ": " <> T.pack (show (dgMessage dg)))
            let s = suggestion tr
            s `shouldSatisfy` maybe False (\x -> not (T.null x) && not ("?" `T.isInfixOf` x))
            winner [tr] `shouldBe` fmap (wrapSnippet 200000) s
          other -> expectationFailure ("attempts: " <> show other)
