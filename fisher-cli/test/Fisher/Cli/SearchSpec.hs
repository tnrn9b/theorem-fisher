-- | @fisher search@'s arguments with @--graph@ and the pure pieces of its
-- wiring: the refusals, the attempt prompt with the brief, the model call's
-- result as the graph search takes it, and the native backend's model.
module Fisher.Cli.SearchSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (..), SomeAsyncException, SomeException, finally, fromException, throwIO, try)
import qualified Data.ByteString as BS
import System.Exit (ExitCode (..))
import System.Posix.Signals (Handler (Default), installHandler, raiseSignal, sigTERM)
import System.Timeout (timeout)
import Data.List (isInfixOf, sort)
import Data.Maybe (isJust, isNothing)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toUpper)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Fisher.Cli.Search
import Fisher.Cli.Watch (Watched (..))
import Fisher.Search.Child (ChildStats (..))
import Fisher.LLM.Normalize (Usage (..))
import Fisher.Py.Value (obj, str)
import Fisher.RLM.Types (ModelError (..), Reply (..))
import Fisher.Search.Auto (AutoConfig (..), defaultAutoConfig, defaultTactics)
import Fisher.Search.Disprove (Disproof (..), SmtEnv (..))
import Fisher.Search.Facts (FactsConfig (..), defaultFacts)
import Fisher.Search.Library (Lemma (..), libraryDir, loadLibrary, saveLibrary)
import Fisher.Search.Lean (Decl (..), openGoals)
import Fisher.Search.Retrieval (Query (..), RetrievalEnv (..), Retrieved (..), Terminated (..), newRetriever, smtFacts)
import Fisher.Smt (Solver (..))
import Fisher.Smt.Lake (importSmtLine, insertImportSmt)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Fisher.Search.GraphRun (AskError (..))
import Fisher.Runlog (Capture (..), GoalRow (..), NewRun (..), RunEnd (..), RunKind (..), RunRow (..), getRun, listRuns, runEvents, runGoals, runlogPath, withRunlog)
import Fisher.Runlog.Stream (StreamEvent (..))
import Fisher.Search.Report (eventJson, eventLine)
import Fisher.Search.Attempt (AttemptSpec (..), plainResult)
import Fisher.Search.Check (AxiomPolicy (..), LeanRun (..), standardAxioms)
import qualified Fisher.Search.Lean as L
import Fisher.Search.Orchestrate (Orchestrator (..), Recording (..), Step (..), realOrchestrator)
import Fisher.Search.Scheduler (SearchEnv (..))
import qualified Data.Set as Set
import Data.IORef (atomicModifyIORef')
import qualified Data.ByteString.Lazy as BL
import Fisher.Search.Scheduler (AttemptRecord (..), Event (..), GoalState (..), GoalStatus (..), SearchConfig (..), SearchReport (..), plainRecord)
import Fisher.Search.Shape (renderShape, shapeOf)
import Fisher.TestUtil (oldName)
import Fisher.Learn.Policy (Defaults (..), GoalPolicy (..), Policy (..), policyPath, readPolicy)
import Fisher.Learn.Stats (Cell (..), Knob (..), Level (..), Stats (..))
import qualified Fisher.Runlog as R
import Fisher.Cli.Mine (judgeCalls, judgeCaps, lessonsConfigOn, lessonsEventData, lessonsEventFrom, lessonsJudgeOn, lessonsLimits, mineEnvFor, mineImported, withLessons)
import Fisher.Learn.Lessons (Limits (..))
import Fisher.Learn.Mine (MineEnv (..), judgeCostEstimate)
import Fisher.Lessons.File (fileHash, projectLessonsPath, writeLessonsFile)
import Fisher.Lessons.Prompt (PromptLessons (..), promptLessons)
import Fisher.Role (partText)
import Fisher.Sha256 (sha256Hex)

-- | Parse, then the refusals, as the command does; Nothing for a usage error.
run :: [String] -> Maybe (Either Text RunArgs)
run args = checkGraph <$> parseRun args defaultRunArgs

ok :: [String] -> RunArgs
ok args = case run args of
  Just (Right ra) -> ra
  other -> error ("not accepted: " <> show args <> ": " <> show other)

usage :: Double -> Usage
usage = Usage 10 5 15 Nothing Nothing . Just

gs :: GoalStatus -> Maybe Text -> Maybe Disproof -> GoalState
gs st by d = GoalState st [] [] Nothing Nothing by Nothing d False False Nothing Nothing Nothing []

spec :: Spec
spec = do
  describe "retrieval and the library's flags" $ do
    it "both on by default, with the spec's caps" $ do
      let ra = ok ["F.lean"]
      factsConfig ra `shouldBe` Just defaultFacts
      fmap (\c -> (fcMax c, fcChars c)) (factsConfig ra) `shouldBe` Just (20, 8000)
      raLibrary ra `shouldBe` True
      raLibraryDir ra `shouldBe` Nothing
      retrievalWanted ra `shouldBe` True
      raPrintFacts ra `shouldBe` False
    it "--no-retrieval turns the facts off, the library stays" $ do
      let ra = ok ["F.lean", "--no-retrieval"]
      factsConfig ra `shouldBe` Nothing
      raLibrary ra `shouldBe` True
      retrievalWanted ra `shouldBe` True
    it "--no-library turns the library off" $ do
      raLibrary (ok ["F.lean", "--no-library"]) `shouldBe` False
      retrievalWanted (ok ["F.lean", "--no-library"]) `shouldBe` True
    it "with both off nothing is built" $
      retrievalWanted (ok ["F.lean", "--no-retrieval", "--no-library"]) `shouldBe` False
    it "--facts and --facts-chars set the caps" $
      fmap (\c -> (fcMax c, fcChars c)) (factsConfig (ok ["F.lean", "--facts", "5", "--facts-chars", "1200"])) `shouldBe` Just (5, 1200)
    it "--library-dir" $ raLibraryDir (ok ["F.lean", "--library-dir", "/tmp/lib"]) `shouldBe` Just "/tmp/lib"
    it "with --graph too" $ factsConfig (ok ["F.lean", "--graph", "--facts", "3"]) `shouldBe` Just defaultFacts {fcMax = 3}
    it "--print-facts" $ raPrintFacts (ok ["F.lean", "--print-facts"]) `shouldBe` True
    it "--library-read-only" $ do
      raLibraryReadOnly (ok ["F.lean"]) `shouldBe` False
      raLibraryReadOnly (ok ["F.lean", "--library-read-only"]) `shouldBe` True
    it "the library directory: per project under the Fisher home by default" $ do
      let ra = ok ["F.lean"]
      libraryDirFor ra "/home/g" "/w/a" `shouldBe` libraryDir "/home/g" "/w/a"
      libraryDirFor ra "/home/g" "/w/a" `shouldNotBe` libraryDirFor ra "/home/g" "/w/b"
      indexCacheFor ra "/home/g" "/w/a" `shouldBe` libraryDir "/home/g" "/w/a" </> "index.json"
    it "--library-dir names the library itself: two workspaces share it, each with its own index cache" $ do
      let ra = ok ["F.lean", "--library-dir", "/bench/lib"]
      libraryDirFor ra "/home/g" "/w/a" `shouldBe` "/bench/lib"
      libraryDirFor ra "/home/g" "/w/b" `shouldBe` "/bench/lib"
      indexCacheFor ra "/home/g" "/w/a" `shouldNotBe` indexCacheFor ra "/home/g" "/w/b"
      takeDirectory (indexCacheFor ra "/home/g" "/w/a") `shouldBe` "/bench/lib"
    it "--library-dir: what one workspace saves the other loads" $
      withSystemTempDirectory "lib" $ \d -> do
        let ra = ok ["F.lean", "--library-dir", d]
            lemma = Lemma "P.x" "theorem P.x : True :=" " trivial" "F.lean" "F" "run" "" [] []
        _ <- saveLibrary (libraryDirFor ra "/g" "/w/a") "/w/a" [lemma] []
        (ls, _) <- loadLibrary (libraryDirFor ra "/g" "/w/b")
        map lmName ls `shouldBe` ["P.x"]
    it "--print-facts loads the library read-only and writes nothing to its directory" $
      withSystemTempDirectory "pf" $ \d -> do
        let root = d </> "proj"
            lib = d </> "lib"
            home = d </> ".fisher"
        createDirectoryIfMissing True root
        BS.writeFile (root </> "lakefile.lean") "import Lake\nopen Lake DSL\npackage p\n"
        BS.writeFile (root </> "F.lean") "theorem t : True := by\n  sorry\n"
        createDirectoryIfMissing True lib
        _ <- saveLibrary lib root [Lemma "P.x" "theorem P.x : True :=" " trivial" "F.lean" "F" "run" "" [] []] []
        -- a bad line the read-write load would set aside
        BS.appendFile (lib </> "lemmas.jsonl") "{bad\n{\"also\": \"bad\"}\n"
        let snapshot = do
              fs <- sort <$> listDirectory lib
              mapM (\f -> (,) f <$> BS.readFile (lib </> f)) fs
        before <- snapshot
        -- the index cache: a private temporary directory, gone afterwards
        tmpd <- (d </>) <$> pure "tmp"
        createDirectoryIfMissing True tmpd
        oldTmp <- lookupEnv "TMPDIR"
        setEnv "TMPDIR" tmpd
        (mre, _) <- buildRetrievalAt True home (ok ["F.lean", "--print-facts", "--library-dir", lib]) root ""
        fmap (map lmName . fst) (mre >>= reLibrary) `shouldBe` Just ["P.x"]
        fmap reHarvest mre `shouldBe` Just False
        snapshot `shouldReturn` before
        listDirectory tmpd `shouldReturn` []
        maybe (unsetEnv "TMPDIR") (setEnv "TMPDIR") oldTmp
        -- without --library-dir, nothing under the Fisher home either
        (mre2, _) <- buildRetrievalAt True home (ok ["F.lean", "--print-facts"]) root ""
        isJust mre2 `shouldBe` True
        doesDirectoryExist (home </> "fisher") `shouldReturn` False
    it "bad values are usage errors" $ do
      run ["F.lean", "--facts", "0"] `shouldBe` Nothing
      run ["F.lean", "--facts", "x"] `shouldBe` Nothing
      run ["F.lean", "--facts-chars", "-1"] `shouldBe` Nothing
      run ["F.lean", "--library-dir"] `shouldBe` Nothing
    it "the usage text lists them" $
      mapM_ (\f -> runUsageText `shouldSatisfy` isInfixOf f) ["--no-retrieval", "--no-library", "--facts N", "--facts-chars N", "--library-dir DIR", "--library-read-only", "--print-facts"]
  describe "SIGTERM" $ do
    it "throws an asynchronous Terminated to the thread that asked, as Ctrl-C throws its interrupt" $ do
      got <- newEmptyMVar
      ready <- newEmptyMVar
      _ <- forkIO $ do
        r <- try (termToException >> putMVar ready () >> threadDelay 10000000)
        putMVar got (either (\e -> (fromException (e :: SomeException), fmap (const ()) (fromException e :: Maybe SomeAsyncException))) (const (Nothing, Nothing)) r)
      takeMVar ready
      raiseSignal sigTERM
      r <- timeout 5000000 (takeMVar got)
      _ <- installHandler sigTERM Default Nothing
      r `shouldBe` Just (Just Terminated, Just ())
    it "Terminated reaching the top exits with code 143" $
      (try (exitOnTerminated (throwIO Terminated)) :: IO (Either ExitCode ())) `shouldReturn` Left (ExitFailure 143)
  describe "the automation's flags" $ do
    it "on by default, with the default settings" $ autoConfig (ok ["F.lean"]) `shouldBe` Just defaultAutoConfig
    it "--auto is the default" $ autoConfig (ok ["F.lean", "--auto"]) `shouldBe` Just defaultAutoConfig
    it "--no-auto turns it off" $ autoConfig (ok ["F.lean", "--no-auto"]) `shouldBe` Nothing
    it "--no-auto with --graph" $ autoConfig (ok ["F.lean", "--graph", "--no-auto"]) `shouldBe` Nothing
    it "--no-repair" $ fmap acRepair (autoConfig (ok ["F.lean", "--no-repair"])) `shouldBe` Just False
    it "--no-disprove" $ fmap acDisprove (autoConfig (ok ["F.lean", "--no-disprove"])) `shouldBe` Just False
    it "--auto-tactics" $ fmap acTactics (autoConfig (ok ["F.lean", "--auto-tactics", "omega,simp"])) `shouldBe` Just ["omega", "simp"]
    it "--auto-seconds" $ fmap acSeconds (autoConfig (ok ["F.lean", "--auto-seconds", "5"])) `shouldBe` Just 5
    it "--repair-seconds, default 120" $ do
      fmap acRepairSeconds (autoConfig (ok ["F.lean"])) `shouldBe` Just 120
      fmap acRepairSeconds (autoConfig (ok ["F.lean", "--repair-seconds", "30"])) `shouldBe` Just 30
    it "--auto-warmup-seconds, default 600" $ do
      raAutoWarmup (ok ["F.lean"]) `shouldBe` 600
      raAutoWarmup (ok ["F.lean", "--auto-warmup-seconds", "90"]) `shouldBe` 90
    it "the server starts only with auto on and not --single" $ do
      serverWanted (ok ["F.lean"]) `shouldBe` True
      serverWanted (ok ["F.lean", "--graph"]) `shouldBe` True
      serverWanted (ok ["F.lean", "--no-auto"]) `shouldBe` False
      serverWanted (ok ["F.lean", "--single", "fisher"]) `shouldBe` False
    it "bad values are usage errors" $ do
      run ["F.lean", "--auto-seconds", "0"] `shouldBe` Nothing
      run ["F.lean", "--auto-seconds", "-3"] `shouldBe` Nothing
      run ["F.lean", "--auto-seconds", "soon"] `shouldBe` Nothing
      run ["F.lean", "--auto-tactics", "omega,,simp"] `shouldBe` Nothing
      run ["F.lean", "--auto-tactics"] `shouldBe` Nothing
      run ["F.lean", "--repair-seconds", "0"] `shouldBe` Nothing
      run ["F.lean", "--auto-warmup-seconds", "never"] `shouldBe` Nothing
    it "the usage text lists them" $
      mapM_ (\f -> runUsageText `shouldSatisfy` isInfixOf f) ["--auto", "--no-auto", "--no-repair", "--no-disprove", "--auto-tactics", "--auto-seconds", "--repair-seconds", "--auto-warmup-seconds"]
  describe "goalTable" $
    it "a column for what the automation did" $
      goalTable (SearchReport
        [ ("a", gs Closed (Just "auto") Nothing), ("bb", gs Closed (Just "model") Nothing)
        , ("c", gs Closed (Just "model+repair") Nothing), ("d", gs Disproved Nothing (Just (DFalse "decide")))
        , ("e", gs GaveUp Nothing (Just (DSuspect "x := 0"))) ] [] 0 4 False "" True 1 True Nothing Nothing False 0)
        `shouldBe`
          [ "goal  status     auto", "a     closed     closed", "bb    closed     -", "c     closed     repair"
          , "d     disproved  disproved", "e     gave up    suspect" ]
  describe "--graph arguments" $ do
    it "the defaults" $ do
      let ra = ok ["F.lean", "--graph"]
      raFile ra `shouldBe` Just "F.lean"
      raGraph ra `shouldBe` True
      raBackends ra `shouldBe` ["fisher"]
      raSlots ra `shouldBe` 4
      raMaxDepth ra `shouldBe` 3
      raMaxSplits ra `shouldBe` 2
      raCapUsd ra `shouldBe` 25
      raModel ra `shouldBe` Nothing
    it "--max-depth" $ raMaxDepth (ok ["F.lean", "--graph", "--max-depth", "5"]) `shouldBe` 5
    it "--max-splits" $ raMaxSplits (ok ["F.lean", "--graph", "--max-splits", "1"]) `shouldBe` 1
    it "--cap-usd" $ raCapUsd (ok ["F.lean", "--graph", "--cap-usd", "7.5"]) `shouldBe` 7.5
    it "--cap-usd as a whole number" $ raCapUsd (ok ["F.lean", "--cap-usd", "40", "--graph"]) `shouldBe` 40
    it "--model" $ raModel (ok ["F.lean", "--graph", "--model", "openai/gpt-5.5"]) `shouldBe` Just "openai/gpt-5.5"
    it "today's flags still apply" $ do
      let ra = ok ["F.lean", "--graph", "--parallel", "2", "--budget", "1h", "--attempt-minutes", "10", "--max-turns", "20", "--apply", "--out", "/tmp/o"]
      (raSlots ra, raBudget ra, raAttemptSecs ra, raMaxTurns ra, raApply ra, raOut ra) `shouldBe` (2, 3600, 600, 20, True, Just "/tmp/o")
    it "--backends fisher" $ raBackends (ok ["F.lean", "--graph", "--backends", "fisher"]) `shouldBe` ["fisher"]
    it "--backends fisher,fisher" $ raBackends (ok ["F.lean", "--graph", "--backends", "fisher,fisher"]) `shouldBe` ["fisher"]
    it "refuses another backend" $
      run ["F.lean", "--graph", "--backends", "fisher,claude-code"] `shouldBe` Just (Left "--graph runs on the native backend only (fisher)")
    it "refuses --single" $
      run ["F.lean", "--graph", "--single", "fisher"] `shouldBe` Just (Left "--graph does not take --single or --one-per-goal")
    it "refuses --one-per-goal" $
      run ["F.lean", "--one-per-goal", "--graph"] `shouldBe` Just (Left "--graph does not take --single or --one-per-goal")
    it "the graph flags need --graph" $
      run ["F.lean", "--max-depth", "2"] `shouldBe` Just (Left "--max-depth, --max-splits and --cap-usd need --graph")
    it "--model needs no --graph" $ raModel (ok ["F.lean", "--model", "m/x"]) `shouldBe` Just "m/x"
    it "bad values are usage errors" $ do
      run ["F.lean", "--graph", "--max-depth", "x"] `shouldBe` Nothing
      run ["F.lean", "--graph", "--max-splits", "-1"] `shouldBe` Nothing
      run ["F.lean", "--graph", "--cap-usd", "0"] `shouldBe` Nothing
  describe "without --graph" $ do
    it "is today's run" $ do
      let ra = ok ["F.lean", "--backends", "claude-code,codex"]
      raGraph ra `shouldBe` False
      raBackends ra `shouldBe` ["claude-code", "codex"]
      raBackends (ok ["F.lean"]) `shouldBe` ["claude-code", "fisher"]
    it "takes the native backend as fisher" $ do
      let ra = ok ["F.lean", "--backends", "claude-code,fisher,codex"]
      raBackends ra `shouldBe` ["claude-code", "fisher", "codex"]
      map isNative (raBackends ra) `shouldBe` [False, True, False]
      raSingle (ok ["F.lean", "--single", "codex"]) `shouldBe` Just "codex"
    it "refuses the native backend's previous name, and any unknown backend, with the list" $ do
      let refusal b = Just (Left ("unknown backend `" <> b <> "`; the backends are: claude-code, codex, fisher"))
      run ["F.lean", "--backends", oldName] `shouldBe` refusal (T.pack oldName)
      run ["F.lean", "--backends", "claude-code," <> oldName] `shouldBe` refusal (T.pack oldName)
      run ["F.lean", "--graph", "--backends", oldName] `shouldBe` refusal (T.pack oldName)
      run ["F.lean", "--single", map toUpper oldName] `shouldBe` refusal (T.pack (map toUpper oldName))
      run ["F.lean", "--backends", "nope"] `shouldBe` refusal "nope"
      isNative (T.pack oldName) `shouldBe` False
  describe "attemptEnv" $
    it "sets the backend and the state directory, over their old values, under Fisher's names only" $ do
      let base = Map.fromList [("FISHER_AUTOFORMALIZE_BACKEND", "codex"), ("FISHER_AUTOFORMALIZE_MANAGED_STATE_DIR", "/old"), ("PATH", "/bin")]
          env = attemptEnv "fisher" (Just "/attempt/managed") base
      Map.lookup "FISHER_AUTOFORMALIZE_BACKEND" env `shouldBe` Just "fisher"
      Map.lookup "FISHER_AUTOFORMALIZE_MANAGED_STATE_DIR" env `shouldBe` Just "/attempt/managed"
      Map.lookup "PATH" env `shouldBe` Just "/bin"
      Map.keys env `shouldBe` ["FISHER_AUTOFORMALIZE_BACKEND", "FISHER_AUTOFORMALIZE_MANAGED_STATE_DIR", "PATH"]
      Map.lookup "FISHER_AUTOFORMALIZE_MANAGED_STATE_DIR" (attemptEnv "fisher" Nothing base) `shouldBe` Just "/old"
  describe "setModel" $ do
    it "replaces the value after -m" $
      setModel "M" ["fisher", "chat", "--provider", "openrouter", "-m", "old", "-t", "x"]
        `shouldBe` Right ["fisher", "chat", "--provider", "openrouter", "-m", "M", "-t", "x"]
    it "replaces the value after --model" $ setModel "M" ["a", "--model", "old"] `shouldBe` Right ["a", "--model", "M"]
    it "errors without -m" $ setModel "M" ["a", "b"] `shouldSatisfy` either (const True) (const False)
  describe "withBrief" $ do
    it "no brief" $ withBrief "prompt" "" `shouldBe` "prompt"
    it "a blank line, then the brief" $ withBrief "prompt" "the brief\n" `shouldBe` "prompt\n\nthe brief\n"
  describe "askResult" $ do
    it "a reply and its cost" $ askResult (Right (Reply "hi" (usage 0.02) "m")) `shouldBe` Right ("hi", 0.02)
    it "a reply without a cost" $ askResult (Right (Reply "hi" (Usage 1 1 2 Nothing Nothing Nothing) "m")) `shouldBe` Right ("hi", 0)
    it "no credit" $ askResult (Left NoCredit) `shouldBe` Left AskNoCredit
    it "another error" $ askResult (Left (OtherError "HTTP 503")) `shouldBe` Left (AskFailed "HTTP 503")
    it "an empty reply" $ askResult (Left Empty) `shouldBe` Left (AskFailed "empty reply")
    it "the judge's: a reply without a cost is charged the estimate" $ do
      judgeAskResult (Right (Reply "hi" (Usage 1 1 2 Nothing Nothing Nothing) "m")) `shouldBe` Right ("hi", judgeCostEstimate)
      judgeCostEstimate `shouldBe` 0.02
      judgeAskResult (Right (Reply "hi" (usage 0.005) "m")) `shouldBe` Right ("hi", 0.005)
      judgeAskResult (Left NoCredit) `shouldBe` Left AskNoCredit
    it "a refusal" $ askResult (Left (Refused "content filter")) `shouldBe` Left (AskFailed "refused: content filter")
  describe "configuredModel" $ do
    it "a string" $ configuredModel (obj [("model", str "z-ai/glm-5")]) `shouldBe` "z-ai/glm-5"
    it "a section's default" $ configuredModel (obj [("model", obj [("default", str "z-ai/glm-5"), ("provider", str "openrouter")])]) `shouldBe` "z-ai/glm-5"
    it "none: the native agent's default" $ configuredModel (obj []) `shouldBe` "anthropic/claude-opus-4.6"
  describe "the compute controller's flags" $ do
    it "--tiers parses to the list" $
      raTiers (ok ["F.lean", "--tiers", "example/model-a,example/model-b"]) `shouldBe` Just ["example/model-a", "example/model-b"]
    it "refuses an empty --tiers or an empty name in it" $ do
      run ["F.lean", "--tiers", ""] `shouldBe` Nothing
      run ["F.lean", "--tiers", "example/model-a,,example/model-b"] `shouldBe` Nothing
    it "refuses a tier name with whitespace in it" $ do
      run ["F.lean", "--tiers", "example/model a,example/model-b"] `shouldBe` Nothing
      run ["F.lean", "--tiers", "example/model-a,example/mo\tdel-b"] `shouldBe` Nothing
      raTiers (ok ["F.lean", "--tiers", "example/model-a, example/model-b"]) `shouldBe` Just ["example/model-a", "example/model-b"]
    it "refuses --stall-turns 0 and --stall-dollars 0" $ do
      run ["F.lean", "--stall-turns", "0"] `shouldBe` Nothing
      run ["F.lean", "--stall-dollars", "0"] `shouldBe` Nothing
    it "the stall rule's defaults and flags" $ do
      (raStallTurns (ok ["F.lean"]), raStallDollars (ok ["F.lean"]), raEarlyStop (ok ["F.lean"]), raCascade (ok ["F.lean"]))
        `shouldBe` (8, 1.5, True, True)
      raStallTurns (ok ["F.lean", "--stall-turns", "2"]) `shouldBe` 2
      raStallDollars (ok ["F.lean", "--stall-dollars", "0.75"]) `shouldBe` 0.75
      raEarlyStop (ok ["F.lean", "--no-early-stop"]) `shouldBe` False
    it "--no-cascade --tiers a,b: one model, b, as --model is today, and early stop off" $
      cascadeFor (ok ["F.lean", "--no-cascade", "--tiers", "a,b"]) [] `shouldBe` Cascade [] (Just "b") False False
    it "--no-cascade: the config's last tier, else --model" $ do
      cascadeFor (ok ["F.lean", "--no-cascade", "--model", "m"]) ["a", "b"] `shouldBe` Cascade [] (Just "b") False False
      cascadeFor (ok ["F.lean", "--no-cascade", "--model", "m"]) [] `shouldBe` Cascade [] (Just "m") False False
    it "two tiers: the controller on, the flag over the config" $ do
      cascadeFor (ok ["F.lean", "--tiers", "a,b"]) ["c"] `shouldBe` Cascade ["a", "b"] Nothing True True
      cascadeFor (ok ["F.lean"]) ["c", "d"] `shouldBe` Cascade ["c", "d"] Nothing True True
      cascadeFor (ok ["F.lean", "--tiers", "a,b", "--no-early-stop"]) [] `shouldBe` Cascade ["a", "b"] Nothing True False
    it "one tier: the controller on with early stop" $ do
      cascadeFor (ok ["F.lean", "--tiers", "a"]) [] `shouldBe` Cascade ["a"] Nothing True True
    it "--tiers example/model-a --no-early-stop gives scControl False, model example/model-a, no watching" $ do
      let ra = ok ["F.lean", "--tiers", "example/model-a", "--no-early-stop"]
          c = cascadeFor ra []
      c `shouldBe` Cascade [] (Just "example/model-a") False False
      scControl (searchConfigFor ra c) `shouldBe` False
      scTiers (searchConfigFor ra c) `shouldBe` []
      attemptWatched c "t1" `shouldBe` False
      cascadeFor (ok ["F.lean", "--no-early-stop"]) ["example/model-a"] `shouldBe` Cascade [] (Just "example/model-a") False False
      cascadeFor (ok ["F.lean"]) ["c"] `shouldBe` Cascade ["c"] Nothing True True
    it "no tiers: today's search, whatever the stall flags say" $ do
      cascadeFor (ok ["F.lean", "--model", "m"]) [] `shouldBe` Cascade [] (Just "m") False False
      cascadeFor (ok ["F.lean", "--no-early-stop"]) [] `shouldBe` Cascade [] Nothing False False
      cascadeFor (ok ["F.lean", "--stall-turns", "2", "--stall-dollars", "0.5"]) [] `shouldBe` Cascade [] Nothing False False
    it "no flags and no config: scControl off and no attempt watched" $ do
      let ra = ok ["F.lean"]
          c = cascadeFor ra []
      scControl (searchConfigFor ra c) `shouldBe` False
      scTiers (searchConfigFor ra c) `shouldBe` []
      attemptWatched c "t1" `shouldBe` False
    it "with tiers every attempt is watched but the --single baseline's" $ do
      let ra = ok ["F.lean", "--tiers", "example/model-a,example/model-b"]
          c = cascadeFor ra []
      scControl (searchConfigFor ra c) `shouldBe` True
      scTiers (searchConfigFor ra c) `shouldBe` ["example/model-a", "example/model-b"]
      -- the default backends: claude-code is outside the tiers
      scUntiered (searchConfigFor ra c) `shouldBe` ["claude-code"]
      scUntiered (searchConfigFor (ok ["F.lean", "--backends", "fisher,codex", "--tiers", "example/model-a,example/model-b"]) c) `shouldBe` ["codex"]
      attemptWatched c "t1" `shouldBe` True
      attemptWatched c "*" `shouldBe` False
    it "searchTiersConfig reads fisher.search.tiers only" $ do
      searchTiersConfig (A.object ["fisher" A..= A.object ["search" A..= A.object ["tiers" A..= (["a", "b"] :: [Text])]]]) `shouldBe` ["a", "b"]
      searchTiersConfig (A.object [K.fromString oldName A..= A.object ["search" A..= A.object ["tiers" A..= (["a", "b"] :: [Text])]]]) `shouldBe` []
      searchTiersConfig (A.object []) `shouldBe` []
      searchTiersConfig (A.object ["fisher" A..= A.object ["search" A..= A.object ["tiers" A..= ("a" :: Text)]]]) `shouldBe` []
    it "the usage names each flag" $
      mapM_ (\f -> runUsageText `shouldSatisfy` isInfixOf f) ["--tiers", "--no-cascade", "--stall-turns", "--stall-dollars", "--no-early-stop"]
  describe "per attempt models and the ledger" $ do
    it "a stream backend's model goes into the workflow command" $ do
      modelCommand (Just "example/model-a") "/prove F.lean --planning=off" `shouldBe` "/prove F.lean --planning=off --model example/model-a"
      modelCommand Nothing "/prove F.lean" `shouldBe` "/prove F.lean"
    it "the attempt's ledger tag: the run directory's name, the run id and the attempt" $
      attemptTag "/home/u/.cache/fisher/search/20261016-101500" "0a1b2c3d" 7 `shouldBe` "search-20261016-101500-0a1b2c3d-7"
    it "two runs with one --out name get distinct tags" $ do
      a <- newRunId
      threadDelay 2000
      b <- newRunId
      map T.length [a, b] `shouldBe` [8, 8]
      T.all (`elem` ("0123456789abcdef" :: String)) (a <> b) `shouldBe` True
      a `shouldNotBe` b
      attemptTag "runs/K/h1" a 1 `shouldNotBe` attemptTag "/elsewhere/runs/S/h1/" b 1
    it "an attempt's dollars: the ledger's; after the stream fallback the larger of the ledger's and cost_usd" $ do
      let stats c = Just (ChildStats (Just 0) 1 Nothing Nothing Nothing c Nothing Map.empty)
          w d unread = Watched Nothing d Nothing unread
      attemptDollars (w (Just 0.2) Nothing) (stats (Just 0.9)) `shouldBe` Just 0.2
      attemptDollars (w (Just 0.2) (Just "busy")) (stats (Just 0.9)) `shouldBe` Just 0.9
      attemptDollars (w (Just 0.7) (Just "busy")) (stats (Just 0.4)) `shouldBe` Just 0.7
      attemptDollars (w Nothing (Just "busy")) (stats (Just 0.4)) `shouldBe` Just 0.4
      attemptDollars (w (Just 0.3) (Just "busy")) (stats Nothing) `shouldBe` Just 0.3
      attemptDollars (w Nothing Nothing) (stats (Just 0.4)) `shouldBe` Nothing
    it "the ledger variables only with the controller on" $ do
      ledgerEnv True "tag-1" (Map.fromList [("A", "1")]) `shouldBe` Map.fromList [("A", "1"), ("FISHER_UI_LEDGER", "1"), ("FISHER_UI_TAG", "tag-1")]
      ledgerEnv False "tag-1" (Map.fromList [("A", "1")]) `shouldBe` Map.fromList [("A", "1")]
  runlogSpec
  learnSpec
  lessonsSpec
  smtSpec
  recordSpec

-- | The run log of a search: the pure pieces, then the writers on a
-- temporary Fisher home (runMain itself needs a Lake project and live
-- children, so its wiring is on the Mac live list).
runlogSpec :: Spec
runlogSpec = describe "the run log" $ do
  let st status by at closedBy = GoalState status ["fisher"] [] by at closedBy Nothing Nothing False False Nothing Nothing Nothing []
      rec i g o s c = (plainRecord i g "fisher" o s (s + 1) Nothing) {arCost = c}
      rep = SearchReport
        { srGoals =
            [ ("t", st Closed (Just (2, "fisher")) (Just 10) (Just "model"))
            , ("u", st Closed Nothing (Just 3) (Just "auto"))
            , ("v", st Disproved Nothing Nothing Nothing)
            , ("w", st Open Nothing Nothing Nothing)
            , ("x", st Waiting (Just (6, "fisher")) Nothing (Just "model")) ]
        , srAttempts =
            [ rec 1 "t" "rejected: no" 0 (Just 0.5), rec 2 "t" "closed" 2 (Just 1.25), rec 3 "t" "cancelled: the goal closed" 4 Nothing
            , rec 5 "t" "closed" 12 (Just 9), rec 6 "x" "waiting on open goals" 1 (Just 0.5), rec 7 "w" "rejected: no" 1 Nothing ]
        , srElapsed = 20, srClosed = 2, srBudgetSpent = False, srBase = "", srAuto = True, srRepairs = 0
        , srAutoRan = True, srAutoOff = Nothing, srRetrieval = Nothing, srControl = False, srEscalations = 0 }
      stmts = [("t", "theorem t (n : Nat) : n + 0 = n := by"), ("u", "theorem u : 1 ≤ 2 := by")]

  it "goalRows: statement and shape, status, who closed it, its seconds and the dollars of its attempts up to the close" $ do
    let rows = goalRows stmts rep
    map grName rows `shouldBe` ["t", "u", "v", "w", "x"]
    map grStatus rows `shouldBe` ["closed", "closed", "disproved", "open", "waiting"]
    map grClosedBy rows `shouldBe` [Just "attempt:2", Just "auto", Just "disproved", Nothing, Nothing]
    map grSecondsToClose rows `shouldBe` [Just 10, Just 3, Nothing, Nothing, Nothing]
    map grDollarsToClose rows `shouldBe` [Just 1.75, Just 0, Nothing, Nothing, Nothing]
    map grStatement rows `shouldBe` ["theorem t (n : Nat) : n + 0 = n := by", "theorem u : 1 ≤ 2 := by", "v", "w", "x"]
    -- a goal of no known statement has the empty shape, which the
    -- statistics skip
    map grShape rows `shouldBe` map (renderShape . shapeOf . snd) stmts <> ["", "", ""]
    all (not . T.null . grShape) (take 2 rows) `shouldBe` True

  it "goalRows: the --single baseline's goal * has no row" $
    map grName (goalRows stmts rep {srGoals = ("*", st Closed (Just (1, "fisher")) (Just 5) (Just "model")) : srGoals rep})
      `shouldBe` ["t", "u", "v", "w", "x"]

  it "an attempt's end keeps the cost, tokens and turns its own end wrote" $ do
    let e = RunEnd 5 (Just 0) "closed" (Just 0.3) (Just 10) (Just 2) (Just 3)
        row c = RunRow "a" SearchAttempt Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] 0 (Just 4) Nothing (Just "completed") c (Just 99) Nothing Nothing Nothing Full
    keepChildEnd Nothing e `shouldBe` e
    keepChildEnd (Just (row (Just 0.5))) e `shouldBe` e {reCostUsd = Just 0.5, reInputTokens = Just 99}
    keepChildEnd (Just (row Nothing)) e `shouldBe` e {reInputTokens = Just 99}

  it "goalRows: a close with no priced attempt has no dollars" $ do
    let rep' = rep {srAttempts = [rec 2 "t" "closed" 2 Nothing]}
    grDollarsToClose (head (goalRows stmts rep')) `shouldBe` Nothing

  it "each search event's kind is eventJson's event name, its data the object, its t the event's" $ do
    let ev e = eventRow (eventJson e)
        (_, k1, d1) = ev (AutoClosed 2.04 "t" "simp" 1.0)
    k1 `shouldBe` "auto closed"
    d1 `shouldBe` eventJson (AutoClosed 2.04 "t" "simp" 1.0)
    [k | (_, k, _) <- map ev [AutoMissed 1 "t" 2, TierChosen 1 2 "t" 0 "m", Launched 1 1 "t" "fisher", PolicyApplied 1 "t" "backend" "a" "b" A.Null False]]
      `shouldBe` ["auto missed", "tier chosen", "launched", "policy"]
    [t | (t, _, _) <- map ev [Launched 7.26 1 "t" "fisher", BudgetSpent 0]] `shouldBe` [7.3, 0]
    eventRow (A.object []) `shouldBe` (0, "event", A.object [])

  it "stream events spread over the attempt's wall time, or at 0 when it is unknown" $ do
    let se t k = StreamEvent t k A.Null
        evs = [se Nothing "message", se Nothing "tool_start", se (Just 1.5) "tool_end", se Nothing "cost"]
    [t | (t, _, _) <- streamTimes (Just 10) evs] `shouldBe` [2.5, 5, 1.5, 10]
    [t | (t, _, _) <- streamTimes Nothing evs] `shouldBe` [0, 0, 1.5, 0]
    [k | (_, k, _) <- streamTimes Nothing evs] `shouldBe` ["message", "tool_start", "tool_end", "cost"]

  it "the child's run-log variables: its id and its parent" $
    attemptRunEnv "search-a-1" "search-a" (Map.fromList [("A", "1")])
      `shouldBe` Map.fromList [("A", "1"), ("FISHER_RUN_ID", "search-a-1"), ("FISHER_RUN_PARENT", "search-a")]

  it "an attempt's end: the search's start plus its end, exit, class, dollars, tokens and turns" $ do
    let stats = ChildStats (Just 0) 9 (Just 4) (Just 100) (Just 20) (Just 0.3) Nothing Map.empty
        a = (plainRecord 2 "t" "fisher" "closed" 2 12 (Just stats)) {arCost = Just 0.35}
    attemptEnd 100 a `shouldBe` RunEnd 112 (Just 0) "closed" (Just 0.35) (Just 100) (Just 20) (Just 4)

  it "a search's outcome: closed when every goal closed, budget when it was spent, else failed" $ do
    searchOutcome rep `shouldBe` "failed"
    searchOutcome rep {srBudgetSpent = True} `shouldBe` "budget"
    searchOutcome rep {srGoals = take 2 (srGoals rep)} `shouldBe` "closed"
    searchOutcome rep {srGoals = []} `shouldBe` "failed"

  let parent = "search-20261024-120000-ab12cd34"
      tag :: Int -> Text
      tag i = parent <> "-" <> T.pack (show i)
      new home i k p = NewRun
        { nrId = i, nrKind = k, nrParent = p, nrProject = Just (home </> "proj"), nrProver = Just "lean"
        , nrBackend = Just "fisher", nrModel = Nothing, nrTier = Nothing, nrWorkflow = Just "prove"
        , nrArgv = ["search", "F.lean"], nrStartedAt = 100, nrRawDir = Just home, nrCapture = Full }
      -- a log on a temporary home whose search started at 100, with
      -- attempts 1 and 2 begun
      withLog k = withSystemTempDirectory "fisher-search-runlog" $ \home -> do
        sl <- openSearchLog home tag (pure 100) (new home parent SearchRun Nothing)
        written <- mapM (\i -> searchLogBegin sl i (new home (tag i) SearchAttempt (Just parent)) (attemptEventData "t" "theorem t : 1 = 1 := by" "fisher" (Just 1200))) [1, 2]
        written `shouldBe` [True, True]
        () <- k sl
        closeSearchLog sl
        withRunlog (runlogPath home) $ \rl -> do
          kids <- listRuns rl Nothing Nothing (Just SearchAttempt) 0
          ps <- maybe [] pure <$> getRun rl parent
          evs <- mapM (runEvents rl . rrId) kids
          gsr <- runGoals rl parent
          pure (ps, sort [(rrId r, rrParent r, rrOutcome r, rrEndedAt r) | r <- kids], evs, gsr)
      stats0 = ChildStats (Just 0) 3 (Just 2) Nothing Nothing (Just 0.2) Nothing Map.empty

  it "a search on a temporary home leaves a search run, two search-attempt children and its goal rows" $ do
    let two = rep {srGoals = take 1 (srGoals rep), srAttempts = take 2 (srAttempts rep)}
    (ps, kids, evs, gsr) <- withLog $ \sl -> do
      searchLogEvent sl (eventJson (Launched 1 1 "t" "fisher"))
      finishSearchLog sl (srAttempts two) (goalRows stmts two) (RunEnd 130 Nothing (searchOutcome two) Nothing Nothing Nothing Nothing)
    [(rrKind p, rrOutcome p, rrEndedAt p) | p <- ps] `shouldBe` [(SearchRun, Just "closed", Just 130)]
    kids `shouldBe` [(tag 1, Just parent, Just "failed", Just 101), (tag 2, Just parent, Just "closed", Just 103)]
    map (\g -> (grName g, grStatus g, grClosedBy g)) gsr `shouldBe` [("t", "closed", Just "attempt:2")]
    all (not . T.null . grShape) gsr `shouldBe` True
    -- each attempt's first event names its goal, shape and backend
    [[(k, d) | (_, _, k, d) <- take 1 es] | es <- evs]
      `shouldBe` replicate 2 [("attempt", A.object [("goal", A.String "t"), ("shape", A.String (renderShape (shapeOf "theorem t : 1 = 1 := by"))), ("backend", A.String "fisher"), ("budget_secs", A.Number 1200)])]

  it "an attempt of no known statement (goal *, a lemma-graph node) has the empty shape in its attempt event" $ do
    attemptEventData "*" (knownStatement stmts "*") "fisher" (Just 7200)
      `shouldBe` A.object [("goal", A.String "*"), ("shape", A.String ""), ("backend", A.String "fisher"), ("budget_secs", A.Number 7200)]
    attemptEventData "t" "theorem t : 1 = 1 := by" "fisher" Nothing
      `shouldBe` A.object [("goal", A.String "t"), ("shape", A.String (renderShape (shapeOf "theorem t : 1 = 1 := by"))), ("backend", A.String "fisher")]
    knownStatement stmts "t" `shouldBe` "theorem t (n : Nat) : n + 0 = n := by"
    knownStatement stmts "lemma_9" `shouldBe` ""

  it "a Finished event ends its attempt with the class at once, the stop from its attempt stopped event" $ do
    (_, kids, _, _) <- withLog $ \sl -> do
      searchLogSearchEvent sl (AttemptStopped 4 1 "t" "turns" 8 0.3)
      searchLogSearchEvent sl (Finished 5 1 "t" "fisher" "rejected: x" (Just stats0) Nothing)
      searchLogSearchEvent sl (Finished 7 2 "t" "fisher" "closed" (Just stats0) Nothing)
    kids `shouldBe` [(tag 1, Just parent, Just "stalled", Just 105), (tag 2, Just parent, Just "closed", Just 107)]

  it "an AttemptCancelled event ends its attempt cancelled after its goal closed, budget otherwise" $ do
    (_, kids, _, _) <- withLog $ \sl -> do
      searchLogSearchEvent sl (AttemptCancelled 3 1 "u" Nothing)
      searchLogSearchEvent sl (GoalClosed 4 "t" 9)
      searchLogSearchEvent sl (AttemptCancelled 5 2 "t" (Just 0.1))
    kids `shouldBe` [(tag 1, Just parent, Just "budget", Just 103), (tag 2, Just parent, Just "cancelled", Just 105)]

  it "an AttemptCancelled event after its goal went to waiting ends it cancelled, not budget" $ do
    (_, kids, _, _) <- withLog $ \sl -> do
      searchLogSearchEvent sl (GoalWaiting 4 "t" 9)
      searchLogSearchEvent sl (AttemptCancelled 5 1 "t" Nothing)
      searchLogSearchEvent sl (GoalDisproved 6 "u" "false" "decide" "plausible" Nothing)
      searchLogSearchEvent sl (AttemptCancelled 7 2 "u" Nothing)
    kids `shouldBe` [(tag 1, Just parent, Just "cancelled", Just 105), (tag 2, Just parent, Just "cancelled", Just 107)]

  it "budget spent ends the attempts still open as budget" $ do
    (_, kids, _, _) <- withLog $ \sl -> do
      searchLogSearchEvent sl (Finished 5 1 "t" "fisher" "closed" (Just stats0) Nothing)
      searchLogSearchEvent sl (BudgetSpent 9)
    kids `shouldBe` [(tag 1, Just parent, Just "closed", Just 105), (tag 2, Just parent, Just "budget", Just 109)]

  it "an interrupted search ends its running attempts cancelled and leaves none unknown" $ do
    (ps, kids, _, _) <- withLog $ \sl -> do
      searchLogSearchEvent sl (Finished 5 1 "t" "fisher" "rejected: x" (Just stats0) Nothing)
      searchLogInterrupted sl
      finishSearchLog sl [] [] (RunEnd 200 Nothing "cancelled" Nothing Nothing Nothing Nothing)
    map rrOutcome ps `shouldBe` [Just "cancelled"]
    [o | (_, _, o, _) <- kids] `shouldBe` [Just "failed", Just "cancelled"]

  it "a run log that cannot be opened logs nothing and stops nothing" $
    withSystemTempDirectory "fisher-search-runlog-bad" $ \tmp -> do
      let home = tmp </> "home"
      BS.writeFile home "a file, not a directory"
      sl <- openSearchLog home tag (pure 0) (new home "search-x" SearchRun Nothing)
      searchLogOpen sl `shouldBe` False
      -- the child is not told an id whose row was not written
      searchLogBegin sl 1 (new home (tag 1) SearchAttempt (Just "search-x")) A.Null `shouldReturn` False
      searchLogEvent sl (eventJson (BudgetSpent 1))
      searchLogSearchEvent sl (Finished 5 1 "t" "fisher" "closed" Nothing Nothing)
      searchLogInterrupted sl
      finishSearchLog sl [] [] (RunEnd 1 Nothing "failed" Nothing Nothing Nothing Nothing)
      closeSearchLog sl

learnSpec :: Spec
learnSpec = describe "the learned policy" $ do
  let stmtS = "theorem s (n : Nat) : n + 0 = n := by"
      shS = renderShape (shapeOf stmtS)
      gp bs ts tacs secs = GoalPolicy bs ts tacs secs False (A.object [])
      pol = Policy 0 "k" (Map.fromList [("*", gp ["claude-code", "fisher"] ["m1", "m2"] ["simp", "omega"] Nothing), (shS, gp ["fisher", "claude-code"] ["m2", "m1"] ["omega", "simp"] (Just 300))])
      goals = [("s", stmtS), ("o", "theorem o : True := by")]
  it "--no-learn turns it off; on by default" $ do
    raLearn (ok ["--no-learn", "F.lean"]) `shouldBe` False
    raLearn (ok ["F.lean"]) `shouldBe` True
  it "--attempt-minutes is noted as given" $ do
    raAttemptSecsGiven (ok ["F.lean", "--attempt-minutes", "5"]) `shouldBe` True
    raAttemptSecs (ok ["F.lean", "--attempt-minutes", "5"]) `shouldBe` 300
    raAttemptSecsGiven (ok ["F.lean"]) `shouldBe` False
  it "learnEnabled: off with --no-learn or fisher.learn.enabled false" $ do
    let cfg v = A.object ["fisher" A..= A.object ["learn" A..= A.object ["enabled" A..= v]]]
    learnEnabled (ok ["F.lean"]) (cfg False) `shouldBe` False
    learnEnabled (ok ["F.lean"]) (cfg True) `shouldBe` True
    learnEnabled (ok ["F.lean"]) (A.object []) `shouldBe` True
    learnEnabled (ok ["F.lean", "--no-learn"]) (A.object []) `shouldBe` False
  it "policyDefaults: the run's backends, tiers, tactics and budget" $ do
    let ra = ok ["F.lean", "--auto-tactics", "simp,omega", "--attempt-minutes", "10"]
    policyDefaults ra (cascadeFor ra ["m1", "m2"]) (autoConfig ra) `shouldBe` Defaults ["claude-code", "fisher"] ["m1", "m2"] ["simp", "omega"] 600
    let off = ok ["F.lean", "--no-auto"]
    dTactics (policyDefaults off (cascadeFor off []) (autoConfig off)) `shouldBe` []
  it "goalPolicies: each goal's shape entry, else the default's" $ do
    let m = goalPolicies (ok ["F.lean", "--auto-tactics", "simp,omega"]) pol goals
    gpBackends <$> Map.lookup "s" m `shouldBe` Just ["fisher", "claude-code"]
    gpBackends <$> Map.lookup "o" m `shouldBe` Just ["claude-code", "fisher"]
    gpAttemptSecs <$> Map.lookup "s" m `shouldBe` Just (Just 300)
  it "goalPolicies with --backends keeps the policy's tiers and replaces its backends" $ do
    let ra = ok ["F.lean", "--backends", "fisher,codex"]
        m = goalPolicies ra pol goals
    gpBackends <$> Map.lookup "s" m `shouldBe` Just ["fisher", "codex"]
    gpTiers <$> Map.lookup "s" m `shouldBe` Just ["m2", "m1"]
  it "goalPolicies with --tiers and --auto-tactics uses the flags' lists" $ do
    let m = goalPolicies (ok ["F.lean", "--tiers", "a,b", "--auto-tactics", "decide"]) pol goals
    gpTiers <$> Map.lookup "s" m `shouldBe` Just ["a", "b"]
    gpTactics <$> Map.lookup "s" m `shouldBe` Just ["decide"]
    gpBackends <$> Map.lookup "s" m `shouldBe` Just ["fisher", "claude-code"]
  it "goalPolicies with --attempt-minutes drops the learned budget" $
    gpAttemptSecs <$> Map.lookup "s" (goalPolicies (ok ["F.lean", "--attempt-minutes", "30"]) pol goals) `shouldBe` Just Nothing
  it "goalPolicies never raises the budget and keeps only the run's arms" $ do
    let raised = pol {pShapes = Map.insert shS (gp ["fisher", "codex"] [] ["omega", "ring"] (Just 5000)) (pShapes pol)}
        m = goalPolicies (ok ["F.lean"]) raised goals
    gpAttemptSecs <$> Map.lookup "s" m `shouldBe` Just Nothing
    gpBackends <$> Map.lookup "s" m `shouldBe` Just ["fisher"]
    gpTactics <$> Map.lookup "s" (goalPolicies (ok ["F.lean", "--no-auto"]) raised goals) `shouldBe` Just []
  it "withExploration: every eighth logged attempt of the goal's shape explores, never with --backends" $ do
    let st n = Stats (Map.fromList [((LShape, shS, KBackend, "fisher"), Cell n 0 0 0), ((LShape, shS, KBackend, "claude-code"), Cell 3 0 0 0), ((LProject, "", KBackend, "fisher"), Cell 50 0 0 0)]) Map.empty
        ra = ok ["F.lean"]
        m0 = goalPolicies ra pol goals
        explored r n = Map.keys (Map.filter gpExplore (withExploration r (st n) goals m0))
    explored ra 4 `shouldBe` ["s"]
    explored ra 3 `shouldBe` []
    explored (ok ["F.lean", "--backends", "fisher,claude-code"]) 4 `shouldBe` []
  it "the scheduler is told the configured attempt budget, for the attempt_secs policy event" $ do
    let ra = ok ["F.lean"]
        ra5 = ok ["F.lean", "--attempt-minutes", "5"]
    scAttemptSecs (searchConfigFor ra (cascadeFor ra [])) `shouldBe` Just 1200
    scAttemptSecs (searchConfigFor ra5 (cascadeFor ra5 [])) `shouldBe` Just 300
  it "learnApplied: per goal, only the knobs the policy moves off the configured ones" $ do
    -- the pre-pass's default tactics: the policy's omega (its simp is not
    -- one of them), for o by the "*" entry; with --auto-tactics the flag's
    -- order is the run's
    let ra = ok ["F.lean"]
        sc = searchConfigFor ra (cascadeFor ra ["m1", "m2"])
        m = goalPolicies ra pol goals
    learnApplied sc (maybe [] acTactics (autoConfig ra)) m
      `shouldBe` A.object
        [ ( "s"
          , A.object
              [ ("backends", A.toJSON (["fisher", "claude-code"] :: [Text])), ("tiers", A.toJSON (["m2", "m1"] :: [Text]))
              , ("tactics", A.toJSON (["omega"] :: [Text])), ("attempt_secs", A.Number 300) ] )
        , ("o", A.object [("tactics", A.toJSON (["omega"] :: [Text]))]) ]
    let flagged = ok ["F.lean", "--auto-tactics", "simp,omega"]
    learnApplied (searchConfigFor flagged (cascadeFor flagged [])) ["simp", "omega"] (goalPolicies flagged pol goals)
      `shouldBe` A.object [("s", A.object [("backends", A.toJSON (["fisher", "claude-code"] :: [Text])), ("attempt_secs", A.Number 300)])]
  it "learnOverridden names the knobs whose flags were given" $ do
    learnOverridden (ok ["F.lean"]) `shouldBe` []
    learnOverridden (ok ["F.lean", "--backends", "fisher", "--tiers", "a", "--auto-tactics", "simp", "--attempt-minutes", "3"])
      `shouldBe` ["backends", "tiers", "tactics", "attempt_secs"]
  it "the usage lists --no-learn" $
    runUsageText `shouldSatisfy` isInfixOf "--no-learn"

  let root home = home </> "proj"
      -- 10 failed claude-code attempts and 10 closed fisher ones on shS
      seed home rl = do
        let nr i k p b = NewRun
              { nrId = i, nrKind = k, nrParent = p, nrProject = Just (root home), nrProver = Just "lean", nrBackend = b
              , nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = [], nrStartedAt = 0, nrRawDir = Nothing, nrCapture = Full }
        R.startRun rl (nr "search-old" SearchRun Nothing Nothing)
        mapM_ (\(b, o, i) -> do
          let aid = "a-" <> b <> "-" <> T.pack (show i)
          R.startRun rl (nr aid SearchAttempt (Just "search-old") (Just b))
          R.event rl aid 0 "attempt" (A.object ["goal" A..= ("s" :: Text), "shape" A..= shS, "backend" A..= b])
          R.endRun rl aid (RunEnd 60 Nothing o (Just 1) Nothing Nothing Nothing))
          ([("claude-code", "failed", i) | i <- [1 .. 10 :: Int]] <> [("fisher", "closed", i) | i <- [1 .. 10]])
      parentRun home i = NewRun
        { nrId = i, nrKind = SearchRun, nrParent = Nothing, nrProject = Just (root home), nrProver = Just "lean"
        , nrBackend = Nothing, nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = [], nrStartedAt = 0
        , nrRawDir = Nothing, nrCapture = Full }
      -- learnPolicy on the search log of a new search on the home
      learnAt ra home i = do
        sl <- openSearchLog home (\n -> i <> "-" <> T.pack (show n)) (pure 0) (parentRun home i)
        r <- learnPolicy ra (A.object []) home (root home) sl goals
        closeSearchLog sl
        pure r
      field k v = case v of
        A.Object o -> KM.lookup (K.fromText k) o
        _ -> Nothing

  it "learnPolicy computes the policy from the log, writes it, applies it and reports it" $
    withSystemTempDirectory "fisher-learn" $ \home -> do
      withRunlog (runlogPath home) (seed home)
      (m, v) <- learnAt (ok ["F.lean"]) home "search-1"
      gpBackends <$> Map.lookup "s" m `shouldBe` Just ["fisher", "claude-code"]
      field "enabled" v `shouldBe` Just (A.Bool True)
      field "policy_file" v `shouldBe` Just (A.toJSON (policyPath home (R.projectKeyOf (root home))))
      field "overridden" v `shouldBe` Just (A.toJSON ([] :: [Text]))
      field "exploration" v `shouldBe` Just (A.toJSON ([] :: [Text]))
      field "changes" v `shouldSatisfy` maybe False (/= A.Number 0)
      -- what each goal runs with where the policy moved it: s's backends
      -- and its budget (p80 of its 60 s closes, floored at half of 1200)
      (field "applied" v >>= field "s") `shouldBe` Just (A.object [("backends", A.toJSON (["fisher", "claude-code"] :: [Text])), ("attempt_secs", A.Number 600)])
      readPolicy (policyPath home (R.projectKeyOf (root home))) >>= (`shouldSatisfy` either (const False) isJust)
      -- the same log again: the same policy, no change recorded
      (_, v2) <- learnAt (ok ["F.lean"]) home "search-2"
      field "changes" v2 `shouldBe` Just (A.Number 0)

  it "learnPolicy with --no-learn, or no run log, applies no policy and says why" $
    withSystemTempDirectory "fisher-learn-off" $ \home -> do
      withRunlog (runlogPath home) (seed home)
      (m, v) <- learnAt (ok ["F.lean", "--no-learn"]) home "search-1"
      m `shouldBe` Map.empty
      field "enabled" v `shouldBe` Just (A.Bool False)
      field "reason" v `shouldSatisfy` isJust
      doesFileExist (policyPath home (R.projectKeyOf (root home))) `shouldReturn` False
      let bad = home </> "not-a-home"
      BS.writeFile bad "a file, not a directory"
      (m2, v2) <- learnAt (ok ["F.lean"]) bad "search-2"
      m2 `shouldBe` Map.empty
      field "enabled" v2 `shouldBe` Just (A.Bool False)

  it "learnPolicy ignores a previous file it cannot read, applies the defaults and writes the fresh policy" $
    withSystemTempDirectory "fisher-learn-bad" $ \home -> do
      withRunlog (runlogPath home) (seed home)
      let path = policyPath home (R.projectKeyOf (root home))
      createDirectoryIfMissing True (takeDirectory path)
      BS.writeFile path "not json"
      (m, v) <- learnAt (ok ["F.lean"]) home "search-1"
      m `shouldBe` Map.empty
      field "enabled" v `shouldBe` Just (A.Bool False)
      field "reason" v `shouldSatisfy` maybe False (\r -> case r of A.String t -> "ignoring" `T.isInfixOf` t; _ -> False)
      readPolicy path >>= (`shouldSatisfy` either (const False) isJust)

  it "the saved policy is the unflagged one: a --backends run between two plain runs records no change" $
    withSystemTempDirectory "fisher-learn-flags" $ \home -> do
      withRunlog (runlogPath home) (seed home)
      let path = policyPath home (R.projectKeyOf (root home))
          savedBackends = do
            p <- readPolicy path
            pure (either (const Nothing) (fmap (fmap gpBackends . Map.lookup shS . pShapes)) p)
      (_, v1) <- learnAt (ok ["F.lean"]) home "search-1"
      field "changes" v1 `shouldSatisfy` maybe False (/= A.Number 0)
      savedBackends `shouldReturn` Just (Just ["fisher", "claude-code"])
      (m2, v2) <- learnAt (ok ["F.lean", "--backends", "fisher"]) home "search-2"
      field "changes" v2 `shouldBe` Just (A.Number 0)
      gpBackends <$> Map.lookup "s" m2 `shouldBe` Just ["fisher"]
      savedBackends `shouldReturn` Just (Just ["fisher", "claude-code"])
      (_, v3) <- learnAt (ok ["F.lean"]) home "search-3"
      field "changes" v3 `shouldBe` Just (A.Number 0)
      -- nor do the other flags
      (_, v4) <- learnAt (ok ["F.lean", "--tiers", "a,b", "--auto-tactics", "simp", "--attempt-minutes", "1", "--no-auto"]) home "search-4"
      field "changes" v4 `shouldBe` Just (A.Number 0)

  it "learnPolicy that cannot write the policy applies none and says why" $
    withSystemTempDirectory "fisher-learn-nowrite" $ \home -> do
      withRunlog (runlogPath home) (seed home)
      BS.writeFile (home </> "fisher" </> "policy") "a file, not a directory"
      (m, v) <- learnAt (ok ["F.lean"]) home "search-1"
      m `shouldBe` Map.empty
      field "enabled" v `shouldBe` Just (A.Bool False)

  it "the portfolio: the pre-pass's tactics, none with --no-auto or --single" $ do
    portfolioTactics (ok ["F.lean", "--auto-tactics", "simp,omega"]) `shouldBe` ["simp", "omega"]
    portfolioTactics (ok ["F.lean", "--no-auto"]) `shouldBe` []
    portfolioTactics (ok ["F.lean", "--single", "fisher"]) `shouldBe` []

  it "the portfolio event goes on the search's run before its auto events" $
    withSystemTempDirectory "fisher-portfolio" $ \home -> do
      sl <- openSearchLog home (\i -> "search-p-" <> T.pack (show i)) (pure 0) (parentRun home "search-p")
      searchLogPortfolio sl ["simp", "omega"]
      searchLogSearchEvent sl (AutoMissed 1 "s" 2)
      searchLogSearchEvent sl (AutoClosed 2 "o" "simp" 1)
      closeSearchLog sl
      evs <- withRunlog (runlogPath home) (`runEvents` "search-p")
      [k | (_, _, k, _) <- evs] `shouldBe` ["portfolio", "auto missed", "auto closed"]
      [d | (_, _, "portfolio", d) <- evs] `shouldBe` [A.object ["tactics" A..= (["simp", "omega"] :: [Text])]]

-- | An active lesson of a project key.
activeLesson :: Text -> Text -> Text -> R.LessonRow
activeLesson pk key txt =
  R.LessonRow
    { R.lrId = 0, R.lrProjectKey = pk, R.lrKind = "unknown_identifier", R.lrKey = key, R.lrText = txt, R.lrStatus = R.Active
    , R.lrEvidenceRuns = ["r1", "r2", "r3"], R.lrAgreements = 0, R.lrFirstSeen = 1, R.lrActivatedAt = Just 2, R.lrRetiredAt = Nothing
    , R.lrRunsActive = [], R.lrHitsWhileActive = 0, R.lrAttemptsWhileActive = 0, R.lrHitsBefore = 0
    , R.lrAttemptsBefore = 0, R.lrRunsBefore = 0, R.lrBeforeWindow = [], R.lrAgreedAttempts = [] }

fieldOf :: Text -> A.Value -> Maybe A.Value
fieldOf k = \case
  A.Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

lessonsSpec :: Spec
lessonsSpec = describe "the lessons" $ do
  let cfg kvs = A.object ["fisher" A..= A.object ["lessons" A..= A.object kvs]]
  it "--no-lessons turns them off; on by default" $ do
    raLessons (ok ["--no-lessons", "F.lean"]) `shouldBe` False
    raLessons (ok ["F.lean"]) `shouldBe` True
  it "lessonsEnabled: off with --no-lessons or fisher.lessons.enabled false" $ do
    lessonsEnabled (ok ["F.lean"]) (cfg ["enabled" A..= False]) `shouldBe` False
    lessonsEnabled (ok ["F.lean"]) (cfg ["enabled" A..= True]) `shouldBe` True
    lessonsEnabled (ok ["F.lean"]) (A.object []) `shouldBe` True
    lessonsEnabled (ok ["F.lean", "--no-lessons"]) (A.object []) `shouldBe` False
    lessonsConfigOn (cfg ["enabled" A..= False]) `shouldBe` False
  it "the judge is on by default, off with fisher.lessons.judge false" $ do
    lessonsJudgeOn (A.object []) `shouldBe` True
    lessonsJudgeOn (cfg ["judge" A..= False]) `shouldBe` False
  it "the caps: 0.25 a run and 2.00 a day by default, else the config's" $ do
    judgeCaps (A.object []) `shouldBe` (0.25, 2.0)
    judgeCaps (cfg ["judge_cap_usd" A..= (0.5 :: Double), "judge_day_cap_usd" A..= (0 :: Double)]) `shouldBe` (0.5, 0)
    judgeCaps (cfg ["judge_cap_usd" A..= ("x" :: Text)]) `shouldBe` (0.25, 2.0)
  it "the call cap: 20 judge calls a run by default, else fisher.lessons.judge_calls" $ do
    judgeCalls (A.object []) `shouldBe` 20
    judgeCalls (cfg ["judge_calls" A..= (3 :: Int)]) `shouldBe` 3
    judgeCalls (cfg ["judge_calls" A..= (0 :: Int)]) `shouldBe` 0
    judgeCalls (cfg ["judge_calls" A..= ("x" :: Text)]) `shouldBe` 20
  it "the env of a pass carries the call cap, a 90 s call timeout and a 300 s judge phase" $
    withSystemTempDirectory "fisher-lessons" $ \home -> withRunlog (runlogPath home) $ \rl -> do
      env <- mineEnvFor (const (pure ())) (cfg ["judge_calls" A..= (7 :: Int)]) home (home </> "proj") Nothing rl
      (meCallCap env, meJudgeTimeout env, meJudgeBudget env) `shouldBe` (7, 90, 300)
  it "fisher.lessons.max sets the cap on a file's lines" $ do
    lMax (lessonsLimits (A.object [])) `shouldBe` 12
    lMax (lessonsLimits (cfg ["max" A..= (5 :: Int)])) `shouldBe` 5
  it "fisher.lessons.auto_judged: true promotes judged and self lessons on their counts (off by default)" $ do
    lAutoJudged (lessonsLimits (A.object [])) `shouldBe` False
    lAutoJudged (lessonsLimits (cfg ["auto_judged" A..= True])) `shouldBe` True
    lAutoJudged (lessonsLimits (cfg ["auto_judged" A..= ("yes" :: Text)])) `shouldBe` False
    lessonsLimits (cfg ["auto_judged" A..= True, "max" A..= (5 :: Int)]) `shouldSatisfy` \l -> lAutoJudged l && lMax l == 5
  it "off, the attempts are told FISHER_LESSONS=0" $ do
    Map.lookup "FISHER_LESSONS" (lessonsEnv False (Map.fromList [("A", "b")])) `shouldBe` Just "0"
    lessonsEnv True (Map.fromList [("A", "b")]) `shouldBe` Map.fromList [("A", "b")]
  it "the usage lists --no-lessons" $
    runUsageText `shouldSatisfy` isInfixOf "--no-lessons"
  it "an attempt's id is the search run's id, a dash and its number (what the miner reads back)" $
    attemptTag "/x/runs/20261025-101500" "0a1b2c3d" 7 `shouldBe` searchRunId "/x/runs/20261025-101500" "0a1b2c3d" <> "-7"
  it "the learn block gains the lessons object" $ do
    withLessons (A.object ["enabled" A..= True]) (A.object ["mined" A..= (1 :: Int)])
      `shouldBe` A.object ["enabled" A..= True, "lessons" A..= A.object ["mined" A..= (1 :: Int)]]
    withLessons (A.String "odd") (A.object []) `shouldBe` A.object ["lessons" A..= A.object []]
  it "after a search the pass mines its attempts (rules only with the judge off); off it says so" $
    withSystemTempDirectory "fisher-lessons" $ \home -> do
      let parent = "search-20261025-120000-ab12cd34"
          tagL i = parent <> "-" <> T.pack (show (i :: Int))
          root = home </> "proj"
          newL i k p = NewRun
            { nrId = i, nrKind = k, nrParent = p, nrProject = Just root, nrProver = Just "lean"
            , nrBackend = Just "fisher", nrModel = Nothing, nrTier = Nothing, nrWorkflow = Just "prove"
            , nrArgv = [], nrStartedAt = 100, nrRawDir = Nothing, nrCapture = Full }
          ra = ok ["F.lean"]
          judgeOff = cfg ["judge" A..= False]
      createDirectoryIfMissing True root
      sl <- openSearchLog home tagL (pure 100) (newL parent SearchRun Nothing)
      _ <- searchLogBegin sl 1 (newL (tagL 1) SearchAttempt (Just parent)) (attemptEventData "t" "theorem t : 1 = 1 := by" "fisher" Nothing)
      searchLogged sl $ \rl -> do
        R.event rl (tagL 1) 1 "tool_start" (A.object ["name" A..= ("lean_build" :: Text), "args" A..= A.Null, "call_id" A..= ("c1" :: Text)])
        R.event rl (tagL 1) 2 "tool_end" (A.object ["name" A..= ("lean_build" :: Text), "call_id" A..= ("c1" :: Text), "ok" A..= True, "result" A..= ("unknown identifier 'Nat.foo_bar'" :: Text)])
        R.endRun rl (tagL 1) (RunEnd 110 Nothing "failed" Nothing Nothing Nothing Nothing)
        ev <- lessonsEventData rl (A.object []) home root (Just (home </> "assets"))
        ev `shouldBe` A.object ["ids" A..= ([] :: [Int]), "block_hash" A..= A.Null, "project_hash" A..= A.Null, "global_hash" A..= A.Null, "skill_revision" A..= A.Null]
      v <- mineSearch ra (cascadeFor ra []) judgeOff home root sl True
      v `shouldBe` A.object ["mined" A..= (1 :: Int), "judged" A..= (0 :: Int), "unjudged" A..= (0 :: Int), "promoted" A..= ([] :: [Int]), "retired" A..= ([] :: [Int])]
      off <- mineSearch (ok ["F.lean", "--no-lessons"]) (cascadeFor ra []) judgeOff home root sl False
      off `shouldBe` A.object ["enabled" A..= False, "reason" A..= ("--no-lessons" :: Text)]
      closeSearchLog sl
  it "the lessons event names the shown lessons the prompt block carries, and hashes that block" $
    withSystemTempDirectory "fisher-lessons" $ \home -> do
      let root = home </> "proj"
      createDirectoryIfMissing True root
      withRunlog (runlogPath home) $ \rl -> do
        a <- R.upsertLesson rl (activeLesson (R.projectKeyOf root) "k:a" "Lesson a.")
        b <- R.upsertLesson rl (activeLesson (R.projectKeyOf root) "k:b" "Lesson b.")
        -- the file lags behind runs.db and holds a line written by hand: neither changes the block
        Right () <- writeLessonsFile (projectLessonsPath root) ["Lesson a.", "Lesson planted."]
        ev <- lessonsEventData rl (A.object []) home root Nothing
        fieldOf "ids" ev `shouldBe` Just (A.toJSON [a, b])
        pl <- promptLessons rl False home root
        fieldOf "block_hash" ev `shouldBe` Just (A.toJSON (sha256Hex (TE.encodeUtf8 (partText (plBlock pl)))))
        partText (plBlock pl) `shouldSatisfy` T.isInfixOf "2. Lesson b."
        partText (plBlock pl) `shouldNotSatisfy` T.isInfixOf "planted"
        ph <- fileHash (projectLessonsPath root)
        fieldOf "project_hash" ev `shouldBe` Just (A.toJSON ph)
  it "the lessons event of a search is the block its attempts carry, even when runs.db changes in between" $
    withSystemTempDirectory "fisher-lessons" $ \home -> do
      let root = home </> "proj"
      createDirectoryIfMissing True root
      withRunlog (runlogPath home) $ \rl -> do
        a <- R.upsertLesson rl (activeLesson (R.projectKeyOf root) "k:a" "Lesson a.")
        pl <- promptLessons rl False home root
        -- a mining pass promotes another lesson after the block was built
        _ <- R.upsertLesson rl (activeLesson (R.projectKeyOf root) "k:late" "Lesson late.")
        ev <- lessonsEventFrom pl home root Nothing
        fieldOf "ids" ev `shouldBe` Just (A.toJSON (plIds pl))
        plIds pl `shouldBe` [a]
        fieldOf "block_hash" ev `shouldBe` Just (A.toJSON (sha256Hex (TE.encodeUtf8 (partText (plBlock pl)))))
  it "an imported launch run gets a lessons event (the ids its files held) before the rules pass" $
    withSystemTempDirectory "fisher-lessons" $ \home -> do
      let root = home </> "proj"
      createDirectoryIfMissing True root
      a <- withRunlog (runlogPath home) $ \rl -> do
        R.startRun rl NewRun
          { nrId = "swarm-t1", nrKind = LaunchRun, nrParent = Nothing, nrProject = Just root, nrProver = Just "lean"
          , nrBackend = Just "claude-code", nrModel = Nothing, nrTier = Nothing, nrWorkflow = Just "prove"
          , nrArgv = [], nrStartedAt = 100, nrRawDir = Nothing, nrCapture = Imported }
        R.endRun rl "swarm-t1" (RunEnd 110 Nothing "failed" Nothing Nothing Nothing Nothing)
        R.upsertLesson rl (activeLesson (R.projectKeyOf root) "k:a" "Lesson a.")
      Right () <- writeLessonsFile (projectLessonsPath root) ["Lesson a."]
      mineImported (const (pure ())) home (A.object []) root "swarm-t1"
      withRunlog (runlogPath home) $ \rl -> do
        es <- runEvents rl "swarm-t1"
        [fieldOf "ids" d | (_, _, "lessons", d) <- es] `shouldBe` [Just (A.toJSON [a])]
        Just l <- R.getLesson rl a
        R.lrRunsActive l `shouldBe` ["swarm-t1"]
      -- imported again: no second event
      mineImported (const (pure ())) home (A.object []) root "swarm-t1"
      withRunlog (runlogPath home) $ \rl -> do
        es <- runEvents rl "swarm-t1"
        length [() | (_, _, "lessons", _) <- es] `shouldBe` 1
  it "with FISHER_LESSONS=0 an imported launch run is neither noted nor mined" $
    withSystemTempDirectory "fisher-lessons" $ \home -> do
      let root = home </> "proj"
      createDirectoryIfMissing True root
      withRunlog (runlogPath home) $ \rl ->
        R.startRun rl NewRun
          { nrId = "swarm-t2", nrKind = LaunchRun, nrParent = Nothing, nrProject = Just root, nrProver = Just "lean"
          , nrBackend = Just "claude-code", nrModel = Nothing, nrTier = Nothing, nrWorkflow = Just "prove"
          , nrArgv = [], nrStartedAt = 100, nrRawDir = Nothing, nrCapture = Imported }
      setEnv "FISHER_LESSONS" "0"
      mineImported (const (pure ())) home (A.object []) root "swarm-t2" `finally` unsetEnv "FISHER_LESSONS"
      withRunlog (runlogPath home) $ \rl -> do
        es <- runEvents rl "swarm-t2"
        [k | (_, _, k, _) <- es] `shouldBe` []
  it "the finished search's outputs come before the pass, the report after it" $ do
    steps <- newIORef []
    let note x = modifyIORef' steps (<> [x])
    reportAfterLessons (note "outputs") (note "mine" >> pure (A.object ["mined" A..= (0 :: Int)])) (\v -> note ("report " <> show v))
    readIORef steps >>= (`shouldBe` ["outputs", "mine", "report " <> show (A.object ["mined" A..= (0 :: Int)])])
  it "an interrupt during the pass still writes the report (lessons interrupted) and goes on up" $ do
    steps <- newIORef []
    let note x = modifyIORef' steps (<> [x])
    reportAfterLessons (note "outputs") (note "mine" >> throwIO UserInterrupt) (\v -> note ("report " <> show v))
      `shouldThrow` (== UserInterrupt)
    readIORef steps >>= (`shouldBe` ["outputs", "mine", "report " <> show (A.object ["interrupted" A..= True])])
  it "any other failure of the pass is the report's error, and the search goes on" $ do
    out <- newIORef A.Null
    reportAfterLessons (pure ()) (ioError (userError "boom")) (\v -> modifyIORef' out (const v))
    v <- readIORef out
    v `shouldSatisfy` \case
      A.Object o -> KM.lookup "enabled" o == Just (A.Bool True) && maybe False (\case A.String e -> "boom" `T.isInfixOf` e; _ -> False) (KM.lookup "error" o)
      _ -> False
  it "on, a pass that cannot run says why with enabled true" $
    withSystemTempDirectory "fisher-lessons" $ \home -> do
      let ra = ok ["F.lean"]
          judgeOff = cfg ["judge" A..= False]
          parentRow = NewRun
            { nrId = "search-x", nrKind = SearchRun, nrParent = Nothing, nrProject = Just home, nrProver = Nothing
            , nrBackend = Nothing, nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing
            , nrArgv = [], nrStartedAt = 1, nrRawDir = Nothing, nrCapture = Full }
      -- a closed log: the pass itself fails
      sl <- openSearchLog home (\n -> "search-x-" <> T.pack (show n)) (pure 0) parentRow
      closeSearchLog sl
      v <- mineSearch ra (cascadeFor ra []) judgeOff home home sl True
      v `shouldSatisfy` \case
        A.Object o -> KM.lookup "enabled" o == Just (A.Bool True) && isJust (KM.lookup "error" o)
        _ -> False
      -- a log that could not be opened
      writeFile (home </> "notadir") ""
      sl2 <- openSearchLog (home </> "notadir") (\n -> "search-y-" <> T.pack (show n)) (pure 0) parentRow
      v2 <- mineSearch ra (cascadeFor ra []) judgeOff home home sl2 True
      v2 `shouldBe` A.object ["enabled" A..= True, "error" A..= ("the run log is not open" :: Text)]

-- | A Lake project with a manifest that lists lean-smt or not.
smtProject :: FilePath -> Bool -> IO FilePath
smtProject d withSmt = do
  let root = d </> "proj"
      pkg name = "{\"type\": \"git\", \"scope\": \"s\", \"rev\": \"abc\", \"name\": \"" <> name <> "\", \"inherited\": false}"
      pkgs = (if withSmt then [pkg "smt"] else []) <> [pkg "mathlib"]
  createDirectoryIfMissing True root
  BS.writeFile (root </> "lakefile.lean") "import Lake\nopen Lake DSL\npackage p\n"
  BS.writeFile (root </> "lake-manifest.json")
    ("{\"version\": \"1.1.0\", \"packagesDir\": \".lake/packages\", \"packages\": [" <> BS.intercalate ", " pkgs <> "], \"name\": \"p\"}")
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 smtSource)
  pure root

smtSource :: Text
smtSource = "import Mathlib\n\ntheorem t : 1 + 1 = 2 := by\n  sorry\n"

-- | The paragraph the brief carries in a project with lean-smt.
smtParagraph :: Text
smtParagraph =
  "`smt` (lean-smt, cvc5) is available: `import Smt`, `by smt`, `by smt [h\8321, h\8322]` to pass hypotheses, `smt +mono` for polymorphic goals; it covers linear integer and real arithmetic, bitvectors, uninterpreted functions and quantifier-free logic over them."

smtSpec :: Spec
smtSpec = describe "the smt arm" $ do
  let parentRun home i = NewRun
        { nrId = i, nrKind = SearchRun, nrParent = Nothing, nrProject = Just (home </> "proj"), nrProver = Just "lean"
        , nrBackend = Nothing, nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = [], nrStartedAt = 0
        , nrRawDir = Nothing, nrCapture = Full }
      -- the portfolio event a search with these arguments logs
      portfolioLogged home ra = do
        sl <- openSearchLog home (\i -> "search-s-" <> T.pack (show i)) (pure 0) (parentRun home "search-s")
        searchLogPortfolio sl (portfolioTactics ra)
        closeSearchLog sl
        evs <- withRunlog (runlogPath home) (`runEvents` "search-s")
        pure [d | (_, _, "portfolio", d) <- evs]
      tacticsJson ts = [A.object ["tactics" A..= (ts :: [Text])]]
      noSmt = filter (/= "smt") defaultTactics

  it "a project without lean-smt logs a portfolio without smt" $
    withSystemTempDirectory "fisher-smt" $ \d -> do
      root <- smtProject d False
      smtAvailable (A.object []) root `shouldReturn` False
      ss <- smtSetup (A.object []) root (ok ["F.lean"]) smtSource
      ssAvailable ss `shouldBe` False
      ssInserted ss `shouldBe` False
      ssBase ss `shouldBe` smtSource
      ssWarning ss `shouldBe` Nothing
      fmap acTactics (autoConfig (ssArgs ss)) `shouldBe` Just noSmt
      portfolioLogged d (ssArgs ss) `shouldReturn` tacticsJson noSmt

  it "a module file logs a portfolio without smt, with one warning" $
    withSystemTempDirectory "fisher-smt" $ \d -> do
      root <- smtProject d True
      let src = "module\n\npublic import Mathlib\n\ntheorem t : 1 + 1 = 2 := by\n  sorry\n"
      ss <- smtSetupWith (\_ -> pure (Just (Z3 "/opt/z3"))) (A.object []) root (ok ["F.lean"]) src
      (ssAvailable ss, ssInserted ss, ssBase ss) `shouldBe` (False, False, src)
      ssWarning ss `shouldBe` Just "smt is not available for a module file: the arm is off for this run"
      (partText (ssFacts ss), isNothing (ssSmt ss), ssSolverWarning ss) `shouldBe` ("", True, Nothing)
      fmap acTactics (autoConfig (ssArgs ss)) `shouldBe` Just noSmt
      portfolioLogged d (ssArgs ss) `shouldReturn` tacticsJson noSmt

  it "a project with lean-smt gets import Smt in the base and in the portfolio" $
    withSystemTempDirectory "fisher-smt" $ \d -> do
      root <- smtProject d True
      smtAvailable (A.object []) root `shouldReturn` True
      ss <- smtSetup (A.object []) root (ok ["F.lean"]) smtSource
      ssAvailable ss `shouldBe` True
      ssInserted ss `shouldBe` True
      ssBase ss `shouldBe` insertImportSmt smtSource
      T.lines (ssBase ss) `shouldBe` ["import Mathlib", importSmtLine, "", "theorem t : 1 + 1 = 2 := by", "  sorry"]
      fmap acTactics (autoConfig (ssArgs ss)) `shouldBe` Just defaultTactics
      portfolioLogged d (ssArgs ss) `shouldReturn` tacticsJson defaultTactics
      -- a file that imports Smt itself is left as it is
      own <- smtSetup (A.object []) root (ok ["F.lean"]) ("import Smt\n" <> smtSource)
      (ssInserted own, ssBase own) `shouldBe` (False, "import Smt\n" <> smtSource)
      -- fisher.smt.enabled false turns the arm and the marker off
      let off = A.object ["fisher" A..= A.object ["smt" A..= A.object ["enabled" A..= False]]]
      smtAvailable off root `shouldReturn` False
      ssOff <- smtSetup off root (ok ["F.lean"]) smtSource
      (ssAvailable ssOff, ssBase ssOff, partText (ssFacts ssOff)) `shouldBe` (False, smtSource, "")
      fmap acTactics (autoConfig (ssArgs ssOff)) `shouldBe` Just noSmt

  it "a user --auto-tactics list loses smt with one warning where it is not available" $
    withSystemTempDirectory "fisher-smt" $ \d -> do
      without <- smtProject (d </> "a") False
      with <- smtProject (d </> "b") True
      ss <- smtSetup (A.object []) without (ok ["F.lean", "--auto-tactics", "smt,omega"]) smtSource
      ssWarning ss `shouldBe` Just "smt is not available here: the project has no lean-smt (fisher project add smt); dropped from --auto-tactics"
      raAutoTactics (ssArgs ss) `shouldBe` Just ["omega"]
      portfolioTactics (ssArgs ss) `shouldBe` ["omega"]
      quiet <- smtSetup (A.object []) without (ok ["F.lean", "--auto-tactics", "simp,omega"]) smtSource
      ssWarning quiet `shouldBe` Nothing
      kept <- smtSetup (A.object []) with (ok ["F.lean", "--auto-tactics", "smt,omega"]) smtSource
      (ssWarning kept, portfolioTactics (ssArgs kept)) `shouldBe` (Nothing, ["smt", "omega"])

  it "a user --auto-tactics list of smt alone turns the automation off where smt is not available" $
    withSystemTempDirectory "fisher-smt" $ \d -> do
      without <- smtProject d False
      ss <- smtSetup (A.object []) without (ok ["F.lean", "--auto-tactics", "smt"]) smtSource
      ssWarning ss `shouldBe` Just "smt is not available here: the project has no lean-smt (fisher project add smt); dropped from --auto-tactics, which leaves no tactic: the automation is off (as with --no-auto)"
      raAuto (ssArgs ss) `shouldBe` False
      autoConfig (ssArgs ss) `shouldBe` Nothing
      serverWanted (ssArgs ss) `shouldBe` False

  it "the SMT countermodels: an env with a solver and lean-smt, one warning without a solver" $
    withSystemTempDirectory "fisher-smt" $ \d -> do
      with <- smtProject (d </> "a") True
      without <- smtProject (d </> "b") False
      let z3 _ = pure (Just (Z3 "/opt/z3"))
          none _ = pure Nothing
          cfgT = A.object ["fisher" A..= A.object ["smt" A..= A.object ["timeout_secs" A..= (7 :: Int), "query_tactic" A..= ("trace" :: Text)]]]
      ss <- smtSetupWith z3 cfgT with (ok ["F.lean"]) smtSource
      fmap (\e -> (smSolver e, smTimeout e, smQueryTactic e)) (ssSmt ss) `shouldBe` Just (Z3 "/opt/z3", 7, "trace")
      ssSolverWarning ss `shouldBe` Nothing
      partText (ssFacts ss) `shouldBe` smtParagraph <> " `z3` is on PATH for SMT-LIB queries."
      cvc <- smtSetupWith (\_ -> pure (Just (Cvc5 "/opt/cvc5"))) (A.object []) with (ok ["F.lean"]) smtSource
      partText (ssFacts cvc) `shouldBe` smtParagraph <> " `cvc5` is on PATH for SMT-LIB queries."
      -- no solver: no env, one warning
      bare <- smtSetupWith none (A.object []) with (ok ["F.lean"]) smtSource
      isNothing (ssSmt bare) `shouldBe` True
      ssSolverWarning bare `shouldBe` Just "smt: no solver on PATH for fisher.smt.solver auto (z3 or cvc5); the SMT countermodels are off for this run"
      -- no warning when Disprove would not run
      quiet <- smtSetupWith none (A.object []) with (ok ["F.lean", "--no-disprove"]) smtSource
      ssSolverWarning quiet `shouldBe` Nothing
      -- no lean-smt, or the config off: no env, and no solver looked up
      noLean <- smtSetupWith (\_ -> fail "looked up") (A.object []) without (ok ["F.lean"]) smtSource
      (isNothing (ssSmt noLean), ssSolverWarning noLean) `shouldBe` (True, Nothing)
      let off = A.object ["fisher" A..= A.object ["smt" A..= A.object ["enabled" A..= False]]]
      offS <- smtSetupWith (\_ -> fail "looked up") off with (ok ["F.lean"]) smtSource
      (isNothing (ssSmt offS), ssSolverWarning offS) `shouldBe` (True, Nothing)

  it "smtFacts: the paragraph, the solver found named, nothing without lean-smt" $ do
    partText (smtFacts True Nothing) `shouldBe` smtParagraph
    partText (smtFacts True (Just "z3")) `shouldBe` smtParagraph <> " `z3` is on PATH for SMT-LIB queries."
    partText (smtFacts True (Just "cvc5")) `shouldBe` smtParagraph <> " `cvc5` is on PATH for SMT-LIB queries."
    partText (smtFacts False (Just "z3")) `shouldBe` ""
    partText (smtFacts False Nothing) `shouldBe` ""

  it "the brief carries the smt facts only with lean-smt" $
    withSystemTempDirectory "fisher-smt" $ \d -> do
      let briefIn withSmt = do
            root <- smtProject (d </> show withSmt) withSmt
            ss <- smtSetup (A.object []) root (ok ["F.lean"]) smtSource
            (mre, _) <- buildRetrievalAt True (d </> ".fisher") (ok ["F.lean", "--no-library"]) root ""
            re <- maybe (fail "no retrieval") pure mre
            retrieveFor <- newRetriever re {reExtra = ssFacts ss} "F.lean"
            goal <- case openGoals (ssBase ss) of
              g : _ -> pure g
              [] -> fail "no open goal"
            partText . rtBrief <$> retrieveFor (Query (dName goal) (dName goal) (dStart goal + 1) (dStatement goal) Nothing [dStatement goal])
      with <- briefIn True
      smtParagraph `T.isInfixOf` with `shouldBe` True
      without <- briefIn False
      "lean-smt" `T.isInfixOf` without `shouldBe` False

-- | A Lean stand-in (fisher-search's test one): a proof containing BAD
-- does not compile, an open goal warns, a proof naming an open goal
-- depends on sorryAx, every name's type is its statement.
fakeLean :: LeanRun
fakeLean = LeanRun $ \root rel -> do
  t <- TE.decodeUtf8 <$> BS.readFile (root </> rel)
  let body = fst (T.breakOn "\n\n#print axioms" t)
      ds = L.declarations body
      opens = Set.fromList (map dName (filter L.isOpen ds))
      queried = [T.strip (T.drop (T.length "#print axioms ") l) | l <- T.lines t, "#print axioms " `T.isPrefixOf` l]
      errs = [rel <> ":" <> show (dStart d + 1) <> ":2: error: BAD" | d <- ds, "BAD" `T.isInfixOf` dProof d]
      warns = [rel <> ":" <> show (dStart d + 1) <> ":8: warning: declaration uses `sorry`" | d <- ds, L.isOpen d]
      uses dirty d = any (\o -> o /= dName d && o `Set.member` dirty) (T.words (dProof d))
      grow dirty =
        let next = Set.union dirty (Set.fromList [dName d | d <- ds, uses dirty d])
        in if next == dirty then dirty else grow next
      sorryDeps = grow opens
      info n = case L.findDecl n ds of
        Nothing -> []
        Just d ->
          [ "'" <> n <> "' depends on axioms: [" <> (if dName d `Set.member` sorryDeps then "sorryAx" else "propext") <> "]"
          , n <> " : " <> L.statementHead d ]
  pure (if null errs then 0 else 1, T.unlines (map T.pack (errs <> warns) <> concatMap info queried))

-- | @--record@ and @--replay@: the flags and their refusals, the tape's
-- reader, and a flat search recorded on a real orchestrator over a fake
-- attempt runner (no live children: 'runMain' needs them) and replayed
-- through 'replayMainWith', which prints the same lines and writes nothing.
recordSpec :: Spec
recordSpec = describe "--record and --replay" $ do
  let source = "theorem a : A := by\n  sorry\n\ntheorem b : B := by\n  sorry\n" :: Text
      -- the attempt's start with this proof under its goal
      splice proof sp =
        let ls = T.splitOn "\n" (asStart sp)
            d = maybe (error "no goal") id (L.findDecl (asGoal sp) (L.declarations (asStart sp)))
         in T.intercalate "\n" (take (dStmtEnd d + 1) ls <> ["  " <> proof] <> drop (dEnd d) ls)
      -- b fails on claude-code; everything else closes
      fakeRun sp = do
        threadDelay 20000
        let proof = if asGoal sp == "b" && asBackend sp == "claude-code" then "exact BAD" else "trivial"
        pure (plainResult (Just (splice proof sp)) Nothing "")
      flags = ["F.lean", "--no-auto", "--no-retrieval", "--no-library", "--no-learn", "--no-lessons"]
      project d = do
        let root = d </> "proj"
        createDirectoryIfMissing True root
        BS.writeFile (root </> "lakefile.lean") "import Lake\nopen Lake DSL\npackage p\n"
        BS.writeFile (root </> "F.lean") (TE.encodeUtf8 source)
        pure root
      -- the flat search recorded to the tape, printed as a run prints it
      -- from its search on: the event lines and the report's lines
      record d tape = do
        let ra = ok (flags <> ["--record", tape])
            ws = d </> "rec"
        createDirectoryIfMissing True ws
        BS.writeFile (ws </> "F.lean") (TE.encodeUtf8 source)
        out <- newIORef []
        clock <- newIORef (0 :: Double)
        let say t = modifyIORef' out (t :)
        o0 <- realOrchestrator fakeRun (atomicModifyIORef' clock (\t -> (t + 1, t + 1))) (say . eventLine) (\_ -> pure Nothing)
        (o, writeTape) <- recordingTo (raRecord ra) o0
        let env = SearchEnv
              { seOrch = o, seLean = fakeLean, sePolicy = AxiomPolicy standardAxioms False, seRoot = ws, seRel = "F.lean"
              , seBaseChanged = \_ -> pure (), seAuto = Nothing, seRetrieval = Nothing }
        r <- swarmSearch ra env (searchConfigFor ra (cascadeFor ra [])) Nothing [] source
        writeTape
        rep <- either (fail . T.unpack) pure r
        mapM_ say (swarmReportLines rep "; recorded")
        reverse <$> readIORef out
      -- the replay of the tape: its exit and printed lines
      replay root tape = do
        out <- newIORef []
        r <- try (replayMainWith (ReplayIO fakeLean (A.object []) (\t -> modifyIORef' out (t :))) (ok (flags <> ["--replay", tape])) (root </> "F.lean") tape)
        ls <- reverse <$> readIORef out
        pure (r :: Either ExitCode (), ls)
      -- with HOME and TMPDIR private, restored afterwards; they are
      -- process-wide, which is safe while the suite runs its examples
      -- sequentially (hspec's default here)
      private d act = do
        let home = d </> "home"
            tmpd = d </> "tmp"
        createDirectoryIfMissing True home
        createDirectoryIfMissing True tmpd
        oldHome <- lookupEnv "HOME"
        oldTmp <- lookupEnv "TMPDIR"
        setEnv "HOME" home
        setEnv "TMPDIR" tmpd
        act home tmpd `finally` (maybe (unsetEnv "HOME") (setEnv "HOME") oldHome >> maybe (unsetEnv "TMPDIR") (setEnv "TMPDIR") oldTmp)

  it "parses --record FILE and --replay FILE" $ do
    raRecord (ok ["F.lean", "--record", "t.json"]) `shouldBe` Just "t.json"
    raReplay (ok ["F.lean", "--replay", "t.json"]) `shouldBe` Just "t.json"
    (raRecord defaultRunArgs, raReplay defaultRunArgs) `shouldBe` (Nothing, Nothing)
    run ["F.lean", "--record"] `shouldBe` Nothing
    run ["F.lean", "--replay"] `shouldBe` Nothing
    mapM_ (\f -> runUsageText `shouldSatisfy` isInfixOf f) ["--record FILE", "--replay FILE"]

  it "--replay refuses --apply, --graph, --out and --record; --record refuses --graph" $ do
    let refused args flag = case run args of
          Just (Left e) -> e `shouldSatisfy` T.isInfixOf flag
          other -> expectationFailure ("not refused: " <> show args <> ": " <> show other)
    refused ["F.lean", "--replay", "t", "--apply"] "--apply"
    refused ["F.lean", "--replay", "t", "--graph"] "--graph"
    refused ["F.lean", "--replay", "t", "--out", "o"] "--out"
    refused ["F.lean", "--replay", "t", "--record", "u"] "--record"
    refused ["F.lean", "--record", "t", "--graph"] "--graph"
    raRecord (ok ["F.lean", "--record", "t", "--apply", "--out", "o"]) `shouldBe` Just "t"

  it "a --record path that cannot be written is refused before the run; a failed final write is a warning" $
    withSystemTempDirectory "fisher-record-path" $ \d -> do
      bad <- checkRecordPath (Just (d </> "missing" </> "tape.json"))
      either id (const "") bad `shouldSatisfy` T.isPrefixOf "--record: cannot write"
      checkRecordPath (Just (d </> "tape.json")) `shouldReturn` Right ()
      doesFileExist (d </> "tape.json") `shouldReturn` True
      checkRecordPath Nothing `shouldReturn` Right ()
      o <- realOrchestrator (\_ -> pure (plainResult Nothing Nothing "")) (pure 0) (\_ -> pure ()) (\_ -> pure Nothing)
      (_, writeTape) <- recordingTo (Just (d </> "missing" </> "tape.json")) o
      writeTape `shouldReturn` ()

  it "replaying: a tape left unconsumed, and a divergence, are errors" $ do
    replaying (\_ -> pure ()) (Recording [Now 1]) (\_ -> pure ())
      `shouldReturn` Left "the replay did not consume the whole recording (0 of 1 steps)"
    r <- replaying (\_ -> pure ()) (Recording [Now 1]) (\o -> oNow o >> oNow o)
    either id (const "") r `shouldSatisfy` T.isPrefixOf "the replay diverged from the recording at step 1: the recording has the end of the tape"
    replaying (\_ -> pure ()) (Recording [Now 1]) oNow `shouldReturn` Right 1

  it "a corrupted tape is a decode error, and the replay exits with code 1" $
    withSystemTempDirectory "fisher-replay-bad" $ \d -> do
      root <- project d
      let tape = d </> "tape.json"
      BS.writeFile tape "{\"steps\": [oops"
      r <- readRecording tape
      either id (const "") r `shouldSatisfy` T.isInfixOf "is not a recording"
      readRecording (d </> "missing.json") >>= (`shouldSatisfy` either (T.isInfixOf "cannot read") (const False))
      (code, ls) <- replay root tape
      (code, ls) `shouldBe` (Left (ExitFailure 1), [])

  it "a recorded search replays to the same printed lines and writes nothing" $
    withSystemTempDirectory "fisher-replay" $ \d -> do
      root <- project d
      let tape = d </> "tape.json"
      recorded <- record d tape
      doesFileExist tape `shouldReturn` True
      rc <- readRecording tape >>= either (fail . T.unpack) pure
      rcSteps rc `shouldSatisfy` (not . null)
      [() | Spawned _ <- rcSteps rc] `shouldSatisfy` (not . null)
      listed <- sort <$> listDirectory root
      private d $ \home tmpd -> do
        (code, ls) <- replay root tape
        code `shouldBe` Right ()
        case ls of
          learn : header : rest -> do
            learn `shouldBe` "learn: off (--no-learn)"
            header `shouldBe` T.pack ("search F.lean in " <> root <> ": 2 open goals; replay of " <> tape)
            -- the events and the report as recorded; only where the
            -- outputs went differs
            init rest `shouldBe` init recorded
            T.stripSuffix ("; replay of " <> T.pack tape) (last rest) `shouldBe` T.stripSuffix "; recorded" (last recorded)
          _ -> expectationFailure ("too few lines: " <> show ls)
        any ("closed" `T.isInfixOf`) recorded `shouldBe` True
        -- no run directory, no files: the home and the temporary
        -- directory are empty, the project as it was
        listDirectory home `shouldReturn` []
        listDirectory tmpd `shouldReturn` []
        (sort <$> listDirectory root) `shouldReturn` listed
      -- a tape with a step the search does not take: exit code 1
      BL.writeFile (d </> "long.json") (A.encode (Recording (rcSteps rc <> [Now 0]) `asTypeOf` rc))
      fst <$> replay root (d </> "long.json") `shouldReturn` Left (ExitFailure 1)
      -- a tape short of a step: the replay diverges, exit code 1
      BL.writeFile (d </> "short.json") (A.encode (Recording (init (rcSteps rc)) `asTypeOf` rc))
      fst <$> replay root (d </> "short.json") `shouldReturn` Left (ExitFailure 1)
