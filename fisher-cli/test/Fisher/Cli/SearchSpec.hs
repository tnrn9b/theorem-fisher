-- | @fisher search@'s arguments with @--graph@ and the pure pieces of its
-- wiring: the refusals, the attempt prompt with the brief, the model call's
-- result as the graph search takes it, and the native backend's model.
module Fisher.Cli.SearchSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import qualified Data.ByteString as BS
import System.Exit (ExitCode (..))
import System.Posix.Signals (Handler (Default), installHandler, raiseSignal, sigTERM)
import System.Timeout (timeout)
import Data.List (isInfixOf, sort)
import Data.Maybe (isJust)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import Data.Char (toUpper)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Fisher.Cli.Search
import Fisher.Cli.Watch (Watched (..))
import Fisher.Search.Child (ChildStats (..))
import Fisher.LLM.Normalize (Usage (..))
import Fisher.Py.Value (obj, str)
import Fisher.RLM.Types (ModelError (..), Reply (..))
import Fisher.Search.Auto (AutoConfig (..), defaultAutoConfig)
import Fisher.Search.Disprove (Disproof (..))
import Fisher.Search.Facts (FactsConfig (..), defaultFacts)
import Fisher.Search.Library (Lemma (..), libraryDir, loadLibrary, saveLibrary)
import Fisher.Search.Retrieval (RetrievalEnv (..), Terminated (..))
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Fisher.Search.GraphRun (AskError (..))
import Fisher.Search.Scheduler (GoalState (..), GoalStatus (..), SearchConfig (..), SearchReport (..))
import Fisher.TestUtil (oldName)

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
gs st by d = GoalState st [] [] Nothing Nothing by Nothing d False Nothing []

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
