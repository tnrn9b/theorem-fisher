-- | @fisher runs list|show|export|import|import-sessions@ over a temporary
-- run log, a bench fixture (test/fixtures/runs/h) and a temporary state.db.
module Fisher.Cli.RunsSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, helper, info, renderFailure, (<**>))
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist)
import System.FilePath (isAbsolute)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Cli.Runs
import Fisher.Cli.Search (searchRunDir)
import Fisher.Learn.Stats (Cell (..), Knob (..), Level (..), cellAt, loadStats)
import Fisher.Runlog
import Fisher.Search.Lean (Decl (..), declarations)
import Fisher.Search.Shape (renderShape, shapeOf)
import Fisher.Store (appendMessage, createSession, createSessionFull, endSession, withStore)

fixtureDir :: FilePath
fixtureDir = "test/fixtures/runs/h"

-- | A run of the command over a run log and a store in a temporary
-- directory: the parsed argv, then its output.
runIn :: FilePath -> [String] -> IO Text
runIn tmp argv = do
  cmd <- case execParserPure defaultPrefs (info runsParser mempty) argv of
    Success c -> pure c
    _ -> fail ("argv did not parse: " <> show argv)
  out <- newIORef ([] :: [Text])
  runRuns (RunsEnv (tmp </> "runs.db") (tmp </> "state.db") (pure 1000000) (\t -> modifyIORef out (t :))) cmd
  T.unlines . reverse <$> readIORef out

newRun :: Text -> RunKind -> Maybe Text -> Maybe Text -> Double -> NewRun
newRun i k par back t =
  NewRun
    { nrId = i, nrKind = k, nrParent = par, nrProject = Just "/work/p", nrProver = Just "lean", nrBackend = back
    , nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = [], nrStartedAt = t, nrRawDir = Nothing
    , nrCapture = Full }

withLog :: FilePath -> (Runlog -> IO a) -> IO a
withLog tmp = withRunlog (tmp </> "runs.db")

spec :: Spec
spec = do
  describe "import --bench" $ do
    it "lays a bench run out as a search: one run, its attempts and its goals with shapes" $
      withSystemTempDirectory "runs" $ \tmp -> do
        out <- runIn tmp ["import", "--bench", fixtureDir]
        out `shouldSatisfy` T.isInfixOf "imported 1 run"
        withLog tmp $ \rl -> do
          [p] <- listRuns rl Nothing Nothing (Just BenchImport) 0
          T.unpack (rrId p) `shouldSatisfy` ("import-" `isPrefixOf`)
          T.length (rrId p) `shouldBe` T.length "import-" + 16
          rrProject p `shouldBe` Just "/work/h"
          rrBackend p `shouldBe` Just "fisher,claude-code"
          rrOutcome p `shouldBe` Just "closed"
          rrCostUsd p `shouldBe` Just 3
          rrCapture p `shouldBe` Imported
          maybe 0 (subtract (rrStartedAt p)) (rrEndedAt p) `shouldBe` 100
          as <- listRuns rl Nothing Nothing (Just SearchAttempt) 0
          [(rrId a, rrParent a, rrBackend a, rrOutcome a, rrCostUsd a) | a <- reverse as]
            `shouldBe` [ (rrId p <> "-1", Just (rrId p), Just "fisher", Just "failed", Just 1)
                       , (rrId p <> "-2", Just (rrId p), Just "claude-code", Just "closed", Just 2) ]
          [(rrTier a, rrModel a, rrTurns a, rrInputTokens a) | a <- reverse as]
            `shouldBe` [(Just "0", Just "m1", Just 12, Just 1000), (Nothing, Nothing, Just 20, Just 3000)]
          [rrStartedAt a - rrStartedAt p | a <- reverse as] `shouldBe` [0.5, 41]
          src <- T.pack <$> readFile (fixtureDir </> "auto" </> "1" </> "base.lean")
          let stmt = head [dStatement d | d <- declarations src, dName d == "N.big"]
              shape = renderShape (shapeOf stmt)
          forM_ as $ \a -> do
            evs <- runEvents rl (rrId a)
            take 1 [(k, d) | (_, _, k, d) <- evs]
              `shouldBe` [("attempt", object ["goal" .= ("N.big" :: Text), "shape" .= shape, "backend" .= rrBackend a])]
          runGoals rl (rrId p)
            `shouldReturn` [GoalRow "N.big" stmt shape "closed" (Just "attempt:2") (Just 90) (Just 3)]
          pevs <- runEvents rl (rrId p)
          [(t, k) | (_, t, k, _) <- pevs] `shouldBe` [(0.2, "auto missed")]
          st <- loadStats rl (projectKeyOf "/work/h")
          cAttempts (cellAt st LShape shape KBackend "fisher") `shouldBe` 1
          cCloses (cellAt st LShape shape KBackend "claude-code") `shouldBe` 1

    it "adds nothing the second time" $
      withSystemTempDirectory "runs" $ \tmp -> do
        _ <- runIn tmp ["import", "--bench", fixtureDir]
        out <- runIn tmp ["import", "--bench", fixtureDir]
        out `shouldSatisfy` T.isInfixOf "imported 0 runs"
        withLog tmp $ \rl -> do
          ps <- listRuns rl Nothing Nothing Nothing 0
          length ps `shouldBe` 3
          forM_ ps $ \r -> do
            evs <- runEvents rl (rrId r)
            length evs `shouldBe` 1

    it "skips a run directory a live search already logged" $
      withSystemTempDirectory "runs" $ \tmp -> do
        live <- canonicalizePath (fixtureDir </> "auto" </> "1")
        withLog tmp $ \rl -> startRun rl ((newRun "search-1-abcd1234" SearchRun Nothing Nothing 100) {nrRawDir = Just live})
        out <- runIn tmp ["import", "--bench", fixtureDir]
        out `shouldSatisfy` T.isInfixOf "imported 0 runs"
        withLog tmp $ \rl -> do
          rs <- listRuns rl Nothing Nothing Nothing 0
          map rrId rs `shouldBe` ["search-1-abcd1234"]

    it "lays a plain import out as a search run" $
      withSystemTempDirectory "runs" $ \tmp -> do
        _ <- runIn tmp ["import", fixtureDir]
        withLog tmp $ \rl -> do
          ps <- listRuns rl Nothing Nothing (Just SearchRun) 0
          length ps `shouldBe` 1

    it "skips a directory a live search logged with a relative --out (made absolute before it is logged)" $
      withSystemTempDirectory "runs" $ \tmp -> do
        live <- searchRunDir "/home/u" "20261024-000000" (Just (fixtureDir </> "auto" </> "1"))
        live `shouldSatisfy` isAbsolute
        searchRunDir "/home/u" "20261024-000000" Nothing `shouldReturn` "/home/u/.cache/fisher/search/20261024-000000"
        withLog tmp $ \rl -> startRun rl ((newRun "search-1-rel" SearchRun Nothing Nothing 100) {nrRawDir = Just live})
        out <- runIn tmp ["import", "--bench", fixtureDir]
        out `shouldSatisfy` T.isInfixOf "imported 0 runs"

    it "counts a report.json it cannot read as skipped and imports the rest" $
      withSystemTempDirectory "runs" $ \tmp -> do
        let tree = tmp </> "tree"
        createDirectoryIfMissing True (tree </> "a" </> "report.json")
        createDirectoryIfMissing True (tree </> "b")
        BS.readFile (fixtureDir </> "auto" </> "1" </> "report.json") >>= BS.writeFile (tree </> "b" </> "report.json")
        out <- runIn tmp ["import", tree]
        out `shouldSatisfy` T.isInfixOf "imported 1 run"
        out `shouldSatisfy` T.isInfixOf "1 skipped"

    it "numbers attempts with no attempt field by their place in the list" $
      withSystemTempDirectory "runs" $ \tmp -> do
        let run = tmp </> "r"
        createDirectoryIfMissing True run
        BS.writeFile (run </> "report.json")
          "{\"project\": \"/w\", \"elapsed_s\": 10, \"goals\": [], \"attempts\": [\
          \{\"goal\": \"g\", \"backend\": \"fisher\", \"outcome\": \"closed\"},\
          \{\"goal\": \"g\", \"backend\": \"fisher\", \"outcome\": \"rejected: x\"}]}"
        _ <- runIn tmp ["import", run]
        withLog tmp $ \rl -> do
          as <- listRuns rl Nothing Nothing (Just SearchAttempt) 0
          [T.takeWhileEnd (/= '-') (rrId a) | a <- as] `shouldMatchList` ["1", "2"]

    it "makes no run log when the directory is missing or holds no report" $
      withSystemTempDirectory "runs" $ \tmp -> do
        createDirectoryIfMissing True (tmp </> "empty")
        missing <- runIn tmp ["import", tmp </> "nope"]
        missing `shouldSatisfy` T.isInfixOf "no directory"
        empty' <- runIn tmp ["import", tmp </> "empty"]
        empty' `shouldSatisfy` T.isInfixOf "no report.json"
        doesFileExist (tmp </> "runs.db") `shouldReturn` False

    it "says so when the directory holds no report" $
      withSystemTempDirectory "runs" $ \tmp -> do
        out <- runIn tmp ["import", tmp]
        out `shouldSatisfy` T.isInfixOf "no report.json"

  describe "list" $
    it "prints one line per run, newest first, with id, kind, backend, outcome and cost" $
      withSystemTempDirectory "runs" $ \tmp -> do
        withLog tmp $ \rl -> do
          startRun rl (newRun "old-run" LaunchRun Nothing (Just "claude-code") 100)
          endRun rl "old-run" (RunEnd 150 (Just 0) "closed" (Just 1.5) Nothing Nothing Nothing)
          startRun rl (newRun "new-run" ChatRun Nothing (Just "native") 200)
        out <- runIn tmp ["list"]
        let rows = filter (\l -> any (`T.isInfixOf` l) ["old-run", "new-run"]) (T.lines out)
        length rows `shouldBe` 2
        length (T.lines out) `shouldBe` 3
        head rows `shouldSatisfy` T.isInfixOf "new-run"
        forM_ ["old-run", "launch", "claude-code", "closed", "$1.50"] $ \w -> (rows !! 1) `shouldSatisfy` T.isInfixOf w
        forM_ ["chat", "native"] $ \w -> head rows `shouldSatisfy` T.isInfixOf w
        kinds <- runIn tmp ["list", "--kind", "chat"]
        kinds `shouldSatisfy` (not . T.isInfixOf "old-run")
        limited <- runIn tmp ["list", "--limit", "1"]
        limited `shouldSatisfy` (not . T.isInfixOf "old-run")

  describe "show" $
    it "prints the goals and the first 20 events, as stored" $
      withSystemTempDirectory "runs" $ \tmp -> do
        withLog tmp $ \rl -> do
          startRun rl (newRun "s1" SearchRun Nothing (Just "fisher") 100)
          upsertGoal rl "s1" (GoalRow "N.big" "n + 0 = n" "eq|Nat|s" "closed" (Just "auto") (Just 3) (Just 0))
          forM_ [1 .. 25 :: Int] $ \i -> event rl "s1" (fromIntegral i) (T.pack ("k" <> pad i)) (object ["i" .= i])
          event rl "s1" 26 "message" (object ["text" .= ("key OPENROUTER_API_KEY=sk-or-v1-abcdef0123456789abcdef" :: Text)])
        out <- runIn tmp ["show", "s1"]
        out `shouldSatisfy` T.isInfixOf "N.big"
        out `shouldSatisfy` T.isInfixOf "k20"
        out `shouldSatisfy` (not . T.isInfixOf "k21")
        out `shouldSatisfy` T.isInfixOf "26 events"
        missing <- runIn tmp ["show", "nope"]
        missing `shouldSatisfy` T.isInfixOf "no run nope"

  it "show never prints a secret the run log stored redacted" $
    withSystemTempDirectory "runs" $ \tmp -> do
      withLog tmp $ \rl -> do
        startRun rl (newRun "s2" ChatRun Nothing Nothing 100)
        event rl "s2" 1 "message" (object ["text" .= ("OPENROUTER_API_KEY=sk-or-v1-abcdef0123456789abcdef" :: Text)])
      out <- runIn tmp ["show", "s2"]
      out `shouldSatisfy` T.isInfixOf "message"
      out `shouldSatisfy` (not . T.isInfixOf "abcdef0123456789abcdef")

  describe "export" $
    it "writes a JSON array of the runs with their goals and events" $
      withSystemTempDirectory "runs" $ \tmp -> do
        withLog tmp $ \rl -> do
          startRun rl (newRun "a" SearchRun Nothing Nothing 100)
          event rl "a" 0 "portfolio" (object ["tactics" .= ([] :: [Text])])
          startRun rl ((newRun "b" ChatRun Nothing Nothing 200) {nrProject = Just "/work/other"})
        _ <- runIn tmp ["export", tmp </> "all.json"]
        Just (Array xs) <- A.decodeStrict <$> BS.readFile (tmp </> "all.json")
        length xs `shouldBe` 2
        forM_ xs $ \case
          Object o -> (KM.member "goals" o, KM.member "events" o) `shouldBe` (True, True)
          _ -> expectationFailure "not an object"
        _ <- runIn tmp ["export", "--project", "/work/p", tmp </> "p.json"]
        Just (Array ys) <- A.decodeStrict <$> BS.readFile (tmp </> "p.json")
        length ys `shouldBe` 1

  describe "import-sessions" $
    it "makes each state.db session a chat run with its messages as events" $
      withSystemTempDirectory "runs" $ \tmp -> do
        withStore (tmp </> "state.db") $ \st -> do
          createSession st 1000 "20261024_100000_abcdef" "cli" (Just "m")
          appendMessage st 1001 "20261024_100000_abcdef" "user" (Just "prove it")
          appendMessage st 1005 "20261024_100000_abcdef" "assistant" (Just "done")
          appendMessage st 1006 "20261024_100000_abcdef" "tool" (Just "ok")
          endSession st 1010 "20261024_100000_abcdef" "user_exit"
        out <- runIn tmp ["import-sessions"]
        out `shouldSatisfy` T.isInfixOf "imported 1 session"
        withLog tmp $ \rl -> do
          Just r <- getRun rl "chat-20261024_100000_abcdef"
          rrKind r `shouldBe` ChatRun
          rrCostUsd r `shouldBe` Nothing
          rrCapture r `shouldBe` Imported
          evs <- runEvents rl (rrId r)
          [(t, k) | (_, t, k, _) <- evs] `shouldBe` [(1, "message"), (5, "message"), (6, "tool_end")]
        again <- runIn tmp ["import-sessions"]
        again `shouldSatisfy` T.isInfixOf "imported 0 sessions"

  it "import-sessions skips sessions already logged: a cron run, a continuation of a logged chat, an attempt's session" $
    withSystemTempDirectory "runs" $ \tmp -> do
      withStore (tmp </> "state.db") $ \st -> do
        createSession st 1000 "cron_job1_20261024" "cron" (Just "m")
        createSession st 1000 "s1" "cli" (Just "m")
        createSessionFull st 1100 "s2" "cli" (Just "m") Nothing (Just "s1")
        createSessionFull st 1200 "s3" "cli" (Just "m") Nothing (Just "s2")
        createSession st 1000 "att" "cli" (Just "m")
        createSession st 1000 "fresh" "cli" (Just "m")
        forM_ ["cron_job1_20261024", "s1", "s2", "s3", "att", "fresh"] $ \sid -> appendMessage st 1001 sid "user" (Just "hi")
      withLog tmp $ \rl -> do
        startRun rl (newRun "cron_job1_20261024" CronRun Nothing Nothing 1000)
        startRun rl (newRun "chat-s1" ChatRun Nothing Nothing 1000)
        startRun rl (newRun "search-x-1" SearchAttempt (Just "search-x") Nothing 1000)
        event rl "search-x-1" 0 "session" (object ["sid" .= ("att" :: Text)])
      out <- runIn tmp ["import-sessions"]
      out `shouldSatisfy` T.isInfixOf "imported 1 session"
      withLog tmp $ \rl -> do
        chats <- listRuns rl Nothing Nothing (Just ChatRun) 0
        map rrId chats `shouldMatchList` ["chat-s1", "chat-fresh"]

  it "import-sessions with no state.db imports nothing and makes none" $
    withSystemTempDirectory "runs" $ \tmp -> do
      out <- runIn tmp ["import-sessions"]
      out `shouldSatisfy` T.isInfixOf "imported 0 sessions"
      doesFileExist (tmp </> "state.db") `shouldReturn` False

  it "list on an empty log gives the import hint" $
    withSystemTempDirectory "runs" $ \tmp -> do
      out <- runIn tmp ["list"]
      out `shouldSatisfy` T.isInfixOf "no runs logged yet: fisher runs import ~/.cache/fisher/search"

  it "fisher runs --help lists the five commands" $
    case execParserPure defaultPrefs (info (runsParser <**> helper) mempty) ["--help"] of
      Failure f -> do
        let (text, _) = renderFailure f "fisher runs"
        forM_ ["list", "show", "export", "import", "import-sessions"] $ \c ->
          T.words (T.pack text) `shouldSatisfy` elem c
      _ -> expectationFailure "--help did not fail with the help text"
  where
    pad i = if i < 10 then "0" <> show i else show i
