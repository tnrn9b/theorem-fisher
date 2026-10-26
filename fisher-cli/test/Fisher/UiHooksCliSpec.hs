-- | The UI's hooks in a scripted @fisher chat@ (Fisher UI, task 4): the
-- step log of every file change, the event ledger with
-- @FISHER_UI_LEDGER=1@ (and nothing in state.db without it), and the inbox
-- delivered at the next tool boundary, with the replay still bit-exact.
-- The run log (runs.db) is written always, ledger or not.
module Fisher.UiHooksCliSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian, utc)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.SQLite.Simple (Only (..), close, execute_, open, query_)
import Numeric (showHex)
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath
import System.Posix.Process (getProcessID)
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Inbox (appendInbox, inboxPath, takeInbox)
import Fisher.Agent.Loop (ToolRunner (..))
import Fisher.Agent.RunLock (runLockPath, runPidPath)
import Fisher.Agent.Steps
import Fisher.Agent.UiHooks (newUiHooks, uiClose, uiEmit)
import Fisher.Cli.AgentChat
import Fisher.ClmCliSpec (Run (..), capturingStderr, completion, runChatWithDeps)

import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Runlog
import Fisher.Store (closeStore, createSessionFull, endSession, openStore, snapshotRows)
import Fisher.Tools.Agent (AgentTools (..), newAgentTools)
import Fisher.Tools.Registry (getToolDefinitions)
import qualified Fisher.UI.Db as Db

-- | The ledger as the UI server reads it (read-only).
ledgerEvents :: FilePath -> Text -> Int -> IO [(Int, Text, A.Value)]
ledgerEvents db sid n0 = Db.withRo db (\c -> Db.ledgerEvents c sid n0)

ledgerRun :: FilePath -> Text -> IO (Maybe (Text, Int))
ledgerRun db tag = Db.withRo db (`Db.ledgerRun` tag)

-- | The process's TERMINAL_CWD set for the action (the file tools read it
-- from the process environment), and put back after.
withTerminalCwd :: FilePath -> IO a -> IO a
withTerminalCwd d act =
  bracket (lookupEnv "TERMINAL_CWD") (maybe (unsetEnv "TERMINAL_CWD") (setEnv "TERMINAL_CWD")) $ \_ ->
    setEnv "TERMINAL_CWD" d >> act

call :: Text -> Text -> [(Text, Text)] -> PyVal
call cid name args =
  PDict
    [ ("id", PStr cid), ("type", PStr "function")
    , ("function", PDict [("name", PStr name), ("arguments", PStr (dumpsAscii (PDict [(k, PStr v) | (k, v) <- args])))]) ]

calling :: [PyVal] -> PyVal
calling cs = completion (PDict [("role", PStr "assistant"), ("content", PStr ""), ("tool_calls", PList cs)])

-- | Two write_file calls and a patch, then the answer; A by its absolute
-- path, B relative to the working directory.
answersFor :: FilePath -> [PyVal]
answersFor work =
  [ calling [call "call_w1" "write_file" [("path", T.pack (work </> "A.lean")), ("content", "theorem a : True := sorry\n")]]
  , calling [call "call_w2" "write_file" [("path", "B.lean"), ("content", "theorem b : True := trivial\n")]]
  , calling [call "call_p1" "patch" [("mode", "replace"), ("path", T.pack (work </> "A.lean")), ("old_string", "sorry"), ("new_string", "trivial")]]
  , completion (PDict [("role", PStr "assistant"), ("content", PStr "Done.")]) ]

-- | The file tools, done here as the real ones do them on a local
-- sandbox: a relative path against TERMINAL_CWD (else the process's
-- directory); the first call runs the hook first.
fileTools :: (Text -> IO ()) -> ToolRunner -> ToolRunner
fileTools onCall tr =
  tr
    { trCall = \name args _ _ -> do
        onCall name
        base <- maybe getCurrentDirectory pure =<< lookupEnv "TERMINAL_CWD"
        let at p = let s = T.unpack p in if "/" `isPrefixOf` s then s else base </> s
            arg k = maybe "" pyStrOf (get k args)
        case name of
          "write_file" -> do
            BS.writeFile (at (arg "path")) (TE.encodeUtf8 (arg "content"))
            pure (dumpsUnicode (PDict [("bytes_written", PInt (fromIntegral (BS.length (TE.encodeUtf8 (arg "content"))))), ("dirs_created", PBool False)]))
          "patch" -> do
            old <- TE.decodeUtf8 <$> BS.readFile (at (arg "path"))
            BS.writeFile (at (arg "path")) (TE.encodeUtf8 (T.replace (arg "old_string") (arg "new_string") old))
            pure (dumpsUnicode (PDict [("success", PBool True), ("diff", PStr "-sorry\n+trivial\n"), ("files_modified", PList [PStr (arg "path")])]))
          _ -> pure "{}"
    , trReadFilesSummary = \_ -> pure [] }

runWith :: FilePath -> [(Text, Text)] -> (Text -> IO ()) -> IO Run
runWith tmp env = runWithArgs tmp env []

runWithArgs :: FilePath -> [(Text, Text)] -> [Text] -> (Text -> IO ()) -> IO Run
runWithArgs tmp env extra onCall = do
  let work = tmp </> "work"
  createDirectoryIfMissing True work
  withTerminalCwd work (runIn tmp work env extra onCall)

-- | The run with the file tools working in @term@ (TERMINAL_CWD must name
-- it); A by its absolute path in the session's directory, B relative.
runIn :: FilePath -> FilePath -> [(Text, Text)] -> [Text] -> (Text -> IO ()) -> IO Run
runIn tmp term env extra onCall = do
  let work = tmp </> "work"
      change d = d {cdTools = fmap (\(defs, _) -> (defs, fileTools onCall)) (cdTools d)}
  createDirectoryIfMissing True term
  runChatWithDeps change (answersFor work) tmp env (["-q", "Prove a and b.", "-Q"] <> extra)

onlySession :: FilePath -> IO Text
onlySession fisher = do
  fs <- listDirectory (fisher </> "fisher" </> "replays")
  case fs of
    [f] -> pure (T.pack (dropExtension f))
    _ -> fail ("replay logs: " <> show fs)

posts :: Run -> [HttpReq]
posts r = [h | h <- rReqs r, hrMethod h == "POST"]

messagesOf :: HttpReq -> [PyVal]
messagesOf hr = case hrBody hr >>= get "messages" of
  Just (PList ms) -> ms
  _ -> []

uiTables :: FilePath -> IO [Text]
uiTables db = do
  c <- open db
  rows <- query_ c "SELECT name FROM sqlite_master WHERE name LIKE 'ui_%' ORDER BY name" :: IO [Only Text]
  close c
  pure (map fromOnly rows)

-- | Every run in the run log, newest first, with its events.
loggedRuns :: FilePath -> IO [(RunRow, [(Text, A.Value)])]
loggedRuns fisher = withRunlog (runlogPath fisher) $ \rl -> do
  rs <- listRuns rl Nothing Nothing Nothing 0
  mapM (\r -> (,) r . map (\(_, _, k, d) -> (k, d)) <$> runEvents rl (rrId r)) rs

-- | A row as a search parent makes it for an attempt.
attemptRow :: Text -> NewRun
attemptRow rid =
  NewRun
    { nrId = rid, nrKind = SearchAttempt, nrParent = Just "search-x", nrProject = Nothing, nrProver = Nothing
    , nrBackend = Just "native", nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = []
    , nrStartedAt = 1790000000, nrRawDir = Nothing, nrCapture = Full }

-- | The shell's native session (as the interactive @fisher@ runs it) for
-- one session id, with the model answering every turn "Fine.": the turns,
-- then the shell's exit; the Fisher home.
shellTurns :: FilePath -> Text -> [Text] -> (FilePath -> IO ()) -> IO FilePath
shellTurns tmp sid msgs beforeExit = do
  let home = tmp </> "home"
      fisher = home </> ".fisher"
      work = tmp </> "work"
  createDirectoryIfMissing True fisher
  createDirectoryIfMissing True work
  offset <- newIORef (0 :: Double)
  uuidN <- newIORef (0 :: Int)
  env <- newIORef (M.fromList [("HOME", T.pack home), ("COLUMNS", "60"), ("FISHER_FORCE_PLAIN_OUTPUT", "1"), ("FISHER_TIMEZONE", "UTC"), ("OPENROUTER_API_KEY", "sk-or-test-1234567890")])
  let clock = (1790000000 +) <$> readIORef offset
      sleep d = modifyIORef offset (+ d)
      sys =
        Sys
          { sysNow = clock, sysMonotonic = (1000 +) <$> readIORef offset, sysSleep = sleep
          , sysGetEnv = \k -> M.lookup k <$> readIORef env
          , sysSetEnv = \k v -> modifyIORef env (M.insert k v)
          , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
          , sysOpenBrowser = \_ -> pure False, sysRun = \_ -> pure (Left "no"), sysRunInteractive = \_ -> pure (Left "no")
          , sysWhich = \_ -> pure Nothing, sysPlatform = "linux"
          , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, T.justifyRight 32 '0' (T.pack (showHex (n + 1) ""))))
          , sysTimeZoneAt = \_ -> pure utc, sysHome = home, sysFisherHome = fisher
          , sysOut = \_ -> pure (), sysInput = \_ -> pure Nothing, sysGetPass = \_ -> pure Nothing
          , sysStdinTty = pure False, sysStdoutTty = pure False, sysPid = 4242 }
      models = PDict [("data", PList [PDict [("id", PStr "anthropic/claude-opus-4.6"), ("context_length", PInt 200000)]])]
      respond hr =
        pure . Right . HttpResp 200 [] . TE.encodeUtf8 . dumpsAscii $
          if hrMethod hr == "GET" then models else completion (PDict [("role", PStr "assistant"), ("content", PStr "Fine.")])
  at <- newAgentTools fisher
  (defs, _) <- getToolDefinitions (atRegistry at) (Just ["fisher-cli"]) Nothing
  let deps =
        ChatDeps
          { cdTransport = pure (Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError)))
          , cdTools = Just (defs, \tr -> tr {trCall = \_ _ _ _ -> pure "{}", trReadFilesSummary = \_ -> pure []})
          , cdNowLocal = pure (LocalTime (fromGregorian 2026 10 6) (TimeOfDay 10 15 0))
          , cdRuntime = Runtime clock sleep (pure 0)
          , cdChoice = \_ -> pure 0
          , cdAnimate = pure False
          , cdCwd = pure work
          , cdWhich = \_ -> pure Nothing
          , cdBannerInfo = pure ("<WORK>", Nothing, True) }
      ui = ChatUi (\_ -> pure ()) (\_ -> pure ()) (\_ -> pure ()) (pure 60) False
  ref <- newIORef Nothing
  let sc = shellChat deps sys ui defaultChatOptions ref
  forM_ msgs (scTurn sc sid)
  beforeExit fisher
  _ <- scExit sc
  pure fisher

key :: A.Key -> A.Value -> Maybe A.Value
key k = \case
  A.Object o -> KM.lookup k o
  _ -> Nothing

spec :: Spec
spec = describe "fisher chat with the UI's hooks" $ do
  it "writes a step for each file change: the whole file, its sha256, the tool and the call" $ withSystemTempDirectory "fisher-ui-steps" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    r <- runWith tmp [] (\_ -> pure ())
    rCode r `shouldBe` 0
    sid <- onlySession (rFisher r)
    steps <- readSteps (stepsPath (rFisher r) sid)
    let work = tmp </> "work"
        a1 = "theorem a : True := sorry\n"
        b1 = "theorem b : True := trivial\n"
        a2 = "theorem a : True := trivial\n"
    [(stSeq s, stCallId s, stPath s, stTool s, stText s) | s <- steps]
      `shouldBe` [ (1, "call_w1", work </> "A.lean", "write_file", a1)
                 , (2, "call_w2", work </> "B.lean", "write_file", b1)
                 , (3, "call_p1", work </> "A.lean", "patch", a2) ]
    map stHash steps `shouldBe` map (fileHash . TE.encodeUtf8) [a1, b1, a2]
    stHash (head steps) `shouldBe` "39c28ca9e313e5c87de4a3dd9151bfcf6edff302f112c23e168d41023cb290df"
  it "takes a relative path against TERMINAL_CWD, as the file tools do, not the session's directory" $ withSystemTempDirectory "fisher-ui-tcwd" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    let term = tmp </> "term"
        work = tmp </> "work"
    createDirectoryIfMissing True term
    r <- withTerminalCwd term (runIn tmp term [] [] (\_ -> pure ()))
    rCode r `shouldBe` 0
    doesFileExist (term </> "B.lean") `shouldReturn` True
    doesFileExist (work </> "B.lean") `shouldReturn` False
    sid <- onlySession (rFisher r)
    steps <- readSteps (stepsPath (rFisher r) sid)
    [(stCallId s, stPath s) | s <- steps]
      `shouldBe` [("call_w1", work </> "A.lean"), ("call_w2", term </> "B.lean"), ("call_p1", work </> "A.lean")]
    stText (steps !! 1) `shouldBe` "theorem b : True := trivial\n"
  it "with FISHER_UI_LEDGER=1 writes the events in order, numbered with no gaps, and the run's tag" $ withSystemTempDirectory "fisher-ui-ledger" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    r <- runWith tmp [("FISHER_UI_LEDGER", "1"), ("FISHER_UI_TAG", "run-7"), ("LEAN_PROJECT_PATH", "/lean/project")] (\_ -> pure ())
    rCode r `shouldBe` 0
    sid <- onlySession (rFisher r)
    let db = rFisher r </> "state.db"
    es <- ledgerEvents db sid 0
    map (\(n, _, _) -> n) es `shouldBe` [1 .. length es]
    let kinds = [k | (_, k, _) <- es]
    kinds `shouldSatisfy` (\ks -> all (`elem` ks) ["chat", "tool_start", "tool_end", "message", "cost", "step", "end"])
    length (filter (== "step") kinds) `shouldBe` 3
    length (filter (== "tool_start") kinds) `shouldBe` 3
    -- each call starts before it ends, and its file's step follows its end
    take 3 [k | k <- kinds, k `elem` ["tool_start", "tool_end", "step"]] `shouldBe` ["tool_start", "tool_end", "step"]
    [key "callId" v | (_, "tool_end", v) <- es] `shouldBe` map (Just . A.String) ["call_w1", "call_w2", "call_p1"]
    [key "ok" v | (_, "tool_end", v) <- es] `shouldBe` replicate 3 (Just (A.Bool True))
    [key "input" v | (_, "cost", v) <- es] `shouldBe` replicate 4 (Just (A.Number 100))
    last kinds `shouldBe` "end"
    [key "status" v | (_, "end", v) <- es] `shouldBe` [Just (A.String "completed")]
    ledgerRun db "run-7" `shouldReturn` Just (sid, 4242)
    -- how the chat was started, for a resume (a key never)
    case [v | (_, "chat", v) <- es] of
      [v] -> do
        key "model" v `shouldSatisfy` (\case Just (A.String m) -> not (T.null m); _ -> False)
        (key "env" v >>= key "LEAN_PROJECT_PATH") `shouldBe` Just (A.String "/lean/project")
        (key "env" v >>= key "OPENROUTER_API_KEY") `shouldBe` Nothing
        (key "toolsets" v) `shouldBe` Just A.Null
      vs -> expectationFailure ("chat events: " <> show vs)
  it "without it writes no ui tables, and state.db's rows are as with it" $ withSystemTempDirectory "fisher-ui-off" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    createDirectoryIfMissing True (tmp </> "off")
    createDirectoryIfMissing True (tmp </> "on")
    off <- runWith (tmp </> "off") [] (\_ -> pure ())
    on <- runWith (tmp </> "on") [("FISHER_UI_LEDGER", "1")] (\_ -> pure ())
    uiTables (rFisher off </> "state.db") `shouldReturn` []
    uiTables (rFisher on </> "state.db") `shouldReturn` ["ui_events", "ui_runs"]
    let rows g = do
          st <- openStore (g </> "state.db")
          x <- snapshotRows st
          closeStore st
          pure x
        noPaths = T.replace (T.pack (tmp </> "on")) (T.pack (tmp </> "off"))
        scrub (ss, ms) = (ss, [[(k, case v of PStr t -> PStr (noPaths t); _ -> v) | (k, v) <- m] | m <- ms])
    a <- rows (rFisher off)
    b <- scrub <$> rows (rFisher on)
    b `shouldBe` a
  it "keys a resumed compression child's steps, ledger and inbox on its chain's root" $ withSystemTempDirectory "fisher-ui-root" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    let fisher = tmp </> "home" </> ".fisher"
    createDirectoryIfMissing True fisher
    st <- openStore (fisher </> "state.db")
    createSessionFull st 1789990000 "20261001_000000_root00" "cli" (Just "example/model-a") Nothing Nothing
    endSession st 1789990001 "20261001_000000_root00" "compression"
    createSessionFull st 1789990002 "20261001_000001_kid000" "cli" (Just "example/model-a") Nothing (Just "20261001_000000_root00")
    closeStore st
    appendInbox (inboxPath fisher "20261001_000000_root00") "Also check B."
    r <- runWithArgs tmp [("FISHER_UI_LEDGER", "1"), ("FISHER_UI_TAG", "run-r")] ["-r", "20261001_000001_kid000"] (\_ -> pure ())
    rCode r `shouldBe` 0
    map stCallId <$> readSteps (stepsPath fisher "20261001_000000_root00") `shouldReturn` ["call_w1", "call_w2", "call_p1"]
    doesFileExist (stepsPath fisher "20261001_000001_kid000") `shouldReturn` False
    ledgerRun (fisher </> "state.db") "run-r" `shouldReturn` Just ("20261001_000000_root00", 4242)
    es <- ledgerEvents (fisher </> "state.db") "20261001_000000_root00" 0
    [key "text" v | (_, "inbox", v) <- es] `shouldBe` [Just (A.String "Also check B.")]
  it "replays bit-exact a session stopped mid-turn (after its first tool call) and then resumed with chat -c" $ withSystemTempDirectory "fisher-ui-cut" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    r1 <- runWith tmp [("FISHER_UI_LEDGER", "1")] (\_ -> pure ())
    rCode r1 `shouldBe` 0
    sid <- onlySession (rFisher r1)
    let lg = rFisher r1 </> "fisher" </> "replays" </> T.unpack (sid <> ".jsonl")
        kindOf l = either (const "") (maybe "" pyStrOf . get "kind") (loads l)
    ls <- BC.lines <$> BS.readFile lg
    -- the log as a SIGTERM after the first tool call leaves it (written line by line)
    case break ((== "tool") . kindOf) ls of
      (pre, t : _) -> BS.writeFile lg (BC.unlines (pre <> [t]))
      _ -> expectationFailure "no tool record"
    r2 <- runChatWithDeps id [completion (PDict [("role", PStr "assistant"), ("content", PStr "Carried on from the edit.")])] tmp
      [("FISHER_UI_LEDGER", "1")] ["-c", sid, "-p", "The user edited A.lean by hand."]
    rCode r2 `shouldBe` 0
    kinds <- map kindOf . BC.lines <$> BS.readFile lg
    length (filter (== "agent") kinds) `shouldBe` 2
    replayMain (rSys r2) [sid] `shouldReturn` 0
  it "writes the run log without the ledger: a completed chat run with its calls, their times, the cost and the outcome" $ withSystemTempDirectory "fisher-runlog" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    r <- runWith tmp [] (\_ -> pure ())
    rCode r `shouldBe` 0
    sid <- onlySession (rFisher r)
    uiTables (rFisher r </> "state.db") `shouldReturn` []
    runs <- loggedRuns (rFisher r)
    case runs of
      [(row, es)] -> do
        rrId row `shouldBe` ("chat-" <> sid)
        rrKind row `shouldBe` ChatRun
        rrParent row `shouldBe` Nothing
        rrOutcome row `shouldBe` Just "completed"
        rrEndedAt row `shouldSatisfy` (/= Nothing)
        rrInputTokens row `shouldBe` Just 400
        rrOutputTokens row `shouldBe` Just 40
        let kinds = map fst es
        kinds `shouldSatisfy` (\ks -> all (`elem` ks) ["chat", "tool_start", "tool_end", "message", "cost", "step", "end", "outcome"])
        length (filter (== "tool_start") kinds) `shouldBe` 3
        [key "callId" v | ("tool_end", v) <- es] `shouldBe` map (Just . A.String) ["call_w1", "call_w2", "call_p1"]
        [key "ms" v | ("tool_end", v) <- es] `shouldSatisfy` all (\case Just (A.Number n) -> n >= 0; _ -> False)
        -- the spec's names too, and the whole result
        [key "call_id" v | ("tool_end", v) <- es] `shouldBe` map (Just . A.String) ["call_w1", "call_w2", "call_p1"]
        -- each start carries its call's id, so a reader pairs it with its end
        [key "call_id" v | ("tool_start", v) <- es] `shouldBe` map (Just . A.String) ["call_w1", "call_w2", "call_p1"]
        [key "result" v >>= \case A.String t -> Just ("bytes_written" `T.isInfixOf` t); _ -> Nothing | ("tool_end", v) <- take 1 [e | e@("tool_end", _) <- es]]
          `shouldBe` [Just True]
        [key "input" v | ("cost", v) <- es] `shouldBe` replicate 4 (Just (A.Number 100))
        [key "outcome" v | ("outcome", v) <- es] `shouldBe` [Just (A.String "completed")]
        -- the key the harness's environment holds never reaches the log
        BS.readFile (runlogPath (rFisher r)) >>= (`shouldNotSatisfy` BS.isInfixOf "sk-or-test-1234567890")
      _ -> expectationFailure ("runs: " <> show (map (rrId . fst) runs))
  it "with the ledger as well, writes both, and the ledger's tool_end is as before (no ms)" $ withSystemTempDirectory "fisher-runlog-both" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    r <- runWith tmp [("FISHER_UI_LEDGER", "1")] (\_ -> pure ())
    rCode r `shouldBe` 0
    sid <- onlySession (rFisher r)
    es <- ledgerEvents (rFisher r </> "state.db") sid 0
    [key "ms" v | (_, "tool_end", v) <- es] `shouldBe` replicate 3 Nothing
    [key "call_id" v | (_, "tool_start", v) <- es] `shouldBe` replicate 3 Nothing
    [k | (_, k, _) <- es] `shouldNotContain` ["outcome"]
    runs <- loggedRuns (rFisher r)
    map (rrOutcome . fst) runs `shouldBe` [Just "completed"]
  it "logs a chat the web app started (FISHER_UI_TAG set) as a ui-run" $ withSystemTempDirectory "fisher-runlog-ui" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    r <- runWith tmp [("FISHER_UI_LEDGER", "1"), ("FISHER_UI_TAG", "run-u")] (\_ -> pure ())
    rCode r `shouldBe` 0
    sid <- onlySession (rFisher r)
    runs <- loggedRuns (rFisher r)
    [(rrId row, rrKind row, rrOutcome row) | (row, _) <- runs] `shouldBe` [("chat-" <> sid, UiRun, Just "completed")]
  it "with FISHER_RUN_ID naming a row its parent made, adds its events to that row and starts no other" $ withSystemTempDirectory "fisher-runlog-att" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    let fisher = tmp </> "home" </> ".fisher"
    createDirectoryIfMissing True fisher
    withRunlog (runlogPath fisher) $ \rl -> do
      startRun rl (attemptRow "att-1")
      event rl "att-1" 0 "parent_note" (A.object [])
    r <- runWith tmp [("FISHER_RUN_ID", "att-1"), ("FISHER_RUN_PARENT", "search-x")] (\_ -> pure ())
    rCode r `shouldBe` 0
    runs <- loggedRuns fisher
    case runs of
      [(row, es)] -> do
        rrId row `shouldBe` "att-1"
        rrKind row `shouldBe` SearchAttempt
        rrParent row `shouldBe` Just "search-x"
        rrStartedAt row `shouldBe` 1790000000
        rrOutcome row `shouldBe` Just "completed"
        -- the parent's event is kept: the child did not start the run afresh
        take 1 (map fst es) `shouldBe` ["parent_note"]
        length (filter ((== "tool_start") . fst) es) `shouldBe` 3
        -- the session behind the attempt, so import-sessions skips it
        sid <- onlySession fisher
        [key "sid" v | ("session", v) <- es] `shouldBe` [Just (A.String sid)]
      _ -> expectationFailure ("runs: " <> show (map (rrId . fst) runs))
  it "with FISHER_RUN_ID naming no row, starts it as a search attempt under FISHER_RUN_PARENT" $ withSystemTempDirectory "fisher-runlog-att2" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    r <- runWith tmp [("FISHER_RUN_ID", "att-2"), ("FISHER_RUN_PARENT", "search-x")] (\_ -> pure ())
    rCode r `shouldBe` 0
    runs <- loggedRuns (rFisher r)
    [(rrId row, rrKind row, rrParent row, rrOutcome row) | (row, _) <- runs]
      `shouldBe` [("att-2", SearchAttempt, Just "search-x", Just "completed")]
  it "runs on when the run log cannot be opened, and says so once on stderr" $ withSystemTempDirectory "fisher-runlog-bad" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    let fisher = tmp </> "home" </> ".fisher"
    -- a directory where the database should be
    createDirectoryIfMissing True (runlogPath fisher)
    (r, err) <- capturingStderr tmp (runWith tmp [] (\_ -> pure ()))
    rCode r `shouldBe` 0
    rOut r `shouldSatisfy` T.isInfixOf "Done."
    length (filter ("fisher: run log: " `T.isPrefixOf`) (T.lines err)) `shouldBe` 1
  it "ends a chat of two turns once, when the session closes: one outcome, and no end while it lives" $ withSystemTempDirectory "fisher-runlog-turns" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    let sid = "20261006_101500_turns0"
    live <- newIORef Nothing
    fisher <- shellTurns tmp sid ["First.", "Second."] $ \f ->
      loggedRuns f >>= writeIORef live . Just . map (\(row, es) -> (rrEndedAt row, length [() | ("outcome", _) <- es]))
    readIORef live `shouldReturn` Just [(Nothing, 0)]
    runs <- loggedRuns fisher
    case runs of
      [(row, es)] -> do
        rrId row `shouldBe` ("chat-" <> sid)
        rrOutcome row `shouldBe` Just "completed"
        rrEndedAt row `shouldSatisfy` (/= Nothing)
        length [() | ("end", _) <- es] `shouldBe` 2
        [key "outcome" v | ("outcome", v) <- es] `shouldBe` [Just (A.String "completed")]
        fst (last es) `shouldBe` "outcome"
      _ -> expectationFailure ("runs: " <> show (map (rrId . fst) runs))
  it "after a failed write writes no more to the run log, at once, and says so once" $ withSystemTempDirectory "fisher-runlog-stop" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    let db = runlogPath tmp
    (secs, err) <- capturingStderr tmp $ withRunlog db $ \rl -> do
      startRun rl (attemptRow "r1")
      h <- newUiHooks Nothing (tmp </> "steps.jsonl") (\_ p -> pure (T.unpack p)) (Just (rl, "r1", 1790000000))
      let other act = do
            c <- open db
            execute_ c act
            close c
      -- the first write fails (no events table)
      other "DROP TABLE events"
      uiEmit h "first" []
      -- the table back, a second write is not tried
      other "CREATE TABLE events (run TEXT NOT NULL, seq INTEGER NOT NULL, t REAL NOT NULL, kind TEXT NOT NULL, data TEXT NOT NULL)"
      t0 <- getPOSIXTime
      uiEmit h "second" []
      uiEmit h "third" []
      t1 <- getPOSIXTime
      uiClose h
      pure (realToFrac (t1 - t0) :: Double)
    secs `shouldSatisfy` (< 1)
    length (filter ("fisher: run log: " `T.isPrefixOf`) (T.lines err)) `shouldBe` 1
    withRunlog db (`runEvents` "r1") >>= (`shouldBe` [])
    -- nor the run's end (uiClose ends it)
    withRunlog db (`getRun` "r1") >>= (`shouldSatisfy` maybe False ((== Nothing) . rrEndedAt))
  it "delivers an inbox line once, at the next tool boundary, as a user turn; the replay is bit-exact" $ inboxRun True []
  it "does so under --context clm too" $ inboxRun True ["--context", "clm"]
  it "does so without the ledger too (a session started outside the UI)" $ inboxRun False []
  it "holds the session's run lock while it lives and keeps FISHER_UI_TAG from the tools' environment" $ withSystemTempDirectory "fisher-ui-lock" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    seen <- newIORef Nothing
    let onCall _ = lookupEnv "FISHER_UI_TAG" >>= \v -> modifyIORef seen (maybe (Just v) Just)
    r <- bracket (lookupEnv "FISHER_UI_TAG") (maybe (unsetEnv "FISHER_UI_TAG") (setEnv "FISHER_UI_TAG")) $ \_ -> do
      setEnv "FISHER_UI_TAG" "run-env"
      x <- runWith tmp [("FISHER_UI_LEDGER", "1"), ("FISHER_UI_TAG", "run-env")] onCall
      after <- lookupEnv "FISHER_UI_TAG"
      after `shouldBe` Nothing
      pure x
    rCode r `shouldBe` 0
    readIORef seen `shouldReturn` Just Nothing
    sid <- onlySession (rFisher r)
    ledgerRun (rFisher r </> "state.db") "run-env" `shouldReturn` Just (sid, 4242)
    (code, _, _) <- readProcessWithExitCode "python3" ["-c", "import fcntl,sys\nf=open(sys.argv[1],'a')\ntry:\n fcntl.lockf(f, fcntl.LOCK_EX | fcntl.LOCK_NB)\nexcept OSError:\n sys.exit(1)\n", runLockPath (rFisher r) sid] ""
    code `shouldBe` ExitFailure 1
    pid <- getProcessID
    readFile (runPidPath (rFisher r) sid) `shouldReturn` (show pid <> "\n")
  where
   inboxRun ledger extra = withSystemTempDirectory "fisher-ui-inbox" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    let fisher = tmp </> "home" </> ".fisher"
    sent <- newIORef False
    let onCall _ = do
          done <- readIORef sent
          if done
            then pure ()
            else do
              writeIORef sent True
              sid <- onlySession fisher
              appendInbox (inboxPath fisher sid) "Also check B."
    r <- runWithArgs tmp [("FISHER_UI_LEDGER", "1") | ledger] extra onCall
    rCode r `shouldBe` 0
    sid <- onlySession (rFisher r)
    let ps = posts r
        -- (prompt caching sends the last messages' content as blocks)
        textOf m = case get "content" m of
          Just (PStr t) -> t
          Just (PList bs) -> T.concat [t | b <- bs, Just (PStr t) <- [get "text" b]]
          _ -> ""
        isInbox m = get "role" m == Just (PStr "user") && textOf m == "Also check B."
    length ps `shouldBe` 4
    map (length . filter isInbox . messagesOf) ps `shouldBe` [0, 1, 1, 1]
    -- right after the first call's result
    let ms2 = messagesOf (ps !! 1)
        upTo = takeWhile (not . isInbox) ms2
    get "role" (last upTo) `shouldBe` Just (PStr "tool")
    get "tool_call_id" (last upTo) `shouldBe` Just (PStr "call_w1")
    lg <- BS.readFile (rFisher r </> "fisher" </> "replays" </> T.unpack (sid <> ".jsonl"))
    let inboxes = [v | l <- BC.lines lg, Right v <- [loads l], get "kind" v == Just (PStr "inbox")]
    [(get "boundary" v, get "text" v) | v <- inboxes] `shouldBe` [(Just (PInt 1), Just (PStr "Also check B."))]
    replayMain (rSys r) [sid] `shouldReturn` 0
    takeInbox (inboxPath fisher sid) `shouldReturn` []
    if ledger
      then do
        es <- ledgerEvents (rFisher r </> "state.db") sid 0
        [key "text" v | (_, "inbox", v) <- es] `shouldBe` [Just (A.String "Also check B.")]
      else uiTables (rFisher r </> "state.db") `shouldReturn` []
