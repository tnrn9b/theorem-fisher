-- | The UI's hooks in a scripted @fisher chat@ (Fisher UI, task 4): the
-- step log of every file change, the event ledger with
-- @FISHER_UI_LEDGER=1@ (and nothing in state.db without it), and the inbox
-- delivered at the next tool boundary, with the replay still bit-exact.
module Fisher.UiHooksCliSpec (spec) where

import Control.Exception (bracket)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple (Only (..), close, open, query_)
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
import Fisher.Cli.AgentChat
import Fisher.ClmCliSpec (Run (..), completion, runChatWithDeps)

import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Store (closeStore, createSessionFull, endSession, openStore, snapshotRows)
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
onlySession gauss = do
  fs <- listDirectory (gauss </> "fisher" </> "replays")
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
    sid <- onlySession (rGauss r)
    steps <- readSteps (stepsPath (rGauss r) sid)
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
    sid <- onlySession (rGauss r)
    steps <- readSteps (stepsPath (rGauss r) sid)
    [(stCallId s, stPath s) | s <- steps]
      `shouldBe` [("call_w1", work </> "A.lean"), ("call_w2", term </> "B.lean"), ("call_p1", work </> "A.lean")]
    stText (steps !! 1) `shouldBe` "theorem b : True := trivial\n"
  it "with FISHER_UI_LEDGER=1 writes the events in order, numbered with no gaps, and the run's tag" $ withSystemTempDirectory "fisher-ui-ledger" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    r <- runWith tmp [("FISHER_UI_LEDGER", "1"), ("FISHER_UI_TAG", "run-7"), ("LEAN_PROJECT_PATH", "/lean/project")] (\_ -> pure ())
    rCode r `shouldBe` 0
    sid <- onlySession (rGauss r)
    let db = rGauss r </> "state.db"
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
    uiTables (rGauss off </> "state.db") `shouldReturn` []
    uiTables (rGauss on </> "state.db") `shouldReturn` ["ui_events", "ui_runs"]
    let rows g = do
          st <- openStore (g </> "state.db")
          x <- snapshotRows st
          closeStore st
          pure x
        noPaths = T.replace (T.pack (tmp </> "on")) (T.pack (tmp </> "off"))
        scrub (ss, ms) = (ss, [[(k, case v of PStr t -> PStr (noPaths t); _ -> v) | (k, v) <- m] | m <- ms])
    a <- rows (rGauss off)
    b <- scrub <$> rows (rGauss on)
    b `shouldBe` a
  it "keys a resumed compression child's steps, ledger and inbox on its chain's root" $ withSystemTempDirectory "fisher-ui-root" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    let gauss = tmp </> "home" </> ".gauss"
    createDirectoryIfMissing True gauss
    st <- openStore (gauss </> "state.db")
    createSessionFull st 1789990000 "20261001_000000_root00" "cli" (Just "example/model-a") Nothing Nothing
    endSession st 1789990001 "20261001_000000_root00" "compression"
    createSessionFull st 1789990002 "20261001_000001_kid000" "cli" (Just "example/model-a") Nothing (Just "20261001_000000_root00")
    closeStore st
    appendInbox (inboxPath gauss "20261001_000000_root00") "Also check B."
    r <- runWithArgs tmp [("FISHER_UI_LEDGER", "1"), ("FISHER_UI_TAG", "run-r")] ["-r", "20261001_000001_kid000"] (\_ -> pure ())
    rCode r `shouldBe` 0
    map stCallId <$> readSteps (stepsPath gauss "20261001_000000_root00") `shouldReturn` ["call_w1", "call_w2", "call_p1"]
    doesFileExist (stepsPath gauss "20261001_000001_kid000") `shouldReturn` False
    ledgerRun (gauss </> "state.db") "run-r" `shouldReturn` Just ("20261001_000000_root00", 4242)
    es <- ledgerEvents (gauss </> "state.db") "20261001_000000_root00" 0
    [key "text" v | (_, "inbox", v) <- es] `shouldBe` [Just (A.String "Also check B.")]
  it "replays bit-exact a session stopped mid-turn (after its first tool call) and then resumed with chat -c" $ withSystemTempDirectory "fisher-ui-cut" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    r1 <- runWith tmp [("FISHER_UI_LEDGER", "1")] (\_ -> pure ())
    rCode r1 `shouldBe` 0
    sid <- onlySession (rGauss r1)
    let lg = rGauss r1 </> "fisher" </> "replays" </> T.unpack (sid <> ".jsonl")
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
    sid <- onlySession (rGauss r)
    ledgerRun (rGauss r </> "state.db") "run-env" `shouldReturn` Just (sid, 4242)
    (code, _, _) <- readProcessWithExitCode "python3" ["-c", "import fcntl,sys\nf=open(sys.argv[1],'a')\ntry:\n fcntl.lockf(f, fcntl.LOCK_EX | fcntl.LOCK_NB)\nexcept OSError:\n sys.exit(1)\n", runLockPath (rGauss r) sid] ""
    code `shouldBe` ExitFailure 1
    pid <- getProcessID
    readFile (runPidPath (rGauss r) sid) `shouldReturn` (show pid <> "\n")
  where
   inboxRun ledger extra = withSystemTempDirectory "fisher-ui-inbox" $ \tmp0 -> do
    tmp <- canonicalizePath tmp0
    let gauss = tmp </> "home" </> ".gauss"
    sent <- newIORef False
    let onCall _ = do
          done <- readIORef sent
          if done
            then pure ()
            else do
              writeIORef sent True
              sid <- onlySession gauss
              appendInbox (inboxPath gauss sid) "Also check B."
    r <- runWithArgs tmp [("FISHER_UI_LEDGER", "1") | ledger] extra onCall
    rCode r `shouldBe` 0
    sid <- onlySession (rGauss r)
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
    lg <- BS.readFile (rGauss r </> "fisher" </> "replays" </> T.unpack (sid <> ".jsonl"))
    let inboxes = [v | l <- BC.lines lg, Right v <- [loads l], get "kind" v == Just (PStr "inbox")]
    [(get "boundary" v, get "text" v) | v <- inboxes] `shouldBe` [(Just (PInt 1), Just (PStr "Also check B."))]
    replayMain (rSys r) [sid] `shouldReturn` 0
    takeInbox (inboxPath gauss sid) `shouldReturn` []
    if ledger
      then do
        es <- ledgerEvents (rGauss r </> "state.db") sid 0
        [key "text" v | (_, "inbox", v) <- es] `shouldBe` [Just (A.String "Also check B.")]
      else uiTables (rGauss r </> "state.db") `shouldReturn` []
