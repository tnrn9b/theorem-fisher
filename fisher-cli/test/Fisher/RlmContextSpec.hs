-- | @--context rlm@ through the CLI (spec phase 2, sections 2, 4 to 6): a
-- scripted turn through @fisher chat@ and the shell's native session, the
-- choice from @fisher.context@, the flag's errors and the usage text, the
-- refusal without @python3@ on the session's PATH, a resume over an invalid
-- @rlm.json@, and the warning ACP and cron give before keeping the
-- compressor. The parts that need no chat session are in
-- "Fisher.RlmSetupSpec".
module Fisher.RlmContextSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian, utc)
import Numeric (showHex)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Context.Rlm (RlmState (..), StoredTurn (..), stateToPy)
import Fisher.Agent.Loop (ToolRunner (..), agentRlmState)
import Fisher.Cli.AgentChat
import Fisher.Cli.Native
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types (HttpReq (..))
import Fisher.Py.Value
import Fisher.Tools.Agent (AgentTools (..), newAgentTools)
import Fisher.Tools.Registry (getToolDefinitions)

completion :: PyVal -> PyVal
completion msg =
  PDict
    [ ("id", PStr "chatcmpl-1"), ("object", PStr "chat.completion"), ("model", PStr "anthropic/claude-opus-4.6")
    , ("choices", PList [PDict [("index", PInt 0), ("message", msg), ("finish_reason", PStr (if get "tool_calls" msg == Nothing then "stop" else "tool_calls"))]])
    , ("usage", PDict [("prompt_tokens", PInt 100), ("completion_tokens", PInt 10), ("total_tokens", PInt 110)]) ]

answers :: [PyVal]
answers =
  map completion
    [ PDict
        [ ("role", PStr "assistant"), ("content", PStr "Reading.")
        , ("tool_calls", PList [PDict [("id", PStr "call_1"), ("type", PStr "function"), ("function", PDict [("name", PStr "read_file"), ("arguments", PStr "{\"path\": \"Math/Foo.lean\"}")])]]) ]
    , PDict [("role", PStr "assistant"), ("content", PStr "It builds.")] ]

data World = World
  { wSys :: Sys
  , wDeps :: ChatDeps
  , wOut :: IO Text
  , wReqs :: IO [HttpReq]
  , wFisher :: FilePath
  }

-- | A scripted world under @tmp@: its Sys's @which@ searches the PATH in
-- its environment (this machine's python3 unless the environment says
-- otherwise), the model answers 'answers' in order, the tools are scripted.
world :: FilePath -> [(Text, Text)] -> IO World
world tmp extraEnv = do
  let home = tmp </> "home"
      fisher = home </> ".fisher"
      work = tmp </> "work"
  createDirectoryIfMissing True fisher
  createDirectoryIfMissing True work
  py <- findExecutable "python3" >>= maybe (fail "no python3 on this machine") (pure . takeDirectory)
  offset <- newIORef (0 :: Double)
  uuidN <- newIORef (0 :: Int)
  outBuf <- newIORef []
  reqs <- newIORef []
  queue <- newIORef answers
  env <-
    newIORef . M.fromList $
      [ ("HOME", T.pack home), ("COLUMNS", "60"), ("FISHER_FORCE_PLAIN_OUTPUT", "1"), ("FISHER_TIMEZONE", "UTC")
      , ("OPENROUTER_API_KEY", "sk-or-test-1234567890"), ("PATH", T.pack py) ]
        <> extraEnv
  let clock = (1790000000 +) <$> readIORef offset
      sleep d = modifyIORef offset (+ d)
      which cmd = do
        p <- maybe "" T.unpack . M.lookup "PATH" <$> readIORef env
        found <- findExecutablesInDirectories (splitSearchPath p) (T.unpack cmd)
        pure (case found of (x : _) -> Just x; [] -> Nothing)
      sys =
        Sys
          { sysNow = clock, sysMonotonic = (1000 +) <$> readIORef offset, sysSleep = sleep
          , sysGetEnv = \k -> M.lookup k <$> readIORef env
          , sysSetEnv = \k v -> modifyIORef env (M.insert k v)
          , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
          , sysOpenBrowser = \_ -> pure False, sysRun = \_ -> pure (Left "no"), sysRunInteractive = \_ -> pure (Left "no")
          , sysWhich = which, sysPlatform = "linux"
          , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, T.justifyRight 32 '0' (T.pack (showHex (n + 1) ""))))
          , sysTimeZoneAt = \_ -> pure utc, sysHome = home, sysFisherHome = fisher
          , sysOut = \t -> modifyIORef outBuf (<> [t]), sysInput = \_ -> pure Nothing, sysGetPass = \_ -> pure Nothing
          , sysStdinTty = pure False, sysStdoutTty = pure False, sysPid = 4242 }
      respond hr = do
        modifyIORef reqs (<> [hr])
        if hrMethod hr == "GET"
          then pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii (PDict [("data", PList [PDict [("id", PStr "anthropic/claude-opus-4.6"), ("context_length", PInt 200000)]])])))))
          else do
            q <- readIORef queue
            case q of
              [] -> pure (Right (HttpResp 500 [] "{\"error\": {\"message\": \"no canned answer\"}}"))
              (r : rest) -> writeIORef queue rest >> pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii r))))
  at <- newAgentTools fisher
  (defs, _) <- getToolDefinitions (atRegistry at) (Just ["fisher-cli"]) Nothing
  let scripted tr = tr {trCall = \_ _ _ _ -> pure "{\"content\": \"     1|theorem foo : True := trivial\", \"total_lines\": 1}", trReadFilesSummary = \_ -> pure []}
      deps =
        ChatDeps
          { cdTransport = pure (Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError)))
          , cdTools = Just (defs, scripted)
          , cdNowLocal = pure (LocalTime (fromGregorian 2026 10 13) (TimeOfDay 10 15 0))
          , cdRuntime = Runtime clock sleep (pure 0)
          , cdChoice = \_ -> pure 0
          , cdAnimate = pure False
          , cdCwd = pure work
          , cdWhich = \_ -> pure Nothing
          , cdBannerInfo = pure ("<WORK>", Nothing, True) }
  pure (World sys deps (T.concat <$> readIORef outBuf) (readIORef reqs) fisher)

data Run = Run {rCode :: Int, rOut :: Text, rReqs :: [HttpReq], rFisher :: FilePath, rSys :: Sys}

runChat :: FilePath -> [(Text, Text)] -> [Text] -> IO Run
runChat tmp extraEnv argv = do
  w <- world tmp extraEnv
  code <- chatMain (wDeps w) (wSys w) argv
  o <- wOut w
  rs <- wReqs w
  pure (Run code o rs (wFisher w) (wSys w))

-- | The kinds of the events in a session's replay log.
logKinds :: FilePath -> Text -> IO [Text]
logKinds fisher sid = do
  lg <- BS.readFile (fisher </> "fisher" </> "replays" </> T.unpack (sid <> ".jsonl"))
  pure [maybe "" pyStrOf (get "kind" v) | l <- BC.lines lg, Right v <- [loads l]]

-- | The one session a run left a replay log for.
onlySession :: FilePath -> IO Text
onlySession fisher = do
  fs <- listDirectory (fisher </> "fisher" </> "replays")
  case fs of
    [f] -> pure (T.pack (dropExtension f))
    _ -> fail ("replay logs: " <> show fs)

-- | The messages of the first chat request a run sent.
firstMessages :: Run -> [PyVal]
firstMessages r = case [hr | hr <- rReqs r, hrMethod hr == "POST"] of
  (hr : _) | Just (PList ms) <- hrBody hr >>= get "messages" -> ms
  _ -> []

-- | The messages of the last chat request a run sent (its whole history).
firstMessagesOfLast :: Run -> [PyVal]
firstMessagesOfLast r = case reverse [hr | hr <- rReqs r, hrMethod hr == "POST"] of
  (hr : _) | Just (PList ms) <- hrBody hr >>= get "messages" -> ms
  _ -> []

textOf :: PyVal -> Text
textOf m = case get "content" m of
  Just (PStr t) -> t
  Just (PList ps) -> T.concat [t | p <- ps, Just (PStr t) <- [get "text" p]]
  _ -> ""

count :: Text -> Text -> Int
count needle hay = length (T.breakOnAll needle hay)

spec :: Spec
spec = do
  describe "fisher chat --context rlm" $ do
    it "runs the turn under RLM and leaves a log with the strategy that replays bit for bit" $ withSystemTempDirectory "fisher-rlm-cli" $ \tmp -> do
      r <- runChat tmp [] ["-q", "Check that the project builds.", "-Q", "--context", "rlm"]
      rCode r `shouldBe` 0
      rOut r `shouldSatisfy` T.isInfixOf "It builds."
      sid <- onlySession (rFisher r)
      kinds <- logKinds (rFisher r) sid
      kinds `shouldSatisfy` elem "rlm"
      replayMain (rSys r) [sid] `shouldReturn` 0
    it "is chosen by fisher.context: rlm in config.yaml" $ withSystemTempDirectory "fisher-rlm-cli" $ \tmp -> do
      createDirectoryIfMissing True (tmp </> "home" </> ".fisher")
      BS.writeFile (tmp </> "home" </> ".fisher" </> "config.yaml") "fisher:\n  context: rlm\n"
      r <- runChat tmp [] ["-q", "Check that the project builds.", "-Q"]
      rCode r `shouldBe` 0
      sid <- onlySession (rFisher r)
      logKinds (rFisher r) sid >>= (`shouldSatisfy` elem "rlm")
    it "is not chosen otherwise" $ withSystemTempDirectory "fisher-rlm-cli" $ \tmp -> do
      r <- runChat tmp [] ["-q", "Check that the project builds.", "-Q"]
      sid <- onlySession (rFisher r)
      logKinds (rFisher r) sid >>= (`shouldNotSatisfy` elem "rlm")
    it "rejects an unknown strategy, listing compress, clm and rlm" $ withSystemTempDirectory "fisher-rlm-cli" $ \tmp -> do
      r <- runChat tmp [] ["-q", "x", "--context", "bogus"]
      rCode r `shouldBe` 2
      rOut r `shouldSatisfy` T.isInfixOf "argument --context: invalid choice: 'bogus' (choose from 'compress', 'clm', 'rlm')"
      rOut r `shouldSatisfy` T.isInfixOf "[--context {compress,clm,rlm}]"
    it "is refused with phase 1's message, exit 2, when the session's PATH has no python3" $ withSystemTempDirectory "fisher-rlm-cli" $ \tmp -> do
      createDirectoryIfMissing True (tmp </> "empty")
      r <- runChat tmp [("PATH", T.pack (tmp </> "empty"))] ["-q", "x", "-Q", "--context", "rlm"]
      rCode r `shouldBe` 2
      T.unwords (T.words (rOut r)) `shouldSatisfy` T.isInfixOf "Failed to initialize agent: python3 not found: python3"
    it "runs on any terminal backend" $ withSystemTempDirectory "fisher-rlm-cli" $ \tmp -> do
      r <- runChat tmp [("TERMINAL_ENV", "docker")] ["-q", "Check that the project builds.", "-Q", "--context", "rlm"]
      rCode r `shouldBe` 0
      rOut r `shouldNotSatisfy` T.isInfixOf "Failed to initialize agent"
    it "a resume over an invalid rlm.json warns on stderr, not stdout, and carries on" $ withSystemTempDirectory "fisher-rlm-cli" $ \tmp -> do
      r1 <- runChat tmp [] ["-q", "Check that the project builds.", "-Q", "--context", "rlm"]
      rCode r1 `shouldBe` 0
      sid <- onlySession (rFisher r1)
      let dir = rFisher r1 </> "fisher" </> "ctx" </> T.unpack sid
      createDirectoryIfMissing True dir
      BS.writeFile (dir </> "rlm.json") "{\"store\": [oops"
      r2 <- runChat tmp [] ["-q", "Check again.", "-Q", "--context", "rlm", "--resume", sid]
      rCode r2 `shouldBe` 0
      -- the warning goes to stderr, so stdout (stream-json under -Q's callers) has none of it
      count "fisher: rlm.json is not valid; resuming with an empty store" (rOut r2) `shouldBe` 0
      rOut r2 `shouldSatisfy` T.isInfixOf "It builds."
  describe "resuming under rlm (state.db and rlm.json)" $
    it "rebuilds the history from the transcript: the stub, then the turns after the stored range" $ withSystemTempDirectory "fisher-rlm-cli" $ \tmp -> do
      r1 <- runChat tmp [] ["-q", "Check that the project builds.", "-Q", "--context", "rlm"]
      rCode r1 `shouldBe` 0
      sid <- onlySession (rFisher r1)
      -- the first three messages (the task, the call, its result) stored
      let firstThree = take 3 [m | m <- firstMessagesOfLast r1, get "role" m /= Just (PStr "system")]
          turn i m = StoredTurn i (maybe "" pyStrOf (get "role" m)) (if get "role" m == Just (PStr "tool") then Just "read_file" else Nothing) m
          st = RlmState (zipWith turn [1 ..] firstThree) 1 (Just (1, 3))
          dir = rFisher r1 </> "fisher" </> "ctx" </> T.unpack sid
      length firstThree `shouldBe` 3
      createDirectoryIfMissing True dir
      BS.writeFile (dir </> "rlm.json") (TE.encodeUtf8 (dumpsAscii (stateToPy st)))
      -- the interactive form: -Q runs the query without the session's history
      r2 <- runChat tmp [] ["-q", "Check again.", "--context", "rlm", "--resume", sid]
      rCode r2 `shouldBe` 0
      let ms = [m | m <- firstMessages r2, get "role" m /= Just (PStr "system")]
      -- the stub as sent (the provider's caching may split content into parts)
      map (T.isPrefixOf "Turns 1 to 3 of this session (3 messages, " . textOf) (take 1 ms) `shouldBe` [True]
      [m | m <- ms, get "tool_call_id" m == Just (PStr "call_1")] `shouldBe` []
      map textOf (drop 1 ms) `shouldBe` ["It builds.", "Check again."]
  describe "the shell's native session with --context rlm" $
    it "runs the turn under RLM" $ withSystemTempDirectory "fisher-rlm-shell" $ \tmp -> do
      w <- world tmp []
      ref <- newIORef Nothing
      let ui = ChatUi (\l -> sysOut (wSys w) (l <> "\n")) (sysOut (wSys w)) (\m -> sysOut (wSys w) (m <> "\n")) (pure 60) False
          sc = shellChat (wDeps w) (wSys w) ui defaultChatOptions {coContext = Just "rlm"} ref
      scTurn sc "20261013_101500_ab12cd" "Check that the project builds."
      _ <- scExit sc
      o <- wOut w
      o `shouldNotSatisfy` T.isInfixOf "Failed to initialize agent"
      logKinds (wFisher w) "20261013_101500_ab12cd" >>= (`shouldSatisfy` elem "rlm")
  describe "ACP and cron (buildNative)" $
    mapM_
      ( \strategy -> it ("warn once for fisher.context: " <> T.unpack strategy <> " and keep the compressor") $ withSystemTempDirectory "fisher-rlm-native" $ \tmp -> do
          w <- world tmp []
          BS.writeFile (wFisher w </> "config.yaml") (TE.encodeUtf8 ("fisher:\n  context: " <> strategy <> "\n"))
          logged <- newIORef []
          sh <- newNativeShared (wSys w) (wDeps w) (\_ -> pure ())
          let spec' = (nativeSpec "acp" "s1") {nsApiKey = "sk-or-test-1234567890", nsReplay = False, nsOut = \t -> modifyIORef logged (<> [t])}
          (ag, _) <- buildNative (wSys w) (wDeps w) sh spec'
          said <- T.concat <$> readIORef logged
          count ("fisher.context: " <> strategy <> " is not supported here yet; this session uses the compressor.\n") said `shouldBe` 1
          agentRlmState ag >>= (`shouldBe` Nothing) )
      ["clm", "rlm"]
