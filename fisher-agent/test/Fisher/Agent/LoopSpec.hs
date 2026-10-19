-- | 'Fisher.Agent.Loop' against @AIAgent.run_conversation@
-- (@test/golden/b3/loop.json@, from tools/corpus/b3_loop.py). Each case
-- builds the agent the way the CLI does and runs its turns with the HTTP
-- answers upstream got (in order, including the OpenRouter model list and
-- the auxiliary summary calls), the tool results upstream's tools gave,
-- the clock fixed and moved only by sleeps. Per turn the spec compares
-- every request, sleep, callback, tool call, the printed output, the
-- result, the session database rows and the session log.
module Fisher.Agent.LoopSpec (spec, scenarioConfig, runCaseWith, DbFactory, hasInterrupts) where

import Control.Monad (forM, forM_, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import Numeric (showHex)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Aux
import Fisher.Agent.AuxSpec (testSys)
import Fisher.Agent.Display (defaultSkin)
import Fisher.Agent.Loop
import Fisher.Agent.Metadata
import Fisher.Agent.Replay
import Fisher.Agent.Session
import Fisher.Agent.TestUtil
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered)
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Tools.Agent (AgentTools (..), newAgentTools)
import Fisher.Agent.Tools.Memory (memoryToolDef)
import Fisher.Agent.Tools.Todo (todoToolDef)
import Fisher.Tools.Registry (Toolset (..), checkToolsetRequirements, createCustomToolset, emojiFor, getToolDefinitions, register)

clock0 :: Double
clock0 = 1790000000

fixedTime :: LocalTime
fixedTime = LocalTime (fromGregorian 2026 10 2) (TimeOfDay 4 50 7)

keepHeaders :: [Text]
keepHeaders =
  [ "authorization", "content-type", "accept", "http-referer", "x-openrouter-title", "x-openrouter-categories"
  , "x-api-key", "anthropic-version", "anthropic-beta" ]

-- | The request as the corpus records it; the body's top-level keys in
-- name order (the SDK and Fisher put them in different orders).
reqVal :: HttpReq -> PyVal
reqVal hr =
  PDict
    [ ("method", PStr (hrMethod hr)), ("url", PStr (hrUrl hr))
    , ("headers", PDict (sortOn fst [(k, PStr v) | (k, v) <- hrHeaders hr, k `elem` keepHeaders]))
    , ("body", maybe PNull sortTop (hrBody hr)) ]

sortTop :: PyVal -> PyVal
sortTop = \case
  PDict kvs -> PDict (sortOn fst kvs)
  v -> v

goldenReq :: PyVal -> PyVal
goldenReq r =
  PDict
    [ ("method", field "method" r), ("url", field "url" r)
    , ("headers", case field "headers" r of PDict kvs -> PDict (sortOn fst kvs); v -> v)
    , ("body", sortTop (field "body" r)) ]

-- | What is left on each line once carriage returns have done their work.
normalizeOut :: Text -> Text
normalizeOut = T.intercalate "\n" . map (last . T.splitOn "\r") . T.splitOn "\n"

dropUnorderedLines :: Text -> Text
dropUnorderedLines = T.intercalate "\n" . filter (not . ("✅ Enabled toolset '" `T.isPrefixOf`)) . T.splitOn "\n"

uuidAt :: Int -> Text
uuidAt n = T.justifyRight 32 '0' (T.pack (showHex (toInteger n * 0x1F2E3D * 2 ^ (104 :: Int)) ""))

scenarioToolsets :: PyVal -> [Text]
scenarioToolsets sc = case get "toolsets" sc of
  Just (PList ts) -> [t | PStr t <- ts]
  _ -> ["gauss-cli"]

scenarioConfig :: PyVal -> Text -> AgentConfig
scenarioConfig sc _ =
  let baseUrl = case get "base_url" sc of
        Just (PStr b) -> b
        _ -> "https://openrouter.ai/api/v1"
      local = baseUrl == "http://127.0.0.1:8080/v1"
      routing = maybe False truthy (get "routing" sc)
   in defaultAgentConfig
        { cfgModel = maybe "anthropic/claude-opus-4.6" pyStrOf (get "model" sc)
        , cfgApiKey = if local then "sk-local-key-12345" else "sk-or-test-1234567890"
        , cfgBaseUrl = baseUrl
        , cfgBaseUrlGiven = True
        , cfgProvider = maybe "openrouter" pyStrOf (get "provider" sc)
        , cfgApiMode = ChatCompletions
        , cfgMaxIterations = case get "max_iterations" sc of
            Just (PInt n) -> fromIntegral n
            _ -> 90
        , cfgEnabledToolsets = Just (scenarioToolsets sc)
        , cfgVerbose = False
        , cfgQuiet = maybe True truthy (get "quiet" sc)
        , cfgEphemeral = case get "ephemeral" sc of
            Just (PStr e) -> Just e
            _ -> Nothing
        , cfgPrefill = case get "prefill" sc of
            Just (PList ps) -> ps
            _ -> []
        , cfgReasoning = case get "reasoning_config" sc of
            Just v | truthy v -> Just v
            _ -> Nothing
        , cfgRouting =
            if routing
              then Routing ["anthropic"] ["deepinfra"] [] (Just "throughput") True (Just "deny")
              else noRouting
        , cfgSessionId = Just "20261002_045007_abc123"
        , cfgPlatform = Just "cli"
        , cfgPassSessionId = maybe False truthy (get "pass_session_id" sc)
        , cfgFallback = case get "fallback" sc of
            Just f | Just (PStr p) <- get "provider" f, Just (PStr m) <- get "model" f -> Just (p, m)
            _ -> Nothing }

spec :: Spec
spec = do
  cases <- runIO (loadCases "test/golden/b3/loop.json")
  describe "AIAgent.run_conversation" $
    forM_ cases $ \c -> it (T.unpack (txt "name" c)) $ runCaseWith memoryDb Nothing c
  b4 <- runIO (loadCases "test/golden/b4/loop.json")
  describe "AIAgent.run_conversation: todo and memory" $
    forM_ b4 $ \c -> it (T.unpack (txt "name" c)) $ runCaseWith memoryDb Nothing c
  where
    memoryDb _ = do
      (db, mdb) <- memorySessionDB
      pure (db, memorySnapshot mdb)

-- | Makes the session database for a case (given the Fisher home): the
-- database and how to read its rows back.
type DbFactory = FilePath -> IO (SessionDB, IO PyVal)

-- | Whether a case interrupts the agent (a replay cannot: interrupts are
-- not in the log).
hasInterrupts :: PyVal -> Bool
hasInterrupts c =
  maybe False truthy (get "tool_hooks" (field "scenario" c))
    || or [has "interrupt" r | t <- list "turns" c, r <- list "requests" t]

runCaseWith :: DbFactory -> Maybe ReplayLog -> PyVal -> IO ()
runCaseWith mkDb rec c = withSystemTempDirectory "fisher-loop" $ \tmp0 -> do
  tmp <- canonicalizePath tmp0
  let sc = field "scenario" c
      home = tmp </> "home"
      gauss = home </> ".gauss"
      work = tmp </> "work"
  createDirectoryIfMissing True (gauss </> "memories")
  createDirectoryIfMissing True work
  case get "memories" sc of
    Just (PDict kvs) -> forM_ kvs $ \(n, v) -> BS.writeFile (gauss </> "memories" </> T.unpack n) (TE.encodeUtf8 (pyStrOf v))
    _ -> pure ()
  case get "config" sc of
    Just (PStr y) -> BS.writeFile (gauss </> "config.yaml") (TE.encodeUtf8 y)
    _ -> pure ()
  case get "fixture_extra" sc of
    Just (PDict kvs) -> forM_ kvs $ \(n, v) -> BS.writeFile (work </> T.unpack n) (TE.encodeUtf8 (pyStrOf v))
    _ -> pure ()
  offset <- newIORef (0 :: Double)
  sleeps <- newIORef []
  uuidN <- newIORef (0 :: Int)
  outBuf <- newIORef []
  reqs <- newIORef []
  queue <- newIORef []
  events <- newIORef []
  toolsSeen <- newIORef []
  toolQueue <- newIORef []
  agentRef <- newIORef Nothing
  readHistory <- newIORef []
  let envVars =
        [("HOME", T.pack home), ("GAUSS_TIMEZONE", "UTC")]
          <> [("OPENROUTER_API_KEY", "sk-or-test-1234567890") | not (maybe False truthy (get "no_aux_key" sc))]
      base = testSys home envVars (\t -> modifyIORef outBuf (<> [t]))
      clock = (clock0 +) <$> readIORef offset
      sleep d = modifyIORef sleeps (<> [d]) >> modifyIORef offset (+ d)
      sys0 =
        base
          { sysNow = clock
          , sysSleep = sleep
          , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, uuidAt (n + 1))) }
      rt = Runtime clock sleep (pure 0)
      modelsAnswer = field "models_answer" (field "init" c)
      respond hr = do
        modifyIORef reqs (<> [hr])
        if hrMethod hr == "GET"
          then pure $ case modelsAnswer of
            PNull -> Left ConnectionError
            v -> Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii v)))
          else do
            q <- readIORef queue
            case q of
              [] -> pure (Right (HttpResp 500 [] "{\"error\": {\"message\": \"no canned answer\"}}"))
              (r : rest) -> do
                writeIORef queue rest
                case get "interrupt" r of
                  Just m -> do
                    ag <- readIORef agentRef
                    forM_ ag $ \a -> interruptAgent a (case m of PStr t | not (T.null t) -> Just t; _ -> Nothing)
                  Nothing -> pure ()
                let resp = field "response" r
                pure $
                  if maybe False truthy (get "connect_error" resp)
                    then Left ConnectionError
                    else
                      Right
                        ( HttpResp
                            (fromIntegral (int "status" resp))
                            (case field "headers" resp of PDict kvs -> [(k, pyStrOf v) | (k, v) <- kvs]; _ -> [])
                            (TE.encodeUtf8 (dumpsAscii (field "body" resp))) )
      tr0 = Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError))
      tr = maybe tr0 (`recordTransport` tr0) rec
      sys = maybe sys0 (`recordUuids` sys0) rec
  at <- newAgentTools gauss
  registerOfflineTools (atRegistry at) gauss
  -- todo and memory, as a process's second agent sees them
  register (atRegistry at) todoToolDef
  register (atRegistry at) memoryToolDef
  case get "custom_toolsets" sc of
    Just (PDict kvs) -> forM_ kvs $ \(n, ts) -> createCustomToolset (atRegistry at) n (Toolset "B4 loop scenarios" [t | PStr t <- asL ts] [])
    _ -> pure ()
  (defs, printed) <- getToolDefinitions (atRegistry at) (Just (scenarioToolsets sc)) Nothing
  emojis <- forM ["read_file", "write_file", "patch", "search_files", "todo", "memory"] $ \n -> (,) n <$> emojiFor (atRegistry at) n
  let hooks = case get "tool_hooks" sc of
        Just (PDict kvs) -> [(read (T.unpack k) :: Int, v) | (k, v) <- kvs]
        _ -> []
      runner0 =
        ToolRunner
          { trDefinitions = defs
          , trPrinted = printed
          , trCall = \name args task _ -> do
              modifyIORef toolsSeen (<> [PDict [("name", PStr name), ("args", args), ("task_id", PStr task)]])
              when (name == "read_file") $ do
                let path = maybe "" pyStrOf (get "path" args)
                    off = case get "offset" args of Just (PInt o) -> o; _ -> 1
                    lim = case get "limit" args of Just (PInt l) -> l; _ -> 500
                modifyIORef readHistory (\h -> if (path, off, lim) `elem` h then h else h <> [(path, off, lim)])
              q <- readIORef toolQueue
              case break (\t -> field "name" t == PStr name && field "args" t == args) q of
                (_, []) -> pure "{\"error\": \"no recorded result\"}"
                (pre, t : post) -> do
                  writeIORef toolQueue (pre <> post)
                  k <- length <$> readIORef toolsSeen
                  forM_ (lookup k hooks) $ \m -> do
                    ag <- readIORef agentRef
                    forM_ ag $ \a -> interruptAgent a (case m of PStr x | not (T.null x) -> Just x; _ -> Nothing)
                  pure (txt "result" t)
          , trEmoji = \n -> lookup n emojis
          , trReadFilesSummary = \_ -> do
              h <- readIORef readHistory
              let paths = sortOn id (foldr (\(p, _, _) acc -> if p `elem` acc then acc else p : acc) [] h)
              pure [(p, sortOn id ["lines " <> T.pack (show o) <> "-" <> T.pack (show (o + l - 1)) | (p', o, l) <- h, p' == p]) | p <- paths]
          , trCleanup = \_ -> pure ()
          , trSetInterrupt = \_ -> pure ()
          , trRequirements = withUnportedToolsets <$> checkToolsetRequirements (atRegistry at) }
      runner = maybe runner0 (`recordToolRunner` runner0) rec
  (db, snapshotDb) <- mkDb gauss
  cache <- newModelCache
  auxState <- newAuxState
  let cfgLoad = loadConfigOrdered sys
  config <- cfgLoad
  let auxEnv = AuxEnv sys rt tr cfgLoad (\_ -> pure ()) auxState
      env =
        AgentEnv
          { envSys = sys, envRuntime = rt, envTransport = tr
          , envLlm = Env (sysUuidHex sys) (\_ r -> pure ("[The " <> r <> " attached an image.]"))
          , envNowLocal = pure fixedTime, envPromptNow = pure fixedTime, envCwd = pure work
          , envChoice = \_ -> pure 0, envAnimate = False, envDb = Just db, envTools = runner
          , envAux = callLlm auxEnv, envAuxEnv = Just auxEnv
          , envMeta = MetaEnv tr clock gauss cache (\_ -> pure ())
          , envSkin = defaultSkin, envConfig = config
          , envCallbacks =
              Callbacks
                { cbThinking = Just (\t -> modifyIORef events (<> [PList [PStr "thinking", PStr t]]))
                , cbToolProgress = Just (\n p a -> modifyIORef events (<> [PList [PStr "progress", PStr n, fromMaybe PNull p, a]]))
                , cbStep = Nothing, cbToolDone = Nothing }
          , envWarn = \_ -> pure ()
          , envPromptHook = maybe pure recordPromptHook rec
          , envWriteFiles = True, envLocalTool = maybe (\_ _ act -> act) recordLocalTool rec, envInbox = pure [] }
      cfg = scenarioConfig sc ""
      mask t = T.replace (T.pack gauss) "<GAUSS_HOME>" (T.replace (T.pack work) "<WORK>" t)
      takeOut = do
        o <- T.concat <$> readIORef outBuf
        writeIORef outBuf []
        pure (mask o)
      takeReqs = do
        r <- readIORef reqs
        writeIORef reqs []
        pure (PList (map reqVal r))
  agent <- newAgent cfg env
  forM_ rec $ \l -> snapshotEnv sys >>= logAgent l cfg runner
  writeIORef agentRef (Just agent)
  case get "title" sc of
    Just (PStr t) -> dbSetSessionTitle db "20261002_045007_abc123" t
    _ -> pure ()
  let ini = field "init" c
  initOut <- takeOut
  dropUnorderedLines (normalizeOut initOut) `shouldBe` dropUnorderedLines (normalizeOut (txt "stdout" ini))
  initReqs <- takeReqs
  shouldMatch "init requests" initReqs (PList (map goldenReq (list "requests" ini)))
  forM_ (zip [1 :: Int ..] (list "turns" c)) $ \(ti, turn) -> do
    writeIORef queue (list "requests" turn)
    writeIORef toolQueue (list "tools" turn)
    writeIORef toolsSeen []
    writeIORef events []
    writeIORef sleeps []
    let ri = (runInput (txt "user" turn)) {riHistory = list "history" turn, riTaskId = Just "20261002_045007_abc123"}
    forM_ rec (`logTurn` ri)
    res <- runConversation agent ri
    forM_ rec (`logOutcome` res)
    let label s' = "turn " <> show ti <> " " <> s'
    o <- takeOut
    rs <- takeReqs
    shouldMatch (label "stdout") (PList (map PStr (T.splitOn "\n" (normalizeOut o)))) (PList (map PStr (T.splitOn "\n" (normalizeOut (txt "stdout" turn)))))
    shouldMatch (label "requests") rs (PList (map goldenReq (list "requests" turn)))
    sl <- readIORef sleeps
    (label "sleeps", PList (map PFloat sl)) `shouldBe` (label "sleeps", PList [PFloat (num d) | PList [_, d] <- list "sleeps" turn])
    ev <- readIORef events
    shouldMatch (label "events") (PList ev) (field "events" turn)
    ts <- readIORef toolsSeen
    shouldMatch (label "tools") (PList (sortOn dumpsAscii ts)) (PList $ sortOn dumpsAscii [PDict [("name", field "name" t), ("args", field "args" t), ("task_id", field "task_id" t)] | t <- list "tools" turn])
    case res of
      RunDone r -> shouldMatch (label "result") r (field "result" turn)
      RunRaised cls m -> (label "raises", PDict [("type", PStr cls), ("message", PStr m)]) `shouldBe` (label "raises", field "raises" turn)
    snap <- snapshotDb
    shouldMatch (label "db") snap (field "db" turn)
    sid <- agentSessionId agent
    (label "session id", PStr sid) `shouldBe` (label "session id", field "session_id_after" turn)
    let logPath = gauss </> "sessions" </> "session_20261002_045007_abc123.json"
    exists <- doesFileExist logPath
    logV <- if exists then either (const PNull) id . loads <$> BS.readFile logPath else pure PNull
    shouldMatch (label "log") logV (field "log" turn)
    when (has "memories" turn) $ do
      let memDir = gauss </> "memories"
      names <- sortOn id . filter (".md" `T.isSuffixOf`) . map T.pack <$> listDirectory memDir
      files <- forM names $ \n -> (,) n . PStr . TE.decodeUtf8 <$> BS.readFile (memDir </> T.unpack n)
      shouldMatch (label "memories") (PDict files) (field "memories" turn)
  where
    num = \case
      PFloat d -> d
      PInt n -> fromIntegral n
      _ -> 0
    asL = \case
      PList xs -> xs
      _ -> []
