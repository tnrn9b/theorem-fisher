-- | Replay logs (Fisher only): every agent session the CLI runs is
-- written as one JSON line per event to
-- @<Fisher home>/fisher/replays/<session>.jsonl@ (the agent's settings,
-- each system prompt, each HTTP exchange, each tool call, each id drawn,
-- each turn and its result), and 'replayFile' runs the turns again with
-- the recorded answers, checking that every request Fisher sends is the
-- recorded one byte for byte.
--
-- Keys and tokens are masked in the log (first 8 and last 4 characters).
module Fisher.Agent.Replay
  ( ReplayLog
  , openReplayLog
  , closeReplayLog
  , replayDir
  , replayPath
  , logEvent
  , recordTransport
  , recordToolRunner
  , recordLocalTool
  , recordInbox
  , recordPromptHook
  , recordUuids
  , logAgent
  , snapshotEnv
  , replayEnvNames
  , logTurn
  , logOutcome
  , configToPy
  , configFromPy
  , ReplayReport (..)
  , ReplayBase (..)
  , replayFile
  , replayFileWith
  , requestRecord
  , recordMirror
  , logClm
  , recordClm
  , spillRecord
  , archiveRecord
  , recordRlm
  , logRlm
  , rlmConfigToPy
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (tails)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (LocalTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, (</>))
import Numeric (readHex, showHex)
import System.IO (Handle, IOMode (AppendMode), hClose, hFlush, openFile)

import Fisher.Agent.Aux (AuxEnv (..), callLlm, newAuxState)
import Fisher.Agent.Context.Clm
import Fisher.Agent.Context.ClmStore (Item (..), itemBody, storeFromPy, storeToPy)
import Fisher.Agent.Context.Recall (RecallResult (..))
import Fisher.Agent.Context.Rlm (Offload (..), RlmState (..), StoredTurn (..), isStub, renderTurn)
import qualified Fisher.Agent.Context.Rlm as Rlm
import Fisher.Agent.Display (defaultSkin)
import Fisher.Agent.Loop
import Fisher.Agent.Metadata (MetaEnv (..), newModelCache)
import Fisher.Agent.Session (memorySessionDB)
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys (Sys (..))
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..), defaultRlmConfig, rlmConfigFromPy)
import Fisher.RLM.Repl (ReplFactory, pythonRepl)
import qualified Fisher.RLM.Types as RT

-- | An open log; writes are serialised.
data ReplayLog = ReplayLog (MVar Handle) FilePath

replayDir :: FilePath -> FilePath
replayDir fisherHome = fisherHome </> "fisher" </> "replays"

replayPath :: FilePath -> Text -> FilePath
replayPath fisherHome sid = replayDir fisherHome </> T.unpack (sid <> ".jsonl")

openReplayLog :: FilePath -> Text -> IO ReplayLog
openReplayLog fisherHome sid = do
  createDirectoryIfMissing True (replayDir fisherHome)
  let p = replayPath fisherHome sid
  h <- openFile p AppendMode
  m <- newMVar h
  pure (ReplayLog m p)

closeReplayLog :: ReplayLog -> IO ()
closeReplayLog (ReplayLog m _) = withMVar m hClose

logEvent :: ReplayLog -> Text -> [(Text, PyVal)] -> IO ()
logEvent (ReplayLog m _) kind kvs =
  withMVar m $ \h -> do
    BS.hPut h (TE.encodeUtf8 (dumpsAscii (PDict (("kind", PStr kind) : kvs))) <> "\n")
    hFlush h

mask :: Text -> Text
mask v
  | "Bearer " `T.isPrefixOf` v = "Bearer " <> maskKey (T.drop 7 v)
  | otherwise = maskKey v
  where
    maskKey k = if T.length k <= 12 then "***" else T.take 8 k <> "..." <> T.takeEnd 4 k

-- | A request as the log keeps it: secrets masked, the body as the JSON
-- text Fisher sends.
requestRecord :: HttpReq -> PyVal
requestRecord hr =
  PDict
    [ ("method", PStr (hrMethod hr)), ("url", PStr (hrUrl hr))
    , ("headers", PList [PList [PStr k, PStr (if k `elem` ["authorization", "x-api-key"] then mask v else v)] | (k, v) <- hrHeaders hr])
    , ("body", maybe PNull (PStr . dumpsUnicode) (hrBody hr)), ("stream", PBool (hrStream hr)) ]

errorRecord :: LLMError -> PyVal
errorRecord = \case
  ConnectionError -> PDict [("error", PStr "ConnectionError")]
  TimeoutError -> PDict [("error", PStr "TimeoutError")]
  StatusError c m b -> PDict [("error", PStr "StatusError"), ("status", PInt (fromIntegral c)), ("message", PStr m), ("body", b)]
  ApiError m -> PDict [("error", PStr "ApiError"), ("message", PStr m)]
  LocalError c m -> PDict [("error", PStr "LocalError"), ("class", PStr c), ("message", PStr m)]

errorFromRecord :: PyVal -> LLMError
errorFromRecord r = case get "error" r of
  Just (PStr "TimeoutError") -> TimeoutError
  Just (PStr "StatusError") -> StatusError (maybe 0 (\case PInt n -> fromIntegral n; _ -> 0) (get "status" r)) (str' "message") (fromMaybe PNull (get "body" r))
  Just (PStr "ApiError") -> ApiError (str' "message")
  Just (PStr "LocalError") -> LocalError (str' "class") (str' "message")
  _ -> ConnectionError
  where
    str' k = maybe "" pyStrOf (get k r)

decodeT :: BS.ByteString -> Text
decodeT = TE.decodeUtf8With TEE.lenientDecode

responseRecord :: HttpResp -> PyVal
responseRecord r =
  PDict [("status", PInt (fromIntegral (rStatus r))), ("headers", PList [PList [PStr k, PStr v] | (k, v) <- rHeaders r]), ("body", PStr (decodeT (rBody r)))]

-- | Every exchange through the transport goes to the log.
recordTransport :: ReplayLog -> Transport -> Transport
recordTransport lg tr =
  Transport
    { tRequest = \hr to -> do
        r <- tRequest tr hr to
        logEvent lg "http" [("request", requestRecord hr), ("response", either errorRecord responseRecord r)]
        pure r
    , tStream = \hr to k -> do
        r <- tStream tr hr to $ \status hs reader -> do
          acc <- newIORef []
          let reader' = do
                c <- reader
                modifyIORef acc (c :)
                pure c
          a <- k status hs reader'
          -- whatever the consumer did not read is drained, so the log holds the whole body
          let drain = do
                c <- reader
                if BS.null c then pure () else modifyIORef acc (c :) >> drain
          drain
          body <- BS.concat . reverse <$> readIORef acc
          pure (a, HttpResp status hs body)
        case r of
          Left e -> logEvent lg "http" [("request", requestRecord hr), ("response", errorRecord e)] >> pure (Left e)
          Right (a, resp) -> logEvent lg "http" [("request", requestRecord hr), ("response", responseRecord resp)] >> pure (Right a)
    }

recordToolRunner :: ReplayLog -> ToolRunner -> ToolRunner
recordToolRunner lg tr =
  tr
    { trCall = \name args task enabled -> do
        r <- try @SomeException (trCall tr name args task enabled)
        case r of
          Right res -> logEvent lg "tool" [("name", PStr name), ("args", args), ("task_id", PStr task), ("result", PStr res)] >> pure res
          Left e -> logEvent lg "tool" [("name", PStr name), ("args", args), ("task_id", PStr task), ("raised", PStr (T.pack (show e)))] >> ioError (userError (show e))
    , trReadFilesSummary = \task -> do
        r <- trReadFilesSummary tr task
        logEvent lg "read_files" [("files", PList [PList [PStr p, PList (map PStr rs)] | (p, rs) <- r])]
        pure r
    }

-- | The tools the loop runs itself (todo, memory) go to the log too.
recordLocalTool :: ReplayLog -> Text -> PyVal -> IO Text -> IO Text
recordLocalTool lg name args act = do
  r <- act
  logEvent lg "local_tool" [("name", PStr name), ("args", args), ("result", PStr r)]
  pure r

-- | The inbox (Fisher UI): each message taken goes to the log as an
-- @inbox@ event with its text and the tool boundary it was taken at
-- (1 for this process's first), where a replay hands it back.
recordInbox :: ReplayLog -> IO [Text] -> IO (IO [Text])
recordInbox lg take' = do
  boundary <- newIORef (0 :: Int)
  pure $ do
    k <- atomicModifyIORef' boundary (\x -> (x + 1, x + 1))
    ts <- take'
    forM_ ts $ \t -> logEvent lg "inbox" [("boundary", PInt (fromIntegral k)), ("text", PStr t)]
    pure ts

-- | CLM's context file: every write and every read-back goes to the log
-- (docs/clm.md section 9).
recordMirror :: ReplayLog -> MirrorIO -> MirrorIO
recordMirror lg io =
  io
    { mioWrite = \t -> mioWrite io t >> logEvent lg "clm_write" [("text", PStr t)]
    , mioRead = do
        r <- mioRead io
        logEvent lg "clm_read" [("file", fileRecord r)]
        pure r }

fileRecord :: Maybe BS.ByteString -> PyVal
fileRecord = \case
  Nothing -> PNull
  Just bs -> case TE.decodeUtf8' bs of
    Right t -> PStr t
    Left _ -> PDict [("bytes_hex", PStr (T.pack (concatMap hex2 (BS.unpack bs))))]
  where
    hex2 w = let h = showHex w "" in if length h < 2 then '0' : h else h

fileFromRecord :: PyVal -> Maybe BS.ByteString
fileFromRecord = \case
  PStr t -> Just (TE.encodeUtf8 t)
  d@(PDict _) -> Just (BS.pack (unhex (T.unpack (maybe "" pyStrOf (get "bytes_hex" d)))))
  _ -> Nothing
  where
    unhex (a : b : rest) = fromIntegral (fst (head (readHex [a, b])) :: Int) : unhex rest
    unhex _ = []

-- | The CLM strategy an agent runs with, logged after its agent record.
-- With the store on, also the store it loaded (@store@, null for none) and
-- recall's configuration (@recall@, absent when recall is off); with it
-- off the record is as before the store.
logClm :: ReplayLog -> ClmSetup -> Maybe ClmState -> IO ()
logClm lg setup st = do
  let on = ccStore (csConfig setup)
  loaded <- if on then csStoreLoad setup else pure Nothing
  logEvent lg "clm" $
    [ ("config", clmConfigToPy (csConfig setup)), ("path", PStr (mioPath (csIO setup)))
    , ("session", PStr (csSessionId setup)), ("state", maybe PNull stateToPy st) ]
      <> [("store", maybe PNull storeToPy loaded) | on]
      <> [("recall", rlmConfigToPy rc) | on, Just (rc, _) <- [csRecall setup]]

-- | CLM's store: each spill, each archive step and each recall goes to the
-- log (spec section 4; a recall as RLM's, as @clm_recall@).
recordClm :: ReplayLog -> ClmSetup -> ClmSetup
recordClm lg s =
  s
    { csOnSpill = \i -> csOnSpill s i >> logEvent lg "clm_spill" (spillRecord i)
    , csOnArchive = \is w -> csOnArchive s is w >> logEvent lg "clm_archive" (archiveRecord is w)
    , csOnRecall = \q r -> csOnRecall s q r >> logEvent lg "clm_recall" (recallRecord q r) }

-- | A spill as the log keeps it: the item's id, the CLM turn its preview
-- is (null when the batch rolled back) and the characters stored (the
-- item's text as read_stored reads it).
spillRecord :: Item -> [(Text, PyVal)]
spillRecord i = [("item", PInt (fromIntegral (itId i))), ("turn", turnRecord i), ("chars", PInt (fromIntegral (itemChars i)))]

-- | An archive step as the log keeps it: the items' ids and CLM turns (null
-- for a message that never became a turn), why (deleted, rewritten,
-- shortened, cut or rolled_back) and the characters stored.
archiveRecord :: [Item] -> Text -> [(Text, PyVal)]
archiveRecord is w =
  [ ("items", PList [PInt (fromIntegral (itId i)) | i <- is]), ("turns", PList (map turnRecord is))
  , ("why", PStr w), ("chars", PInt (fromIntegral (sum (map itemChars is)))) ]

turnRecord :: Item -> PyVal
turnRecord = maybe PNull (PInt . fromIntegral) . itTurn

itemChars :: Item -> Int
itemChars = T.length . itemBody

-- | The RLM strategy's offloads, tail cuts and recalls go to the log (spec
-- phase 2, section 5).
recordRlm :: ReplayLog -> RlmSetup -> RlmSetup
recordRlm lg s =
  s
    { rlOnOffload = \o -> rlOnOffload s o >> logEvent lg "rlm_offload" (offloadRecord o)
    , rlOnCut = \ts -> rlOnCut s ts >> logEvent lg "rlm_cut" (cutRecord ts)
    , rlOnRecall = \q r -> rlOnRecall s q r >> logEvent lg "rlm_recall" (recallRecord q r) }

-- | An offload as the log keeps it: the range moved, the stub's text, the
-- characters stored by this offload (its new turns as recall reads them;
-- a turn a tail cut stored before is counted by that cut) and the turns in
-- the store after it.
offloadRecord :: Offload -> [(Text, PyVal)]
offloadRecord o =
  let (a, b) = ofMoved o
      store = rsStore (ofState o)
      stub = case [m | m <- ofMessages o, isStub m] of
        (m : _) -> maybe "" pyStrOf (get "content" m)
        [] -> ""
   in [ ("moved", PList [PInt (fromIntegral a), PInt (fromIntegral b)]), ("stub", PStr stub)
      , ("chars", PInt (fromIntegral (storedChars (ofNew o))))
      , ("store_turns", PInt (fromIntegral (length store))) ]

-- | A tail cut as the log keeps it: the session indices it stored and their
-- characters as recall reads them.
cutRecord :: [StoredTurn] -> [(Text, PyVal)]
cutRecord ts =
  [ ("turns", PList [PInt (fromIntegral (stIndex t)) | t <- ts])
  , ("chars", PInt (fromIntegral (storedChars ts))) ]

storedChars :: [StoredTurn] -> Int
storedChars = sum . map (T.length . renderTurn)

recallRecord :: Text -> RecallResult -> [(Text, PyVal)]
recallRecord q r =
  [ ("question", PStr q), ("answer", PStr (rcText r))
  , ( "ending"
    , PStr $ case rcEnding r of
        RT.Answered -> "answered"
        RT.DefaultAnswer -> "default"
        RT.Stopped why -> "stopped: " <> why )
  , ("iterations", PInt (fromIntegral (rcIterations r))), ("calls", PInt (fromIntegral (rcCalls r)))
  , ("dollars", PFloat (rcDollars r)) ]

-- | The recall configuration in @fisher.rlm@'s own keys, unset limits
-- left out, so 'rlmConfigFromPy' reads it back.
rlmConfigToPy :: RlmConfig -> PyVal
rlmConfigToPy c =
  PDict $
    [ ("max_depth", PInt (fromIntegral (rcMaxDepth c))), ("max_iterations", PInt (fromIntegral (rcMaxIterations c)))
    , ("max_concurrent_subcalls", PInt (fromIntegral (rcMaxConcurrent c))), ("truncate", PInt (fromIntegral (rcTruncate c)))
    , ("block_timeout", PInt (fromIntegral (rcBlockTimeout c))) ]
      <> [("max_budget", PFloat x) | Just x <- [rcMaxBudget c]]
      <> [("max_timeout", PFloat x) | Just x <- [rcMaxTimeout c]]
      <> [("max_tokens", PInt x) | Just x <- [rcMaxTokens c]]
      <> [("max_errors", PInt (fromIntegral x)) | Just x <- [rcMaxErrors c]]
      <> [("model", PStr (rcModel c))]
      <> [("sub_model", PStr m) | Just m <- [rcSubModel c]]
      <> [("min_offload", PFloat (rcMinOffload c)), ("offload_gap", PInt (fromIntegral (rcOffloadGap c)))]

-- | A log recorded before @min_offload@ and @offload_gap@ existed offloaded
-- with no gap and no size test: replay it that way, a key at a time.
oldOffloads :: Maybe PyVal -> RlmConfig -> RlmConfig
oldOffloads logged c =
  c
    { rcMinOffload = if given "min_offload" then rcMinOffload c else 0
    , rcOffloadGap = if given "offload_gap" then rcOffloadGap c else 0 }
  where
    given k = isJust (logged >>= get k)

-- | The RLM strategy an agent runs with, logged after its agent record: the
-- recall configuration, the session (the log's own name) and the state it
-- resumed from.
logRlm :: ReplayLog -> RlmSetup -> Maybe RlmState -> IO ()
logRlm lg@(ReplayLog _ path) setup st =
  logEvent lg "rlm"
    [ ("config", rlmConfigToPy (rlConfig setup)), ("session", PStr (T.pack (takeBaseName path)))
    , ("state", maybe PNull Rlm.stateToPy st) ]

recordPromptHook :: ReplayLog -> Text -> IO Text
recordPromptHook lg p = logEvent lg "prompt" [("text", PStr p)] >> pure p

recordUuids :: ReplayLog -> Sys -> Sys
recordUuids lg sys = sys {sysUuidHex = sysUuidHex sys >>= \h -> logEvent lg "uuid" [("hex", PStr h)] >> pure h}

configToPy :: AgentConfig -> PyVal
configToPy c =
  PDict
    [ ("model", PStr (cfgModel c)), ("api_key", PStr (mask (cfgApiKey c))), ("base_url", PStr (cfgBaseUrl c))
    , ("base_url_given", PBool (cfgBaseUrlGiven c)), ("provider", PStr (cfgProvider c)), ("api_mode", PStr (apiModeName (cfgApiMode c)))
    , ("max_iterations", PInt (fromIntegral (cfgMaxIterations c))), ("tool_delay", PFloat (cfgToolDelay c))
    , ("enabled_toolsets", maybe PNull (PList . map PStr) (cfgEnabledToolsets c))
    , ("disabled_toolsets", maybe PNull (PList . map PStr) (cfgDisabledToolsets c))
    , ("verbose", PBool (cfgVerbose c)), ("quiet", PBool (cfgQuiet c)), ("ephemeral", maybe PNull PStr (cfgEphemeral c))
    , ("prefill", PList (cfgPrefill c)), ("reasoning", fromMaybe PNull (cfgReasoning c)), ("max_tokens", maybe PNull PInt (cfgMaxTokens c))
    , ( "routing"
      , let r = cfgRouting c
         in PDict
              [ ("only", PList (map PStr (rOnly r))), ("ignore", PList (map PStr (rIgnore r))), ("order", PList (map PStr (rOrder r)))
              , ("sort", maybe PNull PStr (rSort r)), ("require_parameters", PBool (rRequireParameters r))
              , ("data_collection", maybe PNull PStr (rDataCollection r)) ] )
    , ("session_id", maybe PNull PStr (cfgSessionId c)), ("platform", maybe PNull PStr (cfgPlatform c))
    , ("pass_session_id", PBool (cfgPassSessionId c)), ("skip_context_files", PBool (cfgSkipContextFiles c))
    , ("skip_memory", PBool (cfgSkipMemory c))
    , ("fallback", maybe PNull (\(p, m) -> PList [PStr p, PStr m]) (cfgFallback c))
    , ("log_prefix", PStr (cfgLogPrefix c)), ("log_prefix_chars", PInt (fromIntegral (cfgLogPrefixChars c))) ]

-- | The settings back, with the key the caller supplies (the log keeps
-- only a masked one).
configFromPy :: Text -> PyVal -> AgentConfig
configFromPy key v =
  AgentConfig
    { cfgModel = t "model", cfgApiKey = key, cfgBaseUrl = t "base_url", cfgBaseUrlGiven = b "base_url_given"
    , cfgProvider = t "provider", cfgApiMode = fromMaybe ChatCompletions (parseApiMode (t "api_mode"))
    , cfgMaxIterations = fromIntegral (i "max_iterations"), cfgToolDelay = case get "tool_delay" v of Just (PFloat d) -> d; Just (PInt n) -> fromIntegral n; _ -> 1
    , cfgEnabledToolsets = ts "enabled_toolsets", cfgDisabledToolsets = ts "disabled_toolsets"
    , cfgVerbose = b "verbose", cfgQuiet = b "quiet", cfgEphemeral = mt "ephemeral"
    , cfgPrefill = case get "prefill" v of Just (PList xs) -> xs; _ -> []
    , cfgReasoning = case get "reasoning" v of Just PNull -> Nothing; x -> x
    , cfgMaxTokens = case get "max_tokens" v of Just (PInt n) -> Just n; _ -> Nothing
    , cfgRouting =
        let r = fromMaybe (PDict []) (get "routing" v)
            l k = case get k r of Just (PList xs) -> [s | PStr s <- xs]; _ -> []
            m k = case get k r of Just (PStr s) -> Just s; _ -> Nothing
         in Routing (l "only") (l "ignore") (l "order") (m "sort") (get "require_parameters" r == Just (PBool True)) (m "data_collection")
    , cfgSessionId = mt "session_id", cfgPlatform = mt "platform", cfgPassSessionId = b "pass_session_id"
    , cfgSkipContextFiles = b "skip_context_files", cfgSkipMemory = b "skip_memory"
    , cfgFallback = case get "fallback" v of Just (PList [PStr p, PStr m']) -> Just (p, m'); _ -> Nothing
    , cfgLogPrefix = t "log_prefix", cfgLogPrefixChars = fromIntegral (i "log_prefix_chars") }
  where
    t k = maybe "" pyStrOf (get k v)
    mt k = case get k v of Just (PStr s) -> Just s; _ -> Nothing
    b k = get k v == Just (PBool True)
    i k = case get k v of Just (PInt n) -> n; _ -> 0
    ts k = case get k v of Just (PList xs) -> Just [s | PStr s <- xs]; _ -> Nothing

-- | The environment variables the agent and its auxiliary client read
-- (keys and tokens masked).
replayEnvNames :: [Text]
replayEnvNames =
  [ "OPENROUTER_API_KEY", "OPENROUTER_BASE_URL", "OPENAI_BASE_URL", "OPENAI_API_KEY", "OPENAI_MODEL", "FISHER_MODEL", "LLM_MODEL"
  , "GLM_API_KEY", "ZAI_API_KEY", "Z_AI_API_KEY", "GLM_BASE_URL", "KIMI_API_KEY", "KIMI_BASE_URL"
  , "MINIMAX_API_KEY", "MINIMAX_BASE_URL", "MINIMAX_CN_API_KEY", "MINIMAX_CN_BASE_URL", "DEEPSEEK_API_KEY", "DEEPSEEK_BASE_URL"
  , "ANTHROPIC_API_KEY", "ANTHROPIC_TOKEN", "CLAUDE_CODE_OAUTH_TOKEN", "FISHER_INFERENCE_PROVIDER"
  , "CONTEXT_COMPRESSION_THRESHOLD", "CONTEXT_COMPRESSION_ENABLED", "CONTEXT_COMPRESSION_MODEL", "CONTEXT_COMPRESSION_PROVIDER" ]
    <> [p <> t <> "_" <> k | p <- ["AUXILIARY_", "CONTEXT_"], t <- ["COMPRESSION", "MCP", "FLUSH_MEMORIES"], k <- ["PROVIDER", "MODEL", "BASE_URL", "API_KEY"]]

secretName :: Text -> Bool
secretName k = any (`T.isSuffixOf` k) ["_KEY", "_TOKEN"]

snapshotEnv :: Sys -> IO [(Text, Maybe Text)]
snapshotEnv sys = mapM (\k -> (,) k . fmap (\v -> if secretName k then mask v else v) <$> sysGetEnv sys k) replayEnvNames

-- | A stand-in that masks the same way as the original.
unmask :: Text -> Text
unmask masked
  | "Bearer " `T.isPrefixOf` masked = "Bearer " <> unmask (T.drop 7 masked)
  | "..." `T.isInfixOf` masked = T.take 8 masked <> T.replicate 12 "x" <> T.takeEnd 4 masked
  | otherwise = "short"

-- | The agent's settings and tool surface, logged once when it is made.
logAgent :: ReplayLog -> AgentConfig -> ToolRunner -> [(Text, Maybe Text)] -> IO ()
logAgent lg cfg tr envVars = do
  reqs <- trRequirements tr
  logEvent lg "agent"
    [ ("version", PInt 1), ("config", configToPy cfg), ("tools", PList (trDefinitions tr))
    , ("printed", PList (map PStr (trPrinted tr))), ("requirements", PList [PList [PStr n, PBool ok] | (n, ok) <- reqs])
    , ("env", PDict [(k, maybe PNull PStr v) | (k, v) <- envVars]) ]

logTurn :: ReplayLog -> RunInput -> IO ()
logTurn lg ri =
  logEvent lg "turn"
    [ ("user", PStr (riUserMessage ri)), ("system_message", maybe PNull PStr (riSystemMessage ri))
    , ("history", PList (riHistory ri)), ("task_id", maybe PNull PStr (riTaskId ri))
    , ("persist", maybe PNull PStr (riPersistUserMessage ri)) ]

logOutcome :: ReplayLog -> AgentResult -> IO ()
logOutcome lg = \case
  RunDone r -> logEvent lg "result" [("result", r)]
  RunRaised c m -> logEvent lg "raised" [("class", PStr c), ("message", PStr m)]

-- | What a replay found.
data ReplayReport = ReplayReport
  { rpTurns :: Int
  , rpRequests :: Int
  , rpMismatches :: [Text]
  }
  deriving (Show)

-- | What a replay takes from the running program.
data ReplayBase = ReplayBase
  { rbSys :: Sys
  , rbNow :: LocalTime
  , rbLoadConfig :: IO PyVal
  , rbSkinEmoji :: Text -> Maybe Text
  }

-- | Run a logged session again. Each process that wrote to the log (each
-- @agent@ record: a resume appends) gets a fresh agent; each turn starts
-- from its logged history; requests, tool calls and results are compared
-- with the log.
replayFile :: ReplayBase -> FilePath -> IO (Either Text ReplayReport)
replayFile = replayFileWith (\c -> pythonRepl "python3" (rcBlockTimeout c))

-- | The same, with the REPL an RLM session's recalls run in (they run again
-- on replay; their model calls are answered from the log).
replayFileWith :: (RlmConfig -> ReplFactory) -> ReplayBase -> FilePath -> IO (Either Text ReplayReport)
replayFileWith replOf rb path = do
  raw <- BS.readFile path
  let parsed = [v | l <- BC.lines raw, not (BS.null l), Right v <- [loads l]]
      kind v = maybe "" pyStrOf (get "kind" v)
      ofKind k = [v | v <- parsed, kind v == k]
  case ofKind "agent" of
    [] -> pure (Left "no agent record in the log")
    _ -> do
      httpQ <- newIORef (ofKind "http")
      toolQ <- newIORef (ofKind "tool")
      localQ <- newIORef (ofKind "local_tool")
      promptQ <- newIORef [maybe "" pyStrOf (get "text" p) | p <- ofKind "prompt"]
      readQ <- newIORef [[(p, [x | PStr x <- rs]) | PList [PStr p, PList rs] <- fs] | e <- ofKind "read_files", Just (PList fs) <- [get "files" e]]
      uuidQ <- newIORef [maybe "" pyStrOf (get "hex" u) | u <- ofKind "uuid"]
      clmWriteQ <- newIORef [maybe "" pyStrOf (get "text" w) | w <- ofKind "clm_write"]
      clmReadQ <- newIORef [fromMaybe PNull (get "file" r) | r <- ofKind "clm_read"]
      clmWrites <- newIORef (0 :: Int)
      offloadQ <- newIORef (ofKind "rlm_offload")
      cutQ <- newIORef (ofKind "rlm_cut")
      cuts <- newIORef (0 :: Int)
      recallQ <- newIORef (ofKind "rlm_recall")
      clmSpillQ <- newIORef (ofKind "clm_spill")
      clmArchiveQ <- newIORef (ofKind "clm_archive")
      clmSpills <- newIORef (0 :: Int)
      clmArchives <- newIORef (0 :: Int)
      clmRecallQ <- newIORef (ofKind "clm_recall")
      clmRecalls <- newIORef (0 :: Int)
      offloads <- newIORef (0 :: Int)
      recalls <- newIORef (0 :: Int)
      mismatches <- newIORef []
      -- a turn the log has no end for (its process was stopped mid-turn):
      -- run only as far as its own records; once it asks for one the log
      -- does not have, it is cut (the clock's offset then, to put back),
      -- and nothing more is taken from the queues
      truncated <- newIORef False
      cutAt <- newIORef (Nothing :: Maybe Double)
      leftInbox <- newIORef (0 :: Int)
      sent <- newIORef (0 :: Int)
      offset <- newIORef (0 :: Double)
      t0 <- sysNow (rbSys rb)
      let note m = atomicModifyIORef' mismatches (\ms -> (ms <> [m], ()))
          clock = (t0 +) <$> readIORef offset
          strip = \case
            PDict kvs -> PDict [kv | kv@(k, _) <- kvs, k /= "kind"]
            v -> v
          -- the logged event the predicate picks (else the next), compared
          -- with the one computed again
          compareWith what counter q pick got = do
            k <- atomicModifyIORef' counter (\x -> (x + 1, x + 1))
            logged <- atomicModifyIORef' q (takeFirst pick)
            case logged of
              Nothing -> note (what <> " " <> T.pack (show k) <> ": not in the log")
              Just l -> when (strip l /= got) $ note (what <> " " <> T.pack (show k) <> " differs: " <> describe got (strip l))
          sleep d = atomicModifyIORef' offset (\o -> (o + d, ()))
          next q = atomicModifyIORef' q (\case [] -> ([], Nothing); (x : xs) -> (xs, Just x))
          -- the first logged exchange with this request (recall's
          -- concurrent sub-calls are logged in the order they finished),
          -- else the next one, whose difference is noted
          isCut = isJust <$> readIORef cutAt
          cut = readIORef offset >>= \o -> modifyIORef' cutAt (Just . fromMaybe o)
          answer hr = isCut >>= \case
           True -> pure (Left ConnectionError)
           False -> do
            let got0 = requestRecord hr
                pick rec = get "request" rec == Just got0
            trunc <- readIORef truncated
            e <-
              if trunc
                then atomicModifyIORef' httpQ (\q -> case break pick q of
                  (pre, x : post) -> (pre <> post, Just x)
                  _ -> (q, Nothing))
                else atomicModifyIORef' httpQ (takeFirst pick)
            if trunc && isNothing e
              then cut >> pure (Left ConnectionError)
              else answerWith hr e
          answerWith hr e = do
            n <- atomicModifyIORef' sent (\x -> (x + 1, x + 1))
            case e of
              Nothing -> do
                note ("request " <> T.pack (show n) <> ": not in the log (" <> hrMethod hr <> " " <> hrUrl hr <> ")")
                pure (Left ConnectionError)
              Just rec -> do
                let want = fromMaybe PNull (get "request" rec)
                    got = requestRecord hr
                when (got /= want) $ note ("request " <> T.pack (show n) <> " differs: " <> describe got want)
                let resp = fromMaybe PNull (get "response" rec)
                pure $
                  if has "error" resp
                    then Left (errorFromRecord resp)
                    else
                      Right
                        ( HttpResp
                            (case get "status" resp of Just (PInt s) -> fromIntegral s; _ -> 200)
                            (case get "headers" resp of Just (PList hs) -> [(k, v) | PList [PStr k, PStr v] <- hs]; _ -> [])
                            (TE.encodeUtf8 (maybe "" pyStrOf (get "body" resp))) )
          tr =
            Transport
              { tRequest = \hr _ -> answer hr
              , tStream = \hr _ k ->
                  answer hr >>= \case
                    Left e -> pure (Left e)
                    Right resp -> do
                      left <- newIORef (rBody resp)
                      let reader = atomicModifyIORef' left (\b -> ("", b))
                      Right <$> k (rStatus resp) (rHeaders resp) reader }
      (db, _) <- memorySessionDB
      turnNo <- newIORef (0 :: Int)
      -- one run per process: a fresh agent, model cache and aux state;
      -- the session store and the logged queues carry over
      forM_ (runs parsed) $ \(agentRec, evs) -> do
        let sys0 = rbSys rb
            sys =
              sys0
                { sysNow = clock
                , sysSleep = sleep
                , sysOut = \_ -> pure ()
                , sysUuidHex = isCut >>= \c -> if c then sysUuidHex sys0 else next uuidQ >>= maybe (sysUuidHex sys0) pure
                , sysGetEnv = \k -> case get "env" agentRec >>= get k of
                    Just (PStr v) -> pure (Just (if secretName k then unmask v else v))
                    Just PNull -> pure Nothing
                    _ -> sysGetEnv sys0 k }
            rt = Runtime clock sleep (pure 0)
            defs = case get "tools" agentRec of Just (PList ds) -> ds; _ -> []
            runner =
              ToolRunner
                { trDefinitions = defs
                , trPrinted = case get "printed" agentRec of Just (PList ps) -> [p | PStr p <- ps]; _ -> []
                , trCall = \name args task _ -> isCut >>= \c -> if c then pure "{\"error\": \"the logged turn ends here\"}" else do
                    -- concurrent calls are logged in the order they finished:
                    -- match on the call, not the position
                    let same rec = get "name" rec == Just (PStr name) && get "args" rec == Just args && get "task_id" rec == Just (PStr task)
                    e <- atomicModifyIORef' toolQ (\q -> case break same q of
                      (pre, x : post) -> (pre <> post, Just x)
                      _ -> (q, Nothing))
                    trunc <- readIORef truncated
                    case e of
                      Nothing | trunc -> cut >> pure "{\"error\": \"the logged turn ends here\"}"
                      Nothing -> note ("tool call not in the log: " <> name <> " " <> dumpsUnicode args) >> pure "{\"error\": \"not in the replay log\"}"
                      Just rec -> case get "result" rec of
                        Just (PStr r) -> pure r
                        _ -> ioError (userError (T.unpack (maybe "tool raised" pyStrOf (get "raised" rec))))
                , trEmoji = rbSkinEmoji rb
                , trReadFilesSummary = \_ -> isCut >>= \c -> if c then pure [] else fromMaybe [] <$> next readQ
                , trCleanup = \_ -> pure ()
                , trSetInterrupt = \_ -> pure ()
                , trRequirements = pure (case get "requirements" agentRec of Just (PList rs) -> [(n, ok) | PList [PStr n, PBool ok] <- rs]; _ -> []) }
        cache <- newModelCache
        st <- newAuxState
        config <- rbLoadConfig rb
        -- the inbox: this process's messages, each at its boundary
        inboxQ <- newIORef [(n, maybe "" pyStrOf (get "text" e)) | e <- evs, kind e == "inbox", Just (PInt n) <- [get "boundary" e]]
        boundary <- newIORef (0 :: Integer)
        let inbox = do
              k <- atomicModifyIORef' boundary (\x -> (x + 1, x + 1))
              atomicModifyIORef' inboxQ (\q -> ([x | x@(n, _) <- q, n /= k], [t | (n, t) <- q, n == k]))
        let auxEnv = AuxEnv sys rt tr (rbLoadConfig rb) (\_ -> pure ()) st
            env =
              AgentEnv
                { envSys = sys, envRuntime = rt, envTransport = tr
                , envLlm = Env (sysUuidHex sys) (\_ r -> pure ("[The " <> r <> " attached an image.]"))
                , envNowLocal = pure (rbNow rb), envPromptNow = pure (rbNow rb), envCwd = pure "."
                , envChoice = \_ -> pure 0, envAnimate = False, envDb = Just db, envTools = runner
                , envAux = callLlm auxEnv, envAuxEnv = Just auxEnv
                , envMeta = MetaEnv tr clock (sysFisherHome sys) cache (\_ -> pure ())
                , envSkin = defaultSkin, envConfig = config, envCallbacks = noCallbacks, envWarn = \_ -> pure ()
                , envPromptHook = \built -> isCut >>= \c -> if c then pure built else fromMaybe built <$> next promptQ
                , envWriteFiles = False
                -- the todo list runs again (it is in memory); memory writes
                -- files, so its results come from the log
                , envLocalTool = \name args act -> isCut >>= \c -> if c then pure "{\"error\": \"the logged turn ends here\"}" else do
                    e <- next localQ
                    case e of
                      Just rec | get "name" rec == Just (PStr name), get "args" rec == Just args, Just (PStr r) <- get "result" rec -> do
                        when (name /= "memory") $ do
                          r' <- act
                          when (r' /= r) $ note ("local tool " <> name <> " differs: " <> T.take 120 r')
                        pure r
                      _ -> do
                        note ("local tool call not in the log: " <> name <> " " <> dumpsUnicode args)
                        if name == "memory" then pure "{\"error\": \"not in the replay log\"}" else act
                , envInbox = inbox }
            key = "sk-replay-" <> T.replicate 20 "0"
            cfg = configFromPy key (fromMaybe (PDict []) (get "config" agentRec))
        -- the recorded key is masked; requests are compared with it masked
        -- too, so the replay's key must mask the same way
        let cfg' = cfg {cfgApiKey = unmask (maybe "" pyStrOf (get "config" agentRec >>= get "api_key"))}
        agent <- newAgent cfg' env
        -- CLM: the context file's writes are checked against the log, its
        -- reads answered from it; with the store on (a log without the key:
        -- off), the loaded store comes from the log, each spill and archive
        -- step is computed again and compared with it, and recall runs again
        -- in the replay's REPL (as RLM's); nothing is saved
        forM_ [e | e <- evs, kind e == "clm"] $ \c -> do
          let clmCfg = clmConfigFromPy (get "config" c)
          recallSetup <- case get "recall" c of
            Just v | ccStore clmCfg -> case rlmConfigFromPy (Just v) of
              Right rc -> pure (Just (rc, replOf rc))
              Left err -> note ("clm recall config: " <> err) >> pure Nothing
            _ -> pure Nothing
          let io =
                MirrorIO
                  { mioPath = maybe "" pyStrOf (get "path" c)
                  , mioWrite = \t -> do
                      k <- atomicModifyIORef' clmWrites (\x -> (x + 1, x + 1))
                      w <- next clmWriteQ
                      case w of
                        Nothing -> note ("context file write " <> T.pack (show k) <> ": not in the log")
                        Just logged -> when (logged /= t) $ note ("context file write " <> T.pack (show k) <> " differs: " <> describe (PStr t) (PStr logged))
                  , mioRead = next clmReadQ >>= \case
                      Nothing -> note "context file read: not in the log" >> pure Nothing
                      Just r -> pure (fileFromRecord r)
                  , mioWriteBackup = \_ -> pure () }
              setup =
                ClmSetup
                  { csConfig = clmCfg, csIO = io, csObserve = \_ -> pure ()
                  , csSessionId = maybe "" pyStrOf (get "session" c)
                  , csLoad = pure (get "state" c >>= stateFromPy), csSave = \_ -> pure ()
                  , csStoreLoad = pure (get "store" c >>= storeFromPy), csStoreSave = \_ -> pure ()
                  , csOnSpill = \i -> compareWith "spill" clmSpills clmSpillQ (const True) (PDict (spillRecord i))
                  , csOnArchive = \is w -> compareWith "archive" clmArchives clmArchiveQ (const True) (PDict (archiveRecord is w))
                  , csRecall = recallSetup
                  , csOnRecall = \q r -> compareWith "recall" clmRecalls clmRecallQ (\l -> get "question" l == Just (PStr q)) (PDict (recallRecord q r)) }
          setContextStrategy agent (UseClm setup)
        -- RLM: each offload and recall is computed again and compared with
        -- the log; nothing is saved
        forM_ [e | e <- evs, kind e == "rlm"] $ \c -> do
          rcfg <- case rlmConfigFromPy (get "config" c) of
            Right x -> pure (oldOffloads (get "config" c) x)
            Left err -> note ("rlm config: " <> err) >> pure defaultRlmConfig
          let setup =
                RlmSetup
                  { rlConfig = rcfg
                  , rlRepl = replOf rcfg
                  , rlLoad = pure (get "state" c >>= Rlm.stateFromPy)
                  , rlSave = \_ -> pure ()
                  , rlOnOffload = \o -> compareWith "offload" offloads offloadQ (const True) (PDict (offloadRecord o))
                  , rlOnCut = \ts -> compareWith "cut" cuts cutQ (const True) (PDict (cutRecord ts))
                  , rlOnRecall = \q r -> compareWith "recall" recalls recallQ (\l -> get "question" l == Just (PStr q)) (PDict (recallRecord q r)) }
          setContextStrategy agent (UseRlm setup)
        let turns = zip (pairTurns evs) (turnSlices evs)
        forM_ turns $ \((turn, outcome), slice) -> do
          i <- atomicModifyIORef' turnNo (\x -> (x + 1, x + 1))
          writeIORef truncated (isNothing outcome)
          let ri =
                (runInput (maybe "" pyStrOf (get "user" turn)))
                  { riSystemMessage = case get "system_message" turn of Just (PStr s) -> Just s; _ -> Nothing
                  , riHistory = case get "history" turn of Just (PList h) -> h; _ -> []
                  , riTaskId = case get "task_id" turn of Just (PStr s) -> Just s; _ -> Nothing
                  , riPersistUserMessage = case get "persist" turn of Just (PStr s) -> Just s; _ -> Nothing }
          res <- try @SomeException (runConversation agent ri)
          wasCut <- readIORef cutAt
          writeIORef truncated False
          writeIORef cutAt Nothing
          -- a cut turn: every request and tool call its records hold was made
          forM_ wasCut $ \o -> do
            writeIORef offset o
            leftH <- readIORef httpQ
            leftT <- readIORef toolQ
            let posts = [r | r <- slice, kind r == "http", (get "request" r >>= get "method") == Just (PStr "POST")]
                tools = [r | r <- slice, kind r == "tool"]
                missedH = length (filter (`elem` leftH) posts)
                missedT = length (filter (`elem` leftT) tools)
            unless' (missedH + missedT == 0) $
              note ("turn " <> T.pack (show i) <> " (stopped in the log) was cut with " <> T.pack (show missedH) <> " logged request(s) and " <> T.pack (show missedT) <> " tool call(s) not made")
          case (res, outcome) of
            (Left _, _) | isJust wasCut -> pure ()
            (Left e, _) -> note ("turn " <> T.pack (show i) <> ": " <> T.pack (show e))
            (Right (RunDone r), Just o) | kind o == "result" ->
              when (Just r /= get "result" o) $ note ("turn " <> T.pack (show i) <> " result differs: " <> describe r (fromMaybe PNull (get "result" o)))
            (Right (RunRaised c m), Just o) | kind o == "raised" ->
              when (Just (PStr c) /= get "class" o || Just (PStr m) /= get "message" o) $ note ("turn " <> T.pack (show i) <> " raised differently: " <> c <> ": " <> m)
            (Right _, Nothing) -> pure ()
            (Right _, Just _) -> note ("turn " <> T.pack (show i) <> " ended differently")
        notTaken <- length <$> readIORef inboxQ
        modifyIORef' leftInbox (+ notTaken)
      leftHttp <- readIORef httpQ
      unless' (null leftHttp) $ note (T.pack (show (length leftHttp)) <> " logged request(s) were not sent")
      nInbox <- readIORef leftInbox
      unless' (nInbox == 0) $ note (T.pack (show nInbox) <> " logged inbox message(s) were not delivered")
      leftClm <- readIORef clmReadQ
      unless' (null leftClm) $ note (T.pack (show (length leftClm)) <> " logged context file read(s) were not made")
      leftOff <- readIORef offloadQ
      unless' (null leftOff) $ note (T.pack (show (length leftOff)) <> " logged offload(s) were not made")
      leftCut <- readIORef cutQ
      unless' (null leftCut) $ note (T.pack (show (length leftCut)) <> " logged tail cut(s) were not made")
      leftRecall <- readIORef recallQ
      unless' (null leftRecall) $ note (T.pack (show (length leftRecall)) <> " logged recall(s) were not made")
      leftSpill <- readIORef clmSpillQ
      unless' (null leftSpill) $ note (T.pack (show (length leftSpill)) <> " logged spill(s) were not made")
      leftArchive <- readIORef clmArchiveQ
      unless' (null leftArchive) $ note (T.pack (show (length leftArchive)) <> " logged archive(s) were not made")
      leftClmRecall <- readIORef clmRecallQ
      unless' (null leftClmRecall) $ note (T.pack (show (length leftClmRecall)) <> " logged CLM recall(s) were not made")
      n <- readIORef sent
      ms <- readIORef mismatches
      nt <- readIORef turnNo
      pure (Right (ReplayReport nt n ms))
  where
    unless' c a = if c then pure () else a
    -- the first entry the predicate picks, else the next one (whose
    -- difference the caller notes)
    takeFirst :: (a -> Bool) -> [a] -> ([a], Maybe a)
    takeFirst pick q = case break pick q of
      (pre, x : post) -> (pre <> post, Just x)
      _ -> case q of
        [] -> ([], Nothing)
        (x : xs) -> (xs, Just x)
    isAgent v = maybe "" pyStrOf (get "kind" v) == "agent"
    -- each turn's records, up to the next turn
    turnSlices vs = [takeWhile ((/= "turn") . kindOf') rest | (t : rest) <- tails vs, kindOf' t == "turn"]
    kindOf' x = maybe "" pyStrOf (get "kind" x)
    runs vs = case break isAgent vs of
      (_, []) -> []
      (_, a : rest) -> let (seg, more) = break isAgent rest in (a, seg) : runs more
    pairTurns vs = go vs
      where
        go [] = []
        go (v : rest)
          | k v == "turn" =
              let after = dropWhile (\x -> k x `notElem` ["turn", "result", "raised"]) rest
               in case after of
                    (o : rest') | k o `elem` ["result", "raised"] -> (v, Just o) : go rest'
                    _ -> (v, Nothing) : go after
          | otherwise = go rest
        k x = maybe "" pyStrOf (get "kind" x)

-- | The first difference between two values, briefly.
describe :: PyVal -> PyVal -> Text
describe got want = go "" got want
  where
    go p a b
      | a == b = "(same)"
      | otherwise = case (a, b) of
          (PDict xs, PDict ys) | map fst xs == map fst ys ->
            head ([d | ((k, x), (_, y)) <- zip xs ys, x /= y, let d = go (p <> "." <> k) x y] <> [p])
          (PList xs, PList ys) | length xs == length ys ->
            head ([d | (i, x, y) <- zip3 [0 :: Int ..] xs ys, x /= y, let d = go (p <> "[" <> T.pack (show i) <> "]") x y] <> [p])
          (PStr x, PStr y) ->
            let common = length (takeWhile id (zipWith (==) (T.unpack x) (T.unpack y)))
             in p <> " at character " <> T.pack (show common) <> ": " <> T.take 60 (T.drop common x) <> " | logged: " <> T.take 60 (T.drop common y)
          _ -> p <> ": " <> T.take 120 (dumpsUnicode a) <> " | logged: " <> T.take 120 (dumpsUnicode b)
