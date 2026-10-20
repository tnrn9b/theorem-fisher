-- | Runs a scenario of test/golden/clm/scenarios.json through the agent
-- loop with the CLM strategy: a scripted model (the scenario's answers,
-- in order), scripted tools (each call's result, and the text a mirror
-- edit leaves), and the mirror and its backup in memory. Reports what
-- tools/clm/reference.py reports; with @store: true@ in the config, also
-- the store (@store@), each spill (@spills@: item ids) and each archive
-- (@archives@: item ids and why); @store_load@ is the store a resumed
-- session loads. With the store on, recall runs on a fake RLM (a block's
-- code @X@ answers @answer X@; recall's root model takes the next scripted
-- answer, whose content is the block) unless the config has
-- @recall: false@. With @requests: true@ in the scenario, also each POST
-- body's @tools@ (null when absent) and system prompt (@requests@), and the
-- session's cost in dollars (@cost@). An answer's @cost@ goes into its
-- usage; an answer with @http_error@ (a status) is that error, with
-- @body@. Each tool call takes its result from the calls of the answer
-- being run; a run that leaves any of them untaken fails. With
-- @next_task@, a second run follows on the first's messages, and the
-- result is the second's.
module Fisher.Agent.ClmHarness (runScenario, runScenarioWith, fakeRepl) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import Numeric (readHex, showHex)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Fisher.Agent.Aux
import Fisher.Agent.AuxSpec (testSys)
import Fisher.Agent.Context.Budget (promptSection)
import Fisher.Agent.Context.Clm
import Fisher.Agent.Context.ClmStore (Item (..), storeFromPy, storeToPy)
import Fisher.Agent.Context.EditGate (GateMode (..))
import Fisher.Agent.Context.Mirror (Turn (..))
import Fisher.Agent.Display (defaultSkin)
import Fisher.Agent.Loop
import Fisher.Agent.Metadata
import Fisher.Agent.Replay
import Fisher.Agent.TestUtil
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..), defaultRlmConfig)
import Fisher.RLM.Parse (BlockResult (..))
import Fisher.RLM.Repl (ReplFactory, ReplHandle (..))

hexBytes :: Text -> BS.ByteString
hexBytes t = BS.pack (go (T.unpack t))
  where
    go (a : b : rest) = fromIntegral (fst (head (readHex [a, b])) :: Int) : go rest
    go _ = []

toolDef :: Text -> PyVal
toolDef n =
  PDict
    [ ("type", PStr "function")
    , ("function", PDict [("name", PStr n), ("description", PStr n), ("parameters", PDict [("type", PStr "object"), ("properties", PDict [])])]) ]

-- | A chat completion for one scripted answer.
completion :: PyVal -> PyVal
completion a =
  let calls = list "tool_calls" a
      msg =
        PDict $
          [("role", PStr "assistant"), ("content", PStr (txt "content" a))]
            <> [ ( "tool_calls"
                 , PList
                     [ PDict
                         [ ("id", field "id" c), ("type", PStr "function")
                         , ("function", PDict [("name", field "name" c), ("arguments", PStr (dumpsAscii (field "arguments" c)))]) ]
                     | c <- calls ] )
               | not (null calls) ]
   in PDict
        [ ("id", PStr "chatcmpl-clm"), ("object", PStr "chat.completion"), ("model", PStr "openai/gpt-5")
        , ("choices", PList [PDict [("index", PInt 0), ("message", msg), ("finish_reason", PStr (if null calls then "stop" else "tool_calls"))]])
        , ("usage", PDict ([("prompt_tokens", PInt 100), ("completion_tokens", PInt 10), ("total_tokens", PInt 110)] <> [("cost", c) | Just c <- [get "cost" a]])) ]

-- | The scripted call a tool call takes from its batch's calls not yet
-- taken: the first with the same name and arguments, else the first with
-- the same name; the calls without it.
pickCall :: Text -> PyVal -> [PyVal] -> ([PyVal], Maybe PyVal)
pickCall name args q =
  let same c = txt "name" c == name
      exact c = same c && get "arguments" c == Just args
      takeFirst p = case break p q of
        (before, c : after) -> Just (before <> after, Just c)
        _ -> Nothing
   in fromMaybe (q, Nothing) (takeFirst exact <|> takeFirst same)

-- | The calls of an answer the tool runner gives results for (read_stored
-- and recall run in the loop).
runnerCalls :: PyVal -> [PyVal]
runnerCalls a = [c | c <- list "tool_calls" a, txt "name" c `notElem` ["read_stored", "recall"]]

-- | The fake RLM's REPL: a block's code @X@ answers @answer X@.
fakeRepl :: ReplFactory
fakeRepl _ = pure (Right (ReplHandle (\code _ -> pure (BlockResult "" "" [] [] (Just ("answer " <> T.strip code)))) (pure ())))

runScenario :: PyVal -> IO PyVal
runScenario = runScenarioWith Nothing

-- | The same, writing a replay log when one is given (as the CLI does).
runScenarioWith :: Maybe ReplayLog -> PyVal -> IO PyVal
runScenarioWith rec sc = withSystemTempDirectory "fisher-clm" $ \tmp0 -> do
  tmp <- canonicalizePath tmp0
  let home = tmp </> "home"
      fisher = home </> ".fisher"
      cfgV = field "config" sc
      num k d = case get k cfgV of
        Just (PInt n) -> n
        _ -> d
      clmCfg =
        ClmConfig
          { ccBudget = num "budget" 32000, ccReserve = num "reserve" 2048
          , ccGate = if get "gate" cfgV == Just (PStr "shrink") then Shrink else Fit
          , ccMaxRollbacks = fromIntegral (num "max_rollbacks" 50)
          , ccKeepUnread = get "keep_unread" cfgV == Just (PBool True)
          , ccSkill = "", ccStore = get "store" cfgV == Just (PBool True)
          , ccSpill = case get "spill" cfgV of
              Just (PFloat d) -> d
              _ -> 0.25
          , ccStorePrompt = get "store_prompt" cfgV /= Just (PBool False) }
      path = txt "mirror_path" sc
  createDirectoryIfMissing True fisher
  offset <- newIORef (0 :: Double)
  uuidN <- newIORef (0 :: Int)
  answers <- newIORef (list "answers" sc)
  -- the calls of the batch being run not yet taken, and those an earlier
  -- batch left
  calls <- newIORef ([] :: [PyVal])
  leftover <- newIORef ([] :: [PyVal])
  requests <- newIORef ([] :: [PyVal])
  mirror <- newIORef (Nothing :: Maybe BS.ByteString)
  backup <- newIORef (Nothing :: Maybe Text)
  steps <- newIORef ([] :: [[(Text, PyVal)]])
  storeRef <- newIORef PNull
  spills <- newIORef ([] :: [PyVal])
  archives <- newIORef ([] :: [PyVal])
  let clock = (1790000000 +) <$> readIORef offset
      sleep d = modifyIORef offset (+ d)
      sys =
        (testSys home [("HOME", T.pack home), ("FISHER_TIMEZONE", "UTC")] (\_ -> pure ()))
          { sysNow = clock, sysSleep = sleep
          , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, T.justifyRight 32 '0' (T.pack (showHex n "")))) }
      sysR = maybe sys (`recordUuids` sys) rec
      rt = Runtime clock sleep (pure 0)
      respond hr
        | hrMethod hr == "GET" =
            pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii (PDict [("data", PList [PDict [("id", PStr "openai/gpt-5"), ("context_length", PInt 400000)]])])))))
        | otherwise = do
            let body = fromMaybe PNull (hrBody hr)
                sysText = case list "messages" body of
                  (m : _) | get "role" m == Just (PStr "system") -> field "content" m
                  _ -> PNull
            atomicModifyIORef' requests (\rs -> (rs <> [PDict [("tools", fromMaybe PNull (get "tools" body)), ("system", sysText)]], ()))
            q <- atomicModifyIORef' answers (\q0 -> (drop 1 q0, q0))
            case q of
              [] -> pure (Right (HttpResp 500 [] "{\"error\": {\"message\": \"no scripted answer\"}}"))
              (a : _)
                | Just (PInt st) <- get "http_error" a ->
                    pure (Right (HttpResp (fromIntegral st) [] (TE.encodeUtf8 (txt "body" a))))
                | otherwise -> do
                    let cs = runnerCalls a
                    -- a new batch (recall's root answers have no calls)
                    if null (list "tool_calls" a)
                      then pure ()
                      else do
                        old <- atomicModifyIORef' calls (\c0 -> (cs, c0))
                        modifyIORef leftover (<> old)
                    pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii (completion a)))))
      tr0 = Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError))
      tr = maybe tr0 (`recordTransport` tr0) rec
      runner0 =
        ToolRunner
          { trDefinitions = map toolDef ["patch", "read_file", "terminal", "write_file"]
          , trPrinted = []
          -- a batch's calls may run concurrently: each takes its own
          -- scripted call (the same name and arguments) in one atomic step
          , trCall = \name args _ _ -> do
              picked <- atomicModifyIORef' calls (pickCall name args)
              case picked of
                Just c -> do
                  case get "mirror_after" c of
                    Just (PStr t) -> writeIORef mirror (Just (TE.encodeUtf8 t))
                    Just d@(PDict _) -> writeIORef mirror (Just (hexBytes (txt "bytes_hex" d)))
                    _ -> pure ()
                  pure (txt "result" c)
                _ -> pure "{\"error\": \"no scripted call\"}"
          , trEmoji = const Nothing
          , trReadFilesSummary = \_ -> pure []
          , trCleanup = \_ -> pure ()
          , trSetInterrupt = \_ -> pure ()
          , trRequirements = pure [] }
      newStep kvs = modifyIORef steps (<> [kvs])
      addToStep kvs = modifyIORef steps (\ss -> case reverse ss of
        (l : rest) -> reverse ((l <> kvs) : rest)
        [] -> [kvs])
      observe = \case
        EvCall n task ed -> newStep [("call", if n == 0 then PStr "summary" else PInt (fromIntegral n)), ("context", PList ed), ("task_message", task)]
        EvMirrorWrite t -> addToStep [("mirror_write", PStr t)]
        EvMirrorRead b -> addToStep [("mirror_read", case b of
          Nothing -> PNull
          Just bs -> either (const (PDict [("bytes_hex", PStr (T.pack (concatMap hex2 (BS.unpack bs))))])) PStr (TE.decodeUtf8' bs))]
        EvBatch fl -> addToStep fl
      hex2 w = let h = showHex w "" in if length h < 2 then '0' : h else h
      runner = maybe runner0 (`recordToolRunner` runner0) rec
      io0 =
        MirrorIO
          { mioPath = path
          , mioWrite = writeIORef mirror . Just . TE.encodeUtf8
          , mioRead = readIORef mirror
          , mioWriteBackup = writeIORef backup . Just }
      io = maybe io0 (`recordMirror` io0) rec
      maxIt = fromIntegral (num "max_iterations" 90)
      cfg =
        defaultAgentConfig
          { cfgModel = "openai/gpt-5", cfgApiKey = "sk-or-test-1234567890", cfgBaseUrl = "https://openrouter.ai/api/v1"
          , cfgBaseUrlGiven = True, cfgProvider = "openrouter", cfgApiMode = ChatCompletions, cfgMaxIterations = maxIt
          , cfgToolDelay = 0, cfgQuiet = True, cfgSkipContextFiles = True, cfgSkipMemory = True
          , cfgSessionId = Just "20261006_101500_ab12cd", cfgPlatform = Just "cli" }
  cache <- newModelCache
  auxState <- newAuxState
  let auxEnv = AuxEnv sys rt tr (pure (PDict [])) (\_ -> pure ()) auxState
      env =
        AgentEnv
          { envSys = sysR, envRuntime = rt, envTransport = tr
          , envLlm = Env (sysUuidHex sysR) (\_ r -> pure ("[The " <> r <> " attached an image.]"))
          , envNowLocal = pure (LocalTime (fromGregorian 2026 10 6) (TimeOfDay 10 15 0))
          , envPromptNow = pure (LocalTime (fromGregorian 2026 10 6) (TimeOfDay 10 15 0))
          , envCwd = pure tmp, envChoice = \_ -> pure 0, envAnimate = False, envDb = Nothing, envTools = runner
          , envAux = callLlm auxEnv, envAuxEnv = Just auxEnv
          , envMeta = MetaEnv tr clock fisher cache (\_ -> pure ())
          , envSkin = defaultSkin, envConfig = PDict [], envCallbacks = noCallbacks
          , envWarn = \_ -> pure (), envPromptHook = maybe pure recordPromptHook rec, envWriteFiles = False
          , envLocalTool = maybe (\_ _ act -> act) recordLocalTool rec, envInbox = pure [] }
  agent <- newAgent cfg env
  let setup = ClmSetup clmCfg io observe (txt "session_id" sc) (pure Nothing) (\_ -> pure ())
        (pure (get "store_load" sc >>= storeFromPy)) (writeIORef storeRef . storeToPy)
        (\i -> modifyIORef spills (<> [PInt (fromIntegral (itId i))]))
        (\is w -> modifyIORef archives (<> [PDict [("ids", PList [PInt (fromIntegral (itId i)) | i <- is]), ("why", PStr w)]]))
        (if ccStore clmCfg && get "recall" cfgV /= Just (PBool False) then Just (defaultRlmConfig {rcModel = "openai/gpt-5"}, fakeRepl) else Nothing)
        (\_ _ -> pure ())
      -- with a log, the store's spills and archives go to it (as the CLI's)
      setupR = maybe setup (`recordClm` setup) rec
  setContextStrategy agent (UseClm setupR)
  forM_ rec $ \l -> do
    snapshotEnv sysR >>= logAgent l cfg runner
    logClm l setupR Nothing
  let ri = runInput (txt "task" sc)
  forM_ rec (`logTurn` ri)
  res1 <- runConversation agent ri
  forM_ rec (`logOutcome` res1)
  -- a second run of the session, on the first's messages
  res <- case (get "next_task" sc, res1) of
    (Just (PStr t2), RunDone d) -> do
      let ri2 = (runInput t2) {riHistory = list "messages" d}
      forM_ rec (`logTurn` ri2)
      r2 <- runConversation agent ri2
      forM_ rec (`logOutcome` r2)
      pure r2
    _ -> pure res1
  st <- agentClmState agent
  ss <- readIORef steps
  b <- readIORef backup
  storeV <- readIORef storeRef
  spillsV <- readIORef spills
  archivesV <- readIORef archives
  unused <- (<>) <$> readIORef leftover <*> readIORef calls
  if null unused then pure () else fail ("scripted calls not run: " <> show (map (txt "id") unused))
  requestsV <- readIORef requests
  (dollars, _, _) <- agentSessionCost agent
  let r = case res of
        RunDone d ->
          PDict
            [ ("completed", fromMaybe PNull (get "completed" d)), ("partial", fromMaybe PNull (get "partial" d))
            , ("final_response", fromMaybe PNull (get "final_response" d)), ("error", fromMaybe PNull (get "error" d))
            , ("api_calls", fromMaybe PNull (get "api_calls" d)) ]
        RunRaised c m -> PDict [("raised", PStr c), ("message", PStr m)]
  ctx <- forM (maybe [] stTurns st) $ \(Turn i m) -> pure (PDict [("id", PInt (fromIntegral i)), ("message", m)])
  pure . PDict $
      [ ("steps", PList (map PDict ss)), ("result", r), ("final_context", PList ctx)
      , ("counter", PInt (maybe 0 (fromIntegral . stCounter) st)), ("backup", maybe PNull PStr b)
      , ("prompt_section", PStr (promptSection path (clmLimit clmCfg))) ]
        <> [ kv
           | ccStore clmCfg
           , kv <- [("store", storeV), ("spills", PList spillsV), ("archives", PList archivesV)] ]
        <> [ kv
           | get "requests" sc == Just (PBool True)
           , kv <- [("requests", PList requestsV), ("cost", maybe PNull PFloat dollars)] ]
