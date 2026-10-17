-- | RLM sessions replay bit for bit (spec phase 2, section 5): a scripted
-- session that offloads twice and recalls once, the recall's code making
-- two concurrent sub-calls, is recorded with a replay log as the CLI writes
-- it, and 'replayFileWith' runs it again from the log with the same fake
-- REPL. A log whose offload stub or recall reply was changed reports a
-- difference.
module Fisher.Agent.RlmReplaySpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import Numeric (showHex)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.AuxSpec (testSys)
import Fisher.Agent.Display (defaultSkin)
import Fisher.Agent.Loop
import Fisher.Agent.Metadata
import Fisher.Agent.Replay
import Fisher.Agent.TestUtil
import Fisher.Agent.Types (AuxError (..))
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..), defaultRlmConfig)
import Fisher.RLM.Parse (BlockResult (..))
import Fisher.RLM.Repl (CallKind (..), ReplFactory, ReplHandle (..))

data Reply
  = Agent Text [(Text, Text, PyVal)]
  | Root Text

chat :: Text -> [(Text, Text, PyVal)] -> PyVal
chat content calls =
  let msg =
        PDict $
          [("role", PStr "assistant"), ("content", PStr content)]
            <> [ ( "tool_calls"
                 , PList
                     [ PDict
                         [ ("id", PStr i), ("type", PStr "function")
                         , ("function", PDict [("name", PStr n), ("arguments", PStr (dumpsAscii a))]) ]
                     | (i, n, a) <- calls ] )
               | not (null calls) ]
   in PDict
        [ ("id", PStr "chatcmpl-rlm"), ("object", PStr "chat.completion"), ("model", PStr "openai/gpt-5")
        , ("choices", PList [PDict [("index", PInt 0), ("message", msg), ("finish_reason", PStr (if null calls then "stop" else "tool_calls"))]])
        , ("usage", PDict [("prompt_tokens", PInt 100), ("completion_tokens", PInt 10), ("total_tokens", PInt 110), ("cost", PFloat 0.01)]) ]

bigResult, hugeResult :: Text
bigResult = T.replicate 1500 "0123456789"
hugeResult = T.replicate 2500 "abcdefghij"

readCall :: Text -> Text -> (Text, Text, PyVal)
readCall i p = (i, "read_file", PDict [("path", PStr p)])

toolDef :: Text -> PyVal
toolDef n =
  PDict
    [ ("type", PStr "function")
    , ("function", PDict [("name", PStr n), ("description", PStr n), ("parameters", PDict [("type", PStr "object"), ("properties", PDict [])])]) ]

-- | The session: four reads (the fourth over the threshold: an offload), a
-- recall whose code asks two sub-calls at once, another big read (a second
-- offload), the answer.
script :: [Reply]
script =
  [ Agent "" [readCall "c1" "a"]
  , Agent "" [readCall "c2" "b"]
  , Agent "" [readCall "c3" "c"]
  , Agent "" [readCall "c4" "big"]
  , Agent "" [("c5", "recall", PDict [("question", PStr "What did file b say?")])]
  , Root "SUBS"
  , Agent "" [readCall "c6" "big"]
  , Agent "done" [] ]

-- | 'script' with a huge sixth read: the second offload leaves the history
-- over the threshold, and the tail cut stores turn 13.
cutScript :: [Reply]
cutScript = [if isSixth r then Agent "" [readCall "c6" "huge"] else r | r <- script]
  where
    isSixth = \case
      Agent _ [("c6", _, _)] -> True
      _ -> False

-- | The sub-calls' prompts, and the answer each gets.
subAnswers :: [(Text, Text)]
subAnswers = [("sub one", "S1"), ("sub two", "S2")]

-- | A REPL whose block @SUBS@ asks both sub-calls in one call (the loop
-- runs them concurrently) and answers with their replies; any other block
-- answers with its own text.
fakeRepl :: ReplFactory
fakeRepl _ =
  pure . Right $
    ReplHandle
      ( \code cb ->
          if T.strip code == "SUBS"
            then do
              rs <- cb CallLlm (map fst subAnswers) Nothing
              pure (BlockResult "" "" [] [] (Just ("answer " <> T.intercalate "," rs)))
            else pure (BlockResult "" "" [] [] (Just ("answer " <> T.strip code))) )
      (pure ())

now :: LocalTime
now = LocalTime (fromGregorian 2026 10 13) (TimeOfDay 10 15 0)

-- | Run the session with a replay log; the log's path.
record :: FilePath -> IO FilePath
record = recordWith script

recordWith :: [Reply] -> FilePath -> IO FilePath
recordWith scr dir0 = do
  dir <- canonicalizePath dir0
  let home = dir </> "home"
      gauss = home </> ".gauss"
      sid = "20261013_101500_ab12cd"
  createDirectoryIfMissing True gauss
  lg <- openReplayLog dir sid
  offset <- newIORef (0 :: Double)
  uuidN <- newIORef (0 :: Int)
  queue <- newIORef scr
  let clock = (1790000000 +) <$> readIORef offset
      sleep d = atomicModifyIORef' offset (\o -> (o + d, ()))
      sys0 =
        (testSys home [("HOME", T.pack home), ("GAUSS_TIMEZONE", "UTC"), ("CONTEXT_COMPRESSION_THRESHOLD", "0.5")] (\_ -> pure ()))
          { sysNow = clock, sysSleep = sleep
          , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, T.justifyRight 32 '0' (T.pack (showHex n "")))) }
      sys = recordUuids lg sys0
      rt = Runtime clock sleep (pure 0)
      ok v = pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii v))))
      subOf body = case [a | m <- list "messages" body, (p, a) <- subAnswers, get "content" m == Just (PStr p)] of
        (a : _) -> Just a
        [] -> Nothing
      respond hr
        | hrMethod hr == "GET" =
            ok (PDict [("data", PList [PDict [("id", PStr "openai/gpt-5"), ("context_length", PInt 10000)]])])
        | Just a <- subOf (fromMaybe PNull (hrBody hr)) = ok (chat a [])
        | otherwise = do
            next <- atomicModifyIORef' queue (\case [] -> ([], Nothing); (a : rest) -> (rest, Just a))
            case next of
              Nothing -> pure (Right (HttpResp 500 [] "{\"error\": {\"message\": \"no scripted answer\"}}"))
              Just (Agent c calls) -> ok (chat c calls)
              Just (Root code) -> ok (chat ("```repl\n" <> code <> "\n```") [])
      tr = recordTransport lg (Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError)))
      runner =
        recordToolRunner lg
          ToolRunner
            { trDefinitions = [toolDef "read_file"]
            , trPrinted = []
            , trCall = \_ args _ _ -> pure $ case get "path" args of
                Just (PStr "big") -> bigResult
                Just (PStr "huge") -> hugeResult
                _ -> "small file"
            , trEmoji = const Nothing
            , trReadFilesSummary = \_ -> pure []
            , trCleanup = \_ -> pure ()
            , trSetInterrupt = \_ -> pure ()
            , trRequirements = pure [] }
      setup =
        recordRlm lg
          RlmSetup
            -- no gap and a least size near 0, so every offload of the
            -- scripts goes ahead; replay must read both back from the log
            { rlConfig = defaultRlmConfig {rcModel = "openai/gpt-5", rcOffloadGap = 0, rcMinOffload = 1e-9}
            , rlRepl = fakeRepl
            , rlLoad = pure Nothing
            , rlSave = \_ -> pure ()
            , rlOnOffload = \_ -> pure ()
            , rlOnCut = \_ -> pure ()
            , rlOnRecall = \_ _ -> pure () }
      cfg =
        defaultAgentConfig
          { cfgModel = "openai/gpt-5", cfgApiKey = "sk-or-test-1234567890", cfgBaseUrl = "https://openrouter.ai/api/v1"
          , cfgBaseUrlGiven = True, cfgProvider = "openrouter", cfgApiMode = ChatCompletions, cfgMaxIterations = 30
          , cfgToolDelay = 0, cfgQuiet = True, cfgSkipContextFiles = True, cfgSkipMemory = True
          , cfgSessionId = Just sid, cfgPlatform = Just "cli" }
  cache <- newModelCache
  let env =
        AgentEnv
          { envSys = sys, envRuntime = rt, envTransport = tr
          , envLlm = Env (sysUuidHex sys) (\_ r -> pure ("[The " <> r <> " attached an image.]"))
          , envNowLocal = pure now, envPromptNow = pure now
          , envCwd = pure dir, envChoice = \_ -> pure 0, envAnimate = False, envDb = Nothing, envTools = runner
          , envAux = \_ -> pure (Left (AuxNoProvider ""))
          , envAuxEnv = Nothing
          , envMeta = MetaEnv tr clock gauss cache (\_ -> pure ())
          , envSkin = defaultSkin, envConfig = PDict [], envCallbacks = noCallbacks
          , envWarn = \_ -> pure (), envPromptHook = recordPromptHook lg, envWriteFiles = False
          , envLocalTool = recordLocalTool lg, envInbox = pure [] }
  agent <- newAgent cfg env
  snapshotEnv sys >>= logAgent lg cfg runner
  setContextStrategy agent (UseRlm setup)
  logRlm lg setup Nothing
  let ri = runInput "Read the files."
  logTurn lg ri
  res <- runConversation agent ri
  logOutcome lg res
  closeReplayLog lg
  case res of
    RunDone d | get "completed" d == Just (PBool True) -> pure ()
    _ -> expectationFailure "the recorded session did not complete"
  pure (replayPath dir sid)

replay :: FilePath -> FilePath -> IO ReplayReport
replay dir path = do
  let base = ReplayBase (testSys dir [] (\_ -> pure ())) now (pure (PDict [])) (const Nothing)
  replayFileWith (const fakeRepl) base path >>= either (fail . T.unpack) pure

events :: FilePath -> IO [PyVal]
events path = do
  ls <- BC.lines <$> BS.readFile path
  pure [v | l <- ls, Right v <- [loads l]]

-- | Rewrite the events the predicate picks.
rewrite :: (PyVal -> Bool) -> (PyVal -> PyVal) -> FilePath -> IO ()
rewrite pick f path = do
  vs <- events path
  let n = length (filter pick vs)
  if n == 0 then expectationFailure "no event to change" else pure ()
  BS.writeFile path (BC.unlines [TE.encodeUtf8 (dumpsAscii (if pick v then f v else v)) | v <- vs])

kindIs :: Text -> PyVal -> Bool
kindIs k v = get "kind" v == Just (PStr k)

spec :: Spec
spec = describe "RLM sessions replay" $ do
  it "logs the strategy, each offload and the recall" $ withSystemTempDirectory "rlm-replay" $ \dir -> do
    path <- record dir
    vs <- events path
    let ofKind k = filter (kindIs k) vs
    length (ofKind "rlm") `shouldBe` 1
    map (get "moved") (ofKind "rlm_offload") `shouldBe` [Just (PList [PInt 4, PInt 5]), Just (PList [PInt 6, PInt 9])]
    map (get "store_turns") (ofKind "rlm_offload") `shouldBe` [Just (PInt 2), Just (PInt 6)]
    mapM_ (\e -> (maybe "" pyStrOf (get "stub" e)) `shouldSatisfy` T.isPrefixOf "Turns 4 to ") (ofKind "rlm_offload")
    mapM_ (\e -> get "chars" e `shouldSatisfy` (\case Just (PInt n) -> n > 0; _ -> False)) (ofKind "rlm_offload")
    case ofKind "rlm_recall" of
      [r] -> do
        get "question" r `shouldBe` Just (PStr "What did file b say?")
        get "answer" r `shouldBe` Just (PStr "answer S1,S2")
        get "ending" r `shouldBe` Just (PStr "answered")
        get "iterations" r `shouldBe` Just (PInt 1)
        get "calls" r `shouldBe` Just (PInt 3)
      rs -> expectationFailure ("rlm_recall events: " <> show (length rs))
  it "logs a tail cut: the turns it stored and their characters; an offload counts only its new turns" $ withSystemTempDirectory "rlm-replay" $ \dir -> do
    path <- recordWith cutScript dir
    vs <- events path
    let ofKind k = filter (kindIs k) vs
    map (get "turns") (ofKind "rlm_cut") `shouldBe` [Just (PList [PInt 13])]
    map (get "chars") (ofKind "rlm_cut") `shouldBe` [Just (PInt (fromIntegral (T.length "[turn 13, tool read_file]\n" + T.length hugeResult)))]
    map (get "moved") (ofKind "rlm_offload") `shouldBe` [Just (PList [PInt 4, PInt 5]), Just (PList [PInt 6, PInt 9])]
  it "a session with a tail cut replays with no differences, and a changed cut reports one" $ withSystemTempDirectory "rlm-replay" $ \dir -> do
    path <- recordWith cutScript dir
    r <- replay dir path
    rpMismatches r `shouldBe` []
    rewrite (kindIs "rlm_cut") (set "chars" (PInt 1)) path
    r2 <- replay dir path
    rpMismatches r2 `shouldSatisfy` any (T.isInfixOf "cut 1 differs")
  it "a session that offloads and recalls, with two concurrent sub-calls, replays with no differences" $ withSystemTempDirectory "rlm-replay" $ \dir -> do
    path <- record dir
    r <- replay dir path
    rpMismatches r `shouldBe` []
    rpTurns r `shouldBe` 1
  it "replays when the two sub-calls are logged in the other order" $ withSystemTempDirectory "rlm-replay" $ \dir -> do
    path <- record dir
    vs <- events path
    let isSub v = kindIs "http" v && any (\(p, _) -> T.isInfixOf ("\\\"" <> p <> "\\\"") (dumpsAscii (fromMaybe PNull (get "request" v)))) subAnswers
        swapped = case break isSub vs of
          (pre, a : rest) -> case break isSub rest of
            (mid, b : post) -> pre <> [b] <> mid <> [a] <> post
            _ -> vs
          _ -> vs
    length (filter isSub vs) `shouldBe` 2
    BS.writeFile path (BC.unlines (map (TE.encodeUtf8 . dumpsAscii) swapped))
    r <- replay dir path
    rpMismatches r `shouldBe` []
  it "replays a log recorded before min_offload and offload_gap with no gap and no size test" $ withSystemTempDirectory "rlm-replay" $ \dir -> do
    path <- recordWith cutScript dir
    rewrite (kindIs "rlm") (\v -> maybe v (\c -> set "config" (delete "offload_gap" (delete "min_offload" c)) v) (get "config" v)) path
    vs <- events path
    [get "config" v >>= get "min_offload" | v <- vs, kindIs "rlm" v] `shouldBe` [Nothing]
    r <- replay dir path
    rpMismatches r `shouldBe` []
  it "a log whose offload stub was changed reports a difference" $ withSystemTempDirectory "rlm-replay" $ \dir -> do
    path <- record dir
    rewrite (kindIs "rlm_offload") (\v -> set "stub" (PStr (maybe "" pyStrOf (get "stub" v) <> " (changed)")) v) path
    r <- replay dir path
    rpMismatches r `shouldSatisfy` any (T.isInfixOf "offload 1 differs")
  it "a log whose recall reply was changed reports a difference" $ withSystemTempDirectory "rlm-replay" $ \dir -> do
    path <- record dir
    let subOne v = kindIs "http" v && T.isInfixOf "\\\"sub one\\\"" (dumpsAscii (fromMaybe PNull (get "request" v)))
        change v = case get "response" v of
          Just resp -> set "response" (set "body" (PStr (T.replace "S1" "Z1" (maybe "" pyStrOf (get "body" resp)))) resp) v
          Nothing -> v
    rewrite subOne change path
    r <- replay dir path
    rpMismatches r `shouldSatisfy` any (T.isInfixOf "recall 1 differs")
  it "a log whose recall answer was changed reports a difference" $ withSystemTempDirectory "rlm-replay" $ \dir -> do
    path <- record dir
    rewrite (kindIs "rlm_recall") (set "answer" (PStr "answer S1")) path
    r <- replay dir path
    rpMismatches r `shouldSatisfy` any (T.isInfixOf "recall 1 differs")
