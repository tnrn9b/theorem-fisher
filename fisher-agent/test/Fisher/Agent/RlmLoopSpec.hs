-- | The agent loop under the RLM context strategy (spec phase 2, sections
-- 3.2 to 3.5 and 6): a scripted transport answers the agent's calls and
-- recall's model calls in order (both go through the agent's transport, as
-- in production), a scripted tool runner gives small or large results, and
-- a fake REPL factory sets the final answer from the code block it runs.
module Fisher.Agent.RlmLoopSpec (spec) where

import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe, isNothing)
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
import Fisher.Agent.Context.Recall (RecallResult (..), agentRlmConfig)
import Fisher.Agent.Context.Rlm
import Fisher.Agent.Display (defaultSkin)
import Fisher.Agent.Loop
import Fisher.Agent.Metadata
import Fisher.Agent.Session (SessionDB (..))
import Fisher.Agent.TestUtil
import Fisher.Agent.Types (AuxError (..))
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..), defaultRlmConfig)
import Fisher.RLM.Parse (BlockResult (..))
import Fisher.RLM.Repl (ReplHandle (..))

-- | One scripted answer: who it is for, and what the provider sends.
data Reply
  = Agent Text [(Text, Text, PyVal)]
  -- ^ the agent's model: content and calls (id, name, arguments)
  | Root Text
  -- ^ recall's root model: a reply with a repl block holding this code
  | Fail Int Text
  -- ^ an HTTP error status and body (for recall's model)
  | AgentFail Int Text
  -- ^ the same, for the agent's model

data Out = Out
  { oBodies :: [(Bool, PyVal)]
  -- ^ every POST body, in order, True for the agent's
  , oResult :: AgentResult
  -- ^ the last turn's
  , oRows :: [(Text, PyVal)]
  -- ^ state.db's appended messages: session and message
  , oPrompts :: [(Text, Text)]
  -- ^ state.db's system prompt updates
  , oCost :: (Maybe Double, Integer, Integer)
  , oSaves :: [RlmState]
  , oMoved :: [(Int, Int)]
  , oRecalls :: [(Text, Text)]
  , oAux :: Int
  , oState :: Maybe RlmState
  , oCuts :: [[Int]]
  -- ^ each tail cut's stored indices
  }

agentCost, rootCost :: Double
agentCost = 0.01
rootCost = 0.25

chat :: Text -> [(Text, Text, PyVal)] -> Double -> PyVal
chat content calls cost =
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
        , ( "usage"
          , PDict
              [ ("prompt_tokens", PInt 100), ("completion_tokens", PInt 10), ("total_tokens", PInt 110)
              , ("prompt_tokens_details", PDict [("cached_tokens", PInt 7), ("cache_write_tokens", PInt 3)])
              , ("cost", PFloat cost) ] ) ]

bigResult, hugeResult :: Text
bigResult = T.replicate 1500 "0123456789"
hugeResult = T.replicate 2500 "abcdefghij"

-- | How a run is set up: the loaded state, extra environment, the user
-- turns (message and the text persisted for it), whether state.db is there.
data Opts = Opts
  { opLoaded :: Maybe RlmState
  , opEnv :: [(Text, Text)]
  , opTurns :: [(Text, Maybe Text)]
  , opDb :: Bool
  , opRlm :: RlmConfig -> RlmConfig
  -- ^ recall's configuration, from the test's default
  }

-- | The tests written before the offload gap and size conditions keep
-- their scripts: no gap and no least size, so every offload goes ahead.
opts :: Opts
opts = Opts Nothing [] [("Read the files.", Nothing)] False (\c -> c {rcOffloadGap = 0, rcMinOffload = 0})

-- | The agent's own offload conditions (gap 3, a quarter of the room).
gated :: Opts
gated = opts {opRlm = id}

readCall :: Text -> Text -> (Text, Text, PyVal)
readCall i p = (i, "read_file", PDict [("path", PStr p)])

recallCall :: Text -> PyVal -> (Text, Text, PyVal)
recallCall i a = (i, "recall", a)

ask :: Text -> PyVal
ask q = PDict [("question", PStr q)]

toolDef :: Text -> PyVal
toolDef n =
  PDict
    [ ("type", PStr "function")
    , ("function", PDict [("name", PStr n), ("description", PStr n), ("parameters", PDict [("type", PStr "object"), ("properties", PDict [])])]) ]

-- | Run one user turn under the RLM strategy (the loaded state, if any, as a
-- resumed session would give it).
runRlm :: Maybe RlmState -> [Reply] -> IO Out
runRlm loaded = runRlmWith opts {opLoaded = loaded}

runRlmWith :: Opts -> [Reply] -> IO Out
runRlmWith op script = withSystemTempDirectory "fisher-rlm-loop" $ \tmp0 -> do
  tmp <- canonicalizePath tmp0
  let home = tmp </> "home"
      fisher = home </> ".fisher"
  createDirectoryIfMissing True fisher
  offset <- newIORef (0 :: Double)
  uuidN <- newIORef (0 :: Int)
  queue <- newIORef script
  bodies <- newIORef []
  saves <- newIORef []
  moved <- newIORef []
  recalls <- newIORef []
  cuts <- newIORef []
  auxN <- newIORef (0 :: Int)
  rows <- newIORef []
  prompts <- newIORef []
  let loaded = opLoaded op
      db =
        SessionDB
          { dbCreateSession = \_ _ _ _ _ -> pure ()
          , dbEndSession = \_ _ -> pure ()
          , dbUpdateSystemPrompt = \sid p -> modifyIORef prompts (<> [(sid, p)])
          , dbUpdateTokenCounts = \_ _ _ _ -> pure ()
          , dbStoredSystemPrompt = \_ -> pure Nothing
          , dbGetSessionTitle = \_ -> pure Nothing
          , dbSetSessionTitle = \_ _ -> pure ()
          , dbTitles = pure []
          , dbAppendMessage = \sid m -> atomicModifyIORef' rows (\rs -> (rs <> [(sid, m)], ())) }
  let clock = (1790000000 +) <$> readIORef offset
      sleep d = atomicModifyIORef' offset (\o -> (o + d, ()))
      sys =
        (testSys home ([("HOME", T.pack home), ("FISHER_TIMEZONE", "UTC"), ("CONTEXT_COMPRESSION_THRESHOLD", "0.5")] <> opEnv op) (\_ -> pure ()))
          { sysNow = clock, sysSleep = sleep
          , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, T.justifyRight 32 '0' (T.pack (showHex n "")))) }
      rt = Runtime clock sleep (pure 0)
      ok v = pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii v))))
      respond hr
        | hrMethod hr == "GET" =
            ok (PDict [("data", PList [PDict [("id", PStr "openai/gpt-5"), ("context_length", PInt 10000)]])])
        | otherwise = do
            next <- atomicModifyIORef' queue (\case [] -> ([], Nothing); (a : rest) -> (rest, Just a))
            let body = fromMaybe PNull (hrBody hr)
            case next of
              Nothing -> pure (Right (HttpResp 500 [] "{\"error\": {\"message\": \"no scripted answer\"}}"))
              Just (Agent c calls) -> atomicModifyIORef' bodies (\bs -> (bs <> [(True, body)], ())) >> ok (chat c calls agentCost)
              Just (Root code) -> atomicModifyIORef' bodies (\bs -> (bs <> [(False, body)], ())) >> ok (chat ("```repl\n" <> code <> "\n```") [] rootCost)
              Just (Fail st b) -> do
                atomicModifyIORef' bodies (\bs -> (bs <> [(False, body)], ()))
                pure (Right (HttpResp st [] (TE.encodeUtf8 b)))
              Just (AgentFail st b) -> do
                atomicModifyIORef' bodies (\bs -> (bs <> [(True, body)], ()))
                pure (Right (HttpResp st [] (TE.encodeUtf8 b)))
      tr = Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError))
      runner =
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
      -- a block answers with its code, except WAIT, which does not answer
      fakeRepl _ = pure (Right (ReplHandle (\code _ -> pure (BlockResult "" "" [] [] (if T.strip code == "WAIT" then Nothing else Just ("answer " <> T.strip code)))) (pure ())))
      setup =
        RlmSetup
          { rlConfig = opRlm op (defaultRlmConfig {rcModel = "openai/gpt-5"})
          , rlRepl = fakeRepl
          , rlLoad = pure loaded
          , rlSave = \st -> atomicModifyIORef' saves (\ss -> (ss <> [st], ()))
          , rlOnOffload = \o -> atomicModifyIORef' moved (\ms -> (ms <> [ofMoved o], ()))
          , rlOnCut = \ts -> atomicModifyIORef' cuts (\cs -> (cs <> [map stIndex ts], ()))
          , rlOnRecall = \q r -> atomicModifyIORef' recalls (\rs -> (rs <> [(q, rcText r)], ())) }
      cfg =
        defaultAgentConfig
          { cfgModel = "openai/gpt-5", cfgApiKey = "sk-or-test-1234567890", cfgBaseUrl = "https://openrouter.ai/api/v1"
          , cfgBaseUrlGiven = True, cfgProvider = "openrouter", cfgApiMode = ChatCompletions, cfgMaxIterations = 30
          , cfgToolDelay = 0, cfgQuiet = True, cfgSkipContextFiles = True, cfgSkipMemory = True
          , cfgSessionId = Just "20261013_101500_ab12cd", cfgPlatform = Just "cli" }
  cache <- newModelCache
  let env =
        AgentEnv
          { envSys = sys, envRuntime = rt, envTransport = tr
          , envLlm = Env (sysUuidHex sys) (\_ r -> pure ("[The " <> r <> " attached an image.]"))
          , envNowLocal = pure (LocalTime (fromGregorian 2026 10 13) (TimeOfDay 10 15 0))
          , envPromptNow = pure (LocalTime (fromGregorian 2026 10 13) (TimeOfDay 10 15 0))
          , envCwd = pure tmp, envChoice = \_ -> pure 0, envAnimate = False, envDb = if opDb op then Just db else Nothing, envTools = runner
          , envAux = \_ -> atomicModifyIORef' auxN (\n -> (n + 1, ())) >> pure (Left (AuxNoProvider ""))
          , envAuxEnv = Nothing
          , envMeta = MetaEnv tr clock fisher cache (\_ -> pure ())
          , envSkin = defaultSkin, envConfig = PDict [], envCallbacks = noCallbacks
          , envWarn = \_ -> pure (), envPromptHook = pure, envWriteFiles = False
          , envLocalTool = \_ _ act -> act, envInbox = pure [] }
  agent <- newAgent cfg env
  setContextStrategy agent (UseRlm setup)
  let go _ [] acc = pure (reverse acc)
      go hist ((u, persisted) : rest) acc = do
        r <- runConversation agent (runInput u) {riHistory = hist, riPersistUserMessage = persisted}
        let hist' = case r of
              RunDone d -> list "messages" d
              _ -> hist
        go hist' rest (r : acc)
  results <- go [] (opTurns op) []
  let res = last results
  Out <$> readIORef bodies <*> pure res <*> readIORef rows <*> readIORef prompts <*> agentSessionCost agent <*> readIORef saves <*> readIORef moved
    <*> readIORef recalls <*> readIORef auxN <*> agentRlmState agent <*> readIORef cuts

-- | The session that offloads after its fourth tool batch, recalls, then
-- offloads again after its sixth and answers.
mainScript :: [Reply]
mainScript =
  [ Agent "" [readCall "c1" "a"]
  , Agent "" [readCall "c2" "b"]
  , Agent "" [readCall "c3" "c"]
  , Agent "" [readCall "c4" "big"]
  , Agent "" [recallCall "c5" (ask "What did file b say?")]
  , Root "B"
  , Agent "" [readCall "c6" "big"]
  , Agent "done" [] ]

-- | 'mainScript' with a huge output in its sixth batch: the second offload
-- leaves the history over the threshold, so the tail cut stores turn 13.
cutScript :: [Reply]
cutScript =
  [ Agent "" [readCall "c1" "a"]
  , Agent "" [readCall "c2" "b"]
  , Agent "" [readCall "c3" "c"]
  , Agent "" [readCall "c4" "big"]
  , Agent "" [recallCall "c5" (ask "What did file b say?")]
  , Root "B"
  , Agent "" [readCall "c6" "huge"]
  , Agent "done" [] ]

-- | Big results in batches 2 and 4 to 7: the first crossing with a middle
-- is after batch 4, the next allowed one after batch 7.
bigScript :: [Reply]
bigScript =
  [ Agent "" [readCall "c1" "a"]
  , Agent "" [readCall "c2" "big"]
  , Agent "" [readCall "c3" "c"]
  , Agent "" [readCall "c4" "big"]
  , Agent "" [readCall "c5" "big"]
  , Agent "" [readCall "c6" "big"]
  , Agent "" [readCall "c7" "big"]
  , Agent "done" [] ]

agentBodies, recallBodies :: Out -> [PyVal]
agentBodies o = [b | (True, b) <- oBodies o]
recallBodies o = [b | (False, b) <- oBodies o]

messagesOf :: PyVal -> [PyVal]
messagesOf = list "messages"

systemOf :: PyVal -> Text
systemOf b = case messagesOf b of
  (m : _) | get "role" m == Just (PStr "system") -> txt "content" m
  _ -> ""

toolNames :: PyVal -> [Text]
toolNames b = case get "tools" b of
  Just (PList ts) -> [n | t <- ts, Just (PStr n) <- [get "function" t >>= get "name"]]
  _ -> []

-- | The tool results in the messages of a request, by call id.
toolResult :: Text -> PyVal -> Maybe Text
toolResult i b = case [txt "content" m | m <- messagesOf b, get "role" m == Just (PStr "tool"), get "tool_call_id" m == Just (PStr i)] of
  (t : _) -> Just t
  [] -> Nothing

resultDict :: Out -> PyVal
resultDict o = case oResult o of
  RunDone d -> d
  RunRaised c m -> PDict [("raised", PStr c), ("message", PStr m)]

-- | A row's role and its call id (assistant, tool) or text (user, answer).
rowKey :: PyVal -> (Text, Text)
rowKey m = case (txt "role" m, list "tool_calls" m) of
  ("assistant", c : _) -> ("assistant", txt "id" c)
  ("tool", _) -> ("tool", txt "tool_call_id" m)
  (r, _) -> (r, txt "content" m)

-- | The rows of 'mainScript''s session.
mainKeys :: [(Text, Text)]
mainKeys =
  [("user", "Read the files.")]
    <> concat [[("assistant", c), ("tool", c)] | c <- ["c1", "c2", "c3", "c4", "c5", "c6"]]
    <> [("assistant", "done")]

-- | A loaded state with one stored turn.
oneTurn :: RlmState
oneTurn = resumed [StoredTurn 1 "user" Nothing (PDict [("role", PStr "user"), ("content", PStr "old")])]

-- | A state as a resumed session would load it: one offload, the given store.
resumed :: [StoredTurn] -> RlmState
resumed store = RlmState store 1 (if null store then Nothing else Just (1, length store))

spec :: Spec
spec = describe "the agent loop under RLM" $ do
  it "offers neither recall nor the prompt section before the first offload" $ do
    o <- runRlm Nothing mainScript
    let early = take 4 (agentBodies o)
    length early `shouldBe` 4
    mapM_ (\b -> toolNames b `shouldBe` ["read_file"]) early
    mapM_ (\b -> (promptSection `T.isInfixOf` systemOf b) `shouldBe` False) early
  it "offloads after a tool batch over the threshold: head, stub, tail; recall and the section from then on; no auxiliary call" $ do
    o <- runRlm Nothing mainScript
    let bs = agentBodies o
        b4 = bs !! 3
        b5 = bs !! 4
        m4 = drop 1 (messagesOf b4)
        m5 = drop 1 (messagesOf b5)
    length m5 `shouldBe` 8
    take 3 m5 `shouldBe` take 3 m4
    isStub (m5 !! 3) `shouldBe` True
    ("Turns 4 to 5 of this session (2 messages, " `T.isPrefixOf` txt "content" (m5 !! 3)) `shouldBe` True
    take 2 (drop 4 m5) `shouldBe` drop 5 m4
    map (txt "role") (drop 6 m5) `shouldBe` ["assistant", "tool"]
    get "tool_call_id" (m5 !! 7) `shouldBe` Just (PStr "c4")
    toolNames b5 `shouldBe` ["read_file", "recall"]
    (("\n\n" <> promptSection) `T.isSuffixOf` systemOf b5) `shouldBe` True
    oAux o `shouldBe` 0
    oMoved o `shouldBe` [(4, 5), (6, 9)]
    get "completed" (resultDict o) `shouldBe` Just (PBool True)
  it "runs recall: the fake RLM's answer is the tool result, its cost is in the session's, its bodies carry no tools" $ do
    o <- runRlm Nothing mainScript
    let bs = agentBodies o
    toolResult "c5" (bs !! 5) `shouldBe` Just "answer B"
    oRecalls o `shouldBe` [("What did file b say?", "answer B")]
    length (recallBodies o) `shouldBe` 1
    mapM_ (\b -> get "tools" b `shouldSatisfy` isNothing) (recallBodies o)
    let (dollars, cr, cw) = oCost o
    fmap (\d -> round (d * 1000) :: Integer) dollars `shouldBe` Just (round ((7 * agentCost + rootCost) * 1000))
    (cr, cw) `shouldBe` (8 * 7, 8 * 3)
  it "saves the state after each offload" $ do
    o <- runRlm Nothing mainScript
    map rsOffloads (oSaves o) `shouldBe` [1, 2]
    fmap Just (lastMaybe (oSaves o)) `shouldBe` Just (oState o)
    fmap (map stIndex . rsStore) (oState o) `shouldBe` Just [4, 5, 6, 7, 8, 9]
  it "runs two recalls in one batch: both results arrive and both costs count" $ do
    o <- runRlm (Just oneTurn)
      [ Agent "" [recallCall "r1" (ask "first?"), recallCall "r2" (ask "second?")]
      , Root "X"
      , Root "Y"
      , Agent "done" [] ]
    let b2 = agentBodies o !! 1
    sort [toolResult "r1" b2, toolResult "r2" b2] `shouldBe` [Just "answer X", Just "answer Y"]
    length (oRecalls o) `shouldBe` 2
    let (dollars, _, _) = oCost o
    fmap (\d -> round (d * 1000) :: Integer) dollars `shouldBe` Just (round ((2 * agentCost + 2 * rootCost) * 1000))
  it "does not offer recall over an empty store but answers a call to it, and refuses a call with no question" $ do
    o0 <- runRlm (Just (resumed []))
      [ Agent "" [recallCall "e1" (ask "anything?")]
      , Agent "done" [] ]
    let bs0 = agentBodies o0
    toolNames (head bs0) `shouldBe` ["read_file"]
    (("\n\n" <> promptSection) `T.isSuffixOf` systemOf (head bs0)) `shouldBe` False
    toolResult "e1" (bs0 !! 1) `shouldBe` Just emptyStoreText
    recallBodies o0 `shouldBe` []
    o <- runRlm (Just oneTurn)
      [ Agent "" [recallCall "e2" (PDict [])]
      , Agent "" [recallCall "e3" (PDict [("question", PInt 3)])]
      , Agent "done" [] ]
    let bs = agentBodies o
    toolNames (head bs) `shouldBe` ["read_file", "recall"]
    toolResult "e2" (bs !! 1) `shouldBe` Just "Error: recall needs a question (a string)."
    toolResult "e3" (bs !! 2) `shouldBe` Just "Error: recall needs a question (a string)."
    recallBodies o `shouldBe` []
  it "stops the session when recall's model call gets a 402" $ do
    o <- runRlm (Just oneTurn)
      [ Agent "" [recallCall "n1" (ask "anything?")]
      , Fail 402 "{\"error\": {\"message\": \"Insufficient credits\", \"code\": 402}}"
      , Agent "never" [] ]
    let d = resultDict o
    get "completed" d `shouldBe` Just (PBool False)
    get "failed" d `shouldBe` Just (PBool True)
    get "final_response" d `shouldBe` Just PNull
    get "api_calls" d `shouldBe` Just (PInt 1)
    get "messages" d `shouldBe` Just (PList [PDict [("role", PStr "user"), ("content", PStr "Read the files.")]])
    get "error" d `shouldBe` Just (PStr "Error code: 402 (recall's model call: the key's credit limit is reached)")
    length (agentBodies o) `shouldBe` 1
  it "offers recall and the section after a cut of the tail alone (no middle to move)" $ do
    o <- runRlmWith opts {opDb = True}
      [ Agent "" [readCall "h1" "huge"]
      , Agent "" [recallCall "h2" (ask "What was in the file?")]
      , Root "H"
      , Agent "done" [] ]
    let bs = agentBodies o
        b2 = bs !! 1
    toolNames (head bs) `shouldBe` ["read_file"]
    toolNames b2 `shouldBe` ["read_file", "recall"]
    (("\n\n" <> promptSection) `T.isSuffixOf` systemOf b2) `shouldBe` True
    let preview = fromMaybe "" (toolResult "h1" b2)
    (T.take 2000 hugeResult `T.isPrefixOf` preview) `shouldBe` True
    ("the full output is in recall's store as turn 3)" `T.isSuffixOf` preview) `shouldBe` True
    toolResult "h2" (bs !! 2) `shouldBe` Just "answer H"
    oMoved o `shouldBe` []
    oCuts o `shouldBe` [[3]]
    map (\st -> (rsOffloads st, map stIndex (rsStore st))) (oSaves o) `shouldBe` [(0, [3])]
    -- state.db has the full output, and the prompt with the section
    [txt "content" m | (_, m) <- oRows o, get "tool_call_id" m == Just (PStr "h1")] `shouldBe` [hugeResult]
    (("\n\n" <> promptSection) `T.isSuffixOf` maybe "" snd (lastMaybe (oPrompts o))) `shouldBe` True
  it "offloads when compression.enabled is off" $ do
    o <- runRlmWith opts {opEnv = [("CONTEXT_COMPRESSION_ENABLED", "false")]} mainScript
    oMoved o `shouldBe` [(4, 5), (6, 9)]
    get "completed" (resultDict o) `shouldBe` Just (PBool True)
  it "offloads on a 413 and retries" $ do
    o <- runRlm Nothing
      [ Agent "" [readCall "c1" "a"]
      , Agent "" [readCall "c2" "b"]
      , Agent "" [readCall "c3" "c"]
      , Agent "" [readCall "c4" "d"]
      , AgentFail 413 "{\"error\": {\"message\": \"Request entity too large\"}}"
      , Agent "done" [] ]
    oMoved o `shouldBe` [(4, 5)]
    let retried = drop 1 (messagesOf (last (agentBodies o)))
    length retried `shouldBe` 8
    isStub (retried !! 3) `shouldBe` True
    get "completed" (resultDict o) `shouldBe` Just (PBool True)
  it "keeps the full transcript in state.db across offloads: each message once, in order, no stub" $ do
    o <- runRlmWith opts {opDb = True} mainScript
    let rs = oRows o
    map fst rs `shouldSatisfy` all (== "20261013_101500_ab12cd")
    map (rowKey . snd) rs `shouldBe` mainKeys
    filter (isStub . snd) rs `shouldBe` []
    [txt "content" m | (_, m) <- rs, get "tool_call_id" m `elem` [Just (PStr "c4"), Just (PStr "c6")]] `shouldBe` [bigResult, bigResult]
  it "keeps the transcript over a second turn, and the persisted user message lands on its own row" $ do
    o <- runRlmWith opts {opDb = True, opTurns = [("Read the files.", Nothing), ("Read more.", Just "persisted turn two")]}
      (mainScript <> [Agent "" [readCall "c8" "big"], Agent "done2" []])
    let rs = oRows o
    map (rowKey . snd) rs `shouldBe` mainKeys <> [("user", "persisted turn two"), ("assistant", "c8"), ("tool", "c8"), ("assistant", "done2")]
    filter (isStub . snd) rs `shouldBe` []
    length (oMoved o) `shouldSatisfy` (>= 3)
    let live = case oResult o of
          RunDone d -> list "messages" d
          _ -> []
    length (filter isStub live) `shouldBe` 1
    [txt "content" m | m <- live, txt "role" m == "user", not (isStub m)] `shouldBe` ["Read the files.", "persisted turn two"]
  it "rebuilds the live history on resume from the saved store and state.db: two offloads and a tail cut" $ do
    o <- runRlmWith opts {opDb = True} cutScript
    oMoved o `shouldBe` [(4, 5), (6, 9)]
    oCuts o `shouldBe` [[13]]
    let saved = lastMaybe (oSaves o) >>= stateFromPy . stateToPy
        live = list "messages" (resultDict o)
    fmap (map stIndex . rsStore) saved `shouldBe` Just [4, 5, 6, 7, 8, 9, 13]
    fmap rsRange saved `shouldBe` Just (Just (4, 9))
    -- the cut output is a preview in the live history, whole in state.db
    [T.length (txt "content" m) < 3000 | m <- live, get "tool_call_id" m == Just (PStr "c6")] `shouldBe` [True]
    [txt "content" m | (_, m) <- oRows o, get "tool_call_id" m == Just (PStr "c6")] `shouldBe` [hugeResult]
    fmap (\st -> liveHistory st (map snd (oRows o))) saved `shouldBe` Just live
  it "keeps state.db in line with the returned history after a rollback" $ do
    o <- runRlmWith opts {opDb = True, opTurns = [("Read the files.", Nothing), ("Read more.", Nothing)]}
      [ Agent "" [readCall "c1" "a"]
      , Agent "<REASONING_SCRATCHPAD>one" []
      , Agent "<REASONING_SCRATCHPAD>two" []
      , Agent "<REASONING_SCRATCHPAD>three" []
      , Agent "done2" [] ]
    let live = list "messages" (resultDict o)
    map rowKey live `shouldBe` [("user", "Read the files."), ("user", "Read more."), ("assistant", "done2")]
    map (rowKey . snd) (oRows o) `shouldBe` map rowKey live
  describe "the offload conditions (gap and size)" $ do
    it "does not offload over the threshold with fewer than offload_gap batches since the last offload, but still cuts the tail" $ do
      o <- runRlmWith gated {opRlm = \c -> c {rcMinOffload = 0}} cutScript
      oMoved o `shouldBe` [(4, 5)]
      oCuts o `shouldBe` [[13]]
      map (\st -> (rsOffloads st, map stIndex (rsStore st))) (oSaves o) `shouldBe` [(1, [4, 5]), (1, [4, 5, 13])]
      let live = list "messages" (resultDict o)
      [T.length (txt "content" m) < 3000 | m <- live, get "tool_call_id" m == Just (PStr "c6")] `shouldBe` [True]
      get "completed" (resultDict o) `shouldBe` Just (PBool True)
    it "cuts a huge tail result with no middle to move under the default conditions" $ do
      -- the huge result is in the tail, outside the head, so the room is
      -- positive and the empty middle fails the size test
      o <- runRlmWith gated
        [ Agent "" [readCall "h1" "a"]
        , Agent "" [readCall "h2" "b"]
        , Agent "" [readCall "h3" "huge"]
        , Agent "" [recallCall "h4" (ask "What was in the file?")]
        , Root "H"
        , Agent "done" [] ]
      let bs = agentBodies o
      oMoved o `shouldBe` []
      oCuts o `shouldBe` [[7]]
      toolNames (bs !! 3) `shouldBe` ["read_file", "recall"]
      ("the full output is in recall's store as turn 7)" `T.isSuffixOf` fromMaybe "" (toolResult "h3" (bs !! 3))) `shouldBe` True
      toolResult "h4" (bs !! 4) `shouldBe` Just "answer H"
    it "does not offload over the threshold when the middle is under min_offload of the room" $ do
      o <- runRlmWith gated
        [ Agent "" [readCall "c1" "a"]
        , Agent "" [readCall "c2" "b"]
        , Agent "" [readCall "c3" "c"]
        , Agent "" [readCall "c4" "big"]
        , Agent "done" [] ]
      oMoved o `shouldBe` []
      oCuts o `shouldBe` []
      oSaves o `shouldBe` []
      get "completed" (resultDict o) `shouldBe` Just (PBool True)
    it "offloads when both are met, then waits offload_gap batches again" $ do
      o <- runRlmWith gated bigScript
      oMoved o `shouldBe` [(4, 5), (6, 11)]
      map rsOffloads (oSaves o) `shouldBe` [1, 2]
      get "completed" (resultDict o) `shouldBe` Just (PBool True)
    it "offloads on a 413 whatever the gap and the size" $ do
      o <- runRlmWith gated
        [ Agent "" [readCall "c1" "a"]
        , Agent "" [readCall "c2" "big"]
        , Agent "" [readCall "c3" "c"]
        , Agent "" [readCall "c4" "big"]
        , Agent "" [readCall "c5" "a"]
        , AgentFail 413 "{\"error\": {\"message\": \"Request entity too large\"}}"
        , Agent "done" [] ]
      oMoved o `shouldBe` [(4, 5), (6, 7)]
      get "completed" (resultDict o) `shouldBe` Just (PBool True)
    it "offloads on a context-length error whatever the gap and the size" $ do
      o <- runRlmWith gated
        [ Agent "" [readCall "c1" "a"]
        , Agent "" [readCall "c2" "big"]
        , Agent "" [readCall "c3" "c"]
        , Agent "" [readCall "c4" "big"]
        , Agent "" [readCall "c5" "a"]
        , AgentFail 400 "{\"error\": {\"message\": \"This request exceeds the maximum context length of the model\"}}"
        , Agent "done" [] ]
      oMoved o `shouldBe` [(4, 5), (6, 7)]
      get "completed" (resultDict o) `shouldBe` Just (PBool True)
    it "lets a fresh session and a resumed one offload at the first crossing" $ do
      let turns = [("one", Nothing), ("two", Nothing), ("three", Nothing), ("four", Nothing)]
          script = [Agent "1" [], Agent bigResult [], Agent "3" [], Agent "" [readCall "c1" "big"], Agent "done" []]
      fresh <- runRlmWith gated {opTurns = turns} script
      oMoved fresh `shouldBe` [(4, 5)]
      again <- runRlmWith gated {opTurns = turns, opLoaded = Just oneTurn} script
      oMoved again `shouldBe` [(4, 5)]
  describe "the offload keys" $ do
    it "defaults to a quarter of the room and a gap of 3 batches" $ do
      fmap (\c -> (rcMinOffload c, rcOffloadGap c)) (agentRlmConfig "x/agent" (PDict [])) `shouldBe` Right (0.25, 3)
    it "refuses min_offload outside (0, 1] and a negative offload_gap" $ do
      let refuse k v = either id (const "") (agentRlmConfig "x/agent" (PDict [("fisher", PDict [("rlm", PDict [(k, v)])])]))
      refuse "min_offload" (PInt 0) `shouldBe` "fisher.rlm: min_offload must be a number in (0, 1]"
      refuse "min_offload" (PFloat 1.5) `shouldBe` "fisher.rlm: min_offload must be a number in (0, 1]"
      refuse "min_offload" (PStr "x") `shouldBe` "fisher.rlm: min_offload must be a number in (0, 1]"
      refuse "offload_gap" (PInt (-1)) `shouldBe` "fisher.rlm: offload_gap must be a non-negative integer"
  describe "recall's budget" $
    it "stops a recall whose next call would pass max_budget, with its partial answer, inside the budget" $ do
      o <- runRlmWith opts {opLoaded = Just oneTurn, opRlm = \c -> (opRlm opts c) {rcMaxBudget = Just 0.3}}
        [ Agent "" [recallCall "b1" (ask "anything?")]
        , Root "WAIT"
        , Agent "done" [] ]
      length (recallBodies o) `shouldBe` 1
      let text = maybe "" snd (lastMaybe (oRecalls o))
      ("\n\n(recall stopped: budget)" `T.isSuffixOf` text) `shouldBe` True
      ("WAIT" `T.isInfixOf` text) `shouldBe` True
      let (dollars, _, _) = oCost o
      fmap (\d -> round (d * 1000) :: Integer) dollars `shouldBe` Just (round ((2 * agentCost + rootCost) * 1000))
      get "completed" (resultDict o) `shouldBe` Just (PBool True)
  where
    lastMaybe xs = if null xs then Nothing else Just (last xs)
