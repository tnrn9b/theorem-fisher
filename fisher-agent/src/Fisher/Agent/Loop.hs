-- | The agent loop (@AIAgent@ in @run_agent.py@): build the request from
-- the conversation, call the model, run the tools it asks for, and go
-- round until it answers, with upstream's retries, compression, budget
-- warnings, interrupts and persistence.
module Fisher.Agent.Loop
  ( AgentConfig (..)
  , defaultAgentConfig
  , Callbacks (..)
  , noCallbacks
  , ToolRunner (..)
  , AgentEnv (..)
  , Agent
  , agentConfig
  , agentSessionId
  , agentTools
  , agentValidTools
  , agentSessionCounters
  , agentSessionCost
  , batchWritesSameFile
  , responseRefusal
  , hasTextCall
  , agentCachedPrompt
  , newAgent
  , RunInput (..)
  , runInput
  , AgentResult (..)
  , runConversation
  , flushMemories
  , agentTodoStore
  , agentMemoryStore
  , interruptAgent
  , clearInterrupt
  , isInterrupted
    -- * Pieces, for the specs
  , repairToolName
  , budgetWarning
  , stripThink
  , hasContentAfterThink
  , withUnportedToolsets
    -- * Context strategy (docs/clm.md)
  , setContextStrategy
  , ContextSetup (..)
  , agentClmState
  , ContextRun (..)
  , clmOf
    -- * The RLM strategy (spec phase 2)
  , RlmSetup (..)
  , RlmRun (..)
  , agentRlmState
  , RecallNoCredit (..)
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem
import Control.Exception (Exception, SomeException, bracket_, displayException, fromException, throwIO, try)
import Control.Monad (forM, forM_, join, unless, void, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (nub, sort, sortOn, tails)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime, defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath ((</>))

import Fisher.Agent.Aux (AuxEnv, Client (..), ClientKind (..), resolveProviderClient)
import Fisher.Agent.Compress
import Fisher.Agent.Context.Clm
import Fisher.Agent.Context.ClmStore (Item (..), Store (..), archiveRewritten, emptyStore, readStored, readStoredSchema, renderItem, storePromptSection)
import Fisher.Agent.Context.Budget (mentionsContext, promptSection, samePath)
import Fisher.Agent.Context.Mirror (Turn (..), headerIds, render)
import Fisher.Agent.Context.Recall (RecallEnv (..), RecallResult (..), recallSettings, recallTexts)
import Fisher.Agent.Context.Rlm (Offload (..), RlmState (..), StoredTurn (..), cutTail, emptyRlmState, isStub, offload, recallSchema, renderTurn)
import qualified Fisher.Agent.Context.Rlm as Rlm
import Fisher.Agent.Context.RlmModels (liveModels)
import Fisher.Agent.Display
import Fisher.Agent.Metadata
import Fisher.Agent.Prompt
import Fisher.Agent.Session
import Fisher.Agent.Tools.Memory (MemoryStore, loadFromDisk, frozenSnapshot, memoryToolArgs, newMemoryStore)
import Fisher.Agent.Tools.Todo (PyError (..), TodoStore, formatForInjection, hydrateTodo, newTodoStore, todoHasItems, todoToolArgs)
import qualified Fisher.Agent.Tools.Todo as Todo
import Fisher.Agent.Types
import Fisher.LLM.Client (CallResult (..), callModel)
import Fisher.LLM.Errors (LLMError (..), errorClass, errorText)
import Fisher.LLM.Chat (chatHeaders, supportsReasoningExtraBody)
import Fisher.LLM.Http
import Fisher.LLM.Normalize (Outcome (..), Usage (..), extractReasoning, normalizeAnthropic, normalizeChat)
import Fisher.LLM.Prepare (PrepareInput (..), prepareMessages, usesPromptCaching)
import Fisher.LLM.Request (buildRequest)
import Fisher.LLM.Sys (Sys (..))
import Fisher.LLM.Types
import qualified Fisher.Py.Difflib as D
import Fisher.Py.JsonError (jsonError)
import qualified Fisher.Py.Re as Re
import Fisher.Py.Value
import Fisher.Tools.Py (pyInt)
import Fisher.RLM.Config (RlmConfig (..))
import Fisher.RLM.Repl (ReplFactory)
import qualified Fisher.RLM.Types as R

-- ---------------------------------------------------------------- config

-- | @AIAgent.__init__@'s arguments.
data AgentConfig = AgentConfig
  { cfgModel :: Text
  , cfgApiKey :: Text
  , cfgBaseUrl :: Text
  , cfgBaseUrlGiven :: Bool
  , cfgProvider :: Text
  , cfgApiMode :: ApiMode
  , cfgMaxIterations :: Int
  , cfgToolDelay :: Double
  , cfgEnabledToolsets :: Maybe [Text]
  , cfgDisabledToolsets :: Maybe [Text]
  , cfgVerbose :: Bool
  , cfgQuiet :: Bool
  , cfgEphemeral :: Maybe Text
  , cfgPrefill :: [PyVal]
  , cfgReasoning :: Maybe PyVal
  , cfgMaxTokens :: Maybe Integer
  , cfgRouting :: Routing
  , cfgSessionId :: Maybe Text
  , cfgPlatform :: Maybe Text
  , cfgPassSessionId :: Bool
  , cfgSkipContextFiles :: Bool
  , cfgSkipMemory :: Bool
  , cfgFallback :: Maybe (Text, Text)
  -- ^ provider, model
  , cfgLogPrefix :: Text
  , cfgLogPrefixChars :: Int
  }

defaultAgentConfig :: AgentConfig
defaultAgentConfig =
  AgentConfig
    { cfgModel = "anthropic/claude-opus-4.6", cfgApiKey = "", cfgBaseUrl = "https://openrouter.ai/api/v1", cfgBaseUrlGiven = False
    , cfgProvider = "openrouter", cfgApiMode = ChatCompletions, cfgMaxIterations = 90, cfgToolDelay = 1.0
    , cfgEnabledToolsets = Nothing, cfgDisabledToolsets = Nothing, cfgVerbose = False, cfgQuiet = False
    , cfgEphemeral = Nothing, cfgPrefill = [], cfgReasoning = Nothing, cfgMaxTokens = Nothing, cfgRouting = noRouting
    , cfgSessionId = Nothing, cfgPlatform = Nothing, cfgPassSessionId = False, cfgSkipContextFiles = False
    , cfgSkipMemory = False, cfgFallback = Nothing, cfgLogPrefix = "", cfgLogPrefixChars = 100 }

data Callbacks = Callbacks
  { cbThinking :: Maybe (Text -> IO ())
  , cbToolProgress :: Maybe (Text -> Maybe PyVal -> PyVal -> IO ())
  -- ^ name, preview, arguments
  , cbStep :: Maybe (Int -> [Text] -> IO ())
  , cbToolDone :: Maybe (Text -> Text -> Text -> PyVal -> Either Text Text -> IO ())
  -- ^ Fisher only: each call as it returns: the task id it ran under, the
  -- call's id, name, arguments, and its result (Left: what it raised)
  }

noCallbacks :: Callbacks
noCallbacks = Callbacks {cbThinking = Nothing, cbToolProgress = Nothing, cbStep = Nothing, cbToolDone = Nothing}

-- | The tool layer as the loop sees it.
data ToolRunner = ToolRunner
  { trDefinitions :: [PyVal]
  -- ^ the schemas the model is offered
  , trPrinted :: [Text]
  -- ^ what building them printed (shown when not quiet)
  , trCall :: Text -> PyVal -> Text -> Maybe [Text] -> IO Text
  -- ^ @handle_function_call(name, args, task_id, enabled_tools)@
  , trEmoji :: Text -> Maybe Text
  , trReadFilesSummary :: Text -> IO [(Text, [Text])]
  , trCleanup :: Text -> IO ()
  -- ^ @cleanup_vm(task_id)@
  , trSetInterrupt :: Bool -> IO ()
  , trRequirements :: IO [(Text, Bool)]
  }

-- | Everything outside the loop.
data AgentEnv = AgentEnv
  { envSys :: Sys
  , envRuntime :: Runtime
  , envTransport :: Transport
  , envLlm :: Env
  , envNowLocal :: IO LocalTime
  -- ^ @datetime.now()@
  , envPromptNow :: IO LocalTime
  -- ^ @gauss_time.now()@
  , envCwd :: IO FilePath
  , envChoice :: Int -> IO Int
  -- ^ @random.choice@ over n items: the index
  , envAnimate :: Bool
  , envDb :: Maybe SessionDB
  , envTools :: ToolRunner
  , envAux :: AuxLLM
  , envAuxEnv :: Maybe AuxEnv
  , envMeta :: MetaEnv
  , envSkin :: Skin
  , envConfig :: PyVal
  , envCallbacks :: Callbacks
  , envWarn :: Text -> IO ()
  , envPromptHook :: Text -> IO Text
  -- ^ sees each system prompt as it is built (the replay log records it;
  -- a replay hands back the recorded one)
  , envWriteFiles :: Bool
  -- ^ the session log, request dumps and context-length cache (off in a
  -- replay)
  , envLocalTool :: Text -> PyVal -> IO Text -> IO Text
  -- ^ wraps each tool the loop runs itself (todo, memory): the replay log
  -- records the result; a replay hands back the recorded one
  , envInbox :: IO [Text]
  -- ^ Fisher only: messages sent to the running session (the UI's inbox),
  -- taken at each tool boundary and sent as the next user turns; the
  -- replay log records them, a replay hands them back at the same boundary
  }

-- ---------------------------------------------------------------- state

data Agent = Agent
  { agCfg :: AgentConfig
  , agEnv :: AgentEnv
  , agSettings :: IORef Settings
  , agStripHeaders :: IORef Bool
  , agTools :: [PyVal]
  , agValid :: [Text]
  , agCaching :: IORef Bool
  , agSessionId :: IORef Text
  , agSessionStart :: LocalTime
  , agLogFile :: FilePath
  , agCachedPrompt :: IORef (Maybe Text)
  , agComp :: IORef Compressor
  , agCompressionEnabled :: Bool
  , agMemory :: Maybe (Bool, Bool, Int, Int)
  , agMemStore :: Maybe MemoryStore
  , agTodo :: TodoStore
  , agNudgeInterval :: Int
  , agFlushMinTurns :: Int
  , agTurnsSinceMemory :: IORef Int
  , agInterrupt :: IORef (Bool, Maybe Text)
  , agSessionMessages :: IORef [PyVal]
  , agLastFlushed :: IORef Int
  , agUserTurns :: IORef Int
  , agFallback :: IORef (Maybe (Text, Text))
  , agFallbackActivated :: IORef Bool
  , agCounters :: IORef (Integer, Integer, Integer, Int)
  -- ^ session prompt, completion, total tokens; API calls with usage
  , agPersistIdx :: IORef (Maybe Int)
  , agPersistOverride :: IORef (Maybe Text)
  , agPreviewed :: IORef Bool
  , agInvalidTool :: IORef Int
  , agInvalidJson :: IORef Int
  , agEmpty :: IORef Int
  , agScratch :: IORef Int
  , agCodexIncomplete :: IORef Int
  , agLastContentWithTools :: IORef (Maybe Text)
  , agBudget :: IORef (Int, Int)
  -- ^ used, max
  , agContext :: IORef ContextRun
  -- ^ the context strategy in force (CompressRun: upstream's compressor)
  , agCost :: IORef SessionCost
  -- ^ Fisher only: dollars and cache tokens over the session's model calls
  , agRefused :: IORef Int
  -- ^ Fisher only: provider refusals in a row
  , agTextCalls :: IORef Int
  -- ^ Fisher only: replies under CLM sent back for a call written as text
  }

-- | Fisher only: whether a reply has a line that is a call in the
-- context file's notation, @[call NAME ARGUMENTS]@ (docs/clm.md section
-- 4), which the model sometimes writes instead of calling.
hasTextCall :: Text -> Bool
hasTextCall t = any isCall (T.lines t)
  where
    isCall l0 =
      let l = T.strip l0
       in case T.stripPrefix "[call " l of
            Just rest ->
              let (name, args) = T.break (== ' ') rest
               in not (T.null name) && T.all (\c -> c == '_' || c == '-' || c == '.' || isAsciiAlphaNum c) name
                    && "]" `T.isSuffixOf` args && T.length args > 2
            Nothing -> False
    isAsciiAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

textCallNote :: Text
textCallNote =
  "[Your reply has a call written as text (a line like `[call read_file {...}]`), so nothing ran. "
    <> "That notation is only how the context file shows past calls. Make the call as a tool call, "
    <> "or give your final answer without it.]"

-- | Fisher only: whether a batch of calls (name, parsed arguments) must
-- run in order: it has a @write_file@ or @patch@ and any other call
-- (a shell, an MCP tool or another file tool may build, check or read what
-- was written), or two calls on one file of which one writes (relative
-- paths are taken against @cwd@).
batchWritesSameFile :: FilePath -> [(Text, PyVal)] -> Bool
batchWritesSameFile cwd calls =
  (length calls > 1 && any ((`elem` ["write_file", "patch"]) . fst) calls) ||
  let fileCall (name, a) = case a of
        PDict _
          | name `elem` ["read_file", "write_file", "patch", "search_files"]
          , Just (PStr p) <- get "path" a ->
              Just (if "/" `T.isPrefixOf` p then p else T.pack cwd <> "/" <> p, name `elem` ["write_file", "patch"])
        _ -> Nothing
      fs = mapMaybe fileCall calls
   in or [samePath x y && (w1 || w2) | ((x, w1) : rest) <- tails fs, (y, w2) <- rest]

-- | Fisher only: what the session's model calls cost, as the provider
-- reported it (OpenRouter's @usage.cost@ and prompt token details).
data SessionCost = SessionCost
  { scDollars :: Double
  , scPriced :: Int
  -- ^ calls that reported a cost
  , scCalls :: Int
  -- ^ calls that reported usage
  , scCacheRead :: Integer
  , scCacheWrite :: Integer
  }
  deriving (Eq, Show)

-- | The session's cost; 'Nothing' for the dollars unless every call
-- with usage reported one.
agentSessionCost :: Agent -> IO (Maybe Double, Integer, Integer)
agentSessionCost ag = do
  c <- readIORef (agCost ag)
  pure (if scCalls c > 0 && scPriced c == scCalls c then Just (scDollars c) else Nothing, scCacheRead c, scCacheWrite c)

-- | CLM in a running agent: its setup, its state, and how many messages
-- the pinned prefix holds.
data ClmRun = ClmRun
  { crSetup :: ClmSetup
  , crState :: IORef ClmState
  , crPinned :: IORef Int
  , crTranscript :: IORef [PyVal]
  -- ^ every message of this process's runs as it was first made, for
  -- @state.db@ (section 9)
  , crFlushed :: IORef Int
  -- ^ how many of them are in @state.db@
  , crStore :: Maybe (IORef Store)
  -- ^ the store, when @fisher.clm.store@ is on
  , crOrigin :: IORef (M.Map Int Int)
  -- ^ CLM turn id to the transcript position of its original (kept with
  -- the store on only)
  }

-- | The RLM strategy's setup (spec phase 2): the recall configuration and
-- REPL, where the state is loaded from and saved to, and the replay log's
-- hooks.
data RlmSetup = RlmSetup
  { rlConfig :: RlmConfig
  , rlRepl :: ReplFactory
  , rlLoad :: IO (Maybe RlmState)
  , rlSave :: RlmState -> IO ()
  , rlOnOffload :: Offload -> IO ()
  -- ^ the replay log (each offload)
  , rlOnCut :: [StoredTurn] -> IO ()
  -- ^ the replay log (each tail cut): the turns it stored
  , rlOnRecall :: Text -> RecallResult -> IO ()
  -- ^ each recall: the question and the result
  }

-- | RLM in a running agent: its setup and its state.
data RlmRun = RlmRun
  { rrSetup :: RlmSetup
  , rrState :: IORef RlmState
  , rrSinceOffload :: IORef Int
  -- ^ tool batches since the last offload that moved turns; starts at the
  -- gap, so a new or resumed session may offload at its first crossing
  }

-- | Recall's model call got a 402: the session stops, as for any 402.
newtype RecallNoCredit = RecallNoCredit Text
  deriving (Show)

instance Exception RecallNoCredit

-- | A loaded store without its transcript positions.
forgetOrigins :: Store -> Store
forgetOrigins s = s {sOrigins = S.empty, sItems = [i {itOrigin = Nothing} | i <- sItems s]}

-- | Which context strategy the agent's next runs use.
data ContextSetup = UseCompress | UseClm ClmSetup | UseRlm RlmSetup

-- | Choose the context strategy for the agent's next runs.
setContextStrategy :: Agent -> ContextSetup -> IO ()
setContextStrategy ag c = do
  case c of
    UseCompress -> writeIORef (agContext ag) CompressRun
    UseClm setup -> do
      loaded <- csLoad setup
      st <- newIORef (fromMaybe newClmState loaded)
      p <- newIORef 0
      tr <- newIORef []
      fl <- newIORef 0
      store <-
        if ccStore (csConfig setup)
          -- a loaded store's positions are of an earlier process's
          -- transcript: this one starts again at 0 (spec 4: the archive
          -- covers turns made after the resume), so neither the store's
          -- positions nor its items' origins carry over (a rollback note
          -- names only this process's items)
          then Just <$> (newIORef . maybe emptyStore forgetOrigins =<< csStoreLoad setup)
          else pure Nothing
      origin <- newIORef M.empty
      writeIORef (agContext ag) (ClmRunning (ClmRun setup st p tr fl store origin))
    UseRlm setup -> do
      loaded <- rlLoad setup
      st <- newIORef (fromMaybe emptyRlmState loaded)
      since <- newIORef (rcOffloadGap (rlConfig setup))
      writeIORef (agContext ag) (RlmRunning (RlmRun setup st since))
  syncRecallTool ag

-- | The context strategy in force in a running agent.
data ContextRun = CompressRun | ClmRunning ClmRun | RlmRunning RlmRun

-- | The CLM run, when CLM is the strategy.
clmOf :: ContextRun -> Maybe ClmRun
clmOf = \case
  ClmRunning r -> Just r
  _ -> Nothing

-- | The RLM run, when RLM is the strategy.
rlmOf :: ContextRun -> Maybe RlmRun
rlmOf = \case
  RlmRunning r -> Just r
  _ -> Nothing

-- | The RLM state, when RLM is the strategy.
agentRlmState :: Agent -> IO (Maybe RlmState)
agentRlmState ag = (rlmOf <$> readIORef (agContext ag)) >>= traverse (readIORef . rrState)

isRlm :: Agent -> IO Bool
isRlm ag = isJust . rlmOf <$> readIORef (agContext ag)

-- | Whether recall is offered: under RLM once the store holds a turn
-- (after the first offload, or a cut of the tail's results: spec 3.3 and
-- 3.4).
recallOffered :: Agent -> IO Bool
recallOffered ag = maybe False (not . null . rsStore) <$> agentRlmState ag

-- | The store's tools when a store is on: the names the tool-name check
-- accepts, and the schemas offered now. Under RLM, recall (offered once
-- the store holds a turn); under CLM with the store, read_stored and, with
-- recall's setup, recall (offered once the store holds an item: spec 3.3).
-- Plain CLM and Compress have none.
storeTools :: Agent -> IO ([Text], [PyVal])
storeTools ag =
  readIORef (agContext ag) >>= \case
    RlmRunning run -> do
      st <- readIORef (rrState run)
      pure (["recall"], [recallSchema | not (null (rsStore st))])
    ClmRunning run | Just ref <- crStore run -> do
      s <- readIORef ref
      let withRecall = isJust (csRecall (crSetup run))
      pure ("read_stored" : ["recall" | withRecall], if null (sItems s) then [] else readStoredSchema : [recallSchema | withRecall])
    _ -> pure ([], [])

-- | Whether CLM's store holds an item (its tools and prompt section are
-- offered from then on).
clmStoreHolds :: Agent -> IO Bool
clmStoreHolds ag =
  readIORef (agContext ag) >>= \case
    ClmRunning run | Just ref <- crStore run -> not . null . sItems <$> readIORef ref
    _ -> pure False

-- | The store's schemas are in the offered tools exactly when they are
-- offered.
syncRecallTool :: Agent -> IO ()
syncRecallTool ag = do
  (_, offered) <- storeTools ag
  modifyIORef (agSettings ag) $ \s ->
    let ts0 = fromMaybe [] (sTools s)
        ts = filter (`notElem` [recallSchema, readStoredSchema]) ts0 <> offered
     in if ts == ts0 then s else s {sTools = if null ts then Nothing else Just ts}

-- | The tools the model is offered now: the agent's, with the store's when
-- a store is on and they are offered.
offeredTools :: Agent -> IO [PyVal]
offeredTools ag = do
  (names, _) <- storeTools ag
  if null names then pure (agTools ag) else fromMaybe [] . sTools <$> readIORef (agSettings ag)

-- | Sync the turns with the messages after the pinned prefix; the
-- messages that take fresh ids are new, and go to the transcript.
syncRecord :: ClmRun -> [PyVal] -> IO ClmState
syncRecord run ed = do
  st <- readIORef (crState run)
  let st' = syncTurns ed st
      fresh = [t | t@(Turn i _) <- stTurns st', i >= stCounter st]
  n0 <- length <$> readIORef (crTranscript run)
  modifyIORef (crTranscript run) (<> map turnMsg fresh)
  recordOrigins run (zip (map turnId fresh) [n0 ..])
  -- with the store on, a loop-side rewrite (the turns it replaced) keeps
  -- the originals not live in full
  forM_ (crStore run) $ \ref -> do
    let ids = S.fromList (map turnId (stTurns st'))
        gone = [t | t@(Turn i _) <- stTurns st, not (S.member i ids)]
    unless (null gone) $ do
      tr <- readIORef (crTranscript run)
      origin <- readIORef (crOrigin run)
      s0 <- readIORef ref
      let (s1, items) = archiveRewritten (mioPath (csIO (crSetup run))) tr origin gone (stTurns st') s0
      unless (null items) $ do
        writeIORef ref s1
        reportArchive run items
        saveStore run
  writeIORef (crState run) st'
  pure st'

-- | 'syncRecord', and when the sync stored the store's first item, its
-- tools and section from the next request ('storeBegins'): the new prompt.
syncRecordBegins :: Agent -> Maybe Text -> ClmRun -> [PyVal] -> IO (ClmState, Maybe Text)
syncRecordBegins ag systemMessage run ed = do
  held0 <- clmStoreHolds ag
  st <- syncRecord run ed
  held1 <- clmStoreHolds ag
  p <- if held1 && not held0 then Just <$> storeBegins ag systemMessage else pure Nothing
  pure (st, p)

-- | With the store on, the transcript positions of turns' originals.
recordOrigins :: ClmRun -> [(Int, Int)] -> IO ()
recordOrigins run kvs = when (isJust (crStore run) && not (null kvs)) $ modifyIORef' (crOrigin run) (M.union (M.fromList kvs))

-- | Save the store, with the store on.
saveStore :: ClmRun -> IO ()
saveStore run = forM_ (crStore run) $ \ref -> readIORef ref >>= \s -> void (try @SomeException (csStoreSave (crSetup run) s))

-- | Report a batch's archived items to the hook, grouped by why.
reportArchive :: ClmRun -> [(Item, Text)] -> IO ()
reportArchive run items =
  forM_ (nub (map snd items)) $ \w -> csOnArchive (crSetup run) [i | (i, w') <- items, w' == w] w

saveClm :: ClmRun -> IO ()
saveClm run = readIORef (crState run) >>= \st -> void (try @SomeException (csSave (crSetup run) st))

-- | The CLM state, when CLM is the strategy.
agentClmState :: Agent -> IO (Maybe ClmState)
agentClmState ag = (clmOf <$> readIORef (agContext ag)) >>= traverse (readIORef . crState)

agentConfig :: Agent -> AgentConfig
agentConfig = agCfg

agentSessionId :: Agent -> IO Text
agentSessionId = readIORef . agSessionId

agentTools :: Agent -> [PyVal]
agentTools = agTools

agentValidTools :: Agent -> [Text]
agentValidTools = agValid

agentSessionCounters :: Agent -> IO (Integer, Integer, Integer, Int)
agentSessionCounters = readIORef . agCounters

agentCachedPrompt :: Agent -> IO (Maybe Text)
agentCachedPrompt = readIORef . agCachedPrompt

agentTodoStore :: Agent -> TodoStore
agentTodoStore = agTodo

agentMemoryStore :: Agent -> Maybe MemoryStore
agentMemoryStore = agMemStore

-- | The request for a summary when the iterations run out.
summaryRequestText :: Text
summaryRequestText = "You've reached the maximum number of tool-calling iterations allowed. Please provide a final response summarizing what you've found and accomplished so far, without calling any more tools."

prevPathOf :: Text -> Text
prevPathOf p = maybe (p <> ".prev") (<> ".prev.txt") (T.stripSuffix ".txt" p)

-- ---------------------------------------------------------------- output

say :: Agent -> Text -> IO ()
say ag t = sysOut (envSys (agEnv ag)) (t <> "\n")

pfx :: Agent -> Text
pfx ag = let p = cfgLogPrefix (agCfg ag) in if T.null p then "" else p <> " "

quiet :: Agent -> Bool
quiet = cfgQuiet . agCfg

unlessQuiet :: Agent -> IO () -> IO ()
unlessQuiet ag = unless (quiet ag)

tshow :: Show a => a -> Text
tshow = T.pack . show

int :: Int -> Text
int = tshow

now :: Agent -> IO Double
now ag = sysNow (envSys (agEnv ag))

-- ---------------------------------------------------------------- creation

-- | @AIAgent(...)@
newAgent :: AgentConfig -> AgentEnv -> IO Agent
newAgent cfg env = do
  let sys = envSys env
      out t = sysOut sys (t <> "\n")
      q = cfgQuiet cfg
      key = cfgApiKey cfg
      mode = cfgApiMode cfg
  unless q $ do
    if mode == AnthropicMessages
      then do
        out ("🤖 AI Agent initialized with model: " <> cfgModel cfg <> " (Anthropic native)")
        when (T.length key > 12) $ out ("🔑 Using token: " <> T.take 8 key <> "..." <> T.takeEnd 4 key)
      else do
        out ("🤖 AI Agent initialized with model: " <> cfgModel cfg)
        when (cfgBaseUrlGiven cfg) $ out ("🔗 Using custom base URL: " <> cfgBaseUrl cfg)
        if not (T.null key) && key /= "dummy-key" && T.length key > 12
          then out ("🔑 Using API key: " <> T.take 8 key <> "..." <> T.takeEnd 4 key)
          else out ("⚠️  Warning: API key appears invalid or missing (got: '" <> (if T.null key then "none" else T.take 20 key) <> "...')")
  case cfgFallback cfg of
    Just (p, m) | not q, not (T.null p), not (T.null m) -> out ("🔄 Fallback model: " <> m <> " (" <> p <> ")")
    _ -> pure ()
  let tr = envTools env
      tools = trDefinitions tr
      valid = sort [n | t <- tools, Just (PStr n) <- [get "function" t >>= get "name"]]
  unless q $ do
    mapM_ out (trPrinted tr)
    if not (null tools)
      then do
        out ("🛠️  Loaded " <> int (length tools) <> " tools: " <> T.intercalate ", " valid)
        forM_ (cfgEnabledToolsets cfg) $ \ts -> unless (null ts) $ out ("   ✅ Enabled toolsets: " <> T.intercalate ", " ts)
        forM_ (cfgDisabledToolsets cfg) $ \ts -> unless (null ts) $ out ("   ❌ Disabled toolsets: " <> T.intercalate ", " ts)
        reqs <- trRequirements tr
        let missing = [n | (n, False) <- reqs]
        unless (null missing) $ out ("⚠️  Some tools may not work due to missing requirements: " <> pyRepr (PList (map PStr missing)))
      else out "🛠️  No tools loaded (all tools filtered out or unavailable)"
  let caching = usesPromptCaching (cfgBaseUrl cfg) (cfgModel cfg) (mode == AnthropicMessages)
  unless q $ do
    forM_ (cfgEphemeral cfg) $ \e ->
      out ("🔒 Ephemeral system prompt: '" <> (if T.length e > 60 then T.take 60 e <> "..." else e) <> "' (not saved to trajectories)")
    when caching $ out ("💾 Prompt caching: ENABLED (" <> (if mode == AnthropicMessages then "native Anthropic" else "Claude via OpenRouter") <> ", 5m TTL)")
  start <- envNowLocal env
  sid <- case cfgSessionId cfg of
    Just s -> pure s
    Nothing -> do
      h <- sysUuidHex sys
      pure (T.pack (formatTime defaultTimeLocale "%Y%m%d_%H%M%S" start) <> "_" <> T.take 6 h)
  let home = sysGaussHome sys
      logsDir = home </> "sessions"
  createDirectoryIfMissing True logsDir
  forM_ (envDb env) $ \db ->
    void . try @SomeException $
      dbCreateSession db sid (fromMaybe "cli" (cfgPlatform cfg)) (Just (cfgModel cfg))
        (Just (PDict [("max_iterations", PInt (fromIntegral (cfgMaxIterations cfg))), ("reasoning_config", fromMaybe PNull (cfgReasoning cfg)), ("max_tokens", maybe PNull PInt (cfgMaxTokens cfg))]))
        Nothing
  -- memory
  let memCfg = case get "memory" (envConfig env) of
        Just d@(PDict _) -> d
        _ -> PDict []
      flag k = maybe False truthy (get k memCfg)
      limit k d = case get k memCfg of
        Just (PInt n) -> fromIntegral n
        _ -> d
      -- int(mem_config.get(k, d)): a value int() refuses aborts the
      -- memory setup (upstream's try around it)
      intOr k d = case get k memCfg of
        Nothing -> Just d
        Just v -> pyIntOf v
      nudgeFlush = if cfgSkipMemory cfg then Just (10, 6) else (,) <$> intOr "nudge_interval" 10 <*> intOr "flush_min_turns" 6
      (nudge, flushMin) = fromMaybe (10, 6) nudgeFlush
      memory =
        if cfgSkipMemory cfg || isNothing nudgeFlush || not (flag "memory_enabled" || flag "user_profile_enabled")
          then Nothing
          else Just (flag "memory_enabled", flag "user_profile_enabled", limit "memory_char_limit" 2200, limit "user_char_limit" 1375)
  memStore <- case memory of
    Just (_, _, ml, ul) -> do
      st <- newMemoryStore (home </> "memories") ml ul
      _ <- try @SomeException (loadFromDisk st)
      pure (Just st)
    Nothing -> pure Nothing
  todo <- newTodoStore
  -- compressor
  thresholdEnv <- sysGetEnv sys "CONTEXT_COMPRESSION_THRESHOLD"
  enabledEnv <- sysGetEnv sys "CONTEXT_COMPRESSION_ENABLED"
  summaryModel <- sysGetEnv sys "CONTEXT_COMPRESSION_MODEL"
  let pct = maybe 0.5 (\t -> fromMaybe 0.5 (readD (T.unpack t))) thresholdEnv
      compressionOn = T.toLower (fromMaybe "true" enabledEnv) `elem` ["true", "1", "yes"]
  ctx <- getModelContextLength (envMeta env) (cfgModel cfg) (cfgBaseUrl cfg)
  let comp = newCompressor (cfgModel cfg) (cfgBaseUrl cfg) pct 3 4 500 q (summaryModel >>= \m -> if T.null m then Nothing else Just m) ctx
  unless q $
    out $
      if compressionOn
        then "📊 Context limit: " <> commas ctx <> " tokens (compress at " <> int (truncate (pct * 100)) <> "% = " <> commas (cThresholdTokens comp) <> ")"
        else "📊 Context limit: " <> commas ctx <> " tokens (auto-compression disabled)"
  let settings =
        defaultSettings
          { sMode = mode, sBaseUrl = cfgBaseUrl cfg, sApiKey = key, sModel = cfgModel cfg, sProvider = cfgProvider cfg
          , sMaxTokens = cfgMaxTokens cfg, sReasoning = cfgReasoning cfg, sRouting = cfgRouting cfg, sSessionId = sid
          , sTools = if null tools then Nothing else Just tools }
  Agent cfg env
    <$> newIORef settings <*> newIORef False <*> pure tools <*> pure valid <*> newIORef caching
    <*> newIORef sid <*> pure start <*> pure (logsDir </> T.unpack ("session_" <> sid <> ".json"))
    <*> newIORef Nothing <*> newIORef comp <*> pure compressionOn <*> pure memory <*> pure memStore <*> pure todo
    <*> pure nudge <*> pure flushMin <*> newIORef 0
    <*> newIORef (False, Nothing) <*> newIORef [] <*> newIORef 0 <*> newIORef 0
    <*> newIORef (cfgFallback cfg) <*> newIORef False <*> newIORef (0, 0, 0, 0)
    <*> newIORef Nothing <*> newIORef Nothing <*> newIORef False
    <*> newIORef 0 <*> newIORef 0 <*> newIORef 0 <*> newIORef 0 <*> newIORef 0 <*> newIORef Nothing
    <*> newIORef (0, cfgMaxIterations cfg)
    <*> newIORef CompressRun
    <*> newIORef (SessionCost 0 0 0 0 0)
    <*> newIORef 0
    <*> newIORef 0
  where
    readD s = case reads s :: [(Double, String)] of
      [(d, "")] -> Just d
      _ -> Nothing
    pyIntOf = \case
      PInt n -> Just (fromIntegral n)
      PBool b -> Just (if b then 1 else 0)
      PFloat d | not (isNaN d || isInfinite d) -> Just (truncate d)
      PStr t -> fromIntegral <$> pyInt t
      _ -> Nothing

-- ---------------------------------------------------------------- interrupts

-- | @interrupt(message)@
interruptAgent :: Agent -> Maybe Text -> IO ()
interruptAgent ag msg = do
  writeIORef (agInterrupt ag) (True, msg)
  trSetInterrupt (envTools (agEnv ag)) True
  unlessQuiet ag $
    say ag $
      "\n⚡ Interrupt requested" <> case msg of
        Just m | T.length m > 40 -> ": '" <> T.take 40 m <> "...'"
        Just m | not (T.null m) -> ": '" <> m <> "'"
        _ -> ""

clearInterrupt :: Agent -> IO ()
clearInterrupt ag = do
  writeIORef (agInterrupt ag) (False, Nothing)
  trSetInterrupt (envTools (agEnv ag)) False

isInterrupted :: Agent -> IO Bool
isInterrupted ag = fst <$> readIORef (agInterrupt ag)

-- ---------------------------------------------------------------- helpers

-- | @re.sub(r'<think>.*?</think>' + tail, '', t, flags=re.DOTALL)@, with
-- the tail eating trailing whitespace when asked.
removeThink :: Bool -> Text -> Text
removeThink eatSpace = go
  where
    go t = case T.breakOn "<think>" t of
      (_, rest) | T.null rest -> t
      (before, rest) -> case T.breakOn "</think>" (T.drop 7 rest) of
        (_, close) | T.null close -> t
        (_, close) ->
          let after = T.drop 8 close
              after' = if eatSpace then T.dropWhile isPySpace after else after
           in before <> go after'
    isPySpace c = T.null (T.strip (T.singleton c))

-- | @_strip_think_blocks@ (no strip).
stripThink :: Text -> Text
stripThink t = if T.null t then "" else removeThink False t

-- | @_has_content_after_think_block@
hasContentAfterThink :: Text -> Bool
hasContentAfterThink t = not (T.null t) && not (T.null (T.strip (removeThink False t)))

-- | @_repair_tool_call(name)@
repairToolName :: [Text] -> Text -> Maybe Text
repairToolName valid name
  | lower `elem` valid = Just lower
  | norm `elem` valid = Just norm
  | otherwise =
      let scored = [(D.ratio (T.unpack lower) (T.unpack v), v) | v <- valid]
          ok = [(s, v) | (s, v) <- scored, s >= 0.7]
       in case sortOn (\(s, v) -> (Down s, Down v)) ok of
            ((_, v) : _) -> Just v
            [] -> Nothing
  where
    lower = T.toLower name
    norm = T.replace " " "_" (T.replace "-" "_" lower)

-- | @_get_budget_warning(api_call_count)@
budgetWarning :: Int -> Int -> Maybe Text
budgetWarning n maxIt
  | maxIt <= 0 = Nothing
  | progress >= 0.9 =
      Just ("[BUDGET WARNING: Iteration " <> int n <> "/" <> int maxIt <> ". Only " <> int remaining <> " iteration(s) left. Provide your final response NOW. No more tool calls unless absolutely critical.]")
  | progress >= 0.7 = Just ("[BUDGET: Iteration " <> int n <> "/" <> int maxIt <> ". " <> int remaining <> " iterations left. Start consolidating your work.]")
  | otherwise = Nothing
  where
    progress = fromIntegral n / fromIntegral maxIt :: Double
    remaining = maxIt - n

role :: PyVal -> Text
role m = case get "role" m of
  Just (PStr r) -> r
  _ -> ""

contentT :: PyVal -> Text
contentT m = case get "content" m of
  Just (PStr s) -> s
  _ -> ""

toolCalls :: PyVal -> [PyVal]
toolCalls m = case get "tool_calls" m of
  Just (PList xs) -> xs
  _ -> []

tcName :: PyVal -> Text
tcName tc = maybe "" pyStrOf (get "function" tc >>= get "name")

tcId :: PyVal -> PyVal
tcId tc = fromMaybe PNull (get "id" tc)

tcArgs :: PyVal -> PyVal
tcArgs tc = fromMaybe PNull (get "function" tc >>= get "arguments")

setTc :: Text -> PyVal -> PyVal -> PyVal
setTc k v tc = set "function" (set k v (fromMaybe (PDict []) (get "function" tc))) tc

parseArgs :: PyVal -> PyVal
parseArgs a = case a of
  PStr s -> case loads (TE.encodeUtf8 s) of
    Right d@(PDict _) -> d
    _ -> PDict []
  _ -> PDict []

loadsT :: Text -> Maybe PyVal
loadsT = either (const Nothing) Just . loads . TE.encodeUtf8

-- | @_get_messages_up_to_last_assistant@
upToLastAssistant :: [PyVal] -> [PyVal]
upToLastAssistant [] = []
upToLastAssistant ms = case [i | (i, m) <- zip [0 :: Int ..] ms, role m == "assistant"] of
  [] -> ms
  is -> take (last is) ms

-- | The last assistant message with tool calls: its function names.
lastToolNames :: [PyVal] -> [Text]
lastToolNames ms = case [m | m <- reverse ms, role m == "assistant", not (null (toolCalls m))] of
  (m : _) -> map tcName (toolCalls m)
  [] -> []

-- ---------------------------------------------------------------- persistence

saveSessionLog :: Agent -> [PyVal] -> IO ()
saveSessionLog ag msgs = unless (null msgs || not (envWriteFiles (agEnv ag))) $ do
  s <- readIORef (agSettings ag)
  sid <- readIORef (agSessionId ag)
  prompt <- readIORef (agCachedPrompt ag)
  t <- envNowLocal (agEnv ag)
  tools <- offeredTools ag
  let entry = sessionLogEntry sid (sModel s) (sBaseUrl s) (fromMaybe "None" (cfgPlatform (agCfg ag))) (agSessionStart ag) t (fromMaybe "" prompt) tools msgs
      path = agLogFile ag
      tmp = path <> ".tmp"
  _ <- try @SomeException $ do
    BS.writeFile tmp (TE.encodeUtf8 (dumpsIndent True 2 entry))
    renameFile tmp path
  pure ()

applyOverride :: Agent -> [PyVal] -> IO [PyVal]
applyOverride ag msgs = do
  ov <- readIORef (agPersistOverride ag)
  idx <- readIORef (agPersistIdx ag)
  pure $ case (ov, idx) of
    (Just o, Just i) | i >= 0, i < length msgs, role (msgs !! i) == "user" ->
      [if j == i then set "content" (PStr o) m else m | (j, m) <- zip [0 ..] msgs]
    _ -> msgs

flushToDb :: Agent -> [PyVal] -> Int -> IO ()
flushToDb ag msgs historyLen = forM_ (envDb (agEnv ag)) $ \db -> do
  msgs' <- applyOverride ag msgs
  lastIdx <- readIORef (agLastFlushed ag)
  sid <- readIORef (agSessionId ag)
  let from = max historyLen lastIdx
  r <- try @SomeException (mapM_ (dbAppendMessage db sid) (drop from msgs'))
  case r of
    Right () -> writeIORef (agLastFlushed ag) (length msgs')
    Left _ -> pure ()

-- | @_persist_session(messages, history)@: the messages as persisted (the
-- user-message override applied).
persistSession :: Agent -> [PyVal] -> Int -> IO [PyVal]
persistSession ag msgs historyLen = do
  ctxRun <- readIORef (agContext ag)
  case clmOf ctxRun of
    Nothing -> do
      msgs' <- applyOverride ag msgs
      writeIORef (agSessionMessages ag) msgs'
      saveSessionLog ag msgs'
      -- under RLM the flushed count follows the offloads (the run's start
      -- took the history into it), so the history length is not used
      flushToDb ag msgs' (if isJust (rlmOf ctxRun) then 0 else historyLen)
      pure msgs'
    Just run -> do
      -- the live context is the session's messages; state.db keeps the
      -- full transcript (section 9)
      writeIORef (agSessionMessages ag) msgs
      saveSessionLog ag msgs
      forM_ (envDb (agEnv ag)) $ \db -> do
        tr <- readIORef (crTranscript run)
        done <- readIORef (crFlushed run)
        sid <- readIORef (agSessionId ag)
        r <- try @SomeException (mapM_ (dbAppendMessage db sid) (drop done tr))
        case r of
          Right () -> writeIORef (crFlushed run) (length tr)
          Left _ -> pure ()
      saveClm run
      pure msgs

-- | A run that rolls back returns fewer messages than it persists. Under
-- RLM, state.db gets the returned list instead (the session's messages and
-- log are as before), so its rows stay at the live positions that the
-- next run and a resume count on. Rows a flush wrote before the rollback
-- stay. Compress and CLM persist as before.
persistRollback :: Agent -> [PyVal] -> [PyVal] -> Int -> IO ()
persistRollback ag msgs rolled historyLen = do
  rlm <- isRlm ag
  if rlm
    then do
      msgs' <- applyOverride ag msgs
      writeIORef (agSessionMessages ag) msgs'
      saveSessionLog ag msgs'
      flushToDb ag rolled 0
    else void (persistSession ag msgs historyLen)

-- | @_dump_api_request_debug@: the request body (as Fisher sends it) and
-- the error, under the sessions directory.
dumpRequest :: Agent -> HttpReq -> Text -> Maybe LLMError -> IO ()
dumpRequest ag hr reason err = when (envWriteFiles (agEnv ag)) $ do
  sid <- readIORef (agSessionId ag)
  t <- envNowLocal (agEnv ag)
  s <- readIORef (agSettings ag)
  let stamp = T.pack (formatTime defaultTimeLocale "%Y%m%d_%H%M%S_%6q" t)
      path = sysGaussHome (envSys (agEnv ag)) </> "sessions" </> T.unpack ("request_dump_" <> sid <> "_" <> stamp <> ".json")
      key = sApiKey s
      masked = if T.null key then PNull else PStr ("Bearer " <> (if T.length key <= 12 then "***" else T.take 8 key <> "..." <> T.takeEnd 4 key))
      body = maybe PNull dropNulls (hrBody hr)
      dropNulls = \case
        PDict kvs -> PDict [(k, v) | (k, v) <- kvs, v /= PNull]
        v -> v
      entry =
        PDict $
          [ ("timestamp", PStr (isoformat t)), ("session_id", PStr sid), ("reason", PStr reason)
          , ("request", PDict [("method", PStr "POST"), ("url", PStr (T.dropWhileEnd (== '/') (sBaseUrl s) <> "/chat/completions")), ("headers", PDict [("Authorization", masked), ("Content-Type", PStr "application/json")]), ("body", body)]) ]
            <> [ ( "error"
                 , PDict $
                     [("type", PStr (errorClass e)), ("message", PStr (errorText e))]
                       <> case e of
                         StatusError code _ b -> [("status_code", PInt (fromIntegral code)), ("body", b)]
                         _ -> [] )
               | Just e <- [err] ]
  r <- try @SomeException (BS.writeFile path (TE.encodeUtf8 (dumpsIndent False 2 entry)))
  case r of
    Right () -> say ag (pfx ag <> "🧾 Request debug dump written to: " <> T.pack path)
    Left _ -> pure ()

-- ---------------------------------------------------------------- system prompt

buildPrompt :: Agent -> Maybe Text -> IO Text
buildPrompt ag systemMessage = do
  let env = agEnv ag
      cfg = agCfg ag
  t <- envPromptNow env
  cwd <- envCwd env
  snap <- traverse frozenSnapshot (agMemStore ag)
  sid <- readIORef (agSessionId ag)
  clmRun <- (clmOf <$> readIORef (agContext ag))
  rlmOn <- recallOffered ag
  stored <- clmStoreHolds ag
  let clmSection p = case clmRun of
        Just run ->
          let setup = crSetup run
           in p <> "\n\n" <> promptSection (mioPath (csIO setup)) (clmLimit (csConfig setup))
                <> (let sk = T.strip (ccSkill (csConfig setup)) in if T.null sk then "" else "\n\n" <> sk)
                -- with the store on, from the first item on (spec 3.4)
                <> (if stored && ccStorePrompt (csConfig setup) then "\n\n" <> storePromptSection else "")
        Nothing -> p
      -- under RLM, from the first offload on (spec 3.3)
      rlmSection p = if rlmOn then p <> "\n\n" <> Rlm.promptSection else p
  envPromptHook env . rlmSection . clmSection =<< buildSystemPrompt (envWarn env) (
    PromptInput
      { piToolNames = agValid ag
      , piSystemMessage = systemMessage
      , piMemory = case (agMemory ag, snap) of
          (Just (m, u, _, _), Just s) -> Just (m, u, s)
          _ -> Nothing
      , piSkipContextFiles = cfgSkipContextFiles cfg
      , piGaussHome = sysGaussHome (envSys env)
      , piCwd = cwd
      , piNow = t
      , piSessionId = if cfgPassSessionId cfg then Just sid else Nothing
      , piPlatform = cfgPlatform cfg })

-- | Under CLM with the store, once the first item is stored: offer the
-- store's tools and rebuild the prompt with its section (the cached prompt
-- and state.db's, so a resume reuses it). The new prompt.
storeBegins :: Agent -> Maybe Text -> IO Text
storeBegins ag systemMessage = do
  syncRecallTool ag
  invalidatePrompt ag
  p <- buildPrompt ag systemMessage
  writeIORef (agCachedPrompt ag) (Just p)
  forM_ (envDb (agEnv ag)) $ \db -> do
    sid <- readIORef (agSessionId ag)
    void (try @SomeException (dbUpdateSystemPrompt db sid p))
  pure p

-- | @_invalidate_system_prompt()@
invalidatePrompt :: Agent -> IO ()
invalidatePrompt ag = do
  writeIORef (agCachedPrompt ag) Nothing
  forM_ (agMemStore ag) $ \st -> void (try @SomeException (loadFromDisk st))

-- | @_compress_context(messages, system_message, approx_tokens, task_id)@
compressContext :: Agent -> [PyVal] -> Maybe Text -> Maybe Integer -> Text -> IO ([PyVal], Text)
compressContext ag msgs systemMessage approx taskId = do
  flushMemories ag (Just msgs) (Just 0)
  ctxRun <- readIORef (agContext ag)
  case rlmOf ctxRun of
    Just run -> offloadContext ag run True msgs systemMessage
    Nothing -> compressWith ag msgs systemMessage approx taskId

-- | Upstream's compression: the summary, the todo and file notes, and a
-- new @state.db@ session.
compressWith :: Agent -> [PyVal] -> Maybe Text -> Maybe Integer -> Text -> IO ([PyVal], Text)
compressWith ag msgs systemMessage approx taskId = do
  comp <- readIORef (agComp ag)
  (compressed00, comp') <- compress (envAux (agEnv ag)) (say ag) (envWarn (agEnv ag)) comp msgs approx
  writeIORef (agComp ag) comp'
  todoNote <- formatForInjection (agTodo ag)
  let compressed0 = compressed00 <> [PDict [("role", PStr "user"), ("content", PStr t)] | Just t <- [todoNote]]
  files <- either (const []) id <$> try @SomeException (trReadFilesSummary (envTools (agEnv ag)) taskId)
  let fileNote =
        [ PDict
            [ ("role", PStr "user")
            , ( "content"
              , PStr
                  ( "[Files already read in this session — do NOT re-read these]\n"
                      <> T.intercalate "\n" ["  - " <> p <> " (" <> T.intercalate ", " rs <> ")" | (p, rs) <- files]
                      <> "\nUse the information from the context summary above. Proceed with writing, editing, or responding." ) ) ]
        | not (null files) ]
      compressed = compressed0 <> fileNote
  invalidatePrompt ag
  prompt <- buildPrompt ag systemMessage
  writeIORef (agCachedPrompt ag) (Just prompt)
  forM_ (envDb (agEnv ag)) $ \db -> do
    old <- readIORef (agSessionId ag)
    void . try @SomeException $ do
      oldTitle <- dbGetSessionTitle db old
      dbEndSession db old "compression"
      t <- envNowLocal (agEnv ag)
      h <- sysUuidHex (envSys (agEnv ag))
      let new = T.pack (formatTime defaultTimeLocale "%Y%m%d_%H%M%S" t) <> "_" <> T.take 6 h
      writeIORef (agSessionId ag) new
      modifyIORef (agSettings ag) (\s -> s {sSessionId = new})
      s <- readIORef (agSettings ag)
      dbCreateSession db new (fromMaybe "cli" (cfgPlatform (agCfg ag))) (Just (sModel s)) Nothing (Just old)
      forM_ oldTitle $ \ot -> void . try @SomeException $ do
        titles <- dbTitles db
        dbSetSessionTitle db new (nextTitleInLineage titles ot)
      dbUpdateSystemPrompt db new prompt
      writeIORef (agLastFlushed ag) 0
  pure (compressed, prompt)

-- | The RLM strategy where the compressor would compress (spec 3.2 to 3.4):
-- the middle of the history moves to recall's store behind one stub; when
-- there is no middle, or the history is still over the threshold, large
-- tool results in the tail are cut. No summary call, no notes, and the
-- @state.db@ session goes on: every message is flushed to it before the
-- cut, so it keeps the full transcript.
offloadContext :: Agent -> RlmRun -> Bool -> [PyVal] -> Maybe Text -> IO ([PyVal], Text)
offloadContext ag run move msgs systemMessage = do
  comp <- readIORef (agComp ag)
  before <- readIORef (agCachedPrompt ag)
  flushToDb ag msgs 0
  st0 <- readIORef (rrState run)
  let setup = rrSetup run
      save st = void (try @SomeException (rlSave setup st))
      done = if move then offload (cProtectFirst comp) (cProtectLast comp) st0 msgs else Nothing
  (msgs1, st1) <- case done of
    Just o -> do
      writeIORef (rrSinceOffload run) 0
      writeIORef (rrState run) (ofState o)
      save (ofState o)
      rlOnOffload setup o
      syncRecallTool ag
      let (a, b) = ofMoved o
      unlessQuiet ag $ say ag (pfx ag <> "   🗄️  Moved turns " <> int a <> " to " <> int b <> " out of the context (recall can read them)")
      remapFlushed (ofMessages o)
      pure (ofMessages o, ofState o)
    Nothing -> pure (msgs, st0)
  let rebuild = do
        invalidatePrompt ag
        p <- buildPrompt ag systemMessage
        writeIORef (agCachedPrompt ag) (Just p)
        pure p
  prompt1 <- rebuild
  let over = estimateTokensRough prompt1 + estimateMessagesTokensRough msgs1 >= cThresholdTokens comp
  (msgs2, prompt) <-
    if isNothing done || over
      then do
        let (cut, st2) = cutTail st1 msgs1
        if st2 /= st1
          then do
            writeIORef (rrState run) st2
            save st2
            rlOnCut setup [t | t <- rsStore st2, stIndex t `notElem` map stIndex (rsStore st1)]
            -- the first stored turn can come from the cut: recall and the
            -- section are offered from then on
            syncRecallTool ag
            p <- if null (rsStore st1) then rebuild else pure prompt1
            pure (cut, p)
          else pure (cut, prompt1)
      else pure (msgs1, prompt1)
  -- the same session: a resume reuses its stored prompt, so it gets the
  -- recall section when that is new
  when (before /= Just prompt) $ forM_ (envDb (agEnv ag)) $ \db -> do
    sid <- readIORef (agSessionId ag)
    void (try @SomeException (dbUpdateSystemPrompt db sid prompt))
  pure (msgs2, prompt)
  where
    -- the live positions after the cut: the head, the stub, then the tail
    -- as it was (the moved messages are already in state.db)
    remapFlushed new = case findIndexL isStub new of
      Nothing -> pure ()
      Just he -> do
        let ts = he + 1 + length msgs - length new
            shift i
              | i <= he = Just i
              | i >= ts = Just (i - (ts - he - 1))
              | otherwise = Nothing
        -- a flush that failed above leaves the count inside the moved
        -- range: those messages are only in the store now, and the count
        -- goes on after the stub
        modifyIORef (agLastFlushed ag) (\i -> fromMaybe (he + 1) (shift i))
        modifyIORef (agPersistIdx ag) (>>= shift)
    findIndexL p xs = case [i | (i, x) <- zip [0 ..] xs, p x] of
      (i : _) -> Just i
      [] -> Nothing

-- | Under RLM, when the threshold is passed but the offload must wait
-- ('offloadAllowed'): the tail cut alone (spec 3.4), with no memory flush.
-- 'Nothing' when there is nothing to cut, so the prompt is left as it is.
cutOnly :: Agent -> [PyVal] -> Maybe Text -> IO (Maybe ([PyVal], Text))
cutOnly ag msgs systemMessage = do
  ctxRun <- readIORef (agContext ag)
  case rlmOf ctxRun of
    Nothing -> pure Nothing
    Just run -> do
      st <- readIORef (rrState run)
      if snd (cutTail st msgs) == st
        then pure Nothing
        else Just <$> offloadContext ag run False msgs systemMessage

-- | Spec 3.2: whether an offload that is not forced may run, the threshold
-- being passed: at least 'rcOffloadGap' tool batches since the last offload,
-- and a middle of at least 'rcMinOffload' of the room (the threshold less
-- the system prompt and the protected head), by the rough estimates the
-- threshold test uses. Under another strategy it always may.
offloadAllowed :: Agent -> [PyVal] -> Maybe Text -> IO Bool
offloadAllowed ag msgs prompt = do
  ctxRun <- readIORef (agContext ag)
  case rlmOf ctxRun of
    Nothing -> pure True
    Just run -> do
      comp <- readIORef (agComp ag)
      since <- readIORef (rrSinceOffload run)
      st <- readIORef (rrState run)
      p <- maybe (readIORef (agCachedPrompt ag)) (pure . Just) prompt
      let cfg = rlConfig (rrSetup run)
          done = offload (cProtectFirst comp) (cProtectLast comp) st msgs
          -- the head that stays: up to the stub after the offload
          kept = maybe (take (cProtectFirst comp) msgs) (takeWhile (not . isStub) . ofMessages) done
          room = cThresholdTokens comp - estimateTokensRough (fromMaybe "" p) - estimateMessagesTokensRough kept
          middle = maybe 0 (estimateMessagesTokensRough . ofMovedMessages) done
      pure (since >= rcOffloadGap cfg && fromIntegral middle >= rcMinOffload cfg * fromIntegral room)

-- | @flush_memories(messages, min_turns)@: one call, with only the
-- memory tool offered, to save what is worth keeping before the context
-- goes (compression, a new session, exit). The messages are untouched.
flushMemories :: Agent -> Maybe [PyVal] -> Maybe Int -> IO ()
flushMemories ag given minTurns = do
  turns <- readIORef (agUserTurns ag)
  stored <- readIORef (agSessionMessages ag)
  let msgs = fromMaybe stored given
      effective = fromMaybe (agFlushMinTurns ag) minTurns
      memDef = [t | t <- agTools ag, (get "function" t >>= get "name") == Just (PStr "memory")]
  case agMemStore ag of
    Just st
      | not (agFlushMinTurns ag == 0 && isNothing minTurns)
      , "memory" `elem` agValid ag
      , turns >= effective
      , length msgs >= 3
      , (def : _) <- memDef -> do
          s <- readIORef (agSettings ag)
          prompt <- readIORef (agCachedPrompt ag)
          let strict = "api.mistral.ai" `T.isInfixOf` T.toLower (sBaseUrl s)
              flushMsg = PDict [("role", PStr "user"), ("content", PStr "[System: The session is being compressed. Save anything worth remembering — prioritize user preferences, corrections, and recurring patterns over task-specific details.]")]
              apiMsg m =
                let m1 = if role m == "assistant" then case get "reasoning" m of
                      Just r | truthy r -> set "reasoning_content" r m
                      _ -> m
                      else m
                    m2 = delete "finish_reason" (delete "reasoning" m1)
                 in if strict then sanitizeStrict m2 else m2
              apiMessages =
                [PDict [("role", PStr "system"), ("content", PStr p)] | Just p <- [prompt], not (T.null p)]
                  <> map apiMsg (msgs <> [flushMsg])
          _ <- try @SomeException $ do
            aux <- envAux (agEnv ag) (auxCall "flush_memories" apiMessages) {axTools = [def], axTemperature = Just 0.3, axMaxTokens = Just 5120, axTimeout = 30}
            calls <- case aux of
              Right v -> pure (respCalls v)
              Left (AuxFailed e) -> ioError (userError (T.unpack e))
              Left (AuxNoProvider _) -> mainClientCalls s apiMessages def
            forM_ [c | c <- calls, tcName c == "memory"] $ \c -> void . try @SomeException $ do
              let args = case tcArgs c of
                    PStr a -> either (ioError . userError) pure (loads (TE.encodeUtf8 a))
                    _ -> ioError (userError "arguments is not a str")
              a <- args
              _ <- envLocalTool (agEnv ag) "memory" a (memoryToolArgs False (Just st) a)
              unlessQuiet ag $ say ag ("  🧠 Memory flush: saved to " <> maybe "memory" pyStrOf (get "target" a))
          pure ()
    _ -> pure ()
  where
    respCalls v = case get "choices" v of
      Just (PList (c : _)) -> case get "message" c >>= get "tool_calls" of
        Just (PList xs) -> xs
        _ -> []
      _ -> []
    sanitizeStrict m = case get "tool_calls" m of
      Just (PList tcs) -> set "tool_calls" (PList [case tc of PDict kvs -> PDict [(k, v) | (k, v) <- kvs, k `notElem` ["call_id", "response_item_id"]]; x -> x | tc <- tcs]) m
      _ -> m
    -- no auxiliary client: the agent's own client
    mainClientCalls s apiMessages def = do
      let env = agEnv ag
      case sMode s of
        ChatCompletions -> do
          let low = T.toLower (sBaseUrl s)
              maxTok
                | "api.openai.com" `T.isInfixOf` low && not ("openrouter" `T.isInfixOf` low) = ("max_completion_tokens", PInt 5120)
                | otherwise = ("max_tokens", PInt 5120)
              -- the SDK's keyword order
              body = PDict [("messages", PList apiMessages), ("model", PStr (sModel s)), maxTok, ("temperature", PFloat 0.3), ("tools", PList [def])]
              base = T.dropWhileEnd (== '/') (sBaseUrl s)
          r <- sdkSend (envRuntime env) (envTransport env) (HttpReq "POST" (base <> "/chat/completions") (chatHeaders (sBaseUrl s) (sApiKey s)) (Just body) False) (Timeouts 30 30)
          pure $ case r of
            Right resp | Right v <- loads (rBody resp) -> respCalls v
            _ -> []
        _ -> do
          let s' = s {sTools = Just [def], sMaxTokens = Just 5120, sReasoning = Nothing}
          r <- callModel (envRuntime env) (envTransport env) (envLlm env) s' apiMessages
          pure $ case r of
            Right (CallOk o _) -> toolCalls (oMessage o)
            _ -> []

-- ---------------------------------------------------------------- running

data RunInput = RunInput
  { riUserMessage :: Text
  , riSystemMessage :: Maybe Text
  , riHistory :: [PyVal]
  , riTaskId :: Maybe Text
  , riPersistUserMessage :: Maybe Text
  }

runInput :: Text -> RunInput
runInput m = RunInput m Nothing [] Nothing Nothing

-- | The result dict, or the API error @run_conversation@ raised.
data AgentResult
  = RunDone PyVal
  | RunRaised Text Text
  -- ^ the exception's class and message
  deriving (Eq, Show)

-- | Loop-wide locals.
data Loop = Loop
  { lpCalls :: Int
  , lpFinal :: Maybe Text
  , lpInterrupted :: Bool
  , lpAck :: Int
  , lpLengthRetries :: Int
  , lpPrefix :: Text
  , lpMessages :: [PyVal]
  , lpPrompt :: Maybe Text
  }

-- | How the retry loop ended.
data Attempt
  = AOk Outcome PyVal
  | AInterrupted Double
  | AReturn PyVal
  | ARaise LLMError
  | ARestartCompressed [PyVal] Text
  | ARestartLength [PyVal] Text

-- | @run_conversation(user_message, system_message, conversation_history,
-- task_id, persist_user_message)@
runConversation :: Agent -> RunInput -> IO AgentResult
runConversation ag ri = either (\(PyError c m) -> RunRaised c m) id <$> try (runConversation' ag ri)

runConversation' :: Agent -> RunInput -> IO AgentResult
runConversation' ag ri = do
  let env = agEnv ag
      cfg = agCfg ag
      sys = envSys env
      history = riHistory ri
      historyLen = length history
  writeIORef (agPersistIdx ag) Nothing
  writeIORef (agPersistOverride ag) (riPersistUserMessage ri)
  taskId <- case riTaskId ri of
    Just t -> pure t
    Nothing -> uuidString <$> sysUuidHex sys
  mapM_ (\r -> writeIORef r 0) [agInvalidTool ag, agInvalidJson ag, agEmpty ag, agScratch ag, agCodexIncomplete ag]
  writeIORef (agLastContentWithTools ag) Nothing
  writeIORef (agTurnsSinceMemory ag) 0
  writeIORef (agBudget ag) (0, cfgMaxIterations cfg)
  -- _hydrate_todo_store: the latest todo result in the history (what it
  -- raises, run_conversation raises)
  hasItems <- todoHasItems (agTodo ag)
  unless (null history || hasItems) $ do
    n <- hydrateTodo (agTodo ag) history
    forM_ n $ \k -> unlessQuiet ag $ say ag (pfx ag <> "📋 Restored " <> int k <> " todo item(s) from history")
  modifyIORef (agUserTurns ag) (+ 1)
  -- the periodic memory nudge
  nudged <-
    if agNudgeInterval ag > 0 && "memory" `elem` agValid ag && isJust (agMemStore ag)
      then do
        n <- atomicModifyIORef' (agTurnsSinceMemory ag) (\x -> (x + 1, x + 1))
        if n >= agNudgeInterval ag
          then do
            writeIORef (agTurnsSinceMemory ag) 0
            pure "\n\n[System: You've had several exchanges. Consider: has the user shared preferences, corrected you, or revealed something about their workflow worth remembering for future sessions?]"
          else pure ""
      else pure ""
  let userMessage = riUserMessage ri <> nudged
      msgs0 = history <> [PDict [("role", PStr "user"), ("content", PStr userMessage)]]
  writeIORef (agPersistIdx ag) (Just (length msgs0 - 1))
  unlessQuiet ag $
    say ag ("💬 Starting conversation: '" <> T.take 60 userMessage <> (if T.length userMessage > 60 then "..." else "") <> "'")
  cached <- readIORef (agCachedPrompt ag)
  when (isNothing cached) $ do
    stored <- case (history, envDb env) of
      (_ : _, Just db) -> do
        sid <- readIORef (agSessionId ag)
        either (const Nothing) (>>= \p -> if T.null p then Nothing else Just p) <$> try @SomeException (dbStoredSystemPrompt db sid)
      _ -> pure Nothing
    case stored of
      Just p -> writeIORef (agCachedPrompt ag) (Just p)
      Nothing -> do
        p <- buildPrompt ag (riSystemMessage ri)
        writeIORef (agCachedPrompt ag) (Just p)
        forM_ (envDb env) $ \db -> do
          sid <- readIORef (agSessionId ag)
          void (try @SomeException (dbUpdateSystemPrompt db sid p))
  active0 <- readIORef (agCachedPrompt ag)
  clmRun <- (clmOf <$> readIORef (agContext ag))
  forM_ clmRun $ \run -> do
    let pinned = maybe (length msgs0) (+ 1) (lookup True (zip (map ((== "user") . role) msgs0) [0 ..]))
    writeIORef (crPinned run) pinned
    stBefore <- readIORef (crState run)
    modifyIORef (crState run) (\st -> (syncTurns (drop pinned msgs0) st) {stIters = 0, stRollbacks = 0})
    -- the run's own user message is new (the history is already stored)
    let userMsg = last msgs0
        stored = maybe userMsg (\o -> set "content" (PStr o) userMsg) (riPersistUserMessage ri)
    n0 <- length <$> readIORef (crTranscript run)
    modifyIORef (crTranscript run) (<> [stored])
    -- with the store on, the user message's turn has its original there;
    -- the transcript holds the persisted form, which is what is stored
    -- (at the first batch, when it differs from the live message)
    stAfter <- readIORef (crState run)
    case reverse (stTurns stAfter) of
      (Turn i m : _) | i >= stCounter stBefore, m == userMsg -> recordOrigins run [(i, n0)]
      _ -> pure ()
  -- under RLM, state.db holds the history already: the flushed count
  -- starts past it and follows the offloads
  rlmOn <- isRlm ag
  when rlmOn $ modifyIORef (agLastFlushed ag) (max historyLen)
  -- preflight compression (under RLM, an offload, whatever
  -- compression.enabled says)
  comp0 <- readIORef (agComp ag)
  (msgs1, active1) <-
    if (agCompressionEnabled ag || rlmOn) && isNothing clmRun && length msgs0 > cProtectFirst comp0 + cProtectLast comp0 + 1
      then do
        let est ms p = estimateTokensRough (fromMaybe "" p) + estimateMessagesTokensRough ms
            tokens0 = est msgs0 active0
        if tokens0 >= cThresholdTokens comp0
          then do
            -- under RLM each pass is an offload that is not forced
            let pass :: Int -> [PyVal] -> Maybe Text -> Integer -> IO ([PyVal], Maybe Text)
                pass k ms p tokens
                  | k >= 3 = pure (ms, p)
                  | otherwise = do
                      allowed <- offloadAllowed ag ms p
                      if not allowed
                        then maybe (ms, p) (\(ms', p') -> (ms', Just p')) <$> cutOnly ag ms (riSystemMessage ri)
                        else do
                          when (k == 0) $
                            unlessQuiet ag $ say ag ("📦 Preflight compression: ~" <> commas tokens0 <> " tokens >= " <> commas (cThresholdTokens comp0) <> " threshold")
                          (ms', p') <- compressContext ag ms (riSystemMessage ri) (Just tokens) taskId
                          if length ms' >= length ms
                            then pure (ms', Just p')
                            else do
                              let t' = est ms' (Just p')
                              c <- readIORef (agComp ag)
                              if t' < cThresholdTokens c then pure (ms', Just p') else pass (k + 1) ms' (Just p') t'
            pass 0 msgs0 active0 tokens0
          else pure (msgs0, active0)
      else pure (msgs0, active0)
  clearInterrupt ag
  r <- outer (Loop 0 Nothing False 0 0 "" msgs1 active1) taskId historyLen
  pure r
  where
    uuidString h = T.intercalate "-" [T.take 8 h, T.take 4 (T.drop 8 h), T.take 4 (T.drop 12 h), T.take 4 (T.drop 16 h), T.drop 20 h]

    outer :: Loop -> Text -> Int -> IO AgentResult
    outer lp taskId historyLen = do
      let cfg = agCfg ag
          maxIt = cfgMaxIterations cfg
      (used, maxB) <- readIORef (agBudget ag)
      clmRun <- (clmOf <$> readIORef (agContext ag))
      capped <- case clmRun of
        Nothing -> pure (lpCalls lp >= maxIt)
        Just run -> do
          st <- readIORef (crState run)
          pure (stIters st >= maxIt || lpCalls lp >= 2 * maxIt)
      if capped || maxB - used <= 0
        then finish lp taskId historyLen
        else do
          intr <- isInterrupted ag
          if intr
            then do
              unlessQuiet ag $ say ag "\n⚡ Breaking out of tool loop due to interrupt..."
              finish lp {lpInterrupted = True} taskId historyLen
            else do
              let n = lpCalls lp + 1
                  lp1 = lp {lpCalls = n}
              ok <- atomicModifyIORef' (agBudget ag) (\(u, m) -> if u >= m then ((u, m), False) else ((u + 1, m), True))
              when ok $ forM_ clmRun $ \run -> modifyIORef (crState run) (\st -> st {stIters = stIters st + 1})
              if not ok
                then do
                  unlessQuiet ag $ say ag ("\n⚠️  Session iteration budget exhausted (" <> int maxB <> " total across agent + subagents)")
                  finish lp1 taskId historyLen
                else iteration lp1 taskId historyLen

    iteration :: Loop -> Text -> Int -> IO AgentResult
    iteration lp0 taskId historyLen = do
      let env = agEnv ag
          cfg = agCfg ag
          n = lpCalls lp0
          maxIt = cfgMaxIterations cfg
      began <- (clmOf <$> readIORef (agContext ag)) >>= traverse (\run -> do
        pinned <- readIORef (crPinned run)
        let (pre, ed) = splitAt pinned (lpMessages lp0)
        (_, p) <- syncRecordBegins ag (riSystemMessage ri) run ed
        csObserve (crSetup run) (EvCall n (if null pre then PNull else last pre) ed)
        pure p)
      -- the store's first item, stored by the sync: this request's prompt
      -- has its section
      let lp = maybe lp0 (\p -> lp0 {lpPrompt = Just p}) (join began)
      forM_ (cbStep (envCallbacks env)) $ \f -> void (try @SomeException (f n (lastToolNames (lpMessages lp))))
      caching <- readIORef (agCaching ag)
      s0 <- readIORef (agSettings ag)
      let apiMessages =
            prepareMessages (PrepareInput (sBaseUrl s0) (lpPrompt lp) (cfgEphemeral cfg) (cfgPrefill cfg) caching) (lpMessages lp)
          totalChars = sum [fromIntegral (T.length (pyRepr m)) | m <- apiMessages] :: Integer
          approx = totalChars `div` 4
      unlessQuiet ag $ do
        say ag ("\n" <> pfx ag <> "🔄 Making API call #" <> int n <> "/" <> int maxIt <> "...")
        say ag (pfx ag <> "   📊 Request size: " <> int (length apiMessages) <> " messages, ~" <> commas approx <> " tokens (~" <> commas totalChars <> " chars)")
        offered <- offeredTools ag
        say ag (pfx ag <> "   🔧 Available tools: " <> int (length offered))
      spinner <-
        if quiet ag
          then do
            fi <- envChoice env (length kawaiiThinking)
            vi <- envChoice env (length thinkingVerbs)
            let label = (kawaiiThinking !! fi) <> " " <> (thinkingVerbs !! vi) <> "..."
            case cbThinking (envCallbacks env) of
              Just f -> f label >> pure Nothing
              Nothing -> do
                ki <- envChoice env 5
                Just <$> startSpinner (SpinnerOut (sysOut (envSys env)) (envAnimate env)) (envSkin env) label (["brain", "sparkle", "pulse", "moon", "star"] !! ki)
          else pure Nothing
      spRef <- newIORef spinner
      t0 <- now ag
      at <- retryLoop lp apiMessages approx spRef t0 taskId historyLen
      case at of
        AReturn res -> pure (RunDone res)
        ARaise e -> pure (RunRaised (errorClass e) (errorText e))
        AInterrupted elapsed ->
          finish lp {lpInterrupted = True, lpFinal = Just ("Operation interrupted: waiting for model response (" <> fmtF 1 elapsed <> "s elapsed).")} taskId historyLen
        ARestartCompressed ms p -> do
          modifyIORef (agBudget ag) (\(u, m) -> (if u > 0 then u - 1 else u, m))
          outer lp {lpCalls = n - 1, lpMessages = ms, lpPrompt = Just p} taskId historyLen
        ARestartLength ms prefix -> outer lp {lpMessages = ms, lpPrefix = prefix, lpLengthRetries = lpLengthRetries lp + 1} taskId historyLen
        AOk outcome raw -> do
          r <- try @SomeException (processResponse lp outcome raw taskId historyLen)
          case r of
            Right x -> x
            -- recall's model call got a 402: stop at once, as the loop
            -- does for its own 402 (a non-retryable client error)
            Left e | Just (RecallNoCredit why) <- fromException e -> do
              say ag (pfx ag <> "❌ " <> why)
              let ms = lpMessages lp
              _ <- persistSession ag ms historyLen
              pure . RunDone $
                PDict
                  [ ("final_response", PNull), ("messages", PList ms), ("api_calls", PInt (fromIntegral n))
                  , ("completed", PBool False), ("failed", PBool True), ("error", PStr why) ]
            Left e -> do
              let errorMsg = "Error during OpenAI-compatible API call #" <> int n <> ": " <> T.pack (displayException e)
              say ag ("❌ " <> errorMsg)
              let ms = lpMessages lp
                  ms' = addErrorResults ms errorMsg
              if n >= maxIt - 1
                then finish lp {lpMessages = ms', lpFinal = Just ("I apologize, but I encountered repeated errors: " <> errorMsg)} taskId historyLen
                else outer lp {lpMessages = ms'} taskId historyLen

    addErrorResults ms errorMsg =
      let (tailTools, rest) = span ((== "tool") . role) (reverse ms)
          answered = [tid | m <- tailTools, Just (PStr tid) <- [get "tool_call_id" m]]
       in case rest of
            (a : _)
              | role a == "assistant", not (null (toolCalls a)) ->
                  ms <> [PDict [("role", PStr "tool"), ("tool_call_id", tcId tc), ("content", PStr ("Error executing tool: " <> errorMsg))] | tc <- toolCalls a, tcId tc `notElem` map PStr answered]
            _ -> ms <> [PDict [("role", PStr "user"), ("content", PStr ("[System error during processing: " <> errorMsg <> "]"))]]

    stopThinking spRef final = do
      sp <- readIORef spRef
      forM_ sp $ \s -> stopSpinner s (if T.null final then Nothing else Just final)
      writeIORef spRef Nothing
      forM_ (cbThinking (envCallbacks (agEnv ag))) ($ "")

    persistAndReturn ms historyLen result = do
      _ <- persistSession ag ms historyLen
      pure (AReturn result)

    retryLoop :: Loop -> [PyVal] -> Integer -> IORef (Maybe Spinner) -> Double -> Text -> Int -> IO Attempt
    retryLoop lp apiMessages approx spRef t0 taskId historyLen = go 0 0
      where
        env = agEnv ag
        msgs = lpMessages lp
        go :: Int -> Int -> IO Attempt
        go retry compAttempts = do
          s <- readIORef (agSettings ag)
          strip <- readIORef (agStripHeaders ag)
          let tr = if strip then stripDefaultHeaders (envTransport env) else envTransport env
          res <- callModel (envRuntime env) tr (envLlm env) s apiMessages
          t1 <- now ag
          let elapsed = t1 - t0
          intr <- isInterrupted ag
          case res of
            Right _ | not intr -> do
              stopThinking spRef ""
              unlessQuiet ag $ say ag (pfx ag <> "⏱️  API call completed in " <> fmtF 2 elapsed <> "s")
            _ -> pure ()
          if intr
            then do
              stopThinking spRef ""
              say ag (pfx ag <> "⚡ Interrupted during API call.")
              _ <- persistSession ag msgs historyLen
              pure (AInterrupted elapsed)
            else case res of
              Right (CallInvalid v) -> do
                stopThinking spRef "(´;ω;`) oops, retrying..."
                let retry' = retry + 1
                    mode = sMode s
                    details = invalidDetails mode v
                    (errorMsg, provider) = providerOfInvalid v
                say ag (pfx ag <> "⚠️  Invalid API response (attempt " <> int retry' <> "/3): " <> T.intercalate ", " details)
                say ag (pfx ag <> "   🏢 Provider: " <> provider)
                say ag (pfx ag <> "   📝 Provider message: " <> T.take 200 errorMsg)
                say ag (pfx ag <> "   ⏱️  Response time: " <> fmtF 2 elapsed <> "s (fast response often indicates rate limiting)")
                if retry' >= 3
                  then do
                    fb <- tryActivateFallback
                    if fb
                      then go 0 compAttempts
                      else do
                        say ag (pfx ag <> "❌ Max retries (3) exceeded for invalid responses. Giving up.")
                        persistAndReturn msgs historyLen $
                          PDict
                            [ ("messages", PList msgs), ("completed", PBool False), ("api_calls", PInt (fromIntegral (lpCalls lp)))
                            , ("error", PStr "Invalid API response shape. Likely rate limited or malformed provider response."), ("failed", PBool True) ]
                  else do
                    let wait = min (5 * 2 ^ (retry' - 1)) 120 :: Int
                    say ag (pfx ag <> "⏳ Retrying in " <> int wait <> "s (extended backoff for possible rate limit)...")
                    interrupted <- sleepTicks (fromIntegral wait)
                    if interrupted
                      then do
                        say ag (pfx ag <> "⚡ Interrupt detected during retry wait, aborting.")
                        _ <- persistSession ag msgs historyLen
                        clearInterrupt ag
                        pure . AReturn $
                          PDict
                            [ ("final_response", PStr ("Operation interrupted: retrying API call after rate limit (retry " <> int retry' <> "/3).")), ("messages", PList msgs)
                            , ("api_calls", PInt (fromIntegral (lpCalls lp))), ("completed", PBool False), ("interrupted", PBool True) ]
                      else go retry' compAttempts
              Right (CallOk outcome raw) -> do
                let finishPre = oPreFinish outcome
                if finishPre == PStr "length"
                  then lengthTruncated outcome
                  else do
                    accountUsage outcome
                    pure (AOk outcome raw)
              Left err -> do
                stopThinking spRef "(╥_╥) error, retrying..."
                handleError err retry compAttempts elapsed
        sleepTicks :: Double -> IO Bool
        sleepTicks secs = do
          start <- now ag
          let end = start + secs
              loop = do
                t <- now ag
                if t >= end
                  then pure False
                  else do
                    i <- isInterrupted ag
                    if i then pure True else rtSleep (envRuntime env) 0.2 >> loop
          loop
        lengthTruncated outcome = do
          s <- readIORef (agSettings ag)
          say ag (pfx ag <> "⚠️  Response truncated (finish_reason='length') - model hit max output tokens")
          let msg = oMessage outcome
              hasTools = not (null (toolCalls msg))
          if sMode s == ChatCompletions && not hasTools
            then do
              let k = lpLengthRetries lp + 1
                  interim = set "finish_reason" (PStr "length") msg
                  c = contentT msg
                  prefix = lpPrefix lp <> c
                  ms1 = msgs <> [interim]
              if k < 3
                then do
                  say ag (pfx ag <> "↻ Requesting continuation (" <> int k <> "/3)...")
                  let ms2 = ms1 <> [PDict [("role", PStr "user"), ("content", PStr "[System: Your previous response was truncated by the output length limit. Continue exactly where you left off. Do not restart or repeat prior text. Finish the answer directly.]")]]
                  writeIORef (agSessionMessages ag) ms2
                  saveSessionLog ag ms2
                  pure (ARestartLength ms2 prefix)
                else do
                  let partial = T.strip (stripThink prefix)
                  cleanup taskId
                  ms' <- persistSession ag ms1 historyLen
                  pure . AReturn $
                    PDict
                      [ ("final_response", if T.null partial then PNull else PStr partial), ("messages", PList ms'), ("api_calls", PInt (fromIntegral (lpCalls lp)))
                      , ("completed", PBool False), ("partial", PBool True), ("error", PStr "Response remained truncated after 3 continuation attempts") ]
            else
              if length msgs > 1
                then do
                  say ag (pfx ag <> "   ⏪ Rolling back to last complete assistant turn")
                  cleanup taskId
                  persistRollback ag msgs (upToLastAssistant msgs) historyLen
                  pure . AReturn $
                    PDict
                      [ ("final_response", PNull), ("messages", PList (upToLastAssistant msgs)), ("api_calls", PInt (fromIntegral (lpCalls lp)))
                      , ("completed", PBool False), ("partial", PBool True), ("error", PStr "Response truncated due to output length limit") ]
                else do
                  say ag (pfx ag <> "❌ First response truncated - cannot recover")
                  ms' <- persistSession ag msgs historyLen
                  pure . AReturn $
                    PDict
                      [ ("final_response", PNull), ("messages", PList ms'), ("api_calls", PInt (fromIntegral (lpCalls lp)))
                      , ("completed", PBool False), ("failed", PBool True), ("error", PStr "First response truncated due to output length limit") ]
        accountUsage outcome = forM_ (oUsage outcome) $ \u -> do
          comp <- readIORef (agComp ag)
          let comp1 = updateFromResponse (uPrompt u) (uCompletion u) (uTotal u) comp
          writeIORef (agComp ag) comp1
          s <- readIORef (agSettings ag)
          when (cContextProbed comp1) $ do
            when (envWriteFiles env) $ saveContextLength (sysGaussHome (envSys env)) (sModel s) (sBaseUrl s) (cContextLength comp1)
            say ag (pfx ag <> "💾 Cached context length: " <> commas (cContextLength comp1) <> " tokens for " <> sModel s)
            modifyIORef (agComp ag) (\c -> c {cContextProbed = False})
          modifyIORef (agCounters ag) (\(p, c, t, k) -> (p + uPrompt u, c + uCompletion u, t + uTotal u, k + 1))
          modifyIORef' (agCost ag) $ \sc ->
            sc { scDollars = scDollars sc + fromMaybe 0 (uCost u), scPriced = scPriced sc + maybe 0 (const 1) (uCost u)
               , scCalls = scCalls sc + 1, scCacheRead = scCacheRead sc + fromMaybe 0 (uCacheRead u)
               , scCacheWrite = scCacheWrite sc + fromMaybe 0 (uCacheWrite u) }
          case envDb env of
            Just db | cfgPlatform (agCfg ag) == Just "cli" -> do
              sid <- readIORef (agSessionId ag)
              void (try @SomeException (dbUpdateTokenCounts db sid (uPrompt u) (uCompletion u) (Just (sModel s))))
            _ -> pure ()
          caching <- readIORef (agCaching ag)
          when (caching && not (quiet ag)) $ do
            let cached = fromMaybe 0 (uCacheRead u)
                written = fromMaybe 0 (uCacheWrite u)
                prompt = uPrompt u
                pct = if prompt == 0 then 0 else fromIntegral cached / fromIntegral prompt * 100 :: Double
            say ag (pfx ag <> "   💾 Cache: " <> commas cached <> "/" <> commas prompt <> " tokens (" <> fmtF 0 pct <> "% hit, " <> commas written <> " written)")
        -- under CLM an over-long request is a rollback (section 8)
        clmOverflow run ms historyLen' withErr = do
          let setup = crSetup run
          pinned <- readIORef (crPinned run)
          let (pre, ed) = splitAt pinned ms
          (st0, _) <- syncRecordBegins ag (riSystemMessage ri) run ed
          hasTodo <- todoHasItems (agTodo ag)
          todo <- if hasTodo then Todo.formatForInjection (agTodo ag) else pure Nothing
          case overflowRollback (csConfig setup) (mioPath (csIO setup)) todo st0 of
            Left e -> do
              say ag (pfx ag <> "❌ " <> e)
              persistAndReturn ms historyLen' (withErr e)
            Right st1a -> do
              st1 <- case crStore run of
                Nothing -> pure st1a
                Just ref -> do
                  tr <- readIORef (crTranscript run)
                  origin <- readIORef (crOrigin run)
                  s0 <- readIORef ref
                  held0 <- clmStoreHolds ag
                  let (stA, s1, items) = archiveOverflow (mioPath (csIO setup)) tr origin st0 st1a s0
                  writeIORef ref s1
                  reportArchive run items
                  held1 <- clmStoreHolds ag
                  when (held1 && not held0) $ void (storeBegins ag (riSystemMessage ri))
                  pure stA
              writeIORef (crState run) st1
              case reverse (stTurns st1) of
                (Turn _ note : _) -> modifyIORef (crTranscript run) (<> [note])
                [] -> pure ()
              saveClm run
              saveStore run
              say ag (pfx ag <> "⚠️  The provider found the request too long: the newest turn was dropped from the context, retrying...")
              prompt <- fromMaybe "" <$> readIORef (agCachedPrompt ag)
              pure (ARestartCompressed (pre <> map turnMsg (stTurns st1)) prompt)

        handleError err retry compAttempts elapsed = do
          s <- readIORef (agSettings ag)
          let status = case err of
                StatusError c _ _ -> Just c
                _ -> Nothing
          -- 401 refreshes (Codex, Nous, Anthropic) are not wired in yet: the
          -- 401 falls through to the client-error rule, as when a refresh fails
          when (status == Just 401 && sMode s == AnthropicMessages) $ do
            let key = sApiKey s
                p = pfx ag
            say ag (p <> "🔐 Anthropic 401 — authentication failed.")
            say ag (p <> "   Auth method: " <> (if not (T.null key) && not ("sk-ant-api" `T.isPrefixOf` key) then "Bearer (OAuth/setup-token)" else "x-api-key (API key)"))
            say ag (p <> (if T.null key || T.length key <= 12 then "   Token: (empty or short)" else "   Token prefix: " <> T.take 12 key <> "..."))
            say ag (p <> "   Troubleshooting:")
            mapM_ (say ag . (p <>))
              [ "     • Check ANTHROPIC_TOKEN in ~/.gauss/.env for Gauss-managed OAuth/setup tokens"
              , "     • Check ANTHROPIC_API_KEY in ~/.gauss/.env for API keys or legacy token values"
              , "     • For API keys: verify at https://console.anthropic.com/settings/keys"
              , "     • For Claude Code: run 'claude /login' to refresh, then retry"
              , "     • Clear stale keys: gauss config set ANTHROPIC_TOKEN \"\""
              , "     • Legacy cleanup: gauss config set ANTHROPIC_API_KEY \"\"" ]
          let retry' = retry + 1
              etype = errorClass err
              etext = errorText err
              emsg = T.toLower etext
          say ag (pfx ag <> "⚠️  API call failed (attempt " <> int retry' <> "/3): " <> etype)
          say ag (pfx ag <> "   ⏱️  Time elapsed before failure: " <> fmtF 2 elapsed <> "s")
          say ag (pfx ag <> "   📝 Error: " <> T.take 200 etext)
          offered <- offeredTools ag
          say ag (pfx ag <> "   📊 Request context: " <> int (length apiMessages) <> " messages, ~" <> commas approx <> " tokens, " <> int (length offered) <> " tools")
          intr <- isInterrupted ag
          if intr
            then do
              say ag (pfx ag <> "⚡ Interrupt detected during error handling, aborting retries.")
              _ <- persistSession ag msgs historyLen
              clearInterrupt ag
              pure . AReturn $
                PDict
                  [ ("final_response", PStr ("Operation interrupted: handling API error (" <> etype <> ": " <> T.take 80 etext <> ").")), ("messages", PList msgs)
                  , ("api_calls", PInt (fromIntegral (lpCalls lp))), ("completed", PBool False), ("interrupted", PBool True) ]
            else do
              let is413 = status == Just 413 || any (`T.isInfixOf` emsg) ["request entity too large", "payload too large", "error code: 413"]
                  isCtx = any (`T.isInfixOf` emsg) ["context length", "context size", "maximum context", "token limit", "too many tokens", "reduce the length", "exceeds the limit", "context window", "request entity too large", "prompt is too long"]
                  base = PDict [("messages", PList msgs), ("completed", PBool False), ("api_calls", PInt (fromIntegral (lpCalls lp)))]
                  withErr e = case base of
                    PDict kvs -> PDict (kvs <> [("error", PStr e), ("partial", PBool True)])
                    v -> v
              clmRun <- (clmOf <$> readIORef (agContext ag))
              case clmRun of
                Just run | is413 || isCtx -> clmOverflow run msgs historyLen withErr
                _ ->
                  if is413
                    then do
                      let k = compAttempts + 1
                      if k > 3
                        then do
                          say ag (pfx ag <> "❌ Max compression attempts (3) reached for payload-too-large error.")
                          persistAndReturn msgs historyLen (withErr "Request payload too large: max compression attempts (3) reached.")
                        else do
                          say ag (pfx ag <> "⚠️  Request payload too large (413) — compression attempt " <> int k <> "/3...")
                          (ms', p') <- compressContext ag msgs (riSystemMessage ri) (Just approx) taskId
                          smaller <- shrunk msgs ms'
                          if smaller
                            then do
                              say ag (pfx ag <> "   🗜️  Compressed " <> int (length msgs) <> " → " <> int (length ms') <> " messages, retrying...")
                              rtSleep (envRuntime env) 2
                              pure (ARestartCompressed ms' p')
                            else do
                              say ag (pfx ag <> "❌ Payload too large and cannot compress further.")
                              persistAndReturn msgs historyLen (withErr "Request payload too large (413). Cannot compress further.")
                    else
                      if isCtx
                        then do
                          comp <- readIORef (agComp ag)
                          let old = cContextLength comp
                              parsed = parseContextLimitFromError emsg
                          newCtx <- case parsed of
                            Just p | p < old -> do
                              say ag (pfx ag <> "⚠️  Context limit detected from API: " <> commas p <> " tokens (was " <> commas old <> ")")
                              pure (Just p)
                            _ -> pure (nextProbeTier old)
                          stepped <- case newCtx of
                            Just nc | nc < old -> do
                              modifyIORef (agComp ag) (\c -> c {cContextLength = nc, cThresholdTokens = truncate (fromIntegral nc * cThresholdPercent c), cContextProbed = True})
                              say ag (pfx ag <> "⚠️  Context length exceeded — stepping down: " <> commas old <> " → " <> commas nc <> " tokens")
                              pure True
                            _ -> do
                              say ag (pfx ag <> "⚠️  Context length exceeded at minimum tier — attempting compression...")
                              pure False
                          let k = compAttempts + 1
                          if k > 3
                            then do
                              say ag (pfx ag <> "❌ Max compression attempts (3) reached.")
                              persistAndReturn msgs historyLen (withErr "Context length exceeded: max compression attempts (3) reached.")
                            else do
                              say ag (pfx ag <> "   🗜️  Context compression attempt " <> int k <> "/3...")
                              (ms', p') <- compressContext ag msgs (riSystemMessage ri) (Just approx) taskId
                              smaller <- shrunk msgs ms'
                              if smaller || stepped
                                then do
                                  when (length ms' < length msgs) $
                                    say ag (pfx ag <> "   🗜️  Compressed " <> int (length msgs) <> " → " <> int (length ms') <> " messages, retrying...")
                                  rtSleep (envRuntime env) 2
                                  pure (ARestartCompressed ms' p')
                                else do
                                  say ag (pfx ag <> "❌ Context length exceeded and cannot compress further.")
                                  say ag (pfx ag <> "   💡 The conversation has accumulated too much content.")
                                  persistAndReturn msgs historyLen (withErr ("Context length exceeded (" <> commas approx <> " tokens). Cannot compress further."))
                        else do
                          let clientMarkers =
                                [ "error code: 401", "error code: 403", "error code: 404", "error code: 422", "is not a valid model", "invalid model"
                                , "model not found", "invalid api key", "invalid_api_key", "authentication", "unauthorized", "forbidden", "not found" ]
                              isClient =
                                etype `elem` ["ValueError", "TypeError"]
                                  || maybe False (\c -> c >= 400 && c < 500 && c /= 413) status
                                  || any (`T.isInfixOf` emsg) clientMarkers
                          if isClient
                            then do
                              fb <- tryActivateFallback
                              if fb
                                then go 0 compAttempts
                                else do
                                  s' <- readIORef (agSettings ag)
                                  hr <- buildFor s'
                                  forM_ hr $ \h -> dumpRequest ag h "non_retryable_client_error" (Just err)
                                  say ag (pfx ag <> "❌ Non-retryable client error detected. Aborting immediately.")
                                  say ag (pfx ag <> "   💡 This type of error won't be fixed by retrying.")
                                  persistAndReturn msgs historyLen $
                                    PDict
                                      [ ("final_response", PNull), ("messages", PList msgs), ("api_calls", PInt (fromIntegral (lpCalls lp)))
                                      , ("completed", PBool False), ("failed", PBool True), ("error", PStr etext) ]
                            else
                              if retry' >= 3
                                then do
                                  fb <- tryActivateFallback
                                  if fb
                                    then go 0 compAttempts
                                    else do
                                      say ag (pfx ag <> "❌ Max retries (3) exceeded. Giving up.")
                                      pure (ARaise err)
                                else do
                                  let wait = min (2 ^ retry') 60 :: Int
                                  interrupted <- sleepTicks (fromIntegral wait)
                                  if interrupted
                                    then do
                                      say ag (pfx ag <> "⚡ Interrupt detected during retry wait, aborting.")
                                      _ <- persistSession ag msgs historyLen
                                      clearInterrupt ag
                                      pure . AReturn $
                                        PDict
                                          [ ("final_response", PStr ("Operation interrupted: retrying API call after error (retry " <> int retry' <> "/3).")), ("messages", PList msgs)
                                          , ("api_calls", PInt (fromIntegral (lpCalls lp))), ("completed", PBool False), ("interrupted", PBool True) ]
                                    else go retry' compAttempts
        buildFor s' = either (const Nothing) Just <$> buildRequest (envLlm env) s' apiMessages
        -- fewer messages; under RLM also a cut of the tail's results (spec 3.4)
        shrunk old new = do
          rlm <- isRlm ag
          pure (length new < length old || (rlm && new /= old))

    cleanup taskId = do
      r <- try @SomeException (trCleanup (envTools (agEnv ag)) taskId)
      either (const (pure ())) pure r

    -- | @_try_activate_fallback()@
    tryActivateFallback :: IO Bool
    tryActivateFallback = do
      activated <- readIORef (agFallbackActivated ag)
      fb <- readIORef (agFallback ag)
      case (activated, fb, envAuxEnv (agEnv ag)) of
        (False, Just (p0, m0), Just aux) -> do
          let p = T.toLower (T.strip p0)
              m = T.strip m0
          if T.null p || T.null m
            then pure False
            else do
              (client, _) <- resolveProviderClient aux p (Just m) Nothing Nothing
              case client of
                Nothing -> do
                  envWarn (agEnv ag) ("Fallback to " <> p <> " failed: provider not configured")
                  pure False
                Just cl -> do
                  let mode = case p of
                        "openai-codex" -> CodexResponses
                        "anthropic" -> AnthropicMessages
                        _ -> ChatCompletions
                      base = case clKind cl of
                        AnthropicClient -> clBaseUrl cl
                        _ -> if "/" `T.isSuffixOf` clBaseUrl cl then clBaseUrl cl else clBaseUrl cl <> "/"
                  modifyIORef (agSettings ag) (\s -> s {sModel = m, sProvider = p, sBaseUrl = base, sMode = mode, sApiKey = clApiKey cl})
                  writeIORef (agFallbackActivated ag) True
                  writeIORef (agStripHeaders ag) (mode == ChatCompletions)
                  writeIORef (agCaching ag) (usesPromptCaching base m (mode == AnthropicMessages))
                  say ag (pfx ag <> "🔄 Primary model failed — switching to fallback: " <> m <> " via " <> p)
                  pure True
        _ -> pure False

    -- | Response processing (§4.6–4.9): the next step of the loop.
    processResponse :: Loop -> Outcome -> PyVal -> Text -> Int -> IO (IO AgentResult)
    processResponse lp outcome rawResp taskId historyLen = do
      s <- readIORef (agSettings ag)
      let cfg = agCfg ag
          mode = sMode s
          msg0 = oMessage outcome
          content = contentT msg0
          finishReason = oFinish outcome
          msgs = lpMessages lp
          n = lpCalls lp
      when (not (T.null content) && not (quiet ag)) $
        say ag (pfx ag <> "🤖 Assistant: " <> (if cfgVerbose cfg then content else T.take 100 content <> (if T.length content > 100 then "..." else "")))
      if "<REASONING_SCRATCHPAD>" `T.isInfixOf` content && not ("</REASONING_SCRATCHPAD>" `T.isInfixOf` content)
        then do
          k <- atomicModifyIORef' (agScratch ag) (\x -> (x + 1, x + 1))
          say ag (pfx ag <> "⚠️  Incomplete <REASONING_SCRATCHPAD> detected (opened but never closed)")
          if k <= 2
            then do
              say ag (pfx ag <> "🔄 Retrying API call (" <> int k <> "/2)...")
              pure (outer lp taskId historyLen)
            else do
              say ag (pfx ag <> "❌ Max retries (2) for incomplete scratchpad. Saving as partial.")
              writeIORef (agScratch ag) 0
              let rolled = upToLastAssistant msgs
              cleanup taskId
              persistRollback ag msgs rolled historyLen
              pure . pure . RunDone $
                PDict
                  [ ("final_response", PNull), ("messages", PList rolled), ("api_calls", PInt (fromIntegral n))
                  , ("completed", PBool False), ("partial", PBool True), ("error", PStr "Incomplete REASONING_SCRATCHPAD after 2 retries") ]
        else do
          writeIORef (agScratch ag) 0
          codexIncomplete <-
            if mode == CodexResponses && finishReason == PStr "incomplete"
              then do
                k <- atomicModifyIORef' (agCodexIncomplete ag) (\x -> (x + 1, x + 1))
                let interim = set "finish_reason" (PStr "incomplete") msg0
                    hasText = not (T.null (T.strip (contentT interim))) || truthy (fromMaybe PNull (get "reasoning" interim))
                    dup = case reverse msgs of
                      (l : _) ->
                        role l == "assistant" && get "finish_reason" l == Just (PStr "incomplete")
                          && get "content" l == get "content" interim && get "reasoning" l == get "reasoning" interim
                      [] -> False
                    msgs' = if hasText && not dup then msgs <> [interim] else msgs
                if k < 3
                  then do
                    unlessQuiet ag $ say ag (pfx ag <> "↻ Codex response incomplete; continuing turn (" <> int k <> "/3)")
                    saveSessionLog ag msgs'
                    pure (Just (outer lp {lpMessages = msgs'} taskId historyLen))
                  else do
                    writeIORef (agCodexIncomplete ag) 0
                    ms' <- persistSession ag msgs' historyLen
                    pure . Just . pure . RunDone $
                      PDict
                        [ ("final_response", PNull), ("messages", PList ms'), ("api_calls", PInt (fromIntegral n))
                        , ("completed", PBool False), ("partial", PBool True), ("error", PStr "Codex response remained incomplete after 3 continuation attempts") ]
              else writeIORef (agCodexIncomplete ag) 0 >> pure Nothing
          case codexIncomplete of
            Just next -> pure next
            Nothing ->
              if not (null (toolCalls msg0))
                then writeIORef (agRefused ag) 0 >> writeIORef (agTextCalls ag) 0 >> toolBranch lp msg0 finishReason taskId historyLen
                else finalBranch lp msg0 rawResp finishReason taskId historyLen

    toolBranch :: Loop -> PyVal -> PyVal -> Text -> Int -> IO (IO AgentResult)
    toolBranch lp msg0 finishReason taskId historyLen = do
      -- with a store on its tools are always accepted (over an empty store
      -- they answer so), though they are offered only once it holds
      -- something: recall under RLM, read_stored and recall under CLM
      (storeNames, _) <- storeTools ag
      let calls0 = toolCalls msg0
          extra = filter (`notElem` agValid ag) storeNames
          valid = if null extra then agValid ag else sort (extra <> agValid ag)
          msgs = lpMessages lp
          n = lpCalls lp
      unlessQuiet ag $ say ag (pfx ag <> "🔧 Processing " <> int (length calls0) <> " tool call(s)...")
      calls1 <- forM calls0 $ \tc -> do
        let nm = tcName tc
        if nm `elem` valid
          then pure tc
          else case repairToolName valid nm of
            Just fixed -> do
              say ag (pfx ag <> "🔧 Auto-repaired tool name: '" <> nm <> "' -> '" <> fixed <> "'")
              pure (setTc "name" (PStr fixed) tc)
            Nothing -> pure tc
      let invalid = [tcName tc | tc <- calls1, tcName tc `notElem` valid]
          msg1 = set "tool_calls" (PList calls1) msg0
      if not (null invalid)
        then do
          k <- atomicModifyIORef' (agInvalidTool ag) (\x -> (x + 1, x + 1))
          let available = T.intercalate ", " valid
              first = head invalid
              shown = if T.length first > 80 then T.take 80 first <> "..." else first
          say ag (pfx ag <> "⚠️  Unknown tool '" <> shown <> "' — sending error to model for self-correction (" <> int k <> "/3)")
          if k >= 3
            then do
              say ag (pfx ag <> "❌ Max retries (3) for invalid tool calls exceeded. Stopping as partial.")
              writeIORef (agInvalidTool ag) 0
              ms' <- persistSession ag msgs historyLen
              pure . pure . RunDone $
                PDict
                  [ ("final_response", PNull), ("messages", PList ms'), ("api_calls", PInt (fromIntegral n))
                  , ("completed", PBool False), ("partial", PBool True), ("error", PStr ("Model generated invalid tool call: " <> shown)) ]
            else do
              let results =
                    [ PDict
                        [ ("role", PStr "tool"), ("tool_call_id", tcId tc)
                        , ( "content"
                          , PStr
                              ( if tcName tc `notElem` valid
                                  then "Tool '" <> tcName tc <> "' does not exist. Available tools: " <> available
                                  else "Skipped: another tool call in this turn used an invalid name. Please retry this tool call." ) ) ]
                    | tc <- calls1 ]
              pure (outer lp {lpMessages = msgs <> [set "finish_reason" finishReason msg1] <> results} taskId historyLen)
        else do
          writeIORef (agInvalidTool ag) 0
          let normalized = map normArgs calls1
              bad = [(tcName tc, e) | tc <- normalized, PStr a <- [tcArgs tc], Just e <- [jsonError a]]
          case bad of
            ((tool, err) : _) -> do
              k <- atomicModifyIORef' (agInvalidJson ag) (\x -> (x + 1, x + 1))
              say ag (pfx ag <> "⚠️  Invalid JSON in tool call arguments for '" <> tool <> "': " <> err)
              if k < 3
                then do
                  say ag (pfx ag <> "🔄 Retrying API call (" <> int k <> "/3)...")
                  pure (outer lp taskId historyLen)
                else do
                  say ag (pfx ag <> "⚠️  Injecting recovery message for invalid JSON...")
                  writeIORef (agInvalidJson ag) 0
                  let recovery =
                        "Your tool call to '" <> tool <> "' had invalid JSON arguments. Error: " <> err
                          <> ". For tools with no required parameters, use an empty object: {}. Please either retry the tool call with valid JSON, or respond without using that tool."
                  pure (outer lp {lpMessages = msgs <> [PDict [("role", PStr "user"), ("content", PStr recovery)]]} taskId historyLen)
            [] -> do
              writeIORef (agInvalidJson ag) 0
              let assistantMsg = set "tool_calls" (PList normalized) (set "finish_reason" finishReason msg1)
                  content = contentT assistantMsg
              when (not (T.null content) && hasContentAfterThink content) $ do
                writeIORef (agLastContentWithTools ag) (Just content)
                when (quiet ag) $ do
                  let clean = T.strip (stripThink content)
                  unless (T.null clean) $ say ag ("  ┊ 💬 " <> clean)
              clmRun <- (clmOf <$> readIORef (agContext ag))
              case clmRun of
                Just run -> clmBranch run lp assistantMsg normalized taskId historyLen
                Nothing -> compressBranch lp assistantMsg normalized taskId historyLen
      where
        normArgs tc = case tcArgs tc of
          a@(PDict _) -> setTc "arguments" (PStr (dumpsAscii a)) tc
          a@(PList _) -> setTc "arguments" (PStr (dumpsAscii a)) tc
          PStr a | T.null (T.strip a) -> setTc "arguments" (PStr "{}") tc
          PStr _ -> tc
          PNull -> setTc "arguments" (PStr "{}") tc
          a -> let t = pyStrOf a in if T.null (T.strip t) then setTc "arguments" (PStr "{}") tc else setTc "arguments" (PStr t) tc

    -- | The batch under upstream's compressor.
    compressBranch :: Loop -> PyVal -> [PyVal] -> Text -> Int -> IO (IO AgentResult)
    compressBranch lp assistantMsg normalized taskId historyLen = do
              let msgs = lpMessages lp
                  n = lpCalls lp
                  msgs1 = msgs <> [assistantMsg]
                  before = length msgs1
              msgs2 <- (<>) <$> executeToolCalls normalized msgs1 taskId n <*> inboxTurns
              when (map tcName normalized == ["execute_code"] || (not (null normalized) && all ((== "execute_code") . tcName) normalized)) $
                modifyIORef (agBudget ag) (\(u, m) -> (if u > 0 then u - 1 else u, m))
              comp <- readIORef (agComp ag)
              let newChars = sum [fromIntegral (T.length (contentText m)) | m <- drop before msgs2] :: Integer
                  est = cLastPrompt comp + cLastCompletion comp + newChars `div` 3
              rlmRun <- rlmOf <$> readIORef (agContext ag)
              let rlmOn = isJust rlmRun
              forM_ rlmRun $ \run -> modifyIORef' (rrSinceOffload run) (+ 1)
              let over = (agCompressionEnabled ag || rlmOn) && shouldCompress comp est
              allowed <- if over then offloadAllowed ag msgs2 (lpPrompt lp) else pure False
              (msgs3, prompt3) <-
                if allowed
                  then do
                    (ms', p') <- compressContext ag msgs2 (riSystemMessage ri) (Just (cLastPrompt comp)) taskId
                    pure (ms', Just p')
                  else
                    if over
                      then maybe (msgs2, lpPrompt lp) (\(ms', p') -> (ms', Just p')) <$> cutOnly ag msgs2 (riSystemMessage ri)
                      else pure (msgs2, lpPrompt lp)
              writeIORef (agSessionMessages ag) msgs3
              saveSessionLog ag msgs3
              pure (outer lp {lpMessages = msgs3, lpPrompt = prompt3} taskId historyLen)
      where
        contentText m = case get "content" m of
          Just (PStr t) -> t
          Just PNull -> ""
          Just v -> pyStrOf v
          Nothing -> ""

    -- | The batch under CLM (docs/clm.md section 6).
    clmBranch :: ClmRun -> Loop -> PyVal -> [PyVal] -> Text -> Int -> IO (IO AgentResult)
    clmBranch run lp assistantMsg normalized taskId historyLen = do
      let setup = crSetup run
          io = csIO setup
          path = mioPath io
          n = lpCalls lp
          observe = csObserve setup
      pinned <- readIORef (crPinned run)
      let (pre, ed) = splitAt pinned (lpMessages lp)
      -- whether the store holds an item before this batch (step 1's sync
      -- may store the first)
      heldBefore <- clmStoreHolds ag
      -- step 1
      st1 <- shortenReads path <$> syncRecord run ed
      writeIORef (crState run) st1
      let mirror = render (csSessionId setup) (stTurns st1)
      mioWrite io mirror
      observe (EvMirrorWrite mirror)
      -- step 2
      let msgsT = pre <> map turnMsg (stTurns st1) <> [assistantMsg]
      msgs2 <- executeToolCalls normalized msgsT taskId n
      let toolMsgs = drop (length msgsT) msgs2
      p0 <- length <$> readIORef (crTranscript run)
      modifyIORef (crTranscript run) (<> (assistantMsg : toolMsgs))
      -- steps 3 to 6
      file <- mioRead io
      observe (EvMirrorRead file)
      st2 <- readIORef (crState run)
      hasTodo <- todoHasItems (agTodo ag)
      todo <- if hasTodo then Todo.formatForInjection (agTodo ag) else pure Nothing
      -- with the store on: spill (spec 3.1), then archive (3.2)
      let tools0 = [(m, contentT m) | m <- toolMsgs]
      spilt <- forM (crStore run) $ \ref -> do
        s0 <- readIORef ref
        pure (spillBatch (csConfig setup) path (p0 + 1) assistantMsg tools0 s0)
      let (tools, spilled) = maybe (tools0, S.empty) (\(t, sp, _, _) -> (t, sp)) spilt
          out0 =
            afterBatch
              BatchIn
                { biConfig = csConfig setup, biPath = path, biSession = csSessionId setup, biState = st2
                , biMirror = mirror, biFile = file, biAssistant = assistantMsg
                , biTools = tools, biTodo = todo, biSpilled = spilled }
      out <- case (crStore run, spilt) of
        (Just ref, Just (_, _, s1, spills)) -> do
          tr <- readIORef (crTranscript run)
          origin <- readIORef (crOrigin run)
          let (o, origin', s2, items) = archiveBatch path tr origin p0 (stTurns st2) out0 s1
              turnAt = M.fromList [(p, i) | (i, p) <- M.toList origin']
          writeIORef (crOrigin run) origin'
          writeIORef ref s2
          -- each spill, with the CLM turn its preview is (none when the
          -- batch rolled back; the stored item keeps none)
          mapM_ (\it -> csOnSpill setup it {itTurn = itOrigin it >>= (`M.lookup` turnAt)}) spills
          reportArchive run items
          pure o
        _ -> pure out0
      let flag k = lookup k (boFlags out)
      writeIORef (crState run) ((boState out) {stLastMirror = Just mirror})
      -- the rollback note is new
      when (boOutcome out == RolledBack) $ case reverse (stTurns (boState out)) of
        (Turn _ note : _) -> modifyIORef (crTranscript run) (<> [note])
        [] -> pure ()
      saveClm run
      saveStore run
      -- the first item: the store's tools and section from the next request
      heldAfter <- clmStoreHolds ag
      prompt' <- if heldAfter && not heldBefore then Just <$> storeBegins ag (riSystemMessage ri) else pure (lpPrompt lp)
      forM_ (boBackup out) (mioWriteBackup io)
      observe (EvBatch (boFlags out))
      let msgs3 = pre <> map turnMsg (stTurns (boState out))
          refund = modifyIORef (agBudget ag) (\(u, m) -> (if u > 0 then u - 1 else u, m))
      when (boFlush out) $ flushMemories ag (Just msgs3) Nothing
      case boOutcome out of
        Stopped err -> do
          writeIORef (agSessionMessages ag) msgs3
          cleanup taskId
          ms' <- persistSession ag msgs3 historyLen
          pure . pure . RunDone $
            PDict
              [ ("final_response", PNull), ("messages", PList ms'), ("api_calls", PInt (fromIntegral n))
              , ("completed", PBool False), ("partial", PBool True), ("error", PStr err) ]
        oc -> do
          when (oc == RolledBack || isJust (flag "free")) refund
          -- the inbox's messages, as user turns after the batch (the next
          -- request's sync records them)
          sent <- inboxTurns
          let msgs4 = msgs3 <> sent
          writeIORef (agSessionMessages ag) msgs4
          saveSessionLog ag msgs4
          pure (outer lp {lpMessages = msgs4, lpPrompt = prompt'} taskId historyLen)

    finalBranch :: Loop -> PyVal -> PyVal -> PyVal -> Text -> Int -> IO (IO AgentResult)
    finalBranch lp msg0 rawResp finishReason taskId historyLen = do
      s <- readIORef (agSettings ag)
      let msgs = lpMessages lp
          n = lpCalls lp
          final0 = contentT msg0
          refusal = if hasContentAfterThink final0 then Nothing else responseRefusal rawResp <|> (get "refusal" msg0 >>= nonEmptyStr)
          nonEmptyStr = \case PStr r | not (T.null (T.strip r)) -> Just (T.strip r); _ -> Nothing
      case refusal of
        -- Fisher only: a provider's refusal is not the model's answer
        -- (upstream falls back to the last text it saw); retry once, then
        -- stop with the refusal as the error
        Just why -> do
          k <- atomicModifyIORef' (agRefused ag) (\x -> (x + 1, x + 1))
          say ag (pfx ag <> "⚠️  Provider refused the request: " <> T.take 300 why)
          if k < 2
            then do
              say ag (pfx ag <> "🔄 Retrying API call (" <> int k <> "/1)...")
              pure (outer lp taskId historyLen)
            else do
              writeIORef (agRefused ag) 0
              writeIORef (agLastContentWithTools ag) Nothing
              cleanup taskId
              ms' <- persistSession ag msgs historyLen
              pure . pure . RunDone $
                PDict
                  [ ("final_response", PNull), ("messages", PList ms'), ("api_calls", PInt (fromIntegral n))
                  , ("completed", PBool False), ("partial", PBool True), ("error", PStr ("Provider refused the request: " <> why)) ]
        Nothing -> do
          writeIORef (agRefused ag) 0
          if not (hasContentAfterThink final0)
            then do
              lcwt <- readIORef (agLastContentWithTools ag)
              case lcwt of
                Just fallback -> do
                  writeIORef (agLastContentWithTools ag) Nothing
                  writeIORef (agEmpty ag) 0
                  let ms' = rewriteCallingThe msgs
                  writeIORef (agPreviewed ag) True
                  pure (finish lp {lpMessages = ms', lpFinal = Just (T.strip (stripThink fallback))} taskId historyLen)
                Nothing -> do
                  k <- atomicModifyIORef' (agEmpty ag) (\x -> (x + 1, x + 1))
                  let reasoning = case sMode s of
                        ChatCompletions -> extractReasoning (fst (normalizeChat rawResp))
                        AnthropicMessages -> extractReasoning (fst (normalizeAnthropic rawResp))
                        CodexResponses -> case get "reasoning" msg0 of
                          Just (PStr r) | not (T.null r) -> Just r
                          _ -> Nothing
                  say ag (pfx ag <> "⚠️  Response only contains think block with no content after it")
                  case reasoning of
                    Just r -> say ag (pfx ag <> "   Reasoning: " <> T.take 500 r <> (if T.length r > 500 then "..." else ""))
                    Nothing -> say ag (pfx ag <> "   Content: '" <> T.take 80 final0 <> (if T.length final0 > 80 then "..." else "") <> "'")
                  if k < 3
                    then do
                      say ag (pfx ag <> "🔄 Retrying API call (" <> int k <> "/3)...")
                      pure (outer lp taskId historyLen)
                    else do
                      say ag (pfx ag <> "❌ Max retries (3) for empty content exceeded.")
                      writeIORef (agEmpty ag) 0
                      lcwt' <- readIORef (agLastContentWithTools ag)
                      case lcwt' of
                        Just fallback -> do
                          writeIORef (agLastContentWithTools ag) Nothing
                          let ms' = rewriteCallingThe msgs
                          writeIORef (agPreviewed ag) True
                          pure (finish lp {lpMessages = ms', lpFinal = Just (T.strip (stripThink fallback))} taskId historyLen)
                        Nothing -> do
                          let ms1 = msgs <> [PDict [("role", PStr "assistant"), ("content", PStr final0), ("reasoning", maybe PNull PStr reasoning), ("finish_reason", finishReason)]]
                          cleanup taskId
                          ms' <- persistSession ag ms1 historyLen
                          pure . pure . RunDone $
                            PDict
                              [ ("final_response", if T.null final0 then PNull else PStr final0), ("messages", PList ms'), ("api_calls", PInt (fromIntegral n))
                              , ("completed", PBool False), ("partial", PBool True), ("error", PStr "Model generated only think blocks with no actual response after 3 retries") ]
            else do
              writeIORef (agEmpty ag) 0
              if sMode s == CodexResponses && not (null (agValid ag)) && lpAck lp < 2 && looksLikeAck (riUserMessage ri) final0 msgs
                then do
                  let ms' = msgs <> [set "finish_reason" (PStr "incomplete") msg0, PDict [("role", PStr "user"), ("content", PStr "[System: Continue now. Execute the required tool calls and only send your final answer after completing the task.]")]]
                  saveSessionLog ag ms'
                  pure (outer lp {lpMessages = ms', lpAck = lpAck lp + 1} taskId historyLen)
                else do
                  let final1 = T.strip (stripThink (lpPrefix lp <> final0))
                      ms' = msgs <> [set "finish_reason" finishReason msg0]
                  clm <- (clmOf <$> readIORef (agContext ag))
                  k <- readIORef (agTextCalls ag)
                  -- Fisher only: under CLM a call written as text in the
                  -- context file's notation runs nothing; send it back
                  -- (twice at most) instead of ending the run on it
                  if isJust clm && k < 2 && hasTextCall final1
                    then do
                      writeIORef (agTextCalls ag) (k + 1)
                      say ag (pfx ag <> "⚠️  The reply has a call written as text; sending it back (" <> int (k + 1) <> "/2)")
                      let ms2 = ms' <> [PDict [("role", PStr "user"), ("content", PStr textCallNote)]]
                      saveSessionLog ag ms2
                      pure (outer lp {lpMessages = ms2} taskId historyLen)
                    else do
                      writeIORef (agTextCalls ag) 0
                      unlessQuiet ag $ say ag ("🎉 Conversation completed after " <> int n <> " OpenAI-compatible API call(s)")
                      pure (finish lp {lpMessages = ms', lpFinal = Just final1, lpAck = 0} taskId historyLen)

    rewriteCallingThe ms =
      case [i | (i, m) <- zip [0 :: Int ..] ms, role m == "assistant", not (null (toolCalls m))] of
        [] -> ms
        is ->
          let i = last is
              m = ms !! i
              names = [maybe "unknown" pyStrOf (get "function" tc >>= get "name") | tc <- toolCalls m]
              text = "Calling the " <> T.intercalate ", " names <> " tool" <> (if length names > 1 then "s" else "") <> "..."
           in [if j == i then set "content" (PStr text) x else x | (j, x) <- zip [0 ..] ms]

    looksLikeAck userMessage assistantContent ms
      | any ((== "tool") . role) ms = False
      | otherwise =
          let t = T.toLower (T.strip (stripThink assistantContent))
              u = T.toLower (T.strip userMessage)
              actions = ["look into", "look at", "inspect", "scan", "check", "analyz", "review", "explore", "read", "open", "run", "test", "fix", "debug", "search", "find", "walkthrough", "report back", "summarize"]
              workspace = ["directory", "current directory", "current dir", "cwd", "repo", "repository", "codebase", "project", "folder", "filesystem", "file tree", "files", "path"]
              userSide = any (`T.isInfixOf` u) workspace || "~/" `T.isInfixOf` u || "/" `T.isInfixOf` u
           in not (T.null t) && T.length t <= 1200 && isJust (Re.search ackRx t)
                && (userSide || any (`T.isInfixOf` t) workspace) && any (`T.isInfixOf` t) actions
    ackRx = Re.compileOrDie [] "\\b(i['’]ll|i will|let me|i can do that|i can help with that)\\b"

    -- | @_execute_tool_calls@
    -- | Fisher only: what was sent to the session since the last tool
    -- boundary, as user messages.
    inboxTurns :: IO [PyVal]
    inboxTurns = do
      ts <- envInbox (agEnv ag)
      pure [PDict [("role", PStr "user"), ("content", PStr t)] | t <- ts]

    executeToolCalls :: [PyVal] -> [PyVal] -> Text -> Int -> IO [PyVal]
    executeToolCalls calls msgs taskId apiCount = do
      clmRun <- (clmOf <$> readIORef (agContext ag))
      -- under CLM, calls that touch the context file run in order (section 3)
      let touches = case clmRun of
            Just run -> any (touchesContext (mioPath (csIO (crSetup run)))) calls
            Nothing -> False
      -- Fisher only: calls that write a file another call in the batch
      -- reads or writes run in order (upstream runs them in parallel and
      -- loses edits)
      cwd <- envCwd (agEnv ag)
      if length calls <= 1 || any ((== "clarify") . tcName) calls || touches || sameFileWrite cwd calls
        then sequential calls msgs taskId apiCount
        else concurrent calls msgs taskId apiCount

    sameFileWrite cwd calls = batchWritesSameFile cwd [(tcName tc, parseArgs (tcArgs tc)) | tc <- calls]

    touchesContext path tc = case parseArgs (tcArgs tc) of
      a@(PDict _)
        | Just (PStr q) <- get "path" a -> samePath q path || samePath q (prevPathOf path)
        | tcName tc `elem` ["terminal", "execute_code"], Just (PStr t) <- get "command" a -> mentionsContext path t
        | tcName tc `elem` ["terminal", "execute_code"], Just (PStr t) <- get "code" a -> mentionsContext path t
      _ -> False

    -- | @_invoke_tool@: todo and memory run here, the rest through the
    -- tool layer. With a store on, its tools run here too, and not through
    -- 'envLocalTool': a replay runs recall again, its model calls answered
    -- from the recorded transport, and computes read_stored again from the
    -- store (spec 4).
    -- | 'invoke', reporting the call's end to 'cbToolDone'.
    invokeReported tc name args taskId = do
      r <- try @SomeException (invoke name args taskId)
      forM_ (cbToolDone (envCallbacks (agEnv ag))) $ \f ->
        void (try @SomeException (f taskId (pyStrOf (tcId tc)) name args (either (Left . T.pack . displayException) Right r)))
      either throwIO pure r

    invoke name args taskId = do
      ctxRun <- readIORef (agContext ag)
      case ctxRun of
        RlmRunning run
          | name == "recall" ->
              let setup = rrSetup run
               in recallTool (rlConfig setup) (rlRepl setup) (map renderTurn . rsStore <$> readIORef (rrState run)) (rlOnRecall setup) args
        ClmRunning run
          | Just ref <- crStore run
          , name == "recall"
          , Just (rc, repl) <- csRecall (crSetup run) ->
              recallTool rc repl (map renderItem . sItems <$> readIORef ref) (csOnRecall (crSetup run)) args
          | Just ref <- crStore run
          , name == "read_stored" ->
              readStoredTool (clmLimit (csConfig (crSetup run))) ref args
        _ -> invokeTool name args taskId

    -- recall over the store's rendered items (phase 2's, spec 3.3)
    recallTool :: RlmConfig -> ReplFactory -> IO [Text] -> (Text -> RecallResult -> IO ()) -> PyVal -> IO Text
    recallTool rc repl texts onRecall args = case get "question" args of
      Just (PStr q) -> do
        s <- readIORef (agSettings ag)
        strip <- readIORef (agStripHeaders ag)
        store <- texts
        let env = agEnv ag
            tr0 = envTransport env
            -- recall's calls offer no tools: no tools key at all
            tr = withoutNullTools (if strip then stripDefaultHeaders tr0 else tr0)
            models = liveModels (envRuntime env) tr (envLlm env) rc (recallSettings s)
            renv = RecallEnv rc models repl (sysMonotonic (envSys env))
        r <- recallTexts renv store q
        atomicModifyIORef' (agCost ag) $ \sc ->
          ( sc { scDollars = scDollars sc + rcDollars r, scPriced = scPriced sc + rcPriced r
               , scCalls = scCalls sc + rcCalls r, scCacheRead = scCacheRead sc + rcCacheRead r
               , scCacheWrite = scCacheWrite sc + rcCacheWrite r }
          , () )
        onRecall q r
        when (rcEnding r == R.Stopped "no credit") $
          throwIO (RecallNoCredit "Error code: 402 (recall's model call: the key's credit limit is reached)")
        pure (rcText r)
      _ -> pure "Error: recall needs a question (a string)."

    -- read_stored(id, from_line, to_line): lines of an item, no model call
    readStoredTool :: Integer -> IORef Store -> PyVal -> IO Text
    readStoredTool limit ref args = case (get "id" args, get "from_line" args, get "to_line" args) of
      (Just (PStr i), Just (PInt a), Just (PInt b)) -> do
        store <- readIORef ref
        pure (readStored limit store i (clampInt a) (clampInt b))
      _ -> pure "Error: read_stored needs an id (a string such as \"S3\") and from_line and to_line (integers)."
    -- a line number beyond Int's range reads as its nearest bound
    clampInt :: Integer -> Int
    clampInt = fromInteger . max (toInteger (minBound :: Int)) . min (toInteger (maxBound :: Int))

    -- recall's 402 is not a tool error: it stops the session
    rethrowStop :: Either SomeException a -> IO ()
    rethrowStop = \case
      Left e | Just stop@(RecallNoCredit _) <- fromException e -> throwIO stop
      _ -> pure ()

    invokeTool name args taskId = case localTool name args of
      Just run -> envLocalTool (agEnv ag) name args run
      Nothing -> do
        let tr = envTools (agEnv ag)
            valid = agValid ag
        clmRun <- (clmOf <$> readIORef (agContext ag))
        case clmRun of
          Just run
            | name == "read_file"
            , Just (PStr q) <- get "path" args
            , samePath q (mioPath (csIO (crSetup run))) -> do
                -- what the model sees in the mirror (rule 3 of section 5)
                before <- mioRead (csIO (crSetup run))
                res <- trCall tr name args taskId (if null valid then Nothing else Just valid)
                case (TE.decodeUtf8' <$> before, loadsT res) of
                  (Just (Right t), r) | not (isErr r) -> modifyIORef (crState run) (\st -> st {stSeen = headerIds t})
                  _ -> pure ()
                pure res
          _ -> trCall tr name args taskId (if null valid then Nothing else Just valid)
      where
        isErr = \case
          Just d@(PDict _) -> isJust (get "error" d)
          _ -> False

    localTool name args = case name of
      "todo" -> Just (todoToolArgs (Just (agTodo ag)) args)
      "memory" -> Just (memoryToolArgs False (agMemStore ag) args)
      _ -> Nothing

    -- the nudge counter restarts when the memory tool is used
    noteUse name = when (name == "memory") (writeIORef (agTurnsSinceMemory ag) 0)

    emojiFor name = toolEmoji (envSkin (agEnv ag)) (trEmoji (envTools (agEnv ag))) name

    progress name args =
      forM_ (cbToolProgress (envCallbacks (agEnv ag))) $ \f ->
        void (try @SomeException (f name (buildToolPreviewV name args) args))

    truncateResult r =
      if T.length r > 100000
        then T.take 100000 r <> "\n\n[Truncated: tool response was " <> commas (fromIntegral (T.length r)) <> " chars, exceeding the 100,000 char limit]"
        else r

    preview t =
      let k = cfgLogPrefixChars (agCfg ag)
       in if T.length t > k then T.take k t <> "..." else t

    callLine i name args =
      let argsStr = dumpsUnicode args
          keys' = pyRepr (PList (map PStr (keys args)))
       in if cfgVerbose (agCfg ag)
            then [ "  📞 Tool " <> int i <> ": " <> name <> "(" <> keys' <> ")", "     Args: " <> argsStr ]
            else ["  📞 Tool " <> int i <> ": " <> name <> "(" <> keys' <> ") - " <> preview argsStr]

    doneLines i dur result =
      if cfgVerbose (agCfg ag)
        then ["  ✅ Tool " <> int i <> " completed in " <> fmtF 2 dur <> "s", "     Result: " <> result]
        else ["  ✅ Tool " <> int i <> " completed in " <> fmtF 2 dur <> "s - " <> preview result]

    injectBudget msgs apiCount = do
     clmRun <- (clmOf <$> readIORef (agContext ag))
     -- under CLM the context line, not the iteration warning, rides on results
     if isJust clmRun then pure msgs else
      case budgetWarning apiCount (cfgMaxIterations (agCfg ag)) of
        Just w | (l : _) <- reverse msgs, role l == "tool" -> do
          let lastContent = contentT l
              newContent = case loadsT lastContent of
                Just d@(PDict _) -> dumpsUnicode (set "_budget_warning" (PStr w) d)
                _ -> lastContent <> "\n\n" <> w
              maxIt = cfgMaxIterations (agCfg ag)
              remaining = maxIt - apiCount
          unlessQuiet ag $
            say ag (pfx ag <> (if fromIntegral remaining <= (fromIntegral maxIt * 0.1 :: Double) then "⚠️  WARNING" else "💡 CAUTION") <> ": " <> int remaining <> " iterations remaining")
          pure (init msgs <> [set "content" (PStr newContent) l])
        _ -> pure msgs

    sequential calls msgs0 taskId apiCount = go 1 calls msgs0
      where
        total = length calls
        go _ [] msgs = injectBudget msgs apiCount
        go i (tc : rest) msgs = do
          intr <- isInterrupted ag
          if intr
            then do
              let remaining = tc : rest
              say ag (pfx ag <> "⚡ Interrupt: skipping " <> int (length remaining) <> " tool call(s)")
              let skipped = [PDict [("role", PStr "tool"), ("content", PStr ("[Tool execution cancelled — " <> tcName c <> " was skipped due to user interrupt]")), ("tool_call_id", tcId c)] | c <- remaining]
              injectBudget (msgs <> skipped) apiCount
            else do
              let name = tcName tc
                  args = parseArgs (tcArgs tc)
                  env = agEnv ag
              noteUse name
              unlessQuiet ag $ mapM_ (say ag) (callLine i name args)
              progress name args
              t0 <- now ag
              result <-
                if quiet ag && isJust (localTool name args)
                  then do
                    -- no spinner for the loop's own tools: the line after
                    r <- try @SomeException (invokeReported tc name args taskId)
                    t1 <- now ag
                    let (res, forMsg) = case r of
                          Right x -> (x, Just x)
                          Left e -> ("Error executing tool '" <> name <> "': " <> T.pack (displayException e), Nothing)
                    say ag ("  " <> cuteToolMessage (envSkin env) name args (t1 - t0) forMsg)
                    pure res
                else if quiet ag
                  then do
                    fi <- envChoice env (length kawaiiWaiting)
                    let p0 = fromMaybe name (buildToolPreview name args)
                        p = if T.length p0 > 30 then T.take 27 p0 <> "..." else p0
                    sp <- startSpinner (SpinnerOut (sysOut (envSys env)) (envAnimate env)) (envSkin env) ((kawaiiWaiting !! fi) <> " " <> emojiFor name <> " " <> p) "dots"
                    r <- try @SomeException (invokeReported tc name args taskId)
                    t1 <- now ag
                    let (res, forMsg) = case r of
                          Right x -> (x, Just x)
                          Left e -> ("Error executing tool '" <> name <> "': " <> T.pack (displayException e), Nothing)
                    stopSpinner sp (Just (cuteToolMessage (envSkin env) name args (t1 - t0) forMsg))
                    rethrowStop r
                    pure res
                  else do
                    r <- try @SomeException (invokeReported tc name args taskId)
                    rethrowStop r
                    pure $ case r of
                      Right x -> x
                      Left e -> "Error executing tool '" <> name <> "': " <> T.pack (displayException e)
              t1 <- now ag
              let msgs1 = msgs <> [PDict [("role", PStr "tool"), ("content", PStr (truncateResult result)), ("tool_call_id", tcId tc)]]
              unlessQuiet ag $ mapM_ (say ag) (doneLines i (t1 - t0) result)
              intr2 <- isInterrupted ag
              if intr2 && i < total
                then do
                  say ag (pfx ag <> "⚡ Interrupt: skipping " <> int (total - i) <> " remaining tool call(s)")
                  let skipped = [PDict [("role", PStr "tool"), ("content", PStr ("[Tool execution skipped — " <> tcName c <> " was not started. User sent a new message]")), ("tool_call_id", tcId c)] | c <- rest]
                  injectBudget (msgs1 <> skipped) apiCount
                else do
                  when (cfgToolDelay (agCfg ag) > 0 && i < total) $ rtSleep (envRuntime (agEnv ag)) (cfgToolDelay (agCfg ag))
                  go (i + 1) rest msgs1

    concurrent calls msgs taskId apiCount = do
      let env = agEnv ag
          total = length calls
      intr <- isInterrupted ag
      if intr
        then do
          say ag (pfx ag <> "⚡ Interrupt: skipping " <> int total <> " tool call(s)")
          pure (msgs <> [PDict [("role", PStr "tool"), ("content", PStr ("[Tool execution cancelled — " <> tcName c <> " was skipped due to user interrupt]")), ("tool_call_id", tcId c)] | c <- calls])
        else do
          let parsed = [(tc, tcName tc, parseArgs (tcArgs tc)) | tc <- calls]
          forM_ parsed $ \(_, nm, _) -> noteUse nm
          unlessQuiet ag $ do
            say ag ("  ⚡ Concurrent: " <> int total <> " tool calls — " <> T.intercalate ", " [nm | (_, nm, _) <- parsed])
            forM_ (zip [1 :: Int ..] parsed) $ \(i, (_, nm, args)) -> mapM_ (say ag) (callLine i nm args)
          forM_ parsed $ \(_, nm, args) -> progress nm args
          sp <-
            if quiet ag
              then do
                fi <- envChoice env (length kawaiiWaiting)
                Just <$> startSpinner (SpinnerOut (sysOut (envSys env)) (envAnimate env)) (envSkin env) ((kawaiiWaiting !! fi) <> " ⚡ running " <> int total <> " tools concurrently") "dots"
              else pure Nothing
          sem <- newQSem (min total 8)
          results0 <- mapConcurrently
            (\(tc, nm, args) -> bracket_ (waitQSem sem) (signalQSem sem) $ do
                t0 <- now ag
                r <- try @SomeException (invokeReported tc nm args taskId)
                t1 <- now ag
                let res = either (\e -> "Error executing tool '" <> nm <> "': " <> T.pack (displayException e)) id r
                pure ((res, t1 - t0), r))
            parsed
          forM_ sp $ \s' ->
            stopSpinner s' (Just ("⚡ " <> int (length results0) <> "/" <> int total <> " tools completed in " <> fmtF 1 (sum (map (snd . fst) results0)) <> "s total"))
          mapM_ (rethrowStop . snd) results0
          let results = map fst results0
          out <- forM (zip3 [1 :: Int ..] parsed results) $ \(i, (tc, nm, args), (res, dur)) -> do
            if quiet ag
              then say ag ("  " <> cuteToolMessage (envSkin env) nm args dur (Just res))
              else mapM_ (say ag) (doneLines i dur res)
            pure (PDict [("role", PStr "tool"), ("content", PStr (truncateResult res)), ("tool_call_id", tcId tc)])
          injectBudget (msgs <> out) apiCount

    -- | After the loop (§4.11).
    finish :: Loop -> Text -> Int -> IO AgentResult
    finish lp taskId historyLen = do
      let cfg = agCfg ag
          maxIt = cfgMaxIterations cfg
      (used, maxB) <- readIORef (agBudget ag)
      clmRun <- (clmOf <$> readIORef (agContext ag))
      capped <- case clmRun of
        Nothing -> pure (lpCalls lp >= maxIt)
        Just run -> do
          st <- readIORef (crState run)
          pure (stIters st >= maxIt || lpCalls lp >= 2 * maxIt)
      let summarize = isNothing (lpFinal lp) && (capped || maxB - used <= 0)
      when summarize $ forM_ clmRun $ \run -> do
        pinned <- readIORef (crPinned run)
        let (pre, ed) = splitAt pinned (lpMessages lp)
        csObserve (crSetup run) (EvCall 0 (if null pre then PNull else last pre) (ed <> [PDict [("role", PStr "user"), ("content", PStr summaryRequestText)]]))
      (final, msgs) <-
        if summarize
          then do
            when (maxB - used <= 0 && not (quiet ag)) $
              say ag ("\n⚠️  Session iteration budget exhausted (" <> int used <> "/" <> int maxB <> " used, including subagents)")
            (f, ms) <- maxIterations (lpMessages lp) (lpCalls lp)
            pure (Just f, ms)
          else pure (lpFinal lp, lpMessages lp)
      let completed = case clmRun of
            Nothing -> isJust final && lpCalls lp < maxIt
            Just _ -> isJust final && not summarize
      forM_ clmRun $ \run -> do
        pinned <- readIORef (crPinned run)
        _ <- syncRecordBegins ag (riSystemMessage ri) run (drop pinned msgs)
        saveClm run
      cleanup taskId
      msgs' <- persistSession ag msgs historyLen
      let lastReasoning = case [r | m <- reverse msgs', role m == "assistant", Just r <- [get "reasoning" m], truthy r] of
            (r : _) -> r
            [] -> PNull
      previewed <- readIORef (agPreviewed ag)
      writeIORef (agPreviewed ag) False
      (_, imsg) <- readIORef (agInterrupt ag)
      clearInterrupt ag
      pure . RunDone . PDict $
        [ ("final_response", maybe PNull PStr final), ("last_reasoning", lastReasoning), ("messages", PList msgs')
        , ("api_calls", PInt (fromIntegral (lpCalls lp))), ("completed", PBool completed), ("partial", PBool False)
        , ("interrupted", PBool (lpInterrupted lp)), ("response_previewed", PBool previewed) ]
          <> [("interrupt_message", PStr m) | lpInterrupted lp, Just m <- [imsg]]

    -- | @_handle_max_iterations@
    maxIterations :: [PyVal] -> Int -> IO (Text, [PyVal])
    maxIterations msgs _ = do
      let cfg = agCfg ag
          maxIt = cfgMaxIterations cfg
          env = agEnv ag
      say ag ("⚠️  Reached maximum iterations (" <> int maxIt <> "). Requesting summary...")
      let summaryReq = PDict [("role", PStr "user"), ("content", PStr summaryRequestText)]
          msgs1 = msgs <> [summaryReq]
      s <- readIORef (agSettings ag)
      prompt <- readIORef (agCachedPrompt ag)
      let copied = [delete "finish_reason" (delete "reasoning" m) | m <- msgs1]
          effective = case cfgEphemeral cfg of
            Just e | not (T.null e) -> T.strip (fromMaybe "" prompt <> "\n\n" <> e)
            _ -> fromMaybe "" prompt
          withSys = (if T.null effective then [] else [PDict [("role", PStr "system"), ("content", PStr effective)]]) <> copied
          apiMessages =
            let off = if T.null effective then 0 else 1
                (a, b) = splitAt off withSys
             in a <> cfgPrefill cfg <> b
          s' = s {sTools = Nothing}
          attempt = case sMode s of
            ChatCompletions -> do
              let low = T.toLower (sBaseUrl s)
                  r = sRouting s
                  prefs =
                    [("only", PList (map PStr (rOnly r))) | not (null (rOnly r))]
                      <> [("ignore", PList (map PStr (rIgnore r))) | not (null (rIgnore r))]
                      <> [("order", PList (map PStr (rOrder r))) | not (null (rOrder r))]
                      <> [("sort", PStr x) | Just x <- [rSort r], not (T.null x)]
                  extra =
                    [("reasoning", fromMaybe (PDict [("enabled", PBool True), ("effort", PStr "medium")]) (sReasoning s)) | supportsReasoningExtraBody (sBaseUrl s) (sModel s)]
                      <> [("tags", PList [PStr "product=gauss-agent"]) | "nousresearch" `T.isInfixOf` low]
                      <> [("provider", PDict prefs) | not (null prefs)]
                  maxTok = case sMaxTokens s of
                    Just k
                      | "api.openai.com" `T.isInfixOf` low && not ("openrouter" `T.isInfixOf` low) -> [("max_completion_tokens", PInt k)]
                      | otherwise -> [("max_tokens", PInt k)]
                    Nothing -> []
                  body = PDict ([("messages", PList apiMessages), ("model", PStr (sModel s))] <> maxTok <> extra)
                  base = T.dropWhileEnd (== '/') (sBaseUrl s)
              r' <- sdkSend (envRuntime env) (envTransport env) (HttpReq "POST" (base <> "/chat/completions") (chatHeaders (sBaseUrl s) (sApiKey s)) (Just body) False) chatTimeouts
              pure $ case r' of
                Left e -> Left e
                Right resp -> case loads (rBody resp) of
                  Left e -> Left (ApiError (T.pack e))
                  Right v -> Right (case responseContent v of PStr c -> c; _ -> "")
            _ -> do
              strip <- readIORef (agStripHeaders ag)
              let tr = withoutNullTools (if strip then stripDefaultHeaders (envTransport env) else envTransport env)
              r' <- callModel (envRuntime env) tr (envLlm env) s' apiMessages
              pure $ case r' of
                Right (CallOk o _) -> Right (T.strip (contentT (oMessage o)))
                Right (CallInvalid _) -> Right ""
                Left e -> Left e
          clean t = if "<think>" `T.isInfixOf` t then T.strip (removeThink True t) else t
      r1 <- attempt
      case r1 of
        Left e -> do
          envWarn env ("Failed to get summary response: " <> errorText e)
          pure ("I reached the maximum iterations (" <> int maxIt <> ") but couldn't summarize. Error: " <> errorText e, msgs1)
        Right t
          | not (T.null t), let c = clean t, not (T.null c) ->
              pure (c, msgs1 <> [PDict [("role", PStr "assistant"), ("content", PStr c)]])
          | not (T.null t) -> pure ("I reached the iteration limit and couldn't generate a summary.", msgs1)
          | otherwise -> do
              r2 <- attempt
              case r2 of
                Left e -> do
                  envWarn env ("Failed to get summary response: " <> errorText e)
                  pure ("I reached the maximum iterations (" <> int maxIt <> ") but couldn't summarize. Error: " <> errorText e, msgs1)
                Right t2
                  | not (T.null t2), let c = clean t2, not (T.null c) -> pure (c, msgs1 <> [PDict [("role", PStr "assistant"), ("content", PStr c)]])
                  | otherwise -> pure ("I reached the iteration limit and couldn't generate a summary.", msgs1)

    invalidDetails mode v = case mode of
      CodexResponses -> case get "output" v of
        Just (PList []) -> ["response.output is empty"]
        Just (PList _) -> []
        _ -> ["response.output is not a list"]
      AnthropicMessages -> case get "content" v of
        Just (PList []) -> ["response.content is empty"]
        Just (PList _) -> []
        _ -> ["response.content is not a list"]
      ChatCompletions -> case get "choices" v of
        Nothing -> ["response has no 'choices' attribute"]
        Just PNull -> ["response.choices is None"]
        Just (PList []) -> ["response.choices is empty"]
        _ -> []

    providerOfInvalid v =
      -- the SDK keeps an unknown "error" field as a plain dict, which has no
      -- .metadata: the provider comes from the model field
      let m0 = case (get "error" v, get "message" v) of
            (Just e, _) | truthy e -> pyStrOf e
            (_, Just m) | truthy m -> pyStrOf m
            _ -> "Unknown"
          p = case get "model" v of
            Just m | truthy m -> "model=" <> pyStrOf m
            _ -> "Unknown"
       in (m0, p)

-- | The toolset requirements in name order, as the agent prints them.
withUnportedToolsets :: [(Text, Bool)] -> [(Text, Bool)]
withUnportedToolsets = sortOn fst

-- | The max-iterations summary request is built by hand upstream, with
-- no @tools@ key at all.
withoutNullTools :: Transport -> Transport
withoutNullTools tr =
  Transport
    { tRequest = \hr to -> tRequest tr (fix hr) to
    , tStream = \hr to k -> tStream tr (fix hr) to k }
  where
    fix hr = hr {hrBody = fmap (\b -> if get "tools" b == Just PNull then delete "tools" b else b) (hrBody hr)}

-- | The headers upstream's per-request clients lose after a fallback (its
-- client kwargs keep only the key and the URL).
stripDefaultHeaders :: Transport -> Transport
stripDefaultHeaders tr =
  Transport
    { tRequest = \hr to -> tRequest tr (strip hr) to
    , tStream = \hr to k -> tStream tr (strip hr) to k }
  where
    strip hr = hr {hrHeaders = [(k, v) | (k, v) <- hrHeaders hr, k `notElem` ["http-referer", "x-openrouter-title", "x-openrouter-categories"]]}
