-- | Fisher's native agent made outside the chat shell (upstream's
-- @AIAgent(...)@ as acp_adapter and cron/scheduler.py build it): the
-- process-wide tools, and one agent from a model, a resolved provider and
-- the platform's settings, writing a replay log if asked.
module Fisher.Cli.Native
  ( NativeShared (..)
  , newNativeShared
  , NativeSpec (..)
  , nativeSpec
  , buildNative
  , NativeFailure (..)
  , missingCredentials
  ) where

import Control.Exception (Exception (..), throwIO)
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Agent.Aux (AuxEnv (..), callLlm, newAuxState)
import qualified Fisher.Agent.Display as D
import Fisher.Agent.Loop
import Fisher.Agent.Metadata (MetaEnv (MetaEnv), ModelCache, newModelCache)
import Fisher.Agent.Replay
import Fisher.Agent.Tools.Browser (cleanupBrowser)
import Fisher.Agent.Tools.Memory (memoryToolDef)
import Fisher.Agent.Tools.Terminal (sudoPasswordFor)
import Fisher.Agent.Tools.Todo (todoToolDef)
import Fisher.Cli.AgentChat (ChatDeps (..))
import Fisher.Agent.Session (SessionDB)
import Fisher.Cli.NetTools
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered)
import Fisher.LLM.Http (Transport)
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Tools.Agent (AgentTools (..), newAgentToolsWith)
import Fisher.Tools.File (readFilesSummary)
import Fisher.Tools.Registry
import qualified Fisher.Tools.Sandbox as Sb
import Fisher.Tools.Sandbox.Config (cleanupTask)
import Fisher.Tools.Sandbox.Local (SandboxEnv (..))

-- | An error whose text is what upstream's exception says (and its class).
data NativeFailure = NativeFailure Text Text
  deriving (Show)

instance Exception NativeFailure where
  displayException (NativeFailure _ t) = T.unpack t

-- | What the OpenAI SDK says when an agent is made without a key.
missingCredentials :: Text
missingCredentials =
  "Failed to initialize OpenAI client: Missing credentials. Please pass an `api_key`, `workload_identity`, `admin_api_key`, or set the `OPENAI_API_KEY` or `OPENAI_ADMIN_KEY` environment variable."

-- | The tools are the process's, as upstream's module-level registry is;
-- so is the model metadata cache.
data NativeShared = NativeShared
  { nsTools :: AgentTools
  , nsNet :: NetTools
  , nsTransport :: Transport
  , nsModels :: ModelCache
  }

newNativeShared :: Sys -> ChatDeps -> (Text -> IO ()) -> IO NativeShared
newNativeShared sys deps logLine = do
  let ui = NetUi logLine logLine (\_ -> pure (Left False)) (pure Nothing)
  sudo <- sudoPromptFor sys ui
  at <- newAgentToolsWith (\se -> se {seSudoPassword = sudoPasswordFor sudo}) (sysFisherHome sys)
  tr <- cdTransport deps
  net <- newNetTools sys (cdRuntime deps) tr (cdWhich deps) (pure False) ui at
  registerNetTools (atRegistry at) net
  NativeShared at net tr <$> newModelCache

data NativeSpec = NativeSpec
  { nsModel :: Text
  , nsProvider :: Text
  , nsApiMode :: ApiMode
  , nsBaseUrl :: Text
  , nsApiKey :: Text
  , nsBaseUrlGiven :: Bool
  , nsEnabled :: Maybe [Text]
  , nsDisabled :: Maybe [Text]
  , nsPlatform :: Text
  , nsSessionId :: Text
  , nsMaxIterations :: Int
  , nsReasoning :: Maybe PyVal
  , nsPrefill :: [PyVal]
  , nsRouting :: Routing
  , nsDb :: Maybe SessionDB
  , nsCallbacks :: Callbacks
  , nsReplay :: Bool
  , nsOut :: Text -> IO ()
  -- ^ where the agent's own printing goes
  }

nativeSpec :: Text -> Text -> NativeSpec
nativeSpec platform sid =
  NativeSpec
    { nsModel = "anthropic/claude-opus-4.6", nsProvider = "openrouter", nsApiMode = ChatCompletions
    , nsBaseUrl = "https://openrouter.ai/api/v1", nsApiKey = "", nsBaseUrlGiven = False
    , nsEnabled = Nothing, nsDisabled = Nothing, nsPlatform = platform, nsSessionId = sid, nsMaxIterations = 90
    , nsReasoning = Nothing, nsPrefill = [], nsRouting = noRouting, nsDb = Nothing, nsCallbacks = noCallbacks
    , nsReplay = True, nsOut = \_ -> pure () }

-- | @AIAgent(...)@: the agent, and its replay log when it writes one.
-- Raises 'NativeFailure' where the SDK would refuse to make a client.
buildNative :: Sys -> ChatDeps -> NativeShared -> NativeSpec -> IO (Agent, Maybe ReplayLog)
buildNative sys0 deps sh spec = do
  openaiKey <- fromMaybe "" <$> sysGetEnv sys0 "OPENAI_API_KEY"
  when (T.null (nsApiKey spec) && nsApiMode spec /= AnthropicMessages && T.null openaiKey) $
    throwIO (NativeFailure "RuntimeError" missingCredentials)
  cfg0 <- loadConfigOrdered sys0
  -- Fisher only: CLM and RLM are the chat's for now; here the session
  -- keeps the compressor, and says so once
  case get "fisher" cfg0 >>= get "context" of
    Just (PStr c) | c `elem` ["clm", "rlm"] ->
      nsOut spec ("fisher.context: " <> c <> " is not supported here yet; this session uses the compressor.\n")
    _ -> pure ()
  let at = nsTools sh
      net = nsNet sh
      reg = atRegistry at
      sid = nsSessionId spec
  lg <- if nsReplay spec then Just <$> openReplayLog (sysFisherHome sys0) sid else pure Nothing
  let sys = maybe sys0 (`recordUuids` sys0) lg
      cfg =
        defaultAgentConfig
          { cfgModel = nsModel spec, cfgApiKey = nsApiKey spec
          , cfgBaseUrl = if T.null (nsBaseUrl spec) then "https://openrouter.ai/api/v1" else nsBaseUrl spec
          , cfgBaseUrlGiven = nsBaseUrlGiven spec, cfgProvider = nsProvider spec, cfgApiMode = nsApiMode spec
          , cfgMaxIterations = nsMaxIterations spec
          , cfgEnabledToolsets = nsEnabled spec, cfgDisabledToolsets = nsDisabled spec, cfgQuiet = True
          , cfgReasoning = nsReasoning spec, cfgPrefill = nsPrefill spec, cfgRouting = nsRouting spec
          , cfgSessionId = Just sid, cfgPlatform = Just (nsPlatform spec) }
  (defs0, printed) <- getToolDefinitions reg (nsEnabled spec) (nsDisabled spec)
  let defs = maybe defs0 fst (cdTools deps)
  emojis <- mapM (\n -> (,) n <$> emojiFor reg n) [maybe "" pyStrOf (get "function" d >>= get "name") | d <- defs]
  let tr0 = nsTransport sh
      tr = maybe tr0 (`recordTransport` tr0) lg
      runner0 =
        ToolRunner
          { trDefinitions = defs
          , trPrinted = printed
          , trCall = \name args task enabled -> do
              when (name `elem` ["terminal", "read_file", "write_file", "patch", "search_files"]) (ntFirstUse net)
              handleFunctionCall reg defaultCtx {tcTaskId = task, tcEnabledTools = enabled} name args
          , trEmoji = \n -> lookup n emojis
          , trReadFilesSummary = \task -> do
              v <- readFilesSummary (atFileTools at) task
              pure [(p, [x | PStr x <- rs]) | PList fs <- [v], f <- fs, Just (PStr p) <- [get "path" f], Just (PList rs) <- [get "regions" f]]
          , trCleanup = \task -> cleanupTask (atSandboxes at) task >> cleanupBrowser (ntBrowser net) task
          , trSetInterrupt = \on -> (if on then Sb.raiseInterrupt else Sb.clearInterrupt) (seInterrupt (atSandboxEnv at))
          , trRequirements = withUnportedToolsets <$> checkToolsetRequirements reg }
      runner = maybe id recordToolRunner lg (maybe id snd (cdTools deps) runner0)
  st <- newAuxState
  let rtime = cdRuntime deps
      auxEnv = AuxEnv sys rtime tr (loadConfigOrdered sys) (\_ -> pure ()) st
      env =
        AgentEnv
          { envSys = sys {sysOut = nsOut spec}, envRuntime = rtime, envTransport = tr
          , envLlm = Env (sysUuidHex sys) (\_ r -> pure ("[The " <> r <> " attached an image, which Fisher cannot describe yet.]"))
          , envNowLocal = cdNowLocal deps, envPromptNow = cdNowLocal deps
          , envCwd = cdCwd deps, envChoice = cdChoice deps
          , envAnimate = False, envDb = nsDb spec, envTools = runner
          , envAux = callLlm auxEnv, envAuxEnv = Just auxEnv
          , envMeta = MetaEnv tr (sysNow sys) (sysFisherHome sys) (nsModels sh) (\_ -> pure ())
          , envSkin = D.Skin "┊" [] [], envConfig = cfg0
          , envCallbacks = nsCallbacks spec
          , envWarn = \_ -> pure ()
          , envPromptHook = maybe pure recordPromptHook lg
          , envWriteFiles = True, envLocalTool = maybe (\_ _ act -> act) recordLocalTool lg, envInbox = pure [] }
  ag <- newAgent cfg env
  register reg todoToolDef
  when (isJust (agentMemoryStore ag)) (register reg memoryToolDef)
  forM_ lg $ \l -> snapshotEnv sys >>= logAgent l cfg runner
  pure (ag, lg)
