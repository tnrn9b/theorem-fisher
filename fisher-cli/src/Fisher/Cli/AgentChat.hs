-- | Fisher's native agent in the CLI (upstream @GaussCLI.chat@ and
-- @gauss chat -q@): the agent built from the configuration and the
-- resolved provider, one turn run with the conversation so far, the
-- response box, the exit summary, and the replay log of every session.
module Fisher.Cli.AgentChat
  ( ChatUi (..)
  , ChatDeps (..)
  , defaultChatDeps
  , ChatOptions (..)
  , defaultChatOptions
  , ChatSession
  , newChatSession
  , chatTurn
  , chatHistory
  , chatSessionId
  , printExitSummary
  , closeChatSession
  , chatMain
  , ShellChat (..)
  , shellChat
  , replayMain
  , responseBox
  , exitSummaryLines
  ) where

import Control.Exception (SomeException, bracket, displayException, fromException, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time (LocalTime, diffLocalTime, defaultTimeLocale, formatTime, getZonedTime, zonedTimeToLocalTime)
import System.Directory (doesFileExist, findExecutable, getCurrentDirectory)
import System.FilePath ((</>))
import System.Environment (unsetEnv)
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdout)
import qualified System.Posix.Signals as Sig

import Fisher.Agent.Aux (AuxEnv (..), callLlm, newAuxState)
import qualified Fisher.Agent.Display as D
import Fisher.Agent.Loop
import Fisher.Agent.Metadata (MetaEnv (MetaEnv), newModelCache)
import Fisher.Agent.Inbox (inboxPath, takeInbox)
import Fisher.Agent.Ledger (openLedger, recordRun)
import Fisher.Agent.Replay
import Fisher.Agent.RunLock (holdRunLock, runPidPath)
import Fisher.Agent.Steps (stepsPath)
import Fisher.Agent.UiHooks
import Fisher.Agent.StoreDB (restoreConversation, storeSessionDB)
import Fisher.Agent.Tools.Browser (cleanupAllBrowsers, cleanupBrowser)
import Fisher.Cli.NetTools
import Fisher.Cli.ProverChat (chatEnvNames, chatMcpServers, chatProject, defaultToolsets, registerChatToolsets, wantsProject)
import Fisher.Prover (ProverKind)
import Fisher.Cli.ToolsCheck (mainProviderSampling)
import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.Error (formatRuntimeProviderError)
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered)
import Fisher.LLM.Auth.Registry (AuthType (..), ProviderConfig (..), providerRegistry)
import Fisher.LLM.Auth.Resolve (RuntimeRequest (..), getAuthStatus, resolveRuntimeProvider)
import Fisher.LLM.Http (Runtime (..), Transport, systemRuntime, tlsTransport)
import Fisher.LLM.SysIO (fisherHints)
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Mcp.Client
import Fisher.Mcp.HttpClient (tlsHttpClient)
import Fisher.Py.Value
import Fisher.Rich.Render
import Fisher.Rich.Style (parseStyle)
import Fisher.Rich.Text (Justify (..), plainText)
import Fisher.Cli.Session (activeCwd, cliSession)
import Fisher.ProjectCommand (activeProjectSummary)
import Fisher.Shell.Banner (BannerInput (..), chatLines, compactBanner, welcomeBanner)
import Fisher.Term (Style (..), detectStyle)
import Fisher.Skin (Skin, loadSkin, skinBranding, skinColor)
import Fisher.Store (Store, getSession, openStore, reopenSession, resolveSessionId, rootSessionId)
import Fisher.Agent.Tools.Memory (memoryToolDef)
import Fisher.Agent.Tools.Terminal (sudoPasswordFor)
import Fisher.Agent.Tools.Todo (todoToolDef)
import Fisher.Tools.Agent (AgentTools (..), newAgentToolsWith)
import Fisher.Tools.File (FileTools (..), readFilesSummary, setContextPaths)
import Fisher.Agent.Context.Budget (pathAliases, prevPath)
import qualified Fisher.Agent.Context.Clm as Clm
import qualified Fisher.Agent.Context.Rlm as Rlm
import Fisher.Cli.Clm (ClmRefused (..), ContextChoice (..), RlmRefused (..), clmSetupFor, contextChoice, liveHistory, rlmSetupFor, validContext)
import Fisher.Tools.Registry
import qualified Fisher.Tools.Sandbox as Sb
import Fisher.Tools.Sandbox.Config (cleanupAll, cleanupTask)
import Fisher.Tools.Sandbox.Local (SandboxEnv (..))

-- | How the CLI prints.
data ChatUi = ChatUi
  { uiPrint :: Text -> IO ()
  -- ^ @print@: a line
  , uiRaw :: Text -> IO ()
  -- ^ raw writes (the agent's own output, spinners)
  , uiConsole :: Text -> IO ()
  -- ^ @ChatConsole().print(markup)@
  , uiWidth :: IO Int
  , uiAnsi :: Bool
  }

-- | What a session takes from outside, so the specs can script it.
data ChatDeps = ChatDeps
  { cdTransport :: IO Transport
  , cdTools :: Maybe ([PyVal], ToolRunner -> ToolRunner)
  -- ^ replace the real tools: their definitions, and a wrapper over the runner
  , cdNowLocal :: IO LocalTime
  , cdRuntime :: Runtime
  , cdChoice :: Int -> IO Int
  , cdAnimate :: IO Bool
  , cdCwd :: IO FilePath
  , cdWhich :: Text -> IO (Maybe Text)
  -- ^ @shutil.which@, for the browser tools' agent-browser
  , cdBannerInfo :: IO (Text, Maybe Text, Bool)
  -- ^ the banner's working directory, project label and unicode flag
  }

defaultChatDeps :: ChatDeps
defaultChatDeps =
  ChatDeps tlsTransport Nothing localNow systemRuntime (\n -> if n <= 0 then pure 0 else (`mod` n) . floor . (* 1e6) <$> rtRandom systemRuntime) (hIsTerminalDevice stdout) getCurrentDirectory (fmap (fmap T.pack) . findExecutable . T.unpack) bannerInfo

-- | What @show_banner@ reads: @TERMINAL_CWD@ (else the cwd), the active
-- project summary, and whether the terminal takes unicode.
bannerInfo :: IO (Text, Maybe Text, Bool)
bannerInfo = do
  cwd <- activeCwd
  (_, session) <- cliSession
  proj <- either (const Nothing) id <$> try @SomeException (activeProjectSummary session)
  st <- detectStyle
  pure (T.pack cwd, proj, styleUnicode st)

data ChatOptions = ChatOptions
  { coModel :: Maybe Text
  , coProvider :: Maybe Text
  , coToolsets :: Maybe [Text]
  , coVerbose :: Bool
  , coQuiet :: Bool
  , coResume :: Maybe Text
  , coPassSessionId :: Bool
  , coSystemPromptFile :: Maybe FilePath
  -- ^ Fisher only: @--system-prompt-file@, in place of the ephemeral prompt
  , coMcpConfig :: Maybe FilePath
  -- ^ Fisher only: @--mcp-config@, servers added to @mcp_servers@
  , coMaxTurns :: Maybe Int
  -- ^ Fisher only: @--max-turns@, in place of @agent.max_turns@
  , coStream :: Bool
  -- ^ Fisher only: @--output-format stream-json@
  , coContext :: Maybe Text
  -- ^ Fisher only: @--context compress|clm|rlm@, over @fisher.context@
  , coProject :: Maybe (ProverKind, FilePath)
  -- ^ Fisher only: the Rocq or Isabelle project @fisher chat@ runs in
  -- (its prover and prover root), whose server the chat adds
  }

defaultChatOptions :: ChatOptions
defaultChatOptions = ChatOptions Nothing Nothing Nothing False False Nothing False Nothing Nothing Nothing False Nothing Nothing

-- | The servers of a @--mcp-config@ file: Claude Code's shape
-- (@{"mcpServers": {...}}@) or a bare mapping of servers.
mcpConfigServers :: ChatOptions -> IO [(Text, PyVal)]
mcpConfigServers opts = case coMcpConfig opts of
  Nothing -> pure []
  Just p -> do
    b <- BS.readFile p
    pure $ case loads b of
      Right d | Just (PDict s) <- get "mcpServers" d -> s
      Right (PDict s) -> s
      _ -> []

withMcpServers :: [(Text, PyVal)] -> PyVal -> PyVal
withMcpServers [] c = c
withMcpServers extra (PDict kvs) =
  let old = case lookup "mcp_servers" kvs of
        Just (PDict o) -> o
        _ -> []
      merged = [kv | kv@(k, _) <- old, k `notElem` map fst extra] <> extra
   in PDict ([kv | kv@(k, _) <- kvs, k /= "mcp_servers"] <> [("mcp_servers", PDict merged)])
withMcpServers _ c = c

data ChatSession = ChatSession
  { csDeps :: ChatDeps
  , csSys :: Sys
  , csUi :: ChatUi
  , csOpts :: ChatOptions
  , csConfig :: PyVal
  -- ^ @load_config()@ (the defaults under config.yaml)
  , csCli :: PyVal
  -- ^ @load_cli_config()@'s view: what GaussCLI reads
  , csSkin :: Skin
  , csSessionId :: Text
  , csStart :: LocalTime
  , csResumed :: Bool
  , csHistory :: IORef [PyVal]
  , csAgent :: IORef (Maybe Agent)
  , csModel :: IORef Text
  , csRuntime :: IORef (Maybe PyVal)
  , csStore :: IORef (Maybe Store)
  , csTools :: IORef (Maybe (AgentTools, McpClient))
  , csReplay :: IORef (Maybe ReplayLog)
  , csNet :: IORef (Maybe NetTools)
  , csRefused :: IORef Bool
  -- ^ Fisher only: @--context rlm@ was refused (no usable @python3@)
  , csEvents :: Text -> IO ()
  -- ^ Fisher only: where the stream-json events go (stdout, while the
  -- session's own text goes to stderr in that mode)
  , csToolCalls :: IORef Int
  -- ^ Fisher only: the tool calls the agent has run, each once
  , csUiHooks :: IORef (Maybe UiHooks)
  -- ^ Fisher only: the UI's step log and ledger (made with the first agent)
  }

chatHistory :: ChatSession -> IO [PyVal]
chatHistory = readIORef . csHistory

chatSessionId :: ChatSession -> Text
chatSessionId = csSessionId

-- ------------------------------------------------------------------ config

atP :: [Text] -> PyVal -> Maybe PyVal
atP = getPath

textAt :: [Text] -> PyVal -> Maybe Text
textAt p v = case atP p v of
  Just (PStr s) | not (T.null (T.strip s)) -> Just (T.strip s)
  _ -> Nothing

-- | The CLI's own defaults where they differ from @load_config@'s
-- (@compression.summary_provider@ is not among them).
cliView :: PyVal -> PyVal
cliView cfg =
  let comp = case get "compression" cfg of
        Just (PDict kvs) -> PDict [(k, v) | (k, v) <- kvs, k /= "summary_provider" || True]
        _ -> PDict [("enabled", PBool True), ("threshold", PFloat 0.5), ("summary_model", PStr "google/gemini-3-flash-preview")]
   in set "compression" comp cfg

-- | @_parse_reasoning_config(effort)@
parseReasoning :: Text -> Maybe PyVal
parseReasoning e0
  | T.null e = Nothing
  | e == "none" = Just (PDict [("enabled", PBool False)])
  | e `elem` ["xhigh", "high", "medium", "low", "minimal"] = Just (PDict [("enabled", PBool True), ("effort", PStr e)])
  | otherwise = Nothing
  where
    e = T.toLower (T.strip e0)

-- ------------------------------------------------------------------ session

newChatSession :: ChatDeps -> Sys -> ChatUi -> ChatOptions -> Text -> Bool -> IO ChatSession
newChatSession deps sys = newChatSessionWith (sysOut sys) deps sys

-- | 'newChatSession' with the stream-json events written by the first
-- argument.
newChatSessionWith :: (Text -> IO ()) -> ChatDeps -> Sys -> ChatUi -> ChatOptions -> Text -> Bool -> IO ChatSession
newChatSessionWith events deps sys ui opts sid resumed = do
  cfg <- loadConfigOrdered sys
  let cli = cliView cfg
      skinName = fromMaybe "default" (textAt ["display", "skin"] cli)
      model = case coModel opts of
        Just m | not (T.null m) -> m
        _ -> case get "model" cli of
          Just (PStr m) | not (T.null m) -> m
          Just d@(PDict _) -> fromMaybe "anthropic/claude-opus-4.6" (textAt ["default"] d)
          _ -> "anthropic/claude-opus-4.6"
  skin <- loadSkin (sysGaussHome sys) skinName
  -- load_cli_config copies the compression settings into the environment
  forM_ [("enabled", "CONTEXT_COMPRESSION_ENABLED"), ("threshold", "CONTEXT_COMPRESSION_THRESHOLD"), ("summary_model", "CONTEXT_COMPRESSION_MODEL"), ("summary_provider", "CONTEXT_COMPRESSION_PROVIDER")] $ \(k, env) ->
    forM_ (atP ["compression", k] cli) $ \v -> sysSetEnv sys env (pyStrOf v)
  start <- cdNowLocal deps
  ChatSession deps sys ui opts cfg cli skin sid start resumed
    <$> newIORef [] <*> newIORef Nothing <*> newIORef model <*> newIORef Nothing
    <*> newIORef Nothing <*> newIORef Nothing <*> newIORef Nothing <*> newIORef Nothing
    <*> newIORef False <*> pure events <*> newIORef 0 <*> newIORef Nothing

-- | @agent.flush_memories(conversation_history)@ when the session has an
-- agent and a history (before a new session, and at exit).
flushChatMemories :: ChatSession -> IO ()
flushChatMemories cs = do
  hist <- readIORef (csHistory cs)
  readIORef (csAgent cs) >>= mapM_ (\ag -> unless (null hist) (() <$ try @SomeException (flushMemories ag (Just hist) Nothing)))

closeChatSession :: ChatSession -> IO ()
closeChatSession cs = do
  readIORef (csNet cs) >>= mapM_ (cleanupAllBrowsers . ntBrowser)
  readIORef (csTools cs) >>= mapM_ (\(at, mc) -> shutdownMcp mc >> cleanupAll (atSandboxes at))
  readIORef (csReplay cs) >>= mapM_ closeReplayLog
  readIORef (csUiHooks cs) >>= mapM_ uiClose

red :: ChatSession -> Text -> IO ()
red cs t = uiConsole (csUi cs) ("[bold red]" <> t <> "[/]")

-- | @_ensure_runtime_credentials()@
ensureCredentials :: ChatSession -> IO Bool
ensureCredentials cs = do
  let sys = csSys cs
      requested = case coProvider (csOpts cs) of
        Just p | not (T.null p) -> p
        _ -> case textAt ["model", "provider"] (csCli cs) of
          Just p -> p
          Nothing -> "auto"
  envProv <- sysGetEnv sys "GAUSS_INFERENCE_PROVIDER"
  let req = if requested == "auto" then fromMaybe "auto" (envProv >>= \p -> if T.null p then Nothing else Just p) else requested
  r <- try (resolveRuntimeProvider sys (RuntimeRequest (Just req) Nothing Nothing))
  case r of
    Left (e :: SomeException) -> red cs (formatRuntimeProviderError e) >> pure False
    Right rt -> do
      let field' k = maybe "" pyStrOf (get k rt)
          provider = field' "provider"
      if T.null (field' "api_key")
        then do
          red cs $ case provider of
            p | p `elem` ["openrouter", "custom"] -> "No main interactive provider is configured yet. Run `gauss setup`, or save `OPENROUTER_API_KEY`, `OPENAI_API_KEY`, or `ANTHROPIC_API_KEY` in `~/.gauss/.env`."
            "anthropic" -> "No Anthropic credentials are available for chat. Run `gauss setup`, `claude auth login`, or save `ANTHROPIC_API_KEY`."
            "openai-codex" -> "No Codex credentials are available for chat. Run `gauss setup`, `codex login`, or save `OPENAI_API_KEY`."
            _ -> "Provider resolver returned an empty API key."
          pure False
        else
          if T.null (field' "base_url")
            then red cs "Provider resolver returned an empty base URL." >> pure False
            else do
              old <- readIORef (csRuntime cs)
              when (isJust old && old /= Just rt) $ writeIORef (csAgent cs) Nothing
              writeIORef (csRuntime cs) (Just rt)
              when (provider == "openai-codex") $ do
                m <- readIORef (csModel cs)
                when ("/" `T.isInfixOf` m) $ writeIORef (csModel cs) (T.drop 1 (T.dropWhile (/= '/') m))
              pure True

-- | The tool layer: file tools, MCP servers from config.yaml.
toolsFor :: ChatSession -> IO (AgentTools, McpClient)
toolsFor cs =
  readIORef (csTools cs) >>= \case
    Just t -> pure t
    Nothing -> do
      let sys = csSys cs
      sudo <- sudoPromptFor sys (chatNetUi (csUi cs))
      at <- newAgentToolsWith (\se -> se {seSudoPassword = sudoPasswordFor sudo}) (sysGaussHome sys)
      http <- tlsHttpClient
      environ <- fmap concat . forM ["PATH", "HOME", "USER", "LANG", "LC_ALL", "TERM", "SHELL", "TMPDIR", "LOGNAME"] $ \k ->
        maybe [] (\v -> [(k, v)]) <$> sysGetEnv sys k
      mc <- newMcpClient
        McpEnv
          { meRegistry = atRegistry at
          , meLog = \_ _ -> pure ()
          , meSampling = mainProviderSampling sys (csConfig cs)
          , meEnviron = pure environ
          , meHome = T.pack (sysHome sys)
          , meGaussHome = sysGaussHome sys
          , meHttp = Just http }
      given <- mcpConfigServers (csOpts cs)
      chatEnv <- fmap concat . forM chatEnvNames $ \k -> maybe [] (\v -> [(k, T.unpack v)]) <$> sysGetEnv sys (T.pack k)
      -- Fisher only: a prover project's server, and the pet check; the
      -- warnings on stderr, as stdout may be stream-json
      let stateDir = sysGaussHome sys </> "autoformalize" </> "fisher" </> "chat"
      (extra, warnings) <- chatMcpServers chatEnv (csConfig cs) (Just stateDir) (coProject (csOpts cs)) given
      mapM_ (TIO.hPutStrLn stderr) warnings
      _ <- try @SomeException (discoverFromConfig mc (withMcpServers extra (csConfig cs)))
      writeIORef (csTools cs) (Just (at, mc))
      pure (at, mc)

storeFor :: ChatSession -> IO (Maybe Store)
storeFor cs =
  readIORef (csStore cs) >>= \case
    Just s -> pure (Just s)
    Nothing -> do
      r <- try @SomeException (openStore (sysGaussHome (csSys cs) </> "state.db"))
      case r of
        Right s -> writeIORef (csStore cs) (Just s) >> pure (Just s)
        Left _ -> pure Nothing

localNow :: IO LocalTime
localNow = zonedTimeToLocalTime <$> getZonedTime

-- | @_init_agent()@
initAgent :: ChatSession -> IO Bool
initAgent cs =
  readIORef (csAgent cs) >>= \case
    Just _ -> pure True
    Nothing -> do
      ok <- ensureCredentials cs
      if not ok
        then pure False
        else do
          let sid = csSessionId cs
          store <- storeFor cs
          resumedOk <-
            if csResumed cs
              then do
                hist <- readIORef (csHistory cs)
                case store of
                  Just st | null hist -> do
                    row <- getSession st sid
                    case row of
                      Nothing -> do
                        uiConsole (csUi cs) ("[bold red]Session not found: " <> sid <> "[/]")
                        uiConsole (csUi cs) "[dim]Use a session ID from a previous CLI run (gauss sessions list).[/]"
                        pure False
                      Just r -> do
                        restored <- restoreConversation st sid
                        let accent = skinColor (csSkin cs) "ui_accent" "#FFBF00"
                            title = case lookup "title" r of
                              Just (PStr t) -> " \"" <> t <> "\""
                              _ -> ""
                            users = length [() | m <- restored, get "role" m == Just (PStr "user")]
                        if null restored
                          then uiConsole (csUi cs) ("[bold " <> accent <> "]Session " <> sid <> " found but has no messages. Starting fresh.[/]")
                          else do
                            writeIORef (csHistory cs) restored
                            uiConsole (csUi cs) $
                              "[bold " <> accent <> "]↻ Resumed session[/] [bold]" <> sid <> "[/][bold " <> accent <> "]" <> title <> "[/] ("
                                <> T.pack (show users) <> " user message" <> (if users /= 1 then "s" else "") <> ", "
                                <> T.pack (show (length restored)) <> " total messages)"
                        _ <- try @SomeException (reopenSession st sid)
                        pure True
                  _ -> pure True
              else pure True
          if not resumedOk
            then pure False
            else do
              r <- try @SomeException (buildAgent cs store)
              case r of
                Left e -> do
                  forM_ (fromException @RlmRefused e) $ \_ -> writeIORef (csRefused cs) True
                  red cs ("Failed to initialize agent: " <> T.pack (displayException e))
                  pure False
                Right ag -> writeIORef (csAgent cs) (Just ag) >> pure True
  where
    _ = ()

-- | The variables a managed workflow sets for its chat (launch's plans),
-- recorded in the UI's ledger so a resume gets them again. No key or
-- token among them.
resumeVars :: [Text]
resumeVars =
  [ "CLAUDE_PLUGIN_ROOT", "FISHER_LEAN_MCP", "GAUSS_AUTOFORMALIZE", "GAUSS_AUTOFORMALIZE_AUTH_MODE", "GAUSS_AUTOFORMALIZE_BACKEND"
  , "GAUSS_AUTOFORMALIZE_CONTEXT", "GAUSS_AUTOFORMALIZE_INSTRUCTIONS", "GAUSS_AUTOFORMALIZE_LEAN4_SKILLS_REF"
  , "GAUSS_AUTOFORMALIZE_LEAN_LSP_MCP_SPEC", "GAUSS_AUTOFORMALIZE_MANAGED_STATE_DIR", "GAUSS_AUTOFORMALIZE_NATIVE_PROVIDER"
  , "GAUSS_AUTOFORMALIZE_SKILLS_ROOT", "GAUSS_CHAT_CWD", "GAUSS_MANAGED_CHAT", "GAUSS_MANAGED_CHAT_BACKEND", "GAUSS_MANAGED_STATE_DIR"
  , "GAUSS_PROJECT_MANIFEST", "GAUSS_PROJECT_ROOT", "GAUSS_REAL_HOME", "GAUSS_YOLO_MODE", "ISABELLE_PROJECT_PATH", "LEAN4_PLUGIN_ROOT"
  , "LEAN4_REFS", "LEAN4_SCRIPTS", "LEAN_PROJECT_PATH", "ROCQ_COQC_BINARY", "ROCQ_PROJECT_PATH", "ROCQ_WORKSPACE", "TERMINAL_CWD" ]

buildAgent :: ChatSession -> Maybe Store -> IO Agent
buildAgent cs store = do
  let sys0 = csSys cs
      cli = csCli cs
      opts = csOpts cs
      sid = csSessionId cs
  Just rt <- readIORef (csRuntime cs)
  model <- readIORef (csModel cs)
  (at, _) <- toolsFor cs
  lg <- openReplayLog (sysGaussHome sys0) sid
  writeIORef (csReplay cs) (Just lg)
  let field' k = maybe "" pyStrOf (get k rt)
      enabledSets = case coToolsets opts of
        Just ts -> ts
        Nothing -> case atP ["platform_toolsets", "cli"] cli of
          Just (PList ts) -> [t | PStr t <- ts]
          _ -> ["gauss-cli"]
      maxTurns = case (coMaxTurns opts, atP ["agent", "max_turns"] cli) of
        (Just n, _) | n > 0 -> n
        (_, Just (PInt n)) | n > 0 -> fromIntegral n
        _ -> 90
      routing =
        let l k = case atP ["provider_routing", k] cli of
              Just (PList xs) -> [s | PStr s <- xs]
              _ -> []
            m k = textAt ["provider_routing", k] cli
         in Routing (l "only") (l "ignore") (l "order") (m "sort") (maybe False truthy (atP ["provider_routing", "require_parameters"] cli)) (m "data_collection")
      fallback = case get "fallback_model" cli of
        Just d@(PDict _) | Just p <- textAt ["provider"] d, Just m <- textAt ["model"] d -> Just (p, m)
        _ -> Nothing
      verbose = coVerbose opts
  ephEnv <- sysGetEnv sys0 "GAUSS_EPHEMERAL_SYSTEM_PROMPT"
  promptFile <- traverse (fmap decodeUtf8Lenient . BS.readFile) (coSystemPromptFile opts)
  let systemPrompt = case (promptFile, ephEnv) of
        (Just f, _) -> f
        (_, Just e) | not (T.null e) -> e
        _ -> fromMaybe "" (textAt ["agent", "system_prompt"] cli)
  prefill <- case textAt ["agent", "prefill_messages_file"] cli of
    Nothing -> pure []
    Just f -> do
      let p = if "/" `T.isPrefixOf` f then T.unpack f else sysGaussHome sys0 </> T.unpack f
      ex <- doesFileExist p
      if not ex then pure [] else do
        b <- BS.readFile p
        pure $ case loads b of
          Right (PList xs) -> xs
          _ -> []
  let cfg =
        defaultAgentConfig
          { cfgModel = model, cfgApiKey = field' "api_key", cfgBaseUrl = field' "base_url", cfgBaseUrlGiven = True
          , cfgProvider = if T.null (field' "provider") then "openrouter" else field' "provider"
          , cfgApiMode = fromMaybe ChatCompletions (parseApiMode (field' "api_mode"))
          , cfgMaxIterations = maxTurns, cfgEnabledToolsets = Just enabledSets, cfgVerbose = verbose, cfgQuiet = not verbose
          , cfgEphemeral = if T.null systemPrompt then Nothing else Just systemPrompt, cfgPrefill = prefill
          , cfgReasoning = textAt ["agent", "reasoning_effort"] cli >>= parseReasoning, cfgRouting = routing
          , cfgSessionId = Just sid, cfgPlatform = Just "cli", cfgPassSessionId = coPassSessionId opts, cfgFallback = fallback }
      sys = recordUuids lg sys0
      reg = atRegistry at
      deps = csDeps cs
  tr0 <- cdTransport deps
  -- Fisher only: the UI's hooks (docs/superpowers/plans/2026-10-16-fisher-ui.md
  -- task 4): the step log, the run lock and the inbox always; the ledger
  -- with FISHER_UI_LEDGER=1, and FISHER_UI_TAG's run recorded in it
  uiOn <- (== Just "1") <$> sysGetEnv sys0 "FISHER_UI_LEDGER"
  -- keyed on the chat's root session: a resumed compression child (the
  -- UI's hand back resumes the chain's latest) writes where its root's are
  uiKey <- maybe (pure sid) (\st -> either (\(_ :: SomeException) -> sid) id <$> try (rootSessionId st sid)) store
  hooks <- readIORef (csUiHooks cs) >>= \case
    Just h -> pure h
    Nothing -> do
      let gauss = sysGaussHome sys0
      ledger <-
        if uiOn
          then either (\(_ :: SomeException) -> Nothing) Just <$> try (openLedger (gauss </> "state.db") uiKey)
          else pure Nothing
      tag <- sysGetEnv sys0 "FISHER_UI_TAG"
      forM_ ledger $ \l -> forM_ tag $ \t -> unless (T.null t) $ () <$ try @SomeException (recordRun l t (sysPid sys0))
      -- the tag names this process's run only: a chat the tools start
      -- (their subprocesses take this process's environment) is not it
      () <$ try @SomeException (unsetEnv "FISHER_UI_TAG")
      -- the run lock, held while this process lives, tells the UI the
      -- session runs here (it then sends through the inbox, never resumes)
      locked <- holdRunLock gauss uiKey
      unless locked $
        hPutStrLn stderr ("fisher: session " <> T.unpack uiKey <> " is running in another process too (" <> runPidPath gauss uiKey <> ")")
      -- a step's file is the one the tool wrote: its path through the
      -- tool's own FileOps (TERMINAL_CWD, ~)
      h <- newUiHooks ledger (stepsPath gauss uiKey) (toolPathResolver (ftOps (atFileTools at)) (cdCwd deps))
      writeIORef (csUiHooks cs) (Just h)
      -- how this chat was started (its flags and the workflow's variables,
      -- never a key), so the UI can resume it as it ran
      when (uiHasLedger h) $ do
        vars <- fmap concat . forM resumeVars $ \k -> maybe [] (\v -> [(K.fromText k, A.String v)]) <$> sysGetEnv sys0 k
        let o = csOpts cs
            jtext = maybe A.Null A.String
        uiEmit h "chat"
          [ ("model", A.String model), ("provider", jtext (coProvider o))
          , ("toolsets", maybe A.Null (A.toJSON) (coToolsets o))
          , ("systemPromptFile", maybe A.Null (A.String . T.pack) (coSystemPromptFile o))
          , ("mcpConfig", maybe A.Null (A.String . T.pack) (coMcpConfig o))
          , ("context", jtext (coContext o)), ("maxTurns", maybe A.Null A.toJSON (coMaxTurns o))
          , ("env", A.Object (KM.fromList vars)) ]
      pure h
  -- the inbox is read with or without the ledger: the UI sends to a
  -- session that runs elsewhere through it
  let takeSent = do
        r <- try @SomeException (takeInbox (inboxPath (sysGaussHome sys0) uiKey))
        let ts = either (const []) id r
        forM_ ts $ \t -> uiEmit hooks "inbox" [("text", A.String t)]
        pure ts
  inbox <- recordInbox lg takeSent
  -- the tools reach the network themselves, outside the replay log (a
  -- replay answers a tool call from its logged result)
  net <- readIORef (csNet cs) >>= \case
    Just n -> pure n
    Nothing -> do
      n <- newNetTools sys0 (cdRuntime deps) tr0 (cdWhich deps) (pure (maybe False truthy (atP ["browser", "record_sessions"] cli))) (chatNetUi (csUi cs)) at
      writeIORef (csNet cs) (Just n)
      pure n
  registerNetTools reg net
  registerChatToolsets reg enabledSets
  let browserEnv = ntBrowser net
  (defs0, printed) <- getToolDefinitions reg (Just enabledSets) Nothing
  let defs = maybe defs0 fst (cdTools deps)
  when (coStream opts) $ do
    servers <- mcpConfigServers opts
    let names = [tdName' d | d <- defs]
        status n = if any (("mcp_" <> T.replace "." "_" (T.replace "-" "_" n) <> "_") `T.isPrefixOf`) names then "connected" else "failed"
    streamEvent (csEvents cs) $ PDict
      [ ("type", PStr "system"), ("subtype", PStr "init"), ("session_id", PStr sid), ("model", PStr model)
      , ("tools", PList (map PStr names))
      , ("mcp_servers", PList [PDict [("name", PStr n), ("status", PStr (status n))] | (n, _) <- servers]) ]
  emojis <- forM [tdName' d | d <- defs] $ \n -> (,) n <$> emojiFor reg n
  -- cost events only when there is a ledger to write them to
  let tr = (if uiHasLedger hooks then uiTransport hooks else id) (recordTransport lg tr0)
      runner0 =
        ToolRunner
          { trDefinitions = defs
          , trPrinted = printed
          , trCall = \name args task enabled -> do
              -- the sandbox's first use starts upstream's cleanup thread, whose
              -- first pass imports the process registry (and its tool)
              when (name `elem` ["terminal", "read_file", "write_file", "patch", "search_files"]) (ntFirstUse net)
              handleFunctionCall reg defaultCtx {tcTaskId = task, tcEnabledTools = enabled} name args
          , trEmoji = \n -> lookup n emojis
          , trReadFilesSummary = \task -> do
              v <- readFilesSummary (atFileTools at) task
              pure [(p, [x | PStr x <- rs]) | PList fs <- [v], f <- fs, Just (PStr p) <- [get "path" f], Just (PList rs) <- [get "regions" f]]
          , trCleanup = \task -> cleanupTask (atSandboxes at) task >> cleanupBrowser browserEnv task
          , trSetInterrupt = \on -> (if on then Sb.raiseInterrupt else Sb.clearInterrupt) (seInterrupt (atSandboxEnv at))
          , trRequirements = withUnportedToolsets <$> checkToolsetRequirements reg }
      runner = recordToolRunner lg (maybe id snd (cdTools deps) runner0)
  st <- newAuxState
  cache <- newModelCache
  tty <- cdAnimate deps
  let rtime = cdRuntime deps
      auxEnv = AuxEnv sys rtime tr (loadConfigOrdered sys) (\_ -> pure ()) st
      env =
        AgentEnv
          { envSys = sys {sysOut = uiRaw (csUi cs)}, envRuntime = rtime, envTransport = tr
          , envLlm = Env (sysUuidHex sys) (\_ r -> pure ("[The " <> r <> " attached an image, which Fisher cannot describe yet.]"))
          , envNowLocal = cdNowLocal deps, envPromptNow = cdNowLocal deps
          , envCwd = cdCwd deps, envChoice = cdChoice deps
          , envAnimate = tty, envDb = fmap (\s -> uiSessionDB hooks (storeSessionDB s (sysNow sys))) store, envTools = runner
          , envAux = callLlm auxEnv, envAuxEnv = Just auxEnv
          , envMeta = MetaEnv tr (sysNow sys) (sysGaussHome sys) cache (\_ -> pure ())
          , envSkin = D.Skin (skinBranding (csSkin cs) "tool_prefix" "┊") [] [], envConfig = csConfig cs
          , envCallbacks = uiCallbacks hooks noCallbacks {cbThinking = Just (\_ -> pure ()), cbToolProgress = Just progress}
          , envWarn = \_ -> pure ()
          , envPromptHook = recordPromptHook lg
          , envWriteFiles = True, envLocalTool = recordLocalTool lg, envInbox = inbox }
  ag <- newAgent cfg env
  -- AIAgent() imports the todo tool, and the memory tool when memory is
  -- on, after it has built its tool list: later agents' registries hold them
  register reg todoToolDef
  when (isJust (agentMemoryStore ag)) (register reg memoryToolDef)
  snapshotEnv sys >>= logAgent lg cfg runner
  case contextChoice (coContext opts) (csConfig cs) of
    ChooseCompress -> pure ()
    -- Fisher only: the context as a file (docs/clm.md)
    ChooseClm -> do
      -- warnings on stderr: stdout may be stream-json
      r <- clmSetupFor sys0 (TIO.hPutStrLn stderr) (csConfig cs) sid model (Just lg)
      case r of
        Left e -> throwIO (ClmRefused e)
        Right (setup, loaded) -> do
          setContextStrategy ag (UseClm setup)
          let path = Clm.mioPath (Clm.csIO setup)
          setContextPaths (atFileTools at) (concatMap pathAliases [path, prevPath path])
          when (csResumed cs) $ forM_ loaded $ \saved -> modifyIORef (csHistory cs) (`liveHistory` saved)
          logClm lg setup loaded
    -- Fisher only: the middle of the history moved to recall's store
    -- (spec phase 2); state.db keeps the full transcript, so a resume
    -- rebuilds the live history from it and rlm.json
    ChooseRlm -> do
      -- the warning on stderr: stdout may be stream-json
      r <- rlmSetupFor sys0 (TIO.hPutStrLn stderr) (csConfig cs) sid model (Just lg)
      case r of
        Left e -> throwIO (RlmRefused e)
        Right (setup, loaded) -> do
          setContextStrategy ag (UseRlm setup)
          when (csResumed cs) $ forM_ loaded $ \saved -> modifyIORef (csHistory cs) (Rlm.liveHistory saved)
          logRlm lg setup loaded
  pure ag
  where
    tdName' d = maybe "" pyStrOf (get "function" d >>= get "name")
    -- the loop reports each call it runs once, before running it
    progress name _ args = do
      modifyIORef' (csToolCalls cs) (+ 1)
      when (coStream (csOpts cs)) $
        streamEvent (csEvents cs) $ PDict
          [ ("type", PStr "assistant")
          , ("message", PDict [("role", PStr "assistant"), ("content", PList [PDict [("type", PStr "tool_use"), ("name", PStr name), ("input", args)]])]) ]

-- | One line of @--output-format stream-json@ (Claude Code's event shapes:
-- @system/init@, @assistant@ with @tool_use@ blocks, @result@).
streamEvent :: (Text -> IO ()) -> PyVal -> IO ()
streamEvent emit v = emit (dumpsUnicode v <> "\n")

-- | Ctrl-C while the agent runs interrupts it (a second press is left to
-- the default handler).
withInterruptHandler :: Sys -> Agent -> IO a -> IO a
withInterruptHandler sys ag act = do
  pressed <- newIORef False
  let handler = Sig.Catch $ do
        was <- atomicModifyIORef' pressed (\p -> (True, p))
        unless was $ do
          sysOut sys "\n⚡ Interrupting agent... (press Ctrl+C again to force exit)\n"
          interruptAgent ag Nothing
  bracket (Sig.installHandler Sig.sigINT handler Nothing) (\old -> Sig.installHandler Sig.sigINT old Nothing) (const act)

-- | @GaussCLI.chat(message)@: the response, or Nothing when the agent
-- could not be made.
chatTurn :: ChatSession -> Text -> IO (Maybe Text)
chatTurn cs message = fst <$> chatTurnWith cs message

-- | 'chatTurn', with the run's result as well (Nothing when the agent
-- could not be made).
chatTurnWith :: ChatSession -> Text -> IO (Maybe Text, Maybe AgentResult)
chatTurnWith cs message = do
  ok <- ensureCredentials cs
  ok2 <- if ok then initAgent cs else pure False
  if not ok2
    then pure (Nothing, Nothing)
    else do
      Just ag <- readIORef (csAgent cs)
      modifyIORef (csHistory cs) (<> [PDict [("role", PStr "user"), ("content", PStr message)]])
      let ui = csUi cs
          accent = skinColor (csSkin cs) "ui_accent" "#FFBF00"
      uiConsole ui ("[" <> accent <> "]" <> T.replicate 40 "─" <> "[/]")
      uiPrint ui ""
      hist <- readIORef (csHistory cs)
      let ri = (runInput message) {riHistory = init hist, riTaskId = Just (csSessionId cs)}
      lg <- readIORef (csReplay cs)
      forM_ lg (`logTurn` ri)
      r <- try @SomeException (withInterruptHandler (csSys cs) ag (runConversation ag ri))
      readIORef (csUiHooks cs) >>= mapM_ (\h -> uiEnd h (endStatus (either (const Nothing) Just r)))
      case r of
        Left e -> uiPrint ui ("Error: " <> T.pack (displayException e)) >> pure (Nothing, Just (RunRaised "Exception" (T.pack (displayException e))))
        Right res -> fmap (\t -> (t, Just res)) $ do
          forM_ lg (`logOutcome` res)
          hFlush stdout
          case res of
            RunRaised _ m -> uiPrint ui ("Error: " <> m) >> pure Nothing
            RunDone d -> do
              case get "messages" d of
                Just (PList ms) -> writeIORef (csHistory cs) ms
                _ -> pure ()
              let response0 = case get "final_response" d of
                    Just (PStr s) -> s
                    _ -> ""
                  failed = get "failed" d == Just (PBool True) || get "partial" d == Just (PBool True)
                  response1 =
                    if failed && T.null response0
                      then "Error: " <> maybe "Unknown error" pyStrOf (get "error" d)
                      else response0
                  interruptMsg = case get "interrupt_message" d of
                    Just (PStr m) | not (T.null m) -> Just m
                    _ -> Nothing
                  response =
                    if get "interrupted" d == Just (PBool True) && not (T.null response1) && isJust interruptMsg
                      then response1 <> "\n\n---\n_[Interrupted - processing new message]_"
                      else response1
                  previewed = get "response_previewed" d == Just (PBool True)
              when (maybe False truthy (atP ["display", "show_reasoning"] (csCli cs))) $
                forM_ (get "last_reasoning" d) $ \case
                  PStr reasoning | not (T.null reasoning) -> do
                    w <- uiWidth ui
                    mapM_ (uiPrint ui) (reasoningBox w reasoning)
                  _ -> pure ()
              unless (T.null response || previewed) $ do
                w <- uiWidth ui
                mapM_ (uiPrint ui) (responseBox (uiAnsi ui) (csSkin cs) w response)
              when (maybe False truthy (atP ["display", "bell_on_complete"] (csCli cs))) $ uiRaw ui "\a"
              pure (Just response)

-- | The reasoning box (dim, full width).
reasoningBox :: Int -> Text -> [Text]
reasoningBox w reasoning =
  let label = " Reasoning "
      top = "┌─" <> label <> T.replicate (max (w - 2 - T.length label - 1) 0) "─" <> "┐"
      bottom = "└" <> T.replicate (w - 2) "─" <> "┘"
      ls = T.lines (T.strip reasoning)
      body = if length ls > 10 then T.intercalate "\n" (take 10 ls) <> "\n  ... (" <> T.pack (show (length ls - 10)) <> " more lines)" else T.strip reasoning
   in ["", top] <> T.splitOn "\n" body <> [bottom]

-- | The response panel: horizontal rules, the skin's label at the top
-- left, the text wrapped two columns in from each side.
responseBox :: Bool -> Skin -> Int -> Text -> [Text]
responseBox ansi skin w response =
  let label = skinBranding skin "response_label" "∑ Gauss"
      border = skinColor skin "response_border" "#CD7F32"
      text = skinColor skin "banner_text" "#FFF8DC"
      st = parseStyle
      p =
        (panel (RTxt (plainText (stripAnsi response))))
          { pBox = boxHorizontals, pTitle = Just ("[" <> border <> " bold]" <> label <> "[/]"), pTitleAlign = JLeft
          , pBorderStyle = st border, pStyle = st text, pPadding = (1, 2, 1, 2) }
      ls = chatLines (printLines w Nothing (RPanel p))
   in map (if ansi then lineAnsi else linePlain) ls
  where
    stripAnsi t = case T.breakOn "\ESC[" t of
      (a, b) | T.null b -> a
      (a, b) -> a <> stripAnsi (T.drop 1 (T.dropWhile (\c -> not (c >= '@' && c <= '~')) (T.drop 2 b)))

-- | @_print_exit_summary()@'s lines.
exitSummaryLines :: Skin -> Text -> Int -> [PyVal] -> [Text]
exitSummaryLines skin sid elapsed hist
  | null hist = ["", skinBranding skin "goodbye" "Goodbye! @"]
  | otherwise =
      let users = length [() | x <- hist, get "role" x == Just (PStr "user")]
          -- Fisher only: each executed call once, as its tool result (upstream
          -- counts the results and the assistant messages that carry calls);
          -- the results the loop wrote for calls it did not run are left
          -- out: those an interrupt skipped, and a call to an unknown tool
          -- with its siblings in that turn
          skippedNote c =
            any (`T.isPrefixOf` c) ["[Tool execution cancelled ", "[Tool execution skipped ", "Skipped: another tool call in this turn used an invalid name."]
              || ("Tool '" `T.isPrefixOf` c && "' does not exist. Available tools: " `T.isInfixOf` c)
          tools = length [() | x <- hist, get "role" x == Just (PStr "tool"), not (maybe False (skippedNote . pyStrOf) (get "content" x))]
          (h, rest) = elapsed `divMod` 3600
          (m, s) = rest `divMod` 60
          dur
            | h > 0 = T.pack (show h) <> "h " <> T.pack (show m) <> "m " <> T.pack (show s) <> "s"
            | m > 0 = T.pack (show m) <> "m " <> T.pack (show s) <> "s"
            | otherwise = T.pack (show s) <> "s"
       in [ "", "Resume this session with:", "  gauss --resume " <> sid, ""
          , "Session:        " <> sid, "Duration:       " <> dur
          , "Messages:       " <> T.pack (show (length hist)) <> " (" <> T.pack (show users) <> " user, " <> T.pack (show tools) <> " tool calls)" ]

printExitSummary :: ChatSession -> IO ()
printExitSummary cs = do
  hist <- readIORef (csHistory cs)
  t <- cdNowLocal (csDeps cs)
  let secs = floor (realToFrac (diffLocalTime t (csStart cs)) :: Double)
  mapM_ (uiPrint (csUi cs)) (exitSummaryLines (csSkin cs) (csSessionId cs) secs hist)

-- ------------------------------------------------------------------ the shell

-- | The shell's plain text: one native session for the shell's session
-- id, opened on the first turn and replaced when @/clear@ gives the
-- shell a new id; the exit summary when the shell ends.
data ShellChat = ShellChat
  { scTurn :: Text -> Text -> IO () -- ^ session id, message
  , scExit :: IO Bool -- ^ printed the exit summary (else the shell says goodbye)
  }

shellChat :: ChatDeps -> Sys -> ChatUi -> ChatOptions -> IORef (Maybe ChatSession) -> ShellChat
shellChat deps sys ui opts ref = ShellChat turn exit
  where
    fresh sid = do
      cs <- newChatSession deps sys ui opts sid False
      writeIORef ref (Just cs)
      pure cs
    session sid =
      readIORef ref >>= \case
        Just cs | csSessionId cs == sid -> pure cs
        Just cs -> flushChatMemories cs >> closeChatSession cs >> fresh sid
        Nothing -> fresh sid
    turn sid msg = do
      cs <- session sid
      _ <- chatTurn cs msg
      pure ()
    exit =
      readIORef ref >>= \case
        Nothing -> pure False
        Just cs -> do
          hist <- readIORef (csHistory cs)
          flushChatMemories cs
          unless (null hist) (printExitSummary cs)
          closeChatSession cs
          writeIORef ref Nothing
          pure (not (null hist))

-- ------------------------------------------------------------------ fisher chat -q

-- | @_has_any_provider_configured()@
hasAnyProvider :: Sys -> IO Bool
hasAnyProvider sys = do
  let vars = ["OPENROUTER_API_KEY", "OPENAI_API_KEY", "ANTHROPIC_API_KEY", "ANTHROPIC_TOKEN", "OPENAI_BASE_URL"] <> concat [pcApiKeyEnvVars p | p <- providerRegistry, pcAuthType p == ApiKey]
  set' <- or <$> mapM (\v -> maybe False (not . T.null) <$> sysGetEnv sys v) vars
  if set'
    then pure True
    else do
      authOk <- try @SomeException $ do
        let p = sysGaussHome sys </> "auth.json"
        ex <- doesFileExist p
        if not ex then pure False else do
          b <- BS.readFile p
          case loads b of
            Right d | Just (PStr active) <- get "active_provider" d, not (T.null active) -> do
              st <- getAuthStatus sys (Just active)
              pure (get "logged_in" st == Just (PBool True))
            _ -> pure False
      pure (either (const False) id authOk)

data QueryArgs = QueryArgs
  { qaQuery :: Maybe Text
  , qaQuiet :: Bool
  , qaOpts :: ChatOptions
  , qaContinue :: Maybe (Maybe Text)
  , qaPrint :: Bool
  -- ^ Fisher only: @-p@ / @--print@
  }

parseQueryArgs :: [Text] -> Either Text QueryArgs
parseQueryArgs = go (QueryArgs Nothing False defaultChatOptions Nothing False)
  where
    go acc = \case
      [] -> Right acc
      (f : v : r) | f `elem` ["-q", "--query"] -> go acc {qaQuery = Just v} r
      (f : v : r) | f `elem` ["-m", "--model"] -> go acc {qaOpts = (qaOpts acc) {coModel = Just v}} r
      (f : v : r) | f `elem` ["-t", "--toolsets"] -> go acc {qaOpts = (qaOpts acc) {coToolsets = Just (map T.strip (T.splitOn "," v))}} r
      ("--provider" : v : r) -> go acc {qaOpts = (qaOpts acc) {coProvider = Just v}} r
      (f : r) | f `elem` ["-v", "--verbose"] -> go acc {qaOpts = (qaOpts acc) {coVerbose = True}} r
      (f : r) | f `elem` ["-Q", "--quiet"] -> go acc {qaQuiet = True} r
      (f : v : r) | f `elem` ["-r", "--resume"] -> go acc {qaOpts = (qaOpts acc) {coResume = Just v}} r
      (f : v : r) | f `elem` ["-c", "--continue"], not ("-" `T.isPrefixOf` v) -> go acc {qaContinue = Just (Just v)} r
      (f : r) | f `elem` ["-c", "--continue"] -> go acc {qaContinue = Just Nothing} r
      ("--pass-session-id" : r) -> go acc {qaOpts = (qaOpts acc) {coPassSessionId = True}} r
      (f : v : r) | f `elem` ["-p", "--print"] -> go acc {qaQuery = Just v, qaQuiet = True, qaPrint = True} r
      ("--system-prompt-file" : v : r) -> go acc {qaOpts = (qaOpts acc) {coSystemPromptFile = Just (T.unpack v)}} r
      ("--mcp-config" : v : r) -> go acc {qaOpts = (qaOpts acc) {coMcpConfig = Just (T.unpack v)}} r
      ("--max-turns" : v : r) -> case reads (T.unpack v) of
        [(n, "")] | n > 0 -> go acc {qaOpts = (qaOpts acc) {coMaxTurns = Just n}} r
        _ -> Left ("argument --max-turns: invalid positive int value: '" <> v <> "'")
      ("--context" : v : r)
        | validContext v -> go acc {qaOpts = (qaOpts acc) {coContext = Just v}} r
        | otherwise -> Left ("argument --context: invalid choice: '" <> v <> "' (choose from 'compress', 'clm', 'rlm')")
      ("--output-format" : v : r)
        | v == "stream-json" -> go acc {qaOpts = (qaOpts acc) {coStream = True}} r
        | v == "text" -> go acc {qaOpts = (qaOpts acc) {coStream = False}} r
        | otherwise -> Left ("argument --output-format: invalid choice: '" <> v <> "' (choose from 'text', 'stream-json')")
      (f : r) | f `elem` ["--yolo", "--checkpoints", "-w", "--worktree"] -> go acc r
      (x : _) -> Left ("unrecognized arguments: " <> x)

-- | @gauss chat -q QUERY@ (and @-Q@): one turn with the native agent.
chatMain :: ChatDeps -> Sys -> [Text] -> IO Int
chatMain deps sys0 args = do
  loadDotenvIntoSys sys0
  case parseQueryArgs args of
    Left e -> do
      outLn sys0 "usage: fisher chat [-q QUERY] [-m MODEL] [-t TOOLSETS] [--provider PROVIDER] [-v] [-Q] [--resume SESSION_ID] [--continue [SESSION_NAME]] [--pass-session-id] [--context {compress,clm,rlm}]"
      outLn sys0 ("fisher chat: error: " <> e)
      pure 2
    Right qa -> do
      store <- either (const Nothing) Just <$> try @SomeException (openStore (sysGaussHome sys0 </> "state.db"))
      resume <- case (qaContinue qa, coResume (qaOpts qa), store) of
        (Just (Just name), Nothing, Just st) -> resolveSessionId st name
        (_, Just r, Just st) -> Just . fromMaybe r <$> resolveSessionId st r
        (_, Just r, Nothing) -> pure (Just r)
        _ -> pure Nothing
      -- Fisher only: under stream-json the session's text goes to stderr
      -- and stdout carries only the events, the last one the result; under
      -- -p it carries only the answer and the session id line
      let stream = coStream (qaOpts qa)
          strictExit = qaPrint qa || stream
          sysH = if stream || qaPrint qa then onStderr sys0 else sys0
          events = sysOut sys0
      hasProvider <- hasAnyProvider sys0
      if not hasProvider && isJust (qaQuery qa)
        then do
          mapM_ (outLn sysH) ["", "Gauss query mode needs a configured provider.", "", "  Run:  gauss setup", ""]
          when stream $ streamResult events "" Nothing 0 Nothing
          pure 1
        else do
          sysSetEnv sys0 "GAUSS_INTERACTIVE" "1"
          t <- cdNowLocal deps
          h <- sysUuidHex sys0
          let sid = fromMaybe (T.pack (formatTime defaultTimeLocale "%Y%m%d_%H%M%S" t) <> "_" <> T.take 6 h) resume
          ansi <- (&&) <$> sysStdoutTty sys0 <*> (not <$> plainForced)
          let width = do
                c <- sysGetEnv sys0 "COLUMNS"
                pure (maybe 80 (\x -> case reads (T.unpack x) of [(n, "")] | n > 0 -> n; _ -> 80) c)
              renderLine l = if ansi then lineAnsi l else linePlain l
              ui =
                ChatUi
                  { uiPrint = \l -> sysOut sysH (l <> "\n")
                  , uiRaw = sysOut sysH
                  , uiConsole = \m -> do
                      w <- width
                      mapM_ (\l -> sysOut sysH (renderLine l <> "\n")) (chatLines (printLines w Nothing (markup m)))
                  , uiWidth = width
                  , uiAnsi = ansi }
          -- Fisher only: in a Rocq or Isabelle project, that prover's
          -- toolset and server unless -t says otherwise
          project <-
            if wantsProject (coToolsets (qaOpts qa)) (isJust (coMcpConfig (qaOpts qa)))
              then cdCwd deps >>= chatProject
              else pure Nothing
          let opts = (qaOpts qa) {coQuiet = qaQuiet qa, coProject = project, coToolsets = defaultToolsets project (coToolsets (qaOpts qa))}
          cs <- newChatSessionWith events deps sysH ui opts sid (isJust resume)
          code <- case qaQuery qa of
            Nothing -> outLn sys0 "fisher chat: give the query with -q (the interactive session is `fisher`)" >> pure 2
            Just q
              | qaQuiet qa -> do
                  ok <- initAgent cs
                  if not ok
                    then do
                      when stream $ streamResult events sid Nothing 0 Nothing
                      pure (failureCode strictExit Nothing)
                    else do
                      Just ag <- readIORef (csAgent cs)
                      lg <- readIORef (csReplay cs)
                      let ri = runInput q
                      forM_ lg (`logTurn` ri)
                      -- Fisher only: an exception from the run is its result,
                      -- as in a turn of the shell, so the stream still ends
                      -- with one result event
                      r <- try @SomeException (runConversation ag ri)
                      res <- case r of
                        Right res -> forM_ lg (`logOutcome` res) >> pure res
                        Left e -> pure (RunRaised "Exception" (T.pack (displayException e)))
                      readIORef (csUiHooks cs) >>= mapM_ (\uh -> uiEnd uh (endStatus (Just res)))
                      if stream
                        then readIORef (csToolCalls cs) >>= \n -> streamResult events sid (Just ag) n (Just res)
                        else do
                          case res of
                            RunDone d -> case get "final_response" d of
                              Just (PStr answer) | not (T.null answer) -> outLn sys0 answer
                              _ -> pure ()
                            RunRaised _ m -> outLn sys0 ("Error: " <> m)
                          outLn sys0 ("\nsession_id: " <> sid)
                      pure (failureCode strictExit (Just res))
              | otherwise -> do
                  -- show_banner, then a blank line
                  (cwd, proj, uni) <- cdBannerInfo deps
                  w <- width
                  model <- readIORef (csModel cs)
                  let skin = csSkin cs
                      compact = maybe False truthy (atP ["display", "compact"] (csCli cs))
                      ls
                        | compact || w < 34 = compactBanner w uni skin
                        | otherwise =
                            welcomeBanner
                              BannerInput
                                { biWidth = w, biUnicode = uni, biSkin = skin, biModel = model, biCwd = cwd
                                , biProject = proj, biSession = Just sid, biSwarmLine = Nothing, biBehind = Nothing }
                  mapM_ (\l -> sysOut sysH (renderLine l <> "\n")) ls
                  outLn sysH ""
                  uiConsole ui ("[bold blue]Query:[/] " <> q)
                  (_, res) <- chatTurnWith cs q
                  printExitSummary cs
                  when stream $ do
                    ag <- readIORef (csAgent cs)
                    n <- readIORef (csToolCalls cs)
                    streamResult events sid ag n res
                  pure (failureCode strictExit res)
          closeChatSession cs
          -- Fisher only: --context rlm without a usable python3 is a usage
          -- error, as a bad --context is
          refused <- readIORef (csRefused cs)
          pure (if refused then 2 else code)

  where
    -- Fisher only: under -p or stream-json a failed run (the result's
    -- is_error) exits 1, so a headless launch reports it; -q and -Q alone
    -- keep upstream's 0
    failureCode strict res = if strict && runFailed res then 1 else 0
    -- the shell's own checks for colour: GAUSS_FORCE_PLAIN_OUTPUT, NO_COLOR, TERM=dumb
    plainForced = do
      a <- sysGetEnv sys0 "GAUSS_FORCE_PLAIN_OUTPUT"
      b <- sysGetEnv sys0 "NO_COLOR"
      t <- sysGetEnv sys0 "TERM"
      pure (maybe False (\v -> T.toLower v `elem` ["1", "true", "yes", "on"]) a || isJust b || t == Just "dumb")

-- | Fisher only: the session's text on stderr (prompts too), for
-- stream-json, whose stdout carries only the events, and for -p, whose
-- stdout carries only the answer. There is no password
-- prompt in that mode.
onStderr :: Sys -> Sys
onStderr sys =
  sys
    { sysOut = err
    , sysInput = \p -> err p >> sysInput sys ""
    , sysGetPass = \p -> err (p <> "\n") >> pure Nothing }
  where
    -- UTF-8 whatever the locale, as the banner and the tool lines need it
    err t = BS.hPut stderr (encodeUtf8 (fisherHints t)) >> hFlush stderr

-- | Whether a run failed, as the stream's @is_error@ says: no result (the
-- agent could not be made), a raise, or a run that did not complete.
runFailed :: Maybe AgentResult -> Bool
runFailed = \case
  Just (RunDone d) -> get "completed" d /= Just (PBool True)
  _ -> True

-- | The closing @result@ event of @--output-format stream-json@: the
-- status, the turns, the tool calls run (Fisher only: @num_tool_calls@),
-- and the usage and cost. Without a result the agent could not be made,
-- and the event says so.
streamResult :: (Text -> IO ()) -> Text -> Maybe Agent -> Int -> Maybe AgentResult -> IO ()
streamResult emit sid mag toolCalls mres = do
  (prompt, completion, _, _) <- maybe (pure (0, 0, 0, 0)) agentSessionCounters mag
  (dollars, cacheRead, cacheWrite) <- maybe (pure (Nothing, 0, 0)) agentSessionCost mag
  let maxTurns = maybe 0 (cfgMaxIterations . agentConfig) mag
      usage =
        PDict
          [ ("input_tokens", PInt prompt), ("output_tokens", PInt completion)
          , ("cache_read_input_tokens", PInt cacheRead), ("cache_creation_input_tokens", PInt cacheWrite) ]
      -- Fisher only: the provider's own price when every call reported one
      -- (OpenRouter's usage.cost), else null; the compressor's summary
      -- calls are not included
      base =
        [ ("type", PStr "result"), ("session_id", PStr sid), ("usage", usage), ("total_cost_usd", maybe PNull PFloat dollars)
        , ("num_tool_calls", PInt (fromIntegral toolCalls)) ]
  streamEvent emit . PDict $ case mres of
    Nothing ->
      base <> [("subtype", PStr "error_during_execution"), ("is_error", PBool True), ("num_turns", PInt 0), ("error", PStr "The agent could not be started; the reason is on stderr.")]
    Just (RunDone d) ->
      let calls = case get "api_calls" d of Just (PInt n) -> n; _ -> 0
          completed = get "completed" d == Just (PBool True)
          subtype
            | completed = "success"
            | maxTurns > 0 && calls >= fromIntegral maxTurns = "error_max_turns"
            | otherwise = "error_during_execution"
          final = case get "final_response" d of Just (PStr r) -> PStr r; _ -> PNull
       in base <> [ ("subtype", PStr subtype), ("is_error", PBool (not completed)), ("num_turns", PInt calls)
                  , ("result", final) ] <> [("error", e) | Just e <- [get "error" d]]
    Just (RunRaised cls m) ->
      base <> [("subtype", PStr "error_during_execution"), ("is_error", PBool True), ("num_turns", PNull), ("error", PStr (cls <> ": " <> m))]

-- | @fisher replay SESSION@: run a logged session again and report.
replayMain :: Sys -> [Text] -> IO Int
replayMain sys args = case args of
  [sid] -> do
    let path = replayPath (sysGaussHome sys) sid
    ex <- doesFileExist path
    if not ex
      then outLn sys ("No replay log for " <> sid <> " (" <> T.pack path <> ")") >> pure 1
      else do
        t <- localNow
        r <- replayFile (ReplayBase sys t (loadConfigOrdered sys) (const Nothing)) path
        case r of
          Left e -> outLn sys ("replay: " <> e) >> pure 1
          Right rep
            | null (rpMismatches rep) -> do
                outLn sys ("replay " <> sid <> ": " <> T.pack (show (rpTurns rep)) <> " turn(s), " <> T.pack (show (rpRequests rep)) <> " request(s), bit-exact")
                pure 0
            | otherwise -> do
                outLn sys ("replay " <> sid <> ": " <> T.pack (show (length (rpMismatches rep))) <> " difference(s)")
                mapM_ (outLn sys . ("  " <>)) (rpMismatches rep)
                pure 1
  _ -> outLn sys "usage: fisher replay SESSION_ID" >> pure 2

-- | The terminal's prompts on the chat's output, reading this terminal.
chatNetUi :: ChatUi -> NetUi
chatNetUi ui = ttyUi (uiPrint ui) (uiRaw ui)
