-- | The interactive fisher shell: @fisher@ with no command. Upstream's
-- CLI class's @run@ over prompt_toolkit becomes a haskeline loop; the
-- commands themselves are "Fisher.Shell.Commands", wired here to the
-- project model, the workflow plans and the swarm.
module Fisher.Shell
  ( runShell
  , ShellOptions (..)
  , resolvedWorkflow
  , resolvedChat
  , toRow
  , newSessionId
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (intToDigit)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import System.Environment (getEnvironment, lookupEnv, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Posix.IO (stdOutput)
import System.Process (readCreateProcessWithExitCode, shell)
import System.Timeout (timeout)

import Fisher.Cli.AgentChat (ChatOptions (..), ChatUi (..), ShellChat (..), defaultChatDeps, defaultChatOptions, shellChat)
import Fisher.Cli.Mine (mineImported)
import Fisher.Cli.Session (activeCwd, cliSession, saveOverride)
import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.SysIO (newSysIO)
import Fisher.Config (loadConfigAt)
import Fisher.EnvVars (getEnvValueAt)
import Fisher.Project (formatProjectSummary, projectLabel, projectProver, projectProverRoot, projectRoot)
import Fisher.Prover (ProverKind (..))
import Fisher.ProjectCommand (ProjectSession (..), ProjectState (..), handleProjectTokens, projectState)
import Fisher.Shell.CliConfig
import Fisher.Shell.Commands
import Fisher.Runlog (runlogPath)
import Fisher.Skin (skinBranding, skinColor)
import Fisher.Swarm
import Fisher.Swarm.Event (Progress (..))
import Fisher.Swarm.Import (importFinished, importRunId, sessionHomeOf)
import Fisher.Swarm.Pty (getWinSize)
import Fisher.Swarm.Types
import Fisher.Term (Style (..), detectStyle, renderText)
import Fisher.Workflow.Arxiv (installArxivScript)
import Fisher.Workflow.Command (WorkflowSpec (..), rewriteForgivingCommand)
import Fisher.Workflow.Types (Machine (..))
import Fisher.Workflow.Handoff (HandoffRequest (..), strictModeUnavailableReason)
import Fisher.Workflow.Plan
import Fisher.Workflow.Settings (backendName, normalizeBackendName, supportedBackends)
import Fisher.Workflow.Types (errorMessage, liveMachine)

data ShellOptions = ShellOptions
  { soCompact :: Bool
  , soContext :: Maybe Text
  -- ^ Fisher only: @--context clm|compress@
  }

-- | A fresh session id: @%Y%m%d_%H%M%S@ and six hex digits.
newSessionId :: IO Text
newSessionId = do
  now <- getZonedTime
  t <- nowSeconds
  let salt = (floor (t * 1000003) :: Integer) `mod` (16 ^ (6 :: Int))
      hex = T.justifyRight 6 '0' (T.pack (showHex' salt))
  pure (T.pack (formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now) <> "_" <> hex)
  where
    showHex' 0 = "0"
    showHex' n = reverse (go n)
    go 0 = ""
    go n = intToDigit (fromIntegral (n `mod` 16)) : go (n `div` 16)

terminalWidth :: IO Int
terminalWidth = do
  ws <- getWinSize stdOutput
  case ws of
    Just (_, c) | c > 0 -> pure c
    _ -> maybe 80 (\s -> fromMaybe 80 (readMaybe' s)) <$> lookupEnv "COLUMNS"
  where
    readMaybe' s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

resolveWorkflowRequest' :: LaunchInputs -> Text -> IO (Either Text WorkflowPlan)
resolveWorkflowRequest' li cmd = either (Left . errorMessage) Right <$> resolveWorkflowRequest li cmd

resolveChatRequest' :: LaunchInputs -> Text -> IO (Either Text ChatPlan)
resolveChatRequest' li cmd = either (Left . errorMessage) Right <$> resolveChatRequest li cmd

-- | A resolved workflow, spawned through the given spawner (the swarm's,
-- or a test's) under upstream's names for it.
resolvedWorkflow :: WorkflowPlan -> (SpawnSpec -> IO Text) -> Resolved
resolvedWorkflow p spawn =
  let ws = wpSpec p
      label = projectLabel (wpProject p)
      backend = mcBackend (wpContext p)
      r = wpRequest p
   in Resolved (wsKind ws) (wsBackend ws) (wpInstruction p) (wsCanonical ws) label backend $ \th d ->
        spawn (SpawnSpec th d (hrArgv r) (hrCwd r) (hrEnv r) (wsKind ws) (wsBackend ws) label (projectRoot (wpProject p)) backend (projectProver (wpProject p)))

-- | A resolved managed chat, likewise.
resolvedChat :: ChatPlan -> (SpawnSpec -> IO Text) -> Resolved
resolvedChat p spawn =
  let r = cpRequest p
   in Resolved "chat" "/managed-chat" (cpInstruction p) "/managed-chat" "" (cpBackend p) $ \th d ->
        spawn (SpawnSpec th d (hrArgv r) (hrCwd r) (hrEnv r) "chat" "/managed-chat" "" "" (cpBackend p) Lean4)

toRow :: TaskView -> TaskRow
toRow v =
  let i = tvInfo v
      p = tvProgress v
   in TaskRow
        { trId = tiTaskId i, trDescription = tiDescription i, trTheorem = tiTheorem i, trKind = tiWorkflowKind i
        , trCommand = tiWorkflowCommand i, trProjectName = tiProjectName i, trProjectRoot = T.pack (tiProjectRoot i)
        , trWorkingDir = T.pack (tiWorkingDir i), trBackend = tiBackendName i, trStatus = statusName (tvStatus v)
        , trStart = tvStart v, trEnd = tvEnd v, trProgress = pProgress p, trLean = pLean p
        , trSessionId = if T.null (tiSessionId i) then Nothing else Just (tiSessionId i)
        , trResult = pResult p, trError = pError p, trHasPty = tvHasPty v }

runShell :: ShellOptions -> IO ()
runShell opts = do
  (home, session0) <- cliSession
  let configPath = home </> "config.yaml"
  (cliCfg, fileHasTerminal) <- loadCliConfig configPath
  cwd0 <- activeCwd
  -- load_cli_config's terminal settings, into the environment the
  -- managed children inherit.
  forM_ (terminalEnv cliCfg cwd0) $ \(k, v) -> do
    existing <- lookupEnv k
    when (fileHasTerminal || existing == Nothing) (setEnv k v)
  cwd <- activeCwd
  let session = session0 {sessionCwd = cwd}
  st <- detectStyle
  arxiv <- installArxivScript home
  machine <- liveMachine home (Just arxiv) strictModeUnavailableReason
  realHome <- getHomeDirectory
  sw <- newSwarm (mIsRoot machine) realHome
  started <- getZonedTime
  sid <- newSessionId
  external <- newIORef (\t -> TIO.putStrLn t >> hFlush stdout)
  chatRef <- newIORef Nothing
  -- each workflow task's backend, session home (Claude Code's HOME, Codex's
  -- CODEX_HOME) and cwd, for importing its session file into the run log
  -- when it ends
  imports <- newIORef (Map.empty :: Map.Map Text (Text, FilePath, FilePath))
  llmSys <- newSysIO realHome home
  loadDotenvIntoSys llmSys
  let out = Out (\t -> readIORef external >>= \f -> f t)
      inputsFor active = do
        (cfg, _) <- loadConfigAt home
        env <- Map.fromList <$> getEnvironment
        pure (LaunchInputs machine cfg env True (getEnvValueAt home) (Just (T.unpack active)) Nothing)
      spawn spec = tiTaskId . taskInfo <$> spawnInteractive sw spec
      spawnLogged p spec = do
        tid <- spawn spec
        let c = wpContext p
            r = wpRequest p
        modifyIORef' imports (Map.insert tid (mcBackend c, sessionHomeOf (mcBackend c) (mcBackendHome c) (hrEnv r), hrCwd r))
        pure tid
      -- off the swarm's thread: the import never holds up the task's end;
      -- a task that ends before its spawn returned is looked up again
      importDone v = void . forkIO $ do
        let tid = tiTaskId (tvInfo v)
            target = Map.lookup tid <$> readIORef imports
        found <- target >>= maybe (threadDelay 500000 >> target) (pure . Just)
        forM_ found $ \(backend, sessionHome, taskCwd) -> do
          modifyIORef' imports (Map.delete tid)
          importFinished (runlogPath home) backend sessionHome taskCwd v
          -- the lessons' rules over the imported run (the native backend
          -- logs its own runs, which are not imported)
          -- (warnings through the shell's output, not over its prompt)
          let warn t = readIORef external >>= \f -> f ("fisher: lessons: " <> t)
          when (backend /= "fisher") $
            (try (loadConfigAt home) :: IO (Either SomeException (Value, [Text]))) >>= \case
              Left e -> warn (T.pack (show e))
              Right (cfg, _) -> mineImported warn home cfg (tiProjectRoot (tvInfo v)) (importRunId v)
      deps =
        ShellDeps
          { dOut = out
          , dWidth = terminalWidth
          , dAnsi = styleAnsi st
          , dUnicode = styleUnicode st
          , dRenderTerminal = renderText st
          , dFisherHome = home
          , dConfigPath = configPath
          , dProject = do
              ps <- projectState session
              active <- activeCwd
              case stateProject ps of
                Nothing -> pure (ProjectInfo Nothing (Just (stateSource ps)) (stateError ps))
                Just p -> do
                  summary <- formatProjectSummary p (Just active)
                  pure (ProjectInfo (Just (ProjectView (projectLabel p) (T.pack (projectRoot p)) (T.pack (projectProverRoot p)) summary)) (Just (stateSource ps)) Nothing)
          , dProjectCommand = \toks -> do
              r <- handleProjectTokens session toks
              readIORef (sessionOverride session) >>= saveOverride home
              pure r
          , dHandoffCwd = T.pack <$> activeCwd
          , dEnv = \k -> fmap T.pack <$> lookupEnv (T.unpack k)
          , dSetEnv = \k v -> setEnv (T.unpack k) (T.unpack v)
          , dSaveConfigValue = saveConfigValueAt configPath
          , dResolveWorkflow = \cmd active -> do
              li <- inputsFor active
              fmap (\p -> resolvedWorkflow p (spawnLogged p)) <$> resolveWorkflowRequest' li cmd
          , dResolveChat = \payload active -> do
              li <- inputsFor active
              fmap (`resolvedChat` spawn) <$> resolveChatRequest' li payload
          , dNormalizeBackend = fmap backendName . normalizeBackendName
          , dBackends = map backendName supportedBackends
          , dTasks = map toRow <$> allViews sw
          , dTask = \tid -> getTask sw tid >>= maybe (pure Nothing) (fmap (Just . toRow) . taskView)
          , dCancel = cancelTask sw
          , dAttach = \tid ->
              getTask sw tid >>= \case
                Nothing -> pure (Left ("Task " <> tid <> " not found"))
                Just t -> do
                  io <- terminalAttachIO (Just 0x1d)
                  r <- try (attachTask t io) :: IO (Either SomeException (Either Text Int))
                  pure $ case r of
                    Left e -> Left (T.pack (show e))
                    Right (Left e) -> Left e
                    Right (Right _) -> Right ()
          , dWaitAttachable = \tid ->
              let go :: Int -> IO Bool
                  go 0 = pure False
                  go n =
                    getTask sw tid >>= \case
                      Nothing -> pure False
                      Just t -> do
                        v <- taskView t
                        if tvHasPty v && tvStatus v == Running
                          then pure True
                          else if tvStatus v /= Running && tvStatus v /= Queued then pure False else threadDelay 100000 >> go (n - 1)
               in go 20
          , dNow = nowSeconds
          , dClear = TIO.putStr "\ESC[2J\ESC[H" >> hFlush stdout
          , dNewSessionId = newSessionId
          , dStartedAt = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" started)
          , dRunQuick = \c -> do
              r <- timeout 30000000 (try (readCreateProcessWithExitCode (shell (T.unpack c)) "") :: IO (Either SomeException (ExitCode, String, String)))
              pure $ case r of
                Nothing -> Left "timeout"
                Just (Left e) -> Left (T.pack (show e))
                Just (Right (_, o, e)) -> Right (let so = T.strip (T.pack o) in if T.null so then T.strip (T.pack e) else so)
          , dForgiving = rewriteForgivingCommand
          , dChat = scTurn native
          , dChatExit = scExit native
          }
      native =
        shellChat defaultChatDeps llmSys
          ChatUi
            { uiPrint = outLine out
            , uiRaw = \t -> TIO.putStr t >> hFlush stdout
            , uiConsole = consolePrint deps
            , uiWidth = terminalWidth
            , uiAnsi = styleAnsi st }
          defaultChatOptions {coContext = soContext opts}
          chatRef
  state <- newShellState deps cliCfg sid
  setOnComplete sw (Just (\v -> importDone v >> onTaskDone deps state (toRow v)))
  writeIORef (ssCompact state) (soCompact opts)
  showBanner deps state
  plainPrint deps ""
  welcome deps state
  let settings = (defaultSettings :: Settings IO) {historyFile = Just (home </> ".fisher_history")}
  runInputT settings $ do
    ext <- getExternalPrint
    liftIO (writeIORef external (\t -> ext (T.unpack t <> "\n")))
    let loop = do
          skin <- liftIO (readIORef (ssSkin state))
          let symbol = skinBranding skin "prompt_symbol" "❯ "
              promptColour = skinColor skin "prompt" "#FFF8DC"
              prompt = if styleAnsi st then "\ESC[38;2;" <> rgb promptColour <> "m" <> T.unpack symbol <> "\ESC[0m" else T.unpack symbol
          line <- handleInterrupt (pure (Just "")) (withInterrupt (getInputLine prompt))
          case line of
            Nothing -> pure ()
            Just l -> do
              -- direct writes while a command runs; the external printer
              -- only while waiting at the prompt
              liftIO (writeIORef external (\t -> TIO.putStrLn t >> hFlush stdout))
              continue <- liftIO (submit deps state (T.pack l))
              liftIO (writeIORef external (\t -> ext (T.unpack t <> "\n")))
              when continue loop
    loop
  writeIORef external (\t -> TIO.putStrLn t >> hFlush stdout)
  -- Running sessions end with the shell: nothing could reattach to them.
  views <- allViews sw
  forM_ views $ \v -> unless (tvStatus v `elem` [Complete, Failed, Cancelled]) (() <$ cancelTask sw (tiTaskId (tvInfo v)))
  goodbye deps state
  where
    rgb hex =
      let h = T.unpack (T.dropWhile (== '#') hex)
          byte s = case reads ("0x" <> s) of
            [(n, "")] -> show (n :: Int)
            _ -> "255"
       in case h of
            [a, b, c, d, e, f] -> byte [a, b] <> ";" <> byte [c, d] <> ";" <> byte [e, f]
            _ -> "255;248;220"
