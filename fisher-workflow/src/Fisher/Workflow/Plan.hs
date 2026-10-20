-- | Launch plans: a managed workflow command or @/chat@ resolved into the
-- staged files and the child's argv, cwd and environment
-- (@resolve_autoformalize_request@, @resolve_managed_chat_request@ and
-- @prepare_managed_runtime_assets@). Checks happen in upstream's order, so
-- the same broken setup fails with the same message, after the same files
-- have been written.
module Fisher.Workflow.Plan
  ( LaunchInputs (..)
  , ManagedContext (..)
  , WorkflowPlan (..)
  , ChatPlan (..)
  , stagedPaths
  , resolveWorkflowRequest
  , resolveChatRequest
  , prepareManagedRuntimeAssets
  , printArgv
  , claudeModel
  , nativeProvider
  , nativeOpenRouterModel
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import qualified Data.Text.IO as TIO
import System.Environment (getExecutablePath)
import System.FilePath (searchPathSeparator, (</>))

import Fisher.Paths (expandUser)
import Fisher.Project
import Fisher.Prover (Prover (..), ProverKind (..), ServerSpec (..), findProverRoot, prover, proverTitle, pvToolsFor)
import Fisher.Py (lookupKey, pyStrOr)
import Fisher.Workflow.Assets
import Fisher.Workflow.ClaudePlugin
import Fisher.Workflow.Command
import Fisher.Workflow.Handoff
import Fisher.Workflow.LocalAuth
import Fisher.Workflow.Provers
import Fisher.Workflow.Run
import Fisher.Workflow.Settings
import Fisher.Workflow.Stage
import Fisher.Workflow.Types

claudeModel :: Text
claudeModel = "claude-opus-4-6"

-- | Where the native backend's model calls go, and the model there: the
-- same Opus as the claude-code backend. A Claude subscription login is
-- served to Claude Code only, so the default is OpenRouter
-- (@OPENROUTER_API_KEY@); @anthropic@ takes a Console API key or token.
-- From @FISHER_AUTOFORMALIZE_NATIVE_PROVIDER@, else
-- @fisher.autoformalize.native_provider@.
nativeProvider :: Value -> [(String, String)] -> Either Text (Text, Text)
nativeProvider cfg env =
  let override = T.toLower (T.strip (T.pack (fromMaybe "" (lookup "FISHER_AUTOFORMALIZE_NATIVE_PROVIDER" env))))
      configured = T.toLower (T.strip (pyStrOr (lookupKey "native_provider" (autoformalizeConfig cfg))))
      chosen
        | not (T.null override) = override
        | not (T.null configured) = configured
        | otherwise = "openrouter"
   in case chosen of
        "openrouter" -> Right ("openrouter", nativeOpenRouterModel)
        "anthropic" -> Right ("anthropic", claudeModel)
        _ -> Left "`fisher.autoformalize.native_provider` must be one of: openrouter, anthropic."

nativeOpenRouterModel :: Text
nativeOpenRouterModel = "anthropic/claude-opus-4.6"

data LaunchInputs = LaunchInputs
  { liMachine :: Machine
  , liConfig :: Value
  , liBaseEnv :: Env
  , liIncludePersisted :: Bool
  -- ^ Whether the Fisher home's .env may supply credentials (upstream: when no
  -- base environment was passed in).
  , liGetPersisted :: Text -> IO (Maybe Text)
  , liActiveCwd :: Maybe FilePath
  }

data ManagedContext = ManagedContext
  { mcBackend :: Text
  , mcManagedRoot :: FilePath
  , mcProjectRoot :: FilePath
  , mcLeanRoot :: FilePath
  , mcBackendHome :: FilePath
  , mcPluginRoot :: FilePath
  , mcMcpConfig :: FilePath
  , mcStartupContext :: Maybe FilePath
  , mcAssetsRoot :: FilePath
  , mcManifest :: Maybe FilePath
  , mcBackendConfig :: Maybe FilePath
  , mcSkillsRoot :: Maybe FilePath
  , mcInstructions :: Maybe FilePath
  }
  deriving (Eq, Show)

data WorkflowPlan = WorkflowPlan
  { wpRequest :: HandoffRequest
  , wpContext :: ManagedContext
  , wpInstruction :: Text
  , wpProject :: Project
  , wpSpec :: WorkflowSpec
  }

data ChatPlan = ChatPlan
  { cpRequest :: HandoffRequest
  , cpBackend :: Text
  , cpInstruction :: Text
  , cpCwd :: FilePath
  }

-- | Upstream's @staged_paths()@, for diagnostics and tests.
stagedPaths :: WorkflowPlan -> [(Text, Text)]
stagedPaths p =
  let c = wpContext p
      s = T.pack
      o = maybe "" T.pack
   in [ ("backend_name", mcBackend c), ("workflow_kind", wsKind (wpSpec p))
      , ("frontend_command", wsFrontend (wpSpec p)), ("backend_command", wsBackend (wpSpec p))
      , ("managed_root", s (mcManagedRoot c)), ("project_root", s (mcProjectRoot c))
      , ("lean_root", s (mcLeanRoot c)), ("backend_home", s (mcBackendHome c))
      , ("claude_home", s (mcBackendHome c)), ("plugin_root", s (mcPluginRoot c))
      , ("mcp_config_path", s (mcMcpConfig c)), ("startup_context_path", o (mcStartupContext c))
      , ("backend_config_path", o (mcBackendConfig c)), ("project_manifest_path", o (mcManifest c))
      , ("skills_root", o (mcSkillsRoot c)), ("instructions_path", o (mcInstructions c)) ]

resolveDir :: FilePath -> IO FilePath
resolveDir p = expandUser p >>= canonicalizePath

envList :: Env -> [(String, String)]
envList = Map.toList

activeDir :: LaunchInputs -> IO FilePath
activeDir li = do
  let fromEnv = case Map.lookup "TERMINAL_CWD" (liBaseEnv li) of
        Just c | not (null c) -> Just c
        _ -> Nothing
  cwd <- maybe getCurrentDirectory pure (case liActiveCwd li of
    Just c | not (null c) -> Just c
    _ -> fromEnv)
  dir <- resolveDir cwd
  exists <- doesPathExist dir
  unless exists $ throwW (PreflightError ("Active working directory does not exist: " <> T.pack dir))
  pure dir

realHomeOf :: Env -> IO FilePath
realHomeOf env = maybe getHomeDirectory pure (Map.lookup "HOME" env) >>= resolveDir

either' :: (Text -> WorkflowError) -> Either Text a -> IO a
either' f = either (throwW . f) pure

-- | @_resolve_managed_state_base@.
managedStateBase :: LaunchInputs -> IO FilePath
managedStateBase li = do
  let override = T.strip (T.pack (fromMaybe "" (Map.lookup "FISHER_AUTOFORMALIZE_MANAGED_STATE_DIR" (liBaseEnv li))))
      configured = pyStrOr (lookupKey "managed_state_dir" (autoformalizeConfig (liConfig li)))
      chosen = if T.null override then configured else override
  if T.null chosen
    then resolveDir (mFisherHome (liMachine li) </> "autoformalize")
    else resolveDir (T.unpack chosen)

-- | @_managed_root@, with its fallback to the pre-backend layout.
managedRoot :: LaunchInputs -> FilePath -> Backend -> IO FilePath
managedRoot li base backend = do
  defaultRoot <- resolveDir (mFisherHome (liMachine li) </> "autoformalize")
  let backendRoot = base </> T.unpack (backendName backend) </> "managed"
      legacy = defaultRoot </> "managed"
  legacyExists <- doesPathExist legacy
  backendExists <- doesPathExist backendRoot
  pure $
    if backend == ClaudeCode && base == defaultRoot && legacyExists && not backendExists
      then legacy
      else backendRoot

data Bundle = Bundle
  { bManaged :: FilePath
  , bAssets :: FilePath
  , bStartup :: FilePath
  , bMcp :: FilePath
  , bProject :: Project
  , bActive :: FilePath
  , bRealHome :: FilePath
  , bPaths :: LeanPaths
  , bRunner :: [Text]
  , bRevision :: Text
  , _bBackend :: Backend
  , bModel :: Maybe Text
  -- ^ the chosen model, Nothing for the backend's default
  }

data ClaudeAuthPlan = ClaudeAuthPlan
  { capEnv :: Maybe (Text, Text)
  , capCopyOauth :: Bool
  , capCopyKey :: Bool
  , capStrip :: Bool
  }

claudeAuthPlan :: LaunchInputs -> AuthMode -> FilePath -> IO ClaudeAuthPlan
claudeAuthPlan li mode home = do
  login <- hasLocalClaudeLogin (mKeychain (liMachine li)) home
  key <- hasLocalClaudeApiKey home
  staged <- resolveClaudeAuthEnv (liGetPersisted li) (envList (liBaseEnv li)) (liIncludePersisted li)
  case mode of
    AuthAuto
      | login || key -> pure (ClaudeAuthPlan Nothing login key True)
      | Just _ <- staged -> pure (ClaudeAuthPlan staged False False False)
      | otherwise ->
          throwW . PreflightError $
            "Claude Code auth not found. Run `claude auth login`, save `ANTHROPIC_API_KEY`, "
              <> "or set `fisher.autoformalize.auth_mode: login` "
              <> "(or `FISHER_AUTOFORMALIZE_AUTH_MODE=login`) to launch the normal Claude login flow."
    AuthLogin -> pure (ClaudeAuthPlan Nothing login False True)
    AuthApiKey
      | Just _ <- staged -> pure (ClaudeAuthPlan staged False False True)
      | key -> pure (ClaudeAuthPlan Nothing False True True)
      | otherwise ->
          throwW . PreflightError $
            "Claude Code API-key auth not found. Save `ANTHROPIC_API_KEY`, "
              <> "`ANTHROPIC_TOKEN`, or `CLAUDE_CODE_OAUTH_TOKEN`, or switch "
              <> "`fisher.autoformalize.auth_mode` back to `auto` or `login`."

data CodexAuthPlan = CodexAuthPlan { xapSource :: Maybe FilePath, xapKey :: Text }

codexHomeOf :: FilePath -> Env -> IO FilePath
codexHomeOf home env = case T.strip (T.pack (Map.findWithDefault "" "CODEX_HOME" env)) of
  c | T.null c -> pure (home </> ".codex")
  c -> expandUser (T.unpack c)

codexAuthPlan :: LaunchInputs -> AuthMode -> FilePath -> IO CodexAuthPlan
codexAuthPlan li mode home = do
  authPath <- (</> "auth.json") <$> codexHomeOf home (liBaseEnv li)
  payload <- loadLocalCodexAuthPayload home (envList (liBaseEnv li))
  let valid = codexAuthPayloadIsValid payload
      hasKey = codexAuthPayloadHasApiKey payload
  key <- resolveCodexApiKey (liGetPersisted li) (envList (liBaseEnv li)) (liIncludePersisted li)
  let copy = CodexAuthPlan (Just authPath) ""
  case mode of
    AuthAuto
      | valid -> pure copy
      | not (T.null key) -> pure (CodexAuthPlan Nothing key)
      | otherwise ->
          throwW . PreflightError $
            "Codex auth not found. Run `codex login`, save `OPENAI_API_KEY`, "
              <> "or set `fisher.autoformalize.auth_mode: login` "
              <> "(or `FISHER_AUTOFORMALIZE_AUTH_MODE=login`) to launch the normal Codex login flow."
    AuthLogin -> pure (if valid then copy else CodexAuthPlan Nothing "")
    AuthApiKey
      | not (T.null key) -> pure (CodexAuthPlan Nothing key)
      | hasKey -> pure copy
      | otherwise ->
          throwW . PreflightError $
            "Codex API-key auth not found. Save `OPENAI_API_KEY`, or switch "
              <> "`fisher.autoformalize.auth_mode` back to `auto` or `login`."

claudeExe, codexExe :: Env -> IO FilePath
claudeExe = requireExecutable "claude" "Claude Code CLI not found. Install it with `npm install -g @anthropic-ai/claude-code`."
codexExe = requireExecutable "codex" "Codex CLI not found. Install the OpenAI Codex CLI and try again."

-- | Set variables for a child.
setAll :: [(String, String)] -> Env -> Env
setAll kvs e = foldl (\acc (k, v) -> Map.insert k v acc) e kvs

baseChildEnv :: ManagedContext -> FilePath -> [(String, String)]
baseChildEnv c home =
  [ ("FISHER_AUTOFORMALIZE", "1")
  , ("FISHER_AUTOFORMALIZE_BACKEND", T.unpack (mcBackend c))
  , ("FISHER_PROJECT_ROOT", mcProjectRoot c)
  , ("FISHER_PROJECT_MANIFEST", fromMaybe "" (mcManifest c))
  , ("LEAN_PROJECT_PATH", mcLeanRoot c)
  , ("FISHER_MANAGED_STATE_DIR", mcManagedRoot c)
  , ("FISHER_REAL_HOME", home) ]
    <> [("FISHER_AUTOFORMALIZE_SKILLS_ROOT", s) | Just s <- [mcSkillsRoot c]]

permissionArgs :: Machine -> [Text]
permissionArgs m = if mIsRoot m then ["--permission-mode", "bypassPermissions"] else ["--dangerously-skip-permissions"]

startupPrompt :: ManagedContext -> WorkflowSpec -> IO (Maybe Text)
startupPrompt c ws = case mcStartupContext c of
  Nothing -> pure Nothing
  Just ctx -> do
    let quoted = shlexQuote (T.pack ctx)
    if mcBackend c == "codex"
      then do
        skillDoc <- case mcSkillsRoot c of
          Just s -> doesFileExist (s </> "SKILL.md") >>= \ok -> pure (if ok then Just (s </> "SKILL.md") else Nothing)
          Nothing -> pure Nothing
        doc <- workflowDocPath (mcPluginRoot c) (wsKind ws)
        pure . Just . T.unwords $
          [ "You are in a Fisher-managed Lean workflow session."
          , "Read the startup context at " <> quoted <> " first."
          , "Then explicitly invoke the `$lean4` skill and follow the requested Lean workflow in the active project."
          , "Important: `/lean4:*` entries in the startup context are skill workflow names, not shell commands, so do not try to execute them in bash." ]
          <> ["The staged skill entrypoint is " <> shlexQuote (T.pack d) <> "." | Just d <- [skillDoc]]
          <> ["The matching workflow guide is " <> shlexQuote (T.pack d) <> "." | Just d <- [doc]]
      else
        pure . Just $
          "You are in a Fisher-managed Lean workflow session. "
            <> "Read the startup context at " <> quoted <> " first. "
            <> "Then run this command inside the active project as your first workflow action: " <> wsBackend ws

-- | Codex's model flag, for a chosen model; without one Codex keeps its
-- own (or its config's), as upstream.
modelArgs :: Maybe Text -> [Text]
modelArgs = maybe [] (\m -> ["-m", m])

stripKeys :: [Text] -> Env -> Env
stripKeys ks e = foldr (Map.delete . T.unpack) e ks

claudeRuntime :: LaunchInputs -> AuthMode -> Text -> WorkflowSpec -> Bundle -> IO ([Text], Env, ManagedContext)
claudeRuntime li mode instruction ws b = do
  let m = liMachine li
      env = liBaseEnv li
  exe <- claudeExe env
  auth <- claudeAuthPlan li mode (bRealHome b)
  let backendHome = bManaged b </> "claude-home"
      backendConfig = backendHome </> ".claude.json"
      mcpPath = bMcp b </> "lean-lsp.mcp.json"
      project = bProject b
      leanRoot = projectLeanRoot project
  mapM_ (createDirectoryIfMissing True) [backendHome, bMcp b]
  server <- mcpServerPayload (bRunner b) leanRoot
  writeMcpConfig mcpPath (bRunner b) leanRoot
  pluginRoot <- syncPrewarmedPlugin m (bRealHome b) backendHome >>= \case
    Just p -> pure p
    Nothing -> installManagedPlugin exe backendHome env (bAssets b </> "lean4-skills") (lpPlugin (bPaths b))
  keepOnlyManagedPlugin backendHome pluginId
  let skills = pluginRoot </> "skills" </> "lean4"
  stageClaudeCredentials m (bRealHome b) backendHome (capEnv auth) (capCopyOauth auth) (capCopyKey auth)
    (Just (KM.fromList [("lean-lsp", server)]))
  ctx <- writeStartupContext m (bStartup b) "claude-code" (projectRoot project) leanRoot (bActive b) instruction ws
           pluginRoot mcpPath (Just backendConfig) (Just skills)
  let c = ManagedContext "claude-code" (bManaged b) (projectRoot project) leanRoot backendHome pluginRoot mcpPath
            (Just ctx) (bAssets b) (Just (projectManifestPath project)) (Just backendConfig) (Just skills) Nothing
      child =
        setAll [("FISHER_AUTOFORMALIZE_CONTEXT", ctx)]
          . setAll [ ("HOME", backendHome), ("CLAUDE_PLUGIN_ROOT", pluginRoot), ("LEAN4_PLUGIN_ROOT", pluginRoot)
                   , ("LEAN4_SCRIPTS", pluginRoot </> "lib" </> "scripts"), ("LEAN4_REFS", skills </> "references")
                   , ("FISHER_YOLO_MODE", "1") ]
          . setAll (baseChildEnv c (bRealHome b))
          . setAll [(T.unpack k, T.unpack v) | Just (k, v) <- [capEnv auth]]
          . (if capStrip auth then stripKeys claudeAuthEnvKeys else id)
          $ env
  ensureProjectToolPermissions (projectRoot project)
  prompt <- startupPrompt c ws
  pure ([T.pack exe, "--model", fromMaybe claudeModel (bModel b)] <> permissionArgs m <> maybe [] pure prompt, child, c)

codexRuntime :: LaunchInputs -> AuthMode -> Text -> WorkflowSpec -> Bundle -> IO ([Text], Env, ManagedContext)
codexRuntime li mode instruction ws b = do
  let m = liMachine li
      env = liBaseEnv li
      project = bProject b
      leanRoot = projectLeanRoot project
      lp = bPaths b
  exe <- codexExe env
  auth <- codexAuthPlan li mode (bRealHome b)
  let backendHome = bManaged b </> "codex-home"
      codexHome = backendHome </> ".codex"
      skills = backendHome </> ".agents" </> "skills" </> "lean4"
      cfgPath = codexHome </> "config.toml"
      instructions = codexHome </> "fisher-autoformalize-instructions.md"
  mapM_ (createDirectoryIfMissing True) [backendHome, codexHome, backendHome </> ".agents" </> "skills"]
  stageTree (lpSkill lp) skills (bRevision b)
  stageCodexAuth codexHome (xapSource auth) (xapKey auth)
  ctx <- writeStartupContext m (bStartup b) "codex" (projectRoot project) leanRoot (bActive b) instruction ws
           (lpPlugin lp) cfgPath (Just cfgPath) (Just skills)
  writeCodexInstructions instructions (Just ctx) (projectRoot project) leanRoot (bActive b) skills (lpPlugin lp)
    (lpScripts lp) (skills </> "references") (Just ws)
  writeCodexConfig cfgPath instructions (bRunner b) leanRoot
  let c = ManagedContext "codex" (bManaged b) (projectRoot project) leanRoot backendHome (lpPlugin lp) cfgPath
            (Just ctx) (bAssets b) (Just (projectManifestPath project)) (Just cfgPath) (Just skills) (Just instructions)
      child =
        setAll [("FISHER_AUTOFORMALIZE_INSTRUCTIONS", instructions), ("FISHER_AUTOFORMALIZE_CONTEXT", ctx)]
          . setAll [ ("HOME", backendHome), ("CODEX_HOME", codexHome), ("LEAN4_PLUGIN_ROOT", lpPlugin lp)
                   , ("LEAN4_SCRIPTS", lpScripts lp), ("LEAN4_REFS", skills </> "references")
                   , ("FISHER_AUTOFORMALIZE_SKILLS_ROOT", skills) ]
          . setAll (baseChildEnv c (bRealHome b))
          . Map.delete "OPENAI_API_KEY"
          $ env
  prompt <- startupPrompt c ws
  pure ([T.pack exe] <> modelArgs (bModel b) <> ["--dangerously-bypass-approvals-and-sandbox"] <> maybe [] pure prompt, child, c)

-- | Fisher's native backend: the workflow runs on @fisher chat@, this
-- executable's own agent loop, with what the claude-code backend gets: the
-- lean4 skill and the workflow's command doc (inlined in the system
-- prompt, where Claude Code would expand the slash command), the Lean MCP
-- server and the same Opus ('nativeProvider': OpenRouter by default, or
-- Anthropic with Claude credentials staged under a managed HOME). The
-- tools are the @fisher-lean@ toolset: files, terminal, web search and the
-- MCP server's.
fisherRuntime :: LaunchInputs -> AuthMode -> Text -> WorkflowSpec -> Bundle -> IO ([Text], Env, ManagedContext)
fisherRuntime li mode instruction ws b = do
  let m = liMachine li
      env = liBaseEnv li
      project = bProject b
      leanRoot = projectLeanRoot project
      lp = bPaths b
  exe <- getExecutablePath
  (provider, model0) <- either' ConfigError (nativeProvider (liConfig li) (envList env))
  let model = fromMaybe model0 (bModel b)
      mcpPath = bMcp b </> "lean-lsp.mcp.json"
  writeMcpConfig mcpPath (bRunner b) leanRoot
  authEnv <- nativeAuthEnv li mode provider (bManaged b) (bRealHome b) True
  ctx <- writeStartupContext m (bStartup b) "fisher" (projectRoot project) leanRoot (bActive b) instruction ws
           (lpPlugin lp) mcpPath Nothing (Just (lpSkill lp))
  systemPrompt <- fisherSystemPrompt lp ws
  let promptPath = ctx <> ".system.md"
  TIO.writeFile promptPath systemPrompt
  let home = if provider == "anthropic" then bManaged b </> "fisher-home" else bRealHome b
      c = ManagedContext "fisher" (bManaged b) (projectRoot project) leanRoot home (lpPlugin lp) mcpPath
            (Just ctx) (bAssets b) (Just (projectManifestPath project)) Nothing (Just (lpSkill lp)) (Just promptPath)
      child =
        setAll [("FISHER_AUTOFORMALIZE_CONTEXT", ctx), ("FISHER_AUTOFORMALIZE_INSTRUCTIONS", promptPath)]
          . setAll [ ("CLAUDE_PLUGIN_ROOT", lpPlugin lp), ("LEAN4_PLUGIN_ROOT", lpPlugin lp), ("LEAN4_SCRIPTS", lpScripts lp)
                   , ("LEAN4_REFS", lpReferences lp), ("FISHER_YOLO_MODE", "1") ]
          . setAll (baseChildEnv c (bRealHome b))
          . authEnv
          $ env
      prompt =
        "You are in a Fisher-managed Lean workflow session. Read the startup context at " <> shlexQuote (T.pack ctx)
          <> " first. Then run this workflow inside the active project as your first workflow action: " <> wsBackend ws
          <> ". Its command and the lean4 skill are in your system prompt."
  pure ( [ T.pack exe, "chat", "--provider", provider, "-m", model, "-t", "fisher-lean"
         , "--system-prompt-file", T.pack promptPath, "--mcp-config", T.pack mcpPath, "-q", prompt ]
       , child, c )

-- | The native backend's credentials: for anthropic, Claude credentials
-- staged under a managed HOME, as for claude-code (and, for Lean, the real
-- elan); for openrouter, the key from the environment or the Fisher home's .env.
nativeAuthEnv :: LaunchInputs -> AuthMode -> Text -> FilePath -> FilePath -> Bool -> IO (Env -> Env)
nativeAuthEnv li mode provider managed realHome lean = do
  let m = liMachine li
      env = liBaseEnv li
  case provider of
    "anthropic" -> do
      auth <- claudeAuthPlan li mode realHome
      let backendHome = managed </> "fisher-home"
      createDirectoryIfMissing True backendHome
      stageClaudeCredentials m realHome backendHome (capEnv auth) (capCopyOauth auth) (capCopyKey auth) Nothing
      elan <-
        if not lean
          then pure []
          else let e = realHome </> ".elan" in doesDirectoryExist e >>= \ok -> pure [("ELAN_HOME", e) | ok, not (Map.member "ELAN_HOME" env)]
      pure ( setAll [("HOME", backendHome), ("FISHER_HOME", mFisherHome m)] . setAll elan
               . setAll [(T.unpack k, T.unpack v) | Just (k, v) <- [capEnv auth]]
               . (if capStrip auth then stripKeys claudeAuthEnvKeys else id) )
    _ -> do
      inEnv <- pure (maybe False (not . null) (Map.lookup "OPENROUTER_API_KEY" env))
      -- only checked here: the child reads the Fisher home's .env itself
      persisted <- liGetPersisted li "OPENROUTER_API_KEY"
      unless (inEnv || maybe False (not . T.null . T.strip) persisted) $
        throwW (PreflightError ("OpenRouter auth not found. Save `OPENROUTER_API_KEY` in " <> T.pack (mFisherHome m </> ".env") <> ", or set `fisher.autoformalize.native_provider: anthropic`."))
      pure (setAll [("FISHER_HOME", mFisherHome m)])

-- | The native backend's system prompt: how this session maps the
-- skill's Claude Code terms, then SKILL.md and the command doc, front
-- matter dropped.
fisherSystemPrompt :: LeanPaths -> WorkflowSpec -> IO Text
fisherSystemPrompt lp ws = do
  let skillPath = lpSkill lp </> "SKILL.md"
      commandPath = lpPlugin lp </> "commands" </> (T.unpack (wsKind ws) <> ".md")
      body t = case T.lines t of
        ("---" : rest) -> T.unlines (drop 1 (dropWhile (/= "---") rest))
        ls -> T.unlines ls
      readMaybe' p = doesFileExist p >>= \ok -> if ok then Just . body <$> TIO.readFile p else pure Nothing
  skill <- readMaybe' skillPath
  command <- readMaybe' commandPath
  let t = T.pack
  pure . T.unlines $
    [ "# Fisher native Lean workflow session"
    , ""
    , "You run a lean4-skills workflow on Fisher's own agent. This is how its Claude Code terms map here:"
    , ""
    , "- The Lean tools are the lean-lsp MCP server's, named `mcp_lean_lsp_<tool>` here: `lean_goal` is `mcp_lean_lsp_lean_goal`, `lean_diagnostic_messages` is `mcp_lean_lsp_lean_diagnostic_messages`, and so on."
    , "- Bash is the `terminal` tool; Read, Write and Edit are `read_file`, `write_file` and `patch`; Grep and Glob are `search_files`."
    , "- There are no slash commands, hooks or subagents (no Task tool): where the docs offer a fallback for other hosts, use it, and do deep or delegated steps yourself."
    , "- Relative links in the skill resolve under `" <> t (lpSkill lp) <> "` (references in `" <> t (lpReferences lp) <> "`), and in the command under `" <> t (lpPlugin lp </> "commands") <> "`. Read them with `read_file` when a step points to them."
    , "- `$LEAN4_SCRIPTS`, `$LEAN4_REFS` and `$LEAN4_PLUGIN_ROOT` are set in the terminal."
    , "" ]
    <> maybe [] (\s -> ["## The lean4 skill (SKILL.md)", "", s]) skill
    <> maybe [] (\c -> ["## The `/lean4:" <> wsKind ws <> "` command", "", "Invocation: `" <> wsBackend ws <> "`", "", c]) command

-- | A managed workflow command, staged and resolved. A Lean project goes
-- the way upstream's does, check for check; a Rocq or Isabelle project
-- (from its manifest, or @--prover@) needs neither git, rg nor the lean4
-- checkout, but its prover and uv.
resolveWorkflowRequest :: LaunchInputs -> Text -> IO (Either WorkflowError WorkflowPlan)
resolveWorkflowRequest li command = try $ do
  ws0 <- either throwW pure (parseWorkflowCommand command)
  (override, ws1) <- either throwW pure (stripProverFlag ws0)
  (modelFlag, ws2) <- either throwW pure (stripModelFlag ws1)
  let ws = dropArgSeparator ws2
  let env = liBaseEnv li
      cfg = liConfig li
      model = resolveLaunchModel modelFlag cfg
  active <- activeDir li
  backend <- either' ConfigError (resolveBackend cfg (envList env))
  requested <- either throwW pure (requestedModeOf cfg)
  mode <- either' ConfigError (resolveAuthMode cfg (envList env))
  -- which prover, before Lean's checks: a project that cannot be loaded
  -- goes the Lean way, which reports it where upstream does
  peek <- try (discoverProject active) :: IO (Either SomeException Project)
  let kind = fromMaybe (either (const Lean4) projectProver peek) override
  (argv, child, ctx, project, ws', cwd) <-
    if kind == Lean4
      then leanLaunch li ws override active backend mode model
      else proverLaunch li kind ws active backend mode model
  let wid = T.dropWhile (== '/') (wsCanonical ws')
  req <- buildHandoffRequest (liMachine li) argv cwd child requested ("Fisher " <> wid <> " session") ("fisher:" <> wid)
  pure (WorkflowPlan req ctx (wsArgs ws') project ws')

discover :: FilePath -> IO Project
discover active =
  try (discoverProject active) >>= \case
    Right p -> pure p
    Left (ProjectNotFound msg) -> throwW (PreflightError (msg <> " Run `/project init`, `/project convert`, or `/project use <path>` first."))
    Left e -> throwW (PreflightError (projectErrorMessage e))

-- | The project as @--prover@ asks for it: the nearest root of that kind
-- at or above the active directory, when the manifest is of another kind.
selectProver :: Maybe ProverKind -> FilePath -> Project -> IO Project
selectProver Nothing _ p = pure p
selectProver (Just k) active p
  | projectProver p == k = pure p
  | otherwise =
      findProverRoot k active >>= \case
        Just r -> pure p {projectProver = k, projectProverRoot = r, projectLeanRoot = r, projectKind = pvName (prover k)}
        Nothing ->
          throwW . PreflightError $
            "--prover " <> pvName (prover k) <> ": no " <> proverTitle k <> " project root at or above " <> T.pack active <> "."

-- | The child's argv, environment and context, the project and command it
-- runs, and the directory it starts in.
type Launch = ([Text], Env, ManagedContext, Project, WorkflowSpec, FilePath)

leanLaunch :: LaunchInputs -> WorkflowSpec -> Maybe ProverKind -> FilePath -> Backend -> AuthMode -> Maybe Text -> IO Launch
leanLaunch li ws override active backend mode model = do
  let env = liBaseEnv li
  git <- requireExecutable "git" "Git is required to stage the managed Lean workflow assets." env
  runner <- resolveUvRunner (envList env) >>= either' PreflightError
  _ <- requireExecutable "rg" "ripgrep (`rg`) is required for Lean local search in managed workflows. Install it and try again." env
  project <- discover active >>= selectProver override active
  home <- realHomeOf env
  base <- managedStateBase li
  managed <- managedRoot li base backend
  let assets = base </> "assets"
      startup = managed </> "startup"
      mcp = managed </> "mcp"
  mapM_ (createDirectoryIfMissing True) [managed, assets, startup, mcp]
  (co, rev) <- ensureLean4Checkout assets env git False
  let b = Bundle managed assets startup mcp project active home (leanPaths co) runner rev backend model
  (argv, child, ctx) <- case backend of
    ClaudeCode -> claudeRuntime li mode (wsArgs ws) ws b
    Codex -> codexRuntime li mode (wsArgs ws) ws b
    Fisher -> fisherRuntime li mode (wsArgs ws) ws b
  pure (argv, child, ctx, project, ws, active)

-- | What a Rocq or Isabelle launch stages from.
data ProverBundle = ProverBundle
  { pbKind :: ProverKind
  , pbManaged :: FilePath
  , pbAssets :: FilePath
  , pbStartup :: FilePath
  , pbMcp :: FilePath
  , pbProject :: Project
  , pbActive :: FilePath
  , pbRealHome :: FilePath
  , pbCommand :: [Text]
  -- ^ the MCP server's command, uvx resolved
  , pbCoqc :: Maybe RocqCoqc
  -- ^ the coqc wrapper Rocq-MCP runs, when only rocq is on PATH
  , pbFisher :: FilePath
  -- ^ this Fisher's executable, for @fisher check@
  , pbModel :: Maybe Text
  -- ^ the chosen model, Nothing for the backend's default
  }

proverLaunch :: LaunchInputs -> ProverKind -> WorkflowSpec -> FilePath -> Backend -> AuthMode -> Maybe Text -> IO Launch
proverLaunch li kind ws0 active backend mode model = do
  let env = liBaseEnv li
  project <- discover active >>= selectProver (Just kind) active
  ws <- either throwW pure (resolveCommandFor kind ws0)
  offPath <- checkProverBinaries kind env
  forM_ offPath $ \d ->
    mNote (liMachine li) ("Isabelle is not on PATH; this session uses " <> T.pack (d </> "isabelle") <> ".")
  home <- realHomeOf env
  base <- managedStateBase li
  managed <- managedRoot li base backend
  let assets = base </> "assets"
      startup = managed </> "startup"
      mcp = managed </> "mcp"
  mapM_ (createDirectoryIfMissing True) [managed, assets, startup, mcp]
  -- Isabelle-MCP starts through Fisher's shim, staged in a directory of
  -- its own; the prewarm and every client run the same command
  shim <- if kind == Isabelle then Just <$> stageIsabelleShim (managed </> "isabelle-mcp") else pure Nothing
  cmd <- either' PreflightError (proverServerCommand kind (envList env) shim) >>= withUvFallback env
  prewarmProverServer (mNote (liMachine li)) kind env cmd
  coqc <- if kind == Rocq then rocqServerCoqc env (managed </> "rocq-bin") else pure Nothing
  self <- getExecutablePath
  let pb = ProverBundle kind managed assets startup mcp project active home cmd coqc self model
  (argv, child0, ctx) <- case backend of
    ClaudeCode -> proverClaudeRuntime li mode ws pb
    Codex -> proverCodexRuntime li mode ws pb
    Fisher -> proverFisherRuntime li mode ws pb
  -- the skills run `fisher check`: a directory holding only a link to
  -- this Fisher comes first on the child's PATH, whatever fisher (if any)
  -- the PATH holds, and nothing else of the directory Fisher lives in;
  -- an Isabelle found off PATH comes next, for Isabelle-MCP
  fisherBin <- stageFisherBin (managed </> "fisher-bin") self
  let child = prependPath (fisherBin : maybe [] pure offPath) child0
  -- Claude Code starts its MCP servers in its own directory and gives that
  -- as the client root, which is where Isabelle-MCP takes its project root
  -- from: the session starts in the prover root. Codex's and Fisher's
  -- server entries carry the root as their cwd instead.
  let cwd = if backend == ClaudeCode then projectProverRoot project else active
  pure (argv, child, ctx, project, ws, cwd)

-- | These directories first on the environment's PATH.
prependPath :: [FilePath] -> Env -> Env
prependPath dirs e =
  let old = Map.findWithDefault "" "PATH" e
      new = foldr (\d acc -> if null acc then d else d <> [searchPathSeparator] <> acc) old dirs
   in Map.insert "PATH" new e

-- | 'baseChildEnv' with the prover's project variables in place of Lean's.
proverChildEnv :: ProverKind -> ManagedContext -> FilePath -> Maybe RocqCoqc -> [(String, String)]
proverChildEnv kind c home coqc =
  filter ((/= "LEAN_PROJECT_PATH") . fst) (baseChildEnv c home)
    <> [(T.unpack k, T.unpack v) | (k, v) <- proverProjectEnv kind (mcLeanRoot c) coqc]

proverClaudeRuntime :: LaunchInputs -> AuthMode -> WorkflowSpec -> ProverBundle -> IO ([Text], Env, ManagedContext)
proverClaudeRuntime li mode ws pb = do
  let m = liMachine li
      env = liBaseEnv li
      kind = pbKind pb
      name = pvName (prover kind)
      server = svName (pvServer (prover kind))
      project = pbProject pb
      root = projectProverRoot project
  exe <- claudeExe env
  auth <- claudeAuthPlan li mode (pbRealHome pb)
  let backendHome = pbManaged pb </> "claude-home"
      backendConfig = backendHome </> ".claude.json"
      mcpPath = pbMcp pb </> (T.unpack server <> ".mcp.json")
      entry = clientServerPayload kind (pbCommand pb) root (pbCoqc pb)
  mapM_ (createDirectoryIfMissing True) [backendHome, pbMcp pb]
  writeMcpConfigFor kind mcpPath entry
  market <- stageProverSkills kind (pbAssets pb)
  pluginRoot <- installLocalPlugin (proverTitle kind) exe backendHome env market (market </> T.unpack name)
  keepOnlyManagedPlugin backendHome (proverPluginId kind)
  let skills = pluginRoot </> "skills" </> T.unpack name
      guide = pluginRoot </> "commands" </> (T.unpack (wsKind ws) <> ".md")
  stageClaudeCredentials m (pbRealHome pb) backendHome (capEnv auth) (capCopyOauth auth) (capCopyKey auth)
    (Just (KM.fromList [(K.fromText server, entry)]))
  ctx <- writeProverStartupContext m (pbStartup pb) kind "claude-code" (projectRoot project) root (pbActive pb) (wsArgs ws) ws
           (skills </> "SKILL.md") guide mcpPath (pbFisher pb)
  let c = ManagedContext "claude-code" (pbManaged pb) (projectRoot project) root backendHome pluginRoot mcpPath
            (Just ctx) (pbAssets pb) (Just (projectManifestPath project)) (Just backendConfig) (Just skills) Nothing
      child =
        setAll [("FISHER_AUTOFORMALIZE_CONTEXT", ctx)]
          . setAll [("HOME", backendHome), ("CLAUDE_PLUGIN_ROOT", pluginRoot), ("FISHER_YOLO_MODE", "1")]
          -- Claude Code's wait for an MCP server to start, in milliseconds
          . setAll [("MCP_TIMEOUT", show (proverConnectTimeout * 1000))]
          . setAll (proverChildEnv kind c (pbRealHome pb) (pbCoqc pb))
          . setAll [(T.unpack k, T.unpack v) | Just (k, v) <- [capEnv auth]]
          . (if capStrip auth then stripKeys claudeAuthEnvKeys else id)
          $ env
  ensureProjectToolPermissions (projectRoot project)
  let prompt = proverStartupPrompt kind "claude-code" ctx ws (Just skills)
  pure ([T.pack exe, "--model", fromMaybe claudeModel (pbModel pb)] <> permissionArgs m <> [prompt], child, c)

proverCodexRuntime :: LaunchInputs -> AuthMode -> WorkflowSpec -> ProverBundle -> IO ([Text], Env, ManagedContext)
proverCodexRuntime li mode ws pb = do
  let m = liMachine li
      env = liBaseEnv li
      kind = pbKind pb
      name = pvName (prover kind)
      project = pbProject pb
      root = projectProverRoot project
  exe <- codexExe env
  auth <- codexAuthPlan li mode (pbRealHome pb)
  let backendHome = pbManaged pb </> "codex-home"
      codexHome = backendHome </> ".codex"
      skills = backendHome </> ".agents" </> "skills" </> T.unpack name
      cfgPath = codexHome </> "config.toml"
      instructions = codexHome </> "fisher-autoformalize-instructions.md"
      guide = skills </> "commands" </> (T.unpack (wsKind ws) <> ".md")
  mapM_ (createDirectoryIfMissing True) [backendHome, codexHome, backendHome </> ".agents" </> "skills"]
  stageCodexProverSkill kind skills
  stageCodexAuth codexHome (xapSource auth) (xapKey auth)
  ctx <- writeProverStartupContext m (pbStartup pb) kind "codex" (projectRoot project) root (pbActive pb) (wsArgs ws) ws
           (skills </> "SKILL.md") guide cfgPath (pbFisher pb)
  writeProverCodexInstructions instructions kind ctx (projectRoot project) root (pbActive pb) skills ws
  writeCodexConfigFor kind cfgPath instructions (pbCommand pb) root (pbCoqc pb)
  let c = ManagedContext "codex" (pbManaged pb) (projectRoot project) root backendHome skills cfgPath
            (Just ctx) (pbAssets pb) (Just (projectManifestPath project)) (Just cfgPath) (Just skills) (Just instructions)
      child =
        setAll [("FISHER_AUTOFORMALIZE_INSTRUCTIONS", instructions), ("FISHER_AUTOFORMALIZE_CONTEXT", ctx)]
          . setAll [("HOME", backendHome), ("CODEX_HOME", codexHome), ("FISHER_AUTOFORMALIZE_SKILLS_ROOT", skills)]
          . setAll (proverChildEnv kind c (pbRealHome pb) (pbCoqc pb))
          . Map.delete "OPENAI_API_KEY"
          $ env
      prompt = proverStartupPrompt kind "codex" ctx ws (Just skills)
  pure ([T.pack exe] <> modelArgs (pbModel pb) <> ["--dangerously-bypass-approvals-and-sandbox", prompt], child, c)

-- | Fisher's native backend in a Rocq or Isabelle project: @fisher chat@
-- with the prover's toolset (@fisher-rocq@ or @fisher-isabelle@), the
-- prover's server in its MCP config (in the prover root, the coqc tools
-- alone when @pet@ is not on PATH, the configured tool timeout) and the
-- embedded skill and command in the system prompt.
proverFisherRuntime :: LaunchInputs -> AuthMode -> WorkflowSpec -> ProverBundle -> IO ([Text], Env, ManagedContext)
proverFisherRuntime li mode ws pb = do
  let m = liMachine li
      env = liBaseEnv li
      kind = pbKind pb
      name = pvName (prover kind)
      server = svName (pvServer (prover kind))
      project = pbProject pb
      root = projectProverRoot project
  exe <- getExecutablePath
  (provider, model0) <- either' ConfigError (nativeProvider (liConfig li) (envList env))
  let model = fromMaybe model0 (pbModel pb)
  pet <- isJust <$> whichPath env "pet"
  let mcpPath = pbMcp pb </> (T.unpack server <> ".mcp.json")
      entry = serverPayloadFor kind (pbCommand pb) root (pbCoqc pb) (pvToolsFor kind pet) (proverToolTimeoutFrom (liConfig li))
  createDirectoryIfMissing True (pbMcp pb)
  writeMcpConfigFor kind mcpPath entry
  authEnv <- nativeAuthEnv li mode provider (pbManaged pb) (pbRealHome pb) False
  market <- stageProverSkills kind (pbAssets pb)
  let pluginRoot = market </> T.unpack name
      skills = pluginRoot </> "skills" </> T.unpack name
      guide = pluginRoot </> "commands" </> (T.unpack (wsKind ws) <> ".md")
  ctx <- writeProverStartupContext m (pbStartup pb) kind "fisher" (projectRoot project) root (pbActive pb) (wsArgs ws) ws
           (skills </> "SKILL.md") guide mcpPath (pbFisher pb)
  let promptPath = ctx <> ".system.md"
  TIO.writeFile promptPath (proverSystemPrompt kind ws pluginRoot pet)
  let home = if provider == "anthropic" then pbManaged pb </> "fisher-home" else pbRealHome pb
      c = ManagedContext "fisher" (pbManaged pb) (projectRoot project) root home pluginRoot mcpPath
            (Just ctx) (pbAssets pb) (Just (projectManifestPath project)) Nothing (Just skills) (Just promptPath)
      child =
        setAll [("FISHER_AUTOFORMALIZE_CONTEXT", ctx), ("FISHER_AUTOFORMALIZE_INSTRUCTIONS", promptPath)]
          . setAll [("FISHER_YOLO_MODE", "1")]
          . setAll (proverChildEnv kind c (pbRealHome pb) (pbCoqc pb))
          . authEnv
          $ env
      prompt =
        "You are in a Fisher-managed " <> proverTitle kind <> " workflow session. Read the startup context at "
          <> shlexQuote (T.pack ctx) <> " first. Then run this workflow inside the active project as your first workflow action: "
          <> wsBackend ws <> ". Its command and the " <> name <> " skill are in your system prompt."
  pure ( [ T.pack exe, "chat", "--provider", provider, "-m", model, "-t", nativeToolset kind
         , "--system-prompt-file", T.pack promptPath, "--mcp-config", T.pack mcpPath, "-q", prompt ]
       , child, c )

-- | Fisher only: a planned child's argv (its prompt last) in the form that
-- runs to the end without a terminal, for @fisher launch --print@: the
-- native @fisher chat -p@ in place of @-q@, @claude -p@, @codex exec@.
printArgv :: Text -> [Text] -> Either Text [Text]
printArgv backend argv
  | null argv = Left "No headless form for an empty argv."
  | otherwise =
      let front = init argv
          prompt = last argv
       in case backend of
            "fisher" -> case reverse front of
              ("-q" : rest) -> Right (reverse rest <> ["-p", prompt])
              _ -> Right (front <> ["-p", prompt])
            "claude-code" -> Right (front <> ["-p", prompt])
            "codex" -> case argv of
              (exe : rest) -> Right (exe : "exec" : rest)
              [] -> Left "No headless form for an empty argv."
            other -> Left ("No headless form for the " <> other <> " backend.")

-- | @_prepare_managed_chat_lean_assets@: the checkout if it can be had,
-- without failing the chat when it cannot.
chatLeanAssets :: FilePath -> Env -> IO (Maybe (FilePath, LeanPaths, Text))
chatLeanAssets base env = do
  let assets = base </> "assets"
      co = checkoutRoot assets
  createDirectoryIfMissing True assets
  whichPath env "git" >>= \case
    Nothing -> do
      complete <- checkoutIsComplete co
      rev <- readCheckoutRevision co
      pure $ case rev of
        Just r | complete -> Just (co, leanPaths co, r)
        _ -> Nothing
    Just git -> do
      r <- try (ensureLean4Checkout assets env git False)
      pure $ case r of
        Right (c, rev) -> Just (c, leanPaths c, rev)
        Left (StagingError _) -> Nothing
        Left _ -> Nothing

chatPrompt :: Text -> FilePath -> Text -> Text
chatPrompt backend cwd instruction =
  let label = if backend == "codex" then "Codex" else "Claude Code"
   in T.unwords $
        [ "You are " <> label <> " in a Fisher-managed interactive chat session."
        , "The user launched `/chat` from the main Fisher CLI and will return there when this session exits."
        , "Current working directory: " <> T.pack cwd <> "."
        , "Use this session for onboarding, planning, repository questions, and general discussion before a specific Lean workflow is selected."
        , "Use any skills and MCP tools that are already configured in this backend session when they are helpful."
        , "If the user is ready to work in Lean, tell them to return to the main Fisher session and use `/project init`, `/project use`, or `/project create`, then `/prove`, `/review`, `/draft`, `/autoprove`, `/formalize`, or `/autoformalize`." ]
        <> [if T.null (T.strip instruction) then "Start by asking what the user wants to do in Fisher." else "Initial user request: " <> T.strip instruction]

-- | @/chat@ in a managed backend session.
resolveChatRequest :: LaunchInputs -> Text -> IO (Either WorkflowError ChatPlan)
resolveChatRequest li instruction0 = try $ do
  let instruction = T.strip instruction0
      env = liBaseEnv li
      cfg = liConfig li
      m = liMachine li
  active <- activeDir li
  backend <- either' ConfigError (resolveBackend cfg (envList env))
  requested <- either throwW pure (requestedModeOf cfg)
  mode <- either' ConfigError (resolveAuthMode cfg (envList env))
  base <- managedStateBase li
  home <- realHomeOf env
  let chatRoot = base </> T.unpack (backendName backend) </> "chat"
      common = [ ("FISHER_MANAGED_CHAT", "1"), ("FISHER_MANAGED_CHAT_BACKEND", T.unpack (backendName backend))
               , ("FISHER_CHAT_CWD", active), ("FISHER_MANAGED_STATE_DIR", chatRoot), ("FISHER_REAL_HOME", home) ]
      prompt = chatPrompt (backendName backend) active instruction
  (argv, child) <- case backend of
    ClaudeCode -> do
      exe <- claudeExe env
      auth <- claudeAuthPlan li mode home
      let backendHome = chatRoot </> "claude-home"
      assets <- chatLeanAssets base env
      createDirectoryIfMissing True backendHome
      synced <- syncPrewarmedPlugin m home backendHome
      plugin <- case (synced, assets) of
        (Just p, _) -> pure (Just p)
        (Nothing, Just (co, lp, _)) -> Just <$> installManagedPlugin exe backendHome env co (lpPlugin lp)
        _ -> pure Nothing
      stageClaudeCredentials m home backendHome (capEnv auth) (capCopyOauth auth) (capCopyKey auth) Nothing
      let pluginVars p = [ ("CLAUDE_PLUGIN_ROOT", p), ("LEAN4_PLUGIN_ROOT", p), ("LEAN4_SCRIPTS", p </> "lib" </> "scripts")
                         , ("LEAN4_REFS", p </> "skills" </> "lean4" </> "references") ]
          child =
            setAll (maybe [] pluginVars plugin)
              . setAll (common <> [("HOME", backendHome), ("FISHER_YOLO_MODE", "1")])
              . setAll [(T.unpack k, T.unpack v) | Just (k, v) <- [capEnv auth]]
              . (if capStrip auth then stripKeys claudeAuthEnvKeys else id)
              $ env
      pure ([T.pack exe, prompt], child)
    Codex -> do
      exe <- codexExe env
      auth <- codexAuthPlan li mode home
      let backendHome = chatRoot </> "codex-home"
          codexHome = backendHome </> ".codex"
      assets <- chatLeanAssets base env
      source <- codexHomeOf home env
      mapM_ (createDirectoryIfMissing True) [backendHome, codexHome]
      _ <- stageOptionalFileCopy (source </> "config.toml") (codexHome </> "config.toml")
      _ <- stageOptionalTreeCopy (source </> "skills") (codexHome </> "skills")
      _ <- stageOptionalTreeCopy (source </> ".tmp" </> "plugins") (codexHome </> ".tmp" </> "plugins")
      _ <- stageOptionalTreeCopy (home </> ".agents" </> "skills") (backendHome </> ".agents" </> "skills")
      _ <- stageOptionalTreeCopy (home </> ".agents" </> "plugins") (backendHome </> ".agents" </> "plugins")
      let staged = backendHome </> ".agents" </> "skills" </> "lean4"
      forM_ assets $ \(_, lp, rev) -> do
        stageTree (lpSkill lp) staged rev
        stageTree (lpSkill lp) (codexHome </> "skills" </> "lean4") rev
      stageCodexAuth codexHome (xapSource auth) (xapKey auth)
      let child =
            setAll (if isJust assets then [("FISHER_AUTOFORMALIZE_SKILLS_ROOT", staged), ("LEAN4_REFS", staged </> "references")] else [])
              . setAll (concat [[("LEAN4_PLUGIN_ROOT", lpPlugin lp), ("LEAN4_SCRIPTS", lpScripts lp)] | Just (_, lp, _) <- [assets]])
              . setAll (common <> [("HOME", backendHome), ("CODEX_HOME", codexHome), ("FISHER_YOLO_MODE", "1")])
              . Map.delete "OPENAI_API_KEY"
              $ env
      pure ([T.pack exe, "--dangerously-bypass-approvals-and-sandbox", prompt], child)
    Fisher ->
      throwW . ConfigError $
        "The fisher backend runs the Lean workflows (/prove, /autoprove, ...); for a chat, talk to Fisher itself."
  req <- buildHandoffRequest m argv active child requested "Fisher chat session" "fisher:chat"
  pure (ChatPlan req (backendName backend) instruction active)

-- | @prepare_managed_runtime_assets@: fetch the checkout, warm the Lean MCP
-- runtime, and make the user's Claude profile carry the plugin.
prepareManagedRuntimeAssets :: LaunchInputs -> IO (Either WorkflowError [(Text, Text)])
prepareManagedRuntimeAssets li = try $ do
  let env = liBaseEnv li
  base <- managedStateBase li
  let assets = base </> "assets"
  createDirectoryIfMissing True assets
  git <- requireExecutable "git" "Git is required to prewarm the managed Lean workflow assets." env
  runner <- resolveUvRunner (envList env) >>= either' PreflightError
  (co, rev) <- ensureLean4Checkout assets env git True
  _ <- runStaging (map T.unpack runner <> ["--help"]) "Failed to prewarm the managed Lean MCP runtime" Nothing Nothing
  claude <- whichPath env "claude"
  pluginRoot <- case claude of
    Nothing -> pure ""
    Just exe -> do
      home <- realHomeOf env
      T.pack <$> ensureUserPluginState (liMachine li) exe home env
  pure
    [ ("lean4_checkout_root", T.pack co), ("skill_revision", rev)
    , ("lean_lsp_mcp_spec", leanLspMcpSpec (envList env)), ("claude_plugin_root", pluginRoot) ]

