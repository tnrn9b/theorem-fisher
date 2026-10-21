-- | @fisher doctor@, ported from upstream @cli/doctor.py@ for the sections that
-- concern Fisher: configuration files, the Fisher home, external
-- tools, and the managed Lean workflows. Fisher adds the Rocq and Isabelle
-- sections (the prover, coq-lsp's pet, the Isabelle-MCP registration, uvx and
-- Python) for the project in reach, or for every prover with @--all@.
-- Upstream's Python, package,
-- provider-portal, API, submodule, tool and skills-hub sections check
-- machinery Fisher does not have yet.
module Fisher.Doctor
  ( DoctorEnv (..)
  , checkManagedWorkflows
  , checkProvers
  , runDoctor
  , runDoctorTo
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Data.Aeson (Value)
import Data.Char (isDigit)
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, getCurrentDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Fisher.ClaudeAuth (ClaudeAuthStatus (..), claudeAuthStatusWithEnv)
import Fisher.Config (saveConfigAt, defaultConfig)
import Fisher.Paths (configPathIn, envPathIn, expandUser, fisherHomeLabelWith, homeLabel)
import Fisher.Project
import Fisher.Prover
import Fisher.Term
import Fisher.Which (whichIn)
import Fisher.Workflow.LocalAuth
import Fisher.Workflow.Plan (nativeProvider)
import Fisher.Workflow.Settings

data DoctorEnv = DoctorEnv
  { deStyle :: Style
  , deConfig :: Value
  , deEnv :: [(String, String)]
  -- ^ The environment the managed checks read: PATH, HOME and overrides.
  , deActiveCwd :: Maybe FilePath
  , deGetPersisted :: Text -> IO (Maybe Text)
  -- ^ @get_env_value@: the process environment, then the Fisher home's .env.
  , deKeychain :: IO (Maybe Text)
  , deCliName :: Text
  , deClaudeLogin :: Maybe (FilePath -> IO Bool)
  -- ^ With @--fix@ on a terminal: ask (after the Claude auth line it is
  -- about) whether to log in to Claude Code now and, on yes, run @fisher
  -- login --provider claude-code@ with the @claude@ given. True when it ran
  -- the login. Nothing: only report.
  }

newtype Out = Out { outLine :: Text -> IO () }

ok, warn, failed :: Style -> Out -> Text -> Text -> IO ()
ok st o = mark st o "✓" green
warn st o = mark st o "⚠" yellow
failed st o = mark st o "✗" red

mark :: Style -> Out -> Text -> Colour -> Text -> Text -> IO ()
mark st o sym c text detail =
  outLine o ("  " <> color st [c] sym <> " " <> text <> (if T.null detail then "" else " " <> color st [dim] detail))

info :: Style -> Out -> Text -> IO ()
info st o text = outLine o ("    " <> color st [cyan] "→" <> " " <> text)

section :: Style -> Out -> Text -> IO ()
section st o title = outLine o "" >> outLine o (color st [cyan, bold] ("◆ " <> title))

paren :: Text -> Text
paren t = "(" <> t <> ")"

-- | @_check_managed_workflow_requirements@.
checkManagedWorkflows :: DoctorEnv -> (Text -> IO ()) -> (Text -> IO ()) -> IO ()
checkManagedWorkflows de line issue = checkManagedWorkflowsWith de line issue (pure ())

-- | 'checkManagedWorkflows', with what to do for each issue fixed.
--
-- The Claude auth check asks the Claude Code CLI first (@claude auth status
-- --json@): logged in is enough; logged out names @fisher login --provider
-- claude-code@ (and, with 'deClaudeLogin', offers to run it). When the CLI
-- cannot say (not installed, an old CLI, unreadable output), the check
-- reads the local login, API key and staged variables as before.
checkManagedWorkflowsWith :: DoctorEnv -> (Text -> IO ()) -> (Text -> IO ()) -> IO () -> IO ()
checkManagedWorkflowsWith de line issue fixed = do
  let st = deStyle de
      o = Out line
      env = deEnv de
      cfg = deConfig de
      path = maybe "" id (lookup "PATH" env)
  section st o "Managed Lean Workflows"
  case (,) <$> resolveBackend cfg env <*> resolveAuthMode cfg env of
    Left e -> do
      failed st o "Managed workflow config" (paren e)
      issue ("Fix fisher.autoformalize config: " <> e)
    Right (backend, auth) -> do
      ok st o "Managed backend" (paren (backendName backend))
      ok st o "Managed auth mode" (paren (authModeName auth))
      let template = resolveTemplateSource cfg env
      if T.null template
        then warn st o "Project template source" "(`/project create` takes `--template NAME` (built-ins lean, lean-mathlib) or `--template-source` or `fisher.project.template_source`)"
        else ok st o "Project template source" (paren template)
      resolveUvRunner env >>= \case
        Right [_, "lean-mcp"] -> ok st o "Lean MCP server" "(fisher lean-mcp)"
        Right (runner : _) -> ok st o "uv / uvx" (paren (T.pack (takeFileName (T.unpack runner))))
        Right [] -> pure ()
        Left e -> do
          failed st o "uv / uvx" (paren e)
          issue "Install uv so Fisher can launch the managed Lean MCP server"
      inReach <- proverInReach de
      whichIn path "lake" >>= \case
        Just p -> ok st o "Lean toolchain (lake)" (paren (T.pack p))
        Nothing
          | Just _ <- inReach -> warn st o "Lean toolchain (lake)" "(needed for Lean projects only)"
          | otherwise -> do
              failed st o "Lean toolchain (lake)" "(required for managed Lean workflows)"
              issue "Install elan / Lean so `lake` is available on PATH"

      activeDir <- activeDirOf de
      exists <- doesPathExist activeDir
      if exists
        then do
          ok st o "Active working directory" (paren (T.pack activeDir))
          r <- tryProject (discoverProject activeDir)
          case r of
            Right p -> ok st o "Active Fisher project" (paren (projectLabel p))
            Left (ProjectNotFound _) -> do
              warn st o "Active Fisher project" (paren ("none under " <> T.pack activeDir))
              issue "Create or select a Fisher project with `/project init`, `/project convert`, or `/project use <path>`"
            Left (ProjectManifestInvalid m) -> do
              failed st o "Fisher project manifest" (paren m)
              issue ("Repair the active Fisher project manifest: " <> m)
            Left _ -> pure ()
        else do
          failed st o "Active working directory" (paren (T.pack activeDir <> " does not exist"))
          issue ("Fix the managed workflow working directory: " <> T.pack activeDir)

      realHome <- maybe (pure "~") pure (lookup "HOME" env) >>= expandUser >>= canonicalizePath
      let -- claude is found on the doctor's PATH and run in its environment
          cliStatusOf = claudeAuthStatusWithEnv env
          claudeAuthChecks = case auth of
            -- API-key mode does not use the login: no status run
            AuthApiKey -> localClaudeAuthChecks
            _ -> do
              claudeExe <- whichIn path "claude"
              cliStatus <- maybe (pure (Left "not installed")) cliStatusOf claudeExe
              case (claudeExe, cliStatus) of
                (_, Right s) | casLoggedIn s -> okClaudeLogin s
                (Just exe, Right _) -> loggedOutClaudeChecks exe
                _ -> localClaudeAuthChecks
          okClaudeLogin s = ok st o "Claude auth" (paren ("Claude Code login" <> maybe "" (": " <>) (casEmail s)))
          loginHint = "`fisher login --provider claude-code`"
          -- the CLI says logged out: an API key or staged variable still
          -- serves auto mode; else the line, then (--fix on a terminal) the
          -- offer to log in, which fixes it once the status says so
          loggedOutClaudeChecks exe = do
            apiKey <- hasLocalClaudeApiKey realHome
            staged <- resolveClaudeAuthEnv (deGetPersisted de) env True
            let offer = case deClaudeLogin de of
                  Nothing -> pure Nothing
                  Just ask ->
                    ask exe >>= \case
                      False -> pure Nothing
                      True -> either (const Nothing) (\s -> if casLoggedIn s then Just s else Nothing) <$> cliStatusOf exe
                unlessLoggedInNow k = offer >>= \case
                  Just s -> okClaudeLogin s >> fixed
                  Nothing -> k
            case auth of
              AuthAuto
                | apiKey -> ok st o "Claude auth" "(local Claude API key)"
                | Just (k, _) <- staged -> ok st o "Claude auth" (paren ("staged via " <> k))
                | otherwise -> do
                    failed st o "Claude auth" "(Claude Code is not logged in)"
                    unlessLoggedInNow $
                      issue ("Run " <> loginHint <> ", save `ANTHROPIC_API_KEY`, or set `fisher.autoformalize.auth_mode: login`")
              _ -> do
                warn st o "Claude auth" ("(Claude Code is not logged in: run " <> loginHint <> ", or login mode will prompt on first launch)")
                unlessLoggedInNow (pure ())
          localClaudeAuthChecks = do
            login <- hasLocalClaudeLogin (deKeychain de) realHome
            apiKey <- hasLocalClaudeApiKey realHome
            staged <- resolveClaudeAuthEnv (deGetPersisted de) env True
            let stagedKeys = maybe "" fst staged
            case auth of
              AuthAuto
                | login -> ok st o "Claude auth" "(local Claude login)"
                | apiKey -> ok st o "Claude auth" "(local Claude API key)"
                | Just _ <- staged -> ok st o "Claude auth" (paren ("staged via " <> stagedKeys))
                | otherwise -> do
                    failed st o "Claude auth" "(not found)"
                    issue "Run `claude auth login`, save `ANTHROPIC_API_KEY`, or set `fisher.autoformalize.auth_mode: login`"
              AuthLogin
                | login -> ok st o "Claude auth" "(local Claude login)"
                | apiKey -> warn st o "Claude auth" "(login mode ignores local Claude API keys; first launch will prompt)"
                | otherwise -> warn st o "Claude auth" "(login mode will prompt on first launch)"
              AuthApiKey
                | Just _ <- staged -> ok st o "Claude API-key auth" (paren ("staged via " <> stagedKeys))
                | apiKey -> ok st o "Claude API-key auth" "(local Claude API key)"
                | otherwise -> do
                    failed st o "Claude API-key auth" "(not found)"
                    issue "Save `ANTHROPIC_API_KEY` / `ANTHROPIC_TOKEN` / `CLAUDE_CODE_OAUTH_TOKEN`, or switch `fisher.autoformalize.auth_mode` back to `auto` or `login`"
      case backend of
        ClaudeCode -> do
          whichIn path "claude" >>= \case
            Just p -> ok st o "Claude Code CLI" (paren (T.pack p))
            Nothing -> do
              failed st o "Claude Code CLI" "(install with `npm install -g @anthropic-ai/claude-code`)"
              issue "Install the Claude Code CLI for managed Lean workflows"
          claudeAuthChecks
        Fisher -> case nativeProvider cfg env of
          Left e -> do
            failed st o "Native agent provider" (paren e)
            issue ("Fix fisher.autoformalize config: " <> e)
          Right ("anthropic", model) -> do
            ok st o "Native agent" (paren ("fisher chat, anthropic " <> model))
            claudeAuthChecks
          Right (provider, model) -> do
            ok st o "Native agent" (paren ("fisher chat, " <> provider <> " " <> model))
            key <- case lookup "OPENROUTER_API_KEY" env of
              Just v | not (null v) -> pure True
              _ -> maybe False (not . T.null . T.strip) <$> deGetPersisted de "OPENROUTER_API_KEY"
            if key
              then ok st o "OpenRouter auth" "(OPENROUTER_API_KEY)"
              else do
                failed st o "OpenRouter auth" "(not found)"
                label <- T.pack <$> fisherHomeLabelWith (pure . flip lookup (deEnv de))
                issue ("Save `OPENROUTER_API_KEY` in " <> label <> "/.env, or set `fisher.autoformalize.native_provider: anthropic`")
        Codex -> do
          whichIn path "codex" >>= \case
            Just p -> ok st o "Codex CLI" (paren (T.pack p))
            Nothing -> do
              failed st o "Codex CLI" "(install the OpenAI Codex CLI)"
              issue "Install the OpenAI Codex CLI for managed Lean workflows"
          payload <- loadLocalCodexAuthPayload realHome env
          let localAuth = codexAuthPayloadIsValid payload
              localKey = codexAuthPayloadHasApiKey payload
          staged <- resolveCodexApiKey (deGetPersisted de) env True
          case auth of
            AuthAuto
              | localAuth -> ok st o "Codex auth" "(local Codex auth)"
              | not (T.null staged) -> ok st o "Codex auth" "(staged OPENAI_API_KEY)"
              | otherwise -> do
                  failed st o "Codex auth" "(not found)"
                  issue "Run `codex login`, save `OPENAI_API_KEY`, or set `fisher.autoformalize.auth_mode: login`"
            AuthLogin
              | localAuth -> ok st o "Codex auth" "(local Codex auth)"
              | otherwise -> warn st o "Codex auth" "(login mode will prompt on first launch)"
            AuthApiKey
              | not (T.null staged) -> ok st o "Codex API-key auth" "(staged OPENAI_API_KEY)"
              | localKey -> ok st o "Codex API-key auth" "(local Codex API key)"
              | otherwise -> do
                  failed st o "Codex API-key auth" "(not found)"
                  issue "Save `OPENAI_API_KEY`, or switch `fisher.autoformalize.auth_mode` back to `auto` or `login`"
  where
    tryProject :: IO Project -> IO (Either ProjectError Project)
    tryProject = try

-- | The directory the managed checks start from: the given one, else
-- @TERMINAL_CWD@, else this process's.
activeDirOf :: DoctorEnv -> IO FilePath
activeDirOf de = do
  cwd0 <- case deActiveCwd de of
    Just c -> pure c
    Nothing -> maybe getCurrentDirectory pure (nonEmpty (lookup "TERMINAL_CWD" (deEnv de)))
  expandUser cwd0 >>= canonicalizePath
  where
    nonEmpty = \case
      Just s | not (null s) -> Just s
      _ -> Nothing

-- | The Rocq or Isabelle project at or above the active directory, if any.
-- Lean projects, ties and unreadable directories give 'Nothing'.
proverInReach :: DoctorEnv -> IO (Maybe ProverKind)
proverInReach de =
  try @SomeException (activeDirOf de >>= detectProver) >>= \case
    Right (Right (Just (k, _))) | k /= Lean4 -> pure (Just k)
    _ -> pure Nothing

-- | The Rocq and Isabelle sections: for the project in reach, or for both
-- with @allProvers@ (@--all@). Each failing line ends with its fix, which
-- also goes to the issues.
checkProvers :: Bool -> DoctorEnv -> (Text -> IO ()) -> (Text -> IO ()) -> IO ()
checkProvers allProvers de line issue = do
  kinds <- if allProvers then pure [Rocq, Isabelle] else maybe [] pure <$> proverInReach de
  forM_ kinds $ \k -> do
    section st o ("Managed " <> proverTitle k <> " Workflows")
    case k of
      Rocq -> checkRocq
      Isabelle -> checkIsabelle
      Lean4 -> pure ()
    checkUvx k
    checkPython k
  where
    st = deStyle de
    o = Out line
    path = fromMaybe "" (lookup "PATH" (deEnv de))
    withFix fix why = why <> "; fix: " <> fix
    bad k label why fix = do
      failed st o label (withFix fix why)
      issue (proverTitle k <> ": " <> fix)

    -- what Rocq-MCP will run, as the launch finds it: coqc, else rocq
    -- through the coqc wrapper the launch stages
    checkRocq = do
      let fix = binaryFix Rocq "coqc"
      findRocqCompiler (deEnv de) >>= \case
        Nothing -> bad Rocq "Rocq" "(rocq or coqc not found)" fix
        Just (name, bin) -> do
          v <- versionWord <$> runQuiet de bin (versionArgs Rocq name)
          let label = "Rocq (" <> name <> ")"
              how = if name == "rocq" then "(Rocq-MCP runs it through a coqc wrapper)" else ""
          case v of
            Nothing -> warn st o label "(could not read its version)"
            Just ver
              | numeric ver `versionBelow` [8, 17] -> bad Rocq (label <> " " <> ver) "(below 8.17, not supported)" fix
              | numeric ver `versionBelow` [9, 0] -> warn st o (label <> " " <> ver) (withFix fix "(below 9.0: the interactive tools need Rocq 9)")
              | otherwise -> ok st o (label <> " " <> ver) how
      whichIn path "pet" >>= \case
        Just _ -> ok st o "coq-lsp pet" ""
        Nothing -> warn st o "coq-lsp pet not found" (withFix fix "(interactive tools unavailable)")

    checkIsabelle = do
      let installFix = binaryFix Isabelle "isabelle"
      findIsabelle (deEnv de) >>= \case
        Nothing -> bad Isabelle "Isabelle" "(not found on PATH, under ISABELLE_HOME or in Applications)" installFix
        Just (bin, onPath) -> do
          let dir = takeDirectory (takeDirectory bin)
          v <- isabelleVersion <$> runQuiet de bin ["version"]
          let label = "Isabelle " <> fromMaybe "(version unknown)" v
              there = T.pack dir
          unless onPath $ do
            let fix = "add " <> there <> "/bin to PATH"
            warn st o label (withFix fix ("(at " <> there <> ", not on PATH)"))
            issue ("Isabelle: " <> fix)
          case v of
            Just "2025-2" -> when onPath (ok st o label "")
            Just _ -> warn st o label (withFix installFix "(Fisher expects Isabelle2025-2)")
            Nothing -> when onPath (warn st o label "(could not read its version)")
          listed <- runQuiet de bin ["components", "-l"]
          let registered = any (\l -> any (`T.isInfixOf` l) ["isabelle_mcp", "isabelle-mcp"]) (T.lines (fromMaybe "" listed))
              regFix = binaryFix Isabelle "isabelle-mcp" <> (if onPath then "" else " --isabelle-bin " <> T.pack bin)
          if registered
            then ok st o "Isabelle-MCP registered" ""
            else bad Isabelle "Isabelle-MCP not registered" "(isabelle components -l does not list it)" regFix

    checkUvx k =
      ((<|>) <$> whichIn path "uvx" <*> whichIn path "uv") >>= \case
        Just p
          | takeFileName p == "uv" -> ok st o "uvx" "(uv)"
          | otherwise -> ok st o "uvx" ""
        Nothing -> bad k "uvx" "(not found)" (binaryFix k "uvx")

    checkPython k = do
      let (server, need) = case k of
            Isabelle -> ("Isabelle-MCP", [3, 12])
            _ -> ("Rocq-MCP", [3, 11])
          fix = binaryFix k "python3"
          needText = T.intercalate "." (map (T.pack . show) need)
      found <- (<|>) <$> whichIn path "python3" <*> whichIn path "python"
      -- uvx brings its own Python, so with uv there the system one only
      -- warns
      uv <- (<|>) <$> whichIn path "uvx" <*> whichIn path "uv"
      let ownPython = isJust uv
      case found of
        Nothing
          | ownPython -> ok st o "Python" "(none on PATH; uvx brings its own)"
          | otherwise -> bad k "Python" "(python3 not found)" fix
        Just bin -> do
          v <- versionWord <$> runQuiet de bin (versionArgs k "python3")
          case v of
            Nothing -> warn st o "Python" "(could not read its version)"
            Just ver
              | numeric ver `versionBelow` need, ownPython ->
                  warn st o ("Python " <> ver) ("(" <> server <> " needs " <> needText <> " or later; uvx brings its own)")
              | numeric ver `versionBelow` need -> bad k ("Python " <> ver) ("(" <> server <> " needs " <> needText <> " or later)") fix
              | otherwise -> ok st o ("Python " <> ver) ""

-- | The arguments that print a binary's version, from 'pvBinaries'.
versionArgs :: ProverKind -> Text -> [String]
versionArgs k name =
  map T.unpack (maybe ["--version"] binVersionArgs (find ((== name) . binName) (pvBinaries (prover k))))

-- | A binary's standard output and error, in the doctor's environment,
-- or 'Nothing' when it cannot run or takes over a minute.
runQuiet :: DoctorEnv -> FilePath -> [String] -> IO (Maybe Text)
runQuiet de bin args = do
  r <- timeout 60000000 (try @SomeException (readCreateProcessWithExitCode (proc bin args) {env = Just (deEnv de)} ""))
  pure $ case r of
    Just (Right (_, out, err)) -> Just (T.pack (out <> err))
    _ -> Nothing

-- | The first word that starts with a digit: @9.0.0@ in @The Rocq Prover,
-- version 9.0.0@, @3.12.4@ in @Python 3.12.4@.
versionWord :: Maybe Text -> Maybe Text
versionWord out = do
  t <- out
  w <- find (maybe False (isDigit . fst) . T.uncons) (T.words t)
  pure (T.dropWhileEnd (`elem` [',', ';', ':']) w)

-- | @2025-2@ from @Isabelle2025-2@ (the first word of @isabelle version@).
isabelleVersion :: Maybe Text -> Maybe Text
isabelleVersion out = do
  t <- out
  w <- case T.words t of
    (w : _) -> Just w
    [] -> Nothing
  let v = T.dropWhileEnd (== ':') (fromMaybe w (T.stripPrefix "Isabelle" w))
  if T.null v then Nothing else Just v

-- | The leading numbers of a version: @[8, 18, 0]@ from @8.18.0@ (and from
-- @8.18+rc1@ the numbers before the suffix).
numeric :: Text -> [Int]
numeric = mapMaybe (readMaybe . T.unpack . T.takeWhile isDigit) . takeWhile startsDigit . T.splitOn "."
  where
    startsDigit = maybe False (isDigit . fst) . T.uncons

providerEnvHints :: [Text]
providerEnvHints =
  [ "OPENROUTER_API_KEY", "OPENAI_API_KEY", "ANTHROPIC_API_KEY", "ANTHROPIC_TOKEN", "OPENAI_BASE_URL"
  , "GLM_API_KEY", "ZAI_API_KEY", "Z_AI_API_KEY", "KIMI_API_KEY", "MINIMAX_API_KEY", "MINIMAX_CN_API_KEY" ]

readTextLenient :: FilePath -> IO Text
readTextLenient p = either (const "") id <$> (try (TIO.readFile p) :: IO (Either SomeException Text))

-- | The whole doctor run. @fix@ creates what can be created; @allProvers@
-- checks every prover, not only the one the project in reach uses. Returns
-- the lines to print, in order.
runDoctor :: Bool -> Bool -> DoctorEnv -> FilePath -> IO [Text]
runDoctor fix allProvers de home = do
  linesRef <- newIORef []
  runDoctorTo (\t -> modifyIORef' linesRef (<> [t])) fix allProvers de home
  readIORef linesRef

-- | 'runDoctor', each line given to the sink as it comes (so that a
-- question 'deClaudeLogin' asks comes after the lines before it).
runDoctorTo :: (Text -> IO ()) -> Bool -> Bool -> DoctorEnv -> FilePath -> IO ()
runDoctorTo say fix allProvers de home = do
  issuesRef <- newIORef []
  fixedRef <- newIORef (0 :: Int)
  let st = deStyle de
      issue t = modifyIORef' issuesRef (<> [t])
      fixed = modifyIORef' fixedRef (+ 1)
      o = Out say
      cli = deCliName de
      title = "Fisher Doctor"
      -- the home in use, as hints name it ('homeLabel')
      h = T.pack (homeLabel (fromMaybe "" (lookup "HOME" (deEnv de))) home)
      padL = (55 - T.length title) `div` 2
      padR = 55 - T.length title - padL

  say ""
  say (color st [cyan] ("┌" <> T.replicate 57 "─" <> "┐"))
  say (color st [cyan] ("│ " <> T.replicate padL " " <> title <> T.replicate padR " " <> " │"))
  say (color st [cyan] ("└" <> T.replicate 57 "─" <> "┘"))

  section st o "Configuration Files"
  let envPath = envPathIn home
  envExists <- doesPathExist envPath
  if envExists
    then do
      ok st o (h <> "/.env file exists") ""
      content <- readTextLenient envPath
      if any (`T.isInfixOf` content) providerEnvHints
        then ok st o "API key or custom endpoint configured" ""
        else do
          warn st o ("No API key found in " <> h <> "/.env") ""
          issue ("Run '" <> cli <> " setup' to configure API keys")
    else do
      failed st o (h <> "/.env file missing") ""
      if fix
        then do
          createDirectoryIfMissing True home
          TIO.writeFile envPath ""
          ok st o ("Created empty " <> h <> "/.env") ""
          info st o ("Run '" <> cli <> " setup' to configure API keys")
          fixed
        else do
          info st o ("Run '" <> cli <> " setup' to create one")
          issue ("Run '" <> cli <> " setup' to create .env")
  cfgExists <- doesPathExist (configPathIn home)
  if cfgExists
    then ok st o (h <> "/config.yaml exists") ""
    else
      if fix
        then do
          saveConfigAt home defaultConfig
          ok st o ("Created " <> h <> "/config.yaml from defaults") ""
          fixed
        else warn st o "config.yaml not found" "(using defaults)"

  section st o "Directory Structure"
  homeExists <- doesDirectoryExist home
  if homeExists
    then ok st o (h <> " directory exists") ""
    else
      if fix
        then createDirectoryIfMissing True home >> ok st o ("Created " <> h <> " directory") "" >> fixed
        else warn st o (h <> " not found") "(will be created on first use)"
  forM_ ["cron", "sessions", "logs", "skills", "memories"] $ \sub -> do
    present <- doesPathExist (home </> T.unpack sub)
    if present
      then ok st o (h <> "/" <> sub <> "/ exists") ""
      else
        if fix
          then do
            createDirectoryIfMissing True (home </> T.unpack sub)
            ok st o ("Created " <> h <> "/" <> sub <> "/") ""
            fixed
          else warn st o (h <> "/" <> sub <> "/ not found") "(will be created on first use)"
  let soul = home </> "SOUL.md"
  soulExists <- doesPathExist soul
  if soulExists
    then do
      content <- T.strip <$> readTextLenient soul
      let real = [l | l <- T.lines content, not (T.null (T.strip l))
                    , not (any (`T.isPrefixOf` T.strip l) ["<!--", "-->", "#"])]
      if null real
        then info st o (h <> "/SOUL.md exists but is empty — edit it to customize personality")
        else ok st o (h <> "/SOUL.md exists (persona configured)") ""
    else do
      warn st o (h <> "/SOUL.md not found") "(create it to give Fisher a custom voice)"
      when fix $ do
        createDirectoryIfMissing True home
        TIO.writeFile soul
          "# Fisher Voice\n\n<!-- Edit this file to customize how Fisher communicates. -->\n\nYou are Fisher. Be direct, useful, and brief about your identity.\n"
        ok st o ("Created " <> h <> "/SOUL.md with basic template") ""
        fixed
  let memories = home </> "memories"
  memExists <- doesDirectoryExist memories
  if memExists
    then do
      ok st o (h <> "/memories/ directory exists") ""
      forM_ ["MEMORY.md", "USER.md"] $ \f -> do
        present <- doesPathExist (memories </> T.unpack f)
        if present
          then do
            size <- T.length . T.strip <$> readTextLenient (memories </> T.unpack f)
            ok st o (f <> " exists (" <> T.pack (show size) <> " chars)") ""
          else info st o (f <> " not created yet (will be created when the agent first writes a memory)")
    else do
      warn st o (h <> "/memories/ not found") "(will be created on first use)"
      when fix $ do
        createDirectoryIfMissing True memories
        ok st o ("Created " <> h <> "/memories/") ""
        fixed
  stateDb <- doesPathExist (home </> "state.db")
  if stateDb
    then info st o (h <> "/state.db exists (this build does not read session counts yet)")
    else info st o (h <> "/state.db not created yet (will be created on first session)")

  section st o "External Tools"
  let pathEnv = maybe "" id (lookup "PATH" (deEnv de))
  whichIn pathEnv "git" >>= \case
    Just _ -> ok st o "git" ""
    Nothing -> warn st o "git not found" "(optional)"
  whichIn pathEnv "rg" >>= \case
    Just _ -> ok st o "ripgrep (rg)" "(faster file search)"
    Nothing -> do
      warn st o "ripgrep (rg) not found" "(file search uses grep fallback)"
      info st o "Install for faster search: sudo apt install ripgrep"

  checkManagedWorkflowsWith de say issue fixed
  checkProvers allProvers de say issue

  say ""
  remaining <- readIORef issuesRef
  fixedCount <- readIORef fixedRef
  let numbered = [ "  " <> T.pack (show i) <> ". " <> t | (i, t) <- zip [1 :: Int ..] remaining ]
      n = T.pack (show (length remaining))
  if fix && fixedCount > 0
    then do
      say (color st [green] (T.replicate 60 "─"))
      let first = color st [green, bold] ("  Fixed " <> T.pack (show fixedCount) <> " issue(s).")
      say (if null remaining then first else first <> color st [yellow, bold] (" " <> n <> " issue(s) require manual intervention."))
      say ""
      unless (null remaining) (mapM_ say numbered >> say "")
    else
      if not (null remaining)
        then do
          say (color st [yellow] (T.replicate 60 "─"))
          say (color st [yellow, bold] ("  Found " <> n <> " issue(s) to address:"))
          say ""
          mapM_ say numbered
          say ""
          unless fix $ say (color st [dim] ("  Tip: run '" <> cli <> " doctor --fix' to auto-fix what's possible."))
        else do
          say (color st [green] (T.replicate 60 "─"))
          say (color st [green, bold] "  All checks passed! 🎉")
  say ""
