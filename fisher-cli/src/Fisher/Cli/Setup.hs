-- | @fisher setup [section]@: upstream's wizard (@cli/setup.py@,
-- @run_setup_wizard@ and every section it runs: model and provider,
-- terminal backend, tools, agent settings), its non-interactive guidance,
-- the returning user's menu, quick setup and the closing summary. The
-- login goes through the credential layer (@Fisher.LLM.Auth@), as upstream
-- reuses @_login_openai_codex@. Upstream's first provider (a portal login)
-- was removed, so the provider menu starts at its second.
--
-- Upstream's quirk is kept: choosing "Keep current" in the Codex model
-- menu asks for a model name.
module Fisher.Cli.Setup
  ( SetupInfo (..)
  , setupMain
  , setupMainWith
  , runTop
  ) where

import Control.Exception (AsyncException (..), SomeException, fromException, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

import Fisher.Cli.AuthArgs
import Fisher.Cli.Prompt
import Fisher.Cli.SetupCommon
import Fisher.Cli.SetupTools
import qualified Fisher.Config as Config
import Fisher.EnvVars (EnvVar (..), optionalEnvVars, requiredEnvVars)
import Fisher.LLM.Auth.Anthropic (isClaudeCodeTokenValid, readClaudeCodeCredentials, runOauthSetupToken)
import Fisher.LLM.Auth.Codex (defaultCodexOpts, getCodexModelIds, loginOpenAICodex, resolveCodexRuntimeCredentials)
import Fisher.LLM.Auth.Config (readRawConfig, updateConfigForProvider, writeRawConfig)
import Fisher.LLM.Auth.Env (loadDotenvIntoSys, saveAnthropicApiKey, saveAnthropicOauthToken, saveEnvValue)
import Fisher.LLM.Auth.Error
import Fisher.LLM.Auth.Registry
import Fisher.LLM.Auth.Resolve (detectExternalCredentials)
import Fisher.LLM.Auth.Store (deactivateProvider, getActiveProvider)
import Fisher.LLM.Auth.Util (pyFloatText)
import Fisher.LLM.Sys
import Fisher.Paths (configPathIn, envPathIn)
import Fisher.Py.Value
import Fisher.Term

-- | What upstream reads from its own install.
data SetupInfo = SetupInfo
  { siPython :: Text
  -- ^ @sys.executable@, for the pip installs of the Modal and Daytona SDKs
  , siInstallDir :: Text
  -- ^ @PROJECT_ROOT@
  }
  deriving (Eq, Show)

data Env = Env
  { eUI :: UI
  , eSys :: Sys
  , eInfo :: SetupInfo
  }

-- | @fisher setup ...@.
setupMain :: Sys -> [Text] -> IO Int
setupMain sys args = do
  exe <- getExecutablePath
  setupMainWith (SetupInfo "python3" (T.pack (takeDirectory exe))) sys args

setupMainWith :: SetupInfo -> Sys -> [Text] -> IO Int
setupMainWith info sys args = do
  loadDotenvIntoSys sys
  case parseArgs setupParser args of
    Left (msg, code) -> sysOut sys msg >> pure code
    Right opts -> do
      ui <- newUI sys
      runTop sys (runSetupWizard (Env ui sys info) opts >> pure 0)

-- | The process boundary: @sys.exit(n)@ is the exit code; anything else
-- escaping is a traceback, as Python prints one.
runTop :: Sys -> IO Int -> IO Int
runTop sys act =
  try act >>= \case
    Right n -> pure n
    Left (e :: SomeException)
      | Just (SystemExit n) <- fromException e -> pure n
      | Just UserInterrupt <- fromException e -> do
          outLn sys "Traceback (most recent call last):\nKeyboardInterrupt"
          pure 130
      | otherwise -> do
          let line = case fromException e of
                Just (PyError ty _ msg) -> ty <> ": " <> msg
                _ -> case fromException e of
                  Just (a :: AuthError) -> "fisher_cli.auth.AuthError: " <> aeMessage a
                  Nothing -> "Exception: " <> T.pack (show e)
          outLn sys ("Traceback (most recent call last):\n" <> line)
          pure 1

-- | Python's @try: ... except SystemExit / except Exception@.
data Outcome a = Ok a | Exited | Raised SomeException

tryPy :: IO a -> IO (Outcome a)
tryPy act =
  try act >>= \case
    Right x -> pure (Ok x)
    Left e
      | Just (SystemExit _) <- fromException e -> pure Exited
      | Just (_ :: AsyncException) <- fromException e -> throwIO e
      | otherwise -> pure (Raised e)

-- | @try: ... except Exception: pass@.
quietly :: IO () -> IO ()
quietly act =
  tryPy act >>= \case
    Exited -> throwIO (SystemExit 1)
    _ -> pure ()

-- ------------------------------------------------------------------ wizard

sections :: [(Text, Text)]
sections = [("model", "Model & Provider"), ("terminal", "Terminal Backend"), ("tools", "Tools"), ("agent", "Agent Settings")]

runSection :: Env -> IORef PyVal -> Bool -> Text -> IO ()
runSection e cref firstInstall = \case
  "model" -> setupModelProvider e cref
  "terminal" -> setupTerminalBackend e cref
  "tools" -> toolsCommand (ToolsEnv (eUI e) (T.unpack (siInstallDir (eInfo e)))) firstInstall cref
  "agent" -> setupAgentSettings e cref
  _ -> pure ()

-- | @run_setup_wizard(args)@.
runSetupWizard :: Env -> Parsed -> IO ()
runSetupWizard e opts = do
  let ui = eUI e
      sys = eSys e
      home = sysFisherHome sys
  cref <- newIORef =<< loadCfg sys
  tty <- sysStdinTty sys
  if flagSet "non-interactive" opts || not tty
    then noninteractiveGuidance ui
    else case optValue "_" opts of
      Just section -> case lookup section sections of
        Just label -> do
          sayLn ui ""
          sayLn ui (paint ui [magenta] "┌─────────────────────────────────────────────────────────┐")
          sayLn ui (paint ui [magenta] ("│     ⚕ Fisher Setup — " <> T.justifyLeft 33 ' ' label <> " │"))
          sayLn ui (paint ui [magenta] "└─────────────────────────────────────────────────────────┘")
          runSection e cref False section
          readIORef cref >>= saveCfg sys
          sayLn ui ""
          printSuccess ui (label <> " configuration complete!")
        Nothing -> do
          printError ui ("Unknown setup section: " <> section)
          printInfo ui ("Available sections: " <> T.intercalate ", " (map fst sections))
      Nothing -> do
        active <- getActiveProvider sys
        force <- maybe False ((`elem` ["1", "true", "yes", "on"]) . T.toLower . T.strip) <$> sysGetEnv sys "FISHER_FORCE_FIRST_TIME_SETUP"
        orKey <- envValue sys "OPENROUTER_API_KEY"
        baseUrl <- envValue sys "OPENAI_BASE_URL"
        let isExisting = not force && (not (T.null orKey) || not (T.null baseUrl) || active /= PNull)
        sayLn ui ""
        mapM_ (sayLn ui . paint ui [magenta])
          [ "┌─────────────────────────────────────────────────────────┐"
          , "│             ⚕ Fisher Agent Setup Wizard               │"
          , "├─────────────────────────────────────────────────────────┤"
          , "│  Let's configure your Fisher Agent installation.      │"
          , "│  Press Ctrl+C at any time to exit.                     │"
          , "└─────────────────────────────────────────────────────────┘" ]
        proceed <-
          if isExisting
            then do
              sayLn ui ""
              printHeader ui "Welcome Back!"
              printSuccess ui "You already have Fisher configured."
              sayLn ui ""
              choice <- promptChoice ui "What would you like to do?"
                [ "Quick Setup - configure missing items only", "Full Setup - reconfigure everything", "---"
                , "Model & Provider", "Terminal Backend", "Tools", "Agent Settings", "---", "Exit" ] 0
              case choice of
                0 -> runQuickSetup e cref >> pure False
                1 -> pure True
                _ | choice `elem` [2, 7, 8] -> do
                      printInfo ui ("Exiting. Run '" <> cliName <> " setup' again when ready.")
                      pure False
                  | choice >= 3 && choice <= 6 -> do
                      runSection e cref False (fst (sections !! (choice - 3)))
                      readIORef cref >>= saveCfg sys
                      printSetupSummary e cref
                      pure False
                  | otherwise -> pure True
            else do
              sayLn ui ""
              printInfo ui "We'll walk you through:"
              printInfo ui "  1. Model & Provider — choose your AI provider and model"
              printInfo ui "  2. Terminal Backend — where your agent runs commands"
              printInfo ui "  3. Tools — configure web search, image generation, etc."
              printInfo ui "  4. Agent Settings — iterations, compression, session reset"
              sayLn ui ""
              printInfo ui "Press Enter to begin, or Ctrl+C to exit."
              readLine ui (paint ui [yellow] "  Press Enter to start... ") >>= \case
                Typed _ -> pure True
                _ -> sayLn ui "" >> pure False
        when proceed $ do
          printHeader ui "Configuration Location"
          printInfo ui ("Config file:  " <> T.pack (configPathIn home))
          printInfo ui ("Secrets file: " <> T.pack (envPathIn home))
          printInfo ui ("Data folder:  " <> T.pack home)
          printInfo ui ("Install dir:  " <> siInstallDir (eInfo e))
          sayLn ui ""
          printInfo ui ("You can edit these files directly or use '" <> cliName <> " config edit'")
          setupModelProvider e cref
          setupTerminalBackend e cref
          runSection e cref (not isExisting) "tools"
          setupAgentSettings e cref
          readIORef cref >>= saveCfg sys
          printSetupSummary e cref

noninteractiveGuidance :: UI -> IO ()
noninteractiveGuidance ui = do
  sayLn ui ""
  sayLn ui (paint ui [cyan, bold] "⚕ Fisher Setup — Non-interactive mode")
  sayLn ui ""
  printInfo ui "Running in a non-interactive environment (no TTY detected)."
  printInfo ui "The interactive wizard cannot be used here."
  sayLn ui ""
  printInfo ui "Configure Fisher using environment variables or config commands:"
  printInfo ui ("  " <> cliName <> " config set model.provider custom")
  printInfo ui ("  " <> cliName <> " config set model.base_url http://localhost:8080/v1")
  printInfo ui ("  " <> cliName <> " config set model.default your-model-name")
  sayLn ui ""
  printInfo ui "Or set OPENROUTER_API_KEY / OPENAI_API_KEY in your environment."
  printInfo ui ("Run '" <> cliName <> " setup' in an interactive terminal to use the full wizard.")
  sayLn ui ""

-- ------------------------------------------------------------- quick setup

-- | @_run_quick_setup(config, home)@.
runQuickSetup :: Env -> IORef PyVal -> IO ()
runQuickSetup e cref = do
  let ui = eUI e
      sys = eSys e
  sayLn ui ""
  printHeader ui "Quick Setup — Missing Items Only"
  let missingOf vars = fmap concat . mapM (\v -> (\x -> [v | T.null x]) <$> envValue sys (evName v)) $ vars
  missingRequired <- missingOf requiredEnvVars
  missingOptional <- missingOf optionalEnvVars
  cfg <- loadCfg sys
  let defaults = fromAesonCfg
      missingConfig = missingFields defaults cfg ""
      currentVer = fromMaybe 0 (case get "_config_version" cfg of Just (PInt n) -> Just n; _ -> Nothing)
      latestVer = toInteger Config.latestConfigVersion
      anything = not (null missingRequired) || not (null missingOptional) || not (null missingConfig) || currentVer < latestVer
      desc v = if T.null (evDescription v) then evName v else evDescription v
  if not anything
    then do
      printSuccess ui "Everything is configured! Nothing to do."
      sayLn ui ""
      printInfo ui ("Run '" <> cliName <> " setup' and choose 'Full Setup' to reconfigure,")
      printInfo ui "or pick a specific section from the menu."
    else do
      unless (null missingRequired) $ do
        sayLn ui ""
        printInfo ui (showT (length missingRequired) <> " required setting(s) missing:")
        forM_ missingRequired $ \v -> sayLn ui ("     • " <> evName v)
        sayLn ui ""
        forM_ missingRequired $ \v -> do
          sayLn ui ""
          sayLn ui (paint ui [cyan] ("  " <> evName v))
          printInfo ui ("  " <> evDescription v)
          forM_ (evUrl v) $ \u -> unless (T.null u) $ printInfo ui ("  Get key at: " <> u)
          value <- prompt ui ("  " <> evPrompt v) Nothing (evPassword v)
          if not (T.null value)
            then saveEnvValue sys (evName v) value >> printSuccess ui ("  Saved " <> evName v)
            else printWarning ui ("  Skipped " <> evName v)
      let missingTools = [v | v <- missingOptional, evCategory v == "tool"]
      unless (null missingTools) $ do
        sayLn ui ""
        printHeader ui "Tool API Keys"
        let labels =
              [ desc v <> (if null (evTools v) then "" else " → " <> T.intercalate ", " (take 2 (evTools v)))
              | v <- missingTools ]
        picked <- promptChecklist ui "Which tools would you like to configure?" labels []
        forM_ picked $ \i -> promptApiKey ui sys (missingTools !! i)
      unless (null missingConfig) $ do
        sayLn ui ""
        printInfo ui ("Adding " <> showT (length missingConfig) <> " new config option(s) with defaults...")
        forM_ missingConfig $ \(k, d) -> printSuccess ui ("  Added " <> k <> " = " <> pyStrOf d)
        modifyIORef cref (set "_config_version" (PInt latestVer))
        readIORef cref >>= saveCfg sys
      printSetupSummary e cref
  where
    fromAesonCfg = fromAeson Config.defaultConfig
    missingFields defaults current prefix = case defaults of
      PDict kvs ->
        concat
          [ if not (has k current)
              then [(full, dv)]
              else case (dv, get k current) of
                (PDict _, Just c@(PDict _)) -> missingFields dv c full
                _ -> []
          | (k, dv) <- kvs
          , not ("_" `T.isPrefixOf` k)
          , let full = if T.null prefix then k else prefix <> "." <> k ]
      _ -> []

-- | @_prompt_api_key(var)@.
promptApiKey :: UI -> Sys -> EnvVar -> IO ()
promptApiKey ui sys v = do
  let tools = evTools v
      toolsStr = T.intercalate ", " (take 3 tools) <> (if length tools > 3 then ", +" <> showT (length tools - 3) <> " more" else "")
      desc = if T.null (evDescription v) then evName v else evDescription v
  sayLn ui ""
  sayLn ui (paint ui [cyan] ("  ─── " <> desc <> " ───"))
  sayLn ui ""
  unless (T.null toolsStr) $ printInfo ui ("  Enables: " <> toolsStr)
  forM_ (evUrl v) $ \u -> unless (T.null u) $ printInfo ui ("  Get your key at: " <> u)
  sayLn ui ""
  value <- prompt ui ("  " <> evPrompt v) Nothing (evPassword v)
  if not (T.null value)
    then saveEnvValue sys (evName v) value >> printSuccess ui "  ✓ Saved"
    else printWarning ui ("  Skipped (configure later with '" <> cliName <> " setup')")

-- ----------------------------------------------------------------- summary

-- | @_print_setup_summary(config, home)@.
printSetupSummary :: Env -> IORef PyVal -> IO ()
printSetupSummary e cref = do
  let ui = eUI e
      sys = eSys e
      home = sysFisherHome sys
      has' k = not . T.null <$> envValue sys k
  config <- readIORef cref
  sayLn ui ""
  printHeader ui "Tool Availability Summary"
  vision <- either (const []) id <$> (tryPy (availableVisionBackends sys) >>= \case Ok r -> pure r; _ -> pure (Left ()))
  orKey <- has' "OPENROUTER_API_KEY"
  fire <- (||) <$> has' "FIRECRAWL_API_KEY" <*> has' "FIRECRAWL_API_URL"
  abWhich <- sysWhich sys "agent-browser"
  abLocal <- doesFileExist (T.unpack (siInstallDir (eInfo e)) </> "node_modules" </> ".bin" </> "agent-browser")
  bb <- has' "BROWSERBASE_API_KEY"
  fal <- has' "FAL_KEY"
  eleven <- has' "ELEVENLABS_API_KEY"
  voice <- has' "VOICE_TOOLS_OPENAI_KEY"
  tinker <- has' "TINKER_API_KEY"
  wandb <- has' "WANDB_API_KEY"
  hass <- has' "HASS_TOKEN"
  gh <- has' "GITHUB_TOKEN"
  let tts = case get "provider" (getD "tts" config) of
        Just v -> pyStrOf v
        Nothing -> "edge"
      status =
        [ if not (null vision) then ("Vision (image analysis)", True, "") else ("Vision (image analysis)", False, "run '" <> cliName <> " setup' to configure")
        , if orKey then ("Mixture of Agents", True, "") else ("Mixture of Agents", False, "OPENROUTER_API_KEY")
        , if fire then ("Web Search & Extract", True, "") else ("Web Search & Extract", False, "FIRECRAWL_API_KEY")
        , if bb
            then ("Browser Automation (Browserbase)", True, "")
            else if isJust abWhich || abLocal then ("Browser Automation (local)", True, "") else ("Browser Automation", False, "npm install -g agent-browser")
        , if fal then ("Image Generation", True, "") else ("Image Generation", False, "FAL_KEY")
        , if tts == "elevenlabs" && eleven
            then ("Text-to-Speech (ElevenLabs)", True, "")
            else if tts == "openai" && voice then ("Text-to-Speech (OpenAI)", True, "") else ("Text-to-Speech (Edge TTS)", True, "")
        , if tinker && wandb
            then ("RL Training (Tinker)", True, "")
            else if tinker then ("RL Training (Tinker)", False, "WANDB_API_KEY") else ("RL Training (Tinker)", False, "TINKER_API_KEY") ]
          <> [("Smart Home (Home Assistant)", True, "") | hass]
          <> [ if gh then ("Skills Hub (GitHub)", True, "") else ("Skills Hub (GitHub)", False, "GITHUB_TOKEN")
             , ("Terminal/Commands", True, ""), ("Task Planning (todo)", True, ""), ("Skills (view, create, edit)", True, "") ]
      available = length [() | (_, True, _) <- status]
  printInfo ui (showT available <> "/" <> showT (length status) <> " tool categories available:")
  sayLn ui ""
  forM_ status $ \(name, ok, var) ->
    if ok
      then sayLn ui ("   " <> paint ui [green] "✓" <> " " <> name)
      else sayLn ui ("   " <> paint ui [red] "✗" <> " " <> name <> " " <> paint ui [dim] ("(missing " <> var <> ")"))
  sayLn ui ""
  when (available < length status) $ do
    printWarning ui ("Some tools are disabled. Run '" <> cliName <> " setup tools' to configure them,")
    printWarning ui ("or edit " <> homeLabelOf sys <> "/.env directly to add the missing API keys.")
    sayLn ui ""
  sayLn ui ""
  sayLn ui (paint ui [green] "┌─────────────────────────────────────────────────────────┐")
  sayLn ui (paint ui [green] "│              ✓ Setup Complete!                          │")
  sayLn ui (paint ui [green] "└─────────────────────────────────────────────────────────┘")
  sayLn ui ""
  let cfgPath = T.pack (configPathIn home)
      envPath = T.pack (envPathIn home)
      g = paint ui [green]
  sayLn ui (paint ui [cyan, bold] ("📁 All your files are in " <> homeLabelOf sys <> "/:"))
  sayLn ui ""
  sayLn ui ("   " <> paint ui [yellow] "Settings:" <> "  " <> cfgPath)
  sayLn ui ("   " <> paint ui [yellow] "API Keys:" <> "  " <> envPath)
  sayLn ui ("   " <> paint ui [yellow] "Data:" <> "      " <> T.pack home <> "/cron/, sessions/, logs/")
  sayLn ui ""
  sayLn ui (paint ui [dim] (T.replicate 60 "─"))
  sayLn ui ""
  sayLn ui (paint ui [cyan, bold] "📝 To edit your configuration:")
  sayLn ui ""
  sayLn ui ("   " <> g "fisher setup" <> "          Re-run the full wizard")
  sayLn ui ("   " <> g "fisher setup model" <> "    Change model/provider")
  sayLn ui ("   " <> g "fisher setup terminal" <> " Change terminal backend")
  sayLn ui ("   " <> g "fisher setup tools" <> "    Configure tool providers")
  sayLn ui ""
  sayLn ui ("   " <> g "fisher config" <> "         View current settings")
  sayLn ui ("   " <> g "fisher config edit" <> "    Open config in your editor")
  sayLn ui ("   " <> g "fisher config set <key> <value>")
  sayLn ui "                           Set a specific value"
  sayLn ui ""
  sayLn ui "   Or edit the files directly:"
  sayLn ui ("   " <> paint ui [dim] ("nano " <> cfgPath))
  sayLn ui ("   " <> paint ui [dim] ("nano " <> envPath))
  sayLn ui ""
  sayLn ui (paint ui [dim] (T.replicate 60 "─"))
  sayLn ui ""
  sayLn ui (paint ui [cyan, bold] "🚀 Ready to go!")
  sayLn ui ""
  sayLn ui ("   " <> g "fisher" <> "              Start chatting")
  sayLn ui ("   " <> g "fisher doctor" <> "       Check for issues")
  sayLn ui ""

-- ------------------------------------------------------- model & provider

defaultModelName :: Text
defaultModelName = "anthropic/claude-opus-4.6"

-- | @config.get("model", d)@ read as the current model name.
currentModelOf :: Text -> PyVal -> Text
currentModelOf dflt cfg = case get "model" cfg of
  Nothing -> dflt
  Just (PDict kvs) -> maybe dflt pyStrOf (lookup "default" kvs)
  Just v -> if truthy v then pyStrOf v else dflt

-- | The raw @config.yaml@ rewrite of setup's OpenRouter and custom
-- branches (@yaml.safe_dump@, no comment blocks, the mode left alone), at
-- @$FISHER_HOME@ as given, else the Fisher home.
-- False where upstream's attempt raises.
rawModelRewrite :: Sys -> (PyVal -> PyVal) -> IO Bool
rawModelRewrite sys f = do
  given <- sysGetEnv sys "FISHER_HOME"
  let path = case given of
        Just h | not (T.null h) -> T.unpack h </> "config.yaml"
        _ -> sysFisherHome sys </> "config.yaml"
  exists <- doesFileExist path
  loaded <- if exists then readRawConfig path else pure (Just (PDict []))
  case loaded of
    Just disk@(PDict _) -> do
      let section = case get "model" disk of
            Nothing -> Just (PDict [])
            Just (PStr s) -> Just (obj [("default", str s)])
            Just d@(PDict _) -> Just d
            Just _ -> Nothing
      case section of
        Just m -> do
          r <- tryPy (writeRawConfig path (set "model" (f m) disk))
          pure (case r of Ok () -> True; _ -> False)
        Nothing -> pure False
    _ -> pure False

-- | @setup_model_provider(config)@.
setupModelProvider :: Env -> IORef PyVal -> IO ()
setupModelProvider e cref = do
  let ui = eUI e
      sys = eSys e
      modify' f = modifyIORef cref f
      clearCustom existingCustom = unless (T.null existingCustom) $ do
        saveEnvValue sys "OPENAI_BASE_URL" ""
        saveEnvValue sys "OPENAI_API_KEY" ""
  printHeader ui "Inference Provider"
  printInfo ui "Choose how to connect to your main chat model."
  sayLn ui ""
  existingOr <- envValue sys "OPENROUTER_API_KEY"
  activeOauthV <- getActiveProvider sys
  existingCustom <- envValue sys "OPENAI_BASE_URL"
  config0 <- readIORef cref
  let modelCfg = getD "model" config0
      truthyStr k = case get k modelCfg of
        Just v | truthy v -> T.strip (pyStrOf v)
        _ -> ""
      cfgProvider = case T.toLower (truthyStr "provider") of
        "" -> Nothing
        "auto" -> Nothing
        p -> Just p
      cfgBaseUrl = truthyStr "base_url"
      activeOauth = case activeOauthV of
        PStr s -> s
        _ -> ""
      inRegistry p = isJust (lookupProvider p)
      nameOf p = maybe p pcName (lookupProvider p)
  detected <- detectExternalCredentials sys
  unless (null detected) $ do
    printInfo ui "Detected existing credentials:"
    forM_ detected $ \c ->
      if get "provider" c == Just (str "openai-codex")
        then printSuccess ui ("  * " <> getS "label" c <> " -- select \"OpenAI Codex\" to use it")
        else printInfo ui ("  * " <> getS "label" c)
    sayLn ui ""
  let hasAny = isJust cfgProvider || truthy activeOauthV || not (T.null existingCustom) || not (T.null existingOr)
      keepLabel = case cfgProvider of
        Just "custom" ->
          let l = if T.null cfgBaseUrl then existingCustom else cfgBaseUrl
           in Just (if T.null l then "Keep current (Custom)" else "Keep current (Custom: " <> l <> ")")
        Just "openrouter" -> Just "Keep current (OpenRouter)"
        Just p | inRegistry p -> Just ("Keep current (" <> nameOf p <> ")")
        _
          | not (T.null activeOauth) && inRegistry activeOauth -> Just ("Keep current (" <> nameOf activeOauth <> ")")
          | not (T.null existingCustom) -> Just ("Keep current (Custom: " <> existingCustom <> ")")
          | not (T.null existingOr) -> Just "Keep current (OpenRouter)"
          | otherwise -> Nothing
      choices =
        [ "Login with OpenAI Codex"
        , "OpenRouter API key (100+ models, pay-per-use)", "Custom OpenAI-compatible endpoint (self-hosted / VLLM / etc.)"
        , "Z.AI / GLM (Zhipu AI models)", "Kimi / Moonshot (Kimi coding models)", "MiniMax (global endpoint)"
        , "MiniMax China (mainland China endpoint)", "Anthropic (Claude models — API key or Claude Code subscription)" ]
          <> maybe [] pure keepLabel
      dflt = if hasAny then length choices - 1 else 1
  unless hasAny $ do
    printWarning ui "An inference provider is required for Fisher to work."
    sayLn ui ""
  idx <- promptChoice ui "Select your inference provider:" choices dflt
  let loginFailed what = do
        printWarning ui (what <> " login was cancelled or failed.")
        printInfo ui ("You can try again later with: " <> cliName <> " model")
        pure (Nothing, Nothing)
      loginRaised ex = do
        printError ui ("Login failed: " <> errorStr ex)
        printInfo ui ("You can try again later with: " <> cliName <> " model")
        pure (Nothing, Nothing)
  (selected0, selectedBase) <- case idx of
    0 -> do
      sayLn ui ""
      printHeader ui "OpenAI Codex Login"
      sayLn ui ""
      r <- tryPy $ do
        loginOpenAICodex sys
        clearCustom existingCustom
        void (updateConfigForProvider sys "openai-codex" defaultCodexBaseUrl Nothing)
        modify' (setModelProvider "openai-codex" defaultCodexBaseUrl)
      case r of
        Ok () -> pure (Just "openai-codex", Nothing)
        Exited -> loginFailed "OpenAI Codex"
        Raised ex -> loginRaised ex
    1 -> do
      sayLn ui ""
      printHeader ui "OpenRouter API Key"
      printInfo ui "OpenRouter provides access to 100+ models from multiple providers."
      printInfo ui "Get your API key at: https://openrouter.ai/keys"
      if not (T.null existingOr)
        then do
          printInfo ui ("Current: " <> T.take 8 existingOr <> "... (configured)")
          upd <- promptYesNo ui "Update OpenRouter API key?" False
          when upd $ do
            k <- prompt ui "  OpenRouter API key" Nothing True
            unless (T.null k) $ do
              saveEnvValue sys "OPENROUTER_API_KEY" k
              printSuccess ui "OpenRouter API key updated"
        else do
          k <- prompt ui "  OpenRouter API key" Nothing True
          if not (T.null k)
            then saveEnvValue sys "OPENROUTER_API_KEY" k >> printSuccess ui "OpenRouter API key saved"
            else printWarning ui "Skipped - agent won't work without an API key"
      clearCustom existingCustom
      quietly (deactivateProvider sys)
      ok <- rawModelRewrite sys (delete "base_url" . set "provider" (str "openrouter"))
      when ok $ modify' (setModelProvider "openrouter" "")
      pure (Just "openrouter", Nothing)
    2 -> do
      sayLn ui ""
      printHeader ui "Custom OpenAI-Compatible Endpoint"
      printInfo ui "Works with any API that follows OpenAI's chat completions spec"
      currentUrl <- envValue sys "OPENAI_BASE_URL"
      currentKey <- envValue sys "OPENAI_API_KEY"
      cfg <- readIORef cref
      let currentModel = case get "model" cfg of
            Just (PDict kvs) -> maybe "" pyStrOf (lookup "default" kvs)
            Just v | truthy v -> pyStrOf v
            _ -> ""
      unless (T.null currentUrl) $ printInfo ui ("  Current URL: " <> currentUrl)
      unless (T.null currentKey) $ printInfo ui ("  Current key: " <> T.take 8 currentKey <> "... (configured)")
      baseUrl0 <- T.strip <$> prompt ui "  API base URL (e.g., https://api.example.com/v1)" (Just currentUrl) False
      apiKey <- prompt ui "  API key" Nothing True
      modelName <- prompt ui "  Model name (e.g., gpt-4, claude-3-opus)" (Just currentModel) False
      baseUrl <-
        if T.null baseUrl0
          then pure baseUrl0
          else do
            probe <- probeApiModels sys apiKey baseUrl0 5.0
            b <-
              if prUsedFallback probe && not (T.null (prResolvedBaseUrl probe))
                then do
                  printWarning ui ("Endpoint verification worked at " <> prResolvedBaseUrl probe <> "/models, not the exact URL you entered. Saving the working base URL instead.")
                  pure (prResolvedBaseUrl probe)
                else do
                  case prModels probe of
                    Just ms -> printSuccess ui ("Verified endpoint via " <> fromMaybe "None" (prProbedUrl probe) <> " (" <> showT (length ms) <> " model(s) visible)")
                    Nothing -> do
                      printWarning ui ("Could not verify this endpoint via " <> fromMaybe "None" (prProbedUrl probe) <> ". Fisher will still save it.")
                      forM_ (prSuggestedBaseUrl probe) $ \s -> printInfo ui ("  If this server expects /v1, try base URL: " <> s)
                  pure baseUrl0
            saveEnvValue sys "OPENAI_BASE_URL" b
            pure b
      unless (T.null apiKey) $ saveEnvValue sys "OPENAI_API_KEY" apiKey
      unless (T.null modelName) $ modify' (setDefaultModel modelName)
      quietly (deactivateProvider sys)
      unless (T.null baseUrl) $ do
        void $ rawModelRewrite sys $ \m ->
          let m1 = set "base_url" (str (T.dropWhileEnd (== '/') baseUrl)) (set "provider" (str "custom") m)
           in if T.null modelName then m1 else set "default" (str modelName) m1
        modify' (setModelProvider "custom" baseUrl)
      printSuccess ui "Custom endpoint configured"
      pure (Just "custom", Nothing)
    3 -> do
      sayLn ui ""
      printHeader ui "Z.AI / GLM API Key"
      let zaiDefault = "https://api.z.ai/api/paas/v4"
      printInfo ui "Provider: Z.AI / GLM"
      printInfo ui "Get your API key at: https://open.bigmodel.cn/"
      sayLn ui ""
      glm <- envValue sys "GLM_API_KEY"
      existingKey <- if T.null glm then envValue sys "ZAI_API_KEY" else pure glm
      apiKey <-
        if not (T.null existingKey)
          then do
            printInfo ui ("Current: " <> T.take 8 existingKey <> "... (configured)")
            upd <- promptYesNo ui "Update API key?" False
            if upd
              then do
                k <- prompt ui "  GLM API key" Nothing True
                if T.null k
                  then pure existingKey
                  else saveEnvValue sys "GLM_API_KEY" k >> printSuccess ui "GLM API key updated" >> pure k
              else pure existingKey
          else do
            k <- prompt ui "  GLM API key" Nothing True
            if not (T.null k)
              then saveEnvValue sys "GLM_API_KEY" k >> printSuccess ui "GLM API key saved"
              else printWarning ui "Skipped - agent won't work without an API key"
            pure k
      base <-
        if T.null apiKey
          then pure zaiDefault
          else do
            sayLn ui ""
            printInfo ui "Detecting your z.ai endpoint..."
            detectZaiEndpoint sys apiKey 8.0 >>= \case
              Just ep -> do
                printSuccess ui ("Detected: " <> zeLabel ep <> " endpoint")
                printInfo ui ("  URL: " <> zeBaseUrl ep)
                when ("coding" `T.isPrefixOf` zeId ep) $
                  printInfo ui ("  Note: Coding Plan endpoint detected (default model: " <> zeModel ep <> "). GLM-5 may still be available depending on your plan tier.")
                saveEnvValue sys "GLM_BASE_URL" (zeBaseUrl ep)
                pure (zeBaseUrl ep)
              Nothing -> do
                printWarning ui "Could not verify any z.ai endpoint with this key."
                printInfo ui ("  Using default: " <> zaiDefault)
                printInfo ui "  If you get billing errors, check your plan at https://open.bigmodel.cn/"
                pure zaiDefault
      clearCustom existingCustom
      modify' (setModelProvider "zai" base)
      pure (Just "zai", Just base)
    4 -> apiKeyBranch "kimi-coding" "Kimi / Moonshot API Key" "https://platform.moonshot.cn/" "KIMI_API_KEY" "Kimi" existingCustom
    5 -> apiKeyBranch "minimax" "MiniMax API Key" "https://platform.minimaxi.com/" "MINIMAX_API_KEY" "MiniMax" existingCustom
    6 -> apiKeyBranch "minimax-cn" "MiniMax China API Key" "https://platform.minimaxi.com/" "MINIMAX_CN_API_KEY" "MiniMax CN" existingCustom
    7 -> do
      sayLn ui ""
      printHeader ui "Anthropic Authentication"
      cc <- readClaudeCodeCredentials sys
      ccValid <- maybe (pure False) (isClaudeCodeTokenValid sys) cc
      tokenEnv <- envValue sys "ANTHROPIC_TOKEN"
      apiEnv <- envValue sys "ANTHROPIC_API_KEY"
      ccEnv <- fromMaybe "" <$> sysGetEnv sys "CLAUDE_CODE_OAUTH_TOKEN"
      let existingKey = head ([k | k <- [tokenEnv, apiEnv, ccEnv], not (T.null k)] <> [""])
          hasCreds = not (T.null existingKey) || ccValid
      needsAuth <-
        if hasCreds
          then do
            if not (T.null existingKey)
              then printInfo ui ("Current credentials: " <> T.take 12 existingKey <> "...")
              else printSuccess ui "Found valid Claude Code credentials (auto-detected)"
            c <- promptChoice ui "What would you like to do?" ["Use existing credentials", "Reauthenticate (new OAuth login)", "Cancel"] 0
            pure (c == 1)
          else pure True
      when needsAuth $ do
        method <- promptChoice ui "Choose authentication method:" ["Claude Pro/Max subscription (OAuth login)", "Anthropic API key (pay-per-token)"] 0
        if method == 0
          then do
            r <- try $ do
              sayLn ui ""
              printInfo ui "Running 'claude setup-token' — follow the prompts below."
              printInfo ui "A browser window will open for you to authorize access."
              sayLn ui ""
              tok <- runOauthSetupToken sys
              case tok of
                Just t | not (T.null t) -> saveAnthropicOauthToken sys t >> printSuccess ui "OAuth credentials saved"
                _ -> do
                  sayLn ui ""
                  t <- prompt ui "Paste setup-token here (if displayed above)" Nothing True
                  if not (T.null t)
                    then saveAnthropicOauthToken sys t >> printSuccess ui "Setup-token saved"
                    else printWarning ui "Skipped — agent won't work without credentials"
            case r of
              Right () -> pure ()
              Left ex
                | peType ex == "FileNotFoundError" -> do
                    sayLn ui ""
                    printInfo ui "The 'claude' CLI is required for OAuth login."
                    sayLn ui ""
                    printInfo ui "To install: npm install -g @anthropic-ai/claude-code"
                    printInfo ui "Then run:   claude setup-token"
                    printInfo ui "Or paste an existing setup-token below:"
                    sayLn ui ""
                    t <- prompt ui "Setup-token (sk-ant-oat-...)" Nothing True
                    if not (T.null t)
                      then saveAnthropicOauthToken sys t >> printSuccess ui "Setup-token saved"
                      else printWarning ui "Skipped — install Claude Code and re-run setup"
                | otherwise -> throwIO ex
          else do
            sayLn ui ""
            printInfo ui "Get an API key at: https://console.anthropic.com/settings/keys"
            sayLn ui ""
            k <- prompt ui "API key (sk-ant-...)" Nothing True
            if not (T.null k)
              then saveAnthropicApiKey sys k >> printSuccess ui "API key saved"
              else printWarning ui "Skipped — agent won't work without credentials"
      clearCustom existingCustom
      modify' (setModelProvider "anthropic" "")
      pure (Just "anthropic", Just "")
    _ -> pure (Nothing, Nothing)
  let selected = case selected0 of
        Just p -> Just p
        Nothing
          | Just p <- cfgProvider -> Just p
          | not (T.null activeOauth) && inRegistry activeOauth -> Just activeOauth
          | not (T.null existingCustom) -> Just "custom"
          | not (T.null existingOr) -> Just "openrouter"
          | otherwise -> Nothing
  -- Vision.
  backends <- either (const []) id <$> (tryPy (availableVisionBackends sys) >>= \case Ok r -> pure r; _ -> pure (Left ()))
  let needsVision = null backends && not (maybe False (`elem` backends) selected)
  when needsVision $ do
    let display = case selected of
          Just p -> fromMaybe p (lookup p [("zai", "Z.AI / GLM"), ("kimi-coding", "Kimi / Moonshot"), ("minimax", "MiniMax"), ("minimax-cn", "MiniMax CN"), ("anthropic", "Anthropic"), ("custom", "your custom endpoint")])
          Nothing -> "your provider"
    sayLn ui ""
    printHeader ui "Vision & Image Analysis (optional)"
    printInfo ui ("Vision uses a separate multimodal backend. " <> display)
    printInfo ui "doesn't currently provide one Fisher can auto-use for vision,"
    printInfo ui "so choose a backend now or skip and configure later."
    sayLn ui ""
    v <- promptChoice ui "Configure vision:"
      [ "OpenRouter — uses Gemini (free tier at openrouter.ai/keys)"
      , "OpenAI-compatible endpoint — base URL, API key, and vision model", "Skip for now" ] 2
    case v of
      0 -> do
        k <- T.strip <$> prompt ui "  OpenRouter API key" Nothing True
        if not (T.null k)
          then saveEnvValue sys "OPENROUTER_API_KEY" k >> printSuccess ui "OpenRouter key saved — vision will use Gemini"
          else printInfo ui "Skipped — vision won't be available"
      1 -> do
        b0 <- T.strip <$> prompt ui "  Base URL (blank for OpenAI)" Nothing False
        let b = if T.null b0 then "https://api.openai.com/v1" else b0
            isOpenAI = "api.openai.com" `T.isInfixOf` T.toLower b
        k <- T.strip <$> prompt ui (if isOpenAI then "  OpenAI API key" else "  API key") Nothing True
        if not (T.null k)
          then do
            saveEnvValue sys "OPENAI_API_KEY" k
            saveEnvValue sys "OPENAI_BASE_URL" b
            vm <-
              if isOpenAI
                then do
                  let ms = ["gpt-4o", "gpt-4o-mini", "gpt-4.1", "gpt-4.1-mini", "gpt-4.1-nano"]
                  i <- promptChoice ui "Select vision model:" (ms <> ["Use default (gpt-4o-mini)"]) 0
                  pure (if i < length ms then ms !! i else "gpt-4o-mini")
                else T.strip <$> prompt ui "  Vision model (blank = use main/custom default)" Nothing False
            saveEnvValue sys "AUXILIARY_VISION_MODEL" vm
            printSuccess ui ("Vision configured with " <> b <> (if T.null vm then "" else " (" <> vm <> ")"))
          else printInfo ui "Skipped — vision won't be available"
      _ -> printInfo ui ("Skipped — add later with '" <> cliName <> " setup' or configure AUXILIARY_VISION_* settings")
  -- Default model.
  when (selected /= Just "custom") $ do
    printHeader ui "Default Model"
    cfg <- readIORef cref
    let currentModel = currentModelOf defaultModelName cfg
        keep = "Keep current (" <> currentModel <> ")"
    printInfo ui ("Current: " <> currentModel)
    case selected of
      Just "openai-codex" -> do
        tok <- tryPy (resolveCodexRuntimeCredentials sys defaultCodexOpts) >>= \case
          Ok c -> pure (case get "api_key" c of Just (PStr k) -> Just k; _ -> Nothing)
          _ -> pure Nothing
        codexModels <- getCodexModelIds sys tok
        let mc = codexModels <> [keep]
            d = case lookup currentModel (zip codexModels [0 ..]) of
              Just j -> j
              Nothing -> if T.null currentModel then 0 else length mc - 1
        i <- promptChoice ui "Select default model:" mc d
        if i < length codexModels
          then modify' (setDefaultModel (codexModels !! i))
          else when (i == length codexModels) $ do
            n <- prompt ui "Enter model name" Nothing False
            unless (T.null n) $ modify' (setDefaultModel n)
        void (updateConfigForProvider sys "openai-codex" defaultCodexBaseUrl Nothing)
        modify' (setModelProvider "openai-codex" defaultCodexBaseUrl)
      Just p | p `elem` ["zai", "kimi-coding", "minimax", "minimax-cn"] -> providerModelSelection ui sys cref p currentModel
      Just "anthropic" -> do
        live <- fetchAnthropicModels sys
        let fromCatalogue = case live of
              Just xs | not (null xs) -> xs
              _ -> providerModels "anthropic"
            ms = if null fromCatalogue then ["claude-opus-4-6", "claude-sonnet-4-6", "claude-haiku-4-5-20251001"] else fromCatalogue
            mc = ms <> ["Custom model", keep]
        i <- promptChoice ui "Select default model:" mc (length mc - 1)
        if i < length ms
          then modify' (setDefaultModel (ms !! i))
          else when (i == length ms) $ do
            n <- prompt ui "Enter model name (e.g., claude-sonnet-4-20250514)" Nothing False
            unless (T.null n) $ modify' (setDefaultModel n)
      _ -> do
        let mc = menuLabels <> ["Custom model", keep]
        i <- promptChoice ui "Select default model:" mc (length mc - 1)
        if i < length modelIds
          then modify' (setDefaultModel (modelIds !! i))
          else when (i == length modelIds) $ do
            n <- prompt ui "Enter model name (e.g., anthropic/claude-opus-4.6)" Nothing False
            unless (T.null n) $ modify' (setDefaultModel n)
    final <- readIORef cref
    case get "model" final of
      Just m | truthy m -> do
        let display = case m of
              PDict kvs -> maybe (pyRepr m) pyStrOf (lookup "default" kvs)
              _ -> pyStrOf m
        printSuccess ui ("Model set to: " <> display)
      _ -> pure ()
  case (selected, selectedBase) of
    (Just p, Just b) | p `elem` ["zai", "kimi-coding", "minimax", "minimax-cn", "anthropic"] ->
      void (updateConfigForProvider sys p b Nothing)
    _ -> pure ()
  readIORef cref >>= saveCfg sys
  where
    apiKeyBranch pid header keyUrl var label existingCustom = do
      let ui = eUI e
          sys = eSys e
          pc = lookupProvider pid
          url = maybe "" pcInferenceBaseUrl pc
      sayLn ui ""
      printHeader ui header
      printInfo ui ("Provider: " <> maybe pid pcName pc)
      printInfo ui ("Base URL: " <> url)
      printInfo ui ("Get your API key at: " <> keyUrl)
      sayLn ui ""
      existing <- envValue sys var
      if not (T.null existing)
        then do
          printInfo ui ("Current: " <> T.take 8 existing <> "... (configured)")
          upd <- promptYesNo ui "Update API key?" False
          when upd $ do
            k <- prompt ui ("  " <> label <> " API key") Nothing True
            unless (T.null k) $ saveEnvValue sys var k >> printSuccess ui (label <> " API key updated")
        else do
          k <- prompt ui ("  " <> label <> " API key") Nothing True
          if not (T.null k)
            then saveEnvValue sys var k >> printSuccess ui (label <> " API key saved")
            else printWarning ui "Skipped - agent won't work without an API key"
      unless (T.null existingCustom) $ do
        saveEnvValue sys "OPENAI_BASE_URL" ""
        saveEnvValue sys "OPENAI_API_KEY" ""
      modifyIORef (cref) (setModelProvider pid url)
      pure (Just pid, Just url)

-- | @_DEFAULT_PROVIDER_MODELS@.
defaultProviderModels :: Text -> [Text]
defaultProviderModels = \case
  "zai" -> ["glm-5", "glm-4.7", "glm-4.5", "glm-4.5-flash"]
  "kimi-coding" -> ["kimi-k2.5", "kimi-k2-thinking", "kimi-k2-turbo-preview"]
  "minimax" -> ["MiniMax-M2.5", "MiniMax-M2.5-highspeed", "MiniMax-M2.1"]
  "minimax-cn" -> ["MiniMax-M2.5", "MiniMax-M2.5-highspeed", "MiniMax-M2.1"]
  _ -> []

-- | @_setup_provider_model_selection(config, provider_id, current_model, ...)@.
providerModelSelection :: UI -> Sys -> IORef PyVal -> Text -> Text -> IO ()
providerModelSelection ui sys cref pid currentModel = do
  let pc = lookupProvider pid
      name = maybe pid pcName pc
      firstKey [] = pure ""
      firstKey (v : vs) = do
        a <- envValue sys v
        b <- if T.null a then fromMaybe "" <$> sysGetEnv sys v else pure a
        if T.null b then firstKey vs else pure b
  key <- firstKey (maybe [] pcApiKeyEnvVars pc)
  envBase <- case maybe "" pcBaseUrlEnvVar pc of
    "" -> pure ""
    v -> envValue sys v
  let base = if T.null envBase then maybe "" pcInferenceBaseUrl pc else envBase
  live <- fetchApiModels sys key base 5.0
  models <- case live of
    Just ms | not (null ms) -> printInfo ui ("Found " <> showT (length ms) <> " model(s) from " <> name <> " API") >> pure ms
    _ -> do
      let ms = defaultProviderModels pid
      unless (null ms) $
        printWarning ui ("Could not auto-detect models from " <> name <> " API — showing defaults.\n    Use \"Custom model\" if the model you expect isn't listed.")
      pure ms
  let mc = models <> ["Custom model", "Keep current (" <> currentModel <> ")"]
  i <- promptChoice ui "Select default model:" mc (length mc - 1)
  if i < length models
    then modifyIORef cref (setDefaultModel (models !! i))
    else
      if i == length models
        then do
          n <- prompt ui "Enter model name" Nothing False
          unless (T.null n) $ modifyIORef cref (setDefaultModel n)
        else when ("/" `T.isInfixOf` currentModel && not (null models)) $ do
          printWarning ui ("Current model \"" <> currentModel <> "\" looks like an OpenRouter model and won't work with " <> name <> ". Switching to " <> head models <> ".")
          modifyIORef cref (setDefaultModel (head models))

-- ---------------------------------------------------------------- terminal

-- | @_prompt_container_resources(config)@.
promptContainerResources :: UI -> IORef PyVal -> IO ()
promptContainerResources ui cref = do
  modifyIORef cref (\c -> set "terminal" (getD "terminal" c) c)
  term0 <- getD "terminal" <$> readIORef cref
  sayLn ui ""
  printInfo ui "Container Resource Settings:"
  let persist = maybe True truthy (get "container_persistent" term0)
  printInfo ui "  Persistent filesystem keeps files between sessions."
  printInfo ui "  Set to 'no' for ephemeral sandboxes that reset each time."
  p <- prompt ui "  Persist filesystem across sessions? (yes/no)" (Just (if persist then "yes" else "no")) False
  setTerm "container_persistent" (PBool (T.toLower p `elem` ["yes", "true", "y", "1"]))
  cpu <- prompt ui "  CPU cores" (Just (maybe "1" pyStrOf (get "container_cpu" term0))) False
  forM_ (pyFloatText cpu) $ \d -> setTerm "container_cpu" (PFloat d)
  mem <- prompt ui "  Memory in MB (5120 = 5GB)" (Just (maybe "5120" pyStrOf (get "container_memory" term0))) False
  forM_ (pyIntOf mem) $ \n -> setTerm "container_memory" (PInt n)
  disk <- prompt ui "  Disk in MB (51200 = 50GB)" (Just (maybe "51200" pyStrOf (get "container_disk" term0))) False
  forM_ (pyIntOf disk) $ \n -> setTerm "container_disk" (PInt n)
  where
    setTerm k v = modifyIORef cref (\c -> set "terminal" (set k v (getD "terminal" c)) c)

-- | @setup_terminal_backend(config)@.
setupTerminalBackend :: Env -> IORef PyVal -> IO ()
setupTerminalBackend e cref = do
  let ui = eUI e
      sys = eSys e
      setTerm k v = modifyIORef cref (\c -> set "terminal" (set k v (getD "terminal" c)) c)
      pipInstall pkg = do
        uv <- sysWhich sys "uv"
        let argv = case uv of
              Just u -> [T.pack u, "pip", "install", "--python", siPython (eInfo e), pkg]
              Nothing -> [siPython (eInfo e), "-m", "pip", "install", pkg]
        sysRun sys argv >>= \case
          Right r -> pure r
          Left msg -> throwIO (pyError "FileNotFoundError" (msg <> ": '" <> head argv <> "'"))
  printHeader ui "Terminal Backend"
  printInfo ui "Choose where Fisher runs shell commands and code."
  printInfo ui "This affects tool execution, file access, and isolation."
  sayLn ui ""
  cfg <- readIORef cref
  let current = maybe "local" pyStrOf (get "backend" (getD "terminal" cfg))
      isLinux = sysPlatform sys == "linux"
      backends = ["local", "docker", "modal", "ssh", "daytona"] <> ["singularity" | isLinux]
      choices =
        [ "Local - run directly on this machine (default)", "Docker - isolated container with configurable resources"
        , "Modal - serverless cloud sandbox", "SSH - run on a remote machine", "Daytona - persistent cloud development environment" ]
          <> ["Singularity/Apptainer - HPC-friendly container" | isLinux]
          <> ["Keep current (" <> current <> ")"]
      keepIdx = length backends
  idx <- promptChoice ui "Select terminal backend:" choices keepIdx
  if idx == keepIdx
    then printInfo ui ("Keeping current backend: " <> current)
    else do
      let backend = backends !! idx
      setTerm "backend" (str backend)
      case backend of
        "local" -> do
          printSuccess ui "Terminal backend: Local"
          printInfo ui "Commands run directly on this machine."
          sayLn ui ""
          sudo <- envValue sys "SUDO_PASSWORD"
          if not (T.null sudo)
            then printInfo ui "Sudo password: configured"
            else do
              yes <- promptYesNo ui "Enable sudo support? (stores password for apt install, etc.)" False
              when yes $ do
                pw <- prompt ui "  Sudo password" Nothing True
                unless (T.null pw) $ saveEnvValue sys "SUDO_PASSWORD" pw >> printSuccess ui "Sudo password saved"
        "docker" -> do
          printSuccess ui "Terminal backend: Docker"
          sysWhich sys "docker" >>= \case
            Nothing -> do
              printWarning ui "Docker not found in PATH!"
              printInfo ui "Install Docker: https://docs.docker.com/get-docker/"
            Just d -> printInfo ui ("Docker found: " <> T.pack d)
          c <- readIORef cref
          image <- prompt ui "  Docker image" (Just (maybe "python:3.11-slim" pyStrOf (get "docker_image" (getD "terminal" c)))) False
          setTerm "docker_image" (str image)
          saveEnvValue sys "TERMINAL_DOCKER_IMAGE" image
          promptContainerResources ui cref
        "singularity" -> do
          printSuccess ui "Terminal backend: Singularity/Apptainer"
          a <- sysWhich sys "apptainer"
          s <- maybe (sysWhich sys "singularity") (pure . Just) a
          case s of
            Nothing -> do
              printWarning ui "Singularity/Apptainer not found in PATH!"
              printInfo ui "Install: https://apptainer.org/docs/admin/main/installation.html"
            Just b -> printInfo ui ("Found: " <> T.pack b)
          c <- readIORef cref
          image <- prompt ui "  Container image" (Just (maybe "docker://python:3.11-slim" pyStrOf (get "singularity_image" (getD "terminal" c)))) False
          setTerm "singularity_image" (str image)
          saveEnvValue sys "TERMINAL_SINGULARITY_IMAGE" image
          promptContainerResources ui cref
        "modal" -> do
          printSuccess ui "Terminal backend: Modal"
          printInfo ui "Serverless cloud sandboxes. Each session gets its own container."
          printInfo ui "Requires a Modal account: https://modal.com"
          printInfo ui "Installing swe-rex[modal]..."
          r <- pipInstall "swe-rex[modal]"
          if rrCode r == 0
            then printSuccess ui "swe-rex[modal] installed"
            else printWarning ui "Install failed — run manually: pip install 'swe-rex[modal]'"
          sayLn ui ""
          printInfo ui "Modal authentication:"
          printInfo ui "  Get your token at: https://modal.com/settings"
          existing <- envValue sys "MODAL_TOKEN_ID"
          let ask = do
                tid <- prompt ui "    Modal Token ID" Nothing True
                tsec <- prompt ui "    Modal Token Secret" Nothing True
                unless (T.null tid) $ saveEnvValue sys "MODAL_TOKEN_ID" tid
                unless (T.null tsec) $ saveEnvValue sys "MODAL_TOKEN_SECRET" tsec
          if not (T.null existing)
            then do
              printInfo ui "  Modal token: already configured"
              upd <- promptYesNo ui "  Update Modal credentials?" False
              when upd ask
            else ask
          promptContainerResources ui cref
        "daytona" -> do
          printSuccess ui "Terminal backend: Daytona"
          printInfo ui "Persistent cloud development environments."
          printInfo ui "Each session gets a dedicated sandbox with filesystem persistence."
          printInfo ui "Sign up at: https://daytona.io"
          printInfo ui "Installing daytona SDK..."
          r <- pipInstall "daytona"
          if rrCode r == 0
            then printSuccess ui "daytona SDK installed"
            else do
              printWarning ui "Install failed — run manually: pip install daytona"
              unless (T.null (rrStderr r)) $ case T.lines (T.strip (rrStderr r)) of
                [] -> throwIO (pyError "IndexError" "list index out of range")
                ls -> printInfo ui ("  Error: " <> last ls)
          sayLn ui ""
          existing <- envValue sys "DAYTONA_API_KEY"
          if not (T.null existing)
            then do
              printInfo ui "  Daytona API key: already configured"
              upd <- promptYesNo ui "  Update API key?" False
              when upd $ do
                k <- prompt ui "    Daytona API key" Nothing True
                unless (T.null k) $ saveEnvValue sys "DAYTONA_API_KEY" k >> printSuccess ui "    Updated"
            else do
              k <- prompt ui "    Daytona API key" Nothing True
              unless (T.null k) $ saveEnvValue sys "DAYTONA_API_KEY" k >> printSuccess ui "    Configured"
          c <- readIORef cref
          image <- prompt ui "  Sandbox image" (Just (maybe "nikolaik/python-nodejs:python3.11-nodejs20" pyStrOf (get "daytona_image" (getD "terminal" c)))) False
          setTerm "daytona_image" (str image)
          saveEnvValue sys "TERMINAL_DAYTONA_IMAGE" image
          promptContainerResources ui cref
        _ -> do
          printSuccess ui "Terminal backend: SSH"
          printInfo ui "Run commands on a remote machine via SSH."
          curHost <- envValue sys "TERMINAL_SSH_HOST"
          host <- prompt ui "  SSH host (hostname or IP)" (Just curHost) False
          unless (T.null host) $ saveEnvValue sys "TERMINAL_SSH_HOST" host
          curUser <- envValue sys "TERMINAL_SSH_USER"
          envUser <- fromMaybe "" <$> sysGetEnv sys "USER"
          user <- prompt ui "  SSH user" (Just (if T.null curUser then envUser else curUser)) False
          unless (T.null user) $ saveEnvValue sys "TERMINAL_SSH_USER" user
          curPort <- envValue sys "TERMINAL_SSH_PORT"
          port <- prompt ui "  SSH port" (Just (if T.null curPort then "22" else curPort)) False
          when (not (T.null port) && port /= "22") $ saveEnvValue sys "TERMINAL_SSH_PORT" port
          curKey <- envValue sys "TERMINAL_SSH_KEY"
          let defaultKey = T.pack (sysHome sys </> ".ssh" </> "id_rsa")
          sshKey <- prompt ui "  SSH private key path" (Just (if T.null curKey then defaultKey else curKey)) False
          unless (T.null sshKey) $ saveEnvValue sys "TERMINAL_SSH_KEY" sshKey
          testIt <- if T.null host then pure False else promptYesNo ui "  Test SSH connection?" True
          when testIt $ do
            printInfo ui "  Testing connection..."
            let cmd =
                  ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=5"]
                    <> (if T.null sshKey then [] else ["-i", sshKey])
                    <> (if not (T.null port) && port /= "22" then ["-p", port] else [])
                    <> [if T.null user then host else user <> "@" <> host, "echo ok"]
            r <- sysRun sys cmd >>= \case
              Right r -> pure r
              Left msg -> throwIO (pyError "FileNotFoundError" (msg <> ": 'ssh'"))
            if rrCode r == 0
              then printSuccess ui "  SSH connection successful!"
              else do
                printWarning ui ("  SSH connection failed: " <> T.strip (rrStderr r))
                printInfo ui "  Check your SSH key and host settings."
      saveEnvValue sys "TERMINAL_ENV" backend
      readIORef cref >>= saveCfg sys
      sayLn ui ""
      printSuccess ui ("Terminal backend set to: " <> backend)

-- ------------------------------------------------------------------- agent

-- | @setup_agent_settings(config)@.
setupAgentSettings :: Env -> IORef PyVal -> IO ()
setupAgentSettings e cref = do
  let ui = eUI e
      sys = eSys e
      sub k c = getD k c
  printHeader ui "Agent Settings"
  cfg0 <- readIORef cref
  envMax <- envValue sys "FISHER_MAX_ITERATIONS"
  let currentMax = if T.null envMax then maybe "90" pyStrOf (get "max_turns" (sub "agent" cfg0)) else envMax
  printInfo ui "Maximum tool-calling iterations per conversation."
  printInfo ui "Higher = more complex tasks, but costs more tokens."
  printInfo ui "Recommended: 30-60 for most tasks, 100+ for open exploration."
  maxStr <- prompt ui "Max iterations" (Just currentMax) False
  case pyIntOf maxStr of
    Nothing -> printWarning ui "Invalid number, keeping current value"
    Just n -> when (n > 0) $ do
      saveEnvValue sys "FISHER_MAX_ITERATIONS" (showT n)
      modifyIORef cref (\c -> delete "max_turns" (set "agent" (set "max_turns" (PInt n) (sub "agent" c)) c))
      printSuccess ui ("Max iterations set to " <> showT n)
  printInfo ui ""
  printInfo ui "Tool Progress Display"
  printInfo ui "Controls how much tool activity is shown."
  printInfo ui "  off     — Silent, just the final response"
  printInfo ui "  new     — Show tool name only when it changes (less noise)"
  printInfo ui "  all     — Show every tool call with a short preview"
  printInfo ui "  verbose — Full args, results, and debug logs"
  cfg1 <- readIORef cref
  let currentMode = maybe "all" pyStrOf (get "tool_progress" (sub "display" cfg1))
  mode <- prompt ui "Tool progress mode" (Just currentMode) False
  if T.toLower mode `elem` ["off", "new", "all", "verbose"]
    then do
      modifyIORef cref (\c -> set "display" (set "tool_progress" (str (T.toLower mode)) (sub "display" c)) c)
      readIORef cref >>= saveCfg sys
      printSuccess ui ("Tool progress set to: " <> T.toLower mode)
    else printWarning ui ("Unknown mode '" <> mode <> "', keeping '" <> currentMode <> "'")
  printHeader ui "Context Compression"
  printInfo ui "Automatically summarizes old messages when context gets too long."
  printInfo ui "Higher threshold = compress later (use more context). Lower = compress sooner."
  modifyIORef cref (\c -> set "compression" (set "enabled" (PBool True) (sub "compression" c)) c)
  cfg2 <- readIORef cref
  let currentThreshold = fromMaybe (PFloat 0.85) (get "threshold" (sub "compression" cfg2))
  tStr <- prompt ui "Compression threshold (0.5-0.95)" (Just (pyStrOf currentThreshold)) False
  forM_ (pyFloatText tStr) $ \t ->
    when (t >= 0.5 && t <= 0.95) $
      modifyIORef cref (\c -> set "compression" (set "threshold" (PFloat t) (sub "compression" c)) c)
  cfg3 <- readIORef cref
  printSuccess ui ("Context compression threshold set to " <> maybe "0.85" pyStrOf (get "threshold" (sub "compression" cfg3)))
  printHeader ui "Session Reset Policy"
  printInfo ui "Sessions accumulate context over time."
  printInfo ui "Each message adds to the conversation history, which means growing API costs."
  printInfo ui ""
  printInfo ui "To manage this, sessions can automatically reset after a period of inactivity"
  printInfo ui "or at a fixed time each day. When a reset happens, the agent saves important"
  printInfo ui "things to its persistent memory first — but the conversation context is cleared."
  printInfo ui ""
  printInfo ui "You can also manually reset anytime by typing /reset in chat."
  printInfo ui ""
  let policy = sub "session_reset" cfg3
      curMode = maybe "both" pyStrOf (get "mode" policy)
      curIdle = maybe "1440" pyStrOf (get "idle_minutes" policy)
      curHour = maybe "4" pyStrOf (get "at_hour" policy)
      dflt = fromMaybe 0 (lookup curMode [("both", 0), ("idle", 1), ("daily", 2), ("none", 3)])
  idx <- promptChoice ui "Session reset mode:"
    [ "Inactivity + daily reset (recommended - reset whichever comes first)"
    , "Inactivity only (reset after N minutes of no messages)", "Daily only (reset at a fixed hour each day)"
    , "Never auto-reset (context lives until /reset or context compression)", "Keep current settings" ] dflt
  modifyIORef cref (\c -> set "session_reset" (sub "session_reset" c) c)
  let setReset k v = modifyIORef cref (\c -> set "session_reset" (set k v (sub "session_reset" c)) c)
      askIdle = do
        s <- prompt ui "  Inactivity timeout (minutes)" (Just curIdle) False
        forM_ (pyIntOf s) $ \n -> when (n > 0) (setReset "idle_minutes" (PInt n))
      askHour = do
        s <- prompt ui "  Daily reset hour (0-23, local time)" (Just curHour) False
        forM_ (pyIntOf s) $ \n -> when (n >= 0 && n <= 23) (setReset "at_hour" (PInt n))
      resetVal k d = maybe d pyStrOf . get k . sub "session_reset" <$> readIORef cref
  case idx of
    0 -> do
      setReset "mode" (str "both")
      askIdle
      askHour
      i <- resetVal "idle_minutes" "1440"
      h <- resetVal "at_hour" "4"
      printSuccess ui ("Sessions reset after " <> i <> " min idle or daily at " <> h <> ":00")
    1 -> do
      setReset "mode" (str "idle")
      askIdle
      i <- resetVal "idle_minutes" "1440"
      printSuccess ui ("Sessions reset after " <> i <> " min of inactivity")
    2 -> do
      setReset "mode" (str "daily")
      askHour
      h <- resetVal "at_hour" "4"
      printSuccess ui ("Sessions reset daily at " <> h <> ":00")
    3 -> do
      setReset "mode" (str "none")
      printInfo ui "Sessions will never auto-reset. Context is managed only by compression."
      printWarning ui "Long conversations will grow in cost. Use /reset manually when needed."
    _ -> pure ()
  readIORef cref >>= saveCfg sys
