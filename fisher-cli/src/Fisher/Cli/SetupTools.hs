-- | @fisher setup tools@ and the tools step of the full wizard: upstream's
-- @tools_config.tools_command@ (pick a platform, toggle toolsets, then
-- choose a provider and enter keys for the ones that need them). Ported
-- from upstream @cli/tools_config.py@.
module Fisher.Cli.SetupTools
  ( toolsCommand
  , ToolsEnv (..)
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import Fisher.Cli.Prompt
import Fisher.Cli.SetupCommon
import Fisher.LLM.Auth.Env (saveEnvValue)
import Fisher.LLM.Auth.Error (cliName)
import Fisher.LLM.Sys
import Fisher.Py.Value
import Fisher.Term

-- | What the tools flow needs from the wizard.
data ToolsEnv = ToolsEnv
  { teUI :: UI
  , teInstallDir :: FilePath
  -- ^ upstream's @PROJECT_ROOT@ (where @node_modules@ would be)
  }

-- | @CONFIGURABLE_TOOLSETS@.
configurableToolsets :: [(Text, Text, Text)]
configurableToolsets =
  [ ("file", "📁 File Operations", "read, write, patch, search")
  , ("web", "🔍 Web Search", "web_search")
  , ("browser", "🌐 Browser Automation", "navigate, click, type, scroll") ]

configurableKeys :: Set.Set Text
configurableKeys = Set.fromList [k | (k, _, _) <- configurableToolsets]

labelOf :: Text -> Text
labelOf ts = case [l | (k, l, _) <- configurableToolsets, k == ts] of
  (l : _) -> l
  [] -> ts

-- | @PLATFORMS@: label and default toolset (upstream's platform toolsets
-- under Fisher's @fisher-*@ names).
platforms :: [(Text, (Text, Text))]
platforms =
  [ ("cli", ("🖥\xFE0F  CLI", "fisher-cli")), ("telegram", ("📱 Telegram", "fisher-telegram"))
  , ("discord", ("💬 Discord", "fisher-discord")), ("slack", ("💼 Slack", "fisher-slack"))
  , ("whatsapp", ("📱 WhatsApp", "fisher-whatsapp")), ("signal", ("📡 Signal", "fisher-signal"))
  , ("email", ("📧 Email", "fisher-email")) ]

platformLabel :: Text -> Text
platformLabel p = maybe p fst (lookup p platforms)

-- | @toolsets.resolve_toolset(name)@ at the pinned upstream, its
-- platform toolsets under Fisher's names.
resolveToolset :: Text -> Set.Set Text
resolveToolset name
  | name == "file" = fileTools
  | name `elem` ["web", "search"] = Set.fromList ["web_search"]
  | name == "browser" = browserTools
  | name `elem` ["all", "*", "autoformalize"] = fileTools <> browserTools
  | Just p <- T.stripPrefix "fisher-" name
  , p `elem` ["acp", "cli", "telegram", "discord", "whatsapp", "slack", "signal", "homeassistant", "email", "gateway"] =
      fileTools <> browserTools
  | otherwise = Set.empty
  where
    fileTools = Set.fromList ["patch", "read_file", "search_files", "write_file"]
    browserTools =
      Set.fromList
        [ "browser_back", "browser_click", "browser_close", "browser_get_images", "browser_navigate", "browser_press"
        , "browser_scroll", "browser_snapshot", "browser_type", "browser_vision", "web_search" ]

data EnvSpec = EnvSpec
  { esKey :: Text
  , esPrompt :: Text
  , esUrl :: Text
  }

data ToolProvider = ToolProvider
  { tpName :: Text
  , tpTag :: Text
  , tpEnv :: [EnvSpec]
  , tpPostSetup :: Maybe Text
  }

data ToolCategory = ToolCategory
  { tcName :: Text
  , tcSetupTitle :: Maybe Text
  , tcSetupNote :: Maybe Text
  , tcIcon :: Text
  , tcProviders :: [ToolProvider]
  }

-- | @TOOL_CATEGORIES@.
toolCategory :: Text -> Maybe ToolCategory
toolCategory = \case
  "web" ->
    Just
      ToolCategory
        { tcName = "Web Search & Extract"
        , tcSetupTitle = Just "Configure Web Extraction"
        , tcSetupNote = Just "Firecrawl enables direct web search and extraction. Browser tools stay available without it."
        , tcIcon = "🔍"
        , tcProviders =
            [ ToolProvider "Firecrawl Cloud" "Recommended - hosted service" [EnvSpec "FIRECRAWL_API_KEY" "Firecrawl API key" "https://firecrawl.dev"] Nothing
            , ToolProvider "Firecrawl Self-Hosted" "Free - run your own instance" [EnvSpec "FIRECRAWL_API_URL" "Your Firecrawl instance URL (e.g., http://localhost:3002)" ""] Nothing ]
        }
  "browser" ->
    Just
      ToolCategory
        { tcName = "Browser Automation"
        , tcSetupTitle = Nothing
        , tcSetupNote = Nothing
        , tcIcon = "🌐"
        , tcProviders =
            [ ToolProvider "Local Browser" "Free headless Chromium (no API key needed)" [] (Just "browserbase")
            , ToolProvider "Browserbase" "Cloud browser with stealth & proxies"
                [EnvSpec "BROWSERBASE_API_KEY" "Browserbase API key" "https://browserbase.com", EnvSpec "BROWSERBASE_PROJECT_ID" "Browserbase project ID" ""]
                (Just "browserbase") ]
        }
  _ -> Nothing

envSet :: Sys -> Text -> IO Bool
envSet sys k = not . T.null <$> envValue sys k

providerConfigured :: Sys -> ToolProvider -> IO Bool
providerConfigured sys p
  | null (tpEnv p) = pure False
  | otherwise = and <$> mapM (envSet sys . esKey) (tpEnv p)

-- | @_toolset_has_keys@.
toolsetHasKeys :: Sys -> Text -> IO Bool
toolsetHasKeys sys ts = case toolCategory ts of
  Just cat -> or <$> mapM (providerConfigured sys) (tcProviders cat)
  Nothing -> pure True

-- | @_get_enabled_platforms@.
enabledPlatforms :: Sys -> IO [Text]
enabledPlatforms sys = do
  extra <- mapM (\(k, p) -> (\b -> [p | b]) <$> envSet sys k)
    [("TELEGRAM_BOT_TOKEN", "telegram"), ("DISCORD_BOT_TOKEN", "discord"), ("SLACK_BOT_TOKEN", "slack"), ("WHATSAPP_ENABLED", "whatsapp")]
  pure ("cli" : concat extra)

-- | @_get_platform_tools(config, platform)@.
platformTools :: PyVal -> Text -> Set.Set Text
platformTools cfg p =
  let names = case get p (getD "platform_toolsets" cfg) of
        Just (PList xs) -> [pyStrOf x | x <- xs]
        _ -> [maybe p snd (lookup p platforms)]
      allTools = Set.unions (map resolveToolset names)
   in Set.fromList
        [ k | (k, _, _) <- configurableToolsets, let ts = resolveToolset k, not (Set.null ts), ts `Set.isSubsetOf` allTools ]

-- | @_save_platform_tools(config, platform, enabled)@.
savePlatformTools :: Sys -> IORef PyVal -> Text -> Set.Set Text -> IO ()
savePlatformTools sys cref p enabled = do
  cfg <- readIORef cref
  let pts = case get "platform_toolsets" cfg of
        Just d@(PDict _) -> d
        _ -> PDict []
      existing = case get p pts of
        Just (PList xs) -> [pyStrOf x | x <- xs]
        _ -> []
      preserved = Set.fromList [e | e <- existing, not (Set.member e configurableKeys)]
      value = PList (map str (sort (Set.toList (enabled <> preserved))))
      cfg' = set "platform_toolsets" (set p value pts) cfg
  writeIORef cref cfg'
  saveCfg sys cfg'

-- | @_prompt_toolset_checklist(platform_label, enabled)@.
toolsetChecklist :: UI -> Text -> Set.Set Text -> IO (Set.Set Text)
toolsetChecklist ui plabel enabled = do
  let sys = uiSys ui
  labels <- mapM (\(k, l, d) -> do
                    hasKeys <- toolsetHasKeys sys k
                    let suffix = if not hasKeys && maybe False (const True) (toolCategory k) then "  [no API key]" else ""
                    pure (l <> "  (" <> d <> ")" <> suffix))
    configurableToolsets
  let pre = Set.fromList [i | (i, (k, _, _)) <- zip [0 ..] configurableToolsets, Set.member k enabled]
  chosen <- cursesChecklist ui ("Tools for " <> plabel) labels pre pre
  pure (Set.fromList [k | (i, (k, _, _)) <- zip [0 ..] configurableToolsets, Set.member i chosen])

-- | @_run_post_setup(key)@.
runPostSetup :: ToolsEnv -> Text -> IO ()
runPostSetup te key = when (key == "browserbase") $ do
  let ui = teUI te
      sys = uiSys ui
  installed <- doesDirectoryExist (teInstallDir te </> "node_modules" </> "agent-browser")
  npm <- sysWhich sys "npm"
  if not installed && npm /= Nothing
    then do
      printInfo ui "    Installing Node.js dependencies for browser tools..."
      r <- sysRun sys ["npm", "install", "--silent"]
      case r of
        Right rr | rrCode rr == 0 -> sayLn ui (paint ui [green] "✓     Node.js dependencies installed")
        _ -> sayLn ui (paint ui [yellow] ("⚠     npm install failed - run manually: cd " <> T.pack (teInstallDir te) <> " && npm install"))
    else unless installed $
      sayLn ui (paint ui [yellow] ("⚠     Node.js not found - browser tools require: npm install (in " <> T.pack (teInstallDir te) <> ")"))

-- | @_configure_provider(provider, config)@.
configureProvider :: ToolsEnv -> ToolProvider -> IO ()
configureProvider te p = do
  let ui = teUI te
      sys = uiSys ui
  if null (tpEnv p)
    then success ui ("  " <> tpName p <> " - no configuration needed!")
    else do
      oks <- mapM (\v -> do
                     existing <- envSet sys (esKey v)
                     if existing
                       then success ui ("  " <> esKey v <> ": already configured") >> pure True
                       else do
                         unless (T.null (esUrl v)) $ printInfo ui ("  Get yours at: " <> esUrl v)
                         value <- toolsPrompt ui ("    " <> esPrompt v) Nothing True
                         if not (T.null value)
                           then saveEnvValue sys (esKey v) value >> success ui "    Saved" >> pure True
                           else sayLn ui (paint ui [yellow] "⚠     Skipped") >> pure False)
        (tpEnv p)
      let allConfigured = and oks
      when allConfigured $ forM_ (tpPostSetup p) (runPostSetup te)
      when allConfigured $ success ui ("  " <> tpName p <> " configured!")
  where
    success ui t = sayLn ui (paint ui [green] ("✓ " <> t))

providerChoiceLabels :: Sys -> [ToolProvider] -> IO ([Text], Int)
providerChoiceLabels sys ps = do
  confs <- mapM (providerConfigured sys) ps
  let labels =
        [ tpName p <> (if T.null (tpTag p) then "" else " (" <> tpTag p <> ")") <> (if c then " [configured]" else "")
        | (p, c) <- zip ps confs ]
      dflt = case [i | (i, True) <- zip [0 ..] confs] of
        (i : _) -> i
        [] -> 0
  pure (labels, dflt)

-- | @_configure_tool_category(ts_key, cat, config)@.
configureToolCategory :: ToolsEnv -> ToolCategory -> IO ()
configureToolCategory te cat = do
  let ui = teUI te
      sys = uiSys ui
  case tcProviders cat of
    [p] -> do
      sayLn ui ""
      sayLn ui (paint ui [cyan] ("  --- " <> tcIcon cat <> " " <> tcName cat <> " (" <> tpName p <> ") ---"))
      unless (T.null (tpTag p)) $ printInfo ui ("  " <> tpTag p)
      forM_ (tcSetupNote cat) $ \n -> printInfo ui ("  " <> n)
      configureProvider te p
    ps -> do
      sayLn ui ""
      let title = fromMaybe "Choose a provider" (tcSetupTitle cat)
      sayLn ui (paint ui [cyan] ("  --- " <> tcIcon cat <> " " <> tcName cat <> " - " <> title <> " ---"))
      forM_ (tcSetupNote cat) $ \n -> printInfo ui ("  " <> n)
      sayLn ui ""
      (labels, dflt) <- providerChoiceLabels sys ps
      idx <- toolsPromptChoice ui ("  " <> title <> ":") (labels <> ["Skip — keep defaults / configure later"]) dflt
      if idx >= length ps
        then printInfo ui ("  Skipped " <> tcName cat)
        else configureProvider te (ps !! idx)

-- | @_configure_toolset(ts_key, config)@ (no simple requirements exist).
configureToolset :: ToolsEnv -> Text -> IO ()
configureToolset te ts = forM_ (toolCategory ts) (configureToolCategory te)

-- | @_reconfigure_provider(provider, config)@.
reconfigureProvider :: ToolsEnv -> ToolProvider -> IO ()
reconfigureProvider te p = do
  let ui = teUI te
      sys = uiSys ui
  if null (tpEnv p)
    then sayLn ui (paint ui [green] ("✓   " <> tpName p <> " - no configuration needed!"))
    else forM_ (tpEnv p) $ \v -> do
      existing <- envValue sys (esKey v)
      unless (T.null existing) $ printInfo ui ("  " <> esKey v <> ": configured (" <> T.take 8 existing <> "...)")
      unless (T.null (esUrl v)) $ printInfo ui ("  Get yours at: " <> esUrl v)
      value <- toolsPrompt ui ("    " <> esPrompt v <> " (Enter to keep current)") Nothing True
      if not (T.null (T.strip value))
        then saveEnvValue sys (esKey v) (T.strip value) >> sayLn ui (paint ui [green] "✓     Updated")
        else printInfo ui "    Kept current"

-- | @_reconfigure_tool(config)@.
reconfigureTool :: ToolsEnv -> IORef PyVal -> IO ()
reconfigureTool te cref = do
  let ui = teUI te
      sys = uiSys ui
  configurable <- fmap concat . mapM (\(k, l, _) -> case toolCategory k of
                                         Just _ -> (\b -> [(k, l) | b]) <$> toolsetHasKeys sys k
                                         Nothing -> pure [])
    $ configurableToolsets
  if null configurable
    then printInfo ui "No configured tools to reconfigure."
    else do
      let choices = map snd configurable <> ["Cancel"]
      idx <- toolsPromptChoice ui "  Which tool would you like to reconfigure?" choices (length choices - 1)
      when (idx < length configurable) $ do
        let (k, _) = configurable !! idx
        forM_ (toolCategory k) $ \cat -> case tcProviders cat of
          [p] -> do
            sayLn ui ""
            sayLn ui (paint ui [cyan] ("  --- " <> tcIcon cat <> " " <> tcName cat <> " (" <> tpName p <> ") ---"))
            reconfigureProvider te p
          ps -> do
            sayLn ui ""
            sayLn ui (paint ui [cyan] ("  --- " <> tcIcon cat <> " " <> tcName cat <> " - Choose a provider ---"))
            sayLn ui ""
            (labels, dflt) <- providerChoiceLabels sys ps
            pidx <- toolsPromptChoice ui "  Select provider:" labels dflt
            reconfigureProvider te (ps !! pidx)
        readIORef cref >>= saveCfg sys

-- | @tools_command(first_install=..., config=config)@.
toolsCommand :: ToolsEnv -> Bool -> IORef PyVal -> IO ()
toolsCommand te firstInstall cref = do
  let ui = teUI te
      sys = uiSys ui
      total = length configurableToolsets
  enabled <- enabledPlatforms sys
  sayLn ui ""
  sayLn ui (paint ui [cyan, bold] "⚕ Fisher Tool Configuration")
  sayLn ui (paint ui [dim] "  Enable or disable tools per platform.")
  sayLn ui (paint ui [dim] "  Tools that need API keys will be configured when enabled.")
  sayLn ui ""
  if firstInstall
    then forM_ enabled $ \pkey -> do
      cfg <- readIORef cref
      let current = platformTools cfg pkey
          plabel = platformLabel pkey
      new <- toolsetChecklist ui plabel current
      forM_ (sort (Set.toList (new `Set.difference` current))) $ \ts -> sayLn ui (paint ui [green] ("  + " <> labelOf ts))
      forM_ (sort (Set.toList (current `Set.difference` new))) $ \ts -> sayLn ui (paint ui [red] ("  - " <> labelOf ts))
      let toConfigure = [ts | ts <- sort (Set.toList new), not (isNothing (toolCategory ts))]
      unless (null toConfigure) $ do
        sayLn ui ""
        sayLn ui (paint ui [yellow] ("  Configuring " <> showT (length toConfigure) <> " tool(s):"))
        forM_ toConfigure $ \ts -> sayLn ui (paint ui [dim] ("    • " <> labelOf ts))
        sayLn ui (paint ui [dim] "  You can skip any tool you don't need right now.")
        sayLn ui ""
        forM_ toConfigure (configureToolset te)
      savePlatformTools sys cref pkey new
      readIORef cref >>= saveCfg sys
      sayLn ui (paint ui [green] ("  ✓ Saved " <> plabel <> " tool configuration"))
      sayLn ui ""
    else do
      cfg0 <- readIORef cref
      let multi = length enabled > 1
          label cfg pkey = "Configure " <> platformLabel pkey <> "  (" <> showT (Set.size (platformTools cfg pkey)) <> "/" <> showT total <> " enabled)"
          globalIdx = if multi then length enabled else -1
          reconfigIdx = length enabled + (if multi then 1 else 0)
          doneIdx = reconfigIdx + 1
      choicesRef <- newIORef (map (label cfg0) enabled <> ["Configure all platforms (global)" | multi] <> ["Reconfigure an existing tool's provider or API key", "Done"])
      let configureNew prev new = do
            forM_ (sort (Set.toList (new `Set.difference` prev))) $ \ts ->
              when (not (isNothing (toolCategory ts))) $ do
                hasKeys <- toolsetHasKeys sys ts
                unless hasKeys (configureToolset te ts)
          loop = do
            choices <- readIORef choicesRef
            idx <- toolsPromptChoice ui "Select an option:" choices 0
            if idx == doneIdx
              then pure ()
              else if idx == reconfigIdx
                then reconfigureTool te cref >> sayLn ui "" >> loop
                else if idx == globalIdx
                  then do
                    cfg <- readIORef cref
                    let allCurrent = Set.unions [platformTools cfg pk | pk <- enabled]
                    new <- toolsetChecklist ui "All platforms" allCurrent
                    if new /= allCurrent
                      then do
                        forM_ enabled $ \pk -> do
                          cfgNow <- readIORef cref
                          let prev = platformTools cfgNow pk
                              added = new `Set.difference` prev
                              removed = prev `Set.difference` new
                          unless (Set.null added && Set.null removed) $ do
                            sayLn ui (paint ui [dim] ("  " <> platformLabel pk <> ":"))
                            forM_ (sort (Set.toList added)) $ \ts -> sayLn ui (paint ui [green] ("    + " <> labelOf ts))
                            forM_ (sort (Set.toList removed)) $ \ts -> sayLn ui (paint ui [red] ("    - " <> labelOf ts))
                          configureNew prev new
                          savePlatformTools sys cref pk new
                        readIORef cref >>= saveCfg sys
                        sayLn ui (paint ui [green] "  ✓ Saved configuration for all platforms")
                        cfgAfter <- readIORef cref
                        modifyIORef choicesRef (\cs -> map (label cfgAfter) enabled <> drop (length enabled) cs)
                      else sayLn ui (paint ui [dim] "  No changes")
                    sayLn ui ""
                    loop
                  else do
                    let pkey = enabled !! idx
                        plabel = platformLabel pkey
                    cfg <- readIORef cref
                    let current = platformTools cfg pkey
                    new <- toolsetChecklist ui plabel current
                    if new /= current
                      then do
                        forM_ (sort (Set.toList (new `Set.difference` current))) $ \ts -> sayLn ui (paint ui [green] ("  + " <> labelOf ts))
                        forM_ (sort (Set.toList (current `Set.difference` new))) $ \ts -> sayLn ui (paint ui [red] ("  - " <> labelOf ts))
                        configureNew current new
                        savePlatformTools sys cref pkey new
                        readIORef cref >>= saveCfg sys
                        sayLn ui (paint ui [green] ("  ✓ Saved " <> plabel <> " configuration"))
                      else sayLn ui (paint ui [dim] ("  No changes to " <> plabel))
                    sayLn ui ""
                    cfgAfter <- readIORef cref
                    modifyIORef choicesRef (\cs -> [if i == idx then label cfgAfter pkey else c | (i, c) <- zip [0 ..] cs])
                    loop
      loop
      sayLn ui ""
      sayLn ui (paint ui [dim] ("  Tool configuration saved to " <> homeLabelOf sys <> "/config.yaml"))
      sayLn ui (paint ui [dim] ("  Changes take effect on next '" <> cliName <> "' or gateway restart."))
      sayLn ui ""
