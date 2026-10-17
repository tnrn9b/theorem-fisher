-- | @gauss status [--all] [--deep]@: every section upstream prints, from
-- the environment and keys through the auth providers, terminal backend,
-- messaging, gateway service, scheduled jobs and sessions. Ported from
-- @gauss_cli/status.py@. Like upstream it is not read-only: loading the
-- config makes the home, and the Codex check takes the auth lock and may
-- migrate or refresh Codex tokens.
module Fisher.Cli.Status
  ( StatusInfo (..)
  , statusMain
  , showStatus
  , sysStyle
  , gatewayServiceName
  , redactKey
  , formatIsoTimestamp
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath ((</>))

import Fisher.Cli.AuthArgs
import Fisher.LLM.Auth.Codex (getCodexAuthStatus)
import Fisher.LLM.Auth.Config (loadConfigPy)
import Fisher.LLM.Auth.Env (getEnvValue, loadDotenvIntoSys)
import Fisher.LLM.Auth.Error (AuthError, cliName)
import Fisher.LLM.Auth.Nous (getNousAuthStatus)
import Fisher.LLM.Auth.Registry (openrouterModelsUrl, providerLabel)
import Fisher.LLM.Auth.Resolve (resolveProvider, resolveRequestedProvider)
import Fisher.LLM.Auth.Sha256 (sha256Hex)
import Fisher.LLM.Auth.Util (Client (..), formatLocalTime, parseIsoTimestamp, send)
import Fisher.LLM.Sys
import Fisher.Paths (envPathIn)
import Fisher.Py.Value
import Fisher.Term

-- | What upstream reads from its own install: the checkout it runs from
-- and the Python version. Fisher passes its own.
data StatusInfo = StatusInfo
  { siProject :: Text
  , siPython :: Text
  }
  deriving (Eq, Show)

-- | @gauss status ...@ from its arguments: the home's @.env@ loaded as
-- the CLI does at start-up, then argparse, then the report.
statusMain :: StatusInfo -> Sys -> [Text] -> IO Int
statusMain info sys args = do
  loadDotenvIntoSys sys
  case parseArgs statusParser args of
    Left (msg, code) -> sysOut sys msg >> pure code
    Right opts -> showStatus info sys (flagSet "all" opts) (flagSet "deep" opts)

-- | @colors.supports_ansi@ / @supports_unicode@, from the environment
-- 'Sys' gives. The output encoding is taken from the locale variables
-- (an unset, C or POSIX locale is UTF-8, as Python's coercion makes it).
sysStyle :: Sys -> IO Style
sysStyle sys = do
  let envT k = sysGetEnv sys k
      truthyEnv k = maybe False ((`elem` ["1", "true", "yes", "on"]) . T.toLower . T.strip) <$> envT k
  forcePlain <- truthyEnv "GAUSS_FORCE_PLAIN_OUTPUT"
  noColor <- envT "NO_COLOR"
  tty <- sysStdoutTty sys
  term <- maybe "" (T.toLower . T.strip) <$> envT "TERM"
  forceAscii <- truthyEnv "GAUSS_FORCE_ASCII"
  ioEnc <- envT "PYTHONIOENCODING"
  locs <- mapM envT ["LC_ALL", "LC_CTYPE", "LANG"]
  let loc = case [l | Just l <- locs, not (T.null l)] of
        (l : _) -> l
        [] -> "C"
      utf = case ioEnc of
        Just e | not (T.null e) -> "utf" `T.isInfixOf` T.toLower e
        _ -> loc `elem` ["C", "POSIX"] || "utf" `T.isInfixOf` T.toLower loc
  pure (Style (not forcePlain && not (isJust noColor) && tty && term /= "dumb") (not forceAscii && utf))

-- | @redact_key@.
redactKey :: Text -> Text
redactKey k
  | T.null k = "(not set)"
  | T.length k < 12 = "***"
  | otherwise = T.take 4 k <> "..." <> T.takeEnd 4 k

-- | @_format_iso_timestamp@: in the local zone, @(unknown)@ when empty,
-- the value itself when it does not parse.
formatIsoTimestamp :: Sys -> Maybe PyVal -> IO Text
formatIsoTimestamp sys = \case
  Just v@(PStr s) | not (T.null s) ->
    if T.null (T.strip s)
      then pure "(unknown)"
      else case parseIsoTimestamp v of
        Nothing -> pure s
        Just t -> (`formatLocalTime` t) <$> sysTimeZoneAt sys t
  _ -> pure "(unknown)"

-- | @gateway.get_service_name()@: @gauss-gateway@ for the default home,
-- else suffixed with the first 8 hex digits of the resolved home's SHA-256.
gatewayServiceName :: Sys -> IO Text
gatewayServiceName sys = do
  env <- sysGetEnv sys "GAUSS_HOME"
  let dflt = sysHome sys </> ".gauss"
  home <- canonicalizePath (maybe dflt T.unpack env)
  dflt' <- canonicalizePath dflt
  pure $
    if home == dflt'
      then "gauss-gateway"
      else "gauss-gateway-" <> T.take 8 (sha256Hex (TE.encodeUtf8 (T.pack home)))

readJsonFile :: FilePath -> IO (Maybe (Either () PyVal))
readJsonFile p = do
  exists <- doesFileExist p
  if not exists
    then pure Nothing
    else do
      r <- try (BS.readFile p)
      pure . Just $ case r of
        Left (_ :: SomeException) -> Left ()
        Right b -> either (const (Left ())) Right (loads b)

-- | @show_status(args)@.
showStatus :: StatusInfo -> Sys -> Bool -> Bool -> IO Int
showStatus info sys showAll deep = do
  st <- sysStyle sys
  let p = outLn sys
      c codes t = color st codes t
      check ok = if ok then c [green] "✓" else c [red] "✗"
      header t = p "" >> p (c [cyan, bold] ("◆ " <> t))
      title = "Gauss Status" :: Text
      centred = let w = 55; n = T.length title; l = (w - n) `div` 2 in T.replicate l " " <> title <> T.replicate (w - n - l) " "
      envOr k = fromMaybe "" <$> sysGetEnv sys k
      valueOr k = fromMaybe "" <$> getEnvValue sys k
  p ""
  p (c [cyan] "┌─────────────────────────────────────────────────────────┐")
  p (c [cyan] ("│ " <> centred <> " │"))
  p (c [cyan] "└─────────────────────────────────────────────────────────┘")

  header "Environment"
  p ("  Project:      " <> siProject info)
  p ("  Python:       " <> siPython info)
  envExists <- doesFileExist (envPathIn (sysGaussHome sys))
  p ("  .env file:    " <> check envExists <> " " <> (if envExists then "exists" else "not found"))
  cfg <- either (\(_ :: SomeException) -> PDict []) id <$> try (loadConfigPy sys)
  let modelLabel = case get "model" cfg of
        Just m@(PDict _) ->
          let pick = case get "default" m of
                Just v | truthy v -> pyStrOf v
                _ -> case get "name" m of
                  Just v | truthy v -> pyStrOf v
                  _ -> ""
           in T.strip pick
        Just (PStr s) -> T.strip s
        _ -> ""
  p ("  Model:        " <> (if T.null modelLabel then "(not set)" else modelLabel))
  providerText <- effectiveProviderLabel sys
  p ("  Provider:     " <> providerText)

  header "API Keys"
  let keyRows =
        [ ("OpenRouter", "OPENROUTER_API_KEY"), ("OpenAI", "OPENAI_API_KEY"), ("Z.AI/GLM", "GLM_API_KEY")
        , ("Kimi", "KIMI_API_KEY"), ("MiniMax", "MINIMAX_API_KEY"), ("MiniMax-CN", "MINIMAX_CN_API_KEY")
        , ("Firecrawl", "FIRECRAWL_API_KEY"), ("Browserbase", "BROWSERBASE_API_KEY"), ("FAL", "FAL_KEY")
        , ("Tinker", "TINKER_API_KEY"), ("WandB", "WANDB_API_KEY"), ("ElevenLabs", "ELEVENLABS_API_KEY")
        , ("GitHub", "GITHUB_TOKEN") ]
      keyLine name v = p ("  " <> T.justifyLeft 12 ' ' name <> "  " <> check (not (T.null v)) <> " " <> (if showAll then v else redactKey v))
  forM_ keyRows $ \(name, var) -> valueOr var >>= keyLine name
  tok <- valueOr "ANTHROPIC_TOKEN"
  anthropicValue <- if T.null tok then valueOr "ANTHROPIC_API_KEY" else pure tok
  keyLine "Anthropic" anthropicValue

  header "Auth Providers"
  statuses <- try ((,) <$> getNousAuthStatus sys <*> getCodexAuthStatus sys)
  let (nous, codex) = either (\(_ :: SomeException) -> (PDict [], PDict [])) id statuses
      flag k d = maybe False truthy (get k d)
      notLoggedIn = "not logged in (run: " <> cliName <> " model)"
      nousIn = flag "logged_in" nous
  p ("  " <> T.justifyLeft 12 ' ' "Nous Portal" <> "  " <> check nousIn <> " " <> (if nousIn then "logged in" else notLoggedIn))
  when nousIn $ do
    let portal = case get "portal_base_url" nous of
          Just v | truthy v -> pyStrOf v
          _ -> "(unknown)"
    accessExp <- formatIsoTimestamp sys (get "access_expires_at" nous)
    keyExp <- formatIsoTimestamp sys (get "agent_key_expires_at" nous)
    p ("    Portal URL: " <> portal)
    p ("    Access exp: " <> accessExp)
    p ("    Key exp:    " <> keyExp)
    p ("    Refresh:    " <> (if flag "has_refresh_token" nous then "yes" else "no"))
  let codexIn = flag "logged_in" codex
  p ("  " <> T.justifyLeft 12 ' ' "OpenAI Codex" <> "  " <> check codexIn <> " " <> (if codexIn then "logged in" else notLoggedIn))
  forM_ (get "auth_store" codex) $ \v -> when (truthy v) (p ("    Auth file:  " <> pyStrOf v))
  refreshed <- formatIsoTimestamp sys (get "last_refresh" codex)
  when (flag "last_refresh" codex) (p ("    Refreshed:  " <> refreshed))
  when (flag "error" codex && not codexIn) (p ("    Error:      " <> maybe "" pyStrOf (get "error" codex)))

  header "API-Key Providers"
  let apiKeyProviders =
        [ ("Z.AI / GLM", ["GLM_API_KEY", "ZAI_API_KEY", "Z_AI_API_KEY"]), ("Kimi / Moonshot", ["KIMI_API_KEY"])
        , ("MiniMax", ["MINIMAX_API_KEY"]), ("MiniMax (China)", ["MINIMAX_CN_API_KEY"]) ]
      firstValue [] = pure ""
      firstValue (v : vs) = valueOr v >>= \x -> if T.null x then firstValue vs else pure x
  forM_ apiKeyProviders $ \(pname, vars) -> do
    configured <- not . T.null <$> firstValue vars
    p ("  " <> T.justifyLeft 16 ' ' pname <> " " <> check configured <> " "
         <> (if configured then "configured" else "not configured (run: " <> cliName <> " model)"))

  header "Terminal Backend"
  termEnv <- envOr "TERMINAL_ENV"
  backend <-
    if not (T.null termEnv)
      then pure termEnv
      else do
        r <- try (loadConfigPy sys)
        pure $ case r of
          Left (_ :: SomeException) -> "local"
          Right cf -> case get "terminal" cf of
            Just t@(PDict _) -> maybe "local" pyStrOf (get "backend" t)
            _ -> "local"
  p ("  Backend:      " <> backend)
  case backend of
    "ssh" -> do
      h <- envOr "TERMINAL_SSH_HOST"
      u <- envOr "TERMINAL_SSH_USER"
      p ("  SSH Host:     " <> (if T.null h then "(not set)" else h))
      p ("  SSH User:     " <> (if T.null u then "(not set)" else u))
    "docker" -> do
      img <- fromMaybe "python:3.11-slim" <$> sysGetEnv sys "TERMINAL_DOCKER_IMAGE"
      p ("  Docker Image: " <> img)
    "daytona" -> do
      img <- fromMaybe "nikolaik/python-nodejs:python3.11-nodejs20" <$> sysGetEnv sys "TERMINAL_DAYTONA_IMAGE"
      p ("  Daytona Image: " <> img)
    _ -> pure ()
  sudo <- envOr "SUDO_PASSWORD"
  p ("  Sudo:         " <> check (not (T.null sudo)) <> " " <> (if T.null sudo then "disabled" else "enabled"))

  header "Messaging Platforms"
  let platforms =
        [ ("Telegram", "TELEGRAM_BOT_TOKEN", Just "TELEGRAM_HOME_CHANNEL"), ("Discord", "DISCORD_BOT_TOKEN", Just "DISCORD_HOME_CHANNEL")
        , ("WhatsApp", "WHATSAPP_ENABLED", Nothing), ("Signal", "SIGNAL_HTTP_URL", Just "SIGNAL_HOME_CHANNEL")
        , ("Slack", "SLACK_BOT_TOKEN", Nothing), ("Email", "EMAIL_ADDRESS", Just "EMAIL_HOME_ADDRESS") ]
  forM_ platforms $ \(name, tokVar, homeVar) -> do
    t <- envOr tokVar
    home <- maybe (pure "") envOr homeVar
    let status0 = if T.null t then "not configured" else "configured"
        status = if T.null home then status0 else status0 <> " (home: " <> home <> ")"
    p ("  " <> T.justifyLeft 12 ' ' name <> "  " <> check (not (T.null t)) <> " " <> status)

  header "Gateway Service"
  gateway <-
    if "linux" `T.isPrefixOf` sysPlatform sys
      then do
        svc <- either (\(_ :: SomeException) -> "gauss-gateway") id <$> try (gatewayServiceName sys)
        sysRun sys ["systemctl", "--user", "is-active", svc] >>= \case
          Left err -> pure (Left ("FileNotFoundError: " <> err <> ": 'systemctl'"))
          Right r -> do
            let active = T.strip (rrStdout r) == "active"
            p ("  Status:       " <> check active <> " " <> (if active then "running" else "stopped"))
            p "  Manager:      systemd (user)"
            pure (Right ())
      else
        if sysPlatform sys == "darwin"
          then
            sysRun sys ["launchctl", "list", "ai.gauss.gateway"] >>= \case
              Left err -> pure (Left ("FileNotFoundError: " <> err <> ": 'launchctl'"))
              Right r -> do
                let loaded = rrCode r == 0
                p ("  Status:       " <> check loaded <> " " <> (if loaded then "loaded" else "not loaded"))
                p "  Manager:      launchd"
                pure (Right ())
          else do
            p ("  Status:       " <> c [dim] "N/A")
            p "  Manager:      (not supported on this platform)"
            pure (Right ())
  case gateway of
    Left exc -> do
      -- Upstream does not catch a missing systemctl/launchctl: the
      -- traceback ends the command with status 1.
      p "Traceback (most recent call last):"
      p "  File \"gauss_cli/status.py\", line 286, in show_status"
      p "    result = subprocess.run("
      p exc
      pure 1
    Right () -> do
      header "Scheduled Jobs"
      jobs <- readJsonFile (sysGaussHome sys </> "cron" </> "jobs.json")
      p $ "  Jobs:         " <> case jobs of
        Nothing -> "0"
        Just (Right d@(PDict _)) -> case fromMaybe (PList []) (get "jobs" d) of
          PList js | all isDict js ->
            let enabled = [j | j <- js, maybe True truthy (get "enabled" j)]
             in tshow (length enabled) <> " active, " <> tshow (length js) <> " total"
          PDict [] -> "0 active, 0 total"
          PStr "" -> "0 active, 0 total"
          _ -> "(error reading jobs file)"
        _ -> "(error reading jobs file)"

      header "Sessions"
      sessions <- readJsonFile (sysGaussHome sys </> "sessions" </> "sessions.json")
      p $ "  Active:       " <> case sessions of
        Nothing -> "0"
        Just (Right (PDict kvs)) -> tshow (length kvs) <> " session(s)"
        Just (Right (PList xs)) -> tshow (length xs) <> " session(s)"
        Just (Right (PStr s)) -> tshow (T.length s) <> " session(s)"
        _ -> "(error reading sessions file)"

      when deep $ do
        header "Deep Checks"
        orKey <- envOr "OPENROUTER_API_KEY"
        when (not (T.null orKey)) $ do
          r <- try (send sys (Client ViaHttpx [] 10) "GET" openrouterModelsUrl [("Authorization", "Bearer " <> orKey)] NoBody)
          case r of
            Right resp -> do
              let ok = aStatus resp == 200
              p ("  OpenRouter:   " <> check ok <> " " <> (if ok then "reachable" else "error (" <> tshow (aStatus resp) <> ")"))
            Left (e :: SomeException) -> p ("  OpenRouter:   " <> check False <> " error: " <> T.pack (show e))
      -- The 127.0.0.1:18789 port probe of --deep needs a socket, which
      -- 'Sys' does not offer; it is left out.

      p ""
      p (c [dim] (T.replicate 60 "─"))
      p (c [dim] ("  Run '" <> cliName <> " doctor' for detailed diagnostics"))
      p (c [dim] ("  Run '" <> cliName <> " setup' to configure"))
      p ""
      pure 0
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show
    isDict = \case
      PDict _ -> True
      _ -> False

-- | @_effective_provider_label@: what runtime resolution would pick, with
-- OpenRouter shown as a custom endpoint when @OPENAI_BASE_URL@ is set.
effectiveProviderLabel :: Sys -> IO Text
effectiveProviderLabel sys = do
  requested <- resolveRequestedProvider sys Nothing
  r <- try (resolveProvider sys (Just requested) Nothing Nothing)
  let effective0 = case r of
        Right e -> e
        Left (_ :: AuthError) -> if T.null requested then "auto" else requested
  base <- getEnvValue sys "OPENAI_BASE_URL"
  let effective = if effective0 == "openrouter" && maybe False (not . T.null) base then "custom" else effective0
  pure (providerLabel (Just effective))
