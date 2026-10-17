-- | Which provider to use and with what credentials: @resolve_provider@,
-- the status dispatcher, API-key providers, named custom providers and
-- @resolve_runtime_provider@, whose result dict (in upstream's key order)
-- is what the agent is built from. Ported from @gauss_cli/auth.py@ and
-- @gauss_cli/runtime_provider.py@.
module Fisher.LLM.Auth.Resolve
  ( resolveProvider
  , getAuthStatus
  , getApiKeyProviderStatus
  , resolveApiKeyProviderCredentials
  , resolveRequestedProvider
  , normalizeCustomProviderName
  , getNamedCustomProvider
  , RuntimeRequest (..)
  , defaultRuntimeRequest
  , resolveRuntimeProvider
  , detectExternalCredentials
  ) where

import Control.Exception (SomeException, throwIO, try)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

import Fisher.LLM.Auth.Anthropic (resolveAnthropicToken)
import Fisher.LLM.Auth.Codex
import Fisher.LLM.Auth.Config (loadConfigPy, modelConfig)
import Fisher.LLM.Auth.Error
import Fisher.LLM.Auth.Nous
import Fisher.LLM.Auth.Registry
import Fisher.LLM.Auth.Store
import Fisher.LLM.Auth.Util
import Fisher.LLM.Sys
import Fisher.Py.Value

envTruthy :: Sys -> Text -> IO Bool
envTruthy sys k = maybe False (not . T.null) <$> sysGetEnv sys k

envStrip :: Sys -> Text -> IO Text
envStrip sys k = maybe "" T.strip <$> sysGetEnv sys k

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty = \case
  Just t | not (T.null t) -> Just t
  _ -> Nothing

-- | @resolve_provider(requested, explicit_api_key=, explicit_base_url=)@.
resolveProvider :: Sys -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
resolveProvider sys requested explicitKey explicitUrl = do
  let n0 = T.toLower (T.strip (fromMaybe "auto" (nonEmpty requested)))
      normalized = fromMaybe n0 (lookup n0 authProviderAliases)
  case () of
    _ | normalized `elem` ["openrouter", "custom"] -> pure "openrouter"
      | Just _ <- lookupProvider normalized -> pure normalized
      | normalized /= "auto" -> throwIO (AuthError ("Unknown provider '" <> normalized <> "'.") "" (Just "invalid_provider") False)
      | Just _ <- nonEmpty explicitKey -> pure "openrouter"
      | Just _ <- nonEmpty explicitUrl -> pure "openrouter"
      | otherwise -> do
          fromActive <- (try :: IO a -> IO (Either SomeException a)) $ do
            store <- loadAuthStore sys
            case get "active_provider" store of
              Just (PStr a) | not (T.null a), Just _ <- lookupProvider a -> do
                st <- getAuthStatus sys (Just a)
                pure (if maybe False truthy (get "logged_in" st) then Just a else Nothing)
              _ -> pure Nothing
          case fromActive of
            Right (Just a) -> pure a
            _ -> do
              openai <- envTruthy sys "OPENAI_API_KEY"
              orKey <- envTruthy sys "OPENROUTER_API_KEY"
              if openai || orKey
                then pure "openrouter"
                else scan [p | p <- providerRegistry, pcAuthType p == ApiKey]
  where
    scan [] = pure "openrouter"
    scan (p : ps) = do
      found <- or <$> mapM (fmap (not . T.null) . envStrip sys) (pcApiKeyEnvVars p)
      if found then pure (pcId p) else scan ps

-- | @get_auth_status(provider_id)@ (the active provider when none is
-- named).
getAuthStatus :: Sys -> Maybe Text -> IO PyVal
getAuthStatus sys mpid = do
  target <- case nonEmpty mpid of
    Just p -> pure (Just p)
    Nothing ->
      getActiveProvider sys >>= \case
        PStr a | not (T.null a) -> pure (Just a)
        _ -> pure Nothing
  case target of
    Just "nous" -> getNousAuthStatus sys
    Just "openai-codex" -> getCodexAuthStatus sys
    Just t | isApiKeyProvider t -> getApiKeyProviderStatus sys t
    _ -> pure (obj [("logged_in", PBool False)])

firstKey :: Sys -> [Text] -> IO (Text, Text)
firstKey _ [] = pure ("", "")
firstKey sys (v : vs) = do
  val <- envStrip sys v
  if T.null val then firstKey sys vs else pure (val, v)

-- | @get_api_key_provider_status@.
getApiKeyProviderStatus :: Sys -> Text -> IO PyVal
getApiKeyProviderStatus sys pid = case lookupProvider pid of
  Just pc | pcAuthType pc == ApiKey -> do
    (key, source) <- firstKey sys (pcApiKeyEnvVars pc)
    envUrl <- if T.null (pcBaseUrlEnvVar pc) then pure "" else envStrip sys (pcBaseUrlEnvVar pc)
    let base
          | pid == "kimi-coding" = resolveKimiBaseUrl key (pcInferenceBaseUrl pc) envUrl
          | not (T.null envUrl) = envUrl
          | otherwise = pcInferenceBaseUrl pc
    pure $
      obj
        [ ("configured", PBool (not (T.null key))), ("provider", str pid), ("name", str (pcName pc))
        , ("key_source", str source), ("base_url", str base), ("logged_in", PBool (not (T.null key))) ]
  _ -> pure (obj [("configured", PBool False)])

-- | @resolve_api_key_provider_credentials(provider_id)@: no key is not an
-- error (the key is then empty and the source @default@).
resolveApiKeyProviderCredentials :: Sys -> Text -> IO PyVal
resolveApiKeyProviderCredentials sys pid = case lookupProvider pid of
  Just pc | pcAuthType pc == ApiKey -> do
    (key, source) <- firstKey sys (pcApiKeyEnvVars pc)
    envUrl <- if T.null (pcBaseUrlEnvVar pc) then pure "" else envStrip sys (pcBaseUrlEnvVar pc)
    let base
          | pid == "kimi-coding" = resolveKimiBaseUrl key (pcInferenceBaseUrl pc) envUrl
          | not (T.null envUrl) = rstripSlash envUrl
          | otherwise = pcInferenceBaseUrl pc
    pure $
      obj
        [ ("provider", str pid), ("api_key", str key), ("base_url", str (rstripSlash base))
        , ("source", str (if T.null source then "default" else source)) ]
  _ -> throwIO (AuthError ("Provider '" <> pid <> "' is not an API-key provider.") pid (Just "invalid_provider") False)

-- | @resolve_requested_provider(requested)@: the argument, then
-- @model.provider@ in the config, then @GAUSS_INFERENCE_PROVIDER@, then
-- @auto@ (all lower-cased).
resolveRequestedProvider :: Sys -> Maybe Text -> IO Text
resolveRequestedProvider sys requested = case requested of
  Just r | not (T.null (T.strip r)) -> pure (T.toLower (T.strip r))
  _ -> do
    cfg <- loadConfigPy sys
    case get "provider" (modelConfig cfg) of
      Just (PStr p) | not (T.null (T.strip p)) -> pure (T.toLower (T.strip p))
      _ -> do
        e <- T.toLower <$> envStrip sys "GAUSS_INFERENCE_PROVIDER"
        pure (if T.null e then "auto" else e)

-- | @_normalize_custom_provider_name@.
normalizeCustomProviderName :: Text -> Text
normalizeCustomProviderName = T.replace " " "-" . T.toLower . T.strip

-- | @_get_named_custom_provider@: the @custom_providers@ entry the name
-- (or @custom:<name>@) picks, as @(name, base_url, api_key)@. A bare name
-- that is a built-in provider or alias never matches.
getNamedCustomProvider :: Sys -> Text -> IO (Maybe (Text, Text, Text))
getNamedCustomProvider sys requested = do
  let norm = normalizeCustomProviderName requested
  builtin <-
    if T.null norm || norm == "custom" || norm == "auto" || "custom:" `T.isPrefixOf` norm
      then pure False
      else either (\(_ :: AuthError) -> False) (const True) <$> try (resolveProvider sys (Just norm) Nothing Nothing)
  if T.null norm || norm == "custom" || norm == "auto" || builtin
    then pure Nothing
    else do
      cfg <- loadConfigPy sys
      pure $ case get "custom_providers" cfg of
        Just (PList entries) ->
          case [ (T.strip name, T.strip base, apiKeyOf e)
               | e@(PDict _) <- entries
               , Just (PStr name) <- [get "name" e]
               , Just (PStr base) <- [get "base_url" e]
               , let nn = normalizeCustomProviderName name
               , norm `elem` [nn, "custom:" <> nn] ] of
            (x : _) -> Just x
            [] -> Nothing
        _ -> Nothing
  where
    apiKeyOf e = case get "api_key" e of
      Just v | truthy v -> T.strip (pyStrOf v)
      _ -> ""

-- | The keyword arguments of @resolve_runtime_provider@.
data RuntimeRequest = RuntimeRequest
  { rrRequested :: Maybe Text
  , rrExplicitApiKey :: Maybe Text
  , rrExplicitBaseUrl :: Maybe Text
  }
  deriving (Eq, Show)

defaultRuntimeRequest :: RuntimeRequest
defaultRuntimeRequest = RuntimeRequest Nothing Nothing Nothing

-- | @resolve_runtime_provider(requested=, explicit_api_key=,
-- explicit_base_url=)@: the runtime dict (@provider@, @api_mode@,
-- @base_url@, @api_key@, @source@, then per provider @expires_at@ or
-- @last_refresh@, and @requested_provider@). Raises 'AuthError' (and,
-- where upstream lets them through, 'PyError').
resolveRuntimeProvider :: Sys -> RuntimeRequest -> IO PyVal
resolveRuntimeProvider sys req = do
  requested <- resolveRequestedProvider sys (rrRequested req)
  custom <- namedCustomRuntime requested
  case custom of
    Just rt -> pure (set "requested_provider" (str requested) rt)
    Nothing -> do
      provider <- resolveProvider sys (Just requested) (rrExplicitApiKey req) (rrExplicitBaseUrl req)
      let tail' kvs = obj (kvs <> [("requested_provider", str requested)])
      case provider of
        "nous" -> do
          ttlEnv <- fromMaybe "1800" <$> sysGetEnv sys "GAUSS_NOUS_MIN_KEY_TTL_SECONDS"
          ttl <- maybe (throwIO (intError ttlEnv)) pure (pyIntText ttlEnv)
          toEnv <- fromMaybe "15" <$> sysGetEnv sys "GAUSS_NOUS_TIMEOUT_SECONDS"
          to <- maybe (throwIO (pyError "ValueError" ("could not convert string to float: " <> pyRepr (str toEnv)))) pure (pyFloatText toEnv)
          creds <- resolveNousRuntimeCredentials sys defaultNousOpts {noMinKeyTtl = fromInteger (max 60 ttl), noTimeout = to}
          pure $ tail'
            [ ("provider", str "nous"), ("api_mode", str "chat_completions")
            , ("base_url", str (rstripSlash (strOr "" (get "base_url" creds))))
            , ("api_key", fromMaybe (str "") (get "api_key" creds))
            , ("source", fromMaybe (str "portal") (get "source" creds))
            , ("expires_at", fromMaybe PNull (get "expires_at" creds)) ]
        "openai-codex" -> do
          creds <- resolveCodexRuntimeCredentials sys defaultCodexOpts
          pure $ tail'
            [ ("provider", str "openai-codex"), ("api_mode", str "codex_responses")
            , ("base_url", str (rstripSlash (strOr "" (get "base_url" creds))))
            , ("api_key", fromMaybe (str "") (get "api_key" creds))
            , ("source", fromMaybe (str "gauss-auth-store") (get "source" creds))
            , ("last_refresh", fromMaybe PNull (get "last_refresh" creds)) ]
        "anthropic" -> do
          tok <- resolveAnthropicToken sys
          case tok of
            Just t | not (T.null t) ->
              pure $ tail'
                [ ("provider", str "anthropic"), ("api_mode", str "anthropic_messages")
                , ("base_url", str "https://api.anthropic.com"), ("api_key", str t), ("source", str "env") ]
            _ ->
              throwIO . authError $
                "No Anthropic credentials found. Set ANTHROPIC_TOKEN or ANTHROPIC_API_KEY, "
                  <> "run 'claude setup-token', or authenticate with 'claude /login'."
        p | isApiKeyProvider p -> do
          creds <- resolveApiKeyProviderCredentials sys p
          pure $ tail'
            [ ("provider", str p), ("api_mode", str "chat_completions")
            , ("base_url", str (rstripSlash (strOr "" (get "base_url" creds))))
            , ("api_key", fromMaybe (str "") (get "api_key" creds))
            , ("source", fromMaybe (str "env") (get "source" creds)) ]
        _ -> set "requested_provider" (str requested) <$> openrouterRuntime requested
  where
    intError v = pyError "ValueError" ("invalid literal for int() with base 10: " <> pyRepr (str v))
    explicitKey = rrExplicitApiKey req
    explicitUrl = rrExplicitBaseUrl req
    namedCustomRuntime requested =
      getNamedCustomProvider sys requested >>= \case
        Nothing -> pure Nothing
        Just (name, base0, key0) -> do
          let base = rstripSlash (fromMaybe base0 (nonEmpty (T.strip <$> explicitUrl)))
          if T.null base
            then pure Nothing
            else do
              openai <- envStrip sys "OPENAI_API_KEY"
              orKey <- envStrip sys "OPENROUTER_API_KEY"
              let key = fromMaybe "" (orText [T.strip <$> explicitKey, Just key0, Just openai, Just orKey])
              pure . Just $
                obj
                  [ ("provider", str "openrouter"), ("api_mode", str "chat_completions"), ("base_url", str base)
                  , ("api_key", str key), ("source", str ("custom_provider:" <> name)) ]
    openrouterRuntime requested = do
      cfg <- loadConfigPy sys
      let model = modelConfig cfg
          cfgBase = case get "base_url" model of Just (PStr s) -> s; _ -> ""
          cfgProvider = T.toLower (T.strip (case get "provider" model of Just (PStr s) -> s; _ -> ""))
          reqNorm = T.toLower (T.strip requested)
      envOpenaiBase <- envStrip sys "OPENAI_BASE_URL"
      envOrBase <- envStrip sys "OPENROUTER_BASE_URL"
      let useCfg =
            not (T.null (T.strip cfgBase)) && null (nonEmpty explicitUrl) && T.null envOpenaiBase
              && ( (reqNorm == "auto" && (T.null cfgProvider || cfgProvider == "auto"))
                     || (reqNorm == "custom" && cfgProvider == "custom") )
          skipOpenaiBase = reqNorm == "openrouter"
          base =
            rstripSlash . fromMaybe openrouterBaseUrl $
              orText
                [ T.strip <$> explicitUrl
                , if skipOpenaiBase then Nothing else Just envOpenaiBase
                , if useCfg then Just (T.strip cfgBase) else Nothing
                , Just envOrBase ]
      orKey <- nonEmpty <$> sysGetEnv sys "OPENROUTER_API_KEY"
      openaiKey <- nonEmpty <$> sysGetEnv sys "OPENAI_API_KEY"
      let key =
            fromMaybe "" $
              if "openrouter.ai" `T.isInfixOf` base
                then orText [explicitKey, orKey, openaiKey]
                else orText [explicitKey, openaiKey, orKey]
          source = if null (orText [explicitKey, explicitUrl]) then "env/config" else "explicit"
      pure $
        obj
          [ ("provider", str "openrouter"), ("api_mode", str "chat_completions"), ("base_url", str base)
          , ("api_key", str key), ("source", str source) ]

-- | @detect_external_credentials@: the Codex CLI's tokens, if any.
detectExternalCredentials :: Sys -> IO [PyVal]
detectExternalCredentials sys =
  importCodexCliTokens sys >>= \case
    Nothing -> pure []
    Just _ -> do
      let path = T.pack (sysHome sys </> ".codex" </> "auth.json")
      pure
        [ obj
            [ ("provider", str "openai-codex"), ("path", str path)
            , ("label", str ("Codex CLI credentials found (" <> path <> ") — run `" <> cliName <> " model` to create a separate session")) ] ]
