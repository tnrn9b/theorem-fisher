-- | The auxiliary model router (@agent/auxiliary_client.py@): which
-- provider, model and endpoint a side task (compression summaries, memory
-- flushes, MCP sampling) uses, the client cache, and the call itself over
-- OpenAI-compatible chat completions, the Codex Responses API or
-- Anthropic Messages, answered in the chat-completions shape.
--
-- Kept from upstream on purpose: the client cache is keyed without the
-- model, so the first caller's model sticks to the key (and an OpenRouter
-- slug is not dropped for a non-OpenRouter backend); see the README's
-- known differences for what Fisher changes.
module Fisher.Agent.Aux
  ( AuxEnv (..)
  , AuxState
  , newAuxState
  , Client (..)
  , ClientKind (..)
  , resolveTaskProviderModel
  , resolveProviderClient
  , callLlm
  , auxProviderAuxModels
  ) where

import Control.Exception (SomeException, try)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Fisher.LLM.Anthropic as An
import Fisher.LLM.Auth.Anthropic (resolveAnthropicToken)
import Fisher.LLM.Auth.Registry
import Fisher.LLM.Auth.Resolve (RuntimeRequest (..), resolveRuntimeProvider)
import Fisher.LLM.Auth.Store (loadAuthStore, loadProviderState)
import Fisher.LLM.Chat (fisherReferer)
import Fisher.LLM.Client (readEvents)
import qualified Fisher.LLM.Codex as Cx
import Fisher.LLM.Errors (errorText)
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Agent.Types
import Fisher.Py.Value

data ClientKind = OpenAICompat | CodexClient | AnthropicClient
  deriving (Eq, Show)

data Client = Client
  { clKind :: ClientKind
  , clBaseUrl :: Text
  , clApiKey :: Text
  , clHeaders :: [(Text, Text)]
  -- ^ the client's @default_headers@
  }
  deriving (Eq, Show)

-- | The process-wide state upstream keeps in module globals: the client
-- cache.
newtype AuxState = AuxState
  { asCache :: IORef [((Text, Text, Text), (Client, Maybe Text))]
  }

newAuxState :: IO AuxState
newAuxState = AuxState <$> newIORef []

data AuxEnv = AuxEnv
  { axSys :: Sys
  , axRuntime :: Runtime
  , axTransport :: Transport
  , axLoadConfig :: IO PyVal
  , axWarn :: Text -> IO ()
  , axState :: AuxState
  }

auxProviderAuxModels :: [(Text, Text)]
auxProviderAuxModels =
  [ ("zai", "glm-4.5-flash"), ("kimi-coding", "kimi-k2-turbo-preview"), ("minimax", "MiniMax-M2.5-highspeed")
  , ("minimax-cn", "MiniMax-M2.5-highspeed"), ("anthropic", "claude-haiku-4-5-20251001") ]

orHeaders :: [(Text, Text)]
orHeaders =
  [ ("http-referer", fisherReferer), ("x-openrouter-title", "Fisher Agent")
  , ("x-openrouter-categories", "productivity,cli-agent") ]

openRouterModel, codexAuxModel, codexAuxBaseUrl :: Text
openRouterModel = "google/gemini-3-flash-preview"
codexAuxModel = "gpt-5.2-codex"
codexAuxBaseUrl = "https://chatgpt.com/backend-api/codex"

nonEmptyT :: Text -> Maybe Text
nonEmptyT t = if T.null t then Nothing else Just t

envStrip :: AuxEnv -> Text -> IO Text
envStrip env k = maybe "" T.strip <$> sysGetEnv (axSys env) k

-- | @_get_auxiliary_env_override(task, suffix)@
envOverride :: AuxEnv -> Text -> Text -> IO (Maybe Text)
envOverride env task suffix = go ["AUXILIARY_", "CONTEXT_"]
  where
    go [] = pure Nothing
    go (p : ps) = do
      v <- envStrip env (p <> T.toUpper task <> "_" <> suffix)
      if T.null v then go ps else pure (Just v)

-- | @_get_auxiliary_provider(task)@
envProvider :: AuxEnv -> Text -> IO Text
envProvider env task = go ["AUXILIARY_", "CONTEXT_"]
  where
    go [] = pure "auto"
    go (p : ps) = do
      v <- T.toLower <$> envStrip env (p <> T.toUpper task <> "_PROVIDER")
      if not (T.null v) && v /= "auto" then pure v else go ps

-- | @_resolve_task_provider_model(task, provider, model, base_url, api_key)@
resolveTaskProviderModel :: AuxEnv -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO (Text, Maybe Text, Maybe Text, Maybe Text)
resolveTaskProviderModel env task provider model baseUrl apiKey = do
  (cfgProvider, cfgModel, cfgBase, cfgKey) <- case task of
    Nothing -> pure (Nothing, Nothing, Nothing, Nothing)
    Just t -> do
      cfg <- axLoadConfig env
      let tc = case getPath ["auxiliary", t] cfg of
            Just d@(PDict _) -> d
            _ -> PDict []
          field' k = nonEmptyT (T.strip (maybe "" pyStrOf (get k tc)))
          p0 = field' "provider"
          m0 = field' "model"
          comp = case get "compression" cfg of
            Just d@(PDict _) -> d
            _ -> PDict []
          (p1, m1) =
            if t == "compression" && p0 == Nothing
              then (nonEmptyT (T.strip (maybe "" pyStrOf (get "summary_provider" comp))), case m0 of
                     Just m -> Just m
                     Nothing -> nonEmptyT (T.strip (maybe "" pyStrOf (get "summary_model" comp))))
              else (p0, m0)
      pure (p1, m1, field' "base_url", field' "api_key")
  envModel <- maybe (pure Nothing) (\t -> envOverride env t "MODEL") task
  let resolvedModel = case model of
        Just m | not (T.null m) -> Just m
        _ -> case envModel of
          Just m -> Just m
          Nothing -> cfgModel
  case (baseUrl, provider) of
    (Just b, _) | not (T.null b) -> pure ("custom", resolvedModel, Just b, apiKey)
    (_, Just p) | not (T.null p) -> pure (p, resolvedModel, baseUrl, apiKey)
    _ -> case task of
      Nothing -> pure ("auto", resolvedModel, Nothing, Nothing)
      Just t -> do
        eb <- envOverride env t "BASE_URL"
        ek <- envOverride env t "API_KEY"
        case eb of
          Just b -> pure ("custom", resolvedModel, Just b, case ek of Just k -> Just k; Nothing -> cfgKey)
          Nothing -> do
            ep <- envProvider env t
            if ep /= "auto"
              then pure (ep, resolvedModel, Nothing, Nothing)
              else case (cfgBase, cfgProvider) of
                (Just b, _) -> pure ("custom", resolvedModel, Just b, cfgKey)
                (_, Just p) | p /= "auto" -> pure (p, resolvedModel, Nothing, Nothing)
                _ -> pure ("auto", resolvedModel, Nothing, Nothing)

-- ---------------------------------------------------------------- backends

openai :: Text -> Text -> [(Text, Text)] -> Client
openai = Client OpenAICompat

tryOpenRouter :: AuxEnv -> IO (Maybe (Client, Text))
tryOpenRouter env = do
  k <- sysGetEnv (axSys env) "OPENROUTER_API_KEY"
  pure $ case k of
    Just key | not (T.null key) -> Just (openai openrouterBaseUrl key orHeaders, openRouterModel)
    _ -> Nothing

-- | @_resolve_custom_runtime()@
customRuntime :: AuxEnv -> IO (Maybe (Text, Text))
customRuntime env = do
  r <- try @SomeException (resolveRuntimeProvider (axSys env) (RuntimeRequest (Just "custom") Nothing Nothing))
  pure $ case r of
    Right rt
      | Just (PStr b) <- get "base_url" rt, not (T.null (T.strip b))
      , Just (PStr k) <- get "api_key" rt, not (T.null (T.strip k)) ->
          let base = T.dropWhileEnd (== '/') (T.strip b)
           in if "openrouter.ai" `T.isInfixOf` T.toLower base then Nothing else Just (base, T.strip k)
    _ -> Nothing

-- | @_read_main_model()@
readMainModel :: AuxEnv -> IO Text
readMainModel env = do
  let go [] = do
        cfg <- try @SomeException (axLoadConfig env)
        pure $ case cfg of
          Right c -> case get "model" c of
            Just (PStr m) | not (T.null (T.strip m)) -> T.strip m
            Just d@(PDict _) -> T.strip (maybe "" pyStrOf (get "default" d))
            _ -> ""
          Left _ -> ""
      go (k : ks) = do
        v <- envStrip env k
        if T.null v then go ks else pure v
  go ["OPENAI_MODEL", "FISHER_MODEL", "LLM_MODEL"]

tryCustom :: AuxEnv -> IO (Maybe (Client, Text))
tryCustom env =
  customRuntime env >>= \case
    Nothing -> pure Nothing
    Just (base, key) -> do
      m <- readMainModel env
      pure (Just (openai base key [], if T.null m then "gpt-4o-mini" else m))

readCodexToken :: AuxEnv -> IO (Maybe Text)
readCodexToken env = do
  r <- try @SomeException (loadAuthStore (axSys env))
  pure $ case r of
    Right store
      | Just st <- loadProviderState store "openai-codex"
      , Just tokens@(PDict _) <- get "tokens" st
      , Just (PStr a) <- get "access_token" tokens, not (T.null (T.strip a))
      , Just (PStr rf) <- get "refresh_token" tokens, not (T.null (T.strip rf)) ->
          Just (T.strip a)
    _ -> Nothing

tryCodex :: AuxEnv -> IO (Maybe (Client, Text))
tryCodex env = fmap (\t -> (Client CodexClient codexAuxBaseUrl t [], codexAuxModel)) <$> readCodexToken env

tryAnthropic :: AuxEnv -> IO (Maybe (Client, Text))
tryAnthropic env = do
  t <- try @SomeException (resolveAnthropicToken (axSys env))
  pure $ case t of
    Right (Just tok) | not (T.null tok) -> Just (Client AnthropicClient "https://api.anthropic.com" tok [], "claude-haiku-4-5-20251001")
    _ -> Nothing

firstKey :: AuxEnv -> [Text] -> IO Text
firstKey _ [] = pure ""
firstKey env (k : ks) = do
  v <- envStrip env k
  if T.null v then firstKey env ks else pure v

-- | @_resolve_api_key_provider()@
apiKeyProvider :: AuxEnv -> IO (Maybe (Client, Text))
apiKeyProvider env = go [p | p <- providerRegistry, pcAuthType p == ApiKey]
  where
    go [] = pure Nothing
    go (p : ps) = do
      key <- firstKey env (pcApiKeyEnvVars p)
      if T.null key
        then go ps
        else
          if pcId p == "anthropic"
            then tryAnthropic env
            else do
              override <- if T.null (pcBaseUrlEnvVar p) then pure "" else T.dropWhileEnd (== '/') <$> envStrip env (pcBaseUrlEnvVar p)
              let base
                    | not (T.null override) = override
                    | pcId p == "kimi-coding" && "sk-kimi-" `T.isPrefixOf` key = "https://api.kimi.com/coding/v1"
                    | otherwise = pcInferenceBaseUrl p
                  hdrs = [("user-agent", "KimiCLI/1.0") | "api.kimi.com" `T.isInfixOf` T.toLower base]
              pure (Just (openai base key hdrs, fromMaybe "default" (lookup (pcId p) auxProviderAuxModels)))

firstOf :: [IO (Maybe a)] -> IO (Maybe a)
firstOf [] = pure Nothing
firstOf (a : as) = a >>= maybe (firstOf as) (pure . Just)

-- | @resolve_provider_client(provider, model, explicit_base_url,
-- explicit_api_key)@ (sync clients).
resolveProviderClient :: AuxEnv -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO (Maybe Client, Maybe Text)
resolveProviderClient env provider0 model explicitBase explicitKey = do
  let p1 = T.toLower (T.strip (if T.null provider0 then "auto" else provider0))
      provider = case p1 of
        "codex" -> "openai-codex"
        "main" -> "custom"
        x -> x
      done = \case
        Just (c, dflt) -> pure (Just c, Just (fromMaybe dflt (model >>= nonEmptyT)))
        Nothing -> pure (Nothing, Nothing)
      warnNone msg = axWarn env msg >> pure (Nothing, Nothing)
  case provider of
    "auto" ->
      firstOf [tryOpenRouter env, tryCustom env, tryCodex env, apiKeyProvider env] >>= \case
        Nothing -> pure (Nothing, Nothing)
        Just (c, resolved) ->
          let m = case model of
                Just x | "/" `T.isInfixOf` x && not ("/" `T.isInfixOf` resolved) -> Nothing
                x -> x >>= nonEmptyT
           in pure (Just c, Just (fromMaybe resolved m))
    "openrouter" -> tryOpenRouter env >>= maybe (warnNone "resolve_provider_client: openrouter requested but OPENROUTER_API_KEY not set") (done . Just)
    "openai-codex" -> tryCodex env >>= maybe (warnNone "resolve_provider_client: openai-codex requested but no Codex OAuth token found (run: fisher model)") (done . Just)
    "custom" -> case explicitBase of
      Just b | not (T.null b) -> do
        envKey <- envStrip env "OPENAI_API_KEY"
        let key = case explicitKey of
              Just k | not (T.null (T.strip k)) -> T.strip k
              _ -> envKey
            base = T.strip b
        if T.null base || T.null key
          then warnNone "resolve_provider_client: explicit custom endpoint requested but no API key was found (set explicit_api_key or OPENAI_API_KEY)"
          else do
            mm <- readMainModel env
            pure (Just (openai base key []), Just (fromMaybe (if T.null mm then "gpt-4o-mini" else mm) (model >>= nonEmptyT)))
      _ ->
        firstOf [tryCustom env, tryCodex env, apiKeyProvider env]
          >>= maybe (warnNone "resolve_provider_client: custom/main requested but no endpoint credentials found") (done . Just)
    other -> case lookupProvider other of
      Nothing | isRemovedProvider other -> warnNone ("resolve_provider_client: " <> removedProviderMessage)
      Nothing -> warnNone ("resolve_provider_client: unknown provider '" <> other <> "'")
      Just pc
        | pcAuthType pc == ApiKey ->
            if other == "anthropic"
              then tryAnthropic env >>= maybe (warnNone "resolve_provider_client: anthropic requested but no Anthropic credentials found") (done . Just)
              else do
                key <- firstKey env (pcApiKeyEnvVars pc)
                if T.null key
                  then warnNone ("resolve_provider_client: provider " <> other <> " has no API key configured (tried: " <> T.intercalate ", " (pcApiKeyEnvVars pc) <> ")")
                  else do
                    override <- if T.null (pcBaseUrlEnvVar pc) then pure "" else envStrip env (pcBaseUrlEnvVar pc)
                    let base =
                          if other == "kimi-coding"
                            then resolveKimiBaseUrl key (pcInferenceBaseUrl pc) override
                            else if T.null override then pcInferenceBaseUrl pc else override
                        hdrs = [("user-agent", "KimiCLI/1.0") | "api.kimi.com" `T.isInfixOf` T.toLower base]
                    pure (Just (openai base key hdrs), Just (fromMaybe (fromMaybe "" (lookup other auxProviderAuxModels)) (model >>= nonEmptyT)))
        | otherwise -> warnNone ("resolve_provider_client: OAuth provider " <> other <> " not directly supported, try 'auto'")

-- | @_get_cached_client(provider, model, base_url, api_key)@
cachedClient :: AuxEnv -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO (Maybe Client, Maybe Text)
cachedClient env provider model baseUrl apiKey = do
  let key = (provider, fromMaybe "" baseUrl, fromMaybe "" apiKey)
      ref = asCache (axState env)
  cache <- readIORef ref
  case lookup key cache of
    Just (c, dflt) -> pure (Just c, case model >>= nonEmptyT of Just m -> Just m; Nothing -> dflt)
    Nothing -> do
      (c, dflt) <- resolveProviderClient env provider model baseUrl apiKey
      case c of
        Just client -> modifyIORef ref (<> [(key, (client, dflt))])
        Nothing -> pure ()
      pure (c, case model >>= nonEmptyT of Just m -> Just m; Nothing -> dflt)

-- | @call_llm(task, provider, model, base_url, api_key, messages,
-- temperature, max_tokens, tools, timeout, extra_body)@: the response as
-- a chat-completions dict.
callLlm :: AuxEnv -> AuxCall -> IO (Either AuxError PyVal)
callLlm env call = do
  (rp, rm, rb, rk) <- resolveTaskProviderModel env (axTask call) (axProvider call) (axModel call) (axBaseUrl call) (axApiKey call)
  (c0, m0) <- cachedClient env rp rm rb rk
  (client, finalModel) <- case c0 of
    Nothing | rp /= "openrouter", rb == Nothing -> do
      axWarn env ("Provider " <> rp <> " unavailable, falling back to openrouter")
      cachedClient env "openrouter" (Just (fromMaybe openRouterModel rm)) Nothing Nothing
    _ -> pure (c0, m0)
  case client of
    Nothing -> pure (Left (AuxNoProvider ("No LLM provider configured for task=" <> fromMaybe "None" (axTask call) <> " provider=" <> rp <> ". Run: fisher setup")))
    Just cl -> do
      useCompletion <- case (rp, axMaxTokens call) of
        ("custom", Just _) -> do
          base <- case rb of
            Just b -> pure b
            Nothing -> maybe "" fst <$> customRuntime env
          pure ("api.openai.com" `T.isInfixOf` T.toLower base)
        _ -> pure False
      let model = fromMaybe PNull (PStr <$> finalModel)
          extra = axExtraBody call
          tokensKey = if useCompletion then "max_completion_tokens" else "max_tokens"
          body tk =
            [("messages", PList (axMessages call)), ("model", model)]
              <> [(tk, PInt n) | Just n <- [axMaxTokens call]]
              <> [("temperature", PFloat t) | Just t <- [axTemperature call]]
              <> [("tools", PList (axTools call)) | not (null (axTools call))]
              <> extra
      case clKind cl of
        OpenAICompat -> do
          r <- chat cl (body tokensKey)
          case r of
            Left e
              | "max_tokens" `T.isInfixOf` e || "unsupported_parameter" `T.isInfixOf` e -> do
                  let b = [(k, v) | (k, v) <- body tokensKey, k /= "max_tokens"]
                      b' = case lookup "max_completion_tokens" b of
                        Just _ -> b
                        Nothing -> insertAfterModel ("max_completion_tokens", maybe PNull PInt (axMaxTokens call)) b
                  either (pure . Left . AuxFailed) (pure . Right) =<< chat cl b'
              | otherwise -> pure (Left (AuxFailed e))
            Right v -> pure (Right v)
        AnthropicClient -> anthropic cl model
        CodexClient -> codex cl model
  where
    timeouts = Timeouts (axTimeout call) (axTimeout call)
    chat cl b = do
      let base = T.dropWhileEnd (== '/') (clBaseUrl cl)
          hdrs = [("accept", "application/json"), ("authorization", "Bearer " <> clApiKey cl), ("content-type", "application/json")] <> clHeaders cl
      r <- sdkSend (axRuntime env) (axTransport env) (HttpReq "POST" (base <> "/chat/completions") hdrs (Just (PDict b)) False) timeouts
      pure $ case r of
        Left e -> Left (errorText e)
        Right resp -> either (Left . T.pack) Right (loads (rBody resp))
    insertAfterModel kv kvs = case break ((== "model") . fst) kvs of
      (a, m : b) -> a <> [m, kv] <> b
      _ -> kvs <> [kv]
    anthropic cl model = do
      let s =
            defaultSettings
              { sMode = AnthropicMessages, sBaseUrl = clBaseUrl cl, sApiKey = clApiKey cl
              , sModel = case model of PStr m -> m; _ -> "claude-haiku-4-5-20251001"
              , sMaxTokens = Just (fromMaybe 2000 (axMaxTokens call))
              , sTools = if null (axTools call) then Nothing else Just (axTools call) }
          kw0 = An.buildKwargs s (axMessages call)
          kw = maybe kw0 (\t -> set "temperature" (PFloat t) kw0) (axTemperature call)
          base = T.dropWhileEnd (== '/') (clBaseUrl cl)
      r <- sdkSend (axRuntime env) (axTransport env) (HttpReq "POST" (base <> "/v1/messages") (An.anthropicHeaders (clApiKey cl)) (Just kw) False) anthropicTimeouts
      pure $ case r of
        Left e -> Left (AuxFailed (errorText e))
        Right resp -> case loads (rBody resp) of
          Left e -> Left (AuxFailed (T.pack e))
          Right v ->
            let (content, calls, _, finish) = An.normalizeResponse v
                u = fromMaybe (PDict []) (get "usage" v)
                n k = case get k u of
                  Just (PInt x) -> x
                  _ -> 0
             in Right $
                  PDict
                    [ ("choices", PList [PDict [("index", PInt 0), ("message", PDict [("role", PStr "assistant"), ("content", maybe PNull PStr content), ("tool_calls", if null calls then PNull else PList calls)]), ("finish_reason", PStr finish)]])
                    , ("model", model)
                    , ("usage", PDict [("prompt_tokens", PInt (n "input_tokens")), ("completion_tokens", PInt (n "output_tokens")), ("total_tokens", PInt (n "input_tokens" + n "output_tokens"))]) ]
    codex cl model = do
      let instructions = last ("You are a helpful assistant." : [pyStrOf c | m <- axMessages call, get "role" m == Just (PStr "system"), let c = fromMaybe PNull (get "content" m)])
          input =
            [ PDict [("role", fromMaybe PNull (get "role" m)), ("content", convert (fromMaybe PNull (get "content" m)))]
            | m <- axMessages call, get "role" m /= Just (PStr "system") ]
          tools =
            [ PDict [("type", PStr "function"), ("name", n), ("description", fromMaybe (PStr "") (get "description" fn)), ("parameters", fromMaybe (PDict []) (get "parameters" fn))]
            | t <- axTools call, let fn = fromMaybe t (get "function" t), Just n <- [get "name" fn], truthy n ]
          req =
            PDict $
              [ ("model", model), ("instructions", PStr instructions)
              , ("input", PList (if null input then [PDict [("role", PStr "user"), ("content", PStr "")]] else input))
              , ("store", PBool False) ]
                <> [("tools", PList tools) | not (null tools)]
                <> [("stream", PBool True)]
          base = T.dropWhileEnd (== '/') (clBaseUrl cl)
          hdrs = [("accept", "application/json"), ("authorization", "Bearer " <> clApiKey cl), ("content-type", "application/json")]
      r <- sdkStream (axRuntime env) (axTransport env) (HttpReq "POST" (base <> "/responses") hdrs (Just req) True) codexTimeouts readEvents
      pure $ case r of
        Left e -> Left (AuxFailed (errorText e))
        Right evs -> case Cx.accumulateStream evs of
          Cx.StreamError m -> Left (AuxFailed m)
          Cx.StreamNoCompleted -> Left (AuxFailed "Didn't receive a `response.completed` event.")
          Cx.StreamCompleted final ->
            let outs = case get "output" final of
                  Just (PList xs) -> xs
                  _ -> []
                texts = [maybe "" pyStrOf (get "text" p) | o <- outs, get "type" o == Just (PStr "message"), PList ps <- [fromMaybe (PList []) (get "content" o)], p <- ps, get "type" p `elem` [Just (PStr "output_text"), Just (PStr "text")]]
                calls =
                  [ PDict [("id", fromMaybe PNull (get "call_id" o)), ("type", PStr "function"), ("function", PDict [("name", fromMaybe PNull (get "name" o)), ("arguments", fromMaybe (PStr "{}") (get "arguments" o))])]
                  | o <- outs, get "type" o == Just (PStr "function_call") ]
                content = T.strip (T.concat texts)
                u = fromMaybe (PDict []) (get "usage" final)
                n k = fromMaybe (PInt 0) (get k u)
             in Right $
                  PDict
                    [ ("choices", PList [PDict [("index", PInt 0), ("message", PDict [("role", PStr "assistant"), ("content", if T.null content then PNull else PStr content), ("tool_calls", if null calls then PNull else PList calls)]), ("finish_reason", PStr (if null calls then "stop" else "tool_calls"))]])
                    , ("model", model)
                    , ("usage", PDict [("prompt_tokens", n "input_tokens"), ("completion_tokens", n "output_tokens"), ("total_tokens", n "total_tokens")]) ]
    convert = \case
      s@(PStr _) -> s
      PList parts ->
        let conv p = case get "type" p of
              Just (PStr "text") -> [PDict [("type", PStr "input_text"), ("text", fromMaybe (PStr "") (get "text" p))]]
              Just (PStr "image_url") ->
                let iu = fromMaybe PNull (get "image_url" p)
                 in case iu of
                      PDict _ -> [PDict ([("type", PStr "input_image"), ("image_url", fromMaybe (PStr "") (get "url" iu))] <> [("detail", d) | Just d <- [get "detail" iu]])]
                      v -> [PDict [("type", PStr "input_image"), ("image_url", PStr (pyStrOf v))]]
              Just (PStr t) | t `elem` ["input_text", "input_image"] -> [p]
              _ -> case get "text" p of
                Just t | truthy t -> [PDict [("type", PStr "input_text"), ("text", t)]]
                _ -> []
            out' = concat [conv p | p@(PDict _) <- parts]
         in if null out' then PStr "" else PList out'
      v | truthy v -> PStr (pyStrOf v)
      _ -> PStr ""
