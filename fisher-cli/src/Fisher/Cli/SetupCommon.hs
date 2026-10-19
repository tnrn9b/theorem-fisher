-- | What @fisher setup@ and @fisher model@ share beyond the credential layer:
-- the merged configuration as Python holds it (a dict in insertion order,
-- ints and floats apart) and its save; the model catalogues of
-- @gauss_cli/models.py@ and its @/models@ probes; and
-- @agent.auxiliary_client.get_available_vision_backends@.
module Fisher.Cli.SetupCommon
  ( -- * Configuration
    loadCfg
  , saveCfg
  , getS
  , getD
  , modelConfigDict
  , setModelProvider
  , setDefaultModel
  , envValue
  , envValueCompat
  , homeLabelOf
    -- * Catalogues
  , openrouterModels
  , modelIds
  , menuLabels
  , providerModels
    -- * Probes
  , Probe (..)
  , probeApiModels
  , fetchApiModels
  , fetchAnthropicModels
    -- * Vision
  , availableVisionBackends
    -- * Python helpers
  , pyStrStrip
  , capitalize
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Fisher.Compat (envNames)
import Fisher.LLM.Auth.Anthropic (isOauthToken, resolveAnthropicToken)
import Fisher.LLM.Auth.Codex (readCodexTokens)
import Fisher.LLM.Auth.Env (getEnvValue)
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered, saveConfigOrdered)
import Fisher.LLM.Auth.Resolve (RuntimeRequest (..), defaultRuntimeRequest, resolveRuntimeProvider)
import Fisher.LLM.Auth.Util (rstripSlash)
import Fisher.LLM.Sys
import Fisher.Paths (homeLabel)
import Fisher.Py.Value

-- ------------------------------------------------------------ configuration

-- | @load_config()@ with Python's key order and number types.
loadCfg :: Sys -> IO PyVal
loadCfg = loadConfigOrdered

-- | @save_config(config)@.
saveCfg :: Sys -> PyVal -> IO ()
saveCfg = saveConfigOrdered

-- | @cfg.get(k)@ as text (@str()@), empty when absent or None.
getS :: Text -> PyVal -> Text
getS k v = case get k v of
  Just PNull -> ""
  Just x -> pyStrOf x
  Nothing -> ""

-- | @cfg.get(k, {})@ when it is a dict, else @{}@.
getD :: Text -> PyVal -> PyVal
getD k v = case get k v of
  Just d@(PDict _) -> d
  _ -> PDict []

-- | @_model_config_dict(config)@.
modelConfigDict :: PyVal -> PyVal
modelConfigDict cfg = case get "model" cfg of
  Just d@(PDict _) -> d
  Just (PStr s) | not (T.null (T.strip s)) -> obj [("default", str (T.strip s))]
  _ -> PDict []

-- | @_set_model_provider(config, provider_id, base_url)@.
setModelProvider :: Text -> Text -> PyVal -> PyVal
setModelProvider pid baseUrl cfg =
  let m1 = set "provider" (str pid) (modelConfigDict cfg)
      m2 = if T.null baseUrl then delete "base_url" m1 else set "base_url" (str (rstripSlash baseUrl)) m1
   in set "model" m2 cfg

-- | @_set_default_model(config, model_name)@.
setDefaultModel :: Text -> PyVal -> PyVal
setDefaultModel name cfg
  | T.null name = cfg
  | otherwise = set "model" (set "default" (str name) (modelConfigDict cfg)) cfg

-- | @get_env_value(k) or ""@.
envValue :: Sys -> Text -> IO Text
envValue sys k = fromMaybe "" <$> getEnvValue sys k

-- | 'envValue' of a renamed variable: @FISHER_X@ (the environment, then
-- the home's .env), else @GAUSS_X@ the same way.
envValueCompat :: Sys -> Text -> IO Text
envValueCompat sys k = do
  let (new, old) = envNames (T.unpack k)
  getEnvValue sys (T.pack new) >>= \case
    Just v -> pure v
    Nothing -> envValue sys (T.pack old)

-- | How a message names the Fisher home this 'Sys' uses ('homeLabel'):
-- @~/.gauss@ on a machine with only that home, never a literal
-- @~/.fisher@ the user would then create.
homeLabelOf :: Sys -> Text
homeLabelOf sys = T.pack (homeLabel (sysHome sys) (sysFisherHome sys))

-- | @str(v).strip()@ of an optional value; empty for None.
pyStrStrip :: Maybe PyVal -> Text
pyStrStrip = \case
  Nothing -> ""
  Just PNull -> ""
  Just v -> T.strip (pyStrOf v)

-- | @str.capitalize()@.
capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Nothing -> t
  Just (c, r) -> T.toUpper (T.singleton c) <> T.toLower r

-- --------------------------------------------------------------- catalogues

-- | @OPENROUTER_MODELS@.
openrouterModels :: [(Text, Text)]
openrouterModels =
  [ ("anthropic/claude-opus-4.6", "recommended"), ("anthropic/claude-sonnet-4.5", ""), ("openai/gpt-5.4-pro", "")
  , ("openai/gpt-5.4", ""), ("openai/gpt-5.3-codex", ""), ("google/gemini-3-pro-preview", "")
  , ("google/gemini-3-flash-preview", ""), ("qwen/qwen3.5-plus-02-15", ""), ("qwen/qwen3.5-35b-a3b", "")
  , ("stepfun/step-3.5-flash", ""), ("z-ai/glm-5", ""), ("moonshotai/kimi-k2.5", ""), ("minimax/minimax-m2.5", "") ]

modelIds :: [Text]
modelIds = map fst openrouterModels

menuLabels :: [Text]
menuLabels = [if T.null d then m else m <> " (" <> d <> ")" | (m, d) <- openrouterModels]

-- | @models._PROVIDER_MODELS@.
providerModels :: Text -> [Text]
providerModels = \case
  "nous" -> ["claude-opus-4-6", "claude-sonnet-4-6", "gpt-5.4", "gemini-3-flash", "gemini-3.0-pro-preview", "deepseek-v3.2"]
  "openai-codex" -> ["gpt-5.3-codex", "gpt-5.2-codex", "gpt-5.1-codex-mini", "gpt-5.1-codex-max"]
  "zai" -> ["glm-5", "glm-4.7", "glm-4.5", "glm-4.5-flash"]
  "kimi-coding" -> ["kimi-for-coding", "kimi-k2.5", "kimi-k2-thinking", "kimi-k2-thinking-turbo", "kimi-k2-turbo-preview", "kimi-k2-0905-preview"]
  "minimax" -> ["MiniMax-M2.5", "MiniMax-M2.5-highspeed", "MiniMax-M2.1"]
  "minimax-cn" -> ["MiniMax-M2.5", "MiniMax-M2.5-highspeed", "MiniMax-M2.1"]
  "anthropic" ->
    [ "claude-opus-4-6", "claude-sonnet-4-6", "claude-opus-4-5-20251101", "claude-sonnet-4-5-20250929"
    , "claude-opus-4-20250514", "claude-sonnet-4-20250514", "claude-haiku-4-5-20251001" ]
  "deepseek" -> ["deepseek-chat", "deepseek-reasoner"]
  _ -> []

-- ------------------------------------------------------------------- probes

-- | What @probe_api_models@ returns.
data Probe = Probe
  { prModels :: Maybe [Text]
  , prProbedUrl :: Maybe Text
  , prResolvedBaseUrl :: Text
  , prSuggestedBaseUrl :: Maybe Text
  , prUsedFallback :: Bool
  }
  deriving (Eq, Show)

-- | @urllib.request.urlopen(Request(url, headers=...), timeout)@ then
-- @json.loads@: Nothing for anything that would raise.
urlopenJson :: Sys -> Text -> [(Text, Text)] -> Double -> IO (Maybe PyVal)
urlopenJson sys url hs timeout = do
  r <- try (sysHttp sys (AuthReq ViaUrllib "GET" url hs NoBody timeout)) :: IO (Either SomeException (Either HttpFail AuthResp))
  pure $ case r of
    Right (Right resp) | aStatus resp < 400, Right j <- authJson resp -> Just j
    _ -> Nothing

-- | @probe_api_models(api_key, base_url, timeout)@: @/models@ at the URL
-- given, then with @/v1@ added or taken away.
probeApiModels :: Sys -> Text -> Text -> Double -> IO Probe
probeApiModels sys key baseUrl timeout = do
  let normalized = rstripSlash (T.strip baseUrl)
  if T.null normalized
    then pure (Probe Nothing Nothing "" Nothing False)
    else do
      let alternate =
            if "/v1" `T.isSuffixOf` normalized
              then rstripSlash (T.dropEnd 3 normalized)
              else normalized <> "/v1"
          candidates = (normalized, False) : [(alternate, True) | not (T.null alternate), alternate /= normalized]
          hs = [("Authorization", "Bearer " <> key) | not (T.null key)]
          go [] tried =
            pure
              Probe
                { prModels = Nothing
                , prProbedUrl = Just (if null tried then rstripSlash normalized <> "/models" else last tried)
                , prResolvedBaseUrl = normalized
                , prSuggestedBaseUrl = if alternate /= normalized then Just alternate else Nothing
                , prUsedFallback = False
                }
          go ((cand, isFallback) : rest) tried = do
            let url = rstripSlash cand <> "/models"
            r <- urlopenJson sys url hs timeout
            case r >>= modelsOf of
              Just ms ->
                pure
                  Probe
                    { prModels = Just ms
                    , prProbedUrl = Just url
                    , prResolvedBaseUrl = rstripSlash cand
                    , prSuggestedBaseUrl = Just (if alternate /= cand then alternate else normalized)
                    , prUsedFallback = isFallback
                    }
              Nothing -> go rest (tried <> [url])
      go candidates []
  where
    -- @[m.get("id", "") for m in data.get("data", [])]@; Nothing where
    -- that raises.
    modelsOf d = case d of
      PDict _ -> case fromMaybe (PList []) (get "data" d) of
        PList xs -> mapM (\case m@(PDict _) -> Just (maybe "" pyStrOf (get "id" m)); _ -> Nothing) xs
        PStr s | T.null s -> Just []
        _ -> Nothing
      _ -> Nothing

-- | @fetch_api_models(api_key, base_url, timeout)@.
fetchApiModels :: Sys -> Text -> Text -> Double -> IO (Maybe [Text])
fetchApiModels sys key url timeout = prModels <$> probeApiModels sys key url timeout

-- | @models._fetch_anthropic_models()@: the live list, opus, then sonnet,
-- then haiku first; Nothing without a token or on any failure.
fetchAnthropicModels :: Sys -> IO (Maybe [Text])
fetchAnthropicModels sys =
  resolveAnthropicToken sys >>= \case
    Nothing -> pure Nothing
    Just tok | T.null tok -> pure Nothing
    Just tok -> do
      let hs =
            ("anthropic-version", "2023-06-01")
              : if isOauthToken tok
                then
                  [ ("Authorization", "Bearer " <> tok)
                  , ("anthropic-beta", "interleaved-thinking-2025-05-14,fine-grained-tool-streaming-2025-05-14,claude-code-20250219,oauth-2025-04-20") ]
                else [("x-api-key", tok)]
      r <- urlopenJson sys "https://api.anthropic.com/v1/models" hs 5.0
      pure $ do
        d <- r
        entries <- case d of
          PDict _ -> case fromMaybe (PList []) (get "data" d) of
            PList xs -> Just xs
            _ -> Nothing
          _ -> Nothing
        ids <- mapM (\case m@(PDict _) -> Just (get "id" m); _ -> Nothing) entries
        let ms = [pyStrOf v | Just v <- ids, truthy v]
        pure (sortOn (\m -> (not ("opus" `T.isInfixOf` m), not ("sonnet" `T.isInfixOf` m), not ("haiku" `T.isInfixOf` m), m)) ms)

-- ------------------------------------------------------------------- vision

-- | @get_available_vision_backends()@: the backends Fisher can use for
-- vision, the configured provider first. Left where upstream raises: with
-- an Anthropic token the Anthropic client cannot be built in upstream's
-- environment (its SDK wants httpx2's Timeout), and the error escapes the
-- whole list.
availableVisionBackends :: Sys -> IO (Either () [Text])
availableVisionBackends sys = do
  cfg <- loadCfg sys
  let order = ["openrouter", "nous", "openai-codex", "anthropic", "custom"]
      preferred = case get "model" cfg of
        Just m@(PDict _) ->
          let p = normalize (case get "provider" m of Just PNull -> ""; Just v -> pyStrOf v; Nothing -> "")
           in if p `elem` order then Just p else Nothing
        _ -> Nothing
      ordered' = maybe order (\p -> p : filter (/= p) order) preferred
      go [] acc = pure (Right (reverse acc))
      go (p : ps) acc =
        available p >>= \case
          Left () -> pure (Left ())
          Right True -> go ps (p : acc)
          Right False -> go ps acc
  go ordered' []
  where
    normalize p = case T.toLower (T.strip (if T.null p then "auto" else p)) of
      "codex" -> "openai-codex"
      "main" -> "custom"
      x -> x
    available :: Text -> IO (Either () Bool)
    available = \case
      "openrouter" -> Right . maybe False (not . T.null) <$> sysGetEnv sys "OPENROUTER_API_KEY"
      "nous" -> Right <$> nousAvailable
      "openai-codex" -> do
        r <- try (readCodexTokens sys True) :: IO (Either SomeException PyVal)
        pure . Right $ case r of
          Right d -> case get "tokens" d >>= get "access_token" of
            Just (PStr t) -> not (T.null (T.strip t))
            _ -> False
          Left _ -> False
      "anthropic" -> do
        t <- resolveAnthropicToken sys
        pure (if maybe False (not . T.null) t then Left () else Right False)
      "custom" -> do
        r <- try (resolveRuntimeProvider sys defaultRuntimeRequest {rrRequested = Just "custom"}) :: IO (Either SomeException PyVal)
        pure . Right $ case r of
          Right rt
            | Just (PStr b) <- get "base_url" rt
            , Just (PStr k) <- get "api_key" rt
            , not (T.null (T.strip b))
            , not (T.null (T.strip k)) ->
                not ("openrouter.ai" `T.isInfixOf` T.toLower (rstripSlash (T.strip b)))
          _ -> False
      _ -> pure (Right False)
    nousAvailable = do
      let path = sysFisherHome sys </> "auth.json"
      exists <- doesFileExist path
      if not exists
        then pure False
        else do
          r <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
          pure $ case r of
            Right bytes
              | Right d@(PDict _) <- loads bytes
              , get "active_provider" d == Just (str "nous")
              , Just ps@(PDict _) <- Just (fromMaybe (PDict []) (get "providers" d))
              , Just n@(PDict _) <- Just (fromMaybe (PDict []) (get "nous" ps)) ->
                  maybe False truthy (get "agent_key" n) || maybe False truthy (get "access_token" n)
            _ -> False
