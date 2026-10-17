-- | The providers upstream knows by name: @PROVIDER_REGISTRY@ (in its
-- order, which auto-detection follows), the aliases, the labels
-- @gauss_cli/models.py@ shows, the Kimi base-URL rule and the Z.AI
-- endpoint probe. Ported from @gauss_cli/auth.py@ and @gauss_cli/models.py@.
module Fisher.LLM.Auth.Registry
  ( AuthType (..)
  , ProviderConfig (..)
  , providerRegistry
  , lookupProvider
  , isApiKeyProvider
    -- * Aliases and labels
  , authProviderAliases
  , modelProviderAliases
  , normalizeProvider
  , providerLabels
  , providerLabel
    -- * Constants
  , defaultNousPortalUrl
  , defaultNousInferenceUrl
  , defaultNousClientId
  , defaultNousScope
  , defaultAgentKeyMinTtlSeconds
  , accessTokenRefreshSkewSeconds
  , deviceAuthPollIntervalCapSeconds
  , defaultCodexBaseUrl
  , codexOAuthClientId
  , codexOAuthTokenUrl
  , codexAccessTokenRefreshSkewSeconds
  , authLockTimeoutSeconds
  , openrouterBaseUrl
  , openrouterModelsUrl
    -- * Kimi
  , kimiCodeBaseUrl
  , resolveKimiBaseUrl
    -- * Z.AI
  , ZaiEndpoint (..)
  , zaiEndpoints
  , detectZaiEndpoint
  ) where

import Control.Exception (SomeException, try)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.LLM.Auth.Util (httpxClient, send)
import Fisher.LLM.Sys
import Fisher.Py.Value

data AuthType = OAuthDeviceCode | OAuthExternal | ApiKey
  deriving (Eq, Show)

data ProviderConfig = ProviderConfig
  { pcId :: Text
  , pcName :: Text
  , pcAuthType :: AuthType
  , pcPortalBaseUrl :: Text
  , pcInferenceBaseUrl :: Text
  , pcClientId :: Text
  , pcScope :: Text
  , pcApiKeyEnvVars :: [Text]
  , pcBaseUrlEnvVar :: Text
  }
  deriving (Eq, Show)

defaultNousPortalUrl, defaultNousInferenceUrl, defaultNousClientId, defaultNousScope :: Text
defaultNousPortalUrl = "https://portal.nousresearch.com"
defaultNousInferenceUrl = "https://inference-api.nousresearch.com/v1"
defaultNousClientId = "gauss-cli"
defaultNousScope = "inference:mint_agent_key"

defaultAgentKeyMinTtlSeconds, accessTokenRefreshSkewSeconds, deviceAuthPollIntervalCapSeconds, codexAccessTokenRefreshSkewSeconds :: Int
defaultAgentKeyMinTtlSeconds = 30 * 60
accessTokenRefreshSkewSeconds = 120
deviceAuthPollIntervalCapSeconds = 1
codexAccessTokenRefreshSkewSeconds = 120

authLockTimeoutSeconds :: Double
authLockTimeoutSeconds = 15.0

defaultCodexBaseUrl, codexOAuthClientId, codexOAuthTokenUrl :: Text
defaultCodexBaseUrl = "https://chatgpt.com/backend-api/codex"
codexOAuthClientId = "app_EMoamEEZ73f0CkXaXp7hrann"
codexOAuthTokenUrl = "https://auth.openai.com/oauth/token"

-- | @gauss_constants.py@.
openrouterBaseUrl, openrouterModelsUrl :: Text
openrouterBaseUrl = "https://openrouter.ai/api/v1"
openrouterModelsUrl = openrouterBaseUrl <> "/models"

apiKey :: Text -> Text -> Text -> [Text] -> Text -> ProviderConfig
apiKey i n url vars envUrl = ProviderConfig i n ApiKey "" url "" "" vars envUrl

-- | @PROVIDER_REGISTRY@, in its order.
providerRegistry :: [ProviderConfig]
providerRegistry =
  [ ProviderConfig "nous" "Nous Portal" OAuthDeviceCode defaultNousPortalUrl defaultNousInferenceUrl defaultNousClientId defaultNousScope [] ""
  , ProviderConfig "openai-codex" "OpenAI Codex" OAuthExternal "" defaultCodexBaseUrl "" "" [] ""
  , apiKey "zai" "Z.AI / GLM" "https://api.z.ai/api/paas/v4" ["GLM_API_KEY", "ZAI_API_KEY", "Z_AI_API_KEY"] "GLM_BASE_URL"
  , apiKey "kimi-coding" "Kimi / Moonshot" "https://api.moonshot.ai/v1" ["KIMI_API_KEY"] "KIMI_BASE_URL"
  , apiKey "minimax" "MiniMax" "https://api.minimax.io/v1" ["MINIMAX_API_KEY"] "MINIMAX_BASE_URL"
  , apiKey "anthropic" "Anthropic" "https://api.anthropic.com" ["ANTHROPIC_API_KEY", "ANTHROPIC_TOKEN", "CLAUDE_CODE_OAUTH_TOKEN"] ""
  , apiKey "minimax-cn" "MiniMax (China)" "https://api.minimaxi.com/v1" ["MINIMAX_CN_API_KEY"] "MINIMAX_CN_BASE_URL"
  , apiKey "deepseek" "DeepSeek" "https://api.deepseek.com/v1" ["DEEPSEEK_API_KEY"] "DEEPSEEK_BASE_URL"
  ]

lookupProvider :: Text -> Maybe ProviderConfig
lookupProvider pid = find ((== pid) . pcId) providerRegistry

isApiKeyProvider :: Text -> Bool
isApiKeyProvider pid = maybe False ((== ApiKey) . pcAuthType) (lookupProvider pid)

-- | The aliases @resolve_provider@ applies.
authProviderAliases :: [(Text, Text)]
authProviderAliases =
  [ ("glm", "zai"), ("z-ai", "zai"), ("z.ai", "zai"), ("zhipu", "zai")
  , ("kimi", "kimi-coding"), ("moonshot", "kimi-coding")
  , ("minimax-china", "minimax-cn"), ("minimax_cn", "minimax-cn")
  , ("claude", "anthropic"), ("claude-code", "anthropic") ]

-- | @models._PROVIDER_ALIASES@: the same plus @deep-seek@.
modelProviderAliases :: [(Text, Text)]
modelProviderAliases = authProviderAliases <> [("deep-seek", "deepseek")]

-- | @models.normalize_provider@.
normalizeProvider :: Maybe Text -> Text
normalizeProvider p =
  let n = T.toLower (T.strip (fromMaybe "openrouter" (nonEmpty p)))
   in fromMaybe n (lookup n modelProviderAliases)

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty = \case
  Just t | not (T.null t) -> Just t
  _ -> Nothing

-- | @models._PROVIDER_LABELS@.
providerLabels :: [(Text, Text)]
providerLabels =
  [ ("openrouter", "OpenRouter"), ("openai-codex", "OpenAI Codex"), ("nous", "Nous Portal")
  , ("zai", "Z.AI / GLM"), ("kimi-coding", "Kimi / Moonshot"), ("minimax", "MiniMax")
  , ("minimax-cn", "MiniMax (China)"), ("anthropic", "Anthropic"), ("deepseek", "DeepSeek")
  , ("custom", "Custom endpoint") ]

-- | @models.provider_label@.
providerLabel :: Maybe Text -> Text
providerLabel p =
  let original = T.strip (fromMaybe "openrouter" (nonEmpty p))
      lowered = T.toLower original
   in if lowered == "auto"
        then "Auto"
        else fromMaybe (if T.null original then "OpenRouter" else original) (lookup (normalizeProvider (Just lowered)) providerLabels)

-- | Keys from Kimi Code work only on this endpoint.
kimiCodeBaseUrl :: Text
kimiCodeBaseUrl = "https://api.kimi.com/coding/v1"

-- | @_resolve_kimi_base_url(api_key, default_url, env_override)@.
resolveKimiBaseUrl :: Text -> Text -> Text -> Text
resolveKimiBaseUrl key dflt override
  | not (T.null override) = override
  | "sk-kimi-" `T.isPrefixOf` key = kimiCodeBaseUrl
  | otherwise = dflt

data ZaiEndpoint = ZaiEndpoint
  { zeId :: Text
  , zeBaseUrl :: Text
  , zeModel :: Text
  , zeLabel :: Text
  }
  deriving (Eq, Show)

-- | @ZAI_ENDPOINTS@, in probe order.
zaiEndpoints :: [ZaiEndpoint]
zaiEndpoints =
  [ ZaiEndpoint "global" "https://api.z.ai/api/paas/v4" "glm-5" "Global"
  , ZaiEndpoint "cn" "https://open.bigmodel.cn/api/paas/v4" "glm-5" "China"
  , ZaiEndpoint "coding-global" "https://api.z.ai/api/coding/paas/v4" "glm-4.7" "Global (Coding Plan)"
  , ZaiEndpoint "coding-cn" "https://open.bigmodel.cn/api/coding/paas/v4" "glm-4.7" "China (Coding Plan)" ]

-- | @detect_zai_endpoint(api_key, timeout)@: the first endpoint whose
-- one-token completion answers 200; failures of any kind move on.
detectZaiEndpoint :: Sys -> Text -> Double -> IO (Maybe ZaiEndpoint)
detectZaiEndpoint sys key timeout = go zaiEndpoints
  where
    go [] = pure Nothing
    go (ep : rest) = do
      let body =
            obj
              [ ("model", str (zeModel ep)), ("stream", PBool False), ("max_tokens", PInt 1)
              , ("messages", PList [obj [("role", str "user"), ("content", str "ping")]]) ]
      r <- try (send sys (httpxClient [] timeout) "POST" (zeBaseUrl ep <> "/chat/completions")
                  [("Authorization", "Bearer " <> key), ("Content-Type", "application/json")] (JsonBody body))
      case r :: Either SomeException AuthResp of
        Right resp | aStatus resp == 200 -> pure (Just ep)
        _ -> go rest
