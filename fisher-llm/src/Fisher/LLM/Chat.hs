-- | OpenAI chat completions, as @run_agent.py@ sends them to OpenRouter,
-- Nous, the API-key providers and custom endpoints.
module Fisher.LLM.Chat
  ( chatRequest
  , chatHeaders
  , buildBody
  , supportsReasoningExtraBody
  , openRouterHeaders
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Fisher.LLM.Types
import Fisher.Py.Value

-- | The referer URL and the Nous @product=gauss-agent@ tag are the
-- providers' registrations of upstream's app, so they stay; the title is ours.
openRouterHeaders :: [(Text, Text)]
openRouterHeaders =
  [ ("http-referer", "https://gauss-agent.nousresearch.com")
  , ("x-openrouter-categories", "productivity,cli-agent")
  , ("x-openrouter-title", "Fisher Agent") ]

-- | The OpenAI SDK's headers plus the client's @default_headers@
-- (@run_agent.py:515-526@).
chatHeaders :: Text -> Text -> [(Text, Text)]
chatHeaders baseUrl key =
  [("accept", "application/json"), ("authorization", "Bearer " <> key), ("content-type", "application/json")]
    <> extra
  where
    low = T.toLower baseUrl
    extra
      | "openrouter" `T.isInfixOf` low = openRouterHeaders
      | "api.kimi.com" `T.isInfixOf` low = [("user-agent", "KimiCLI/1.3")]
      | otherwise = []

-- | @_supports_reasoning_extra_body@.
supportsReasoningExtraBody :: Text -> Text -> Bool
supportsReasoningExtraBody baseUrl model
  | "nousresearch" `T.isInfixOf` low = True
  | not ("openrouter" `T.isInfixOf` low) = False
  | "api.mistral.ai" `T.isInfixOf` low = False
  | otherwise = any (`T.isPrefixOf` T.toLower model) ["deepseek/", "anthropic/", "openai/", "x-ai/", "google/gemini-2", "qwen/qwen3"]
  where
    low = T.toLower baseUrl

-- | The JSON body: @_build_api_kwargs@ with @extra_body@ merged in and the
-- SDK-only @timeout@ dropped.
buildBody :: Settings -> [PyVal] -> PyVal
buildBody s msgs =
  let needs = any needsSanitizing msgs
      sanitized = if needs then map scrub msgs else msgs
      low = T.toLower (sBaseUrl s)
      r = sRouting s
      prefs =
        [("only", PList (map str (rOnly r))) | not (null (rOnly r))]
          <> [("ignore", PList (map str (rIgnore r))) | not (null (rIgnore r))]
          <> [("order", PList (map str (rOrder r))) | not (null (rOrder r))]
          <> [("sort", str x) | Just x <- [rSort r], not (T.null x)]
          <> [("require_parameters", PBool True) | rRequireParameters r]
          <> [("data_collection", str x) | Just x <- [rDataCollection r], not (T.null x)]
      isOpenRouter = "openrouter" `T.isInfixOf` low
      isNous = "nousresearch" `T.isInfixOf` low
      maxTok = case sMaxTokens s of
        Just n
          | "api.openai.com" `T.isInfixOf` low && not ("openrouter" `T.isInfixOf` low) -> [("max_completion_tokens", PInt n)]
          | otherwise -> [("max_tokens", PInt n)]
        Nothing -> []
      reasoning
        | supportsReasoningExtraBody (sBaseUrl s) (sModel s) = case sReasoning s of
            Just rc -> if isNous && get "enabled" rc == Just (PBool False) then [] else [("reasoning", rc)]
            Nothing -> [("reasoning", obj [("enabled", PBool True), ("effort", str "medium")])]
        | otherwise = []
      extra =
        [("provider", obj prefs) | not (null prefs), isOpenRouter]
          <> reasoning
          <> [("tags", PList [str "product=gauss-agent"]) | isNous]
      tools = case sTools s of
        Just ts@(_ : _) -> PList ts
        _ -> PNull
   in obj ([("model", str (sModel s)), ("messages", PList sanitized), ("tools", tools)] <> maxTok <> extra)
  where
    needsSanitizing m =
      has "codex_reasoning_items" m
        || case get "tool_calls" m of
          Just (PList tcs) -> any (\tc -> has "call_id" tc || has "response_item_id" tc) tcs
          _ -> False
    scrub m@(PDict _) =
      let m' = delete "codex_reasoning_items" m
       in case get "tool_calls" m' of
            Just (PList tcs) -> set "tool_calls" (PList [delete "response_item_id" (delete "call_id" tc) | tc <- tcs]) m'
            _ -> m'
    scrub m = m

chatRequest :: Settings -> [PyVal] -> HttpReq
chatRequest s msgs =
  let base = T.dropWhileEnd (== '/') (sBaseUrl s)
   in HttpReq "POST" (base <> "/chat/completions") (chatHeaders (sBaseUrl s) (sApiKey s)) (Just (buildBody s msgs)) False
