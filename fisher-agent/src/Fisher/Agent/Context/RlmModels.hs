-- | The model builders over Fisher's LLM client, shared by @fisher rlm@ and
-- the agent's recall: a root and a sub-call 'Model'.
module Fisher.Agent.Context.RlmModels
  ( Models (..)
  , liveModels
  , rlmModel
  , contentFilter
  , zeroUsage
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Agent.Types (responseRefusal)
import Fisher.LLM.Client (CallResult (..), callModel)
import Fisher.LLM.Errors (LLMError (..), errorClass, errorText)
import Fisher.LLM.Http (Runtime, Transport)
import Fisher.LLM.Normalize (Outcome (..), Usage (..))
import Fisher.LLM.Prepare (PrepareInput (..), prepareMessages, usesPromptCaching)
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..))
import Fisher.RLM.Types

-- | The root and sub-call models.
data Models = Models
  { mRoot :: Model
  , mSub :: Model
  }

-- | The models over Fisher's LLM client: the root's default is
-- 'rcModel', a sub-call's 'rcSubModel' or else 'rcModel'.
liveModels :: Runtime -> Transport -> Env -> RlmConfig -> Settings -> Models
liveModels rt tr env cfg base =
  Models
    { mRoot = rlmModel rt tr env base {sModel = rcModel cfg}
    , mSub = rlmModel rt tr env base {sModel = fromMaybe (rcModel cfg) (rcSubModel cfg)}
    }

-- | One model call through Fisher's LLM client: the settings' model unless
-- the loop names another, the messages prepared as the agent prepares them,
-- no @max_tokens@. 402 is 'NoCredit'; a refusal with no content is
-- 'Refused'; no content is 'Empty'.
rlmModel :: Runtime -> Transport -> Env -> Settings -> Model
rlmModel rt tr env base override msgs = do
  let s = base {sModel = fromMaybe (sModel base) override, sMaxTokens = Nothing}
      caching = usesPromptCaching (sBaseUrl s) (sModel s) (sMode s == AnthropicMessages)
      prepared = prepareMessages (PrepareInput (sBaseUrl s) Nothing Nothing [] caching) [obj [("role", str r), ("content", str c)] | (r, c) <- msgs]
  res <- callModel rt tr env s prepared
  pure $ case res of
    Left (StatusError 402 _ _) -> Left NoCredit
    Left e
      | T.null (errorClass e) -> Left (OtherError (errorText e))
      | otherwise -> Left (OtherError (errorClass e <> ": " <> errorText e))
    Right (CallInvalid _) -> Left Empty
    Right (CallOk outc raw) ->
      let content = case get "content" (oMessage outc) of
            Just (PStr t) -> t
            _ -> ""
       in if T.null content
            then Left (maybe (maybe Empty Refused (contentFilter outc raw)) Refused (responseRefusal raw))
            else Right (Reply content (fromMaybe zeroUsage (oUsage outc)) (sModel s))

-- | A reply cut off by the provider's content filter (finish reason
-- @content_filter@, with the upstream's own reason in parentheses when the
-- response has one): a refusal, not an empty reply.
contentFilter :: Outcome -> PyVal -> Maybe Text
contentFilter outc raw
  | finish == Just "content_filter" = Just ("content filter" <> maybe "" (\n -> " (" <> n <> ")") native)
  | otherwise = Nothing
  where
    choice = case get "choices" raw of
      Just (PList (c : _)) -> Just c
      _ -> Nothing
    str' = \case Just (PStr t) | not (T.null t) -> Just t; _ -> Nothing
    finish = case str' (Just (oFinish outc)) of
      Nothing -> str' (choice >>= get "finish_reason")
      f -> f
    native = str' (choice >>= get "native_finish_reason")

zeroUsage :: Usage
zeroUsage = Usage 0 0 0 Nothing Nothing Nothing
