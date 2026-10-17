-- | One place that turns settings and prepared messages into the HTTP
-- request for the active wire format.
module Fisher.LLM.Request
  ( buildRequest
  ) where

import Data.Text (Text)

import qualified Fisher.LLM.Anthropic as An
import qualified Fisher.LLM.Chat as Ch
import qualified Fisher.LLM.Codex as Cx
import Fisher.LLM.Types
import Fisher.Py.Value (PyVal)

-- | Left is a request upstream's preflight refuses (Codex only).
buildRequest :: Env -> Settings -> [PyVal] -> IO (Either Text HttpReq)
buildRequest env s msgs = case sMode s of
  ChatCompletions -> pure (Right (Ch.chatRequest s msgs))
  CodexResponses -> Cx.codexRequest env s msgs
  AnthropicMessages -> Right <$> An.anthropicRequest env s msgs
