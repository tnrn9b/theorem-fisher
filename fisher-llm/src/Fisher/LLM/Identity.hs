{-# LANGUAGE TemplateHaskell #-}
-- | Upstream's @DEFAULT_AGENT_IDENTITY@ (@agent/prompt_builder.py@), the
-- Codex instructions when there is no system prompt. Generated into
-- @data/agent-identity.txt@ by @tools/gen-defaults.py@.
module Fisher.LLM.Identity (defaultAgentIdentity) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

defaultAgentIdentity :: Text
defaultAgentIdentity = TE.decodeUtf8 $(makeRelativeToProject "data/agent-identity.txt" >>= embedFile)
