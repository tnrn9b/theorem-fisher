-- | @fisher logout [--provider openai-codex]@: argparse, then
-- 'logoutCommand'. @--provider claude-code@ runs @claude auth logout@
-- ('claudeCodeLogout').
module Fisher.Cli.Logout
  ( logoutMain
  ) where

import Data.Text (Text)

import Fisher.Cli.AuthArgs
import Fisher.Cli.ClaudeLogin (claudeCodeLogout)
import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.Logout (logoutCommand)
import Fisher.LLM.Sys

-- | The home's @.env@ loaded as the CLI does at start-up, the arguments
-- parsed, the logout run. Returns the exit code.
logoutMain :: Sys -> [Text] -> IO Int
logoutMain sys args = do
  loadDotenvIntoSys sys
  case parseArgs logoutParser args of
    Left (msg, code) -> sysOut sys msg >> pure code
    Right opts
      | optValue "provider" opts == Just "claude-code" -> claudeCodeLogout sys
      | otherwise -> logoutCommand sys (optValue "provider" opts)
