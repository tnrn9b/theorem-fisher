-- | @fisher login ...@: upstream removed the command; argparse still checks
-- the flags, then it points at @fisher model@ and @fisher setup@. The login
-- flow itself ('Fisher.LLM.Auth.Codex.loginOpenAICodex') is reached from
-- model and setup. Upstream's flags that only a removed provider's login
-- read (portal and inference URLs, client id, scope) are gone.
--
-- @fisher login --provider claude-code [--console] [--sso] [--email E]@
-- runs the Claude Code CLI's own login ('claudeCodeLogin'); the three
-- options go with that provider only.
module Fisher.Cli.Login
  ( loginMain
  ) where

import Data.Text (Text)

import Fisher.ClaudeAuth (ClaudeLogin (..), validClaudeEmail)
import Fisher.Cli.AuthArgs
import Fisher.Cli.ClaudeLogin (claudeCodeLogin)
import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.Logout (loginCommand)
import Fisher.LLM.Sys

loginMain :: Sys -> [Text] -> IO Int
loginMain sys args = do
  loadDotenvIntoSys sys
  case parseArgs loginParser args of
    Left (msg, code) -> sysOut sys msg >> pure code
    Right opts
      | optValue "provider" opts == Just "claude-code" -> case optValue "email" opts of
          Just e | not (validClaudeEmail e) -> usageError "argument --email: give one email address (no spaces, at most 254 characters)"
          email -> claudeCodeLogin sys (ClaudeLogin (flagSet "console" opts) (flagSet "sso" opts) email)
      | any (`flagSet` opts) ["console", "sso", "email"] -> usageError "--console, --sso and --email go with --provider claude-code"
      | otherwise -> loginCommand sys
  where
    usageError m = sysOut sys (apUsage loginParser <> apProg loginParser <> ": error: " <> m <> "\n") >> pure 2
