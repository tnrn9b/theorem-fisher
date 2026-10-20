-- | @fisher login ...@: upstream removed the command; argparse still checks
-- the flags, then it points at @fisher model@ and @fisher setup@. The login
-- flow itself ('Fisher.LLM.Auth.Codex.loginOpenAICodex') is reached from
-- model and setup. Upstream's flags that only a removed provider's login
-- read (portal and inference URLs, client id, scope) are gone.
module Fisher.Cli.Login
  ( loginMain
  ) where

import Data.Text (Text)

import Fisher.Cli.AuthArgs
import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.Logout (loginCommand)
import Fisher.LLM.Sys

loginMain :: Sys -> [Text] -> IO Int
loginMain sys args = do
  loadDotenvIntoSys sys
  case parseArgs loginParser args of
    Left (msg, code) -> sysOut sys msg >> pure code
    Right _ -> loginCommand sys
