-- | @gauss logout@ and the removed @gauss login@. Ported from
-- @logout_command@ and @login_command@ in @gauss_cli/auth.py@.
module Fisher.LLM.Auth.Logout
  ( logoutCommand
  , loginCommand
  ) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.LLM.Auth.Config (resetConfigProvider)
import Fisher.LLM.Auth.Error (cliName)
import Fisher.LLM.Auth.Registry
import Fisher.LLM.Auth.Store
import Fisher.LLM.Sys
import Fisher.Py.Value

-- | @logout_command(args)@ with @--provider@ as given: clear that
-- provider's state (the active one by default), put @config.yaml@ back on
-- @auto@ and say what happens next. Returns the exit code.
logoutCommand :: Sys -> Maybe Text -> IO Int
logoutCommand sys mprovider = case mprovider of
  Just p | not (T.null p), Nothing <- lookupProvider p -> do
    outLn sys ("Unknown provider: " <> p)
    pure 1
  _ -> do
    active <- getActiveProvider sys
    let target = case mprovider of
          Just p | not (T.null p) -> Just p
          _ -> case active of
            PStr a | not (T.null a) -> Just a
            v | truthy v -> Just (pyStrOf v)
            _ -> Nothing
    case target of
      Nothing -> outLn sys "No provider is currently logged in." >> pure 0
      Just t -> do
        let name = maybe t pcName (lookupProvider t)
        cleared <- clearProviderAuth sys (Just t)
        if cleared
          then do
            void (resetConfigProvider sys)
            outLn sys ("Logged out of " <> name <> ".")
            orKey <- fromMaybe "" <$> sysGetEnv sys "OPENROUTER_API_KEY"
            outLn sys $
              if T.null orKey
                then "Run `" <> cliName <> " model` or configure an API key to continue."
                else "Gauss will use OpenRouter for inference."
          else outLn sys ("No auth state found for " <> name <> ".")
        pure 0

-- | @login_command@: the command was removed upstream; it says where to go
-- instead and exits 0.
loginCommand :: Sys -> IO Int
loginCommand sys = do
  outLn sys ("The '" <> cliName <> " login' command has been removed.")
  outLn sys ("Use '" <> cliName <> " model' to select a provider and model,")
  outLn sys ("or '" <> cliName <> " setup' for full interactive setup.")
  pure 0
