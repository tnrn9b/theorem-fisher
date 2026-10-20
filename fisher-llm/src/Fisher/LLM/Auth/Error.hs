-- | The errors the credential code raises, in upstream's terms:
-- @AuthError@ (with its provider, code and re-login flag), any other Python
-- exception upstream lets escape (kept as its type, module and @str()@),
-- and @SystemExit@ from the login flows. Ported from upstream @cli/auth.py@.
module Fisher.LLM.Auth.Error
  ( cliName
  , AuthError (..)
  , authError
  , PyError (..)
  , pyError
  , SystemExit (..)
  , formatAuthError
  , formatRuntimeProviderError
  , errorStr
  ) where

import Control.Exception (Exception, SomeException, fromException)
import Data.Text (Text)
import qualified Data.Text as T

-- | @get_cli_command_name()@ (upstream's own command name there): the command's name,
-- which every message that names the command uses.
cliName :: Text
cliName = "fisher"

data AuthError = AuthError
  { aeMessage :: Text
  , aeProvider :: Text
  , aeCode :: Maybe Text
  , aeRelogin :: Bool
  }
  deriving (Eq, Show)

instance Exception AuthError

-- | @AuthError(message)@ with the defaults.
authError :: Text -> AuthError
authError m = AuthError m "" Nothing False

-- | Any other exception upstream raises out of the credential code:
-- @type(exc).__name__@, @type(exc).__module__@ and @str(exc)@.
data PyError = PyError
  { peType :: Text
  , peModule :: Text
  , peStr :: Text
  }
  deriving (Eq, Show)

instance Exception PyError

pyError :: Text -> Text -> PyError
pyError ty = PyError ty "builtins"

-- | @raise SystemExit(n)@.
newtype SystemExit = SystemExit Int
  deriving (Eq, Show)

instance Exception SystemExit

-- | @format_auth_error@.
formatAuthError :: AuthError -> Text
formatAuthError e
  | aeRelogin e = aeMessage e <> " Run `" <> cliName <> " model` to re-authenticate."
  | aeCode e == Just "temporarily_unavailable" = aeMessage e <> " Please retry in a few seconds."
  | otherwise = aeMessage e

-- | @str(exc)@ for whatever was raised.
errorStr :: SomeException -> Text
errorStr e
  | Just a <- fromException e = aeMessage a
  | Just p <- fromException e = peStr p
  | Just (SystemExit n) <- fromException e = T.pack (show n)
  | otherwise = T.pack (show e)

-- | @format_runtime_provider_error@: 'formatAuthError' for an AuthError,
-- else @str()@.
formatRuntimeProviderError :: SomeException -> Text
formatRuntimeProviderError e = case fromException e of
  Just a -> formatAuthError a
  Nothing -> errorStr e
