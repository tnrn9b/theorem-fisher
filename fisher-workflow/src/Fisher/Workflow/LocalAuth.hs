-- | What a machine already has for Claude Code and Codex authentication,
-- and what Fisher would stage for them. Ported from the @_has_local_*@,
-- @_resolve_*_auth*@ and @_codex_auth_payload_*@ helpers in
-- @gauss_cli/autoformalize.py@.
module Fisher.Workflow.LocalAuth
  ( claudeAuthEnvKeys
  , hasLocalClaudeLogin
  , hasLocalClaudeApiKey
  , readKeychainClaudeCredentials
  , resolveClaudeAuthEnv
  , resolveCodexApiKey
  , loadLocalCodexAuthPayload
  , codexAuthPayloadIsValid
  , codexAuthPayloadHasApiKey
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (os)
import System.Process.Typed (proc, readProcess)

import Fisher.Paths (expandUser)
import Fisher.Py (lookupKey, pyStr, pyStrOr)

claudeAuthEnvKeys :: [Text]
claudeAuthEnvKeys = ["CLAUDE_CODE_OAUTH_TOKEN", "ANTHROPIC_TOKEN", "ANTHROPIC_API_KEY"]

-- | A JSON object from a file, or an empty object for a missing file, an
-- unreadable one, or one that does not hold an object.
loadJsonObject :: FilePath -> IO Value
loadJsonObject path = do
  exists <- doesFileExist path
  if not exists
    then pure (Object KM.empty)
    else do
      r <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case r of
        Right bs | Just v@(Object _) <- decodeStrict bs -> v
        _ -> Object KM.empty

hasAccessToken :: Value -> Bool
hasAccessToken v = case lookupKey "claudeAiOauth" v of
  Just o@(Object _) -> not (T.null (T.strip (maybe "" pyStr (lookupKey "accessToken" o))))
  _ -> False

-- | The macOS Keychain entry Claude Code writes, when it holds an access
-- token. Nothing on other systems.
readKeychainClaudeCredentials :: IO (Maybe Text)
readKeychainClaudeCredentials
  | os /= "darwin" = pure Nothing
  | otherwise = do
      r <- try (readProcess (proc "security" ["find-generic-password", "-s", "Claude Code-credentials", "-w"]))
      pure $ case r :: Either SomeException (ExitCode, BL.ByteString, BL.ByteString) of
        Right (ExitSuccess, out, _) ->
          let payload = BL.toStrict out
           in case decodeStrict payload of
                Just v | hasAccessToken v -> Just (T.strip (TE.decodeUtf8With TEE.lenientDecode payload))
                _ -> Nothing
        _ -> Nothing

-- | @~/.claude/.credentials.json@ with an access token, else the Keychain
-- reader given.
hasLocalClaudeLogin :: IO (Maybe Text) -> FilePath -> IO Bool
hasLocalClaudeLogin keychain home = do
  creds <- loadJsonObject (home </> ".claude" </> ".credentials.json")
  if hasAccessToken creds then pure True else (/= Nothing) <$> keychain

-- | @primaryApiKey@ in @~/.claude.json@. As upstream, a null key reads as
-- the text "None" and so counts.
hasLocalClaudeApiKey :: FilePath -> IO Bool
hasLocalClaudeApiKey home = do
  cfg <- loadJsonObject (home </> ".claude.json")
  pure (not (T.null (T.strip (maybe "" pyStr (lookupKey "primaryApiKey" cfg)))))

envValue :: [(String, String)] -> Text -> Text
envValue env k = T.strip (T.pack (fromMaybe "" (lookup (T.unpack k) env)))

-- | The first Claude credential variable set in the given environment, or,
-- when allowed, in the persisted one.
resolveClaudeAuthEnv :: (Text -> IO (Maybe Text)) -> [(String, String)] -> Bool -> IO (Maybe (Text, Text))
resolveClaudeAuthEnv getPersisted env includePersisted = go claudeAuthEnvKeys
  where
    go [] = pure Nothing
    go (k : ks) = do
      let direct = envValue env k
      v <- if T.null direct && includePersisted then maybe "" T.strip <$> getPersisted k else pure direct
      if T.null v then go ks else pure (Just (k, v))

resolveCodexApiKey :: (Text -> IO (Maybe Text)) -> [(String, String)] -> Bool -> IO Text
resolveCodexApiKey getPersisted env includePersisted = do
  let direct = envValue env "OPENAI_API_KEY"
  if T.null direct && includePersisted then maybe "" T.strip <$> getPersisted "OPENAI_API_KEY" else pure direct

-- | @$CODEX_HOME/auth.json@, or @~/.codex/auth.json@.
loadLocalCodexAuthPayload :: FilePath -> [(String, String)] -> IO Value
loadLocalCodexAuthPayload home env = do
  codexHome <- case envValue env "CODEX_HOME" of
    t | T.null t -> pure (home </> ".codex")
    t -> expandUser (T.unpack t)
  loadJsonObject (codexHome </> "auth.json")

authMode :: Value -> Text
authMode p = T.toLower (pyStrOr (lookupKey "auth_mode" p))

apiKeyPresent :: Value -> Bool
apiKeyPresent p = not (T.null (T.strip (maybe "" pyStr (lookupKey "OPENAI_API_KEY" p))))

codexAuthPayloadIsValid :: Value -> Bool
codexAuthPayloadIsValid p
  | authMode p == "apikey" = apiKeyPresent p
  | otherwise = case lookupKey "tokens" p of
      Just t@(Object _) -> all (\k -> not (T.null (pyStrOr (lookupKey k t)))) ["access_token", "refresh_token", "id_token"]
      _ -> False

codexAuthPayloadHasApiKey :: Value -> Bool
codexAuthPayloadHasApiKey p = authMode p == "apikey" && apiKeyPresent p
