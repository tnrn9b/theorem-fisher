-- | Anthropic credentials: Claude Code's own file
-- (@~/.claude/.credentials.json@), its refresh, the order
-- @resolve_anthropic_token@ tries the sources in, and @claude
-- setup-token@. Ported from @agent/anthropic_adapter.py@.
module Fisher.LLM.Auth.Anthropic
  ( ClaudeCreds (..)
  , claudeCredentialsPath
  , readClaudeCodeCredentials
  , readClaudeManagedKey
  , isClaudeCodeTokenValid
  , refreshOauthToken
  , resolveClaudeCodeTokenFromCredentials
  , preferRefreshableClaudeCodeToken
  , isOauthToken
  , resolveAnthropicToken
  , getAnthropicTokenSource
  , runOauthSetupToken
  , anthropicOauthClientId
  ) where

import Control.Exception (AsyncException (UserInterrupt), IOException, SomeException, catch, throwIO, try)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)

import Fisher.LLM.Auth.Error
import Fisher.LLM.Auth.Util
import Fisher.LLM.Sys
import Fisher.Py.Value

data ClaudeCreds = ClaudeCreds
  { ccAccessToken :: Text
  , ccRefreshToken :: PyVal
  -- ^ as stored (@""@ when absent)
  , ccExpiresAt :: PyVal
  -- ^ milliseconds, as stored (@0@ when absent)
  }
  deriving (Eq, Show)

anthropicOauthClientId :: Text
anthropicOauthClientId = "9d1c250a-e61b-44d9-88ed-5944d1962f5e"

claudeCredentialsPath :: Sys -> FilePath
claudeCredentialsPath sys = sysHome sys </> ".claude" </> ".credentials.json"

readJsonFile :: FilePath -> IO (Maybe PyVal)
readJsonFile p = do
  exists <- doesFileExist p
  if not exists
    then pure Nothing
    else do
      r <- try (BS.readFile p) :: IO (Either IOException BS.ByteString)
      pure (either (const Nothing) (either (const Nothing) Just . loads) r)

-- | @read_claude_code_credentials@: @claudeAiOauth@ with a non-empty
-- @accessToken@.
readClaudeCodeCredentials :: Sys -> IO (Maybe ClaudeCreds)
readClaudeCodeCredentials sys = do
  d <- readJsonFile (claudeCredentialsPath sys)
  pure $ do
    o@(PDict _) <- d >>= get "claudeAiOauth"
    tok <- get "accessToken" o
    if truthy tok
      then Just (ClaudeCreds (pyStrOf tok) (fromMaybe (str "") (get "refreshToken" o)) (fromMaybe (PInt 0) (get "expiresAt" o)))
      else Nothing

-- | @read_claude_managed_key@ (diagnostics only): @~/.claude.json@
-- @primaryApiKey@.
readClaudeManagedKey :: Sys -> IO (Maybe Text)
readClaudeManagedKey sys = do
  d <- readJsonFile (sysHome sys </> ".claude.json")
  pure $ case d >>= get "primaryApiKey" of
    Just (PStr k) | not (T.null (T.strip k)) -> Just (T.strip k)
    _ -> Nothing

-- | @is_claude_code_token_valid@: no expiry means valid when a token is
-- there; otherwise a minute of margin.
isClaudeCodeTokenValid :: Sys -> ClaudeCreds -> IO Bool
isClaudeCodeTokenValid sys c
  | not (truthy (ccExpiresAt c)) = pure (not (T.null (ccAccessToken c)))
  | otherwise = do
      now <- sysNow sys
      let nowMs = floor (now * 1000) :: Integer
      pure $ case ccExpiresAt c of
        PInt e -> nowMs < e - 60000
        PFloat e -> fromInteger nowMs < e - 60000
        _ -> False

-- | @_refresh_oauth_token@: the refresh grant at console.anthropic.com
-- (urllib), the new token written back to Claude Code's file.
refreshOauthToken :: Sys -> ClaudeCreds -> IO (Maybe Text)
refreshOauthToken sys c
  | not (truthy (ccRefreshToken c)) = pure Nothing
  | otherwise = do
      let rt = pyStrOf (ccRefreshToken c)
      r <- try $ do
        resp <- send sys (urllibClient 10) "POST" "https://console.anthropic.com/v1/oauth/token"
                  [("Content-Type", "application/x-www-form-urlencoded")]
                  (FormBody [("grant_type", "refresh_token"), ("refresh_token", rt), ("client_id", anthropicOauthClientId)])
        if aStatus resp >= 400
          then pure Nothing
          else case respJson resp of
            Left _ -> pure Nothing
            Right result -> do
              let newAccess = fromMaybe (str "") (get "access_token" result)
                  newRefresh = fromMaybe (str rt) (get "refresh_token" result)
                  expiresIn = fromMaybe (PInt 3600) (get "expires_in" result)
              if truthy newAccess
                then do
                  now <- sysNow sys
                  let nowMs = floor (now * 1000) :: Integer
                      expMs = case expiresIn of
                        PInt n -> PInt (nowMs + n * 1000)
                        PFloat d -> PFloat (fromInteger nowMs + d * 1000)
                        _ -> PInt nowMs
                  writeClaudeCodeCredentials sys newAccess newRefresh expMs
                  pure (Just (pyStrOf newAccess))
                else pure Nothing
      pure (either (\(_ :: SomeException) -> Nothing) id r)

-- | @_write_claude_code_credentials@: other top-level keys kept, indent 2,
-- no trailing newline, mode 0600.
writeClaudeCodeCredentials :: Sys -> PyVal -> PyVal -> PyVal -> IO ()
writeClaudeCodeCredentials sys access refresh expMs = do
  let p = claudeCredentialsPath sys
  r <- try $ do
    existing <- fromMaybe (PDict []) <$> readJsonFile p
    let new = set "claudeAiOauth" (obj [("accessToken", access), ("refreshToken", refresh), ("expiresAt", expMs)]) existing
    createDirectoryIfMissing True (takeDirectory p)
    BS.writeFile p (TE.encodeUtf8 (dumpsIndent True 2 new))
    setFileMode p 0o600
  either (\(_ :: IOException) -> pure ()) pure r

-- | @_resolve_claude_code_token_from_credentials@.
resolveClaudeCodeTokenFromCredentials :: Sys -> Maybe ClaudeCreds -> IO (Maybe Text)
resolveClaudeCodeTokenFromCredentials sys mc = do
  creds <- maybe (readClaudeCodeCredentials sys) (pure . Just) mc
  case creds of
    Nothing -> pure Nothing
    Just c -> do
      ok <- isClaudeCodeTokenValid sys c
      if ok then pure (Just (ccAccessToken c)) else refreshOauthToken sys c

-- | @_is_oauth_token@: anything but a Console API key.
isOauthToken :: Text -> Bool
isOauthToken k = not (T.null k) && not ("sk-ant-api" `T.isPrefixOf` k)

-- | @_prefer_refreshable_claude_code_token@.
preferRefreshableClaudeCodeToken :: Sys -> Text -> Maybe ClaudeCreds -> IO (Maybe Text)
preferRefreshableClaudeCodeToken sys envTok = \case
  Just c | isOauthToken envTok, truthy (ccRefreshToken c) -> do
    r <- resolveClaudeCodeTokenFromCredentials sys (Just c)
    pure $ case r of
      Just t | not (T.null t), t /= envTok -> Just t
      _ -> Nothing
  _ -> pure Nothing

envStripped :: Sys -> Text -> IO Text
envStripped sys k = maybe "" T.strip <$> sysGetEnv sys k

-- | @resolve_anthropic_token@: @ANTHROPIC_TOKEN@, @CLAUDE_CODE_OAUTH_TOKEN@
-- (each yielding to a refreshable Claude Code file), the file, then
-- @ANTHROPIC_API_KEY@.
resolveAnthropicToken :: Sys -> IO (Maybe Text)
resolveAnthropicToken sys = do
  creds <- readClaudeCodeCredentials sys
  tok <- envStripped sys "ANTHROPIC_TOKEN"
  if not (T.null tok)
    then Just . fromMaybe tok <$> preferRefreshableClaudeCodeToken sys tok creds
    else do
      cc <- envStripped sys "CLAUDE_CODE_OAUTH_TOKEN"
      if not (T.null cc)
        then Just . fromMaybe cc <$> preferRefreshableClaudeCodeToken sys cc creds
        else do
          fromFile <- resolveClaudeCodeTokenFromCredentials sys creds
          case fromFile of
            Just t | not (T.null t) -> pure (Just t)
            _ -> do
              k <- envStripped sys "ANTHROPIC_API_KEY"
              pure (if T.null k then Nothing else Just k)

-- | @get_anthropic_token_source@.
getAnthropicTokenSource :: Sys -> Maybe Text -> IO Text
getAnthropicTokenSource sys mtok = do
  let tok = T.strip (fromMaybe "" mtok)
  if T.null tok
    then pure "none"
    else do
      envTok <- envStripped sys "ANTHROPIC_TOKEN"
      cc <- envStripped sys "CLAUDE_CODE_OAUTH_TOKEN"
      creds <- readClaudeCodeCredentials sys
      managed <- readClaudeManagedKey sys
      apiK <- envStripped sys "ANTHROPIC_API_KEY"
      pure $ case () of
        _ | not (T.null envTok) && envTok == tok -> "anthropic_token_env"
          | not (T.null cc) && cc == tok -> "claude_code_oauth_token_env"
          | Just c <- creds, ccAccessToken c == tok -> "claude_code_credentials_file"
          | managed == Just tok -> "claude_json_primary_api_key"
          | not (T.null apiK) && apiK == tok -> "anthropic_api_key_env"
          | otherwise -> "unknown"

-- | @run_oauth_setup_token@: @claude setup-token@ on the terminal, then
-- the file's token if valid, else @CLAUDE_CODE_OAUTH_TOKEN@ or
-- @ANTHROPIC_TOKEN@. A missing @claude@ raises FileNotFoundError.
runOauthSetupToken :: Sys -> IO (Maybe Text)
runOauthSetupToken sys =
  sysWhich sys "claude" >>= \case
    Nothing ->
      throwIO (pyError "FileNotFoundError" "The 'claude' CLI is not installed. Install it with: npm install -g @anthropic-ai/claude-code")
    Just path -> do
      interrupted <-
        (sysRunInteractive sys [T.pack path, "setup-token"] >>= \case
            Left e -> throwIO (pyError "FileNotFoundError" (e <> ": '" <> T.pack path <> "'"))
            Right _ -> pure False)
          `catch` \case
            UserInterrupt -> pure True
            e -> throwIO e
      if interrupted
        then pure Nothing
        else do
          creds <- readClaudeCodeCredentials sys
          valid <- maybe (pure False) (isClaudeCodeTokenValid sys) creds
          case creds of
            Just c | valid -> pure (Just (ccAccessToken c))
            _ -> firstEnv ["CLAUDE_CODE_OAUTH_TOKEN", "ANTHROPIC_TOKEN"]
  where
    firstEnv [] = pure Nothing
    firstEnv (k : ks) = do
      v <- envStripped sys k
      if T.null v then firstEnv ks else pure (Just v)
