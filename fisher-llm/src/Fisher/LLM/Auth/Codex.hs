-- | OpenAI Codex: Fisher's own copy of the ChatGPT OAuth tokens in
-- @auth.json@, the one-time import from the Codex CLI's file, refresh when
-- the JWT is about to expire, the device-code login, the status snapshot
-- and the model list. Ported from upstream @cli/auth.py@ and
-- upstream @cli/codex_models.py@.
module Fisher.LLM.Auth.Codex
  ( readCodexTokens
  , saveCodexTokens
  , refreshCodexAuthTokens
  , importCodexCliTokens
  , codexAccessTokenIsExpiring
  , CodexOpts (..)
  , defaultCodexOpts
  , resolveCodexRuntimeCredentials
  , getCodexAuthStatus
  , loginOpenAICodex
  , codexDeviceCodeLogin
    -- * Models
  , defaultCodexModels
  , addForwardCompatModels
  , getCodexModelIds
  ) where

import Control.Exception (AsyncException (UserInterrupt), IOException, SomeException, catch, fromException, throwIO, try)
import Control.Monad (void, when)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Fisher.Paths (homeLabel)

import Fisher.LLM.Auth.Config (updateConfigForProvider)
import Fisher.LLM.Auth.Error
import Fisher.LLM.Auth.Registry
import Fisher.LLM.Auth.Store
import Fisher.LLM.Auth.Util
import Fisher.LLM.Sys
import Fisher.Py.Value

codexErr :: Text -> Text -> AuthError
codexErr code msg = AuthError msg "openai-codex" (Just code) True

-- | @_read_codex_tokens(_lock=...)@: @{tokens, last_refresh}@, or the
-- AuthError for each way the stored state can be unusable.
readCodexTokens :: Sys -> Bool -> IO PyVal
readCodexTokens sys lock = do
  store <- (if lock then withAuthLock sys else id) (loadAuthStore sys)
  st <- case loadProviderState store "openai-codex" of
    Just s | truthy s -> pure s
    _ -> throwIO (codexErr "codex_auth_missing" ("No Codex credentials stored. Run `" <> cliName <> " model` to authenticate."))
  tokens <- case get "tokens" st of
    Just t@(PDict _) -> pure t
    _ -> throwIO (codexErr "codex_auth_invalid_shape" ("Codex auth state is missing tokens. Run `" <> cliName <> " model` to re-authenticate."))
  case get "access_token" tokens of
    Just (PStr a) | not (T.null (T.strip a)) -> pure ()
    _ -> throwIO (codexErr "codex_auth_missing_access_token" ("Codex auth is missing access_token. Run `" <> cliName <> " model` to re-authenticate."))
  case get "refresh_token" tokens of
    Just (PStr r) | not (T.null (T.strip r)) -> pure ()
    _ -> throwIO (codexErr "codex_auth_missing_refresh_token" ("Codex auth is missing refresh_token. Run `" <> cliName <> " model` to re-authenticate."))
  pure (obj [("tokens", tokens), ("last_refresh", fromMaybe PNull (get "last_refresh" st))])

-- | @_save_codex_tokens(tokens, last_refresh)@ (now, with a @Z@, when not
-- given). Makes Codex the active provider.
saveCodexTokens :: Sys -> PyVal -> Maybe PyVal -> IO ()
saveCodexTokens sys tokens mlast = do
  lastRefresh <- maybe (str <$> isoNowZ sys) pure mlast
  withAuthLock sys $ do
    store <- loadAuthStore sys
    let st0 = fromMaybe (PDict []) (loadProviderState store "openai-codex")
        st = set "auth_mode" (str "chatgpt") (set "last_refresh" lastRefresh (set "tokens" tokens st0))
    void (saveAuthStore sys (saveProviderState store "openai-codex" st))

-- | @_refresh_codex_auth_tokens(tokens, timeout)@.
refreshCodexAuthTokens :: Sys -> PyVal -> Double -> IO PyVal
refreshCodexAuthTokens sys tokens timeoutS = do
  rt <- case get "refresh_token" tokens of
    Just (PStr r) | not (T.null (T.strip r)) -> pure r
    _ -> throwIO (codexErr "codex_auth_missing_refresh_token" ("Codex auth is missing refresh_token. Run `" <> cliName <> " model` to re-authenticate."))
  resp <- send sys (httpxClient [("Accept", "application/json")] (max 5.0 timeoutS)) "POST" codexOAuthTokenUrl
            [("Content-Type", "application/x-www-form-urlencoded")]
            (FormBody [("grant_type", "refresh_token"), ("refresh_token", rt), ("client_id", codexOAuthClientId)])
  when (aStatus resp /= 200) $ do
    let (code, message) = case respJson resp of
          Right err@(PDict _) ->
            let c = case get "error" err of
                  Just (PStr s) | not (T.null (T.strip s)) -> T.strip s
                  _ -> "codex_refresh_failed"
                desc = case get "error_description" err of
                  Just v | truthy v -> Just v
                  _ -> get "message" err
                m = case desc of
                  Just (PStr d) | not (T.null (T.strip d)) -> "Codex token refresh failed: " <> T.strip d
                  _ -> statusMsg
             in (c, m)
          _ -> ("codex_refresh_failed", statusMsg)
        statusMsg = "Codex token refresh failed with status " <> T.pack (show (aStatus resp)) <> "."
    throwIO (AuthError message "openai-codex" (Just code) (code `elem` ["invalid_grant", "invalid_token", "invalid_request"]))
  payload <- case respJson resp of
    Right p -> pure p
    Left _ -> throwIO (codexErr "codex_refresh_invalid_json" "Codex token refresh returned invalid JSON.")
  access <- case get "access_token" payload of
    Just (PStr a) | not (T.null (T.strip a)) -> pure (T.strip a)
    _ -> throwIO (codexErr "codex_refresh_missing_access_token" "Codex token refresh response was missing access_token.")
  let t1 = set "access_token" (str access) tokens
      t2 = case get "refresh_token" payload of
        Just (PStr r) | not (T.null (T.strip r)) -> set "refresh_token" (str (T.strip r)) t1
        _ -> t1
  saveCodexTokens sys t2 Nothing
  pure t2

codexHome :: Sys -> IO FilePath
codexHome sys = do
  env <- maybe "" T.strip <$> sysGetEnv sys "CODEX_HOME"
  let raw = if T.null env then sysHome sys </> ".codex" else T.unpack env
  pure $ case raw of
    "~" -> sysHome sys
    ('~' : rest@('/' : _)) -> sysHome sys <> rest
    p -> p

readJson :: FilePath -> IO (Maybe PyVal)
readJson p = do
  exists <- doesFileExist p
  if not exists
    then pure Nothing
    else either (\(_ :: IOException) -> Nothing) (either (const Nothing) Just . loads) <$> try (BS.readFile p)

-- | @_import_codex_cli_tokens@: the tokens dict from
-- @$CODEX_HOME/auth.json@ (default @~/.codex@) when it has both tokens.
-- The file is never written.
importCodexCliTokens :: Sys -> IO (Maybe PyVal)
importCodexCliTokens sys = do
  home <- codexHome sys
  d <- readJson (home </> "auth.json")
  pure $ do
    t@(PDict _) <- d >>= get "tokens"
    if maybe False truthy (get "access_token" t) && maybe False truthy (get "refresh_token" t) then Just t else Nothing

-- | @_codex_access_token_is_expiring@: only a JWT with a numeric @exp@
-- can be expiring.
codexAccessTokenIsExpiring :: Sys -> Text -> Int -> IO Bool
codexAccessTokenIsExpiring sys tok skew = case get "exp" (decodeJwtClaims tok) of
  Just (PInt e) -> check (fromInteger e)
  Just (PFloat e) -> check e
  Just (PBool b) -> check (if b then 1 else 0)
  _ -> pure False
  where
    check e = (\now -> e <= now + fromIntegral (max 0 skew)) <$> sysNow sys

data CodexOpts = CodexOpts
  { coForceRefresh :: Bool
  , coRefreshIfExpiring :: Bool
  , coSkew :: Int
  }

defaultCodexOpts :: CodexOpts
defaultCodexOpts = CodexOpts False True codexAccessTokenRefreshSkewSeconds

-- | @resolve_codex_runtime_credentials(...)@: the stored tokens (migrated
-- from the Codex CLI when there are none, with upstream's warning),
-- refreshed under the lock when the access token is about to expire.
resolveCodexRuntimeCredentials :: Sys -> CodexOpts -> IO PyVal
resolveCodexRuntimeCredentials sys opts = do
  dat0 <-
    readCodexTokens sys True `catch` \(e :: AuthError) ->
      if aeCode e /= Just "codex_auth_missing"
        then throwIO e
        else
          importCodexCliTokens sys >>= \case
            Nothing -> throwIO e
            Just cli -> do
              outLn sys "⚠️  Migrating Codex credentials to Fisher's own auth store."
              outLn sys "   This avoids conflicts with Codex CLI and VS Code."
              outLn sys ("   Run `" <> cliName <> " model` to create a fully independent session.\n")
              saveCodexTokens sys cli Nothing
              readCodexTokens sys True
  timeoutEnv <- fromMaybe "20" <$> sysGetEnv sys "FISHER_CODEX_REFRESH_TIMEOUT_SECONDS"
  refreshTimeout <- maybe (throwIO (pyError "ValueError" ("could not convert string to float: " <> pyRepr (str timeoutEnv)))) pure (pyFloatText timeoutEnv)
  let accessOf d = T.strip (maybe "" (\v -> if truthy v then pyStrOf v else "") (getPath ["tokens", "access_token"] d))
      decide d =
        if coForceRefresh opts
          then pure True
          else if coRefreshIfExpiring opts then codexAccessTokenIsExpiring sys (accessOf d) (coSkew opts) else pure False
  should <- decide dat0
  (dat, access) <-
    if not should
      then pure (dat0, accessOf dat0)
      else withAuthLockTimeout sys (max authLockTimeoutSeconds (refreshTimeout + 5)) $ do
        d <- readCodexTokens sys False
        again <- decide d
        if again
          then do
            toks <- refreshCodexAuthTokens sys (fromMaybe (PDict []) (get "tokens" d)) refreshTimeout
            pure (d, T.strip (maybe "" pyStrOf (get "access_token" toks)))
          else pure (d, accessOf d)
  baseEnv <- maybe "" (rstripSlash . T.strip) <$> sysGetEnv sys "FISHER_CODEX_BASE_URL"
  pure $
    obj
      [ ("provider", str "openai-codex"), ("base_url", str (if T.null baseEnv then defaultCodexBaseUrl else baseEnv))
      , ("api_key", str access), ("source", str "fisher-auth-store")
      , ("last_refresh", fromMaybe PNull (get "last_refresh" dat)), ("auth_mode", str "chatgpt") ]

-- | @get_codex_auth_status@: AuthErrors become @logged_in: False@ with the
-- message; anything else propagates.
getCodexAuthStatus :: Sys -> IO PyVal
getCodexAuthStatus sys = do
  let path = str (T.pack (authFilePath sys))
  r <- try (resolveCodexRuntimeCredentials sys defaultCodexOpts)
  pure $ case r of
    Right c ->
      obj
        [ ("logged_in", PBool True), ("auth_store", path), ("last_refresh", fromMaybe PNull (get "last_refresh" c))
        , ("auth_mode", fromMaybe PNull (get "auth_mode" c)), ("source", fromMaybe PNull (get "source" c)) ]
    Left (e :: AuthError) -> obj [("logged_in", PBool False), ("auth_store", path), ("error", str (aeMessage e))]

-- | @input()@ whose EOF and Ctrl-C give the default.
askDefault :: Sys -> Text -> Text -> IO Text
askDefault sys prompt dflt = do
  r <- tryInterrupt (sysInput sys prompt)
  pure $ case r of
    Right (Just x) -> T.toLower (T.strip x)
    _ -> dflt

-- | @_login_openai_codex(args, pconfig)@: reuse Fisher's stored session,
-- or import the Codex CLI's, or run the device flow.
loginOpenAICodex :: Sys -> IO ()
loginOpenAICodex sys = do
  existing <- try (resolveCodexRuntimeCredentials sys defaultCodexOpts)
  reused <- case existing of
    Left (_ :: AuthError) -> pure False
    Right ex -> do
      outLn sys "Existing Codex credentials found in Fisher auth store."
      reuse <- askDefault sys "Use existing credentials? [Y/n]: " "y"
      if reuse `elem` ["", "y", "yes"]
        then do
          path <- updateConfigForProvider sys "openai-codex" (strOr defaultCodexBaseUrl (get "base_url" ex)) Nothing
          outLn sys ""
          outLn sys "Login successful!"
          outLn sys ("  Config updated: " <> T.pack path <> " (model.provider=openai-codex)")
          pure True
        else pure False
  if reused
    then pure ()
    else do
      cli <- importCodexCliTokens sys
      imported <- case cli of
        Nothing -> pure False
        Just toks -> do
          outLn sys "Found existing Codex CLI credentials at ~/.codex/auth.json"
          outLn sys "Fisher will create its own session to avoid conflicts with Codex CLI / VS Code."
          ans <- askDefault sys "Import these credentials? (a separate login is recommended) [y/N]: " "n"
          if ans `elem` ["y", "yes"]
            then do
              saveCodexTokens sys toks Nothing
              baseEnv <- maybe "" (rstripSlash . T.strip) <$> sysGetEnv sys "FISHER_CODEX_BASE_URL"
              path <- updateConfigForProvider sys "openai-codex" (if T.null baseEnv then defaultCodexBaseUrl else baseEnv) Nothing
              outLn sys ""
              outLn sys "Credentials imported. Note: if Codex CLI refreshes its token,"
              outLn sys "Fisher will keep working independently with its own session."
              outLn sys ("  Config updated: " <> T.pack path <> " (model.provider=openai-codex)")
              pure True
            else pure False
      if imported
        then pure ()
        else do
          outLn sys ""
          outLn sys "Signing in to OpenAI Codex..."
          outLn sys "(Fisher creates its own session — won't affect Codex CLI or VS Code)"
          outLn sys ""
          creds <- codexDeviceCodeLogin sys
          saveCodexTokens sys (fromMaybe (PDict []) (get "tokens" creds)) (get "last_refresh" creds)
          path <- updateConfigForProvider sys "openai-codex" (strOr defaultCodexBaseUrl (get "base_url" creds)) Nothing
          outLn sys ""
          outLn sys "Login successful!"
          outLn sys ("  Auth state: " <> T.pack (homeLabel (sysHome sys) (sysFisherHome sys) </> "auth.json"))
          outLn sys ("  Config updated: " <> T.pack path <> " (model.provider=openai-codex)")

-- | @_codex_device_code_login@: the user code from auth.openai.com, the
-- poll (sleep first, 403/404 mean not yet), then the authorization-code
-- exchange. Ctrl-C while polling is @SystemExit(130)@.
codexDeviceCodeLogin :: Sys -> IO PyVal
codexDeviceCodeLogin sys = do
  let issuer = "https://auth.openai.com"
      client = httpxClient [] 15.0
      failWith code prefix act =
        act `catch` \(e :: SomeException) -> case fromException e of
          Just UserInterrupt -> throwIO e
          _ -> throwIO (AuthError (prefix <> errorStr e) "openai-codex" (Just code) False)
      plain code msg = AuthError msg "openai-codex" (Just code) False
  resp <- failWith "device_code_request_failed" "Failed to request device code: " $
    send sys client "POST" (issuer <> "/api/accounts/deviceauth/usercode") [("Content-Type", "application/json")]
      (JsonBody (obj [("client_id", str codexOAuthClientId)]))
  when (aStatus resp /= 200) $
    throwIO (plain "device_code_request_error" ("Device code request returned status " <> T.pack (show (aStatus resp)) <> "."))
  device <- either throwIO pure (respJson resp)
  let userCode = maybe "" pyStrOf (get "user_code" device)
      deviceAuthId = maybe "" pyStrOf (get "device_auth_id" device)
  interval <- case pyInt (fromMaybe (str "5") (get "interval" device)) of
    Just n -> pure (max 3 n)
    Nothing -> throwIO (pyError "ValueError" ("invalid literal for int() with base 10: " <> maybe "" pyRepr (get "interval" device)))
  when (T.null userCode || T.null deviceAuthId) $
    throwIO (plain "device_code_incomplete" "Device code response missing required fields.")
  outLn sys "To continue, follow these steps:\n"
  outLn sys "  1. Open this URL in your browser:"
  outLn sys ("     \ESC[94m" <> issuer <> "/codex/device\ESC[0m\n")
  outLn sys "  2. Enter this code:"
  outLn sys ("     \ESC[94m" <> userCode <> "\ESC[0m\n")
  outLn sys "Waiting for sign-in... (press Ctrl+C to cancel)"
  start <- sysMonotonic sys
  let maxWait = 15 * 60 :: Double
      poll = do
        now <- sysMonotonic sys
        if now - start >= maxWait
          then pure Nothing
          else do
            sysSleep sys (fromInteger interval)
            r <- send sys client "POST" (issuer <> "/api/accounts/deviceauth/token") [("Content-Type", "application/json")]
                   (JsonBody (obj [("device_auth_id", str deviceAuthId), ("user_code", str userCode)]))
            case aStatus r of
              200 -> Just <$> either throwIO pure (respJson r)
              s | s == 403 || s == 404 -> poll
              s -> throwIO (plain "device_code_poll_error" ("Device auth polling returned status " <> T.pack (show s) <> "."))
  codeResp <-
    poll `catch` \case
      UserInterrupt -> outLn sys "\nLogin cancelled." >> throwIO (SystemExit 130)
      e -> throwIO e
  cr <- maybe (throwIO (plain "device_code_timeout" "Login timed out after 15 minutes.")) pure codeResp
  let authCode = maybe "" pyStrOf (get "authorization_code" cr)
      verifier = maybe "" pyStrOf (get "code_verifier" cr)
  when (T.null authCode || T.null verifier) $
    throwIO (plain "device_code_incomplete_exchange" "Device auth response missing authorization_code or code_verifier.")
  tokResp <- failWith "token_exchange_failed" "Token exchange failed: " $
    send sys client "POST" codexOAuthTokenUrl [("Content-Type", "application/x-www-form-urlencoded")]
      (FormBody [ ("grant_type", "authorization_code"), ("code", authCode), ("redirect_uri", issuer <> "/deviceauth/callback")
                , ("client_id", codexOAuthClientId), ("code_verifier", verifier) ])
  when (aStatus tokResp /= 200) $
    throwIO (plain "token_exchange_error" ("Token exchange returned status " <> T.pack (show (aStatus tokResp)) <> "."))
  toks <- either throwIO pure (respJson tokResp)
  let access = fromMaybe (str "") (get "access_token" toks)
      refresh = fromMaybe (str "") (get "refresh_token" toks)
  when (not (truthy access)) $
    throwIO (plain "token_exchange_no_access_token" "Token exchange did not return an access_token.")
  baseEnv <- maybe "" (rstripSlash . T.strip) <$> sysGetEnv sys "FISHER_CODEX_BASE_URL"
  lastRefresh <- isoNowZ sys
  pure $
    obj
      [ ("tokens", obj [("access_token", access), ("refresh_token", refresh)])
      , ("base_url", str (if T.null baseEnv then defaultCodexBaseUrl else baseEnv))
      , ("last_refresh", str lastRefresh), ("auth_mode", str "chatgpt"), ("source", str "device-code") ]

-- | @DEFAULT_CODEX_MODELS@.
defaultCodexModels :: [Text]
defaultCodexModels = ["gpt-5.3-codex", "gpt-5.2-codex", "gpt-5.1-codex-max", "gpt-5.1-codex-mini"]

-- | @_add_forward_compat_models@.
addForwardCompatModels :: [Text] -> [Text]
addForwardCompatModels ids =
  let ordered = foldl (\acc x -> if x `elem` acc then acc else acc <> [x]) [] ids
      templates =
        [ ("gpt-5.3-codex", ["gpt-5.2-codex"])
        , ("gpt-5.4", ["gpt-5.3-codex", "gpt-5.2-codex"])
        , ("gpt-5.3-codex-spark", ["gpt-5.3-codex", "gpt-5.2-codex"]) ]
   in foldl (\acc (synthetic, from) -> if synthetic `elem` acc || not (any (`elem` acc) from) then acc else acc <> [synthetic]) ordered templates

-- | The visible slugs of a models list, by priority then slug.
rankedSlugs :: [PyVal] -> [Text]
rankedSlugs items =
  map snd . sortOn id $
    [ (rank, T.strip slug)
    | it@(PDict _) <- items
    , Just (PStr slug) <- [get "slug" it]
    , not (T.null (T.strip slug))
    , get "supported_in_api" it /= Just (PBool False)
    , not (hidden (get "visibility" it))
    , let rank = case get "priority" it of
            Just (PInt p) -> p
            Just (PFloat p) -> truncate p
            Just (PBool b) -> if b then 1 else 0
            _ -> 10000 :: Integer ]
  where
    hidden = \case
      Just (PStr v) -> T.toLower (T.strip v) `elem` ["hide", "hidden"]
      _ -> False

-- | @get_codex_model_ids(access_token)@: the live list when there is a
-- token and it answers, else Codex's config default, its cache and the
-- built-in list; forward-compatible names added in every case.
getCodexModelIds :: Sys -> Maybe Text -> IO [Text]
getCodexModelIds sys mtok = do
  home <- codexHome sys
  live <- case mtok of
    Just tok | not (T.null tok) -> do
      r <- try (send sys (httpxClient [] 10) "GET" "https://chatgpt.com/backend-api/codex/models?client_version=1.0.0"
                  [("Authorization", "Bearer " <> tok)] NoBody)
      pure $ case r of
        Right resp | aStatus resp == 200, Right d <- respJson resp ->
          let entries = case d of
                PDict _ -> case get "models" d of Just (PList xs) -> xs; _ -> []
                _ -> []
           in addForwardCompatModels (rankedSlugs entries)
        Right _ -> []
        Left (_ :: SomeException) -> []
    _ -> pure []
  if not (null live)
    then pure (addForwardCompatModels live)
    else do
      dflt <- readDefaultModel (home </> "config.toml")
      cache <- readJson (home </> "models_cache.json")
      let cached = case cache >>= get "models" of
            Just (PList xs) -> foldl (\acc x -> if x `elem` acc then acc else acc <> [x]) [] (rankedSlugs xs)
            _ -> []
          add acc x = if x `elem` acc then acc else acc <> [x]
          ordered = foldl add (foldl add (maybe [] pure dflt) cached) defaultCodexModels
      pure (addForwardCompatModels ordered)

-- | @_read_default_model@: the top-level @model = "..."@ of Codex's
-- @config.toml@ (a basic string, before any table).
readDefaultModel :: FilePath -> IO (Maybe Text)
readDefaultModel path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      r <- try (BS.readFile path)
      pure $ case r of
        Left (_ :: IOException) -> Nothing
        Right bytes -> case TE.decodeUtf8' bytes of
          Left _ -> Nothing
          Right txt -> findModel (takeWhile (not . T.isPrefixOf "[" . T.strip) (T.lines txt))
  where
    findModel [] = Nothing
    findModel (l : ls) =
      let (k, rest) = T.breakOn "=" (T.strip l)
       in if T.strip k == "model" && not (T.null rest)
            then case T.uncons (T.strip (T.drop 1 rest)) of
              Just ('"', v) ->
                let m = T.strip (T.takeWhile (/= '"') v)
                 in if T.null m then Nothing else Just m
              Just ('\'', v) ->
                let m = T.strip (T.takeWhile (/= '\'') v)
                 in if T.null m then Nothing else Just m
              _ -> Nothing
            else findModel ls
