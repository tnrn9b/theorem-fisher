-- | Nous Portal: the device-code login, the runtime refresh of the access
-- token and minting of the short-lived agent key, the status snapshot and
-- the model list. Ported from @gauss_cli/auth.py@.
module Fisher.LLM.Auth.Nous
  ( LoginArgs (..)
  , defaultLoginArgs
  , loginNous
  , NousOpts (..)
  , defaultNousOpts
  , resolveNousRuntimeCredentials
  , getNousAuthStatus
  , fetchNousModels
  , Verify (..)
  ) where

import Control.Exception (AsyncException (UserInterrupt), SomeException, catch, fromException, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.LLM.Auth.Config (promptModelSelection, saveModelChoice, updateConfigForProvider)
import Fisher.LLM.Auth.Error
import Fisher.LLM.Auth.Registry
import Fisher.LLM.Auth.Store
import Fisher.LLM.Auth.Util
import Fisher.LLM.Sys
import Fisher.Py.Value

-- | TLS verification as upstream resolves it: off, a CA bundle, or on.
-- Only recorded in the stored state here (the transport is 'Sys').
data Verify = VerifyOff | VerifyCa Text | VerifyOn
  deriving (Eq, Show)

tlsOf :: Verify -> PyVal
tlsOf v =
  obj
    [ ("insecure", PBool (v == VerifyOff))
    , ("ca_bundle", case v of VerifyCa p -> str p; _ -> PNull) ]

-- | The attributes @_login_nous@ reads off its argparse namespace.
data LoginArgs = LoginArgs
  { laPortalUrl :: Maybe Text
  , laInferenceUrl :: Maybe Text
  , laClientId :: Maybe Text
  , laScope :: Maybe Text
  , laNoBrowser :: Bool
  , laTimeout :: Maybe Double
  , laCaBundle :: Maybe Text
  , laInsecure :: Bool
  }
  deriving (Eq, Show)

-- | What @fisher model@ and @fisher setup@ pass: argparse's defaults.
defaultLoginArgs :: LoginArgs
defaultLoginArgs = LoginArgs Nothing Nothing Nothing Nothing False (Just 15.0) Nothing False

nonEmptyT :: Maybe Text -> Maybe Text
nonEmptyT = \case
  Just t | not (T.null t) -> Just t
  _ -> Nothing

envT :: Sys -> Text -> IO (Maybe Text)
envT sys k = nonEmptyT <$> sysGetEnv sys k

-- | 'envT' for a renamed variable: @FISHER_X@, else @GAUSS_X@.
envTCompat :: Sys -> Text -> IO (Maybe Text)
envTCompat sys k = nonEmptyT <$> sysGetEnvCompat sys k

-- | @_login_nous(args, pconfig)@. Raises 'SystemExit' 130 on Ctrl-C and 1
-- on any other failure, as upstream.
loginNous :: Sys -> LoginArgs -> ProviderConfig -> IO ()
loginNous sys args pc = do
  fisherPortal <- envTCompat sys "FISHER_PORTAL_BASE_URL"
  nousPortal <- envT sys "NOUS_PORTAL_BASE_URL"
  nousInf <- envT sys "NOUS_INFERENCE_BASE_URL"
  fisherCa <- envTCompat sys "FISHER_CA_BUNDLE"
  sslCert <- envT sys "SSL_CERT_FILE"
  sshClient <- envT sys "SSH_CLIENT"
  sshTty <- envT sys "SSH_TTY"
  let portal = rstripSlash (fromMaybe (pcPortalBaseUrl pc) (orText [nonEmptyT (laPortalUrl args), fisherPortal, nousPortal]))
      requestedInf = rstripSlash (fromMaybe (pcInferenceBaseUrl pc) (orText [nonEmptyT (laInferenceUrl args), nousInf]))
      clientId = fromMaybe (pcClientId pc) (nonEmptyT (laClientId args))
      scope = fromMaybe (pcScope pc) (nonEmptyT (laScope args))
      timeoutS = case laTimeout args of
        Just t | t /= 0 -> t
        _ -> 15.0
      insecure = laInsecure args
      caBundle = orText [nonEmptyT (laCaBundle args), fisherCa, sslCert]
      verify
        | insecure = VerifyOff
        | Just ca <- caBundle = VerifyCa ca
        | otherwise = VerifyOn
      openBrowser = not (laNoBrowser args) && not (isJust sshClient || isJust sshTty)
      client = httpxClient [("Accept", "application/json")] timeoutS
  outLn sys ("Starting Fisher login via " <> pcName pc <> "...")
  outLn sys ("Portal: " <> portal)
  if insecure
    then outLn sys "TLS verification: disabled (--insecure)"
    else forM_ caBundle $ \ca -> outLn sys ("TLS verification: custom CA bundle (" <> ca <> ")")
  let body = do
        device <- requestDeviceCode sys client portal clientId scope
        let field k = fromMaybe PNull (get k device)
            verificationUrl = pyStrOf (field "verification_uri_complete")
            userCode = pyStrOf (field "user_code")
        expiresIn <- intOrRaise (field "expires_in")
        interval <- intOrRaise (field "interval")
        outLn sys ""
        outLn sys "To continue:"
        outLn sys ("  1. Open: " <> verificationUrl)
        outLn sys ("  2. If prompted, enter code: " <> userCode)
        when openBrowser $ do
          opened <- sysOpenBrowser sys verificationUrl
          outLn sys (if opened then "  (Opened browser for verification)" else "  Could not open browser automatically — use the URL above.")
        let effective = max 1 (min interval (toInteger deviceAuthPollIntervalCapSeconds))
        outLn sys ("Waiting for approval (polling every " <> T.pack (show effective) <> "s)...")
        token <- pollForToken sys client portal clientId (pyStrOf (field "device_code")) expiresIn interval
        now <- sysNow sys
        let ttl = coerceTtlSeconds (Just (fromMaybe (PInt 0) (get "expires_in" token)))
            inf = fromMaybe requestedInf (optionalBaseUrl (get "inference_base_url" token))
        when (inf /= requestedInf) (outLn sys ("Using portal-provided inference URL: " <> inf))
        let st =
              obj
                [ ("portal_base_url", str portal), ("inference_base_url", str inf), ("client_id", str clientId)
                , ("scope", case get "scope" token of Just v | truthy v -> v; _ -> str scope)
                , ("token_type", fromMaybe (str "Bearer") (get "token_type" token))
                , ("access_token", fromMaybe PNull (get "access_token" token))
                , ("refresh_token", fromMaybe PNull (get "refresh_token" token))
                , ("obtained_at", str (isoFromEpoch now))
                , ("expires_at", str (isoFromEpoch (now + fromInteger ttl)))
                , ("expires_in", PInt ttl), ("tls", tlsOf verify)
                , ("agent_key", PNull), ("agent_key_id", PNull), ("agent_key_expires_at", PNull)
                , ("agent_key_expires_in", PNull), ("agent_key_reused", PNull), ("agent_key_obtained_at", PNull) ]
        withAuthLock sys $ do
          store <- loadAuthStore sys
          void (saveAuthStore sys (saveProviderState store "nous" st))
        configPath <- updateConfigForProvider sys "nous" inf Nothing
        outLn sys ""
        outLn sys "Login successful!"
        outLn sys ("  Auth state: " <> T.pack (authFilePath sys))
        outLn sys ("  Config updated: " <> T.pack configPath <> " (model.provider=nous)")
        r <- try $ do
          creds <- resolveNousRuntimeCredentials sys (NousOpts (5 * 60) timeoutS (Just insecure) caBundle False)
          let runtimeKey = get "api_key" creds
              runtimeBase = strOr inf (get "base_url" creds)
          key <- case runtimeKey of
            Just (PStr k) | not (T.null k) -> pure k
            _ -> throwIO (AuthError "No runtime API key available to fetch models" "nous" (Just "invalid_token") False)
          ids <- fetchNousModels sys runtimeBase key timeoutS
          outLn sys ""
          if null ids
            then outLn sys "No models were returned by the inference API."
            else do
              sel <- promptModelSelection sys ids ""
              forM_ sel $ \m -> do
                saveModelChoice sys m
                outLn sys ("Default model set to: " <> m)
        case r of
          Right () -> pure ()
          Left (e :: SomeException)
            | Just UserInterrupt <- fromException e -> throwIO e
            | otherwise -> do
                outLn sys ""
                outLn sys ("Login succeeded, but could not fetch available models. Reason: " <> formatRuntimeProviderError e)
  body `catch` \(e :: SomeException) -> case fromException e of
    Just UserInterrupt -> outLn sys "\nLogin cancelled." >> throwIO (SystemExit 130)
    _ | Just (SystemExit _) <- fromException e -> throwIO e
      | otherwise -> outLn sys ("Login failed: " <> errorStr e) >> throwIO (SystemExit 1)
  where
    intOrRaise v = case pyInt v of
      Just n -> pure n
      Nothing -> throwIO (pyError (if v == PNull then "TypeError" else "ValueError") ("invalid literal for int() with base 10: " <> pyRepr v))

-- | @_request_device_code@.
requestDeviceCode :: Sys -> Client -> Text -> Text -> Text -> IO PyVal
requestDeviceCode sys client portal clientId scope = do
  let url = portal <> "/api/oauth/device/code"
  resp <- send sys client "POST" url [] (FormBody ([("client_id", clientId)] <> [("scope", scope) | not (T.null scope)]))
  raiseForStatus url resp
  dat <- either throwIO pure (respJson resp)
  let required = ["device_code", "user_code", "verification_uri", "verification_uri_complete", "expires_in", "interval"]
      missing = [f | f <- required, not (has f dat)]
  unless (null missing) (throwIO (pyError "ValueError" ("Device code response missing fields: " <> T.intercalate ", " missing)))
  pure dat

-- | @_poll_for_token@: every second (the cap) until approved, refused or
-- out of time.
pollForToken :: Sys -> Client -> Text -> Text -> Text -> Integer -> Integer -> IO PyVal
pollForToken sys client portal clientId deviceCode expiresIn pollInterval = do
  start <- sysNow sys
  let deadline = start + fromInteger (max 1 expiresIn)
      url = portal <> "/api/oauth/token"
      loop interval = do
        now <- sysNow sys
        if now >= deadline
          then throwIO (pyError "TimeoutError" "Timed out waiting for device authorization")
          else do
            resp <- send sys client "POST" url []
                      (FormBody [("grant_type", "urn:ietf:params:oauth:grant-type:device_code"), ("client_id", clientId), ("device_code", deviceCode)])
            if aStatus resp == 200
              then do
                payload <- either throwIO pure (respJson resp)
                unless (has "access_token" payload) (throwIO (pyError "ValueError" "Token response did not include access_token"))
                pure payload
              else case respJson resp of
                Left _ -> do
                  raiseForStatus url resp
                  throwIO (pyError "RuntimeError" "Token endpoint returned a non-JSON error response")
                Right err -> do
                  let code = maybe "" pyStrOf (get "error" err)
                  case code of
                    "authorization_pending" -> sysSleep sys (fromInteger interval) >> loop interval
                    "slow_down" -> do
                      let i' = min (interval + 1) 30
                      sysSleep sys (fromInteger i')
                      loop i'
                    _ -> do
                      let desc = case get "error_description" err of
                            Just v | truthy v -> pyStrOf v
                            _ -> "Unknown authentication error"
                      throwIO (pyError "RuntimeError" (code <> ": " <> desc))
  loop (max 1 (min pollInterval (toInteger deviceAuthPollIntervalCapSeconds)))

-- | @_refresh_access_token@.
refreshAccessToken :: Sys -> Client -> Text -> Text -> Text -> IO PyVal
refreshAccessToken sys client portal clientId refreshToken = do
  resp <- send sys client "POST" (portal <> "/api/oauth/token") []
            (FormBody [("grant_type", "refresh_token"), ("client_id", clientId), ("refresh_token", refreshToken)])
  if aStatus resp == 200
    then do
      payload <- either throwIO pure (respJson resp)
      unless (has "access_token" payload) $
        throwIO (AuthError "Refresh response missing access_token" "nous" (Just "invalid_token") True)
      pure payload
    else case respJson resp of
      Left _ -> throwIO (AuthError "Refresh token exchange failed" "nous" Nothing True)
      Right err -> do
        let code = maybe "invalid_grant" pyStrOf (get "error" err)
            desc = case get "error_description" err of
              Just v | truthy v -> pyStrOf v
              _ -> "Refresh token exchange failed"
        throwIO (AuthError desc "nous" (Just code) (code `elem` ["invalid_grant", "invalid_token"]))

-- | @_mint_agent_key@.
mintAgentKey :: Sys -> Client -> Text -> Text -> Int -> IO PyVal
mintAgentKey sys client portal access minTtl = do
  resp <- send sys client "POST" (portal <> "/api/oauth/agent-key") [("Authorization", "Bearer " <> access)]
            (JsonBody (obj [("min_ttl_seconds", PInt (toInteger (max 60 minTtl)))]))
  if aStatus resp == 200
    then do
      payload <- either throwIO pure (respJson resp)
      unless (has "api_key" payload) $
        throwIO (AuthError "Mint response missing api_key" "nous" (Just "server_error") False)
      pure payload
    else case respJson resp of
      Left _ -> throwIO (AuthError "Agent key mint request failed" "nous" (Just "server_error") False)
      Right err -> do
        let code = maybe "server_error" pyStrOf (get "error" err)
            desc = case get "error_description" err of
              Just v | truthy v -> pyStrOf v
              _ -> "Agent key mint request failed"
        throwIO (AuthError desc "nous" (Just code) (code `elem` ["invalid_token", "invalid_grant"]))

-- | @fetch_nous_models@: the ids from @{base}/models@ without upstream
-- Gauss's own models (ids containing @gauss@, Nous's names, kept), opus first, then pro, then the rest, then sonnet.
fetchNousModels :: Sys -> Text -> Text -> Double -> IO [Text]
fetchNousModels sys base key timeoutS = do
  resp <- send sys (httpxClient [("Accept", "application/json")] timeoutS) "GET" (rstripSlash base <> "/models")
            [("Authorization", "Bearer " <> key)] NoBody
  when (aStatus resp /= 200) $ do
    let dflt = "/models request failed with status " <> T.pack (show (aStatus resp))
        desc = case respJson resp of
          Right err@(PDict _) -> case (get "error_description" err, get "error" err) of
            (Just v, _) | truthy v -> pyStrOf v
            (_, Just v) | truthy v -> pyStrOf v
            _ -> dflt
          _ -> dflt
    throwIO (AuthError desc "nous" (Just "models_fetch_failed") False)
  payload <- either throwIO pure (respJson resp)
  case get "data" payload of
    Just (PList items) -> do
      let ids =
            [ T.strip m
            | PDict kvs <- items
            , Just (PStr m) <- [lookup "id" kvs]
            , not (T.null (T.strip m))
            , not ("gauss" `T.isInfixOf` T.toLower (T.strip m)) ]
          prio mid =
            let low = T.toLower mid
             in if "opus" `T.isInfixOf` low then 0 :: Int
                  else if "pro" `T.isInfixOf` low && not ("sonnet" `T.isInfixOf` low) then 1
                  else if "sonnet" `T.isInfixOf` low then 3
                  else 2
      pure (dedupe (sortOn (\m -> (prio m, m)) ids))
    _ -> pure []
  where
    dedupe = foldl (\acc x -> if x `elem` acc then acc else acc <> [x]) []

data NousOpts = NousOpts
  { noMinKeyTtl :: Int
  , noTimeout :: Double
  , noInsecure :: Maybe Bool
  , noCaBundle :: Maybe Text
  , noForceMint :: Bool
  }

defaultNousOpts :: NousOpts
defaultNousOpts = NousOpts defaultAgentKeyMinTtlSeconds 15.0 Nothing Nothing False

-- | @resolve_nous_runtime_credentials(...)@: under the auth lock, refresh
-- the access token if it is within two minutes of expiry, mint an agent
-- key unless the cached one lives long enough, persist, and return the
-- runtime dict.
resolveNousRuntimeCredentials :: Sys -> NousOpts -> IO PyVal
resolveNousRuntimeCredentials sys opts = do
  let minTtl = max 60 (noMinKeyTtl opts)
  _sequenceId <- sysUuidHex sys
  (st, inf, usedCache) <- withAuthLock sys $ do
    store0 <- loadAuthStore sys
    st0 <- case loadProviderState store0 "nous" of
      Just s | truthy s -> pure s
      _ -> throwIO (AuthError "Fisher is not logged into Nous Portal." "nous" Nothing True)
    fisherPortal <- envTCompat sys "FISHER_PORTAL_BASE_URL"
    nousPortal <- envT sys "NOUS_PORTAL_BASE_URL"
    nousInf <- envT sys "NOUS_INFERENCE_BASE_URL"
    fisherCa <- envTCompat sys "FISHER_CA_BUNDLE"
    sslCert <- envT sys "SSL_CERT_FILE"
    let portal = rstripSlash (fromMaybe defaultNousPortalUrl (orText [optionalBaseUrl (get "portal_base_url" st0), fisherPortal, nousPortal]))
        inf0 = rstripSlash (fromMaybe defaultNousInferenceUrl (orText [optionalBaseUrl (get "inference_base_url" st0), nousInf]))
        clientId = case get "client_id" st0 of
          Just v | truthy v -> pyStrOf v
          _ -> defaultNousClientId
        tls = case get "tls" st0 of
          Just t@(PDict _) -> t
          _ -> PDict []
        insecure = fromMaybe (maybe False truthy (get "insecure" tls)) (noInsecure opts)
        ca = orText [nonEmptyT (noCaBundle opts), case get "ca_bundle" tls of Just v | truthy v -> Just (pyStrOf v); _ -> Nothing, fisherCa, sslCert]
        verify
          | insecure = VerifyOff
          | Just c <- ca = VerifyCa c
          | otherwise = VerifyOn
        client = httpxClient [("Accept", "application/json")] (if noTimeout opts /= 0 then noTimeout opts else 15.0)
    storeRef <- newIORef store0
    stRef <- newIORef st0
    infRef <- newIORef inf0
    let persist = do
          s <- readIORef stRef
          store <- readIORef storeRef
          store' <- saveAuthStore sys (saveProviderState store "nous" s)
          writeIORef storeRef store'
        field k = get k <$> readIORef stRef
        setField k v = modifyIORef stRef (set k v)
        applyRefresh refreshed oldRefresh = do
          now <- sysNow sys
          let ttl = coerceTtlSeconds (get "expires_in" refreshed)
          cur <- readIORef stRef
          setField "access_token" (fromMaybe PNull (get "access_token" refreshed))
          setField "refresh_token" (case get "refresh_token" refreshed of Just v | truthy v -> v; _ -> str oldRefresh)
          setField "token_type" $ case (get "token_type" refreshed, get "token_type" cur) of
            (Just v, _) | truthy v -> v
            (_, Just v) | truthy v -> v
            _ -> str "Bearer"
          setField "scope" $ case get "scope" refreshed of
            Just v | truthy v -> v
            _ -> fromMaybe PNull (get "scope" cur)
          forM_ (optionalBaseUrl (get "inference_base_url" refreshed)) (writeIORef infRef)
          setField "obtained_at" (str (isoFromEpoch now))
          setField "expires_in" (PInt ttl)
          setField "expires_at" (str (isoFromEpoch (now + fromInteger ttl)))
          persist
    access0 <- field "access_token"
    case access0 of
      Just (PStr a) | not (T.null a) -> pure ()
      _ -> throwIO (AuthError "No access token found for Nous Portal login." "nous" Nothing True)
    expiring <- isExpiring sys (get "expires_at" st0) accessTokenRefreshSkewSeconds
    when expiring $ do
      rt <- field "refresh_token"
      case rt of
        Just (PStr r) | not (T.null r) -> do
          refreshed <- refreshAccessToken sys client portal clientId r
          applyRefresh refreshed r
        _ -> throwIO (AuthError "Session expired and no refresh token is available." "nous" Nothing True)
    stNow <- readIORef stRef
    usable <- agentKeyIsUsable stNow minTtl
    used <-
      if not (noForceMint opts) && usable
        then pure True
        else do
          accessNow <- maybe "" pyStrOf <$> field "access_token"
          payload <-
            mintAgentKey sys client portal accessNow minTtl `catch` \(e :: AuthError) -> do
              latest <- field "refresh_token"
              case latest of
                Just (PStr r) | not (T.null r), aeCode e `elem` [Just "invalid_token", Just "invalid_grant"] -> do
                  refreshed <- refreshAccessToken sys client portal clientId r
                  applyRefresh refreshed r
                  accessNew <- maybe "" pyStrOf <$> field "access_token"
                  mintAgentKey sys client portal accessNew minTtl
                _ -> throwIO e
          now <- sysNow sys
          setField "agent_key" (fromMaybe PNull (get "api_key" payload))
          setField "agent_key_id" (fromMaybe PNull (get "key_id" payload))
          setField "agent_key_expires_at" (fromMaybe PNull (get "expires_at" payload))
          setField "agent_key_expires_in" (fromMaybe PNull (get "expires_in" payload))
          setField "agent_key_reused" (PBool (maybe False truthy (get "reused" payload)))
          setField "agent_key_obtained_at" (str (isoFromEpoch now))
          forM_ (optionalBaseUrl (get "inference_base_url" payload)) (writeIORef infRef)
          pure False
    inf <- readIORef infRef
    setField "portal_base_url" (str portal)
    setField "inference_base_url" (str inf)
    setField "client_id" (str clientId)
    setField "tls" (tlsOf verify)
    persist
    s <- readIORef stRef
    pure (s, inf, used)
  apiKey <- case get "agent_key" st of
    Just (PStr k) | not (T.null k) -> pure k
    _ -> throwIO (AuthError "Failed to resolve a Nous inference API key" "nous" (Just "server_error") False)
  let expiresAt = fromMaybe PNull (get "agent_key_expires_at" st)
  now <- sysNow sys
  let expiresIn = case parseIsoTimestamp expiresAt of
        Just e -> max 0 (truncate (e - now))
        Nothing -> coerceTtlSeconds (get "agent_key_expires_in" st)
  pure $
    obj
      [ ("provider", str "nous"), ("base_url", str inf), ("api_key", str apiKey)
      , ("key_id", fromMaybe PNull (get "agent_key_id" st)), ("expires_at", expiresAt)
      , ("expires_in", PInt expiresIn), ("source", str (if usedCache then "cache" else "portal")) ]
  where
    agentKeyIsUsable s ttl = case get "agent_key" s of
      Just (PStr k) | not (T.null (T.strip k)) -> not <$> isExpiring sys (get "agent_key_expires_at" s) ttl
      _ -> pure False

-- | @get_nous_auth_status@ (a plain read: expiry is not checked).
getNousAuthStatus :: Sys -> IO PyVal
getNousAuthStatus sys = do
  ms <- getProviderAuthState sys "nous"
  pure $ case ms of
    Just s | truthy s ->
      obj
        [ ("logged_in", PBool (maybe False truthy (get "access_token" s)))
        , ("portal_base_url", fromMaybe PNull (get "portal_base_url" s))
        , ("inference_base_url", fromMaybe PNull (get "inference_base_url" s))
        , ("access_expires_at", fromMaybe PNull (get "expires_at" s))
        , ("agent_key_expires_at", fromMaybe PNull (get "agent_key_expires_at" s))
        , ("has_refresh_token", PBool (maybe False truthy (get "refresh_token" s))) ]
    _ ->
      obj
        [ ("logged_in", PBool False), ("portal_base_url", PNull), ("inference_base_url", PNull)
        , ("access_expires_at", PNull), ("agent_key_expires_at", PNull), ("has_refresh_token", PBool False) ]
