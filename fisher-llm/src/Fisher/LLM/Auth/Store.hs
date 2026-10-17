-- | @~/.gauss/auth.json@ and its lock: loading (with the legacy
-- @systems@ shape migrated), saving (indent 2, version and @updated_at@
-- set, written beside the file and renamed over it, owner-only), the
-- per-provider state, and the active provider. Ported from
-- @gauss_cli/auth.py@.
module Fisher.LLM.Auth.Store
  ( authFilePath
  , authLockPath
  , withAuthLock
  , withAuthLockTimeout
  , loadAuthStore
  , saveAuthStore
  , loadProviderState
  , saveProviderState
  , getProviderAuthState
  , getActiveProvider
  , clearProviderAuth
  , deactivateProvider
  , isExpiring
  , coerceTtlSeconds
  , optionalBaseUrl
  ) where

import Control.Exception (IOException, finally, throwIO, try)
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import GHC.IO.Handle.Lock (LockMode (..), hTryLock, hUnlock)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (IOMode (ReadWriteMode), hClose, openFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (setFileMode)

import Fisher.LLM.Auth.Error (pyError)
import Fisher.LLM.Auth.Registry (authLockTimeoutSeconds)
import Fisher.LLM.Auth.Util
import Fisher.LLM.Sys
import Fisher.Py.Value

authFilePath :: Sys -> FilePath
authFilePath sys = sysGaussHome sys </> "auth.json"

-- | @auth.json@ with its suffix swapped: @auth.lock@.
authLockPath :: Sys -> FilePath
authLockPath sys = sysGaussHome sys </> "auth.lock"

-- | Lock depth held by this process: the lock is reentrant, as upstream's
-- thread-local counter makes it (the credential code runs on one thread).
lockDepth :: IORef Int
lockDepth = unsafePerformIO (newIORef 0)
{-# NOINLINE lockDepth #-}

withAuthLock :: Sys -> IO a -> IO a
withAuthLock sys = withAuthLockTimeout sys authLockTimeoutSeconds

-- | @_auth_store_lock(timeout)@: an exclusive advisory lock on
-- @auth.lock@ (created with the default mode, left empty), retried every
-- 50 ms until the timeout, reentrant.
withAuthLockTimeout :: Sys -> Double -> IO a -> IO a
withAuthLockTimeout sys timeout act = do
  depth <- readIORef lockDepth
  if depth > 0
    then (modifyIORef' lockDepth (+ 1) >> act) `finally` modifyIORef' lockDepth (subtract 1)
    else do
      let path = authLockPath sys
      createDirectoryIfMissing True (takeDirectory path)
      h <- openFile path ReadWriteMode
      let release = void (try (hUnlock h) :: IO (Either IOException ())) >> hClose h
      flip finally release $ do
        start <- sysNow sys
        let deadline = start + max 1 timeout
            acquire = do
              ok <- either (const False) id <$> (try (hTryLock h ExclusiveLock) :: IO (Either IOException Bool))
              unless ok $ do
                now <- sysNow sys
                when (now >= deadline) (throwIO (pyError "TimeoutError" "Timed out waiting for auth store lock"))
                sysSleep sys 0.05
                acquire
        acquire
        writeIORef lockDepth 1
        act `finally` writeIORef lockDepth 0

emptyStore :: PyVal
emptyStore = obj [("version", PInt 1), ("providers", PDict [])]

-- | @_load_auth_store@.
loadAuthStore :: Sys -> IO PyVal
loadAuthStore sys = do
  let path = authFilePath sys
  exists <- doesFileExist path
  if not exists
    then pure emptyStore
    else do
      r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      pure $ case r of
        Left _ -> emptyStore
        Right bytes -> case loads bytes of
          Right raw@(PDict _) | Just (PDict _) <- get "providers" raw -> raw
          Right raw@(PDict _) | Just systems@(PDict _) <- get "systems" raw ->
            let providers = maybe [] (\s -> [("nous", s)]) (get "nous_portal" systems)
             in obj
                  [ ("version", PInt 1), ("providers", PDict providers)
                  , ("active_provider", if null providers then PNull else str "nous") ]
          _ -> emptyStore

-- | @_save_auth_store@: returns the store as written (version and
-- @updated_at@ set).
saveAuthStore :: Sys -> PyVal -> IO PyVal
saveAuthStore sys store0 = do
  let path = authFilePath sys
  createDirectoryIfMissing True (takeDirectory path)
  now <- isoNow sys
  let store = set "updated_at" (str now) (set "version" (PInt 1) store0)
      payload = dumpsIndent True 2 store <> "\n"
  hex <- sysUuidHex sys
  let tmp = takeDirectory path </> (takeFileName path <> ".tmp." <> show (sysPid sys) <> "." <> T.unpack hex)
  (BS.writeFile tmp (TE.encodeUtf8 payload) >> renameFile tmp path)
    `finally` (void (try (removeFile tmp) :: IO (Either IOException ())))
  void (try (setFileMode path 0o600) :: IO (Either IOException ()))
  pure store

-- | @_load_provider_state@: a copy of the provider's dict, if it is one.
loadProviderState :: PyVal -> Text -> Maybe PyVal
loadProviderState store pid = case get "providers" store of
  Just ps@(PDict _) -> case get pid ps of
    Just st@(PDict _) -> Just st
    _ -> Nothing
  _ -> Nothing

-- | @_save_provider_state@: stores the state and makes the provider the
-- active one.
saveProviderState :: PyVal -> Text -> PyVal -> PyVal
saveProviderState store pid st =
  let providers = case get "providers" store of
        Just ps@(PDict _) -> ps
        _ -> PDict []
   in set "active_provider" (str pid) (set "providers" (set pid st providers) store)

getProviderAuthState :: Sys -> Text -> IO (Maybe PyVal)
getProviderAuthState sys pid = (`loadProviderState` pid) <$> loadAuthStore sys

-- | @get_active_provider@: the stored value as it is (None when absent).
getActiveProvider :: Sys -> IO PyVal
getActiveProvider sys = maybe PNull id . get "active_provider" <$> loadAuthStore sys

-- | @clear_provider_auth(provider_id)@: drop the provider's state (the
-- active one when none is named); True when something was removed.
clearProviderAuth :: Sys -> Maybe Text -> IO Bool
clearProviderAuth sys mpid = withAuthLock sys $ do
  store <- loadAuthStore sys
  let active = get "active_provider" store
      target = case mpid of
        Just p | not (T.null p) -> Just p
        _ -> case active of
          Just (PStr a) | not (T.null a) -> Just a
          _ -> Nothing
  case target of
    Nothing -> pure False
    Just t -> case get "providers" store of
      Just ps | has t ps -> do
        let store1 = set "providers" (delete t ps) store
            store2 = if active == Just (str t) then set "active_provider" PNull store1 else store1
        _ <- saveAuthStore sys store2
        pure True
      _ -> pure False

-- | @deactivate_provider@: no active provider, credentials kept.
deactivateProvider :: Sys -> IO ()
deactivateProvider sys = withAuthLock sys $ do
  store <- loadAuthStore sys
  void (saveAuthStore sys (set "active_provider" PNull store))

-- | @_is_expiring(expires_at, skew)@: unparseable counts as expiring.
isExpiring :: Sys -> Maybe PyVal -> Int -> IO Bool
isExpiring sys v skew = case v >>= parseIsoTimestamp of
  Nothing -> pure True
  Just e -> (\now -> e <= now + fromIntegral skew) <$> sysNow sys

-- | @_coerce_ttl_seconds@.
coerceTtlSeconds :: Maybe PyVal -> Integer
coerceTtlSeconds v = max 0 (maybe 0 id (v >>= pyInt))

-- | @_optional_base_url@.
optionalBaseUrl :: Maybe PyVal -> Maybe Text
optionalBaseUrl = \case
  Just (PStr s) -> let c = rstripSlash (T.strip s) in if T.null c then Nothing else Just c
  _ -> Nothing
