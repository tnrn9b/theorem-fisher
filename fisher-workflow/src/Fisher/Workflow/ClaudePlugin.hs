-- | The lean4 plugin in a Claude Code profile: its marketplace and
-- installed-plugin records, syncing a prewarmed install into a managed
-- home, and installing it through the @claude@ CLI. Ported from the
-- @_claude_*@ helpers in @gauss_cli/autoformalize.py@.
module Fisher.Workflow.ClaudePlugin
  ( marketplaceName
  , pluginId
  , isoNow
  , settingsPath
  , pluginCacheRoot
  , mergeMarketplaceSettings
  , findInstalledPluginRoot
  , userPluginStateIsHealthy
  , ensureUserPluginState
  , syncPrewarmedPlugin
  , installManagedPlugin
  , installLocalPlugin
  , keepOnlyManagedPlugin
  ) where

import Control.Exception (catch)
import Control.Monad (unless, when)
import Data.Aeson (Value (..), eitherDecodeStrict, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (formatTime, defaultTimeLocale)
import qualified Data.Vector as V
import System.Directory
import System.FilePath (isAbsolute, splitDirectories, takeDirectory, takeFileName, (</>))
import System.Posix.Files (createSymbolicLink)

import Fisher.Paths (expandUser)
import Fisher.Workflow.Assets (removeExistingPath)
import Fisher.Workflow.Json
import Fisher.Workflow.Run (runStaging)
import Fisher.Workflow.Types

marketplaceName, pluginName, pluginId, marketplaceRepo :: Text
marketplaceName = "lean4-skills"
pluginName = "lean4"
pluginId = "lean4@lean4-skills"
marketplaceRepo = "cameronfreer/lean4-skills"

-- | @datetime.now(timezone.utc).isoformat(timespec="milliseconds")@ with
-- the offset written @Z@.
isoNow :: Machine -> IO Text
isoNow m = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" <$> mNow m

settingsPath, pluginsRoot, knownMarketplacesPath, installedPluginsPath, marketplacesRoot, marketplaceRoot, pluginCacheRoot
  :: FilePath -> FilePath
settingsPath h = h </> ".claude" </> "settings.json"
pluginsRoot h = h </> ".claude" </> "plugins"
knownMarketplacesPath h = pluginsRoot h </> "known_marketplaces.json"
installedPluginsPath h = pluginsRoot h </> "installed_plugins.json"
marketplacesRoot h = pluginsRoot h </> "marketplaces"
marketplaceRoot h = marketplacesRoot h </> T.unpack marketplaceName
pluginCacheRoot h = pluginsRoot h </> "cache" </> T.unpack marketplaceName </> T.unpack pluginName

marketplaceSource :: Value
marketplaceSource = object ["source" .= ("github" :: Text), "repo" .= marketplaceRepo]

key :: Text -> K.Key
key = K.fromText

objAt :: Text -> KM.KeyMap Value -> KM.KeyMap Value
objAt k o = fromMaybe KM.empty (objectOf (KM.lookup (key k) o))

-- | @_extract_claude_path@: a non-blank field, user-expanded.
extractPath :: KM.KeyMap Value -> Text -> IO (Maybe FilePath)
extractPath entry field = case getText field entry of
  t | T.null t -> pure Nothing
  t -> Just <$> expandUser (T.unpack t)

-- | @_path_text_within_root@.
pathWithin :: FilePath -> FilePath -> IO Bool
pathWithin p root
  | null p || not (isAbsolute p) = pure False
  | otherwise = do
      r <- expandUser root
      pure (splitDirectories r `isPrefixOf` splitDirectories p)

mergeMarketplaceSettings :: FilePath -> IO ()
mergeMarketplaceSettings home = do
  s <- loadJsonObject (settingsPath home)
  let entry = KM.insert "autoUpdate" (Bool True) (KM.insert "source" marketplaceSource (objAt marketplaceName (objAt "extraKnownMarketplaces" s)))
      markets = KM.insert (key marketplaceName) (Object entry) (objAt "extraKnownMarketplaces" s)
      enabled = KM.insert (key pluginId) (Bool True) (objAt "enabledPlugins" s)
  writeJsonObject (settingsPath home) (KM.insert "enabledPlugins" (Object enabled) (KM.insert "extraKnownMarketplaces" (Object markets) s))

upsertKnownMarketplace :: Machine -> FilePath -> Maybe FilePath -> Maybe (KM.KeyMap Value) -> IO ()
upsertKnownMarketplace m home location template = do
  payload <- loadJsonObject (knownMarketplacesPath home)
  now <- isoNow m
  let entry0 = fromMaybe (objAt marketplaceName payload) template
      entry1 = KM.insert "autoUpdate" (Bool True) (KM.insert "source" marketplaceSource entry0)
      entry2 = maybe entry1 (\l -> KM.insert "installLocation" (String (T.pack l)) entry1) location
      entry = if KM.member "lastUpdated" entry2 then entry2 else KM.insert "lastUpdated" (String now) entry2
  writeJsonObject (knownMarketplacesPath home) (KM.insert (key marketplaceName) (Object entry) payload)

readKnownMarketplace :: FilePath -> IO (Maybe (KM.KeyMap Value))
readKnownMarketplace home = objectOf . KM.lookup (key marketplaceName) <$> loadJsonObject (knownMarketplacesPath home)

-- | @_select_claude_installed_plugin_entry@: from a list, the first entry
-- whose install path exists (when that is required), else the first entry.
selectInstalledEntry :: KM.KeyMap Value -> Bool -> IO (Maybe (KM.KeyMap Value))
selectInstalledEntry payload requireExisting =
  case objectOf (KM.lookup "plugins" payload) >>= KM.lookup (key pluginId) of
    Just (Array xs) -> do
      let cands = [o | Object o <- V.toList xs]
          pick [] = pure Nothing
          pick (c : cs) = do
            p <- extractPath c "installPath"
            ok <- case p of
              Nothing -> pure False
              Just path -> if requireExisting then doesPathExist path else pure True
            if ok then pure (Just c) else pick cs
      found <- pick cands
      pure $ case found of
        Just c -> Just c
        Nothing -> case V.toList xs of
          (Object o : _) -> Just o
          _ -> Nothing
    Just (Object o) -> pure (Just o)
    _ -> pure Nothing

findInstalledPluginRoot :: FilePath -> IO (Maybe FilePath)
findInstalledPluginRoot home = do
  payload <- loadJsonObject (installedPluginsPath home)
  entry <- selectInstalledEntry payload True
  case entry of
    Nothing -> pure Nothing
    Just e -> extractPath e "installPath" >>= \case
      Nothing -> pure Nothing
      Just p -> doesPathExist p >>= \x -> pure (if x then Just p else Nothing)

writeInstalledEntry :: Machine -> FilePath -> FilePath -> Text -> Maybe (KM.KeyMap Value) -> IO ()
writeInstalledEntry m home installPath version template = do
  payload <- loadJsonObject (installedPluginsPath home)
  now <- isoNow m
  let base = fromMaybe KM.empty template
      scope = let t = getText "scope" base in if T.null t then "user" else t
      e1 = KM.insert "version" (String version) (KM.insert "installPath" (String (T.pack installPath)) (KM.insert "scope" (String scope) base))
      e2 = if KM.member "installedAt" e1 then e1 else KM.insert "installedAt" (String now) e1
      e3 = if KM.member "lastUpdated" e2 then e2 else KM.insert "lastUpdated" (fromMaybe (String now) (KM.lookup "installedAt" e2)) e2
      plugins = KM.insert (key pluginId) (Array (V.singleton (Object e3))) (objAt "plugins" payload)
  writeJsonObject (installedPluginsPath home) (KM.insert "plugins" (Object plugins) (KM.insert "version" (Number 2) payload))

deleteKnownMarketplace :: FilePath -> IO ()
deleteKnownMarketplace home = do
  payload <- loadJsonObject (knownMarketplacesPath home)
  when (KM.member (key marketplaceName) payload) $
    writeJsonObject (knownMarketplacesPath home) (KM.delete (key marketplaceName) payload)

deleteInstalledEntry :: FilePath -> IO ()
deleteInstalledEntry home = do
  payload <- loadJsonObject (installedPluginsPath home)
  case objectOf (KM.lookup "plugins" payload) of
    Just plugins | KM.member (key pluginId) plugins -> do
      let p1 = KM.insert "plugins" (Object (KM.delete (key pluginId) plugins)) payload
          p2 = if KM.member "version" p1 then p1 else KM.insert "version" (Number 2) p1
      writeJsonObject (installedPluginsPath home) p2
    _ -> pure ()

scrubPluginState :: FilePath -> IO ()
scrubPluginState home = do
  deleteKnownMarketplace home
  deleteInstalledEntry home
  removeExistingPath (marketplaceRoot home)
  removeExistingPath (pluginCacheRoot home)

userPluginStateIsHealthy :: FilePath -> IO Bool
userPluginStateIsHealthy home =
  readKnownMarketplace home >>= \case
    Nothing -> pure False
    Just entry
      | KM.lookup "source" entry /= Just marketplaceSource -> pure False
      | otherwise -> extractPath entry "installLocation" >>= \case
          Nothing -> pure False
          Just root -> do
            inside <- pathWithin root (marketplacesRoot home)
            exists <- doesPathExist root
            if not (inside && exists) then pure False else do
              payload <- loadJsonObject (installedPluginsPath home)
              selectInstalledEntry payload True >>= \case
                Nothing -> pure False
                Just pe -> extractPath pe "installPath" >>= \case
                  Nothing -> pure False
                  Just ip -> do
                    inCache <- pathWithin ip (pluginCacheRoot home)
                    (inCache &&) <$> doesPathExist ip

findMarketplaceRoot :: FilePath -> IO (Maybe FilePath)
findMarketplaceRoot home = do
  fromEntry <- readKnownMarketplace home >>= \case
    Nothing -> pure Nothing
    Just e -> extractPath e "installLocation" >>= \case
      Just l -> doesPathExist l >>= \x -> pure (if x then Just l else Nothing)
      Nothing -> pure Nothing
  case fromEntry of
    Just l -> pure (Just l)
    Nothing -> do
      let d = marketplaceRoot home
      x <- doesPathExist d
      pure (if x then Just d else Nothing)

alreadyInstalled :: WorkflowError -> Bool
alreadyInstalled e = "already installed" `T.isInfixOf` T.toLower (errorMessage e)

addMarketplace :: FilePath -> Env -> Text -> Text -> IO ()
addMarketplace claude env target prefix =
  (() <$ runStaging [claude, "plugin", "marketplace", "add", T.unpack target] prefix (Just env) Nothing)
    `catch` \e -> unless (alreadyInstalled e) (throwW e)

installPluginTarget :: FilePath -> Env -> Text -> Text -> Text -> IO ()
installPluginTarget claude env name pid prefix = go [name, pid] []
  where
    go [] failures = throwW (StagingError (last failures))
    go (t : ts) failures = do
      r <- (Right <$> runStaging [claude, "plugin", "install", T.unpack t] prefix (Just env) Nothing)
             `catch` (pure . Left)
      case r of
        Right _ -> pure ()
        Left e
          | alreadyInstalled e -> pure ()
          | otherwise -> go ts (failures <> [errorMessage e])

-- | @_ensure_claude_user_plugin_state@, used by prewarm: make the user's
-- own profile carry a healthy lean4 install.
ensureUserPluginState :: Machine -> FilePath -> FilePath -> Env -> IO FilePath
ensureUserPluginState m claude home base = do
  createDirectoryIfMissing True home
  mergeMarketplaceSettings home
  existing <- findInstalledPluginRoot home
  healthy <- userPluginStateIsHealthy home
  case existing of
    Just p | healthy -> do
      upsertKnownMarketplace m home (Just (marketplaceRoot home)) Nothing
      pure p
    _ -> do
      scrubPluginState home
      let env = Map.insert "HOME" home base
      addMarketplace claude env marketplaceRepo "Failed to register the Lean4 Claude marketplace in the user profile"
      installPluginTarget claude env pluginName pluginId "Failed to install the Lean4 Claude plugin in the user profile"
      mergeMarketplaceSettings home
      upsertKnownMarketplace m home (Just (marketplaceRoot home)) Nothing
      findInstalledPluginRoot home >>= maybe
        (throwW (StagingError ("Managed Lean Claude plugin is not installed after configuration: " <> pluginId)))
        pure

-- | Point a link at a tree, replacing whatever was there.
replaceTreeLink :: FilePath -> FilePath -> IO ()
replaceTreeLink dest src = do
  removeExistingPath dest
  createDirectoryIfMissing True (takeDirectory dest)
  createSymbolicLink src dest

-- | @_sync_prewarmed_claude_plugin@: reuse the user's installed plugin in
-- a managed home through links, and write the managed home's records.
syncPrewarmedPlugin :: Machine -> FilePath -> FilePath -> IO (Maybe FilePath)
syncPrewarmedPlugin m realHome backendHome = do
  root <- findInstalledPluginRoot realHome
  hasSettings <- doesFileExist (settingsPath realHome)
  payload <- loadJsonObject (installedPluginsPath realHome)
  entry <- selectInstalledEntry payload True
  case (root, entry) of
    (Just pluginRoot, Just pe) | hasSettings -> do
      let version = let v = getText "version" pe in if T.null v then T.pack (takeFileName pluginRoot) else v
      removeExistingPath (pluginsRoot backendHome)
      createDirectoryIfMissing True (backendHome </> ".claude")
      copyFileWithMetadata (settingsPath realHome) (backendHome </> ".claude" </> "settings.json")
      let managedInstall = pluginCacheRoot backendHome </> T.unpack version
      replaceTreeLink managedInstall pluginRoot
      marketEntry <- readKnownMarketplace realHome
      marketRoot <- findMarketplaceRoot realHome
      let managedMarket = marketplaceRoot backendHome
      case marketRoot of
        Just r -> replaceTreeLink managedMarket r
        Nothing -> createDirectoryIfMissing True managedMarket
      mergeMarketplaceSettings backendHome
      upsertKnownMarketplace m backendHome (Just managedMarket) marketEntry
      writeInstalledEntry m backendHome managedInstall version (Just pe)
      pure (Just managedInstall)
    _ -> pure Nothing

readMarketplaceNameAt :: Text -> FilePath -> IO Text
readMarketplaceNameAt label root = do
  let f = root </> ".claude-plugin" </> "marketplace.json"
  name <- getText "name" <$> loadJsonObject f
  when (T.null name) $ throwW (StagingError ("Managed " <> label <> " marketplace manifest is missing a name: " <> T.pack f))
  pure name

readPluginIdentity :: Text -> FilePath -> IO (Text, Text)
readPluginIdentity label src = do
  let f = src </> ".claude-plugin" </> "plugin.json"
  p <- loadJsonObject f
  let name = getText "name" p
      version = getText "version" p
  when (T.null name || T.null version) $
    throwW (StagingError ("Managed " <> label <> " plugin manifest is missing name/version: " <> T.pack f))
  pure (name, version)

-- | @_install_managed_claude_plugin@: register the local checkout as a
-- marketplace in the managed home, install from it, and read back where.
installManagedPlugin :: FilePath -> FilePath -> Env -> FilePath -> FilePath -> IO FilePath
installManagedPlugin = installLocalPlugin "Lean"

-- | Register a local marketplace directory in the managed home, install one
-- of its plugins, and read back where Claude Code put it. The label names
-- the prover in the error messages (@Lean@ for lean4-skills, as upstream;
-- @Rocq@ or @Isabelle@ for Fisher's own @fisher-provers@).
installLocalPlugin :: Text -> FilePath -> FilePath -> Env -> FilePath -> FilePath -> IO FilePath
installLocalPlugin label claude backendHome base marketSource pluginSource = do
  mname <- readMarketplaceNameAt label marketSource
  (pname, _) <- readPluginIdentity label pluginSource
  let pid = pname <> "@" <> mname
      env = Map.insert "HOME" backendHome base
      what = "managed " <> label <> " Claude"
  removeExistingPath (backendHome </> ".claude" </> "plugins")
  addMarketplace claude env (T.pack marketSource) ("Failed to register the " <> what <> " marketplace")
  installPluginTarget claude env pname pid ("Failed to install the " <> what <> " plugin")
  out <- runStaging [claude, "plugin", "list", "--json"] ("Failed to inspect the " <> what <> " plugin state") (Just env) Nothing
  entries <- case eitherDecodeStrict (TE.encodeUtf8 (if T.null (T.strip out) then "[]" else out)) of
    Left _ -> throwW (StagingError ("Failed to parse the " <> what <> " plugin state."))
    Right (Array xs) -> pure [o | Object o <- V.toList xs]
    Right _ -> throwW (StagingError ("Managed " <> label <> " Claude plugin state is not a JSON list."))
  case [e | e <- entries, pyText "id" e == pid] of
    (e : _) -> do
      p <- expandUser (T.unpack (pyText "installPath" e))
      exists <- doesPathExist p
      unless exists $ throwW (StagingError ("Managed " <> label <> " Claude plugin install path does not exist: " <> T.pack p))
      canonicalizePath p
    [] -> throwW (StagingError ("Managed " <> label <> " Claude plugin is not installed after configuration: " <> pid))

-- | Leave only this plugin of Fisher's enabled in a managed home's
-- settings: the other provers' plugins (and their marketplaces) that an
-- earlier session there enabled go; the user's own entries stay. The file
-- is rewritten only when something goes.
keepOnlyManagedPlugin :: FilePath -> Text -> IO ()
keepOnlyManagedPlugin home keep = do
  let managedIds = ["lean4@lean4-skills", "rocq@fisher-provers", "isabelle@fisher-provers"]
      keepMarket = T.drop 1 (T.dropWhile (/= '@') keep)
      otherMarkets = [m | m <- ["lean4-skills", "fisher-provers"], m /= keepMarket]
      dropKeys ks o = KM.filterWithKey (\k _ -> K.toText k `notElem` ks) o
  exists <- doesFileExist (settingsPath home)
  when exists $ do
    s <- loadJsonObject (settingsPath home)
    let enabled = objAt "enabledPlugins" s
        markets = objAt "extraKnownMarketplaces" s
        enabled' = dropKeys [i | i <- managedIds, i /= keep] enabled
        markets' = dropKeys otherMarkets markets
        s' = (if KM.member "enabledPlugins" s then KM.insert "enabledPlugins" (Object enabled') else id)
               . (if KM.member "extraKnownMarketplaces" s then KM.insert "extraKnownMarketplaces" (Object markets') else id)
               $ s
    when (s' /= s) $ writeJsonObject (settingsPath home) s'
