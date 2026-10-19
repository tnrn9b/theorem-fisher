-- | The names Fisher had before it was renamed from Gauss, read as a
-- fallback. Fisher always writes the new names; every function here reads
-- the new name first and the old one second.
--
-- * Environment: @FISHER_X@, else @GAUSS_X@ ('lookupEnvCompat',
--   'lookupCompat'). A child process gets both ('childEnvCompat',
--   'exportEnvCompat'), since it may be an older Fisher or an upstream tool.
-- * Project directory: @.fisher@, else @.gauss@ ('projectDirName').
-- * Configuration: @fisher.<path>@, else @gauss.<path>@, per leaf
--   ('lookupConfigCompat', 'normalizeConfigCompat', 'configPathCompat').
--   A load folds the file's @gauss@ section into @fisher@; a save writes
--   @fisher@ and keeps the file's @gauss@ section as it was
--   ('legacySection', 'withLegacySection'), so an older binary sharing the
--   file keeps its settings.
--
-- Precedence of variables, in every reader here:
--
-- * A set @FISHER_X@ wins even when it is empty: 'lookupEnvCompat' and
--   'lookupCompat' then answer @Just ""@ and never look at @GAUSS_X@
--   (as 'lookupEnv' treats an empty value as set). Callers that ignore
--   empty values (the home, see 'Fisher.Paths.getFisherHome') test each
--   name themselves.
-- * The process environment and a home's @.env@ are one source per name:
--   'Fisher.EnvVars.getEnvValueAt' reads @FISHER_X@ from the environment,
--   then from @.env@, and only then @GAUSS_X@ (environment, then @.env@).
--   So @FISHER_X@ in @.env@ beats @GAUSS_X@ in the environment.
-- * Backend name: @gauss@ is read as @fisher@ ('canonicalBackendName').
--
-- The home directory chain (@FISHER_HOME@, @GAUSS_HOME@, @~/.fisher@,
-- @~/.gauss@) is 'Fisher.Paths.getFisherHome', built on these.
module Fisher.Compat
  ( -- * Environment variables
    newEnvPrefix
  , oldEnvPrefix
  , envNames
  , lookupEnvCompat
  , lookupEnvCompatNamed
  , lookupEnvCompatWith
  , lookupCompat
  , lookupCompatNamed
  , childEnvCompat
  , exportEnvCompat
    -- * The project directory
  , fisherDirName
  , gaussDirName
  , projectDirNames
  , projectDirName
    -- * Configuration keys
  , configRoot
  , legacyConfigRoot
  , lookupConfigCompat
  , normalizeConfigCompat
  , configPathCompat
  , legacySection
  , withLegacySection
    -- * Backend names
  , nativeBackend
  , canonicalBackendName
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesPathExist)
import System.Environment (getEnvironment, lookupEnv, setEnv)
import System.FilePath ((</>))

-- ------------------------------------------------------------ environment

newEnvPrefix, oldEnvPrefix :: String
newEnvPrefix = "FISHER_"
oldEnvPrefix = "GAUSS_"

-- | The new and the old name of a variable. The argument is the full new
-- name (@FISHER_HOME@), the full old name (@GAUSS_HOME@) or the bare suffix
-- (@HOME@): all three give @(\"FISHER_HOME\", \"GAUSS_HOME\")@.
envNames :: String -> (String, String)
envNames name =
  let suffix = fromMaybe name (stripPrefix newEnvPrefix name `orElse` stripPrefix oldEnvPrefix name)
   in (newEnvPrefix <> suffix, oldEnvPrefix <> suffix)
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b

-- | @FISHER_X@ when set (an empty value counts, as with 'lookupEnv'), else
-- @GAUSS_X@.
lookupEnvCompat :: String -> IO (Maybe String)
lookupEnvCompat name = fmap snd <$> lookupEnvCompatNamed name

-- | 'lookupEnvCompat' over any lookup (a scripted or recorded environment):
-- the new name when the lookup has it, else the old.
lookupEnvCompatWith :: Monad m => (Text -> m (Maybe Text)) -> Text -> m (Maybe Text)
lookupEnvCompatWith get name = do
  let (new, old) = envNames (T.unpack name)
  get (T.pack new) >>= maybe (get (T.pack old)) (pure . Just)

-- | 'lookupEnvCompat', with the name the value was found under, for
-- messages that say where a setting came from.
lookupEnvCompatNamed :: String -> IO (Maybe (String, String))
lookupEnvCompatNamed name = do
  let (new, old) = envNames name
  lookupEnv new >>= \case
    Just v -> pure (Just (new, v))
    Nothing -> fmap (old,) <$> lookupEnv old

-- | 'lookupEnvCompat' over an environment held as a list.
lookupCompat :: String -> [(String, String)] -> Maybe String
lookupCompat name env = snd <$> lookupCompatNamed name env

-- | 'lookupCompat', with the name the value was found under.
lookupCompatNamed :: String -> [(String, String)] -> Maybe (String, String)
lookupCompatNamed name env =
  let (new, old) = envNames name
   in case lookup new env of
        Just v -> Just (new, v)
        Nothing -> (old,) <$> lookup old env

-- | An environment for a child process: every @FISHER_X@ is also given as
-- @GAUSS_X@ with the same value (replacing any @GAUSS_X@ already there),
-- so a child that only knows the old names sees what Fisher meant.
childEnvCompat :: [(String, String)] -> [(String, String)]
childEnvCompat env =
  let mirrored = [(snd (envNames k), v) | (k, v) <- env, newEnvPrefix `isPrefixOf` k]
      shadowed = map fst mirrored
   in [kv | kv@(k, _) <- env, k `notElem` shadowed] <> mirrored

-- | 'childEnvCompat' on this process's own environment, so children that
-- inherit it see both names. (An empty @FISHER_X@ unsets @GAUSS_X@, as
-- 'setEnv' does with an empty value.)
exportEnvCompat :: IO ()
exportEnvCompat = do
  env <- getEnvironment
  sequence_ [setEnv (snd (envNames k)) v | (k, v) <- env, newEnvPrefix `isPrefixOf` k]

-- ------------------------------------------------------- project directory

fisherDirName, gaussDirName :: FilePath
fisherDirName = ".fisher"
gaussDirName = ".gauss"

-- | Both directory names, the new one first.
projectDirNames :: [FilePath]
projectDirNames = [fisherDirName, gaussDirName]

-- | The project directory's name under a root: @.fisher@ when it exists,
-- else @.gauss@ when it exists, else @.fisher@ (a new project).
projectDirName :: FilePath -> IO FilePath
projectDirName root = do
  new <- doesPathExist (root </> fisherDirName)
  if new
    then pure fisherDirName
    else do
      old <- doesPathExist (root </> gaussDirName)
      pure (if old then gaussDirName else fisherDirName)

-- ------------------------------------------------------- configuration keys

configRoot, legacyConfigRoot :: Text
configRoot = "fisher"
legacyConfigRoot = "gauss"

lookupKeyV :: Text -> Value -> Maybe Value
lookupKeyV k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

getPathV :: [Text] -> Value -> Maybe Value
getPathV [] v = Just v
getPathV (k : ks) v = lookupKeyV k v >>= getPathV ks

-- | Mappings merged leaf by leaf, the second winning.
mergeV :: Value -> Value -> Value
mergeV (Object a) (Object b) = Object (KM.unionWith pick a b)
  where
    pick x y = case (x, y) of
      (Object _, Object _) -> mergeV x y
      _ -> y
mergeV _ b = b

-- | The value at @fisher.<path>@, else at @gauss.<path>@. Where both hold
-- a mapping the two are merged leaf by leaf, @fisher@ winning, so a
-- section read whole still falls back per leaf.
lookupConfigCompat :: [Text] -> Value -> Maybe Value
lookupConfigCompat path cfg =
  case (getPathV (configRoot : path) cfg, getPathV (legacyConfigRoot : path) cfg) of
    (Just new@(Object _), Just old@(Object _)) -> Just (mergeV old new)
    (Just new, _) -> Just new
    (Nothing, old) -> old

-- | A configuration with its @gauss@ section folded into @fisher@ (leaf by
-- leaf, @fisher@ winning) and the @gauss@ key gone. Applied to a user file
-- before the defaults are merged under it, so a @gauss.*@ setting beats a
-- @fisher.*@ default.
normalizeConfigCompat :: Value -> Value
normalizeConfigCompat (Object o) = case KM.lookup old o of
  Nothing -> Object o
  Just g ->
    let merged = case KM.lookup new o of
          Nothing -> g
          Just f -> mergeV g f
     in Object (KM.insert new merged (KM.delete old o))
  where
    new = K.fromText configRoot
    old = K.fromText legacyConfigRoot
normalizeConfigCompat v = v

-- | The top-level @gauss@ section of a configuration, if any.
legacySection :: Value -> Maybe Value
legacySection = lookupKeyV legacyConfigRoot

-- | A configuration to save, with the old file's @gauss@ section put back
-- unchanged (when it had one). Applied after 'normalizeConfigCompat', so
-- the configuration's own settings are all under @fisher@.
withLegacySection :: Maybe Value -> Value -> Value
withLegacySection (Just g) (Object o) = Object (KM.insert (K.fromText legacyConfigRoot) g o)
withLegacySection _ v = v

-- | A dotted path as typed (@fisher config set gauss.x.y v@): one under
-- @gauss@ is written under @fisher@.
configPathCompat :: [Text] -> [Text]
configPathCompat = \case
  (k : ks) | k == legacyConfigRoot -> configRoot : ks
  ks -> ks

-- ----------------------------------------------------------------- backends

-- | The native backend's name.
nativeBackend :: Text
nativeBackend = "fisher"

-- | A backend name as given (flag, config or environment) to the name Fisher
-- uses and prints: @gauss@ (any case, surrounding space ignored) is
-- @fisher@; anything else is unchanged.
canonicalBackendName :: Text -> Text
canonicalBackendName b
  | T.toLower (T.strip b) == legacyConfigRoot = nativeBackend
  | otherwise = b
