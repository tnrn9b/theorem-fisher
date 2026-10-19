-- | @migrate_config@ and @check_config_version@ from @gauss_cli/config.py@.
-- The printed lines are upstream's; they go through the given writer so a
-- test can read them back.
module Fisher.Config.Migrate
  ( MigrationResult (..)
  , Prompter (..)
  , noPrompter
  , configVersionOf
  , checkConfigVersionAt
  , missingConfigFields
  , migrateConfigAt
  , pyStrDeep
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Fisher.Compat (lookupEnvCompatNamed)
import Fisher.Config
import Fisher.Env (saveEnvValueAt)
import Fisher.EnvVars
import Fisher.Py (lookupKey, pyStr)

data MigrationResult = MigrationResult
  { envAdded :: [Text]
  , configAdded :: [Text]
  , migrationWarnings :: [Text]
  }
  deriving (Eq, Show)

-- | How the interactive path asks for a value: a plain line, or a secret
-- (no echo). 'noPrompter' answers nothing.
data Prompter = Prompter
  { promptLine :: Text -> IO Text
  , promptSecret :: Text -> IO Text
  }

noPrompter :: Prompter
noPrompter = Prompter (const (pure "")) (const (pure ""))

-- | @config.get("_config_version", 0)@ as a number; a non-number counts
-- as 0, where upstream would raise comparing it.
configVersionOf :: Value -> Integer
configVersionOf cfg = case lookupKey "_config_version" cfg of
  Just (Number n) -> either floor id (floatingOrInteger n :: Either Double Integer)
  _ -> 0

checkConfigVersionAt :: FilePath -> IO (Integer, Integer)
checkConfigVersionAt home = do
  (cfg, _) <- loadConfigAt home
  pure (configVersionOf cfg, fromIntegral latestConfigVersion)

-- | Keys in the defaults, at any depth, that the loaded configuration lacks.
-- Keys starting with @_@ are skipped, and a branch is entered only where
-- both sides hold a mapping. Keys are visited in sorted order; upstream
-- visits them in its own insertion order.
missingConfigFields :: Value -> [(Text, Value)]
missingConfigFields = go "" defaultConfig
  where
    go prefix (Object d) (Object c) =
      concat
        [ let k = K.toText key
              full = if T.null prefix then k else prefix <> "." <> k
           in case KM.lookup key c of
                Nothing -> [(full, dv)]
                Just cv@(Object _) | Object _ <- dv -> go full dv cv
                Just _ -> []
        | (key, dv) <- KM.toAscList d
        , not ("_" `T.isPrefixOf` K.toText key) ]
    go _ _ _ = []

-- | @_set_nested@: create or replace mappings along a dotted path.
setNested :: [Text] -> Value -> Value -> Value
setNested [] v _ = v
setNested (k : ks) v (Object o) =
  let key = K.fromText k
      inner = case KM.lookup key o of
        Just x@(Object _) -> x
        _ -> Object KM.empty
   in Object (KM.insert key (setNested ks v inner) o)
setNested ks v _ = setNested ks v (Object KM.empty)

-- | Python's @str()@ of a value, containers included: what
-- @f"{default}"@ prints for a nested default.
pyStrDeep :: Value -> Text
pyStrDeep = \case
  Object o -> "{" <> T.intercalate ", " [repr (String (K.toText k)) <> ": " <> repr v | (k, v) <- KM.toList o] <> "}"
  Array a -> "[" <> T.intercalate ", " (map repr (V.toList a)) <> "]"
  v -> pyStr v
  where
    repr = \case
      String s -> "'" <> s <> "'"
      v -> pyStrDeep v

lower :: Text -> Text
lower = T.toLower

-- | Upstream's migration. Each step re-loads the configuration, as it does,
-- so a step sees what the one before it saved.
migrateConfigAt :: FilePath -> Bool -> Bool -> Prompter -> (Text -> IO ()) -> IO MigrationResult
migrateConfigAt home interactive quiet prompter say = do
  envRef <- newIORef []
  cfgRef <- newIORef []
  warnRef <- newIORef []
  let addEnv x = modifyIORef' envRef (<> [x])
      addCfg x = modifyIORef' cfgRef (<> [x])
      addWarn x = modifyIORef' warnRef (<> [x])
      out = unless quiet . say
      getEnv = getEnvValueAt home
      getEnvNamed = getEnvValueNamedAt home
  (current, latest) <- checkConfigVersionAt home

  -- Version 3 to 4: tool progress moves from .env to config.yaml.
  when (current < 4) $ do
    (cfg, _) <- loadConfigAt home
    let display = case lookupKey "display" cfg of
          Just d@(Object _) -> d
          _ -> Object KM.empty
    when (isNothing (lookupKey "tool_progress" display)) $ do
      -- FISHER_TOOL_PROGRESS, else GAUSS_TOOL_PROGRESS; the message names
      -- the one read.
      oldEnabled <- getEnvNamed "FISHER_TOOL_PROGRESS"
      oldMode <- getEnvNamed "FISHER_TOOL_PROGRESS_MODE"
      let nonEmpty = \case
            Just (n, t) | not (T.null t) -> Just (n, t)
            _ -> Nothing
      progress <- case (nonEmpty oldEnabled, nonEmpty oldMode) of
        (Just (n, e), _) | lower e `elem` ["false", "0", "no"] -> do
          addCfg ("display.tool_progress=off (from " <> n <> "=false)")
          pure "off"
        (_, Just (n, md)) | lower md `elem` ["new", "all"] -> do
          addCfg ("display.tool_progress=" <> lower md <> " (from " <> n <> ")")
          pure (lower md)
        _ -> do
          addCfg "display.tool_progress=all (default)"
          pure "all"
      saveConfigAt home (setNested ["display"] (setNested ["tool_progress"] (String progress) display) cfg)
      out ("  ✓ Migrated tool progress to config.yaml: " <> progress)

  -- Version 4 to 5: a timezone field.
  when (current < 5) $ do
    (cfg, _) <- loadConfigAt home
    when (isNothing (lookupKey "timezone" cfg)) $ do
      tzVar <- lookupEnvCompatNamed "FISHER_TIMEZONE"
      let oldTz = maybe "" (T.strip . T.pack . snd) tzVar
          tzName = maybe "FISHER_TIMEZONE" (T.pack . fst) tzVar
      if T.null oldTz
        then addCfg "timezone= (empty, uses server-local)"
        else addCfg ("timezone=" <> oldTz <> " (from " <> tzName <> ")")
      saveConfigAt home (setNested ["timezone"] (String oldTz) cfg)
      out ("  ✓ Added timezone to config.yaml: " <> (if T.null oldTz then "(server-local)" else oldTz))

  when (current < latest) $
    out ("Config version: " <> T.pack (show current) <> " → " <> T.pack (show latest))

  -- Required variables (none at the pinned upstream).
  missingRequired <- fmap concat . forM requiredEnvVars $ \v -> do
    val <- getEnv (evName v)
    pure [v | maybe True T.null val]
  unless (null missingRequired) $ do
    out ""
    out "⚠️  Missing required environment variables:"
    forM_ missingRequired $ \v -> out ("   • " <> evName v <> ": " <> evDescription v)
  when (interactive && not (null missingRequired)) $ do
    say ""
    say "Let's configure them now:"
    say ""
    forM_ missingRequired $ \v -> do
      forM_ (evUrl v) $ \u -> say ("  Get your key at: " <> u)
      value <- T.strip <$> (if evPassword v then promptSecret prompter else promptLine prompter) ("  " <> evPrompt v <> ": ")
      if T.null value
        then addWarn ("Skipped " <> evName v <> " - some features may not work")
        else do
          saveEnvValueAt home (evName v) value
          addEnv (evName v)
          say ("  ✓ Saved " <> evName v)
      say ""

  -- New optional variables since the user's version, offered interactively.
  let newNames = sort . concat $ [Map.findWithDefault [] (fromIntegral ver) envVarsByVersion | ver <- [current + 1 .. latest]]
      byName = Map.fromList [(evName v, v) | v <- optionalEnvVars]
  when (not (null newNames) && interactive && not quiet) $ do
    newAndUnset <- fmap concat . forM newNames $ \n -> do
      val <- getEnv n
      pure [v | maybe True T.null val, Just v <- [Map.lookup n byName]]
    unless (null newAndUnset) $ do
      say ""
      say ("  " <> T.pack (show (length newAndUnset)) <> " new optional key(s) in this update:")
      forM_ newAndUnset $ \v -> say ("    • " <> evName v <> " — " <> evDescription v)
      say ""
      answer <- T.toLower . T.strip <$> promptLine prompter "  Configure new keys? [y/N]: "
      if answer `elem` ["y", "yes"]
        then do
          say ""
          forM_ newAndUnset $ \v -> do
            say ("  " <> (if T.null (evDescription v) then evName v else evDescription v))
            forM_ (evUrl v) $ \u -> say ("  Get your key at: " <> u)
            value <- T.strip <$> (if evPassword v then promptSecret prompter else promptLine prompter)
                       ("  " <> evPrompt v <> " (Enter to skip): ")
            unless (T.null value) $ do
              saveEnvValueAt home (evName v) value
              addEnv (evName v)
              say ("  ✓ Saved " <> evName v)
            say ""
        else say "  Set later with: fisher config set <key> <value>"

  -- Missing config fields get their defaults, and the version is bumped.
  (loaded, _) <- loadConfigAt home
  let missing = missingConfigFields loaded
      bump = setNested ["_config_version"] (Number (fromIntegral latest))
  if not (null missing)
    then do
      let filled = foldl (\c (k, d) -> setNested (T.splitOn "." k) d c) loaded missing
      forM_ missing $ \(k, d) -> do
        addCfg k
        out ("  ✓ Added " <> k <> " = " <> pyStrDeep d)
      saveConfigAt home (bump filled)
    else when (current < latest) $ saveConfigAt home (bump loaded)

  MigrationResult <$> readIORef envRef <*> readIORef cfgRef <*> readIORef warnRef
