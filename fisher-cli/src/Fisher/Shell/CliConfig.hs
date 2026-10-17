-- | The interactive shell's view of the configuration, as upstream's
-- @cli.py@ builds it in @load_cli_config@: its own defaults, each
-- top-level section of @config.yaml@ laid over them one level deep, and the
-- terminal settings copied into the environment for the children.
module Fisher.Shell.CliConfig
  ( loadCliConfig
  , cliDefaults
  , terminalEnv
  , saveConfigValueAt
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import System.Posix.Files (setFileMode)

import Fisher.Yaml.Ordered (encodeOrdered, keyOrderOfFile)

-- | The parts of @load_cli_config@'s defaults the shell reads.
cliDefaults :: Value
cliDefaults =
  object
    [ "model" .= object ["default" .= String "anthropic/claude-opus-4.6", "base_url" .= String "https://openrouter.ai/api/v1", "provider" .= String "auto"]
    , "terminal" .= object
        [ "env_type" .= String "local", "cwd" .= String ".", "timeout" .= Number 60, "lifetime_seconds" .= Number 300
        , "docker_image" .= String "python:3.11", "singularity_image" .= String "docker://python:3.11"
        , "modal_image" .= String "python:3.11", "daytona_image" .= String "nikolaik/python-nodejs:python3.11-nodejs20"
        , "docker_volumes" .= Array mempty, "docker_mount_cwd_to_workspace" .= Bool False ]
    , "cli" .= object ["handoff" .= object ["mode" .= String "auto", "launchers" .= object []]]
    , "agent" .= object ["max_turns" .= Number 90, "verbose" .= Bool False]
    , "toolsets" .= [String "all"]
    , "display" .= object
        [ "compact" .= Bool False, "resume_display" .= String "full", "show_reasoning" .= Bool False
        , "show_cost" .= Bool False, "skin" .= String "default" ]
    ]

-- | @load_cli_config@ (without touching the environment): the merged
-- configuration, and whether the file had a terminal section.
loadCliConfig :: FilePath -> IO (Value, Bool)
loadCliConfig path = do
  exists <- doesFileExist path
  file <-
    if exists
      then either (const (Object KM.empty)) id <$> (Y.decodeFileEither path :: IO (Either Y.ParseException Value))
      else pure (Object KM.empty)
  let defs = case cliDefaults of
        Object o -> o
        _ -> KM.empty
      fileO = case file of
        Object o -> o
        _ -> KM.empty
      withModel = case KM.lookup "model" fileO of
        Just (String m) -> adjustKey (setKey "default" (String m)) "model" defs
        Just (Object m) -> adjustKey (\case Object d -> Object (m <> d); other -> other) "model" defs
        _ -> defs
      merged =
        KM.mapWithKey
          (\k v -> if k == "model" then v else case (v, KM.lookup k fileO) of
              (Object d, Just (Object f)) -> Object (f <> d)
              (_, Just f) -> f
              (_, Nothing) -> v)
          withModel
      extra = KM.filterWithKey (\k _ -> k /= "model" && not (KM.member k defs)) fileO
      all' = merged <> extra
      agentHasTurns = case KM.lookup "agent" fileO of
        Just (Object a) -> maybe False (/= Null) (KM.lookup "max_turns" a)
        _ -> False
      legacyTurns = case KM.lookup "max_turns" fileO of
        Just t | not agentHasTurns -> Just t
        _ -> Nothing
      final = case legacyTurns of
        Just t -> adjustKey (setKey "max_turns" t) "agent" all'
        Nothing -> all'
  pure (Object final, KM.member "terminal" fileO)
  where
    adjustKey f k m = maybe m (\v -> KM.insert k (f v) m) (KM.lookup k m)
    setKey k v = \case
      Object o -> Object (KM.insert k v o)
      other -> other

-- | The environment variables @load_cli_config@ sets from the terminal
-- section, for a local backend with @cwd: .@ resolved to @cwd@. Each is
-- set when the file had a terminal section or the variable is unset.
terminalEnv :: Value -> FilePath -> [(String, String)]
terminalEnv cfg cwd =
  let t = case cfg of
        Object o | Just (Object x) <- KM.lookup "terminal" o -> x
        _ -> KM.empty
      t1 = maybe t (\b -> KM.insert "env_type" b t) (KM.lookup "backend" t)
      backend = case KM.lookup "env_type" t1 of
        Just (String b) -> b
        _ -> "local"
      t2 = case KM.lookup "cwd" t1 of
        Just (String c) | c `elem` [".", "auto", "cwd"] ->
          if backend == "local" then KM.insert "cwd" (String (T.pack cwd)) t1 else KM.delete "cwd" t1
        _ -> t1
      mapping =
        [ ("env_type", "TERMINAL_ENV"), ("cwd", "TERMINAL_CWD"), ("timeout", "TERMINAL_TIMEOUT")
        , ("lifetime_seconds", "TERMINAL_LIFETIME_SECONDS"), ("docker_image", "TERMINAL_DOCKER_IMAGE")
        , ("singularity_image", "TERMINAL_SINGULARITY_IMAGE"), ("modal_image", "TERMINAL_MODAL_IMAGE")
        , ("daytona_image", "TERMINAL_DAYTONA_IMAGE"), ("ssh_host", "TERMINAL_SSH_HOST"), ("ssh_user", "TERMINAL_SSH_USER")
        , ("ssh_port", "TERMINAL_SSH_PORT"), ("ssh_key", "TERMINAL_SSH_KEY"), ("container_cpu", "TERMINAL_CONTAINER_CPU")
        , ("container_memory", "TERMINAL_CONTAINER_MEMORY"), ("container_disk", "TERMINAL_CONTAINER_DISK")
        , ("container_persistent", "TERMINAL_CONTAINER_PERSISTENT"), ("docker_volumes", "TERMINAL_DOCKER_VOLUMES")
        , ("docker_mount_cwd_to_workspace", "TERMINAL_DOCKER_MOUNT_CWD_TO_WORKSPACE"), ("sandbox_dir", "TERMINAL_SANDBOX_DIR")
        , ("persistent_shell", "TERMINAL_PERSISTENT_SHELL"), ("sudo_password", "SUDO_PASSWORD") ]
   in mapMaybe (\(k, e) -> (\v -> (e, render v)) <$> KM.lookup (K.fromText k) t2) mapping
  where
    render = \case
      String s -> T.unpack s
      Number n -> either (show :: Double -> String) (show :: Integer -> String) (floatingOrInteger n)
      Bool b -> if b then "True" else "False"
      Null -> "None"
      v -> BL8.unpack (encode v)

-- | @save_config_value@: read config.yaml as written, set one dotted path,
-- write it back (owner-only). False when that fails.
saveConfigValueAt :: FilePath -> [Text] -> Value -> IO Bool
saveConfigValueAt path keys v = do
  r <- try $ do
    createDirectoryIfMissing True (takeDirectory path)
    exists <- doesFileExist path
    current <-
      if exists
        then either (const (Object KM.empty)) (fromMaybe (Object KM.empty) . nonNull) <$> (Y.decodeFileEither path :: IO (Either Y.ParseException Value))
        else pure (Object KM.empty)
    order <- keyOrderOfFile path
    BS.writeFile path (encodeOrdered [order] (setPath keys current))
    setFileMode path 0o600
  pure (either (\(_ :: SomeException) -> False) (const True) r)
  where
    nonNull = \case
      Null -> Nothing
      x -> Just x
    setPath [] _ = v
    setPath (k : ks) cur =
      let o = case cur of
            Object m -> m
            _ -> KM.empty
          inner = case KM.lookup (K.fromText k) o of
            Just x@(Object _) -> x
            _ -> Object KM.empty
       in Object (KM.insert (K.fromText k) (if null ks then v else setPath ks inner) o)
