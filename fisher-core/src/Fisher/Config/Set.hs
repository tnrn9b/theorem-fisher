-- | @gauss config set KEY VALUE@ (@set_config_value@ in
-- @gauss_cli/config.py@). Credentials go to @.env@; anything else goes to
-- @config.yaml@, written from the user's own file rather than the merged
-- defaults.
module Fisher.Config.Set
  ( SetTarget (..)
  , setTarget
  , Coerced (..)
  , coerceValue
  , setConfigValueAt
  ) where

import Control.Monad (forM_)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (fromFloatDigits)
import qualified Data.Yaml as Y
import Data.Bits ((.&.))
import System.Directory (canonicalizePath, doesFileExist)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)

import Fisher.Atomic (atomicWriteFile, secureFile)
import Fisher.Env (saveEnvValueAt)
import Fisher.Paths (configPathIn, ensureGaussHomeAt, envPathIn)
import Fisher.Py (pyTruthy)
import Fisher.Yaml.Ordered (encodeOrdered, keyOrderOfFile)

data SetTarget = ToEnv Text | ToConfig [Text]
  deriving (Eq, Show)

envKeys :: [Text]
envKeys =
  [ "OPENROUTER_API_KEY", "OPENAI_API_KEY", "ANTHROPIC_API_KEY", "VOICE_TOOLS_OPENAI_KEY"
  , "FIRECRAWL_API_KEY", "FIRECRAWL_API_URL", "BROWSERBASE_API_KEY", "BROWSERBASE_PROJECT_ID"
  , "FAL_KEY", "TELEGRAM_BOT_TOKEN", "DISCORD_BOT_TOKEN"
  , "TERMINAL_SSH_HOST", "TERMINAL_SSH_USER", "TERMINAL_SSH_KEY"
  , "SUDO_PASSWORD", "SLACK_BOT_TOKEN", "SLACK_APP_TOKEN"
  , "GITHUB_TOKEN", "WANDB_API_KEY", "TINKER_API_KEY" ]

-- | Listed credentials, and any @*_API_KEY@, @*_TOKEN@ or @TERMINAL_SSH*@,
-- go to .env under the upper-cased name; anything else is a dotted path.
setTarget :: Text -> SetTarget
setTarget key
  | up `elem` envKeys || "_API_KEY" `T.isSuffixOf` up || "_TOKEN" `T.isSuffixOf` up
      || "TERMINAL_SSH" `T.isPrefixOf` up = ToEnv up
  | otherwise = ToConfig (T.splitOn "." key)
  where
    up = T.toUpper key

-- | What a typed value becomes: true/yes/on and false/no/off a boolean,
-- digits an integer, digits with one dot a float, anything else text.
data Coerced = CBool Bool | CInt Integer | CFloat Double | CText Text
  deriving (Eq, Show)

coerceValue :: Text -> Coerced
coerceValue v
  | low `elem` ["true", "yes", "on"] = CBool True
  | low `elem` ["false", "no", "off"] = CBool False
  | allDigits v = CInt (read (T.unpack v))
  | (a, rest) <- T.breakOn "." v, not (T.null rest), allDigits (a <> T.drop 1 rest) =
      CFloat (read ("0" <> T.unpack a <> "." <> T.unpack (T.drop 1 rest) <> "0"))
  | otherwise = CText v
  where
    low = T.toLower v
    allDigits t = not (T.null t) && T.all isDigit t

toValue :: Coerced -> Value
toValue = \case
  CBool b -> Bool b
  CInt i -> Number (fromInteger i)
  CFloat d -> Number (fromFloatDigits d)
  CText t -> String t

-- | Python's @str()@ of the coerced value: what the confirmation prints and
-- the .env mirror stores (@True@, @42@, @1.0@).
render :: Coerced -> Text
render = \case
  CBool True -> "True"
  CBool False -> "False"
  CInt i -> T.pack (show i)
  CFloat d -> T.pack (show d)
  CText t -> t

configToEnv :: [(Text, Text)]
configToEnv =
  [ ("terminal.backend", "TERMINAL_ENV"), ("terminal.docker_image", "TERMINAL_DOCKER_IMAGE")
  , ("terminal.singularity_image", "TERMINAL_SINGULARITY_IMAGE"), ("terminal.modal_image", "TERMINAL_MODAL_IMAGE")
  , ("terminal.daytona_image", "TERMINAL_DAYTONA_IMAGE")
  , ("terminal.docker_mount_cwd_to_workspace", "TERMINAL_DOCKER_MOUNT_CWD_TO_WORKSPACE")
  , ("terminal.cwd", "TERMINAL_CWD"), ("terminal.timeout", "TERMINAL_TIMEOUT")
  , ("terminal.sandbox_dir", "TERMINAL_SANDBOX_DIR"), ("terminal.persistent_shell", "TERMINAL_PERSISTENT_SHELL") ]

setPath :: [Text] -> Value -> KM.KeyMap Value -> KM.KeyMap Value
setPath [] _ o = o
setPath [k] v o = KM.insert (K.fromText k) v o
setPath (k : ks) v o =
  let inner = case KM.lookup (K.fromText k) o of
        Just (Object i) -> i
        _ -> KM.empty
   in KM.insert (K.fromText k) (Object (setPath ks v inner)) o

-- | Returns the confirmation line, or why nothing was written. Upstream
-- crashes on a config.yaml that is not a mapping; Fisher refuses and leaves
-- the file alone.
setConfigValueAt :: FilePath -> Text -> Text -> IO (Either Text Text)
setConfigValueAt home key value = case setTarget key of
  ToEnv name -> do
    saveEnvValueAt home name value
    pure (Right ("✓ Set " <> key <> " in " <> T.pack (envPathIn home)))
  ToConfig path -> do
    let cfgPath = configPathIn home
    exists <- doesFileExist cfgPath
    user <- if not exists then pure (Right KM.empty) else do
      decoded <- Y.decodeFileEither cfgPath
      pure $ case decoded of
        Left _ -> Right KM.empty
        Right (Object o) -> Right o
        Right v | not (pyTruthy v) -> Right KM.empty
        Right _ -> Left (T.pack cfgPath <> " does not hold a mapping; nothing was changed.")
    case user of
      Left e -> pure (Left e)
      Right o -> do
        let c = coerceValue value
            v = toValue c
        ensureGaussHomeAt home
        order <- keyOrderOfFile cfgPath
        -- Fisher only: a temporary file renamed over it, never a
        -- half-written config.yaml. As upstream's plain write: through a
        -- link to the file it names (the link stays), and the file's mode
        -- kept (a new file gets 0600, as save_config leaves it)
        target <- if exists then canonicalizePath cfgPath else pure cfgPath
        mode <- if exists then Just . (.&. 0o7777) . fileMode <$> getFileStatus target else pure Nothing
        atomicWriteFile ".config_.tmp" target (encodeOrdered [order] (Object (setPath path v o)))
        maybe (secureFile target) (setFileMode target) mode
        forM_ (lookup key configToEnv) $ \envName -> saveEnvValueAt home envName (render c)
        pure (Right ("✓ Set " <> key <> " = " <> render c <> " in " <> T.pack cfgPath))
