{-# LANGUAGE TemplateHaskell #-}
-- | Upstream's catalogue of environment variables (@REQUIRED_ENV_VARS@,
-- @OPTIONAL_ENV_VARS@, @ENV_VARS_BY_VERSION@), generated into
-- @data/env-vars.json@ by @tools/gen-defaults.py@, in upstream's order.
module Fisher.EnvVars
  ( EnvVar (..)
  , requiredEnvVars
  , optionalEnvVars
  , envVarsByVersion
  , getEnvValueAt
  ) where

import Data.Aeson (FromJSON (..), Value, eitherDecodeStrict, withObject, (.:), (.:?), (.!=))
import qualified Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import Fisher.Env (loadEnvAt)

data EnvVar = EnvVar
  { evName :: Text
  , evDescription :: Text
  , evPrompt :: Text
  , evUrl :: Maybe Text
  , evPassword :: Bool
  , evCategory :: Text
  , evAdvanced :: Bool
  , evTools :: [Text]
  }
  deriving (Eq, Show)

data Catalogue = Catalogue [(Text, Value)] [(Text, Value)] (Map.Map Text [Text])

instance FromJSON Catalogue where
  parseJSON = withObject "env-vars" $ \o ->
    Catalogue <$> o .: "required" <*> o .: "optional" <*> o .: "by_version"

envVarsJson :: ByteString
envVarsJson = $(makeRelativeToProject "data/env-vars.json" >>= embedFile)

catalogue :: Catalogue
catalogue = either (error . ("data/env-vars.json: " <>)) id (eitherDecodeStrict envVarsJson)

toVar :: (Text, Value) -> EnvVar
toVar (name, v) = case Data.Aeson.Types.parseMaybe parser v of
  Just ev -> ev
  Nothing -> error ("data/env-vars.json: bad entry " <> T.unpack name)
  where
    parser = withObject "var" $ \o ->
      EnvVar name
        <$> o .:? "description" .!= ""
        <*> o .:? "prompt" .!= name
        <*> o .:? "url"
        <*> o .:? "password" .!= False
        <*> o .:? "category" .!= ""
        <*> o .:? "advanced" .!= False
        <*> o .:? "tools" .!= []

requiredEnvVars, optionalEnvVars :: [EnvVar]
requiredEnvVars = let Catalogue r _ _ = catalogue in map toVar r
optionalEnvVars = let Catalogue _ o _ = catalogue in map toVar o

-- | The variables introduced at each config version.
envVarsByVersion :: Map.Map Int [Text]
envVarsByVersion =
  let Catalogue _ _ m = catalogue
   in Map.fromList [(read (T.unpack k), vs) | (k, vs) <- Map.toList m]

-- | @get_env_value@: the process environment first (an empty value counts),
-- then the home's @.env@.
getEnvValueAt :: FilePath -> Text -> IO (Maybe Text)
getEnvValueAt home key =
  lookupEnv (T.unpack key) >>= \case
    Just v -> pure (Just (T.pack v))
    Nothing -> Map.lookup key <$> loadEnvAt home
