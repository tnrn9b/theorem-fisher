-- | JSON files the way upstream reads and writes them: a missing,
-- unreadable or non-object file reads as an empty object, and a write is
-- the object indented by two, with a final newline.
module Fisher.Workflow.Json
  ( loadJsonObject
  , writeJsonObject
  , jsonPretty
  , objectOf
  , getText
  , pyText
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

import Fisher.Py (pyStr, pyTruthy)

loadJsonObject :: FilePath -> IO (KM.KeyMap Value)
loadJsonObject path = do
  exists <- doesFileExist path
  if not exists
    then pure KM.empty
    else do
      r <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case r of
        Right bs | Just (Object o) <- decodeStrict bs -> o
        _ -> KM.empty

jsonPretty :: Value -> BL.ByteString
jsonPretty = P.encodePretty' P.defConfig { P.confIndent = P.Spaces 2 }

writeJsonObject :: FilePath -> KM.KeyMap Value -> IO ()
writeJsonObject path o = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (jsonPretty (Object o) <> "\n")

objectOf :: Maybe Value -> Maybe (KM.KeyMap Value)
objectOf = \case
  Just (Object o) -> Just o
  _ -> Nothing

-- | @str(d.get(k, "") or "").strip()@.
getText :: Text -> KM.KeyMap Value -> Text
getText k o = case KM.lookup (K.fromText k) o of
  Just v | pyTruthy v -> T.strip (pyStr v)
  _ -> ""

-- | @str(d.get(k, "")).strip()@: no @or@, so a null is the text "None".
pyText :: Text -> KM.KeyMap Value -> Text
pyText k o = maybe "" (T.strip . pyStr) (KM.lookup (K.fromText k) o)
