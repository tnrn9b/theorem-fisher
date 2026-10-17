-- | Reading the reference outputs recorded from the pinned OpenGauss by
-- @tools/corpus/a1.py@, and the @<TMP>@ substitution they were written with.
module Fisher.Golden
  ( goldenCases
  , field
  , textField
  , untmp
  , retmp
  ) where

import Data.Aeson (FromJSON, Value (..), eitherDecodeFileStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

-- | The @cases@ array of a golden file.
goldenCases :: FilePath -> IO [Value]
goldenCases path = do
  decoded <- eitherDecodeFileStrict path
  case decoded of
    Left e -> fail (path <> ": " <> e)
    Right (Object o) | Just (Array cs) <- KM.lookup "cases" o -> pure (V.toList cs)
    Right (Object o) | Just c@(Object _) <- KM.lookup "cases" o -> pure [c]
    Right _ -> fail (path <> ": no cases")

field :: FromJSON a => Text -> Value -> a
field k (Object o) = case KM.lookup (K.fromText k) o of
  Just v -> either (error . ((T.unpack k <> ": ") <>)) id (parseEither parseJSON v)
  Nothing -> either (error . ((T.unpack k <> ": ") <>)) id (parseEither parseJSON Null)
field k _ = error ("not an object, looking for " <> T.unpack k)

textField :: Text -> Value -> Text
textField = field

-- | A scratch path back to @<TMP>@, throughout a value.
untmp :: FilePath -> Value -> Value
untmp tmp = \case
  String s -> String (T.replace (T.pack tmp) "<TMP>" s)
  Array a -> Array (fmap (untmp tmp) a)
  Object o -> Object (fmap (untmp tmp) o)
  v -> v

-- | @<TMP>@ to a scratch path, in text.
retmp :: FilePath -> Text -> Text
retmp tmp = T.replace "<TMP>" (T.pack tmp)
