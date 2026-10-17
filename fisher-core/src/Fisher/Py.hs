-- | The handful of Python coercions OpenGauss applies to YAML values, so the
-- ports below can say @str(x or "").strip()@ once instead of re-deriving it
-- at every field.
module Fisher.Py
  ( pyTruthy
  , pyStr
  , pyRepr
  , pyStrOr
  , pyIter
  , lookupKey
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

-- | Python's truth value of a decoded YAML value.
pyTruthy :: Value -> Bool
pyTruthy = \case
  Null -> False
  Bool b -> b
  Number n -> n /= 0
  String s -> not (T.null s)
  Array a -> not (V.null a)
  Object o -> not (KM.null o)

-- | @str(x)@. Containers fall back to their JSON text, which is close to,
-- but not the same as, Python's repr; no manifest field OpenGauss reads is a
-- container.
pyStr :: Value -> Text
pyStr = \case
  Null -> "None"
  Bool True -> "True"
  Bool False -> "False"
  String s -> s
  Number n -> case floatingOrInteger n :: Either Double Integer of
    Right i -> T.pack (show i)
    Left d -> T.pack (show d)
  v -> TL.toStrict (encodeToLazyText v)

-- | @repr(x)@ as it appears in OpenGauss error messages. 'Nothing' is a key
-- that was absent, which Python's @dict.get@ reports as @None@.
pyRepr :: Maybe Value -> Text
pyRepr = \case
  Nothing -> "None"
  Just (String s) -> "'" <> s <> "'"
  Just v -> pyStr v

-- | @str(x or "").strip()@, the coercion every text field goes through.
pyStrOr :: Maybe Value -> Text
pyStrOr = \case
  Just v | pyTruthy v -> T.strip (pyStr v)
  _ -> ""

-- | What @for x in (v or [])@ iterates over: a list's items, a string's
-- characters, a mapping's keys.
pyIter :: Value -> [Value]
pyIter v | not (pyTruthy v) = []
pyIter (Array a) = V.toList a
pyIter (String s) = map (String . T.singleton) (T.unpack s)
pyIter (Object o) = map (String . K.toText) (KM.keys o)
pyIter _ = []

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = KM.lookup (K.fromText k) o
lookupKey _ _ = Nothing
