-- | Reading @tools/corpus/a3.py@'s recordings, and the path masking it
-- wrote them with.
module Fisher.A3Golden
  ( a3Cases
  , a3Doc
  , obj
  , str
  , mstr
  , arr
  , mask
  , padded
  ) where

import Data.Aeson (Value (..), eitherDecodeFileStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (canonicalizePath, createDirectory)
import System.FilePath ((</>))

a3Doc :: FilePath -> IO Value
a3Doc name = eitherDecodeFileStrict ("test/golden/a3/" <> name) >>= either fail pure

a3Cases :: FilePath -> IO [Value]
a3Cases name =
  a3Doc name >>= \case
    Object o | Just (Array cs) <- KM.lookup "cases" o -> pure (V.toList cs)
    _ -> fail (name <> ": no cases")

obj :: Text -> Value -> Value
obj k (Object o) = maybe Null id (KM.lookup (K.fromText k) o)
obj _ _ = Null

str :: Text -> Value -> Text
str k v = case obj k v of
  String s -> s
  _ -> ""

mstr :: Text -> Value -> Maybe Text
mstr k v = case obj k v of
  String s -> Just s
  _ -> Nothing

arr :: Text -> Value -> [Value]
arr k v = case obj k v of
  Array a -> V.toList a
  _ -> []

-- | The harness's @mask@: the root becomes the tag, and a prefix of it (ten
-- characters or more) cut short by an ellipsis becomes @<TAG:k>@. (The
-- harness takes any such prefix; its two roots never shared ten
-- characters, the tests' can, so here the ellipsis must follow.)
mask :: Text -> Text -> Text -> Text
mask root tag = go
  where
    n = T.length root
    least = T.take 10 root
    go t
      | T.null t = t
      | least `T.isPrefixOf` t
      , let k = 10 + length (takeWhile id (zipWith (==) (T.unpack (T.drop 10 t)) (T.unpack (T.drop 10 root))))
      , k == n || any (`T.isPrefixOf` T.drop k t) ["…", "..."] =
          (if k == n then tag else T.init tag <> ":" <> T.pack (show k) <> ">") <> go (T.drop k t)
      | otherwise = T.cons (T.head t) (go (T.tail t))

-- | A directory under @base@ whose canonical path is exactly @len@ long, as
-- the recordings' scratch roots were.
padded :: FilePath -> Int -> IO FilePath
padded base len = do
  b <- canonicalizePath base
  let k = len - length b - 1
  if k < 1 then fail ("scratch base " <> b <> " is too long for " <> show len) else pure ()
  let p = b </> replicate k 'p'
  createDirectory p
  pure p
