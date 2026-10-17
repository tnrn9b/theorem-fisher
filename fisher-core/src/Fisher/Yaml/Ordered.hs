-- | YAML written in a given key order. Python's @yaml.dump(sort_keys=False)@
-- writes a dict in insertion order; an aeson object has none, so the order
-- is read from YAML that has it (upstream's defaults, the file being
-- rewritten) and laid back on when writing.
module Fisher.Yaml.Ordered
  ( KeyOrder (..)
  , keyOrderOf
  , keyOrderOfFile
  , encodeOrdered
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit (runConduitRes, (.|))
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Yaml.Builder as B
import Data.Yaml.Parser (RawDoc (..), YamlValue (..), sinkRawDoc)
import System.Directory (doesFileExist)
import qualified Text.Libyaml as L

-- | The keys of each mapping, in document order, with their own orders.
newtype KeyOrder = KeyOrder [(Text, KeyOrder)]
  deriving (Eq, Show)

fromYaml :: YamlValue -> KeyOrder
fromYaml = \case
  Mapping kvs _ -> KeyOrder [(k, fromYaml v) | (k, v) <- kvs]
  _ -> KeyOrder []

-- | The key order of a YAML document; empty when it does not parse.
keyOrderOf :: ByteString -> IO KeyOrder
keyOrderOf bs = do
  r <- try (runConduitRes (L.decode bs .| sinkRawDoc)) :: IO (Either SomeException RawDoc)
  pure (either (const (KeyOrder [])) (\(RawDoc v _) -> fromYaml v) r)

keyOrderOfFile :: FilePath -> IO KeyOrder
keyOrderOfFile path = do
  exists <- doesFileExist path
  if exists then BS.readFile path >>= keyOrderOf else pure (KeyOrder [])

-- | Block YAML with each mapping's keys in the first order that names them,
-- then the next, then any left over sorted (where Python would have
-- appended them).
encodeOrdered :: [KeyOrder] -> Value -> ByteString
encodeOrdered orders0 = B.toByteString . go orders0
  where
    go os = \case
      Object o ->
        let present = Map.fromList [(K.toText k, v) | (k, v) <- KM.toList o]
            named = concat [map fst ks | KeyOrder ks <- os]
            keys = dedupe (filter (`Map.member` present) named <> sort (Map.keys present))
            child k = [c | KeyOrder ks <- os, Just c <- [lookup k ks]]
         in B.mapping [(k, go (child k) v) | k <- keys, Just v <- [Map.lookup k present]]
      Array a -> B.array (map (go []) (V.toList a))
      String s -> B.string s
      Number n -> B.scientific n
      Bool b -> B.bool b
      Null -> B.null
    dedupe = goD Set.empty
      where
        goD _ [] = []
        goD seen (x : xs)
          | Set.member x seen = goD seen xs
          | otherwise = x : goD (Set.insert x seen) xs
