module Fisher.Tools.TestUtil
  ( loadDoc
  , cases
  , txt
  , field
  , strList
  , intOf
  , boolOf
  , listOf
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)

import Fisher.Py.Value

loadDoc :: FilePath -> IO PyVal
loadDoc p = either fail pure . loads =<< BS.readFile p

cases :: PyVal -> [PyVal]
cases d = listOf (field "cases" d)

field :: Text -> PyVal -> PyVal
field k v = maybe PNull id (get k v)

txt :: Text -> PyVal -> Text
txt k v = case get k v of
  Just (PStr s) -> s
  _ -> ""

strList :: PyVal -> [Text]
strList v = [s | PStr s <- listOf v]

listOf :: PyVal -> [PyVal]
listOf = \case
  PList xs -> xs
  _ -> []

intOf :: PyVal -> Integer
intOf = \case
  PInt n -> n
  _ -> 0

boolOf :: PyVal -> Bool
boolOf = \case
  PBool b -> b
  _ -> False
