-- | Python's view of JSON: dicts keep insertion order, ints and floats are
-- different things, and @json.loads@, @json.dumps@ and @repr@ have their
-- exact Python spellings. The LLM layer passes messages around in this
-- form, so what it stores and prints matches upstream byte for byte.
module Fisher.Py.Value
  ( PyVal (..)
    -- * Reading and writing
  , loads
  , loadsLazy
  , dumpsAscii
  , dumpsIndent
  , dumpsUnicode
  , pyRepr
  , pyStrOf
  , reprStr
    -- * Dicts
  , get
  , getPath
  , set
  , delete
  , has
  , keys
  , obj
  , str
  , truthy
  , asText
  , asList
  , asDict
    -- * Aeson
  , toAeson
  , fromAeson
  ) where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, generalCategory, GeneralCategory (..), isHexDigit, digitToInt, ord)
import Data.List (foldl')
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Vector as V
import Numeric (showHex)

import Fisher.Py.Json (PyVal (..), dumps, pyFloatRepr)

-- ------------------------------------------------------------------ loads

-- | @json.loads@: numbers with a fraction or exponent are floats, the rest
-- ints; a repeated key keeps its first position and its last value;
-- @NaN@, @Infinity@ and @-Infinity@ are accepted.
loads :: BS.ByteString -> Either String PyVal
loads = P.parseOnly (P.skipSpace *> value <* P.skipSpace <* P.endOfInput)

loadsLazy :: BL.ByteString -> Either String PyVal
loadsLazy = loads . BL.toStrict

value :: P.Parser PyVal
value =
  P.peekChar' >>= \case
    '{' -> P.char '{' *> P.skipSpace *> object
    '[' -> P.char '[' *> P.skipSpace *> array
    '"' -> PStr <$> stringLit
    't' -> PBool True <$ P.string "true"
    'f' -> PBool False <$ P.string "false"
    'n' -> PNull <$ P.string "null"
    'N' -> PFloat (0 / 0) <$ P.string "NaN"
    'I' -> PFloat (1 / 0) <$ P.string "Infinity"
    _ -> (PFloat (-1 / 0) <$ P.string "-Infinity") <|> number

object :: P.Parser PyVal
object = (P.char '}' *> pure (PDict [])) <|> (PDict . dedupe <$> go)
  where
    go = do
      k <- stringLit
      P.skipSpace *> P.char ':' *> P.skipSpace
      v <- value
      P.skipSpace
      (P.char ',' *> P.skipSpace *> (((k, v) :) <$> go)) <|> (P.char '}' *> pure [(k, v)])
    dedupe = foldl' add []
    add acc (k, v) = case lookup k acc of
      Just _ -> [(k', if k' == k then v else v') | (k', v') <- acc]
      Nothing -> acc <> [(k, v)]

array :: P.Parser PyVal
array = (P.char ']' *> pure (PList [])) <|> (PList <$> go)
  where
    go = do
      v <- value
      P.skipSpace
      (P.char ',' *> P.skipSpace *> ((v :) <$> go)) <|> (P.char ']' *> pure [v])

number :: P.Parser PyVal
number = do
  (raw, _) <- P.match $ do
    _ <- P.option '+' (P.char '-')
    _ <- P.takeWhile1 P.isDigit
    _ <- P.option "" (P.string "." *> P.takeWhile1 P.isDigit)
    P.option "" (P.satisfy (`elem` ("eE" :: String)) *> P.option '+' (P.satisfy (`elem` ("+-" :: String))) *> P.takeWhile1 P.isDigit)
  let s = TE.decodeUtf8 raw
  if T.any (`elem` (".eE" :: String)) s
    then case reads (T.unpack s) :: [(Double, String)] of
      [(d, "")] -> pure (PFloat d)
      _ -> fail "bad float"
    else pure (PInt (read (T.unpack s)))

stringLit :: P.Parser Text
stringLit = do
  _ <- P.char '"'
  chunks <- go []
  pure (T.concat (reverse chunks))
  where
    go acc = do
      plain <- P.takeWhile (\c -> c /= '"' && c /= '\\')
      let acc' = if BS.null plain then acc else TE.decodeUtf8With TEE.lenientDecode plain : acc
      P.anyChar >>= \case
        '"' -> pure acc'
        _ -> do
          t <- escape
          go (t : acc')
    escape =
      P.anyChar >>= \case
        '"' -> pure "\""
        '\\' -> pure "\\"
        '/' -> pure "/"
        'b' -> pure "\b"
        'f' -> pure "\f"
        'n' -> pure "\n"
        'r' -> pure "\r"
        't' -> pure "\t"
        'u' -> do
          hi <- hex4
          if hi >= 0xD800 && hi < 0xDC00
            then
              ( do
                  _ <- P.string "\\u"
                  lo <- hex4
                  if lo >= 0xDC00 && lo < 0xE000
                    then pure (T.singleton (chr (0x10000 + ((hi - 0xD800) `shiftL` 10) + (lo - 0xDC00))))
                    else pure (T.pack [repl hi, repl lo])
              )
                <|> pure (T.singleton (repl hi))
            else pure (T.singleton (repl hi))
        c -> fail ("bad escape " <> [c])
    hex4 = do
      ds <- P.count 4 (P.satisfy isHexDigit)
      pure (foldl' (\a d -> a * 16 + digitToInt d) 0 ds)
    -- A lone surrogate cannot live in Text; Python would keep it.
    repl n = if n >= 0xD800 && n < 0xE000 then '\xFFFD' else chr n

-- ------------------------------------------------------------------ dumps

-- | @json.dumps(v)@ with Python's defaults: @", "@ and @": "@ separators and
-- @ensure_ascii=True@ (non-ASCII as @\\uXXXX@, astral characters as
-- surrogate pairs).
dumpsAscii :: PyVal -> Text
dumpsAscii = \case
  PStr s -> quoteAscii s
  PList xs -> "[" <> T.intercalate ", " (map dumpsAscii xs) <> "]"
  PDict kvs -> "{" <> T.intercalate ", " [quoteAscii k <> ": " <> dumpsAscii v | (k, v) <- kvs] <> "}"
  v -> dumps v

-- | @json.dumps(v, indent=n)@ (ensure_ascii=True; ensure_ascii=False when
-- the flag says so).
dumpsIndent :: Bool -> Int -> PyVal -> Text
dumpsIndent ascii n = go 0
  where
    q = if ascii then quoteAscii else \s -> dumps (PStr s)
    pad d = T.replicate (d * n) " "
    go d = \case
      PStr s -> q s
      PList [] -> "[]"
      PDict [] -> "{}"
      PList xs -> "[\n" <> T.intercalate ",\n" [pad (d + 1) <> go (d + 1) x | x <- xs] <> "\n" <> pad d <> "]"
      PDict kvs -> "{\n" <> T.intercalate ",\n" [pad (d + 1) <> q k <> ": " <> go (d + 1) v | (k, v) <- kvs] <> "\n" <> pad d <> "}"
      v -> dumps v

-- | @json.dumps(v, ensure_ascii=False)@.
dumpsUnicode :: PyVal -> Text
dumpsUnicode = dumps

quoteAscii :: Text -> Text
quoteAscii s = "\"" <> T.concatMap esc s <> "\""
  where
    esc c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\b' -> "\\b"
      '\f' -> "\\f"
      _
        | ord c < 0x20 || (ord c >= 0x7F && ord c < 0x10000) -> u (ord c)
        | ord c >= 0x10000 ->
            let n = ord c - 0x10000
             in u (0xD800 .|. (n `div` 0x400)) <> u (0xDC00 .|. (n .&. 0x3FF))
        | otherwise -> T.singleton c
    u n = "\\u" <> T.justifyRight 4 '0' (T.pack (showHex n ""))

-- | @repr(v)@ of the decoded value, as Python prints dicts inside error
-- messages: @{'error': {'message': 'x'}, 'ok': True, 'n': None}@.
pyRepr :: PyVal -> Text
pyRepr = \case
  PNull -> "None"
  PBool b -> if b then "True" else "False"
  PInt n -> T.pack (show n)
  PFloat d
    | isNaN d -> "nan"
    | isInfinite d -> if d > 0 then "inf" else "-inf"
    | otherwise -> pyFloatRepr d
  PStr s -> reprStr s
  PList xs -> "[" <> T.intercalate ", " (map pyRepr xs) <> "]"
  PDict kvs -> "{" <> T.intercalate ", " [reprStr k <> ": " <> pyRepr v | (k, v) <- kvs] <> "}"

-- | @str(v)@: a string as itself, anything else as its repr.
pyStrOf :: PyVal -> Text
pyStrOf = \case
  PStr s -> s
  v -> pyRepr v

-- | Python's @repr()@ of a string: single quotes unless the text has a
-- single quote and no double quote.
reprStr :: Text -> Text
reprStr s =
  let q = if T.any (== '\'') s && not (T.any (== '"') s) then '"' else '\''
   in T.singleton q <> T.concatMap (esc q) s <> T.singleton q
  where
    esc q c
      | c == q = T.pack ['\\', c]
      | c == '\\' = "\\\\"
      | c == '\n' = "\\n"
      | c == '\r' = "\\r"
      | c == '\t' = "\\t"
      | printable c = T.singleton c
      | ord c < 0x100 = "\\x" <> T.justifyRight 2 '0' (T.pack (showHex (ord c) ""))
      | ord c < 0x10000 = "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (ord c) ""))
      | otherwise = "\\U" <> T.justifyRight 8 '0' (T.pack (showHex (ord c) ""))
    printable c
      | c == ' ' = True
      | otherwise = case generalCategory c of
          Control -> False
          Format -> False
          Surrogate -> False
          PrivateUse -> False
          NotAssigned -> False
          LineSeparator -> False
          ParagraphSeparator -> False
          Space -> False
          _ -> True

-- ------------------------------------------------------------------ dicts

get :: Text -> PyVal -> Maybe PyVal
get k (PDict kvs) = lookup k kvs
get _ _ = Nothing

getPath :: [Text] -> PyVal -> Maybe PyVal
getPath [] v = Just v
getPath (k : ks) v = get k v >>= getPath ks

-- | @d[k] = v@: in place when present, appended when new.
set :: Text -> PyVal -> PyVal -> PyVal
set k v (PDict kvs)
  | any ((== k) . fst) kvs = PDict [(k', if k' == k then v else v') | (k', v') <- kvs]
  | otherwise = PDict (kvs <> [(k, v)])
set k v _ = PDict [(k, v)]

delete :: Text -> PyVal -> PyVal
delete k (PDict kvs) = PDict (filter ((/= k) . fst) kvs)
delete _ v = v

has :: Text -> PyVal -> Bool
has k (PDict kvs) = any ((== k) . fst) kvs
has _ _ = False

keys :: PyVal -> [Text]
keys (PDict kvs) = map fst kvs
keys _ = []

obj :: [(Text, PyVal)] -> PyVal
obj = PDict

str :: Text -> PyVal
str = PStr

-- | Python truthiness.
truthy :: PyVal -> Bool
truthy = \case
  PNull -> False
  PBool b -> b
  PInt n -> n /= 0
  PFloat d -> d /= 0
  PStr s -> not (T.null s)
  PList xs -> not (null xs)
  PDict kvs -> not (null kvs)

asText :: PyVal -> Maybe Text
asText (PStr s) = Just s
asText _ = Nothing

asList :: PyVal -> Maybe [PyVal]
asList (PList xs) = Just xs
asList _ = Nothing

asDict :: PyVal -> Maybe [(Text, PyVal)]
asDict (PDict kvs) = Just kvs
asDict _ = Nothing

-- ------------------------------------------------------------------ aeson

-- | To aeson; dict order is lost (aeson objects are maps).
toAeson :: PyVal -> A.Value
toAeson = \case
  PNull -> A.Null
  PBool b -> A.Bool b
  PInt n -> A.Number (fromInteger n)
  PFloat d -> A.Number (fromFloatDigits d)
  PStr s -> A.String s
  PList xs -> A.Array (V.fromList (map toAeson xs))
  PDict kvs -> A.Object (KM.fromList [(K.fromText k, toAeson v) | (k, v) <- kvs])

-- | From aeson, keys in aeson's order; a whole number is an int.
fromAeson :: A.Value -> PyVal
fromAeson = \case
  A.Null -> PNull
  A.Bool b -> PBool b
  A.Number n -> case floatingOrInteger n :: Either Double Integer of
    Right i -> PInt i
    Left d -> PFloat d
  A.String s -> PStr s
  A.Array a -> PList (map fromAeson (V.toList a))
  A.Object o -> PDict [(K.toText k, fromAeson v) | (k, v) <- KM.toList o]
