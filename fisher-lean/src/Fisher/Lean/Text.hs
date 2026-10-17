-- | The Python string and path semantics leanclient and lean-lsp-mcp rely
-- on: @str.splitlines()@ (more line breaks than LSP's), universal-newline
-- file reads, @str.isascii()@, and @pathlib.Path.as_uri()@ / its inverse.
module Fisher.Lean.Text
  ( splitLines
  , splitLinesKeep
  , readFileContents
  , pyReEscape
  , isAscii
  , universalNewlines
  , readTextUtf8
  , readTextLenient
  , utf8Error
  , pathAsUri
  , uriToPath
  , percentDecode
  ) where

import Control.Exception (throwIO)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isHexDigit, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Numeric (showHex)

import Fisher.Lean.Error (LeanError (..))

-- | @str.splitlines()@: breaks on @\\n@, @\\r@, @\\r\\n@, @\\v@, @\\f@,
-- @\\x1c@ to @\\x1e@, @\\x85@, U+2028 and U+2029; one trailing break is
-- dropped and @""@ gives @[]@.
splitLines :: Text -> [Text]
splitLines t
  | T.null t = []
  | otherwise = go t
  where
    go s
      | T.null s = []
      | otherwise =
          let (line, rest) = T.break isBreak s
           in case T.uncons rest of
                Nothing -> [line]
                Just ('\r', r) | Just ('\n', r') <- T.uncons r -> line : go r'
                Just (_, r) -> line : go r
    isBreak c = c `elem` ("\n\r\v\f\x1c\x1d\x1e\x85\x2028\x2029" :: String)

-- | @str.splitlines(keepends=True)@.
splitLinesKeep :: Text -> [Text]
splitLinesKeep t
  | T.null t = []
  | otherwise = go t
  where
    go s
      | T.null s = []
      | otherwise =
          let (line, rest) = T.break isBreak s
           in case T.uncons rest of
                Nothing -> [line]
                Just ('\r', r) | Just ('\n', r') <- T.uncons r -> (line <> "\r\n") : go r'
                Just (c, r) -> (line <> T.singleton c) : go r
    isBreak c = c `elem` ("\n\r\v\f\x1c\x1d\x1e\x85\x2028\x2029" :: String)

-- | lean-lsp-mcp's @get_file_contents@: UTF-8, else Latin-1, universal
-- newlines.
readFileContents :: FilePath -> IO Text
readFileContents p = do
  b <- BS.readFile p
  pure . universalNewlines $ case TE.decodeUtf8' b of
    Right t -> t
    Left _ -> TE.decodeLatin1 b

-- | Python 3.12's @re.escape@.
pyReEscape :: Text -> Text
pyReEscape = T.concatMap (\c -> if c `elem` ("()[]{}?*+-|^$\\.&~# \t\n\r\v\f" :: String) then T.pack ['\\', c] else T.singleton c)

isAscii :: Text -> Bool
isAscii = T.all (\c -> ord c < 128)

-- | Text-mode reads: @\\r\\n@ and a lone @\\r@ become @\\n@.
universalNewlines :: Text -> Text
universalNewlines = T.replace "\r" "\n" . T.replace "\r\n" "\n"

-- | @Path.read_text(encoding="utf-8")@: strict decoding (a bad byte raises
-- Python's UnicodeDecodeError text), universal newlines.
readTextUtf8 :: FilePath -> IO Text
readTextUtf8 p = do
  b <- BS.readFile p
  case TE.decodeUtf8' b of
    Right t -> pure (universalNewlines t)
    Left _ -> throwIO (ClientError (utf8Error b))

-- | @'utf-8' codec can't decode byte 0xNN in position N: <reason>@, as
-- CPython reports the first invalid sequence.
utf8Error :: BS.ByteString -> Text
utf8Error b = go 0
  where
    n = BS.length b
    at i = BS.index b i
    cont i = i < n && at i .&. 0xC0 == 0x80
    msg i reason =
      "'utf-8' codec can't decode byte 0x" <> T.justifyRight 2 '0' (T.pack (showHex (at i) "")) <> " in position "
        <> T.pack (show i) <> ": " <> reason
    go i
      | i >= n = "'utf-8' codec can't decode"
      | c < 0x80 = go (i + 1)
      | c >= 0xC2 && c <= 0xDF = seqOf 1 (const True)
      | c == 0xE0 = seqOf 2 (>= 0xA0)
      | (c >= 0xE1 && c <= 0xEC) || c == 0xEE || c == 0xEF = seqOf 2 (const True)
      | c == 0xED = seqOf 2 (< 0xA0)
      | c == 0xF0 = seqOf 3 (>= 0x90)
      | c >= 0xF1 && c <= 0xF3 = seqOf 3 (const True)
      | c == 0xF4 = seqOf 3 (< 0x90)
      | otherwise = msg i "invalid start byte"
      where
        c = at i
        seqOf k firstOk
          | i + 1 >= n = msg i "unexpected end of data"
          | not (cont (i + 1)) || not (firstOk (at (i + 1))) = msg i "invalid continuation byte"
          | otherwise = rest (2 :: Int)
          where
            rest j
              | j > k = go (i + k + 1)
              | i + j >= n = msg i "unexpected end of data"
              | not (cont (i + j)) = msg i "invalid continuation byte"
              | otherwise = rest (j + 1)

-- | @read_text(encoding="utf-8", errors="replace")@.
readTextLenient :: FilePath -> IO Text
readTextLenient p = universalNewlines . TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile p

-- | @Path(p).as_uri()@ for an absolute POSIX path: @file://@ and the path
-- percent-encoded as @urllib.parse.quote_from_bytes(..., safe="/")@
-- (unreserved: letters, digits, @_.-~@ and @/@).
pathAsUri :: FilePath -> Text
pathAsUri p = "file://" <> T.concatMap enc (T.pack p)
  where
    enc c
      | ord c < 128 && (c `elem` ("/_.-~" :: String) || isAlnumAscii c) = T.singleton c
      | otherwise = T.concat [pct w | w <- BS.unpack (TE.encodeUtf8 (T.singleton c))]
    pct w = "%" <> T.toUpper (T.justifyRight 2 '0' (T.pack (showHex w "")))
    isAlnumAscii c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | @url2pathname(unquote(urlparse(uri).path))@ for a @file://@ URI.
uriToPath :: Text -> FilePath
uriToPath u =
  let afterScheme = case T.breakOn "://" u of
        (_, r) | not (T.null r) -> T.drop 3 r
        _ -> u
      -- urlparse: the path starts at the first "/" after the authority
      path = snd (T.breakOn "/" afterScheme)
      path' = fst (T.breakOn "?" (fst (T.breakOn "#" path)))
   in T.unpack (percentDecode path')

-- | @urllib.parse.unquote@: percent escapes decoded as UTF-8 (bad bytes
-- replaced).
percentDecode :: Text -> Text
percentDecode t = TE.decodeUtf8With TEE.lenientDecode (BS.pack (go (T.unpack t)))
  where
    go = \case
      [] -> []
      ('%' : a : b : r) | isHexDigit a && isHexDigit b -> fromIntegral (digitToInt a * 16 + digitToInt b) : go r
      (c : r) -> utf8 c <> go r
    utf8 c = BS.unpack (TE.encodeUtf8 (T.singleton c))
