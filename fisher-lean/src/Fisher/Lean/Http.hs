-- | HTTP as the search tools see it: one request, and either a response or
-- the exception Python's @urllib.request.urlopen@ would raise, carried as
-- the text @str(exc)@ gives (that text is what reaches a tool error).
module Fisher.Lean.Http
  ( HttpRequest (..)
  , HttpOutcome (..)
  , HttpClient
  , outcomeError
  , pyQuote
  , orjsonError
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)

data HttpRequest = HttpRequest
  { hqMethod :: Text
  , hqUrl :: Text
  , hqHeaders :: [(Text, Text)]
  -- ^ as the tool set them, in order
  , hqBody :: Maybe BS.ByteString
  , hqTimeout :: Double
  }
  deriving (Eq, Show)

data HttpOutcome
  = HttpResponse Int Text BS.ByteString
  -- ^ status, reason, body
  | HttpError Int Text
  -- ^ urllib's HTTPError: status, reason
  | HttpFailure Text
  -- ^ any other exception, as @str()@ shows it (@<urlopen error ...>@,
  -- @timed out@)
  deriving (Eq, Show)

type HttpClient = HttpRequest -> IO HttpOutcome

-- | The exception text, for anything but a response.
outcomeError :: HttpOutcome -> Maybe Text
outcomeError = \case
  HttpResponse {} -> Nothing
  HttpError code reason -> Just ("HTTP Error " <> T.pack (show code) <> ": " <> reason)
  HttpFailure t -> Just t

-- | @urllib.parse.quote(s)@ (safe @/@): UTF-8, with letters, digits,
-- @_.-~@ and @/@ kept.
pyQuote :: Text -> Text
pyQuote = T.concatMap enc
  where
    enc c
      | ord c < 128 && (c `elem` ("/_.-~" :: String) || isAlnumAscii c) = T.singleton c
      | otherwise = T.concat [pct w | w <- BS.unpack (TE.encodeUtf8 (T.singleton c))]
    pct w = "%" <> T.toUpper (T.justifyRight 2 '0' (T.pack (showHex w "")))
    isAlnumAscii c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | orjson's message for a document it cannot parse, for the common cases
-- (an empty body, a word that is not a JSON literal, any other first
-- character). Deeper errors get the first-character form, which is
-- correct only when the document goes wrong at its start.
orjsonError :: BS.ByteString -> Text
orjsonError b =
  let lead = BC.length (BC.takeWhile isSpace b)
      at = " line 1 column " <> T.pack (show (lead + 1)) <> " (char " <> T.pack (show lead) <> ")"
   in case BC.uncons (BC.drop lead b) of
        Nothing -> "Input is a zero-length, empty document: line 1 column 1 (char 0)"
        Just ('n', _) -> "invalid literal, expected 'null':" <> at
        Just ('t', _) -> "invalid literal, expected 'true':" <> at
        Just ('f', _) -> "invalid literal, expected 'false':" <> at
        Just _ -> "unexpected character:" <> at
