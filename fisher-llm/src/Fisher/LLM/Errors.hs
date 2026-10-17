-- | The errors the OpenAI and Anthropic SDKs raise, spelled as their
-- @str()@ (upstream's retry logic matches on that text), and their retry
-- rules: which responses are retried and how long to wait.
module Fisher.LLM.Errors
  ( LLMError (..)
  , errorText
  , errorClass
  , statusError
  , shouldRetry
  , retryAfter
  , retryDelay
  , maxRetryAfter
  , sdkMaxRetries
  ) where

import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Text.Read (readMaybe)

import Fisher.Py.Value

data LLMError
  = StatusError Int Text PyVal
  -- ^ a non-2xx response: status, the SDK's message, the decoded body
  | ConnectionError
  | TimeoutError
  | ApiError Text
  -- ^ an error inside a stream, or a malformed response
  | LocalError Text Text
  -- ^ an exception upstream raises itself: its class and message
  deriving (Eq, Show)

-- | @str(e)@.
errorText :: LLMError -> Text
errorText = \case
  StatusError _ m _ -> m
  ConnectionError -> "Connection error."
  TimeoutError -> "Request timed out."
  ApiError m -> m
  LocalError _ m -> m

-- | The SDK exception class.
errorClass :: LLMError -> Text
errorClass = \case
  StatusError s _ _ -> case s of
    400 -> "BadRequestError"
    401 -> "AuthenticationError"
    403 -> "PermissionDeniedError"
    404 -> "NotFoundError"
    409 -> "ConflictError"
    422 -> "UnprocessableEntityError"
    429 -> "RateLimitError"
    _ | s >= 500 -> "InternalServerError"
      | otherwise -> "APIStatusError"
  ConnectionError -> "APIConnectionError"
  TimeoutError -> "APITimeoutError"
  ApiError _ -> "APIError"
  LocalError c _ -> c

-- | @_make_status_error_from_response@: JSON bodies are shown as their
-- Python repr after @"Error code: N - "@; other text as itself.
statusError :: Int -> BS.ByteString -> LLMError
statusError status raw =
  let text = T.strip (TE.decodeUtf8With TEE.lenientDecode raw)
   in case loads (TE.encodeUtf8 text) of
        Right body -> StatusError status ("Error code: " <> tshow status <> " - " <> pyStrOf body) body
        Left _ -> StatusError status (if T.null text then "Error code: " <> tshow status else text) (str text)
  where
    tshow = T.pack . show

maxRetryAfter :: Double
maxRetryAfter = 120

sdkMaxRetries :: Int
sdkMaxRetries = 2

lookupH :: Text -> [(Text, Text)] -> Maybe Text
lookupH k hs = listToMaybe [v | (k', v) <- hs, T.toLower k' == k]

-- | @_parse_retry_after_header@: milliseconds, then seconds, then an HTTP
-- date relative to now.
retryAfter :: Double -> [(Text, Text)] -> Maybe Double
retryAfter now hs =
  case [d / divisor | (h, divisor) <- [("retry-after-ms", 1000), ("retry-after", 1)], Just v <- [lookupH h hs], Just d <- [pyFloat v]] of
    (d : _) -> Just d
    [] -> do
      v <- lookupH "retry-after" hs
      t <- parseDate (T.unpack (T.strip v))
      pure (realToFrac (utcTimeToPOSIXSeconds t) - now)
  where
    parseDate s =
      listToMaybe
        [ t | fmt <- ["%a, %d %b %Y %H:%M:%S GMT", "%a, %d %b %Y %H:%M:%S %Z", "%d %b %Y %H:%M:%S GMT", "%A, %d-%b-%y %H:%M:%S GMT", "%a %b %e %H:%M:%S %Y"]
            , Just t <- [parseTimeM True defaultTimeLocale fmt s :: Maybe UTCTime] ]

-- | Python's @float(s)@ for the forms headers use; an overflowing number
-- is the SDK's "too long" sentinel.
pyFloat :: Text -> Maybe Double
pyFloat raw =
  let s = map toLower (T.unpack (T.strip raw))
      unsigned = case s of
        ('+' : r) -> r
        r -> r
   in case unsigned of
        _ | unsigned `elem` ["inf", "infinity"] -> Just (if take 1 s == "-" then -1 / 0 else 1 / 0)
        _ | unsigned `elem` ["nan"] -> Just (0 / 0)
        _ -> case readMaybe (fixup unsigned) :: Maybe Double of
          Just d | isInfinite d -> Just (maxRetryAfter + 1)
          Just d -> Just d
          Nothing -> Nothing
  where
    fixup x = case x of
      ('.' : _) -> '0' : x
      ('-' : '.' : r) -> "-0." <> r
      _ | not (null x) && last x == '.' -> x <> "0"
      _ -> x

-- | @_should_retry@.
shouldRetry :: Double -> Int -> [(Text, Text)] -> Bool
shouldRetry now status hs
  | Just ra <- retryAfter now hs, not (isInfinite ra || isNaN ra), ra > maxRetryAfter = False
  | lookupH "x-should-retry" hs == Just "true" = True
  | lookupH "x-should-retry" hs == Just "false" = False
  | status `elem` [408, 409, 429] = True
  | status >= 500 = True
  | otherwise = False

-- | @_calculate_retry_timeout@ for the n-th retry (0-based), given
-- @random()@ in [0, 1).
retryDelay :: Double -> Int -> [(Text, Text)] -> Double -> Double
retryDelay now n hs rnd = case retryAfter now hs of
  Just ra | not (isInfinite ra || isNaN ra), ra > 0, ra <= maxRetryAfter -> ra
  _ ->
    let sleep = min (0.5 * 2 ^^ min n 1000) 8
        t = sleep * (1 - 0.25 * rnd)
     in if t >= 0 then t else 0
