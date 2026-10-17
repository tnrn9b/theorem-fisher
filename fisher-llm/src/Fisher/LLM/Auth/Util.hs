-- | The small pieces of Python the credential code leans on: @int()@ and
-- @float()@ coercions, @rstrip("/")@, @datetime.fromisoformat@ and
-- @isoformat@, JWT claims, and an httpx-like client over 'Sys'.
module Fisher.LLM.Auth.Util
  ( -- * Python coercions
    pyInt
  , pyIntText
  , pyFloatText
  , rstripSlash
  , strOr
  , textOf
  , orText
  , tryInterrupt
    -- * Time
  , parseIsoTimestamp
  , isoZ
  , isoNowZ
  , formatLocalTime
    -- * JWT
  , decodeJwtClaims
  , base64UrlDecode
    -- * HTTP
  , Client (..)
  , httpxClient
  , urllibClient
  , send
  , respJson
  , raiseForStatus
  , jsonDecodeError
  ) where

import Control.Exception (AsyncException (UserInterrupt), throwIO, try)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Char (isDigit, ord)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time (TimeZone (..), utcToLocalTime)
import Data.Time.Calendar (fromGregorianValid, toModifiedJulianDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word8)

import Fisher.LLM.Auth.Error
import Fisher.LLM.Sys
import Fisher.Py.Value

-- ------------------------------------------------------------- coercions

-- | @int(v)@: Nothing where Python raises.
pyInt :: PyVal -> Maybe Integer
pyInt = \case
  PInt n -> Just n
  PBool b -> Just (if b then 1 else 0)
  PFloat d | not (isNaN d || isInfinite d) -> Just (truncate d)
  PStr s -> pyIntText s
  _ -> Nothing

-- | @int(s)@ of a string.
pyIntText :: Text -> Maybe Integer
pyIntText s0 =
  let s = T.strip s0
      (sign, digits) = case T.uncons s of
        Just ('-', r) -> (-1, r)
        Just ('+', r) -> (1, r)
        _ -> (1, s)
      clean = T.filter (/= '_') digits
   in if not (T.null clean) && T.all isDigit clean && not ("_" `T.isPrefixOf` digits) && not ("_" `T.isSuffixOf` digits)
        then Just (sign * read (T.unpack clean))
        else Nothing

-- | @float(s)@ of a string.
pyFloatText :: Text -> Maybe Double
pyFloatText s0 =
  let s = T.toLower (T.strip s0)
   in case s of
        _ | s `elem` ["inf", "+inf", "infinity", "+infinity"] -> Just (1 / 0)
          | s `elem` ["-inf", "-infinity"] -> Just (-1 / 0)
          | s `elem` ["nan", "+nan", "-nan"] -> Just (0 / 0)
        _ -> case TR.signed TR.double s of
          Right (d, "") -> Just d
          _ -> Nothing

-- | @s.rstrip("/")@.
rstripSlash :: Text -> Text
rstripSlash = T.dropWhileEnd (== '/')

-- | The string inside, or the given default when the value is not a
-- non-empty string (Python's @x or default@ for string-or-None values).
strOr :: Text -> Maybe PyVal -> Text
strOr d = \case
  Just (PStr s) | not (T.null s) -> s
  _ -> d

-- | A string value, if it is one.
textOf :: Maybe PyVal -> Maybe Text
textOf = \case
  Just (PStr s) -> Just s
  _ -> Nothing

-- | Python's @a or b@ on optional strings: the first non-empty one.
orText :: [Maybe Text] -> Maybe Text
orText = foldr (\x acc -> case x of Just t | not (T.null t) -> Just t; _ -> acc) Nothing

-- | Run an action, turning Ctrl-C (@KeyboardInterrupt@) into Left.
tryInterrupt :: IO a -> IO (Either () a)
tryInterrupt act =
  try act >>= \case
    Right x -> pure (Right x)
    Left UserInterrupt -> pure (Left ())
    Left e -> throwIO e

-- ------------------------------------------------------------------ time

-- | @datetime.fromisoformat@ (after a trailing @Z@ becomes @+00:00@), as
-- epoch seconds, with naive times taken as UTC; Nothing where Python
-- raises. Covers the forms Python 3.11 reads: @YYYY-MM-DD@ or @YYYYMMDD@,
-- then any one separator and @HH[:MM[:SS[.f+]]]@ (or compact), then @Z@ or
-- @±HH[:MM[:SS[.f+]]]@.
parseIsoTimestamp :: PyVal -> Maybe Double
parseIsoTimestamp = \case
  PStr v | not (T.null v) ->
    let t0 = T.strip v
        t = if "Z" `T.isSuffixOf` t0 then T.dropEnd 1 t0 <> "+00:00" else t0
     in if T.null t then Nothing else parseIso t
  _ -> Nothing

parseIso :: Text -> Maybe Double
parseIso t = do
  (day, rest) <- dateP t
  (secs, tz) <-
    if T.null rest
      then Just (0, 0)
      else do
        (_, timePart) <- T.uncons rest
        timeP timePart
  let days = toModifiedJulianDay day - 40587
  pure (fromInteger days * 86400 + secs - tz)
  where
    num :: Text -> Maybe Integer
    num s = if not (T.null s) && T.all isDigit s then Just (read (T.unpack s)) else Nothing
    dateP s
      | T.length s >= 10 && T.index s 4 == '-' && T.index s 7 == '-' = do
          y <- num (T.take 4 s)
          m <- num (T.take 2 (T.drop 5 s))
          d <- num (T.take 2 (T.drop 8 s))
          day <- fromGregorianValid y (fromInteger m) (fromInteger d)
          pure (day, T.drop 10 s)
      | T.length s >= 8 && T.all isDigit (T.take 8 s) = do
          y <- num (T.take 4 s)
          m <- num (T.take 2 (T.drop 4 s))
          d <- num (T.take 2 (T.drop 6 s))
          day <- fromGregorianValid y (fromInteger m) (fromInteger d)
          pure (day, T.drop 8 s)
      | otherwise = Nothing
    timeP s = do
      let (clock, tzs) = T.break (`elem` ("+-" :: String)) s
      secs <- clockP True clock
      tz <-
        if T.null tzs
          then Just 0
          else do
            (sign, off) <- T.uncons tzs
            o <- clockP False off
            pure (if sign == '-' then negate o else o)
      pure (secs, tz)
    clockP allowFrac c = do
      let (hms, frac) = T.break (`elem` (".," :: String)) c
          parts
            | T.any (== ':') hms = T.splitOn ":" hms
            | otherwise = T.chunksOf 2 hms
      if null parts || length parts > 3 || any ((/= 2) . T.length) parts then Nothing else pure ()
      ns <- mapM num parts
      (h, mi, se) <- case ns <> [0, 0] of
        (a : b : c' : _) -> Just (a, b, c')
        _ -> Nothing
      if h > 23 || mi > 59 || se > 59 then Nothing else pure ()
      fr <-
        if T.null frac
          then Just 0
          else do
            let ds = T.drop 1 frac
            if not allowFrac && T.length ds /= 6 then Nothing else pure ()
            if T.null ds || not (T.all isDigit ds) || length parts /= 3 then Nothing else pure ()
            let six = T.take 6 (ds <> "000000")
            pure (fromInteger (read (T.unpack six)) / 1e6)
      pure (fromInteger (h * 3600 + mi * 60 + se) + fr)

-- | @datetime.fromtimestamp(t, utc).isoformat().replace("+00:00", "Z")@.
isoZ :: Double -> Text
isoZ t = T.replace "+00:00" "Z" (isoFromEpoch t)

isoNowZ :: Sys -> IO Text
isoNowZ sys = isoZ <$> sysNow sys

-- | @strftime("%Y-%m-%d %H:%M:%S %Z")@ of an epoch in the given zone.
formatLocalTime :: TimeZone -> Double -> Text
formatLocalTime tz t =
  let whole = floor t :: Integer
      lt = utcToLocalTime tz (posixSecondsToUTCTime (fromInteger whole))
   in T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lt) <> " " <> T.pack (timeZoneName tz)

-- ------------------------------------------------------------------- JWT

-- | @base64.urlsafe_b64decode@ of padded text; Nothing on bad input.
base64UrlDecode :: Text -> Maybe BS.ByteString
base64UrlDecode t0 = do
  let t = T.filter (/= '=') t0
      vals = map val (T.unpack t)
  if any (< 0) vals || T.length t `mod` 4 == 1 then Nothing else pure ()
  pure (BS.pack (go vals))
  where
    val c
      | c >= 'A' && c <= 'Z' = ord c - 65
      | c >= 'a' && c <= 'z' = ord c - 71
      | c >= '0' && c <= '9' = ord c + 4
      | c == '-' || c == '+' = 62
      | c == '_' || c == '/' = 63
      | otherwise = -1
    go :: [Int] -> [Word8]
    go (a : b : c : d : rest) =
      let n = (a `shiftL` 18) .|. (b `shiftL` 12) .|. (c `shiftL` 6) .|. d
       in [fromIntegral (n `shiftR` 16), fromIntegral ((n `shiftR` 8) .&. 255), fromIntegral (n .&. 255)] <> go rest
    go [a, b, c] =
      let n = (a `shiftL` 18) .|. (b `shiftL` 12) .|. (c `shiftL` 6)
       in [fromIntegral (n `shiftR` 16), fromIntegral ((n `shiftR` 8) .&. 255)]
    go [a, b] = let n = (a `shiftL` 18) .|. (b `shiftL` 12) in [fromIntegral (n `shiftR` 16)]
    go _ = []

-- | @_decode_jwt_claims@: the payload of a three-part token as a dict, or
-- an empty dict.
decodeJwtClaims :: Text -> PyVal
decodeJwtClaims tok
  | T.count "." tok /= 2 = PDict []
  | otherwise = fromMaybe (PDict []) $ do
      raw <- base64UrlDecode (T.splitOn "." tok !! 1)
      case loads raw of
        Right d@(PDict _) -> Just d
        _ -> Nothing

-- ------------------------------------------------------------------ HTTP

-- | What an @httpx.Client(...)@ (or a bare urllib call) brings to each
-- request: the stack, default headers and the timeout.
data Client = Client
  { cVia :: Via
  , cHeaders :: [(Text, Text)]
  , cTimeout :: Double
  }

httpxClient :: [(Text, Text)] -> Double -> Client
httpxClient = Client ViaHttpx

urllibClient :: Double -> Client
urllibClient = Client ViaUrllib []

-- | One request. Request headers override the client's (by name, ignoring
-- case); a form or JSON body brings its content type unless one is set,
-- as httpx does. A request that gets no answer raises as upstream's stack
-- would (httpx's @ConnectError@, urllib's @URLError@).
send :: Sys -> Client -> Text -> Text -> [(Text, Text)] -> AuthBody -> IO AuthResp
send sys c method url hs body = do
  let lower = T.toLower . fst
      merged0 = [h | h <- cHeaders c, lower h `notElem` map lower hs] <> hs
      ctype = case body of
        FormBody _ -> Just "application/x-www-form-urlencoded"
        JsonBody _ -> Just "application/json"
        NoBody -> Nothing
      merged = case ctype of
        Just ct | "content-type" `notElem` map lower merged0 -> merged0 <> [("Content-Type", ct)]
        _ -> merged0
  r <- sysHttp sys (AuthReq (cVia c) method url merged body (cTimeout c))
  case r of
    Right resp -> pure resp
    Left (HttpFail cls msg) -> throwIO (PyError cls (case cVia c of ViaHttpx -> "httpx"; ViaUrllib -> "urllib.error") msg)

-- | @json.JSONDecodeError@ as raised by @response.json()@ on a body that is
-- not JSON.
jsonDecodeError :: PyError
jsonDecodeError = PyError "JSONDecodeError" "json.decoder" "Expecting value: line 1 column 1 (char 0)"

-- | @response.json()@.
respJson :: AuthResp -> Either PyError PyVal
respJson r = either (const (Left jsonDecodeError)) Right (authJson r)

-- | httpx's @raise_for_status()@ for a 4xx or 5xx answer.
raiseForStatus :: Text -> AuthResp -> IO ()
raiseForStatus url r
  | s < 400 = pure ()
  | otherwise =
      throwIO . PyError "HTTPStatusError" "httpx" $
        kind <> " '" <> T.pack (show s) <> " " <> reason <> "' for url '" <> url
          <> "'\nFor more information check: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/" <> T.pack (show s)
  where
    s = aStatus r
    kind = if s < 500 then "Client error" else "Server error"
    reason = fromMaybe "" (Map.lookup s reasons)
    reasons =
      Map.fromList
        [ (400, "Bad Request"), (401, "Unauthorized"), (402, "Payment Required"), (403, "Forbidden")
        , (404, "Not Found"), (405, "Method Not Allowed"), (408, "Request Timeout"), (409, "Conflict")
        , (410, "Gone"), (422, "Unprocessable Entity"), (429, "Too Many Requests")
        , (500, "Internal Server Error"), (501, "Not Implemented"), (502, "Bad Gateway")
        , (503, "Service Unavailable"), (504, "Gateway Timeout") ]
