-- | The part of Python's @datetime@ the cron code leans on: naive and
-- aware datetimes (a zone, or a fixed offset), wall-clock arithmetic,
-- @astimezone@, @isoformat@, @fromisoformat@ (3.11) and @strftime@.
module Fisher.Cron.Dt
  ( Tz (..)
  , Dt (..)
  , dtOffset
  , dtFromInstant
  , dtInstantMicros
  , dtInstantSeconds
  , addMicros
  , addMinutes
  , astimezone
  , replaceTz
  , isoformat
  , fromIsoformat
  , strftimeYmdHm
  , strftimeYmdHms
  , strftimeFile
  , strftimeHms
  , strftimeSessionId
  , micros
  , localSeconds
  , fromLocalSeconds
  ) where

import Data.Char (isDigit, ord)
import Data.Fixed (Pico)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, diffDays, fromGregorian, fromGregorianValid, gregorianMonthLength, toGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))

import Fisher.Cron.Zone

-- | A datetime's tzinfo: a zone (with the offset it is known to have when
-- it came from an instant, as @fold@ would pin it), or a fixed offset.
data Tz = TzZone Zone (Maybe Int) | TzFixed Int
  deriving (Eq, Show)

data Dt = Dt {dtLocal :: LocalTime, dtTz :: Maybe Tz}
  deriving (Eq, Show)

-- | Wall-clock seconds since 1970-01-01T00:00 as if UTC (whole seconds).
localSeconds :: LocalTime -> Int64
localSeconds (LocalTime d (TimeOfDay h m s)) =
  fromIntegral (diffDays d (fromGregorian 1970 1 1)) * 86400 + fromIntegral (h * 3600 + m * 60) + floor s

fromLocalSeconds :: Int64 -> Integer -> LocalTime
fromLocalSeconds secs us =
  let (dd, r) = fromIntegral secs `divMod` (86400 :: Integer)
      (h, r2) = r `divMod` 3600
      (m, s) = r2 `divMod` 60
   in LocalTime (addDays dd (fromGregorian 1970 1 1)) (TimeOfDay (fromIntegral h) (fromIntegral m) (fromIntegral s + fromIntegral us / 1000000))

-- | The microsecond field.
micros :: LocalTime -> Integer
micros (LocalTime _ (TimeOfDay _ _ s)) = round ((s - fromIntegral (floor s :: Integer)) * 1000000)

dtOffset :: Dt -> Maybe Int
dtOffset (Dt l tz) = case tz of
  Nothing -> Nothing
  Just (TzFixed o) -> Just o
  Just (TzZone _ (Just o)) -> Just o
  Just (TzZone z Nothing) -> Just (offsetForLocal z (localSeconds l))

-- | @datetime.fromtimestamp(t, tz)@, t in microseconds.
dtFromInstant :: Tz -> Integer -> Dt
dtFromInstant tz us =
  let (secs, frac) = us `divMod` 1000000
      off = case tz of
        TzFixed o -> o
        TzZone z _ -> offsetAt z (fromIntegral secs)
      l = fromLocalSeconds (fromIntegral secs + fromIntegral off) frac
   in Dt l (Just (case tz of TzZone z _ -> TzZone z (Just off); f -> f))

dtInstantMicros :: Dt -> Integer
dtInstantMicros d =
  (fromIntegral (localSeconds (dtLocal d)) - fromIntegral (maybe 0 id (dtOffset d))) * 1000000 + micros (dtLocal d)

dtInstantSeconds :: Dt -> Integer
dtInstantSeconds d = dtInstantMicros d `div` 1000000

-- | @d + timedelta(microseconds=n)@: wall-clock arithmetic, the zone kept
-- and its offset worked out again.
addMicros :: Integer -> Dt -> Dt
addMicros n (Dt l tz) =
  let total = (fromIntegral (localSeconds l)) * 1000000 + micros l + n
      (s, us) = total `divMod` 1000000
   in Dt (fromLocalSeconds (fromIntegral s) us) (fmap unpin tz)
  where
    unpin = \case
      TzZone z _ -> TzZone z Nothing
      f -> f

addMinutes :: Integer -> Dt -> Dt
addMinutes m = addMicros (m * 60 * 1000000)

-- | @d.astimezone(tz)@ for an aware datetime.
astimezone :: Tz -> Dt -> Dt
astimezone tz d = dtFromInstant tz (dtInstantMicros d)

-- | @d.replace(tzinfo=tz)@.
replaceTz :: Maybe Tz -> Dt -> Dt
replaceTz tz (Dt l _) = Dt l (fmap (\case TzZone z _ -> TzZone z Nothing; f -> f) tz)

two :: Integer -> Text
two n = T.justifyRight 2 '0' (T.pack (show n))

four :: Integer -> Text
four n = T.justifyRight 4 '0' (T.pack (show n))

-- | @d.isoformat()@
isoformat :: Dt -> Text
isoformat d =
  let l@(LocalTime day (TimeOfDay h m s)) = dtLocal d
      (y, mo, dd) = toGregorian day
      us = micros l
      base =
        four y <> "-" <> two (fromIntegral mo) <> "-" <> two (fromIntegral dd) <> "T" <> two (fromIntegral h) <> ":" <> two (fromIntegral m)
          <> ":" <> two (floor s) <> (if us /= 0 then "." <> T.justifyRight 6 '0' (T.pack (show us)) else "")
   in base <> maybe "" offsetText (dtOffset d)
  where
    offsetText o =
      let sign = if o < 0 then "-" else "+"
          a = abs o
          (hh, r) = a `divMod` 3600
          (mm, ss) = r `divMod` 60
       in sign <> two (fromIntegral hh) <> ":" <> two (fromIntegral mm) <> (if ss /= 0 then ":" <> two (fromIntegral ss) else "")

fields :: Dt -> (Integer, Int, Int, Int, Int, Int)
fields (Dt (LocalTime day (TimeOfDay h m s)) _) = let (y, mo, d) = toGregorian day in (y, mo, d, h, m, floor (s :: Pico))

-- | @strftime('%Y-%m-%d %H:%M')@
strftimeYmdHm :: Dt -> Text
strftimeYmdHm d = let (y, mo, dd, h, m, _) = fields d in four y <> "-" <> two' mo <> "-" <> two' dd <> " " <> two' h <> ":" <> two' m

-- | @strftime('%Y-%m-%d %H:%M:%S')@
strftimeYmdHms :: Dt -> Text
strftimeYmdHms d = let (_, _, _, _, _, s) = fields d in strftimeYmdHm d <> ":" <> two' s

-- | @strftime('%Y-%m-%d_%H-%M-%S')@
strftimeFile :: Dt -> Text
strftimeFile d = let (y, mo, dd, h, m, s) = fields d in four y <> "-" <> two' mo <> "-" <> two' dd <> "_" <> two' h <> "-" <> two' m <> "-" <> two' s

-- | @strftime('%H:%M:%S')@
strftimeHms :: Dt -> Text
strftimeHms d = let (_, _, _, h, m, s) = fields d in two' h <> ":" <> two' m <> ":" <> two' s

-- | @strftime('%Y%m%d_%H%M%S')@
strftimeSessionId :: Dt -> Text
strftimeSessionId d = let (y, mo, dd, h, m, s) = fields d in four y <> two' mo <> two' dd <> "_" <> two' h <> two' m <> two' s

two' :: Int -> Text
two' = two . fromIntegral

-- ---------------------------------------------------------------- fromisoformat

-- | @datetime.fromisoformat(s)@ (Python 3.11): the error text on failure.
fromIsoformat :: Text -> Either Text Dt
fromIsoformat s = do
  let invalid = Left ("Invalid isoformat string: " <> pyQuote s)
  (y, mo, d, rest) <- case parseDate s of
    Just x -> Right x
    Nothing -> invalid
  (tod, tz) <-
    if T.null rest
      then Right ((0, 0, 0, 0), Nothing)
      else case parseTime (T.drop 1 rest) of
        Just x -> Right x
        Nothing -> invalid
  let (h, mi, sec, us) = tod
  day <- case () of
    _
      | mo < 1 || mo > 12 -> Left "month must be in 1..12"
      | d < 1 || d > gregorianMonthLength y mo -> Left "day is out of range for month"
      | otherwise -> maybe (Left "day is out of range for month") Right (fromGregorianValid y mo d)
  if h > 23
    then Left "hour must be in 0..23"
    else
      if mi > 59
        then Left "minute must be in 0..59"
        else
          if sec > 59
            then Left "second must be in 0..59"
            else Right (Dt (LocalTime day (TimeOfDay h mi (fromIntegral sec + fromIntegral us / 1000000))) (fmap TzFixed tz))
  where
    digitsN :: Read a => Int -> Text -> Maybe (a, Text)
    digitsN n t = let (a, b) = T.splitAt n t in if T.length a == n && T.all isDigit a then Just (read (T.unpack a), b) else Nothing
    parseDate t
      | T.length t >= 10, T.index t 4 == '-' = do
          (y, r1) <- digitsN 4 t
          r1' <- T.stripPrefix "-" r1
          (mo, r2) <- digitsN 2 r1'
          r2' <- T.stripPrefix "-" r2
          (d, r3) <- digitsN 2 r2'
          Just (y, mo, d, r3)
      | otherwise = do
          (y, r1) <- digitsN 4 t
          (mo, r2) <- digitsN 2 r1
          (d, r3) <- digitsN 2 r2
          Just (y, mo, d, r3)
    parseTime :: Text -> Maybe ((Int, Int, Int, Integer), Maybe Int)
    parseTime t = do
      let (timePart, tzPart) = T.break (`elem` ['+', '-', 'Z']) t
      tod <- hms timePart
      tz <-
        if T.null tzPart
          then Just Nothing
          else
            if tzPart == "Z"
              then Just (Just 0)
              else do
                let sign = if T.head tzPart == '-' then -1 else 1
                (oh, om, os, _) <- hms (T.drop 1 tzPart)
                Just (Just (sign * (oh * 3600 + om * 60 + os)))
      Just (tod, tz)
    hms :: Text -> Maybe (Int, Int, Int, Integer)
    hms t = do
      (h, r1) <- digitsN 2 t
      if T.null r1
        then Just (h, 0, 0, 0)
        else do
          let ext = T.isPrefixOf ":" r1
              r1' = if ext then T.drop 1 r1 else r1
          (m, r2) <- digitsN 2 r1'
          if T.null r2
            then Just (h, m, 0, 0)
            else do
              r2' <- if ext then T.stripPrefix ":" r2 else Just r2
              (sec, r3) <- digitsN 2 r2'
              if T.null r3
                then Just (h, m, sec, 0)
                else do
                  r3' <- if T.head r3 `elem` ['.', ','] then Just (T.drop 1 r3) else Nothing
                  let ds = T.take 6 r3'
                      tailDs = T.drop 6 r3'
                  if T.null ds || not (T.all isDigit ds) || not (T.all isDigit tailDs)
                    then Nothing
                    else Just (h, m, sec, read (T.unpack ds) * 10 ^ (6 - T.length ds))

-- | Python's @repr@ of a string, as an error message shows it.
pyQuote :: Text -> Text
pyQuote t =
  let q = if T.any (== '\'') t && not (T.any (== '"') t) then '"' else '\''
      esc c
        | c == q = T.pack ['\\', c]
        | c == '\\' = "\\\\"
        | c == '\n' = "\\n"
        | c == '\t' = "\\t"
        | ord c < 32 = "\\x" <> T.justifyRight 2 '0' (T.pack (hex (ord c)))
        | otherwise = T.singleton c
      hex n = let ds = "0123456789abcdef" in [ds !! (n `div` 16), ds !! (n `mod` 16)]
   in T.singleton q <> T.concatMap esc t <> T.singleton q
