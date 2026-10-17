-- | Time zones as Python's @zoneinfo@ sees them: the compiled tz database
-- files (TZif, versions 1 to 4, with the POSIX TZ rule that covers times
-- after the last transition), or a fixed offset (what
-- @datetime.now().astimezone()@ gives when no zone is configured).
module Fisher.Cron.Zone
  ( Zone (..)
  , loadZone
  , utcZone
  , fixedZone
  , offsetAt
  , offsetForLocal
  , parseTzif
  , parsePosixTz
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isDigit)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, addDays, diffDays, fromGregorian, gregorianMonthLength, isLeapYear, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Vector as V
import System.FilePath ((</>))

-- | A zone: its name, its transitions (UTC seconds, offset in seconds
-- east of UTC), the offset before the first one, and the rule after the
-- last one.
data Zone = Zone
  { zName :: Text
  , zTransitions :: V.Vector (Int64, Int)
  , zInitial :: Int
  , zRule :: Maybe Posix
  }

instance Show Zone where
  show z = "Zone " <> show (zName z)

instance Eq Zone where
  a == b = zName a == zName b

utcZone :: Zone
utcZone = fixedZone "UTC" 0

fixedZone :: Text -> Int -> Zone
fixedZone n off = Zone n V.empty off Nothing

-- | @ZoneInfo(name)@ from a tz database directory; Nothing where it raises.
loadZone :: FilePath -> Text -> IO (Maybe Zone)
loadZone dir name
  | T.null name || ".." `T.isInfixOf` name || "/" `T.isPrefixOf` name = pure Nothing
  | otherwise = do
      r <- try @SomeException (BS.readFile (dir </> T.unpack name))
      pure $ case r of
        Right b -> parseTzif name b
        Left _ -> Nothing

-- ---------------------------------------------------------------- TZif

be :: BS.ByteString -> Int -> Int -> Integer
be b off n = foldl (\acc i -> acc * 256 + fromIntegral (BS.index b (off + i))) 0 [0 .. n - 1]

signed :: Int -> Integer -> Int64
signed n v = fromIntegral (if v >= 2 ^ (8 * n - 1) then v - 2 ^ (8 * n) else v)

parseTzif :: Text -> BS.ByteString -> Maybe Zone
parseTzif name b
  | BS.length b < 44 || BS.take 4 b /= "TZif" = Nothing
  | otherwise =
      let version = BS.index b 4
          counts off = let c i = fromIntegral (be b (off + 20 + 4 * i) 4) :: Int in (c 0, c 1, c 2, c 3, c 4, c 5)
          (isut, isstd, leap, timecnt, typecnt, charcnt) = counts 0
          v1len = timecnt * 4 + timecnt + typecnt * 6 + charcnt + leap * 8 + isstd + isut
       in if version == 0
            then block 44 4 timecnt typecnt Nothing
            else
              let h2 = 44 + v1len
                  (isut2, isstd2, leap2, timecnt2, typecnt2, charcnt2) = counts h2
                  d2 = h2 + 44
                  len2 = timecnt2 * 8 + timecnt2 + typecnt2 * 6 + charcnt2 + leap2 * 12 + isstd2 + isut2
                  footer = T.pack (map (toEnum . fromIntegral) (BS.unpack (BS.takeWhile (/= 10) (BS.drop (d2 + len2 + 1) b))))
               in if BS.length b < d2 + len2 then Nothing else block d2 8 timecnt2 typecnt2 (parsePosixTz footer)
  where
    block off w timecnt typecnt rule =
      let times = [signed w (be b (off + w * i) w) | i <- [0 .. timecnt - 1]]
          idxs = [fromIntegral (BS.index b (off + w * timecnt + i)) :: Int | i <- [0 .. timecnt - 1]]
          tbase = off + w * timecnt + timecnt
          types = V.fromList [fromIntegral (signed 4 (be b (tbase + 6 * i) 4)) :: Int | i <- [0 .. typecnt - 1]]
          offOf i = if i < V.length types then types V.! i else 0
          trans = V.fromList (zip times (map offOf idxs))
          initial = if V.null types then maybe 0 posixStd rule else offOf 0
       in Just (Zone name trans initial rule)

-- | The offset in effect at a UTC instant.
offsetAt :: Zone -> Int64 -> Int
offsetAt z t
  | V.null tr = maybe (zInitial z) (`posixOffsetAt` t) (zRule z)
  | t < fst (V.head tr) = zInitial z
  | t >= fst (V.last tr), Just r <- zRule z = posixOffsetAt r t
  | otherwise = snd (tr V.! search 0 (V.length tr - 1))
  where
    tr = zTransitions z
    -- the last transition at or before t
    search lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = (lo + hi + 1) `div` 2
           in if fst (tr V.! mid) <= t then search mid hi else search lo (mid - 1)

-- | The offset of a wall-clock time (seconds since the epoch as if it
-- were UTC), with @fold=0@: an ambiguous time takes the earlier offset,
-- a skipped one the offset before the change.
offsetForLocal :: Zone -> Int64 -> Int
offsetForLocal z l =
  let before = offsetAt z (l - 86400)
      after = offsetAt z (l + 86400)
      ok o = offsetAt z (l - fromIntegral o) == o
   in if ok before then before else if ok after then after else before

-- ---------------------------------------------------------------- POSIX TZ

-- | A POSIX TZ rule: standard offset, and a daylight offset with its
-- start and end (as @(rule, seconds after local midnight)@).
data Posix = Posix Int (Maybe (Int, (DateRule, Int), (DateRule, Int)))

posixStd :: Posix -> Int
posixStd (Posix s _) = s

data DateRule = MonthWeekDay Int Int Int | Julian1 Int | Julian0 Int

parsePosixTz :: Text -> Maybe Posix
parsePosixTz s0 = do
  (_, s1) <- name s0
  (stdW, s2) <- offset s1
  let std = negate stdW
  if T.null s2
    then Just (Posix std Nothing)
    else do
      (_, s3) <- name s2
      (dst, s4) <- case offset s3 of
        Just (w, r) | not (T.null s3), T.head s3 /= ',' -> Just (negate w, r)
        _ -> Just (std + 3600, s3)
      if T.null s4
        then Just (Posix std (Just (dst, (MonthWeekDay 3 2 0, 7200), (MonthWeekDay 11 1 0, 7200))))
        else do
          s5 <- T.stripPrefix "," s4
          let (a, rest) = T.breakOn "," s5
          startR <- rule a
          endR <- rule (T.drop 1 rest)
          Just (Posix std (Just (dst, startR, endR)))
  where
    name t
      | Just r <- T.stripPrefix "<" t = let (n, r') = T.breakOn ">" r in if T.null r' then Nothing else Just (n, T.drop 1 r')
      | otherwise = let (n, r) = T.span isAlpha t in if T.length n < 3 then Nothing else Just (n, r)
    offset t =
      let (sign, t1) = case T.uncons t of
            Just ('-', r) -> (-1, r)
            Just ('+', r) -> (1, r)
            _ -> (1, t)
          (body, rest) = T.span (\c -> isDigit c || c == ':') t1
          parts = map (read . T.unpack) (T.splitOn ":" body) :: [Int]
          secs = case parts of
            [h] -> h * 3600
            [h, m] -> h * 3600 + m * 60
            [h, m, s] -> h * 3600 + m * 60 + s
            _ -> 0
       in if T.null body then Nothing else Just (sign * secs, rest)
    rule t = do
      let (d, tm) = T.breakOn "/" t
      dr <- case T.uncons d of
        Just ('M', r) -> case T.splitOn "." r of
          [m, w, wd] -> Just (MonthWeekDay (rd m) (rd w) (rd wd))
          _ -> Nothing
        Just ('J', r) -> Just (Julian1 (rd r))
        _ | not (T.null d), T.all isDigit d -> Just (Julian0 (rd d))
        _ -> Nothing
      secs <- if T.null tm then Just 7200 else fst <$> offset (T.drop 1 tm)
      Just (dr, secs)
    rd = read . T.unpack :: Text -> Int

ruleDay :: Integer -> DateRule -> Day
ruleDay y = \case
  MonthWeekDay m w wd ->
    let first = fromGregorian y m 1
        (_, _, fwd) = toWeekDate first
        firstWd = fwd `mod` 7 -- Sunday = 0
        d1 = addDays (fromIntegral ((wd - firstWd) `mod` 7)) first
        cand = addDays (fromIntegral (7 * (w - 1))) d1
        lastDay = fromGregorian y m (gregorianMonthLength y m)
     in if cand > lastDay then addDays (-7) cand else cand
  Julian1 n ->
    -- 1..365, February 29 never counted
    let base = addDays (fromIntegral (n - 1)) (fromGregorian y 1 1)
     in if isLeapYear y && n >= 60 then addDays 1 base else base
  Julian0 n -> addDays (fromIntegral n) (fromGregorian y 1 1)

posixOffsetAt :: Posix -> Int64 -> Int
posixOffsetAt (Posix std Nothing) _ = std
posixOffsetAt (Posix std (Just (dst, (sr, st), (er, et)))) t =
  let dayOf x = fromIntegral (x `div` 86400) :: Integer
      (y, _, _) = toGregorian (addDays (dayOf (t + fromIntegral std)) (fromGregorian 1970 1 1))
      epoch d = fromIntegral (diffDays d (fromGregorian 1970 1 1)) * 86400 :: Int64
      -- transitions as UTC instants: the start in standard time, the end in daylight time
      startAt yy = epoch (ruleDay yy sr) + fromIntegral st - fromIntegral std
      endAt yy = epoch (ruleDay yy er) + fromIntegral et - fromIntegral dst
      inDst yy =
        let s = startAt yy
            e = endAt yy
         in if s < e then t >= s && t < e else not (t >= e && t < s)
   in if inDst y then dst else std
