-- | A port of the part of croniter 6.0.0 that upstream uses: expanding a
-- numeric cron expression (5 fields, or 6 with seconds, or 7 with years)
-- with croniter's own error texts, and @get_next@ from an aware datetime,
-- with its daylight-saving adjustment as it is (including its quirks).
module Fisher.Cron.Croniter
  ( CronExpr
  , CronError (..)
  , expand
  , Iter
  , newIter
  , getNext
  ) where

import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Time.Calendar (dayOfWeek, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))

import Fisher.Cron.Dt

data Val = Star | Num Int
  deriving (Eq, Show)

data CronExpr = CronExpr
  { ceFields :: [[Val]]
  , ceDomDowOr :: Bool
  }
  deriving (Show)

-- | The exception's class and message.
data CronError = CronError Text Text
  deriving (Eq, Show)

ranges :: [(Int, Int)]
ranges = [(0, 59), (0, 23), (1, 31), (1, 12), (0, 6), (0, 59), (1970, 2099)]

lenMeansAll :: [Int]
lenMeansAll = [60, 24, 31, 12, 7, 60, 130]

dayF, dowF :: Int
dayF = 2
dowF = 4

badE :: Text -> Either CronError a
badE = Left . CronError "CroniterBadCronError"

isInt :: Text -> Bool
isInt t = not (T.null t) && T.all isDigit t

-- | @croniter.expand(expr_format)@ for the syntax upstream lets through.
expand :: Text -> Either CronError CronExpr
expand exprFormat = do
  let efl = T.toLower exprFormat
      exprs = T.words efl
      n = length exprs
  if n `notElem` [5, 6, 7]
    then badE "Exactly 5, 6 or 7 columns has to be specified for iterator expression."
    else do
      fs <- mapM (field exprs n) (zip [0 ..] exprs)
      let collapsed =
            [ if length vs == lenMeansAll !! i && not ((i == dayF && not ("*" `T.isInfixOf` (exprs !! dowF))) || (i == dowF && not ("*" `T.isInfixOf` (exprs !! dayF))))
                then [Star]
                else vs
            | (i, vs) <- zip [0 ..] fs ]
      pure (CronExpr [if vs == [Star] then [Star] else vs | vs <- collapsed] True)
  where
    field exprs n (i, expr) = go (reverse (T.splitOn "," expr)) []
      where
        bad :: Text -> Either CronError a
        bad = badE
        notAlpha :: Either CronError a
        notAlpha = Left (CronError "CroniterNotAlphaError" ("[" <> T.unwords exprs <> "] is not acceptable"))
        (lo, hi) = ranges !! i
        alias v
          | i == dowF && v == 7 && n == 5 = 0
          | i == dayF && v == 0 && n == 6 = 1
          | otherwise = v
        go [] acc = Right (sortOn key (nub acc))
        go (e : rest) acc =
          let t0 = case T.stripPrefix "*/" e of
                Just st | not (T.null st) -> T.pack (show lo) <> "-" <> T.pack (show hi) <> "/" <> st
                _ -> e
              m0 = stepSearch t0
              t1 = case T.breakOnEnd "/" e of
                (a, b) | not (T.null a), T.length a > 1, not (T.null b) -> T.dropEnd 1 a <> "-" <> T.pack (show hi) <> "/" <> b
                _ -> e
              m = case m0 of
                Just x -> Just x
                Nothing -> stepSearch t1
           in case m of
                Just (low, high, step) -> do
                  if not (isInt low) || not (isInt high) then notAlpha else Right ()
                  let lowV = alias (read (T.unpack low))
                      highV = alias (read (T.unpack high))
                      stepV = maybe 1 (read . T.unpack) step
                  if max lowV highV > max lo hi then bad (exprFormat <> " is out of bands") else Right ()
                  rng <-
                    if stepV == 0
                      then bad "invalid range: range() arg 3 must not be zero"
                      else
                        Right $
                          if lowV > highV
                            then
                              let whole = [lo .. hi]
                                  r1 = [lowV, lowV + stepV .. hi]
                                  toSkip = case r1 of
                                    [] -> 0
                                    _ ->
                                      let lastV = last r1
                                          alreadySkipped = length (takeWhile (/= lastV) (reverse whole))
                                          curpos = length (takeWhile (/= lastV) whole)
                                       in if curpos + stepV > length whole && alreadySkipped < stepV then stepV - alreadySkipped else 0
                               in r1 <> [lo + toSkip, lo + toSkip + stepV .. highV]
                            else
                              if lowV == highV
                                then [lo, lo + stepV .. hi]
                                else [lowV, lowV + stepV .. highV]
                  let pending = map (T.pack . show) rng
                      rest' = reverse [p | p <- pending, p `notElem` rest] <> rest
                  go rest' acc
                Nothing
                  | "-" `T.isPrefixOf` e -> bad ("[" <> exprFormat <> "] is not acceptable,negative numbers not allowed")
                  | e == "*" -> go rest (Star : acc)
                  | not (isInt e) -> notAlpha
                  | otherwise -> do
                      let v = alias (read (T.unpack e))
                      if v < lo || v > hi then bad ("[" <> exprFormat <> "] is not acceptable, out of range") else go rest (Num v : acc)
    key = \case
      Star -> "*"
      Num v -> T.justifyRight 2 '0' (T.pack (show v))
    -- @^([^-]+)-([^-/]+)(/(\d+))?$@
    stepSearch t =
      let (a, r) = T.breakOn "-" t
       in if T.null a || T.null r
            then Nothing
            else
              let r' = T.drop 1 r
                  (b, s) = T.break (\c -> c == '-' || c == '/') r'
               in if T.null b
                    then Nothing
                    else case T.uncons s of
                      Nothing -> Just (a, b, Nothing)
                      Just ('/', st) | isInt st -> Just (a, b, Just st)
                      _ -> Nothing

-- ---------------------------------------------------------------- iteration

-- | A croniter: the expression, the zone of the start time, the current
-- time and the DST reference (seconds).
data Iter = Iter
  { itExpr :: CronExpr
  , itTz :: Maybe Tz
  , itCur :: Rational
  , itDstStart :: Rational
  }

-- | @croniter(expr, start)@
newIter :: CronExpr -> Dt -> Iter
newIter e start =
  let ts = toRational (dtInstantMicros start) / 1000000
   in Iter e (dtTz start) ts ts

toDt :: Maybe Tz -> Integer -> Dt
toDt tz secs = case tz of
  Just z -> dtFromInstant z (secs * 1000000)
  Nothing -> dtFromInstant (TzFixed 0) (secs * 1000000)

tsOf :: Dt -> Integer
tsOf = dtInstantSeconds

-- | relativedelta: relative years, months, days, hours, minutes, seconds;
-- absolute month, day, hour, minute, second.
data RD = RD {rYears, rMonths, rDays, rHours, rMinutes, rSeconds :: Integer, aMonth, aDay, aHour, aMinute, aSecond :: Maybe Int}

rd0 :: RD
rd0 = RD 0 0 0 0 0 0 Nothing Nothing Nothing Nothing Nothing

applyRD :: RD -> Dt -> Dt
applyRD r d =
  let LocalTime day (TimeOfDay h mi sec) = dtLocal d
      (y0, mo0, dd0) = toGregorian day
      mAbs = maybe mo0 id (aMonth r)
      total = toInteger mAbs - 1 + rMonths r
      y = y0 + rYears r + total `div` 12
      mo = fromInteger (total `mod` 12) + 1
      dd = min (gregorianMonthLength y mo) (maybe dd0 id (aDay r))
      us = micros (dtLocal d)
      sec' = case aSecond r of
        Just s' -> fromIntegral s' + fromIntegral us / 1000000
        Nothing -> sec
      replaced = d {dtLocal = LocalTime (fromGregorian y mo dd) (TimeOfDay (maybe h id (aHour r)) (maybe mi id (aMinute r)) sec')}
      delta = ((rDays r * 24 + rHours r) * 60 + rMinutes r) * 60 + rSeconds r
   in addMicros (delta * 1000000) replaced

daysIn :: Integer -> Int -> Int
daysIn = gregorianMonthLength

nextDiff :: Int -> [Int] -> Maybe Int -> Maybe Int
nextDiff x xs rangeVal = case [v - x | v <- xs, v >= x] of
  (d : _) -> Just d
  [] -> case (rangeVal, xs) of
    (Nothing, _) -> Nothing
    (Just r, v : _) -> Just (v - x + r)
    (Just _, []) -> Nothing

-- | @croniter._calc(now, expanded, ..., is_prev=False)@: Nothing when no
-- date is found within 50 years.
calc :: Maybe Tz -> Rational -> [[Val]] -> Maybe Integer
calc tz now0 fs = loop dst0 (yearOf dst0) (monthOf dst0)
  where
    n = length fs
    offset = if n > 5 then 1 else 60
    now = floor now0 :: Integer
    dst0 = toDt tz (now + offset)
    currentYear = yearOf dst0
    yearOf d = let (y, _, _) = toGregorian (localDay (dtLocal d)) in y
    monthOf d = let (_, m, _) = toGregorian (localDay (dtLocal d)) in m
    dayOf d = let (_, _, dd) = toGregorian (localDay (dtLocal d)) in dd
    tod d = localTimeOfDay (dtLocal d)
    ints i = [v | Num v <- fs !! i]
    star i = Star `elem` (fs !! i)
    -- each step: Nothing to stop (no year left), or whether it changed the
    -- date (then the loop starts again) and the date
    loop d year month
      | abs (year - currentYear) > 50 = Nothing
      | otherwise = run procs d
      where
        run [] dd = Just (tsOf dd)
        run (p : ps) dd = case p dd of
          Nothing -> Nothing
          Just (True, dd') -> loop dd' (yearOf dd') (monthOf dd')
          Just (False, dd') -> run ps dd'
        procs = [procYear, procMonth, procDom, procDow, procHour, procMinute, procSecond]
        moved r dd = Just (True, applyRD r dd)
        same dd = Just (False, dd)
        procYear dd
          | n == 7, not (star 6) = case nextDiff (fromInteger (yearOf dd)) (ints 6) Nothing of
              Nothing -> Nothing
              Just 0 -> same dd
              Just dy -> moved rd0 {rYears = toInteger dy, aMonth = Just 1, aDay = Just 1, aHour = Just 0, aMinute = Just 0, aSecond = Just 0} dd
          | otherwise = same dd
        procMonth dd
          | star 3 = same dd
          | otherwise = case nextDiff (monthOf dd) (ints 3) (Just 12) of
              Just dm | dm /= 0 -> moved rd0 {rMonths = toInteger dm, aDay = Just 1, aHour = Just 0, aMinute = Just 0, aSecond = Just 0} dd
              _ -> same dd
        procDom dd
          | star dayF = same dd
          | otherwise = case nextDiff (dayOf dd) (ints dayF) (Just (daysIn year month)) of
              Just ddf | ddf /= 0 -> moved rd0 {rDays = toInteger ddf, aHour = Just 0, aMinute = Just 0, aSecond = Just 0} dd
              _ -> same dd
        procDow dd
          | star dowF = same dd
          | otherwise =
              let wd = fromEnum (dayOfWeek (localDay (dtLocal dd))) `mod` 7
               in case nextDiff wd (ints dowF) (Just 7) of
                    Just dw | dw /= 0 -> moved rd0 {rDays = toInteger dw, aHour = Just 0, aMinute = Just 0, aSecond = Just 0} dd
                    _ -> same dd
        procHour dd
          | star 1 = same dd
          | otherwise = case nextDiff (todHour (tod dd)) (ints 1) (Just 24) of
              Just dh | dh /= 0 -> moved rd0 {rHours = toInteger dh, aMinute = Just 0, aSecond = Just 0} dd
              _ -> same dd
        procMinute dd
          | star 0 = same dd
          | otherwise = case nextDiff (todMin (tod dd)) (ints 0) (Just 60) of
              Just dmi | dmi /= 0 -> moved rd0 {rMinutes = toInteger dmi, aSecond = Just 0} dd
              _ -> same dd
        procSecond dd
          | n > 5 =
              if star 5
                then same dd
                else case nextDiff (floor (todSec (tod dd))) (ints 5) (Just 60) of
                  Just ds | ds /= 0 -> moved rd0 {rSeconds = toInteger ds} dd
                  _ -> same dd
          -- five fields: the seconds go to zero and the search carries on
          | otherwise = Just (False, applyRD rd0 {aSecond = Just 0} dd)

-- | @get_next(datetime)@: the next fire time, and the iterator moved on.
getNext :: Iter -> Either CronError (Dt, Iter)
getNext it = do
  let e = itExpr it
      fs = ceFields e
      tz = itTz it
      cur = itCur it
      both = ceDomDowOr e && head (fs !! dayF) /= Star && head (fs !! dowF) /= Star
      run fs' = calc tz cur fs'
      res =
        if both
          then
            let t1 = run (set dowF [Star] fs)
                t2 = run (set dayF [Star] fs)
             in case (t1, t2) of
                  (Just a, Just b) -> Just (min a b)
                  _ -> Nothing
          else run fs
  result <- maybe (Left (CronError "CroniterBadDateError" "failed to find next date")) Right res
  let dtStart = toDt tz (floor (itDstStart it))
      startOff = maybe 0 id (dtOffset dtStart)
      dtResult = toDt tz result
      resOff = maybe 0 id (dtOffset dtResult)
      lagHours = fromIntegral (wallSeconds dtResult - wallSeconds dtStart) / 3600 :: Double
      lag = resOff - startOff
      hoursBeforeMidnight = 24 - todHour (localTimeOfDay (dtLocal dtStart))
      adjust = resOff /= startOff && ((lag > 0 && abs lagHours >= fromIntegral hoursBeforeMidnight) || (lag < 0 && (3600 * abs lagHours + fromIntegral (abs lag)) >= fromIntegral (hoursBeforeMidnight * 3600)))
  if adjust
    then do
      let adjusted = addMicros (negate (toInteger lag) * 1000000) dtResult
          resultAdj = tsOf adjusted
      pure (adjusted, it {itCur = fromInteger resultAdj, itDstStart = fromInteger resultAdj})
    else pure (dtResult, it {itCur = fromInteger result})
  where
    set i v xs = take i xs <> [v] <> drop (i + 1) xs
    wallSeconds d = toInteger (localSeconds (dtLocal d))
