-- | Repair: turn a failed model attempt into a partial proof. Each tactic
-- unit Lean rejects in the target's proof becomes a @sorry@, for a few
-- rounds, until the proof elaborates; then Auto fills the new holes it
-- can, and what is left goes to the next attempt as its start, with the
-- goal state at each remaining hole.
--
-- Nothing here marks a goal proved; a repaired proof is only a start.
module Fisher.Search.Repair
  ( tacticUnit
  , sorryUnits
  , Repaired (..)
  , maxRepairRounds
  , maxRepairHoles
  , repair
  ) where

import Data.Char (isSpace)
import Data.List (find, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Fisher.Search.Auto
import Fisher.Search.Lean (Decl (..), declarations, findDecl, statementHead)

-- | A repaired attempt.
data Repaired = Repaired
  { rpText :: Text
  -- ^ the whole file, holes Auto closed filled
  , rpHolesBefore :: Int
  -- ^ holes in the target's proof before Auto
  , rpHolesAfter :: Int
  , rpAutoClosed :: Int
  , rpGoals :: [(Hole, Text)]
  -- ^ each remaining hole and its goal state
  }
  deriving (Eq, Show)

maxRepairRounds :: Int
maxRepairRounds = 4

maxRepairHoles :: Int
maxRepairHoles = 5

-- | The tactic unit holding an error line: @[from, to)@, 1-based lines.
-- Arguments: the file's lines, the declaration's first proof line and the
-- error line (both 1-based).
--
-- Inside the body of a @have ... := by@ (any line ending in @by@ with its
-- brackets closed), a @·@ branch, or a @case@, @next@ or @|@ alternative
-- ending in @=>@, the unit is that body. A branch whose body starts on
-- its header line (@· simp@) is the whole branch, header included;
-- 'sorryUnits' keeps the header. Elsewhere the unit is the error line
-- plus the lines below it indented deeper. A blank error line counts as
-- the nearest code line above it.
tacticUnit :: [Text] -> Int -> Int -> (Int, Int)
tacticUnit ls start e0 = fst (unitOf ls start e0)

-- | 'tacticUnit', and whether the unit is the error line's own one: the
-- line sits in no enclosing block or continuation.
unitOf :: [Text] -> Int -> Int -> ((Int, Int), Bool)
unitOf ls start e0 = (go e, null (parent e))
  where
    n = length ls
    line i = if i >= 1 && i <= n then ls !! (i - 1) else ""
    e = fromMaybe e0 (find (not . blank . line) [e0, e0 - 1 .. start])
    go i = case parent i of
      Nothing -> (i, contEnd i)
      Just p
        | Just (_, inline) <- branchHeader (line p) -> (if inline then p else p + 1, contEnd p)
        | opensBy (line p) -> (p + 1, contEnd p)
        | otherwise -> go p
    -- the nearest code line above, inside the proof, indented less
    parent i = find (\j -> not (blank (line j)) && indent (line j) < indent (line i)) [i - 1, i - 2 .. start]
    contEnd i = trim (scan (i + 1))
      where
        scan j
          | j > n = n + 1
          | blank (line j) || indent (line j) > indent (line i) = scan (j + 1)
          | otherwise = j
        trim k
          | k > i + 1 && blank (line (k - 1)) = trim (k - 1)
          | otherwise = k

blank :: Text -> Bool
blank = T.all isSpace

indent :: Text -> Int
indent = T.length . T.takeWhile (== ' ')

-- | The code of a line without a trailing @--@ comment.
code :: Text -> Text
code = T.stripEnd . fst . T.breakOn "--"

-- | A line ending in the word @by@ with its brackets balanced: it opens a
-- tactic block below it.
opensBy :: Text -> Bool
opensBy l =
  let c = code l
      r = T.dropEnd 2 c
      depth = T.length (T.filter (`elem` ("([{⟨" :: String)) c) - T.length (T.filter (`elem` (")]}⟩" :: String)) c)
   in "by" `T.isSuffixOf` c && (T.null (T.strip r) || isSpace (T.last r)) && depth <= 0

-- | A branch header: its text through the bullet or the @=>@ (indentation
-- included), and whether code follows on the same line.
branchHeader :: Text -> Maybe (Text, Bool)
branchHeader l =
  let ind = T.takeWhile (== ' ') l
      s = T.drop (T.length ind) (code l)
      firstWord = T.takeWhile (not . isSpace) s
   in case () of
        _
          | Just r <- T.stripPrefix "·" s <|> bullet s -> Just (ind <> T.take 1 s <> " ", not (blank r))
          | firstWord `elem` ["case", "next", "|"]
          , (pre, hit) <- T.breakOn "=>" s
          , not (T.null hit) ->
              Just (ind <> pre <> "=> ", not (blank (T.drop 2 hit)))
          | otherwise -> Nothing
  where
    bullet s = case T.stripPrefix "." s of
      Just r | T.null r || isSpace (T.head r) -> Just r
      _ -> Nothing
    a <|> b = maybe b Just a

data Act = Sorry | Drop
  deriving (Eq, Show)

-- | The merged units of a set of error lines, ascending; a unit that
-- overlaps another collapses with it into the outer one, and a 'Sorry'
-- wins over a 'Drop'.
unitsOf :: Text -> Decl -> [(Int, Act)] -> [((Int, Int), Act)]
unitsOf src d errs = merge (sortBy (comparing fst) us)
  where
    ls = T.splitOn "\n" src
    start = dStmtEnd d + 2
    us =
      [ (u, act)
      | (l, a) <- errs
      , l >= start && l <= dEnd d
      , let (u, own) = unitOf ls start l
            -- a dropped unit must be the error line's own one, never a
            -- block body (which would leave the block empty)
            act = if a == Drop && own then Drop else Sorry
      ]
    merge ((u1, a1) : (u2, a2) : rest)
      | fst u2 < snd u1 = merge (((fst u1, max (snd u1) (snd u2)), if a1 == Sorry || a2 == Sorry then Sorry else a1) : rest)
      | otherwise = (u1, a1) : merge ((u2, a2) : rest)
    merge xs = xs

-- | Replace merged units: a 'Sorry' unit becomes one @sorry@ at its first
-- line's indentation (after its branch header, when it starts with one);
-- a 'Drop' unit is removed.
rewrite :: Text -> [((Int, Int), Act)] -> Text
rewrite src us = T.intercalate "\n" (go 1 ls us)
  where
    ls = T.splitOn "\n" src
    go _ rest [] = rest
    go i rest (((a, b), act) : more) =
      let (keep, from) = splitAt (a - i) rest
          (unit, after) = splitAt (b - a) from
          first = case unit of
            (x : _) -> x
            [] -> ""
          new = case act of
            Drop -> []
            Sorry -> [maybe (T.takeWhile (== ' ') first) fst (branchHeader first) <> "sorry"]
       in keep <> new <> go b after more

-- | Replace the tactic unit of each error line (1-based) in the
-- declaration's proof with one @sorry@ at the unit's indentation. Lines
-- outside the proof are ignored; overlapping units collapse to the outer
-- one.
sorryUnits :: Text -> Decl -> [Int] -> Text
sorryUnits src d errs = rewrite src (unitsOf src d [(l, Sorry) | l <- errs])

-- | Repair a failed attempt: the goal's statement as in the start file,
-- the agent's file, the goal's name. @Right Nothing@ when there is
-- nothing to repair or the result fails a guard: the statement changed,
-- errors remain after 'maxRepairRounds' rounds (or a round changes
-- nothing), more than 'maxRepairHoles' holes, only @sorry@ left, or an
-- error in the target once Auto has filled its holes. Auto tries every
-- hole of the repaired target, the agent's own @sorry@s included, from
-- the last upward.
-- Errors outside the target declaration are ignored. An error saying
-- there are no goals left drops its unit instead of leaving a @sorry@,
-- when that unit is the error line's own (not an enclosing block's body).
-- The Lsp gets each file only up to the end of the target ('cutAfter').
-- The whole repair stops at 'acRepairSeconds' (calling 'lspTimedOut'),
-- with nothing repaired.
repair :: AutoConfig -> Lsp -> IO Double -> Text -> Text -> Text -> IO (Either AutoFail (Maybe Repaired))
repair cfg lsp clock stmt file name = do
  r <- timeout (max 1 (ceiling (acRepairSeconds cfg * 1000000))) (repairWithin cfg lsp clock stmt file name)
  case r of
    Just x -> pure x
    Nothing -> lspTimedOut lsp >> pure (Right Nothing)

repairWithin :: AutoConfig -> Lsp -> IO Double -> Text -> Text -> Text -> IO (Either AutoFail (Maybe Repaired))
repairWithin cfg lsp clock stmt file name =
  case findDecl name (declarations file) of
    Nothing -> pure (Right Nothing)
    Just d0
      | statementHead d0 /= statementHead d0 {dStatement = stmt} -> pure (Right Nothing)
      | otherwise ->
          diagsOf file >>= \case
            Left e -> pure (Left e)
            Right Nothing -> pure (Right Nothing)
            Right (Just ds)
              | null (proofErrors d0 ds) -> pure (Right Nothing)
              | otherwise -> rounds 1 file d0 ds
  where
    rounds :: Int -> Text -> Decl -> [Diag] -> IO (Either AutoFail (Maybe Repaired))
    rounds k src d ds
      | null (proofErrors d ds) =
          if any (inDecl d) (errorsOf ds) then pure (Right Nothing) else finish src d
      | k > maxRepairRounds = pure (Right Nothing)
      | otherwise =
          let us = unitsOf src d (proofErrors d ds)
              src' = rewrite src us
           in case findDecl name (declarations src') of
                Just d' | src' /= src ->
                  diagsOf src' >>= \case
                    Left e -> pure (Left e)
                    Right Nothing -> pure (Right Nothing)
                    Right (Just ds') -> rounds (k + 1) src' d' ds'
                _ -> pure (Right Nothing)

    finish src d =
      let hs = holesIn src d
       in if length hs > maxRepairHoles || onlySorry d
            then pure (Right Nothing)
            else do
              filled <- fill src 0 (sortBy (comparing (\h -> Down (hLine h, hCol h))) hs)
              case filled of
                Left e -> pure (Left e)
                Right (src', closed) -> case findDecl name (declarations src') of
                  Nothing -> pure (Right Nothing)
                  Just d' ->
                    -- one last check of the filled file: no error in the target
                    diagsOf src' >>= \case
                      Left e -> pure (Left e)
                      Right Nothing -> pure (Right Nothing)
                      Right (Just ds')
                        | any (inDecl d') (errorsOf ds') -> pure (Right Nothing)
                        | otherwise -> do
                            let left = holesIn src' d'
                            gs <- goals src' left
                            pure $ case gs of
                              Left e -> Left e
                              Right g -> Right (Just (Repaired src' (length hs) (length left) closed (zip left g)))

    fill src closed [] = pure (Right (src, closed))
    fill src closed (h : hs) =
      autoHole cfg lsp clock src h >>= \case
        Left (LspDown e) -> pure (Left (LspDown e))
        Left Missed -> fill src closed hs
        Right f -> fill (fillHole src h f) (closed + 1) hs

    goals _ [] = pure (Right [])
    goals src (h : hs) =
      call (lspGoal lsp (cut src) h) >>= \case
        Left e -> pure (Left e)
        Right g -> fmap (fromMaybe "" (g >>= id) :) <$> goals src hs

    diagsOf src = call (lspDiagnostics lsp (cut src))

    -- the file up to the end of the target
    cut src = maybe src (cutAfter src) (findDecl name (declarations src))

    -- one Lsp call under the per-hole wall cap: Nothing when it runs out;
    -- a synchronous exception counts as the server failing
    call :: IO (Either Text a) -> IO (Either AutoFail (Maybe a))
    call = guarded lsp (acSeconds cfg)

    errorsOf ds = [g | g <- ds, dgSeverity g == SevError]
    -- 1-based proof lines: dStmtEnd + 2 .. dEnd
    proofErrors d ds =
      [ (dgLine g, if "no goals" `T.isInfixOf` dgMessage g then Drop else Sorry)
      | g <- errorsOf ds, dgLine g >= dStmtEnd d + 2, dgLine g <= dEnd d ]
    inDecl d g = dgLine g >= dStart d + 1 && dgLine g <= dEnd d
    onlySorry d = all trivial (filter (not . blank) (map code (T.splitOn "\n" (dProof d))))
    trivial l =
      let rest = maybe (T.strip l) (\(h, _) -> T.strip (T.drop (T.length h) l)) (branchHeader l)
       in rest == "sorry" || T.null rest
