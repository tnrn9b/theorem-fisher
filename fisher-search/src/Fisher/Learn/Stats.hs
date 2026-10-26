-- | Statistics of the search arms, from the run log ("Fisher.Runlog").
--
-- Three knobs are scored separately: the backend, the model tier (native
-- attempts only, by model name) and the pre-pass tactic. For each knob and
-- arm a 'Cell' of attempts, closes, dollars and seconds is kept at four
-- levels: the project's shape, the project (any shape), the shape in any
-- project, and everything.
--
-- What counts:
--
-- * An attempt is a run of kind @search-attempt@ with a kernel-checked
--   outcome: @closed@, @failed@, @timeout@, @stalled@ or @budget@. A
--   @cancelled@ attempt (another attempt closed its goal) and an @unknown@
--   one count as neither. Its shape is the one in its @attempt@ event,
--   else its goal's row on the parent search; its seconds are its wall
--   time, its dollars its cost (0 when unknown). Its tier arm is its
--   model; a tiered row with no model is not scored on the tier knob.
-- * A tactic is scored from the parent search's (a run of kind @search@,
--   @bench-import@ or @import@: an imported search is laid out as a live
--   one) @auto closed@ and
--   @auto missed@ events, an approximation of the pre-pass: an
--   @auto closed@ is one attempt and one close for each distinct tactic
--   that fired, with the event's seconds; an @auto missed@ is one failed
--   attempt for every tactic of the search's portfolio (the @tactics@ of
--   its first @portfolio@ event, else 'defaultTactics' when it logged
--   none), the seconds shared among them. Tactics cost no dollars. The tactics
--   tried before the one that fired are not seen, so a close does not
--   count against them. The tactic of a snippet ('snippetTactic') is its
--   first word once the @set_option maxHeartbeats N in@ wrapper and a
--   leading @by@ are stripped, mapped back to the default portfolio entry
--   that suggested it (@exact Foo.bar@ is @exact?@) when the word itself
--   is not one; any other word is kept as it is, so a configured tactic
--   outside the defaults (@--auto-tactics norm_num@) gets its closes, and
--   the policy drops the words that are not configured.
-- * The seconds to close ('stCloseSeconds') are the wall times of the
--   project's closed attempts, by shape, and under @""@ for all of them,
--   each with the budget the attempt ran at (@budget_secs@ of its
--   @attempt@ event, when it has one).
-- * An attempt of no known statement counts nowhere: goal @*@ (the
--   @--single@ baseline's whole file), an empty @shape@ in its @attempt@
--   event, or a goal row with an empty shape; nor do the auto events of
--   such a goal.
--
-- Smoothing runs down the hierarchy shape, project, shape anywhere,
-- everything: the rate at a level is @(closes + 4 p) / (attempts + 4)@
-- with @p@ the smoothed rate at the next coarser level, so an arm with no
-- attempts at a level has exactly that level's rate; at the global level
-- the prior is @a = b = 1@.
module Fisher.Learn.Stats
  ( Knob (..)
  , renderKnob
  , Cell (..)
  , Level (..)
  , Stats (..)
  , emptyStats
  , loadStats
  , cellAt
  , attemptsAnyLevel
  , shapeAttempts
  , smoothedRate
  , smoothedRateAt
  , dollarsPerAttempt
  , costPerClose
  , closesPerHour
  , countedOutcome
  , snippetTactic
    -- * Tables
  , ArmRow (..)
  , statsTable
  , tableShapes
  ) where

import Control.Monad (forM)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Runlog
import Fisher.Search.Auto (defaultTactics)

data Knob = KBackend | KTier | KTactic
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The knob's name in the policy file and the reports.
renderKnob :: Knob -> Text
renderKnob = \case
  KBackend -> "backends"
  KTier -> "tiers"
  KTactic -> "tactics"

data Cell = Cell {cAttempts :: Int, cCloses :: Int, cDollars :: Double, cSeconds :: Double}
  deriving (Eq, Show)

instance Semigroup Cell where
  Cell a c d s <> Cell a' c' d' s' = Cell (a + a') (c + c') (d + d') (s + s')

instance Monoid Cell where
  mempty = Cell 0 0 0 0

-- | The project's shape, the project, the shape anywhere, everything. The
-- shape is @""@ at 'LProject' and 'LGlobal'.
data Level = LShape | LProject | LGlobalShape | LGlobal
  deriving (Eq, Ord, Show, Enum, Bounded)

data Stats = Stats
  { stCells :: Map (Level, Text, Knob, Text) Cell
  , stCloseSeconds :: Map Text [(Double, Maybe Int)]
  -- ^ by shape (@""@: all of the project's), each closed attempt's wall
  -- time and the budget it ran at (its @attempt@ event's @budget_secs@;
  -- 'Nothing' when it logged none)
  }
  deriving (Eq, Show)

emptyStats :: Stats
emptyStats = Stats Map.empty Map.empty

-- | Kernel-checked outcomes; the others (@cancelled@, @unknown@) count as
-- neither close nor failure.
countedOutcome :: Text -> Bool
countedOutcome o = o `elem` ["closed", "failed", "timeout", "stalled", "budget"]

-- One observation: project key, shape, knob, arm, cell.
data Obs = Obs (Maybe Text) (Maybe Text) Knob Text Cell

-- | The stats of a project (by key) from the whole log: its own levels
-- from its runs, the global levels from every project's. The events read
-- are only the attempts' @attempt@ events and the searches' auto events,
-- in one query; goal rows are read only for searches that need them.
loadStats :: Runlog -> Text -> IO Stats
loadStats rl key = do
  attempts <- filter (maybe False countedOutcome . rrOutcome) <$> listRuns rl Nothing Nothing (Just SearchAttempt) 0
  parents <- concat <$> mapM (\k -> listRuns rl Nothing Nothing (Just k) 0) [SearchRun, BenchImport, ImportRun]
  evs <- runEventsOfKinds rl ["attempt", "portfolio", "auto closed", "auto missed"]
  let attemptData = Map.fromListWith (\_ first -> first) [(r, d) | (r, _, _, "attempt", d) <- evs]
      autoEvents = Map.fromListWith (flip (++)) [(r, [(k, d)]) | (r, _, _, k, d) <- evs, k `elem` ["auto closed", "auto missed"]]
      portfolios = Map.fromListWith (\_ first -> first) [(r, ts) | (r, _, _, "portfolio", d) <- evs, Just ts <- [strs "tactics" d]]
      shapeIn r = Map.lookup (rrId r) attemptData >>= str "shape"
      needGoals =
        Set.fromList ([rrId p | p <- parents, Map.member (rrId p) autoEvents] ++ [p | r <- attempts, Nothing <- [shapeIn r], Just p <- [rrParent r]])
  goalShapes <- Map.fromList <$> forM (Set.toList needGoals) (\p -> (,) p . goalMap <$> runGoals rl p)
  let attemptObs = concatMap (\r -> attemptObservations goalShapes (Map.lookup (rrId r) attemptData) (shapeIn r) r) attempts
      tacticObs =
        concat
          [ tacticObservations (rrProjectKey p) (Map.findWithDefault defaultTactics (rrId p) portfolios)
              (Map.findWithDefault Map.empty (rrId p) goalShapes) es
          | p <- parents, Just es <- [Map.lookup (rrId p) autoEvents] ]
      closes = [(sh, close) | (Obs k sh _ _ _, Just close) <- attemptObs, k == Just key]
  pure
    Stats
      { stCells = Map.fromListWith (<>) (concatMap (placed key) (map fst attemptObs ++ tacticObs))
      , stCloseSeconds =
          Map.fromListWith (flip (++)) ([(sh, [x]) | (Just sh, x) <- closes] ++ [("", [x]) | (_, x) <- closes])
      }
  where
    goalMap gs = Map.fromList [(grName g, grShape g) | g <- gs]

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty = \case Just t | not (T.null t) -> Just t; _ -> Nothing

-- An attempt's observations (its backend, and its tier when it has one),
-- the backend's paired with its seconds and budget when it closed (so each
-- closed attempt gives one close time). None for an attempt of no known
-- statement: goal @*@ (a whole-file run), an empty shape in its @attempt@
-- event, or, with no shape there, an empty shape on its goal's row.
attemptObservations :: Map Text (Map Text Text) -> Maybe Value -> Maybe Text -> RunRow -> [(Obs, Maybe (Double, Maybe Int))]
attemptObservations goalShapes d own r
  | goal == Just "*" || shape == Just "" = []
  | otherwise = backend ++ tier
  where
    goal = d >>= str "goal"
    fromParent = do
      p <- rrParent r
      g <- goal
      Map.lookup p goalShapes >>= Map.lookup g
    shape = maybe fromParent Just own
    budget = case d of
      Just (Object o) | Just (Number n) <- KM.lookup "budget_secs" o -> Just (round (toRealFloat n :: Double))
      _ -> Nothing
    secs = max 0 (fromMaybe (rrStartedAt r) (rrEndedAt r) - rrStartedAt r)
    closed = rrOutcome r == Just "closed"
    c = Cell 1 (if closed then 1 else 0) (fromMaybe 0 (rrCostUsd r)) secs
    key = rrProjectKey r
    backend = [(Obs key shape KBackend b c, if closed then Just (secs, budget) else Nothing) | Just b <- [rrBackend r]]
    tier = [(Obs key shape KTier m c, Nothing) | Just _ <- [rrTier r], Just m <- [nonEmpty (rrModel r)]]

-- The tactic observations of a search's auto events; none for a goal
-- whose row has an empty shape (no known statement).
tacticObservations :: Maybe Text -> [Text] -> Map Text Text -> [(Text, Value)] -> [Obs]
tacticObservations key portfolio shapes = concatMap one
  where
    one (kind, d) = case (kind, str "goal" d) of
      (_, Just g) | Map.lookup g shapes == Just "" -> []
      ("auto closed", Just g) ->
        let tactics = nub (mapMaybe (snippetTactic defaultTactics) (snippets (fromMaybe "" (str "snippets" d))))
         in [Obs key (Map.lookup g shapes) KTactic t (Cell 1 1 0 (seconds d)) | t <- tactics]
      ("auto missed", Just g) ->
        let n = fromIntegral (max 1 (length portfolio))
         in [Obs key (Map.lookup g shapes) KTactic t (Cell 1 0 0 (seconds d / n)) | t <- portfolio]
      _ -> []
    seconds d = case d of
      Object o | Just (Number n) <- KM.lookup "seconds" o -> max 0 (toRealFloat n)
      _ -> 0

-- The snippets of an @auto closed@ event, joined with @"; "@ by the
-- scheduler. Wrapped snippets are split only before the next wrapper, so
-- a @;@ inside one snippet does not make two.
snippets :: Text -> [Text]
snippets t
  | wrapper `T.isPrefixOf` t = [wrapper <> x | x <- drop 1 (T.splitOn ("; " <> wrapper) ("; " <> t))]
  | otherwise = T.splitOn "; " t
  where
    wrapper = "set_option maxHeartbeats "

-- | The tactic that produced a snippet: its first word after the
-- @set_option maxHeartbeats N in@ wrapper and a leading @by@; when that
-- word is not in the portfolio but the word with @?@ is, the latter (a
-- suggestion: @exact Foo@ came from @exact?@). 'Nothing' for an empty
-- snippet.
snippetTactic :: [Text] -> Text -> Maybe Text
snippetTactic portfolio s = case dropBy (unwrap (T.words s)) of
  w : _
    | w `notElem` portfolio, (w <> "?") `elem` portfolio -> Just (w <> "?")
    | otherwise -> Just w
  [] -> Nothing
  where
    unwrap = \case
      "set_option" : "maxHeartbeats" : _ : "in" : rest -> rest
      ws -> ws
    dropBy = \case
      "by" : rest -> rest
      ws -> ws

-- A list of strings under a key (the non-strings in it dropped).
strs :: Text -> Value -> Maybe [Text]
strs k = \case
  Object o | Just (Array a) <- KM.lookup (K.fromText k) o -> Just [t | String t <- foldr (:) [] a]
  _ -> Nothing

str :: Text -> Value -> Maybe Text
str k = \case
  Object o | Just (String t) <- KM.lookup (K.fromText k) o -> Just t
  _ -> Nothing

-- Where an observation goes: the project levels when it is the project's,
-- the global ones always; the shape levels only when its shape is known.
placed :: Text -> Obs -> [((Level, Text, Knob, Text), Cell)]
placed key (Obs k sh knob arm c) =
  [((LGlobal, "", knob, arm), c)]
    ++ [((LGlobalShape, s, knob, arm), c) | Just s <- [sh]]
    ++ [((LProject, "", knob, arm), c) | k == Just key]
    ++ [((LShape, s, knob, arm), c) | k == Just key, Just s <- [sh]]

-- | A cell, empty when absent. The shape is ignored at 'LProject' and
-- 'LGlobal'.
cellAt :: Stats -> Level -> Text -> Knob -> Text -> Cell
cellAt st lvl shape knob arm = Map.findWithDefault mempty (lvl, shapeAt lvl shape, knob, arm) (stCells st)

shapeAt :: Level -> Text -> Text
shapeAt lvl shape = case lvl of
  LShape -> shape
  LGlobalShape -> shape
  _ -> ""

-- | The most attempts the arm has at any level of the shape's hierarchy.
attemptsAnyLevel :: Stats -> Text -> Knob -> Text -> Int
attemptsAnyLevel st shape knob arm = maximum [cAttempts (cellAt st l shape knob arm) | l <- [minBound .. maxBound]]

-- | The attempts logged for a shape in the project: its 'LShape' cells'
-- attempts summed over the backend arms (each attempt has one backend).
shapeAttempts :: Stats -> Text -> Int
shapeAttempts st shape = sum [cAttempts c | ((LShape, sh, KBackend, _), c) <- Map.toList (stCells st), sh == shape]

-- The next coarser level: shape, project, shape anywhere, everything.
coarser :: Level -> Maybe Level
coarser = \case
  LShape -> Just LProject
  LProject -> Just LGlobalShape
  LGlobalShape -> Just LGlobal
  LGlobal -> Nothing

-- | The smoothed rate at a level for a shape: @(closes + 4 p) /
-- (attempts + 4)@, @p@ the next coarser level's; @(closes + 1) /
-- (attempts + 2)@ globally.
smoothedRateAt :: Stats -> Level -> Text -> Knob -> Text -> Double
smoothedRateAt st lvl shape knob arm =
  let c = cellAt st lvl shape knob arm
      n = fromIntegral (cAttempts c)
      k = fromIntegral (cCloses c)
   in case coarser lvl of
        Nothing -> (k + 1) / (n + 2)
        Just up -> (k + 4 * smoothedRateAt st up shape knob arm) / (n + 4)

-- The level of a policy entry's shape: the project's for @""@ or @"*"@,
-- else the shape's.
entryLevel :: Text -> Level
entryLevel shape = if shape `elem` ["", "*"] then LProject else LShape

entryShape :: Text -> Text
entryShape shape = if shape == "*" then "" else shape

-- | The smoothed close rate of an arm on a shape of the project (@""@ or
-- @"*"@: the project's).
smoothedRate :: Stats -> Text -> Knob -> Text -> Double
smoothedRate st shape = smoothedRateAt st (entryLevel shape) (entryShape shape)

-- The cell at the finest level of the entry's hierarchy where the arm has
-- attempts, 'Nothing' when it has none anywhere.
finest :: Stats -> Text -> Knob -> Text -> Maybe Cell
finest st shape knob arm = go (Just (entryLevel shape))
  where
    go = \case
      Nothing -> Nothing
      Just lvl ->
        let c = cellAt st lvl (entryShape shape) knob arm
         in if cAttempts c > 0 then Just c else go (coarser lvl)

-- | Dollars per attempt at the finest level where the arm has attempts;
-- 'Nothing' when it has none anywhere.
dollarsPerAttempt :: Stats -> Text -> Knob -> Text -> Maybe Double
dollarsPerAttempt st shape knob arm = (\c -> cDollars c / fromIntegral (cAttempts c)) <$> finest st shape knob arm

-- | Dollars per attempt over the smoothed rate (0 for an arm with no
-- attempts anywhere, which the policy ranks last).
costPerClose :: Stats -> Text -> Knob -> Text -> Double
costPerClose st shape knob arm = fromMaybe 0 (dollarsPerAttempt st shape knob arm) / smoothedRate st shape knob arm

-- | Closes per hour of the arm's seconds at the finest level where it has
-- attempts (0 with no seconds).
closesPerHour :: Stats -> Text -> Knob -> Text -> Double
closesPerHour st shape knob arm = case finest st shape knob arm of
  Just c | cSeconds c > 0 -> 3600 * fromIntegral (cCloses c) / cSeconds c
  _ -> 0

-- Tables -----------------------------------------------------------------

-- | One arm's line of a knob's table ('statsTable'): its cell's attempts
-- and closes at the level, and the entry's smoothed rate, dollars per
-- close ('Nothing' when the arm has no recorded dollars) and closes per
-- hour.
data ArmRow = ArmRow
  { arArm :: Text
  , arAttempts :: Int
  , arCloses :: Int
  , arRate :: Double
  , arCostPerClose :: Maybe Double
  , arClosesPerHour :: Double
  }
  deriving (Eq, Show)

-- | A knob's table at a level for a shape (@"*"@ at 'LProject'): the arms
-- of the given order (the policy's) first, then any others seen at that
-- level, sorted. What @fisher learn stats@ prints and the web app's Learn
-- view shows.
statsTable :: Stats -> Level -> Text -> Knob -> [Text] -> [ArmRow]
statsTable st lvl sh knob ordered = map line (ordered <> filter (`notElem` ordered) seen)
  where
    seen =
      sort (nub [arm | ((l, s, k, arm), _) <- Map.toList (stCells st), l == lvl, k == knob, lvl `notElem` [LShape, LGlobalShape] || s == sh])
    line arm =
      let c = cellAt st lvl sh knob arm
          perClose = case dollarsPerAttempt st sh knob arm of
            Just d | d > 0 -> Just (costPerClose st sh knob arm)
            _ -> Nothing
       in ArmRow arm (cAttempts c) (cCloses c) (smoothedRate st sh knob arm) perClose (closesPerHour st sh knob arm)

-- | The shapes the project has cells at, sorted.
tableShapes :: Stats -> [Text]
tableShapes st = Set.toList (Set.fromList [s | ((LShape, s, _, _), _) <- Map.toList (stCells st)])
