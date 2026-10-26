-- | The learned search policy: per project and per goal shape, the order of
-- the backends, model tiers and tactics and the per-attempt budget,
-- computed from the arms' statistics ("Fisher.Learn.Stats") within the
-- configured bounds.
--
-- The rules ('computePolicy'):
--
-- * Each knob's configured arms are ordered by cost per close
--   ('costPerClose': dollars per attempt over the smoothed rate),
--   ascending, ties by closes per hour, descending, then by the configured
--   order. An arm with no attempts at any level goes after the arms that
--   have some. An arm that has attempts but no recorded dollars (a backend
--   that reports no cost) is costed at the cheapest priced arm's dollars
--   per attempt, so an unknown cost does not rank it first; when no arm
--   is priced (tactics) the order is by closes per hour.
-- * The order is applied only when its first arm has at least 5 attempts
--   at some level and a smoothed rate at least 0.1 above the configured
--   first arm's; otherwise the configured order stands. So when the best
--   arm is the configured first one, the other arms keep their
--   configured order whatever the evidence among them.
-- * Only configured arms appear: an arm seen in the log but not
--   configured is dropped.
-- * The attempt budget is the 80th percentile (nearest rank) of the
--   shape's seconds to close, at least half of (rounded up) and at most the configured
--   budget, and 'Nothing' (the configured budget) with fewer than 5
--   closes. The policy never raises a budget and touches no cap. Only the
--   closes of attempts that ran at the configured budget count (a close
--   that logged no budget is taken to have): a close under a lowered
--   budget never exceeds it, so counting those would ratchet the budget
--   down on every recompute.
-- * Exploration ('gpExplore') is chosen per goal by the caller from the
--   attempts logged for the shape ('exploreNow'); 'computePolicy' leaves
--   it 'False'.
--
-- The file, @$FISHER_HOME/fisher/policy/<project_key>.json@, is
-- @{"version": 1, "computed_at", "project_key", "shapes": {shape:
-- {"backends", "tiers", "tactics", "attempt_secs", "evidence"}}}@ with a
-- @"*"@ shape for the default. A file that does not parse, of another
-- version, or with a key this version does not know is a 'Left'.
module Fisher.Learn.Policy
  ( GoalPolicy (..)
  , Policy (..)
  , Defaults (..)
  , computePolicy
  , policyFor
  , knobArms
  , exploreNow
  , policyPath
  , readPolicy
  , writePolicy
  , policyDiff
  , policyVersion
  , minAttempts
  , minGap
  ) where

import Control.Exception (IOException, onException, try)
import Control.Monad (join, unless)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openBinaryTempFile)

import Fisher.Learn.Stats

data GoalPolicy = GoalPolicy
  { gpBackends :: [Text]
  , gpTiers :: [Text]
  , gpTactics :: [Text]
  , gpAttemptSecs :: Maybe Int
  , gpExplore :: Bool
  , gpEvidence :: Value
  -- ^ per knob, per arm: @{attempts, closes, cost_per_close, rate}@
  }
  deriving (Eq, Show)

data Policy = Policy
  { pComputedAt :: Double
  , pProjectKey :: Text
  , pShapes :: Map Text GoalPolicy
  -- ^ by shape, @"*"@ for the default
  }
  deriving (Eq, Show)

-- | The configured arms (in their configured order) and attempt budget.
data Defaults = Defaults
  { dBackends :: [Text]
  , dTiers :: [Text]
  , dTactics :: [Text]
  , dAttemptSecs :: Int
  }
  deriving (Eq, Show)

policyVersion :: Int
policyVersion = 1

-- | The evidence an order needs: attempts of its first arm at some level,
-- and the gap of its smoothed rate over the configured first arm's.
minAttempts :: Int
minAttempts = 5

minGap :: Double
minGap = 0.1

-- | The policy at a time for a project, from the defaults and the stats,
-- with an entry for each shape seen and for @"*"@.
computePolicy :: Double -> Text -> Defaults -> Stats -> [Text] -> Policy
computePolicy at key d st shapes =
  Policy at key (Map.fromList [(sh, entry sh) | sh <- "*" : filter (/= "*") shapes])
  where
    entry sh =
      GoalPolicy
        { gpBackends = order sh KBackend (dBackends d)
        , gpTiers = order sh KTier (dTiers d)
        , gpTactics = order sh KTactic (dTactics d)
        , gpAttemptSecs = budget (if sh == "*" then "" else sh)
        , gpExplore = False
        , gpEvidence = object [K.fromText (renderKnob k) .= evidence sh k arms | (k, arms) <- knobs]
        }
    knobs = [(KBackend, dBackends d), (KTier, dTiers d), (KTactic, dTactics d)]
    order sh knob configured = case configured of
      [] -> []
      first : _ ->
        case rank sh knob configured of
          ranked@(best : _)
            | attemptsAnyLevel st sh knob best >= minAttempts
            , smoothedRate st sh knob best - smoothedRate st sh knob first >= minGap ->
                ranked
          _ -> configured
    rank sh knob configured =
      let perAttempt arm = dollarsPerAttempt st sh knob arm
          priced = [x | arm <- configured, Just x <- [perAttempt arm], x > 0]
          imputed = if null priced then 0 else minimum priced
          cost arm = case perAttempt arm of
            Just x | x > 0 -> x / smoothedRate st sh knob arm
            _ -> imputed / smoothedRate st sh knob arm
       in map snd
            ( sortOn
                (\(i, arm) -> (attemptsAnyLevel st sh knob arm == 0, cost arm, Down (closesPerHour st sh knob arm), i))
                (zip [0 :: Int ..] configured) )
    -- only the closes of attempts that ran at the configured budget (or
    -- logged none): a close under a lowered budget is at most that
    -- budget, so counting it would only ever lower the next one
    budget sh =
      let cap = dAttemptSecs d
          closes = [x | (x, b) <- Map.findWithDefault [] sh (stCloseSeconds st), maybe True (== cap) b]
       in if length closes < minAttempts
            then Nothing
            else Just (max ((cap + 1) `div` 2) (min cap (ceiling (percentile80 closes))))
    evidence sh knob arms =
      object
        [ K.fromText arm
            .= object
              [ "attempts" .= cAttempts c
              , "closes" .= cCloses c
              , "cost_per_close" .= costPerClose st sh knob arm
              , "rate" .= smoothedRate st sh knob arm ]
        | arm <- arms
        , let c = cellAt st (if sh == "*" then LProject else LShape) sh knob arm ]

-- The nearest-rank 80th percentile of a non-empty list.
percentile80 :: [Double] -> Double
percentile80 xs =
  let sorted = sortOn id xs
      n = length sorted
      r = max 1 (ceiling (0.8 * fromIntegral n :: Double))
   in sorted !! (r - 1)

-- | A shape's entry, else the default's (an empty one when the policy has
-- neither).
policyFor :: Policy -> Text -> GoalPolicy
policyFor p sh =
  Map.findWithDefault (Map.findWithDefault (GoalPolicy [] [] [] Nothing False (object [])) "*" (pShapes p)) sh (pShapes p)

-- | A knob's order in a shape's entry.
knobArms :: Knob -> GoalPolicy -> [Text]
knobArms = \case
  KBackend -> gpBackends
  KTier -> gpTiers
  KTactic -> gpTactics

-- | Whether an attempt explores (starts on the second-ranked arm), from the
-- attempts already logged for its shape: every eighth.
exploreNow :: Int -> Bool
exploreNow n = n `mod` 8 == 7

policyPath :: FilePath -> Text -> FilePath
policyPath home key = home </> "fisher" </> "policy" </> (T.unpack key <> ".json")

-- JSON -------------------------------------------------------------------

entryKeys, topKeys :: [K.Key]
entryKeys = ["backends", "tiers", "tactics", "attempt_secs", "evidence"]
topKeys = ["version", "computed_at", "project_key", "shapes"]

instance ToJSON GoalPolicy where
  toJSON g =
    object
      [ "backends" .= gpBackends g
      , "tiers" .= gpTiers g
      , "tactics" .= gpTactics g
      , "attempt_secs" .= gpAttemptSecs g
      , "evidence" .= gpEvidence g ]

instance FromJSON GoalPolicy where
  parseJSON = withObject "shape policy" $ \o -> do
    known o entryKeys
    GoalPolicy
      <$> o .: "backends"
      <*> o .: "tiers"
      <*> o .: "tactics"
      <*> (join <$> o .:? "attempt_secs")
      <*> pure False
      <*> o .:? "evidence" .!= object []

instance ToJSON Policy where
  toJSON p =
    object
      [ "version" .= policyVersion
      , "computed_at" .= pComputedAt p
      , "project_key" .= pProjectKey p
      , "shapes" .= pShapes p ]

instance FromJSON Policy where
  parseJSON = withObject "policy" $ \o -> do
    known o topKeys
    v <- o .: "version" :: Parser Int
    unless (v == policyVersion) (fail ("policy version " <> show v <> ", this build reads " <> show policyVersion))
    Policy <$> o .: "computed_at" <*> o .: "project_key" <*> o .: "shapes"

known :: A.Object -> [K.Key] -> Parser ()
known o ks = case filter (`notElem` ks) (KM.keys o) of
  [] -> pure ()
  extra -> fail ("unknown keys: " <> T.unpack (T.intercalate ", " (map K.toText extra)))

-- | The policy at a path: 'Nothing' when there is no file, 'Left' with the
-- reason when it cannot be read or does not parse. Never throws.
readPolicy :: FilePath -> IO (Either Text (Maybe Policy))
readPolicy path = do
  r <- try @IOException $ do
    there <- doesFileExist path
    if there then Just <$> BS.readFile path else pure Nothing
  pure $ case r of
    Left e -> Left (T.pack path <> ": " <> T.pack (show e))
    Right Nothing -> Right Nothing
    Right (Just bytes) -> case A.eitherDecodeStrict bytes of
      Left e -> Left (T.pack path <> ": " <> T.pack e)
      Right p -> Right (Just p)

-- | Write the policy (the directory made, the file replaced whole through
-- a temporary file of its own, so two writers do not share one).
writePolicy :: FilePath -> Policy -> IO ()
writePolicy path p = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  (tmp, h) <- openBinaryTempFile dir (takeFileName path <> ".tmp")
  (LBS.hPut h (A.encode p) >> hClose h >> renameFile tmp path)
    `onException` (hClose h >> try @IOException (removeFile tmp))

-- | What changes from the old policy (none: everything) to the new, by
-- shape and knob (@backends@, @tiers@, @tactics@, @attempt_secs@): old and
-- new values, @null@ for a shape one side lacks.
policyDiff :: Maybe Policy -> Policy -> [(Text, Text, Value, Value)]
policyDiff old new =
  [ (sh, knob, o, n)
  | sh <- Map.keys (Map.union (pShapes new) oldShapes)
  , (knob, f) <- fields
  , let o = maybe Null f (Map.lookup sh oldShapes)
        n = maybe Null f (Map.lookup sh (pShapes new))
  , old == Nothing || o /= n ]
  where
    oldShapes = maybe Map.empty pShapes old
    fields =
      [ ("backends", toJSON . gpBackends)
      , ("tiers", toJSON . gpTiers)
      , ("tactics", toJSON . gpTactics)
      , ("attempt_secs", toJSON . gpAttemptSecs) ]
