-- | The compute controller core: the stall rule over a goal's ledger, model
-- failure classification, and tier selection with per-tier health.  Pure.
module Fisher.Search.Controller
  ( StopReason (..)
  , stopReasonText
  , Stop (..)
  , StallRule (..)
  , defaultStallRule
  , Signal (..)
  , Progress (..)
  , emptyProgress
  , feed
  , stallVerdict
  , ledgerSignals
  , lastAssistant
  , ModelFailure (..)
  , modelFailureText
  , classifyFailure
  , Health
  , emptyHealth
  , noteResult
  , skipped
  , tierFor
  , cap2000
  ) where

import Data.Aeson (FromJSON, ToJSON, Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Scientific (toRealFloat)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data StopReason = StallTurns | StallDollars deriving (Eq, Show, Generic)

-- | Generic: the constructor's name as a string.
instance ToJSON StopReason

instance FromJSON StopReason

stopReasonText :: StopReason -> Text
stopReasonText = \case
  StallTurns -> "turns"
  StallDollars -> "dollars"

-- | Why and where a goal was stopped: turns and dollars since the last change.
data Stop = Stop { stReason :: StopReason, stTurns :: Int, stDollars :: Double }
  deriving (Eq, Show, Generic)

-- | Generic, with the field names as they are.
instance ToJSON Stop

instance FromJSON Stop

-- | Stop after this many turns, or this many dollars, without a change to the
-- target.  Nothing for dollars means the turn rule only.
data StallRule = StallRule { srTurns :: Int, srDollars :: Maybe Double }

defaultStallRule :: StallRule
defaultStallRule = StallRule 8 (Just 1.50)

data Signal = Turn | Spent Double | Changed deriving (Eq, Show)

-- | Work since the last change (turns, dollars), the total spend, and the last
-- assistant text seen.
data Progress = Progress
  { pgTurns :: Int
  , pgDollars :: Double
  , pgTotal :: Double
  , pgLastText :: Maybe Text
  , pgChanged :: Bool
  -- ^ the target has changed at least once
  }

emptyProgress :: Progress
emptyProgress = Progress 0 0 0 Nothing False

feed :: Progress -> Signal -> Progress
feed p = \case
  Turn -> p { pgTurns = pgTurns p + 1 }
  Spent d -> p { pgDollars = pgDollars p + d, pgTotal = pgTotal p + d }
  Changed -> p { pgTurns = 0, pgDollars = 0, pgChanged = True }

-- | Turns are checked first.  Before the target's first change the turn
-- limit is doubled: an attempt reads the goal and its context before it
-- edits (a cheap model that closed a TWISTED goal made 9 calls first).
stallVerdict :: StallRule -> Progress -> Maybe Stop
stallVerdict (StallRule t0 md) p
  | pgTurns p >= t = Just (Stop StallTurns (pgTurns p) (pgDollars p))
  | Just d <- md, pgDollars p >= d = Just (Stop StallDollars (pgTurns p) (pgDollars p))
  | otherwise = Nothing
  where
    t = if pgChanged p then t0 else 2 * t0

-- | Signals from ledger rows (seq, kind, data) in seq order.  A cost row is a
-- turn (and spend when it has a dollar figure), and a step on the target path
-- is a change.
ledgerSignals :: FilePath -> [(Int, Text, Value)] -> [Signal]
ledgerSignals target = concatMap signal
  where
    -- A turn is one model call, which the agent records as a cost row as it
    -- happens; its message rows are written only when the session ends, so
    -- they cannot pace a live attempt.
    signal (_, kind, v) = case kind of
      "cost" -> Turn : [Spent (toRealFloat n) | Just (Number n) <- [field "dollars" v]]
      "step" | field "path" v == Just (String (T.pack target)) -> [Changed]
      _ -> []

field :: Text -> Value -> Maybe Value
field k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

-- | Text of the last assistant message.
lastAssistant :: [(Int, Text, Value)] -> Maybe Text
lastAssistant rows = listToMaybe (reverse [t | (_, "message", v) <- rows, field "role" v == Just (String "assistant"), Just (String t) <- [field "text" v]])

data ModelFailure = Unavailable | NoTools | RateLimited deriving (Eq, Show, Generic)

-- | Generic: the constructor's name as a string.
instance ToJSON ModelFailure

instance FromJSON ModelFailure

modelFailureText :: ModelFailure -> Text
modelFailureText = \case
  Unavailable -> "unavailable model"
  NoTools -> "no tool support"
  RateLimited -> "rate limited"

-- | Case-insensitive; rate limits are tested first, then tool support, then
-- unavailable models.
classifyFailure :: Text -> Maybe ModelFailure
classifyFailure raw
  | has ["429", "ratelimiterror", "rate limit"] = Just RateLimited
  | has ["support tool use", "does not support tool", "tools are not supported", "tool use is not supported"] = Just NoTools
  | has ["404", "no endpoints found", "not a valid model", "model_not_found", "model not found", "is not available"] = Just Unavailable
  | otherwise = Nothing
  where
    t = T.toLower raw
    has = any (`T.isInfixOf` t)

-- | Per tier: consecutive rate limits, and the tiers skipped.
data Health = Health (M.Map Int Int) (S.Set Int)

emptyHealth :: Health
emptyHealth = Health M.empty S.empty

-- | Record a call result for a tier (Nothing is a success or any other error).
-- Unavailable and NoTools skip the tier at once; the third rate limit in a row
-- skips it.  The Bool is True when this result newly skips the tier.
noteResult :: Int -> Maybe ModelFailure -> Health -> (Health, Bool)
noteResult i r h@(Health counts skips)
  | S.member i skips = (h, False)
  | otherwise = case r of
      Nothing -> (Health (M.delete i counts) skips, False)
      Just Unavailable -> skip
      Just NoTools -> skip
      Just RateLimited ->
        let n = M.findWithDefault 0 i counts + 1
        in if n >= 3 then skip else (Health (M.insert i n counts) skips, False)
  where
    skip = (Health (M.delete i counts) (S.insert i skips), True)

skipped :: Health -> Int -> Bool
skipped (Health _ skips) i = S.member i skips

-- | The tier for the next step: start at @min k (n-1)@, else the next unskipped
-- tier above, else the highest unskipped tier below.
tierFor :: Int -> Health -> Int -> Maybe Int
tierFor n h k = case [i | i <- [start .. n - 1], ok i] of
  (i : _) -> Just i
  [] -> listToMaybe [i | i <- [start - 1, start - 2 .. 0], ok i]
  where
    start = max 0 (min k (n - 1))
    ok = not . skipped h

-- | Keep the last 2,000 characters, prefixing "[...]\n" when text was cut.
cap2000 :: Text -> Text
cap2000 t
  | T.length t <= 2000 = t
  | otherwise = "[...]\n" <> T.takeEnd 2000 t
