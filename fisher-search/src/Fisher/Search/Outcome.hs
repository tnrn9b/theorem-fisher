-- | An attempt's outcome in the run log's vocabulary (the design's 2.1):
-- @closed@, @failed@, @timeout@, @stalled@, @cancelled@, @budget@ or
-- @unknown@, from the scheduler's outcome text and what stopped the child.
module Fisher.Search.Outcome
  ( outcomeClass
  , outcomeClassText
  ) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Scheduler (AttemptRecord (..))

-- | In order: an outcome that starts with @closed@ is @closed@;
-- @cancelled: budget spent@, and the lemma graph's @cancelled: time@ and
-- @cancelled: dollars@ (its budget or its cap spent), are @budget@; any other cancellation (another
-- attempt closed or disproved the goal, or the search was interrupted) is
-- @cancelled@; an attempt the controller stopped
-- is @stalled@; a child killed at the time limit is @timeout@; a rejected,
-- unspliced, missing, errored or discarded proof, a model error, and a
-- proof left waiting on open goals (not a close) are @failed@; anything
-- else is @unknown@.
outcomeClass :: AttemptRecord -> Text
outcomeClass a = outcomeClassText (arOutcome a) (csExit <$> arChild a) (isJust (arStopped a))

-- | 'outcomeClass' from its parts, as a report records them: the outcome
-- text, the child's exit when there was a child ('Nothing' with no child,
-- @Just Nothing@ for a child killed at the time limit) and whether the
-- controller stopped it.
outcomeClassText :: Text -> Maybe (Maybe Int) -> Bool -> Text
outcomeClassText o child stopped
  | "closed" `T.isPrefixOf` o = "closed"
  | any (`T.isPrefixOf` o) ["cancelled: budget spent", "cancelled: time", "cancelled: dollars"] = "budget"
  | "cancelled" `T.isPrefixOf` o = "cancelled"
  | stopped = "stalled"
  | Just Nothing <- child = "timeout"
  | any (`T.isPrefixOf` o) failedPrefixes = "failed"
  | otherwise = "unknown"
  where
    failedPrefixes = ["rejected", "not spliced", "no file", "error", "model error", "discarded", "waiting"]
