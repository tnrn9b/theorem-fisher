-- | What one attempt is given and what it returns ('AttemptSpec',
-- 'AttemptResult'), and the events a search writes to its log ('Event').
-- Its own module so that "Fisher.Search.Orchestrate" can use them without
-- importing "Fisher.Search.Scheduler", which re-exports all of it.
module Fisher.Search.Attempt
  ( AttemptSpec (..)
  , AttemptResult (..)
  , plainResult
  , Event (..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

import Fisher.Role (PromptPart)
import Fisher.Search.Child (ChildStats)
import Fisher.Search.Controller (ModelFailure, Stop)

data AttemptSpec = AttemptSpec
  { asId :: Int
  , asGoal :: Text
  , asBackend :: Text
  , asStart :: Text
  -- ^ the base the attempt starts from
  , asBrief :: PromptPart
  -- ^ extra text for the attempt's prompt (the graph search's brief; empty here)
  , asModel :: Maybe Text
  -- ^ the tier's model; Nothing is the backend's own
  , asTier :: Maybe Int
  -- ^ the index of the tier in 'scTiers'
  }
  deriving (Eq, Show)

data AttemptResult = AttemptResult
  { arText :: Maybe Text
  -- ^ the target file as the agent left it
  , arStats :: Maybe ChildStats
  , arNote :: Text
  , arStop :: Maybe Stop
  -- ^ the controller stopped the attempt, and why
  , arDollars :: Maybe Double
  -- ^ what the attempt spent, when known
  , arLastMessage :: Maybe Text
  -- ^ the attempt's last assistant text, when known
  , arFailure :: Maybe ModelFailure
  -- ^ the model failed (not the proof)
  }
  deriving (Eq, Show, Generic)

-- | Generic, with the field names as they are: a recorded run's results
-- are written to a file and read back for a replay.
instance ToJSON AttemptResult

instance FromJSON AttemptResult

-- | A result with no controller fields.
plainResult :: Maybe Text -> Maybe ChildStats -> Text -> AttemptResult
plainResult t s n = AttemptResult
  { arText = t, arStats = s, arNote = n, arStop = Nothing, arDollars = Nothing, arLastMessage = Nothing, arFailure = Nothing }

data Event
  = Launched Double Int Text Text
  | Finished Double Int Text Text Text (Maybe ChildStats) (Maybe Double)
  -- ^ time, attempt, goal, backend, outcome, stats, dollars (Nothing
  -- unless the controller is on)
  | GoalClosed Double Text Int
  | GoalWaiting Double Text Int
  | GoalGaveUp Double Text
  | AttemptCancelled Double Int Text (Maybe Double)
  -- ^ time, attempt, goal, dollars (Nothing unless the controller is on
  -- and the attempt was priced)
  | BudgetSpent Double
  | AutoClosed Double Text Text Double
  -- ^ time, goal, the snippets joined by @; @, seconds Auto took
  | AutoMissed Double Text Double
  -- ^ time, goal, seconds
  | Repaired Double Int Text Int Int Int
  -- ^ time, attempt, goal, holes before Auto, holes after, holes Auto closed
  | GoalDisproved Double Text Text Text Text (Maybe Text)
  -- ^ time, goal, @false@ or @suspect@, the negation's tactic or the
  -- counterexample, the source (@plausible@ or @smt@), and for a negation
  -- proved by a model's witness tactic the model
  | DisproveRan Double Text Text Text
  -- ^ time, goal, the source (@plausible@ or @smt@), its result:
  -- @counterexample@, @none@, @unsat@, @sat@, @unknown@ or @error@
  | AutoOff Double Text
  -- ^ time, why
  | AutoTimeout Double Text Text
  -- ^ time, goal, the step whose Lsp call was cut off at its cap:
  -- @auto@, @repair@ or @disprove@ (a step cut by @oCapped@ is
  -- @AutoMissed@)
  | IndexBuilt Double Int Int Double
  -- ^ time, files, declarations, seconds the build took
  | IndexSkipped Double FilePath Text
  -- ^ time, a file the index left out, why
  | Facts Double Text Int Int Bool
  -- ^ time, goal, facts, characters of the facts section, whether the
  -- retrieval was cut off
  | LibraryOffered Double Text [Text] Int Bool
  -- ^ time, goal, the lemmas offered, the notes offered, whether the
  -- retrieval was cut off
  | LibrarySaved Double Int Int
  -- ^ time, lemmas and notes the run added to the library
  | LibraryProblem Double Text
  -- ^ time, a library file set aside or a save that failed
  | TierChosen Double Int Text Int Text
  -- ^ time, attempt, goal or node, tier index, model
  | AttemptStopped Double Int Text Text Int Double
  -- ^ time, attempt, goal, reason (@turns@ or @dollars@), turns, dollars
  | Escalated Double Text Int Int
  -- ^ time, goal or node, from tier, to tier
  | TierSkipped Double Text Text
  -- ^ time, model, reason
  | LedgerUnread Double Int Text
  -- ^ time, attempt, reason
  | PolicyApplied Double Text Text Text Text Value Bool
  -- ^ time, goal, knob, the configured default, the value applied, the
  -- evidence, whether the attempt is exploration (the learned policy)
  deriving (Eq, Show)
