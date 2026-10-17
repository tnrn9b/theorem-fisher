-- | The model-call jobs of the graph search: the planner's split (with its
-- sketch checked), the verifier's review of a split, and the verifier's
-- check of a worker's claim that a statement is false. Each runs on its own
-- thread and returns its result for the loop to apply.
module Fisher.Search.GraphJobs
  ( Role (..)
  , AskError (..)
  , Ask
  , Asked (..)
  , Checker (..)
  , locked
  , Plan (..)
  , planJob
  , reviewJob
  , falseJob
  ) where

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Exception (SomeException, displayException, try)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Search.Check (AxiomPolicy, LeanRun, Verdict)
import Fisher.Search.Review
import Fisher.Search.Sketch
import Fisher.Search.Split

data Role = Worker | Planner | Verifier deriving (Eq, Ord, Show)

data AskError = AskFailed Text | AskNoCredit deriving (Eq, Show)

-- | One model call: the reply and its dollars.
type Ask = Role -> [Message] -> IO (Either AskError (Text, Double))

-- | What a job's model calls came to, with the dollars they cost.
data Asked a = NoCredit Double | Asked Double (Either Text a)

-- | How sketches are compiled. Probes write one scratch file beside the
-- target, so every check takes the lock.
data Checker = Checker
  { ckLean :: LeanRun, ckPolicy :: AxiomPolicy, ckRoot :: FilePath, ckRel :: FilePath, ckLock :: MVar () }

locked :: Checker -> IO a -> IO a
locked ck act = withMVar (ckLock ck) (const act)

-- | A planner's split that passed its check.
data Plan = Plan
  { plBlueprint :: Text
  , plLean :: Text
  -- ^ the sketch as the planner wrote it
  , plSketch :: Sketch
  , plBase :: Text
  -- ^ the base it was checked against
  , plApplied :: Text
  , plVerdict :: Verdict
  -- ^ the node's verdict in the applied text: Closed or Waiting
  , plTypes :: Map.Map Text Text
  -- ^ each lemma's printed type, by its name in the sketch
  }

-- | Two calls at most: a failed call, or a reply the reader refuses, gets one
-- more. A refused reply goes to the second call's messages with what was
-- wrong with it; the last problem, as text, is the job's error.
askTwice :: Ask -> Role -> (Maybe (Text, e) -> [Message]) -> (e -> Text) -> (Text -> IO (Either e a)) -> IO (Asked a)
askTwice ask role msgsFor errText readReply = go (0 :: Int) Nothing 0
  where
    go n prev cost = do
      r <- try (ask role (msgsFor prev))
      case r of
        Left e -> broke cost e
        Right (Left AskNoCredit) -> pure (NoCredit cost)
        Right (Left (AskFailed e)) -> again n cost ("the model call failed: " <> e) prev
        Right (Right (t, c)) -> do
          x <- try (readReply t)
          case x of
            Left e -> broke (cost + c) e
            Right (Right a) -> pure (Asked (cost + c) (Right a))
            Right (Left e) -> again n (cost + c) (errText e) (Just (t, e))
    again n cost e next
      | n == 0 = go 1 next cost
      | otherwise = pure (Asked cost (Left e))
    -- an exception ends the job as a failure, keeping the dollars already spent
    broke cost e = pure (Asked cost (Left ("the call broke: " <> T.take 300 (T.pack (displayException (e :: SomeException))))))

-- | The planner's split of a node: base, node name, its printed type, the
-- lemmas of the current plan it may use, whiteboard, notes of its failed
-- attempts, its retrieval text (facts and library; may be empty). One
-- repair round.
planJob :: Checker -> Ask -> Text -> Text -> Text -> [Text] -> Text -> [Text] -> Text -> IO (Asked Plan)
planJob ck ask base name expected usable board notes retrieved =
  askTwice ask Planner (plannerMessages base name usable board notes retrieved) problemText readPlan
  where
    readPlan t = case parsePlan t of
      Left e -> pure (Left (FormatError e))
      Right (bp, lean) -> case parseSketch base name lean >>= \sk -> (,) sk <$> applySketch base name sk of
        Left e -> pure (Left (FormatError (sketchErrorText e)))
        Right (sk, applied) -> do
          r <- locked ck (checkSketchVerdict (ckLean ck) (ckPolicy ck) (ckRoot ck) (ckRel ck) applied name expected (map fst (skLemmas sk)))
          pure (either (Left . LeanErrors) (\(v, tys) -> Right (Plan bp lean sk base applied v tys)) r)

-- | The verifier's review of a split: the node's statement, the plan, the whiteboard.
reviewJob :: Ask -> Text -> Plan -> Text -> IO (Asked SplitVerdict)
reviewJob ask stmt p board =
  askTwice ask Verifier (retryMessages (reviewMessages stmt (plBlueprint p) (plLean p) board)) id (pure . parseReview)

-- | The verifier's check of a claim that a statement is false.
falseJob :: Ask -> Text -> Text -> IO (Asked (Bool, Text))
falseJob ask stmt claim =
  askTwice ask Verifier (retryMessages (falseMessages stmt claim)) id (pure . parseFalse)
