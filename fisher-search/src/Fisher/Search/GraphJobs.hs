{-# LANGUAGE DataKinds #-}

-- | The model-call jobs of the graph search: the planner's split (with its
-- sketch checked), the verifier's review of a split, and the verifier's
-- check of a worker's claim that a statement is false. Each runs in its
-- role's monad ("Fisher.Role.Agent") on its own thread and returns its
-- result for the loop to apply.
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
  , sketchProblem
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Role (PromptPart, Role (..), Said, saidText, static, toPrompt)
import Fisher.Role.Agent (AgentM, askTwice, excerpt, withChecker)
import Fisher.Search.Ask
import Fisher.Search.Check (Verdict)
import Fisher.Search.Review
import Fisher.Search.Sketch
import Fisher.Search.Split

-- | A planner's split that passed its check.
data Plan = Plan
  { plBlueprint :: Said 'Planner
  -- ^ what the planner said the split is: its blueprint block, read out of
  -- its reply
  , plLean :: Text
  -- ^ the sketch as the planner wrote it; checked Lean once the kernel has
  -- accepted it, so it goes into a prompt as source
  , plSketch :: Sketch
  , plBase :: Text
  -- ^ the base it was checked against
  , plApplied :: Text
  , plVerdict :: Verdict
  -- ^ the node's verdict in the applied text: Closed or Waiting
  , plTypes :: Map.Map Text Text
  -- ^ each lemma's printed type, by its name in the sketch
  }

-- | The planner's split of a node: base, node name, its printed type, the
-- lemmas of the current plan it may use, whiteboard, notes of its failed
-- attempts, its retrieval text (facts and library; may be empty). One
-- repair round. The sketch is checked with the planner's checker, under
-- its lock.
planJob :: Text -> Text -> Text -> [Text] -> PromptPart -> [Text] -> PromptPart -> AgentM 'Planner (Asked Plan)
planJob base name expected usable board notes retrieved =
  askTwice (plannerMessages base name usable board notes retrieved) problemText readPlan
  where
    readPlan s = case excerpt parsePlan s of
      Left e -> pure (Left (FormatError (static e)))
      Right (bp, lean) -> case parseSketch base name lean >>= \sk -> (,) sk <$> applySketch base name sk of
        Left e -> pure (Left (FormatError (sketchProblem name s e)))
        Right (sk, applied) -> do
          r <- withChecker (\ck -> checkSketchVerdict (ckLean ck) (ckPolicy ck) (ckRoot ck) (ckRel ck) applied name expected (map fst (skLemmas sk)))
          pure (either (Left . LeanErrors) (\(v, tys) -> Right (Plan bp lean sk base applied v tys)) r)

-- | A sketch error for the planner's repair round: Fisher's sentence, and
-- in it the planner's name or line as what the planner said: an 'excerpt'
-- that finds the error's name or line in the reply @s@ and reads it out of
-- the reply (empty if it were not there; the parser took it from the
-- reply's lean block, so it is). Its text is 'sketchErrorText'. A lemma
-- name that clashes is qualified with the node's namespace (@name@'s),
-- which is Fisher's.
sketchProblem :: Text -> Said 'Planner -> SketchError -> PromptPart
sketchProblem name s = \case
  LemmaNotOpen n -> static "the lemma " <> said n <> static " has a proof; each new lemma must end in `:= by sorry` or `:= sorry`"
  DuplicateName n -> static "the name " <> said n <> static " is used for two lemmas"
  NameClash n -> static "the name " <> qualified n <> static " is already in the file"
  DeniedLine l -> static "a line is not allowed (axiom, #exit or an option that turns checks off): " <> said l
  ExtraText l -> static "text outside declarations: " <> said l
  e -> static (sketchErrorText e)
  where
    said x = case excerpt (\t -> Right (T.take (T.length x) (snd (T.breakOn x t)), ()) :: Either () (Text, ())) s of
      Right (p, ()) -> toPrompt p
      Left () -> mempty
    pre = nsPrefix name
    qualified n
      | not (T.null pre), not (n `T.isInfixOf` saidText s), pre `T.isPrefixOf` n = static pre <> said (T.drop (T.length pre) n)
      | otherwise = said n

-- | The verifier's review of a split: the node's statement, the plan, the
-- whiteboard. The blueprint goes in as the planner's reply ('toPrompt'),
-- the sketch as checked source.
reviewJob :: Text -> Plan -> PromptPart -> AgentM 'Verifier (Asked SplitVerdict)
reviewJob stmt p board =
  askTwice (retryMessages (reviewMessages stmt (plBlueprint p) (plLean p) board)) id (pure . parseReview)

-- | The verifier's check of a claim that a statement is false: whether it
-- holds, and the verifier's reason.
falseJob :: Text -> Text -> AgentM 'Verifier (Asked (Bool, Said 'Verifier))
falseJob stmt claim =
  askTwice (retryMessages (falseMessages stmt claim)) id (pure . parseFalse)
