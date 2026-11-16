-- | The lemma-graph search with retrieval and the library: the facts in
-- workers' briefs and the planner's messages, the harvest of proved
-- lemmas and notes, the report, and today's run when both are off.
module Fisher.Search.GraphRetrievalSpec (spec) where

import Control.Exception (AsyncException (UserInterrupt), fromException)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Search.Graph
import Fisher.Search.GraphHarness
import Fisher.Search.GraphRun
import Fisher.Search.Library
import Fisher.Search.Report
import Fisher.Search.Retrieval (RetrievalEnv)
import Fisher.Search.RetrievalFixture
import Fisher.Search.Scheduler (Event (..))
import Fisher.Search.Scripted (accept, plan)

root1 :: Text
root1 = T.unlines ["import Mathlib", "import P.Defs", "", "theorem r1 : P.bump 0 = 1 := by", "  sorry"]

earlierNote :: Note
earlierNote = Note "unfold bump before simp" "F.lean" "r0" "run0"

withRetrieval :: FilePath -> IO (Maybe RetrievalEnv)
withRetrieval root = do
  project root root1
  Just <$> retrievalIn (Just ([bumpLemma], [earlierNote])) root

field :: Text -> Value -> Value
field k = \case
  Object o -> fromMaybe Null (KM.lookup (K.fromText k) o)
  _ -> Null

keysOf :: Value -> [Text]
keysOf = \case
  Object o -> sort (map K.toText (KM.keys o))
  _ -> []

splitRun :: (FilePath -> IO (Maybe RetrievalEnv)) -> IO GOut
splitRun mk =
  runGRet mk keyLean Nothing cfg root1
    [("r1", [bad {rFinal = Just "NOTE: simp loops on bump"}]), ("l1", [ok])]
    [say Planner (plan [("l1", "P.bump 0 = 0 + 1")] "theorem r1 : P.bump 0 = 1" "exact l1"), say Verifier accept]

spec :: Spec
spec = describe "retrieval in the lemma graph" $ do
  it "gives workers and the planner the facts and the library" $ do
    o <- splitRun withRetrieval
    statusOf o "r1" `shouldBe` NProved
    case [b | ("r1", b) <- oBriefs o] of
      (b : _) -> do
        b `shouldSatisfy` T.isInfixOf "Relevant project facts:\n- P.bump : def bump (n : Nat) : Nat (P/Defs.lean:3)\n"
        b `shouldSatisfy` T.isInfixOf "Lemmas proved in earlier runs"
        b `shouldSatisfy` T.isInfixOf "- r0: unfold bump before simp"
      [] -> expectationFailure "no attempt on r1"
    [b | ("l1", b) <- oBriefs o] `shouldSatisfy` all (T.isInfixOf "- P.bump : def bump")
    case [ms | (Planner, ms) <- oAsks o] of
      (ms : _) -> snd (ms !! 1) `shouldSatisfy` T.isInfixOf "Relevant project facts:\n- P.bump : def bump"
      [] -> expectationFailure "no planner call"
    let r1 = nId (nodeBy o "r1")
        l1 = nId (nodeBy o "l1")
    -- one retrieval per node, shared by its workers and its planner
    [(i, c) | NodeFacts _ i _ _ c <- oEvents o] `shouldBe` [(r1, False), (l1, False)]
    [(i, ls, n) | NodeLibraryOffered _ i ls n _ <- oEvents o] `shouldBe` [(r1, ["P.bump_eq"], 1), (l1, ["P.bump_eq"], 1)]

  it "saves the proved lemmas and the notes at the end, logged" $ do
    o <- splitRun withRetrieval
    [(l, n) | GraphRunEvent (LibrarySaved _ l n) <- oEvents o] `shouldBe` [(1, 1)]
    map lmName (fst (oLibrary o)) `shouldBe` ["l1"]
    map lmUses (fst (oLibrary o)) `shouldBe` [["P.bump"]]
    [(ntGoal n, ntText n) | n <- snd (oLibrary o)] `shouldBe` [("r1", "simp loops on bump")]

  it "gives facts, offered and used per node in report.json, and the totals" $ do
    o <- splitRun withRetrieval
    let j = graphReportJson [] (oRep o)
    field "offered_total" j `shouldBe` toJSON (2 :: Int)
    field "used_total" j `shouldBe` toJSON (0 :: Int)
    field "saved_lemmas" j `shouldBe` toJSON (1 :: Int)
    field "saved_notes" j `shouldBe` toJSON (1 :: Int)
    case field "retrieval" j of
      Array xs -> map keysOf (foldr (:) [] xs) `shouldBe` replicate 2 (sort ["node", "name", "facts", "offered", "used"])
      v -> expectationFailure ("retrieval: " <> show v)

  it "saves the proved lemmas when the run is interrupted, then ends with the interrupt" $ do
    let two = root1 <> T.unlines ["", "theorem r2 : P.bump 1 = 2 := by", "  sorry"]
        mk root = project root two >> Just <$> retrievalIn (Just ([bumpLemma], [earlierNote])) root
    (r, es, lib) <- runGInterrupted (\case NodeProved {} -> True; _ -> False) mk cfg two
      [("r1", [bad {rFinal = Just "NOTE: simp loops on bump"}]), ("l1", [ok]), ("r2", [ok {rClock = 60}, ok {rClock = 60}])]
      [say Planner (plan [("l1", "P.bump 0 = 0 + 1")] "theorem r1 : P.bump 0 = 1" "exact l1"), say Verifier accept]
    either (\e -> fromException e == Just UserInterrupt) (const False) r `shouldBe` True
    [l | GraphRunEvent (LibrarySaved _ l _) <- es] `shouldBe` [1]
    map lmName (fst lib) `shouldBe` ["l1"]

  it "with both off, no retrieval events and today's report" $ do
    o <- splitRun (\_ -> pure Nothing)
    [() | NodeFacts {} <- oEvents o] `shouldBe` []
    [() | NodeLibraryOffered {} <- oEvents o] `shouldBe` []
    [() | GraphRunEvent _ <- oEvents o] `shouldBe` []
    keysOf (graphReportJson [] (oRep o)) `shouldBe`
      sort ["elapsed_s", "goals_total", "goals_closed", "nodes_total", "nodes_proved", "splits_total", "stopped", "dollars", "tree", "attempts"]
    [b | ("r1", b) <- oBriefs o] `shouldSatisfy` all (not . T.isInfixOf "Relevant project facts")
