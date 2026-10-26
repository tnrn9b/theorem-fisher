module Fisher.Search.OutcomeSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Controller (Stop (..), StopReason (..))
import Fisher.Search.Outcome (outcomeClass, outcomeClassText)
import Fisher.Search.Scheduler (AttemptRecord (..), plainRecord)

stats :: Maybe Int -> ChildStats
stats e = ChildStats e 12 (Just 4) Nothing Nothing Nothing Nothing Map.empty

rec' :: Text -> Maybe ChildStats -> AttemptRecord
rec' o = plainRecord 1 "t" "fisher" o 0 10

spec :: Spec
spec = do
  it "classes seven attempts as closed, failed, timeout, stalled, cancelled, budget and unknown" $ do
    let stopped = (rec' "no file: stopped: turns" (Just (stats Nothing))) {arStopped = Just (Stop StallTurns 8 0.4)}
    map outcomeClass
      [ rec' "closed (helpers: a, b)" (Just (stats (Just 0)))
      , rec' "rejected: type mismatch" (Just (stats (Just 0)))
      , rec' "no file: killed at the time limit" (Just (stats Nothing))
      , stopped
      , rec' "cancelled: the goal closed" Nothing
      , rec' "cancelled: budget spent" Nothing
      , rec' "something new" (Just (stats (Just 0))) ]
      `shouldBe` ["closed", "failed", "timeout", "stalled", "cancelled", "budget", "unknown"]

  it "a close wins over a stop or a kill at the limit" $ do
    outcomeClass ((rec' "closed by repair" (Just (stats Nothing))) {arStopped = Just (Stop StallDollars 3 1.6)}) `shouldBe` "closed"

  it "a disproved goal's cancellation is cancelled, not budget" $
    outcomeClass (rec' "cancelled: the goal was disproved" Nothing) `shouldBe` "cancelled"

  it "the lemma graph's cancellations at its time budget or its dollar cap are budget" $
    map (\o -> outcomeClass (rec' o Nothing)) ["cancelled: time", "cancelled: dollars"] `shouldBe` ["budget", "budget"]

  it "an interrupted search's cancellation is cancelled" $
    outcomeClass (rec' "cancelled: the search was interrupted" Nothing) `shouldBe` "cancelled"

  it "every failure text of the scheduler is failed, waiting included" $
    map (\o -> outcomeClass (rec' o (Just (stats (Just 1)))))
      [ "not spliced: no such goal", "no file: exit 1", "no file: plan failed: x", "error: boom", "model error: refused"
      , "discarded: sorry left", "waiting on open goals", "waiting after repair" ]
      `shouldBe` replicate 8 "failed"

  it "a stop wins over a kill at the limit, which wins over a rejection" $ do
    outcomeClass ((rec' "rejected: x" (Just (stats Nothing))) {arStopped = Just (Stop StallTurns 8 0)}) `shouldBe` "stalled"
    outcomeClass (rec' "rejected: x" (Just (stats Nothing))) `shouldBe` "timeout"

  it "classes a report's outcome text, child exit and stop as the record would be" $
    [ outcomeClassText "closed (helpers: a)" (Just (Just 0)) False
    , outcomeClassText "rejected: type mismatch" (Just (Just 0)) False
    , outcomeClassText "no file: killed at the time limit" (Just Nothing) False
    , outcomeClassText "no file: stopped: turns" (Just Nothing) True
    , outcomeClassText "rejected: x" Nothing False
    , outcomeClassText "cancelled: budget spent" Nothing False ]
      `shouldBe` ["closed", "failed", "timeout", "stalled", "failed", "budget"]
