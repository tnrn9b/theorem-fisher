-- | The simulator's event stream is the real orchestrator's: one scenario
-- run through both, the events compared with their times set to 0. Its
-- real run's sleeps total well under a second.
module Fisher.Search.SchedulerIdentitySpec (spec) where

import Test.Hspec

import Fisher.Search.Orchestrate
import Fisher.Search.Scheduler
import Fisher.Search.SimHarness

spec :: Spec
spec = describe "the simulator against the real orchestrator" $
  it "gives the same events, times aside, for the same script" $ do
    let sc = emptyScript {scAttempts = identityEntries}
    (_, simEvs, tr) <- simSearch identityFile identityConfig sc
    realEvs <- realSearch pure identityFile identityConfig sc
    map zeroTimes realEvs `shouldBe` map zeroTimes simEvs
    -- the scenario is the one meant: a closes, b fails, then the budget
    [(t, i, g, b) | Launched t i g b <- simEvs] `shouldBe` [(0, 1, "a", "x"), (0, 2, "b", "y"), (4, 3, "b", "x")]
    [(t, g) | GoalClosed t g _ <- simEvs] `shouldBe` [(2, "a")]
    [t | BudgetSpent t <- simEvs] `shouldBe` [8]
    stClock tr `shouldBe` 8
