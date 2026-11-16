module Fisher.Search.SchedulerSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Fisher.Search.Lean
import Fisher.Search.Orchestrate (SimTrace (..))
import Fisher.Search.Scheduler
import Fisher.Search.SimHarness

file :: T.Text
file = T.unlines
  [ "import Mathlib"
  , ""
  , "theorem a : A := by"
  , "  sorry"
  , ""
  , "theorem b : B := by"
  , "  sorry"
  , ""
  , "theorem c : C := by"
  , "  sorry"
  ]

-- | A search of the three goals; every attempt on a goal and backend the
-- entries name writes its proof after its virtual seconds, and every
-- attempt launched must be named ('allNamed').
search :: SearchConfig -> [ScriptEntry] -> IO (SearchReport, [Event])
search cfg es = do
  (rep, evs, tr) <- simSearch file cfg (script es)
  allNamed tr
  pure (rep, evs)

-- | 'search' for a scenario that leaves some attempts to the default,
-- which writes @sorry@ at once.
searchPast :: SearchConfig -> [ScriptEntry] -> IO (SearchReport, [Event])
searchPast cfg es = (\(rep, evs, _) -> (rep, evs)) <$> simSearch file cfg (script es)

-- | Every attempt on this goal and backend writes this proof in these seconds.
on :: T.Text -> T.Text -> T.Text -> Double -> [ScriptEntry]
on g b proof secs = always g b secs (proofAttempt proof)

status :: SearchReport -> [(T.Text, GoalStatus)]
status rep = [(g, gsStatus s) | (g, s) <- srGoals rep]

cfg0 :: SearchConfig
cfg0 = defaultSearchConfig {scBackends = ["claude-code", "fisher"], scSlots = 4, scMaxAttempts = 3, scBudget = 6000, scParallelPerGoal = True}

spec :: Spec
spec = describe "the swarm's scheduler" $ do
  it "closes every goal and merges the proofs into one base" $ do
    (rep, _) <- searchPast cfg0 {scParallelPerGoal = False}
      (concat [on g "claude-code" ("exact " <> g <> "_ok") 2 | g <- ["a", "b", "c"]])
    status rep `shouldBe` [("a", Closed), ("b", Closed), ("c", Closed)]
    srClosed rep `shouldBe` 3
    length (openGoals (srBase rep)) `shouldBe` 0
    T.isInfixOf "exact b_ok" (srBase rep) `shouldBe` True
  it "gives every goal a first attempt before any second, spread over backends" $ do
    (rep, es) <- search cfg0 {scSlots = 3, scParallelPerGoal = False}
      (concat [on g b "ok" 4 | g <- ["a", "b", "c"], b <- ["claude-code", "fisher"]])
    srClosed rep `shouldBe` 3
    [(g, b) | Launched _ _ g b <- es] `shouldBe` [("a", "claude-code"), ("b", "fisher"), ("c", "claude-code")]
  it "retries a failed goal on the backend that has not tried it" $ do
    (rep, es) <- searchPast cfg0 {scSlots = 1}
      (on "a" "claude-code" "BAD" 1 <> on "a" "fisher" "ok" 1 <> on "b" "claude-code" "ok" 1 <> on "c" "claude-code" "ok" 1)
    lookup "a" (status rep) `shouldBe` Just Closed
    [b | Launched _ _ "a" b <- es] `shouldBe` ["claude-code", "fisher"]
  it "keeps a proof that uses an open goal waiting, and closes it when that goal closes" $ do
    (rep, es) <- search cfg0 {scSlots = 3, scParallelPerGoal = False}
      (on "a" "claude-code" "exact b" 1 <> on "b" "fisher" "ok" 40 <> on "c" "claude-code" "ok" 1)
    status rep `shouldBe` [("a", Closed), ("b", Closed), ("c", Closed)]
    [g | GoalWaiting _ g _ <- es] `shouldBe` ["a"]
  it "runs a goal on two backends at once and cancels the slower when one closes it" $ do
    (rep, es) <- search cfg0 {scSlots = 6}
      (on "a" "claude-code" "ok" 1 <> on "a" "fisher" "ok" 1000 <> concat [on g b "ok" 1 | g <- ["b", "c"], b <- ["claude-code", "fisher"]])
    lookup "a" (status rep) `shouldBe` Just Closed
    [g | AttemptCancelled _ _ g _ <- es] `shouldSatisfy` elem "a"
    srElapsed rep `shouldBe` 1
  it "gives up after the attempts allowed" $ do
    (rep, es) <- searchPast cfg0 {scSlots = 2, scMaxAttempts = 2, scParallelPerGoal = False}
      (concat ([on g b "BAD" 1 | g <- ["a"], b <- ["claude-code", "fisher"]] <> [on g "claude-code" "ok" 1 | g <- ["b", "c"]]))
    lookup "a" (status rep) `shouldBe` Just GaveUp
    length [() | Launched _ _ "a" _ <- es] `shouldBe` 2
  it "stops at the budget and cancels what runs" $ do
    (rep, es) <- search cfg0 {scBudget = 100}
      (concat [on g b "ok" 12000 | g <- ["a", "b", "c"], b <- ["claude-code", "fisher"]])
    srBudgetSpent rep `shouldBe` True
    srClosed rep `shouldBe` 0
    [() | BudgetSpent _ <- es] `shouldBe` [()]
    srElapsed rep `shouldBe` 100
  it "picks nothing when every goal is decided" $
    pickLaunch cfg0 ["a"] (Map.fromList [("a", GoalState Closed [] [] Nothing Nothing Nothing Nothing Nothing False False Nothing Nothing Nothing [])]) Map.empty `shouldBe` Nothing
  it "delivers an attempt due exactly at the budget, and cuts one due after it" $ do
    let cfg = cfg0 {scBackends = ["claude-code"], scSlots = 1, scParallelPerGoal = False}
        es = on "a" "claude-code" "ok" 5
    (rep, evs) <- search cfg {scBudget = 5} es
    lookup "a" (status rep) `shouldBe` Just Closed
    [(t, i, o) | Finished t i _ _ o _ _ <- evs] `shouldBe` [(5, 1, "closed")]
    [() | AttemptCancelled {} <- evs] `shouldBe` []
    [t | BudgetSpent t <- evs] `shouldBe` [5]
    (cut, cutEvs) <- search cfg {scBudget = 4.9} es
    lookup "a" (status cut) `shouldBe` Just Open
    [arOutcome r | r <- srAttempts cut] `shouldBe` ["cancelled: budget spent"]
    [() | Finished {} <- cutEvs] `shouldBe` []
    [t | BudgetSpent t <- cutEvs] `shouldBe` [4.9]
  it "never delivers a cancelled attempt, though its script entry finishes later" $ do
    -- a closes at 1 and cancels its second attempt, due at 3; b and c
    -- run to 5, and their second attempts are cancelled then, c's first:
    -- of attempts due together the first listed (the newest) is delivered
    let es = on "a" "claude-code" "ok" 1 <> on "a" "fisher" "ok" 3 <> on "b" "fisher" "ok" 5 <> on "c" "claude-code" "ok" 5
               <> on "b" "claude-code" "ok" 10 <> on "c" "fisher" "ok" 10
    (rep, evs, tr) <- simSearch file cfg0 (script es)
    allNamed tr
    [(t, i, g, b) | Launched t i g b <- evs]
      `shouldBe` [(0, 1, "a", "claude-code"), (0, 2, "b", "fisher"), (0, 3, "c", "claude-code"), (0, 4, "a", "fisher"), (1, 5, "b", "claude-code"), (1, 6, "c", "fisher")]
    [(t, i, g) | AttemptCancelled t i g _ <- evs] `shouldBe` [(1, 4, "a"), (5, 6, "c"), (5, 5, "b")]
    length (stCancelled tr) `shouldBe` 3
    [i | Finished _ i _ _ _ _ _ <- evs] `shouldBe` [1, 3, 2]
    stClock tr `shouldBe` 5
    srClosed rep `shouldBe` 3
