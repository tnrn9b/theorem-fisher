module Fisher.Search.SchedulerPolicySpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Learn.Policy (GoalPolicy (..))
import Fisher.Search.Auto
import Fisher.Search.FakeLsp
import Fisher.Search.Scheduler
import Fisher.Search.SimHarness

file :: Text
file = T.unlines
  [ "import Mathlib", ""
  , "theorem a : A := by", "  sorry", ""
  , "theorem b : B := by", "  sorry", ""
  , "theorem c : C := by", "  sorry" ]

-- | A search of the three goals; every attempt on a goal and backend the
-- entries name writes its proof after its virtual seconds, any other
-- writes @sorry@ at once.
search :: SearchConfig -> Maybe AutoConfig -> Fake -> [ScriptEntry] -> IO (SearchReport, [Event])
search cfg ac f es = do
  calls <- newIORef []
  let se = (simEnv file) {simAuto = fmap (\c -> AutoEnv c (fakeLsp calls f) Nothing) ac}
  (rep, evs, _) <- simSearchEnv se cfg (script es)
  pure (rep, evs)

-- | Every attempt on this goal and backend writes this proof in these seconds.
on :: Text -> Text -> Text -> Double -> [ScriptEntry]
on g b proof secs = always g b secs (proofAttempt proof)

cfg0 :: SearchConfig
cfg0 = defaultSearchConfig {scBackends = ["claude-code", "fisher"], scSlots = 4, scMaxAttempts = 3, scBudget = 6000, scParallelPerGoal = True}

gp :: GoalPolicy
gp = GoalPolicy
  { gpBackends = [], gpTiers = [], gpTactics = [], gpAttemptSecs = Nothing, gpExplore = False
  , gpEvidence = object ["backends" .= object ["fisher" .= object ["attempts" .= (10 :: Int)]]] }

withPolicy :: [(Text, GoalPolicy)] -> SearchConfig -> SearchConfig
withPolicy ps c = c {scGoalPolicy = Map.fromList ps}

-- | Every goal closes on its first attempt, whatever the backend.
allOk :: [ScriptEntry]
allOk = concat [on g b "ok" 1 | g <- ["a", "b", "c"], b <- ["claude-code", "fisher"]]

firstLaunch :: Text -> [Event] -> Maybe Text
firstLaunch g es = case [b | Launched _ _ g' b <- es, g' == g] of
  b : _ -> Just b
  [] -> Nothing

policies :: [Event] -> [(Text, Text, Text, Text, Bool)]
policies es = [(g, k, d, a, x) | PolicyApplied _ g k d a _ x <- es]

fresh :: GoalState
fresh = GoalState Open [] [] Nothing Nothing Nothing Nothing Nothing False False Nothing Nothing Nothing []

spec :: Spec
spec = describe "the scheduler with a per-goal policy" $ do
  it "launches a goal first on its policy's first backend, and a goal with no entry as before" $ do
    (rep, es) <- search (withPolicy [("a", gp {gpBackends = ["fisher", "claude-code"]})] cfg0 {scParallelPerGoal = False}) Nothing fake0 allOk
    srClosed rep `shouldBe` 3
    firstLaunch "a" es `shouldBe` Just "fisher"
    firstLaunch "b" es `shouldBe` Just "claude-code"

  it "logs the backends knob once for the goal whose order differs, with its evidence" $ do
    let entries = concat ([on "a" b "BAD" 1 | b <- ["claude-code", "fisher"]] <> [on g "claude-code" "ok" 1 | g <- ["b", "c"]])
    (_, es) <- search (withPolicy [("a", gp {gpBackends = ["fisher", "claude-code"]}), ("b", gp {gpBackends = ["claude-code", "fisher"]})] cfg0 {scSlots = 1}) Nothing fake0 entries
    [b | Launched _ _ "a" b <- es] `shouldBe` ["fisher", "claude-code", "fisher"]
    policies es `shouldBe` [("a", "backends", "claude-code,fisher", "fisher,claude-code", False)]
    [ev | PolicyApplied _ _ _ _ _ ev _ <- es] `shouldBe` [object ["fisher" .= object ["attempts" .= (10 :: Int)]]]

  it "explores: the goal's first attempt runs on the second arm, later ones by the normal rule" $ do
    let entries = concat ([on "a" b "BAD" 1 | b <- ["claude-code", "fisher"]] <> [on g b "ok" 1 | g <- ["b", "c"], b <- ["claude-code", "fisher"]])
    (_, es) <- search (withPolicy [("a", gp {gpBackends = ["fisher", "claude-code"], gpExplore = True})] cfg0 {scSlots = 1, scMaxAttempts = 2}) Nothing fake0 entries
    [b | Launched _ _ "a" b <- es] `shouldBe` ["claude-code", "fisher"]
    [(g, k, x) | (g, k, _, _, x) <- policies es] `shouldBe` [("a", "backends", True)]

  it "puts the policy's order ahead of the spread over backends" $ do
    let p = gp {gpBackends = ["fisher", "claude-code"]}
    (_, es) <- search (withPolicy [(g, p) | g <- ["a", "b", "c"]] cfg0 {scSlots = 3, scParallelPerGoal = False}) Nothing fake0 allOk
    [(g, b) | Launched _ _ g b <- es] `shouldBe` [("a", "fisher"), ("b", "fisher"), ("c", "fisher")]
    length (policies es) `shouldBe` 3

  it "a policy equal to the defaults changes nothing and logs nothing" $ do
    let p = gp {gpBackends = ["claude-code", "fisher"]}
    (_, es) <- search (withPolicy [(g, p) | g <- ["a", "b", "c"]] cfg0 {scSlots = 3, scParallelPerGoal = False}) Nothing fake0 allOk
    [(g, b) | Launched _ _ g b <- es] `shouldBe` [("a", "claude-code"), ("b", "fisher"), ("c", "claude-code")]
    policies es `shouldBe` []

  it "ignores a backend that is not configured" $ do
    (_, es) <- search (withPolicy [("a", gp {gpBackends = ["ghost", "fisher", "claude-code"]})] cfg0 {scParallelPerGoal = False}) Nothing fake0 allOk
    firstLaunch "a" es `shouldBe` Just "fisher"
    policies es `shouldBe` [("a", "backends", "claude-code,fisher", "fisher,claude-code", False)]
    [b | Launched _ _ _ b <- es] `shouldSatisfy` notElem "ghost"

  it "an entry with only unconfigured backends falls back to the configured ones" $ do
    (_, es) <- search (withPolicy [("a", gp {gpBackends = ["ghost"]})] cfg0 {scParallelPerGoal = False}) Nothing fake0 allOk
    firstLaunch "a" es `shouldBe` Just "claude-code"
    policies es `shouldBe` []

  it "chooses the tier from the policy's order, and logs the tiers knob" $ do
    let c = withPolicy [("a", gp {gpTiers = ["m2", "m1"]})] cfg0 {scTiers = ["m1", "m2"], scSlots = 1}
    (_, es) <- search c Nothing fake0 allOk
    [(k, m) | TierChosen _ _ "a" k m <- es] `shouldBe` [(1, "m2")]
    [(k, m) | TierChosen _ _ "b" k m <- es] `shouldBe` [(0, "m1")]
    [(g, k, d, a) | (g, k, d, a, _) <- policies es] `shouldBe` [("a", "tiers", "m1,m2", "m2,m1")]

  it "logs the tiers knob as no exploration, even for a goal that explores its backends" $ do
    let c = withPolicy [("a", gp {gpTiers = ["m2", "m1"], gpBackends = ["fisher", "claude-code"], gpExplore = True})] cfg0 {scTiers = ["m1", "m2"], scSlots = 1}
    (_, es) <- search c Nothing fake0 allOk
    [(k, x) | ("a", k, _, _, x) <- policies es] `shouldBe` [("backends", True), ("tiers", False)]

  it "logs a learned attempt budget once per goal, the configured one as the default" $ do
    let c = withPolicy [("a", gp {gpAttemptSecs = Just 600}), ("b", gp {gpAttemptSecs = Just 1200}), ("c", gp {gpAttemptSecs = Just 1500})]
              cfg0 {scSlots = 1, scAttemptSecs = Just 1200}
        entries = concat ([on "a" b "BAD" 1 | b <- ["claude-code", "fisher"]] <> [on g b "ok" 1 | g <- ["b", "c"], b <- ["claude-code", "fisher"]])
    (_, es) <- search c Nothing fake0 entries
    length [() | Launched _ _ "a" _ <- es] `shouldBe` 3
    policies es `shouldBe` [("a", "attempt_secs", "1200", "600", False)]
    -- with no configured budget known, nothing is logged
    (_, es2) <- search c {scAttemptSecs = Nothing} Nothing fake0 entries
    policies es2 `shouldBe` []

  it "climbs the policy's tier order on later attempts" $ do
    let c = withPolicy [("a", gp {gpTiers = ["m2", "m1"]})] cfg0 {scTiers = ["m1", "m2"], scSlots = 1, scBackends = ["fisher"], scMaxAttempts = 2}
        entries = on "a" "fisher" "BAD" 1 <> concat [on g "fisher" "ok" 1 | g <- ["b", "c"]]
    (_, es) <- search c Nothing fake0 entries
    [m | TierChosen _ _ "a" _ m <- es] `shouldBe` ["m2", "m1"]
    length [() | (_, "tiers", _, _, _) <- policies es] `shouldBe` 1

  it "ignores a tier that is not configured" $ do
    let c = withPolicy [("a", gp {gpTiers = ["m9", "m2"]})] cfg0 {scTiers = ["m1", "m2"], scSlots = 1}
    (_, es) <- search c Nothing fake0 allOk
    [m | TierChosen _ _ "a" _ m <- es] `shouldBe` ["m2"]
    [a | (_, "tiers", _, a, _) <- policies es] `shouldBe` ["m2,m1"]

  it "runs the pre-pass with the goal's tactic order, within the configured tactics" $ do
    let ac = defaultAutoConfig {acTactics = ["rfl", "simp_all"]}
        c = withPolicy [("a", gp {gpTactics = ["ghost", "simp_all", "rfl"]})] cfg0 {scSlots = 1}
        f = fake0 {fClose = \_ _ -> Just Nothing}
    (rep, es) <- search c (Just ac) f allOk
    srClosed rep `shouldBe` 3
    let snips g = [s | AutoClosed _ g' s _ <- es, g' == g]
    map (T.isInfixOf "simp_all") (snips "a") `shouldBe` [True]
    map (T.isInfixOf "rfl") (snips "b") `shouldBe` [True]
    policies es `shouldBe` [("a", "tactics", "rfl,simp_all", "simp_all,rfl", False)]

  it "logs no policy event with scPolicyLogged off" $ do
    let c = (withPolicy [("a", gp {gpBackends = ["fisher", "claude-code"]})] cfg0 {scParallelPerGoal = False}) {scPolicyLogged = False}
    (_, es) <- search c Nothing fake0 allOk
    firstLaunch "a" es `shouldBe` Just "fisher"
    policies es `shouldBe` []

  it "pickLaunch reads the map from the config" $ do
    let goals = Map.fromList [("a", fresh)]
    pickLaunch cfg0 ["a"] goals Map.empty `shouldBe` Just ("a", "claude-code")
    pickLaunch (withPolicy [("a", gp {gpBackends = ["fisher", "claude-code"]})] cfg0) ["a"] goals Map.empty `shouldBe` Just ("a", "fisher")
    pickLaunch (withPolicy [("a", gp {gpBackends = ["fisher", "claude-code"], gpExplore = True})] cfg0) ["a"] goals Map.empty `shouldBe` Just ("a", "claude-code")
    defaultSearchConfig `shouldSatisfy` (Map.null . scGoalPolicy)
    scPolicyLogged defaultSearchConfig `shouldBe` True
    scAttemptSecs defaultSearchConfig `shouldBe` Nothing
    gpEvidence gp `shouldNotBe` Null
