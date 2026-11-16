module Fisher.Search.SchedulerPolicySpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Learn.Policy (GoalPolicy (..))
import Fisher.Search.Auto
import Fisher.Search.Check (AxiomPolicy (..), standardAxioms)
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.FakeLsp
import Fisher.Search.Lean
import Fisher.Search.Orchestrate (realOrchestrator)
import Fisher.Search.Scheduler

file :: Text
file = T.unlines
  [ "import Mathlib", ""
  , "theorem a : A := by", "  sorry", ""
  , "theorem b : B := by", "  sorry", ""
  , "theorem c : C := by", "  sorry" ]

-- | Attempts per (goal, backend): the proof to write and a delay in ms;
-- past the script an attempt writes @sorry@.
type Script = Map.Map (Text, Text) (Text, Int)

search :: SearchConfig -> Maybe AutoConfig -> Fake -> Script -> IO (SearchReport, [Event])
search cfg ac f script = withSystemTempDirectory "policy" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  events <- newIORef []
  calls <- newIORef []
  let run sp = do
        let (proof, delay) = fromMaybe ("sorry", 0) (Map.lookup (asGoal sp, asBackend sp) script)
        threadDelay (delay * 1000)
        let ls = T.splitOn "\n" (asStart sp)
            d = fromMaybe (error "no goal") (findDecl (asGoal sp) (declarations (asStart sp)))
            out = T.intercalate "\n" (take (dStmtEnd d + 1) ls <> ["  " <> proof] <> drop (dEnd d) ls)
        pure (plainResult (Just out) Nothing "")
  orch <- realOrchestrator run (realToFrac <$> getPOSIXTime) (\e -> modifyIORef events (e :)) (\_ -> pure Nothing)
  let env = SearchEnv
        { seOrch = orch, seLean = fakeLean, sePolicy = AxiomPolicy standardAxioms False, seRoot = root, seRel = "F.lean"
        , seBaseChanged = \_ -> pure ()
        , seAuto = fmap (\c -> AutoEnv c (fakeLsp calls f) Nothing) ac, seRetrieval = Nothing }
  r <- runSearch cfg env file
  es <- reverse <$> readIORef events
  either (fail . T.unpack) (\rep -> pure (rep, es)) r

cfg0 :: SearchConfig
cfg0 = defaultSearchConfig {scBackends = ["claude-code", "fisher"], scSlots = 4, scMaxAttempts = 3, scBudget = 30, scParallelPerGoal = True}

gp :: GoalPolicy
gp = GoalPolicy
  { gpBackends = [], gpTiers = [], gpTactics = [], gpAttemptSecs = Nothing, gpExplore = False
  , gpEvidence = object ["backends" .= object ["fisher" .= object ["attempts" .= (10 :: Int)]]] }

withPolicy :: [(Text, GoalPolicy)] -> SearchConfig -> SearchConfig
withPolicy ps c = c {scGoalPolicy = Map.fromList ps}

-- | Every goal closes on its first attempt, whatever the backend.
allOk :: Script
allOk = Map.fromList [((g, b), ("ok", 5)) | g <- ["a", "b", "c"], b <- ["claude-code", "fisher"]]

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
    let script = Map.fromList ([(("a", b), ("BAD", 5)) | b <- ["claude-code", "fisher"]] <> [((g, "claude-code"), ("ok", 5)) | g <- ["b", "c"]])
    (_, es) <- search (withPolicy [("a", gp {gpBackends = ["fisher", "claude-code"]}), ("b", gp {gpBackends = ["claude-code", "fisher"]})] cfg0 {scSlots = 1}) Nothing fake0 script
    [b | Launched _ _ "a" b <- es] `shouldBe` ["fisher", "claude-code", "fisher"]
    policies es `shouldBe` [("a", "backends", "claude-code,fisher", "fisher,claude-code", False)]
    [ev | PolicyApplied _ _ _ _ _ ev _ <- es] `shouldBe` [object ["fisher" .= object ["attempts" .= (10 :: Int)]]]

  it "explores: the goal's first attempt runs on the second arm, later ones by the normal rule" $ do
    let script = Map.fromList ([(("a", b), ("BAD", 5)) | b <- ["claude-code", "fisher"]] <> [((g, b), ("ok", 5)) | g <- ["b", "c"], b <- ["claude-code", "fisher"]])
    (_, es) <- search (withPolicy [("a", gp {gpBackends = ["fisher", "claude-code"], gpExplore = True})] cfg0 {scSlots = 1, scMaxAttempts = 2}) Nothing fake0 script
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
        script = Map.fromList ([(("a", b), ("BAD", 5)) | b <- ["claude-code", "fisher"]] <> [((g, b), ("ok", 5)) | g <- ["b", "c"], b <- ["claude-code", "fisher"]])
    (_, es) <- search c Nothing fake0 script
    length [() | Launched _ _ "a" _ <- es] `shouldBe` 3
    policies es `shouldBe` [("a", "attempt_secs", "1200", "600", False)]
    -- with no configured budget known, nothing is logged
    (_, es2) <- search c {scAttemptSecs = Nothing} Nothing fake0 script
    policies es2 `shouldBe` []

  it "climbs the policy's tier order on later attempts" $ do
    let c = withPolicy [("a", gp {gpTiers = ["m2", "m1"]})] cfg0 {scTiers = ["m1", "m2"], scSlots = 1, scBackends = ["fisher"], scMaxAttempts = 2}
        script = Map.fromList [(("a", "fisher"), ("BAD", 5))] <> Map.fromList [((g, "fisher"), ("ok", 5)) | g <- ["b", "c"]]
    (_, es) <- search c Nothing fake0 script
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
