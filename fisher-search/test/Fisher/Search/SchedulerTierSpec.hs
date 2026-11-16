module Fisher.Search.SchedulerTierSpec (spec) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (newIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Search.Auto
import Fisher.Search.Controller (ModelFailure (..))
import Fisher.Search.FakeLsp
import Fisher.Search.Orchestrate (Script (..), SimTrace (..), emptyScript)
import Fisher.Search.Report
import Fisher.Search.Scheduler
import Fisher.Search.SimHarness

one :: Text
one = T.unlines ["import Mathlib", "", "theorem t : T := by", "  sorry"]

three :: Text
three = T.unlines
  [ "import Mathlib", ""
  , "theorem a : A := by", "  sorry", ""
  , "theorem b : B := by", "  sorry", ""
  , "theorem c : C := by", "  sorry" ]

-- | One result: the proof, the model failure, the virtual seconds.
type Res = (Text, Maybe ModelFailure, Double)

data Run = Run
  { rReport :: SearchReport
  , rEvents :: [Event]
  , rSpecs :: [AttemptSpec]
  }

-- | A goal's results in launch order on the one backend @x@.
results :: Text -> [Res] -> [ScriptEntry]
results g = resultsOn g . map ((,) "x")

-- | A goal's results in launch order, each on its backend.
resultsOn :: Text -> [(Text, Res)] -> [ScriptEntry]
resultsOn g xs = inOrder g [(b, secs, resultOf p f) | (b, (p, f, secs)) <- xs]

-- | An attempt that writes this proof and fails (or not) this way.
resultOf :: Text -> Maybe ModelFailure -> AttemptSpec -> Either Text AttemptResult
resultOf p failure sp = Right (plainResult (Just (spliceProof p sp)) Nothing "exit 1") {arFailure = failure}

-- | A search; every attempt launched must be named ('allNamed').
search :: SearchConfig -> Maybe AutoConfig -> Fake -> Text -> [ScriptEntry] -> IO Run
search = searchF (const Nothing)

-- | 'search' with a model failure by attempt, ahead of the script's.
searchF :: (AttemptSpec -> Maybe ModelFailure) -> SearchConfig -> Maybe AutoConfig -> Fake -> Text -> [ScriptEntry] -> IO Run
searchF = searchWith True

-- | 'search' for a scenario that runs past its entries, where an attempt
-- writes @BAD@ at once.
searchPast :: SearchConfig -> Maybe AutoConfig -> Fake -> Text -> [ScriptEntry] -> IO Run
searchPast = searchWith False (const Nothing)

-- | 'searchF'; with True every attempt launched must be named.
searchWith :: Bool -> (AttemptSpec -> Maybe ModelFailure) -> SearchConfig -> Maybe AutoConfig -> Fake -> Text -> [ScriptEntry] -> IO Run
searchWith named failOf cfg ac f file es = do
  calls <- newIORef []
  let se = (simEnv file) {simAuto = fmap (\c -> AutoEnv c (fakeLsp calls f) Nothing) ac}
      failing g sp = fmap (\r -> r {arFailure = failOf sp <|> arFailure r}) (g sp)
      sc = emptyScript
        { scAttempts = [(k, secs, failing g) | (k, secs, g) <- es]
        , scDefault = Just (failing (resultOf "exact BAD" Nothing)) }
  (rep, evs, tr) <- simSearchEnv se cfg sc
  if named then allNamed tr else pure ()
  pure (Run rep evs (stSpawned tr))

tiers :: [Text]
tiers = ["example/model-a", "example/model-b"]

-- | One backend, one slot, three attempts, the controller on.
cfgT :: SearchConfig
cfgT = defaultSearchConfig
  { scBackends = ["x"], scSlots = 1, scMaxAttempts = 3, scBudget = 6000, scParallelPerGoal = False
  , scTiers = tiers, scControl = True }

models :: Run -> [Maybe Text]
models = map asModel . rSpecs

statusOf :: Text -> Run -> GoalStatus
statusOf g r = maybe (error "no goal") gsStatus (lookup g (srGoals (rReport r)))

triedOf :: Text -> Run -> [Text]
triedOf g r = maybe (error "no goal") gsTried (lookup g (srGoals (rReport r)))

eventName :: Value -> Text
eventName = \case
  Object o | Just (String n) <- KM.lookup "event" o -> n
  _ -> ""

keysOf :: Value -> [Text]
keysOf = \case
  Object o -> sort (map K.toText (KM.keys o))
  _ -> []

spec :: Spec
spec = describe "model tiers and model failures in flat search" $ do
  it "steps a failing goal up the tiers by its counted attempts" $ do
    r <- searchPast cfgT Nothing fake0 one []
    models r `shouldBe` map Just ["example/model-a", "example/model-b", "example/model-b"]
    map asTier (rSpecs r) `shouldBe` [Just 0, Just 1, Just 1]
    [(i, g, k, m) | TierChosen _ i g k m <- rEvents r]
      `shouldBe` [(1, "t", 0, "example/model-a"), (2, "t", 1, "example/model-b"), (3, "t", 1, "example/model-b")]
    [(g, a, b) | Escalated _ g a b <- rEvents r] `shouldBe` [("t", 0, 1)]
    statusOf "t" r `shouldBe` GaveUp

  it "uses only the first tier for a goal closed on its first attempt" $ do
    r <- search cfgT Nothing fake0 one (results "t" [("ok", Nothing, 1)])
    models r `shouldBe` [Just "example/model-a"]
    [() | Escalated {} <- rEvents r] `shouldBe` []
    statusOf "t" r `shouldBe` Closed

  it "skips an unavailable tier, records a model error, and does not count the attempt" $ do
    r <- searchPast cfgT Nothing fake0 one (results "t" [("exact BAD", Just Unavailable, 1)])
    [(m, why) | TierSkipped _ m why <- rEvents r] `shouldBe` [("example/model-a", "unavailable model")]
    take 1 (map arOutcome (srAttempts (rReport r))) `shouldBe` ["model error: unavailable model"]
    models r `shouldBe` map Just ["example/model-a", "example/model-b", "example/model-b", "example/model-b"]
    length (triedOf "t" r) `shouldBe` 3
    statusOf "t" r `shouldBe` GaveUp

  it "sends a model error to neither Repair nor the kernel" $ do
    let closes = fake0 {fClose = \_ p -> if "intro x" `T.isInfixOf` p then Just Nothing else Nothing}
    r <- search cfgT {scMaxAttempts = 1} (Just defaultAutoConfig {acTactics = ["rfl"], acDisprove = False}) closes one
      (results "t" [("intro x\n  exact BAD", Just Unavailable, 1), ("ok", Nothing, 1)])
    [() | Repaired {} <- rEvents r] `shouldBe` []
    map arOutcome (srAttempts (rReport r)) `shouldBe` ["model error: unavailable model", "closed"]
    statusOf "t" r `shouldBe` Closed

  it "skips a tier after three rate limits in a row" $ do
    let rl = ("exact BAD", Just RateLimited, 1)
    r <- searchPast cfgT Nothing fake0 one (results "t" [rl, rl, rl])
    take 4 (models r) `shouldBe` map Just ["example/model-a", "example/model-a", "example/model-a", "example/model-b"]
    [(m, why) | TierSkipped _ m why <- rEvents r] `shouldBe` [("example/model-a", "rate limited")]
    length (triedOf "t" r) `shouldBe` 3

  it "a success between rate limits resets the count" $ do
    let rl g = (g, Just RateLimited, 1)
    r <- search cfgT {scSlots = 1} Nothing fake0 three
      (results "a" [rl "exact BAD", rl "exact BAD", ("ok", Nothing, 1)] <> results "b" [rl "exact BAD", ("ok", Nothing, 1)] <> results "c" [("ok", Nothing, 1)])
    [() | TierSkipped {} <- rEvents r] `shouldBe` []

  it "counts model errors past the third on a goal as failed attempts" $ do
    let nt = ("exact BAD", Just NoTools, 1)
    r <- search cfgT {scTiers = ["example/model-a"]} Nothing fake0 one (results "t" (replicate 6 nt))
    -- the only tier is skipped at once, so the goal gives up
    [(m, why) | TierSkipped _ m why <- rEvents r] `shouldBe` [("example/model-a", "no tool support")]
    [g | GoalGaveUp _ g <- rEvents r] `shouldBe` ["t"]
    length (rSpecs r) `shouldBe` 1
    -- with no tiers model errors are still kept out of the count, three
    -- times; the next three count as failed attempts
    r2 <- search cfgT {scTiers = []} Nothing fake0 one (results "t" (replicate 6 nt))
    length (rSpecs r2) `shouldBe` 6
    map arOutcome (srAttempts (rReport r2)) `shouldBe` replicate 6 "model error: no tool support"
    length (triedOf "t" r2) `shouldBe` 3
    [() | TierChosen {} <- rEvents r2] `shouldBe` []
    statusOf "t" r2 `shouldBe` GaveUp

  it "gives up on every open goal when every tier is skipped" $ do
    r <- search cfgT Nothing fake0 three
      (results "a" [("exact BAD", Just Unavailable, 1), ("exact BAD", Just NoTools, 1)])
    [(m, why) | TierSkipped _ m why <- rEvents r] `shouldBe` [("example/model-a", "unavailable model"), ("example/model-b", "no tool support")]
    sort [g | GoalGaveUp _ g <- rEvents r] `shouldBe` ["a", "b", "c"]
    map (`statusOf` r) ["a", "b", "c"] `shouldBe` [GaveUp, GaveUp, GaveUp]
    length (rSpecs r) `shouldBe` 2

  it "gives parallel attempts on one goal the goal's current tier" $ do
    r <- searchPast cfgT {scBackends = ["x", "y"], scSlots = 2, scParallelPerGoal = True} Nothing fake0 one
      (resultsOn "t" [("x", ("exact BAD", Nothing, 20)), ("y", ("exact BAD", Nothing, 20))])
    take 2 (models r) `shouldBe` map Just ["example/model-a", "example/model-a"]
    drop 2 (models r) `shouldBe` [Just "example/model-b"]
    [(g, a, b) | Escalated _ g a b <- rEvents r] `shouldBe` [("t", 0, 1)]

  it "a mixed backend list: tiers for the native backend only" $ do
    -- claude-code outside the tiers, every one of its attempts unavailable:
    -- no tier, no tier event, and no tier skipped for fisher
    let cfg = cfgT {scBackends = ["claude-code", "fisher"], scUntiered = ["claude-code"]}
        cc sp = if asBackend sp == "claude-code" then Just Unavailable else Nothing
    r <- searchWith False cc cfg Nothing fake0 one []
    let byBackend b = [sp | sp <- rSpecs r, asBackend sp == b]
    map asBackend (rSpecs r) `shouldSatisfy` elem "claude-code"
    [(asModel sp, asTier sp) | sp <- byBackend "claude-code"] `shouldSatisfy` all (== (Nothing, Nothing))
    take 1 (map asTier (byBackend "fisher")) `shouldBe` [Just 0]
    map asTier (byBackend "fisher") `shouldSatisfy` all (`elem` [Just 0, Just 1])
    map asModel (byBackend "fisher") `shouldBe` map (fmap (tiers !!) . asTier) (byBackend "fisher")
    [() | TierSkipped {} <- rEvents r] `shouldBe` []
    sort [i | TierChosen _ i _ _ _ <- rEvents r] `shouldBe` sort (map asId (byBackend "fisher"))

  it "a mixed backend list with every tier skipped: the backend outside the tiers still runs" $ do
    let cfg = cfgT {scBackends = ["claude-code", "fisher"], scUntiered = ["claude-code"]}
        nativeOut sp = if asBackend sp == "fisher" then Just Unavailable else Nothing
        bad = ("exact BAD", Nothing, 1)
    -- launched: claude-code, then fisher at tier 0 and at tier 1 (both
    -- unavailable), then only claude-code
    r <- searchF nativeOut cfg Nothing fake0 one
      (resultsOn "t" [("claude-code", bad), ("fisher", bad), ("fisher", bad), ("claude-code", bad), ("claude-code", bad), ("claude-code", ("ok", Nothing, 1))])
    sort [m | TierSkipped _ m _ <- rEvents r] `shouldBe` ["example/model-a", "example/model-b"]
    [() | GoalGaveUp {} <- rEvents r, statusOf "t" r /= GaveUp] `shouldBe` []
    -- after both skips only claude-code launches, with no tier
    length [() | sp <- rSpecs r, asBackend sp == "fisher"] `shouldBe` 2
    [asTier sp | sp <- rSpecs r, asBackend sp == "claude-code"] `shouldSatisfy` all (== Nothing)
    length [() | sp <- rSpecs r, asBackend sp == "claude-code"] `shouldSatisfy` (>= 2)

  it "with no tiers and the controller off, runs today's search exactly" $ do
    let cfg = defaultSearchConfig {scBackends = ["claude-code", "fisher"], scSlots = 1, scMaxAttempts = 3, scBudget = 6000}
        ok = ("ok", Nothing, 1)
    r <- search cfg Nothing fake0 three
      (resultsOn "a" [("claude-code", ("exact BAD", Nothing, 1)), ("fisher", ok)] <> resultsOn "b" [("fisher", ok)] <> resultsOn "c" [("claude-code", ok)])
    map asModel (rSpecs r) `shouldBe` replicate 4 Nothing
    map asTier (rSpecs r) `shouldBe` replicate 4 Nothing
    let js = map eventJson (rEvents r)
    map eventName js `shouldBe`
      ["launched", "finished", "launched", "finished", "closed", "launched", "finished", "closed", "launched", "finished", "closed"]
    [keysOf j | j <- js, eventName j == "finished"]
      `shouldBe` replicate 4 (sort ["event", "t", "attempt", "goal", "backend", "outcome", "stats"])
    [d | Finished _ _ _ _ _ _ d <- rEvents r] `shouldBe` replicate 4 Nothing

  it "names the new events in JSON and adds dollars to finished only when known" $ do
    keysOf (eventJson (TierChosen 1 2 "t" 0 "example/model-a")) `shouldBe` sort ["event", "t", "attempt", "goal", "tier", "model"]
    keysOf (eventJson (AttemptStopped 1 2 "t" "turns" 8 0.5)) `shouldBe` sort ["event", "t", "attempt", "goal", "reason", "turns", "dollars"]
    keysOf (eventJson (Escalated 1 "t" 0 1)) `shouldBe` sort ["event", "t", "goal", "from", "to"]
    keysOf (eventJson (TierSkipped 1 "example/model-a" "rate limited")) `shouldBe` sort ["event", "t", "model", "reason"]
    keysOf (eventJson (LedgerUnread 1 2 "busy")) `shouldBe` sort ["event", "t", "attempt", "reason"]
    map (eventName . eventJson) [TierChosen 1 2 "t" 0 "m", AttemptStopped 1 2 "t" "turns" 8 0.5, Escalated 1 "t" 0 1, TierSkipped 1 "m" "r", LedgerUnread 1 2 "r"]
      `shouldBe` ["tier chosen", "attempt stopped", "escalated", "tier skipped", "ledger unread"]
    keysOf (eventJson (Finished 1 2 "t" "x" "closed" Nothing (Just 0.25)))
      `shouldBe` sort ["event", "t", "attempt", "goal", "backend", "outcome", "stats", "dollars"]
