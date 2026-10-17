module Fisher.Search.SchedulerTierSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Applicative ((<|>))
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Search.Auto
import Fisher.Search.Check (AxiomPolicy (..), standardAxioms)
import Fisher.Search.Controller (ModelFailure (..))
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.FakeLsp
import Fisher.Search.Lean
import Fisher.Search.Report
import Fisher.Search.Scheduler

one :: Text
one = T.unlines ["import Mathlib", "", "theorem t : T := by", "  sorry"]

three :: Text
three = T.unlines
  [ "import Mathlib", ""
  , "theorem a : A := by", "  sorry", ""
  , "theorem b : B := by", "  sorry", ""
  , "theorem c : C := by", "  sorry" ]

-- | Results per goal, taken in launch order: the proof, the model failure,
-- a delay in ms. Past the end an attempt writes @BAD@.
type Script = Map.Map Text [(Text, Maybe ModelFailure, Int)]

data Run = Run
  { rReport :: SearchReport
  , rEvents :: [Event]
  , rSpecs :: [AttemptSpec]
  }

search :: SearchConfig -> Maybe AutoConfig -> Fake -> Text -> Script -> IO Run
search = searchF (const Nothing)

-- | 'search' with a model failure by attempt, ahead of the script's.
searchF :: (AttemptSpec -> Maybe ModelFailure) -> SearchConfig -> Maybe AutoConfig -> Fake -> Text -> Script -> IO Run
searchF failOf cfg ac f file script0 = withSystemTempDirectory "tiers" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  events <- newIORef []
  specs <- newIORef []
  calls <- newIORef []
  script <- newIORef script0
  let pop g = atomicModifyIORef' script $ \m -> case Map.findWithDefault [] g m of
        x : xs -> (Map.insert g xs m, x)
        [] -> (m, ("exact BAD", Nothing, 0))
      run sp = do
        atomicModifyIORef' specs (\ss -> (ss <> [sp], ()))
        (proof, failure, delay) <- pop (asGoal sp)
        threadDelay (delay * 1000)
        let ls = T.splitOn "\n" (asStart sp)
            d = fromMaybe (error "no goal") (findDecl (asGoal sp) (declarations (asStart sp)))
            out = T.intercalate "\n" (take (dStmtEnd d + 1) ls <> ["  " <> proof] <> drop (dEnd d) ls)
        pure (plainResult (Just out) Nothing "exit 1") {arFailure = failOf sp <|> failure}
      env = SearchEnv
        { seRun = run, seLean = fakeLean, sePolicy = AxiomPolicy standardAxioms False, seRoot = root, seRel = "F.lean"
        , seNow = realToFrac <$> getPOSIXTime, seLog = \e -> modifyIORef events (e :), seBaseChanged = \_ -> pure ()
        , seAuto = fmap (\c -> AutoEnv c (fakeLsp calls f)) ac, seRetrieval = Nothing, sePrice = \_ -> pure Nothing }
  r <- runSearch cfg env file
  es <- reverse <$> readIORef events
  ss <- readIORef specs
  either (fail . T.unpack) (\rep -> pure (Run rep es ss)) r

tiers :: [Text]
tiers = ["example/model-a", "example/model-b"]

-- | One backend, one slot, three attempts, the controller on.
cfgT :: SearchConfig
cfgT = defaultSearchConfig
  { scBackends = ["x"], scSlots = 1, scMaxAttempts = 3, scBudget = 30, scParallelPerGoal = False
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
    r <- search cfgT Nothing fake0 one Map.empty
    models r `shouldBe` map Just ["example/model-a", "example/model-b", "example/model-b"]
    map asTier (rSpecs r) `shouldBe` [Just 0, Just 1, Just 1]
    [(i, g, k, m) | TierChosen _ i g k m <- rEvents r]
      `shouldBe` [(1, "t", 0, "example/model-a"), (2, "t", 1, "example/model-b"), (3, "t", 1, "example/model-b")]
    [(g, a, b) | Escalated _ g a b <- rEvents r] `shouldBe` [("t", 0, 1)]
    statusOf "t" r `shouldBe` GaveUp

  it "uses only the first tier for a goal closed on its first attempt" $ do
    r <- search cfgT Nothing fake0 one (Map.fromList [("t", [("ok", Nothing, 5)])])
    models r `shouldBe` [Just "example/model-a"]
    [() | Escalated {} <- rEvents r] `shouldBe` []
    statusOf "t" r `shouldBe` Closed

  it "skips an unavailable tier, records a model error, and does not count the attempt" $ do
    r <- search cfgT Nothing fake0 one (Map.fromList [("t", [("exact BAD", Just Unavailable, 5)])])
    [(m, why) | TierSkipped _ m why <- rEvents r] `shouldBe` [("example/model-a", "unavailable model")]
    take 1 (map arOutcome (srAttempts (rReport r))) `shouldBe` ["model error: unavailable model"]
    models r `shouldBe` map Just ["example/model-a", "example/model-b", "example/model-b", "example/model-b"]
    length (triedOf "t" r) `shouldBe` 3
    statusOf "t" r `shouldBe` GaveUp

  it "sends a model error to neither Repair nor the kernel" $ do
    let closes = fake0 {fClose = \_ p -> if "intro x" `T.isInfixOf` p then Just Nothing else Nothing}
    r <- search cfgT {scMaxAttempts = 1} (Just defaultAutoConfig {acTactics = ["rfl"], acDisprove = False}) closes one
      (Map.fromList [("t", [("intro x\n  exact BAD", Just Unavailable, 5), ("ok", Nothing, 5)])])
    [() | Repaired {} <- rEvents r] `shouldBe` []
    map arOutcome (srAttempts (rReport r)) `shouldBe` ["model error: unavailable model", "closed"]
    statusOf "t" r `shouldBe` Closed

  it "skips a tier after three rate limits in a row" $ do
    let rl = ("exact BAD", Just RateLimited, 5)
    r <- search cfgT Nothing fake0 one (Map.fromList [("t", [rl, rl, rl])])
    take 4 (models r) `shouldBe` map Just ["example/model-a", "example/model-a", "example/model-a", "example/model-b"]
    [(m, why) | TierSkipped _ m why <- rEvents r] `shouldBe` [("example/model-a", "rate limited")]
    length (triedOf "t" r) `shouldBe` 3

  it "a success between rate limits resets the count" $ do
    let rl g = (g, Just RateLimited, 5)
    r <- search cfgT {scSlots = 1} Nothing fake0 three
      (Map.fromList [("a", [rl "exact BAD", rl "exact BAD", ("ok", Nothing, 5)]), ("b", [rl "exact BAD", ("ok", Nothing, 5)]), ("c", [("ok", Nothing, 5)])])
    [() | TierSkipped {} <- rEvents r] `shouldBe` []

  it "counts model errors past the third on a goal as failed attempts" $ do
    let nt = ("exact BAD", Just NoTools, 5)
    r <- search cfgT {scTiers = ["example/model-a"]} Nothing fake0 one (Map.fromList [("t", replicate 6 nt)])
    -- the only tier is skipped at once, so the goal gives up
    [(m, why) | TierSkipped _ m why <- rEvents r] `shouldBe` [("example/model-a", "no tool support")]
    [g | GoalGaveUp _ g <- rEvents r] `shouldBe` ["t"]
    length (rSpecs r) `shouldBe` 1
    -- with no tiers model errors are still kept out of the count, three
    -- times; the next three count as failed attempts
    r2 <- search cfgT {scTiers = []} Nothing fake0 one (Map.fromList [("t", replicate 6 nt)])
    length (rSpecs r2) `shouldBe` 6
    map arOutcome (srAttempts (rReport r2)) `shouldBe` replicate 6 "model error: no tool support"
    length (triedOf "t" r2) `shouldBe` 3
    [() | TierChosen {} <- rEvents r2] `shouldBe` []
    statusOf "t" r2 `shouldBe` GaveUp

  it "gives up on every open goal when every tier is skipped" $ do
    r <- search cfgT Nothing fake0 three
      (Map.fromList [("a", [("exact BAD", Just Unavailable, 5), ("exact BAD", Just NoTools, 5)])])
    [(m, why) | TierSkipped _ m why <- rEvents r] `shouldBe` [("example/model-a", "unavailable model"), ("example/model-b", "no tool support")]
    sort [g | GoalGaveUp _ g <- rEvents r] `shouldBe` ["a", "b", "c"]
    map (`statusOf` r) ["a", "b", "c"] `shouldBe` [GaveUp, GaveUp, GaveUp]
    length (rSpecs r) `shouldBe` 2

  it "gives parallel attempts on one goal the goal's current tier" $ do
    r <- search cfgT {scBackends = ["x", "y"], scSlots = 2, scParallelPerGoal = True} Nothing fake0 one
      (Map.fromList [("t", [("exact BAD", Nothing, 100), ("exact BAD", Nothing, 100)])])
    take 2 (models r) `shouldBe` map Just ["example/model-a", "example/model-a"]
    drop 2 (models r) `shouldBe` [Just "example/model-b"]
    [(g, a, b) | Escalated _ g a b <- rEvents r] `shouldBe` [("t", 0, 1)]

  it "a mixed backend list: tiers for the native backend only" $ do
    -- claude-code outside the tiers, every one of its attempts unavailable:
    -- no tier, no tier event, and no tier skipped for gauss
    let cfg = cfgT {scBackends = ["claude-code", "gauss"], scUntiered = ["claude-code"]}
        cc sp = if asBackend sp == "claude-code" then Just Unavailable else Nothing
    r <- searchF cc cfg Nothing fake0 one Map.empty
    let byBackend b = [sp | sp <- rSpecs r, asBackend sp == b]
    map asBackend (rSpecs r) `shouldSatisfy` elem "claude-code"
    [(asModel sp, asTier sp) | sp <- byBackend "claude-code"] `shouldSatisfy` all (== (Nothing, Nothing))
    take 1 (map asTier (byBackend "gauss")) `shouldBe` [Just 0]
    map asTier (byBackend "gauss") `shouldSatisfy` all (`elem` [Just 0, Just 1])
    map asModel (byBackend "gauss") `shouldBe` map (fmap (tiers !!) . asTier) (byBackend "gauss")
    [() | TierSkipped {} <- rEvents r] `shouldBe` []
    sort [i | TierChosen _ i _ _ _ <- rEvents r] `shouldBe` sort (map asId (byBackend "gauss"))

  it "a mixed backend list with every tier skipped: the backend outside the tiers still runs" $ do
    let cfg = cfgT {scBackends = ["claude-code", "gauss"], scUntiered = ["claude-code"]}
        gaussOut sp = if asBackend sp == "gauss" then Just Unavailable else Nothing
    r <- searchF gaussOut cfg Nothing fake0 one (Map.fromList [("t", replicate 5 ("exact BAD", Nothing, 5) <> [("ok", Nothing, 5)])])
    sort [m | TierSkipped _ m _ <- rEvents r] `shouldBe` ["example/model-a", "example/model-b"]
    [() | GoalGaveUp {} <- rEvents r, statusOf "t" r /= GaveUp] `shouldBe` []
    -- after both skips only claude-code launches, with no tier
    length [() | sp <- rSpecs r, asBackend sp == "gauss"] `shouldBe` 2
    [asTier sp | sp <- rSpecs r, asBackend sp == "claude-code"] `shouldSatisfy` all (== Nothing)
    length [() | sp <- rSpecs r, asBackend sp == "claude-code"] `shouldSatisfy` (>= 2)

  it "with no tiers and the controller off, runs today's search exactly" $ do
    let cfg = defaultSearchConfig {scBackends = ["claude-code", "gauss"], scSlots = 1, scMaxAttempts = 3, scBudget = 30}
        ok = ("ok", Nothing, 5)
    r <- search cfg Nothing fake0 three (Map.fromList [("a", [("exact BAD", Nothing, 5), ok]), ("b", [ok]), ("c", [ok])])
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
