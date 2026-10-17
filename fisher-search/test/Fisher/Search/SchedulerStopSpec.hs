module Fisher.Search.SchedulerStopSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
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

import Fisher.Search.Auto
import Fisher.Search.Check (AxiomPolicy (..), standardAxioms)
import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Controller (ModelFailure (..), Stop (..), StopReason (..), cap2000)
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.FakeLsp
import Fisher.Search.Lean
import Fisher.Search.Report (eventJson, reportJson)
import Fisher.Search.Scheduler

one :: Text
one = T.unlines ["import Mathlib", "", "theorem t : T := by", "  sorry"]

three :: Text
three = T.unlines
  [ "import Mathlib", ""
  , "theorem a : A := by", "  sorry", ""
  , "theorem b : B := by", "  sorry", ""
  , "theorem c : C := by", "  sorry" ]

-- | One scripted attempt: its proof and what the run reports about it.
data Step = Step
  { sProof :: Text
  , sStop :: Maybe Stop
  , sMessage :: Maybe Text
  , sDollars :: Maybe Double
  , sCost :: Maybe Double
  -- ^ the child's own cost figure
  , sFailure :: Maybe ModelFailure
  , sThrow :: Bool
  , sDelay :: Int
  -- ^ ms the attempt runs
  }

step :: Text -> Step
step p = Step p Nothing Nothing Nothing Nothing Nothing False 5

-- | Steps per goal, taken in launch order; past the end an attempt writes
-- @BAD@.
type Script = Map.Map Text [Step]

data Run = Run
  { rReport :: SearchReport
  , rEvents :: [Event]
  , rSpecs :: [AttemptSpec]
  }

search :: SearchConfig -> Maybe AutoConfig -> Text -> Script -> IO Run
search = searchPriced (\_ -> pure Nothing)

-- | 'search' with a price for cancelled attempts ('sePrice').
searchPriced :: (AttemptSpec -> IO (Maybe Double)) -> SearchConfig -> Maybe AutoConfig -> Text -> Script -> IO Run
searchPriced price cfg ac file script0 = withSystemTempDirectory "stops" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  events <- newIORef []
  specs <- newIORef []
  calls <- newIORef []
  script <- newIORef script0
  let pop g = atomicModifyIORef' script $ \m -> case Map.findWithDefault [] g m of
        x : xs -> (Map.insert g xs m, x)
        [] -> (m, step "exact BAD")
      run sp = do
        atomicModifyIORef' specs (\ss -> (ss <> [sp], ()))
        s <- pop (asGoal sp)
        threadDelay (sDelay s * 1000)
        if sThrow s then ioError (userError "boom") else do
          let ls = T.splitOn "\n" (asStart sp)
              d = fromMaybe (error "no goal") (findDecl (asGoal sp) (declarations (asStart sp)))
              out = T.intercalate "\n" (take (dStmtEnd d + 1) ls <> ["  " <> sProof s] <> drop (dEnd d) ls)
              stats = (\c -> ChildStats (Just 1) 1 (Just 3) Nothing Nothing (Just c) Nothing Map.empty) <$> sCost s
          pure (plainResult (Just out) stats "exit 1")
            { arStop = sStop s, arLastMessage = sMessage s, arDollars = sDollars s, arFailure = sFailure s }
      env = SearchEnv
        { seRun = run, seLean = fakeLean, sePolicy = AxiomPolicy standardAxioms False, seRoot = root, seRel = "F.lean"
        , seNow = realToFrac <$> getPOSIXTime, seLog = \e -> modifyIORef events (e :), seBaseChanged = \_ -> pure ()
        , seAuto = fmap (\c -> AutoEnv c (fakeLsp calls fake0)) ac, seRetrieval = Nothing, sePrice = price }
  r <- runSearch cfg env file
  es <- reverse <$> readIORef events
  ss <- readIORef specs
  either (fail . T.unpack) (\rep -> pure (Run rep es ss)) r

-- | One backend, one slot, three attempts, two tiers, the controller on.
cfgS :: SearchConfig
cfgS = defaultSearchConfig
  { scBackends = ["x"], scSlots = 1, scMaxAttempts = 3, scBudget = 30, scParallelPerGoal = False
  , scTiers = ["example/model-a", "example/model-b"], scControl = True }

auto :: AutoConfig
auto = defaultAutoConfig {acTactics = ["rfl"], acDisprove = False}

goalOf :: Text -> Run -> GoalState
goalOf g r = fromMaybe (error "no goal") (lookup g (srGoals (rReport r)))

stop8 :: Stop
stop8 = Stop StallTurns 8 0.42

-- | The repairs without their times.
repairs :: Run -> [(Int, Text, Int, Int, Int)]
repairs r = [(i, g, b, a, c) | Repaired _ i g b a c <- rEvents r]

spec :: Spec
spec = describe "stopped attempts and the stall note" $ do
  describe "stallNote" $ do
    it "with the prover's errors and the last message" $
      stallNote stop8 "does not compile: F.lean:4:2: error: BAD" (Just "I could not find the lemma.")
        `shouldBe` T.intercalate "\n"
          [ "A previous attempt on this goal was stopped after 8 turns and $0.42 without progress on the file (turns)."
          , ""
          , "The prover's last errors on the file:"
          , "does not compile: F.lean:4:2: error: BAD"
          , ""
          , "Its last message:"
          , "I could not find the lemma." ]
    it "with neither" $
      stallNote (Stop StallDollars 3 1.5) "" Nothing
        `shouldBe` "A previous attempt on this goal was stopped after 3 turns and $1.50 without progress on the file (dollars)."
    it "caps the errors and the message at 2,000 characters each" $ do
      let errs = T.replicate 3000 "e"
          msg = T.replicate 2500 "m"
          note = stallNote stop8 errs (Just msg)
      note `shouldSatisfy` T.isInfixOf ("The prover's last errors on the file:\n" <> cap2000 errs <> "\n\n")
      note `shouldSatisfy` T.isSuffixOf ("Its last message:\n" <> cap2000 msg)
      note `shouldNotSatisfy` T.isInfixOf errs
      note `shouldNotSatisfy` T.isInfixOf msg

  it "logs a stopped attempt, repairs it as a rejected one, and puts the note in the goal's next brief only" $ do
    let long = T.replicate 2100 "x" <> "END"
        stopped = (step "intro x\n  exact BAD") {sStop = Just stop8, sMessage = Just long}
        plain = step "intro x\n  exact BAD"
    r <- search cfgS (Just auto) one (Map.fromList [("t", [stopped, step "exact BAD", step "exact BAD"])])
    p <- search cfgS (Just auto) one (Map.fromList [("t", [plain, step "exact BAD", step "exact BAD"])])
    [(i, g, why, n, d) | AttemptStopped _ i g why n d <- rEvents r] `shouldBe` [(1, "t", "turns", 8, 0.42)]
    [() | AttemptStopped {} <- rEvents p] `shouldBe` []
    -- Repair exactly as for the rejected attempt
    repairs r `shouldBe` repairs p
    repairs r `shouldSatisfy` (not . null)
    map arOutcome (srAttempts (rReport r)) `shouldBe` map arOutcome (srAttempts (rReport p))
    take 1 (map arOutcome (srAttempts (rReport r))) `shouldSatisfy` all ("rejected: " `T.isPrefixOf`)
    map asStart (rSpecs r) `shouldBe` map asStart (rSpecs p)
    gsTried (goalOf "t" r) `shouldBe` gsTried (goalOf "t" p)
    case (rSpecs r, rSpecs p) of
      ([_, second, third], [_, second', third']) -> do
        let b = asBrief second
        b `shouldSatisfy` T.isInfixOf "stopped after 8 turns and $0.42"
        b `shouldSatisfy` T.isInfixOf "The prover's last errors on the file:\n"
        b `shouldSatisfy` T.isInfixOf "error: BAD"
        b `shouldSatisfy` T.isInfixOf ("Its last message:\n" <> cap2000 long)
        b `shouldNotSatisfy` T.isInfixOf long
        -- the note follows today's brief
        b `shouldSatisfy` T.isPrefixOf (asBrief second' <> "\n\n")
        -- the next attempt moves up a tier
        asTier second `shouldBe` Just 1
        -- used once
        asBrief third `shouldBe` asBrief third'
        asBrief third `shouldNotSatisfy` T.isInfixOf "stopped after"
      (ss, ps) -> expectationFailure ("attempts: " <> show (length ss, length ps))
    gsStall (goalOf "t" r) `shouldBe` Nothing

  it "the prover's errors in the note are the verification's" $ do
    r <- search cfgS Nothing one (Map.fromList [("t", [(step "exact BAD") {sStop = Just stop8}])])
    case (srAttempts (rReport r), rSpecs r) of
      (a1 : _, _ : second : _) -> do
        let why = fromMaybe (error "not rejected") (T.stripPrefix "rejected: " (arOutcome a1))
        asBrief second `shouldBe` stallNote stop8 why Nothing
      _ -> expectationFailure "too few attempts"

  it "a stopped attempt whose file closes the goal closes it, with no note" $ do
    r <- search cfgS Nothing three (Map.fromList [("a", [(step "ok") {sStop = Just stop8, sMessage = Just "done"}])])
    gsStatus (goalOf "a" r) `shouldBe` Closed
    gsStall (goalOf "a" r) `shouldBe` Nothing
    [i | AttemptStopped _ i _ _ _ _ <- rEvents r] `shouldBe` [1]
    take 1 (map arOutcome (srAttempts (rReport r))) `shouldBe` ["closed"]
    [asBrief s | s <- rSpecs r] `shouldSatisfy` all (not . T.isInfixOf "stopped after")

  it "records model, tier, stop and dollars (the run's, else the child's)" $ do
    r <- search cfgS Nothing one (Map.fromList
      [("t", [(step "exact BAD") {sStop = Just stop8, sDollars = Just 0.42, sCost = Just 9}, (step "exact BAD") {sCost = Just 0.3}, step "ok"])])
    let recs = srAttempts (rReport r)
    map arModel recs `shouldBe` map Just ["example/model-a", "example/model-b", "example/model-b"]
    map arTierOf recs `shouldBe` [Just 0, Just 1, Just 1]
    map arStopped recs `shouldBe` [Just stop8, Nothing, Nothing]
    map arCost recs `shouldBe` [Just 0.42, Just 0.3, Nothing]
    gsTiers (goalOf "t" r) `shouldBe` [0, 1]
    srControl (rReport r) `shouldBe` True
    srEscalations (rReport r) `shouldBe` 1

  it "prices an attempt cancelled because a sibling closed the goal" $ do
    asked <- newIORef ([] :: [Int])
    let price sp = modifyIORef' asked (asId sp :) >> pure (Just 0.25)
        cfg = cfgS {scBackends = ["x", "y"], scSlots = 2, scParallelPerGoal = True}
        script = Map.fromList [("t", [(step "exact BAD") {sDelay = 5000}, (step "ok") {sDelay = 50}])]
    r <- searchPriced price cfg Nothing one script
    readIORef asked `shouldReturn` [1]
    [(arOutcome a, arCost a) | a <- srAttempts (rReport r), arId a == 1] `shouldBe` [("cancelled: the goal closed", Just 0.25)]
    [(i, d) | AttemptCancelled _ i _ d <- rEvents r] `shouldBe` [(1, Just 0.25)]
    let j = reportJson [] (rReport r)
    case j of
      Object o -> KM.lookup "dollars_by_tier" o `shouldBe` Just (object ["0" .= (0.25 :: Double)])
      _ -> expectationFailure "no object"
    [KM.lookup "dollars" o | e@AttemptCancelled {} <- rEvents r, Object o <- [eventJson e]] `shouldBe` [Just (Number 0.25)]
    -- the controller off: the price is not asked, and the event has no dollars
    writeIORef asked []
    r2 <- searchPriced price cfg {scTiers = [], scControl = False} Nothing one script
    readIORef asked `shouldReturn` []
    [d | AttemptCancelled _ _ _ d <- rEvents r2] `shouldBe` [Nothing]

  it "prices the attempts cancelled at the budget, each logged with its dollars" $ do
    let price sp = pure (if asId sp == 1 then Just 0.4 else Nothing)
    r <- searchPriced price cfgS {scBudget = 0.3} Nothing one (Map.fromList [("t", [(step "exact BAD") {sDelay = 5000}])])
    [(arOutcome a, arCost a) | a <- srAttempts (rReport r)] `shouldBe` [("cancelled: budget spent", Just 0.4)]
    [(i, d) | AttemptCancelled _ i _ d <- rEvents r] `shouldBe` [(1, Just 0.4)]

  it "a price that throws is no dollars" $ do
    let cfg = cfgS {scBackends = ["x", "y"], scSlots = 2, scParallelPerGoal = True}
    r <- searchPriced (\_ -> ioError (userError "no ledger")) cfg Nothing one
           (Map.fromList [("t", [(step "exact BAD") {sDelay = 5000}, (step "ok") {sDelay = 50}])])
    [arCost a | a <- srAttempts (rReport r), arId a == 1] `shouldBe` [Nothing]

  it "feeds no exception to the tier's health" $ do
    -- two rate limits on tier 0, an exception, then a third rate limit:
    -- the exception says nothing about the model, so the tier is skipped
    let rl = (step "exact BAD") {sFailure = Just RateLimited}
    r <- search cfgS Nothing three (Map.fromList
      [("a", [rl, rl, (step "exact BAD") {sThrow = True}, step "ok"]), ("b", [rl, step "ok"]), ("c", [step "ok"])])
    take 4 [(asGoal s, asTier s) | s <- rSpecs r] `shouldBe` [("a", Just 0), ("a", Just 0), ("a", Just 0), ("b", Just 0)]
    take 3 (map arOutcome (srAttempts (rReport r))) `shouldBe` ["model error: rate limited", "model error: rate limited", "error: user error (boom)"]
    [m | TierSkipped _ m _ <- rEvents r] `shouldBe` ["example/model-a"]

  it "keeps the note past an attempt that ends in a model error, for the next real attempt only" $ do
    r <- search cfgS Nothing one (Map.fromList
      [("t", [(step "exact BAD") {sStop = Just stop8}, (step "exact BAD") {sFailure = Just RateLimited}, step "exact BAD", step "exact BAD"])])
    take 2 (drop 1 (map arOutcome (srAttempts (rReport r)))) `shouldSatisfy` (\os -> take 1 os == ["model error: rate limited"])
    case map asBrief (rSpecs r) of
      [_, b2, b3, b4] -> do
        b2 `shouldSatisfy` T.isInfixOf "stopped after 8 turns and $0.42"
        b3 `shouldBe` b2
        b4 `shouldNotSatisfy` T.isInfixOf "stopped after"
      bs -> expectationFailure ("attempts: " <> show (length bs))
    gsStall (goalOf "t" r) `shouldBe` Nothing
