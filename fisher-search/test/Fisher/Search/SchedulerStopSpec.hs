module Fisher.Search.SchedulerStopSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Role (partText)
import Fisher.Search.Auto
import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Controller (ModelFailure (..), Stop (..), StopReason (..), cap2000)
import Fisher.Search.FakeLsp
import Fisher.Search.Orchestrate (AttemptKey (..), Script (..), SimTrace (..), emptyScript)
import Fisher.Search.Report (eventJson, reportJson)
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
  , sSecs :: Double
  -- ^ virtual seconds the attempt runs
  }

step :: Text -> Step
step p = Step p Nothing Nothing Nothing Nothing Nothing False 1

-- | A step's result from the attempt's start.
stepResult :: Step -> AttemptSpec -> Either Text AttemptResult
stepResult s sp
  | sThrow s = Left "user error (boom)"
  | otherwise =
      let stats = (\c -> ChildStats (Just 1) 1 (Just 3) Nothing Nothing (Just c) Nothing Map.empty) <$> sCost s
       in Right (plainResult (Just (spliceProof (sProof s) sp)) stats "exit 1")
            { arStop = sStop s, arLastMessage = sMessage s, arDollars = sDollars s, arFailure = sFailure s }

-- | A goal's steps in launch order, each on its backend.
stepsOn :: Text -> [(Text, Step)] -> [ScriptEntry]
stepsOn g xs = inOrder g [(b, sSecs s, stepResult s) | (b, s) <- xs]

-- | A goal's steps in launch order on the one backend @x@.
steps :: Text -> [Step] -> [ScriptEntry]
steps g = stepsOn g . map ((,) "x")

-- | The script: past its entries an attempt writes @BAD@ at once.
scriptOf :: [ScriptEntry] -> Script
scriptOf es = emptyScript {scAttempts = es, scDefault = Just (stepResult (step "exact BAD") {sSecs = 0})}

data Run = Run
  { rReport :: SearchReport
  , rEvents :: [Event]
  , rSpecs :: [AttemptSpec]
  , rTrace :: SimTrace
  }

-- | A search; every attempt launched must be named ('allNamed').
search :: SearchConfig -> Maybe AutoConfig -> Text -> [ScriptEntry] -> IO Run
search cfg ac file = searchScript Nothing cfg ac file . scriptOf

-- | 'search' for a scenario that leaves some attempts to the default,
-- which writes @BAD@ at once.
searchPast :: SearchConfig -> Maybe AutoConfig -> Text -> [ScriptEntry] -> IO Run
searchPast cfg ac file = runScript False Nothing cfg ac file . scriptOf

-- | 'search' with a price for cancelled attempts ('oPrice') in place of
-- the script's.
searchPriced :: (AttemptSpec -> IO (Maybe Double)) -> SearchConfig -> Maybe AutoConfig -> Text -> [ScriptEntry] -> IO Run
searchPriced price cfg ac file = searchScript (Just price) cfg ac file . scriptOf

searchScript :: Maybe (AttemptSpec -> IO (Maybe Double)) -> SearchConfig -> Maybe AutoConfig -> Text -> Script -> IO Run
searchScript = runScript True

-- | A search of this script; with True every attempt launched must be
-- named ('allNamed').
runScript :: Bool -> Maybe (AttemptSpec -> IO (Maybe Double)) -> SearchConfig -> Maybe AutoConfig -> Text -> Script -> IO Run
runScript named price cfg ac file sc = do
  calls <- newIORef []
  let se = (simEnv file) {simAuto = fmap (\c -> AutoEnv c (fakeLsp calls fake0) Nothing) ac, simPrice = price}
  (rep, es, tr) <- simSearchEnv se cfg sc
  if named then allNamed tr else pure ()
  pure (Run rep es (stSpawned tr) tr)

-- | One backend, one slot, three attempts, two tiers, the controller on.
cfgS :: SearchConfig
cfgS = defaultSearchConfig
  { scBackends = ["x"], scSlots = 1, scMaxAttempts = 3, scBudget = 6000, scParallelPerGoal = False
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
      partText (stallNote stop8 "does not compile: F.lean:4:2: error: BAD" (Just "I could not find the lemma."))
        `shouldBe` T.intercalate "\n"
          [ "A previous attempt on this goal was stopped after 8 turns and $0.42 without progress on the file (turns)."
          , ""
          , "The prover's last errors on the file:"
          , "does not compile: F.lean:4:2: error: BAD"
          , ""
          , "Its last message:"
          , "I could not find the lemma." ]
    it "with neither" $
      partText (stallNote (Stop StallDollars 3 1.5) "" Nothing)
        `shouldBe` "A previous attempt on this goal was stopped after 3 turns and $1.50 without progress on the file (dollars)."
    it "caps the errors and the message at 2,000 characters each" $ do
      let errs = T.replicate 3000 "e"
          msg = T.replicate 2500 "m"
          note = partText (stallNote stop8 errs (Just msg))
      note `shouldSatisfy` T.isInfixOf ("The prover's last errors on the file:\n" <> cap2000 errs <> "\n\n")
      note `shouldSatisfy` T.isSuffixOf ("Its last message:\n" <> cap2000 msg)
      note `shouldNotSatisfy` T.isInfixOf errs
      note `shouldNotSatisfy` T.isInfixOf msg

  it "logs a stopped attempt, repairs it as a rejected one, and puts the note in the goal's next brief only" $ do
    let long = T.replicate 2100 "x" <> "END"
        stopped = (step "intro x\n  exact BAD") {sStop = Just stop8, sMessage = Just long}
        plain = step "intro x\n  exact BAD"
    r <- search cfgS (Just auto) one (steps "t" [stopped, step "exact BAD", step "exact BAD"])
    p <- search cfgS (Just auto) one (steps "t" [plain, step "exact BAD", step "exact BAD"])
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
        let b = briefText second
        b `shouldSatisfy` T.isInfixOf "stopped after 8 turns and $0.42"
        b `shouldSatisfy` T.isInfixOf "The prover's last errors on the file:\n"
        b `shouldSatisfy` T.isInfixOf "error: BAD"
        b `shouldSatisfy` T.isInfixOf ("Its last message:\n" <> cap2000 long)
        b `shouldNotSatisfy` T.isInfixOf long
        -- the note follows today's brief
        b `shouldSatisfy` T.isPrefixOf (briefText second' <> "\n\n")
        -- the next attempt moves up a tier
        asTier second `shouldBe` Just 1
        -- used once
        briefText third `shouldBe` briefText third'
        briefText third `shouldNotSatisfy` T.isInfixOf "stopped after"
      (ss, ps) -> expectationFailure ("attempts: " <> show (length ss, length ps))
    gsStall (goalOf "t" r) `shouldBe` Nothing

  it "the prover's errors in the note are the verification's" $ do
    r <- searchPast cfgS Nothing one (steps "t" [(step "exact BAD") {sStop = Just stop8}])
    case (srAttempts (rReport r), rSpecs r) of
      (a1 : _, _ : second : _) -> do
        let why = fromMaybe (error "not rejected") (T.stripPrefix "rejected: " (arOutcome a1))
        briefText second `shouldBe` partText (stallNote stop8 why Nothing)
      _ -> expectationFailure "too few attempts"

  it "a stopped attempt whose file closes the goal closes it, with no note" $ do
    r <- searchPast cfgS Nothing three (steps "a" [(step "ok") {sStop = Just stop8, sMessage = Just "done"}])
    gsStatus (goalOf "a" r) `shouldBe` Closed
    gsStall (goalOf "a" r) `shouldBe` Nothing
    [i | AttemptStopped _ i _ _ _ _ <- rEvents r] `shouldBe` [1]
    take 1 (map arOutcome (srAttempts (rReport r))) `shouldBe` ["closed"]
    [briefText s | s <- rSpecs r] `shouldSatisfy` all (not . T.isInfixOf "stopped after")

  it "records model, tier, stop and dollars (the run's, else the child's)" $ do
    r <- search cfgS Nothing one
      (steps "t" [(step "exact BAD") {sStop = Just stop8, sDollars = Just 0.42, sCost = Just 9}, (step "exact BAD") {sCost = Just 0.3}, step "ok"])
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
        entries = stepsOn "t" [("x", (step "exact BAD") {sSecs = 1000}), ("y", (step "ok") {sSecs = 10})]
    r <- searchPriced price cfg Nothing one entries
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
    r2 <- searchPriced price cfg {scTiers = [], scControl = False} Nothing one entries
    readIORef asked `shouldReturn` []
    [d | AttemptCancelled _ _ _ d <- rEvents r2] `shouldBe` [Nothing]

  it "prices the attempts cancelled at the budget, each logged with its dollars" $ do
    let price sp = pure (if asId sp == 1 then Just 0.4 else Nothing)
    r <- searchPriced price cfgS {scBudget = 60} Nothing one (steps "t" [(step "exact BAD") {sSecs = 1000}])
    [(arOutcome a, arCost a) | a <- srAttempts (rReport r)] `shouldBe` [("cancelled: budget spent", Just 0.4)]
    [(i, d) | AttemptCancelled _ i _ d <- rEvents r] `shouldBe` [(1, Just 0.4)]

  it "a price that throws is no dollars" $ do
    let cfg = cfgS {scBackends = ["x", "y"], scSlots = 2, scParallelPerGoal = True}
    r <- searchPriced (\_ -> ioError (userError "no ledger")) cfg Nothing one
           (stepsOn "t" [("x", (step "exact BAD") {sSecs = 1000}), ("y", (step "ok") {sSecs = 10})])
    [arCost a | a <- srAttempts (rReport r), arId a == 1] `shouldBe` [Nothing]

  it "feeds no exception to the tier's health" $ do
    -- two rate limits on tier 0, an exception, then a third rate limit:
    -- the exception says nothing about the model, so the tier is skipped
    let rl = (step "exact BAD") {sFailure = Just RateLimited}
    r <- search cfgS Nothing three
      (steps "a" [rl, rl, (step "exact BAD") {sThrow = True}, step "ok"] <> steps "b" [rl, step "ok"] <> steps "c" [step "ok"])
    take 4 [(asGoal s, asTier s) | s <- rSpecs r] `shouldBe` [("a", Just 0), ("a", Just 0), ("a", Just 0), ("b", Just 0)]
    take 3 (map arOutcome (srAttempts (rReport r))) `shouldBe` ["model error: rate limited", "model error: rate limited", "error: user error (boom)"]
    [m | TierSkipped _ m _ <- rEvents r] `shouldBe` ["example/model-a"]

  it "keeps the note past an attempt that ends in a model error, for the next real attempt only" $ do
    r <- search cfgS Nothing one
      (steps "t" [(step "exact BAD") {sStop = Just stop8}, (step "exact BAD") {sFailure = Just RateLimited}, step "exact BAD", step "exact BAD"])
    take 2 (drop 1 (map arOutcome (srAttempts (rReport r)))) `shouldSatisfy` (\os -> take 1 os == ["model error: rate limited"])
    case map briefText (rSpecs r) of
      [_, b2, b3, b4] -> do
        b2 `shouldSatisfy` T.isInfixOf "stopped after 8 turns and $0.42"
        b3 `shouldBe` b2
        b4 `shouldNotSatisfy` T.isInfixOf "stopped after"
      bs -> expectationFailure ("attempts: " <> show (length bs))
    gsStall (goalOf "t" r) `shouldBe` Nothing

  it "cancels both attempts in flight when the budget runs out, prices both and logs it" $ do
    let cfg = cfgS {scBackends = ["x", "y"], scSlots = 2, scParallelPerGoal = True, scBudget = 10}
        es = stepsOn "t" [("x", (step "exact BAD") {sSecs = 12}), ("y", (step "ok") {sSecs = 15})]
    r <- searchScript Nothing cfg Nothing one
           emptyScript {scAttempts = es, scPrices = [(AttemptKey "t" "x" 0, 0.4), (AttemptKey "t" "y" 0, 0.7)]}
    [(i, b) | Launched _ i _ b <- rEvents r] `shouldBe` [(1, "x"), (2, "y")]
    [(t, i, d) | AttemptCancelled t i _ d <- rEvents r] `shouldBe` [(10, 2, Just 0.7), (10, 1, Just 0.4)]
    [t | BudgetSpent t <- rEvents r] `shouldBe` [10]
    [(arId a, arOutcome a, arCost a) | a <- srAttempts (rReport r)]
      `shouldBe` [(2, "cancelled: budget spent", Just 0.7), (1, "cancelled: budget spent", Just 0.4)]
    (srBudgetSpent (rReport r), srElapsed (rReport r)) `shouldBe` (True, 10)
    length (stCancelled (rTrace r)) `shouldBe` 2
    [() | Finished {} <- rEvents r] `shouldBe` []

  it "cancels the other attempt on a goal one closes, logged with its price" $ do
    let cfg = cfgS {scBackends = ["x", "y"], scSlots = 2, scParallelPerGoal = True}
        es = stepsOn "t" [("x", (step "ok") {sSecs = 2}), ("y", (step "exact BAD") {sSecs = 9})]
    r <- searchScript Nothing cfg Nothing one emptyScript {scAttempts = es, scPrices = [(AttemptKey "t" "y" 0, 0.25)]}
    gsStatus (goalOf "t" r) `shouldBe` Closed
    [(t, i, g, d) | AttemptCancelled t i g d <- rEvents r] `shouldBe` [(2, 2, "t", Just 0.25)]
    [(arOutcome a, arCost a) | a <- srAttempts (rReport r), arId a == 2] `shouldBe` [("cancelled: the goal closed", Just 0.25)]
    [i | Finished _ i _ _ _ _ _ <- rEvents r] `shouldBe` [1]
    stClock (rTrace r) `shouldBe` 2

-- | An attempt's brief as the text the backend is handed.
briefText :: AttemptSpec -> Text
briefText = partText . asBrief
