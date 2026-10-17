module Fisher.Search.SchedulerAutoSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (isDigit)
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
import Fisher.Search.Check (AxiomPolicy (..), LeanRun (..), standardAxioms)
import Fisher.Search.Disprove (Disproof (..))
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.FakeLsp
import Fisher.Search.Lean
import Fisher.Search.Report
import Fisher.Search.Scheduler

three :: Text
three = T.unlines
  [ "import Mathlib", ""
  , "theorem a : A := by", "  sorry", ""
  , "theorem b : B := by", "  sorry", ""
  , "theorem c : C := by", "  sorry" ]

one :: Text
one = T.unlines ["import Mathlib", "", "theorem t : T := by", "  sorry"]

-- | Proofs per (goal, backend), taken in order; @*@ for any backend. A
-- proof is written under the statement; the delay is in ms.
type Script = Map.Map (Text, Text) [(Text, Int)]

data Run = Run
  { rReport :: SearchReport
  , rEvents :: [Event]
  , rSpecs :: [AttemptSpec]
  , rCalls :: [Text]
  }

search :: SearchConfig -> Maybe AutoConfig -> Fake -> LeanRun -> Text -> Script -> IO Run
search cfg ac f lean file script0 = withSystemTempDirectory "search" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  events <- newIORef []
  specs <- newIORef []
  calls <- newIORef []
  script <- newIORef script0
  let pop g b = atomicModifyIORef' script $ \m ->
        case [(k, x, xs) | k <- [(g, b), (g, "*")], x : xs <- [Map.findWithDefault [] k m]] of
          ((k, x, xs) : _) -> (Map.insert k xs m, x)
          [] -> (m, ("sorry", 0))
      run sp = do
        atomicModifyIORef' specs (\ss -> (ss <> [sp], ()))
        (proof, delay) <- pop (asGoal sp) (asBackend sp)
        threadDelay (delay * 1000)
        let ls = T.splitOn "\n" (asStart sp)
            d = fromMaybe (error "no goal") (findDecl (asGoal sp) (declarations (asStart sp)))
            out = T.intercalate "\n" (take (dStmtEnd d + 1) ls <> ["  " <> proof] <> drop (dEnd d) ls)
        pure (plainResult (Just out) Nothing "")
      env = SearchEnv run lean (AxiomPolicy standardAxioms False) root "F.lean"
              (realToFrac <$> getPOSIXTime) (\e -> modifyIORef events (e :)) (\_ -> pure ())
              (fmap (\c -> AutoEnv c (fakeLsp calls f)) ac) Nothing (\_ -> pure Nothing)
  r <- runSearch cfg env file
  es <- reverse <$> readIORef events
  ss <- readIORef specs
  cs <- readIORef calls
  either (fail . T.unpack) (\rep -> pure (Run rep es ss cs)) r

auto :: AutoConfig
auto = defaultAutoConfig {acTactics = ["rfl", "simp_all"]}

cfg0 :: SearchConfig
cfg0 = defaultSearchConfig {scBackends = ["claude-code", "gauss"], scSlots = 4, scMaxAttempts = 3, scBudget = 30, scParallelPerGoal = True}

goalOf :: Text -> SearchReport -> GoalState
goalOf g rep = fromMaybe (error "no goal") (lookup g (srGoals rep))

specsFor :: Text -> Run -> [AttemptSpec]
specsFor g r = [s | s <- rSpecs r, asGoal s == g]

-- | The holes of a goal in a file.
holesOf :: Text -> Text -> [Hole]
holesOf g src = maybe [] (holesIn src) (findDecl g (declarations src))

-- | The line numbers a brief lists.
briefLines :: Text -> [Int]
briefLines b = [ read (T.unpack n) | l <- T.lines b, Just r <- [T.stripPrefix "- line " l], let n = T.takeWhile isDigit r, not (T.null n) ]

isAutoEvent :: Event -> Bool
isAutoEvent = \case
  AutoClosed {} -> True
  AutoMissed {} -> True
  Repaired {} -> True
  GoalDisproved {} -> True
  AutoOff {} -> True
  AutoTimeout {} -> True
  _ -> False

keysOf :: Value -> [Text]
keysOf = \case
  Object o -> sort (map K.toText (KM.keys o))
  _ -> []

field :: Text -> Value -> Value
field k = \case
  Object o -> fromMaybe Null (KM.lookup (K.fromText k) o)
  _ -> Null

goalsJson :: Value -> [Value]
goalsJson v = case field "goals" v of
  Array xs -> foldr (:) [] xs
  _ -> []

spec :: Spec
spec = describe "the automation pre-pass in flat search" $ do
  it "with auto off, runs today's search: no new events, today's report fields" $ do
    r <- search cfg0 {scSlots = 1} Nothing fake0 fakeLean three
      (Map.fromList [ (("a", "claude-code"), [("BAD", 5)]), (("a", "gauss"), [("ok", 5)])
                    , (("b", "*"), [("ok", 5)]), (("c", "*"), [("ok", 5)]) ])
    lookup "a" [(g, gsStatus s) | (g, s) <- srGoals (rReport r)] `shouldBe` Just Closed
    [b | Launched _ _ "a" b <- rEvents r] `shouldBe` ["claude-code", "gauss"]
    filter isAutoEvent (rEvents r) `shouldBe` []
    rCalls r `shouldBe` []
    (srAutoRan (rReport r), srAutoOff (rReport r)) `shouldBe` (False, Nothing)
    let j = reportJson [] (rReport r)
    keysOf j `shouldBe` sort ["elapsed_s", "goals_total", "goals_closed", "closed_per_hour", "budget_spent", "goals", "backends", "attempts"]
    map keysOf (goalsJson j) `shouldBe` replicate 3 (sort ["goal", "status", "attempts", "by_attempt", "by_backend", "closed_at_s"])

  it "closes a goal Auto closes without a model, merging the filled proof" $ do
    r <- search cfg0 (Just auto) fake0 {fClose = \g _ -> if g == "a" then Just Nothing else Nothing} fakeLean three
      (Map.fromList [((g, "*"), [("ok", 5)]) | g <- ["b", "c"]])
    [g | Launched _ _ g _ <- rEvents r] `shouldSatisfy` notElem "a"
    [(g, s) | AutoClosed _ g s _ <- rEvents r] `shouldBe` [("a", "set_option maxHeartbeats 200000 in rfl")]
    sort [g | AutoMissed _ g _ <- rEvents r] `shouldBe` ["b", "c"]
    gsStatus (goalOf "a" (rReport r)) `shouldBe` Closed
    gsClosedBy (goalOf "a" (rReport r)) `shouldBe` Just "auto"
    gsClosedBy (goalOf "b" (rReport r)) `shouldBe` Just "model"
    fmap dProof (findDecl "a" (declarations (srBase (rReport r)))) `shouldBe` Just "  set_option maxHeartbeats 200000 in rfl"
    srClosed (rReport r) `shouldBe` 3

  it "sends a goal whose Auto candidate the kernel rejects to a model, logging no miss" $ do
    r <- search cfg0 {scSlots = 1} (Just auto) fake0 {fClose = \g _ -> if g == "a" then Just (Just "exact BAD") else Nothing} fakeLean three
      (Map.fromList [((g, "*"), [("ok", 5)]) | g <- ["a", "b", "c"]])
    [g | AutoClosed _ g _ _ <- rEvents r] `shouldBe` []
    [g | AutoMissed _ g _ <- rEvents r] `shouldSatisfy` notElem "a"
    [g | Launched _ _ g _ <- rEvents r] `shouldSatisfy` elem "a"
    gsStatus (goalOf "a" (rReport r)) `shouldBe` Closed
    gsClosedBy (goalOf "a" (rReport r)) `shouldBe` Just "model"
    -- the first attempt starts from the base Auto left unchanged
    fmap (holesOf "a" . asStart) (take 1 (specsFor "a" r)) `shouldBe` [[Hole 4 2 False]]

  it "merges nothing for a goal Auto misses and logs the miss" $ do
    r <- search cfg0 {scSlots = 1} (Just auto) fake0 fakeLean three
      (Map.fromList [((g, "*"), [("ok", 5)]) | g <- ["a", "b", "c"]])
    [g | AutoMissed _ g _ <- rEvents r] `shouldBe` ["a", "b", "c"]
    map (length . holesOf "a" . asStart) (take 1 (specsFor "a" r)) `shouldBe` [1]
    map (gsClosedBy . snd) (srGoals (rReport r)) `shouldBe` replicate 3 (Just "model")

  it "repairs a failed attempt into the next attempt's start, brief and all" $ do
    r <- search cfg0 {scSlots = 2, scParallelPerGoal = False} (Just auto) fake0 fakeLean three
      (Map.fromList [ (("a", "*"), [("skip\n  ok", 300)]), (("c", "*"), [("ok", 400)])
                    , (("b", "*"), [("intro x\n  exact BAD", 10), ("ok", 5)]) ])
    [(i, g, b, a', c') | Repaired _ i g b a' c' <- rEvents r] `shouldBe` [(2, "b", 1, 1, 0)]
    -- a repaired attempt that did not close is recorded once, with why it failed
    [arOutcome a | a <- srAttempts (rReport r), arId a == 2] `shouldBe` ["rejected: does not compile: _fisher_search_F.lean:6:2: error: BAD"]
    case specsFor "b" r of
      [_, second] -> do
        fmap dProof (findDecl "b" (declarations (asStart second))) `shouldBe` Just "  intro x\n  sorry"
        -- a's two-line proof was merged meanwhile: the start is the current base
        T.isInfixOf "skip\n  ok" (asStart second) `shouldBe` True
        asBrief second `shouldSatisfy` T.isPrefixOf "Start from this partial proof; fill these holes"
        briefLines (asBrief second) `shouldBe` map hLine (holesOf "b" (asStart second))
        -- the repaired file had the hole a line higher
        briefLines (asBrief second) `shouldBe` [9]
        asBrief second `shouldSatisfy` T.isInfixOf "goal at line"
      ss -> expectationFailure ("attempts on b: " <> show (length ss))
    gsStatus (goalOf "b" (rReport r)) `shouldBe` Closed
    gsClosedBy (goalOf "b" (rReport r)) `shouldBe` Just "model+repair"
    gsClosedBy (goalOf "a" (rReport r)) `shouldBe` Just "model"

  it "keeps the partial proof with the fewest holes" $ do
    let two = "intro x\n  exact BAD\n  exact BAD"
        threeH = "intro x\n  exact BAD\n  exact BAD\n  exact BAD"
    r <- search (defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 5, scBudget = 30, scParallelPerGoal = False}) (Just auto {acDisprove = False}) fake0 fakeLean one
      (Map.fromList [(("t", "*"), [(two, 5), (threeH, 5), ("intro x\n  exact BAD", 5), ("ok", 5)])])
    [(i, a') | Repaired _ i _ _ a' _ <- rEvents r] `shouldBe` [(1, 2), (2, 3), (3, 1)]
    map (length . holesOf "t" . asStart) (specsFor "t" r) `shouldBe` [1, 2, 2, 1]
    let gs = goalOf "t" (rReport r)
    fmap (\(_, ag, _) -> length (holesOf "t" ag)) (gsPartial gs) `shouldBe` Just 1
    gsClosedBy gs `shouldBe` Just "model+repair"
    srRepairs (rReport r) `shouldBe` 3

  it "closes a goal at once when Auto fills every hole Repair left" $ do
    r <- search (defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 3, scBudget = 30, scParallelPerGoal = False}) (Just auto) fake0 {fClose = \_ p -> if "intro x" `T.isInfixOf` p then Just Nothing else Nothing} fakeLean one
      (Map.fromList [(("t", "*"), [("intro x\n  exact BAD", 5)])])
    [(i, b, a', c') | Repaired _ i _ b a' c' <- rEvents r] `shouldBe` [(1, 1, 0, 1)]
    length (specsFor "t" r) `shouldBe` 1
    gsStatus (goalOf "t" (rReport r)) `shouldBe` Closed
    gsClosedBy (goalOf "t" (rReport r)) `shouldBe` Just "model+repair"
    [g | GoalClosed _ g 1 <- rEvents r] `shouldBe` ["t"]
    -- the attempt is recorded once, as Repair's close
    map arOutcome (srAttempts (rReport r)) `shouldBe` ["closed by repair"]
    [o | Finished _ 1 _ _ o _ _ <- rEvents r] `shouldBe` ["closed by repair"]

  it "does not keep a zero-hole repair the kernel rejects as a start" $ do
    r <- search (defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 3, scBudget = 30, scParallelPerGoal = False}) (Just auto {acDisprove = False})
      fake0 {fClose = \_ p -> if "intro x" `T.isInfixOf` p then Just (Just "exact BAD") else Nothing} fakeLean one
      (Map.fromList [(("t", "*"), [("intro x\n  exact BAD", 5), ("ok", 5)])])
    [(i, a') | Repaired _ i _ _ a' _ <- rEvents r] `shouldBe` [(1, 0)]
    gsPartial (goalOf "t" (rReport r)) `shouldBe` Nothing
    case specsFor "t" r of
      [_, second] -> do
        asBrief second `shouldBe` ""
        fmap dProof (findDecl "t" (declarations (asStart second))) `shouldBe` Just "  sorry"
      ss -> expectationFailure ("attempts on t: " <> show (length ss))
    gsClosedBy (goalOf "t" (rReport r)) `shouldBe` Just "model"

  it "runs a goal's pre-pass just before its first launch, so a slot fills after one pre-pass, not three" $ do
    r <- search cfg0 {scSlots = 2, scParallelPerGoal = False} (Just auto {acTactics = ["rfl"], acDisprove = False})
      fake0 {fDelay = \m -> if m == "attempts" then 300 else 0} fakeLean three
      (Map.fromList [((g, "*"), [("ok", 1500)]) | g <- ["a", "b", "c"]])
    let es = rEvents r
        before = takeWhile (not . isLaunch) es
    [g | AutoMissed _ g _ <- before] `shouldBe` ["a"]
    [g | e <- es, Just g <- [autoOrLaunch e]] `shouldBe` ["auto a", "launch a", "auto b", "launch b", "auto c", "launch c"]
    -- c's pre-pass waited for a free slot
    let tAutoC = head [t | AutoMissed t "c" _ <- es]
        tFirstEnd = minimum [t | Finished t _ _ _ _ _ _ <- es]
    tAutoC `shouldSatisfy` (>= tFirstEnd)
    srClosed (rReport r) `shouldBe` 3

  it "logs a pre-pass call cut off at its cap as an auto timeout" $ do
    r <- search (defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 1, scBudget = 30, scParallelPerGoal = False}) (Just auto {acSeconds = 0.3, acDisprove = False})
      fake0 {fDelay = \m -> if m == "attempts" then 2000 else 0} fakeLean one
      (Map.fromList [(("t", "*"), [("ok", 5)])])
    [(g, step) | AutoTimeout _ g step <- rEvents r] `shouldBe` [("t", "auto")]
    [g | AutoMissed _ g _ <- rEvents r] `shouldBe` ["t"]

  it "logs a repair cut off at its wall cap as a repair timeout" $ do
    r <- search (defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 2, scBudget = 30, scParallelPerGoal = False}) (Just auto {acRepairSeconds = 0.3, acDisprove = False})
      fake0 {fDelay = \m -> if m == "diagnostics" then 2000 else 0} fakeLean one
      (Map.fromList [(("t", "*"), [("intro x\n  exact BAD", 5), ("ok", 5)])])
    [(g, step) | AutoTimeout _ g step <- rEvents r] `shouldBe` [("t", "repair")]
    [() | Repaired {} <- rEvents r] `shouldBe` []
    gsStatus (goalOf "t" (rReport r)) `shouldBe` Closed

  it "stops the pre-pass at the budget and launches nothing" $ do
    r <- search cfg0 {scBudget = 1} (Just auto) fake0 {fDelay = \m -> if m == "attempts" then 5000 else 0} fakeLean three
      (Map.fromList [((g, "*"), [("ok", 5)]) | g <- ["a", "b", "c"]])
    [() | Launched {} <- rEvents r] `shouldBe` []
    [() | BudgetSpent _ <- rEvents r] `shouldBe` [()]
    srBudgetSpent (rReport r) `shouldBe` True
    length (filter ("attempts" `T.isPrefixOf`) (rCalls r)) `shouldBe` 1
    srElapsed (rReport r) `shouldSatisfy` (< 2.5)

  it "cuts a slow repair off at the budget and launches nothing after it" $ do
    r <- search (defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 3, scBudget = 1, scParallelPerGoal = False}) (Just auto) fake0 {fDelay = \m -> if m == "diagnostics" then 5000 else 0} fakeLean one
      (Map.fromList [(("t", "*"), [("intro x\n  exact BAD", 5), ("ok", 5)])])
    [() | Launched {} <- rEvents r] `shouldBe` [()]
    [() | BudgetSpent _ <- rEvents r] `shouldBe` [()]
    srElapsed (rReport r) `shouldSatisfy` (< 2.5)

  it "disproves a goal after its second failure and cancels what still runs on it" $ do
    r <- search (defaultSearchConfig {scBackends = ["x", "y", "z"], scSlots = 3, scMaxAttempts = 3, scBudget = 30, scParallelPerGoal = True}) (Just auto) fake0 {fSuspects = ["t"]} fakeLean one
      (Map.fromList [(("t", "x"), [("BAD", 10)]), (("t", "y"), [("BAD", 60)]), (("t", "z"), [("ok", 5000)])])
    let gs = goalOf "t" (rReport r)
    gsStatus gs `shouldBe` Disproved
    gsDisproof gs `shouldBe` Just (DFalse "decide")
    [(g, k, x) | GoalDisproved _ g k x <- rEvents r] `shouldBe` [("t", "false", "decide")]
    [g | AttemptCancelled _ _ g _ <- rEvents r] `shouldBe` ["t"]
    [(arBackend a, arOutcome a) | a <- srAttempts (rReport r), "cancelled" `T.isPrefixOf` arOutcome a]
      `shouldBe` [("z", "cancelled: the goal was disproved")]
    length (filter ("plausible" `T.isPrefixOf`) (rCalls r)) `shouldBe` 1
    srElapsed (rReport r) `shouldSatisfy` (< 4)
    statusName Disproved `shouldBe` "disproved"

  it "passes a counterexample it cannot certify to the next brief and goes on" $ do
    r <- search (defaultSearchConfig {scBackends = ["x"], scSlots = 1, scMaxAttempts = 4, scBudget = 30, scParallelPerGoal = False}) (Just auto) fake0 {fSuspects = ["t"]} noCertify one
      (Map.fromList [(("t", "*"), [("BAD", 5), ("BAD", 5), ("ok", 5)])])
    [(g, k, x) | GoalDisproved _ g k x <- rEvents r] `shouldBe` [("t", "suspect", "x := 0")]
    map (T.isInfixOf "x := 0" . asBrief) (specsFor "t" r) `shouldBe` [False, False, True]
    let gs = goalOf "t" (rReport r)
    gsStatus gs `shouldBe` Closed
    gsDisproof gs `shouldBe` Just (DSuspect "x := 0")
    length (filter ("plausible" `T.isPrefixOf`) (rCalls r)) `shouldBe` 1

  it "turns auto off once when the server fails at the pre-pass, and searches as today" $ do
    r <- search cfg0 {scSlots = 1} (Just auto) fake0 {fDown = const True} fakeLean three
      (Map.fromList [((g, "*"), [("BAD", 5), ("BAD", 5), ("ok", 5)]) | g <- ["a", "b", "c"]])
    [() | AutoOff {} <- rEvents r] `shouldBe` [()]
    -- the failed call, then the server closed at once
    rCalls r `shouldBe` ["attempts a", "close"]
    filter (\e -> isAutoEvent e && not (isOff e)) (rEvents r) `shouldBe` []
    srClosed (rReport r) `shouldBe` 3
    -- it went off before any step completed: it did not run
    (srAutoRan (rReport r), srAutoOff (rReport r)) `shouldBe` (False, Just "server died")

  it "turns auto off once when the server fails mid-run" $ do
    r <- search cfg0 {scSlots = 1} (Just auto) fake0 {fDown = (== "diagnostics"), fSuspects = ["a"]} fakeLean three
      (Map.fromList [((g, "*"), [("intro x\n  exact BAD", 5), ("BAD", 5), ("ok", 5)]) | g <- ["a", "b", "c"]])
    [() | AutoOff {} <- rEvents r] `shouldBe` [()]
    -- after the failed repair nothing reaches the server: no plausible
    length (filter (== "diagnostics") (rCalls r)) `shouldBe` 1
    filter (== "close") (rCalls r) `shouldBe` ["close"]
    length (filter ("plausible" `T.isPrefixOf`) (rCalls r)) `shouldBe` 0
    [() | Repaired {} <- rEvents r] `shouldBe` []
    [() | GoalDisproved {} <- rEvents r] `shouldBe` []
    srClosed (rReport r) `shouldBe` 3
    -- the pre-pass completed first: it ran, then went off
    (srAutoRan (rReport r), srAutoOff (rReport r)) `shouldBe` (True, Just "server died")

  describe "the report" $ do
    let st = GoalState Closed ["x"] [] (Just (1, "x")) (Just 2) Nothing Nothing Nothing False Nothing []
        rep = SearchReport
          { srGoals =
              [ ("a", st {gsBy = Nothing, gsClosedBy = Just "auto"})
              , ("b", st {gsClosedBy = Just "model+repair"})
              , ("c", st {gsStatus = Disproved, gsBy = Nothing, gsAt = Nothing, gsDisproof = Just (DFalse "decide")})
              , ("d", st {gsStatus = GaveUp, gsBy = Nothing, gsAt = Nothing, gsDisproof = Just (DSuspect "x := 0")})
              , ("e", st {gsClosedBy = Just "model", gsDisproof = Just DUnknown}) ]
          , srAttempts = [], srElapsed = 10, srClosed = 3, srBudgetSpent = False, srBase = ""
          , srAuto = True, srRepairs = 2, srAutoRan = True, srAutoOff = Nothing, srRetrieval = Nothing
          , srControl = False, srEscalations = 0 }
        j = reportJson [] rep
    it "carries closedBy and disproof per goal" $ do
      map (field "closedBy") (goalsJson j) `shouldBe` [toJSON ("auto" :: Text), toJSON ("model+repair" :: Text), Null, Null, toJSON ("model" :: Text)]
      map (field "disproof") (goalsJson j)
        `shouldBe` [ Null, Null
                   , field "x" (toJSONObj [("verdict", "false"), ("tactic", "decide")])
                   , field "x" (toJSONObj [("verdict", "suspect"), ("counterexample", "x := 0")])
                   , Null ]
      map (elem "disproof" . keysOf) (goalsJson j) `shouldBe` [False, False, True, True, False]
      map (field "status") (goalsJson j) !! 2 `shouldBe` toJSON ("disproved" :: Text)
    it "carries the run totals" $ do
      map (\k -> field k j) ["auto_closed", "repairs_kept", "repaired_then_closed", "disproved"]
        `shouldBe` map (toJSON :: Int -> Value) [1, 2, 1, 1]
    it "says whether the automation was asked for, ran, and why it went off" $ do
      let obj ps = Object (KM.fromList [(K.fromText k, v) | (k, v) <- ps])
          j0 = obj [(K.toText k, v) | (k, v) <- autoStateJson Nothing True Nothing]
          j1 = obj [(K.toText k, v) | (k, v) <- autoStateJson (Just "the Lean server did not start: x") True Nothing]
          j2 = obj [(K.toText k, v) | (k, v) <- autoStateJson Nothing False (Just "server died")]
      map (\k -> field k j0) ["auto_requested", "auto_ran", "auto_off"] `shouldBe` [Bool True, Bool True, Null]
      map (\k -> field k j1) ["auto_requested", "auto_ran", "auto_off"] `shouldBe` [Bool True, Bool False, String "the Lean server did not start: x"]
      map (\k -> field k j2) ["auto_requested", "auto_ran", "auto_off"] `shouldBe` [Bool True, Bool False, String "server died"]
    it "leaves them out when auto was off" $
      keysOf (reportJson [] rep {srAuto = False}) `shouldBe` sort ["elapsed_s", "goals_total", "goals_closed", "closed_per_hour", "budget_spent", "goals", "backends", "attempts"]
    it "renders the new events" $ do
      let evs = [ AutoClosed 1 "a" "rfl; omega" 2.5, AutoMissed 2 "b" 60, Repaired 3 4 "c" 2 1 1
                , GoalDisproved 4 "d" "false" "decide", GoalDisproved 5 "e" "suspect" "x := 0\ny := 1", AutoOff 6 "server died"
                , AutoTimeout 7 "f" "repair" ]
      map (field "event" . eventJson) evs
        `shouldBe` map (toJSON :: Text -> Value) ["auto closed", "auto missed", "repaired", "disproved", "disproved", "auto off", "auto timeout"]
      map (\k -> field k (eventJson (evs !! 6))) ["goal", "step"] `shouldBe` map (toJSON :: Text -> Value) ["f", "repair"]
      field "snippets" (eventJson (head evs)) `shouldBe` toJSON ("rfl; omega" :: Text)
      map (\k -> field k (eventJson (evs !! 2))) ["attempt", "holes_before", "holes_after", "auto_closed"]
        `shouldBe` map (toJSON :: Int -> Value) [4, 2, 1, 1]
      field "tactic" (eventJson (evs !! 3)) `shouldBe` toJSON ("decide" :: Text)
      field "counterexample" (eventJson (evs !! 4)) `shouldBe` toJSON ("x := 0\ny := 1" :: Text)
      field "reason" (eventJson (evs !! 5)) `shouldBe` toJSON ("server died" :: Text)
      map eventLine evs
        `shouldBe` [ "00:00:01  AUTO closed a: rfl; omega (2 s)"
                   , "00:00:02  auto missed b (60 s)"
                   , "00:00:03  #4 c: repaired, holes 2 -> 1 (auto closed 1)"
                   , "00:00:04  DISPROVED d (negation by decide)"
                   , "00:00:05  suspect e: counterexample x := 0; y := 1"
                   , "00:00:06  auto off: server died"
                   , "00:00:07  auto timeout f (repair)" ]
  where
    isOff = \case
      AutoOff {} -> True
      _ -> False
    isLaunch = \case
      Launched {} -> True
      _ -> False
    autoOrLaunch = \case
      AutoMissed _ g _ -> Just ("auto " <> g)
      AutoClosed _ g _ _ -> Just ("auto " <> g)
      Launched _ _ g _ -> Just ("launch " <> g)
      _ -> Nothing
    toJSONObj :: [(Text, Text)] -> Value
    toJSONObj kvs = Object (KM.fromList [("x", Object (KM.fromList [(K.fromText k, String v) | (k, v) <- kvs]))])
