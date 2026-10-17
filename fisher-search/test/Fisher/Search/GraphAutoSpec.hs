module Fisher.Search.GraphAutoSpec (spec) where

import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import qualified Data.IntMap.Strict as IM
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Search.Auto
import Fisher.Search.Check (LeanRun)
import Fisher.Search.FakeLsp
import Fisher.Search.Graph
import Fisher.Search.GraphHarness
import Fisher.Search.GraphRun
import Fisher.Search.Lean
import Fisher.Search.Report (graphEventJson, graphEventLine)
import Fisher.Search.Scheduler (AttemptRecord (..), AutoEnv (..))
import Fisher.Search.Scripted (accept, plan)

auto :: AutoConfig
auto = defaultAutoConfig {acTactics = ["rfl", "simp_all"]}

-- | A run with the automation on, the fake server behaving as given.
runA :: LeanRun -> Fake -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO Out
runA lean f = runGAuto lean (Just (\tr -> AutoEnv auto (fakeLsp tr f)))

closes :: [Text] -> Fake
closes gs = fake0 {fClose = \g _ -> if g `elem` gs then Just Nothing else Nothing}

nodeId :: Out -> Text -> NodeId
nodeId o = nId . nodeBy o

briefsOf :: Out -> Text -> [Text]
briefsOf o g = [b | (x, b) <- oBriefs o, x == g]

startsOf :: Out -> Text -> [Text]
startsOf o g = [s | (x, s) <- oStarts o, x == g]

-- | The position of the first trace entry with the prefix.
firstAt :: Out -> Text -> Maybe Int
firstAt o p = findIndex (p `T.isPrefixOf`) (oTrace o)

lastAt :: Out -> Text -> Maybe Int
lastAt o p = case [i | (i, x) <- zip [0 ..] (oTrace o), p `T.isPrefixOf` x] of
  [] -> Nothing
  is -> Just (last is)

board :: Out -> NodeId -> Text
board o r = fromMaybe "" (lookup r (grWhiteboards (oRep o)))

isAutoEvent :: GraphEvent -> Bool
isAutoEvent = \case
  NodeAutoClosed {} -> True
  NodeRepaired {} -> True
  NodeDisproved {} -> True
  GraphAutoOff {} -> True
  _ -> False

field :: Text -> Value -> Value
field k = \case
  Object o -> fromMaybe Null (KM.lookup (K.fromText k) o)
  _ -> Null

-- | The line numbers a brief lists.
briefLines :: Text -> [Int]
briefLines b = [ read (T.unpack n) | l <- T.lines b, Just r <- [T.stripPrefix "- line " l], let n = T.takeWhile isDigit r, not (T.null n) ]

spec :: Spec
spec = describe "the automation in graph mode" $ do
  it "closes a root by Auto before any planner or worker call, with no model cost" $ do
    o <- runA keyLean (closes ["r1"]) cfg oneRoot [("r1", [ok])] []
    oLaunched o `shouldBe` []
    oAsks o `shouldBe` []
    take 1 (oTrace o) `shouldBe` ["attempts r1"]
    [(i, s) | NodeAutoClosed _ i s <- oEvents o] `shouldBe` [(1, "set_option maxHeartbeats 200000 in rfl")]
    tags o `shouldBe` ["auto closed r1", "proved r1"]
    statusOf o "r1" `shouldBe` NProved
    sum (Map.elems (grDollars (oRep o))) `shouldBe` 0
    grClosedRoots (oRep o) `shouldBe` 1
    fmap dProof (findDecl "r1" (declarations (grBase (oRep o)))) `shouldBe` Just "  set_option maxHeartbeats 200000 in rfl"

  it "runs Auto on each new lemma before any worker; lemmas it closes count for the split" $ do
    o <- runA keyLean (closes ["l1", "l2"]) cfg oneRoot
      [("r1", [bad]), ("l1", [ok]), ("l2", [ok])]
      [say Planner (plan [("l1", "P"), ("l2", "Q")] "theorem r1 : A" "exact combine l1 l2"), say Verifier accept]
    oLaunched o `shouldBe` ["r1"]
    tags o `shouldBe`
      [ "launch r1", "end r1", "proposed r1", "accepted r1", "added l1", "added l2"
      , "auto closed l1", "proved l1", "auto closed l2", "proved l2", "proved r1" ]
    sStatus (splitOf o "r1" 0) `shouldBe` SDone
    grClosedRoots (oRep o) `shouldBe` 1
    T.isInfixOf "sorry" (grBase (oRep o)) `shouldBe` False
    -- the lemmas' Auto comes after the verifier, and nothing follows it on the model side
    (>) <$> firstAt o "attempts l1" <*> lastAt o "ask verifier" `shouldBe` Just True
    count "worker r1" (oTrace o) `shouldBe` 1
    length (filter ("worker" `T.isPrefixOf`) (oTrace o)) `shouldBe` 1

  it "keeps the server and the checker apart: Auto takes the checker lock" $ do
    o <- runA keyLean (closes ["l", "m"]) {fDelay = \m -> if m == "attempts" then 300 else 0} cfg twoRoots
      [("r1", [bad]), ("r2", [bad])]
      [ sayTo Planner (toNode "r1") (plan [("l", "P")] "theorem r1 : A" "exact use l")
      , Reply Planner (Just (toNode "r2")) (Right (plan [("m", "Q")] "theorem r2 : B" "exact use m", 0)) 150
      , say Verifier accept, say Verifier accept ]
    oOverlaps o `shouldBe` 0
    grClosedRoots (oRep o) `shouldBe` 2
    [nm | NodeAutoClosed _ i _ <- oEvents o, let nm = nName (nodeAt o i)] `shouldMatchList` ["l", "m"]

  it "disproves a new lemma Auto misses at once: a certified negation fails it and its split" $ do
    o <- runA keyLean fake0 {fSuspects = ["l"]} cfg {gcMaxSplits = 1} oneRoot
      [("r1", [bad]), ("l", [ok])]
      [say Planner (plan [("l", "P")] "theorem r1 : A" "exact use l"), say Verifier accept]
    let l = nodeId o "l"
    [(i, k, x) | NodeDisproved _ i k x <- oEvents o] `shouldBe` [(l, "false", "decide")]
    ["added l", "disproved l false", "failed l", "split failed r1"] `shouldSatisfy` (`infixOf` tags o)
    statusOf o "l" `shouldBe` NFailed "false: the negation is proved by decide"
    sStatus (splitOf o "r1" 0) `shouldBe` SFailed "lemma l failed"
    oLaunched o `shouldBe` ["r1"]
    count "plausible l" (oTrace o) `shouldBe` 1
    statusOf o "r1" `shouldBe` NFailed "nothing left to try"

  it "writes a counterexample it cannot certify to the root's whiteboard, and disproves a node once" $ do
    o <- runA (noCertifyOn keyLean) fake0 {fSuspects = ["l"]} cfg {gcMaxSplits = 1, gcMaxDepth = 1} oneRoot
      [("r1", [bad]), ("l", [bad, bad])]
      [say Planner (plan [("l", "P")] "theorem r1 : A" "exact use l"), say Verifier accept]
    let l = nodeId o "l"
    [(i, k, x) | NodeDisproved _ i k x <- oEvents o] `shouldBe` [(l, "suspect", "x := 0")]
    board o 1 `shouldSatisfy` T.isInfixOf "l: SUSPECT: plausible found a counterexample: x := 0"
    -- the lemma goes on: its workers read the counterexample on the whiteboard
    length (briefsOf o "l") `shouldBe` 2
    briefsOf o "l" `shouldSatisfy` all (T.isInfixOf "x := 0")
    -- once per node: not again after its second failure
    count "plausible l" (oTrace o) `shouldBe` 1
    (>) <$> firstAt o "worker l" <*> firstAt o "plausible l" `shouldBe` Just True

  it "repairs a failed attempt into the node's next start and brief" $ do
    o <- runA keyLean fake0 cfg {gcMaxDepth = 0} oneRoot
      [("r1", [w "intro x\n  exact BAD", ok])] []
    [(a, i, b, af) | NodeRepaired _ a i b af <- oEvents o] `shouldBe` [(1, 1, 1, 1)]
    case (startsOf o "r1", briefsOf o "r1") of
      ([_, start2], [brief1, brief2]) -> do
        fmap dProof (findDecl "r1" (declarations start2)) `shouldBe` Just "  intro x\n  sorry"
        brief1 `shouldSatisfy` (not . T.isInfixOf "Start from this partial proof")
        brief2 `shouldSatisfy` T.isInfixOf "Start from this partial proof; fill these holes:"
        briefLines brief2 `shouldBe` [5]
        brief2 `shouldSatisfy` T.isInfixOf "goal at line 5"
        -- the worker's own brief comes first
        brief2 `shouldSatisfy` ("This is an unattended run" `T.isPrefixOf`)
      other -> expectationFailure ("attempts on r1: " <> show (length (fst other)))
    fmap (\(_, ag, _) -> fmap dProof (findDecl "r1" (declarations ag))) (nPartial (nodeBy o "r1"))
      `shouldBe` Just (Just "  intro x\n  sorry")
    statusOf o "r1" `shouldBe` NProved
    count "plausible r1" (oTrace o) `shouldBe` 0

  it "closes a node at once when Auto fills every hole Repair left" $ do
    o <- runA keyLean fake0 {fClose = \_ p -> if "intro x" `T.isInfixOf` p then Just Nothing else Nothing} cfg {gcMaxDepth = 0} oneRoot
      [("r1", [w "intro x\n  exact BAD"])] []
    [(b, af) | NodeRepaired _ _ _ b af <- oEvents o] `shouldBe` [(1, 0)]
    oLaunched o `shouldBe` ["r1"]
    statusOf o "r1" `shouldBe` NProved
    nAttempts (nodeBy o "r1") `shouldBe` 0
    [arOutcome a | a <- grAttempts (oRep o)] `shouldBe` ["closed by repair"]
    [x | AttemptEnded _ _ _ x <- oEvents o] `shouldBe` ["closed by repair"]
    precedes (tags o) "end r1" "proved r1" `shouldBe` True

  it "disproves a node once after its second failed attempt" $ do
    o <- runA keyLean fake0 {fSuspects = ["r1"]} cfg {gcMaxDepth = 0} oneRoot
      [("r1", [bad, bad])] []
    [(k, x) | NodeDisproved _ _ k x <- oEvents o] `shouldBe` [("false", "decide")]
    count "plausible r1" (oTrace o) `shouldBe` 1
    -- after the second attempt, not at the pre-pass or after the first
    (>) <$> firstAt o "plausible r1" <*> lastAt o "worker r1" `shouldBe` Just True
    length (oLaunched o) `shouldBe` 2
    statusOf o "r1" `shouldBe` NFailed "false: the negation is proved by decide"
    ["end r1", "disproved r1 false", "failed r1"] `shouldSatisfy` (`infixOf` tags o)
    -- the first failure's note is on the whiteboard; the one the disproof ended is not
    T.count "r1: rejected" (board o 1) `shouldBe` 1

  it "with the automation off, runs today's graph search" $ do
    o <- runGAuto keyLean Nothing cfg oneRoot
      [("r1", [bad]), ("l1", [(w "exact done") {rDelay = 20}]), ("l2", [(w "exact done") {rDelay = 300}])]
      [say Planner (plan [("l1", "P"), ("l2", "Q")] "theorem r1 : A" "exact combine l1 l2"), say Verifier accept]
    tags o `shouldBe`
      [ "launch r1", "end r1", "proposed r1", "accepted r1", "added l1", "added l2"
      , "launch l1", "launch l2", "end l1", "proved l1", "end l2", "proved l2", "proved r1" ]
    filter isAutoEvent (oEvents o) `shouldBe` []
    filter (\x -> not ("worker" `T.isPrefixOf` x || "ask" `T.isPrefixOf` x)) (oTrace o) `shouldBe` []
    all (not . ("Start from this partial proof" `T.isInfixOf`) . snd) (oBriefs o) `shouldBe` True

  it "turns the automation off once when the server fails, and searches as today" $ do
    o <- runA keyLean fake0 {fDown = const True} cfg {gcMaxDepth = 0} oneRoot
      [("r1", [w "intro x\n  exact BAD", ok])] []
    [() | GraphAutoOff {} <- oEvents o] `shouldBe` [()]
    filter (\x -> not ("worker" `T.isPrefixOf` x)) (oTrace o) `shouldBe` ["attempts r1", "close"]
    [() | NodeRepaired {} <- oEvents o] `shouldBe` []
    statusOf o "r1" `shouldBe` NProved
    (grAutoRan (oRep o), grAutoOff (oRep o)) `shouldBe` (False, Just "server died")

  it "logs a root's Auto call cut off at its cap as an auto timeout; the automation ran" $ do
    let slowAuto tr = AutoEnv auto {acSeconds = 0.3} (fakeLsp tr fake0 {fDelay = \m -> if m == "attempts" then 2000 else 0})
    o <- runGAuto keyLean (Just slowAuto) cfg {gcMaxDepth = 0} oneRoot [("r1", [ok])] []
    [(i, step) | NodeAutoTimeout _ i step <- oEvents o] `shouldBe` [(1, "auto")]
    statusOf o "r1" `shouldBe` NProved
    (grAutoRan (oRep o), grAutoOff (oRep o)) `shouldBe` (True, Nothing)

  it "renders the new events" $ do
    let evs = [ NodeAutoClosed 1 3 "rfl; omega", NodeRepaired 2 4 3 2 1
              , NodeDisproved 3 3 "false" "decide", NodeDisproved 4 5 "suspect" "x := 0\ny := 1", GraphAutoOff 5 "server died"
              , NodeAutoTimeout 6 3 "disprove" ]
    map (field "event" . graphEventJson) evs
      `shouldBe` map (toJSON :: Text -> Value) ["auto closed", "repaired", "disproved", "disproved", "auto off", "auto timeout"]
    field "node" (graphEventJson (evs !! 5)) `shouldBe` toJSON (3 :: Int)
    field "step" (graphEventJson (evs !! 5)) `shouldBe` toJSON ("disprove" :: Text)
    field "snippets" (graphEventJson (head evs)) `shouldBe` toJSON ("rfl; omega" :: Text)
    map (\k -> field k (graphEventJson (evs !! 1))) ["attempt", "node", "holes_before", "holes_after"]
      `shouldBe` map (toJSON :: Int -> Value) [4, 3, 2, 1]
    field "tactic" (graphEventJson (evs !! 2)) `shouldBe` toJSON ("decide" :: Text)
    field "result" (graphEventJson (evs !! 3)) `shouldBe` toJSON ("suspect" :: Text)
    field "counterexample" (graphEventJson (evs !! 3)) `shouldBe` toJSON ("x := 0\ny := 1" :: Text)
    field "reason" (graphEventJson (evs !! 4)) `shouldBe` toJSON ("server died" :: Text)
    map graphEventLine evs
      `shouldBe` [ "00:00:01  AUTO closed node 3: rfl; omega"
                 , "00:00:02  #4 node 3: repaired, holes 2 -> 1"
                 , "00:00:03  DISPROVED node 3 (negation by decide)"
                 , "00:00:04  node 5 suspect: counterexample x := 0; y := 1"
                 , "00:00:05  auto off: server died"
                 , "00:00:06  auto timeout node 3 (disprove)" ]
  where
    nodeAt o i = gNodes (grGraph (oRep o)) IM.! i
