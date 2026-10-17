module Fisher.Search.ReportSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Fisher.Search.Controller (Stop (..), StopReason (..))
import Fisher.Search.Golden
import Fisher.Search.Graph
import Fisher.Search.GraphRun
import Fisher.Search.Report
import Fisher.Search.Scheduler (AttemptRecord (..), GoalState (..), GoalStatus (..), SearchReport (..), plainRecord)

-- | Roots t (1) and u (2). t splits into l1 (3, proved) and l2 (4, open,
-- its own split rejected); u's split shares l2; u's first split failed.
small :: Graph
small =
  let (t, g1) = addRoot "t" "kt" emptyGraph
      (u, g2) = addRoot "u" "ku" g1
      (s1, g3) = proposeSplit t g2
      g4 = acc s1 [("l1", "k1"), ("l2", "k2")] g3
      g5 = attemptDone t (markProved 3 g4)
      (s2, g6) = proposeSplit 4 g5
      g7 = rejectSplit s2 "too hard" g6
      (s3, g8) = proposeSplit u g7
      g9 = failSplit s3 "the sketch did not compile" g8
      (s4, g10) = proposeSplit u g9
  in acc s4 [("l2", "k2")] g10
  where acc s ls g = either (error . T.unpack) fst (acceptSplit (const True) s ls g)

-- | Every node proved.
finished :: Graph
finished = markProved 1 (markProved 2 (markProved 4 small))

boardsOf :: [(NodeId, Text)]
boardsOf = [(1, "# Plan for t"), (2, "# Plan for u")]

encodeT :: Value -> Text
encodeT = TE.decodeUtf8 . BL.toStrict . A.encode

openBase, provedBase :: Text
openBase = T.unlines
  [ "lemma l1 : 1 = 1 := by rfl", "", "lemma l2 : 2 = 2 := by sorry", "", "theorem t : True := by sorry", "", "theorem u : True := by sorry" ]
provedBase = T.unlines
  [ "lemma l1 : 1 = 1 := by rfl", "", "lemma l2 : 2 = 2 := by rfl", "", "theorem t : True := by trivial", "", "theorem u : True := by trivial" ]

report :: GraphReport
report = GraphReport
  { grGraph = small
  , grWhiteboards = boardsOf
  , grDollars = Map.fromList [(Worker, 1.25), (Planner, 0.5)]
  , grElapsed = 61.27
  , grStopped = Just "dollars"
  , grBase = openBase
  , grClosedRoots = 0
  , grAttempts = [plainRecord 1 "t" "gauss" "rejected: no" 0 12.34 Nothing]
  , grAutoRan = False, grAutoOff = Nothing, grRetrieval = Nothing, grControl = False, grEscalations = 0 }

-- | A flat run on two tiers: t stopped on tier 0, then closed on tier 1
-- (an escalation); u closed on tier 0; one attempt cancelled.
flat :: Bool -> SearchReport
flat control = SearchReport
  { srGoals = [("t", goal [0, 1] (Just (2, "x"))), ("u", goal [0] (Just (3, "x")))]
  , srAttempts =
      [ rec 1 "t" "rejected: no" (Just 0) (Just stop) (Just 0.42)
      , rec 2 "t" "closed" (Just 1) Nothing (Just 1.256)
      , rec 3 "u" "closed" (Just 0) Nothing (Just 0.1)
      , rec 4 "u" "cancelled: the goal closed" (Just 0) Nothing Nothing ]
  , srElapsed = 60, srClosed = 2, srBudgetSpent = False, srBase = ""
  , srAuto = False, srRepairs = 0, srAutoRan = False, srAutoOff = Nothing, srRetrieval = Nothing
  , srControl = control, srEscalations = 1 }
  where
    stop = Stop StallTurns 8 0.42
    goal ts by = GoalState
      { gsStatus = Closed, gsTried = ["x"], gsRunning = [], gsBy = by, gsAt = Just 10, gsClosedBy = Nothing
      , gsPartial = Nothing, gsDisproof = Nothing, gsDisproveTried = False, gsStall = Nothing, gsTiers = ts }
    rec i g o k st d = (plainRecord i g "x" o 0 1 Nothing)
      { arModel = fmap (["example/model-a", "example/model-b"] !!) k, arTierOf = k, arStopped = st, arCost = d }

at :: Text -> Value -> Value
at k = \case
  Object o -> fromMaybe Null (KM.lookup (K.fromText k) o)
  _ -> Null

keys :: Value -> [Text]
keys = \case
  Object o -> sort (map K.toText (KM.keys o))
  _ -> []

list :: Value -> [Value]
list = \case
  Array xs -> foldr (:) [] xs
  _ -> []

spec :: Spec
spec = do
  describe "reportJson with the controller" $ do
    let j = reportJson [] (flat True)
        u = reportJson [] (flat False)
    it "per attempt: tier, model, stopped, dollars" $ do
      map (at "tier") (list (at "attempts" j)) `shouldBe` [A.toJSON (0 :: Int), A.toJSON (1 :: Int), A.toJSON (0 :: Int), A.toJSON (0 :: Int)]
      map (at "model") (list (at "attempts" j))
        `shouldBe` map (A.toJSON @Text) ["example/model-a", "example/model-b", "example/model-a", "example/model-a"]
      map (at "stopped") (list (at "attempts" j)) `shouldBe` [String "turns", Null, Null, Null]
      map (at "dollars") (list (at "attempts" j)) `shouldBe` [A.toJSON (0.42 :: Double), A.toJSON (1.256 :: Double), A.toJSON (0.1 :: Double), Null]
    it "per goal: the tiers used" $
      map (at "tiers_used") (list (at "goals" j)) `shouldBe` [A.toJSON [0, 1 :: Int], A.toJSON [0 :: Int]]
    it "totals: dollars by tier, stops, escalations" $ do
      at "dollars_by_tier" j `shouldBe` object ["0" .= (0.52 :: Double), "1" .= (1.26 :: Double)]
      at "stopped_total" j `shouldBe` A.toJSON (1 :: Int)
      at "escalations" j `shouldBe` A.toJSON (1 :: Int)
    it "an uncontrolled report has none of these keys" $ do
      keys u `shouldBe` sort ["elapsed_s", "goals_total", "goals_closed", "closed_per_hour", "budget_spent", "goals", "backends", "attempts"]
      map keys (list (at "goals" u)) `shouldBe` replicate 2 (sort ["goal", "status", "attempts", "by_attempt", "by_backend", "closed_at_s"])
      map keys (list (at "attempts" u)) `shouldBe` replicate 4 (sort ["attempt", "goal", "backend", "outcome", "started_s", "ended_s", "stats"])
  describe "graphJson" $
    it "nodes, splits, roots and whiteboards" $ golden "graph.json" (encodeT (graphJson small boardsOf) <> "\n")
  describe "graphTree" $ do
    it "t: splits, a rejected one, statuses" $ golden "tree-t.txt" (T.unlines (graphTree small 1))
    it "u: a failed split, then the shared lemma" $ golden "tree-u.txt" (T.unlines (graphTree small 2))
    it "a node seen twice in one tree is printed once with its subtree" $ do
      let (r, g1) = addRoot "r" "kr" emptyGraph
          (s1, g2) = proposeSplit r g1
          g3 = either (error . T.unpack) fst (acceptSplit (const True) s1 [("a", "ka")] g2)
          (s2, g4) = proposeSplit 2 g3
          (s3, g5) = proposeSplit r g4
          g6 = either (error . T.unpack) fst (acceptSplit (const True) s2 [("b", "kb")] g5)
          g7 = either (error . T.unpack) fst (acceptSplit (const True) s3 [("b", "kb")] g6)
      graphTree g7 1 `shouldBe`
        [ "r: open", "  split 1: accepted", "    a: open", "      split 2: accepted", "        b: open"
        , "  split 3: accepted", "    b: open (shared, see above)" ]
  describe "graphReportJson" $
    it "extra fields, totals, dollars by role, the tree per root, attempts" $
      golden "report.json" (encodeT (graphReportJson ["mode" .= ("graph" :: Text)] report) <> "\n")
  describe "graphEventJson" $ do
    let ev = encodeT . graphEventJson
    it "NodeAdded" $ ev (NodeAdded 1.04 3 "l1" 1) `shouldBe` encodeT (object ["event" .= ("node added" :: Text), "t" .= (1.0 :: Double), "node" .= (3 :: Int), "name" .= ("l1" :: Text), "depth" .= (1 :: Int)])
    it "NodeShared" $ ev (NodeShared 2 4 3) `shouldBe` encodeT (object ["event" .= ("node shared" :: Text), "t" .= (2 :: Double), "node" .= (4 :: Int), "split" .= (3 :: Int)])
    it "NodeReused" $ ev (NodeReused 2 4 3) `shouldBe` encodeT (object ["event" .= ("node reused" :: Text), "t" .= (2 :: Double), "node" .= (4 :: Int), "split" .= (3 :: Int)])
    it "SplitProposed" $ ev (SplitProposed 2 3 1) `shouldBe` encodeT (object ["event" .= ("split proposed" :: Text), "t" .= (2 :: Double), "split" .= (3 :: Int), "node" .= (1 :: Int)])
    it "SplitRejected" $ ev (SplitRejected 2 3 "why") `shouldBe` encodeT (object ["event" .= ("split rejected" :: Text), "t" .= (2 :: Double), "split" .= (3 :: Int), "reason" .= ("why" :: Text)])
    it "SplitAccepted" $ ev (SplitAccepted 2 3 [4, 5]) `shouldBe` encodeT (object ["event" .= ("split accepted" :: Text), "t" .= (2 :: Double), "split" .= (3 :: Int), "lemmas" .= [4, 5 :: Int]])
    it "SplitFailed" $ ev (SplitFailed 2 3 "why") `shouldBe` encodeT (object ["event" .= ("split failed" :: Text), "t" .= (2 :: Double), "split" .= (3 :: Int), "reason" .= ("why" :: Text)])
    it "NodeProved" $ ev (NodeProved 2 4) `shouldBe` encodeT (object ["event" .= ("node proved" :: Text), "t" .= (2 :: Double), "node" .= (4 :: Int)])
    it "NodeFailed" $ ev (NodeFailed 2 4 "why") `shouldBe` encodeT (object ["event" .= ("node failed" :: Text), "t" .= (2 :: Double), "node" .= (4 :: Int), "reason" .= ("why" :: Text)])
    it "FalseLemma" $ ev (FalseLemma 2 4 "n = 0") `shouldBe` encodeT (object ["event" .= ("false lemma" :: Text), "t" .= (2 :: Double), "node" .= (4 :: Int), "claim" .= ("n = 0" :: Text)])
    it "AttemptLaunched" $ ev (AttemptLaunched 2 7 4) `shouldBe` encodeT (object ["event" .= ("launched" :: Text), "t" .= (2 :: Double), "attempt" .= (7 :: Int), "node" .= (4 :: Int)])
    it "AttemptEnded" $ ev (AttemptEnded 2 7 4 "closed") `shouldBe` encodeT (object ["event" .= ("finished" :: Text), "t" .= (2 :: Double), "attempt" .= (7 :: Int), "node" .= (4 :: Int), "outcome" .= ("closed" :: Text)])
    it "RoleCost" $ ev (RoleCost 2 Planner 0.0123) `shouldBe` encodeT (object ["event" .= ("cost" :: Text), "t" .= (2 :: Double), "role" .= ("planner" :: Text), "usd" .= (0.0123 :: Double)])
    it "GraphStopped" $ ev (GraphStopped 2 "time") `shouldBe` encodeT (object ["event" .= ("stopped" :: Text), "t" .= (2 :: Double), "reason" .= ("time" :: Text)])
  describe "graphEventLine" $ do
    it "added" $ graphEventLine (NodeAdded 3661 3 "l1" 1) `shouldBe` "01:01:01  added l1 (node 3, depth 1)"
    it "a root" $ graphEventLine (NodeAdded 0 1 "Foo.t" 0) `shouldBe` "00:00:00  added Foo.t (node 1, depth 0)"
    it "accepted" $ graphEventLine (SplitAccepted 5 2 [3, 4]) `shouldBe` "00:00:05  split 2 accepted: nodes 3, 4"
    it "proved" $ graphEventLine (NodeProved 5 3) `shouldBe` "00:00:05  PROVED node 3"
    it "attempt" $ graphEventLine (AttemptEnded 5 7 3 "rejected: x") `shouldBe` "00:00:05  #7 node 3: rejected: x"
    it "cost" $ graphEventLine (RoleCost 5 Verifier 0.01) `shouldBe` "00:00:05  verifier $0.0100"
    it "stopped" $ graphEventLine (GraphStopped 5 "credit") `shouldBe` "00:00:05  stopped: credit"
  describe "applyAllowed" $ do
    it "every node proved, no sorry" $ applyAllowed finished provedBase `shouldBe` True
    it "an open lemma still sorry in the base" $ do
      applyAllowed small openBase `shouldBe` False
      sorryLemmas small openBase `shouldBe` ["l2"]
    it "a failed lemma still sorry in the base" $ applyAllowed (markFailed 4 "no" small) openBase `shouldBe` False
    it "a failed lemma no longer in the base" $
      applyAllowed (markFailed 4 "no" small) (T.unlines ["lemma l1 : 1 = 1 := by rfl", "", "theorem t : True := by sorry"]) `shouldBe` True
    it "roots left sorry do not block" $
      applyAllowed (markProved 4 small) (T.replace "2 = 2 := by sorry" "2 = 2 := by rfl" openBase) `shouldBe` True
    it "matches full qualified names only" $ do
      let nsBase = T.unlines ["namespace N", "", "lemma l2 : 2 = 2 := by sorry", "", "end N"]
          qualified = small {gNodes = IM.adjust (\n -> n {nName = "N.l2"}) 4 (gNodes small)}
      sorryLemmas small nsBase `shouldBe` []
      sorryLemmas qualified nsBase `shouldBe` ["N.l2"]
      sorryLemmas qualified (T.unlines ["namespace M.N", "", "lemma l2 : 2 = 2 := by sorry", "", "end M.N"]) `shouldBe` []
  describe "graphBackends" $ do
    let refusal = "--graph runs on the native backend only (gauss or fisher)"
    it "defaults to gauss" $ graphBackends Nothing `shouldBe` Right ["gauss"]
    it "gauss" $ graphBackends (Just ["gauss"]) `shouldBe` Right ["gauss"]
    it "fisher is gauss" $ graphBackends (Just ["fisher"]) `shouldBe` Right ["gauss"]
    it "both names, once" $ graphBackends (Just ["fisher", "gauss"]) `shouldBe` Right ["gauss"]
    it "another backend" $ graphBackends (Just ["gauss", "claude-code"]) `shouldBe` Left refusal
    it "none" $ graphBackends (Just []) `shouldBe` Left refusal
