-- | The swarm's events and report as JSON, and its events as log lines;
-- the same for the lemma-graph search, with its graph, its tree per root
-- and the rule for writing its merged file back.
module Fisher.Search.Report
  ( eventJson
  , eventLine
  , reportJson
  , autoStateJson
  , statusName
  -- * The lemma graph
  , graphJson
  , graphReportJson
  , graphTree
  , graphEventJson
  , graphEventLine
  , roleName
  , sorryLemmas
  , applyAllowed
  , graphBackends
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as K
import Data.Aeson.Types (Pair)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (find, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)

import Fisher.Compat (canonicalBackendName, nativeBackend)
import Fisher.Search.Child
import Fisher.Search.Controller (Stop (..), stopReasonText)
import Fisher.Search.Disprove (Disproof (..))
import Fisher.Search.Graph
import Fisher.Search.GraphRun (GraphEvent (..), GraphReport (..), Role (..))
import Fisher.Search.Lean (Decl (..), declarations, proofCode)
import Fisher.Search.Retrieval (GoalRetrieval (..), RetrievalReport (..))
import Fisher.Search.Scheduler

statusName :: GoalStatus -> Text
statusName = \case
  Open -> "open"
  Closed -> "closed"
  Waiting -> "waiting"
  GaveUp -> "gave up"
  Disproved -> "disproved"

statsJson :: ChildStats -> Value
statsJson c =
  object
    [ "exit" .= csExit c, "wall_s" .= round1 (csWall c), "turns" .= csTurns c
    , "input_tokens" .= csInputTokens c, "output_tokens" .= csOutputTokens c, "cost_usd" .= csCostUsd c
    , "tools" .= csTools c ]

round1 :: Double -> Double
round1 x = fromIntegral (round (x * 10) :: Integer) / 10

eventJson :: Event -> Value
eventJson = \case
  Launched t i g b -> object ["event" .= ("launched" :: Text), "t" .= round1 t, "attempt" .= i, "goal" .= g, "backend" .= b]
  Finished t i g b o s d ->
    object $ ["event" .= ("finished" :: Text), "t" .= round1 t, "attempt" .= i, "goal" .= g, "backend" .= b, "outcome" .= o, "stats" .= fmap statsJson s]
      <> ["dollars" .= x | Just x <- [d]]
  GoalClosed t g i -> object ["event" .= ("closed" :: Text), "t" .= round1 t, "goal" .= g, "attempt" .= i]
  GoalWaiting t g i -> object ["event" .= ("waiting" :: Text), "t" .= round1 t, "goal" .= g, "attempt" .= i]
  GoalGaveUp t g -> object ["event" .= ("gave up" :: Text), "t" .= round1 t, "goal" .= g]
  AttemptCancelled t i g d -> object (["event" .= ("cancelled" :: Text), "t" .= round1 t, "attempt" .= i, "goal" .= g] <> ["dollars" .= x | Just x <- [d]])
  BudgetSpent t -> object ["event" .= ("budget spent" :: Text), "t" .= round1 t]
  AutoClosed t g snips secs -> object ["event" .= ("auto closed" :: Text), "t" .= round1 t, "goal" .= g, "snippets" .= snips, "seconds" .= round1 secs]
  AutoMissed t g secs -> object ["event" .= ("auto missed" :: Text), "t" .= round1 t, "goal" .= g, "seconds" .= round1 secs]
  Repaired t i g before after closed ->
    object [ "event" .= ("repaired" :: Text), "t" .= round1 t, "attempt" .= i, "goal" .= g
           , "holes_before" .= before, "holes_after" .= after, "auto_closed" .= closed ]
  GoalDisproved t g kind detail ->
    object [ "event" .= ("disproved" :: Text), "t" .= round1 t, "goal" .= g, "result" .= kind
           , (if kind == "false" then "tactic" else "counterexample") .= detail ]
  AutoOff t why -> object ["event" .= ("auto off" :: Text), "t" .= round1 t, "reason" .= why]
  AutoTimeout t g step -> object ["event" .= ("auto timeout" :: Text), "t" .= round1 t, "goal" .= g, "step" .= step]
  IndexBuilt t files decls secs ->
    object ["event" .= ("index built" :: Text), "t" .= round1 t, "files" .= files, "declarations" .= decls, "seconds" .= round1 secs]
  IndexSkipped t file why -> object ["event" .= ("index skipped" :: Text), "t" .= round1 t, "file" .= file, "reason" .= why]
  Facts t g n chars cut -> object ["event" .= ("facts" :: Text), "t" .= round1 t, "goal" .= g, "entries" .= n, "chars" .= chars, "cut" .= cut]
  LibraryOffered t g ls n cut -> object ["event" .= ("library offered" :: Text), "t" .= round1 t, "goal" .= g, "lemmas" .= ls, "notes" .= n, "cut" .= cut]
  LibrarySaved t l n -> object ["event" .= ("library saved" :: Text), "t" .= round1 t, "lemmas" .= l, "notes" .= n]
  LibraryProblem t why -> object ["event" .= ("library problem" :: Text), "t" .= round1 t, "reason" .= why]
  TierChosen t i g k m -> object ["event" .= ("tier chosen" :: Text), "t" .= round1 t, "attempt" .= i, "goal" .= g, "tier" .= k, "model" .= m]
  AttemptStopped t i g why turns d ->
    object ["event" .= ("attempt stopped" :: Text), "t" .= round1 t, "attempt" .= i, "goal" .= g, "reason" .= why, "turns" .= turns, "dollars" .= d]
  Escalated t g a b -> object ["event" .= ("escalated" :: Text), "t" .= round1 t, "goal" .= g, "from" .= a, "to" .= b]
  TierSkipped t m why -> object ["event" .= ("tier skipped" :: Text), "t" .= round1 t, "model" .= m, "reason" .= why]
  LedgerUnread t i why -> object ["event" .= ("ledger unread" :: Text), "t" .= round1 t, "attempt" .= i, "reason" .= why]

clock :: Double -> Text
clock t =
  let s = round t :: Int
      (h, r) = s `divMod` 3600
      (m, sec) = r `divMod` 60
      two n = (if n < 10 then "0" else "") <> T.pack (show n)
   in two h <> ":" <> two m <> ":" <> two sec

eventLine :: Event -> Text
eventLine = \case
  Launched t i g b -> clock t <> "  #" <> tshow i <> " " <> g <> " on " <> b <> ": launched"
  Finished t i g b o s d -> clock t <> "  #" <> tshow i <> " " <> g <> " on " <> b <> ": " <> o
    <> maybe "" (\c -> " (" <> T.pack (showFFloat (Just 0) (csWall c) "") <> " s" <> maybe "" (\n -> ", " <> tshow n <> " turns") (csTurns c) <> ")") s
    <> maybe "" (\x -> " $" <> dollars2 x) d
  GoalClosed t g i -> clock t <> "  CLOSED " <> g <> " (attempt #" <> tshow i <> ")"
  GoalWaiting t g i -> clock t <> "  waiting " <> g <> " (attempt #" <> tshow i <> " uses goals still open)"
  GoalGaveUp t g -> clock t <> "  gave up on " <> g
  AttemptCancelled t i g d -> clock t <> "  #" <> tshow i <> " " <> g <> ": cancelled" <> maybe "" (\x -> " $" <> dollars2 x) d
  BudgetSpent t -> clock t <> "  budget spent"
  AutoClosed t g snips secs -> clock t <> "  AUTO closed " <> g <> ": " <> snips <> " (" <> secs0 secs <> " s)"
  AutoMissed t g secs -> clock t <> "  auto missed " <> g <> " (" <> secs0 secs <> " s)"
  Repaired t i g before after closed -> clock t <> "  #" <> tshow i <> " " <> g <> ": repaired, holes " <> tshow before <> " -> " <> tshow after
    <> " (auto closed " <> tshow closed <> ")"
  GoalDisproved t g kind detail
    | kind == "false" -> clock t <> "  DISPROVED " <> g <> " (negation by " <> detail <> ")"
    | otherwise -> clock t <> "  suspect " <> g <> ": counterexample " <> T.intercalate "; " (T.lines detail)
  AutoOff t why -> clock t <> "  auto off: " <> why
  AutoTimeout t g step -> clock t <> "  auto timeout " <> g <> " (" <> step <> ")"
  IndexBuilt t files decls secs -> clock t <> "  index: " <> tshow decls <> " declarations in " <> tshow files <> " files (" <> secs0 secs <> " s)"
  IndexSkipped t file why -> clock t <> "  index skipped " <> T.pack file <> ": " <> why
  Facts t g n chars cut -> clock t <> "  facts " <> g <> ": " <> tshow n <> " (" <> tshow chars <> " chars)" <> cutOff cut
  LibraryOffered t g ls n cut -> clock t <> "  library " <> g <> ": " <> (if null ls then "no lemmas" else T.intercalate ", " ls) <> ", " <> tshow n <> " notes" <> cutOff cut
  LibrarySaved t l n -> clock t <> "  library saved " <> tshow l <> " lemmas, " <> tshow n <> " notes"
  LibraryProblem t why -> clock t <> "  library: " <> why
  TierChosen t i g k m -> clock t <> "  #" <> tshow i <> " " <> g <> ": tier " <> tshow k <> " (" <> m <> ")"
  AttemptStopped t i g why turns d -> clock t <> "  #" <> tshow i <> " " <> g <> ": stopped (" <> why <> ", " <> tshow turns <> " turns, $" <> dollars2 d <> ")"
  Escalated t g a b -> clock t <> "  escalated " <> g <> ": tier " <> tshow a <> " -> " <> tshow b
  TierSkipped t m why -> clock t <> "  tier skipped " <> m <> ": " <> why
  LedgerUnread t i why -> clock t <> "  #" <> tshow i <> ": ledger unread: " <> why
  where tshow :: Show a => a -> Text
        tshow = T.pack . show
        secs0 x = T.pack (showFFloat (Just 0) x "")
        dollars2 x = T.pack (showFFloat (Just 2) x "")

reportJson :: [Pair] -> SearchReport -> Value
reportJson extra r =
  object $
    extra <>
    [ "elapsed_s" .= round1 (srElapsed r)
    , "goals_total" .= length (srGoals r)
    , "goals_closed" .= srClosed r
    , "closed_per_hour" .= (if srElapsed r > 0 then round1 (fromIntegral (srClosed r) * 3600 / srElapsed r) else 0)
    , "budget_spent" .= srBudgetSpent r ]
    <> autoTotals
    <> retrievalTotals (srRetrieval r)
    <> controlTotals
    <>
    [ "goals" .= [ object ([ "goal" .= g, "status" .= statusName (gsStatus s), "attempts" .= gsTried s
                           , "by_attempt" .= fmap fst (gsBy s), "by_backend" .= fmap snd (gsBy s), "closed_at_s" .= fmap round1 (gsAt s) ]
                           <> autoFields s <> retrievalFields g <> controlFields s)
                 | (g, s) <- srGoals r ]
    , "backends" .= Map.fromListWith (\a b -> zipWith (+) a b)
        [ (arBackend a, [1 :: Int, if any (\(_, s) -> gsBy s == Just (arId a, arBackend a) && gsStatus s == Closed) (srGoals r) then 1 else 0])
        | a <- srAttempts r ]
    , "attempts" .= map (\a -> object (attemptPairs a <> controlAttempt control a)) (srAttempts r)
    ]
  where
    -- with the controller off, nothing
    control = srControl r
    controlFields s = ["tiers_used" .= gsTiers s | control]
    controlTotals = controlTotalPairs control (srAttempts r) (srEscalations r)
    -- with the automation off at the start, today's fields and nothing more
    closedBy s = if gsStatus s == Closed then gsClosedBy s else Nothing
    autoFields s
      | not (srAuto r) = []
      | otherwise =
          ("closedBy" .= closedBy s) : case gsDisproof s of
            Just (DFalse tac) -> ["disproof" .= object ["verdict" .= ("false" :: Text), "tactic" .= tac]]
            Just (DSuspect c) -> ["disproof" .= object ["verdict" .= ("suspect" :: Text), "counterexample" .= c]]
            _ -> []
    count p = length [() | (_, s) <- srGoals r, p s]
    -- with retrieval and the library off, nothing
    retrievalFields g = case srRetrieval r of
      Nothing -> []
      Just rr ->
        let gr = fromMaybe (GoalRetrieval 0 [] []) (lookup g (rrGoals rr))
         in ["facts" .= rgFacts gr, "offered" .= rgOffered gr, "used" .= rgUsed gr]
    autoTotals
      | not (srAuto r) = []
      | otherwise =
          [ "auto_closed" .= count (\s -> closedBy s == Just "auto")
          , "repairs_kept" .= srRepairs r
          , "repaired_then_closed" .= count (\s -> closedBy s == Just "model+repair")
          , "disproved" .= count (\s -> gsStatus s == Disproved) ]

-- | The fields report.json gains whenever the flags asked for the
-- automation: @auto_requested@, @auto_ran@ (false when the server did not
-- start, or went off before any automation step completed) and
-- @auto_off@ (why it went off, or null). Arguments: why the server did not
-- start (Nothing when it started), whether the run kept it on long enough,
-- and why it went off during the run.
autoStateJson :: Maybe Text -> Bool -> Maybe Text -> [Pair]
autoStateJson atStart ran during =
  [ "auto_requested" .= True
  , "auto_ran" .= (maybe ran (const False) atStart)
  , "auto_off" .= maybe during Just atStart ]

-- | The run's retrieval totals: the mean facts per goal retrieved for,
-- the lemmas offered and used, and what was saved to the library.
retrievalTotals :: Maybe RetrievalReport -> [Pair]
retrievalTotals = \case
  Nothing -> []
  Just rr ->
    let gs = map snd (rrGoals rr)
        mean = if null gs then 0 else fromIntegral (sum (map rgFacts gs)) / fromIntegral (length gs) :: Double
     in [ "facts_mean" .= round1 mean
        , "offered_total" .= sum (map (length . rgOffered) gs)
        , "used_total" .= sum (map (length . rgUsed) gs)
        , "saved_lemmas" .= fst (rrSaved rr)
        , "saved_notes" .= snd (rrSaved rr) ]

-- | An attempt's fields with the controller off.
attemptPairs :: AttemptRecord -> [Pair]
attemptPairs a =
  [ "attempt" .= arId a, "goal" .= arGoal a, "backend" .= arBackend a, "outcome" .= arOutcome a
  , "started_s" .= round1 (arStarted a), "ended_s" .= round1 (arEnded a), "stats" .= fmap statsJson (arChild a) ]

-- | An attempt's controller fields; with the controller off, nothing.
controlAttempt :: Bool -> AttemptRecord -> [Pair]
controlAttempt control a
  | not control = []
  | otherwise =
      [ "tier" .= arTierOf a, "model" .= arModel a
      , "stopped" .= fmap (stopReasonText . stReason) (arStopped a), "dollars" .= arCost a ]

-- | A run's controller totals over its attempts and escalations; with the
-- controller off, nothing. @escalations@ counts the escalation events, so
-- also a change of tier made because a tier was skipped.
controlTotalPairs :: Bool -> [AttemptRecord] -> Int -> [Pair]
controlTotalPairs control as escalations
  | not control = []
  | otherwise =
      let tiers = nub [k | a <- as, Just k <- [arTierOf a]]
          spent k = round2 (sum [d | a <- as, arTierOf a == Just k, Just d <- [arCost a]])
       in [ "dollars_by_tier" .= object [K.fromText (T.pack (show k)) .= spent k | k <- tiers]
          , "stopped_total" .= length [() | a <- as, Just _ <- [arStopped a]]
          , "escalations" .= escalations ]

round2 :: Double -> Double
round2 x = fromIntegral (round (x * 100) :: Integer) / 100

-- ---------------------------------------------------------- the lemma graph

roleName :: Role -> Text
roleName = \case
  Worker -> "worker"
  Planner -> "planner"
  Verifier -> "verifier"

nodeStatusName :: NodeStatus -> Text
nodeStatusName = \case
  NOpen -> "open"
  NProved -> "proved"
  NFailed _ -> "failed"

nodeReason :: NodeStatus -> [Pair]
nodeReason = \case
  NFailed why -> ["reason" .= why]
  _ -> []

splitStatusName :: SplitStatus -> Text
splitStatusName = \case
  SProposed -> "proposed"
  SRejected _ -> "rejected"
  SAccepted -> "accepted"
  SFailed _ -> "failed"
  SDone -> "done"

splitReason :: SplitStatus -> [Pair]
splitReason = \case
  SRejected why -> ["reason" .= why]
  SFailed why -> ["reason" .= why]
  _ -> []

-- | The graph as saved in graph.json, with each root's whiteboard.
graphJson :: Graph -> [(NodeId, Text)] -> Value
graphJson g boards =
  object
    [ "roots" .= gRoots g
    , "nodes" .= [ object ([ "id" .= nId n, "name" .= nName n, "key" .= nKey n, "root" .= nRoot n, "depth" .= nDepth n
                           , "status" .= nodeStatusName (nStatus n), "splits" .= nSplits n, "attempts" .= nAttempts n ]
                           <> nodeReason (nStatus n))
                 | n <- IM.elems (gNodes g) ]
    , "splits" .= [ object ([ "id" .= sId s, "parent" .= sParent s, "lemmas" .= sLemmas s, "status" .= splitStatusName (sStatus s) ]
                            <> splitReason (sStatus s))
                  | s <- IM.elems (gSplits g) ]
    , "whiteboards" .= [ object ["root" .= r, "text" .= t] | (r, t) <- boards ]
    ]

-- | Walk a root's tree: each node once (a node met again, as a shared lemma
-- is, gets @again@), each of its splits in the order proposed, and the
-- lemmas of a split in order.
walkTree
  :: Graph -> NodeId
  -> (Int -> Node -> [a] -> a)        -- ^ a node first met: depth, node, its splits
  -> (Int -> Node -> a)               -- ^ a node met again
  -> (Int -> Split -> [a] -> a)       -- ^ a split: depth, split, its lemmas
  -> [a]
walkTree g root onNode again onSplit = fst (node 0 IS.empty root)
  where
    node d seen i = case IM.lookup i (gNodes g) of
      Nothing -> ([], seen)
      Just n
        | IS.member i seen -> ([again d n], seen)
        | otherwise ->
            let (ss, seen') = many (split (d + 1)) (IS.insert i seen) (nSplits n)
             in ([onNode d n ss], seen')
    split d seen sid = case IM.lookup sid (gSplits g) of
      Nothing -> ([], seen)
      Just s ->
        let (ls, seen') = many (node (d + 1)) seen (sLemmas s)
         in ([onSplit d s ls], seen')
    many :: (IntSet -> Int -> ([b], IntSet)) -> IntSet -> [Int] -> ([b], IntSet)
    many f seen0 = foldl (\(acc, sn) i -> let (xs, sn') = f sn i in (acc <> xs, sn')) ([], seen0)

-- | A root's tree as text: each node with its status, each split with its
-- status, two spaces a level.
graphTree :: Graph -> NodeId -> [Text]
graphTree g root = concat (walkTree g root onNode again onSplit)
  where
    ind d = T.replicate (2 * d) " "
    st n = nodeStatusName (nStatus n) <> case nStatus n of
      NFailed why -> ": " <> why
      _ -> ""
    onNode d n ss = (ind d <> nName n <> ": " <> st n) : concat ss
    again d n = [ind d <> nName n <> ": " <> st n <> " (shared, see above)"]
    onSplit d s ls =
      (ind d <> "split " <> T.pack (show (sId s)) <> ": " <> splitStatusName (sStatus s) <> case sStatus s of
          SRejected why -> ": " <> why
          SFailed why -> ": " <> why
          _ -> "")
        : concat ls

-- | A root's tree as JSON.
treeJson :: Graph -> NodeId -> Value
treeJson g root = case walkTree g root onNode again onSplit of
  v : _ -> v
  [] -> object []
  where
    base n = [ "id" .= nId n, "name" .= nName n, "status" .= nodeStatusName (nStatus n) ] <> nodeReason (nStatus n)
    onNode _ n ss = object (base n <> [ "depth" .= nDepth n, "attempts" .= nAttempts n, "splits" .= ss ])
    again _ n = object (base n <> [ "shared" .= True ])
    onSplit _ s ls = object ([ "id" .= sId s, "status" .= splitStatusName (sStatus s) ] <> splitReason (sStatus s) <> [ "lemmas" .= ls ])

round4 :: Double -> Double
round4 x = fromIntegral (round (x * 10000) :: Integer) / 10000

-- | report.json for a graph run: @extra@ first (the command's own fields),
-- then totals, dollars by role, why it stopped, the tree per root and the
-- attempts.
graphReportJson :: [Pair] -> GraphReport -> Value
graphReportJson extra r =
  let g = grGraph r
      nodes = IM.elems (gNodes g)
      dollars = grDollars r
   in object $
        extra <>
        [ "elapsed_s" .= round1 (grElapsed r)
        , "goals_total" .= length (gRoots g)
        , "goals_closed" .= grClosedRoots r
        , "nodes_total" .= length nodes
        , "nodes_proved" .= length [() | n <- nodes, nStatus n == NProved]
        , "splits_total" .= IM.size (gSplits g)
        , "stopped" .= grStopped r
        , "dollars" .= object ([ K.fromText (roleName k) .= round4 (Map.findWithDefault 0 k dollars) | k <- [Worker, Planner, Verifier] ]
                               <> [ "total" .= round4 (sum (Map.elems dollars)) ])
        , "tree" .= map (treeJson g) (gRoots g)
        , "attempts" .= map (\a -> object (attemptPairs a <> controlAttempt (grControl r) a)) (grAttempts r)
        ]
        <> controlTotalPairs (grControl r) (grAttempts r) (grEscalations r)
        <> retrievalTotals (grRetrieval r)
        <> case grRetrieval r of
          Nothing -> []
          Just rr ->
            [ "retrieval" .=
                [ object ["node" .= nodeIdOf name, "name" .= name, "facts" .= rgFacts gr, "offered" .= rgOffered gr, "used" .= rgUsed gr]
                | (name, gr) <- rrGoals rr ] ]
  where
    nodeIdOf name = fmap nId (find ((== name) . nName) (IM.elems (gNodes (grGraph r))))

graphEventJson :: GraphEvent -> Value
graphEventJson = \case
  NodeAdded t i name d -> ev "node added" t ["node" .= i, "name" .= name, "depth" .= d]
  NodeShared t i s -> ev "node shared" t ["node" .= i, "split" .= s]
  NodeReused t i s -> ev "node reused" t ["node" .= i, "split" .= s]
  SplitProposed t s i -> ev "split proposed" t ["split" .= s, "node" .= i]
  SplitRejected t s why -> ev "split rejected" t ["split" .= s, "reason" .= why]
  SplitAccepted t s is -> ev "split accepted" t ["split" .= s, "lemmas" .= is]
  SplitFailed t s why -> ev "split failed" t ["split" .= s, "reason" .= why]
  NodeProved t i -> ev "node proved" t ["node" .= i]
  NodeFailed t i why -> ev "node failed" t ["node" .= i, "reason" .= why]
  FalseLemma t i claim -> ev "false lemma" t ["node" .= i, "claim" .= claim]
  AttemptLaunched t a i -> ev "launched" t ["attempt" .= a, "node" .= i]
  AttemptEnded t a i o -> ev "finished" t ["attempt" .= a, "node" .= i, "outcome" .= o]
  RoleCost t role d -> ev "cost" t ["role" .= roleName role, "usd" .= d]
  GraphStopped t why -> ev "stopped" t ["reason" .= why]
  NodeAutoClosed t i snips -> ev "auto closed" t ["node" .= i, "snippets" .= snips]
  NodeRepaired t a i before after -> ev "repaired" t ["attempt" .= a, "node" .= i, "holes_before" .= before, "holes_after" .= after]
  NodeDisproved t i kind detail ->
    ev "disproved" t ["node" .= i, "result" .= kind, (if kind == "false" then "tactic" else "counterexample") .= detail]
  GraphAutoOff t why -> ev "auto off" t ["reason" .= why]
  NodeAutoTimeout t i step -> ev "auto timeout" t ["node" .= i, "step" .= step]
  NodeFacts t i n chars cut -> ev "facts" t ["node" .= i, "entries" .= n, "chars" .= chars, "cut" .= cut]
  NodeLibraryOffered t i ls n cut -> ev "library offered" t ["node" .= i, "lemmas" .= ls, "notes" .= n, "cut" .= cut]
  GraphRunEvent e -> eventJson e
  NodeTierChosen t a i k m -> ev "tier chosen" t ["attempt" .= a, "node" .= i, "tier" .= k, "model" .= m]
  NodeStopped t a i why turns d -> ev "attempt stopped" t ["attempt" .= a, "node" .= i, "reason" .= why, "turns" .= turns, "dollars" .= d]
  NodeEscalated t i a b -> ev "escalated" t ["node" .= i, "from" .= a, "to" .= b]
  NodeTierSkipped t m why -> ev "tier skipped" t ["model" .= m, "reason" .= why]
  where
    ev :: Text -> Double -> [Pair] -> Value
    ev name t rest = object (["event" .= name, "t" .= round1 t] <> rest)

graphEventLine :: GraphEvent -> Text
graphEventLine (GraphRunEvent e) = eventLine e
graphEventLine e = clock (at e) <> "  " <> case e of
  NodeAdded _ i name d -> "added " <> name <> " (node " <> tshow i <> ", depth " <> tshow d <> ")"
  NodeShared _ i s -> "node " <> tshow i <> " shared by split " <> tshow s
  NodeReused _ i s -> "node " <> tshow i <> " (proved) reused by split " <> tshow s
  SplitProposed _ s i -> "split " <> tshow s <> " proposed for node " <> tshow i
  SplitRejected _ s why -> "split " <> tshow s <> " rejected: " <> why
  SplitAccepted _ s is -> "split " <> tshow s <> " accepted: nodes " <> T.intercalate ", " (map tshow is)
  SplitFailed _ s why -> "split " <> tshow s <> " failed: " <> why
  NodeProved _ i -> "PROVED node " <> tshow i
  NodeFailed _ i why -> "node " <> tshow i <> " failed: " <> why
  FalseLemma _ i claim -> "node " <> tshow i <> " claimed false: " <> claim
  AttemptLaunched _ a i -> "#" <> tshow a <> " node " <> tshow i <> ": launched"
  AttemptEnded _ a i o -> "#" <> tshow a <> " node " <> tshow i <> ": " <> o
  RoleCost _ role d -> roleName role <> " $" <> T.pack (showFFloat (Just 4) d "")
  GraphStopped _ why -> "stopped: " <> why
  NodeAutoClosed _ i snips -> "AUTO closed node " <> tshow i <> ": " <> snips
  NodeRepaired _ a i before after -> "#" <> tshow a <> " node " <> tshow i <> ": repaired, holes " <> tshow before <> " -> " <> tshow after
  NodeDisproved _ i kind detail
    | kind == "false" -> "DISPROVED node " <> tshow i <> " (negation by " <> detail <> ")"
    | otherwise -> "node " <> tshow i <> " suspect: counterexample " <> T.intercalate "; " (T.lines detail)
  GraphAutoOff _ why -> "auto off: " <> why
  NodeAutoTimeout _ i step -> "auto timeout node " <> tshow i <> " (" <> step <> ")"
  NodeFacts _ i n chars cut -> "facts node " <> tshow i <> ": " <> tshow n <> " (" <> tshow chars <> " chars)" <> cutOff cut
  NodeLibraryOffered _ i ls n cut -> "library node " <> tshow i <> ": " <> (if null ls then "no lemmas" else T.intercalate ", " ls) <> ", " <> tshow n <> " notes" <> cutOff cut
  NodeTierChosen _ a i k m -> "#" <> tshow a <> " node " <> tshow i <> ": tier " <> tshow k <> " (" <> m <> ")"
  NodeStopped _ a i why turns d -> "#" <> tshow a <> " node " <> tshow i <> ": stopped (" <> why <> ", " <> tshow turns <> " turns, $" <> T.pack (showFFloat (Just 2) d "") <> ")"
  NodeEscalated _ i a b -> "escalated node " <> tshow i <> ": tier " <> tshow a <> " -> " <> tshow b
  NodeTierSkipped _ m why -> "tier skipped " <> m <> ": " <> why
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show
    at = \case
      NodeAdded t _ _ _ -> t
      NodeShared t _ _ -> t
      NodeReused t _ _ -> t
      SplitProposed t _ _ -> t
      SplitRejected t _ _ -> t
      SplitAccepted t _ _ -> t
      SplitFailed t _ _ -> t
      NodeProved t _ -> t
      NodeFailed t _ _ -> t
      FalseLemma t _ _ -> t
      AttemptLaunched t _ _ -> t
      AttemptEnded t _ _ _ -> t
      RoleCost t _ _ -> t
      GraphStopped t _ -> t
      NodeAutoClosed t _ _ -> t
      NodeRepaired t _ _ _ _ -> t
      NodeDisproved t _ _ _ -> t
      GraphAutoOff t _ -> t
      NodeAutoTimeout t _ _ -> t
      NodeFacts t _ _ _ _ -> t
      NodeLibraryOffered t _ _ _ _ -> t
      NodeTierChosen t _ _ _ _ -> t
      NodeStopped t _ _ _ _ _ -> t
      NodeEscalated t _ _ _ -> t
      NodeTierSkipped t _ _ -> t
      GraphRunEvent _ -> 0

-- | The lemmas from splits (every node below a root) not proved whose
-- declaration in the base still has a @sorry@, in node order. Node names
-- are fully qualified, as the base's declarations are.
sorryLemmas :: Graph -> Text -> [Text]
sorryLemmas g base =
  [ nName n
  | n <- IM.elems (gNodes g), nDepth n > 0, nStatus n /= NProved
  , Just d <- [find ((== nName n) . dName) decls]
  , any (T.isInfixOf "sorry") (proofCode d) ]
  where
    decls = declarations base

-- | @--apply@ writes the merged base back only when no lemma from a split is
-- left with its @sorry@.
applyAllowed :: Graph -> Text -> Bool
applyAllowed g base = null (sorryLemmas g base)

-- | The backends of a graph run: the native one, given as fisher (or by
-- its old name, gauss), and run and reported as fisher.
graphBackends :: Maybe [Text] -> Either Text [Text]
graphBackends = \case
  Nothing -> Right [nativeBackend]
  Just bs
    | not (null bs), all ((== nativeBackend) . canonicalBackendName) bs -> Right [nativeBackend]
    | otherwise -> Left "--graph runs on the native backend only (fisher)"

-- | The mark on a retrieval event's line when the retrieval was cut off.
cutOff :: Bool -> Text
cutOff cut = if cut then " (cut off)" else ""
