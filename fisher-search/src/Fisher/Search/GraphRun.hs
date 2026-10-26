-- | The lemma-graph search: today's attempts on a file's open goals, and for
-- a goal its attempt cannot close, a split into lemmas from the planner,
-- reviewed by the verifier, merged into the base and worked on in turn.
--
-- The pure graph ("Fisher.Search.Graph") picks the next action; this loop
-- runs it. Worker attempts take a slot; planner and verifier calls run as
-- jobs that take none. A node has one job at a time. Every result is
-- applied on the loop's thread: proofs are spliced and checked as the
-- scheduler checks them, a split's sketch is checked again when the base
-- has moved under it, and after a merge that proves something the nodes
-- waiting on others (and the parents of splits now proved) are checked
-- again until nothing more closes. The run ends when nothing runs and
-- nothing can start, at the time budget, at the dollar cap, or when the
-- model says there is no credit.
--
-- With automation on ('seAuto'), each root before the first planner or
-- worker call, and each new lemma of an accepted split before any worker,
-- gets Auto's tactic portfolio; a fill the kernel closes the node with is
-- merged and the node is proved as a worker's proof would prove it. A new
-- lemma Auto misses goes to Disprove at once, and any node goes there
-- after its second failed attempt (once per node): a certified negation
-- fails the node as a confirmed FALSE does, a counterexample goes on the
-- root's whiteboard. A worker's failed attempt goes to Repair, and a
-- partial proof it keeps becomes the node's next start. All of it runs
-- under the checker lock. If the language server fails, the automation is
-- off for the rest of the run.
--
-- With retrieval on (the search env's 'seRetrieval'), each node gets its
-- facts and its offer from the library once, before its first worker or
-- planner call, and both carry them. With the library on, the run's new
-- sorry-free lemmas (helpers and proved split lemmas) and its failure
-- notes are saved to it at the end.
module Fisher.Search.GraphRun
  ( Role (..)
  , AskError (..)
  , GraphConfig (..)
  , defaultGraphConfig
  , GraphEvent (..)
  , GraphEnv (..)
  , GraphReport (..)
  , plannerModel
  , runGraph
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Async, async, cancel, waitAnyCatch)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (SomeException, displayException, toException)
import Control.Monad (forM, forM_, join, unless, void, when)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (foldl', nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Fisher.Search.Auto (AutoConfig (..), AutoFail (..), autoGoal)
import Fisher.Search.Check (Verdict (..))
import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Controller (Health, emptyHealth, modelFailureText, noteResult, stopReasonText, tierFor)
import qualified Fisher.Search.Controller as Ctl
import Fisher.Search.Disprove (Disproof (..), DisproveInfo (..), DisproveSource (..), disprove, sourceName)
import Fisher.Search.Graph
import Fisher.Search.GraphJobs
import Fisher.Search.Headless (nativeBackend)
import Fisher.Search.Lean
import qualified Fisher.Search.Repair as R
import Fisher.Search.Retrieval
import Fisher.Search.Review (SplitVerdict (..))
import Fisher.Search.Scheduler
  ( AttemptRecord (..), plainRecord, AttemptResult (..), AttemptSpec (..), AutoEnv (..), AutoTrack (..), Event (..), SearchEnv (..)
  , autoStep, keptPartial, noAutoTrack, partialStart, priceCancelled, stallNote, trackRan )
import Fisher.Search.Sketch
import Fisher.Search.Split (Message)
import Fisher.Search.Verify
import Fisher.Search.Whiteboard

data GraphConfig = GraphConfig
  { gcSlots :: Int
  , gcMaxDepth :: Int
  , gcMaxSplits :: Int
  , gcCapUsd :: Double
  , gcBudget :: Double
  -- ^ seconds of wall clock for the whole search
  , gcTiers :: [Text]
  -- ^ the workers' model tiers, cheapest first; empty is the backend's own model
  , gcControl :: Bool
  -- ^ the compute controller is on
  }
  deriving (Eq, Show)

defaultGraphConfig :: GraphConfig
defaultGraphConfig = GraphConfig
  { gcSlots = 4, gcMaxDepth = 3, gcMaxSplits = 2, gcCapUsd = 25, gcBudget = 7200, gcTiers = [], gcControl = False }

-- | The planner's and the verifier's model: the last tier when tiers are
-- set, else the model given (@--model@).
plannerModel :: [Text] -> Maybe Text -> Maybe Text
plannerModel tiers m
  | null tiers = m
  | otherwise = Just (last tiers)

data GraphEvent
  = NodeAdded Double NodeId Text Int
  -- ^ time, node, name, depth
  | NodeShared Double NodeId SplitId
  | NodeReused Double NodeId SplitId
  | SplitProposed Double SplitId NodeId
  | SplitRejected Double SplitId Text
  | SplitAccepted Double SplitId [NodeId]
  | SplitFailed Double SplitId Text
  | NodeProved Double NodeId
  | NodeFailed Double NodeId Text
  | FalseLemma Double NodeId Text
  | AttemptLaunched Double Int NodeId
  | AttemptEnded Double Int NodeId Text
  | RoleCost Double Role Double
  | GraphStopped Double Text
  | NodeAutoClosed Double NodeId Text
  -- ^ time, node, the snippets joined by @; @
  | NodeRepaired Double Int NodeId Int Int
  -- ^ time, attempt, node, holes before Auto, holes after
  | NodeDisproved Double NodeId Text Text Text (Maybe Text)
  -- ^ time, node, @false@ or @suspect@, the negation's tactic or the
  -- counterexample, the source (@plausible@ or @smt@), and for a negation
  -- proved by a model's witness tactic the model
  | NodeDisproveRan Double NodeId Text Text
  -- ^ time, node, the source, its result (as 'DisproveRan')
  | GraphAutoOff Double Text
  -- ^ time, why
  | NodeAutoTimeout Double NodeId Text
  -- ^ time, node, the step whose Lsp call (or the step itself) was cut
  -- off at its cap: @auto@, @repair@ or @disprove@
  | NodeFacts Double NodeId Int Int Bool
  -- ^ time, node, facts, characters of the facts section
  | NodeLibraryOffered Double NodeId [Text] Int Bool
  -- ^ time, node, the lemmas offered, the notes offered
  | GraphRunEvent Event
  -- ^ a run-level retrieval event: 'IndexBuilt', 'IndexSkipped',
  -- 'LibrarySaved' or 'LibraryProblem'
  | NodeTierChosen Double Int NodeId Int Text
  -- ^ time, attempt, node, tier index, model
  | NodeStopped Double Int NodeId Text Int Double
  -- ^ time, attempt, node, reason (@turns@ or @dollars@), turns, dollars
  | NodeEscalated Double NodeId Int Int
  -- ^ time, node, from tier, to tier
  | NodeTierSkipped Double Text Text
  -- ^ time, model, reason
  deriving (Eq, Show)

data GraphEnv = GraphEnv
  { geSearch :: SearchEnv
  -- ^ its seLog is not used
  , geAsk :: Role -> [Message] -> IO (Either AskError (Text, Double))
  , geLogG :: GraphEvent -> IO ()
  , geSave :: Graph -> [(NodeId, Text)] -> IO ()
  -- ^ the graph and each root's whiteboard, after every job
  }

data GraphReport = GraphReport
  { grGraph :: Graph
  , grWhiteboards :: [(NodeId, Text)]
  , grDollars :: Map.Map Role Double
  , grElapsed :: Double
  , grStopped :: Maybe Text
  -- ^ "time", "dollars" or "credit" when the run was cut short
  , grBase :: Text
  , grClosedRoots :: Int
  , grAttempts :: [AttemptRecord]
  , grAutoRan :: Bool
  -- ^ the automation was on at the start and did not go off before any
  -- of its steps completed
  , grAutoOff :: Maybe Text
  -- ^ why the automation went off during the run
  , grRetrieval :: Maybe RetrievalReport
  -- ^ per node (by name) and what was saved; Nothing with retrieval and
  -- the library off
  , grControl :: Bool
  -- ^ the compute controller was on
  , grEscalations :: Int
  -- ^ the 'NodeEscalated' events logged
  }
  deriving (Eq, Show)

-- | The native backend the workers run on.
workerBackend :: Text
workerBackend = nativeBackend

data Kind
  = KWorker AttemptSpec Double
  -- ^ the attempt and when it started
  | KPlan SplitId
  | KReview SplitId Plan
  | KFalse Text

data Job = Job {jNode :: NodeId, jKind :: Kind}

data Out
  = OutWorker AttemptResult
  | OutPlan (Asked Plan)
  | OutReview (Asked SplitVerdict)
  | OutFalse (Asked (Bool, Text))

data St = St
  { stGraph :: Graph
  , stBase :: Text
  , stTypes :: Map.Map Text Text
  -- ^ each node's printed type, by name: what its proof must have
  , stWaiting :: IntSet
  , stRunning :: [(Async Out, Job)]
  , stFailures :: IM.IntMap [Text]
  -- ^ whiteboard failure lines per root, oldest first
  , stNotes :: IM.IntMap [Text]
  -- ^ notes of each node's failed attempts, for the planner
  , stDollars :: Map.Map Role Double
  , stCapHit :: Bool
  -- ^ something could not start for the dollar cap (the stop reason only
  -- when some root is still open at the end)
  , stNext :: Int
  , stRecords :: [AttemptRecord]
  , stDisproveTried :: IntSet
  -- ^ nodes Disprove has run on
  , stRetrieved :: IM.IntMap Retrieved
  -- ^ each node's retrieval, made before its first worker or planner call
  , stHelpers :: IM.IntMap [Text]
  -- ^ the helpers merged with each node's kept proofs
  , stRetried :: IntSet
  -- ^ nodes whose cut-off retrieval was made again
  , stHealth :: Health
  -- ^ the tiers' health, run-wide
  , stLastTier :: IM.IntMap Int
  -- ^ each node's last chosen tier
  , stModelErrors :: IM.IntMap Int
  -- ^ model errors per node kept out of its attempt count
  , stStall :: IM.IntMap Text
  -- ^ the note on a node's stopped worker, for its next worker's brief
  , stEscalations :: Int
  -- ^ 'NodeEscalated' events logged
  , stStallUsed :: IM.IntMap Text
  -- ^ the stall note each running worker took into its brief, by attempt
  }

data Ctx = Ctx
  { cCfg :: GraphConfig, cEnv :: GraphEnv, cCk :: Checker, cRef :: IORef St, cT0 :: Double
  , cAuto :: IORef (Maybe AutoEnv)
  -- ^ the automation, Nothing once off
  , cTrack :: IORef AutoTrack
  , cRetrieve :: Maybe (Query -> IO Retrieved)
  , cBase0 :: Text
  -- ^ the file the run started from
  , cHarvest :: IO (Int, Int)
  -- ^ the library's harvest, run once ('harvestRun')
  }

-- | Search the open goals of @base0@ as a lemma graph.
runGraph :: GraphConfig -> GraphEnv -> Text -> IO (Either Text GraphReport)
runGraph cfg env base0 = do
  let se = geSearch env
      order = map dName (openGoals base0)
  types0 <- goalTypes (seLean se) (seRoot se) (seRel se) base0 order
  case types0 of
    Left e -> pure (Left e)
    Right types -> do
      lock <- newMVar ()
      t0 <- seNow se
      let g0 = foldl' (\g n -> snd (addRoot n (typeKey n (Map.findWithDefault "" n types)) g)) emptyGraph order
      ref <- newIORef St
        { stGraph = g0, stBase = base0, stTypes = types, stWaiting = IS.empty, stRunning = [], stFailures = IM.empty
        , stNotes = IM.empty, stDollars = Map.empty, stCapHit = False, stNext = 1, stRecords = [], stDisproveTried = IS.empty
        , stRetrieved = IM.empty, stHelpers = IM.empty, stRetried = IS.empty
        , stHealth = emptyHealth, stLastTier = IM.empty, stModelErrors = IM.empty, stStall = IM.empty, stEscalations = 0
        , stStallUsed = IM.empty }
      autoRef <- newIORef (seAuto se)
      track <- newIORef noAutoTrack
      -- one retriever for the run: its corpora are shared by every node
      retrieve <- traverse (\re -> newRetriever re (seRel se)) (seRetrieval se)
      harvestRef <- newIORef (pure (0, 0))
      let c = Ctx cfg env (Checker (seLean se) (sePolicy se) (seRoot se) (seRel se) lock) ref t0 autoRef track retrieve base0
                (join (readIORef harvestRef))
      writeIORef harvestRef =<< once (harvestRun c)
      forM_ (IM.elems (gNodes g0)) $ \n -> logG c (\t -> NodeAdded t (nId n) (nName n) 0)
      -- on an exception (Ctrl-C, SIGTERM): the harvest with the base as it
      -- is then, the jobs stopped, and the exception goes on
      let stopJobs = getSt c >>= mapM_ (cancel . fst) . stRunning
      harvestOnException (void (cHarvest c)) stopJobs $ do
        -- the pre-pass: Auto at each root before any model call
        forM_ (gRoots g0) (autoNode c)
        Right <$> loop c

-- * Small helpers

elapsed :: Ctx -> IO Double
elapsed c = subtract (cT0 c) <$> seNow (geSearch (cEnv c))

logG :: Ctx -> (Double -> GraphEvent) -> IO ()
logG c f = elapsed c >>= geLogG (cEnv c) . f

getSt :: Ctx -> IO St
getSt = readIORef . cRef

modify :: Ctx -> (St -> St) -> IO ()
modify c = modifyIORef' (cRef c)

modGraph :: Ctx -> (Graph -> Graph) -> IO ()
modGraph c f = modify c (\s -> s {stGraph = f (stGraph s)})

limits :: Ctx -> Limits
limits c = Limits (gcMaxDepth (cCfg c)) (gcMaxSplits (cCfg c))

nodeOf :: St -> NodeId -> Node
nodeOf s i = gNodes (stGraph s) IM.! i

nameOf :: St -> NodeId -> Text
nameOf s = nName . nodeOf s

spent :: St -> Double
spent = sum . Map.elems . stDollars

capped :: Ctx -> St -> Bool
capped c s = spent s >= gcCapUsd (cCfg c)

-- | Tiers are set and every one is skipped: no worker or planner starts.
allSkipped :: Ctx -> St -> Bool
allSkipped c s = let n = length (gcTiers (cCfg c)) in n > 0 && all (Ctl.skipped (stHealth s)) [0 .. n - 1]

busySet :: St -> IntSet
busySet s = IS.fromList (map (jNode . snd) (stRunning s))

expectedOf :: St -> NodeId -> Text
expectedOf s i = Map.findWithDefault "" (nameOf s i) (stTypes s)

-- | A declaration's statement as written in the base (its printed type when absent).
stmtOf :: St -> NodeId -> Text
stmtOf s i = maybe (expectedOf s i) statementHead (findDecl (nameOf s i) (declarations (stBase s)))

-- | The lemmas of the node's root's current plan its proof may use: all but
-- the node and those above it.
usableOf :: St -> NodeId -> [Text]
usableOf s n =
  let g = stGraph s
      above = ancestors n g
   in [nameOf s i | i <- planLemmas g (nRoot (nodeOf s n)), i /= n, not (IS.member i above)]

boardOf :: St -> NodeId -> Text
boardOf s r = whiteboard (stGraph s) r (IM.findWithDefault [] r (stFailures s))

boards :: St -> [(NodeId, Text)]
boards s = [(r, boardOf s r) | r <- gRoots (stGraph s)]

oneLine :: Text -> Text
oneLine = T.take 300 . T.unwords . T.words

showErr :: SomeException -> Text
showErr = T.take 300 . T.pack . displayException

addCost :: Ctx -> Role -> Double -> IO ()
addCost c role d = do
  modify c (\s -> s {stDollars = Map.insertWith (+) role d (stDollars s)})
  logG c (\t -> RoleCost t role d)

-- | A whiteboard line @<node>: <note>@ on the node's root.
failureLine :: Ctx -> NodeId -> Text -> IO ()
failureLine c i note = modify c $ \s ->
  let n = nodeOf s i
   in s {stFailures = IM.insertWith (flip (<>)) (nRoot n) [nName n <> ": " <> oneLine note] (stFailures s)}

addNote :: Ctx -> NodeId -> Text -> IO ()
addNote c i note = modify c (\s -> s {stNotes = IM.insertWith (flip (<>)) i [note] (stNotes s)})

mergeBase :: Ctx -> Text -> IO ()
mergeBase c text = do
  modify c (\s -> s {stBase = text})
  seBaseChanged (geSearch (cEnv c)) text

startJob :: Ctx -> NodeId -> Kind -> IO Out -> IO ()
startJob c n kind act = do
  a <- async act
  modify c (\s -> s {stRunning = (a, Job n kind) : stRunning s})

-- * The loop

loop :: Ctx -> IO GraphReport
loop c = do
  t <- elapsed c
  s0 <- getSt c
  let pending s = not (null (stRunning s)) || isJust (nextAction (limits c) (IS.union (busySet s) (stWaiting s)) (stGraph s))
  if t >= gcBudget (cCfg c)
    then if pending s0 then stopAll c "time" else finish c Nothing
    else do
      launchAll c IS.empty
      s <- getSt c
      if null (stRunning s) && not (IS.null (stWaiting s))
        then releaseWaiting c >> failStuck c >> save c >> loop c
      else if null (stRunning s)
        then finish c (if stCapHit s && rootOpen s then Just "dollars" else Nothing)
        else do
          t' <- elapsed c
          let left = max 0 (gcBudget (cCfg c) - t')
          r <- timeout (ceiling (left * 1000000)) (waitAnyCatch (map fst (stRunning s)))
          case r of
            Nothing -> stopAll c "time"
            Just (a, res) -> do
              let job = head [j | (a', j) <- stRunning s, a' == a]
              modify c (\s' -> s' {stRunning = [x | x@(a', _) <- stRunning s', a' /= a]})
              stop <- handle c job res
              case stop of
                Just why -> stopAll c why
                Nothing -> do
                  failStuck c
                  save c
                  loop c

-- | Some root is neither proved nor failed.
rootOpen :: St -> Bool
rootOpen s = any (\r -> nStatus (nodeOf s r) == NOpen) (gRoots (stGraph s))

launchAll :: Ctx -> IntSet -> IO ()
launchAll c skipped = do
  s <- getSt c
  let excl = IS.unions [busySet s, stWaiting s, skipped]
      workers = length [() | (_, Job _ KWorker {}) <- stRunning s]
  case nextAction (limits c) excl (stGraph s) of
    Nothing -> pure ()
    Just _ | allSkipped c s -> pure ()
    Just act
      | capped c s -> modify c (\s' -> s' {stCapHit = True})
      | otherwise -> case act of
          Attempt n
            | workers >= gcSlots (cCfg c) -> launchAll c (IS.insert n skipped)
            | otherwise -> startWorker c n >> launchAll c skipped
          Propose n -> startPlan c n >> launchAll c skipped

-- | A node's retrieval text, made once (before its first worker or
-- planner call) and logged; empty with retrieval off.
retrievalOf :: Ctx -> NodeId -> IO Text
retrievalOf c n = case cRetrieve c of
  Nothing -> pure ""
  Just get -> do
    s <- getSt c
    case IM.lookup n (stRetrieved s) of
      Just rt | not (rtCutOff rt) || IS.member n (stRetried s) -> pure (rtBrief rt)
      done -> do
        when (isJust done) $ modify c (\x -> x {stRetried = IS.insert n (stRetried x)})
        let node = nodeOf s n
            rootName = nameOf s (nRoot node)
            line = maybe 1 ((+ 1) . dStart) (findDecl rootName (declarations (cBase0 c)))
            opens = [stmtOf s i | (i, x) <- IM.toList (gNodes (stGraph s)), nStatus x == NOpen || i == n]
            q = Query (nName node) rootName line (stmtOf s n) (Map.lookup (nName node) (stTypes s)) opens
        rt <- get q
        modify c (\x -> x {stRetrieved = IM.insert n rt (stRetrieved x)})
        forM_ (seRetrieval (geSearch (cEnv c))) $ \re -> do
          when (isJust (reFacts re)) $ logG c (\t -> NodeFacts t n (rtFacts rt) (rtFactsChars rt) (rtCutOff rt))
          when (isJust (reLibrary re)) $ logG c (\t -> NodeLibraryOffered t n (rtOffered rt) (rtNotes rt) (rtCutOff rt))
        pure (rtBrief rt)

startWorker :: Ctx -> NodeId -> IO ()
startWorker c n = do
  retrieved <- retrievalOf c n
  s <- getSt c
  t <- elapsed c
  let name = nameOf s n
      brief0 = workerBrief name (stmtOf s n) (usableOf s n) (boardOf s (nRoot (nodeOf s n)))
      brief = if T.null retrieved then brief0 else brief0 <> "\n" <> retrieved <> "\n"
      -- a partial proof from Repair, spliced into the current base, and its holes
      (start, brief1) = case nPartial (nodeOf s n) >>= \p -> partialStart name p (stBase s) of
        Just (st, holes) -> (st, brief <> "\n" <> holes <> "\n")
        Nothing -> (stBase s, brief)
      -- the note on the node's stopped worker, used once
      brief' = maybe brief1 (\note -> brief1 <> "\n" <> note <> "\n") (IM.lookup n (stStall s))
      -- the node's tier: its step is its counted attempts (launchAll starts
      -- no worker once every tier is skipped)
      tiers = gcTiers (cCfg c)
      tier = if null tiers then Nothing else tierFor (length tiers) (stHealth s) (nAttempts (nodeOf s n))
      spec = AttemptSpec
        { asId = stNext s, asGoal = name, asBackend = workerBackend, asStart = start, asBrief = brief'
        , asModel = fmap (tiers !!) tier, asTier = tier }
  modify c (\s' -> s' { stNext = stNext s' + 1, stGraph = recordAction n (stGraph s'), stStall = IM.delete n (stStall s')
                      , stLastTier = maybe id (IM.insert n) tier (stLastTier s')
                      , stStallUsed = maybe id (IM.insert (asId spec)) (IM.lookup n (stStall s)) (stStallUsed s') })
  startJob c n (KWorker spec t) (OutWorker <$> seRun (geSearch (cEnv c)) spec)
  logG c (\t' -> AttemptLaunched t' (asId spec) n)
  forM_ ((,) <$> tier <*> asModel spec) $ \(k, m) -> do
    logG c (\t' -> NodeTierChosen t' (asId spec) n k m)
    -- any change of a node's tier is an escalation, also one made because
    -- a tier was skipped
    forM_ (IM.lookup n (stLastTier s)) $ \k0 ->
      when (k0 /= k) $ do
        modify c (\x -> x {stEscalations = stEscalations x + 1})
        logG c (\t' -> NodeEscalated t' n k0 k)

startPlan :: Ctx -> NodeId -> IO ()
startPlan c n = do
  retrieved <- retrievalOf c n
  s <- getSt c
  let (sid, g') = proposeSplit n (recordAction n (stGraph s))
      root = nRoot (nodeOf s n)
  modify c (\s' -> s' {stGraph = g'})
  startJob c n (KPlan sid) $ OutPlan <$>
    planJob (cCk c) (geAsk (cEnv c)) (stBase s) (nameOf s n) (expectedOf s n) (usableOf s n) (boardOf s root) (IM.findWithDefault [] n (stNotes s)) retrieved
  logG c (\t -> SplitProposed t sid n)

-- | Cancel what runs and end the run.
stopAll :: Ctx -> Text -> IO GraphReport
stopAll c why = do
  s <- getSt c
  forM_ (stRunning s) $ \(a, job) -> do
    cancel a
    case jKind job of
      KWorker spec started -> do
        -- priced from its ledger with the controller on
        d <- priceCancelled (gcControl (cCfg c)) (geSearch (cEnv c)) spec
        t <- elapsed c
        let outcome = "cancelled: " <> why
        modify c (\s' -> s' {stRecords = (recordOf spec started t outcome Nothing) {arCost = d} : stRecords s'})
        mapM_ (addCost c Worker) d
        logG c (\t' -> AttemptEnded t' (asId spec) (jNode job) outcome)
      _ -> pure ()
  modify c (\s' -> s' {stRunning = []})
  finish c (Just why)

finish :: Ctx -> Maybe Text -> IO GraphReport
finish c why = do
  forM_ why $ \w -> logG c (`GraphStopped` w)
  getSt c >>= prune c . IM.keys . gNodes . stGraph
  save c
  saved <- cHarvest c
  s <- getSt c
  t <- elapsed c
  tr <- readIORef (cTrack c)
  let g = stGraph s
      on0 = isJust (seAuto (geSearch (cEnv c)))
  pure GraphReport
    { grGraph = g
    , grWhiteboards = boards s
    , grDollars = stDollars s
    , grElapsed = t
    , grStopped = why
    , grBase = stBase s
    , grClosedRoots = length [() | r <- gRoots g, maybe False ((== NProved) . nStatus) (IM.lookup r (gNodes g))]
    , grAttempts = reverse (stRecords s)
    , grAutoRan = on0 && trackRan tr
    , grAutoOff = fst <$> atOff tr
    , grRetrieval = retrievalReport s saved <$ seRetrieval (geSearch (cEnv c))
    , grControl = gcControl (cCfg c)
    , grEscalations = stEscalations s }

-- | The library's harvest at the end of the run: the merged file's new
-- sorry-free lemmas, the roots' failure lines and the nodes' notes.
harvestRun :: Ctx -> IO (Int, Int)
harvestRun c = case seRetrieval (geSearch (cEnv c)) of
  Nothing -> pure (0, 0)
  Just re -> do
    s <- getSt c
    let failures = [ (T.strip nm, T.strip (T.drop 1 rest))
                   | l <- concat (IM.elems (stFailures s)), let (nm, rest) = T.breakOn ":" l, not (T.null rest) ]
        notes = failures <> [(nameOf s i, x) | (i, xs) <- IM.toList (stNotes s), x <- xs]
    r <- saveRun re (seRel (geSearch (cEnv c))) (cBase0 c) (stBase s) notes
    case r of
      Nothing -> pure (0, 0)
      Just (Left e) -> logG c (\t -> GraphRunEvent (LibraryProblem t e)) >> pure (0, 0)
      Just (Right (l, k)) -> logG c (\t -> GraphRunEvent (LibrarySaved t l k)) >> pure (l, k)

-- | Each node's retrieval, with the offered lemmas it used: merged as a
-- helper with its proof, or named in its proof in the final base.
retrievalReport :: St -> (Int, Int) -> RetrievalReport
retrievalReport s saved =
  RetrievalReport
    [ (nName node, GoalRetrieval (rtFacts rt) (rtOffered rt) (usedOf (rtLemmas rt) proof helpers))
    | (i, rt) <- IM.toList (stRetrieved s)
    , Just node <- [IM.lookup i (gNodes (stGraph s))]
    , let proof = maybe "" dProof (findDecl (nName node) decls)
          helpers = helperStatements decls (IM.findWithDefault [] i (stHelpers s)) ]
    saved
  where
    decls = declarations (stBase s)

save :: Ctx -> IO ()
save c = do
  s <- getSt c
  geSave (cEnv c) (stGraph s) (boards s)

-- * Applying results

-- | Apply one job's result; a reason when the run must stop.
handle :: Ctx -> Job -> Either SomeException Out -> IO (Maybe Text)
handle c (Job n kind) res = case kind of
  KWorker spec started -> do
    let unwrap = \case
          OutWorker ar -> Right ar
          _ -> Left "not a worker's result"
        res' = res >>= either (Left . toException . userError) Right . unwrap
    workerDone c n spec started res'
    modify c (\x -> x {stStallUsed = IM.delete (asId spec) (stStallUsed x)})
    -- fed to the tier's health after the record; an exception says nothing
    -- about the model and is not fed
    forM_ res' (noteTier c spec)
    pure Nothing
  KPlan sid -> case res of
    Left e -> splitFailed c n sid (showErr e) >> pure Nothing
    Right (OutPlan a) -> asked Planner a $ \case
      Left e -> splitFailed c n sid e
      Right p
        | plVerdict p == Closed -> closedSketch c n sid p
        | otherwise -> toReview c n sid p
    Right _ -> pure Nothing
  KReview sid p -> case res of
    Left e -> splitRejected c n sid (showErr e) >> pure Nothing
    Right (OutReview a) -> asked Verifier a $ \case
      Left e -> splitRejected c n sid e
      Right v
        | svAccept v -> acceptSketch c n sid p
        | otherwise -> splitRejected c n sid (svReason v)
    Right _ -> pure Nothing
  KFalse claim -> case res of
    Left e -> failureLine c n ("FALSE: " <> claim <> " (not checked: " <> showErr e <> ")") >> pure Nothing
    Right (OutFalse a) -> asked Verifier a $ \case
      Right (True, _) -> do
        s <- getSt c
        when (nStatus (nodeOf s n) == NOpen) $ do
          logG c (\t -> FalseLemma t n claim)
          falseNode c n claim
      Right (False, why) -> failureLine c n ("FALSE: " <> claim <> " (not confirmed: " <> why <> ")")
      Left e -> failureLine c n ("FALSE: " <> claim <> " (not checked: " <> e <> ")")
    Right _ -> pure Nothing
  where
    asked role a k = case a of
      NoCredit d -> addCost c role d >> pure (Just "credit")
      Asked d r -> addCost c role d >> k r >> pure Nothing

workerDone :: Ctx -> NodeId -> AttemptSpec -> Double -> Either SomeException AttemptResult -> IO ()
workerDone c n spec started res = do
  s <- getSt c
  let mar = either (const Nothing) Just res
      stats = mar >>= arStats
      final = stats >>= csResult
      -- the ledger's dollars when known, else the child's own
      dollars = (mar >>= arDollars) <|> (stats >>= csCostUsd)
      ended outcome = do
        t <- elapsed c
        let rec = (recordOf spec started t outcome stats) {arStopped = mar >>= arStop, arCost = dollars}
        modify c (\s' -> s' {stRecords = rec : stRecords s'})
        logG c (\t' -> AttemptEnded t' (asId spec) n outcome)
      -- a failed attempt; with the agent's file, Repair first, whose proof
      -- the kernel may keep (the attempt's record then says so). A stopped
      -- worker's file fails the same way, and its note (the verification's
      -- errors, @errs@) goes into the node's next brief.
      failed outcome errs agent = do
        repaired <- maybe (pure Nothing) (repairNode c n spec) agent
        case repaired of
          Just (v, merged) -> do
            ended (if v == Closed then "closed by repair" else "waiting after repair")
            keepProof c n v merged
          Nothing -> do
            forM_ ((,) <$> errs <*> (mar >>= \ar -> (,) ar <$> arStop ar)) $ \(e, (ar, st)) ->
              modify c (\x -> x {stStall = IM.insert n (stallNote st e (arLastMessage ar)) (stStall x)})
            ended outcome
            modGraph c (attemptDone n)
            s1 <- getSt c
            when (nAttempts (nodeOf s1 n) >= 2) (disproveNode c n)
            s2 <- getSt c
            -- a node Disprove just failed gets no note and no FALSE check
            let open = nStatus (nodeOf s2 n) == NOpen
            case workerEnd final outcome of
              Note x -> when open (failureLine c n x >> addNote c n x)
              FalseClaim claim -> do
                addNote c n ("FALSE: " <> claim)
                if not open
                  then pure ()
                else if capped c s2
                  then do
                    modify c (\x -> x {stCapHit = True})
                    failureLine c n ("FALSE: " <> claim <> " (not checked: the dollar cap)")
                  else startJob c n (KFalse claim) (OutFalse <$> falseJob (geAsk (cEnv c)) (stmtOf s2 n) claim)
  addCost c Worker (fromMaybe 0 dollars)
  -- a stopped worker is logged before its record, its file judged as any other's
  forM_ (mar >>= arStop) $ \st ->
    logG c (\t -> NodeStopped t (asId spec) n (stopReasonText (Ctl.stReason st)) (Ctl.stTurns st) (Ctl.stDollars st))
  if nStatus (nodeOf s n) /= NOpen
    then ended "discarded: the node was decided meanwhile"
    else case res of
      Left e -> failed ("error: " <> showErr e) Nothing Nothing
      -- the model failed: neither verified nor repaired; kept out of the
      -- node's attempts three times, then a failed attempt
      Right ar | Just f <- arFailure ar -> do
        let outcome = "model error: " <> modelFailureText f
            k = IM.findWithDefault 0 n (stModelErrors s)
        -- the stall note it carried goes back for the next worker, unless
        -- a newer one is there
        forM_ (IM.lookup (asId spec) (stStallUsed s)) $ \note ->
          modify c (\x -> x {stStall = IM.insertWith (\_ newer -> newer) n note (stStall x)})
        if k < 3
          then modify c (\x -> x {stModelErrors = IM.insert n (k + 1) (stModelErrors x)}) >> ended outcome
          else failed outcome Nothing Nothing
      Right ar -> case arText ar of
        Nothing -> failed ("no file: " <> arNote ar) Nothing Nothing
        Just agent -> do
          let ck = cCk c
              name = nameOf s n
          o <- locked ck (verifyAttempt (ckLean ck) (ckPolicy ck) (ckRoot ck) (ckRel ck) (asStart spec) agent (stBase s) name (expectedOf s n))
          case o of
            NotSpliced e -> failed ("not spliced: " <> T.pack (show e)) (Just (T.pack (show e))) (Just agent)
            Judged (Rejected why) _ _ -> failed ("rejected: " <> why) (Just why) (Just agent)
            Judged v merged helpers -> do
              modify c (\x -> x {stHelpers = IM.insertWith (flip (<>)) n helpers (stHelpers x)})
              ended ((if v == Closed then "closed" else "waiting on open goals")
                     <> (if null helpers then "" else " (helpers: " <> T.intercalate ", " helpers <> ")"))
              keepProof c n v merged

-- | An attempt's record with its tier and model.
recordOf :: AttemptSpec -> Double -> Double -> Text -> Maybe ChildStats -> AttemptRecord
recordOf spec started t outcome stats =
  (plainRecord (asId spec) (asGoal spec) (asBackend spec) outcome started t stats) {arModel = asModel spec, arTierOf = asTier spec}

-- | A worker's result for its tier's health; a tier newly skipped is
-- logged (the open nodes give up in 'failStuck' once every tier is).
noteTier :: Ctx -> AttemptSpec -> AttemptResult -> IO ()
noteTier c spec ar = forM_ (asTier spec) $ \i -> do
  s <- getSt c
  let (h, newly) = noteResult i (arFailure ar) (stHealth s)
  modify c (\x -> x {stHealth = h})
  when newly $
    logG c (\t -> NodeTierSkipped t (gcTiers (cCfg c) !! i) (maybe "" modelFailureText (arFailure ar)))

-- | A node's proof the kernel kept: merged, then the node proved (and the
-- graph settled) or waiting on open goals.
keepProof :: Ctx -> NodeId -> Verdict -> Text -> IO ()
keepProof c n v merged = do
  mergeBase c merged
  if v == Closed
    then proveNode c n >> settle c
    else modify c (\x -> x {stWaiting = IS.insert n (stWaiting x)})

toReview :: Ctx -> NodeId -> SplitId -> Plan -> IO ()
toReview c n sid p = do
  s <- getSt c
  if capped c s
    then modify c (\x -> x {stCapHit = True})
    else startJob c n (KReview sid p) $ OutReview <$>
      reviewJob (geAsk (cEnv c)) (stmtOf s n) p (boardOf s (nRoot (nodeOf s n)))

splitFailed :: Ctx -> NodeId -> SplitId -> Text -> IO ()
splitFailed c n sid why = do
  modGraph c (failSplit sid why)
  logG c (\t -> SplitFailed t sid why)
  failureLine c n ("split failed: " <> why)
  s <- getSt c
  prune c (maybe [] sLemmas (IM.lookup sid (gSplits (stGraph s))))

splitRejected :: Ctx -> NodeId -> SplitId -> Text -> IO ()
splitRejected c n sid why = do
  modGraph c (rejectSplit sid why)
  logG c (\t -> SplitRejected t sid why)
  failureLine c n ("split rejected: " <> why)

proveNode :: Ctx -> NodeId -> IO ()
proveNode c i = do
  modify c (\s -> s {stGraph = markProved i (stGraph s), stWaiting = IS.delete i (stWaiting s)})
  logG c (`NodeProved` i)

-- | A node shown false (a confirmed FALSE claim, or a certified negation):
-- it fails, and so do the splits that held it.
falseNode :: Ctx -> NodeId -> Text -> IO ()
falseNode c n claim = do
  logG c (\t -> NodeFailed t n ("false: " <> claim))
  failNode c n ("false: " <> claim)

-- | Fail a node; the splits that held it fail with it.
failNode :: Ctx -> NodeId -> Text -> IO ()
failNode c i why = do
  s <- getSt c
  let g = stGraph s
      g' = markFailed i why g
      newly = [ (sid, r) | (sid, sp) <- IM.toList (gSplits g'), SFailed r <- [sStatus sp]
              , fmap sStatus (IM.lookup sid (gSplits g)) /= Just (sStatus sp) ]
  modify c (\x -> x {stGraph = g', stWaiting = IS.delete i (stWaiting x)})
  forM_ newly $ \(sid, r) -> logG c (\t -> SplitFailed t sid r)
  prune c (concat [maybe [] sLemmas (IM.lookup sid (gSplits g')) | (sid, _) <- newly])

-- | Lemmas left behind by failed splits: of these nodes, those not proved and
-- no longer live whose declaration in the base is still just @sorry@ are
-- removed from the base, unless another declaration names them.
prune :: Ctx -> [NodeId] -> IO ()
prune c ids = do
  s <- getSt c
  let g = stGraph s
      live = liveNodes g
      ds = declarations (stBase s)
      cands = [ d | i <- nub ids, Just nd <- [IM.lookup i (gNodes g)], nDepth nd > 0, nStatus nd /= NProved
              , not (IS.member i live), Just d <- [findDecl (nName nd) ds], isOpen d ]
      others = [d | d <- ds, dName d `notElem` map dName cands]
      named d = any (\o -> mentions (dName d) (dStatement o <> "\n" <> dProof o)) others
      gone = [dName d | d <- cands, not (named d)]
  unless (null gone) (mergeBase c (removeDecls gone (stBase s)))

-- | Nodes with nothing left to try fail, until none is left. With every
-- tier skipped, that is every open live node nothing runs on.
failStuck :: Ctx -> IO ()
failStuck c = do
  s <- getSt c
  let busy = IS.union (busySet s) (stWaiting s)
      g = stGraph s
      given
        | allSkipped c s = [i | (i, nd) <- IM.toList (gNodes g), nStatus nd == NOpen, IS.member i (liveNodes g), not (IS.member i busy)]
        | otherwise = []
  case nub (stuck (limits c) busy g <> given) of
    [] -> pure ()
    ns -> do
      forM_ ns $ \i -> do
        let why = "nothing left to try"
        logG c (\t -> NodeFailed t i why)
        failNode c i why
      failStuck c

-- | Check again the waiting nodes and the parents of splits whose lemmas
-- are all proved; repeat while something closes. Such a split is done when
-- its parent closes, fails when the parent's recheck is rejected, and stays
-- pending while the parent still waits on open goals.
settle :: Ctx -> IO ()
settle c = do
  s <- getSt c
  let g = stGraph s
      dones = [(sid, sParent sp) | sid <- doneSplits g, Just sp <- [IM.lookup sid (gSplits g)]]
      targets = nub [i | i <- IS.toList (stWaiting s) <> map snd dones, nStatus (nodeOf s i) == NOpen]
      ck = cCk c
  unless (null targets) $ do
    vs <- locked ck (recheck (ckLean ck) (ckPolicy ck) (ckRoot ck) (ckRel ck) (stBase s) (stTypes s) (map (nameOf s) targets))
    closed <- forM (zip targets (map snd vs)) $ \(i, v) -> do
      let mine = [sid | (sid, p) <- dones, p == i]
      case v of
        Closed -> do
          forM_ mine (modGraph c . closeSplit)
          proveNode c i
          pure True
        Rejected why -> do
          forM_ mine $ \sid -> splitFailed c i sid "the sketch does not close with its lemmas proved"
          when (IS.member i (stWaiting s)) $ do
            modify c (\x -> x {stWaiting = IS.delete i (stWaiting x), stGraph = attemptDone i (stGraph x)})
            failureLine c i ("rejected on recheck: " <> why)
          pure False
        -- still waiting (on another plan lemma, say): the split stays pending
        -- and is checked again when something else closes
        _ -> pure False
    when (or closed) (settle c)

-- | Nothing runs and nothing can start: the waiting nodes are checked once
-- more; those still waiting leave the set with their attempt counted.
releaseWaiting :: Ctx -> IO ()
releaseWaiting c = do
  s <- getSt c
  let ws = IS.toList (stWaiting s)
      ck = cCk c
  vs <- locked ck (recheck (ckLean ck) (ckPolicy ck) (ckRoot ck) (ckRel ck) (stBase s) (stTypes s) (map (nameOf s) ws))
  closed <- forM (zip ws (map snd vs)) $ \(i, v) -> case v of
    Closed -> proveNode c i >> pure True
    _ -> do
      modify c (\x -> x {stWaiting = IS.delete i (stWaiting x), stGraph = attemptDone i (stGraph x)})
      failureLine c i (case v of
        Rejected why -> "rejected on recheck: " <> why
        _ -> "waits on lemmas that did not close")
      pure False
  when (or closed) (settle c)

-- | Fit a sketch to the current base: as checked when nothing moved, else
-- applied and checked again. The applied text, the node's verdict, the lemma types.
refit :: Ctx -> NodeId -> Plan -> Sketch -> IO (Either Text (Text, Verdict, Map.Map Text Text))
refit c n p sk = do
  s <- getSt c
  let base = stBase s
      name = nameOf s n
      ck = cCk c
  if base == plBase p && sk == plSketch p
    then pure (Right (plApplied p, plVerdict p, plTypes p))
    else case applySketch base name sk of
      Left e -> pure (Left (sketchErrorText e))
      Right applied -> do
        r <- locked ck (checkSketchVerdict (ckLean ck) (ckPolicy ck) (ckRoot ck) (ckRel ck) applied name (expectedOf s n) (map fst (skLemmas sk)))
        pure (fmap (\(v, tys) -> (applied, v, tys)) r)

-- | A sketch that already closes its node: the node's new proof alone (the
-- lemmas it does not use left out) is checked once more and merged, and the
-- node is proved with no review. Should the proof need its lemmas after all,
-- the whole sketch goes to review.
closedSketch :: Ctx -> NodeId -> SplitId -> Plan -> IO ()
closedSketch c n sid p = do
  r <- refit c n p (plSketch p) {skLemmas = []}
  case r of
    Right (applied, Closed, _) -> do
      mergeBase c applied
      modGraph c (closeSplit sid)
      proveNode c n
      settle c
    _ -> do
      r' <- refit c n p (plSketch p)
      case r' of
        Left e -> splitFailed c n sid e
        Right (applied, v, tys) -> do
          s <- getSt c
          toReview c n sid p {plBase = stBase s, plApplied = applied, plVerdict = v, plTypes = tys}

-- | An accepted split: lemma names (the sketch's, qualified as the node's
-- name is), lemma keys, links to the graph, aliases for shared and reused lemmas, the sketch fitted to the current base, then the merge.
acceptSketch :: Ctx -> NodeId -> SplitId -> Plan -> IO ()
acceptSketch c n sid p = do
  s <- getSt c
  let base = stBase s
      ds = declarations base
      g = stGraph s
      pre = nsPrefix (nameOf s n)
      lemmas = [(b, printed, qualify pre b) | (b, _) <- skLemmas (plSketch p), Just printed <- [Map.lookup b (plTypes p)]]
      parentStart = dDocStart <$> findDecl (nameOf s n) ds
      canAlias i = case (dStart <$> findDecl (nameOf s i) ds, parentStart) of
        (Just a, Just b) -> a < b
        _ -> False
  case acceptSplit canAlias sid [(qn, typeKey qn printed) | (_, printed, qn) <- lemmas] g of
    Left e -> splitRejected c n sid e
    Right (g', links) -> do
      let tgt i = maybe "" nName (IM.lookup i (gNodes g'))
          alias sk b qn i
            | qn == tgt i = sk {skLemmas = filter ((/= b) . fst) (skLemmas sk)}
            | otherwise = aliasLemma b (tgt i) sk
          step sk ((b, _, qn), l) = case l of
            NewNode _ -> sk
            Shared i -> alias sk b qn i
            Reused i -> alias sk b qn i
          sk' = foldl' step (plSketch p) (zip lemmas links)
          clashes = [qn | base /= plBase p, ((_, _, qn), NewNode _) <- zip lemmas links, isJust (findDecl qn ds)]
      case clashes of
        (x : _) -> splitFailed c n sid ("the name " <> x <> " is already in the file")
        [] -> do
          r <- refit c n p sk'
          case r of
            Left e -> splitFailed c n sid e
            Right (applied, v, _) -> do
              let newTypes = Map.fromList [(qn, printed) | ((_, printed, qn), NewNode _) <- zip lemmas links]
              modify c (\x -> x {stGraph = g', stTypes = Map.union (stTypes x) newTypes})
              mergeBase c applied
              logG c (\t -> SplitAccepted t sid (map linkId links))
              forM_ links $ \case
                NewNode i -> forM_ (IM.lookup i (gNodes g')) $ \nd -> logG c (\t -> NodeAdded t i (nName nd) (nDepth nd))
                Shared i -> logG c (\t -> NodeShared t i sid)
                Reused i -> logG c (\t -> NodeReused t i sid)
              when (v == Closed) (modGraph c (closeSplit sid) >> proveNode c n)
              when (v == Closed || or [True | Reused _ <- links]) (settle c)
              -- Auto at each new lemma before any worker; one it misses goes to Disprove
              forM_ [i | NewNode i <- links] (autoLemma c)

linkId :: Link -> NodeId
linkId = \case
  NewNode i -> i
  Shared i -> i
  Reused i -> i

-- * The automation

-- | One automation step and what follows it (@k@, given the step's
-- result), both under the checker lock: the language server and the
-- probes work on one project. Nothing when the automation is off, the
-- budget is spent, or the server just failed (logged once, and the
-- automation off from then on).
-- @step@ and @n@ name it in a timeout's event.
withAuto :: Ctx -> Text -> NodeId -> (AutoEnv -> IO (Either AutoFail a)) -> (a -> IO b) -> IO (Maybe (Either AutoFail b))
withAuto c step n act k = readIORef (cAuto c) >>= \case
  Nothing -> pure Nothing
  Just _ -> locked (cCk c) $ do
    let budgetLeft = (gcBudget (cCfg c) -) <$> elapsed c
    r <- autoStep (cAuto c) (cTrack c) budgetLeft (\why -> logG c (`GraphAutoOff` why))
           (logG c (\t -> NodeAutoTimeout t n step)) act
    traverse (traverse k) r

-- | Auto at an open node's hole; a fill the kernel closes the node with is
-- merged and the node proved. Nothing when the automation did not run,
-- else whether the node closed.
autoNode :: Ctx -> NodeId -> IO (Maybe Bool)
autoNode c n = do
  s <- getSt c
  let name = nameOf s n
      base = stBase s
      ck = cCk c
  r <- withAuto c "auto" n (\ae -> autoGoal (aeConfig ae) (aeLsp ae) (elapsed c) base name) $ \(filled, snips) -> do
    o <- verifyAttempt (ckLean ck) (ckPolicy ck) (ckRoot ck) (ckRel ck) base filled base name (expectedOf s n)
    pure (o, snips)
  case r of
    Nothing -> pure Nothing
    Just (Right (Judged Closed merged _, snips)) -> do
      mergeBase c merged
      logG c (\t -> NodeAutoClosed t n (T.intercalate "; " snips))
      proveNode c n
      settle c
      pure (Just True)
    -- a fill that leaves the node waiting on open goals is dropped on
    -- purpose: only a fill that closes the node is merged, so Auto never
    -- leaves a proof built on lemmas nobody has proved yet
    Just _ -> pure (Just False)

-- | A new lemma of an accepted split, while it is open and live: Auto, and
-- Disprove when Auto misses.
autoLemma :: Ctx -> NodeId -> IO ()
autoLemma c i = do
  s <- getSt c
  let fresh = nStatus (nodeOf s i) == NOpen && IS.member i (liveNodes (stGraph s))
  when fresh $ autoNode c i >>= \case
    Just False -> disproveNode c i
    _ -> pure ()

-- | Disprove, once per node, on an open node: a certified negation fails
-- it as a confirmed FALSE does; a counterexample goes on its root's
-- whiteboard, which its workers and planner read.
disproveNode :: Ctx -> NodeId -> IO ()
disproveNode c n = do
  s <- getSt c
  on <- isJust <$> readIORef (cAuto c)
  when (on && nStatus (nodeOf s n) == NOpen && not (IS.member n (stDisproveTried s))) $ do
    modify c (\x -> x {stDisproveTried = IS.insert n (stDisproveTried x)})
    let ck = cCk c
        go ae
          | acDisprove (aeConfig ae) =
              disprove (aeSmt ae) [ViaPlausible, ViaSmt] (aeConfig ae) (aeLsp ae) (ckLean ck) (ckPolicy ck) (ckRoot ck) (ckRel ck) (stBase s) (nameOf s n)
          | otherwise = pure (Left Missed)
    r <- withAuto c "disprove" n go pure
    forM_ r $ \case
      Right (_, _, info) -> forM_ (diSteps info) $ \(src, res) -> logG c (\t -> NodeDisproveRan t n (sourceName src) res)
      Left _ -> pure ()
    case r of
      Just (Right (DFalse tac, src, info)) -> do
        logG c (\t -> NodeDisproved t n "false" tac (maybe "" sourceName src) (diWitness info))
        falseNode c n ("the negation is proved by " <> tac)
      Just (Right (DSuspect cx, src, _)) -> do
        logG c (\t -> NodeDisproved t n "suspect" cx (maybe "" sourceName src) Nothing)
        failureLine c n $ case src of
          Just ViaSmt -> "SUSPECT: an SMT solver found a countermodel: " <> cx
          _ -> "SUSPECT: plausible found a counterexample: " <> cx
      _ -> pure ()

-- | Repair on a failed attempt's file. A partial proof with holes left
-- (fewer than the one kept) becomes the node's next start; one with none
-- is checked as the attempt's proof. Its verdict and merged file when the
-- kernel kept it (closed, or waiting on open goals), for the caller to apply.
repairNode :: Ctx -> NodeId -> AttemptSpec -> Text -> IO (Maybe (Verdict, Text))
repairNode c n spec agent = do
  s <- getSt c
  let name = nameOf s n
      stmt = maybe "" dStatement (findDecl name (declarations (asStart spec)))
      ck = cCk c
      go ae
        | acRepair (aeConfig ae) = R.repair (aeConfig ae) (aeLsp ae) (elapsed c) stmt agent name
        | otherwise = pure (Right Nothing)
  r <- withAuto c "repair" n go $ traverse $ \rp ->
    if R.rpHolesAfter rp > 0
      then pure (rp, Nothing)
      else do
        o <- verifyAttempt (ckLean ck) (ckPolicy ck) (ckRoot ck) (ckRel ck) (asStart spec) (R.rpText rp) (stBase s) name (expectedOf s n)
        pure (rp, Just o)
  case r of
    Just (Right (Just (rp, o))) -> do
      logG c (\t -> NodeRepaired t (asId spec) n (R.rpHolesBefore rp) (R.rpHolesAfter rp))
      modGraph c (setPartial n (keptPartial name (asStart spec) rp (nPartial (nodeOf s n))))
      case o of
        Just (Judged v merged _) | kept v -> pure (Just (v, merged))
        _ -> pure Nothing
    _ -> pure Nothing
  where
    kept = \case
      Rejected _ -> False
      _ -> True
