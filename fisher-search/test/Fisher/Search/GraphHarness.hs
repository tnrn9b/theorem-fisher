-- | The scripted graph run the GraphRun specs share: workers that write a
-- given proof, scripted planner and verifier replies, an optional fake
-- language server, and what the run did. The graph's jobs run on the
-- simulator ('simOrchestrator' over 'jobKey'): a worker's virtual seconds
-- are its 'rClock', and the run's clock is the simulator's.
module Fisher.Search.GraphHarness
  ( keyLean
  , oneRoot
  , twoRoots
  , Run (..)
  , w
  , ok
  , bad
  , Reply (..)
  , say
  , sayTo
  , toNode
  , garbage
  , reject
  , GOut (..)
  , OrchFor
  , simFor
  , runG
  , runGWith
  , runGAuto
  , runGRet
  , runGPriced
  , runGTimed
  , runGBy
  , runGInterrupted
  , splitOf
  , declsNamed
  , cfg
  , nodeBy
  , statusOf
  , tags
  , precedes
  , count
  , infixOf
  ) where

import Control.Concurrent.Async (async, asyncThreadId, race, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (AsyncException (UserInterrupt), SomeException, finally, throwTo)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.List (findIndex, isPrefixOf, tails)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)

import Fisher.Role (partText)
import Fisher.Search.Auto (Lsp (..))
import Fisher.Search.Check (AxiomPolicy (..), LeanRun (..), standardAxioms)
import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Controller (ModelFailure, Stop)
import Fisher.Search.FakeLean (fakeLeanBy)
import Fisher.Search.Graph
import Fisher.Search.GraphRun
import Fisher.Search.Lean
import Fisher.Search.Library (Library, loadLibrary)
import Fisher.Search.Orchestrate (AttemptKey (..), Entry (Scripted), Orchestrator (..), Script (..), attemptKey, emptyScript, simOrchestrator)
import qualified Fisher.Search.Orchestrate as O
import Fisher.Search.Retrieval (RetrievalEnv (..))
import Fisher.Search.Scheduler
import Fisher.Search.Scripted (withProof)
import Fisher.Search.Split (Message)

-- | Fake Lean whose printed type leaves out the keyword and the name, so two
-- lemmas with one statement and different names share a key.
keyLean :: LeanRun
keyLean = fakeLeanBy $ \d ->
  let s = statementHead d
      afterKw = case [r | k <- ["theorem ", "lemma "], let (_, r) = T.breakOn k s, not (T.null r)] of
        (r : _) -> T.drop (T.length (T.takeWhile (not . isSpace) r) + 1) r
        [] -> s
      afterName = T.stripStart (T.dropWhile (not . isSpace) (T.stripStart afterKw))
   in T.stripStart (fromMaybe afterName (T.stripPrefix ":" afterName))

oneRoot, twoRoots :: Text
oneRoot = T.unlines ["import Mathlib", "", "theorem r1 : A := by", "  sorry"]
twoRoots = T.unlines ["import Mathlib", "", "theorem r1 : A := by", "  sorry", "", "theorem r2 : B := by", "  sorry"]

-- | One worker attempt: the proof written, the final text, dollars (the
-- child's own), the virtual seconds it takes (from its spawn to its
-- delivery), and the controller's fields of its result: the stop, the
-- dollars from its ledger, its last message and a model failure.
data Run = Run
  { rProof :: Text, rFinal :: Maybe Text, rCost :: Double, rClock :: Double
  , rStop :: Maybe Stop, rDollars :: Maybe Double, rLast :: Maybe Text, rFailure :: Maybe ModelFailure }

w :: Text -> Run
w p = Run
  { rProof = p, rFinal = Nothing, rCost = 0, rClock = 0
  , rStop = Nothing, rDollars = Nothing, rLast = Nothing, rFailure = Nothing }

ok, bad :: Run
ok = w "exact done"
bad = w "BAD"

-- | One model reply: the role, a text the last message must contain, the
-- reply. A model-call job takes no virtual time unless the run names its
-- seconds ('runGTimed').
data Reply = Reply Role (Maybe Text) (Either AskError (Text, Double))

say :: Role -> Text -> Reply
say r t = Reply r Nothing (Right (t, 0))

sayTo :: Role -> Text -> Text -> Reply
sayTo r m t = Reply r (Just m) (Right (t, 0))

toNode :: Text -> Text
toNode n = "declaration `" <> n <> "`"

garbage :: Text
garbage = "I cannot split this."

reject :: Text -> Text
reject why = "{\"accept\": false, \"reason\": \"" <> why <> "\", \"suspect\": []}"

data GOut = GOut
  { oRep :: GraphReport
  , oEvents :: [GraphEvent]
  , oAsks :: [(Role, [Message])]
  , oLaunched :: [Text]
  , oProbes :: [Text]
  , oBriefs :: [(Text, Text)]
  -- ^ each attempt's node and brief, in launch order
  , oStarts :: [(Text, Text)]
  -- ^ each attempt's node and start
  , oTrace :: [Text]
  -- ^ @worker g@, @ask planner@, @ask verifier@ and the language server's calls, in order
  , oOverlaps :: Int
  -- ^ Lean probes and language server calls that ran at the same time
  , oLibrary :: Library
  -- ^ the library directory at the end (empty without retrieval)
  , oSpecs :: [AttemptSpec]
  -- ^ each attempt's spec, in launch order
  }

-- | How a run's graph orchestrator is made: from the run's script and the
-- run function 'runGraph' hands over ('runGraphJob' over the run's role
-- contexts).
type OrchFor = Script Job Out -> (Job -> IO Out) -> IO (Orchestrator Job Out)

-- | The simulator over 'jobKey'; it ignores the run function.
simFor :: OrchFor
simFor sc _ = fst <$> simOrchestrator jobKey sc

-- | A worker's result: its proof spliced into its start, its stats and
-- the controller's fields.
resultOf :: Run -> AttemptSpec -> AttemptResult
resultOf r sp =
  let out = withProof (asGoal sp) (rProof r) (asStart sp)
      stats = ChildStats (Just 0) 0 Nothing Nothing Nothing (Just (rCost r)) (rFinal r) Map.empty
   in (plainResult (Just out) (Just stats) "")
        { arStop = rStop r, arDollars = rDollars r, arLastMessage = rLast r, arFailure = rFailure r }

-- | A run's script: each goal's worker runs in order, the n-th worker on
-- a node named g keyed @(g, "worker")@ at index n, taking its 'rClock'
-- seconds, its result made at spawn by @worker@ (which does the harness's
-- bookkeeping); the model-call jobs named in @timed@ with their seconds;
-- and every other job through the default, at once: a worker past its
-- goal's list gets 'bad', a model-call job runs the fake ask through the
-- run function.
graphScript
  :: (Run -> AttemptSpec -> IO AttemptResult) -> [(Text, [Run])] -> [(AttemptKey, Double)] -> (Job -> IO Out) -> Script Job Out
graphScript worker runs timed run = emptyScript
  { scAttempts =
      [(AttemptKey g "worker" i, rClock r, Scripted (workerJob r)) | (g, rs) <- runs, (i, r) <- zip [0 ..] rs]
        <> [(k, secs, O.Run run) | (k, secs) <- timed]
  , scDefault = Just . Scripted $ \job -> case jKind job of
      KWorker {} -> workerJob bad job
      _ -> Right <$> run job
  }
  where
    workerJob r job = case jKind job of
      KWorker sp _ -> Right . OutWorker <$> worker r sp
      _ -> pure (Left "a worker's entry for a model-call job")

runG :: GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO GOut
runG = runGWith keyLean

-- | 'runG' on a given Lean stand-in.
runGWith :: LeanRun -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO GOut
runGWith leanRun = runGAuto leanRun Nothing

-- | 'runGWith' with the automation: its settings and server, given the trace to log calls to.
runGAuto :: LeanRun -> Maybe (IORef [Text] -> AutoEnv) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO GOut
runGAuto = runGRet (\_ -> pure Nothing)

-- | 'runGAuto' with retrieval, made from the run's directory once F.lean
-- is there.
runGRet :: (FilePath -> IO (Maybe RetrievalEnv)) -> LeanRun -> Maybe (IORef [Text] -> AutoEnv) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO GOut
runGRet = runGPriced (\_ -> pure Nothing)

-- | 'runGRet' with a price for cancelled workers ('oPrice').
runGPriced :: (AttemptSpec -> IO (Maybe Double)) -> (FilePath -> IO (Maybe RetrievalEnv)) -> LeanRun -> Maybe (IORef [Text] -> AutoEnv) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO GOut
runGPriced = runGTimed []

-- | 'runGPriced' with the virtual seconds of some model-call jobs, by key
-- (the node's name, @plan@, @review@ or @false@, and how many of that
-- key ran before).
runGTimed
  :: [(AttemptKey, Double)] -> (AttemptSpec -> IO (Maybe Double)) -> (FilePath -> IO (Maybe RetrievalEnv)) -> LeanRun
  -> Maybe (IORef [Text] -> AutoEnv) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO GOut
runGTimed = runGOn simFor

-- | 'runG' on another orchestrator, made by the given builder from the
-- same script.
runGBy :: OrchFor -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO GOut
runGBy orchFor = runGOn orchFor [] (\_ -> pure Nothing) (\_ -> pure Nothing) keyLean Nothing

-- | The scripted graph run every builder above is: the graph's
-- orchestrator from 'graphScript' (the inner one an empty simulator,
-- since no job reaches it), and what the run did.
runGOn
  :: OrchFor -> [(AttemptKey, Double)] -> (AttemptSpec -> IO (Maybe Double)) -> (FilePath -> IO (Maybe RetrievalEnv)) -> LeanRun
  -> Maybe (IORef [Text] -> AutoEnv) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO GOut
runGOn orchFor timed price mkRet leanRun mkAuto gcfg file runs0 replies0 = withSystemTempDirectory "graph" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  ret <- mkRet root
  repliesRef <- newIORef replies0
  asks <- newIORef []
  events <- newIORef []
  launched <- newIORef []
  probes <- newIORef []
  briefs <- newIORef []
  starts <- newIORef []
  specs <- newIORef []
  trace <- newIORef []
  busy <- newIORef (0 :: Int)
  overlaps <- newIORef (0 :: Int)
  let traced x = atomicModifyIORef' trace (\ts -> (ts <> [x], ()))
      -- one Lean or server call: counted when another is under way
      exclusive :: IO a -> IO a
      exclusive act = do
        before <- atomicModifyIORef' busy (\b -> (b + 1, b))
        if before > 0 then atomicModifyIORef' overlaps (\o -> (o + 1, ())) else pure ()
        act `finally` atomicModifyIORef' busy (\b -> (b - 1, ()))
      lean = LeanRun $ \r rel -> exclusive $ do
        t <- TE.decodeUtf8 <$> BS.readFile (r </> rel)
        atomicModifyIORef' probes (\ps -> (t : ps, ()))
        runLean leanRun r rel
      watched ae =
        let l = aeLsp ae
         in ae { aeLsp = l { lspAttempts = \src h ss -> exclusive (lspAttempts l src h ss)
                           , lspGoal = \src h -> exclusive (lspGoal l src h)
                           , lspDiagnostics = exclusive . lspDiagnostics l } }
      worker r sp = do
        traced ("worker " <> asGoal sp)
        atomicModifyIORef' launched (\xs -> (asGoal sp : xs, ()))
        atomicModifyIORef' briefs (\xs -> ((asGoal sp, partText (asBrief sp)) : xs, ()))
        atomicModifyIORef' starts (\xs -> ((asGoal sp, asStart sp) : xs, ()))
        atomicModifyIORef' specs (\xs -> (sp : xs, ()))
        pure (resultOf r sp)
      ask role msgs = do
        traced ("ask " <> T.toLower (T.pack (show role)))
        let lastText = maybe "" snd (if null msgs then Nothing else Just (last msgs))
            fits (Reply r m _) = r == role && maybe True (`T.isInfixOf` lastText) m
        pick <- atomicModifyIORef' repliesRef $ \rs -> case break fits rs of
          (pre, x : post) -> (pre <> post, Just x)
          _ -> (rs, Nothing)
        atomicModifyIORef' asks (\a -> ((role, msgs) : a, ()))
        case pick of
          Nothing -> pure (Left (AskFailed "nothing scripted"))
          Just (Reply _ _ (Left (AskFailed "THROW"))) -> ioError (userError "the connection broke")
          Just (Reply _ _ out) -> pure out
  (inner, _) <- simOrchestrator attemptKey emptyScript
  let senv = SearchEnv inner lean (AxiomPolicy standardAxioms False) root "F.lean"
               (\_ -> pure ()) (fmap (\mk -> watched (mk trace)) mkAuto) ret
      priceJob job = case jKind job of
        KWorker sp _ -> price sp
        _ -> pure Nothing
      orch run = (\o -> o {oPrice = priceJob}) <$> orchFor (graphScript worker runs0 timed run) run
      genv = GraphEnv senv orch ask (\e -> atomicModifyIORef' events (\es -> (e : es, ()))) (\_ _ -> pure ())
  -- a guard against a run that never ends, not a part of any scenario
  r <- timeout 20000000 (runGraph gcfg genv file)
  rep <- case r of
    Nothing -> fail "the run did not end"
    Just (Left e) -> fail (T.unpack e)
    Just (Right x) -> pure x
  GOut rep <$> (reverse <$> readIORef events) <*> (reverse <$> readIORef asks) <*> (reverse <$> readIORef launched)
    <*> (reverse <$> readIORef probes) <*> (reverse <$> readIORef briefs) <*> (reverse <$> readIORef starts)
    <*> readIORef trace <*> readIORef overlaps <*> maybe (pure ([], [])) (loadLibrary . reLibDir) ret
    <*> (reverse <$> readIORef specs)

-- | A graph run with retrieval interrupted (Ctrl-C's exception thrown to
-- its thread) at the first event that satisfies the predicate: the run
-- logs it and waits there until the exception comes. What it ended with,
-- its events, and the library afterwards.
runGInterrupted
  :: (GraphEvent -> Bool) -> (FilePath -> IO (Maybe RetrievalEnv)) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply]
  -> IO (Either SomeException (Either Text GraphReport), [GraphEvent], Library)
runGInterrupted stopAt mkRet gcfg file runs0 replies0 = withSystemTempDirectory "graph" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  ret <- mkRet root
  repliesRef <- newIORef replies0
  events <- newIORef []
  ready <- newEmptyMVar
  hold <- newEmptyMVar
  reached <- newIORef False
  let ask role _ = do
        pick <- atomicModifyIORef' repliesRef $ \rs -> case break (\(Reply r _ _) -> r == role) rs of
          (pre, x : post) -> (pre <> post, Just x)
          _ -> (rs, Nothing)
        pure $ case pick of
          Just (Reply _ _ out) -> out
          Nothing -> Left (AskFailed "nothing scripted")
      logE e = do
        atomicModifyIORef' events (\es -> (e : es, ()))
        first <- if stopAt e then atomicModifyIORef' reached (\was -> (True, not was)) else pure False
        when first (putMVar ready () >> takeMVar hold)
  (inner, _) <- simOrchestrator attemptKey emptyScript
  let senv = SearchEnv inner keyLean (AxiomPolicy standardAxioms False) root "F.lean"
               (\_ -> pure ()) Nothing ret
      genv = GraphEnv senv (\run -> simFor (graphScript (\r sp -> pure (resultOf r sp)) runs0 [] run) run) ask logE (\_ _ -> pure ())
  a <- async (runGraph gcfg genv file)
  -- the run reaches the point, or ends without it
  r <- race (takeMVar ready) (waitCatch a) >>= \case
    Left () -> throwTo (asyncThreadId a) UserInterrupt >> waitCatch a
    Right done -> pure done
  -- the run may still hold @hold@; it is released only now
  _ <- tryPutMVar hold ()
  es <- reverse <$> readIORef events
  lib <- maybe (pure ([], [])) (loadLibrary . reLibDir) ret
  pure (r, es, lib)

-- | The split of a node, by the node's name and the split's position among its splits.
splitOf :: GOut -> Text -> Int -> Split
splitOf o n k = gSplits (grGraph (oRep o)) IM.! (nSplits (nodeBy o n) !! k)

declsNamed :: GOut -> Text -> [Decl]
declsNamed o n = [d | d <- declarations (grBase (oRep o)), dName d == n]

cfg :: GraphConfig
cfg = defaultGraphConfig

nodeBy :: GOut -> Text -> Node
nodeBy o n = case [x | x <- IM.elems (gNodes (grGraph (oRep o))), nName x == n] of
  (x : _) -> x
  [] -> error ("no node " <> T.unpack n)

statusOf :: GOut -> Text -> NodeStatus
statusOf o = nStatus . nodeBy o

-- | Events as short tags (costs and the roots' NodeAdded left out), with node names.
tags :: GOut -> [Text]
tags o = mapMaybe tag (oEvents o)
  where
    g = grGraph (oRep o)
    nm i = maybe "?" nName (IM.lookup i (gNodes g))
    parent s = maybe "?" (nm . sParent) (IM.lookup s (gSplits g))
    tag = \case
      NodeAdded _ _ _ 0 -> Nothing
      NodeAdded _ i _ _ -> Just ("added " <> nm i)
      NodeShared _ i _ -> Just ("shared " <> nm i)
      NodeReused _ i _ -> Just ("reused " <> nm i)
      SplitProposed _ _ i -> Just ("proposed " <> nm i)
      SplitRejected _ s _ -> Just ("rejected " <> parent s)
      SplitAccepted _ s _ -> Just ("accepted " <> parent s)
      SplitFailed _ s _ -> Just ("split failed " <> parent s)
      NodeProved _ i -> Just ("proved " <> nm i)
      NodeFailed _ i _ -> Just ("failed " <> nm i)
      FalseLemma _ i _ -> Just ("false " <> nm i)
      AttemptLaunched _ _ i -> Just ("launch " <> nm i)
      AttemptEnded _ _ i _ -> Just ("end " <> nm i)
      RoleCost {} -> Nothing
      GraphStopped _ why -> Just ("stopped " <> why)
      NodeAutoClosed _ i _ -> Just ("auto closed " <> nm i)
      NodeRepaired _ _ i _ _ -> Just ("repaired " <> nm i)
      NodeDisproved _ i kind _ _ _ -> Just ("disproved " <> nm i <> " " <> kind)
      GraphAutoOff {} -> Just "auto off"
      _ -> Nothing

precedes :: [Text] -> Text -> Text -> Bool
precedes ts a b = case (findIndex (== a) ts, findIndex (== b) ts) of
  (Just i, Just j) -> i < j
  _ -> False

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

infixOf :: Eq a => [a] -> [a] -> Bool
infixOf xs ys = any (xs `isPrefixOf`) (tails ys)
