-- | The scripted graph run the GraphRun specs share: workers that write a
-- given proof, scripted planner and verifier replies, an optional fake
-- language server, and what the run did.
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
  , Out (..)
  , runG
  , runGWith
  , runGAuto
  , runGRet
  , runGPriced
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

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, asyncThreadId, waitCatch)
import Control.Exception (AsyncException (UserInterrupt), SomeException, finally, throwTo)
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

import Fisher.Search.Auto (Lsp (..))
import Fisher.Search.Check (AxiomPolicy (..), LeanRun (..), standardAxioms)
import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Controller (ModelFailure, Stop)
import Fisher.Search.FakeLean (fakeLeanBy)
import Fisher.Search.Graph
import Fisher.Search.GraphRun
import Fisher.Search.Lean
import Fisher.Search.Library (Library, loadLibrary)
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

-- | One worker attempt: the proof written, a delay in ms, the final text,
-- dollars (the child's own), seconds the clock moves, and the controller's
-- fields of its result: the stop, the dollars from its ledger, its last
-- message and a model failure.
data Run = Run
  { rProof :: Text, rDelay :: Int, rFinal :: Maybe Text, rCost :: Double, rClock :: Double
  , rStop :: Maybe Stop, rDollars :: Maybe Double, rLast :: Maybe Text, rFailure :: Maybe ModelFailure }

w :: Text -> Run
w p = Run
  { rProof = p, rDelay = 0, rFinal = Nothing, rCost = 0, rClock = 0
  , rStop = Nothing, rDollars = Nothing, rLast = Nothing, rFailure = Nothing }

ok, bad :: Run
ok = w "exact done"
bad = w "BAD"

-- | One model reply: the role, a text the last message must contain, the reply, a delay in ms.
data Reply = Reply Role (Maybe Text) (Either AskError (Text, Double)) Int

say :: Role -> Text -> Reply
say r t = Reply r Nothing (Right (t, 0)) 0

sayTo :: Role -> Text -> Text -> Reply
sayTo r m t = Reply r (Just m) (Right (t, 0)) 0

toNode :: Text -> Text
toNode n = "declaration `" <> n <> "`"

garbage :: Text
garbage = "I cannot split this."

reject :: Text -> Text
reject why = "{\"accept\": false, \"reason\": \"" <> why <> "\", \"suspect\": []}"

data Out = Out
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

runG :: GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO Out
runG = runGWith keyLean

-- | 'runG' on a given Lean stand-in.
runGWith :: LeanRun -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO Out
runGWith leanRun = runGAuto leanRun Nothing

-- | 'runGWith' with the automation: its settings and server, given the trace to log calls to.
runGAuto :: LeanRun -> Maybe (IORef [Text] -> AutoEnv) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO Out
runGAuto = runGRet (\_ -> pure Nothing)

-- | 'runGAuto' with retrieval, made from the run's directory once F.lean
-- is there.
runGRet :: (FilePath -> IO (Maybe RetrievalEnv)) -> LeanRun -> Maybe (IORef [Text] -> AutoEnv) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO Out
runGRet = runGPriced (\_ -> pure Nothing)

-- | 'runGRet' with a price for cancelled workers ('sePrice').
runGPriced :: (AttemptSpec -> IO (Maybe Double)) -> (FilePath -> IO (Maybe RetrievalEnv)) -> LeanRun -> Maybe (IORef [Text] -> AutoEnv) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply] -> IO Out
runGPriced price mkRet leanRun mkAuto gcfg file runs0 replies0 = withSystemTempDirectory "graph" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  ret <- mkRet root
  clock <- newIORef (0 :: Double)
  runsRef <- newIORef (Map.fromList runs0)
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
  let seRunF sp = do
        traced ("worker " <> asGoal sp)
        modifyIORef' launched (asGoal sp :)
        modifyIORef' briefs ((asGoal sp, asBrief sp) :)
        modifyIORef' starts ((asGoal sp, asStart sp) :)
        modifyIORef' specs (sp :)
        r <- atomicModifyIORef' runsRef $ \m -> case Map.findWithDefault [] (asGoal sp) m of
          (x : xs) -> (Map.insert (asGoal sp) xs m, x)
          [] -> (m, bad)
        threadDelay (rDelay r * 1000)
        modifyIORef' clock (+ rClock r)
        let out = withProof (asGoal sp) (rProof r) (asStart sp)
            stats = ChildStats (Just 0) 0 Nothing Nothing Nothing (Just (rCost r)) (rFinal r) Map.empty
        pure (plainResult (Just out) (Just stats) "")
          { arStop = rStop r, arDollars = rDollars r, arLastMessage = rLast r, arFailure = rFailure r }
      ask role msgs = do
        traced ("ask " <> T.toLower (T.pack (show role)))
        let lastText = maybe "" snd (if null msgs then Nothing else Just (last msgs))
            fits (Reply r m _ _) = r == role && maybe True (`T.isInfixOf` lastText) m
        pick <- atomicModifyIORef' repliesRef $ \rs -> case break fits rs of
          (pre, x : post) -> (pre <> post, Just x)
          _ -> (rs, Nothing)
        atomicModifyIORef' asks (\a -> ((role, msgs) : a, ()))
        case pick of
          Nothing -> pure (Left (AskFailed "nothing scripted"))
          Just (Reply _ _ (Left (AskFailed "THROW")) _) -> ioError (userError "the connection broke")
          Just (Reply _ _ out delay) -> threadDelay (delay * 1000) >> pure out
      senv = SearchEnv seRunF lean (AxiomPolicy standardAxioms False) root "F.lean"
               (readIORef clock) (\_ -> pure ()) (\_ -> pure ()) (fmap (\mk -> watched (mk trace)) mkAuto) ret price
      genv = GraphEnv senv ask (\e -> atomicModifyIORef' events (\es -> (e : es, ()))) (\_ _ -> pure ())
  r <- timeout 20000000 (runGraph gcfg genv file)
  rep <- case r of
    Nothing -> fail "the run did not end"
    Just (Left e) -> fail (T.unpack e)
    Just (Right x) -> pure x
  Out rep <$> (reverse <$> readIORef events) <*> (reverse <$> readIORef asks) <*> (reverse <$> readIORef launched)
    <*> (reverse <$> readIORef probes) <*> (reverse <$> readIORef briefs) <*> (reverse <$> readIORef starts)
    <*> readIORef trace <*> readIORef overlaps <*> maybe (pure ([], [])) (loadLibrary . reLibDir) ret
    <*> (reverse <$> readIORef specs)

-- | A graph run with retrieval interrupted (Ctrl-C's exception thrown to
-- its thread) once an event satisfies the predicate: what it ended with,
-- its events, and the library afterwards.
runGInterrupted
  :: (GraphEvent -> Bool) -> (FilePath -> IO (Maybe RetrievalEnv)) -> GraphConfig -> Text -> [(Text, [Run])] -> [Reply]
  -> IO (Either SomeException (Either Text GraphReport), [GraphEvent], Library)
runGInterrupted stopAt mkRet gcfg file runs0 replies0 = withSystemTempDirectory "graph" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  ret <- mkRet root
  runsRef <- newIORef (Map.fromList runs0)
  repliesRef <- newIORef replies0
  events <- newIORef []
  let seRunF sp = do
        r <- atomicModifyIORef' runsRef $ \m -> case Map.findWithDefault [] (asGoal sp) m of
          (x : xs) -> (Map.insert (asGoal sp) xs m, x)
          [] -> (m, bad)
        threadDelay (rDelay r * 1000)
        let stats = ChildStats (Just 0) 0 Nothing Nothing Nothing (Just (rCost r)) (rFinal r) Map.empty
        pure (plainResult (Just (withProof (asGoal sp) (rProof r) (asStart sp))) (Just stats) "")
      ask role _ = do
        pick <- atomicModifyIORef' repliesRef $ \rs -> case break (\(Reply r _ _ _) -> r == role) rs of
          (pre, x : post) -> (pre <> post, Just x)
          _ -> (rs, Nothing)
        pure $ case pick of
          Just (Reply _ _ out _) -> out
          Nothing -> Left (AskFailed "nothing scripted")
      senv = SearchEnv seRunF keyLean (AxiomPolicy standardAxioms False) root "F.lean"
               (pure 0) (\_ -> pure ()) (\_ -> pure ()) Nothing ret (\_ -> pure Nothing)
      genv = GraphEnv senv ask (\e -> atomicModifyIORef' events (\es -> (e : es, ()))) (\_ _ -> pure ())
  a <- async (runGraph gcfg genv file)
  let waitFor n = do
        es <- readIORef events
        if any stopAt es || n > (2000 :: Int) then pure () else threadDelay 10000 >> waitFor (n + 1)
  waitFor 0
  throwTo (asyncThreadId a) UserInterrupt
  r <- waitCatch a
  es <- reverse <$> readIORef events
  lib <- maybe (pure ([], [])) (loadLibrary . reLibDir) ret
  pure (r, es, lib)

-- | The split of a node, by the node's name and the split's position among its splits.
splitOf :: Out -> Text -> Int -> Split
splitOf o n k = gSplits (grGraph (oRep o)) IM.! (nSplits (nodeBy o n) !! k)

declsNamed :: Out -> Text -> [Decl]
declsNamed o n = [d | d <- declarations (grBase (oRep o)), dName d == n]

cfg :: GraphConfig
cfg = defaultGraphConfig

nodeBy :: Out -> Text -> Node
nodeBy o n = case [x | x <- IM.elems (gNodes (grGraph (oRep o))), nName x == n] of
  (x : _) -> x
  [] -> error ("no node " <> T.unpack n)

statusOf :: Out -> Text -> NodeStatus
statusOf o = nStatus . nodeBy o

-- | Events as short tags (costs and the roots' NodeAdded left out), with node names.
tags :: Out -> [Text]
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
      NodeDisproved _ i kind _ -> Just ("disproved " <> nm i <> " " <> kind)
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
