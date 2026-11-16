{-# LANGUAGE RankNTypes #-}

-- | The Scheduler's effects behind one record ('Orchestrator'): spawning
-- an attempt, waiting for the first of some to finish, cancelling one,
-- the clock, the run log, a capped step and a cancelled attempt's price.
--
-- 'realOrchestrator' is the behaviour the Scheduler had inline: threads,
-- the wall clock and the run log. 'simOrchestrator' runs a 'Script'
-- instead: which attempt finishes when, in virtual seconds, with what
-- result. It never sleeps and never forks, so an ordering rule can be
-- tested deterministically, and its 'SimTrace' records what was done.
module Fisher.Search.Orchestrate
  ( Running
  , Orchestrator (..)
  , realOrchestrator
  , AttemptKey (..)
  , Script (..)
  , emptyScript
  , Scripted (..)
  , scriptEntry
  , SimTrace (..)
  , simOrchestrator
  ) where

import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, poll, waitAnyCatch)
import Control.Exception (ErrorCall (..), SomeException, mask_, throwIO, toException)
import Data.IORef
import Data.List (find, minimumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Fisher.Search.Attempt

-- | An attempt in flight, opaque to the Scheduler.
newtype Running = Running Int
  deriving (Eq, Ord, Show)

-- | The Scheduler's effects. Each field's haddock is the contract every
-- interpreter keeps.
data Orchestrator = Orchestrator
  { oSpawn :: AttemptSpec -> IO Running
  -- ^ start an attempt now and return its handle
  , oAwaitAny :: Double -> [Running] -> IO (Maybe (Running, Either SomeException AttemptResult))
  -- ^ the first of these to finish, with its result or its exception;
  -- Nothing when the given seconds pass first, and Nothing at once when
  -- none of them is still in flight. An attempt already finished is
  -- delivered in list order (even with zero seconds); otherwise the first
  -- to finish, the one first in the list among those finishing together.
  -- An attempt delivered is gone: it is never delivered again.
  , oCancel :: Running -> IO ()
  -- ^ stop an attempt, so it is never delivered; cancelling one already
  -- delivered, or unknown, is a no-op
  , oNow :: IO Double
  -- ^ the clock, in seconds
  , oLog :: Event -> IO ()
  -- ^ write one event to the run log
  , oCapped :: forall a. Text -> Double -> IO a -> IO (Maybe a)
  -- ^ run a named step under a wall cap in seconds; Nothing when the cap
  -- hits. The name is for the simulator's script; it is not logged.
  , oPrice :: AttemptSpec -> IO (Maybe Double)
  -- ^ the dollars a cancelled attempt spent, when known
  }

-- | The real one, from the run function, the clock, the log and the
-- price: each attempt runs on its own thread, waiting polls the handles
-- in list order for one already finished and otherwise is 'waitAnyCatch'
-- under a 'timeout', and a capped step is a 'timeout' rounded as
-- @autoStep@ rounds it (so an asynchronous exception other than the
-- timeout's own goes through it). Cancelling an attempt unknown or
-- already delivered is a no-op; a cancelled attempt is forgotten, so it
-- is never delivered.
realOrchestrator
  :: (AttemptSpec -> IO AttemptResult)
  -> IO Double
  -> (Event -> IO ())
  -> (AttemptSpec -> IO (Maybe Double))
  -> IO Orchestrator
realOrchestrator run now logE price = do
  handles <- newIORef (Map.empty :: Map.Map Int (Async AttemptResult))
  counter <- newIORef (0 :: Int)
  -- the fork and the handle's record are one step, masked, so no
  -- asynchronous exception can leave a thread nobody can cancel; the
  -- attempt itself runs unmasked
  let spawn sp = mask_ $ do
        a <- asyncWithUnmask (\unmask -> unmask (run sp))
        n <- atomicModifyIORef' counter (\c -> (c + 1, c))
        atomicModifyIORef' handles (\m -> (Map.insert n a m, ()))
        pure (Running n)
      awaitAny secs rs = do
        m <- readIORef handles
        let live = [(r, a) | r@(Running n) <- rs, Just a <- [Map.lookup n m]]
            deliver r@(Running n) res = do
              atomicModifyIORef' handles (\m' -> (Map.delete n m', ()))
              pure (Just (r, res))
            -- the first in list order already finished, so that zero
            -- seconds still delivers it
            firstDone [] = pure Nothing
            firstDone ((r, a) : more) = poll a >>= maybe (firstDone more) (pure . Just . (,) r)
        if null live
          then pure Nothing
          else firstDone live >>= \case
            Just (r, res) -> deliver r res
            Nothing -> do
              got <- timeout (max 0 (ceiling (secs * 1000000))) (waitAnyCatch (map snd live))
              case got of
                Nothing -> pure Nothing
                Just (a, res) -> case find ((== a) . snd) live of
                  -- waitAnyCatch returns one of the handles it was given
                  Nothing -> error "realOrchestrator: waitAnyCatch returned a handle it was not given"
                  Just (r, _) -> deliver r res
      cancelOne (Running n) = Map.lookup n <$> readIORef handles >>= mapM_ (\a -> do
        cancel a
        atomicModifyIORef' handles (\m -> (Map.delete n m, ())))
  pure Orchestrator
    { oSpawn = spawn
    , oAwaitAny = awaitAny
    , oCancel = cancelOne
    , oNow = now
    , oLog = logE
    , oCapped = \_ secs act -> timeout (max 1 (ceiling (secs * 1000000))) act
    , oPrice = price
    }

-- | Names a spawned attempt in a 'Script': its goal, its backend and how
-- many attempts on that goal and backend were spawned before it (the
-- first is 0).
data AttemptKey = AttemptKey { akGoal :: Text, akBackend :: Text, akIndex :: Int }
  deriving (Eq, Ord, Show)

data Script = Script
  { scAttempts :: [(AttemptKey, Double, AttemptSpec -> Either Text AttemptResult)]
  -- ^ per spawned attempt, the first entry with its key: the virtual
  -- seconds it takes, and from its spec its result or an exception's text
  , scDefault :: Maybe (AttemptSpec -> Either Text AttemptResult)
  -- ^ the result of an attempt no entry names, taking no virtual time;
  -- Nothing makes such an attempt throw at spawn
  , scCaps :: [(Text, Double)]
  -- ^ named steps that hit their cap, each with the virtual seconds
  -- consumed
  , scPrices :: [(AttemptKey, Double)]
  , scStepCost :: Double
  -- ^ virtual seconds every other capped step consumes, and every await
  -- that delivers an attempt, after the delivery
  }

-- | Where a spawned attempt falls in a script.
data Scripted = Scripted
  { sdKey :: AttemptKey
  , sdSecs :: Double
  -- ^ its virtual seconds (0 for the default)
  , sdResult :: AttemptSpec -> Either Text AttemptResult
  , sdDefaulted :: Bool
  -- ^ no entry names it, so 'scDefault' gives its result
  }

-- | The script's answer for an attempt spawned now, given how many were
-- spawned before on each goal and backend: its key (the count is its
-- index), the first entry with that key or else 'scDefault', and the
-- counts with this spawn added. Left names the key of an attempt neither
-- scripted nor covered by a default.
scriptEntry :: Script -> Map.Map (Text, Text) Int -> AttemptSpec -> Either Text (Scripted, Map.Map (Text, Text) Int)
scriptEntry sc counts sp =
  let pair = (asGoal sp, asBackend sp)
      i = Map.findWithDefault 0 pair counts
      k = AttemptKey (asGoal sp) (asBackend sp) i
      counts' = Map.insert pair (i + 1) counts
   in case find (\(k', _, _) -> k' == k) (scAttempts sc) of
        Just (_, d, f) -> Right (Scripted k d f False, counts')
        Nothing -> case scDefault sc of
          Just f -> Right (Scripted k 0 f True, counts')
          Nothing -> Left ("no script entry for " <> T.pack (show k))

-- | No attempts, no default, no caps, no prices, and steps that take no
-- time.
emptyScript :: Script
emptyScript = Script {scAttempts = [], scDefault = Nothing, scCaps = [], scPrices = [], scStepCost = 0}

-- | What the simulator was asked to do, each list oldest first, and its
-- clock.
data SimTrace = SimTrace
  { stSpawned :: [AttemptSpec]
  , stDefaulted :: [AttemptSpec]
  -- ^ the spawned attempts no entry named, whose result came from
  -- 'scDefault'
  , stCancelled :: [Running]
  , stEvents :: [Event]
  , stClock :: Double
  }
  deriving (Eq, Show)

-- | The simulator's state; the trace's lists are newest first.
data Sim = Sim
  { smClock :: Double
  , smNext :: Int
  , smKeys :: [(AttemptSpec, AttemptKey)]
  -- ^ every spawned spec with its key, newest first
  , smCounts :: Map.Map (Text, Text) Int
  -- ^ how many were spawned on each goal and backend
  , smDefaulted :: [AttemptSpec]
  , smDue :: Map.Map Int (Double, Either Text AttemptResult)
  -- ^ the attempts in flight: when each finishes and its result
  , smCancelled :: [Running]
  , smEvents :: [Event]
  }

-- | The simulated one and a reader of its trace. Virtual time starts at
-- 0, never moves backwards, and moves only through an await (to the
-- attempt delivered plus 'scStepCost', or to the deadline) and a capped
-- step. An await's deadline is the clock plus its seconds, taken before
-- any step cost; an await with none of its attempts in flight returns at
-- once and leaves the clock alone. An attempt neither scripted nor covered by
-- 'scDefault', or a price the script does not list, throws an 'ErrorCall'
-- naming its key. Awaiting delivers, as the real one does, the first in
-- the list of the given attempts already due (at or before now), and
-- otherwise the earliest due, the first in the list among those due
-- together; a cancelled attempt is never delivered and its exception never thrown. Cancelling
-- an attempt already delivered, or unknown, is a no-op and is not
-- recorded.
simOrchestrator :: Script -> IO (Orchestrator, IO SimTrace)
simOrchestrator script = do
  ref <- newIORef Sim
    { smClock = 0, smNext = 0, smKeys = [], smCounts = Map.empty, smDefaulted = []
    , smDue = Map.empty, smCancelled = [], smEvents = [] }
  let keyOf s sp = AttemptKey (asGoal sp) (asBackend sp) (Map.findWithDefault 0 (asGoal sp, asBackend sp) (smCounts s))
      spawn sp = do
        s <- readIORef ref
        (hit, counts) <- either (throwIO . ErrorCall . ("simOrchestrator: " <>) . T.unpack) pure
          (scriptEntry script (smCounts s) sp)
        let n = smNext s
        writeIORef ref s
          { smNext = n + 1
          , smKeys = (sp, sdKey hit) : smKeys s
          , smCounts = counts
          , smDefaulted = if sdDefaulted hit then sp : smDefaulted s else smDefaulted s
          , smDue = Map.insert n (smClock s + sdSecs hit, sdResult hit sp) (smDue s)
          }
        pure (Running n)
      awaitAny secs rs = do
        s <- readIORef ref
        let now = smClock s
            deadline = now + max 0 secs
            live = [(due, pos, n, res) | (pos, Running n) <- zip [0 :: Int ..] rs, Just (due, res) <- [Map.lookup n (smDue s)]]
        case live of
          [] -> pure Nothing
          _ -> do
            -- the first listed already due (the real side's poll), else
            -- the earliest due, and of those due together the first listed
            let (due, _, n, res) = case [x | x@(d, _, _, _) <- live, d <= now] of
                  x : _ -> x
                  [] -> minimumBy (comparing (\(d, pos, _, _) -> (d, pos))) live
            if due <= deadline
              then do
                writeIORef ref s {smClock = max now due + scStepCost script, smDue = Map.delete n (smDue s)}
                pure (Just (Running n, either (Left . toException . ErrorCall . T.unpack) Right res))
              else do
                writeIORef ref s {smClock = deadline}
                pure Nothing
      cancelOne r@(Running n) = modifyIORef' ref $ \s ->
        if Map.member n (smDue s)
          then s {smDue = Map.delete n (smDue s), smCancelled = r : smCancelled s}
          else s
      capped :: Text -> Double -> IO a -> IO (Maybe a)
      capped name secs act = case lookup name (scCaps script) of
        Just c -> do
          modifyIORef' ref (\s -> s {smClock = smClock s + min c (max 0 secs)})
          pure Nothing
        Nothing -> do
          modifyIORef' ref (\s -> s {smClock = smClock s + scStepCost script})
          Just <$> act
      priceOf sp = do
        s <- readIORef ref
        -- a spec spawned more than once is priced as its latest spawn
        let k = maybe (keyOf s sp) snd (find ((== sp) . fst) (smKeys s))
        case lookup k (scPrices script) of
          Just d -> pure (Just d)
          Nothing -> throwIO (ErrorCall ("simOrchestrator: no price for " <> show k))
      trace = do
        s <- readIORef ref
        pure SimTrace
          { stSpawned = reverse [sp | (sp, _) <- smKeys s]
          , stDefaulted = reverse (smDefaulted s)
          , stCancelled = reverse (smCancelled s)
          , stEvents = reverse (smEvents s)
          , stClock = smClock s
          }
      orch = Orchestrator
        { oSpawn = spawn
        , oAwaitAny = awaitAny
        , oCancel = cancelOne
        , oNow = smClock <$> readIORef ref
        , oLog = \e -> modifyIORef' ref (\s -> s {smEvents = e : smEvents s})
        , oCapped = capped
        , oPrice = priceOf
        }
  pure (orch, trace)
