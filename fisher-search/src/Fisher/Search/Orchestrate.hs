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
  , SimTrace (..)
  , simOrchestrator
  ) where

import Control.Concurrent.Async (Async, async, cancel, waitAnyCatch)
import Control.Exception (ErrorCall (..), SomeException, throwIO, toException)
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

data Orchestrator = Orchestrator
  { oSpawn :: AttemptSpec -> IO Running
  , oAwaitAny :: Double -> [Running] -> IO (Maybe (Running, Either SomeException AttemptResult))
  -- ^ the first of these to finish, or Nothing when the given seconds pass
  -- first (at once when none of them is still in flight); one delivered is
  -- gone
  , oCancel :: Running -> IO ()
  , oNow :: IO Double
  , oLog :: Event -> IO ()
  , oCapped :: forall a. Text -> Double -> IO a -> IO (Maybe a)
  -- ^ run a named step under a wall cap in seconds; Nothing when the cap
  -- hits (the step name is for the simulator's script and the
  -- @AutoTimeout@ event)
  , oPrice :: AttemptSpec -> IO (Maybe Double)
  }

-- | The real one, from the run function, the clock, the log and the
-- price: each attempt runs on its own thread, waiting is 'waitAnyCatch'
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
  let spawn sp = do
        a <- async (run sp)
        n <- atomicModifyIORef' counter (\c -> (c + 1, c))
        atomicModifyIORef' handles (\m -> (Map.insert n a m, ()))
        pure (Running n)
      awaitAny secs rs = do
        m <- readIORef handles
        let live = [(r, a) | r@(Running n) <- rs, Just a <- [Map.lookup n m]]
        if null live
          then pure Nothing
          else do
            got <- timeout (max 0 (ceiling (secs * 1000000))) (waitAnyCatch (map snd live))
            case got of
              Nothing -> pure Nothing
              Just (a, res) -> case find ((== a) . snd) live of
                Nothing -> pure Nothing
                Just (r@(Running n), _) -> do
                  atomicModifyIORef' handles (\m' -> (Map.delete n m', ()))
                  pure (Just (r, res))
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
  -- ^ virtual seconds every other capped step and every await consumes
  }

-- | No attempts, no default, no caps, no prices, and steps that take no
-- time.
emptyScript :: Script
emptyScript = Script {scAttempts = [], scDefault = Nothing, scCaps = [], scPrices = [], scStepCost = 0}

-- | What the simulator was asked to do, each list oldest first, and its
-- clock.
data SimTrace = SimTrace
  { stSpawned :: [AttemptSpec]
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
  , smDue :: Map.Map Int (Double, Either Text AttemptResult)
  -- ^ the attempts in flight: when each finishes and its result
  , smCancelled :: [Running]
  , smEvents :: [Event]
  }

-- | The simulated one and a reader of its trace. Virtual time starts at
-- 0 and moves only through an await (to the attempt delivered, or to the
-- deadline) and a capped step. An attempt neither scripted nor covered by
-- 'scDefault', or a price the script does not list, throws an 'ErrorCall'
-- naming its key. Awaiting delivers the earliest due of the given
-- attempts, the one spawned first among those due together; a cancelled
-- attempt is never delivered and its exception never thrown. Cancelling
-- an attempt already delivered, or unknown, is a no-op and is not
-- recorded.
simOrchestrator :: Script -> IO (Orchestrator, IO SimTrace)
simOrchestrator script = do
  ref <- newIORef Sim {smClock = 0, smNext = 0, smKeys = [], smDue = Map.empty, smCancelled = [], smEvents = []}
  let keyOf s sp = AttemptKey (asGoal sp) (asBackend sp)
        (length [() | (_, k) <- smKeys s, akGoal k == asGoal sp, akBackend k == asBackend sp])
      spawn sp = do
        s <- readIORef ref
        let k = keyOf s sp
        (secs, res) <- case find (\(k', _, _) -> k' == k) (scAttempts script) of
          Just (_, d, f) -> pure (d, f sp)
          Nothing -> case scDefault script of
            Just f -> pure (0, f sp)
            Nothing -> throwIO (ErrorCall ("simOrchestrator: no script entry for " <> show k))
        let n = smNext s
        writeIORef ref s
          { smNext = n + 1
          , smKeys = (sp, k) : smKeys s
          , smDue = Map.insert n (smClock s + secs, res) (smDue s)
          }
        pure (Running n)
      awaitAny secs rs = do
        modifyIORef' ref (\s -> s {smClock = smClock s + scStepCost script})
        s <- readIORef ref
        let now = smClock s
            live = [(due, n, res) | Running n <- rs, Just (due, res) <- [Map.lookup n (smDue s)]]
        case live of
          [] -> pure Nothing
          _ -> do
            let (due, n, res) = minimumBy (comparing (\(d, i, _) -> (d, i))) live
            if due <= now + secs
              then do
                writeIORef ref s {smClock = max now due, smDue = Map.delete n (smDue s)}
                pure (Just (Running n, either (Left . toException . ErrorCall . T.unpack) Right res))
              else do
                writeIORef ref s {smClock = now + secs}
                pure Nothing
      cancelOne r@(Running n) = modifyIORef' ref $ \s ->
        if Map.member n (smDue s)
          then s {smDue = Map.delete n (smDue s), smCancelled = r : smCancelled s}
          else s
      capped :: Text -> Double -> IO a -> IO (Maybe a)
      capped name secs act = case lookup name (scCaps script) of
        Just c -> do
          modifyIORef' ref (\s -> s {smClock = smClock s + min c secs})
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
