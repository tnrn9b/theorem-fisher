{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The Scheduler's effects behind one record ('Orchestrator'): spawning
-- a job, waiting for the first of some to finish, cancelling one, the
-- clock, the run log, a capped step and a cancelled job's price. The
-- record is polymorphic in the job's spec and result; the flat search's
-- is 'AttemptOrchestrator'.
--
-- 'realOrchestrator' is the behaviour the Scheduler had inline: threads,
-- the wall clock and the run log. 'simOrchestrator' runs a 'Script'
-- instead: which job finishes when, in virtual seconds, with what
-- result. It never sleeps and never forks, so an ordering rule can be
-- tested deterministically, and its 'SimTrace' records what was done.
--
-- 'recordOrchestrator' wraps any orchestrator and records what it did as
-- a 'Recording', which 'recordingScript' turns into a 'Script' that
-- replays the run through the simulator. The flat search's recording
-- ('AttemptScript') has JSON instances, so a run can be written to a
-- file and replayed later.
module Fisher.Search.Orchestrate
  ( Running
  , Orchestrator (..)
  , AttemptOrchestrator
  , realOrchestrator
  , AttemptKey (..)
  , attemptKey
  , Entry (..)
  , Script (..)
  , emptyScript
  , ScriptHit (..)
  , scriptEntry
  , SimTrace (..)
  , simOrchestrator
  , Seconds (..)
  , Recording (..)
  , AttemptScript
  , recordOrchestrator
  , recordingScript
  ) where

import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, poll, waitAnyCatch)
import Control.Exception (ErrorCall (..), SomeAsyncException, SomeException, displayException, fromException, mask_, throwIO, toException, try)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.IORef
import Data.List (find, minimumBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Timeout (timeout)

import Fisher.Search.Attempt

-- | A job in flight, opaque to the Scheduler.
newtype Running = Running Int
  deriving (Eq, Ord, Show)

-- | The Scheduler's effects, for jobs described by @spec@ that give a
-- @res@. Each field's haddock is the contract every interpreter keeps.
data Orchestrator spec res = Orchestrator
  { oSpawn :: spec -> IO Running
  -- ^ start an attempt now and return its handle
  , oAwaitAny :: Double -> [Running] -> IO (Maybe (Running, Either SomeException res))
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
  , oPrice :: spec -> IO (Maybe Double)
  -- ^ the dollars a cancelled attempt spent, when known
  }

-- | The flat search's orchestrator: its jobs are attempts.
type AttemptOrchestrator = Orchestrator AttemptSpec AttemptResult

-- | The real one, from the run function, the clock, the log and the
-- price: each attempt runs on its own thread, waiting polls the handles
-- in list order for one already finished and otherwise is 'waitAnyCatch'
-- under a 'timeout', and a capped step is a 'timeout' rounded as
-- @autoStep@ rounds it (so an asynchronous exception other than the
-- timeout's own goes through it). Cancelling an attempt unknown or
-- already delivered is a no-op; a cancelled attempt is forgotten, so it
-- is never delivered.
realOrchestrator
  :: forall spec res
   . (spec -> IO res)
  -> IO Double
  -> (Event -> IO ())
  -> (spec -> IO (Maybe Double))
  -> IO (Orchestrator spec res)
realOrchestrator run now logE price = do
  handles <- newIORef (Map.empty :: Map.Map Int (Async res))
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

-- | Names a spawned job in a 'Script': the two halves of its key (for an
-- attempt, its goal and its backend) and how many jobs with that key were
-- spawned before it (the first is 0).
data AttemptKey = AttemptKey { akGoal :: Text, akBackend :: Text, akIndex :: Int }
  deriving (Eq, Ord, Show, Generic)

-- | Generic, with the field names as they are.
instance ToJSON AttemptKey

instance FromJSON AttemptKey

-- | The key function for attempts: an attempt's goal and backend.
attemptKey :: AttemptSpec -> (Text, Text)
attemptKey sp = (asGoal sp, asBackend sp)

-- | How a scripted job finishes.
data Entry spec res
  = Scripted (spec -> IO (Either Text res))
  -- ^ the result (or an exception's text) from the spec, computed at
  -- spawn; IO so a harness can record what was spawned. The job's result
  -- is a Left from a Left here or from a synchronous exception the
  -- function throws; an asynchronous one is rethrown from 'oSpawn'
  | Run (spec -> IO res)
  -- ^ run this action at spawn, on the caller's thread, and deliver its
  -- result (or its exception) after the entry's seconds

data Script spec res = Script
  { scAttempts :: [(AttemptKey, Double, Entry spec res)]
  -- ^ per spawned job, the first entry with its key: the virtual
  -- seconds it takes, and how it finishes
  , scDefault :: Maybe (Entry spec res)
  -- ^ how a job no entry names finishes, taking no virtual time;
  -- Nothing makes such a job throw at spawn
  , scCaps :: [(Text, Double)]
  -- ^ named steps that hit their cap, each with the virtual seconds
  -- consumed
  , scPrices :: [(AttemptKey, Double)]
  , scStepCost :: Double
  -- ^ virtual seconds every other capped step consumes, and every await
  -- that delivers a job, after the delivery
  }

-- | Where a spawned job falls in a script.
data ScriptHit spec res = ScriptHit
  { sdKey :: AttemptKey
  , sdSecs :: Double
  -- ^ its virtual seconds (0 for the default)
  , sdEntry :: Entry spec res
  , sdDefaulted :: Bool
  -- ^ no entry names it, so 'scDefault' gives its entry
  }

-- | The script's answer for a job spawned now, given the key function
-- and how many were spawned before with each key: its key (the count is
-- its index), the first entry with that key or else 'scDefault', and the
-- counts with this spawn added. Left names the key of a job neither
-- scripted nor covered by a default.
scriptEntry
  :: (spec -> (Text, Text))
  -> Script spec res
  -> Map.Map (Text, Text) Int
  -> spec
  -> Either Text (ScriptHit spec res, Map.Map (Text, Text) Int)
scriptEntry keyFn sc counts sp =
  let (k, counts') = spawnKey keyFn counts sp
   in case find (\(k', _, _) -> k' == k) (scAttempts sc) of
        Just (_, d, e) -> Right (ScriptHit k d e False, counts')
        Nothing -> case scDefault sc of
          Just e -> Right (ScriptHit k 0 e True, counts')
          Nothing -> Left ("no script entry for " <> T.pack (show k))

-- | The key of a job spawned now, given how many were spawned before
-- with each key, and the counts with this spawn added.
spawnKey :: (spec -> (Text, Text)) -> Map.Map (Text, Text) Int -> spec -> (AttemptKey, Map.Map (Text, Text) Int)
spawnKey keyFn counts sp =
  let pair@(g, b) = keyFn sp
      i = Map.findWithDefault 0 pair counts
   in (AttemptKey g b i, Map.insert pair (i + 1) counts)

-- | No attempts, no default, no caps, no prices, and steps that take no
-- time.
emptyScript :: Script spec res
emptyScript = Script {scAttempts = [], scDefault = Nothing, scCaps = [], scPrices = [], scStepCost = 0}

-- | What the simulator was asked to do, each list oldest first, and its
-- clock.
data SimTrace spec = SimTrace
  { stSpawned :: [spec]
  , stDefaulted :: [spec]
  -- ^ the spawned jobs no entry named, whose entry came from
  -- 'scDefault'
  , stCancelled :: [Running]
  , stEvents :: [Event]
  , stClock :: Double
  }
  deriving (Eq, Show)

-- | The simulator's state; the trace's lists are newest first.
data Sim spec res = Sim
  { smClock :: Double
  , smNext :: Int
  , smKeys :: [(spec, AttemptKey)]
  -- ^ every spawned spec with its key, newest first
  , smCounts :: Map.Map (Text, Text) Int
  -- ^ how many were spawned with each key
  , smDefaulted :: [spec]
  , smDue :: Map.Map Int (Double, Either SomeException res)
  -- ^ the jobs in flight: when each finishes and its result
  , smCancelled :: [Running]
  , smEvents :: [Event]
  }

-- | The simulated one and a reader of its trace, given the key function
-- (how a spec is matched to a script entry: 'attemptKey' for attempts).
-- Virtual time starts at 0, never moves backwards, and moves only through
-- an await (to the job delivered plus 'scStepCost', or to the deadline)
-- and a capped step. An await's deadline is the clock plus its seconds,
-- taken before any step cost; an await with none of its jobs in flight
-- returns at once and leaves the clock alone. A job neither scripted nor
-- covered by 'scDefault', or a price the script does not list, throws an
-- 'ErrorCall' naming its key. A 'Scripted' entry's function and a 'Run'
-- entry's action run at spawn, on the caller's thread, after the spawn is
-- recorded; the job is due at the clock at spawn plus the entry's
-- seconds. A synchronous exception from either is the job's result (a
-- Left); an asynchronous one is rethrown from 'oSpawn', and that job,
-- recorded as spawned, is never delivered. Awaiting delivers, as the real one does, the first in the
-- list of the given jobs already due (at or before now), and otherwise
-- the earliest due, the first in the list among those due together; a
-- cancelled job is never delivered and its exception never thrown.
-- Cancelling a job already delivered, or unknown, is a no-op and is not
-- recorded. A spec is priced as its latest spawn, found by equality.
simOrchestrator :: forall spec res. Eq spec => (spec -> (Text, Text)) -> Script spec res -> IO (Orchestrator spec res, IO (SimTrace spec))
simOrchestrator keyFn script = do
  ref <- newIORef Sim
    { smClock = 0, smNext = 0, smKeys = [], smCounts = Map.empty, smDefaulted = []
    , smDue = Map.empty, smCancelled = [], smEvents = [] }
  let keyOf s sp =
        let pair@(g, b) = keyFn sp
         in AttemptKey g b (Map.findWithDefault 0 pair (smCounts s))
      spawn sp = do
        s <- readIORef ref
        (hit, counts) <- either (throwIO . ErrorCall . ("simOrchestrator: " <>) . T.unpack) pure
          (scriptEntry keyFn script (smCounts s) sp)
        let n = smNext s
            due = smClock s + sdSecs hit
        -- the spawn is recorded before its entry runs, so an entry that
        -- uses this orchestrator sees it
        writeIORef ref s
          { smNext = n + 1
          , smKeys = (sp, sdKey hit) : smKeys s
          , smCounts = counts
          , smDefaulted = if sdDefaulted hit then sp : smDefaulted s else smDefaulted s
          }
        -- a synchronous exception is the job's result; an asynchronous
        -- one is rethrown
        let caught :: IO a -> IO (Either SomeException a)
            caught act = try @SomeException act >>= \case
              Left e | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
              r -> pure r
        res <- case sdEntry hit of
          Scripted f -> (>>= either (Left . toException . ErrorCall . T.unpack) Right) <$> caught (f sp)
          Run f -> caught (f sp)
        modifyIORef' ref (\s' -> s' {smDue = Map.insert n (due, res) (smDue s')})
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
                pure (Just (Running n, res))
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

-- | Seconds in a recording; infinite for a job never delivered, written
-- in JSON as the string @never@ (JSON has no infinite number).
newtype Seconds = Seconds Double
  deriving (Eq, Ord, Show)

instance ToJSON Seconds where
  toJSON (Seconds d)
    | isInfinite d && d > 0 = String "never"
    | otherwise = toJSON d

instance FromJSON Seconds where
  parseJSON (String "never") = pure (Seconds (1 / 0))
  parseJSON v = Seconds <$> parseJSON v

-- | What 'recordOrchestrator' saw, each list oldest first: per spawned
-- job its key, the seconds from its spawn to its delivery (infinite for
-- one cancelled or never delivered) and its result (or the exception's
-- text); the named steps whose cap hit, each with the seconds it was
-- given; the prices given.
data Recording spec res = Recording
  { rcEntries :: [(AttemptKey, Seconds, Either Text res)]
  , rcCaps :: [(Text, Double)]
  , rcPrices :: [(AttemptKey, Double)]
  }
  deriving (Eq, Show, Generic)

-- | Generic, with the field names as they are: tuples are arrays and a
-- result is @{"Left": text}@ or @{"Right": result}@. The spec is not
-- written: on replay the Scheduler builds the specs again.
instance ToJSON res => ToJSON (Recording spec res)

instance FromJSON res => FromJSON (Recording spec res)

-- | The flat search's recording, the file @--record@ writes.
type AttemptScript = Recording AttemptSpec AttemptResult

-- | The recorder's state; its lists are newest first.
data Rec spec res = Rec
  { rkCounts :: Map.Map (Text, Text) Int
  , rkSpawns :: [(Running, spec, AttemptKey, Double)]
  -- ^ every spawn with its key and the clock at spawn
  , rkDone :: Map.Map Running (Seconds, Either Text res)
  -- ^ the jobs delivered or cancelled
  , rkCaps :: [(Text, Double)]
  , rkPrices :: [(AttemptKey, Double)]
  }

-- | A wrapper over an orchestrator (a real one, in use) that records what
-- it did, given the key function, and a reader of the recording so far.
-- Every field is the wrapped one's; besides, a spawn records the job's
-- key (counted as 'scriptEntry' counts) and the clock; an await that
-- delivers a job records the seconds since its spawn and its result (an
-- exception as its text); a cancel of a job not yet delivered records it
-- as never delivered, with the text @cancelled@; a capped step whose cap
-- hit records its name and the seconds it was given (a step that ran is
-- not recorded); a price given records it under the key of the spec's
-- latest spawn, found by equality. A job still in flight when the
-- recording is read is never delivered, with the text @in flight@.
recordOrchestrator
  :: forall spec res
   . Eq spec
  => (spec -> (Text, Text))
  -> Orchestrator spec res
  -> IO (Orchestrator spec res, IO (Recording spec res))
recordOrchestrator keyFn o = do
  ref <- newIORef Rec {rkCounts = Map.empty, rkSpawns = [], rkDone = Map.empty, rkCaps = [], rkPrices = []}
  let never = Seconds (1 / 0)
      spawn sp = do
        t0 <- oNow o
        r <- oSpawn o sp
        atomicModifyIORef' ref $ \s ->
          let (k, counts) = spawnKey keyFn (rkCounts s) sp
           in (s {rkCounts = counts, rkSpawns = (r, sp, k, t0) : rkSpawns s}, ())
        pure r
      awaitAny secs rs = do
        got <- oAwaitAny o secs rs
        forM_ got $ \(r, res) -> do
          t1 <- oNow o
          let res' = either (Left . T.pack . displayException) Right res
          atomicModifyIORef' ref $ \s -> case [t0 | (r', _, _, t0) <- rkSpawns s, r' == r] of
            t0 : _ -> (s {rkDone = Map.insert r (Seconds (t1 - t0), res') (rkDone s)}, ())
            [] -> (s, ())
        pure got
      cancelOne r = do
        oCancel o r
        -- one delivered or already cancelled keeps its entry
        atomicModifyIORef' ref $ \s ->
          if any (\(r', _, _, _) -> r' == r) (rkSpawns s)
            then (s {rkDone = Map.insertWith (\_ old -> old) r (never, Left "cancelled") (rkDone s)}, ())
            else (s, ())
      capped :: Text -> Double -> IO a -> IO (Maybe a)
      capped name secs act = do
        got <- oCapped o name secs act
        when (isNothing got) $ atomicModifyIORef' ref (\s -> (s {rkCaps = (name, secs) : rkCaps s}, ()))
        pure got
      priceOf sp = do
        got <- oPrice o sp
        forM_ got $ \d -> atomicModifyIORef' ref $ \s ->
          let k = case [k' | (_, sp', k', _) <- rkSpawns s, sp' == sp] of
                k' : _ -> k'
                [] -> fst (spawnKey keyFn (rkCounts s) sp)
           in (s {rkPrices = (k, d) : rkPrices s}, ())
        pure got
      recording = do
        s <- readIORef ref
        pure Recording
          { rcEntries =
              [ (k, secs, res)
              | (r, _, k, _) <- reverse (rkSpawns s)
              , let (secs, res) = fromMaybe (never, Left "in flight") (Map.lookup r (rkDone s)) ]
          , rcCaps = reverse (rkCaps s)
          , rcPrices = reverse (rkPrices s)
          }
      orch = Orchestrator
        { oSpawn = spawn
        , oAwaitAny = awaitAny
        , oCancel = cancelOne
        , oNow = oNow o
        , oLog = oLog o
        , oCapped = capped
        , oPrice = priceOf
        }
  pure (orch, recording)

-- | The script that replays a recording through 'simOrchestrator': each
-- entry's constant result after its seconds (one never delivered is never
-- due, so it is cancelled on replay as it was in the run), the caps and
-- prices as recorded, no default and no step cost.
recordingScript :: Recording spec res -> Script spec res
recordingScript rc = Script
  { scAttempts = [(k, secs, Scripted (const (pure res))) | (k, Seconds secs, res) <- rcEntries rc]
  , scDefault = Nothing
  , scCaps = rcCaps rc
  , scPrices = rcPrices rc
  , scStepCost = 0
  }
