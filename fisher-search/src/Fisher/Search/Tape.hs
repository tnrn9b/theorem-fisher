{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The tape of a run: 'recordOrchestrator' wraps any orchestrator and
-- records its answers, and every event logged through it, in call order
-- ('Recording'); 'replayOrchestrator' plays the tape back. Every answer and
-- every event logged through the orchestrator is compared with the tape,
-- and a difference ends the replay with 'ReplayDiverged', so what a replay
-- runs again (Lean, the language server, retrieval) can only fail loudly.
-- The flat search's tape ('AttemptScript') has JSON instances, so a run
-- can be written to a file and replayed later.
module Fisher.Search.Tape
  ( Step (..)
  , Recording (..)
  , tapeOf
  , tapeVersion
  , AttemptScript
  , recordOrchestrator
  , ReplayDiverged (..)
  , replayOrchestrator
  ) where

import Control.Exception (ErrorCall (..), Exception, SomeAsyncException, SomeException, displayException, fromException, throwIO, toException, try, uninterruptibleMask_)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Timeout (timeout)

import Fisher.Learn.Policy (GoalPolicy (..))
import Fisher.Search.Attempt
import Fisher.Search.Orchestrate (AttemptKey (..), Orchestrator (..), Running (..), spawnKey)

-- | One answer of an orchestrator, or one event logged through it, in a
-- 'Recording'.
data Step res
  = Now Double
  -- ^ the clock read
  | Spawned AttemptKey
  -- ^ a job spawned, with its key
  | Delivered Double AttemptKey Double (Either Text res)
  -- ^ an await of these seconds delivered this job: the clock after it
  -- (for readers; replay does not use it), and the result (an exception
  -- as its text)
  | AwaitTimedOut Double Double
  -- ^ an await of these seconds delivered nothing: the clock after it
  -- (for readers)
  | Cancelled AttemptKey
  | Capped Text Double Bool [Event]
  -- ^ a capped step: its name, its seconds, whether the cap hit, and
  -- for a hit the events its action logged before the cap hit, in order
  -- (empty for a step that ran: its events follow it as 'Logged' steps).
  -- A step whose action threw (a synchronous exception, not the cap) is
  -- one that ran: the calls made inside it follow it, and on replay its
  -- action runs again and throws again
  | Priced AttemptKey (Maybe Double)
  -- ^ a price asked for, and the answer the caller used (Nothing when
  -- the price threw)
  | Logged Event
  -- ^ an event logged through the orchestrator
  deriving (Eq, Show, Generic)

-- | Generic: an object with the constructor's name under @tag@ and its
-- fields under @contents@.
instance ToJSON res => ToJSON (Step res)

instance FromJSON res => FromJSON (Step res)

-- | An orchestrator's answers and logged events in call order: what
-- 'recordOrchestrator' saw and what 'replayOrchestrator' plays, with the
-- tape format's version ('tapeVersion'), the learned policy the run
-- applied (each goal's, exploration included; empty for none) and the
-- @learn@ value the run printed its learn line from ('Null' for none).
-- The recorder cannot know the policy, so the caller sets the last two
-- before writing the tape, and a replay runs that policy. The spec is
-- not recorded: on replay the Scheduler builds the specs again, and each
-- spawn's key is checked against the tape.
data Recording spec res = Recording
  { rcVersion :: Int
  , rcSteps :: [Step res]
  , rcPolicy :: Map.Map Text GoalPolicy
  , rcLearn :: Value
  }
  deriving (Eq, Show, Generic)

-- | A tape of these steps, of this build's version, with no policy.
tapeOf :: [Step res] -> Recording spec res
tapeOf steps = Recording tapeVersion steps Map.empty Null

-- | The version of the tape format this build writes and reads.
tapeVersion :: Int
tapeVersion = 1

-- | @{"rcVersion": 1, "rcSteps": [...], "rcPolicy": {goal: {"policy":
-- ..., "explore": bool}}, "rcLearn": ...}@: a goal's policy as the
-- policy file writes it, and its exploration beside it (the policy
-- file does not hold it).
instance ToJSON res => ToJSON (Recording spec res) where
  toJSON rc = object
    [ "rcVersion" .= rcVersion rc
    , "rcSteps" .= rcSteps rc
    , "rcPolicy" .= Map.map (\gp -> object ["policy" .= gp, "explore" .= gpExplore gp]) (rcPolicy rc)
    , "rcLearn" .= rcLearn rc ]

-- | A tape of another version (or of none) does not decode; the message
-- names the version found. A tape without @rcPolicy@ or @rcLearn@ (one
-- written before they were) has no policy: an empty map and 'Null'.
instance FromJSON res => FromJSON (Recording spec res) where
  parseJSON = withObject "Recording" $ \o -> do
    v <- o .: "rcVersion"
    if v /= tapeVersion
      then fail ("a tape of version " <> show v <> ", where this build reads version " <> show tapeVersion)
      else do
        steps <- o .: "rcSteps"
        entries <- o .:? "rcPolicy" .!= (Map.empty :: Map.Map Text Value)
        policy <- traverse goalEntry entries
        learn <- o .:? "rcLearn" .!= Null
        pure (Recording v steps policy learn)
    where
      goalEntry = withObject "a goal's policy" $ \e -> do
        gp <- e .: "policy"
        explore <- e .:? "explore" .!= False
        pure gp {gpExplore = explore}

-- | The flat search's tape, the file @--record@ writes.
type AttemptScript = Recording AttemptSpec AttemptResult

-- | The recorder's state.
data Rec res = Rec
  { rkCounts :: Map.Map (Text, Text) Int
  , rkKeys :: Map.Map Running AttemptKey
  -- ^ every spawned job's key
  , rkNext :: Int
  , rkSteps :: Map.Map Int (Step res)
  -- ^ the steps by position; a capped step's position is taken when it
  -- starts, so the calls made inside it come after it
  }

-- | The key of a job not spawned (by this orchestrator).
unknownKey :: AttemptKey
unknownKey = AttemptKey "" "" (-1)

-- | The key a price is asked under: the latest spawn with the spec's
-- goal and backend (index 0 when there was none).
priceKey :: (spec -> (Text, Text)) -> Map.Map (Text, Text) Int -> spec -> AttemptKey
priceKey keyFn counts sp =
  let pair@(g, b) = keyFn sp
   in AttemptKey g b (max 0 (Map.findWithDefault 0 pair counts - 1))

-- | The events a dropped step carries: a 'Logged' step's event, and a hit
-- 'Capped' step's own.
loggedIn :: Step res -> [Event]
loggedIn = \case
  Logged e -> [e]
  Capped _ _ True evs -> evs
  _ -> []

-- | A wrapper over an orchestrator (a real one, in use) that records its
-- answers as a tape, given the key function, and a reader of the tape so
-- far. Every field is the wrapped one's, and each call appends its step
-- once the wrapped call returns: the clock read ('Now'); a spawn's key,
-- counted as 'scriptEntry' counts ('Spawned'); an await's job and result
-- (an exception as its text) or its timing out, each with its seconds
-- and the wrapped clock read after it ('Delivered', 'AwaitTimedOut'); a
-- cancel ('Cancelled'); a capped step's name, seconds and whether its
-- cap hit ('Capped'; its place is taken when it starts, so the calls
-- made inside a step that ran follow it; a step whose cap hit drops the
-- calls made inside it, since replay skips its action, and carries the
-- events they logged, in order; a step whose action throws a synchronous
-- exception is written as one that ran, the calls made inside it kept
-- after it, and the exception rethrown; an asynchronous one leaves no
-- step); a
-- price and the answer the caller used, under the key of the latest spawn
-- with the spec's goal and backend ('Priced'; a price that throws a
-- synchronous exception is recorded as Nothing, as 'priceCancelled' uses
-- it, and the exception rethrown); and every event logged ('Logged').
recordOrchestrator
  :: forall spec res
   . (spec -> (Text, Text))
  -> Orchestrator spec res
  -> IO (Orchestrator spec res, IO (Recording spec res))
recordOrchestrator keyFn o = do
  ref <- newIORef Rec {rkCounts = Map.empty, rkKeys = Map.empty, rkNext = 0, rkSteps = Map.empty}
  let slot = atomicModifyIORef' ref (\s -> (s {rkNext = rkNext s + 1}, rkNext s))
      put n st = atomicModifyIORef' ref (\s -> (s {rkSteps = Map.insert n st (rkSteps s)}, ()))
      append st = slot >>= \n -> put n st
      keyOf r = Map.findWithDefault unknownKey r . rkKeys <$> readIORef ref
      now = do
        t <- oNow o
        append (Now t)
        pure t
      spawn sp = do
        r <- oSpawn o sp
        k <- atomicModifyIORef' ref $ \s ->
          let (k, counts) = spawnKey keyFn (rkCounts s) sp
           in (s {rkCounts = counts, rkKeys = Map.insert r k (rkKeys s)}, k)
        append (Spawned k)
        pure r
      awaitAny secs rs = do
        got <- oAwaitAny o secs rs
        t <- oNow o
        case got of
          Nothing -> append (AwaitTimedOut secs t)
          Just (r, res) -> do
            k <- keyOf r
            append (Delivered secs k t (either (Left . T.pack . displayException) Right res))
        pure got
      cancelOne r = do
        oCancel o r
        keyOf r >>= append . Cancelled
      -- the log and its step are one, uninterruptibly masked: a hit cap's
      -- timeout cannot land between them
      logOne e = uninterruptibleMask_ $ do
        oLog o e
        append (Logged e)
      capped :: Text -> Double -> IO a -> IO (Maybe a)
      capped name secs act = do
        n <- slot
        got <- try @SomeException (oCapped o name secs act) >>= \case
          Right x -> pure x
          Left e
            | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
            | otherwise -> do
                -- the step ran and threw: it keeps its place, before the
                -- calls made inside it, and replays by running again
                put n (Capped name secs False [])
                throwIO e
        -- a hit cap's action is skipped on replay, so the steps it took
        -- (the loop waits on it, so they are all after its slot) go, and
        -- the events among them go into the step, to be logged on replay
        atomicModifyIORef' ref $ \s ->
          let hit = isNothing got
              (kept, inside) = Map.partitionWithKey (\i _ -> i < n) (rkSteps s)
              steps
                | hit = Map.insert n (Capped name secs True (concatMap loggedIn (Map.elems inside))) kept
                | otherwise = Map.insert n (Capped name secs False []) (rkSteps s)
           in (s {rkSteps = steps, rkNext = if hit then n + 1 else rkNext s}, ())
        pure got
      priceOf sp = do
        d <- try @SomeException (oPrice o sp)
        k <- priceKey keyFn . rkCounts <$> readIORef ref <*> pure sp
        case d of
          Left e
            | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
            | otherwise -> append (Priced k Nothing) >> throwIO e
          Right x -> append (Priced k x) >> pure x
      orch = Orchestrator
        { oSpawn = spawn
        , oAwaitAny = awaitAny
        , oCancel = cancelOne
        , oNow = now
        , oLog = logOne
        , oCapped = capped
        , oPrice = priceOf
        }
  pure (orch, tapeOf . Map.elems . rkSteps <$> readIORef ref)

-- | A replay asked for something other than the tape's next step: the
-- step's position (from 0), the step expected and the call made.
data ReplayDiverged = ReplayDiverged { rdPosition :: Int, rdExpected :: Text, rdActual :: Text }
  deriving (Eq, Show)

instance Exception ReplayDiverged

-- | A step in one line, without its result.
describeStep :: Step res -> Text
describeStep = \case
  Now t -> "now " <> tshow t
  Spawned k -> "spawn " <> showKey k
  Delivered secs k t _ -> "await " <> tshow secs <> " delivering " <> showKey k <> " at " <> tshow t
  AwaitTimedOut secs t -> "await " <> tshow secs <> " timing out at " <> tshow t
  Cancelled k -> "cancel " <> showKey k
  Capped name secs hit _ -> "capped " <> name <> " " <> tshow secs <> (if hit then " (hit)" else " (ran)")
  Priced k _ -> "price " <> showKey k
  Logged e -> "log " <> tshow e

showKey :: AttemptKey -> Text
showKey (AttemptKey g b i) = g <> "/" <> b <> "/" <> tshow i

-- | Seconds equal within 1e-9 (the replay's are computed from the tape's
-- clock readings as the run's were).
near :: Double -> Double -> Bool
near a b = a == b || abs (a - b) <= 1e-9

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | The replayer's state.
data Replay res = Replay
  { rpTape :: [Step res]
  , rpPos :: Int
  , rpCounts :: Map.Map (Text, Text) Int
  , rpNext :: Int
  , rpByKey :: Map.Map AttemptKey Running
  , rpKeys :: Map.Map Running AttemptKey
  }

-- | An orchestrator that plays a tape, given the key function and the
-- run log, and the number of steps it has consumed. Each call takes the
-- tape's next step and answers from it: the clock ('Now'); a fresh job
-- for a spawn whose key (counted as 'scriptEntry' counts) is the step's
-- ('Spawned'); for an await, the job with the step's key, which must be
-- among those awaited, with its result (a Left as an 'ErrorCall' of its
-- text), or Nothing ('Delivered', 'AwaitTimedOut'), the await's seconds
-- the step's within 1e-9; a cancel of the job with the step's key
-- ('Cancelled'); a capped step with the step's name and seconds: for a
-- hit, the step's events given to the log and Nothing, without running
-- the action; for a step that ran, the action's result under a real
-- 'timeout' of the step's seconds ('Capped'); a price asked under the
-- step's key, its answer ('Priced'); an event logged that equals the
-- step's, given to the log ('Logged'). Any other call, a call past the
-- tape's end, or a step that ran and does not end within its seconds,
-- throws 'ReplayDiverged'.
replayOrchestrator
  :: forall spec res
   . (spec -> (Text, Text))
  -> (Event -> IO ())
  -> Recording spec res
  -> IO (Orchestrator spec res, IO Int)
replayOrchestrator keyFn logE rc = do
  ref <- newIORef Replay
    { rpTape = rcSteps rc, rpPos = 0, rpCounts = Map.empty, rpNext = 0, rpByKey = Map.empty, rpKeys = Map.empty }
  let -- the next step, consumed; the call's description and the step's
      -- answer, Nothing when the step is not the call's
      next :: Text -> (Step res -> Replay res -> Maybe (Replay res, IO a)) -> IO a
      next call answer = do
        got <- atomicModifyIORef' ref $ \s -> case rpTape s of
          [] -> (s, Left (ReplayDiverged (rpPos s) "the end of the tape" call))
          st : rest -> case answer st s {rpTape = rest, rpPos = rpPos s + 1} of
            Just (s', act) -> (s', Right act)
            Nothing -> (s, Left (ReplayDiverged (rpPos s) (describeStep st) call))
        either throwIO id got
      keyOf s r = Map.findWithDefault unknownKey r (rpKeys s)
      now = next "now" $ \st s -> case st of
        Now t -> Just (s, pure t)
        _ -> Nothing
      spawn sp =
        let call s = "spawn " <> showKey (fst (spawnKey keyFn (rpCounts s) sp))
         in readIORef ref >>= \s0 -> next (call s0) $ \st s -> case st of
              Spawned k
                | (k', counts) <- spawnKey keyFn (rpCounts s) sp, k' == k ->
                    let r = Running (rpNext s)
                     in Just (s {rpCounts = counts, rpNext = rpNext s + 1, rpByKey = Map.insert k r (rpByKey s), rpKeys = Map.insert r k (rpKeys s)}, pure r)
              _ -> Nothing
      awaitAny secs rs = readIORef ref >>= \s0 ->
        next ("await " <> tshow secs <> " on " <> T.intercalate ", " (map (showKey . keyOf s0) rs)) $ \st s -> case st of
          Delivered secs' k _ res
            | near secs secs', Just r <- Map.lookup k (rpByKey s), r `elem` rs ->
                Just (s, pure (Just (r, either (Left . toException . ErrorCall . T.unpack) Right res)))
          AwaitTimedOut secs' _ | near secs secs' -> Just (s, pure Nothing)
          _ -> Nothing
      cancelOne r = readIORef ref >>= \s0 ->
        next ("cancel " <> showKey (keyOf s0 r)) $ \st s -> case st of
          Cancelled k | k == keyOf s r -> Just (s, pure ())
          _ -> Nothing
      logOne e = next ("log " <> tshow e) $ \st s -> case st of
        Logged e' | e' == e -> Just (s, logE e)
        _ -> Nothing
      capped :: Text -> Double -> IO a -> IO (Maybe a)
      capped name secs act = do
        (hit, evs, at) <- next ("capped " <> name <> " " <> tshow secs) $ \st s -> case st of
          Capped name' secs' h evs | name' == name, near secs secs' -> Just (s, pure (h, evs, rpPos s - 1))
          _ -> Nothing
        if hit
          then do
            -- the events the step logged before its cap hit
            mapM_ logE evs
            pure Nothing
          else
            -- the step ran within its cap on the run, so it must on replay
            timeout (max 1 (ceiling (secs * 1000000))) act >>= \case
              Just a -> pure (Just a)
              Nothing -> throwIO (ReplayDiverged at (describeStep (Capped name secs False []))
                                    ("the step `" <> name <> "` did not end within its recorded cap"))
      priceOf sp = readIORef ref >>= \s0 ->
        let k0 = priceKey keyFn (rpCounts s0) sp
         in next ("price " <> showKey k0) $ \st s -> case st of
              Priced k d | k == priceKey keyFn (rpCounts s) sp -> Just (s, pure d)
              _ -> Nothing
      orch = Orchestrator
        { oSpawn = spawn
        , oAwaitAny = awaitAny
        , oCancel = cancelOne
        , oNow = now
        , oLog = logOne
        , oCapped = capped
        , oPrice = priceOf
        }
  pure (orch, rpPos <$> readIORef ref)
