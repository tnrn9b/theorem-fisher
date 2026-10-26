-- | What the Fisher UI sees of a running session, hooked onto the agent
-- loop from outside it: the step log (always on), and with a ledger the
-- events of 'Fisher.Agent.Ledger' (tool calls from the loop's callbacks,
-- cost from the transport, messages from the session store, the run's
-- end). The hooks only watch: whatever they do is out of the replay log,
-- and a failure in one never reaches the run.
--
-- With a run log ('Fisher.Runlog', always on when it opens) every event
-- is also a run-log event of the same kind and data, a @tool_end@ there
-- with the call's @ms@, @call_id@ and whole @result@ as well. The run ends
-- (an @outcome@ event and the run's outcome, cost and tokens) once, at
-- 'uiFinish' or 'uiClose', after its last turn. A failed run-log write is
-- one line on stderr, once; the hooks then write no more to the run log,
-- and the run goes on.
module Fisher.Agent.UiHooks
  ( UiHooks
  , newUiHooks
  , toolPathResolver
  , uiHasLedger
  , uiHasRunlog
  , uiEmit
  , uiCallbacks
  , uiTransport
  , uiSessionDB
  , uiEnd
  , uiFinish
  , uiClose
  , usageOfBody
  , endStatus
  , runlogGuard
  , runlogWarn
  ) where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath (isAbsolute, (</>))
import System.IO (hPutStrLn, stderr)

import Fisher.Agent.Ledger
import Fisher.Agent.Loop (AgentResult (..), Callbacks (..))
import Fisher.Agent.Session (SessionDB (..))
import Fisher.Agent.Steps
import Fisher.LLM.Http
import Fisher.Py.Value
import Fisher.Runlog (RunEnd (..), Runlog, closeRunlog, endRun, event)
import Fisher.Tools.FileOps (FileOps (..), expandPath)

data UiHooks = UiHooks
  { uhLedger :: Maybe Ledger
  , uhSteps :: FilePath
  , uhResolve :: Text -> Text -> IO FilePath
  -- ^ a task id and a path argument: the file the tool wrote
  , uhRunlog :: Maybe (Runlog, Text, Double)
  -- ^ the run log, the run's id there and its start (seconds since the
  -- epoch): an event's @t@ is the seconds since then
  , uhStarts :: IORef (M.Map Text [Double])
  -- ^ when each running call of a tool started, oldest first (the loop
  -- reports a call's start by its tool's name only)
  , uhUsage :: IORef Usage
  , uhWarned :: IORef Bool
  -- ^ whether a run-log write has failed (and been reported): no more
  -- are tried
  , uhPending :: IORef (Maybe Text)
  -- ^ how the last turn ended, while the run's end is not yet written
  }

-- | What the model calls reported, for the run's end: dollars, the calls
-- that reported usage and those that also reported a cost, input and
-- output tokens.
data Usage = Usage Double Int Int Integer Integer

-- | Hooks for a session: its ledger (none: no events), its step log, how
-- a file tool's path argument names a file (see 'toolPathResolver'), and
-- its run log with the run's id and start (none: no run-log writes). The
-- run must already be in the log; the hooks only add to it.
newUiHooks :: Maybe Ledger -> FilePath -> (Text -> Text -> IO FilePath) -> Maybe (Runlog, Text, Double) -> IO UiHooks
newUiHooks lg steps resolve rl =
  UiHooks lg steps resolve rl <$> newIORef M.empty <*> newIORef (Usage 0 0 0 0 0) <*> newIORef False <*> newIORef Nothing

-- | A path argument as the file tools take it: through the task's
-- 'FileOps' ('expandPath' for @~@, then relative to its working directory,
-- which is TERMINAL_CWD when that is set). When the task has no file
-- operations, relative to the fallback directory.
toolPathResolver :: (Text -> IO (Either Text FileOps)) -> IO FilePath -> Text -> Text -> IO FilePath
toolPathResolver opsFor fallback task p = do
  r <- opsFor task
  case r of
    Right fo -> do
      e <- T.unpack <$> expandPath fo p
      pure (if isAbsolute e then e else T.unpack (foCwd fo) </> e)
    Left _ -> do
      let s = T.unpack p
      if isAbsolute s then pure s else (</> s) <$> fallback

-- | Whether the hooks write a ledger.
uiHasLedger :: UiHooks -> Bool
uiHasLedger = isJust . uhLedger

-- | Whether the hooks write a run log.
uiHasRunlog :: UiHooks -> Bool
uiHasRunlog = isJust . uhRunlog

-- | An event to the ledger, if there is one, and to the run log, if there
-- is one; never fails.
uiEmit :: UiHooks -> Text -> [(Text, A.Value)] -> IO ()
uiEmit h kind kvs = uiEmitWith h kind kvs []

-- | 'uiEmit' with fields for the run log only.
uiEmitWith :: UiHooks -> Text -> [(Text, A.Value)] -> [(Text, A.Value)] -> IO ()
uiEmitWith h kind kvs extra = do
  forM_ (uhLedger h) $ \lg ->
    void (try @SomeException (emit lg (UiEvent kind (fields kvs))))
  withRun h $ \rl rid start -> do
    now <- clock
    event rl rid (max 0 (now - start)) kind (fields (kvs <> extra))
  where
    fields ps = A.object [(K.fromText k, v) | (k, v) <- ps]

-- | A run-log write, if there is a run log, through 'runlogGuard'.
withRun :: UiHooks -> (Runlog -> Text -> Double -> IO ()) -> IO ()
withRun h k = forM_ (uhRunlog h) $ \(rl, rid, start) -> runlogGuard (uhWarned h) (k rl rid start)

-- | Run a run-log action, unless one has already failed (the flag): a
-- locked or broken runs.db then costs the run no more waits. A failure
-- (not an asynchronous exception, which goes on) is reported with
-- 'runlogWarn' and goes no further.
runlogGuard :: IORef Bool -> IO () -> IO ()
runlogGuard warned act = do
  failed <- readIORef warned
  unless failed $
    try @SomeException act >>= \case
      Right () -> pure ()
      Left e
        | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
        | otherwise -> runlogWarn warned (T.pack (displayException e))

-- | @fisher: run log: <msg>@ on stderr, the first time only (the flag
-- says whether it was printed).
runlogWarn :: IORef Bool -> Text -> IO ()
runlogWarn warned msg = do
  first <- atomicModifyIORef' warned (\w -> (True, not w))
  when first $ void (try @SomeException (hPutStrLn stderr ("fisher: run log: " <> T.unpack msg)))

clock :: IO Double
clock = realToFrac <$> getPOSIXTime

jstr :: Text -> A.Value
jstr = A.String

-- | The loop's callbacks with the hooks added: thinking, each call's
-- start, and each call's end (with the step log for a file change).
uiCallbacks :: UiHooks -> Callbacks -> Callbacks
uiCallbacks h cb =
  cb
    { -- kept unset when unset: the loop then runs its own spinner
      cbThinking = (\f label -> uiEmit h "thinking" [("label", jstr label)] >> f label) <$> cbThinking cb
    , cbToolProgress = Just $ \callId name preview args -> do
        now <- clock
        modifyIORef' (uhStarts h) (M.insertWith (\new old -> old <> new) name [now])
        -- the call id goes to the run log only, so the ledger's events are as before
        uiEmitWith h "tool_start" [("name", jstr name), ("args", toAeson args)] [("call_id", jstr callId)]
        forM_ (cbToolProgress cb) (\f -> f callId name preview args)
    , cbToolDone = Just $ \task callId name args res -> do
        let ok = either (const False) resultOk res
            out = either id id res
        now <- clock
        started <- atomicModifyIORef' (uhStarts h) $ \m -> case M.lookup name m of
          Just (t : rest) -> (if null rest then M.delete name m else M.insert name rest m, Just t)
          _ -> (m, Nothing)
        let ms = maybe A.Null (\t -> A.toJSON (max 0 (round ((now - t) * 1000) :: Integer))) started
        uiEmitWith h "tool_end" [("name", jstr name), ("callId", jstr callId), ("preview", jstr (T.take 200 out)), ("ok", A.Bool ok)] [("ms", ms), ("call_id", jstr callId), ("result", jstr out)]
        case res of
          Right r | name `elem` ["write_file", "patch"] -> forM_ (changedPaths name args r) (logStep h task callId name)
          _ -> pure ()
        forM_ (cbToolDone cb) (\f -> f task callId name args res)
    }
  where
    resultOk r = case loads (TE.encodeUtf8 (fst (T.breakOn "\n\n[Hint:" r))) of
      Right d@(PDict _) -> maybe True (not . truthy) (get "error" d) && get "success" d /= Just (PBool False)
      _ -> not ("Error" `T.isPrefixOf` r)

-- | A changed file to the step log (and a @step@ event): read as the tool
-- left it.
logStep :: UiHooks -> Text -> Text -> Text -> Text -> IO ()
logStep h task callId tool p = void . try @SomeException $ do
  path <- uhResolve h task p
  bytes <- BS.readFile path
  at <- realToFrac <$> getPOSIXTime
  st <- appendNextStep (uhSteps h) $ \k -> Step k callId path (fileHash bytes) (TE.decodeUtf8With TEE.lenientDecode bytes) tool at
  uiEmit h "step" [("seq", A.toJSON (stSeq st)), ("path", jstr (T.pack path)), ("hash", jstr (stHash st))]

-- | The transport with a @cost@ event for each model call that reports
-- usage.
uiTransport :: UiHooks -> Transport -> Transport
uiTransport h tr =
  Transport
    { tRequest = \hr to -> do
        r <- tRequest tr hr to
        case r of
          Right resp -> report (rBody resp)
          Left _ -> pure ()
        pure r
    , tStream = \hr to k -> do
        acc <- newIORef []
        r <- tStream tr hr to $ \status hs reader ->
          k status hs (reader >>= \c -> modifyIORef acc (c :) >> pure c)
        BS.concat . reverse <$> readIORef acc >>= report
        pure r
    }
  where
    report body = forM_ (usageOfBody body) $ \(dollars, i, o) -> do
      modifyIORef' (uhUsage h) $ \(Usage d calls priced ti to) ->
        Usage (d + fromMaybe 0 dollars) (calls + 1) (priced + maybe 0 (const 1) dollars) (ti + i) (to + o)
      uiEmit h "cost" [("dollars", maybe A.Null A.toJSON dollars), ("input", A.toJSON i), ("output", A.toJSON o)]

-- | The usage a response body reports (a JSON reply, or a stream's
-- @data:@ lines): dollars (OpenRouter's @usage.cost@), input and output
-- tokens.
usageOfBody :: BS.ByteString -> Maybe (Maybe Double, Integer, Integer)
usageOfBody body =
  let docs = case loads body of
        Right v -> [v]
        Left _ -> [v | l <- BC.lines body, Just j <- [BS.stripPrefix "data:" l], Right v <- [loads (BC.strip j)]]
      usages = [u | d <- docs, Just u <- [usageIn d], truthy u]
      num k u = case get k u of
        Just (PInt n) -> Just n
        Just (PFloat x) -> Just (truncate x)
        _ -> Nothing
      cost u = case get "cost" u of
        Just (PFloat x) -> Just x
        Just (PInt n) -> Just (fromIntegral n)
        _ -> Nothing
      biggest ks = maximum (0 : mapMaybe (\u -> foldr (\k a -> maybe a Just (num k u)) Nothing ks) usages)
   in if null usages
        then Nothing
        else Just (lastJust (map cost usages), biggest ["prompt_tokens", "input_tokens"], biggest ["completion_tokens", "output_tokens"])
  where
    usageIn d = case get "usage" d of
      Just u -> Just u
      Nothing -> (get "message" d >>= get "usage") `orElse` (get "response" d >>= get "usage")
    orElse a b = maybe b Just a
    lastJust xs = case [x | Just x <- reverse xs] of
      (x : _) -> Just x
      [] -> Nothing

-- | The session store with a @message@ event for each message it keeps.
uiSessionDB :: UiHooks -> SessionDB -> SessionDB
uiSessionDB h db =
  db
    { dbAppendMessage = \sid m -> do
        dbAppendMessage db sid m
        uiEmit h "message" [("role", jstr (maybe "unknown" pyStrOf (get "role" m))), ("text", jstr (contentText m))]
    }
  where
    contentText m = case get "content" m of
      Just (PStr t) -> t
      Just (PList ps) -> T.concat [t | p <- ps, Just (PStr t) <- [get "text" p]]
      _ -> ""

-- | A turn's end: an @end@ event. The status is kept for the run's end
-- ('uiFinish'), which a chat of several turns writes once, after its last.
uiEnd :: UiHooks -> Text -> IO ()
uiEnd h status = do
  uiEmit h "end" [("status", jstr status)]
  writeIORef (uhPending h) (Just status)

-- | The run's end, if a turn has ended since the last: in the run log an
-- @outcome@ event and the run's end with the last turn's status as its
-- outcome, the dollars (when every call with usage reported a cost) and
-- the tokens. A headless run calls it when its one turn ends; 'uiClose'
-- calls it too.
uiFinish :: UiHooks -> IO ()
uiFinish h = atomicModifyIORef' (uhPending h) (\p -> (Nothing, p)) >>= mapM_ (\status ->
  withRun h $ \rl rid start -> do
    now <- clock
    event rl rid (max 0 (now - start)) "outcome" (A.object ["outcome" A..= status, "reason" A..= A.Null])
    Usage d calls priced ti to <- readIORef (uhUsage h)
    endRun rl rid
      RunEnd
        { reEndedAt = now, reExit = Nothing, reOutcome = status
        , reCostUsd = if calls > 0 && priced == calls then Just d else Nothing
        , reInputTokens = if calls > 0 then Just (fromIntegral ti) else Nothing
        , reOutputTokens = if calls > 0 then Just (fromIntegral to) else Nothing
        , reTurns = Nothing })

-- | How a turn ended: completed, interrupted, failed or error.
endStatus :: Maybe AgentResult -> Text
endStatus = \case
  Just (RunDone d)
    | get "completed" d == Just (PBool True) -> "completed"
    | get "interrupted" d == Just (PBool True) -> "interrupted"
    | otherwise -> "failed"
  _ -> "error"

-- | End the run ('uiFinish'), and close the ledger and the run log, if
-- there are.
uiClose :: UiHooks -> IO ()
uiClose h = do
  uiFinish h
  forM_ (uhLedger h) (void . try @SomeException . closeLedger)
  forM_ (uhRunlog h) $ \(rl, _, _) -> void (try @SomeException (closeRunlog rl))
