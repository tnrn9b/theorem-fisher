-- | Live sessions (U2): runs the page starts, their ledger as a stream,
-- stop and send.
--
--   * @POST /api/sessions@ @{project, command}@: runs
--     @fisher launch --print <command>@ (the command one argument) in the project with
--     @FISHER_UI_LEDGER=1 FISHER_UI_TAG=<tag>@ (and the server's
--     GAUSS_HOME), stdin from /dev/null, in its own process group, output
--     to @$GAUSS_HOME/fisher/ui-runs/<tag>.log@; answers @{tag}@.
--   * @GET /api/tags/:tag@: @{tag, session, status, running, exit}@; the
--     session is null until the agent records the tag in @ui_runs@.
--     @POST /api/tags/:tag/stop@ stops a run that has no session yet.
--   * @GET /api/sessions/:id/live@: the session's root and the run of
--     this server that drives it, if any.
--   * @GET /api/sessions/:id/events@: server-sent events from @ui_events@,
--     polled every 250 ms: @id:@ is the event's number, @data:@ is
--     @{seq, kind, data}@. It starts after @Last-Event-ID@ (a reconnect),
--     else after @?after=@, else at the start, and ends with the @end@
--     event. A run of this server that exits without one (a crash, a stop)
--     gets one made up for it, numbered one past the ledger's last, with
--     the run's status (the ledger is read three more times first, 200 ms
--     apart). A stream no run of this server drives ends with a made-up
--     @end {status: unknown}@ after 'leIdle' (5 minutes) with no new
--     event. A stream asked for past its end answers 204, so the browser
--     does not reconnect; so does one for a session with no ledger events
--     that no run of this server drives and no other process runs (its
--     run lock is free: it ran without the ledger).
--   * @POST /api/sessions/:id/stop@: SIGTERM to the run's process tree
--     (its group, and each descendant and its group, as 'stopChild' lists
--     them), SIGKILL after 10 s ('leGrace'); answers 202 at once. A session
--     that runs in another process (its run lock is held, see below) is
--     409 "running in another process": this server stops only its own.
--   * @POST /api/sessions/:id/message@ @{text}@: one line appended to the
--     session's inbox, which the agent takes at its next tool boundary;
--     for a live run of this server, or a session running in another
--     process (then @elsewhere@ is its pid).
--
-- A session runs in another process when its run lock
-- (@$GAUSS_HOME/fisher/steps/<root>.lock@, 'Fisher.Agent.RunLock') is held
-- and no live run of this server drives it: whoever started that chat (a
-- terminal, another server), the server sends to it only through the
-- inbox and never resumes it. @live@ gives that process as @elsewhere@.
--
-- The ledger, step log and inbox are keyed on the chat's root session (it
-- stays the same across compression), so every route maps the id it is
-- given to that root first.
--
-- A run's status: @starting@ (no session yet), @running@, @stopping@;
-- once it exits, @stopped@ when it was stopped here, else the status of
-- its @end@ event (@completed@, @interrupted@, @failed@, @error@), else
-- @crashed@.
module Fisher.UI.Api.Live
  ( LiveEnv (..)
  , Run (..)
  , newLiveEnv
  , liveApi
  , stopRuns
  , runStatus
    -- * For the other live routes (the proof canvas)
  , spawnRun
  , spawnRunWith
  , rootOf
  , runFor
  , runAlive
  , readDb
  , elsewhere
  , bad
  , jsonBodyWith
  , textField
  , cachedSteps
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Network.HTTP.Types
import Network.Wai
import Data.Time.Clock (UTCTime)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, getFileSize, getModificationTime)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, replaceExtension, (</>))
import Text.Read (readMaybe)

import Fisher.Agent.Inbox (appendInbox, inboxPath)
import Fisher.Agent.RunLock (runLockHolder)
import Fisher.Agent.Steps (Step, readSteps)
import Fisher.UI.Children
import Fisher.UI.Db
import Fisher.UI.Response (Api, jsonResponse, pageHeaders)

-- | A run this server started.
data Run = Run
  { rnChild :: Child
  , rnExit :: MVar ExitCode
  -- ^ filled when the child exits
  , rnSession :: IORef (Maybe Text)
  -- ^ the session it became, once @ui_runs@ has it
  , rnStopping :: IORef Bool
  -- ^ a stop was asked for here
  , rnProject :: FilePath
  , rnCommand :: Text
  , rnLog :: FilePath
  , rnBase :: Int
  -- ^ the session's last ledger number when the run started (0 for a new
  -- session; a resume continues a ledger whose earlier end is not its own)
  }

data LiveEnv = LiveEnv
  { leUi :: UiEnv
  , leFisher :: FilePath
  -- ^ the fisher executable a run starts (the server's own)
  , leGrace :: Int
  -- ^ microseconds from SIGTERM to SIGKILL ('stopGrace', 10 s)
  , lePoll :: Int
  -- ^ microseconds between ledger reads (250 ms)
  , leKeepAlive :: Int
  -- ^ microseconds of quiet before a comment line keeps the stream open
  , leIdle :: Int
  -- ^ microseconds with no new event, and no live run of this server for
  -- the session, before a stream ends with a made-up end (5 minutes)
  , leRuns :: IORef (M.Map Text Run)
  , leClosing :: IORef Bool
  , leInbox :: MVar ()
  -- ^ one inbox append at a time
  , leLake :: FilePath
  -- ^ the @lake@ a Lean step's check runs (a name looked up on PATH)
  , leCheckSecs :: Int
  -- ^ seconds a step's check may take (10 minutes)
  , leCheck :: MVar ()
  -- ^ one step check at a time
  , leHand :: MVar ()
  -- ^ one hand back at a time
  , leCheckWaiting :: IORef Int
  -- ^ the step checks asked for and not done
  , leCheckQueue :: Int
  -- ^ how many may wait behind the running one before a check is refused (4)
  , leSteps :: IORef (M.Map FilePath ((Integer, UTCTime), [Step]))
  -- ^ each step log as last parsed, by its size and time
  }

newLiveEnv :: UiEnv -> FilePath -> IO LiveEnv
newLiveEnv ui fisher = do
  e <- LiveEnv ui fisher stopGrace 250000 10000000 (5 * 60 * 1000000) <$> newIORef M.empty <*> newIORef False <*> newMVar ()
  e "lake" 600 <$> newMVar () <*> newMVar () <*> newIORef 0 <*> pure 4 <*> newIORef M.empty

liveApi :: LiveEnv -> Api
liveApi env r = case (requestMethod r, pathInfo r) of
  (m, ["api", "sessions"]) | m == methodPost -> Just <$> start env r
  (m, ["api", "tags", tag]) | m == methodGet -> Just <$> guarded tag (tagView env tag)
  (m, ["api", "tags", tag, "stop"]) | m == methodPost -> Just <$> guarded tag (stopTag env tag)
  (m, ["api", "sessions", sid, "live"]) | m == methodGet -> Just <$> guarded sid (liveView env sid)
  (m, ["api", "sessions", sid, "events"]) | m == methodGet -> Just <$> guarded sid (events env r sid)
  (m, ["api", "sessions", sid, "stop"]) | m == methodPost -> Just <$> guarded sid (stopSession env sid)
  (m, ["api", "sessions", sid, "message"]) | m == methodPost -> Just <$> guarded sid (message env r sid)
  _ -> pure Nothing
  where
    guarded i k = if safeId i then k else pure (bad badRequest400 "bad id")

bad :: Status -> Text -> Response
bad s m = jsonResponse s (A.object ["error" A..= m])

-- | The body as JSON, at most 64 KB.
jsonBody :: Request -> IO (Maybe A.Object)
jsonBody = jsonBodyWith 65536

-- | The body as JSON, at most this many bytes.
jsonBodyWith :: Int -> Request -> IO (Maybe A.Object)
jsonBodyWith limit r = go 0 []
  where
    go n acc = do
      c <- getRequestBodyChunk r
      if B.null c
        then pure $ case A.decode (BL.fromChunks (reverse acc)) of
          Just (A.Object o) -> Just o
          _ -> Nothing
        else if n + B.length c > limit then pure Nothing else go (n + B.length c) (c : acc)

textField :: A.Key -> A.Object -> Maybe Text
textField k o = case KM.lookup k o of
  Just (A.String t) -> Just t
  _ -> Nothing

-- ------------------------------------------------------------------ start

start :: LiveEnv -> Request -> IO Response
start env r = do
  body <- jsonBody r
  case body of
    Nothing -> pure (bad badRequest400 "send {project, command} as JSON")
    Just o -> case (textField "project" o, T.strip <$> textField "command" o) of
      (Just p, Just cmd)
        | not (isAbsolute (T.unpack p)) -> pure (bad badRequest400 "the project is not an absolute path")
        | T.null cmd || T.head cmd /= '/' -> pure (bad badRequest400 "the command is a Fisher command such as /prove FILE")
        | T.any (`elem` ("\r\n\0" :: String)) cmd || T.length cmd > 4000 -> pure (bad badRequest400 "the command is one line")
        | otherwise -> do
            isDir <- doesDirectoryExist (T.unpack p)
            closing <- readIORef (leClosing env)
            if not isDir
              then pure (bad badRequest400 "no such project directory")
              else if closing then pure (bad serviceUnavailable503 "fisher ui is stopping") else launch env (T.unpack p) cmd
      _ -> pure (bad badRequest400 "send {project, command} as JSON")

launch :: LiveEnv -> FilePath -> Text -> IO Response
launch env project cmd =
  -- one argument: launch finds its own flags only as words of their
  -- own, so the command reaches the plan as typed (its --model or
  -- --prover included, which the plan takes from the text) and a
  -- path's spacing is kept
  spawnRun env project ["launch", "--print", T.unpack cmd] cmd 0 Nothing >>= \case
    Left e -> pure (bad internalServerError500 e)
    Right tag -> pure (jsonResponse ok200 (A.object ["tag" A..= tag]))

-- | Start @fisher ARGS@ in the project as a run of this server, with the
-- ledger on (@FISHER_UI_LEDGER=1@, a new @FISHER_UI_TAG@, the server's
-- GAUSS_HOME), output to @$GAUSS_HOME/fisher/ui-runs/<tag>.log@: its tag.
-- The label is what the page shows as its command; the base is the
-- session's last ledger number at the start, and the session, when known
-- already (a resume), is recorded at once.
spawnRun :: LiveEnv -> FilePath -> [String] -> Text -> Int -> Maybe Text -> IO (Either Text Text)
spawnRun env project0 args label base sess = spawnRunWith env project0 args [] label base sess

-- | 'spawnRun' with more variables for the child (the ledger's own win).
spawnRunWith :: LiveEnv -> FilePath -> [String] -> [(String, String)] -> Text -> Int -> Maybe Text -> IO (Either Text Text)
spawnRunWith env project0 args extra label base sess = do
  closing0 <- readIORef (leClosing env)
  if closing0
    then pure (Left "fisher ui is stopping")
    else do
      project <- canonicalizePath project0
      tag <- newTag
      let home = ueHome (leUi env)
          logp = home </> "fisher" </> "ui-runs" </> T.unpack (tag <> ".log")
          vars = [("FISHER_UI_LEDGER", "1"), ("FISHER_UI_TAG", T.unpack tag), ("GAUSS_HOME", home)]
          ours = map fst vars
      r <- try (spawnChild (Just logp) project (leFisher env : args) (vars <> [kv | kv@(k, _) <- extra, k `notElem` ours]))
      case r of
        Left (e :: SomeException) -> pure (Left (T.pack (show e)))
        Right (ch, exited) -> do
          run <- Run ch exited <$> newIORef sess <*> newIORef False <*> pure project <*> pure label <*> pure logp <*> pure base
          atomicModifyIORef' (leRuns env) (\m -> (M.insert tag run m, ()))
          -- a stop of the server that came before the run was recorded did not see it
          closing <- readIORef (leClosing env)
          when closing (stopRun env run)
          pure (Right tag)

-- ----------------------------------------------------------------- status

-- | Read state.db (read-only), or the default when it is not there or the
-- read fails (the next poll tries again).
readDb :: LiveEnv -> a -> (Connection -> IO a) -> IO a
readDb env dflt k = do
  let db = stateDb (leUi env)
  here <- doesFileExist db
  if not here
    then pure dflt
    else either (\(_ :: SomeException) -> dflt) id <$> try (withRo db k)

-- | The session the run became, once the agent has recorded its tag.
runSession :: LiveEnv -> Run -> IO (Maybe Text)
runSession env run =
  readIORef (rnSession run) >>= \case
    Just s -> pure (Just s)
    Nothing -> do
      found <- readDb env Nothing (`ledgerRun` chTag (rnChild run))
      forM_ found $ \(s, _) -> writeIORef (rnSession run) (Just s)
      pure (fst <$> found)

endStatusOf :: A.Value -> Maybe Text
endStatusOf = \case
  A.Object o -> textField "status" o
  _ -> Nothing

-- | The run's status, its session, and its exit status once it exited.
runStatus :: LiveEnv -> Run -> IO (Text, Maybe Text, Maybe ExitCode)
runStatus env run = do
  sess <- runSession env run
  ex <- tryReadMVar (rnExit run)
  stopping <- readIORef (rnStopping run)
  end <- maybe (pure Nothing) (\s -> snd <$> readDb env (0, Nothing) (`ledgerTail` s)) sess
  let endSt = end >>= \(n, v) -> if n > rnBase run then endStatusOf v else Nothing
      st = case ex of
        Nothing
          | stopping -> "stopping"
          | Just s <- endSt -> s
          | isJust sess -> "running"
          | otherwise -> "starting"
        Just _
          | stopping -> "stopped"
          | Just s <- endSt -> s
          | otherwise -> "crashed"
  pure (st, sess, ex)

exitJson :: Maybe ExitCode -> A.Value
exitJson = \case
  Nothing -> A.Null
  Just ExitSuccess -> A.Number 0
  Just (ExitFailure n) -> A.toJSON n

runJson :: LiveEnv -> Run -> IO A.Value
runJson env run = do
  (st, sess, ex) <- runStatus env run
  pure $
    A.object
      [ "tag" A..= chTag (rnChild run), "session" A..= sess, "status" A..= st, "running" A..= not (isJust ex)
      , "exit" A..= exitJson ex, "project" A..= rnProject run, "command" A..= rnCommand run
      , "started" A..= chStarted (rnChild run), "log" A..= rnLog run, "base" A..= rnBase run ]

tagView :: LiveEnv -> Text -> IO Response
tagView env tag = do
  mine <- M.lookup tag <$> readIORef (leRuns env)
  case mine of
    Just run -> jsonResponse ok200 <$> runJson env run
    Nothing -> do
      -- a run of an earlier start of the server: what the ledger says
      found <- readDb env Nothing (`ledgerRun` tag)
      case found of
        Nothing -> pure (bad notFound404 "no such run")
        Just (s, _) -> do
          (_, end) <- readDb env (0, Nothing) (`ledgerTail` s)
          pure . jsonResponse ok200 $
            A.object
              [ "tag" A..= tag, "session" A..= s, "status" A..= maybe "unknown" id (end >>= endStatusOf . snd)
              , "running" A..= False, "exit" A..= A.Null ]

rootOf :: LiveEnv -> Text -> IO Text
rootOf env sid = readDb env sid (`sessionRoot` sid)

-- | This server's latest run that drives the root session.
runFor :: LiveEnv -> Text -> IO (Maybe Run)
runFor env root = do
  runs <- M.elems <$> readIORef (leRuns env)
  hits <- fmap concat . forM runs $ \run -> do
    s <- runSession env run
    pure [run | s == Just root]
  pure (listToMaybe (sortOn (Down . chStarted . rnChild) hits))

-- | Whether the run is still going.
runAlive :: Run -> IO Bool
runAlive run = not . isJust <$> tryReadMVar (rnExit run)

-- | The inbox lines the agent has not taken yet (a message sent to a run
-- that ended before its next tool call waits for the session's next run).
inboxPending :: LiveEnv -> Text -> IO Int
inboxPending env root = do
  let p = inboxPath (ueHome (leUi env)) root
  r <- try @SomeException $ do
    raw <- B.readFile p
    off <- either (\(_ :: SomeException) -> 0) (\b -> fromMaybe 0 (readMaybe (BC.unpack (BC.strip b)))) <$> try (B.readFile (replaceExtension p "offset"))
    pure (length (filter (not . B.null) (BC.lines (B.drop off raw))))
  pure (either (const 0) id r)

liveView :: LiveEnv -> Text -> IO Response
liveView env sid = do
  root <- rootOf env sid
  run <- runFor env root
  pending <- inboxPending env root
  ours <- maybe (pure False) runAlive run
  other <- if ours then pure Nothing else elsewhere env root
  case run of
    Just x | ours || isNothing other -> do
      v <- runJson env x
      pure (jsonResponse ok200 (addPending pending (addRoot root (addElse Nothing v))))
    _ -> do
      (_, end) <- readDb env (0, Nothing) (`ledgerTail` root)
      pure . jsonResponse ok200 $
        A.object
          [ "root" A..= root, "tag" A..= A.Null, "session" A..= root
          , "status" A..= (if isJust other then Just "running" else end >>= endStatusOf . snd)
          , "running" A..= False, "inboxPending" A..= pending
          , "elsewhere" A..= other ]
  where
    addPending n = \case
      A.Object o -> A.Object (KM.insert "inboxPending" (A.toJSON n) o)
      v -> v
    addRoot root = \case
      A.Object o -> A.Object (KM.insert "root" (A.String root) o)
      v -> v
    addElse (e :: Maybe Int) = \case
      A.Object o -> A.Object (KM.insert "elsewhere" (A.toJSON e) o)
      v -> v

-- | A step log's steps, parsed again only when its size or time changed
-- (a session's log holds every file it wrote, whole, and the canvas asks
-- for it on each step it shows).
cachedSteps :: LiveEnv -> FilePath -> IO [Step]
cachedSteps env p = do
  st <- try @SomeException ((,) <$> getFileSize p <*> getModificationTime p)
  case st of
    Left _ -> pure []
    Right k -> do
      hit <- M.lookup p <$> readIORef (leSteps env)
      case hit of
        Just (k', ss) | k' == k -> pure ss
        _ -> do
          ss <- readSteps p
          atomicModifyIORef' (leSteps env) (\m -> (M.insert p (k, ss) m, ()))
          pure ss

-- | The process that holds the session's run lock, if any: the session runs
-- there (callers ask only when no live run of this server drives it).
elsewhere :: LiveEnv -> Text -> IO (Maybe Int)
elsewhere env root = fmap fromIntegral <$> runLockHolder (ueHome (leUi env)) root

-- ----------------------------------------------------------------- stream

-- | Where a stream starts: after Last-Event-ID, else ?after=, else 0.
startAfter :: Request -> Int
startAfter r =
  case lookup "Last-Event-ID" (requestHeaders r) >>= readMaybe . BC.unpack of
    Just n -> max 0 n
    Nothing -> case lookup "after" (queryString r) of
      Just (Just v) | Just n <- readMaybe (BC.unpack v) -> max 0 n
      _ -> 0

sseEvent :: Int -> Text -> A.Value -> BB.Builder
sseEvent n kind v =
  "id: " <> BB.intDec n <> "\ndata: "
    <> BB.lazyByteString (A.encode (A.object ["seq" A..= n, "kind" A..= kind, "data" A..= v])) <> "\n\n"

events :: LiveEnv -> Request -> Text -> IO Response
events env r sid = do
  root <- rootOf env sid
  let after = startAfter r
      readAfter n = readDb env [] (\c -> ledgerEvents c root n)
      exited x = isJust <$> tryReadMVar (rnExit x)
  first <- readAfter after
  (top, end) <- readDb env (0, Nothing) (`ledgerTail` root)
  run0 <- runFor env root
  live0 <- maybe (pure False) (fmap not . exited) run0
  -- past the end: after the ledger's last event when that is an end, or
  -- after its last event at all (a made-up end) with no live run here
  -- (a live resume's base: the end the ledger has is an earlier run's)
  let base0 = if live0 then maybe 0 rnBase run0 else 0
      endedThere = maybe False (\(n, _) -> n >= top && n > base0) end && top > 0 && after >= top
      -- a session with no ledger events that no run of this server
      -- drives will get none here (it ran, or runs, without the ledger)
      noLedger = top == 0 && isNothing run0
      pastEnd0 = null first && (endedThere || not live0 && after > top)
  -- unless another process holds its run lock: then it runs, and its
  -- events may come (it waits as for any session no run here drives)
  other <- if noLedger && null first && not pastEnd0 then elsewhere env root else pure Nothing
  let pastEnd = pastEnd0 || null first && noLedger && isNothing other
  if pastEnd
    then pure (responseLBS noContent204 (("Cache-Control", "no-store") : pageHeaders) "")
    else pure . responseStream ok200 sseHeaders $ \write flush -> do
      write "retry: 1000\n\n" >> flush
      let send evs = unless (null evs) (mapM_ (\(n, k, v) -> write (sseEvent n k v)) evs >> flush)
          lastOf n evs = if null evs then n else maximum [x | (x, _, _) <- evs]
          -- an end of an earlier run (a resumed session's) does not end it
          base = maybe 0 rnBase run0
          isEnd evs = any (\(n, k, _) -> k == "end" && n > base) evs
          madeUp n st ex = write (sseEvent (n + 1) "end" (A.object ["status" A..= st, "exit" A..= exitJson ex, "made" A..= True])) >> flush
          -- the run exited: what it wrote just before (read again three
          -- times, 200 ms apart, for an end written as it went), then its end
          finish run m k = do
            more <- readAfter m
            send more
            let m' = lastOf m more
            if isEnd more
              then pure ()
              else
                if k > 0
                  then threadDelay 200000 >> finish run m' (k - 1 :: Int)
                  else do
                    (st, _, ex) <- runStatus env run
                    madeUp m' st ex
          loop n quiet idle run evs = do
            send evs
            let n' = lastOf n evs
                idle' = if null evs then idle + lePoll env else 0
            if isEnd evs
              then pure ()
              else do
                gone <- maybe (pure False) exited run
                case run of
                  Just x | gone -> finish x n' 3
                  _ -> do
                    -- no run of this server drives it: a run may have
                    -- appeared since; if not, the stream ends once idle
                    run' <- if isNothing run && idle' >= leIdle env then runFor env root else pure run
                    if isNothing run' && idle' >= leIdle env
                      then madeUp n' ("unknown" :: Text) Nothing
                      else do
                        let quiet' = if null evs then quiet + lePoll env else 0
                        q <- if quiet' >= leKeepAlive env then write ": keep-alive\n\n" >> flush >> pure 0 else pure quiet'
                        threadDelay (lePoll env)
                        next <- readAfter n'
                        loop n' q idle' run' next
      loop after 0 0 run0 first
  where
    sseHeaders =
      [ (hContentType, "text/event-stream; charset=utf-8"), ("Cache-Control", "no-store")
      , ("X-Accel-Buffering", "no") ] <> pageHeaders

-- ------------------------------------------------------------ stop, send

stopRun :: LiveEnv -> Run -> IO ()
stopRun env run = do
  atomicModifyIORef' (rnStopping run) (const (True, ()))
  void (try (stopChildWith (leGrace env) (rnChild run)) :: IO (Either SomeException ()))

-- | Stop in the background; the status says stopping, then stopped.
stopAsync :: LiveEnv -> Run -> IO Response
stopAsync env run = do
  ex <- tryReadMVar (rnExit run)
  if isJust ex
    then pure (bad conflict409 "the run has ended")
    else do
      atomicModifyIORef' (rnStopping run) (const (True, ()))
      _ <- forkIO (stopRun env run)
      pure (jsonResponse accepted202 (A.object ["status" A..= ("stopping" :: Text), "tag" A..= chTag (rnChild run)]))

stopSession :: LiveEnv -> Text -> IO Response
stopSession env sid = do
  root <- rootOf env sid
  run <- runFor env root
  ours <- maybe (pure False) runAlive run
  other <- if ours then pure Nothing else elsewhere env root
  case (run, other) of
    (_, Just p) -> pure (bad conflict409 ("the session is running in another process (pid " <> T.pack (show p) <> "); stop it there"))
    (Nothing, _) -> pure (bad conflict409 "no run of this server drives this session")
    (Just x, _) -> stopAsync env x

stopTag :: LiveEnv -> Text -> IO Response
stopTag env tag =
  (M.lookup tag <$> readIORef (leRuns env)) >>= \case
    Nothing -> pure (bad notFound404 "no such run")
    Just run -> stopAsync env run

message :: LiveEnv -> Request -> Text -> IO Response
message env r sid = do
  body <- jsonBody r
  case T.strip <$> (body >>= textField "text") of
    Nothing -> pure (bad badRequest400 "send {text} as JSON")
    Just t
      | T.null t -> pure (bad badRequest400 "the message is empty")
      | T.length t > 20000 -> pure (bad badRequest400 "the message is longer than 20000 characters")
      | otherwise -> do
          root <- rootOf env sid
          run <- runFor env root
          alive <- maybe (pure False) runAlive run
          other <- if alive then pure Nothing else elsewhere env root
          if not alive && isNothing other
            then pure (bad conflict409 "no live run drives this session")
            else do
              withMVar (leInbox env) $ \_ -> appendInbox (inboxPath (ueHome (leUi env)) root) t
              pure (jsonResponse ok200 (A.object ["queued" A..= True, "session" A..= root, "elsewhere" A..= other]))

-- | Stop every run that is still going and wait for them; no run starts
-- after this. The server calls it as it stops.
stopRuns :: LiveEnv -> IO ()
stopRuns env = do
  atomicModifyIORef' (leClosing env) (const (True, ()))
  runs <- M.elems <$> readIORef (leRuns env)
  dones <- forM runs $ \run -> do
    done <- newEmptyMVar
    ex <- tryReadMVar (rnExit run)
    if isJust ex
      then putMVar done ()
      else void (forkIO (stopRun env run >> putMVar done ()))
    pure done
  mapM_ takeMVar dones
