-- | What the read routes read: state.db (opened read-only, retried while
-- a writer holds it busy) and the agent's replay logs (one pass per log,
-- cached by the file's size and modification time, so a 60 MB log is
-- read once however often the page asks).
module Fisher.UI.Db
  ( -- * The server's places and caches
    UiEnv (..)
  , newUiEnv
  , Fingerprint
  , GateStatus (..)
  , GateRun (..)
  , stateDb
  , replayLogPath
  , ctxDirOf
  , safeId
    -- * state.db
  , withRo
  , busyWindow
  , SessionRow (..)
  , allSessions
  , findSession
  , MessageRow (..)
  , sessionMessages
  , ledgerEvents
  , ledgerRun
  , ledgerTail
  , sessionRoot
  , sessionLeaf
  , sessionModel
    -- * Replay logs
  , Cost (..)
  , CtxEvent (..)
  , LogSummary (..)
  , emptySummary
  , summarize
  , LogCache
  , newLogCache
  , logSummary
  , sessionCostWith
  , sessionCost
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.DeepSeq (force)
import Control.Exception (SomeAsyncException, SomeException, bracket, evaluate, fromException, mask, throwIO, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (isAlphaNum, isSpace)
import Data.IORef
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Database.SQLite.Simple hiding (field, toRow)
import System.Directory (doesFileExist, getFileSize, getModificationTime)
import System.FilePath ((</>))
import System.Exit (ExitCode)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (ProcessHandle)

-- | Where the server reads from, and its caches.
data UiEnv = UiEnv
  { ueHome :: FilePath
  -- ^ GAUSS_HOME: state.db, fisher/replays, fisher/ctx
  , ueBench :: FilePath
  -- ^ the bench root (~/.cache/fisher/bench)
  , ueRepo :: FilePath
  -- ^ the repository the server was started from (tools/bench/<b>/report.py)
  , ueLogs :: LogCache
  , ueProjects :: IORef (M.Map FilePath (Maybe (FilePath, Text)))
  -- ^ a directory's project root and prover, as found
  , ueGates :: IORef (M.Map Text GateRun)
  -- ^ each bench's gate: at most one report.py running per bench
  , ueClosing :: IORef Bool
  -- ^ set when the server stops: no gate child starts after it
  , ueGateTimeout :: Int
  -- ^ microseconds a gate child may run before it is killed (45 minutes:
  -- bench h's report.py takes about 16 minutes on its own on the Mac)
  , ueGateQuiet :: Double
  -- ^ seconds after the last request for a bench's gate past which a
  -- stale gate run is not followed by another (5 minutes)
  }

-- | The size and time of every file a bench's report.py reads.
type Fingerprint = [(FilePath, Integer, UTCTime)]

-- | A gate as the page sees it: being computed, failed (a nonzero exit,
-- no output, or killed at the timeout), not run while the bench is still
-- running (a run directory with events.jsonl and no report.json), or its
-- line.
data GateStatus = GatePending | GateFailed | GateBusy | GateOk Text
  deriving (Eq, Show)

-- | A bench's gate: the inputs its status (or its running child) is for,
-- whether a child runs, whether the inputs changed while it ran (it then
-- runs again once it ends, if it was asked for lately), the child with
-- the MVar its waiter fills, and when it was last asked for.
data GateRun = GateRun
  { gFp :: Fingerprint
  , gStatus :: GateStatus
  , gRunning :: Bool
  , gStale :: Bool
  , gChild :: Maybe (ProcessHandle, MVar (Either SomeException ExitCode))
  , gAsked :: Double
  -- ^ when a request last asked for this gate (POSIX seconds)
  }

newUiEnv :: FilePath -> FilePath -> FilePath -> IO UiEnv
newUiEnv home bench repo =
  UiEnv home bench repo <$> newLogCache BL.readFile <*> newIORef M.empty <*> newIORef M.empty <*> newIORef False
    <*> pure (45 * 60 * 1000000) <*> pure 300

stateDb :: UiEnv -> FilePath
stateDb e = ueHome e </> "state.db"

replayLogPath :: FilePath -> Text -> FilePath
replayLogPath home sid = home </> "fisher" </> "replays" </> T.unpack (sid <> ".jsonl")

ctxDirOf :: FilePath -> Text -> FilePath
ctxDirOf home sid = home </> "fisher" </> "ctx" </> T.unpack sid

-- | A session id that is safe as a file name.
safeId :: Text -> Bool
safeId t = not (T.null t) && T.all ok t && not (".." `T.isInfixOf` t) && T.head t /= '.'
  where
    ok c = isAlphaNum c || c `elem` ("_-." :: String)

-- ------------------------------------------------------------------ state.db

-- | How long a read keeps retrying while the database is busy.
busyWindow :: Double
busyWindow = 2

-- | Open @path@ read-only (@query_only@) and run the action, retrying the
-- whole read on SQLITE_BUSY or SQLITE_LOCKED for up to two seconds. Under
-- WAL a reader never blocks the agent's writes.
--
-- A WAL database opened @mode=ro@ still gets its @-wal@ and @-shm@ files
-- created, so @mode=ro@ is used only while the @-wal@ file exists (an agent
-- has the database open); otherwise it is opened @immutable=1@, which
-- writes nothing beside it.
withRo :: FilePath -> (Connection -> IO a) -> IO a
withRo path k = do
  deadline <- addUTCTime (realToFrac busyWindow) <$> getCurrentTime
  let go = do
        live <- doesFileExist (path <> "-wal")
        r <- try (bracket (open (roUri live path)) close (\c -> setup c >> k c))
        case r of
          Right a -> pure a
          Left e
            | sqlError e `elem` [ErrorBusy, ErrorLocked] -> do
                now <- getCurrentTime
                if now >= deadline then throwIO e else threadDelay 50000 >> go
            | otherwise -> throwIO e
  go
  where
    setup c = do
      execute_ c "PRAGMA busy_timeout = 250"
      execute_ c "PRAGMA query_only = ON"

-- | @file:<path>?mode=ro@ (with @&immutable=1@ unless the database is
-- live), with the characters a URI path reserves escaped.
roUri :: Bool -> FilePath -> String
roUri live p = "file:" <> concatMap esc p <> (if live then "?mode=ro" else "?mode=ro&immutable=1")
  where
    esc = \case
      '%' -> "%25"
      '?' -> "%3f"
      '#' -> "%23"
      ' ' -> "%20"
      c -> [c]

data SessionRow = SessionRow
  { srId :: Text
  , srTitle :: Maybe Text
  , srModel :: Maybe Text
  , srStarted :: Double
  , srEnded :: Maybe Double
  , srEndReason :: Maybe Text
  , srParent :: Maybe Text
  , srTurns :: Int
  -- ^ the assistant messages
  }
  deriving (Eq, Show)

sessionSelect :: Query
sessionSelect =
  "SELECT s.id, s.title, s.model, s.started_at, s.ended_at, s.end_reason, s.parent_session_id,\
  \ (SELECT COUNT(*) FROM messages m WHERE m.session_id = s.id AND m.role = 'assistant') FROM sessions s"

toRow :: (Text, Maybe Text, Maybe Text, Double, Maybe Double, Maybe Text, Maybe Text, Int) -> SessionRow
toRow (a, b, c, d, e, f, g, h) = SessionRow a b c d e f g h

-- | Every session, newest first.
allSessions :: Connection -> IO [SessionRow]
allSessions c = map toRow <$> query_ c (sessionSelect <> " ORDER BY s.started_at DESC, s.id")

findSession :: Connection -> Text -> IO (Maybe SessionRow)
findSession c sid = \case { (r : _) -> Just (toRow r); [] -> Nothing } <$> query c (sessionSelect <> " WHERE s.id = ?") (Only sid)

data MessageRow = MessageRow
  { mrRole :: Text
  , mrContent :: Maybe Text
  , mrToolCalls :: Maybe Text
  , mrToolName :: Maybe Text
  , mrToolCallId :: Maybe Text
  , mrAt :: Double
  }
  deriving (Eq, Show)

-- | A session's messages in the order the agent wrote them.
sessionMessages :: Connection -> Text -> IO [MessageRow]
sessionMessages c sid =
  map (\(a, b, d, e, f, g) -> MessageRow a b d e f g)
    <$> query c "SELECT role, content, tool_calls, tool_name, tool_call_id, timestamp FROM messages\
                \ WHERE session_id = ? ORDER BY timestamp, id" (Only sid)

-- | The agent's ledger (fisher-agent's Fisher.Agent.Ledger writes it
-- with FISHER_UI_LEDGER=1): a session's events after a number, in order:
-- number, kind, data. None when the agent never wrote a ledger here.
ledgerEvents :: Connection -> Text -> Int -> IO [(Int, Text, A.Value)]
ledgerEvents c sid after = do
  has <- hasTable c "ui_events"
  if not has
    then pure []
    else do
      rows <- query c "SELECT seq, kind, data FROM ui_events WHERE session_id = ? AND seq > ? ORDER BY seq" (sid, after) :: IO [(Int, Text, Text)]
      pure [(n, k, v) | (n, k, d) <- rows, Just v <- [A.decodeStrict (TE.encodeUtf8 d)]]

-- | The session and process the UI's run with a tag (FISHER_UI_TAG)
-- became.
ledgerRun :: Connection -> Text -> IO (Maybe (Text, Int))
ledgerRun c tag = do
  has <- hasTable c "ui_runs"
  if not has
    then pure Nothing
    else (\case (r : _) -> Just r; [] -> Nothing) <$> (query c "SELECT session_id, pid FROM ui_runs WHERE tag = ?" (Only tag) :: IO [(Text, Int)])

-- | A session's last number in the ledger (0 when it has none) and its
-- latest @end@ event (number, data).
ledgerTail :: Connection -> Text -> IO (Int, Maybe (Int, A.Value))
ledgerTail c sid = do
  has <- hasTable c "ui_events"
  if not has
    then pure (0, Nothing)
    else do
      top <- query c "SELECT COALESCE(MAX(seq), 0) FROM ui_events WHERE session_id = ?" (Only sid) :: IO [Only Int]
      end <- query c "SELECT seq, data FROM ui_events WHERE session_id = ? AND kind = 'end' ORDER BY seq DESC LIMIT 1" (Only sid) :: IO [(Int, Text)]
      pure
        ( case top of { (Only n : _) -> n; [] -> 0 }
        , case end of { ((n, d) : _) -> Just (n, maybe A.Null id (A.decodeStrict (TE.encodeUtf8 d))); [] -> Nothing } )

-- | The root of a session's compression chain (its @parent_session_id@s
-- followed up): the agent keys the ledger, the step log and the inbox on
-- the chat's root session, which stays the same across compression. A
-- session state.db does not know is its own root.
sessionRoot :: Connection -> Text -> IO Text
sessionRoot c sid0 = do
  has <- hasTable c "sessions"
  if not has then pure sid0 else go sid0 (1000 :: Int)
  where
    go s n = do
      rows <- query c "SELECT parent_session_id FROM sessions WHERE id = ?" (Only s) :: IO [Only (Maybe Text)]
      case rows of
        (Only (Just p) : _) | n > 0, not (T.null p), p /= s -> go p (n - 1)
        _ -> pure s

-- | The latest session of a chain from this one: its latest compression
-- child, followed down (the one a resume continues). A session with no
-- child is its own leaf.
sessionLeaf :: Connection -> Text -> IO Text
sessionLeaf c sid0 = do
  has <- hasTable c "sessions"
  if not has then pure sid0 else go sid0 (1000 :: Int)
  where
    go s n = do
      rows <- query c "SELECT id FROM sessions WHERE parent_session_id = ? AND id <> ? ORDER BY rowid DESC LIMIT 1" (s, s) :: IO [Only Text]
      case rows of
        (Only k : _) | n > 0 -> go k (n - 1)
        _ -> pure s

-- | The model a session ran with, when state.db records one.
sessionModel :: Connection -> Text -> IO (Maybe Text)
sessionModel c sid = do
  has <- hasTable c "sessions"
  if not has
    then pure Nothing
    else do
      cols <- query_ c "SELECT name FROM pragma_table_info('sessions')" :: IO [Only Text]
      if Only "model" `notElem` cols
        then pure Nothing
        else do
          rows <- query c "SELECT model FROM sessions WHERE id = ?" (Only sid) :: IO [Only (Maybe Text)]
          pure $ case rows of
            (Only (Just m) : _) | not (T.null m) -> Just m
            _ -> Nothing

hasTable :: Connection -> Text -> IO Bool
hasTable c name = not . null <$> (query c "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?" (Only name) :: IO [Only Int])

-- ---------------------------------------------------------------- replay logs

-- | Dollars and tokens over a log's priced responses.
data Cost = Cost
  { costDollars :: !Double
  , costInput :: !Integer
  , costOutput :: !Integer
  , costCacheRead :: !Integer
  }
  deriving (Eq, Show)

instance Semigroup Cost where
  Cost a b c d <> Cost a' b' c' d' = Cost (a + a') (b + b') (c + c') (d + d')

instance Monoid Cost where
  mempty = Cost 0 0 0 0

instance A.ToJSON Cost where
  toJSON (Cost d i o r) = A.object ["dollars" A..= d, "input" A..= i, "output" A..= o, "cacheRead" A..= r]

-- | A context-strategy event of the log (@clm_*@, @rlm_*@).
data CtxEvent = CtxEvent { ceKind :: !Text, ceTurn :: !Int, ceDetail :: !A.Value }
  deriving (Eq, Show)

instance A.ToJSON CtxEvent where
  toJSON (CtxEvent k t d) = A.object ["kind" A..= k, "turn" A..= t, "detail" A..= d]

-- | One pass over a replay log.
data LogSummary = LogSummary
  { lsCost :: !Cost
  , lsStrategy :: !(Maybe Text)
  -- ^ "clm" or "rlm" when the log has that strategy's header
  , lsResult :: !(Maybe Text)
  -- ^ "completed", "incomplete" or "error", from the last outcome
  , lsPaths :: [FilePath]
  -- ^ the first absolute paths the tools touched (for the project)
  , lsEvents :: [CtxEvent]
  , lsCalls :: !Int
  -- ^ model calls (@http@ events)
  }
  deriving (Eq, Show)

emptySummary :: LogSummary
emptySummary = LogSummary mempty Nothing Nothing [] [] 0

maxPaths :: Int
maxPaths = 16

-- | Fold the log line by line. Only the kinds read here are decoded.
summarize :: BL.ByteString -> LogSummary
summarize = finish . foldl' step emptySummary . BLC.lines
  where
    finish s = s {lsPaths = reverse (lsPaths s), lsEvents = reverse (lsEvents s)}
    step s line = case kindOf line of
      Nothing -> s
      Just k
        | k == "http" -> withObj line s $ \o -> s {lsCost = lsCost s <> fromMaybe mempty (httpCost o), lsCalls = lsCalls s + 1}
        | k == "tool" -> if length (lsPaths s) >= maxPaths then s else withObj line s $ \o ->
            let ps = force (reverse (take (maxPaths - length (lsPaths s)) (toolPaths o)))
             in ps `seq` s {lsPaths = ps <> lsPaths s}
        | k == "clm" || k == "rlm" -> s {lsStrategy = Just k}
        | "clm_" `T.isPrefixOf` k || "rlm_" `T.isPrefixOf` k -> withObj line s $ \o ->
            -- strict fields: the turn is taken now, not a thunk holding s
            let ev = CtxEvent k (turnOf s o) (force (A.Object (KM.map clip (KM.delete "kind" o))))
             in ev `seq` s {lsEvents = ev : lsEvents s}
        | k == "result" -> withObj line s $ \o -> s {lsResult = Just (resultOf o)}
        | k == "raised" -> s {lsResult = Just "error"}
        | otherwise -> s
    withObj line s f = case A.decode line of
      Just (A.Object o) -> f o
      _ -> s
    turnOf s o = case KM.lookup "turn" o of
      Just (A.Number n) | Just t <- toBoundedInteger n -> t
      _ -> lsCalls s
    resultOf o = case KM.lookup "result" o >>= field "completed" of
      Just (A.Bool False) -> "incomplete"
      _ -> "completed"

-- | A long text in an event's detail is cut to 400 characters (the full
-- text is in the ctx files or the log).
clip :: A.Value -> A.Value
clip = \case
  A.String t | T.length t > 400 -> A.String (T.take 400 t <> "...")
  v -> v

-- | The kind of a log line, from its prefix @{"kind": "K"@.
kindOf :: BL.ByteString -> Maybe Text
kindOf line = do
  rest <- BL.stripPrefix "{\"kind\": \"" (BL.take 64 line)
  let k = BLC.takeWhile (/= '"') rest
  pure (TE.decodeUtf8 (BL.toStrict k))

field :: Text -> A.Value -> Maybe A.Value
field k = \case
  A.Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

-- | The usage of an @http@ event's response body: one JSON document, or
-- server-sent events (each @data:@ line; the largest value of each field).
httpCost :: A.Object -> Maybe Cost
httpCost o = do
  A.String body <- KM.lookup "response" o >>= field "body"
  -- OpenRouter sends blank keep-alive lines before the document
  let b = BC.dropWhile isSpace (TE.encodeUtf8 body)
  case BC.uncons b of
    Just ('{', _) -> A.decodeStrict b >>= usageOf
    _ ->
      let chunks = mapMaybe (\l -> BC.stripPrefix "data:" l >>= A.decodeStrict . BC.dropWhile (== ' ')) (BC.lines b)
       in case mapMaybe usageOf chunks of
            [] -> Nothing
            us -> Just (foldr1 maxCost us)
  where
    maxCost (Cost a b c d) (Cost a' b' c' d') = Cost (max a a') (max b b') (max c c') (max d d')

-- | OpenAI-shape usage (with OpenRouter's @cost@) or Anthropic-shape.
usageOf :: A.Value -> Maybe Cost
usageOf v = do
  u@(A.Object _) <- field "usage" v
  let num k = case field k u of
        Just (A.Number n) -> Just n
        _ -> Nothing
      int ks = fromMaybe 0 (firstJust [round <$> num k | k <- ks])
      cached = case field "prompt_tokens_details" u >>= field "cached_tokens" of
        Just (A.Number n) -> round n
        _ -> int ["cache_read_input_tokens"]
  pure (Cost (maybe 0 toRealFloat (num "cost")) (int ["prompt_tokens", "input_tokens"]) (int ["completion_tokens", "output_tokens"]) cached)
  where
    firstJust xs = case [x | Just x <- xs] of
      (x : _) -> Just x
      [] -> Nothing

-- | Absolute paths in a tool call's arguments: path-like fields, and the
-- directory of a terminal command's @cd@.
toolPaths :: A.Object -> [FilePath]
toolPaths o = case KM.lookup "args" o of
  Just (A.Object a) ->
    [T.unpack p | k <- ["path", "file", "file_path", "workdir", "cwd"], Just (A.String p) <- [KM.lookup k a], "/" `T.isPrefixOf` p]
      <> case KM.lookup "command" a of
        Just (A.String cmd) -> cds (T.words cmd)
        _ -> []
  _ -> []
  where
    cds = \case
      ("cd" : d : rest) | "/" `T.isPrefixOf` d -> T.unpack (T.dropWhileEnd (`elem` (";&|" :: String)) d) : cds rest
      (_ : rest) -> cds rest
      [] -> []

-- | Summaries by path, each with the size and time it was read at. The
-- MVar is filled when the one read of that version ends, so concurrent
-- requests for a log wait for one read instead of each reading it.
data LogCache = LogCache
  { lcRead :: FilePath -> IO BL.ByteString
  , lcMap :: IORef (M.Map FilePath ((Integer, UTCTime), MVar (Either SomeException LogSummary)))
  }

-- | A cache that reads logs with the given reader (tests count its calls).
newLogCache :: (FilePath -> IO BL.ByteString) -> IO LogCache
newLogCache rd = LogCache rd <$> newIORef M.empty

-- | The log's summary; read again only when its size or mtime changed,
-- and by one request at a time (the others wait for its result). A
-- missing log is the empty summary.
logSummary :: LogCache -> FilePath -> IO LogSummary
logSummary lc path = do
  here <- doesFileExist path
  if not here
    then pure emptySummary
    else do
      key <- (,) <$> getFileSize path <*> getModificationTime path
      fresh <- newEmptyMVar
      -- masked from the claim to the fill, so a claimed read always fills
      -- its MVar (a cancelled one with the exception)
      mask $ \restore -> do
        (mine, mv) <- atomicModifyIORef' (lcMap lc) $ \m -> case M.lookup path m of
          Just (k, v) | k == key -> (m, (False, v))
          _ -> (M.insert path (key, fresh) m, (True, fresh))
        if mine then readLog restore mv else restore (readMVar mv) >>= waited
  where
    readLog restore mv = do
      r <- try . restore $ do
        s <- summarize <$> lcRead lc path
        _ <- evaluate (length (lsEvents s) + length (lsPaths s))
        pure s
      putMVar mv r
      case r of
        Right s -> pure s
        Left e -> do
          -- forget the failed read so the next request reads again
          atomicModifyIORef' (lcMap lc) (\m -> (M.update (\(k, v) -> if v == mv then Nothing else Just (k, v)) path m, ()))
          throwIO e
    waited = \case
      Right s -> pure s
      -- the reading request was cancelled (its client went away): read it here
      Left e | Just (_ :: SomeAsyncException) <- fromException e -> logSummary lc path
      Left e -> throwIO e

sessionCostWith :: LogCache -> FilePath -> IO Cost
sessionCostWith lc path = lsCost <$> logSummary lc path

globalCache :: LogCache
globalCache = unsafePerformIO (newLogCache BL.readFile)
{-# NOINLINE globalCache #-}

-- | The cost of a replay log, cached by (path, size, mtime).
sessionCost :: FilePath -> IO Cost
sessionCost = sessionCostWith globalCache
