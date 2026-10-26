-- | The run log: one SQLite database, @$FISHER_HOME/fisher/runs.db@, that
-- records every run of every agent (search attempts, launches, chats, web
-- app children, cron jobs, imports) with its goals and its events, in one
-- schema whatever the backend.
--
-- Every string an event carries under a secret-named key (@api_key@,
-- @Authorization@, @GLM_API_KEY@, ...) is masked, every string under
-- @args@, @result@, @text@, @error@, @preview@ or @lesson@ (and every string of data
-- that is not an object) is capped and then passes 'redactSensitiveText'
-- before the write, an @args@, @result@ or @text@ value is capped at 65536
-- characters as a whole, and the value of a secret-looking flag in a run's
-- argv is masked, so no secret and no unbounded blob reaches the file
-- ('scrubEventData').
--
-- The writers here may throw (an 'SQLError' from a locked or full disk, for
-- instance). The log must never stop a run, so callers wrap each write and
-- turn a failure into one warning line.
module Fisher.Runlog
  ( Runlog
  , runlogPath
  , openRunlog
  , closeRunlog
  , withRunlog
  , withRunlogRo
  , runlogOn
    -- * What is written
  , RunKind (..)
  , renderRunKind
  , parseRunKind
  , Capture (..)
  , renderCapture
  , parseCapture
  , NewRun (..)
  , RunEnd (..)
  , GoalRow (..)
  , startRun
  , event
  , endRun
  , upsertGoal
  , recordPolicyChange
  , withWrite
    -- * Lessons
  , LessonStatus (..)
  , renderLessonStatus
  , parseLessonStatus
  , LessonRow (..)
  , upsertLesson
  , getLesson
  , findLesson
  , listLessons
  , setLessonStatus
  , deleteLessons
  , modifyLesson
  , modifyLessonE
  , mergeLesson
  , addSpend
  , spendOn
  , lessonRowJson
    -- * What is read
  , RunRow (..)
  , listRuns
  , getRun
  , childRuns
  , runEvents
  , runEventsUpTo
  , runEventsOfKinds
  , runGoals
  , runProjects
  , listPolicyChanges
  , exportRuns
  , runRowJson
  , goalRowJson
    -- * Helpers
  , projectKeyOf
  , scrubEventData
  , redactArgv
  , capChars
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (SomeException, bracket, fromException, mask, onException, throwIO, try)
import Control.Monad (forM, forM_, void, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (dropTrailingPathSeparator, normalise, takeDirectory, (</>))

import Fisher.Db.ReadOnly (withRo)
import Fisher.Redact (maskToken, redactSensitiveText)
import Fisher.Sha256 (sha256Hex)

-- | An open run log: the connection and a lock that serialises this
-- process's use of it, holding whether the connection is still open.
-- Other processes (and other 'Runlog's on the same file) are kept apart by
-- SQLite's own locking and the busy timeout.
data Runlog = Runlog Connection (MVar Bool)

-- | @fisherHome/fisher/runs.db@.
runlogPath :: FilePath -> FilePath
runlogPath home = home </> "fisher" </> "runs.db"

schemaVersion :: Int
schemaVersion = 1

schemaSql :: [Query]
schemaSql =
  [ "CREATE TABLE IF NOT EXISTS schema_version (version INTEGER NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS runs (id TEXT PRIMARY KEY, kind TEXT NOT NULL, parent TEXT, project TEXT,\
    \ project_key TEXT, prover TEXT, backend TEXT, model TEXT, tier TEXT, workflow TEXT, argv TEXT NOT NULL,\
    \ started_at REAL NOT NULL, ended_at REAL, exit INTEGER, outcome TEXT, cost_usd REAL, input_tokens INTEGER,\
    \ output_tokens INTEGER, turns INTEGER, raw_dir TEXT, capture TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS goals (run TEXT NOT NULL, name TEXT NOT NULL, statement TEXT NOT NULL,\
    \ shape TEXT NOT NULL, status TEXT NOT NULL, closed_by TEXT, seconds_to_close REAL, dollars_to_close REAL)"
  , "CREATE TABLE IF NOT EXISTS events (run TEXT NOT NULL, seq INTEGER NOT NULL, t REAL NOT NULL,\
    \ kind TEXT NOT NULL, data TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS policy_changes (at REAL NOT NULL, project_key TEXT NOT NULL, shape TEXT NOT NULL,\
    \ knob TEXT NOT NULL, old TEXT NOT NULL, new TEXT NOT NULL, evidence TEXT NOT NULL)"
  , "CREATE INDEX IF NOT EXISTS idx_runs_project_started ON runs(project_key, started_at)"
  , "CREATE INDEX IF NOT EXISTS idx_goals_shape ON goals(shape)"
  , "CREATE UNIQUE INDEX IF NOT EXISTS idx_events_run_seq ON events(run, seq)"
    -- Lessons (added with schema version 1 kept: new tables only, so an
    -- older runs.db gains them on its next writable open)
  , "CREATE TABLE IF NOT EXISTS lessons (id INTEGER PRIMARY KEY AUTOINCREMENT, project_key TEXT NOT NULL,\
    \ kind TEXT NOT NULL, key TEXT NOT NULL, text TEXT NOT NULL, status TEXT NOT NULL, evidence_runs TEXT NOT NULL,\
    \ agreements INTEGER NOT NULL, first_seen REAL NOT NULL, activated_at REAL, retired_at REAL, runs_active TEXT NOT NULL,\
    \ hits_while_active INTEGER NOT NULL, attempts_while_active INTEGER NOT NULL, hits_before INTEGER NOT NULL,\
    \ attempts_before INTEGER NOT NULL, runs_before INTEGER NOT NULL, before_window TEXT NOT NULL DEFAULT '[]',\
    \ agreed_attempts TEXT NOT NULL DEFAULT '[]', UNIQUE (project_key, key))"
  , "CREATE TABLE IF NOT EXISTS lessons_spend (day TEXT PRIMARY KEY, dollars REAL NOT NULL)"
    -- A judged or self key is its kind, a colon and its text's key
    -- (Fisher.Learn.Judge.judgeKey), so it never shares a key with a rule
    -- row; a row stored before that gets the prefix (once; a row whose
    -- prefixed key is taken is left as it is)
  , "UPDATE OR IGNORE lessons SET key = kind || ':' || key\
    \ WHERE kind IN ('judged', 'self') AND substr(key, 1, length(kind) + 1) <> kind || ':'" ]

-- | Open (creating the directory and the schema), with WAL and a 10 s busy
-- timeout, as 'Fisher.Store.openStore' does. A file that is not a database
-- (or is corrupt) is renamed to @runs.db.broken-<YYYYmmdd-HHMMSS>@, with
-- its @-wal@ and @-shm@ files, and a fresh database is made in its place,
-- once. Any other failure is a 'Left' with the reason.
openRunlog :: FilePath -> IO (Either Text Runlog)
openRunlog path = do
  first <- try (setup path)
  case first of
    Right rl -> pure (Right rl)
    Left e
      | corrupt e -> do
          moved <- try (moveAside path) :: IO (Either SomeException ())
          case moved of
            Left m -> pure (Left ("runs.db is corrupt and could not be moved aside: " <> T.pack (show m)))
            Right () -> either (Left . reason) Right <$> try (setup path)
      | otherwise -> pure (Left (reason e))
  where
    corrupt :: SomeException -> Bool
    corrupt e = maybe False (\x -> sqlError x `elem` [ErrorNotADatabase, ErrorCorrupt]) (fromException e)
    reason :: SomeException -> Text
    reason e = "cannot open " <> T.pack path <> ": " <> T.pack (show e)

setup :: FilePath -> IO Runlog
setup path = do
  createDirectoryIfMissing True (takeDirectory path)
  conn <- open path
  ( do
      execute_ conn "PRAGMA busy_timeout = 10000"
      _ <- query_ conn "PRAGMA journal_mode=WAL" :: IO [Only Text]
      mapM_ (execute_ conn) schemaSql
      foldBareJudgeKeys conn
      -- One statement, so two processes opening a new file write one row.
      execute conn "INSERT INTO schema_version (version) SELECT ? WHERE NOT EXISTS (SELECT 1 FROM schema_version)" (Only schemaVersion)
      Runlog conn <$> newMVar True
    )
    `onException` close conn

-- A judged or self row still on a bare key after the prefixing UPDATE
-- (an older binary wrote it when the prefixed row already existed) would
-- take the project's rule hits of that key: its evidence runs and agreed
-- attempts are folded into the prefixed row (agreements counted once per
-- new attempt; a bare row with no agreed attempts recorded adds its
-- agreements), the stronger status of the two is kept (pinned, then
-- dropped, retired, active, candidate: a human's pin or drop is never
-- lost), with that row's timestamps, and the bare row is deleted, so the
-- key is the rule's again. One write transaction; a lessons table of an
-- older shape is left alone.
foldBareJudgeKeys :: Connection -> IO ()
foldBareJudgeKeys c = missingTable () . immediately c $ do
  bare <- mapMaybe decodeLesson <$> query_ c (Query ("SELECT " <> lessonColumns <> " FROM lessons WHERE kind IN ('judged', 'self')\
            \ AND substr(key, 1, length(kind) + 1) <> kind || ':'"))
  forM_ bare $ \r -> do
    twins <- query c (Query ("SELECT " <> lessonColumns <> " FROM lessons WHERE project_key = ? AND key = ?"))
               [SQLText (lrProjectKey r), SQLText (lrKind r <> ":" <> lrKey r)]
    case mapMaybe decodeLesson twins of
      t : _ -> do
        let new = [a | a <- lrAgreedAttempts r, a `notElem` lrAgreedAttempts t]
            added = if null (lrAgreedAttempts r) then lrAgreements r else length new
            merged = t { lrEvidenceRuns = lrEvidenceRuns t <> [x | x <- lrEvidenceRuns r, x `notElem` lrEvidenceRuns t]
                       , lrAgreedAttempts = lrAgreedAttempts t <> new, lrAgreements = lrAgreements t + added }
            strength = \case Pinned -> 4; Dropped -> 3; Retired -> 2; Active -> 1; Candidate -> (0 :: Int)
        writeLesson c $
          if strength (lrStatus r) > strength (lrStatus t)
            then merged {lrStatus = lrStatus r, lrActivatedAt = lrActivatedAt r, lrRetiredAt = lrRetiredAt r}
            else merged
        execute c "DELETE FROM lessons WHERE id = ?" [SQLInteger (fromIntegral (lrId r))]
      [] -> pure ()

moveAside :: FilePath -> IO ()
moveAside path = do
  stamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getZonedTime
  let broken = path <> ".broken-" <> stamp
  renameFile path broken
  mapM_
    (\suffix -> do
        there <- doesFileExist (path <> suffix)
        when there (renameFile (path <> suffix) (broken <> suffix)))
    ["-wal", "-shm"]

-- | Close the connection, once: a second close (a writer's own teardown,
-- then the 'withRunlog' around it) does nothing, and a failure to close is
-- dropped, so closing never throws.
closeRunlog :: Runlog -> IO ()
closeRunlog (Runlog c lock) = modifyMVar_ lock $ \open' -> do
  when open' (void (try (close c) :: IO (Either SomeException ())))
  pure False

-- | 'openRunlog' around an action. A failure to open is thrown as an
-- 'IOError' carrying the reason.
withRunlog :: FilePath -> (Runlog -> IO a) -> IO a
withRunlog path = bracket opened closeRunlog
  where
    opened = openRunlog path >>= either (throwIO . userError . T.unpack) pure

-- | The run log at a path opened read-only ('withRo'): no schema, no key
-- migration, no write transaction and no move of a corrupt file; only the
-- readers below may be used on it, and a table an older runs.db lacks
-- reads as empty. A failure to open or read is thrown.
withRunlogRo :: FilePath -> (Runlog -> IO a) -> IO a
withRunlogRo path k = withRo path (\c -> runlogOn c >>= k)

-- | A run log over a connection opened elsewhere (the web app opens
-- runs.db read-only): no schema is made and nothing is written, so only
-- the readers below may be used on it. The caller closes the connection.
runlogOn :: Connection -> IO Runlog
runlogOn c = Runlog c <$> newMVar True

-- A use of the connection, under the lock; an 'IOError' once the log is
-- closed (never a call on a closed handle).
locked :: Runlog -> (Connection -> IO a) -> IO a
locked (Runlog c lock) k = withMVar lock $ \open' ->
  if open' then k c else throwIO (userError "the run log is closed")

-- Kinds ------------------------------------------------------------------

data RunKind = SearchRun | SearchAttempt | LaunchRun | ChatRun | UiRun | CronRun | BenchImport | ImportRun
  deriving (Eq, Show, Enum, Bounded)

renderRunKind :: RunKind -> Text
renderRunKind = \case
  SearchRun -> "search"
  SearchAttempt -> "search-attempt"
  LaunchRun -> "launch"
  ChatRun -> "chat"
  UiRun -> "ui-run"
  CronRun -> "cron"
  BenchImport -> "bench-import"
  ImportRun -> "import"

parseRunKind :: Text -> Maybe RunKind
parseRunKind t = lookup t [(renderRunKind k, k) | k <- [minBound .. maxBound]]

-- | How much of a run was seen: all of it, read back from a backend's own
-- session files, or only its start, end and exit.
data Capture = Full | Imported | ShellOnly
  deriving (Eq, Show, Enum, Bounded)

renderCapture :: Capture -> Text
renderCapture = \case
  Full -> "full"
  Imported -> "imported"
  ShellOnly -> "shell-only"

parseCapture :: Text -> Maybe Capture
parseCapture t = lookup t [(renderCapture c, c) | c <- [minBound .. maxBound]]

-- Rows -------------------------------------------------------------------

-- | A run as it starts. Times are seconds since the epoch.
data NewRun = NewRun
  { nrId :: Text
  , nrKind :: RunKind
  , nrParent :: Maybe Text
  , nrProject :: Maybe FilePath
  , nrProver :: Maybe Text
  , nrBackend :: Maybe Text
  , nrModel :: Maybe Text
  , nrTier :: Maybe Text
  , nrWorkflow :: Maybe Text
  , nrArgv :: [Text]
  , nrStartedAt :: Double
  , nrRawDir :: Maybe FilePath
  , nrCapture :: Capture
  }
  deriving (Eq, Show)

data RunEnd = RunEnd
  { reEndedAt :: Double
  , reExit :: Maybe Int
  , reOutcome :: Text
  , reCostUsd :: Maybe Double
  , reInputTokens :: Maybe Int
  , reOutputTokens :: Maybe Int
  , reTurns :: Maybe Int
  }
  deriving (Eq, Show)

data GoalRow = GoalRow
  { grName :: Text
  , grStatement :: Text
  , grShape :: Text
  , grStatus :: Text
  , grClosedBy :: Maybe Text
  , grSecondsToClose :: Maybe Double
  , grDollarsToClose :: Maybe Double
  }
  deriving (Eq, Show)

-- | Every column of @runs@.
data RunRow = RunRow
  { rrId :: Text
  , rrKind :: RunKind
  , rrParent :: Maybe Text
  , rrProject :: Maybe FilePath
  , rrProjectKey :: Maybe Text
  , rrProver :: Maybe Text
  , rrBackend :: Maybe Text
  , rrModel :: Maybe Text
  , rrTier :: Maybe Text
  , rrWorkflow :: Maybe Text
  , rrArgv :: [Text]
  , rrStartedAt :: Double
  , rrEndedAt :: Maybe Double
  , rrExit :: Maybe Int
  , rrOutcome :: Maybe Text
  , rrCostUsd :: Maybe Double
  , rrInputTokens :: Maybe Int
  , rrOutputTokens :: Maybe Int
  , rrTurns :: Maybe Int
  , rrRawDir :: Maybe FilePath
  , rrCapture :: Capture
  }
  deriving (Eq, Show)

runColumns :: Text
runColumns =
  "id, kind, parent, project, project_key, prover, backend, model, tier, workflow, argv, started_at, ended_at,\
  \ exit, outcome, cost_usd, input_tokens, output_tokens, turns, raw_dir, capture"

-- | The project key: the first 16 hex digits of the SHA-256 of the
-- normalised root (the lemma library's key, 'Fisher.Search.Library.libraryKey').
projectKeyOf :: FilePath -> Text
projectKeyOf root = T.take 16 (sha256Hex (TE.encodeUtf8 (T.pack (canon root))))
  where
    canon = dropTrailingPathSeparator . normalise

-- Writing ----------------------------------------------------------------

-- | Record a run's start (@INSERT OR REPLACE@). Starting an id that is
-- already logged starts it afresh: its row is replaced (end, outcome and
-- cost back to null) and its events and goals are deleted, in the same
-- transaction. A writer that adds to a run started elsewhere (the native
-- agent under a search attempt, say) must not call 'startRun' for it.
-- The argv is redacted ('redactArgv').
startRun :: Runlog -> NewRun -> IO ()
startRun rl n = locked rl $ \c -> immediately c $ do
  execute c "DELETE FROM events WHERE run = ?" [SQLText (nrId n)]
  execute c "DELETE FROM goals WHERE run = ?" [SQLText (nrId n)]
  execute
    c
    "INSERT OR REPLACE INTO runs (id, kind, parent, project, project_key, prover, backend, model, tier, workflow,\
    \ argv, started_at, ended_at, exit, outcome, cost_usd, input_tokens, output_tokens, turns, raw_dir, capture)\
    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ?, ?)"
    [ SQLText (nrId n)
    , SQLText (renderRunKind (nrKind n))
    , mtext (nrParent n)
    , mtext (T.pack <$> nrProject n)
    , mtext (projectKeyOf <$> nrProject n)
    , mtext (nrProver n)
    , mtext (nrBackend n)
    , mtext (nrModel n)
    , mtext (nrTier n)
    , mtext (nrWorkflow n)
    , SQLText (jsonText (A.toJSON (redactArgv (nrArgv n))))
    , SQLFloat (nrStartedAt n)
    , mtext (T.pack <$> nrRawDir n)
    , SQLText (renderCapture (nrCapture n))
    ]

-- | Append an event to a run: run id, @t@ (seconds since the run started),
-- kind and data. The data is scrubbed ('scrubEventData') here. The event's
-- @seq@ is one more than the run's largest, computed in the same statement
-- as the insert, so concurrent writers never share a number.
event :: Runlog -> Text -> Double -> Text -> Value -> IO ()
event rl run t kind d = locked rl $ \c ->
  execute
    c
    "INSERT INTO events (run, seq, t, kind, data)\
    \ SELECT ?, COALESCE(MAX(seq), 0) + 1, ?, ?, ? FROM events WHERE run = ?"
    [SQLText run, SQLFloat t, SQLText kind, SQLText (jsonText (scrubEventData d)), SQLText run]

-- | Record a run's end.
endRun :: Runlog -> Text -> RunEnd -> IO ()
endRun rl run e = locked rl $ \c ->
  execute
    c
    "UPDATE runs SET ended_at = ?, exit = ?, outcome = ?, cost_usd = ?, input_tokens = ?, output_tokens = ?, turns = ?\
    \ WHERE id = ?"
    [ SQLFloat (reEndedAt e)
    , mint (reExit e)
    , SQLText (reOutcome e)
    , mdouble (reCostUsd e)
    , mint (reInputTokens e)
    , mint (reOutputTokens e)
    , mint (reTurns e)
    , SQLText run
    ]

-- | Write a goal of a run, replacing the run's earlier row for the same
-- goal name.
upsertGoal :: Runlog -> Text -> GoalRow -> IO ()
upsertGoal rl run g = locked rl $ \c -> immediately c $ do
  execute c "DELETE FROM goals WHERE run = ? AND name = ?" [SQLText run, SQLText (grName g)]
  execute
    c
    "INSERT INTO goals (run, name, statement, shape, status, closed_by, seconds_to_close, dollars_to_close)\
    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    [ SQLText run
    , SQLText (grName g)
    , SQLText (grStatement g)
    , SQLText (grShape g)
    , SQLText (grStatus g)
    , mtext (grClosedBy g)
    , mdouble (grSecondsToClose g)
    , mdouble (grDollarsToClose g)
    ]

-- | A change of the learned policy: at, project key, shape, knob, the old
-- and new values and the evidence.
recordPolicyChange :: Runlog -> Double -> Text -> Text -> Text -> Value -> Value -> Value -> IO ()
recordPolicyChange rl at key shape knob old new evidence = locked rl $ \c ->
  execute
    c
    "INSERT INTO policy_changes (at, project_key, shape, knob, old, new, evidence) VALUES (?, ?, ?, ?, ?, ?, ?)"
    [ SQLFloat at
    , SQLText key
    , SQLText shape
    , SQLText knob
    , SQLText (jsonText old)
    , SQLText (jsonText new)
    , SQLText (jsonText evidence)
    ]

-- | One write transaction (BEGIN IMMEDIATE) on the log's connection, for a
-- writer that needs several statements together. It is committed when the
-- action returns and rolled back when it throws.
withWrite :: Runlog -> (Connection -> IO a) -> IO a
withWrite rl k = locked rl $ \c -> immediately c (k c)

-- A write transaction taken up front (BEGIN IMMEDIATE), so the busy
-- timeout covers it. Masked, as sqlite-simple's withImmediateTransaction
-- is: an asynchronous exception (an attempt cancelled, a Ctrl-C) can land
-- only inside the action or the COMMIT, and both roll back, so no
-- transaction is ever left open on a shared connection.
immediately :: Connection -> IO a -> IO a
immediately c act = mask $ \restore -> do
  execute_ c "BEGIN IMMEDIATE"
  r <- restore act `onException` rollback
  execute_ c "COMMIT" `onException` rollback
  pure r
  where
    rollback = void (try (execute_ c "ROLLBACK") :: IO (Either SomeException ()))

-- Lessons ----------------------------------------------------------------

-- | Where a lesson stands: proposed, shown to agents, shown and kept from
-- retirement, retired by measurement, or dropped by the user.
data LessonStatus = Candidate | Active | Pinned | Retired | Dropped
  deriving (Eq, Show, Enum, Bounded)

renderLessonStatus :: LessonStatus -> Text
renderLessonStatus = \case
  Candidate -> "candidate"
  Active -> "active"
  Pinned -> "pinned"
  Retired -> "retired"
  Dropped -> "dropped"

parseLessonStatus :: Text -> Maybe LessonStatus
parseLessonStatus t = lookup t [(renderLessonStatus s, s) | s <- [minBound .. maxBound]]

-- | Every column of @lessons@. The project key is @*@ for a global
-- lesson; the run-id lists are stored as JSON text; times are seconds
-- since the epoch. An 'lrId' of 0 is a lesson not yet stored.
-- 'lrBeforeWindow' is the before window as (run, hits, attempts), oldest
-- first; when it is not empty the three before counters read back as its
-- sums ('lrRunsBefore' its length), so the window is what the
-- measurement uses and the counters are its summary.
data LessonRow = LessonRow
  { lrId :: Int
  , lrProjectKey :: Text
  , lrKind :: Text
  , lrKey :: Text
  , lrText :: Text
  , lrStatus :: LessonStatus
  , lrEvidenceRuns :: [Text]
  , lrAgreements :: Int
  , lrFirstSeen :: Double
  , lrActivatedAt :: Maybe Double
  , lrRetiredAt :: Maybe Double
  , lrRunsActive :: [Text]
  , lrHitsWhileActive :: Int
  , lrAttemptsWhileActive :: Int
  , lrHitsBefore :: Int
  , lrAttemptsBefore :: Int
  , lrRunsBefore :: Int
  , lrBeforeWindow :: [(Text, Int, Int)]
  , lrAgreedAttempts :: [Text]
  -- ^ the attempts whose judge call agreed, once each ('lrAgreements' is
  -- their number for a judged or self lesson)
  }
  deriving (Eq, Show)

-- The columns after the id, in the order 'lessonValues' gives them.
lessonDataColumns :: [Text]
lessonDataColumns =
  [ "project_key", "kind", "key", "text", "status", "evidence_runs", "agreements", "first_seen", "activated_at"
  , "retired_at", "runs_active", "hits_while_active", "attempts_while_active", "hits_before", "attempts_before"
  , "runs_before", "before_window", "agreed_attempts" ]

lessonColumns :: Text
lessonColumns = T.intercalate ", " ("id" : lessonDataColumns)

lessonValues :: LessonRow -> [SQLData]
lessonValues l =
  [ SQLText (lrProjectKey l)
  , SQLText (lrKind l)
  , SQLText (lrKey l)
  , SQLText (lrText l)
  , SQLText (renderLessonStatus (lrStatus l))
  , SQLText (jsonText (A.toJSON (lrEvidenceRuns l)))
  , SQLInteger (fromIntegral (lrAgreements l))
  , SQLFloat (lrFirstSeen l)
  , mdouble (lrActivatedAt l)
  , mdouble (lrRetiredAt l)
  , SQLText (jsonText (A.toJSON (lrRunsActive l)))
  , SQLInteger (fromIntegral (lrHitsWhileActive l))
  , SQLInteger (fromIntegral (lrAttemptsWhileActive l))
  , SQLInteger (fromIntegral (lrHitsBefore l))
  , SQLInteger (fromIntegral (lrAttemptsBefore l))
  , SQLInteger (fromIntegral (lrRunsBefore l))
  , SQLText (jsonText (A.toJSON (lrBeforeWindow l)))
  , SQLText (jsonText (A.toJSON (lrAgreedAttempts l)))
  ]

-- | Store a lesson and answer its id. A row with an id (above 0) that is
-- stored is updated in place, every column. Any other row is inserted; an
-- insert whose (project key, key) is already stored refreshes only that
-- row's kind, text, evidence runs and agreements, so the miner's fresh
-- candidate for a known key never demotes an active or pinned lesson nor
-- resets its first sighting, timestamps, runs in force or counters, and
-- the lesson keeps its id. One write transaction.
upsertLesson :: Runlog -> LessonRow -> IO Int
upsertLesson rl l = locked rl $ \c -> immediately c $ do
  updated <-
    if lrId l > 0
      then do
        execute
          c
          (Query ("UPDATE lessons SET " <> T.intercalate ", " [col <> " = ?" | col <- lessonDataColumns] <> " WHERE id = ?"))
          (lessonValues l <> [SQLInteger (fromIntegral (lrId l))])
        (> 0) <$> changes c
      else pure False
  if updated
    then pure (lrId l)
    else do
      let marks = T.intercalate ", " (map (const "?") ("id" : lessonDataColumns))
          refresh = T.intercalate ", " [col <> " = excluded." <> col | col <- ["kind", "text", "evidence_runs", "agreements"]]
      execute
        c
        (Query ("INSERT INTO lessons (" <> lessonColumns <> ") VALUES (" <> marks <> ")\
                \ ON CONFLICT (project_key, key) DO UPDATE SET " <> refresh))
        ((if lrId l > 0 then SQLInteger (fromIntegral (lrId l)) else SQLNull) : lessonValues l)
      rows <- query c "SELECT id FROM lessons WHERE project_key = ? AND key = ?" [SQLText (lrProjectKey l), SQLText (lrKey l)]
      case rows of
        Only i : _ -> pure i
        [] -> throwIO (userError "the lesson was not stored")

-- | A lesson by id.
getLesson :: Runlog -> Int -> IO (Maybe LessonRow)
getLesson rl i = locked rl $ \c -> missingTable Nothing $ do
  rows <- query c (Query ("SELECT " <> lessonColumns <> " FROM lessons WHERE id = ?")) [SQLInteger (fromIntegral i)]
  pure (case mapMaybe decodeLesson rows of r : _ -> Just r; [] -> Nothing)

-- | A lesson by project key and key.
findLesson :: Runlog -> Text -> Text -> IO (Maybe LessonRow)
findLesson rl key k = locked rl $ \c -> missingTable Nothing $ do
  rows <- query c (Query ("SELECT " <> lessonColumns <> " FROM lessons WHERE project_key = ? AND key = ?")) [SQLText key, SQLText k]
  pure (case mapMaybe decodeLesson rows of r : _ -> Just r; [] -> Nothing)

-- | The lessons of a project key (all of them for 'Nothing') with one of
-- the statuses (any status for @[]@), the oldest first seen first.
listLessons :: Runlog -> Maybe Text -> [LessonStatus] -> IO [LessonRow]
listLessons rl key statuses = locked rl $ \c -> missingTable [] $ do
  let inStatus
        | null statuses = ""
        | otherwise = " AND status IN (" <> T.intercalate ", " (map (const "?") statuses) <> ")"
  rows <-
    query
      c
      (Query ("SELECT " <> lessonColumns <> " FROM lessons WHERE (? IS NULL OR project_key = ?)" <> inStatus
              <> " ORDER BY first_seen, id"))
      ([mtext key, mtext key] <> map (SQLText . renderLessonStatus) statuses)
  pure (mapMaybe decodeLesson rows)

-- | Change a lesson's status at a time: active sets @activated_at@ and
-- clears @retired_at@; pinned keeps an earlier @activated_at@ (else sets
-- it) and clears @retired_at@; retired and dropped set @retired_at@;
-- candidate clears both. A status set again (active on an active lesson,
-- say) keeps its timestamp, since the measurement counts from it. A
-- missing id changes nothing.
setLessonStatus :: Runlog -> Int -> LessonStatus -> Double -> IO ()
setLessonStatus rl i st at = locked rl $ \c ->
  let same col = col <> " = CASE WHEN status = '" <> renderLessonStatus st <> "' THEN " <> col <> " ELSE ? END"
      (sets, vals) = case st of
        Candidate -> ("activated_at = NULL, retired_at = NULL", [])
        Active -> (same "activated_at" <> ", retired_at = NULL", [SQLFloat at])
        Pinned -> ("activated_at = COALESCE(activated_at, ?), retired_at = NULL", [SQLFloat at])
        Retired -> (same "retired_at", [SQLFloat at])
        Dropped -> (same "retired_at", [SQLFloat at])
   in execute
        c
        (Query ("UPDATE lessons SET status = ?, " <> sets <> " WHERE id = ?"))
        ([SQLText (renderLessonStatus st)] <> vals <> [SQLInteger (fromIntegral i)])

-- | Delete every lesson of a project key, of any status, in one write
-- transaction; answers how many. The keys can then be proposed afresh.
deleteLessons :: Runlog -> Text -> IO Int
deleteLessons rl key = withWrite rl $ \c -> missingTable 0 $ do
  execute c "DELETE FROM lessons WHERE project_key = ?" [SQLText key]
  changes c

-- | Change a stored lesson by a function of it, read and written in one
-- write transaction, so a change made meanwhile by another pass (a
-- promotion, a retirement) is never written over with an older copy.
-- Every column is written from the function's result, the id kept.
-- 'Nothing' for a missing id (nothing written).
modifyLesson :: Runlog -> Int -> (LessonRow -> LessonRow) -> IO (Maybe LessonRow)
modifyLesson rl i f = withWrite rl $ \c -> do
  cur <- lessonById c i
  case cur of
    Nothing -> pure Nothing
    Just r -> do
      let r' = (f r) {lrId = i}
      writeLesson c r'
      lessonById c i

-- | 'modifyLesson' by a function that can refuse: the row is written only
-- on a 'Right', in the same write transaction as the read, so the decision
-- is made on the row as stored. @Right Nothing@ for a missing id (nothing
-- written); a 'Left' is the refusal (nothing written); else the row as
-- stored after the write.
modifyLessonE :: Runlog -> Int -> (LessonRow -> Either Text LessonRow) -> IO (Either Text (Maybe LessonRow))
modifyLessonE rl i f = withWrite rl $ \c -> do
  cur <- lessonById c i
  case cur of
    Nothing -> pure (Right Nothing)
    Just r -> case f r of
      Left why -> pure (Left why)
      Right r' -> do
        writeLesson c r' {lrId = i}
        Right <$> lessonById c i

-- | The lesson of a project key and key, changed by the merge when it is
-- stored, else the fresh row inserted (its id ignored); read and written
-- in one write transaction, so two first sightings of one key make one row
-- that holds both. Answers the row as stored.
mergeLesson :: Runlog -> Text -> Text -> LessonRow -> (LessonRow -> LessonRow) -> IO LessonRow
mergeLesson rl pk k fresh f = withWrite rl $ \c -> do
  rows <- query c (Query ("SELECT " <> lessonColumns <> " FROM lessons WHERE project_key = ? AND key = ?")) [SQLText pk, SQLText k]
  case mapMaybe decodeLesson rows of
    r : _ -> do
      let r' = (f r) {lrId = lrId r, lrProjectKey = pk, lrKey = k}
      writeLesson c r'
      fromMaybe r' <$> lessonById c (lrId r)
    [] -> do
      let new = fresh {lrProjectKey = pk, lrKey = k}
          marks = T.intercalate ", " (map (const "?") lessonDataColumns)
      execute c (Query ("INSERT INTO lessons (" <> T.intercalate ", " lessonDataColumns <> ") VALUES (" <> marks <> ")")) (lessonValues new)
      back <- query c (Query ("SELECT " <> lessonColumns <> " FROM lessons WHERE project_key = ? AND key = ?")) [SQLText pk, SQLText k]
      case mapMaybe decodeLesson back of
        r : _ -> pure r
        [] -> throwIO (userError "the lesson was not stored")

lessonById :: Connection -> Int -> IO (Maybe LessonRow)
lessonById c i = do
  rows <- query c (Query ("SELECT " <> lessonColumns <> " FROM lessons WHERE id = ?")) [SQLInteger (fromIntegral i)]
  pure (case mapMaybe decodeLesson rows of r : _ -> Just r; [] -> Nothing)

writeLesson :: Connection -> LessonRow -> IO ()
writeLesson c r =
  execute
    c
    (Query ("UPDATE lessons SET " <> T.intercalate ", " [col <> " = ?" | col <- lessonDataColumns] <> " WHERE id = ?"))
    (lessonValues r <> [SQLInteger (fromIntegral (lrId r))])

-- | Add dollars to a day's judge spend (the day as @YYYY-MM-DD@).
addSpend :: Runlog -> Text -> Double -> IO ()
addSpend rl day dollars = locked rl $ \c ->
  execute
    c
    "INSERT INTO lessons_spend (day, dollars) VALUES (?, ?)\
    \ ON CONFLICT (day) DO UPDATE SET dollars = dollars + excluded.dollars"
    [SQLText day, SQLFloat dollars]

-- | A day's judge spend; 0 for a day with none.
spendOn :: Runlog -> Text -> IO Double
spendOn rl day = locked rl $ \c -> missingTable 0 $ do
  rows <- query c "SELECT dollars FROM lessons_spend WHERE day = ?" [SQLText day]
  pure (case rows of Only d : _ -> d; [] -> 0)

-- | A lesson as a JSON object keyed by its column names.
lessonRowJson :: LessonRow -> Value
lessonRowJson l =
  object
    [ "id" .= lrId l
    , "project_key" .= lrProjectKey l
    , "kind" .= lrKind l
    , "key" .= lrKey l
    , "text" .= lrText l
    , "status" .= renderLessonStatus (lrStatus l)
    , "evidence_runs" .= lrEvidenceRuns l
    , "agreements" .= lrAgreements l
    , "first_seen" .= lrFirstSeen l
    , "activated_at" .= lrActivatedAt l
    , "retired_at" .= lrRetiredAt l
    , "runs_active" .= lrRunsActive l
    , "hits_while_active" .= lrHitsWhileActive l
    , "attempts_while_active" .= lrAttemptsWhileActive l
    , "hits_before" .= lrHitsBefore l
    , "attempts_before" .= lrAttemptsBefore l
    , "runs_before" .= lrRunsBefore l
    , "before_window" .= lrBeforeWindow l
    , "agreed_attempts" .= lrAgreedAttempts l
    ]

-- A row whose status this build does not know is left out.
decodeLesson :: [SQLData] -> Maybe LessonRow
decodeLesson = \case
  [i, pk, kd, k, tx', st, ev, ag, fs, act, ret, ra, hwa, awa, hb, ab, rb, bw, aa] -> do
    lid <- int i
    status <- txt st >>= parseLessonStatus
    fmap (summarised (list bw)) $ LessonRow lid
      <$> txt pk
      <*> txt kd
      <*> txt k
      <*> txt tx'
      <*> pure status
      <*> pure (list ev)
      <*> int ag
      <*> dbl fs
      <*> pure (dbl act)
      <*> pure (dbl ret)
      <*> pure (list ra)
      <*> int hwa
      <*> int awa
      <*> int hb
      <*> int ab
      <*> int rb
      <*> pure []
      <*> pure (list aa)
  _ -> Nothing
  where
    txt = \case SQLText t -> Just t; _ -> Nothing
    dbl = \case SQLFloat d -> Just d; SQLInteger n -> Just (fromIntegral n); _ -> Nothing
    int = \case SQLInteger n -> Just (fromIntegral n); SQLFloat d -> Just (truncate d); _ -> Nothing
    list :: A.FromJSON b => SQLData -> [b]
    list v = fromMaybe [] (txt v >>= A.decodeStrict . TE.encodeUtf8)
    summarised w r
      | null w = r
      | otherwise =
          r { lrBeforeWindow = w, lrRunsBefore = length w
            , lrHitsBefore = sum [h | (_, h, _) <- w], lrAttemptsBefore = sum [a | (_, _, a) <- w] }

-- A read of a table an older runs.db may not have yet: on a connection
-- opened read-only no schema is made, so a missing table reads as empty.
missingTable :: a -> IO a -> IO a
missingTable empty act = do
  r <- try act
  case r of
    Right v -> pure v
    Left e
      | any (`T.isInfixOf` sqlErrorDetails e) ["no such table", "no such column"] -> pure empty
      | otherwise -> throwIO e

-- Scrubbing --------------------------------------------------------------

redactKeys, capKeys :: [Text]
redactKeys = ["args", "result", "text", "error", "preview", "lesson"]
capKeys = ["args", "result", "text"]

-- | The data of an event as it is written:
--
-- * every string under a secret-named key ('secretKey'), at any depth and
--   whatever the key above it, becomes @***@ (@Bearer ***@ for a bearer
--   value); numbers and booleans under it are kept;
-- * every string anywhere under an @args@, @result@, @text@, @error@, @lesson@ or
--   @preview@ key, and every string of data that is not an object (a bare
--   string or array), is redacted ('redactSensitiveText');
-- * an @args@, @result@ or @text@ value is capped as a whole: a string at
--   65536 characters ('capChars'), any other value whose JSON text is
--   longer than 65536 characters becomes that text, capped.
--
-- The cap comes before the redaction: a string is cut to 65536 + 8192
-- characters before the patterns run on it (a @-----BEGIN@ block the cut leaves
-- unterminated is dropped whole), so a megabyte of tool output
-- costs what 64 KB does.
scrubEventData :: Value -> Value
scrubEventData = \case
  v@(Object _) -> go False v
  v -> go True v
  where
    go red = \case
      Object o -> Object (KM.mapWithKey (\k v -> field red (K.toText k) v) o)
      Array a -> Array (fmap (go red) a)
      String t | red -> String (scrubText t)
      v -> v
    field red n v
      | secretKey n = maskStrings v
      | n `elem` capKeys = capped (maskSecretKeys v)
      | otherwise = go (red || n `elem` redactKeys) v
    capped = \case
      String t -> String (scrubText t)
      v | jsonLonger v -> String (scrubText (jsonText v))
        | otherwise -> go True v
    jsonLonger v = LBS.length (A.encode v) > fromIntegral capLimit

-- Every string under a secret-named key masked, at any depth, and nothing
-- else changed.
maskSecretKeys :: Value -> Value
maskSecretKeys = \case
  Object o -> Object (KM.mapWithKey (\k v -> if secretKey (K.toText k) then maskStrings v else maskSecretKeys v) o)
  Array a -> Array (fmap maskSecretKeys a)
  v -> v

maskStrings :: Value -> Value
maskStrings = \case
  String t -> String (maskSecret t)
  Object o -> Object (fmap maskStrings o)
  Array a -> Array (fmap maskStrings a)
  v -> v

maskSecret :: Text -> Text
maskSecret t
  | T.null t = t
  | T.toLower (T.take 7 t) == "bearer " = T.take 7 t <> "***"
  | otherwise = "***"

-- | A key whose value is a secret, in any case: the field names
-- 'redactSensitiveText' knows in JSON text (api_key, token, password,
-- ...), the HTTP headers @authorization@ and @x-api-key@, credentials, and
-- any name ending in @_key@, @_token@, @_secret@ or @_password@, as
-- Replay's environment masking has it (@GLM_API_KEY@, @GITHUB_TOKEN@). A
-- plural count (@max_tokens@, @input_tokens@) and @token_count@ are not.
secretKey :: Text -> Bool
secretKey k =
  let l = T.toLower k
   in l `elem` secretKeyNames || any (`T.isSuffixOf` l) ["_key", "-key", "_token", "-token", "_secret", "_password"]

secretKeyNames :: [Text]
secretKeyNames =
  [ "api_key", "apikey", "token", "secret", "password", "passwd", "access_token", "refresh_token", "auth_token"
  , "bearer", "secret_value", "raw_secret", "secret_input", "key_material", "credential", "credentials"
  , "authorization", "proxy-authorization", "x-api-key", "cookie", "set-cookie" ]

-- Cut to the cap plus a margin (8192 characters, more than a PEM block),
-- redact, then cap: the cut may only end a secret past the cap, where the
-- cap drops it anyway. A @-----BEGIN@ block left with no @-----END@ after
-- it (the cut fell inside it, or the text itself was cut) is replaced to
-- the end by @[REDACTED PRIVATE KEY]@, since the key pattern needs both
-- markers.
scrubText :: Text -> Text
scrubText t =
  let cut = T.compareLength t preLimit == GT
      r = redactSensitiveText (unterminatedBlock (T.take preLimit t))
   in if cut || T.compareLength r capLimit == GT then T.take capLimit r <> truncatedMark else r
  where
    preLimit = capLimit + 8192

-- From the last @-----BEGIN@ to the end, when no @-----END@ follows it.
unterminatedBlock :: Text -> Text
unterminatedBlock t = case T.breakOnAll "-----BEGIN" t of
  [] -> t
  ms ->
    let (before, from) = last ms
     in if "-----END" `T.isInfixOf` from then t else before <> "[REDACTED PRIVATE KEY]"

-- | At most 65536 characters, with @"... [truncated]"@ after a cut.
capChars :: Text -> Text
capChars s
  | T.compareLength s capLimit /= GT = s
  | otherwise = T.take capLimit s <> truncatedMark

truncatedMark :: Text
truncatedMark = "... [truncated]"

capLimit :: Int
capLimit = 65536

-- | Each word redacted, and the value of a secret-looking flag masked:
-- the word after @--api-key@, @--token@, @-password@ and the like, and the
-- part after the @=@ of @--token=...@. A flag is secret-looking when its
-- name holds key, token, secret, password, passwd, auth or credential
-- (any case); a plural @tokens@ (@--max-tokens 100@) is a count, not one.
redactArgv :: [Text] -> [Text]
redactArgv = go
  where
    go = \case
      w : v : rest
        | Just name <- flagName w, not ("=" `T.isInfixOf` w), secretFlag name, not (isFlag v) ->
            redactSensitiveText w : maskToken v : go rest
      w : rest -> one w : go rest
      [] -> []
    one w = case T.breakOn "=" w of
      (f, eqv)
        | not (T.null eqv), Just name <- flagName f, secretFlag name -> f <> "=" <> maskToken (T.drop 1 eqv)
      _ -> redactSensitiveText w
    isFlag w = "-" `T.isPrefixOf` w && T.length w > 1
    flagName w
      | isFlag w = Just (T.toLower (T.dropWhile (== '-') w))
      | otherwise = Nothing
    secretFlag name =
      let n = T.replace "tokens" "" name
       in any (`T.isInfixOf` n) ["key", "token", "secret", "password", "passwd", "auth", "credential"]

-- Reading ----------------------------------------------------------------

-- | Runs started at or after @since@, of a project key and of a kind (each
-- filter optional), newest first, at most @limit@ (0 or less: all).
listRuns :: Runlog -> Maybe Double -> Maybe Text -> Maybe RunKind -> Int -> IO [RunRow]
listRuns rl since key kind limit = locked rl $ \c -> do
  rows <-
    query
      c
      -- sqlite-simple binds only unnamed ? parameters: each filter's value
      -- is given twice
      (Query ("SELECT " <> runColumns <> " FROM runs WHERE (? IS NULL OR started_at >= ?)\
              \ AND (? IS NULL OR project_key = ?) AND (? IS NULL OR kind = ?)\
              \ ORDER BY started_at DESC, id DESC LIMIT ?"))
      ( [v | f <- [mdouble since, mtext key, mtext (renderRunKind <$> kind)], v <- [f, f]]
          <> [SQLInteger (fromIntegral (if limit <= 0 then -1 else limit))] )
  pure (mapMaybe decodeRun rows)

getRun :: Runlog -> Text -> IO (Maybe RunRow)
getRun rl run = locked rl $ \c -> do
  rows <- query c (Query ("SELECT " <> runColumns <> " FROM runs WHERE id = ?")) [SQLText run]
  pure (case rows of r : _ -> decodeRun r; [] -> Nothing)

-- | The runs whose parent is the given run, oldest first.
childRuns :: Runlog -> Text -> IO [RunRow]
childRuns rl run = locked rl $ \c -> do
  rows <- query c (Query ("SELECT " <> runColumns <> " FROM runs WHERE parent = ? ORDER BY started_at, id")) [SQLText run]
  pure (mapMaybe decodeRun rows)

-- | A run's events in order: seq, t, kind, data.
runEvents :: Runlog -> Text -> IO [(Int, Double, Text, Value)]
runEvents rl run = locked rl $ \c -> eventsOf c run

-- | A run's first events, at most @limit@ of them, in order.
runEventsUpTo :: Runlog -> Text -> Int -> IO [(Int, Double, Text, Value)]
runEventsUpTo rl run limit = locked rl $ \c -> do
  rows <-
    query c "SELECT seq, t, kind, data FROM events WHERE run = ? ORDER BY seq LIMIT ?"
      [SQLText run, SQLInteger (fromIntegral (max 0 limit))] :: IO [(Int, Double, Text, Text)]
  pure [(s, t, k, decodeJson d) | (s, t, k, d) <- rows]

-- | The projects runs were in, the one with the latest run first.
runProjects :: Runlog -> IO [FilePath]
runProjects rl = locked rl $ \c -> do
  rows <- query_ c "SELECT project FROM runs WHERE project IS NOT NULL GROUP BY project ORDER BY MAX(started_at) DESC, project" :: IO [Only Text]
  pure [T.unpack p | Only p <- rows]

-- | A project's policy changes (by key), newest first, at most @limit@ (0
-- or less: all): at, shape, knob, old, new, evidence.
listPolicyChanges :: Runlog -> Text -> Int -> IO [(Double, Text, Text, Value, Value, Value)]
listPolicyChanges rl key limit = locked rl $ \c -> do
  rows <-
    query c "SELECT at, shape, knob, old, new, evidence FROM policy_changes WHERE project_key = ?\
            \ ORDER BY at DESC, rowid DESC LIMIT ?"
      [SQLText key, SQLInteger (fromIntegral (if limit <= 0 then -1 else limit))] :: IO [(Double, Text, Text, Text, Text, Text)]
  pure [(at, sh, k, decodeJson o, decodeJson n, decodeJson e) | (at, sh, k, o, n, e) <- rows]

-- | The events of the given kinds across every run, by run then seq:
-- run, seq, t, kind, data. One query, so a reader that needs one kind of
-- event from many runs does not load every event of each.
runEventsOfKinds :: Runlog -> [Text] -> IO [(Text, Int, Double, Text, Value)]
runEventsOfKinds _ [] = pure []
runEventsOfKinds rl kinds = locked rl $ \c -> do
  let marks = T.intercalate ", " (map (const "?") kinds)
  rows <-
    query c (Query ("SELECT run, seq, t, kind, data FROM events WHERE kind IN (" <> marks <> ") ORDER BY run, seq"))
      (map SQLText kinds) :: IO [(Text, Int, Double, Text, Text)]
  pure [(r, s, t, k, decodeJson d) | (r, s, t, k, d) <- rows]

eventsOf :: Connection -> Text -> IO [(Int, Double, Text, Value)]
eventsOf c run = do
  rows <- query c "SELECT seq, t, kind, data FROM events WHERE run = ? ORDER BY seq" [SQLText run] :: IO [(Int, Double, Text, Text)]
  pure [(s, t, k, decodeJson d) | (s, t, k, d) <- rows]

runGoals :: Runlog -> Text -> IO [GoalRow]
runGoals rl run = locked rl $ \c -> goalsOf c run

goalsOf :: Connection -> Text -> IO [GoalRow]
goalsOf c run = do
  rows <-
    query
      c
      "SELECT name, statement, shape, status, closed_by, seconds_to_close, dollars_to_close FROM goals\
      \ WHERE run = ? ORDER BY rowid"
      [SQLText run]
  pure [GoalRow n s sh st cb sec dol | (n, s, sh, st, cb, sec, dol) <- rows]

-- | One JSON object per run started at or after @since@ (and of a project
-- key), oldest first: the run's columns plus @goals@ and @events@.
exportRuns :: Runlog -> Maybe Double -> Maybe Text -> IO [Value]
exportRuns rl since key = locked rl $ \c -> do
  rows <-
    query
      c
      (Query ("SELECT " <> runColumns <> " FROM runs WHERE (? IS NULL OR started_at >= ?)\
              \ AND (? IS NULL OR project_key = ?) ORDER BY started_at, id"))
      [v | f <- [mdouble since, mtext key], v <- [f, f]]
  forM (mapMaybe decodeRun rows) $ \r -> do
    gs <- goalsOf c (rrId r)
    es <- eventsOf c (rrId r)
    pure
      ( Object
          ( runRowObject r
              <> KM.fromList
                [ ("goals", A.toJSON (map goalRowJson gs))
                , ("events", A.toJSON [object ["seq" .= s, "t" .= t, "kind" .= k, "data" .= d] | (s, t, k, d) <- es])
                ]
          )
      )

-- | A run as a JSON object keyed by its column names.
runRowJson :: RunRow -> Value
runRowJson = Object . runRowObject

runRowObject :: RunRow -> A.Object
runRowObject r =
  KM.fromList
    [ "id" .= rrId r
    , "kind" .= renderRunKind (rrKind r)
    , "parent" .= rrParent r
    , "project" .= rrProject r
    , "project_key" .= rrProjectKey r
    , "prover" .= rrProver r
    , "backend" .= rrBackend r
    , "model" .= rrModel r
    , "tier" .= rrTier r
    , "workflow" .= rrWorkflow r
    , "argv" .= rrArgv r
    , "started_at" .= rrStartedAt r
    , "ended_at" .= rrEndedAt r
    , "exit" .= rrExit r
    , "outcome" .= rrOutcome r
    , "cost_usd" .= rrCostUsd r
    , "input_tokens" .= rrInputTokens r
    , "output_tokens" .= rrOutputTokens r
    , "turns" .= rrTurns r
    , "raw_dir" .= rrRawDir r
    , "capture" .= renderCapture (rrCapture r)
    ]

-- | A goal row as a JSON object keyed by its column names.
goalRowJson :: GoalRow -> Value
goalRowJson g =
  object
    [ "name" .= grName g
    , "statement" .= grStatement g
    , "shape" .= grShape g
    , "status" .= grStatus g
    , "closed_by" .= grClosedBy g
    , "seconds_to_close" .= grSecondsToClose g
    , "dollars_to_close" .= grDollarsToClose g
    ]

-- A row whose kind or capture this build does not know is left out.
decodeRun :: [SQLData] -> Maybe RunRow
decodeRun = \case
  [i, k, par, proj, key, prov, back, model, tier, wf, argv, st, en, ex, out, cost, inp, outp, turns, raw, cap] -> do
    rid <- tx i
    kind <- tx k >>= parseRunKind
    capture <- tx cap >>= parseCapture
    started <- dbl st
    pure
      RunRow
        { rrId = rid
        , rrKind = kind
        , rrParent = tx par
        , rrProject = T.unpack <$> tx proj
        , rrProjectKey = tx key
        , rrProver = tx prov
        , rrBackend = tx back
        , rrModel = tx model
        , rrTier = tx tier
        , rrWorkflow = tx wf
        , rrArgv = fromMaybe [] (tx argv >>= \a -> A.decodeStrict (TE.encodeUtf8 a))
        , rrStartedAt = started
        , rrEndedAt = dbl en
        , rrExit = int ex
        , rrOutcome = tx out
        , rrCostUsd = dbl cost
        , rrInputTokens = int inp
        , rrOutputTokens = int outp
        , rrTurns = int turns
        , rrRawDir = T.unpack <$> tx raw
        , rrCapture = capture
        }
  _ -> Nothing
  where
    tx = \case SQLText t -> Just t; _ -> Nothing
    dbl = \case SQLFloat d -> Just d; SQLInteger n -> Just (fromIntegral n); _ -> Nothing
    int = \case SQLInteger n -> Just (fromIntegral n); SQLFloat d -> Just (truncate d); _ -> Nothing

-- Fields -----------------------------------------------------------------

mtext :: Maybe Text -> SQLData
mtext = maybe SQLNull SQLText

mdouble :: Maybe Double -> SQLData
mdouble = maybe SQLNull SQLFloat

mint :: Maybe Int -> SQLData
mint = maybe SQLNull (SQLInteger . fromIntegral)

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . LBS.toStrict . A.encode

decodeJson :: Text -> Value
decodeJson t = fromMaybe Null (A.decodeStrict (TE.encodeUtf8 t))
