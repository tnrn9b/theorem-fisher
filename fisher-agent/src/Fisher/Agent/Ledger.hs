-- | The event ledger (Fisher UI): what a running session does, as rows
-- the UI server streams to the page, in two tables of @state.db@:
--
-- * @ui_events(session_id, seq, at, kind, data)@: one row per event,
--   numbered 1, 2, ... per session with no gaps (a resume carries on from
--   the last number); @data@ is the event's JSON.
-- * @ui_runs(tag, session_id, pid, started)@: a run the UI started (its
--   @FISHER_UI_TAG@), so the server finds the session it became.
--
-- The kinds and their data: @tool_start@ {name, args}, @tool_end@ {name,
-- callId, preview, ok}, @thinking@ {label}, @message@ {role, text}, @cost@
-- {dollars, input, output}, @step@ {seq, path, hash}, @inbox@ {text},
-- @end@ {status}.
--
-- Only the agent writes these tables, and only with @FISHER_UI_LEDGER=1@;
-- without it nothing here touches @state.db@. This module only writes; the
-- UI server reads the tables read-only ('Fisher.UI.Db.ledgerEvents').
module Fisher.Agent.Ledger
  ( UiEvent (..)
  , Ledger
  , openLedger
  , closeLedger
  , emit
  , recordRun
  ) where

import Control.Concurrent.MVar
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data UiEvent = UiEvent {ueKind :: Text, ueData :: Value}
  deriving (Eq, Show)

-- | An open ledger for one session; this handle's writes are serialised.
-- The numbers are computed by SQLite as each row goes in, so two writers
-- of one session (two handles, two processes) share one sequence.
data Ledger = Ledger
  { lgConn :: Connection
  , lgSession :: Text
  , lgLock :: MVar ()
  }

schema :: [Query]
schema =
  [ "CREATE TABLE IF NOT EXISTS ui_events (session_id TEXT NOT NULL, seq INTEGER NOT NULL, at REAL NOT NULL,\
    \ kind TEXT NOT NULL, data TEXT NOT NULL, PRIMARY KEY (session_id, seq))"
  , "CREATE TABLE IF NOT EXISTS ui_runs (tag TEXT PRIMARY KEY, session_id TEXT NOT NULL, pid INTEGER NOT NULL, started REAL NOT NULL)" ]

connect :: FilePath -> IO Connection
connect path = do
  createDirectoryIfMissing True (takeDirectory path)
  conn <- open path
  execute_ conn "PRAGMA busy_timeout = 10000"
  _ <- query_ conn "PRAGMA journal_mode=WAL" :: IO [Only Text]
  pure conn

-- | Open the ledger in the @state.db@ at the path for a session, making
-- the two tables when they are not there.
openLedger :: FilePath -> Text -> IO Ledger
openLedger path sid = do
  conn <- connect path
  mapM_ (execute_ conn) schema
  m <- newMVar ()
  pure (Ledger conn sid m)

closeLedger :: Ledger -> IO ()
closeLedger lg = withMVar (lgLock lg) (\_ -> close (lgConn lg))

now :: IO Double
now = realToFrac <$> getPOSIXTime

-- | Write an event; its number, one past the session's highest when the
-- row goes in (one statement, so no other writer takes it between). A
-- failed write takes no number.
emit :: Ledger -> UiEvent -> IO Int
emit lg ev =
  withMVar (lgLock lg) $ \_ -> do
    t <- now
    execute (lgConn lg)
      "INSERT INTO ui_events (session_id, seq, at, kind, data)\
      \ SELECT ?, COALESCE(MAX(seq), 0) + 1, ?, ?, ? FROM ui_events WHERE session_id = ?"
      (lgSession lg, t, ueKind ev, TE.decodeUtf8With TEE.lenientDecode (BL.toStrict (encode (ueData ev))), lgSession lg)
    rows <- query_ (lgConn lg) "SELECT seq FROM ui_events WHERE rowid = last_insert_rowid()" :: IO [Only Int]
    case rows of
      (Only k : _) -> pure k
      [] -> ioError (userError "the ledger row just written is not there")

-- | Record that the run with the tag is this session (process id, start).
-- The first record of a tag stands: a later process given the same tag (a
-- chat its tools started) does not move it.
recordRun :: Ledger -> Text -> Int -> IO ()
recordRun lg tag pid = withMVar (lgLock lg) $ \_ -> do
  t <- now
  execute (lgConn lg) "INSERT OR IGNORE INTO ui_runs (tag, session_id, pid, started) VALUES (?, ?, ?, ?)" (tag, lgSession lg, pid, t)
