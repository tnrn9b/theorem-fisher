-- | @fisher ui --prune DAYS@: what the UI keeps grows with every session
-- and run (docs/ui.md, "What grows"), and nothing else deletes it. This
-- deletes, for each session not running whose last change is older than
-- DAYS days, while it holds that session's run lock (taken if no process
-- holds it, see 'Fisher.Agent.RunLock'; the lock file itself stays): its
-- @ui_events@ rows and @ui_runs@ rows in state.db, its step files in
-- @fisher/steps@ (the log, its lock, the verdicts and the pid
-- file) and its inbox in @fisher/inbox@. A session's last change is the
-- latest of its last ledger event and its files' times. It also deletes
-- the run logs in @fisher/ui-runs@ older than DAYS days, but not one
-- whose run's session is running. The sessions
-- themselves (state.db's sessions and messages, the replay logs) are
-- Fisher's and stay.
module Fisher.UI.Prune
  ( Pruned (..)
  , pruneUi
  , pruneUiAt
  , pruneUiWith
  , pruneLine
  ) where

import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (filterM, forM)
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Database.SQLite.Simple
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory, removeFile)
import System.FilePath (takeBaseName, (</>))

import Fisher.Agent.Inbox (inboxDir)
import Fisher.Agent.RunLock (releaseRunLock, runLockHolder, runLockPath, tryRunLock)
import Fisher.Agent.Steps (stepsDir)

data Pruned = Pruned
  { prSessions :: Int
  -- ^ sessions whose UI records went
  , prRows :: Int
  -- ^ ledger rows (ui_events and ui_runs)
  , prFiles :: Int
  -- ^ step and inbox files
  , prLogs :: Int
  -- ^ run logs
  }
  deriving (Eq, Show)

pruneLine :: Pruned -> String
pruneLine p =
  "fisher ui --prune: " <> show (prSessions p) <> " sessions (" <> show (prRows p) <> " ledger rows, "
    <> show (prFiles p) <> " step and inbox files), " <> show (prLogs p) <> " run logs"

-- | Prune the home's UI records older than this many days.
pruneUi :: FilePath -> Double -> IO Pruned
pruneUi home days = do
  t <- realToFrac <$> getPOSIXTime
  pruneUiAt home (t - days * 86400)

-- | The suffixes of a session's files, after its id.
stepSuffixes, inboxSuffixes :: [String]
stepSuffixes = [".jsonl.lock", ".verdicts.json.tmp", ".verdicts.json", ".jsonl", ".lock", ".pid"]
inboxSuffixes = [".offset.tmp", ".offset", ".jsonl"]

-- | A session's files in a directory, by session id.
filesBy :: [String] -> FilePath -> IO (M.Map Text [FilePath])
filesBy suffixes dir = do
  here <- doesDirectoryExist dir
  names <- if here then sort <$> listDirectory dir else pure []
  pure . M.fromListWith (<>) $
    [ (T.pack (take (length n - length suf) n), [dir </> n])
    | n <- names, Just suf <- [firstSuffix n], length n > length suf ]
  where
    firstSuffix n = case [x | x <- suffixes, x `isSuffixOf` n] of
      (x : _) -> Just x
      [] -> Nothing

mtime :: FilePath -> IO Double
mtime p = realToFrac . utcTimeToPOSIXSeconds <$> getModificationTime p

-- | Prune what changed last before this time (POSIX seconds).
pruneUiAt :: FilePath -> Double -> IO Pruned
pruneUiAt = pruneAtWith (\_ -> pure ())

-- | 'pruneUi' with an action run for each session while prune holds its
-- run lock, before its records go (for the tests).
pruneUiWith :: (Text -> IO ()) -> FilePath -> Double -> IO Pruned
pruneUiWith during home days = do
  t <- realToFrac <$> getPOSIXTime
  pruneAtWith during home (t - days * 86400)

pruneAtWith :: (Text -> IO ()) -> FilePath -> Double -> IO Pruned
pruneAtWith during home cutoff = do
  let db = home </> "state.db"
  steps <- filesBy stepSuffixes (stepsDir home)
  inbox <- filesBy inboxSuffixes (inboxDir home)
  haveDb <- doesFileExist db
  let withDb :: a -> (Connection -> IO a) -> IO a
      withDb dflt k = if not haveDb then pure dflt else bracket (open db) close $ \c -> do
        execute_ c "PRAGMA busy_timeout = 10000"
        tables <- map fromOnly <$> (query_ c "SELECT name FROM sqlite_master WHERE type = 'table' AND name IN ('ui_events', 'ui_runs')" :: IO [Only Text])
        if "ui_events" `elem` tables && "ui_runs" `elem` tables then k c else pure dflt
  lastEvents <- withDb M.empty $ \c ->
    M.fromList <$> (query_ c "SELECT session_id, MAX(at) FROM ui_events GROUP BY session_id" :: IO [(Text, Double)])
  -- each run's session, read before any row goes (for the run logs)
  runSessions <- withDb M.empty $ \c ->
    M.fromList <$> (query_ c "SELECT tag, session_id FROM ui_runs" :: IO [(Text, Text)])
  let roots = S.toList (S.unions [M.keysSet lastEvents, M.keysSet steps, M.keysSet inbox])
      lockOf r = runLockPath home r
  candidates <- fmap concat . forM roots $ \r -> do
    let files = M.findWithDefault [] r steps <> M.findWithDefault [] r inbox
    times <- forM files $ \f -> either (\(_ :: SomeException) -> 0) id <$> try (mtime f)
    let latest = maximum (fromMaybe 0 (M.lookup r lastEvents) : times)
    -- the run lock itself is never removed: a chat starting now opens it
    pure [(r, filter (/= lockOf r) files) | latest < cutoff]
  -- each session pruned while prune holds its run lock: a chat that starts
  -- meanwhile finds it taken, and one already running keeps it (skipped)
  done <- fmap concat . forM candidates $ \(r, files) ->
    tryRunLock home r >>= \case
      Nothing -> pure []
      Just fd -> (`finally` releaseRunLock fd) $ do
        during r
        rows <- withDb 0 $ \c -> withTransaction c $ do
          execute c "DELETE FROM ui_events WHERE session_id = ?" (Only r)
          a <- changes c
          execute c "DELETE FROM ui_runs WHERE session_id = ? AND started < ?" (r, cutoff)
          b <- changes c
          pure (a + b)
        gone <- length <$> filterM (\f -> either (\(_ :: SomeException) -> False) (const True) <$> try (removeFile f)) files
        pure [(rows, gone)]
  -- the run logs: old ones, but never one whose session is running (no
  -- lock is held by prune here)
  let logDir = home </> "fisher" </> "ui-runs"
  logHere <- doesDirectoryExist logDir
  logs <- if logHere then map (logDir </>) <$> listDirectory logDir else pure []
  oldLogs <- filterM (\f -> either (\(_ :: SomeException) -> False) (< cutoff) <$> try (mtime f)) logs
  idle <- filterM (\f -> case M.lookup (T.pack (takeBaseName f)) runSessions of
                      Just sess -> isNothing <$> runLockHolder home sess
                      Nothing -> pure True) oldLogs
  removed <- filterM (\f -> either (\(_ :: SomeException) -> False) (const True) <$> try (removeFile f)) idle
  pure (Pruned (length done) (sum (map fst done)) (sum (map snd done)) (length removed))
