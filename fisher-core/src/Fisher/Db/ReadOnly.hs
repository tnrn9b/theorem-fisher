-- | Opening an SQLite database read-only so that nothing beside it is
-- written: no schema, no migration, no @-wal@ or @-shm@ made where there
-- is none. The web app reads state.db and runs.db this way, and the
-- prompts read runs.db's lessons this way at launch.
module Fisher.Db.ReadOnly
  ( withRo
  , busyWindow
  , roUri
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, bracket, fromException, throwIO, try)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.SQLite.Simple
import System.Directory (doesFileExist)

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
-- writes nothing beside it. A read under @immutable=1@ that fails while a
-- @-wal@ file has appeared meanwhile (a writer opened the database after
-- the check) is run once more with @mode=ro@.
withRo :: FilePath -> (Connection -> IO a) -> IO a
withRo path k = do
  deadline <- addUTCTime (realToFrac busyWindow) <$> getCurrentTime
  let once live = try (bracket (open (roUri live path)) close (\c -> setup c >> k c))
      go = do
        live <- doesFileExist (path <> "-wal")
        r0 <- once live
        -- a writer that opened the database after the check: an immutable
        -- read may have seen a checkpoint under it, so read it again live
        r <- case r0 of
          Left e | not live, Nothing <- (fromException e :: Maybe SomeAsyncException) -> do
            nowLive <- doesFileExist (path <> "-wal")
            if nowLive then once True else pure r0
          _ -> pure r0
        case r of
          Right a -> pure a
          Left e
            | Just q <- fromException e, sqlError q `elem` [ErrorBusy, ErrorLocked] -> do
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
