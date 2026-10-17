-- | The run lock (Fisher UI): a chat process holds an exclusive fcntl lock
-- on @$GAUSS_HOME/fisher/steps/<root>.lock@ for as long as it lives, and
-- writes its process id to @<root>.pid@ beside it, so the UI server knows a
-- session is running whoever started it (a terminal, launch, another
-- server). The UI then sends to such a session through its inbox and never
-- resumes it.
--
-- fcntl locks belong to a process, and any close of a descriptor on the
-- file by that process drops them: the holder opens the file once and
-- never closes it ('holdRunLock' keeps every descriptor it opened), and
-- 'runLockHolder' is for other processes (in the holder's own process it
-- would see nothing, and its close would drop the lock).
module Fisher.Agent.RunLock
  ( runLockPath
  , runPidPath
  , holdRunLock
  , heldRunLockFd
  , runLockHolder
  , tryRunLock
  , releaseRunLock
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (SeekMode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO (LockRequest (..), OpenFileFlags (..), OpenMode (..), closeFd, defaultFileFlags, getLock, openFd, setLock)
import System.Posix.Process (getProcessID)
import System.Posix.Types (Fd, ProcessID)

import Fisher.Agent.Steps (stepsDir)

runLockPath :: FilePath -> Text -> FilePath
runLockPath gaussHome root = stepsDir gaussHome </> T.unpack (root <> ".lock")

runPidPath :: FilePath -> Text -> FilePath
runPidPath gaussHome root = stepsDir gaussHome </> T.unpack (root <> ".pid")

-- | The descriptors this process holds its run locks with, by lock file.
held :: MVar (M.Map FilePath Fd)
held = unsafePerformIO (newMVar M.empty)
{-# NOINLINE held #-}

-- | Take the session's run lock for the rest of this process's life and
-- write the pid file: whether it was taken (False: another process holds
-- it for more than 2 s, or the file could not be opened; the run goes on
-- either way).
holdRunLock :: FilePath -> Text -> IO Bool
holdRunLock gaussHome root = modifyMVar held $ \m -> do
  let p = runLockPath gaussHome root
  case M.lookup p m of
    Just _ -> pure (m, True)
    Nothing -> do
      r <- try @SomeException $ do
        createDirectoryIfMissing True (stepsDir gaussHome)
        fd <- openLockFile p
        -- a prune holds it for a moment: wait up to 2 s for that
        let attempt :: Int -> IO (Either SomeException ())
            attempt n = do
              r' <- try @SomeException (setLock fd (WriteLock, AbsoluteSeek, 0, 0))
              case r' of
                Left _ | n > 0 -> threadDelay 100000 >> attempt (n - 1)
                _ -> pure r'
        got <- attempt 20
        case got of
          Right () -> do
            pid <- getProcessID
            writeFile (runPidPath gaussHome root) (show pid <> "\n")
            pure (Just fd)
          Left _ -> closeFd fd >> pure Nothing
      pure $ case r of
        Right (Just fd) -> (M.insert p fd m, True)
        _ -> (m, False)

-- | The lock file opened for reading and writing, made when it is not
-- there, close-on-exec: the tools' subprocesses do not inherit it.
openLockFile :: FilePath -> IO Fd
openLockFile p = openFd p ReadWrite defaultFileFlags {creat = Just 0o644, cloexec = True}

-- | The descriptor 'holdRunLock' holds the session's lock with, if it does.
heldRunLockFd :: FilePath -> Text -> IO (Maybe Fd)
heldRunLockFd gaussHome root = M.lookup (runLockPath gaussHome root) <$> readMVar held

-- | Take the session's run lock now if no process holds it (the lock file
-- made when missing): its descriptor, to give back with
-- 'releaseRunLock'; Nothing when another process holds it. For a process
-- that acts on a session only while it is not running (prune): a chat
-- that starts meanwhile finds the lock taken. The file is never removed.
tryRunLock :: FilePath -> Text -> IO (Maybe Fd)
tryRunLock gaussHome root = do
  r <- try @SomeException $ do
    createDirectoryIfMissing True (stepsDir gaussHome)
    fd <- openLockFile (runLockPath gaussHome root)
    got <- try @SomeException (setLock fd (WriteLock, AbsoluteSeek, 0, 0))
    case got of
      Right () -> pure (Just fd)
      Left _ -> closeFd fd >> pure Nothing
  pure (either (const Nothing) id r)

-- | Give back a lock 'tryRunLock' took.
releaseRunLock :: Fd -> IO ()
releaseRunLock fd = () <$ try @SomeException (closeFd fd)

-- | The process that holds the session's run lock, if any (for a process
-- other than the holder; see the module's note).
runLockHolder :: FilePath -> Text -> IO (Maybe ProcessID)
runLockHolder gaussHome root = do
  let p = runLockPath gaussHome root
  here <- doesFileExist p
  if not here
    then pure Nothing
    else do
      r <- try @SomeException $ do
        fd <- openFd p ReadWrite defaultFileFlags {cloexec = True}
        l <- try @SomeException (getLock fd (WriteLock, AbsoluteSeek, 0, 0))
        closeFd fd
        pure (either (const Nothing) (fmap fst) l)
      pure (either (const Nothing) id r)
