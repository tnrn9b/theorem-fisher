-- | Running @lake@ for the build and profile tools: stderr merged into
-- stdout, a new session so the whole process group can be killed (lake,
-- elan and lean together), an optional time limit.
module Fisher.Lean.Process
  ( ProcessRunner
  , runMerged
  , runSplit
  , ioErrorText
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, finally, try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hClose)
import System.IO.Error (ioeGetFileName, isDoesNotExistError, isPermissionError)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Exit (ExitCode (..))
import System.Process
import System.Timeout (timeout)

-- | cwd, argv, time limit in seconds: the exit code and the merged output,
-- or Nothing when the limit passed (the group is killed either way).
type ProcessRunner = FilePath -> [String] -> Maybe Double -> IO (Maybe (Int, BS.ByteString))

runMerged :: ProcessRunner
runMerged dir argv mlimit = case argv of
  [] -> pure (Just (0, BS.empty))
  (cmd : args) -> do
    (r, w) <- createPipe
    (_, _, _, ph) <-
      createProcess (proc cmd args) {cwd = Just dir, std_in = NoStream, std_out = UseHandle w, std_err = UseHandle w, new_session = True}
        `onIOError` (hClose r >> hClose w)
    out <- newEmptyMVar
    _ <- forkIO (BS.hGetContents r >>= putMVar out)
    mpid <- getPid ph
    let wait' = do
          code <- waitForProcess ph
          bytes <- takeMVar out
          pure (exitInt code, bytes)
    ( case mlimit of
        Nothing -> Just <$> wait'
        Just t -> timeout (round (t * 1000000)) wait' )
      `finally` maybe (pure ()) (\pid -> void (try @SomeException (signalProcessGroup sigKILL pid))) mpid
  where
    onIOError act cleanup = do
      x <- try @IOException act
      case x of
        Right v -> pure v
        Left e -> cleanup >> ioError e
    exitInt = \case
      ExitSuccess -> 0
      ExitFailure n -> n

-- | cwd, argv: the exit code, stdout and stderr, read to the end (rg for
-- lean_local_search).
runSplit :: FilePath -> [String] -> IO (Int, BS.ByteString, BS.ByteString)
runSplit dir argv = case argv of
  [] -> pure (0, BS.empty, BS.empty)
  (cmd : args) -> do
    (_, Just o, Just e, ph) <- createProcess (proc cmd args) {cwd = Just dir, std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe}
    errV <- newEmptyMVar
    _ <- forkIO (BS.hGetContents e >>= putMVar errV)
    out <- BS.hGetContents o
    err <- takeMVar errV
    code <- waitForProcess ph
    pure (case code of ExitSuccess -> 0; ExitFailure n -> n, out, err)

-- | @str(OSError)@ for a failed spawn of @cmd@, as Python words it.
ioErrorText :: String -> IOException -> Text
ioErrorText cmd e
  | isDoesNotExistError e = "[Errno 2] No such file or directory: " <> quoted
  | isPermissionError e = "[Errno 13] Permission denied: " <> quoted
  | otherwise = T.pack (show e)
  where
    quoted = "'" <> T.pack (maybe cmd id (ioeGetFileName e)) <> "'"
