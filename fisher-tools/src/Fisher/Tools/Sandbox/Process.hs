-- | Running one child the way upstream's backends do: stdout and stderr
-- merged into one pipe, stdin (when given) written from its own thread,
-- the child in a new session, and a 0.2 s loop that watches for exit, the
-- interrupt flag and the deadline.
module Fisher.Tools.Sandbox.Process
  ( Spawn (..)
  , Stop (..)
  , Outcome (..)
  , runPolled
  , killGroup
  ) where

import Control.Concurrent
import Control.Exception (IOException, SomeException, catch, finally, try)
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTime)
import System.Exit (ExitCode (..))
import System.IO
import System.Posix.Signals (Signal, signalProcess, signalProcessGroup, sigKILL, sigTERM)
import System.Process
import System.Timeout (timeout)

import Fisher.Tools.Sandbox (Interrupt, isInterrupted)
import Fisher.Tools.Sandbox.Common (decodeOutput)

data Spawn = Spawn
  { spArgv :: [String]
  , spCwd :: Maybe FilePath
  , spEnv :: Maybe [(String, String)]
  , spStdin :: Maybe Text
  -- ^ 'Just' gives the child a pipe (written and closed), 'Nothing' /dev/null
  }

-- | How a backend stops a child on interrupt and on timeout.
data Stop
  = -- | local: SIGTERM to the process group, SIGKILL after 1 s on interrupt;
    -- SIGTERM to the group on timeout
    StopGroup
  | -- | docker, ssh: terminate then kill the client on interrupt; kill it on
    -- timeout
    StopClient

data Outcome
  = Finished Int Text
  | TimedOut Text
  | Interrupted Text

runPolled :: Interrupt -> Stop -> Double -> Spawn -> IO Outcome
runPolled intr stopHow limit sp = do
  (r, w) <- createPipe
  devnull <- case spStdin sp of
    Nothing -> Just <$> openFile "/dev/null" ReadMode
    Just _ -> pure Nothing
  let cp =
        (proc (head (spArgv sp)) (tail (spArgv sp)))
          { cwd = rootFix <$> spCwd sp
          , env = spEnv sp
          , std_in = maybe CreatePipe UseHandle devnull
          , std_out = UseHandle w
          , std_err = UseHandle w
          , new_session = True
          }
  (mIn, _, _, ph) <- createProcess cp
  chunks <- newIORef []
  readerDone <- newEmptyMVar
  _ <- forkIO $ do
    let loop = do
          c <- BS.hGetSome r 65536 `catch` \(_ :: IOException) -> pure BS.empty
          unless (BS.null c) (modifyIORef' chunks (c :) >> loop)
    loop `finally` (hClose r `catch` \(_ :: IOException) -> pure ()) `finally` putMVar readerDone ()
  case (mIn, spStdin sp) of
    (Just h, Just txt) -> void . forkIO $ do
      _ <- try (BS.hPut h (TE.encodeUtf8 txt)) :: IO (Either SomeException ())
      _ <- try (hClose h) :: IO (Either SomeException ())
      pure ()
    _ -> pure ()
  exitVar <- newEmptyMVar
  _ <- forkIO (waitForProcess ph >>= putMVar exitVar)
  start <- getMonotonicTime
  let collected = decodeOutput . BS.concat . reverse <$> readIORef chunks
      joinReader secs = void (timeout (round (secs * 1e6 :: Double)) (readMVar readerDone))
      loop = do
        done <- timeout 200000 (readMVar exitVar)
        case done of
          Just code -> do
            joinReader 5
            Finished (exitInt code) <$> collected
          Nothing -> do
            stop <- isInterrupted intr
            now <- getMonotonicTime
            if stop
              then do
                case stopHow of
                  StopGroup -> do
                    killGroup ph sigTERM
                    gone <- timeout 1000000 (readMVar exitVar)
                    when (gone == Nothing) (killGroup ph sigKILL)
                  StopClient -> do
                    terminateProcess ph
                    gone <- timeout 1000000 (readMVar exitVar)
                    when (gone == Nothing) (signalPid ph sigKILL)
                joinReader 2
                Interrupted <$> collected
              else
                if now - start > limit
                  then do
                    case stopHow of
                      StopGroup -> killGroup ph sigTERM
                      StopClient -> signalPid ph sigKILL
                    joinReader 2
                    TimedOut <$> collected
                  else loop
  loop
  where
    -- process on macOS cannot start a child in "/" when it also sets the
    -- environment ("chdir: does not exist"); "/." is the same directory
    rootFix d = if d == "/" then "/." else d
    exitInt = \case
      ExitSuccess -> 0
      ExitFailure n -> n

-- | Signal the child's process group (it leads its own session).
killGroup :: ProcessHandle -> Signal -> IO ()
killGroup ph sig = do
  mpid <- getPid ph
  case mpid of
    Just pid -> signalProcessGroup sig pid `catch` \(_ :: IOException) -> signalProcess sig pid `catch` \(_ :: IOException) -> pure ()
    Nothing -> pure ()

signalPid :: ProcessHandle -> Signal -> IO ()
signalPid ph sig = do
  mpid <- getPid ph
  case mpid of
    Just pid -> signalProcess sig pid `catch` \(_ :: IOException) -> pure ()
    Nothing -> pure ()
