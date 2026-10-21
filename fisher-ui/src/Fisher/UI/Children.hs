-- | The runs the UI starts: each a child process in its own process group,
-- stdin from /dev/null, its output to a log file. Stopping takes the whole
-- tree: the child's descendants are listed first (@ps@, walked by parent),
-- since some live in process groups and sessions of their own (MCP stdio
-- servers, the Lean transport, terminal and sandbox tools) and are
-- reparented once their parent dies. SIGTERM goes to the child's group,
-- to each descendant's group and to each descendant; after a grace (10 s)
-- SIGKILL goes the same way to what is left (with any descendant started
-- meanwhile). A waiter thread reaps the child as it exits, so a crash
-- leaves no zombie and never reaches the server.
module Fisher.UI.Children
  ( Child (..)
  , startChild
  , spawnChild
  , spawnChildPiped
  , stopChild
  , stopChildWith
  , groupAlive
  , descendants
  , newTag
  , stopGrace
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.List (nub)
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (Handle, IOMode (..), hClose, openBinaryFile, withBinaryFile)
import System.Posix.Process (getProcessGroupID)
import System.Posix.Signals (Signal, nullSignal, sigKILL, sigTERM, signalProcess, signalProcessGroup)
import System.Process (CreateProcess (..), Pid, StdStream (..), createPipe, createProcess, getPid, proc, readProcess, waitForProcess)
import Text.Read (readMaybe)
import Text.Printf (printf)


-- | A started run: its tag (the @FISHER_UI_TAG@ it was given), its pid
-- (also its process group) and when it started (POSIX seconds).
data Child = Child { chTag :: Text, chPid :: Pid, chStarted :: Double }
  deriving (Eq, Show)

-- | The grace between SIGTERM and SIGKILL, in microseconds.
stopGrace :: Int
stopGrace = 10 * 1000000

-- | 64 random bits as 16 hex characters.
newTag :: IO Text
newTag = do
  bytes <- withBinaryFile "/dev/urandom" ReadMode (`B.hGet` 8)
  pure (T.pack (concatMap (printf "%02x") (B.unpack bytes)))

-- | Start @argv@ in a directory with these variables over the server's
-- own, output discarded. The tag is the @FISHER_UI_TAG@ among them (a new
-- one when it is not there).
startChild :: FilePath -> [String] -> [(String, String)] -> IO Child
startChild cwd' argv env = fst <$> spawnChild Nothing cwd' argv env

-- | 'startChild' with the output appended to a log file, and an MVar
-- filled with the exit status when the child exits.
spawnChild :: Maybe FilePath -> FilePath -> [String] -> [(String, String)] -> IO (Child, MVar ExitCode)
spawnChild logPath cwd' argv env = case argv of
  [] -> ioError (userError "fisher ui: an empty command")
  (exe : args) -> do
    tag <- maybe newTag (pure . T.pack) (lookup "FISHER_UI_TAG" env)
    base <- getEnvironment
    let full = M.toList (M.union (M.fromList env) (M.fromList base))
    devnull <- openBinaryFile "/dev/null" ReadMode
    out <- maybe (openBinaryFile "/dev/null" WriteMode) openLog logPath
    started <- realToFrac <$> getPOSIXTime
    r <- try $
      createProcess
        (proc exe args)
          { cwd = Just cwd', env = Just full, std_in = UseHandle devnull, std_out = UseHandle out
          , std_err = UseHandle out, create_group = True, close_fds = True }
    closeAll [devnull, out]
    case r of
      Left (e :: SomeException) -> ioError (userError ("fisher ui: could not start " <> exe <> ": " <> show e))
      Right (_, _, _, ph) -> do
        mpid <- getPid ph
        exited <- newEmptyMVar
        _ <- forkIO $ do
          c <- try (waitForProcess ph)
          putMVar exited (either (\(_ :: SomeException) -> ExitFailure 255) id c)
        case mpid of
          Just pid -> pure (Child tag pid started, exited)
          -- it exited before its pid was read: there is no group to signal
          Nothing -> pure (Child tag 0 started, exited)
  where
    openLog p = do
      createDirectoryIfMissing True (takeDirectory p)
      openBinaryFile p AppendMode
    closeAll = mapM_ (\h -> void (try (hClose h) :: IO (Either SomeException ())))

-- | A child that is meant to be interactive (@claude auth login@, which
-- may read a pasted code): as 'spawnChild', but its stdin is a pipe the
-- caller writes to, and its stdout and stderr go together to a pipe the
-- caller reads (nothing is written to disk). The caller closes both.
spawnChildPiped :: FilePath -> [String] -> [(String, String)] -> IO (Child, MVar ExitCode, Handle, Handle)
spawnChildPiped cwd' argv env = case argv of
  [] -> ioError (userError "fisher ui: an empty command")
  (exe : args) -> do
    tag <- maybe newTag (pure . T.pack) (lookup "FISHER_UI_TAG" env)
    base <- getEnvironment
    let full = M.toList (M.union (M.fromList env) (M.fromList base))
    (readOut, writeOut) <- createPipe
    started <- realToFrac <$> getPOSIXTime
    r <- try $
      createProcess
        (proc exe args)
          { cwd = Just cwd', env = Just full, std_in = CreatePipe, std_out = UseHandle writeOut
          , std_err = UseHandle writeOut, create_group = True, close_fds = True }
    void (try (hClose writeOut) :: IO (Either SomeException ()))
    case r of
      Left (e :: SomeException) -> do
        void (try (hClose readOut) :: IO (Either SomeException ()))
        ioError (userError ("fisher ui: could not start " <> exe <> ": " <> show e))
      Right (Just hin, _, _, ph) -> do
        mpid <- getPid ph
        exited <- newEmptyMVar
        _ <- forkIO $ do
          c <- try (waitForProcess ph)
          putMVar exited (either (\(_ :: SomeException) -> ExitFailure 255) id c)
        pure (Child tag (fromMaybe0 mpid) started, exited, hin, readOut)
      Right _ -> ioError (userError "fisher ui: no stdin pipe")
  where
    fromMaybe0 = maybe 0 id

-- | Whether any process of the group is still there.
groupAlive :: Pid -> IO Bool
groupAlive pid
  | pid <= 0 = pure False
  | otherwise = either (\(_ :: SomeException) -> False) (const True) <$> try (signalProcessGroup nullSignal pid)

-- | Every process below these (not themselves), with its process group,
-- from one @ps -A -o pid=,ppid=,pgid=@ (none when ps fails).
descendants :: [Pid] -> IO [(Pid, Pid)]
descendants roots = do
  r <- try (readProcess "ps" ["-A", "-o", "pid=,ppid=,pgid="] "")
  let table = case r of
        Left (_ :: SomeException) -> []
        Right out -> [(p, pp, g) | l <- lines out, [Just p, Just pp, Just g] <- [map readMaybe (words l)]]
      walk seen [] = seen
      walk seen (x : xs) =
        let kids = [(p, g) | (p, pp, g) <- table, pp == x, not (S.member p (S.map fst seen))]
         in walk (foldr S.insert seen kids) (map fst kids <> xs)
  pure (S.toList (walk S.empty (filter (> 1) roots)))

alivePid :: Pid -> IO Bool
alivePid p = either (\(_ :: SomeException) -> False) (const True) <$> try (signalProcess nullSignal p)

-- | SIGTERM to the child's tree; SIGKILL to what is left of it after 10 s.
-- Returns once all of it is gone (or 5 s after the kill).
stopChild :: Child -> IO ()
stopChild = stopChildWith stopGrace

-- | 'stopChild' with another grace (microseconds).
stopChildWith :: Int -> Child -> IO ()
stopChildWith grace ch = do
  let pid = chPid ch
  own <- getProcessGroupID
  tree0 <- if pid > 0 then descendants [pid] else pure []
  let signalTree s tree = do
        signalGroup s pid
        forM_ (nub [g | (_, g) <- tree, g > 1, g /= own, g /= pid]) (signalGroup s)
        forM_ (map fst tree) (signalPid s)
  signalTree sigTERM tree0
  gone <- waitGone pid (map fst tree0) grace
  unless gone $ do
    -- what is left, and anything it started meanwhile
    more <- descendants (pid : map fst tree0)
    let tree = nub (tree0 <> more)
    signalTree sigKILL tree
    void (waitGone pid (map fst tree) 5000000)
  where
    signalGroup :: Signal -> Pid -> IO ()
    signalGroup s g
      | g <= 1 = pure ()
      | otherwise = void (try (signalProcessGroup s g) :: IO (Either SomeException ()))
    signalPid :: Signal -> Pid -> IO ()
    signalPid s p
      | p <= 1 = pure ()
      | otherwise = void (try (signalProcess s p) :: IO (Either SomeException ()))
    waitGone :: Pid -> [Pid] -> Int -> IO Bool
    waitGone pid ps left = do
      g <- groupAlive pid
      a <- or <$> mapM alivePid ps
      if not (g || a)
        then pure True
        else if left <= 0 then pure False else threadDelay 50000 >> waitGone pid ps (left - 50000)
