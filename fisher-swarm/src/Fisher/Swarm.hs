-- | The swarm: managed workflow sessions running beside the CLI
-- (@SwarmManager@ in @swarm_manager.py@). An interactive session runs
-- behind a PTY whose output is always read by one thread, kept in a
-- rolling buffer, and passed to the terminal while someone is attached; a
-- background session runs on pipes and reports progress from its
-- stream-json output.
module Fisher.Swarm
  ( Swarm
  , newSwarm
  , SpawnSpec (..)
  , Task
  , taskInfo
  , spawnInteractive
  , spawnBackground
  , cancelTask
  , getTask
  , latestTask
  , listTasks
  , taskView
  , allViews
  , waitTask
  , setOnComplete
  , AttachIO (..)
  , terminalAttachIO
  , attachTask
  , recentLimit
  , nowSeconds
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, finally, try, IOException)
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8)
import Numeric (showHex)
import System.Exit (ExitCode (..))
import System.IO
import System.IO.Error (isEOFError)
import System.Posix.IO (dup, fdToHandle, stdInput, stdOutput)
import System.Posix.Signals (installHandler, Handler (..), signalProcess, signalProcessGroup, sigTERM)
import System.Posix.Signals.Exts (windowChange)
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd)
import System.Process
import System.Timeout (timeout)

import Fisher.Prover.Kind (ProverKind (..))
import Fisher.Swarm.Event
import Fisher.Swarm.Launch (normalizeClaudeLaunch)
import Fisher.Swarm.Pty
import Fisher.Swarm.Types
import Fisher.Workflow.Types (Env)
import qualified Data.Map.Strict as M

recentLimit :: Int
recentLimit = 256 * 1024

nowSeconds :: IO Double
nowSeconds = realToFrac <$> getPOSIXTime

data St = St
  { stStatus :: Status
  , stProgress :: Progress
  , stStart :: Maybe Double
  , stEnd :: Maybe Double
  }

data Task = Task
  { taskInfo :: TaskInfo
  , tState :: TVar St
  , tProc :: TVar (Maybe ProcessHandle)
  , tMasterW :: TVar (Maybe Handle)
  , tMasterFd :: TVar (Maybe Fd)
  , tRecent :: TVar BS.ByteString
  , tSink :: TVar (Maybe (TQueue BS.ByteString))
  , tExit :: TMVar ExitCode
  , tDone :: TMVar ()
  }

data Swarm = Swarm
  { swTasks :: TVar (Map.Map Int Task)
  , swCounter :: TVar Int
  , swOnComplete :: TVar (Maybe (TaskView -> IO ()))
  , swIsRoot :: Bool
  , swTrustHome :: FilePath
  }

-- | A swarm. @isRoot@ picks Claude Code's permission flags; @trustHome@ is
-- the home whose @.claude.json@ gets the workspace-trust entries.
newSwarm :: Bool -> FilePath -> IO Swarm
newSwarm isRoot home = Swarm <$> newTVarIO Map.empty <*> newTVarIO 0 <*> newTVarIO Nothing <*> pure isRoot <*> pure home

setOnComplete :: Swarm -> Maybe (TaskView -> IO ()) -> IO ()
setOnComplete sw = atomically . writeTVar (swOnComplete sw)

data SpawnSpec = SpawnSpec
  { ssTheorem :: Text
  , ssDescription :: Text
  , ssArgv :: [Text]
  , ssCwd :: FilePath
  , ssEnv :: Env
  , ssWorkflowKind :: Text
  , ssWorkflowCommand :: Text
  , ssProjectName :: Text
  , ssProjectRoot :: FilePath
  , ssBackendName :: Text
  , ssProver :: ProverKind
  -- ^ the project's prover, whose hole words label the progress
  }

sessionId :: IO Text
sessionId = do
  bytes <- (withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 4)) `catch` \(_ :: IOException) -> pure "\0\0\0\0"
  pure ("af_" <> T.pack (concatMap (\b -> let h = showHex b "" in if length h == 1 then '0' : h else h) (BS.unpack bytes)))

newTask :: Swarm -> SpawnSpec -> IO Task
newTask sw s = do
  sid <- sessionId
  now <- nowSeconds
  n <- atomically $ do
    c <- (+ 1) <$> readTVar (swCounter sw)
    writeTVar (swCounter sw) c
    pure c
  let tid = T.pack ("af-" <> pad (show n))
      pad x = replicate (3 - length x) '0' <> x
      info = TaskInfo tid (ssDescription s) (ssTheorem s) (ssWorkflowKind s) (ssWorkflowCommand s)
               (ssProjectName s) (ssProjectRoot s) (ssCwd s) (ssBackendName s) sid
  t <- Task info
         <$> newTVarIO (St Running (initialProgress {pProgress = "Launching Claude Code...", pLean = Just "starting"}) (Just now) Nothing)
         <*> newTVarIO Nothing <*> newTVarIO Nothing <*> newTVarIO Nothing
         <*> newTVarIO BS.empty <*> newTVarIO Nothing <*> newEmptyTMVarIO <*> newEmptyTMVarIO
  atomically (modifyTVar' (swTasks sw) (Map.insert n t))
  pure t

modifyProgress :: Task -> (Progress -> Progress) -> IO ()
modifyProgress t f = atomically (modifyTVar' (tState t) (\st -> st {stProgress = f (stProgress st)}))

-- | Close out a task once its process is gone, as upstream does.
finish :: Swarm -> Task -> Either Text ExitCode -> Text -> IO ()
finish sw t outcome stderrTail = do
  now <- nowSeconds
  atomically $ do
    st <- readTVar (tState t)
    let p = stProgress st
        st' = case outcome of
          Left e | stStatus st /= Cancelled -> st {stStatus = Failed, stProgress = p {pError = Just e, pProgress = if e == "claude CLI not found" then "Launch failed" else "Unexpected error"}}
          _ | stStatus st == Cancelled -> st {stProgress = p {pProgress = "Cancelled"}}
          Right ExitSuccess ->
            st { stStatus = if stStatus st == Running then Complete else stStatus st
               , stProgress = if isJust (pResult p) then p else p {pProgress = "Session ended successfully"} }
          Right (ExitFailure c) ->
            st { stStatus = Failed
               , stProgress = p { pError = Just ("exit " <> T.pack (show c) <> (if T.null stderrTail then "" else ": " <> stderrTail))
                                , pProgress = "Claude Code exited with error" } }
          Left _ -> st
    writeTVar (tState t) st' {stEnd = maybe (Just now) Just (stEnd st')}
    writeTVar (tProc t) Nothing
    writeTVar (tMasterW t) Nothing
    writeTVar (tMasterFd t) Nothing
    case outcome of
      Right ec -> void (tryPutTMVar (tExit t) ec)
      Left _ -> pure ()
    void (tryPutTMVar (tDone t) ())
  cb <- readTVarIO (swOnComplete sw)
  v <- taskView t
  forM_ cb $ \f -> f v `catch` \(_ :: SomeException) -> pure ()

launchError :: SomeException -> Text
launchError e =
  let s = T.pack (show e)
   in if "does not exist" `T.isInfixOf` s || "No such file" `T.isInfixOf` s then "claude CLI not found" else T.take 300 s

decodeLine :: BS.ByteString -> Text
decodeLine = T.dropWhileEnd (== '\r') . TE.decodeUtf8With TEE.lenientDecode

-- | Spawn a session behind a PTY. It runs whether or not anyone attaches.
spawnInteractive :: Swarm -> SpawnSpec -> IO Task
spawnInteractive sw s = do
  t <- newTask sw s
  (argv, env) <- normalizeClaudeLaunch (swIsRoot sw) (swTrustHome sw) (ssArgv s) (ssEnv s) (Just (ssCwd s))
  r <- try $ do
    (master, slave) <- openPseudoTerminal
    size <- getWinSize stdOutput
    let (rows, cols) = maybe (24, 80) id size
    setWinSize slave rows cols
    slaveH <- fdToHandle slave
    (exe, args) <- case map T.unpack argv of
      e : as -> pure (e, as)
      [] -> ioError (userError "empty command line")
    (_, _, _, ph) <- createProcess (proc exe args)
      { cwd = Just (ssCwd s), env = Just (M.toList env)
      , std_in = UseHandle slaveH, std_out = UseHandle slaveH, std_err = UseHandle slaveH
      , close_fds = True, create_group = True }
    masterR <- fdToHandle master
    masterW <- dup master >>= fdToHandle
    forM_ [masterR, masterW] $ \h -> hSetBinaryMode h True >> hSetBuffering h NoBuffering
    pure (ph, master, masterR, masterW)
  case r of
    Left e -> t <$ finish sw t (Left (launchError e)) ""
    Right (ph, master, masterR, masterW) -> do
      atomically $ do
        writeTVar (tProc t) (Just ph)
        writeTVar (tMasterW t) (Just masterW)
        writeTVar (tMasterFd t) (Just master)
      modifyProgress t (\p -> p {pProgress = "Claude Code running"})
      readerDone <- newEmptyTMVarIO
      -- The reader owns its handle and closes it itself: closing a handle
      -- another thread is blocked reading would wait on that read forever.
      reader <- forkIO $
        (readLoop t masterR BS.empty `catch` \(_ :: SomeException) -> pure ())
          `finally` ((hClose masterR `catch` \(_ :: SomeException) -> pure ()) >> atomically (void (tryPutTMVar readerDone ())))
      _ <- forkIO $ do
        ec <- waitForProcess ph
        -- Give the reader a second to drain what the session printed last;
        -- a grandchild still holding the PTY open would keep it waiting.
        drained <- timeout 1000000 (atomically (readTMVar readerDone))
        when (drained == Nothing) (killThread reader)
        hClose masterW `catch` \(_ :: SomeException) -> pure ()
        finish sw t (Right ec) ""
      pure t
  where
    readLoop t h buf = do
      chunk <- BS.hGetSome h 4096
      unless (BS.null chunk) $ do
        sink <- atomically $ do
          modifyTVar' (tRecent t) (\old -> let b = old <> chunk in BS.drop (BS.length b - recentLimit) b)
          readTVar (tSink t)
        forM_ sink $ \q -> atomically (writeTQueue q chunk)
        let (complete, rest) = splitLines (buf <> chunk)
        forM_ complete $ \l -> modifyProgress t (parseStreamEventFor (ssProver s) (decodeLine l))
        readLoop t h rest

splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines b = case BC.elemIndex '\n' b of
  Nothing -> ([], b)
  Just i -> let (ls, rest) = splitLines (BS.drop (i + 1) b) in (BS.take i b : ls, rest)

-- | Spawn a session on pipes, for @claude -p --output-format stream-json@.
spawnBackground :: Swarm -> SpawnSpec -> IO Task
spawnBackground sw s = do
  t <- newTask sw s
  (argv, env) <- normalizeClaudeLaunch (swIsRoot sw) (swTrustHome sw) (ssArgv s) (ssEnv s) (Just (ssCwd s))
  r <- try $ do
    (exe, args) <- case map T.unpack argv of
      e : as -> pure (e, as)
      [] -> ioError (userError "empty command line")
    (_, Just out, Just err, ph) <- createProcess (proc exe args)
      { cwd = Just (ssCwd s), env = Just (M.toList env), std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe
      , create_group = True }
    pure (out, err, ph)
  case r of
    Left e -> t <$ finish sw t (Left (launchError e)) ""
    Right (out, err, ph) -> do
      atomically (writeTVar (tProc t) (Just ph))
      modifyProgress t (\p -> p {pProgress = "Claude Code running"})
      errRef <- newIORef BS.empty
      _ <- forkIO $ (BS.hGetContents err >>= writeIORef errRef) `catch` \(_ :: SomeException) -> pure ()
      _ <- forkIO $ do
        let loop = do
              eof <- hIsEOF out
              unless eof $ do
                line <- decodeLine <$> BS.hGetLine out
                unless (T.null line) $ modifyProgress t (parseStreamEventFor (ssProver s) line)
                st <- readTVarIO (tState t)
                if stStatus st == Cancelled then terminateGroup ph else loop
        loop `catch` \(_ :: SomeException) -> pure ()
        ec <- waitForProcess ph
        tailBytes <- readIORef errRef
        let errText = TE.decodeUtf8With TEE.lenientDecode tailBytes
        finish sw t (Right ec) (T.takeEnd 300 errText)
      pure t

-- | @cancel@: mark the task cancelled and stop its process. False when it
-- is unknown or already over.
cancelTask :: Swarm -> Text -> IO Bool
cancelTask sw tid = do
  now <- nowSeconds
  found <- getTask sw tid
  case found of
    Nothing -> pure False
    Just t -> do
      ok <- atomically $ do
        st <- readTVar (tState t)
        if stStatus st `elem` [Complete, Failed, Cancelled]
          then pure False
          else True <$ writeTVar (tState t) st {stStatus = Cancelled, stEnd = Just now}
      when ok $ readTVarIO (tProc t) >>= mapM_ terminateGroup
      pure ok

-- | SIGTERM to the session's whole process group, so a shell's children go
-- with it. (Upstream terminates only the direct child.)
terminateGroup :: ProcessHandle -> IO ()
terminateGroup ph = do
  pid <- getPid ph
  case pid of
    Just p -> signalProcessGroup sigTERM p `catch` \(_ :: SomeException) -> terminateProcess ph
    Nothing -> pure ()

listTasks :: Swarm -> IO [Task]
listTasks sw = Map.elems <$> readTVarIO (swTasks sw)

getTask :: Swarm -> Text -> IO (Maybe Task)
getTask sw tid = do
  ts <- listTasks sw
  pure (case [t | t <- ts, tiTaskId (taskInfo t) == tid] of
    (t : _) -> Just t
    [] -> Nothing)

taskView :: Task -> IO TaskView
taskView t = atomically $ do
  st <- readTVar (tState t)
  pty <- isJust <$> readTVar (tMasterW t)
  pure (TaskView (taskInfo t) (stStatus st) (stProgress st) (stStart st) (stEnd st) pty)

allViews :: Swarm -> IO [TaskView]
allViews sw = listTasks sw >>= mapM taskView

-- | The newest task with the given status, optionally one with a PTY.
latestTask :: Swarm -> Maybe Status -> Bool -> IO (Maybe Task)
latestTask sw status requirePty = do
  ts <- reverse <$> listTasks sw
  vs <- forM ts (\t -> (,) t <$> taskView t)
  pure (case [t | (t, v) <- vs, maybe True (== tvStatus v) status, not requirePty || tvHasPty v] of
    (t : _) -> Just t
    [] -> Nothing)

-- | Block until a task is over.
waitTask :: Task -> IO TaskView
waitTask t = atomically (readTMVar (tDone t)) >> taskView t

-- | Where an attach reads keys and writes the session's screen.
data AttachIO = AttachIO
  { aiInput :: Handle
  , aiOutput :: BS.ByteString -> IO ()
  , aiTerminal :: Maybe Fd
  -- ^ The real terminal, put in raw mode and followed for its size.
  , aiDetachKey :: Maybe Word8
  -- ^ Upstream detaches on Ctrl-] (0x1d).
  }

terminalAttachIO :: Maybe Word8 -> IO AttachIO
terminalAttachIO detach = do
  -- ByteString reads and writes skip the handles' text encoding, so the
  -- handles stay in text mode: the shell prints UTF-8 through them again
  -- after a detach.
  hSetBuffering stdin NoBuffering
  pure (AttachIO stdin (\b -> BS.hPut stdout b >> hFlush stdout) (Just stdInput) detach)

-- | Attach to a running interactive task until detach or until it ends.
-- Returns the exit code, or -1 on detach.
attachTask :: Task -> AttachIO -> IO (Either Text Int)
attachTask t io = do
  st <- readTVarIO (tState t)
  masterW <- readTVarIO (tMasterW t)
  masterFd <- readTVarIO (tMasterFd t)
  ph <- readTVarIO (tProc t)
  case (masterW, masterFd, ph) of
    (Just w, Just mfd, Just p) | stStatus st == Running -> Right <$> run w mfd p
    _ -> pure (Left "Task is not running or has no PTY")
  where
    tid = tiTaskId (taskInfo t)
    winch p = getPid p >>= mapM_ (\pid -> signalProcess windowChange pid `catch` \(_ :: SomeException) -> pure ())
    sync mfd p = forM_ (aiTerminal io) $ \fd -> getWinSize fd >>= mapM_ (\(r, c) -> setWinSize mfd r c >> winch p)
    run w mfd p = do
      q <- newTQueueIO
      recent <- atomically $ do
        writeTVar (tSink t) (Just q)
        readTVar (tRecent t)
      done <- newEmptyTMVarIO
      trusted <- newIORef (False, BS.empty)
      let body = do
            aiOutput io ("\ESC]0;Fisher \226\128\186 " <> TE.encodeUtf8 tid <> "  \226\148\130  Ctrl-] detach\a")
            sync mfd p
            aiOutput io recent
            prev <- maybe (pure Nothing) (const (Just <$> installHandler windowChange (Catch (sync mfd p)) Nothing)) (aiTerminal io)
            out <- forkIO . forever' $ do
              chunk <- atomically (readTQueue q)
              aiOutput io chunk
              (handled, seen) <- readIORef trusted
              unless handled $ do
                let seen' = seen <> chunk
                    low = T.toLower (TE.decodeUtf8With TEE.lenientDecode seen')
                if "yes, i trust" `T.isInfixOf` low || "trust this folder" `T.isInfixOf` low
                  then BS.hPut w "\r" >> writeIORef trusted (True, BS.empty)
                  else writeIORef trusted (BS.length seen' > 8192, if BS.length seen' > 8192 then BS.empty else seen')
            inp <- forkIO $ do
              -- Wait in short slices, so killThread lands promptly even when
              -- the input is a terminal, whose read() under the threaded RTS
              -- is a blocking foreign call that cannot be interrupted.
              let loop = do
                    ready <- hWaitForInput (aiInput io) 100 `catch` \(e :: IOException) ->
                      if isEOFError e then pure True else ioError e
                    if not ready then loop else do
                     chunk <- BS.hGetSome (aiInput io) 1024
                     if BS.null chunk
                       then atomically (void (tryPutTMVar done ()))
                       else case aiDetachKey io of
                         Just k | BS.elem k chunk -> atomically (void (tryPutTMVar done ()))
                         _ -> BS.hPut w chunk >> loop
              loop `catch` \(_ :: SomeException) -> atomically (void (tryPutTMVar done ()))
            atomically (readTMVar done `orElse` readTMVar (tDone t))
            killThread inp
            -- Let what the session printed last reach the screen.
            threadDelay 50000
            killThread out
            forM_ prev $ \h -> installHandler windowChange h Nothing
          cleanup = do
            atomically (writeTVar (tSink t) Nothing)
            aiOutput io "\ESC]0;Fisher\a" `catch` \(_ :: SomeException) -> pure ()
      maybe id withRawMode (aiTerminal io) (body `finally` cleanup)
      ec <- atomically (tryReadTMVar (tExit t))
      pure $ case ec of
        Just ExitSuccess -> 0
        Just (ExitFailure c) -> c
        Nothing -> -1
    forever' a = a >> forever' a

