-- | The local backend: each command through @bash -lic@ inside output
-- fences, or (when persistent) through one long-lived @bash -l@ whose
-- working directory and variables carry over, using upstream's temp-file
-- protocol byte for byte.
module Fisher.Tools.Sandbox.Local
  ( SandboxEnv (..)
  , defaultSandboxEnv
  , newLocalSandbox
  , findBash
  , Persistent (..)
  , PersistentHooks (..)
  , newPersistent
  , executePersistent
  , cleanupPersistent
  ) where

import Control.Concurrent
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import GHC.Clock (getMonotonicTime)
import System.Directory (doesFileExist, findExecutable, getCurrentDirectory, listDirectory, removeFile)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode)
import System.FilePath (takeDirectory, takeFileName)
import System.IO
import GHC.IO.Encoding (setFileSystemEncoding)
import System.Posix.Signals (sigKILL)
import System.Process
import System.Random (randomRIO)
import System.Timeout (timeout)
import Numeric (showHex)

import Fisher.Tools.Sandbox
import Fisher.Tools.Sandbox.Common
import Fisher.Tools.Sandbox.Process

-- | What every backend needs from the outside: the interrupt flag, the
-- sudo password (upstream: @SUDO_PASSWORD@, a cached answer, or a prompt
-- when interactive), and this process's environment.
data SandboxEnv = SandboxEnv
  { seInterrupt :: Interrupt
  , seSudoPassword :: Text -> IO Text
  , seEnviron :: IO [(Text, Text)]
  }

-- | Sudo from @SUDO_PASSWORD@ only; the environment of this process.
defaultSandboxEnv :: IO SandboxEnv
defaultSandboxEnv = do
  -- children get their argv and environment as UTF-8, whatever the locale
  -- (Python does the same in UTF-8 mode); undecodable bytes round-trip
  setFileSystemEncoding =<< mkTextEncoding "UTF-8//ROUNDTRIP"
  intr <- newInterrupt
  pure
    SandboxEnv
      { seInterrupt = intr
      , seSudoPassword = \_ -> maybe "" T.pack <$> lookupEnv "SUDO_PASSWORD"
      , seEnviron = map (\(k, v) -> (T.pack k, T.pack v)) <$> getEnvironment
      }

-- | @_find_bash()@.
findBash :: IO FilePath
findBash = do
  w <- findExecutable "bash"
  case w of
    Just p -> pure p
    Nothing -> do
      a <- doesFileExist "/usr/bin/bash"
      b <- doesFileExist "/bin/bash"
      sh <- lookupEnv "SHELL"
      pure $ if a then "/usr/bin/bash" else if b then "/bin/bash" else maybe "/bin/sh" id sh

-- | @LocalEnvironment(cwd, timeout, persistent)@.
newLocalSandbox :: SandboxEnv -> Text -> Int -> Bool -> IO Sandbox
newLocalSandbox se cwd0 defTimeout persistent = do
  cwd <- if T.null cwd0 then T.pack <$> getCurrentDirectory else pure cwd0
  cwdRef <- newIORef cwd
  if persistent
    then do
      p <- newPersistent se (localHooks se) cwdRef
      pure
        Sandbox
          { sbKind = "local"
          , sbExecute = \req -> executePersistent p defTimeout (oneshot cwdRef) req
          , sbCwd = readIORef cwdRef
          , sbDefaultTimeout = defTimeout
          , sbCleanup = cleanupPersistent p
          }
    else
      pure
        Sandbox
          { sbKind = "local"
          , sbExecute = oneshot cwdRef
          , sbCwd = readIORef cwdRef
          , sbDefaultTimeout = defTimeout
          , sbCleanup = pure ()
          }
  where
    oneshot cwdRef req = do
      cwd <- readIORef cwdRef
      here <- getCurrentDirectory
      pw <- seSudoPassword se (erCommand req)
      let workDir = firstNonEmpty [erCwd req, cwd, T.pack here]
          eff = effTimeout defTimeout (erTimeout req)
          (cmd, sudoIn) = transformSudo pw (erCommand req)
      bash <- findBash
      environ <- seEnviron se
      let runEnv = makeRunEnv environ []
      out <-
        runPolled (seInterrupt se) StopGroup (fromIntegral eff) $
          Spawn [bash, "-lic", T.unpack (fencedCommand cmd)] (Just (T.unpack workDir)) (Just [(T.unpack k, T.unpack v) | (k, v) <- runEnv]) (mergeStdin sudoIn (erStdin req))
      pure $ case out of
        Finished rc raw -> ExecResult (dropSetpgidNoise (extractFencedOutput raw)) rc
        TimedOut _ -> timeoutResult eff
        Interrupted raw -> ExecResult (raw <> "\n[Command interrupted \8212 user sent a new message]") 130

-- | macOS's bash, interactive in a fresh session, sometimes reports
-- "bash: child setpgid (N to N): Operation not permitted" for a child that
-- has already started; the line is noise, not the command's output.
dropSetpgidNoise :: Text -> Text
dropSetpgidNoise t
  | "bash: child setpgid (" `T.isInfixOf` t =
      T.intercalate "\n" [l | l <- T.splitOn "\n" t, not ("bash: child setpgid (" `T.isPrefixOf` l && "Operation not permitted" `T.isInfixOf` l)]
  | otherwise = t

firstNonEmpty :: [Text] -> Text
firstNonEmpty xs = case filter (not . T.null) xs of
  (x : _) -> x
  [] -> ""

effTimeout :: Int -> Maybe Int -> Int
effTimeout def = \case
  Just t | t /= 0 -> t
  _ -> def

-- ------------------------------------------------------------- persistent

-- | The backend-specific parts of upstream's @PersistentShellMixin@.
data PersistentHooks = PersistentHooks
  { phPrefixBase :: Text
  -- ^ "/tmp/fisher-local" (the session id is appended)
  , phSpawn :: IO (Handle, Handle, ProcessHandle)
  -- ^ the shell: its stdin and stdout (stderr discarded)
  , phReadFiles :: Text -> [Text] -> IO [Text]
  -- ^ session id, paths
  , phKillChildren :: Int -> IO ()
  , phCleanupFiles :: Text -> IO ()
  -- ^ the prefix
  , phPollInterval :: Double
  }

data Persistent = Persistent
  { pHooks :: PersistentHooks
  , pEnv :: SandboxEnv
  , pCwd :: IORef Text
  , pState :: MVar PState
  , pLock :: MVar ()
  }

data PState = PState
  { psSession :: Text
  , psProc :: Maybe (Handle, ProcessHandle)
  , psAlive :: IORef Bool
  , psPid :: Maybe Int
  }

prefixOf :: Persistent -> PState -> Text
prefixOf p st = phPrefixBase (pHooks p) <> "-" <> psSession st

hex12 :: IO Text
hex12 = T.pack . concat <$> mapM (const (flip showHex "" <$> randomRIO (0, 15 :: Int))) [1 .. 12 :: Int]

hex8 :: IO Text
hex8 = T.take 8 <$> hex12

newPersistent :: SandboxEnv -> PersistentHooks -> IORef Text -> IO Persistent
newPersistent se hooks cwdRef = do
  alive <- newIORef False
  st <- newMVar (PState "" Nothing alive Nothing)
  lock <- newMVar ()
  let p = Persistent hooks se cwdRef st lock
  initShell p
  pure p

-- | @_init_persistent_shell@.
initShell :: Persistent -> IO ()
initShell p = do
  sid <- hex12
  (hin, hout, ph) <- phSpawn (pHooks p)
  alive <- newIORef True
  _ <- forkIO $ do
    let drain = do
          c <- BS.hGetSome hout 4096 `catch` \(_ :: IOException) -> pure BS.empty
          unless (BS.null c) drain
    drain
    writeIORef alive False
  let st0 = PState sid (Just (hin, ph)) alive Nothing
      pre = prefixOf p st0
  sendTo st0 $
    "export TERM=${TERM:-dumb}\ntouch " <> pre <> "-stdout " <> pre <> "-stderr " <> pre <> "-status " <> pre <> "-cwd " <> pre <> "-pid\n"
      <> "echo $$ > " <> pre <> "-pid\npwd > " <> pre <> "-cwd\n"
  start <- getMonotonicTime
  let waitPid = do
        [raw] <- phReadFiles (pHooks p) sid [pre <> "-pid"]
        let s = T.strip raw
        if not (T.null s) && T.all isDigit s
          then pure (Just (read (T.unpack s)))
          else do
            now <- getMonotonicTime
            if now - start > 3.0 then pure Nothing else threadDelay 50000 >> waitPid
  pid <- waitPid
  [cwdRaw] <- phReadFiles (pHooks p) sid [pre <> "-cwd"]
  let c = T.strip cwdRaw
  unless (T.null c) (writeIORef (pCwd p) c)
  void (swapMVar (pState p) st0 {psPid = pid})

sendTo :: PState -> Text -> IO ()
sendTo st txt = do
  alive <- readIORef (psAlive st)
  when alive $ case psProc st of
    Just (hin, _) -> do
      r <- try (BS.hPut hin (TE.encodeUtf8 txt) >> hFlush hin) :: IO (Either SomeException ())
      either (const (writeIORef (psAlive st) False)) pure r
    Nothing -> pure ()

-- | @_execute_persistent@: commands that need stdin (or a sudo password)
-- fall back to a oneshot run, as upstream does.
executePersistent :: Persistent -> Int -> (ExecRequest -> IO ExecResult) -> ExecRequest -> IO ExecResult
executePersistent p defTimeout oneshot req = do
  st0 <- readMVar (pState p)
  alive0 <- readIORef (psAlive st0)
  unless alive0 (initShell p)
  pw <- seSudoPassword (pEnv p) (erCommand req)
  let (cmd, sudoIn) = transformSudo pw (erCommand req)
      nonEmpty = maybe False (not . T.null)
  if nonEmpty (erStdin req) || nonEmpty sudoIn
    then oneshot req
    else withMVar (pLock p) $ \_ -> do
      st <- readMVar (pState p)
      cwd <- readIORef (pCwd p)
      let eff = effTimeout defTimeout (erTimeout req)
      runLocked p st cmd (firstNonEmpty [erCwd req, cwd]) eff

runLocked :: Persistent -> PState -> Text -> Text -> Int -> IO ExecResult
runLocked p st cmd workDir eff = do
  cmdId <- hex8
  let pre = prefixOf p st
      hooks = pHooks p
      escaped = T.replace "'" "'\\''" cmd
  sendTo st (": > " <> pre <> "-stdout\n: > " <> pre <> "-stderr\n: > " <> pre <> "-status\n")
  sendTo st $
    "cd " <> shQuote workDir <> "\neval '" <> escaped <> "' < /dev/null > " <> pre <> "-stdout 2> " <> pre <> "-stderr\n__EC=$?\npwd > "
      <> pre <> "-cwd\necho " <> cmdId <> ":$__EC > " <> pre <> "-status\n"
  start <- getMonotonicTime
  let killKids = maybe (pure ()) (phKillChildren hooks) (psPid st)
      poll = do
        intr <- isInterrupted (seInterrupt (pEnv p))
        now <- getMonotonicTime
        alive <- readIORef (psAlive st)
        if intr
          then do
            killKids
            (o, _, _) <- readOut
            pure (ExecResult (o <> "\n[Command interrupted]") 130)
          else
            if now - start > fromIntegral eff
              then do
                killKids
                (o, _, _) <- readOut
                pure $
                  if T.null o
                    then timeoutResult eff
                    else ExecResult (o <> "\n[Command timed out after " <> T.pack (show eff) <> "s]") 124
              else
                if not alive
                  then pure (ExecResult "Persistent shell died during execution" 1)
                  else do
                    [status] <- phReadFiles hooks (psSession st) [pre <> "-status"]
                    if (cmdId <> ":") `T.isPrefixOf` T.strip status
                      then do
                        (o, code, newCwd) <- readOut
                        unless (T.null newCwd) (writeIORef (pCwd p) newCwd)
                        pure (ExecResult o code)
                      else threadDelay (round (phPollInterval hooks * 1e6)) >> poll
      readOut = do
        [so, se_, status, cwd] <- phReadFiles hooks (psSession st) [pre <> "-stdout", pre <> "-stderr", pre <> "-status", pre <> "-cwd"]
        let merged = T.intercalate "\n" [T.dropWhileEnd (== '\n') part | part <- [so, se_], not (T.null (T.strip part))]
            code = case T.breakOn ":" (T.strip status) of
              (_, rest) | not (T.null rest), [(n, "")] <- reads (T.unpack (T.strip (T.drop 1 rest))) -> n
              _ -> 1
        pure (merged, code, T.strip cwd)
  poll

-- | @shlex.quote@.
shQuote :: Text -> Text
shQuote s
  | T.null s = "''"
  | T.all safe s = s
  | otherwise = "'" <> T.replace "'" "'\"'\"'" s <> "'"
  where
    safe c = c `elem` ("@%+=:,./-_" :: String) || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | @_cleanup_persistent_shell@.
cleanupPersistent :: Persistent -> IO ()
cleanupPersistent p = do
  st <- readMVar (pState p)
  case psProc st of
    Nothing -> pure ()
    Just (hin, ph) -> do
      unless (T.null (psSession st)) (phCleanupFiles (pHooks p) (prefixOf p st))
      _ <- try (hClose hin) :: IO (Either SomeException ())
      terminateProcess ph
      r <- timeout 3000000 (waitForProcess ph)
      when (r == Nothing) (killGroup ph sigKILL)
      writeIORef (psAlive st) False
      void (swapMVar (pState p) st {psProc = Nothing})

localHooks :: SandboxEnv -> PersistentHooks
localHooks se =
  PersistentHooks
    { phPrefixBase = "/tmp/fisher-local"
    , phSpawn = do
        bash <- findBash
        environ <- seEnviron se
        let runEnv = makeRunEnv environ []
        (Just hin, Just hout, _, ph) <-
          createProcess
            (proc bash ["-l"])
              { std_in = CreatePipe
              , std_out = CreatePipe
              , std_err = NoStream
              , env = Just [(T.unpack k, T.unpack v) | (k, v) <- runEnv]
              , new_session = True
              }
        pure (hin, hout, ph)
    , phReadFiles = \_ paths -> mapM readOr paths
    , phKillChildren = \pid -> void (try (readProcessWithExitCode "pkill" ["-P", show pid] "") :: IO (Either SomeException (ExitCode, String, String)))
    , phCleanupFiles = \pre -> do
        let dir = takeDirectory (T.unpack pre)
            base = takeFileName (T.unpack pre) <> "-"
        names <- either (const []) id <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
        forM_ [n | n <- names, take (length base) n == base] $ \n -> removeFile (dir <> "/" <> n) `catch` \(_ :: IOException) -> pure ()
    , phPollInterval = 0.01
    }
  where
    readOr path = do
      r <- try (BS.readFile (T.unpack path)) :: IO (Either IOException BS.ByteString)
      pure (either (const "") (TE.decodeUtf8With TEE.lenientDecode) r)

