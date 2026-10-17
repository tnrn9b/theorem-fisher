-- | The docker and ssh backends, with their command lines built by plain
-- functions so the goldens can check them without a daemon or a host.
module Fisher.Tools.Sandbox.Remote
  ( -- * Docker
    DockerOpts (..)
  , defaultDockerOpts
  , securityArgs
  , DockerHost (..)
  , systemDockerHost
  , dockerRunArgs
  , dockerStartArgv
  , dockerExecArgv
  , findDocker
  , ensureDockerAvailable
  , newDockerSandbox
    -- * SSH
  , SshOpts (..)
  , sshBaseArgv
  , sshControlSocket
  , newSshSandbox
  ) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (forM, void, when)
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, executable, findExecutable, getPermissions, getTemporaryDirectory, makeAbsolute, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process
import System.Random (randomRIO)
import System.Timeout (timeout)
import Numeric (showHex)

import Fisher.Py.Value (PyVal (..), pyRepr)
import Fisher.Tools.Sandbox
import Fisher.Tools.Sandbox.Common
import Fisher.Tools.Sandbox.Local (PersistentHooks (..), SandboxEnv (..), cleanupPersistent, executePersistent, newPersistent)
import Fisher.Tools.Sandbox.Process

-- ------------------------------------------------------------------ docker

data DockerOpts = DockerOpts
  { doImage :: Text
  , doCwd :: Text
  , doTimeout :: Int
  , doCpu :: Double
  , doMemory :: Integer
  , doDisk :: Integer
  , doPersistent :: Bool
  , doTaskId :: Text
  , doVolumes :: PyVal
  -- ^ as configured (upstream checks it is a list of strings)
  , doHostCwd :: Maybe Text
  , doAutoMountCwd :: Bool
  }

defaultDockerOpts :: DockerOpts
defaultDockerOpts = DockerOpts "nikolaik/python-nodejs:python3.11-nodejs20" "/root" 60 0 0 0 False "default" (PList []) Nothing False

securityArgs :: [Text]
securityArgs =
  [ "--cap-drop", "ALL", "--cap-add", "DAC_OVERRIDE", "--cap-add", "CHOWN", "--cap-add", "FOWNER"
  , "--security-opt", "no-new-privileges", "--pids-limit", "256"
  , "--tmpfs", "/tmp:rw,nosuid,size=512m", "--tmpfs", "/var/tmp:rw,noexec,nosuid,size=256m"
  , "--tmpfs", "/run:rw,noexec,nosuid,size=64m"
  ]

-- | What building the run arguments asks of the machine.
data DockerHost = DockerHost
  { dhPlatform :: Text
  , dhRun :: [Text] -> IO (Int, Text)
  -- ^ run a command, its exit code and stdout
  , dhSandboxDir :: IO FilePath
  -- ^ @get_sandbox_dir()@ (created)
  , dhIsDir :: Text -> IO Bool
  , dhMkdirs :: FilePath -> IO ()
  , dhWarn :: Text -> IO ()
  }

systemDockerHost :: Text -> FilePath -> DockerHost
systemDockerHost platform sandbox =
  DockerHost
    { dhPlatform = platform
    , dhRun = \argv -> do
        r <- try (readProcessWithExitCode (T.unpack (head argv)) (map T.unpack (tail argv)) "") :: IO (Either SomeException (ExitCode, String, String))
        pure $ case r of
          Right (code, o, _) -> (case code of ExitSuccess -> 0; ExitFailure n -> n, T.pack o)
          Left _ -> (-1, "")
    , dhSandboxDir = createDirectoryIfMissing True sandbox >> pure sandbox
    , dhIsDir = doesDirectoryExist . T.unpack
    , dhMkdirs = createDirectoryIfMissing True
    , dhWarn = \t -> hPutStrLn stderr (T.unpack t)
    }

-- | The @run_args@ upstream hands mini-swe-agent: security flags, writable
-- mounts, resource limits, then user volumes.
dockerRunArgs :: DockerHost -> Text -> DockerOpts -> IO [Text]
dockerRunArgs host exe o = do
  storage <-
    if doDisk o > 0 && dhPlatform host /= "darwin"
      then do
        ok <- storageOptSupported host exe
        when (not ok) (dhWarn host "Docker storage driver does not support per-container disk limits (requires overlay2 on XFS with pquota). Container will run without disk quota.")
        pure [x | ok, x <- ["--storage-opt", "size=" <> tshow (doDisk o) <> "m"]]
      else pure []
  let resource =
        ["--cpus" | doCpu o > 0] <> [pyFloatStr (doCpu o) | doCpu o > 0]
          <> concat [["--memory", tshow (doMemory o) <> "m"] | doMemory o > 0]
          <> storage
  vols <- case doVolumes o of
    PList xs -> fmap catMaybes . forM xs $ \case
      PStr v0 -> do
        let v = T.strip v0
        if T.null v
          then pure Nothing
          else
            if ":" `T.isInfixOf` v
              then pure (Just v)
              else dhWarn host ("Docker volume '" <> v <> "' missing colon, skipping") >> pure Nothing
      other -> dhWarn host ("Docker volume entry is not a string: " <> pyRepr other) >> pure Nothing
    PNull -> pure []
    other -> dhWarn host ("docker_volumes config is not a list: " <> pyRepr other) >> pure []
  let explicitWs = any (":/workspace" `T.isInfixOf`) vols
  hostAbs <- case doHostCwd o of
    Just h | not (T.null h) -> T.pack <$> makeAbsolute (T.unpack h)
    _ -> pure ""
  hostIsDir <- if T.null hostAbs then pure False else dhIsDir host hostAbs
  let bindHost = doAutoMountCwd o && not (T.null hostAbs) && hostIsDir && not explicitWs
  writable <-
    if doPersistent o
      then do
        sb <- dhSandboxDir host
        let base = sb <> "/docker/" <> T.unpack (doTaskId o)
            home = base <> "/home"
            ws = base <> "/workspace"
        dhMkdirs host home
        wsArgs <-
          if not bindHost && not explicitWs
            then dhMkdirs host ws >> pure ["-v", T.pack ws <> ":/workspace"]
            else pure []
        pure (["-v", T.pack home <> ":/root"] <> wsArgs)
      else
        pure $
          (if not bindHost && not explicitWs then ["--tmpfs", "/workspace:rw,exec,size=10g"] else [])
            <> ["--tmpfs", "/home:rw,exec,size=1g", "--tmpfs", "/root:rw,exec,size=1g"]
  let volArgs = (if bindHost then ["-v", hostAbs <> ":/workspace"] else []) <> concat [["-v", v] | v <- vols]
  pure (securityArgs <> writable <> resource <> volArgs)
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show

-- | Python's @str(float)@ for the cpu count (1.0, 2.5, ...).
pyFloatStr :: Double -> Text
pyFloatStr d
  | d == fromIntegral (round d :: Integer) && abs d < 1e16 = T.pack (show (round d :: Integer)) <> ".0"
  | otherwise = T.pack (show d)

storageOptSupported :: DockerHost -> Text -> IO Bool
storageOptSupported host exe = do
  (rc, drv) <- dhRun host [exe, "info", "--format", "{{.Driver}}"]
  if rc /= 0 || T.toLower (T.strip drv) /= "overlay2"
    then pure False
    else do
      (rc2, cid) <- dhRun host [exe, "create", "--storage-opt", "size=1m", "hello-world"]
      if rc2 == 0
        then dhRun host [exe, "rm", T.strip cid] >> pure True
        else pure False

-- | mini-swe-agent's container start: @docker run -d --name ... -w cwd ARGS image sleep 2h@.
dockerStartArgv :: Text -> Text -> Text -> [Text] -> Text -> [Text]
dockerStartArgv exe name cwd runArgs image = [exe, "run", "-d", "--name", name, "-w", cwd] <> runArgs <> [image, "sleep", "2h"]

-- | The @docker exec@ line for one command; a @~@ working directory turns
-- into a @cd@ inside the command.
dockerExecArgv :: Text -> Text -> Text -> Text -> Bool -> [Text]
dockerExecArgv exe cid workDir0 cmd0 hasStdin =
  let (cmd, workDir)
        | workDir0 == "~" || "~/" `T.isPrefixOf` workDir0 = ("cd " <> workDir0 <> " && " <> cmd0, "/")
        | otherwise = (cmd0, workDir0)
   in [exe, "exec"] <> ["-i" | hasStdin] <> ["-w", workDir, cid, "bash", "-lc", cmd]

-- | @find_docker()@.
findDocker :: IO (Maybe FilePath)
findDocker = do
  w <- findExecutable "docker"
  case w of
    Just p -> pure (Just p)
    Nothing -> firstExec ["/usr/local/bin/docker", "/opt/homebrew/bin/docker", "/Applications/Docker.app/Contents/Resources/bin/docker"]
  where
    firstExec [] = pure Nothing
    firstExec (p : ps) = do
      ok <- doesFileExist p
      x <- if ok then executable <$> getPermissions p else pure False
      if x then pure (Just p) else firstExec ps

-- | @_ensure_docker_available()@: the docker path, or upstream's error.
ensureDockerAvailable :: IO (Either Text FilePath)
ensureDockerAvailable = do
  md <- findDocker
  case md of
    Nothing -> pure (Left "Docker executable not found in PATH or known install locations. Install Docker and ensure the 'docker' command is available.")
    Just exe -> do
      r <- try (timeout 5000000 (readProcessWithExitCode exe ["version"] "")) :: IO (Either IOException (Maybe (ExitCode, String, String)))
      pure $ case r of
        Left _ -> Left "Docker executable could not be executed. Check your Docker installation."
        Right Nothing -> Left "Docker daemon is not responding. Ensure Docker is running and try again."
        Right (Just (ExitSuccess, _, _)) -> Right exe
        Right (Just _) -> Left "Docker command is available but 'docker version' failed. Check your Docker installation."

-- | @DockerEnvironment(...)@: checks docker, starts the container, and runs
-- each command with @docker exec@ (raw output, no fences).
newDockerSandbox :: SandboxEnv -> DockerHost -> DockerOpts -> IO (Either Text Sandbox)
newDockerSandbox se host o0 = do
  avail <- ensureDockerAvailable
  case avail of
    Left e -> pure (Left e)
    Right exe0 -> do
      let exe = T.pack exe0
          o = o0 {doCwd = if doCwd o0 == "~" then "/root" else doCwd o0}
      args <- dockerRunArgs host exe o
      suffix <- T.pack . concat <$> mapM (const (flip showHex "" <$> randomRIO (0, 15 :: Int))) [1 .. 8 :: Int]
      (rc, out) <- dhRun host (dockerStartArgv exe ("minisweagent-" <> suffix) (doCwd o) args (doImage o))
      if rc /= 0
        then pure (Left ("Command " <> pyRepr (PList (map PStr (dockerStartArgv exe ("minisweagent-" <> suffix) (doCwd o) args (doImage o)))) <> " returned non-zero exit status " <> T.pack (show rc) <> "."))
        else do
          let cid = T.strip out
          pure . Right $
            Sandbox
              { sbKind = "docker"
              , sbExecute = \req -> do
                  pw <- seSudoPassword se (erCommand req)
                  let (cmd, sudoIn) = transformSudo pw (erCommand req)
                      stdin = mergeStdin sudoIn (erStdin req)
                      workDir = if T.null (erCwd req) then doCwd o else erCwd req
                      eff = case erTimeout req of Just t | t /= 0 -> t; _ -> doTimeout o
                      argv = dockerExecArgv exe cid workDir cmd (stdin /= Nothing)
                  r <- try (runPolled (seInterrupt se) StopClient (fromIntegral eff) (Spawn (map T.unpack argv) Nothing Nothing (nonEmptyStdin stdin))) :: IO (Either SomeException Outcome)
                  pure $ case r of
                    Left e -> ExecResult ("Docker execution error: " <> T.pack (show e)) 1
                    Right (Finished code raw) -> ExecResult raw code
                    Right (TimedOut _) -> timeoutResult eff
                    Right (Interrupted raw) -> ExecResult (raw <> "\n[Command interrupted]") 130
              , sbCwd = pure (doCwd o)
              , sbDefaultTimeout = doTimeout o
              , sbCleanup =
                  void
                    ( spawnCommand ("(timeout 60 " <> exe0 <> " stop " <> T.unpack cid <> " || " <> exe0 <> " rm -f " <> T.unpack cid <> ") >/dev/null 2>&1 &")
                    )
              }
  where
    nonEmptyStdin = \case
      Just s | not (T.null s) -> Just s
      _ -> Nothing

-- --------------------------------------------------------------------- ssh

data SshOpts = SshOpts
  { soHost :: Text
  , soUser :: Text
  , soPort :: Integer
  , soKey :: Text
  , soCwd :: Text
  , soTimeout :: Int
  , soPersistent :: Bool
  }

-- | @<tmp>/gauss-ssh/<user>@<host>:<port>.sock@.
sshControlSocket :: FilePath -> SshOpts -> FilePath
sshControlSocket tmp o = tmp <> "/gauss-ssh/" <> T.unpack (soUser o <> "@" <> soHost o <> ":" <> T.pack (show (soPort o))) <> ".sock"

-- | @_build_ssh_command(extra)@ without the remote command.
sshBaseArgv :: FilePath -> SshOpts -> [Text] -> [Text]
sshBaseArgv sock o extra =
  [ "ssh", "-o", "ControlPath=" <> T.pack sock, "-o", "ControlMaster=auto", "-o", "ControlPersist=300"
  , "-o", "BatchMode=yes", "-o", "StrictHostKeyChecking=accept-new", "-o", "ConnectTimeout=10"
  ]
    <> (if soPort o /= 22 then ["-p", T.pack (show (soPort o))] else [])
    <> (if T.null (soKey o) then [] else ["-i", soKey o])
    <> extra
    <> [soUser o <> "@" <> soHost o]

newSshSandbox :: SandboxEnv -> SshOpts -> IO (Either Text Sandbox)
newSshSandbox se o = do
  tmp <- getTemporaryDirectory
  createDirectoryIfMissing True (tmp <> "/gauss-ssh")
  let sock = sshControlSocket (stripSlash tmp) o
      base = sshBaseArgv sock o []
      run1 :: [Text] -> Int -> IO (Maybe (ExitCode, String, String))
      run1 argv secs = either (const Nothing) id <$> (try (timeout (secs * 1000000) (readProcessWithExitCode (T.unpack (head argv)) (map T.unpack (tail argv)) "")) :: IO (Either IOException (Maybe (ExitCode, String, String))))
  haveSsh <- findExecutable "ssh"
  case haveSsh of
    Nothing -> pure (Left "SSH is not installed or not in PATH. Install OpenSSH client: apt install openssh-client")
    Just _ -> do
      est <- run1 (base <> ["echo 'SSH connection established'"]) 15
      case est of
        Nothing -> pure (Left ("SSH connection to " <> soUser o <> "@" <> soHost o <> " timed out"))
        Just (ExitFailure _, out, err) -> pure (Left ("SSH connection failed: " <> firstNonBlank (T.strip (T.pack err)) (T.strip (T.pack out))))
        Just (ExitSuccess, _, _) -> do
          cwdRef <- newIORef (soCwd o)
          let oneshot req = do
                cwd <- readIORef cwdRef
                pw <- seSudoPassword se (erCommand req)
                let workDir = if T.null (erCwd req) then cwd else erCwd req
                    (cmd, sudoIn) = transformSudo pw (erCommand req)
                    stdin = mergeStdin sudoIn (erStdin req)
                    eff = case erTimeout req of Just t | t /= 0 -> t; _ -> soTimeout o
                    argv = base <> ["cd " <> workDir <> " && " <> cmd]
                out <- runPolled (seInterrupt se) StopClient (fromIntegral eff) (Spawn (map T.unpack argv) Nothing Nothing (case stdin of Just s | not (T.null s) -> Just s; _ -> Nothing))
                pure $ case out of
                  Finished code raw -> ExecResult raw code
                  TimedOut _ -> timeoutResult eff
                  Interrupted raw -> ExecResult (raw <> "\n[Command interrupted]") 130
              closeMaster = do
                exists <- doesFileExist sock
                when exists $ do
                  _ <- run1 ["ssh", "-o", "ControlPath=" <> T.pack sock, "-O", "exit", soUser o <> "@" <> soHost o] 5
                  void (try (removeFile sock) :: IO (Either IOException ()))
          if soPersistent o
            then do
              p <- newPersistent se (sshHooks base run1) cwdRef
              pure . Right $ sandbox (executePersistent p (soTimeout o) oneshot) cwdRef (cleanupPersistent p >> closeMaster)
            else pure . Right $ sandbox oneshot cwdRef closeMaster
  where
    sandbox ex cwdRef clean = Sandbox "ssh" ex (readIORef cwdRef) (soTimeout o) clean
    stripSlash = reverse . dropWhile (== '/') . reverse
    firstNonBlank a b = if T.null a then b else a

sshHooks :: [Text] -> ([Text] -> Int -> IO (Maybe (ExitCode, String, String))) -> PersistentHooks
sshHooks base run1 =
  PersistentHooks
    { phPrefixBase = "/tmp/gauss-ssh"
    , phSpawn = do
        (Just hin, Just hout, _, ph) <-
          createProcess (proc (T.unpack (head base)) (map T.unpack (tail base) <> ["bash -l"])) {std_in = CreatePipe, std_out = CreatePipe, std_err = NoStream}
        pure (hin, hout, ph)
    , phReadFiles = \sid paths -> case paths of
        [p] -> do
          r <- run1 (base <> ["cat " <> p <> " 2>/dev/null"]) 10
          pure [maybe "" (\(_, o, _) -> T.pack o) r]
        _ -> do
          let delim = "__GAUSS_SEP_" <> sid <> "__"
              script = T.intercalate "; " ["cat " <> p <> " 2>/dev/null; echo '" <> delim <> "'" | p <- paths]
          r <- run1 (base <> [script]) 10
          case r of
            Nothing -> pure (map (const "") paths)
            Just (_, o, _) ->
              let parts = T.splitOn (delim <> "\n") (T.pack o)
               in pure [if i < length parts then parts !! i else "" | i <- [0 .. length paths - 1]]
    , phKillChildren = \pid -> void (run1 (base <> ["pkill -P " <> T.pack (show pid) <> " 2>/dev/null; true"]) 5)
    , phCleanupFiles = \pre -> void (run1 (base <> ["rm -f " <> pre <> "-*"]) 5)
    , phPollInterval = 0.15
    }
