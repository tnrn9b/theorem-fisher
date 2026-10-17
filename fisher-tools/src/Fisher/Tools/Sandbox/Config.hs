-- | Which backend to use and how, read from the @TERMINAL_*@ environment
-- variables on every call as upstream's @_get_env_config@ does; building a
-- sandbox from that; the requirements check; and the per-task cache with
-- its idle cleanup.
module Fisher.Tools.Sandbox.Config
  ( EnvConfig (..)
  , getEnvConfig
  , envConfigDict
  , TaskOverrides (..)
  , noOverrides
  , createSandbox
  , checkTerminalRequirements
  , SandboxManager
  , newSandboxManager
  , sandboxFor
  , sandboxGeneration
  , touchTask
  , cleanupInactive
  , cleanupTask
  , cleanupAll
  , defaultImage
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (doesDirectoryExist, makeAbsolute)

import Fisher.Py.Value (PyVal (..), loads, reprStr)
import Fisher.Tools.Py
import Fisher.Tools.Sandbox
import Fisher.Tools.Sandbox.Local
import Fisher.Tools.Sandbox.Remote

defaultImage :: Text
defaultImage = "nikolaik/python-nodejs:python3.11-nodejs20"

data EnvConfig = EnvConfig
  { ecEnvType :: Text
  , ecDockerImage :: Text
  , ecSingularityImage :: Text
  , ecModalImage :: Text
  , ecDaytonaImage :: Text
  , ecCwd :: Text
  , ecHostCwd :: Maybe Text
  , ecDockerMountCwd :: Bool
  , ecTimeout :: Integer
  , ecLifetime :: Integer
  , ecSshHost :: Text
  , ecSshUser :: Text
  , ecSshPort :: Integer
  , ecSshKey :: Text
  , ecSshPersistent :: Bool
  , ecLocalPersistent :: Bool
  , ecContainerCpu :: Double
  , ecContainerMemory :: Integer
  , ecContainerDisk :: Integer
  , ecContainerPersistent :: Bool
  , ecDockerVolumes :: PyVal
  }
  deriving (Show)

-- | @_get_env_config()@; 'Left' is the @ValueError@ text for a malformed
-- number or JSON value. @cwdNow@ is this process's working directory.
getEnvConfig :: (Text -> IO (Maybe Text)) -> Text -> IO (Either Text EnvConfig)
getEnvConfig getenv cwdNow = do
  let env k d = maybe d id <$> getenv k
  envType <- env "TERMINAL_ENV" "local"
  mountRaw <- env "TERMINAL_DOCKER_MOUNT_CWD_TO_WORKSPACE" "false"
  let mount = pyBool mountRaw
      defaultCwd
        | envType == "local" = cwdNow
        | envType == "ssh" = "~"
        | otherwise = "/root"
      hostPrefixes = ["/Users/", "/home/", "C:\\", "C:/"]
  rawCwd <- getenv "TERMINAL_CWD"
  let cwd0 = maybe defaultCwd id rawCwd
  (cwd, hostCwd) <-
    if envType == "docker" && mount
      then do
        let src = case rawCwd of
              Just c | not (T.null c) -> c
              _ -> cwdNow
        cand <- T.pack <$> makeAbsolute (T.unpack src)
        isDir <- doesDirectoryExist (T.unpack cand)
        if any (`T.isPrefixOf` cand) hostPrefixes || ("/" `T.isPrefixOf` cand && isDir && not (any (`T.isPrefixOf` cand) ["/workspace", "/root"]))
          then pure ("/workspace", Just cand)
          else pure (cwd0, Nothing)
      else
        if envType `elem` ["modal", "docker", "singularity", "daytona"] && not (T.null cwd0) && any (`T.isPrefixOf` cwd0) hostPrefixes && cwd0 /= defaultCwd
          then pure (defaultCwd, Nothing)
          else pure (cwd0, Nothing)
  dockerImage <- env "TERMINAL_DOCKER_IMAGE" defaultImage
  singImage <- env "TERMINAL_SINGULARITY_IMAGE" ("docker://" <> defaultImage)
  modalImage <- env "TERMINAL_MODAL_IMAGE" defaultImage
  daytonaImage <- env "TERMINAL_DAYTONA_IMAGE" defaultImage
  let parseWith :: (Text -> Maybe a) -> Text -> Text -> Text -> IO (Either Text a)
      parseWith conv label name def = do
        raw <- env name def
        pure $ case conv raw of
          Just v -> Right v
          Nothing -> Left ("Invalid value for " <> name <> ": " <> reprStr raw <> " (expected " <> label <> "). Check ~/.gauss/.env or environment variables.")
      int = parseWith pyInt "integer"
  sshHost <- env "TERMINAL_SSH_HOST" ""
  sshUser <- env "TERMINAL_SSH_USER" ""
  sshKey <- env "TERMINAL_SSH_KEY" ""
  persistentShell <- env "TERMINAL_PERSISTENT_SHELL" "true"
  sshPersistent <- pyBool <$> env "TERMINAL_SSH_PERSISTENT" persistentShell
  localPersistent <- pyBool <$> env "TERMINAL_LOCAL_PERSISTENT" "false"
  containerPersistent <- pyBool <$> env "TERMINAL_CONTAINER_PERSISTENT" "true"
  rT <- int "TERMINAL_TIMEOUT" "180"
  rL <- int "TERMINAL_LIFETIME_SECONDS" "300"
  rP <- int "TERMINAL_SSH_PORT" "22"
  rC <- parseWith pyFloat "number" "TERMINAL_CONTAINER_CPU" "1"
  rM <- int "TERMINAL_CONTAINER_MEMORY" "5120"
  rD <- int "TERMINAL_CONTAINER_DISK" "51200"
  rV <- parseWith (either (const Nothing) Just . loads . TE.encodeUtf8) "valid JSON" "TERMINAL_DOCKER_VOLUMES" "[]"
  pure $ do
    timeout' <- rT
    lifetime <- rL
    port <- rP
    cpu <- rC
    mem <- rM
    disk <- rD
    vols <- rV
    Right
      EnvConfig
        { ecEnvType = envType
        , ecDockerImage = dockerImage
        , ecSingularityImage = singImage
        , ecModalImage = modalImage
        , ecDaytonaImage = daytonaImage
        , ecCwd = cwd
        , ecHostCwd = hostCwd
        , ecDockerMountCwd = mount
        , ecTimeout = timeout'
        , ecLifetime = lifetime
        , ecSshHost = sshHost
        , ecSshUser = sshUser
        , ecSshPort = port
        , ecSshKey = sshKey
        , ecSshPersistent = sshPersistent
        , ecLocalPersistent = localPersistent
        , ecContainerCpu = cpu
        , ecContainerMemory = mem
        , ecContainerDisk = disk
        , ecContainerPersistent = containerPersistent
        , ecDockerVolumes = vols
        }

-- | The config as upstream's dict, keys in its order.
envConfigDict :: EnvConfig -> PyVal
envConfigDict c =
  PDict
    [ ("env_type", PStr (ecEnvType c))
    , ("docker_image", PStr (ecDockerImage c))
    , ("singularity_image", PStr (ecSingularityImage c))
    , ("modal_image", PStr (ecModalImage c))
    , ("daytona_image", PStr (ecDaytonaImage c))
    , ("cwd", PStr (ecCwd c))
    , ("host_cwd", maybe PNull PStr (ecHostCwd c))
    , ("docker_mount_cwd_to_workspace", PBool (ecDockerMountCwd c))
    , ("timeout", PInt (ecTimeout c))
    , ("lifetime_seconds", PInt (ecLifetime c))
    , ("ssh_host", PStr (ecSshHost c))
    , ("ssh_user", PStr (ecSshUser c))
    , ("ssh_port", PInt (ecSshPort c))
    , ("ssh_key", PStr (ecSshKey c))
    , ("ssh_persistent", PBool (ecSshPersistent c))
    , ("local_persistent", PBool (ecLocalPersistent c))
    , ("container_cpu", PFloat (ecContainerCpu c))
    , ("container_memory", PInt (ecContainerMemory c))
    , ("container_disk", PInt (ecContainerDisk c))
    , ("container_persistent", PBool (ecContainerPersistent c))
    , ("docker_volumes", ecDockerVolumes c)
    ]

-- | @register_task_env_overrides@: per-task image and cwd.
data TaskOverrides = TaskOverrides {toDockerImage :: Maybe Text, toCwd :: Maybe Text}

noOverrides :: TaskOverrides
noOverrides = TaskOverrides Nothing Nothing

-- | @_create_environment@ for local, docker and ssh. The docker host
-- working directory is bind-mounted whenever it is configured (upstream's
-- file tools forget to pass the mount flag; Fisher has one creation path).
createSandbox :: SandboxEnv -> DockerHost -> EnvConfig -> Text -> TaskOverrides -> IO (Either Text Sandbox)
createSandbox se host c taskId ov =
  let cwd = maybe (ecCwd c) id (nonEmpty =<< toCwd ov)
      timeout' = fromIntegral (ecTimeout c)
   in case ecEnvType c of
        "local" -> Right <$> newLocalSandbox se cwd timeout' (ecLocalPersistent c)
        "docker" ->
          newDockerSandbox se host $
            defaultDockerOpts
              { doImage = maybe (ecDockerImage c) id (nonEmpty =<< toDockerImage ov)
              , doCwd = cwd
              , doTimeout = timeout'
              , doCpu = ecContainerCpu c
              , doMemory = ecContainerMemory c
              , doDisk = ecContainerDisk c
              , doPersistent = ecContainerPersistent c
              , doTaskId = taskId
              , doVolumes = ecDockerVolumes c
              , doHostCwd = ecHostCwd c
              , doAutoMountCwd = ecDockerMountCwd c
              }
        "ssh"
          | T.null (ecSshHost c) || T.null (ecSshUser c) -> pure (Left "SSH environment requires ssh_host and ssh_user to be configured")
          | otherwise -> newSshSandbox se (SshOpts (ecSshHost c) (ecSshUser c) (ecSshPort c) (ecSshKey c) cwd timeout' (ecSshPersistent c))
        other
          | other `elem` ["singularity", "modal", "daytona"] -> pure (Left ("The " <> other <> " backend is not available in Fisher; use 'local', 'docker' or 'ssh'"))
          | otherwise -> pure (Left ("Unknown environment type: " <> other <> ". Use 'local', 'docker', 'singularity', 'modal', 'daytona', or 'ssh'"))
  where
    nonEmpty t = if T.null t then Nothing else Just t

-- | @check_terminal_requirements()@ (what @check_file_requirements@
-- returns): local always; docker when @docker version@ succeeds; ssh when
-- host and user are both set. A config error is False.
checkTerminalRequirements :: (Text -> IO (Maybe Text)) -> Text -> IO Bool
checkTerminalRequirements getenv cwdNow = do
  r <- getEnvConfig getenv cwdNow
  case r of
    Left _ -> pure False
    Right c -> case ecEnvType c of
      "local" -> pure True
      "docker" -> either (const False) (const True) <$> ensureDockerAvailable
      "ssh" -> pure (not (T.null (ecSshHost c)) && not (T.null (ecSshUser c)))
      _ -> pure False

-- ------------------------------------------------------------------ manager

-- | Sandboxes per task id, created on first use and dropped after
-- @TERMINAL_LIFETIME_SECONDS@ without use.
data SandboxManager = SandboxManager
  { smEnvs :: MVar (M.Map Text (Sandbox, Double))
  , smCreate :: Text -> IO (Either Text Sandbox)
  , smGen :: MVar (M.Map Text Int)
  }

newSandboxManager :: (Text -> IO (Either Text Sandbox)) -> IO SandboxManager
newSandboxManager create = SandboxManager <$> newMVar M.empty <*> pure create <*> newMVar M.empty

-- | Which incarnation of the task's sandbox is current (it changes each
-- time one is created), so callers can drop state built on an old one.
sandboxGeneration :: SandboxManager -> Text -> IO Int
sandboxGeneration m tid = M.findWithDefault 0 tid <$> readMVar (smGen m)

now' :: IO Double
now' = realToFrac <$> getPOSIXTime

-- | The task's sandbox, creating it if needed (creation errors are not
-- cached; the next call retries).
sandboxFor :: SandboxManager -> Text -> IO (Either Text Sandbox)
sandboxFor m tid = modifyMVar (smEnvs m) $ \envs -> do
  t <- now'
  case M.lookup tid envs of
    Just (sb, _) -> pure (M.insert tid (sb, t) envs, Right sb)
    Nothing -> do
      r <- smCreate m tid
      case r of
        Right sb -> do
          modifyMVar_ (smGen m) (pure . M.insertWith (+) tid 1)
          pure (M.insert tid (sb, t) envs, Right sb)
        Left e -> pure (envs, Left e)

-- | Mark a task as active (Fisher also does this after each command, so a
-- long command is never cleaned up under itself).
touchTask :: SandboxManager -> Text -> IO ()
touchTask m tid = do
  t <- now'
  modifyMVar_ (smEnvs m) (pure . M.adjust (\(sb, _) -> (sb, t)) tid)

-- | @_cleanup_inactive_envs(lifetime)@: the task ids cleaned.
cleanupInactive :: SandboxManager -> Double -> IO [Text]
cleanupInactive m lifetime = do
  t <- now'
  stale <- modifyMVar (smEnvs m) $ \envs ->
    let (old, keep) = M.partition (\(_, last') -> t - last' > lifetime) envs
     in pure (keep, M.toList old)
  forM stale $ \(tid, (sb, _)) -> do
    _ <- try (sbCleanup sb) :: IO (Either SomeException ())
    pure tid

cleanupTask :: SandboxManager -> Text -> IO Bool
cleanupTask m tid = do
  found <- modifyMVar (smEnvs m) (\envs -> pure (M.delete tid envs, M.lookup tid envs))
  case found of
    Nothing -> pure False
    Just (sb, _) -> do
      _ <- try (sbCleanup sb) :: IO (Either SomeException ())
      pure True

cleanupAll :: SandboxManager -> IO Int
cleanupAll m = do
  tids <- M.keys <$> readMVar (smEnvs m)
  n <- length . filter id <$> mapM (cleanupTask m) tids
  pure n
