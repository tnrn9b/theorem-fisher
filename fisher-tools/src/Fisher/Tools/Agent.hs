-- | The tool layer as the agent uses it: a registry with the file tools,
-- each task's sandbox made on first use from the @TERMINAL_*@ settings,
-- and the file operations built on it (upstream's @_get_file_ops@).
module Fisher.Tools.Agent
  ( AgentTools (..)
  , newAgentTools
  , newAgentToolsWith
  ) where

import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Environment (lookupEnv)
import System.Info (os)

import Fisher.Tools.File
import Fisher.Tools.FileOps
import Fisher.Tools.Registry
import Fisher.Tools.Sandbox.Config
import Fisher.Tools.Sandbox.Local
import Fisher.Tools.Sandbox.Remote

data AgentTools = AgentTools
  { atRegistry :: Registry
  , atFileTools :: FileTools
  , atSandboxes :: SandboxManager
  , atSandboxEnv :: SandboxEnv
  }

getenvT :: Text -> IO (Maybe Text)
getenvT k = fmap T.pack <$> lookupEnv (T.unpack k)

-- | @fisherHome@ is where docker workspaces go (@<home>/sandboxes@ unless
-- @TERMINAL_SANDBOX_DIR@ says otherwise).
newAgentTools :: FilePath -> IO AgentTools
newAgentTools = newAgentToolsWith id

-- | With the sandbox environment adjusted (the CLI's sudo prompt).
newAgentToolsWith :: (SandboxEnv -> SandboxEnv) -> FilePath -> IO AgentTools
newAgentToolsWith adjust fisherHome = do
  se <- adjust <$> defaultSandboxEnv
  home <- T.pack <$> getHomeDirectory
  scope <- homeWriteScope home
  sandboxDir <- maybe (fisherHome <> "/sandboxes") id <$> lookupEnv "TERMINAL_SANDBOX_DIR"
  let platform = if os == "darwin" then "darwin" else T.pack os
      host = systemDockerHost platform sandboxDir
      create tid = do
        cwd <- T.pack <$> getCurrentDirectory
        cfg <- getEnvConfig getenvT cwd
        case cfg of
          Left e -> pure (Left e)
          Right c -> createSandbox se host c tid noOverrides
  mgr <- newSandboxManager create
  opsCache <- newMVar M.empty
  let opsFor tid = do
        sb <- sandboxFor mgr tid
        case sb of
          Left e -> pure (Left e)
          Right s -> do
            gen <- sandboxGeneration mgr tid
            modifyMVar opsCache $ \cache -> case M.lookup tid cache of
              Just (g, o) | g == gen -> pure (cache, Right o)
              _ -> do
                o <- newFileOps scope s
                pure (M.insert tid (gen, o) cache, Right o)
      check = do
        cwd <- T.pack <$> getCurrentDirectory
        checkTerminalRequirements getenvT cwd
  ft <- newFileTools opsFor (fmap T.pack <$> lookupEnv "FISHER_REDACT_SECRETS") check
  reg <- newRegistry
  registerFileTools reg ft
  pure (AgentTools reg ft mgr se)
