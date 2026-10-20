-- | The tools beyond the file tools: web_search and web_extract, the
-- browser, the terminal and the process tool, made once per process and
-- registered beside the file tools; and the terminal's prompts (approval
-- and sudo).
module Fisher.Cli.NetTools
  ( NetTools (..)
  , NetUi (..)
  , newNetTools
  , registerNetTools
  , ttyUi
  , sudoPromptFor
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTime)
import System.Directory (doesDirectoryExist, doesFileExist, executable, findExecutable, getCurrentDirectory, getHomeDirectory, getPermissions, getTemporaryDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO
import System.Info (os)
import qualified System.Posix.Signals as Sig
import qualified System.Timeout as Timeout
import Control.Concurrent (threadDelay)

import Fisher.Agent.Aux (AuxEnv (..), callLlm, newAuxState)
import Fisher.Agent.Tools.Approval
import Fisher.Agent.Tools.Browser
import Fisher.Agent.Tools.Process
import Fisher.Agent.Tools.Terminal
import Fisher.Agent.Tools.Web
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered)
import Fisher.LLM.Http (Runtime (..), Transport)
import Fisher.LLM.Sys
import qualified Fisher.Tools.Sandbox as Sb
import Fisher.Tools.Agent (AgentTools (..))
import Fisher.Tools.Registry (Registry, register)
import Fisher.Tools.Sandbox.Config (checkTerminalRequirements, sandboxFor)
import Fisher.Tools.Sandbox.Local (SandboxEnv (..), findBash)

-- | How the terminal's prompts reach the user.
data NetUi = NetUi
  { nuSay :: Text -> IO ()
  -- ^ @print(...)@
  , nuRaw :: Text -> IO ()
  , nuReadLine :: Text -> IO (Either Bool Text)
  -- ^ prompt, then one line: Left True after 60 s, Left False at end of input
  , nuReadHidden :: IO (Maybe Text)
  -- ^ a password from the terminal, unechoed; Nothing after 45 s
  }

data NetTools = NetTools
  { ntWeb :: WebEnv
  , ntBrowser :: BrowserEnv
  , ntProcesses :: ProcessRegistry
  , ntTerminal :: TerminalEnv
  , ntFirstUse :: IO ()
  -- ^ the terminal's cleanup pass, whose import registers @process@
  }

-- | The prompts on this process's terminal.
ttyUi :: (Text -> IO ()) -> (Text -> IO ()) -> NetUi
ttyUi say raw = NetUi say raw readLine readHidden
  where
    readLine prompt = do
      raw prompt
      r <- Timeout.timeout (60 * 1000000) (try @SomeException TIO.getLine)
      pure $ case r of
        Nothing -> Left True
        Just (Left _) -> Left False
        Just (Right l) -> Right l
    readHidden = do
      r <- try @SomeException $ bracket (openFile "/dev/tty" ReadMode) hClose $ \h -> do
        old <- hGetEcho h
        hSetEcho h False
        l <- Timeout.timeout (45 * 1000000) (TIO.hGetLine h)
        hSetEcho h old
        pure l
      pure $ case r of
        Left _ -> Just ""
        Right l -> l

-- | @_transform_sudo_command@'s password source for the sandboxes.
sudoPromptFor :: Sys -> NetUi -> IO SudoPrompt
sudoPromptFor sys ui = newSudoPrompt (sysGetEnv sys) (threadDelay 200000 >> ttySudoPrompt (nuRaw ui) (nuReadHidden ui))

newNetTools :: Sys -> Runtime -> Transport -> (Text -> IO (Maybe Text)) -> IO Bool -> NetUi -> AgentTools -> IO NetTools
newNetTools sys rt tr which record ui at = do
  aux <- AuxEnv sys rt tr (loadConfigOrdered sys) (\_ -> pure ()) <$> newAuxState
  let intr = Sb.isInterrupted (seInterrupt (atSandboxEnv at))
      gh = sysFisherHome sys
  web <- newWebEnv tr (sysGetEnv sys) (rtSleep rt) (callLlm aux) intr
  tmp <- getTemporaryDirectory
  browser <-
    newBrowserEnv processRunner which (sysGetEnv sys) tr (callLlm aux) intr (sysUuidHex sys) gh
      (socketRootFor (T.pack os) tmp) doesDirectoryExist
      (\pid -> () <$ try @SomeException (Sig.signalProcess Sig.sigTERM (fromIntegral pid)))
      (realToFrac <$> getPOSIXTime)
      (T.pack . formatTime defaultTimeLocale "%Y%m%d_%H%M%S" <$> getZonedTime)
      record
  procs <-
    newProcessRegistry
      ProcessEnv
        { peNow = realToFrac <$> getPOSIXTime, peMonotonic = getMonotonicTime, peSleep = \s -> threadDelay (round (s * 1e6))
        , peUuidHex = sysUuidHex sys, peGetEnv = sysGetEnv sys, peEnviron = map (\(k, v) -> (T.pack k, T.pack v)) <$> getEnvironment, peBash = findBash
        , peCwd = getCurrentDirectory, peInterrupted = intr, peCheckpoint = gh </> "processes.json", pePollRemote = True }
  st <- newApprovalState
  home <- getHomeDirectory
  let isExec p = do
        f <- doesFileExist p
        if f then executable <$> getPermissions p else pure False
      tirith =
        TirithEnv
          { tiGetEnv = sysGetEnv sys, tiSecurity = configSecurity gh, tiWhich = fmap (fmap T.pack) . findExecutable . T.unpack
          , tiIsExecutable = isExec
          , tiExpandUser = \p -> pure (if "~/" `T.isPrefixOf` p || p == "~" then T.pack home <> T.drop 1 p else p)
          , tiFisherHome = gh
          , tiRun = \argv secs -> do
              -- a missing binary fails as Python's spawn does
              ok <- case argv of
                (p : _) | "/" `T.isInfixOf` p -> doesFileExist (T.unpack p)
                (p : _) -> maybe False (const True) <$> findExecutable (T.unpack p)
                [] -> pure False
              if ok
                then processRunner argv [] secs
                else pure (RunFailed ("[Errno 2] No such file or directory: '" <> head argv <> "'")) }
      approval =
        ApprovalEnv
          { aeGetEnv = sysGetEnv sys, aeMode = configApprovalMode gh, aeTirith = tirithCheck tirith, aeAux = callLlm aux
          , aePrompt = textPrompt (nuSay ui) (nuReadLine ui), aeSavePermanent = saveAllowlist gh, aeState = st }
  registered <- newIORef False
  regRef <- newIORef Nothing
  let firstUse = do
        done <- readIORef registered
        unless done $ do
          writeIORef registered True
          readIORef regRef >>= mapM_ (\reg -> register reg (processToolDef procs))
      term =
        TerminalEnv
          { teGetEnv = sysGetEnv sys, teCwd = T.pack <$> getCurrentDirectory, teSandbox = sandboxFor (atSandboxes at)
          , teApproval = approval, teSleep = rtSleep rt, teProcesses = procs, teFirstUse = firstUse
          , teCheck = do
              cwd <- T.pack <$> getCurrentDirectory
              checkTerminalRequirements (sysGetEnv sys) cwd }
  writeIORef regRef (Just (atRegistry at))
  pure (NetTools web browser procs term firstUse)

registerNetTools :: Registry -> NetTools -> IO ()
registerNetTools reg nt = do
  registerWebTools reg (ntWeb nt)
  registerBrowserTools reg (ntBrowser nt)
  register reg (terminalToolDef (ntTerminal nt))
