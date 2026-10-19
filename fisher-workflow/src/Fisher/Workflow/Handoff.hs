-- | A resolved handoff: the argv, cwd and environment a managed child gets,
-- and the terminal mode it would run in (@build_handoff_request@ and
-- @_resolve_mode@ in @gauss_cli/handoff.py@).
module Fisher.Workflow.Handoff
  ( HandoffRequest (..)
  , buildHandoffRequest
  , resolveMode
  , requestedModeOf
  , strictModeUnavailableReason
  ) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (unless)
import Data.Aeson (Value)

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.IO.Error (ioeGetErrorString)
import System.Posix.IO (OpenMode (ReadWrite), closeFd, defaultFileFlags, openFd)
import System.Posix.Terminal (getTerminalProcessGroupID)

import Fisher.Paths (expandUser)
import Fisher.Py (lookupKey, pyStrOr)
import Fisher.Workflow.Run (whichPath)
import Fisher.Workflow.Settings (autoformalizeConfig)
import Fisher.Workflow.Types

data HandoffRequest = HandoffRequest
  { hrArgv :: [Text]
  , hrCwd :: FilePath
  , hrEnv :: Env
  , hrMode :: Text
  , hrRequestedMode :: Text
  , hrSource :: Text
  , hrLabel :: Text
  , hrModeNote :: Maybe Text
  }
  deriving (Eq, Show)

-- | @fisher.autoformalize.handoff_mode@: auto, helper or strict.
requestedModeOf :: Value -> Either WorkflowError Text
requestedModeOf cfg =
  let v = T.toLower (pyStrOr (lookupKey "handoff_mode" (autoformalizeConfig cfg)))
      value = if T.null v then "auto" else v
   in if value `elem` ["auto", "helper", "strict"]
        then Right value
        else Left (ConfigError "`fisher.autoformalize.handoff_mode` must be one of: auto, helper, strict.")

-- | Why strict mode cannot run: it needs a controlling terminal whose
-- foreground process group can be read.
strictModeUnavailableReason :: IO (Maybe Text)
strictModeUnavailableReason = do
  r <- tryIO (bracket (openFd "/dev/tty" ReadWrite defaultFileFlags) closeFd getTerminalProcessGroupID)
  pure $ case r of
    Right _ -> Nothing
    Left e -> Just ("strict mode requires a controlling TTY (" <> T.pack (ioeGetErrorString e) <> ")")
  where
    tryIO :: IO a -> IO (Either IOException a)
    tryIO = try

resolveMode :: Maybe Text -> Text -> Either WorkflowError (Text, Maybe Text)
resolveMode reason = \case
  "helper" -> Right ("helper", Nothing)
  "strict" -> maybe (Right ("strict", Nothing)) (Left . HandoffModeUnavailable) reason
  _ -> Right (maybe ("strict", Nothing) (\r -> ("helper", Just ("auto fallback from strict: " <> r))) reason)

-- | Validate and assemble a request. The environment is the one given;
-- upstream also lays it over this process's environment, which brings back
-- any credential variable the plan removed (see README).
buildHandoffRequest :: Machine -> [Text] -> FilePath -> Env -> Text -> Text -> Text -> IO HandoffRequest
buildHandoffRequest m argv0 cwd0 env requested label source = do
  let argv = filter (not . T.null) (map T.strip argv0)
  case argv of
    [] -> throwW (HandoffConfig (if null argv0 then "Handoff argv must contain at least one executable." else "Handoff argv must contain non-empty strings."))
    (exe : _) -> do
      cwd <- expandUser cwd0 >>= canonicalizePath
      isDir <- doesDirectoryExist cwd
      unless isDir $ throwW (HandoffConfig ("Handoff cwd does not exist: " <> T.pack cwd))
      found <- whichPath env (T.unpack exe)
      unless (found /= Nothing) $ throwW (HandoffLaunch ("Executable not found: " <> exe))
      reason <- mStrictUnavailable m
      (mode, note) <- either throwW pure (resolveMode reason requested)
      pure (HandoffRequest argv cwd env mode requested source label note)

