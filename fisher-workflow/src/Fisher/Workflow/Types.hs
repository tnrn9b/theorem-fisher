-- | The errors a managed launch can end in, and the facts about this
-- machine that change a launch plan. Tests inject the facts recorded with
-- the reference outputs instead of probing the machine they run on.
module Fisher.Workflow.Types
  ( WorkflowError (..)
  , errorClass
  , errorMessage
  , throwW
  , Env
  , Machine (..)
  , liveMachine
  ) where

import Control.Exception (Exception, throwIO)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO (stderr)
import Data.Time (UTCTime, getCurrentTime, getZonedTime, formatTime, defaultTimeLocale)
import System.Posix.User (getEffectiveUserID)

import Fisher.Workflow.LocalAuth (readKeychainClaudeCredentials)

-- | Upstream's exception classes, by name, with their messages.
data WorkflowError
  = UsageError Text
  | ConfigError Text
  | PreflightError Text
  | StagingError Text
  | HandoffConfig Text
  | HandoffLaunch Text
  | HandoffModeUnavailable Text
  deriving (Eq, Show)

instance Exception WorkflowError

errorClass :: WorkflowError -> Text
errorClass = \case
  UsageError _ -> "AutoformalizeUsageError"
  ConfigError _ -> "AutoformalizeConfigError"
  PreflightError _ -> "AutoformalizePreflightError"
  StagingError _ -> "AutoformalizeStagingError"
  HandoffConfig _ -> "HandoffConfigError"
  HandoffLaunch _ -> "HandoffLaunchError"
  HandoffModeUnavailable _ -> "HandoffModeUnavailable"

errorMessage :: WorkflowError -> Text
errorMessage = \case
  UsageError m -> m
  ConfigError m -> m
  PreflightError m -> m
  StagingError m -> m
  HandoffConfig m -> m
  HandoffLaunch m -> m
  HandoffModeUnavailable m -> m

throwW :: WorkflowError -> IO a
throwW = throwIO

-- | An environment, as a map so plans print the same every time.
type Env = Map String String

data Machine = Machine
  { mIsRoot :: Bool
  -- ^ Claude Code refuses @--dangerously-skip-permissions@ as root.
  , mStrictUnavailable :: IO (Maybe Text)
  -- ^ Why strict terminal handoff cannot run here, if it cannot.
  , mKeychain :: IO (Maybe Text)
  -- ^ Claude Code's Keychain credentials, on macOS.
  , mNow :: IO UTCTime
  , mStamp :: IO String
  -- ^ The local-time stamp in startup-context file names.
  , mFisherHome :: FilePath
  , mArxivScript :: Maybe FilePath
  -- ^ Where the arXiv helper is installed, if it is.
  , mNote :: Text -> IO ()
  -- ^ A note or warning for the person launching (stderr).
  }

-- | The machine this process runs on.
liveMachine :: FilePath -> Maybe FilePath -> IO (Maybe Text) -> IO Machine
liveMachine home arxiv strict = do
  euid <- getEffectiveUserID

  pure Machine
    { mIsRoot = euid == 0
    , mStrictUnavailable = strict
    , mKeychain = readKeychainClaudeCredentials
    , mNow = getCurrentTime
    , mStamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getZonedTime
    , mFisherHome = home
    , mArxivScript = arxiv
    , mNote = TIO.hPutStrLn stderr
    }

