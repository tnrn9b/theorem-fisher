-- | The one interface every execution backend implements: run a shell
-- command in a working directory, with an optional timeout and stdin, and
-- get back its output and exit code. File tools, and later the terminal
-- tool, only ever talk to a 'Sandbox'.
module Fisher.Tools.Sandbox
  ( Sandbox (..)
  , ExecRequest (..)
  , ExecResult (..)
  , exec
  , Interrupt
  , newInterrupt
  , raiseInterrupt
  , clearInterrupt
  , isInterrupted
  , timeoutResult
  ) where

import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T

-- | One command. An empty 'erCwd' means the sandbox's own working
-- directory; 'Nothing' for the timeout means its default.
data ExecRequest = ExecRequest
  { erCommand :: Text
  , erCwd :: Text
  , erTimeout :: Maybe Int
  , erStdin :: Maybe Text
  }
  deriving (Eq, Show)

-- | @{"output": ..., "returncode": ...}@.
data ExecResult = ExecResult
  { exOutput :: Text
  , exCode :: Int
  }
  deriving (Eq, Show)

data Sandbox = Sandbox
  { sbKind :: Text
  -- ^ local, docker or ssh
  , sbExecute :: ExecRequest -> IO ExecResult
  , sbCwd :: IO Text
  -- ^ the working directory now (a persistent shell's changes as it goes)
  , sbDefaultTimeout :: Int
  , sbCleanup :: IO ()
  }

exec :: Sandbox -> Text -> IO ExecResult
exec sb cmd = sbExecute sb (ExecRequest cmd "" Nothing Nothing)

-- | Upstream's process-wide interrupt flag (set when the user sends a new
-- message mid-command); backends poll it and never clear it.
newtype Interrupt = Interrupt (TVar Bool)

newInterrupt :: IO Interrupt
newInterrupt = Interrupt <$> newTVarIO False

raiseInterrupt, clearInterrupt :: Interrupt -> IO ()
raiseInterrupt (Interrupt v) = atomically (writeTVar v True)
clearInterrupt (Interrupt v) = atomically (writeTVar v False)

isInterrupted :: Interrupt -> IO Bool
isInterrupted (Interrupt v) = readTVarIO v

-- | @_timeout_result@.
timeoutResult :: Int -> ExecResult
timeoutResult t = ExecResult ("Command timed out after " <> T.pack (show t) <> "s") 124
