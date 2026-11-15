-- | What a model-call job needs from the run: the model call ('Ask'), what
-- its calls came to ('Asked'), and the kernel checker ('Checker'). Its own
-- module so that "Fisher.Role.Agent" and "Fisher.Search.GraphJobs" (which
-- runs its jobs in the role monad) can both use them;
-- "Fisher.Search.GraphJobs" re-exports all of it.
module Fisher.Search.Ask
  ( AskError (..)
  , Ask
  , Asked (..)
  , Checker (..)
  , locked
  ) where

import Control.Concurrent.MVar (MVar, withMVar)
import Data.Text (Text)

import Fisher.Role (Role)
import Fisher.Search.Check (AxiomPolicy, LeanRun)
import Fisher.Search.Split (Message)

data AskError = AskFailed Text | AskNoCredit deriving (Eq, Show)

-- | One model call: the reply and its dollars.
type Ask = Role -> [Message] -> IO (Either AskError (Text, Double))

-- | What a job's model calls came to, with the dollars they cost.
data Asked a = NoCredit Double | Asked Double (Either Text a)

-- | How sketches are compiled. Probes write one scratch file beside the
-- target, so every check takes the lock.
data Checker = Checker
  { ckLean :: LeanRun, ckPolicy :: AxiomPolicy, ckRoot :: FilePath, ckRel :: FilePath, ckLock :: MVar () }

locked :: Checker -> IO a -> IO a
locked ck act = withMVar (ckLock ck) (const act)
