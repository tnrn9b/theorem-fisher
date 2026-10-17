-- | A swarm task as the views see it.
module Fisher.Swarm.Types
  ( Status (..)
  , statusName
  , TaskInfo (..)
  , TaskView (..)
  ) where

import Data.Text (Text)

import Fisher.Swarm.Event (Progress)

data Status = Queued | Running | Complete | Failed | Cancelled
  deriving (Eq, Ord, Show)

statusName :: Status -> Text
statusName = \case
  Queued -> "queued"
  Running -> "running"
  Complete -> "complete"
  Failed -> "failed"
  Cancelled -> "cancelled"

-- | What a task is, fixed when it is spawned.
data TaskInfo = TaskInfo
  { tiTaskId :: Text
  , tiDescription :: Text
  , tiTheorem :: Text
  , tiWorkflowKind :: Text
  , tiWorkflowCommand :: Text
  , tiProjectName :: Text
  , tiProjectRoot :: FilePath
  , tiWorkingDir :: FilePath
  , tiBackendName :: Text
  , tiSessionId :: Text
  }
  deriving (Eq, Show)

-- | A task at one moment.
data TaskView = TaskView
  { tvInfo :: TaskInfo
  , tvStatus :: Status
  , tvProgress :: Progress
  , tvStart :: Maybe Double
  , tvEnd :: Maybe Double
  , tvHasPty :: Bool
  }
  deriving (Eq, Show)
