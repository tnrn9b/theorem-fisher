-- | A run-log event read from a managed backend's stream (Claude Code's
-- or Codex's JSON lines, or the native agent's stream-json), before it is
-- written with 'Fisher.Runlog.event'. Here so that every package that
-- reads a backend's stream shares the type; the parsers live with their
-- readers ('Fisher.Search.Stream').
module Fisher.Runlog.Stream
  ( StreamEvent (..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

data StreamEvent = StreamEvent
  { stT :: Maybe Double
  -- ^ seconds since the run's start; Nothing when the stream has no
  -- clock (the writer fills it in)
  , stKind :: Text
  -- ^ the run-log event kind: @message@, @tool_start@, @tool_end@,
  -- @cost@, @outcome@
  , stData :: Value
  }
  deriving (Eq, Show)
