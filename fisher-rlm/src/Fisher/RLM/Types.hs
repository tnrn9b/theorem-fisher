-- | The types the RLM loop works with: model replies and errors, the
-- environment a run needs, and what a run returns. Shaped after RLMIteration,
-- CodeBlock and RLMChatCompletion in rlm/core/types.py (alexzhang13/rlm at
-- d04208afbad29ca675ab13478c40ee8bebc84bfe, MIT; see NOTICE).
module Fisher.RLM.Types
  ( Reply (..)
  , ModelError (..)
  , Model
  , Ending (..)
  , RlmResult (..)
  , IterationRecord (..)
  , SubCall (..)
  , RlmEnv (..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Fisher.LLM.Normalize (Usage)
import Fisher.RLM.Config (RlmConfig)
import Fisher.RLM.Parse (BlockResult, Message)
import Fisher.RLM.Repl (CallKind, ReplFactory)

-- | One successful model call.
data Reply = Reply
  { rpText :: Text
  , rpUsage :: Usage
  , rpModel :: Text
  -- ^ the model that answered; usage is summed per this name
  }
  deriving (Eq, Show)

data ModelError
  = Refused Text
  | Empty
  | NoCredit
  -- ^ HTTP 402: stops the whole run at once
  | OtherError Text
  | OverBudget Text
  -- ^ refused before the call: it would pass the run's budget; the text is
  -- the refusal as the code sees it
  deriving (Eq, Show)

-- | A model: the model name to use instead of its default, if any, and the
-- messages to send.
type Model = Maybe Text -> [Message] -> IO (Either ModelError Reply)

data Ending
  = Answered
  | DefaultAnswer
  -- ^ the iterations ran out and one more call gave the answer
  | Stopped Text
  -- ^ a limit or an error ended the run; the answer is the best partial one
  deriving (Eq, Show)

data RlmResult = RlmResult
  { rrAnswer :: Text
  , rrEnding :: Ending
  , rrIterations :: Int
  , rrUsage :: Map Text Usage
  -- ^ per model, over every call in the tree
  , rrTrajectory :: [IterationRecord]
  }
  deriving (Eq, Show)

data IterationRecord = IterationRecord
  { irPrompt :: [Message]
  , irResponse :: Text
  , irBlocks :: [(Text, BlockResult, [SubCall])]
  -- ^ each block that ran: code, result, the calls it made
  , irFinal :: Maybe Text
  , irSeconds :: Double
  }
  deriving (Eq, Show)

data SubCall = SubCall
  { scKind :: CallKind
  , scPrompt :: Text
  , scResponse :: Text
  , scDepth :: Int
  -- ^ the depth the call ran at: one below the code that made it
  , scModel :: Text
  -- ^ the model that served it: the one the code named, else the sub-model
  -- (the root model for a child run)
  }
  deriving (Eq, Show)

data RlmEnv = RlmEnv
  { reConfig :: RlmConfig
  , reRoot :: Model
  , reSub :: Model
  -- ^ for @llm_query@ and for @rlm_query@ at the depth limit
  , reRepl :: ReplFactory
  , reClock :: IO Double
  -- ^ seconds, from any fixed origin
  , reOnIteration :: Int -> IterationRecord -> IO ()
  -- ^ called after each iteration of the run (0-based; the default answer
  -- call comes last, numbered 'Fisher.RLM.Config.rcMaxIterations')
  }
