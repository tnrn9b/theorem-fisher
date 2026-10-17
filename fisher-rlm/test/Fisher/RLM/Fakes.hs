-- | Stand-ins for the model, the REPL and the clock, for testing the loop.
module Fisher.RLM.Fakes
  ( scriptedModel
  , scriptedReplies
  , reply
  , usageOf
  , fakeRepl
  , pyLike
  , steppingClock
  , fenced
  , testEnv
  ) where

import Control.Concurrent.MVar
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Fisher.LLM.Normalize (Usage (..))
import Fisher.RLM.Config (RlmConfig)
import Fisher.RLM.Parse (BlockResult (..), Message)
import Fisher.RLM.Repl (CallKind (..), Callback, ReplFactory, ReplHandle (..))
import Fisher.RLM.Types

-- | Usage of 10 prompt and 5 completion tokens at the given cost.
usageOf :: Maybe Double -> Usage
usageOf = Usage 10 5 15 Nothing Nothing

-- | A reply from the model "scripted" with 'usageOf' 'Nothing'.
reply :: Text -> Reply
reply t = Reply t (usageOf Nothing) "scripted"

-- | A model giving these texts in order (see 'scriptedReplies').
scriptedModel :: [Text] -> IO (Model, IO [[Message]])
scriptedModel = scriptedReplies . map (Right . reply)

-- | A model giving these outcomes in order, whatever it is asked, and the
-- message lists it was called with, oldest first. Past the end of the script
-- it gives an 'OtherError'.
scriptedReplies :: [Either ModelError Reply] -> IO (Model, IO [[Message]])
scriptedReplies script = do
  st <- newMVar (script, [])
  let model _ msgs = modifyMVar st $ \(rest, seen) -> case rest of
        r : more -> pure ((more, msgs : seen), r)
        [] -> pure (([], msgs : seen), Left (OtherError "script exhausted"))
  pure (model, reverse . snd <$> readMVar st)

-- | A REPL running every block with the given function; closing does nothing.
fakeRepl :: (Text -> Callback -> IO BlockResult) -> ReplFactory
fakeRepl run _ = pure (Right (ReplHandle run (pure ())))

-- | A block that understands these lines, one per block:
--
-- * @ANSWER text@: sets the final answer;
-- * @PRINT text@: prints the text;
-- * @ERROR@: an exception, in stderr;
-- * @LLM prompt@, @RLM prompt@: one call, printing the result;
-- * @LLMS a|b|c@, @RLMS a|b|c@: one batched call, printing the results one
--   per line.
--
-- A call keyword may name a model: @RLM\@kid prompt@. Anything else prints
-- nothing.
pyLike :: Text -> Callback -> IO BlockResult
pyLike code cb = case T.breakOn "@" word of
  (kw, m) -> go kw (if T.null m then Nothing else Just (T.drop 1 m))
  where
    (word, rest0) = T.breakOn " " code
    rest = T.drop 1 rest0
    out t = BlockResult t "" [] [] Nothing
    call kind prompts model = out . T.intercalate "\n" <$> cb kind prompts model
    go kw model = case kw of
      "ANSWER" -> pure (BlockResult "" "" [] [] (Just rest))
      "PRINT" -> pure (out rest)
      "ERROR" -> pure (BlockResult "" "\nValueError: boom" [] [] Nothing)
      "LLM" -> call CallLlm [rest] model
      "RLM" -> call CallRlm [rest] model
      "LLMS" -> call CallLlm (T.splitOn "|" rest) model
      "RLMS" -> call CallRlm (T.splitOn "|" rest) model
      _ -> pure (out "")

-- | A clock reading 0, then one step more at each reading.
steppingClock :: Double -> IO (IO Double)
steppingClock step = do
  ref <- newIORef 0
  pure (atomicModifyIORef' ref (\t -> (t + step, t)))

-- | A reply holding one repl block.
fenced :: Text -> Text
fenced code = "```repl\n" <> code <> "\n```"

-- | An environment on 'pyLike', a clock stuck at 0, and no iteration hook.
testEnv :: RlmConfig -> Model -> Model -> RlmEnv
testEnv cfg root sub =
  RlmEnv
    { reConfig = cfg
    , reRoot = root
    , reSub = sub
    , reRepl = fakeRepl pyLike
    , reClock = pure 0
    , reOnIteration = \_ _ -> pure ()
    }
