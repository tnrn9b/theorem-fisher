-- | Recall over the RLM strategy's store: phase 1's recursive language model
-- run with the stored turns as its context and the agent's question as its
-- query.
module Fisher.Agent.Context.Recall
  ( RecallEnv (..)
  , RecallResult (..)
  , agentRlmConfig
  , recallSettings
  , recall
  , recallTexts
  , recallQuery
  ) where

import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)

import Fisher.Agent.Context.Rlm (StoredTurn, emptyStoreText, renderTurn)
import Fisher.Agent.Context.RlmModels (Models (..))
import Fisher.LLM.Normalize (Usage (..))
import Fisher.LLM.Types (Settings (..))
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..), rlmConfigFromPy)
import Fisher.RLM.Loop (completion)
import Fisher.RLM.Repl (ReplFactory)
import Fisher.RLM.Types

data RecallEnv = RecallEnv
  { reqConfig :: RlmConfig
  , reqModels :: Models
  , reqRepl :: ReplFactory
  , reqClock :: IO Double
  }

data RecallResult = RecallResult
  { rcText :: Text
  -- ^ the tool result
  , rcEnding :: Ending
  , rcIterations :: Int
  , rcCalls :: Int
  , rcPriced :: Int
  -- ^ calls whose usage reported a cost
  , rcDollars :: Double
  , rcCacheRead :: Integer
  , rcCacheWrite :: Integer
  }

-- | The recall configuration: the @fisher.rlm@ section of the config, with
-- the agent's defaults for what it leaves out (15 iterations, a budget of
-- 0.5 dollars, the agent's own model).
agentRlmConfig :: Text -> PyVal -> Either Text RlmConfig
agentRlmConfig model cfg = do
  c <- rlmConfigFromPy section
  pure
    c
      { rcMaxIterations = if given "max_iterations" then rcMaxIterations c else 15
      , rcMaxBudget = if given "max_budget" then rcMaxBudget c else Just 0.5
      , rcModel = if given "model" then rcModel c else model
      }
  where
    section = get "fisher" cfg >>= get "rlm"
    given k = case section of
      Just s -> isJust (get k s)
      Nothing -> False

-- | Recall's calls carry no tools and no output cap; the reasoning setting
-- is the agent's. (Settings given no other tool-choice field.)
recallSettings :: Settings -> Settings
recallSettings s = s {sTools = Nothing, sMaxTokens = Nothing}

recallQuery :: Text -> Text
recallQuery q =
  "Answer from the stored turns of an agent session. Quote exact text (code, errors, numbers, file paths) where it helps.\n\n" <> q

-- | What the counting wrapper has seen: calls, priced calls, dollars, cache
-- tokens read and written.
data Tally = Tally !Int !Int !Double !Integer !Integer

recall :: RecallEnv -> [StoredTurn] -> Text -> IO RecallResult
recall renv store q = recallTexts renv (map renderTurn store) q

-- | Recall over rendered texts, one per stored item.
recallTexts :: RecallEnv -> [Text] -> Text -> IO RecallResult
recallTexts _ [] _ = pure (RecallResult emptyStoreText Answered 0 0 0 0 0 0)
recallTexts renv store q = do
  tally <- newIORef (Tally 0 0 0 0 0)
  let count :: Model -> Model
      count model override msgs = do
        r <- model override msgs
        case r of
          Right rep -> do
            let u = rpUsage rep
            atomicModifyIORef' tally $ \(Tally n p d cr cw) ->
              (Tally (n + 1) (p + maybe 0 (const 1) (uCost u)) (d + fromMaybe 0 (uCost u)) (cr + fromMaybe 0 (uCacheRead u)) (cw + fromMaybe 0 (uCacheWrite u)), ())
          Left _ -> pure ()
        pure r
      env =
        RlmEnv
          { reConfig = reqConfig renv
          , reRoot = count (mRoot (reqModels renv))
          , reSub = count (mSub (reqModels renv))
          , reRepl = reqRepl renv
          , reClock = reqClock renv
          , reOnIteration = \_ _ -> pure ()
          }
  result <- completion env 0 (PList (map PStr store)) (Just (recallQuery q))
  Tally n p d cr cw <- readIORef tally
  let text = case rrEnding result of
        Stopped r -> rrAnswer result <> "\n\n(recall stopped: " <> r <> ")"
        _ -> rrAnswer result
  pure (RecallResult text (rrEnding result) (rrIterations result) n p d cr cw)
