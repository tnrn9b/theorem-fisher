{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- | Recall over a store: the empty store, the context handed to the REPL, the
-- query, the call tally, a stopped ending, the config defaults and the
-- settings.
module Fisher.Agent.RecallSpec (spec) where

import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Agent.Context.Recall
import Fisher.Agent.Context.Rlm
import Fisher.Agent.Context.RlmModels (Models (..))
import Fisher.LLM.Normalize (Usage (..))
import Fisher.LLM.Types (Settings (..), defaultSettings)
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..), defaultRlmConfig)
import Fisher.RLM.Parse (BlockResult (..))
import Fisher.RLM.Repl (ReplHandle (..))
import Fisher.RLM.Types

turn :: Int -> Text -> StoredTurn
turn i t = StoredTurn i (if odd i then "user" else "assistant") Nothing (obj [("role", PStr "user"), ("content", PStr t)])

store3 :: [StoredTurn]
store3 = [turn 1 "first", turn 2 "an error: E0042 in src/Main.hs", turn 3 "third"]

-- | A model that gives the same reply each time, recording what it was asked.
scripted :: IORef [[(Text, Text)]] -> Maybe Double -> Text -> Model
scripted seen cost txt _ msgs = do
  modifyIORef' seen (msgs :)
  pure (Right (Reply txt (Usage 10 5 15 (Just 3) (Just 2) cost) "scripted"))

-- | A REPL whose first block sets the final answer; it records its context.
replAnswering :: IORef (Maybe PyVal) -> Text -> ReplFactory'
replAnswering ctxRef ans ctx = do
  writeIORef ctxRef (Just ctx)
  pure (Right (ReplHandle (\_ _ -> pure (BlockResult "" "" [] [] (Just ans))) (pure ())))

type ReplFactory' = PyVal -> IO (Either Text ReplHandle)

mkEnv :: RlmConfig -> Model -> Model -> ReplFactory' -> RecallEnv
mkEnv cfg root sub repl = RecallEnv cfg (Models root sub) repl (pure 0)

spec :: Spec
spec = do
  describe "recall" $ do
    it "answers an empty store without a model call" $ do
      seen <- newIORef []
      r <- recall (mkEnv defaultRlmConfig (scripted seen Nothing "x") (scripted seen Nothing "x") (replAnswering undefined' "a")) [] "q"
      rcText r `shouldBe` emptyStoreText
      rcCalls r `shouldBe` 0
      readIORef seen >>= (`shouldBe` [])
    it "loads the rendered turns and asks the query of the root model" $ do
      seen <- newIORef []
      ctxRef <- newIORef Nothing
      let root = scripted seen (Just 0.25) "```repl\nFINAL('x')\n```"
      r <- recall (mkEnv defaultRlmConfig root root (replAnswering ctxRef "the fake answer")) store3 "where is the error?"
      readIORef ctxRef >>= (`shouldBe` Just (PList (map (PStr . renderTurn) store3)))
      msgs <- readIORef seen
      any (any (\(_, c) -> recallQuery "where is the error?" `T.isInfixOf` c)) msgs `shouldBe` True
      rcText r `shouldBe` "the fake answer"
      rcEnding r `shouldBe` Answered
      rcCalls r `shouldBe` 1
      rcPriced r `shouldBe` 1
      rcDollars r `shouldBe` 0.25
      rcCacheRead r `shouldBe` 3
      rcCacheWrite r `shouldBe` 2
    it "counts an unpriced call without a price" $ do
      seen <- newIORef []
      ctxRef <- newIORef Nothing
      let root = scripted seen Nothing "```repl\nFINAL('x')\n```"
      r <- recall (mkEnv defaultRlmConfig root root (replAnswering ctxRef "a")) store3 "q"
      (rcCalls r, rcPriced r, rcDollars r) `shouldBe` (1, 0, 0)
    it "appends the reason when the loop stops" $ do
      seen <- newIORef []
      ctxRef <- newIORef Nothing
      -- the first call costs 1.0, over a budget of 0.5, and the block gives no answer
      let root = scripted seen (Just 1.0) "```repl\nprint(1)\n```"
          repl ctx = do
            writeIORef ctxRef (Just ctx)
            pure (Right (ReplHandle (\_ _ -> pure (BlockResult "" "" [] [] Nothing)) (pure ())))
          cfg = defaultRlmConfig {rcMaxBudget = Just 0.5}
      r <- recall (mkEnv cfg root root repl) store3 "q"
      rcEnding r `shouldBe` Stopped "budget"
      ("\n\n(recall stopped: budget)" `T.isSuffixOf` rcText r) `shouldBe` True
  describe "agentRlmConfig" $ do
    it "defaults to 15 iterations, a 0.5 budget and the agent's model" $ do
      let Right c = agentRlmConfig "x/agent" (PDict [])
      (rcMaxIterations c, rcMaxBudget c, rcModel c) `shouldBe` (15, Just 0.5, "x/agent")
    it "takes max_iterations from fisher.rlm" $ do
      let Right c = agentRlmConfig "x/agent" (section [("max_iterations", PInt 4)])
      (rcMaxIterations c, rcMaxBudget c, rcModel c) `shouldBe` (4, Just 0.5, "x/agent")
    it "takes model and max_budget from fisher.rlm" $ do
      let Right c = agentRlmConfig "x/agent" (section [("model", PStr "y/m"), ("max_budget", PFloat 2)])
      (rcMaxIterations c, rcMaxBudget c, rcModel c) `shouldBe` (15, Just 2, "y/m")
    it "refuses a bad section" $
      agentRlmConfig "x/agent" (section [("bogus", PInt 1)]) `shouldSatisfy` either (const True) (const False)
  describe "recallSettings" $
    it "clears the tools and the output cap and keeps the reasoning" $ do
      let reasoning = Just (PDict [("enabled", PBool True)])
          s = defaultSettings {sTools = Just [PDict [("type", PStr "function")]], sMaxTokens = Just 100, sReasoning = reasoning}
          s' = recallSettings s
      sTools s' `shouldBe` Nothing
      sMaxTokens s' `shouldBe` Nothing
      sReasoning s' `shouldBe` reasoning
  where
    undefined' = error "unused"
    section kvs = PDict [("fisher", PDict [("rlm", PDict kvs)])]
