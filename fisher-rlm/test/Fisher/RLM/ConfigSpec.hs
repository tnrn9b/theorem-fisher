module Fisher.RLM.ConfigSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Fisher.Py.Value (PyVal (..))
import Fisher.RLM.Config
import Test.Hspec

leftText :: Either Text a -> Text
leftText = either id (const "")

spec :: Spec
spec = describe "Fisher.RLM.Config" $ do
  it "has the documented defaults" $ do
    defaultRlmConfig
      `shouldBe` RlmConfig
        { rcMaxDepth = 1
        , rcMaxIterations = 30
        , rcMaxConcurrent = 4
        , rcTruncate = 20000
        , rcBlockTimeout = 300
        , rcMaxBudget = Nothing
        , rcMaxTimeout = Nothing
        , rcMaxTokens = Nothing
        , rcMaxErrors = Nothing
        , rcModel = "anthropic/claude-sonnet-5.5"
        , rcSubModel = Nothing
        , rcMinOffload = 0.25
        , rcOffloadGap = 3
        }

  it "gives the defaults for a missing or empty section" $ do
    rlmConfigFromPy Nothing `shouldBe` Right defaultRlmConfig
    rlmConfigFromPy (Just (PDict [])) `shouldBe` Right defaultRlmConfig

  it "sets only the keys given" $ do
    rlmConfigFromPy (Just (PDict [("max_depth", PInt 2), ("sub_model", PStr "x")]))
      `shouldBe` Right defaultRlmConfig {rcMaxDepth = 2, rcSubModel = Just "x"}

  it "reads every key" $ do
    rlmConfigFromPy
      ( Just
          ( PDict
              [ ("max_depth", PInt 3)
              , ("max_iterations", PInt 10)
              , ("max_concurrent_subcalls", PInt 2)
              , ("truncate", PInt 500)
              , ("block_timeout", PInt 60)
              , ("max_budget", PFloat 1.5)
              , ("max_timeout", PInt 90)
              , ("max_tokens", PInt 100000)
              , ("max_errors", PInt 5)
              , ("model", PStr "m")
              , ("sub_model", PStr "s")
              , ("min_offload", PFloat 0.5)
              , ("offload_gap", PInt 0)
              ]
          )
      )
      `shouldBe` Right
        RlmConfig
          { rcMaxDepth = 3
          , rcMaxIterations = 10
          , rcMaxConcurrent = 2
          , rcTruncate = 500
          , rcBlockTimeout = 60
          , rcMaxBudget = Just 1.5
          , rcMaxTimeout = Just 90
          , rcMaxTokens = Just 100000
          , rcMaxErrors = Just 5
          , rcModel = "m"
          , rcSubModel = Just "s"
          , rcMinOffload = 0.5
          , rcOffloadGap = 0
          }

  it "refuses non-positive limits, naming the key" $ do
    leftText (rlmConfigFromPy (Just (PDict [("max_depth", PInt 0)]))) `shouldSatisfy` T.isInfixOf "max_depth"
    leftText (rlmConfigFromPy (Just (PDict [("truncate", PInt (-3))]))) `shouldSatisfy` T.isInfixOf "truncate"
    leftText (rlmConfigFromPy (Just (PDict [("max_budget", PFloat 0)]))) `shouldSatisfy` T.isInfixOf "max_budget"
    leftText (rlmConfigFromPy (Just (PDict [("max_timeout", PInt 0)]))) `shouldSatisfy` T.isInfixOf "max_timeout"
    leftText (rlmConfigFromPy (Just (PDict [("max_tokens", PInt 0)]))) `shouldSatisfy` T.isInfixOf "max_tokens"

  it "refuses values of the wrong type, naming the key" $ do
    leftText (rlmConfigFromPy (Just (PDict [("max_depth", PStr "2")]))) `shouldSatisfy` T.isInfixOf "max_depth"
    leftText (rlmConfigFromPy (Just (PDict [("max_depth", PFloat 2.5)]))) `shouldSatisfy` T.isInfixOf "max_depth"
    leftText (rlmConfigFromPy (Just (PDict [("model", PInt 1)]))) `shouldSatisfy` T.isInfixOf "model"
    rlmConfigFromPy (Just (PList [])) `shouldSatisfy` isLeft

  it "reads the agent's offload keys, min_offload in (0, 1] and offload_gap from 0" $ do
    rlmConfigFromPy (Just (PDict [("min_offload", PInt 1), ("offload_gap", PInt 7)]))
      `shouldBe` Right defaultRlmConfig {rcMinOffload = 1, rcOffloadGap = 7}
    let minErr = Left "fisher.rlm: min_offload must be a number in (0, 1]"
        gapErr = Left "fisher.rlm: offload_gap must be a non-negative integer"
    rlmConfigFromPy (Just (PDict [("min_offload", PInt 0)])) `shouldBe` minErr
    rlmConfigFromPy (Just (PDict [("min_offload", PFloat 1.5)])) `shouldBe` minErr
    rlmConfigFromPy (Just (PDict [("min_offload", PStr "x")])) `shouldBe` minErr
    rlmConfigFromPy (Just (PDict [("offload_gap", PInt (-1))])) `shouldBe` gapErr
    rlmConfigFromPy (Just (PDict [("offload_gap", PFloat 2.0)])) `shouldBe` gapErr

  it "refuses unknown keys by name" $
    rlmConfigFromPy (Just (PDict [("maxdepth", PInt 2)]))
      `shouldBe` Left "fisher.rlm: unknown key maxdepth"
