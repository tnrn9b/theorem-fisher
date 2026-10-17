module Fisher.Lean.WatchSpec (spec) where

import Data.List (sort)
import Test.Hspec

import Fisher.Lean.Watch

spec :: Spec
spec = describe "the project file watcher's batching" $ do
  it "gathers changes that arrive within a step and sends each once" $ do
    b <- newBatcher
    mapM_ (pushChange b) [(2, "/p/A.lean"), (2, "/p/A.lean"), (1, "/p/B.ilean")]
    sort <$> nextBatch b 200 50 `shouldReturn` [(1, "/p/B.ilean"), (2, "/p/A.lean")]
  it "watches only .lean and .ilean files" $
    map watchedFile ["/p/A.lean", "/p/.lake/build/A.ilean", "/p/A.olean", "/p/lakefile.toml"] `shouldBe` [True, True, False, False]
