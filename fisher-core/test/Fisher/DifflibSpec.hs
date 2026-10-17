-- | Fisher.Difflib against Python's difflib, on the random pairs
-- @tools/corpus/a3.py@ recorded (@dispatch.json@'s @ratios@).
module Fisher.DifflibSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), eitherDecodeFileStrict)
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (toRealFloat)
import qualified Data.Vector as V
import Test.Hspec

import Fisher.Difflib

spec :: Spec
spec = do
  pairs <- runIO $ do
    r <- eitherDecodeFileStrict "test/golden/a3/dispatch.json"
    case r of
      Right (Object o) | Just (Array rs) <- KM.lookup "ratios" o -> pure (V.toList rs)
      Right _ -> fail "dispatch.json: no ratios"
      Left e -> fail e
  describe "SequenceMatcher.ratio" $
    forM_ pairs $ \case
      Object p | Just (String a) <- KM.lookup "a" p, Just (String b) <- KM.lookup "b" p, Just (Number x) <- KM.lookup "ratio" p ->
        it (show a <> " / " <> show b) $ ratio a b `shouldBe` toRealFloat x
      _ -> pure ()
  describe "get_close_matches" $ do
    it "takes the best above the cutoff" $
      getCloseMatches "chta" ["chat", "start", "project", "help"] 1 0.75 `shouldBe` ["chat"]
    it "returns nothing below it" $
      getCloseMatches "zz" ["chat", "start", "project", "help"] 1 0.75 `shouldBe` []
