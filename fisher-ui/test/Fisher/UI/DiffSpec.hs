-- | The unified diff a hand back sends the agent.
module Fisher.UI.DiffSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Fisher.UI.Diff (unifiedDiff)

spec :: Spec
spec = describe "Fisher.UI.Diff" $ do
  describe "unifiedDiff" $ do
    it "prints a change as diff -u does" $
      unifiedDiff "P" "a\nb\nc\n" "a\nB\nc\n" `shouldBe` "--- P\n+++ P\n@@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n"
    it "keeps three lines of context and splits far changes into hunks" $ do
      let old = T.unlines (map (T.pack . show) [1 .. 20 :: Int])
          new = T.replace "\n2\n" "\ntwo\n" (T.replace "\n18\n" "\neighteen\n" old)
      unifiedDiff "P" old new
        `shouldBe` T.unlines
          [ "--- P", "+++ P", "@@ -1,5 +1,5 @@", " 1", "-2", "+two", " 3", " 4", " 5"
          , "@@ -15,6 +15,6 @@", " 15", " 16", " 17", "-18", "+eighteen", " 19", " 20" ]
    it "adds to an empty file and is empty for no change" $ do
      unifiedDiff "P" "" "x\ny\n" `shouldBe` "--- P\n+++ P\n@@ -0,0 +1,2 @@\n+x\n+y\n"
      unifiedDiff "P" "same\n" "same\n" `shouldBe` ""
