module Fisher.Search.ReviewSpec (spec) where

import Test.Hspec

import Fisher.Search.Golden
import Fisher.Search.Review

spec :: Spec
spec = do
  describe "reviewMessages" $
    it "golden" $ golden "review.txt" (render (reviewMessages "theorem main : P" "a then b" "lemma a : A := by sorry\ntheorem main : P := by sorry" "# Plan for main"))
  describe "falseMessages" $
    it "golden" $ golden "false.txt" (render (falseMessages "lemma a : forall n : Nat, n > 0" "n = 0 is a counterexample"))
  describe "parseReview" $ do
    it "bare JSON" $
      parseReview "{\"accept\": true, \"reason\": \"ok\", \"suspect\": [\"a\"]}" `shouldBe` Right (SplitVerdict True "ok" ["a"])
    it "fenced JSON with prose" $
      parseReview "Verdict:\n```json\n{\"accept\": false}\n```\n" `shouldBe` Right (SplitVerdict False "" [])
    it "JSON in an untagged fence" $
      parseReview "```\n{\"accept\": true}\n```" `shouldBe` Right (SplitVerdict True "" [])
    it "JSON in a fence of another tag, after another block" $
      parseReview "```text\nnot it\n```\n```javascript\n{\"accept\": false, \"reason\": \"r\"}\n```" `shouldBe` Right (SplitVerdict False "r" [])
    it "JSON inside prose" $
      parseReview "I think {\"accept\": true, \"reason\": \"a {nested} brace\"} is right." `shouldBe` Right (SplitVerdict True "a {nested} brace" [])
    it "missing accept" $ parseReview "{\"reason\": \"x\"}" `shouldBe` Left bad
    it "invalid JSON" $ parseReview "accept: yes" `shouldBe` Left bad
    it "wrong types" $ parseReview "{\"accept\": \"yes\"}" `shouldBe` Left bad
  describe "parseFalse" $ do
    it "bare JSON" $ parseFalse "{\"false\": true, \"reason\": \"n=0\"}" `shouldBe` Right (True, "n=0")
    it "fenced JSON" $ parseFalse "```json\n{\"false\": false}\n```" `shouldBe` Right (False, "")
    it "JSON in an untagged fence" $ parseFalse "Here:\n```\n{\"false\": true}\n```" `shouldBe` Right (True, "")
    it "JSON inside prose" $ parseFalse "So {\"false\": false, \"reason\": \"no\"}." `shouldBe` Right (False, "no")
    it "missing key" $ parseFalse "{\"reason\": \"x\"}" `shouldBe` Left bad
    it "invalid JSON" $ parseFalse "nope" `shouldBe` Left bad
  describe "retryMessages" $ do
    it "adds the previous reply and the parse error" $
      retryMessages [("system", "s"), ("user", "u")] (Just ("prev", "the verdict is not the JSON asked for"))
        `shouldBe` [ ("system", "s"), ("user", "u"), ("assistant", "prev")
                   , ("user", "Your reply could not be used: the verdict is not the JSON asked for\nReply with the JSON object only.") ]
    it "leaves the first call alone" $
      retryMessages [("user", "u")] Nothing `shouldBe` [("user", "u")]
  where
    bad = "the verdict is not the JSON asked for"
    render ms = mconcat [ "[" <> r <> "]\n" <> c <> "\n" | (r, c) <- ms ]
