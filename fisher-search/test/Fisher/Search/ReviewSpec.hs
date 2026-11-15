{-# LANGUAGE DataKinds #-}

module Fisher.Search.ReviewSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Fisher.Role (Role (..), Said, partText, saidText, static, toPrompt)
import Fisher.Role.Internal (said')
import Fisher.Search.Golden
import Fisher.Search.Review

-- | A verifier's reply, as the role monad's ask gives it.
verifier :: Text -> Said 'Verifier
verifier = said'

spec :: Spec
spec = do
  describe "reviewMessages" $
    it "golden" $ golden "review.txt" (render (reviewMessages "theorem main : P" (said' "a then b") "lemma a : A := by sorry\ntheorem main : P := by sorry" (static "# Plan for main")))
  describe "falseMessages" $
    it "golden" $ golden "false.txt" (render (falseMessages "lemma a : forall n : Nat, n > 0" "n = 0 is a counterexample"))
  describe "parseReview" $ do
    it "bare JSON" $
      parseReview (verifier "{\"accept\": true, \"reason\": \"ok\", \"suspect\": [\"a\"]}") `shouldBe` Right (SplitVerdict True (verifier "ok") ["a"])
    it "fenced JSON with prose" $
      parseReview (verifier "Verdict:\n```json\n{\"accept\": false}\n```\n") `shouldBe` Right (SplitVerdict False (verifier "") [])
    it "JSON in an untagged fence" $
      parseReview (verifier "```\n{\"accept\": true}\n```") `shouldBe` Right (SplitVerdict True (verifier "") [])
    it "JSON in a fence of another tag, after another block" $
      parseReview (verifier "```text\nnot it\n```\n```javascript\n{\"accept\": false, \"reason\": \"r\"}\n```") `shouldBe` Right (SplitVerdict False (verifier "r") [])
    it "JSON inside prose" $
      parseReview (verifier "I think {\"accept\": true, \"reason\": \"a {nested} brace\"} is right.") `shouldBe` Right (SplitVerdict True (verifier "a {nested} brace") [])
    it "a reason with an escaped quote is kept as the reply spells it (an excerpt of the reply)" $
      fmap (saidText . svReason) (parseReview (verifier "{\"accept\": false, \"reason\": \"lemma \\\"a\\\" is false\"}"))
        `shouldBe` Right "lemma \\\"a\\\" is false"
    it "missing accept" $ parseReview (verifier "{\"reason\": \"x\"}") `shouldBe` Left bad
    it "invalid JSON" $ parseReview (verifier "accept: yes") `shouldBe` Left bad
    it "wrong types" $ parseReview (verifier "{\"accept\": \"yes\"}") `shouldBe` Left bad
  describe "parseFalse" $ do
    it "bare JSON" $ parseFalse (verifier "{\"false\": true, \"reason\": \"n=0\"}") `shouldBe` Right (True, verifier "n=0")
    it "fenced JSON" $ parseFalse (verifier "```json\n{\"false\": false}\n```") `shouldBe` Right (False, verifier "")
    it "JSON in an untagged fence" $ parseFalse (verifier "Here:\n```\n{\"false\": true}\n```") `shouldBe` Right (True, verifier "")
    it "JSON inside prose" $ parseFalse (verifier "So {\"false\": false, \"reason\": \"no\"}.") `shouldBe` Right (False, verifier "no")
    it "missing key" $ parseFalse (verifier "{\"reason\": \"x\"}") `shouldBe` Left bad
    it "invalid JSON" $ parseFalse (verifier "nope") `shouldBe` Left bad
  describe "retryMessages" $ do
    it "adds the previous reply and the parse error" $
      texts (retryMessages [("system", static "s"), ("user", static "u")] (Just (toPrompt (verifier "prev"), "the verdict is not the JSON asked for")))
        `shouldBe` [ ("system", "s"), ("user", "u"), ("assistant", "prev")
                   , ("user", "Your reply could not be used: the verdict is not the JSON asked for\nReply with the JSON object only.") ]
    it "leaves the first call alone" $
      texts (retryMessages [("user", static "u")] Nothing) `shouldBe` [("user", "u")]
  where
    bad = "the verdict is not the JSON asked for"
    render ms = mconcat [ "[" <> r <> "]\n" <> partText c <> "\n" | (r, c) <- ms ]
    texts = map (fmap partText)
