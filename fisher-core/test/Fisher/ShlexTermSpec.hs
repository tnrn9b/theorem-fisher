module Fisher.ShlexTermSpec (spec) where

import Test.Hspec

import Fisher.Shlex
import Fisher.Term

spec :: Spec
spec = do
  describe "shlexSplit" $ do
    it "splits on whitespace and honours both quote kinds" $
      shlexSplit "/project create ../X --name \"Generated Project\" --t 'a b'"
        `shouldBe` Right ["/project", "create", "../X", "--name", "Generated Project", "--t", "a b"]
    it "keeps an empty quoted word" $
      shlexSplit "a '' b" `shouldBe` Right ["a", "", "b"]
    it "joins quoted and bare parts of one word" $
      shlexSplit "pre\"mid\"'post'" `shouldBe` Right ["premidpost"]
    it "escapes outside quotes, and only \" and \\ inside double quotes" $
      shlexSplit "a\\ b \"c\\\"d\\e\"" `shouldBe` Right ["a b", "c\"d\\e"]
    it "reports an unclosed quote and a trailing backslash as Python does" $ do
      shlexSplit "use \"unterminated" `shouldBe` Left "No closing quotation"
      shlexSplit "use 'x" `shouldBe` Left "No closing quotation"
      shlexSplit "x\\" `shouldBe` Left "No escaped character"

  describe "color" $ do
    it "adds codes only when ANSI is on" $ do
      color (Style True True) [green] "ok" `shouldBe` "\ESC[32mok\ESC[0m"
      color plainStyle [green] "ok" `shouldBe` "ok"
    it "falls back to ASCII when the output is not UTF-8" $
      color (Style False False) [] "◆ ✓ — → café" `shouldBe` "* OK - -> cafe"
