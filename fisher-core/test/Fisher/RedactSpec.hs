module Fisher.RedactSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Fisher.Redact

spec :: Spec
spec = do
  it "masks an API key assignment" $
    redactSensitiveText "OPENROUTER_API_KEY=sk-or-v1-abcdef0123456789abcdef"
      `shouldSatisfy` (not . T.isInfixOf "abcdef0123456789")

  it "leaves plain text alone" $
    redactSensitiveText "plain text" `shouldBe` "plain text"

  it "keeps the first 6 and last 4 characters of a long token" $
    maskToken "sk-or-v1-abcdef0123456789abcdef" `shouldBe` "sk-or-...cdef"

  -- An env-assignment or telegram pattern tried from every position of a
  -- long run of letters or digits backtracks quadratically; the patterns
  -- are anchored at the start of a run so a 64 KB tool result stays cheap.
  it "takes linear time on long runs of letters and digits" $ do
    T.length (redactSensitiveText (T.replicate 100000 "x")) `shouldBe` 100000
    T.length (redactSensitiveText (T.replicate 100000 "7")) `shouldBe` 100000
    T.length (redactSensitiveText (T.replicate 20000 "token")) `shouldBe` 100000

  it "still masks an assignment inside a longer name" $
    redactSensitiveText "MY_GITHUB_TOKEN=ghx0123456789abcdefghij" `shouldBe` "MY_GITHUB_TOKEN=ghx012...ghij"

  it "still masks a telegram bot token" $
    redactSensitiveText "bot123456789:ABCdefGHIjklMNOpqrSTUvwxYZ0123456789"
      `shouldBe` "bot123456789:***"
