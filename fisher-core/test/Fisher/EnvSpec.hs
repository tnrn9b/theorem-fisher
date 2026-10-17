module Fisher.EnvSpec (spec) where

import qualified Data.Map.Strict as Map
import System.Environment (lookupEnv, unsetEnv)
import System.FilePath ((</>))
import System.Posix.Files (fileMode, getFileStatus, intersectFileModes)
import Test.Hspec

import Fisher.Env
import Fisher.TestUtil (withTmp)

spec :: Spec
spec = do
  -- Ported from TestSaveEnvValueSecure in tests/gauss_cli/test_config.py.
  it "saves a value that loads back" $ withTmp $ \tmp -> do
    saveEnvValueAt tmp "TENOR_API_KEY" "sk-test-secret"
    env <- loadEnvAt tmp
    Map.lookup "TENOR_API_KEY" env `shouldBe` Just "sk-test-secret"

  it "sets the variable in this process" $ withTmp $ \tmp -> do
    unsetEnv "FISHER_TEST_KEY"
    saveEnvValueAt tmp "FISHER_TEST_KEY" "v1"
    lookupEnv "FISHER_TEST_KEY" `shouldReturn` Just "v1"
    saveEnvValueAt tmp "FISHER_TEST_KEY" ""
    lookupEnv "FISHER_TEST_KEY" `shouldReturn` Just ""
    unsetEnv "FISHER_TEST_KEY"

  it "makes .env owner-only" $ withTmp $ \tmp -> do
    saveEnvValueAt tmp "TENOR_API_KEY" "x"
    st <- getFileStatus (tmp </> ".env")
    (fileMode st `intersectFileModes` 0o777) `shouldBe` 0o600

  it "refuses an invalid name" $ withTmp $ \tmp ->
    saveEnvValueAt tmp "1BAD" "x" `shouldThrow` (== InvalidEnvName "1BAD")

  describe "parseEnv" $ do
    it "skips blanks, comments and lines without =, and strips quotes" $
      parseEnv "# c\n\nNOEQ\nA=1\n B = \"two\" \nC='three'\nA=4\n"
        `shouldBe` Map.fromList [("A", "4"), ("B", "two"), ("C", "three")]

    it "reads CRLF files" $
      parseEnv "A=1\r\nB=2\r\n" `shouldBe` Map.fromList [("A", "1"), ("B", "2")]

  describe "updateEnvText" $ do
    it "replaces the first matching line in place" $
      updateEnvText "B" "9" "A=1\n  B=2\nC=3\nB=4\n" `shouldBe` "A=1\nB=9\nC=3\nB=4\n"

    it "appends, fixing a missing final newline" $
      updateEnvText "B" "2" "A=1" `shouldBe` "A=1\nB=2\n"

    it "starts an empty file" $
      updateEnvText "A" "1" "" `shouldBe` "A=1\n"

    it "does not match a key that only shares a prefix" $
      updateEnvText "A" "2" "AB=1\n" `shouldBe` "AB=1\nA=2\n"

  it "drops newlines from the value" $ withTmp $ \tmp -> do
    saveEnvValueAt tmp "K" "a\nb\rc"
    Map.lookup "K" <$> loadEnvAt tmp `shouldReturn` Just "abc"
