module Fisher.RLM.ParseSpec (spec) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:))
import Data.Text (Text)
import Fisher.RLM.Parse
import Test.Hspec

data Case = Case
  { cName :: Text
  , cText :: Text
  , cFind :: [Text]
  , cResults :: [BlockResult]
  , cMessages :: [Message]
  }

instance FromJSON Case where
  parseJSON = withObject "case" $ \o ->
    Case
      <$> o .: "name"
      <*> o .: "text"
      <*> o .: "find"
      <*> (map unR <$> o .: "results")
      <*> o .: "messages"

newtype R = R {unR :: BlockResult}

instance FromJSON R where
  parseJSON = withObject "result" $ \o ->
    (\a b c -> R (BlockResult a b c [] Nothing))
      <$> o .: "stdout"
      <*> o .: "stderr"
      <*> o .: "vars"

spec :: Spec
spec = describe "Fisher.RLM.Parse" $ do
  raw <- runIO (eitherDecodeFileStrict "test/golden/parse.json")
  cases <- either (\e -> runIO (fail ("parse.json: " ++ e))) pure raw
  mapM_
    ( \c -> describe (show (cName c)) $ do
        it "finds the same code blocks as the reference" $
          findCodeBlocks (cText c) `shouldBe` cFind c
        it "formats the iteration as the reference does" $
          formatIteration 20000 (cText c) (cResults c) `shouldBe` cMessages c
    )
    (cases :: [Case])
