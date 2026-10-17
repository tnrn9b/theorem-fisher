module Fisher.RLM.PromptSpec (spec) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:), (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Fisher.Py.Value (PyVal, loads)
import Fisher.RLM.Parse (Message)
import Fisher.RLM.Prompt
import Test.Hspec

-- | A context as the golden stores it: JSON text, or a repeated character
-- (to keep the million-character case out of the file).
data Ctx = Ctx (Maybe Text) (Maybe (Text, Int))

instance FromJSON Ctx where
  parseJSON = withObject "ctx" $ \o -> Ctx <$> o .:? "contextJson" <*> o .:? "contextRepeat"

ctxValue :: Ctx -> Either String PyVal
ctxValue (Ctx (Just j) _) = loads (TE.encodeUtf8 j)
ctxValue (Ctx _ (Just (c, n))) = loads (TE.encodeUtf8 ("\"" <> T.replicate n c <> "\""))
ctxValue _ = Left "context missing"

data Shape = Shape {shName :: Text, shCtx :: Ctx, shType :: Text, shLen :: Int}

instance FromJSON Shape where
  parseJSON v = withObject "shape" (\o -> Shape <$> o .: "name" <*> parseJSON v <*> o .: "type" <*> o .: "length") v

data Initial = Initial {inName :: Text, inCtx :: Ctx, inRoot :: Maybe Text, inMessages :: [Message]}

instance FromJSON Initial where
  parseJSON v = withObject "initial" (\o -> Initial <$> o .: "name" <*> parseJSON v <*> o .: "root" <*> o .: "messages") v

data Turn = Turn {tIter :: Int, tMax :: Int, tMessage :: Message}

instance FromJSON Turn where
  parseJSON = withObject "turn" $ \o -> Turn <$> o .: "iteration" <*> o .: "maxIterations" <*> o .: "message"

data Golden = Golden
  { gShapes :: [Shape]
  , gInitial :: [Initial]
  , gTurns :: [Turn]
  , gDefault :: Message
  }

instance FromJSON Golden where
  parseJSON = withObject "golden" $ \o ->
    Golden <$> o .: "shapes" <*> o .: "initial" <*> o .: "turns" <*> o .: "default"

shapeOf :: Ctx -> ContextShape
shapeOf c = either error contextShape (ctxValue c)

spec :: Spec
spec = describe "Fisher.RLM.Prompt" $ do
  raw <- runIO (eitherDecodeFileStrict "test/golden/prompt.json")
  g <- either (\e -> runIO (fail ("prompt.json: " ++ e))) pure raw
  describe "contextShape" $
    mapM_
      ( \s -> it ("measures " ++ show (shName s) ++ " as the reference does") $
          shapeOf (shCtx s) `shouldBe` ContextShape (shType s) (shLen s)
      )
      (gShapes g)
  describe "initialMessages" $
    mapM_
      ( \c -> it ("builds the reference messages for " ++ show (inName c) ++ ", root " ++ show (inRoot c)) $
          initialMessages (shapeOf (inCtx c)) (inRoot c) `shouldBe` inMessages c
      )
      (gInitial g)
  describe "turnMessage" $
    mapM_
      ( \t -> it ("builds the reference message for iteration " ++ show (tIter t) ++ " of " ++ show (tMax t)) $
          turnMessage (tIter t) (tMax t) `shouldBe` tMessage t
      )
      (gTurns g)
  it "defaultAnswerMessage is the reference's" $
    defaultAnswerMessage `shouldBe` gDefault g
