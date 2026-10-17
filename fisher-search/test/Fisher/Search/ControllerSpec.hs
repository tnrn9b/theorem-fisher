module Fisher.Search.ControllerSpec (spec) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as A
import Data.List (scanl')
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Fisher.Search.Controller

run :: StallRule -> [Signal] -> Maybe Stop
run rule = stallVerdict rule . foldl feed emptyProgress

-- | A stretch of work between two changes: few turns, little money.
data Block = Block Int [Int] deriving (Show)

instance Arbitrary Block where
  arbitrary = do
    turns <- choose (0, 7)
    cents <- listOf (choose (0, 70))
    pure (Block turns (takeWhileSum 140 cents))
    where
      takeWhileSum _ [] = []
      takeWhileSum room (c : cs)
        | c <= room = c : takeWhileSum (room - c) cs
        | otherwise = []

blockSignals :: Block -> [Signal]
blockSignals (Block t cs) = replicate t Turn ++ map (\c -> Spent (fromIntegral c / 100)) cs ++ [Changed]

row :: Int -> Text -> Value -> (Int, Text, Value)
row s k v = (s, k, v)

spec :: Spec
spec = do
  describe "stall rule" $ do
    it "stops after 16 turns with no change" $
      run defaultStallRule (replicate 16 Turn) `shouldBe` Just (Stop StallTurns 16 0)
    it "before the first change the limit is twice the rule" $ do
      run defaultStallRule (replicate 15 Turn) `shouldBe` Nothing
      run defaultStallRule (replicate 16 Turn) `shouldBe` Just (Stop StallTurns 16 0)
    it "after a change the limit is the rule" $
      run defaultStallRule ([Turn, Changed] ++ replicate 8 Turn) `shouldBe` Just (Stop StallTurns 8 0)
    it "does not stop when a change resets the count" $
      run defaultStallRule (replicate 7 Turn ++ [Changed] ++ replicate 7 Turn) `shouldBe` Nothing
    it "stops on dollars" $
      run defaultStallRule [Spent 0.80, Spent 0.80] `shouldBe` Just (Stop StallDollars 0 1.60)
    it "checks turns before dollars" $
      run defaultStallRule (replicate 16 Turn ++ [Spent 2]) `shouldBe` Just (Stop StallTurns 16 2)
    it "resets dollars on a change but keeps the total" $ do
      let p = foldl feed emptyProgress [Spent 1.00, Changed, Spent 1.00]
      stallVerdict defaultStallRule p `shouldBe` Nothing
      pgTotal p `shouldBe` 2.00
      pgDollars p `shouldBe` 1.00
    it "never stops on dollars when the rule has no dollar limit" $
      run (StallRule 8 Nothing) (replicate 7 Turn ++ [Spent 1000]) `shouldBe` Nothing
    it "never stops while a change comes at least every 7 turns and under $1.50" $
      property $ \bs ->
        let sigs = concatMap blockSignals (bs :: [Block])
            ps = scanl' feed emptyProgress sigs
        in all ((== Nothing) . stallVerdict defaultStallRule) ps

  describe "ledgerSignals" $ do
    it "reads turns and spend from cost rows and changes to the target" $
      ledgerSignals "/w/F.lean"
        [ row 1 "message" (object ["role" .= ("assistant" :: Text), "text" .= ("a" :: Text)])
        , row 2 "cost" (object ["dollars" .= (0.2 :: Double)])
        , row 3 "step" (object ["path" .= ("/w/G.lean" :: Text)])
        , row 4 "step" (object ["path" .= ("/w/F.lean" :: Text)])
        , row 5 "cost" (object ["dollars" .= A.Null])
        ]
        `shouldBe` [Turn, Spent 0.2, Changed, Turn]
    it "counts a cost row with no dollars as a turn and messages as nothing" $
      ledgerSignals "/w/F.lean"
        [ row 1 "message" (object ["role" .= ("user" :: Text)])
        , row 2 "message" (object ["role" .= ("assistant" :: Text)])
        , row 3 "cost" (object [])
        ]
        `shouldBe` [Turn]

  describe "lastAssistant" $
    it "returns the last assistant text" $
      lastAssistant
        [ row 1 "message" (object ["role" .= ("assistant" :: Text), "text" .= ("first" :: Text)])
        , row 2 "message" (object ["role" .= ("assistant" :: Text), "text" .= ("second" :: Text)])
        , row 3 "message" (object ["role" .= ("user" :: Text), "text" .= ("u" :: Text)])
        , row 4 "cost" (object ["dollars" .= (1 :: Double)])
        ]
        `shouldBe` Just "second"

  describe "classifyFailure" $ do
    it "recognises tool support errors" $
      classifyFailure "OpenRouter 404: No endpoints found that support tool use" `shouldBe` Just NoTools
    it "recognises rate limits" $
      classifyFailure "RateLimitError: 429" `shouldBe` Just RateLimited
    it "recognises unknown models" $
      classifyFailure "not a valid model ID" `shouldBe` Just Unavailable
    it "leaves other errors alone" $
      classifyFailure "unsolved goals" `shouldBe` Nothing
    it "has display text" $ do
      stopReasonText StallTurns `shouldBe` "turns"
      stopReasonText StallDollars `shouldBe` "dollars"
      map modelFailureText [Unavailable, NoTools, RateLimited]
        `shouldBe` ["unavailable model", "no tool support", "rate limited"]

  describe "noteResult" $ do
    it "skips at once on Unavailable and NoTools" $ do
      let (h1, s1) = noteResult 0 (Just Unavailable) emptyHealth
          (h2, s2) = noteResult 1 (Just NoTools) emptyHealth
      (s1, skipped h1 0, skipped h1 1) `shouldBe` (True, True, False)
      (s2, skipped h2 1, skipped h2 0) `shouldBe` (True, True, False)
    it "skips on the third rate limit in a row" $ do
      let (h1, s1) = noteResult 0 (Just RateLimited) emptyHealth
          (h2, s2) = noteResult 0 (Just RateLimited) h1
          (h3, s3) = noteResult 0 (Just RateLimited) h2
      [s1, s2, s3] `shouldBe` [False, False, True]
      map (skipped h3) [0, 1] `shouldBe` [True, False]
      skipped h2 0 `shouldBe` False
    it "resets the rate limit count on a success" $ do
      let step hh r = fst (noteResult 0 r hh)
          h = foldl step emptyHealth [Just RateLimited, Just RateLimited, Nothing, Just RateLimited, Just RateLimited]
      skipped h 0 `shouldBe` False

  describe "tierFor" $ do
    let skip i h = fst (noteResult i (Just Unavailable) h)
    it "starts at the step count, capped at the top tier" $ do
      tierFor 3 emptyHealth 0 `shouldBe` Just 0
      tierFor 3 emptyHealth 7 `shouldBe` Just 2
    it "moves up past a skipped tier" $
      tierFor 3 (skip 1 emptyHealth) 1 `shouldBe` Just 2
    it "falls back below when nothing above is left" $
      tierFor 3 (skip 2 emptyHealth) 5 `shouldBe` Just 1
    it "gives Nothing when every tier is skipped" $
      tierFor 3 (foldr skip emptyHealth [0, 1, 2]) 0 `shouldBe` Nothing

  describe "cap2000" $ do
    it "keeps the last 2000 characters of a long text" $ do
      let t = T.pack (replicate 500 'a' ++ replicate 2000 'b')
          out = cap2000 t
      T.takeEnd 2000 out `shouldBe` T.replicate 2000 "b"
      T.isPrefixOf "[...]\n" out `shouldBe` True
    it "leaves a short text alone" $
      cap2000 "short" `shouldBe` "short"
