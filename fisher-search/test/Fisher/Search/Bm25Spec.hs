module Fisher.Search.Bm25Spec (spec) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Fisher.Search.Bm25

-- Five documents; ids are 1 to 5.
docs :: [(Int, [Text])]
docs =
  [ (1, ["list", "append", "length"])
  , (2, ["list", "map", "append", "append"])
  , (3, ["nat", "add", "comm"])
  , (4, ["list", "length", "nat", "add", "comm", "assoc", "zero", "succ"])
  , (5, ["map", "filter"])
  ]

-- Reference implementation straight from the formula, k1 = 1.2, b = 0.75,
-- idf = ln (1 + (N - df + 0.5) / (df + 0.5)).
reference :: [(Int, [Text])] -> [Text] -> [(Int, Double)]
reference ds q =
  sortOn (\(i, s) -> (negate s, i)) [(i, s) | (i, ts) <- ds, let s = score ts, s > 0]
  where
    n = fromIntegral (length ds) :: Double
    avg = fromIntegral (sum (map (length . snd) ds)) / n
    score ts = sum [idf t * tfPart (fromIntegral (length (filter (== t) ts))) (fromIntegral (length ts)) | t <- uniq q]
    idf t = let df = fromIntegral (length (filter ((t `elem`) . snd) ds)) in log (1 + (n - df + 0.5) / (df + 0.5))
    tfPart f l = if f == 0 then 0 else f * 2.2 / (f + 1.2 * (1 - 0.75 + 0.75 * l / avg))
    uniq = foldr (\x acc -> x : filter (/= x) acc) []

spec :: Spec
spec = do
  describe "tokens" $ do
    it "splits on dots, underscores and case changes" $
      tokens "Step.Graft.runBody_append" `shouldBe` ["step", "graft", "run", "body", "append"]
    it "drops Lean keywords and 1-char tokens" $
      tokens "fun x => x ∧ True" `shouldBe` ["true"]
    it "keeps acronyms together" $
      tokens "HTTPServer" `shouldBe` ["http", "server"]
    it "splits IsOpen without dropping anything" $
      tokens "IsOpen" `shouldBe` ["is", "open"]
    it "keeps stop words that are components of a larger identifier" $ do
      tokens "PartialOrder" `shouldBe` ["partial", "order"]
      tokens "ByCases" `shouldBe` ["by", "cases"]
      tokens "Foo.fun" `shouldBe` ["foo", "fun"]
    it "does not drop tactic names" $ do
      tokens "And.intro" `shouldBe` ["and", "intro"]
      tokens "exact rfl" `shouldBe` ["exact", "rfl"]
    it "drops a standalone syntax word" $
      tokens "fun x => x" `shouldBe` []
    it "handles Unicode input stably" $ do
      let ins = ["ℝ≥0∞", "∀ ε > 0", "x₁ + α₂", "ℝ≥0∞ ∀ ε > 0 x₁ + α₂"]
      mapM_ (\i -> tokens i `shouldBe` tokens i) ins
      mapM_ (\i -> length (tokens i) `shouldSatisfy` (>= 0)) ins
    it "is empty for empty input" $
      tokens "" `shouldBe` []
  describe "rank" $ do
    let c = corpus docs
    it "orders a two-token query as the formula says" $ do
      let got = rank c ["list", "append"]
          want = reference docs ["list", "append"]
      map fst got `shouldBe` map fst want
      map fst got `shouldBe` [2, 1, 4]
      mapM_ (\((_, a), (_, b)) -> a `shouldSatisfy` (\x -> abs (x - b) < 1e-9)) (zip got want)
    it "returns nothing for a query that shares no token" $
      rank c ["zzz", "qqq"] `shouldBe` []
    it "returns nothing for an empty query or an empty corpus" $ do
      rank c [] `shouldBe` []
      rank (corpus []) ["list"] `shouldBe` []
    it "breaks ties by id" $ do
      let c2 = corpus [(7, ["a1", "b1"]), (3, ["a1", "b1"]), (5, ["a1", "b1"])]
      map fst (rank c2 ["a1"]) `shouldBe` [3, 5, 7]
    it "scores a term that is in every document above zero" $ do
      let c3 = corpus [(1, ["common", "aa"]), (2, ["common", "bb"]), (3, ["common"])]
      map fst (rank c3 ["common"]) `shouldBe` [3, 1, 2]
      map snd (rank c3 ["common"]) `shouldSatisfy` all (> 0)
    it "counts a repeated query token once" $
      rank c ["list", "list"] `shouldBe` rank c ["list"]
    it "adding an unrelated document keeps the order of two equal-length documents (one-token query)" $
      property $ \(Positive k1) (Positive k2) (Positive extra) ->
        let n1 = 1 + k1 `mod` 5 :: Int
            n2 = 1 + k2 `mod` 5 :: Int
            len = 6 :: Int
            mk i k = (i, replicate k "key" ++ [T.pack ("f" <> show j) | j <- [1 .. len - k]])
            base = [mk 1 n1, mk 2 n2, (3, ["other", "words"])]
            unrelated = (4, replicate (extra `mod` 9 + 1) "unrelated")
            order ds = map fst (filter ((`elem` [1, 2]) . fst) (rank (corpus ds) ["key"]))
        in order base === order (base ++ [unrelated])
