-- | 'Fisher.Py.Value' against Python itself (@pyvalue.json@: json.loads,
-- json.dumps with and without ensure_ascii and indent, repr and str of 508 values).
module Fisher.PyValueSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Fisher.Py.Value

spec :: Spec
spec = do
  doc <- runIO (either fail pure . loads =<< BS.readFile "test/golden/b1/pyvalue.json")
  let cases = case get "cases" doc of
        Just (PList cs) -> cs
        _ -> []
  describe "Fisher.Py.Value against Python" $
    forM_ (zip [1 :: Int ..] cases) $ \(i, c) -> it (show i <> ": " <> T.unpack (T.take 50 (txt "input" c))) $ do
      v <- either fail pure (loads (TE.encodeUtf8 (txt "input" c)))
      dumpsAscii v `shouldBe` txt "ascii" c
      dumpsUnicode v `shouldBe` txt "unicode" c
      pyRepr v `shouldBe` txt "repr" c
      pyStrOf v `shouldBe` txt "str" c
      dumpsIndent True 2 v `shouldBe` txt "indent2" c
      dumpsIndent False 2 v `shouldBe` txt "indent2u" c
  where
    txt k c = case get k c of
      Just (PStr s) -> s
      _ -> ""
