-- | 'Fisher.Py.JsonError' and 'pyRepr' against CPython 3.11
-- (@test/golden/b3/pyjson.json@ and @pyrepr.json@, from
-- tools/corpus/b3.py): the message @json.loads@ raises for text that does
-- not parse, and @str(dict)@ of messages (whose length drives the agent's
-- token estimates).
module Fisher.PyJsonSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Test.Hspec

import Fisher.Py.JsonError (jsonError)
import Fisher.Py.Value

load :: FilePath -> IO [PyVal]
load p = do
  doc <- either fail pure . loads =<< BS.readFile p
  pure $ case get "cases" doc of
    Just (PList cs) -> cs
    _ -> []

txt :: T.Text -> PyVal -> T.Text
txt k c = case get k c of
  Just (PStr s) -> s
  _ -> ""

spec :: Spec
spec = do
  jcases <- runIO (load "test/golden/b3/pyjson.json")
  describe "json.loads error messages" $
    forM_ (zip [1 :: Int ..] jcases) $ \(i, c) -> it (show i) $ do
      let t = txt "text" c
          want = case get "error" c of
            Just (PStr e) -> Just e
            _ -> Nothing
      jsonError t `shouldBe` want
  rcases <- runIO (load "test/golden/b3/pyrepr.json")
  describe "str(dict)" $
    forM_ (zip [1 :: Int ..] rcases) $ \(i, c) -> it (show i) $ do
      let v = maybe PNull id (get "value" c)
          want = txt "str" c
      pyRepr v `shouldBe` want
      fromIntegral (T.length (pyRepr v)) `shouldBe` (case get "len" c of Just (PInt n) -> n; _ -> -1)
