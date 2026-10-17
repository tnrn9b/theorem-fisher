-- | 'Fisher.Py.Difflib' and 'Fisher.Py.Re' against CPython 3.11
-- (@test/golden/b2/difflib.json@ and @re.json@, from tools/corpus/b2.py):
-- ratios, matching blocks, opcodes, grouped opcodes and unified diffs; and
-- finditer, match and fullmatch for every pattern the ported code uses.
module Fisher.PyLibSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import qualified Fisher.Py.Difflib as D
import qualified Fisher.Py.Re as Re
import Fisher.Py.Value

load :: FilePath -> IO [PyVal]
load p = do
  doc <- either fail pure . loads =<< BS.readFile p
  pure $ case get "cases" doc of
    Just (PList cs) -> cs
    _ -> []

txt :: Text -> PyVal -> Text
txt k c = case get k c of
  Just (PStr s) -> s
  _ -> ""

strs :: Text -> PyVal -> [Text]
strs k c = case get k c of
  Just (PList xs) -> [s | PStr s <- xs]
  _ -> []

ints :: PyVal -> [Int]
ints (PList xs) = [fromIntegral n | PInt n <- xs]
ints _ = []

opcodeVal :: D.Opcode -> PyVal
opcodeVal (D.Opcode t a b c d) = PList [PStr (tagName t), PInt (fromIntegral a), PInt (fromIntegral b), PInt (fromIntegral c), PInt (fromIntegral d)]
  where
    tagName = \case
      D.TReplace -> "replace"
      D.TDelete -> "delete"
      D.TInsert -> "insert"
      D.TEqual -> "equal"

spec :: Spec
spec = do
  dcases <- runIO (load "test/golden/b2/difflib.json")
  describe "Fisher.Py.Difflib against CPython" $
    forM_ (zip [1 :: Int ..] dcases) $ \(i, c) -> it (show i <> " " <> T.unpack (txt "kind" c)) $
      case txt "kind" c of
        "chars" -> do
          let a = T.unpack (txt "a" c)
              b = T.unpack (txt "b" c)
          map (\(x, y, z) -> [x, y, z]) (D.matchingBlocks a b) `shouldBe` maybe [] (\case PList bs -> map ints bs; _ -> []) (get "blocks" c)
          PList (map opcodeVal (D.opcodes a b)) `shouldBe` maybe PNull id (get "opcodes" c)
          case get "ratio" c of
            Just (PFloat r) -> D.ratio a b `shouldBe` r
            Just (PInt r) -> D.ratio a b `shouldBe` fromIntegral r
            _ -> expectationFailure "no ratio"
        "lines" -> do
          let a = strs "a" c
              b = strs "b" c
          PList (map opcodeVal (D.opcodes a b)) `shouldBe` maybe PNull id (get "opcodes" c)
          PList (map (PList . map opcodeVal) (D.groupedOpcodes 3 a b)) `shouldBe` maybe PNull id (get "grouped" c)
          D.unifiedDiff a b "a/f.py" "b/f.py" `shouldBe` txt "diff" c
        _ -> D.splitLinesKeepEnds (txt "text" c) `shouldBe` strs "lines" c
  rcases <- runIO (load "test/golden/b2/re.json")
  describe "Fisher.Py.Re against CPython" $
    forM_ (zip [1 :: Int ..] rcases) $ \(i, c) -> it (show i <> " " <> T.unpack (txt "label" c) <> " " <> show (T.take 30 (txt "text" c))) $ do
      let flags = [Re.IgnoreCase | get "icase" c == Just (PBool True)]
      rx <- either fail pure (Re.compile flags (txt "pattern" c))
      let subject = txt "text" c
          render m = obj [("span", PList [PInt (fromIntegral (Re.mStart m)), PInt (fromIntegral (Re.mEnd m))]),
                          ("groups", PList [maybe PNull PStr (Re.group g m) | g <- [1 .. Re.groupCount rx]])]
      PList (map render (Re.findAll rx subject)) `shouldBe` maybe PNull id (get "finditer" c)
      maybe PNull render (Re.matchStart rx subject) `shouldBe` maybe PNull id (get "match" c)
      PBool (isJust (Re.fullMatch rx subject)) `shouldBe` maybe PNull id (get "fullmatch" c)
