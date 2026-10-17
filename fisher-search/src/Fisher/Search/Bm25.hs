-- | Identifier tokenizer and BM25 ranking, used to retrieve premises for a goal.
module Fisher.Search.Bm25
  ( tokens
  , Corpus
  , corpus
  , rank
  ) where

import Data.Char (isAlphaNum, isLower, isUpper)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

-- | Split identifiers and Lean text into lower-case word tokens.  Splits on every
-- character that is not a letter or digit (so '.', '_' and symbols separate), and
-- at case changes (@runBody@ gives @run@, @body@; @HTTPServer@ gives @http@, @server@).
-- A whole standalone word (a run of letters, digits, '.', '_' and quotes between other
-- characters) that is a statement syntax word is dropped; components split out of a
-- larger identifier are kept.  1-character tokens are dropped.  Duplicates are kept.
tokens :: Text -> [Text]
tokens = filter ((> 1) . T.length) . map T.toLower . concatMap pieces . T.split (not . isWordChar)
  where
    isWordChar c = isAlphaNum c || c == '.' || c == '_' || c == '\''
    pieces w
      | S.member (T.toLower w) keywords = []
      | otherwise = concatMap camel (T.split (not . isAlphaNum) w)

camel :: Text -> [Text]
camel w = map T.pack (go (T.unpack w))
  where
    go [] = []
    go s = let (a, r) = cut s in a : go r
    cut [] = ([], [])
    cut (c : cs)
      | isUpper c =
          let (u, rest) = span isUpper cs
          in case rest of
               (n : _) | not (null u), isLower n ->
                 -- acronym then a capitalised word: the last capital starts the next word
                 (c : init u, last u : rest)
               _ | null u -> let (l, r) = break isUpper cs in (c : l, r)
                 | otherwise -> (c : u, rest)
      | otherwise = let (l, r) = break isUpper cs in (c : l, r)

-- | Statement syntax words only; tactic names are ordinary vocabulary.
keywords :: Set Text
keywords = S.fromList
  [ "fun", "by", "have", "show", "let", "in", "at", "with", "then", "else", "if"
  , "match", "calc", "from", "do", "where", "sorry" ]

-- | A BM25 index over documents (id, tokens), built once.
data Corpus = Corpus
  { cN :: !Int
  , cAvg :: !Double
  , cLens :: !(M.Map Int Int)
  , cPostings :: !(M.Map Text [(Int, Int)])  -- term -> (doc id, term frequency)
  }

corpus :: [(Int, [Text])] -> Corpus
corpus ds = Corpus
  { cN = length ds
  , cAvg = if null ds then 0 else fromIntegral (sum lens) / fromIntegral (length ds)
  , cLens = M.fromList (zip (map fst ds) lens)
  , cPostings = M.fromListWith (++)
      [ (t, [(i, f)])
      | (i, ts) <- ds
      , (t, f) <- M.toList (M.fromListWith (+) [(t', 1 :: Int) | t' <- ts]) ]
  }
  where lens = map (length . snd) ds

k1, b :: Double
k1 = 1.2
b = 0.75

-- | Ids with a positive score, best first, ties by id.  Repeated query tokens count once.
rank :: Corpus -> [Text] -> [(Int, Double)]
rank c q = sortOn (\(i, s) -> (negate s, i)) (filter ((> 0) . snd) (M.toList scores))
  where
    scores = M.fromListWith (+)
      [ (i, idf * (fromIntegral f * (k1 + 1)) / (fromIntegral f + k1 * (1 - b + b * len i / avg)))
      | t <- S.toList (S.fromList q)
      , Just ps <- [M.lookup t (cPostings c)]
      , let df = fromIntegral (length ps) :: Double
            n = fromIntegral (cN c) :: Double
            idf = log (1 + (n - df + 0.5) / (df + 0.5))
      , (i, f) <- ps
      ]
    avg = if cAvg c <= 0 then 1 else cAvg c
    len i = maybe 0 fromIntegral (M.lookup i (cLens c))
