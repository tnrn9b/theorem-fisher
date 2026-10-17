-- | Python's @difflib.SequenceMatcher.ratio@ and @get_close_matches@, for
-- strings, as upstream uses them to forgive typed commands. No junk
-- function, and the autojunk heuristic only applies from 200 characters,
-- which no command reaches; neither is ported.
module Fisher.Difflib
  ( ratio
  , matchingBlocks
  , getCloseMatches
  ) where

import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing, Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

-- | @find_longest_match@ over a[alo:ahi], b[blo:bhi]: the longest block,
-- earliest in a, then earliest in b.
longestMatch :: V.Vector Char -> M.Map Char [Int] -> Int -> Int -> Int -> Int -> (Int, Int, Int)
longestMatch a b2j alo ahi blo bhi = go alo M.empty (alo, blo, 0)
  where
    go i j2len best@(_, _, bestSize)
      | i >= ahi = best
      | otherwise =
          let js = takeWhile (< bhi) (dropWhile (< blo) (M.findWithDefault [] (a V.! i) b2j))
              step (m, bst@(_, _, bs)) j =
                let k = M.findWithDefault 0 (j - 1) j2len + 1
                    m' = M.insert j k m
                 in if k > bs then (m', (i - k + 1, j - k + 1, k)) else (m', bst)
              (newJ2len, best') = foldl step (M.empty, best) js
           in bestSize `seq` go (i + 1) newJ2len best'

-- | @get_matching_blocks@, without the closing sentinel.
matchingBlocks :: Text -> Text -> [(Int, Int, Int)]
matchingBlocks ta tb =
  let a = V.fromList (T.unpack ta)
      b = V.fromList (T.unpack tb)
      b2j = M.fromListWith (flip (<>)) [(c, [j]) | (j, c) <- zip [0 ..] (V.toList b)]
      go [] acc = acc
      go ((alo, ahi, blo, bhi) : queue) acc =
        let (i, j, k) = longestMatch a b2j alo ahi blo bhi
         in if k == 0
              then go queue acc
              else
                let left = [(alo, i, blo, j) | alo < i && blo < j]
                    right = [(i + k, ahi, j + k, bhi) | i + k < ahi && j + k < bhi]
                 in go (left <> right <> queue) ((i, j, k) : acc)
      raw = sortBy (comparing (\(i, j, _) -> (i, j))) (go [(0, V.length a, 0, V.length b)] [])
   in collapse raw
  where
    -- adjacent blocks are joined, as difflib does
    collapse ((i1, j1, k1) : (i2, j2, k2) : rest)
      | i1 + k1 == i2 && j1 + k1 == j2 = collapse ((i1, j1, k1 + k2) : rest)
    collapse (x : rest) = x : collapse rest
    collapse [] = []

-- | @SequenceMatcher(None, a, b).ratio()@.
ratio :: Text -> Text -> Double
ratio a b =
  let total = T.length a + T.length b
      matches = sum [k | (_, _, k) <- matchingBlocks a b]
   in if total == 0 then 1.0 else 2.0 * fromIntegral matches / fromIntegral total

-- | @get_close_matches(word, possibilities, n, cutoff)@: the best @n@ at or
-- above the cutoff, highest score first, ties to the greater string.
getCloseMatches :: Text -> [Text] -> Int -> Double -> [Text]
getCloseMatches word possibilities n cutoff =
  let scored = [(ratio x word, x) | x <- possibilities, ratio x word >= cutoff]
   in map snd (take n (sortBy (comparing Down) scored))
