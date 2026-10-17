-- | A unified diff of two texts as @diff -u@ prints it (the proof
-- canvas's hand back tells the agent what changed with it).
module Fisher.UI.Diff
  ( Op (..)
  , unifiedDiff
  , editScript
  ) where

import Data.Array (Array, listArray, (!))
import Data.Text (Text)
import qualified Data.Text as T

data Op = Keep | Del | Add
  deriving (Eq, Show)

-- | A unified diff (three lines of context) of two texts, as @diff -u@
-- prints it, both sides named PATH; empty when they are the same.
unifiedDiff :: FilePath -> Text -> Text -> Text
unifiedDiff path old new
  | old == new = ""
  | otherwise =
      T.unlines $
        ["--- " <> T.pack path, "+++ " <> T.pack path] <> concatMap hunk (groups (zip [0 :: Int ..] script))
  where
    a = T.lines old
    b = T.lines new
    script = editScript a b
    ctx = 3
    changed = [i | (i, (op, _)) <- zip [0 ..] script, op /= Keep]
    -- change positions closer than two contexts share a hunk
    groups xs = case changed of
      [] -> []
      (c : cs) -> map (\(lo, hi) -> [x | x@(i, _) <- xs, i >= max 0 (lo - ctx), i <= hi + ctx]) (spans c c cs)
    spans lo hi [] = [(lo, hi)]
    spans lo hi (c : cs)
      | c - hi <= 2 * ctx = spans lo c cs
      | otherwise = (lo, hi) : spans c c cs
    hunk xs =
      let first = fst (head xs)
          before = take first script
          oldStart = length [() | (op, _) <- before, op /= Add]
          newStart = length [() | (op, _) <- before, op /= Del]
          oldN = length [() | (_, (op, _)) <- xs, op /= Add]
          newN = length [() | (_, (op, _)) <- xs, op /= Del]
          range s n = T.pack (show (if n == 0 then s else s + 1)) <> (if n == 1 then "" else "," <> T.pack (show n))
       in ("@@ -" <> range oldStart oldN <> " +" <> range newStart newN <> " @@")
            : [mark op <> l | (_, (op, l)) <- xs]
    mark = \case
      Keep -> " "
      Del -> "-"
      Add -> "+"

-- | The lines kept, removed and added from @a@ to @b@: the common ends
-- kept, the middle by a longest common subsequence (when the middle is
-- more than a million line pairs, all of it removed and added).
editScript :: [Text] -> [Text] -> [(Op, Text)]
editScript a b =
  let pre = length (takeWhile id (zipWith (==) a b))
      a1 = drop pre a
      b1 = drop pre b
      suf = length (takeWhile id (zipWith (==) (reverse a1) (reverse b1)))
      am = take (length a1 - suf) a1
      bm = take (length b1 - suf) b1
   in map ((,) Keep) (take pre a) <> middle am bm <> map ((,) Keep) (drop (length a1 - suf) a1)
  where
    middle am bm
      | null am = map ((,) Add) bm
      | null bm = map ((,) Del) am
      | length am * length bm > 1000000 = map ((,) Del) am <> map ((,) Add) bm
      | otherwise = lcsScript am bm

lcsScript :: [Text] -> [Text] -> [(Op, Text)]
lcsScript as bs = walk 0 0
  where
    n = length as
    m = length bs
    av = listArray (0, n - 1) as :: Array Int Text
    bv = listArray (0, m - 1) bs :: Array Int Text
    -- table i j: the longest common subsequence of as[i..] and bs[j..]
    table :: Array (Int, Int) Int
    table = listArray ((0, 0), (n, m)) [cell i j | i <- [0 .. n], j <- [0 .. m]]
    cell i j
      | i == n || j == m = 0
      | av ! i == bv ! j = 1 + table ! (i + 1, j + 1)
      | otherwise = max (table ! (i + 1, j)) (table ! (i, j + 1))
    walk i j
      | i == n = [(Add, bv ! k) | k <- [j .. m - 1]]
      | j == m = [(Del, av ! k) | k <- [i .. n - 1]]
      | av ! i == bv ! j = (Keep, av ! i) : walk (i + 1) (j + 1)
      | table ! (i + 1, j) >= table ! (i, j + 1) = (Del, av ! i) : walk (i + 1) j
      | otherwise = (Add, bv ! j) : walk i (j + 1)
