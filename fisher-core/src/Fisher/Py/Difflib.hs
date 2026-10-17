-- | Python 3.11's @difflib.SequenceMatcher@ (no junk function, autojunk on,
-- as every upstream caller constructs it) and @unified_diff@, over any
-- sequence of comparable elements: characters for similarity ratios,
-- lines for diffs. Element-for-element the same matching blocks, opcodes
-- and hunks as CPython, including the "popular element" purge that kicks
-- in once the second sequence reaches 200 elements.
module Fisher.Py.Difflib
  ( Opcode (..)
  , Tag (..)
  , matchingBlocks
  , opcodes
  , groupedOpcodes
  , ratio
  , ratioText
  , unifiedDiff
  , splitLinesKeepEnds
  ) where

import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

data Tag = TReplace | TDelete | TInsert | TEqual
  deriving (Eq, Show)

data Opcode = Opcode {opTag :: !Tag, opI1 :: !Int, opI2 :: !Int, opJ1 :: !Int, opJ2 :: !Int}
  deriving (Eq, Show)

data Matcher a = Matcher
  { mA :: V.Vector a
  , mB :: V.Vector a
  , _mB2j :: M.Map a [Int]
  }

newMatcher :: Ord a => [a] -> [a] -> Matcher a
newMatcher as bs =
  let b = V.fromList bs
      full = M.fromListWith (flip (<>)) [(e, [j]) | (j, e) <- zip [0 ..] bs]
      n = V.length b
      b2j
        | n >= 200 =
            let ntest = n `div` 100 + 1
                popular = S.fromList [e | (e, is) <- M.toList full, length is > ntest]
             in M.withoutKeys full popular
        | otherwise = full
   in Matcher (V.fromList as) b b2j

-- | @find_longest_match(alo, ahi, blo, bhi)@. The junk set is empty, so the
-- two junk-extension loops are no-ops; the non-junk extension loops still
-- run, and matter when popular elements were purged.
longest :: Ord a => Matcher a -> Int -> Int -> Int -> Int -> (Int, Int, Int)
longest (Matcher a b b2j) alo ahi blo bhi =
  let scan i j2len best
        | i >= ahi = best
        | otherwise =
            let js = takeWhile (< bhi) (dropWhile (< blo) (M.findWithDefault [] (a V.! i) b2j))
                step (m, bst@(_, _, bs)) j =
                  let k = IM.findWithDefault 0 (j - 1) j2len + 1
                      m' = IM.insert j k m
                   in if k > bs then (m', (i - k + 1, j - k + 1, k)) else (m', bst)
                (newJ2len, best') = foldl step (IM.empty, best) js
             in best' `seq` scan (i + 1) newJ2len best'
      (bi0, bj0, bs0) = scan alo IM.empty (alo, blo, 0)
      back (bi, bj, bs)
        | bi > alo && bj > blo && a V.! (bi - 1) == b V.! (bj - 1) = back (bi - 1, bj - 1, bs + 1)
        | otherwise = (bi, bj, bs)
      fwd (bi, bj, bs)
        | bi + bs < ahi && bj + bs < bhi && a V.! (bi + bs) == b V.! (bj + bs) = fwd (bi, bj, bs + 1)
        | otherwise = (bi, bj, bs)
   in fwd (back (bi0, bj0, bs0))

-- | @get_matching_blocks()@, including the closing @(len a, len b, 0)@.
matchingBlocks :: Ord a => [a] -> [a] -> [(Int, Int, Int)]
matchingBlocks as bs = matchingBlocksM (newMatcher as bs)

matchingBlocksM :: Ord a => Matcher a -> [(Int, Int, Int)]
matchingBlocksM m =
  let la = V.length (mA m)
      lb = V.length (mB m)
      go [] acc = acc
      go ((alo, ahi, blo, bhi) : queue) acc =
        let x@(i, j, k) = longest m alo ahi blo bhi
         in if k == 0
              then go queue acc
              else
                let left = [(alo, i, blo, j) | alo < i && blo < j]
                    right = [(i + k, ahi, j + k, bhi) | i + k < ahi && j + k < bhi]
                 in go (right <> left <> queue) (x : acc)
      blocks = sort (go [(0, la, 0, lb)] [])
      collapse (i1, j1, k1) [] = [(i1, j1, k1) | k1 /= 0]
      collapse (i1, j1, k1) ((i2, j2, k2) : rest)
        | i1 + k1 == i2 && j1 + k1 == j2 = collapse (i1, j1, k1 + k2) rest
        | otherwise = [(i1, j1, k1) | k1 /= 0] <> collapse (i2, j2, k2) rest
   in collapse (0, 0, 0) blocks <> [(la, lb, 0)]

-- | @get_opcodes()@.
opcodes :: Ord a => [a] -> [a] -> [Opcode]
opcodes as bs = opcodesFrom (matchingBlocks as bs)

opcodesFrom :: [(Int, Int, Int)] -> [Opcode]
opcodesFrom = go 0 0
  where
    go _ _ [] = []
    go i j ((ai, bj, size) : rest) =
      let tag
            | i < ai && j < bj = Just TReplace
            | i < ai = Just TDelete
            | j < bj = Just TInsert
            | otherwise = Nothing
          pre = maybe [] (\t -> [Opcode t i ai j bj]) tag
          i' = ai + size
          j' = bj + size
          eq = [Opcode TEqual ai i' bj j' | size /= 0]
       in pre <> eq <> go i' j' rest

-- | @get_grouped_opcodes(n)@.
groupedOpcodes :: Ord a => Int -> [a] -> [a] -> [[Opcode]]
groupedOpcodes n as bs =
  let codes0 = case opcodes as bs of
        [] -> [Opcode TEqual 0 1 0 1]
        cs -> cs
      trimFirst = \case
        (Opcode TEqual i1 i2 j1 j2 : rest) -> Opcode TEqual (max i1 (i2 - n)) i2 (max j1 (j2 - n)) j2 : rest
        cs -> cs
      trimLast cs = case reverse cs of
        (Opcode TEqual i1 i2 j1 j2 : rest) -> reverse (Opcode TEqual i1 (min i2 (i1 + n)) j1 (min j2 (j1 + n)) : rest)
        _ -> cs
      codes = trimLast (trimFirst codes0)
      nn = n + n
      go [] grp = [reverse grp | not (null grp), not (isLoneEqual grp)]
      go (Opcode tag i1 i2 j1 j2 : rest) grp
        | tag == TEqual && i2 - i1 > nn =
            let closed = reverse (Opcode tag i1 (min i2 (i1 + n)) j1 (min j2 (j1 + n)) : grp)
                i1' = max i1 (i2 - n)
                j1' = max j1 (j2 - n)
             in closed : go rest [Opcode tag i1' i2 j1' j2]
        | otherwise = go rest (Opcode tag i1 i2 j1 j2 : grp)
      isLoneEqual [Opcode TEqual _ _ _ _] = True
      isLoneEqual _ = False
   in go codes []

-- | @ratio()@: 2*M/T, or 1.0 for two empty sequences.
ratio :: Ord a => [a] -> [a] -> Double
ratio as bs =
  let matches = sum [k | (_, _, k) <- matchingBlocks as bs]
      total = length as + length bs
   in if total == 0 then 1.0 else 2.0 * fromIntegral matches / fromIntegral total

-- | 'ratio' over the characters of two strings.
ratioText :: Text -> Text -> Double
ratioText a b = ratio (T.unpack a) (T.unpack b)

-- | Python's @str.splitlines(keepends=True)@ restricted to what upstream's
-- diff inputs contain: splits after @\\n@, @\\r\\n@, @\\r@ and the other
-- Unicode line boundaries Python recognises.
splitLinesKeepEnds :: Text -> [Text]
splitLinesKeepEnds t
  | T.null t = []
  | otherwise =
      let (line, rest) = T.break isBoundary t
       in case T.uncons rest of
            Nothing -> [line]
            Just ('\r', r) | Just ('\n', r') <- T.uncons r -> (line <> "\r\n") : splitLinesKeepEnds r'
            Just (c, r) -> T.snoc line c : splitLinesKeepEnds r
  where
    isBoundary c = c `elem` ("\n\r\v\f\x1c\x1d\x1e\x85\x2028\x2029" :: String)

-- | @''.join(difflib.unified_diff(a, b, fromfile, tofile))@ with the
-- default context of 3 and @lineterm='\\n'@.
unifiedDiff :: [Text] -> [Text] -> Text -> Text -> Text
unifiedDiff a b fromfile tofile =
  case groupedOpcodes 3 a b of
    [] -> ""
    groups ->
      T.concat $
        ("--- " <> fromfile <> "\n")
          : ("+++ " <> tofile <> "\n")
          : concatMap hunk groups
  where
    va = V.fromList a
    vb = V.fromList b
    slice v i j = V.toList (V.slice i (j - i) v)
    hunk grp =
      let first = head grp
          lst = last grp
          r1 = range (opI1 first) (opI2 lst)
          r2 = range (opJ1 first) (opJ2 lst)
       in ("@@ -" <> r1 <> " +" <> r2 <> " @@\n") : concatMap body grp
    body (Opcode tag i1 i2 j1 j2) = case tag of
      TEqual -> map (" " <>) (slice va i1 i2)
      TReplace -> map ("-" <>) (slice va i1 i2) <> map ("+" <>) (slice vb j1 j2)
      TDelete -> map ("-" <>) (slice va i1 i2)
      TInsert -> map ("+" <>) (slice vb j1 j2)
    range start stop =
      let beginning = start + 1
          len = stop - start
       in if len == 1
            then tshow beginning
            else tshow (if len == 0 then beginning - 1 else beginning) <> "," <> tshow len
    tshow = T.pack . show
