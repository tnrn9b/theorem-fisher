{-# LANGUAGE TemplateHaskell #-}
-- | Terminal cell widths, as Rich measures them (@rich/cells.py@, Rich
-- 15.0.0). The width table is Rich's own for its latest Unicode version,
-- taken by @tools/gen-defaults.py@ into @data/cell-widths.json@.
module Fisher.Rich.Cells
  ( charWidth
  , cellLen
  , setCellSize
  , chopCells
  , splitGraphemes
  , splitCellText
  , isPySpace
  ) where

import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Data.IntSet as IS
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

cellWidthsJson :: ByteString
cellWidthsJson = $(makeRelativeToProject "data/cell-widths.json" >>= embedFile)

data CellTable = CellTable
  { ctStarts :: !(U.Vector Int)
  , ctEnds :: !(U.Vector Int)
  , ctWidths :: !(U.Vector Int)
  , ctNarrowToWide :: !IS.IntSet
  }

cellTable :: CellTable
cellTable = fromMaybe (error "data/cell-widths.json is malformed") $ do
  Object o <- decodeStrict cellWidthsJson
  Array ws <- KM.lookup (K.fromString "widths") o
  Array nw <- KM.lookup (K.fromString "narrow_to_wide") o
  triples <- mapM triple (V.toList ws)
  wide <- mapM int (V.toList nw)
  let (s, e, w) = unzip3 triples
  pure (CellTable (U.fromList s) (U.fromList e) (U.fromList w) (IS.fromList wide))
  where
    int (Number n) = toBoundedInteger n
    int _ = Nothing
    triple (Array v) | [a, b, c] <- V.toList v = (,,) <$> int a <*> int b <*> int c
    triple _ = Nothing
{-# NOINLINE cellTable #-}

-- | @get_character_cell_size@: 0, 1 or 2 cells.
charWidth :: Char -> Int
charWidth ch
  | cp /= 0 && cp < 32 || (cp >= 0x7F && cp < 0xA0) = 0
  | cp > U.last (ctEnds t) = 1
  | otherwise = go 0 (U.length (ctStarts t) - 1)
  where
    cp = ord ch
    t = cellTable
    go lo hi
      | lo > hi = 1
      | otherwise =
          let i = (lo + hi) `div` 2
              s = ctStarts t U.! i
              e = ctEnds t U.! i
           in if cp < s then go lo (i - 1) else if cp > e then go (i + 1) hi else ctWidths t U.! i

-- | @_SINGLE_CELLS@: characters Rich knows are one cell wide without a lookup.
singleCell :: Char -> Bool
singleCell ch =
  let c = ord ch
   in (c >= 0x20 && c <= 0x7E) || (c >= 0xA0 && c <= 0xAC) || (c >= 0xAE && c <= 0x2FF)
        || (c >= 0x370 && c <= 0x482) || (c >= 0x2500 && c <= 0x25FC) || (c >= 0x2800 && c <= 0x28FF)

zwj, vs16 :: Char
zwj = '\x200d'
vs16 = '\xfe0f'

-- | @cell_len@.
cellLen :: Text -> Int
cellLen t
  | T.all singleCell t = T.length t
  | not (T.any (\c -> c == zwj || c == vs16) t) = T.foldl' (\n c -> n + charWidth c) 0 t
  | otherwise = go 0 Nothing (T.unpack t)
  where
    go total _ [] = total
    go total lastC (c : cs)
      | c == zwj = go total lastC (drop 1 cs)
      | c == vs16 = case lastC of
          Just l -> go (total + (if IS.member (ord l) (ctNarrowToWide cellTable) then 1 else 0)) Nothing cs
          Nothing -> go total Nothing cs
      | otherwise =
          let w = charWidth c
           in if w /= 0 then go (total + w) (Just c) cs else go total lastC cs

-- | @split_graphemes@: (start, end, cells) for each grapheme, covering the
-- text, and the total width.
splitGraphemes :: Text -> ([(Int, Int, Int)], Int)
splitGraphemes txt = go 0 Nothing 0 []
  where
    v = V.fromList (T.unpack txt)
    n = V.length v
    narrow c = IS.member (ord c) (ctNarrowToWide cellTable)
    go i lastC total acc
      | i >= n = (reverse acc, total)
      | otherwise =
          let c = v V.! i
           in if c == zwj || c == vs16
                then case acc of
                  [] -> go (i + 1) lastC total [(i, i + 1, 0)]
                  ((s, _, w) : rest)
                    | c == zwj ->
                        let i' = if i < n - 1 then i + 2 else i + 1
                         in go i' lastC total ((s, i', w) : rest)
                    | otherwise -> case lastC of
                        Just l | narrow l -> go (i + 1) Nothing (total + 1) ((s, i + 1, w + 1) : rest)
                        Just _ -> go (i + 1) lastC total ((s, i + 1, w) : rest)
                        Nothing -> go (i + 1) lastC total ((s, i + 1, w) : rest)
                else
                  let w = charWidth c
                   in if w /= 0
                        then go (i + 1) (Just c) (total + w) ((i, i + 1, w) : acc)
                        else case acc of
                          ((s, _, sw) : rest) -> go (i + 1) lastC total ((s, i + 1, sw) : rest)
                          [] -> go (i + 1) lastC total [(i, i + 1, 0)]

-- | @_split_text@: split at a cell position; a double-width character
-- straddling it becomes a space on each side.
splitCellText :: Text -> Int -> (Text, Text)
splitCellText txt pos
  | pos <= 0 = ("", txt)
  | T.all singleCell txt = T.splitAt pos txt
  | otherwise =
      let (spans, total) = splitGraphemes txt
          sv = V.fromList spans
          m = V.length sv
          cellOf (_, _, w) = w
          offset0 = floor ((fromIntegral pos / fromIntegral total :: Double) * fromIntegral m) :: Int
          left0 = sum (map cellOf (take offset0 spans))
          loop offset left
            | left == pos =
                if offset >= m then (txt, "") else let (s, _, _) = sv V.! offset in (T.take s txt, T.drop s txt)
            | left < pos =
                let (s, e, w) = sv V.! offset
                 in if left + w > pos then (T.take s txt <> " ", " " <> T.drop e txt) else loop (offset + 1) (left + w)
            | otherwise =
                let (s, e, w) = sv V.! (offset - 1)
                 in if left - w < pos then (T.take s txt <> " ", " " <> T.drop e txt) else loop (offset - 1) (left - w)
       in if total == 0 then (txt, "") else loop offset0 left0

-- | @set_cell_size@: crop or pad with spaces to exactly @total@ cells.
setCellSize :: Text -> Int -> Text
setCellSize txt total
  | T.all singleCell txt =
      let size = T.length txt
       in if size < total then txt <> T.replicate (total - size) " " else T.take (max 0 total) txt
  | total <= 0 = ""
  | otherwise =
      let size = cellLen txt
       in if size == total
            then txt
            else if size < total then txt <> T.replicate (total - size) " " else fst (splitCellText txt total)

-- | @chop_cells@: fold into lines of at most @width@ cells.
chopCells :: Text -> Int -> [Text]
chopCells txt width
  | T.all singleCell txt = if T.null txt then [] else T.chunksOf (max 1 width) txt
  | otherwise =
      let (spans, _) = splitGraphemes txt
          go [] _ size start acc = reverse (if size > 0 then T.drop start txt : acc else acc)
          go ((s, _, w) : rest) _ size start acc
            | size + w > width = go rest () w s (T.take (s - start) (T.drop start txt) : acc)
            | otherwise = go rest () (size + w) start acc
       in go spans () 0 0 []

-- | Python's @str.isspace@ for one character.
isPySpace :: Char -> Bool
isPySpace c =
  c `elem` ['\t', '\n', '\v', '\f', '\r', ' ', '\x85', '\xa0', '\x1680', '\x2028', '\x2029', '\x202f', '\x205f', '\x3000']
    || (c >= '\x1c' && c <= '\x1f')
    || (c >= '\x2000' && c <= '\x200a')
