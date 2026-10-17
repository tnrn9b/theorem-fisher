-- | UTF-16 to code-point positions (leanclient @aio/convert.py@). LSP
-- columns count UTF-16 code units; the client API counts code points.
module Fisher.Lean.Utf16
  ( codepointToUtf16
  , utf16ToCodepoint
  , positionToUtf16
  , positionFromUtf16
  , rangeFromUtf16
  , lineAt
  ) where

import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Lean.Text (isAscii)
import Fisher.Py.Value

units :: Char -> Int
units ch = if ord ch > 0xFFFF then 2 else 1

-- | Column clamped to the line's length.
codepointToUtf16 :: Text -> Int -> Int
codepointToUtf16 line col0
  | isAscii line = col
  | otherwise = sum (map units (T.unpack (T.take col line)))
  where
    col = max 0 (min col0 (T.length line))

-- | Past the end clamps; inside a surrogate pair gives that character.
utf16ToCodepoint :: Text -> Int -> Int
utf16ToCodepoint line col
  | col <= 0 = 0
  | isAscii line = min col (T.length line)
  | otherwise = go 0 0 (T.unpack line)
  where
    go i u = \case
      [] -> T.length line
      (ch : rest)
        | u >= col -> i
        | u + units ch > col -> i
        | otherwise -> go (i + 1) (u + units ch) rest

lineAt :: [Text] -> Int -> Text
lineAt ls i = if i >= 0 && i < length ls then ls !! i else ""

positionToUtf16 :: [Text] -> Int -> Int -> PyVal
positionToUtf16 ls line col = PDict [("line", PInt (fromIntegral line)), ("character", PInt (fromIntegral (codepointToUtf16 (lineAt ls line) col)))]

intOf :: Maybe PyVal -> Int
intOf = \case
  Just (PInt n) -> fromIntegral n
  Just (PFloat d) -> truncate d
  Just (PBool b) -> if b then 1 else 0
  _ -> 0

positionFromUtf16 :: [Text] -> PyVal -> (Int, Int)
positionFromUtf16 ls pos =
  let line = intOf (get "line" pos)
   in (line, utf16ToCodepoint (lineAt ls line) (intOf (get "character" pos)))

-- | Both ends converted; keys in Python's order (start, end; line, character).
rangeFromUtf16 :: [Text] -> PyVal -> PyVal
rangeFromUtf16 ls rng =
  let (sl, sc) = positionFromUtf16 ls (maybe (PDict []) id (get "start" rng))
      (el, ec) = positionFromUtf16 ls (maybe (PDict []) id (get "end" rng))
      p l c = PDict [("line", PInt (fromIntegral l)), ("character", PInt (fromIntegral c))]
   in PDict [("start", p sl sc), ("end", p el ec)]
