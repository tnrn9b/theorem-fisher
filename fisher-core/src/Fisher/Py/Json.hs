-- | Python's @json.dumps@ (default separators, @ensure_ascii=False@) and
-- @repr(float)@, for output that has to match upstream byte for byte.
module Fisher.Py.Json
  ( PyVal (..)
  , dumps
  , pyFloatRepr
  , pyFormatFixed
  ) where

import Data.Char (intToDigit, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (floatToDigits, showHex)

data PyVal
  = PNull
  | PBool Bool
  | PInt Integer
  | PFloat Double
  | PStr Text
  | PList [PyVal]
  | PDict [(Text, PyVal)]
  deriving (Eq, Show)

dumps :: PyVal -> Text
dumps = \case
  PNull -> "null"
  PBool b -> if b then "true" else "false"
  PInt n -> T.pack (show n)
  PFloat d
    | isNaN d -> "NaN"
    | isInfinite d -> if d > 0 then "Infinity" else "-Infinity"
    | otherwise -> pyFloatRepr d
  PStr s -> quote s
  PList xs -> "[" <> T.intercalate ", " (map dumps xs) <> "]"
  PDict kvs -> "{" <> T.intercalate ", " [quote k <> ": " <> dumps v | (k, v) <- kvs] <> "}"

quote :: Text -> Text
quote s = "\"" <> T.concatMap esc s <> "\""
  where
    esc c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\b' -> "\\b"
      '\f' -> "\\f"
      _ | ord c < 0x20 -> "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (ord c) ""))
        | otherwise -> T.singleton c

-- | @repr(float)@: the shortest digits that read back, fixed notation from
-- 1e-4 up to 1e16, exponent notation outside it.
pyFloatRepr :: Double -> Text
pyFloatRepr d
  | d == 0 = if isNegativeZero d then "-0.0" else "0.0"
  | d < 0 = "-" <> pyFloatRepr (negate d)
  | otherwise =
      let (ds, e) = shortestDigits d
          digits = map intToDigit ds
          n = length digits
       in if e > -4 && e <= 16
            then
              if e <= 0
                then T.pack ("0." <> replicate (negate e) '0' <> digits)
                else
                  if e >= n
                    then T.pack (digits <> replicate (e - n) '0' <> ".0")
                    else T.pack (take e digits <> "." <> drop e digits)
            else
              let mant = case digits of
                    [x] -> [x]
                    (x : xs) -> x : '.' : xs
                    [] -> "0"
                  ex = e - 1
                  sign = if ex < 0 then "-" else "+"
                  expo = let a = show (abs ex) in if length a < 2 then '0' : a else a
               in T.pack (mant <> "e" <> sign <> expo)

-- | Python's shortest repr digits (dtoa mode 0): for the fewest
-- significant digits n, the correctly rounded n-digit decimal that reads
-- back as the same double. Returns (digits, e) with x = 0.d1d2... * 10^e.
shortestDigits :: Double -> ([Int], Int)
shortestDigits x = head [r | n <- [1 .. 17], Just r <- [attempt n]]
  where
    q = toRational x
    (_, e0) = floatToDigits 10 x
    attempt n =
      -- the exponent of the leading digit may be e0 or e0 - 1 after rounding
      let try ex =
            let scaled = q * 10 ^^ (n - ex)
                mm = roundHalfEven' scaled
             in if mm >= 10 ^ n then try' (ex + 1) else (mm, ex)
          try' ex = let scaled = q * 10 ^^ (n - ex) in (roundHalfEven' scaled, ex)
          (m, e) = try e0
          value = fromRational (fromInteger m * 10 ^^ (e - n)) :: Double
          ds = map (\c -> fromEnum c - 48) (show m)
          trimmed = reverse (dropWhile (== 0) (reverse ds))
       in if length (show m) == n && value == x then Just (if null trimmed then [0] else trimmed, e) else Nothing
    roundHalfEven' :: Rational -> Integer
    roundHalfEven' r =
      let f = floor r
          frac = r - fromInteger f
       in if frac > 1 / 2 then f + 1 else if frac < 1 / 2 then f else if even f then f else f + 1

-- | @f"{x:.Nf}"@: fixed point, rounding half to even on the binary value,
-- as Python's formatting does.
pyFormatFixed :: Int -> Double -> Text
pyFormatFixed places x =
  let scaled = toRational x * 10 ^ places
      r = roundHalfEven scaled
      neg = r < 0
      digits = show (abs r)
      padded = replicate (places + 1 - length digits) '0' <> digits
      (ip, fp) = splitAt (length padded - places) padded
      body = if places == 0 then ip else ip <> "." <> fp
   in T.pack ((if neg then "-" else "") <> body)
  where
    roundHalfEven :: Rational -> Integer
    roundHalfEven q =
      let f = floor q
          frac = q - fromInteger f
       in if frac > 1 / 2 then f + 1 else if frac < 1 / 2 then f else if even f then f else f + 1
