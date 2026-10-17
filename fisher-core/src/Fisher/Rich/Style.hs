-- | Text styles as Rich names them (@"bold #FFD700"@, @"dim"@,
-- @"on red"@), and the SGR codes that draw them. Rich resolves a style it
-- cannot parse to no style at all; so does 'parseStyle'.
module Fisher.Rich.Style
  ( Style (..)
  , Color (..)
  , nullStyle
  , parseStyle
  , themeStyle
  , sgr
  , styled
  ) where

import Data.Bits (shiftR, (.&.))
import Data.Char (isHexDigit, digitToInt)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

data Color = RGB !Int !Int !Int | Palette !Int | DefaultColor
  deriving (Eq, Show)

data Style = Style
  { stBold, stDim, stItalic, stUnderline, stBlink, stReverse, stStrike :: !(Maybe Bool)
  , stFg, stBg :: !(Maybe Color)
  }
  deriving (Eq, Show)

nullStyle :: Style
nullStyle = Style Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Combination as Rich's @Style.__add__@: what the right side sets wins.
instance Semigroup Style where
  a <> b =
    Style (pick stBold) (pick stDim) (pick stItalic) (pick stUnderline) (pick stBlink) (pick stReverse)
      (pick stStrike) (pick stFg) (pick stBg)
    where
      pick f = maybe (f a) Just (f b)

instance Monoid Style where
  mempty = nullStyle

named :: M.Map Text Int
named = M.fromList (zip names [0 ..] <> zip (map ("bright_" <>) names) [8 ..])
  where
    names = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]

parseColor :: Text -> Maybe Color
parseColor w
  | w == "default" = Just DefaultColor
  | Just h <- T.stripPrefix "#" w, T.length h == 6, T.all isHexDigit h =
      let n = T.foldl' (\acc c -> acc * 16 + digitToInt c) 0 h
       in Just (RGB (n `shiftR` 16 .&. 255) (n `shiftR` 8 .&. 255) (n .&. 255))
  | Just r <- T.stripPrefix "color(" w, Just i <- T.stripSuffix ")" r, Just n <- readMaybe (T.unpack i), n >= 0, n < 256 =
      Just (Palette n)
  | Just r <- T.stripPrefix "rgb(" w, Just i <- T.stripSuffix ")" r
  , Just [a, b, c] <- mapM (readMaybe . T.unpack . T.strip) (T.splitOn "," i) =
      Just (RGB a b c)
  | otherwise = Palette <$> M.lookup w named

-- | @Style.parse@ for the words Fisher's output uses, plus the theme names
-- Rich gives tables (@table.header@, @table.title@). Anything unparseable
-- is no style, as Rich's @get_style(..., default=Style.null())@.
parseStyle :: Text -> Style
parseStyle raw = maybe nullStyle id (go nullStyle (T.words (T.toLower raw)))
  where
    go s [] = Just s
    go s ("not" : w : rest) = attr w (Just False) s >>= \s' -> go s' rest
    go s ("on" : w : rest) = parseColor w >>= \c -> go s {stBg = Just c} rest
    go s (w : rest)
      | Just t <- themeStyle w = go (s <> t) rest
      | Just s' <- attr w (Just True) s = go s' rest
      | Just c <- parseColor w = go s {stFg = Just c} rest
      | otherwise = Nothing
    attr w v s = case w of
      _ | w `elem` ["bold", "b"] -> Just s {stBold = v}
        | w `elem` ["dim", "d"] -> Just s {stDim = v}
        | w `elem` ["italic", "i"] -> Just s {stItalic = v}
        | w `elem` ["underline", "u"] -> Just s {stUnderline = v}
        | w == "blink" -> Just s {stBlink = v}
        | w `elem` ["reverse", "r"] -> Just s {stReverse = v}
        | w `elem` ["strike", "s"] -> Just s {stStrike = v}
        | otherwise -> Nothing

-- | The few theme styles Rich's tables refer to by name.
themeStyle :: Text -> Maybe Style
themeStyle = \case
  "table.header" -> Just nullStyle {stBold = Just True}
  "table.title" -> Just nullStyle {stItalic = Just True}
  "table.caption" -> Just nullStyle {stItalic = Just True, stDim = Just True}
  "none" -> Just nullStyle
  _ -> Nothing

-- | The SGR parameters for a style, empty when it sets nothing visible.
sgr :: Style -> String
sgr s =
  intercalate ";" . catMaybes $
    [ on stBold "1", on stDim "2", on stItalic "3", on stUnderline "4", on stBlink "5", on stReverse "7"
    , on stStrike "9", stFg s >>= colour 30 90 38, stBg s >>= colour 40 100 48 ]
  where
    on f code = if f s == Just True then Just code else Nothing
    colour :: Int -> Int -> Int -> Color -> Maybe String
    colour base bright ext = \case
      RGB r g b -> Just (show ext <> ";2;" <> show r <> ";" <> show g <> ";" <> show b)
      Palette n
        | n < 8 -> Just (show (base + n))
        | n < 16 -> Just (show (bright + n - 8))
        | otherwise -> Just (show ext <> ";5;" <> show n)
      DefaultColor -> Just (show (base + 9))

-- | Text wrapped in a style's escape codes, as Rich writes a segment.
styled :: Style -> Text -> Text
styled s t
  | null code || T.null t = t
  | otherwise = "\ESC[" <> T.pack code <> "m" <> t <> "\ESC[0m"
  where
    code = sgr s
