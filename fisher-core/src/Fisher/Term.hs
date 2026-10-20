-- | Upstream's terminal rendering (@cli/colors.py@): colour only on a
-- terminal that wants it, and an ASCII fallback for symbols when the output
-- encoding is not UTF-8.
module Fisher.Term
  ( Style (..)
  , plainStyle
  , detectStyle
  , Colour
  , reset, bold, dim, red, green, yellow, cyan
  , color
  , renderText
  ) where

import Data.Char (isAscii)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Normalize as N
import System.Environment (lookupEnv)
import System.IO (hGetEncoding, hIsTerminalDevice, stdout)


data Style = Style { styleAnsi :: Bool, styleUnicode :: Bool }
  deriving (Eq, Show)

-- | No colour, UTF-8: what upstream prints into a pipe or a test.
plainStyle :: Style
plainStyle = Style False True

-- | A variable read as a yes.
truthyEnv :: String -> IO Bool
truthyEnv name = maybe False ((`elem` ["1", "true", "yes", "on"]) . T.toLower . T.strip . T.pack) <$> lookupEnv name

detectStyle :: IO Style
detectStyle = do
  forcePlain <- truthyEnv "FISHER_FORCE_PLAIN_OUTPUT"
  noColor <- lookupEnv "NO_COLOR"
  tty <- hIsTerminalDevice stdout
  term <- maybe "" (T.toLower . T.strip . T.pack) <$> lookupEnv "TERM"
  forceAscii <- truthyEnv "FISHER_FORCE_ASCII"
  enc <- hGetEncoding stdout
  let ansi = not forcePlain && noColor == Nothing && tty && term /= "dumb"
      utf = maybe False (T.isInfixOf "utf" . T.toLower . T.pack . show) enc
  pure (Style ansi (not forceAscii && utf))

type Colour = Text

reset, bold, dim, red, green, yellow, cyan :: Colour
reset = "\ESC[0m"
bold = "\ESC[1m"
dim = "\ESC[2m"
red = "\ESC[31m"
green = "\ESC[32m"
yellow = "\ESC[33m"
cyan = "\ESC[36m"

asciiReplacements :: [(Text, Text)]
asciiReplacements =
  [ ("⚠️", "!"), ("⚠", "!"), ("✓", "OK"), ("✔", "OK"), ("✗", "X"), ("✘", "X"), ("⏳", "...")
  , ("⏱", "T"), ("⏭", ">>"), ("✨", "*"), ("⚡", "*"), ("⚕", "+"), ("◆", "*"), ("•", "*")
  , ("·", "|"), ("—", "-"), ("–", "-"), ("…", "..."), ("→", "->"), ("←", "<-"), ("↔", "<->")
  , ("╭", "+"), ("╮", "+"), ("╰", "+"), ("╯", "+"), ("┌", "+"), ("┐", "+"), ("└", "+")
  , ("┘", "+"), ("├", "+"), ("┤", "+"), ("┬", "+"), ("┴", "+"), ("┼", "+"), ("│", "|")
  , ("║", "|"), ("─", "-"), ("═", "="), ("█", "#"), ("∑", "S") ]

-- | @render_terminal_text@ with ANSI already decided: the Unicode-to-ASCII
-- step only.
renderText :: Style -> Text -> Text
renderText st t
  | styleUnicode st = t
  | otherwise =
      T.filter isAscii . N.normalize N.NFKD $
        foldl (\acc (from, to) -> T.replace from to acc) t asciiReplacements

-- | @color(text, *codes)@.
color :: Style -> [Colour] -> Text -> Text
color st codes t
  | styleAnsi st = T.concat codes <> renderText st t <> reset
  | otherwise = renderText st t
