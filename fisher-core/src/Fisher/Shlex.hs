-- | @shlex.split@ in POSIX mode, the way @/project@ arguments are split:
-- whitespace separates words, single quotes are literal, double quotes let
-- a backslash escape only @"@ and @\\@, and outside quotes a backslash
-- escapes any character.
module Fisher.Shlex (shlexSplit) where

import Data.Text (Text)
import qualified Data.Text as T

shlexSplit :: Text -> Either Text [Text]
shlexSplit = go [] Nothing . T.unpack
  where
    -- acc: finished words, newest first; cur: the word being built, if any.
    go acc cur [] = Right (reverse (maybe acc (\w -> T.pack (reverse w) : acc) cur))
    go acc cur (c : cs)
      | c `elem` [' ', '\t', '\n', '\r'] = go (maybe acc (\w -> T.pack (reverse w) : acc) cur) Nothing cs
      | c == '\\' = case cs of
          (d : rest) -> go acc (Just (d : word cur)) rest
          [] -> Left "No escaped character"
      | c == '\'' = case break (== '\'') cs of
          (inside, _ : rest) -> go acc (Just (reverse inside <> word cur)) rest
          (_, []) -> Left "No closing quotation"
      | c == '"' = dquote acc (word cur) cs
      | otherwise = go acc (Just (c : word cur)) cs
    dquote _ _ [] = Left "No closing quotation"
    dquote acc w ('"' : rest) = go acc (Just w) rest
    dquote acc w ('\\' : d : rest)
      | d == '"' || d == '\\' = dquote acc (d : w) rest
      | otherwise = dquote acc (d : '\\' : w) rest
    dquote _ _ ['\\'] = Left "No closing quotation"
    dquote acc w (d : rest) = dquote acc (d : w) rest
    word = maybe [] id
