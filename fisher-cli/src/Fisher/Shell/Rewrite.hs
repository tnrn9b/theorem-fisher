-- | Upstream's forgiving input (@cli/commands.py@): onboarding words
-- typed without a slash, and near misses of the onboarding commands.
module Fisher.Shell.Rewrite
  ( rewriteFriendlyEntry
  , rewriteFriendlySlash
  , normalizeEntryToken
  , pyStrip
  , pySplit1
  ) where

import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Difflib (getCloseMatches)
import Fisher.Rich.Cells (isPySpace)

entryAliases :: [(Text, Text)]
entryAliases = [("chat", "/chat"), ("start", "/chat"), ("begin", "/chat"), ("project", "/project"), ("help", "/help")]

entryPhrases :: [Text]
entryPhrases =
  [ "get started", "getting started", "how do i start", "how do i get started", "what do i do first"
  , "what should i do first", "where do i start" ]

fuzzyTargets :: [(Text, Text)]
fuzzyTargets = [("chat", "/chat"), ("start", "/chat"), ("project", "/project"), ("help", "/help")]

-- | @_normalize_entry_token@: lower case, letters a-z only.
normalizeEntryToken :: Text -> Text
normalizeEntryToken = T.filter (\c -> c >= 'a' && c <= 'z') . T.map toLower

pyStrip :: Text -> Text
pyStrip = T.dropAround isPySpace

-- | @text.split(maxsplit=1)@.
pySplit1 :: Text -> [Text]
pySplit1 t =
  let s = T.dropWhile isPySpace t
      (a, rest) = T.break isPySpace s
      b = T.dropWhile isPySpace rest
   in if T.null s then [] else if T.null b then [a] else [a, b]

withRemainder :: Text -> Text -> Text
withRemainder c r = if T.null r then c else c <> " " <> r

fuzzy :: Text -> Maybe Text
fuzzy token = case getCloseMatches token (map fst fuzzyTargets) 1 0.75 of
  (m : _) -> lookup m fuzzyTargets
  [] -> Nothing

-- | @rewrite_friendly_entry_command@.
rewriteFriendlyEntry :: Text -> Maybe Text
rewriteFriendlyEntry command
  | T.null text || "/" `T.isPrefixOf` text = Nothing
  | T.unwords (T.split isPySpace (T.toLower text) `without` "") `elem` entryPhrases = Just "/chat"
  | otherwise =
      let parts = pySplit1 text
          token = normalizeEntryToken (head parts)
          remainder = if length parts > 1 then pyStrip (parts !! 1) else ""
       in case lookup token entryAliases of
            Just d -> Just (withRemainder d remainder)
            Nothing -> (`withRemainder` remainder) <$> fuzzy token
  where
    text = pyStrip command
    without xs x = filter (/= x) xs

-- | @rewrite_friendly_slash_command@, against the shell's command table.
rewriteFriendlySlash :: [Text] -> Text -> Maybe Text
rewriteFriendlySlash commands command
  | not ("/" `T.isPrefixOf` text) = Nothing
  | token == "/start" || token `elem` commands = Nothing
  | otherwise = (`withRemainder` remainder) <$> fuzzy (normalizeEntryToken (T.dropWhile (== '/') token))
  where
    text = pyStrip command
    parts = pySplit1 text
    token = T.toLower (pyStrip (head parts))
    remainder = if length parts > 1 then pyStrip (parts !! 1) else ""
