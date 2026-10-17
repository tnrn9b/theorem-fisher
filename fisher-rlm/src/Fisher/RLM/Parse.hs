-- | Finding code blocks in a model reply and formatting a turn's REPL output
-- for the next prompt. Ported from rlm/utils/parsing.py
-- (alexzhang13/rlm at d04208afbad29ca675ab13478c40ee8bebc84bfe, MIT; see
-- NOTICE).
module Fisher.RLM.Parse
  ( Message
  , BlockResult (..)
  , findCodeBlocks
  , formatIteration
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | A chat message: (role, content).
type Message = (Text, Text)

-- | What running one code block produced.
data BlockResult = BlockResult
  { brStdout :: Text
  , brStderr :: Text
  , brVars :: [Text]
  -- ^ names the reference would list under "REPL variables"
  , brTypes :: [Text]
  -- ^ the type name of each of 'brVars', in order (for the log)
  , brFinal :: Maybe Text
  -- ^ a final answer set by the block; not used when formatting
  }
  deriving (Eq, Show)

-- | Python's @str.isspace@, which is also what @\\s@ and @strip@ use on a
-- Python string.
isPySpace :: Char -> Bool
isPySpace c =
  c `elem` ("\t\n\v\f\r \x1c\x1d\x1e\x1f\x85\xa0\x1680\x2028\x2029\x202f\x205f\x3000" :: String)
    || (c >= '\x2000' && c <= '\x200a')

pyStrip :: Text -> Text
pyStrip = T.dropAround isPySpace

newlineIndices :: Text -> [Int]
newlineIndices w = [i | (i, c) <- zip [0 ..] (T.unpack w), c == '\n']

-- | The same blocks as the reference's pattern @```repl\\s*\\n(.*?)\\n```@
-- with dot matching newlines: each body is stripped.
--
-- After the tag, @\\s*@ takes the whole whitespace run and backs off until a
-- newline follows, so the body may start after any newline in that run, the
-- last one first. The body then ends at the first newline followed by three
-- backticks.
findCodeBlocks :: Text -> [Text]
findCodeBlocks t = case T.breakOn "```repl" t of
  (_, rest) | T.null rest -> []
  (_, rest) ->
    let afterTag = T.drop 7 rest
        (ws, _) = T.span isPySpace afterTag
        starts = [T.drop (i + 1) afterTag | i <- reverse (newlineIndices ws)]
     in case firstJust (map closeAt starts) of
          Just (body, next) -> pyStrip body : findCodeBlocks next
          Nothing -> findCodeBlocks afterTag
  where
    closeAt s = case T.breakOn "\n```" s of
      (_, r) | T.null r -> Nothing
      (body, r) -> Just (body, T.drop 4 r)
    firstJust xs = case [x | Just x <- xs] of
      (x : _) -> Just x
      [] -> Nothing

-- | The messages one turn adds to the history: the assistant reply, then,
-- if any block ran, one user message joining each block's output (each cut
-- to the given number of characters).
formatIteration :: Int -> Text -> [BlockResult] -> [Message]
formatIteration limit response results =
  ("assistant", response)
    : [("user", T.intercalate "\n\n" parts) | not (null parts)]
  where
    multi = length results > 1
    parts = zipWith part [1 :: Int ..] results
    part i r =
      let out = truncateTo limit (formatExecutionResult r)
          header
            | multi = "REPL output (block " <> T.pack (show i) <> "):"
            | otherwise = "REPL output:"
       in header <> "\n" <> out

truncateTo :: Int -> Text -> Text
truncateTo limit s
  | n > limit = T.take limit s <> "... + [" <> T.pack (show (n - limit)) <> " chars...]"
  | otherwise = s
  where
    n = T.length s

formatExecutionResult :: BlockResult -> Text
formatExecutionResult r
  | null parts = "No output"
  | otherwise = T.intercalate "\n\n" parts
  where
    parts =
      [ "\n" <> brStdout r | not (T.null (brStdout r))
      ]
        ++ ["\n" <> brStderr r | not (T.null (brStderr r))]
        ++ ["REPL variables: " <> pyList (brVars r) <> "\n" | not (null (brVars r))]
    pyList names = "[" <> T.intercalate ", " (map quote names) <> "]"
    quote n = "'" <> n <> "'"
