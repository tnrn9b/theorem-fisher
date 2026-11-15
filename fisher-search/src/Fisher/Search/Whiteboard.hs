-- | The shared plan text the workers and the planner read, and how a worker's last words are taken.
module Fisher.Search.Whiteboard
  ( WorkerEnd (..)
  , workerEnd
  , whiteboard
  , workerBrief
  , usableLine
  ) where

import qualified Data.IntMap.Strict as IM
import Data.List (intersperse)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Role (PromptPart, partText, parts, source, static)
import Fisher.Search.Graph

data WorkerEnd = Note Text | FalseClaim Text deriving (Eq, Show)

-- | The child's final text, the fallback note.
workerEnd :: Maybe Text -> Text -> WorkerEnd
workerEnd Nothing fallback = Note fallback
workerEnd (Just t) fallback = case mapMaybe tagged (reverse (T.lines t)) of
  (e : _) -> e
  [] -> Note fallback
  where
    tagged l =
      let s = T.strip (T.filter (/= '\r') l)
      in case tag "NOTE:" s of
           Just r -> Just (Note (if T.null (T.strip r) then fallback else T.strip r))
           Nothing -> FalseClaim . T.strip <$> tag "FALSE:" s
    -- the tag bare or in markdown emphasis: **NOTE:**, *NOTE:*, `NOTE:`
    tag k s = listToMaybe [r | w <- ["**", "*", "`", ""], Just r <- [T.stripPrefix (w <> k <> w) s]]

capChars :: Int
capChars = 2000

-- | Graph, root, failure lines (oldest first). The heading and the tree's
-- marks are Fisher's, the node names the checked sketch's declarations
-- (source); each failure line carries its own provenance.
whiteboard :: Graph -> NodeId -> [PromptPart] -> PromptPart
whiteboard g root failures = fit (length failures)
  where
    name = maybe "" nName (IM.lookup root (gNodes g))
    tree = treeLines g root
    heading = static "# Plan for " <> source name
    render ts fs = linesOf $
      [heading] ++ ts ++ (if null fs then [] else static "" : static "## Failed" : fs)
    -- drop the oldest failures first (the length is measured on the text)
    fit k
      | len full <= capChars = full
      | k > 0 = fit (k - 1)
      | otherwise = cutTree (length tree)
      where full = render tree (drop (length failures - k) failures)
    -- the heading alone cut to the cap: its name cut, then "..."
    cutTree n
      | n <= 0 = static "# Plan for " <> source (T.take (capChars - 3 - T.length "# Plan for ") name) <> static "..."
      | len r <= capChars = r
      | otherwise = cutTree (n - 1)
      where r = render (take n tree ++ [static "..."]) []
    len = T.length . partText

-- | Parts with a newline between each two.
linesOf :: [PromptPart] -> PromptPart
linesOf = mconcat . intersperse (static "\n")

treeLines :: Graph -> NodeId -> [PromptPart]
treeLines g root = go [] 0 root
  where
    go seen d i
      | i `elem` seen = []
      | otherwise = case IM.lookup i (gNodes g) of
          Nothing -> []
          Just n ->
            (static (T.replicate (2 * d) " " <> "- ") <> source (nName n) <> static (" (" <> status (nStatus n) <> ")"))
              : concat [ go (i : seen) (d + 1) c
                       | sid <- nSplits n
                       , Just s <- [IM.lookup sid (gSplits g)]
                       , sStatus s `elem` [SAccepted, SDone]
                       , c <- sLemmas s ]
    status = \case
      NOpen -> "open"
      NProved -> "proved"
      NFailed _ -> "failed"

-- | Node name, its statement, the lemmas of the current plan it may use, the
-- whiteboard. The name, the statement and the lemma names are the file's
-- (source); the rest is Fisher's.
workerBrief :: Text -> Text -> [Text] -> PromptPart -> PromptPart
workerBrief name stmt usable board = mconcat $ map (<> static "\n")
  [ static "This is an unattended run: never stop to ask a question."
  , static "Prove exactly one declaration in the file: `" <> source name <> static "`."
  , static "Its statement is:"
  , static "```lean"
  , source (T.stripEnd stmt)
  , static "```"
  , usableLine usable
  , static "Do not edit any other declaration."
  , static ""
  , static "The plan so far:"
  -- the board with its trailing whitespace stripped ('parts' of one part)
  , parts [board]
  , static ""
  , static "When you finish, make the last line of your reply one of these:"
  , static "NOTE: <what you tried and why you stopped>"
  , static "FALSE: <a counterexample>"
  , static "Use the FALSE line only if the statement is false."
  ]

-- | Which lemmas still ending in @sorry@ a proof may use: only those of the current plan.
usableLine :: [Text] -> PromptPart
usableLine [] = static "Use no declaration that still ends in `sorry`."
usableLine ns =
  static "Besides what the file already proves, you may use only these lemmas of the current plan, even where they still end in `sorry`: "
    <> mconcat (intersperse (static ", ") [static "`" <> source n <> static "`" | n <- ns]) <> static ". Use no other declaration that still ends in `sorry`."

