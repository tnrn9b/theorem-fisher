-- | The shared plan text the workers and the planner read, and how a worker's last words are taken.
module Fisher.Search.Whiteboard
  ( WorkerEnd (..)
  , workerEnd
  , whiteboard
  , workerBrief
  , usableLine
  ) where

import qualified Data.IntMap.Strict as IM
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

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

-- | Graph, root, failure lines (oldest first).
whiteboard :: Graph -> NodeId -> [Text] -> Text
whiteboard g root failures = fit (length failures)
  where
    name = maybe "" nName (IM.lookup root (gNodes g))
    tree = treeLines g root
    render ts fs = T.intercalate "\n" $
      ["# Plan for " <> name] ++ ts ++ (if null fs then [] else "" : "## Failed" : fs)
    -- drop the oldest failures first
    fit k
      | T.length full <= capChars = full
      | k > 0 = fit (k - 1)
      | otherwise = cutTree (length tree)
      where full = render tree (drop (length failures - k) failures)
    cutTree n
      | n <= 0 = T.take (capChars - 3) (render [] []) <> "..."
      | T.length r <= capChars = r
      | otherwise = cutTree (n - 1)
      where r = render (take n tree ++ ["..."]) []

treeLines :: Graph -> NodeId -> [Text]
treeLines g root = go [] 0 root
  where
    go seen d i
      | i `elem` seen = []
      | otherwise = case IM.lookup i (gNodes g) of
          Nothing -> []
          Just n ->
            (T.replicate (2 * d) " " <> "- " <> nName n <> " (" <> status (nStatus n) <> ")")
              : concat [ go (i : seen) (d + 1) c
                       | sid <- nSplits n
                       , Just s <- [IM.lookup sid (gSplits g)]
                       , sStatus s `elem` [SAccepted, SDone]
                       , c <- sLemmas s ]
    status = \case
      NOpen -> "open"
      NProved -> "proved"
      NFailed _ -> "failed"

-- | Node name, its statement, the lemmas of the current plan it may use, the whiteboard.
workerBrief :: Text -> Text -> [Text] -> Text -> Text
workerBrief name stmt usable board = T.unlines
  [ "This is an unattended run: never stop to ask a question."
  , "Prove exactly one declaration in the file: `" <> name <> "`."
  , "Its statement is:"
  , "```lean"
  , T.stripEnd stmt
  , "```"
  , usableLine usable
  , "Do not edit any other declaration."
  , ""
  , "The plan so far:"
  , T.stripEnd board
  , ""
  , "When you finish, make the last line of your reply one of these:"
  , "NOTE: <what you tried and why you stopped>"
  , "FALSE: <a counterexample>"
  , "Use the FALSE line only if the statement is false."
  ]

-- | Which lemmas still ending in @sorry@ a proof may use: only those of the current plan.
usableLine :: [Text] -> Text
usableLine [] = "Use no declaration that still ends in `sorry`."
usableLine ns =
  "Besides what the file already proves, you may use only these lemmas of the current plan, even where they still end in `sorry`: "
    <> T.intercalate ", " ["`" <> n <> "`" | n <- ns] <> ". Use no other declaration that still ends in `sorry`."

