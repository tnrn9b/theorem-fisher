-- | The planner: asks for a split of a goal into simpler lemmas, and reads the reply.
module Fisher.Search.Split
  ( Message
  , Problem (..)
  , problemPart
  , plannerMessages
  , parsePlan
  , fencedBlocks
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Role (Prompt, PromptPart, partText, parts, source, static, toPrompt, tool, worker)

-- | (role, content); the roles are "system", "user" and "assistant".
type Message = (Text, Text)

-- | Why a planner's reply was not used: its form (Fisher's sentence, with
-- any name or line of the planner's in it as the planner's words), or
-- Lean's check of its sketch.
data Problem = FormatError PromptPart | LeanErrors Text deriving (Eq, Show)

-- | A problem as the job's error: a form problem as its pieces were
-- labelled, Lean's errors as tool output.
problemPart :: Problem -> PromptPart
problemPart = \case
  FormatError p -> p
  LeanErrors t -> tool t

plannerSystem :: Text
plannerSystem = T.unlines
  [ "You plan Lean 4 proofs. You prove nothing directly."
  , "Given a goal, propose lemmas, each simpler than the goal, each ending in `:= by sorry`."
  , "Then give the goal's declaration with its statement unchanged and a proof that uses the lemmas."
  , "Every lemma must be plausibly true and must be stated with the names and types of the file."
  , "Reply with exactly two fenced blocks and no other text."
  , "The first is tagged blueprint: a short plain description of each lemma and how they combine."
  , "The second is tagged lean: the lemmas and then the goal's declaration."
  ]

-- | Base file, node name, the lemmas of the current plan the proof may use,
-- whiteboard, notes of the node's failed attempts, the node's retrieval
-- text (its facts and library offer; empty for none), and for a repair
-- round the previous reply (the planner's own, as the role monad's
-- @askTwice@ hands it back) with what was wrong with it.
plannerMessages :: Text -> Text -> [Text] -> PromptPart -> [Text] -> PromptPart -> Maybe (PromptPart, Problem) -> Prompt
plannerMessages base name usable board notes retrieved prev =
  [ ("system", static plannerSystem)
  , ("user", paragraphs [goal, retrieved, failed])
  ]
    <> case prev of
      Nothing -> []
      Just (reply, problem) -> [("assistant", reply), ("user", repair problem)]
  where
    goal =
      static "The goal is the declaration `" <> source name <> static "`.\n\nThe file so far:\n```lean\n"
        <> source (T.stripEnd base) <> static "\n```\n\nThe plan so far:\n"
        <> parts [board] <> static "\n\n" <> usable'
    usable' = case usable of
      [] -> static "Besides the new lemmas, the proof may use no declaration that still ends in `sorry`."
      ns -> static "Besides the new lemmas and what the file already proves, the proof may use only these lemmas of the current plan, even where they still end in `sorry`: "
              <> mconcat (intersperse (static ", ") [static "`" <> source n <> static "`" | n <- ns])
              <> static ". It may use no other declaration that still ends in `sorry`."
    -- a note is a worker's (its NOTE or FALSE line)
    failed
      | null notes = mempty
      | otherwise = static "Direct attempts at this goal failed:\n" <> mconcat [static "- " <> toPrompt (worker (oneLine n)) <> static "\n" | n <- notes]
    repair = \case
      FormatError why -> static "Your reply could not be used: " <> why <> static "\nReply again with both blocks."
      LeanErrors e -> static "Lean reported:\n```\n" <> tool (T.stripEnd e) <> static "\n```\nReply again with both blocks, fixing these errors."

-- | The non-empty parts, each without trailing whitespace, a blank line
-- between them. Unlike 'parts', a part of only whitespace still counts
-- (as an empty paragraph), as the planner's messages always had it.
paragraphs :: [PromptPart] -> PromptPart
paragraphs ps = mconcat (intersperse (static "\n\n") [parts [p] | p <- ps, not (T.null (partText p))])

-- | The first block tagged blueprint and the first tagged lean.
parsePlan :: Text -> Either Text (Text, Text)
parsePlan t = case (lookup "blueprint" bs, lookup "lean" bs) of
  (Just b, Just l) -> Right (b, l)
  _ -> Left "the reply needs a blueprint block and a lean block"
  where bs = fencedBlocks t

-- | Fenced blocks in order, as (tag, body), tags lower-cased with lean4 read as lean.
-- Inside an open block a tagged fence opens a nested block and a bare fence closes
-- the innermost one. An unclosed block is dropped.
fencedBlocks :: Text -> [(Text, Text)]
fencedBlocks = go . map (T.dropWhileEnd (== '\r')) . T.lines
  where
    go [] = []
    go (l : ls)
      | Just tag <- opener l =
          case scan (0 :: Int) [] ls of
            Nothing -> []
            Just (body, rest) -> (normTag tag, T.intercalate "\n" body) : go rest
      | otherwise = go ls
    scan _ _ [] = Nothing
    scan d acc (l : ls)
      | isCloser l = if d == 0 then Just (reverse acc, ls) else scan (d - 1) (l : acc) ls
      | Just t <- opener l, not (T.null t) = scan (d + 1) (l : acc) ls
      | otherwise = scan d (l : acc) ls
    opener l = let s = T.strip l in
      if "```" `T.isPrefixOf` s then Just (T.strip (T.dropWhile (== '`') s)) else Nothing
    isCloser l = T.strip l == "```"
    normTag t = case T.toLower t of
      "lean4" -> "lean"
      x -> x

-- | A note on one line, at most 300 characters.
oneLine :: Text -> Text
oneLine = T.take 300 . T.unwords . T.words
