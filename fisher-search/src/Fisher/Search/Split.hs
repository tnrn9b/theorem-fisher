-- | The planner: asks for a split of a goal into simpler lemmas, and reads the reply.
module Fisher.Search.Split
  ( Message
  , Problem (..)
  , problemText
  , plannerMessages
  , parsePlan
  , fencedBlocks
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | (role, content); the roles are "system", "user" and "assistant".
type Message = (Text, Text)

-- | Why a planner's reply was not used: its form, or Lean's check of its sketch.
data Problem = FormatError Text | LeanErrors Text deriving (Eq, Show)

problemText :: Problem -> Text
problemText = \case
  FormatError t -> t
  LeanErrors t -> t

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
-- round the previous reply with what was wrong with it.
plannerMessages :: Text -> Text -> [Text] -> Text -> [Text] -> Text -> Maybe (Text, Problem) -> [Message]
plannerMessages base name usable board notes retrieved prev =
  [ ("system", plannerSystem)
  , ("user", T.intercalate "\n\n" (map T.stripEnd (filter (not . T.null) [user, retrieved, failed])))
  ]
    <> case prev of
      Nothing -> []
      Just (reply, problem) -> [("assistant", reply), ("user", T.stripEnd (repair problem))]
  where
    user = T.unlines
      [ "The goal is the declaration `" <> name <> "`."
      , ""
      , "The file so far:"
      , "```lean"
      , T.stripEnd base
      , "```"
      , ""
      , "The plan so far:"
      , T.stripEnd board
      , ""
      , usable'
      ]
    usable' = case usable of
      [] -> "Besides the new lemmas, the proof may use no declaration that still ends in `sorry`."
      ns -> "Besides the new lemmas and what the file already proves, the proof may use only these lemmas of the current plan, even where they still end in `sorry`: "
              <> T.intercalate ", " ["`" <> n <> "`" | n <- ns] <> ". It may use no other declaration that still ends in `sorry`."
    failed
      | null notes = ""
      | otherwise = T.unlines ("Direct attempts at this goal failed:" : map (("- " <>) . oneLine) notes)
    repair = \case
      FormatError why -> T.unlines
        [ "Your reply could not be used: " <> why
        , "Reply again with both blocks."
        ]
      LeanErrors e -> T.unlines
        [ "Lean reported:"
        , "```"
        , T.stripEnd e
        , "```"
        , "Reply again with both blocks, fixing these errors."
        ]

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
