{-# LANGUAGE DataKinds #-}

-- | The verifier: judges a split, and checks a worker's claim that a lemma is false.
module Fisher.Search.Review
  ( SplitVerdict (..)
  , reviewMessages
  , parseReview
  , falseMessages
  , parseFalse
  , retryMessages
  , jsonObject
  , textOr
  ) where

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Role (Prompt, PromptPart, Promptable (..), Role (..), Said, parts, source, static, worker)
import Fisher.Role.Agent (excerpt)
import Fisher.Search.Split (fencedBlocks)

-- | The verifier's verdict on a split; its reason is what the verifier
-- said.
data SplitVerdict = SplitVerdict
  { svAccept :: Bool, svReason :: Said 'Verifier, svSuspect :: [Text] } deriving (Eq, Show)

reviewSystem :: Text
reviewSystem = T.unlines
  [ "You check a proposed split of a Lean 4 goal into lemmas."
  , "Judge whether each lemma is simpler than the goal and plausibly true,"
  , "and whether the sketch proves the goal from the lemmas."
  , "Reply with the JSON object only, in this form:"
  , "{\"accept\": true, \"reason\": \"...\", \"suspect\": []}"
  , "accept is true when the split should be used. reason is a short sentence."
  , "suspect lists the names of lemmas that look false or no simpler than the goal."
  ]

-- | Node statement, blueprint (the planner's), sketch (checked Lean),
-- whiteboard.
reviewMessages :: Text -> Said 'Planner -> Text -> PromptPart -> Prompt
reviewMessages stmt blueprint sketch board =
  [ ("system", static reviewSystem)
  , ("user", mconcat (intersperse (static "\n\n")
      [ static "The goal:\n```lean\n" <> source (T.stripEnd stmt) <> static "\n```"
      , static "The blueprint:\n" <> parts [toPrompt blueprint]
      , static "The sketch:\n```lean\n" <> source (T.stripEnd sketch) <> static "\n```"
      , static "The plan so far:\n" <> parts [board]
      ]))
  ]

-- | The verdict in the verifier's reply; its reason is read out of the
-- reply ('excerpt').
parseReview :: Said 'Verifier -> Either Text SplitVerdict
parseReview = fmap (\(r, (a, s)) -> SplitVerdict a r s) . excerpt readReview
  where
    readReview t = case jsonObject t of
      Just o | Just (Bool a) <- KM.lookup "accept" o
             , Just r <- textOr "reason" o
             , Just s <- listOr "suspect" o -> Right (r, (a, s))
      _ -> Left "the verdict is not the JSON asked for"

falseSystem :: Text
falseSystem = T.unlines
  [ "You are given a Lean 4 statement and a claim that the statement is false."
  , "Decide whether the claim really shows that the statement is false."
  , "Reply with the JSON object only, in this form:"
  , "{\"false\": true, \"reason\": \"...\"}"
  , "The value of false is true only when the claim holds up. reason is a short sentence."
  ]

-- | Lemma statement (source), the worker's claim (its FALSE line).
falseMessages :: Text -> Text -> Prompt
falseMessages stmt claim =
  [ ("system", static falseSystem)
  , ("user", static "The statement:\n```lean\n" <> source (T.stripEnd stmt) <> static "\n```\n\nThe claim:\n" <> toPrompt (worker (T.stripEnd claim)))
  ]

-- | The verdict on a FALSE claim and its reason, read out of the
-- verifier's reply ('excerpt').
parseFalse :: Said 'Verifier -> Either Text (Bool, Said 'Verifier)
parseFalse = fmap (\(r, f) -> (f, r)) . excerpt readFalse
  where
    readFalse t = case jsonObject t of
      Just o | Just (Bool f) <- KM.lookup "false" o
             , Just r <- textOr "reason" o -> Right (r, f)
      _ -> Left "the verdict is not the JSON asked for"

-- | The messages of a call, and for a second call the previous reply (the
-- same role's, as the role monad's @askTwice@ hands it back) and why it
-- could not be read.
retryMessages :: Prompt -> Maybe (PromptPart, Text) -> Prompt
retryMessages ms = \case
  Nothing -> ms
  Just (reply, why) ->
    ms <> [("assistant", reply), ("user", static ("Your reply could not be used: " <> why <> "\nReply with the JSON object only."))]

-- | A bare JSON object; else the first fenced block (of any tag, or none)
-- that is one; else the text from the first @{@ to the last @}@.
jsonObject :: Text -> Maybe (KM.KeyMap Value)
jsonObject t = listToMaybe (mapMaybe obj (T.strip t : map snd (fencedBlocks t) <> [braces]))
  where
    obj x = case decodeStrict (TE.encodeUtf8 x) of
      Just (Object o) -> Just o
      _ -> Nothing
    braces = let (_, from) = T.breakOn "{" t in T.dropWhileEnd (/= '}') from

textOr :: Text -> KM.KeyMap Value -> Maybe Text
textOr k o = case KM.lookup (K.fromText k) o of
  Nothing -> Just ""
  Just (String s) -> Just s
  _ -> Nothing

listOr :: Text -> KM.KeyMap Value -> Maybe [Text]
listOr k o = case KM.lookup (K.fromText k) o of
  Nothing -> Just []
  Just (Array a) -> traverse str (toList a)
  _ -> Nothing
  where str (String s) = Just s
        str _ = Nothing
