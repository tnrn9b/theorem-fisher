{-# LANGUAGE TemplateHaskell #-}

-- | The messages the RLM loop sends: system prompt, context metadata,
-- per-turn nudge and the default answer request. Ported from
-- rlm/utils/prompts.py (build_rlm_system_prompt, build_user_prompt) and
-- QueryMetadata in rlm/core/types.py, with RLMClient._default_answer in
-- rlm/core/rlm.py (alexzhang13/rlm at
-- d04208afbad29ca675ab13478c40ee8bebc84bfe, MIT; see NOTICE).
--
-- The system prompt is the reference's RLM_SYSTEM_PROMPT with no custom tools,
-- followed by the orchestrator addendum, held in data/system-prompt.txt.
module Fisher.RLM.Prompt
  ( ContextShape (..)
  , contextShape
  , initialMessages
  , turnMessage
  , defaultAnswerMessage
  ) where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Py.Value (PyVal (..), dumpsAscii, get, pyStrOf)
import Fisher.RLM.Parse (Message)

-- | What the metadata message says about the context: the Python type name
-- and the total length in characters.
data ContextShape = ContextShape
  { csType :: Text
  , csLength :: Int
  }
  deriving (Eq, Show)

-- | The reference's @QueryMetadata@. A string counts its code points; a dict
-- counts, per value, a string's length or the length of its JSON text; a
-- list counts per item, by the first item's kind (see 'listLengths').
--
-- Where the reference would raise (a context that is not a str, list or
-- dict, @len@ of a number, @.get@ of a non-dict) this gives 0 and, for a
-- bare scalar, the Python type name.
contextShape :: PyVal -> ContextShape
contextShape = \case
  PStr s -> ContextShape "str" (T.length s)
  PDict kvs -> ContextShape "dict" (sum [strOrJson v | (_, v) <- kvs])
  PList xs -> ContextShape "list" (sum (listLengths xs))
  PInt _ -> ContextShape "int" 0
  PFloat _ -> ContextShape "float" 0
  PBool _ -> ContextShape "bool" 0
  PNull -> ContextShape "NoneType" 0

strOrJson :: PyVal -> Int
strOrJson = \case
  PStr s -> T.length s
  v -> jsonLength v

jsonLength :: PyVal -> Int
jsonLength = T.length . dumpsAscii

listLengths :: [PyVal] -> [Int]
listLengths = \case
  [] -> [0]
  xs@(first@(PDict _) : _)
    | Just _ <- get "content" first -> map contentLength xs
    | otherwise -> map jsonLength xs
  xs -> map itemLength xs
  where
    -- len(str(item.get("content", "")))
    contentLength item = maybe 0 (T.length . pyStrOf) (get "content" item)
    -- len(item)
    itemLength = \case
      PStr s -> T.length s
      PList ys -> length ys
      PDict kvs -> length kvs
      _ -> 0

systemPrompt :: Text
systemPrompt = TE.decodeUtf8 $(embedFile "data/system-prompt.txt")

-- | The system message and the metadata message, as
-- @build_rlm_system_prompt@ builds them with no custom tools and the
-- orchestrator addendum. The root prompt, when present and not empty, leads
-- the metadata message.
initialMessages :: ContextShape -> Maybe Text -> [Message]
initialMessages shape root =
  [ ("system", systemPrompt)
  , ("user", metadata)
  ]
  where
    body =
      "Your context is a "
        <> csType shape
        <> " of "
        <> T.pack (show (csLength shape))
        <> " total characters. Each sub-LLM call can handle roughly ~100k tokens at once."
    metadata = case root of
      Just r | not (T.null r) -> "Answer the following: " <> r <> "\n\n" <> body
      _ -> body

-- | The user message for a turn, as @build_user_prompt@ builds it with one
-- context and no history. The iteration is 0-based; the first turn carries
-- the safeguard against answering before looking at the context.
turnMessage :: Int -> Int -> Message
turnMessage iteration maxIterations = ("user", prefix <> body)
  where
    body = "Turn " <> T.pack (show (iteration + 1)) <> "/" <> T.pack (show maxIterations) <> ":"
    prefix
      | iteration == 0 =
          "You have not interacted with the REPL environment or seen your prompt / context yet. Look at the context first; do not provide a final answer yet.\n\n"
      | otherwise = ""

-- | The request appended when the iterations run out.
defaultAnswerMessage :: Message
defaultAnswerMessage =
  ("assistant", "Please provide a final answer to the user's question based on the information provided.")
