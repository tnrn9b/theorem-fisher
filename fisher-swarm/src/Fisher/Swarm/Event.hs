-- | What a session's output says about its progress: one line of Claude
-- Code @stream-json@ at a time (@_parse_stream_event@ in
-- @swarm_manager.py@).
module Fisher.Swarm.Event
  ( Progress (..)
  , initialProgress
  , parseStreamEvent
  , parseStreamEventFor
  ) where

import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Prover.Holes (eventHoleWords)
import Fisher.Prover.Kind (ProverKind (..))
import Fisher.Py (pyStr, pyTruthy)

data Progress = Progress
  { pProgress :: Text
  , pResult :: Maybe Text
  , pError :: Maybe Text
  , pLean :: Maybe Text
  }
  deriving (Eq, Show)

initialProgress :: Progress
initialProgress = Progress "Waiting" Nothing Nothing Nothing

get :: Text -> KM.KeyMap Value -> Maybe Value
get k = KM.lookup (K.fromText k)

text :: Maybe Value -> Text
text = \case
  Just (String s) -> s
  Just v | pyTruthy v -> pyStr v
  _ -> ""

-- | Update progress from one output line. Lines that are not JSON objects
-- change nothing. (Upstream raises on a JSON line that is not an object,
-- which fails the whole session; see README.)
--
-- A session does not yet carry its project's prover, so this reads tool
-- results as Lean's: the call sites use this form. 'parseStreamEventFor'
-- takes the kind whose hole words a tool result is searched for.
parseStreamEvent :: Text -> Progress -> Progress
parseStreamEvent = parseStreamEventFor Lean4

parseStreamEventFor :: ProverKind -> Text -> Progress -> Progress
parseStreamEventFor kind line p = case decodeStrict (TE.encodeUtf8 line) of
  Just (Object ev) -> case text (get "type" ev) of
    "assistant" | Just (Object msg) <- get "message" ev ->
      let stop = case text (get "stopReason" msg) of
            "" -> text (get "stop_reason" msg)
            s -> s
       in if stop == "end_turn" then p {pProgress = "Agent finished turn"} else p
    "content_block_start" -> case get "content_block" ev of
      Just (Object cb) | text (get "type" cb) == "tool_use" ->
        let name = text (get "name" cb)
            low = T.toLower name
            p' = p {pProgress = "Tool: " <> name}
         in if "lean" `T.isInfixOf` low || "lsp" `T.isInfixOf` low then p' {pLean = Just "active"} else p'
      _ -> p
    "result" ->
      let r = text (get "result" ev)
          p1 = p {pProgress = "Session complete"}
          p2 = if T.null r then p1 else p1 {pResult = Just (T.take 500 r)}
       in case text (get "subtype" ev) of
            "error_max_turns" -> p2 {pProgress = "Hit max turns"}
            "error_api" -> p2 {pError = Just (maybe "API error" (\v -> if v == Null then "None" else pyStr v) (get "error" ev))}
            _ -> p2
    "tool_result" -> case get "content" ev of
      Just (String c)
        | any (`T.isInfixOf` T.toLower c) (eventHoleWords kind) -> p {pLean = Just "has sorry"}
        | any (`T.isInfixOf` T.toLower c) ["no errors", "goals accomplished"] -> p {pLean = Just "verified"}
      _ -> p
    _ -> p
  _ -> p
