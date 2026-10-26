-- | Run-log events from a managed backend's stream: the JSON lines an
-- attempt writes to its @stream.jsonl@.
--
-- * @claude-code@ (stream-json): an @assistant@ line's @text@ blocks are
--   @message {role, text}@ and its @tool_use@ blocks @tool_start {name,
--   args, call_id}@; a @user@ line's @tool_result@ blocks are @tool_end
--   {name, call_id, ok, result}@ (@ok@ is not @is_error@, @name@ from the
--   matching start or null); the @result@ line is @cost {dollars, input,
--   output}@ then @outcome {outcome}@, @completed@ or @error@.
-- * @codex@ (@exec --json@): an @item.completed@ @agent_message@ is a
--   @message@; a @command_execution@ (tool @shell@) or @function_call@ is
--   a @tool_start@, and the command's completion or the
--   @function_call_output@ its @tool_end@ (a command without an id is
--   given @codex-<n>@); @turn.completed@ is a @cost@ with null dollars.
-- * @fisher@ (the native agent's @fisher chat -p@ stream): only its
--   @tool_start@ and @cost@ events, from the same shapes as Claude Code's
--   (the agent writes the rest to the run log itself).
--
-- Any other backend name is read as @claude-code@. A line that does not
-- decode to a JSON object (a partial last line, say) is skipped, and a
-- field of an unexpected type is null, so neither function throws.
-- Every event's @t@ is Nothing: the stream has no clock.
module Fisher.Search.Stream
  ( StreamEvent (..)
  , streamEvents
  , streamSessionId
  ) where

import Data.Aeson (Object, Value (..), decodeStrict, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Runlog.Stream (StreamEvent (..))

-- | The events of a stream, given the backend that wrote it.
streamEvents :: Text -> BS.ByteString -> [StreamEvent]
streamEvents backend bs = case backend of
  "codex" -> codexEvents objs
  "fisher" -> filter ((`elem` ["tool_start", "cost"]) . stKind) (claudeEvents False objs)
  _ -> claudeEvents True objs
  where
    objs = streamObjects bs

-- | Claude Code's session id: its last @result@ line's @session_id@, else
-- the first one any line carries (the @system@ init line has it), so a
-- stream cut before its result still names its session.
streamSessionId :: BS.ByteString -> Maybe Text
streamSessionId bs =
  let objs = streamObjects bs
      fromResult = listToMaybe (reverse [s | o <- objs, str "type" o == Just "result", Just s <- [str "session_id" o]])
   in case fromResult of
        Just s -> Just s
        Nothing -> listToMaybe (mapMaybe (str "session_id") objs)

streamObjects :: BS.ByteString -> [Object]
streamObjects bs = [o | l <- BC.lines bs, Just (Object o) <- [decodeStrict l]]

ev :: Text -> [(K.Key, Value)] -> StreamEvent
ev kind kvs = StreamEvent Nothing kind (object [k .= v | (k, v) <- kvs])

-- * Claude Code and the native agent

claudeEvents :: Bool -> [Object] -> [StreamEvent]
claudeEvents withOutcome = go Map.empty
  where
    go _ [] = []
    go names (o : os) = case str "type" o of
      Just "assistant" ->
        let bs = blocks (at "message" o >>= KM.lookup "content")
            starts = [(i, n) | b <- bs, str "type" b == Just "tool_use", Just i <- [str "id" b], Just n <- [str "name" b]]
         in mapMaybe assistantBlock bs ++ go (Map.union (Map.fromList starts) names) os
      Just "user" ->
        [toolEnd names b | b <- blocks (at "message" o >>= KM.lookup "content"), str "type" b == Just "tool_result"] ++ go names os
      Just "result" -> cost o : [outcome o | withOutcome] ++ go names os
      _ -> go names os
    assistantBlock b = case str "type" b of
      Just "text" | Just t <- str "text" b, not (T.null (T.strip t)) -> Just (message t)
      Just "tool_use" ->
        Just (ev "tool_start" [("name", txt (str "name" b)), ("args", maybe Null id (KM.lookup "input" b)), ("call_id", txt (str "id" b))])
      _ -> Nothing
    toolEnd names b =
      let cid = str "tool_use_id" b
       in ev
            "tool_end"
            [ ("name", txt (cid >>= (`Map.lookup` names)))
            , ("call_id", txt cid)
            , ("ok", Bool (KM.lookup "is_error" b /= Just (Bool True)))
            , ("result", String (contentText (KM.lookup "content" b)))
            ]
    cost o =
      let u = at "usage" o
       in ev "cost" [("dollars", num (KM.lookup "total_cost_usd" o)), ("input", num (u >>= KM.lookup "input_tokens")), ("output", num (u >>= KM.lookup "output_tokens"))]
    outcome o =
      let failed = KM.lookup "is_error" o == Just (Bool True) || maybe False ("error" `T.isPrefixOf`) (str "subtype" o)
       in ev "outcome" [("outcome", String (if failed then "error" else "completed"))]

-- | A message's content blocks; a plain string is one text block.
blocks :: Maybe Value -> [Object]
blocks = \case
  Just (Array xs) -> [b | Object b <- toList xs]
  Just (String t) -> [KM.fromList [("type", String "text"), ("text", String t)]]
  _ -> []

-- | A tool result's text: a string, or its text blocks one per line.
contentText :: Maybe Value -> Text
contentText = \case
  Just (String t) -> t
  Just (Array xs) -> T.intercalate "\n" [t | Object b <- toList xs, str "type" b == Just "text", Just t <- [str "text" b]]
  _ -> ""

message :: Text -> StreamEvent
message t = ev "message" [("role", String "assistant"), ("text", String t)]

-- * Codex

codexEvents :: [Object] -> [StreamEvent]
codexEvents = go Map.empty . withIds 1
  where
    -- the call ids started so far, with their tool names
    go _ [] = []
    go started (o : os) = case (str "type" o, at "item" o) of
      (Just "item.started", Just it)
        | Just (cid, name, args) <- call it, Map.notMember cid started ->
            start cid name args : go (Map.insert cid name started) os
      (Just "item.completed", Just it) -> case str "type" it of
        Just "agent_message" | Just t <- str "text" it, not (T.null (T.strip t)) -> message t : go started os
        Just "command_execution" | Just (cid, name, args) <- call it ->
          let ok = case KM.lookup "exit_code" it of
                Just (Number n) -> n == 0
                _ -> str "status" it == Just "completed"
              end = ev "tool_end" [("name", String name), ("call_id", String cid), ("ok", Bool ok), ("result", String (maybe "" id (str "aggregated_output" it)))]
           in [start cid name args | Map.notMember cid started] ++ end : go (Map.insert cid name started) os
        Just "function_call" | Just (cid, name, args) <- call it, Map.notMember cid started ->
          start cid name args : go (Map.insert cid name started) os
        Just "function_call_output" ->
          let cid = str "call_id" it
              end = ev "tool_end" [("name", txt (cid >>= (`Map.lookup` started))), ("call_id", txt cid), ("ok", Bool True), ("result", String (outputText (KM.lookup "output" it)))]
           in end : go started os
        _ -> go started os
      (Just "turn.completed", _) ->
        let u = at "usage" o
         in ev "cost" [("dollars", Null), ("input", num (u >>= KM.lookup "input_tokens")), ("output", num (u >>= KM.lookup "output_tokens"))] : go started os
      _ -> go started os
    start cid name args = ev "tool_start" [("name", String name), ("args", args), ("call_id", String cid)]
    -- a completed command without a string id gets @codex-<n>@ (n counts
    -- such commands from 1), so it still has a start and an end; a started
    -- one without an id is skipped, and its completion starts it
    withIds :: Int -> [Object] -> [Object]
    withIds _ [] = []
    withIds n (o : os) = case (str "type" o, at "item" o) of
      (Just "item.completed", Just it)
        | str "type" it == Just "command_execution", Nothing <- str "id" it ->
            KM.insert "item" (Object (KM.insert "id" (String ("codex-" <> T.pack (show n))) it)) o : withIds (n + 1) os
      _ -> o : withIds n os
    -- a tool call's id, name and arguments
    call it = case str "type" it of
      Just "command_execution" -> do
        cid <- str "id" it
        pure (cid, "shell", object ["command" .= maybe Null String (str "command" it)])
      Just "function_call" -> do
        cid <- maybe (str "id" it) Just (str "call_id" it)
        pure (cid, maybe "" id (str "name" it), arguments (KM.lookup "arguments" it))
      _ -> Nothing
    -- function-call arguments arrive as a JSON string
    arguments = \case
      Just (String s) -> maybe (String s) id (decodeStrict (TE.encodeUtf8 s))
      Just v -> v
      Nothing -> Null
    outputText = \case
      Just (String s) -> s
      Just (Object b) | Just s <- str "output" b -> s
      _ -> ""

-- * Fields

str :: K.Key -> Object -> Maybe Text
str k o = case KM.lookup k o of
  Just (String t) -> Just t
  _ -> Nothing

at :: K.Key -> Object -> Maybe Object
at k o = case KM.lookup k o of
  Just (Object v) -> Just v
  _ -> Nothing

txt :: Maybe Text -> Value
txt = maybe Null String

num :: Maybe Value -> Value
num = \case
  Just n@(Number _) -> n
  _ -> Null
