-- | From a provider's response to the assistant message upstream stores
-- (@_build_assistant_message@), with the finish reason and token usage,
-- for all three wire formats.
module Fisher.LLM.Normalize
  ( Assistant (..)
  , Usage (..)
  , Outcome (..)
  , coerceContent
  , extractReasoning
  , buildAssistantMessage
  , normalizeChat
  , normalizeAnthropic
  , normalizeCodex
  , responseInvalid
  , usageOf
  , preFinishReason
  , outcomeOf
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Fisher.LLM.Anthropic as An
import qualified Fisher.LLM.Codex as Cx
import Fisher.LLM.Types
import Fisher.Py.Value

-- | The assistant_message-like object each mode produces.
data Assistant = Assistant
  { aContent :: PyVal
  , aToolCalls :: [PyVal]
  , aReasoning :: PyVal
  , aReasoningContent :: PyVal
  , aReasoningDetails :: PyVal
  , aCodexItems :: PyVal
  }

data Usage = Usage
  { uPrompt :: Integer
  , uCompletion :: Integer
  , uTotal :: Integer
  , uCacheRead :: Maybe Integer
  , uCacheWrite :: Maybe Integer
  , uCost :: Maybe Double
  -- ^ Fisher only: OpenRouter's @usage.cost@, in dollars, when present
  }
  deriving (Eq, Show)

-- | A successful call, normalized.
data Outcome = Outcome
  { oPreFinish :: PyVal
  -- ^ the finish reason upstream reads before normalizing (drives the
  -- truncation path)
  , oFinish :: PyVal
  , oUsage :: Maybe Usage
  , oMessage :: PyVal
  -- ^ the stored assistant message
  }

-- | The content coercion every mode goes through (@run_agent.py:5048-5064@).
coerceContent :: PyVal -> PyVal
coerceContent = \case
  PNull -> PNull
  s@(PStr _) -> s
  d@(PDict _) -> case (get "text" d, get "content" d) of
    (Just t, _) | truthy t -> t
    (_, Just c) | truthy c -> c
    _ -> str (dumpsAscii d)
  PList parts ->
    str . T.intercalate "\n" $
      [ t | p <- parts, t <- case p of
              PStr s -> [s]
              PDict _
                | get "type" p == Just (PStr "text") -> [maybe "" pyStrOf (get "text" p)]
                | Just v <- get "text" p -> [pyStrOf v]
              _ -> [] ]
  v -> str (pyStrOf v)

-- | @_extract_reasoning@.
extractReasoning :: Assistant -> Maybe Text
extractReasoning a =
  let p1 = [t | PStr t <- [aReasoning a], not (T.null t)]
      p2 = [t | PStr t <- [aReasoningContent a], not (T.null t), t `notElem` p1]
      details = case aReasoningDetails a of
        PList ds -> [s | d@(PDict _) <- ds, Just (PStr s) <- [firstTruthy [get "summary" d, get "content" d, get "text" d]]]
        _ -> []
      parts = foldl (\acc s -> if T.null s || s `elem` acc then acc else acc <> [s]) (p1 <> p2) details
   in if null parts then Nothing else Just (T.intercalate "\n\n" parts)
  where
    firstTruthy = \case
      (Just v : _) | truthy v -> Just v
      (_ : rest) -> firstTruthy rest
      [] -> Nothing

-- | @re.findall(r'<think>(.*?)</think>', content, re.DOTALL)@.
thinkBlocks :: Text -> [Text]
thinkBlocks t = case T.breakOn "<think>" t of
  (_, rest) | T.null rest -> []
  (_, rest) ->
    let after = T.drop 7 rest
     in case T.breakOn "</think>" after of
          (_, close) | T.null close -> []
          (inner, close) -> inner : thinkBlocks (T.drop 8 close)

-- | @_build_assistant_message@.
buildAssistantMessage :: IO Text -> Assistant -> PyVal -> IO PyVal
buildAssistantMessage uuidHex a finish = do
  let contentText = case aContent a of
        PStr s -> s
        _ -> ""
      reasoning = case extractReasoning a of
        Just r -> Just r
        Nothing ->
          let bs = [T.strip b | b <- thinkBlocks contentText, not (T.null (T.strip b))]
           in if null bs then Nothing else Just (T.intercalate "\n\n" bs)
      base =
        [ ("role", str "assistant"), ("content", if truthy (aContent a) then aContent a else str "")
        , ("reasoning", maybe PNull str reasoning), ("finish_reason", finish) ]
      details = case aReasoningDetails a of
        PList ds | not (null ds) -> [("reasoning_details", PList [d | d@(PDict _) <- ds]) | any isDict ds]
        _ -> []
      codex = [("codex_reasoning_items", aCodexItems a) | truthy (aCodexItems a)]
  calls <- mapM toolCall (aToolCalls a)
  pure (obj (base <> details <> codex <> [("tool_calls", PList calls) | not (null calls)]))
  where
    isDict = \case
      PDict _ -> True
      _ -> False
    toolCall tc = do
      let rawId = fromMaybe PNull (get "id" tc)
          (embCall, embItem) = Cx.splitToolId rawId
      cid <- case get "call_id" tc of
        Just (PStr c) | not (T.null (T.strip c)) -> pure c
        _ -> case embCall of
          Just c | not (T.null (T.strip c)) -> pure c
          _ -> case rawId of
            PStr r | not (T.null (T.strip r)) -> pure (T.strip r)
            _ -> ("call_" <>) . T.take 12 <$> uuidHex
      let cid' = T.strip cid
          rid0 = case get "response_item_id" tc of
            Just (PStr r) | not (T.null (T.strip r)) -> Just r
            _ -> embItem
      rid <- Cx.deriveFunctionCallId uuidHex cid' rid0
      let fn = fromMaybe (obj []) (get "function" tc)
      pure . obj $
        [ ("id", str cid'), ("call_id", str cid'), ("response_item_id", str rid)
        , ("type", fromMaybe PNull (get "type" tc))
        , ("function", obj [("name", fromMaybe PNull (get "name" fn)), ("arguments", fromMaybe PNull (get "arguments" fn))]) ]
          <> [("extra_content", e) | Just e <- [get "extra_content" tc], e /= PNull]

-- | The shape check before normalizing (@run_agent.py:4446-4478@).
responseInvalid :: ApiMode -> PyVal -> Bool
responseInvalid mode r = case mode of
  CodexResponses -> case get "output" r of
    Just (PList (_ : _)) -> False
    _ -> True
  AnthropicMessages -> case get "content" r of
    Just (PList (_ : _)) -> False
    _ -> True
  ChatCompletions -> case get "choices" r of
    Just (PList (_ : _)) -> False
    _ -> True

preFinishReason :: ApiMode -> PyVal -> PyVal
preFinishReason mode r = case mode of
  CodexResponses ->
    let reason = get "incomplete_details" r >>= get "reason"
     in if get "status" r == Just (PStr "incomplete") && reason `elem` [Just (PStr "max_output_tokens"), Just (PStr "length")]
          then str "length"
          else str "stop"
  AnthropicMessages -> str $ case get "stop_reason" r of
    Just (PStr "end_turn") -> "stop"
    Just (PStr "tool_use") -> "tool_calls"
    Just (PStr "max_tokens") -> "length"
    _ -> "stop"
  ChatCompletions -> case get "choices" r of
    Just (PList (c : _)) -> fromMaybe PNull (get "finish_reason" c)
    _ -> PNull

usageOf :: ApiMode -> PyVal -> Maybe Usage
usageOf mode r = case get "usage" r of
  Just u | truthy u ->
    let cost = case get "cost" u of
          Just (PFloat d) -> Just d
          Just (PInt i) -> Just (fromIntegral i)
          _ -> Nothing
        n k = case get k u of
          Just (PInt i) -> i
          Just (PFloat d) -> truncate d
          _ -> 0
     in Just $ case mode of
          ChatCompletions ->
            let d = get "prompt_tokens_details" u
                dn k = case d >>= get k of
                  Just (PInt i) -> i
                  _ -> 0
             in Usage (n "prompt_tokens") (n "completion_tokens") (n "total_tokens")
                  (if maybe False (/= PNull) d then Just (dn "cached_tokens") else Nothing)
                  (if maybe False (/= PNull) d then Just (dn "cache_write_tokens") else Nothing)
                  cost
          _ ->
            let p = n "input_tokens"
                c = n "output_tokens"
                t = case get "total_tokens" u of
                  Just (PInt i) | i /= 0 -> i
                  _ -> p + c
             in Usage p c t
                  (if mode == AnthropicMessages then Just (n "cache_read_input_tokens") else Nothing)
                  (if mode == AnthropicMessages then Just (n "cache_creation_input_tokens") else Nothing)
                  cost
  _ -> Nothing

normalizeChat :: PyVal -> (Assistant, PyVal)
normalizeChat r =
  let choice = case get "choices" r of
        Just (PList (c : _)) -> c
        _ -> obj []
      m = fromMaybe (obj []) (get "message" choice)
      g k = fromMaybe PNull (get k m)
      calls = case get "tool_calls" m of
        Just (PList xs) -> xs
        _ -> []
   in (Assistant (coerceContent (g "content")) calls (g "reasoning") (g "reasoning_content") (g "reasoning_details") PNull, fromMaybe PNull (get "finish_reason" choice))

normalizeAnthropic :: PyVal -> (Assistant, PyVal)
normalizeAnthropic r =
  let (content, calls, reasoning, finish) = An.normalizeResponse r
   in (Assistant (maybe PNull str content) calls (maybe PNull str reasoning) PNull PNull PNull, str finish)

normalizeCodex :: IO Text -> PyVal -> IO (Either Text (Assistant, PyVal))
normalizeCodex uuidHex r =
  Cx.normalizeResponse uuidHex r >>= \case
    Left e -> pure (Left e)
    Right nc ->
      pure (Right (Assistant (str (Cx.ncContent nc)) (Cx.ncToolCalls nc) (maybe PNull str (Cx.ncReasoning nc)) PNull PNull (maybe PNull PList (Cx.ncReasoningItems nc)), str (Cx.ncFinish nc)))

-- | Everything upstream does with a response that came back: Left is the
-- normalizer's error; an invalid shape is reported separately by
-- 'responseInvalid'.
outcomeOf :: IO Text -> ApiMode -> PyVal -> IO (Either Text Outcome)
outcomeOf uuidHex mode r = do
  let pre = preFinishReason mode r
      usage = usageOf mode r
  norm <- case mode of
    ChatCompletions -> pure (Right (normalizeChat r))
    AnthropicMessages -> pure (Right (normalizeAnthropic r))
    CodexResponses -> normalizeCodex uuidHex r
  case norm of
    Left e -> pure (Left e)
    Right (a, finish) -> do
      let finish' = if mode == ChatCompletions then pre else finish
      msg <- buildAssistantMessage uuidHex a {aContent = coerceContent (aContent a)} finish'
      pure (Right (Outcome pre finish' usage msg))
