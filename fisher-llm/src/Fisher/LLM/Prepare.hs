-- | What upstream does to the stored conversation before every API call
-- (@run_agent.py@ 4287-4343): reasoning copied to @reasoning_content@,
-- trajectory-only fields dropped, the system prompt and prefill put in
-- front, Anthropic cache breakpoints, and orphaned tool calls and results
-- repaired.
module Fisher.LLM.Prepare
  ( PrepareInput (..)
  , prepareMessages
  , applyCacheControl
  , sanitizeToolPairs
  , stripStrictToolFields
  , usesPromptCaching
  ) where

import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Value

data PrepareInput = PrepareInput
  { piBaseUrl :: Text
  , piSystem :: Maybe Text
  , piEphemeral :: Maybe Text
  , piPrefill :: [PyVal]
  , piCaching :: Bool
  }

-- | @_use_prompt_caching@ (@run_agent.py:386-390@).
usesPromptCaching :: Text -> Text -> Bool -> Bool
usesPromptCaching baseUrl model anthropicMode =
  ("openrouter" `T.isInfixOf` T.toLower baseUrl && "claude" `T.isInfixOf` T.toLower model) || anthropicMode

prepareMessages :: PrepareInput -> [PyVal] -> [PyVal]
prepareMessages pin history =
  let copied = map copyOne history
      effective =
        let base = fromMaybe "" (piSystem pin)
         in case piEphemeral pin of
              Just e | not (T.null e) -> T.strip (base <> "\n\n" <> e)
              _ -> base
      withSystem = if T.null effective then copied else obj [("role", str "system"), ("content", str effective)] : copied
      offset = if T.null effective then 0 else 1
      withPrefill = let (a, b) = splitAt offset withSystem in a <> piPrefill pin <> b
      cached = if piCaching pin then applyCacheControl withPrefill else withPrefill
   in sanitizeToolPairs cached
  where
    copyOne m =
      let withRc = case (get "role" m, get "reasoning" m) of
            (Just (PStr "assistant"), Just r) | truthy r -> set "reasoning_content" r m
            _ -> m
          dropped = delete "finish_reason" (delete "reasoning" withRc)
       in if "api.mistral.ai" `T.isInfixOf` T.toLower (piBaseUrl pin) then stripStrictToolFields dropped else dropped

-- | @_sanitize_tool_calls_for_strict_api@: @call_id@ and @response_item_id@
-- out of each tool call.
stripStrictToolFields :: PyVal -> PyVal
stripStrictToolFields m = case get "tool_calls" m of
  Just (PList tcs) -> set "tool_calls" (PList (map strip tcs)) m
  _ -> m
  where
    strip tc@(PDict _) = delete "response_item_id" (delete "call_id" tc)
    strip tc = tc

-- | @apply_anthropic_cache_control@ (5m): the system message and the last
-- (4 - used) other messages get @{"type": "ephemeral"}@.
applyCacheControl :: [PyVal] -> [PyVal]
applyCacheControl [] = []
applyCacheControl msgs =
  let sysFirst = get "role" (head msgs) == Just (PStr "system")
      used = if sysFirst then 1 else 0 :: Int
      nonSys = [i | (i, m) <- zip [0 :: Int ..] msgs, get "role" m /= Just (PStr "system")]
      chosen = drop (length nonSys - (4 - used)) nonSys
      marked = [(i == 0 && sysFirst) || i `elem` chosen | i <- [0 .. length msgs - 1]]
   in [if b then mark m else m | (m, b) <- zip msgs marked]
  where
    marker = obj [("type", str "ephemeral")]
    mark m
      | get "role" m == Just (PStr "tool") = set "cache_control" marker m
      | otherwise = case get "content" m of
          Nothing -> set "cache_control" marker m
          Just PNull -> set "cache_control" marker m
          Just (PStr "") -> set "cache_control" marker m
          Just (PStr s) -> set "content" (PList [obj [("type", str "text"), ("text", str s), ("cache_control", marker)]]) m
          Just (PList parts@(_ : _)) -> case last parts of
            p@(PDict _) -> set "content" (PList (init parts <> [set "cache_control" marker p])) m
            _ -> m
          _ -> m

-- | @ContextCompressor._sanitize_tool_pairs@.
sanitizeToolPairs :: [PyVal] -> [PyVal]
sanitizeToolPairs msgs =
  let callId tc = case get "id" tc of
        Just (PStr s) -> s
        _ -> ""
      surviving = nub [cid | m <- msgs, role m == "assistant", tc <- toolCalls m, let cid = callId tc, not (T.null cid)]
      results = nub (mapMaybe resultId msgs)
      orphaned = filter (`notElem` surviving) results
      msgs1 =
        if null orphaned
          then msgs
          else filter (\m -> not (role m == "tool" && maybe False (`elem` orphaned) (resultId m))) msgs
      missing = filter (`notElem` results) surviving
   in if null missing
        then msgs1
        else
          concat
            [ m : [ obj [("role", str "tool"), ("content", str "[Result from earlier conversation — see context summary above]"), ("tool_call_id", str cid)]
                  | role m == "assistant", tc <- toolCalls m, let cid = callId tc, cid `elem` missing ]
            | m <- msgs1 ]
  where
    role m = case get "role" m of
      Just (PStr r) -> r
      _ -> ""
    toolCalls m = case get "tool_calls" m of
      Just (PList xs) -> xs
      _ -> []
    resultId m
      | role m == "tool" = case get "tool_call_id" m of
          Just (PStr s) | not (T.null s) -> Just s
          _ -> Nothing
      | otherwise = Nothing
