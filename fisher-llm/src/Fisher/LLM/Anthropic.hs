-- | Anthropic messages (@agent/anthropic_adapter.py@): OpenAI-shaped
-- messages to Anthropic's, the request, the headers, and the response back
-- to the common shape.
module Fisher.LLM.Anthropic
  ( convertMessages
  , convertTools
  , buildKwargs
  , normalizeModelName
  , anthropicHeaders
  , isOAuthToken
  , anthropicRequest
  , prepareImages
  , normalizeResponse
  , commonBetas
  , oauthBetas
  ) where

import Control.Monad (foldM)
import Data.Char (isAlphaNum, isAscii)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.LLM.Types
import Fisher.Py.Value

commonBetas, oauthBetas :: [Text]
commonBetas = ["interleaved-thinking-2025-05-14", "fine-grained-tool-streaming-2025-05-14"]
oauthBetas = ["claude-code-20250219", "oauth-2025-04-20"]

-- | @_is_oauth_token@: anything but a Console key (@sk-ant-api...@).
isOAuthToken :: Text -> Bool
isOAuthToken k = not (T.null k) && not ("sk-ant-api" `T.isPrefixOf` k)

anthropicHeaders :: Text -> [(Text, Text)]
anthropicHeaders key =
  [ ("accept", "application/json"), ("anthropic-version", "2023-06-01"), ("content-type", "application/json") ]
    <> if isOAuthToken key
      then [("authorization", "Bearer " <> key), ("anthropic-beta", T.intercalate "," (commonBetas <> oauthBetas))]
      else [("x-api-key", key), ("anthropic-beta", T.intercalate "," commonBetas)]

-- | @normalize_model_name@: no @anthropic/@ prefix, dots as hyphens.
normalizeModelName :: Text -> Text
normalizeModelName m =
  let m' = if "anthropic/" `T.isPrefixOf` T.toLower m then T.drop 10 m else m
   in T.replace "." "-" m'

sanitizeToolId :: Text -> Text
sanitizeToolId t
  | T.null t = "tool_0"
  | otherwise = T.map (\c -> if isAscii c && (isAlphaNum c || c == '_' || c == '-') then c else '_') t

imageSource :: Text -> PyVal
imageSource raw =
  let url = T.strip raw
   in if T.null url
        then obj [("type", str "url"), ("url", str "")]
        else
          if "data:" `T.isPrefixOf` url
            then
              let (header, rest) = T.breakOn "," url
                  dat = T.drop 1 rest
                  mime = T.strip (fst (T.breakOn ";" (T.drop 5 header)))
                  media = if "image/" `T.isPrefixOf` mime then mime else "image/jpeg"
               in obj [("type", str "base64"), ("media_type", str media), ("data", str dat)]
            else obj [("type", str "url"), ("url", str url)]

convertPart :: PyVal -> Maybe PyVal
convertPart = \case
  PNull -> Nothing
  PStr s -> Just (obj [("type", str "text"), ("text", str s)])
  part@(PDict _) ->
    let block = case get "type" part of
          Just (PStr "input_text") -> obj [("type", str "text"), ("text", fromMaybe (str "") (get "text" part))]
          Just (PStr t)
            | t `elem` ["image_url", "input_image"] ->
                let url = case get "image_url" part of
                      Just d@(PDict _) -> case get "url" d of
                        Just (PStr u) -> u
                        Just v -> pyStrOf v
                        Nothing -> ""
                      Just v | truthy v -> pyStrOf v
                      _ -> ""
                 in obj [("type", str "image"), ("source", imageSource url)]
          _ -> part
     in Just $ case get "cache_control" part of
          Just cc@(PDict _) | not (has "cache_control" block) -> set "cache_control" cc block
          _ -> block
  other -> Just (obj [("type", str "text"), ("text", str (pyStrOf other))])

convertContent :: PyVal -> PyVal
convertContent = \case
  PList parts -> PList (mapMaybe convertPart parts)
  c -> c

-- | @convert_messages_to_anthropic@: the system prompt and the messages.
convertMessages :: [PyVal] -> (Maybe PyVal, [PyVal])
convertMessages msgs =
  let (system, result) = foldl step (Nothing, []) msgs
      resultIds = [i | m <- result, role m == "user", PList bs <- [content m], b <- bs, get "type" b == Just (PStr "tool_result"), Just i <- [get "tool_use_id" b]]
      stripped = map (stripOrphans resultIds) result
   in (system, alternate stripped)
  where
    role m = case get "role" m of
      Just (PStr r) -> r
      _ -> "user"
    content m = fromMaybe (str "") (get "content" m)
    step (sys, acc) m =
      let c = fromMaybe (str "") (get "content" m)
       in case role m of
            "system" ->
              let s = case c of
                    PList ps ->
                      if any (\p -> maybe False truthy (get "cache_control" p)) [p | p@(PDict _) <- ps]
                        then PList [p | p@(PDict _) <- ps]
                        else str (T.intercalate "\n" [t | p <- ps, get "type" p == Just (PStr "text"), Just (PStr t) <- [get "text" p]])
                    other -> other
               in (Just s, acc)
            "assistant" ->
              let textBlocks
                    | truthy c = case c of
                        PList _ -> case convertContent c of
                          PList bs -> bs
                          _ -> []
                        _ -> [obj [("type", str "text"), ("text", str (pyStrOf c))]]
                    | otherwise = []
                  tcs = case get "tool_calls" m of
                    Just (PList xs) -> xs
                    _ -> []
                  useBlock tc =
                    let fn = fromMaybe (obj []) (get "function" tc)
                        args = fromMaybe (str "{}") (get "arguments" fn)
                        parsed = case args of
                          PStr s -> either (const (obj [])) id (loads (TE.encodeUtf8 s))
                          v -> v
                        tid = case get "id" tc of
                          Just (PStr s) -> s
                          _ -> ""
                     in obj [ ("type", str "tool_use"), ("id", str (sanitizeToolId tid))
                            , ("name", fromMaybe (str "") (get "name" fn)), ("input", parsed) ]
                  blocks = textBlocks <> map useBlock tcs
                  effective
                    | not (null blocks) = PList blocks
                    | truthy c = c
                    | otherwise = PList [obj [("type", str "text"), ("text", str "(empty)")]]
               in (sys, acc <> [obj [("role", str "assistant"), ("content", effective)]])
            "tool" ->
              let rc = case c of
                    PStr s -> s
                    v -> dumpsAscii v
                  rc' = if T.null rc then "(no output)" else rc
                  tid = case get "tool_call_id" m of
                    Just (PStr s) -> s
                    _ -> ""
                  tr0 = obj [("type", str "tool_result"), ("tool_use_id", str (sanitizeToolId tid)), ("content", str rc')]
                  tr = case get "cache_control" m of
                    Just cc@(PDict _) -> set "cache_control" cc tr0
                    _ -> tr0
               in case reverse acc of
                    (lastM : before)
                      | role lastM == "user"
                      , PList (b0 : bs) <- content lastM
                      , get "type" b0 == Just (PStr "tool_result") ->
                          (sys, reverse before <> [set "content" (PList ((b0 : bs) <> [tr])) lastM])
                    _ -> (sys, acc <> [obj [("role", str "user"), ("content", PList [tr])]])
            _ ->
              let c' = case c of
                    PList _ -> case convertContent c of
                      PList [] -> PList [obj [("type", str "text"), ("text", str "")]]
                      bs -> bs
                    other -> other
               in (sys, acc <> [obj [("role", str "user"), ("content", c')]])
    stripOrphans ids m
      | role m == "assistant"
      , PList bs <- content m =
          let kept = [b | b <- bs, get "type" b /= Just (PStr "tool_use") || maybe False (`elem` ids) (get "id" b)]
           in set "content" (if null kept then PList [obj [("type", str "text"), ("text", str "(tool call removed)")]] else PList kept) m
      | otherwise = m
    alternate = foldl merge []
    merge [] m = [m]
    merge fixed m =
      let prev = last fixed
          front = init fixed
       in if role prev /= role m
            then fixed <> [m]
            else
              let pc = content prev
                  cc = content m
               in if role m == "user"
                    then
                      let merged = case (pc, cc) of
                            (PStr a, PStr b) -> PStr (a <> "\n" <> b)
                            (PList a, PList b) -> PList (a <> b)
                            _ -> PList (wrap pc <> wrap cc)
                       in front <> [set "content" merged prev]
                    else case (pc, cc) of
                      (PList a, PList b) -> front <> [set "content" (PList (a <> b)) prev]
                      (PStr a, PStr b) -> front <> [set "content" (PStr (a <> "\n" <> b)) prev]
                      _ -> front <> [m]
    wrap = \case
      PStr s -> [obj [("type", str "text"), ("text", str s)]]
      PList xs -> xs
      v -> [v]

convertTools :: [PyVal] -> [PyVal]
convertTools = map $ \t ->
  let fn = fromMaybe (obj []) (get "function" t)
   in obj
        [ ("name", fromMaybe (str "") (get "name" fn))
        , ("description", fromMaybe (str "") (get "description" fn))
        , ("input_schema", fromMaybe (obj [("type", str "object"), ("properties", obj [])]) (get "parameters" fn)) ]

-- | @build_anthropic_kwargs@ with the agent's arguments (tool_choice is
-- always None there, so @{"type": "auto"}@).
buildKwargs :: Settings -> [PyVal] -> PyVal
buildKwargs s msgs =
  let (system, ams) = convertMessages msgs
      tools = case sTools s of
        Just ts@(_ : _) -> convertTools ts
        _ -> []
      model = normalizeModelName (sModel s)
      maxTok = case sMaxTokens s of
        Just n | n /= 0 -> n
        _ -> 16384
      base =
        [("model", str model), ("messages", PList ams), ("max_tokens", PInt maxTok)]
          <> [("system", sv) | Just sv <- [system], truthy sv]
          <> (if null tools then [] else [("tools", PList tools), ("tool_choice", obj [("type", str "auto")])])
      thinking = case sReasoning s of
        Just rc@(PDict _)
          | truthy rc
          , get "enabled" rc /= Just (PBool False)
          , not ("haiku" `T.isInfixOf` T.toLower model) ->
              let effort = T.toLower (maybe "medium" pyStrOf (get "effort" rc))
                  budget = fromMaybe 8000 (lookup effort [("xhigh", 32000), ("high", 16000), ("medium", 8000), ("low", 4000)])
               in if "4-6" `T.isInfixOf` model || "4.6" `T.isInfixOf` model
                    then
                      [ ("thinking", obj [("type", str "adaptive")])
                      , ("output_config", obj [("effort", str (fromMaybe "medium" (lookup effort [("xhigh", "max"), ("high", "high"), ("medium", "medium"), ("low", "low"), ("minimal", "low")])))]) ]
                    else
                      [ ("thinking", obj [("type", str "enabled"), ("budget_tokens", PInt budget)])
                      , ("temperature", PInt 1)
                      , ("max_tokens", PInt (max maxTok (budget + 4096))) ]
        _ -> []
   in foldl (\acc (k, v) -> set k v acc) (obj base) thinking

-- | @_prepare_anthropic_messages_for_api@: messages holding images become
-- text, the images described by the vision fallback.
prepareImages :: Env -> [PyVal] -> IO [PyVal]
prepareImages env msgs
  | not (any (hasImages . fromMaybe PNull . get "content") msgs) = pure msgs
  | otherwise = mapM one msgs
  where
    hasImages = \case
      PList ps -> any (\p -> get "type" p `elem` [Just (PStr "image_url"), Just (PStr "input_image")]) ps
      _ -> False
    one m@(PDict _) = do
      let roleT = case get "role" m of
            Just r | truthy r -> pyStrOf r
            _ -> "user"
      c <- preprocess (fromMaybe PNull (get "content" m)) roleT
      pure (set "content" c m)
    one m = pure m
    preprocess c roleT
      | not (hasImages c) = pure c
      | PList parts <- c = do
          (texts, notes) <- foldM (part roleT) ([], []) parts
          let prefix = T.strip (T.intercalate "\n\n" (filter (not . T.null) (reverse notes)))
              suffix = T.strip (T.intercalate "\n" (filter (not . T.null) (reverse texts)))
          pure . str $
            if not (T.null prefix) && not (T.null suffix)
              then prefix <> "\n\n" <> suffix
              else
                if not (T.null prefix)
                  then prefix
                  else if not (T.null suffix) then suffix else "[A multimodal message was converted to text for Anthropic compatibility.]"
      | otherwise = pure c
    textOf p = case get "text" p of
      Just v | truthy v -> T.strip (pyStrOf v)
      _ -> ""
    part roleT (ts, ns) = \case
      PStr s -> pure (if T.null (T.strip s) then ts else T.strip s : ts, ns)
      p@(PDict _) -> case get "type" p of
        Just (PStr t)
          | t `elem` ["text", "input_text"] -> pure (let x = textOf p in if T.null x then ts else x : ts, ns)
          | t `elem` ["image_url", "input_image"] -> do
              let url = case get "image_url" p of
                    Just d@(PDict _) -> maybe "" pyStrOf (get "url" d)
                    Just v | truthy v -> pyStrOf v
                    _ -> ""
              note <-
                if T.null url
                  then pure "[An image was attached but no image source was available.]"
                  else eDescribeImage env url roleT
              pure (ts, note : ns)
        _ -> pure (let x = textOf p in if T.null x then ts else x : ts, ns)
      _ -> pure (ts, ns)

anthropicRequest :: Env -> Settings -> [PyVal] -> IO HttpReq
anthropicRequest env s msgs = do
  prepared <- prepareImages env msgs
  let base = T.dropWhileEnd (== '/') (if T.null (sBaseUrl s) then "https://api.anthropic.com" else sBaseUrl s)
  pure (HttpReq "POST" (base <> "/v1/messages") (anthropicHeaders (sApiKey s)) (Just (buildKwargs s prepared)) False)

-- | @normalize_anthropic_response@: (content, tool calls, reasoning) and
-- the finish reason.
normalizeResponse :: PyVal -> (Maybe Text, [PyVal], Maybe Text, Text)
normalizeResponse resp =
  let blocks = case get "content" resp of
        Just (PList bs) -> bs
        _ -> []
      ty b = maybe "" pyStrOf (get "type" b)
      texts = [maybe "" pyStrOf (get "text" b) | b <- blocks, ty b == "text"]
      thinks = [maybe "" pyStrOf (get "thinking" b) | b <- blocks, ty b == "thinking"]
      calls =
        [ obj [ ("id", fromMaybe PNull (get "id" b)), ("type", str "function")
              , ("function", obj [("name", fromMaybe PNull (get "name" b)), ("arguments", str (dumpsAscii (fromMaybe (obj []) (get "input" b))))]) ]
        | b <- blocks, ty b == "tool_use" ]
      finish = case get "stop_reason" resp of
        Just (PStr "end_turn") -> "stop"
        Just (PStr "tool_use") -> "tool_calls"
        Just (PStr "max_tokens") -> "length"
        _ -> "stop"
   in ( if null texts then Nothing else Just (T.intercalate "\n" texts)
      , calls
      , if null thinks then Nothing else Just (T.intercalate "\n\n" thinks)
      , finish )
