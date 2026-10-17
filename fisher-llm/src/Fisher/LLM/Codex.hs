-- | The Codex (OpenAI Responses) wire format, as @run_agent.py@ builds it:
-- chat messages to Responses input items, the request and its preflight,
-- the event stream, and the response back to the common shape.
module Fisher.LLM.Codex
  ( responsesInput
  , responsesTools
  , buildKwargs
  , preflight
  , codexRequest
  , splitToolId
  , deriveFunctionCallId
  , StreamOutcome (..)
  , accumulateStream
  , fallbackTerminal
  , normalizeResponse
  , NormalizedCodex (..)
  , defaultAgentIdentity
  ) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as B16
import Data.Char (isAlphaNum, isAscii)
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.LLM.Identity (defaultAgentIdentity)
import Fisher.LLM.Types
import Fisher.Py.Value

-- | @_split_responses_tool_id@: a stored id to (call_id, response_item_id).
splitToolId :: PyVal -> (Maybe Text, Maybe Text)
splitToolId = \case
  PStr raw ->
    let v = T.strip raw
     in if T.null v
          then (Nothing, Nothing)
          else
            if "|" `T.isInfixOf` v
              then
                let (a, b) = T.breakOn "|" v
                    nz x = let x' = T.strip x in if T.null x' then Nothing else Just x'
                 in (nz a, nz (T.drop 1 b))
              else if "fc_" `T.isPrefixOf` v then (Nothing, Just v) else (Just v, Nothing)
  _ -> (Nothing, Nothing)

-- | @_derive_responses_function_call_id@ (the uuid seed only matters when
-- both ids are empty).
deriveFunctionCallId :: IO Text -> Text -> Maybe Text -> IO Text
deriveFunctionCallId uuidHex callId responseItemId =
  case fmap T.strip responseItemId of
    Just c | "fc_" `T.isPrefixOf` c -> pure c
    _ -> do
      let source = T.strip callId
          sanitized = T.filter (\c -> isAscii c && (isAlphaNum c || c == '_' || c == '-')) source
      if "fc_" `T.isPrefixOf` source
        then pure source
        else
          if "call_" `T.isPrefixOf` source && T.length source > 5
            then pure ("fc_" <> T.drop 5 source)
            else
              if "fc_" `T.isPrefixOf` sanitized
                then pure sanitized
                else
                  if "call_" `T.isPrefixOf` sanitized && T.length sanitized > 5
                    then pure ("fc_" <> T.drop 5 sanitized)
                    else
                      if not (T.null sanitized)
                        then pure ("fc_" <> T.take 48 sanitized)
                        else do
                          seed <-
                            if not (T.null source)
                              then pure source
                              else case responseItemId of
                                Just r | not (T.null r) -> pure r
                                _ -> uuidHex
                          pure ("fc_" <> T.take 24 (TE.decodeUtf8 (B16.encode (SHA1.hash (TE.encodeUtf8 seed)))))

nonBlank :: PyVal -> Maybe Text
nonBlank = \case
  PStr s | not (T.null (T.strip s)) -> Just s
  _ -> Nothing

argText :: PyVal -> Text
argText = \case
  PStr s -> s
  d@(PDict _) -> dumpsUnicode d
  v -> pyStrOf v

-- | @_chat_messages_to_responses_input@.
responsesInput :: Env -> [PyVal] -> IO [PyVal]
responsesInput env msgs = concat <$> mapM one msgs
  where
    one m@(PDict _) = case get "role" m of
      Just (PStr "system") -> pure []
      Just (PStr r) | r `elem` ["user", "assistant"] -> do
        let ctext = case get "content" m of
              Nothing -> ""
              Just PNull -> ""
              Just v -> pyStrOf v
        if r == "user"
          then pure [obj [("role", str r), ("content", str ctext)]]
          else do
            let reasoning = case get "codex_reasoning_items" m of
                  Just (PList ris) -> [ri | ri@(PDict _) <- ris, maybe False truthy (get "encrypted_content" ri)]
                  _ -> []
                textItem = [obj [("role", str "assistant"), ("content", str ctext)] | not (T.null (T.strip ctext))]
                tcs = case get "tool_calls" m of
                  Just (PList xs) -> [tc | tc@(PDict _) <- xs]
                  _ -> []
            calls <- fmap concat . mapM call $ tcs
            pure (reasoning <> textItem <> calls)
      Just (PStr "tool") -> do
        let raw = fromMaybe PNull (get "tool_call_id" m)
            (cid0, _) = splitToolId raw
            cid = case cid0 of
              Just c | not (T.null (T.strip c)) -> Just c
              _ -> T.strip <$> nonBlank raw
        pure $ case cid of
          Just c | not (T.null (T.strip c)) ->
            let out = case get "content" m of
                  Just v | truthy v -> pyStrOf v
                  _ -> ""
             in [obj [("type", str "function_call_output"), ("call_id", str c), ("output", str out)]]
          _ -> []
      _ -> pure []
    one _ = pure []
    call tc = do
      let fn = fromMaybe (obj []) (get "function" tc)
      case get "name" fn >>= nonBlank of
        Nothing -> pure []
        Just name -> do
          let (embCall, embItem) = splitToolId (fromMaybe PNull (get "id" tc))
          cid <- case get "call_id" tc >>= nonBlank of
            Just c -> pure c
            Nothing -> case embCall of
              Just c | not (T.null (T.strip c)) -> pure c
              _ -> case embItem of
                Just i | "fc_" `T.isPrefixOf` i && T.length i > 3 -> pure ("call_" <> T.drop 3 i)
                _ -> ("call_" <>) . T.take 12 <$> eUuidHex env
          let args = T.strip (argText (fromMaybe (str "{}") (get "arguments" fn)))
          pure [obj [("type", str "function_call"), ("call_id", str (T.strip cid)), ("name", str name), ("arguments", str (if T.null args then "{}" else args))]]

-- | @_responses_tools@.
responsesTools :: Maybe [PyVal] -> Maybe [PyVal]
responsesTools = \case
  Just ts@(_ : _) ->
    let conv t =
          let fn = case t of
                PDict _ -> fromMaybe (obj []) (get "function" t)
                _ -> obj []
           in case get "name" fn >>= nonBlank of
                Nothing -> Nothing
                Just _ ->
                  Just $ obj
                    [ ("type", str "function"), ("name", fromMaybe PNull (get "name" fn))
                    , ("description", fromMaybe (str "") (get "description" fn)), ("strict", PBool False)
                    , ("parameters", fromMaybe (obj [("type", str "object"), ("properties", obj [])]) (get "parameters" fn)) ]
     in case mapMaybe conv ts of
          [] -> Nothing
          xs -> Just xs
  _ -> Nothing

-- | @_build_api_kwargs@ for codex_responses (before preflight).
buildKwargs :: Env -> Settings -> [PyVal] -> IO PyVal
buildKwargs env s msgs = do
  let (instr0, payload) = case msgs of
        (m : rest) | get "role" m == Just (PStr "system") ->
          (T.strip (case get "content" m of Just v | truthy v -> pyStrOf v; _ -> ""), rest)
        _ -> ("", msgs)
      instr = if T.null instr0 then defaultAgentIdentity else instr0
      (enabled, effort) = case sReasoning s of
        Just rc@(PDict _) | truthy rc ->
          if get "enabled" rc == Just (PBool False)
            then (False, "medium")
            else case get "effort" rc of
              Just e | truthy e -> (True, pyStrOf e)
              _ -> (True, "medium")
        _ -> (True, "medium")
  input <- responsesInput env payload
  let base =
        [ ("model", str (sModel s)), ("instructions", str instr), ("input", PList input)
        , ("tools", maybe PNull PList (responsesTools (sTools s))), ("tool_choice", str "auto")
        , ("parallel_tool_calls", PBool True), ("store", PBool False), ("prompt_cache_key", str (sSessionId s)) ]
      reasoning =
        if enabled
          then [("reasoning", obj [("effort", str effort), ("summary", str "auto")]), ("include", PList [str "reasoning.encrypted_content"])]
          else [("include", PList [])]
      maxTok = [("max_output_tokens", PInt n) | Just n <- [sMaxTokens s]]
  pure (obj (base <> reasoning <> maxTok))

-- | @_preflight_codex_api_kwargs@ (allow_stream=False): the request
-- revalidated and renormalized, or upstream's ValueError text.
preflight :: PyVal -> Either Text PyVal
preflight kw = do
  let required = ["model", "instructions", "input"]
      missing = filter (not . (`has` kw)) required
  if not (null missing) then Left ("Codex Responses request missing required field(s): " <> T.intercalate ", " (sortOn id missing) <> ".") else Right ()
  model <- case get "model" kw of
    Just (PStr m) | not (T.null (T.strip m)) -> Right (T.strip m)
    _ -> Left "Codex Responses request 'model' must be a non-empty string."
  let instr0 = case get "instructions" kw of
        Nothing -> ""
        Just PNull -> ""
        Just v -> pyStrOf v
      instr = let t = T.strip instr0 in if T.null t then defaultAgentIdentity else t
  input <- inputItems (fromMaybe PNull (get "input" kw))
  tools <- case get "tools" kw of
    Nothing -> Right PNull
    Just PNull -> Right PNull
    Just (PList ts) -> PList <$> mapM tool (zip [0 :: Int ..] ts)
    Just _ -> Left "Codex Responses request 'tools' must be a list when provided."
  case get "store" kw of
    Nothing -> Right ()
    Just (PBool False) -> Right ()
    Just _ -> Left "Codex Responses contract requires 'store' to be false."
  let allowed = ["model", "instructions", "input", "tools", "store", "reasoning", "include", "max_output_tokens", "temperature", "tool_choice", "parallel_tool_calls", "prompt_cache_key"]
      base = [("model", str model), ("instructions", str instr), ("input", PList input), ("tools", tools), ("store", PBool False)]
      reasoning = [("reasoning", r) | Just r@(PDict _) <- [get "reasoning" kw]]
      include = [("include", i) | Just i@(PList _) <- [get "include" kw]]
      maxOut = case get "max_output_tokens" kw of
        Just (PInt n) | n > 0 -> [("max_output_tokens", PInt n)]
        Just (PFloat d) | d > 0 -> [("max_output_tokens", PInt (truncate d))]
        _ -> []
      temp = case get "temperature" kw of
        Just (PInt n) -> [("temperature", PFloat (fromInteger n))]
        Just (PFloat d) -> [("temperature", PFloat d)]
        _ -> []
      passthrough = [(k, v) | k <- ["tool_choice", "parallel_tool_calls", "prompt_cache_key"], Just v <- [get k kw], v /= PNull]
  if has "stream" kw then Left "Codex Responses stream flag is only allowed in fallback streaming requests." else Right ()
  let unexpected = sortOn id [k | k <- keys kw, k `notElem` allowed]
  if not (null unexpected) then Left ("Codex Responses request has unsupported field(s): " <> T.intercalate ", " unexpected <> ".") else Right ()
  pure (obj (base <> reasoning <> include <> maxOut <> temp <> passthrough))
  where
    tool (idx, t@(PDict _)) = do
      if get "type" t /= Just (PStr "function")
        then Left ("Codex Responses tools[" <> tshow idx <> "] has unsupported type " <> maybe "None" pyRepr (get "type" t) <> ".")
        else Right ()
      name <- case get "name" t of
        Just (PStr n) | not (T.null (T.strip n)) -> Right (T.strip n)
        _ -> Left ("Codex Responses tools[" <> tshow idx <> "] is missing a valid name.")
      params <- case get "parameters" t of
        Just p@(PDict _) -> Right p
        _ -> Left ("Codex Responses tools[" <> tshow idx <> "] is missing valid parameters.")
      let desc = case get "description" t of
            Nothing -> ""
            Just PNull -> ""
            Just v -> pyStrOf v
          strict = case get "strict" t of
            Nothing -> False
            Just (PBool b) -> b
            Just v -> truthy v
      pure (obj [("type", str "function"), ("name", str name), ("description", str desc), ("strict", PBool strict), ("parameters", params)])
    tool (idx, _) = Left ("Codex Responses tools[" <> tshow idx <> "] must be an object.")
    inputItems = \case
      PList items -> catMaybes <$> mapM item (zip [0 :: Int ..] items)
      _ -> Left "Codex Responses input must be a list of input items."
    item (idx, it@(PDict _)) = case get "type" it of
      Just (PStr "function_call") -> do
        cid <- case get "call_id" it of
          Just (PStr c) | not (T.null (T.strip c)) -> Right (T.strip c)
          _ -> Left ("Codex Responses input[" <> tshow idx <> "] function_call is missing call_id.")
        name <- case get "name" it of
          Just (PStr n) | not (T.null (T.strip n)) -> Right (T.strip n)
          _ -> Left ("Codex Responses input[" <> tshow idx <> "] function_call is missing name.")
        let args = T.strip (argText (fromMaybe (str "{}") (get "arguments" it)))
        pure (Just (obj [("type", str "function_call"), ("call_id", str cid), ("name", str name), ("arguments", str (if T.null args then "{}" else args))]))
      Just (PStr "function_call_output") -> do
        cid <- case get "call_id" it of
          Just (PStr c) | not (T.null (T.strip c)) -> Right (T.strip c)
          _ -> Left ("Codex Responses input[" <> tshow idx <> "] function_call_output is missing call_id.")
        let out = case get "output" it of
              Nothing -> ""
              Just PNull -> ""
              Just v -> pyStrOf v
        pure (Just (obj [("type", str "function_call_output"), ("call_id", str cid), ("output", str out)]))
      Just (PStr "reasoning") -> case get "encrypted_content" it of
        Just (PStr e) | not (T.null e) ->
          Right . Just . obj $
            [("type", str "reasoning"), ("encrypted_content", str e)]
              <> [("id", str i) | Just (PStr i) <- [get "id" it], not (T.null i)]
              <> [("summary", case get "summary" it of Just l@(PList _) -> l; _ -> PList [])]
        _ -> Right Nothing
      ty -> case get "role" it of
        Just (PStr r) | r `elem` ["user", "assistant"] ->
          let c = case get "content" it of
                Nothing -> ""
                Just PNull -> ""
                Just v -> pyStrOf v
           in Right (Just (obj [("role", str r), ("content", str c)]))
        rl -> Left ("Codex Responses input[" <> tshow idx <> "] has unsupported item shape (type=" <> maybe "None" pyRepr ty <> ", role=" <> maybe "None" pyRepr rl <> ").")
    item (idx, _) = Left ("Codex Responses input[" <> tshow idx <> "] must be an object.")
    tshow = T.pack . show

-- | The streamed request: preflighted kwargs plus @stream: true@.
codexRequest :: Env -> Settings -> [PyVal] -> IO (Either Text HttpReq)
codexRequest env s msgs = do
  kw <- buildKwargs env s msgs
  pure $ do
    pre <- preflight kw
    -- upstream never sends tools: null (the SDK cannot even build such a
    -- request), so a request without tools leaves the field out
    let body = if get "tools" pre == Just PNull then delete "tools" pre else pre
        base = T.dropWhileEnd (== '/') (sBaseUrl s)
    pure (HttpReq "POST" (base <> "/responses") [("accept", "application/json"), ("authorization", "Bearer " <> sApiKey s), ("content-type", "application/json")] (Just (set "stream" (PBool True) body)) True)

-- | What one streamed attempt produced, read from its SSE events
-- (@responses.stream@ in the SDK).
data StreamOutcome
  = StreamCompleted PyVal
  -- ^ the final response
  | StreamNoCompleted
  -- ^ the stream ended without @response.completed@
  | StreamError Text
  -- ^ an event carried an @error@: the SDK's APIError message
  deriving (Eq, Show)

-- | The SDK's accumulator: the @response.completed@ payload, its output
-- rebuilt from @response.output_item.done@ when it is null.
accumulateStream :: [(Maybe Text, PyVal)] -> StreamOutcome
accumulateStream events = go [] events
  where
    go done = \case
      [] -> StreamNoCompleted
      ((_, d) : rest)
        | Just e <- get "error" d, truthy e -> StreamError (errMsg e)
        | get "type" d == Just (PStr "response.output_item.done") ->
            let idx = case get "output_index" d of
                  Just (PInt i) -> i
                  _ -> toInteger (length done)
             in go (done <> [(idx, fromMaybe PNull (get "item" d))]) rest
        | get "type" d == Just (PStr "response.completed") ->
            let r = fromMaybe PNull (get "response" d)
             in StreamCompleted $ case get "output" r of
                  Just PNull -> set "output" (PList (map snd (sortOn fst done))) r
                  _ -> r
        | otherwise -> go done rest
    errMsg e = case get "message" e of
      Just (PStr m) | not (T.null m) -> m
      _ -> "An error occurred during streaming"

-- | The create(stream=True) fallback: the first completed, incomplete or
-- failed response.
fallbackTerminal :: [(Maybe Text, PyVal)] -> Either Text PyVal
fallbackTerminal events =
  case [r | (_, d) <- events, Just (PStr t) <- [get "type" d], t `elem` ["response.completed", "response.incomplete", "response.failed"], Just r <- [get "response" d]] of
    (r : _) -> Right r
    [] -> Left "Responses create(stream=True) fallback did not emit a terminal response."

data NormalizedCodex = NormalizedCodex
  { ncContent :: Text
  , ncToolCalls :: [PyVal]
  , ncReasoning :: Maybe Text
  , ncReasoningItems :: Maybe [PyVal]
  , ncFinish :: Text
  }

-- | @_normalize_codex_response@: Left is the RuntimeError it raises.
normalizeResponse :: IO Text -> PyVal -> IO (Either Text NormalizedCodex)
normalizeResponse uuidHex resp = case get "output" resp of
  Just (PList output@(_ : _)) -> do
    let status = case get "status" resp of
          Just (PStr s) -> Just (T.toLower (T.strip s))
          _ -> Nothing
    if status `elem` [Just "failed", Just "cancelled"]
      then
        pure . Left $ case get "error" resp of
          Just e@(PDict _) -> case get "message" e of
            Just m | truthy m -> pyStrOf m
            _ -> pyStrOf e
          Just e | truthy e -> pyStrOf e
          _ -> "Responses API returned status '" <> fromMaybe "" status <> "'"
      else do
        calls <- fmap concat . mapM callOf $ output
        let itemStatus it = case get "status" it of
              Just (PStr s) -> Just (T.toLower (T.strip s))
              _ -> Nothing
            pending s = s `elem` map Just ["queued", "in_progress", "incomplete"]
            incomplete = pending status || any (pending . itemStatus) output
            phases = [T.toLower (T.strip p) | it <- output, get "type" it == Just (PStr "message"), Just (PStr p) <- [get "phase" it]]
            commentary = any (`elem` ["commentary", "analysis"]) phases
            final = any (`elem` ["final_answer", "final"]) phases
            texts = [t | it <- output, get "type" it == Just (PStr "message"), let t = messageText it, not (T.null t)]
            reasonings = [t | it <- output, get "type" it == Just (PStr "reasoning"), let t = reasoningText it, not (T.null t)]
            rawItems = mapMaybe rawReasoning [it | it <- output, get "type" it == Just (PStr "reasoning")]
            text0 = T.strip (T.intercalate "\n" texts)
            outputText = T.concat [t | it <- output, get "type" it == Just (PStr "message"), Just (PList ps) <- [get "content" it], p <- ps, get "type" p == Just (PStr "output_text"), Just (PStr t) <- [get "text" p]]
            text = if T.null text0 then T.strip outputText else text0
            finish
              | not (null calls) = "tool_calls"
              | incomplete || (commentary && not final) = "incomplete"
              | otherwise = "stop"
        pure . Right $
          NormalizedCodex text calls (if null reasonings then Nothing else Just (T.strip (T.intercalate "\n\n" reasonings))) (if null rawItems then Nothing else Just rawItems) finish
  _ -> pure (Left "Responses API returned no output items")
  where
    messageText it = case get "content" it of
      Just (PList ps) -> T.strip (T.concat [t | p <- ps, get "type" p `elem` [Just (PStr "output_text"), Just (PStr "text")], Just (PStr t) <- [get "text" p], not (T.null t)])
      _ -> ""
    reasoningText it =
      let fromSummary = case get "summary" it of
            Just (PList ps) -> [t | p <- ps, Just (PStr t) <- [get "text" p], not (T.null t)]
            _ -> []
       in if not (null fromSummary)
            then T.strip (T.intercalate "\n" fromSummary)
            else case get "text" it of
              Just (PStr t) | not (T.null t) -> T.strip t
              _ -> ""
    rawReasoning it = case get "encrypted_content" it of
      Just (PStr e) | not (T.null e) ->
        Just . obj $
          [("type", str "reasoning"), ("encrypted_content", str e)]
            <> [("id", str i) | Just (PStr i) <- [get "id" it], not (T.null i)]
            <> [("summary", PList [obj [("type", str "summary_text"), ("text", str t)] | p <- ps, Just (PStr t) <- [get "text" p]]) | Just (PList ps) <- [get "summary" it]]
      _ -> Nothing
    callOf it = case get "type" it of
      Just (PStr ty) | ty `elem` ["function_call", "custom_tool_call"] -> do
        let st = case get "status" it of
              Just (PStr s) -> Just (T.toLower (T.strip s))
              _ -> Nothing
        if ty == "function_call" && st `elem` map Just ["queued", "in_progress", "incomplete"]
          then pure []
          else do
            let name = case get "name" it of
                  Just v | truthy v -> v
                  _ -> str ""
                argKey = if ty == "function_call" then "arguments" else "input"
                args = case fromMaybe (str "{}") (get argKey it) of
                  PStr s -> s
                  v -> dumpsUnicode v
                rawId = fromMaybe PNull (get "id" it)
                (emb, _) = splitToolId rawId
            cid <- case get "call_id" it >>= nonBlank of
              Just c -> pure c
              Nothing -> case emb of
                Just c | not (T.null (T.strip c)) -> pure c
                _ -> ("call_" <>) . T.take 12 <$> uuidHex
            let cid' = T.strip cid
            rid <- deriveFunctionCallId uuidHex cid' (case rawId of PStr r -> Just r; _ -> Nothing)
            pure [obj [("id", str cid'), ("call_id", str cid'), ("response_item_id", str rid), ("type", str "function"), ("function", obj [("name", name), ("arguments", str args)])]]
      _ -> pure []
