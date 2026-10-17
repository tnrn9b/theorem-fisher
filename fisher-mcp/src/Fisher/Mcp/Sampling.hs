-- | Answering a server's @sampling/createMessage@ as upstream's
-- @SamplingHandler@ does: a per-server sliding rate limit, model choice
-- (config, then the server's hints) checked against an allow-list, MCP
-- messages turned into chat messages, the call to an LLM, and the reply as
-- text (credential-redacted) or tool calls, with tool loops bounded.
module Fisher.Mcp.Sampling
  ( SamplingCall (..)
  , samplingCallDict
  , SamplingLLM
  , SamplingHandler
  , newSamplingHandler
  , handleCreateMessage
  , samplingMetrics
  , pySafeNumeric
  ) where

import Control.Exception (SomeException, displayException, try)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Timeout (timeout)

import Fisher.Py.Value (PyVal (..), dumpsAscii, get, loads, pyRepr, truthy)
import Fisher.Tools.Py (pyFloat, pyInt)
import Fisher.Tools.Redact (sanitizeError)
import qualified Data.Text.Encoding as TE

-- | What upstream passes to @call_llm(task="mcp", ...)@.
data SamplingCall = SamplingCall
  { scModel :: Maybe Text
  , scMessages :: [PyVal]
  , scTemperature :: PyVal
  , scMaxTokens :: Integer
  , scTools :: Maybe [PyVal]
  , scTimeout :: PyVal
  }

samplingCallDict :: SamplingCall -> PyVal
samplingCallDict c =
  PDict
    [ ("task", PStr "mcp")
    , ("model", maybe PNull PStr (scModel c))
    , ("messages", PList (scMessages c))
    , ("temperature", scTemperature c)
    , ("max_tokens", PInt (scMaxTokens c))
    , ("tools", maybe PNull PList (scTools c))
    , ("timeout", scTimeout c)
    ]

-- | The LLM: an OpenAI-shaped response dict (@choices@, @model@, @usage@),
-- or an exception.
type SamplingLLM = SamplingCall -> IO PyVal

data SamplingHandler = SamplingHandler
  { shServer :: Text
  , shMaxRpm :: Integer
  , shTimeout :: PyVal
  , shMaxTokensCap :: Integer
  , shMaxToolRounds :: Integer
  , shModelOverride :: PyVal
  , shAllowed :: [Text]
  , shAudit :: Text
  , shStamps :: IORef [Double]
  , shToolLoops :: IORef Integer
  , shMetrics :: IORef (Integer, Integer, Integer, Integer)
  , shLog :: Text -> Text -> IO ()
  , shLLM :: SamplingLLM
  }

-- | @_safe_numeric(value, default, coerce, minimum)@ with @int@ (True) or
-- @float@ (False) as the coercion.
pySafeNumeric :: Bool -> PyVal -> PyVal -> Integer -> PyVal
pySafeNumeric asInt v def lowest =
  let coerced
        | asInt = case v of
            PInt n -> Just (PInt n)
            PBool b -> Just (PInt (if b then 1 else 0))
            PFloat d | not (isNaN d || isInfinite d) -> Just (PInt (truncate d))
            PStr s -> PInt <$> pyInt s
            _ -> Nothing
        | otherwise = case v of
            PInt n -> Just (PFloat (fromIntegral n))
            PBool b -> Just (PFloat (if b then 1 else 0))
            PFloat d -> Just (PFloat d)
            PStr s -> PFloat <$> pyFloat s
            _ -> Nothing
   in case coerced of
        Nothing -> def
        Just (PFloat d)
          | isNaN d || isInfinite d -> def
          | d < fromIntegral lowest -> PInt lowest
          | otherwise -> PFloat d
        Just (PInt n) -> PInt (max n lowest)
        Just x -> x

asInteger :: PyVal -> Integer
asInteger = \case
  PInt n -> n
  PFloat d -> truncate d
  _ -> 0

asSeconds :: PyVal -> Double
asSeconds = \case
  PInt n -> fromIntegral n
  PFloat d -> d
  _ -> 30

newSamplingHandler :: Text -> PyVal -> (Text -> Text -> IO ()) -> SamplingLLM -> IO SamplingHandler
newSamplingHandler server cfg logf llm = do
  let g k d = fromMaybe d (get k cfg)
      level = T.toLower (case g "log_level" (PStr "info") of PStr s -> s; v -> pyRepr v)
  SamplingHandler server
    (asInteger (pySafeNumeric True (g "max_rpm" (PInt 10)) (PInt 10) 1))
    (pySafeNumeric False (g "timeout" (PInt 30)) (PInt 30) 1)
    (asInteger (pySafeNumeric True (g "max_tokens_cap" (PInt 4096)) (PInt 4096) 1))
    (asInteger (pySafeNumeric True (g "max_tool_rounds" (PInt 5)) (PInt 5) 0))
    (g "model" PNull)
    [s | PStr s <- case g "allowed_models" (PList []) of PList xs -> xs; _ -> []]
    (if level `elem` ["debug", "info", "warning"] then T.toUpper level else "INFO")
    <$> newIORef []
    <*> newIORef 0
    <*> newIORef (0, 0, 0, 0)
    <*> pure logf
    <*> pure llm

-- | @{"requests", "errors", "tokens_used", "tool_use_count"}@.
samplingMetrics :: SamplingHandler -> IO PyVal
samplingMetrics h = do
  (r, e, t, u) <- readIORef (shMetrics h)
  pure (PDict [("requests", PInt r), ("errors", PInt e), ("tokens_used", PInt t), ("tool_use_count", PInt u)])

bumpErr, bumpReq, bumpToolUse :: SamplingHandler -> IO ()
bumpErr h = modifyIORef' (shMetrics h) (\(r, e, t, u) -> (r, e + 1, t, u))
bumpReq h = modifyIORef' (shMetrics h) (\(r, e, t, u) -> (r + 1, e, t, u))
bumpToolUse h = modifyIORef' (shMetrics h) (\(r, e, t, u) -> (r, e, t, u + 1))

err :: Text -> Either (Integer, Text, Maybe PyVal) PyVal
err m = Left (-1, m, Nothing)

-- | The SDK's validation of the request, then the handler.
handleCreateMessage :: SamplingHandler -> PyVal -> IO (Either (Integer, Text, Maybe PyVal) PyVal)
handleCreateMessage h params = case (get "messages" params, get "maxTokens" params) of
  (Just (PList msgs), Just (PInt maxTok)) | all validMsg msgs -> run msgs maxTok
  _ -> pure (Left (-32602, "Invalid request parameters", Just (PStr "")))
  where
    validMsg m = case (get "role" m, get "content" m) of
      (Just (PStr r), Just c) -> r `elem` ["user", "assistant"] && validContent c
      _ -> False
    validContent = \case
      PList bs -> all validBlock bs
      b -> validBlock b
    validBlock b = case get "type" b of
      Just (PStr t) -> t `elem` ["text", "image", "audio", "tool_use", "tool_result"]
      _ -> False
    srv = shServer h
    run msgs maxTok = do
      now <- realToFrac <$> getPOSIXTime
      allowed <- atomicModifyIORef' (shStamps h) $ \ts ->
        let kept = [t | t <- ts, t > now - 60]
         in if fromIntegral (length kept) >= shMaxRpm h then (kept, False) else (kept <> [now], True)
      if not allowed
        then do
          shLog h "WARNING" ("MCP server '" <> srv <> "' sampling rate limit exceeded (" <> tshow (shMaxRpm h) <> "/min)")
          bumpErr h
          pure (err ("Sampling rate limit exceeded for server '" <> srv <> "' (" <> tshow (shMaxRpm h) <> " requests/minute)"))
        else do
          let override = case shModelOverride h of
                PStr s | not (T.null s) -> Just s
                _ -> Nothing
              hint = case get "modelPreferences" params >>= get "hints" of
                Just (PList hs) -> case [n | hh <- hs, Just (PStr n) <- [get "name" hh], not (T.null n)] of
                  (n : _) -> Just n
                  [] -> Nothing
                _ -> Nothing
              resolved = fromMaybe "" (maybe hint Just override)
          if not (null (shAllowed h)) && not (T.null resolved) && resolved `notElem` shAllowed h
            then do
              shLog h "WARNING" ("MCP server '" <> srv <> "' requested model '" <> resolved <> "' not in allowed_models")
              bumpErr h
              pure (err ("Model '" <> resolved <> "' not allowed for server '" <> srv <> "'. Allowed: " <> T.intercalate ", " (shAllowed h)))
            else do
              converted <- concat <$> mapM convert msgs
              let messages = case get "systemPrompt" params of
                    Just sp@(PStr s) | not (T.null s) -> PDict [("role", PStr "system"), ("content", sp)] : converted
                    _ -> converted
                  maxTokens = min maxTok (shMaxTokensCap h)
                  temperature = fromMaybe PNull (get "temperature" params)
                  tools = case get "tools" params of
                    Just (PList ts@(_ : _)) ->
                      Just
                        [ PDict
                            [ ("type", PStr "function")
                            , ( "function"
                              , PDict
                                  [ ("name", fromMaybe (PStr "") (get "name" t))
                                  , ("description", case get "description" t of Just d | truthy d -> d; _ -> PStr "")
                                  , ("parameters", case get "inputSchema" t of Just s | truthy s -> s; _ -> PDict [])
                                  ]
                              )
                            ]
                        | t <- ts
                        ]
                    _ -> Nothing
              shLog h (shAudit h) ("MCP server '" <> srv <> "' sampling request: model=" <> resolved <> ", max_tokens=" <> tshow maxTokens <> ", messages=" <> tshow (length messages))
              let call = SamplingCall (if T.null resolved then Nothing else Just resolved) messages temperature maxTokens tools (shTimeout h)
              r <- try (timeout (round (asSeconds (shTimeout h) * 1e6)) (shLLM h call >>= \x -> length (show x) `seq` pure x))
              case r of
                Right Nothing -> do
                  bumpErr h
                  pure (err ("Sampling LLM call timed out after " <> pyRepr (shTimeout h) <> "s for server '" <> srv <> "'"))
                Left (e :: SomeException) -> do
                  bumpErr h
                  pure (err ("Sampling LLM call failed: " <> sanitizeError (T.pack (displayException e))))
                Right (Just resp) -> respond resp
    respond resp = case get "choices" resp of
      Just (PList (choice : _)) -> do
        bumpReq h
        case get "usage" resp >>= get "total_tokens" of
          Just (PInt n) -> modifyIORef' (shMetrics h) (\(a, b, t, u) -> (a, b, t + n, u))
          _ -> pure ()
        let msg = fromMaybe PNull (get "message" choice)
            finish = get "finish_reason" choice
            toolCalls = case get "tool_calls" msg of
              Just (PList tcs@(_ : _)) -> Just tcs
              _ -> Nothing
            model = fromMaybe PNull (get "model" resp)
            tokens = case get "usage" resp of
              Just u -> maybe "?" pyStrLike (get "total_tokens" u)
              Nothing -> "?"
        case (finish, toolCalls) of
          (Just (PStr "tool_calls"), Just tcs) -> toolUse tcs model tokens
          _ -> do
            writeIORef (shToolLoops h) 0
            let text = case get "content" msg of
                  Just (PStr t) -> t
                  _ -> ""
                stop = case finish of
                  Just (PStr "stop") -> "endTurn"
                  Just (PStr "length") -> "maxTokens"
                  Just (PStr "tool_calls") -> "toolUse"
                  _ -> "endTurn"
            shLog h (shAudit h) ("MCP server '" <> srv <> "' sampling response: model=" <> pyStrLike model <> ", tokens=" <> tokens)
            pure (Right (PDict [("role", PStr "assistant"), ("content", PDict [("type", PStr "text"), ("text", PStr (sanitizeError text))]), ("model", model), ("stopReason", PStr stop)]))
      _ -> do
        bumpErr h
        pure (err ("LLM returned empty response (no choices) for server '" <> srv <> "'"))
    toolUse tcs model tokens = do
      bumpToolUse h
      if shMaxToolRounds h == 0
        then do
          writeIORef (shToolLoops h) 0
          pure (err ("Tool loops disabled for server '" <> srv <> "' (max_tool_rounds=0)"))
        else do
          n <- atomicModifyIORef' (shToolLoops h) (\c -> (c + 1, c + 1))
          if n > shMaxToolRounds h
            then do
              writeIORef (shToolLoops h) 0
              pure (err ("Tool loop limit exceeded for server '" <> srv <> "' (max " <> tshow (shMaxToolRounds h) <> " rounds)"))
            else do
              blocks <- mapM block tcs
              shLog h (shAudit h) ("MCP server '" <> srv <> "' sampling response: model=" <> pyStrLike model <> ", tokens=" <> tokens <> ", tool_calls=" <> tshow (length blocks))
              pure (Right (PDict [("role", PStr "assistant"), ("content", PList blocks), ("model", model), ("stopReason", PStr "toolUse")]))
    block tc = do
      let fn = fromMaybe PNull (get "function" tc)
          args = fromMaybe PNull (get "arguments" fn)
      parsed <- case args of
        PStr s -> case loads (TE.encodeUtf8 s) of
          Right v -> pure v
          Left _ -> do
            shLog h "WARNING" ("MCP server '" <> srv <> "': malformed tool_calls arguments from LLM (wrapping as raw): " <> T.take 100 s)
            pure (PDict [("_raw", PStr s)])
        PDict _ -> pure args
        other -> pure (PDict [("_raw", PStr (pyStrLike other))])
      pure (PDict [("type", PStr "tool_use"), ("id", fromMaybe PNull (get "id" tc)), ("name", fromMaybe PNull (get "name" fn)), ("input", parsed)])
    pyStrLike = \case
      PStr s -> s
      v -> pyRepr v
    convert m = do
      let role = fromMaybe (PStr "user") (get "role" m)
          blocks = case get "content" m of
            Just (PList bs) -> bs
            Just b -> [b]
            Nothing -> []
          ty b = case get "type" b of Just (PStr t) -> t; _ -> ""
          toolResults = [b | b <- blocks, ty b == "tool_result"]
          toolUses = [b | b <- blocks, ty b == "tool_use"]
          contentBlocks = [b | b <- blocks, ty b `notElem` ["tool_result", "tool_use"]]
          textOf b = case get "text" b of Just (PStr t) | ty b == "text" -> Just t; _ -> Nothing
          trs =
            [ PDict
                [ ("role", PStr "tool")
                , ("tool_call_id", fromMaybe PNull (get "toolUseId" tr))
                , ("content", PStr (T.intercalate "\n" [t | item <- case get "content" tr of Just (PList xs) -> xs; Just x | x /= PNull -> [x]; _ -> [], Just t <- [textOf item]]))
                ]
            | tr <- toolResults
            ]
      rest <-
        if not (null toolUses)
          then do
            let calls =
                  [ PDict
                      [ ("id", fromMaybe (PStr ("call_" <> tshow i)) (get "id" tu))
                      , ("type", PStr "function")
                      , ( "function"
                        , PDict
                            [ ("name", fromMaybe PNull (get "name" tu))
                            , ("arguments", PStr (case get "input" tu of Just d@(PDict _) -> dumpsAscii d; Just v -> pyStrLike v; Nothing -> ""))
                            ]
                        )
                      ]
                  | (i, tu) <- zip [0 :: Int ..] toolUses
                  ]
                texts = [t | b <- contentBlocks, Just t <- [textOf b]]
            pure [PDict ([("role", role), ("tool_calls", PList calls)] <> [("content", PStr (T.intercalate "\n" texts)) | not (null texts)])]
          else case contentBlocks of
            [] -> pure []
            [b] | Just t <- textOf b -> pure [PDict [("role", role), ("content", PStr t)]]
            bs -> do
              parts <- fmap concat . mapM part $ bs
              pure [PDict [("role", role), ("content", PList parts)] | not (null parts)]
      pure (trs <> rest)
    part b = case (textOf' b, get "data" b, get "mimeType" b) of
      (Just t, _, _) -> pure [PDict [("type", PStr "text"), ("text", PStr t)]]
      (_, Just (PStr d), Just (PStr mt)) -> pure [PDict [("type", PStr "image_url"), ("image_url", PDict [("url", PStr ("data:" <> mt <> ";base64," <> d))])]]
      _ -> do
        shLog h "WARNING" ("Unsupported sampling content block type: " <> blockClass b <> " (skipped)")
        pure []
    textOf' b = case (get "type" b, get "text" b) of
      (Just (PStr "text"), Just (PStr t)) -> Just t
      _ -> Nothing
    blockClass b = case get "type" b of
      Just (PStr "image") -> "ImageContent"
      Just (PStr "audio") -> "AudioContent"
      _ -> "dict"

tshow :: Show a => a -> Text
tshow = T.pack . show
