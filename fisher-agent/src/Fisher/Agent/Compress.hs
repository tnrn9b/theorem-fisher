-- | Context compression (@agent/context_compressor.py@): keep the first
-- few and last few messages, replace the middle with a handoff summary
-- the auxiliary model writes, and repair tool-call / tool-result pairs the
-- cut separated.
module Fisher.Agent.Compress
  ( Compressor (..)
  , newCompressor
  , summaryPrefix
  , legacySummaryPrefix
  , withSummaryPrefix
  , updateFromResponse
  , shouldCompress
  , compress
  , generateSummary
  , summaryPrompt
  , sanitizeToolPairs
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Agent.Metadata (estimateMessagesTokensRough)
import Fisher.Agent.Types
import Fisher.LLM.Prepare (sanitizeToolPairs)
import Fisher.Py.Value
import Fisher.Tools.Py (pyStrip)

data Compressor = Compressor
  { cModel :: Text
  , cBaseUrl :: Text
  , cThresholdPercent :: Double
  , cProtectFirst :: Int
  , cProtectLast :: Int
  , cSummaryTarget :: Integer
  , cQuiet :: Bool
  , cContextLength :: Integer
  , cThresholdTokens :: Integer
  , cCompressionCount :: Int
  , cContextProbed :: Bool
  , cLastPrompt :: Integer
  , cLastCompletion :: Integer
  , cLastTotal :: Integer
  , cSummaryModel :: Text
  }
  deriving (Eq, Show)

-- | @ContextCompressor(model, threshold_percent, protect_first_n,
-- protect_last_n, summary_target_tokens, quiet_mode,
-- summary_model_override, base_url)@ with the context length already
-- looked up.
newCompressor :: Text -> Text -> Double -> Int -> Int -> Integer -> Bool -> Maybe Text -> Integer -> Compressor
newCompressor model baseUrl pct first lastN target quiet override ctx =
  Compressor model baseUrl pct first lastN target quiet ctx (truncate (fromIntegral ctx * pct)) 0 False 0 0 0 (maybe "" id override)

summaryPrefix, legacySummaryPrefix :: Text
summaryPrefix =
  "[CONTEXT COMPACTION] Earlier turns in this conversation were compacted to save context space. The summary below \
  \describes work that was already completed, and the current session state may still reflect that work (for example, \
  \files may already be changed). Use the summary and the current state to continue from where things left off, and \
  \avoid repeating work:"
legacySummaryPrefix = "[CONTEXT SUMMARY]:"

-- | @_with_summary_prefix@
withSummaryPrefix :: Text -> Text
withSummaryPrefix summary =
  let t0 = pyStrip summary
      t = case [p | p <- [legacySummaryPrefix, summaryPrefix], p `T.isPrefixOf` t0] of
        (p : _) -> T.dropWhile isPyWs (T.drop (T.length p) t0)
        [] -> t0
   in if T.null t then summaryPrefix else summaryPrefix <> "\n" <> t
  where
    isPyWs c = T.null (pyStrip (T.singleton c))

-- | @update_from_response(usage)@
updateFromResponse :: Integer -> Integer -> Integer -> Compressor -> Compressor
updateFromResponse p c t comp = comp {cLastPrompt = p, cLastCompletion = c, cLastTotal = t}

shouldCompress :: Compressor -> Integer -> Bool
shouldCompress comp tokens = tokens >= cThresholdTokens comp

-- | The prompt @_generate_summary@ sends.
summaryPrompt :: Integer -> [PyVal] -> Text
summaryPrompt target turns =
  "Create a concise handoff summary for a later assistant that will continue this conversation after earlier turns are compacted.\n\n\
  \Describe:\n\
  \1. What actions were taken (tool calls, searches, file operations)\n\
  \2. Key information or results obtained\n\
  \3. Important decisions, constraints, or user preferences\n\
  \4. Relevant data, file names, outputs, or next steps needed to continue\n\n\
  \Keep it factual, concise, and focused on helping the next assistant resume without repeating work. Target ~"
    <> T.pack (show target)
    <> " tokens.\n\n---\nTURNS TO SUMMARIZE:\n"
    <> T.intercalate "\n\n" (map part turns)
    <> "\n---\n\nWrite only the summary body. Do not include any preamble or prefix; the system will add the handoff wrapper."
  where
    part m =
      let role = case get "role" m of
            Just (PStr r) -> r
            _ -> "unknown"
          c0 = case get "content" m of
            Just (PStr s) -> s
            Just v | truthy v -> pyStrOf v
            _ -> ""
          c1 = if T.length c0 > 2000 then T.take 1000 c0 <> "\n...[truncated]...\n" <> T.takeEnd 500 c0 else c0
          names = case get "tool_calls" m of
            Just (PList tcs@(_ : _)) -> Just [maybe "?" pyStrOf (get "function" tc >>= get "name") | tc@(PDict _) <- tcs]
            _ -> Nothing
          c2 = maybe c1 (\ns -> c1 <> "\n[Tool calls: " <> T.intercalate ", " ns <> "]") names
       in "[" <> T.toUpper role <> "]: " <> c2

-- | @_generate_summary(turns)@: the prefixed summary, or 'Nothing' when no
-- provider answered (a warning is logged and the turns are dropped).
generateSummary :: AuxLLM -> (Text -> IO ()) -> Compressor -> [PyVal] -> IO (Maybe Text)
generateSummary llm warn comp turns = do
  let call =
        (auxCall "compression" [PDict [("role", PStr "user"), ("content", PStr (summaryPrompt (cSummaryTarget comp) turns))]])
          { axTemperature = Just 0.3
          , axMaxTokens = Just (cSummaryTarget comp * 2)
          , axModel = if T.null (cSummaryModel comp) then Nothing else Just (cSummaryModel comp) }
  r <- llm call
  case r of
    Left (AuxNoProvider _) -> do
      warn "Context compression: no provider available for summary. Middle turns will be dropped without summary."
      pure Nothing
    Left (AuxFailed e) -> do
      warn ("Failed to generate context summary: " <> e)
      pure Nothing
    Right resp -> do
      let content = case responseContent resp of
            PStr s -> s
            v | truthy v -> pyStrOf v
            _ -> ""
      pure (Just (withSummaryPrefix (pyStrip content)))

-- | @compress(messages, current_tokens)@: the new list (the same list
-- when there is too little to compress) and the compressor after it.
compress :: AuxLLM -> (Text -> IO ()) -> (Text -> IO ()) -> Compressor -> [PyVal] -> Maybe Integer -> IO ([PyVal], Compressor)
compress llm out warn comp msgs current = do
  let n = length msgs
      minimum' = cProtectFirst comp + cProtectLast comp + 1
      say t = if cQuiet comp then pure () else out t
  if n <= minimum'
    then do
      say ("⚠️  Cannot compress: only " <> tshow n <> " messages (need > " <> tshow minimum' <> ")")
      pure (msgs, comp)
    else do
      let start0 = cProtectFirst comp
          end0 = n - cProtectLast comp
      if start0 >= end0
        then pure (msgs, comp)
        else do
          let start = alignForward start0
              end = alignBackward end0
          if start >= end
            then pure (msgs, comp)
            else do
              let turns = take (end - start) (drop start msgs)
                  display = case current of
                    Just c | c /= 0 -> c
                    _ -> if cLastPrompt comp /= 0 then cLastPrompt comp else estimateMessagesTokensRough msgs
              say ("\n📦 Context compression triggered (" <> commas display <> " tokens ≥ " <> commas (cThresholdTokens comp) <> " threshold)")
              say ("   📊 Model context limit: " <> commas (cContextLength comp) <> " tokens (" <> fmtF 0 (cThresholdPercent comp * 100) <> "% = " <> commas (cThresholdTokens comp) <> ")")
              say ("   🗜️  Summarizing turns " <> tshow (start + 1) <> "-" <> tshow end <> " (" <> tshow (length turns) <> " turns)")
              summary <- generateSummary llm warn comp turns
              let headMsgs =
                    [ if i == 0 && role m == "system" && cCompressionCount comp == 0
                        then set "content" (PStr (contentText m <> "\n\n[Note: Some earlier conversation turns have been compacted into a handoff summary to preserve context space. The current session state may still reflect earlier work, so build on that summary and state rather than re-doing work.]")) m
                        else m
                    | (i, m) <- zip [0 :: Int ..] (take start msgs) ]
                  lastHeadRole = if start > 0 then maybe "user" pyStrOf (get "role" (msgs !! (start - 1))) else "user"
                  summaryRole = if lastHeadRole `elem` ["assistant", "tool"] then "user" else "assistant"
              mid <- case summary of
                Just s -> pure [PDict [("role", PStr summaryRole), ("content", PStr s)]]
                Nothing -> do
                  say "   ⚠️  No summary model available — middle turns dropped without summary"
                  pure []
              let comp' = comp {cCompressionCount = cCompressionCount comp + 1}
                  compressed = sanitizeToolPairs (headMsgs <> mid <> drop end msgs)
              if cQuiet comp
                then pure ()
                else do
                  let saved = display - estimateMessagesTokensRough compressed
                  out ("   ✅ Compressed: " <> tshow n <> " → " <> tshow (length compressed) <> " messages (~" <> commas saved <> " tokens saved)")
                  out ("   💡 Compression #" <> tshow (cCompressionCount comp') <> " complete")
              pure (compressed, comp')
  where
    role m = case get "role" m of
      Just (PStr r) -> r
      _ -> ""
    contentText m = case get "content" m of
      Just (PStr s) -> s
      Just v | truthy v -> pyStrOf v
      _ -> ""
    alignForward i
      | i < length msgs && role (msgs !! i) == "tool" = alignForward (i + 1)
      | otherwise = i
    alignBackward i
      | i <= 0 || i >= length msgs = i
      | otherwise =
          let prev = msgs !! (i - 1)
           in if role prev == "assistant" && maybe False truthy (get "tool_calls" prev) then i - 1 else i

tshow :: Int -> Text
tshow = T.pack . show
