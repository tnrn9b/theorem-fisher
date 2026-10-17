-- | How big a model's context is (@agent/model_metadata.py@): the
-- context-length cache on disk (lengths learned from errors), OpenRouter's
-- @/models@ list (fetched once an hour per process), the built-in table
-- and the probe tiers; the limit a provider names in an error; and the
-- rough token estimates the compressor triggers on.
module Fisher.Agent.Metadata
  ( MetaEnv (..)
  , newModelCache
  , ModelCache
  , contextProbeTiers
  , defaultContextLengths
  , fetchModelMetadata
  , getModelContextLength
  , getCachedContextLength
  , saveContextLength
  , contextCachePath
  , nextProbeTier
  , parseContextLimitFromError
  , estimateTokensRough
  , estimateMessagesTokensRough
  , openRouterModelsUrl
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

import Fisher.LLM.Auth.PyYaml (dumpPyYaml, parsePyYaml)
import Fisher.LLM.Http
import Fisher.LLM.Types (HttpReq (..))
import qualified Fisher.Py.Re as Re
import Fisher.Py.Value

-- | The in-process copy of OpenRouter's model list and when it was fetched.
newtype ModelCache = ModelCache (IORef ([(Text, PyVal)], Double))

newModelCache :: IO ModelCache
newModelCache = ModelCache <$> newIORef ([], 0)

data MetaEnv = MetaEnv
  { meTransport :: Transport
  , meNow :: IO Double
  , meGaussHome :: FilePath
  , meCache :: ModelCache
  , meWarn :: Text -> IO ()
  }

openRouterModelsUrl :: Text
openRouterModelsUrl = "https://openrouter.ai/api/v1/models"

contextProbeTiers :: [Integer]
contextProbeTiers = [2000000, 1000000, 512000, 200000, 128000, 64000, 32000]

-- | @DEFAULT_CONTEXT_LENGTHS@, in its order (the fuzzy match takes the
-- first hit).
defaultContextLengths :: [(Text, Integer)]
defaultContextLengths =
  [ ("anthropic/claude-opus-4", 200000), ("anthropic/claude-opus-4.5", 200000), ("anthropic/claude-opus-4.6", 200000)
  , ("anthropic/claude-sonnet-4", 200000), ("anthropic/claude-sonnet-4-20250514", 200000)
  , ("anthropic/claude-haiku-4.5", 200000), ("claude-opus-4-6", 200000), ("claude-sonnet-4-6", 200000)
  , ("claude-opus-4-5-20251101", 200000), ("claude-sonnet-4-5-20250929", 200000), ("claude-opus-4-1-20250805", 200000)
  , ("claude-opus-4-20250514", 200000), ("claude-sonnet-4-20250514", 200000), ("claude-haiku-4-5-20251001", 200000)
  , ("openai/gpt-4o", 128000), ("openai/gpt-4-turbo", 128000), ("openai/gpt-4o-mini", 128000)
  , ("google/gemini-2.0-flash", 1048576), ("google/gemini-2.5-pro", 1048576)
  , ("meta-llama/llama-3.3-70b-instruct", 131072), ("deepseek/deepseek-chat-v3", 65536)
  , ("qwen/qwen-2.5-72b-instruct", 32768), ("glm-4.7", 202752), ("glm-5", 202752), ("glm-4.5", 131072)
  , ("glm-4.5-flash", 131072), ("kimi-for-coding", 262144), ("kimi-k2.5", 262144), ("kimi-k2-thinking", 262144)
  , ("kimi-k2-thinking-turbo", 262144), ("kimi-k2-turbo-preview", 262144), ("kimi-k2-0905-preview", 131072)
  , ("MiniMax-M2.5", 204800), ("MiniMax-M2.5-highspeed", 204800), ("MiniMax-M2.1", 204800) ]

-- | @fetch_model_metadata()@: model id (and canonical slug) → its entry.
-- A failure keeps whatever was fetched before (or nothing).
fetchModelMetadata :: MetaEnv -> IO [(Text, PyVal)]
fetchModelMetadata env = do
  let ModelCache ref = meCache env
  (cached, at) <- readIORef ref
  now <- meNow env
  if not (null cached) && now - at < 3600
    then pure cached
    else do
      r <- try (tRequest (meTransport env) (HttpReq "GET" openRouterModelsUrl [] Nothing False) (Timeouts 10 10))
      let failWith e = do
            meWarn env ("Failed to fetch model metadata from OpenRouter: " <> e)
            pure cached
      case r of
        Left (e :: SomeException) -> failWith (T.pack (show e))
        Right (Left e) -> failWith (T.pack (show e))
        Right (Right resp)
          | rStatus resp >= 400 -> failWith ("HTTP " <> T.pack (show (rStatus resp)))
          | otherwise -> case loads (rBody resp) of
              Left e -> failWith (T.pack e)
              Right d -> do
                let models = case get "data" d of
                      Just (PList ms) -> ms
                      _ -> []
                    entries = concatMap entry models
                    -- a later id replaces an earlier one in place, as dict assignment does
                    merged = foldl (\acc (k, v) -> if any ((== k) . fst) acc then [(k', if k' == k then v else v') | (k', v') <- acc] else acc <> [(k, v)]) [] entries
                t <- meNow env
                writeIORef ref (merged, t)
                pure merged
  where
    entry m =
      let mid = maybe "" pyStrOf (get "id" m)
          e =
            PDict
              [ ("context_length", maybe (PInt 128000) id (get "context_length" m))
              , ("max_completion_tokens", maybe (PInt 4096) id (get "top_provider" m >>= get "max_completion_tokens"))
              , ("name", maybe (PStr mid) id (get "name" m))
              , ("pricing", maybe (PDict []) id (get "pricing" m)) ]
          canon = case get "canonical_slug" m of
            Just (PStr c) | not (T.null c), c /= mid -> [(c, e)]
            _ -> []
       in (mid, e) : canon

-- | @get_model_context_length(model, base_url)@
getModelContextLength :: MetaEnv -> Text -> Text -> IO Integer
getModelContextLength env model baseUrl = do
  cachedLen <- if T.null baseUrl then pure Nothing else getCachedContextLength (meGaussHome env) model baseUrl
  case cachedLen of
    Just n -> pure n
    Nothing -> do
      meta <- fetchModelMetadata env
      case lookup model meta of
        Just e -> pure $ case get "context_length" e of
          Just (PInt n) -> n
          Just (PFloat d) -> truncate d
          _ -> 128000
        Nothing -> pure $ case [v | (k, v) <- defaultContextLengths, k `T.isInfixOf` model || model `T.isInfixOf` k] of
          (v : _) -> v
          [] -> head contextProbeTiers

contextCachePath :: FilePath -> FilePath
contextCachePath home = home </> "context_length_cache.yaml"

loadContextCache :: FilePath -> IO [(Text, PyVal)]
loadContextCache home = do
  let p = contextCachePath home
  ok <- doesFileExist p
  if not ok
    then pure []
    else do
      r <- try (BS.readFile p >>= parsePyYaml)
      pure $ case r of
        Right (Right d) -> case get "context_lengths" d of
          Just (PDict kvs) -> kvs
          _ -> []
        Right (Left _) -> []
        Left (_ :: SomeException) -> []

getCachedContextLength :: FilePath -> Text -> Text -> IO (Maybe Integer)
getCachedContextLength home model baseUrl = do
  c <- loadContextCache home
  pure $ case lookup (model <> "@" <> baseUrl) c of
    Just (PInt n) -> Just n
    _ -> Nothing

-- | @save_context_length@: the cache is rewritten with its keys sorted, as
-- @yaml.dump@ writes it.
saveContextLength :: FilePath -> Text -> Text -> Integer -> IO ()
saveContextLength home model baseUrl n = do
  let key = model <> "@" <> baseUrl
  c <- loadContextCache home
  if lookup key c == Just (PInt n)
    then pure ()
    else do
      let c' = if any ((== key) . fst) c then [(k, if k == key then PInt n else v) | (k, v) <- c] else c <> [(key, PInt n)]
          p = contextCachePath home
      _ <- try @SomeException $ do
        createDirectoryIfMissing True (takeDirectory p)
        BS.writeFile p (dumpPyYaml (PDict [("context_lengths", PDict (sortOn fst c'))]))
      pure ()

-- | @get_next_probe_tier@
nextProbeTier :: Integer -> Maybe Integer
nextProbeTier cur = listToMaybe [t | t <- contextProbeTiers, t < cur]

-- | @parse_context_limit_from_error@
parseContextLimitFromError :: Text -> Maybe Integer
parseContextLimitFromError msg = go patterns
  where
    low = T.toLower msg
    go [] = Nothing
    go (p : ps) = case Re.search p low >>= Re.group 1 of
      Just d | let n = read (T.unpack d) :: Integer, n >= 1024 && n <= 10000000 -> Just (read (T.unpack d))
      _ -> go ps
    patterns =
      map (Re.compileOrDie [])
        [ "(?:max(?:imum)?|limit)\\s*(?:context\\s*)?(?:length|size|window)?\\s*(?:is|of|:)?\\s*(\\d{4,})"
        , "context\\s*(?:length|size|window)\\s*(?:is|of|:)?\\s*(\\d{4,})"
        , "(\\d{4,})\\s*(?:token)?\\s*(?:context|limit)"
        , ">\\s*(\\d{4,})\\s*(?:max|limit|token)"
        , "(\\d{4,})\\s*(?:max(?:imum)?)\\b" ]

-- | @estimate_tokens_rough@: a quarter of the length.
estimateTokensRough :: Text -> Integer
estimateTokensRough t = fromIntegral (T.length t) `div` 4

-- | @estimate_messages_tokens_rough@: a quarter of the length of each
-- message's Python repr.
estimateMessagesTokensRough :: [PyVal] -> Integer
estimateMessagesTokensRough ms = sum [fromIntegral (T.length (pyRepr m)) | m <- ms] `div` 4
