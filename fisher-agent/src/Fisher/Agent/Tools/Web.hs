{-# LANGUAGE TemplateHaskell #-}

-- | @tools/web_tools.py@: @web_search@ and @web_extract@ over Firecrawl's
-- v2 API, as firecrawl-py 4.46.0 calls it (requests, three attempts with
-- backoff on 502 and connection errors, its error texts and pydantic
-- models), with long pages summarised by the auxiliary model.
module Fisher.Agent.Tools.Web
  ( WebEnv (..)
  , newWebEnv
  , registerWebTools
  , webSearch
  , webExtract
  , cleanBase64Images
  , toolSchemas
  , pydanticError
  ) where

import Control.Concurrent.Async (forConcurrently)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Text.Read (readMaybe)

import Fisher.Agent.Types
import Fisher.LLM.Errors (errorText)
import Fisher.LLM.Http (HttpResp (..), Timeouts (..), Transport (..))
import Fisher.LLM.Types (HttpReq (..))
import Fisher.Py.JsonError (jsonError)
import Fisher.Py.Value
import Fisher.Tools.Registry (Registry, ToolDef (..), register)

-- | The registry entries upstream's tool modules declare (schema, toolset,
-- requirements, emoji), recorded by tools/corpus/b4.py.
toolSchemas :: PyVal
toolSchemas = either (const (PDict [])) id (loads $(makeRelativeToProject "data/tool-schemas.json" >>= embedFile))

data WebEnv = WebEnv
  { weTransport :: Transport
  , weGetEnv :: Text -> IO (Maybe Text)
  , weSleep :: Double -> IO ()
  , weAux :: AuxLLM
  , weInterrupted :: IO Bool
  , weSummaryModel :: Maybe Text
  -- ^ @AUXILIARY_WEB_EXTRACT_MODEL@, read once as upstream reads it at import
  , weClient :: IORef (Maybe Client)
  -- ^ @_firecrawl_client@: made on first use and kept
  }

newWebEnv :: Transport -> (Text -> IO (Maybe Text)) -> (Double -> IO ()) -> AuxLLM -> IO Bool -> IO WebEnv
newWebEnv tr getEnv sleep aux intr = do
  m <- getEnv "AUXILIARY_WEB_EXTRACT_MODEL"
  WebEnv tr getEnv sleep aux intr (case T.strip <$> m of Just s | not (T.null s) -> Just s; _ -> Nothing) <$> newIORef Nothing

data Client = Client {clKey :: Maybe Text, clUrl :: Text}

origin :: Text
origin = "python-sdk@4.46.0"

-- ------------------------------------------------------------ registration

registerWebTools :: Registry -> WebEnv -> IO ()
registerWebTools reg env = do
  let entry n = fromMaybe (PDict []) (get n toolSchemas)
      def n h =
        ToolDef
          { tdName = n
          , tdToolset = maybe "web" pyStrOf (get "toolset" (entry n))
          , tdSchema = fromMaybe PNull (get "schema" (entry n))
          , tdHandler = \_ args -> h args
          , tdCheck = Just (maybe False (not . T.null) <$> weGetEnv env "FIRECRAWL_API_KEY")
          , tdRequiresEnv = ["FIRECRAWL_API_KEY"]
          , tdEmoji = maybe "" pyStrOf (get "emoji" (entry n)) }
  register reg (def "web_search" (\args -> webSearch env (fromMaybe (PStr "") (get "query" args))))
  register reg $ def "web_extract" $ \args ->
    webExtract env (case get "urls" args of Just (PList us) -> take 5 us; _ -> [])

-- ------------------------------------------------------------ the client

-- | @_get_firecrawl_client()@
client :: WebEnv -> IO (Either Text Client)
client env =
  readIORef (weClient env) >>= \case
    Just c -> pure (Right c)
    Nothing -> do
      key <- weGetEnv env "FIRECRAWL_API_KEY"
      url <- weGetEnv env "FIRECRAWL_API_URL"
      let nonEmpty = \case Just s | not (T.null s) -> Just s; _ -> Nothing
      case (nonEmpty key, nonEmpty url) of
        (Nothing, Nothing) ->
          pure (Left "FIRECRAWL_API_KEY environment variable not set. Set it for cloud Firecrawl, or set FIRECRAWL_API_URL to use a self-hosted instance.")
        (k, u) -> do
          let c = Client k (fromMaybe "https://api.firecrawl.dev" u)
          writeIORef (weClient env) (Just c)
          pure (Right c)

-- | A Python exception's @str()@.
type Exc = Text

-- | @urljoin(base + "/", "/v2/...")@: an absolute path replaces the base's.
endpointUrl :: Text -> Text -> Text
endpointUrl base ep =
  let (scheme, rest) = T.breakOn "://" base
   in if T.null rest then ep else scheme <> "://" <> T.takeWhile (/= '/') (T.drop 3 rest) <> ep

-- | @HttpClient.post@: three attempts; a 502 or a connection error waits
-- 0.5 s, then 1 s. A connection error on the last attempt is raised (its
-- @response@ is None, which the callers' handlers then trip over).
post :: WebEnv -> Client -> Text -> [(Text, PyVal)] -> IO (Either Exc HttpResp)
post env c ep payload = go (0 :: Int)
  where
    body = PDict (payload <> [("origin", PStr origin) | not (any ((== "origin") . fst) payload)])
    headers = ("Content-Type", "application/json") : [("Authorization", "Bearer " <> k) | Just k <- [clKey c], not (T.null k)]
    req = HttpReq "POST" (endpointUrl (clUrl c) ep) headers (Just body) False
    go attempt = do
      r <- tRequest (weTransport env) req (Timeouts 3600 3600)
      case r of
        Right resp
          | rStatus resp == 502 && attempt < 2 -> weSleep env (0.5 * 2 ^ attempt) >> go (attempt + 1)
          | otherwise -> pure (Right resp)
        Left e
          | attempt == 2 -> pure (Left (errorText e))
          | otherwise -> weSleep env (0.5 * 2 ^ attempt) >> go (attempt + 1)

respText :: HttpResp -> Text
respText = TE.decodeUtf8With TEE.lenientDecode . rBody

respJson :: HttpResp -> Either Text PyVal
respJson r = case loads (rBody r) of
  Right v -> Right v
  Left _ -> Left (fromMaybe "Expecting value: line 1 column 1 (char 0)" (jsonError (respText r)))

-- | @handle_response_error(response, action)@: the message it raises.
responseError :: HttpResp -> Text -> Text
responseError r action =
  let parsed = case respJson r of
        Right d@(PDict _) ->
          Right
            ( maybe "No error message provided." pyStrOf (get "error" d)
            , maybe "No additional error details provided." pyStrOf (get "details" d)
            , get "code" d )
        _ ->
          let t = T.take 500 (respText r)
           in Left $
                if not (T.null (T.strip t))
                  then ("Server returned non-JSON response: " <> t, "Full response status: " <> tshow (rStatus r))
                  else ("Server returned empty response with status " <> tshow (rStatus r), "No additional details available")
      (msg, details, code) = case parsed of
        Right x -> x
        Left (m, d) -> (m, d, Nothing)
      tail' = msg <> " - " <> details
   in case rStatus r of
        400 -> "Bad Request: Failed to " <> action <> ". " <> tail'
        401 -> "Unauthorized: Failed to " <> action <> ". " <> tail'
        402 -> "Payment Required: Failed to " <> action <> ". " <> tail'
        403 | code == Just (PStr "THIRD_PARTY_DATA_TERMS_REQUIRED") -> msg
        403 -> "Website Not Supported: Failed to " <> action <> ". " <> tail'
        408 -> "Request Timeout: Failed to " <> action <> " as the request timed out. " <> tail'
        429 -> "Rate Limit Exceeded: Failed to " <> action <> ". " <> tail'
        500 -> "Internal Server Error: Failed to " <> action <> ". " <> tail'
        s -> "Unexpected error during " <> action <> ": Status code " <> tshow s <> ". " <> tail'

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | What the search method's handler raises when the error it handles has
-- no response: a connection error, or a body that is not JSON.
noResponse :: Exc
noResponse = "'NoneType' object has no attribute 'status_code'"

-- ------------------------------------------------------------- pydantic

-- | One field's complaint: location, type, message, input.
data FieldError = FieldError Text Text Text PyVal

-- | @ValidationError.__str__@ for pydantic 2.13.
pydanticError :: Text -> [FieldError] -> Text
pydanticError model errs =
  tshow (length errs) <> (if length errs == 1 then " validation error for " else " validation errors for ") <> model <> "\n"
    <> T.intercalate "\n"
      [ loc <> "\n  " <> msg <> " [type=" <> ty <> ", input_value=" <> pyRepr v <> ", input_type=" <> pyTypeName v <> "]\n    For further information visit https://errors.pydantic.dev/2.13/v/" <> ty
      | FieldError loc ty msg v <- errs ]

pyTypeName :: PyVal -> Text
pyTypeName = \case
  PNull -> "NoneType"
  PBool _ -> "bool"
  PInt _ -> "int"
  PFloat _ -> "float"
  PStr _ -> "str"
  PList _ -> "list"
  PDict _ -> "dict"

-- | A @str@ field (lax mode): a string, else @string_type@; a missing
-- one quotes the whole input.
strField :: PyVal -> Text -> Bool -> Maybe PyVal -> Either FieldError PyVal
strField whole name optional = \case
  Nothing | optional -> Right PNull
  Nothing -> Left (FieldError name "missing" "Field required" whole)
  Just PNull | optional -> Right PNull
  Just v@(PStr _) -> Right v
  Just v -> Left (FieldError name "string_type" "Input should be a valid string" v)

-- | An @Optional[int]@ field (lax mode).
intField :: Text -> Maybe PyVal -> Either FieldError PyVal
intField name = \case
  Nothing -> Right PNull
  Just PNull -> Right PNull
  Just v@(PInt _) -> Right v
  Just (PBool b) -> Right (PInt (if b then 1 else 0))
  Just v@(PFloat d)
    | d == fromIntegral (truncate d :: Integer) -> Right (PInt (truncate d))
    | otherwise -> Left (FieldError name "int_from_float" "Input should be a valid integer, got a number with a fractional part" v)
  Just v@(PStr s) -> case readMaybe (T.unpack (T.strip s)) of
    Just n -> Right (PInt n)
    Nothing -> Left (FieldError name "int_parsing" "Input should be a valid integer, unable to parse string as an integer" v)
  Just v -> Left (FieldError name "int_type" "Input should be a valid integer" v)

-- | @SearchResultWeb(**item).model_dump()@
searchResultWeb :: [(Text, PyVal)] -> Either Text PyVal
searchResultWeb kvs =
  let f k = lookup k kvs
      fields =
        [ ("url", strField (PDict kvs) "url" False (f "url"))
        , ("title", strField (PDict kvs) "title" True (f "title"))
        , ("description", strField (PDict kvs) "description" True (f "description"))
        , ("position", intField "position" (f "position"))
        , ("category", strField (PDict kvs) "category" True (f "category")) ]
      errs = [e | (_, Left e) <- fields]
   in if null errs then Right (PDict [(k, v) | (k, Right v) <- fields]) else Left (pydanticError "SearchResultWeb" errs)

-- | @Document(**normalize_document_input(item)).model_dump()@ for a search
-- result that carries scraped content (only with scrape options, which
-- Gauss never sends): the document's fields, raw.
documentDump :: [(Text, PyVal)] -> PyVal
documentDump kvs =
  let ren k = case k of "rawHtml" -> "raw_html"; "changeTracking" -> "change_tracking"; _ -> k
      kvs' = [(ren k, v) | (k, v) <- kvs]
   in PDict [(k, fromMaybe PNull (lookup k kvs')) | k <- documentFields]

documentFields :: [Text]
documentFields =
  [ "agent_hints", "markdown", "html", "raw_html", "json", "summary", "metadata", "links", "images", "screenshot"
  , "audio", "video", "actions", "answer", "highlights", "warning", "change_tracking", "branding", "product", "menu"
  , "pages", "blocks", "tools" ]

-- ------------------------------------------------------------ web_search

-- | @web_search_tool(query, limit=5)@
webSearch :: WebEnv -> PyVal -> IO Text
webSearch env query = do
  intr <- weInterrupted env
  if intr
    then pure (dumpsAscii (PDict [("error", PStr "Interrupted"), ("success", PBool False)]))
    else do
      r <- search
      pure $ case r of
        Left e -> dumpsUnicode (PDict [("error", PStr ("Error searching web: " <> e))])
        Right web -> dumpsIndent False 2 (PDict [("success", PBool True), ("data", PDict [("web", PList web)])])
  where
    search = do
      ec <- client env
      case ec of
        Left e -> pure (Left e)
        Right c -> case query of
          PStr q
            | T.null (T.strip q) -> pure (Left "Query cannot be empty")
            | otherwise -> do
                resp <- post env c "/v2/search" [("query", PStr q), ("limit", PInt 5)]
                pure $ case resp of
                  Left _ -> Left noResponse
                  Right rr
                    | rStatus rr /= 200 -> Left (responseError rr "search")
                    | otherwise -> case respJson rr of
                        Left _ -> Left noResponse
                        Right body
                          | not (maybe False truthy (get "success" body)) -> Left (responseError rr "search")
                          | otherwise ->
                              let dat = case get "data" body of
                                    Just d@(PDict _) -> d
                                    _ -> PDict []
                               in case get "web" dat of
                                    Nothing -> Right []
                                    Just (PList items) -> mapM item items
                                    Just PNull -> Left "'NoneType' object is not iterable"
                                    Just v -> mapM item (case v of PDict kvs -> map (PStr . fst) kvs; PStr s -> map (PStr . T.singleton) (T.unpack s); _ -> [])
          v -> pure (Left (pydanticError "SearchRequest" [FieldError "query" "string_type" "Input should be a valid string" v]))
    item = \case
      PDict kvs
        | not (null kvs) && any ((`elem` ["markdown", "html", "rawHtml", "links", "screenshot", "changeTracking", "summary", "json"]) . fst) kvs ->
            Right (documentDump kvs)
        | not (null kvs) -> searchResultWeb kvs
      v -> searchResultWeb [("url", v)]

-- ----------------------------------------------------------- web_extract

-- | @web_extract_tool(urls, "markdown")@, LLM processing on.
webExtract :: WebEnv -> [PyVal] -> IO Text
webExtract env urls = do
  results <- mapM one urls
  processed <- forConcurrently results $ \res ->
    case (pgError res, pgContent res) of
      (Nothing, Just raw) | not (T.null raw) -> do
        p <- processContent env raw (pgUrl res) (pgTitle res)
        pure $ case p of
          Just s | not (T.null s) -> res {pgContent = Just s}
          _ -> res
      _ -> pure res
  let trimmed = [PDict [("url", PStr (pgUrl p)), ("title", pgTitle p), ("content", maybe PNull PStr (pgContent p)), ("error", maybe PNull PStr (pgError p))] | p <- processed]
      out =
        if null trimmed
          then dumpsUnicode (PDict [("error", PStr "Content was inaccessible or not found")])
          else dumpsIndent False 2 (PDict [("results", PList trimmed)])
  pure (cleanBase64Images out)
  where
    urlText = \case PStr s -> s; v -> pyStrOf v
    one u = do
      intr <- weInterrupted env
      if intr
        then pure (failed (urlText u) "Interrupted")
        else do
          r <- scrape u
          pure $ case r of
            Left e -> failed (urlText u) e
            Right (md, meta) ->
              let title = case meta of
                    Nothing -> PStr ""
                    Just m -> fromMaybe (PStr "") (get "title" m)
               in Page (urlText u) title md Nothing
    scrape u = do
      ec <- client env
      case ec of
        Left e -> pure (Left e)
        Right c -> case u of
          PNull -> pure (Left "URL cannot be empty")
          PStr s | T.null (T.strip s) -> pure (Left "URL cannot be empty")
          PStr s -> attempt c (T.strip s) (0 :: Int) 0
          v -> pure (Left ("'" <> pyTypeName v <> "' object has no attribute 'strip'"))
    payload s =
      [ ("url", PStr s), ("onlyMainContent", PBool True), ("skipTlsVerification", PBool True), ("removeBase64Images", PBool True)
      , ("fastMode", PBool False), ("blockAds", PBool True), ("storeInCache", PBool True), ("maxAge", PInt 14400000)
      , ("formats", PList [PStr "markdown"]), ("mobile", PBool False) ]
    attempt c s resumes waited = do
      resp <- post env c "/v2/scrape" (payload s)
      case resp of
        -- a connection error reaches the tool's own handler as itself
        Left e -> pure (Left e)
        Right rr
          | rStatus rr >= 400 -> case resumeDelay rr of
              Just d | resumes < 5, waited + d <= 1200 -> weSleep env d >> attempt c s (resumes + 1) (waited + d)
              _ -> pure (Left (responseError rr "scrape"))
          | otherwise -> pure $ case respJson rr of
              Left e -> Left e
              Right body
                | not (maybe False truthy (get "success" body)) -> Left (responseError rr "scrape")
                | otherwise -> case fromMaybe (PDict []) (get "data" body) of
                    PDict d -> Right (case lookup "markdown" d of Just (PStr m) -> Just m; _ -> Nothing, metadataDump <$> lookup "metadata" d)
                    PNull -> Left "'NoneType' object is not iterable"
                    _ -> Left "dictionary update sequence element #0 has length 1; 2 is required"

-- | A page as the extract loop holds it: content None when the scrape
-- had no markdown.
data Page = Page {pgUrl :: Text, pgTitle :: PyVal, pgContent :: Maybe Text, pgError :: Maybe Text}

-- | A failed (or interrupted) page: empty title and content.
failed :: Text -> Text -> Page
failed u e = Page u (PStr "") (Just "") (Just e)

-- | The metadata as @DocumentMetadata(...).model_dump()@ sees it; only its
-- title is read.
metadataDump :: PyVal -> PyVal
metadataDump = \case
  PDict kvs -> PDict [("title", fromMaybe PNull (lookup "title" kvs))]
  _ -> PDict [("title", PNull)]

-- | @processing_continues_delay_s@
resumeDelay :: HttpResp -> Maybe Double
resumeDelay r
  | rStatus r /= 408 = Nothing
  | otherwise = case respJson r of
      Right body@(PDict _)
        | get "code" body == Just (PStr "SCRAPE_TIMEOUT")
        , Just details@(PDict _) <- get "details" body
        , get "state" details == Just (PStr "processing_continues") ->
            let fromBody = case get "retryAfterSeconds" details of
                  Just (PInt n) -> Just (fromIntegral n)
                  Just (PFloat d) | not (isNaN d || isInfinite d) -> Just d
                  _ -> Nothing
                fromHeader = do
                  h <- lookup "retry-after" [(T.toLower k, v) | (k, v) <- rHeaders r]
                  d <- readMaybe (T.unpack (T.strip h)) :: Maybe Double
                  if isNaN d || isInfinite d then Nothing else Just d
                secs = fromMaybe 60 (maybe fromHeader Just fromBody)
             in Just (min 600 (max 5 secs))
      _ -> Nothing

-- ------------------------------------------------------- summarising

-- | @process_content_with_llm(content, url, title, model, 5000)@
processContent :: WebEnv -> Text -> Text -> PyVal -> IO (Maybe Text)
processContent env content url title
  | n > 2000000 =
      pure (Just ("[Content too large to process: " <> fmtF 1 (fromIntegral n / 1000000) <> "MB. Try using web_crawl with specific extraction instructions, or search for a more focused source.]"))
  | n < 5000 = pure Nothing
  | n > 500000 = Just <$> chunked env content ctx
  | otherwise = do
      r <- summarise env content ctx 20000 Nothing
      pure $ case r of
        Left e -> Just ("[Failed to process content: " <> T.take 100 e <> ". Content size: " <> commas (fromIntegral n) <> " chars]")
        Right (Just p) | T.length p > 5000 -> Just (T.take 5000 p <> "\n\n[... summary truncated for context management ...]")
        Right p -> p
  where
    n = T.length content
    titleT = case title of PStr t -> t; _ -> ""
    info = ["Title: " <> titleT | not (T.null titleT)] <> ["Source: " <> url | not (T.null url)]
    ctx = if null info then "" else T.intercalate "\n" info <> "\n\n"

-- | @_call_summarizer_llm@: six attempts, waiting 2, 4, 8, 16 and 32 s;
-- no provider gives Nothing; the last failure is raised (Left).
summarise :: WebEnv -> Text -> Text -> Integer -> Maybe Text -> IO (Either Text (Maybe Text))
summarise env content ctx maxTokens chunkInfo = go (0 :: Int) 2
  where
    (system, user) = case chunkInfo of
      Just ci -> (chunkSystem, "Extract key information from this SECTION of a larger document:\n\n" <> ctx <> ci <> "\n\nSECTION CONTENT:\n" <> content <> "\n\nExtract all important information from this section in a structured format. Focus on facts, data, insights, and key details. Do not add introductions or conclusions.")
      Nothing -> (fullSystem, "Please process this web content and create a comprehensive markdown summary:\n\n" <> ctx <> "CONTENT TO PROCESS:\n" <> content <> "\n\nCreate a markdown summary that captures all key information in a well-organized, scannable format. Include important quotes and code snippets in their original formatting. Focus on actionable information, specific details, and unique insights.")
    go attempt delay = do
      r <- callSummary env system user maxTokens
      case r of
        Right t -> pure (Right (Just t))
        Left Nothing -> pure (Right Nothing)
        Left (Just e)
          | attempt < 5 -> weSleep env delay >> go (attempt + 1) (min (delay * 2) 60)
          | otherwise -> pure (Left e)

-- | One call: the stripped content, or Left Nothing (no provider) / Left the error.
callSummary :: WebEnv -> Text -> Text -> Integer -> IO (Either (Maybe Text) Text)
callSummary env system user maxTokens = do
  r <- weAux env (auxCall "web_extract" [PDict [("role", PStr "system"), ("content", PStr system)], PDict [("role", PStr "user"), ("content", PStr user)]])
    { axTemperature = Just 0.1, axMaxTokens = Just maxTokens, axModel = weSummaryModel env }
  pure $ case r of
    Left (AuxNoProvider _) -> Left Nothing
    Left (AuxFailed e) -> Left (Just e)
    Right resp -> case responseContent resp of
      PStr s -> Right (T.strip s)
      v -> Left (Just ("'" <> pyTypeName v <> "' object has no attribute 'strip'"))

-- | @_process_large_content_chunked@: 100,000-character chunks summarised
-- side by side, then one synthesis.
chunked :: WebEnv -> Text -> Text -> IO Text
chunked env content ctx = do
  let chunks = T.chunksOf 100000 content
      total = length chunks
  rs <- forConcurrently (zip [1 :: Int ..] chunks) $ \(i, ch) -> do
    r <- summarise env ch ctx 10000 (Just ("[Processing chunk " <> tshow i <> " of " <> tshow total <> "]"))
    pure $ case r of
      Right (Just s) | not (T.null s) -> Just ("## Section " <> tshow i <> "\n" <> s)
      _ -> Nothing
  let summaries = [s | Just s <- rs]
  case summaries of
    [] -> pure "[Failed to process large content: all chunk summarizations failed]"
    [s] -> pure (cap s "\n\n[... truncated ...]")
    _ -> do
      let combined = T.intercalate "\n\n---\n\n" summaries
          prompt =
            "You have been given summaries of different sections of a large document. \nSynthesize these into ONE cohesive, comprehensive summary that:\n1. Removes redundancy between sections\n2. Preserves all key facts, figures, and actionable information\n3. Is well-organized with clear structure\n4. Is under 5000 characters\n\n"
              <> ctx <> "SECTION SUMMARIES:\n" <> combined <> "\n\nCreate a single, unified markdown summary."
      r <- callSummary env "You synthesize multiple summaries into one cohesive, comprehensive summary. Be thorough but concise." prompt 20000
      pure $ case r of
        Right s -> cap s "\n\n[... summary truncated for context management ...]"
        Left _ -> cap (T.intercalate "\n\n" summaries) "\n\n[... truncated due to synthesis failure ...]"
  where
    cap s suffix = if T.length s > 5000 then T.take 5000 s <> suffix else s

fullSystem, chunkSystem :: Text
fullSystem =
  "You are an expert content analyst. Your job is to process web content and create a comprehensive yet concise summary that preserves all important information while dramatically reducing bulk.\n\nCreate a well-structured markdown summary that includes:\n1. Key excerpts (quotes, code snippets, important facts) in their original format\n2. Comprehensive summary of all other important information\n3. Proper markdown formatting with headers, bullets, and emphasis\n\nYour goal is to preserve ALL important information while reducing length. Never lose key facts, figures, insights, or actionable information. Make it scannable and well-organized."
chunkSystem =
  "You are an expert content analyst processing a SECTION of a larger document. Your job is to extract and summarize the key information from THIS SECTION ONLY.\n\nImportant guidelines for chunk processing:\n1. Do NOT write introductions or conclusions - this is a partial document\n2. Focus on extracting ALL key facts, figures, data points, and insights from this section\n3. Preserve important quotes, code snippets, and specific details verbatim\n4. Use bullet points and structured formatting for easy synthesis later\n5. Note any references to other sections (e.g., \"as mentioned earlier\", \"see below\") without trying to resolve them\n\nYour output will be combined with summaries of other sections, so focus on thorough extraction rather than narrative flow."

-- --------------------------------------------------------------- base64

-- | @clean_base64_images@: @(data:image/T;base64,B)@ and then bare
-- @data:image/T;base64,B@ become @[BASE64_IMAGE_REMOVED]@.
cleanBase64Images :: Text -> Text
cleanBase64Images = pass False . pass True
  where
    marker = "[BASE64_IMAGE_REMOVED]"
    pass parens t = case T.breakOn (if parens then "(data:image/" else "data:image/") t of
      (pre, rest)
        | T.null rest -> pre
        | otherwise ->
            let afterPrefix = T.drop (if parens then 12 else 11) rest
                (ty, more) = T.break (== ';') afterPrefix
                ok1 = not (T.null ty) && ";base64," `T.isPrefixOf` more
                b64 = T.takeWhile isB64 (T.drop 8 more)
                after = T.drop (T.length b64) (T.drop 8 more)
                closeOk = not parens || ")" `T.isPrefixOf` after
             in if ok1 && not (T.null b64) && closeOk
                  then pre <> marker <> pass parens (if parens then T.drop 1 after else after)
                  else pre <> T.take 1 rest <> pass parens (T.drop 1 rest)
    isB64 c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c `elem` ("+/=" :: String)
