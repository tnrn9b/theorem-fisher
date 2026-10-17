-- | Upstream's MCP client (@tools/mcp_tool.py@): the @mcp_servers@ config,
-- one long-lived task per server (connect, initialize, list tools, wait;
-- reconnect after transport errors with 1-16 s backoff, five times in a
-- server's life), its tools registered as @mcp_<server>_<tool>@ plus four
-- resource and prompt helpers, calls with upstream's result shapes and
-- error strings, sampling, status and shutdown.
module Fisher.Mcp.Client
  ( McpEnv (..)
  , McpClient
  , newMcpClient
  , discover
  , discoverFromConfig
  , mcpStatus
  , shutdownMcp
  , parseBoolish
  , normalizeUri
  , contentRepr
  ) where

import Control.Concurrent
import Control.Concurrent.Async (async, cancel, mapConcurrently, waitCatchSTM)
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, fromException, throwIO, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.Timeout (timeout)

import Fisher.Mcp.Http
import Fisher.Mcp.Sampling
import Fisher.Mcp.Schema (validateSchema)
import Fisher.Mcp.Session
import Fisher.Mcp.Stdio
import Fisher.Py.Re (compileOrDie, matchStart)
import Fisher.Py.Value (PyVal (..), dumpsAscii, get, pyRepr, reprStr, truthy)
import Fisher.Tools.Py (pyTypeName)
import Fisher.Tools.Redact (sanitizeError)
import Fisher.Tools.Registry

data McpEnv = McpEnv
  { meRegistry :: Registry
  , meLog :: Text -> Text -> IO ()
  -- ^ level (DEBUG, INFO, WARNING, ERROR) and message, as upstream logs them
  , meSampling :: SamplingLLM
  , meEnviron :: IO [(Text, Text)]
  , meHome :: Text
  , meGaussHome :: FilePath
  , meHttp :: Maybe HttpClient
  }

data Server = Server
  { svName :: Text
  , svConfig :: PyVal
  , svHttp :: Bool
  , svSession :: TVar (Maybe Session)
  , svTools :: IORef [PyVal]
  , svSchemas :: IORef (M.Map Text PyVal)
  , svRegistered :: IORef [Text]
  , svSampling :: Maybe SamplingHandler
  , svShutdown :: TMVar ()
  , svDone :: MVar ()
  }

data McpClient = McpClient
  { mcEnv :: McpEnv
  , mcServers :: MVar [(Text, Server)]
  }

newMcpClient :: McpEnv -> IO McpClient
newMcpClient env = McpClient env <$> newMVar []

-- ------------------------------------------------------------------ config

-- | @_parse_boolish(value, default)@: the value, and the warning it logs.
parseBoolish :: PyVal -> Bool -> (Bool, Maybe Text)
parseBoolish v def = case v of
  PNull -> (def, Nothing)
  PBool b -> (b, Nothing)
  PStr s
    | T.toLower (T.strip s) `elem` ["true", "1", "yes", "on"] -> (True, Nothing)
    | T.toLower (T.strip s) `elem` ["false", "0", "no", "off"] -> (False, Nothing)
  _ -> (def, Just ("MCP config expected a boolean-ish value, got " <> pyRepr v <> "; using default=" <> (if def then "True" else "False")))

boolish :: McpEnv -> PyVal -> Bool -> IO Bool
boolish env v def = do
  let (b, w) = parseBoolish v def
  forM_ w (meLog env "WARNING")
  pure b

nameSet :: McpEnv -> Text -> PyVal -> IO [Text]
nameSet env label = \case
  PNull -> pure []
  PStr s -> pure [s]
  PList xs -> pure (map strOf xs)
  v -> do
    meLog env "WARNING" ("MCP config " <> label <> " must be a string or list of strings; ignoring " <> pyRepr v)
    pure []
  where
    strOf = \case
      PStr s -> s
      x -> pyRepr x

cfgGet :: Text -> PyVal -> PyVal
cfgGet k c = fromMaybe PNull (get k c)

expandHome :: Text -> Text -> Text
expandHome home t
  | t == "~" = home
  | "~/" `T.isPrefixOf` t = home <> T.drop 1 t
  | otherwise = t

safe :: Text -> Text
safe = T.replace "." "_" . T.replace "-" "_"

-- ---------------------------------------------------------------- connect

connectFailure :: Text -> McpFailure -> Text
connectFailure _ (McpFailure "FileNotFoundError" cmd) =
  let base = snd (T.breakOnEnd "/" cmd)
      hint =
        if base `elem` ["npx", "npm", "node"]
          then " (ensure Node.js is installed and PATH includes its bin directory, or set mcp_servers.<name>.command to an absolute path and include that directory in mcp_servers.<name>.env.PATH)"
          else ""
   in sanitizeError ("missing executable '" <> cmd <> "'" <> hint)
connectFailure _ (McpFailure cls msg) = sanitizeError (if T.null msg then cls else msg)

-- | One connection attempt: the session, ready for calls.
connectOnce :: McpEnv -> Server -> IO Session
connectOnce env sv = do
  let cfg = svConfig sv
      name = svName sv
  transport <-
    if svHttp sv
      then case meHttp env of
        Nothing -> throwIO (McpFailure "ImportError" ("MCP server '" <> name <> "' requires HTTP transport but mcp.client.streamable_http is not available. Upgrade the mcp package to get HTTP support."))
        Just hc -> do
          let headers = case cfgGet "headers" cfg of
                PDict kvs -> [(k, case v of PStr s -> s; x -> pyRepr x) | (k, v) <- kvs]
                _ -> []
              url = case cfgGet "url" cfg of PStr u -> u; v -> pyRepr v
          connectHttp hc (HttpOpts url headers)
      else do
        command <- case cfgGet "command" cfg of
          PStr c | not (T.null c) -> pure c
          PNull -> throwIO (McpFailure "ValueError" ("MCP server '" <> name <> "' has no 'command' in config"))
          PStr _ -> throwIO (McpFailure "ValueError" ("MCP server '" <> name <> "' has no 'command' in config"))
          v -> pure (pyRepr v)
        args <- case cfgGet "args" cfg of
          PNull -> pure []
          PList xs | all isStr xs -> pure [s | PStr s <- xs]
          v -> throwIO (McpFailure "ValidationError" (pydanticOne "StdioServerParameters" "args" "Input should be a valid list" "list_type" v))
        userEnv <- case cfgGet "env" cfg of
          PNull -> pure []
          PDict kvs -> forM kvs $ \(k, v) -> case v of
            PStr s -> pure (k, s)
            other -> throwIO (McpFailure "ValidationError" (pydanticOne "StdioServerParameters" ("env." <> k) "Input should be a valid string" "string_type" other))
          v -> throwIO (McpFailure "ValidationError" (pydanticOne "StdioServerParameters" "env" "Input should be a valid dictionary" "dict_type" v))
        environ <- meEnviron env
        let safeE = safeEnv environ userEnv
        (cmd, safeE') <- resolveCommand (meHome env) (meGaussHome env) command safeE
        let finalEnv = if null safeE' then sdkDefaultEnv environ else foldl put (sdkDefaultEnv environ) safeE'
        -- Fisher only: the server's working directory (a prover server's
        -- project root), with ~ expanded
        let dir = case cfgGet "cwd" cfg of
              PStr d | not (T.null (T.strip d)) -> Just (T.unpack (expandHome (meHome env) (T.strip d)))
              _ -> Nothing
        forM_ dir $ \d -> do
          ok <- doesDirectoryExist d
          unless ok (throwIO (McpFailure "OSError" ("working directory does not exist: " <> T.pack d)))
        r <- spawnStdio (StdioSpawn cmd args finalEnv dir)
        either throwIO pure r
  let handler method params = case method of
        "ping" -> pure (Right (PDict []))
        "roots/list" -> pure (Left (-32600, "List roots not supported", Nothing))
        "elicitation/create" -> pure (Left (-32600, "Elicitation not supported", Nothing))
        "sampling/createMessage" -> case svSampling sv of
          Nothing -> pure (Left (-32600, "Sampling not supported", Nothing))
          Just h -> handleCreateMessage h params
        _ -> pure (Left (-32602, "Invalid request parameters", Just (PStr "")))
      caps = if isJust (svSampling sv) then PDict [("sampling", PDict [("tools", PDict [])])] else PDict []
  session <- newSession transport handler
  let orFatal act = do
        a <- async act
        r <- atomically ((Left <$> sessionFatal session) `orElse` (Right <$> waitSTMish a))
        case r of
          Left msg -> cancel a >> throwIO (McpFailure "ExceptionGroup" msg)
          Right (Left e) -> throwIO e
          Right (Right v) -> pure v
  initResult <-
    orFatal . request session "initialize" . Just $
      PDict [("protocolVersion", PStr "2025-11-25"), ("capabilities", caps), ("clientInfo", PDict [("name", PStr "mcp"), ("version", PStr "0.1.0")])]
  case get "protocolVersion" initResult of
    Just (PStr v) | v `elem` ["2024-11-05", "2025-03-26", "2025-06-18", "2025-11-25"] -> pure ()
    v -> throwIO (McpFailure "RuntimeError" ("Unsupported protocol version from the server: " <> maybe "None" (\case PStr s -> s; x -> pyRepr x) v))
  notify session "notifications/initialized" Nothing
  listed <- orFatal (request session "tools/list" Nothing)
  tools <- either (throwIO . McpFailure "ValidationError") pure (validateTools listed)
  writeIORef (svTools sv) tools
  writeIORef (svSchemas sv) (M.fromList [(n, fromMaybe PNull (get "outputSchema" t)) | t <- tools, Just (PStr n) <- [get "name" t]])
  pure session
  where
    isStr = \case
      PStr _ -> True
      _ -> False
    put acc (k, v) = if any ((== k) . fst) acc then [(k', if k' == k then v else v') | (k', v') <- acc] else acc <> [(k, v)]
    waitSTMish a = waitCatchSTM a

-- | pydantic's text for one invalid field.
pydanticOne :: Text -> Text -> Text -> Text -> PyVal -> Text
pydanticOne model loc msg ty v =
  "1 validation error for " <> model <> "\n" <> loc <> "\n  " <> msg <> " [type=" <> ty <> ", input_value=" <> pyRepr v <> ", input_type=" <> pyTypeName v <> "]\n    For further information visit https://errors.pydantic.dev/2.13/v/" <> ty

validateTools :: PyVal -> Either Text [PyVal]
validateTools res = case get "tools" res of
  Just (PList ts) ->
    let missing =
          [ (i, f, t)
          | (i, t) <- zip [0 :: Int ..] ts
          , f <- ["name", "inputSchema"]
          , case get f t of
              Nothing -> True
              Just PNull -> True
              _ -> False
          ]
     in case missing of
          [] -> Right ts
          _ ->
            Left
              ( T.pack (show (length missing)) <> " validation error" <> (if length missing == 1 then "" else "s") <> " for ListToolsResult"
                  <> T.concat
                    [ "\ntools." <> T.pack (show i) <> "." <> f <> "\n  Field required [type=missing, input_value=" <> pyRepr t <> ", input_type=dict]\n    For further information visit https://errors.pydantic.dev/2.13/v/missing"
                    | (i, f, t) <- missing
                    ]
              )
  _ -> Left (pydanticOne "ListToolsResult" "tools" "Field required" "missing" res)

-- | @MCPServerTask.run@: the result of the first attempt goes to @ready@;
-- later transport errors reconnect.
runServer :: McpEnv -> Server -> MVar (Either McpFailure ()) -> IO ()
runServer env sv ready = loop (0 :: Int) (1 :: Double) `finallyDo` putMVar (svDone sv) ()
  where
    name = svName sv
    reconnectOnExit = cfgGet "reconnect_on_exit" (svConfig sv) == PBool True
    finallyDo a b = do
      r <- try a
      _ <- b
      either (\(e :: SomeException) -> throwIO e) pure r
    loop retries backoff = do
      r <- try (connectOnce env sv)
      case r of
        Left (e :: SomeException) -> failed retries backoff (asFailure e)
        Right session -> do
          atomically (writeTVar (svSession sv) (Just session))
          _ <- tryPutMVar ready (Right ())
          -- Fisher only: with reconnect_on_exit, a server that exits (its
          -- output closes) is reconnected as a transport error is; upstream
          -- leaves it closed
          let exited = if reconnectOnExit then "Connection closed" <$ sessionClosedSTM session else retry
          ev <- atomically ((Left <$> readTMVar (svShutdown sv)) `orElse` (Right <$> sessionFatal session) `orElse` (Right <$> exited))
          case ev of
            Left () -> do
              _ <- try (closeSession session) :: IO (Either SomeException ())
              atomically (writeTVar (svSession sv) Nothing)
            Right msg -> do
              _ <- try (tClose' session) :: IO (Either SomeException ())
              failed retries backoff (McpFailure "ExceptionGroup" msg)
    tClose' = closeSession
    failed retries backoff e = do
      atomically (writeTVar (svSession sv) Nothing)
      first <- tryPutMVar ready (Left e)
      down <- atomically (isJust <$> tryReadTMVar (svShutdown sv))
      unless (first || down) $ do
        let retries' = retries + 1
            msg = case e of McpFailure _ m -> m
        if retries' > 5
          then meLog env "WARNING" ("MCP server '" <> name <> "' failed after 5 reconnection attempts, giving up: " <> msg)
          else do
            meLog env "WARNING" ("MCP server '" <> name <> "' connection lost (attempt " <> tshow retries' <> "/5), reconnecting in " <> tshow (round backoff :: Integer) <> "s: " <> msg)
            _ <- timeout (round (backoff * 1e6)) (atomically (readTMVar (svShutdown sv)))
            stop <- atomically (isJust <$> tryReadTMVar (svShutdown sv))
            unless stop (loop retries' (min (backoff * 2) 60))

asFailure :: SomeException -> McpFailure
asFailure e = case fromException e of
  Just f -> f
  Nothing -> McpFailure "Exception" (T.pack (displayException e))


-- ---------------------------------------------------------------- calls

-- | Run a call under the server's tool timeout, with upstream's failure
-- strings (the logged line is not sanitized, the returned one is).
withSession :: McpEnv -> Server -> Text -> (Session -> IO Text) -> IO Text
withSession env sv logPrefix act = do
  -- Fisher only: with fisher_messages, a timeout names its seconds
  let fisherMessages = cfgGet "fisher_messages" (svConfig sv) == PBool True
      timeoutText = \case
        Just us ->
          let secs = fromIntegral us / 1e6 :: Double
              shown = if secs == fromIntegral (round secs :: Integer) then tshow (round secs :: Integer) else tshow secs
           in "no reply within " <> shown <> (if secs == 1 then " second" else " seconds")
        Nothing -> ""
  ms <- readTVarIO (svSession sv)
  case ms of
    Nothing -> pure (dumpsAscii (PDict [("error", PStr ("MCP server '" <> svName sv <> "' is not connected"))]))
    Just s -> do
      r <- case cfgGet "timeout" (svConfig sv) of
        PStr _ -> pure (Left (McpFailure "TypeError" "'>' not supported between instances of 'str' and 'int'"))
        tmo -> do
          let micros = case tmo of
                PNull -> Just (120 * 1000000)
                PInt n -> Just (fromIntegral n * 1000000)
                PFloat d -> Just (round (d * 1e6))
                PBool b -> Just (if b then 1000000 else 0)
                _ -> Nothing
          x <- try (maybe (Just <$> act s) (`timeout` act s) micros)
          pure $ case x of
            Left e -> Left (asFailure e)
            Right Nothing -> Left (McpFailure "TimeoutError" (if fisherMessages then timeoutText micros else ""))
            Right (Just t) -> Right t
      case r of
        Right t -> pure t
        Left f@(McpFailure _ msg) -> do
          meLog env "ERROR" (logPrefix <> msg)
          pure (dumpsAscii (PDict [("error", PStr (sanitizeError ("MCP call failed: " <> failureText f)))]))

textBlocks :: PyVal -> [Text]
textBlocks res = [t | b <- listOf (cfgGet "content" res), get "type" b == Just (PStr "text"), Just (PStr t) <- [get "text" b]]

listOf :: PyVal -> [PyVal]
listOf = \case
  PList xs -> xs
  _ -> []

-- | A server tool's handler.
toolHandler :: McpEnv -> Server -> Text -> PyVal -> IO Text
toolHandler env sv raw args =
  withSession env sv ("MCP tool " <> svName sv <> "/" <> raw <> " call failed: ") $ \s -> do
    res <- request s "tools/call" (Just (PDict [("name", PStr raw), ("arguments", args)]))
    if truthy (cfgGet "isError" res)
      then
        let txt = T.concat (textBlocks res)
         in pure (dumpsAscii (PDict [("error", PStr (sanitizeError (if T.null txt then "MCP tool returned an error" else txt)))]))
      else do
        known <- M.member raw <$> readIORef (svSchemas sv)
        unless known $ do
          listed <- request s "tools/list" Nothing
          forM_ (either (const []) id (validateTools listed)) $ \t -> case get "name" t of
            Just (PStr n) -> modifyIORef' (svSchemas sv) (M.insert n (fromMaybe PNull (get "outputSchema" t)))
            _ -> pure ()
        schema <- M.lookup raw <$> readIORef (svSchemas sv)
        case schema of
          Just sch | sch /= PNull -> case get "structuredContent" res of
            Nothing -> throwIO (McpFailure "RuntimeError" ("Tool " <> raw <> " has an output schema but did not return structured content"))
            Just PNull -> throwIO (McpFailure "RuntimeError" ("Tool " <> raw <> " has an output schema but did not return structured content"))
            Just sc -> forM_ (validateSchema sch sc) $ \e -> throwIO (McpFailure "RuntimeError" ("Invalid structured content returned by tool " <> raw <> ": " <> e))
          _ -> pure ()
        pure (dumpsAscii (PDict [("result", PStr (T.intercalate "\n" (textBlocks res)))]))

-- | pydantic's @AnyUrl@ spelling: an http(s) URL with no path gains "/".
normalizeUri :: Text -> Text
normalizeUri u = case T.breakOn "://" u of
  (scheme, rest)
    | T.toLower scheme `elem` ["http", "https", "ws", "wss"]
    , not (T.null rest) ->
        let after = T.drop 3 rest
         in if T.any (`elem` ("/?#" :: String)) after then u else u <> "/"
  _ -> u

uriOk :: Text -> Bool
uriOk = isJust . matchStart (compileOrDie [] "^[A-Za-z][A-Za-z0-9+.-]*:")

-- | Python's @str()@ of a non-text content block, as pydantic prints it.
contentRepr :: PyVal -> Text
contentRepr c =
  let f k = fromMaybe PNull (get k c)
      r = pyField
      common = ["annotations=" <> annotationsRepr (f "annotations"), "meta=" <> r (f "_meta")]
   in T.unwords $ case get "type" c of
        Just (PStr "resource") -> ["type='resource'", "resource=" <> resourceRepr (f "resource")] <> common
        Just (PStr "resource_link") ->
          ["name=" <> r (f "name"), "title=" <> r (f "title"), "uri=" <> anyUrl (f "uri"), "description=" <> r (f "description"), "mimeType=" <> r (f "mimeType"), "size=" <> r (f "size"), "icons=" <> r (f "icons")] <> common <> ["type='resource_link'"]
        Just (PStr t) -> ["type=" <> reprStr t, "data=" <> r (f "data"), "mimeType=" <> r (f "mimeType")] <> common
        _ -> [pyRepr c]
  where
    resourceRepr res =
      let f k = fromMaybe PNull (get k res)
       in case get "text" res of
            Just t -> "TextResourceContents(uri=" <> anyUrl (f "uri") <> ", mimeType=" <> pyField (f "mimeType") <> ", meta=" <> pyField (f "_meta") <> ", text=" <> pyField t <> ")"
            Nothing -> "BlobResourceContents(uri=" <> anyUrl (f "uri") <> ", mimeType=" <> pyField (f "mimeType") <> ", meta=" <> pyField (f "_meta") <> ", blob=" <> pyField (f "blob") <> ")"
    anyUrl = \case
      PStr u -> "AnyUrl(" <> reprStr (normalizeUri u) <> ")"
      v -> pyField v
    annotationsRepr = \case
      PNull -> "None"
      a -> "Annotations(audience=" <> pyField (fromMaybe PNull (get "audience" a)) <> ", priority=" <> pyField (fromMaybe PNull (get "priority" a)) <> ", lastModified=" <> pyField (fromMaybe PNull (get "lastModified" a)) <> ")"

pyField :: PyVal -> Text
pyField = pyRepr

utilityHandler :: McpEnv -> Server -> Text -> PyVal -> IO Text
utilityHandler env sv which args =
  let name = svName sv
      run = withSession env sv ("MCP " <> name <> "/" <> which <> " failed: ")
   in case which of
        "list_resources" -> run $ \s -> do
          res <- request s "resources/list" Nothing
          let one r =
                PDict $
                  [("uri", PStr (normalizeUri (strOr (cfgGet "uri" r)))), ("name", cfgGet "name" r)]
                    <> [("description", d) | Just d <- [get "description" r], truthy d]
                    <> [("mimeType", m) | Just m <- [get "mimeType" r], truthy m]
          pure (dumpsAscii (PDict [("resources", PList (map one (listOf (cfgGet "resources" res))))]))
        "read_resource" -> case get "uri" args of
          Just u | truthy u -> run $ \s -> do
            let uri = strOr u
            unless (uriOk uri) $
              throwIO (McpFailure "ValidationError" (pydanticOne "ReadResourceRequestParams" "uri" "Input should be a valid URL, relative URL without a base" "url_parsing" u))
            res <- request s "resources/read" (Just (PDict [("uri", PStr (normalizeUri uri))]))
            let part c = case (get "text" c, get "blob" c) of
                  (Just (PStr t), _) -> Just t
                  (_, Just (PStr b)) -> Just ("[binary data, " <> tshow (T.length b) <> " bytes]")
                  _ -> Nothing
            pure (dumpsAscii (PDict [("result", PStr (T.intercalate "\n" [p | c <- listOf (cfgGet "contents" res), Just p <- [part c]]))]))
          _ -> notConnectedOr (dumpsAscii (PDict [("error", PStr "Missing required parameter 'uri'")]))
        "list_prompts" -> run $ \s -> do
          res <- request s "prompts/list" Nothing
          let arg a = PDict ([("name", cfgGet "name" a)] <> [("description", d) | Just d <- [get "description" a], truthy d] <> [("required", cfgGet "required" a)])
              one p =
                PDict $
                  [("name", cfgGet "name" p)]
                    <> [("description", d) | Just d <- [get "description" p], truthy d]
                    <> [("arguments", PList (map arg as)) | let as = listOf (cfgGet "arguments" p), not (null as)]
          pure (dumpsAscii (PDict [("prompts", PList (map one (listOf (cfgGet "prompts" res))))]))
        "get_prompt" -> case get "name" args of
          Just n | truthy n -> run $ \s -> do
            let arguments = fromMaybe (PDict []) (get "arguments" args)
            case arguments of
              PNull -> pure ()
              PDict kvs -> case [(k, v) | (k, v) <- kvs, not (isStrV v)] of
                [] -> pure ()
                bad ->
                  throwIO . McpFailure "ValidationError" $
                    T.pack (show (length bad)) <> " validation error" <> (if length bad == 1 then "" else "s") <> " for GetPromptRequestParams"
                      <> T.concat ["\narguments." <> k <> "\n  Input should be a valid string [type=string_type, input_value=" <> pyRepr v <> ", input_type=" <> pyTypeName v <> "]\n    For further information visit https://errors.pydantic.dev/2.13/v/string_type" | (k, v) <- bad]
              v -> throwIO (McpFailure "ValidationError" (pydanticOne "GetPromptRequestParams" "arguments" "Input should be a valid dictionary" "dict_type" v))
            res <- request s "prompts/get" (Just (PDict ([("name", n)] <> [("arguments", arguments) | arguments /= PNull])))
            let msg m =
                  let c = cfgGet "content" m
                      content = case (get "type" c, get "text" c) of
                        (Just (PStr "text"), Just t) -> t
                        _ -> PStr (contentRepr c)
                   in PDict [("role", cfgGet "role" m), ("content", content)]
            pure (dumpsAscii (PDict ([("messages", PList (map msg (listOf (cfgGet "messages" res))))] <> [("description", d) | Just d <- [get "description" res], truthy d])))
          _ -> notConnectedOr (dumpsAscii (PDict [("error", PStr "Missing required parameter 'name'")]))
        _ -> pure (dumpsAscii (PDict [("error", PStr ("Unknown tool: " <> which))]))
  where
    strOr = \case
      PStr t -> t
      v -> pyRepr v
    isStrV = \case
      PStr _ -> True
      _ -> False
    notConnectedOr missing = do
      ms <- readTVarIO (svSession sv)
      pure $ case ms of
        Nothing -> dumpsAscii (PDict [("error", PStr ("MCP server '" <> svName sv <> "' is not connected"))])
        Just _ -> missing

-- ---------------------------------------------------------- registration

utilitySchemas :: Text -> [(Text, Text, PyVal, Bool)]
utilitySchemas name =
  [ ("list_resources", "List available resources from MCP server '" <> name <> "'", emptyObj, True)
  , ("read_resource", "Read a resource by URI from MCP server '" <> name <> "'", PDict [("type", PStr "object"), ("properties", PDict [("uri", PDict [("type", PStr "string"), ("description", PStr "URI of the resource to read")])]), ("required", PList [PStr "uri"])], True)
  , ("list_prompts", "List available prompts from MCP server '" <> name <> "'", emptyObj, False)
  , ( "get_prompt"
    , "Get a prompt by name from MCP server '" <> name <> "'"
    , PDict
        [ ("type", PStr "object")
        , ("properties", PDict [("name", PDict [("type", PStr "string"), ("description", PStr "Name of the prompt to retrieve")]), ("arguments", PDict [("type", PStr "object"), ("description", PStr "Optional arguments to pass to the prompt")])])
        , ("required", PList [PStr "name"])
        ]
    , False
    )
  ]
  where
    emptyObj = PDict [("type", PStr "object"), ("properties", PDict [])]

registerServer :: McpClient -> Server -> IO [Text]
registerServer mc sv = do
  let env = mcEnv mc
      reg = meRegistry env
      name = svName sv
      filt = case cfgGet "tools" (svConfig sv) of
        PNull -> PDict []
        v -> v
      isUp = do
        servers <- readMVar (mcServers mc)
        case lookup name servers of
          Just s -> isJust <$> readTVarIO (svSession s)
          Nothing -> pure False
      def n desc params h = register reg (ToolDef n ("mcp-" <> name) (PDict [("name", PStr n), ("description", PStr desc), ("parameters", params)]) (\_ a -> h a) (Just isUp) [] "")
  include <- nameSet env ("mcp_servers." <> name <> ".tools.include") (cfgGet "include" filt)
  exclude <- nameSet env ("mcp_servers." <> name <> ".tools.exclude") (cfgGet "exclude" filt)
  tools <- readIORef (svTools sv)
  names1 <- fmap concat . forM tools $ \t -> do
    let raw = case get "name" t of Just (PStr n) -> n; _ -> ""
        keep
          | not (null include) = raw `elem` include
          | not (null exclude) = raw `notElem` exclude
          | otherwise = True
    if not keep
      then meLog env "DEBUG" ("MCP server '" <> name <> "': skipping tool '" <> raw <> "' (filtered by config)") >> pure []
      else do
        let n = "mcp_" <> safe name <> "_" <> safe raw
            desc = case get "description" t of
              Just (PStr d) | not (T.null d) -> d
              _ -> "MCP tool " <> raw <> " from " <> name
            params = case get "inputSchema" t of
              Just s | truthy s -> s
              _ -> PDict [("type", PStr "object"), ("properties", PDict [])]
        def n desc params (toolHandler env sv raw)
        pure [n]
  resOn <- boolish env (cfgGet "resources" filt) True
  promptsOn <- boolish env (cfgGet "prompts" filt) True
  names2 <- fmap concat . forM (utilitySchemas name) $ \(which, desc, params, isRes) ->
    if (isRes && not resOn) || (not isRes && not promptsOn)
      then meLog env "DEBUG" ("MCP server '" <> name <> "': skipping utility '" <> which <> "' (" <> (if isRes then "resources" else "prompts") <> " disabled)") >> pure []
      else do
        let n = "mcp_" <> safe name <> "_" <> which
        def n desc params (utilityHandler env sv which)
        pure [n]
  let names = names1 <> names2
  unless (null names) (createCustomToolset reg ("mcp-" <> name) (Toolset ("MCP tools from " <> name <> " server") names []))
  writeIORef (svRegistered sv) names
  pure names

-- ------------------------------------------------------------- discovery

numSeconds :: PyVal -> Double -> Double
numSeconds v d = case v of
  PInt n -> fromIntegral n
  PFloat x -> x
  _ -> d

-- | @discover_mcp_tools()@ for an @mcp_servers@ value: connects the new
-- enabled servers in parallel and returns every registered MCP tool name.
discover :: McpClient -> PyVal -> IO [Text]
discover mc servers = do
  let env = mcEnv mc
  existing <- readMVar (mcServers mc)
  case servers of
    PDict entries | all (isDict . snd) entries -> do
      fresh <- fmap concat . forM entries $ \(k, v) ->
        if k `elem` map fst existing
          then pure []
          else do
            on <- boolish env (cfgGet "enabled" v) True
            pure [(k, v) | on]
      unless (null fresh) $ do
        results <- mapConcurrently (connectNew mc) fresh
        let failed = [(k, v, e) | ((k, v), Left e) <- zip fresh results]
            okNames = concat [ns | Right ns <- results]
        forM_ failed $ \(k, v, e) ->
          meLog env "WARNING" ("Failed to connect to MCP server '" <> k <> "'" <> (case cfgGet "command" v of PStr c | not (T.null c) -> " (command=" <> c <> ")"; _ -> "") <> ": " <> e)
        when (not (null okNames) || not (null failed)) $
          meLog env "INFO" ("  MCP: " <> tshow (length okNames) <> " tool(s) from " <> tshow (length fresh - length failed) <> " server(s)" <> (if null failed then "" else " (" <> tshow (length failed) <> " failed)"))
        unless (null okNames) (appendToToolsets (meRegistry env) "gauss-" okNames)
      allNames
    PDict _ -> do
      meLog env "DEBUG" "MCP tool discovery failed: an mcp_servers entry is not a mapping"
      allNames
    _ -> do
      meLog env "DEBUG" "No MCP servers configured"
      allNames
  where
    isDict = \case
      PDict _ -> True
      _ -> False
    allNames = do
      ss <- readMVar (mcServers mc)
      concat <$> mapM (readIORef . svRegistered . snd) ss

connectNew :: McpClient -> (Text, PyVal) -> IO (Either Text [Text])
connectNew mc (name, cfg) = do
  let env = mcEnv mc
      http = isJust (get "url" cfg)
  when (http && isJust (get "command" cfg)) $
    meLog env "WARNING" ("MCP server '" <> name <> "' has both 'url' and 'command' in config. Using HTTP transport ('url'). Remove 'command' to silence this warning.")
  samplingCfg <- case get "sampling" cfg of
    Nothing -> pure (Just (PDict []))
    Just PNull -> pure Nothing
    Just v -> pure (Just v)
  case samplingCfg of
    Nothing -> do
      -- upstream's run() fails before it can report, so the wait times out
      threadDelay (round (numSeconds (cfgGet "connect_timeout" cfg) 60 * 1e6))
      pure (Left "CancelledError")
    Just scfg -> do
      handler <-
        if truthy (fromMaybe (PBool True) (get "enabled" scfg))
          then Just <$> newSamplingHandler name scfg (meLog env) (meSampling env)
          else pure Nothing
      sv <- Server name cfg http <$> newTVarIO Nothing <*> newIORef [] <*> newIORef M.empty <*> newIORef [] <*> pure handler <*> newEmptyTMVarIO <*> newEmptyMVar
      ready <- newEmptyMVar
      task <- async (runServer env sv ready)
      r <- timeout (round (numSeconds (cfgGet "connect_timeout" cfg) 60 * 1e6)) (readMVar ready)
      case r of
        Nothing -> do
          atomically (void (tryPutTMVar (svShutdown sv) ()))
          cancel task
          pure (Left "CancelledError")
        Just (Left e) -> do
          _ <- waitCatchSTM' task
          pure (Left (connectFailure name e))
        Just (Right ()) -> do
          modifyMVar_ (mcServers mc) (\ss -> pure (ss <> [(name, sv)]))
          names <- registerServer mc sv
          meLog env "INFO" ("MCP server '" <> name <> "' (" <> (if http then "HTTP" else "stdio") <> "): registered " <> tshow (length names) <> " tool(s): " <> T.intercalate ", " names)
          pure (Right names)
  where
    waitCatchSTM' t = atomically (waitCatchSTM t)

-- | @discover_mcp_tools()@ reading @mcp_servers@ from the loaded config.
discoverFromConfig :: McpClient -> PyVal -> IO [Text]
discoverFromConfig mc config = case get "mcp_servers" config of
  Just v | truthy v -> discover mc v
  _ -> discover mc PNull

-- | @get_mcp_status()@ for the configured servers.
mcpStatus :: McpClient -> PyVal -> IO PyVal
mcpStatus mc config = do
  ss <- readMVar (mcServers mc)
  let servers = case get "mcp_servers" config of
        Just (PDict kvs) -> kvs
        _ -> []
  fmap PList . forM servers $ \(name, cfg) -> do
    let transport = if isJust (get "url" cfg) then "http" else "stdio"
    case lookup name ss of
      Just sv -> do
        sess <- readTVarIO (svSession sv)
        if isJust sess
          then do
            n <- length <$> readIORef (svRegistered sv)
            samp <- maybe (pure []) (fmap (\m -> [("sampling", m)]) . samplingMetrics) (svSampling sv)
            pure (PDict ([("name", PStr name), ("transport", PStr transport), ("tools", PInt (fromIntegral n)), ("connected", PBool True)] <> samp))
          else pure (off name transport)
      Nothing -> pure (off name transport)
  where
    off name transport = PDict [("name", PStr name), ("transport", PStr transport), ("tools", PInt 0), ("connected", PBool False)]

-- | @shutdown_mcp_servers()@: every server told to stop, each given 10 s.
shutdownMcp :: McpClient -> IO ()
shutdownMcp mc = do
  ss <- readMVar (mcServers mc)
  _ <- forConcurrently' ss $ \(name, sv) -> do
    atomically (void (tryPutTMVar (svShutdown sv) ()))
    done <- timeout 10000000 (readMVar (svDone sv))
    when (done == Nothing) (meLog (mcEnv mc) "WARNING" ("MCP server '" <> name <> "' shutdown timed out, cancelling task"))
    atomically (writeTVar (svSession sv) Nothing)
  modifyMVar_ (mcServers mc) (const (pure []))
  where
    forConcurrently' xs f = mapConcurrently f xs

tshow :: Show a => a -> Text
tshow = T.pack . show

