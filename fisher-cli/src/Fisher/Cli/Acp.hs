-- | @fisher acp@: Fisher's native agent behind the Agent Client Protocol
-- (upstream @gauss acp@, acp_adapter/ on agent-client-protocol 0.8.1),
-- JSON-RPC over stdio for editors (Zed, VS Code, JetBrains).
--
-- Each ACP session holds an agent (platform @acp@, toolset @fisher-acp@,
-- quiet) and the conversation so far. A prompt runs one turn and streams
-- what happens as @session/update@ notifications: the spinner's thinking
-- lines as @agent_thought_chunk@, each tool as a @tool_call@ when it
-- starts and a @tool_call_update@ when the next model call begins, then
-- the final answer as @agent_message_chunk@. @session/cancel@ interrupts
-- the turn. Prompts starting with a known slash command (/help /model
-- /tools /context /reset /compact /version) are answered without the
-- model.
--
-- Differences from upstream: nothing but JSON-RPC is written to stdout
-- (upstream's tool spinner and error lines leak onto it); the agent's own
-- output goes to stderr. Every session writes a replay log, as
-- @fisher chat@ does.
module Fisher.Cli.Acp
  ( AcpDeps (..)
  , defaultAcpDeps
  , AcpIO (..)
  , acpServe
  , acpMain
    -- * Pieces, for the specs
  , toolKind
  , toolTitle
  , toolStart
  , toolComplete
  , parseModelInput
  , detectProviderForModel
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, displayException, finally, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, getCurrentDirectory, makeAbsolute)
import System.IO (hFlush, hSetBinaryMode, stderr, stdin, stdout)
import qualified Data.Text.IO as TIO

import Fisher.Agent.Loop
import Fisher.Agent.Replay
import Fisher.Cli.AgentChat (ChatDeps (..), defaultChatDeps)
import Fisher.Cli.Native
import Fisher.Cli.SetupCommon (openrouterModels, providerModels)
import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered)
import Fisher.LLM.Auth.Registry (ProviderConfig (..), lookupProvider, modelProviderAliases, normalizeProvider, providerLabels)
import Fisher.LLM.Auth.Resolve (RuntimeRequest (..), resolveRuntimeProvider)
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Shell.Data (upstreamVersion)
import Fisher.Tools.Agent (AgentTools (..))
import Fisher.Tools.Registry

-- ------------------------------------------------------------------ deps

-- | What the server takes from outside, so the specs can script it.
data AcpDeps = AcpDeps
  { adChat :: ChatDeps
  -- ^ the transport, tools, clock and working directory, as @fisher chat@'s
  , adSessionUuid :: IO Text
  -- ^ @uuid.uuid4()@ in acp_adapter/session.py, as text
  , adToolCallHex :: IO Text
  -- ^ @uuid.uuid4().hex@ in acp_adapter/tools.py
  , adLog :: Text -> IO ()
  -- ^ upstream's logger (stderr)
  , adReplay :: Bool
  -- ^ write a replay log per session
  }

defaultAcpDeps :: Sys -> AcpDeps
defaultAcpDeps sys =
  AcpDeps
    { adChat = defaultChatDeps
    , adSessionUuid = dashed <$> sysUuidHex sys
    , adToolCallHex = sysUuidHex sys
    , adLog = \t -> TIO.hPutStrLn stderr t
    , adReplay = True }
  where
    dashed h = T.intercalate "-" [T.take 8 h, T.take 4 (T.drop 8 h), T.take 4 (T.drop 12 h), T.take 4 (T.drop 16 h), T.drop 20 h]

-- | The two ends of the connection: a line in, a line out.
data AcpIO = AcpIO
  { aiRead :: IO (Maybe BS.ByteString)
  , aiWrite :: BS.ByteString -> IO ()
  }

-- ------------------------------------------------------------------ state

data Hooks = Hooks
  { hkProgress :: Text -> PyVal -> IO ()
  , hkThinking :: Text -> IO ()
  , hkStep :: [Text] -> IO ()
  }

noHooks :: Hooks
noHooks = Hooks (\_ _ -> pure ()) (\_ -> pure ()) (\_ -> pure ())

data AcpAgent = AcpAgent
  { aaAgent :: Agent
  , aaModel :: Text
  , aaProvider :: Text
  , aaReplay :: Maybe ReplayLog
  }

data Session = Session
  { ssId :: Text
  , ssCwd :: IORef FilePath
  , ssModel :: IORef Text
  , ssHistory :: IORef [PyVal]
  , ssAgent :: IORef AcpAgent
  , ssCancel :: IORef Bool
  , ssHooks :: IORef Hooks
  }

data Server = Server
  { svDeps :: AcpDeps
  , svSys :: Sys
  , svSessions :: IORef (M.Map Text Session)
  , svShared :: MVar (Maybe NativeShared)
  , svWrite :: MVar ()
  , svIo :: AcpIO
  , svPending :: IORef (M.Map Int (MVar PyVal))
  }

-- ------------------------------------------------------------------ wire

send :: Server -> PyVal -> IO ()
send sv v = withMVar (svWrite sv) $ \_ -> aiWrite (svIo sv) (TE.encodeUtf8 (dumpsUnicode v) <> "\n")

rpc :: [(Text, PyVal)] -> PyVal
rpc kvs = PDict (("jsonrpc", PStr "2.0") : kvs)

data RpcError = RpcError Int Text (Maybe PyVal)

errorVal :: RpcError -> PyVal
errorVal (RpcError code msg dat) = PDict ([("code", PInt (fromIntegral code)), ("message", PStr msg)] <> maybe [] (\d -> [("data", d)]) dat)

methodNotFound :: Text -> RpcError
methodNotFound m = RpcError (-32601) "Method not found" (Just (PDict [("method", PStr m)]))

internalError :: Text -> RpcError
internalError d = RpcError (-32603) "Internal error" (Just (PDict [("details", PStr d)]))

-- | pydantic's "Field required" errors for the fields a request lacks.
requireFields :: [Text] -> PyVal -> Either RpcError ()
requireFields fields params = case [f | f <- fields, not (has f params)] of
  [] -> Right ()
  ms ->
    Left . RpcError (-32602) "Invalid params" . Just $
      PDict
        [ ( "errors"
          , PList
              [ PDict
                  [ ("type", PStr "missing"), ("loc", PList [PStr f]), ("msg", PStr "Field required")
                  , ("input", params), ("url", PStr "https://errors.pydantic.dev/2.13/v/missing") ]
              | f <- ms ] ) ]

update :: Server -> Text -> [(Text, PyVal)] -> IO ()
update sv sid u = send sv (rpc [("method", PStr "session/update"), ("params", PDict [("sessionId", PStr sid), ("update", PDict u)])])

textBlock :: Text -> PyVal
textBlock t = PDict [("type", PStr "text"), ("text", PStr t)]

messageChunk :: Text -> [(Text, PyVal)]
messageChunk t = [("sessionUpdate", PStr "agent_message_chunk"), ("content", textBlock t)]

thoughtChunk :: Text -> [(Text, PyVal)]
thoughtChunk t = [("sessionUpdate", PStr "agent_thought_chunk"), ("content", textBlock t)]

-- ------------------------------------------------------------------ tools

-- | @TOOL_KIND_MAP@
toolKind :: Text -> Text
toolKind n = fromMaybe "other" (lookup n kinds)
  where
    kinds =
      [ ("read_file", "read"), ("write_file", "edit"), ("patch", "edit"), ("search_files", "search")
      , ("terminal", "execute"), ("process", "execute"), ("execute_code", "execute")
      , ("web_search", "fetch"), ("web_extract", "fetch")
      , ("browser_navigate", "fetch"), ("browser_click", "execute"), ("browser_type", "execute")
      , ("browser_snapshot", "read"), ("browser_vision", "read"), ("browser_scroll", "execute")
      , ("browser_press", "execute"), ("browser_back", "execute"), ("browser_close", "execute")
      , ("browser_get_images", "read"), ("delegate_task", "execute"), ("vision_analyze", "read")
      , ("image_generate", "execute"), ("text_to_speech", "execute"), ("_thinking", "think") ]

argText :: Text -> Text -> PyVal -> Text
argText k dflt args = case get k args of
  Just v -> pyStr v
  Nothing -> dflt

-- | @str(x)@ for what a tool's arguments hold.
pyStr :: PyVal -> Text
pyStr = \case
  PStr s -> s
  v -> pyRepr v

-- | @build_tool_title(tool_name, args)@
toolTitle :: Text -> PyVal -> Text
toolTitle name args = case name of
  "terminal" ->
    let cmd = argText "command" "" args
     in "terminal: " <> (if T.length cmd > 80 then T.take 77 cmd <> "..." else cmd)
  "read_file" -> "read: " <> argText "path" "?" args
  "write_file" -> "write: " <> argText "path" "?" args
  "patch" -> "patch (" <> argText "mode" "replace" args <> "): " <> argText "path" "?" args
  "search_files" -> "search: " <> argText "pattern" "?" args
  "web_search" -> "web search: " <> argText "query" "?" args
  "web_extract" -> case get "urls" args of
    Just (PList us@(u : _)) -> "extract: " <> pyStr u <> (if length us > 1 then " (+" <> tshow (length us - 1) <> ")" else "")
    _ -> "web extract"
  "delegate_task" ->
    let goal = argText "goal" "" args
        g = if T.length goal > 60 then T.take 57 goal <> "..." else goal
     in if T.null goal then "delegate task" else "delegate: " <> g
  "execute_code" -> "execute code"
  "vision_analyze" -> "analyze image: " <> T.take 50 (argText "question" "?" args)
  _ -> name

-- | @extract_locations(arguments)@
locations :: PyVal -> PyVal
locations args = case get "path" args of
  Just p | truthy p ->
    let line = case get "offset" args of
          Just o | truthy o -> Just o
          _ -> case get "line" args of
            Just l | truthy l -> Just l
            _ -> Nothing
     in PList [PDict ([("path", p)] <> maybe [] (\l -> [("line", l)]) line)]
  _ -> PList []

contentText :: Text -> PyVal
contentText t = PDict [("type", PStr "content"), ("content", textBlock t)]

diff :: PyVal -> Text -> Maybe Text -> PyVal
diff path new old = PDict ([("type", PStr "diff"), ("path", path), ("newText", PStr new)] <> maybe [] (\o -> [("oldText", PStr o)]) old)

-- | @build_tool_start(tool_call_id, tool_name, arguments)@
toolStart :: Text -> Text -> PyVal -> [(Text, PyVal)]
toolStart tcId name args =
  [ ("sessionUpdate", PStr "tool_call"), ("toolCallId", PStr tcId), ("title", PStr (toolTitle name args))
  , ("kind", PStr (toolKind name)), ("content", PList content), ("locations", locations args), ("rawInput", args) ]
  where
    path = fromMaybe (PStr "") (get "path" args)
    content = case name of
      "patch"
        | argText "mode" "replace" args == "replace" ->
            [diff path (argText "new_string" "" args) (Just (argText "old_string" "" args))]
        | otherwise -> [contentText (argText "patch" "" args)]
      "write_file" -> [diff path (argText "content" "" args) Nothing]
      "terminal" -> [contentText ("$ " <> argText "command" "" args)]
      "read_file" -> [contentText ("Reading " <> pyStr path)]
      "search_files" -> [contentText ("Searching for '" <> argText "pattern" "" args <> "' (" <> argText "target" "content" args <> ")")]
      _ -> [contentText (dumpsIndent True 2 args)]

-- | @build_tool_complete(tool_call_id, tool_name, result)@
toolComplete :: Text -> Text -> Maybe Text -> [(Text, PyVal)]
toolComplete tcId name result =
  [ ("sessionUpdate", PStr "tool_call_update"), ("toolCallId", PStr tcId), ("kind", PStr (toolKind name))
  , ("status", PStr "completed"), ("content", PList [contentText shown]) ]
    <> maybe [] (\r -> [("rawOutput", PStr r)]) result
  where
    r0 = fromMaybe "" result
    shown = if T.length r0 > 5000 then T.take 4900 r0 <> "\n... (" <> tshow (T.length r0) <> " chars total, truncated)" else r0

-- ------------------------------------------------------------------ models

-- | @_KNOWN_PROVIDER_NAMES@
knownProviders :: [Text]
knownProviders = map fst providerLabels <> map fst modelProviderAliases <> ["openrouter", "custom"]

-- | @parse_model_input(raw, current_provider)@
parseModelInput :: Text -> Text -> (Text, Text)
parseModelInput raw current =
  let stripped = T.strip raw
      (l, r) = T.breakOn ":" stripped
   in if not (T.null r) && not (T.null l)
        then
          let p = T.toLower (T.strip l)
              m = T.strip (T.drop 1 r)
           in if not (T.null p) && not (T.null m) && p `elem` knownProviders
                then (normalizeProvider (Just p), m)
                else (current, stripped)
        else (current, stripped)

-- | @_find_openrouter_slug(model_name)@
findOpenrouterSlug :: Text -> Maybe Text
findOpenrouterSlug name =
  let n = T.toLower (T.strip name)
      ids = map fst openrouterModels
   in if T.null n
        then Nothing
        else case [m | m <- ids, T.toLower m == n] of
          (m : _) -> Just m
          [] -> case [m | m <- ids, "/" `T.isInfixOf` m, T.toLower (T.drop 1 (T.dropWhile (/= '/') m)) == n] of
            (m : _) -> Just m
            [] -> Nothing

catalogProviders :: [Text]
catalogProviders = ["nous", "openai-codex", "zai", "kimi-coding", "minimax", "minimax-cn", "anthropic", "deepseek"]

-- | @detect_provider_for_model(model_name, current_provider)@
detectProviderForModel :: Sys -> Text -> Text -> IO (Maybe (Text, Text))
detectProviderForModel sys model current = do
  let name = T.strip model
      lower = T.toLower name
      inCatalog pid = any ((== lower) . T.toLower) (providerModels pid)
  if T.null name || inCatalog current
    then pure Nothing
    else case [p | p <- catalogProviders, p /= current, p `notElem` ["nous", "openrouter"], inCatalog p] of
      (direct : _) -> do
        creds <- case lookupProvider direct of
          Just pc -> or <$> mapM (\v -> maybe False (not . T.null . T.strip) <$> sysGetEnv sys v) (pcApiKeyEnvVars pc)
          Nothing -> pure False
        pure $
          if creds
            then Just (direct, name)
            else Just (maybe (direct, name) ("openrouter",) (findOpenrouterSlug name))
      [] -> pure $ case findOpenrouterSlug name of
        Just slug
          | current /= "openrouter" -> Just ("openrouter", slug)
          | slug /= name -> Just ("openrouter", slug)
        _ -> Nothing

-- ------------------------------------------------------------------ agents

blank :: Text -> Bool
blank = T.null . T.strip

-- | @detect_provider()@ (acp_adapter/auth.py)
detectProvider :: Sys -> IO (Maybe Text)
detectProvider sys = do
  r <- try @SomeException (resolveRuntimeProvider sys (RuntimeRequest Nothing Nothing Nothing))
  pure $ case r of
    Right rt
      | Just (PStr k) <- get "api_key" rt, not (blank k)
      , Just (PStr p) <- get "provider" rt, not (blank p) -> Just (T.toLower (T.strip p))
    _ -> Nothing

sharedFor :: Server -> IO NativeShared
sharedFor sv = modifyMVar (svShared sv) $ \case
  Just s -> pure (Just s, s)
  Nothing -> do
    s <- newNativeShared (svSys sv) (adChat (svDeps sv)) (adLog (svDeps sv))
    pure (Just s, s)

-- | @SessionManager._make_agent(session_id, cwd, model, provider)@
makeAgent :: Server -> Session -> Text -> Maybe Text -> Maybe Text -> IO AcpAgent
makeAgent sv ss sid model provider = do
  let sys0 = svSys sv
  cfg0 <- loadConfigOrdered sys0
  let (defaultModel, requestedProvider) = case get "model" cfg0 of
        Just d@(PDict _) ->
          ( case get "default" d of
              Just v | truthy v -> pyStrOf v
              _ -> "anthropic/claude-opus-4.6"
          , case get "provider" d of
              Just (PStr p) | not (T.null p) -> Just p
              _ -> Nothing )
        Just (PStr m) | not (blank m) -> (T.strip m, Nothing)
        _ -> ("anthropic/claude-opus-4.6", Nothing)
      req = case provider of
        Just p | not (T.null p) -> Just p
        _ -> requestedProvider
      theModel = fromMaybe defaultModel model
  rtR <- try @SomeException (resolveRuntimeProvider sys0 (RuntimeRequest req Nothing Nothing))
  envKey <- fromMaybe "" <$> sysGetEnv sys0 "OPENROUTER_API_KEY"
  let field' rt k = maybe "" pyStrOf (get k rt)
      spec0 = nativeSpec "acp" sid
      spec1 = case rtR of
        Right rt ->
          spec0
            { nsProvider = if T.null (field' rt "provider") then "openrouter" else field' rt "provider"
            , nsApiMode = fromMaybe ChatCompletions (parseApiMode (field' rt "api_mode"))
            , nsBaseUrl = field' rt "base_url", nsApiKey = field' rt "api_key", nsBaseUrlGiven = True }
        Left _ -> spec0 {nsApiKey = envKey}
      hooks f = readIORef (ssHooks ss) >>= f
      spec =
        spec1
          { nsModel = theModel, nsEnabled = Just ["fisher-acp"], nsReplay = adReplay (svDeps sv)
          , nsOut = adLog (svDeps sv) . T.dropWhileEnd (== '\n')
          , nsCallbacks =
              Callbacks
                (Just (\t -> hooks (\h -> hkThinking h t)))
                (Just (\name _ args -> hooks (\h -> hkProgress h name args)))
                (Just (\_ names -> hooks (\h -> hkStep h names)))
                Nothing }
  sh <- sharedFor sv
  (ag, lg) <- buildNative sys0 (adChat (svDeps sv)) sh spec
  pure (AcpAgent ag theModel (nsProvider spec) lg)

-- ------------------------------------------------------------------ server

-- | @_normalize_cwd(cwd)@
normalizeCwd :: Text -> IO FilePath
normalizeCwd t = do
  let raw = if T.null t then "." else T.unpack t
  r <- try @SomeException (makeAbsolute raw >>= canonicalizeLenient)
  either (const getCurrentDirectory) pure r
  where
    canonicalizeLenient p = either (const p) id <$> try @SomeException (canonicalizePath p)

newSession :: Server -> Text -> Text -> IO Session
newSession sv sid cwdT = do
  cwd <- normalizeCwd cwdT
  ss <- Session sid <$> newIORef cwd <*> newIORef "" <*> newIORef [] <*> newIORef undefinedAgent <*> newIORef False <*> newIORef noHooks
  ag <- makeAgent sv ss sid Nothing Nothing
  writeIORef (ssAgent ss) ag
  writeIORef (ssModel ss) (aaModel ag)
  modifyIORef (svSessions sv) (M.insert sid ss)
  pure ss
  where
    undefinedAgent = error "the session's agent is made before it is used"

param :: Text -> PyVal -> Text
param k p = maybe "" pyStrOf (get k p)

handleRequest :: Server -> Text -> PyVal -> IO (Either RpcError PyVal)
handleRequest sv method params = case method of
  "initialize" -> withFields ["protocolVersion"] $ do
    prov <- detectProvider (svSys sv)
    pure . Right . PDict $
      [ ("protocolVersion", PInt 1)
      , ("agentCapabilities", PDict [("loadSession", PBool True), ("sessionCapabilities", PDict [("fork", PDict []), ("list", PDict []), ("resume", PDict [])])])
      , ("agentInfo", PDict [("name", PStr "fisher-agent"), ("version", PStr upstreamVersion)]) ]
        <> maybe [] (\p -> [("authMethods", PList [PDict [("description", PStr ("Authenticate Fisher using the currently configured " <> p <> " runtime credentials.")), ("id", PStr p), ("name", PStr (p <> " runtime credentials"))]])]) prov
  "authenticate" -> withFields ["methodId"] (pure (Right (PDict [])))
  "session/new" -> withFields ["cwd", "mcpServers"] $ do
    sid <- adSessionUuid (svDeps sv)
    _ <- newSession sv sid (param "cwd" params)
    pure (Right (PDict [("sessionId", PStr sid)]))
  "session/load" -> withFields ["cwd", "mcpServers", "sessionId"] $ do
    sessions <- readIORef (svSessions sv)
    case M.lookup (param "sessionId" params) sessions of
      Nothing -> adLog (svDeps sv) ("WARNING:acp_adapter.server:load_session: session " <> param "sessionId" params <> " not found")
      Just ss -> normalizeCwd (param "cwd" params) >>= writeIORef (ssCwd ss)
    pure (Right (PDict []))
  "session/set_mode" -> withFields ["modeId", "sessionId"] (pure (Right (PDict [])))
  "session/set_config_option" -> withFields ["configId", "sessionId", "value"] (pure (Right (PDict [])))
  "session/prompt" -> withFields ["prompt", "sessionId"] (Right <$> prompt sv (param "sessionId" params) (case get "prompt" params of Just (PList bs) -> bs; _ -> []))
  m | m `elem` ["session/list", "session/set_model", "session/fork", "session/resume"] -> pure (Left (methodNotFound m))
  m -> pure (Left (methodNotFound m))
  where
    withFields fs act = either (pure . Left) (const act) (requireFields fs params)

handleNotification :: Server -> Text -> PyVal -> IO ()
handleNotification sv method params = case method of
  "session/cancel" | has "sessionId" params -> do
    sessions <- readIORef (svSessions sv)
    forM_ (M.lookup (param "sessionId" params) sessions) $ \ss -> do
      writeIORef (ssCancel ss) True
      ag <- readIORef (ssAgent ss)
      void (try @SomeException (interruptAgent (aaAgent ag) Nothing))
  _ -> pure ()

-- | @GaussACPAgent.prompt(prompt, session_id)@
prompt :: Server -> Text -> [PyVal] -> IO PyVal
prompt sv sid blocks = do
  sessions <- readIORef (svSessions sv)
  case M.lookup sid sessions of
    Nothing -> do
      adLog (svDeps sv) ("ERROR:acp_adapter.server:prompt: session " <> sid <> " not found")
      pure (stop "refusal")
    Just ss -> do
      let userText = T.strip (T.intercalate "\n" [t | b <- blocks, get "type" b == Just (PStr "text"), Just (PStr t) <- [get "text" b]])
      slash <- if "/" `T.isPrefixOf` userText then slashCommand sv ss userText else pure Nothing
      case () of
        _
          | T.null userText -> pure (stop "end_turn")
          | Just reply <- slash -> update sv sid (messageChunk reply) >> pure (stop "end_turn")
          | otherwise -> do
              writeIORef (ssCancel ss) False
              ids <- newIORef (M.empty :: M.Map Text [Text])
              writeIORef (ssHooks ss) $
                Hooks
                  { hkProgress = \name args0 -> do
                      let args = case args0 of
                            PStr s -> either (const (PDict [("raw", PStr s)])) id (loads (TE.encodeUtf8 s))
                            v -> v
                          args' = case args of
                            PDict _ -> args
                            _ -> PDict []
                      tc <- ("tc-" <>) . T.take 12 <$> adToolCallHex (svDeps sv)
                      modifyIORef ids (M.insertWith (flip (<>)) name [tc])
                      update sv sid (toolStart tc name args')
                  , hkThinking = \t -> unless (T.null t) (update sv sid (thoughtChunk t))
                  , hkStep = \names -> forM_ names $ \name -> do
                      q <- M.findWithDefault [] name <$> readIORef ids
                      case q of
                        (tc : rest) -> do
                          modifyIORef ids (if null rest then M.delete name else M.insert name rest)
                          update sv sid (toolComplete tc name Nothing)
                        [] -> pure () }
              ag <- readIORef (ssAgent ss)
              hist <- readIORef (ssHistory ss)
              let ri = (runInput userText) {riHistory = hist, riTaskId = Just sid}
              forM_ (aaReplay ag) (`logTurn` ri)
              r <- try @SomeException (runConversation (aaAgent ag) ri) `finally` writeIORef (ssHooks ss) noHooks
              forM_ (aaReplay ag) $ \l -> either (const (pure ())) (logOutcome l) r
              let (final, msgs, usage) = case r of
                    Left e -> ("Error: " <> T.pack (displayException e), Nothing, Nothing)
                    Right (RunRaised _ m) -> ("Error: " <> m, Nothing, Nothing)
                    Right (RunDone d) ->
                      ( case get "final_response" d of
                          Just (PStr s) -> s
                          _ -> ""
                      , case get "messages" d of
                          Just (PList ms) | not (null ms) -> Just ms
                          _ -> Nothing
                      , case get "usage" d of
                          Just u@(PDict _) -> Just u
                          _ -> Nothing )
              forM_ msgs (writeIORef (ssHistory ss))
              unless (T.null final) (update sv sid (messageChunk final))
              cancelled <- readIORef (ssCancel ss)
              let u = fmap (\d -> PDict ([ (k', fromMaybe (PInt 0) (get k d)) | (k', k) <- [("inputTokens", "prompt_tokens"), ("outputTokens", "completion_tokens"), ("totalTokens", "total_tokens")]]
                                          <> [(k', v) | (k', k) <- [("thoughtTokens", "reasoning_tokens"), ("cachedReadTokens", "cached_tokens")], Just v <- [get k d], v /= PNull])) usage
              pure (PDict ([("stopReason", PStr (if cancelled then "cancelled" else "end_turn"))] <> maybe [] (\x -> [("usage", x)]) u))
  where
    stop r = PDict [("stopReason", PStr r)]

-- ------------------------------------------------------------------ slash commands

slashCommands :: [(Text, Text)]
slashCommands =
  [ ("help", "Show available commands"), ("model", "Show or change current model"), ("tools", "List available tools")
  , ("context", "Show conversation context info"), ("reset", "Clear conversation history")
  , ("compact", "Compress conversation context"), ("version", "Show Fisher version") ]

-- | @_handle_slash_command(text, state)@: Nothing for a command it does
-- not know (the text goes to the model).
slashCommand :: Server -> Session -> Text -> IO (Maybe Text)
slashCommand sv ss text = do
  let (cmd0, rest) = T.break isSpace (T.stripStart text)
      cmd = T.toLower (T.dropWhile (== '/') cmd0)
      args = T.strip rest
  if cmd `notElem` map fst slashCommands
    then pure Nothing
    else do
      r <- try @SomeException (run cmd args)
      pure . Just $ case r of
        Right t -> t
        Left e -> "Error executing /" <> cmd <> ": " <> T.pack (displayException e)
  where
    run cmd args = case cmd of
      "help" ->
        pure . T.intercalate "\n" $
          ["Available commands:", ""] <> ["  /" <> T.justifyLeft 10 ' ' c <> "  " <> d | (c, d) <- slashCommands]
            <> ["", "Unrecognized /commands are sent to the model as normal messages."]
      "model"
        | T.null args -> do
            m <- readIORef (ssModel ss)
            ag <- readIORef (ssAgent ss)
            pure ("Current model: " <> (if T.null m then aaModel ag else m) <> "\nProvider: " <> (if T.null (aaProvider ag) then "auto" else aaProvider ag))
        | otherwise -> do
            ag <- readIORef (ssAgent ss)
            let current = if T.null (aaProvider ag) then "openrouter" else aaProvider ag
                (target0, model0) = parseModelInput args current
            (target, newModel) <-
              if target0 == current
                then fromMaybe (target0, model0) <$> detectProviderForModel (svSys sv) model0 current
                else pure (target0, model0)
            writeIORef (ssModel ss) newModel
            ag' <- makeAgent sv ss (ssId ss) (Just newModel) (Just target)
            writeIORef (ssAgent ss) ag'
            pure ("Model switched to: " <> newModel <> "\nProvider: " <> (if T.null target then aaProvider ag' else target))
      "tools" -> do
        sh <- sharedFor sv
        (defs, _) <- getToolDefinitions (atRegistry (nsTools sh)) (Just ["fisher-acp"]) Nothing
        let defs' = maybe defs fst (cdTools (adChat (svDeps sv)))
        pure $
          if null defs'
            then "No tools available."
            else
              T.intercalate "\n" $
                ("Available tools (" <> tshow (length defs') <> "):")
                  : [ "  " <> n <> ": " <> (if T.length d > 80 then T.take 77 d <> "..." else d)
                    | t <- defs'
                    , let f = fromMaybe PNull (get "function" t)
                          n = maybe "?" pyStrOf (get "name" f)
                          d = maybe "" pyStrOf (get "description" f) ]
      "context" -> do
        hist <- readIORef (ssHistory ss)
        if null hist
          then pure "Conversation is empty (no messages yet)."
          else do
            m <- readIORef (ssModel ss)
            ag <- readIORef (ssAgent ss)
            let count r = length [() | h <- hist, maybe "unknown" pyStrOf (get "role" h) == r]
                model = if T.null m then aaModel ag else m
            pure . T.intercalate "\n" $
              [ "Conversation: " <> tshow (length hist) <> " messages"
              , "  user: " <> tshow (count "user") <> ", assistant: " <> tshow (count "assistant")
                  <> ", tool: " <> tshow (count "tool") <> ", system: " <> tshow (count "system") ]
                <> ["Model: " <> model | not (T.null model)]
      "reset" -> writeIORef (ssHistory ss) [] >> pure "Conversation history cleared."
      "compact" -> do
        hist <- readIORef (ssHistory ss)
        pure (if null hist then "Nothing to compress — conversation is empty." else "Context compression not available for this agent.")
      _ -> pure ("Fisher Agent v" <> upstreamVersion)

-- ------------------------------------------------------------------ loop

-- | Serve one connection until its input ends.
acpServe :: AcpDeps -> Sys -> AcpIO -> IO ()
acpServe deps sys io = do
  sv <- Server deps sys <$> newIORef M.empty <*> newMVar Nothing <*> newMVar () <*> pure io <*> newIORef M.empty
  inflight <- newIORef (0 :: Int)
  done <- newEmptyMVar
  let loop = aiRead io >>= \case
        Nothing -> pure ()
        Just line
          | BC.all isSpace line -> loop
          | otherwise -> do
              case loads line of
                Left _ -> adLog deps ("ERROR: could not parse " <> T.take 200 (TE.decodeUtf8With (\_ _ -> Just '?') line))
                Right msg -> route sv inflight done msg
              loop
  loop
  -- let requests still running answer before the connection closes
  n <- readIORef inflight
  when (n > 0) (takeMVar done)
  where
    route sv inflight done msg = case (get "method" msg, get "id" msg) of
      (Just (PStr method), Just rid) -> do
        atomicModifyIORef' inflight (\n -> (n + 1, ()))
        void . forkIO $ do
          r <- try @SomeException (handleRequest sv method (fromMaybe (PDict []) (nonNull (get "params" msg))))
          let reply = case r of
                Right (Right v) -> [("id", rid), ("result", v)]
                Right (Left e) -> [("id", rid), ("error", errorVal e)]
                Left e -> [("id", rid), ("error", errorVal (internalError (T.pack (displayException e))))]
          send sv (rpc reply) `finally` do
            left <- atomicModifyIORef' inflight (\n -> (n - 1, n - 1))
            when (left == 0) (void (tryPutMVar done ()))
      (Just (PStr method), Nothing) -> handleNotification sv method (fromMaybe (PDict []) (nonNull (get "params" msg)))
      (Nothing, Just (PInt i)) -> do
        pending <- readIORef (svPending sv)
        forM_ (M.lookup (fromIntegral i) pending) $ \mv -> void (tryPutMVar mv msg)
      _ -> pure ()
    nonNull = \case
      Just PNull -> Nothing
      v -> v

-- | @fisher acp@
acpMain :: Sys -> [Text] -> IO Int
acpMain sys0 _ = do
  loadDotenvIntoSys sys0
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  let sys = sys0 {sysOut = \t -> TIO.hPutStr stderr t}
      io = AcpIO (either (const Nothing) Just <$> try @SomeException (BS.hGetLine stdin)) (\b -> BS.hPut stdout b >> hFlush stdout)
  acpServe (defaultAcpDeps sys) sys io
  pure 0

tshow :: Show a => a -> Text
tshow = T.pack . show
