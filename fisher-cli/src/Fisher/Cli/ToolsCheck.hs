-- | @fisher tools-check@ (Fisher only): build the agent's tool set the way
-- the agent will (file tools on the configured sandbox, MCP servers from
-- @mcp_servers@ in config.yaml), then list the definitions the model would
-- see, call one tool, or show MCP status.
--
-- MCP sampling goes to the main provider and model (the auxiliary-model
-- routing upstream uses for task "mcp" arrives with the agent loop, B3).
module Fisher.Cli.ToolsCheck
  ( toolsCheckMain
  , mainProviderSampling
  ) where

import Control.Exception (SomeException, throwIO, try)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO (stderr)

import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered)
import Fisher.LLM.Auth.Resolve (defaultRuntimeRequest, resolveRuntimeProvider)
import Fisher.LLM.Client
import Fisher.LLM.Errors (errorClass, errorText)
import Fisher.LLM.Http (systemRuntime, tlsTransport)
import Fisher.LLM.Normalize (Outcome (..), Usage (..))
import Fisher.LLM.Prepare
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Mcp.Client
import Fisher.Mcp.HttpClient (tlsHttpClient)
import Fisher.Mcp.Sampling (SamplingCall (..), SamplingLLM)
import Fisher.Py.Value
import Fisher.Tools.Agent
import Fisher.Tools.Registry
import Fisher.Tools.Sandbox.Config (cleanupAll)
import Fisher.Agent.Tools.Browser (cleanupAllBrowsers)
import Fisher.Cli.NetTools
import System.Directory (findExecutable)

usage :: Text
usage =
  "usage: fisher tools-check [list] [--toolsets A,B] [--disable A,B]\n\
  \       fisher tools-check call NAME [ARGS_JSON] [--task ID]\n\
  \       fisher tools-check calls '[[NAME, ARGS], ...]'\n\
  \       fisher tools-check status"

toolsCheckMain :: Sys -> [Text] -> IO Int
toolsCheckMain sys args = do
  loadDotenvIntoSys sys
  cfg <- loadConfigOrdered sys
  at <- newAgentTools (sysGaussHome sys)
  http <- tlsHttpClient
  environ <- sysEnviron
  let env =
        McpEnv
          { meRegistry = atRegistry at
          , meLog = \lvl msg -> if lvl == "DEBUG" then pure () else TIO.hPutStrLn stderr ("mcp " <> T.toLower lvl <> ": " <> msg)
          , meSampling = mainProviderSampling sys cfg
          , meEnviron = pure environ
          , meHome = T.pack (sysHome sys)
          , meGaussHome = sysGaussHome sys
          , meHttp = Just http
          }
  mc <- newMcpClient env
  tr <- tlsTransport
  net <- newNetTools sys systemRuntime tr (fmap (fmap T.pack) . findExecutable . T.unpack) (pure (maybe False truthy (getPath ["browser", "record_sessions"] cfg))) (ttyUi (outLn sys) (sysOut sys)) at
  registerNetTools (atRegistry at) net
  let finish code = shutdownMcp mc >> cleanupAllBrowsers (ntBrowser net) >> cleanupAll (atSandboxes at) >> pure code
      reg = atRegistry at
  case args of
    ("call" : name : rest) -> do
      _ <- discoverFromConfig mc cfg
      let (task, rest') = case break (== "--task") rest of
            (a, _ : t : b) -> (t, a <> b)
            (a, _) -> ("default", a)
      callArgs <- case rest' of
        [] -> pure (Right (PDict []))
        [j] -> pure (loads (TE.encodeUtf8 j))
        _ -> pure (Left "too many arguments")
      case callArgs of
        Left e -> outLn sys (usage <> "\nfisher tools-check: error: " <> T.pack e) >> finish 2
        Right a -> do
          r <- handleFunctionCall reg defaultCtx {tcTaskId = task} name a
          outLn sys r
          finish 0
    -- several calls in one process (a browser session lives as long as it)
    ["calls", j] -> do
      _ <- discoverFromConfig mc cfg
      case loads (TE.encodeUtf8 j) of
        Right (PList calls) -> do
          mapM_ (\c -> case c of
            PList [PStr name, a] -> handleFunctionCall reg defaultCtx {tcTaskId = "default"} name a >>= outLn sys
            _ -> outLn sys "{\"error\": \"each call is [name, args]\"}") calls
          finish 0
        _ -> outLn sys (usage <> "\nfisher tools-check: error: calls takes a JSON list of [name, args]") >> finish 2
    ["status"] -> do
      _ <- discoverFromConfig mc cfg
      st <- mcpStatus mc cfg
      outLn sys (dumpsIndent False 2 st)
      finish 0
    _ -> case listOpts (dropList args) of
      Left e -> outLn sys (usage <> "\nfisher tools-check: error: " <> e) >> finish 2
      Right (en, dis) -> do
        _ <- discoverFromConfig mc cfg
        (defs, printed) <- getToolDefinitions reg en dis
        mapM_ (outLn sys) printed
        mapM_ (\d -> outLn sys ("  " <> defName d)) defs
        finish 0
  where
    dropList = \case
      ("list" : r) -> r
      r -> r
    listOpts = go (Nothing, Nothing)
      where
        go acc = \case
          [] -> Right acc
          ("--toolsets" : v : r) -> go (Just (T.splitOn "," v), snd acc) r
          ("--disable" : v : r) -> go (fst acc, Just (T.splitOn "," v)) r
          (x : _) -> Left ("unrecognized argument: " <> x)
    defName d = case get "function" d >>= get "name" of
      Just (PStr n) -> n
      _ -> "?"
    sysEnviron = do
      -- the process environment, as the MCP stdio spawn filters it
      let names = ["PATH", "HOME", "USER", "LANG", "LC_ALL", "TERM", "SHELL", "TMPDIR", "LOGNAME"]
      vals <- mapM (\k -> fmap ((,) k) <$> sysGetEnv sys k) names
      pure [kv | Just kv <- vals]

-- | Sampling through the main provider: the requested model, else
-- @AUXILIARY_MCP_MODEL@, else the configured one.
mainProviderSampling :: Sys -> PyVal -> SamplingLLM
mainProviderSampling sys cfg call = do
  rt <- try (resolveRuntimeProvider sys defaultRuntimeRequest)
  r <- either (\(e :: SomeException) -> throwIO e) pure rt
  aux <- sysGetEnv sys "AUXILIARY_MCP_MODEL"
  let field k = maybe "" pyStrOf (get k r)
      cfgModel = case get "model" cfg of
        Just (PStr m) -> m
        Just d@(PDict _) -> maybe "" pyStrOf (get "default" d)
        _ -> ""
      model = fromMaybe (fromMaybe cfgModel (nonEmpty =<< aux)) (scModel call)
      mode = fromMaybe ChatCompletions (parseApiMode (field "api_mode"))
      s =
        defaultSettings
          { sMode = mode, sBaseUrl = field "base_url", sApiKey = field "api_key", sModel = model
          , sProvider = field "provider", sMaxTokens = Just (scMaxTokens call), sSessionId = "fisher-mcp-sampling", sTools = scTools call }
      msgs = prepareMessages (PrepareInput (sBaseUrl s) Nothing Nothing [] False) (scMessages call)
  tr <- tlsTransport
  res <- callModel systemRuntime tr (Env (sysUuidHex sys) (\_ _ -> pure "[image omitted]")) s msgs
  case res of
    Left e -> throwIO (userError (T.unpack (errorClass e <> ": " <> errorText e)))
    Right (CallInvalid _) -> pure (PDict [("choices", PList []), ("model", PStr model)])
    Right (CallOk outcome _) ->
      pure . PDict $
        [ ("choices", PList [PDict [("message", oMessage outcome), ("finish_reason", oFinish outcome)]])
        , ("model", PStr model)
        ]
          <> maybe [] (\u -> [("usage", PDict [("total_tokens", PInt (uTotal u))])]) (oUsage outcome)
  where
    nonEmpty t = if T.null t then Nothing else Just t
