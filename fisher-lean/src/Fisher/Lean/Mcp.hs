-- | Fisher's stand-in for the @lean-lsp-mcp@ server: MCP over stdio
-- (newline-delimited JSON-RPC), answering @initialize@ and @tools/list@ as
-- lean-lsp-mcp 0.31.0 does (its text, for the C1 tools) and running
-- @tools/call@ with Fisher's ports. Requests run concurrently, as the
-- Python SDK runs them; @notifications/cancelled@ stops one, and a
-- cancelled request gets no answer.
module Fisher.Lean.Mcp
  ( serveMcp
  , handleMessage
  , negotiateVersion
  , mcpData
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (AsyncException, SomeException, finally, fromException, try)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.IO

import Fisher.Lean.McpData (mcpDataJson)
import Fisher.Lean.Session
import Fisher.Lean.Tool
import Fisher.Lean.Tools
import Fisher.Py.Value

mcpData :: PyVal
mcpData = either (error . ("McpData: " <>)) id (loads (TE.encodeUtf8 mcpDataJson))

-- | The SDK's answer to a client's offer: the offer when it is a handshake
-- version, else the newest handshake version.
negotiateVersion :: Maybe Text -> Text
negotiateVersion = \case
  Just v | v `elem` ["2024-11-05", "2025-03-26", "2025-06-18", "2025-11-25"] -> v
  _ -> "2025-11-25"

field' :: Text -> PyVal
field' k = maybe PNull id (get k mcpData)

-- | One message in, the answer to send (if any). Tool calls are run by the
-- caller so they can run concurrently; this answers the rest.
handleMessage :: PyVal -> Either (PyVal, Text, PyVal) (Maybe PyVal)
handleMessage msg =
  case (get "id" msg, get "method" msg) of
    (Just rid, Just (PStr m)) -> case m of
      "tools/call" -> Left (rid, toolName', arguments)
      _ -> Right (Just (answer rid m))
    _ -> Right Nothing
  where
    params = maybe (PDict []) id (get "params" msg)
    toolName' = case get "name" params of Just (PStr n) -> n; _ -> ""
    arguments = case get "arguments" params of Just v@(PDict _) -> v; _ -> PDict []
    ok rid r = PDict [("jsonrpc", PStr "2.0"), ("id", rid), ("result", r)]
    answer rid = \case
      "initialize" ->
        ok rid $
          PDict
            [ ("capabilities", field' "capabilities"), ("instructions", field' "instructions")
            , ("protocolVersion", PStr (negotiateVersion (case get "protocolVersion" params of Just (PStr v) -> Just v; _ -> Nothing)))
            , ("serverInfo", field' "serverInfo") ]
      "ping" -> ok rid (PDict [])
      "tools/list" -> ok rid (PDict [("tools", field' "tools")])
      "prompts/list" -> ok rid (PDict [("prompts", PList [])])
      "resources/list" -> ok rid (PDict [("resources", PList [])])
      "resources/templates/list" -> ok rid (PDict [("resourceTemplates", PList [])])
      m -> PDict [("jsonrpc", PStr "2.0"), ("id", rid), ("error", PDict [("code", PInt (-32601)), ("message", PStr "Method not found"), ("data", PStr m)])]

-- | Serve until stdin closes.
serveMcp :: LeanEnv -> Handle -> Handle -> IO ()
serveMcp env hin hout = do
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  hSetBuffering hout NoBuffering
  s <- newSession env
  outLock <- newMVar ()
  running <- newIORef (M.empty :: M.Map Text ThreadId)
  let send v = withMVar outLock $ \_ -> BS.hPut hout (TE.encodeUtf8 (dumpsUnicode v) <> "\n") >> hFlush hout
      key = dumpsUnicode
      loop = do
        eof <- hIsEOF hin
        unless eof $ do
          line <- BS.hGetLine hin
          unless (BC.all (`elem` (" \t\r" :: String)) line) $
            case loads line of
              Left _ -> pure ()
              Right msg -> dispatch msg
          loop
      dispatch msg = case get "method" msg of
        Just (PStr "notifications/cancelled") ->
          forM_ (getPath ["params", "requestId"] msg) $ \rid -> do
            t <- M.lookup (key rid) <$> readIORef running
            forM_ t killThread
        _ -> case handleMessage msg of
          Right reply -> forM_ reply send
          Left (rid, name, args) -> do
            gate <- newEmptyMVar
            tid <- forkIO $ do
              -- registered before it runs, so an early cancel still finds it
              takeMVar gate
              r <- try @SomeException $ case findTool name of
                Nothing -> pure (ToolResult ("Unknown tool: " <> name) Nothing True)
                Just t -> runTool t s args
              case r of
                Left e | Just (_ :: AsyncException) <- fromException e -> pure ()
                _ -> do
                  let res = either (\e -> ToolResult ("Error executing tool " <> name <> ": " <> exceptionText e) Nothing True) id r
                  send (PDict [("jsonrpc", PStr "2.0"), ("id", rid), ("result", resultPy res)])
              atomicModifyIORef' running (\m -> (M.delete (key rid) m, ()))
            atomicModifyIORef' running (\m -> (M.insert (key rid) tid m, ()))
            putMVar gate ()
  let drain = do
        -- stdin closed: let the calls in flight answer, then stop
        busy <- not . M.null <$> readIORef running
        if busy then threadDelay 10000 >> drain else pure ()
  (loop >> drain) `finally` closeSession s
