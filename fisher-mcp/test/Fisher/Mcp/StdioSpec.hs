-- | Fisher-only keys of a stdio server's entry, which upstream does not
-- have (test/mcp/fake_server.py, run with python3): @cwd@, the directory
-- the server starts in; @reconnect_on_exit@, a server whose output closes
-- is reconnected; @fisher_messages@, a timeout's text names its seconds.
module Fisher.Mcp.StdioSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, createDirectoryIfMissing, makeAbsolute)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Mcp.Client
import Fisher.Py.Value
import Fisher.Tools.Registry

scenario :: PyVal
scenario =
  PDict
    [ ("tools", PList [tool "die", tool "ok", tool "slow"])
    , ( "handlers"
      , PDict
          [ ("tools/call:die", PList [PDict [("exit", PInt 0)]])
          , ("tools/call:ok", PList [PDict [("result", PDict [("content", PList [PDict [("type", PStr "text"), ("text", PStr "ok")]])])]])
          , ("tools/call:slow", PList [PDict [("sleep", PInt 5)]]) ] ) ]
  where
    tool n = PDict [("name", PStr n), ("inputSchema", PDict [("type", PStr "object")])]

data Run = Run {rReg :: Registry, rLogs :: IORef [(Text, Text)], rWire :: FilePath}

-- | One server @s@ with the given extra keys, connected; its wire log.
withServer :: [(Text, PyVal)] -> (FilePath -> Run -> IO a) -> IO a
withServer extra act = withSystemTempDirectory "fisher-mcp-stdio" $ \d0 -> do
  d <- canonicalizePath d0
  server <- makeAbsolute "test/mcp/fake_server.py"
  let scen = d </> "scenario.json"
      wire = d </> "wire.log"
  BS.writeFile scen (TE.encodeUtf8 (dumpsUnicode scenario))
  reg <- newRegistry
  logs <- newIORef []
  let env =
        McpEnv
          { meRegistry = reg
          , meLog = \lvl msg -> modifyIORef logs (<> [(lvl, msg)])
          , meSampling = \_ -> fail "no sampling"
          , meEnviron = pure [("HOME", T.pack d), ("PATH", "/usr/local/bin:/usr/bin:/bin")]
          , meHome = T.pack d
          , meFisherHome = d </> "fisher"
          , meHttp = Nothing }
      entry = PDict ([("command", PStr "python3"), ("args", PList (map PStr [T.pack server, T.pack scen, T.pack wire, "--log-cwd"])), ("sampling", PDict [("enabled", PBool False)])] <> extra)
  bracket (newMcpClient env) shutdownMcp $ \mc -> do
    _ <- discover mc (PDict [("s", entry)])
    act d (Run reg logs wire)

call :: Run -> Text -> IO Text
call r n = dispatch (rReg r) defaultCtx n (PDict [])

wireCwds :: FilePath -> IO [PyVal]
wireCwds p = do
  b <- BS.readFile p
  pure [c | l <- T.lines (TE.decodeUtf8 b), not (T.null l), Right v <- [loads (TE.encodeUtf8 l)], Just c <- [get "cwd" v]]

spec :: Spec
spec = do
  describe "cwd" $ do
    it "starts the server in the directory given" $ withSystemTempDirectory "fisher-mcp-cwd" $ \w0 -> do
      w <- canonicalizePath w0
      withServer [("cwd", PStr (T.pack w))] $ \_ r -> do
        call r "mcp_s_ok" `shouldReturn` "{\"result\": \"ok\"}"
        wireCwds (rWire r) `shouldReturn` [PStr (T.pack w)]

    it "starts it in ours without one" $
      withServer [] $ \_ r -> do
        here <- canonicalizePath "."
        _ <- call r "mcp_s_ok"
        wireCwds (rWire r) `shouldReturn` [PStr (T.pack here)]

    it "fails the connection, naming the directory, when it does not exist" $
      withServer [("cwd", PStr "/nonexistent/fisher-dir")] $ \_ r -> do
        logs <- readIORef (rLogs r)
        [m | ("WARNING", m) <- logs] `shouldBe` ["Failed to connect to MCP server 's' (command=python3): working directory does not exist: /nonexistent/fisher-dir"]
        call r "mcp_s_ok" `shouldReturn` "{\"error\": \"Unknown tool: mcp_s_ok\"}"

  describe "reconnect_on_exit" $ do
    it "reconnects a server that exits, and a later call returns" $
      withServer [("reconnect_on_exit", PBool True)] $ \_ r -> do
        call r "mcp_s_ok" `shouldReturn` "{\"result\": \"ok\"}"
        call r "mcp_s_die" `shouldReturn` "{\"error\": \"MCP call failed: McpError: Connection closed\"}"
        let retry :: Int -> IO Text
            retry n = do
              x <- call r "mcp_s_ok"
              if x == "{\"result\": \"ok\"}" || n <= 0 then pure x else threadDelay 250000 >> retry (n - 1)
        retry 60 `shouldReturn` "{\"result\": \"ok\"}"
        length <$> wireCwds (rWire r) `shouldReturn` 2
        logs <- readIORef (rLogs r)
        [m | ("WARNING", m) <- logs] `shouldBe` ["MCP server 's' connection lost (attempt 1/5), reconnecting in 1s: Connection closed"]

    it "leaves it closed without the key, as upstream does" $
      withServer [] $ \_ r -> do
        _ <- call r "mcp_s_die"
        threadDelay 1500000
        call r "mcp_s_ok" `shouldReturn` "{\"error\": \"MCP call failed: ClosedResourceError: \"}"

  describe "fisher_messages" $ do
    it "names the timeout's seconds" $
      withServer [("timeout", PInt 1), ("fisher_messages", PBool True)] $ \_ r ->
        call r "mcp_s_slow" `shouldReturn` "{\"error\": \"MCP call failed: TimeoutError: no reply within 1 second\"}"

    it "keeps upstream's text without the key" $
      withServer [("timeout", PInt 1)] $ \_ r ->
        call r "mcp_s_slow" `shouldReturn` "{\"error\": \"MCP call failed: TimeoutError: \"}"
