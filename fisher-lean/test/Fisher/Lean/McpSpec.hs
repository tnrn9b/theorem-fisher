-- | The MCP front: what lean-lsp-mcp 0.31.0 answered (recorded with
-- tools/corpus/c1-mcp-dump.py and probed by hand), message for message.
module Fisher.Lean.McpSpec (spec) where

import Control.Concurrent.Async (async, wait)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.IO
import System.Process (createPipe)
import Test.Hspec

import Fisher.Lean.Mcp
import Fisher.Lean.Session
import Fisher.Py.Value

-- | Run the server over pipes on these messages; the answers in order.
exchange :: [PyVal] -> IO [PyVal]
exchange msgs = do
  (inR, inW) <- createPipe
  (outR, outW) <- createPipe
  let env = defaultLeanEnv {leGetEnv = \k -> pure (M.lookup k (M.fromList ([] :: [(Text, Text)])))}
  reader <- async (BS.hGetContents outR)
  writer <- async (mapM_ (\m -> BS.hPut inW (TE.encodeUtf8 (dumpsUnicode m) <> "\n")) msgs >> hClose inW)
  serveMcp env inR outW
  hClose outW
  wait writer
  out <- wait reader
  pure [v | l <- BC.lines out, not (BS.null l), Right v <- [loads l]]

req :: Integer -> Text -> PyVal -> PyVal
req i m p = PDict [("jsonrpc", PStr "2.0"), ("id", PInt i), ("method", PStr m), ("params", p)]

initReq :: Text -> PyVal
initReq v = req 1 "initialize" (PDict [("protocolVersion", PStr v), ("capabilities", PDict []), ("clientInfo", PDict [("name", PStr "t"), ("version", PStr "0")])])

result :: PyVal -> PyVal
result = maybe PNull id . get "result"

spec :: Spec
spec = describe "fisher's lean MCP server (as lean-lsp-mcp 0.31.0)" $ do
  it "negotiates the protocol version as the SDK does" $
    map negotiateVersion [Just "2024-11-05", Just "2025-06-18", Just "2025-11-25", Just "2026-07-28", Just "2099-01-01", Nothing]
      `shouldBe` ["2024-11-05", "2025-06-18", "2025-11-25", "2025-11-25", "2025-11-25", "2025-11-25"]
  it "answers initialize with lean-lsp-mcp's server info, capabilities and instructions" $ do
    [r] <- exchange [initReq "2025-06-18"]
    get "serverInfo" (result r) `shouldBe` Just (PDict [("name", PStr "Lean LSP"), ("version", PStr "0.31.0")])
    get "protocolVersion" (result r) `shouldBe` Just (PStr "2025-06-18")
    get "instructions" (result r) `shouldBe` get "instructions" mcpData
  it "lists all 22 tools with lean-lsp-mcp's schemas" $ do
    [_, r] <- exchange [initReq "2025-06-18", req 2 "tools/list" (PDict [])]
    case get "tools" (result r) of
      Just (PList ts) -> length ts `shouldBe` 22
      _ -> expectationFailure "no tools"
  it "answers ping, unknown methods, unknown tools, bad arguments and an unbound project as lean-lsp-mcp did" $ do
    rs <-
      exchange
        [ initReq "2025-06-18"
        , req 10 "ping" (PDict [])
        , req 11 "tools/call" (PDict [("name", PStr "nope"), ("arguments", PDict [])])
        , req 12 "bogus/method" (PDict [])
        , req 13 "tools/call" (PDict [("name", PStr "lean_goal"), ("arguments", PDict [("line", PInt 1)])])
        , req 14 "tools/call" (PDict [("name", PStr "lean_run_code"), ("arguments", PDict [("code", PStr "x")])])
        , req 15 "prompts/list" (PDict [])
        , req 16 "resources/list" (PDict []) ]
    let byId i = [r | r <- rs, get "id" r == Just (PInt i)]
        text r = getPath ["result", "content"] r
    map (result) (byId 10) `shouldBe` [PDict []]
    map text (byId 11) `shouldBe` [Just (PList [PDict [("text", PStr "Unknown tool: nope"), ("type", PStr "text")]])]
    map (get "error") (byId 12) `shouldBe` [Just (PDict [("code", PInt (-32601)), ("message", PStr "Method not found"), ("data", PStr "bogus/method")])]
    map text (byId 13)
      `shouldBe` [Just (PList [PDict [("text", PStr "Error executing tool lean_goal: 1 validation error for goalArguments\nfile_path\n  Field required [type=missing, input_value={'line': 1}, input_type=dict]\n    For further information visit https://errors.pydantic.dev/2.13/v/missing"), ("type", PStr "text")]])]
    map text (byId 14) `shouldBe` [Just (PList [PDict [("text", PStr "Error executing tool lean_run_code: No valid Lean project path found. Run another tool first to set it up."), ("type", PStr "text")]])]
    map result (byId 15) `shouldBe` [PDict [("prompts", PList [])]]
    map result (byId 16) `shouldBe` [PDict [("resources", PList [])]]
