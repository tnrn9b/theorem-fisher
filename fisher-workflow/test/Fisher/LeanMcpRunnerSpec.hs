-- | Which Lean MCP server a managed workflow launches: Fisher's own by
-- default, upstream's through uv when asked.
module Fisher.LeanMcpRunnerSpec (spec) where

import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, emptyPermissions, setOwnerExecutable, setOwnerReadable, setPermissions)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Workflow.Settings (resolveUvRunner)

spec :: Spec
spec = describe "the managed Lean MCP server" $ do
  it "is fisher lean-mcp by default, with no uv needed" $ do
    self <- getExecutablePath
    resolveUvRunner [("PATH", "/nonexistent")] `shouldReturn` Right [T.pack self, "lean-mcp"]
  it "is uvx --from SPEC lean-lsp-mcp with FISHER_LEAN_MCP=uvx" $
    withSystemTempDirectory "runner" $ \tmp -> do
      let bin = tmp </> "bin"
      createDirectoryIfMissing True bin
      writeFile (bin </> "uvx") "#!/bin/sh\n"
      setPermissions (bin </> "uvx") (setOwnerExecutable True (setOwnerReadable True emptyPermissions))
      resolveUvRunner [("PATH", bin), ("FISHER_LEAN_MCP", "uvx")]
        `shouldReturn` Right [T.pack (bin </> "uvx"), "--from", "lean-lsp-mcp", "lean-lsp-mcp"]
  it "says uv is missing when upstream's server is asked for without it" $
    resolveUvRunner [("PATH", "/nonexistent"), ("FISHER_LEAN_MCP", "uvx")]
      `shouldReturn` Left "Neither `uvx` nor `uv` is available. Install uv so Fisher can run the managed Lean MCP server."
