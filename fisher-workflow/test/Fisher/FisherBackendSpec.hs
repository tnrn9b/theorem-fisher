-- | Fisher's native backend (C3): `gauss.autoformalize.backend: fisher`
-- (or the old name `gauss`) plans the workflow on `fisher chat`, with the skill and the command doc
-- in a system prompt file, the Lean MCP server in an --mcp-config file and
-- the Claude credentials staged as for claude-code.
module Fisher.FisherBackendSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.A2Fixture
import Fisher.Golden
import Fisher.Workflow.Handoff
import Fisher.Workflow.Plan
import Fisher.Workflow.Settings
import Fisher.Workflow.Types (WorkflowError, errorMessage)

spec :: Spec
spec = describe "the fisher backend" $ do
  it "is accepted under its names, and upstream's list is unchanged" $ do
    map normalizeBackendName ["gauss", "native", "Fisher", "fisher"] `shouldBe` replicate 4 (Right Fisher)
    map backendName supportedBackends `shouldBe` ["claude-code", "codex"]

  it "is named fisher, and the old name gauss still selects it" $ do
    backendName Fisher `shouldBe` "fisher"
    normalizeBackendName "gauss" `shouldBe` Right Fisher
    normalizeBackendName "fisher" `shouldBe` Right Fisher
    -- the name a workflow writes into its context round-trips
    normalizeBackendName (backendName Fisher) `shouldBe` Right Fisher

  it "says Fisher-managed in the startup text and names the context fisher" $
    planProve [("GAUSS_AUTOFORMALIZE_NATIVE_PROVIDER", "anthropic")] $ \_ _ r -> do
      p <- either (fail . show) pure r
      mcBackend (wpContext p) `shouldBe` "fisher"
      let req = wpRequest p
          prompt = last (hrArgv req)
      T.isInfixOf "Fisher-managed Lean workflow session" prompt `shouldBe` True
      any (T.isInfixOf "Gauss") (hrArgv req) `shouldBe` False

  it "plans /prove on OpenRouter by default, with the key from the environment" $
    planProve [("OPENROUTER_API_KEY", "sk-or-test")] $ \root base r -> do
      p <- either (fail . show) pure r
      self <- getExecutablePath
      let argv = hrArgv (wpRequest p)
      take 8 argv `shouldBe` [T.pack self, "chat", "--provider", "openrouter", "-m", nativeOpenRouterModel, "-t", "fisher-lean"]
      Map.lookup "HOME" (hrEnv (wpRequest p)) `shouldBe` Map.lookup "HOME" base
      Map.lookup "GAUSS_HOME" (hrEnv (wpRequest p)) `shouldBe` Just (root </> "gauss-home")

  it "refuses OpenRouter without a key" $
    planProve [] $ \_ _ r ->
      either (T.isInfixOf "OPENROUTER_API_KEY" . errorMessage) (const False) r `shouldBe` True

  it "plans /prove on Anthropic with the skill, the MCP server and the Claude credentials" $
    planProve [("GAUSS_AUTOFORMALIZE_NATIVE_PROVIDER", "anthropic")] $ \root base r -> do
      p <- either (fail . show) pure r
      self <- getExecutablePath
      let req = wpRequest p
          argv = hrArgv req
          managed = root </> "gauss-home" </> "autoformalize" </> "fisher" </> "managed"
      take 7 argv `shouldBe` [T.pack self, "chat", "--provider", "anthropic", "-m", claudeModel, "-t"]
      drop 7 (take 8 argv) `shouldBe` ["fisher-lean"]
      let after k = case dropWhile (/= k) argv of (_ : v : _) -> T.unpack v; _ -> ""
          prompt = last argv
      argv !! (length argv - 2) `shouldBe` "-q"
      T.isInfixOf "/lean4:prove Main.lean" prompt `shouldBe` True
      sysPrompt <- TIO.readFile (after "--system-prompt-file")
      T.isPrefixOf "# Fisher native Lean workflow session" sysPrompt `shouldBe` True
      T.isInfixOf "# lean4 skill" sysPrompt `shouldBe` True
      T.isInfixOf "Invocation: `/lean4:prove Main.lean`" sysPrompt `shouldBe` True
      mcp <- either fail pure =<< (Data.Aeson.eitherDecodeFileStrict (after "--mcp-config") :: IO (Either String Value))
      let server = case mcp of
            Object o | Just (Object s) <- KM.lookup "mcpServers" o -> KM.lookup "lean-lsp" s
            _ -> Nothing
      -- the fixture asks for upstream's server (FISHER_LEAN_MCP=uvx); the
      -- runner is whatever the claude-code backend would launch
      runner <- either (fail . T.unpack) pure =<< resolveUvRunner (Map.toList base)
      fmap (\case Object s -> KM.lookup "args" s; _ -> Nothing) server `shouldBe` Just (Just (Data.Aeson.toJSON (drop 1 runner)))
      Map.lookup "HOME" (hrEnv req) `shouldBe` Just (managed </> "gauss-home")
      Map.lookup "GAUSS_HOME" (hrEnv req) `shouldBe` Just (root </> "gauss-home")
      Map.lookup "GAUSS_AUTOFORMALIZE_BACKEND" (hrEnv req) `shouldBe` Just "fisher"
      doesFileExist (managed </> "gauss-home" </> ".claude" </> ".credentials.json") `shouldReturn` True

  it "rejects an unknown provider" $
    planProve [("GAUSS_AUTOFORMALIZE_NATIVE_PROVIDER", "bogus")] $ \_ _ r ->
      either errorMessage (const "") r `shouldBe` "`gauss.autoformalize.native_provider` must be one of: openrouter, anthropic."

  it "has no chat of its own" $ do
    file <- either fail pure =<< (Data.Aeson.eitherDecodeFileStrict "test/golden/a2/workflow-plans.json" :: IO (Either String Value))
    withSystemTempDirectory "fisher-gauss" $ \d -> do
      root <- canonicalizePath d
      base <- setupCase root (object [])
      let cfg = object ["gauss" .= object ["autoformalize" .= object ["backend" .= ("gauss" :: T.Text)]]]
          li = LaunchInputs (machineFor file (root </> "gauss-home")) cfg base False (const (pure Nothing)) (Just root)
      r <- withProcessEnv base (resolveChatRequest li "hello")
      either (T.isInfixOf "for a chat, talk to Fisher itself" . errorMessage) (const False) r `shouldBe` True

-- | /prove Main.lean planned with the fisher backend in the claude-code
-- prewarmed fixture, with extra environment.
planProve :: [(String, String)] -> (FilePath -> Map.Map String String -> Either WorkflowError WorkflowPlan -> IO ()) -> IO ()
planProve extra k = do
  cases <- goldenCases "test/golden/a2/workflow-plans.json"
  file <- either fail pure =<< (Data.Aeson.eitherDecodeFileStrict "test/golden/a2/workflow-plans.json" :: IO (Either String Value))
  let c = head [x | x <- cases, textField "name" x == "claude-prove-prewarmed"]
  withSystemTempDirectory "fisher-gauss" $ \d -> do
    root <- canonicalizePath d
    base0 <- setupCase root (field "extras" c)
    let base = foldr (uncurry Map.insert) (Map.delete "OPENROUTER_API_KEY" base0) extra
        cfg = object ["gauss" .= object ["autoformalize" .= object ["backend" .= ("fisher" :: T.Text)]]]
        li = LaunchInputs (machineFor file (root </> "gauss-home")) cfg base False (const (pure Nothing)) (Just (root </> "project"))
    r <- withProcessEnv base (resolveWorkflowRequest li "/prove Main.lean")
    k root base r
