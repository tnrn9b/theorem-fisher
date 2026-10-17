-- | Rocq and Isabelle projects on the claude-code and codex backends: the
-- command resolution, the launch checks, the staged skills and the MCP
-- server entries. Goldens under @test/golden/provers@; set
-- @FISHER_GOLDEN_UPDATE=1@ to rewrite them.
module Fisher.ProversSpec (spec) where

import Control.Monad (forM, forM_, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode (..))
import System.Posix.Files (setFileMode)
import System.Process.Typed (proc, readProcess, setWorkingDir)
import Test.Hspec

import Fisher.A2Fixture
import Fisher.Project (Project (..), defaultInitOptions, initName, initializeProject)
import Fisher.Prover (Prover (..), ProverKind (..), ServerSpec (..), prover)
import Fisher.Workflow.Command
import Fisher.Workflow.Handoff
import Fisher.Workflow.Plan
import Fisher.Workflow.Provers
import Fisher.Workflow.Types

goldenDir :: FilePath
goldenDir = "test/golden/provers"

-- | Compare with the golden file, or write it under FISHER_GOLDEN_UPDATE.
golden :: FilePath -> Value -> Expectation
golden name v = do
  let path = goldenDir </> name
  update <- maybe False (not . null) <$> lookupEnv "FISHER_GOLDEN_UPDATE"
  if update
    then do
      createDirectoryIfMissing True goldenDir
      BL.writeFile path (P.encodePretty' P.defConfig {P.confIndent = P.Spaces 2, P.confCompare = compare} v <> "\n")
    else do
      present <- doesFileExist path
      when (not present) $ expectationFailure ("missing golden " <> path <> "; run with FISHER_GOLDEN_UPDATE=1")
      expected <- either fail pure =<< (Data.Aeson.eitherDecodeFileStrict path :: IO (Either String Value))
      v `shouldBe` expected

spec' :: Text -> Either WorkflowError WorkflowSpec
spec' c = parseWorkflowCommand c

parsed :: Text -> IO WorkflowSpec
parsed = ok . spec'

backendOf :: Either WorkflowError WorkflowSpec -> Either WorkflowError Text
backendOf = fmap wsBackend

-- | Every file under a directory, relative to it, sorted.
filesUnder :: FilePath -> IO [FilePath]
filesUnder root = sort <$> go root
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else pure [makeRelative root p]

readJson :: FilePath -> IO Value
readJson p = either fail pure =<< (Data.Aeson.eitherDecodeFileStrict p :: IO (Either String Value))

writeExe :: FilePath -> Text -> IO ()
writeExe p body = TIO.writeFile p body >> setFileMode p 0o755

-- | A claude that logs its calls and lists the prover plugin it was told of.
fakeClaude :: Text
fakeClaude = T.unlines
  [ "#!/bin/bash"
  , "export PATH=\"/usr/bin:/bin:$PATH\""
  , "printf '%s' \"$(basename \"$0\")\" >> \"$FAKE_LOG\""
  , "for a in \"$@\"; do printf ' [%s]' \"$a\" >> \"$FAKE_LOG\"; done"
  , "printf '\\n' >> \"$FAKE_LOG\""
  , "if [ \"$1 $2\" = \"plugin install\" ]; then"
  , "  python3 -I - \"$HOME/.claude/settings.json\" \"$FAKE_PLUGIN_ID\" <<'PY'"
  , "import json, os, sys"
  , "path, pid = sys.argv[1], sys.argv[2]"
  , "os.makedirs(os.path.dirname(path), exist_ok=True)"
  , "d = json.load(open(path)) if os.path.exists(path) else {}"
  , "d.setdefault('enabledPlugins', {})[pid] = True"
  , "d.setdefault('extraKnownMarketplaces', {})[pid.split('@')[1]] = {'source': {'source': 'directory'}}"
  , "json.dump(d, open(path, 'w'))"
  , "PY"
  , "fi"
  , "if [ \"$1 $2\" = \"plugin list\" ]; then"
  , "  printf '[{\"id\": \"%s\", \"installPath\": \"%s\"}]\\n' \"$FAKE_PLUGIN_ID\" \"$FAKE_PLUGIN_DIR\""
  , "fi"
  , "exit 0" ]

-- | coqc printing FAKE_COQ_VERSION, or Rocq 9.0's banner.
fakeCoqc :: Text
fakeCoqc = T.unlines
  [ "#!/bin/bash"
  , "if [ -n \"$FAKE_COQ_VERSION\" ]; then echo \"$FAKE_COQ_VERSION\"; else"
  , "  echo 'The Rocq Prover, version 9.0.0'; echo 'compiled with OCaml 4.14.1'; fi" ]

-- | A project of the kind under @root/<name>@, initialized; returns its root.
makeProject :: FilePath -> ProverKind -> IO FilePath
makeProject root kind = do
  let (dir, marker, body) = case kind of
        Rocq -> ("rocqproj", "_RocqProject", "-Q theories Demo\ntheories/Main.v\n")
        Isabelle -> ("isaproj", "ROOT", "session Demo = HOL +\n  theories Main\n")
        Lean4 -> ("leanproj", "lakefile.lean", "-- lean\n")
      p = root </> dir
  createDirectoryIfMissing True p
  writeFile (p </> marker) body
  _ <- initializeProject p defaultInitOptions {initName = Just "Prover Demo"}
  pure p

data Fixture = Fixture
  { fxBackend :: Text
  , fxKind :: ProverKind
  , fxBins :: [Text]
  , fxEnv :: [(String, String)]
  , fxProjectDir :: Maybe (FilePath -> IO FilePath)
  -- ^ build the project in another way (the --prover case)
  , fxSubdir :: FilePath
  -- ^ where in the project the launch starts
  , fxConfig :: [(Data.Aeson.Key.Key, Value)]
  -- ^ more top-level config keys
  , fxAuto :: [(Data.Aeson.Key.Key, Value)]
  -- ^ more @gauss.autoformalize@ keys
  }

fixture :: Text -> ProverKind -> Fixture
fixture b k =
  Fixture b k (["claude", "codex", "uvx"] <> (if k == Rocq then ["coqc"] else ["isabelle"])) [] Nothing "." [] []

-- | The native backend on OpenRouter, the key in the environment.
native :: ProverKind -> Fixture
native k = (fixture "fisher" k) {fxEnv = [("OPENROUTER_API_KEY", "sk-or-test")]}

atPath :: [Text] -> Value -> Maybe Value
atPath [] v = Just v
atPath (k : ks) (Object o) = KM.lookup (Data.Aeson.Key.fromText k) o >>= atPath ks
atPath _ _ = Nothing

toJSONList :: [Text] -> Value
toJSONList = Data.Aeson.toJSON

-- | Plan a command in a fresh project of the fixture's kind. No git, rg or
-- lean4 checkout is there: a Rocq or Isabelle launch must not need them.
plan :: Fixture -> Text -> (FilePath -> Env -> Either WorkflowError WorkflowPlan -> IO ()) -> IO ()
plan fx command k = do
  file <- readJson "test/golden/a2/workflow-plans.json"
  withSystemTempDirectory "fisher-provers" $ \d -> do
    root <- canonicalizePath d
    base0 <- setupCase root (object ["bins" .= filter (`notElem` ["coqc", "rocq"]) (fxBins fx), "checkout" .= ("none" :: Text)])
    when ("claude" `elem` fxBins fx) $ writeExe (root </> "bin" </> "claude") fakeClaude
    forM_ ["coqc", "rocq"] $ \b -> when (b `elem` fxBins fx) $ writeExe (root </> "bin" </> T.unpack b) fakeCoqc
    proj <- maybe (makeProject root (fxKind fx)) ($ root) (fxProjectDir fx)
    let name = if fxKind fx == Rocq then "rocq" else "isabelle"
        pluginDir = root </> "gauss-home" </> "autoformalize" </> "assets" </> "fisher-provers" </> name
        base =
          foldr (uncurry Map.insert) (Map.delete "OPENROUTER_API_KEY" base0) $
            [ ("FAKE_PLUGIN_ID", name <> "@fisher-provers"), ("FAKE_PLUGIN_DIR", pluginDir)
            , ("ANTHROPIC_API_KEY", "sk-ant-test"), ("OPENAI_API_KEY", "sk-test") ]
              <> fxEnv fx
        cfg = object (("gauss" .= object ["autoformalize" .= object (("backend" .= fxBackend fx) : fxAuto fx)]) : [k .= v | (k, v) <- fxConfig fx])
        -- the launch's notes and warnings go to notes.log
        machine = (machineFor file (root </> "gauss-home")) {mNote = \t -> TIO.appendFile (root </> "notes.log") (t <> "\n")}
        li = LaunchInputs machine cfg base False (const (pure Nothing)) (Just (proj </> fxSubdir fx))
    createDirectoryIfMissing True (proj </> fxSubdir fx)
    r <- withProcessEnv base (resolveWorkflowRequest li command)
    k root base r

-- | The tools Fisher offers from the prover's server.
serverTools :: ProverKind -> [Text]
serverTools k = svTools (pvServer (prover k))

-- | The staged directory holding only the @fisher@ link, for a backend.
fisherBinOf :: FilePath -> Text -> FilePath
fisherBinOf root b = root </> "gauss-home" </> "autoformalize" </> T.unpack b </> "managed" </> "fisher-bin"

-- | This test executable's path as @<EXE>@ in every string.
normSelf :: FilePath -> Value -> Value
normSelf self = \case
  String t -> String (T.replace (T.pack self) "<EXE>" t)
  Array a -> Array (fmap (normSelf self) a)
  Object o -> Object (fmap (normSelf self) o)
  v -> v

-- | The keys of a JSON object's @mcpServers@ (or of any key path), sorted.
keysAt :: [Text] -> Value -> [Text]
keysAt path v = case (path, v) of
  ([], Object o) -> sort (map Data.Aeson.Key.toText (KM.keys o))
  (k : ks, Object o) | Just x <- KM.lookup (Data.Aeson.Key.fromText k) o -> keysAt ks x
  _ -> []

mcpServerKeys :: Value -> [Text]
mcpServerKeys = keysAt ["mcpServers"]

errorText :: Either WorkflowError a -> Text
errorText = either errorMessage (const "")

ok :: Either WorkflowError a -> IO a
ok = either (\e -> fail (T.unpack (errorMessage e))) pure

-- | Imports isabelle-mcp's FileWatcher, with the shim or without, forces
-- the platform to macOS and starts the watcher: @started True@, or the
-- name of the error. The platform's observer is imported first, as it
-- would be on the Mac.
darwinProbe :: Text
darwinProbe = T.unlines
  [ "import importlib.util, sys"
  , "import watchdog.observers"
  , "from isabelle_mcp.file_watcher import FileWatcher"
  , "if sys.argv[1] == 'shim':"
  , "    spec = importlib.util.spec_from_file_location('fisher_isabelle_shim', sys.argv[2])"
  , "    spec.loader.exec_module(importlib.util.module_from_spec(spec))"
  , "sys.platform = 'darwin'"
  , "w = FileWatcher()"
  , "try:"
  , "    w.start()"
  , "    print('started', w._inotify_enabled)"
  , "except UnboundLocalError as e:"
  , "    print('UnboundLocalError', 'avail' if 'avail' in str(e) else e)"
  , "finally:"
  , "    w.stop()" ]

spec :: Spec
spec = do
  describe "resolveCommandFor" $ do
    it "leaves Lean commands as they are" $
      forM_ ["/prove A.lean", "/draft x", "/autoprove", "/golf B.lean"] $ \c ->
        (spec' c >>= resolveCommandFor Lean4) `shouldBe` spec' c

    it "sends /prove and /draft to the prover's skill" $ do
      backendOf (spec' "/prove theories/Main.v:foo" >>= resolveCommandFor Rocq) `shouldBe` Right "/rocq:prove theories/Main.v:foo"
      backendOf (spec' "/draft the sum of two evens" >>= resolveCommandFor Rocq) `shouldBe` Right "/rocq:draft the sum of two evens"
      backendOf (spec' "/prove" >>= resolveCommandFor Isabelle) `shouldBe` Right "/isabelle:prove"
      backendOf (spec' "/draft Main.thy" >>= resolveCommandFor Isabelle) `shouldBe` Right "/isabelle:draft Main.thy"

    it "keeps every other workflow for Lean projects" $ do
      errorText (spec' "/autoprove" >>= resolveCommandFor Rocq) `shouldBe` "/autoprove is available for Lean projects only."
      errorText (spec' "/auto-proof x" >>= resolveCommandFor Isabelle) `shouldBe` "/autoprove is available for Lean projects only."
      forM_ ["/review", "/checkpoint", "/refactor", "/golf", "/formalize", "/autoformalize"] $ \c ->
        errorText (spec' c >>= resolveCommandFor Rocq) `shouldBe` (c <> " is available for Lean projects only.")

  describe "the --prover flag" $ do
    it "is taken out of the arguments" $ do
      ws <- parsed "/prove --prover rocq theories/Main.v"
      fmap (\(k, w) -> (k, wsArgs w, wsBackend w)) (stripProverFlag ws)
        `shouldBe` Right (Just Rocq, "theories/Main.v", "/lean4:prove theories/Main.v")
      ws2 <- parsed "/draft topic --prover=isabelle"
      fmap (\(k, w) -> (k, wsArgs w)) (stripProverFlag ws2) `shouldBe` Right (Just Isabelle, "topic")
      ws3 <- parsed "/prove A.lean"
      stripProverFlag ws3 `shouldBe` Right (Nothing, ws3)

    it "wants a known prover" $ do
      ws <- parsed "/prove --prover agda"
      errorText (stripProverFlag ws) `shouldBe` "--prover takes lean4, rocq or isabelle."

  describe "the server entries" $ do
    it "launch Rocq-MCP and Isabelle-MCP with the project root, fisher-mcp's keys and the tool list" $ do
      let shim = Just "/work/state/isabelle-mcp/isabelle_mcp_shim.py"
          entryWith s k env = either (String . ("error: " <>)) id (proverServerPayload k env s "/work/proj" Nothing)
          entry = entryWith shim
      golden "server-payloads.json" $
        object
          [ "rocq" .= entry Rocq []
          , "isabelle" .= entry Isabelle []
          , "isabelle-no-shim" .= entryWith Nothing Isabelle []
          , "rocq-override" .= entry Rocq [("GAUSS_AUTOFORMALIZE_ROCQ_MCP_SPEC", "rocq-mcp==0.3")]
          , "isabelle-override" .= entry Isabelle [("GAUSS_AUTOFORMALIZE_ISABELLE_MCP_SPEC", "git+https://example.org/isabelle-mcp")]
          , "blank-override" .= entry Rocq [("GAUSS_AUTOFORMALIZE_ROCQ_MCP_SPEC", "  ")]
          , "bad-override" .= entry Rocq [("GAUSS_AUTOFORMALIZE_ROCQ_MCP_SPEC", "uvx rocq-mcp")] ]

    it "start Isabelle-MCP through the shim: uvx --from isabelle-mcp==0.6.0 python <shim>" $ do
      proverServerCommand Isabelle [] (Just "/s/isabelle_mcp_shim.py")
        `shouldBe` Right ["uvx", "--from", "isabelle-mcp==0.6.0", "python", "/s/isabelle_mcp_shim.py"]
      -- the shim is for Isabelle only
      proverServerCommand Rocq [] (Just "/s/isabelle_mcp_shim.py")
        `shouldBe` Right ["uvx", "--from", "git+https://github.com/LLM4Rocq/rocq-mcp@6983113d0844c0b7f987c79dab13988445109bfb", "rocq-mcp"]

    it "stage the shim alone in its directory, and leave an up-to-date one as it is" $
      withSystemTempDirectory "fisher-shim" $ \d -> do
        p <- stageIsabelleShim (d </> "isabelle-mcp")
        p `shouldBe` d </> "isabelle-mcp" </> "isabelle_mcp_shim.py"
        BS.readFile p `shouldReturn` isabelleShim
        listDirectory (d </> "isabelle-mcp") `shouldReturn` ["isabelle_mcp_shim.py"]
        t0 <- getModificationTime p
        _ <- stageIsabelleShim (d </> "isabelle-mcp")
        getModificationTime p `shouldReturn` t0

    it "the shim lets FileWatcher.start return on macOS (live, FISHER_ISABELLE_MCP_PYTHON: a Python with isabelle-mcp 0.6.0)" $ do
      py <- lookupEnv "FISHER_ISABELLE_MCP_PYTHON"
      case py of
        Nothing -> pendingWith "FISHER_ISABELLE_MCP_PYTHON is not set"
        Just python -> withSystemTempDirectory "fisher-shim" $ \d -> do
          shim <- stageIsabelleShim d
          let probe = d </> "probe.py"
          TIO.writeFile probe darwinProbe
          let runProbe which = readProcess (proc python ["-I", probe, which, shim])
          -- without the shim: the error of the Mac
          (c0, out0, _) <- runProbe "bare"
          (c0, out0) `shouldBe` (ExitSuccess, "UnboundLocalError avail\n")
          (c1, out1, err1) <- runProbe "shim"
          (c1, out1, err1) `shouldBe` (ExitSuccess, "started True\n", "")
          -- and the entry point still answers --help, for the prewarm
          (c2, out2, _) <- readProcess (proc python [shim, "--help"])
          c2 `shouldBe` ExitSuccess
          BL8.unpack out2 `shouldStartWith` "usage: isabelle-mcp"

    it "write the MCP config under the server's name" $
      withSystemTempDirectory "fisher-provers" $ \d -> do
        forM_ [Rocq, Isabelle] $ \k ->
          writeMcpConfigFor k (d </> show k <> ".json") (clientServerPayload k (either (const []) id (proverServerCommand k [] (Just "/work/state/isabelle-mcp/isabelle_mcp_shim.py"))) "/work/proj" Nothing)
        r <- readJson (d </> "Rocq.json")
        i <- readJson (d </> "Isabelle.json")
        golden "mcp-config.json" (object ["rocq" .= r, "isabelle" .= i])

  describe "the staged skills" $ do
    it "form the fisher-provers marketplace with the rocq and isabelle plugins" $
      withSystemTempDirectory "fisher-provers" $ \d -> do
        market <- stageProverSkills Rocq d
        market `shouldBe` d </> "fisher-provers"
        files <- filesUnder market
        m <- readJson (market </> ".claude-plugin" </> "marketplace.json")
        pr <- readJson (market </> "rocq" </> ".claude-plugin" </> "plugin.json")
        pi' <- readJson (market </> "isabelle" </> ".claude-plugin" </> "plugin.json")
        golden "marketplace.json" (object ["files" .= files, "marketplace.json" .= m, "rocq/plugin.json" .= pr, "isabelle/plugin.json" .= pi'])
        -- staging again leaves an identical tree
        again <- stageProverSkills Isabelle d >>= filesUnder
        again `shouldBe` files

    it "name only tools the server offers, in fisher-mcp's spelling, and forbid the sharp ones" $
      withSystemTempDirectory "fisher-provers" $ \d -> do
        market <- stageProverSkills Rocq d
        rocq <- mapM (TIO.readFile . (market </>)) ["rocq/skills/rocq/SKILL.md", "rocq/commands/prove.md", "rocq/commands/draft.md"]
        isa <- mapM (TIO.readFile . (market </>)) ["isabelle/skills/isabelle/SKILL.md", "isabelle/commands/prove.md", "isabelle/commands/draft.md"]
        let all' = rocq <> isa
        forM_ all' $ \t -> do
          T.any (`elem` ['\x2013', '\x2014']) t `shouldBe` False
          forM_ ["Claude", "Codex", "OpenAI", "Anthropic", "GPT"] $ \w -> T.isInfixOf w t `shouldBe` False
        T.isInfixOf "rocq_switch" (head rocq) `shouldBe` True
        T.isInfixOf "mcp_rocq_rocq_compile_file" (head rocq) `shouldBe` True
        T.isInfixOf "mcp_isabelle_isabelle_goal" (head isa) `shouldBe` True
        T.isInfixOf "debugger" (head isa) `shouldBe` True
        forM_ [rocq !! 1, isa !! 1] $ \t -> T.isInfixOf "fisher check" t `shouldBe` True

    it "make the compile-first loop required steps, with the tools as Fisher names them" $
      withSystemTempDirectory "fisher-provers" $ \d -> do
        market <- stageProverSkills Rocq d
        let commands = [(Rocq, "rocq/commands/prove.md"), (Rocq, "rocq/commands/draft.md"), (Isabelle, "isabelle/commands/prove.md"), (Isabelle, "isabelle/commands/draft.md")]
        forM_ commands $ \(k, f) -> do
          t <- TIO.readFile (market </> f)
          let server = if k == Rocq then "rocq" else "isabelle"
              named = [T.takeWhile (\c -> c /= '`') w | w <- drop 1 (T.splitOn ("`mcp_" <> server <> "_") t)]
          T.isInfixOf "Every step below is required, in this order." t `shouldBe` True
          T.isInfixOf "without checking it through the server is not\ndone" t `shouldBe` True
          -- every tool named is one the server offers, in Fisher's spelling
          named `shouldSatisfy` (not . null)
          forM_ named $ \n -> (f, n `elem` serverTools k) `shouldBe` (f, True)
        prove <- mapM (TIO.readFile . (market </>)) ["rocq/commands/prove.md", "isabelle/commands/prove.md"]
        forM_ prove $ \t -> T.isInfixOf "Run `fisher check` once, in the terminal" t `shouldBe` True
        forM_ ["mcp_rocq_rocq_compile_file", "mcp_rocq_rocq_health"] $ \n -> T.isInfixOf n (head prove) `shouldBe` True
        forM_ ["mcp_isabelle_isabelle_evaluate_to", "mcp_isabelle_isabelle_command_output"] $ \n -> T.isInfixOf n (prove !! 1) `shouldBe` True

    it "go under .agents/skills/<prover> for codex" $
      withSystemTempDirectory "fisher-provers" $ \d -> do
        forM_ [Rocq, Isabelle] $ \k -> stageCodexProverSkill k (d </> show k)
        r <- filesUnder (d </> "Rocq")
        i <- filesUnder (d </> "Isabelle")
        golden "codex-skills.json" (object ["rocq" .= r, "isabelle" .= i])

  describe "launch checks" $ do
    it "stop when isabelle is missing" $
      plan (fixture "claude-code" Isabelle) {fxBins = ["claude", "uvx"]} "/prove" $ \_ _ r ->
        errorText r `shouldBe` "isabelle not found: install Isabelle2025-2 from isabelle.in.tum.de"

    it "stop when neither rocq nor coqc is there" $
      plan (fixture "claude-code" Rocq) {fxBins = ["claude", "uvx"]} "/prove" $ \_ _ r ->
        errorText r `shouldBe` "coqc or rocq not found: opam install rocq-prover coq-lsp"

    it "stop when neither uvx nor uv is there" $
      plan (fixture "claude-code" Rocq) {fxBins = ["claude", "coqc"]} "/prove" $ \_ _ r ->
        errorText r `shouldBe` "uvx not found: install uv"

    it "stop on Coq 8.16" $
      plan (fixture "claude-code" Rocq) {fxEnv = [("FAKE_COQ_VERSION", "The Coq Proof Assistant, version 8.16.1")]} "/prove" $ \_ _ r ->
        errorText r `shouldBe` "Coq 8.16.1 is older than 8.17, which Rocq-MCP does not support."

    it "let Coq 8.18 through" $
      plan (fixture "claude-code" Rocq) {fxEnv = [("FAKE_COQ_VERSION", "The Coq Proof Assistant, version 8.18.0")]} "/prove" $ \_ _ r ->
        errorText r `shouldBe` ""

    it "refuse the other workflows before anything is staged" $
      plan (fixture "claude-code" Rocq) "/autoprove" $ \_ _ r ->
        errorText r `shouldBe` "/autoprove is available for Lean projects only."

    it "stop the native backend without an OpenRouter key" $
      plan (fixture "fisher" Rocq) "/prove" $ \_ _ r ->
        errorText r `shouldBe` "OpenRouter auth not found. Save `OPENROUTER_API_KEY` in ~/.gauss/.env, or set `gauss.autoformalize.native_provider: anthropic`."

  describe "plans" $ do
    forM_ [Rocq, Isabelle] $ \kind -> do
      let name = if kind == Rocq then "rocq" else "isabelle" :: String
      it ("stage the claude-code session for a " <> show kind <> " project") $
        plan (fixture "claude-code" kind) "/prove Main" $ \root base r -> do
          p <- ok r
          let c = wpContext p
              req = wpRequest p
              market = root </> "gauss-home" </> "autoformalize" </> "assets" </> "fisher-provers"
          files <- filesUnder market
          mcp <- readJson (mcMcpConfig c)
          cfg <- readJson (mcBackendHome c </> ".claude.json")
          logText <- TIO.readFile (root </> "fake.log")
          let servers = case cfg of
                Object o -> KM.lookup "mcpServers" o
                _ -> Nothing
              keys = ["HOME", "CLAUDE_PLUGIN_ROOT", "GAUSS_AUTOFORMALIZE_BACKEND", "GAUSS_PROJECT_ROOT", "ROCQ_PROJECT_PATH", "ROCQ_WORKSPACE", "ISABELLE_PROJECT_PATH", "LEAN_PROJECT_PATH", "GAUSS_AUTOFORMALIZE_SKILLS_ROOT"]
              envView = object [ (Data.Aeson.Key.fromString k, maybe Null (String . normText root . T.pack) (Map.lookup k (hrEnv req))) | k <- keys ]
          startup <- maybe (pure "") TIO.readFile (mcStartupContext c)
          self <- getExecutablePath
          golden ("claude-" <> name <> ".json") . normSelf self . normalize root $
            object
              [ "argv" .= hrArgv req
              , "cwd" .= hrCwd req
              , "backend_command" .= wsBackend (wpSpec p)
              , "staged_paths" .= object [(Data.Aeson.Key.fromText k, String v) | (k, v) <- stagedPaths p]
              , "marketplace_files" .= files
              , "mcp_config" .= mcp
              , "claude_json_mcp_servers" .= servers
              , "env" .= envView
              , "claude_calls" .= filter ("claude" `T.isPrefixOf`) (T.lines logText)
              , "startup_context" .= T.lines startup ]
          Map.member "LEAN4_PLUGIN_ROOT" (hrEnv req) `shouldBe` Map.member "LEAN4_PLUGIN_ROOT" base

      it ("stage the codex session for a " <> show kind <> " project") $
        plan (fixture "codex" kind) "/draft two evens sum to an even" $ \root _ r -> do
          p <- ok r
          let c = wpContext p
              req = wpRequest p
          skills <- maybe (pure []) filesUnder (mcSkillsRoot c)
          toml <- TIO.readFile (mcMcpConfig c)
          instructions <- maybe (pure "") TIO.readFile (mcInstructions c)
          self <- getExecutablePath
          golden ("codex-" <> name <> ".json") . normSelf self . normalize root $
            object
              [ "argv" .= hrArgv req
              , "backend_command" .= wsBackend (wpSpec p)
              , "staged_paths" .= object [(Data.Aeson.Key.fromText k, String v) | (k, v) <- stagedPaths p]
              , "skills" .= skills
              , "config_toml" .= T.lines toml
              , "instructions" .= T.lines instructions ]

      it ("stage the native session for a " <> show kind <> " project") $
        plan (native kind) "/prove Main" $ \root _ r -> do
          p <- ok r
          self <- T.pack <$> getExecutablePath
          let c = wpContext p
              req = wpRequest p
              keys = [ "GAUSS_AUTOFORMALIZE_BACKEND", "GAUSS_AUTOFORMALIZE_CONTEXT", "GAUSS_AUTOFORMALIZE_INSTRUCTIONS", "GAUSS_PROJECT_ROOT"
                     , "ROCQ_PROJECT_PATH", "ROCQ_WORKSPACE", "ISABELLE_PROJECT_PATH", "LEAN_PROJECT_PATH", "GAUSS_YOLO_MODE", "GAUSS_HOME" ]
              envView = object [ (Data.Aeson.Key.fromString k, maybe Null (String . normText root . T.pack) (Map.lookup k (hrEnv req))) | k <- keys ]
          mcp <- readJson (mcMcpConfig c)
          startup <- maybe (pure "") TIO.readFile (mcStartupContext c)
          system <- maybe (pure "") TIO.readFile (mcInstructions c)
          golden ("fisher-" <> name <> ".json") . normSelf (T.unpack self) . normalize root $
            object
              [ "argv" .= map (\a -> if a == self then "<EXE>" else a) (hrArgv req)
              , "cwd" .= hrCwd req
              , "backend_command" .= wsBackend (wpSpec p)
              , "staged_paths" .= object [(Data.Aeson.Key.fromText k, String v) | (k, v) <- stagedPaths p]
              , "mcp_config" .= mcp
              , "env" .= envView
              , "startup_context" .= T.lines startup
              , "system_prompt" .= T.lines system ]
          -- the skill and the command are inline, front matter dropped
          T.isInfixOf ("## The `/" <> T.pack name <> ":prove` command") system `shouldBe` True
          T.isInfixOf "\nname: " system `shouldBe` False
          T.any (`elem` ['\x2013', '\x2014']) system `shouldBe` False
          forM_ ["Claude", "Codex", "OpenAI", "Anthropic", "GPT"] $ \w -> T.isInfixOf w system `shouldBe` False

    describe "the model" $ do
      let valueAfter f argv = case dropWhile (/= f) argv of
            (_ : v : _) -> Just v
            _ -> Nothing
          configured fx = fx {fxAuto = [("model", String "from-config")]}
          argvOf r = hrArgv . wpRequest <$> ok r
          cases =
            [ ("claude-code", "--model", Just claudeModel)
            , ("codex", "-m", Nothing)
            , ("fisher", "-m", Just nativeOpenRouterModel) ]
          fx0 b = if b == "fisher" then native Rocq else fixture b Rocq
      mapM_ (\(b, flag, def) -> do
        it ("is the default on " <> T.unpack b) $ plan (fx0 b) "/prove Main" $ \_ _ r -> do
          argv <- argvOf r
          valueAfter flag argv `shouldBe` def
        it ("is the config's on " <> T.unpack b) $ plan (configured (fx0 b)) "/prove Main" $ \_ _ r -> do
          argv <- argvOf r
          valueAfter flag argv `shouldBe` Just "from-config"
        it ("is the flag's, over the config, on " <> T.unpack b) $ plan (configured (fx0 b)) "/prove --model from-flag Main" $ \_ _ r -> do
          p <- ok r
          valueAfter flag (hrArgv (wpRequest p)) `shouldBe` Just "from-flag"
          wsBackend (wpSpec p) `shouldBe` "/rocq:prove Main"
        -- the whole planned argv with a chosen model, and its headless form
        it ("is in the plan's golden, and the headless one's, on " <> T.unpack b) $ plan (fx0 b) "/prove --model from-flag Main" $ \root _ r -> do
          p <- ok r
          self <- getExecutablePath
          let argv = hrArgv (wpRequest p)
          printed <- either (fail . T.unpack) pure (printArgv (mcBackend (wpContext p)) argv)
          golden ("model-" <> T.unpack b <> "-rocq.json") . normSelf self . normalize root $
            object ["argv" .= argv, "print_argv" .= printed, "backend_command" .= wsBackend (wpSpec p)]) cases

    it "offer the five coqc tools on the native backend without pet, and all twelve with it" $ do
      let includeOf p = do
            v <- readJson (mcMcpConfig (wpContext p))
            pure (atPath ["mcpServers", "rocq", "tools", "include"] v)
      plan (native Rocq) "/prove" $ \_ _ r -> do
        p <- ok r
        includeOf p `shouldReturn` Just (toJSONList ["rocq_compile", "rocq_compile_file", "rocq_verify", "rocq_diag", "rocq_health"])
      plan (native Rocq) {fxBins = fxBins (native Rocq) <> ["pet"]} "/prove" $ \_ _ r -> do
        p <- ok r
        Just (Array a) <- includeOf p
        length a `shouldBe` 12

    -- fisher ui starts runs as fisher launch --print with these two set;
    -- the planned fisher chat child must see them (docs/superpowers/plans/2026-10-16-fisher-ui.md task 5)
    it "pass FISHER_UI_LEDGER and FISHER_UI_TAG through to the native child" $
      forM_ [Rocq, Isabelle] $ \k ->
        plan (native k) {fxEnv = fxEnv (native k) <> [("FISHER_UI_LEDGER", "1"), ("FISHER_UI_TAG", "run-7")]} "/prove" $ \_ _ r -> do
          p <- ok r
          let env = hrEnv (wpRequest p)
          (Map.lookup "FISHER_UI_LEDGER" env, Map.lookup "FISHER_UI_TAG" env) `shouldBe` (Just "1", Just "run-7")
          take 2 (drop 1 (hrArgv (wpRequest p))) `shouldBe` ["chat", "--provider"]

    it "take the tool timeout from fisher.provers.tool_timeout" $
      plan (native Isabelle) {fxConfig = [("fisher", object ["provers" .= object ["tool_timeout" .= (45 :: Int)]])]} "/prove" $ \_ _ r -> do
        p <- ok r
        v <- readJson (mcMcpConfig (wpContext p))
        atPath ["mcpServers", "isabelle", "timeout"] v `shouldBe` Just (Number 45)

    it "--prover rocq in a Lean directory selects Rocq" $ do
      let leanWithRocq root = do
            let pdir = root </> "mixed"
            createDirectoryIfMissing True (pdir </> "theories")
            writeFile (pdir </> "lakefile.lean") "-- lean\n"
            _ <- initializeProject pdir defaultInitOptions {initName = Just "Mixed"}
            writeFile (pdir </> "_RocqProject") "-Q theories Demo\n"
            pure pdir
      plan (fixture "claude-code" Rocq) {fxProjectDir = Just leanWithRocq} "/prove --prover rocq theories/Main.v" $ \root _ r -> do
        p <- ok r
        projectProver (wpProject p) `shouldBe` Rocq
        projectProverRoot (wpProject p) `shouldBe` root </> "mixed"
        wsBackend (wpSpec p) `shouldBe` "/rocq:prove theories/Main.v"
        wsArgs (wpSpec p) `shouldBe` "theories/Main.v"
        mcBackend (wpContext p) `shouldBe` "claude-code"
        mcMcpConfig (wpContext p) `shouldBe` root </> "gauss-home" </> "autoformalize" </> "claude-code" </> "managed" </> "mcp" </> "rocq.mcp.json"
        mcpServerKeys <$> readJson (mcMcpConfig (wpContext p)) `shouldReturn` ["rocq"]
        T.isInfixOf "/rocq:prove theories/Main.v" (last (hrArgv (wpRequest p))) `shouldBe` True

  describe "one managed prover per session" $ do
    it "keeps only the current prover's server and plugin in the shared managed home" $ do
      file <- readJson "test/golden/a2/workflow-plans.json"
      withSystemTempDirectory "fisher-provers" $ \d -> do
        root <- canonicalizePath d
        base0 <- setupCase root (object ["bins" .= (["git", "uvx", "rg", "claude"] :: [Text])])
        writeExe (root </> "bin" </> "claude") fakeClaude
        writeExe (root </> "bin" </> "coqc") fakeCoqc
        rocqProj <- makeProject root Rocq
        let home = root </> "home"
            managedHome = root </> "gauss-home" </> "autoformalize" </> "claude-code" </> "managed" </> "claude-home"
            settings = managedHome </> ".claude" </> "settings.json"
        -- a server and a plugin added to the managed home, which every
        -- session keeps (the user's own .claude.json has no servers, so the
        -- managed servers carry over from one session to the next)
        BL.writeFile (home </> ".claude.json") (Data.Aeson.encode (object ["theme" .= ("light" :: Text)]))
        createDirectoryIfMissing True (takeDirectory settings)
        BL.writeFile (managedHome </> ".claude.json") (Data.Aeson.encode (object ["mcpServers" .= object ["mine" .= object ["command" .= ("mine" :: Text)]]]))
        BL.writeFile settings (Data.Aeson.encode (object
          [ "enabledPlugins" .= object ["other@x" .= True]
          , "extraKnownMarketplaces" .= object ["x" .= object []] ]))
        let cfg = object ["gauss" .= object ["autoformalize" .= object ["backend" .= ("claude-code" :: Text)]]]
            run cwd pid pdir = do
              let base = foldr (uncurry Map.insert) base0
                    [ ("FAKE_PLUGIN_ID", pid), ("FAKE_PLUGIN_DIR", pdir), ("ANTHROPIC_API_KEY", "sk-ant-test") ]
                  li = LaunchInputs (machineFor file (root </> "gauss-home")) cfg base False (const (pure Nothing)) (Just cwd)
              r <- withProcessEnv base (resolveWorkflowRequest li "/prove Main")
              _ <- ok r
              cj <- readJson (managedHome </> ".claude.json")
              st <- readJson settings
              pure (mcpServerKeys cj, keysAt ["enabledPlugins"] st, keysAt ["extraKnownMarketplaces"] st)
            lean = run (root </> "project") "lean4@lean4-skills" (root </> "fake-plugin")
            rocq = run rocqProj "rocq@fisher-provers"
                     (root </> "gauss-home" </> "autoformalize" </> "assets" </> "fisher-provers" </> "rocq")
            leanView = (["lean-lsp", "mine"], ["lean4@lean4-skills", "other@x"], ["lean4-skills", "x"])
        lean `shouldReturn` leanView
        rocq `shouldReturn` (["mine", "rocq"], ["other@x", "rocq@fisher-provers"], ["fisher-provers", "x"])
        lean `shouldReturn` leanView

  describe "fisher check in the session" $ do
    forM_ ["claude-code", "codex", "fisher"] $ \b ->
      it ("is this Fisher: a directory holding only a link to it first on the child's PATH, its command in the startup context (" <> T.unpack b <> ")") $ do
        let fx = if b == "fisher" then native Isabelle else fixture b Isabelle
        plan fx "/prove" $ \root base r -> do
          p <- ok r
          self <- getExecutablePath
          let path = Map.findWithDefault "" "PATH" (hrEnv (wpRequest p))
              bin = fisherBinOf root b
          path `shouldBe` bin <> ":" <> Map.findWithDefault "" "PATH" base
          listDirectory bin `shouldReturn` ["fisher"]
          pathIsSymbolicLink (bin </> "fisher") `shouldReturn` True
          getSymbolicLinkTarget (bin </> "fisher") `shouldReturn` self
          ctx <- maybe (pure "") TIO.readFile (mcStartupContext (wpContext p))
          T.lines ctx `shouldContain` ["- Fisher check command: `" <> T.pack self <> " check`"]
          _ <- pure root
          pure ()

  describe "the fisher-only directory" $ do
    it "holds only the fisher link, replaces a stale one and clears anything else" $
      withSystemTempDirectory "fisher-bin" $ \d -> do
        let dir = d </> "fisher-bin"
        createDirectoryIfMissing True dir
        writeFile (dir </> "rocq") "#!/bin/sh\n"
        createFileLink "/old/fisher" (dir </> "fisher")
        -- another launch's link in flight is not touched
        createFileLink "/other/fisher" (dir </> ".fisher.99999")
        stageFisherBin dir "/opt/bin/fisher" `shouldReturn` dir
        sort <$> listDirectory dir `shouldReturn` [".fisher.99999", "fisher"]
        removeFile (dir </> ".fisher.99999")
        listDirectory dir `shouldReturn` ["fisher"]
        getSymbolicLinkTarget (dir </> "fisher") `shouldReturn` "/opt/bin/fisher"
        _ <- stageFisherBin dir "/opt/bin/fisher"
        listDirectory dir `shouldReturn` ["fisher"]

  describe "Isabelle off PATH" $ do
    let withApp root = do
          let bin = root </> "home" </> "Applications" </> "Isabelle2025-2.app" </> "bin"
          createDirectoryIfMissing True bin
          writeExe (bin </> "isabelle") "#!/bin/sh\nexit 0\n"
          makeProject root Isabelle
        appBin root = root </> "home" </> "Applications" </> "Isabelle2025-2.app" </> "bin"
    forM_ ["claude-code", "codex", "fisher"] $ \b ->
      it ("is found in ~/Applications, put on the child's PATH, with one note (" <> T.unpack b <> ")") $ do
        let fx0 = if b == "fisher" then native Isabelle else fixture b Isabelle
            fx = fx0 {fxBins = filter (/= "isabelle") (fxBins fx0), fxProjectDir = Just withApp}
        plan fx "/prove" $ \root base r -> do
          p <- ok r
          self <- getExecutablePath
          _ <- pure self
          Map.lookup "PATH" (hrEnv (wpRequest p))
            `shouldBe` Just (fisherBinOf root b <> ":" <> appBin root <> ":" <> Map.findWithDefault "" "PATH" base)
          notes <- T.lines <$> TIO.readFile (root </> "notes.log")
          notes `shouldBe` ["Isabelle is not on PATH; this session uses " <> T.pack (appBin root </> "isabelle") <> ".", prewarmStartLine Isabelle]
    it "on PATH: no note but the prewarm's line" $
      plan (fixture "codex" Isabelle) "/prove" $ \root _ r -> do
        _ <- ok r
        T.lines <$> TIO.readFile (root </> "notes.log") `shouldReturn` [prewarmStartLine Isabelle]

  describe "the server's start" $ do
    forM_ [(b, k) | b <- ["claude-code", "codex", "fisher"], k <- [Rocq, Isabelle]] $ \(b, k) ->
      it ("is prewarmed once per launch, through uvx with --help (" <> T.unpack b <> ", " <> show k <> ")") $ do
        let fx = if b == "fisher" then native k else fixture b k
        plan fx "/prove" $ \root _ r -> do
          _ <- ok r
          calls <- filter ("uvx " `T.isPrefixOf`) . T.lines <$> TIO.readFile (root </> "fake.log")
          let shim = root </> "gauss-home" </> "autoformalize" </> T.unpack b </> "managed" </> "isabelle-mcp" </> "isabelle_mcp_shim.py"
              server = if k == Rocq then "[--from] [git+https://github.com/LLM4Rocq/rocq-mcp@6983113d0844c0b7f987c79dab13988445109bfb] [rocq-mcp]" else "[--from] [isabelle-mcp==0.6.0] [python] [" <> T.pack shim <> "]"
          calls `shouldBe` ["uvx " <> server <> " [--help]"]
          when (k == Isabelle) $ BS.readFile shim `shouldReturn` isabelleShim

    it "gives Claude Code the connect timeout in milliseconds" $
      plan (fixture "claude-code" Rocq) "/prove" $ \_ _ r -> do
        p <- ok r
        Map.lookup "MCP_TIMEOUT" (hrEnv (wpRequest p)) `shouldBe` Just "300000"

    it "gives Codex the connect timeout as startup_timeout_sec" $
      plan (fixture "codex" Isabelle) "/prove" $ \_ _ r -> do
        p <- ok r
        toml <- T.lines <$> TIO.readFile (mcMcpConfig (wpContext p))
        toml `shouldContain` ["startup_timeout_sec = 300"]

    it "prewarm: closes stdin, adds --help, and a failure is a warning" $
      withSystemTempDirectory "fisher-prewarm" $ \d -> do
        let logf = d </> "argv.log"
            okExe = d </> "uvx"
            badExe = d </> "bad"
            notes = d </> "notes.log"
            note t = TIO.appendFile notes ("note: " <> t <> "\n")
            noted = T.lines <$> TIO.readFile notes <* removeFile notes
        writeExe okExe (T.unlines ["#!/bin/sh", "printf '%s\\n' \"$*\" > " <> T.pack logf, "cat > /dev/null", "exit 0"])
        writeExe badExe (T.unlines ["#!/bin/sh", "echo 'no such package' >&2", "exit 3"])
        prewarmProverServer note Rocq Map.empty [T.pack okExe, "--from", "x", "rocq-mcp"]
        TIO.readFile logf `shouldReturn` "--from x rocq-mcp --help\n"
        noted `shouldReturn` ["note: Starting the Rocq server (first run can take a minute)"]
        prewarmProverServer note Isabelle Map.empty [T.pack badExe, "isabelle-mcp"]
        noted `shouldReturn`
          [ "note: Starting the Isabelle server (first run can take a minute)"
          , "note: Could not prewarm the Isabelle MCP server (exit 3: no such package); the session may wait for it to start." ]
        prewarmProverServer note Rocq Map.empty [T.pack (d </> "missing")]
        missing <- noted
        take 1 missing `shouldBe` ["note: Starting the Rocq server (first run can take a minute)"]
        drop 1 missing `shouldSatisfy` (\case [w] -> "note: Could not prewarm the Rocq MCP server (" `T.isPrefixOf` w; _ -> False)

    it "prewarm: the line comes before the server runs, the warning after" $
      withSystemTempDirectory "fisher-prewarm" $ \d -> do
        let logf = d </> "order.log"
            exe = d </> "uvx"
            note t = TIO.appendFile logf ("note: " <> t <> "\n")
        writeExe exe (T.unlines ["#!/bin/sh", "echo 'server ran' >> " <> T.pack logf, "exit 1"])
        prewarmProverServer note Rocq Map.empty [T.pack exe, "rocq-mcp"]
        T.lines <$> TIO.readFile logf `shouldReturn`
          [ "note: Starting the Rocq server (first run can take a minute)"
          , "server ran"
          , "note: Could not prewarm the Rocq MCP server (exit 1); the session may wait for it to start." ]

  describe "Rocq-MCP's coqc" $ do
    let rocqOnly fx = fx {fxBins = filter (/= "coqc") (fxBins fx) <> ["rocq"]}
        wrapperFor root b = root </> "gauss-home" </> "autoformalize" </> b </> "managed" </> "rocq-bin" </> "coqc"
        checkWrapper root w = do
          TIO.readFile w `shouldReturn` coqcWrapper (root </> "bin" </> "rocq")
          executable <$> getPermissions w `shouldReturn` True
          -- written under a temporary name and renamed: nothing else is left
          listDirectory (takeDirectory w) `shouldReturn` ["coqc"]
    it "is a staged wrapper running rocq compile when only rocq is on PATH (claude-code)" $
      plan (rocqOnly (fixture "claude-code" Rocq)) "/prove" $ \root _ r -> do
        p <- ok r
        let w = wrapperFor root "claude-code"
        checkWrapper root w
        mcp <- readJson (mcMcpConfig (wpContext p))
        atPath ["mcpServers", "rocq", "env", "ROCQ_COQC_BINARY"] mcp `shouldBe` Just (String (T.pack w))
        cj <- readJson (mcBackendHome (wpContext p) </> ".claude.json")
        atPath ["mcpServers", "rocq", "env", "ROCQ_COQC_BINARY"] cj `shouldBe` Just (String (T.pack w))
    it "is a staged wrapper running rocq compile when only rocq is on PATH (codex)" $
      plan (rocqOnly (fixture "codex" Rocq)) "/prove" $ \root _ r -> do
        p <- ok r
        let w = wrapperFor root "codex"
        checkWrapper root w
        toml <- T.lines <$> TIO.readFile (mcMcpConfig (wpContext p))
        toml `shouldContain` ["ROCQ_COQC_BINARY = \"" <> T.pack w <> "\""]
    it "is a staged wrapper running rocq compile when only rocq is on PATH (native)" $
      plan (rocqOnly (native Rocq)) "/prove" $ \root _ r -> do
        p <- ok r
        let w = wrapperFor root "fisher"
        checkWrapper root w
        mcp <- readJson (mcMcpConfig (wpContext p))
        atPath ["mcpServers", "rocq", "env", "ROCQ_COQC_BINARY"] mcp `shouldBe` Just (String (T.pack w))
    it "is the coqc on PATH when there is one: no wrapper, no variable" $
      plan (fixture "claude-code" Rocq) {fxBins = fxBins (fixture "claude-code" Rocq) <> ["rocq"]} "/prove" $ \root _ r -> do
        p <- ok r
        doesPathExist (wrapperFor root "claude-code") `shouldReturn` False
        mcp <- readJson (mcMcpConfig (wpContext p))
        atPath ["mcpServers", "rocq", "env", "ROCQ_COQC_BINARY"] mcp `shouldBe` Nothing
    it "names the opam switch the rocq lives in as OPAM_SWITCH_PREFIX, unless the environment sets one" $
      withSystemTempDirectory "fisher-switch" $ \d0 -> do
        d <- canonicalizePath d0
        let switch = d </> "opam" </> "fisher-rocq"
            plain = d </> "plain"
        forM_ [switch </> "bin", switch </> ".opam-switch", plain </> "bin"] (createDirectoryIfMissing True)
        forM_ [switch, plain] $ \p -> writeExe (p </> "bin" </> "rocq") fakeCoqc
        let envOf p extra = Map.fromList ([("PATH", p </> "bin")] <> extra)
            serverEnv w = proverProjectEnv Rocq "/work/proj" w
        inSwitch <- rocqServerCoqc (envOf switch []) (d </> "w1")
        fmap rcSwitch inSwitch `shouldBe` Just (Just switch)
        serverEnv inSwitch
          `shouldBe` [ ("ROCQ_PROJECT_PATH", "/work/proj"), ("ROCQ_WORKSPACE", "/work/proj")
                     , ("ROCQ_COQC_BINARY", T.pack (d </> "w1" </> "coqc")), ("OPAM_SWITCH_PREFIX", T.pack switch) ]
        -- through a link on PATH, the switch the link points into
        createDirectoryIfMissing True (d </> "links")
        createFileLink (switch </> "bin" </> "rocq") (d </> "links" </> "rocq")
        linked <- rocqServerCoqc (Map.fromList [("PATH", d </> "links")]) (d </> "w4")
        fmap rcSwitch linked `shouldBe` Just (Just switch)
        given <- rocqServerCoqc (envOf switch [("OPAM_SWITCH_PREFIX", "/elsewhere")]) (d </> "w2")
        fmap rcSwitch given `shouldBe` Just Nothing
        lookup "OPAM_SWITCH_PREFIX" (serverEnv given) `shouldBe` Nothing
        outside <- rocqServerCoqc (envOf plain []) (d </> "w3")
        fmap rcSwitch outside `shouldBe` Just Nothing
        lookup "OPAM_SWITCH_PREFIX" (serverEnv outside) `shouldBe` Nothing

    it "the wrapper compiles with the real rocq (pending without rocq on PATH)" $ do
      found <- findExecutable "rocq"
      case found of
        Nothing -> pendingWith "rocq is not on PATH"
        Just real -> withSystemTempDirectory "fisher-coqc" $ \d -> do
          let bin = d </> "bin"
          createDirectoryIfMissing True bin
          createFileLink real (bin </> "rocq")
          w <- rocqServerCoqc (Map.fromList [("PATH", bin)]) (d </> "managed")
          fmap rcWrapper w `shouldBe` Just (d </> "managed" </> "coqc")
          writeFile (d </> "A.v") "Definition x := 1.\nTheorem t : x = 1. Proof. reflexivity. Qed.\n"
          (code, out, err) <- readProcess (setWorkingDir d (proc (d </> "managed" </> "coqc") ["A.v"]))
          (code, out, err) `shouldBe` (ExitSuccess, "", "")
          doesFileExist (d </> "A.vo") `shouldReturn` True

  describe "the prover's working directory" $ do
    it "is the prover root for the claude-code child, so Isabelle-MCP starts there" $
      plan (fixture "claude-code" Isabelle) {fxSubdir = "sub/dir"} "/prove" $ \root _ r -> do
        p <- ok r
        hrCwd (wpRequest p) `shouldBe` root </> "isaproj"
        mcProjectRoot (wpContext p) `shouldBe` root </> "isaproj"

    it "is in the codex server table, with the tool timeout" $
      plan (fixture "codex" Isabelle) {fxSubdir = "sub"} "/prove" $ \root _ r -> do
        p <- ok r
        toml <- T.lines <$> TIO.readFile (mcMcpConfig (wpContext p))
        let table = takeWhile (not . T.isPrefixOf "[mcp_servers.isabelle.env]") (dropWhile (/= "[mcp_servers.isabelle]") toml)
        table `shouldContain` ["cwd = \"" <> T.pack (root </> "isaproj") <> "\""]
        table `shouldContain` ["tool_timeout_sec = 300"]
