-- | The native toolsets @fisher-rocq@ and @fisher-isabelle@ end to end
-- against a fake prover server (test/fixtures/fake-mcp/fake_mcp.py, run
-- with python3, serving the tool schemas recorded from Rocq-MCP's and
-- Isabelle-MCP's own tools/list): the server entry as the workflow writes
-- it, the servers @fisher chat@ adds and checks, fisher-mcp's connection,
-- and the tools the toolset offers; a crash and the reconnect, a hang and
-- the timeout, the server's working directory.
module Fisher.Cli.ProverChatSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTime)
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import Test.Hspec

import Fisher.Cli.ProverChat
import Fisher.Mcp.Client
import Fisher.Project (defaultInitOptions, initName, initializeProject)
import Fisher.Prover (ProverKind (..), Prover (..), ServerSpec (..), petMissingWarning, prover, pvToolsFor, rocqCoqcTools)
import Fisher.Py.Value
import Fisher.Tools.Agent (AgentTools (..), newAgentTools)
import Fisher.Tools.Registry
import Fisher.Workflow.Provers (nativeToolset, serverPayloadFor)

fixtureDir :: FilePath
fixtureDir = "test/fixtures/fake-mcp"

toPy :: A.Value -> PyVal
toPy = either error id . loads . BL.toStrict . A.encode

-- | The entry the workflow writes for the prover, with the fake server as
-- its command.
fakeEntry :: ProverKind -> FilePath -> FilePath -> [String] -> Int -> IO PyVal
fakeEntry kind root logPath extra secs = do
  fake <- makeAbsolute (fixtureDir </> "fake_mcp.py")
  tools <- makeAbsolute (fixtureDir </> (if kind == Rocq then "rocq-tools.json" else "isabelle-tools.json"))
  let cmd = map T.pack (["python3", fake, "--tools", tools, "--log", logPath] <> extra)
  pure (toPy (serverPayloadFor kind cmd root Nothing (svTools (pvServer (prover kind))) secs))

data Chat = Chat
  { chReg :: Registry
  , chWarnings :: [Text]
  , chLogs :: IORef [(Text, Text)]
  }

-- | What @fisher chat@ does with its servers: add and check them
-- ('chatMcpServers'), connect them, and register the toolset in use.
withChat :: FilePath -> Maybe Text -> [(Text, PyVal)] -> [Text] -> (Chat -> IO a) -> IO a
withChat home path extra sets act = do
  at <- newAgentTools (home </> ".fisher")
  logs <- newIORef []
  let reg = atRegistry at
      env =
        McpEnv
          { meRegistry = reg
          , meLog = \lvl msg -> modifyIORef logs (<> [(lvl, msg)])
          , meSampling = \_ -> fail "no sampling in tests"
          , meEnviron = pure [("HOME", T.pack home), ("PATH", "/usr/local/bin:/usr/bin:/bin")]
          , meHome = T.pack home
          , meFisherHome = home </> ".fisher"
          , meHttp = Nothing }
  (servers, warnings) <- chatMcpServers [("PATH", T.unpack p) | Just p <- [path]] (PDict []) Nothing Nothing extra
  bracket (newMcpClient env) shutdownMcp $ \mc -> do
    _ <- discover mc (PDict servers)
    registerChatToolsets reg sets
    act (Chat reg warnings logs)

offered :: Chat -> [Text] -> IO [Text]
offered c sets = do
  (defs, _) <- getToolDefinitions (chReg c) (Just sets) Nothing
  pure (sort [n | d <- defs, Just (PStr n) <- [get "function" d >>= get "name"]])

call :: Chat -> Text -> IO Text
call c name = dispatch (chReg c) defaultCtx name (PDict [("source", PStr "Lemma x : True. Proof. exact I. Qed.")])

logLines :: FilePath -> IO [PyVal]
logLines p = do
  exists <- doesFileExist p
  if not exists then pure [] else do
    b <- BS.readFile p
    pure [v | l <- T.lines (TE.decodeUtf8 b), not (T.null l), Right v <- [loads (TE.encodeUtf8 l)]]

starts :: FilePath -> IO [PyVal]
starts p = filter (isJust . get "start") <$> logLines p

-- | A directory holding an executable @pet@ stub.
petDir :: FilePath -> IO FilePath
petDir tmp = do
  let d = tmp </> "petbin"
  createDirectoryIfMissing True d
  writeFile (d </> "pet") "#!/bin/sh\nexit 0\n"
  setFileMode (d </> "pet") 0o755
  pure d

withTmp :: (FilePath -> IO a) -> IO a
withTmp act = withSystemTempDirectory "fisher-prover-chat" (\d -> canonicalizePath d >>= act)

prefixed :: Text -> [Text] -> [Text]
prefixed p = filter (p `T.isPrefixOf`)

spec :: Spec
spec = do
  describe "the native toolsets" $ do
    it "are the file and terminal tools of fisher-lean plus the prover's server" $ do
      reg <- newRegistry
      registerChatToolsets reg ["fisher-lean", "fisher-rocq", "fisher-isabelle"]
      tss <- toolsets reg
      let view n = fmap (\ts -> (tsTools ts, tsIncludes ts)) (lookup n tss)
          files = ["read_file", "write_file", "patch", "search_files", "terminal", "process"]
      view "fisher-lean" `shouldBe` Just (files, ["web", "mcp-lean-lsp"])
      view "fisher-rocq" `shouldBe` Just (files, ["mcp-rocq"])
      view "fisher-isabelle" `shouldBe` Just (files, ["mcp-isabelle"])
      map nativeToolset [Lean4, Rocq, Isabelle] `shouldBe` ["fisher-lean", "fisher-rocq", "fisher-isabelle"]

    it "are registered only when asked for" $ do
      reg <- newRegistry
      registerChatToolsets reg ["fisher-cli"]
      tss <- toolsets reg
      filter (`elem` ["fisher-lean", "fisher-rocq", "fisher-isabelle"]) (map fst tss) `shouldBe` []

  describe "pvToolsFor" $ do
    it "gives Rocq's twelve with pet and the five coqc tools without it" $ do
      pvToolsFor Rocq True `shouldBe` svTools (pvServer (prover Rocq))
      length (pvToolsFor Rocq True) `shouldBe` 12
      pvToolsFor Rocq False `shouldBe` rocqCoqcTools
      pvToolsFor Isabelle False `shouldBe` svTools (pvServer (prover Isabelle))

  describe "fisher-rocq against the recorded Rocq-MCP tools" $ do
    it "offers exactly the included tools, not rocq_switch, with pet on PATH" $ withTmp $ \tmp -> do
      pet <- petDir tmp
      entry <- fakeEntry Rocq tmp (tmp </> "log") [] 300
      withChat tmp (Just (T.pack pet <> ":/usr/bin:/bin")) [("rocq", entry)] ["fisher-rocq"] $ \c -> do
        names <- offered c ["fisher-rocq"]
        prefixed "mcp_rocq_rocq_" names `shouldBe` sort (map ("mcp_rocq_" <>) (svTools (pvServer (prover Rocq))))
        names `shouldNotContain` ["mcp_rocq_rocq_switch"]
        forM_ ["read_file", "write_file"] $ \t -> names `shouldContain` [t]
        chWarnings c `shouldBe` []

    it "offers the five coqc tools and warns once without pet" $ withTmp $ \tmp -> do
      entry <- fakeEntry Rocq tmp (tmp </> "log") [] 300
      withChat tmp (Just "/usr/bin:/bin") [("rocq", entry)] ["fisher-rocq"] $ \c -> do
        names <- offered c ["fisher-rocq"]
        prefixed "mcp_rocq_rocq_" names `shouldBe` sort (map ("mcp_rocq_" <>) rocqCoqcTools)
        chWarnings c `shouldBe` [petMissingWarning]
        petMissingWarning `shouldBe` "pet not found: the interactive Rocq tools are unavailable"

    it "calls a tool and gets the server's text" $ withTmp $ \tmp -> do
      entry <- fakeEntry Rocq tmp (tmp </> "log") [] 300
      withChat tmp (Just "/usr/bin:/bin") [("rocq", entry)] ["fisher-rocq"] $ \c ->
        call c "mcp_rocq_rocq_compile" `shouldReturn` "{\"result\": \"ok: rocq_compile\"}"

    it "starts the server in the entry's cwd" $ withTmp $ \tmp -> do
      let root = tmp </> "proj"
      createDirectoryIfMissing True root
      entry <- fakeEntry Rocq root (tmp </> "log") [] 300
      withChat tmp (Just "/usr/bin:/bin") [("rocq", entry)] ["fisher-rocq"] $ \_ -> do
        s <- starts (tmp </> "log")
        map (get "cwd") s `shouldBe` [Just (PStr (T.pack root))]

    it "reconnects after the server crashes, and a later call returns" $ withTmp $ \tmp -> do
      let logPath = tmp </> "log"
      entry <- fakeEntry Rocq tmp logPath ["--crash-after", "1"] 300
      withChat tmp (Just "/usr/bin:/bin") [("rocq", entry)] ["fisher-rocq"] $ \c -> do
        call c "mcp_rocq_rocq_compile" `shouldReturn` "{\"result\": \"ok: rocq_compile\"}"
        crashed <- call c "mcp_rocq_rocq_compile"
        crashed `shouldBe` "{\"error\": \"MCP call failed: McpError: Connection closed\"}"
        let retry :: Int -> IO Text
            retry n = do
              r <- call c "mcp_rocq_rocq_verify"
              if "\"result\"" `T.isPrefixOf` T.drop 1 r || n <= 0 then pure r else threadDelay 250000 >> retry (n - 1)
        retry 60 `shouldReturn` "{\"result\": \"ok: rocq_verify\"}"
        length <$> starts logPath `shouldReturn` 2
        logs <- readIORef (chLogs c)
        [m | ("WARNING", m) <- logs, "connection lost (attempt 1/5)" `T.isInfixOf` m] `shouldSatisfy` ((== 1) . length)

    it "returns the timeout error after the configured seconds when the server hangs" $ withTmp $ \tmp -> do
      entry <- fakeEntry Rocq tmp (tmp </> "log") ["--hang"] 2
      withChat tmp (Just "/usr/bin:/bin") [("rocq", entry)] ["fisher-rocq"] $ \c -> do
        t0 <- getMonotonicTime
        r <- call c "mcp_rocq_rocq_compile"
        t1 <- getMonotonicTime
        r `shouldBe` "{\"error\": \"MCP call failed: TimeoutError: no reply within 2 seconds\"}"
        (t1 - t0) `shouldSatisfy` (\d -> d >= 1.9 && d < 10)

  describe "fisher-isabelle against the recorded Isabelle-MCP tools" $
    it "offers the 13 evaluation and inspection tools and no debugger tool" $ withTmp $ \tmp -> do
      entry <- fakeEntry Isabelle tmp (tmp </> "log") [] 300
      withChat tmp (Just "/usr/bin:/bin") [("isabelle", entry)] ["fisher-isabelle"] $ \c -> do
        names <- offered c ["fisher-isabelle"]
        let mine = prefixed "mcp_isabelle_isabelle_" names
        mine `shouldBe` sort (map ("mcp_isabelle_" <>) (svTools (pvServer (prover Isabelle))))
        length mine `shouldBe` 13
        forM_ ["breakpoint", "debug", "step", "locals", "continue", "breakable"] $ \w ->
          filter (w `T.isInfixOf`) mine `shouldBe` []
        chWarnings c `shouldBe` []

  describe "fisher chat in a prover project" $ do
    let project kind tmp = do
          let (dir, marker) = if kind == Rocq then ("rocqproj", "_RocqProject") else ("isaproj", "ROOT")
              p = tmp </> dir
          createDirectoryIfMissing True (p </> "sub")
          writeFile (p </> marker) "\n"
          _ <- initializeProject p defaultInitOptions {initName = Just "Demo"}
          pure p
    it "finds the project's prover from a directory inside it" $ withTmp $ \tmp -> do
      r <- project Rocq tmp
      i <- project Isabelle tmp
      chatProject (r </> "sub") `shouldReturn` Just (Rocq, r)
      chatProject i `shouldReturn` Just (Isabelle, i)
      chatProject tmp `shouldReturn` Nothing

    -- a bin directory holding uvx (and pet when asked), as the PATH
    let binWith tmp names = do
          let d = tmp </> "bin"
          createDirectoryIfMissing True d
          forM_ names $ \n -> writeFile (d </> n) "#!/bin/sh\nexit 0\n" >> setFileMode (d </> n) 0o755
          pure [("PATH", d <> ":/usr/bin:/bin")]
        entryOf servers name = maybe PNull id (lookup name servers)
    it "adds the prover's server when none is given, in the prover root, with the configured timeout" $ withTmp $ \tmp -> do
      r <- project Rocq tmp
      env <- binWith tmp ["uvx"]
      let cfg = PDict [("fisher", PDict [("provers", PDict [("tool_timeout", PInt 45)])])]
      (servers, warnings) <- chatMcpServers env cfg Nothing (Just (Rocq, r)) []
      map fst servers `shouldBe` ["rocq"]
      let entry = entryOf servers "rocq"
      get "command" entry `shouldBe` Just (PStr (T.pack (tmp </> "bin" </> "uvx")))
      get "args" entry `shouldBe` Just (PList (map PStr ["--from", "git+https://github.com/LLM4Rocq/rocq-mcp@6983113d0844c0b7f987c79dab13988445109bfb", "rocq-mcp"]))
      get "cwd" entry `shouldBe` Just (PStr (T.pack r))
      get "timeout" entry `shouldBe` Just (PInt 45)
      (get "tools" entry >>= get "include") `shouldBe` Just (PList (map PStr rocqCoqcTools))
      get "reconnect_on_exit" entry `shouldBe` Just (PBool True)
      (get "env" entry >>= get "ROCQ_WORKSPACE") `shouldBe` Just (PStr (T.pack r))
      warnings `shouldBe` [petMissingWarning]

    it "runs Rocq-MCP's coqc as a staged wrapper when only rocq is on PATH" $ withTmp $ \tmp -> do
      r <- project Rocq tmp
      -- only the bin directory: a coqc of the host's must not be found
      _ <- (binWith tmp ["uvx", "rocq"] :: IO [(String, String)])
      let env = [("PATH", tmp </> "bin")]
      (servers, _) <- chatMcpServers env (PDict []) (Just (tmp </> "w")) (Just (Rocq, r)) []
      (get "env" (entryOf servers "rocq") >>= get "ROCQ_COQC_BINARY") `shouldBe` Just (PStr (T.pack (tmp </> "w" </> "rocq-bin" </> "coqc")))
      doesFileExist (tmp </> "w" </> "rocq-bin" </> "coqc") `shouldReturn` True
      _ <- (binWith tmp ["coqc"] :: IO [(String, String)])
      let env2 = env
      (servers2, _) <- chatMcpServers env2 (PDict []) (Just (tmp </> "w2")) (Just (Rocq, r)) []
      (get "env" (entryOf servers2 "rocq") >>= get "ROCQ_COQC_BINARY") `shouldBe` Nothing

    it "starts Isabelle-MCP through the shim staged under the state directory" $ withTmp $ \tmp -> do
      i <- project Isabelle tmp
      env <- binWith tmp ["uvx"]
      (servers, warnings) <- chatMcpServers env (PDict []) (Just (tmp </> "w")) (Just (Isabelle, i)) []
      let shim = tmp </> "w" </> "isabelle-mcp" </> "isabelle_mcp_shim.py"
      get "args" (entryOf servers "isabelle") `shouldBe` Just (PList (map PStr ["--from", "isabelle-mcp==0.6.0", "python", T.pack shim]))
      doesFileExist shim `shouldReturn` True
      warnings `shouldBe` []

    it "honours the override variable" $ withTmp $ \tmp -> do
      i <- project Isabelle tmp
      env <- binWith tmp ["uvx"]
      (servers, warnings) <- chatMcpServers (env <> [("FISHER_AUTOFORMALIZE_ISABELLE_MCP_SPEC", "isabelle-mcp==0.6.1")]) (PDict []) Nothing (Just (Isabelle, i)) []
      get "args" (entryOf servers "isabelle") `shouldBe` Just (PList (map PStr ["--from", "isabelle-mcp==0.6.1", "isabelle-mcp"]))
      warnings `shouldBe` []

    it "warns and adds no server for a malformed override" $ withTmp $ \tmp -> do
      r <- project Rocq tmp
      env <- binWith tmp ["uvx", "pet"]
      (servers, warnings) <- chatMcpServers (env <> [("FISHER_AUTOFORMALIZE_ROCQ_MCP_SPEC", "uvx rocq-mcp")]) (PDict []) Nothing (Just (Rocq, r)) []
      servers `shouldBe` []
      warnings `shouldBe` ["FISHER_AUTOFORMALIZE_ROCQ_MCP_SPEC must be a package spec, without spaces."]

    it "warns and adds no server when neither uvx nor uv is on PATH" $ withTmp $ \tmp -> do
      i <- project Isabelle tmp
      env <- binWith tmp []
      (servers, warnings) <- chatMcpServers env (PDict []) Nothing (Just (Isabelle, i)) []
      servers `shouldBe` []
      warnings `shouldBe` ["uvx not found: install uv"]

    it "takes uv when uvx is missing" $ withTmp $ \tmp -> do
      i <- project Isabelle tmp
      env <- binWith tmp ["uv"]
      (servers, warnings) <- chatMcpServers env (PDict []) Nothing (Just (Isabelle, i)) []
      get "command" (entryOf servers "isabelle") `shouldBe` Just (PStr (T.pack (tmp </> "bin" </> "uv")))
      get "args" (entryOf servers "isabelle") `shouldBe` Just (PList (map PStr ["x", "--from", "isabelle-mcp==0.6.0", "isabelle-mcp"]))
      warnings `shouldBe` []

    it "checks pet for a rocq server from config.yaml too" $ do
      let mine = PDict [("command", PStr "/opt/rocq-mcp"), ("tools", PDict [("include", PList (map PStr ["rocq_compile", "rocq_start"]))])]
          cfg = PDict [("mcp_servers", PDict [("rocq", mine)])]
      (servers, warnings) <- chatMcpServers [("PATH", "/usr/bin:/bin")] cfg Nothing Nothing []
      (lookup "rocq" servers >>= get "tools" >>= get "include") `shouldBe` Just (PList [PStr "rocq_compile"])
      warnings `shouldBe` [petMissingWarning]

    it "keeps a server given for it, and adds none outside a prover project" $ withTmp $ \tmp -> do
      i <- project Isabelle tmp
      env <- binWith tmp ["uvx"]
      let mine = PDict [("command", PStr "/opt/isabelle-mcp")]
      (servers, warnings) <- chatMcpServers env (PDict []) Nothing (Just (Isabelle, i)) [("isabelle", mine)]
      servers `shouldBe` [("isabelle", mine)]
      warnings `shouldBe` []
      (servers2, _) <- chatMcpServers env (PDict []) Nothing Nothing []
      servers2 `shouldBe` []
      let configured = PDict [("mcp_servers", PDict [("isabelle", mine)])]
      (servers3, _) <- chatMcpServers env configured Nothing (Just (Isabelle, i)) []
      servers3 `shouldBe` []

    it "defaults to the prover's toolset" $ do
      defaultToolsets (Just (Rocq, "/p")) Nothing `shouldBe` Just ["fisher-rocq"]
      defaultToolsets (Just (Isabelle, "/p")) Nothing `shouldBe` Just ["fisher-isabelle"]
      defaultToolsets (Just (Rocq, "/p")) (Just ["fisher-cli"]) `shouldBe` Just ["fisher-cli"]
      defaultToolsets Nothing Nothing `shouldBe` Nothing

    it "looks for the project without -t, or with a prover toolset and no --mcp-config" $ do
      wantsProject Nothing False `shouldBe` True
      wantsProject Nothing True `shouldBe` True
      wantsProject (Just ["fisher-rocq"]) False `shouldBe` True
      wantsProject (Just ["fisher-isabelle", "web"]) False `shouldBe` True
      wantsProject (Just ["fisher-rocq"]) True `shouldBe` False
      wantsProject (Just ["fisher-cli"]) False `shouldBe` False
      wantsProject (Just ["fisher-lean"]) False `shouldBe` False

    it "reads the server's environment from these variables" $
      chatEnvNames `shouldBe` ["PATH", "FISHER_AUTOFORMALIZE_ROCQ_MCP_SPEC", "FISHER_AUTOFORMALIZE_ISABELLE_MCP_SPEC"]
