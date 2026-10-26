-- | The parts of the doctor run around the managed section.
module Fisher.DoctorSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (object, (.=))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, getPermissions, setOwnerExecutable, setPermissions)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Doctor
import Fisher.Paths (ensureFisherHomeAt)
import Fisher.Term (plainStyle)
import Fisher.TestUtil (withTmp)

envFor :: FilePath -> DoctorEnv
envFor tmp = DoctorEnv plainStyle (object []) [("PATH", ""), ("HOME", tmp)] (Just tmp) (const (pure Nothing)) (pure Nothing) "fisher" Nothing Nothing

spec :: Spec
spec = do
  it "draws upstream's box, with the title centred as Python centres it" $ withTmp $ \tmp -> do
    out <- runDoctor False False (envFor tmp) (tmp </> "gh")
    take 4 out `shouldBe`
      [ ""
      , "┌" <> T.replicate 57 "─" <> "┐"
      -- Python: f"│ {'Fisher Doctor':^55} │"
      , "│                      Fisher Doctor                      │"
      , "└" <> T.replicate 57 "─" <> "┘" ]

  it "reports a fresh home as issues, with the --fix tip, naming the home in use" $ withTmp $ \tmp -> do
    out <- runDoctor False False (envFor tmp) (tmp </> ".fisher")
    out `shouldSatisfy` elem "  ✗ ~/.fisher/.env file missing"
    out `shouldSatisfy` elem "  ⚠ config.yaml not found (using defaults)"
    out `shouldSatisfy` elem "  ⚠ ~/.fisher/skills/ not found (will be created on first use)"
    out `shouldSatisfy` any ("Run 'fisher setup' to create .env" `T.isInfixOf`)
    out `shouldSatisfy` elem "  Tip: run 'fisher doctor --fix' to auto-fix what's possible."
    doesFileExist (tmp </> ".fisher" </> ".env") `shouldReturn` False
    elsewhere <- runDoctor False False (envFor tmp) (tmp </> "gh")
    elsewhere `shouldSatisfy` elem ("  ✗ " <> T.pack (tmp </> "gh") <> "/.env file missing")

  it "fixes what it can with --fix" $ withTmp $ \tmp -> do
    let gh = tmp </> "gh"
    out <- runDoctor True False (envFor tmp) gh
    doesFileExist (gh </> ".env") `shouldReturn` True
    doesFileExist (gh </> "config.yaml") `shouldReturn` True
    doesFileExist (gh </> "SOUL.md") `shouldReturn` True
    doesDirectoryExist (gh </> "skills") `shouldReturn` True
    out `shouldSatisfy` any ("  Fixed " `T.isPrefixOf`)

  it "passes the home checks once the home is set up" $ withTmp $ \tmp -> do
    let gh = tmp </> "gh"
    ensureFisherHomeAt gh
    createDirectoryIfMissing True (gh </> "skills")
    writeFile (gh </> ".env") "ANTHROPIC_API_KEY=sk\n"
    out <- runDoctor False False (envFor tmp) gh
    out `shouldSatisfy` elem "  ✓ API key or custom endpoint configured"
    -- a home elsewhere is named by its path
    out `shouldSatisfy` elem ("  ✓ " <> T.pack gh <> "/SOUL.md exists (persona configured)")
    out `shouldSatisfy` elem ("  ✓ " <> T.pack gh <> "/skills/ exists")

  describe "provers" proverSpec
  describe "Claude auth" claudeSpec
  describe "SMT" smtSpec

-- | An executable shell script in @dir@.
stub :: FilePath -> String -> String -> IO ()
stub dir name body = do
  createDirectoryIfMissing True dir
  let p = dir </> name
  writeFile p ("#!/bin/sh\n" <> body <> "\n")
  getPermissions p >>= setPermissions p . setOwnerExecutable True

echo :: String -> String
echo t = "echo '" <> t <> "'"

-- | The doctor run in @proj@ with only @bin@ on PATH (and extra variables).
doctorIn :: Bool -> FilePath -> FilePath -> FilePath -> [(String, String)] -> IO [Text]
doctorIn allProvers tmp proj bin extra =
  runDoctor False allProvers
    (DoctorEnv plainStyle (object []) ([("PATH", bin), ("HOME", tmp)] <> extra) (Just proj) (const (pure Nothing)) (pure Nothing) "fisher" Nothing Nothing)
    (tmp </> "gh")

-- | uvx and Python 3.12 stubs.
helpers :: FilePath -> IO ()
helpers bin = do
  stub bin "uvx" (echo "uv 0.5.0")
  stub bin "python3" (echo "Python 3.12.4")

-- | An @isabelle@ stub printing this version and, when @registered@, a
-- components list holding Isabelle-MCP's component.
isabelleStub :: FilePath -> String -> Bool -> IO ()
isabelleStub bin version registered =
  stub bin "isabelle" $ unlines
    [ "case \"$1\" in"
    , "  version) " <> echo version <> " ;;"
    , "  components) " <> echo "/Users/t/.isabelle/contrib/naproche" <> if registered then " ; " <> echo "/Users/t/.local/share/uv/tools/isabelle-mcp/lib/python3.12/site-packages/isabelle_mcp/component" <> " ;;" else " ;;"
    , "esac" ]

rocqProject :: FilePath -> IO FilePath
rocqProject tmp = do
  let p = tmp </> "rp"
  createDirectoryIfMissing True p
  writeFile (p </> "_CoqProject") "-R theories Demo\n"
  pure p

isabelleProject :: FilePath -> IO FilePath
isabelleProject tmp = do
  let p = tmp </> "ip"
  createDirectoryIfMissing True p
  writeFile (p </> "ROOT") "session Demo = HOL +\n  theories Demo\n"
  pure p

proverSpec :: Spec
proverSpec = do
  it "passes Rocq 9.0 with pet, uvx and Python" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "rocq" (echo "The Rocq Prover, version 9.0.0" <> "; " <> echo "compiled with OCaml 4.14.1")
    stub bin "pet" (echo "0.2.5")
    helpers bin
    proj <- rocqProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "◆ Managed Rocq Workflows"
    out `shouldSatisfy` elem "  ✓ Rocq (rocq) 9.0.0 (Rocq-MCP runs it through a coqc wrapper)"
    out `shouldSatisfy` elem "  ✓ coq-lsp pet"
    out `shouldSatisfy` elem "  ✓ uvx"
    out `shouldSatisfy` elem "  ✓ Python 3.12.4"
    out `shouldNotSatisfy` elem "◆ Managed Isabelle Workflows"
    out `shouldNotSatisfy` any ("Rocq: " `T.isInfixOf`)

  it "reports coqc, which Rocq-MCP runs, when both coqc and rocq are there" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "rocq" (echo "The Rocq Prover, version 9.1.1")
    stub bin "coqc" (echo "The Rocq Prover, version 9.1.1")
    stub bin "pet" (echo "0.2.5")
    helpers bin
    proj <- rocqProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ✓ Rocq (coqc) 9.1.1"

  it "warns on Coq 8.18 (coqc only)" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "coqc" (echo "The Coq Proof Assistant, version 8.18.0")
    stub bin "pet" (echo "0.1.8")
    helpers bin
    proj <- rocqProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ⚠ Rocq (coqc) 8.18.0 (below 9.0: the interactive tools need Rocq 9); fix: opam install rocq-prover coq-lsp"

  it "fails on Coq 8.16, with the fix as an issue" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "coqc" (echo "The Coq Proof Assistant, version 8.16.1")
    stub bin "pet" (echo "0.1.8")
    helpers bin
    proj <- rocqProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ✗ Rocq (coqc) 8.16.1 (below 8.17, not supported); fix: opam install rocq-prover coq-lsp"
    out `shouldSatisfy` any ("Rocq: opam install rocq-prover coq-lsp" `T.isSuffixOf`)

  it "fails without Rocq" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    helpers bin
    proj <- rocqProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ✗ Rocq (rocq or coqc not found); fix: opam install rocq-prover coq-lsp"

  it "warns when pet is missing" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "rocq" (echo "The Rocq Prover, version 9.1.1")
    helpers bin
    proj <- rocqProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ✓ Rocq (rocq) 9.1.1 (Rocq-MCP runs it through a coqc wrapper)"
    out `shouldSatisfy` elem "  ⚠ coq-lsp pet not found (interactive tools unavailable); fix: opam install rocq-prover coq-lsp"

  it "fails without uvx or on an old Python" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "rocq" (echo "The Rocq Prover, version 9.0.0")
    stub bin "pet" (echo "0.2.5")
    stub bin "python3" (echo "Python 3.10.12")
    proj <- rocqProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ✗ uvx (not found); fix: install uv"
    out `shouldSatisfy` elem "  ✗ Python 3.10.12 (Rocq-MCP needs 3.11 or later); fix: install Python 3.12"

  it "passes Isabelle2025-2 with Isabelle-MCP registered" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    isabelleStub bin "Isabelle2025-2" True
    helpers bin
    proj <- isabelleProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "◆ Managed Isabelle Workflows"
    out `shouldSatisfy` elem "  ✓ Isabelle 2025-2"
    out `shouldSatisfy` elem "  ✓ Isabelle-MCP registered"
    out `shouldSatisfy` elem "  ✓ uvx"
    out `shouldSatisfy` elem "  ✓ Python 3.12.4"
    out `shouldNotSatisfy` elem "◆ Managed Rocq Workflows"

  it "warns on Isabelle2024" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    isabelleStub bin "Isabelle2024" True
    helpers bin
    proj <- isabelleProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ⚠ Isabelle 2024 (Fisher expects Isabelle2025-2); fix: install Isabelle2025-2 from isabelle.in.tum.de"

  it "fails without the Isabelle-MCP registration, ending with its fix" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    isabelleStub bin "Isabelle2025-2" False
    helpers bin
    proj <- isabelleProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ✗ Isabelle-MCP not registered (isabelle components -l does not list it); fix: isabelle-mcp install"
    out `shouldSatisfy` any ("Isabelle: isabelle-mcp install" `T.isSuffixOf`)

  it "only warns on Python below 3.12 for Isabelle-MCP when uv is there (uvx brings its own)" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    isabelleStub bin "Isabelle2025-2" True
    stub bin "uvx" (echo "uv 0.5.0")
    stub bin "python3" (echo "Python 3.11.9")
    proj <- isabelleProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ⚠ Python 3.11.9 (Isabelle-MCP needs 3.12 or later; uvx brings its own)"
    out `shouldNotSatisfy` any ("install Python" `T.isInfixOf`)

  it "passes without python3 when uv is there" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    isabelleStub bin "Isabelle2025-2" True
    stub bin "uv" (echo "uv 0.5.0")
    proj <- isabelleProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ✓ Python (none on PATH; uvx brings its own)"

  it "takes Rocq 9 as 9.0 (versions padded)" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "coqc" (echo "The Rocq Prover, version 9")
    stub bin "pet" (echo "0.2.5")
    helpers bin
    proj <- rocqProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ✓ Rocq (coqc) 9"

  it "finds Isabelle off PATH under ~/Applications and says how to put it on PATH" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
        app = tmp </> "Applications" </> "Isabelle2025-2.app"
    isabelleStub (app </> "bin") "Isabelle2025-2" True
    helpers bin
    proj <- isabelleProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem (T.pack ("  ⚠ Isabelle 2025-2 (at " <> app <> ", not on PATH); fix: add " <> app <> "/bin to PATH"))
    out `shouldSatisfy` elem "  ✓ Isabelle-MCP registered"

  it "finds Isabelle under ISABELLE_HOME, and the registration fix names that binary" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
        home = tmp </> "isa"
    isabelleStub (home </> "bin") "Isabelle2025-2" False
    helpers bin
    proj <- isabelleProject tmp
    out <- doctorIn False tmp proj bin [("ISABELLE_HOME", home)]
    out `shouldSatisfy` elem (T.pack ("  ⚠ Isabelle 2025-2 (at " <> home <> ", not on PATH); fix: add " <> home <> "/bin to PATH"))
    out `shouldSatisfy` elem (T.pack ("  ✗ Isabelle-MCP not registered (isabelle components -l does not list it); fix: isabelle-mcp install --isabelle-bin " <> home <> "/bin/isabelle"))

  it "fails without Isabelle" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    helpers bin
    proj <- isabelleProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ✗ Isabelle (not found on PATH, under ISABELLE_HOME or in Applications); fix: install Isabelle2025-2 from isabelle.in.tum.de"

  it "checks no prover outside a Rocq or Isabelle project, and every prover with --all" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    helpers bin
    createDirectoryIfMissing True (tmp </> "plain")
    out <- doctorIn False tmp (tmp </> "plain") bin []
    out `shouldNotSatisfy` elem "◆ Managed Rocq Workflows"
    out `shouldNotSatisfy` elem "◆ Managed Isabelle Workflows"
    every <- doctorIn True tmp (tmp </> "plain") bin []
    every `shouldSatisfy` elem "◆ Managed Rocq Workflows"
    every `shouldSatisfy` elem "◆ Managed Isabelle Workflows"
    every `shouldSatisfy` elem "  ✗ Rocq (rocq or coqc not found); fix: opam install rocq-prover coq-lsp"

  it "does not ask for Lean's lake in a Rocq project" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "rocq" (echo "The Rocq Prover, version 9.0.0")
    stub bin "pet" (echo "0.2.5")
    helpers bin
    proj <- rocqProject tmp
    out <- doctorIn False tmp proj bin []
    out `shouldSatisfy` elem "  ⚠ Lean toolchain (lake) (needed for Lean projects only)"
    out `shouldNotSatisfy` any ("Install elan" `T.isInfixOf`)

-- | A fake @claude@ in @bin@: @auth status@ prints the status file and
-- exits with the code file's number.
claudeStub :: FilePath -> FilePath -> IO ()
claudeStub tmp bin = do
  writeFile (tmp </> "status.json") "{\"loggedIn\": false}"
  writeFile (tmp </> "code") "1"
  stub bin "claude" ("if [ \"$1 $2\" = \"auth status\" ]; then /bin/cat " <> tmp </> "status.json" <> "; exit $(/bin/cat " <> tmp </> "code" <> "); fi")

loggedInJson :: String
loggedInJson = "{\"loggedIn\": true, \"authMethod\": \"claude.ai\", \"email\": \"x@y.z\", \"subscriptionType\": \"max\"}"

-- | The doctor run with only @bin@ on PATH, a login hook, and --fix or not.
claudeDoctor :: Bool -> Maybe (FilePath -> IO Bool) -> FilePath -> FilePath -> IO [Text]
claudeDoctor fix hook tmp bin =
  runDoctor fix False
    (DoctorEnv plainStyle (object []) [("PATH", bin), ("HOME", tmp)] (Just tmp) (const (pure Nothing)) (pure Nothing) "fisher" hook Nothing)
    (tmp </> "gh")

claudeSpec :: Spec
claudeSpec = do
  it "is ok with the Claude Code login, naming its email" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    claudeStub tmp bin
    writeFile (tmp </> "status.json") loggedInJson
    writeFile (tmp </> "code") "0"
    out <- claudeDoctor False Nothing tmp bin
    out `shouldSatisfy` elem "  ✓ Claude auth (Claude Code login: x@y.z)"
    out `shouldSatisfy` (not . any ("--provider claude-code" `T.isInfixOf`))
  it "names fisher login --provider claude-code when Claude Code is logged out" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    claudeStub tmp bin
    out <- claudeDoctor False Nothing tmp bin
    out `shouldSatisfy` elem "  ✗ Claude auth (Claude Code is not logged in)"
    out `shouldSatisfy` any ("Run `fisher login --provider claude-code`, save `ANTHROPIC_API_KEY`, or set `fisher.autoformalize.auth_mode: login`" `T.isSuffixOf`)
  it "falls back to the local login when the status cannot be read" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "claude" "exit 0"
    out <- claudeDoctor False Nothing tmp bin
    out `shouldSatisfy` elem "  ✗ Claude auth (not found)"
    out `shouldSatisfy` any ("Run `claude auth login`, save `ANTHROPIC_API_KEY`" `T.isInfixOf`)
    createDirectoryIfMissing True (tmp </> ".claude")
    writeFile (tmp </> ".claude" </> ".credentials.json") "{\"claudeAiOauth\": {\"accessToken\": \"at\"}}"
    out2 <- claudeDoctor False Nothing tmp bin
    out2 `shouldSatisfy` elem "  ✓ Claude auth (local Claude login)"
  it "with --fix and a terminal, offers the login and counts it fixed once it is done" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    claudeStub tmp bin
    asked <- newIORef []
    let hook exe = do
          modifyIORef asked (<> [exe])
          writeFile (tmp </> "status.json") loggedInJson
          writeFile (tmp </> "code") "0"
          pure True
    out <- claudeDoctor True (Just hook) tmp bin
    readIORef asked `shouldReturn` [bin </> "claude"]
    -- the failed line first (the question is about it), then the login's result
    let next = drop 1 (dropWhile (/= "  ✗ Claude auth (Claude Code is not logged in)") out)
    take 1 next `shouldBe` ["  ✓ Claude auth (Claude Code login: x@y.z)"]
    out `shouldSatisfy` (not . any ("--provider claude-code" `T.isInfixOf`))
    out `shouldSatisfy` any ("  Fixed " `T.isPrefixOf`)
  it "reports as without --fix when the login is declined" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    claudeStub tmp bin
    out <- claudeDoctor True (Just (\_ -> pure False)) tmp bin
    out `shouldSatisfy` elem "  ✗ Claude auth (Claude Code is not logged in)"
    out `shouldSatisfy` any ("Run `fisher login --provider claude-code`" `T.isInfixOf`)
  it "asks the question after the line it is about" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    claudeStub tmp bin
    seen <- newIORef []
    printed <- newIORef []
    let hook _ = readIORef printed >>= writeIORef seen >> pure False
    runDoctorTo (\l -> modifyIORef printed (<> [l])) True False
      (DoctorEnv plainStyle (object []) [("PATH", bin), ("HOME", tmp)] (Just tmp) (const (pure Nothing)) (pure Nothing) "fisher" (Just hook) Nothing)
      (tmp </> "gh")
    printedFirst <- readIORef seen
    take 1 (reverse printedFirst) `shouldBe` ["  ✗ Claude auth (Claude Code is not logged in)"]
  it "runs claude with the doctor's environment" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "claude" ("if [ -n \"$FISHER_DOCTOR_FAKE\" ]; then echo '" <> loggedInJson <> "'; else echo '{\"loggedIn\": false}'; exit 1; fi")
    out <- runDoctor False False
      (DoctorEnv plainStyle (object []) [("PATH", bin), ("HOME", tmp), ("FISHER_DOCTOR_FAKE", "1")] (Just tmp) (const (pure Nothing)) (pure Nothing) "fisher" Nothing Nothing)
      (tmp </> "gh")
    out `shouldSatisfy` elem "  ✓ Claude auth (Claude Code login: x@y.z)"
  it "does not run claude auth status in api_key mode" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "claude" ("echo \"$*\" >> " <> tmp </> "argv")
    _ <- runDoctor False False
      (DoctorEnv plainStyle (object ["fisher" .= object ["autoformalize" .= object ["auth_mode" .= ("api_key" :: Text)]]])
         [("PATH", bin), ("HOME", tmp)] (Just tmp) (const (pure Nothing)) (pure Nothing) "fisher" Nothing Nothing)
      (tmp </> "gh")
    doesFileExist (tmp </> "argv") `shouldReturn` False
  it "in login mode, warns with the login command" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    claudeStub tmp bin
    out <- runDoctor False False
      (DoctorEnv plainStyle (object ["fisher" .= object ["autoformalize" .= object ["auth_mode" .= ("login" :: Text)]]])
         [("PATH", bin), ("HOME", tmp)] (Just tmp) (const (pure Nothing)) (pure Nothing) "fisher" Nothing Nothing)
      (tmp </> "gh")
    out `shouldSatisfy` elem "  ⚠ Claude auth (Claude Code is not logged in: run `fisher login --provider claude-code`, or login mode will prompt on first launch)"

-- | Run with the process PATH set to @p@ (the solver lookup reads it).
withPath :: FilePath -> IO a -> IO a
withPath p act = bracket (lookupEnv "PATH") restore (\_ -> setEnv "PATH" p >> act)
  where restore = maybe (unsetEnv "PATH") (setEnv "PATH")

smtSpec :: Spec
smtSpec = do
  it "doctor names the solver" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    stub bin "z3" (echo "Z3 version 4.13.4 - 64 bit")
    out <- withPath bin (runDoctor False False (envFor tmp) (tmp </> "gh"))
    out `shouldSatisfy` elem "  ✓ SMT solver: z3 4.13.4"
  it "doctor says none without a solver" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
    createDirectoryIfMissing True bin
    out <- withPath bin (runDoctor False False (envFor tmp) (tmp </> "gh"))
    out `shouldSatisfy` elem "  ⚠ SMT solver: none (fisher project add smt still works; the smt tactic needs no solver on PATH)"
  it "doctor reports lean-smt from the manifest" $ withTmp $ \tmp -> do
    let bin = tmp </> "bin"
        manifest names = "{\"version\": \"1.1.0\", \"packages\": [" <> commaSep ["{\"name\": \"" <> n <> "\", \"type\": \"git\"}" | n <- names] <> "], \"name\": \"Foo\"}"
        commaSep = foldr1' (\a b -> a <> "," <> b)
        foldr1' _ [] = ""
        foldr1' f xs = foldr1 f xs
        project name pkgs = do
          let d = tmp </> name
          createDirectoryIfMissing True d
          writeFile (d </> "lake-manifest.json") (manifest pkgs)
          writeFile (d </> "lean-toolchain") "leanprover/lean4:v4.24.0\n"
          pure d
    createDirectoryIfMissing True bin
    with <- project "with" ["smt", "mathlib"]
    without <- project "without" ["mathlib"]
    a <- withPath bin (runDoctor False False (envFor tmp) { deActiveCwd = Just with } (tmp </> "gh"))
    a `shouldSatisfy` elem "  ✓ lean-smt: in lake-manifest.json"
    b <- withPath bin (runDoctor False False (envFor tmp) { deActiveCwd = Just without } (tmp </> "gh"))
    b `shouldSatisfy` elem "  ✓ lean-smt: not required (fisher project add smt)"
