-- | The parts of the doctor run around the managed section.
module Fisher.DoctorSpec (spec) where

import Data.Aeson (object)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Doctor
import Fisher.Paths (ensureFisherHomeAt)
import Fisher.Term (plainStyle)
import Fisher.TestUtil (withTmp)

envFor :: FilePath -> DoctorEnv
envFor tmp = DoctorEnv plainStyle (object []) [("PATH", ""), ("HOME", tmp)] (Just tmp) (const (pure Nothing)) (pure Nothing) "fisher"

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
    out <- runDoctor False False (envFor tmp) (tmp </> ".gauss")
    out `shouldSatisfy` elem "  ✗ ~/.gauss/.env file missing"
    out `shouldSatisfy` elem "  ⚠ config.yaml not found (using defaults)"
    out `shouldSatisfy` elem "  ⚠ ~/.gauss/skills/ not found (will be created on first use)"
    out `shouldSatisfy` any ("Run 'fisher setup' to create .env" `T.isInfixOf`)
    out `shouldSatisfy` elem "  Tip: run 'fisher doctor --fix' to auto-fix what's possible."
    doesFileExist (tmp </> ".gauss" </> ".env") `shouldReturn` False
    fresh <- runDoctor False False (envFor tmp) (tmp </> ".fisher")
    fresh `shouldSatisfy` elem "  ✗ ~/.fisher/.env file missing"

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
    (DoctorEnv plainStyle (object []) ([("PATH", bin), ("HOME", tmp)] <> extra) (Just proj) (const (pure Nothing)) (pure Nothing) "fisher")
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
