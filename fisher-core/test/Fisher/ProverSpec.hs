module Fisher.ProverSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix.Files (nullFileMode, setFileMode)
import System.Posix.User (getEffectiveUserID)
import Test.Hspec

import Fisher.Prover
import Fisher.TestUtil

-- | A directory holding one file.
withFile :: FilePath -> FilePath -> String -> IO ()
withFile dir name body = do
  createDirectoryIfMissing True dir
  writeFile (dir </> name) body

spec :: Spec
spec = do
  describe "names" $ do
    it "maps each kind to its manifest name and back" $
      forM_ [minBound .. maxBound] $ \k -> do
        proverByName (pvName (prover k)) `shouldBe` Just k
        pvKind (prover k) `shouldBe` k
    it "names the three kinds" $ do
      map (pvName . prover) [Lean4, Rocq, Isabelle] `shouldBe` ["lean4", "rocq", "isabelle"]
      proverByName "coq" `shouldBe` Nothing
      proverByName "Lean4" `shouldBe` Nothing
    it "gives each kind its source extension" $
      map (pvSourceExt . prover) [Lean4, Rocq, Isabelle] `shouldBe` [[".lean"], [".v"], [".thy"]]

  describe "servers" $ do
    it "launches Rocq-MCP from git through uvx, pinned to a commit, overridable" $ do
      let s = pvServer (prover Rocq)
      svName s `shouldBe` "rocq"
      svDefault s `shouldBe` ["uvx", "--from", "git+https://github.com/LLM4Rocq/rocq-mcp@6983113d0844c0b7f987c79dab13988445109bfb", "rocq-mcp"]
      svOverride s `shouldBe` "FISHER_AUTOFORMALIZE_ROCQ_MCP_SPEC"
    it "offers every Rocq-MCP tool but rocq_switch" $ do
      let ts = svTools (pvServer (prover Rocq))
      length ts `shouldBe` 12
      ts `shouldNotContain` ["rocq_switch"]
      forM_ rocqCoqcTools $ \t -> ts `shouldContain` [t]
    it "names the five tools that work without pet" $
      rocqCoqcTools `shouldBe` ["rocq_compile", "rocq_compile_file", "rocq_verify", "rocq_diag", "rocq_health"]
    it "offers the coqc tools alone when pet is not on PATH, with one warning text" $ do
      pvToolsFor Rocq True `shouldBe` svTools (pvServer (prover Rocq))
      pvToolsFor Rocq False `shouldBe` rocqCoqcTools
      forM_ [True, False] $ \pet -> pvToolsFor Isabelle pet `shouldBe` svTools (pvServer (prover Isabelle))
      petMissingWarning `shouldBe` "pet not found: the interactive Rocq tools are unavailable"
    it "launches Isabelle-MCP 0.6.0 through uvx and offers its 13 evaluation tools" $ do
      let s = pvServer (prover Isabelle)
      svName s `shouldBe` "isabelle"
      svDefault s `shouldBe` ["uvx", "--from", "isabelle-mcp==0.6.0", "isabelle-mcp"]
      svOverride s `shouldBe` "FISHER_AUTOFORMALIZE_ISABELLE_MCP_SPEC"
      svTools s `shouldBe`
        [ "isabelle_launch", "isabelle_terminate", "isabelle_evaluate_to", "isabelle_evaluation_status"
        , "isabelle_cancel_evaluation", "isabelle_hover", "isabelle_definition", "isabelle_local_occurrences"
        , "isabelle_goal", "isabelle_find_theorems", "isabelle_command_output", "isabelle_command_status"
        , "isabelle_session_info" ]

  describe "binaries" $ do
    it "requires isabelle for Isabelle, and for Rocq coqc or rocq, neither alone" $ do
      [binName b | b <- pvBinaries (prover Rocq), binRequired b] `shouldNotContain` ["coqc"]
      [binName b | b <- pvBinaries (prover Rocq), binRequired b] `shouldNotContain` ["rocq"]
      map binName (pvBinaries (prover Rocq)) `shouldContain` ["coqc", "rocq"]
      [binName b | b <- pvBinaries (prover Isabelle), binRequired b] `shouldBe` ["isabelle", "uvx"]
      [binName b | b <- pvBinaries (prover Rocq), binRequired b] `shouldBe` ["uvx"]
      [binName b | b <- pvBinaries (prover Lean4), binRequired b] `shouldBe` ["lake"]
    it "gives the install line for each binary" $ do
      binaryFix Rocq "coqc" `shouldBe` "opam install rocq-prover coq-lsp"
      binaryFix Rocq "pet" `shouldBe` "opam install rocq-prover coq-lsp"
      binaryFix Isabelle "isabelle" `shouldBe` "install Isabelle2025-2 from isabelle.in.tum.de"
      binaryFix Isabelle "isabelle-mcp" `shouldBe` "isabelle-mcp install"
      binaryFix Rocq "uvx" `shouldBe` "install uv"
      binaryFix Isabelle "python3" `shouldBe` "install Python 3.12"
    it "has a fix line for every binary it checks" $
      forM_ [minBound .. maxBound] $ \k -> forM_ (pvBinaries (prover k)) $ \b ->
        T.null (binaryFix k (binName b)) `shouldBe` False

  describe "findRocqCompiler" $ do
    let stubIn dir n = do
          createDirectoryIfMissing True dir
          writeFile (dir </> n) "#!/bin/sh\n"
          setFileMode (dir </> n) 0o755
    it "takes coqc when it is on PATH" $ withTmp $ \tmp -> do
      stubIn (tmp </> "a") "rocq"
      stubIn (tmp </> "b") "coqc"
      findRocqCompiler [("PATH", tmp </> "a" <> ":" <> tmp </> "b")] `shouldReturn` Just ("coqc", tmp </> "b" </> "coqc")
    it "takes rocq when coqc is not there" $ withTmp $ \tmp -> do
      stubIn (tmp </> "a") "rocq"
      findRocqCompiler [("PATH", tmp </> "a")] `shouldReturn` Just ("rocq", tmp </> "a" </> "rocq")
    it "finds nothing without either" $ withTmp $ \tmp ->
      findRocqCompiler [("PATH", tmp)] `shouldReturn` Nothing

  describe "versionBelow" $
    it "pads the shorter version with zeros" $ do
      versionBelow [9] [9, 0] `shouldBe` False
      versionBelow [9, 0] [9] `shouldBe` False
      versionBelow [8, 18, 0] [9, 0] `shouldBe` True
      versionBelow [8, 16, 1] [8, 17] `shouldBe` True
      versionBelow [8, 17] [8, 17, 0] `shouldBe` False
      versionBelow [3, 12] [3, 11] `shouldBe` False

  describe "findIsabelle" $ do
    let isaIn dir = do
          createDirectoryIfMissing True (dir </> "bin")
          writeFile (dir </> "bin" </> "isabelle") "#!/bin/sh\n"
          setFileMode (dir </> "bin" </> "isabelle") 0o755
    it "takes the one on PATH first" $ withTmp $ \tmp -> do
      isaIn (tmp </> "onpath")
      isaIn (tmp </> "home")
      findIsabelle [("PATH", tmp </> "onpath" </> "bin"), ("ISABELLE_HOME", tmp </> "home")]
        `shouldReturn` Just (tmp </> "onpath" </> "bin" </> "isabelle", True)
    it "then ISABELLE_HOME, off PATH" $ withTmp $ \tmp -> do
      isaIn (tmp </> "home")
      isaIn (tmp </> "Applications" </> "Isabelle2025-2.app")
      findIsabelle [("PATH", tmp), ("ISABELLE_HOME", tmp </> "home"), ("HOME", tmp)]
        `shouldReturn` Just (tmp </> "home" </> "bin" </> "isabelle", False)
    it "then ~/Applications/Isabelle2025-2.app" $ withTmp $ \tmp -> do
      isaIn (tmp </> "Applications" </> "Isabelle2025-2.app")
      findIsabelle [("PATH", tmp), ("HOME", tmp)]
        `shouldReturn` Just (tmp </> "Applications" </> "Isabelle2025-2.app" </> "bin" </> "isabelle", False)
    it "finds nothing when none is there (unless /Applications holds the app)" $ withTmp $ \tmp -> do
      r <- findIsabelle [("PATH", tmp), ("HOME", tmp)]
      r `shouldSatisfy` maybe True (\(p, onPath) -> not onPath && p == "/Applications/Isabelle2025-2.app/bin/isabelle")

  describe "detectProver" $ do
    let detectsAt name k = it ("detects " <> name) $ withTmp $ \tmp -> do
          let root = tmp </> "p"
          withFile root name "x\n"
          createDirectoryIfMissing True (root </> "sub" </> "deeper")
          detectProver (root </> "sub" </> "deeper") `shouldReturn` Right (Just (k, root))
    detectsAt "lakefile.lean" Lean4
    detectsAt "lakefile.toml" Lean4
    detectsAt "_RocqProject" Rocq
    detectsAt "_CoqProject" Rocq
    detectsAt "ROOT" Isabelle
    detectsAt "ROOTS" Isabelle

    it "detects a dune-project using coq or rocq" $ withTmp $ \tmp -> do
      withFile (tmp </> "a") "dune-project" "(lang dune 3.8)\n(using coq 0.8)\n"
      withFile (tmp </> "b") "dune-project" "(lang dune 3.21)\n  (using rocq 0.11)\n"
      detectProver (tmp </> "a") `shouldReturn` Right (Just (Rocq, tmp </> "a"))
      detectProver (tmp </> "b") `shouldReturn` Right (Just (Rocq, tmp </> "b"))

    it "ignores a dune-project that uses neither" $ withTmp $ \tmp -> do
      withFile (tmp </> "c") "dune-project" "(lang dune 3.8)\n(using menhir 2.1)\n; (using coq 0.8)\n(using coqx 1.0)\n"
      detectProver (tmp </> "c") `shouldReturn` Right Nothing

    it "takes an unreadable dune-project for no marker" $ withTmp $ \tmp -> do
      createDirectoryIfMissing True (tmp </> "e" </> "dune-project")
      detectProver (tmp </> "e") `shouldReturn` Right Nothing
      uid <- getEffectiveUserID
      if uid == 0
        then pendingWith "root reads a file whatever its mode"
        else do
          let f = tmp </> "f" </> "dune-project"
          withFile (tmp </> "f") "dune-project" "(using coq 0.8)\n"
          setFileMode f nullFileMode
          detectProver (tmp </> "f") `shouldReturn` Right Nothing

    it "puts an before Isabelle in the tie text" $
      tieMessage "/d" Lean4 Isabelle `shouldBe` "/d holds both a Lean 4 and an Isabelle project; pass --prover lean4|rocq|isabelle."

    it "ignores a directory named like a marker" $ withTmp $ \tmp -> do
      createDirectoryIfMissing True (tmp </> "d" </> "ROOT")
      detectProver (tmp </> "d") `shouldReturn` Right Nothing

    it "finds nothing outside any project" $ withTmp $ \tmp ->
      detectProver tmp `shouldReturn` Right Nothing

    it "stops at the nearest marker" $ withTmp $ \tmp -> do
      writeIsabelleRoot tmp
      writeRocqRoot (tmp </> "inner")
      createDirectoryIfMissing True (tmp </> "inner" </> "theories")
      detectProver (tmp </> "inner" </> "theories") `shouldReturn` Right (Just (Rocq, tmp </> "inner"))
      detectProver tmp `shouldReturn` Right (Just (Isabelle, tmp))

    it "refuses two kinds in one directory" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      writeRocqRoot tmp
      detectProver tmp `shouldReturn`
        Left (T.pack tmp <> " holds both a Lean 4 and a Rocq project; pass --prover lean4|rocq|isabelle.")

  describe "findProverRoot" $ do
    it "finds the nearest root of the kind asked, past other kinds" $ withTmp $ \tmp -> do
      writeRocqRoot tmp
      writeLeanRoot (tmp </> "lean")
      findProverRoot Rocq (tmp </> "lean") `shouldReturn` Just tmp
      findProverRoot Lean4 (tmp </> "lean") `shouldReturn` Just (tmp </> "lean")
      findProverRoot Isabelle (tmp </> "lean") `shouldReturn` Nothing
    it "skips the tie check" $ withTmp $ \tmp -> do
      writeLeanRoot tmp
      writeIsabelleRoot tmp
      findProverRoot Isabelle tmp `shouldReturn` Just tmp
      findProverRoot Lean4 tmp `shouldReturn` Just tmp
