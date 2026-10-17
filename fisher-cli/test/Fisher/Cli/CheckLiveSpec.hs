-- | @fisher check@ run for real on a small Rocq project. Pending when
-- neither @rocq@ nor @coqc@ is on PATH (in the cloud, put /opt/rocq9/bin on
-- PATH; on the Mac, the opam switch @fisher-rocq@).
module Fisher.Cli.CheckLiveSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Cli.Check
import Fisher.Prover (ProverKind (..), binaryFix)
import Fisher.Prover.Check (CheckResult (..), resultLine)

copyProject :: FilePath -> IO ()
copyProject dst = do
  let src = "test/fixtures/rocq-proj"
  createDirectoryIfMissing True (dst </> "theories")
  forM_ ["_RocqProject", "theories/Base.v", "theories/A.v"] $ \f -> copyFile (src </> f) (dst </> f)

haveRocq :: IO Bool
haveRocq = (||) <$> (isJust <$> findExecutable "rocq") <*> (isJust <$> findExecutable "coqc")

withRocq :: (FilePath -> IO ()) -> IO ()
withRocq act = do
  yes <- haveRocq
  if not yes
    then pendingWith "neither rocq nor coqc is on PATH"
    else withSystemTempDirectory "fisher-check-live" $ \d -> copyProject d >> act d

lines' :: Either T.Text [CheckResult] -> [T.Text]
lines' = either (\e -> ["error: " <> e]) (map resultLine)

spec :: Spec
spec = do
  it "every theorem of the project: proved ok, Admitted and axiom FAIL, Abort missing" $ withRocq $ \d -> do
    r <- runCheck defaultCheckOptions d
    lines' r
      `shouldBe` [ "ok theories/A.v:proved"
                 , "FAIL theories/A.v:admitted: admitted is Admitted (axiom A.admitted)"
                 , "FAIL theories/A.v:uses_axiom: axiom A.planted"
                 , "FAIL theories/A.v:aborted: missing (not declared, or ended with Abort)"
                 ]
  it "named targets, from a subdirectory, with --prover" $ withRocq $ \d -> do
    r <- runCheck defaultCheckOptions {coProver = Just Rocq, coTargets = ["A.v:proved", "A.v:admitted", "A.v:nothere"]} (d </> "theories")
    lines' r
      `shouldBe` [ "ok A.v:proved"
                 , "FAIL A.v:admitted: admitted is Admitted (axiom A.admitted)"
                 , "FAIL A.v:nothere: missing (not declared, or ended with Abort)"
                 ]
  it "writes nothing into the project: no .vo, .vos, .vok, .glob or generated file" $ withRocq $ \d -> do
    _ <- runCheck defaultCheckOptions d
    (sort <$> listDirectory (d </> "theories")) `shouldReturn` ["A.v", "Base.v"]
    (sort <$> listDirectory d) `shouldReturn` ["_RocqProject", "theories"]
  it "leaves the user's own compiled files alone" $ withRocq $ \d -> do
    writeFile (d </> "theories/Base.vo") "the user's"
    _ <- runCheck defaultCheckOptions {coTargets = ["theories/A.v:proved"]} d
    readFile (d </> "theories/Base.vo") `shouldReturn` "the user's"
  it "names a proved theorem inside Module Import M as M.x" $ withRocq $ \d -> do
    writeFile (d </> "theories/Mods.v") "Module Import M.\nTheorem x : True. Proof. exact I. Qed.\nEnd M.\nModule Type T. Axiom a : True. End T.\nDeclare Module D : T.\nTheorem top : True. Proof. exact I. Qed.\n"
    r <- runCheck defaultCheckOptions {coTargets = ["theories/Mods.v"]} d
    lines' r `shouldBe` ["ok theories/Mods.v:M.x", "ok theories/Mods.v:top"]
  it "a file named alone that declares no theorem fails" $ withRocq $ \d -> do
    r <- runCheck defaultCheckOptions {coTargets = ["theories/Base.v"]} d
    lines' r `shouldBe` ["FAIL theories/Base.v: no theorems found"]
  it "a project file with no flags: a file Requires its sibling" $ withRocq $ \d0 -> do
    let d = d0 </> "flat"
    createDirectoryIfMissing True d
    writeFile (d </> "_CoqProject") "# no flags\n"
    writeFile (d </> "B.v") "Definition two := 2.\n"
    writeFile (d </> "A.v") "Require Import B.\nTheorem t : two = 2. Proof. reflexivity. Qed.\n"
    r <- runCheck defaultCheckOptions d
    lines' r `shouldBe` ["ok A.v:t"]
    (sort <$> listDirectory d) `shouldReturn` ["A.v", "B.v", "_CoqProject"]
  it "without rocq or coqc on PATH, says how to install them" $ withRocq $ \d -> do
    old <- lookupEnv "PATH"
    setEnv "PATH" "/nonexistent"
    r <- runCheck defaultCheckOptions d
    setEnv "PATH" (fromMaybe "" old)
    r `shouldBe` Left ("neither rocq nor coqc is on PATH; " <> binaryFix Rocq "coqc")
  it "a file that does not compile fails its targets and what depends on it" $ withRocq $ \d -> do
    writeFile (d </> "theories/Base.v") "Definition two := (I I).\n"
    r <- runCheck defaultCheckOptions {coTargets = ["theories/A.v:proved"]} d
    case r of
      Right [CheckResult _ False [p]] -> T.unpack p `shouldStartWith` "not compiled: theories/Base.v failed to compile"
      other -> expectationFailure (show other)
  it "a file outside any -Q mapping is checked under its base name" $ withRocq $ \d -> do
    createDirectoryIfMissing True (d </> "extra")
    writeFile (d </> "extra/Loose.v") "Theorem loose : True. Proof. exact I. Qed.\n"
    r <- runCheck defaultCheckOptions {coTargets = ["extra/Loose.v"]} d
    lines' r `shouldBe` ["ok extra/Loose.v:loose"]
  it "a dune project builds with dune and loads from _build" $ withRocq $ \d -> do
    dune <- findExecutable "dune"
    case dune of
      Nothing -> pendingWith "dune is not on PATH"
      Just _ -> do
        removeFile (d </> "_RocqProject")
        writeFile (d </> "dune-project") "(lang dune 3.8)\n(using coq 0.8)\n"
        writeFile (d </> "theories/dune") "(coq.theory\n (name Proj))\n"
        r <- runCheck defaultCheckOptions {coTargets = ["theories/A.v:proved", "theories/A.v:admitted"]} d
        lines' r
          `shouldBe` [ "ok theories/A.v:proved"
                     , "FAIL theories/A.v:admitted: admitted is Admitted (axiom A.admitted)"
                     ]
  it "a dune project builds with only rocq on PATH (no coqc, no coqdep): through the coqc and coqdep wrappers" $ withRocq $ \d -> do
    rocq <- findExecutable "rocq"
    dune <- findExecutable "dune"
    case (rocq, dune) of
      (Just r, Just _) -> do
        removeFile (d </> "_RocqProject")
        writeFile (d </> "dune-project") "(lang dune 3.8)\n(using coq 0.8)\n"
        writeFile (d </> "theories/dune") "(coq.theory\n (name Proj))\n"
        -- a PATH holding rocq, and dune with the OCaml tools it runs, but
        -- no coqc or coqdep (outside the project, where dune looks)
        withSystemTempDirectory "fisher-only-rocq" $ \bin -> do
          createFileLink r (bin </> "rocq")
          forM_ ["dune", "ocamlc", "ocamlc.opt", "ocamlopt", "ocamlopt.opt", "ocamlfind", "ocamldep", "ocamlrun"] $ \n ->
            findExecutable n >>= mapM_ (\p -> createFileLink p (bin </> n))
          old <- lookupEnv "PATH"
          setEnv "PATH" bin
          found <- (,) <$> findExecutable "coqc" <*> findExecutable "coqdep"
          r' <- runCheck defaultCheckOptions {coTargets = ["theories/A.v:proved", "theories/A.v:admitted"]} d
          setEnv "PATH" (fromMaybe "" old)
          found `shouldBe` (Nothing, Nothing)
          lines' r'
            `shouldBe` [ "ok theories/A.v:proved"
                       , "FAIL theories/A.v:admitted: admitted is Admitted (axiom A.admitted)"
                       ]
      _ -> pendingWith "rocq or dune is not on PATH"
  it "Isabelle: strict build, then one check session, against a fake isabelle replaying the Mac's outputs" $
    withSystemTempDirectory "fisher-check-isa" $ \d -> do
      let src = "test/fixtures/isabelle-proj"
      forM_ ["ROOT", "Probe.thy"] $ \f -> copyFile (src </> f) (d </> f)
      fake <- makeAbsolute (src </> "fake-isabelle")
      fixtures <- makeAbsolute "../fisher-core/test/fixtures/check"
      let logf = d </> "calls.log"
      setEnv "FAKE_ISABELLE_LOG" logf
      setEnv "FAKE_ISABELLE_FIXTURES" fixtures
      r <- runCheck defaultCheckOptions {coIsabelle = Just fake} d
      lines' r
        `shouldBe` [ "ok Probe.thy:good"
                   , "FAIL Probe.thy:bad: sorry at line 3; oracle Pure.skip_proof (sorry)"
                   , "FAIL Probe.thy:ugly: missing (not declared, or ended with oops)"
                   ]
      calls <- T.lines . T.pack <$> readFile logf
      root <- canonicalizePath d
      take 1 calls `shouldBe` [T.pack (root <> " build -c -o quick_and_dirty=false -d " <> root <> " Probe")]
      any ("theory Fisher_Check imports \"Probe.Probe\" begin" ==) calls `shouldBe` True
      any (T.isInfixOf "quick_and_dirty=true" ) calls `shouldBe` True
      fs <- listDirectory d
      sort fs `shouldBe` ["Probe.thy", "ROOT", "calls.log"]
  it "Isabelle: found off PATH where the launch and doctor look (ISABELLE_HOME)" $
    withSystemTempDirectory "fisher-check-isa" $ \d -> do
      let src = "test/fixtures/isabelle-proj"
      forM_ ["ROOT", "Probe.thy"] $ \f -> copyFile (src </> f) (d </> f)
      createDirectoryIfMissing True (d </> "isa" </> "bin")
      copyFile (src </> "fake-isabelle") (d </> "isa" </> "bin" </> "isabelle")
      getPermissions (d </> "isa" </> "bin" </> "isabelle") >>= setPermissions (d </> "isa" </> "bin" </> "isabelle") . setOwnerExecutable True
      fixtures <- makeAbsolute "../fisher-core/test/fixtures/check"
      let logf = d </> "calls.log"
      setEnv "FAKE_ISABELLE_LOG" logf
      setEnv "FAKE_ISABELLE_FIXTURES" fixtures
      old <- lookupEnv "PATH"
      oldHome <- lookupEnv "ISABELLE_HOME"
      setEnv "PATH" "/usr/bin:/bin"
      setEnv "ISABELLE_HOME" (d </> "isa")
      r <- runCheck defaultCheckOptions d
      setEnv "PATH" (fromMaybe "" old)
      maybe (unsetEnv "ISABELLE_HOME") (setEnv "ISABELLE_HOME") oldHome
      lines' r
        `shouldBe` [ "ok Probe.thy:good"
                   , "FAIL Probe.thy:bad: sorry at line 3; oracle Pure.skip_proof (sorry)"
                   , "FAIL Probe.thy:ugly: missing (not declared, or ended with oops)"
                   ]
      calls <- T.lines . T.pack <$> readFile logf
      length calls `shouldSatisfy` (> 1)
  it "Isabelle: a theory in no session fails" $
    withSystemTempDirectory "fisher-check-isa" $ \d -> do
      writeFile (d </> "ROOT") "session Probe = HOL +\n  theories Probe\n"
      writeFile (d </> "Other.thy") "theory Other imports Main begin\nlemma x: \"True\" by simp\nend\n"
      fake <- makeAbsolute "test/fixtures/isabelle-proj/fake-isabelle"
      r <- runCheck defaultCheckOptions {coIsabelle = Just fake, coTargets = ["Other.thy:x"]} d
      lines' r `shouldBe` ["FAIL Other.thy:x: Other.thy is in no session of ROOT"]
  it "Isabelle: --isabelle PATH that does not exist cannot run (exit 2)" $
    withSystemTempDirectory "fisher-check-isa" $ \d -> do
      writeFile (d </> "ROOT") "session Probe = HOL +\n  theories Probe\n"
      writeFile (d </> "Probe.thy") "theory Probe imports Main begin\nlemma t: \"True\" by simp\nend\n"
      r <- runCheck defaultCheckOptions {coIsabelle = Just "/nonexistent/isabelle"} d
      r `shouldBe` Left "--isabelle /nonexistent/isabelle: no such file"
  it "Isabelle: --isabelle takes a command name on PATH" $
    withSystemTempDirectory "fisher-check-isa" $ \d -> do
      let src = "test/fixtures/isabelle-proj"
      forM_ ["ROOT", "Probe.thy"] $ \f -> copyFile (src </> f) (d </> f)
      createDirectoryIfMissing True (d </> "bin")
      copyFile (src </> "fake-isabelle") (d </> "bin" </> "my-isabelle")
      getPermissions (d </> "bin" </> "my-isabelle") >>= setPermissions (d </> "bin" </> "my-isabelle") . setOwnerExecutable True
      fixtures <- makeAbsolute "../fisher-core/test/fixtures/check"
      setEnv "FAKE_ISABELLE_LOG" (d </> "calls.log")
      setEnv "FAKE_ISABELLE_FIXTURES" fixtures
      old <- lookupEnv "PATH"
      setEnv "PATH" (d </> "bin" <> ":/usr/bin:/bin")
      r <- runCheck defaultCheckOptions {coIsabelle = Just "my-isabelle", coTargets = ["Probe.thy:good"]} d
      missing <- runCheck defaultCheckOptions {coIsabelle = Just "no-such-isabelle"} d
      setEnv "PATH" (fromMaybe "" old)
      lines' r `shouldBe` ["ok Probe.thy:good"]
      missing `shouldBe` Left "--isabelle no-such-isabelle: not found on PATH"
  it "--isabelle is not looked at in a Rocq project" $ withRocq $ \d -> do
    r <- runCheck defaultCheckOptions {coIsabelle = Just "/nonexistent/isabelle", coTargets = ["theories/A.v:proved"]} d
    lines' r `shouldBe` ["ok theories/A.v:proved"]
  it "parses its arguments" $ do
    parseCheckArgs ["--prover", "isabelle", "--isabelle", "/x/isabelle", "A.thy:foo"]
      `shouldBe` Right (Just defaultCheckOptions {coProver = Just Isabelle, coIsabelle = Just "/x/isabelle", coTargets = ["A.thy:foo"]})
    parseCheckArgs ["--help"] `shouldBe` Right Nothing
    parseCheckArgs ["--prover", "coq"] `shouldBe` Left "unknown prover coq; use lean4, rocq or isabelle"
