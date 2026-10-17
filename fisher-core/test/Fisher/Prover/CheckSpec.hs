module Fisher.Prover.CheckSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

import Fisher.Prover.Check
import Fisher.Prover.Holes (declarations)
import Fisher.Prover.Kind

fixture :: FilePath -> IO T.Text
fixture f = TIO.readFile ("test/fixtures/check/" <> f)

shouldReturn' :: T.Text -> IO T.Text -> Expectation
shouldReturn' x act = act >>= (x `shouldBe`)

spec :: Spec
spec = do
  describe "parseTarget" $ do
    it "splits FILE:decl at the last colon" $
      parseTarget "theories/A.v:good" `shouldBe` Target "theories/A.v" (Just "good")
    it "takes a bare file as every theorem in it" $
      parseTarget "theories/A.v" `shouldBe` Target "theories/A.v" Nothing
    it "keeps a qualified declaration" $
      parseTarget "A.v:M.inner" `shouldBe` Target "A.v" (Just "M.inner")

  describe "rocqFlags" $ do
    it "reads -Q, -R and -I with comments and file lines dropped" $
      rocqFlags
        "# the logical paths\n-Q theories Lib  # trailing comment\n-R src \"My Src\"\n-I plugins\n-arg -w -arg -notation-overridden\ntheories/A.v\n\n"
        `shouldBe` ["-Q", "theories", "Lib", "-R", "src", "My Src", "-I", "plugins"]
    it "lists the project's files" $
      rocqProjectFiles "-Q theories Lib\n# theories/Old.v\ntheories/B.v theories/A.v\n" `shouldBe` ["theories/B.v", "theories/A.v"]

  describe "rocqModule" $ do
    let fl = ["-Q", "theories", "Lib", "-R", "src", "Src"]
    it "names a file under a -Q mapping by its logical path" $
      rocqModule fl "theories/sub/A.v" `shouldBe` ("Lib.sub.A", True)
    it "names a file under -R" $ rocqModule fl "src/B.v" `shouldBe` ("Src.B", True)
    it "names an unmapped file by its base name" $ rocqModule fl "other/C.v" `shouldBe` ("C", False)
    it "takes ./ and an empty prefix" $ rocqModule ["-Q", ".", ""] "D.v" `shouldBe` ("D", True)

  describe "parseRocqDeps" $
    it "reads rocq dep lines into file dependencies" $
      parseRocqDeps
        "theories/B.vo theories/B.glob theories/B.v.beautified theories/B.required_vo: theories/B.v /opt/rocq9/lib/rocq-runtime/rocqworker\ntheories/A.vo theories/A.glob theories/A.v.beautified theories/A.required_vo: theories/A.v theories/B.vo /opt/rocq9/lib/rocq-runtime/rocqworker\n"
        `shouldBe` [("theories/B.v", []), ("theories/A.v", ["theories/B.v"])]

  describe "compileOrder" $ do
    let deps = [("A.v", ["B.v"]), ("B.v", ["C.v"]), ("C.v", []), ("D.v", [])]
    it "puts dependencies first and keeps only what the targets need" $
      compileOrder deps ["A.v"] `shouldBe` ["C.v", "B.v", "A.v"]
    it "survives a cycle" $ compileOrder [("A.v", ["B.v"]), ("B.v", ["A.v"])] ["A.v"] `shouldBe` ["B.v", "A.v"]

  describe "parseAssumptions (recorded live with Rocq 9.0)" $ do
    it "a closed theorem" $ (parseAssumptions <$> fixture "rocq-closed.txt") `shouldReturn` Right []
    it "one planted axiom" $ (parseAssumptions <$> fixture "rocq-axiom.txt") `shouldReturn` Right ["A.ax"]
    it "an Admitted theorem lists itself as an axiom" $
      (parseAssumptions <$> fixture "rocq-admitted.txt") `shouldReturn` Right ["A.adm"]
    it "a theorem ended with Abort is missing" $
      (parseAssumptions <$> fixture "rocq-missing-abort.txt") `shouldReturn` Left "missing"
    it "another error is reported" $
      (parseAssumptions <$> fixture "rocq-compile-error.txt")
        `shouldReturn` Left "Illegal application (Non-functional construction): (line 2 of \"./E.v\")"
    it "an Admitted theorem whose type is wrapped onto the next line (Rocq 9.1.1 on the Mac)" $
      (parseAssumptions <$> fixture "mac-rocq-admitted-wrapped.txt") `shouldReturn` Right ["Lists.rev_involutive'"]
    it "a wrapped entry is not taken for a section header" $
      parseAssumptions "Axioms:\nfoo :\n  nat\nSection Variables:\nx : nat\n" `shouldBe` Right ["foo", "x"]
    it "reads multi-line types and several sections" $
      parseAssumptions "Axioms:\nfoo : forall n : nat,\n      n = n\nbar : False\nSection Variables:\nx : nat\n"
        `shouldBe` Right ["foo", "bar", "x"]

  describe "the Rocq verdict" $ do
    it "an Admitted target is named as an axiom" $
      rocqProblems "Lib.A.adm" "adm" (Right ["A.adm"]) `shouldBe` ["adm is Admitted (axiom A.adm)"]
    it "the in-file form (Rocq 9.1 on the Mac) also names the target" $
      rocqProblems "Lib.A.adm" "adm" (Right ["adm"]) `shouldBe` ["adm is Admitted (axiom adm)"]
    it "an axiom of the same base name in another module is not the target" $
      rocqProblems "Lib.A.adm" "adm" (Right ["B.adm", "Lib.Other.adm"]) `shouldBe` ["axiom B.adm", "axiom Lib.Other.adm"]
    it "a planted axiom is named" $ rocqProblems "Lib.A.useax" "useax" (Right ["A.ax"]) `shouldBe` ["axiom A.ax"]
    it "a missing target says so" $ rocqProblems "Lib.A.gone" "gone" (Left "missing") `shouldBe` ["missing (not declared, or ended with Abort)"]
    it "a closed target has no problems" $ rocqProblems "Lib.A.good" "good" (Right []) `shouldBe` []
    it "flags pointing outside the copied project are made absolute" $
      relocateFlags "/r" ["-Q", "theories", "Lib", "-R", "../ext", "Ext", "-I", "plugins", "-Q", "/abs", "X"]
        `shouldBe` ["-Q", "theories", "Lib", "-R", "/r/../ext", "Ext", "-I", "/r/plugins", "-Q", "/abs", "X"]
    it "the check file requires the module and prints the qualified name" $
      rocqCheckFile "Lib.A" "good" `shouldBe` "Require Lib.A.\nPrint Assumptions Lib.A.good.\n"

  describe "Rocq on the Mac (Rocq 9.1, in-file Print Assumptions)" $
    it "reads the closed, Admitted, axiom and missing blocks in turn" $ do
      t <- fixture "mac-rocq-print-assumptions.txt"
      let ls = T.lines t
      parseAssumptions (T.unlines (take 1 ls)) `shouldBe` Right []
      parseAssumptions (T.unlines (take 2 (drop 1 ls))) `shouldBe` Right ["adm"]
      parseAssumptions (T.unlines (take 2 (drop 3 ls))) `shouldBe` Right ["ax"]
      parseAssumptions (T.unlines (drop 5 ls)) `shouldBe` Left "missing"

  describe "parseIsabelleBuild (recorded on the Mac, Isabelle2025-2)" $ do
    it "a passing build" $ (parseIsabelleBuild <$> fixture "isabelle-build-pass.txt") `shouldReturn` Right ()
    it "a sorry fails the strict build at its line" $
      (parseIsabelleBuild <$> fixture "isabelle-build-sorry.txt")
        `shouldReturn` Left "sorry at line 3 of ~/IdeaProjects/Fisher/.build/probe/isa/Probe.thy"
    it "lists every sorry" $
      isabelleSorries <$> fixture "isabelle-build-sorry.txt"
        `shouldReturn` [("~/IdeaProjects/Fisher/.build/probe/isa/Probe.thy", 3)]
    it "skips a sorry line whose number does not parse" $
      isabelleSorries "*** At command \"sorry\" (line x of \"A.thy\")\n" `shouldBe` []
    it "reports another error" $
      parseIsabelleBuild "Running A ...\nA FAILED (see also \"isabelle build_log -H Error A\")\n*** Type unification failed\n*** At command \"lemma\" (line 2 of \"A.thy\")\nUnfinished session(s): A\n"
        `shouldBe` Left "Type unification failed (line 2 of A.thy)"

  describe "parseOracles (recorded on the Mac)" $ do
    it "an oracle line" $ parseOracles "FISHER_ORACLES bad: Pure.skip_proof" `shouldBe` Right ["Pure.skip_proof"]
    it "a clean line" $ parseOracles "FISHER_OK good" `shouldBe` Right []
    it "a missing line" $ parseOracles "FISHER_MISSING ugly" `shouldBe` Left "missing"
    it "the build output's per-target results" $
      oracleResults <$> fixture "isabelle-oracles-skip-proof.txt" `shouldReturn` [("bad", Right ["Pure.skip_proof"])]
    it "an undefined fact is missing" $
      oracleResults <$> fixture "isabelle-undefined-fact.txt" `shouldReturn` [("nothere", Left "missing")]
    it "the generated check session's results, every target at once" $
      oracleResults <$> fixture "isabelle-check-session.txt"
        `shouldReturn` [("good", Right []), ("bad", Right ["Pure.skip_proof"]), ("ugly", Left "missing"), ("nothere", Left "missing")]
    it "the strict build with -c" $
      (parseIsabelleBuild <$> fixture "isabelle-build-sorry-strict.txt")
        `shouldReturn` Left "sorry at line 3 of ~/IdeaProjects/Fisher/.build/probe/isa/Probe.thy"
    it "several results in one error" $
      oracleResults "*** FISHER_OK good\n*** FISHER_ORACLES bad: Pure.skip_proof, Foo.bar\n*** FISHER_MISSING ugly\n*** \n*** At command \"ML\" (line 20 of \"/tmp/x/Fisher_Check.thy\")\n"
        `shouldBe` [("good", Right []), ("bad", Right ["Pure.skip_proof", "Foo.bar"]), ("ugly", Left "missing")]

  describe "the Isabelle verdict" $ do
    it "skip_proof is a sorry" $
      isabelleProblems (Right ["Pure.skip_proof"]) `shouldBe` ["oracle Pure.skip_proof (sorry)"]
    it "a missing target mentions oops" $
      isabelleProblems (Left "missing") `shouldBe` ["missing (not declared, or ended with oops)"]

  describe "the Isabelle check session" $ do
    it "the ROOT extends the target session" $
      isabelleCheckRoot "Probe" `shouldBe` "session Fisher_Check = \"Probe\" +\n  theories Fisher_Check\n"
    it "the theory imports the targets' theories and checks each" $ do
      let t = isabelleCheckTheory ["Probe.Probe"] [("Probe", "good"), ("Probe", "bad")]
      T.isInfixOf "theory Fisher_Check imports \"Probe.Probe\" begin" t `shouldBe` True
      T.isInfixOf "[(\"good\", [\"Probe.good\", \"good\"]), (\"bad\", [\"Probe.bad\", \"bad\"])]" t `shouldBe` True
      T.isInfixOf "Thm_Deps.all_oracles" t `shouldBe` True
    it "is byte for byte the theory verified on the Mac (isabelle-check-session.txt)" $
      (isabelleCheckTheory ["Probe.Probe"] [("Probe", n) | n <- ["good", "bad", "ugly", "nothere"]])
        `shouldReturn'` fixture "isabelle-check-theory.thy"
    it "build arguments: strict with -c, then the check, with quick_and_dirty only after a failed strict build" $ do
      isabelleStrictArgs "/r" "Probe" `shouldBe` ["build", "-c", "-o", "quick_and_dirty=false", "-d", "/r", "Probe"]
      isabelleCheckArgs False "/r" "/t" `shouldBe` ["build", "-c", "-o", "quick_and_dirty=true", "-d", "/r", "-d", "/t", "Fisher_Check"]
      isabelleCheckArgs True "/r" "/t" `shouldBe` ["build", "-c", "-o", "quick_and_dirty=false", "-d", "/r", "-d", "/t", "Fisher_Check"]

  describe "parseRootSessions" $ do
    it "a plain session" $
      parseRootSessions "session Probe = HOL +\n  theories Probe\n" `shouldBe` [RootSession "Probe" "" ["Probe"]]
    it "groups, in DIR, options, comments, quoted names and several sessions" $
      parseRootSessions
        "chapter AFP\n(* session Fake = HOL + theories Nope *)\nsession \"My_Sess\" (AFP) in \"src\" = \"HOL-Library\" +\n  description \\<open>a session\\<close>\n  options [timeout = 600, quick_and_dirty]\n  sessions \"HOL-Analysis\"\n  directories Sub\n  theories [document = false]\n    A\n    \"Sub/B\" (global)\n  theories C\n  document_files \"root.tex\"\nsession Two = My_Sess + theories D\n"
        `shouldBe` [RootSession "My_Sess" "src" ["A", "Sub/B", "C"], RootSession "Two" "" ["D"]]
    it "finds the session of a theory file" $ do
      let ss = [RootSession "My_Sess" "src" ["A", "Sub/B"], RootSession "Two" "" ["D"]]
      sessionFor ss "src/Sub/B.thy" `shouldBe` Just ("My_Sess", "B")
      sessionFor ss "D.thy" `shouldBe` Just ("Two", "D")
      sessionFor ss "E.thy" `shouldBe` Nothing

  describe "declarations (the file outline)" $ do
    it "Rocq theorems, with Module qualification and no Module Type ones" $
      map snd (declarations Rocq "Axiom ax : False.\nTheorem good : True. Proof. exact I. Qed.\nModule M.\nLemma inner : True. Proof. exact I. Qed.\nEnd M.\nModule Type T.\nAxiom t : True.\nLemma no : True. Admitted.\nEnd T.\nModule N := M.\nSection S.\nLemma sec : True. Proof. exact I. Qed.\nEnd S.\nTheorem gone : True. Abort.\n")
        `shouldBe` ["good", "M.inner", "sec", "gone"]
    it "Module Import and Module Export open a frame that End closes" $
      map snd (declarations Rocq "Module Import M.\nTheorem x : True. Proof. exact I. Qed.\nEnd M.\nModule Export N.\nLemma y : True. Proof. exact I. Qed.\nEnd N.\nTheorem top : True. Proof. exact I. Qed.\n")
        `shouldBe` ["M.x", "N.y", "top"]
    it "Declare Module opens no frame" $
      map snd (declarations Rocq "Module Type T. Axiom a : True. End T.\nDeclare Module D : T.\nModule O.\nDeclare Module E : T.\nLemma inner : True. Proof. exact I. Qed.\nEnd O.\nTheorem top : True. Proof. exact I. Qed.\n")
        `shouldBe` ["O.inner", "top"]
    it "Isabelle lemmas with lines" $
      declarations Isabelle "theory Probe imports Main begin\nlemma good: \"x + (0::nat) = x\" by simp\nlemma bad: \"x = (x::nat)\" sorry\nlemma ugly: \"True\" oops\nend\n"
        `shouldBe` [(2, "good"), (3, "bad"), (4, "ugly")]
    it "the declaration holding a line" $ do
      let ds = [(2, "good"), (3, "bad"), (4, "ugly")]
      declAt ds 3 `shouldBe` Just "bad"
      declAt ds 1 `shouldBe` Nothing

  describe "the report line" $ do
    it "ok" $ resultLine (CheckResult "A.v:good" True []) `shouldBe` "ok A.v:good"
    it "FAIL with problems" $
      resultLine (CheckResult "A.v:adm" False ["adm is Admitted (axiom A.adm)", "x"]) `shouldBe` "FAIL A.v:adm: adm is Admitted (axiom A.adm); x"
