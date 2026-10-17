# fisher check fixtures

Outputs of the real tools, read by `Fisher.Prover.CheckSpec`.

- `rocq-*.txt`: recorded live in the cloud on 2026-10-15 with Rocq 9.0.0
  (`/opt/rocq9/bin`), stdout and stderr together. Each is `rocq compile` of a
  generated `Fisher_Check.v` holding `Require Lib.A.` and
  `Print Assumptions Lib.A.<decl>.` against a module with a closed theorem
  (`closed`), one using a planted `Axiom` (`axiom`), one ended with
  `Admitted` (`admitted`) and one ended with `Abort` (`missing-abort`);
  `rocq-compile-error.txt` is a compile of a file with a type error.
- `mac-rocq-print-assumptions.txt`: Rocq 9.1 on the Mac (opam switch
  `fisher-rocq`), `rocq compile A.v` of a file running `Print Assumptions`
  in place on a closed theorem, an `Admitted` one, one using an axiom and a
  missing name.
- `mac-rocq-admitted-wrapped.txt`: Rocq 9.1.1 on the Mac, `Print
  Assumptions Live.Lists.rev_involutive'.` of an `Admitted` theorem whose
  type is too long for the line: Rocq prints `name :` and the type on the
  next line.
- `isabelle-*.txt`: recorded on the Mac on 2026-10-15 with Isabelle2025-2
  against the session `Probe` (`lemma good ... by simp`, `lemma bad ... sorry`,
  `lemma ugly ... oops`):
  - `isabelle-build-pass.txt`: `isabelle build -o quick_and_dirty=true`;
  - `isabelle-build-sorry.txt`: `isabelle build -o quick_and_dirty=false`;
  - `isabelle-build-sorry-strict.txt`: the same with `-c`, as `fisher check`
    runs it;
  - `isabelle-oracles-skip-proof.txt`: an earlier one-target check theory;
  - `isabelle-undefined-fact.txt`: `thm_oracles` on a missing name;
  - `isabelle-check-session.txt`: the check session `fisher check` generates
    (`Fisher.Prover.Check.isabelleCheckTheory`) on good, bad, ugly and a
    missing name.
  - `isabelle-check-theory.thy`: the check theory that produced
    `isabelle-check-session.txt`, as `isabelleCheckTheory` writes it.
