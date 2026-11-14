# SMT: the `smt` tactic in the pre-pass, countermodels before spending, and an `smt_check` tool

Date: 25 Oct 2026. Status: approved by Tom in conversation. (Restored on 14 Nov 2026 from the session that wrote it; the original commit lived only in a cloud mirror.)

## 1. What and why

Fisher has no SMT solver anywhere. Three jobs want one: closing the decidable goals a Lean project throws up (linear integer and real arithmetic, bitvectors, uninterpreted functions, quantifier-free logic over them) without a model call; finding out that a goal or a proposed lemma is false before attempts are spent on it; and giving the agent a solver to test conjectures and side conditions while it proves.

Decided with Tom (25 Oct 2026):

- All three jobs, in one design.
- lean-smt is the single bridge to Lean goals: its `smt` tactic closes goals, and its translation (`smt_show`) produces the SMT-LIB query Fisher sends to a standalone solver. Fisher writes no Lean-to-SMT translator of its own.
- A project gets lean-smt by detection and offer: Fisher uses `smt` only when the project's `lake-manifest.json` lists the `smt` package; `fisher project add smt` adds it, and the `lean-mathlib` template carries it by default.
- The standalone solver is `z3`, with `cvc5` as the fallback, from PATH.
- A model for a goal's negation is confirmed in Lean before it changes anything: a confirmed negation skips the goal; an unconfirmed model is a hint in the brief and the attempts run.

Facts the design rests on (Reservoir, 25 Oct 2026): the require line is `require smt from git "https://github.com/ufmg-smite/lean-smt.git" @ "main"` (`lakefile.toml`: `[[require]] name = "smt" scope = "ufmg-smite" rev = "main"`); `import Smt` brings the tactic, `import Smt.Real` real arithmetic; `main` depends on Mathlib and a `no_mathlib` branch exists; cvc5 comes through the lean-cvc5 FFI package, so the tactic needs no solver on PATH; Reservoir lists the commits that build on each Lean version.

## 2. The solver runner and the dependency

### 2.1 `Fisher.Smt` (fisher-core)

Everything that runs a solver on SMT-LIB text:

- `data Solver = Z3 FilePath | Cvc5 FilePath`; `findSolver :: SmtConfig -> IO (Maybe Solver)`: config `fisher.smt.solver` is `z3`, `cvc5` or `auto` (default); `auto` takes `z3` then `cvc5` from PATH.
- `data SmtResult = Unsat | Sat Model | Unknown Text | SolverError Text`; `Model = [(Text, Sort, Value)]` parsed from `(get-model)` output: the value kept as text, plus a decoded `VInt Integer`, `VBool Bool` or `VBitVec Int Integer` when the sort is `Int`, `Bool` or `(_ BitVec w)`; anything else (an uninterpreted function, a real, an array) is `VOther`.
- `runQuery :: Solver -> Int -> Text -> IO SmtResult` (the `Int` is the wall cap in seconds): writes the query to a temp file, runs `z3 -smt2 -T:N FILE` or `cvc5 --tlimit=N000 --produce-models FILE` with stdin from `/dev/null`, caps the output at 64 KB, kills the process at the cap plus one second, and classifies the first result line (`sat`, `unsat`, `unknown`, `timeout`, anything else is `SolverError` with the first 2 KB of stderr). The query never goes on argv.
- `prepareQuery :: Text -> (Text, [Text])`: appends `(check-sat)` when missing and `(get-model)` when models are wanted and missing; removes `set-option` lines that name a path or an output channel and returns them as notes.
- Config: `fisher.smt.enabled` (default true; false turns off the arm, the countermodels and the tool), `fisher.smt.solver`, `fisher.smt.timeout_secs` (default 10, cap 60).

### 2.2 `fisher doctor`

An `SMT` line: the solver found and its version (`z3 4.13.4` or `none`), and for the current project whether lean-smt is present (`lean-smt: in lake-manifest.json` or `not required; fisher project add smt`). `--fix` offers `brew install z3` on macOS and prints the apt line elsewhere; it never edits the project.

### 2.3 `fisher project add smt`

`fisher project add smt [DIR] [--rev REV] [--no-build] [--yes]`:

1. Refuses when the lakefile already has a `smt` require or the manifest lists it (exit 0 with "already present").
2. Picks the revision: `--rev` wins; else Reservoir's build list for the package, the newest commit that built on the project's `lean-toolchain`; else (offline, or no build for that version) `main` with a warning. A project without Mathlib in its manifest gets the `no_mathlib` branch instead of `main`.
3. Appends the require line to `lakefile.lean` or the `[[require]]` block to `lakefile.toml`.
4. Unless `--no-build`: `lake update smt`, `lake build`, then the sample check `example : ∀ x : Int, x + 0 = x := by smt` in a scratch file with `import Smt`, through the same check `fisher project create` uses; prints `Ready: smt` or the failure.
5. Output in `.fisher/add-smt.log`. A failed build leaves the lakefile edited and says which line to delete to revert.

`fisher project create` and `fisher doctor` mention the command when the project has no lean-smt. The `lean-mathlib` template gains the require line (its sample check gains `by smt` on the example above), so a scratch base built from it has lean-smt; the plain `lean` template is unchanged.

## 3. The `smt` pre-pass arm

- `defaultTactics` becomes `["rfl", "decide", "omega", "simp_all", "aesop", "grind", "smt", "exact?"]`.
- `Fisher.Search.Auto` gates the arm: `hasSmt :: FilePath -> IO Bool` reads the project's `lake-manifest.json`; without lean-smt, or with `fisher.smt.enabled: false`, `smt` is removed from the portfolio before the pre-pass runs. The `portfolio` event records the list used, so the learned policy and `fisher learn stats` see `smt` only where it ran.
- The pre-pass scratch copy gets `import Smt` prepended when the file lacks it and the arm is on; `--apply` adds the import to the written file only when a kept proof uses `smt`, and the report says so.
- The learned policy orders `smt` among the arms like any other; no special case.
- Agents learn of it through the project facts in the brief (`Fisher.Search.Facts`), one section present only when the project has lean-smt: "`smt` (lean-smt, cvc5) is available: `import Smt`, `by smt`, `by smt [h₁, h₂]` to pass hypotheses, `smt +mono` for polymorphic goals; it covers linear integer and real arithmetic, bitvectors, uninterpreted functions and quantifier-free logic over them." When a standalone solver is on PATH the same section adds "`z3` is on PATH for SMT-LIB queries." The fetched Lean skill and the workflow prompts are not edited.

## 4. Countermodels in the disproof stage

`Fisher.Search.Disprove` gains a second source beside `plausible`:

1. For a goal in a project with lean-smt, the stage runs `smt_show` at the hole through the `Lsp` and reads the query from the diagnostics. If the pinned revision lacks `smt_show`, the fallback is `set_option trace.smt.translate.query true in smt`, whose trace message carries the same query; the live check on the Mac decides which the code uses by default, and both parsers are written.
2. `prepareQuery` adds `(check-sat) (get-model)`; `runQuery` runs it under `fisher.smt.timeout_secs`.
3. `Unsat`: a `disprove` event `{source: smt, result: unsat}`, nothing else (closing goals is the arm's job). `Unknown` and `SolverError`: logged, ignored. `Sat model`: a `DSuspect` carrying the model text.
4. Certification runs as today, with one new tactic tried first when the model allows it: `push_neg; refine ⟨v₁, …, vₙ, ?_⟩; first | decide | norm_num | omega | simp_all`, where the vᵢ are the model's values for the statement's leading bound variables in order, rendered as Lean literals (`(-3 : Int)`, a `Nat` from a non-negative integer, `true`/`false`, `0x1f#8`). A model that does not cover those variables, or whose relevant values are `VOther`, gets no witness tactic and the existing list runs alone.
5. Only a certified negation is `DFalse` (no attempts, reported as false with the witness). A model that fails certification stays `DSuspect`, which the brief already prints as a possible counterexample.

Order and cost: `plausible` first, SMT second, each under its own cap; both are skipped on a goal's second and later attempts. The graph planner's proposed lemmas go through the same `disprove` call before they are split off; a false lemma is dropped with its witness in `report.json`. The report's `disproved` entries gain `source` (`plausible` or `smt`) and `witness`.

## 5. The `smt_check` tool

In fisher-tools, registered for the native agent only (Claude Code and Codex have their own shell and are told `z3` is on PATH through the facts section):

- Arguments: `query` (SMT-LIB text, at most 64 KB), `solver` (`z3`, `cvc5`, or omitted for the configured one), `timeout_secs` (default `fisher.smt.timeout_secs`, never above the cap), `models` (default true).
- Runs `prepareQuery` then `runQuery` under the Python tool's sandbox (no network, a temp directory, the solver resolved once when the tools are built).
- Returns `{result: sat|unsat|unknown|error, model: [{name, sort, value}], notes: [stripped options], stderr: first 2 KB, seconds}`.
- Calls are ordinary `tool_start`/`tool_end` run-log events. `Fisher.Learn.Mistakes` gains `smt_unknown_loop`: three or more consecutive `smt_check` calls returning `unknown` or `error` (timeout) in one attempt; template: "The solver cannot decide this shape; cut the query down or prove this part by hand."

## 6. Errors

No solver on PATH: the tool and the countermodel source are off, one warning per run; the arm is independent of it (lean-smt brings cvc5). A solver crash, timeout or unreadable output is `SolverError`/`Unknown`, never an exception out of a stage. `smt_show` producing no query (a goal outside the fragment) is logged and the goal goes on as before. `fisher project add smt` failing leaves the lakefile edited, the log in `.fisher/add-smt.log` and the revert line printed. `fisher.smt.enabled: false` turns the whole stage off.

## 7. Testing

- `Fisher.Smt` with a fake solver script: sat with a model, unsat, unknown, timeout, garbage, output over 64 KB; `prepareQuery` (missing commands added, path options stripped and noted); model parsing for `Int`, `Bool`, `BitVec`, a real and an uninterpreted function.
- The witness tactic renderer (literals per sort; no tactic when a variable is uncovered or `VOther`).
- `defaultTactics` gating on manifests with and without `smt`; the `import Smt` prepend and the `--apply` import rule.
- `smt_show` diagnostics parsing and the trace fallback.
- Disprove with a fake `Lsp` and fake solver: a model certified to `DFalse` by the witness tactic; a spurious model stays `DSuspect`; `unsat` logged only; the second attempt skips; a graph lemma dropped.
- The tool: argument checks, the cap, stripped options, the result shape; `smt_unknown_loop` positive and negative.
- `fisher project add smt` on fixture `lakefile.lean` and `lakefile.toml`: the line added, idempotence, the `no_mathlib` choice, `--rev`, the revert message on a failed build (fake lake).
- `fisher doctor` lines with and without a solver.
- Live on the Mac: `brew install z3`; `fisher project add smt` on a scratch; a search on a true arithmetic goal that `smt` closes in the pre-pass; a search on `∀ x y : Int, x * y = x + y` disproved with a witness; `fisher runs show` listing an `smt_check` call from the native agent.

## 8. Not in this version

Rocq and Isabelle (both have their own SMT bridges: `smtcoq`, `sledgehammer`); real arithmetic in the witness tactic (`Smt.Real` works in the arm, a real-valued model gets no witness); a translator of Fisher's own; lean-smt proof reconstruction settings; running `smt` from the web app's chat.

## 9. As built (added 14 Nov 2026)

What the live checks changed after approval, all in `docs/smt.md`: the smt require goes before Mathlib's and the template's `smt` variable resolves to a Reservoir commit for the Mathlib release or to `none` (no block) when there is none; every kernel check runs `lake lean FILE` because `lake env lean` does not load lean-smt's cvc5 dynlib; `smt_show`'s query names constants with inaccessible names (`|x✝|`), so the witness renderer strips them and falls back to declare-const order; the early (pre-attempt) SMT step certifies with the witness tactic only and the late step runs the plain tactics; `prepareQuery` applies an allowlist of SMT-LIB commands and the solver runs in a fresh temp directory; `smt_check` runs in process rather than in the shell sandbox.
