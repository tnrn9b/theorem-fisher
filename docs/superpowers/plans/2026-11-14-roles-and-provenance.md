# Roles and Provenance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make prompt provenance and role capabilities structural: every prompt is assembled from `PromptPart`, a judge's text can only become a `Candidate`, and the planner, verifier and judge jobs run in a role-indexed monad whose operations are type-class constrained.

**Architecture:** A new fisher-core module `Fisher.Role` holds `Role`, `Said r`, `PromptPart` and `Candidate`; the existing prompt builders (lessons block, search brief, graph messages) change their types to use them with identical text. A new fisher-search module `Fisher.Role.Agent` holds `AgentM r`, `RoleCtx r` and the operations; the four model-call jobs move onto it and GraphRun and Mine build one context per role from the existing `Ask`.

**Tech Stack:** Haskell (stack, LTS 22.44, hspec), DataKinds and KindSignatures for the role index, the existing `sanitiseLesson` and `redactSensitiveText`.

**Spec:** `docs/superpowers/specs/2026-11-14-roles-and-provenance-design.md`

## Global Constraints

- No em or en dashes anywhere (code, comments, docs, commit messages).
- No behaviour change: every golden stays byte-identical (workflow prompts in `fisher-workflow/test/golden`, `fisher-cli/test/golden`, GraphRun and Scheduler goldens). A golden diff is a defect, not a thing to regenerate.
- `PromptPart` and `Candidate` constructors are not exported; `Said r` is constructed only by `Fisher.Role.Agent.ask` and `Fisher.Role.worker`.
- `AgentM r` exports no `MonadIO` instance and no way to run IO except the operations in the spec.
- `Promptable` has instances for `'Worker`, `'Planner`, `'Verifier` and none for `'Judge`; `CanCheck` has instances for `'Planner` and `'Verifier` only.
- Work happens on the Mac in the `monads` branch of `~/IdeaProjects/Fisher`. Files are edited through the connected folder (`$HOME/mnt/Fisher` in the shell: read with cat or sed, edit in place with a python read-modify-write, never re-type a file from tool output); builds, tests and git run on the Mac through the watcher: `bash tools/bw.sh run "zsh .build/NAME.sh < /dev/null"` then `bash tools/bw.sh wait 170` (repeat `wait` while it says STILL BUILDING), where `.build/NAME.sh` begins with `export PATH="$HOME/.local/bin:$HOME/.elan/bin:/opt/homebrew/bin:$PATH"; cd ~/IdeaProjects/Fisher`. Commits: `git add -A <paths> && git -c user.name="Tom Reynolds" -c user.email="thomas.nelson.reynolds@gmail.com" commit -q -m "MESSAGE"` in such a script; never push.
- Test commands: `stack test fisher-core`, `stack test fisher-search`, `stack test fisher-workflow`, `stack test fisher-cli` (grep the log for `examples,` and `Failures:`).

## Review Focus

1. A lesson row whose text passes `sanitiseLesson` but contains a `]` or a Rich markup fragment: `static` must not interpret it; the block's text must be byte-identical to today's (Task 2 test "block text unchanged on the lessons fixture").
2. A planner reply fed back on a retry that contains the literal string of the guard sentence or a `## Yours` heading: it stays inside the planner's own `toPrompt` part and does not become a lessons line (Task 6 test "a retry carries the previous reply as the planner's own part").
3. `candidate` on a judge text that redaction turns into an empty or whitespace string: `Nothing`, never an empty `Candidate` (Task 1 test "candidate refuses an empty redaction").
4. `check` called with a `Checker` whose lock is held by another thread: it waits rather than racing (Task 5 test "check runs under the lock").
5. `roleCtx` for `'Judge` built with `Just ck` by mistake: the type must forbid it or `rcCheck` must be ignored; the plan makes `roleCtx` take `Maybe Checker` only for roles with `CanCheck` (Task 5 interface and test "a judge context has no checker").

---

### Task 1: `Fisher.Role` (fisher-core)

**Files:**
- Create: `fisher-core/src/Fisher/Role.hs`
- Modify: `fisher-search/src/Fisher/Search/GraphJobs.hs` (remove its `data Role`, import and re-export `Role (..)` from `Fisher.Role`)
- Test: `fisher-core/test/Fisher/RoleSpec.hs`

**Interfaces:**
- Produces, exactly as the spec's section 2:
  - `data Role = Worker | Planner | Verifier | Judge deriving (Eq, Ord, Show, Enum, Bounded)`; `roleName :: Role -> Text` (`"worker"`, `"planner"`, `"verifier"`, `"judge"`)
  - `class KnownRole (r :: Role) where roleVal :: Proxy r -> Role` with the four instances
  - `newtype Said (r :: Role) = Said { saidText :: Text }` (constructor exported from an `Internal`-style explicit list only to `Fisher.Role.Agent`: export `unsafeSaid :: Text -> Said r` under the name `said'` from a hidden module `Fisher.Role.Internal`, re-exported by nothing else); `worker :: Text -> Said 'Worker`
  - `newtype PromptPart` (constructor hidden), `partText`, `static`, `user`, `source`, `tool`, `parts :: [PromptPart] -> PromptPart` (stripEnd each, drop those blank after strip, join with `"\n\n"`: the exact rule of `Fisher.Search.Retrieval.joinBrief`)
  - `type Prompt = [(Text, PromptPart)]` (a role, `"system"` or `"user"` or `"assistant"`, and its part)
  - `class Promptable (r :: Role) where toPrompt :: Said r -> PromptPart` with instances `'Worker`, `'Planner`, `'Verifier`
  - `newtype Candidate` (constructor hidden), `candidate :: Said r -> Maybe Candidate` (`redactSensitiveText` then `sanitiseLesson`; `Nothing` when the sanitiser refuses or the result is blank), `template :: Text -> Candidate`, `candidateText :: Candidate -> Text`

- [ ] **Step 1: Write the failing tests** in `RoleSpec.hs`: "roleName spells the run log's names", "parts follows joinBrief" (property over lists of strings against a local copy of joinBrief's rule), "candidate sanitises and redacts" (a text with a URL is refused; a text with `sk-or-abc...` is redacted the way `redactSensitiveText` does), "candidate refuses an empty redaction", "template keeps the text", "toPrompt keeps the text for a worker and a planner".
- [ ] **Step 2: Run to verify failure**: `stack test fisher-core` (module not found).
- [ ] **Step 3: Implement `Fisher.Role` and `Fisher.Role.Internal`**; move `Role` out of `GraphJobs` with a re-export so no caller changes.
- [ ] **Step 4: Run** `stack test fisher-core` and `stack build fisher-search`: 0 failures, builds.
- [ ] **Step 5: Commit** "Role: Said, PromptPart and Candidate, the provenance types".

### Task 2: the lessons block as a `PromptPart`

**Files:**
- Modify: `fisher-core/src/Fisher/Lessons/File.hs` (`renderLessonsBlock :: ([Text], [Text], [Text]) -> PromptPart`), `fisher-core/src/Fisher/Lessons/Prompt.hs` (`plBlock :: PromptPart`), `fisher-workflow/src/Fisher/Workflow/Plan.hs` (`lessonsBlockFor :: LaunchInputs -> FilePath -> IO PromptPart`; the assembly appends `partText`), `fisher-cli/src/Fisher/Cli/Search.hs` and `fisher-cli/src/Fisher/Cli/Mine.hs` where `plBlock` is hashed or logged (`partText` at the edge), `fisher-search/src/Fisher/Learn/Mine.hs` likewise
- Test: `fisher-core/test/Fisher/Lessons/FileSpec.hs`, `PromptSpec.hs`, `fisher-workflow/test` (existing; types only)

**Interfaces:**
- Consumes: Task 1.
- Produces: `plBlock :: PromptPart`, `lessonsBlockFor :: LaunchInputs -> FilePath -> IO PromptPart`. The rendered rows are `static` (they passed the sanitiser), `## Yours` lines are `user`, the heading and guard sentence are `static`.

- [ ] **Step 1: Add the test** "block text unchanged on the lessons fixture": render the existing FileSpec fixture rows and compare `partText` of the new result with the old `Text` result captured in the test as a literal (take it from the current test's expectation).
- [ ] **Step 2: Change the types**, following every compile error through fisher-core, fisher-workflow, fisher-search and fisher-cli.
- [ ] **Step 3: Run** `stack test fisher-core`, `stack test fisher-workflow`, `stack test fisher-cli`: 0 failures, every golden unchanged (the PlanGolden spec is the check).
- [ ] **Step 4: Commit** "Lessons: the prompt block is a PromptPart".

### Task 3: the search brief as a `PromptPart`

**Files:**
- Modify: `fisher-search/src/Fisher/Search/Retrieval.hs` (`rtBrief :: PromptPart`, `joinBrief` becomes `parts` and is removed or kept as an alias, `smtFacts :: Bool -> Maybe Text -> PromptPart` with `static`, `factsBrief :: [Fact] -> PromptPart` with `source`, `reExtra :: PromptPart`), `fisher-search/src/Fisher/Search/Library.hs` (`libraryBrief` returns `PromptPart`, `source`), `fisher-search/src/Fisher/Search/Scheduler.hs` (`asBrief :: PromptPart`; `suspectBrief` returns `PromptPart` with the counterexample as `tool`; `partText` where the brief is logged or handed to `seRun`), `fisher-search/src/Fisher/Search/Whiteboard.hs` (`workerBrief :: Text -> Text -> [Text] -> PromptPart -> PromptPart`; the board is a `PromptPart`), `fisher-search/src/Fisher/Search/GraphRun.hs` (`boardOf :: St -> NodeId -> PromptPart`; blueprints stored as `Said 'Planner` and placed with `toPrompt`), `fisher-cli/src/Fisher/Cli/Search.hs` (edges)
- Test: existing Retrieval, Scheduler, GraphRun, Whiteboard specs (types; `partText` in assertions)

**Interfaces:**
- Consumes: Task 1.
- Produces: `rtBrief :: PromptPart`, `asBrief :: PromptPart`, `boardOf :: St -> NodeId -> PromptPart`, `Plan.plBlueprint :: Said 'Planner` (the field type changes; `plLean` stays `Text` because it is checked Lean, placed in prompts as `source` after the kernel accepted it).

- [ ] **Step 1: Change the types**, following the compile errors; where a test asserted on the brief's `Text`, wrap with `partText`, never change the expected string.
- [ ] **Step 2: Run** `stack test fisher-search` and `stack test fisher-cli`: 0 failures, goldens unchanged.
- [ ] **Step 3: Commit** "Search: briefs and boards are PromptParts; a blueprint is what the planner said".

### Task 4: judge text as `Said 'Judge`, evidence as `Candidate`

**Files:**
- Modify: `fisher-search/src/Fisher/Learn/Judge.hs` (`Verdict { vMistake :: Said 'Judge, vLesson :: Maybe (Said 'Judge), vConfidence :: Double, vTellYourself :: Said 'Judge }`; `parseVerdict :: Said 'Judge -> Either Text Verdict` builds the fields from the reply with `said'`), `fisher-search/src/Fisher/Learn/Lessons.hs` (`evText :: Candidate`; `recordEvidence` no longer sanitises or redacts; `judgeKey` takes `candidateText`), `fisher-search/src/Fisher/Learn/Mine.hs` (`candidate` for the verdict's texts, `template` for rule lessons; the redaction at lines 454-455 goes, `candidate` does it; the `mistake` event's `text`, `lesson` fields take `saidText` after `redactSensitiveText`, as the event is a log, not a prompt)
- Test: `JudgeSpec`, `LessonsSpec`, `MineSpec` (existing; types), plus "a judge text with a URL yields no evidence row" in MineSpec if not already present

**Interfaces:**
- Consumes: Task 1.
- Produces: `Verdict` with `Said 'Judge` fields; `Evidence.evText :: Candidate`; `recordEvidence :: Runlog -> Double -> Evidence -> IO LessonRow` (no `Maybe`: refusal happened at `candidate`).

- [ ] **Step 1: Change the types**, following compile errors; keep the `mistake` event JSON identical (same keys, same redacted text).
- [ ] **Step 2: Run** `stack test fisher-search`: 0 failures; the lessons goldens unchanged.
- [ ] **Step 3: Commit** "Lessons: a judge's text is Said Judge, evidence is a Candidate".

### Task 5: `Fisher.Role.Agent` (fisher-search)

**Files:**
- Create: `fisher-search/src/Fisher/Role/Agent.hs`
- Test: `fisher-search/test/Fisher/Role/AgentSpec.hs`

**Interfaces:**
- Consumes: Task 1 (`Said`, `said'` from `Fisher.Role.Internal`, `Prompt`, `partText`, `KnownRole`), `Checker`, `locked`, `Ask`, `AskError`, `Asked` from `GraphJobs`, `probe` and `Probe` from `Fisher.Search.Check`.
- Produces, exactly:
  - `newtype AgentM (r :: Role) a` with `Functor`, `Applicative`, `Monad` only
  - `data RoleCtx (r :: Role) = RoleCtx { rcAsk :: [Message] -> IO (Either AskError (Text, Double)), rcCheck :: Maybe Checker, rcSpend :: Double -> IO (), rcEvent :: Text -> Value -> IO () }`
  - `ask :: Prompt -> AgentM r (Either AskError (Said r, Double))`: converts with `partText`, calls `rcAsk`, charges `rcSpend` with the dollars on success
  - `askTwice :: (Maybe (Said r, e) -> Prompt) -> (e -> Text) -> (Text -> IO (Either e a)) -> AgentM r (Asked a)`: the body of today's `GraphJobs.askTwice` over `ask`
  - `check :: CanCheck r => Text -> [Text] -> AgentM r Probe`: `locked ck (probe (ckLean ck) (ckRoot ck) (ckRel ck) text names)`; `rcCheck` is `Nothing` only for roles without `CanCheck`, so `check` errors with `error "check without a checker"` only on a programming mistake the type should prevent
  - `event :: Text -> Value -> AgentM r ()`
  - `runRole :: RoleCtx r -> AgentM r a -> IO a`
  - `roleCtx :: forall r. KnownRole r => Ask -> Maybe Checker -> (Role -> Double -> IO ()) -> (Role -> Text -> Value -> IO ()) -> RoleCtx r` (the `Ask` is applied to `roleVal (Proxy @r)`)
  - `class CanCheck (r :: Role)`; instances `'Planner`, `'Verifier`

- [ ] **Step 1: Write the failing tests**: "ask charges the role's dollars and names the role in events" (a fake `Ask` returning `("ok", 0.25)`; `rcSpend` records 0.25; an `event` call reaches `rcEvent` with role `"planner"`), "askTwice retries once on a refused reply and feeds it back" (the second prompt contains the first reply's text), "askTwice stops at NoCredit", "check runs under the lock" (a `Checker` whose `ckLock` is held for 200 ms by another thread; `check` returns after it; a counter shows no overlap), "a judge context has no checker" (`roleCtx @'Judge ask Nothing ...` builds; `rcCheck` is `Nothing`), "runRole is the only way in" (a compile-level fact stated in a comment; no test body).
- [ ] **Step 2: Run** `stack test fisher-search` to see the new spec fail to compile.
- [ ] **Step 3: Implement** the module.
- [ ] **Step 4: Run** `stack test fisher-search`: 0 failures.
- [ ] **Step 5: Commit** "Role.Agent: a role-indexed monad for the model-call jobs".

### Task 6: the jobs onto `AgentM`

**Files:**
- Modify: `fisher-search/src/Fisher/Search/GraphJobs.hs` (`planJob :: Text -> Text -> Text -> [Text] -> PromptPart -> [Text] -> PromptPart -> AgentM 'Planner (Asked Plan)`, `reviewJob :: Text -> Plan -> PromptPart -> AgentM 'Verifier (Asked SplitVerdict)`, `falseJob :: Text -> Text -> AgentM 'Verifier (Asked (Bool, Text))`; the sketch check inside `planJob` uses `check`; the old `askTwice` and `Ask`-taking signatures go), `fisher-search/src/Fisher/Search/Split.hs` and `Review.hs` (the message builders return `Prompt`; the previous reply is `toPrompt`), `fisher-search/src/Fisher/Learn/Judge.hs` (`judgeAttempt :: AttemptView -> AgentM 'Judge (Asked Verdict)`), `fisher-search/src/Fisher/Search/GraphRun.hs` (one `RoleCtx` per role built from `geAsk`, the checker and the existing cost and event writers; `runRole` at the three call sites), `fisher-search/src/Fisher/Learn/Mine.hs` (the judge's `RoleCtx` with `Nothing`; `runRole`)
- Test: GraphRunSpec, GraphJobs tests, JudgeSpec, MineSpec (existing; adapted), plus "a retry carries the previous reply as the planner's own part" (the second prompt's user part equals `toPrompt` of the first reply plus the problem text; no lessons line is touched)

**Interfaces:**
- Consumes: Tasks 1, 3, 4, 5.
- Produces: the four signatures above; `GraphRun.Ctx` gains `cRoles :: RoleCtxs` with `rcPlanner :: RoleCtx 'Planner`, `rcVerifier :: RoleCtx 'Verifier`; the per-role dollars reach `GraphReport` through the same accumulator the jobs' `Asked` costs reach today (no new field).

- [ ] **Step 1: Adapt the tests** to the new signatures (fake `Ask` wrapped by `roleCtx`); add the retry test.
- [ ] **Step 2: Move the jobs**, following compile errors.
- [ ] **Step 3: Run** `stack test fisher-search` and `stack test fisher-cli`: 0 failures; GraphRun goldens and the lessons goldens unchanged.
- [ ] **Step 4: Commit** "Jobs: planner, verifier and judge run in AgentM".

### Task 7: the negative compile check and the docs

**Files:**
- Create: `tools/test/typecheck-negative.sh`, `tools/test/negative/JudgePrompt.hs` (`toPrompt (x :: Said 'Judge)`), `tools/test/negative/JudgeCheck.hs` (`check` inside `AgentM 'Judge`)
- Modify: `tools/test.sh` (runs the script after the suites), `docs/learn.md` (one paragraph: a judge's text is a `Candidate` by type), new `docs/roles.md` (the types, the crossings table from the spec, how to add a job)

**Interfaces:** none new.

- [ ] **Step 1: Write the two snippets and the script**: `stack ghc -- -fno-code -isrc FILE` from `fisher-search` (and the core path) must fail with `No instance for (Promptable 'Judge)` and `No instance for (CanCheck 'Judge)`; the script exits 0 only when both fail with those messages.
- [ ] **Step 2: Run** the script: both snippets rejected.
- [ ] **Step 3: Write the docs**; check for dashes with the python one-liner from earlier plans.
- [ ] **Step 4: Commit** "Roles: negative compile check and docs".
