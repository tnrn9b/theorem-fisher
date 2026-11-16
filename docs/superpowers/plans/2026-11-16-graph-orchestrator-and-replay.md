# Graph Orchestrator and Replay Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `Orchestrator` polymorphic, run the graph search's jobs through its own orchestrator, move the graph specs onto the simulator, and record a flat search as a replayable script with a file format and CLI flags.

**Architecture:** `Fisher.Search.Orchestrate` becomes `Orchestrator spec res` with a key function for the simulator and a `Run` entry; `GraphRun` gets `geOrch :: Orchestrator Job Out` and spawns, awaits, cancels, reads the clock, caps and prices through it; `recordOrchestrator` wraps a real orchestrator into a `Script`; `AttemptScript` is its JSON view; `fisher search` gains `--record` and `--replay`.

**Tech Stack:** Haskell (stack, LTS 22.44, hspec, async, aeson).

**Spec:** `docs/superpowers/specs/2026-11-16-graph-orchestrator-and-replay-design.md`

## Global Constraints

- No em or en dashes anywhere; no references to Claude.
- The flat and graph searches' behaviour and event streams are unchanged; every golden byte-identical; every existing scenario keeps its assertions (real-time bounds may become exact virtual values).
- The record's field names and contracts stay as docs/orchestrator.md states them; `AttemptOrchestrator` is a synonym so stage 3's callers read as before.
- `Said` and `PromptPart` values are never built from a file: no `FromJSON` for them, no graph script file.
- Work happens on the Mac in the `monads` branch through the connected folder and the watcher, as `.superpowers/sdd/2026-11-15-orchestrator/HOWTO.md` describes (the same HOWTO applies). Commits with Tom's identity, never pushed.

## Decisions the spec leaves open

- `Entry`'s `Scripted` function is `spec -> IO (Either Text res)`; stage 3's `SimHarness` helpers keep their pure signatures and lift with `pure .`; `scriptEntry` returns the `Entry`.
- A `Run` action's exception at spawn is caught with `try @SomeException` (asynchronous exceptions rethrown) and delivered as the `Left`.
- `Kind` carries each job's inputs: `KPlan SplitId` plus the planner's arguments as a record `PlanInputs`, `KReview SplitId Plan` plus the reviewer's, `KFalse` plus the falsifier's (whatever `startPlan`, `toReview` and the false job's starter compute today); `runJob` lives in `GraphRun` as an exported `runGraphJob :: GraphEnv -> RoleCtxs -> Job -> IO Out` so the CLI and the harness do not repeat the dispatch (the role contexts are the run's; `runGraph` builds them, so the real orchestrator is built inside `runGraph` from a `GraphEnv` field `geRunJob :: (Job -> IO Out) -> IO (Orchestrator Job Out)`; the simulator ignores the run function). Record the final shape in the Task 2 report.
- The recorder's cancelled entry has seconds `1/0`; `toAttemptScript` writes it as the JSON number `null` is not allowed, so use the string "never" through a small newtype `Seconds` with its own instances.
- `--replay` runs with `scStepCost = 0` and prints what a run prints; it refuses `--apply`, `--graph`, `--out`, `--record` with a usage error.

## Review Focus

1. The polymorphic rewrite must leave `SchedulerSpec`, `SimOrchestrateSpec`, `OrchestrateSpec`, `SchedulerIdentitySpec` and the five other Scheduler specs green with no assertion changed (Task 1).
2. `Run` entries execute on the caller's thread at spawn; an exception in one is a `Left` delivered at the scripted time, never thrown from `oSpawn` (Task 1 test).
3. GraphRun's `loop` awaits `stRunning` in the same order as before, `stopAll`'s order of cancel, price, record, log is unchanged, and `stopJobs` on exception still cancels every job (Task 2).
4. The graph harness's `rClock` advance maps to the simulator's clock exactly (worker seconds = `rClock`, delay no longer a sleep), so `oEvents` times and `GraphReport` times are the old values (Task 3).
5. The recorder's replay reproduces cancellations: an attempt cancelled in the real run is never due on replay, and the budget cut lands at the same virtual time (Task 4 test).

---

### Task 1: the polymorphic record and `Run` entries

**Files:**
- Modify: `fisher-search/src/Fisher/Search/Orchestrate.hs` (`Orchestrator spec res`, `AttemptOrchestrator`, `attemptKey`, `Entry (..)`, `Script spec res`, `SimTrace spec`, `simOrchestrator :: (spec -> (Text, Text)) -> Script spec res -> IO (Orchestrator spec res, IO (SimTrace spec))`, `realOrchestrator` polymorphic, `scriptEntry` polymorphic), `fisher-search/src/Fisher/Search/Scheduler.hs` (`SearchEnv.seOrch :: AttemptOrchestrator`; `autoStep` takes the cap function `forall a. Text -> Double -> IO a -> IO (Maybe a)`; `runToEnd` on `AttemptOrchestrator`), `GraphRun.hs` (`withAuto` passes `oCapped`), `fisher-search/test/Fisher/Search/SimHarness.hs` (lift to `Scripted (pure . f)`; `simOrchestrator attemptKey`), the simulator and identity specs, `fisher-cli/src/Fisher/Cli/Search.hs` and any other caller (type-only changes).
- Test: `SimOrchestrateSpec` gains "a Run entry runs at spawn on the caller's thread and is delivered after its seconds" (an IORef written by the action, read before the await), "a Run entry's exception is a Left delivered at its time", "a Run entry's asynchronous exception is rethrown at spawn".

- [ ] **Step 1: Write the three failing tests.**
- [ ] **Step 2: Generalise the module**, following compile errors through fisher-search and fisher-cli; `stack test fisher-search` and `stack test fisher-cli` to 0 failures (914 and 1908 plus the new three).
- [ ] **Step 3: Commit** "Orchestrate: polymorphic in the job, with Run entries".

### Task 2: the graph search on its orchestrator

**Files:**
- Modify: `fisher-search/src/Fisher/Search/GraphRun.hs` (`Job` gains `jName`; `jobKey`; `Kind` carries inputs; `runGraphJob`; `GraphEnv` gains the orchestrator (see the decision above); `startJob`, `loop`, `stopAll`, `stopJobs`, `elapsed`, `withAuto`, the worker pricing on `geOrch`; the `async`, `cancel`, `waitAnyCatch`, `timeout` imports go), `fisher-cli/src/Fisher/Cli/Search.hs` (builds the graph orchestrator with the same clock function and `logE` as the attempt one, price from the ledger for workers), `fisher-search/test/Fisher/Search/GraphHarness.hs` (minimal: a real orchestrator over `runGraphJob`, the sleeps kept; the move to the simulator is Task 3), `GraphLiveSpec.hs`.
- Test: the existing graph suites, unchanged.

- [ ] **Step 1: Change the types** and follow compile errors; every graph spec green with the real orchestrator (`stack test fisher-search`, `stack test fisher-cli`).
- [ ] **Step 2: Commit** "GraphRun: jobs, clock, cap and pricing through the graph's Orchestrator".

### Task 3: the graph specs on the simulator

**Files:**
- Modify: `fisher-search/test/Fisher/Search/GraphHarness.hs` (`runG` family on `simOrchestrator jobKey`: worker `Run`s as `Scripted` entries with `rClock` as seconds and the bookkeeping inside, replies as `Run` entries over the fake `geAsk` with their delay as seconds, the inner `seOrch` an empty-script simulator, `rDelay` and the clock IORef removed; `runGInterrupted` on a reached point), `GraphRunSpec.hs`, `GraphAutoSpec.hs`, `GraphRetrievalSpec.hs` (assertions kept; real-time bounds made exact).
- Create: `fisher-search/test/Fisher/Search/GraphIdentitySpec.hs` (two roots, one split accepted, one worker cancelled at the budget: both constructors, `GraphEvent`s compared with times zeroed, `zeroG :: GraphEvent -> GraphEvent` over every constructor).
- Test: these files; `stack test fisher-search` three times in one watcher script, 0 failures each, the suite's time reported against the previous run.

- [ ] **Step 1: Port the harness**, then each spec file, keeping every `it`.
- [ ] **Step 2: Add `GraphIdentitySpec`.**
- [ ] **Step 3: Run three times**; commit "Graph specs: scripted scenarios on the simulator".

### Task 4: recording, and the attempt script's JSON

**Files:**
- Modify: `fisher-search/src/Fisher/Search/Orchestrate.hs` (`recordOrchestrator`, `AttemptScript (..)`, `Seconds`, `toAttemptScript`, `fromAttemptScript`, `ToJSON`/`FromJSON` for `AttemptScript`, `AttemptKey`, `Seconds`), `fisher-search/src/Fisher/Search/Attempt.hs` (`AttemptSpec`, `AttemptResult` instances; `PromptPart` in `asBrief` is written as its text and read back as `static`? No: `asBrief` is a prompt part; the JSON writes `partText` and reads it as `tool` text, which the replay never puts in a prompt because no attempt runs on replay; say so in a comment), `Child.hs` (`ChildStats`), `Controller.hs` (`Stop`, `ModelFailure`): generic instances, no orphans.
- Create: `fisher-search/test/Fisher/Search/RecordSpec.hs` (the identity scenario recorded through `recordOrchestrator` over a real orchestrator with scaled sleeps and replayed through the simulator: equal events with times zeroed; a cancelled attempt is in the record with "never" and is cancelled on replay; a cap that hit replays as a cap; `AttemptScript` round-trips through JSON and through `fromAttemptScript`/`toAttemptScript`).

- [ ] **Step 1: Write the failing tests.**
- [ ] **Step 2: Implement**; `stack test fisher-search` to 0 failures.
- [ ] **Step 3: Commit** "Orchestrate: record a run as a script; the attempt script's JSON".

### Task 5: `--record` and `--replay`

**Files:**
- Modify: `fisher-cli/src/Fisher/Cli/Search.hs` (`raRecord :: Maybe FilePath`, `raReplay :: Maybe FilePath`; parsing; `--record` wraps the attempt orchestrator and writes the file when the run ends normally; `--replay` reads the file, builds `simOrchestrator attemptKey (fromAttemptScript s)` with `scStepCost = 0`, runs the search with the given flags, prints the report and events, writes no run log, ledger, lessons or output file; usage errors for `--replay` with `--apply`, `--graph`, `--out` or `--record`, and `--record` with `--graph`), the usage text, `docs/orchestrator.md` (one section on the two flags).
- Test: `fisher-cli/test/Fisher/Cli/SearchSpec.hs`: `--record` to a temp file then `--replay` of it prints the same report lines; the usage errors.

- [ ] **Step 1: Write the failing tests** (the cli spec's existing fake-search fixtures show how a run is driven).
- [ ] **Step 2: Implement**; `stack test fisher-cli` to 0 failures, goldens unchanged.
- [ ] **Step 3: Commit** "fisher search: --record and --replay".

### Task 6: docs

**Files:**
- Modify: `docs/orchestrator.md` (the polymorphic record, `Run` entries, the graph's orchestrator and `jobKey`, the recorder, the attempt script file, the flags; "What is not here" rewritten: the class or transformer, the swarm, graph script files, Lean answers in a record), `docs/roles.md` (the pointer sentence still right).

- [ ] **Step 1: Write**, dash-free, and commit "docs: the graph search on the Orchestrator, recording and replay".
