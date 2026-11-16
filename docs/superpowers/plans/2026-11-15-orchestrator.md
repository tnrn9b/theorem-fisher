# Orchestrator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Put the flat search Scheduler's seven effects behind one `Orchestrator` record with a real constructor and a scripted simulator, and move the Scheduler specs from sleeps to scripts.

**Architecture:** A new fisher-search module `Fisher.Search.Orchestrate` holds the record, `realOrchestrator` (today's `async`, `waitAnyCatch`, `timeout`, `cancel` code moved) and `simOrchestrator` (a virtual clock and a script). `SearchEnv` trades `seRun`, `seNow`, `seLog`, `sePrice` for `seOrch`; the Scheduler's logic and its event stream do not change. The six Scheduler specs then build a simulator instead of a fake run function with delays.

**Tech Stack:** Haskell (stack, LTS 22.44, hspec, async), RankNTypes for `oCapped`.

**Spec:** `docs/superpowers/specs/2026-11-15-orchestrator-design.md`

## Global Constraints

- No em or en dashes anywhere (code, comments, docs, commit messages); no references to Claude.
- The Scheduler's behaviour and event stream are unchanged: every existing golden stays byte-identical, and the existing Scheduler scenarios keep their assertions (only the way attempts are faked changes).
- The record's fields exactly: `oSpawn :: AttemptSpec -> IO Running`, `oAwaitAny :: Double -> [Running] -> IO (Maybe (Running, Either SomeException AttemptResult))`, `oCancel :: Running -> IO ()`, `oNow :: IO Double`, `oLog :: Event -> IO ()`, `oCapped :: forall a. Text -> Double -> IO a -> IO (Maybe a)`, `oPrice :: AttemptSpec -> IO (Maybe Double)`.
- The simulator never sleeps and never forks; an attempt the script does not cover and no default handles throws at spawn with the key in the message.
- Work happens on the Mac in the `monads` branch of `~/IdeaProjects/Fisher` through the connected folder and the watcher, as `.superpowers/sdd/2026-11-14-roles-and-provenance/HOWTO.md` describes (the same HOWTO applies). Commits with Tom's identity, never pushed.

## Decisions the spec leaves open

- Script entries carry a function `AttemptSpec -> Either Text AttemptResult`, not a fixed result, so today's fake runs (which build the output from `asStart`) port as they are; the spec's fixed result is the constant function. `Script` gains `scDefault :: Maybe (AttemptSpec -> Either Text AttemptResult)` for the tests that rely on today's "sorry when unscripted" default; without it an unscripted attempt throws, as the spec says.
- `AttemptKey`'s index counts earlier spawns for the same goal and backend (so a retry on the same backend is index 1), matching how the tests today key on `(goal, backend)` with one entry.
- The simulator's `oCapped` for a step in `scCaps` does not run the action (the spec); a step not listed runs the action and charges `scStepCost`.
- `realOrchestrator`'s handles live in an `IORef (Map Int (Async AttemptResult))`; `oCancel` on an unknown or finished `Running` is a no-op; `oAwaitAny` on an empty list returns Nothing at once without waiting.

## Review Focus

1. Two attempts due at the same virtual time: `oAwaitAny` must return one, and the next call the other, never lose one (Task 1 test "two attempts due together").
2. `oAwaitAny` with a deadline of 0 and an attempt already due: returns it (at or before) rather than Nothing (Task 1 test "a due attempt at deadline zero").
3. A cancelled attempt whose script entry is an exception: never delivered, never thrown (Task 1 test "a cancelled attempt that would have thrown").
4. The real orchestrator's `oAwaitAny` when the awaited thread is killed by `cancel` from another path: `waitAnyCatch` delivers the `AsyncCancelled` as a `Left`; the Scheduler already handles `Left` as an error record, and the test pins that (Task 1 test "a cancelled real attempt is a Left").
5. `oCapped` in the real orchestrator must rethrow asynchronous exceptions from the step (the Scheduler's own cancel) rather than turn them into Nothing (Task 1 test "capped rethrows async").

---

### Task 1: `Fisher.Search.Orchestrate`

**Files:**
- Create: `fisher-search/src/Fisher/Search/Orchestrate.hs`
- Test: `fisher-search/test/Fisher/Search/OrchestrateSpec.hs` (the real orchestrator, short real sleeps), `fisher-search/test/Fisher/Search/SimOrchestrateSpec.hs` (the simulator, no sleeps)

**Interfaces:**
- Consumes: `AttemptSpec`, `AttemptResult`, `Event` from `Fisher.Search.Scheduler` and `Fisher.Search.Report` (check where `Event` lives; import without creating a cycle: if `Scheduler` must import `Orchestrate`, the record must not import `Scheduler`; move `AttemptSpec` and `AttemptResult` into a small `Fisher.Search.Attempt` module re-exported by `Scheduler` if needed, and say so in the report).
- Produces, exactly as the spec's section 2 plus the decisions above: `Running`, `Orchestrator (..)`, `realOrchestrator :: (AttemptSpec -> IO AttemptResult) -> IO Double -> (Event -> IO ()) -> (AttemptSpec -> IO (Maybe Double)) -> IO Orchestrator`, `Script (..)` with `scAttempts :: [(AttemptKey, Double, AttemptSpec -> Either Text AttemptResult)]`, `scDefault`, `scCaps`, `scPrices`, `scStepCost`, `emptyScript`, `AttemptKey (..)`, `SimTrace (..)`, `simOrchestrator :: Script -> IO (Orchestrator, IO SimTrace)`.

- [ ] **Step 1: Write the failing tests.** SimOrchestrateSpec: "spawn then await returns the scripted result and moves the clock", "await past the deadline returns Nothing and moves the clock to the deadline", "two attempts due together", "a due attempt at deadline zero", "cancel removes an attempt and records it", "a cancelled attempt that would have thrown", "a capped step does not run and charges its seconds", "an unlisted step runs and charges scStepCost", "an unscripted attempt throws at spawn with its key", "scDefault covers an unscripted attempt", "prices come from the script", "the trace holds spawns, cancels, events and the clock". OrchestrateSpec: "await the first of two" (10 ms and 200 ms sleeps), "cancel the other", "a cap that hits returns Nothing", "a cancelled real attempt is a Left", "capped rethrows async" (throwTo the capped thread from outside; the exception escapes `oCapped`).
- [ ] **Step 2: Run** `stack test fisher-search` through the watcher: the new specs fail to compile.
- [ ] **Step 3: Implement the module.**
- [ ] **Step 4: Run** `stack test fisher-search`: 0 failures.
- [ ] **Step 5: Commit** "Orchestrate: the Scheduler's effects as a record, real and simulated".

### Task 2: the Scheduler on the record

**Files:**
- Modify: `fisher-search/src/Fisher/Search/Scheduler.hs` (`SearchEnv` gains `seOrch :: Orchestrator`, loses `seRun`, `seNow`, `seLog`, `sePrice`; `stRunning` holds `Running`; `async`, `waitAnyCatch`, `timeout`, `cancel` uses replaced per the spec's section 3; `autoStep` takes the orchestrator's `oCapped` with the step name), `fisher-cli/src/Fisher/Cli/Search.hs` (builds `realOrchestrator` from the four functions it passed; every `seNow env`/`seLog env` becomes `oNow (seOrch env)`/`oLog (seOrch env)`, or a local `now`/`logEv` binding), `fisher-search/src/Fisher/Search/GraphRun.hs` and `fisher-search/test/Fisher/Search/GraphHarness.hs` (the same four fields), the six Scheduler specs and `fisher-cli/test/Fisher/Cli/SearchSpec.hs` (minimal change: wrap today's `fakeRun` with `realOrchestrator`; the move to scripts is Task 3)
- Test: the existing suites

**Interfaces:**
- Consumes: Task 1.
- Produces: `SearchEnv { seOrch :: Orchestrator, seLean, sePolicy, seRoot, seRel, seBaseChanged, seAuto, seRetrieval }`; `runSearch` and `runSingle` signatures unchanged.

- [ ] **Step 1: Change the types**, following compile errors through fisher-search and fisher-cli; `autoStep`'s `timeout` becomes `oCapped step (max 1 left)`-equivalent with the step name the caller already passes to `withAuto`.
- [ ] **Step 2: Run** `stack test fisher-search` and `stack test fisher-cli`: 0 failures, no golden changed, every Scheduler scenario still green with the real orchestrator wrapping the old fakes.
- [ ] **Step 3: Commit** "Scheduler: spawn, await, cancel, clock, log and cap through the Orchestrator".

### Task 3: the Scheduler specs on the simulator

**Files:**
- Modify: `fisher-search/test/Fisher/Search/SchedulerSpec.hs`, `SchedulerAutoSpec.hs`, `SchedulerPolicySpec.hs`, `SchedulerRetrievalSpec.hs`, `SchedulerStopSpec.hs`, `SchedulerTierSpec.hs` (a shared helper `simSearch :: SearchConfig -> Script -> IO (SearchReport, [Event], SimTrace)` in a new `fisher-search/test/Fisher/Search/SimHarness.hs`; each scenario's `(proof, delay)` entry becomes a script entry with the delay as virtual seconds and the proof builder as the result function)
- Test: these files

**Interfaces:**
- Consumes: Tasks 1 and 2.
- Produces: `SimHarness.simSearch`, `SimHarness.proofAttempt :: Text -> AttemptSpec -> Either Text AttemptResult` (today's `fakeRun` body without the sleep).

- [ ] **Step 1: Port the scenarios** one spec file at a time, keeping every existing assertion; where an assertion read real elapsed time, read the trace's clock.
- [ ] **Step 2: Add the new scenarios** from the spec's section 4: "budget out with two in flight: both cancelled and priced, BudgetSpent logged", "cancel on close with a price", "a capped disprove step still launches the attempt and AutoTimeout names the step" (SchedulerAutoSpec, with the existing FakeLsp), "at or before the deadline", "a cancelled attempt that finishes anyway is never delivered"; and the byte-identity test from section 6: the same script through `simOrchestrator` and through `realOrchestrator` with a fake run that sleeps the script's seconds (scaled down to milliseconds), events compared with times removed.
- [ ] **Step 3: Run** `stack test fisher-search` three times in a row through one watcher script: 0 failures each time (no flakiness), and note the suite's wall time against the previous run in the report.
- [ ] **Step 4: Commit** "Scheduler specs: scripted scenarios on the simulator".

### Task 4: docs

**Files:**
- Create: `docs/orchestrator.md` (the record, the two constructors, how to write a scenario, what stage 4 and 5 would add)
- Modify: `docs/roles.md` (one pointer sentence), the project note is the controller's

- [ ] **Step 1: Write the page**, dash-free (the python one-liner).
- [ ] **Step 2: Commit** "docs: the Orchestrator and scripted scenarios".
