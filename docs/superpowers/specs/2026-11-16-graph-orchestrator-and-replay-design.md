# The graph search on the Orchestrator, and recording a search for replay

Date: 16 Nov 2026. Status: for Tom's review. Stages 4 and 5 of the examination in the project note on a swarm monad transformer and a role-indexed agent monad, on top of stage 3 (docs/superpowers/specs/2026-11-15-orchestrator-design.md, built as docs/orchestrator.md describes). Tom chose a polymorphic record for stage 4 and file replay of the flat search for stage 5 (16 Nov 2026).

## 1. What and why

Stage 3 put the flat search's seven effects behind `Orchestrator` and moved its specs onto a simulator. The graph search (`Fisher.Search.GraphRun`) still runs its own pool: `startJob` is `async`, `loop` is `timeout` over `waitAnyCatch`, `stopAll` and the exception path are `cancel`, and only its worker attempts, its clock and its automation cap reach the record (through the `SearchEnv` it carries). Its jobs are not all attempts: a worker's attempt, a planner's split, a reviewer's verdict and a falsifier's answer are awaited together as one `Out`. So the record as it stands, fixed to `AttemptSpec` and `AttemptResult`, cannot hold a graph job, and the graph specs (`GraphRunSpec`, `GraphAutoSpec`, `GraphRetrievalSpec`, about 1700 lines with `GraphHarness`) still sleep and poll.

Stage 4 makes the record polymorphic in what it spawns and what comes back, gives the graph search its own orchestrator over its jobs, and moves the graph specs onto the simulator. Stage 5 adds a recording wrapper that turns any real run into a `Script`, a file format for the flat search's scripts, and `fisher search --record FILE` and `--replay FILE`. Nothing in either stage changes what a run does or logs.

## 2. The record, polymorphic (stage 4)

```haskell
newtype Running = Running Int
data Orchestrator spec res = Orchestrator
  { oSpawn    :: spec -> IO Running
  , oAwaitAny :: Double -> [Running] -> IO (Maybe (Running, Either SomeException res))
  , oCancel   :: Running -> IO ()
  , oNow      :: IO Double
  , oLog      :: Event -> IO ()
  , oCapped   :: forall a. Text -> Double -> IO a -> IO (Maybe a)
  , oPrice    :: spec -> IO (Maybe Double)
  }
type AttemptOrchestrator = Orchestrator AttemptSpec AttemptResult
```

The seven fields keep their names and contracts (docs/orchestrator.md). `oLog` stays on the flat search's `Event`: the graph search logs its own `GraphEvent`s through `geLogG` as it does today, and its orchestrator's `oLog` is unused, as its `SearchEnv`'s is today. (A log type parameter would be a third parameter for one unused field; not worth it.)

```haskell
realOrchestrator :: (spec -> IO res) -> IO Double -> (Event -> IO ()) -> (spec -> IO (Maybe Double)) -> IO (Orchestrator spec res)
simOrchestrator  :: (spec -> (Text, Text)) -> Script spec res -> IO (Orchestrator spec res, IO (SimTrace spec))

data Script spec res = Script
  { scAttempts :: [(AttemptKey, Double, Entry spec res)]
  , scDefault  :: Maybe (Entry spec res)
  , scCaps     :: [(Text, Double)]
  , scPrices   :: [(AttemptKey, Double)]
  , scStepCost :: Double
  }
data Entry spec res
  = Scripted (spec -> IO (Either Text res))
  -- ^ the result (or an exception's text) from the spec, computed at spawn; IO so a harness can record what was spawned
  | Run (spec -> IO res)
  -- ^ run this action at spawn, on the caller's thread, and deliver its result (or its exception) after the entry's seconds
data SimTrace spec = SimTrace { stSpawned :: [spec], stCancelled :: [Running], stDefaulted :: [spec], stEvents :: [Event], stClock :: Double }
```

The key function says how a spec is matched to a script entry: for attempts it is `\sp -> (asGoal sp, asBackend sp)`, exported as `attemptKey`; `akIndex` still counts earlier spawns with the same key. `Scripted` is stage 3's entry with IO added (stage 3's pure functions lift with `pure .`; the `SimHarness` helpers keep their signatures). `Run` is new: it exists so a graph scenario can keep the fake `geAsk` of today's harness for the planner, reviewer and falsifier jobs, which then run at spawn (the fake answers at once) and are delivered at their scripted virtual time, while a worker's entry is `Scripted` and never touches the worker's inner orchestrator. An exception from a `Run` action is caught at spawn and delivered as the `Left`, like a `Scripted` `Left`. Everything else in the simulator is as in stage 3, including list-order delivery of past-due attempts and the trace.

`autoStep` takes the cap function (`forall a. Text -> Double -> IO a -> IO (Maybe a)`) instead of an orchestrator, so either instantiation can pass its `oCapped`. `runToEnd :: AttemptOrchestrator -> AttemptSpec -> IO AttemptResult` is unchanged.

## 3. The graph search on its orchestrator (stage 4)

```haskell
data Job = Job { jNode :: NodeId, jName :: Text, jKind :: Kind }     -- jName added: the node's name, for the key
jobKey :: Job -> (Text, Text)                                      -- (name, "worker" | "plan" | "review" | "false")
data GraphEnv = GraphEnv
  { geSearch :: SearchEnv            -- the worker attempts' orchestrator (runToEnd), Lean, policy, paths, automation, retrieval
  , geOrch   :: Orchestrator Job Out -- the graph's jobs, clock and automation cap
  , geAsk, geLogG, geSave }           -- as today
```

In `GraphRun`: `startJob c n kind act` becomes `oSpawn (geOrch env) (Job n name kind)` where the real orchestrator's run function is `runJob :: Job -> IO Out`, built by the caller from the same pieces the jobs are built from today (`runToEnd (seOrch se) spec` for a worker; `runRole` of the planner's, reviewer's or falsifier's `AgentM` for the others). So the `Kind` carries what the job needs (`KWorker AttemptSpec Double` already does; `KPlan`, `KReview`, `KFalse` gain the inputs their `AgentM` program takes, which `startPlan`, `toReview` and `falseNode` compute today, and `runJob` runs them). `stRunning :: [(Running, Job)]`; the `timeout`/`waitAnyCatch` in `loop` becomes `oAwaitAny (geOrch env) left`; the `cancel`s in `stopAll` and `stopJobs` become `oCancel`; `elapsed` reads `oNow (geOrch env)`; `withAuto` passes `oCapped (geOrch env)`; `stopAll` prices a worker through `oPrice (geOrch env) job`, where the real run's price function is `\job -> case jKind job of KWorker spec _ -> price spec; _ -> pure Nothing` with the `priceCancelled` guard kept. The loop's logic, the whiteboards, the roles and the event stream do not change.

Callers: `fisher-cli/src/Fisher/Cli/Search.hs` builds the graph orchestrator with `realOrchestrator runJob now logE priceJob` next to the attempt one (same clock function); `GraphHarness` builds it the same way for the live specs.

## 4. The graph specs on the simulator (stage 4)

`GraphHarness.runG` builds `simOrchestrator jobKey script` for `geOrch`, where the harness's `Run` records (proof, delay, clock advance, cost, stop, failure) become `Scripted` worker entries with the delay as virtual seconds (the fake clock IORef goes: the simulator's clock is the run's clock), and every model-call job is a `Run` entry at the seconds the harness's `Reply` delay gave, over the unchanged fake `geAsk` (`Reply` matching by role and text stays). The inner `seOrch` is a simulator with an empty script, since no worker entry reaches it; a scenario that wants an exception from a worker scripts a `Left`. The bookkeeping the harness does in `seRunF` today (launched, briefs, starts, specs) happens in the worker entries' IO. `runGInterrupted` stops at a point the run reaches (an MVar in the fake ask or in a worker entry), as the Scheduler's interrupt tests do since stage 3. Every existing `it` in `GraphRunSpec`, `GraphAutoSpec` and `GraphRetrievalSpec` keeps its assertions, with real-time bounds made exact virtual values. `GraphLiveSpec` keeps real orchestrators. A graph identity test (`GraphIdentitySpec`) runs one scenario with two roots, a split and a cancelled worker through both constructors and compares the `GraphEvent` lists with times zeroed.

## 5. Recording and replay (stage 5)

```haskell
recordOrchestrator :: Orchestrator spec res -> IO (Orchestrator spec res, IO (Script spec res))
```

A wrapper over any orchestrator (a real one, in use): `oSpawn` records the spec, its key index and the clock; `oAwaitAny` records, for the attempt delivered, its seconds since spawn and its result (or the exception's text) as a `Scripted (const (pure ...))` entry; `oCancel` records the attempt with seconds `1/0` (never delivered: on replay it is cancelled before it is due, as it was); `oCapped` records a cap that hit under `scCaps` with the seconds it was given (a step that ran is not recorded: on replay the step runs again, which for Auto, Repair and Disprove means Lean runs again); `oPrice` records the price given. `scStepCost` is 0. The script read at the end is a scenario that, through `simOrchestrator`, replays the run: the same spawns, the same results in the same order, the same cancellations and budget cut, so the same events with virtual times in place of the clock's.

The flat search's scripts go to a file. `Fisher.Search.Attempt` gives `AttemptSpec`, `AttemptResult`, `ChildStats`, `Stop` and `ModelFailure` aeson instances (generic, field names as they are, in their own modules, no orphans), and `Fisher.Search.Orchestrate` gives `AttemptScript`, a serialisable view of `Script AttemptSpec AttemptResult` whose entries are constant results:

```haskell
data AttemptScript = AttemptScript { asEntries :: [(AttemptKey, Double, Either Text AttemptResult)], asCaps, asPrices }
toAttemptScript   :: Script AttemptSpec AttemptResult -> IO (Maybe AttemptScript)   -- Nothing when an entry is not constant
fromAttemptScript :: AttemptScript -> Script AttemptSpec AttemptResult
```

with `ToJSON`/`FromJSON` on `AttemptScript` and `AttemptKey`. The graph search gets the in-memory recorder only: its `Out` holds `Said` and `PromptPart` values that must not be built from a file (the role types' guarantee), so no `GraphScript` and no flags.

`fisher search --record FILE` wraps the real attempt orchestrator with `recordOrchestrator` and writes the script as JSON when the run ends (not on an interrupt: a cut run is not a scenario; the flag is refused with `--graph`). `fisher search --replay FILE` reads the script, runs the search through `simOrchestrator attemptKey` with the same configuration flags, prints the report and events as a run does, and writes no run log, ledger, lessons or `--apply` (a replay is not a run; the flags that would write are refused with `--replay`). Both are documented in docs/orchestrator.md.

## 6. Tests

- `SimOrchestrateSpec`: `Run` entries (delivered after their seconds; an exception is a `Left`; the action runs at spawn on the caller's thread, pinned by an IORef written inside it and read before the await).
- `RecordSpec` (fisher-search): a recorded real run of the flat search (the identity spec's scenario, fake run with scaled sleeps) replayed through the simulator gives the same events with times zeroed; a cancelled attempt in the record is cancelled on replay; a cap that hit is a cap on replay; `AttemptScript` JSON round-trips (`toAttemptScript . fromAttemptScript` and `decode . encode`).
- The graph specs as in section 4, plus `GraphIdentitySpec`.
- `fisher-cli` `SearchSpec`: `--record` writes a file that `--replay` reads to the same printed report; `--replay` with `--apply` or `--graph` is a usage error.
- Byte-identical goldens; every suite green.

## 7. Not in these stages

A type class or transformer over the record; the process swarm in `Fisher.Swarm`; a file format for graph scripts; replaying the native agent's conversations (that is `fisher replay`, unchanged); recording Lean or Lsp answers (a replay re-runs them).
