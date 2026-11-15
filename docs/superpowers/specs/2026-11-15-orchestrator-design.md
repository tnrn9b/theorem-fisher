# The Orchestrator: the Scheduler's effects behind one record, with a scripted simulator

Date: 15 Nov 2026. Status: for Tom's review. Stage 3 of the examination in the project note on a swarm monad transformer and a role-indexed agent monad; stages 4 (GraphRun) and 5 (recording and replay) are not in this spec. Tom chose the record shape over a class and transformer (14 Nov 2026).

## 1. What and why

`Fisher.Search.Scheduler.runSearch` orchestrates the flat search: it spawns attempts with `async (seRun env spec)`, waits for the first to finish with `waitAnyCatch` under a `timeout` of the budget left, cancels the rest when a goal closes or the budget runs out, reads the clock through `seNow`, writes events through `seLog`, caps each automation step with `timeout` (`autoStep`), and prices a cancelled attempt through `sePrice`. Those seven effects are spread through 1100 lines of IO, and the only way to test an ordering rule (the early SMT disproof, `gsSmtTried`, the budget cut, cancel-on-close) is a spec with real threads and `threadDelay`, which is slow and timing-sensitive.

This stage gathers the seven effects into one record, `Orchestrator`, with two constructors: the real one (threads, the wall clock, the run log) and a simulator driven by a script (which attempt finishes next, with what result, after how much virtual time; no threads, no sleeps, deterministic). The Scheduler's logic does not change; the Scheduler specs move from delays to scripts.

## 2. The record (fisher-search, new module `Fisher.Search.Orchestrate`)

```haskell
newtype Running = Running Int          -- an attempt in flight, opaque to the Scheduler
data Orchestrator = Orchestrator
  { oSpawn    :: AttemptSpec -> IO Running
  , oAwaitAny :: Double -> [Running] -> IO (Maybe (Running, Either SomeException AttemptResult))
    -- ^ the first of these to finish, or Nothing when the given seconds pass first
  , oCancel   :: Running -> IO ()
  , oNow      :: IO Double
  , oLog      :: Event -> IO ()
  , oCapped   :: Text -> Double -> IO a -> IO (Maybe a)
    -- ^ run a named step under a wall cap in seconds; Nothing when the cap hits
    -- (the step name is for the simulator's script and the AutoTimeout event)
  , oPrice    :: AttemptSpec -> IO (Maybe Double)
  }
```

`oCapped`'s rank-2 field needs `RankNTypes`; the alternative, a monomorphic `IO ()` cap, would not cover `autoStep`'s result-carrying actions.

### 2.1 `realOrchestrator :: (AttemptSpec -> IO AttemptResult) -> IO Double -> (Event -> IO ()) -> (AttemptSpec -> IO (Maybe Double)) -> IO Orchestrator`

The behaviour the Scheduler has today, moved: `oSpawn` is `async` on the run function with the handle kept in a `Map Int (Async AttemptResult)`; `oAwaitAny` is `timeout` over `waitAnyCatch` of the named handles; `oCancel` is `cancel`; `oNow`, `oLog`, `oPrice` are the given functions; `oCapped` is `System.Timeout.timeout` with the same `max 1 (ceiling (secs * 1000000))` rounding as `autoStep`. Cancelling a `Running` that already finished is a no-op; awaiting an empty list returns Nothing at once.

### 2.2 `simOrchestrator :: Script -> IO (Orchestrator, IO SimTrace)`

```haskell
data Script = Script
  { scAttempts :: [(AttemptKey, Double, Either Text AttemptResult)]
    -- ^ per spawned attempt (matched in order by goal, backend and the count of
    -- earlier spawns for that goal): the virtual seconds it takes, and its result or
    -- an exception's text
  , scCaps     :: [(Text, Double)]
    -- ^ named steps that hit their cap, each with the virtual seconds consumed
  , scPrices   :: [(AttemptKey, Double)]
  , scStepCost :: Double
    -- ^ virtual seconds every other oCapped step and every oAwaitAny poll consumes (default 0)
  }
data AttemptKey = AttemptKey { akGoal :: Text, akBackend :: Text, akIndex :: Int }
data SimTrace = SimTrace
  { stSpawned :: [AttemptSpec], stCancelled :: [Running], stEvents :: [Event], stClock :: Double }
```

Virtual time starts at 0. `oSpawn` records the spec, looks up its script entry (an attempt the script does not name is a programming error in the test and throws at spawn) and marks it due at `now + seconds`. `oAwaitAny secs rs` finds the earliest due attempt among `rs`: if its due time is at or before `now + secs`, the clock moves to the due time and the attempt's result (or the exception) is returned; else the clock moves to `now + secs` and Nothing is returned. `oCancel` removes the attempt from the due list and records it. `oCapped name secs act`: when `name` is in `scCaps`, the clock advances by the listed seconds (or `secs`, whichever is less) and Nothing is returned without running the action; otherwise the clock advances by `scStepCost` and the action runs. `oNow` reads the clock. `oLog` appends to the trace. `oPrice` looks up `scPrices`. The returned `IO SimTrace` reads the trace at any time.

## 3. The Scheduler on the record

`SearchEnv` loses `seRun`, `seNow`, `seLog`, `sePrice` and gains `seOrch :: Orchestrator`. In `runSearch`: `async (seRun env spec)` becomes `oSpawn`; the `timeout (...) (waitAnyCatch ...)` becomes `oAwaitAny left`; `cancel a` becomes `oCancel`; `seNow` and `seLog` become `oNow` and `oLog`; `autoStep`'s `timeout` becomes `oCapped step secs` with the step name it already has (`"auto"`, `"repair"`, `"disprove"`); `priceOf` becomes `oPrice`. `stRunning` holds `Running` instead of `Async AttemptResult`. Nothing else in the module changes; the event sequence a run produces is identical (the real orchestrator's `oLog` is the old `seLog`).

Callers (`fisher-cli/src/Fisher/Cli/Search.hs` and the graph search's use of the flat run for leaves, if any) build `realOrchestrator` from the four functions they passed before.

## 4. The specs on the simulator

`SchedulerSpec`, `SchedulerAutoSpec`, `SchedulerPolicySpec`, `SchedulerRetrievalSpec`, `SchedulerStopSpec`, `SchedulerTierSpec` (97 examples today) build their `SearchEnv` with `simOrchestrator` and a `Script` instead of `fakeRun` with `threadDelay`. Each scenario states its attempts' virtual durations and results, so a budget cut is "the budget is 10 s and the attempt takes 12 s", not a sleep. Assertions on timing read the trace's clock. The run's events and goal states are asserted as today. New scenario tests the old specs could not express cheaply:

- the budget runs out while two attempts are in flight: both are cancelled, both priced, `BudgetSpent` logged, the report says so;
- one attempt closes a goal while another on the same goal runs: the second is cancelled, `AttemptCancelled` logged with its price;
- the early SMT disproof step hits its cap (`scCaps` has `("disprove", 3)`): the attempt still launches afterwards, the `AutoTimeout` event names the step;
- `oAwaitAny` with the budget exactly at the attempt's due time returns the attempt (at or before);
- a cancelled attempt that finishes anyway in the script is never delivered.

The real orchestrator keeps one spec of its own (`OrchestrateSpec`): spawn, await the first of two, cancel the other, a cap that hits, with short real sleeps, since it is the only place real threads remain.

## 5. Errors

Nothing new at run time: the real orchestrator is the old code moved. In the simulator, an unscripted attempt or an unknown price throws with the key in the message, so a wrong scenario fails loudly rather than deadlocking.

## 6. Testing

Section 4 is the test plan. Byte identity of the event stream: one SchedulerSpec scenario runs the same script through the simulator and through `realOrchestrator` with a fake run function whose delays are the script's seconds, and compares the two event lists with times removed.

## 7. Not in this stage

GraphRun on the orchestrator (it has its own `Ctx`, thread pool and `withAuto`; stage 4); recording a real run's choices as a `Script` and replaying it (stage 5); a `MonadOrchestrate` class or transformer (the record can be wrapped later if a second interpreter needs the type restriction); `Fisher.Swarm` (the process swarm) on the record.
