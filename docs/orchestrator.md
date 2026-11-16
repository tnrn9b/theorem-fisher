# The Orchestrator

This page describes `Fisher.Search.Orchestrate` (fisher-search), the record
through which the Scheduler does everything that touches threads, time or
the outside world, and the scripted simulator that stands in for it in
tests. It is the companion page to [roles.md](roles.md): that page says who
may say what in a prompt, this one says what the search itself may do.

The Scheduler orchestrates the flat search: it spawns attempts, waits for
the first to finish under the budget left, cancels the rest when a goal
closes or the budget runs out, reads the clock, writes events, caps each
automation step and prices a cancelled attempt. Those seven effects used
to be spread through the module as `async`, `waitAnyCatch`, `timeout` and
a few functions on `SearchEnv`, so the only way to test an ordering rule (a
budget cut, cancel-on-close, the early SMT disproof) was a spec with real
threads and `threadDelay`. They are now one record, `Orchestrator`, with two
constructors: the real one, and a simulator that follows a script in
virtual seconds. The Scheduler's logic and its event stream are the same
under both.

## The record

```haskell
newtype Running = Running Int          -- an attempt in flight, opaque to the Scheduler
data Orchestrator = Orchestrator
  { oSpawn    :: AttemptSpec -> IO Running
  , oAwaitAny :: Double -> [Running] -> IO (Maybe (Running, Either SomeException AttemptResult))
  , oCancel   :: Running -> IO ()
  , oNow      :: IO Double
  , oLog      :: Event -> IO ()
  , oCapped   :: forall a. Text -> Double -> IO a -> IO (Maybe a)
  , oPrice    :: AttemptSpec -> IO (Maybe Double)
  }
```

| Field | What it does |
|---|---|
| `oSpawn` | Starts an attempt and returns its handle. |
| `oAwaitAny` | Waits up to the given seconds for the first of the listed attempts to finish and returns it with its result or exception; Nothing when the time passes first, or at once when none of them is still in flight. An attempt delivered is gone. |
| `oCancel` | Stops an attempt. Cancelling one already delivered, or unknown, does nothing. |
| `oNow` | The clock, in seconds. |
| `oLog` | Writes one event to the run log. |
| `oCapped` | Runs a named step under a cap in seconds; Nothing when the cap hits. The name is for the simulator's script; it is not logged. |
| `oPrice` | The dollars a cancelled attempt spent, when known. Asked only with the controller on, once the attempt is cancelled. |

`oCapped` has a rank-2 type (`RankNTypes`) so that one field covers the
automation steps whatever result they carry.

## The two constructors

`realOrchestrator run now logE price` is the behaviour the Scheduler had
inline. Each attempt runs on its own thread: `oSpawn` forks under `mask_`
with `asyncWithUnmask`, so the fork and the recording of its handle are one
step and no asynchronous exception can leave a thread nobody can cancel,
while the attempt itself runs unmasked. The handles live in an
`IORef (Map Int (Async AttemptResult))`. `oAwaitAny` is `waitAnyCatch`
under `System.Timeout.timeout`; `oCancel` is `cancel` and forgets the
handle, so a cancelled attempt is never delivered; `oCapped` is `timeout`
with the rounding `max 1 (ceiling (secs * 1000000))`. `oNow`, `oLog` and
`oPrice` are the functions it was given.

`simOrchestrator script` returns the orchestrator and an `IO SimTrace` that
reads what was done so far. It never sleeps and never forks. Virtual time
starts at 0, never moves backwards, and moves only through an await (to the
attempt delivered plus `scStepCost`, or to the deadline) and a capped step.

```haskell
data Script = Script
  { scAttempts :: [(AttemptKey, Double, AttemptSpec -> Either Text AttemptResult)]
  , scDefault  :: Maybe (AttemptSpec -> Either Text AttemptResult)
  , scCaps     :: [(Text, Double)]
  , scPrices   :: [(AttemptKey, Double)]
  , scStepCost :: Double
  }
data AttemptKey = AttemptKey { akGoal :: Text, akBackend :: Text, akIndex :: Int }
data SimTrace = SimTrace
  { stSpawned :: [AttemptSpec], stCancelled :: [Running], stEvents :: [Event], stClock :: Double }
emptyScript :: Script
```

Each entry of `scAttempts` names an attempt, says how many virtual seconds
it takes, and gives a function from the attempt's spec to its result or to
the text of an exception. A function rather than a fixed result lets an
entry build its output from `asStart`, the base the attempt starts from.
`akIndex` counts the earlier spawns with the same goal and backend, so the
first attempt on a pair is 0 and a retry on the same backend is 1.
`scDefault` is the result of an attempt no entry names, taking no virtual
time; with Nothing such an attempt throws. An unscripted attempt, and a
price the script does not list, throw an `ErrorCall` with the key in the
message, so a wrong scenario fails at once instead of waiting forever.

The simulator's rules, in the order they come up:

| Call | Behaviour |
|---|---|
| `oSpawn` | Looks up the entry for the attempt's key and marks it due at now plus its seconds. |
| `oAwaitAny secs rs` | With none of `rs` in flight, returns Nothing at once and leaves the clock alone. Otherwise the deadline is now plus `secs` (never less than now), and it takes the earliest due of `rs` (the one spawned first among those due together). If it is due at or before the deadline, the clock moves to its due time plus `scStepCost` and it is delivered; otherwise the clock moves to the deadline and the result is Nothing. |
| `oCancel` | Removes the attempt from the due list and records it in `stCancelled`. The attempt is never delivered, even though its entry would have finished. |
| `oCapped name secs act` | When `name` is in `scCaps`, the clock moves by the listed seconds (or `secs` clamped at 0, whichever is less) and the result is Nothing without running `act`. Otherwise the clock moves by `scStepCost` and `act` runs. |
| `oPrice` | Looks the attempt's key up in `scPrices`. |
| `oLog` | Appends to the trace. |

The simulator differs from the real side in two places, and the Scheduler reaches neither: on a tie the real side delivers in list order (newest first, as the Scheduler lists them) while the simulator delivers the attempt spawned first, and with zero seconds the real side returns Nothing (`timeout 0`) while the simulator delivers an attempt already due.

## SearchEnv and runToEnd

`SearchEnv` holds the orchestrator where it used to hold four functions:

```haskell
data SearchEnv = SearchEnv
  { seOrch      :: Orchestrator
  , seLean      :: LeanRun
  , sePolicy    :: AxiomPolicy
  , seRoot      :: FilePath
  , seRel       :: FilePath
  , seBaseChanged :: Text -> IO ()
  , seAuto      :: Maybe AutoEnv
  , seRetrieval :: Maybe RetrievalEnv
  }
```

The run function, clock, log and price that callers passed as `seRun`,
`seNow`, `seLog` and `sePrice` are now the four arguments of
`realOrchestrator`. In `autoStep`, an `oCapped` that returns Nothing is a
missed step (`AutoMissed`); `AutoTimeout` is logged only when an Lsp call is
cut at its own cap.

```haskell
runToEnd :: Orchestrator -> AttemptSpec -> IO AttemptResult
```

`runToEnd` spawns one attempt and waits for it to the end, cancelling it if
the caller is interrupted. `runSingle` and the graph search's workers use
it, so they run on the record too. Its caller must be the only holder of the
`Running`: if anyone else cancelled or awaited it, `oAwaitAny` would return
Nothing at once and the wait would spin.

## Writing a scenario

A scenario is a script, a configuration and assertions on the report and
the events. `Fisher.Search.SimHarness` (in the test tree) builds the
`SearchEnv` over a fresh project directory and gives terse ways to write
entries.

| Helper | What it gives |
|---|---|
| `simSearch file cfg script` | Runs the search to its end and returns the report, the events (oldest first) and the `SimTrace`. |
| `script entries` | A `Script` of these entries in which an unscripted attempt writes `sorry` at once. |
| `attempt g b secs f` | The first attempt on goal `g` and backend `b`: it takes `secs` and `f` gives its result. `attemptN` adds the index. |
| `always g b secs f` | The same entry for every attempt on that goal and backend. |
| `inOrder g [(b, secs, f)]` | A goal's attempts in launch order; the index is counted per backend. |
| `proofAttempt proof` | An attempt that writes this proof under the goal's statement. `sorryAttempt` writes `sorry`. |

The example is from `SchedulerSpec`: an attempt due exactly at the budget
is delivered, and one due after it is cut. The goals, the helper `on` (every
attempt on a goal and backend writes this proof in these seconds) and the
helper `search` are the spec's own.

```haskell
search :: SearchConfig -> [ScriptEntry] -> IO (SearchReport, [Event])
search cfg es = (\(rep, evs, _) -> (rep, evs)) <$> simSearch file cfg (script es)

on :: T.Text -> T.Text -> T.Text -> Double -> [ScriptEntry]
on g b proof secs = always g b secs (proofAttempt proof)

it "delivers an attempt due exactly at the budget, and cuts one due after it" $ do
  let cfg = cfg0 {scBackends = ["x"], scSlots = 1, scParallelPerGoal = False}
      es = on "a" "x" "ok" 5
  (rep, evs) <- search cfg {scBudget = 5} es
  lookup "a" (status rep) `shouldBe` Just Closed
  [(t, i, o) | Finished t i _ _ o _ _ <- evs] `shouldBe` [(5, 1, "closed")]
  [() | AttemptCancelled {} <- evs] `shouldBe` []
  [t | BudgetSpent t <- evs] `shouldBe` [5]
  (cut, cutEvs) <- search cfg {scBudget = 4.9} es
  lookup "a" (status cut) `shouldBe` Just Open
  [arOutcome r | r <- srAttempts cut] `shouldBe` ["cancelled: budget spent"]
  [() | Finished {} <- cutEvs] `shouldBe` []
  [t | BudgetSpent t <- cutEvs] `shouldBe` [4.9]
```

The attempt takes 5 virtual seconds. With a budget of 5 it is due at the
deadline and is delivered; with 4.9 the clock stops at 4.9, the attempt is
cancelled and the outcome says the budget was spent. Nothing sleeps.

A scenario with prices reads the trace as well. From `SchedulerStopSpec`:
the budget is 10 s and two attempts that take 12 and 15 s are in flight, so
both are cancelled and priced from `scPrices`, and the trace shows two
cancellations.

```haskell
let cfg = cfgS {scBackends = ["x", "y"], scSlots = 2, scParallelPerGoal = True, scBudget = 10}
    es = stepsOn "t" [("x", (step "exact BAD") {sSecs = 12}), ("y", (step "ok") {sSecs = 15})]
r <- searchScript Nothing cfg Nothing one
       emptyScript {scAttempts = es, scPrices = [(AttemptKey "t" "x" 0, 0.4), (AttemptKey "t" "y" 0, 0.7)]}
[(t, i, d) | AttemptCancelled t i _ d <- rEvents r] `shouldBe` [(10, 2, Just 0.7), (10, 1, Just 0.4)]
[t | BudgetSpent t <- rEvents r] `shouldBe` [10]
length (stCancelled (rTrace r)) `shouldBe` 2
```

Here `stepsOn` and `step` are that spec's own wrappers over `inOrder`, and
`searchScript` runs `simSearchEnv` with a `SimEnv` that adds the automation
or a price function when a scenario needs one. A step that should hit its
cap is a `scCaps` entry, for example `("disprove", 3)`; a clock reading
after the run is `stClock`.

## What the simulator cannot speed up

The simulator replaces only what goes through the record. Waits that are
real and happen inside an attempt's own work are not orchestrator time and
stay as they are: the real waits inside the Auto pre-pass, Repair and
Retrieval timeouts. A scenario that exercises one of those still pays for
it. The six Scheduler specs (`SchedulerSpec`, `SchedulerAutoSpec`,
`SchedulerPolicySpec`, `SchedulerRetrievalSpec`, `SchedulerStopSpec`,
`SchedulerTierSpec`) run on the simulator; the fisher-search suite went from
about 31 s to about 20 s. `OrchestrateSpec` keeps real threads, because the
real constructor is only tested there, and `SchedulerIdentitySpec` runs one
scenario through both constructors, with each script's seconds scaled to
real milliseconds, and compares the two event lists with their times set
to 0, so the simulator's events cannot drift from the real ones unnoticed.

## What is not here

Two things are not built. The graph search (`GraphRun`) has its own context, thread pool and
automation wrapper. Its workers' attempts (by `runToEnd`), its clock and its
automation cap go through the record; only its own thread pool and job
handling do not, and moving those onto the record is future work.
Recording a real run's choices as a `Script` and replaying it is also future
work. The record is not a type class or a transformer, and the process
swarm in `Fisher.Swarm` does not use it.
