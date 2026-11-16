# The Orchestrator

This page describes `Fisher.Search.Orchestrate` (fisher-search), the record
through which the searches do everything that touches threads, time or the
outside world, and the scripted simulator that stands in for it in tests,
and `Fisher.Search.Tape`, the recorder and replayer that turn a real run
into a tape. It is the
companion page to [roles.md](roles.md): that page says who may say what in a
prompt, this one says what the search itself may do.

The flat search's Scheduler orchestrates attempts: it spawns them, waits for
the first to finish under the budget left, cancels the rest when a goal
closes or the budget runs out, reads the clock, writes events, caps each
automation step and prices a cancelled attempt. The graph search
(`Fisher.Search.GraphRun`) does the same with its own jobs: workers,
planners, reviewers and falsifiers. Those seven effects used to be spread
through the modules as `async`, `waitAnyCatch`, `timeout` and a few functions
on `SearchEnv`, so the only way to test an ordering rule (a budget cut,
cancel-on-close, the early SMT disproof) was a spec with real threads and
`threadDelay`. They are now one record, `Orchestrator`, with two
constructors: the real one, and a simulator that follows a script in virtual
seconds. The searches' logic and event streams are the same under both.

## The record

The record is polymorphic in the spec of a job and in its result:

```haskell
newtype Running = Running Int          -- a job in flight, opaque to the search
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

The flat search uses `AttemptOrchestrator`, so stage 3's callers read as
before. The graph search uses `Orchestrator Job Out` (see the graph section
below).

| Field | What it does |
|---|---|
| `oSpawn` | Starts a job and returns its handle. |
| `oAwaitAny` | Waits up to the given seconds for the first of the listed jobs to finish and returns it with its result or exception; Nothing when the time passes first, or at once when none of them is still in flight. A job delivered is gone. |
| `oCancel` | Stops a job. Cancelling one already delivered, or unknown, does nothing. |
| `oNow` | The clock, in seconds. |
| `oLog` | Writes one event to the run log. |
| `oCapped` | Runs a named step under a cap in seconds; Nothing when the cap hits. The name is for the simulator's script; it is not logged. |
| `oPrice` | The dollars a cancelled job spent, when known. Asked only with the controller on, once the job is cancelled. |

`oCapped` has a rank-2 type (`RankNTypes`) so that one field covers the
automation steps whatever result they carry.

## The two constructors

`realOrchestrator run now logE price` is the behaviour the Scheduler had
inline. Each job runs on its own thread: `oSpawn` forks under `mask_`
with `asyncWithUnmask`, so the fork and the recording of its handle are one
step and no asynchronous exception can leave a thread nobody can cancel,
while the job itself runs unmasked. The handles live in an
`IORef (Map Int (Async res))`. `oAwaitAny` first polls the
handles in list order and delivers the first already finished, so zero
seconds still delivers it; otherwise it is `waitAnyCatch` (which also
takes the first in list order) under `System.Timeout.timeout`; `oCancel` is `cancel` and forgets the
handle, so a cancelled job is never delivered; `oCapped` is `timeout`
with the rounding `max 1 (ceiling (secs * 1000000))`. `oNow`, `oLog` and
`oPrice` are the functions it was given.

`simOrchestrator keyFn script` returns the orchestrator and an `IO SimTrace`
that reads what was done so far. It needs `Eq spec` (a price is looked up by
the spec's equality). It never sleeps and never forks. Virtual time
starts at 0, never moves backwards, and moves only through an await (to the
job delivered plus `scStepCost`, or to the deadline) and a capped step.

The key function says how a spec is matched to a script entry: it gives the
two halves of the key, and the simulator adds how many jobs with those
halves were spawned before. The flat search's is `attemptKey`, an attempt's
goal and backend; the graph search's is `jobKey`, a job's node name and kind.

```haskell
data Entry spec res
  = Scripted (spec -> IO (Either Text res))
  | Run      (spec -> IO res)
data Script spec res = Script
  { scAttempts :: [(AttemptKey, Double, Entry spec res)]
  , scDefault  :: Maybe (Entry spec res)
  , scCaps     :: [(Text, Double)]
  , scPrices   :: [(AttemptKey, Double)]
  , scStepCost :: Double
  }
data AttemptKey = AttemptKey { akGoal :: Text, akBackend :: Text, akIndex :: Int }
data SimTrace spec = SimTrace
  { stSpawned :: [spec], stDefaulted :: [spec]
  , stCancelled :: [Running], stEvents :: [Event], stClock :: Double }
attemptKey  :: AttemptSpec -> (Text, Text)
simOrchestrator :: Eq spec => (spec -> (Text, Text)) -> Script spec res -> IO (Orchestrator spec res, IO (SimTrace spec))
emptyScript :: Script spec res
scriptEntry :: (spec -> (Text, Text)) -> Script spec res -> Map (Text, Text) Int -> spec -> Either Text (ScriptHit spec res, Map (Text, Text) Int)
```

An `Entry` says how a job finishes:

| Entry | What it does |
|---|---|
| `Scripted f` | `f` gives the result, or the text of an exception as a Left, from the spec. It runs at spawn, in IO so a harness can record what was spawned. A function that throws is a Left too. |
| `Run f` | `f` is an action run at spawn, on the caller's thread, after the spawn is recorded (so the action sees it in the trace); its result is delivered after the entry's seconds. A synchronous exception it throws is the job's result, a Left; an asynchronous one is rethrown from `oSpawn`, and that job, recorded as spawned, is never delivered. |

The same holds for `Scripted`: a synchronous exception from its function is a
Left, an asynchronous one is rethrown. The effect of `Run` is that a
scenario can do real work at spawn (a harness's bookkeeping, a fake model
call) while the job's virtual time stays what the script says.

`stDefaulted` lists, oldest first, the spawned jobs no entry named, whose
entry came from `scDefault`. A scenario whose script names every job it
expects asserts it is empty, so a default cannot hide a wrong guess about
which goal or backend runs. `scriptEntry` is how a script is read: from the
key function and the counts of earlier spawns per key it gives the job's key,
its entry (or the default, marked as such in the `ScriptHit`) and the counts
with this spawn added; Left names a job neither scripted nor defaulted. The
identity specs' real run functions read the script through it too.

Each entry of `scAttempts` names a job, says how many virtual seconds
it takes, and says how it finishes. `akIndex` counts the earlier spawns with
the same key, so the first job on a pair is 0 and a retry on the same
backend is 1. `scDefault` is the entry of a job no entry names, taking no
virtual time; with Nothing such a job throws. An unscripted job, and a
price the script does not list, throw an `ErrorCall` with the key in the
message, so a wrong scenario fails at once instead of waiting forever.

The simulator's rules, in the order they come up:

| Call | Behaviour |
|---|---|
| `oSpawn` | Looks up the entry for the job's key and marks the job due at now plus its seconds. A `Scripted` function or a `Run` action runs here, on the caller's thread; an exception it throws is a `Left` result (an asynchronous one is rethrown). |
| `oAwaitAny secs rs` | With none of `rs` in flight, returns Nothing at once and leaves the clock alone. Otherwise, when some of `rs` are already due (at or before now), the first of them in `rs` is delivered and the clock moves only by `scStepCost`. When none is, the deadline is now plus `secs` (never less than now), and it takes the earliest due of `rs` (the one first in `rs` among those due together). If it is due at or before the deadline, the clock moves to its due time plus `scStepCost` and it is delivered; otherwise the clock moves to the deadline and the result is Nothing. |
| `oCancel` | Removes the job from the due list and records it in `stCancelled`. The job is never delivered, even though its entry would have finished. |
| `oCapped name secs act` | When `name` is in `scCaps`, the clock moves by the listed seconds (or `secs` clamped at 0, whichever is less) and the result is Nothing without running `act`. Otherwise the clock moves by `scStepCost` and `act` runs. |
| `oPrice` | Looks the spec up as its latest spawn, by equality, and then that key in `scPrices`. |
| `oLog` | Appends to the trace. |

Both sides keep one rule for an await: a job already finished is delivered in list order, even with zero seconds; otherwise the first to finish, the one first in `rs` among those finishing together (the Scheduler lists the newest first).

## SearchEnv and runToEnd

`SearchEnv` holds the orchestrator where it used to hold four functions:

```haskell
data SearchEnv = SearchEnv
  { seOrch      :: AttemptOrchestrator
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
runToEnd :: AttemptOrchestrator -> AttemptSpec -> IO AttemptResult
```

`runToEnd` spawns one attempt and waits for it to the end, cancelling it if
the caller is interrupted. `runSingle` and the graph search's workers use
it, so they run on the record too. Its caller must be the only holder of the
`Running`: if anyone else cancelled or awaited it, `oAwaitAny` would return
Nothing at once and the wait would spin.

## The graph search on the record

The graph search keeps its own orchestrator, of type `Orchestrator Job Out`,
over its own jobs. Its pieces are in `Fisher.Search.GraphRun`:

```haskell
data Job = Job { jId :: Int, jNode :: NodeId, jName :: Text, jKind :: Kind }
data Kind
  = KWorker AttemptSpec Double          -- the attempt, and when it started
  | KPlan SplitId PlanInputs
  | KReview SplitId Plan ReviewInputs   -- the split and the plan under review
  | KFalse Text FalseInputs             -- the worker's claim
data Out
  = OutWorker AttemptResult
  | OutPlan (Asked Plan)
  | OutReview (Asked SplitVerdict)
  | OutFalse (Asked (Bool, Said 'Verifier))
jobKey :: Job -> (Text, Text)           -- (node name, "worker" | "plan" | "review" | "false")
runGraphJob :: RoleCtxs -> AttemptOrchestrator -> Job -> IO Out   -- not exported
```

`jId` is the job's number, unique in the run, so two jobs that look alike
(the same node asking the same planner twice) are different jobs to the
simulator, which prices a cancelled one by its spec's equality. `jName` is the
node's name and `jKind` says what the job is. `Kind` carries each job's
inputs, so the job is a value that can be run later: `PlanInputs` is what the
planner's job is called with (the base, the node's name and expected type,
the usable lemmas, the whiteboard, the notes of failed attempts and the
retrieved text), `ReviewInputs` the reviewer's besides the plan (the
statement and the whiteboard) and `FalseInputs` the falsifier's besides the
claim (the statement); the plan and the claim are `KReview`'s and `KFalse`'s
own fields, held once. The input records, `RoleCtxs` and `runGraphJob` are
internal to `GraphRun`; `Job (..)`, `jobKey`, `Kind (..)` and `Out (..)` are
exported for the harness. `Out` is the result by kind: a worker's `AttemptResult`, or what a
role call returned (`Asked`, the call's reply with its dollars).

`runGraphJob` is the one place that says how a job runs: a worker's attempt
goes to its end on the flat orchestrator (`runToEnd`), and a planner, reviewer
or falsifier is a role call over the job's inputs, through the run's role
contexts. Only `runGraph` calls it: it hands `runGraphJob` over the run's
role contexts to the builder (`geOrch`), so a real builder's orchestrator
runs jobs with it, and neither the CLI nor the test harness repeats the
dispatch.

### GraphEnv.geOrch

```haskell
data GraphEnv = GraphEnv
  { geSearch :: SearchEnv
  , geOrch   :: (Job -> IO Out) -> IO (Orchestrator Job Out)
  , geAsk    :: Role -> [Message] -> IO (Either AskError (Text, Double))
  , geLogG   :: GraphEvent -> IO ()
  , geSave   :: Graph -> [(NodeId, Text)] -> IO ()
  }
```

`geOrch` is a builder, not an orchestrator. The role contexts that
`runGraphJob` needs (the planner's and the verifier's) are made inside
`runGraph`, from `geAsk` and the run's checker, which holds the Lean run and
the checker lock that `runGraph` creates. The caller cannot have them before
the run starts, so it cannot build the orchestrator first. `runGraph` builds
the contexts, then calls `geOrch env (runGraphJob roles (seOrch se))` once, and
the builder returns the orchestrator for the run. A real builder passes the run
function it is given to `realOrchestrator` with the clock, the log and a
price; the simulator's builder ignores the run function and reads a script.
The orchestrator's log is not used by the graph search: its events go
through `geLogG`. The graph's automation caps (`oCapped`) and a cancelled
worker's price (`oPrice`) go through this orchestrator too, and a worker's
inner attempt goes through `seOrch` of `geSearch`.

### The loop's guard

`loop` is the graph search's main cycle: start what can start, wait for the
first job to finish, apply it, repeat. With no budget left it stops without
awaiting. Before the move onto the record the await was a `timeout 0`, which
delivered nothing; an `oAwaitAny` with zero seconds delivers a job already
finished (the rule above), so without a guard a job finished at the instant the
budget ran out would be applied after the budget was spent. The guard is
`left <= 0` after the clock is read: the loop stops with the reason `time`
and cancels what runs, as it did.

### Writing a graph scenario

`Fisher.Search.GraphHarness` (in the test tree) runs the graph search on the
simulator over `jobKey`. A scenario is a configuration, a file, each goal's
workers in order, and the scripted model replies:

| Piece | What it is |
|---|---|
| `Run` (the harness's record, not the `Run` entry) | One worker's attempt: `rProof` (the proof it writes), `rFinal`, `rCost`, `rClock` (the virtual seconds it takes from spawn to delivery) and the controller's fields. `w proof` is one that writes a proof, `ok` and `bad` are the proofs `exact done` and `BAD`. |
| `Reply role match answer` | A scripted reply to the first model call of that role whose last message contains `match`. `say role text` is one with no match and no dollars, `sayTo role m text` one with a match. |
| `runG cfg file runs replies` | Runs the graph search to its end and returns a `GOut` (the report, the events, the asks, what was launched and probed). |
| `runGTimed timed price mkRet lean auto cfg file runs replies` | `runG` with virtual seconds for some model-call jobs. `timed` is a list of `(AttemptKey, seconds)` keyed by node name and kind (`plan`, `review` or `false`) and how many of that key ran before. |
| `runGBy orchFor cfg file runs replies` | `runG` on another builder, for the identity test. |

The n-th worker on a node named `g` is keyed `(g, "worker")` at index n and
takes its `rClock` seconds; its result is made at spawn by a `Scripted` entry
that does the harness's bookkeeping. A model-call job named in `timed` is a
`Run` entry that calls the fake ask at spawn and is delivered after its seconds.
Every other job goes through `scDefault` at no virtual time: a worker past its
goal's list gets `bad`, and a model-call job calls the fake ask. The fake ask
takes the first of the scripted replies that fits its role and `match`, and
answers `AskFailed "nothing scripted"` when none does.

The first example is from `GraphRunSpec`: the planner's reply costs 2 dollars,
which reaches the cap of 1, so the verifier is not started.

```haskell
it "9: the dollar cap reached by the planner's reply does not start the verifier" $ do
  o <- runG cfg {gcCapUsd = 1} oneRoot
    [("r1", [bad {rCost = 0.25}])]
    [Reply Planner Nothing (Right (plan [("l", "P")] "theorem r1 : A" "exact use l", 2)), say Verifier accept]
  map fst (oAsks o) `shouldBe` [Planner]
  grStopped (oRep o) `shouldBe` Just "dollars"
  Map.lookup Planner (grDollars (oRep o)) `shouldBe` Just 2
  Map.lookup Worker (grDollars (oRep o)) `shouldBe` Just 0.25
  count "stopped dollars" (tags o) `shouldBe` 1
```

`r1`'s first worker fails, so the node asks the planner once and the cap stops
the run. Nothing sleeps. The second example gives two planners and a
reviewer their own seconds with `runGTimed`, so that two roots' proposals are
both out before either is accepted:

```haskell
it "10: two roots proposing the same lemma at once end with one node" $ do
  let timed = [(AttemptKey "r1" "plan" 0, 0.1), (AttemptKey "r2" "plan" 0, 0.1), (AttemptKey "r2" "review" 0, 0.4)]
  o <- runGTimed timed (\_ -> pure Nothing) (\_ -> pure Nothing) keyLean Nothing cfg twoRoots
    [("r1", [bad]), ("r2", [bad]), ("l", [ok {rClock = 0.9}])]
    [ Reply Planner (Just (toNode "r1")) (Right (plan [("l", "P")] "theorem r1 : A" "exact use l", 0))
    , Reply Planner (Just (toNode "r2")) (Right (plan [("l", "P")] "theorem r2 : B" "exact use l", 0))
    , Reply Verifier (Just "theorem r1") (Right (accept, 0))
    , Reply Verifier (Just "theorem r2") (Right (accept, 0)) ]
  precedes (tags o) "proposed r2" "accepted r1" `shouldBe` True
  length [() | n <- IM.elems (gNodes (grGraph (oRep o))), nName n == "l"] `shouldBe` 1
```

Each planner takes 0.1 virtual seconds and `r2`'s review 0.4, so both
proposals are out before `r1`'s is accepted, and `l` is shared by the two.
The old spec used real sleeps for these gaps.

### The graph identity test

`GraphIdentitySpec` runs one scenario twice: on the simulator, and on real
threads through `realFor` (the real orchestrator over the same script, each
job sleeping its seconds scaled to 50 real milliseconds per virtual second).
It compares the two event lists with their times set to 0, so the simulator's
graph events cannot drift from the real ones. The scenario has two roots: the
first's worker fails at 2 s, its split into `l` is accepted at once and `l`
proves it at 4 s, and the second's worker would take 60 s and is cancelled at
the budget, 8 s. It then pins the simulated times (the launches at 0, 0 and 2,
the proofs at 4, the cancellation at 8). It is the only graph spec on real
threads apart from the checker-lock case described below, and its sleeps
total well under a second.

## Writing a scenario

A scenario for the flat search is a script, a configuration and assertions on
the report and the events. `Fisher.Search.SimHarness` (in the test tree)
builds the `SearchEnv` over a fresh project directory and gives terse ways to
write entries. Its helpers keep their pure signatures; each lifts its
function into a `Scripted` entry.

| Helper | What it gives |
|---|---|
| `simSearch file cfg script` | Runs the search to its end and returns the report, the events (oldest first) and the `SimTrace`. |
| `script entries` | A `Script` of these entries in which an unscripted attempt writes `sorry` at once. |
| `attempt g b secs f` | The first attempt on goal `g` and backend `b`: it takes `secs` and `f` gives its result. `attemptN` adds the index. |
| `always g b secs f` | The same entry for every attempt on that goal and backend (indices 0 to 99). |
| `allNamed trace` | Expects `stDefaulted` empty: every attempt launched was named by an entry. The specs' search helpers assert it; a scenario that leaves attempts to the default uses the form without the check (`searchPast`). |
| `inOrder g [(b, secs, f)]` | A goal's attempts in launch order; the index is counted per backend. |
| `proofAttempt proof` | An attempt that writes this proof under the goal's statement. `sorryAttempt` writes `sorry`. |

The example is from `SchedulerSpec`: an attempt due exactly at the budget
is delivered, and one due after it is cut. The goals, the helper `on` (every
attempt on a goal and backend writes this proof in these seconds) and the
helper `search` are the spec's own.

```haskell
search :: SearchConfig -> [ScriptEntry] -> IO (SearchReport, [Event])
search cfg es = do
  (rep, evs, tr) <- simSearch file cfg (script es)
  allNamed tr
  pure (rep, evs)

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
real and happen inside a job's own work are not orchestrator time and
stay as they are: the real waits inside the Auto pre-pass, Repair and
Retrieval timeouts. A scenario that exercises one of those still pays for
it. The graph suite shows the same: the scenarios that exercise the Auto,
Repair and Retrieval timeouts keep those real waits, and the checker-lock case
(`GraphAutoSpec`, "Auto takes the checker lock") runs on real threads through
`realFor`, because the simulator runs every job on the caller's thread and so
cannot show a Lean check and a language server call overlapping; the harness
counts the calls that ran at the same time (`oOverlaps`), and the spec fails
when the lock is dropped. The six Scheduler specs (`SchedulerSpec`, `SchedulerAutoSpec`,
`SchedulerPolicySpec`, `SchedulerRetrievalSpec`, `SchedulerStopSpec`,
`SchedulerTierSpec`) and the graph specs run on the simulator.
`OrchestrateSpec` keeps real threads, because the real constructor is only
tested there, `SchedulerIdentitySpec` runs one scenario through both
constructors, with each script's seconds scaled to real milliseconds, and
compares the two event lists with their times set to 0, so the simulator's
events cannot drift from the real ones unnoticed; `GraphIdentitySpec` does the
same for the graph search.

## Recording and replay

The recorder and the replayer are in `Fisher.Search.Tape` (fisher-search),
which imports `AttemptKey`, `attemptKey` and `spawnKey` from
`Fisher.Search.Orchestrate`; `Orchestrate` itself keeps the record and its two
interpreters and has no aeson. A recording is a tape: the orchestrator's
answers, and the events logged through it, in call order. It is not a script
for the simulator. A real run has time of its own outside its awaits (Lean
checks, steps that run without a cap), so measuring each job's seconds since
its spawn and feeding them to the simulator would reorder deliveries or spawn
a job the run never made; and a cap named only by its step's name cannot tell
its occurrences apart. A tape holds exactly what the search was told and what
it logged, in the order it asked, and a replay plays it back.

```haskell
data Recording spec res = Recording { rcVersion :: Int, rcSteps :: [Step res] }
data Step res
  = Now Double
  | Spawned AttemptKey
  | Delivered Double AttemptKey Double (Either Text res)
  | AwaitTimedOut Double Double
  | Cancelled AttemptKey
  | Capped Text Double Bool [Event]
  | Priced AttemptKey (Maybe Double)
  | Logged Event
tapeVersion :: Int                      -- 1
type AttemptScript = Recording AttemptSpec AttemptResult
recordOrchestrator :: (spec -> (Text, Text)) -> Orchestrator spec res -> IO (Orchestrator spec res, IO (Recording spec res))
replayOrchestrator :: (spec -> (Text, Text)) -> (Event -> IO ()) -> Recording spec res -> IO (Orchestrator spec res, IO Int)
```

The guarantee: every orchestrator answer and every event logged through the
orchestrator is compared, and a difference ends the replay with
`ReplayDiverged`; what a replay re-runs (Lean, the language server, retrieval
over the library as it is now) can therefore only fail loudly, never
silently; events written around the orchestrator (the CLI's `LedgerUnread`
lines from the watcher, and the start events the CLI computes before the
search and writes with its own log function: the language server's `AutoOff`
at the start and the retrieval's `IndexBuilt`, `IndexSkipped` and
`LibraryProblem`, the first of which carries the index build's own seconds;
besides these, the learn line, the header and the language server's warm-up
lines are printed, not logged) are not covered.

The spec is not recorded: on replay the search builds the specs again, and
each spawn's key is checked against the tape. Each constructor of `Step` is
one answer or one event:

| Step | What it records | How replay answers it |
|---|---|---|
| `Now t` | A clock read and its value. | `oNow` returns `t`. |
| `Spawned key` | A spawn, with its key (counted as `scriptEntry` counts). | `oSpawn` returns a fresh handle when the key of the spec being spawned is `key`. |
| `Delivered secs key t res` | An await of `secs` that delivered the job `key` with `res` (an exception as its text), and the wrapped clock `t` after it. | `oAwaitAny` delivers the job with that key, which must be among those awaited, with `res` (a Left becomes an `ErrorCall` of its text); `secs` must equal the call's within 1e-9. `t` is for readers; replay does not use it. |
| `AwaitTimedOut secs t` | An await of `secs` that delivered nothing, and the clock after it. | `oAwaitAny` returns Nothing; `secs` is checked. |
| `Cancelled key` | A cancel of the job `key`. | `oCancel` accepts a cancel of the job with that key. |
| `Capped name secs hit evs` | A capped step: its name, its seconds, whether the cap hit, and for a hit the events its action logged before the cap hit, in order (empty for a step that ran). | When it hit, `oCapped` gives `evs` to the log function and returns Nothing without running the action. When it ran, the action runs again under a real `timeout` of `secs` and its result is returned; when the timeout fires, `ReplayDiverged` with "the step `name` did not end within its recorded cap". The name and seconds are checked. |
| `Priced key d` | A price asked for a spec, under the key of the latest spawn with the spec's goal and backend, and the answer the caller used: a price that throws a synchronous exception is recorded as Nothing (as `priceCancelled` uses it) and the exception rethrown. | `oPrice` returns `d`. |
| `Logged e` | An event logged through the orchestrator. | `oLog e'` must find `Logged e` with `e' == e`; the event is then given to the log function. Otherwise `ReplayDiverged`, its description showing both events. |

`recordOrchestrator keyFn orch` wraps an orchestrator in use. Every field is
the wrapped one's, and each call appends its step once the wrapped call
returns; `oLog` writes through the wrapped log and appends a `Logged` step. A
capped step takes its place on the tape when it starts, so the calls made
inside a step that ran, and the events it logged, follow it. A step whose
cap hit drops the steps made inside it, because replay does not run its
action (the loop waits on it, so they are all after its place), and gathers
the events among them into its own list: each dropped `Logged` step's event,
and each dropped hit `Capped` step's own list, in order. So a cap nested in a
cap whose outer one hits after the inner one ran is one `Capped` step whose
list holds the inner step's events and then the outer's. A step that throws
leaves no step.

`replayOrchestrator keyFn logE tape` plays the tape back and returns, beside the
orchestrator, a reader of the number of steps consumed. Each call takes the
tape's next step and answers from it. A call that is not the next step (the
wrong kind, a different key, seconds that differ, another event), any call
past the tape's end, or a step that ran and does not end within its recorded
seconds, throws `ReplayDiverged`, which names the step's position (from 0),
what the tape had and what the search asked for:

```haskell
data ReplayDiverged = ReplayDiverged { rdPosition :: Int, rdExpected :: Text, rdActual :: Text }
-- ReplayDiverged 0 "now 1.0" "spawn a/x/0"
-- ReplayDiverged 1 "the end of the tape" "now"
-- ReplayDiverged 0 "capped auto 2.0 (hit)" "capped auto 3.0"
-- ReplayDiverged 0 "log BudgetSpent 0.0" "log BudgetSpent 1.0"
-- ReplayDiverged 0 "capped repair 5.0e-2 (ran)" "the step `repair` did not end within its recorded cap"
```

Replay reproduces the events byte for byte, times included: the clock is the
tape's readings, Lean still runs for any step that ran, and every event is
checked against the tape before it is written. A Lean that judges a proof
otherwise on replay changes the event the search logs (an attempt's
`Finished` outcome, say), and the replay stops there. A tape that the search
does not consume to its end is not an error inside the orchestrator; the
caller compares the steps consumed with the tape's length (the CLI does).

The flat search's tape has JSON instances, so a run can be written to a file.
`Step` is generic: an object with the constructor's name under `tag` and its
fields under `contents`. `Recording` is `{"rcVersion": 1, "rcSteps": [...]}`;
its decoder refuses a tape whose `rcVersion` is not `tapeVersion` (1), with a
message naming the version found, and one without `rcVersion`. `AttemptKey`
is an object with its field names (its instances are in `Tape`), an `Either`
is `{"Left": ...}` or `{"Right": ...}`, and `AttemptResult`, `ChildStats`,
`Stop`, `StopReason`, `ModelFailure` and the scheduler's `Event` have generic
instances in their own modules, field names as they are. An example of a few
steps (the first attempt fails and its end is logged, the second is
cancelled, an automation step hits its cap after logging an event):

```json
{"rcVersion": 1, "rcSteps": [
  {"tag": "Now", "contents": 1000.5},
  {"tag": "Spawned", "contents": {"akGoal": "a", "akBackend": "x", "akIndex": 0}},
  {"tag": "Spawned", "contents": {"akGoal": "a", "akBackend": "y", "akIndex": 0}},
  {"tag": "Delivered", "contents": [30, {"akGoal": "a", "akBackend": "x", "akIndex": 0}, 1002.25, {"Left": "boom"}]},
  {"tag": "Now", "contents": 1002.5},
  {"tag": "Logged", "contents": {"tag": "Finished", "contents": [2, 1, "a", "x", "error: boom", null, null]}},
  {"tag": "Cancelled", "contents": {"akGoal": "a", "akBackend": "y", "akIndex": 0}},
  {"tag": "Priced", "contents": [{"akGoal": "a", "akBackend": "y", "akIndex": 0}, 0.125]},
  {"tag": "Capped", "contents": ["auto", 4, true, [{"tag": "AutoTimeout", "contents": [3.5, "a", "auto"]}]]},
  {"tag": "AwaitTimedOut", "contents": [5, 1008.5]}
]}
```

A delivered `Right` holds the attempt's `AttemptResult` as an object. The
asBrief of an attempt is a `PromptPart` and is not on the tape at all (the
spec is not recorded), so no `PromptPart` is ever built from a file.

### On the command line

`fisher search FILE --record TAPE` wraps the attempts' real orchestrator
with `recordOrchestrator attemptKey` and, once the search returns its
report, writes its tape (an `AttemptScript`) to TAPE as JSON. The run is
otherwise a normal run: run directory, run log, ledger, lessons and policy as
before. Before the run starts TAPE is opened for appending and closed, so a
path that cannot be written is refused then, a missing file is created empty
and a file already there keeps its content. An interrupted or failed run
writes no tape (an existing TAPE survives it). The final write goes to a
temporary file in TAPE's directory, renamed over TAPE once whole, so TAPE is
either the old file or the whole new tape; a final write that fails is a
warning on stderr while the run still finishes. The flag is refused with
`--graph`, whose jobs carry values that are never built from a file.

`fisher search FILE --replay TAPE`, with the recorded run's other flags,
plays the tape back through `replayOrchestrator attemptKey`: every spawn,
await, cancel, clock reading, cap and price comes from the tape, and every
event the search logs is checked against it, while Lean still runs for the
proof checks (the replay reproduces the model side only). It prints the learn
line, the header, the events and the report as a run prints them, with
`replay of TAPE` where a run names its run directory, and writes nothing: no
run directory (its base workspace is a temporary directory, removed at the
end), no run log, ledger, lessons or learned policy. It refuses `--apply`,
`--graph`, `--out` and `--record`. A tape that cannot be read or decoded (of
another version, say), a search that asks for something the tape does not
hold or logs an event other than the tape's (`ReplayDiverged`), and a search
that leaves steps of the tape unconsumed (the consumed count is checked
against the tape's length) each end the replay with a message on stderr and
exit code 1. The replay runs no learned policy (the policy is not in the
tape, and the one on disk is the one the run left, not the one it ran with),
so a tape recorded with one replays only when the policy left the run's
choices as the defaults; record with `--no-learn` to replay reliably. A
replay's retrieval reads the library as it is now, which the recorded run may
have added to: a brief that differs is not compared, but a retrieval event
that differs (`Facts`, `LibraryOffered`) ends the replay. The start events
and the `LedgerUnread` lines are written around the orchestrator, as the
guarantee above says, so they are neither on the tape nor compared; the
watcher's lines are not replayed at all.

The graph search has the recorder and the replayer too, in memory, since both
are polymorphic: `recordOrchestrator jobKey` wraps its orchestrator, and
`GraphIdentitySpec` records a real graph run and replays it to the same
events. Its tape has no file format and no flags (see below), and the graph
search logs its events through `geLogG`, not through the orchestrator, so a
graph tape compares the orchestrator's answers only.

## What is not here

- A type class or a transformer over the record. The record is a plain value
  that the searches are handed.
- The process swarm in `Fisher.Swarm` does not use the record.
- A file format for graph recordings. A graph tape's results hold `Said` and
  `PromptPart` values, which are never built from a file (the role types'
  guarantee, see [roles.md](roles.md)), so `Step Out` has no `FromJSON` and
  `fisher search --graph` takes neither `--record` nor `--replay`.
- The learned policy in the tape. A replay runs none, so a recording made with
  a learned policy is faithful only when the policy left the choices as they
  were.
- Lean or Lsp answers in a recording. A replay runs them again, and the
  events they lead to are compared with the tape's.
