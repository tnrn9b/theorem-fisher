# Roles and provenance: typed prompt parts and a role-indexed monad for the model-call jobs

Date: 14 Nov 2026. Status: for Tom's review. Stages 1 and 2 of the examination in the project note "a swarm monad transformer and a role-indexed agent monad"; stages 3 to 5 (an orchestration class over the Scheduler and GraphRun) are not in this spec.

## 1. What and why

Text produced by a model reaches prompts along several paths: a planner's blueprint goes into worker briefs, a model's earlier reply goes back to the same model on a retry, a judge's lesson goes into the lessons block of every later run. The lessons work (Oct 2026) found that the last path needed a sanitiser, a key namespace and a pin gate, all enforced at run time and found by review. The checks that a verifier never writes to the project, a judge never spends solver time, and a judge's text never reaches a prompt except through the candidate pipeline are conventions today.

This stage makes two of those structural:

- Provenance in the types. Prompts are assembled from `PromptPart` values, and the only ways to make one are from Fisher's own text, the user's own text, project source, tool output, or a model's text whose role is allowed to speak to prompts. A judge's text has no such function; it becomes a `Candidate` and nothing else.
- A role-indexed monad for the model-call jobs. The planner's split, the verifier's review and false-claim check, and the lessons judge run in `AgentM r`, where `r` is the role and the operations a role may perform are type-class constrained. The native agent loop and the search's worker attempts are not changed: a worker stays an opaque run whose output comes back as `Said 'Worker`.

## 2. Types (fisher-core, new module `Fisher.Role`)

```haskell
data Role = Worker | Planner | Verifier | Judge deriving (Eq, Ord, Show, Enum, Bounded)
roleName :: Role -> Text                      -- "worker", "planner", "verifier", "judge" (the run log's spelling)
class KnownRole (r :: Role) where roleVal :: proxy r -> Role   -- the four instances

newtype Said (r :: Role) = Said { saidText :: Text } deriving (Eq, Show)
-- Constructed only by `Fisher.Role.Agent.ask` (section 4) and by `worker :: Text -> Said 'Worker`
-- (the search's attempt results, which come from a backend, not from AgentM).

newtype PromptPart = PromptPart Text deriving (Eq, Show)   -- constructor not exported
partText  :: PromptPart -> Text
static    :: Text -> PromptPart     -- Fisher's own text: templates, headings, the guard sentence, hints
user      :: Text -> PromptPart     -- the user's own text: `## Yours` lines, --statement, a chat message
source    :: Text -> PromptPart     -- project source and facts read from the project (declarations, lemmas, docs)
tool      :: Text -> PromptPart     -- tool and prover output: Lean diagnostics, plausible's counterexample, the solver's model
parts     :: [PromptPart] -> PromptPart   -- joinBrief's rule: stripEnd each, drop blanks, two newlines between
class Promptable (r :: Role) where toPrompt :: Said r -> PromptPart
instance Promptable 'Worker; instance Promptable 'Planner; instance Promptable 'Verifier
-- no instance for 'Judge

newtype Candidate = Candidate Text        -- constructor not exported
candidate :: Said r -> Maybe Candidate    -- redactSensitiveText then sanitiseLesson; Nothing when the sanitiser refuses
template  :: Text -> Candidate            -- a rule lesson rendered from data/lessons/templates.yaml
candidateText :: Candidate -> Text
```

`Fisher.Lessons.File.sanitiseLesson` and `Fisher.Redact.redactSensitiveText` already exist; `candidate` is their composition and the only way a model's text becomes lesson evidence. `Fisher.Search.GraphJobs` re-exports `Role (..)` from `Fisher.Role` so no caller moves.

## 3. The crossings (stage 1)

Every place text enters a prompt takes a `PromptPart`. The list, from the code:

| Where | Today | After |
| --- | --- | --- |
| `Fisher.Lessons.Prompt.PromptLessons.plBlock` | `Text` | `PromptPart`: rows pass `sanitiseLesson` (as now) then `static`; `## Yours` lines are `user`; the guard sentence is `static` |
| `Fisher.Lessons.File.renderLessonsBlock`, `blockFrom` | `Text` lines | unchanged internally; `renderLessonsBlock` returns `PromptPart` |
| `Fisher.Workflow.Plan` (the lessons block in the system prompt, startup context, Codex instructions) | appends `Text` | appends `partText` of a `PromptPart`; the prompt assembly function takes `[PromptPart]` for the parts that are not the fetched skill or the template |
| `Fisher.Search.Retrieval.rtBrief`, `joinBrief`, `smtFacts`, `factsBrief`, `libraryBrief` | `Text` | `PromptPart`; facts and lemmas are `source`, the SMT paragraph is `static`, `reExtra` is `PromptPart` |
| `Fisher.Search.Scheduler.asBrief` and the disproof hint ("possible counterexample") | `Text` | `PromptPart`; the counterexample and model text are `tool` |
| `Fisher.Search.GraphRun`: a planner's blueprint and sketch into worker briefs and the board | `Text` | `Said 'Planner` through `toPrompt` |
| `Fisher.Search.GraphJobs.askTwice`: the previous reply fed back on a retry | `Text` | `Said r` through `toPrompt` (the same role) |
| `Fisher.Learn.Judge.Verdict` | `vMistake, vLesson, vTellYourself :: Text` | `Said 'Judge`, with `vLesson :: Maybe (Said 'Judge)` |
| `Fisher.Learn.Lessons.Evidence.evText`, `recordEvidence` | `Text`, sanitised inside | `Candidate`; `recordEvidence` no longer sanitises (the constructor did) |
| `Fisher.Learn.Mine` | builds `Evidence` from the verdict and from templates | `candidate (vLesson v)`, `candidate (vTellYourself v)`, `template t` |

Where the brief is logged or shown (events, report.json, the UI), `partText` is applied at the edge. `Message` in `Fisher.Search.Split` stays `(Text, Text)`; the role-indexed `ask` (section 4) takes `[PromptPart]` for the user side of each message and `static` for the system side, so the conversion happens in one place.

What this does not change: the fetched Lean skill and the workflow templates are read as `Text` and wrapped with `static` at the point the prompt is assembled (they are Fisher's inputs, not a model's); the native agent loop's own messages are untouched (its prompts are built by upstream's code paths and pinned by parity goldens).

## 4. The role monad (stage 2, fisher-search, new module `Fisher.Role.Agent`)

```haskell
newtype AgentM (r :: Role) a = AgentM (ReaderT (RoleCtx r) IO a)
  deriving (Functor, Applicative, Monad)        -- no MonadIO instance is exported

data RoleCtx (r :: Role) = RoleCtx
  { rcAsk    :: [Message] -> IO (Either AskError (Text, Double))   -- the existing Ask, applied to this role
  , rcCheck  :: Maybe Checker            -- Nothing for roles that may not check
  , rcSpend  :: Double -> IO ()          -- adds to this role's dollars in the run's cost record
  , rcEvent  :: Text -> Value -> IO ()   -- a run-log event, tagged with the role
  }

ask       :: [Message] -> AgentM r (Either AskError (Said r, Double))   -- charges the dollars through rcSpend
askTwice  :: (Maybe (Said r, e) -> [Message]) -> (e -> Text) -> (Text -> IO (Either e a)) -> AgentM r (Asked a)
check     :: CanCheck r => Text -> [Text] -> AgentM r Probe              -- the kernel probe, under the checker's lock
event     :: Text -> Value -> AgentM r ()
runRole   :: RoleCtx r -> AgentM r a -> IO a
roleCtx   :: forall r. KnownRole r => Ask -> Maybe Checker -> (Role -> Double -> IO ()) -> (Role -> Text -> Value -> IO ()) -> RoleCtx r

class CanCheck (r :: Role); instance CanCheck 'Planner; instance CanCheck 'Verifier
-- no CanWrite: no job in this stage writes to the project; the class is added when one does
```

The jobs become:

```haskell
planJob   :: Text -> Text -> Text -> [Text] -> PromptPart -> [Text] -> PromptPart -> AgentM 'Planner (Asked Plan)
reviewJob :: Text -> Plan -> PromptPart -> AgentM 'Verifier (Asked SplitVerdict)
falseJob  :: Text -> Text -> AgentM 'Verifier (Asked (Bool, Text))
judgeAttempt :: AttemptView -> AgentM 'Judge (Asked Verdict)
```

GraphRun builds one `RoleCtx` per role from `geAsk` and the checker (`roleCtx @'Planner (geAsk env) (Just ck) spend logEv`, and so on) and runs each job with `runRole`; `Fisher.Learn.Mine` builds the judge's context with `rcCheck = Nothing`. The per-role dollars go to the existing cost record of the run (`GraphReport` costs and the `cost` events) under the role's name; nothing new is stored.

Why no `MonadIO`: a job that needs an effect not in the list has to add it to `RoleCtx`, which is where a reviewer sees it. The module's internals use `liftIO`; nothing outside can.

## 5. Tests

- `Fisher.RoleSpec` (fisher-core): `parts` equals `joinBrief` on the same strings; `candidate` refuses what `sanitiseLesson` refuses and redacts what `redactSensitiveText` redacts; `template` keeps the text.
- A negative compile check that `toPrompt (x :: Said 'Judge)` and `check` in `AgentM 'Judge` are rejected: with the `should-not-typecheck` package if the snapshot has it, else a `tools/test/typecheck-negative.sh` that runs `stack ghc -- -fno-code` on two snippets and expects the errors.
- `Fisher.Role.AgentSpec` (fisher-search): `ask` charges `rcSpend` with the call's dollars and tags the event with the role; `askTwice` retries once on a refused reply and feeds the previous `Said r` back; `check` runs under the checker's lock (a counter proves one probe at a time); `runRole` is the only entry.
- Existing specs updated for the signatures (GraphRunSpec, GraphJobs tests, JudgeSpec, MineSpec, LessonsSpec, the Retrieval and Scheduler specs, PlanGoldenSpec for the workflow prompts). Goldens must not change: the prompts' text is identical, only its type differs. Any golden diff is a defect.

## 6. Errors

No new runtime behaviour: `candidate` returning `Nothing` is today's "sanitiser refused" path; `ask` failing is today's `AskFailed`. `partText` at the edges keeps every event, report and UI field as it is.

## 7. Not in this stage

A `MonadOrchestrate` over the Scheduler and GraphRun with a simulated swarm; recording and replaying searches; converting `Fisher.Agent.Loop` or `Fisher.Swarm`; `CanWrite` (no job writes yet); role-specific tool sets for the native agent.
