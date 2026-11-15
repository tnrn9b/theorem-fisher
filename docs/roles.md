# Roles and provenance

Fisher's prompts are assembled from text with different origins: Fisher's
own templates, the user's words, project source, prover output and what a
model said. This page describes the types that keep those apart, so that a
mistake such as putting a judge's reply into a worker's prompt does not
compile.

Two layers. `Fisher.Role` (fisher-core) says who spoke and where a piece of
prompt text came from. `Fisher.Role.Agent` (fisher-search) is the monad a
model-call job runs in, so that a job can do only what its role may do.

## The types

```haskell
data Role = Worker | Planner | Verifier | Judge
roleName :: Role -> Text            -- "worker", "planner", "verifier", "judge"

class KnownRole (r :: Role)         -- roleVal: the Role value of a promoted role

newtype Said (r :: Role)            -- what a model in role r replied
saidText :: Said r -> Text
worker   :: Text -> Said 'Worker    -- a backend's attempt result
```

`Said` has no exported constructor. A `Said r` is made by `Fisher.Role.Agent.ask`
(and so by `askTwice`), by `worker`, by `excerpt` and `oneLineOf` (which cut a
piece out of a reply and keep its role), and by `Fisher.Learn.Judge.parseVerdict`
(which cuts a judge's reply into the parts of a `Verdict`). Anywhere else a
`Said r` can be received and passed on, never made.

```haskell
data PromptPart                     -- constructor not exported; Semigroup, Monoid
partText :: PromptPart -> Text
static   :: Text -> PromptPart      -- Fisher's own text: templates, headings, hints, the guard sentence
user     :: Text -> PromptPart      -- the user's own text: "## Yours" lines, --statement, a chat message
source   :: Text -> PromptPart      -- project source and facts: declarations, lemmas, docs
tool     :: Text -> PromptPart      -- prover and tool output: diagnostics, a counterexample, a model
parts    :: [PromptPart] -> PromptPart   -- strip each, drop blanks, two newlines between
oneLinePart :: Int -> PromptPart -> PromptPart  -- on one line, cut at n: only takes text away
type Prompt = [(Text, PromptPart)]  -- message role ("system", "user", "assistant") and its text

class Promptable (r :: Role) where toPrompt :: Said r -> PromptPart
-- instances: 'Worker, 'Planner, 'Verifier. None for 'Judge.
```

`PromptPart` has a `Semigroup` and `Monoid` instance (plain concatenation),
so a part built from parts each made by a constructor keeps its provenance
story. `parts` is the brief's joining rule (it strips trailing whitespace only, with `stripEnd`, and drops parts that are blank after a full strip); `<>` joins with nothing between.

```haskell
data Candidate                      -- constructor not exported
candidate     :: Said r -> Maybe Candidate    -- the only way a model's text becomes lesson evidence
template      :: Text -> Maybe Candidate      -- a rule lesson rendered from data/lessons/templates.yaml
candidateText :: Candidate -> Text
```

Both `candidate` and `template` run the same gate: the text is redacted
(`redactSensitiveText`, repeated until it stops changing, at most three
passes, because one pass can leave the edges of a long token behind), then
sanitised (`sanitiseLesson`). They give `Nothing` when the sanitiser refuses
the text or nothing is left, so every `Candidate` has passed the gate.

### The role monad

```haskell
newtype AgentM (r :: Role) a        -- Functor, Applicative, Monad; no MonadIO
data RoleCtx (r :: Role) = RoleCtx
  { rcAsk   :: [Message] -> IO (Either AskError (Text, Double))
  , rcCheck :: Maybe Checker        -- Nothing for a role that may not check
  , rcSpend :: Double -> IO ()      -- adds to this role's dollars in the run's cost record
  , rcEvent :: Text -> Value -> IO ()
  }

roleCtx   :: KnownRole r => Ask -> Maybe Checker -> (Role -> Double -> IO ()) -> (Role -> Text -> Value -> IO ()) -> RoleCtx r
runRole   :: RoleCtx r -> AgentM r a -> IO a

ask       :: Prompt -> AgentM r (Either AskError (Said r, Double))
askTwice  :: (Maybe (PromptPart, e) -> Prompt) -> (e -> PromptPart)
          -> (Said r -> AgentM r (Either e a)) -> AgentM r (Asked a)
check     :: CanCheck r => Text -> [Text] -> AgentM r Probe
withChecker :: CanCheck r => (Checker -> IO a) -> AgentM r a
event     :: Text -> Value -> AgentM r ()
excerpt   :: (Text -> Either e (Text, a)) -> Said r -> Either e (Said r, a)
oneLineOf :: Int -> Said r -> Said r

class CanCheck (r :: Role)          -- instances: 'Planner, 'Verifier
```

`AgentM` exports no way to run IO except these operations. A job that needs
another effect has to add it to `RoleCtx`, which is where a reviewer sees it.

- `ask` makes one model call as `r`, turns the prompt into text at the hand-off
  to the model, and charges the dollars to `r`.
- `askTwice` makes at most two calls. A failed call (`AskFailed`) is retried
  once with the builder given `Nothing` again; `AskNoCredit` ends the job at
  once. A reply the reader refuses gets one more call, and only then is the
  builder given `Just (previousReplyPart, problem)`; the previous reply arrives as a
  `PromptPart` made inside the module, so a role sees its own refused reply
  again (the judge too, which has no `toPrompt`) and the builder never holds
  the `Said`. The reader runs in `AgentM r` and gets what `r` said. Asynchronous
  exceptions are rethrown; any other exception ends the job as a failure that
  keeps the dollars already spent. The job's error (`Asked Double (Either
  PromptPart a)`) is a `PromptPart`: a refused reply's problem as the reader
  labelled its pieces (the second argument gives it), a failed call as
  `static "the model call failed: " <> tool providerText`, an exception as
  `static "the call broke: " <> tool exceptionText`. GraphRun puts it on the
  whiteboard as it is, so the planner's words in a refused sketch never
  become tool output.
- `check` is the kernel probe under the checker's lock; `withChecker` runs any
  other kernel check a job needs (the planner's sketch check) under the same
  lock. Both need `CanCheck r`.
- `excerpt` cuts a piece out of a reply (a blueprint block, a name, a reason)
  and keeps the role: the piece is still `r`'s words. The reader must be a
  pure function of the reply alone; a reader whose closure supplies other text
  as the piece is a review error. `oneLineOf n` puts a reply on one line and
  cuts it at `n` characters.

The jobs:

```haskell
planJob      :: Text -> Text -> Text -> [Text] -> PromptPart -> [Text] -> PromptPart -> AgentM 'Planner (Asked Plan)
reviewJob    :: Text -> Plan -> PromptPart -> AgentM 'Verifier (Asked SplitVerdict)
falseJob     :: Text -> Text -> AgentM 'Verifier (Asked (Bool, Said 'Verifier))
judgeAttempt :: AttemptView -> AgentM 'Judge (Asked Verdict)
parseVerdict :: Said 'Judge -> Either Text Verdict
```

`Verdict` holds `Said 'Judge` fields (`vLesson :: Maybe (Said 'Judge)`), so a
judge's text stays a judge's text until `candidate` turns it into lesson
evidence.

## The crossings

Every place text enters a prompt takes a `PromptPart`. What shipped:

| Where | Type | Provenance |
| --- | --- | --- |
| `Fisher.Lessons.Prompt.PromptLessons.plBlock` | `PromptPart` | rows pass `sanitiseLesson`, then `static`; `## Yours` lines are `user`; the guard sentence is `static` |
| `Fisher.Lessons.File.renderLessonsBlock` | `PromptPart` | as above |
| `Fisher.Workflow.Stage.appendLessons` (lessons block, startup context, Codex instructions) | takes a `PromptPart` and applies `partText` | the fetched skill and the template text are appended as `Text` by the workflow stage and are not `PromptPart`s yet |
| `Fisher.Search.Retrieval.rtBrief`, `reExtra`, `smtFacts`; `Fisher.Search.Facts.factsBrief`; `Fisher.Search.Library.libraryBrief` | `PromptPart` | facts and lemmas are `source`, the SMT paragraph is `static` |
| `Fisher.Search.Scheduler.asBrief` and the disproof hint | `PromptPart` | the counterexample and model text are `tool` |
| `Fisher.Search.GraphRun`: a planner's blueprint and sketch in worker briefs and on the board | `PromptPart` | `Said 'Planner` through `toPrompt`; a worker's note is `toPrompt . worker` |
| `Fisher.Search.Review.reviewMessages`: the blueprint and the sketch under review | `Said 'Planner` | `plBlueprint` and `plLean` are read out of the planner's reply with `excerpt` and placed with `toPrompt`; `saidText plLean` only where the kernel-accepted text is needed |
| A job's error on the whiteboard (`Asked`'s `Left`, `refit`, a split the graph refuses) | `PromptPart` | Fisher's sentence `static`, a provider's or an exception's text and Lean's errors `tool`, a name from the sketch the planner's words (`excerpt`) |
| `askTwice`: the previous reply on a retry | `PromptPart` | the same role's own reply, made inside `Fisher.Role.Agent` |
| `Fisher.Learn.Judge.Verdict` | `Said 'Judge` fields | no `toPrompt`, so it cannot reach a prompt |
| `Fisher.Learn.Lessons.Evidence.evText` | `Candidate` | `recordEvidence` does not sanitise: the gate already ran |
| `Fisher.Learn.Mine` | builds `Evidence` | `candidate` applied to the unwrapped `Said` inside `vLesson`'s `Maybe`, `candidate (vTellYourself v)`, and `template rendered`; a refused text makes no lesson and is counted |

Where a brief is logged or shown (events, `report.json`, the UI), `partText`
is applied at the edge. `Message` in `Fisher.Search.Split` stays
`(Text, Text)`; `ask` takes a `Prompt` and converts in one place.

Not covered: the fetched Lean skill and the workflow templates are read as
`Text` and appended as `Text` by the workflow stage (they are Fisher's inputs,
not a model's, and are not `PromptPart`s yet); the native agent loop's own messages are untouched (they are pinned
by parity goldens).

## What the compiler refuses

`tools/test/typecheck-negative.sh` compiles two snippets in
`tools/test/negative/` with `ghc -fno-code` and passes only when each fails
with the expected error. `toPrompt` on a `Said 'Judge` fails with
`No instance for 'Promptable Judge'`; `check` in `AgentM 'Judge` fails with
`No instance for 'CanCheck Judge'`. `tools/test.sh` runs the script after the
suites.

## Adding a job

1. Pick the role. A job that writes to the project, or that needs another
   effect, adds it to `RoleCtx` first (and a class like `CanCheck` when only
   some roles may use it).
2. Build the context where the job is run, with `roleCtx` from the run's `Ask`,
   the checker (`Nothing` for a role that does not check; the judge's checker is always `Nothing`)
   and the run's spend and event writers:
   `roleCtx @'Planner (geAsk env) (Just ck) spend logEv`. Run the job with
   `runRole`.
3. Write the job in `AgentM r`. Use `askTwice` when a refused reply deserves a
   second call; put the parsing in the reader.
4. Read replies with `excerpt` (or `oneLineOf` for a failure note) so a piece of
   a reply stays a `Said r`. Do not rebuild one from `saidText`.
5. Place text with the constructor its provenance demands: `static` for
   Fisher's text, `user` for the user's, `source` for project facts, `tool` for
   prover output, `toPrompt` for a role's reply that may be reused. If the new
   role's reply must not be reused directly (as the judge's), give it no
   `Promptable` instance and let it reach the lessons only through
   `candidate`.
