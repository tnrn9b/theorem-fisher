# Recursive language models: `fisher rlm`

This is the user guide for the `fisher rlm` command and the `fisher-rlm`
library. The design is in
`docs/superpowers/specs/2026-10-13-rlm-design.md`. Where this page and the
spec disagree, the code is the reference.

## 1. What it is

A recursive language model (RLM) answers a question about a context that is
too long to read in one model call. The context is not put in the prompt. It
sits in a Python REPL as a variable called `context`. The root model sees
only the question and a short note about the context (its type and length).
It writes code in ```` ```repl ```` blocks to look at the context. That code
can call other models: `llm_query` asks a sub-model about a piece of text, and
`rlm_query` starts a child RLM on it. The root model never reads the whole
context. It ends the run by setting `answer["content"]` and
`answer["ready"] = True`.

Fisher ports the method from Zhang, Kraska and Khattab (arXiv:2512.24601) and
their library, github.com/alexzhang13/rlm, at commit
`d04208afbad29ca675ab13478c40ee8bebc84bfe`. Fisher runs the loop and every
model call in Haskell. The Python side only runs code and calls back to Fisher
over pipes.

## 2. Quick start

One file as the context:

```sh
fisher rlm --context report.txt --query "Which section mentions the audit?"
```

Several files. `context` becomes a list of strings, in the order given:

```sh
fisher rlm --context a.txt --context b.txt --context c.txt \
  --query "Which of these files disagree about the release date?"
```

A `.json` file is loaded as JSON, so `context` is a list or a dict. Use `-` to
read one context from stdin:

```sh
fisher rlm --context records.json --query "How many records have status 'open'?"
cat big.log | fisher rlm --context - --query "What was the first error?"
```

Machine-readable output, with a cost cap and a trajectory log:

```sh
fisher rlm --context report.txt --query "Summarise the findings." \
  --json --max-budget 2.00 --log-dir ./rlm-logs
```

## 3. The command

```
fisher rlm [--context FILE|-]... [--query TEXT] [--model M] [--sub-model M]
           [--max-depth N] [--max-iterations N] [--max-budget USD]
           [--max-timeout S] [--max-errors N] [--block-timeout S]
           [--log-dir DIR] [--json] [--python PATH] [--script FILE]
           [--record-prompts FILE] [--replay LOG]
```

At least one `--context` is required. `fisher rlm --help` prints the same list.

| Flag | Meaning | Default |
| --- | --- | --- |
| `--context FILE\|-` | A context file; `-` reads stdin (once only). One file gives a `str`, several give a `list[str]` in order, a `.json` file is loaded as JSON. Bytes that are not UTF-8 are replaced, with a note on stderr. | required |
| `--query TEXT` | The root prompt: the question. | none |
| `--model M` | The root model. | `anthropic/claude-sonnet-5.5` |
| `--sub-model M` | The model for sub-calls. | the root model |
| `--max-depth N` | Recursion depth. `0` makes one plain call on the context. | 1 |
| `--max-iterations N` | Turns per run. | 30 |
| `--max-budget USD` | Stop once the whole tree of calls has cost more. | none |
| `--max-timeout S` | Stop once the run has taken longer than S seconds. | none |
| `--max-errors N` | Stop after N consecutive turns with errors. | none |
| `--block-timeout S` | Seconds of Python time allowed per block; time spent serving its model calls does not count. | 300 |
| `--log-dir DIR` | Write a trajectory log (JSONL) in DIR. | off |
| `--json` | Print the result as JSON. | off |
| `--python PATH` | The Python interpreter (3.9 or newer). | `python3` |
| `--script FILE` | Answer model calls from a JSON list of replies, not the network. An entry `{"sub": "P"}` answers every sub-call with P followed by the first 20 characters of the sub-call's last user message. Cannot be used with `--replay`. | off |
| `--record-prompts FILE` | Write every message list sent to a model, as JSON. | off |
| `--replay LOG` | Answer model calls from a replay log, not the network. | off |

The numeric flags must be positive, except `--max-depth`, which may be 0.

### Exit codes

| Code | Meaning |
| --- | --- |
| 0 | Answered, or the default answer after the iteration limit. |
| 1 | The run was stopped by a limit or an error, or it failed to start (bad context file, no usable Python, no provider). With `--replay`, also 1 when a request had no logged response. |
| 2 | Bad arguments or a bad `fisher.rlm` config. |
| 3 | No credit (HTTP 402). The run stops at once, with no retry. |

Config warnings and errors go to stderr, so stdout holds only the answer or
the JSON.

### The `--json` fields

| Field | Content |
| --- | --- |
| `answer` | The answer text. |
| `ending` | `answered`, `default_answer` or `stopped`. |
| `reason` | For `stopped`, why (see section 5); otherwise `null`. |
| `iterations` | The number of turns the root loop took. |
| `usage.models` | Per model id: `calls`, `input_tokens`, `output_tokens`, `cost_usd`. |
| `usage.total_cost_usd` | The sum of the costs, or `null` when no cost was reported. |
| `log` | The trajectory file path, or `null`. |
| `replay_log` | The replay log path (section 7), or `null`. |

## 4. Configuration

Settings can also be given in the `fisher.rlm` section of `config.yaml`.
Flags win over the file. Unknown keys and non-positive values are refused by
name, and the command exits with 2.

```yaml
fisher:
  rlm:
    model: anthropic/claude-sonnet-5.5
    sub_model: anthropic/claude-sonnet-5.5
    max_depth: 1
    max_iterations: 30
    max_budget: 5.0
```

| Key | Meaning | Default |
| --- | --- | --- |
| `model` | The root model. | `anthropic/claude-sonnet-5.5` |
| `sub_model` | The model for sub-calls. | the root model |
| `max_depth` | Recursion depth (positive here; only the flag accepts 0). | 1 |
| `max_iterations` | Turns per run. | 30 |
| `max_concurrent_subcalls` | How many calls of a batched query run at once. | 4 |
| `truncate` | Characters of REPL output shown to the model. | 20000 |
| `block_timeout` | Seconds of Python time allowed per block; time spent serving its model calls does not count. | 300 |
| `max_budget` | Dollars for the whole tree. | none |
| `max_timeout` | Seconds for the whole run. | none |
| `max_tokens` | Tokens for the whole tree. | none |
| `max_errors` | Consecutive error turns allowed. | none |

`max_concurrent_subcalls`, `truncate` and `max_tokens` have no flag.
`min_offload` and `offload_gap` are read here too but only the agent uses
them (section 11); `fisher rlm` accepts and ignores them.

## 5. How a run ends

- **Answered.** A code block leaves `answer["ready"]` true (set it in the
  block, or rebind `answer` to a dict with `ready` true). The `content`, as a
  string, is the answer. Exit code 0.
- **Default answer.** The model used all its iterations without an answer.
  Fisher sends the history plus a request for a final answer and one more
  call, and its text is the answer. Exit code 0.
- **Stopped.** A limit or an error ended the run early. The best answer so far
  is printed, a line `fisher rlm: stopped: REASON` goes to stderr, and the
  exit code is 1. The reasons are:
  - `budget`, `timeout` or `tokens`: `max_budget`, `max_timeout` or
    `max_tokens` was passed. They count the whole tree and are checked after
    each turn. The budget is also checked before every model call: a call
    starts only if the dollars spent, the estimates of the calls still in
    flight and its own estimate fit in `max_budget`. The estimate is a
    quarter of the call's characters at the run's dollars per prompt token
    so far (0 before any reply has reported a cost, so the first call always
    goes, and so do the calls that start alongside it, unreserved). A root
    call refused this way stops the run with `budget`.
  - `errors`: `max_errors` consecutive turns had an error. The answer is the
    newest non-empty reply.
  - `refused` or `empty`: the model refused, or returned nothing, after
    retries.
  - `model error: ...`: any other model failure.
  - `repl: <why>`: the Python REPL could not start (for example, no
    `python3` 3.9 or newer).
  - `internal: <why>`: an unexpected failure inside Fisher while running the
    root's REPL. The answer is the best one so far.
- **No credit.** The provider answered 402. The run stops at once with the
  reason `no credit` and exit code 3.

A failed sub-call does not stop the run. Its error text (`Error: ...`) is
returned to the code that made the call. A sub-call refused on the budget
returns `Error: Budget exhausted (spent $X of $Y)`, the text a child run
refused on the budget returns.

## 6. Safety

The model's code is not sandboxed.

- It runs as you, in a fresh temporary directory, with your file access and
  your network access.
- The Python process never receives an API key in its environment. Fisher
  drops every variable whose name, compared in upper case, ends in `_API_KEY`
  or `_TOKEN`, or starts with `OPENROUTER_`, `OPENAI_` or one more provider
  prefix (the full list is `secretName` in `fisher-rlm/src/Fisher/RLM/Repl.hs`).
  No other variable is removed, so secrets under other names stay. Model calls
  go through Fisher, which holds the key. The model's code still runs as you,
  so it can read credential files on disk, such as `$FISHER_HOME/.env`.
- The protocol runs on private copies of the process's stdin and stdout, so
  code that prints cannot forge protocol messages.
- `python3` 3.9 or newer is required. Fisher checks this before the first
  model call and refuses to start otherwise. Use `--python PATH` to choose
  another interpreter.

Do not run `fisher rlm` on a context you do not trust, with a model you do not
trust, on a machine with files you cannot lose.

## 7. Logging and replay

**Trajectory log.** `--log-dir DIR` writes `DIR/rlm_<time>_<id>.jsonl`. The
first line is metadata. Then there is one line per iteration: the prompt, the
response, the code blocks with their stdout, stderr and sub-calls, and the
final answer. The shape is the one the reference's `RLMLogger` writes, so the
reference's visualizer opens these files. Each line is written and closed on
its own, so a run that dies leaves a valid file. In these logs a block's
`locals` map each variable name to its type name, not its value; execution
time is 0.0; per-call usage is empty.

**Replay log.** Unless you use `--script` or `--replay`, every model call is
also written to Fisher's replay log, `$FISHER_HOME/fisher/replays/rlm_<id>.jsonl`.
The path is the `replay_log` field of the `--json` output.

**Replay.** `--replay LOG` runs the command again, answering each model
request from the log instead of the network. A request that is not in the log
fails, is reported on stderr, and makes the exit code 1. The context and
query must match the recorded run, and so must the model, the sub-model and
the limits, whether they come from config.yaml or from flags: replay builds
its requests from today's settings, so if any of these changed, every call is
a miss.

**Recording prompts.** `--record-prompts FILE` writes every message list sent
to a model, with its role (`root` or `sub`) and model, as JSON.

**Scripts.** `--script FILE` answers calls from a JSON list of reply strings,
in order, with no network. The tests and the cross-check use it.

## 8. Differences from the reference

These are deliberate.

- **Timeouts and restarts.** A block whose Python time runs past
  `--block-timeout` is killed. Only Python's own work counts: the allowance
  starts again after each `llm_query` or `rlm_query` call is answered, so
  slow model calls and child runs never time a block out. The Python process
  is restarted and `context` is loaded again. The turn's output says `REPL
  timed out after N s; the REPL was restarted and its variables were lost`. A process that exits unexpectedly is restarted the
  same way, and the output says it exited and with what code. The
  reference's local REPL has no block timeout, so this has no counterpart
  there, and the cross-check scripts do not exercise it.
- **The Python side is a separate script.** `fisher-rlm/data/rlm_repl.py` is
  a port of the reference's `LocalREPL` that uses only the standard library.
  It talks to Fisher over pipes, and no API key reaches it (section 6).
- **Phase 1 leaves out:** persistence across calls (`context_N`,
  `history_N`), compaction, custom tools, `user_prologue`, the
  non-orchestrator prompt, and the other environments (ipython, docker, modal,
  prime, daytona, e2b). Only the local Python REPL exists.
- **Trajectory logs** carry type names instead of variable values, and no
  execution times or per-call usage (section 7).
- **`--max-depth 0`** makes one plain call on the context, like the
  reference's `_fallback_answer`, and like it does not add `--query`. The
  context is sent as the user message: a string as one message; a list of
  `{"role", "content"}` dicts as those messages; anything else as its JSON
  text. Put the question in the context file for a plain call.
- **Model calls** go through Fisher's LLM client (OpenRouter by default), so
  Fisher's retries, response normalisation and replay log apply.
- **The budget before every call.** Every model call checks the budget
  before it starts (section 5), so a run holds to its `max_budget` to within
  the estimate's error. The reference checks the budget only between turns
  and before a child run starts, so one turn's plain sub-calls can run far
  past it.
- **Batch width.** `max_concurrent_subcalls` (default 4) bounds plain
  `llm_query_batched` batches as well as `rlm_query_batched` ones. The
  reference bounds only `rlm_query_batched` with it and runs up to 16 prompts
  of a plain batch at once, so a wide plain batch takes longer here.

## 9. Checking the port

`tools/rlm/` checks that Fisher sends the same messages as the reference.

```sh
tools/rlm/setup-reference.sh
.build/rlm-ref/bin/python tools/rlm/crosscheck.py
```

`setup-reference.sh` makes a venv at `.build/rlm-ref`, clones
github.com/alexzhang13/rlm, checks out the pinned commit and installs it.
`DRY_RUN=1` prints the commands without running them.

`crosscheck.py` reads the scripts in `tools/rlm/scripts/`. Each holds a
context, a query, an iteration limit and a list of scripted replies. It runs
the reference with a scripted client, and `fisher rlm --script
--record-prompts` on the same script. It then compares the message lists sent
to the model, call by call, role and content exactly. Root calls are compared
in order. Sub-calls within one root turn are compared as a multiset, because
batched calls run concurrently. Any mismatch, a non-zero Fisher exit, or a
script that does not exercise its calls counts as a difference. Use
`--fisher CMD` to say how to start Fisher. `--self-test` checks that the
comparison catches deliberate mutations. `tools/rlm/test_repl.py` tests the
Python REPL script.

## 10. Evaluation

The bench in `tools/bench/rlm/` compares the RLM with one plain model call on
tasks whose answers are known. It uses the Python standard library only.

**Tasks.**

| Task | Content | Answer known by |
| --- | --- | --- |
| needle | One planted sentence (`The magic number is N.`) among filler text. | construction |
| aggregation | A table of generated records; a question that counts or filters. | construction |
| TWISTED | Questions over the Lean sources under `Step/` (about 3M characters), each file after a `=== Step/<path> ===` line. | a script over the sources |

Needle and aggregation are generated at 100K, 1M and 5M characters, with 3
seeds each. TWISTED has 3 to 5 questions (default 4) over one shared context.
Every question asks for one number, and the answer is scored by extracting it.
Generation is deterministic.

**Arms.**

- **RLM:** `fisher rlm --context FILE --query Q --json --model M --max-budget B`.
- **Plain:** `fisher rlm --max-depth 0 --json ...` on a file made by `run.py`.
  The file is a JSON list of two messages. The system message
  (`PLAIN_SYSTEM`) says the documents are synthetic tests, mostly random
  filler words. The user message holds one framing line (`PLAIN_FRAME`:
  "Below is a long document used to test question answering over long
  inputs."), the context inside `<document>` tags, a blank line, and
  `Question: Q`. Without the system message the provider's filter refuses
  most of the plain needle calls. The plain arm is
  skipped, and recorded as "does not fit", when the context is over 3.5
  characters per token of `--context-tokens` (default 1,000,000).

**Refusals and cost.** A run the provider refuses without billing is tried
again, up to `--refusal-attempts` times in all (default 3), in both arms. The
record keeps the number of attempts. `--cap-usd` (default 25) stops the whole
evaluation: no run starts once the cumulative cost is at or over the cap. A
run that reports no cost is charged its `--max-budget` (default 3.0 a run) and
marked as estimated. A run that exits 3 (no credit) stops the evaluation and
is not recorded. Results are saved after every run, so an interrupted
evaluation resumes where it stopped.

**Running it.**

```sh
python3 tools/bench/rlm/gen.py                 # tasks under ~/.cache/fisher/bench/rlm/tasks
python3 tools/bench/rlm/run.py --cap-usd 25    # results.json beside the tasks
python3 tools/bench/rlm/report.py              # report.md beside the results
```

`gen.py` takes `--out`, `--seeds`, `--sizes`, `--tasks`, `--twisted DIR` (the
Lean sources) and `--twisted-questions`. `run.py` takes `--tasks-dir`,
`--results`, `--fisher`, `--model`, `--only`, `--arms`, `--run-timeout` and
`--script-dir` (dry runs with scripted replies). `report.py` writes a table of
accuracy, mean dollars and mean iterations for each task, size and arm, and a
verdict: where the plain call fits, RLM must be at least as accurate; where it
does not fit, RLM must answer. `tools/bench/rlm/test_bench.py` tests the
scripts.

### Results

Run on 13 Oct 2026 through OpenRouter, root and sub-call model
`anthropic/claude-sonnet-5.5`, 3 seeds a cell (4 questions for TWISTED, over
the 1,575,655 characters of `Step/`). Gate verdict: PASS. Total $16.26, of which
the RLM arm was $1.34 and the plain arm $14.92.

| task | size (chars) | RLM correct | RLM mean $ | RLM mean turns | plain correct | plain mean $ |
| --- | --- | --- | --- | --- | --- | --- |
| needle | 100K | 3/3 | 0.014 | 4.3 | 3/3 | 0.101 |
| needle | 1M | 3/3 | 0.014 | 4.7 | 3/3 | 1.000 |
| needle | 5M | 3/3 | 0.006 | 3.3 | does not fit | |
| aggregation | 100K | 3/3 | 0.015 | 5.3 | 0/3 | 0.132 |
| aggregation | 1M | 3/3 | 0.014 | 5.0 | 0/3 | 1.166 |
| aggregation | 5M | 3/3 | 0.011 | 4.7 | does not fit | |
| TWISTED | 1.6M | 4/4 | 0.281 | 9.0 | 3/4 | 1.930 |

RLM answered all 22 runs correctly. The plain call found every needle it was
given, missed every aggregation count (it estimates rather than counts) and
one of the four TWISTED counts. One TWISTED RLM run took 21 turns and $1.06;
every other RLM run cost under 3 cents.

The needle plain cells were run again after the plain arm gained its system
message: without it the provider refused 5 of the 6 plain needle calls, even
after 3 attempts each. The aggregation and TWISTED plain cells, never refused,
are from the first run, without the system message.

## 11. In the agent: `--context rlm`

Fisher's agent has three ways to keep a long session inside the model's
context: the compressor (`--context compress`, the default), CLM
(`docs/clm.md`) and this one. `--context rlm`, or `fisher.context: rlm` in
`config.yaml`, works on `fisher chat` and on the shell's native sessions.

**What it does.** At the point where the compressor would summarise, the
middle of the history moves out of the prompt into a store, unchanged, and the
agent gets a `recall` tool that runs a recursive language model over the store
to answer a question about those turns. Nothing is summarised and nothing is
lost; the agent pays for what it looks up.

**When it offloads.** At the compressor's points and threshold, with the
same estimate: before the first API call of a turn, after a tool batch, and
on a 413 or context-length error (up to 3 attempts). Before a turn and after
a batch, two more conditions must hold, so the prompt cache is not thrown
away every turn: at least `offload_gap` tool batches since the last offload
(a new or resumed session may offload at its first crossing), and a middle
to move of at least `min_offload` of the room, the threshold less the system
prompt and the protected head. When either fails the middle stays where it
is that batch, so the context can run somewhat over the threshold between
offloads; the tail cut below still applies, so a huge tool result in the
tail is cut at once. The 413 and context-length offloads are forced:
they ignore both conditions and behave as before. The compressor's head
(the first 3 messages) and tail (the last 4) stay in the prompt; the cut points
move outward so that a tool call and its results are never split. The memory
flush that runs before a compression also runs before an offload. No
auxiliary-model call is made, and the `state.db` session is not ended. If one
huge tool result in the tail keeps the history over the threshold (or there
is no middle to move, or the offload has to wait), each tail result over
20,000 characters is stored in full and replaced by its first 2,000
characters and a line saying where the rest is.

**The stub.** The moved turns are replaced by one user message, for example:
"Turns 4 to 37 of this session (34 messages, 212,400 characters) were moved out
of the context to save space. Nothing was summarised or lost. To get anything
from them, such as an earlier tool output, a decision, or a proof attempt, call
recall with a specific question." A later offload rewrites that stub to cover
the whole stored range, so there is only ever one.

**The `recall` tool.** It takes one argument, `question`. It is offered, and a
short paragraph about it is added to the system prompt, once the store holds a
turn (after the first offload or the first tail cut); before then the prompt is
the compressor's, byte for byte. A call
starts a fresh run of the engine in sections 1 to 10 over the store (one string
per stored turn, in order) and returns its answer. If the run is stopped by a
limit, the result is the partial answer followed by `(recall stopped: reason)`;
a REPL or model error comes back as the tool result and the agent carries on.
A 402 from the provider stops the session, as it does anywhere else. An empty
store answers `Nothing has been moved out of the context yet.` Recall runs on
the host, never in the terminal sandbox, so the docker and ssh backends work.
Its model calls count in the session's usage, and their dollars are part of
the session's cost and of the bench's cost. The agent has no dollar limit of
its own; recall's `max_budget` limits each recall, to within the estimate's
error, since every model call checks it before it starts (section 5). A
recall stopped that way returns its partial answer followed by `(recall
stopped: budget)`.

**Configuration.** Recall reads the `fisher.rlm` keys of section 4. Where a key
is absent the agent uses its own defaults: `max_iterations` 15 and
`max_budget` 0.50 dollars a recall (the rest as in section 4). The root and
sub-call model are the agent's own model and provider unless `model` or
`sub_model` is set. Two keys are the agent's alone: `min_offload` (default
0.25, a number in (0, 1]) is the least part of the room an offload must
move, and `offload_gap` (default 3, a non-negative integer) is the number of
tool batches that must pass after an offload before the next. Both are
logged with the strategy, so replay makes the same decisions; a log recorded
before they existed has neither key and replays with no gap and no size
test, as it ran.

**Resume and replay.** `$FISHER_HOME/fisher/ctx/<session>/rlm.json` holds the
store, the offload count and the stub's range, and is written after each
offload or tail cut. On `--resume` it is loaded and the live history is rebuilt
from `state.db`: the head, the stub, then the messages after the stored range,
with each tool result a tail cut stored shortened again to its preview.
`state.db` keeps the full transcript. The replay log gets an `rlm` event when
the strategy is set up, an `rlm_offload` event per offload (the range moved,
the stub, the characters this offload stored, the turns in the store), an
`rlm_cut` event per tail cut (the turns it stored and their characters) and an
`rlm_recall` event per recall (question, answer, how it ended, iterations,
model calls, dollars). Replay computes each offload and tail cut again and
reports a difference; recall's model calls are answered from the log.

**Where it is not available.** ACP and cron sessions keep the compressor; asking
for `rlm` there prints the same warning `clm` gets. Without a `python3` of 3.9
or newer, `--context rlm` is refused and there is no fallback to another
strategy: `fisher chat` exits with code 2 at startup, and the interactive shell
prints the refusal and carries on.

**Safety.** Recall runs the model's code in the same REPL as `fisher rlm`, so
section 6 applies unchanged: the code is not sandboxed, and it sees the stored
turns, which can hold anything the session read.

### Results

Run on 13 Oct 2026, Sonnet 5.5 through OpenRouter, on the CLM4 long session
(the 12 erased proofs of TWISTED's `Step/Kernel.lean`). The RLM arm offloads at
the threshold the compared compressor runs used. The compressor (run6-five)
and CLM (run7-follow) numbers come from earlier runs with an earlier binary.
Gate: not met at either budget.

| budget | strategy | sessions | closed | dollars |
| --- | --- | --- | --- | --- |
| 32K | RLM | 3 | 36/36 | 5.19 |
| 32K | compressor | 3 | 36/36 | 4.78 |
| 32K | CLM | 3 | 36/36 | 2.34 |
| 16K | RLM | 2 | 21/24 | 52.80 |
| 16K | compressor | 3 | 30/36 | 36.04 |
| 16K | CLM | 3 | 36/36 | 2.98 |

What the runs show:

- At 32K the agent never called recall. Each session offloaded 8 or 9
  times and closed all 12 proofs, at about 8% more than the compressor.
  The sessions re-read about as often (44 re-reads against 43); most of the
  gap is one short compressor session (34 turns, against 59 to 69).
- At 16K the threshold sits just above the pinned prompt, so nearly every
  turn offloads (162 and 209 offloads), which also keeps the prompt cache
  from working (55% and 44% of prompt tokens cached, against about 70% for
  the compressor).
- Recall was used at 16K (11 and 12 calls) and cost $8.38 and $21.64,
  about $0.76 and $1.80 a call, over the $0.50 `max_budget`. The RLM then
  checked its budget only after each of its turns, and one turn can fan out
  over a store of 2 to 3 million characters. Every model call now checks it
  before it starts (section 5).
- These runs predate `min_offload` and `offload_gap`, which were added
  after the 16K runs offloaded nearly every turn.
- The third 16K session was not run: the evaluation's $40 limit is checked
  between sessions, and the second 16K session took the total to $57.99.

#### Second run, with the budget check and the offload gap

Run on 14 Oct 2026 with `max_budget` checked before every model call and the
default `offload_gap` 3 and `min_offload` 0.25; the same task, model,
thresholds and comparison runs as above. $12.21 in all. Gate: 16K passes,
32K does not, so the gate as a whole is not met.

| budget | strategy | sessions | closed | dollars |
| --- | --- | --- | --- | --- |
| 32K | RLM | 3 | 36/36 | 5.84 |
| 32K | compressor | 3 | 36/36 | 4.78 |
| 32K | CLM | 3 | 36/36 | 2.34 |
| 16K | RLM | 3 | 36/36 | 6.37 |
| 16K | compressor | 3 | 30/36 | 36.04 |
| 16K | CLM | 3 | 36/36 | 2.98 |

- At 16K the gap took offloads from 162 and 209 a session to 9 to 12, the
  prompt cache from 44 to 55% of prompt tokens to 83 to 84%, and the cost
  from $18.39 and $34.41 a session to $1.87 to $2.44. The compressor at 16K
  still compresses on nearly every turn (476 summaries over its three
  sessions); it has no such gap, so part of RLM's lead there is the gap
  rather than the store.
- Recall was called once in six sessions (32K, rep 2) and cost $0.52
  against its $0.50 budget, within the estimate's error.
- At 32K RLM costs about 22% more than the compressor. The sessions re-read
  about as often (44 re-reads against 43), but RLM took 190 turns against
  the compressor's 163, most of the gap being one short compressor session
  (34 turns).
- CLM remains the cheapest strategy at both budgets.

#### Phase 3: a store for CLM

RLM's store and `recall` are also used by CLM: with `fisher.clm.store: true`
CLM spills oversized tool results to a store, archives what its edits, cuts
and rollbacks remove, and offers `read_stored` and `recall` over it, so that
tight budgets (8K, 4K) still work. See "The store" in `docs/clm.md`.

## 12. Licence

Fisher RLM is a port of the Recursive Language Models reference
implementation, https://github.com/alexzhang13/rlm at commit
`d04208afbad29ca675ab13478c40ee8bebc84bfe`. Copyright (c) 2026 Alex Zhang,
under the MIT licence. The licence text is in `fisher-rlm/NOTICE`. Prompt text
and behaviour are ported from the reference. Each ported module names its
source file and commit.

The method is from Zhang, Kraska and Khattab, arXiv:2512.24601.
