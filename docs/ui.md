# The web app (`fisher ui`)

`fisher ui` serves a local web app over the files Fisher already writes:
the sessions in `state.db`, their replay logs, step logs and context
files, the `fisher search` runs, the benchmark results, and the settings
in `config.yaml` and `.env`. It can also start, watch and stop runs. It
reads and writes the same Fisher home (`FISHER_HOME`; the old `GAUSS_HOME`,
`~/.gauss` are the fallback) as every other Fisher command.

## Start

```
fisher ui [--port N] [--no-open]
fisher ui --prune DAYS
```

The server listens on 127.0.0.1 only, port 8732 by default (`--port 0`
picks a free one). Each start makes a new random 128-bit token and prints

```
Fisher UI: http://127.0.0.1:8732/?t=<token>
```

and opens that URL in the browser (`open` on macOS, `xdg-open`
elsewhere) unless `--no-open` is given. The benchmark gate scripts
(`tools/bench/<b>/report.py`) are run from the Fisher repository: the one
`fisher.repo` in `config.yaml` names (`fisher config set fisher.repo
~/IdeaProjects/Fisher`), else the first directory at or above the
executable's that has `tools/bench` (a build run from its checkout), else
the directory the server is started in.
Ctrl-C, SIGTERM or SIGHUP stops it, and with it every run and gate child
it started.

The page is the React app under `ui/`, built into `ui/dist` and embedded
in the executable at build time; `ui/dist` is committed, and a test fails
when a file under `ui/src` is newer than `ui/dist/index.html`. After
changing the page, run `npm run build` in `ui/` and rebuild.

## Access

Only the browser on this machine that was given the printed URL can use
the app:

- `GET /?t=<token>` trades the token for an HttpOnly, SameSite=Strict
  cookie `fisher_ui_<port>` and redirects to `/`. A token from an earlier
  start is refused. The port is in the cookie's name because a browser
  keeps cookies per host, not per port: two servers on one machine keep
  one cookie each.
- Every `/api` route needs that cookie (401 without it).
- The Host header must be `127.0.0.1:<port>` or `localhost:<port>` on
  every path (403 otherwise), so a page on another name that resolves to
  this machine is refused.
- Every request that changes something (any method but GET and HEAD)
  needs an `Origin` equal to `http://<Host>` (403 otherwise). No CORS
  header is ever sent.
- `state.db` is opened read-only, except for the two ledger tables the
  agent writes for runs the page starts.
- Credentials leave the server only as their last 4 characters, and the
  server never prints one.

## Views

Every view has its own URL, so it can be linked, reloaded or printed by a
terminal command.

### Sessions (`/sessions`)

Every session in `state.db`: title, project and prover (Lean, Rocq,
Isabelle), model, start, turns, dollars and result, with filters for
project, prover, model and context strategy. The project is not stored in
`state.db`; it is taken from the first absolute path the session's tools
touched that lies in a Fisher project or a prover project root (lakefile,
`_CoqProject`, `ROOT`). A session with no such path has no project.

The form at the top starts a run: a project directory and a Fisher
command (such as `/prove theories/Lists.v`). The server runs
`fisher launch --print <command>` in that directory with the event ledger
on, waits for the session it becomes, and opens its live view.

### Session (`/session/<id>`)

The transcript (messages, tool calls with their results collapsed,
prover diagnostics in place) and a cost and token strip. When the session
has a step log, a timeline with one tick per file change and the proof
canvas beside it: the file as each step left it, a check verdict for any
step (errors and holes left), a diff of two steps, and an editor to change
the file by hand and hand it back to the agent.

A step's check runs in a copy of the project under
`$FISHER_HOME/fisher/ui-check`, never in the project itself: Lean through
the search's language-server probe, Rocq and Isabelle through
`fisher check`. A verdict is computed once and kept beside the step log.

A hand back writes the file (temp file, then rename), records a `hand`
step, checks it, and tells the agent: through the inbox when a run of
this server drives the session or the session runs in another process
(below), otherwise by resuming the session (`fisher chat -c <latest
session> -p <message>`, its model kept) as a new run of this server.

Every `fisher chat` holds an exclusive lock on
`$FISHER_HOME/fisher/steps/<root>.lock` for as long as it runs (and writes
its pid to `<root>.pid`), whoever started it: a terminal, `fisher launch`,
another `fisher ui`. A session whose lock is held by a process that is not
a run of this server runs elsewhere: the server never resumes it beside
itself, a hand back or a message goes through its inbox (every chat reads
its inbox, with or without the ledger), and stop answers 409 "running in
another process" (stop it where it runs). The live view says so, with the
pid.

### Live session (`/live/<id>`)

A run the page started, as it happens: each tool start and end, message
and inbox event from the ledger, streamed with server-sent events. A
reload resumes after the last event the page saw, with no gap and no
duplicate. Stop sends SIGTERM to the run's whole process tree and SIGKILL
after 10 seconds. Send puts a message in the session's inbox; the agent
takes it at its next tool boundary, and the page shows it as queued until
the ledger says it arrived. When the run ends, crashes or is stopped, the
page shows the end state.

### Context (`/context/<id>`)

What the session's context strategy kept. CLM: the context file now and
before its last edit, its state file and the `clm_*` events. RLM: its
state and `rlm_*` events. The compressor: the summary that opens each
session of the compression chain.

### Goal board (`/search`, `/search/<run>`)

The runs of `fisher search` under `~/.cache/fisher/search`, and the
bench g, h and w runs. A flat run shows each goal's status, its attempts per
backend and what closed it (the pre-pass, a model, a model with repair);
a graph run shows the lemma graph from `graph.json` (roots, splits and
their status, shared lemmas) and each root's whiteboard. Pre-pass, repair,
disproof and timeout events sit on the goal or node they belong to, and a
running board follows the events file as it grows. So does retrieval: a
goal's `facts` and `library offered` events (and, once the run has its
report, its `facts`, `offered` and `used` fields) are marks on it, and the
run's `index built`, `library saved` and `library problem` events, with
the report's `facts_mean`, `offered_total` and `used_total`, are shown
above the goals.

From the board: start a run on a Lean file (flat or graph, pre-pass on or
off, retrieval and the lemma library on or off: `--no-retrieval`,
`--no-library`), stop it, run it again (with the same choices), and apply its merged file to the project.
Apply follows the rule of `fisher search --apply` (a graph run only when
no lemma from a split is left with its sorry), writes with a temp file
and a rename, and asks before writing over a file changed since the run
started.

### Runs (`/runs`, `/runs/<bench>`)

The benchmark results under `~/.cache/fisher/bench` (c3, clm4, d, g, h,
w, rlm): for each goal and arm, solved, turns, dollars and what closed it,
with two arms side by side and sortable columns. Bench w runs whole files
(`runs/<arm>/<n>/`): a table of its runs (closed of all goals, dollars as
bench h counts them, facts per goal, lemmas offered and used) comes before
the goals, and each goal shows its facts and the lemmas offered and used.
For g, h and w, the gate line is the last line of that bench's
`report.py`, run in the background (it checks proofs and can take many
minutes; the page shows it as pending until then) and cached until its
inputs change. It is not run while the bench itself is running (a run
directory with `events.jsonl` and no `report.json`: the page says "bench
in progress"), and a gate whose inputs changed while it ran is run again
only while the page still asks for it (a request in the last 5 minutes).

### Settings (`/settings`)

The settings a run started from the page uses, read and written as
`fisher config show` and `fisher config set` do:

| Field | Where |
| --- | --- |
| Provider | `model.provider` in `config.yaml` (default `auto`) |
| Model | `model.default` (or `model` when it is a plain string) |
| Context strategy | `fisher.context`: `compress`, `clm` or `rlm` |
| Autoformalize model | `fisher.autoformalize.model` (empty: each backend's own default) |
| Keys | the credentials in the home's `.env` (`~/.fisher/.env`, else `~/.gauss/.env`) |
| Other variables | the `.env` variables with no secret (`TERMINAL_SSH_HOST`, `TERMINAL_SSH_USER`, `FIRECRAWL_API_URL`, ...), with their values |
| Search pre-pass | shown only (see below) |

Only the fields that changed are written, into the user's own
`config.yaml` with its key order kept (a temp file, then a rename, the
file's mode kept, and through a symbolic link to the file it names). A key
is shown only as its last 4 characters (none for a value under 8
characters). A new value replaces that key's line in `.env` in place, or
is appended; every other line stays as it was and in its order, and the
file is written with a temp file and a rename, mode 600. Runs started
after a save read the new values.

## What is live

- A run the page starts: its ledger events stream to the live view as
  the agent writes them (polled every 250 ms).
- A `fisher search` run: its events file streams to the board as it
  grows, for runs started from the page or anywhere else.
- The bench g, h and w gate lines: polled until `report.py` finishes.
- Everything else (the sessions list, a finished session, context,
  benchmark tables, settings) is read when the page asks for it; reload to
  see newer data.

## What grows

Nothing here is deleted on its own:

- `state.db`'s `ui_events` (every event of every run with the ledger on,
  a few hundred rows per run) and `ui_runs` (one row per run).
- `$FISHER_HOME/fisher/ui-runs/`: the output of every run the page starts.
- `$FISHER_HOME/fisher/steps/`: each session's step log, which holds the
  whole file after every change (a long session's log can reach many
  megabytes), with its verdicts, lock and pid files.
- `$FISHER_HOME/fisher/inbox/`: the messages sent to each session.

`fisher ui --prune DAYS` (DAYS at least 1) deletes, for every session
whose last change (its last ledger event, its step and inbox files'
times) is older than DAYS days, its ledger rows, step files and inbox.
It takes each session's run lock first and deletes while it holds it (a
chat that starts meanwhile waits for it, up to 2 seconds, before it
runs), and a session whose lock another process holds is running and is
skipped; the lock file itself is never deleted. It also deletes the run
logs older than DAYS days, except one whose run's session is running,
and prints what it deleted. The sessions themselves (`state.db`'s sessions and messages,
the replay logs) are Fisher's and stay. It can run while `fisher ui`
runs.

## Limits

- Local only: no access from other devices, and one user.
- Stop works only for runs this server started; a session running in
  another process gets messages and hand backs through its inbox, and is
  stopped where it runs. Queued messages are listed only in the page that
  sent them.
- The step log is written for the local terminal backend; sessions on the
  docker or ssh backends have no step log (their paths are inside the
  sandbox).
- An inbox message is consumed at the next tool boundary even when no
  model call follows; a hand-back message that is not read before the run
  ends reaches the next run after its `-p` prompt.
- Stopping a run sends SIGKILL to the process ids it listed when the grace
  ends; a process id reused in those 10 seconds could be hit.
- With no agent live, `state.db` is read as an immutable snapshot; an agent
  that opens it between the check and the read is not seen until the next
  read. Two requests for a growing replay log can both read it.
- Retry on the goal board searches the whole file again: `fisher search`
  has no single-goal flag.
- A bench report.py run by hand while the server runs its own checks in
  the same workspaces, and the two can fail each other's checks.
- Settings: the search pre-pass has no config keys, so the page shows the
  defaults of `fisher search` (tactics, 60 seconds per hole, repair and
  disprove on) and they are changed per run with `--auto-tactics`,
  `--auto-seconds`, `--no-repair`, `--no-disprove` and `--no-auto`. A key
  cannot be removed from the page (edit `.env` for that). Writing `.env`
  by rename replaces a symbolic link at that path with a file (mode 600).
- The page tests run against a stub server where `fisher-ui` cannot be
  built; its recorded answers drift from the real routes until
  `ui/tests/capture.py` is run again.
