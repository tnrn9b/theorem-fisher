# Theorem Fisher

Theorem Fisher (Fisher for short) is a Haskell rewrite of a Lean workflow
orchestrator: the upstream repository named in `UPSTREAM`, pinned there at
one commit. The plan is the Haskell rewrite plan doc.

New here? Start with `docs/quickstart.md` (the terminal) or
`docs/quickstart-ui.md` (the web app).

The executable is `fisher`. It keeps upstream's file formats (`config.yaml`,
`.env`, `auth.json`, `state.db` in the home; `project.yaml` in a project)
under Fisher's own names.

### Names

| | Fisher |
| --- | --- |
| home directory | `$FISHER_HOME`, else `~/.fisher` |
| project directory | `.fisher/project.yaml` |
| environment variables | `FISHER_X` |
| config root | `fisher:` (keys `fisher.<path>`) |
| native backend | `fisher` |
| default toolset | `fisher-cli` (and `fisher-X` for the platform toolsets) |

Fisher reads and writes only these names. There is no fallback to the names
the product had before the rename: a home, project directory, environment
variable, config section, backend or toolset under a previous name is not
read. A machine whose only home is a previous one behaves as a fresh
install (no config, no key) and says how to set up; a project whose only
project directory is a previous one is not a Fisher project until
`fisher project init` makes `.fisher`; a previous backend name on the
command line is refused with the list of valid backends. When Fisher sets
a variable for a child process (a managed workflow, a session, a tool), it
sets only `FISHER_*`. A save of `config.yaml` writes only `fisher:`; any other
root section is kept as any unknown key is.

To keep a home from before the rename, move your previous home directory to
`~/.fisher`, rename its config root to `fisher:` (the first key of
`config.yaml`), and in its `.env` rename each variable's previous prefix to
`FISHER_` (keys such as `OPENROUTER_API_KEY` stay as they are). In each
project, rename the previous project directory (the hidden directory that
holds `project.yaml`) to `.fisher`, or run `fisher project init` there;
until then `fisher` says it found no active project and points at the
directory to rename. In the moved home, `autoformalize/` may still hold the
managed state of the native backend under the previous name (a directory
named after it, whose `managed/` holds a home directory under the previous
name). Fisher keeps its own under `autoformalize/fisher/` and never reads
the old one, so it can be deleted.

Run directories are data: bench reports and events, and the UI, show a run
under the backend name it recorded, also when that is the previous one.

## Layout

| package | holds |
| --- | --- |
| `fisher-core` | config defaults, load, merge, save, `set` and migration; `.env`; the project model; the `/project` command; shlex; terminal styling; `state.db` (`Fisher.Store`); the part of rich the shell draws with (`Fisher.Rich.*`); difflib; Python's `json.dumps` and float formatting; YAML written in a given key order |
| `fisher-workflow` | backend and auth-mode resolution; local Claude Code and Codex auth detection; workflow command parsing; staging of the managed Lean assets, credentials, MCP and Codex config and startup context; the launch plan (argv, cwd, environment) for each workflow and for `/chat` |
| `fisher-swarm` | sessions behind a PTY or on pipes, held in an STM task table; stream-json progress; attach, detach on Ctrl-] and the folder-trust answer; process-group cancel; the swarm table and detail views |
| `fisher-llm` | the LLM clients: OpenAI chat completions, Anthropic messages and Codex responses (request building, message preparation and cache breakpoints, SSE, normalization to the stored assistant message, the SDKs' retry rules and error text); providers and credentials (`auth.json`, Codex OAuth, Claude Code tokens, runtime provider resolution); `Sys`, the effects record the credential code runs on, and `Replay`, which replays upstream's recorded runs through it |
| `fisher-tools` | the agent's tools: the registry and toolsets, the four file tools (read_file, write_file, patch, search_files) on upstream's shell file layer, fuzzy matching, V4A patches, secret redaction, the read-loop guard; the sandboxes (local oneshot and persistent, docker, ssh), their `TERMINAL_*` config and a per-task cache |
| `fisher-lean` | the Lean language-server client (leanclient's aio client): `lake serve` over a process group, documents and versions, `waitForDiagnostics` barriers, stale-import rebuilds, the file watcher, scratch slots, Lean RPC and widgets; all 22 of lean-lsp-mcp's tools on it (the 17 LSP-backed ones, local search and the four remote search services), and its MCP server (`fisher lean-mcp`), which managed workflows launch |
| `fisher-mcp` | the MCP client: `mcp_servers` from config, stdio and streamable HTTP transports, JSON-RPC sessions, tools and the four resource/prompt helpers in the registry, output-schema checks, sampling, status, shutdown |
| `fisher-agent` | the native agent loop (`AIAgent.run_conversation`): the system prompt with context files, memory and skills guidance; the tool-calling loop with retries, fallback, interrupts and the max-iteration summary; context compression; model metadata and context-length probing; the auxiliary client for side tasks; tool previews, spinners and completion lines; sessions in `state.db`; and the replay log (`Fisher.Agent.Replay`) |
| `fisher-cli` | the `fisher` executable: the interactive shell (banner, skins, slash commands, plain text to the native agent, the swarm), and `project`, `doctor`, `config`, `sessions`, `version`, `setup`, `model`, `login`, `logout`, `status`, `llm-check`, `tools-check`, `plan`, `launch`, `chat`, `replay`, `prewarm`, `lean-mcp`, `acp` and `cron` |
| `fisher-ui` | `fisher ui`: the local web app's server (Warp on 127.0.0.1, the token cookie, Host and Origin rules, the JSON and event-stream routes over sessions, step logs, context, search runs, benchmarks and settings, the children it starts) with the React page from `ui/` embedded; see `docs/ui.md` |

`fisher-core/data/` and `fisher-workflow/data/search_arxiv.py` are generated
from the pinned upstream by `tools/gen-defaults.py /path/to/upstream-checkout`,
which applies the rename on the way (`tools/gen_defaults_rename.py`: the text,
the `fisher:` config root, `fisher-cli`, `FISHER_*`, the FISHER banner logo)
and leaves out the removed Nous Portal provider; do not edit them by hand.

## Building

Stack, on LTS 22.44 (GHC 9.6.7), the same snapshot as
formal-encoding-prototype.

```
stack build
stack test
stack exec fisher                                    # the interactive shell ([--compact])
stack exec fisher -- sessions list | export OUT | delete ID | prune | rename ID TITLE | stats
stack exec fisher -- config show | edit
stack exec fisher -- version
stack exec fisher -- setup [model|terminal|tools|agent] | model | login | logout [--provider P] | status [--all]
stack exec fisher -- llm-check [--provider P] [--model M] [--prompt TEXT]   # one small request to the configured model
stack exec fisher -- tools-check [list] [--toolsets A,B] [--disable A,B]    # the tool definitions the model would see
stack exec fisher -- tools-check call NAME '{"path": "x"}' [--task ID]     # call one tool (file tools, MCP tools)
stack exec fisher -- tools-check status                                     # MCP server status
stack exec fisher -- project status | init [PATH] [--name N] | convert [PATH] | use [PATH] | clear
stack exec fisher -- project create PATH [--template-source S] [--name N]
stack exec fisher -- doctor [--fix] [--all]           # --all: every prover, not only the project's
stack exec fisher -- check [--prover rocq|isabelle] [FILE[:decl]...]   # are Rocq or Isabelle targets proved
stack exec fisher -- config path | env-path | check | migrate | set KEY VALUE
stack exec fisher -- plan /prove Math/Foo.lean     # stage and print the launch plan as JSON
stack exec fisher -- launch /prove Math/Foo.lean   # stage and run it here, attached
stack exec fisher -- chat -q TEXT [-Q] [-m MODEL] [-t TOOLSETS] [--provider P] [-v]
                     [--resume ID | --continue [NAME]] [--pass-session-id]   # one turn of Fisher's own agent
stack exec fisher -- chat [TEXT...]                 # managed backend chat session
stack exec fisher -- replay SESSION_ID              # run a logged session again, compare every request
stack exec fisher -- ui [--port N] [--no-open]     # the web app on 127.0.0.1 (docs/ui.md)
stack exec fisher -- prewarm                        # fetch lean4-skills, warm the Lean MCP runtime
```

`plan` and `launch` take any workflow command (`/prove`, `/draft`,
`/review`, `/autoformalize`, ...) with or without the leading slash.
`plan` shows credential-like environment values as `<redacted>`.

`tools/test.sh [package ...]` builds the test suites and runs each under a
time limit (`FISHER_TEST_LIMIT`, default 120 s), so a hung test cannot block
the watcher; output is in `.build/test-<package>.txt`. `tools/e2e-a2.sh`
runs `fisher plan` and `fisher launch` end to end against fake `git`,
`uvx`, `rg` and `claude` in a throwaway `HOME`, with `launch` under
`script(1)` for a real terminal. `tools/e2e-a3.sh` does the same for the
shell: banner, `/help`, `/skin`, `/project`, a workflow into the swarm,
detach, `/swarm` and `/quit`, then the one-shot subcommands.
`tools/e2e-b2.sh` runs `fisher tools-check` on a throwaway project and
`FISHER_HOME`: the file tools on the local sandbox (oneshot and persistent),
an MCP server over stdio and one over HTTP from `config.yaml`, a failing
server, docker when a daemon answers, and a real npm MCP server
(`@modelcontextprotocol/server-filesystem`) when `npx` is installed.
`tools/e2e-b3.sh` runs real sessions against the provider in `~/.fisher`
(copied into a throwaway `FISHER_HOME`; a few short requests): `chat -Q -q`
and `chat -q` with no tools, `read_file`, `search_files` and `patch`, a
resumed session, plain text in the shell, then `fisher replay` on each, and
one log with a changed answer that replay must refuse.
`tools/e2e-b4.sh` drives the browser tools on a real local agent-browser
against a page served on localhost (vision makes one small request to your
auxiliary model), runs an agent session that browses and replays it, runs
the terminal and process tools, and answers a dangerous-command prompt
from stdin; web search is checked as unavailable without a Firecrawl key.
`tools/negative-a3.sh`, `tools/negative-b1.sh`, `tools/negative-b2.sh`,
`tools/negative-b3.sh` and `tools/negative-b4.sh` spoil one expected value
in every A3, B1, B2, B3 or B4 recording, check each spoiled case fails, and
put the recordings back.

`tools/e2e-c2.sh` runs the C2 search sessions twice, live and now, through
lean-lsp-mcp and through `fisher lean-mcp`, and compares the two runs (the
services' answers drift, so upstream today is the reference);
`tools/negative-c2.sh` spoils every recorded C2 result; `tools/gate-c2.sh`
stages a managed `/prove` for claude-code with `fisher plan`, runs that
child headless and checks it called the Lean tools on Fisher's server.

`tools/bench-c3.sh` is the C3 benchmark, run on demand: the Lean
workflows on 60 pinned goals per backend (claude-code, fisher), scored
outside the agent, with a report (`tools/bench/c3/`: `prepare.py` pins and
builds the goals, `run.py` runs one backend, `score.py` judges a proof,
`rescore.py` scores a finished run again, `report.py` writes the report).

`tools/bench/d/` is the D benchmark: `prepare.py` erases every eligible
proof of one TWISTED file in a copy of the C3 clone and pins them,
`judge.py` judges a `fisher search` run's merged file on its own (compile,
kept lines, statements, pp.all types, axioms).

`tools/e2e-c1.sh` runs `fisher lean-mcp` through every C1 session on real
`lake serve` for both toolchains and compares each call with the
lean-lsp-mcp goldens; `tools/negative-c1.sh` spoils every recorded C1
result and checks the replay reports each offline step as different.

Every session of Fisher's agent (`fisher chat -q`, plain text in the shell)
writes a replay log, one JSONL file per session in
`$FISHER_HOME/fisher/replays/SESSION_ID.jsonl`: the agent's configuration,
the environment it read, every HTTP exchange, tool call and result, files
the read tracker saw, the built system prompt, uuids, and each turn's input
and outcome. Keys are written masked (first 8 and last 4 characters). A
resumed session appends to its log. `fisher replay SESSION_ID` runs the
session again from the log, with the transport and tools answering from it,
and reports whether every request came out byte for byte as logged.

## The web app (`fisher ui`)

```
stack exec fisher -- ui [--port N] [--no-open]
```

serves a local web app on 127.0.0.1 (port 8732 by default) and opens it in
the browser. It prints `http://127.0.0.1:<port>/?t=<token>` with a new
random token on each start; that URL trades the token for a cookie, and
every API route needs it. Host must be `127.0.0.1:<port>` or
`localhost:<port>`, and a request that changes something must come from
the page's own origin. Start it from the Fisher checkout so that the
benchmark gate scripts are found.

Its views: Sessions (every session in `state.db`, with filters, and a
form that starts a run), Session (transcript, cost, the step timeline and
the proof canvas: check any step, diff two, edit and hand back), Live
session (the run's events as they happen, stop, send a message), Context
(what CLM, RLM or the compressor kept), the goal board (`fisher search`
runs, flat and graph: start, stop, run again, apply), Runs (the
benchmarks with their gate lines) and Settings (provider, model,
autoformalize model and context strategy in `config.yaml`, keys in
`.env` shown only as their last 4 characters, and the search pre-pass
defaults, shown only). `docs/ui.md` describes each view, what is live and
the limits.

The page is built from `ui/` (`npm run build` there) into `ui/dist`, which
is committed and embedded at build time. The page tests are Playwright
(`npx playwright test` in `ui/`): against a stub server by default, or
against `fisher-ui-fixture-server serve PORT` with
`FISHER_UI_BASE=http://127.0.0.1:PORT`; `FISHER_UI_BROWSER` names a Chrome
or Chromium binary.

## Context as a file (CLM)

Fisher's own alternative to upstream's context compressor, after the
Context Language Models paper (arXiv 2609.37725): the model keeps its
conversation in a file, the *mirror*, and edits it with the file tools;
the loop reads the file back, gates the edit and keeps the context under
a fixed token budget with nudges and rollback. The spec is `docs/clm.md`
(Fisher's own design and wording; nothing is taken from the paper's
CC BY-NC repository). Upstream's compressor stays the default, and
nothing below runs unless CLM is chosen.

Status: CLM3 is done; CLM4 (the evaluation against the compressor) is
next. Choose it with `fisher.context: clm` in `config.yaml` (and the
`fisher.clm` budget, reserve, gate and max_rollbacks), or per session:

```
fisher chat -q "..." --context clm     # one turn under CLM
fisher --context clm                   # the shell's native sessions
```

The context file and its backup are `$FISHER_HOME/fisher/ctx/<session>/
context.txt` and `context.prev.txt`, with `state.json` beside them;
`--resume` picks the live context up from there, while `state.db` keeps
the full transcript. The prompt section is added to the system prompt,
and every write and read-back of the file is in the replay log, so
`fisher replay` checks a CLM session bit for bit. CLM needs the local
terminal backend for now (docker and ssh are refused), and ACP and cron
use the compressor.

```
python3 tools/clm/cases.py      # rebuild the goldens from the spec's reference reading
python3 tools/clm/fuzz.py 5000  # the properties on the reference
zsh tools/negative-clm.sh       # spoil every CLM case; all 79 must fail
zsh tools/e2e-clm.sh            # live: a session that compacts, its replay, a tampered log refused
```

The goldens in `fisher-agent/test/golden/clm/` hold 53 hand-written
read-back cases, 8 gate cases and 18 scripted loop scenarios.
`tools/clm/reference.py` is an executable reading of the spec, written
from the spec alone, and the Haskell implementation is checked against
it: every case, the five QuickCheck properties (5,000 runs each), and the
negative control (79 of 79 fail when spoiled). Every scenario also
replays bit for bit from its replay log, and a log with a changed
read-back or write is refused.

## Rocq and Isabelle

`/prove` and `/draft` also run in Rocq and Isabelle projects, on every
backend, through Rocq-MCP and Isabelle-MCP; the other workflows stay Lean
only. `fisher check` compiles the targets and audits their axioms (Rocq) or
oracles (Isabelle), with no model involved, and `fisher doctor` checks the
prover of the project in reach (`--all` for every prover). Setup, commands,
the tools offered per backend and the limits are in `docs/provers.md`; the
design is `docs/superpowers/specs/2026-10-14-provers-design.md`.

## Release

Releases are macOS arm64 only for now. `tools/release.sh` builds the
binary, runs the whole regression and packs the release; each stage logs
to `.build/release/<stage>.txt` and the script stops at the first stage
that does not pass:

```
tools/release.sh                       # build test negative e2e package
tools/release.sh test                  # or any of the stages on their own
```

- `build`: `stack build --copy-bins` into `.build/release/bin`, and
  `fisher version`.
- `test`: every package's unit and golden suite (`tools/test.sh`).
- `negative`: every `tools/negative-*.sh`. Each spoils its goldens, must
  see them fail, and puts them back (it says NOT RESTORED if it could not).
- `e2e`: the end-to-end checks that need no paid model or public service:
  `e2e-a2`, `e2e-a3`, `e2e-b2` and `e2e-c1`. The live ones (`e2e-b3`,
  `e2e-b4`, `e2e-c2`, `gate-c2`, `e2e-e1`, `e2e-e2`) are run by
  hand.
- `package`: `dist/fisher-VERSION-macos-arm64.tar.gz` (the binary, README,
  LICENSE, CHANGELOG and UPSTREAM) with a `.sha256`. The tarball is
  unpacked and its binary run before the script says it is done.

The binary links only system libraries (libSystem, libncurses, libz,
libiconv, libffi, CoreFoundation, CoreServices). Unpack it anywhere and put
`bin` on `PATH`.

Fisher 0.2.0 (the E3 release) passed: 12,227 examples in the ten suites
with 0 failures; all nine negative controls failing as expected (E1's 29 of
29 sessions, E2's 1,306 of 1,309 spoiled cases, the three left being
croniter cases with nothing to spoil); and the four offline end-to-end
checks.

## Reference outputs from upstream

`tools/corpus/a1.py` runs the pinned upstream over fixed inputs and records
what it does: config load, save and `set`, `.env` parsing and updates,
manifest loading (37 manifests), `/project` transcripts (16), the doctor's
managed-workflow section (26 setups) and `config migrate` (13). The JSON
lives in `fisher-*/test/golden/a1/`, with scratch paths written as `<TMP>`,
and the golden specs require Fisher to reproduce it exactly. To re-record,
with a Python 3.11+ that has upstream installed (`pip install -e`):

```
UPSTREAM_CLI=/path/to/venv/bin/<upstream program> \
  python tools/corpus/a1.py /path/to/upstream-checkout OUTDIR
```

The checkout must be at the commit in `UPSTREAM`; the script refuses
otherwise. Recording twice gives byte-identical files. Every recorder under
`tools/corpus/` takes the upstream program from `UPSTREAM_CLI`, which is
required (there is no default name; in-process recorders use its basename
as the program name), and builds upstream's own names (its package, modules,
classes, environment variables and directories) at run time from the
repository name in `UPSTREAM` (`tools/corpus/upstream_names.py`). What they
write goes through the same rename as `tools/gen-defaults.py`, so the
recordings say Fisher's names. Upstream still has the Nous Portal provider,
which Fisher removed: the recorders that meet it (B1 requests and
credentials, B3, B4, E1, E2) pass what they write through
`tools/corpus/post.py`, which drops its cases, menu entries, status line and
login options, renumbers the menus and the typed answers, rewrites
upstream's OpenRouter referer to Fisher's `https://fisher.invalid`, and
replaces the gateway service probe in `status` (Fisher has no gateway) with
the line Fisher prints, so a
re-record reproduces the committed goldens on these points. A recording
that still mentions Nous after that stops with an error. Checked against
the pinned upstream: B1 requests, responses, SDK, prepare, resolve, login
and logout, E2 schedules, croniter and tool, and B3 aux, metadata and
prompt re-record byte for byte. What still differs is not about Nous or the
referer: the program name is one letter longer than upstream's, so
argparse continuation indents and the padding of Rich titles in `status`,
`setup` and `cron` help are laid out the way Fisher prints them, a few B1
`setup` and `model` lines show the home as `<FISHER_HOME>` where the
recording says `~/.fisher`, and timings, token estimates and spinner frames
vary from run to run.

`tools/corpus/a2.py` does the same for A2: the staged files and launch
request of every workflow and `/chat` setup (fake `git`, `uvx`, `rg`,
`claude` and `codex` on a fixed `PATH`), prewarm, the forgiving command
rewrite, the stream-json progress parser, the Claude launch normalisation
and the elapsed-time format. It lives in `fisher-*/test/golden/a2/`, with
`<TMP>`, `<STAMP>`, `<ISO>` and `<ARXIV>` placeholders, and records the
machine facts (root or not, why strict mode is unavailable) that the specs
replay.

`tools/corpus/a3.py` records A3: shell transcripts run with Fisher's command
table (`repl.json`), the friendly and forgiving rewrites with difflib
(`dispatch.json`), skins, the welcome banner at each width, 640 random rich
renders, and `sessions`, `config show` and `config edit` over a fixture
`state.db` on a fixed clock (`subcommands.json`). Scratch roots have fixed
lengths (100 characters, 90 for upstream's home) so widths reproduce; paths
are masked as `<TMP>`, or `<TMP:k>` where an ellipsis cut one short.

The A3 gate, which checked that upstream and Fisher could share one home and
project, was retired with the clean break: Fisher no longer reads upstream's
names, so there is no shared state to check.

`tools/corpus/b1.py` records B1's LLM layer through upstream's own call
paths with the HTTP transport mocked: the exact request of every wire format
over a matrix of providers, histories, tools, reasoning settings and caching
(`requests.json`), canned responses and Codex event streams through the
call and normalization (`responses.json`), the per-call message preparation
(`prepare.json`) and the OpenAI SDK's retry rules and error text
(`sdk.json`). `tools/corpus/b1_auth.py` (with `b1_launch.py`) records the
credential surface: `resolve_runtime_provider` over 92 states, `status`,
`logout`, `login`, and `setup` and `model` driven through a pty (70 and 45
transcripts), with HTTP, clock, subprocesses, the browser and typed input
all scripted. Fisher's credential, setup and model code runs on
`Fisher.LLM.Sys`; the specs replay each recording through a scripted `Sys`
(`Fisher.LLM.Replay`) and compare the transcript, exit code, HTTP log,
sleeps, subprocesses and the final files.

The B1 gate (recorded streams parse) uses real traffic:
`tools/capture-b1.py`, run once with your own credentials, sends the small
requests in `fisher-llm/test/golden/b1/live-requests.json` (built by
upstream) and saves the responses; `b1.py live-normalize CAPTURES` runs
upstream's normalization over them into `live.json`, which
`Fisher.LLM.LiveSpec` replays through Fisher's full call path. The
capture of 2026-10-01 has seven responses: four from OpenRouter (a one-word
reply, a tool call, the answer after a tool result, a reasoning model's
reply with encrypted reasoning details) and three from Anthropic over a
Claude Code login (a one-word reply, a tool call, and a 429 rate-limit
error on the thinking request). All seven are handled exactly as upstream's
code handles them. Codex is not captured yet. The script reads Claude
Code's per-directory keychain items (`Claude Code-credentials-<hash>`) and
relaxes Python 3.13's strict X.509 mode, which rejects the CA certificates
of TLS-inspecting proxies (FortiGate's); Fisher's own TLS stack accepts them.

`tools/corpus/b2.py` records B2's tool layer through upstream's own code:
CPython's difflib and `re` for every pattern the port uses (`difflib.json`,
`re.json`, in fisher-core), `agent.redact` and the MCP error sanitizer,
fuzzy matching (324 cases) and the V4A parser, 56 sequences of file tool
calls on a fixture tree with every shell command upstream sent and its
answer (`fileops.json`, which Fisher must reproduce command for command),
the same calls with paths as `<ROOT>` for a run on a real sandbox
(`live.json`), schemas, toolsets and `get_tool_definitions` with its
printed lines (`registry.json`), and the sandboxes' pure parts: the sudo
rewrite, the subprocess environment and its 83-name blocklist, fences,
`TERMINAL_*` parsing, and the docker and ssh command lines
(`sandbox.json`). `tools/corpus/b2_mcp.py`, run with mcp 1.26.0 (the
version upstream pins), drives upstream's MCP client against the scripted
server in `fisher-mcp/test/mcp/fake_server.py` over 13 scenarios (stdio,
HTTP, filters, timeouts, a server that dies, connect failures, output
schemas, sampling and rate limits, the server's own requests) and records
what was registered, every result, the log lines, what each server
received and what was asked of the LLM (`mcp.json`); Fisher's spec runs the
same server with python3.

The B2 gate (tool golden tests pass) held on 2 Oct 2026 on the Mac: every
recording above reproduced (fisher-core 3,995 examples, fisher-tools 733 with
two machine-dependent live cases skipped, fisher-mcp 13 including HTTP), the
negative control failed every spoiled case but one whose spoil changed
nothing, and `tools/e2e-b2.sh` passed, including a real npm MCP server.
Docker and ssh are checked by their recorded command lines only; no docker
daemon or ssh host was available.

`tools/corpus/b3.py` records B3 through upstream's own agent with the HTTP
transport mocked, a clock that moves only on sleeps, counter uuids and a
fixed date: CPython's `json` error messages and `repr` (`pyjson.json`,
`pyrepr.json`, in fisher-core); tool previews, emojis, failure detection
and completion lines for every tool name (`display.json`); model metadata,
context-length lookup, the disk cache and probe tiers (`metadata.json`);
the context compressor (`compress.json`); system prompts over fixture trees
of context files, SOUL.md and memory (`prompt.json`); auxiliary provider
resolution and calls (`aux.json`); and 51 scripted `run_conversation`
scenarios (`loop.json`: every request, sleep, tool call, callback event,
printed line, result, raise and `state.db` row; tool calls, parallel tools,
retries and fallback, invalid responses, length and context-limit errors,
compression, interrupts, reasoning, prefill, the iteration limit). Browser
and web tools are switched off; file tools run for real on a fixture tree.
`tools/corpus/b3_cli.py` records ten upstream `chat -q` and `-Q` runs through
its CLI main with the same mocks (`fisher-cli/test/golden/b3/cli.json`):
stdout at 60 columns with the banner, the exit code, requests and session
rows.

The B3 gate (replays are bit-exact) held on 2 Oct 2026 on the Mac: every
recording above reproduced, every loop scenario also replays from the log
it wrote (49 without interrupts, and one log written by two processes), and
`tools/e2e-b3.sh` ran six real OpenRouter sessions (claude-opus-4.6: no
tools, read_file, search_files, patch, a resume, the shell) whose replays
were all bit-exact, while the spoiled log was refused. The negative control
failed every spoiled case; Display's emoji cases for the 44 tools Fisher does
not have yet are pending rather than checked.

`tools/corpus/b4.py` records B4 through upstream's own tool modules
(`fisher-agent/test/golden/b4/`): web search and extraction with
Firecrawl's client mocked at `requests.post` and the summariser at the
auxiliary call (63 cases, firecrawl-py 4.46.0, `web.json`); the browser
tools with agent-browser mocked at `subprocess.run` and Browserbase at
`requests.post` (61, `browser.json`); the terminal tool over a scripted
sandbox, with the approval prompt and its answers, tirith, smart
approval, sudo, retries and the saved allowlist (52, plus 59 commands
through the dangerous-command patterns, `terminal.json`); background
processes on real local bash and a scripted remote sandbox (19,
`process.json`); the todo tool and history hydration (12 and 9,
`todo.json`); the memory tool over real files, its limits, scan and frozen
snapshot (20 and 2, `memory.json`); and 16 `run_conversation` scenarios
over B3's harness (`loop.json`) with todo and memory offered through a
custom toolset: inline calls, quiet and verbose display, concurrent
calls, the nudge, hydration and its notice, the todo note after
compression, the memory flush before compression, and a hydration that
raises.

The B4 gate (tool golden tests pass) held on 2 Oct 2026 on the Mac: every
recording above reproduced (fisher-agent 3,678 examples, 31 pending;
fisher-cli 1,332; no warnings), the loop scenarios also replay from their
logs, the negative control failed all 313 spoiled cases (and the 16 loop
replays checked against them), and `tools/e2e-b4.sh` passed: the live
browser, vision, an agent session that browses and replays, the terminal
and process tools, and the approval prompt. Firecrawl is checked against
mocks only (no key was available).

`tools/corpus/c1.py` records C1 against lean-lsp-mcp 0.31.0 (leanclient
0.13.2, mcp 2.0.0; upstream pins none of them) over real `lake serve`, on
Lean 4.33.0 and 4.33.1 (`fisher-lean/test/golden/c1/`): the MCP
`initialize` and `tools/list`, then 121 steps in 8 sessions over two
fixture projects (`fisher-lean/test/fixtures/core`, a small Lean-only
project with Unicode names, deliberate errors and widgets, and
`fixtures/mathlib`, built by `tools/lean-fixtures.sh`): every one of the
17 tools, error paths included, file edits between calls, a build and
clean with the server restart around it, and the source scan without
ripgrep. Every LSP message between leanclient and the server is recorded
too. Offline, `fisher-lean`'s replay spec answers Fisher's client from
those transcripts (Fisher must send what leanclient sent, in an order the
transcript allows) and checks every tool result, for the 7 core sessions;
`lean_profile_proof`'s timings and the Mathlib session are checked live by
`tools/e2e-c1.sh`, which masks only timings and Lean's run-to-run order of
published diagnostics. `fisher-lean/data/lean-lsp-mcp-0.31.0.json` is
lean-lsp-mcp's own `initialize` and `tools/list`, cut to the C1 tools
(`tools/corpus/c1-mcp-dump.py`, then `tools/gen-lean-mcp-data.py`).

The C1 gate (Fisher's client replaces lean-lsp-mcp for the LSP tools) held
on 2 Oct 2026 on the Mac: the replay passed on both toolchains (fisher-lean
20 examples, the 2 profile steps pending as live-only), the negative
control failed all 190 spoiled offline steps, and `tools/e2e-c1.sh` matched
all 119 calls on each toolchain (one 4.33.1 run saw a multi_attempt's two
diagnostics in the other order, which Lean decides). The other suites were
unchanged and there were no warnings. A managed workflow uses Fisher's
server instead of `uvx lean-lsp-mcp` when `FISHER_LEAN_MCP=fisher` (since
C2 that is the default).

`tools/corpus/c2.py` records C2 the same way, adding every HTTP exchange the
server makes (urllib is patched in the server process): lean_local_search
on the core and Mathlib fixtures, before and after a language server is up
and without ripgrep; leansearch.net, Loogle, Lean Finder and leanpremise.net
called live once, Loogle's rate limit included; and their failures (HTTP 500,
bad JSON, an error answer, a read timeout, a refused connection) against a
stub on 127.0.0.1 (54 steps per toolchain, `fisher-lean/test/golden/c2/`).
Offline, the replay answers Fisher's HTTP from those exchanges (Fisher must
send the request urllib sent) and runs the real `rg` over the installed
toolchain's sources.

The C2 gate (claude-code uses Fisher's server) held on 2 Oct 2026 on the Mac:
`tools/gate-c2.sh` staged `/prove` on the core fixture with `fisher plan`
(the staged `.claude.json` launches `fisher lean-mcp`), ran that child
(Claude Code 2.1.285) headless, and it called lean_goal, lean_local_search
and lean_loogle on Fisher's server, which answered `⊢ double 2 = 4`,
`Fixture.double` and `Nat.add_comm`; the trap `uvx` was never called. The C2
replay passed on both toolchains (fisher-lean 30 examples, the C1 profile
steps still live-only), the negative control failed all 98 spoiled offline
steps, `tools/e2e-c2.sh` matched upstream on all 54 calls, and the other
suites were unchanged (their upstream recordings run with
`FISHER_LEAN_MCP=uvx`), with no warnings.

C3 adds a native backend, `fisher.autoformalize.backend: fisher`: the managed
workflows run on `fisher chat` (Fisher's own agent loop) with what the
claude-code backend gets, which is the lean4 skill and the workflow's
command doc (inlined in a system prompt file, where Claude Code would expand
the slash command), the Lean MCP server (`--mcp-config`) and the same Opus.
The model goes through OpenRouter (`anthropic/claude-opus-4.6`) by default;
`fisher.autoformalize.native_provider: anthropic` (or
`FISHER_AUTOFORMALIZE_NATIVE_PROVIDER`) takes a Console key or token
instead, since a Claude subscription login is served to Claude Code only.
The tools are the `fisher-lean` toolset: files, terminal, web search and the
MCP server's. `fisher chat` gained the flags a headless run needs: `-p`,
`--max-turns`, `--mcp-config`, `--system-prompt-file` and
`--output-format stream-json`.

The benchmark (`tools/bench/c3/goals.json`) is 40 miniF2F problems
(google-deepmind/miniF2F at f0a20e14, ported to Mathlib v4.33.0) and 20
TWISTED lemmas re-proved after their proofs are erased (a read-only clone of
~/TWISTED-lean at 9fe548c4; QUIPU has two eligible lemmas, so its other two
places went to the other directories). A goal is solved if the file
compiles with no sorry, the statement and every other line of the file are
kept, the theorem's pp.all type is the starting file's, nothing but
helpers was added (no axioms, `#exit` or kernel-off options), no other file
changed, and `#print axioms` shows only propext, Classical.choice and
Quot.sound (TWISTED goals may also use native_decide, which Lean 4.33 makes
one axiom per use). Every run is 40 turns and 20 minutes at most.

The C3 gate (fisher solves at least as many as claude-code) held on 3 Oct
2026, `/prove`, 56 of 60 each: miniF2F 36/40 each (the same four missed),
TWISTED 20/20 each; median 116 s per goal for fisher against 333 s.
`tools/bench/c3/gate-report.md` has every goal. Caveats: claude-code's
first 52 goals were scored during the run by an older scorer (stricter on
added lines, without the type check; its files were later overwritten, so
they could not be scored again), which can only have helped it; 22 fisher
goals (OpenRouter credits ran out) and 4 claude-code goals (its login
dropped) were run again; and the scorer was fixed after the run to let a
term-mode proof drop the statement's trailing `by`, which turned fisher's
`WCL.Symmetry.orbit_invariance` into a solve (55 to 56).

D adds the proof-search swarm, `fisher search FILE` (package
`fisher-search`). Each open goal (a declaration whose proof is `sorry`) is
an attempt on one backend in its own copy of the Lake project (APFS clone,
`.lake/build` cloned, `.lake/packages` linked), running the managed
workflow headless: claude-code `-p` with stream-json, fisher `fisher chat
-p`, codex `codex exec --json` (built, not yet run: codex is not
installed). The agent's proof of its goal, and any helpers it added, are
spliced into the shared base (its edits to other declarations are dropped;
changed statements, removed lines, name clashes, added axioms, `#exit`,
kernel-off options and helpers left `sorry` refuse the splice), and the
kernel judges the merged file as C3's scorer does. A proof whose only
`sorryAx` comes from goals still open waits and is checked again after
each merge. The scheduler gives every goal a first attempt before any
second, retries on a backend that has not tried the goal, runs a goal on
two backends when slots are spare (cancelling the other when one closes
it), and stops at the budget. `--single BACKEND` is the baseline: one child
at the whole file. The run directory holds events.jsonl, report.json,
base.lean and every attempt's transcript; `--apply` writes the merged file
back. `fisher search check` judges one saved proof the same way.

The D gate (the swarm closes more goals per hour than a single claude-code
child) held on 4 Oct 2026, on TWISTED's `Step/Kernel.lean` with all 12
eligible proofs erased (122 lines): the single claude-code `/prove` closed
12/12 in 1588 s (27.2 per hour), the swarm (claude-code and fisher, 4
slots) 12/12 in 1368 s (31.6 per hour), 14 attempts, 6 of them waiting on
a goal another attempt closed. `judge.py` agrees with both reports.
Caveats: one run each; both finished every goal long before the 2 h
budget, so the rate is the finishing time and the margin is 16 %; the
swarm spent 2.4 times the agent time (3780 s across attempts) and about 8 M
input tokens on fisher's side.

`fisher search FILE --graph` searches the file's open goals as a lemma
graph (docs/superpowers/specs/2026-10-14-lemma-graph-design.md). Each goal
gets a direct attempt on the native backend; a goal its attempts cannot
close is split by a planner model call into `sorry` lemmas and a proof from
them, the sketch is compiled and checked, a verifier model call reviews the
split, and an accepted split is merged into the base and its lemmas become
goals in turn (a lemma with the key of a proved or open goal reuses or
shares it; one with an ancestor's key is a cycle). Each root goal keeps a
whiteboard (its tree of splits and the notes of failed attempts, capped at
2,000 characters) that workers and the planner read. A goal with two failed
splits fails, and so does its parent's split; a split whose lemmas are all
proved but whose sketch then does not close fails too. Only the goals of a
root's current plan are worked on, and workers and the planner are told to
use only its lemmas among those still `sorry`. Lemmas of a failed split left
with `sorry` and named by no other declaration are removed from the base,
when the split fails and again at the end of the run. The run directory adds
`graph.json` and `whiteboard-<n>-<root>.md`, `events.jsonl` gains the
graph's events, and `report.json` the tree per root, dollars by role
(worker, planner, verifier) and why the run stopped (`time`, `dollars` or
`credit`). `--apply` writes the merged file back only when no lemma from a
split is left with its `sorry`; otherwise it says how many are and writes
nothing. Its flags, with today's `--parallel`, `--budget`,
`--attempt-minutes`, `--max-turns`, `--out`, `--apply`, `--allow` and
`--native-decide`:

- `--max-depth N` (3): how deep lemmas go; a goal at the limit gets two
  direct attempts and no split.
- `--max-splits N` (2): splits tried per goal.
- `--cap-usd X` (25): dollars over all roles; once reached nothing new
  starts and running attempts finish, so the cap can be passed by what up
  to `--parallel` running attempts spend. It is the stop reason only when
  some root goal is still open at the end.
- `--model M`: the model for the planner and verifier calls (through the
  native agent's provider; default: the configured `model`) and for the
  native workers, whose `-m` it replaces. It also works without `--graph`,
  for the native backend's attempts.
- `--backends` defaults to the native backend and may name only it, as
  `fisher`; `--single` and `--one-per-goal` are refused.

### Automation before the models

`fisher search` (flat and `--graph`) first tries to close or refute each
goal without a model call (docs/superpowers/specs/2026-10-15-auto-prepass-design.md).
Three parts, all through one Lean language server per run (fisher-lean,
called as a library, started in the search's check workspace; its trials are
virtual documents, so nothing is written to the project):

- Auto: before a goal's first attempt, a tactic portfolio is tried at each
  of its holes in order, `rfl`, `decide`, `omega`, `simp_all`, `aesop`,
  `grind`, `exact?`, each as `set_option maxHeartbeats 200000 in <tactic>`
  with a wall cap per hole. A `Try this:` suggestion replaces the searching
  tactic (Lean 4.33 prints it on the next line, after `[apply]`); a fill
  that still holds `exact?`, `apply?` or `aesop?` never wins. A term hole (`:= sorry`) is filled as `by <tactic>`. The goal
  closes only when every hole does; the filled file then goes through the
  same kernel check as a model's proof, and the attempt is never launched.
- Repair: a failed attempt's failing tactic units become `sorry` (at most
  4 rounds, 120 s in all), Auto runs on the new holes, and a result with at
  most 5 holes left is kept as a partial proof: the goal's next attempt
  starts from it.
- Disprove: `plausible` runs at the goal's hole. A counterexample alone
  marks the goal `suspect` (a hint in the next brief, never a verdict); a
  negation `theorem _fisher_neg_<k> : ¬ (type_of% @NAME)` proved by
  `decide`, `simp_all`, `norm_num` or `omega`, checked by the kernel with
  its axioms printed (no `sorryAx`, nothing the axiom policy does not
  allow) marks it `disproved`, and the search stops spending on it. Each
  probe has the per-hole cap.

In flat search a goal's pre-pass runs just before its first attempt is
launched, so the slots fill as each pre-pass ends; the lemma graph runs
the roots' pre-pass before its loop. Every text sent to the language
server, and every certify probe, stops after the declaration being worked
on (the file then ends inside any open `namespace` or `section`, which
Lean accepts), so a trial elaborates only up to its goal. A call cut off
at its cap is logged as an `auto timeout` event (goal or node, and the
step: `auto`, `repair` or `disprove`).

The kernel accepts every proof: nothing here marks a goal proved on the
language server's word. The server runs in the search's check workspace
(the run directory's `base`); it elaborates the whole file once before
the search starts (the warm-up, at most 600 s, counted in no budget and
printed). If the server does not start, its warm-up fails or runs out of
time, or it dies, the run logs one `auto off` event, closes the server and
goes on as `--no-auto`. `--single` never starts it. Whenever the flags ask
for the automation, `report.json` says `auto_requested`, `auto_ran` (false
when the server did not start, or went off before any automation step
completed) and `auto_off` (why, or null); `auto_warmup_s` is the warm-up's
seconds when the server started.
A flat run with the automation on ends with a table of the goals, their
status and what the automation did (`closed`, `repair`, `disproved`,
`suspect` or `-`); `events.jsonl` and `report.json` gain the automation's
events and fields.

- `--auto` (default) and `--no-auto`: `--no-auto` is exactly the search
  without the automation (no server, no new events or fields).
- `--no-repair`, `--no-disprove`: turn off one part.
- `--auto-tactics A,B,...`: the portfolio in place of the default.
- `--auto-seconds N` (60): the wall cap per hole.
- `--repair-seconds N` (120): the wall cap on one Repair.
- `--auto-warmup-seconds N` (600): the cap on the server's warm-up.

Requirements: `lake` on PATH (the Lean toolchain through elan) and a
project whose imports are built. Disprove needs `plausible` and the certify
tactics, so the file should import Mathlib (or `Plausible` and
`Mathlib.Tactic.NormNum`); without them those calls fail and count as
misses. Tested on macOS with the TWISTED bench clone's Mathlib (Lean
4.33.0): loading Mathlib's imports takes from seconds to a minute on the
first elaboration, before the search starts.

### Retrieval and the lemma library

`fisher search` (flat and `--graph`) also gives each goal the project's
own facts and the lemmas earlier runs proved
(docs/superpowers/specs/2026-10-16-retrieval-library-design.md).

- The index: before the run, every declaration of the file's Lake package
  (its `srcDir`; `.lake` and other packages left out) is indexed once,
  with its kind, statement, file, line, module and the project names it
  uses. Each file's scan is cached in the library directory's
  `index.json`, so a later run rereads only changed files. A file that
  does not scan is left out with an `index skipped` event; `index built`
  gives the files, declarations and seconds.
- Facts: per goal, the project constants its `pp.all` type uses, the
  constants their statements use, then BM25 keyword matches on the goal's
  statement, only declarations the goal's file can see (above it in the
  same file, or in its import closure), at most 20 and 8,000 characters,
  as "Relevant project facts:" in the brief. At most 5 of the 20 are of
  the header-only kinds (structure, inductive, class, abbrev), across all
  three tiers; the slots they would take go to the next facts in order, so
  theorems, lemmas and defs reach the brief.
- The library: up to 5 lemmas from earlier runs that share a project
  constant with the goal (each still in the index and visible from the
  file; none stating an open goal or already in the file) and up to 3
  notes, as "Lemmas proved in earlier runs" in the brief. The constants a
  lemma's proof uses must be visible too; one that is another library
  lemma (a helper using a helper from the same run) is offered with it,
  just before it. A pasted lemma is a helper like any other: the kernel
  checks it with the goal.
- The brief: facts, then the library section, then any repair holes or
  counterexample, in flat attempts, graph workers and the planner's
  message. A goal's retrieval is made once, at its first launch (a node's
  before its first worker or planner call), and cut off after 10 s with
  what was ready; `facts` and `library offered` events give what it got,
  with `cut` true when it was cut off. A cut-off retrieval is made again,
  once, at the goal's next launch.
- The harvest: at the end of a run the merged file's new sorry-free
  theorems and lemmas (and in graph mode the roots' failure lines and the
  workers' `NOTE:` lines) are appended to the library, under a lock, as
  `library saved`. A lemma whose proof names a declaration new in the run
  (or one still `sorry`) is kept only when that declaration is kept too,
  and a lemma whose statement the library already has (under any name) is
  not saved again; an offer never shows two lemmas with one statement.
  A library file that does not parse is set aside as `*.bad` (a `library
  problem` event) and a fresh one started. A run that ends in an
  exception, Ctrl-C or SIGTERM harvests the merged file as it is then
  (at most 20 s), stops its attempts and exits (SIGTERM with code 143).
- `report.json` gains per goal (graph: a `retrieval` list per node)
  `facts`, `offered` and `used` (offered lemmas named by their full name
  in the goal's merged proof, or merged as its helpers under the full
  name, or under another name only with the lemma's own statement), and
  `facts_mean`, `offered_total`,
  `used_total`, `saved_lemmas`, `saved_notes`.

The library lives in `$FISHER_HOME/fisher/library/<key>/` (`root.txt`,
`index.json`, `lemmas.jsonl`, `notes.jsonl`), the key the first 16 hex
characters of the sha256 of the package root's absolute path. Nothing here
can stop a search or mark a goal proved. `--single` uses none of it.

- `--no-retrieval`: no facts (the index is still built for the library).
- `--no-library`: nothing offered and nothing saved.
- `--no-retrieval --no-library`: exactly the search without either (no
  index, no new events or fields).
- `--facts N` (20), `--facts-chars N` (8000): the facts' caps.
- `--library-dir DIR`: DIR is the library directory itself, with no
  per-project key under it, so runs on different workspaces of one
  project (the bench's clones) share one library; each package root keeps
  its own index cache there, `index-<key>.json`.
- `--library-read-only`: offer from the library, never save to it.
- `fisher search FILE --print-facts`: build the index, compile the file
  once for the goals' types (keyword facts only if it does not compile),
  print each open goal's facts and library section, and stop; no attempt,
  and nothing written to the library directory or under `$FISHER_HOME`
  (the library is read without its lock, the index cache goes to a
  private temporary directory, mode 700, removed once the index is built).

E adds `fisher acp`: the native agent behind the Agent Client Protocol
(JSON-RPC on stdio), for editors such as Zed, VS Code and JetBrains, as
upstream's `acp` command. A session holds an agent (platform `acp`, toolset
`fisher-acp`) and its conversation; a prompt streams the spinner's lines as
thoughts, each tool as a `tool_call` and its `tool_call_update`, then the
answer; `session/cancel` interrupts the turn; /help /model /tools /context
/reset /compact /version are answered without the model. Upstream's
reference pins agent-client-protocol 0.8.1 (the only release its adapter
imports with), under which session/list, fork, resume and set_model are
unstable methods and answer "Method not found", as they do in Fisher.
`tools/corpus/e1.py` records 29 upstream sessions over a scripted model
(every message both ways, the model requests, the tools); Fisher's spec
matches all of them, and `tools/negative-e1.sh` spoils each and sees each
fail. `tools/e2e-e1.py` drives `fisher acp` against the real model: it
reads a file through a tool call, answers /context and cancels a long
turn, with nothing but JSON-RPC on stdout.

E also adds `fisher cron`, upstream's `cron` command (list, create, edit,
pause, resume, run, remove, status, tick) on the same `cron/jobs.json` of the Fisher home
and output files, with upstream's text. Schedules are upstream's: a
duration once, `every N` (minutes, hours, days), a cron expression (a port
of croniter 6.0.0, daylight-saving quirks included) or an ISO timestamp,
in the zone `FISHER_TIMEZONE` or config.yaml's `timezone` names (read from
the system's tz database) or else the system's local offset. A due job
runs on the native agent (platform `cron`, every toolset but `cronjob`,
the session saved in state.db). Upstream fires jobs from its messaging
gateway, which Fisher does not port; Fisher's ticker is `fisher cron serve
[--interval SECONDS]`, in the foreground (run it under launchd or systemd
to keep it), with its PID in `cron/ticker.pid`. `tools/corpus/e2.py`
records schedules (648 cases in three zones), croniter (615 fire
sequences and 18 errors), the `cronjob` tool, 9 CLI sessions and 15 ticks
over a scripted model; all match, `tools/negative-e2.sh` spoils them and
sees them fail, and `tools/e2e-e2.sh` creates a job, runs `fisher cron
serve` against the real model and checks the answer, the job's removal
and the session row.

### Build watcher

When the work is driven from the Cowork VM, builds run on the Mac through
the watcher. Start it once in a Mac terminal and leave it running:

```
tools/build-watcher.sh
```

From the VM shell, in the mounted folder:

```
tools/bw.sh run "<one-line command>"   # queue; default with no command is `stack build`
tools/bw.sh wait 170                   # wait for pickup and completion, print exit and log tail
```

A queued command must be one line and runs under zsh from the repo root.
Redirect long output to a file under `.build/` rather than piping it into
`head`. The watcher's environment is the one its terminal had when it
started.

## Known differences from upstream

Behaviour is upstream's unless listed here.

- `str()` of a YAML float that is a whole number (`1.0`) renders as `1`.
  No field upstream reads as text holds one.
- Every Lean project that `project init` detects markers for is flagged as a
  blueprint, because upstream lists `lakefile.lean` as a blueprint marker.
  This is upstream's behaviour, kept and pinned by a test.
- `fisher project use`, `init`, `convert` and `create` set an override that
  later `fisher` commands see, kept in `$FISHER_HOME/fisher/project-override`
  until `fisher project clear`. Upstream keeps it in the REPL session; a
  one-shot command has nowhere else to keep it.
- `fisher doctor` covers configuration files, the Fisher home directory,
  external tools (git, rg) and the managed Lean workflows. Upstream's
  Python, package, provider-portal, API, submodule, tool and skills-hub
  sections are for machinery Fisher does not have yet. It does not read the
  session count from `state.db`, and `--fix` writes a missing `config.yaml`
  from the defaults rather than from `cli-config.yaml.example`.
- `fisher config set` on a `config.yaml` that is not a mapping refuses and
  changes nothing; upstream raises a TypeError.
- Migration visits missing config fields in sorted key order. At the pinned
  upstream there are never any, because the loaded config is already merged
  with the defaults.
- A hand-written manifest with an unquoted `created_at` keeps the text as
  written; Python reads it as a datetime and prints it with a space for the
  `T`. Manifests either side writes are quoted.
- Hints name `fisher`, not upstream's program (`fisher setup`, `fisher doctor --fix`).
  `fisher setup` is not written yet (see A3 below).

From A2:

- The child's environment is built from the base environment Fisher is
  given (its own, when run as `fisher`), not merged again with the process
  environment as upstream's `os.environ` copy does. As `fisher` the two are
  the same.
- Lines of session output that are JSON but not an object are skipped;
  upstream raises on them inside its reader and loses the rest of the
  stream's progress.
- Cancel stops the session's whole process group, so tools a session
  started go too; upstream signals only the direct child.
- The arXiv helper is installed to `$FISHER_HOME/fisher/scripts/` rather than
  read from the Python package's directory.
- Progress events are parsed while a session is attached as well as while
  it runs in the background.
- `fisher launch` and `fisher chat` stay attached until the session exits;
  detach and reattach need the long-running fisher shell (A3). If input
  closes while the session runs (or stdin is not a terminal), the session
  is cancelled, since nothing could reach it again. `fisher launch
  --print` runs it headless instead (see the provers follow-ups below).
- The terminal title says `Fisher`, not upstream's name.
- Whether to pass root-safe permission flags comes from the machine
  record, which the tests set from the recording, rather than from
  `os.geteuid()` at the call site.

From A3:

- The agent's commands (`/new`, `/history`, `/model`, `/retry`, `/undo`,
  `/compress` and the rest) are not ported yet; they answer that they need
  the native agent, and `/help` does not list them. Plain text goes to the
  native agent (B3).
- Not in A3: `setup`, `status`, `login` and `logout` (they configure
  upstream's own agent's providers; moved to B1), and `gateway`, `cron`,
  `pairing`, `skills`, `tools`, `model`, `whatsapp`, ACP and claw (later
  phases).
- `fisher version` prints Fisher's version and the upstream release it
  matches. `config show` gives the `fisher` executable's directory as the
  install path.
- Without `config.yaml` the shell uses its built-in defaults; upstream reads
  and writes `cli-config.yaml` in its own checkout, which Fisher does not
  have.
- Kept from upstream, pinned by the recordings: `display.compact` in the
  config is ignored (only `--compact` counts), and the `[task-id]` in the
  `/swarm` usage line is eaten as markup.
- Fisher keeps its own files under `$FISHER_HOME/fisher/` (the project
  override, the arXiv helper); upstream never reads them.

From B1:

- Requests carry `User-Agent: Fisher/0.2.0` (Kimi's `KimiCLI/1.3` is kept)
  and none of the SDKs' `x-stainless-*` headers. A Codex request with no
  tools leaves `tools` out; upstream's SDK cannot build that request at all.
- Timeouts are http-client's response timeout (900 s for chat completions
  and Anthropic, 600 s for Codex); the SDKs' separate connect limits are not
  enforced.
- On a terminal, command hints say `fisher` (`fisher model`, `fisher setup`);
  the port prints upstream's program name and the terminal output is rewritten,
  so the recordings still compare.
- `fisher status` shows Fisher's executable directory as the project and
  "n/a (Fisher)" for Python, and `--deep` leaves out the port 18789 probe.
- Menus are always the numbered ones upstream falls back to when curses is
  unavailable; there is no arrow-key menu yet.
- Kept from upstream, pinned by the recordings: `fisher login` only says the
  command was removed (the logins live in `fisher model` and `fisher setup
  model`); setup's Codex "Keep current" asks for a model name; Claude Code credentials are read from
  `~/.claude/.credentials.json` only (macOS keeps them in the keychain, so
  there `fisher model` does not see a Claude Code login); `status` writes the
  home directories and `auth.lock`, and migrates `~/.codex` tokens.
- Also kept, though it comes from upstream's reference environment rather
  than its code: with any Anthropic credential present, setup offers the
  vision step and the summary lists vision as unavailable (building the
  Anthropic client fails in upstream's venv, which empties its vision
  backend list).
- Setup's Modal and Daytona steps pip-install their SDKs into the
  `python3` on PATH, where upstream uses its own venv's Python.

From B2:

- The singularity, modal and daytona sandboxes are not ported.
- Docker and ssh output is decoded with replacement characters, and stdin is
  written while output is read; upstream decodes strictly (a bad byte loses
  the rest of the output) and writes stdin first, which can deadlock on
  large input.
- With `TERMINAL_DOCKER_MOUNT_CWD_TO_WORKSPACE` set, the file tools' docker
  sandbox bind-mounts the host directory; upstream's file tools create the
  container without the mount.
- On macOS, bash sometimes reports "child setpgid ... Operation not
  permitted" inside a command's output; Fisher drops that line. A sandbox
  command in `/` is started in `/.` (the process library cannot start a
  child in `/` when it also sets the environment there).
- An MCP server that misses its connect timeout is stopped; upstream leaves
  the attempt running in the background, unregistered.
- MCP sampling asks the main provider and model (or `AUXILIARY_MCP_MODEL`);
  upstream's auxiliary-model routing for task "mcp" comes with the agent
  loop (B3).
- pydantic's and httpx's error texts are reproduced for the shapes the
  recordings show (missing fields, wrong types, bad URLs); others are close
  but not exact. Output-schema checks cover jsonschema's common keywords
  (type, properties, required, items, additionalProperties, enum, const,
  numeric and length bounds).
- A tool argument of the wrong type (a string offset, say) gets a
  Python-like error, but not always upstream's exact text or timing.
- Idle sandboxes are not reaped while a session runs; they are cleaned
  up when it ends.

From B3:

- The request debug dump (`sessions/request_dump_*.json`) holds the body
  as Fisher sends it on the wire; upstream's holds the arguments of its SDK
  call.
- Context files are found by a directory walk in sorted order; upstream's
  `os.walk` order depends on the file system. The prompt-injection scan
  lists invisible characters in code-point order; upstream's comes from a
  set.
- A 401 inside the loop is not retried with refreshed credentials (Codex,
  Anthropic); it is handled as a client error, as upstream does when a
  refresh fails.
- Not yet: `errors.log`, trajectory files, checkpoints, session search and
  skills, and the agent's slash commands.
- Responses in the box are plain text (no markdown rendering), with ANSI
  stripped, as upstream prints them when the output is not a terminal.
- `/clear` in the shell starts a new native session; the old one is not
  marked ended in `state.db` (upstream ends it with reason `new_session`).
- A turn interrupted with Ctrl-C is logged, but replay does not reproduce
  the interrupt, so it does not replay exactly.
- Kept from upstream: `-Q` with `--resume` runs the query without the
  resumed history (upstream's quiet path calls `run_conversation(query)`
  with no history); and, pinned by the recordings, the auxiliary client's
  cache is keyed without the model, so a second side task with another
  model reuses the first client.

From B4:

- Kept from upstream, pinned by the recordings: no built-in toolset holds
  the terminal, process, todo or memory tools, so `-t terminal` is an
  unknown toolset and the model is offered them only through a custom
  toolset. The process tool appears once a terminal or file tool has run;
  todo (and memory, when it is on) join the registry after the first agent
  is built. In Fisher that registry belongs to the chat session, not the
  process, so a shell `/clear` starts over.
- Kept from upstream: a saved command allowlist is never read back in a
  later process; unquoted `off` for `approvals.mode` is YAML false, so the
  mode is manual; the memory nudge is reset at the start of every turn, so
  it fires only with `nudge_interval: 1`; and a non-string tool message in
  the history makes the todo hydration raise.
- Firecrawl: the client version is the one recorded (upstream does not pin
  it); a connection error's text differs from requests'; search results
  that carry scraped content are dumped raw; `AUXILIARY_WEB_EXTRACT_MODEL`
  is read when the session starts.
- Browser: the repo's `node_modules` lookup is skipped (Fisher has no
  repo); summarised pages go through Fisher's auxiliary client, and
  concurrent summaries may finish in another order.
- Terminal: commands always run over a pipe (no pty); tirith is used when
  it is installed and never downloaded; the saved allowlist is written in
  sorted order; the tty sudo prompt is also used in the shell; background
  processes are not reaped while a session runs.
- Memory: when content holds several invisible characters, the first in
  code-point order is reported (upstream iterates a set); a USER.md that is
  not valid UTF-8 leaves MEMORY.md's entries unloaded too (upstream keeps
  them).
- The memory flush without an auxiliary client uses the agent's own
  client: for Codex it sends no temperature.

From E:

- `fisher acp` writes nothing but JSON-RPC to stdout. Upstream's agent
  prints its tool spinner and error lines there too, between the messages;
  Fisher sends that output to stderr.
- Every ACP session writes a replay log, as `fisher chat` does (a
  cancelled turn cannot be replayed: interrupts are not in the log).
- `fisher cron` points at `fisher cron serve` where upstream points at
  its gateway (upstream's `gateway install`), and adds `serve`. Delivery to a
  chat platform (telegram, discord, `platform:chat_id`, origin) needs the
  gateway, which is not ported: the result stays in the job's output file
  and a warning says so. A failed run's output file ends without
  upstream's Python traceback.
- The terminal's approval prompt is not bridged to
  `session/request_permission`: upstream's ACP toolset has no
  terminal tool, so the bridge never runs there.

From D:

- `fisher search` is Fisher's own command; upstream has no swarm.

From C3:

- `fisher` is a backend of Fisher's own. It is not listed in
  the "must be one of" and "Available" messages, which stay upstream's;
  `/chat` with it is refused (talk to Fisher itself).
- `fisher chat -p`, `--max-turns`, `--mcp-config`, `--system-prompt-file`
  and `--output-format stream-json` are Fisher's (the stream mimics Claude
  Code's: `system/init`, `assistant` tool_use, `result`).

From C1 and C2:

- Managed workflows launch Fisher's own server, `fisher lean-mcp`, and need
  no uv; `FISHER_LEAN_MCP=uvx` goes back to `uvx --from SPEC lean-lsp-mcp`.
  The recorded launch plans and doctor transcripts are upstream's, so their
  specs run with `FISHER_LEAN_MCP=uvx`; doctor reports "Lean MCP server
  (fisher lean-mcp)" where upstream checks for uv.
- Not ported: the REPL fast path (off unless `LEAN_REPL` is set upstream),
  local Loogle (`LEAN_LOOGLE_LOCAL`), the build coordinator's `cancel` and
  `queue` modes (builds run as called, as with the default `allow`), and
  progress notifications from `lean_build` and the remote searches.
- HTTP failures are worded as urllib words them (`HTTP Error 500: ...`,
  `<urlopen error [Errno 61] Connection refused>`, `timed out`); a body
  that is not JSON gets orjson's message for its first character, which is
  exact for the common cases only. `FISHER_LEANSEARCH_URL` (not upstream's)
  moves leansearch.net, for the live check's stub.
- `lean_local_search` reads ripgrep's output to the end instead of stopping
  it at the candidate cap; the matches kept are the same.
- `lean_verify` scans the source itself instead of running `rg --json`;
  it still reports ripgrep as missing when `rg` is not on `PATH`, as
  upstream does.
- Kept from upstream, pinned by the recordings: the file watcher marks
  every other open file stale on any `.lean` change (including
  `lean_profile_proof`'s temporary file), `lean_get_widget_source` asks at
  position 0:0 so a widget defined in the same file is "not registered",
  the outline cache is process-wide, and `lean_file_outline` can list a
  declaration twice (from the server and from the source text).
- Scratch slots are handed out in the order calls arrive, as asyncio's
  queue does; with one slot (the default) that makes trials sequential.

From the CLM4 fixes:

- A batch of tool calls in which a `write_file` or `patch` names a file
  that another call of the batch reads or writes runs one call at a time,
  in the model's order. Upstream runs every batch without `clarify` in
  parallel, and two patches on one file can lose an edit. Paths are
  compared after relative ones are taken against the working directory,
  repeated `/` and `/./` are collapsed, and macOS's `/private` before
  `/var`, `/tmp` and `/etc` is dropped. `terminal` and `execute_code` are
  not read for paths, so a shell edit still runs beside the rest.
- `--output-format stream-json`'s `result` carries `total_cost_usd`, the
  sum of the provider's own `usage.cost` over the session's model calls
  (OpenRouter reports it; the Anthropic API does not), or null when any
  call reported none. The compressor's summary calls are not counted.
  `usage` also carries `cache_read_input_tokens` and
  `cache_creation_input_tokens`, summed the same way.
- Kept from upstream, noted because it bites benchmarks: `config.yaml`'s
  `compression.enabled`, `.threshold`, `.summary_model` and
  `.summary_provider` are copied into the environment over
  `CONTEXT_COMPRESSION_*` when a session starts, so a per-run threshold
  goes through `fisher config set compression.threshold` in that run's
  `FISHER_HOME`.
- Kept from upstream: no `max_tokens` is sent, so OpenRouter checks a
  key's remaining credit against the model's whole output limit, and a key
  near its limit gets a 402 early. A 402 ends a headless session with
  `error_during_execution`.

From the provers follow-ups:

- The exit summary's `tool calls` counts each executed call once, as its
  tool result; upstream also counts every assistant message that carries
  calls, so four calls over three messages printed 7. The results an
  interrupt writes for calls it skipped are not counted. The recorded
  transcripts keep upstream's line, and their spec takes the count from
  the recorded rows.
- Under `--output-format stream-json`, with `-q` as well as `-p`, stdout
  carries only the events, one JSON object a line, and always ends with
  one `result` event; the banner, the tool lines, the response box, the
  exit summary and any prompt go to stderr (no password prompt is asked).
  When the agent cannot be started the `result` says so with `is_error`.
  `result` also carries `num_tool_calls`, the tool calls the agent ran.
- The managed workflows' model can be chosen (upstream's is fixed):
  `fisher launch --model M` (or `--model M` in the command, as the shell
  takes it), else `fisher.autoformalize.model`, else the old default. Each
  backend's child gets it in its own flag; codex gets `-m` only when one
  is chosen.
- `fisher launch --print` (`-p`; upstream has no such flag) runs the
  planned child headless, `fisher chat -p`, `claude -p` or `codex exec`,
  with stdin from /dev/null, and exits with its status. `fisher plan
  --print` prints that plan.
- Under `-p` or `--output-format stream-json`, `fisher chat` exits 1 when
  the run fails (the `result`'s `is_error`: the agent could not start,
  the run raised, or it did not complete), so `fisher launch --print`
  reports it; `-q` and `-Q` alone keep upstream's 0. An exception from the
  run under `-p` is caught and ends the stream with an error `result`.
- Under `fisher chat -p` without stream-json, stdout carries only the
  answer and the `session_id:` line; the tool progress goes to stderr.
  `-Q -q` keeps upstream's stdout.
