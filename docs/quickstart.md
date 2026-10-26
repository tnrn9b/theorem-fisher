# Quickstart: Fisher from the terminal

This page gets you from a checkout to a proved theorem with the `fisher`
command. For the web app, see `docs/quickstart-ui.md`.

## 1. Build and install

You need stack (LTS 22.44 is pulled in for you), and for Lean projects elan
and `lake` on `PATH`.

```
cd ~/IdeaProjects/Fisher
stack build
stack install          # copies fisher to ~/.local/bin; put that on PATH
fisher version
```

Without `stack install`, use `stack exec fisher -- <args>` everywhere below.
A release tarball (macOS arm64) works too: unpack it and put its `bin` on
`PATH`.

## 2. Give it a model

Fisher's own agent (the `fisher` backend) talks to OpenRouter. Put your key
in the home's `.env`:

```
fisher setup model                 # interactive: provider, model, key
# or by hand:
echo 'OPENROUTER_API_KEY=sk-or-...' >> ~/.fisher/.env
fisher config set model.default anthropic/claude-sonnet-5.5    # the shell and fisher chat
fisher config set fisher.autoformalize.backend fisher          # workflows on Fisher's own agent
fisher config set fisher.autoformalize.model anthropic/claude-sonnet-5.5
fisher llm-check                   # one small request; says if the key works
```

Workflows (`/prove` and the rest) run on the `claude-code` backend unless
`fisher.autoformalize.backend` says otherwise; without the model setting the
`fisher` backend uses Opus on OpenRouter.

The home is `~/.fisher` (or `$FISHER_HOME`). A home from before the rename
is not read: move your previous home directory to `~/.fisher` and rename its
config root to `fisher:`. `fisher config env-path` prints the `.env` in
use.

The `claude-code` and `codex` backends use those tools' own logins instead.

To log in to Claude Code from Fisher, run
`fisher login --provider claude-code` (add `--console` for an Anthropic
Console account with API billing, `--sso` to force SSO, `--email E` to name
the account). It runs `claude auth login`, which opens the browser, then
prints `Logged in to Claude Code as EMAIL (PLAN)`. Claude Code keeps the
token; Fisher never reads it. `fisher logout --provider claude-code` logs
out, `fisher status` shows a Claude Code line, and `fisher doctor --fix`
offers the login when Claude Code is logged out. The `claude` CLI must be
installed first: `npm install -g @anthropic-ai/claude-code`.

## 3. Point it at a project

From the root of a Lean project (the folder with `lakefile.lean` or
`lakefile.toml`):

```
fisher project init                # writes .fisher/project.yaml
fisher doctor                      # checks Lean, lake, the MCP runtime, keys
fisher prewarm                     # optional: fetches the Lean skills once
```

Or start a new Lean project from a template. It writes the files, builds
them and checks a sample theorem before it says `Ready:`:

```
fisher project templates                                   # lean and lean-mathlib, and any configured ones
fisher project create ~/proofs/demo --template lean-mathlib
fisher project create ~/proofs/plain --template lean --var lean=v4.24.0 --no-build
fisher project build ~/proofs/plain                        # the remaining steps, then the check
```

With neither `--template` nor `--template-source`, a configured template
source (`fisher.project.template_source` or
`FISHER_BLUEPRINT_TEMPLATE_SOURCE`) is used, else `lean-mathlib`. `elan`
and `lake` must be on PATH (with `--no-build`, a missing one is only a
warning).
Variables (`--var NAME=VALUE`) default to the latest release; offline,
pass the version. Output goes to `.fisher/create.log`. A step that fails
or is interrupted (Ctrl-C) leaves the project; `fisher project build`
picks up at that step. A configured template (`fisher.templates.NAME: PATH
or git URL`) runs its own commands, so the first create from it lists its
steps and asks, and so does `fisher project build` where it is not yet
trusted; `--yes` answers yes. `--trust-revision REV` answers yes only
while the template is at revision REV (the one whose steps you saw), and
otherwise stops with "the template changed since it was confirmed"; the
web app uses it. A failure exits 1, a wrong use 2.

To use the `smt` tactic in an existing Lean project, run
`fisher project add smt [PATH] [--rev REV] [--no-build] [--yes]` (PATH
defaults to the project in reach). It adds the require for lean-smt to
the lakefile: one `require smt from git ...` line in `lakefile.lean`, or
one `[[require]]` block in `lakefile.toml`, placed before Mathlib's require
so that Mathlib's versions of the packages both depend on take precedence
(at the end when there is no Mathlib). The revision is the newest
commit Reservoir built on the project's `lean-toolchain`; with no such
build, or Reservoir out of reach, it is `main`, with a warning. `--rev REV`
wins over both, and a project without Mathlib gets lean-smt's
`no_mathlib` branch. It asks before the lakefile changes (`--yes` answers
yes), then runs `lake update smt`, `lake build` and a check that
`example : ∀ x : Int, x + 0 = x := by smt` closes, and says
`Ready: smt`; `--no-build` stops after the lakefile. A lakefile or
`lake-manifest.json` that already has smt is left as it is (exit 0,
"already present"). Output goes to `.fisher/add-smt.log`. A failed update,
build or check exits 1 and prints the revert line ("delete the require
line for smt in LAKEFILE"). The `lean-mathlib` template requires lean-smt
before Mathlib, and its sample ends with that `example`. The template's
`smt` variable sets the revision: `latest` (the default) is the newest
Reservoir build for the Mathlib release. With no such build, or Reservoir
out of reach, it is `none`: the project gets no lean-smt (no require, no
`import Smt`, no `example`) and a warning says "lean-smt has no build for
TOOLCHAIN; add it later with fisher project add smt". `--var smt=main` (or
a commit) skips Reservoir, and `--var smt=none` leaves lean-smt out.

For a quick try with no project of your own, make a scratch environment:

```
fisher scratch --statement "∀ n : ℕ, n * 1 = n"            # Scratch.lean with theorem scratch_goal
fisher scratch --template lean Mine.lean                     # a copy of Mine.lean, next to the sample
fisher scratch --list                                        # scratches and bases: age, size, a base's releases
fisher scratch --prune 7 [--bases]                           # remove those older than 7 days
```

It prints the scratch's root (`~/.cache/fisher/scratch/<date>-<time>-<hex>`)
and the file to work on. The first scratch for a template and its
variables builds a base under `~/.cache/fisher/scratch/bases/` with the
normal create (steps and sample check), which takes as long as a create;
after that each scratch is a copy-on-write clone of the base (`cp -c` on
macOS, `cp --reflink=auto` on Linux, file times kept so Lake does not
rebuild) and takes under a minute. Two first scratches at once build the base
once; later scratches from one base clone at the same time. `--statement`
takes the statement only (no `theorem NAME :`). A base whose build fails is removed. The
template defaults to `lean-mathlib`; `--var`, `--yes` and `--trust-revision` work as for
`fisher project create`. Bases are kept by `--prune` unless `--bases` is
given. A scratch's age counts from the last change to it or its work
file; a base's from its last clone.

What to expect (an M-series Mac, Lean v4.35.0, 20 Oct 2026): `lean` is
ready in about 3 minutes and takes 100 KB; `lean-mathlib` in about 8
minutes and 7.2 GB (most of it the Mathlib cache). The first scratch built
its base in about 5 minutes and the second, a clone of it, took 45 s; `fisher search` on the scratch's
`scratch_goal` (`∀ n : ℕ, n * 1 = n`) closed it in the pre-pass in 11 s.
`--list` adds up each clone's full size, so copy-on-write clones look
larger on disk than they are.

Rocq and Isabelle projects work the same way for `/prove` and `/draft`;
see `docs/provers.md`.

## 4. Prove one theorem

Open the shell and run a workflow on a file:

```
fisher
> /prove Math/Foo.lean
```

The workflow runs in the swarm; `/swarm` lists runs, Ctrl-] detaches from
one, plain text talks to the agent. Or run it straight from the terminal,
attached:

```
fisher launch /prove Math/Foo.lean
fisher plan /prove Math/Foo.lean   # see what it would run, without running it
```

Other workflows: `/draft`, `/review`, `/autoformalize`.

## 5. Close every `sorry` in a file

`fisher search` works the goals of one file in parallel attempts, checks
every result with the kernel, and merges what closed:

```
fisher search Math/Foo.lean --backends fisher --model anthropic/claude-sonnet-5.5
fisher search Math/Foo.lean --backends fisher --apply          # write the merged file back
fisher search Math/Foo.lean --graph --cap-usd 10               # split hard goals into lemmas
```

Useful flags: `--parallel 4`, `--max-attempts 3`, `--attempt-minutes 20`,
`--budget 2h`, `--out DIR`. Before any model call a pre-pass tries a
tactic portfolio (`--no-auto` turns it off); each brief carries the project
facts the goal is likely to need (`--no-retrieval`) and lemmas proved in
earlier runs (`--no-library`). The run directory has `report.json`,
`events.jsonl` and the merged file; a wrong flag prints the full usage.

Model tiers (`--tiers CHEAP,anthropic/claude-sonnet-5.5`) start each goal
on a cheap model and move up after a failed or stalled attempt. On TWISTED
this cost more than Sonnet alone (bench K, `tools/bench/k/README.md`), so
leave it off unless your goals are easier.

Fisher logs every run (`fisher runs`, section 6) and `fisher search` uses
the log: per project and goal shape it can put the backends, tiers and
pre-pass tactics in the order that has closed goals most cheaply, and
shorten an attempt's time limit. It changes an order only with at least 5
attempts of evidence, never raises a budget or cap, and a flag you give
(`--backends`, `--tiers`, `--auto-tactics`, `--attempt-minutes`) wins. Each
change is in the run's `events.jsonl`. `--no-learn` ignores it for a run,
and `fisher.learn.enabled: false` turns it off. See `docs/learn.md`.

In a Lean project with lean-smt (`fisher project add smt`) the pre-pass also
tries the `smt` tactic, and a solver on PATH adds countermodels; see
`docs/smt.md`.

## 6. Look back

```
fisher sessions list               # every agent session
fisher sessions export out.jsonl   # sessions to a file
fisher replay ID                   # run a logged session again and compare
fisher chat -q "explain the last error" --continue
```

Every run of every backend is also in one log:

```
fisher runs list --since 7d --kind search-attempt   # newest first
fisher runs show ID                  # a run, its goals, its first events
fisher runs export out.json          # runs with goals and events
fisher runs import ~/.cache/fisher/search   # earlier searches, once
fisher runs import --bench DIR       # a bench's run directories
fisher learn stats                   # what the log says about each arm
fisher learn show                    # the policy `fisher search` applies
fisher learn diff                    # what a recompute would change
fisher learn reset                   # forget the learned policy
```

`fisher runs import-sessions` reads the native agent's earlier chats.

Fisher also mines the log for mistakes agents repeat and shows the ones that
keep recurring to every backend at the start of a run:

```
fisher lessons list [--all]          # active, pinned and proposed lessons; --all adds candidates
fisher lessons show ID               # one lesson with its evidence
fisher lessons pin ID                # keep it (also unpin, drop)
fisher lessons mine --since 30d      # mine the log now; --again redoes mined attempts
fisher lessons reset                 # delete the project's lessons
```

Lessons live in `.fisher/lessons.md`; edit only under `## Yours`. A rule's
lesson is added on its evidence, but one a model wrote (judged or self) is
only proposed until you approve it with `fisher lessons pin ID`. Turn them
off for one search with `--no-lessons`, or always with
`fisher.lessons.enabled: false`.
`docs/learn.md` has the tables, the rules and the bounds.

## When something fails

- `fisher doctor --fix` repairs what it can and says what it cannot.
- `fisher status --all` shows providers, keys (last 4 characters) and
  backends.
- `fisher config show` prints the settings in effect; `fisher config path`
  says which file they came from.
