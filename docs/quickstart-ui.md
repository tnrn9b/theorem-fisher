# Quickstart: the Fisher web app

`fisher ui` is a local web app for watching, steering and auditing Fisher's
proof work. It reads the same home, sessions and runs as the terminal
commands, so anything started either way shows up in both. The full
reference is `docs/ui.md`; the terminal quickstart is `docs/quickstart.md`.

## 1. Before you start

Do steps 1 to 3 of `docs/quickstart.md` once: build and install `fisher`,
give it a key, and run `fisher project init` in your project. The web app
needs nothing else; Node is only needed to change the page itself.

Tell it where the Fisher checkout is, so the Runs view can compute benchmark
gate lines:

```
fisher config set fisher.repo ~/IdeaProjects/Fisher
```

## 2. Start it

```
fisher ui                  # port 8732, opens the browser
fisher ui --port 0         # any free port
fisher ui --no-open        # print the URL only
```

It prints `Fisher UI: http://127.0.0.1:8732/?t=<token>`. Open that exact
URL: the token is new on every start and becomes a cookie for this browser.
The server listens on 127.0.0.1 only. Ctrl-C stops it and every run it
started.

## 3. Prove something

1. **Sessions** (`/sessions`) is the home page: every agent session, with
   filters for project, prover, model and context strategy.
2. In **Start a run** at the top, give the project folder and a command
   such as `/prove Math/Foo.lean`, and press **Start**.
3. The **live view** opens: each tool call, message and prover result as it
   happens. **Send** puts a message in front of the agent at its next tool
   call; **Stop** ends the run.

## 3a. Make a new Lean project

1. On **Sessions**, follow **New project** (`/new`).
2. Pick a template (`lean-mathlib` by default, `lean`, or one named under
   `fisher.templates` in `config.yaml`). Give an absolute folder that does
   not exist yet or is empty, or choose **Scratch** (no folder: a copy of
   a base that is built once per template and release). Fill in any
   variable you want other than its default; a value that does not match
   the variable's pattern is flagged as you type.
3. Press **Create**. A template of your own that is not trusted yet first
   shows the steps it will run; press **Confirm and create** to run them.
4. Each step (the toolchain, `lake exe cache get`, `lake build`) shows as
   it runs, then the sample check. **Stop** ends it; after a failure,
   **Retry** runs `fisher project build`, which picks up at the step that
   failed.
5. At **Ready**, **Start a run** and **Start a search** open those forms
   with the new project and its sample filled in.

## 4. Audit or fix a proof

Open any session from the list:

- The transcript is on the left, the proof file on the right, with a
  timeline of every file change.
- Click a step to see the file as that step left it, and press **Check this
  step** for its errors and holes left. Pick a **From step** and a **To
  step** and press **Show the diff** to compare two.
- Type into the file on the right and press **Hand back**: the file is
  saved, checked, and the agent is told what changed. **Discard the edit**
  throws your change away.

## 5. Close every `sorry` in a file

1. Open the **goal board** (`/search`). In **Start a search**, give a Lean
   file, pick flat or graph, switch the pre-pass, project facts or lemma
   library off if you want, and press **Start search**.
2. Each goal shows its status, its attempts and what closed it. Graph runs
   also draw the lemma graph and each root's notes. **Stop** ends the run,
   **Run again** repeats it with the same choices, and **Retry** on a goal
   searches again.
3. When it is done, **Apply to <file>** writes the merged file into the
   project. If the file changed since the run started it asks first
   (**Apply anyway** or **Cancel**).

Runs started from the terminal (`fisher search ...`) appear here too, and
running ones update as they go.

## 6. Compare and configure

- **Runs** (`/runs`) shows the benchmark results under
  `~/.cache/fisher/bench`, two arms side by side, with each bench's gate
  line.
- **Run log** (`/runlog`) lists every run Fisher logged, newest first,
  with filters for kind and project. Open a run for its row, its goals
  (with their shape and what closed them) and its events. It reads the
  same log as `fisher runs list`; if it is empty, `fisher runs import`
  reads earlier searches.
- **Learn** (`/learn`) shows, for a project, the statistics of each
  backend, model tier and pre-pass tactic (attempts, closes, close rate,
  dollars per close), the order the search now uses for each shape, and a
  history of the changes. It is what `fisher learn stats` and
  `fisher learn show` print. It only shows; use `fisher learn reset` or
  `fisher search --no-learn` to stop using it. Below it, the **Lessons**
  panel lists the project's mined lessons and the global ones (status,
  runs seen, text) with **Pin**, **Unpin** and **Drop**, as
  `fisher lessons list --all` and its actions do.
- **Context** (`/context/<session>`) shows what the session's context
  strategy kept.
- **Settings** (`/settings`) sets the provider, model, autoformalize model
  and context strategy, and the keys in `.env`. Keys are shown only as their
  last 4 characters. **Save** writes only what changed; runs started after
  a save use the new values.
  Its **Claude Code** panel shows whether the Claude Code CLI is logged in
  (and as whom), and logs it in or out: choose Subscription or Anthropic
  Console, tick Force SSO if needed, and press **Log in**. The page shows
  what `claude auth login` prints, with the URL to open as a link; if the
  browser gives a code, paste it in the code box and press **Send code**.
  **Stop** ends a login, **Log out** runs `claude auth logout`. Claude Code
  keeps the token; Fisher never sees it.

## If the page says 401 or 403

Use the URL from the latest start: an old token is refused. Open it as
`127.0.0.1` or `localhost`, not another name for the machine.
