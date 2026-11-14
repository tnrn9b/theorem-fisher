# The run log and the learned search policy (reconstructed)

Date: 24 Oct 2026, approved by Tom in conversation. This file is a reconstruction written on 14 Nov 2026 from the project note on the run log and from the code as shipped; the original spec was committed only in a cloud mirror that no longer exists. Where this page and the code disagree, the code is the reference (`docs/learn.md` describes it).

## 1. What and why

Fisher ran agents through several backends and kept their output in separate places (the native agent's session log, the UI ledger, search run directories, bench reports), so nothing could learn from all of them. This stage gives every run one log and uses it for the first improvement: ordering the search's arms by what has worked.

Decided with Tom:

- Stage the self-improving harness as logging plus the search policy first; prompt and skill mining and agent self-critique later, on the same log (they became the lessons stage).
- The learned policy is applied, with a report of every change it made; never silently.
- The store is SQLite in the Fisher home: `$FISHER_HOME/fisher/runs.db`.
- A goal's features are the project plus the statement's shape.
- Headless runs are captured at the source; interactive Claude Code and Codex runs are imported from their session files.

## 2. The run log

`Fisher.Runlog` (fisher-core): tables `runs`, `goals`, `events`, `policy_changes`, WAL journal, a corrupt file moved aside and recreated. Redaction by key name (`api_key`, `token`, `password`, `authorization`, `*_KEY`, `*_TOKEN`) and by pattern (`Fisher.Redact`, moved into fisher-core); tool results and arguments capped at 64 KB, an open private-key block at the cut dropped.

Writers: the native agent through its UI hooks (always on, the UI ledger stays alongside); cron; `fisher search` (a parent run, one run per attempt with an `attempt` event `{goal, shape, backend, budget_secs}`, goal rows carrying shapes, every search event on the parent, a `portfolio` event naming the pre-pass tactics); the web app's native children (kind `ui-run`); `Fisher.Swarm.Import` for interactive Claude Code and Codex runs from the session files under the managed home.

`Fisher.Search.Shape`: the shape key is `connective | up to three Mathlib-style heads | namespaces | size`, for example `eq|||small`.

## 3. The learned policy

`Fisher.Learn.Stats` and `Fisher.Learn.Policy`: per project and shape, each arm (backend, tier, pre-pass tactic) gets a close rate with Beta smoothing whose prior is centred on the next coarser level (shape, then project, then the global shape, then global); cost per close is dollars per attempt over the smoothed rate. An order changes only with at least 5 attempts of evidence and a 0.1 gap in rate; the attempt budget is the 80th percentile of seconds to close, clamped to [half the configured, the configured]; exploration runs every eighth attempt per shape; the policy never raises a budget or a cap and never adds an arm that is not configured.

`fisher search` computes and applies the policy per goal (backends, tiers, tactics, attempt seconds); a flag given on the command line wins over its knob; `--no-learn` ignores the policy for one run and `fisher.learn.enabled: false` turns it off; each change is a `policy` event and the report gets a `learn` block.

## 4. Commands and the web app

`fisher runs list|show|export|import [--bench]|import-sessions`, `fisher learn stats|show|diff|reset`, web pages `/runlog` and `/learn`, `docs/learn.md`, quickstart sections, a `H_LEARN` switch on bench H for the A/B.

## 5. Testing

Unit tests on the store (schema, redaction, caps, corrupt file), the shape key, the statistics and the policy (smoothing levels, the 5-attempt and 0.1 thresholds, the budget clamp, exploration cadence, never raising a budget), the writers (search events and attempt rows, the importers on fixture session files), the commands with a fake home, route tests for the two pages. Live on the Mac: imports of the bench H runs and the chat sessions, a search with the policy on, the pages on real data.
