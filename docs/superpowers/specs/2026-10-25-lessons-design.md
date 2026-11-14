# Lessons: mined mistakes, a judge, and a per-project lessons file

Date: 25 Oct 2026. Status: approved by Tom in conversation. Second stage on the run log (the first was `2026-10-24-run-log-and-learned-policy-design.md`). (Restored on 14 Nov 2026 from the session that wrote it; the original commit lived only in a cloud mirror.)

## 1. What and why

The run log holds every agent action and every kernel verdict, but nothing reads the transcripts for the mistakes agents keep making: a lemma name that does not exist in this project, Lean 3 syntax, a proof handed in without a check, a loop of timing-out calls. This adds a loop that finds those mistakes, turns them into short rules, shows the rules to every backend at the start of a run, and keeps only the rules that change the outcome.

Decided with Tom (25 Oct 2026):

- Lessons land in a per-project file, `.fisher/lessons.md`, plus a global one in the Fisher home; the Lean skill (which Fisher fetches and does not own) is not edited.
- Mining is rules first, a judge model on the residue, under dollar caps.
- Self-critique is one extra field in the same judge call, feeding the same candidate pipeline.
- Promotion by count (3 distinct runs; judge lessons also need two agreements), retirement by measurement (no change in the mistake's rate after 10 runs in force), a cap of 12 active lines per file, `fisher lessons` to list, pin, drop.
- The miner is an offline pass over the run log, run at the end of each run and by command; the agent loop is not changed.

## 2. Mistakes

`Fisher.Learn.Mistakes` (fisher-search): a closed set of kinds, each a pure classifier over one attempt's events (the `tool_start`/`tool_end`/`message` events of the run log) and its outcome text.

| Kind | Found by |
| --- | --- |
| `unknown_identifier` | a Lean diagnostic `unknown identifier X` or `unknown constant X` in a `tool_end` result of a Lean tool; carries X |
| `deprecated` | `has been deprecated` (with the replacement name when given); carries the old and new names |
| `lean3_syntax` | diagnostics or edits with Lean 3 markers: `by {`, `λ x,`, `cases'`-free `cases h with`, `assume`, `obtain ⟨⟩ := ` with Lean 3 forms, `#check` of a Lean 3 name; carries the marker |
| `sorry_left` | outcome `rejected: still sorry` or `rejected: the proof uses sorry` |
| `statement_changed` | outcome `rejected: the statement elaborates to a different type` |
| `axiom_used` | outcome `rejected: axioms beyond the allowed`; carries the axiom |
| `timeout_loop` | 3 or more consecutive `lean_run_code` / `lean_multi_attempt` calls whose result says timed out |
| `repeated_call` | the same tool with the same args 3 times in one attempt |
| `no_check_before_finish` | no `lean_diagnostic_messages`, `lean_verify` or `lean_build` call after the attempt's last `write_file`/`patch` |
| `budget_burn` | class `stalled` or `timeout` with no file written |

Tool names are matched by suffix (`lean_run_code`), since the native agent and Claude Code prefix them differently. A hit is `Mistake { kind, goal, shape, name :: Maybe Text, seq :: Int }`.

Each kind has a lesson template in `fisher-core/data/lessons/templates.yaml` (embedded, user-editable by copying to `$FISHER_HOME/fisher/lessons-templates.yaml`, which wins when present), with `{name}`, `{new}` placeholders. Example: `unknown_identifier`: "In this project `{name}` does not exist. Search with `lean_loogle` or `exact?` before naming a lemma."

The classifiers run at the end of every search run and every imported launch run, over that run's attempts, and write a `mistake` event `{kind, name, seq, goal, shape}` on the attempt run. `fisher lessons mine [--since D] [--project P]` runs them over history (idempotent: an attempt already carrying `mistake` events is skipped unless `--again`).

## 3. The judge and the self-critique

- **Residue:** a failed attempt (class failed, timeout, stalled) with no rule hit.
- **One call per residue attempt**, on the route the graph planner uses (`liveModelFor` and the configured provider; the key stays in the existing path). Input: the attempt's last 40 events (messages and tool names; tool results cut to 500 characters; the data is already redacted) and the outcome text. Output JSON: `{mistake: one line, lesson: one or two imperative sentences, confidence: 0 to 1, tell_yourself: two sentences to read before starting this goal again}`. Confidence under 0.6 drops the `lesson`; `tell_yourself` is always a candidate with evidence 1. Bad JSON: one retry with a correction message, then skip.
- **Rule-explained failures** get no judge call.
- **Caps:** `fisher.lessons.judge_cap_usd` per run (default 0.25) and `fisher.lessons.judge_day_cap_usd` per day (default 2.00), spend recorded in a `lessons_spend` table (day, dollars). At a cap the remaining residue is left unjudged and the run's report says how many. `fisher.lessons.judge: false` keeps the rules and drops the calls.
- **Output:** `mistake` events with kind `judged` (text, confidence) and `self` (the tell_yourself text), and candidates (section 4).
- **Dedup key:** rule lessons key on `kind:name`; judge and self lessons key on the normalised text (lowercase, punctuation removed, Lean identifiers kept) with a Levenshtein distance of at most 8 on texts up to 120 characters counting as the same, or the same Lean identifier mentioned.

## 4. Candidates, promotion, retirement

A `lessons` table in `runs.db`: `id`, `project_key` (`*` for global), `kind`, `key`, `text`, `status` (candidate, active, pinned, retired, dropped), `evidence_runs` (JSON list of run ids), `first_seen`, `activated_at`, `retired_at`, `runs_active` (JSON list), `hits_while_active`, `hits_before` (the mistake's count in the 10 runs before activation), `attempts_before`, `attempts_while_active`.

- **Promotion:** a candidate becomes active when `evidence_runs` holds 3 distinct runs; a `judged` or `self` candidate also needs 2 judge calls that produced the same key. Promotion happens in the mining pass. A kind with no name seen in 3 projects (for example `no_check_before_finish`) becomes a global lesson; a named one stays per project.
- **In force:** each run writes a `lessons` event `{ids, project_hash, global_hash}` for the lines it showed, and the mining pass appends the run id to `runs_active` of each.
- **Retirement:** after 10 runs in force, if the mistake's rate per attempt in those runs is at least 0.8 times the rate in the 10 runs before activation, the lesson retires; a lower rate keeps it. The comparison needs at least 5 runs before activation; with fewer, the lesson is not retired (it stays active until the cap or a drop removes it). Pinned never retires. A retired or dropped key is not re-proposed unless `fisher lessons reset`.
- **Cap:** at most `fisher.lessons.max` (default 12) active plus pinned lines per file; when a promotion would exceed it, the active line with the oldest latest evidence goes back to candidate.

## 5. The file and the prompt

- `.fisher/lessons.md` (project) and `$FISHER_HOME/fisher/lessons.md` (global), regenerated after each mining pass: a first line `<!-- fisher lessons: generated, edit only under "## Yours" -->`, then a numbered list of active and pinned lessons (oldest first), then a `## Yours` heading whose lines are kept verbatim across regenerations. A file whose first line is not that header is left alone and the pass warns once.
- Delivery: when either file has lines, a block headed `Lessons from earlier runs on this project` (the project lines, then the global lines, then the `## Yours` lines) is appended to the startup prompt of every backend: `fisherSystemPrompt` (native), the Claude Code startup-context file, the Codex instructions file, and so the search attempts (they reuse those). Nothing else about the prompts changes. The run row records the lean4 skill revision (`bRevision`) and the `lessons` event records the file hashes.

## 6. Commands and the web app

`fisher lessons list [--all] [--project P]` (active, pinned, candidates with counts), `fisher lessons show ID`, `fisher lessons pin|unpin|drop ID`, `fisher lessons mine [--since D] [--project P] [--again]`, `fisher lessons reset [--project P]`. `fisher search` gains `--no-lessons`; `fisher.lessons.enabled: false` turns the stage off (no mining, no block). The Learn page gets a Lessons panel: the same list and the pin, unpin, drop actions; `/api/lessons` (GET, POST actions).

## 7. Errors

Mining never blocks a run: it runs after the run's rows are written, inside the warn-once wrapper; a failure leaves the run logged and the files unchanged. A judge failure is retried once then skipped. An unreadable lessons file gives an empty block and one warning. The file generator writes atomically.

## 8. Testing

Per classifier a fixture transcript and a negative; the judge prompt builder and parser (good, bad, low confidence, retry); dedup (same identifier, near text, different lemma); promotion and retirement on hand-built histories (3 runs promote, 2 do not, judge needs two agreements, 10 runs at the same rate retire, a fall keeps, pinned never retires, the cap pushes the oldest back); the file generator and parser (`## Yours` survives, a broken header is left alone); prompt assembly for the three backends (the block exactly once, only when non-empty); the spend caps; the commands with a fake env; a route test and a Playwright test for the panel. Live on the Mac: mine the existing history; two searches on a scratch goal with a planted unknown identifier, the mistake logged and the candidate made; after the third, the lesson in the prompt.

## 9. Not in this version

Edits to the skill or the system prompt text; per-shape lesson selection; Rocq and Isabelle classifiers (the delivery path would carry their lessons, the templates would not fit); fine-tuning.

## 10. As built (added 14 Nov 2026)

After approval Tom asked for the injection problem to be fixed: judged and self lessons no longer auto-promote (they are proposed after 3 runs and 2 agreements and need `fisher lessons pin`; `fisher.lessons.auto_judged: true` restores the old rule); judged and self keys live in their own namespace (`judged:`, `self:`) so a model's text can never merge into a rule row or reach a global `*` row; the prompt block is built from the rows in runs.db through a read-only connection, with only the `## Yours` lines read from the files and every line passed through the sanitiser; the Fisher home's files are on the agent's file-tool deny list and in the staged Claude Code deny rules. Details in `docs/learn.md` and the project note on lessons.
