---
name: isabelle
description: Work on Isabelle proofs in a Fisher project through the isabelle MCP server. Covers the holes to look for, the tools and when to use each, the rules for edits, and the final check with fisher check. Read it before /isabelle:prove or /isabelle:draft.
---

# Isabelle in a Fisher project

You work unattended in an Isabelle project that Fisher manages. Nobody
answers questions while you work: decide, act, and say what you did in the
final report.

## Holes

A hole is an unfinished proof: `sorry`, or `oops` (which drops the proof).
Neither counts inside comments (`(* ... *)`, which nest, and `\<comment>`),
strings, cartouches (`\<open> ... \<close>`) or `text` and `txt` blocks.

## Tools

The tools come from the `isabelle` MCP server. This text names them
`mcp_isabelle_isabelle_<tool>`; your host may spell the prefix another way
(for example `mcp__isabelle__isabelle_goal`). Go by the part after the
server name.

Starting and evaluating:

- `mcp_isabelle_isabelle_launch`: start the prover. Call it first, once.
  Give it the project's base session (the parent in the project's `ROOT`
  entry, `session NAME = BASE + ...`), not the project's own session, whose
  theories could not then be edited. If it says the session is not built,
  run the `isabelle build -b ...` command it prints, then launch again.
- `mcp_isabelle_isabelle_evaluate_to`: evaluate a theory file up to a line
  (`-1` for the end). This is the compile tool: use it after every edit.
- `mcp_isabelle_isabelle_evaluation_status`: how far evaluation has got,
  and its errors. Poll it until evaluation is done.
- `mcp_isabelle_isabelle_cancel_evaluation`: stop an evaluation that is
  piling up errors.
- `mcp_isabelle_isabelle_terminate`: stop the prover, for a fresh launch.
- `mcp_isabelle_isabelle_session_info`: the running session.

Reading the evaluated text (evaluate up to the line first):

- `mcp_isabelle_isabelle_goal`: the command at a position and the subgoals
  after it.
- `mcp_isabelle_isabelle_command_output`: the messages a command produced.
- `mcp_isabelle_isabelle_command_status`: whether commands are processed,
  running or failed.
- `mcp_isabelle_isabelle_find_theorems`: search for theorems by name,
  pattern, or use as intro, elim, dest or simp rule.
- `mcp_isabelle_isabelle_hover`, `mcp_isabelle_isabelle_definition` and
  `mcp_isabelle_isabelle_local_occurrences`: the type, the definition and
  the uses of a symbol.

Never use the ML debugger: do not launch with `debug` set, and do not call
the breakpoint or debugger tools (`isabelle_set_breakpoint`,
`isabelle_debug_state`, `isabelle_step_at_breakpoint` and the rest), even if
they are listed. One server holds one prover; do not start another.

## Rules

- Keep each lemma's name, assumptions and conclusion as they are, unless
  the task asks for a change. A proof of another statement is not a proof.
- Do not close a goal with `sorry`, `oops`, a new `axiomatization`, or an
  oracle, and do not turn on `quick_and_dirty`.
- Keep edits small and local. The theory must evaluate without errors after
  each one, apart from the holes you have not reached.
- Prefer structured Isar proofs with named steps for anything longer than
  one method call; try `by auto`, `by simp`, `by blast` and
  `by (metis ...)` first on small goals.
- When a goal cannot be proved as stated, leave the hole and say why in the
  report. Do not weaken the statement.

## The check

`fisher check FILE[:NAME]`, run in the terminal, builds the session with
`quick_and_dirty` off and lists the oracles of each target. A target passes
only when it exists and has no oracles. A proof is done when this check
passes, and not before. Give it every target in one call
(`fisher check FILE:a FILE:b ...`). When the startup context names a
Fisher check command, run that one.
