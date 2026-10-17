---
name: rocq
description: Work on Rocq proofs in a Fisher project through the rocq MCP server. Covers the holes to look for, the tools and when to use each, the rules for edits, and the final check with fisher check. Read it before /rocq:prove or /rocq:draft.
---

# Rocq in a Fisher project

You work unattended in a Rocq project that Fisher manages. Nobody answers
questions while you work: decide, act, and say what you did in the final
report.

## Holes

A hole is an unfinished proof: `Admitted.`, `admit` or `give_up`, outside
comments and strings. Comments are `(* ... *)` and nest; strings are
`"..."`, with `""` for a quote inside one.

## Tools

The tools come from the `rocq` MCP server. This text names them
`mcp_rocq_rocq_<tool>`; your host may spell the prefix another way (for
example `mcp__rocq__rocq_compile`). Go by the part after the server name.

Interactive tools (they need coq-lsp's `pet`):

- `mcp_rocq_rocq_start`: open a proof session on a theorem in a file; it
  returns the goals and a state id.
- `mcp_rocq_rocq_check`: run proof commands from a state; on an error it
  returns the last good state.
- `mcp_rocq_rocq_step_multi`: try several tactics on one state at once,
  without advancing it.
- `mcp_rocq_rocq_query`: `Search`, `Check`, `Print`, `About` and other
  queries.
- `mcp_rocq_rocq_assumptions`: the axioms a theorem depends on.
- `mcp_rocq_rocq_toc`: the outline of a file (its definitions, lemmas and
  sections).
- `mcp_rocq_rocq_notations`: how the notations in a statement resolve.

Compile and verify tools (they need only `coqc`):

- `mcp_rocq_rocq_compile_file`: compile a file on disk. This is the compile
  tool: use it after every edit.
- `mcp_rocq_rocq_compile`: compile source text.
- `mcp_rocq_rocq_verify`: check, in a sandbox, that a proof proves a given
  statement.
- `mcp_rocq_rocq_health` and `mcp_rocq_rocq_diag`: the toolchain the server
  runs and its state.

Call `mcp_rocq_rocq_health` once at the start. If it reports no `pet`, or
the interactive tools fail, use only the compile and verify tools: edit the
file, compile it, and let the errors guide the next attempt.

Never call `rocq_switch`, even if it is listed. It moves the running server
to another opam switch and throws away every proof state. Do not change the
opam switch or the build setup in any other way either.

## Rules

- Keep each theorem's name, arguments and statement as they are, unless the
  task asks for a change. A proof of another statement is not a proof.
- Do not close a goal with `Axiom`, `Parameter`, `Admitted`, `admit` or
  `give_up`, and do not remove a theorem with `Abort`.
- Keep edits small and local. The file must compile after each one.
- When a goal cannot be proved as stated, leave the hole and say why in the
  report. Do not weaken the statement.

## The check

`fisher check FILE[:NAME]`, run in the terminal, compiles the project with
its own flags and prints the assumptions of each target. A target passes
only when it exists and is closed under the global context. A proof is done
when this check passes, and not before. Give it every target in one call
(`fisher check FILE:a FILE:b ...`). When the startup context names a
Fisher check command, run that one.
