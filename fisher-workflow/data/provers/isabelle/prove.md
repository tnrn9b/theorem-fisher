---
description: Prove the holes in an Isabelle theory or in one of its lemmas
argument-hint: "[FILE[:NAME]]"
---

# /isabelle:prove

Arguments: `$ARGUMENTS`. A theory file, or `FILE:NAME` for one lemma. If
they are empty or were not filled in, use the scope given with the request;
if there is none, take every `.thy` file of the project that has holes.

Read the `isabelle` skill first: it names the tools and the rules.

Every step below is required, in this order. Work through the `isabelle`
MCP server: a proof written without checking it through the server is not
done, whatever it looks like. Evaluate after every edit, before the next
one.

1. Launch the prover with `mcp_isabelle_isabelle_launch` on the project's
   base session.
2. List the holes in scope. Search the files for `sorry` and `oops` outside
   comments, strings, cartouches and `text` blocks, and note the lemma
   around each. Write the list down: file, line, lemma.
3. Take the holes one at a time, in file order. For each one:
   1. Evaluate the file up to the hole with
      `mcp_isabelle_isabelle_evaluate_to`, and poll
      `mcp_isabelle_isabelle_evaluation_status` until it is done.
   2. Read the subgoals there with `mcp_isabelle_isabelle_goal`.
   3. Find the facts you need with `mcp_isabelle_isabelle_find_theorems`.
   4. Replace the hole with a proof in the file.
   5. Evaluate the file again with `mcp_isabelle_isabelle_evaluate_to`,
      poll `mcp_isabelle_isabelle_evaluation_status`, and read the result
      with `mcp_isabelle_isabelle_command_output` at the proof. Go on only
      when it has no error; if it has one, fix the proof or put the hole
      back first.
   6. Run `fisher check FILE:NAME` on the lemma only if that is cheap (a
      small session); each run rebuilds the session.
4. If a hole will not close after a real effort, leave it as it was, note
   what you tried, and go on to the next one.
5. Run `fisher check` once, in the terminal, on every lemma you worked on:
   `fisher check FILE:a FILE:b ...` in one call, not one call per lemma
   (or `fisher check FILE` for a whole theory).
6. Report the holes closed (file, line, lemma), the holes left with the
   reason for each, and what `fisher check` printed.
