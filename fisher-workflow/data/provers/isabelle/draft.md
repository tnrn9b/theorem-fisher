---
description: Turn a statement in prose into an Isabelle statement and a proof skeleton with holes
argument-hint: "TOPIC [FILE]"
---

# /isabelle:draft

Arguments: `$ARGUMENTS`. The statement to formalize, in prose, and
optionally the `.thy` file to write. If they are empty or were not filled
in, use the statement given with the request.

Read the `isabelle` skill first: it names the tools and the rules.

Every step below is required, in this order. Work through the `isabelle`
MCP server: a draft written without checking it through the server is not
done; it is done when the theory evaluates with its holes in place and no
error. Evaluate after every edit, before the next one.

1. Choose the file. If a `.thy` file is named, write there, adding to what
   is in it. Otherwise create a new theory in the project's directory,
   named after the topic, whose name matches the file name, importing what
   the neighbouring theories import. Add it to the `theories` of the
   project's session in `ROOT`; that is the only change to the build setup
   allowed here.
2. Launch the prover with `mcp_isabelle_isabelle_launch` on the project's
   base session, and look at what already exists with
   `mcp_isabelle_isabelle_find_theorems` and
   `mcp_isabelle_isabelle_definition`. Use existing definitions instead of
   new copies of them.
3. Write the definitions you need and each claim as a `lemma` or `theorem`
   with a proof skeleton: `sorry`, or, when the proof plan is clear, an Isar
   outline (`proof ... qed` with `have` and `show` steps) whose steps end in
   `sorry`.
4. Check that the theory parses and typechecks with the holes in place:
   evaluate it to the end with `mcp_isabelle_isabelle_evaluate_to` (line
   `-1`), poll `mcp_isabelle_isabelle_evaluation_status`, read any error
   with `mcp_isabelle_isabelle_command_output`, and fix every error. Go on
   only when the theory evaluates with no error.
5. Read each statement against the prose once more: quantifiers,
   assumptions, the number types (`nat`, `int`, `real`) and the edge cases.
   After any change, evaluate again as in step 4.
6. Report the file, each statement made with its name, and how the parts of
   the prose map to it. Do not prove the holes here; that is
   `/isabelle:prove`.
