---
description: Prove the holes in a Rocq file or in one of its declarations
argument-hint: "[FILE[:NAME]]"
---

# /rocq:prove

Arguments: `$ARGUMENTS`. A file, or `FILE:NAME` for one declaration. If
they are empty or were not filled in, use the scope given with the request;
if there is none, take every `.v` file of the project that has holes.

Read the `rocq` skill first: it names the tools and the rules.

Every step below is required, in this order. Work through the `rocq` MCP
server: a proof written without checking it through the server is not
done, whatever it looks like. Compile after every edit, before the next
one.

1. Call `mcp_rocq_rocq_health` once. If it reports no `pet`, or an
   interactive tool fails, follow the steps for a session without `pet`
   below.
2. List the holes in scope. Search the files for `Admitted.`, `admit` and
   `give_up` outside comments and strings, and find the declaration around
   each with `mcp_rocq_rocq_toc`. Write the list down: file, line,
   declaration.
3. Take the holes one at a time, in file order. For each one:
   1. Open it with `mcp_rocq_rocq_start` (the file and the theorem) and read
      the goals.
   2. Find the lemmas you need with `mcp_rocq_rocq_query` (`Search`,
      `Check`, `About`).
   3. Try tactics with `mcp_rocq_rocq_step_multi`, and advance with
      `mcp_rocq_rocq_check` until no goals are left.
   4. Write the proof into the file in place of the hole, ending it with
      `Qed.`
   5. Compile the file with `mcp_rocq_rocq_compile_file`. Go on only when it
      compiles; if it does not, fix the proof or put the hole back first.
   6. Run `fisher check FILE:NAME` on the declaration only if that is
      cheap (a small project); each run compiles the project again.
4. If a hole will not close after a real effort, leave it as it was, note
   what you tried, and go on to the next one.
5. Run `fisher check` once, in the terminal, on every declaration you
   worked on: `fisher check FILE:a FILE:b ...` in one call, not one call
   per declaration (or `fisher check FILE` for a whole file).
6. Report the holes closed (file, line, declaration), the holes left with
   the reason for each, and what `fisher check` printed.

Without `pet`, the interactive tools do not work, `mcp_rocq_rocq_toc` and
`mcp_rocq_rocq_query` included. The steps stay required, with these
changes:

- In step 2, read the file instead to find the declaration around each
  hole.
- In step 3, replace 3.1 to 3.3 with: to look a lemma up, put `Search`,
  `Check` or `About` commands in a scratch text after the file's imports
  and compile it with `mcp_rocq_rocq_compile`; the answers are in its
  messages. Then write a proof into the file (3.4), compile it with
  `mcp_rocq_rocq_compile_file` (3.5), and use the errors to improve it.
