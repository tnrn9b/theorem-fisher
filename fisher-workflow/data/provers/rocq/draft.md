---
description: Turn a statement in prose into a Rocq statement and a proof skeleton with holes
argument-hint: "TOPIC [FILE]"
---

# /rocq:draft

Arguments: `$ARGUMENTS`. The statement to formalize, in prose, and
optionally the `.v` file to write. If they are empty or were not filled in,
use the statement given with the request.

Read the `rocq` skill first: it names the tools and the rules.

Every step below is required, in this order. Work through the `rocq` MCP
server: a draft written without checking it through the server is not
done; it is done when the file compiles with its holes in place. Compile
after every edit, before the next one.

1. Call `mcp_rocq_rocq_health` once. Without `pet`, `mcp_rocq_rocq_toc` and
   `mcp_rocq_rocq_query` do not work: in step 3 read the nearby files
   instead, and put the queries in a scratch text after the file's imports,
   compiled with `mcp_rocq_rocq_compile`.
2. Choose the file. If a `.v` file is named, write there, adding to what is
   in it. Otherwise create a new file in the project's source directory,
   named after the topic. If the project file (`_RocqProject` or
   `_CoqProject`) lists its source files one by one, add the new file to it;
   that is the only change to the build setup allowed here.
3. Look at what the project and the standard library already have, with
   `mcp_rocq_rocq_toc` on nearby files and `mcp_rocq_rocq_query`
   (`Search`, `Locate`, `About`). Use existing definitions instead of new
   copies of them.
4. Write the definitions you need and each claim as a `Theorem` or `Lemma`
   with a proof skeleton: `Proof. Admitted.`, or, when the proof plan is
   clear, its steps as `assert` lines closed by `admit`, ending with
   `Admitted.`
5. Check that the file parses and typechecks with the holes in place:
   compile it with `mcp_rocq_rocq_compile_file` and fix every error. Go on
   only when it compiles.
6. Read each statement against the prose once more: quantifiers,
   hypotheses, the number types (`nat`, `Z`, `R`) and the edge cases. After
   any change, compile again with `mcp_rocq_rocq_compile_file`.
7. Report the file, each statement made with its name, and how the parts of
   the prose map to it. Do not prove the holes here; that is
   `/rocq:prove`.
