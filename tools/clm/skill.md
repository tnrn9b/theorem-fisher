How to keep the context file small while you work:

- Read the whole output of every build or check: do not cut it with head or tail, which can hide the error that matters.
- Do not search or grep the context file; you can see all of it already. Read it only to compact it.
- Compact before the budget is nearly full, in one edit: keep your last few turns as they are, and replace each older turn's tool output with one line that says what it showed (for example "built: 2 errors in foo_bar, line 40" or "read Foo.lean lines 1 to 200"). Keep any text you may reuse, such as a proof that almost worked.
- If the context is too full to read the file, write it afresh with write_file: a few lines on what is done, what is left and what you are doing now.
- Keep track of the work with the todo tool, which survives compaction: what is done, what is left, and what you tried on each part that failed. Update it each time you compact.
- After compacting, go straight back to the task.
