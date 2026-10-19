# Context as a file: Fisher's CLM spec

Status: draft for review (CLM1, 6 Oct 2026). This is the reference for
CLM2 to CLM4. There is no upstream to match; where this spec and the code
disagree, the code is wrong or the spec is changed on purpose.

Fisher's own design, written from the paper (Shao et al., "Context
Language Models", arXiv 2609.37725) and the descriptions in the
facebookresearch/context-language-models READMEs. The file format, the
read-back rules, the prompt and every message the model sees are Fisher's
own. No code, identifiers or prompt text come from that repository.

## 1. The idea

Today Fisher keeps a long conversation under the model's limit the way
upstream does: at a threshold the harness summarises the middle turns
through the auxiliary model. The model is not asked.

Under CLM the model manages its own context. The loop keeps a file, the
*mirror*, that holds the conversation's turns as plain text. The model
edits the mirror with the file tools it already has. After each batch of
tool calls the loop reads the mirror back, checks the edit, and makes the
edited turns the conversation. A token budget, nudges and a rollback keep
the context under a fixed size.

CLM is opt-in (section 11). With the default strategy nothing in this
spec runs, and Fisher behaves exactly as upstream.

## 2. Terms

- **Messages**: the list the loop holds and sends after the system
  prompt: user, assistant and tool messages, in Fisher's OpenAI-style
  form (`role`, `content`, `tool_calls`, `tool_call_id`, `reasoning`...).
- **Pinned prefix**: the system prompt, the tool definitions, any prefill
  messages, and the messages up to and including the first user message
  (the *task*). It never appears in the mirror and edits cannot change it.
- **Live turn**: while a batch of tool calls runs, the assistant message
  that asked for them, plus their results as they arrive.
- **Editable turns**: every message after the pinned prefix and before the
  live turn. These, and only these, are in the mirror.
- **Group**: an assistant message with `tool_calls`, followed directly by
  the tool messages that answer its calls (matched by `tool_call_id`). A
  tool message that is not in a group is *loose*.
- **Structured** message: one that keeps its original form (tool calls,
  ids, reasoning fields). **Flattened** message: plain `user` or
  `assistant` text made from what the model wrote.
- **Whitespace** means the ASCII characters space, tab, `\n`, `\r`, `\v`
  and `\f`, and nothing else. A *blank* line is empty or all whitespace.
- **T(ms)**: the token measure, Fisher's existing rough estimate
  (`estimateMessagesTokensRough`: the lengths of the messages' Python
  reprs, summed, then divided by 4, rounding down). It is deterministic
  and needs no API call, so every decision below replays exactly.
- **Budget** B (default 32,000), **reserve** (default 2,048), **limit**
  L = B minus reserve (default 29,952). The budget covers the editable
  turns and the live turn only; the pinned prefix is not counted (see
  section 12 for why).
- Divisions written `floor(x / y)` round down. Numbers shown to the model
  use commas for thousands.

## 3. Where the mirror lives

- Local sandbox: `$FISHER_HOME/fisher/ctx/<session id>/context.txt`, with
  the loop's own state beside it in `state.json` (section 9).
- The *backup*, `context.prev.txt` beside the mirror (the mirror's path
  with `.txt` replaced by `.prev.txt`): before each applied edit the loop
  writes there the mirror as it wrote it at step 1 of that turn, so the
  model can copy back turns it dropped by mistake. It does not exist
  before the first applied edit.
- Docker and ssh sandboxes: not yet. The file tools run inside the
  sandbox, where the host's `$FISHER_HOME` is out of reach, so `--context
  clm` refuses to start unless `TERMINAL_ENV` is `local` (the message:
  `--context clm needs the local terminal backend for now (TERMINAL_ENV
  is docker)`). A path inside the sandbox, read and written through it,
  is later work.
- The prompt section (section 10) gives the model the absolute path, in
  its canonical spelling (on macOS `/var` is a link to `/private/var`, and
  tool output shows the latter). A call's `path` names the mirror or the
  backup when it is the same after repeated `/` and `/./` are collapsed
  and a `/private` before `/var`, `/tmp` or `/etc` is dropped; every
  rule below that asks whether a call touches the mirror uses that test.
- A `terminal` or `execute_code` call touches the mirror when its
  `command` or `code` contains the mirror's directory in any of those
  spellings. Such a call is ordered and can be a free turn (section 6), but
  a read through the shell does not count as a read for rule 3 of section
  5: the loop cannot tell what the model saw.

The mirror is special to the file tools:

- reads of it do not count towards the read-loop guard;
- it is never checkpointed;
- `patch` on it uses exact matching only (no fuzzy strategies), so an edit
  cannot land on the wrong turn;
- tool calls in one batch that touch it run one after another, in the
  order the model gave them, even when the batch would otherwise run in
  parallel.

## 4. The file format

The file is UTF-8 text with `\n` line ends. It opens with three banner
lines, then one block per editable message, in order:

```
# Fisher context file for session 20261006_101500_ab12cd.
# Each turn starts with a header line. Edit, shorten or delete turns to free context.
# The system prompt and the task are pinned and are not shown here.
<<< turn 3 assistant >>>
I'll look at the lemma first.
[call read_file {"path": "Math/Foo.lean"}]

<<< turn 4 tool read_file >>>
     1|theorem foo : 1 + 1 = 2 := by
     2|  sorry

<<< turn 5 assistant >>>
...
```

**Header line.** A line is a header exactly when it matches

```
^<<< turn ([1-9][0-9]*) (user|assistant|tool)( [A-Za-z0-9_.:-]+)? >>>\r?$
```

The number is the turn's *id* (section 9). An id of more than nine digits
is read as an id no turn has, so it always fits a machine integer. The optional name is written on tool turns:
the name of the tool that produced the result, with every character
outside `A-Za-z0-9_.:-` written as `_`. It is ignored elsewhere and never
compared. The optional `\r` lets a header survive a file with mixed line
ends (see rule 1 of section 5 for files written with CRLF throughout).

**Body.** The lines after a header up to the next header or the end of
the file. When writing, each block is the header, the body, then one blank
line. The body of a message is:

- `user`: its content. A string content is written as is. A list of parts
  is written as its text parts joined by `\n`, with `[image]` for each
  image part.
- `assistant`: its content, then one line per tool call:
  `[call NAME ARGUMENTS]`, where ARGUMENTS is the call's argument string
  as stored. If the content is empty only the call lines are written.
  Reasoning fields are not written.
- `tool`: its content.

**Escaping.** A body line that begins with `<<< turn `, or with one or
more backslashes followed by `<<< turn `, is written with one more
backslash in front. Reading undoes this: a line that begins with one or
more backslashes followed by `<<< turn ` loses one backslash. Every other
line is read verbatim. A line that begins `<<< turn ` but does not match
the header pattern is read as body text.

**Before the first header.** On reading, the three banner lines (exactly
as written, in any order, a trailing `\r` allowed) are dropped from the
lines before the first header. The other lines there, unescaped, form one
block with role `user` and no id. If its trimmed text (rule 4) is empty
there is no such block. So a file rewritten as prose with no headers
becomes one user turn rather than an empty context.

## 5. Reading back

After each batch of tool calls (section 6) the loop reads the mirror and
turns it into a new list of editable messages. The rules, in order:

1. **Decode.** If the file is missing, there is no edit. If it is not valid
   UTF-8, the edit is rejected (section 7). If the text has a `\n` and
   every `\n` in it comes right after a `\r`, each `\r\n` is read as `\n`.
   Then split the text into lines at `\n` (a final `\n` does not start
   another line).
2. **Split** the lines into blocks at header lines. Lines before the first
   header are handled as section 4 says.
3. **Unread turns are kept** (only with `keep_unread: true`, section
   11; off by default). The loop keeps the set of ids the model has
   *seen* in the file. A successful `read_file` of the mirror sets it to
   the ids in the mirror's headers at that moment (a read of the backup
  does not count); an applied edit adds
   the ids of the messages made from the blocks the model wrote (not those
   kept by this rule). It is empty before the first read. A turn (a group,
   or a single message) none of whose messages the model has seen, and
   none of which has a block in the file, is added after the last block as
   the loop would write it; such turns are added in their order. The
   receipt counts the messages kept (section 10). So a rewrite composed
   from an old read cannot silently delete turns that arrived after it.
   Removing a whole turn, by `write_file` or by `patch`, needs a read
   first; emptying a block's text does not. Off (the default), every
   editable turn counts as seen, since all of them were in the prompt
   the model wrote the file from, and a turn without a block in the file
   is removed. The CLM4 runs turned the rule off: it kept turns in most
   edits made through the shell, and near the budget, where a read of
   the file no longer fits, it made every compaction a no-op.
4. **Trim.** A block's text is its body with leading blank lines removed
   and trailing whitespace removed.
5. **Unchanged blocks.** A block is *unchanged* when its id names a
   message in the editable turns, its role is that message's role, it is
   the first block in the file with that id, and its trimmed text equals
   the trimmed body of that message as written. Every other block is
   *changed*.
6. **Groups hold together.** A group keeps its structure only when the
   assistant block and all its tool blocks are unchanged, adjacent, and in
   their original order. Otherwise every block of that group is treated as
   changed, with whatever text it now has. A loose tool message is always
   treated as changed.
7. **Build messages.** An unchanged block becomes its original message,
   bit for bit. A changed block becomes a flattened message:
   - an `assistant` block becomes `{"role": "assistant", "content": TEXT}`;
   - a `user` block becomes `{"role": "user", "content": TEXT}`;
   - a `tool` block becomes a user message whose content is
     `Result of NAME:` and a newline before TEXT, or `Tool result:` and a
     newline when the header has no name.
   A changed block whose trimmed text is empty is dropped.
8. **Merge.** Adjacent flattened messages with the same role become one,
   their contents joined by a blank line (`\n\n`). Structured and
   unchanged plain messages are never merged.
9. **Text before a call goes into it.** An assistant message without tool
   calls that is directly before the assistant message of a structured
   group, and was not directly before it in the editable turns (any
   flattened one, or an unchanged one the edit moved there), is merged
   into that message: its text goes before the message's own content,
   joined by `\n\n` (or replaces it when the content is empty). The
   message keeps its id, tool calls, reasoning and every other field; the
   merged one gives up its id. (Some providers cannot take two assistant
   messages in a row, and Fisher's Anthropic adapter drops the first.)
10. **Ids.** An unchanged message keeps its id. A flattened message keeps
    its block's id if that id named an editable turn and this was its
    first block; merged messages keep the first one's id; every other
    message gets a fresh id from the session's counter. Ids are given
    after rules 8 and 9, in order, so a block merged into another never
    takes one.

The result is always a legal message list: tool messages only ever follow
the assistant message that called them, no flattened message is empty, no
two adjacent flattened messages share a role, and no flattened assistant
comes directly before an assistant with tool calls. Nothing in the pinned
prefix can change. Reading back a file the model did not touch gives back
the editable messages exactly, for any legal list (property P2 in section
13).

There was an edit when the new list differs from the old one, in its
messages or its ids, even if only an id changed. If both are equal there
was no edit, whatever the file's bytes; when rule 3 kept turns, the
context line still says so (section 10).

The read-back also says whether the new list ends with an assistant
message without tool calls that was not the last editable message before:
such a message would sit directly before the live turn's assistant (step
4 of section 6).

## 6. One turn under CLM

When the model answers with tool calls:

1. **Write.** Earlier reads of the mirror are shortened (below), then the
   loop writes the mirror from the editable turns. The live turn is not
   in it.
2. **Run.** The tools run. A successful `read_file` of the mirror records
   what the model has now seen (rule 3 of section 5). Any tool result
   other than a successful read of the mirror or the backup that is longer
   than L characters is cut to its first and last
   floor(L / 2) characters with `\n[... N characters cut ...]\n` between
   them, N being the number left out. For ordinary text that is about a
   quarter of the budget in tokens, so one result alone cannot keep a
   batch rolling back.
3. **Read back** the mirror (section 5). If there was an edit, the gate
   decides (section 7).
4. **Append.** The live turn's mirror writes are shortened (below), and
   the live turn is appended after the editable turns, which are now the
   edited ones or the old ones.
5. **Budget.** The budget check runs (section 8): it may add a nudge, or
   roll back. If it does not roll back, and an applied edit left an
   assistant message without tool calls newly last in the editable turns
   (section 5), that message is now merged into the live turn's assistant
   message as in rule 9. The budget measures before this merge.
6. **Context line.** A context line is appended to the content of the
   last tool message of the live turn (section 10). It always has the
   readout, and the receipt and the nudge when there are any.

When the model answers without tool calls, the run ends as it does today.
No mirror write, no read-back and no budget check.

**Edit turns are kept short.** Without this, an edit would cost more
context than it saves: a `write_file` of the mirror carries the whole new
file in its arguments, a `patch` carries the text it removes, and a read
of the mirror returns the whole context again.

- At step 4, in the live turn's assistant message, each call to
  `write_file` or `patch` whose `path` is the mirror has its text
  arguments replaced: `content` for `write_file`; `old_string`,
  `new_string` and `patch` for `patch`. Each such value that is a string
  becomes `[N characters]`, N being its length. The arguments are written
  back as Python's `json.dumps` writes them by default (ASCII escapes, `, `
  and `: ` separators, keys in their order). Arguments that are not a JSON
  object are left as they are.
- At step 1, every tool message in the editable turns that answers a
  `read_file` of the mirror or the backup, and has not been shortened
  before (the loop
  keeps their ids in `state.json`), has its content replaced by
  `[context file, N characters, shown once; read it again to see it now]`,
  N being the length of the content as stored (its context line
  included). So the model sees each read in full in exactly one request.

A turn shortened this way is the turn from then on: its shortened form is
what the mirror shows and what an unchanged block is compared with.

**Accounting form.** The gate and the budget measure the live turn with
its mirror writes shortened and its own reads of the mirror and the backup
counted at the size of their shortened form, N being the length of the read's content as
it is then (before any context line), though the next request still
carries those reads in full. Without this, reading a mirror more than half the budget
would always roll back.

**Free turns.** A batch is *context-only* when every call in it is
`read_file`, `write_file` or `patch` whose `path` argument is the mirror's
path, or `read_file` of the backup, or `terminal` or `execute_code` whose
`command` or `code` names the mirror's directory (section 3), and every one succeeded (its result is not a JSON object with an
`error` key).

**Ending a run.** Under CLM the run keeps its own count of iterations,
apart from `api_calls`: each model call adds one, except a call whose
batch was context-only or was rolled back. The session's iteration budget
is given back for those same calls. Upstream's warnings about running
out of iterations are not added to tool results under CLM: the context
line rides there instead. The run ends with the summary request,
as it does today on running out of iterations, when the count reaches
max_iterations or when model calls reach 2 × max_iterations. `completed`
is true when the run ends with the model's own answer, and false when it
ends with the summary request or the budget stop (section 8).

## 7. The gate

The gate compares the editable turns before (`E`) and after (`E2`) a
read-back, with the live turn `V` in its accounting form (section 6):

- **fit** (default): accept when T(E2 ++ V) is at most L, or when T(E2) is
  less than T(E).
- **shrink**: accept only when T(E2) is less than T(E).

A rejected edit leaves the editable turns as they were, and the ids it
would have given are not used up. The file is rewritten from the editable
turns at the next write. A file that is not valid UTF-8 is always
rejected.

## 8. Budget, nudges and rollback

Let t = T(editable turns ++ live turn), the live turn in its accounting
form, after step 4 of section 6 and before the context line is added.

**Rollback.** If t is over L, the loop rolls back:

- It removes whole turns from the end until t is at most L, starting with
  the live turn and always removing at least that one. A turn is a group
  (assistant message and its tool results) or a single plain message.
- It adds the rollback note (section 10), a user message. If the last
  editable message is still the previous rollback note (the loop keeps its
  id), the new note replaces it and keeps its id; otherwise the note is
  appended with a fresh id. If the todo list has items, the note ends with
  a blank line and the list as `formatForInjection` writes it.
- It calls the model again. A rollback does not use up an iteration.
- The tool calls in removed turns have already run; their effects on files
  and the world stay. Only the context forgets them.

Rollbacks in a row are counted, and the count resets when a batch ends
within the limit. After max_rollbacks (default 50) in a row, the next
model call is the last: if that batch also ends over L, the run stops.
The over-limit turns are removed as in a rollback, no note is added, and
the result has the context that is left as its messages, `completed`
false, `partial` true and the error
`Context budget exceeded: 50 rollbacks in a row did not bring the context under 29,952 tokens.`
(numbers from the config).

Provider errors for an over-long request (413, context length exceeded)
are handled as a rollback instead of a compression: the newest whole turn
of the editable turns is dropped (a rollback note that is last goes first
and is replaced in place), the rollback note is added, and the request is
sent again. These count towards max_rollbacks; when they run out, or
there is no turn left to drop (`Context length exceeded and there is no
turn left to drop.`), the run ends with that error, as upstream's does
when it cannot compress further.

**Nudges.** If t is within L:

- Three plain nudges, at thresholds floor(L × 25 / 100), floor(L × 50 /
  100) and floor(L × 75 / 100) (7,488, 14,976 and 22,464 by default). A
  nudge fires when t is at or above its threshold and it is armed; firing
  disarms it. It is armed again when a later check finds t below its
  threshold. If one check crosses more than one armed threshold, only the
  highest fires, and all the crossed ones are disarmed.
- One urgent nudge: when t is at least floor(L × 50 / 100) and L minus t
  is less than the larger of floor(L / 10) and 2 × Q. Q is the largest
  T([m]) over the last five assistant messages of the editable turns and
  the live turn (the live one included, in its accounting form; 0 if
  there are none). It fires at every check while this holds, and when it
  fires no plain nudge is shown. Thresholds crossed at that check are
  still disarmed.

**Memory.** The first time in a session that the 75% threshold fires
(whether the plain or the urgent nudge is shown), the loop runs the memory
flush as compression does today (when memory is on). It runs once per
session.

## 9. Ids and the loop's state

- Every message that enters the editable turns gets an id from a counter
  kept per session, starting at 1. Ids are never reused.
- Messages join the editable turns, and take ids, in this order within a
  turn: those made by the read-back, then the live turn's messages (when
  the batch ends within the limit), then a rollback note. A live turn that
  is rolled back never takes ids.
- `state.json` beside the mirror holds `version` (1), `turns` (each
  editable message with its `id`), `counter`, `armed` (the armed nudge
  thresholds), `memory_flushed`, `shortened_reads`, `seen`, `note_id`,
  `rollbacks` and `mirror` (the text last written). It is written
  atomically after every batch and at the end of every run. The run's
  iteration count is not kept: each run starts its own.
- One `state.db` session per conversation: CLM never splits it. The
  session's stored messages are the full transcript, every message as it
  was first created: the run's user message (with the persist override,
  when there is one), each live turn's assistant message and tool results
  as they came back (before shortening, cuts and the context line), each
  rollback note, and the final answer (or the summary request and its
  answer). The live context, after edits, lives in `state.json`.
  `--resume` restores it from there: the history the run gets is the
  pinned prefix from the transcript, then the live turns. Without
  `state.json` it is the transcript.
- Replay: right after the agent's record, a `clm` event holds the
  configuration, the mirror's path, the session id and the state the
  session resumed from. Every mirror write is a `clm_write` (`text`) and
  every read of it (the read-back, and the look the loop takes when the
  model reads the file) a `clm_read` (`file`: the text, `{"bytes_hex":
  ...}` when it is not UTF-8, or null when it was missing). On replay the
  writes are compared with the log and the reads answered from it, so the
  rest is exact. A write that differs, a read the log does not have, or
  logged reads left over, are differences, and the replay is refused.

## 10. What the model sees

All wording is Fisher's own and is fixed by these specs.

**Prompt section**, added at the end of the system prompt, after a blank
line, with PATH, PREV (the backup's path) and the numbers filled in:

> Your conversation is kept in a context file at PATH. It holds every turn
> after the task, one block per turn, each starting with a header line such
> as `<<< turn 7 assistant >>>`. The system prompt and the task are pinned
> and are not in the file. You can edit the file with patch or write_file
> to drop turns you no longer need, or to replace long tool output with a
> short note of what mattered. After each round of tool calls your edits
> become the conversation. Keep headers as they are on turns you leave
> untouched: an untouched turn keeps its tool calls, and a changed one
> becomes plain text. Read the file before you rewrite it with write_file:
> turns you have not read in it are kept. Before an edit is applied, the
> file as it was is saved at PREV, so you can copy back turns you dropped
> by mistake. Once a round is over, your edits
> to the file are kept only as their lengths, and a read of the file only
> as its length. The context has a budget of LIMIT tokens. The budget
> limits how much of the conversation is kept, not how much work you can
> do: when it fills, compact the file and carry on with the task. Each tool
> result ends with a context line showing how much is used. If the budget
> is exceeded, your newest turns are dropped and you are asked to compact
> the file. Turns that only edit the context file do not count towards
> your step limit. Read and edit the context file with read_file, patch
> and write_file, not through the terminal.

**Context line**, one line, appended to the last tool message of the live
turn after a blank line. Parts are joined with `; ` inside one pair of
brackets, in this order:

- the receipt, if there was an edit:
  - `context edit applied: 12 turns -> 5, 18,204 -> 9,880 tokens`, with
    `, 3 turns you had not read kept` (or `1 turn`) added when rule 3 of
    section 5 kept any. The counts and tokens are those of the editable
    turns before and after the edit, without the live turn, before any
    merge into it.
  - `context edit not applied: it only left out 2 turns you had not read, which were kept`
    (or `1 turn`), when rule 3 kept turns and nothing else changed;
  - `context edit rejected: 31,002 tokens would exceed the limit of 29,952; the file was restored`
    (the tokens are T(E2 ++ V));
  - `context edit rejected: it did not reduce the context (9,880 -> 10,114 tokens); the file was restored`;
  - `context edit rejected: the file is not valid UTF-8; the file was restored`;
- the readout, always: `context 10,012 of 29,952 tokens (33%)`, t with the
  percent floor(t × 100 / L);
- a nudge, if one fired:
  - `at 50% of the budget, consider dropping turns you no longer need`
    (25%, 50% or 75%);
  - `nearly full: compact the context file, then carry on with the task (otherwise the newest turns will be dropped)`.

Example: `[context edit applied: 12 turns -> 5, 18,204 -> 9,880 tokens; context 10,012 of 29,952 tokens (33%)]`.

**Rollback note**, a user message: one line, followed by a blank line and
the todo list when the list has items (section 8):

> [Context over budget: the last 2 turns (terminal; read_file, write_file;
> 2,100 tokens) were dropped from the context. Tool calls in them already
> ran. Compact the context file at PATH, then carry on with the task.]

Inside the brackets each dropped turn, oldest first, is named by its
tools joined with `, ` (a group) or by its role (a single message), the
turns joined with `; `, followed by the tokens they took, T of all their
messages in accounting form. With one turn the note reads
`the last turn (terminal, 1,262 tokens) was dropped` and `Tool calls in it
already ran`.

## 11. Configuration

In `config.yaml`, under Fisher's own key (upstream ignores it):

```yaml
fisher:
  context: compress        # or clm
  clm:
    budget: 32000
    reserve: 2048
    gate: fit              # or shrink
    max_rollbacks: 50
    keep_unread: false     # rule 3 of section 5; true turns it on
    skill: /path/to/file   # or none; default: tools/clm/skill.md's text
```

After CLM's prompt section (section 10) and a blank line comes guidance
on how to work under the budget: by default the text of
`tools/clm/skill.md` (built in), else the text of the file `skill`
names, or nothing with `skill: none`. A missing file is an error at the
start of the session.

`--context clm` or `--context compress` on `fisher chat`, and on `fisher`
for the shell's native sessions, overrides the config for that session.
Any other value is refused (`argument --context: invalid choice: 'x'
(choose from 'compress', 'clm', 'rlm')`). ACP and cron sessions use the
compressor for now. A third strategy, `rlm`, which moves old turns to a store and
lets the agent recall them, is described in `docs/rlm.md`. Under `clm` the compressor
does not run (neither the preflight nor the threshold check), and its
side effects are replaced as sections 8 and 9 say: the memory flush runs
once at the first 75% threshold, there is no todo note (the todo list
rides on the rollback note instead), and the session is not split.

## 12. Decisions taken for this draft

These follow the leanings in the plan doc. Each is a question for review.

| Decision | This draft | Why |
| --- | --- | --- |
| Default or opt-in | Opt-in until CLM4 | Upstream parity stays the default |
| Budget | Fixed, 32,000, in config | The paper's default; model-independent |
| What the budget counts | Editable turns and live turn, not the pinned prefix | Fisher's system prompt and tool definitions are large and vary with toolsets; counting them would leave a 32,000 budget little room for turns |
| How the model edits | File tools only (`patch`, `write_file`, `read_file`) | No terminal at the pin; a terminal toolset can come later |
| Gate default | fit, which also accepts any shrinking edit | An edit that helps is never refused |
| Token measure | The rough estimate, not provider usage | Deterministic, replays exactly, no API call |
| Edit turns | Shortened to lengths once their round is over; reads shown in full once | Otherwise every edit costs more context than it saves |
| Unread turns | Kept when a rewrite leaves them out | A rewrite from an old read must not delete what the model never saw in the file |
| Huge results | Cut to L characters, the start and the end kept (decided 6 Oct) | A result that cannot fit would roll back forever; characters are simple and exact |
| Recovering from a bad edit | A backup, `context.prev.txt`, of the file before each applied edit (decided 6 Oct) | Under fit a rewrite that drops too much is accepted; the backup lets the model copy turns back |
| Memory flush | Once per session, at the first 75% threshold | The point where the model will start dropping turns |
| Todo note | On the rollback note only | Otherwise the model's own todo calls stay visible until it drops them |
| `state.db` session | One session, full transcript; live context in `state.json` | Nothing the model drops is lost from the record |

Decided on review (6 Oct 2026): the backup for bad edits, and cutting
long results by characters. A result full of characters that Python's
repr escapes can still measure up to about L tokens after the cut; the
rollback handles that case.

## 13. What the cases check

The cases are in `fisher-agent/test/golden/clm/`, made by
`tools/clm/cases.py`, and run by `ClmReadBackSpec`, `ClmPropSpec` and
`ClmScenarioSpec` in `fisher-agent/test/Fisher/Agent/` (the scenarios
through `ClmHarness`, which drives the real loop with a scripted model,
scripted tools and the mirror in memory).

- `readback.json`, 53 cases written by hand: deleted turns and groups,
  broken and indented headers, a header with id 0, text before the first
  header, merged roles, emptied turns, a tampered banner, reordered turns
  and groups, results moved out of their group, text moved into the next
  call (from a flattened turn and from an unchanged answer moved there),
  escapes, CRLF and mixed line ends, new, duplicate and over-long ids,
  image parts, reasoning fields, unread turns kept (with and without other
  changes), a loose tool message, a tool name outside the header's
  characters, a missing file and one that is not UTF-8. Each gives the
  exact messages, ids, next id, the number of unread messages kept, and
  whether an answer is left newly last.
- `gate.json`, 8 cases: fit and shrink against growing, shrinking and
  same-size edits, inside and over the limit.
- Properties (QuickCheck), over random legal turn lists, random sets of
  seen turns, and random edits of their mirror (lines deleted,
  duplicated, inserted, swapped, CRLF):
  - P1: the read-back is a legal list, as section 5 defines it.
  - P2: reading back the unedited mirror is no edit.
  - P3: reading back is idempotent: the mirror of a read-back result reads
    back as no edit.
  - P4: any body text, header look-alikes and backslashes included,
    round-trips through escaping.
  - P5: ids stay unique, and every new id comes from the counter.
  The pinned prefix is checked in the scenarios: it is never in the mirror
  and no request changes it.
- `scenarios.json`, 18 scripted runs: for every model call, the messages
  after the task, the mirror text written and read back, the context line,
  and the nudges, edits, rollbacks, free turns, stops and memory flush;
  then the result, the final context with ids and the prompt section.
  They cover the nudges (in turn with an odd limit, a jump past two
  thresholds, re-arming, urgent), free turns, the call cap and its summary
  request, flattening in the loop, `patch` edits, both gates, a rollback,
  rollbacks running out with a replaced note and cut results, unread turns
  kept, the same rewrite sent twice, text moved into the live turn, the
  backup and a copy back from it, and a write that is not UTF-8.
- `ClmReplaySpec`: every scenario runs again with a replay log and
  replays bit for bit; a log with a read-back or a write changed is
  refused; `state.json` round-trips; the over-long-request rollback.
- `fisher-tools`' `ContextPathSpec`: on the context file `patch` matches
  exactly and the loop guard does not count reads; elsewhere both behave
  as upstream.
- `fisher-cli`'s `ClmCliSpec`: `fisher chat -q ... --context clm` through
  the CLI with a scripted model: the context file and `state.json` where
  section 3 says, the prompt section, the context line, the transcript
  in `state.db`, a replay log with the `clm` events that `fisher replay`
  replays bit for bit; the flag's errors and the local-backend refusal;
  the flag over the config; the resumed history.
- `tools/e2e-clm.sh`: a live session on the configured model with a
  small budget that must compact, then `fisher replay`, then the same log
  with its read-backs changed, which must be refused.
- `tools/negative-clm.sh` spoils all 81 cases (53 read-back, 8 gate, 20 scenarios) and requires every one to
  fail.
- `tools/clm/reference.py` is an executable reading of this spec, written
  from it alone. It checks the hand-written read-back and gate cases (a
  disagreement stops `cases.py`) and computes the scenarios' expected
  runs. `tools/clm/fuzz.py` checks P1 to P5 on it with random edits.

## 14. The store

With `fisher.clm.store: true` CLM keeps a store beside the context file. The
default is false, and then CLM behaves exactly as sections 1 to 13 say. The
design is `docs/superpowers/specs/2026-10-14-clm-store-design.md`. It exists
because a tight budget (8K, 4K) can be smaller than one tool result or one
batch, so CLM rolled back in a loop or the model gave up.

**Items.** The store is a list of items, shown as `S1`, `S2`, ... in order.
An item holds one original message unchanged, with the CLM turn and the tool
it came from when it had them. Items are never deleted.

**Spill.** After a batch's tools run and before the read-back, each tool
result whose estimated tokens exceed `spill` times the limit is stored as an
item, except a read of the context file or its backup. In the live context
(and so in the mirror) the result becomes its first lines, as many as fit in
10% of the limit, then one line:

```
[stored as S7: 18,400 characters, 412 lines; read_stored("S7", from_line, to_line) or recall(question)]
```

Results of `read_stored` and `recall` are never spilled. A spilled result is not cut again by the middle cut. The read-back, the edit
gate and the budget check see the preview.

**Archive.** Everything that leaves the context is stored once: turns the
model deleted or rewrote, reads CLM shortened, results it cut, and units a
rollback dropped. A spilled result is not stored again, and text the model
wrote into the mirror is never stored. A rollback note names the items that
hold what it dropped (`The dropped turns are stored as S9 to S11.`).

**Tools.** Offered once the store holds an item:

- `read_stored(id, from_line, to_line)`: the lines of item `id` (1-based,
  inclusive), with no model call. Line numbers past the end are clipped. A
  slice over 25% of the limit is cut to that size and ends with a line that
  says where to continue. An unknown id gives
  `Error: no stored item S99 (the store holds S1 to S14).`
- `recall(question)`: the recursive model of `docs/rlm.md` over all items,
  with its limits (`fisher.rlm`, agent defaults) and its budget check before
  every model call.

Without a usable `python3` (missing, or failing the interpreter check of
`docs/rlm.md`) the store works with `read_stored` only: recall is not
offered, and the session prints one warning on stderr. With the store on, a
`fisher.rlm` section that is not valid refuses the session (as
`--context rlm` does), even when recall would not run.

**Prompt.** From the first item on, after CLM's section and skill:

> Large tool outputs and anything you cut from the context are kept in a
> store, so nothing is lost. read_stored gives exact lines of a stored item
> (S1, S2, ...); recall answers a question about all of them. The context
> budget is never a reason to stop: store, read back or recall what you need,
> and keep working until the task is done.

**Settings.**

```yaml
fisher:
  clm:
    store: false           # true turns the store on
    spill: 0.25            # a result over this fraction of the limit is spilled; (0, 1]
    store_prompt: true     # false drops the store's prompt paragraph, for experiments (the tools stay)
```

An invalid value is replaced by the default (0.25 for `spill`, off for `store`, on for `store_prompt`), as the other `fisher.clm` keys are.

**Resume and replay.** The store is saved after each batch to
`$FISHER_HOME/fisher/ctx/<session>/store.json` beside `state.json`; if it
cannot be written the session warns once and keeps it in memory. `--resume`
loads it and ids continue from the highest; a `store.json` that is not valid gives the one-time warning `fisher: store.json is not valid; resuming with an empty store`. The archive covers turns created
after the resume (the map from messages to turns is per process); items
stored before stay readable. The replay log holds `clm_spill` and
`clm_archive` events (and `clm_recall` for a recall), which `fisher replay`
compares; `read_stored` is computed again from the store. A log with a
changed spill or archive is refused.

### Results

Run on 14 Oct 2026, Sonnet 5.5 through OpenRouter, on the CLM4 long session
(the 12 erased proofs of TWISTED's `Step/Kernel.lean`), `--budget 8000` unless
noted. $8.91 for the evaluation. Gate: PASS (more lemmas than plain CLM, for at
most 1.5 times its dollars).

| arm | sessions | closed | dollars | rollbacks | spilled | archived | read_stored | recall |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CLM with the store, 8K | 3 | 36/36 (12, 12, 12) | 5.41 | 5, 7, 6 | 22, 14, 15 | 39, 30, 33 | 0 | 0 |
| plain CLM, 8K | 3 | 17/36 (0, 5, 12) | 4.53 | 8, 0, 6 | | | | |
| CLM with the store, 16K | 1 | 12/12 | 0.90 | 0 | 2 | 6 | 0 | 0 |
| CLM with the store, 4K | 2 | 0/24 | 1.04 | 6, 1 | 24, 8 | 39, 15 | 19, 7 | 0 |

The first two plain sessions are the probe's (13 Oct); the third ran with the
store build and the store off.

- At 8K every store session closed all 12 proofs. The plain sessions failed in
  two ways the store removes: one ended after 50 rollbacks in a row because a
  single batch of Lean output did not fit, and one stopped at 5/12 saying the
  budget had run out. With the store, oversized outputs became previews
  (14 to 22 a session, 130,000 to 220,000 characters kept out of the context)
  and rollbacks stayed at 5 to 7.
- The agent never called `read_stored` or `recall` at 8K; it read files
  again when it needed them. What helped was keeping big outputs out of the
  context, and the prompt's line that the budget is no reason to stop; these
  runs cannot tell the two apart.
- Plain CLM at 8K is uneven (0, 5 and 12), so three sessions a side is a small
  sample; the third plain session closing 12/12 shows CLM alone can succeed at
  8K.
- At 16K the store changes little (2 spills, $0.90 against plain CLM's mean of
  $0.99 a session).
- At 4K the agent used `read_stored` (19 and 7 calls) but closed nothing: with
  about 1,950 tokens it cannot hold a goal, a definition and a proof attempt at
  once. One session still stopped citing the budget.

### Ablation: spill or the prompt line

Run on 14 Oct 2026, same task, model and budget (8K), three sessions an arm,
$10.37 for the ablation (`tools/bench/clm4/ablation.sh`). Arm A turns the store
on with its prompt section off (`--store --no-store-prompt`): spill and
archive, no line about the budget. Arm B leaves the store off and adds only the
line "The context budget is never a reason to stop: compact, read back what
you need, and keep working until the task is done." to CLM's skill.

| arm | closed | dollars | rollbacks | spilled |
| --- | --- | --- | --- | --- |
| A: store, no prompt section | 31/36 (12, 12, 7) | 5.13 | 6, 5, 2 | 11, 15, 14 |
| B: prompt line, no store | 36/36 (12, 12, 12) | 5.24 | 3, 4, 3 | 0 |
| store with its prompt (above) | 36/36 | 5.41 | 5, 7, 6 | 22, 14, 15 |
| plain CLM (above) | 17/36 | 4.53 | 8, 0, 6 | |

- The prompt line alone closed every proof, for $0.17 less than the full
  store. No session in either arm stopped citing the budget.
- Spill alone also removed the plain failures (no rollback loop, no early
  stop), but one session ended at 7/12 with five proofs still `sorry`.
- On this task the line carries most of the gain; the store adds a guard
  against oversized outputs that these sessions rarely needed. Three sessions
  an arm is a small sample: B's 36/36 and A's 31/36 differ by one session.
