"""An executable reading of docs/clm.md (Fisher's CLM spec).

It is the oracle for the CLM cases: it checks the hand-written read-back
cases and computes the scenarios' expected runs. Written from the spec
alone. Section numbers below are the spec's.
"""

from __future__ import annotations

import json
import re

# ---------------------------------------------------------------- tokens


def tokens(msgs):
    """T(ms): a quarter of the summed Python repr lengths (section 2)."""
    return sum(len(repr(m)) for m in msgs) // 4


def commas(n):
    return f"{n:,}"


# ---------------------------------------------------------------- format (4)

HEADER = re.compile(r"^<<< turn ([1-9][0-9]*) (user|assistant|tool)( [A-Za-z0-9_.:-]+)? >>>\r?$")
ESCAPED = re.compile(r"^\\+<<< turn ")
NAME_BAD = re.compile(r"[^A-Za-z0-9_.:-]")
WS = " \t\n\r\x0b\x0c"  # the spec's whitespace: ASCII only


def banner(session_id):
    return [
        f"# Fisher context file for session {session_id}.",
        "# Each turn starts with a header line. Edit, shorten or delete turns to free context.",
        "# The system prompt and the task are pinned and are not shown here.",
    ]


def content_text(content):
    if content is None:
        return ""
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        out = []
        for p in content:
            if isinstance(p, dict) and p.get("type") == "text":
                out.append(p.get("text", ""))
            elif isinstance(p, dict) and p.get("type") in ("image_url", "image", "input_image"):
                out.append("[image]")
        return "\n".join(out)
    return str(content)


def body_of(m):
    role = m["role"]
    text = content_text(m.get("content"))
    if role == "assistant":
        calls = m.get("tool_calls") or []
        lines = [f"[call {c['function']['name']} {c['function']['arguments']}]" for c in calls]
        if not calls:
            return text
        return "\n".join(([text] if text != "" else []) + lines)
    return text


def escape_line(line):
    if line.startswith("<<< turn ") or ESCAPED.match(line):
        return "\\" + line
    return line


def unescape_line(line):
    if ESCAPED.match(line):
        return line[1:]
    return line


def tool_names(editable):
    names = {}
    for _, m in editable:
        if m["role"] == "assistant":
            for c in m.get("tool_calls") or []:
                names[c["id"]] = c["function"]["name"]
    return names


def header_name(m, names):
    if m["role"] != "tool":
        return ""
    n = names.get(m.get("tool_call_id"))
    return NAME_BAD.sub("_", n) if n else ""


def render(editable, session_id):
    """The mirror text for the editable turns, a list of (id, message)."""
    names = tool_names(editable)
    out = list(banner(session_id))
    for i, m in editable:
        name = header_name(m, names)
        out.append(f"<<< turn {i} {m['role']}{' ' + name if name else ''} >>>")
        for line in body_of(m).split("\n"):
            out.append(escape_line(line))
        out.append("")
    return "\n".join(out) + "\n"


def blank(line):
    return line.strip(WS) == ""


def trim(text):
    lines = text.split("\n")
    while lines and blank(lines[0]):
        lines.pop(0)
    return "\n".join(lines).rstrip(WS)


def header_ids(text):
    return {int(h.group(1)) for line in text.split("\n") for h in [HEADER.match(line)] if h}


# ---------------------------------------------------------------- read back (5)


def groups_of(editable):
    """Map id -> group key, and group key -> ordered ids (assistant first).
    A group's results are the tool messages directly after it."""
    member = {}
    groups = {}
    i = 0
    while i < len(editable):
        gid, m = editable[i]
        calls = m.get("tool_calls") or [] if m["role"] == "assistant" else []
        if calls:
            ids = {c["id"] for c in calls}
            members = [gid]
            j = i + 1
            while j < len(editable) and editable[j][1]["role"] == "tool" and editable[j][1].get("tool_call_id") in ids:
                members.append(editable[j][0])
                j += 1
            groups[gid] = members
            for x in members:
                member[x] = gid
            i = j
        else:
            i += 1
    return member, groups


class ReadBack:
    def __init__(self, messages, counter, edited, rejected_utf8=False, missing=False, kept=0):
        self.messages = messages  # list of (id, message)
        self.counter = counter
        self.edited = edited
        self.rejected_utf8 = rejected_utf8
        self.missing = missing
        self.kept = kept  # turns kept because the model had not read them
        self.merge_into_live = False
        self.written = set()


def plain_answer(m):
    return m["role"] == "assistant" and not (m.get("tool_calls") or [])


def read_back(editable, counter, data, session_id, seen=None):
    """data: bytes or None (file missing). seen: ids the model has seen in
    the mirror (None: all of them). Returns ReadBack. Rule numbers are the
    spec's (section 5)."""
    if data is None:
        return ReadBack(editable, counter, False, missing=True)
    try:
        text = data.decode("utf-8")
    except UnicodeDecodeError:
        return ReadBack(editable, counter, False, rejected_utf8=True)
    original = dict(editable)
    position = {i: k for k, (i, _) in enumerate(editable)}
    if seen is None:
        seen = set(original)
    # rule 1: a file whose every \n is preceded by \r is read with \n alone
    if "\n" in text and text.count("\r\n") == text.count("\n"):
        text = text.replace("\r\n", "\n")
    lines = text.split("\n")
    if text.endswith("\n"):
        lines = lines[:-1]
    # rule 2
    ban = set(banner(session_id))
    pre = []
    blocks = []
    cur = None
    for line in lines:
        h = HEADER.match(line)
        if h:
            digits = h.group(1)
            cur = {"id": int(digits) if len(digits) <= 9 else 0, "role": h.group(2),
                   "name": (h.group(3) or "").strip(), "lines": [], "kept": False}
            blocks.append(cur)
        elif cur is None:
            if line.rstrip("\r") in ban:
                continue
            pre.append(unescape_line(line))
        else:
            cur["lines"].append(unescape_line(line))
    if pre and trim("\n".join(pre)) != "":
        blocks.insert(0, {"id": None, "role": "user", "name": "", "lines": pre, "kept": False})
    # rule 3: whole turns the model has not seen, with no block in the file, are kept
    present = {b["id"] for b in blocks}
    names = tool_names(editable)
    kept = 0
    msgs = [m for _, m in editable]
    for a, b in units(msgs):
        unit = editable[a:b]
        if all(i not in seen and i not in present for i, _ in unit):
            for i, m in unit:
                kept += 1
                blocks.append({"id": i, "role": m["role"], "name": header_name(m, names),
                               "lines": body_of(m).split("\n"), "kept": True})
    # rules 4 and 5
    firsts = set()
    for b in blocks:
        b["text"] = trim("\n".join(b["lines"]))
        i = b["id"]
        b["first"] = i is not None and i not in firsts
        if i is not None:
            firsts.add(i)
        b["known"] = i in original and b["first"]
        b["unchanged"] = b["known"] and original[i]["role"] == b["role"] and b["text"] == trim(body_of(original[i]))
    # rule 6
    member, groups = groups_of(editable)
    held = set()
    for gid, members in groups.items():
        pos = [k for k, b in enumerate(blocks) if b["first"] and b["id"] == gid]
        if pos:
            seq = blocks[pos[0]:pos[0] + len(members)]
            if len(seq) == len(members) and all(b["unchanged"] and b["id"] == x for b, x in zip(seq, members)):
                held.add(gid)
    # rule 7
    items = []
    for b in blocks:
        i = b["id"]
        if b["unchanged"]:
            g = member.get(i)
            if g is None and original[i]["role"] != "tool":
                items.append({"kind": "plain", "id": i, "msg": original[i], "block": b})
                continue
            if g is not None and g in held:
                items.append({"kind": "structured", "id": i, "msg": original[i], "block": b})
                continue
        it = flat_item(b)
        if it is not None:
            items.append(it)
    # rule 8: adjacent flattened messages with the same role merge
    merged = []
    for it in items:
        if merged and it["kind"] == "flat" and merged[-1]["kind"] == "flat" and merged[-1]["msg"]["role"] == it["msg"]["role"]:
            prev = merged[-1]
            prev["msg"] = {"role": prev["msg"]["role"], "content": prev["msg"]["content"] + "\n\n" + it["msg"]["content"]}
        else:
            merged.append(it)
    # rule 9: an answer newly before an assistant with tool calls goes into it
    def newly_before(a, b):
        if a["kind"] == "flat":
            return True
        k = position[a["id"]]
        return not (k + 1 < len(editable) and editable[k + 1][0] == b["id"])
    items = []
    for it in merged:
        if (items and items[-1]["kind"] in ("flat", "plain") and plain_answer(items[-1]["msg"])
                and it["kind"] == "structured" and it["msg"]["role"] == "assistant" and newly_before(items[-1], it)):
            a = items.pop()
            it = {**it, "msg": into(content_text(a["msg"].get("content")), it["msg"])}
        items.append(it)
    # rule 10: ids
    result = []
    written = set()
    for it in items:
        if it["kind"] == "flat" and not it["block"]["known"]:
            nid = counter
            counter += 1
        else:
            nid = it["block"]["id"] if it["kind"] == "flat" else it["id"]
        result.append((nid, it["msg"]))
        if not it["block"]["kept"]:
            written.add(nid)
    if result == editable:
        return ReadBack(editable, counter, False, kept=kept)
    rb = ReadBack(result, counter, True, kept=kept)
    rb.written = written
    last = result[-1] if result else None
    rb.merge_into_live = bool(last) and plain_answer(last[1]) and (not editable or editable[-1][0] != last[0])
    return rb


def into(text, m):
    """Rule 8: text put before an assistant message's own content."""
    own = content_text(m.get("content"))
    return {**m, "content": text + "\n\n" + own if own != "" else text}


def flat_item(b):
    if b["text"] == "":
        return None
    if b["role"] == "assistant":
        msg = {"role": "assistant", "content": b["text"]}
    elif b["role"] == "user":
        msg = {"role": "user", "content": b["text"]}
    else:
        head = f"Result of {b['name']}:" if b["name"] else "Tool result:"
        msg = {"role": "user", "content": head + "\n" + b["text"]}
    return {"kind": "flat", "id": b["id"], "msg": msg, "group": None, "block": b}


# ---------------------------------------------------------------- gate (7)


def gate(mode, before, after, live, limit):
    tb, ta = tokens(before), tokens(after)
    if mode == "shrink":
        return ta < tb
    return tokens(after + live) <= limit or ta < tb


# ---------------------------------------------------------------- the loop (6, 8, 10)

SUMMARY_REQUEST = ("You've reached the maximum number of tool-calling iterations allowed. Please provide a final "
                   "response summarizing what you've found and accomplished so far, without calling any more tools.")


def nudge_text(p):
    return f"at {p}% of the budget, consider dropping turns you no longer need"


URGENT = "nearly full: compact the context file, then carry on with the task (otherwise the newest turns will be dropped)"


def turn_label(msgs):
    """A dropped turn in the rollback note: its tools, or its role."""
    calls = msgs[0].get("tool_calls") or [] if msgs[0]["role"] == "assistant" else []
    return ", ".join(c["function"]["name"] for c in calls) if calls else msgs[0]["role"]


def rollback_note(dropped, path, todo=None):
    """dropped: the removed turns, oldest first, each a list of messages (accounting form)."""
    labels = "; ".join(turn_label(t) for t in dropped)
    size = commas(tokens([m for t in dropped for m in t]))
    if len(dropped) == 1:
        what, it = f"the last turn ({labels}, {size} tokens) was", "it"
    else:
        what, it = f"the last {len(dropped)} turns ({labels}; {size} tokens) were", "them"
    note = (f"[Context over budget: {what} dropped from the context. Tool calls in {it} already ran. "
            f"Compact the context file at {path}, then carry on with the task.]")
    return note + ("\n\n" + todo if todo else "")


def budget_error(n, limit):
    return f"Context budget exceeded: {n} rollbacks in a row did not bring the context under {commas(limit)} tokens."


def cut_result(content, limit):
    """Section 6: a tool result over L characters keeps its first and last floor(L / 2)."""
    if len(content) <= limit:
        return content
    h = limit // 2
    return content[:h] + f"\n[... {len(content) - 2 * h} characters cut ...]\n" + content[-h:]


def prompt_section(path, limit):
    return (
        f"Your conversation is kept in a context file at {path}. It holds every turn after the task, one block per "
        "turn, each starting with a header line such as `<<< turn 7 assistant >>>`. The system prompt and the task "
        "are pinned and are not in the file. You can edit the file with patch or write_file to drop turns you no "
        "longer need, or to replace long tool output with a short note of what mattered. After each round of tool "
        "calls your edits become the conversation. Keep headers as they are on turns you leave untouched: an "
        "untouched turn keeps its tool calls, and a changed one becomes plain text. Read the file before you rewrite "
        "it with write_file: turns you have not read in it are kept. Before an edit is applied, the file as it was "
        f"is saved at {prev_path(path)}, so you can copy back turns you dropped by mistake. Once a round is over, your "
        "edits to the file are kept only as their lengths, and a read of the file only as its length. "
        f"The context has a budget of {commas(limit)} tokens. The budget limits how much of the conversation is "
        "kept, not how much work you can do: when it fills, compact the file and carry on with the task. Each tool "
        "result ends with a context line showing how "
        "much is used. If the budget is exceeded, your newest turns are dropped and you are asked to compact the "
        "file. Turns that only edit the context file do not count towards your step limit. Read and edit the context "
        "file with read_file, patch and write_file, not through the terminal."
    )


def units(msgs):
    """Split a message list into turns: a group, or a single message."""
    out = []
    i = 0
    while i < len(msgs):
        m = msgs[i]
        calls = m.get("tool_calls") or [] if m["role"] == "assistant" else []
        if calls:
            ids = {c["id"] for c in calls}
            j = i + 1
            while j < len(msgs) and msgs[j]["role"] == "tool" and msgs[j].get("tool_call_id") in ids:
                j += 1
            out.append((i, j))
            i = j
        else:
            out.append((i, i + 1))
            i += 1
    return out


def blocks_text(text):
    """Split mirror text into (prefix lines, [(id, header line, body lines)])."""
    lines = text.split("\n")
    if text.endswith("\n"):
        lines = lines[:-1]
    pre, blocks = [], []
    for line in lines:
        h = HEADER.match(line)
        if h:
            blocks.append([int(h.group(1)), line, []])
        elif blocks:
            blocks[-1][2].append(line)
        else:
            pre.append(line)
    return pre, blocks


def join_blocks(pre, blocks):
    out = list(pre)
    for _, h, body in blocks:
        out.append(h)
        out.extend(body)
    return "\n".join(out) + "\n"


def apply_edit(text, edit):
    """A scripted model edit of the mirror text; returns the new text (str) or bytes."""
    if "write" in edit:
        return edit["write"]
    if "bytes_hex" in edit:
        return bytes.fromhex(edit["bytes_hex"])
    pre, blocks = blocks_text(text)
    if "drop" in edit:
        blocks = [b for b in blocks if b[0] not in edit["drop"]]
    if "keep" in edit:
        blocks = [b for b in blocks if b[0] in edit["keep"]]
    if "set" in edit:
        new = []
        for b in blocks:
            if str(b[0]) in edit["set"]:
                body = edit["set"][str(b[0])].split("\n") + [""]
                new.append([b[0], b[1], body])
            else:
                new.append(b)
        blocks = new
    if "replace" in edit:
        old, new = edit["replace"]
        t = join_blocks(pre, blocks)
        if old not in t:
            raise SystemExit(f"replace: {old!r} not in the mirror")
        return t.replace(old, new, 1)
    if "append" in edit:
        return join_blocks(pre, blocks) + edit["append"]
    return join_blocks(pre, blocks)


def shorten_writes(assistant, path):
    """Section 6: a live turn's mirror writes keep only the lengths of their texts."""
    for c in assistant.get("tool_calls") or []:
        fn = c["function"]
        keys = {"write_file": ("content",), "patch": ("old_string", "new_string", "patch")}.get(fn["name"])
        if not keys:
            continue
        args = json.loads(fn["arguments"])
        if args.get("path") != path:
            continue
        for k in keys:
            if isinstance(args.get(k), str):
                args[k] = f"[{len(args[k])} characters]"
        fn["arguments"] = json.dumps(args)


def norm_path(p):
    """Section 3: repeated / and /./ collapsed, macOS's /private dropped before /var, /tmp and /etc."""
    while True:
        q = p.replace("//", "/").replace("/./", "/")
        if q == p:
            break
        p = q
    if p.startswith("/private/") and p[len("/private/"):].startswith(("var/", "tmp/", "etc/")):
        p = p[len("/private"):]
    return p


def same_path(a, b):
    return a is not None and b is not None and norm_path(a) == norm_path(b)


def path_aliases(p):
    n = norm_path(p)
    return [p, n, "/private" + n] if n.startswith(("/var/", "/tmp/", "/etc/")) else [p, n]


def mentions_context(path, text):
    """Section 3: a shell command or code naming the context file's directory."""
    d = path.rsplit("/", 1)[0]
    return bool(d) and any(a in (text or "") for a in path_aliases(d))


def prev_path(path):
    """Section 3: the backup beside the mirror."""
    return path[:-4] + ".prev.txt" if path.endswith(".txt") else path + ".prev"


def shorten_reads(ctx, path, done):
    """Section 6: earlier reads of the mirror keep only their length."""
    reads = set()
    for _, m in ctx:
        if m["role"] == "assistant":
            for c in m.get("tool_calls") or []:
                if c["function"]["name"] == "read_file":
                    try:
                        a = json.loads(c["function"]["arguments"])
                    except Exception:
                        continue
                    if a.get("path") in (path, prev_path(path)):
                        reads.add(c["id"])
    for k, (i, m) in enumerate(ctx):
        if m["role"] == "tool" and m.get("tool_call_id") in reads and i not in done:
            done.add(i)
            ctx[k] = (i, {**m, "content": read_stub(m["content"])})


def read_stub(content):
    return f"[context file, {len(content)} characters, shown once; read it again to see it now]"


def is_error_result(result):
    try:
        v = json.loads(result)
    except Exception:
        return False
    return isinstance(v, dict) and "error" in v


def is_mirror_call(c, path, names):
    if c["function"]["name"] not in names:
        return False
    try:
        a = json.loads(c["function"]["arguments"])
    except Exception:
        return False
    return isinstance(a, dict) and a.get("path") == path


def run(sc):
    cfg = {"budget": 32000, "reserve": 2048, "gate": "fit", "max_rollbacks": 50, "max_iterations": 90,
           "keep_unread": False}
    cfg.update(sc.get("config", {}))
    limit = cfg["budget"] - cfg["reserve"]
    path = sc["mirror_path"]
    sid = sc["session_id"]
    max_it = cfg["max_iterations"]
    ctx = []          # editable turns: (id, message)
    counter = 1
    armed = {25: True, 50: True, 75: True}
    flushed = False
    shortened = set()  # ids of mirror reads already shortened
    seen = set()       # ids in the mirror as the model last read it
    note_id = None     # id of the last rollback note
    backup = None      # the mirror before the last applied edit (context.prev.txt)
    rollbacks = 0
    calls = 0
    iters = 0
    call_no = 0
    answers = list(sc["answers"])
    steps = []
    concrete = []
    result = None

    def final(content, completed):
        return {"completed": completed, "partial": False, "final_response": content, "error": None, "api_calls": calls}

    while True:
        if iters >= max_it or calls >= 2 * max_it:
            summary = {"role": "user", "content": SUMMARY_REQUEST}
            if not answers:
                raise SystemExit(f"{sc['name']}: no answer left for the summary request")
            ans = answers.pop(0)
            concrete.append({"content": ans.get("content", "")})
            steps.append({"call": "summary", "context": [m for _, m in ctx] + [summary]})
            c = ans.get("content", "").strip()
            ctx.append((counter, summary))
            ctx.append((counter + 1, {"role": "assistant", "content": c}))
            counter += 2
            result = final(c, False)
            break
        calls += 1
        iters += 1
        if not answers:
            raise SystemExit(f"{sc['name']}: ran out of answers at call {calls}")
        ans = answers.pop(0)
        step = {"call": calls, "context": [m for _, m in ctx]}
        steps.append(step)
        tcs = ans.get("tool_calls") or []
        if not tcs:
            c = ans.get("content", "")
            concrete.append({"content": c})
            ctx.append((counter, {"role": "assistant", "content": c, "reasoning": None, "finish_reason": "stop"}))
            counter += 1
            result = final(c, True)
            break
        # step 1: earlier reads of the mirror are shortened, then the mirror is written
        shorten_reads(ctx, path, shortened)
        mirror = render(ctx, sid)
        step["mirror_write"] = mirror
        file = mirror.encode("utf-8")
        # step 2: the tools run
        tool_msgs, ccalls, live_reads = [], [], []
        free = True
        for tc in tcs:
            call_no += 1
            cid = f"call_{call_no}"
            args = dict(tc.get("arguments", {}))
            for k, v in list(args.items()):
                if v == "{MIRROR}":
                    args[k] = path
                elif v == "{PREV}":
                    args[k] = prev_path(path)
                elif isinstance(v, str) and "{MIRROR}" in v:
                    args[k] = v.replace("{MIRROR}", path)
            after = None
            if "edit" in tc:
                new = apply_edit(file.decode("utf-8"), tc["edit"])
                if tc["name"] == "write_file":
                    args["content"] = new if isinstance(new, str) else new.decode("latin-1")
                file = new.encode("utf-8") if isinstance(new, str) else new
                after = new if isinstance(new, str) else {"bytes_hex": new.hex()}
            mirror_read = tc["name"] == "read_file" and same_path(args.get("path"), path)
            prev_read = tc["name"] == "read_file" and same_path(args.get("path"), prev_path(path))
            res = tc.get("result")
            if res is None:
                if tc["name"] == "write_file":
                    res = json.dumps({"bytes_written": len(file)})
                elif mirror_read:
                    res = json.dumps({"content": file.decode("utf-8")})
                elif prev_read:
                    res = json.dumps({"content": backup} if backup is not None else {"error": "File not found"})
                else:
                    res = json.dumps({"success": True})
            raw = res  # what the tool returned, before the cut
            if mirror_read and not is_error_result(res):
                seen = header_ids(file.decode("utf-8"))
                live_reads.append(len(tool_msgs))
            elif prev_read and not is_error_result(res):
                live_reads.append(len(tool_msgs))
            else:
                res = cut_result(res, limit)
            shell = tc["name"] in ("terminal", "execute_code") and mentions_context(
                path, args.get("command") if isinstance(args.get("command"), str) else args.get("code"))
            ctx_call = (tc["name"] in ("read_file", "write_file", "patch") and same_path(args.get("path"), path)) \
                or prev_read or shell
            if not (ctx_call and not is_error_result(res)):
                free = False
            tool_msgs.append({"role": "tool", "content": res, "tool_call_id": cid})
            entry = {"id": cid, "name": tc["name"], "arguments": args, "result": raw}
            if after is not None:
                entry["mirror_after"] = after
            ccalls.append(entry)
        concrete.append({"content": ans.get("content", ""), "tool_calls": ccalls})
        assistant = {
            "role": "assistant", "content": ans.get("content", ""), "reasoning": None, "finish_reason": "tool_calls",
            "tool_calls": [
                {"id": c["id"], "call_id": c["id"], "response_item_id": "fc_" + c["id"][5:], "type": "function",
                 "function": {"name": c["name"], "arguments": json.dumps(c["arguments"])}}
                for c in ccalls],
        }
        shorten_writes(assistant, path)
        # the live turn as the budget counts it: its reads of the mirror at their shortened size
        def acct_live():
            return [assistant] + [{**m, "content": read_stub(m["content"])} if k in live_reads else m
                                  for k, m in enumerate(tool_msgs)]
        # step 3: read back, gate
        # rule 3 off (keep_unread false, the default): every turn in the prompt counts as read
        rb = read_back(ctx, counter, file, sid, seen if cfg["keep_unread"] else {i for i, _ in ctx})
        step["mirror_read"] = file.decode("utf-8") if not rb.rejected_utf8 else {"bytes_hex": file.hex()}
        parts = []
        merge_live = False
        if rb.rejected_utf8:
            parts.append("context edit rejected: the file is not valid UTF-8; the file was restored")
        elif rb.edited:
            before = [m for _, m in ctx]
            after_m = [m for _, m in rb.messages]
            if gate(cfg["gate"], before, after_m, acct_live(), limit):
                receipt = (f"context edit applied: {len(before)} turns -> {len(after_m)}, "
                           f"{commas(tokens(before))} -> {commas(tokens(after_m))} tokens")
                if rb.kept:
                    receipt += f", {rb.kept} turn{'s' if rb.kept != 1 else ''} you had not read kept"
                parts.append(receipt)
                ctx = rb.messages
                counter = rb.counter
                seen = seen | rb.written
                backup = mirror
                step["edit"] = "applied"
                merge_live = rb.merge_into_live
            elif cfg["gate"] == "shrink":
                parts.append(f"context edit rejected: it did not reduce the context ({commas(tokens(before))} -> "
                             f"{commas(tokens(after_m))} tokens); the file was restored")
                step["edit"] = "rejected"
            else:
                parts.append(f"context edit rejected: {commas(tokens(after_m + acct_live()))} tokens would exceed "
                             f"the limit of {commas(limit)}; the file was restored")
                step["edit"] = "rejected"
        elif rb.kept:
            parts.append(f"context edit not applied: it only left out {rb.kept} turn{'s' if rb.kept != 1 else ''} "
                         "you had not read, which were kept")
        combined = [m for _, m in ctx] + acct_live()
        t = tokens(combined)
        # step 5: budget
        if t > limit:
            us = units(combined)
            keep = len(us) - 1
            while keep > 0 and tokens(combined[:us[keep - 1][1]]) > limit:
                keep -= 1
            dropped = [combined[a:b] for a, b in us[keep:]]
            kept_ctx = ctx[:us[keep - 1][1]] if keep > 0 else []
            if rollbacks == cfg["max_rollbacks"]:
                ctx = kept_ctx
                step["stop"] = True
                result = {"completed": False, "partial": True, "final_response": None,
                          "error": budget_error(cfg["max_rollbacks"], limit), "api_calls": calls}
                break
            rollbacks += 1
            note = {"role": "user", "content": rollback_note(dropped, path)}
            if kept_ctx and kept_ctx[-1][0] == note_id:
                kept_ctx = kept_ctx[:-1] + [(note_id, note)]
                step["note_replaced"] = True
            else:
                note_id = counter
                kept_ctx = kept_ctx + [(counter, note)]
                counter += 1
            ctx = kept_ctx
            step["rollback"] = len(dropped)
            iters -= 1
            continue
        rollbacks = 0
        # step 4, after the budget check: an answer left last goes into the live assistant
        if merge_live:
            _, a = ctx.pop()
            assistant = into(content_text(a.get("content")), assistant)
            step["moved_into_live"] = True
        if free:
            iters -= 1
            step["free"] = True
        crossed = [p for p in (25, 50, 75) if t >= limit * p // 100 and armed[p]]
        for p in (25, 50, 75):
            if t < limit * p // 100:
                armed[p] = True
        for p in crossed:
            armed[p] = False
        fired = max(crossed) if crossed else None
        q = max([tokens([m]) for m in combined if m["role"] == "assistant"][-5:], default=0)
        urgent = t >= limit * 50 // 100 and (limit - t) < max(limit // 10, 2 * q)
        parts.append(f"context {commas(t)} of {commas(limit)} tokens ({t * 100 // limit}%)")
        if urgent:
            parts.append(URGENT)
            step["nudge"] = "urgent"
        elif fired:
            parts.append(nudge_text(fired))
            step["nudge"] = fired
        if fired == 75 and not flushed:
            flushed = True
            step["memory_flush"] = True
        # step 6: the context line
        line = "[" + "; ".join(parts) + "]"
        step["context_line"] = line
        tool_msgs[-1]["content"] = tool_msgs[-1]["content"] + "\n\n" + line
        for m in [assistant] + tool_msgs:
            ctx.append((counter, m))
            counter += 1
    return {
        "backup": backup,
        "answers": concrete,
        "steps": steps,
        "result": result,
        "final_context": [{"id": i, "message": m} for i, m in ctx],
        "counter": counter,
        "prompt_section": prompt_section(path, limit),
    }
