"""The CLM1 cases (docs/clm.md section 13).

The read-back and gate cases are written by hand here: each says what the
model did to the mirror and, by hand, exactly what must come back. The
reference reading of the spec (reference.py) is then run on every case;
a case where the two disagree stops the script.

The scenarios are scripted model runs; their expected requests come from
the reference.

    python3 tools/clm/cases.py            # check, then write the goldens
"""

import json
import os
import sys

sys.path.insert(0, os.path.dirname(__file__))
import reference as R  # noqa: E402
from scenarios import SCENARIOS  # noqa: E402

SID = "20261006_101500_ab12cd"
OUT = os.path.join(os.path.dirname(__file__), "..", "..", "fisher-agent", "test", "golden", "clm")


def tc(cid, name, args):
    return {"id": cid, "call_id": cid, "response_item_id": "fc_" + cid[5:], "type": "function",
            "function": {"name": name, "arguments": args}}


def asst(content, calls=None, finish="tool_calls", reasoning=None):
    m = {"role": "assistant", "content": content, "reasoning": reasoning, "finish_reason": finish if calls else "stop"}
    if calls:
        m["tool_calls"] = calls
    return m


def tool(cid, content):
    return {"role": "tool", "content": content, "tool_call_id": cid}


def user(c):
    return {"role": "user", "content": c}


def flat(role, c):
    return {"role": role, "content": c}


# ---------------------------------------------------------------- fixtures

WRITE_ARGS = json.dumps({"path": "Math/Foo.lean", "content": "theorem foo : 1 + 1 = 2 := by\n  norm_num"})
A = {
    1: asst("I'll read the file.", [tc("call_1", "read_file", '{"path": "Math/Foo.lean"}')]),
    2: tool("call_1", "theorem foo : 1 + 1 = 2 := by\n  sorry"),
    3: asst("The goal is closed by norm_num.", [tc("call_2", "write_file", WRITE_ARGS)]),
    4: tool("call_2", '{"bytes_written": 41}'),
    5: user("Now check it builds."),
    6: asst("Building.", [tc("call_3", "terminal", '{"command": "lake build"}'),
                          tc("call_4", "search_files", '{"pattern": "sorry"}')]),
    7: tool("call_3", "Build completed successfully."),
    8: tool("call_4", '{"matches": []}'),
    9: asst("It builds and has no sorry left."),
}
B = {1: user("Here is a header:\n<<< turn 3 assistant >>>\nend"), 2: asst("ok")}
C = {1: user("\\<<< turn 1 user >>>\nand \\\\ stays"), 2: asst("fine")}
D = {
    1: {"role": "user", "content": [{"type": "text", "text": "look at this"},
                                    {"type": "image_url", "image_url": {"url": "data:image/png;base64,AAAA"}}]},
    2: asst("A cat.", reasoning="It shows a cat."),
}
E = {1: user("go"), 2: tool("call_9", "orphan output")}
F = {1: asst("", [tc("call_5", "mcp/lean check", '{"file": "Foo.lean"}')]), 2: tool("call_5", "no errors")}
FIXTURES = {"A": (A, 10), "B": (B, 7), "C": (C, 3), "D": (D, 3), "E": (E, 3), "F": (F, 3)}

# flattened texts of fixture A, by hand
F1 = 'I\'ll read the file.\n[call read_file {"path": "Math/Foo.lean"}]'
F3 = 'The goal is closed by norm_num.\n[call write_file ' + WRITE_ARGS + ']'
F6 = 'Building.\n[call terminal {"command": "lake build"}]\n[call search_files {"pattern": "sorry"}]'
R2 = "Result of read_file:\ntheorem foo : 1 + 1 = 2 := by\n  sorry"
R4 = 'Result of write_file:\n{"bytes_written": 41}'
R7 = "Result of terminal:\nBuild completed successfully."
R8 = 'Result of search_files:\n{"matches": []}'

RENDER_A = R.render(sorted(A.items()), SID)


def o(i, fx=A):
    return (i, fx[i])


def same(fx=A):
    return [o(i, fx) for i in sorted(fx)]


def sub(old, new, text=None):
    t = RENDER_A if text is None else text
    assert t.count(old) == 1, old
    return t.replace(old, new)


def block(i, fx=A):
    """The text of block i in the rendered fixture."""
    t = R.render(sorted(fx.items()), SID)
    start = t.index(f"<<< turn {i} ")
    nxt = t.find("<<< turn ", start + 1)
    while nxt != -1 and t[nxt - 1] != "\n":
        nxt = t.find("<<< turn ", nxt + 1)
    return t[start:] if nxt == -1 else t[start:nxt]


BANNER = "\n".join(R.banner(SID)) + "\n"

# Each case: name, why, fixture, edit (text, or None for a missing file, or
# bytes), and the expectation: edited (bool), messages [(id, msg)], counter;
# or "rejected" for a file that is not UTF-8.
CASES = []


def case(name, why, text, messages=None, counter=None, edited=True, fx="A", rejected=False, missing=False,
         seen=None, kept=0, merge_live=False):
    CASES.append(dict(name=name, why=why, fx=fx, text=text, messages=messages, counter=counter,
                      edited=edited, rejected=rejected, missing=missing, seen=seen, kept=kept, merge_live=merge_live))


case("unedited", "the file as written reads back to the same turns", RENDER_A, same(), 10, edited=False)
case("banner-deleted", "banner lines are ignored, so deleting them is no edit",
     RENDER_A.replace(BANNER, ""), same(), 10, edited=False)
case("banner-altered", "a changed banner line is text before the first header: a new user turn",
     sub(R.banner(SID)[0], "# my notes"), [(10, user("# my notes"))] + same(), 11)
case("prose-only", "a file rewritten as prose with no headers becomes one user turn",
     "Summary: proved foo with norm_num; it builds.\n",
     [(10, user("Summary: proved foo with norm_num; it builds."))], 11)
case("drop-group", "deleting a whole group keeps every other turn as it was",
     sub(block(1) + block(2), ""), [o(i) for i in range(3, 10)], 10)
case("drop-tool-result", "deleting a tool result flattens its group; the flattened assistant then sits "
     "before the next group's assistant, so its text goes into that message, which keeps its tool calls",
     sub(block(2), ""), [(3, {**A[3], "content": F1 + "\n\n" + "The goal is closed by norm_num."}), o(4), o(5), o(6), o(7), o(8), o(9)], 10)
case("drop-assistant", "deleting a group's assistant leaves its result as a user turn",
     sub(block(1), ""), [(2, flat("user", R2))] + [o(i) for i in range(3, 10)], 10)
case("shorten-result", "a shortened tool result flattens its own group only",
     sub("by\n  sorry\n", "by\n"),
     [(1, flat("assistant", F1)), (2, flat("user", "Result of read_file:\ntheorem foo : 1 + 1 = 2 := by"))]
     + [o(i) for i in range(3, 10)], 10)
case("empty-result", "an emptied turn is dropped, with the same effect as deleting it",
     sub("theorem foo : 1 + 1 = 2 := by\n  sorry\n", ""),
     [(3, {**A[3], "content": F1 + "\n\n" + "The goal is closed by norm_num."}), o(4), o(5), o(6), o(7), o(8), o(9)], 10)
case("empty-user", "an emptied user turn is dropped",
     sub("Now check it builds.\n", ""), [o(i) for i in (1, 2, 3, 4, 6, 7, 8, 9)], 10)
case("drop-all", "deleting every block leaves no editable turns", BANNER, [], 10)
case("empty-file", "an empty file leaves no editable turns", "", [], 10)
case("missing-file", "a missing file is no edit", None, same(), 10, edited=False, missing=True)
case("not-utf8", "a file that is not UTF-8 is rejected", RENDER_A.encode() + b"\xff\xfe", same(), 10,
     edited=False, rejected=True)
case("broken-header", "a header that does not match is body text of the block before it",
     sub("<<< turn 5 user >>>", "<<< turn 5 user >>"),
     [o(1), o(2), (3, flat("assistant", F3)),
      (4, flat("user", 'Result of write_file:\n{"bytes_written": 41}\n\n<<< turn 5 user >>\nNow check it builds.')),
      o(6), o(7), o(8), o(9)], 10)
case("role-changed", "a header with the wrong role makes a changed turn; a flattened assistant before a "
     "group's assistant goes into it and gives up its id",
     sub("<<< turn 5 user >>>", "<<< turn 5 assistant >>>"),
     [o(1), o(2), o(3), o(4), (6, {**A[6], "content": "Now check it builds.\n\nBuilding."}), o(7), o(8), o(9)], 10)
case("user-line-added", "a changed user turn keeps its id",
     sub("Now check it builds.\n", "Now check it builds.\nAlso run the tests.\n"),
     [o(i) for i in range(1, 5)] + [(5, user("Now check it builds.\nAlso run the tests."))] + [o(i) for i in range(6, 10)], 10)
case("new-block", "a block with an unknown id is a new turn with a fresh id",
     RENDER_A + "<<< turn 42 user >>>\nNote: foo is done.\n", same() + [(10, user("Note: foo is done."))], 11)
case("duplicate-block", "a second block with the same id is a new turn, flattened; an answer that is newly "
     "last will go into the live turn's assistant",
     RENDER_A + block(9), same() + [(10, flat("assistant", "It builds and has no sorry left."))], 11, merge_live=True)
case("reorder-turns", "moving whole turns keeps them structured, in the new order",
     sub(block(5), "").replace(block(1), block(5) + block(1)),
     [o(5), o(1), o(2), o(3), o(4), o(6), o(7), o(8), o(9)], 10)
case("swap-results", "tool results out of order break their group",
     sub(block(7) + block(8), block(8) + block(7)),
     [o(i) for i in range(1, 6)] + [(6, flat("assistant", F6)), (8, flat("user", R8 + "\n\n" + R7)), o(9)], 10)
case("tool-renamed", "the name on a tool header is not compared",
     sub("<<< turn 2 tool read_file >>>", "<<< turn 2 tool lean_check >>>"), same(), 10, edited=False)
case("tool-no-name", "a changed tool turn with no name in its header",
     sub(block(2), "<<< turn 2 tool >>>\nshort\n\n"),
     [(1, flat("assistant", F1)), (2, flat("user", "Tool result:\nshort"))] + [o(i) for i in range(3, 10)], 10)
case("trailing-space", "trailing whitespace is trimmed before comparing",
     sub("Now check it builds.\n", "Now check it builds.   \n\n\n"), same(), 10, edited=False)
case("leading-blank", "leading blank lines are trimmed before comparing",
     sub("<<< turn 5 user >>>\n", "<<< turn 5 user >>>\n\n  \n"), same(), 10, edited=False)
case("indented", "leading spaces on the first line are not trimmed",
     sub("<<< turn 2 tool read_file >>>\ntheorem", "<<< turn 2 tool read_file >>>\n  theorem"),
     [(1, flat("assistant", F1)), (2, flat("user", "Result of read_file:\n  theorem foo : 1 + 1 = 2 := by\n  sorry"))]
     + [o(i) for i in range(3, 10)], 10)
case("merge-users", "adjacent flattened users merge; the merged block takes no id",
     sub("Now check it builds.\n", "Check the build.\n<<< turn 77 user >>>\nThen stop.\n"),
     [o(i) for i in range(1, 5)] + [(5, user("Check the build.\n\nThen stop."))] + [o(i) for i in range(6, 10)], 10)
case("header-id-zero", "turn ids start at 1, so a header with id 0 is body text",
     RENDER_A + "<<< turn 0 user >>>\nZero.\n",
     [o(i) for i in range(1, 9)] + [(9, flat("assistant", "It builds and has no sorry left.\n\n<<< turn 0 user >>>\nZero."))], 10)
case("header-indented", "a header with a leading space is body text",
     sub("<<< turn 5 user >>>", " <<< turn 5 user >>>"),
     [o(1), o(2), (3, flat("assistant", F3)),
      (4, flat("user", 'Result of write_file:\n{"bytes_written": 41}\n\n <<< turn 5 user >>>\nNow check it builds.')),
      o(6), o(7), o(8), o(9)], 10)
case("crlf", "a file rewritten with CRLF line ends is read as if it had LF ones",
     RENDER_A.replace("\n", "\r\n"), same(), 10, edited=False)
case("crlf-partly", "with only some lines ending in \\r, headers still match and the \\r stays in the text",
     sub("Now check it builds.\n", "Now check it builds.\r\nAnd test.\r\n").replace("<<< turn 9 assistant >>>\n", "<<< turn 9 assistant >>>\r\n"),
     [o(i) for i in range(1, 5)] + [(5, user("Now check it builds.\r\nAnd test."))] + [o(i) for i in range(6, 10)], 10)
case("summary-known-id", "a summary under a known id with a new role keeps that id",
     BANNER + "<<< turn 1 user >>>\nSummary: read Foo.lean, closed foo with norm_num, the build passes.\n",
     [(1, user("Summary: read Foo.lean, closed foo with norm_num, the build passes."))], 10)
case("insert-in-group", "a block inside a group breaks it; the new block merges with the result after it",
     sub(block(2), "<<< turn 50 user >>>\nnote\n\n" + block(2)),
     [(1, flat("assistant", F1)), (10, user("note\n\n" + R2))] + [o(i) for i in range(3, 10)], 11)
case("result-moved", "a result moved away from its call flattens its group; the flattened assistant goes "
     "into the next group's assistant, and the result is a user turn where it now stands",
     sub(block(2), "").replace(block(5), block(5) + block(2)),
     [(3, {**A[3], "content": F1 + "\n\n" + "The goal is closed by norm_num."}), o(4), o(5), (2, flat("user", R2)), o(6), o(7), o(8), o(9)], 10)
case("escape-unedited", "a body line that looks like a header is escaped and reads back",
     R.render(sorted(B.items()), SID), [o(1, B), o(2, B)], 7, edited=False, fx="B")
case("escape-removed", "removing the escape makes the line a header; its unknown id is a new turn",
     R.render(sorted(B.items()), SID).replace("\\<<< turn 3", "<<< turn 3"),
     [(1, user("Here is a header:")), (7, flat("assistant", "end")), o(2, B)], 8, fx="B")
case("escape-backslashes", "lines that already start with backslashes round-trip",
     R.render(sorted(C.items()), SID), [o(1, C), o(2, C)], 3, edited=False, fx="C")
case("image-unedited", "image parts show as [image] and an untouched turn keeps them",
     R.render(sorted(D.items()), SID), [o(1, D), o(2, D)], 3, edited=False, fx="D")
case("image-dropped", "editing a turn with an image makes it plain text",
     R.render(sorted(D.items()), SID).replace("look at this\n[image]\n", "look at this\n"),
     [(1, user("look at this")), o(2, D)], 3, fx="D")
case("reasoning-dropped", "a changed assistant turn loses its reasoning",
     R.render(sorted(D.items()), SID).replace("A cat.", "A small cat."),
     [o(1, D), (2, flat("assistant", "A small cat."))], 3, fx="D")
case("user-name", "a name on a user header is ignored",
     sub("<<< turn 5 user >>>", "<<< turn 5 user note >>>"), same(), 10, edited=False)
case("duplicate-id", "the first block with an id is the turn; a later one with the same id is new, "
     "and here merges into it",
     sub("Now check it builds.\n", "Check the build.\n<<< turn 5 user >>>\nNow check it builds.\n"),
     [o(i) for i in range(1, 5)] + [(5, user("Check the build.\n\nNow check it builds."))] + [o(i) for i in range(6, 10)], 10)
case("reorder-groups", "a whole group moved earlier stays structured",
     sub(block(6) + block(7) + block(8), "").replace(block(5), block(6) + block(7) + block(8) + block(5)),
     [o(1), o(2), o(3), o(4), o(6), o(7), o(8), o(5), o(9)], 10)
case("role-to-tool", "a known id under the tool role is a changed tool turn",
     sub("<<< turn 9 assistant >>>", "<<< turn 9 tool >>>"),
     [o(i) for i in range(1, 9)] + [(9, flat("user", "Tool result:\nIt builds and has no sorry left."))], 10)
case("keep-one", "keeping a single turn", BANNER + block(5), [o(5)], 10)
case("unseen-kept", "whole turns the model has not read in the file are kept when it leaves them out, "
     "at the end, structure and all",
     BANNER + block(5), [o(5), o(6), o(7), o(8), o(9)], 10, seen={1, 2, 3, 4, 5}, kept=4)
case("unseen-only", "leaving out only unread turns is no edit, but the loop says they were kept",
     sub(block(9), ""), same(), 10, edited=False, seen=set(range(1, 9)), kept=1)
case("answer-before-call", "an unchanged answer moved directly before an assistant with tool calls goes into it",
     sub(block(9), "").replace(block(6), block(9) + block(6)),
     [o(i) for i in range(1, 6)] + [(6, {**A[6], "content": "It builds and has no sorry left.\n\nBuilding."}), o(7), o(8)], 10)
case("id-too-long", "a header id of more than nine digits is an unknown id",
     RENDER_A + "<<< turn 1234567890 user >>>\nBig id.\n", same() + [(10, user("Big id."))], 11)
case("crlf-no-final-newline", "a CRLF file without a final line end is still read as LF",
     RENDER_A.replace("\n", "\r\n")[:-2], same(), 10, edited=False)
case("nothing-read", "before any read of the file, leaving every turn out keeps them all",
     BANNER, same(), 10, edited=False, seen=set(), kept=9)
case("loose-tool", "a tool message with no call before it is always a user turn",
     R.render([(1, user("go")), (2, tool("call_9", "orphan output"))], SID),
     [(1, user("go")), (2, flat("user", "Tool result:\norphan output"))], 3, fx="E")
case("tool-name-sanitized", "a tool name outside the header's characters is written with underscores",
     R.render(sorted(F.items()), SID), [o(1, F), o(2, F)], 3, edited=False, fx="F")


GATES = [
    # name, mode, after (a read-back case's result), limit choice, accept (by hand)
    ("fit-shrinks", "fit", "drop-group", "large", True),
    ("fit-grows-fits", "fit", "new-block", "large", True),
    ("fit-grows-over", "fit", "new-block", "before", False),
    ("fit-shrinks-still-over", "fit", "drop-group", "tiny", True),
    ("shrink-grows", "shrink", "new-block", "large", False),
    ("shrink-shrinks", "shrink", "drop-group", "large", True),
    ("shrink-same-size", "shrink", "reorder-turns", "large", False),
    ("fit-same-size", "fit", "reorder-turns", "large", True),
]
LIVE = [asst("", [tc("call_9", "read_file", '{"path": "{MIRROR}"}')]), tool("call_9", '{"content": "..."}')]


def main():
    fails = 0
    out_cases = []
    by_name = {}
    for c in CASES:
        fx, counter0 = FIXTURES[c["fx"]]
        editable = sorted(fx.items())
        data = None if c["text"] is None else (c["text"] if isinstance(c["text"], bytes) else c["text"].encode())
        rb = R.read_back(editable, counter0, data, SID, c["seen"])
        got = (rb.edited, rb.messages, rb.counter, rb.rejected_utf8, rb.missing, rb.kept, rb.merge_into_live)
        want = (c["edited"], c["messages"], c["counter"], c["rejected"], c["missing"], c["kept"], c["merge_live"])
        if got != want:
            fails += 1
            print(f"DISAGREE {c['name']}")
            for k, g, w in zip(("edited", "messages", "counter", "rejected", "missing", "kept", "merge_live"), got, want):
                if g != w:
                    print(f"  {k}:\n    reference {g!r}\n    by hand   {w!r}")
        # P2/P3 on every result: the mirror of the result reads back unchanged
        again = R.read_back(c["messages"], c["counter"], R.render(c["messages"], SID).encode(), SID)
        if again.edited or again.messages != c["messages"]:
            fails += 1
            print(f"NOT IDEMPOTENT {c['name']}")
        by_name[c["name"]] = c
        out_cases.append({
            "name": c["name"], "why": c["why"], "fixture": c["fx"],
            "text": None if c["text"] is None else (c["text"] if isinstance(c["text"], str) else None),
            "bytes_hex": c["text"].hex() if isinstance(c["text"], bytes) else None,
            "seen": None if c["seen"] is None else sorted(c["seen"]),
            "expect": {"edited": c["edited"], "rejected": c["rejected"], "missing": c["missing"],
                       "kept": c["kept"], "merge_into_live": c["merge_live"],
                       "messages": [{"id": i, "message": m} for i, m in c["messages"]], "counter": c["counter"]},
        })
    gates = []
    before = [m for _, m in sorted(A.items())]
    for name, mode, after_case, lim, accept in GATES:
        after = [m for _, m in by_name[after_case]["messages"]]
        limit = {"large": 100000, "before": R.tokens(before + LIVE), "tiny": 10}[lim]
        got = R.gate(mode, before, after, LIVE, limit)
        if got != accept:
            fails += 1
            print(f"DISAGREE gate {name}: reference {got}, by hand {accept}")
        gates.append({"name": name, "mode": mode, "after": after_case, "limit": limit, "accept": accept,
                      "tokens": {"before": R.tokens(before), "after": R.tokens(after), "after_with_live": R.tokens(after + LIVE)}})
    scenarios = []
    for sc in SCENARIOS:
        r = R.run(sc)
        for chk in sc.get("check", []):
            ok, msg = chk(r)
            if not ok:
                fails += 1
                print(f"SCENARIO {sc['name']}: {msg}")
        scenarios.append({"name": sc["name"], "about": sc["about"], "config": sc.get("config", {}),
                          "session_id": sc["session_id"], "mirror_path": sc["mirror_path"], "task": sc["task"],
                          **r})
    print(f"{len(CASES)} read-back cases, {len(GATES)} gate cases, {len(SCENARIOS)} scenarios; {fails} problems")
    if fails:
        sys.exit(1)
    os.makedirs(OUT, exist_ok=True)
    fixtures = {k: {"editable": [{"id": i, "message": m} for i, m in sorted(fx.items())], "counter": n}
                for k, (fx, n) in FIXTURES.items()}
    meta = {"spec": "docs/clm.md", "session_id": SID}
    for name, body in (("readback", {"fixtures": fixtures, "cases": out_cases}),
                       ("gate", {"fixture": "A", "live": LIVE, "cases": gates}),
                       ("scenarios", {"cases": scenarios})):
        with open(os.path.join(OUT, name + ".json"), "w") as f:
            json.dump({**meta, **body}, f, ensure_ascii=False, indent=1)
            f.write("\n")
    print("written to", os.path.normpath(OUT))


if __name__ == "__main__":
    main()
