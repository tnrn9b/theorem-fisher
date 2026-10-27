"""CLM scenarios: scripted model runs over the loop (docs/clm.md sections
6 to 10). Each answer is what the model says; a tool call's `edit` is what
it does to the mirror (the reference turns it into the exact text). The
checks state, by hand, what each scenario is for; the reference fills in
the exact requests.
"""

import json

SID = "20261006_101500_ab12cd"
PATH = "/ctx/20261006_101500_ab12cd/context.txt"


def out(n, tag="x"):
    """A tool result of exactly n characters."""
    unit = f"{tag} output line\n"
    s = (unit * (n // len(unit) + 1))[:n]
    return s


def term(cmd, n, tag="x"):
    return {"name": "terminal", "arguments": {"command": cmd}, "result": out(n, tag)}


def write(edit):
    return {"name": "write_file", "arguments": {"path": "{MIRROR}", "content": None}, "edit": edit}


def patch(old, new):
    return {"name": "patch", "arguments": {"mode": "replace", "path": "{MIRROR}", "old_string": old, "new_string": new},
            "edit": {"replace": [old, new]}, "result": '{"success": true}'}


def read_mirror():
    return {"name": "read_file", "arguments": {"path": "{MIRROR}"}}


def read_prev():
    return {"name": "read_file", "arguments": {"path": "{PREV}"}}


def say(text, *calls):
    return {"content": text, "tool_calls": list(calls)} if calls else {"content": text}


def steps(r):
    return [s for s in r["steps"] if s["call"] != "summary"]


def check(cond, msg):
    return lambda r: (cond(r), msg)


def base(name, about, answers, config=None, checks=()):
    return {"name": name, "about": about, "session_id": SID, "mirror_path": PATH,
            "task": "Prove foo in Math/Foo.lean and check that the project builds.",
            "config": config or {}, "answers": answers, "check": list(checks)}


SCENARIOS = [
    base("one-tool-turn",
         "one tool call then an answer: the mirror is written with no turns, the readout rides on the result",
         [say("Building.", term("lake build", 200)), say("It builds.")],
         {"budget": 4000, "reserve": 0},
         [check(lambda r: steps(r)[0]["mirror_write"].count("<<< turn") == 0, "first mirror has no turns"),
          check(lambda r: "nudge" not in steps(r)[0], "no nudge"),
          check(lambda r: r["result"]["completed"], "completes"),
          check(lambda r: steps(r)[1]["context"][-1]["content"].endswith(steps(r)[0]["context_line"]),
                "the context line ends the last tool result")]),
    base("nudges-in-turn",
         "the context grows past 25%, 50% and 75% of the limit, one threshold a turn; each nudge fires once",
         [say("", term("step 1", 1700, "a")), say("", term("step 2", 1700, "b")),
          say("", term("step 3", 1700, "c")), say("Done.")],
         {"budget": 2000, "reserve": 1},
         [check(lambda r: [s.get("nudge") for s in steps(r)[:3]] == [25, 50, 75], "25, 50, then 75"),
          check(lambda r: r["prompt_section"].count("1,999 tokens") == 1, "an odd limit"),
          check(lambda r: steps(r)[2].get("memory_flush") is True, "memory flush at the first 75%")]),
    base("nudge-jump",
         "one result crosses 25% and 50% at once: only the 50% nudge shows, and 25% does not fire later",
         [say("", term("big", 1990, "a"), term("big too", 1990, "e")), say("", term("more", 1200, "b")), say("Done.")],
         {"budget": 2000, "reserve": 0},
         [check(lambda r: [s.get("nudge") for s in steps(r)[:2]] == [50, 75], "50 then 75")]),
    base("nudge-rearm",
         "after the 25% nudge the model drops turns below 25%; crossing 25% again fires it again",
         [say("", term("step 1", 2100, "a")),
          say("Dropping the old output.", read_mirror(), write({"keep": []})),
          say("", term("step 2", 2100, "b")), say("Done.")],
         {"budget": 2000, "reserve": 0},
         [check(lambda r: steps(r)[0].get("nudge") == 25, "first 25%"),
          check(lambda r: steps(r)[1].get("edit") == "applied" and "nudge" not in steps(r)[1], "edit, below 25%"),
          check(lambda r: steps(r)[2].get("nudge") == 25, "25% again")]),
    base("free-edit-turns",
         "two turns that only edit the context file do not use up iterations: with max_iterations 2 the "
         "run still finishes its work",
         [say("", term("lake build", 300)),
          say("Compacting.", write({"set": {"2": "build ok"}})),
          say("Checking the file.", read_mirror()),
          say("", term("lake env lean Math/Foo.lean", 200, "y")),
          say("Proved and built.")],
         {"budget": 8000, "reserve": 0, "max_iterations": 3},
         [check(lambda r: [s.get("free", False) for s in steps(r)[:4]] == [False, True, True, False], "free turns"),
          check(lambda r: r["result"]["final_response"] == "Proved and built.", "finishes without the summary"),
          check(lambda r: r["result"]["api_calls"] == 5, "five calls")]),
    base("flatten-in-loop",
         "the model shortens a tool result: that group becomes plain text in the next request",
         [say("Reading.", term("cat Math/Foo.lean", 600)),
          say("Shortening the file dump.", write({"set": {"2": "Foo.lean has one sorry, at line 2."}})),
          say("Done.")],
         {"budget": 8000, "reserve": 0},
         [check(lambda r: [m["role"] for m in steps(r)[2]["context"][:2]] == ["assistant", "user"], "flattened"),
          check(lambda r: "tool_calls" not in steps(r)[2]["context"][0], "no tool calls left on the flattened turn"),
          check(lambda r: steps(r)[2]["context"][1]["content"] ==
                "Result of terminal:\nFoo.lean has one sorry, at line 2.", "the result as plain text"),
          check(lambda r: steps(r)[1].get("edit") == "applied", "applied")]),
    base("patch-edit",
         "an edit made with patch instead of write_file",
         [say("Building.", term("lake build", 400)),
          say("Trimming.", patch(out(400)[:-1], "build ok")),
          say("Done.")],
         {"budget": 8000, "reserve": 0},
         [check(lambda r: steps(r)[1].get("edit") == "applied" and steps(r)[1].get("free"), "applied, free")]),
    base("fit-rejects-growth",
         "under fit, an edit that grows the context over the limit is rejected and the turns stay",
         [say("", term("lake build", 1200)),
          say("Adding notes.", write({"append": "<<< turn 99 user >>>\n" + out(4000, "n")})),
          say("Done.")],
         {"budget": 1200, "reserve": 0},
         [check(lambda r: steps(r)[1].get("edit") == "rejected", "rejected"),
          check(lambda r: "would exceed the limit" in steps(r)[1]["context_line"], "receipt")]),
    base("shrink-gate",
         "under shrink, a longer rewrite is rejected even though it fits; a shorter one is applied",
         [say("", term("lake build", 600)),
          say("Expanding.", write({"set": {"2": out(1400, "e")}})),
          say("Shortening.", write({"set": {"2": "ok"}})),
          say("Done.")],
         {"budget": 8000, "reserve": 0, "gate": "shrink"},
         [check(lambda r: [s.get("edit") for s in steps(r)[1:3]] == ["rejected", "applied"], "rejected, applied"),
          check(lambda r: "did not reduce the context" in steps(r)[1]["context_line"], "shrink receipt")]),
    base("rollback",
         "a result pushes the context over the limit: the turn is dropped, a note asks for compaction, "
         "the model compacts and goes on",
         [say("", term("lake build", 1600, "a")),
          say("", term("lake test", 990, "b"), term("lake test 2", 990, "d"), term("lake test 3", 990, "e")),
          say("Compacting.", write({"set": {"2": "build ok"}})),
          say("", term("lake test --quiet", 200, "c")),
          say("All done.")],
         {"budget": 1000, "reserve": 0},
         [check(lambda r: steps(r)[1].get("rollback") == 1, "one turn dropped"),
          check(lambda r: steps(r)[2]["context"][-1]["content"].startswith("[Context over budget: the last turn (terminal, terminal, terminal, "),
                "the note is the last message"),
          check(lambda r: r["result"]["completed"], "completes"),
          check(lambda r: "[... 600 characters cut ...]" in r["steps"][1]["context"][1]["content"], "a long result is cut"),
          check(lambda r: r["answers"][0]["tool_calls"][0]["result"].count("a output line") > 100, "the tool's own result is whole")]),
    base("rollback-exhausted",
         "with max_rollbacks 2, three over-limit turns in a row stop the run",
         [say("", term("a", 5000, "a"), term("b", 5000, "b"), term("c", 5000, "c"))] * 3,
         {"budget": 1000, "reserve": 0, "max_rollbacks": 2},
         [check(lambda r: [s.get("rollback") for s in steps(r)] == [1, 1, None], "two rollbacks then a stop"),
          check(lambda r: steps(r)[2].get("stop") is True, "stopped"),
          check(lambda r: r["result"]["error"] ==
                "Context budget exceeded: 2 rollbacks in a row did not bring the context under 1,000 tokens.", "error"),
          check(lambda r: r["result"]["partial"] and not r["result"]["completed"], "partial"),
          check(lambda r: steps(r)[1].get("note_replaced") is True, "the second note replaces the first"),
          check(lambda r: len(r["final_context"]) == 1, "one note left"),
          check(lambda r: "1,011 tokens) was dropped" in r["steps"][1]["context"][0]["content"],
                "the dropped turn is measured after its results were cut")]),
    base("unseen-kept",
         "with keep_unread on (rule 3): the model reads the file, more turns arrive, then it rewrites the file "
         "from its old read: the turns it had not read are kept, and the receipt says so",
         [say("", term("lake build", 300, "a")),
          say("", read_mirror(), term("lake test", 300, "b")),
          say("Dropping everything I read.", write({"keep": []})),
          say("Done.")],
         {"budget": 8000, "reserve": 0, "keep_unread": True},
         [check(lambda r: steps(r)[2].get("edit") == "applied", "applied"),
          check(lambda r: "3 turns you had not read kept" in steps(r)[2]["context_line"], "receipt"),
          check(lambda r: steps(r)[2]["context"] != [] , "context before the edit"),
          check(lambda r: [m["role"] for m in steps(r)[3]["context"]][:3] == ["assistant", "tool", "tool"],
                "the unread turn is still structured"),
          check(lambda r: steps(r)[3]["context"][1]["content"].startswith("[context file, "), "the read is shortened")]),
    base("unseen-dropped",
         "the same as unseen-kept with keep_unread off, the default: every turn was in the model's prompt, so "
         "the file is taken as written and the turns it had not read in the file go too",
         [say("", term("lake build", 300, "a")),
          say("", read_mirror(), term("lake test", 300, "b")),
          say("Dropping everything I read.", write({"keep": []})),
          say("Done.")],
         {"budget": 8000, "reserve": 0},
         [check(lambda r: steps(r)[2].get("edit") == "applied", "applied"),
          check(lambda r: "had not read" not in steps(r)[2]["context_line"], "nothing kept"),
          check(lambda r: steps(r)[3]["context"][0]["role"] == "assistant", "only the live turn is left")]),
    base("write-twice",
         "the same rewrite sent twice after one read leaves one note, not two: what the model wrote counts as seen",
         [say("", term("lake build", 300, "a")),
          say("Noting.", read_mirror(), write({"keep": [], "append": "<<< turn 50 user >>>\nNote: the build is fine.\n"})),
          say("Again.", write({"keep": [], "append": "<<< turn 50 user >>>\nNote: the build is fine.\n"})),
          say("Done.")],
         {"budget": 8000, "reserve": 0},
         [check(lambda r: sum("Note: the build is fine." in json.dumps(t["message"]) for t in r["final_context"]
                              if t["message"]["role"] == "user") == 1, "one note"),
          check(lambda r: [s.get("edit") for s in steps(r)[1:3]] == ["applied", "applied"], "both applied")]),
    base("backup",
         "an applied edit saves the file as it was beside it; reading the backup is a context turn, shown "
         "once and not cut, and the model copies a dropped turn back from it",
         [say("", term("lake build", 900, "a")),
          say("Dropping everything.", read_mirror(), write({"keep": []})),
          say("Oops, checking the backup.", read_prev()),
          say("Restoring the build result.", write({"append": "<<< turn 2 tool terminal >>>\nbuild ok after all\n"})),
          say("Done.")],
         {"budget": 1000, "reserve": 0},
         [check(lambda r: json.loads(r["answers"][2]["tool_calls"][0]["result"])["content"] == steps(r)[1]["mirror_write"],
                "the backup read gives the file before the first edit"),
          check(lambda r: r["backup"] == steps(r)[3]["mirror_write"], "the last edit made the last backup"),
          check(lambda r: steps(r)[2].get("free") is True, "reading the backup is free"),
          check(lambda r: len(r["answers"][2]["tool_calls"][0]["result"]) > 1000, "the backup read is longer than L, not cut"),
          check(lambda r: any(str(m.get("content", "")).startswith("[context file, ") for m in steps(r)[4]["context"]),
                "and is shortened next time"),
          check(lambda r: any(m["message"]["content"] == "Result of terminal:\nbuild ok after all"
                              for m in r["final_context"]), "the copied turn is back, as text")]),
    base("flat-into-live",
         "the model drops a result, which leaves its call's assistant as text at the end; that text goes into "
         "the live turn's assistant message",
         [say("Looking.", term("ls", 300, "a")),
          say("Removing the listing.", read_mirror(), write({"drop": [2]})),
          say("Done.")],
         {"budget": 8000, "reserve": 0},
         [check(lambda r: steps(r)[1].get("moved_into_live") is True, "moved"),
          check(lambda r: steps(r)[2]["context"][0]["content"].startswith("Looking.\n[call terminal"), "text in the live assistant"),
          check(lambda r: "tool_calls" in steps(r)[2]["context"][0], "which keeps its calls")]),
    base("urgent",
         "near the limit the urgent nudge shows at every check instead of the plain ones",
         [say("I will run the whole suite and read the log closely. " * 22, term("a", 1990, "a"), term("a2", 1990, "e")),
          say("", term("b", 100, "b")), say("", term("c", 100, "c")), say("Done.")],
         {"budget": 2000, "reserve": 0},
         [check(lambda r: [s.get("nudge") for s in steps(r)[:3]] == ["urgent", "urgent", "urgent"], "urgent three times")]),
    base("terminal-edit-free",
         "a terminal command naming the context file is a context-only call: the turn is free and the "
         "edit it made is read back",
         [say("", term("lake build", 300)),
          say("Compacting with a script.",
              {"name": "terminal", "arguments": {"command": "python3 compact.py {MIRROR}"},
               "edit": {"set": {"2": "build ok"}}, "result": '{"output": "", "exit_code": 0}'}),
          say("Done.")],
         {"budget": 8000, "reserve": 0},
         [check(lambda r: steps(r)[1].get("free") is True, "free"),
          check(lambda r: steps(r)[1].get("edit") == "applied", "applied")]),
    base("not-utf8",
         "a write that is not UTF-8 is rejected; the turn still counts as context-only",
         [say("", term("lake build", 300)),
          say("Writing.", write({"bytes_hex": "3c3c3c20ff0a"})),
          say("Done.")],
         {"budget": 8000, "reserve": 0},
         [check(lambda r: "not valid UTF-8" in steps(r)[1]["context_line"], "receipt"),
          check(lambda r: steps(r)[1].get("free") is True, "free")]),
    base("call-cap",
         "context-only turns are free of iterations but not of the call cap: max_iterations 2 allows 4 calls, "
         "then the summary request",
         [say("", term("lake build", 200)),
          say("Look.", read_mirror()), say("Look again.", read_mirror()), say("And again.", read_mirror()),
          say("Summary: the build passed; foo is not proved yet.")],
         {"budget": 8000, "reserve": 0, "max_iterations": 2},
         [check(lambda r: r["steps"][-1]["call"] == "summary", "summary request"),
          check(lambda r: r["result"]["api_calls"] == 4, "four calls"),
          check(lambda r: r["result"]["final_response"].startswith("Summary:"), "summary answer")]),
]
