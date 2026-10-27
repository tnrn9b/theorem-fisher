"""Drives fisher-rlm/data/rlm_repl.py as a subprocess, playing Fisher's part.

Run with: python3 -I tools/rlm/test_repl.py
"""
import json
import os
import subprocess
import sys
import threading

HERE = os.path.dirname(os.path.abspath(__file__))
SCRIPT = os.path.join(HERE, "..", "..", "fisher-rlm", "data", "rlm_repl.py")


class Repl:
    def __init__(self):
        self.p = subprocess.Popen(
            [sys.executable, "-I", SCRIPT],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.calls = []

    def send(self, obj):
        self.p.stdin.write((json.dumps(obj) + "\n").encode())
        self.p.stdin.flush()

    def recv(self):
        line = self.p.stdout.readline()
        assert line.endswith(b"\n"), "no complete line from the script: %r" % line
        return json.loads(line)

    def load(self, context):
        self.send({"op": "load", "context": context})
        msg = self.recv()
        assert msg == {"op": "ok"}, msg

    def exec(self, code, answer=lambda call: None):
        """Run a block; answer(call) gives the results list, or a raw message."""
        self.calls = []
        self.send({"op": "exec", "code": code})
        while True:
            msg = self.recv()
            if msg["op"] == "call":
                self.calls.append(msg)
                reply = answer(msg)
                if isinstance(reply, dict):
                    self.send(reply)
                else:
                    self.send({"op": "results", "results": reply})
            else:
                assert msg["op"] == "result", msg
                return msg

    def close(self):
        self.send({"op": "close"})
        assert self.p.wait(timeout=10) == 0
        self.p.stdin.close()
        self.p.stdout.close()
        self.p.stderr.close()


def fresh(context="hello"):
    r = Repl()
    r.load(context)
    return r


def test_persist_and_stdout():
    r = fresh()
    a = r.exec("x = 41")
    assert a["stdout"] == "" and a["stderr"] == "", a
    b = r.exec("x += 1\nprint(x)")
    assert b["stdout"] == "42\n", b
    assert b["final"] is None
    r.close()


def test_vars_and_types():
    r = fresh()
    res = r.exec("x = 1\nfl = 2.5\nb = True\nt = (1,)\nn = None\n_h = 3\nimport os\ndef g(): pass")
    assert res["vars"] == ["answer", "context_0", "context", "x", "fl", "b", "t"], res["vars"]
    assert res["types"] == ["_AnswerDict", "str", "str", "int", "float", "bool", "tuple"], res["types"]
    r.close()


def test_exception_text():
    r = fresh()
    res = r.exec("1/0")
    assert res["stderr"].endswith("\nZeroDivisionError: division by zero"), res
    assert "Traceback" not in res["stderr"]
    res = r.exec("print('before')\nraise KeyError('k')")
    assert res["stdout"] == "before\n"
    assert res["stderr"] == "\nKeyError: 'k'", res
    r.close()


def test_answer_item_assignment():
    r = fresh()
    res = r.exec('answer["content"] = "x"\nanswer["ready"] = True')
    assert res["final"] == "x", res
    res = r.exec("pass")
    assert res["final"] is None, "final must be cleared after each exec"
    r.close()


def test_answer_rebound_to_plain_dict():
    r = fresh()
    res = r.exec('answer = {"content": "y", "ready": True}')
    assert res["final"] == "y", res
    res = r.exec('answer["content"] = "z"\nanswer["ready"] = True')
    assert res["final"] == "z", "answer must be an answer dict again"
    res = r.exec('print(type(answer).__name__)')
    assert res["stdout"] == "_AnswerDict\n", res
    r.close()


def test_scaffold_restored():
    r = fresh("orig")
    r.exec('context = "z"\nllm_query = 5')
    res = r.exec("print(context)\nprint(callable(llm_query))")
    assert res["stdout"] == "orig\nTrue\n", res
    r.close()


def test_show_vars():
    r = Repl()
    res = r.exec("print(SHOW_VARS())")
    assert res["stdout"] == "No variables created yet. Use ```repl``` blocks to create variables.\n", res
    r.close()
    r = fresh()
    r.exec("x = 1")
    res = r.exec("print(SHOW_VARS())")
    # 'f' is the file handle left by the reference's own context-loading code.
    want = "Available variables: {'f': 'TextIOWrapper', 'context_0': 'str', 'context': 'str', 'x': 'int'}\n"
    assert res["stdout"] == want, res
    r.close()


def test_llm_query():
    r = fresh()
    res = r.exec('r = llm_query("p")\nprint(r)', lambda c: ["got p"])
    assert r.calls == [{"op": "call", "kind": "llm", "prompts": ["p"], "model": None}], r.calls
    assert res["stdout"] == "got p\n", res
    r.exec('llm_query("q", model="m1")', lambda c: ["x"])
    assert r.calls[0]["model"] == "m1"
    r.close()


def test_llm_query_batched():
    r = fresh()
    res = r.exec('print(llm_query_batched(["a", "b"]))', lambda c: ["A", "B"])
    assert len(r.calls) == 1 and r.calls[0]["prompts"] == ["a", "b"], r.calls
    assert res["stdout"] == "['A', 'B']\n", res
    r.close()


def test_rlm_query():
    r = fresh()
    r.exec('rlm_query("p")', lambda c: ["x"])
    assert r.calls == [{"op": "call", "kind": "rlm", "prompts": ["p"], "model": None}], r.calls
    r.exec('rlm_query_batched(["a", "b"], model="m")', lambda c: ["x", "y"])
    assert r.calls == [{"op": "call", "kind": "rlm", "prompts": ["a", "b"], "model": "m"}], r.calls
    r.close()


def test_malformed_reply():
    r = fresh()
    res = r.exec("print(llm_query('p'))", lambda c: {"op": "nonsense"})
    assert res["stdout"].startswith("Error: LM query failed - "), res
    res = r.exec("print(llm_query_batched(['a', 'b']))", lambda c: ["only one"])
    out = res["stdout"]
    assert out.count("Error: LM query failed - ") == 2, res
    r.close()


def test_calls_from_threads_get_their_own_answers():
    r = fresh()

    def answer(call):
        return ["R:" + p for p in call["prompts"]]

    code = (
        "from concurrent.futures import ThreadPoolExecutor\n"
        "ps = ['p%d' % i for i in range(16)]\n"
        "with ThreadPoolExecutor(16) as ex:\n"
        "    rs = list(ex.map(llm_query, ps))\n"
        "print(sum(r == 'R:' + p for p, r in zip(ps, rs)), rs)"
    )
    # Without serialisation, answers are swapped or a call is lost (a hang),
    # and only some of the time: try several rounds, and end a hang by
    # killing the script after 60 s.
    watchdog = threading.Timer(60, r.p.kill)
    watchdog.start()
    try:
        for _ in range(20):
            res = r.exec(code, answer)
            assert res["stdout"].startswith("16 "), res
            assert len(r.calls) == 16 and all(len(c["prompts"]) == 1 for c in r.calls), r.calls
    finally:
        watchdog.cancel()
    r.close()


def test_stray_fd_write_does_not_break_protocol():
    r = fresh()
    res = r.exec('import os\nos.write(1, b"junk\\n")\nprint("ok")')
    assert res["stdout"] == "ok\n", res
    res = r.exec("print(1)")
    assert res["stdout"] == "1\n"
    r.close()


def test_result_text_intact():
    r = fresh()
    tricky = 'line1\nline2 {"op": "close"}\né☃\U0001F600 tab\t"q"\\'
    res = r.exec("print(llm_query('p'), end='')", lambda c: [tricky])
    assert res["stdout"] == tricky, res
    r.close()


def test_blocked_builtins():
    r = fresh()
    res = r.exec("print([input, eval, exec, compile, globals, locals])")
    assert res["stdout"] == "[None, None, None, None, None, None]\n", res
    r.close()


def test_context_json_and_text_and_cwd():
    r = fresh({"a": [1, 2], "b": "é"})
    res = r.exec("print(context['a'], context is context_0, context['b'])")
    assert res["stdout"] == "[1, 2] True \u00e9\n", res
    r.close()
    r = fresh("café ☃")
    res = r.exec("print(len(context))")
    assert res["stdout"] == "6\n", res
    r.close()
    r = fresh()
    res = r.exec("import os\nopen('f.txt','w').write('x')\nprint(os.path.dirname(os.getcwd()) != '' and os.listdir('.'))")
    assert "context_0.txt" in res["stdout"] and "f.txt" in res["stdout"], res
    r.close()


def test_unknown_op_continues():
    r = fresh()
    r.send({"op": "bogus"})
    msg = r.recv()
    assert msg["op"] == "error" and "bogus" in msg["message"], msg
    r.send("not an object")
    assert r.recv()["op"] == "error"
    assert r.exec("print(1)")["stdout"] == "1\n"
    r.close()


if __name__ == "__main__":
    names = sorted(n for n in globals() if n.startswith("test_"))
    for n in names:
        globals()[n]()
        print("ok  " + n)
    print("%d passed" % len(names))
