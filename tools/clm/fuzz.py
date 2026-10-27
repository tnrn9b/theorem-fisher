"""Random checks of the spec's properties (docs/clm.md section 13) on the
reference: the same generators as fisher-agent's ClmPropSpec, in Python.
    python3 tools/clm/fuzz.py [count]"""
import random, sys, os
sys.path.insert(0, os.path.dirname(__file__))
import reference as R
SID = "20261006_101500_ab12cd"
rnd = random.Random(7)

def line():
    k = rnd.random()
    if k < .5: return "".join(rnd.choice("abcdefgh .:;{}[]\"'") for _ in range(rnd.randint(0, 12)))
    return rnd.choice(["", "   ", f"<<< turn {rnd.randint(0,30)} user >>>", "\\" * rnd.randint(1, 3) + "<<< turn 3 assistant >>>",
                       "<<< turn x >>>", "line with a carriage return\r", "théorème ∀ x, x = x",
                       "# Fisher context file for session 20261006_101500_ab12cd."])

def body(): return "\n".join(line() for _ in range(rnd.randint(1, 4)))

def editable():
    out, n = [], 1
    for _ in range(rnd.randint(0, 6)):
        k = rnd.random()
        if k < .3: out.append({"role": "user", "content": body()})
        elif k < .45: out.append({"role": "assistant", "content": body(), "reasoning": None, "finish_reason": "stop"})
        else:
            res = [body() for _ in range(rnd.randint(1, 3))]
            names = ["terminal", "read_file", "search_files"]
            calls = [{"id": f"call_{n+i}", "call_id": f"call_{n+i}", "response_item_id": f"fc_{n+i}", "type": "function",
                      "function": {"name": names[i], "arguments": '{"command": "x"}'}} for i in range(len(res))]
            out.append({"role": "assistant", "content": body(), "reasoning": None, "finish_reason": "tool_calls", "tool_calls": calls})
            out += [{"role": "tool", "content": r, "tool_call_id": f"call_{n+i}"} for i, r in enumerate(res)]
            n += len(res)
    return list(enumerate(out, 1))

def edit(t):
    ls = t.split("\n")
    for _ in range(rnd.randint(1, 4)):
        k = rnd.random(); i = rnd.randint(0, len(ls))
        if k < .3: ls = ls[:i] + ls[i+1:]
        elif k < .5: ls = ls[:i+1] + ls[i:i+1] + ls[i+1:]
        elif k < .75: ls = ls[:i] + body().split("\n") + ls[i:]
        elif k < .95:
            j = rnd.randint(0, len(ls))
            if i < len(ls) and j < len(ls): ls[i], ls[j] = ls[j], ls[i]
        else: ls = [x + "\r" for x in ls]
    return "\n".join(ls)

def legal(orig, ms):
    origs = [m for _, m in orig]
    open_ = []
    for m in ms:
        if m["role"] == "tool":
            if m.get("tool_call_id") not in open_: return "tool out of place"
        elif m["role"] == "assistant": open_ = [c["id"] for c in m.get("tool_calls") or []]
        else: open_ = []
    flat = [m not in origs for m in ms]
    for m, f in zip(ms, flat):
        if f and not R.content_text(m.get("content")).strip(): return "empty flattened"
    for a, b, fa, fb in zip(ms, ms[1:], flat, flat[1:]):
        if fa and fb and a["role"] == b["role"]: return "adjacent flattened"
        if fa and a["role"] == "assistant" and b["role"] == "assistant" and b.get("tool_calls"):
            return "flattened assistant before a call"
    return None

n = int(sys.argv[1]) if len(sys.argv) > 1 else 3000
bad = 0
for k in range(n):
    ts = editable(); c0 = len(ts) + 1
    t0 = R.render(ts, SID)
    rb = R.read_back(ts, c0, t0.encode(), SID)
    if rb.edited: bad += 1; print("P2", k, repr(t0)); break
    t = edit(t0)
    seen = None if rnd.random() < .6 else {i for i, _ in ts if rnd.random() < .5}
    rb = R.read_back(ts, c0, t.encode(), SID, seen)
    res = rb.messages
    why = legal(ts, [m for _, m in res])
    if why: bad += 1; print("P1", why, repr(t)); break
    if rb.edited:
        again = R.read_back(res, rb.counter, R.render(res, SID).encode(), SID)
        if again.edited: bad += 1; print("P3", repr(t)); break
        ids = [i for i, _ in res]
        if len(ids) != len(set(ids)) or any(not (i in dict(ts) or c0 <= i < rb.counter) for i in ids):
            bad += 1; print("P5", ids); break
print(f"{n} random edits, {bad} property failures")
