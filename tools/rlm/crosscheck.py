#!/usr/bin/env python3
"""Cross-check fisher rlm against the reference RLM, call by call.

Usage: .build/rlm-ref/bin/python tools/rlm/crosscheck.py [--fisher CMD] [script.json ...]

Each file in tools/rlm/scripts/ holds {"name", "context" (str or list), "query",
"max_iterations", "replies": [...], "sub": "P"}. The reference
(github.com/alexzhang13/rlm at d04208afbad29ca675ab13478c40ee8bebc84bfe, MIT)
runs RLM(...).completion with its client factory replaced by a scripted client;
`fisher rlm --script --record-prompts` runs the same script. Both record every
model call as {"role": "root"|"sub", "messages": [...]}. The two lists must agree
in length, roles and, call by call, the messages (role and content exactly). The
model name is ignored.

Sub-calls: within one root turn they may arrive in any order on either side
(llm_query_batched runs them concurrently), so they are compared as a multiset
per root turn; root calls are compared in order.

Allowed differences: none. A final-answer mismatch, a non-zero fisher exit, an
empty record, a script that ran no root call, a reference that left scripted
replies unused, and a script that calls llm_query but recorded no sub-call all
count as differences. Messages are compared as full dicts; both sides record
only role and content.

Exit status 0 only when there are no differences. `--self-test` runs one script
and checks that the comparison reports a problem for each of several mutations
of Fisher's record.
"""
import argparse
import collections
import copy
import difflib
import json
import os
import pathlib
import shlex
import subprocess
import sys
import tempfile
import threading

try:
    import rlm.core.rlm as rlm_mod
    from rlm import RLM
    from rlm.clients.base_lm import BaseLM
    from rlm.core.types import ModelUsageSummary, UsageSummary
except ImportError as e:
    sys.exit(
        f"crosscheck.py: cannot import the reference 'rlm' ({e}).\n"
        "Run it with the reference venv's Python: "
        ".build/rlm-ref/bin/python tools/rlm/crosscheck.py\n"
        "(create the venv with tools/rlm/setup-reference.sh)"
    )

HERE = pathlib.Path(__file__).resolve().parent
SCRIPTS = HERE / "scripts"

def as_messages(prompt):
    if isinstance(prompt, str):
        return [{"role": "user", "content": prompt}]
    return [dict(m) for m in prompt]


class ScriptedClient(BaseLM):
    """Answers root calls from the script in order and sub-calls by the sub rule."""

    def __init__(self, replies, sub_prefix):
        super().__init__(model_name="scripted")
        self.replies = list(replies)
        self.sub_prefix = sub_prefix
        self.calls = []
        self.root_calls = 0
        self.sub_calls = 0
        self.lock = threading.Lock()

    def _answer(self, prompt):
        msgs = as_messages(prompt)
        is_root = not isinstance(prompt, str) and not (len(msgs) == 1 and msgs[0]["role"] == "user")
        with self.lock:
            self.calls.append({"role": "root" if is_root else "sub", "messages": copy.deepcopy(msgs)})
            if is_root:
                self.root_calls += 1
                if not self.replies:
                    raise RuntimeError("script: out of replies")
                return self.replies.pop(0)
            self.sub_calls += 1
        last_user = [m["content"] for m in msgs if m["role"] == "user"][-1:]
        return self.sub_prefix + (last_user[0] if last_user else "")[:20]

    def completion(self, prompt):
        return self._answer(prompt)

    async def acompletion(self, prompt):
        return self._answer(prompt)

    def get_usage_summary(self):
        return UsageSummary(model_usage_summaries={"scripted": ModelUsageSummary(len(self.calls), 0, 0, None)})

    def get_last_usage(self):
        return ModelUsageSummary(1, 0, 0, None)


def run_reference(script):
    client = ScriptedClient(script["replies"], script["sub"])
    original = rlm_mod.get_client
    rlm_mod.get_client = lambda backend, kwargs: client
    try:
        r = RLM(
            backend="openai",
            backend_kwargs={"model_name": "scripted"},
            environment="local",
            max_iterations=script["max_iterations"],
        )
        result = r.completion(script["context"], root_prompt=script["query"])
    finally:
        rlm_mod.get_client = original
    problems = []
    if client.replies:
        problems.append(f"reference left {len(client.replies)} scripted replies unused")
    if client.root_calls == 0:
        problems.append("reference made no root call")
    if calls_llm_query(script) and client.sub_calls == 0:
        problems.append("script calls llm_query but the reference made no sub-call")
    return client.calls, result.response, problems


def calls_llm_query(script):
    return any("llm_query" in r for r in script["replies"])


def run_fisher(script, fisher_cmd, tmp):
    ctx = script["context"]
    if isinstance(ctx, str):
        ctx_path = os.path.join(tmp, "context.txt")
        with open(ctx_path, "w", encoding="utf-8") as f:
            f.write(ctx)
    else:
        ctx_path = os.path.join(tmp, "context.json")
        with open(ctx_path, "w", encoding="utf-8") as f:
            json.dump(ctx, f, ensure_ascii=False)
    script_path = os.path.join(tmp, "script.json")
    with open(script_path, "w", encoding="utf-8") as f:
        json.dump(list(script["replies"]) + [{"sub": script["sub"]}], f, ensure_ascii=False)
    rec = os.path.join(tmp, "recorded.json")
    cmd = shlex.split(fisher_cmd) + [
        "rlm", "--context", ctx_path, "--query", script["query"],
        "--max-iterations", str(script["max_iterations"]),
        "--script", script_path, "--record-prompts", rec,
    ]
    p = subprocess.run(cmd, capture_output=True, text=True, encoding="utf-8", timeout=600)
    if not os.path.exists(rec):
        raise RuntimeError(f"fisher rlm wrote no record (exit {p.returncode}):\n{p.stdout}\n{p.stderr}")
    with open(rec, encoding="utf-8") as f:
        calls = [{"role": c["role"], "messages": c["messages"]} for c in json.load(f)]
    return calls, p.stdout, p.stderr, p.returncode


def turns(calls):
    """Split a call list into root calls, each with the sub-calls after it."""
    out = []
    for c in calls:
        if c["role"] == "root":
            out.append((c, []))
        elif out:
            out[-1][1].append(c)
        else:
            out.append((None, [c]))
    return out


def key(c):
    return json.dumps(c["messages"], sort_keys=True, ensure_ascii=False)


def diff_messages(label, a, b):
    """A unified diff of the first differing message of two message lists."""
    for i in range(max(len(a), len(b))):
        ma = a[i] if i < len(a) else None
        mb = b[i] if i < len(b) else None
        if ma != mb:
            ta = json.dumps(ma, indent=1, ensure_ascii=False).splitlines() if ma is not None else ["<missing>"]
            tb = json.dumps(mb, indent=1, ensure_ascii=False).splitlines() if mb is not None else ["<missing>"]
            # split long contents into lines so the diff points at the change
            ta = [x for s in ta for x in s.replace("\\n", "\\n\n").splitlines()]
            tb = [x for s in tb for x in s.replace("\\n", "\\n\n").splitlines()]
            head = f"{label}: message {i}"
            return head + "\n" + "\n".join(
                difflib.unified_diff(ta, tb, "reference", "fisher", lineterm="", n=2)
            )
    return f"{label}: messages equal"


def compare(ref, fis):
    """The list of differences (strings); empty when the runs agree."""
    problems = []
    nroot_r = sum(c["role"] == "root" for c in ref)
    nroot_f = sum(c["role"] == "root" for c in fis)
    if nroot_r == 0:
        problems.append("reference recorded no root call")
    if nroot_f == 0:
        problems.append("fisher recorded no root call")
    if len(ref) != len(fis):
        problems.append(f"call count: reference {len(ref)}, fisher {len(fis)}")
    if nroot_r != nroot_f:
        problems.append(f"root calls: reference {nroot_r}, fisher {nroot_f}")
    tr, tf = turns(ref), turns(fis)
    for n, ((ra, sa), (rb, sb)) in enumerate(zip(tr, tf)):
        if (ra is None) != (rb is None):
            problems.append(f"root turn {n}: one side has no root call")
        elif ra is not None and ra["messages"] != rb["messages"]:
            problems.append(diff_messages(f"root call {n}", ra["messages"], rb["messages"]))
        ca, cb = collections.Counter(map(key, sa)), collections.Counter(map(key, sb))
        if ca != cb:
            lines = [f"root turn {n}: sub-calls differ (reference {len(sa)}, fisher {len(sb)}); as a multiset:"]
            for label, c in (("only in reference", ca - cb), ("only in fisher", cb - ca)):
                for k, v in c.items():
                    lines.append(f"  {label} x{v}: {k[:300]}")
            problems.append("\n".join(lines))
    if len(tr) != len(tf):
        problems.append(f"root turns: reference {len(tr)}, fisher {len(tf)}")
    return problems


def self_test(fisher_cmd):
    """The comparison must pass the real record and fail each mutation of it."""
    path = SCRIPTS / "chunk_batched.json"
    with open(path, encoding="utf-8") as f:
        script = json.load(f)
    ref, _, ref_problems = run_reference(script)
    with tempfile.TemporaryDirectory() as tmp:
        fis, _, _, code = run_fisher(script, fisher_cmd, tmp)
    bad = 0
    if ref_problems or code != 0 or compare(ref, fis):
        print("self-test FAIL: the unmutated record does not compare equal")
        bad += 1
    else:
        print("self-test ok: unmutated record compares equal")
    subs = [i for i, c in enumerate(fis) if c["role"] == "sub"]
    roots = [i for i, c in enumerate(fis) if c["role"] == "root"]
    assert len(subs) >= 2, "self-test script needs at least two sub-calls"

    def mut_root(m):
        m[roots[1]]["messages"][-1]["content"] += "X"

    def mut_sub(m):
        m[subs[0]]["messages"][-1]["content"] += "X"

    def mut_drop(m):
        del m[subs[-1]]

    def mut_dup(m):
        # [X, Y] becomes [X, X]: same length, every member of the reference present
        m[subs[1]]["messages"] = copy.deepcopy(m[subs[0]]["messages"])

    def mut_empty(m):
        m.clear()

    for name, mut in (
        ("a root message changed", mut_root), ("a sub-call prompt changed", mut_sub),
        ("a sub-call dropped", mut_drop), ("a duplicate swapped ([X, Y] vs [X, X])", mut_dup),
        ("an empty record", mut_empty),
    ):
        m = copy.deepcopy(fis)
        mut(m)
        found = compare(ref, m)
        if found:
            print(f"self-test ok: {name}: reported")
        else:
            print(f"self-test FAIL: {name}: not reported")
            bad += 1
    print(f"self-test: {bad} failures")
    return 0 if bad == 0 else 1


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--fisher", default="fisher", help="the command that runs Fisher (default: fisher)")
    ap.add_argument("--self-test", action="store_true", help="check that the comparison catches mutations of Fisher's record")
    ap.add_argument("files", nargs="*", help="script files (default: tools/rlm/scripts/*.json)")
    args = ap.parse_args()
    if args.self_test:
        return self_test(args.fisher)
    files = [pathlib.Path(f) for f in args.files] or sorted(SCRIPTS.glob("*.json"))
    if not files:
        print("0 scripts, no scripts ran")
        return 1
    failures = 0
    for path in files:
        with open(path, encoding="utf-8") as f:
            script = json.load(f)
        name = script["name"]
        try:
            ref, ref_answer, ref_problems = run_reference(script)
            with tempfile.TemporaryDirectory() as tmp:
                fis, fis_answer, fis_err, code = run_fisher(script, args.fisher, tmp)
        except Exception as e:  # a run that cannot be made is a failure, not a skip
            print(f"FAIL {name}: {type(e).__name__}: {e}")
            failures += 1
            continue
        problems = ref_problems + compare(ref, fis)
        if code != 0:
            problems.append(f"fisher exited {code}: {fis_err.strip()[:300]}")
        if fis_answer.removesuffix("\n") != ref_answer:
            problems.append(f"final answer: reference {ref_answer[:80]!r}, fisher {fis_answer.removesuffix(chr(10))[:80]!r}")
        if calls_llm_query(script) and not any(c["role"] == "sub" for c in fis):
            problems.append("script calls llm_query but fisher recorded no sub-call")
        nsub = sum(1 for c in ref if c["role"] == "sub")
        batched = any(len(s) > 1 for _, s in turns(ref))
        note = " (sub-calls compared as a multiset within each root turn)" if batched else ""
        if problems:
            failures += len(problems)
            print(f"FAIL {name}: {len(ref)} calls ({nsub} sub){note}")
            for p in problems:
                print("  " + p.replace("\n", "\n  "))
        else:
            print(f"ok   {name}: {len(ref)} calls ({nsub} sub){note}")
    print(f"{len(files)} scripts, {failures} differences")
    return 0 if failures == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
