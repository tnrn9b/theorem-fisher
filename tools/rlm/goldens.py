#!/usr/bin/env python3
"""Write golden files for fisher-rlm from the reference implementation.

Usage: .build/rlm-ref/bin/python tools/rlm/goldens.py <name>...

Each name is a generator in GENERATORS; it returns a JSON-serialisable value,
written to fisher-rlm/test/golden/<name>.json. The reference is
github.com/alexzhang13/rlm at d04208afbad29ca675ab13478c40ee8bebc84bfe (MIT);
see tools/rlm/setup-reference.sh. Later tasks register `parse`, `prompt`
and `log`.
"""
import json
import pathlib
import sys

try:
    import rlm  # noqa: F401
except ImportError as e:
    sys.exit(
        f"goldens.py: cannot import the reference 'rlm' ({e}).\n"
        "Run it with the reference venv's Python: "
        ".build/rlm-ref/bin/python tools/rlm/goldens.py <name>...\n"
        "(create the venv with tools/rlm/setup-reference.sh)"
    )

ROOT = pathlib.Path(__file__).resolve().parents[2]
OUT = ROOT / "fisher-rlm" / "test" / "golden"

# name -> zero-argument function returning a JSON-serialisable value
GENERATORS = {}


def _parse_cases():
    from rlm.core.types import CodeBlock, REPLResult, RLMIteration
    from rlm.utils.parsing import find_code_blocks, format_iteration

    def res(stdout="", stderr="", vars=()):
        return {"stdout": stdout, "stderr": stderr, "vars": list(vars)}

    cases = [
        ("no block", "Just thinking, no code.", []),
        ("one block", "Let me look.\n```repl\nprint(1)\n```\nDone.", [res("1\n")]),
        (
            "two blocks",
            "```repl\na = 1\n```\ntext\n```repl\nprint(a)\n```",
            [res("", "", ["a"]), res("1\n")],
        ),
        ("python block ignored", "```python\nprint(1)\n```", []),
        ("unclosed fence", "Start\n```repl\nprint(1)\n", []),
        ("trailing spaces after tag", "```repl   \nprint(2)\n```", [res("2\n")]),
        ("windows line ends", "```repl\r\nx = 1\r\nprint(x)\r\n```\r\n", [res("1\n", "", ["x"])]),
        ("no output", "```repl\npass\n```", [res()]),
        ("stderr only", "```repl\n1/0\n```", [res("", "\nZeroDivisionError: division by zero")]),
        ("output of 20001 characters", "```repl\nprint('a' * 20001)\n```", [res("a" * 20000)]),
        ("output of exactly 20000", "```repl\nprint('a' * 20000)\n```", [res("a" * 19999)]),
        (
            "variables listed",
            "```repl\nanswer = 1\n```",
            [res("ok\n", "", ["answer", "context_0", "context", "x"])],
        ),
        ("unicode output", "```repl\nprint('h\u00e9llo \u4e16\u754c')\n```", [res("h\u00e9llo \u4e16\u754c\n")]),
        ("reply without blocks", "The answer is 4.", []),
        # edge cases for the \s* and non-greedy behaviour of the pattern
        ("empty body", "```repl\n\n```", [res()]),
        ("blank lines after tag", "```repl\n\n\nprint(3)\n```", [res("3\n")]),
        ("tag then spaces then newline then closing", "```repl  \n  \n```x", [res()]),
        ("unclosed then later block", "```repl\nprint(1)\n\nand then\n```repl\nprint(2)\n```", [res("2\n")]),
        ("tag glued to text", "```replx\nprint(1)\n```", []),
        ("closing fence with extra backticks", "```repl\nprint(1)\n````\nmore", [res("1\n")]),
        ("stdout stderr and vars", "```repl\nx=1\n```", [res("out\n", "err\n", ["x"])]),
        ("output of 20001 astral characters", "```repl\nprint(1)\n```", [res("\U0001F600" * 20001)]),
        ("two blocks, one long", "```repl\na\n```\n```repl\nb\n```", [res("x" * 25000), res("short")]),
    ]
    out = []
    for name, text, results in cases:
        blocks = [
            CodeBlock(code="", result=REPLResult(r["stdout"], r["stderr"], {v: "" for v in r["vars"]}))
            for r in results
        ]
        it = RLMIteration(prompt="", response=text, code_blocks=blocks)
        out.append(
            {
                "name": name,
                "text": text,
                "find": find_code_blocks(text),
                "results": results,
                "messages": [[m["role"], m["content"]] for m in format_iteration(it, 20000)],
            }
        )
    return out


GENERATORS["parse"] = _parse_cases


def _prompt_cases():
    from rlm.core.types import QueryMetadata
    from rlm.utils.prompts import RLM_SYSTEM_PROMPT, build_rlm_system_prompt, build_user_prompt

    # (name, context as Python value, or ("repeat", char, count))
    contexts = [
        ("short string", "abc"),
        ("list of strings", ["a", "bb"]),
        ("dict of strings", {"k": "v"}),
        ("million characters", ("repeat", "x", 1000000)),
        ("astral string", "a\U0001F600b"),
        ("empty list", []),
        ("empty string", ""),
        ("list of dicts with content", [{"role": "user", "content": "hello"}, {"role": "assistant", "content": "hi there"}, {"role": "user"}]),
        ("list of dicts with non-string content", [{"content": 12345}, {"content": True}, {"content": None}, {"content": [1, "a"]}, {"content": 1.5}, {"content": 1e16}, {"content": {"k": "caf\u00e9"}}]),
        ("list of dicts without content", [{"a": 1}, {"b": "caf\u00e9", "c": [1, 2.5, None]}]),
        ("list of dicts, content only in a later item", [{"a": 1}, {"content": "xyz"}]),
        ("list of lists", [[1, 2, 3], ["a"], []]),
        ("list of dicts, then a list", [{"a": 1}, [1, 2]]),
        ("dict with non-string values", {"n": 12345, "f": 1.5, "t": True, "z": None, "l": [1, "a"], "d": {"x": "\u00e9\U0001F600"}, "s": "abcd"}),
        ("empty dict", {}),
    ]
    shapes = []
    initial = []
    for name, ctx in contexts:
        if isinstance(ctx, tuple):
            value = ctx[1] * ctx[2]
            enc = {"contextRepeat": [ctx[1], ctx[2]]}
        else:
            value = ctx
            enc = {"contextJson": json.dumps(ctx, ensure_ascii=True)}
        qm = QueryMetadata(value)
        shapes.append({"name": name, **enc, "type": qm.context_type, "length": qm.context_total_length})
        for root in (None, "What is the answer?", "Caf\u00e9 \u4e16\u754c: {braces} and `ticks`"):
            if root is not None and name not in ("short string", "million characters", "dict with non-string values"):
                continue
            msgs = build_rlm_system_prompt(RLM_SYSTEM_PROMPT, qm, None, root, True)
            initial.append({"name": name, **enc, "root": root, "messages": [[m["role"], m["content"]] for m in msgs]})
    system = build_rlm_system_prompt(RLM_SYSTEM_PROMPT, QueryMetadata("abc"), None, None, True)[0]["content"]
    (ROOT / "fisher-rlm" / "data" / "system-prompt.txt").write_text(system, encoding="utf-8", newline="")
    turns = []
    for i in (0, 1, 29):
        m = build_user_prompt("What is the answer?", i, 1, 0, 30)
        turns.append({"iteration": i, "maxIterations": 30, "message": [m["role"], m["content"]]})
    m = build_user_prompt(None, 0, 1, 0, 5)
    turns.append({"iteration": 0, "maxIterations": 5, "message": [m["role"], m["content"]]})
    default = [
        "assistant",
        "Please provide a final answer to the user's question based on the information provided.",
    ]
    return {"shapes": shapes, "initial": initial, "turns": turns, "default": default}


GENERATORS["prompt"] = _prompt_cases


def _log_entries():
    import tempfile

    from rlm.core.types import (
        CodeBlock,
        REPLResult,
        RLMChatCompletion,
        RLMIteration,
        RLMMetadata,
        UsageSummary,
    )
    from rlm.logger.rlm_logger import RLMLogger

    def call(model, prompt, response):
        return RLMChatCompletion(
            root_model=model,
            prompt=prompt,
            response=response,
            usage_summary=UsageSummary(model_usage_summaries={}),
            execution_time=0.0,
        )

    meta = RLMMetadata(
        root_model="test/root",
        max_depth=1,
        max_iterations=30,
        backend="openrouter",
        backend_kwargs={"model_name": "test/root"},
        environment_type="local",
        environment_kwargs={},
        other_backends=["openrouter"],
    )
    blocks = [
        CodeBlock(
            code="x = llm_query('hi')\nprint(x)",
            result=REPLResult(
                stdout="hello\n",
                stderr="",
                locals={"x": "str", "n": "int"},
                execution_time=0.0,
                rlm_calls=[
                    call("test/sub", "hi", "hello"),
                    call("test/root", "why? caf\u00e9", "because"),
                ],
                final_answer=None,
            ),
        ),
        CodeBlock(
            code="1/0",
            result=REPLResult(
                stdout="",
                stderr="\nZeroDivisionError: division by zero",
                locals={},
                execution_time=0.0,
                rlm_calls=[],
                final_answer="42",
            ),
        ),
    ]
    it = RLMIteration(
        prompt=[
            {"role": "system", "content": "sys"},
            {"role": "user", "content": "go \u4e16\u754c"},
        ],
        response="Trying.\n```repl\nx = 1\n```",
        code_blocks=blocks,
        final_answer="42",
        iteration_time=1.5,
    )
    with tempfile.TemporaryDirectory() as d:
        logger = RLMLogger(log_dir=d)
        logger.log_metadata(meta)
        logger.log(it)
        lines = pathlib.Path(logger.log_file_path).read_text(encoding="utf-8").splitlines()
    out = []
    for line in lines:
        e = json.loads(line)
        assert isinstance(e["timestamp"], str)
        e["timestamp"] = ""
        out.append(e)
    return {"metadata": out[0], "iteration": out[1]}


GENERATORS["log"] = _log_entries


def main(argv):
    names = argv[1:]
    if not names:
        sys.exit("usage: goldens.py <name>...  (known: %s)" % (", ".join(sorted(GENERATORS)) or "none yet"))
    unknown = [n for n in names if n not in GENERATORS]
    if unknown:
        sys.exit("goldens.py: unknown generator(s): %s (known: %s)" % (", ".join(unknown), ", ".join(sorted(GENERATORS)) or "none yet"))
    OUT.mkdir(parents=True, exist_ok=True)
    for n in names:
        path = OUT / f"{n}.json"
        path.write_text(json.dumps(GENERATORS[n](), indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
        print("wrote", path.relative_to(ROOT))


if __name__ == "__main__":
    main(sys.argv)
