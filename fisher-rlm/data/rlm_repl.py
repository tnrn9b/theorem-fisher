# Ported from rlm/environments/local_repl.py at
# d04208afbad29ca675ab13478c40ee8bebc84bfe of github.com/alexzhang13/rlm
# (MIT, Copyright (c) 2026 Alex Zhang; the license is in fisher-rlm/NOTICE).
#
# The Python side of one RLM run. One process holds the REPL namespace and
# speaks JSON lines to Fisher (Haskell), which makes every model call. The
# protocol runs on the process's own stdin and stdout, duplicated at start;
# descriptors 0 and 1 are then pointed at /dev/null so nothing the model's
# code writes there can corrupt it. Standard library only, Python 3.9+.
#
# in : {"op":"load","context":V}   -> out {"op":"ok"}
# in : {"op":"exec","code":S}      -> out zero or more
#        {"op":"call","kind":"llm"|"rlm","prompts":[S],"model":S|null}
#        each answered by in {"op":"results","results":[S]}, then out
#        {"op":"result","stdout":S,"stderr":S,"vars":[S],"types":[S],"final":S|null}
# in : {"op":"close"}              -> exit 0

import io
import json
import os
import shutil
import sys
import tempfile
import threading
import uuid
from contextlib import contextmanager

RESERVED_TOOL_NAMES = frozenset(
    {
        "llm_query",
        "llm_query_batched",
        "rlm_query",
        "rlm_query_batched",
        "SHOW_VARS",
        "answer",
        "context",
    }
)


class _AnswerDict(dict):
    """REPL-visible dict where ``answer["ready"] = True`` signals completion.

    Behaves exactly like ``dict`` for the model, but invokes ``on_ready`` the
    first time ``ready`` flips truthy. The callback receives the current
    ``content``, lets the env capture it, and the next exec reports it as
    ``final``.
    """

    def __init__(self, on_ready=None):
        super().__init__()
        super().__setitem__("content", "")
        super().__setitem__("ready", False)
        self._on_ready = on_ready

    def __setitem__(self, key, value):
        super().__setitem__(key, value)
        if key == "ready" and value and self._on_ready is not None:
            try:
                self._on_ready(self.get("content", ""))
            except Exception:
                pass


# Safe builtins - blocks dangerous operations like eval/exec/input
_SAFE_BUILTINS = {
    # Core types and functions
    "print": print,
    "len": len,
    "str": str,
    "int": int,
    "float": float,
    "list": list,
    "dict": dict,
    "set": set,
    "tuple": tuple,
    "bool": bool,
    "type": type,
    "isinstance": isinstance,
    "issubclass": issubclass,
    "enumerate": enumerate,
    "zip": zip,
    "map": map,
    "filter": filter,
    "sorted": sorted,
    "reversed": reversed,
    "range": range,
    "min": min,
    "max": max,
    "sum": sum,
    "abs": abs,
    "round": round,
    "any": any,
    "all": all,
    "pow": pow,
    "divmod": divmod,
    "chr": chr,
    "ord": ord,
    "hex": hex,
    "bin": bin,
    "oct": oct,
    "repr": repr,
    "ascii": ascii,
    "format": format,
    "hash": hash,
    "id": id,
    "iter": iter,
    "next": next,
    "slice": slice,
    "callable": callable,
    "hasattr": hasattr,
    "getattr": getattr,
    "setattr": setattr,
    "delattr": delattr,
    "dir": dir,
    "vars": vars,
    "bytes": bytes,
    "bytearray": bytearray,
    "memoryview": memoryview,
    "complex": complex,
    "object": object,
    "super": super,
    "property": property,
    "staticmethod": staticmethod,
    "classmethod": classmethod,
    "__import__": __import__,
    "open": open,
    # Exceptions
    "Exception": Exception,
    "BaseException": BaseException,
    "ValueError": ValueError,
    "TypeError": TypeError,
    "KeyError": KeyError,
    "IndexError": IndexError,
    "AttributeError": AttributeError,
    "FileNotFoundError": FileNotFoundError,
    "OSError": OSError,
    "IOError": IOError,
    "RuntimeError": RuntimeError,
    "NameError": NameError,
    "ImportError": ImportError,
    "StopIteration": StopIteration,
    "AssertionError": AssertionError,
    "NotImplementedError": NotImplementedError,
    "ArithmeticError": ArithmeticError,
    "LookupError": LookupError,
    "Warning": Warning,
    # Blocked
    "input": None,
    "eval": None,
    "exec": None,
    "compile": None,
    "globals": None,
    "locals": None,
}

# The script's own use of exec below is not affected by the table above.
_exec = exec


class _Channel:
    """JSON lines to and from Fisher on the duplicated stdin and stdout."""

    def __init__(self, proto_in, proto_out):
        self._in = proto_in
        self._out = proto_out

    def send(self, obj):
        self._out.write(json.dumps(obj) + "\n")
        self._out.flush()

    def recv(self):
        """The next message as a dict, or None at end of input."""
        while True:
            line = self._in.readline()
            if line == "":
                return None
            if line.strip() == "":
                continue
            return json.loads(line)


class Repl:
    """Persistent Python namespace, ported from LocalREPL."""

    def __init__(self, channel):
        self.channel = channel
        # Code may call llm_query from several threads; one call is on the
        # channel at a time, so each thread reads its own reply.
        self._call_lock = threading.Lock()
        self.temp_dir = tempfile.mkdtemp(prefix="repl_env_%s_" % uuid.uuid4())
        self.globals = {
            "__builtins__": _SAFE_BUILTINS.copy(),
            "__name__": "__main__",
        }
        self.locals = {}
        # Captured the first time the model sets ``answer["ready"] = True``.
        self._last_final_answer = None

        self.globals["SHOW_VARS"] = self._show_vars
        self.globals["llm_query"] = self._llm_query
        self.globals["llm_query_batched"] = self._llm_query_batched
        self.globals["rlm_query"] = self._rlm_query
        self.globals["rlm_query_batched"] = self._rlm_query_batched
        self.locals["answer"] = _AnswerDict(on_ready=self._capture_answer)

    def _capture_answer(self, content):
        self._last_final_answer = str(content)

    def _show_vars(self):
        """Show all available variables in the REPL environment."""
        available = {
            k: type(v).__name__
            for k, v in self.locals.items()
            if not k.startswith("_") and k != "answer"
        }
        if not available:
            return "No variables created yet. Use ```repl``` blocks to create variables."
        return "Available variables: %s" % (available,)

    # -- calls into Fisher ---------------------------------------------------

    def _call(self, kind, prompts, model):
        """Send one call message; return the list of result strings."""
        with self._call_lock:
            self.channel.send(
                {"op": "call", "kind": kind, "prompts": list(prompts), "model": model}
            )
            reply = self.channel.recv()
        if reply is None:
            raise RuntimeError("Fisher closed the connection")
        if not isinstance(reply, dict) or reply.get("op") != "results":
            raise RuntimeError("unexpected reply %r" % (reply,))
        results = reply.get("results")
        if (
            not isinstance(results, list)
            or len(results) != len(prompts)
            or not all(isinstance(x, str) for x in results)
        ):
            raise RuntimeError("malformed results %r" % (results,))
        return results

    def _query(self, kind, prompt, model):
        try:
            return self._call(kind, [prompt], model)[0]
        except Exception as e:
            return "Error: LM query failed - %s" % (e,)

    def _query_batched(self, kind, prompts, model):
        try:
            return self._call(kind, prompts, model)
        except Exception as e:
            return ["Error: LM query failed - %s" % (e,)] * len(prompts)

    def _llm_query(self, prompt, model=None):
        """Query the LM with a single plain completion (no REPL, no recursion)."""
        return self._query("llm", prompt, model)

    def _llm_query_batched(self, prompts, model=None):
        """Query the LM with several prompts at once; results keep their order."""
        return self._query_batched("llm", prompts, model)

    def _rlm_query(self, prompt, model=None):
        """Spawn a recursive RLM sub-call (Fisher applies the depth fallback)."""
        return self._query("rlm", prompt, model)

    def _rlm_query_batched(self, prompts, model=None):
        """Spawn recursive RLM sub-calls for several prompts at once."""
        return self._query_batched("rlm", prompts, model)

    # -- context -------------------------------------------------------------

    def load_context(self, context_payload):
        """Load context into the environment as context_0 (and 'context' alias)."""
        var_name = "context_0"
        if isinstance(context_payload, str):
            context_path = os.path.join(self.temp_dir, "context_0.txt")
            with open(context_path, "w") as f:
                f.write(context_payload)
            self.execute_code(
                "with open(r'%s', 'r') as f:\n    %s = f.read()" % (context_path, var_name)
            )
        else:
            context_path = os.path.join(self.temp_dir, "context_0.json")
            with open(context_path, "w") as f:
                json.dump(context_payload, f)
            self.execute_code(
                "import json\nwith open(r'%s', 'r') as f:\n    %s = json.load(f)"
                % (context_path, var_name)
            )
        # Alias context_0 as 'context' for backward compatibility
        self.execute_code("context = %s" % var_name)

    # -- execution -----------------------------------------------------------

    @contextmanager
    def _capture_output(self):
        old_stdout, old_stderr = sys.stdout, sys.stderr
        stdout_buf, stderr_buf = io.StringIO(), io.StringIO()
        try:
            sys.stdout, sys.stderr = stdout_buf, stderr_buf
            yield stdout_buf, stderr_buf
        finally:
            sys.stdout, sys.stderr = old_stdout, old_stderr

    @contextmanager
    def _temp_cwd(self):
        """Temporarily change to temp directory for execution."""
        old_cwd = os.getcwd()
        try:
            os.chdir(self.temp_dir)
            yield
        finally:
            os.chdir(old_cwd)

    def _restore_scaffold(self):
        """Restore scaffold names so overwrites (e.g. context = 'x') don't persist."""
        for name in RESERVED_TOOL_NAMES:
            if name == "llm_query":
                self.globals["llm_query"] = self._llm_query
            elif name == "llm_query_batched":
                self.globals["llm_query_batched"] = self._llm_query_batched
            elif name == "rlm_query":
                self.globals["rlm_query"] = self._rlm_query
            elif name == "rlm_query_batched":
                self.globals["rlm_query_batched"] = self._rlm_query_batched
            elif name == "SHOW_VARS":
                self.globals["SHOW_VARS"] = self._show_vars
            elif name == "answer":
                current = self.locals.get("answer")
                # If the model rebound ``answer`` to a plain dict, the
                # _AnswerDict callback never fired; capture content here if
                # ``ready=True``, then re-wrap so the next cell signals.
                if not isinstance(current, _AnswerDict):
                    replacement = _AnswerDict(on_ready=self._capture_answer)
                    if isinstance(current, dict):
                        for k, v in current.items():
                            dict.__setitem__(replacement, k, v)
                        if current.get("ready") and self._last_final_answer is None:
                            self._last_final_answer = str(current.get("content", ""))
                    self.locals["answer"] = replacement
            elif name == "context" and "context_0" in self.locals:
                self.locals["context"] = self.locals["context_0"]

    def execute_code(self, code):
        """Run code in the persistent namespace; return (stdout, stderr, final)."""
        with self._capture_output() as (stdout_buf, stderr_buf), self._temp_cwd():
            try:
                combined = {**self.globals, **self.locals}
                _exec(code, combined, combined)

                # Update locals with new variables
                for key, value in combined.items():
                    if key not in self.globals and not key.startswith("_"):
                        self.locals[key] = value

                self._restore_scaffold()

                stdout = stdout_buf.getvalue()
                stderr = stderr_buf.getvalue()
            except Exception as e:
                stdout = stdout_buf.getvalue()
                stderr = stderr_buf.getvalue() + "\n%s: %s" % (type(e).__name__, e)

        final_answer = self._last_final_answer
        self._last_final_answer = None
        return stdout, stderr, final_answer

    def important_vars(self):
        """Names and type names of the simple-typed, non-underscore locals."""
        names, types = [], []
        for key, value in self.locals.items():
            if key.startswith("_"):
                continue
            if isinstance(value, (str, int, float, bool, list, dict, tuple)):
                names.append(key)
                types.append(type(value).__name__)
        return names, types

    def cleanup(self):
        try:
            shutil.rmtree(self.temp_dir)
        except Exception:
            pass


def serve(channel):
    repl = Repl(channel)
    try:
        while True:
            try:
                msg = channel.recv()
            except ValueError as e:
                channel.send({"op": "error", "message": "bad JSON: %s" % (e,)})
                continue
            if msg is None:
                return
            op = msg.get("op") if isinstance(msg, dict) else None
            if op == "load":
                repl.load_context(msg.get("context"))
                channel.send({"op": "ok"})
            elif op == "exec":
                stdout, stderr, final = repl.execute_code(msg.get("code", ""))
                names, types = repl.important_vars()
                channel.send(
                    {
                        "op": "result",
                        "stdout": stdout,
                        "stderr": stderr,
                        "vars": names,
                        "types": types,
                        "final": final,
                    }
                )
            elif op == "close":
                return
            else:
                channel.send({"op": "error", "message": "unknown op: %r" % (op,)})
    finally:
        repl.cleanup()


def main():
    proto_in = os.fdopen(os.dup(0), "r", encoding="utf-8", newline="\n")
    proto_out = os.fdopen(os.dup(1), "w", encoding="utf-8", newline="\n")
    devnull = os.open(os.devnull, os.O_RDWR)
    os.dup2(devnull, 0)
    os.dup2(devnull, 1)
    os.close(devnull)
    serve(_Channel(proto_in, proto_out))
    return 0


if __name__ == "__main__":
    sys.exit(main())
