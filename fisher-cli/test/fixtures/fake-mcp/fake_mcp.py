"""A fake prover MCP server for Fisher's native toolset tests (stdlib only).

  python3 fake_mcp.py --tools TOOLS.json [--script SCRIPT.json] [--log LOG]
                      [--crash-after N] [--hang]

It speaks newline-delimited JSON-RPC on stdin and stdout:

  initialize   the client's protocol version, tools and resources capabilities
  tools/list   the tools in TOOLS.json (schemas recorded from Rocq-MCP or
               Isabelle-MCP)
  tools/call   the reply SCRIPT.json gives for the tool's name, else its "*"
               entry, else "ok: <tool>". A reply is {"text": str,
               "structured": object?, "error": bool?}. A tool whose recorded
               schema has an outputSchema gets structuredContent (the
               script's, or {"ok": true}).

Modes:
  --crash-after N  exit (status 1, no reply) when call N+1 arrives; each
                   process counts its own calls
  --hang           never answer tools/call

LOG gets one JSON line per process start ({"start": pid, "cwd": ...}) and
per call ({"call": name, "arguments": ...}).
"""
import json
import os
import sys
import time


def arg(name, default=None):
    if name in sys.argv:
        return sys.argv[sys.argv.index(name) + 1]
    return default


TOOLS = json.load(open(arg("--tools")))
SCRIPT = json.load(open(arg("--script"))) if arg("--script") else {}
LOG_PATH = arg("--log")
CRASH_AFTER = int(arg("--crash-after", "-1"))
HANG = "--hang" in sys.argv
SCHEMAS = {t["name"]: t.get("outputSchema") for t in TOOLS}


def log(obj):
    if LOG_PATH:
        with open(LOG_PATH, "a") as f:
            f.write(json.dumps(obj, sort_keys=True) + "\n")


def send(obj):
    sys.stdout.write(json.dumps(obj) + "\n")
    sys.stdout.flush()


def call_result(name):
    reply = SCRIPT.get(name, SCRIPT.get("*", {"text": "ok: " + name}))
    result = {"content": [{"type": "text", "text": reply.get("text", "")}],
              "isError": bool(reply.get("error", False))}
    if SCHEMAS.get(name) is not None:
        result["structuredContent"] = reply.get("structured", {"ok": True})
    return result


def main():
    log({"start": os.getpid(), "cwd": os.getcwd()})
    calls = 0
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        msg = json.loads(line)
        method, rid = msg.get("method"), msg.get("id")
        if rid is None:
            continue
        if method == "initialize":
            send({"jsonrpc": "2.0", "id": rid, "result": {
                "protocolVersion": msg["params"]["protocolVersion"],
                "capabilities": {"tools": {"listChanged": False}, "resources": {}, "prompts": {}},
                "serverInfo": {"name": "fake-prover", "version": "1"}}})
        elif method == "tools/list":
            send({"jsonrpc": "2.0", "id": rid, "result": {"tools": TOOLS}})
        elif method == "tools/call":
            name = msg["params"]["name"]
            log({"call": name, "arguments": msg["params"].get("arguments")})
            if CRASH_AFTER >= 0 and calls >= CRASH_AFTER:
                os._exit(1)
            calls += 1
            if HANG:
                while True:
                    time.sleep(3600)
            send({"jsonrpc": "2.0", "id": rid, "result": call_result(name)})
        elif method == "ping":
            send({"jsonrpc": "2.0", "id": rid, "result": {}})
        else:
            send({"jsonrpc": "2.0", "id": rid, "error": {"code": -32601, "message": "Method not found"}})


main()
