"""A scripted MCP server for Fisher's B2 goldens and tests (stdlib only).

  python3 fake_server.py SCENARIO.json LOG [--http PORT] [--log-cwd]

Without --http it speaks newline-delimited JSON-RPC on stdin/stdout; with
it, streamable HTTP on 127.0.0.1:PORT. Every message it receives is
appended to LOG as one JSON line {"in": message} (HTTP adds the method,
path and the headers that carry meaning), and a stdio server first logs
{"env": sorted names of its environment variables} (and, with --log-cwd,
{"cwd": its working directory}).

The scenario is {"tools": [...], "next_cursor": str?, "protocol": str?,
"handlers": {KEY: [action, ...]}} where KEY is a method name, or
"tools/call:<tool name>", and the actions run in order:
  {"result": ...} / {"error": ...}   reply to the request
  {"sleep": seconds}
  {"exit": code}                     exit immediately
  {"raw": "text"}                    write a raw line (stdio only)
  {"request": {...}}                 send a server->client request, wait
                                     for its response, then reply with
                                     {"content":[{"type":"text","text": <json of that response>}]}
  {"sse": true}                      (HTTP) answer the next result as an
                                     event stream instead of JSON
  {"status": code}                   (HTTP) answer with a bare status
"""
import json
import os
import sys
import threading
import time

SCENARIO = json.load(open(sys.argv[1]))
LOG = open(sys.argv[2], "a")
LOCK = threading.Lock()


def log(obj):
    with LOCK:
        LOG.write(json.dumps(obj, sort_keys=True) + "\n")
        LOG.flush()


# set by the interpreter or its launcher (macOS's /usr/bin/python3 shim),
# not by the client
LAUNCHER_ENV = {"LC_CTYPE", "CPATH", "LIBRARY_PATH", "MANPATH", "SDKROOT", "__CF_USER_TEXT_ENCODING"}
if "--http" not in sys.argv:
    log({"env": sorted(k for k in os.environ if k not in LAUNCHER_ENV)})
    if "--log-cwd" in sys.argv:
        log({"cwd": os.getcwd()})


def default_result(method, msg):
    if method == "initialize":
        return {"protocolVersion": SCENARIO.get("protocol") or msg["params"]["protocolVersion"],
                "capabilities": SCENARIO.get("capabilities", {"tools": {}}),
                "serverInfo": {"name": "fake", "version": "1"}}
    if method == "tools/list":
        r = {"tools": SCENARIO.get("tools", [])}
        if SCENARIO.get("next_cursor"):
            r["nextCursor"] = SCENARIO["next_cursor"]
        return r
    if method == "ping":
        return {}
    return None


def actions_for(msg):
    method = msg.get("method")
    key = method
    if method == "tools/call":
        key = "tools/call:" + msg["params"]["name"]
    return SCENARIO.get("handlers", {}).get(key)


# ------------------------------------------------------------------ stdio
def stdio():
    out = sys.stdout

    def send(obj):
        out.write(json.dumps(obj) + "\n")
        out.flush()

    lines = iter(sys.stdin.readline, "")
    for line in lines:
        try:
            msg = json.loads(line)
        except ValueError:
            log({"in_raw": line})
            continue
        log({"in": msg})
        mid = msg.get("id")
        method = msg.get("method")
        if method is None:
            continue  # a response we did not ask for
        acts = actions_for(msg)
        if acts is None:
            if mid is None:
                continue
            res = default_result(method, msg)
            if res is None:
                send({"jsonrpc": "2.0", "id": mid, "error": {"code": -32601, "message": "Method not found"}})
            else:
                send({"jsonrpc": "2.0", "id": mid, "result": res})
            continue
        for act in acts:
            if "sleep" in act:
                time.sleep(act["sleep"])
            elif "exit" in act:
                LOG.flush()
                os._exit(act["exit"])
            elif "raw" in act:
                out.write(act["raw"] + "\n")
                out.flush()
            elif "result" in act:
                send({"jsonrpc": "2.0", "id": mid, "result": act["result"]})
            elif "error" in act:
                send({"jsonrpc": "2.0", "id": mid, "error": act["error"]})
            elif "request" in act:
                req = dict(act["request"], jsonrpc="2.0")
                send(req)
                for reply_line in lines:
                    reply = json.loads(reply_line)
                    log({"in": reply})
                    if reply.get("id") == req["id"] and "method" not in reply:
                        break
                    if reply.get("method") and reply.get("id") is not None:
                        send({"jsonrpc": "2.0", "id": reply["id"], "error": {"code": -32603, "message": "busy"}})
                send({"jsonrpc": "2.0", "id": mid,
                      "result": {"content": [{"type": "text", "text": json.dumps(reply, sort_keys=True)}]}})


# ------------------------------------------------------------------- http
KEEP = ("accept", "content-type", "mcp-session-id", "mcp-protocol-version", "last-event-id", "cache-control",
        "authorization", "x-custom")


def http(port):
    from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
    inits = [0]

    class H(BaseHTTPRequestHandler):
        protocol_version = "HTTP/1.1"

        def entry(self, body=None):
            hs = {}
            for k, v in self.headers.items():
                k = k.lower()
                if k in KEEP or k.startswith("x-"):
                    hs.setdefault(k, []).append(v)
            e = {"http": self.command, "path": self.path, "headers": hs}
            if body is not None:
                e["in"] = body
            log(e)

        def reply(self, code, obj=None, extra=None, sse=False):
            if obj is None:
                data, ctype = b"", None
            elif sse:
                data = ("event: message\ndata: " + json.dumps(obj) + "\n\n").encode()
                ctype = "text/event-stream"
            else:
                data, ctype = json.dumps(obj).encode(), "application/json"
            self.send_response(code)
            if ctype:
                self.send_header("Content-Type", ctype)
            for k, v in (extra or {}).items():
                self.send_header(k, v)
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def do_GET(self):
            self.entry()
            self.reply(405)

        def do_DELETE(self):
            self.entry()
            self.reply(200)

        def do_POST(self):
            body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
            self.entry(body)
            mid = body.get("id")
            method = body.get("method")
            if mid is None or method is None:
                return self.reply(202)
            extra = {}
            if method == "initialize":
                inits[0] += 1
                extra["Mcp-Session-Id"] = f"sess-{inits[0]}"
            acts = actions_for(body)
            if acts is None:
                res = default_result(method, body)
                if res is None:
                    return self.reply(200, {"jsonrpc": "2.0", "id": mid, "error": {"code": -32601, "message": "Method not found"}}, extra)
                return self.reply(200, {"jsonrpc": "2.0", "id": mid, "result": res}, extra)
            sse = False
            for act in acts:
                if "sleep" in act:
                    time.sleep(act["sleep"])
                elif "sse" in act:
                    sse = True
                elif "status" in act:
                    return self.reply(act["status"], {"detail": "scripted"})
                elif "result" in act:
                    return self.reply(200, {"jsonrpc": "2.0", "id": mid, "result": act["result"]}, extra, sse)
                elif "error" in act:
                    return self.reply(200, {"jsonrpc": "2.0", "id": mid, "error": act["error"]}, extra, sse)
            self.reply(202)

        def log_message(self, *a):
            pass

    srv = ThreadingHTTPServer(("127.0.0.1", port), H)
    print("ready", flush=True)
    srv.serve_forever()


if "--http" in sys.argv:
    http(int(sys.argv[sys.argv.index("--http") + 1]))
else:
    stdio()
