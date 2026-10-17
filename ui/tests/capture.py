"""Record the read routes' JSON from the fixture server for the page tests.

python3 -I ui/tests/capture.py SERVER_BINARY OUT.json

Starts `SERVER_BINARY serve 0` (fisher-ui's fixture server), reads the URL it
prints, and GETs every route the page reads with the cookie: the session
list (unfiltered and each single filter value), each session and its
context, an unknown session, the run list and each bench (bench h until its
gate is no longer pending, keeping each distinct answer in order), the search
runs, and the goal board's detail and whole event stream of four of them. The stub
server (stub-server.mjs) serves these answers to the Playwright tests.
"""

import json
import re
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

BENCHES = ["c3", "clm4", "d", "g", "h", "w", "rlm"]
SEARCH_RUNS = ["s.20261016-090000", "s.20261016-080000", "g.graph.5", "h.auto.4", "w.B.4", "w.C.4"]


def main():
    binary, out = sys.argv[1], sys.argv[2]
    proc = subprocess.Popen([binary, "serve", "0"], stdout=subprocess.PIPE, stdin=subprocess.DEVNULL, text=True)
    try:
        line = proc.stdout.readline()
        m = re.search(r"http://127\.0\.0\.1:(\d+)/\?t=([0-9a-f]+)", line)
        if not m:
            sys.exit("no URL from the fixture server: %r" % line)
        port, token = m.group(1), m.group(2)
        base = "http://127.0.0.1:%s" % port

        def get(path):
            req = urllib.request.Request(base + path, headers={"Cookie": "fisher_ui_" + str(port) + "=" + token})
            try:
                with urllib.request.urlopen(req) as r:
                    return {"status": r.status, "body": json.loads(r.read())}
            except urllib.error.HTTPError as e:
                body = e.read()
                try:
                    return {"status": e.code, "body": json.loads(body)}
                except ValueError:
                    return {"status": e.code, "text": body.decode("utf-8", "replace")}

        routes = {}

        def record(path):
            routes[path] = [get(path)]
            return routes[path][0]

        record("/api/ping")
        sessions = record("/api/sessions")["body"]
        for key in ["project", "prover", "model", "strategy"]:
            for v in sorted({s[key] for s in sessions if s[key]}):
                record("/api/sessions?" + urllib.parse.urlencode({key: v}))
        for s in sessions:
            record("/api/sessions/" + urllib.parse.quote(s["id"], safe=""))
            record("/api/context/" + urllib.parse.quote(s["id"], safe=""))
        record("/api/sessions/nope")
        record("/api/context/nope")
        record("/api/runs")
        for b in BENCHES:
            path = "/api/runs/" + b
            seen = [get(path)]
            deadline = time.time() + 60
            while seen[-1]["body"].get("gateStatus") == "pending" and time.time() < deadline:
                time.sleep(0.5)
                r = get(path)
                if r != seen[-1]:
                    seen.append(r)
            routes[path] = seen
        record("/api/runs/zz")
        # the goal board: the search runs, and for the runs the page tests
        # open, the detail and the whole event stream (as text)
        record("/api/search")
        for run in SEARCH_RUNS:
            record("/api/search/" + run)
            req = urllib.request.Request(base + "/api/search/" + run + "/events", headers={"Cookie": "fisher_ui_" + str(port) + "=" + token})
            with urllib.request.urlopen(req, timeout=30) as r:
                routes["/api/search/" + run + "/events"] = [{"status": r.status, "text": r.read().decode("utf-8")}]
        # without the cookie: what a page that was not given the URL sees
        req = urllib.request.Request(base + "/api/sessions")
        try:
            urllib.request.urlopen(req)
            unauth = {"status": 200}
        except urllib.error.HTTPError as e:
            unauth = {"status": e.code, "text": e.read().decode("utf-8", "replace")}
        with open(out, "w") as f:
            json.dump({"token": token, "unauthorized": unauth, "routes": routes}, f, indent=1, sort_keys=True)
            f.write("\n")
        print("captured %d routes" % len(routes))
    finally:
        proc.terminate()
        proc.wait(10)


if __name__ == "__main__":
    main()
