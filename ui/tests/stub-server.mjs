// The page tests' server where fisher ui cannot be built (no warp): ui/dist
// and the answers the real routes gave on the Haskell fixtures, recorded by
// capture.py into fixture-api.json. It keeps the real server's rules the
// page meets: GET /?t=TOKEN sets the cookie and redirects to /, /api needs
// the cookie (else 401), unknown /api paths are 404, and any other path is
// a file of ui/dist or index.html.
//
// node tests/stub-server.mjs PORT
// A route recorded with several answers (bench h: pending, then the gate
// line) gives them in order and then repeats the last.
//
// A test can ask for a state the fixtures do not have with the cookie
// fisher_stub=NAME; that scenario's routes then answer in place of the
// recorded ones (each in order from its first answer, per request):
//   empty         no sessions at all
//   gate-failed   bench h whose report.py failed
//   poll-fails    bench h pending, then a 500 on the next poll
//
// The live routes (POST /api/sessions, /api/tags/:tag, and a session's
// live, events, stop and message) run one scripted run in memory, as the
// fixture server's fake fisher does: three events 0.3 s apart, then each
// message back as an inbox event, until it is stopped (then a made-up end,
// status stopped). The events stream as the real server sends them (id:
// is the number; Last-Event-ID or ?after= resumes; 204 past the end).
//
// The goal board's reads are the recorded ones; a run's events stream
// replays its recorded stream from Last-Event-ID or ?from= (a byte offset,
// as the real server's ids are). POST /api/search starts one scripted flat
// run in memory, as the fixture server's fake fisher search does (Demo.a's
// events over about four seconds, then its report); apply answers as the
// recorded detail's apply rule says (a second apply of a run is refused as
// a file changed since the run started, unless forced); stop stops the
// scripted run.
//
// The New project view's routes (GET /api/templates, POST /api/create and
// a create's detail, events, stop and retry) are scripted in memory: the
// templates are lean and lean-mathlib (built-in, trusted) and custom (a
// configured one at revision rev-custom-1, not trusted until a create
// confirms that revision: confirm is the revision a 409 gave). The
// variables are checked against their patterns as the real route does. A
// create writes, as lines of its events file, two steps (fetch, build)
// 0.3 s apart, the check and ready; a folder whose path has "fail" in it
// fails its build on the first try, and a retry (fisher project build)
// then appends the build again, the check and ready. A folder whose path
// has "changed" in it stops at once the first time, as the command line
// does when the template changed since it was confirmed (nothing written),
// and its retry answers 409 with the steps, a new revision and the request
// to send again. A scratch's root is /tmp/stub-scratch/20991231-000000-abcd
// and its file Scratch.lean.
//
// The settings (GET and POST /api/settings) are kept in memory, starting
// as the fixture server's home has them (model example/model-a, provider
// openrouter, context clm, autoformalize model example/model-b, an
// OPENROUTER_API_KEY ending in 1234, TERMINAL_SSH_HOST box.local); a key is answered as its last 4
// characters only, as the real route answers.
//
// The Settings page's Claude Code routes (GET /api/claude-auth, POST
// /api/claude-auth/login, a login's output, input and stop, and POST
// /api/claude-auth/logout) script the claude CLI in memory: logged out at
// first; a login prints a line and a URL at once and waits at a prompt
// (no newline) for a code; a code logs in as x@y.z (max, or the console
// auth method with console: true), a stop ends it (exit 143). The body is
// checked as the real route checks it. The scenario claude-missing
// answers as with no claude installed.
//
// The proof canvas's routes keep one step log in memory, the Lean
// session's (as fisher-ui's fixture: three changes to Main.lean): a check
// counts sorry as a hole and BAD as an error; a hand back appends a hand
// step and answers as a resume would (no run of the stub drives it).

import { createServer } from "node:http";
import { readFileSync, existsSync, statSync } from "node:fs";
import { dirname, extname, join, normalize } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const dist = join(here, "..", "dist");
const fixture = JSON.parse(readFileSync(join(here, "fixture-api.json"), "utf8"));
const token = fixture.token;
const port = Number(process.argv[2] ?? 8733);

const filterKeys = ["project", "prover", "model", "strategy"];

/** One spelling per request: the path and its filters in a fixed order. */
function canonical(url) {
  const u = new URL(url, "http://127.0.0.1");
  const q = new URLSearchParams();
  for (const k of filterKeys) {
    const v = u.searchParams.get(k);
    if (v) q.set(k, v);
  }
  const s = q.toString();
  return u.pathname + (s ? `?${s}` : "");
}

const routes = new Map();
for (const [k, v] of Object.entries(fixture.routes)) routes.set(canonical(k), { answers: v, next: 0 });

const h = fixture.routes["/api/runs/h"].at(-1).body;
const scenarios = {
  empty: { "/api/sessions": [{ status: 200, body: [] }] },
  "gate-failed": { "/api/runs/h": [{ status: 200, body: { ...h, gate: null, gateStatus: "failed" } }] },
  "poll-fails": {
    "/api/runs/h": [
      { status: 200, body: { ...h, gate: null, gateStatus: "pending" } },
      { status: 500, text: "the stub failed this poll" },
    ],
  },
};
const scenarioNext = new Map();

function scenarioOf(req) {
  for (const c of (req.headers.cookie ?? "").split(/;\s*/)) if (c.startsWith("fisher_stub=")) return c.slice(12);
  return null;
}

/** The scenario's answer for this request, or null to use the recorded one. */
function fromScenario(req, path) {
  const name = scenarioOf(req);
  const answers = name && scenarios[name]?.[path];
  if (!answers) return null;
  const k = `${name} ${path}`;
  const n = scenarioNext.get(k) ?? 0;
  scenarioNext.set(k, n + 1);
  return answers[Math.min(n, answers.length - 1)];
}

const types = { ".html": "text/html; charset=utf-8", ".js": "text/javascript", ".css": "text/css", ".svg": "image/svg+xml" };

// ------------------------------------------------------------- live run
const runs = new Map(); // tag -> run
let runCount = 0;

function newRun(project, command) {
  runCount += 1;
  const tag = `stubtag${runCount}`;
  const run = { tag, session: `s_${tag}`, project, command, events: [], status: "starting", running: true, exit: null, timers: [] };
  const emit = (kind, data) => run.events.push({ seq: run.events.length + 1, kind, data });
  run.emit = emit;
  run.timers.push(setTimeout(() => (run.status = "running"), 100));
  run.timers.push(setTimeout(() => emit("tool_start", { name: "read_file", args: { path: "theories/Demo.v" } }), 300));
  run.timers.push(setTimeout(() => emit("tool_end", { name: "read_file", callId: "call_1", preview: "Theorem t", ok: true }), 600));
  run.timers.push(setTimeout(() => emit("message", { role: "assistant", text: "Reading the file first." }), 900));
  runs.set(tag, run);
  return run;
}

function runOfSession(id) {
  for (const r of runs.values()) if (r.session === id) return r;
  return null;
}

function runJson(r) {
  return {
    tag: r.tag, session: r.status === "starting" ? null : r.session, status: r.status, running: r.running,
    exit: r.exit, project: r.project, command: r.command, started: 0, log: `/tmp/${r.tag}.log`,
  };
}

function stopRun(r) {
  if (!r.running) return;
  r.status = "stopping";
  r.timers.forEach(clearTimeout);
  setTimeout(() => {
    r.running = false;
    r.status = "stopped";
    r.exit = -15;
    r.emit("end", { status: "stopped", exit: -15, made: true });
  }, 300);
}

function readBody(req) {
  return new Promise((resolve) => {
    let b = "";
    req.on("data", (c) => (b += c));
    req.on("end", () => {
      try {
        resolve(JSON.parse(b || "{}"));
      } catch {
        resolve(null);
      }
    });
  });
}

function json(res, status, body) {
  send(res, status, "application/json", JSON.stringify(body));
}

/** The live routes; true when this request was one of them. */
function live(req, res, u) {
  const parts = u.pathname.split("/").filter(Boolean).map(decodeURIComponent);
  if (req.method === "POST" && u.pathname === "/api/sessions") {
    readBody(req).then((b) => {
      if (!b || typeof b.project !== "string" || typeof b.command !== "string" || !b.command.trim().startsWith("/"))
        return json(res, 400, { error: "send {project, command} as JSON" });
      if (!b.project.startsWith("/")) return json(res, 400, { error: "the project is not an absolute path" });
      json(res, 200, { tag: newRun(b.project, b.command.trim()).tag });
    });
    return true;
  }
  if (parts[1] === "tags" && parts.length >= 3) {
    const r = runs.get(parts[2]);
    if (!r) return json(res, 404, { error: "no such run" }), true;
    if (parts.length === 3 && req.method === "GET") return json(res, 200, runJson(r)), true;
    if (parts[3] === "stop" && req.method === "POST") return stopRun(r), json(res, 202, { status: "stopping" }), true;
    return false;
  }
  if (parts[1] !== "sessions" || parts.length !== 4) return false;
  const [, , id, what] = parts;
  const r = runOfSession(id);
  if (what === "live" && req.method === "GET") {
    if (!r) return json(res, 200, { root: id, tag: null, session: id, status: null, running: false }), true;
    return json(res, 200, { ...runJson(r), root: id }), true;
  }
  if (what === "stop" && req.method === "POST") {
    if (!r || !r.running) return json(res, 409, { error: "no run of this server drives this session" }), true;
    stopRun(r);
    return json(res, 202, { status: "stopping", tag: r.tag }), true;
  }
  if (what === "message" && req.method === "POST") {
    readBody(req).then((b) => {
      const text = typeof b?.text === "string" ? b.text.trim() : "";
      if (!text) return json(res, 400, { error: "the message is empty" });
      if (!r || !r.running) return json(res, 409, { error: "no live run drives this session" });
      // delivered at the next tool boundary
      r.timers.push(setTimeout(() => r.emit("inbox", { text }), 800));
      json(res, 200, { queued: true, session: id });
    });
    return true;
  }
  if (what === "events" && req.method === "GET") {
    const header = Number(req.headers["last-event-id"]);
    const query = Number(u.searchParams.get("after"));
    let after = Number.isFinite(header) && req.headers["last-event-id"] !== undefined ? header : Number.isFinite(query) ? query : 0;
    const evs = r ? r.events : [];
    const last = evs.at(-1);
    if (last && last.kind === "end" && after >= last.seq) {
      res.writeHead(204, { "Cache-Control": "no-store" });
      res.end();
      return true;
    }
    res.writeHead(200, { "Content-Type": "text/event-stream; charset=utf-8", "Cache-Control": "no-store" });
    res.write("retry: 1000\n\n");
    const tick = () => {
      for (const e of evs.filter((x) => x.seq > after)) {
        res.write(`id: ${e.seq}\ndata: ${JSON.stringify(e)}\n\n`);
        after = e.seq;
        if (e.kind === "end") {
          clearInterval(timer);
          res.end();
          return;
        }
      }
    };
    const timer = setInterval(tick, 100);
    req.on("close", () => clearInterval(timer));
    tick();
    return true;
  }
  return false;
}

// ---------------------------------------------------------- goal board
const searches = new Map(); // run id -> scripted run
const applied = new Set(); // runs applied once: their file has changed since
let searchCount = 0;

/** SSE blocks of a recorded stream: [{id, data}] and the end's data. */
function parseStream(text) {
  const evs = [];
  let end = null;
  for (const block of text.split("\n\n")) {
    const ls = block.split("\n");
    const f = (k) => ls.filter((l) => l.startsWith(k)).map((l) => l.slice(k.length));
    const [ev] = f("event: ");
    const [id] = f("id: ");
    const [data] = f("data: ");
    if (ev === "end") end = data;
    else if (id !== undefined && data !== undefined) evs.push({ id: Number(id), data });
  }
  return { evs, end };
}

function newSearch(project, file, graph, auto, retrieval = true, library = true) {
  searchCount += 1;
  const run = `s.20991231-00000${searchCount}`;
  const r = { run, project, file, graph, auto, retrieval, library, lines: [], running: true, status: "running", report: null, timers: [] };
  const line = (o) => r.lines.push(JSON.stringify(o));
  r.timers.push(setTimeout(() => line({ event: "auto missed", goal: "Demo.a", seconds: 1, t: 1 }), 500));
  r.timers.push(setTimeout(() => line({ attempt: 1, backend: "fisher", event: "launched", goal: "Demo.a", t: 1 }), 1000));
  r.timers.push(setTimeout(() => line({ attempt: 1, backend: "fisher", event: "finished", goal: "Demo.a", outcome: "closed", t: 3 }), 3000));
  r.timers.push(setTimeout(() => line({ attempt: 1, event: "closed", goal: "Demo.a", t: 3 }), 3000));
  r.timers.push(
    setTimeout(() => {
      r.report = {
        mode: "swarm", file, project, elapsed_s: 3, goals_total: 1, goals_closed: 1, auto_requested: true, attempts: [],
        goals: [{ goal: "Demo.a", status: "closed", closedBy: "model", attempts: ["fisher"], by_attempt: 1, by_backend: "fisher", closed_at_s: 3 }],
      };
      r.running = false;
      r.status = "done";
    }, 3500),
  );
  searches.set(run, r);
  return r;
}

/** The scripted run's events as (end offset, line) pairs. */
function offsets(lines) {
  let at = 0;
  return lines.map((l) => ((at += Buffer.byteLength(l) + 1), { id: at, data: l }));
}

function searchDetail(r) {
  return {
    run: r.run, dir: `/tmp/stub-search/${r.run}`, bench: null, file: r.file, project: r.project, mode: r.graph ? "graph" : "flat",
    started: 1790000000, closed: r.report ? 1 : 0, total: 1, status: r.status, running: r.running,
    report: r.report, graph: null, whiteboards: [], goals: r.report ? r.report.goals : null, attempts: [], auto: r.auto,
    retrieval: r.retrieval, library: r.library,
    apply: { allowed: false, reason: r.report ? "the run has no base.lean" : "the run is still going" },
  };
}

function sse(req, res, u, read, done) {
  const header = req.headers["last-event-id"];
  let at = header !== undefined ? Number(header) : Number(u.searchParams.get("from") ?? 0);
  const first = read();
  if (done() !== null && !first.some((e) => e.id > at) && at > 0 && at >= (first.at(-1)?.id ?? 0)) {
    res.writeHead(204, { "Cache-Control": "no-store" });
    res.end();
    return;
  }
  res.writeHead(200, { "Content-Type": "text/event-stream; charset=utf-8", "Cache-Control": "no-store" });
  res.write("retry: 1000\n\n");
  const tick = () => {
    const end = done();
    for (const e of read().filter((x) => x.id > at)) {
      res.write(`id: ${e.id}\ndata: ${e.data}\n\n`);
      at = e.id;
    }
    if (end !== null) {
      res.write(`event: end\ndata: ${end}\n\n`);
      clearInterval(timer);
      res.end();
    }
  };
  const timer = setInterval(tick, 100);
  req.on("close", () => clearInterval(timer));
  tick();
}

/** The goal board's routes the recorded answers do not cover; true when handled. */
function searchRoutes(req, res, u) {
  const parts = u.pathname.split("/").filter(Boolean).map(decodeURIComponent);
  if (parts[1] !== "search") return false;
  if (parts.length === 2 && req.method === "POST") {
    readBody(req).then((b) => {
      if (!b || typeof b.project !== "string" || typeof b.file !== "string") return json(res, 400, { error: "send {project, file, graph, auto} as JSON" });
      if (!b.project.startsWith("/")) return json(res, 400, { error: "the project is not an existing absolute directory" });
      if (!b.file.endsWith(".lean")) return json(res, 400, { error: "fisher search takes a Lean file" });
      json(res, 200, { run: newSearch(b.project, b.file, !!b.graph, b.auto !== false, b.retrieval !== false, b.library !== false).run });
    });
    return true;
  }
  if (parts.length < 3) return false;
  const id = parts[2];
  const mine = searches.get(id);
  if (parts.length === 3 && req.method === "GET" && mine) return json(res, 200, searchDetail(mine)), true;
  if (parts[3] === "events" && req.method === "GET") {
    if (mine) {
      sse(req, res, u, () => offsets(mine.lines), () => (mine.running ? null : JSON.stringify({ status: mine.status })));
      return true;
    }
    const rec = fixture.routes[`/api/search/${id}/events`];
    if (!rec) return json(res, 404, { error: "no such run" }), true;
    const { evs, end } = parseStream(rec[0].text);
    sse(req, res, u, () => evs, () => end ?? JSON.stringify({ status: "unknown" }));
    return true;
  }
  if (parts[3] === "stop" && req.method === "POST") {
    if (!mine || !mine.running) return json(res, 409, { error: "no run of this server is going there" }), true;
    mine.timers.forEach(clearTimeout);
    mine.running = false;
    mine.status = "stopped";
    return json(res, 202, { status: "stopping" }), true;
  }
  if (parts[3] === "apply" && req.method === "POST") {
    const d = mine ? searchDetail(mine) : fixture.routes[`/api/search/${id}`]?.[0]?.body;
    if (!d) return json(res, 404, { error: "no such run" }), true;
    if (!d.apply.allowed) return json(res, 409, { error: d.apply.reason, changed: false }), true;
    readBody(req).then((b) => {
      // a file written by an earlier apply has changed since the run started
      if (applied.has(id) && !b?.force)
        return json(res, 409, { error: `${d.file.split("/").pop()} was written after the run started`, changed: true });
      applied.add(id);
      json(res, 200, { written: `${d.project}/${d.file}`, bytes: 1 });
    });
    return true;
  }
  return false;
}

// ---------------------------------------------------------- new project
const projectVar = { name: "project", default: "", pattern: "[A-Za-z][A-Za-z0-9_]*", description: "The project name (default: the target directory's name)" };
const templates = [
  {
    name: "lean", prover: "lean4", description: "A plain Lean 4 package with its toolchain pinned", origin: "builtin", source: null,
    variables: [projectVar, { name: "lean", default: "latest", pattern: "latest|v4\\.[0-9]+\\.[0-9]+(-rc[0-9]+)?", description: "The Lean release to pin (a tag such as v4.24.0, or latest)" }],
    steps: [{ name: "build", run: ["lake", "build"], timeout_minutes: 30 }], sample: "{{project}}/Sample.lean", trusted: true,
  },
  {
    name: "lean-mathlib", prover: "lean4", description: "A Lean 4 package that depends on Mathlib, its cache fetched", origin: "builtin", source: null,
    variables: [projectVar, { name: "mathlib", default: "latest", pattern: "latest|v4\\.[0-9]+\\.[0-9]+(-rc[0-9]+)?", description: "The Mathlib release (a tag such as v4.24.0, or latest)" }],
    steps: [{ name: "cache", run: ["lake", "exe", "cache", "get"], timeout_minutes: 30 }, { name: "build", run: ["lake", "build"], timeout_minutes: 30 }],
    sample: "{{project}}/Sample.lean", trusted: true,
  },
  {
    name: "custom", prover: "lean4", description: "A team template with its own setup", origin: "configured", source: "/tmp/stub-templates/custom",
    variables: [projectVar], steps: [{ name: "setup", run: ["./setup.sh", "{{project}}"], timeout_minutes: 10 }], sample: "Main.lean", trusted: false,
    revision: "rev-custom-1",
  },
];
const creates = new Map(); // id -> scripted create
let createCount = 0;
const changedOnce = new Set(); // folders that stopped once as changed since confirmed

const render = (t, vals) => t.replace(/\{\{([A-Za-z0-9_]+)\}\}/g, (_, k) => vals[k] ?? "");
const baseOf = (p) => p.split("/").filter(Boolean).at(-1) ?? "";

function createAttempt(c, failing) {
  const line = (o) => c.lines.push(JSON.stringify(o));
  const at = (ms, f) => c.timers.push(setTimeout(f, ms));
  c.running = true;
  c.status = "running";
  c.reason = null;
  const finish = (ok, reason) => {
    c.running = false;
    c.status = ok ? "ready" : "failed";
    c.reason = reason ?? null;
  };
  if (c.attempts > 0 && c.mode === "project") {
    // fisher project build: the steps not done yet
    at(200, () => line({ event: "step start", step: "build" }));
    at(500, () => line({ event: "step end", step: "build", exit: 0, tail: [] }));
    at(600, () => line({ event: "check", ok: true, problems: [] }));
    at(700, () => (line({ event: "ready" }), finish(true)));
  } else {
    at(300, () => line({ event: "step start", step: "fetch" }));
    at(600, () => line({ event: "step end", step: "fetch", exit: 0, tail: [] }));
    at(900, () => line({ event: "step start", step: "build" }));
    if (failing) {
      at(1200, () => line({ event: "step end", step: "build", exit: 1, tail: ["error: unknown package 'Mathlib'"] }));
      at(1300, () => (line({ event: "failed", reason: "Step build failed (exit 1)" }), finish(false, "Step build failed (exit 1); see .fisher/create.log\nRerun with fisher project build " + c.root)));
    } else {
      at(1200, () => line({ event: "step end", step: "build", exit: 0, tail: [] }));
      at(1300, () => line({ event: "check", ok: true, problems: [] }));
      at(1400, () => (line({ event: "ready" }), finish(true)));
    }
  }
  c.attempts += 1;
}

function createDetail(c) {
  const shown = c.mode === "project" || c.status === "ready";
  return {
    id: c.id, template: c.template, mode: c.mode, root: shown ? c.root : null, file: shown ? c.file : null, status: c.status,
    running: c.running, reason: c.reason, log: `/tmp/stub-fisher/ui-runs/create-${c.id.slice(2)}.log`,
    events: c.mode === "project" ? `${c.root}/.fisher/create.jsonl` : "/tmp/stub-scratch/bases/0123456789abcdef/.fisher/create.jsonl",
  };
}

/** The New project view's routes; true when this request was one of them. */
function createRoutes(req, res, u) {
  const parts = u.pathname.split("/").filter(Boolean).map(decodeURIComponent);
  if (u.pathname === "/api/templates" && req.method === "GET") return json(res, 200, { templates, notes: [] }), true;
  if (parts[1] !== "create") return false;
  if (parts.length === 2 && req.method === "POST") {
    readBody(req).then((b) => {
      if (!b || typeof b.template !== "string") return json(res, 400, { error: "send {template, path | scratch: true, vars, confirm} as JSON" });
      const t = templates.find((x) => x.name === b.template);
      if (!t) return json(res, 400, { error: `Unknown template \`${b.template}\` (known: ${templates.map((x) => x.name).join(", ")})` });
      const scratch = b.scratch === true;
      const path = typeof b.path === "string" ? b.path.trim() : "";
      if (scratch && path) return json(res, 400, { error: "give a folder or scratch, not both" });
      if (!scratch && !path) return json(res, 400, { error: "give the project's folder (an absolute path) or scratch: true" });
      if (!scratch && !path.startsWith("/")) return json(res, 400, { error: `the folder must be an absolute path (got \`${path}\`)` });
      const given = b.vars && typeof b.vars === "object" ? b.vars : {};
      for (const k of Object.keys(given))
        if (!t.variables.some((v) => v.name === k))
          return json(res, 400, { error: `Unknown variable \`${k}\` for template \`${t.name}\` (known: ${t.variables.map((v) => v.name).join(", ")})` });
      const vals = {};
      for (const v of t.variables) {
        const x = String(given[v.name] ?? "").trim() || (v.name === "project" && !v.default ? (scratch ? "Scratch" : baseOf(path)) : v.default);
        if (v.pattern && !new RegExp(`^(?:${v.pattern})$`).test(x))
          return json(res, 400, { error: `Variable \`${v.name}\` is \`${x}\`, which does not match \`${v.pattern}\`` });
        vals[v.name] = x;
      }
      const revision = t.revision ?? `rev-${t.name}`;
      if (!t.trusted && b.confirm !== revision)
        return json(res, 409, {
          error: typeof b.confirm === "string"
            ? `Template \`${t.name}\` changed since its steps were shown; confirm them again`
            : `Template \`${t.name}\` (${t.source}) runs these steps in the new project; confirm them to go on`,
          template: t.name, revision,
          steps: t.steps.map((s) => ({ ...s, run: s.run.map((a) => render(a, vals)) })),
        });
      if (!t.trusted) t.trusted = true;
      createCount += 1;
      const id = `c.20991231-00000${createCount}`;
      const c = {
        id, template: t.name, mode: scratch ? "scratch" : "project", lines: [], timers: [], attempts: 0, running: true, status: "running", reason: null,
        root: scratch ? "/tmp/stub-scratch/20991231-000000-abcd" : path.replace(/\/+$/, ""),
        file: scratch ? "Scratch.lean" : render(t.sample, vals),
      };
      creates.set(id, c);
      if (!scratch && path.includes("changed") && !changedOnce.has(c.root)) {
        changedOnce.add(c.root);
        c.changed = { template: t.name, path: c.root, vars: given, steps: t.steps.map((s) => ({ ...s, run: s.run.map((a) => render(a, vals)) })) };
        c.attempts = 1;
        c.timers.push(setTimeout(() => {
          c.running = false;
          c.status = "failed";
          c.reason = `Template \`${t.name}\`: the template changed since it was confirmed (now at rev-${t.name}-2); nothing was written. Confirm its steps again.`;
        }, 300));
      } else createAttempt(c, !scratch && path.includes("fail"));
      json(res, 200, { id });
    });
    return true;
  }
  const c = creates.get(parts[2]);
  if (!c) return json(res, 404, { error: "no such create run" }), true;
  if (parts.length === 3 && req.method === "GET") return json(res, 200, createDetail(c)), true;
  if (parts[3] === "events" && req.method === "GET") {
    const d = () => createDetail(c);
    sse(req, res, u, () => offsets(c.lines), () => (c.running ? null : JSON.stringify({ status: c.status, reason: c.reason, root: d().root, file: d().file })));
    return true;
  }
  if (parts[3] === "stop" && req.method === "POST") {
    if (!c.running) return json(res, 409, { error: "the create has ended" }), true;
    c.timers.forEach(clearTimeout);
    c.running = false;
    c.status = "stopped";
    c.reason = "interrupted";
    return json(res, 202, { status: "stopping" }), true;
  }
  if (parts[3] === "retry" && req.method === "POST") {
    if (c.running) return json(res, 409, { error: "the create is still going" }), true;
    if (c.status === "ready") return json(res, 409, { error: "it is ready; there is nothing to retry" }), true;
    if (c.changed)
      return json(res, 409, {
        error: `Template \`${c.changed.template}\` changed since it was confirmed; confirm its steps again`,
        template: c.changed.template, revision: `rev-${c.changed.template}-2`, steps: c.changed.steps,
        request: { template: c.changed.template, path: c.changed.path, vars: c.changed.vars },
      }), true;
    if (c.mode === "scratch") c.lines = [];
    const from = offsets(c.lines).at(-1)?.id ?? 0;
    createAttempt(c, false);
    return json(res, 200, { id: c.id, from }), true;
  }
  return false;
}

// ---------------------------------------------------------- step log
const leanId = "20261001_100000_aaaaaa";
const leanProject = fixture.routes[`/api/sessions/${leanId}`][0].body.session.project;
const leanTexts = [
  "theorem t : True := by\n  sorry\n",
  "theorem t : True := by\n  skip\n  sorry\n",
  "theorem t : True := by\n  skip\n  trivial\n",
];
const steps = leanTexts.map((text, i) => ({
  seq: i + 1, path: `${leanProject}/Main.lean`, hash: `h${i + 1}`, tool: i === 0 ? "write_file" : "patch", at: 1790000002 + i, text,
}));
const verdicts = new Map();

function checkText(s) {
  const holes = (s.text.match(/\bsorry\b/g) ?? []).length;
  const errors = s.text.split("\n").flatMap((l, i) => (l.includes("BAD") ? [`Main.lean:${i + 1}:2: error: unknown tactic`] : []));
  return { errors, holes, ok: !holes && !errors.length, prover: "lean4", hash: s.hash };
}

/** The step routes; true when this request was one of them. */
function stepRoutes(req, res, u) {
  const parts = u.pathname.split("/").filter(Boolean).map(decodeURIComponent);
  if (parts[1] !== "sessions" || !["steps", "handback"].includes(parts[3])) return false;
  const mine = parts[2] === leanId;
  const head = ({ text, ...h }) => h;
  if (parts[3] === "steps" && parts.length === 4 && req.method === "GET") return json(res, 200, mine ? steps.map(head) : []), true;
  if (parts[3] === "steps" && parts.length >= 5) {
    const s = mine ? steps.find((x) => x.seq === Number(parts[4])) : undefined;
    if (!s) return json(res, 404, { error: "no such step" }), true;
    if (parts.length === 5 && req.method === "GET") return json(res, 200, { ...s, verdict: verdicts.get(s.seq) ?? null }), true;
    if (parts[5] === "verdict" && req.method === "POST") {
      if (!verdicts.has(s.seq)) verdicts.set(s.seq, checkText(s));
      return json(res, 200, verdicts.get(s.seq)), true;
    }
    return false;
  }
  if (parts[3] === "handback" && req.method === "POST") {
    readBody(req).then((b) => {
      if (!b || typeof b.path !== "string" || typeof b.text !== "string" || typeof b.base !== "string")
        return json(res, 400, { error: "send {path, text, base} as JSON" });
      if (!mine || !steps.some((s) => s.path === b.path)) return json(res, 400, { error: "the session's steps never changed this file" });
      const last = [...steps].reverse().find((s) => s.path === b.path);
      if (b.base !== last.hash) return json(res, 409, { error: "the file changed since the step the edit started from; reload it" });
      if (last.text === b.text) return json(res, 400, { error: "the file already has this text" });
      const s = { seq: steps.length + 1, path: b.path, hash: `h${steps.length + 1}`, tool: "hand", at: 1790000100, text: b.text };
      steps.push(s);
      verdicts.set(s.seq, checkText(s));
      json(res, 200, { seq: s.seq, verdict: verdicts.get(s.seq), delivered: "resume", tag: "stubresume1", after: 0, message: "The user edited ..." });
    });
    return true;
  }
  return false;
}

// ------------------------------------------------------------- settings
const settings = {
  provider: "openrouter", model: "example/model-a", autoformalizeModel: "example/model-b", contextStrategy: "clm",
  env: { OPENROUTER_API_KEY: "sk-fixture-0000000000001234", TERMINAL_SSH_HOST: "box.local" },
};
const strategies = ["compress", "clm", "rlm"];
const mainKeys = ["OPENROUTER_API_KEY", "OPENAI_API_KEY", "ANTHROPIC_API_KEY"];

// as the real route: a name with no secret in it is a plain field
const isSecret = (n) => /(_KEY|_TOKEN)$/.test(n) || /PASS|SECRET|CREDENTIAL/.test(n);
const plainKnown = ["TERMINAL_SSH_HOST", "TERMINAL_SSH_USER", "FIRECRAWL_API_URL", "BROWSERBASE_PROJECT_ID"];

function settingsView() {
  const names = [...mainKeys, ...Object.keys(settings.env).filter((n) => !mainKeys.includes(n) && isSecret(n)).sort()];
  const plain = [...plainKnown, ...Object.keys(settings.env).filter((n) => !isSecret(n) && !plainKnown.includes(n) && /^(TERMINAL_SSH|FIRECRAWL_API_URL|BROWSERBASE_PROJECT_ID)/.test(n)).sort()];
  return {
    provider: settings.provider, model: settings.model, autoformalizeModel: settings.autoformalizeModel,
    contextStrategy: settings.contextStrategy, strategies,
    auto: { tactics: ["rfl", "decide", "omega", "simp_all", "aesop", "grind", "exact?"], seconds: 60, repair: true, disprove: true, settable: false },
    keys: names.map((name) => {
      const v = settings.env[name] ?? "";
      return { name, set: v !== "", last4: v.length >= 8 ? v.slice(-4) : null };
    }),
    plain: plain.map((name) => ({ name, value: settings.env[name] ?? "" })),
    warnings: [],
  };
}

/** The settings routes; true when this request was one of them. */
function settingsRoutes(req, res, u) {
  if (u.pathname !== "/api/settings") return false;
  if (req.method === "GET") return json(res, 200, settingsView()), true;
  if (req.method !== "POST") return json(res, 405, { error: "GET or POST" }), true;
  readBody(req).then((b) => {
    if (!b || typeof b !== "object") return json(res, 400, { error: "send the settings as a JSON object" });
    const fields = { model: false, provider: false, autoformalizeModel: true, contextStrategy: false };
    const next = {};
    for (const [k, mayBeEmpty] of Object.entries(fields)) {
      if (b[k] === undefined || b[k] === null) continue;
      if (typeof b[k] !== "string") return json(res, 400, { error: `${k} is not text` });
      const v = b[k].trim();
      if (!v && !mayBeEmpty) return json(res, 400, { error: `${k} is empty` });
      if (k === "contextStrategy" && !strategies.includes(v)) return json(res, 400, { error: "contextStrategy is one of compress, clm, rlm" });
      next[k] = v;
    }
    const keys = Array.isArray(b.keys) ? b.keys : [];
    for (const k of keys) {
      if (typeof k?.name !== "string" || !(/^[A-Z_][A-Z0-9_]*(_API_KEY|_TOKEN)$/.test(k.name) || plainKnown.includes(k.name)))
        return json(res, 400, { error: "a key's name is not a credential name (such as OPENROUTER_API_KEY)" });
      if (typeof k.value !== "string" || !k.value.trim()) return json(res, 400, { error: `${k.name}: the value is empty` });
    }
    Object.assign(settings, next);
    for (const k of keys) settings.env[k.name] = k.value.trim();
    json(res, 200, { ...settingsView(), saved: [...Object.keys(next), ...keys.map((k) => k.name)] });
  });
  return true;
}

// ------------------------------------------------------------- claude auth
const claude = { loggedIn: false, authMethod: null, subscriptionType: null, email: null, runs: new Map(), n: 0 };
const claudeMissing = "The 'claude' CLI is not installed. Install it with: npm install -g @anthropic-ai/claude-code";

function claudeGoing() {
  for (const [tag, r] of claude.runs) if (r.running) return tag;
  return null;
}

function claudeView(req) {
  if (scenarioOf(req) === "claude-missing")
    return { installed: false, loggedIn: false, authMethod: null, email: null, subscriptionType: null, error: null, login: null };
  return {
    installed: true, loggedIn: claude.loggedIn, authMethod: claude.authMethod, email: claude.email,
    subscriptionType: claude.subscriptionType, error: null, login: claudeGoing(),
  };
}

/** The Claude Code routes; true when this request was one of them. */
function claudeRoutes(req, res, u) {
  const parts = u.pathname.split("/").filter(Boolean).map(decodeURIComponent);
  if (parts[0] !== "api" || parts[1] !== "claude-auth") return false;
  const only = (m) => (req.method === m ? true : (json(res, 405, { error: m }), false));
  const missing = scenarioOf(req) === "claude-missing";
  if (parts.length === 2) {
    if (only("GET")) json(res, 200, claudeView(req));
    return true;
  }
  if (parts.length === 3 && parts[2] === "logout") {
    if (!only("POST")) return true;
    if (missing) return json(res, 409, { error: claudeMissing }), true;
    Object.assign(claude, { loggedIn: false, authMethod: null, subscriptionType: null, email: null });
    json(res, 200, claudeView(req));
    return true;
  }
  if (parts.length === 3 && parts[2] === "login") {
    if (!only("POST")) return true;
    readBody(req).then((b) => {
      if (!b || typeof b !== "object" || Array.isArray(b)) return json(res, 400, { error: "send {console, sso, email} as a JSON object" });
      for (const k of ["console", "sso"])
        if (b[k] !== undefined && b[k] !== null && typeof b[k] !== "boolean") return json(res, 400, { error: `${k} is true or false` });
      if (b.email !== undefined && b.email !== null) {
        if (typeof b.email !== "string") return json(res, 400, { error: "email is text" });
        const e = b.email.trim();
        if (e && (e.length > 254 || /[\s\x00-\x1f\x7f]/.test(e) || e.startsWith("-")))
          return json(res, 400, { error: "email is one address: no spaces or line breaks, at most 254 characters" });
      }
      if (missing) return json(res, 409, { error: claudeMissing });
      const going = claudeGoing();
      if (going) return json(res, 409, { error: "a Claude Code login is already going", tag: going });
      claude.n += 1;
      const tag = `stubclaude${claude.n}`;
      claude.runs.set(tag, {
        console: b.console === true, running: true, stopping: false, exitCode: null,
        lines: ["Opening your browser to log in.", "https://claude.ai/oauth/authorize?code=true&state=stub"],
        partial: "Paste code here if prompted > ",
      });
      json(res, 200, { tag });
    });
    return true;
  }
  const r = parts.length >= 4 && parts[2] === "login" ? claude.runs.get(parts[3]) : undefined;
  if (!r) return json(res, 404, { error: "no such login" }), true;
  if (parts.length === 4) {
    if (!only("GET")) return true;
    const from = Math.max(0, Math.min(Number(u.searchParams.get("from") ?? 0) || 0, r.lines.length));
    json(res, 200, {
      tag: parts[3], running: r.running, stopping: r.stopping, exitCode: r.exitCode,
      lines: r.lines.slice(from), partial: r.partial, next: r.lines.length, truncated: false,
    });
    return true;
  }
  if (parts.length === 5 && parts[4] === "input") {
    if (!only("POST")) return true;
    readBody(req).then((b) => {
      if (typeof b?.text !== "string") return json(res, 400, { error: "send {text} as JSON" });
      if (/[\r\n\0]/.test(b.text)) return json(res, 400, { error: "the text is one line" });
      if (!r.running) return json(res, 409, { error: "the login has ended" });
      r.lines.push(r.partial + "Login successful.");
      r.partial = "";
      Object.assign(r, { running: false, exitCode: 0 });
      Object.assign(claude, r.console
        ? { loggedIn: true, authMethod: "console", subscriptionType: null, email: "x@y.z" }
        : { loggedIn: true, authMethod: "claude.ai", subscriptionType: "max", email: "x@y.z" });
      json(res, 200, { sent: true });
    });
    return true;
  }
  if (parts.length === 5 && parts[4] === "stop") {
    if (!only("POST")) return true;
    if (!r.running) return json(res, 409, { error: "the login has ended" }), true;
    Object.assign(r, { running: false, stopping: true, exitCode: 143 });
    json(res, 202, { status: "stopping" });
    return true;
  }
  json(res, 404, { error: "no such route" });
  return true;
}

// ------------------------------------------------------------- run log
// Two runs as fisher-ui's /api/runs-log gives them (Fisher.Runlog's
// columns): a search in /work/demo and its closed attempt.
const runRow = (o) => ({
  parent: null, project: "/work/demo", project_key: "6a1f0e1d2c3b4a59", prover: "lean", backend: null, model: null,
  tier: null, workflow: null, exit: 0, cost_usd: null, input_tokens: null, output_tokens: null, turns: null,
  raw_dir: null, capture: "full", ...o,
});
const runlog = [
  runRow({
    id: "a.20261024-100500", kind: "search-attempt", parent: "s.20261024-100000", backend: "fisher",
    argv: ["fisher", "search", "Demo.lean", "--attempt"], started_at: 1792836300, ended_at: 1792836330,
    outcome: "closed", cost_usd: 0.042, turns: 4,
  }),
  runRow({
    id: "s.20261024-100000", kind: "search", argv: ["fisher", "search", "Demo.lean"],
    started_at: 1792836000, ended_at: 1792836400, outcome: "closed",
  }),
];
const runDetail = {
  "a.20261024-100500": {
    goals: [],
    events: [
      { seq: 1, t: 0, kind: "attempt", data: { goal: "Demo.main_thm", shape: "eq||Nat|small", backend: "fisher" } },
      { seq: 2, t: 12.5, kind: "tool_start", data: { name: "lean_check", args: { file: "Demo.lean" } } },
      { seq: 3, t: 30, kind: "closed", data: { goal: "Demo.main_thm" } },
    ],
  },
  "s.20261024-100000": {
    goals: [{ name: "Demo.main_thm", statement: "1 + 1 = 2", shape: "eq||Nat|small", status: "closed", closed_by: "fisher", seconds_to_close: 30, dollars_to_close: 0.042 }],
    events: [{ seq: 1, t: 0, kind: "portfolio", data: { tactics: ["simp", "omega"] } }],
  },
};

/** The run log routes; true when this request was one of them. */
function runlogRoutes(req, res, u) {
  if (u.pathname === "/api/runs-log") {
    const kind = u.searchParams.get("kind");
    const project = u.searchParams.get("project");
    const rows = runlog.filter((r) => (!kind || r.kind === kind) && (!project || r.project === project));
    return json(res, 200, rows), true;
  }
  const m = u.pathname.match(/^\/api\/runs-log\/([^/]+)$/);
  if (!m) return false;
  const id = decodeURIComponent(m[1]);
  const run = runlog.find((r) => r.id === id);
  if (!run) return json(res, 404, { error: "no such run" }), true;
  return json(res, 200, { run, ...runDetail[id], truncated: false }), true;
}

// ---------------------------------------------------------------- learn
const armRow = (arm, attempts, closes, rate, cost_per_close, closes_per_hour) => ({ arm, attempts, closes, rate, cost_per_close, closes_per_hour });
const learnDemo = {
  stats: {
    "*": {
      backends: [armRow("fisher", 12, 9, 0.72, 0.06, 40.5), armRow("claude-code", 10, 3, 0.31, 1.25, 6)],
      tiers: [],
      tactics: [armRow("simp", 20, 6, 0.3, null, 120), armRow("omega", 20, 2, 0.12, null, 30)],
    },
    "eq||Nat|small": {
      backends: [armRow("fisher", 8, 7, 0.8, 0.05, 50), armRow("claude-code", 2, 0, 0.25, 2.1, 0)],
      tiers: [],
      tactics: [],
    },
  },
  policy: {
    version: 1, computed_at: 1792836400, project_key: "6a1f0e1d2c3b4a59",
    shapes: {
      "*": {
        backends: ["fisher", "claude-code"], tiers: [], tactics: ["simp", "omega"], attempt_secs: 240,
        evidence: { backends: { fisher: { attempts: 12, closes: 9, rate: 0.72, cost_per_close: 0.06 } } },
      },
    },
  },
  changes: [
    { at: 1792836400, shape: "*", knob: "backends", old: ["claude-code", "fisher"], new: ["fisher", "claude-code"], evidence: { fisher: { attempts: 12, closes: 9 } } },
  ],
};

/** The Learn route; true when this request was it. */
function learnRoutes(req, res, u) {
  if (u.pathname !== "/api/learn") return false;
  const project = u.searchParams.get("project");
  const projects = ["/work/demo", "/work/other"];
  if (project && !project.startsWith("/")) return json(res, 400, { error: "project is the absolute path of a project's root" }), true;
  const base = { project, project_key: project ? "0000000000000000" : null, projects, policy_error: null };
  if (project === "/work/demo") return json(res, 200, { ...base, project_key: "6a1f0e1d2c3b4a59", ...learnDemo }), true;
  const stats = project ? { "*": { backends: [], tiers: [], tactics: [] } } : {};
  return json(res, 200, { ...base, stats, policy: null, changes: [] }), true;
}

// ---------------------------------------------------------------- lessons
const lessonRow = (o) => ({
  project_key: "6a1f0e1d2c3b4a59", kind: "unknown_identifier", agreements: 0, first_seen: 1792836000,
  activated_at: null, retired_at: null, runs_active: [], hits_while_active: 0, attempts_while_active: 0,
  hits_before: 0, attempts_before: 0, runs_before: 0, proposed: false, ...o,
});
const lessons = [
  lessonRow({ id: 1, key: "k:build", text: "Run `lake build` before finishing.", status: "candidate", evidence_runs: ["a.20261024-100500", "s.20261024-100000"] }),
  lessonRow({ id: 2, key: "k:foo", text: "In this project `Nat.foo` does not exist.", status: "active", evidence_runs: ["a.20261024-100500"], activated_at: 1792836400 }),
  lessonRow({ id: 4, kind: "judged", key: "foo.bar_baz", text: "Check `Foo.bar_baz` before using it.", status: "candidate", evidence_runs: ["r1", "r2", "r3"], agreements: 2, proposed: true }),
  lessonRow({ id: 3, project_key: "*", key: "k:compile", text: "Always check the file compiles before you finish.", status: "active", evidence_runs: ["a.20261024-100500"], activated_at: 1792836400 }),
];

/** The pin, unpin and drop rules of Fisher.Learn.Lessons.actionTarget: the new status, or why not. */
function lessonTarget(action, status) {
  if (action === "pin") return status === "pinned" ? [null, "already pinned"] : status === "dropped" ? [null, "dropped"] : ["pinned"];
  if (action === "unpin") return status === "pinned" ? ["active"] : [null, "not pinned"];
  return status === "dropped" ? [null, "already dropped"] : ["dropped"];
}

/** The Lessons routes; true when this request was one of them. */
function lessonsRoutes(req, res, u) {
  if (u.pathname !== "/api/lessons") return false;
  if (req.method === "POST") {
    readBody(req).then((b) => {
      if (!b || !Number.isInteger(b.id) || !["pin", "unpin", "drop"].includes(b.action))
        return json(res, 400, { error: "send {id, action, project} as JSON (action one of pin, unpin, drop)" });
      const l = lessons.find((x) => x.id === b.id);
      if (!l) return json(res, 404, { error: "no such lesson" });
      // the global lesson stands in for a refusal the server would give
      if (l.id === 3) return json(res, 409, { error: "the stub refuses" });
      const [st, why] = lessonTarget(b.action, l.status);
      if (!st) return json(res, 409, { error: why });
      // an unpinned judged or self lesson is a candidate again, proposed (Fisher.Learn.Lessons.applyTo)
      const judged = l.kind === "judged" || l.kind === "self";
      l.status = b.action === "unpin" && judged ? "candidate" : st;
      l.proposed = l.status === "candidate" && judged;
      json(res, 200, l);
    });
    return true;
  }
  const project = u.searchParams.get("project");
  if (project && !project.startsWith("/")) return json(res, 400, { error: "project is the absolute path of a project's root" }), true;
  const key = project === "/work/demo" ? "6a1f0e1d2c3b4a59" : project ? "0000000000000000" : null;
  return json(res, 200, {
    project, project_key: key,
    lessons: lessons.filter((l) => key && l.project_key === key),
    global: lessons.filter((l) => l.project_key === "*"),
  }), true;
}

function hasCookie(req) {
  return (req.headers.cookie ?? "").split(/;\s*/).includes(`fisher_ui_${port}=${token}`);
}

function send(res, status, type, body, extra = {}) {
  res.writeHead(status, { "Content-Type": type, "Cache-Control": "no-store", ...extra });
  res.end(body);
}

createServer((req, res) => {
  const u = new URL(req.url, "http://127.0.0.1");
  if (!u.pathname.startsWith("/api") && u.searchParams.has("t")) {
    if (u.searchParams.get("t") === token)
      return send(res, 302, "text/plain", "", {
        Location: "/",
        "Set-Cookie": `fisher_ui_${port}=${token}; HttpOnly; SameSite=Strict; Path=/`,
      });
    return send(res, 401, "text/plain; charset=utf-8", "This link is from an earlier start of fisher ui; use the URL it printed last.");
  }
  if (u.pathname === "/api" || u.pathname.startsWith("/api/")) {
    if (!hasCookie(req)) return send(res, 401, "text/plain; charset=utf-8", fixture.unauthorized.text ?? "");
    if (settingsRoutes(req, res, u)) return;
    if (claudeRoutes(req, res, u)) return;
    if (stepRoutes(req, res, u)) return;
    if (searchRoutes(req, res, u)) return;
    if (createRoutes(req, res, u)) return;
    if (runlogRoutes(req, res, u)) return;
    if (learnRoutes(req, res, u)) return;
    if (lessonsRoutes(req, res, u)) return;
    if (live(req, res, u)) return;
    const path = canonical(req.url);
    let a = fromScenario(req, path);
    if (!a) {
      const r = routes.get(path);
      if (!r) return send(res, 404, "application/json", JSON.stringify({ error: "not recorded by capture.py" }));
      a = r.answers[Math.min(r.next, r.answers.length - 1)];
      r.next += 1;
    }
    if (a.body !== undefined) return send(res, a.status, "application/json", JSON.stringify(a.body));
    return send(res, a.status, "text/plain; charset=utf-8", a.text ?? "");
  }
  const rel = normalize(decodeURIComponent(u.pathname)).replace(/^\/+/, "");
  const file = join(dist, rel);
  if (rel && file.startsWith(dist) && existsSync(file) && statSync(file).isFile())
    return send(res, 200, types[extname(file)] ?? "application/octet-stream", readFileSync(file));
  if (rel.startsWith("assets/")) return send(res, 404, "text/plain", "Not found.");
  return send(res, 200, types[".html"], readFileSync(join(dist, "index.html")));
}).listen(port, "127.0.0.1", () => console.log(`Fisher UI stub: http://127.0.0.1:${port}/?t=${token}`));
