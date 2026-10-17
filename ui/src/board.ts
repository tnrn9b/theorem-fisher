// The goal board's state from a search run's events (events.jsonl, as
// fisher-search's Report.eventJson and graphEventJson write them), with
// the run's report.json and graph.json laid over it once the run is done:
// the report's statuses and closedBy are the final word.

import type { GraphJson, ReportGoal } from "./api";

/** A search event as one line of events.jsonl. */
export type SearchEvent = Record<string, unknown> & { event: string; t?: number };

/** What the automation, repair or disproof did to a goal or node. */
export interface Mark {
  kind:
    | "auto closed"
    | "auto missed"
    | "auto timeout"
    | "repaired"
    | "disproved"
    | "suspect"
    | "claimed false"
    | "facts"
    | "library offered"
    | "library used"
    | "tier"
    | "attempt stopped"
    | "escalated";
  text: string;
  /** what makes two marks the same one, when kind and text do not (the attempt) */
  key?: string;
  /** shown in place of the kind and text (a tier badge, a stop mark) */
  label?: string;
  /** the tooltip, when it is not the text */
  title?: string;
}

/** The compute controller's tier and stop marks on a goal or node, from the events that carry them. */
function controllerMark(e: Record<string, unknown>): Mark | null {
  switch (e.event) {
    case "tier chosen":
      return { kind: "tier", text: `T${s(e.tier)}`, label: `T${s(e.tier)}`, title: s(e.model), key: s(e.attempt) };
    case "attempt stopped": {
      const why = s(e.reason);
      return { kind: "attempt stopped", text: why, label: `#${s(e.attempt)} stopped (${why})`, key: s(e.attempt), title: `${s(e.turns)} turns, $${s(e.dollars)}` };
    }
    case "escalated":
      return { kind: "escalated", text: `T${s(e.from)} to T${s(e.to)}`, label: `escalated T${s(e.from)} to T${s(e.to)}` };
    default:
      return null;
  }
}

/** What the controller did to the whole run: the tiers it skipped, the ledgers it could not read. */
export interface RunTiers {
  skipped: string[];
  ledgerUnread: string[];
}

export function foldTiers(events: SearchEvent[]): RunTiers {
  const out: RunTiers = { skipped: [], ledgerUnread: [] };
  for (const e of events) {
    if (e.event === "tier skipped") {
      const x = `${s(e.model)} (${s(e.reason)})`;
      if (!out.skipped.includes(x)) out.skipped.push(x);
    } else if (e.event === "ledger unread") {
      const x = `attempt ${s(e.attempt)}: ${s(e.reason)}`;
      if (!out.ledgerUnread.includes(x)) out.ledgerUnread.push(x);
    }
  }
  return out;
}

/** What retrieval did to the whole run: its index, the library's save, its problems. */
export interface RunRetrieval {
  index: string | null;
  saved: string | null;
  problems: string[];
}

export interface GoalState {
  goal: string;
  /** open, closed, waiting, gave up, disproved */
  status: string;
  closedBy: string | null;
  /** attempts launched and not finished */
  running: number;
  attempts: number;
  marks: Mark[];
}

export interface NodeState {
  id: number;
  name: string;
  depth: number;
  /** open, proved, failed */
  status: string;
  reason: string | null;
  splits: number[];
  running: number;
  attempts: number;
  marks: Mark[];
}

export interface SplitState {
  id: number;
  parent: number;
  lemmas: number[];
  /** proposed, rejected, accepted, failed, done */
  status: string;
  reason: string | null;
}

export interface GraphState {
  nodes: Map<number, NodeState>;
  splits: Map<number, SplitState>;
  roots: number[];
  dollars: Record<string, number>;
  stopped: string | null;
}

const s = (v: unknown): string => (typeof v === "string" ? v : v === undefined || v === null ? "" : String(v));
const n = (v: unknown): number | null => (typeof v === "number" ? v : null);

function addMark(marks: Mark[], m: Mark) {
  if (!marks.some((x) => x.kind === m.kind && x.text === m.text && x.key === m.key)) marks.push(m);
}

function disproofMark(e: Record<string, unknown>): Mark {
  return s(e.result) === "false"
    ? { kind: "disproved", text: `negation by ${s(e.tactic)}` }
    : { kind: "suspect", text: `counterexample ${s(e.counterexample)}` };
}

function factsMark(e: Record<string, unknown>): Mark {
  return { kind: "facts", text: n(e.chars) !== null ? `${s(e.entries)} (${s(e.chars)} chars)` : s(e.entries) };
}

function offeredMark(e: Record<string, unknown>): Mark {
  const ls = Array.isArray(e.lemmas) ? e.lemmas.map(s) : [];
  return { kind: "library offered", text: ls.length ? ls.join(", ") : "no lemmas" };
}

/** The report's retrieval fields of a goal or node, as marks the events did not already give. */
function reportRetrieval(marks: Mark[], r: { facts?: number; offered?: string[]; used?: string[] }) {
  if (typeof r.facts === "number" && !marks.some((m) => m.kind === "facts")) marks.push({ kind: "facts", text: String(r.facts) });
  if (Array.isArray(r.offered) && r.offered.length && !marks.some((m) => m.kind === "library offered"))
    marks.push({ kind: "library offered", text: r.offered.join(", ") });
  if (Array.isArray(r.used) && r.used.length && !marks.some((m) => m.kind === "library used"))
    marks.push({ kind: "library used", text: r.used.join(", ") });
}

/**
 * The run's own retrieval events (fisher-search's Report.eventJson):
 * "index built" or "index skipped", "library saved", "library problem".
 */
export function foldRetrieval(events: SearchEvent[]): RunRetrieval {
  const out: RunRetrieval = { index: null, saved: null, problems: [] };
  for (const e of events) {
    switch (e.event) {
      case "index built":
        out.index = `${s(e.declarations)} declarations in ${s(e.files)} files`;
        break;
      case "index skipped":
        out.index = `skipped ${s(e.file)}: ${s(e.reason)}`;
        break;
      case "library saved":
        out.saved = `${s(e.lemmas)} lemmas, ${s(e.notes)} notes`;
        break;
      case "library problem":
        if (!out.problems.includes(s(e.reason))) out.problems.push(s(e.reason));
        break;
    }
  }
  return out;
}

/** The status a row shows: a goal still open with an attempt going is running. */
export function shownStatus(x: { status: string; running: number }): string {
  return x.running > 0 && x.status === "open" ? "running" : x.status;
}

// ------------------------------------------------------------------ flat

export function foldFlat(events: SearchEvent[], reportGoals: ReportGoal[] | null): GoalState[] {
  const goals = new Map<string, GoalState>();
  const repaired = new Set<number>();
  const get = (g: string): GoalState => {
    let x = goals.get(g);
    if (!x) {
      x = { goal: g, status: "open", closedBy: null, running: 0, attempts: 0, marks: [] };
      goals.set(g, x);
    }
    return x;
  };
  // the report's order first, then any goal only the events name
  for (const r of reportGoals ?? []) get(r.goal);
  for (const e of events) {
    const g = s(e.goal);
    if (!g) continue;
    const x = get(g);
    switch (e.event) {
      case "launched":
        x.running += 1;
        x.attempts += 1;
        break;
      case "finished":
      case "cancelled":
        x.running = Math.max(0, x.running - 1);
        break;
      case "closed":
        x.status = "closed";
        x.closedBy = repaired.has(n(e.attempt) ?? -1) ? "model+repair" : "model";
        break;
      case "waiting":
        if (x.status === "open") x.status = "waiting";
        break;
      case "gave up":
        x.status = "gave up";
        break;
      case "auto closed":
        x.status = "closed";
        x.closedBy = "auto";
        addMark(x.marks, { kind: "auto closed", text: s(e.snippets) });
        break;
      case "auto missed":
        addMark(x.marks, { kind: "auto missed", text: n(e.seconds) !== null ? `${e.seconds} s` : "" });
        break;
      case "auto timeout":
        addMark(x.marks, { kind: "auto timeout", text: s(e.step) });
        break;
      case "repaired":
        if (n(e.attempt) !== null) repaired.add(n(e.attempt)!);
        addMark(x.marks, { kind: "repaired", text: `holes ${s(e.holes_before)} to ${s(e.holes_after)}` });
        break;
      case "disproved": {
        const m = disproofMark(e);
        addMark(x.marks, m);
        if (m.kind === "disproved") x.status = "disproved";
        break;
      }
      case "facts":
        addMark(x.marks, factsMark(e));
        break;
      case "library offered":
        addMark(x.marks, offeredMark(e));
        break;
      case "tier chosen":
      case "attempt stopped":
      case "escalated": {
        const m = controllerMark(e);
        if (m) addMark(x.marks, m);
        break;
      }
    }
  }
  if (reportGoals) {
    for (const r of reportGoals) {
      const x = get(r.goal);
      x.status = r.status;
      x.closedBy = r.closedBy ?? null;
      x.running = 0;
      x.attempts = Math.max(x.attempts, r.attempts?.length ?? 0);
      if (r.disproof && !x.marks.some((m) => m.kind === "disproved" || m.kind === "suspect"))
        x.marks.push(disproofMark({ result: r.disproof.verdict, tactic: r.disproof.tactic, counterexample: r.disproof.counterexample }));
      if (r.closedBy === "auto" && !x.marks.some((m) => m.kind === "auto closed")) x.marks.push({ kind: "auto closed", text: "" });
      if (r.closedBy === "model+repair" && !x.marks.some((m) => m.kind === "repaired")) x.marks.push({ kind: "repaired", text: "" });
      reportRetrieval(x.marks, r);
    }
  }
  return [...goals.values()];
}

// ------------------------------------------------------------------ graph

function emptyNode(id: number): NodeState {
  return { id, name: `node ${id}`, depth: 0, status: "open", reason: null, splits: [], running: 0, attempts: 0, marks: [] };
}

/**
 * The lemma graph from the events; with graph.json (a finished run's), its
 * nodes, splits and statuses instead, and the events' marks on them.
 */
export function foldGraph(
  events: SearchEvent[],
  final: GraphJson | null,
  retrieval: { node?: number | null; facts?: number; offered?: string[]; used?: string[] }[] = [],
): GraphState {
  const st: GraphState = { nodes: new Map(), splits: new Map(), roots: [], dollars: {}, stopped: null };
  const node = (id: number): NodeState => {
    let x = st.nodes.get(id);
    if (!x) {
      x = emptyNode(id);
      st.nodes.set(id, x);
    }
    return x;
  };
  const split = (id: number, parent = 0): SplitState => {
    let x = st.splits.get(id);
    if (!x) {
      x = { id, parent, lemmas: [], status: "proposed", reason: null };
      st.splits.set(id, x);
    }
    return x;
  };
  for (const e of events) {
    const id = n(e.node);
    const sp = n(e.split);
    switch (e.event) {
      case "node added": {
        if (id === null) break;
        const x = node(id);
        x.name = s(e.name) || x.name;
        x.depth = n(e.depth) ?? x.depth;
        if (x.depth === 0 && !st.roots.includes(id)) st.roots.push(id);
        break;
      }
      case "node shared":
      case "node reused":
        if (id !== null && sp !== null) {
          const x = split(sp);
          if (!x.lemmas.includes(id)) x.lemmas.push(id);
          node(id);
        }
        break;
      case "split proposed":
        if (sp !== null && id !== null) {
          const x = split(sp, id);
          x.parent = id;
          const p = node(id);
          if (!p.splits.includes(sp)) p.splits.push(sp);
        }
        break;
      case "split rejected":
      case "split failed":
        if (sp !== null) {
          const x = split(sp);
          x.status = e.event === "split rejected" ? "rejected" : "failed";
          x.reason = s(e.reason) || null;
        }
        break;
      case "split accepted":
        if (sp !== null) {
          const x = split(sp);
          x.status = "accepted";
          const ls = Array.isArray(e.lemmas) ? (e.lemmas.filter((v) => typeof v === "number") as number[]) : [];
          for (const l of ls) {
            if (!x.lemmas.includes(l)) x.lemmas.push(l);
            node(l);
          }
        }
        break;
      case "node proved":
        if (id !== null) node(id).status = "proved";
        break;
      case "node failed":
        if (id !== null) {
          node(id).status = "failed";
          node(id).reason = s(e.reason) || null;
        }
        break;
      case "false lemma":
        if (id !== null) addMark(node(id).marks, { kind: "claimed false", text: s(e.claim) });
        break;
      case "launched":
        if (id !== null) {
          node(id).running += 1;
          node(id).attempts += 1;
        }
        break;
      case "finished":
        if (id !== null) node(id).running = Math.max(0, node(id).running - 1);
        break;
      case "cost":
        st.dollars[s(e.role)] = (st.dollars[s(e.role)] ?? 0) + (n(e.usd) ?? 0);
        break;
      case "stopped":
        st.stopped = s(e.reason) || "stopped";
        break;
      case "auto closed":
        if (id !== null) addMark(node(id).marks, { kind: "auto closed", text: s(e.snippets) });
        break;
      case "auto timeout":
        if (id !== null) addMark(node(id).marks, { kind: "auto timeout", text: s(e.step) });
        break;
      case "repaired":
        if (id !== null) addMark(node(id).marks, { kind: "repaired", text: `holes ${s(e.holes_before)} to ${s(e.holes_after)}` });
        break;
      case "disproved":
        if (id !== null) addMark(node(id).marks, disproofMark(e));
        break;
      case "facts":
        if (id !== null) addMark(node(id).marks, factsMark(e));
        break;
      case "library offered":
        if (id !== null) addMark(node(id).marks, offeredMark(e));
        break;
      case "tier chosen":
      case "attempt stopped":
      case "escalated": {
        const m = controllerMark(e);
        if (id !== null && m) addMark(node(id).marks, m);
        break;
      }
    }
  }
  if (final) {
    // graph.json is the final word on the structure and the statuses
    const marks = new Map([...st.nodes.values()].map((x) => [x.id, x.marks]));
    st.nodes = new Map();
    st.splits = new Map();
    st.roots = [...final.roots];
    for (const x of final.nodes)
      st.nodes.set(x.id, {
        id: x.id, name: x.name, depth: x.depth, status: x.status, reason: x.reason ?? null, splits: [...x.splits],
        running: 0, attempts: x.attempts, marks: marks.get(x.id) ?? [],
      });
    for (const x of final.splits)
      st.splits.set(x.id, { id: x.id, parent: x.parent, lemmas: [...x.lemmas], status: x.status, reason: x.reason ?? null });
  }
  // report.json's retrieval per node (a finished graph run's)
  for (const r of retrieval) if (typeof r.node === "number") reportRetrieval(node(r.node).marks, r);
  return st;
}
