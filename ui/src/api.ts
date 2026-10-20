// Typed fetchers for the read routes (fisher-ui/src/Fisher/UI/Api/*.hs).

export interface SessionSummary {
  id: string;
  title: string | null;
  project: string | null;
  prover: string | null;
  model: string | null;
  strategy: string;
  started: number;
  turns: number;
  dollars: number;
  result: string | null;
}

export interface ToolCallJson {
  id?: string;
  type?: string;
  function?: { name?: string; arguments?: string };
}

export interface Message {
  role: string;
  content: string | null;
  toolCalls: ToolCallJson[] | null;
  toolName: string | null;
  toolCallId: string | null;
  at: number;
}

export interface Cost {
  dollars: number;
  input: number;
  output: number;
  cacheRead: number;
}

export interface SessionDetail {
  session: SessionSummary;
  messages: Message[];
  cost: Cost;
  children: string[];
}

export interface CtxEvent {
  kind: string;
  turn: number;
  detail: Record<string, unknown>;
}

export interface ContextView {
  strategy: string;
  now: string | null;
  prev: string | null;
  state: unknown;
  events: CtxEvent[];
  summaries: { session: string; text: string }[];
}

export interface RunRoot {
  bench: string;
  arm: string;
  root: string;
}

export interface GoalRow {
  goal: string;
  arm: string;
  solved: boolean;
  turns: number | null;
  dollars: number | null;
  closedBy: string | null;
  /** bench w: the goal's facts, and the library's lemmas offered and used */
  facts?: number;
  offered?: number;
  used?: number;
}

/** A bench w run: one whole file. */
export interface FileRow {
  arm: string;
  run: string;
  file: string | null;
  closed: number;
  total: number | null;
  dollars: number | null;
  /** report.json's retrieval totals (null without retrieval) */
  factsMean: number | null;
  offered: number | null;
  used: number | null;
  report: boolean;
}

export type GateStatus = "pending" | "failed" | "bench in progress" | "ok";

export interface BenchView {
  bench: string;
  arms: string[];
  goals: GoalRow[];
  gate: string | null;
  gateStatus: GateStatus | null;
  /** bench w only */
  files?: FileRow[];
}

export interface SessionFilters {
  project?: string;
  prover?: string;
  model?: string;
  strategy?: string;
}

/** A template the New project view offers (GET /api/templates). */
export interface TemplateVariable {
  name: string;
  default: string;
  /** a regular expression the whole value must match ("" for any) */
  pattern: string;
  description: string;
}

export interface TemplateStep {
  name: string;
  run: string[];
  timeout_minutes: number;
}

export interface TemplateView {
  name: string;
  prover: string;
  description: string;
  variables: TemplateVariable[];
  origin: "builtin" | "configured" | "source";
  source: string | null;
  steps: TemplateStep[];
  sample: string;
  trusted: boolean;
}

export interface TemplateList {
  templates: TemplateView[];
  /** configured templates that could not be loaded */
  notes: string[];
}

export interface CreateRequest {
  template: string;
  path?: string;
  scratch?: boolean;
  statement?: string;
  vars: Record<string, string>;
  /** the revision a 409 answered with: the steps shown were confirmed */
  confirm?: string;
}

/** A create run (GET /api/create/:id); root and file once known. */
export interface CreateView {
  id: string;
  template: string;
  mode: "project" | "scratch";
  root: string | null;
  file: string | null;
  status: string;
  running: boolean;
  reason: string | null;
  log: string;
  events: string | null;
}

/** A line of a project's .fisher/create.jsonl. */
export type CreateEvent =
  | { event: "step start"; step: string }
  | { event: "step end"; step: string; exit: number; tail: string[] }
  | { event: "check"; ok: boolean; problems: string[] }
  | { event: "ready" }
  | { event: "failed"; reason: string };

/** A failed request: 401 means the page has no cookie (open the printed URL). */
export class ApiError extends Error {
  constructor(public status: number, message: string, public body: Record<string, unknown> | null = null) {
    super(message);
  }
}

async function getJson<T>(path: string): Promise<T> {
  const r = await fetch(path, { credentials: "same-origin", headers: { Accept: "application/json" } });
  return answer<T>(r);
}

async function postJson<T>(path: string, body: unknown): Promise<T> {
  const r = await fetch(path, {
    method: "POST",
    credentials: "same-origin",
    headers: { Accept: "application/json", "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  return answer<T>(r);
}

async function answer<T>(r: Response): Promise<T> {
  if (!r.ok) {
    let msg = `${r.status}`;
    let body: Record<string, unknown> | null = null;
    try {
      body = (await r.json()) as Record<string, unknown>;
      if (typeof body.error === "string") msg = body.error;
    } catch {
      // a plain-text refusal
    }
    throw new ApiError(r.status, msg, body);
  }
  return (await r.json()) as T;
}

const filterKeys = ["project", "prover", "model", "strategy"] as const;

export function filterQuery(f: SessionFilters): string {
  const q = new URLSearchParams();
  for (const k of filterKeys) {
    const v = f[k];
    if (v) q.set(k, v);
  }
  const s = q.toString();
  return s ? `?${s}` : "";
}

/** A run the page started (fisher-ui/src/Fisher/UI/Api/Live.hs). */
export interface RunView {
  tag: string | null;
  session: string | null;
  status: string | null;
  running: boolean;
  exit?: number | null;
  project?: string;
  command?: string;
  log?: string;
  root?: string;
  /** messages in the session's inbox its agent has not read yet */
  inboxPending?: number;
  /** the pid of another process the session runs in (its run lock is held there) */
  elsewhere?: number | null;
}

/** One ledger event as the stream sends it. */
export interface LiveEvent {
  seq: number;
  kind: string;
  data: Record<string, unknown>;
}

/** A step of the session's step log (fisher-ui/src/Fisher/UI/Api/Steps.hs). */
export interface StepHead {
  seq: number;
  path: string;
  hash: string;
  tool: string;
  at: number;
}

/** A step's check: the errors, the holes left, and ok when neither. */
export interface Verdict {
  errors: string[];
  holes: number;
  ok: boolean;
  prover: string;
  hash: string;
}

export interface StepView extends StepHead {
  text: string;
  verdict: Verdict | null;
}

/** What a hand back did: the new step, its check, and how the agent heard. */
export interface HandBack {
  seq: number;
  verdict: Verdict;
  delivered: "inbox" | "resume" | "pending";
  tag: string | null;
  after: number;
  message: string;
  /** why the resume could not start (delivered "pending": the change waits in the inbox) */
  error?: string;
  /** the pid of the process the session runs in, when that is not this server's (told through the inbox) */
  elsewhere?: number | null;
}

/** A run of fisher search (fisher-ui/src/Fisher/UI/Api/Search.hs). */
export interface SearchRow {
  run: string;
  dir: string;
  bench: string | null;
  file: string | null;
  project: string | null;
  mode: "flat" | "graph";
  started: number;
  closed: number;
  total: number;
  /** running, stopping, done, stopped, crashed, incomplete */
  status: string;
  running: boolean;
}

/** report.json's goal of a flat run. */
export interface ReportGoal {
  goal: string;
  status: string;
  closedBy?: string | null;
  attempts?: string[];
  closed_at_s?: number | null;
  disproof?: { verdict: string; tactic?: string; counterexample?: string };
  /** with retrieval or the library on: the facts in its brief, the lemmas offered and used */
  facts?: number;
  offered?: string[];
  used?: string[];
}

export interface GraphNodeJson {
  id: number;
  name: string;
  key?: string;
  root?: number;
  depth: number;
  status: string;
  reason?: string;
  splits: number[];
  attempts: number;
}

export interface GraphSplitJson {
  id: number;
  parent: number;
  lemmas: number[];
  status: string;
  reason?: string;
}

export interface GraphJson {
  roots: number[];
  nodes: GraphNodeJson[];
  splits: GraphSplitJson[];
  whiteboards?: { root: number; text: string }[];
}

export interface SearchDetail extends SearchRow {
  report: Record<string, unknown> | null;
  graph: GraphJson | null;
  whiteboards: { root: number | null; file: string | null; text: string }[];
  goals: ReportGoal[] | null;
  attempts: string[];
  auto: boolean;
  /** the project facts (--no-retrieval when off) and the lemma library (--no-library) */
  retrieval: boolean;
  library: boolean;
  /** changed: the only reason is that the file changed since the run started (force overrides) */
  apply: { allowed: boolean; reason: string | null; changed: boolean };
}

export interface KeyRow {
  name: string;
  set: boolean;
  /** the last 4 characters only (none for a value under 8) */
  last4: string | null;
}

export interface Settings {
  provider: string;
  model: string;
  autoformalizeModel: string;
  contextStrategy: string;
  strategies: string[];
  auto: { tactics: string[]; seconds: number; repair: boolean; disprove: boolean; settable: boolean };
  keys: KeyRow[];
  /** the .env variables that hold no secret, with their values */
  plain: { name: string; value: string }[];
  warnings: string[];
  saved?: string[];
}

export interface SettingsWrite {
  provider?: string;
  model?: string;
  autoformalizeModel?: string;
  contextStrategy?: string;
  keys?: { name: string; value: string }[];
}

const sid = (id: string) => `/api/sessions/${encodeURIComponent(id)}`;
const rid = (id: string) => `/api/search/${encodeURIComponent(id)}`;
const cid = (id: string) => `/api/create/${encodeURIComponent(id)}`;

export const api = {
  sessions: (f: SessionFilters = {}) => getJson<SessionSummary[]>(`/api/sessions${filterQuery(f)}`),
  session: (id: string) => getJson<SessionDetail>(`/api/sessions/${encodeURIComponent(id)}`),
  context: (id: string) => getJson<ContextView>(`/api/context/${encodeURIComponent(id)}`),
  runs: () => getJson<RunRoot[]>("/api/runs"),
  bench: (b: string) => getJson<BenchView>(`/api/runs/${encodeURIComponent(b)}`),
  start: (project: string, command: string) => postJson<{ tag: string }>("/api/sessions", { project, command }),
  tag: (tag: string) => getJson<RunView>(`/api/tags/${encodeURIComponent(tag)}`),
  live: (id: string) => getJson<RunView>(`/api/sessions/${encodeURIComponent(id)}/live`),
  stop: (id: string) => postJson<{ status: string }>(`/api/sessions/${encodeURIComponent(id)}/stop`, {}),
  stopTag: (tag: string) => postJson<{ status: string }>(`/api/tags/${encodeURIComponent(tag)}/stop`, {}),
  message: (id: string, text: string) =>
    postJson<{ queued: boolean }>(`/api/sessions/${encodeURIComponent(id)}/message`, { text }),
  eventsUrl: (id: string, after?: number) =>
    `/api/sessions/${encodeURIComponent(id)}/events${after ? `?after=${after}` : ""}`,
  steps: (id: string) => getJson<StepHead[]>(`${sid(id)}/steps`),
  step: (id: string, seq: number) => getJson<StepView>(`${sid(id)}/steps/${seq}`),
  verdict: (id: string, seq: number) => postJson<Verdict>(`${sid(id)}/steps/${seq}/verdict`, {}),
  handBack: (id: string, path: string, text: string, base: string) =>
    postJson<HandBack>(`${sid(id)}/handback`, { path, text, base }),
  searches: () => getJson<SearchRow[]>("/api/search"),
  search: (id: string) => getJson<SearchDetail>(rid(id)),
  startSearch: (project: string, file: string, graph: boolean, auto: boolean, retrieval = true, library = true) =>
    postJson<{ run: string }>("/api/search", { project, file, graph, auto, retrieval, library }),
  stopSearch: (id: string) => postJson<{ status: string }>(`${rid(id)}/stop`, {}),
  applySearch: (id: string, force = false) => postJson<{ written: string }>(`${rid(id)}/apply`, force ? { force: true } : {}),
  settings: () => getJson<Settings>("/api/settings"),
  saveSettings: (w: SettingsWrite) => postJson<Settings>("/api/settings", w),
  searchEventsUrl: (id: string, from = 0) => `${rid(id)}/events${from ? `?from=${from}` : ""}`,
  templates: () => getJson<TemplateList>("/api/templates"),
  create: (req: CreateRequest) => postJson<{ id: string }>("/api/create", req),
  createRun: (id: string) => getJson<CreateView>(cid(id)),
  stopCreate: (id: string) => postJson<{ status: string }>(`${cid(id)}/stop`, {}),
  retryCreate: (id: string) => postJson<{ id: string; from: number }>(`${cid(id)}/retry`, {}),
  createEventsUrl: (id: string, from = 0) => `${cid(id)}/events${from ? `?from=${from}` : ""}`,
};

/** The statuses of a run that has ended. */
export function ended(status: string | null | undefined): boolean {
  return !!status && !["starting", "running", "stopping"].includes(status);
}

/** Dollars as the bench reports print them: three decimals. */
export function dollars(d: number | null | undefined): string {
  if (d === null || d === undefined) return "";
  return `$${d.toFixed(3)}`;
}

export function when(secs: number): string {
  const t = new Date(secs * 1000);
  const p = (n: number) => String(n).padStart(2, "0");
  return `${t.getFullYear()}-${p(t.getMonth() + 1)}-${p(t.getDate())} ${p(t.getHours())}:${p(t.getMinutes())}`;
}

export function tokens(n: number): string {
  return n.toLocaleString("en-US");
}
