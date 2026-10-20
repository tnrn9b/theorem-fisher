import { useCallback, useEffect, useMemo, useRef, useState, type FormEvent } from "react";
import { Link, useNavigate, useParams, useSearchParams } from "react-router-dom";
import { api, ApiError, dollars, when, type SearchDetail, type SearchRow } from "../api";
import { foldFlat, foldGraph, foldRetrieval, foldTiers, type SearchEvent } from "../board";
import GoalRow from "../components/GoalRow";
import LemmaGraph from "../components/LemmaGraph";
import Whiteboard from "../components/Whiteboard";
import bcss from "../components/board.module.css";
import { Loaded, useLoad } from "../useLoad";
import css from "./views.module.css";

/** /search: the runs and a form to start one; /search/:run: its board. */
export default function GoalBoard() {
  const { run } = useParams();
  return run ? <Board key={run} run={run} /> : <SearchList />;
}

// ------------------------------------------------------------------ list

function StartSearch({ projects }: { projects: string[] }) {
  const nav = useNavigate();
  // the New project view links here with its project and sample
  const [params] = useSearchParams();
  const [project, setProject] = useState(params.get("start") ?? "");
  const [file, setFile] = useState(params.get("file") ?? "");
  const [graph, setGraph] = useState(false);
  const [auto, setAuto] = useState(true);
  const [retrieval, setRetrieval] = useState(true);
  const [library, setLibrary] = useState(true);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const submit = (e: FormEvent) => {
    e.preventDefault();
    setBusy(true);
    setError(null);
    api.startSearch(project.trim(), file.trim(), graph, auto, retrieval, library).then(
      (r) => nav(`/search/${encodeURIComponent(r.run)}`),
      (err: Error) => {
        setBusy(false);
        setError(err.message);
      },
    );
  };
  return (
    <form className={bcss.startSearch} onSubmit={submit} aria-label="Start a search">
      <label className={bcss.grow}>
        Project directory
        <input type="text" value={project} onChange={(e) => setProject(e.target.value)} list="search-projects" required />
        <datalist id="search-projects">
          {projects.map((p) => (
            <option key={p} value={p} />
          ))}
        </datalist>
      </label>
      <label className={bcss.grow}>
        Lean file (in the project)
        <input type="text" value={file} onChange={(e) => setFile(e.target.value)} placeholder="Step/Graft.lean" required />
      </label>
      <label className={bcss.check}>
        <input type="checkbox" checked={graph} onChange={(e) => setGraph(e.target.checked)} />
        Lemma graph
      </label>
      <label className={bcss.check}>
        <input type="checkbox" checked={auto} onChange={(e) => setAuto(e.target.checked)} />
        Automation first
      </label>
      <label className={bcss.check} title="Project facts in each brief (off: --no-retrieval)">
        <input type="checkbox" checked={retrieval} onChange={(e) => setRetrieval(e.target.checked)} />
        Retrieval
      </label>
      <label className={bcss.check} title="The lemma library (off: --no-library)">
        <input type="checkbox" checked={library} onChange={(e) => setLibrary(e.target.checked)} />
        Library
      </label>
      <button type="submit" disabled={busy}>
        {busy ? "Starting..." : "Start search"}
      </button>
      {error && (
        <p className={bcss.error} role="alert">
          Could not start: {error}
        </p>
      )}
    </form>
  );
}

function SearchList() {
  const load = useLoad(() => api.searches(), "searches");
  return (
    <main>
      <h1>Search runs</h1>
      <Loaded load={load}>
        {(rows) => (
          <>
            <StartSearch projects={[...new Set(rows.filter((r) => !r.bench && r.project).map((r) => r.project!))]} />
            {rows.length === 0 ? (
              <p className="muted">No runs of fisher search yet.</p>
            ) : (
              <div className={css.scroll}>
                <table className={css.table} data-testid="search-runs">
                  <thead>
                    <tr>
                      <th>Run</th>
                      <th>File</th>
                      <th>Mode</th>
                      <th>Status</th>
                      <th className={css.num}>Closed</th>
                      <th>Started</th>
                    </tr>
                  </thead>
                  <tbody>
                    {rows.map((r: SearchRow) => (
                      <tr key={r.run} data-testid="search-row" data-run={r.run}>
                        <td className={css.nowrap}>
                          <Link to={`/search/${encodeURIComponent(r.run)}`}>{r.run}</Link>
                        </td>
                        <td className={css.mono}>{r.file ?? ""}</td>
                        <td>{r.mode}</td>
                        <td>{r.status}</td>
                        <td className={css.num}>
                          {r.closed} of {r.total}
                        </td>
                        <td className={css.nowrap}>{r.started ? when(r.started) : ""}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            )}
          </>
        )}
      </Loaded>
    </main>
  );
}

// ------------------------------------------------------------------ board

function eventText(e: SearchEvent): string {
  const skip = new Set(["event", "t", "stats"]);
  const rest = Object.entries(e)
    .filter(([k]) => !skip.has(k))
    .map(([k, v]) => `${k} ${typeof v === "string" ? v : JSON.stringify(v)}`)
    .join(", ");
  return `${typeof e.t === "number" ? e.t.toFixed(1).padStart(7) : "".padStart(7)}  ${e.event}${rest ? `: ${rest}` : ""}`;
}

function Board({ run }: { run: string }) {
  const nav = useNavigate();
  const [detail, setDetail] = useState<SearchDetail | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [events, setEvents] = useState<SearchEvent[]>([]);
  const [streamState, setStreamState] = useState<"connecting" | "open" | "ended" | "waiting">("connecting");
  const [confirmApply, setConfirmApply] = useState<string | null>(null);
  const [note, setNote] = useState<string | null>(null);
  const [actionError, setActionError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const offset = useRef(0);

  const reload = useCallback(
    () =>
      api.search(run).then(
        (d) => {
          setDetail(d);
          setLoadError(null);
        },
        (e: Error) => setLoadError(e.message),
      ),
    [run],
  );

  useEffect(() => {
    void reload();
  }, [reload]);

  // the events: each line once (the id is its end offset in the file).
  // The stream ends with a status: "rewritten" (the file was truncated or
  // replaced) starts again from the top; "unknown" (no process of the
  // server writes it and it has been still a while) tries again later
  // from where it was; anything else is the run's end.
  useEffect(() => {
    let es: EventSource | null = null;
    let retry: ReturnType<typeof setTimeout> | null = null;
    let gone = false;
    const connect = (from: number) => {
      if (gone) return;
      setStreamState("connecting");
      const s = new EventSource(api.searchEventsUrl(run, from));
      es = s;
      s.onopen = () => setStreamState("open");
      s.onmessage = (m: MessageEvent<string>) => {
        const at = Number(m.lastEventId);
        if (Number.isFinite(at) && at <= offset.current) return;
        let e: SearchEvent;
        try {
          e = JSON.parse(m.data) as SearchEvent;
        } catch {
          return;
        }
        if (Number.isFinite(at)) offset.current = at;
        setEvents((xs) => [...xs, e]);
      };
      s.addEventListener("end", (m) => {
        s.close();
        let status = "";
        try {
          status = String((JSON.parse((m as MessageEvent<string>).data) as { status?: string }).status ?? "");
        } catch {
          // no status
        }
        if (status === "rewritten") {
          offset.current = 0;
          setEvents([]);
          connect(0);
        } else if (status === "unknown") {
          setStreamState("waiting");
          retry = setTimeout(() => connect(offset.current), 5000);
        } else {
          setStreamState("ended");
          void reload();
        }
      });
      s.onerror = () => {
        if (s.readyState === EventSource.CLOSED) {
          // 204: the run had ended already
          setStreamState("ended");
          void reload();
        } else setStreamState("connecting");
      };
    };
    offset.current = 0;
    setEvents([]);
    connect(0);
    return () => {
      gone = true;
      es?.close();
      if (retry) clearTimeout(retry);
    };
  }, [run, reload]);

  // while it runs, the status now and then (stop, a crash)
  useEffect(() => {
    if (!detail?.running) return;
    const t = setInterval(() => void reload(), 2000);
    return () => clearInterval(t);
  }, [detail?.running, reload]);

  const final = !!detail && !!detail.report && !detail.running;
  const goals = useMemo(() => foldFlat(events, final ? detail!.goals : null), [events, final, detail]);
  const graph = useMemo(
    () =>
      foldGraph(
        events,
        final ? detail!.graph : null,
        final && Array.isArray((detail!.report as Record<string, unknown> | null)?.retrieval)
          ? ((detail!.report as Record<string, unknown>).retrieval as { node?: number | null; facts?: number; offered?: string[]; used?: string[] }[])
          : [],
      ),
    [events, final, detail],
  );
  const runRetrieval = useMemo(() => foldRetrieval(events), [events]);
  const runTiers = useMemo(() => foldTiers(events), [events]);

  if (!detail)
    return (
      <main>
        <p className={css.crumbs}>
          <Link to="/search">Search runs</Link>
        </p>
        {loadError ? <p role="alert">Could not load: {loadError}</p> : <p className="muted">Loading...</p>}
      </main>
    );

  const isGraph = detail.mode === "graph";
  const report = detail.report as Record<string, unknown> | null;
  const total = (() => {
    if (isGraph) {
      const d = report?.dollars as { total?: number } | undefined;
      if (typeof d?.total === "number") return d.total;
      return Object.values(graph.dollars).reduce((a, b) => a + b, 0);
    }
    const atts = (report?.attempts as { stats?: { cost_usd?: number } }[] | undefined) ?? [];
    let sum = atts.reduce((a, x) => a + (x.stats?.cost_usd ?? 0), 0);
    if (!report) for (const e of events) if (e.event === "finished") sum += (e.stats as { cost_usd?: number } | undefined)?.cost_usd ?? 0;
    return sum;
  })();
  const closed = isGraph
    ? graph.roots.filter((r) => graph.nodes.get(r)?.status === "proved").length
    : goals.filter((g) => g.status === "closed").length;
  const goalCount = isGraph ? graph.roots.length : goals.length;

  const canRetry = !!detail.project && !!detail.file;
  const retry = () => {
    if (!canRetry) return;
    setBusy(true);
    setActionError(null);
    api.startSearch(detail.project!, detail.file!, isGraph, detail.auto, detail.retrieval ?? true, detail.library ?? true).then(
      (r) => {
        setBusy(false);
        nav(`/search/${encodeURIComponent(r.run)}`);
      },
      (e: Error) => {
        setBusy(false);
        setActionError(`Could not start: ${e.message}`);
      },
    );
  };
  const stop = () => {
    setBusy(true);
    api.stopSearch(run).then(
      () => {
        setBusy(false);
        void reload();
      },
      (e: Error) => {
        setBusy(false);
        setActionError(e.message);
      },
    );
  };
  const apply = (force: boolean) => {
    setBusy(true);
    setActionError(null);
    setNote(null);
    setConfirmApply(null);
    api.applySearch(run, force).then(
      (r) => {
        setBusy(false);
        setNote(`Written to ${r.written}.`);
        void reload();
      },
      (e: Error) => {
        setBusy(false);
        // the file changed since the run started: ask before writing over it
        if (e instanceof ApiError && e.status === 409 && e.body?.changed === true) setConfirmApply(e.message);
        else setActionError(`Not applied: ${e.message}`);
      },
    );
  };

  return (
    <main>
      <p className={css.crumbs}>
        <Link to="/search">Search runs</Link>
      </p>
      <h1 className={css.mono}>{detail.file ?? run}</h1>
      <div className={css.meta}>
        <span>{run}</span>
        <span>{isGraph ? "lemma graph" : "flat"}</span>
        <span>
          status <strong data-testid="run-status">{detail.status}</strong>
        </span>
        <span data-testid="run-closed">
          {closed} of {goalCount} closed
        </span>
        {total > 0 && <span>{dollars(total)}</span>}
        {detail.started > 0 && <span>{when(detail.started)}</span>}
        {graph.stopped && <span>stopped: {graph.stopped}</span>}
        {detail.retrieval === false && <span data-testid="no-retrieval">no retrieval</span>}
        {detail.library === false && <span data-testid="no-library">no library</span>}
        <span>events {streamState}</span>
      </div>
      {detail.project && <div className={css.sub}>{detail.project}</div>}
      {(runTiers.skipped.length > 0 || runTiers.ledgerUnread.length > 0) && (
        <p className={css.meta} data-testid="run-tiers">
          {runTiers.skipped.length > 0 && <span>tiers skipped: {runTiers.skipped.join(", ")}</span>}
          {runTiers.ledgerUnread.length > 0 && <span>ledger unread: {runTiers.ledgerUnread.join(", ")}</span>}
        </p>
      )}
      {(runRetrieval.index || runRetrieval.saved || runRetrieval.problems.length > 0 || typeof report?.facts_mean === "number") && (
        <p className={css.meta} data-testid="run-retrieval">
          {runRetrieval.index && <span>index: {runRetrieval.index}</span>}
          {typeof report?.facts_mean === "number" && (
            <span>
              facts per goal {String(report.facts_mean)}, lemmas offered {String(report.offered_total ?? 0)}, used{" "}
              {String(report.used_total ?? 0)}
            </span>
          )}
          {runRetrieval.saved && <span>library saved {runRetrieval.saved}</span>}
          {runRetrieval.problems.map((p) => (
            <span key={p} className={bcss.error}>
              library problem: {p}
            </span>
          ))}
        </p>
      )}
      <div className={bcss.controls}>
        {detail.running && (
          <button type="button" onClick={stop} disabled={busy || detail.status === "stopping"}>
            Stop
          </button>
        )}
        {!detail.running && canRetry && (
          <button type="button" onClick={retry} disabled={busy} title="fisher search has no single-goal retry: this searches the whole file as it is now">
            Run again
          </button>
        )}
        {!detail.running && detail.report && (
          <button type="button" onClick={() => apply(false)} disabled={busy || !(detail.apply.allowed || detail.apply.changed)} data-testid="apply">
            Apply to {detail.file ?? "the file"}
          </button>
        )}
        {confirmApply && (
          <span className={bcss.note} role="alert" data-testid="apply-confirm">
            {confirmApply}. Write the merged file over it anyway?{" "}
            <button type="button" onClick={() => apply(true)} disabled={busy} data-testid="apply-force">
              Apply anyway
            </button>{" "}
            <button type="button" onClick={() => setConfirmApply(null)}>
              Cancel
            </button>
          </span>
        )}
        {!detail.apply.allowed && !detail.apply.changed && detail.apply.reason && detail.report && (
          <span className={bcss.note} data-testid="apply-reason">
            {detail.apply.reason}
          </span>
        )}
        {note && (
          <span className={bcss.note} role="status">
            {note}
          </span>
        )}
        {actionError && (
          <span className={bcss.error} role="alert">
            {actionError}
          </span>
        )}
      </div>
      {!detail.running && canRetry && <p className={bcss.note}>A retry starts a new run on the whole file as it is now; fisher search has no single-goal flag.</p>}

      {isGraph ? (
        <>
          <h2>Lemma graph</h2>
          <LemmaGraph graph={graph} />
          {detail.whiteboards.length > 0 && <h2>Whiteboards</h2>}
          {detail.whiteboards.map((w, i) => (
            <Whiteboard key={w.file ?? i} root={w.root} name={w.root !== null ? graph.nodes.get(w.root)?.name ?? null : null} text={w.text} />
          ))}
        </>
      ) : (
        <>
          <h2>Goals</h2>
          {goals.length === 0 ? (
            <p className="muted">No goals in the events yet.</p>
          ) : (
            <div className={css.scroll}>
              <table className={`${css.table} ${bcss.goalTable}`} data-testid="goals">
                <thead>
                  <tr>
                    <th>Goal</th>
                    <th>Status</th>
                    <th>Closed by</th>
                    <th className={css.num}>Attempts</th>
                    <th>Marks</th>
                    <th />
                  </tr>
                </thead>
                <tbody>
                  {goals.map((g) => (
                    <GoalRow key={g.goal} goal={g} onRetry={!detail.running && canRetry ? retry : undefined} retrying={busy} />
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </>
      )}

      <h2>Events</h2>
      {events.length === 0 ? (
        <p className="muted">No events yet.</p>
      ) : (
        <ol className={bcss.events} data-testid="events">
          {events.map((e, i) => (
            <li key={i}>{eventText(e)}</li>
          ))}
        </ol>
      )}
      {detail.attempts.length > 0 && (
        <>
          <h2>Attempts</h2>
          <div className={css.sub}>
            {detail.attempts.map((a) => (
              <div key={a}>{`${detail.dir}/attempts/${a}`}</div>
            ))}
          </div>
        </>
      )}
    </main>
  );
}
