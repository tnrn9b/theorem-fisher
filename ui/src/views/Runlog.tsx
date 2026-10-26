import type { ReactNode } from "react";
import { Link, useParams, useSearchParams } from "react-router-dom";
import { api, dollars, runKinds, tokens, when, type RunlogDetail, type RunlogEvent, type RunlogRow } from "../api";
import { Loaded, useLoad } from "../useLoad";
import css from "./views.module.css";

/** /runlog: every logged run (fisher runs list); /runlog/:id: one run. */
export default function Runlog() {
  const { id } = useParams();
  return id ? <RunPage key={id} id={id} /> : <RunList />;
}

function seconds(s: number): string {
  if (s < 60) return `${s.toFixed(1)} s`;
  if (s < 3600) return `${Math.floor(s / 60)} min ${Math.round(s % 60)} s`;
  return `${Math.floor(s / 3600)} h ${Math.round((s % 3600) / 60)} min`;
}

function duration(r: RunlogRow): string {
  return r.ended_at === null ? "" : seconds(Math.max(0, r.ended_at - r.started_at));
}

function RunList() {
  const [params, setParams] = useSearchParams();
  const kind = params.get("kind") ?? "";
  const project = params.get("project") ?? "";
  const load = useLoad(() => api.runlog({ kind, project }), `${kind}\u0000${project}`);
  const projects = useLoad(() => api.learn().then((v) => v.projects), "projects");
  const choose = (k: "kind" | "project", v: string) => {
    const next = new URLSearchParams(params);
    if (v) next.set(k, v);
    else next.delete(k);
    setParams(next, { replace: true });
  };
  const projectOptions = projects.state === "ok" ? projects.data : [];
  return (
    <main>
      <h1>Run log</h1>
      <p className="muted">
        Every run Fisher logged (searches and their attempts, launches, chats, the web app's runs, cron jobs and imports),
        newest first.
      </p>
      <div className={css.filters} role="group" aria-label="Filters">
        <label>
          Kind
          <select aria-label="Kind" value={kind} onChange={(e) => choose("kind", e.target.value)}>
            <option value="">All</option>
            {runKinds.map((k) => (
              <option key={k} value={k}>
                {k}
              </option>
            ))}
          </select>
        </label>
        <label>
          Project
          <select aria-label="Project" value={project} onChange={(e) => choose("project", e.target.value)}>
            <option value="">All</option>
            {projectOptions.map((p) => (
              <option key={p} value={p} title={p}>
                {p}
              </option>
            ))}
            {project && !projectOptions.includes(project) && <option value={project}>{project}</option>}
          </select>
        </label>
      </div>
      <Loaded load={load}>
        {(rows) =>
          rows.length === 0 ? (
            <p className="muted">
              {kind || project ? "No runs match." : "No runs logged yet (fisher runs import reads earlier ones)."}
            </p>
          ) : (
            <div className={css.scroll}>
              <table className={css.table} data-testid="runlog">
                <thead>
                  <tr>
                    <th>Run</th>
                    <th>Kind</th>
                    <th>Started</th>
                    <th>Took</th>
                    <th>Project</th>
                    <th>Backend</th>
                    <th>Model</th>
                    <th>Outcome</th>
                    <th className={css.num}>Cost</th>
                    <th className={css.num}>Turns</th>
                  </tr>
                </thead>
                <tbody>
                  {rows.map((r) => (
                    <tr key={r.id} data-testid="runlog-row" data-run={r.id}>
                      <td className={css.mono}>
                        <Link to={`/runlog/${encodeURIComponent(r.id)}`}>{r.id}</Link>
                      </td>
                      <td className={css.nowrap}>{r.kind}</td>
                      <td className={css.nowrap}>{when(r.started_at)}</td>
                      <td className={css.nowrap}>{duration(r)}</td>
                      <td className={css.sub} title={r.project ?? undefined}>
                        {r.project ?? ""}
                      </td>
                      <td>{r.backend ?? ""}</td>
                      <td>{r.model ?? ""}</td>
                      <td>
                        <Outcome o={r.outcome} />
                      </td>
                      <td className={css.num}>{dollars(r.cost_usd)}</td>
                      <td className={css.num}>{r.turns ?? ""}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )
        }
      </Loaded>
    </main>
  );
}

function Outcome({ o }: { o: string | null }) {
  if (!o) return <span className="muted">running or unknown</span>;
  const cls = o === "closed" || o === "ok" || o === "completed" ? css.good : o === "cancelled" ? undefined : css.bad;
  return <span className={cls}>{o}</span>;
}

function RunPage({ id }: { id: string }) {
  const load = useLoad(() => api.runlogRun(id), id);
  return (
    <main>
      <p className={css.crumbs}>
        <Link to="/runlog">Run log</Link>
      </p>
      <h1 className={css.mono}>{id}</h1>
      <Loaded load={load}>{(v) => <RunBody v={v} />}</Loaded>
    </main>
  );
}

function RunBody({ v }: { v: RunlogDetail }) {
  const r = v.run;
  const fields: [string, ReactNode][] = [
    ["kind", r.kind],
    ["outcome", <Outcome key="o" o={r.outcome} />],
    ["started", when(r.started_at)],
    ["took", duration(r)],
    ["project", r.project],
    ["prover", r.prover],
    ["backend", r.backend],
    ["model", r.model],
    ["tier", r.tier],
    ["workflow", r.workflow],
    ["exit", r.exit === null ? null : String(r.exit)],
    ["cost", r.cost_usd === null ? null : dollars(r.cost_usd)],
    ["tokens in", r.input_tokens === null ? null : tokens(r.input_tokens)],
    ["tokens out", r.output_tokens === null ? null : tokens(r.output_tokens)],
    ["turns", r.turns === null ? null : String(r.turns)],
    ["capture", r.capture],
    ["raw files", r.raw_dir],
  ];
  return (
    <>
      <div className={css.meta} data-testid="runlog-meta">
        {fields
          .filter(([, x]) => x !== null && x !== "")
          .map(([k, x]) => (
            <span key={k}>
              <span className={css.label}>{k}</span> {x}
            </span>
          ))}
        {r.parent && (
          <span>
            <span className={css.label}>parent</span>{" "}
            <Link className={css.mono} to={`/runlog/${encodeURIComponent(r.parent)}`}>
              {r.parent}
            </Link>
          </span>
        )}
      </div>
      {r.argv.length > 0 && <pre className={css.file}>{r.argv.join(" ")}</pre>}

      <h2>Goals</h2>
      {v.goals.length === 0 ? (
        <p className="muted">No goals recorded for this run.</p>
      ) : (
        <div className={css.scroll}>
          <table className={css.table}>
            <thead>
              <tr>
                <th>Goal</th>
                <th>Shape</th>
                <th>Status</th>
                <th>Closed by</th>
                <th className={css.num}>Seconds</th>
                <th className={css.num}>Dollars</th>
              </tr>
            </thead>
            <tbody>
              {v.goals.map((g) => (
                <tr key={g.name} data-testid="runlog-goal">
                  <td>
                    <div className={css.mono}>{g.name}</div>
                    {g.statement !== g.name && <div className={css.sub}>{g.statement}</div>}
                  </td>
                  <td className={css.sub}>{g.shape}</td>
                  <td>{g.status}</td>
                  <td>{g.closed_by ?? ""}</td>
                  <td className={css.num}>{g.seconds_to_close === null ? "" : g.seconds_to_close.toFixed(1)}</td>
                  <td className={css.num}>{dollars(g.dollars_to_close)}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      <h2>Events</h2>
      {v.truncated && (
        <p className="muted" role="note">
          The first {v.events.length} events are shown; fisher runs show {r.id} prints them all.
        </p>
      )}
      {v.events.length === 0 ? (
        <p className="muted">No events recorded for this run.</p>
      ) : (
        <div className={css.scroll}>
          <table className={css.table}>
            <thead>
              <tr>
                <th className={css.num}>#</th>
                <th className={css.num}>t</th>
                <th>Kind</th>
                <th>Data</th>
              </tr>
            </thead>
            <tbody>
              {v.events.map((e) => (
                <EventRow key={e.seq} e={e} />
              ))}
            </tbody>
          </table>
        </div>
      )}
    </>
  );
}

function EventRow({ e }: { e: RunlogEvent }) {
  const text = JSON.stringify(e.data, null, 2) ?? "";
  const short = text.length > 400;
  return (
    <tr data-testid="runlog-event">
      <td className={css.num}>{e.seq}</td>
      <td className={css.num}>{e.t.toFixed(1)}</td>
      <td className={css.nowrap}>{e.kind}</td>
      <td>
        {short ? (
          <details>
            <summary className={css.cell}>{text.slice(0, 200)}...</summary>
            <pre className={css.cell}>{text}</pre>
          </details>
        ) : (
          <pre className={css.cell}>{text}</pre>
        )}
      </td>
    </tr>
  );
}
