import { useEffect, useState } from "react";
import { Link, useParams, useSearchParams } from "react-router-dom";
import { api, dollars, type BenchView, type FileRow, type GoalRow, type RunRoot } from "../api";
import { Loaded, useLoad } from "../useLoad";
import css from "./views.module.css";

export default function Runs() {
  const { bench } = useParams();
  return bench ? <Bench key={bench} bench={bench} /> : <RunList />;
}

function RunList() {
  const load = useLoad(() => api.runs(), "runs");
  return (
    <main>
      <h1>Runs</h1>
      <Loaded load={load}>
        {(roots) => {
          if (roots.length === 0) return <p className="muted">No benchmark runs under the bench root.</p>;
          const by = new Map<string, RunRoot[]>();
          for (const r of roots) by.set(r.bench, [...(by.get(r.bench) ?? []), r]);
          return (
            <table className={css.table} data-testid="benches">
              <thead>
                <tr>
                  <th>Bench</th>
                  <th>Arms</th>
                </tr>
              </thead>
              <tbody>
                {[...by.entries()].map(([b, rs]) => (
                  <tr key={b}>
                    <td>
                      <Link to={`/runs/${encodeURIComponent(b)}`}>{b}</Link>
                    </td>
                    <td>
                      {rs.map((r) => (
                        <span key={r.root} className={css.chip} title={r.root}>
                          {r.arm}
                        </span>
                      ))}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          );
        }}
      </Loaded>
    </main>
  );
}

/** Each goal once, in the order the bench lists them, with its row in each arm. */
function byGoal(goals: GoalRow[]): { key: string; goal: string; rows: Map<string, GoalRow> }[] {
  const out: { key: string; goal: string; rows: Map<string, GoalRow> }[] = [];
  const index = new Map<string, number>();
  const seen = new Map<string, number>();
  for (const g of goals) {
    // a goal an arm ran twice gets a second line
    const n = seen.get(`${g.arm}\u0000${g.goal}`) ?? 0;
    seen.set(`${g.arm}\u0000${g.goal}`, n + 1);
    const key = n === 0 ? g.goal : `${g.goal}\u0000${n}`;
    let i = index.get(key);
    if (i === undefined) {
      i = out.length;
      index.set(key, i);
      out.push({ key, goal: g.goal, rows: new Map() });
    }
    out[i].rows.set(g.arm, g);
  }
  return out;
}

type SortKey = { col: "goal" } | { col: "solved" | "turns" | "dollars"; arm: string };

function GateLine({ v }: { v: BenchView }) {
  if (v.gateStatus === null) return null;
  return (
    <p className={css.gate} data-testid="gate" data-status={v.gateStatus}>
      <span className={css.label}>Gate</span>{" "}
      {v.gateStatus === "ok" && v.gate}
      {v.gateStatus === "pending" && <span className="muted">checking the proofs (report.py)...</span>}
      {v.gateStatus === "failed" && <span className={css.bad}>report.py failed or timed out</span>}
      {v.gateStatus === "bench in progress" && (
        <span className="muted">the bench is still running (a run has events and no report); checked once it is done</span>
      )}
    </p>
  );
}

function Bench({ bench }: { bench: string }) {
  // keyed by bench in Runs: another bench starts with no data and tick 0
  const [tick, setTick] = useState(0);
  const load = useLoad(() => api.bench(bench), String(tick));
  const [data, setData] = useState<BenchView | null>(null);
  useEffect(() => {
    if (load.state === "ok") setData(load.data);
  }, [load]);
  // the gate runs in the background: ask again until it is done, and stop
  // at the first poll that fails (its error is shown)
  const failed = load.state === "error";
  const pending = !failed && data?.gateStatus === "pending";
  // a bench still running: ask again now and then, more slowly
  const busy = !failed && data?.gateStatus === "bench in progress";
  useEffect(() => {
    if (!pending && !busy) return;
    const t = setTimeout(() => setTick((n) => n + 1), pending ? 2000 : 30000);
    return () => clearTimeout(t);
  }, [pending, busy, tick]);

  return (
    <main>
      <p className={css.crumbs}>
        <Link to="/runs">Runs</Link>
      </p>
      <h1>Bench {bench}</h1>
      {failed && <Loaded load={load}>{() => null}</Loaded>}
      {data ? <BenchTable v={data} /> : !failed && <Loaded load={load}>{(v) => <BenchTable v={v} />}</Loaded>}
    </main>
  );
}

function BenchTable({ v }: { v: BenchView }) {
  const [params, setParams] = useSearchParams();
  // null: the order the bench lists the goals in
  const [sort, setSort] = useState<SortKey | null>(null);
  const [desc, setDesc] = useState(false);
  if (v.arms.length === 0)
    return (
      <>
        <GateLine v={v} />
        <p className="muted">No runs for this bench.</p>
      </>
    );
  const pick = (k: "a" | "b", fallback: string | undefined) => {
    const x = params.get(k);
    return x && v.arms.includes(x) ? x : fallback;
  };
  const a = pick("a", v.arms[0])!;
  // the query may name one arm twice: B is then the first other arm
  const other = v.arms.find((x) => x !== a);
  const qb = pick("b", other);
  const b = qb === a ? other : qb;
  const shown = b ? [a, b] : [a];
  const choose = (k: "a" | "b", arm: string) => {
    const next = new URLSearchParams(params);
    next.set(k, arm);
    setParams(next, { replace: true });
  };

  const lines = byGoal(v.goals.filter((g) => shown.includes(g.arm)));
  const value = (row: Map<string, GoalRow>, s: SortKey): string | number => {
    if (s.col === "goal") return "";
    const g = row.get(s.arm);
    if (!g) return -1;
    if (s.col === "solved") return g.solved ? 1 : 0;
    return g[s.col] ?? -1;
  };
  const sorted = [...lines];
  if (sort?.col === "goal") sorted.sort((x, y) => x.goal.localeCompare(y.goal));
  else if (sort) sorted.sort((x, y) => (value(x.rows, sort) as number) - (value(y.rows, sort) as number));
  if (sort && desc) sorted.reverse();
  const head = (label: string, s: SortKey) => {
    const on = JSON.stringify(s) === JSON.stringify(sort);
    return (
      <th rowSpan={s.col === "goal" ? 2 : undefined} className={s.col === "goal" ? undefined : css.num} aria-sort={on ? (desc ? "descending" : "ascending") : "none"}>
        <button
          type="button"
          className={css.sortBtn}
          onClick={() => {
            if (on) setDesc(!desc);
            else {
              setSort(s);
              setDesc(false);
            }
          }}
        >
          {label}
          {on ? (desc ? " ↓" : " ↑") : ""}
        </button>
      </th>
    );
  };
  const totals = (arm: string) => {
    const gs = v.goals.filter((g) => g.arm === arm);
    return {
      solved: gs.filter((g) => g.solved).length,
      total: gs.length,
      turns: gs.reduce((n, g) => n + (g.turns ?? 0), 0),
      dollars: gs.reduce((n, g) => n + (g.dollars ?? 0), 0),
    };
  };

  return (
    <>
      <GateLine v={v} />
      {v.files && v.files.length > 0 && <Files files={v.files.filter((f) => shown.includes(f.arm))} />}
      <div className={css.filters}>
        <label>
          Arm A
          <select aria-label="Arm A" value={a} onChange={(e) => choose("a", e.target.value)}>
            {v.arms.map((x) => (
              <option key={x}>{x}</option>
            ))}
          </select>
        </label>
        {b && (
          <label>
            Arm B
            <select aria-label="Arm B" value={b} onChange={(e) => choose("b", e.target.value)}>
              {v.arms.map((x) => (
                <option key={x}>{x}</option>
              ))}
            </select>
          </label>
        )}
      </div>
      <div className={css.scroll}>
        <table className={css.table} data-testid="bench-goals">
          <thead>
            <tr>
              {head("Goal", { col: "goal" })}
              {shown.map((arm) => (
                <th key={arm} colSpan={4} className={css.armHead} data-testid="arm-head">
                  {arm}
                </th>
              ))}
            </tr>
            <tr>
              {shown.map((arm) => (
                <FragmentHeads key={arm} arm={arm} head={head} />
              ))}
            </tr>
          </thead>
          <tbody>
            {sorted.map((l) => (
              <tr key={l.key} data-goal={l.goal}>
                <td className={css.mono}>{l.goal}</td>
                {shown.map((arm) => {
                  const g = l.rows.get(arm);
                  return g ? (
                    <Cells key={arm} g={g} />
                  ) : (
                    <td key={arm} colSpan={4} className="muted">
                      not run
                    </td>
                  );
                })}
              </tr>
            ))}
          </tbody>
          <tfoot>
            <tr data-testid="totals">
              <th>Total</th>
              {shown.map((arm) => {
                const t = totals(arm);
                return (
                  <TotalCells key={arm} t={t} />
                );
              })}
            </tr>
          </tfoot>
        </table>
      </div>
    </>
  );
}

function FragmentHeads({ arm, head }: { arm: string; head: (l: string, s: SortKey) => JSX.Element }) {
  return (
    <>
      {head("Solved", { col: "solved", arm })}
      {head("Turns", { col: "turns", arm })}
      {head("Dollars", { col: "dollars", arm })}
      <th>Closed by</th>
    </>
  );
}

function Cells({ g }: { g: GoalRow }) {
  return (
    <>
      <td className={`${css.num} ${g.solved ? css.good : css.bad}`}>{g.solved ? "yes" : "no"}</td>
      <td className={css.num}>{g.turns ?? ""}</td>
      <td className={css.num}>{dollars(g.dollars)}</td>
      <td>
        {g.closedBy ?? ""}
        {g.facts !== undefined && (
          <span className="muted" data-testid="retrieval">
            {g.closedBy ? " " : ""}facts {g.facts}, offered {g.offered ?? 0}, used {g.used ?? 0}
          </span>
        )}
      </td>
    </>
  );
}

/** Bench w: each run is a whole file; its closed goals, dollars and retrieval totals. */
function Files({ files }: { files: FileRow[] }) {
  const n = (x: number | null) => (x === null ? "" : String(x));
  return (
    <div className={css.scroll}>
      <table className={css.table} data-testid="bench-files">
        <thead>
          <tr>
            <th>Arm</th>
            <th>Run</th>
            <th>File</th>
            <th className={css.num}>Closed</th>
            <th className={css.num}>Dollars</th>
            <th className={css.num}>Facts per goal</th>
            <th className={css.num}>Lemmas offered</th>
            <th className={css.num}>Lemmas used</th>
          </tr>
        </thead>
        <tbody>
          {files.map((f) => (
            <tr key={`${f.arm}/${f.run}`} data-testid="file-row" data-arm={f.arm}>
              <td>{f.arm}</td>
              <td>{f.run}</td>
              <td className={css.mono}>{f.file ?? ""}</td>
              <td className={css.num}>
                {f.closed}
                {f.total !== null ? `/${f.total}` : ""}
                {!f.report && <span className="muted"> (no report)</span>}
              </td>
              <td className={css.num}>{dollars(f.dollars)}</td>
              <td className={css.num}>{n(f.factsMean)}</td>
              <td className={css.num}>{n(f.offered)}</td>
              <td className={css.num}>{n(f.used)}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function TotalCells({ t }: { t: { solved: number; total: number; turns: number; dollars: number } }) {
  return (
    <>
      <td className={css.num}>
        {t.solved}/{t.total}
      </td>
      <td className={css.num}>{t.turns}</td>
      <td className={css.num}>{dollars(t.dollars)}</td>
      <td />
    </>
  );
}
