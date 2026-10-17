import { Link, useSearchParams } from "react-router-dom";
import { api, dollars, when, type SessionFilters, type SessionSummary } from "../api";
import StartRun from "../components/StartRun";
import { Loaded, useLoad } from "../useLoad";
import css from "./views.module.css";

const filters = [
  { key: "project", label: "Project" },
  { key: "prover", label: "Prover" },
  { key: "model", label: "Model" },
  { key: "strategy", label: "Context" },
] as const;

type FilterKey = (typeof filters)[number]["key"];

const proverNames: Record<string, string> = { lean4: "Lean", rocq: "Rocq", isabelle: "Isabelle" };

export function proverName(p: string | null): string {
  return p ? (proverNames[p] ?? p) : "";
}

function baseName(p: string | null): string {
  if (!p) return "";
  const parts = p.split("/").filter(Boolean);
  return parts[parts.length - 1] ?? p;
}

function optionsOf(all: SessionSummary[], k: FilterKey): string[] {
  const vs = new Set<string>();
  for (const s of all) {
    const v = s[k];
    if (v) vs.add(v);
  }
  return [...vs].sort();
}

function optionLabel(k: FilterKey, v: string): string {
  if (k === "prover") return proverName(v);
  if (k === "project") return baseName(v);
  return v;
}

export default function Sessions() {
  const [params, setParams] = useSearchParams();
  const chosen: SessionFilters = {};
  for (const f of filters) {
    const v = params.get(f.key);
    if (v) chosen[f.key] = v;
  }
  // the options come from every session; the table from the filtered list
  const all = useLoad(() => api.sessions(), "all");
  const key = JSON.stringify(chosen);
  const shown = useLoad(() => api.sessions(chosen), key);

  const choose = (k: FilterKey, v: string) => {
    const next = new URLSearchParams(params);
    if (v) next.set(k, v);
    else next.delete(k);
    setParams(next, { replace: true });
  };

  return (
    <main>
      <h1>Sessions</h1>
      <StartRun projects={all.state === "ok" ? optionsOf(all.data, "project") : []} />
      <div className={css.filters} aria-label="Filters">
        {filters.map((f) => (
          <label key={f.key}>
            {f.label}
            <select
              aria-label={f.label}
              value={chosen[f.key] ?? ""}
              onChange={(e) => choose(f.key, e.target.value)}
            >
              <option value="">All</option>
              {all.state === "ok" &&
                optionsOf(all.data, f.key).map((v) => (
                  <option key={v} value={v} title={v}>
                    {optionLabel(f.key, v)}
                  </option>
                ))}
              {chosen[f.key] && all.state === "ok" && !optionsOf(all.data, f.key).includes(chosen[f.key]!) && (
                <option value={chosen[f.key]}>{optionLabel(f.key, chosen[f.key]!)}</option>
              )}
            </select>
          </label>
        ))}
      </div>
      <Loaded load={shown}>
        {(rows) =>
          rows.length === 0 ? (
            Object.values(chosen).some(Boolean) ? (
              <p className="muted">No sessions match.</p>
            ) : (
              <p className="muted" data-testid="no-sessions">
                No sessions yet.
              </p>
            )
          ) : (
            <>
              <p className="muted" data-testid="session-count">
                {rows.length} {rows.length === 1 ? "session" : "sessions"}
              </p>
              <div className={css.scroll}>
                <table className={css.table} data-testid="sessions">
                  <thead>
                    <tr>
                      <th>Session</th>
                      <th>Project</th>
                      <th>Prover</th>
                      <th>Model</th>
                      <th>Context</th>
                      <th>Started</th>
                      <th className={css.num}>Turns</th>
                      <th className={css.num}>Dollars</th>
                      <th>Result</th>
                    </tr>
                  </thead>
                  <tbody>
                    {rows.map((s) => (
                      <tr key={s.id} data-session={s.id}>
                        <td>
                          <Link to={`/session/${encodeURIComponent(s.id)}`}>{s.title ?? s.id}</Link>
                          {s.title && <div className={css.sub}>{s.id}</div>}
                        </td>
                        <td title={s.project ?? ""}>{baseName(s.project)}</td>
                        <td>{proverName(s.prover)}</td>
                        <td>{s.model ?? ""}</td>
                        <td>{s.strategy}</td>
                        <td className={css.nowrap}>{when(s.started)}</td>
                        <td className={css.num}>{s.turns}</td>
                        <td className={css.num}>{dollars(s.dollars)}</td>
                        <td>{s.result ?? ""}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </>
          )
        }
      </Loaded>
    </main>
  );
}
