import { Link, useParams } from "react-router-dom";
import { api, type CtxEvent } from "../api";
import { Loaded, useLoad } from "../useLoad";
import css from "./views.module.css";

function detailText(e: CtxEvent): string {
  return Object.entries(e.detail)
    .map(([k, v]) => `${k}: ${typeof v === "string" ? v : JSON.stringify(v)}`)
    .join("\n");
}

export default function Context() {
  const { id = "" } = useParams();
  const load = useLoad(() => api.context(id), id);
  return (
    <main>
      <p className={css.crumbs}>
        <Link to="/sessions">Sessions</Link> / <Link to={`/session/${encodeURIComponent(id)}`}>{id}</Link>
      </p>
      <h1>Context</h1>
      <Loaded load={load}>
        {(c) => (
          <>
            <p className={css.meta}>
              <span data-testid="ctx-strategy">strategy {c.strategy}</span>
            </p>
            {c.now === null && c.events.length === 0 && c.summaries.length === 0 && (
              <p className="muted">This session kept no context file, events or summaries.</p>
            )}
            {c.now !== null && (
              <section>
                <h2>Context file now</h2>
                <pre className={css.file} data-testid="ctx-now">
                  {c.now}
                </pre>
              </section>
            )}
            {c.prev !== null && (
              <section>
                <h2>Before the last edit</h2>
                <pre className={css.file} data-testid="ctx-prev">
                  {c.prev}
                </pre>
              </section>
            )}
            {c.events.length > 0 && (
              <section>
                <h2>Events</h2>
                <table className={css.table} data-testid="ctx-events">
                  <thead>
                    <tr>
                      <th className={css.num}>Turn</th>
                      <th>Event</th>
                      <th>Detail</th>
                    </tr>
                  </thead>
                  <tbody>
                    {c.events.map((e, i) => (
                      <tr key={i}>
                        <td className={css.num}>{e.turn}</td>
                        <td className={css.mono}>{e.kind}</td>
                        <td>
                          <pre className={css.cell}>{detailText(e)}</pre>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </section>
            )}
            {c.summaries.length > 0 && (
              <section>
                <h2>Compression summaries</h2>
                {c.summaries.map((s, i) => (
                  <div key={i} className={css.summary} data-testid="ctx-summary">
                    <p className={css.meta}>
                      opens <Link to={`/session/${encodeURIComponent(s.session)}`}>{s.session}</Link>
                    </p>
                    <pre className={css.file}>{s.text}</pre>
                  </div>
                ))}
              </section>
            )}
            {c.state !== null && c.state !== undefined && (
              <details>
                <summary>State</summary>
                <pre className={css.file} data-testid="ctx-state">
                  {JSON.stringify(c.state, null, 2)}
                </pre>
              </details>
            )}
          </>
        )}
      </Loaded>
    </main>
  );
}
