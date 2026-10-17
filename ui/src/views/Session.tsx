import { Link, useParams } from "react-router-dom";
import { api, when, type StepHead } from "../api";
import Canvas from "../components/Canvas";
import CostStrip from "../components/CostStrip";
import Transcript from "../components/Transcript";
import { Loaded, useLoad } from "../useLoad";
import { proverName } from "./Sessions";
import css from "./views.module.css";

export default function Session() {
  const { id = "" } = useParams();
  // the session with its step log (none for a session recorded before it)
  const load = useLoad(
    () => Promise.all([api.session(id), api.steps(id).catch((): StepHead[] => [])]).then(([d, steps]) => ({ ...d, steps })),
    id,
  );
  return (
    <main>
      <p className={css.crumbs}>
        <Link to="/sessions">Sessions</Link>
      </p>
      <Loaded load={load}>
        {(d) => {
          const s = d.session;
          return (
            <>
              <h1>{s.title ?? s.id}</h1>
              <p className={css.meta} data-testid="session-meta">
                <span>{s.id}</span>
                {s.prover && <span>{proverName(s.prover)}</span>}
                {s.model && <span>{s.model}</span>}
                <span>context {s.strategy}</span>
                <span>started {when(s.started)}</span>
                {s.result && <span>{s.result}</span>}
              </p>
              {s.project && (
                <p className={css.meta}>
                  <span title="The project this session's tools worked in">{s.project}</span>
                </p>
              )}
              <CostStrip cost={d.cost} turns={s.turns} />
              <p className={css.links}>
                <Link to={`/context/${encodeURIComponent(s.id)}`}>Context</Link>
                <Link to={`/live/${encodeURIComponent(s.id)}`}>Live events</Link>
                {d.children.map((c) => (
                  <Link key={c} to={`/session/${encodeURIComponent(c)}`}>
                    continued in {c}
                  </Link>
                ))}
              </p>
              <div className={d.steps.length ? css.split : undefined}>
                <div className={css.splitLeft}>
                  {d.messages.length === 0 ? <p className="muted">No messages.</p> : <Transcript messages={d.messages} />}
                </div>
                {d.steps.length > 0 && <Canvas id={s.id} steps={d.steps} />}
              </div>
            </>
          );
        }}
      </Loaded>
    </main>
  );
}
