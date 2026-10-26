import { useEffect, useState } from "react";
import { useSearchParams } from "react-router-dom";
import {
  api,
  when,
  type ArmStats,
  type KnobName,
  type LearnView,
  type Lesson,
  type LessonAction,
  type LessonsView,
  type PolicyChange,
  type ShapePolicy,
} from "../api";
import { Loaded, useLoad } from "../useLoad";
import css from "./views.module.css";

const knobs: KnobName[] = ["backends", "tiers", "tactics"];

function shapeTitle(shape: string): string {
  return shape === "*" ? "The project (every shape)" : `Shape ${shape}`;
}

function fixed(x: number | null | undefined, digits: number): string {
  return x === null || x === undefined ? "-" : x.toFixed(digits);
}

function compact(v: unknown): string {
  if (Array.isArray(v) && v.every((x) => typeof x === "string")) return v.length ? v.join(", ") : "(none)";
  return JSON.stringify(v) ?? "null";
}

/** /learn?project=ROOT: the learned search policy of a project and the statistics behind it (fisher learn). */
export default function Learn() {
  const [params, setParams] = useSearchParams();
  const project = params.get("project") ?? "";
  const load = useLoad(() => api.learn(project || undefined), project);
  const choose = (p: string) => {
    const next = new URLSearchParams(params);
    if (p) next.set("project", p);
    else next.delete("project");
    setParams(next, { replace: true });
  };
  // no project asked for: the one with the latest run
  const first = load.state === "ok" && !project ? load.data.projects[0] : undefined;
  useEffect(() => {
    if (first) choose(first);
    // choose reads params, which this runs after
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [first]);
  const projects = load.state === "ok" ? load.data.projects : [];
  return (
    <main>
      <h1>Learn</h1>
      <p className="muted">
        What the run log says about each backend, model tier and pre-pass tactic, and the order the search uses for
        them.
      </p>
      <div className={css.filters}>
        <label>
          Project
          <select aria-label="Project" value={project} onChange={(e) => choose(e.target.value)}>
            {!project && <option value="">Choose a project</option>}
            {projects.map((p) => (
              <option key={p} value={p} title={p}>
                {p}
              </option>
            ))}
            {project && !projects.includes(project) && <option value={project}>{project}</option>}
          </select>
        </label>
      </div>
      <Loaded load={load}>
        {(v) =>
          !v.project ? (
            <p className="muted">
              {v.projects.length === 0 ? "No runs logged yet, so no project to learn from." : "Choose a project."}
            </p>
          ) : (
            <LearnBody v={v} />
          )
        }
      </Loaded>
    </main>
  );
}

function LearnBody({ v }: { v: LearnView }) {
  const shapes = Object.keys(v.stats);
  return (
    <>
      <p className={css.sub}>
        {v.project} (key {v.project_key})
      </p>
      <h2>Statistics</h2>
      <p className="muted">
        Per knob, each arm's attempts and closes at the level, its smoothed close rate, dollars per close and closes per
        hour; the arms in the policy's order, then the others seen.
      </p>
      {shapes.length === 0 && <p className="muted">No attempts logged for this project.</p>}
      {shapes.map((sh) => (
        <section key={sh} className={css.summary}>
          <h3>{shapeTitle(sh)}</h3>
          {knobs.map((k) => (
            <KnobTable key={k} shape={sh} knob={k} rows={v.stats[sh]?.[k] ?? []} />
          ))}
        </section>
      ))}

      <h2>Policy</h2>
      {v.policy_error && (
        <p role="alert">
          The policy file could not be read: {v.policy_error}
        </p>
      )}
      {v.policy ? (
        <>
          <p className="muted">Computed {when(v.policy.computed_at)}.</p>
          {Object.entries(v.policy.shapes).map(([sh, gp]) => (
            <ShapeEntry key={sh} shape={sh} gp={gp} />
          ))}
        </>
      ) : (
        !v.policy_error && <p className="muted">No policy for this project (a search computes one from its history).</p>
      )}

      <h2>Changes</h2>
      <Changes changes={v.changes} />

      <h2>Lessons</h2>
      <Lessons project={v.project ?? ""} />
    </>
  );
}

function KnobTable({ shape, knob, rows }: { shape: string; knob: KnobName; rows: ArmStats[] }) {
  const id = `stats-${shape}-${knob}`;
  if (rows.length === 0)
    return (
      <p className="muted" data-testid={id}>
        {knob}: none
      </p>
    );
  return (
    <div className={css.scroll}>
      <table className={css.table} data-testid={id}>
        <caption className={css.label} style={{ textAlign: "left" }}>
          {knob}
        </caption>
        <thead>
          <tr>
            <th>Arm</th>
            <th className={css.num}>Attempts</th>
            <th className={css.num}>Closes</th>
            <th className={css.num}>Rate</th>
            <th className={css.num}>$/close</th>
            <th className={css.num}>Closes/h</th>
          </tr>
        </thead>
        <tbody>
          {rows.map((r) => (
            <tr key={r.arm}>
              <td className={css.mono} data-testid="stats-arm">
                {r.arm}
              </td>
              <td className={css.num}>{r.attempts}</td>
              <td className={css.num}>{r.closes}</td>
              <td className={css.num}>{fixed(r.rate, 2)}</td>
              <td className={css.num}>{fixed(r.cost_per_close, 2)}</td>
              <td className={css.num}>{fixed(r.closes_per_hour, 1)}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function ShapeEntry({ shape, gp }: { shape: string; gp: ShapePolicy }) {
  const evidence = Object.entries(gp.evidence ?? {}).flatMap(([knob, arms]) =>
    Object.entries(arms ?? {}).map(([arm, e]) => ({ knob, arm, e })),
  );
  const list = (xs: string[]) => (xs.length ? xs.join(", ") : "(none)");
  return (
    <section className={css.summary} data-testid={`policy-${shape}`}>
      <h3>{shapeTitle(shape)}</h3>
      <div className={css.meta}>
        <span>
          <span className={css.label}>backends</span> {list(gp.backends)}
        </span>
        <span>
          <span className={css.label}>tiers</span> {list(gp.tiers)}
        </span>
        <span>
          <span className={css.label}>tactics</span> {list(gp.tactics)}
        </span>
        <span>
          <span className={css.label}>attempt budget</span>{" "}
          {gp.attempt_secs === null ? "the configured one" : `${gp.attempt_secs} s`}
        </span>
      </div>
      {evidence.length > 0 && (
        <div className={css.scroll}>
          <table className={css.table}>
            <caption className={css.label} style={{ textAlign: "left" }}>
              evidence
            </caption>
            <thead>
              <tr>
                <th>Knob</th>
                <th>Arm</th>
                <th className={css.num}>Attempts</th>
                <th className={css.num}>Closes</th>
                <th className={css.num}>Rate</th>
                <th className={css.num}>$/close</th>
              </tr>
            </thead>
            <tbody>
              {evidence.map(({ knob, arm, e }) => (
                <tr key={`${knob}\u0000${arm}`}>
                  <td>{knob}</td>
                  <td className={css.mono}>{arm}</td>
                  <td className={css.num}>{e.attempts ?? "-"}</td>
                  <td className={css.num}>{e.closes ?? "-"}</td>
                  <td className={css.num}>{fixed(e.rate, 2)}</td>
                  <td className={css.num}>{fixed(e.cost_per_close, 2)}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </section>
  );
}

function Changes({ changes }: { changes: PolicyChange[] }) {
  if (changes.length === 0) return <p className="muted">No changes recorded.</p>;
  return (
    <div className={css.scroll}>
      <table className={css.table} data-testid="policy-changes">
        <thead>
          <tr>
            <th>When</th>
            <th>Shape</th>
            <th>Knob</th>
            <th>From</th>
            <th>To</th>
          </tr>
        </thead>
        <tbody>
          {changes.map((c, i) => (
            <tr key={i} data-testid="policy-change">
              <td className={css.nowrap}>{when(c.at)}</td>
              <td className={css.sub}>{c.shape}</td>
              <td>{c.knob}</td>
              <td className={css.mono}>{compact(c.old)}</td>
              <td className={css.mono}>{compact(c.new)}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

/** The lessons mined from the run log (fisher lessons): the project's, then the global ones, each with its actions. */
function Lessons({ project }: { project: string }) {
  const load = useLoad(() => api.lessons(project), project);
  return (
    <>
      <p className="muted">
        What the agents are told from the mistakes logged: a candidate is proposed, an active or pinned one is shown,
        a pinned one is never retired, a dropped one is never proposed again. A judged or self lesson is written by a
        model from a transcript, so it is never shown on its counts alone: it waits under Proposed for your Pin.
      </p>
      <Loaded load={load}>{(v) => <LessonsBody key={project} project={project} v={v} />}</Loaded>
    </>
  );
}

function LessonsBody({ project, v }: { project: string; v: LessonsView }) {
  const [own, setOwn] = useState(v.lessons);
  const [global, setGlobal] = useState(v.global);
  const [busy, setBusy] = useState<number[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [notice, setNotice] = useState<string | null>(null);
  const act = (l: Lesson, action: LessonAction) => {
    setBusy((b) => [...b, l.id]);
    setError(null);
    setNotice(null);
    api.lessonAction(l.id, action, project || undefined).then(
      (row) => {
        const put = (xs: Lesson[]) => xs.map((x) => (x.id === row.id ? row : x));
        setOwn(put);
        setGlobal(put);
        if (action === "unpin" && row.status === "candidate")
          setNotice(`lesson ${row.id}: unpinned; it is proposed again until you pin it`);
        if (row.regenerate_error)
          setError(`lesson ${row.id}: ${row.status}, but a lessons file was not written: ${row.regenerate_error}`);
        setBusy((b) => b.filter((x) => x !== l.id));
      },
      (e: Error) => {
        setError(`lesson ${l.id}: ${e.message}`);
        setBusy((b) => b.filter((x) => x !== l.id));
      },
    );
  };
  const waiting = [...own, ...global].filter((l) => l.proposed);
  return (
    <>
      {error && <p role="alert">{error}</p>}
      {notice && <p role="status" data-testid="lessons-notice">{notice}</p>}
      {waiting.length > 0 && (
        <section className={css.summary}>
          <h3>Proposed (needs your approval)</h3>
          <p className="muted">Judged and self lessons with enough evidence. Pin one to show it to the agents, or drop it.</p>
          <LessonTable testId="lessons-proposed" label="Proposed lessons" rows={waiting} busy={busy} act={act} />
        </section>
      )}
      <section className={css.summary}>
        <h3>The project</h3>
        <LessonTable testId="lessons-project" label="Project lessons" rows={own.filter((l) => !l.proposed)} busy={busy} act={act} />
      </section>
      <section className={css.summary}>
        <h3>Global (every project)</h3>
        <LessonTable testId="lessons-global" label="Global lessons" rows={global.filter((l) => !l.proposed)} busy={busy} act={act} />
      </section>
    </>
  );
}

function LessonTable({
  testId,
  label,
  rows,
  busy,
  act,
}: {
  testId: string;
  label: string;
  rows: Lesson[];
  busy: number[];
  act: (l: Lesson, a: LessonAction) => void;
}) {
  if (rows.length === 0)
    return (
      <p className="muted" data-testid={testId}>
        No lessons.
      </p>
    );
  return (
    <div className={css.scroll}>
      <table className={css.table} data-testid={testId} aria-label={label}>
        <thead>
          <tr>
            <th className={css.num}>Id</th>
            <th>Status</th>
            <th className={css.num}>Runs</th>
            <th>Text</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
          {rows.map((l) => (
            <tr key={l.id} data-testid="lesson-row" data-lesson={l.id}>
              <td className={css.num}>{l.id}</td>
              <td data-testid="lesson-status">{l.status}</td>
              <td className={css.num} data-testid="lesson-evidence" title="Runs the lesson was seen in">
                {l.evidence_runs.length}
              </td>
              <td>
                {l.proposed && (
                  <>
                    <strong data-testid="lesson-approval">needs your approval</strong>{" "}
                  </>
                )}
                {l.text}
              </td>
              <td className={css.nowrap}>
                {l.status === "pinned" ? (
                  <ActionButton l={l} action="unpin" busy={busy} act={act} />
                ) : (
                  l.status !== "dropped" && <ActionButton l={l} action="pin" busy={busy} act={act} />
                )}{" "}
                {l.status !== "dropped" && <ActionButton l={l} action="drop" busy={busy} act={act} />}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

const actionNames: Record<LessonAction, string> = { pin: "Pin", unpin: "Unpin", drop: "Drop" };

function ActionButton({
  l,
  action,
  busy,
  act,
}: {
  l: Lesson;
  action: LessonAction;
  busy: number[];
  act: (l: Lesson, a: LessonAction) => void;
}) {
  const name = actionNames[action];
  return (
    <button type="button" aria-label={`${name} lesson ${l.id}`} disabled={busy.includes(l.id)} onClick={() => act(l, action)}>
      {name}
    </button>
  );
}
