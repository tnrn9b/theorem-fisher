import { useCallback, useEffect, useRef, useState } from "react";
import { Link } from "react-router-dom";
import { ApiError, api, when, type HandBack, type StepHead, type StepView, type Verdict } from "../api";
import Diff from "./Diff";
import Timeline, { verdictWords } from "./Timeline";
import css from "./canvas.module.css";

/** A plain monospace editor with line numbers. */
function Editor({ value, onChange }: { value: string; onChange: (v: string) => void }) {
  const gutter = useRef<HTMLPreElement>(null);
  const lines = Math.max(1, value.split("\n").length - (value.endsWith("\n") ? 1 : 0));
  const numbers = Array.from({ length: lines }, (_, i) => i + 1).join("\n");
  return (
    <div className={css.editor}>
      <pre ref={gutter} className={css.gutter} aria-hidden="true">
        {numbers}
      </pre>
      <textarea
        aria-label="File at this step"
        data-testid="editor"
        className={css.text}
        value={value}
        spellCheck={false}
        wrap="off"
        onChange={(e) => onChange(e.target.value)}
        onScroll={(e) => {
          if (gutter.current) gutter.current.scrollTop = e.currentTarget.scrollTop;
        }}
      />
    </div>
  );
}

function shortPath(p: string): string {
  const parts = p.split("/");
  return parts.slice(-2).join("/");
}

/**
 * The proof file at each step of the session: the timeline, the file as
 * the selected step left it (with its check, run on demand), a diff of any
 * two steps, and a hand edit handed back to the agent.
 */
export default function Canvas({ id, steps: steps0 }: { id: string; steps: StepHead[] }) {
  const [steps, setSteps] = useState(steps0);
  const [selected, setSelected] = useState(steps0[steps0.length - 1]?.seq ?? 0);
  const [views, setViews] = useState(new Map<number, StepView>());
  const [verdicts, setVerdicts] = useState(new Map<number, Verdict>());
  const [draft, setDraft] = useState<string | null>(null);
  const [checking, setChecking] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [compare, setCompare] = useState<{ a: number; b: number }>({
    a: steps0[0]?.seq ?? 0,
    b: steps0[steps0.length - 1]?.seq ?? 0,
  });
  const [showDiff, setShowDiff] = useState(false);
  const [handing, setHanding] = useState(false);
  const [handed, setHanded] = useState<HandBack | null>(null);
  // a change sent to a live run: whether the run ended before reading it
  const [unread, setUnread] = useState(false);

  useEffect(() => {
    if (!handed || handed.delivered !== "inbox") return;
    let live = true;
    let timer: number | undefined;
    const tick = () =>
      api.live(id).then(
        (r) => {
          if (!live) return;
          // a session running in another process is still going too
          if (r.running || typeof r.elsewhere === "number") timer = window.setTimeout(tick, 2000);
          else setUnread((r.inboxPending ?? 0) > 0);
        },
        () => undefined,
      );
    tick();
    return () => {
      live = false;
      window.clearTimeout(timer);
    };
  }, [handed, id]);

  const load = useCallback(
    (seq: number) => {
      if (views.has(seq)) return Promise.resolve(views.get(seq)!);
      return api.step(id, seq).then((v) => {
        setViews((m) => new Map(m).set(seq, v));
        if (v.verdict) setVerdicts((m) => new Map(m).set(seq, v.verdict!));
        return v;
      });
    },
    [id, views],
  );

  useEffect(() => {
    if (!selected) return;
    setError(null);
    load(selected).catch((e: Error) => setError(e.message));
  }, [selected, load]);

  useEffect(() => {
    if (!showDiff) return;
    Promise.all([load(compare.a), load(compare.b)]).catch((e: Error) => setError(e.message));
  }, [showDiff, compare, load]);

  const view = views.get(selected);
  const head = steps.find((s) => s.seq === selected);
  const verdict = verdicts.get(selected) ?? view?.verdict ?? null;
  const text = draft ?? view?.text ?? "";
  const edited = draft !== null && view !== undefined && draft !== view.text;

  const select = (seq: number) => {
    if (edited && !window.confirm("Leave the hand edit unsent?")) return;
    setDraft(null);
    setSelected(seq);
  };

  const check = () => {
    setChecking(true);
    api.verdict(id, selected).then(
      (v) => {
        setVerdicts((m) => new Map(m).set(selected, v));
        setChecking(false);
      },
      (e: Error) => {
        setError(e.message);
        setChecking(false);
      },
    );
  };

  const handBack = () => {
    if (!view || draft === null) return;
    setHanding(true);
    setError(null);
    setUnread(false);
    api.handBack(id, view.path, draft, view.hash).then(
      (h) => {
        setHanded(h);
        setHanding(false);
        setVerdicts((m) => new Map(m).set(h.seq, h.verdict));
        api.steps(id).then((ss) => {
          setSteps(ss);
          setDraft(null);
          setSelected(h.seq);
          setCompare((c) => ({ ...c, b: h.seq }));
        });
      },
      (e: Error) => {
        setError(
          e instanceof ApiError && e.status === 409
            ? "The file changed on disk since this step; pick its latest step and edit that."
            : `Could not hand back: ${e.message}`,
        );
        setHanding(false);
      },
    );
  };

  const a = views.get(compare.a);
  const b = views.get(compare.b);

  return (
    <section className={css.canvas} aria-label="Proof canvas" data-testid="canvas">
      <h2>Proof file</h2>
      <Timeline steps={steps} verdicts={verdicts} selected={selected} onSelect={select} />
      {head && (
        <p className={css.stepMeta} data-testid="step-meta">
          <span>
            step <strong data-testid="step-seq">{head.seq}</strong> of {steps.length}
          </span>
          <span>{head.tool}</span>
          <span title={head.path}>{shortPath(head.path)}</span>
          <span>{when(head.at)}</span>
          <span data-testid="verdict" className={css.verdict}>
            check: {checking ? "checking..." : verdictWords(verdict)}
          </span>
          {!verdict && (
            <button type="button" onClick={check} disabled={checking}>
              {checking ? "Checking..." : "Check this step"}
            </button>
          )}
        </p>
      )}
      {verdict && verdict.errors.length > 0 && (
        <ul className={css.errors} data-testid="verdict-errors">
          {verdict.errors.map((e, i) => (
            <li key={i}>{e}</li>
          ))}
        </ul>
      )}
      {error && <p role="alert">{error}</p>}
      {view ? <Editor value={text} onChange={setDraft} /> : <p className="muted">Loading the step...</p>}
      {edited && (
        <div className={css.handRow}>
          <button type="button" onClick={handBack} disabled={handing}>
            {handing ? "Handing back..." : "Hand back"}
          </button>
          <button type="button" onClick={() => setDraft(null)} disabled={handing}>
            Discard the edit
          </button>
          <span className="muted">
            Hand back saves the file, checks it, and tells the agent what changed.
          </span>
        </div>
      )}
      {handed && (
        <p data-testid="handed" className={css.handed}>
          Handed back as step {handed.seq} (check: {verdictWords(handed.verdict)}).{" "}
          {handed.delivered === "inbox" &&
            (typeof handed.elsewhere === "number"
              ? `The session runs in another process (pid ${handed.elsewhere}); its agent reads the change at its next tool call.`
              : "The agent reads the change at its next tool call.")}
          {handed.delivered === "resume" && `The session was resumed with the change as run ${handed.tag ?? ""}.`}
          {handed.delivered === "pending" &&
            `The session could not be resumed (${handed.error ?? "unknown error"}); the change waits in its inbox for its next run.`}{" "}
          {handed.delivered !== "pending" && (
            <Link to={`/live/${encodeURIComponent(id)}${handed.after ? `?after=${handed.after}` : ""}`}>Watch it</Link>
          )}
        </p>
      )}
      {handed && unread && (
        <p role="status" data-testid="unread" className="muted">
          The run ended before it read the change; it waits in the session&apos;s inbox for its next run.
        </p>
      )}
      <div className={css.compare}>
        <label>
          Compare step
          <select aria-label="From step" value={compare.a} onChange={(e) => setCompare({ ...compare, a: Number(e.target.value) })}>
            {steps.map((s) => (
              <option key={s.seq} value={s.seq}>
                {s.seq}
              </option>
            ))}
          </select>
        </label>
        <label>
          with step
          <select aria-label="To step" value={compare.b} onChange={(e) => setCompare({ ...compare, b: Number(e.target.value) })}>
            {steps.map((s) => (
              <option key={s.seq} value={s.seq}>
                {s.seq}
              </option>
            ))}
          </select>
        </label>
        <button type="button" onClick={() => setShowDiff((x) => !x)}>
          {showDiff ? "Hide the diff" : "Show the diff"}
        </button>
      </div>
      {showDiff &&
        (a && b ? (
          <Diff from={a.text} to={b.text} fromLabel={`step ${a.seq} ${shortPath(a.path)}`} toLabel={`step ${b.seq} ${shortPath(b.path)}`} />
        ) : (
          <p className="muted">Loading the steps...</p>
        ))}
    </section>
  );
}
