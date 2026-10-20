import { useCallback, useEffect, useRef, useState, type FormEvent } from "react";
import { Link, useLocation, useNavigate, useParams } from "react-router-dom";
import { api, ApiError, type CreateEvent, type CreateView, type TemplateStep, type TemplateView } from "../api";
import { Loaded, useLoad } from "../useLoad";
import css from "./newproject.module.css";

/**
 * /new: make a project (or a scratch environment) from a template;
 * /new/:id: that create's steps as they run, then Ready with a link to
 * start a run in it (and, for a scratch, whose file has a sorry, a search).
 */
export default function NewProject() {
  const { id } = useParams();
  return id ? <Progress key={id} id={id} /> : <NewForm />;
}

/** Why a variable's value does not do, checked as the server checks it (the whole value matches). */
export function patternError(pattern: string, value: string): string | null {
  if (!pattern || !value) return null;
  let rx: RegExp;
  try {
    rx = new RegExp(`^(?:${pattern})$`);
  } catch {
    // a pattern the browser cannot read: the server checks it
    return null;
  }
  return rx.test(value) ? null : `does not match ${pattern}`;
}

function baseName(p: string): string {
  const parts = p.split("/").filter(Boolean);
  return parts[parts.length - 1] ?? "";
}

const command = (s: TemplateStep) => s.run.join(" ");

/** A create to confirm again, as a retry's 409 gives it: the request it was, the steps and revision now. */
export interface Reconfirm {
  request: { template: string; path?: string; scratch?: boolean; vars?: Record<string, string>; statement?: string };
  steps: TemplateStep[];
  revision: string;
  error: string;
}

function NewForm() {
  const load = useLoad(() => api.templates(), "templates");
  const again = (useLocation().state as { reconfirm?: Reconfirm } | null)?.reconfirm;
  return (
    <main>
      <p className={css.crumbs}>
        <Link to="/sessions">Sessions</Link>
      </p>
      <h1>New project</h1>
      <p className="muted">
        A Lean project from a template: its files are written, its steps run (the toolchain, the Mathlib cache, a
        build), and its sample is checked before it says Ready.
      </p>
      <Loaded load={load}>{(list) => <CreateForm templates={list.templates} notes={list.notes} again={again} />}</Loaded>
    </main>
  );
}

function CreateForm({ templates, notes, again }: { templates: TemplateView[]; notes: string[]; again?: Reconfirm }) {
  const nav = useNavigate();
  // a retry sent back here (the template changed since it was confirmed) starts as that request, at Confirm
  const asked = again && templates.some((t) => t.name === again.request.template) ? again : undefined;
  const [name, setName] = useState(() =>
    asked
      ? asked.request.template
      : templates.some((t) => t.name === "lean-mathlib")
        ? "lean-mathlib"
        : (templates[0]?.name ?? ""),
  );
  const [where, setWhere] = useState<"folder" | "scratch">(asked?.request.scratch ? "scratch" : "folder");
  const [folder, setFolder] = useState(asked?.request.path ?? "");
  const [statement, setStatement] = useState(asked?.request.statement ?? "");
  const [values, setValues] = useState<Record<string, string>>(asked?.request.vars ?? {});
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(asked ? asked.error : null);
  // the steps a 409 showed, and the template's revision then
  const [confirm, setConfirm] = useState<{ steps: TemplateStep[]; revision: string } | null>(
    asked ? { steps: asked.steps, revision: asked.revision } : null,
  );
  const t = templates.find((x) => x.name === name);

  const folderError =
    where === "folder" && folder.trim() && !folder.trim().startsWith("/")
      ? "The folder must be an absolute path (such as /Users/you/proofs/Demo)"
      : null;
  const varErrors: Record<string, string> = {};
  for (const v of t?.variables ?? []) {
    const e = patternError(v.pattern, (values[v.name] ?? "").trim());
    if (e) varErrors[v.name] = e;
  }
  const invalid = !t || !!folderError || Object.keys(varErrors).length > 0 || (where === "folder" && !folder.trim());

  const choose = (n: string) => {
    setName(n);
    setValues({});
    setConfirm(null);
    setError(null);
  };
  // a confirm is of the steps as shown: any change to what renders them drops it
  const changed = <T,>(set: (v: T) => void) => (v: T) => {
    set(v);
    setConfirm(null);
  };

  const send = (revision?: string) => {
    if (!t || invalid) return;
    setBusy(true);
    setError(null);
    const vars: Record<string, string> = {};
    for (const [k, v] of Object.entries(values)) if (v.trim()) vars[k] = v.trim();
    const req =
      where === "scratch"
        ? { template: t.name, scratch: true, vars, confirm: revision, ...(statement.trim() ? { statement: statement.trim() } : {}) }
        : { template: t.name, path: folder.trim(), vars, confirm: revision };
    api.create(req).then(
      (r) => nav(`/new/${encodeURIComponent(r.id)}`),
      (err: Error) => {
        setBusy(false);
        const steps = err instanceof ApiError ? err.body?.steps : undefined;
        const rev = err instanceof ApiError ? err.body?.revision : undefined;
        if (err instanceof ApiError && err.status === 409 && Array.isArray(steps) && typeof rev === "string") {
          setConfirm({ steps: steps as TemplateStep[], revision: rev });
          // a confirm of an older revision: say why the steps are shown again
          if (revision) setError(err.message);
        } else setError(err.message);
      },
    );
  };

  const submit = (e: FormEvent) => {
    e.preventDefault();
    send();
  };

  const placeholder = (v: TemplateView["variables"][number]) => {
    if (v.name === "project" && !v.default) return where === "scratch" ? "Scratch" : baseName(folder) || "the folder's name";
    return v.default;
  };

  return (
    <form className={css.form} onSubmit={submit} aria-label="New project">
      {notes.length > 0 && (
        <ul className={css.notes} data-testid="template-notes">
          {notes.map((n) => (
            <li key={n}>{n}</li>
          ))}
        </ul>
      )}
      <label className={css.field}>
        Template
        <select aria-label="Template" value={name} onChange={(e) => choose(e.target.value)}>
          {templates.map((x) => (
            <option key={x.name} value={x.name}>
              {x.name}
              {x.origin === "builtin" ? "" : " (configured)"}
            </option>
          ))}
        </select>
      </label>
      {t && (
        <div className={css.about} data-testid="template-about">
          <p>{t.description}</p>
          <p className={css.steps}>
            Steps:{" "}
            {t.steps.map((s, i) => (
              <span key={s.name}>
                {i > 0 && ", "}
                <code>{command(s)}</code>
              </span>
            ))}
          </p>
          {!t.trusted && (
            <p className={css.warn} data-testid="untrusted">
              This template is not trusted yet: it runs its own steps{t.source ? ` (from ${t.source})` : ""}. You see
              them and confirm before anything runs.
            </p>
          )}
        </div>
      )}
      <fieldset className={css.where}>
        <legend>Where</legend>
        <label className={css.radio}>
          <input type="radio" name="where" checked={where === "folder"} onChange={() => changed(setWhere)("folder")} />
          In a folder
        </label>
        <label className={css.radio}>
          <input type="radio" name="where" checked={where === "scratch"} onChange={() => changed(setWhere)("scratch")} />
          Scratch (a throwaway copy of a base built once)
        </label>
      </fieldset>
      {where === "folder" ? (
        <label className={css.field}>
          Folder (absolute; new or empty)
          <input
            aria-label="Folder"
            value={folder}
            onChange={(e) => changed(setFolder)(e.target.value)}
            placeholder="/Users/you/proofs/Demo"
            spellCheck={false}
            aria-invalid={!!folderError}
          />
          {folderError && (
            <span className={css.fieldError} data-testid="folder-error">
              {folderError}
            </span>
          )}
        </label>
      ) : (
        <label className={css.field}>
          Statement (optional: the goal of Scratch.lean)
          <input
            aria-label="Statement"
            value={statement}
            onChange={(e) => changed(setStatement)(e.target.value)}
            placeholder="∀ n : Nat, n + 0 = n"
            spellCheck={false}
          />
        </label>
      )}
      {t && t.variables.length > 0 && (
        <fieldset className={css.vars}>
          <legend>Variables</legend>
          {t.variables.map((v) => (
            <label key={v.name} className={css.field} data-var={v.name}>
              <span>
                <code>{v.name}</code> <span className="muted">{v.description}</span>
              </span>
              <input
                aria-label={`Variable ${v.name}`}
                value={values[v.name] ?? ""}
                onChange={(e) => {
                  const value = e.target.value;
                  setValues((x) => ({ ...x, [v.name]: value }));
                  setConfirm(null);
                }}
                placeholder={placeholder(v)}
                spellCheck={false}
                aria-invalid={!!varErrors[v.name]}
              />
              {varErrors[v.name] && (
                <span className={css.fieldError} data-testid={`var-error-${v.name}`}>
                  {v.name} {varErrors[v.name]}
                </span>
              )}
            </label>
          ))}
        </fieldset>
      )}
      {confirm ? (
        <div className={css.confirm} data-testid="confirm-steps">
          <p>
            <strong>{t?.name}</strong> runs these steps in the new {where === "scratch" ? "scratch base" : "project"}:
          </p>
          <ol>
            {confirm.steps.map((s) => (
              <li key={s.name}>
                {s.name}: <code>{command(s)}</code> <span className="muted">({s.timeout_minutes} min at most)</span>
              </li>
            ))}
          </ol>
          <div className={css.buttons}>
            <button type="button" onClick={() => send(confirm.revision)} disabled={busy || invalid}>
              Confirm and create
            </button>
            <button type="button" onClick={() => setConfirm(null)} disabled={busy}>
              Cancel
            </button>
          </div>
        </div>
      ) : (
        <div className={css.buttons}>
          <button type="submit" disabled={busy || invalid}>
            {busy ? "Starting..." : "Create"}
          </button>
        </div>
      )}
      {error && (
        <p role="alert" className={css.error}>
          Could not start: {error}
        </p>
      )}
    </form>
  );
}

// ------------------------------------------------------------- progress

interface StepRow {
  name: string;
  state: "running" | "done" | "failed";
  exit: number | null;
  tail: string[];
}

interface Attempt {
  n: number;
  from: number;
}

function fold(rows: StepRow[], e: CreateEvent): StepRow[] {
  if (e.event === "step start") return [...rows.filter((r) => r.name !== e.step), { name: e.step, state: "running", exit: null, tail: [] }];
  if (e.event === "step end") {
    const row: StepRow = { name: e.step, state: e.exit === 0 ? "done" : "failed", exit: e.exit, tail: e.tail ?? [] };
    return rows.some((r) => r.name === e.step) ? rows.map((r) => (r.name === e.step ? row : r)) : [...rows, row];
  }
  return rows;
}

const ended = (s: string | undefined) => !!s && !["running", "stopping"].includes(s);

function Progress({ id }: { id: string }) {
  const nav = useNavigate();
  const [view, setView] = useState<CreateView | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [steps, setSteps] = useState<StepRow[]>([]);
  const [check, setCheck] = useState<{ ok: boolean; problems: string[] } | null>(null);
  const [attempt, setAttempt] = useState<Attempt>({ n: 0, from: 0 });
  const [actionError, setActionError] = useState<string | null>(null);
  const offset = useRef(0);

  const reload = useCallback(
    () =>
      api.createRun(id).then(
        (v) => {
          setView(v);
          setLoadError(null);
        },
        (e: Error) => setLoadError(e.message),
      ),
    [id],
  );

  useEffect(() => {
    void reload();
  }, [reload]);

  // the events of this attempt: each line once (the id is its end offset);
  // "rewritten" starts again from the top
  useEffect(() => {
    offset.current = attempt.from;
    const s = new EventSource(api.createEventsUrl(id, attempt.from));
    s.onmessage = (m: MessageEvent<string>) => {
      const at = Number(m.lastEventId);
      if (Number.isFinite(at) && at <= offset.current) return;
      let e: CreateEvent;
      try {
        e = JSON.parse(m.data) as CreateEvent;
      } catch {
        return;
      }
      if (Number.isFinite(at)) offset.current = at;
      if (e.event === "check") setCheck({ ok: e.ok, problems: e.problems ?? [] });
      else setSteps((rows) => fold(rows, e));
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
        setSteps([]);
        setCheck(null);
        setAttempt((a) => ({ n: a.n + 1, from: 0 }));
      } else void reload();
    });
    s.onerror = () => {
      // 204: it had ended already
      if (s.readyState === EventSource.CLOSED) void reload();
    };
    return () => s.close();
  }, [id, attempt, reload]);

  const stop = () => {
    setActionError(null);
    api.stopCreate(id).then(
      () => setView((v) => (v ? { ...v, status: "stopping" } : v)),
      (e: Error) => setActionError(e.message),
    );
  };

  const retry = () => {
    setActionError(null);
    api.retryCreate(id).then(
      (r) => {
        if (view?.mode === "scratch" || r.from === 0) setSteps([]);
        setCheck(null);
        setView((v) => (v ? { ...v, status: "running", running: true, reason: null } : v));
        setAttempt((a) => ({ n: a.n + 1, from: r.from }));
      },
      (e: Error) => {
        // the template changed since it was confirmed: back to Confirm with the same request
        const b = e instanceof ApiError && e.status === 409 ? e.body : null;
        if (b && Array.isArray(b.steps) && typeof b.revision === "string" && b.request && typeof b.request === "object") {
          const reconfirm: Reconfirm = {
            request: b.request as Reconfirm["request"],
            steps: b.steps as TemplateStep[],
            revision: b.revision,
            error: e.message,
          };
          nav("/new", { state: { reconfirm } });
        } else setActionError(e.message);
      },
    );
  };

  if (loadError && !view)
    return (
      <main>
        <h1>New project</h1>
        <p role="alert">Could not load {id}: {loadError}</p>
        <p>
          <Link to="/new">Make another</Link>
        </p>
      </main>
    );
  if (!view)
    return (
      <main>
        <p className="muted">Loading...</p>
      </main>
    );

  const where = view.mode === "scratch" ? "Scratch" : (view.root ?? "");
  const q = (o: Record<string, string>) => new URLSearchParams(o).toString();
  return (
    <main>
      <p className={css.crumbs}>
        <Link to="/sessions">Sessions</Link> / <Link to="/new">New project</Link>
      </p>
      <h1>
        {view.template}: {view.mode === "scratch" ? "a scratch environment" : baseName(view.root ?? "")}
      </h1>
      <p className="muted" data-testid="create-where">
        {where} <span className={css.status} data-testid="create-status" data-status={view.status}>{view.status}</span>
      </p>
      {steps.length === 0 && !ended(view.status) && <p className="muted">Waiting for the first step...</p>}
      {steps.length > 0 && (
        <ol className={css.stepList} data-testid="steps">
          {steps.map((s) => (
            <li key={s.name} data-testid="step" data-step={s.name} data-state={s.state} className={css[s.state]}>
              <span className={css.stepName}>{s.name}</span>{" "}
              <span className="muted">
                {s.state === "running" ? "running..." : s.state === "done" ? "done" : s.exit === -1 ? "timed out" : `failed (exit ${s.exit})`}
              </span>
              {s.state === "failed" && s.tail.length > 0 && <pre className={css.tail}>{s.tail.join("\n")}</pre>}
            </li>
          ))}
        </ol>
      )}
      {check && (
        <p className={check.ok ? css.done : css.failed} data-testid="check">
          Sample check: {check.ok ? "it builds, with no sorry and only the standard axioms" : check.problems.join("; ")}
        </p>
      )}
      {view.running && (
        <div className={css.buttons}>
          <button type="button" onClick={stop} disabled={view.status === "stopping"}>
            {view.status === "stopping" ? "Stopping..." : "Stop"}
          </button>
        </div>
      )}
      {view.status === "ready" && view.root && (
        <section className={css.ready} data-testid="ready">
          <h2>Ready</h2>
          <p className={css.mono}>{view.root}</p>
          {view.file && <p className="muted">Sample: {view.file}</p>}
          <p className={css.links}>
            <Link to={`/sessions?${q({ start: view.root, ...(view.file ? { file: view.file } : {}) })}`} data-testid="start-run">
              Start a run
            </Link>
            {view.mode === "scratch" && (
              <Link to={`/search?${q({ start: view.root, ...(view.file ? { file: view.file } : {}) })}`} data-testid="start-search">
                Start a search
              </Link>
            )}
          </p>
          {view.mode !== "scratch" && (
            <p className="muted" data-testid="search-hint">
              Next: add a theorem with sorry, then start a search.
            </p>
          )}
        </section>
      )}
      {(view.status === "failed" || view.status === "stopped") && (
        <section className={css.failedBox}>
          <p role="alert" data-testid="create-failed">
            {view.status === "stopped" ? "Stopped." : "It failed."} {view.reason && <span className={css.reason}>{view.reason}</span>}
          </p>
          <p className="muted">The output is in {view.log}.</p>
          <div className={css.buttons}>
            <button type="button" onClick={retry}>
              Retry
            </button>
          </div>
        </section>
      )}
      {actionError && (
        <p role="alert" className={css.error}>
          {actionError}
        </p>
      )}
    </main>
  );
}
