import { useEffect, useState, type FormEvent } from "react";
import { useNavigate } from "react-router-dom";
import { api, ended, type RunView } from "../api";
import css from "./components.module.css";

/**
 * Start a run: a Fisher command (such as /prove theories/Lists.v) in a
 * project directory. The server runs it headless with the event ledger on;
 * the page waits for the session it becomes, then opens its live view.
 * The New project view links here with the project (and its sample)
 * filled in.
 */
export default function StartRun({
  projects,
  initialProject = "",
  initialFile = "",
}: {
  projects: string[];
  initialProject?: string;
  initialFile?: string;
}) {
  const navigate = useNavigate();
  const [project, setProject] = useState(initialProject);
  const [command, setCommand] = useState(`/prove ${initialFile}`);
  const [tag, setTag] = useState<string | null>(null);
  const [run, setRun] = useState<RunView | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);

  useEffect(() => {
    if (!tag) return;
    let live = true;
    let timer: number | undefined;
    const poll = () =>
      api.tag(tag).then(
        (r) => {
          if (!live) return;
          setRun(r);
          if (r.session) navigate(`/live/${encodeURIComponent(r.session)}`);
          else if (ended(r.status)) setBusy(false);
          else timer = window.setTimeout(poll, 500);
        },
        (e: Error) => {
          if (!live) return;
          setError(e.message);
          setBusy(false);
        },
      );
    poll();
    return () => {
      live = false;
      window.clearTimeout(timer);
    };
  }, [tag, navigate]);

  const submit = (e: FormEvent) => {
    e.preventDefault();
    setError(null);
    setRun(null);
    setBusy(true);
    api.start(project.trim(), command.trim()).then(
      (r) => setTag(r.tag),
      (err: Error) => {
        setError(err.message);
        setBusy(false);
      },
    );
  };

  const cancel = () => {
    if (tag) api.stopTag(tag).catch(() => undefined);
  };

  return (
    <form className={css.startRun} onSubmit={submit} aria-label="Start a run">
      <label>
        Project directory
        <input
          list="projects"
          value={project}
          onChange={(e) => setProject(e.target.value)}
          placeholder="/path/to/project"
          required
          spellCheck={false}
        />
      </label>
      <datalist id="projects">
        {projects.map((p) => (
          <option key={p} value={p} />
        ))}
      </datalist>
      <label>
        Command
        <input value={command} onChange={(e) => setCommand(e.target.value)} required spellCheck={false} />
      </label>
      <button type="submit" disabled={busy}>
        Start
      </button>
      {busy && tag && !run?.session && (
        <span className="muted" data-testid="starting">
          Starting run {tag}...{" "}
          <button type="button" onClick={cancel}>
            Cancel
          </button>
        </span>
      )}
      {run && !run.session && ended(run.status) && (
        <p role="alert" className={css.startError}>
          The run ended ({run.status}
          {run.exit !== null && run.exit !== undefined ? `, exit ${run.exit}` : ""}) before it began a session.
          {run.log && <> Its output is in {run.log}.</>}
        </p>
      )}
      {error && (
        <p role="alert" className={css.startError}>
          Could not start: {error}
        </p>
      )}
    </form>
  );
}
