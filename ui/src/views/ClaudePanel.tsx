import { Fragment, useCallback, useEffect, useRef, useState, type FormEvent, type ReactNode } from "react";
import { api, ApiError, type ClaudeAuth } from "../api";
import css from "./settings.module.css";

const installHint = "npm install -g @anthropic-ai/claude-code";

/** A line of output with its URLs as links (the login prints one to open). */
function linked(line: string): ReactNode {
  const parts = line.split(/(https?:\/\/\S+)/);
  return parts.map((p, i) =>
    i % 2 === 1 ? (
      <a key={i} href={p} target="_blank" rel="noreferrer noopener">
        {p}
      </a>
    ) : (
      <Fragment key={i}>{p}</Fragment>
    ),
  );
}

function statusText(s: ClaudeAuth): string {
  if (!s.installed) return `The claude CLI is not installed. Install it with: ${installHint}`;
  if (s.loggedIn) {
    const plan = s.subscriptionType ?? s.authMethod;
    return `Logged in as ${s.email ?? "an unnamed account"}${plan ? ` (${plan})` : ""}`;
  }
  if (s.error) return `Not logged in (the status could not be read: ${s.error})`;
  return "Not logged in";
}

/**
 * The Claude Code CLI's own login, which the claude-code backend uses:
 * Fisher runs `claude auth login` and shows what it prints (the URL to
 * open; a code pasted here goes to it). Claude Code keeps the token.
 */
export default function ClaudePanel() {
  const [status, setStatus] = useState<ClaudeAuth | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [account, setAccount] = useState<"subscription" | "console">("subscription");
  const [sso, setSso] = useState(false);
  const [email, setEmail] = useState("");
  const [tag, setTag] = useState<string | null>(null);
  const [lines, setLines] = useState<string[]>([]);
  const [partial, setPartial] = useState("");
  const [running, setRunning] = useState(false);
  const [code, setCode] = useState("");
  const [truncated, setTruncated] = useState(false);
  const [busy, setBusy] = useState(false);
  const [note, setNote] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const next = useRef(0);

  const refresh = useCallback(
    () =>
      api.claudeAuth().then(
        (s) => {
          setStatus(s);
          setLoadError(null);
          return s;
        },
        (e: Error) => {
          setLoadError(e.message);
          return null;
        },
      ),
    [],
  );

  // the status, and a login already going (the page was reloaded)
  useEffect(() => {
    refresh().then((s) => {
      if (s?.login) {
        next.current = 0;
        setTag(s.login);
        setRunning(true);
      }
    });
  }, [refresh]);

  // follow the login's output until it ends, then read the status again
  useEffect(() => {
    if (!tag) return;
    let live = true;
    let timer: ReturnType<typeof setTimeout> | undefined;
    const poll = () => {
      api.claudeLoginOutput(tag, next.current).then(
        (o) => {
          if (!live) return;
          if (o.lines.length) setLines((ls) => [...ls, ...o.lines]);
          next.current = o.next;
          setPartial(o.partial);
          setTruncated(o.truncated === true);
          setRunning(o.running);
          if (o.running) {
            timer = setTimeout(poll, 400);
          } else {
            setCode("");
            refresh().then((s) => {
              if (!live) return;
              if (s?.loggedIn) setNote("Logged in to Claude Code.");
              else if (o.stopping) setNote("The login was stopped.");
              else setNote(`The login ended without logging in${o.exitCode ? ` (exit code ${o.exitCode})` : ""}.`);
            });
          }
        },
        (e: Error) => {
          if (!live) return;
          setRunning(false);
          setError(`Could not follow the login: ${e.message}`);
        },
      );
    };
    poll();
    return () => {
      live = false;
      if (timer) clearTimeout(timer);
    };
  }, [tag, refresh]);

  const follow = (t: string) => {
    setLines([]);
    setPartial("");
    next.current = 0;
    setRunning(true);
    setTag(t);
  };

  const login = (e: FormEvent) => {
    e.preventDefault();
    setError(null);
    setNote(null);
    setBusy(true);
    const trimmed = email.trim();
    api
      .claudeLogin({ console: account === "console", sso, ...(trimmed ? { email: trimmed } : {}) })
      .then(
        (r) => follow(r.tag),
        (err: Error) => {
          // a login already going (another tab): follow it
          const going = err instanceof ApiError && err.status === 409 ? err.body?.tag : undefined;
          if (typeof going === "string") {
            follow(going);
            setNote("A login was already going; this is its output.");
          } else setError(`Could not start the login: ${err.message}`);
        },
      )
      .finally(() => setBusy(false));
  };

  const sendCode = (e: FormEvent) => {
    e.preventDefault();
    if (!tag || !code.trim()) return;
    setError(null);
    api.claudeLoginInput(tag, code.trim()).then(
      () => setCode(""),
      (err: Error) => setError(`Could not send the code: ${err.message}`),
    );
  };

  const stop = () => {
    if (!tag) return;
    api.claudeLoginStop(tag).catch((err: Error) => setError(`Could not stop the login: ${err.message}`));
  };

  const logout = () => {
    setError(null);
    setNote(null);
    setBusy(true);
    api
      .claudeLogout()
      .then(
        (s) => {
          setStatus(s);
          setNote("Logged out of Claude Code.");
        },
        (err: Error) => setError(`Could not log out: ${err.message}`),
      )
      .finally(() => setBusy(false));
  };

  const shown = lines.length > 0 || partial !== "";

  return (
    <section className={css.group} aria-labelledby="claude-title" data-testid="claude-panel">
      <h2 id="claude-title" className={css.panelTitle}>
        Claude Code
      </h2>
      <p className="muted">
        The <code>claude-code</code> backend runs the Claude Code CLI with its own login. Logging in here runs{" "}
        <code>claude auth login</code>; Claude Code keeps the token, Fisher never sees it.
      </p>
      <p data-testid="claude-status" aria-live="polite">
        {loadError ? `Could not read the Claude Code status: ${loadError}` : status ? statusText(status) : "Checking..."}
      </p>

      {status?.installed && !running && (
        <form className={css.claudeForm} onSubmit={login} aria-label="Log in to Claude Code">
          <fieldset className={css.choices}>
            <legend>Account</legend>
            <label className={css.choice}>
              <input
                type="radio"
                name="claude-account"
                value="subscription"
                checked={account === "subscription"}
                onChange={() => setAccount("subscription")}
              />
              Subscription (Claude Pro or Max)
            </label>
            <label className={css.choice}>
              <input
                type="radio"
                name="claude-account"
                value="console"
                checked={account === "console"}
                onChange={() => setAccount("console")}
              />
              Anthropic Console (API billing)
            </label>
            <label className={css.choice}>
              <input type="checkbox" checked={sso} onChange={(e) => setSso(e.target.checked)} />
              Force SSO
            </label>
          </fieldset>
          <label>
            Email (optional)
            <input
              type="email"
              autoComplete="email"
              value={email}
              maxLength={254}
              onChange={(e) => setEmail(e.target.value)}
              spellCheck={false}
            />
          </label>
          <div className={css.actions}>
            <button type="submit" disabled={busy}>
              {status.loggedIn ? "Log in again" : "Log in"}
            </button>
            {status.loggedIn && (
              <button type="button" onClick={logout} disabled={busy}>
                Log out
              </button>
            )}
          </div>
        </form>
      )}

      {shown && (
        <pre className={css.output} data-testid="claude-output" role="log" aria-label="Claude Code login output">
          {lines.map((l, i) => (
            <Fragment key={i}>
              {linked(l)}
              {"\n"}
            </Fragment>
          ))}
          {partial && linked(partial)}
        </pre>
      )}
      {shown && truncated && <p className="muted">The output passed 1 MB; the rest is not shown.</p>}

      {running && (
        <form className={css.claudeForm} onSubmit={sendCode} aria-label="Code for the Claude Code login">
          <label>
            Code from the browser (if the login asks for one)
            <input
              value={code}
              onChange={(e) => setCode(e.target.value)}
              autoComplete="off"
              spellCheck={false}
            />
          </label>
          <div className={css.actions}>
            <button type="submit" disabled={!code.trim()}>
              Send code
            </button>
            <button type="button" onClick={stop}>
              Stop
            </button>
          </div>
        </form>
      )}

      {note && (
        <p aria-live="polite" data-testid="claude-note">
          {note}
        </p>
      )}
      {error && (
        <p role="alert" className={css.error}>
          {error}
        </p>
      )}
    </section>
  );
}
