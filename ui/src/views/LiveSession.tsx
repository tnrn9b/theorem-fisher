import { useCallback, useEffect, useRef, useState, type FormEvent } from "react";
import { Link, useParams, useSearchParams } from "react-router-dom";
import { api, dollars, ended, type LiveEvent, type RunView } from "../api";
import css from "../components/components.module.css";
import vcss from "./views.module.css";

function str(v: unknown): string {
  if (v === null || v === undefined) return "";
  return typeof v === "string" ? v : JSON.stringify(v);
}

function clip(s: string, n: number): string {
  return s.length > n ? `${s.slice(0, n)}...` : s;
}

/** One line for an event of the ledger (fisher-agent Fisher.Agent.Ledger). */
export function eventLine(e: LiveEvent): string {
  const d = e.data ?? {};
  switch (e.kind) {
    case "tool_start":
      return `${str(d.name)} ${clip(str(d.args), 160)}`;
    case "tool_end":
      return `${str(d.name)} ${d.ok === false ? "failed" : "done"}${d.preview ? `: ${clip(str(d.preview), 160)}` : ""}`;
    case "thinking":
      return str(d.label);
    case "message":
      return `${str(d.role)}: ${clip(str(d.text), 400)}`;
    case "cost":
      return `${typeof d.dollars === "number" ? dollars(d.dollars) : "?"}, ${str(d.input)} in, ${str(d.output)} out`;
    case "step":
      return `${str(d.path)} (step ${str(d.seq)})`;
    case "chat":
      return `started with ${str(d.model)}${d.toolsets ? `, tools ${str(d.toolsets)}` : ""}`;
    case "inbox":
      return `message delivered: ${clip(str(d.text), 400)}`;
    case "end":
      return `${str(d.status)}${d.exit !== undefined && d.exit !== null ? ` (exit ${str(d.exit)})` : ""}`;
    default:
      return clip(JSON.stringify(d), 200);
  }
}

const kindLabels: Record<string, string> = {
  tool_start: "tool",
  tool_end: "result",
  thinking: "thinking",
  message: "message",
  cost: "cost",
  step: "step",
  inbox: "you",
  chat: "start",
  end: "end",
};

/**
 * A session as it runs: its ledger streamed from the server (resumed after
 * the last event seen when the stream reconnects), stop, and a message
 * box. A message waits in the session's inbox until the agent's next tool
 * call; it shows as queued until the ledger says it was delivered.
 */
export default function LiveSession() {
  const { id = "" } = useParams();
  // ?after=N: the events after N (a resumed session: the earlier run's are not this run's)
  const [search] = useSearchParams();
  const after = Math.max(0, Number(search.get("after") ?? 0) || 0);
  const [events, setEvents] = useState<LiveEvent[]>([]);
  const [run, setRun] = useState<RunView | null>(null);
  const [runError, setRunError] = useState<string | null>(null);
  const [streamState, setStreamState] = useState<"connecting" | "open" | "ended" | "lost">("connecting");
  const [queued, setQueued] = useState<{ key: number; text: string }[]>([]);
  const [draft, setDraft] = useState("");
  const [sendError, setSendError] = useState<string | null>(null);
  const [stopping, setStopping] = useState(false);
  const seen = useRef(new Set<number>());
  const nextKey = useRef(0);

  // the stream: each number once, in order
  useEffect(() => {
    seen.current = new Set();
    setEvents([]);
    setStreamState("connecting");
    const es = new EventSource(api.eventsUrl(id, after));
    es.onopen = () => setStreamState("open");
    es.onmessage = (m: MessageEvent<string>) => {
      let e: LiveEvent;
      try {
        e = JSON.parse(m.data) as LiveEvent;
      } catch {
        return;
      }
      if (seen.current.has(e.seq)) return;
      seen.current.add(e.seq);
      setEvents((xs) => (xs.length && xs[xs.length - 1].seq > e.seq ? [...xs, e].sort((a, b) => a.seq - b.seq) : [...xs, e]));
      if (e.kind === "inbox") {
        const text = str(e.data?.text);
        setQueued((q) => {
          const i = q.findIndex((x) => x.text === text);
          return i < 0 ? q : [...q.slice(0, i), ...q.slice(i + 1)];
        });
      }
      if (e.kind === "end") {
        es.close();
        setStreamState("ended");
      }
    };
    es.onerror = () => {
      // closed by the server past the end (204), or the server went away;
      // the browser retries on its own unless it gave up
      setStreamState(es.readyState === EventSource.CLOSED ? "lost" : "connecting");
    };
    return () => es.close();
  }, [id, after]);

  // the run's status, polled while it runs
  const refresh = useCallback(
    () =>
      api.live(id).then(
        (r) => {
          setRun(r);
          setRunError(null);
          return r;
        },
        (e: Error) => {
          setRunError(e.message);
          return null;
        },
      ),
    [id],
  );
  useEffect(() => {
    let live = true;
    let timer: number | undefined;
    const tick = () =>
      refresh().then((r) => {
        if (live && (!r || r.running)) timer = window.setTimeout(tick, 1000);
      });
    tick();
    return () => {
      live = false;
      window.clearTimeout(timer);
    };
  }, [refresh]);

  const ended_ = streamState === "ended" || (run !== null && !run.running && ended(run.status));
  useEffect(() => {
    if (streamState === "ended") refresh();
  }, [streamState, refresh]);

  const stop = () => {
    setStopping(true);
    api.stop(id).then(
      () => refresh(),
      (e: Error) => {
        setRunError(e.message);
        setStopping(false);
      },
    );
  };

  const send = (e: FormEvent) => {
    e.preventDefault();
    const text = draft.trim();
    if (!text) return;
    setSendError(null);
    api.message(id, text).then(
      () => {
        const key = nextKey.current++;
        setQueued((q) => [...q, { key, text }]);
        setDraft("");
      },
      (err: Error) => setSendError(err.message),
    );
  };

  const status = run?.status ?? (streamState === "ended" ? str(events[events.length - 1]?.data?.status) : null);
  const canSteer = !!run?.running && !!run.tag;
  // running in another process (a terminal, another server): it reads its
  // inbox, so a message reaches it; it is stopped where it runs
  const elsewhere = !canSteer && typeof run?.elsewhere === "number" ? run.elsewhere : null;
  const canSend = canSteer || elsewhere !== null;
  const endEvent = events.find((e) => e.kind === "end");

  return (
    <main className={css.live}>
      <p className={vcss.crumbs}>
        <Link to="/sessions">Sessions</Link>
      </p>
      <h1>Live: {id}</h1>
      <p className={vcss.meta}>
        <span>
          status <strong data-testid="live-status">{status ?? "unknown"}</strong>
        </span>
        {run?.command && <span className={vcss.mono}>{run.command}</span>}
        {run?.project && <span title="The project the run started in">{run.project}</span>}
        <span data-testid="stream-state">stream {streamState}</span>
        <Link to={`/session/${encodeURIComponent(id)}`}>Transcript</Link>
      </p>
      {runError && <p role="alert">Could not read the run: {runError}</p>}
      {elsewhere !== null && (
        <p className="muted" data-testid="elsewhere">
          This session is running in another process (pid {elsewhere}). A message reaches it through its inbox; stop it
          where it runs.
        </p>
      )}
      {run && !run.tag && !ended_ && elsewhere === null && (
        <p className="muted">This session was not started by this fisher ui; it can be watched, not steered.</p>
      )}
      {canSend && (
        <div className={css.liveControls}>
          {canSteer && (
            <button type="button" onClick={stop} disabled={stopping || run?.status === "stopping"}>
              {stopping || run?.status === "stopping" ? "Stopping..." : "Stop"}
            </button>
          )}
          <form onSubmit={send} className={css.sendForm}>
            <label>
              Message to the agent
              <textarea value={draft} onChange={(e) => setDraft(e.target.value)} rows={2} />
            </label>
            <button type="submit" disabled={!draft.trim()}>
              Send
            </button>
          </form>
          <p className="muted">A message reaches the agent at its next tool call.</p>
          {sendError && <p role="alert">Could not send: {sendError}</p>}
        </div>
      )}
      {queued.length > 0 && (
        <ul className={css.queued} aria-label="Queued messages">
          {queued.map((q) => (
            <li key={q.key} data-testid="queued">
              <span className={css.queuedTag}>queued</span> {q.text}
            </li>
          ))}
        </ul>
      )}
      {ended_ && queued.length > 0 && (
        <p className="muted">The run ended before these messages reached the agent.</p>
      )}
      {events.length === 0 ? (
        <p className="muted">{streamState === "lost" ? "No events." : "Waiting for the first event..."}</p>
      ) : (
        <ol className={css.events} data-testid="live-events">
          {events.map((e) => (
            <li key={e.seq} data-testid="live-event" data-seq={e.seq} data-kind={e.kind} className={css[`ev_${e.kind}`] ?? ""}>
              <span className={css.evSeq}>{e.seq}</span>
              <span className={css.evKind}>{kindLabels[e.kind] ?? e.kind}</span>
              <span className={css.evText}>{eventLine(e)}</span>
            </li>
          ))}
        </ol>
      )}
      {endEvent && (
        <p data-testid="live-ended">
          The run ended: {eventLine(endEvent)}.
        </p>
      )}
    </main>
  );
}
