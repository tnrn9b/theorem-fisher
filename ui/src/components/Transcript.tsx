import type { Message } from "../api";
import { when } from "../api";
import ToolCall from "./ToolCall";
import css from "./components.module.css";

/**
 * The messages in the order the agent wrote them. A tool result is shown
 * inside the call that asked for it; one with no matching call stands alone.
 */
export default function Transcript({ messages }: { messages: Message[] }) {
  const callsOf = (m: Message) => (Array.isArray(m.toolCalls) ? m.toolCalls : []);
  const results = new Map<string, Message>();
  for (const m of messages) if (m.role === "tool" && m.toolCallId) results.set(m.toolCallId, m);
  const asked = new Set<string>();
  for (const m of messages) for (const c of callsOf(m)) if (c.id && results.has(c.id)) asked.add(c.id);

  return (
    <ol className={css.transcript} data-testid="transcript">
      {messages.map((m, i) => {
        if (m.role === "tool" && m.toolCallId && asked.has(m.toolCallId)) return null;
        const calls = callsOf(m);
        return (
          <li key={i} className={`${css.msg} ${css[m.role] ?? ""}`} data-testid="message" data-role={m.role}>
            <div className={css.msgHead}>
              <span className={css.role}>{m.role === "tool" ? `tool ${m.toolName ?? ""}` : m.role}</span>
              <span className={css.at}>{when(m.at)}</span>
            </div>
            {m.content ? <div className={css.content}>{m.content}</div> : null}
            {calls.map((c, j) => (
              <ToolCall key={c.id ?? j} call={c} result={c.id ? results.get(c.id) : undefined} />
            ))}
          </li>
        );
      })}
    </ol>
  );
}
