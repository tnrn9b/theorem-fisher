import { useState } from "react";
import type { Message, ToolCallJson } from "../api";
import css from "./components.module.css";

const previewChars = 160;

function preview(t: string | null | undefined): string {
  if (!t) return "";
  const one = t.replace(/\s+/g, " ").trim();
  return one.length > previewChars ? `${one.slice(0, previewChars)}...` : one;
}

/** Arguments as indented JSON when they parse, else as sent. */
function args(a: string | undefined): string {
  if (!a) return "";
  try {
    return JSON.stringify(JSON.parse(a), null, 2);
  } catch {
    return a;
  }
}

/** One tool call with its result: collapsed to a one-line preview. */
export default function ToolCall({ call, result }: { call: ToolCallJson; result?: Message }) {
  const [open, setOpen] = useState(false);
  const name = call.function?.name ?? result?.toolName ?? "tool";
  return (
    <div className={css.tool} data-testid="tool-call" data-open={open}>
      <button type="button" className={css.toolHead} aria-expanded={open} onClick={() => setOpen(!open)}>
        <span className={css.caret} aria-hidden>
          {open ? "▾" : "▸"}
        </span>
        <span className={css.toolName}>{name}</span>
        {!open && <span className={css.preview}>{result ? preview(result.content) : "no result"}</span>}
      </button>
      {open && (
        <div className={css.toolBody} data-testid="tool-body">
          <div className={css.label}>Arguments</div>
          <pre>{args(call.function?.arguments) || "(none)"}</pre>
          <div className={css.label}>Result</div>
          <pre>{result ? (result.content ?? "") : "(no result recorded)"}</pre>
        </div>
      )}
    </div>
  );
}
