import type { StepHead, Verdict } from "../api";
import { when } from "../api";
import css from "./canvas.module.css";

/** A verdict in a few words: ok, the errors, the holes left, or unchecked. */
export function verdictWords(v: Verdict | null | undefined): string {
  if (!v) return "unchecked";
  if (v.ok) return "ok";
  const parts: string[] = [];
  if (v.errors.length) parts.push(`${v.errors.length} error${v.errors.length === 1 ? "" : "s"}`);
  if (v.holes) parts.push(`${v.holes} hole${v.holes === 1 ? "" : "s"}`);
  return parts.length ? parts.join(", ") : "not ok";
}

function verdictClass(v: Verdict | null | undefined): string {
  if (!v) return css.unchecked;
  return v.ok ? css.okMark : v.errors.length ? css.errMark : css.holeMark;
}

/**
 * One tick per file change: its number, the tool that made it and its
 * check. The slider scrubs through them (arrow keys, Home, End); a tick
 * jumps to its step.
 */
export default function Timeline({
  steps,
  verdicts,
  selected,
  onSelect,
}: {
  steps: StepHead[];
  verdicts: Map<number, Verdict>;
  selected: number;
  onSelect: (seq: number) => void;
}) {
  const index = Math.max(0, steps.findIndex((s) => s.seq === selected));
  return (
    <div className={css.timeline}>
      <input
        type="range"
        aria-label="Step"
        min={0}
        max={Math.max(0, steps.length - 1)}
        step={1}
        value={index}
        onChange={(e) => {
          const s = steps[Number(e.target.value)];
          if (s) onSelect(s.seq);
        }}
        className={css.slider}
      />
      <ol className={css.ticks} aria-label="Steps">
        {steps.map((s) => {
          const v = verdicts.get(s.seq);
          return (
            <li key={s.seq}>
              <button
                type="button"
                data-testid="tick"
                data-seq={s.seq}
                data-tool={s.tool}
                aria-pressed={s.seq === selected}
                aria-label={`Step ${s.seq}, ${s.tool}, ${verdictWords(v)}`}
                className={`${css.tick} ${s.seq === selected ? css.tickOn : ""}`}
                title={`${s.path}\n${when(s.at)}`}
                onClick={() => onSelect(s.seq)}
              >
                <span className={css.tickSeq}>{s.seq}</span>
                <span className={css.tickTool}>{s.tool}</span>
                <span className={`${css.tickVerdict} ${verdictClass(v)}`} data-testid="tick-verdict">
                  {verdictWords(v)}
                </span>
              </button>
            </li>
          );
        })}
      </ol>
    </div>
  );
}
