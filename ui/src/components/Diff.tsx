import { structuredPatch } from "diff";
import css from "./canvas.module.css";

/** The lines that changed from one step's file to another's, three lines of context around each change. */
export default function Diff({ from, to, fromLabel, toLabel }: { from: string; to: string; fromLabel: string; toLabel: string }) {
  const patch = structuredPatch(fromLabel, toLabel, from, to, "", "", { context: 3 });
  if (patch.hunks.length === 0) return <p className="muted" data-testid="diff-same">The two steps leave the file the same.</p>;
  return (
    <div className={css.diff} data-testid="diff">
      <div className={css.diffHead}>
        <span className={css.del}>--- {fromLabel}</span>
        <span className={css.add}>+++ {toLabel}</span>
      </div>
      {patch.hunks.map((h, i) => (
        <div key={i} className={css.hunk}>
          <div className={css.hunkHead}>
            @@ -{h.oldStart},{h.oldLines} +{h.newStart},{h.newLines} @@
          </div>
          {h.lines
            .filter((l) => !l.startsWith("\\"))
            .map((l, j) => {
              const op = l[0] === "+" ? "add" : l[0] === "-" ? "del" : "same";
              return (
                <div key={j} data-testid="diff-line" data-op={op} className={`${css.diffLine} ${op === "add" ? css.add : op === "del" ? css.del : ""}`}>
                  <span className={css.diffMark}>{op === "add" ? "+" : op === "del" ? "-" : " "}</span>
                  {l.slice(1)}
                </div>
              );
            })}
        </div>
      ))}
    </div>
  );
}
