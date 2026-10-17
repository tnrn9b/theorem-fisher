import { shownStatus, type GoalState } from "../board";
import { MarkChips } from "./LemmaGraph";
import css from "./board.module.css";

/**
 * One goal of a flat run: its status (running while an attempt is out),
 * how it closed, the automation, repair and disproof marks, and retry.
 * fisher search has no single-goal flag, so a retry runs the whole file
 * again as it is now.
 */
export default function GoalRow({ goal, onRetry, retrying }: { goal: GoalState; onRetry?: () => void; retrying?: boolean }) {
  const st = shownStatus(goal);
  return (
    <tr data-testid="goal-row" data-goal={goal.goal} data-status={st}>
      <td className={css.goalName}>{goal.goal}</td>
      <td>
        <span className={`${css.status} ${css[`st_${st.replace(/ /g, "_")}`] ?? ""}`} data-testid="goal-status">
          {st}
        </span>
      </td>
      <td data-testid="closed-by">{goal.closedBy ?? ""}</td>
      <td className={css.num}>{goal.attempts}</td>
      <td>
        <MarkChips marks={goal.marks} />
      </td>
      <td>
        {onRetry && st !== "closed" && st !== "running" && (
          <button type="button" onClick={onRetry} disabled={retrying} title="Starts a new run on this file as it is now">
            Retry
          </button>
        )}
      </td>
    </tr>
  );
}
