import { dollars, tokens, type Cost } from "../api";
import css from "./components.module.css";

/** The session's dollars and tokens over its priced responses. */
export default function CostStrip({ cost, turns }: { cost: Cost; turns: number }) {
  return (
    <dl className={css.strip} data-testid="cost-strip">
      <div>
        <dt>Dollars</dt>
        <dd data-testid="cost-dollars">{dollars(cost.dollars)}</dd>
      </div>
      <div>
        <dt>Turns</dt>
        <dd>{turns}</dd>
      </div>
      <div>
        <dt>Input tokens</dt>
        <dd>{tokens(cost.input)}</dd>
      </div>
      <div>
        <dt>Output tokens</dt>
        <dd>{tokens(cost.output)}</dd>
      </div>
      <div>
        <dt>Cache read</dt>
        <dd>{tokens(cost.cacheRead)}</dd>
      </div>
    </dl>
  );
}
