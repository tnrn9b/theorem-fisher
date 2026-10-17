import css from "./board.module.css";

/**
 * A root goal's whiteboard (whiteboard-<root>-<name>.md, as fisher-search's
 * Whiteboard writes it): headings and an indented list, shown as written.
 */
export default function Whiteboard({ root, name, text }: { root: number | null; name: string | null; text: string }) {
  const lines = text.split("\n");
  return (
    <section className={css.board} data-testid="whiteboard" data-root={root ?? ""}>
      <h3>{name ?? `Whiteboard${root !== null ? ` of goal ${root}` : ""}`}</h3>
      <div className={css.boardText}>
        {lines.map((l, i) => {
          const h = /^(#+)\s+(.*)$/.exec(l);
          if (h) return <div key={i} className={css.boardHead}>{h[2]}</div>;
          const item = /^(\s*)[-*]\s+(.*)$/.exec(l);
          if (item)
            return (
              <div key={i} className={css.boardItem} style={{ paddingLeft: `${item[1].length * 0.6 + 0.8}rem` }}>
                {item[2]}
              </div>
            );
          return <div key={i}>{l || " "}</div>;
        })}
      </div>
    </section>
  );
}
