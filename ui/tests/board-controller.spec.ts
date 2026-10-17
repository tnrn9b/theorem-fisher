import { expect, test } from "@playwright/test";
import { foldFlat, foldGraph, foldTiers, type SearchEvent } from "../src/board";

// The reducer alone (no browser): the controller's tier and stop events as
// fisher-search's Report.eventJson and graphEventJson write them.
const flat: SearchEvent[] = [
  { event: "tier skipped", t: 0.1, model: "example/model-b", reason: "no key" },
  { event: "launched", t: 1, attempt: 1, goal: "Demo.a" },
  { event: "tier chosen", t: 1, attempt: 1, goal: "Demo.a", tier: 0, model: "example/model-a" },
  { event: "attempt stopped", t: 5, attempt: 1, goal: "Demo.a", reason: "turns", turns: 12, dollars: 0.4 },
  { event: "finished", t: 5, attempt: 1, goal: "Demo.a" },
  { event: "escalated", t: 5, goal: "Demo.a", from: 0, to: 1 },
  { event: "launched", t: 6, attempt: 2, goal: "Demo.a" },
  { event: "tier chosen", t: 6, attempt: 2, goal: "Demo.a", tier: 1, model: "example/model-b" },
  { event: "attempt stopped", t: 9, attempt: 2, goal: "Demo.a", reason: "dollars", turns: 3, dollars: 2.5 },
  { event: "ledger unread", t: 9, attempt: 2, reason: "not json" },
];

test("flat board: tier badges with the model, stop marks, escalation, skipped tiers", () => {
  const [a] = foldFlat(flat, null);
  const tiers = a.marks.filter((m) => m.kind === "tier");
  expect(tiers.map((m) => [m.label, m.title])).toEqual([
    ["T0", "example/model-a"],
    ["T1", "example/model-b"],
  ]);
  expect(a.marks.filter((m) => m.kind === "attempt stopped").map((m) => m.label)).toEqual(["#1 stopped (turns)", "#2 stopped (dollars)"]);
  expect(a.marks.find((m) => m.kind === "escalated")?.label).toBe("escalated T0 to T1");
  expect(foldTiers(flat)).toEqual({ skipped: ["example/model-b (no key)"], ledgerUnread: ["attempt 2: not json"] });
});

test("graph board: node forms, and an attempt stop leaves the run's own stop reason alone", () => {
  const events: SearchEvent[] = [
    { event: "node added", t: 0, node: 1, name: "Demo.main", depth: 0 },
    { event: "tier chosen", t: 1, attempt: 1, node: 1, tier: 0, model: "example/model-a" },
    { event: "attempt stopped", t: 2, attempt: 1, node: 1, reason: "turns", turns: 9, dollars: 0.2 },
    { event: "escalated", t: 2, node: 1, from: 0, to: 1 },
    { event: "stopped", t: 3, reason: "budget" },
  ];
  const st = foldGraph(events, null);
  expect(st.stopped).toBe("budget");
  expect(st.nodes.get(1)!.marks.map((m) => m.label)).toEqual(["T0", "#1 stopped (turns)", "escalated T0 to T1"]);
  // in either order
  const rev = foldGraph([...events.slice(0, 1), events[4], ...events.slice(1, 4)], null);
  expect(rev.stopped).toBe("budget");
});

test("two attempts on one goal that both stop on turns, and both use tier 1, each keep their mark", () => {
  const evs: SearchEvent[] = [
    { event: "tier chosen", t: 1, attempt: 3, goal: "Demo.a", tier: 1, model: "example/model-b" },
    { event: "attempt stopped", t: 2, attempt: 3, goal: "Demo.a", reason: "turns", turns: 12, dollars: 0.4 },
    { event: "tier chosen", t: 3, attempt: 4, goal: "Demo.a", tier: 1, model: "example/model-b" },
    { event: "attempt stopped", t: 4, attempt: 4, goal: "Demo.a", reason: "turns", turns: 11, dollars: 0.5 },
  ];
  const [a] = foldFlat(evs, null);
  expect(a.marks.filter((m) => m.kind === "attempt stopped").map((m) => [m.label, m.title])).toEqual([
    ["#3 stopped (turns)", "12 turns, $0.4"],
    ["#4 stopped (turns)", "11 turns, $0.5"],
  ]);
  expect(a.marks.filter((m) => m.kind === "tier").map((m) => m.label)).toEqual(["T1", "T1"]);
});
