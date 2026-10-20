import { expect, test, type Page } from "@playwright/test";

// The goal board over fisher-ui's search fixtures (test/fixtures/search):
// s.20261016-090000 a lemma-graph run (root Demo.main_thm open, its split 1
// accepted with Demo.main_thm_l1 auto closed and Demo.main_thm_l2
// disproved, split 2 rejected; Demo.side_thm proved after a repair) and
// s.20261016-080000 a flat run (Demo.a auto closed, Demo.b closed after a
// repair, Demo.c disproved, Demo.d given up). A run started from the page
// is the fixture's fake fisher search: Demo.a's events a line at a time
// over about four seconds, then its report.
const token = "0123456789abcdef0123456789abcdef";
const graphRun = "s.20261016-090000";
const flatRun = "s.20261016-080000";

async function signIn(page: Page) {
  await page.goto(`/?t=${token}`);
  await expect(page).toHaveURL(/\/sessions$/);
}

async function top(page: Page, testId: string): Promise<number> {
  const box = await page.getByTestId(testId).boundingBox();
  if (!box) throw new Error(`${testId} has no box`);
  return box.y;
}

test("the graph board: a split's lemmas under their parent, the auto closed and disproof marks, whiteboards, apply refused", async ({ page }) => {
  await signIn(page);
  await page.goto("/search");
  await page.locator(`[data-testid=search-row][data-run="${graphRun}"] a`).click();
  await expect(page).toHaveURL(new RegExp(`/search/${graphRun.replace(".", "\\.")}$`));
  await expect(page.getByTestId("run-status")).toHaveText("done");
  const graph = page.getByTestId("lemma-graph");
  await expect(graph).toBeVisible();
  for (const id of [1, 2, 3, 4]) await expect(page.getByTestId(`node-${id}`)).toBeVisible();
  await expect(page.getByTestId("split-1")).toHaveAttribute("data-status", "accepted");
  await expect(page.getByTestId("split-2")).toHaveAttribute("data-status", "rejected");
  // the parent, then its split, then the split's lemmas below it
  const [y1, ys, y3, y4] = [await top(page, "node-1"), await top(page, "split-1"), await top(page, "node-3"), await top(page, "node-4")];
  expect(ys).toBeGreaterThan(y1);
  expect(y3).toBeGreaterThan(ys);
  expect(y4).toBeGreaterThan(ys);
  expect(Math.abs(y3 - y4)).toBeLessThan(2);
  await expect(page.getByTestId("node-1")).toHaveAttribute("data-status", "open");
  await expect(page.getByTestId("node-3")).toHaveAttribute("data-status", "proved");
  await expect(page.getByTestId("node-4")).toHaveAttribute("data-status", "failed");
  await expect(page.getByTestId("node-3").locator("[data-testid=mark]")).toHaveAttribute("data-kind", "auto closed");
  await expect(page.getByTestId("node-4").locator("[data-testid=mark]")).toHaveAttribute("data-kind", "disproved");
  await expect(page.getByTestId("node-2").locator("[data-testid=mark]")).toHaveAttribute("data-kind", "repaired");
  // whiteboards, one per root
  const boards = page.getByTestId("whiteboard");
  await expect(boards).toHaveCount(2);
  await expect(boards.nth(0)).toContainText("split 1 (accepted)");
  await expect(boards.nth(1)).toContainText("Demo.side_thm (proved)");
  // a lemma from the split is still sorry: no apply
  await expect(page.getByTestId("apply")).toBeDisabled();
  await expect(page.getByTestId("apply-reason")).toContainText("Demo.main_thm_l2");
  await expect(page.getByTestId("run-closed")).toHaveText("1 of 2 closed");
});

test("the bench g graph run shows its one goal proved", async ({ page }) => {
  await signIn(page);
  await page.goto("/search/g.graph.5");
  await expect(page.getByTestId("node-1")).toHaveAttribute("data-status", "proved");
  await expect(page.getByTestId("node-1")).toContainText("Step.Relay.fireCand_third");
  await expect(page.getByTestId("whiteboard")).toHaveCount(1);
});

test("the flat board: statuses, closedBy and marks from the report; apply writes the file, then asks before writing over a changed one", async ({ page }) => {
  await signIn(page);
  await page.goto(`/search/${flatRun}`);
  await expect(page.getByTestId("run-status")).toHaveText("done");
  const rows = page.getByTestId("goal-row");
  await expect(rows).toHaveCount(4);
  expect(await rows.evaluateAll((es) => es.map((e) => [e.getAttribute("data-goal"), e.getAttribute("data-status")]))).toEqual([
    ["Demo.a", "closed"],
    ["Demo.b", "closed"],
    ["Demo.c", "disproved"],
    ["Demo.d", "gave up"],
  ]);
  const row = (g: string) => page.locator(`[data-testid=goal-row][data-goal="${g}"]`);
  await expect(row("Demo.a").getByTestId("closed-by")).toHaveText("auto");
  await expect(row("Demo.b").getByTestId("closed-by")).toHaveText("model+repair");
  await expect(row("Demo.a").locator("[data-kind='auto closed']")).toBeVisible();
  await expect(row("Demo.b").locator("[data-kind='repaired']")).toBeVisible();
  await expect(row("Demo.c").locator("[data-kind='disproved']")).toBeVisible();
  await expect(row("Demo.d").locator("[data-kind='auto missed']")).toBeVisible();
  await expect(page.getByTestId("apply")).toBeEnabled();
  await page.getByTestId("apply").click();
  await expect(page.getByRole("status")).toContainText("Demo/Flat.lean");
  // the file now differs from the one the run started on: apply asks first
  await page.getByTestId("apply").click();
  await expect(page.getByTestId("apply-confirm")).toContainText("written after the run started");
  await page.getByTestId("apply-force").click();
  await expect(page.getByRole("status")).toContainText("Demo/Flat.lean");
  await expect(page.getByTestId("apply-confirm")).toHaveCount(0);
});

test("run again starts a new run on the file and its board follows the growing events file", async ({ page }) => {
  await signIn(page);
  await page.goto(`/search/${flatRun}`);
  await page.getByRole("button", { name: "Run again" }).click();
  await expect(page).not.toHaveURL(new RegExp(`${flatRun.replace(".", "\\.")}$`));
  await expect(page).toHaveURL(/\/search\/s\./);
  const a = page.locator("[data-testid=goal-row][data-goal='Demo.a']");
  await expect(page.getByTestId("run-status")).toHaveText("running");
  await expect(a.locator("[data-kind='auto missed']")).toBeVisible();
  await expect(a).toHaveAttribute("data-status", "running");
  await expect(a).toHaveAttribute("data-status", "closed", { timeout: 10000 });
  await expect(a.getByTestId("closed-by")).toHaveText("model");
  await expect(page.getByTestId("run-status")).toHaveText("done", { timeout: 10000 });
  await expect(page.getByTestId("events").locator("li")).toHaveCount(4);
});

// A run recorded before the rename names the previous backend in its report,
// its attempt directories and its events. The board shows it as any other
// run: backend names are data. The previous name is built here, so that the
// repository does not spell it.
const oldBackend = "ga" + "uss";

test("a run whose report and events name the previous backend is still shown", async ({ page }) => {
  let swapped = 0;
  await page.route(new RegExp(`/api/search/${flatRun.replace(".", "\\.")}(/events)?(\\?.*)?$`), async (route) => {
    const res = await route.fetch();
    const body = (await res.text()).replace(/"fisher"/g, `"${oldBackend}"`).replace(/-fisher"/g, `-${oldBackend}"`);
    if (body.includes(oldBackend)) swapped += 1;
    await route.fulfill({ response: res, body });
  });
  await signIn(page);
  await page.goto(`/search/${flatRun}`);
  await expect(page.getByTestId("run-status")).toHaveText("done");
  const rows = page.getByTestId("goal-row");
  await expect(rows).toHaveCount(4);
  expect(await rows.evaluateAll((es) => es.map((e) => [e.getAttribute("data-goal"), e.getAttribute("data-status")]))).toEqual([
    ["Demo.a", "closed"],
    ["Demo.b", "closed"],
    ["Demo.c", "disproved"],
    ["Demo.d", "gave up"],
  ]);
  const row = (g: string) => page.locator(`[data-testid=goal-row][data-goal="${g}"]`);
  await expect(row("Demo.b").getByTestId("closed-by")).toHaveText("model+repair");
  await expect(row("Demo.b").locator("[data-kind='repaired']")).toBeVisible();
  // both the report and the events stream carried the previous name
  expect(swapped).toBeGreaterThanOrEqual(2);
});
