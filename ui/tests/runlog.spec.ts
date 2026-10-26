import { expect, test, type Page } from "@playwright/test";

// The Run log and Learn views over the stub's run log (runlogRoutes and
// learnRoutes in stub-server.mjs): a search s.20261024-100000 in /work/demo
// and its closed attempt a.20261024-100500 (backend fisher, three events),
// and the project's policy, which puts fisher before claude-code; and its
// lessons (lessonsRoutes): a candidate 1, an active 2 and a judged
// candidate 4 proposed (waiting for a Pin) of /work/demo, and a global
// active 3.
const token = "0123456789abcdef0123456789abcdef";
const search = "s.20261024-100000";
const attempt = "a.20261024-100500";

async function signIn(page: Page) {
  await page.goto(`/?t=${token}`);
  await expect(page).toHaveURL(/\/sessions$/);
}

test("the run log lists two runs, newest first, and a run's page shows its goals and events", async ({ page }) => {
  await signIn(page);
  await page.getByRole("link", { name: "Run log" }).click();
  await expect(page).toHaveURL(/\/runlog$/);
  const rows = page.getByTestId("runlog-row");
  await expect(rows).toHaveCount(2);
  await expect(rows.nth(0)).toHaveAttribute("data-run", attempt);
  await expect(rows.nth(1)).toHaveAttribute("data-run", search);
  await rows.nth(0).getByRole("link").click();
  await expect(page).toHaveURL(new RegExp(`/runlog/${attempt.replace(".", "\\.")}$`));
  const events = page.getByTestId("runlog-event");
  await expect(events).toHaveCount(3);
  await expect(events.nth(0)).toContainText("attempt");
  await expect(page.getByTestId("runlog-meta")).toContainText("closed");
  await page.getByRole("link", { name: "Run log" }).first().click();
  await page.getByTestId("runlog-row").nth(1).getByRole("link").click();
  await expect(page.getByTestId("runlog-goal")).toHaveCount(1);
  await expect(page.getByTestId("runlog-goal")).toContainText("Demo.main_thm");
});

test("the run log filters by kind and by project, each control labelled", async ({ page }) => {
  await signIn(page);
  await page.goto("/runlog");
  await expect(page.getByTestId("runlog-row")).toHaveCount(2);
  await page.getByLabel("Kind").selectOption("search-attempt");
  await expect(page.getByTestId("runlog-row")).toHaveCount(1);
  await expect(page).toHaveURL(/kind=search-attempt/);
  await page.getByLabel("Kind").selectOption("");
  await expect(page.getByTestId("runlog-row")).toHaveCount(2);
  await page.getByLabel("Project").selectOption("/work/other");
  await expect(page.getByTestId("runlog-row")).toHaveCount(0);
  await expect(page.getByText("No runs")).toBeVisible();
});

test("Learn shows the project's backends table with fisher first, the policy and its changes", async ({ page }) => {
  await signIn(page);
  await page.getByRole("link", { name: "Learn" }).click();
  await expect(page).toHaveURL(/\/learn/);
  await expect(page.getByLabel("Project", { exact: true })).toHaveValue("/work/demo");
  const backends = page.getByTestId("stats-*-backends");
  await expect(backends).toBeVisible();
  const arms = backends.getByTestId("stats-arm");
  await expect(arms.first()).toHaveText("fisher");
  await expect(arms.nth(1)).toHaveText("claude-code");
  await expect(page.getByTestId("policy-*")).toContainText("fisher, claude-code");
  await expect(page.getByTestId("policy-change")).toHaveCount(1);
  // another project: no policy, no runs
  await page.getByLabel("Project", { exact: true }).selectOption("/work/other");
  await expect(page.getByText("No policy for this project")).toBeVisible();
});

test("Learn lists the project's lessons and the global ones, and Pin pins a candidate", async ({ page }) => {
  await signIn(page);
  await page.goto("/learn?project=/work/demo");
  const own = page.getByRole("table", { name: "Project lessons" });
  await expect(own.getByTestId("lesson-row")).toHaveCount(2);
  const cand = own.locator('[data-lesson="1"]');
  await expect(cand).toContainText("Run `lake build` before finishing.");
  await expect(cand.getByTestId("lesson-status")).toHaveText("candidate");
  await expect(cand.getByTestId("lesson-evidence")).toHaveText("2");
  await expect(page.getByRole("table", { name: "Global lessons" }).getByTestId("lesson-row")).toHaveCount(1);
  await expect(page.getByRole("button", { name: "Unpin lesson 1", exact: true })).toHaveCount(0);
  await page.getByRole("button", { name: "Pin lesson 1", exact: true }).click();
  await expect(cand.getByTestId("lesson-status")).toHaveText("pinned");
  await expect(page.getByRole("button", { name: "Unpin lesson 1", exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Pin lesson 1", exact: true })).toHaveCount(0);
  // a refusal is said, and the row stays as it was
  await page.getByRole("button", { name: "Drop lesson 3", exact: true }).click();
  await expect(page.getByRole("alert")).toContainText("lesson 3: the stub refuses");
  await expect(page.getByTestId("lessons-global").getByTestId("lesson-status")).toHaveText("active");
  // the pin is what the server holds: it survives a reload
  await page.reload();
  await expect(page.getByTestId("lessons-project").locator('[data-lesson="1"]').getByTestId("lesson-status")).toHaveText("pinned");
});

test("Learn shows a judged lesson with the counts as proposed, needing approval, and Pin approves it", async ({ page }) => {
  await signIn(page);
  await page.goto("/learn?project=/work/demo");
  const proposed = page.getByRole("table", { name: "Proposed lessons" });
  await expect(proposed.getByTestId("lesson-row")).toHaveCount(1);
  const row = proposed.locator('[data-lesson="4"]');
  await expect(row).toContainText("Check `Foo.bar_baz` before using it.");
  await expect(row.getByTestId("lesson-approval")).toHaveText("needs your approval");
  // listed once: not in the project's table while it waits
  await expect(page.getByTestId("lessons-project").locator('[data-lesson="4"]')).toHaveCount(0);
  await expect(page.getByTestId("lessons-project").getByTestId("lesson-approval")).toHaveCount(0);
  await page.getByRole("button", { name: "Pin lesson 4", exact: true }).click();
  const pinned = page.getByTestId("lessons-project").locator('[data-lesson="4"]');
  await expect(pinned.getByTestId("lesson-status")).toHaveText("pinned");
  await expect(pinned.getByTestId("lesson-approval")).toHaveCount(0);
  await expect(page.getByRole("table", { name: "Proposed lessons" })).toHaveCount(0);
  await page.reload();
  await expect(page.getByTestId("lessons-project").locator('[data-lesson="4"]').getByTestId("lesson-status")).toHaveText("pinned");
  // unpinned, it is proposed again, and the page says so
  await page.getByRole("button", { name: "Unpin lesson 4", exact: true }).click();
  await expect(page.getByTestId("lessons-notice")).toHaveText("lesson 4: unpinned; it is proposed again until you pin it");
  await expect(page.getByRole("table", { name: "Proposed lessons" }).locator('[data-lesson="4"]').getByTestId("lesson-approval")).toBeVisible();
  await expect(page.getByTestId("lessons-project").locator('[data-lesson="4"]')).toHaveCount(0);
});
