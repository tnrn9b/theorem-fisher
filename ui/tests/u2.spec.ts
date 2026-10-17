import { expect, test, type Page } from "@playwright/test";

// Against the stub's scripted run or the fixture server's fake fisher
// (fisher-ui/test/Fisher/UI/FakeFisher.hs pageRun): three events 0.3 s
// apart, each message back as an inbox event, until stopped.
const token = "0123456789abcdef0123456789abcdef";

async function signIn(page: Page) {
  await page.goto(`/?t=${token}`);
  await expect(page).toHaveURL(/\/sessions$/);
}

async function seqs(page: Page): Promise<number[]> {
  return page.getByTestId("live-event").evaluateAll((es) => es.map((e) => Number(e.getAttribute("data-seq"))));
}

test("start a run from the page, watch three events, reload with no duplicate, send a message, stop it", async ({ page }) => {
  await signIn(page);
  // a project the sessions already name
  await expect(page.locator("#projects option").first()).toBeAttached();
  const project = await page.locator("#projects option").first().getAttribute("value");
  expect(project).toBeTruthy();
  await page.getByLabel("Project directory").fill(project!);
  await page.getByLabel("Command").fill("/prove theories/Demo.v");
  await page.getByRole("button", { name: "Start" }).click();

  await expect(page).toHaveURL(/\/live\/s_/);
  const evs = page.getByTestId("live-event");
  await expect(evs).toHaveCount(3);
  expect(await seqs(page)).toEqual([1, 2, 3]);
  expect(await evs.evaluateAll((es) => es.map((e) => e.getAttribute("data-kind")))).toEqual(["tool_start", "tool_end", "message"]);
  await expect(page.getByTestId("live-status")).toHaveText("running");

  // a reload mid-run: the same events once each
  await page.reload();
  await expect(evs).toHaveCount(3);
  expect(await seqs(page)).toEqual([1, 2, 3]);

  // a message shows queued until the ledger says it was delivered
  await page.getByLabel("Message to the agent").fill("try induction on n");
  await page.getByRole("button", { name: "Send" }).click();
  await expect(page.getByTestId("queued")).toHaveText(/queued\s+try induction on n/);
  await expect(evs).toHaveCount(4);
  await expect(evs.nth(3)).toHaveAttribute("data-kind", "inbox");
  await expect(evs.nth(3)).toContainText("try induction on n");
  await expect(page.getByTestId("queued")).toHaveCount(0);

  await page.getByRole("button", { name: "Stop" }).click();
  await expect(page.getByTestId("live-status")).toHaveText("stopped", { timeout: 15000 });
  await expect(evs).toHaveCount(5);
  await expect(evs.nth(4)).toHaveAttribute("data-kind", "end");
  await expect(page.getByTestId("live-ended")).toContainText("stopped");
  expect(await seqs(page)).toEqual([1, 2, 3, 4, 5]);
  await expect(page.getByRole("button", { name: "Stop" })).toHaveCount(0);
});

test("a start that the server refuses says why", async ({ page }) => {
  await signIn(page);
  await page.getByLabel("Project directory").fill("relative/dir");
  await page.getByLabel("Command").fill("/prove x");
  await page.getByRole("button", { name: "Start" }).click();
  await expect(page.getByRole("alert")).toContainText("Could not start");
});
