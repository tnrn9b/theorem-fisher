import { expect, test, type Page } from "@playwright/test";

// fisher-ui/test/Fisher/UI/Fixture.hs: its fixed token and three sessions
const token = "0123456789abcdef0123456789abcdef";
const s1 = "20261001_100000_aaaaaa"; // Lean, CLM, $0.035
const s2 = "20261001_110000_bbbbbb"; // Rocq, ended by compression
const s3 = "20261001_113000_cccccc"; // s2's compression child

async function signIn(page: Page) {
  await page.goto(`/?t=${token}`);
  await expect(page).toHaveURL(/\/sessions$/);
}

function rowIds(page: Page) {
  return page.locator("[data-testid=sessions] tbody tr").evaluateAll((rs) => rs.map((r) => r.getAttribute("data-session")));
}

test("without the cookie the page says to open the printed URL", async ({ page }) => {
  await page.goto("/sessions");
  await expect(page.getByRole("alert")).toContainText("Open the URL that fisher ui printed");
});

test("Sessions lists the three fixture sessions and filters by prover", async ({ page }) => {
  await signIn(page);
  await expect(page.getByTestId("session-count")).toHaveText("3 sessions");
  expect(await rowIds(page)).toEqual([s3, s2, s1]);
  const lean = page.locator(`tr[data-session="${s1}"]`);
  await expect(lean).toContainText("Lean session");
  await expect(lean).toContainText("Lean");
  await expect(lean).toContainText("example/model-a");
  await expect(lean).toContainText("$0.035");
  await expect(lean).toContainText("completed");

  await page.getByLabel("Prover").selectOption({ label: "Rocq" });
  await expect(page).toHaveURL(/prover=rocq/);
  await expect(page.getByTestId("session-count")).toHaveText("2 sessions");
  expect(await rowIds(page)).toEqual([s3, s2]);

  await page.getByLabel("Prover").selectOption({ label: "Lean" });
  await expect(page.getByTestId("session-count")).toHaveText("1 session");
  expect(await rowIds(page)).toEqual([s1]);

  await page.getByLabel("Prover").selectOption({ label: "All" });
  await expect(page.getByTestId("session-count")).toHaveText("3 sessions");

  await page.getByRole("link", { name: "Lean session" }).click();
  await expect(page).toHaveURL(new RegExp(`/session/${s1}$`));
});

test("a session shows its messages in order, a tool call that collapses and expands, and the cost", async ({ page }) => {
  await signIn(page);
  await page.goto(`/session/${s1}`);
  await expect(page.getByRole("heading", { level: 1 })).toHaveText("Lean session");
  await expect(page.getByTestId("cost-dollars")).toHaveText("$0.035");
  await expect(page.getByTestId("cost-strip")).toContainText("300");

  // the tool result sits inside the call that asked for it
  const msgs = page.getByTestId("message");
  await expect(msgs).toHaveCount(3);
  expect(await msgs.evaluateAll((ms) => ms.map((m) => m.getAttribute("data-role")))).toEqual(["user", "assistant", "assistant"]);
  await expect(msgs.nth(0)).toContainText("prove t");
  await expect(msgs.nth(2)).toContainText("done");

  const call = msgs.nth(1).getByTestId("tool-call");
  const head = call.getByRole("button");
  await expect(head).toHaveAttribute("aria-expanded", "false");
  await expect(head).toContainText("read_file");
  await expect(head).toContainText("theorem t");
  await expect(call.getByTestId("tool-body")).toHaveCount(0);

  await head.click();
  await expect(head).toHaveAttribute("aria-expanded", "true");
  await expect(call.getByTestId("tool-body")).toContainText("Arguments");
  await expect(call.getByTestId("tool-body")).toContainText("theorem t");

  await head.click();
  await expect(head).toHaveAttribute("aria-expanded", "false");
  await expect(call.getByTestId("tool-body")).toHaveCount(0);

  await page.getByRole("link", { name: "Context" }).click();
  await expect(page).toHaveURL(new RegExp(`/context/${s1}$`));
});

test("a compressed session links its child, whose context shows the summary", async ({ page }) => {
  await signIn(page);
  await page.goto(`/session/${s2}`);
  await page.getByRole("link", { name: `continued in ${s3}` }).click();
  await expect(page).toHaveURL(new RegExp(`/session/${s3}$`));
  await page.goto(`/context/${s3}`);
  await expect(page.getByTestId("ctx-strategy")).toHaveText("strategy compress");
  await expect(page.getByTestId("ctx-summary")).toContainText("[CONTEXT COMPACTION]");
  await expect(page.getByTestId("ctx-summary")).toContainText("Proved two lemmas.");
});

test("Context shows the CLM file and its events", async ({ page }) => {
  await signIn(page);
  await page.goto(`/context/${s1}`);
  await expect(page.getByTestId("ctx-strategy")).toHaveText("strategy clm");
  await expect(page.getByTestId("ctx-now")).toHaveText("goal: t\nproved");
  await expect(page.getByTestId("ctx-prev")).toHaveText("goal: t");
  const rows = page.locator("[data-testid=ctx-events] tbody tr");
  await expect(rows).toHaveCount(3);
  expect(await rows.locator("td:nth-child(2)").allTextContents()).toEqual(["clm_write", "clm_spill", "clm_read"]);
  expect(await rows.locator("td:nth-child(1)").allTextContents()).toEqual(["1", "2", "3"]);
  await expect(rows.nth(1)).toContainText("chars: 900");
});

test("an unknown session says not found", async ({ page }) => {
  await signIn(page);
  await page.goto("/session/nope");
  await expect(page.getByRole("alert")).toContainText("Not found");
});

test("Runs lists the benches and Runs h shows both arms and the gate line", async ({ page }) => {
  await signIn(page);
  await page.goto("/runs");
  const benches = page.getByTestId("benches");
  await expect(benches).toContainText("flat");
  await expect(benches).toContainText("noauto");
  await page.getByRole("link", { name: "h", exact: true }).click();
  await expect(page).toHaveURL(/\/runs\/h$/);

  const heads = page.getByTestId("arm-head");
  await expect(heads).toHaveText(["auto", "noauto"]);
  const rows = page.locator("[data-testid=bench-goals] tbody tr");
  await expect(rows).toHaveCount(8);
  const first = page.locator('tr[data-goal="Step.Diamond.fireDiamond_of\'"]');
  // auto: $0.883, 19 turns, closed by the model; noauto: $0.898, 18 turns
  await expect(first).toContainText("$0.883");
  await expect(first).toContainText("$0.898");
  await expect(first).toContainText("model");
  await expect(page.getByTestId("totals")).toContainText("8/8");

  await expect(page.getByTestId("gate")).toHaveAttribute("data-status", "ok", { timeout: 20000 });
  await expect(page.getByTestId("gate")).toContainText("auto closes more, or as many for at least 25% fewer dollars: PASS");

  // swap the arms
  await page.getByLabel("Arm A").selectOption("noauto");
  await page.getByLabel("Arm B").selectOption("auto");
  await expect(heads).toHaveText(["noauto", "auto"]);

  // sort by arm A's dollars, cheapest first
  await page.locator("thead th").filter({ hasText: "Dollars" }).first().getByRole("button").click();
  await expect(rows.first()).toHaveAttribute("data-goal", "Step.SymExpr.evalPure_typed");
});

test("a session with no priced responses reads $0.000", async ({ page }) => {
  await signIn(page);
  await page.goto(`/session/${s3}`);
  await expect(page.getByTestId("cost-dollars")).toHaveText("$0.000");
  await expect(page.getByTestId("cost-strip")).toContainText("Turns1");
  await expect(page.getByTestId("message")).toHaveCount(2);
});

// The states below are the stub's scenarios (tests/stub-server.mjs), chosen
// with its fisher_stub cookie; the real fixture server has none of them.
async function scenario(page: Page, name: string) {
  test.skip(!!process.env.FISHER_UI_BASE, "a stub scenario");
  await signIn(page);
  const url = new URL(page.url());
  await page.context().addCookies([{ name: "fisher_stub", value: name, domain: url.hostname, path: "/" }]);
}

test("with no sessions at all the list says so, not that none match", async ({ page }) => {
  await scenario(page, "empty");
  await page.goto("/sessions");
  await expect(page.getByTestId("no-sessions")).toHaveText("No sessions yet.");
  await expect(page.getByTestId("sessions")).toHaveCount(0);
  await expect(page.getByText("No sessions match.")).toHaveCount(0);
});

test("a bench whose report.py failed says so in the gate line", async ({ page }) => {
  await scenario(page, "gate-failed");
  await page.goto("/runs/h");
  await expect(page.getByTestId("gate")).toHaveAttribute("data-status", "failed");
  await expect(page.getByTestId("gate")).toContainText("report.py failed or timed out");
  await expect(page.locator("[data-testid=bench-goals] tbody tr")).toHaveCount(8);
});

test("a failed poll stops the polling and shows its error", async ({ page }) => {
  await scenario(page, "poll-fails");
  let polls = 0;
  page.on("request", (r) => {
    if (r.url().endsWith("/api/runs/h")) polls += 1;
  });
  await page.goto("/runs/h");
  await expect(page.getByTestId("gate")).toHaveAttribute("data-status", "pending");
  await expect(page.getByRole("alert")).toContainText("Could not load", { timeout: 10000 });
  await expect(page.locator("[data-testid=bench-goals] tbody tr")).toHaveCount(8);
  const seen = polls;
  await page.waitForTimeout(4500);
  expect(polls).toBe(seen);
});

test("a query naming one arm twice shows it once beside the other", async ({ page }) => {
  await signIn(page);
  await page.goto("/runs/h?a=noauto&b=noauto");
  await expect(page.getByTestId("arm-head")).toHaveText(["noauto", "auto"]);
});
