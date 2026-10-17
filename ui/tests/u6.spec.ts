import { expect, test, type Page } from "@playwright/test";

// Retrieval and bench w over fisher-ui's fixtures (test/fixtures/bench/w):
// run 4 of each arm on Step/Latency.lean. A: no retrieval, no library (1
// of 3 closed). B: retrieval, --no-library (2 of 3 closed, each goal's
// facts, nothing offered). C: retrieval and the library (each goal's facts
// and lemmas offered and used, an index built and a library problem in its
// events). Also the search form's retrieval and library switches and the
// settings' plain variables (TERMINAL_SSH_HOST box.local in the fixture's
// .env).
const token = "0123456789abcdef0123456789abcdef";

async function signIn(page: Page) {
  await page.goto(`/?t=${token}`);
  await expect(page).toHaveURL(/\/sessions$/);
}

test("bench w: a row per file with closed, dollars and retrieval totals, and each goal's facts, offers and uses", async ({ page }) => {
  await signIn(page);
  await page.goto("/runs/w?a=A&b=B");
  const files = page.getByTestId("file-row");
  await expect(files).toHaveCount(2);
  await expect(page.locator("[data-testid=file-row][data-arm=A]")).toContainText("1/3");
  await expect(page.locator("[data-testid=file-row][data-arm=A]")).toContainText("$2.000");
  const b = page.locator("[data-testid=file-row][data-arm=B]");
  await expect(b).toContainText("2/3");
  await expect(b).toContainText("$0.750");
  await expect(b).toContainText("13.3");
  const goalA = page.locator("[data-testid=bench-goals] tr[data-goal='Step.Latency.a']");
  await expect(goalA.getByTestId("retrieval")).toHaveText(/facts 20, offered 0, used 0/);
  await page.goto("/runs/w?a=B&b=C");
  await expect(page.locator("[data-testid=bench-goals] tr[data-goal='Step.Latency.a']").getByTestId("retrieval").last()).toHaveText(/facts 20, offered 2, used 1/);
  // no gate script for w in the fixtures
  await expect(page.getByTestId("gate")).toHaveCount(0);
});

test("the board of a run with retrieval: facts and offered lemmas on each goal, the index and a library problem on the run", async ({ page }) => {
  await signIn(page);
  await page.goto("/search/w.C.4");
  await expect(page.getByTestId("run-status")).toHaveText("done");
  const a = page.locator("[data-testid=goal-row][data-goal='Step.Latency.a']");
  await expect(a.locator("[data-testid=mark][data-kind=facts]")).toContainText("20");
  await expect(a.locator("[data-testid=mark][data-kind='library offered']")).toContainText("Step.Topology.lemma1");
  await expect(a.locator("[data-testid=mark][data-kind='library used']")).toContainText("Step.Topology.lemma1");
  const c = page.locator("[data-testid=goal-row][data-goal='Step.Latency.c']");
  await expect(c.locator("[data-testid=mark][data-kind=facts]")).toContainText("8");
  const run = page.getByTestId("run-retrieval");
  await expect(run).toContainText("index: 812 declarations in 40 files");
  await expect(run).toContainText("facts per goal 13.3");
  await expect(run).toContainText("library problem: the library is read-only");
  // B ran with --no-library: facts, no library marks, and the board says so
  await page.goto("/search/w.B.4");
  await expect(page.getByTestId("run-status")).toHaveText("done");
  const b = page.locator("[data-testid=goal-row][data-goal='Step.Latency.a']");
  await expect(b.locator("[data-testid=mark][data-kind=facts]")).toContainText("20");
  await expect(b.locator("[data-testid=mark][data-kind='library offered']")).toHaveCount(0);
  await expect(page.getByTestId("no-library")).toBeVisible();
  await expect(page.getByTestId("no-retrieval")).toHaveCount(0);
});

test("starting a search with retrieval and the library off sends both switches", async ({ page }) => {
  await signIn(page);
  await page.goto("/search");
  const form = page.getByRole("form", { name: "Start a search" });
  await form.getByLabel("Project directory").fill("/tmp/no-such-project-for-the-test");
  await form.getByLabel("Lean file (in the project)").fill("Demo/Flat.lean");
  await form.getByLabel("Retrieval").uncheck();
  await form.getByLabel("Library").uncheck();
  const sent = page.waitForRequest((r) => r.url().endsWith("/api/search") && r.method() === "POST");
  await form.getByRole("button", { name: "Start search" }).click();
  const body = JSON.parse((await sent).postData() ?? "{}") as Record<string, unknown>;
  expect(body.retrieval).toBe(false);
  expect(body.library).toBe(false);
  expect(body.auto).toBe(true);
});

test("settings show a .env variable that holds no secret as a plain field with its value", async ({ page }) => {
  await signIn(page);
  await page.goto("/settings");
  const plain = page.getByTestId("plain");
  await expect(plain.getByLabel("TERMINAL_SSH_HOST")).toHaveValue("box.local");
  await expect(plain.getByLabel("TERMINAL_SSH_USER")).toHaveValue("");
  await expect(page.getByTestId("key-TERMINAL_SSH_HOST")).toHaveCount(0);
});
