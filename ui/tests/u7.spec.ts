import { expect, test, type Page } from "@playwright/test";

// The New project view against the stub's scripted create routes: the
// templates lean, lean-mathlib (built-in) and custom (configured, not
// trusted yet); a create's events are two steps (fetch, build), the check
// and ready; a folder with "fail" in its path fails its build once.
const token = "0123456789abcdef0123456789abcdef";

async function signIn(page: Page) {
  await page.goto(`/?t=${token}`);
  await expect(page).toHaveURL(/\/sessions$/);
}

async function openNew(page: Page) {
  await signIn(page);
  await page.getByTestId("new-project").click();
  await expect(page).toHaveURL(/\/new$/);
  await expect(page.getByLabel("Template")).toBeVisible();
}

test("lean in a folder: the pattern error before submit, two steps streamed, then Ready with Start a run filled in (no search: the sample has no sorry)", async ({ page }) => {
  await openNew(page);
  await page.getByLabel("Template").selectOption("lean");
  await expect(page.getByTestId("template-about")).toContainText("lake build");
  // a relative folder is refused before anything is sent
  await page.getByLabel("Folder", { exact: true }).fill("proofs/Demo");
  await expect(page.getByTestId("folder-error")).toContainText("absolute");
  await expect(page.getByRole("button", { name: "Create" })).toBeDisabled();
  await page.getByLabel("Folder", { exact: true }).fill("/tmp/proofs/Demo");
  await expect(page.getByTestId("folder-error")).toHaveCount(0);
  // the project variable's placeholder is the folder's name
  await expect(page.getByLabel("Variable project")).toHaveAttribute("placeholder", "Demo");
  // a value that does not match the variable's pattern, as it is typed
  await page.getByLabel("Variable lean").fill("v5");
  await expect(page.getByTestId("var-error-lean")).toContainText("does not match");
  await expect(page.getByRole("button", { name: "Create" })).toBeDisabled();
  await page.getByLabel("Variable lean").fill("v4.9.0");
  await expect(page.getByTestId("var-error-lean")).toHaveCount(0);
  await page.getByRole("button", { name: "Create" }).click();

  await expect(page).toHaveURL(/\/new\/c\./);
  const steps = page.getByTestId("step");
  await expect(steps.first()).toHaveAttribute("data-step", "fetch");
  await expect(page.getByTestId("ready")).toBeVisible({ timeout: 10000 });
  await expect(steps).toHaveCount(2);
  await expect(page.locator('[data-testid=step][data-step=fetch]')).toHaveAttribute("data-state", "done");
  await expect(page.locator('[data-testid=step][data-step=build]')).toHaveAttribute("data-state", "done");
  await expect(page.getByTestId("check")).toContainText("it builds");
  await expect(page.getByTestId("create-status")).toHaveText("ready");
  await expect(page.getByTestId("ready")).toContainText("/tmp/proofs/Demo");
  await expect(page.getByTestId("ready")).toContainText("Demo/Sample.lean");

  // Start a run: the Sessions form with the project (and the sample) filled in
  await page.getByTestId("start-run").click();
  await expect(page).toHaveURL(/\/sessions\?start=/);
  const run = page.getByRole("form", { name: "Start a run" });
  await expect(run.getByLabel("Project directory")).toHaveValue("/tmp/proofs/Demo");
  await expect(run.getByLabel("Command")).toHaveValue("/prove Demo/Sample.lean");
  await page.goBack();
  await expect(page.getByTestId("ready")).toBeVisible();

  // no search on the sample, which is proved already: a hint instead
  await expect(page.getByTestId("start-search")).toHaveCount(0);
  await expect(page.getByTestId("search-hint")).toContainText("add a theorem with sorry, then start a search");
});

test("a custom template not trusted yet shows its steps and runs only once confirmed", async ({ page }) => {
  await openNew(page);
  await page.getByLabel("Template").selectOption("custom");
  await expect(page.getByTestId("untrusted")).toContainText("not trusted yet");
  await expect(page.getByTestId("template-about")).toContainText("./setup.sh {{project}}");
  await page.getByLabel("Folder", { exact: true }).fill("/tmp/proofs/Custom");
  await page.getByRole("button", { name: "Create" }).click();
  // the server answers with the steps rendered; nothing runs yet
  const confirm = page.getByTestId("confirm-steps");
  await expect(confirm).toContainText("setup: ./setup.sh Custom");
  await expect(page).toHaveURL(/\/new$/);
  // a change to what renders the steps drops the confirm: they are shown again
  await page.getByLabel("Folder", { exact: true }).fill("/tmp/proofs/Other");
  await expect(confirm).toHaveCount(0);
  await expect(page.getByRole("button", { name: "Create" })).toBeVisible();
  await page.getByLabel("Folder", { exact: true }).fill("/tmp/proofs/Custom");
  await page.getByRole("button", { name: "Create" }).click();
  await expect(confirm).toContainText("setup: ./setup.sh Custom");
  await page.getByLabel("Variable project").fill("Renamed");
  await expect(confirm).toHaveCount(0);
  await page.getByRole("button", { name: "Create" }).click();
  await expect(confirm).toContainText("setup: ./setup.sh Renamed");
  await confirm.getByRole("button", { name: "Confirm and create" }).click();
  await expect(page).toHaveURL(/\/new\/c\./);
  await expect(page.getByTestId("ready")).toBeVisible({ timeout: 10000 });
});

test("a failed build shows its reason and the step's output; Retry resumes and ends Ready", async ({ page }) => {
  await openNew(page);
  await page.getByLabel("Template").selectOption("lean");
  await page.getByLabel("Folder", { exact: true }).fill("/tmp/proofs/will-fail");
  // will-fail is not a Lean name: the project variable's own pattern says so
  await expect(page.getByTestId("var-error-project")).toHaveCount(0);
  await page.getByLabel("Variable project").fill("will-fail");
  await expect(page.getByTestId("var-error-project")).toContainText("does not match");
  await page.getByLabel("Variable project").fill("WillFail");
  await page.getByRole("button", { name: "Create" }).click();
  await expect(page.getByTestId("create-failed")).toContainText("Step build failed (exit 1)", { timeout: 10000 });
  await expect(page.locator('[data-testid=step][data-step=build]')).toHaveAttribute("data-state", "failed");
  await expect(page.locator('[data-testid=step][data-step=build]')).toContainText("unknown package");
  await page.getByRole("button", { name: "Retry" }).click();
  await expect(page.getByTestId("ready")).toBeVisible({ timeout: 10000 });
  await expect(page.locator('[data-testid=step][data-step=build]')).toHaveAttribute("data-state", "done");
  await expect(page.locator('[data-testid=step][data-step=fetch]')).toHaveAttribute("data-state", "done");
});

test("scratch needs no folder: its file is Scratch.lean, ready for a search", async ({ page }) => {
  await openNew(page);
  await page.getByLabel("Template").selectOption("lean-mathlib");
  await page.getByLabel("Scratch (a throwaway copy of a base built once)").check();
  await expect(page.getByLabel("Folder", { exact: true })).toHaveCount(0);
  await expect(page.getByLabel("Variable project")).toHaveAttribute("placeholder", "Scratch");
  await page.getByLabel("Statement").fill("1 + 1 = 2");
  await page.getByRole("button", { name: "Create" }).click();
  await expect(page.getByTestId("ready")).toBeVisible({ timeout: 10000 });
  await expect(page.getByTestId("ready")).toContainText("/tmp/stub-scratch/20991231-000000-abcd");
  await page.getByTestId("start-search").click();
  const search = page.getByRole("form", { name: "Start a search" });
  await expect(search.getByLabel("Lean file (in the project)")).toHaveValue("Scratch.lean");
});

test("Stop ends a create as stopped and offers Retry", async ({ page }) => {
  await openNew(page);
  await page.getByLabel("Template").selectOption("lean");
  await page.getByLabel("Folder", { exact: true }).fill("/tmp/proofs/Stopped");
  await page.getByRole("button", { name: "Create" }).click();
  await expect(page).toHaveURL(/\/new\/c\./);
  await page.getByRole("button", { name: "Stop" }).click();
  await expect(page.getByTestId("create-failed")).toContainText("Stopped.");
  await expect(page.getByRole("button", { name: "Retry" })).toBeVisible();
});

test("Retry after the template changed since it was confirmed goes back to Confirm with the same request", async ({ page }) => {
  await openNew(page);
  await page.getByLabel("Template").selectOption("lean");
  await page.getByLabel("Folder", { exact: true }).fill("/tmp/proofs/changed");
  await page.getByLabel("Variable lean").fill("v4.9.0");
  await page.getByRole("button", { name: "Create" }).click();
  await expect(page).toHaveURL(/\/new\/c\./);
  await expect(page.getByTestId("create-failed")).toContainText("changed since it was confirmed", { timeout: 10000 });
  await page.getByRole("button", { name: "Retry" }).click();
  // back at the form, as it was sent, with the steps as they are now to confirm
  await expect(page).toHaveURL(/\/new$/);
  await expect(page.getByRole("alert")).toContainText("changed since it was confirmed");
  await expect(page.getByLabel("Template")).toHaveValue("lean");
  await expect(page.getByLabel("Folder", { exact: true })).toHaveValue("/tmp/proofs/changed");
  await expect(page.getByLabel("Variable lean")).toHaveValue("v4.9.0");
  const confirm = page.getByTestId("confirm-steps");
  await expect(confirm).toContainText("build: lake build");
  await confirm.getByRole("button", { name: "Confirm and create" }).click();
  await expect(page).toHaveURL(/\/new\/c\./);
  await expect(page.getByTestId("ready")).toBeVisible({ timeout: 10000 });
});
