import { expect, test, type Page } from "@playwright/test";

// Settings against the stub's in-memory settings or the fixture server's
// scratch FISHER_HOME (config.yaml: model example/model-a, provider
// openrouter, context clm, autoformalize model example/model-b; .env:
// OPENROUTER_API_KEY ending in 1234).
const token = "0123456789abcdef0123456789abcdef";
const newKey = "sk-page-test-00000000000000004321";

async function signIn(page: Page) {
  await page.goto(`/?t=${token}`);
  await expect(page).toHaveURL(/\/sessions$/);
}

test("change the model and the context strategy and see them persisted; a key shows only its last 4", async ({ page }) => {
  await signIn(page);
  await page.getByRole("link", { name: "Settings" }).click();
  await expect(page).toHaveURL(/\/settings$/);

  const model = page.getByLabel("Model", { exact: true });
  await expect(model).toHaveValue("example/model-a");
  await expect(page.getByLabel("Provider")).toHaveValue("openrouter");
  await expect(page.getByLabel("Autoformalize model")).toHaveValue("example/model-b");
  await expect(page.getByLabel("Context strategy")).toHaveValue("clm");
  await expect(page.getByTestId("key-OPENROUTER_API_KEY")).toContainText("1234");
  // the pre-pass defaults are shown, not editable
  await expect(page.getByTestId("auto")).toContainText("60");
  await expect(page.getByTestId("auto")).toContainText("omega");

  await model.fill("example/model-c");
  await page.getByLabel("Context strategy").selectOption("rlm");
  await page.getByRole("button", { name: "Save" }).click();
  await expect(page.getByRole("status")).toHaveText("Saved.");

  await page.reload();
  await expect(model).toHaveValue("example/model-c");
  await expect(page.getByLabel("Context strategy")).toHaveValue("rlm");

  // a new key: the field clears and the row shows its last 4 only
  await page.getByLabel("New value for OPENROUTER_API_KEY").fill(newKey);
  await page.getByRole("button", { name: "Save" }).click();
  await expect(page.getByRole("status")).toHaveText("Saved.");
  await expect(page.getByTestId("key-OPENROUTER_API_KEY")).toContainText("4321");
  await expect(page.getByLabel("New value for OPENROUTER_API_KEY")).toHaveValue("");
  await page.reload();
  await expect(page.getByTestId("key-OPENROUTER_API_KEY")).toContainText("4321");
  expect(await page.content()).not.toContain("sk-page-test");

  // put the fixture back for later runs against the same server
  await model.fill("example/model-a");
  await page.getByLabel("Context strategy").selectOption("clm");
  await page.getByLabel("New value for OPENROUTER_API_KEY").fill("sk-fixture-0000000000001234");
  await page.getByRole("button", { name: "Save" }).click();
  await expect(page.getByRole("status")).toHaveText("Saved.");
  await expect(page.getByTestId("key-OPENROUTER_API_KEY")).toContainText("1234");
});

test("a save the server refuses says why", async ({ page }) => {
  await signIn(page);
  await page.goto("/settings");
  await page.getByLabel("Model", { exact: true }).fill("   ");
  await page.getByRole("button", { name: "Save" }).click();
  await expect(page.getByRole("alert")).toContainText("Could not save");
});
