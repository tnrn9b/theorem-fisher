import { expect, test, type Page } from "@playwright/test";

// The Settings page's Claude Code panel. The stub (and the fixture server's
// fake claude) start logged out; a login prints a line and a URL and waits
// at a prompt for a code, which logs in as x@y.z (max). The tests run in
// order on one server: each ends logged out.
const token = "0123456789abcdef0123456789abcdef";

async function signIn(page: Page) {
  await page.goto(`/?t=${token}`);
  await expect(page).toHaveURL(/\/sessions$/);
}

test.describe.configure({ mode: "serial" });

test("log in with a subscription: the URL as a link, the code sent, the status after, then log out", async ({ page }) => {
  await signIn(page);
  await page.goto("/settings");
  const panel = page.getByTestId("claude-panel");
  await expect(panel.getByRole("heading", { name: "Claude Code" })).toBeVisible();
  await expect(page.getByTestId("claude-status")).toHaveText("Not logged in");
  // every control has a label
  await expect(panel.getByLabel("Subscription (Claude Pro or Max)")).toBeChecked();
  await expect(panel.getByLabel("Anthropic Console (API billing)")).not.toBeChecked();
  await expect(panel.getByLabel("Force SSO")).not.toBeChecked();
  await expect(panel.getByLabel("Email (optional)")).toHaveValue("");

  const sent = page.waitForRequest((r) => r.url().endsWith("/api/claude-auth/login") && r.method() === "POST");
  await panel.getByRole("button", { name: "Log in" }).click();
  expect((await sent).postDataJSON()).toEqual({ console: false, sso: false });

  const output = page.getByTestId("claude-output");
  await expect(output).toContainText("Opening your browser to log in.");
  await expect(output).toContainText("Paste code here if prompted >");
  const link = output.getByRole("link", { name: /^https:\/\/claude\.ai\/oauth\/authorize/ });
  await expect(link).toHaveAttribute("target", "_blank");
  // while it runs: the code box and Stop, not the login form
  await expect(panel.getByRole("button", { name: "Log in" })).toHaveCount(0);
  const code = panel.getByLabel("Code from the browser (if the login asks for one)");
  await expect(panel.getByRole("button", { name: "Send code" })).toBeDisabled();
  await code.fill("code-123#state");
  const input = page.waitForRequest((r) => r.url().includes("/input") && r.method() === "POST");
  await panel.getByRole("button", { name: "Send code" }).click();
  expect((await input).postDataJSON()).toEqual({ text: "code-123#state" });

  await expect(page.getByTestId("claude-note")).toHaveText("Logged in to Claude Code.");
  await expect(page.getByTestId("claude-status")).toHaveText("Logged in as x@y.z (max)");
  await expect(output).toContainText("Login successful.");
  await expect(code).toHaveCount(0);

  await panel.getByRole("button", { name: "Log out" }).click();
  await expect(page.getByTestId("claude-note")).toHaveText("Logged out of Claude Code.");
  await expect(page.getByTestId("claude-status")).toHaveText("Not logged in");
});

test("a Console login with SSO and an email sends them; Stop ends it", async ({ page }) => {
  await signIn(page);
  await page.goto("/settings");
  const panel = page.getByTestId("claude-panel");
  await expect(page.getByTestId("claude-status")).toHaveText("Not logged in");
  await panel.getByLabel("Anthropic Console (API billing)").check();
  await panel.getByLabel("Force SSO").check();
  await panel.getByLabel("Email (optional)").fill(" a@b.c ");
  const sent = page.waitForRequest((r) => r.url().endsWith("/api/claude-auth/login") && r.method() === "POST");
  await panel.getByRole("button", { name: "Log in" }).click();
  expect((await sent).postDataJSON()).toEqual({ console: true, sso: true, email: "a@b.c" });
  await expect(page.getByTestId("claude-output")).toContainText("Paste code here if prompted >");
  await panel.getByRole("button", { name: "Stop" }).click();
  await expect(page.getByTestId("claude-note")).toHaveText("The login was stopped.");
  await expect(page.getByTestId("claude-status")).toHaveText("Not logged in");
  await expect(panel.getByRole("button", { name: "Log in" })).toBeVisible();
});

test("a login going when the page is reloaded is followed again", async ({ page }) => {
  await signIn(page);
  await page.goto("/settings");
  const panel = page.getByTestId("claude-panel");
  await panel.getByRole("button", { name: "Log in" }).click();
  await expect(page.getByTestId("claude-output")).toContainText("Paste code here if prompted >");
  await page.reload();
  await expect(page.getByTestId("claude-output")).toContainText("https://claude.ai/oauth/authorize");
  await panel.getByRole("button", { name: "Stop" }).click();
  await expect(page.getByTestId("claude-note")).toHaveText("The login was stopped.");
});

test("Log in while a login started elsewhere is going follows that one", async ({ page, baseURL }) => {
  await signIn(page);
  await page.goto("/settings");
  const panel = page.getByTestId("claude-panel");
  await expect(page.getByTestId("claude-status")).toHaveText("Not logged in");
  // another tab starts one
  const origin = new URL(baseURL ?? page.url()).origin;
  const other = await page.request.post("/api/claude-auth/login", { data: {}, headers: { Origin: origin } });
  expect(other.status()).toBe(200);
  await panel.getByRole("button", { name: "Log in" }).click();
  await expect(page.getByTestId("claude-note")).toHaveText("A login was already going; this is its output.");
  await expect(page.getByTestId("claude-output")).toContainText("https://claude.ai/oauth/authorize");
  await expect(page.getByRole("alert")).toHaveCount(0);
  await panel.getByRole("button", { name: "Stop" }).click();
  await expect(page.getByTestId("claude-note")).toHaveText("The login was stopped.");
});

test("without claude it says how to install it, and offers no login", async ({ page }) => {
  test.skip(!!process.env.FISHER_UI_BASE, "a stub scenario");
  await signIn(page);
  const url = new URL(page.url());
  await page.context().addCookies([{ name: "fisher_stub", value: "claude-missing", domain: url.hostname, path: "/" }]);
  await page.goto("/settings");
  await expect(page.getByTestId("claude-status")).toHaveText(
    "The claude CLI is not installed. Install it with: npm install -g @anthropic-ai/claude-code",
  );
  await expect(page.getByTestId("claude-panel").getByRole("button", { name: "Log in" })).toHaveCount(0);
});
