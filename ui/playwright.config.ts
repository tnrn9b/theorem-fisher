import { existsSync } from "node:fs";
import { defineConfig } from "@playwright/test";

// Against the stub (tests/stub-server.mjs, the recorded fixture answers) by
// default; FISHER_UI_BASE=http://127.0.0.1:PORT runs against a running
// fisher-ui-fixture-server instead. The cloud's bundled Chromium is used
// when it is there; FISHER_UI_BROWSER names another Chromium or Chrome
// binary (the Mac has no Chromium of the pinned Playwright's revision).
const external = process.env.FISHER_UI_BASE;
const port = 8733;
const bundled = process.env.FISHER_UI_BROWSER ?? "/opt/pw-browsers/chromium";

export default defineConfig({
  testDir: "tests",
  timeout: 30000,
  retries: 0,
  workers: 1,
  reporter: "list",
  use: {
    baseURL: external ?? `http://127.0.0.1:${port}`,
    launchOptions: existsSync(bundled) ? { executablePath: bundled } : {},
  },
  webServer: external
    ? undefined
    : { command: `node tests/stub-server.mjs ${port}`, url: `http://127.0.0.1:${port}/sessions`, reuseExistingServer: false },
});
