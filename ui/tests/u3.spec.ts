import { expect, test, type Page } from "@playwright/test";

// The Lean session's step log, as the stub and fisher-ui's fixture
// (fisher-ui/test/Fisher/UI/Fixture.hs leanSteps) both have it: three
// changes to Main.lean. A check counts sorry as a hole and BAD as an error.
const token = "0123456789abcdef0123456789abcdef";
const lean = "20261001_100000_aaaaaa";
const texts = [
  "theorem t : True := by\n  sorry\n",
  "theorem t : True := by\n  skip\n  sorry\n",
  "theorem t : True := by\n  skip\n  trivial\n",
];

async function signIn(page: Page) {
  await page.goto(`/?t=${token}`);
  await expect(page).toHaveURL(/\/sessions$/);
}

test("scrub the timeline, check a step, diff steps 1 and 3, edit in the canvas and hand back", async ({ page }) => {
  await signIn(page);
  await page.goto(`/session/${lean}`);
  const ticks = page.getByTestId("tick");
  await expect(ticks).toHaveCount(3);
  expect(await ticks.evaluateAll((es) => es.map((e) => e.getAttribute("data-tool")))).toEqual(["write_file", "patch", "patch"]);
  const editor = page.getByTestId("editor");
  // the last step first
  await expect(page.getByTestId("step-seq")).toHaveText("3");
  await expect(editor).toHaveValue(texts[2]);

  // scrub: Home to the first step, then one to the right
  const slider = page.getByRole("slider", { name: "Step" });
  await slider.focus();
  await page.keyboard.press("Home");
  await expect(page.getByTestId("step-seq")).toHaveText("1");
  await expect(editor).toHaveValue(texts[0]);
  await page.keyboard.press("ArrowRight");
  await expect(page.getByTestId("step-seq")).toHaveText("2");
  await expect(editor).toHaveValue(texts[1]);
  // a tick jumps
  await ticks.nth(0).click();
  await expect(editor).toHaveValue(texts[0]);

  // the check, on demand
  await expect(page.getByTestId("verdict")).toHaveText("check: unchecked");
  await page.getByRole("button", { name: "Check this step" }).click();
  await expect(page.getByTestId("verdict")).toHaveText("check: 1 hole");
  await expect(ticks.nth(0).getByTestId("tick-verdict")).toHaveText("1 hole");

  // diff steps 1 and 3: the changed lines
  await page.getByLabel("From step").selectOption("1");
  await page.getByLabel("To step").selectOption("3");
  await page.getByRole("button", { name: "Show the diff" }).click();
  const lines = page.getByTestId("diff-line");
  await expect(page.getByTestId("diff")).toBeVisible();
  const ops = await lines.evaluateAll((es) => es.map((e) => `${e.getAttribute("data-op")} ${e.textContent}`));
  expect(ops).toEqual(["same  theorem t : True := by", "del -  sorry", "add +  skip", "add +  trivial"]);

  // a hand edit from an older step: the file has moved on since, refused
  await ticks.nth(0).click();
  await editor.fill("theorem t : True := by\n  exact trivial\n");
  await page.getByRole("button", { name: "Hand back" }).click();
  await expect(page.getByRole("alert")).toContainText("The file changed on disk since this step");
  await page.getByRole("button", { name: "Discard the edit" }).click();
  await expect(ticks).toHaveCount(3);

  // a hand edit on the last step, handed back
  await ticks.nth(2).click();
  await expect(editor).toHaveValue(texts[2]);
  const edited = "theorem t : True := by\n  trivial\n";
  await editor.fill(edited);
  await page.getByRole("button", { name: "Hand back" }).click();
  await expect(page.getByTestId("handed")).toContainText("Handed back as step 4 (check: ok)", { timeout: 15000 });
  await expect(page.getByTestId("handed")).toContainText("resumed with the change");
  await expect(ticks).toHaveCount(4);
  await expect(ticks.nth(3)).toHaveAttribute("data-tool", "hand");
  await expect(ticks.nth(3).getByTestId("tick-verdict")).toHaveText("ok");
  await expect(page.getByTestId("step-seq")).toHaveText("4");
  await expect(editor).toHaveValue(edited);
  await expect(page.getByRole("button", { name: "Hand back" })).toHaveCount(0);
});

test("a session with no step log shows its transcript and no canvas", async ({ page }) => {
  await signIn(page);
  await page.goto("/session/20261001_110000_bbbbbb");
  await expect(page.getByTestId("transcript")).toBeVisible();
  await expect(page.getByTestId("canvas")).toHaveCount(0);
});
