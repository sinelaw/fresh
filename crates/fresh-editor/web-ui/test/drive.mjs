// Headless suite for the web client (run.sh starts the host). Each case
// starts from a fresh editor (POST /reset) and a fresh page, drives the
// client the way a person would, and checks what the page shows.
import { chromium } from "playwright";
import assert from "node:assert/strict";
import { readFileSync, writeFileSync } from "node:fs";

const URL = process.env.UI_URL || "http://127.0.0.1:8141";
const PROJECT = process.env.PROJECT || "";
const SKINS = process.env.SKINS || "";

const browser = await chromium.launch(process.env.CHROMIUM ? { executablePath: process.env.CHROMIUM } : {});
let failures = 0;

async function page() {
  await fetch(URL + "/reset", { method: "POST" });
  const p = await browser.newPage({ viewport: { width: 1280, height: 800 } });
  p.errors = [];
  p.on("pageerror", (e) => p.errors.push(e.message));
  p.on("console", (m) => { if (m.type() === "error") p.errors.push(m.text()); });
  await p.goto(URL);
  await p.waitForFunction(() => typeof views !== "undefined" && [...views.values()].some((v) => v.data));
  return p;
}

/** The active view's rows as text, from the rows the core sent. */
const textOf = (p) => p.evaluate(() => {
  const v = activeView();
  return v && v.data ? v.data.rows.map((row) => rowText(row)) : [];
});

async function until(f, what, ms = 4000) {
  const end = Date.now() + ms;
  let last;
  while (Date.now() < end) {
    try { last = await f(); if (last) return last; } catch (e) { last = e; }
    await new Promise((r) => setTimeout(r, 50));
  }
  throw new Error(`timed out: ${what} (last: ${last})`);
}

async function test(name, f) {
  // ONLY=<text> runs the cases whose name contains it.
  if (process.env.ONLY && !name.includes(process.env.ONLY)) return;
  const p = await page();
  try {
    await f(p);
    assert.deepEqual(p.errors, [], "no page errors");
    console.log(`ok   ${name}`);
  } catch (e) {
    failures++;
    console.log(`FAIL ${name}\n     ${e.message.split("\n").join("\n     ")}`);
    await p.screenshot({ path: `fail-${name.replace(/\W+/g, "_")}.png` }).catch(() => {});
  } finally {
    await p.close();
  }
}

await test("the file's text is drawn from the core's lines", async (p) => {
  const lines = await textOf(p);
  assert.ok(lines.some((l) => l.includes("fn main()")), lines.join("\n"));
  assert.ok(await p.$(".tab.active"), "a tab for it");
});

await test("typing edits the buffer and marks it modified", async (p) => {
  await p.click(".view .lines", { position: { x: 400, y: 4 } });
  await p.keyboard.press("End");
  await p.keyboard.type(" // web");
  await until(async () => (await textOf(p)).some((l) => l.includes("{ // web")), "typed text");
  await until(() => p.$eval(".tab.active .tab-close", (e) => e.textContent === "●"), "modified mark");
});

await test("the menus are the core's and run its actions", async (p) => {
  const labels = await p.$$eval("#menubar .mtop", (b) => b.map((x) => x.textContent));
  for (const l of ["File", "Edit", "View"]) assert.ok(labels.includes(l), labels.join(","));
  await p.click("#menubar .mtop:text('View')");
  await p.click(".dropdown .mitem:has-text('Split Vertical')");
  await until(() => p.$$eval(".pane", (ps) => ps.length === 2), "two panes");

  // A menu taller than the room below its title scrolls; it never moves over
  // the title, where the press's release would run an item.
  await p.setViewportSize({ width: 1280, height: 420 });
  await p.click("#menubar .mtop:text('View')");
  const dd = await p.$eval(".dropdown", (d) => ({ top: d.getBoundingClientRect().top, scrolls: d.scrollHeight > d.clientHeight }));
  const title = await p.locator("#menubar .mtop:text('View')").boundingBox();
  assert.ok(dd.top >= title.y + title.height - 1, `the menu hangs below its title (${dd.top})`);
  assert.ok(dd.scrolls, "and scrolls");
  assert.equal(await p.$$eval(".pane", (ps) => ps.length), 2, "no item ran");
});

await test("the palette is the core's prompt, drawn by the client", async (p) => {
  await p.click(".view .lines");
  await p.keyboard.press("Control+p");
  await p.waitForSelector(".ask.palette .ask-input");
  // The palette opens on ">", the core's prefix for commands.
  await until(() => p.$eval(".ask-input", (i) => i.value === ">"), "the command prefix");
  await p.keyboard.type("split horiz");
  await until(() => p.$eval(".ask-row.selected .ask-text", (e) => e.textContent.includes("Split Horizontal")), "suggestion");
  await p.keyboard.press("Enter");
  await until(() => p.$$eval(".split.stack", (s) => s.length === 1), "stacked panes");
  assert.equal(await p.$(".ask.palette"), null, "the palette closed");
  // Like every modal it dims the window, and a press outside it cancels it.
  await p.keyboard.press("Control+p");
  await p.waitForSelector(".scrim > .ask.palette");
  const dim = await p.$eval(".scrim", (s) => getComputedStyle(s).backgroundColor);
  assert.match(dim, /rgba\(0, 0, 0, 0\.[2-9]/, `dimmed (${dim})`);
  await p.mouse.click(20, 780);
  await until(async () => (await p.$(".ask.palette")) === null, "cancelled by a press outside");
});

await test("each pane wraps at its own width", async (p) => {
  await p.click(".view .lines", { position: { x: 400, y: 4 } });
  await p.keyboard.press("End");
  await p.keyboard.type(" " + "word ".repeat(40));
  await p.click("#menubar .mtop:text('View')");
  await p.click(".dropdown .mitem:has-text('Split Vertical')");
  await until(() => p.$$eval(".pane", (ps) => ps.length === 2), "two panes");
  // Each view drawn at the size it asked for.
  await until(() => p.evaluate(() => [...views.values()].every((v) => v.data && v.size && v.data.width === v.size[0])),
    "both views drawn at their sizes");
  const panes = await p.evaluate(() => [...views.values()].map((v) => ({
    drawn: v.lines.getBoundingClientRect().width,
    box: v.root.getBoundingClientRect().width,
    wrapped: v.data.rows.filter((row) => rowText(row).includes("word")).length,
  })));
  panes.forEach((v, i) => {
    assert.ok(v.drawn <= v.box + 1, `pane ${i} text fits: ${v.drawn} <= ${v.box}`);
    assert.ok(v.wrapped >= 2, `pane ${i} wraps the long line: ${v.wrapped} rows`);
  });
});

await test("the explorer lists the project and opens a file", async (p) => {
  await p.click(".ex-row:has-text('notes.md')");
  await until(() => p.$eval(".tab.active .tab-name", (e) => e.textContent === "notes.md"), "notes.md tab");
  await until(async () => (await textOf(p)).some((l) => l.includes("# Notes")), "its text");
  await p.click(".ex-row:has-text('src')");
  await p.waitForSelector(".ex-row:has-text('lib.rs')");
});

await test("the status bar shows the core's facts", async (p) => {
  await until(() => p.$eval("#statusbar", (s) => /Ln \d+, Col \d+/.test(s.textContent)), "cursor position");
  const text = await p.$eval("#statusbar", (s) => s.textContent);
  assert.ok(text.includes("Rust"), text);
  // The palette's key, as the terminal hints it; pressing it opens the palette.
  assert.match(await p.$eval(".spalette", (e) => e.textContent), /Palette .+/);
  await p.click(".spalette");
  await p.waitForSelector(".ask.palette");
});

await test("settings are read from and written to the core", async (p) => {
  await p.click(".view .lines");
  await p.keyboard.press("Control+Comma");
  await p.waitForSelector(".settings");
  // The theme is chosen from the installed themes, as Select Theme offers them.
  const themes = await p.locator(".setting", { has: p.locator(".setting-name", { hasText: /^Theme$/ }) })
    .locator("select option").allTextContents();
  assert.ok(themes.length >= 3 && themes.some((t) => /dark/i.test(t)) && themes.some((t) => /light/i.test(t)), themes.join(","));
  await p.click(".settings-cat:text('Editor')");
  const row = p.locator(".setting", { has: p.locator(".setting-name", { hasText: /^Line numbers$/ }) });
  const box = row.locator("input[type=checkbox]");
  assert.equal(await box.isChecked(), true);
  await box.uncheck();
  await p.click(".settings .btn:text('Done')");
  // A new buffer takes the default: no gutter numbers.
  await p.keyboard.press("Control+n");
  await until(async () => {
    const lines = await textOf(p);
    return lines.length > 0 && !/^\s*1\s/.test(lines[0]);
  }, "no line number in a new buffer");
  // Settings persist; put this one back for the cases after.
  await p.keyboard.press("Control+Comma");
  await p.click(".settings-cat:text('Editor')");
  await row.locator("input[type=checkbox]").check();
  await p.click(".settings .btn:text('Done')");
});

await test("a confirmation is the core's question, answered in the client", async (p) => {
  await p.click(".view .lines");
  await p.keyboard.type("x");
  await until(() => p.$eval(".tab.active .tab-close", (e) => e.textContent === "●"), "modified");
  await p.click(".tab.active .tab-close");
  await p.waitForSelector(".ask.dialog .btn");
  const buttons = await p.$$eval(".ask.dialog .btn", (b) => b.map((x) => x.textContent));
  assert.ok(buttons.length >= 2, buttons.join(","));
  await p.keyboard.press("Escape");
  await until(async () => !(await p.$(".ask.dialog")), "dismissed");
});

await test("open file browses the core's filesystem", async (p) => {
  await p.click(".view .lines");
  await p.keyboard.press("Control+o");
  await p.waitForSelector(".browser .ask-row");
  await p.click(".browser .ask-row:has-text('notes.md')");
  await p.click(".browser .ask-row.selected:has-text('notes.md')", { clickCount: 2 });
  await until(() => p.$eval(".tab.active .tab-name", (e) => e.textContent === "notes.md"), "notes.md opened");
});

await test("a shortcut added on the shortcuts screen works once saved", async (p) => {
  await p.keyboard.press("Control+Comma");
  await p.click(".settings-cat.link");
  await p.waitForSelector(".kb-table tbody tr");
  await p.click(".settings.keys .btn:text('Add…')");
  await p.focus(".kb-capture");
  await p.keyboard.press("F7");
  await until(() => p.$eval(".kb-capture", (e) => e.value === "F7"), "captured F7");
  await p.fill(".kb-action", "move_line_end");
  await p.click(".kb-edit .btn:text('Add')");
  await until(() => p.$eval(".settings.keys .btn:text('Save')", (b) => !b.disabled), "changes to save");
  await p.click(".settings.keys .btn:text('Save')");
  await until(async () => !(await p.$(".settings.keys")), "closed");
  await p.click(".view .lines", { position: { x: 300, y: 4 } });
  await p.keyboard.press("Home");
  await p.keyboard.press("F7");
  await p.keyboard.type("!");
  // Whatever an earlier case left on the line, the `!` lands at its end.
  await until(async () => (await textOf(p)).some((l) => l.trimEnd().endsWith("!")), "F7 moved to the line end");
});

await test("a terminal is drawn in its view at the view's size", async (p) => {
  await p.click(".view .lines");
  await p.evaluate(() => api.send("action", { name: "open_terminal" }));
  await until(() => p.evaluate(() => { const b = bufferById(core.status.buffer); return !!(b && b.terminal); }),
    "a terminal buffer in front");
  const cols = await p.evaluate(() => activeView().size[0]);
  await p.keyboard.type("echo $COLUMNS-cols");
  await p.keyboard.press("Enter");
  await until(async () => (await textOf(p)).some((l) => l.trim() === `${cols}-cols`), `the shell sees ${cols} columns`);
});

await test("a tab dragged to a pane's edge splits it, and the sash sets the core's ratio", async (p) => {
  await p.click(".ex-row:has-text('notes.md')");
  await until(() => p.$$eval(".tab", (t) => t.length >= 2), "two tabs");
  const tab = p.locator(".tab", { hasText: "notes.md" });
  const body = await p.locator(".pane-body").first().boundingBox();
  await tab.dragTo(p.locator(".pane-body").first(), { targetPosition: { x: body.width - 10, y: body.height / 2 } });
  await until(() => p.$$eval(".pane", (ps) => ps.length === 2), "two panes side by side");
  const sash = await p.locator(".split-sash").boundingBox();
  const box = await p.locator(".split").boundingBox();
  await p.mouse.move(sash.x + 2, sash.y + 20);
  await p.mouse.down();
  await p.mouse.move(box.x + box.width * 0.3, sash.y + 20, { steps: 5 });
  await p.mouse.up();
  await until(() => p.evaluate(() => Math.abs(core.layout.root.ratio - 0.3) < 0.02), "the core's ratio is 0.3");
});

await test("the scrollbar drags the view by line", async (p) => {
  await p.click(".view .lines");
  await p.evaluate(() => api.call("buffer.new", { name: "long.txt", text: Array.from({ length: 2000 }, (_, i) => `row ${i}`).join("\n") }));
  await until(async () => (await textOf(p)).some((l) => l.includes("row 0")), "the long buffer");
  const bar = await p.locator(".pane.active .vbar").boundingBox();
  // The thumb is the window's share of the lines: small, but there.
  const thumb = await p.locator(".pane.active .thumb").boundingBox();
  assert.ok(thumb.height >= 18 && thumb.height < bar.height / 4, `thumb ${thumb.height}px of ${bar.height}px`);
  await p.mouse.click(bar.x + bar.width / 2, bar.y + bar.height / 2);
  await until(async () => (await textOf(p)).some((l) => /row 9\d\d|row 1[01]\d\d/.test(l)), "the middle of the buffer");
  const moved = await p.locator(".pane.active .thumb").boundingBox();
  assert.ok(Math.abs(moved.y + moved.height / 2 - (bar.y + bar.height / 2)) < moved.height, "the thumb follows");
});

await test("the explorer is driven from the keyboard", async (p) => {
  await p.click(".ex-row:has-text('main.rs')");
  await p.keyboard.press("ArrowDown");
  await until(() => p.$eval(".ex-row.selected", (r) => r.textContent.includes("notes.md")), "notes.md selected");
  await p.keyboard.press("Enter");
  await until(() => p.$eval(".tab.active .tab-name", (e) => e.textContent === "notes.md"), "notes.md opened");
});

await test("a file dropped from the computer opens as a named buffer", async (p) => {
  await p.evaluate(() => {
    const dt = new DataTransfer();
    dt.items.add(new File(["dropped text\n"], "dropped.txt", { type: "text/plain" }));
    const body = document.querySelector(".pane-body");
    for (const type of ["dragover", "drop"]) body.dispatchEvent(new DragEvent(type, { dataTransfer: dt, bubbles: true, cancelable: true }));
  });
  await until(() => p.$eval(".tab.active .tab-name", (e) => e.textContent === "dropped.txt"), "dropped.txt tab");
  await until(async () => (await textOf(p)).some((l) => l.includes("dropped text")), "its text");
});

await test("an input method composes at the caret and commits as text", async (p) => {
  await p.click(".view .lines", { position: { x: 400, y: 4 } });
  await p.keyboard.press("End");
  await p.evaluate(() => {
    const kb = document.getElementById("kb");
    kb.dispatchEvent(new CompositionEvent("compositionstart", { data: "" }));
    kb.dispatchEvent(new CompositionEvent("compositionupdate", { data: "にほ" }));
  });
  assert.equal(await p.$eval("#preedit", (e) => !e.hidden && e.textContent), "にほ");
  await p.evaluate(() => document.getElementById("kb").dispatchEvent(new CompositionEvent("compositionend", { data: "日本" })));
  await until(async () => (await textOf(p)).some((l) => l.includes("{日本")), "the committed text");
  assert.equal(await p.$eval("#preedit", (e) => e.hidden), true);
});

await test("a screen reader hears the caret's line", async (p) => {
  await p.click(".view .lines", { position: { x: 400, y: 4 } });
  await p.keyboard.press("ArrowDown");
  await until(() => p.$eval("#live", (e) => e.textContent.includes("println!")), "the line read out");
  assert.equal(await p.$eval(".view", (v) => v.getAttribute("role")), "textbox");
});

/** Pick a skin from the picker by its name. */
async function pickSkin(p, name) {
  await p.click("#themebtn");
  await p.click(`#thememenu .ts-row:has(.ts-label:text-is("${name}"))`);
}

await test("the skin picker switches skins, and the pick is remembered", async (p) => {
  assert.ok(await p.$("body.theme-cosmos.frame-bezel"), "Cosmos by default");
  assert.ok(await p.$("#device .dv-screen"), "its furniture");
  await pickSkin(p, "Winamp Classic");
  await until(() => p.$("body.theme-winamp.frame-bezel #device .wa-title"), "Winamp's furniture");
  assert.equal(await p.$("body.theme-cosmos"), null, "Cosmos is gone");
  await p.reload();
  await until(() => p.$("body.theme-winamp"), "Winamp after a reload");
  await p.keyboard.press("Control+Alt+t");
  await until(() => p.evaluate(() => skinState.current !== "winamp"), "Ctrl+Alt+T moves on");
  await p.goto(URL + "/?skin=cosmos");
  await until(() => p.$("body.theme-cosmos"), "?skin= picks");
});

await test("a skin can make the menu bar a column: its menus open beside it", async (p) => {
  await p.evaluate(() => setSkin("bridge"));
  await until(() => p.$("body.theme-bridge.frame-bezel"), "Bridge");
  // Its web font's @import is first in the sheet, where a browser keeps it.
  assert.ok(await p.$eval("#skin-css", (s) => s.textContent.trimStart().startsWith("@import")), "the @import leads");
  const view = await p.locator("#menus .mtop:text-is('View')").boundingBox();
  const file = await p.locator("#menus .mtop:text-is('File')").boundingBox();
  assert.ok(view.y > file.y && Math.abs(view.x - file.x) < 1, "the titles are a column");
  await p.click("#menus .mtop:text-is('View')");
  await p.waitForSelector(".dropdown .mitem");
  const menu = await p.locator(".dropdown").first().boundingBox();
  assert.ok(menu.x >= view.x + view.width - 1, `beside its title (${menu.x} vs ${view.x + view.width})`);
  // The keyboard still walks the menus.
  await p.keyboard.press("ArrowRight");
  assert.equal(await p.evaluate(() => core.menus[menubar.open].label), "Selection");
  await p.keyboard.press("Escape");
  // The status bar is the frame's bottom bar, under the client.
  const [bar, app] = [await p.locator("#statusbar").boundingBox(), await p.locator("#app").boundingBox()];
  assert.ok(bar.y >= app.y + app.height, "below the client");
  await until(() => p.$eval("#device .br-col .br-read b", (b) => b.textContent !== "–"), "a live readout");
  await p.evaluate(() => setSkin("cosmos"));
});

await test("a skin's colours win over the editor theme's, and the buffer keeps its own", async (p) => {
  await pickSkin(p, "macOS Light");
  await until(() => p.$("body.theme-macos #mactitle"), "macOS");
  const look = await p.evaluate(() => ({
    menu: getComputedStyle(document.getElementById("menubar")).backgroundColor,
    edFg: getComputedStyle(document.documentElement).getPropertyValue("--ed-fg").trim(),
    themeFg: core.theme.fg,
    title: document.querySelector("#mactitle .mt-name").textContent,
  }));
  assert.equal(look.menu, "rgb(236, 236, 236)", "the skin's frame");
  assert.equal(look.edFg, look.themeFg, "the buffer's ink is the editor theme's");
  assert.equal(look.title, "main.rs — Fresh");
  await p.evaluate(() => setSkin("cosmos"));
});

await test("a skins-folder skin: inert furniture, live values, its editor settings, re-applied on save", async (p) => {
  const theme = await p.evaluate(() => core.theme.name);
  await pickSkin(p, "Probe");
  await until(() => p.$("body.theme-probe.frame-bezel"), "the folder skin");
  // Furniture is rebuilt from inert elements only.
  assert.equal(await p.$("#device script, #device a"), null, "no script, no link");
  assert.equal(await p.evaluate(() => window.skinRan), undefined);
  // Live values from the core.
  await until(() => p.$eval("#device .probe-buffers", (e) => /^[0-9]+$/.test(e.textContent)), "a live value");
  await until(() => p.$eval("#device .probe-gauge", (e) => e.style.getPropertyValue("--v") !== ""), "a bound property");
  // Picking it applied its editor theme and the explorer's side.
  await until(() => p.evaluate(() => core.theme.name.includes("probe")), "its editor theme");
  await until(() => p.$("body.sidebar-right"), "the explorer on the right");
  assert.equal(await p.$eval("#menubar", (e) => getComputedStyle(e).backgroundColor), "rgb(1, 2, 3)");
  // Saving the skin re-applies it.
  const file = `${SKINS}/probe/skin.css`;
  const css = readFileSync(file, "utf8");
  writeFileSync(file, css.replace("rgb(1, 2, 3)", "rgb(4, 5, 6)"));
  try {
    await until(() => p.$eval("#menubar", (e) => getComputedStyle(e).backgroundColor === "rgb(4, 5, 6)"), "the saved change", 8000);
  } finally {
    writeFileSync(file, css);
  }
  // Put the editor back for the other cases.
  await p.evaluate((t) => api.call("skin.theme", { name: t }), theme);
  await p.evaluate(() => api.call("settings.set", { changes: { "/file_explorer/side": "left" } }));
  await p.evaluate(() => setSkin("cosmos"));
  await until(() => p.$("body.sidebar-left"), "the explorer back on the left");
});

await test("a skin pack installs from a folder and from a URL: CSS, assets, furniture, editor theme", async (p) => {
  const theme = await p.evaluate(() => core.theme.name);
  const pack = new globalThis.URL("../example-skins/neon-hive/", import.meta.url).pathname;
  const installed = async (how, image) => {
    await until(() => p.$("body.theme-neon-hive.frame-bezel"), `${how}: the skin`);
    // Its relative image is resolved: inlined from files, absolute from a URL.
    await until(() => p.$eval("#skin-css", (s, i) => s.textContent.includes(i) && !s.textContent.includes("url(img/"), image), `${how}: the image`);
    await until(() => p.$("#device *"), `${how}: its furniture`);
    await until(() => p.evaluate(() => core.theme.name === "neon-night"), `${how}: its editor theme`);
    // The picker stays open, saying so.
    await until(() => p.$('#thememenu .ts-note:has-text("Installed “Neon Hive”")'), `${how}: reported`);
    await p.click('#thememenu .ts-row:has(.ts-label:text-is("Neon Hive")) .ts-tool:text-is("✕")');
    await p.click('#thememenu .ts-row:has(.ts-label:text-is("Neon Hive")) .ts-tool:text-is("Remove")');
    await until(async () => (await p.$('#thememenu .ts-label:text-is("Neon Hive")')) === null, `${how}: removed`);
    await p.keyboard.press("Escape");
    await p.evaluate((t) => api.call("skin.theme", { name: t }), theme);
    await p.evaluate(() => api.call("settings.set", { changes: { "/file_explorer/side": "left" } }));
  };
  // From a folder: the picker's folder input, as "Install from folder…" opens it.
  await p.click("#themebtn");
  await p.setInputFiles("#thememenu .ts-file[webkitdirectory]", pack);
  await installed("folder", "data:image/svg+xml");
  // From a URL: the skin.css, then the furniture and theme its header names.
  await p.route("https://skins.example/neon-hive/**", (r) =>
    r.fulfill({ path: pack + new globalThis.URL(r.request().url()).pathname.slice("/neon-hive/".length) }));
  await p.click("#themebtn");
  await p.click('#thememenu .ts-act:text-is("Install from URL…")');
  await p.fill("#thememenu .ts-url input", "https://skins.example/neon-hive/");
  await p.keyboard.press("Enter");
  await installed("URL", "https://skins.example/neon-hive/img/hexgrid.svg");
  await p.evaluate(() => setSkin("cosmos"));
});

await test("menus are driven from the keyboard, submenus included, and give the keys back", async (p) => {
  // On the first line, so the caret's line is known before any keys.
  await p.click(".view .lines", { position: { x: 300, y: 4 } });
  const line = 1;
  await p.keyboard.press("F10");
  await p.keyboard.press("ArrowRight");
  await p.keyboard.press("ArrowRight");
  assert.equal(await p.evaluate(() => core.menus[menubar.open].label), "View");
  // A letter jumps to a row; Right opens a submenu with its first row lit.
  await p.keyboard.press("t");
  while (!(await p.$eval(".mitem.hi", (r) => r.classList.contains("sub")))) await p.keyboard.press("t");
  await p.keyboard.press("ArrowRight");
  assert.equal(await p.$$eval(".dropdown .dropdown .mitem.hi", (r) => r.length), 1, "in the submenu");
  await p.keyboard.press("ArrowLeft");
  await p.keyboard.press("Escape");
  assert.equal(await p.evaluate(() => [menubar.open, document.activeElement.id].join()), "-1,kb");
  // The caret did not move while the menu had the keys.
  assert.equal(await p.evaluate(() => core.status.line), line);
});

await test("tabs and the editor have their context menus", async (p) => {
  await p.click(".tab:has-text('Welcome')", { button: "right" }).catch(async () => {
    await p.evaluate(() => api.call("buffer.new", { name: "other.txt", text: "x" }));
    await p.click(".tab:has-text('other.txt')", { button: "right" });
  });
  // The tab's rows are the core's (menu.rows), so the menu opens once they arrive.
  await until(async () => {
    const items = await p.$$eval(".dropdown.context .mlabel", (r) => r.map((x) => x.textContent));
    return items.includes("Close Others") && items.includes("Copy Full Path");
  }, "the tab's menu");
  await p.keyboard.press("Escape");
  // A file's tab and explorer entry also offer what plugins add (the
  // bundled Open in Default App), as the terminal's menus do.
  await p.click(".tab:has-text('main.rs')", { button: "right" });
  await until(() => p.$$eval(".dropdown.context .mlabel", (r) => r.some((x) => x.textContent === "Open in Default App")), "the plugin's tab item");
  await p.keyboard.press("Escape");
  await p.click("#explorer .ex-row:has-text('notes.md')", { button: "right" });
  await until(() => p.$$eval(".dropdown.context .mlabel", (r) => r.some((x) => x.textContent === "Open in Default App")), "the plugin's explorer item");
  await p.keyboard.press("Escape");
  await p.click(".tab:has-text('Welcome')", { button: "right" }).catch(async () => {
    await p.click(".tab:has-text('other.txt')", { button: "right" });
  });
  await p.waitForSelector(".dropdown.context .mitem");
  await p.keyboard.press("ArrowDown");
  await p.keyboard.press("ArrowDown");
  await p.keyboard.press("Enter");
  await until(() => p.$$eval(".pane.active .tab", (t) => t.length === 1), "only that tab left");
  await p.click(".view .lines", { button: "right", position: { x: 200, y: 4 } });
  await p.keyboard.press("s");
  assert.equal(await p.$eval(".mitem.hi .mlabel", (e) => e.textContent), "Select All");
  await p.keyboard.press("Enter");
  await until(() => p.evaluate(() => activeView().data.selection), "everything selected");
});

await test("a status item's menu opens above it", async (p) => {
  await p.click("#statusbar .slsp");
  await p.waitForSelector(".popups .popup");
  const pop = await p.locator(".popups").boundingBox();
  const lsp = await p.locator("#statusbar .slsp").boundingBox();
  assert.ok(pop.y + pop.height <= lsp.y + 1 && pop.x + pop.width >= lsp.x, `above the LSP item (${JSON.stringify(pop)})`);
  await p.keyboard.press("Escape");
  await until(async () => (await p.$(".popups")) === null, "closed");
});

await test("the Orchestrator dock is a side column at the left, resizable", async (p) => {
  await p.click("#menubar .mtop:text('View')");
  await p.click(".dropdown .mitem:has-text('Orchestrator Dock')");
  await p.waitForSelector("#dock:not([hidden]) .native-panel");
  const box = (id) => p.locator(id).boundingBox();
  const [dock, side, eds] = [await box("#dock"), await box("#sidebar"), await box("#editors")];
  assert.ok(dock.x < side.x && side.x < eds.x, "dock, explorer, editors, left to right");
  assert.ok(Math.abs(dock.height - eds.height) < 2, "as tall as the editors");
  const s = await box("#dock-sash");
  await p.mouse.move(s.x + 2, s.y + 100);
  await p.mouse.down();
  await p.mouse.move(s.x + 62, s.y + 100, { steps: 4 });
  await p.mouse.up();
  assert.ok(Math.abs((await box("#dock")).width - dock.width - 60) < 3, "60px wider");
  // A plugin's text field takes typing (its text is the core's).
  await p.click('#dock [data-wkey="search-toggle"]');
  await p.waitForSelector('#dock [data-wkey="filter"] .w-text-input');
  await p.keyboard.type("pro");
  await until(() => p.$eval('#dock [data-wkey="filter"] .w-text-input', (i) => i.value === "pro"), "typed into the field");
  await p.keyboard.press("Escape");
  await p.click("#menubar .mtop:text('View')");
  await p.click(".dropdown .mitem:has-text('Orchestrator Dock')");
  await until(() => p.$eval("#dock", (d) => d.hidden), "closed again");
});

await browser.close();
if (failures) {
  console.log(`${failures} failed`);
  process.exit(1);
}
console.log("all passed");
