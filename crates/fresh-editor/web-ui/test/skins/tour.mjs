// The skins review tour: every surface of the client, in every skin, as an
// element screenshot `<surface>--<skin>.png` (sheet.py puts one surface's
// shots side by side). Run against a host started on a project with
// src/api.rs (a long file), README.md and a git repository, e.g.
//   UI_URL=http://127.0.0.1:8174 node tour.mjs <outdir> [skin,skin,…]
// (run from web-ui/test so `playwright` resolves).
import { chromium } from "playwright";
const OUT = process.argv[2];
const SKINS = (process.argv[3] || 'cosmos,macos,macos-dark,compact,winamp,bridge,neon-hive').split(',');
const U = process.env.UI_URL || 'http://127.0.0.1:8174';
const wait = (ms) => new Promise((r) => setTimeout(r, ms));
const b = await chromium.launch();
for (const skin of SKINS) {
  await fetch(U + '/reset', { method: 'POST' });
  const p = await b.newPage({ viewport: { width: 1440, height: 900 } });
  p.on('pageerror', (e) => console.log(skin, 'PAGEERROR', e.message));
  await p.goto(U + '/?skin=' + skin);
  await p.waitForFunction(() => [...views.values()].some((v) => v.data));
  await wait(700);
  const shot = async (name, sel, pad = 6) => {
    try {
      const boxes = await p.evaluate((s) => [...document.querySelectorAll(s)].filter((e) => e.offsetParent || getComputedStyle(e).position === 'fixed').map((e) => e.getBoundingClientRect().toJSON()), sel);
      if (!boxes.length) { console.log(skin, name, 'MISSING', sel); return; }
      const x0 = Math.max(0, Math.min(...boxes.map((r) => r.left)) - pad), y0 = Math.max(0, Math.min(...boxes.map((r) => r.top)) - pad);
      const x1 = Math.min(1440, Math.max(...boxes.map((r) => r.right)) + pad), y1 = Math.min(900, Math.max(...boxes.map((r) => r.bottom)) + pad);
      await p.screenshot({ path: `${OUT}/${name}--${skin}.png`, clip: { x: x0, y: y0, width: x1 - x0, height: y1 - y0 } });
    } catch (e) { console.log(skin, name, 'ERR', e.message.split('\n')[0]); }
  };
  const esc = async () => { await p.keyboard.press('Escape'); await wait(250); };
  const edit = async () => { await p.click('.pane.active .view canvas', { position: { x: 300, y: 10 } }); await wait(150); };
  try {
    // dock open, explorer open
    await p.click('#menubar .mtop:text("View")'); await p.click('.dropdown .mitem:has-text("Orchestrator Dock")'); await wait(1200);
    await edit();
    await p.screenshot({ path: `${OUT}/main--${skin}.png` });
    await shot('dock', skin === 'winamp' ? '#dock, #device .wa-dockwin' : '#dock');
    await shot('tabs', '.pane.active .tabs', 2);
    await shot('statusbar', '#statusbar', 2);
    await shot('explorer', '#sidebar', 0);
    // menu bar menu + submenu
    await p.click('#menubar .mtop:text("View")');
    const rows = await p.evaluate(() => [...document.querySelectorAll('#overlays > [data-layer=menu] > .mitem')].map((e) => e.textContent));
    await p.hover('#overlays .mitem:has-text("Terminal")'); await wait(200);
    await p.keyboard.press('ArrowDown');
    await shot('menu', '#overlays .dropdown, #menubar');
    await esc(); await esc();
    // context menus
    await p.click('.pane.active .view canvas', { button: 'right', position: { x: 260, y: 60 } }); await wait(200);
    await p.keyboard.press('ArrowDown'); await p.keyboard.press('ArrowDown');
    await shot('ctx-editor', '#overlays .dropdown');
    await esc();
    await p.click('.tab >> nth=0', { button: 'right' }); await wait(200);
    await p.hover('#overlays .mitem >> nth=1');
    await shot('ctx-tab', '#overlays .dropdown, .pane.active .tabs');
    await esc();
    await p.click('#explorer .ex-row:has-text("README.md")', { button: 'right' }); await wait(200);
    await p.hover('#overlays .mitem >> nth=2');
    await shot('ctx-explorer', '#overlays .dropdown, #explorer .ex-row.selected');
    await esc();
    // orchestrator menu
    await p.click('#dock [data-wkey="dock-menu"]'); await wait(600);
    await p.hover('.floating-panel .w-button:has-text("Machines")');
    await shot('orch-menu', '.floating-panel, #dock [data-wkey="dock-menu"]');
    await esc(); await wait(300);
    // dock search
    await p.click('#dock [data-wkey="search-toggle"]'); await wait(400);
    await p.keyboard.type('pro'); await wait(500);
    await shot('dock-search', '#dock .w-col');
    await esc(); await esc(); await wait(300);
    // palette
    await edit();
    await p.keyboard.press('Control+p'); await wait(300); await p.keyboard.type('split'); await wait(500);
    await p.keyboard.press('ArrowDown'); await wait(200);
    await shot('palette', '.ask.palette');
    await esc();
    // lsp popup
    await p.click('#statusbar .slsp'); await wait(600);
    await shot('lsp-popup', '.popups, #statusbar .slsp');
    await esc();
    // settings
    await p.keyboard.press('Control+Comma'); await wait(600);
    await shot('settings', '.settings');
    await p.click('.settings .btn:has-text("Done")'); await wait(300);
    // confirm
    await edit(); await p.keyboard.type('x'); await wait(200);
    await p.keyboard.press('Alt+w'); await wait(500);
    await shot('confirm', '.ask.dialog');
    await p.click('.ask.dialog .btn:has-text("Cancel")'); await wait(200);
    await p.keyboard.press('Control+z'); await wait(200);
    // long file, scrolled: scrollbar
    await p.click('#explorer .ex-row:has-text("src")'); await wait(400);
    await p.click('#explorer .ex-row:has-text("api.rs")'); await wait(800);
    await p.mouse.move(900, 400); for (let i = 0; i < 8; i++) { await p.mouse.wheel(0, 500); await wait(50); } await wait(500);
    await shot('scrollbar', '.pane.active .vbar', 30);
  } catch (e) { console.log(skin, 'FLOW ERR', e.message.split('\n')[0]); }
  await p.close();
}
await b.close();
