// Headless end-to-end test: drives the web UI that taps the REAL render pipeline.
//
// Start the bridge, then run this:
//   cargo run -p fresh-editor --example webui_server -- 127.0.0.1:8141 crates/fresh-editor/src/view/chrome_snapshot.rs &
//   CHROMIUM=/path/to/chrome UI_URL=http://127.0.0.1:8141 node web-ui/test/drive.mjs
//
// Asserts that:
//   - the buffer interior is the pipeline's real syntax-highlighted CELLS,
//   - the chrome (menu bar, tabs, status bar, menu dropdown) is rendered as
//     NATIVE HTML from the pipeline's semantic model (no chrome cells), and
//   - keyboard / mouse / menu interactions run through the real Editor.
import { chromium } from 'playwright';
import { mkdirSync } from 'node:fs';
const EXE = process.env.CHROMIUM || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome';
const URL = process.env.UI_URL || 'http://127.0.0.1:8141';
const SHOTS = process.env.SHOTS || '/tmp/pw/shots';
mkdirSync(SHOTS, { recursive: true });
let pass = 0, fail = 0;
const check = (n, c, x = '') => { c ? (pass++, console.log('  PASS ' + n)) : (fail++, console.log('  FAIL ' + n + ' ' + x)); };
const scene = p => p.evaluate(() => JSON.parse(JSON.stringify(window.fresh.scene)));
const paneText = s => s.regions.panes[0].cells.map(r => r.map(x => x.t).join('')).join('\n');

const browser = await chromium.launch({ executablePath: EXE, headless: true, args: ['--no-sandbox'] });
const page = await browser.newPage({ viewport: { width: 1280, height: 800 }, deviceScaleFactor: 2 });
const errs = []; page.on('pageerror', e => errs.push(String(e))); page.on('console', m => { if (m.type() === 'error') errs.push('console:' + m.text()); });
let stateReqs = 0; page.on('request', r => { if (r.url().endsWith('/state')) stateReqs++; });
await page.goto(URL, { waitUntil: 'networkidle' });
await page.waitForFunction(() => window.fresh && window.fresh.scene && window.fresh.scene.regions.panes.length > 0);
await page.keyboard.press('Escape'); await page.waitForTimeout(150); // close any menu left open in the live editor
await page.screenshot({ path: `${SHOTS}/20-real-pipeline.png` });

const s = await scene(page);
console.log('\n[buffer interior = real pipeline cells]');
check('has pane(s) from split_areas', s.regions.panes.length >= 1);
check('pane interior shows REAL file content', paneText(s).length > 0);
const fgs = new Set(); s.regions.panes[0].cells.forEach(r => r.forEach(x => { if (x.fg) fgs.add(x.fg); }));
check('pane interior has REAL syntax highlighting (≥3 colors)', fgs.size >= 3, `colors=${fgs.size}`);
check('buffer interior IS drawn from cells (svg)', (await page.locator('.region.pane-content svg.cells').count()) >= 1);

console.log('\n[chrome = native HTML from the semantic model, NOT cells]');
check('semantic menu model present (File menu)', s.regions.menus.some(m => m.label === 'File'));
check('menu bar rendered as native .menu spans', (await page.locator('.menubar .menu').count()) >= 5);
check('NO svg/cells used for the menu bar', (await page.locator('.menubar svg').count()) === 0);
check('tabs rendered as native .tab elements', (await page.locator('.tabbar .tab').count()) >= 1);
check('status bar rendered as native segments', (await page.locator('.statusbar .seg, .statusbar .txt').count()) >= 2);
check('status model has labeled segments', Array.isArray(s.regions.statusbar.segments) && s.regions.statusbar.segments.length >= 2);

console.log('\n[menu opens a native dropdown via the real editor]');
await page.locator('.menubar .menu', { hasText: 'File' }).first().click();
await page.waitForTimeout(200);
const sm = await scene(page);
check('editor reports the open menu', sm.regions.menuOpen != null);
check('dropdown rows rendered as native .mitem', (await page.locator('.mitem').count()) >= 4);
check('dropdown shows accelerators (e.g. Ctrl+N)', (await page.locator('.mitem .accel').count()) >= 1);
check('NO cells/svg inside the dropdown', (await page.locator('.dropdown svg').count()) === 0);
await page.screenshot({ path: `${SHOTS}/22-native-menu.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(150);

console.log('\n[command palette = native HTML from the prompt model]');
await page.locator('body').click();      // focus the page so Ctrl+P reaches the editor
await page.keyboard.press('Control+p');
await page.waitForTimeout(300);
const sp = await scene(page);
check('editor opened the picker (palette in scene)', !!sp.regions.palette, 'no palette');
check('palette rendered as native .palette card', (await page.locator('.palette').count()) >= 1);
check('palette has native rows (.prow)', (await page.locator('.palette .prow').count()) >= 1);
check('NO svg/cells inside the palette', (await page.locator('.palette svg').count()) === 0);
await page.screenshot({ path: `${SHOTS}/23-native-palette.png` });
await page.keyboard.type('split');
await page.waitForTimeout(300);
const sp2 = await scene(page);
check('typing filtered the real suggestion list', sp2.regions.palette && sp2.regions.palette.total < sp.regions.palette.total, `before=${sp.regions.palette.total} after=${sp2.regions.palette && sp2.regions.palette.total}`);
await page.keyboard.press('Escape'); await page.waitForTimeout(150);
check('Escape closed the palette', !(await scene(page)).regions.palette);

console.log('\n[edit through the real pipeline]');
await page.mouse.click(300, 300);
await page.keyboard.type('QWZX');
await page.waitForFunction(() => window.fresh.scene.regions.panes[0].cells.map(r => r.map(x => x.t).join('')).join('\n').includes('QWZX'), { timeout: 5000 }).catch(() => {});
const s2 = await scene(page);
check('typed text appears in the real pipeline-rendered cells', paneText(s2).includes('QWZX'), `head="${paneText(s2).slice(0, 40)}"`);
await page.screenshot({ path: `${SHOTS}/21-real-pipeline-typed.png` });

console.log('\n[frame pump advances without user input, like the TUI loop]');
const reqs0 = stateReqs;
await page.waitForTimeout(1600);   // no input at all
check('GET /state keeps ticking the editor with no input', stateReqs > reqs0 + 1, `reqs ${reqs0}->${stateReqs}`);
check('idle poll is throttled, not a busy loop', (stateReqs - reqs0) < 12, `polls=${stateReqs - reqs0}`);
check('scene carries a poll pacing hint', !!(await scene(page)).regions.poll);

check('no JS page errors', errs.length === 0, errs.join(' | '));
await browser.close();
console.log(`\n==== ${pass} passed, ${fail} failed ====`);
process.exit(fail ? 1 : 0);
