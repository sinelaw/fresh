// Headless end-to-end test: drives the web UI that taps the REAL render pipeline.
//
// Start the bridge, then run this:
//   cargo run -p fresh-editor --example webui_server -- 127.0.0.1:8141 crates/fresh-editor/src/view/chrome_snapshot.rs &
//   CHROMIUM=/path/to/chrome UI_URL=http://127.0.0.1:8141 node web-ui/test/drive.mjs
//
// Asserts that chrome is UI/DOM positioned at the pipeline's computed rects, the
// buffer interior is the pipeline's real syntax-highlighted cells, and typing
// edits through the real pipeline.
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
await page.goto(URL, { waitUntil: 'networkidle' });
await page.waitForFunction(() => window.fresh && window.fresh.scene && window.fresh.scene.regions.panes.length > 0);
await page.screenshot({ path: `${SHOTS}/20-real-pipeline.png` });

const s = await scene(page);
console.log('\n[scene from the real render pipeline]');
check('has pane(s) from split_areas', s.regions.panes.length >= 1);
check('menu bar region present (UI, real titles)', !!s.regions.menubar && s.regions.menubar.cells[0].map(x => x.t).join('').includes('File'));
check('status bar region present', !!s.regions.statusbar);
check('pane interior shows REAL file content', paneText(s).includes('Chrome seam'));
const fgs = new Set(); s.regions.panes[0].cells.forEach(r => r.forEach(x => { if (x.fg) fgs.add(x.fg); }));
check('pane interior has REAL syntax highlighting (≥3 colors)', fgs.size >= 3, `colors=${fgs.size}`);
check('chrome rendered as UI/DOM (not cells)', (await page.locator('.region.menubar').count()) >= 1 && (await page.locator('.region.pane-content svg.cells').count()) >= 1);
check('tab bar region present', (await page.locator('.region.tabbar').count()) >= 1);

console.log('\n[edit through the real pipeline]');
await page.locator('body').click();
await page.keyboard.type('QWZX');
await page.waitForFunction(() => window.fresh.scene.regions.panes[0].cells.map(r => r.map(x => x.t).join('')).join('\n').includes('QWZX'), { timeout: 5000 }).catch(() => {});
const s2 = await scene(page);
check('typed text appears in the real pipeline-rendered cells', paneText(s2).includes('QWZX'), `head="${paneText(s2).slice(0, 40)}"`);
await page.screenshot({ path: `${SHOTS}/21-real-pipeline-typed.png` });

check('no JS page errors', errs.length === 0, errs.join(' | '));
await browser.close();
console.log(`\n==== ${pass} passed, ${fail} failed ====`);
process.exit(fail ? 1 : 0);
