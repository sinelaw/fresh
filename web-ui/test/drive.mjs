// Headless end-to-end test: drives the REAL editor web UI (no mocks).
//
// Start the bridge first, then run this:
//   cargo run -p fresh-editor --example webui_server -- 127.0.0.1:8139 crates/fresh-editor/src/view/chrome_snapshot.rs &
//   CHROMIUM=/path/to/chrome UI_URL=http://127.0.0.1:8139 node web-ui/test/drive.mjs
//
// Asserts the page renders REAL editor state (menubar, file contents) and that
// keystrokes mutate the REAL buffer via Editor::handle_key.
import { chromium } from 'playwright';
import { mkdirSync, readFileSync } from 'node:fs';

const EXE = process.env.CHROMIUM || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome';
const URL = process.env.UI_URL || 'http://127.0.0.1:8139';
const SHOTS = process.env.SHOTS || '/tmp/pw/shots';
const REAL_FILE = process.env.REAL_FILE || 'crates/fresh-editor/src/view/chrome_snapshot.rs';
mkdirSync(SHOTS, { recursive: true });

let pass = 0, fail = 0;
const check = (n, c, x = '') => { c ? (pass++, console.log('  PASS ' + n)) : (fail++, console.log('  FAIL ' + n + ' ' + x)); };
const view = (p) => p.evaluate(() => JSON.parse(JSON.stringify(window.fresh.view)));

const browser = await chromium.launch({ executablePath: EXE, headless: true, args: ['--no-sandbox'] });
const page = await browser.newPage({ viewport: { width: 1200, height: 760 }, deviceScaleFactor: 2 });
const errors = [];
page.on('pageerror', e => errors.push(String(e)));
page.on('console', m => { if (m.type() === 'error') errors.push('console: ' + m.text()); });

await page.goto(URL, { waitUntil: 'networkidle' });
await page.waitForSelector('#app .menubar');
await page.waitForFunction(() => window.fresh && window.fresh.view);
await page.screenshot({ path: `${SHOTS}/10-real-initial.png` });

const v = await view(page);
console.log('\n[real editor state — no mock]');
check('menubar from the real Editor (has LSP + Explorer)',
  v.chrome.menubar.includes('LSP') && v.chrome.menubar.includes('Explorer'), JSON.stringify(v.chrome.menubar));
const realFirst = readFileSync(REAL_FILE, 'utf8').split('\n')[0];
const active = String(v.active);
check('active buffer shows REAL file content (first line matches disk)',
  v.buffers[active].lines[0] === realFirst, `got="${v.buffers[active].lines[0].slice(0, 40)}"`);
check('rendered SVG text present', (await page.locator('svg.textbody text').count()) > 5);

console.log('\n[edit through the REAL Editor::handle_key]');
await page.locator('body').click();
await page.keyboard.type('ZZZ');
await page.waitForFunction(() => { const v = window.fresh.view; return v.buffers[String(v.active)].lines[0].startsWith('ZZZ'); }, { timeout: 5000 }).catch(() => {});
let v2 = await view(page);
check('typing inserted real text (buffer mutated by the editor)',
  v2.buffers[String(v2.active)].lines[0].startsWith('ZZZ'), `line0="${v2.buffers[String(v2.active)].lines[0].slice(0, 30)}"`);
const before = v2.buffers[String(v2.active)].lines.length;
await page.keyboard.press('Enter');
await page.keyboard.type('// added via web -> real editor');
await page.waitForFunction(() => { const v = window.fresh.view; return v.buffers[String(v.active)].lines.join('\n').includes('added via web'); }, { timeout: 5000 }).catch(() => {});
let v3 = await view(page);
check('Enter created a new line in the real buffer', v3.buffers[String(v3.active)].lines.length > before);
check('typed comment present in real buffer', v3.buffers[String(v3.active)].lines.join('\n').includes('added via web -> real editor'));
await page.screenshot({ path: `${SHOTS}/11-real-typed.png` });

const serverState = await page.evaluate(async () => (await (await fetch('/state')).json()));
check('server /state agrees the real buffer changed', serverState.buffers[String(serverState.active)].lines[0].startsWith('ZZZ'));

check('no JS page errors', errors.length === 0, errors.join(' | '));
await browser.close();
console.log(`\n==== ${pass} passed, ${fail} failed ====`);
process.exit(fail ? 1 : 0);
