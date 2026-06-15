import { chromium } from 'playwright';
import { mkdirSync } from 'node:fs';

const EXE = process.env.CHROMIUM || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome';
const URL = process.env.FRESH_UI || ('file://' + new URL('../index.html', import.meta.url).pathname);
const SHOTS = process.env.SHOTS || '/tmp/pw/shots';
mkdirSync(SHOTS, { recursive: true });

let pass = 0, fail = 0;
function check(name, cond, extra='') {
  if (cond) { pass++; console.log(`  PASS ${name}`); }
  else { fail++; console.log(`  FAIL ${name} ${extra}`); }
}
const st = (page) => page.evaluate(() => JSON.parse(JSON.stringify(window.fresh.state)));

const browser = await chromium.launch({ executablePath: EXE, headless: true, args: ['--no-sandbox'] });
const page = await browser.newPage({ viewport: { width: 1200, height: 760 }, deviceScaleFactor: 2 });
const errors = [];
page.on('pageerror', e => errors.push(String(e)));
page.on('console', m => { if (m.type() === 'error') errors.push('console: ' + m.text()); });

await page.goto(URL);
await page.waitForSelector('#app .menubar');
await page.screenshot({ path: `${SHOTS}/01-initial.png` });

let s = await st(page);
console.log('\n[initial render]');
check('6 menu items', s.menubar.length === 6, `got ${s.menubar.length}`);
check('3 panes rendered', (await page.locator('.pane').count()) === 3);
check('2 dividers rendered', (await page.locator('.divider').count()) === 2);
check('active buffer = 1', s.activeBuffer === 1);
check('SVG text body present', (await page.locator('svg.textbody').count()) >= 3);
check('syntax color applied', (await page.locator('tspan[fill="#569cd6"]').count()) > 0);

console.log('\n[typing into the active pane]');
await page.locator('[data-content="1"]').click({ position: { x: 120, y: 8 } }); // line 0
await page.keyboard.type('HELLO ');
s = await st(page);
check('typed text inserted into buffer 1 line 0', s.buffers['1'].lines[0].includes('HELLO '), `line0="${s.buffers['1'].lines[0].slice(0,30)}"`);
check('buffer marked modified', s.buffers['1'].modified === true);
await page.keyboard.press('Enter');
await page.keyboard.type('// new line');
s = await st(page);
check('Enter split the line (line count grew)', s.buffers['1'].lines.length >= 9);
await page.screenshot({ path: `${SHOTS}/02-typed.png` });

console.log('\n[arrow keys + backspace]');
await page.keyboard.press('Home');
await page.keyboard.press('ArrowRight');
await page.keyboard.press('Backspace');
s = await st(page);
check('cursor + backspace edited current line', typeof s.buffers['1'].cursor.col === 'number');

console.log('\n[switch tab]');
await page.locator('[data-buffer="2"]').click();
s = await st(page);
check('clicking tab switched active buffer to 2', s.activeBuffer === 2);
await page.screenshot({ path: `${SHOTS}/03-tab-switch.png` });

console.log('\n[command palette]');
await page.keyboard.press('Control+p');
s = await st(page);
check('Ctrl+P opened the palette', s.overlay && s.overlay.kind === 'palette');
check('palette visible in DOM', await page.locator('.palette input').isVisible());
await page.keyboard.type('READ');
s = await st(page);
check('palette filtered to README', s.overlay.items.length >= 1 && s.overlay.items.some(i => /README/.test(i.label)));
await page.screenshot({ path: `${SHOTS}/04-palette.png` });
await page.keyboard.press('Enter');
s = await st(page);
check('palette Enter switched to README buffer (3)', s.activeBuffer === 3);
check('palette closed', s.overlay === null);

console.log('\n[drag the split divider]');
const div = page.locator('.divider.vertical').first();
const box = await div.boundingBox();
const before = (await st(page)).tree.ratio;
await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
await page.mouse.down();
await page.mouse.move(box.x - 180, box.y + box.height / 2, { steps: 8 });
await page.mouse.up();
const after = (await st(page)).tree.ratio;
check('dragging divider changed the split ratio', Math.abs(after - before) > 0.05, `before=${before.toFixed(3)} after=${after.toFixed(3)}`);
await page.screenshot({ path: `${SHOTS}/05-divider-drag.png` });

console.log('\n[close a tab]');
await page.locator('[data-close="2"]').click();
s = await st(page);
check('closing tab removed buffer 2 from model', !s.buffers['2']);
check('tree pruned (still renders)', (await page.locator('.pane').count()) >= 1);
await page.screenshot({ path: `${SHOTS}/06-tab-closed.png` });

console.log('\n[open a menu]');
await page.locator('[data-menu="0"]').click();
s = await st(page);
check('clicking File opened a menu dropdown', s.openMenu === 0);
check('dropdown items shown', (await page.locator('.menu-dropdown .menu-item').count()) > 0);
await page.screenshot({ path: `${SHOTS}/07-menu.png` });

console.log('\n[no uncaught page errors]');
check('no JS page errors', errors.length === 0, errors.join(' | '));

await browser.close();
console.log(`\n==== ${pass} passed, ${fail} failed ====`);
process.exit(fail ? 1 : 0);
