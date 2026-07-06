// Headless end-to-end test: drives the web UI that taps the REAL render pipeline.
//
// Start the bridge, then run this:
//   cargo run -p fresh-editor --example webui_server -- 127.0.0.1:8141 crates/fresh-editor/src/view/scene.rs &
//   CHROMIUM=/path/to/chrome UI_URL=http://127.0.0.1:8141 node web-ui/test/drive.mjs
//
// Asserts that:
//   - the buffer interior is the pipeline's real syntax-highlighted CELLS,
//   - the chrome (menu bar, tabs, status bar, menu dropdown) is rendered as
//     NATIVE HTML from the pipeline's semantic model (no chrome cells),
//   - keyboard / mouse / menu interactions run through the real Editor — all
//     input rides the WebSocket transport (JSON messages on /ws), and
//   - the WebSocket PUSH transport works: the server sends region-diff frames
//     when (and only when) the scene changes, one client at a time, with the
//     HTTP routes still live alongside for curl / the parity harness.
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
const errs = []; page.on('pageerror', e => errs.push(String(e)));
// The single-client test below deliberately opens a second /ws socket that the
// server rejects (409) — Chromium logs that handshake failure as a console
// error, so /ws connection noise is filtered out of the page-error assertion.
page.on('console', m => { if (m.type() === 'error' && !/WebSocket connection to .*\/ws/.test(m.text())) errs.push('console:' + m.text()); });
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

console.log('\n[popups = native HTML from the popup model, NOT cells]');
await page.locator('body').click();
await page.locator('.statusbar .seg[data-name="remote"]').first().click();   // opens the Remote indicator popup
await page.waitForTimeout(300);
const pv = await scene(page);
check('editor reports a popup', (pv.regions.popups || []).length >= 1, 'popups=' + (pv.regions.popups || []).length);
check('popup rendered as native .popup', (await page.locator('.popup').count()) >= 1);
check('popup has native rows (.popup-row)', (await page.locator('.popup .popup-row').count()) >= 1);
check('NO svg/cells inside the popup', (await page.locator('.popup svg').count()) === 0);
await page.screenshot({ path: `${SHOTS}/24-native-popup.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(150);
check('Escape closed the popup', ((await scene(page)).regions.popups || []).length === 0);

console.log('\n[edit through the real pipeline]');
// Click the center of pane 0's content rect (robust against a file explorer
// left open by earlier runs on the same live server shifting the pane right).
const editRect = (await scene(page)).regions.panes[0].content;
await page.mouse.click((editRect.x + Math.floor(editRect.w / 2)) * 8.2, (editRect.y + Math.floor(editRect.h / 2)) * 18);
await page.keyboard.type('QWZX');
await page.waitForFunction(() => window.fresh.scene.regions.panes[0].cells.map(r => r.map(x => x.t).join('')).join('\n').includes('QWZX'), { timeout: 5000 }).catch(() => {});
const s2 = await scene(page);
check('typed text appears in the real pipeline-rendered cells', paneText(s2).includes('QWZX'), `head="${paneText(s2).slice(0, 40)}"`);
await page.screenshot({ path: `${SHOTS}/21-real-pipeline-typed.png` });

console.log('\n[file explorer = native tree, NOT cells]');
await page.locator('body').click();
// Open the sidebar if it isn't already (Ctrl+B toggles; the live editor may
// carry prior state), then wait for the async directory scan to arrive as a
// pushed frame (don't re-toggle while it's merely still loading).
if (!(await scene(page)).regions.fileExplorer) {
  await page.keyboard.press('Control+b');
}
await page.waitForFunction(() => { const fe = window.fresh.scene.regions.fileExplorer; return fe && fe.rows && fe.rows.length > 0; }, { timeout: 8000 }).catch(() => {});
const fx = await scene(page);
check('file explorer is a native tree in the scene', !!(fx.regions.fileExplorer && fx.regions.fileExplorer.rows.length > 0), 'rows=' + ((fx.regions.fileExplorer && fx.regions.fileExplorer.rows.length) || 0));
check('explorer rendered as native .fx-row', (await page.locator('.fileexplorer .fx-row').count()) >= 1);
check('NO svg/cells inside the explorer', (await page.locator('.fileexplorer svg').count()) === 0);
await page.screenshot({ path: `${SHOTS}/25-native-explorer.png` });

console.log('\n[workspace-trust dialog = native modal, NOT cells]');
await page.request.post(URL + '/action', { data: { action: 'workspace_trust_prompt' } });
await page.waitForFunction(() => !!window.fresh.scene.regions.trustDialog, { timeout: 6000 }).catch(() => {});
check('editor reports the trust dialog', !!(await scene(page)).regions.trustDialog);
check('trust dialog rendered as native modal (3 options)', (await page.locator('.trustdialog .td-opt').count()) === 3);
check('NO svg/cells inside the trust dialog', (await page.locator('.trustdialog svg').count()) === 0);
await page.screenshot({ path: `${SHOTS}/26-native-trust.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(200);
check('Escape closed the trust dialog', !(await scene(page)).regions.trustDialog);

console.log('\n[plugin widgets = native WidgetSpec (live-grep toolbar), NOT cells]');
await page.request.post(URL + '/action', { data: { action: 'start_live_grep' } });
await page.waitForFunction(() => { const p = window.fresh.scene.regions.palette; return p && p.toolbar; }, { timeout: 8000 }).catch(() => {});
await page.waitForTimeout(300);
check('palette carries a plugin toolbar WidgetSpec', !!(await scene(page)).regions.palette?.toolbar);
check('toolbar rendered as native toggles', (await page.locator('.ptoolbar .w-toggle').count()) >= 5);
check('NO svg/cells in the toolbar', (await page.locator('.ptoolbar svg').count()) === 0);
const tgl = page.locator('.ptoolbar .w-toggle').filter({ hasText: 'Ignored' }).first();
const tBefore = await tgl.evaluate(el => el.classList.contains('on'));
await tgl.click(); await page.waitForTimeout(400);
check('clicking a toggle flips it via the real plugin path',
  (await page.locator('.ptoolbar .w-toggle').filter({ hasText: 'Ignored' }).first().evaluate(el => el.classList.contains('on'))) !== tBefore);
await page.screenshot({ path: `${SHOTS}/27-native-widgets.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(200);

console.log('\n[plugin floating/dock widget = native WidgetSpec, NOT cells]');
if (!((await scene(page)).regions.widgets || []).length) {
  await page.request.post(URL + '/action', { data: { action: 'orchestrator_dock_toggle' } });
}
await page.waitForFunction(() => { const w = window.fresh.scene.regions.widgets; return w && w.length > 0; }, { timeout: 8000 }).catch(() => {});
await page.waitForTimeout(300);
check('editor reports a widget surface', !!((await scene(page)).regions.widgets || []).length);
check('dock rendered as a native widget panel', (await page.locator('.widget-surface .w-button').count()) >= 3);
check('NO svg/cells inside the widget panel', (await page.locator('.widget-surface svg').count()) === 0);
await page.screenshot({ path: `${SHOTS}/28-native-dock.png` });

console.log('\n[keybinding editor = full native modal incl. edit dialog]');
// Start clean: dismiss any focused dock/floating panel so keys reach the editor.
await page.keyboard.press('Escape'); await page.waitForTimeout(120);
if (((await scene(page)).regions.widgets || []).length) {
  await page.request.post(URL + '/action', { data: { action: 'orchestrator_dock_toggle' } });
  await page.waitForTimeout(200);
}
await page.request.post(URL + '/action', { data: { action: 'open_keybinding_editor' } });
await page.waitForFunction(() => !!window.fresh.scene.regions.keybindingEditor, { timeout: 8000 }).catch(() => {});
await page.waitForTimeout(300);
check('keybinding editor is a native modal', (await page.locator('.kbedit .kb-table .kb-row').count()) >= 5);
check('NO svg/cells in the keybinding editor', (await page.locator('.kbedit svg').count()) === 0);
await page.waitForFunction(() => { const k = window.fresh.scene.regions.keybindingEditor; return k && k.rows.length > 0; }, { timeout: 8000 }).catch(() => {});
// Open the add dialog ('a'); retry in case the first keypress races a cold-start poll.
for (let i = 0; i < 3 && !((await scene(page)).regions.keybindingEditor || {}).editDialog; i++) {
  await page.keyboard.press('a'); await page.waitForTimeout(400);
}
check('Add-binding dialog renders natively (fields)', (await page.locator('.kbedit .kb-dialog .kb-field').count()) >= 3);
await page.screenshot({ path: `${SHOTS}/29-native-keybindings.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(150); await page.keyboard.press('Escape'); await page.waitForTimeout(200);

console.log('\n[Settings = full native modal incl. entry dialog]');
await page.keyboard.press('Escape'); await page.waitForTimeout(120);
await page.request.post(URL + '/action', { data: { action: 'open_settings' } });
await page.waitForFunction(() => !!window.fresh.scene.regions.settings, { timeout: 8000 }).catch(() => {});
await page.waitForTimeout(300);
check('Settings is a native modal (categories+items)', (await page.locator('.settings-modal .set-cat').count()) >= 5 && (await page.locator('.settings-modal .set-item').count()) >= 3);
check('NO svg/cells in the settings modal', (await page.locator('.settings-modal svg').count()) === 0);
await page.keyboard.press('Tab'); await page.waitForTimeout(120);
for (let i = 0; i < 4; i++) { await page.keyboard.press('ArrowDown'); await page.waitForTimeout(80); }
await page.keyboard.press('Enter');
await page.waitForFunction(() => { const s = window.fresh.scene.regions.settings; return s && s.entryDialog; }, { timeout: 5000 }).catch(() => {});
check('Settings entry (add/edit) dialog renders natively', (await page.locator('.settings-modal .set-entry .set-item').count()) >= 3);
await page.screenshot({ path: `${SHOTS}/30-native-settings.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(120); await page.keyboard.press('Escape'); await page.waitForTimeout(150);

console.log('\n[WebSocket push transport (no polling)]');
check('WebSocket transport is open (window.fresh.wsOpen)', await page.evaluate(() => window.fresh.wsOpen));
// Genuine server PUSH: mutate the editor over the HTTP route (curl-equivalent,
// no page input at all) and watch the change arrive as a pushed frame.
const feBefore = !!(await scene(page)).regions.fileExplorer;
const frames0 = await page.evaluate(() => window.fresh.frames);
await page.request.post(URL + '/action', { data: { action: 'toggle_file_explorer' } });
await page.waitForFunction(fe0 => (!!window.fresh.scene.regions.fileExplorer) !== fe0, feBefore, { timeout: 5000 }).catch(() => {});
check('HTTP-route mutation arrives as a PUSHED frame (no page input)',
  (!!(await scene(page)).regions.fileExplorer) !== feBefore && (await page.evaluate(() => window.fresh.frames)) > frames0,
  `explorer ${feBefore}->${!!(await scene(page)).regions.fileExplorer} frames ${frames0}->${await page.evaluate(() => window.fresh.frames)}`);
await page.request.post(URL + '/action', { data: { action: 'toggle_file_explorer' } });   // restore
await page.waitForTimeout(500);
// Idle discipline: nothing changes → (almost) no frames. The poll.active hint
// may allow the odd stray frame, so bound it loosely.
const framesIdle0 = await page.evaluate(() => window.fresh.frames);
await page.waitForTimeout(1600);   // no input at all
const framesIdle1 = await page.evaluate(() => window.fresh.frames);
check('idle: no frames pushed while nothing changes', framesIdle1 - framesIdle0 <= 3, `frames ${framesIdle0}->${framesIdle1}`);
check('scene still carries the poll pacing hint', !!(await scene(page)).regions.poll);

console.log('\n[region diffs: typing resends only what changed]');
// Focus the buffer itself (the explorer toggles above may have left keyboard
// focus in the tree): click the center of pane 0's content rect.
const paneRect = (await scene(page)).regions.panes[0].content;
await page.mouse.click((paneRect.x + Math.floor(paneRect.w / 2)) * 8.2, (paneRect.y + Math.floor(paneRect.h / 2)) * 18);
await page.waitForTimeout(300);
const seqT0 = await page.evaluate(() => window.fresh.seq);
await page.keyboard.type('J');
await page.waitForFunction(s0 => window.fresh.seq > s0, seqT0, { timeout: 5000 }).catch(() => {});
const diffKeys = await page.evaluate(() => window.fresh.lastFrameKeys);
check('a typing frame is a region DIFF (changed paths, not a scene)', diffKeys.length > 0, JSON.stringify(diffKeys));
check('typing frame touches the pane, per index', diffKeys.some(k => k.startsWith('regions.panes.')), JSON.stringify(diffKeys));
check('typing frame does NOT resend heavyweight unrelated regions',
  !diffKeys.includes('regions.settings') && !diffKeys.includes('regions.keybindingEditor') && !diffKeys.includes('regions.widgets'),
  JSON.stringify(diffKeys));

console.log('\n[single-client model: a second WebSocket is rejected]');
const second = await page.evaluate(() => new Promise(res => {
  const w = new WebSocket((location.protocol === 'https:' ? 'wss' : 'ws') + '://' + location.host + '/ws');
  w.onopen = () => { w.close(); res('open'); };
  w.onclose = () => res('closed');
  setTimeout(() => res('timeout'), 3000);
}));
check('second WebSocket is rejected before upgrade (409)', second === 'closed', second);
check('first socket unaffected by the rejected second one', await page.evaluate(() => window.fresh.wsOpen));
const seqR0 = await page.evaluate(() => window.fresh.seq);
await page.keyboard.type('Q');
check('first socket still functional (input still round-trips)',
  await page.waitForFunction(s0 => window.fresh.seq > s0, seqR0, { timeout: 5000 }).then(() => true).catch(() => false));

check('no JS page errors', errs.length === 0, errs.join(' | '));
await browser.close();
console.log(`\n==== ${pass} passed, ${fail} failed ====`);
process.exit(fail ? 1 : 0);
