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
//     HTTP routes still live alongside for curl / the parity harness, and
//   - the frontend patches the DOM per region (a typing frame rebuilds only
//     its pane), metrics are measured + zoomable (Ctrl+= / Ctrl+0, exact
//     hit-testing while zoomed), and touch pan/tap works in a hasTouch context.
import { chromium } from 'playwright';
import { mkdirSync } from 'node:fs';
// CHROMIUM (optional) points at an existing Chromium binary; when unset,
// playwright launches the browser its own `playwright install` provisioned
// (which is what CI uses — never hardcode a machine-specific path here).
const EXE = process.env.CHROMIUM;
const URL = process.env.UI_URL || 'http://127.0.0.1:8141';
const SHOTS = process.env.SHOTS || '/tmp/pw/shots';
mkdirSync(SHOTS, { recursive: true });
let pass = 0, fail = 0;
const check = (n, c, x = '') => { c ? (pass++, console.log('  PASS ' + n)) : (fail++, console.log('  FAIL ' + n + ' ' + x)); };
const scene = p => p.evaluate(() => JSON.parse(JSON.stringify(window.fresh.scene)));
// Grid→page pixel mapping: the COSMOS shell insets #app from the page origin,
// so cell coordinates offset by the app origin the metrics carry (ax/ay).
const gx = (m, col) => m.ax + col * m.cw;
const gy = (m, row) => m.ay + row * m.ch;
const paneText = s => s.regions.panes[0].cells.map(r => r.map(x => x.t).join('')).join('\n');

const browser = await chromium.launch({ ...(EXE ? { executablePath: EXE } : {}), headless: true, args: ['--no-sandbox'] });
// 1280×960: the COSMOS shell (wallpaper margin + bezel) insets the cell grid
// ~190px vertically, and the View dropdown needs the taller grid to fit all
// its items (the TUI clips a dropdown to the grid, hiding the tail rows).
const page = await browser.newPage({ viewport: { width: 1280, height: 960 }, deviceScaleFactor: 2 });
const errs = []; page.on('pageerror', e => errs.push(String(e)));
// The mirroring test below opens a second /ws socket alongside the page's own
// and later closes it — normal WebSocket churn that Chromium may log as console
// noise, so /ws connection messages are filtered out of the page-error
// assertion.
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
const editM = await page.evaluate(() => window.fresh.metrics);
await page.mouse.click(gx(editM, editRect.x + Math.floor(editRect.w / 2)), gy(editM, editRect.y + Math.floor(editRect.h / 2)));
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
// The explorer chrome must not be drawn from buffer CELLS (svg.cells). Decorative
// file-type/folder icons are legitimate inline-SVG chrome (class "ficon"), so this
// targets the buffer-cell class specifically rather than all SVG.
check('NO buffer cells (svg.cells) inside the explorer', (await page.locator('.fileexplorer svg.cells').count()) === 0);
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
// The redesigned dock (hierarchical folder tree) collapses the filter
// toggles behind a "Filters" disclosure — expand it so the full toolbar
// (view / project / worktree / hide-trivial / Manage) is rendered.
await page.locator('.widget-surface.w-dock .w-button', { hasText: 'Filters' }).first().click();
await page.waitForTimeout(400);
check('dock rendered as a native widget panel', (await page.locator('.widget-surface .w-button').count()) >= 3);
check('NO svg/cells inside the widget panel', (await page.locator('.widget-surface svg').count()) === 0);
await page.screenshot({ path: `${SHOTS}/28-native-dock.png` });

console.log('\n[dock widget clicks: identity-routed, robust against hit-list drift]');
// Panel clicks are delivered by IDENTITY (widgetKey + eventType + payload),
// with the raw hit index only as a tiebreaker — raw indices go stale whenever
// the plugin re-renders between the pushed frame and the click, and list hits
// are windowed to the TUI's visible rows (the web list is natively scrolled,
// so its lower rows may have no recorded hit at all). Full multi-workspace
// reproduction (start real workspaces, click rows below the TUI fold) is
// deliberately NOT run here: each orchestrator workspace creates a real git
// worktree + spawned shell session under ~/.local/share/fresh (persistent
// side effects, seconds each, trust-dialog churn) — not CI-safe. The
// off-window synthesis path is covered by the resolver's spec-driven design;
// what CI covers is the identity routing itself, including stale indices.
const dockSurf = async () => ((await scene(page)).regions.widgets || []).find(w => w.kind === 'dock');
const findByKey = (sp, key) => { if (!sp) return undefined; if (sp.key === key) return sp;
  for (const c of (sp.children || [])) { const r = findByKey(c, key); if (r !== undefined) return r; }
  return sp.child ? findByKey(sp.child, key) : undefined; };
const d0 = await dockSurf();
const t0v = !!(findByKey(d0.spec, 'hide-trivial') || {}).checked;
await page.locator('.widget-surface.w-dock .w-toggle', { hasText: 'empty' }).first().click();
await page.waitForFunction(v0 => { const d = (window.fresh.scene.regions.widgets || []).find(w => w.kind === 'dock');
  const f = sp => { if (!sp) return undefined; if (sp.key === 'hide-trivial') return sp; for (const c of (sp.children || [])) { const r = f(c); if (r !== undefined) return r; } return sp.child ? f(sp.child) : undefined; };
  return d && !!(f(d.spec) || {}).checked !== v0; }, t0v, { timeout: 5000 }).catch(() => {});
const t1v = !!(findByKey((await dockSurf()).spec, 'hide-trivial') || {}).checked;
check('dock toggle click flips real plugin state (identity-routed)', t1v !== t0v, `checked ${t0v}->${t1v}`);
// The hits list was regenerated by that re-render; the next click must still resolve.
await page.locator('.widget-surface.w-dock .w-toggle', { hasText: 'empty' }).first().click();
await page.waitForFunction(v1 => { const d = (window.fresh.scene.regions.widgets || []).find(w => w.kind === 'dock');
  const f = sp => { if (!sp) return undefined; if (sp.key === 'hide-trivial') return sp; for (const c of (sp.children || [])) { const r = f(c); if (r !== undefined) return r; } return sp.child ? f(sp.child) : undefined; };
  return d && !!(f(d.spec) || {}).checked !== v1; }, t1v, { timeout: 5000 }).catch(() => {});
const t2v = !!(findByKey((await dockSurf()).spec, 'hide-trivial') || {}).checked;
check('dock click still resolves after the hits list was regenerated', t2v === t0v, `checked ${t1v}->${t2v}`);
// THE regression shape: a stale/bogus hitIndex with a correct identity must
// still deliver (pre-fix, hitIndex was the only routing and this was a
// silent no-op). Same bridge dispatch as the WS path.
const dStale = await dockSurf();
const staleHit = dStale.hits.find(h => h.widgetKey === 'hide-trivial');
await page.request.post(URL + '/widget', { data: { surface: 'panel', plugin: dStale.plugin, panelId: dStale.panelId,
  widgetKey: staleHit.widgetKey, eventType: staleHit.eventType, payload: staleHit.payload, hitIndex: 999 } });
await page.waitForFunction(v2 => { const d = (window.fresh.scene.regions.widgets || []).find(w => w.kind === 'dock');
  const f = sp => { if (!sp) return undefined; if (sp.key === 'hide-trivial') return sp; for (const c of (sp.children || [])) { const r = f(c); if (r !== undefined) return r; } return sp.child ? f(sp.child) : undefined; };
  return d && !!(f(d.spec) || {}).checked !== v2; }, t2v, { timeout: 5000 }).catch(() => {});
const t3v = !!(findByKey((await dockSurf()).spec, 'hide-trivial') || {}).checked;
check('stale hitIndex + correct identity still delivers (drift regression)', t3v !== t2v, `checked ${t2v}->${t3v}`);
// A session tree-row click selects it through the same identity path.
await page.locator('.widget-surface.w-dock .w-tree-row').first().click();
await page.waitForTimeout(500);
const dSel = await dockSurf();
check('clicking a dock workspace row selects it in the scene',
  !!dSel && dSel.instances && dSel.instances.sessions && dSel.instances.sessions.selectedIndex === 0,
  JSON.stringify(dSel && dSel.instances));

console.log('\n[dock right-click = plugin context menu (anchored popup, like the TUI)]');
// Right-click on a session row fires a `context` widget event (never in the
// recorded hits — the bridge synthesizes it, exactly like the TUI's
// right-click path) and the orchestrator raises its anchored Visit / Move /
// Archive / Delete popup, undimmed, dismissed by Esc or an outside click.
const modalSurf = async () => ((await scene(page)).regions.widgets || []).find(w => w.kind === 'floatingModal');
await page.locator('.widget-surface.w-dock .w-tree-row').first().click({ button: 'right' });
await page.waitForFunction(() => (window.fresh.scene.regions.widgets || []).some(w => w.kind === 'floatingModal'), { timeout: 8000 }).catch(() => {});
const ctxM = await modalSurf();
check('right-click on a dock row opens the context-menu panel', !!ctxM);
check('context menu is ANCHORED (popup placement, not centered)', !!ctxM && ctxM.anchored === true);
const ctxText = ctxM ? await page.locator('.widget-surface.w-floatingModal').innerText() : '';
check('context menu offers Visit / Move to folder / Archive / Delete',
  /Visit/.test(ctxText) && /Move to Folder/i.test(ctxText) && /Archive/.test(ctxText) && /Delete/.test(ctxText), ctxText);
check('anchored popup scrim is transparent (no modal dim)', (await page.locator('.modal-scrim.scrim-clear').count()) === 1);
await page.screenshot({ path: `${SHOTS}/29-dock-context-menu.png` });
// Esc dismisses the popup; the dock survives.
await page.keyboard.press('Escape');
await page.waitForFunction(() => !(window.fresh.scene.regions.widgets || []).some(w => w.kind === 'floatingModal'), { timeout: 8000 }).catch(() => {});
check('Esc closes the context menu', !(await modalSurf()));
// Reopen; a click outside the popup dismisses it (standard menu behaviour).
await page.locator('.widget-surface.w-dock .w-tree-row').first().click({ button: 'right' });
await page.waitForFunction(() => (window.fresh.scene.regions.widgets || []).some(w => w.kind === 'floatingModal'), { timeout: 8000 }).catch(() => {});
await page.locator('.modal-scrim.scrim-clear').click({ position: { x: 900, y: 600 } });
await page.waitForFunction(() => !(window.fresh.scene.regions.widgets || []).some(w => w.kind === 'floatingModal'), { timeout: 8000 }).catch(() => {});
check('outside-click dismisses the context menu', !(await modalSurf()));

console.log('\n[widget text field = native <input>, host-authoritative echo + caret]');
// The dock's "Search Tasks" field is a widget Text: it must render as a real
// <input> whose value/caret mirror the host TextEdit per keystroke (the spec
// value is initial-only and would lag until the plugin round-trip).
const dockSearch = page.locator('.widget-surface.w-dock input.w-text-input').first();
check('widget text field renders as a native <input>', (await dockSearch.count()) === 1);
await dockSearch.click();
await page.waitForTimeout(400);
check('clicking the field takes real DOM focus (native caret)', await page.evaluate(() =>
  document.activeElement && document.activeElement.classList.contains('w-text-input')));
let echoOk = true;
for (const ch of 'abc') {
  await page.keyboard.type(ch);
  const want = 'abc'.slice(0, 'abc'.indexOf(ch) + 1);
  const ok = await page.waitForFunction(w => document.activeElement.value === w, want, { timeout: 2000 }).then(() => true).catch(() => false);
  if (!ok) { echoOk = false; break; }
}
check('every keystroke echoes into the input (host TextEdit → instance state)', echoOk,
  JSON.stringify(await page.evaluate(() => document.activeElement.value)));
await page.keyboard.press('Home'); await page.waitForTimeout(300);
check('caret follows the host cursor (Home → 0)', (await page.evaluate(() => document.activeElement.selectionStart)) === 0);
// Clear the filter so later dock assertions see the full session list.
await page.keyboard.press('End');
for (let i = 0; i < 3; i++) await page.keyboard.press('Backspace');
await page.keyboard.press('Escape'); await page.waitForTimeout(200);

console.log('\n[dock chrome polish: no h-scroll, list fills, dropdowns float over content]');
// Regression cover for the orchestrator-dock visual fixes: the dock never
// raises a spurious horizontal scrollbar; the session tree fills the panel
// (the TUI hint-padding rows are dropped in the web); and the toolbar
// dropdowns float over the content (positioned by `layoutDockOverlays`)
// instead of reflowing the list, with a full-width option highlight and an
// outside-click dismiss.
const dockOverlay = () => page.locator('.widget-surface.w-dock > .w-col > .w-overlay');
const treeHeight = () => page.evaluate(() => {
  const t = document.querySelector('.widget-surface.w-dock .w-tree');
  return t ? Math.round(t.getBoundingClientRect().height) : -1;
});

// (1) No horizontal scrollbar: over-wide `pre` tree rows ellipsis-clip.
const dockOverflowX = await page.evaluate(() =>
  getComputedStyle(document.querySelector('.widget-surface.w-dock')).overflowX);
check('dock never scrolls horizontally (overflow-x: hidden)', dockOverflowX === 'hidden', dockOverflowX);

// (2) The session list fills the panel (no blank filler stealing the space).
const fillRatio = await page.evaluate(() => {
  const dock = document.querySelector('.widget-surface.w-dock');
  const tree = document.querySelector('.widget-surface.w-dock .w-tree');
  return tree ? tree.getBoundingClientRect().height / dock.getBoundingClientRect().height : 0;
});
check('session list fills the dock (tree ≥ 55% of dock height)', fillRatio >= 0.55, `ratio=${fillRatio.toFixed(2)}`);

// (3) New Task… dropdown floats flush under its button (never on top of it),
//     and its option highlight fills the row instead of hugging the text.
await page.locator('.widget-surface.w-dock .w-button.primary', { hasText: 'New Task' }).first().click();
await page.waitForFunction(() => !!document.querySelector('.widget-surface.w-dock > .w-col > .w-overlay'),
  { timeout: 8000 }).catch(() => {});
const newInfo = await page.evaluate(() => {
  const ov = document.querySelector('.widget-surface.w-dock > .w-col > .w-overlay');
  const btn = [...document.querySelectorAll('.widget-surface.w-dock .w-button.primary')]
    .find(b => /New Task/.test(b.textContent));
  if (!ov || !btn) return null;
  const o = ov.getBoundingClientRect(), b = btn.getBoundingClientRect();
  const opt = ov.querySelector('.w-button'), row = opt && opt.closest('.w-row');
  return {
    floats: getComputedStyle(ov).position === 'absolute',
    belowButton: o.top >= b.bottom - 2,
    optionFillsRow: !!row && opt.getBoundingClientRect().width >= row.getBoundingClientRect().width - 2,
  };
});
check('New Task dropdown floats absolutely, flush below its button', !!newInfo && newInfo.floats && newInfo.belowButton, JSON.stringify(newInfo));
check('dropdown option highlight fills the row (no half-width spacer)', !!newInfo && newInfo.optionFillsRow, JSON.stringify(newInfo));
await page.keyboard.press('Escape');
await page.waitForFunction(() => !document.querySelector('.widget-surface.w-dock > .w-col > .w-overlay'),
  { timeout: 8000 }).catch(() => {});

// (4) Move to Folder… floats over the tree without reflowing it, and an
//     outside click dismisses it (the dropdowns carry no scrim of their own).
await page.locator('.widget-surface.w-dock .w-tree-row').first().click({ button: 'right' });
await page.waitForFunction(() => (window.fresh.scene.regions.widgets || []).some(w => w.kind === 'floatingModal'),
  { timeout: 8000 }).catch(() => {});
const treeBeforeMove = await treeHeight();
await page.locator('.widget-surface.w-floatingModal .w-button', { hasText: 'Move to Folder' }).first().click();
await page.waitForFunction(() => !!document.querySelector('.widget-surface.w-dock > .w-col > .w-overlay'),
  { timeout: 8000 }).catch(() => {});
const moveInfo = await page.evaluate(() => {
  const ov = document.querySelector('.widget-surface.w-dock > .w-col > .w-overlay');
  const tree = document.querySelector('.widget-surface.w-dock .w-tree');
  const div = document.querySelector('.widget-surface.w-dock .w-divider');
  if (!ov) return null;
  const ovTop = Math.round(ov.getBoundingClientRect().top);
  // The menu is pinned to the divider at the head of the tree (a small
  // divider gap sits above the first tree row); the pre-fix bug parked it at
  // the panel top over the toolbar, which this catches.
  return {
    floats: getComputedStyle(ov).position === 'absolute',
    atTreeHead: !!div && Math.abs(ovTop - Math.round(div.getBoundingClientRect().top)) <= 3
      && (!tree || ovTop >= Math.round(tree.getBoundingClientRect().top) - 16),
    treeH: tree ? Math.round(tree.getBoundingClientRect().height) : -1,
  };
});
check('Move to Folder floats absolutely at the head of the tree', !!moveInfo && moveInfo.floats && moveInfo.atTreeHead, JSON.stringify(moveInfo));
check('Move to Folder does not collapse the session list', !!moveInfo && moveInfo.treeH >= treeBeforeMove * 0.7, `tree ${treeBeforeMove} -> ${moveInfo && moveInfo.treeH}`);
await page.screenshot({ path: `${SHOTS}/30-dock-move-dropdown.png` });
await page.mouse.click(1000, 700); // click in the editor, outside the dock
await page.waitForFunction(() => !document.querySelector('.widget-surface.w-dock > .w-col > .w-overlay'),
  { timeout: 8000 }).catch(() => {});
check('outside-click dismisses the Move to Folder dropdown', (await dockOverlay().count()) === 0);
await page.keyboard.press('Escape'); await page.waitForTimeout(150);

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
// Size: the modal should claim ~80% of the viewport (with the old capped size
// only as a small-window floor), not a fixed 960×640 box.
const setSize = await page.evaluate(() => {
  const r = document.querySelector('.settings-modal').getBoundingClientRect();
  return { pw: +(100 * r.width / window.innerWidth).toFixed(1), ph: +(100 * r.height / window.innerHeight).toFixed(1) };
});
check('settings modal uses ~80% of the viewport', setSize.pw >= 79 && setSize.ph >= 79, JSON.stringify(setSize));
// Map/list parity with the TUI: entries render as a visible key column plus
// the domain display value (e.g. Languages: "asm │ Assembly") under a dimmed
// Name│<column> header — never the raw JSON blob — and auto-managed (noAdd)
// maps hide the add row exactly like the TUI.
const langRows = await page.evaluate(() => {
  const item = [...document.querySelectorAll('.settings-modal .set-items > .set-item')]
    .find(r => r.textContent.startsWith('Languages'));
  if (!item) return null;
  const head = item.querySelector('.set-list-head');
  const row = item.querySelector('.set-list-row:not(.set-list-head)');
  const label = row && row.querySelector('.set-list-label');
  const sub = row && row.querySelector('.set-list-sub');
  const model = window.fresh.scene.regions.settings.items.find(i => i.name === 'Languages');
  return { header: head && head.textContent, key: label && label.textContent,
           keyWidth: label ? label.getBoundingClientRect().width : 0,
           display: sub && sub.textContent, noAdd: model.control.noAdd,
           addRows: item.querySelectorAll('.set-list-add').length };
});
check('map entries render key + TUI display value (visible key, no raw JSON)',
  !!langRows && langRows.keyWidth > 10 && !!langRows.display && !langRows.display.startsWith('{'),
  JSON.stringify(langRows));
check('map shows the Name│<column> header; add row follows the noAdd projection',
  !!langRows && /Name/.test(langRows.header || '') && langRows.addRows === (langRows.noAdd ? 0 : 1),
  JSON.stringify(langRows));
await page.keyboard.press('Escape'); await page.waitForTimeout(120); await page.keyboard.press('Escape'); await page.waitForTimeout(150);

console.log('\n[Settings search jump scrolls the selection into view]');
await page.request.post(URL + '/action', { data: { action: 'open_settings' } });
await page.waitForFunction(() => !!window.fresh.scene.regions.settings, { timeout: 8000 }).catch(() => {});
await page.waitForTimeout(250);
await page.keyboard.press('/'); await page.waitForTimeout(150);
await page.keyboard.type('scroll', { delay: 30 }); await page.waitForTimeout(250);
check('"/" enters search mode with results', await page.evaluate(() => {
  const s = window.fresh.scene.regions.settings; return s.searchActive && s.searchResults.length >= 2; }));
await page.keyboard.press('ArrowDown'); await page.keyboard.press('ArrowDown'); await page.waitForTimeout(150);
await page.keyboard.press('Enter'); await page.waitForTimeout(400);
const setScroll = await page.evaluate(() => {
  const list = document.querySelector('.settings-modal .set-items');
  const sel = document.querySelector('.settings-modal .set-item.sel');
  if (!list || !sel) return { ok: false, why: 'missing ' + (!list ? '.set-items' : '.set-item.sel') };
  const lr = list.getBoundingClientRect(), sr = sel.getBoundingClientRect();
  return { ok: sr.top >= lr.top - 1 && sr.bottom <= lr.bottom + 1,
           selTop: Math.round(sr.top), listTop: Math.round(lr.top), listBottom: Math.round(lr.bottom), scrollTop: list.scrollTop };
});
check('Enter on a search result scrolls the selected item into view', setScroll.ok, JSON.stringify(setScroll));
await page.screenshot({ path: `${SHOTS}/31-settings-search-jump.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(150);

console.log('\n[Settings map entry rows: mouse + keyboard, TUI-parity interactions]');
// Single click focuses a row (highlight + [Enter to edit] hint, no dialog);
// Enter / double-click open the entry edit dialog; arrow keys walk the
// entries with the focused row kept highlighted and scrolled into view.
await page.request.post(URL + '/action', { data: { action: 'open_settings' } });
await page.waitForFunction(() => !!window.fresh.scene.regions.settings, { timeout: 8000 }).catch(() => {});
await page.waitForTimeout(250);
const langIdx = await page.evaluate(() => [...document.querySelectorAll('.set-items > .set-item')]
  .findIndex(r => (r.querySelector('.set-name') || {}).textContent === 'Languages'));
const langRowsLoc = page.locator('.set-items > .set-item').nth(langIdx).locator('.set-list-row:not(.set-list-head)');
await langRowsLoc.nth(2).click(); await page.waitForTimeout(250);
const rowFocus = await page.evaluate(() => {
  const it = window.fresh.scene.regions.settings.items.find(i => i.name === 'Languages');
  const sel = document.querySelector('.set-items .set-list-row.sel');
  return { focused: it.control.focused, dialog: !!window.fresh.scene.regions.settings.entryDialog,
           sel: sel && sel.textContent.slice(0, 20), hint: !!(sel && sel.querySelector('.set-list-hint')) };
});
check('clicking a map entry row focuses + highlights it (no dialog)',
  rowFocus.focused === 2 && !rowFocus.dialog && !!rowFocus.sel && rowFocus.hint, JSON.stringify(rowFocus));
await page.keyboard.press('Enter'); await page.waitForTimeout(350);
check('Enter opens the focused entry edit dialog',
  await page.evaluate(() => !!window.fresh.scene.regions.settings.entryDialog));
await page.keyboard.press('Escape'); await page.waitForTimeout(250);
await langRowsLoc.nth(3).dblclick(); await page.waitForTimeout(350);
check('double-click opens the entry edit dialog',
  await page.evaluate(() => !!window.fresh.scene.regions.settings.entryDialog));
await page.keyboard.press('Escape'); await page.waitForTimeout(250);
await langRowsLoc.nth(2).click(); await page.waitForTimeout(200);
for (let i = 0; i < 40; i++) { await page.keyboard.press('ArrowDown'); await page.waitForTimeout(30); }
await page.waitForTimeout(250);
const entryWalk = await page.evaluate(() => {
  const list = document.querySelector('.settings-modal .set-items');
  const sel = document.querySelector('.set-items .set-list-row.sel');
  if (!sel) return { sel: false };
  const lr = list.getBoundingClientRect(), sr = sel.getBoundingClientRect();
  return { visible: sr.top >= lr.top - 1 && sr.bottom <= lr.bottom + 1, scrollTop: list.scrollTop };
});
check('keyboard walk through entries keeps the focused row visible', !!entryWalk.visible && entryWalk.scrollTop > 0, JSON.stringify(entryWalk));
await page.screenshot({ path: `${SHOTS}/32-settings-entry-rows.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(150);

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
const paneM = await page.evaluate(() => window.fresh.metrics);
await page.mouse.click(gx(paneM, paneRect.x + Math.floor(paneRect.w / 2)), gy(paneM, paneRect.y + Math.floor(paneRect.h / 2)));
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

console.log('\n[shared-view mirroring: multiple clients are all live at once]');
// Opening the editor again (a second tab, another device, or the page you load
// after a server restart) JOINS the shared session — both clients mirror the
// SAME editor, neither is rejected. This is what stops a stale/half-open
// background tab from locking out a new page (the old model bounced the newcomer
// with 409, so a zombie held the single slot until a server restart).
const secondJoined = await page.evaluate(() => new Promise(res => {
  const w = new WebSocket((location.protocol === 'https:' ? 'wss' : 'ws') + '://' + location.host + '/ws');
  window.__mirror = w;
  window.__mirrorFrames = [];
  w.onmessage = ev => { try { const m = JSON.parse(ev.data); window.__mirrorFrames.push(m.type);
    if (m.type === 'hello') res('hello'); } catch (_) {} };
  w.onclose = () => res('closed');
  setTimeout(() => res('timeout'), 3000);
}));
check('a second WebSocket JOINS and gets its own hello (mirroring, no 409)', secondJoined === 'hello', secondJoined);
check('the page socket stays open alongside the second (both live)', await page.evaluate(() => window.fresh.wsOpen));
// Input from the SECOND client mutates the shared editor, and BOTH clients get
// the resulting pushed frame (the scene is built once and broadcast to all).
const pFrames0 = await page.evaluate(() => window.fresh.frames);
const mFrames0 = await page.evaluate(() => window.__mirrorFrames.length);
await page.evaluate(() => window.__mirror.send(JSON.stringify({ type: 'key', key: 'Z' })));
check("the page receives a frame from the OTHER client's input (shared editor)",
  await page.waitForFunction(f0 => window.fresh.frames > f0, pFrames0, { timeout: 5000 }).then(() => true).catch(() => false));
check('the second client also receives broadcast frames',
  await page.waitForFunction(f0 => window.__mirrorFrames.length > f0, mFrames0, { timeout: 5000 }).then(() => true).catch(() => false));
// The page's own input still round-trips too (it remained fully live).
const pFrames1 = await page.evaluate(() => window.fresh.frames);
await page.keyboard.type('Q');
check('the page socket is still functional (its own input round-trips)',
  await page.waitForFunction(f0 => window.fresh.frames > f0, pFrames1, { timeout: 5000 }).then(() => true).catch(() => false));
// Min-viewport resize policy: the shared grid is fit to the SMALLEST client so
// it fits every window. Have the mirror report a narrower viewport and the
// page's rendered width must shrink to match (bigger windows letterbox). Keep
// the mirror's rows equal to the page's so only width changes — leaving the
// vertical layout (and the later zoom hit-test) undisturbed.
const sBig = await scene(page); const wBig = sBig.w;
await page.evaluate(d => window.__mirror.send(JSON.stringify({ type: 'resize', cols: Math.max(20, d.w - 20), rows: d.h })), { w: wBig, h: sBig.h });
check('a narrower mirror shrinks the shared grid to fit it (min viewport wins)',
  await page.waitForFunction(w0 => window.fresh.scene.w < w0, wBig, { timeout: 5000 }).then(() => true).catch(() => false),
  `w was ${wBig}`);
// Close the extra client so the rest of the suite runs against the page alone;
// the grid grows back once the small viewport that constrained it is gone.
await page.evaluate(() => window.__mirror.close());
check('closing the small mirror grows the grid back (constraint lifted)',
  await page.waitForFunction(w0 => window.fresh.scene.w >= w0, wBig, { timeout: 5000 }).then(() => true).catch(() => false),
  `w should return to ${wBig}`);
check('closing one mirror leaves the page live (session survives a client leaving)',
  await page.evaluate(() => window.fresh.wsOpen));

console.log('\n[per-region DOM patching: a typing frame rebuilds only its regions]');
// Have the file explorer open as the heavyweight *unrelated* region.
if (!(await scene(page)).regions.fileExplorer) {
  await page.keyboard.press('Control+b');
  await page.waitForFunction(() => !!window.fresh.scene.regions.fileExplorer, { timeout: 8000 }).catch(() => {});
}
// Stamp the live explorer DOM node from the test: any rebuild of its region
// container would replace the element and lose the stamp.
const stamped = await page.evaluate(() => {
  const el = document.querySelector('[data-region="fileExplorer"] .fileexplorer');
  if (!el) return false; el.dataset.stamp = 'untouched'; return true;
});
const mtr0 = await page.evaluate(() => window.fresh.metrics);
const patchRect = (await scene(page)).regions.panes[0].content;
await page.mouse.click(gx(mtr0, patchRect.x + Math.floor(patchRect.w / 2)), gy(mtr0, patchRect.y + Math.floor(patchRect.h / 2)));
await page.waitForTimeout(300);
const seqP0 = await page.evaluate(() => window.fresh.seq);
await page.keyboard.type('R');
await page.waitForFunction(s0 => window.fresh.seq > s0, seqP0, { timeout: 5000 }).catch(() => {});
const rr = await page.evaluate(() => window.fresh.renderedRegions);
check('typing frame rebuilt the pane region (panes.N in renderedRegions)', rr.some(r => /^panes(\.|$)/.test(r)), JSON.stringify(rr));
check('typing frame did NOT rebuild the file explorer region', !rr.includes('fileExplorer'), JSON.stringify(rr));
check('explorer DOM node survived the typing frame (same element, stamp intact)',
  stamped && await page.evaluate(() => document.querySelector('[data-region="fileExplorer"] .fileexplorer')?.dataset.stamp === 'untouched'));

console.log('\n[measured metrics + app zoom (frontend-owned Ctrl+= / Ctrl+0)]');
const m0 = await page.evaluate(() => window.fresh.metrics);
const w0 = (await scene(page)).w;
await page.keyboard.press('Control+=');
await page.waitForFunction(w => window.fresh.scene.w < w, w0, { timeout: 5000 }).catch(() => {});
const m1 = await page.evaluate(() => window.fresh.metrics);
const w1 = (await scene(page)).w;
check('Ctrl+= raises the app zoom (frontend-owned, never forwarded)', m0.zoom === 1 && m1.zoom > 1, JSON.stringify([m0, m1]));
check('zoom re-measures the grid unit (CW/CH grew with the font)', m1.cw > m0.cw && m1.ch > m0.ch && m1.font > m0.font, JSON.stringify([m0, m1]));
check('the editor re-fit the grid to the bigger cells (scene.w shrank)', w1 < w0, `w ${w0}->${w1}`);
await page.screenshot({ path: `${SHOTS}/31-zoomed-in.png` });
// Hit-testing under zoom: click a known cell using the CURRENT metrics and the
// cursor must land there (cellAt divides by the live CW/CH).
const zp = (await scene(page)).regions.panes[0];
const zCol = zp.content.x + (zp.gutterWidth || 0) + 1, zRow = zp.content.y + 2;
await page.mouse.click(gx(m1, zCol + 0.5), gy(m1, zRow + 0.5));
await page.waitForTimeout(400);
const zc = (await scene(page)).regions.cursor;
check('a buffer click still lands on the right cell while zoomed', !!zc && zc.y === zRow && Math.abs(zc.x - zCol) <= 2, JSON.stringify({ zc, zCol, zRow }));
await page.keyboard.press('Control+0');
await page.waitForFunction(() => window.fresh.metrics.zoom === 1, { timeout: 5000 }).catch(() => {});
const m2 = await page.evaluate(() => window.fresh.metrics);
check('Ctrl+0 resets zoom and restores the measured base metrics', m2.zoom === 1 && m2.cw === m0.cw && m2.ch === m0.ch, JSON.stringify(m2));
await page.waitForTimeout(400);   // let the reset resize round-trip settle

console.log('\n[TUI-parity placement: dropdown flush under the menu bar; palette bottom-sheet + centered-modal modes]');
// The dropdown panel is placed on the pipeline's full bordered box, whose top
// edge is the row directly under the menu bar — no border-row gap.
await page.locator('.menubar .menu', { hasText: 'File' }).first().click();
await page.waitForTimeout(250);
const mbBox = await page.locator('.menubar').boundingBox();
const ddBox = await page.locator('.dropdown').first().boundingBox();
check('dropdown panel sits flush under the menu bar (no row gap)',
  !!ddBox && Math.abs(ddBox.y - (mbBox.y + mbBox.height)) <= 2, `gap=${ddBox && (ddBox.y - (mbBox.y + mbBox.height))}px`);
await page.keyboard.press('Escape'); await page.waitForTimeout(150);

// Core regression (suggestions-box Clear gate): opening the palette must NOT
// blank the buffer cells beneath it. The suggestions Clear used to punch a
// hole at the bottom of the buffer that only the bottom sheet happened to
// cover — a centered modal exposed it as a missing strip. Assert the pane
// keeps at least as many non-blank rows once the palette is open.
const nonBlankRows = s => s.regions.panes.reduce(
  (n, p) => n + p.cells.filter(row => row.some(x => x.t && x.t.trim())).length, 0);
await page.keyboard.press('Escape'); await page.waitForTimeout(150);
const rowsBefore = nonBlankRows(await scene(page));
await page.locator('body').click();
await page.keyboard.press('Control+p');
await page.waitForFunction(() => !!window.fresh.scene.regions.palette, { timeout: 5000 }).catch(() => {});
await page.waitForTimeout(200);
check('opening the palette does not blank the buffer cells beneath it (no bottom hole)',
  nonBlankRows(await scene(page)) >= rowsBefore,
  `before=${rowsBefore} after=${nonBlankRows(await scene(page))}`);
await page.keyboard.press('Escape'); await page.waitForTimeout(150);

// Bottom-sheet placement: card bottom at the cell grid's bottom edge, input bar
// UNDER the list (the terminal's order), and no scrim.
await page.evaluate(() => window.fresh.setPaletteCentered(false));
await page.locator('body').click();
await page.keyboard.press('Control+p');
await page.waitForFunction(() => !!window.fresh.scene.regions.palette, { timeout: 5000 }).catch(() => {});
await page.waitForTimeout(200);
check('bottom-sheet placement has no scrim', (await page.locator('.modal-scrim').count()) === 0);
const palBox = await page.locator('.palette').boundingBox();
const gridBottom = await page.evaluate(() => { const m = window.fresh.metrics; return m.ay + window.fresh.scene.h * m.ch; });
check('palette hugs the bottom of the cell grid (TUI placement)',
  !!palBox && Math.abs((palBox.y + palBox.height) - gridBottom) <= 2, `bottom=${palBox && (palBox.y + palBox.height)} grid=${gridBottom}`);
check('palette input bar sits BELOW the suggestion list (terminal order)',
  await page.evaluate(() => {
    const bar = document.querySelector('.palette .pinput'), list = document.querySelector('.palette .plist');
    return !!bar && !!list && bar.getBoundingClientRect().top >= list.getBoundingClientRect().bottom - 1;
  }));
await page.keyboard.press('Escape'); await page.waitForTimeout(150);

// Centered-modal placement: a `.modal-scrim` backdrop, the card centered in the
// viewport (not hugging the bottom), and the input bar ABOVE the list — the
// familiar command-palette order.
await page.evaluate(() => window.fresh.setPaletteCentered(true));
await page.locator('body').click();
await page.keyboard.press('Control+p');
await page.waitForFunction(() => !!window.fresh.scene.regions.palette, { timeout: 5000 }).catch(() => {});
await page.waitForTimeout(200);
check('centered placement adds a .modal-scrim backdrop', (await page.locator('.modal-scrim').count()) >= 1);
check('centered palette card carries the .centered class', (await page.locator('.palette.centered').count()) >= 1);
const cBox = await page.locator('.palette').boundingBox();
const vp = page.viewportSize();
check('centered palette is horizontally centered in the viewport',
  !!cBox && Math.abs((cBox.x + cBox.width / 2) - vp.width / 2) <= Math.max(10, vp.width * 0.06),
  `mid=${cBox && (cBox.x + cBox.width / 2)} vpmid=${vp.width / 2}`);
check('centered palette does NOT hug the grid bottom',
  !!cBox && (cBox.y + cBox.height) < gridBottom - 20, `bottom=${cBox && (cBox.y + cBox.height)} grid=${gridBottom}`);
check('centered palette input bar sits ABOVE the suggestion list (modal order)',
  await page.evaluate(() => {
    const bar = document.querySelector('.palette .pinput'), list = document.querySelector('.palette .plist');
    return !!bar && !!list && bar.getBoundingClientRect().bottom <= list.getBoundingClientRect().top + 1;
  }));
await page.screenshot({ path: `${SHOTS}/23b-centered-palette.png` });
// Click-away: tapping the scrim (well outside the centered card) dismisses the
// palette via Escape through the real editor.
await page.locator('.modal-scrim').click({ position: { x: 6, y: 6 } });
await page.waitForTimeout(200);
check('clicking the scrim closed the palette', !(await scene(page)).regions.palette);

console.log('\n[wave animation: any key/mouse dismisses it and is consumed (TUI parity)]');
// The interactive wave (also the screensaver) repaints every tick, so frames
// keep flowing while it runs and stop once dismissed — that plus the scene's
// poll.active hint are the observables (wave state itself isn't in the scene).
const wavePane = () => page.evaluate(() => window.fresh.scene.regions.panes[0].cells.map(r => r.map(x => x.t).join('')).join('\n'));
const paneBeforeWave = await wavePane();
await page.request.post(URL + '/action', { data: { action: 'trigger_wave_animation' } });
await page.waitForFunction(() => window.fresh.scene.regions.poll.active, { timeout: 5000 }).catch(() => {});
check('wave: poll.active while the animation runs', await page.evaluate(() => window.fresh.scene.regions.poll.active));
const wf0 = await page.evaluate(() => window.fresh.frames);
await page.waitForTimeout(700);
const wf1 = await page.evaluate(() => window.fresh.frames);
check('wave: animation frames keep arriving', wf1 - wf0 >= 3, `frames ${wf0}->${wf1}`);
// A keypress dismisses the wave and is CONSUMED — 'x' must NOT reach the buffer.
await page.keyboard.press('x');
await page.waitForFunction(() => !window.fresh.scene.regions.poll.active, { timeout: 5000 }).catch(() => {});
check('wave: a keypress dismisses it (poll.active back off)', !(await page.evaluate(() => window.fresh.scene.regions.poll.active)));
await page.waitForTimeout(400);   // let the dismissal frame(s) settle
const wf2 = await page.evaluate(() => window.fresh.frames);
await page.waitForTimeout(1200);
const wf3 = await page.evaluate(() => window.fresh.frames);
check('wave: frame flow stops after dismissal', wf3 - wf2 <= 3, `frames ${wf2}->${wf3}`);
check('wave: the dismissing keypress was consumed (no \'x\' typed, cells restored)',
  (await wavePane()) === paneBeforeWave);
// Re-trigger: a bare mouse move (no button) over the buffer dismisses it too —
// plain motion is forwarded as `moved` events (de-duped per cell crossing).
await page.request.post(URL + '/action', { data: { action: 'trigger_wave_animation' } });
await page.waitForFunction(() => window.fresh.scene.regions.poll.active, { timeout: 5000 }).catch(() => {});
const wp = (await scene(page)).regions.panes[0].content;
const wm = await page.evaluate(() => window.fresh.metrics);
await page.mouse.move(gx(wm, wp.x + 4), gy(wm, wp.y + 4));
await page.mouse.move(gx(wm, wp.x + 8), gy(wm, wp.y + 6));
await page.waitForFunction(() => !window.fresh.scene.regions.poll.active, { timeout: 5000 }).catch(() => {});
check('wave: a bare mouse move dismisses it', !(await page.evaluate(() => window.fresh.scene.regions.poll.active)));
check('wave: cells restored after mouse-move dismissal', (await wavePane()) === paneBeforeWave);

console.log('\n[idle mouse motion: moved events do not spam frames]');
// Non-drag motion is forwarded to the editor, but the server only pushes on
// scene change — sweeping the pointer over an unchanged buffer must not
// produce a frame stream.
await page.waitForTimeout(400);
const mf0 = await page.evaluate(() => window.fresh.frames);
for (let i = 0; i < 12; i++) await page.mouse.move(gx(wm, wp.x + 10 + i), gy(wm, wp.y + 8 + (i % 3)));
await page.waitForTimeout(800);
const mf1 = await page.evaluate(() => window.fresh.frames);
check('idle mousemove over an unchanged buffer pushes (almost) no frames', mf1 - mf0 <= 3, `frames ${mf0}->${mf1}`);

console.log('\n[Open File prompt: input line surfaced at the bottom (cell browser stays in the pane)]');
// Action::Open starts an OpenFile prompt whose file browser is drawn into the
// PANE cells; the prompt's input line must still project so the web has a path
// box to type into (the TUI draws it on the bottom prompt row).
await page.keyboard.press('Escape'); await page.waitForTimeout(120);
await page.request.post(URL + '/action', { data: { action: 'open' } });
await page.waitForFunction(() => !!window.fresh.scene.regions.palette, { timeout: 5000 }).catch(() => {});
await page.keyboard.type('src/');
await page.waitForFunction(() => (window.fresh.scene.regions.palette || {}).query === 'src/', { timeout: 5000 }).catch(() => {});
const op = (await scene(page)).regions.palette;
check('Open File projects a palette (scene.regions.palette non-null)', !!op && op.promptType === 'openfile', JSON.stringify(op && { q: op.query, t: op.promptType }));
check('Open File input bar shows the typed path', !!op && op.query === 'src/', op && op.query);
check('Open File has NO native suggestion list (browser is in the pane cells)', !!op && op.listRect == null && op.outerRect == null, JSON.stringify(op && { l: op.listRect, o: op.outerRect }));
check('Open File renders a native input-only bar', (await page.locator('.palette.input-only .pinput').count()) >= 1);
check('Open File does NOT render a suggestion list', (await page.locator('.palette .plist').count()) === 0);
const opBox = await page.locator('.palette').boundingBox();
const opGridBottom = await page.evaluate(() => { const m = window.fresh.metrics; return m.ay + window.fresh.scene.h * m.ch; });
check('Open File input bar hugs the bottom of the cell grid (TUI prompt row)', !!opBox && Math.abs((opBox.y + opBox.height) - opGridBottom) <= 3, `bottom=${opBox && (opBox.y + opBox.height)} grid=${opGridBottom}`);
await page.screenshot({ path: `${SHOTS}/33-openfile-prompt.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(150);
check('Escape closed the Open File prompt', !(await scene(page)).regions.palette);
// Regression: the normal command palette (Ctrl+P) still renders its list.
await page.locator('body').click();
await page.keyboard.press('Control+p');
await page.waitForFunction(() => !!window.fresh.scene.regions.palette, { timeout: 5000 }).catch(() => {});
check('command palette (Ctrl+P) still renders its suggestion list (no regression)', (await page.locator('.palette .plist .prow').count()) >= 1);
await page.keyboard.press('Escape'); await page.waitForTimeout(150);

console.log('\n[New Workspace dialog is modal: a scrim dims + blocks the dock behind it]');
if (!((await scene(page)).regions.widgets || []).some(w => w.kind === 'dock')) {
  await page.request.post(URL + '/action', { data: { action: 'orchestrator_dock_toggle' } });
  await page.waitForFunction(() => (window.fresh.scene.regions.widgets || []).some(w => w.kind === 'dock'), { timeout: 8000 }).catch(() => {});
}
await page.waitForTimeout(200);
check('dock alone (no modal) has zero modal-scrims', (await page.locator('.modal-scrim').count()) === 0);
// "New Task… ▾" opens a create dropdown first (dock redesign); pick the
// "New Task…" option (rendered as "  New Task…" / "● New Task…") to open
// the form.
await page.locator('.widget-surface.w-dock .w-button', { hasText: 'New Task' }).first().click();
await page.waitForTimeout(300);
await page.locator('.widget-surface.w-dock .w-button', { hasText: /New Task…$/ }).first().click();
await page.waitForFunction(() => (window.fresh.scene.regions.widgets || []).some(w => w.kind === 'floatingModal'), { timeout: 8000 }).catch(() => {});
await page.waitForTimeout(200);
check('New Workspace dialog is a floatingModal', ((await scene(page)).regions.widgets || []).some(w => w.kind === 'floatingModal'));
check('exactly one modal-scrim behind the floatingModal', (await page.locator('.modal-scrim').count()) === 1);
const dockSel = async () => { const d = ((await scene(page)).regions.widgets || []).find(w => w.kind === 'dock'); return d && d.instances && d.instances.sessions ? d.instances.sessions.selectedIndex : null; };
// A freshly (re)opened dock reports selectedIndex -1 until an async probe
// repaint pins it to the highlighted session (refreshOpenDialog re-pins on
// every repaint) — wait for the pin so the scrim check below isn't racing it.
await page.waitForFunction(() => { const d = (window.fresh.scene.regions.widgets || []).find(w => w.kind === 'dock');
  return d && d.instances && d.instances.sessions && d.instances.sessions.selectedIndex >= 0; }, { timeout: 8000 }).catch(() => {});
const sel0 = await dockSel();
const cardBox = await page.locator('.widget-surface.w-dock .w-tree-row').first().boundingBox();
if (cardBox) await page.mouse.click(cardBox.x + cardBox.width / 2, cardBox.y + cardBox.height / 2);
await page.waitForTimeout(300);
const sel1 = await dockSel();
check('a click over the dock is eaten by the scrim (dock selection unchanged, modal still open)',
  sel1 === sel0 && ((await scene(page)).regions.widgets || []).some(w => w.kind === 'floatingModal'), `sel ${sel0}->${sel1}`);
await page.screenshot({ path: `${SHOTS}/34-new-workspace-modal.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(250);
check('Escape closed the New Workspace dialog', !((await scene(page)).regions.widgets || []).some(w => w.kind === 'floatingModal'));
check('scrim removed once the modal closed', (await page.locator('.modal-scrim').count()) === 0);
// Close the dock again: the sections below assert on status-bar text, and
// with the COSMOS bezel already costing ~13 columns the dock's width pushes
// long status messages ("… read only (Ctrl+Space …)") into truncation.
await page.request.post(URL + '/action', { data: { action: 'orchestrator_dock_toggle' } });
await page.waitForFunction(() => !(window.fresh.scene.regions.widgets || []).some(w => w.kind === 'dock'), { timeout: 8000 }).catch(() => {});

console.log('\n[menu submenu panel sits edge-to-edge with its parent (no overlap seam)]');
await page.keyboard.press('Escape'); await page.waitForTimeout(120);
await page.locator('.menubar .menu', { hasText: 'View' }).first().click();
await page.waitForTimeout(300);
await page.locator('.mitem', { hasText: 'Keybinding Style' }).first().hover();
await page.waitForTimeout(400);
const ddPanels = await page.locator('.dropdown').evaluateAll(els => els.map(e => { const r = e.getBoundingClientRect(); return { x: r.x, right: r.right, submenu: e.classList.contains('submenu') }; }));
const parentPanel = ddPanels.find(p => !p.submenu), subPanel = ddPanels.find(p => p.submenu);
check('View ▸ Keybinding Style expanded a submenu backing panel', !!subPanel && !!parentPanel, JSON.stringify(ddPanels));
check('submenu panel left edge >= parent panel right edge (no overlap)',
  !!subPanel && !!parentPanel && subPanel.x >= parentPanel.right - 0.5, JSON.stringify({ subLeft: subPanel && subPanel.x, parentRight: parentPanel && parentPanel.right }));
await page.screenshot({ path: `${SHOTS}/35-submenu-edge.png` });
await page.keyboard.press('Escape'); await page.waitForTimeout(150);

console.log('\n[terminal text selection: drag → scrollback selection → editor copy]');
// Open a real PTY terminal, echo a marker, and drag across the echoed row.
// A drag on the LIVE grid must drop the split into read-only scrollback
// (pixel-identical view) and build a real editor selection there; Ctrl+C
// then copies through the editor clipboard (scene.clipboard sync) AND
// resumes the live terminal — the copy completes the gesture, so the split
// must not stay stuck in scrollback. A bare click must NOT leave live mode
// (click-to-focus-and-type).
await page.request.post(URL + '/action', { data: { action: 'open_terminal' } });
await page.waitForFunction(() => window.fresh.scene.regions.statusbar.segments.some(s => /Terminal \d+ opened/.test(s.text || s.label || '')), null, { timeout: 15000 }).catch(() => {});
await page.waitForTimeout(800);
await page.keyboard.type('echo web-sel-MARKER-42');
await page.keyboard.press('Enter');
await page.waitForFunction(() => window.fresh.scene.regions.panes.some(p =>
  p.cells.some(r => r.map(x => x.t).join('').startsWith('web-sel-MARKER-42'))), null, { timeout: 15000 }).catch(() => {});
const tmet = await page.evaluate(() => window.fresh.metrics);
const ts = await scene(page);
const tpi = ts.regions.panes.findIndex(p => p.cells.some(r => r.map(x => x.t).join('').startsWith('web-sel-MARKER-42')));
const tp = ts.regions.panes[tpi];
check('terminal echoed the marker into the live grid', !!tp);
const mrow = tp ? tp.cells.findIndex(r => r.map(x => x.t).join('').startsWith('web-sel-MARKER-42')) : -1;
if (tp) {
  const my = gy(tmet, tp.content.y + mrow + 0.5);
  await page.mouse.move(gx(tmet, tp.content.x + 0.2), my);
  await page.mouse.down();
  for (let i = 1; i <= 6; i++) await page.mouse.move(gx(tmet, tp.content.x + 0.2 + i * 3), my);
  await page.mouse.up();
  await page.waitForTimeout(400);
}
const tStatus = s => s.regions.statusbar.segments.map(x => x.text || x.label || '').join(' | ');
let sd = await scene(page);
check('drag on the live grid drops the split into read-only scrollback', /read only/.test(tStatus(sd)), tStatus(sd));
const selRun = tp ? (sd.regions.panes[tpi].cells[mrow] || []).filter(c => c.bg).map(c => c.t).join('') : '';
check('drag built a real editor selection over the terminal text', selRun.includes('web-sel-MARKER'), JSON.stringify(selRun));
await page.screenshot({ path: `${SHOTS}/36-terminal-drag-select.png` });
await page.keyboard.press('Control+c');
await page.waitForFunction(() => window.fresh.scene.clipboard && /web-sel-MARKER/.test(window.fresh.scene.clipboard.text || ''), null, { timeout: 5000 }).catch(() => {});
sd = await scene(page);
check('Ctrl+C copies the terminal selection through the editor clipboard',
  !!sd.clipboard && (sd.clipboard.text || '').startsWith('web-sel-MARKER-42'), JSON.stringify(sd.clipboard));
check('copying the selection auto-resumes the live terminal', /terminal resumed/.test(tStatus(sd)), tStatus(sd));
// Ctrl+Space still round-trips: into scrollback and back to live.
await page.keyboard.press('Control+ ');
await page.waitForTimeout(300);
sd = await scene(page);
check('Ctrl+Space still drops the live terminal into scrollback', /read only/.test(tStatus(sd)), tStatus(sd));
await page.keyboard.press('Control+ ');
await page.waitForTimeout(300);
sd = await scene(page);
check('Ctrl+Space resumes the live terminal from scrollback', /Terminal mode enabled/.test(tStatus(sd)), tStatus(sd));
if (tp) {  // a bare click must keep the terminal live
  await page.mouse.click(gx(tmet, tp.content.x + 4), gy(tmet, tp.content.y + 1));
  await page.waitForTimeout(300);
}
sd = await scene(page);
check('a bare click keeps the terminal live (click-to-focus)', !/read only/.test(tStatus(sd)), tStatus(sd));

console.log('\n[Alt-hold native browser selection (works on any surface)]');
// Holding Alt suspends input forwarding (capture-phase guards) and re-enables
// user-select, so the BROWSER builds a real selection over any text — the
// SVG cell grid included. Ctrl+C copies it row-aware (newlines preserved).
await page.keyboard.down('Alt');
await page.waitForTimeout(120);
check('holding Alt engages native-selection mode (body.natsel + pill)',
  await page.evaluate(() => document.body.classList.contains('natsel') && document.getElementById('natsel').classList.contains('show')));
const ap = (await scene(page)).regions.panes[0];
const ay = gy(tmet, ap.content.y + (mrow >= 0 ? mrow : 1) + 0.5);
await page.mouse.move(gx(tmet, ap.content.x + 0.2), ay);
await page.mouse.down();
for (let i = 1; i <= 5; i++) await page.mouse.move(gx(tmet, ap.content.x + 0.2 + i * 4), ay);
await page.mouse.up();
await page.waitForTimeout(250);
const natText = await page.evaluate(() => window.getSelection().toString());
check('Alt+drag builds a native browser selection over the SVG grid', natText.trim().length >= 3, JSON.stringify(natText).slice(0, 60));
sd = await scene(page);
check('the terminal stayed LIVE under an Alt+drag (no forwarding, no scrollback)', !/read only/.test(tStatus(sd)), tStatus(sd));
await page.keyboard.up('Alt');
await page.waitForTimeout(150);
check('releasing Alt disengages native-selection mode', await page.evaluate(() => !document.body.classList.contains('natsel')));
await page.mouse.click(gx(tmet, ap.content.x + 2), gy(tmet, ap.content.y + 1));
await page.waitForTimeout(250);
check('an ordinary click clears the leftover native selection', (await page.evaluate(() => window.getSelection().toString())) === '');
await page.screenshot({ path: `${SHOTS}/37-alt-native-selection.png` });
// Restore: exit the shell and close the terminal tab so the sections below
// (touch page) see the original file buffer in pane 0.
await page.keyboard.type('exit');
await page.keyboard.press('Enter');
await page.waitForTimeout(600);
await page.request.post(URL + '/action', { data: { action: 'close_tab' } });
await page.waitForTimeout(600);

console.log('\n[web-UI theme system: switch chrome look without touching the buffer/editor]');
// The web theme is a FRONTEND view preference (like zoom / palette placement):
// it re-skins the native chrome and never reaches the editor. Default is Cosmos
// (the hardware-bezel shell); macOS and Compact are the added looks.
check('theme API is exposed (setWebTheme / webTheme / webThemes)', await page.evaluate(() =>
  typeof window.fresh.setWebTheme === 'function' && Array.isArray(window.fresh.webThemes) && window.fresh.webThemes.length === 5));
check('default theme is Cosmos with the hardware bezel on', await page.evaluate(() =>
  window.fresh.webTheme === 'cosmos' && document.body.classList.contains('theme-cosmos') && document.getElementById('device').classList.contains('on')));
// The buffer stays the TUI monospace stack regardless of the chrome font.
const svgFamily = () => page.evaluate(() => { const t = document.querySelector('.pane-content svg.cells'); return t ? getComputedStyle(t).fontFamily : ''; });
const cosmosBufFont = await svgFamily();
// → macOS: light title bar with traffic lights, system chrome font, no bezel.
await page.evaluate(() => window.fresh.setWebTheme('macos'));
await page.waitForTimeout(300);
check('macOS: body theme class set, bezel off, title bar shown', await page.evaluate(() =>
  document.body.classList.contains('theme-macos') && !document.getElementById('device').classList.contains('on') &&
  getComputedStyle(document.getElementById('mactitle')).display !== 'none'));
check('macOS: chrome font is proportional but the BUFFER stays monospace',
  await page.evaluate(() => /apple-system|sans-serif/i.test(getComputedStyle(document.body).fontFamily)) &&
  (await svgFamily()) === cosmosBufFont && /mono/i.test(cosmosBufFont));
// → macOS Dark: same structural chrome (title bar + traffic lights, macfam
// class), but a dark System palette (dark toolbar surface).
await page.evaluate(() => window.fresh.setWebTheme('macos-dark'));
await page.waitForTimeout(300);
check('macOS Dark: shares the macOS title bar but swaps to a dark palette', await page.evaluate(() => {
  const dark = document.body.classList.contains('theme-macos-dark') && document.body.classList.contains('macfam') &&
    getComputedStyle(document.getElementById('mactitle')).display !== 'none';
  // toolbar surface (--shell) is dark, not the light macOS value
  const shell = getComputedStyle(document.documentElement).getPropertyValue('--shell').trim();
  const m = /rgb\((\d+)/.exec(shell) || (shell[0] === '#' ? [null, parseInt(shell.slice(1, 3), 16)] : null);
  return dark && m && Number(m[1]) < 80;
}));
// → Compact: dense, no bezel, a smaller measured grid (webThemeScale < 1).
const cwCosmos = (await page.evaluate(() => window.fresh.metrics)).cw;
await page.evaluate(() => window.fresh.setWebTheme('compact'));
await page.waitForTimeout(400);
check('Compact: theme class set, bezel off, denser grid (cell width shrank)', await page.evaluate(() =>
  document.body.classList.contains('theme-compact') && !document.getElementById('device').classList.contains('on')) &&
  (await page.evaluate(() => window.fresh.metrics)).cw < cwCosmos);
// The switch is persisted as a pure view preference in localStorage.
check('theme choice persists in localStorage', await page.evaluate(() => localStorage.getItem('fresh.webtheme') === 'compact'));
// → Winamp: a SHELL theme (like Cosmos) — the hardware bezel host is on and
// dressed as the "CODE STUDIO" skin window (furniture), and its own chrome
// palette is layered inline (navy playlist selection, LCD-green accent). The
// BUFFER stays the TUI monospace stack, same as every other web theme.
await page.evaluate(() => window.fresh.setWebTheme('winamp'));
await page.waitForTimeout(400);
check('Winamp: theme class set, shell bezel ON, skin furniture built', await page.evaluate(() =>
  document.body.classList.contains('theme-winamp') && document.getElementById('device').classList.contains('on') &&
  !!document.querySelector('#device .wa-title .wa-title-text')));
check('Winamp: chrome palette layered inline (navy selection + LCD-green accent), buffer stays monospace',
  await page.evaluate(() => {
    const r = document.documentElement.style;
    return r.getPropertyValue('--sel').trim() === '#313c90' && r.getPropertyValue('--accent').trim() === '#4ef07f';
  }) && (await svgFamily()) === cosmosBufFont);
await page.screenshot({ path: `${SHOTS}/33b-theme-winamp.png` });
// The floating switcher: clicking the pill opens a 3-row menu; a row switches.
await page.locator('#themebtn').click();
await page.waitForTimeout(150);
check('theme switcher menu opens with all five themes', (await page.locator('#thememenu.open .ts-row').count()) === 5);
await page.locator('#thememenu .ts-row', { hasText: 'Cosmos' }).first().click();
await page.waitForTimeout(400);
check('picking Cosmos from the menu restores the bezel shell', await page.evaluate(() =>
  window.fresh.webTheme === 'cosmos' && document.getElementById('device').classList.contains('on')));
await page.screenshot({ path: `${SHOTS}/34-theme-cosmos.png` });

console.log('\n[embedded terminal: Ctrl+hover carries the modifier and underlines a path link]');
// Regression: the frontend used to send `moved` events WITHOUT the ctrl flag,
// so Ctrl+hover over a file path in the embedded terminal never underlined it
// (the server only computes the link highlight while CONTROL is held), even
// though Ctrl+click — whose `down` event DID carry ctrl — worked. Open a real
// terminal, print a path that resolves at the server's cwd (repo root), and
// Ctrl-move over it.
await page.keyboard.press('Escape'); await page.waitForTimeout(150);
await page.request.post(URL + '/action', { data: { action: 'open_terminal' } });
await page.waitForFunction(() => window.fresh.scene.regions.panes.some(p =>
  p.cells.some(r => r.map(x => x.t).join('').match(/[#$]/))), { timeout: 8000 }).catch(() => {});
await page.waitForTimeout(500);
// Capture outgoing WS mouse messages so we can see what the frontend emits.
await page.evaluate(() => {
  window.__mouse = [];
  const orig = WebSocket.prototype.send;
  WebSocket.prototype.send = function (data) {
    try { const o = JSON.parse(data); if (o && o.type === 'mouse') window.__mouse.push(o); } catch {}
    return orig.call(this, data);
  };
});
// Create a real file and print its ABSOLUTE path, so the link resolves
// regardless of the terminal's cwd.
const PROBE = '/tmp/fresh_link_probe.txt';
await page.keyboard.type(`touch ${PROBE} && echo "see ${PROBE}:1:1 here"`);
await page.keyboard.press('Enter');
await page.waitForFunction(() => window.fresh.scene.regions.panes.some(p =>
  p.cells.some(r => r.map(x => x.t).join('').includes('see /tmp/fresh_link_probe.txt:1:1 here'))), { timeout: 6000 }).catch(() => {});
await page.waitForTimeout(300);
// Locate the OUTPUT line's path (last occurrence; the first is the typed echo).
{
  const NEEDLE = 'fresh_link_probe.txt:1:1';
  // The web terminal grid is run-encoded (each cell is a styled run, not one
  // char), so a cell's column is the sum of the preceding runs' text lengths.
  // Locate the needle by accumulating run widths, prefer the OUTPUT line (last
  // occurrence), and report whether the runs covering it are underlined.
  const findLink = s => {
    let best = null;
    for (const pane of s.regions.panes) {
      for (let r = 0; r < pane.cells.length; r++) {
        const row = pane.cells[r];
        let text = '', starts = [], col = 0;
        for (const cell of row) { starts.push(col); text += cell.t; col += cell.t.length; }
        const i = text.indexOf(NEEDLE);
        if (i < 0) continue;
        const underlinedAt = ci => {
          for (let k = 0; k < row.length; k++) { if (ci >= starts[k] && ci < starts[k] + row[k].t.length) return !!row[k].u; }
          return false;
        };
        best = { pane, r, col: pane.content.x + i, row: pane.content.y + r,
                 underlined: [0, 2, 4, NEEDLE.length - 1].every(k => underlinedAt(i + k)) };
      }
    }
    return best;
  };
  const tm2 = await page.evaluate(() => window.fresh.metrics);
  const h = findLink(await scene(page));
  if (h) {
    await page.keyboard.down('Control');
    await page.mouse.move(gx(tm2, h.col + 2 + 0.5), gy(tm2, h.row + 0.5));
    await page.mouse.move(gx(tm2, h.col + 3 + 0.5), gy(tm2, h.row + 0.5)); // cross a cell so the de-dupe re-sends
    await page.waitForTimeout(400);
    const movedCtrl = await page.evaluate(() => (window.__mouse || []).some(m => m.kind === 'moved' && m.ctrl === true));
    check('Ctrl+move over the terminal emits a `moved` event carrying ctrl:true', movedCtrl,
      JSON.stringify(await page.evaluate(() => (window.__mouse || []).filter(m => m.kind === 'moved').slice(-2))));
    // Observe rendered output: the path span is underlined in the scene cells.
    const u = findLink(await scene(page));
    check('Ctrl+hover underlines the resolvable path in the terminal (scene cells)', !!(u && u.underlined),
      JSON.stringify(u && { row: u.row, col: u.col, underlined: u.underlined }));
    await page.keyboard.up('Control');
    await page.screenshot({ path: `${SHOTS}/35-terminal-ctrl-hover.png` });
  } else {
    check('embedded terminal printed the path link', false, `path "${NEEDLE}" not found in terminal cells`);
  }
}

check('no JS page errors', errs.length === 0, errs.join(' | '));

// ---------------------------------------------------------------------------
// Dropdown pop-over placement, macOS skin. The macOS vibrancy modal uses
// `backdrop-filter`, making it the containing block for `position:fixed`
// descendants; the Settings modal is a `transform`ed, `overflow:hidden` box
// with a scrolling item list. A dropdown's floating option list must, in both:
// open under its trigger (not offset by the modal origin), never be clipped by
// those scroll/overflow containers, and grow to fit its widest option. The
// closed pill's width must also stay stable open↔closed.
console.log('\n[dropdown pop-overs anchor under their trigger, never clip (macOS skin)]');
await page.keyboard.press('Escape'); await page.waitForTimeout(120);
// Skin is read from localStorage at boot; single-client bridge → /reset first,
// then set the skin and reload so the vibrancy modal is in play.
await page.request.post(URL + '/reset', { data: {} });
await page.evaluate(() => { try { localStorage.setItem('fresh.webtheme', 'macos'); } catch (_) {} });
await page.reload({ waitUntil: 'networkidle' });
await page.waitForFunction(() => window.fresh && window.fresh.wsOpen && window.fresh.scene && window.fresh.scene.regions.panes.length > 0, null, { timeout: 20000 });
await page.waitForTimeout(400);
check('macOS skin active (vibrancy modal in play)',
  await page.evaluate(() => document.body.classList.contains('theme-macos')));

// (a) Orchestrator "Run Agent" widget dropdown, mounted inside the
// backdrop-filter modal — the case that regressed.
await page.request.post(URL + '/action', { data: { action: 'orchestrator_run_agent' } });
await page.waitForFunction(() => document.querySelectorAll('.w-dropdown .w-dd-pill').length > 0, null, { timeout: 8000 }).catch(() => {});
const raBox = await page.locator('.w-dropdown .w-dd-pill').first().boundingBox();
if (raBox) {
  const closedW = await page.evaluate(() => document.querySelector('.w-dd-pill').getBoundingClientRect().width);
  await page.mouse.click(raBox.x + raBox.width / 2, raBox.y + raBox.height / 2);
  await page.waitForFunction(() => !!document.querySelector('.w-dd.w-dd-floating'), null, { timeout: 5000 }).catch(() => {});
  const w = await page.evaluate(() => {
    const list = document.querySelector('.w-dd.w-dd-floating'), pill = document.querySelector('.w-dd-pill');
    if (!list || !pill) return null;
    const lr = list.getBoundingClientRect(), pr = pill.getBoundingClientRect();
    return { dx: Math.round(lr.left - pr.left), dy: Math.round(lr.top - pr.bottom), openW: pr.width };
  });
  await page.screenshot({ path: `${SHOTS}/34-macos-widget-dropdown.png` });
  check('Run-Agent widget dropdown opens directly under its pill (not offset by the modal)',
    !!w && Math.abs(w.dx) <= 4 && w.dy >= -2 && w.dy <= 8, JSON.stringify(w));
  // The open (▴) and closed (▾) chevrons must be the same width so the pill
  // doesn't jump — regressed when open used the full-size ▲ vs the small ▾.
  check('closed pill width stays stable when opened (matched-size chevron)',
    !!w && Math.abs(w.openW - closedW) <= 1, JSON.stringify({ closedW: Math.round(closedW), openW: Math.round(w.openW) }));
}
await page.keyboard.press('Escape'); await page.waitForTimeout(100);
await page.keyboard.press('Escape'); await page.waitForTimeout(200);

// (b) Settings "Default Language" dropdown: a long option list far wider than
// its compact "(none)" pill, with the control flush against the dialog's right
// edge — the exact case that used to run off the edge and get clipped by the
// scroll container. It must open below the pill, grow WIDER than the pill (to
// fit option text), and sit fully inside the modal (no clipping on any side).
await page.request.post(URL + '/action', { data: { action: 'open_settings' } });
await page.waitForFunction(() => !!window.fresh.scene.regions.settings, { timeout: 8000 }).catch(() => {});
await page.waitForTimeout(300);
const langPill = page.locator('.settings-modal .set-item', { hasText: 'Default Language' }).locator('.set-pill');
await langPill.scrollIntoViewIfNeeded().catch(() => {});
const lpBox = await langPill.boundingBox();
if (lpBox) {
  await page.mouse.click(lpBox.x + lpBox.width / 2, lpBox.y + lpBox.height / 2);
  await page.waitForFunction(() => !!document.querySelector('.set-dd'), null, { timeout: 5000 }).catch(() => {});
  const sd = await page.evaluate(() => {
    const item = [...document.querySelectorAll('.settings-modal .set-item')].find(r => r.textContent.includes('Default Language'));
    // The option list is portaled to the body-level host, not inside the modal.
    const dd = document.querySelector('#fresh-popover-host .set-dd'), pill = item && item.querySelector('.set-pill');
    const modal = document.querySelector('.settings-modal');
    if (!dd || !pill || !modal) return null;
    const d = dd.getBoundingClientRect(), p = pill.getBoundingClientRect(), m = modal.getBoundingClientRect();
    return {
      below: Math.round(d.top - p.bottom), grewWiderBy: Math.round(d.width - p.width),
      insideModal: d.left >= m.left - 1 && d.right <= m.right + 1 && d.top >= m.top - 1 && d.bottom <= m.bottom + 1,
      d: { l: Math.round(d.left), r: Math.round(d.right), b: Math.round(d.bottom) }, m: { l: Math.round(m.left), r: Math.round(m.right), b: Math.round(m.bottom) },
    };
  });
  await page.screenshot({ path: `${SHOTS}/35-macos-settings-dropdown.png` });
  check('Settings "Default Language" list opens below its pill', !!sd && sd.below >= -1 && sd.below <= 8, JSON.stringify(sd));
  check('Settings dropdown grows wider than its compact pill to fit options', !!sd && sd.grewWiderBy > 8, JSON.stringify(sd));
  check('Settings dropdown is NOT clipped — fully inside the dialog', !!sd && sd.insideModal, JSON.stringify(sd));
}
await page.keyboard.press('Escape'); await page.waitForTimeout(120);

console.log('\n[touch pan/scroll on mobile (hasTouch context)]');
// The bridge is single-client: close the desktop page (frees /ws) before
// opening a touch context against the same server.
await page.close();
const tctx = await browser.newContext({ hasTouch: true, viewport: { width: 390, height: 780 }, deviceScaleFactor: 2 });
const tpage = await tctx.newPage();
const terrs = []; tpage.on('pageerror', e => terrs.push(String(e)));
// Scenario isolation: the desktop sections leave scrolled/wrapped views in
// the single server-side editor, and the swipe/tap assertions below need a
// known top-of-buffer view. /reset rebuilds a fresh editor on the same file.
await tpage.request.post(URL + '/reset', { data: {} });
await tpage.goto(URL, { waitUntil: 'networkidle' });
await tpage.waitForFunction(() => window.fresh && window.fresh.wsOpen && window.fresh.scene && window.fresh.scene.regions.panes.length > 0, null, { timeout: 20000 });
await tpage.waitForTimeout(400);
check('narrow viewport engages the mobile touch shell', await tpage.evaluate(() => document.body.classList.contains('mobile')));
// The desktop sections left the file explorer open; on mobile it is a
// full-width sheet OVER the buffer (chrome, by design) — close it so the
// swipe/tap below exercise the buffer itself.
if ((await scene(tpage)).regions.fileExplorer) {
  await tpage.request.post(URL + '/action', { data: { action: 'toggle_file_explorer' } });
  await tpage.waitForFunction(() => !window.fresh.scene.regions.fileExplorer, null, { timeout: 5000 }).catch(() => {});
}
const rowText = s => s.regions.panes[0].cells[0].map(x => x.t).join('');
const t0 = await scene(tpage);
// Swipe up on the buffer: the vertical pan must ride the wheel path (scrolldown
// at the touch cell), moving the first visible line.
await tpage.evaluate(() => {
  const m = window.fresh.metrics, p = window.fresh.scene.regions.panes[0].content;
  const x = m.ax + (p.x + Math.floor(p.w / 2)) * m.cw, y0 = m.ay + (p.y + Math.floor(p.h / 2)) * m.ch;
  const mk = (type, cy) => {
    const t = new Touch({ identifier: 7, target: document.body, clientX: x, clientY: cy });
    document.body.dispatchEvent(new TouchEvent(type, { touches: type === 'touchend' ? [] : [t], changedTouches: [t], bubbles: true, cancelable: true }));
  };
  mk('touchstart', y0);
  for (let i = 1; i <= 6; i++) mk('touchmove', y0 - i * m.ch * 1.5);
  mk('touchend', y0 - 6 * m.ch * 1.5);
});
await tpage.waitForFunction(first => window.fresh.scene.regions.panes[0].cells[0].map(x => x.t).join('') !== first,
  rowText(t0), { timeout: 5000 }).catch(() => {});
const t1 = await scene(tpage);
check('swipe-up pans the buffer (first visible row changed)', rowText(t1) !== rowText(t0), `first="${rowText(t1).slice(0, 30)}"`);
await tpage.screenshot({ path: `${SHOTS}/32-mobile-touch.png` });
// A tap must still run the existing click path (synthetic mouse events are
// left alone by the pan handlers) → tap-to-position-cursor works.
const tm = await tpage.evaluate(() => window.fresh.metrics);
const tp2 = t1.regions.panes[0];
const tRow = tp2.content.y + 3;
await tpage.touchscreen.tap(gx(tm, tp2.content.x + (tp2.gutterWidth || 0) + 1.5), gy(tm, tRow + 0.5));
await tpage.waitForFunction(r => window.fresh.scene.regions.cursor && window.fresh.scene.regions.cursor.y === r, tRow, { timeout: 5000 }).catch(() => {});
const tc = (await scene(tpage)).regions.cursor;
check('tap still positions the cursor (click path intact)', !!tc && tc.y === tRow, JSON.stringify({ tc, tRow }));
check('no JS page errors (touch page)', terrs.length === 0, terrs.join(' | '));
await tctx.close();

await browser.close();
console.log(`\n==== ${pass} passed, ${fail} failed ====`);
process.exit(fail ? 1 : 0);
