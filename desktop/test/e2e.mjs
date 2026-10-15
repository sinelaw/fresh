// End-to-end tests of the desktop app's window and editor lifecycle, driven
// through Playwright's Electron support against `electron .` and a real
// `fresh` (the same one `npm start` would run; FRESH_DESKTOP_BINARY picks
// another).
//
//   npm run test:e2e            (Linux; headless, under xvfb-run -a)
//
// Each case gets its own project directory and its own HOME, so the editor's
// workspaces, hot-exit recovery and the app's sessions.json never leak from
// one case (or from your own use of Fresh) into another.

import { _electron } from 'playwright-core';
import { execFileSync, spawn } from 'node:child_process';
import { createRequire } from 'node:module';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const desktop = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const electron = createRequire(import.meta.url)('electron');

// ── Harness ─────────────────────────────────────────────────────────────────

/** A fresh project directory with `a.txt`, and a HOME of its own. */
function sandbox() {
  // Short paths: the editor's session socket lives under the project path,
  // and Unix socket paths are limited to ~100 bytes.
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'fd-'));
  const project = path.join(root, 'p');
  const home = path.join(root, 'h');
  fs.mkdirSync(project);
  fs.mkdirSync(home);
  fs.writeFileSync(path.join(project, 'a.txt'), 'hello\n');
  const env = {
    ...process.env,
    HOME: home,
    XDG_CONFIG_HOME: path.join(home, '.config'),
    XDG_DATA_HOME: path.join(home, '.local', 'share'),
    XDG_STATE_HOME: path.join(home, '.local', 'state'),
    XDG_CACHE_HOME: path.join(home, '.cache'),
  };
  return { root, project, home, env };
}

/** The pids of editors serving `project` (their file arguments name it). */
function editorsIn(project) {
  const out = execFileSync('ps', ['-eo', 'pid=,args='], { encoding: 'utf8' });
  return out.split('\n')
    .filter((l) => /\bfresh\b.*--web=/.test(l) && l.includes(project))
    .map((l) => Number(l.trim().split(/\s+/)[0]));
}

async function until(fn, what, timeout = 15_000) {
  const end = Date.now() + timeout;
  for (;;) {
    if (await fn()) return;
    if (Date.now() > end) throw new Error(`timed out waiting for ${what}`);
    await new Promise((r) => setTimeout(r, 200));
  }
}

function launch(sb, args = ['a.txt']) {
  return _electron.launch({
    executablePath: electron,
    // The sandbox needs a setuid helper an unpacked Electron does not have.
    args: ['--no-sandbox', desktop, ...args],
    cwd: sb.project,
    env: sb.env,
  });
}

/** Launch and wait for the editor's page to render its first frame. */
async function open(sb, args) {
  const app = await launch(sb, args);
  const page = await app.firstWindow();
  await page.waitForURL(/^http:\/\/127\.0\.0\.1:\d+\/$/, { timeout: 60_000 });
  await page.waitForFunction(() => window.fresh && window.fresh.wsOpen && window.fresh.frames > 0, null, { timeout: 60_000 });
  return { app, page };
}

const windowCount = (app) => app.evaluate(({ BrowserWindow }) => BrowserWindow.getAllWindows().length);
const closeWindow = (app) => app.evaluate(({ BrowserWindow }) => BrowserWindow.getAllWindows()[0]?.close());
const confirmShown = (page) => page.evaluate(() =>
  (window.fresh.scene?.regions?.tree?.surfaces || []).some((s) => s.kind === 'confirm'));
const sceneHas = (page, text) => page.evaluate((t) => JSON.stringify(window.fresh.scene).includes(t), text);
const typeChar = (page, ch) => page.evaluate((key) => window.fresh.sendKey({
  key, code: `Key${key.toUpperCase()}`, ctrl: false, meta: false, shift: key !== key.toLowerCase(), alt: false,
}), ch);
const closed = (app, timeout = 20_000) =>
  app.waitForEvent('close', { timeout }).then(() => true, () => false);

const cases = [];
const test = (name, fn) => cases.push({ name, fn });
function assert(cond, msg) {
  if (!cond) throw new Error(msg);
}

// ── Cases ───────────────────────────────────────────────────────────────────

test('a second launch hands its files to the running window', async (sb) => {
  fs.writeFileSync(path.join(sb.project, 'b.txt'), 'second\n');
  const { app, page } = await open(sb);
  try {
    const title = await app.evaluate(({ BrowserWindow }) => BrowserWindow.getAllWindows()[0].getTitle());
    assert(title === 'p — Fresh', `window title is ${JSON.stringify(title)}`);
    const second = spawn(electron, ['--no-sandbox', desktop, 'b.txt'], { cwd: sb.project, env: sb.env, stdio: 'ignore' });
    const code = await new Promise((r) => second.on('exit', r));
    assert(code === 0, `the second instance exited with ${code}`);
    await until(() => sceneHas(page, 'b.txt'), 'b.txt to open in the first window');
    assert(await windowCount(app) === 1, 'a second window was opened');
  } finally {
    await app.evaluate(({ app: a }) => a.exit(0)).catch(() => {});
  }
});

test('links never open a window of the app', async (sb) => {
  const { app, page } = await open(sb);
  try {
    await page.evaluate(() => window.open('https://example.com'));
    await new Promise((r) => setTimeout(r, 500));
    assert(await windowCount(app) === 1, 'window.open created a window');
  } finally {
    await app.evaluate(({ app: a }) => a.exit(0)).catch(() => {});
  }
});

test('closing asks about unsaved work; cancel keeps everything, discard closes', async (sb) => {
  const { app, page } = await open(sb);
  await typeChar(page, 'Q');
  await until(() => sceneHas(page, 'Qhello'), 'the edit to show');
  const [pid] = editorsIn(sb.project);

  await closeWindow(app);
  await until(() => confirmShown(page), 'the unsaved-changes card');
  assert(await windowCount(app) === 1, 'the window closed while asking');
  await page.keyboard.press('Escape');
  await until(async () => !(await confirmShown(page)), 'the card to go');
  await new Promise((r) => setTimeout(r, 500));
  assert(await windowCount(app) === 1, 'cancelling closed the window');
  assert(editorsIn(sb.project).includes(pid), 'cancelling stopped the editor');

  await closeWindow(app);
  await until(() => confirmShown(page), 'the card, asked again');
  const gone = closed(app);
  await page.keyboard.press('d'); // Discard and Quit
  assert(await gone, 'discarding did not close the app');
  await until(() => editorsIn(sb.project).length === 0, 'the editor to exit');
  const text = fs.readFileSync(path.join(sb.project, 'a.txt'), 'utf8');
  assert(text === 'hello\n', `the file changed: ${JSON.stringify(text)}`);
});

test('closing a clean editor closes at once', async (sb) => {
  const { app } = await open(sb);
  const gone = closed(app, 15_000);
  await closeWindow(app);
  assert(await gone, 'the window did not close');
  await until(() => editorsIn(sb.project).length === 0, 'the editor to exit');
});

test('closing during startup leaves no editor behind', async (sb) => {
  const app = await launch(sb);
  await app.firstWindow();
  await new Promise((r) => setTimeout(r, 300));
  const gone = closed(app);
  await closeWindow(app);
  assert(await gone, 'the app did not close');
  await new Promise((r) => setTimeout(r, 3_000));
  assert(editorsIn(sb.project).length === 0, 'an editor was left running');
});

test('quitting from the editor closes the window', async (sb) => {
  const { app, page } = await open(sb);
  const gone = closed(app);
  await page.evaluate(() => fetch('/action', { method: 'POST', body: JSON.stringify({ action: 'force_quit' }) }));
  assert(await gone, 'the window outlived its editor');
});

test('an editor that outlives a crashed app is re-attached, unsaved edit and all', async (sb) => {
  let { app, page } = await open(sb);
  await typeChar(page, 'Q');
  await until(() => sceneHas(page, 'Qhello'), 'the edit to show');
  const url = page.url();
  process.kill(app.process().pid, 'SIGKILL');
  await new Promise((r) => setTimeout(r, 1_500));
  const [pid] = editorsIn(sb.project);
  assert(pid, 'the editor died with the app');

  ({ app, page } = await open(sb));
  try {
    assert(page.url() === url, `re-attached to ${page.url()}, not ${url}`);
    await until(() => sceneHas(page, 'Qhello'), 'the unsaved edit after re-attach');
    assert(editorsIn(sb.project).length === 1, 'a second editor was started');
  } finally {
    await app.evaluate(({ app: a }) => a.exit(0)).catch(() => {});
    try { process.kill(pid, 'SIGKILL'); } catch { /* gone */ }
  }
});

// ── Run ─────────────────────────────────────────────────────────────────────

const only = process.argv[2];
let failed = 0;
for (const { name, fn } of cases) {
  if (only && !name.includes(only)) continue;
  const sb = sandbox();
  const started = Date.now();
  try {
    await fn(sb);
    console.log(`  PASS ${name} (${((Date.now() - started) / 1000).toFixed(1)}s)`);
  } catch (err) {
    failed++;
    console.log(`  FAIL ${name}\n       ${err.message}`);
    const logs = path.join(sb.home, '.config', 'Fresh Desktop', 'logs');
    for (const f of fs.existsSync(logs) ? fs.readdirSync(logs) : []) {
      console.log(`       ── ${f}\n${fs.readFileSync(path.join(logs, f), 'utf8').split('\n').slice(-20).map((l) => `       ${l}`).join('\n')}`);
    }
  } finally {
    for (const pid of editorsIn(sb.project)) {
      try { process.kill(pid, 'SIGKILL'); } catch { /* gone */ }
    }
    fs.rmSync(sb.root, { recursive: true, force: true });
  }
}
console.log(failed ? `\n${failed} failed` : '\nall passed');
process.exit(failed ? 1 : 0);
