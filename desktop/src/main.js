'use strict';

// Fresh Desktop: the Fresh web UI in its own window, with the editor bundled.
//
// Every window runs one `fresh --web` session (see backend.js) for one project
// directory and shows the page that session serves. Nothing about the editor
// lives here — this file only owns windows, the command line, and the few
// places where a desktop app has to behave differently from a browser tab.

const { app, BrowserWindow, Menu, dialog, screen, session, shell } = require('electron');
const fs = require('node:fs');
const path = require('node:path');
const { fileURLToPath } = require('node:url');
const { Backend, freshBinary, homeDir } = require('./backend');

const SMOKE_TEST = process.env.FRESH_DESKTOP_SMOKE_TEST || '';

app.setName('Fresh Desktop');
if (process.platform === 'win32') app.setAppUserModelId('dev.getfresh.FreshDesktop');

// One Fresh Desktop process owns every window. A second launch hands its
// command line to the first (`second-instance` below) and quits.
const primary = app.requestSingleInstanceLock();
if (!primary) app.quit();

/** workingDir → { win, backend, workingDir, ready, closing } */
const windows = new Map();
// Files and folders macOS asked us to open before the app was ready.
const pendingOpen = [];
let quitting = false;

// ── Command line ────────────────────────────────────────────────────────────

/** The arguments after the executable (and, unpackaged, after the app path). */
function userArgs(argv) {
  // Chromium/Electron switches, and macOS's `-psn_…` process serial number.
  const args = argv.slice(1).filter((a) => !a.startsWith('-'));
  // `electron <app dir> …` in a development checkout.
  if (process.defaultApp) args.shift();
  return args;
}

/**
 * Whether a launch's working directory says anything about what to open.
 * A shell's cwd does; the `/` Finder and launchd start apps in, the install
 * directory a Start-menu shortcut uses, or the home directory a desktop
 * launcher starts in, does not. (From a shell in ~ the home directory is
 * still where a launch without arguments lands, as the last resort.)
 */
function meaningfulCwd(cwd) {
  if (!cwd) return false;
  const resolved = path.resolve(cwd);
  if (resolved === path.parse(resolved).root) return false;
  if (key(resolved) === key(homeDir())) return false;
  const inside = (dir) => {
    const rel = path.relative(dir, resolved);
    return rel === '' || (!rel.startsWith('..') && !path.isAbsolute(rel));
  };
  if (inside(path.dirname(process.execPath))) return false;
  if (process.platform === 'win32' && process.env.SystemRoot && inside(process.env.SystemRoot)) return false;
  return true;
}

/** A launch argument as a path: desktop launchers pass `file://` URIs. */
function argPath(arg) {
  if (!arg.startsWith('file:')) return arg;
  try { return fileURLToPath(arg); } catch { return arg; }
}

/**
 * Turn launch paths into a project directory plus files to open in it:
 * a directory argument is the project (`explicit`); otherwise the launch's
 * working directory, else the first file's directory, else the home directory.
 */
function resolveLaunch(paths, cwd) {
  const base = cwd || homeDir();
  let dir = null;
  const files = [];
  for (const p of paths) {
    const abs = path.resolve(base, argPath(p));
    let isDir = false;
    try { isDir = fs.statSync(abs).isDirectory(); } catch { /* a new file */ }
    if (isDir) dir = dir || abs;
    else files.push(abs);
  }
  const explicit = dir !== null;
  if (!dir) dir = meaningfulCwd(cwd) ? path.resolve(cwd) : files.length ? path.dirname(files[0]) : homeDir();
  return { workingDir: dir, files, explicit };
}

function key(dir) {
  const k = path.resolve(dir);
  return process.platform === 'linux' ? k : k.toLowerCase();
}

/** Open a launch in the window already serving its directory, or a new one. */
function open(launch) {
  const { workingDir, files, explicit } = launch;
  const existing = windows.get(key(workingDir));
  if (existing && existing.closing && !existing.win.isVisible()) {
    // Its editor is on the way out and still holds the directory's session:
    // a new one would be refused. Open once it is gone.
    existing.backend.exited.then(() => setImmediate(() => open(launch)));
    return;
  }
  if (existing) {
    if (existing.ready) existing.backend.openFiles(files);
    else existing.queued.push(...files);
    focus(existing.win);
    return;
  }
  // A bare file with no folder of its own (a Finder double-click, a second
  // launch) goes to the window whose project contains it, if there is one.
  if (files.length > 0 && !explicit) {
    const owner = [...windows.values()].find((w) => !w.closing && files.every((f) => {
      const rel = path.relative(w.workingDir, f);
      return !rel.startsWith('..') && !path.isAbsolute(rel);
    }));
    if (owner) {
      if (owner.ready) owner.backend.openFiles(files);
      else owner.queued.push(...files);
      focus(owner.win);
      return;
    }
  }
  createWindow(workingDir, files);
}

function focus(win) {
  if (win.isMinimized()) win.restore();
  win.show();
  win.focus();
}

// ── Window state ────────────────────────────────────────────────────────────

const statePath = () => path.join(app.getPath('userData'), 'window-state.json');

function loadWindowState() {
  const fallback = { width: 1280, height: 820 };
  try {
    const s = JSON.parse(fs.readFileSync(statePath(), 'utf8'));
    const bounds = { width: s.width, height: s.height, x: s.x, y: s.y, maximized: !!s.maximized };
    if (!(bounds.width > 200 && bounds.height > 150)) return fallback;
    // Forget a position that is no longer on any screen (a monitor unplugged).
    const visible = screen.getAllDisplays().some(({ workArea: a }) =>
      bounds.x >= a.x - 50 && bounds.y >= a.y - 50 &&
      bounds.x < a.x + a.width - 50 && bounds.y < a.y + a.height - 50);
    if (!visible) { delete bounds.x; delete bounds.y; }
    // A second window cascades instead of landing exactly on the first.
    if (windows.size > 0 && bounds.x !== undefined) { bounds.x += 30 * windows.size; bounds.y += 30 * windows.size; }
    return bounds;
  } catch {
    return fallback;
  }
}

function saveWindowState(win) {
  try {
    const b = win.getNormalBounds();
    fs.mkdirSync(path.dirname(statePath()), { recursive: true });
    fs.writeFileSync(statePath(), JSON.stringify({ ...b, maximized: win.isMaximized() }));
  } catch { /* cosmetic */ }
}

// ── Windows ─────────────────────────────────────────────────────────────────

function sameOrigin(a, b) {
  try { return new URL(a).origin === new URL(b).origin; } catch { return false; }
}

function openExternal(url) {
  try {
    const { protocol } = new URL(url);
    if (protocol === 'http:' || protocol === 'https:' || protocol === 'mailto:') shell.openExternal(url);
  } catch { /* not a URL */ }
}

function createWindow(workingDir, files) {
  const state = loadWindowState();
  const win = new BrowserWindow({
    width: state.width,
    height: state.height,
    x: state.x,
    y: state.y,
    minWidth: 480,
    minHeight: 320,
    show: false,
    title: windowTitle(workingDir),
    backgroundColor: '#1e1e1e',
    icon: process.platform === 'linux' ? path.join(__dirname, '..', 'assets', 'icon.png') : undefined,
    webPreferences: {
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true,
      spellcheck: false,
      preload: path.join(__dirname, 'preload.js'),
    },
  });
  if (state.maximized) win.maximize();

  const backend = new Backend({
    binary: freshBinary(app),
    workingDir,
    files,
    logDir: app.getPath('logs'),
    sessionsFile: path.join(app.getPath('userData'), 'sessions.json'),
  });
  const entry = { win, backend, workingDir, ready: false, closing: false, queued: [] };
  windows.set(key(workingDir), entry);

  win.once('ready-to-show', () => win.show());
  // The page calls itself "Fresh"; name the window after its project instead.
  win.on('page-title-updated', (e) => e.preventDefault());

  // The page is the editor, nothing else: links leave for the system browser.
  win.webContents.setWindowOpenHandler(({ url }) => {
    openExternal(url);
    return { action: 'deny' };
  });
  win.webContents.on('will-navigate', (e, url) => {
    if (!backend.url || !sameOrigin(url, backend.url)) {
      e.preventDefault();
      openExternal(url);
    }
  });

  // Closing the window quits its editor, and the window closes when the
  // editor has exited. Once the page is up that is the editor's own quit, in
  // the window: unsaved work hot exit cannot keep is asked about there, and
  // cancelling keeps both open (closing again asks again). An editor that
  // cannot be asked, or one still starting, is stopped outright.
  const stopAndClose = () => {
    entry.closing = true;
    win.hide();
    backend.stop().finally(() => { if (!win.isDestroyed()) win.destroy(); });
  };
  win.on('close', (e) => {
    saveWindowState(win);
    if (!entry.ready) {
      backend.cancel();
      if (!backend.running) return;
      e.preventDefault();
      if (!entry.closing) stopAndClose();
      return;
    }
    if (!backend.running) return;
    e.preventDefault();
    if (entry.closing && !win.isVisible()) return;
    entry.closing = true;
    backend.requestQuit().catch(stopAndClose);
  });
  win.on('closed', () => windows.delete(key(workingDir)));

  win.loadFile(path.join(__dirname, 'loading.html'));

  backend.start().then(async () => {
    if (win.isDestroyed()) throw new Error('cancelled');
    entry.ready = true;
    await win.loadURL(backend.url);
    if (entry.queued.length) backend.openFiles(entry.queued.splice(0));
    if (SMOKE_TEST) smokeTest(entry);
  }).catch((err) => {
    const gaveUp = entry.closing || quitting || backend.cancelled;
    // Never leave an editor behind serving no window.
    backend.stop();
    if (gaveUp) {
      if (!win.isDestroyed()) win.destroy();
      return;
    }
    entry.closing = true;
    reportFailure(`Fresh could not start in ${workingDir}: ${err.message}.`, backend);
    if (!win.isDestroyed()) win.destroy();
  });

  // The editor exiting — quit from its own menu, after the window asked it
  // to, or dying — closes its window.
  backend.exited.then((info) => {
    if (!entry.ready) return; // startup's own failure path above
    if (!entry.closing && info.code !== 0 && !quitting) {
      reportFailure(`Fresh exited unexpectedly (${info.signal || `exit code ${info.code}`}).`, backend);
    }
    if (!win.isDestroyed()) win.destroy();
  });

  return entry;
}

function windowTitle(workingDir) {
  const name = path.basename(workingDir) || workingDir;
  return `${name} — Fresh`;
}

function reportFailure(message, backend) {
  const output = backend.output();
  const detail = [
    backend.binary && `Editor: ${backend.binary}`,
    output && `Output:\n${output.split('\n').slice(-25).join('\n')}`,
    backend.logPath && `Full log: ${backend.logPath}`,
  ].filter(Boolean).join('\n\n');
  if (SMOKE_TEST) {
    console.error(`${message}\n${detail}`);
    app.exit(1);
    return;
  }
  dialog.showErrorBox('Fresh Desktop', detail ? `${message}\n\n${detail}` : message);
}

async function openFolderDialog() {
  const res = await dialog.showOpenDialog({ properties: ['openDirectory', 'createDirectory'] });
  if (!res.canceled && res.filePaths[0]) open({ workingDir: res.filePaths[0], files: [], explicit: true });
}

// ── Menus ───────────────────────────────────────────────────────────────────

// The editor draws its own menu bar, and every key chord belongs to it — an
// application menu's accelerators (Ctrl+W, Ctrl+R, Ctrl+= …) would take keys
// the editor is bound to. Windows and Linux get no menu at all. macOS needs
// the app menu, and an Edit ▸ Paste item: without it Cmd+V never produces the
// DOM paste event that is the web UI's one paste path.
function installMenu() {
  if (process.platform !== 'darwin') {
    Menu.setApplicationMenu(null);
    return;
  }
  Menu.setApplicationMenu(Menu.buildFromTemplate([
    {
      role: 'appMenu',
      submenu: [
        { role: 'about' },
        { type: 'separator' },
        { role: 'hide' },
        { role: 'hideOthers' },
        { role: 'unhide' },
        { type: 'separator' },
        { role: 'quit' },
      ],
    },
    {
      label: 'File',
      submenu: [{ label: 'Open Folder in New Window…', click: openFolderDialog }],
    },
    { label: 'Edit', submenu: [{ role: 'paste' }] },
    {
      role: 'windowMenu',
      submenu: [{ role: 'minimize' }, { role: 'zoom' }, { type: 'separator' }, { role: 'front' }],
    },
    {
      role: 'help',
      submenu: [{ label: 'Fresh Documentation', click: () => shell.openExternal('https://sinelaw.github.io/fresh/') }],
    },
  ]));
}

// ── Smoke test ──────────────────────────────────────────────────────────────

// FRESH_DESKTOP_SMOKE_TEST=<png> runs the packaged app end to end once — the
// bundled editor starts, the page connects and renders, a keystroke goes in
// and a frame comes back, the window is captured to <png>, and the editor
// quits cleanly — then exits 0, or 1 on any failure. CI runs it against every
// installer before it is released.
async function smokeTest(entry) {
  const { win, backend } = entry;
  const fail = (msg) => {
    console.error(`smoke test: FAIL — ${msg}\n${backend.output()}`);
    backend.kill();
    app.exit(1);
  };
  const timer = setTimeout(() => fail('timed out'), 90_000);
  const js = (code) => win.webContents.executeJavaScript(code);
  const waitFor = async (code, what) => {
    const until = Date.now() + 45_000;
    while (Date.now() < until) {
      if (await js(code).catch(() => false)) return;
      await new Promise((r) => setTimeout(r, 200));
    }
    throw new Error(`timed out waiting for ${what}`);
  };
  try {
    await waitFor('!!(window.fresh && window.fresh.wsOpen && window.fresh.frames > 0 && window.fresh.scene)', 'the first frame');
    const before = await js('window.fresh.frames');
    await js(`window.fresh.sendKey({key:"x",code:"KeyX",ctrl:false,meta:false,shift:false,alt:false})`);
    await waitFor(`window.fresh.frames > ${before}`, 'a frame answering a keystroke');
    await new Promise((r) => setTimeout(r, 1_000));
    const image = await win.webContents.capturePage();
    if (image.isEmpty()) throw new Error('the window captured as an empty image');
    fs.writeFileSync(SMOKE_TEST, image.toPNG());
    console.log(`smoke test: rendered ${image.getSize().width}x${image.getSize().height}, wrote ${SMOKE_TEST}`);
    entry.closing = true;
    await backend.stop();
    clearTimeout(timer);
    if (backend.exitInfo.code !== 0) return fail(`editor exited with ${JSON.stringify(backend.exitInfo)}`);
    console.log('smoke test: editor quit cleanly — PASS');
    app.exit(0);
  } catch (err) {
    fail(err.message);
  }
}

// ── App lifecycle ───────────────────────────────────────────────────────────

app.on('open-file', (e, p) => {
  // macOS: a file or folder dropped on the Dock icon, or opened with the app.
  e.preventDefault();
  if (app.isReady()) open(resolveLaunch([p], null));
  else pendingOpen.push(p);
});

app.on('second-instance', (_e, argv, cwd) => {
  open(resolveLaunch(userArgs(argv), cwd));
});

app.on('before-quit', () => { quitting = true; });

app.on('window-all-closed', () => {
  // macOS apps stay in the Dock with no windows open — unless asked to quit.
  if (process.platform !== 'darwin' || quitting) app.quit();
});

app.on('activate', () => {
  if (app.isReady() && windows.size === 0 && !quitting) open({ workingDir: homeDir(), files: [], explicit: true });
});

app.whenReady().then(() => {
  if (!primary) return;
  installMenu();

  // Clipboard access for the editor's copy/paste; nothing else (camera,
  // notifications, geolocation …) is the page's to ask for.
  const allowed = new Set(['clipboard-read', 'clipboard-sanitized-write']);
  const local = (url) => {
    try { return new URL(url).hostname === '127.0.0.1'; } catch { return false; }
  };
  session.defaultSession.setPermissionRequestHandler((_wc, permission, cb, details) => {
    cb(allowed.has(permission) && local(details.requestingUrl));
  });
  session.defaultSession.setPermissionCheckHandler((_wc, permission, origin) =>
    allowed.has(permission) && local(origin));

  const launch = pendingOpen.length
    ? resolveLaunch(pendingOpen.splice(0), null)
    : resolveLaunch(userArgs(process.argv), process.cwd());
  open(launch);
});
