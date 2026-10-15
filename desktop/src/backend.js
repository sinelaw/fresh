'use strict';

// The editor process behind one Fresh Desktop window.
//
// Fresh Desktop does not re-implement anything: each window is the web UI that
// `fresh --web` already serves (crates/fresh-editor/src/webui), loaded from a
// `fresh` binary bundled inside the app. This module finds that binary, starts
// it on a free loopback port in the window's project directory, waits until it
// answers, and shuts it down through the editor's own quit path so hot exit
// and workspace saving run exactly as they do for a terminal session.

const { spawn } = require('node:child_process');
const crypto = require('node:crypto');
const fs = require('node:fs');
const http = require('node:http');
const net = require('node:net');
const os = require('node:os');
const path = require('node:path');

const EXE = process.platform === 'win32' ? 'fresh.exe' : 'fresh';

// How long a starting editor may take to answer before we give up on it.
const START_TIMEOUT_MS = 60_000;
// How long a quitting editor gets to save its workspace before it is killed.
const STOP_TIMEOUT_MS = 8_000;
// How much of the end of the editor's log an error dialog shows.
const OUTPUT_TAIL_BYTES = 8 * 1024;

// How many editor logs to keep in the logs directory.
const KEPT_LOGS = 20;

/**
 * Where a development checkout's `fresh` may be: the staged copy in bin/,
 * then cargo's release and debug builds — in CARGO_TARGET_DIR when that is
 * set, as cargo itself does.
 */
function workspaceBinaries() {
  const repo = path.resolve(__dirname, '..', '..');
  const target = process.env.CARGO_TARGET_DIR
    ? path.resolve(repo, process.env.CARGO_TARGET_DIR)
    : path.join(repo, 'target');
  return [
    path.join(__dirname, '..', 'bin', EXE),
    path.join(target, 'release', EXE),
    path.join(target, 'debug', EXE),
  ];
}

/**
 * The `fresh` binary to run, or null when there is none. Packaged builds
 * carry it in `resources/bin` (see `extraResources` in electron-builder.yml);
 * a development checkout runs the newest of [`workspaceBinaries`] — never a
 * `fresh` from PATH, which may be any older release.
 */
function freshBinary(app) {
  if (process.env.FRESH_DESKTOP_BINARY) return path.resolve(process.env.FRESH_DESKTOP_BINARY);
  if (app.isPackaged) return path.join(process.resourcesPath, 'bin', EXE);
  let best = null;
  for (const c of workspaceBinaries()) {
    try {
      const mtime = fs.statSync(c).mtimeMs;
      if (!best || mtime > best.mtime) best = { path: c, mtime };
    } catch { /* not built */ }
  }
  return best && best.path;
}

/** What to tell someone whose `fresh` cannot be found. */
function missingBinaryHelp() {
  return [
    'no fresh binary was found. Looked in:',
    ...workspaceBinaries().map((c) => `  ${c}`),
    'Build one with `cargo build --release --features web` (from the repository root),',
    'or point FRESH_DESKTOP_BINARY at one',
  ].join('\n');
}

/** Make room for one more log: keep only the newest KEPT_LOGS - 1. */
function pruneLogs(dir) {
  let logs;
  try {
    logs = fs.readdirSync(dir)
      .filter((f) => /^fresh-.*\.log$/.test(f))
      .map((f) => {
        const file = path.join(dir, f);
        return { file, mtime: fs.statSync(file).mtimeMs };
      });
  } catch {
    return;
  }
  logs.sort((a, b) => b.mtime - a.mtime);
  for (const { file } of logs.slice(KEPT_LOGS - 1)) {
    try { fs.rmSync(file); } catch { /* in use, or gone */ }
  }
}

// The page's own preferences (web theme, zoom, palette placement) live in its
// localStorage, which is keyed by origin — port included. A random port per
// launch would forget them every time, so windows take the first free port of
// a fixed range: a lone window always lands on the same origin.
const PREFERRED_PORTS = Array.from({ length: 20 }, (_, i) => 47613 + i);

function tryPort(port) {
  return new Promise((resolve) => {
    const srv = net.createServer();
    srv.unref();
    srv.once('error', () => resolve(null));
    srv.listen(port, '127.0.0.1', () => {
      const bound = srv.address().port;
      srv.close(() => resolve(bound));
    });
  });
}

// Ports handed to an editor of this app. Two windows opening at once would
// otherwise both find the same port free before either editor has bound it.
const claimed = new Set();

/** An unused loopback port. The editor binds it a moment later. */
async function freePort() {
  for (const port of PREFERRED_PORTS) {
    if (!claimed.has(port) && await tryPort(port)) {
      claimed.add(port);
      return port;
    }
  }
  const any = await tryPort(0);
  if (!any) throw new Error('no free loopback port');
  return any;
}

let loginEnv = null;

/**
 * The user's login-shell environment.
 *
 * An app started from the Dock, Finder, a desktop launcher or the Start menu
 * inherits a bare session environment — on macOS a PATH of just
 * /usr/bin:/bin:/usr/sbin:/sbin — so language servers, formatters and
 * everything the integrated terminal runs would go missing. Like other
 * desktop editors, ask the login shell once for the environment it sets up
 * and give that to the editor. Started from a terminal (TERM is set), the
 * environment already is the user's and is used as is.
 */
function loginShellEnv() {
  if (loginEnv) return loginEnv;
  loginEnv = new Promise((resolve) => {
    if (process.platform === 'win32' || process.env.TERM) return resolve({});
    const shell = process.env.SHELL || (process.platform === 'darwin' ? '/bin/zsh' : '/bin/sh');
    const mark = `__FRESH_ENV_${crypto.randomBytes(8).toString('hex')}__`;
    const quote = (s) => `'${s.replace(/'/g, `'\\''`)}'`;
    // Electron's own binary, run as plain node, prints the environment as JSON
    // between markers, so whatever the shell's rc files echo is ignored.
    const script = `"${mark}" + JSON.stringify(process.env) + "${mark}"`;
    const command = `${quote(process.execPath)} -p ${quote(script)}`;
    let out = '';
    let child;
    try {
      child = spawn(shell, ['-i', '-l', '-c', command], {
        stdio: ['ignore', 'pipe', 'ignore'],
        // Unmodified, AppImage library path included: the probe below runs
        // the app's own Electron binary. What it reports is cleaned up in
        // editorEnv() like everything else.
        env: { ...process.env, ELECTRON_RUN_AS_NODE: '1', ELECTRON_NO_ATTACH_CONSOLE: '1' },
        detached: true,
      });
    } catch {
      return resolve({});
    }
    const timer = setTimeout(() => {
      try { process.kill(-child.pid, 'SIGKILL'); } catch { /* already gone */ }
      resolve({});
    }, 10_000);
    child.stdout.on('data', (d) => { out += d; });
    child.on('error', () => { clearTimeout(timer); resolve({}); });
    child.on('close', () => {
      clearTimeout(timer);
      const start = out.indexOf(mark);
      const end = out.lastIndexOf(mark);
      if (start < 0 || end <= start) return resolve({});
      try {
        const env = JSON.parse(out.slice(start + mark.length, end));
        delete env.ELECTRON_RUN_AS_NODE;
        delete env.ELECTRON_NO_ATTACH_CONSOLE;
        resolve(env);
      } catch {
        resolve({});
      }
    });
  });
  return loginEnv;
}

/**
 * `env` without what the app's own packaging put there.
 *
 * The AppImage's launcher prepends its mount to PATH, LD_LIBRARY_PATH,
 * XDG_DATA_DIRS and GSETTINGS_SCHEMA_DIR — inherited by every shell the
 * editor's terminal opens, where the bundled libraries would shadow the
 * system's. And its APPIMAGE/APPDIR would make the bundled `fresh` believe it
 * *is* the AppImage, and offer to replace itself with the terminal edition.
 */
function withoutPackaging(env) {
  const appdir = process.env.APPIMAGE && process.env.APPDIR;
  if (appdir) {
    for (const k of ['PATH', 'LD_LIBRARY_PATH', 'XDG_DATA_DIRS', 'GSETTINGS_SCHEMA_DIR']) {
      if (env[k] === undefined) continue;
      const kept = env[k].split(':').filter((p) => p && p !== appdir && !p.startsWith(`${appdir}/`));
      if (kept.length) env[k] = kept.join(':');
      else delete env[k];
    }
  }
  for (const k of ['APPIMAGE', 'APPDIR', 'ARGV0', 'OWD', 'ELECTRON_RUN_AS_NODE',
    'ELECTRON_NO_ATTACH_CONSOLE', 'CHROME_DESKTOP', 'GIO_LAUNCHED_DESKTOP_FILE',
    'GIO_LAUNCHED_DESKTOP_FILE_PID']) {
    delete env[k];
  }
  return env;
}

/** The environment the editor (and every terminal it opens) runs in. */
async function editorEnv() {
  return withoutPackaging({ ...process.env, ...(await loginShellEnv()) });
}

function request(url, { method = 'GET', body } = {}) {
  return new Promise((resolve, reject) => {
    const req = http.request(url, {
      method,
      timeout: 2_000,
      headers: body ? { 'Content-Type': 'application/json', 'Content-Length': Buffer.byteLength(body) } : {},
    }, (res) => {
      res.resume();
      res.on('error', reject);
      res.on('end', () => resolve(res.statusCode));
    });
    req.on('timeout', () => req.destroy(new Error('timeout')));
    req.on('error', reject);
    if (body) req.write(body);
    req.end();
  });
}

function pidAlive(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch (err) {
    return err.code === 'EPERM';
  }
}

/**
 * The sessions this app's windows are running, by project directory, on disk.
 *
 * An editor outlives an app that is killed outright (a crash, a force quit),
 * still holding its project's session — and a new `fresh --web` for that
 * directory is refused while it does. With this record the next window for the
 * directory re-attaches to the survivor instead: nothing is lost, and the
 * session's quit path still runs when that window closes.
 */
class SessionRecord {
  constructor(file) {
    this.file = file;
  }

  read() {
    try { return JSON.parse(fs.readFileSync(this.file, 'utf8')); } catch { return {}; }
  }

  update(fn) {
    const all = this.read();
    fn(all);
    try {
      fs.mkdirSync(path.dirname(this.file), { recursive: true });
      fs.writeFileSync(this.file, JSON.stringify(all, null, 2));
    } catch { /* best effort: without it a crash just costs the re-attach */ }
  }
}

class Backend {
  /**
   * @param {object} opts
   * @param {string} opts.binary       the `fresh` executable
   * @param {string} opts.workingDir   the project directory the session serves
   * @param {string[]} opts.files      files to open at startup
   * @param {string} opts.logDir       where the editor's output is written
   * @param {string} opts.sessionsFile where running sessions are recorded
   */
  constructor({ binary, workingDir, files, logDir, sessionsFile }) {
    this.binary = binary;
    this.workingDir = workingDir;
    this.files = files;
    this.logDir = logDir;
    this.sessions = new SessionRecord(sessionsFile);
    this.recordKey = path.resolve(workingDir);
    this.pid = null;
    this.child = null;
    this.url = null;
    this.spawnError = null;
    this.exitInfo = null;
    // Set once the window gave up on this editor: a start still under way
    // stops at its next step instead of going on to serve nobody.
    this.cancelled = false;
    // Resolves with { code, signal[, error] } once the editor process is gone.
    this.exited = new Promise((resolve) => {
      this.resolveExited = (info) => {
        if (this.exitInfo && this.exitInfo !== info) return;
        this.exitInfo = info;
        this.sessions.update((all) => {
          if (all[this.recordKey] && all[this.recordKey].pid === this.pid) delete all[this.recordKey];
        });
        resolve(info);
      };
    });
  }

  /** The end of the editor's output, for an error dialog. */
  output() {
    let tail = '';
    if (this.logPath) {
      try {
        const fd = fs.openSync(this.logPath, 'r');
        const size = fs.fstatSync(fd).size;
        const len = Math.min(size, OUTPUT_TAIL_BYTES);
        const buf = Buffer.alloc(len);
        fs.readSync(fd, buf, 0, len, size - len);
        fs.closeSync(fd);
        tail = buf.toString('utf8');
      } catch { /* no log yet */ }
    }
    return [tail.trim(), this.spawnError].filter(Boolean).join('\n');
  }

  /** Start the editor (or re-attach to its survivor) and resolve once its web UI answers. */
  async start() {
    if (!this.binary) throw new Error(missingBinaryHelp());
    this.env = await editorEnv();
    if (this.cancelled) throw new Error('cancelled');
    if (await this.adopt()) return;
    if (this.cancelled) throw new Error('cancelled');

    const port = await freePort();
    this.exited.then(() => claimed.delete(port));
    const addr = `127.0.0.1:${port}`;
    this.url = `http://${addr}/`;

    // The editor writes straight into its log file rather than through a pipe
    // to us, so it keeps running (and logging) if this app dies under it.
    fs.mkdirSync(this.logDir, { recursive: true });
    pruneLogs(this.logDir);
    this.logPath = path.join(this.logDir, `fresh-${Date.now()}-${process.pid}.log`);
    const logFd = fs.openSync(this.logPath, 'a');
    const args = [`--web=${addr}`, ...this.files];
    try {
      this.child = spawn(this.binary, args, {
        cwd: this.workingDir,
        env: this.env,
        stdio: ['ignore', logFd, logFd],
        windowsHide: true,
        // Its own process group, so kill() takes the language servers and
        // tools it started along with it.
        detached: process.platform !== 'win32',
      });
    } finally {
      fs.closeSync(logFd);
    }
    this.pid = this.child.pid ?? null;
    this.child.on('error', (err) => {
      this.spawnError = err.message;
      this.resolveExited({ code: null, signal: null, error: err });
    });
    this.child.on('exit', (code, signal) => this.resolveExited({ code, signal }));

    const deadline = Date.now() + START_TIMEOUT_MS;
    for (;;) {
      if (this.exitInfo) throw new Error(this.startupFailure());
      if (this.cancelled) {
        this.kill();
        throw new Error('cancelled');
      }
      try {
        if ((await request(this.url)) === 200) break;
      } catch { /* not listening yet */ }
      if (Date.now() > deadline) {
        this.kill();
        throw new Error('the editor did not start in time');
      }
      await Promise.race([new Promise((r) => setTimeout(r, 100)), this.exited]);
    }
    if (this.pid) this.sessions.update((all) => { all[this.recordKey] = { pid: this.pid, port }; });
  }

  /** Why the editor exited during startup, as best its output tells. */
  startupFailure() {
    const out = this.output();
    if (/unexpected argument '--web'|compiled without the web UI|built without the `web` feature/i.test(out)) {
      return `${this.binary} has no web UI: it is an older release, or was built without ` +
        'the `web` feature (cargo build --release --features web)';
    }
    return 'the editor exited during startup';
  }

  /** Re-attach to an editor a previous run of the app left serving this directory. */
  async adopt() {
    const rec = this.sessions.read()[this.recordKey];
    if (!rec || !pidAlive(rec.pid)) return false;
    const url = `http://127.0.0.1:${rec.port}/`;
    try {
      if ((await request(`${url}state`)) !== 200) return false;
    } catch {
      return false;
    }
    this.pid = rec.pid;
    this.url = url;
    claimed.add(rec.port);
    this.exited.then(() => claimed.delete(rec.port));
    // Not our child, so no exit event: watch for the process going away.
    const watch = setInterval(() => {
      if (!pidAlive(this.pid)) {
        clearInterval(watch);
        this.resolveExited({ code: 0, signal: null });
      }
    }, 500);
    this.openFiles(this.files);
    return true;
  }

  get running() {
    return this.pid !== null && this.exitInfo === null;
  }

  /** Open more files in this window's session (a second launch, a Finder drop). */
  openFiles(files) {
    if (!this.running || files.length === 0) return;
    // Straight to this window's editor, over the bridge it serves the page
    // on. Not through the session socket (`fresh --cmd session open-file`):
    // that client first probes whether the daemon is alive, and an editor
    // still busy starting can lose the probe — the client then starts a
    // second, headless daemon and hands *it* the files.
    request(`${this.url}open`, { method: 'POST', body: JSON.stringify({ files }) })
      .then((status) => { if (status === 404) this.openFilesBySession(files); })
      .catch(() => this.openFilesBySession(files));
  }

  /** The session-socket hand-off, for an editor without the bridge's `open`. */
  openFilesBySession(files) {
    const child = spawn(this.binary, ['--cmd', 'session', 'open-file', '.', ...files], {
      cwd: this.workingDir,
      env: this.env,
      stdio: 'ignore',
      windowsHide: true,
    });
    child.on('error', () => {});
  }

  /**
   * Ask the editor to quit, as closing its window should: whatever unsaved
   * work hot exit cannot keep is asked about in the window, and the editor
   * exits (resolving `exited`) once the user has answered — or stays, if they
   * cancel. Rejects when the editor could not be asked.
   */
  async requestQuit() {
    if (!this.running) return;
    const status = await request(`${this.url}quit`, { method: 'POST', body: '{}' });
    // An editor too old to know the route quits the unconditional way.
    if (status === 404) await this.stop();
    else if (status !== 200) throw new Error(`the editor answered ${status}`);
  }

  /**
   * Quit the editor without asking anything — auto-save, hot exit, workspace
   * save still run — and kill it if it has not exited within STOP_TIMEOUT_MS.
   */
  async stop() {
    this.cancel();
    if (!this.running) return;
    try {
      await request(`${this.url}action`, { method: 'POST', body: JSON.stringify({ action: 'force_quit' }) });
    } catch {
      // Not listening (yet, or any more) or wedged: nothing will quit it.
      this.kill();
    }
    const timedOut = await Promise.race([
      this.exited.then(() => false),
      new Promise((r) => setTimeout(() => r(true), STOP_TIMEOUT_MS)),
    ]);
    if (timedOut) this.kill();
    await this.exited;
  }

  /** Give up on this editor: a start under way stops at its next step. */
  cancel() {
    this.cancelled = true;
  }

  /** Kill the editor and everything it started. */
  kill() {
    if (!this.running) return;
    if (process.platform === 'win32') {
      const tk = spawn('taskkill', ['/T', '/F', '/PID', String(this.pid)], { stdio: 'ignore', windowsHide: true });
      tk.on('error', () => { try { process.kill(this.pid); } catch { /* already gone */ } });
      return;
    }
    try {
      process.kill(-this.pid, 'SIGKILL');
    } catch {
      // Not a group leader (an editor some other way started): just it.
      try { process.kill(this.pid, 'SIGKILL'); } catch { /* already gone */ }
    }
  }
}

module.exports = { Backend, freshBinary, workspaceBinaries, homeDir: os.homedir };
