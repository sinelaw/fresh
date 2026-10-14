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

/**
 * The `fresh` binary to run. Packaged builds carry it in `resources/bin`
 * (see `extraResources` in electron-builder.yml); a development checkout falls
 * back to whatever cargo last built in the workspace.
 */
function freshBinary(app) {
  if (process.env.FRESH_DESKTOP_BINARY) return process.env.FRESH_DESKTOP_BINARY;
  if (app.isPackaged) return path.join(process.resourcesPath, 'bin', EXE);
  const repo = path.resolve(__dirname, '..', '..');
  const candidates = [
    path.join(__dirname, '..', 'bin', EXE),
    path.join(repo, 'target', 'release', EXE),
    path.join(repo, 'target', 'debug', EXE),
  ];
  return candidates.find((c) => fs.existsSync(c)) || EXE;
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
  const env = withoutPackaging({ ...process.env, ...(await loginShellEnv()) });
  // The bundled binary is updated by installing a newer Fresh Desktop, never
  // in place: point the editor's update notice at the releases page instead
  // of `fresh --cmd update`.
  if (!env.FRESH_INSTALL_CHANNEL) env.FRESH_INSTALL_CHANNEL = 'unknown';
  return env;
}

function request(url, { method = 'GET', body } = {}) {
  return new Promise((resolve, reject) => {
    const req = http.request(url, {
      method,
      timeout: 2_000,
      headers: body ? { 'Content-Type': 'application/json', 'Content-Length': Buffer.byteLength(body) } : {},
    }, (res) => {
      res.resume();
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
    this.env = await editorEnv();
    if (await this.adopt()) return;

    const port = await freePort();
    this.exited.then(() => claimed.delete(port));
    const addr = `127.0.0.1:${port}`;
    this.url = `http://${addr}/`;

    // The editor writes straight into its log file rather than through a pipe
    // to us, so it keeps running (and logging) if this app dies under it.
    fs.mkdirSync(this.logDir, { recursive: true });
    this.logPath = path.join(this.logDir, `fresh-${Date.now()}-${process.pid}.log`);
    const logFd = fs.openSync(this.logPath, 'a');
    const args = [`--web=${addr}`, ...this.files];
    try {
      this.child = spawn(this.binary, args, {
        cwd: this.workingDir,
        env: this.env,
        stdio: ['ignore', logFd, logFd],
        windowsHide: true,
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
      if (this.exitInfo) throw new Error('the editor exited during startup');
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
    // `fresh --cmd session open-file .` hands the list to the session serving
    // the current directory — the one this window's editor runs.
    const child = spawn(this.binary, ['--cmd', 'session', 'open-file', '.', ...files], {
      cwd: this.workingDir,
      env: this.env,
      stdio: 'ignore',
      windowsHide: true,
    });
    child.on('error', () => {});
  }

  /**
   * Quit the editor the way its own Quit does — auto-save, hot exit, workspace
   * save — and kill it if it has not exited within STOP_TIMEOUT_MS.
   */
  async stop() {
    if (!this.running) return;
    try {
      await request(`${this.url}action`, { method: 'POST', body: JSON.stringify({ action: 'force_quit' }) });
    } catch { /* already going away, or wedged — the timeout handles both */ }
    const timedOut = await Promise.race([
      this.exited.then(() => false),
      new Promise((r) => setTimeout(() => r(true), STOP_TIMEOUT_MS)),
    ]);
    if (timedOut) this.kill();
    await this.exited;
  }

  kill() {
    if (!this.running) return;
    try {
      if (this.child) this.child.kill('SIGKILL');
      else process.kill(this.pid, 'SIGKILL');
    } catch { /* already gone */ }
  }
}

module.exports = { Backend, freshBinary, homeDir: os.homedir };
