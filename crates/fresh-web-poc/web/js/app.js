// The client application shell: panes and tabs, the palette, the status
// bar, the keymap and command registry, terminals, and the glue between the
// server's events, the buffers and the plugins. Features with UI of their
// own (explorer, search, git, language-server panels, markdown, tours,
// keybindings, themes) are plugins in `web/plugins/`.

import { rpc, attempt, clientId, connectEvents, failure } from "./rpc.js";
import { shiftRange } from "./decor.js";
import { byteLen, byteToIdx } from "./utf8.js";
import { shownFrom } from "./paths.js";
import { hostText } from "./hosts.js";
import { fileBuffer, virtualBuffer, terminalBuffer, terminalOf, ENSURE_AFTER, synced, whenSynced, opsSince, filePath, untitledNumber } from "./buffer.js";
import { Pane, copyTab, cursorAt, selecting, sel } from "./view.js";
import { put, KEEP, lineAt, addCursorVertical, shrinkSelection, unfoldAll, selectLine, foldAll, blockSelect, toggleFold } from "./editing.js";
import { THEME } from "./highlight.js";
import { sash, clamp } from "./sash.js";

import { $, setClass } from "./dom.js";

import { asCommand, asArgCommand } from "./commands.js";
import { listOf, found, some, none } from "./maybe.js";
import { bufferHandle, viewHandle } from "./host.js";
import { action, actionOf, SEPARATOR } from "./menu.js";
import { rowOf, SEP } from "./menubar.js";

// The clipboard's keys are the browser's own (not in the keymap); menus
// show them.
const CLIPBOARD_KEYS = new Map([["edit.cut", "Ctrl+X"], ["edit.copy", "Ctrl+C"], ["edit.paste", "Ctrl+V"]]);

// The settings that are on or off, by name (menus check them, commands
// toggle them).
/** const DEFAULT_FLAGS: Dict<Boolean> */
const DEFAULT_FLAGS = {
  auto_close: true, auto_save: false, highlight_current_line: true, highlight_occurrences: true, show_whitespace: false,
  show_indent_guides: true, preview_tabs: true, inlay_hints: true, highlight_brackets: true, rainbow_brackets: true, scroll_sync: false,
};
/** const DEFAULT_SETTINGS: Settings */
export const DEFAULT_SETTINGS = { flags: DEFAULT_FLAGS, auto_save_seconds: 30, rulers: [], page_width: 80, keymap: "default", theme: "dark", zoom: 1 };
// The settings' storage key (it names their shape: settings kept in
// another shape are left alone).
const SETTINGS_KEY = "fresh-poc:settings2";

// The settings and the user's own key bindings this browser remembers.
/** function loadSettings() => Settings */
export function loadSettings() {
  for (const k of kept(SETTINGS_KEY)) return { ...DEFAULT_SETTINGS, ...k, flags: flagsFrom(k.flags ?? DEFAULT_FLAGS) };
  return DEFAULT_SETTINGS;
}
// What this browser kept under `key`, parsed (a list of at most one: none
// when nothing was kept, or it cannot be read).
function kept(key) {
  try { const t = localStorage.getItem(key); return t === null ? [] : [JSON.parse(t)]; } catch { return []; }
}
// The flags kept, each one not kept at its default.
/** function flagsFrom(kept: Dict<Boolean>) => Dict<Boolean> */
function flagsFrom(kept) {
  /** const flags: Dict<Boolean> */
  const flags = {};
  for (const [k, v] of Object.entries(DEFAULT_FLAGS)) flags[k] = kept[k] ?? v;
  return flags;
}
/** function loadUserKeys() => Dict<String> */
export function loadUserKeys() {
  /** let keys: Dict<String> */
  let keys = {};
  try { keys = JSON.parse(localStorage.getItem("fresh-poc:keys") ?? "{}"); } catch {}
  return keys;
}

// The application: built by main.js from its parts (each made before it,
// holding a getter for it), with the workspace the server said this page
// views. Its constructor only sets fields; start() wires it up.
// The editor area's layout: a pane, or a split of two layouts, side by
// side ("h") or stacked ("v"), the first taking `size.ratio` of it (its
// sash moves it).
/** type Fork<P> = {dir: String, size: {ratio: Number}, a: Layout<P>, b: Layout<P>} */
/** type Layout<P> = {kind: "pane", pane: P} | {kind: "split", fork: Fork<P>} */
/** function leaf<P>(pane: P) => Layout<P> */
function leaf(pane) { return { kind: "pane", pane }; }
/** function fork<P>(dir: String, ratio: Number, a: Layout<P>, b: Layout<P>) => Layout<P> */
function fork(dir, ratio, a, b) { return { kind: "split", fork: { dir, size: { ratio }, a, b } }; }
// A layout's pane when it is one, its split when it is one.
/** function paneAt<P>(n: Layout<P>) => P | Null */
function paneAt(n) { return n.kind === "pane" ? n.pane : null; }
/** function forkAt<P>(n: Layout<P>) => Fork<P> | Null */
function forkAt(n) { return n.kind === "split" ? n.fork : null; }
// Its panes, in order.
/** function panesOf<P>(n: Layout<P>) => P[] */
function panesOf(n) { return n.kind === "pane" ? [n.pane] : [...panesOf(n.fork.a), ...panesOf(n.fork.b)]; }
// `n` with pane `old` split in two: `old`, then `np` (`dir`, `ratio`).
/** function withSplit<P>(n: Layout<P>, old: P, np: P, dir: String, ratio: Number) => Layout<P> */
function withSplit(n, old, np, dir, ratio) {
  if (n.kind === "pane") return n.pane === old ? fork(dir, ratio, leaf(old), leaf(np)) : n;
  const f = n.fork;
  return { kind: "split", fork: { dir: f.dir, size: f.size, a: withSplit(f.a, old, np, dir, ratio), b: withSplit(f.b, old, np, dir, ratio) } };
}
// `n` without pane `p` (its sibling takes its split's place).
/** function without<P>(n: Layout<P>, p: P) => Layout<P> */
function without(n, p) {
  if (n.kind === "pane") return n;
  const f = n.fork;
  if (paneAt(f.a) === p) return f.b;
  if (paneAt(f.b) === p) return f.a;
  return { kind: "split", fork: { dir: f.dir, size: f.size, a: without(f.a, p), b: without(f.b, p) } };
}

// The remembered layout's parts (types/fresh.d.js).
/** function savedTab(ref: TabRef, pos: Number, top: Number, preview: Boolean) => SavedTab */
function savedTab(ref, pos, top, preview) { return { ref, pos, top, preview }; }
/** function tabFile(path: String) => TabRef */
function tabFile(path) { return { kind: "file", path }; }
/** function tabUntitled(n: Number) => TabRef */
function tabUntitled(n) { return { kind: "untitled", n }; }
/** function tabTerminal(term: Number) => TabRef */
function tabTerminal(term) { return { kind: "terminal", term }; }
// A tab's name in the remembered layout (which of a split's tabs shows).
/** function refKey(ref: TabRef) => String */
function refKey(ref) { return ref.kind === "file" ? ref.path : ref.kind === "untitled" ? `untitled:${ref.n}` : `terminal:${ref.term}`; }
/** function savedPane(tabs: SavedTab[], active: String | Null) => SavedNode */
function savedPane(tabs, active) { return { kind: "pane", tabs, active }; }
/** function savedSplit(dir: String, ratio: Number, a: SavedNode, b: SavedNode) => SavedNode */
function savedSplit(dir, ratio, a, b) { return { kind: "split", dir, ratio, a, b }; }

// A language server's (line, character) as a byte in the held lines (a
// list of at most one), and back: byte `pos` on held line `i` (none: -1).
/** function offsetIn(held: HeldLines[], line: Number, character: Number) => Number | Null */
function offsetIn(held, line, character) {
  for (const L of held) {
    const first = L.firstLine;
    if (first === null) return null;
    const i = line - first;
    if (i < 0 || i >= L.lines.length) return null;
    return L.starts[i] + byteLen(L.lines[i].slice(0, character));
  }
  return null;
}
// (none also when the lines' place in the file is not known yet).
/** function positionIn(L: HeldLines, i: Number, pos: Number) => LspPos | Null */
function positionIn(L, i, pos) {
  const first = L.firstLine;
  if (i < 0 || first === null) return null;
  return { line: first + i, character: byteToIdx(L.lines[i], pos - L.starts[i]) };
}

// A process's exit: its code and output.
// (`code`: none when a signal ended it, or it was stopped.)
/** type ProcResult = {code: Number | Null, stdout: String, stderr: String} */
/** function procResult(code: Number | Null, stdout: String, stderr: String) => ProcResult */
function procResult(code, stdout, stderr) { return { code, stdout, stderr }; }
// A part's item in the status bar.
/** type StatusFact = {text: String, title: String, onClick: (() => Undefined) | Null} */
/** function statusFact() => StatusFact */
function statusFact() { return { text: "", title: "", onClick: null }; }
/** function statusFactOf(
 *   text: String,
 *   title: String,
 *   onClick: (() => Undefined) | Null,
 * ) => StatusFact */
function statusFactOf(text, title, onClick) { return { text, title, onClick }; }
/** function noFacts() => Map<String, StatusFact> */
function noFacts() { return new Map(); }
/** function noFormats() => DocFormats[] */
function noFormats() { return []; }
/** function noBytes() => Number[] */
function noBytes() { return []; }
/** function noCommands() => Map<String, Command> */
function noCommands() { return new Map(); }
/** function noStatus() => Dict<LspState> */
function noStatus() { return {}; }
// Whether a language's server may be asked (not stopped, nor failed).
/** function serverUp(s: LspState | Undefined) => Boolean */
function serverUp(s) { return s === undefined || s.kind === "running"; }
/** function noWaiting() => Map<Number, (ProcResult) => Undefined> */
function noWaiting() { return new Map(); }
/** function noExits() => Map<Number, ProcResult> */
function noExits() { return new Map(); }

// The buffer modes' keys, typed; the key `k` of mode `mode` (none: a
// buffer of no mode, or a key it leaves alone).
/** function noModes() => Map<String, ModeKey[]> */
function noModes() { return new Map(); }
// The plugins' key bindings, typed.
/** function noKeys() => Map<String, String> */
function noKeys() { return new Map(); }
// The keys a text field keeps for its own editing.
const FIELD_KEYS = new Set(["C-a", "C-c", "C-v", "C-x", "C-z", "C-S-z", "C-y", "C-Left", "C-Right", "C-S-Left", "C-S-Right", "C-Backspace", "C-Delete", "C-Home", "C-End", "C-S-Home", "C-S-End"]);

// Line endings, as the pickers say them.
const LINE_ENDINGS = new Map([["LF", "LF — Unix, macOS"], ["CRLF", "CRLF — Windows"], ["CR", "CR — classic Mac"]]);

// Keys the keymap names its own way.
const KEY_NAMES = new Map([["ArrowUp", "Up"], ["ArrowDown", "Down"], ["ArrowLeft", "Left"], ["ArrowRight", "Right"], [" ", "Space"], ["Escape", "Esc"]]);

// How a key's modifiers are written for people.
const MODIFIERS = new Map([["C", "Ctrl"], ["M", "Alt"], ["S", "Shift"]]);
/** function modeKey(
 *   modes: Map<String, ModeKey[]>,
 *   mode: String | Null,
 *   k: String,
 * ) => ModeKey | Undefined */
function modeKey(modes, mode, k) { return mode === null ? undefined : (modes.get(mode) ?? []).find((b) => b.key === k); }

/** function spot(remember: () => Undefined, restore: () => Undefined) => Spot */
function spot(remember, restore) { return { remember, restore }; }

// A tab's key, to find it again (a list of at most one).
function tabKeys(b) { return tabRef(b).map(refKey); }
// A list's first string (none: it is empty).
/** function firstOf(xs: String[]) => String | Null */
function firstOf(xs) { return xs.length > 0 ? some(xs[0]) : none(); }

// What a tab shows, to restore it: a file, an untitled document or a
// terminal (none: a plugin's buffer).
function tabRef(b) {
  for (const p of filePath(b.origin)) return [tabFile(p)];
  for (const n of untitledNumber(b.origin)) return [tabUntitled(n)];
  for (const id of terminalOf(b.source)) return [tabTerminal(id)];
  return [];
}

export async function scrollToFraction(pane, f) {
  if (pane.shown.length === 0) return;
  const v = pane.shown[0];
  const b = v.buf;
  // The document's line count, when its line index is built.
  const lines = b.doc === null ? [] : listOf(b.totalLines).filter((n) => n > 0);
  if (b.full) {
    for (const L of listOf(b.text)) v.setTop(L.starts[Math.min(L.count - 1, Math.floor(f * L.count))], 0);
  } else if (lines.length > 0) {
    const r = await rpc("doc.line_start", { doc: b.doc, line: Math.floor(f * lines[0]) });
    if (r.pos !== null) { await b.ensure(r.pos, ENSURE_AFTER); v.setTop(r.pos, 0); }
  } else {
    const p = Math.floor(f * b.len);
    await b.ensure(p, ENSURE_AFTER);
    for (const L of listOf(b.text)) v.setTop(L.starts[Math.max(0, L.indexOf(p))], 0);
  }
  pane.scheduleRender();
}

// A language server's (line, UTF-16 column) as a byte offset in a held buffer.
export function lspOffset(b, line, character) {
  const L = b.text;
  return offsetIn(L === null ? [] : [{ lines: L.lines, starts: L.starts, firstLine: L.firstLine }], line, character);
}

// Byte `pos` as a language server's (line, UTF-16 column), when held.
export function lspPosition(b, pos) {
  const L = b.text;
  return L === null ? null : positionIn({ lines: L.lines, starts: L.starts, firstLine: L.firstLine }, L.indexOf(pos), pos);
}

/** function keyName(e: DomEvent<El>) => String */
export function keyName(e) {
  let k = found(KEY_NAMES.get(e.key)).at(0) ?? e.key;
  // A digit is named by its key, shifted or not (Shift+1 is "S-1", not "!").
  if (/^Digit\d$/.test(e.code)) k = e.code.slice(5);
  if (k.length === 1) k = k.toLowerCase();
  const shift = e.shiftKey && (k.length > 1 || /[a-z0-9]/.test(k));
  return (e.ctrlKey || e.metaKey ? "C-" : "") + (e.altKey ? "M-" : "") + (shift ? "S-" : "") + k;
}

// `text` in the popup at `at`.
export function popup(text, at) {
  const h = $("#popup");
  h.textContent = text;
  h.style.left = `${Math.min(at.x, innerWidth - 420)}px`;
  h.style.top = `${at.y}px`;
  h.classList.remove("hidden");
}

export function hidePopup() { $("#popup").classList.add("hidden"); }

export class App {
  constructor(c) {
    this.buffers = new Map();
    this.docs = new Map();
    this.commands = noCommands(); // by id
    this.navSeq = 0; // the latest navigation (gotoLine)
    this.hoverTimer = 0; // the hover asked for, after a pause
    this.modes = noModes(); // buffer mode -> its keys
    this.keys = noKeys(); // key -> command, from plugins
    // Keys a focused terminal leaves to the editor: its toggle, moving
    // between tabs, and the command palette's keys a shell does not use.
    this.terminalKeys = new Set(["C-`", "C-Tab", "C-S-Tab", "C-PageDown", "C-PageUp", "F1", "C-S-p"]);
    this.statusItems = noFacts();
    // Processes run (`runProcess`): the callers waiting for their exit,
    // and exits that came before the request was answered.
    this.waiting = noWaiting();
    this.exited = noExits();
    this.watches = new Map();
    this.hoverProviders = [];
    this.history = { back: [], fwd: [] };
    this.opening = Promise.resolve(undefined); // the file being opened (openFile)
    this.scrollDriver = []; // the split the pointer scrolled last, if any (syncScroll)
    this.scrollLinks = []; // [{a, b, aToB, bToA}]: buffers scrolled together
    this.killRing = [];
    this.metrics = { cw: 8, lh: 18 };
    this.status = "";
    this.statusSig = ""; // what the status bar shows, to write it only when it changes
    this.statusRuns = []; // its items' actions
    this.formats = noFormats(); // the server's languages, encodings and line endings, once asked (at most one)
    this.maximized = []; // the split filling the editor area: a list of at most one
    this.restoring = true; // putting the remembered layout back: nothing is remembered meanwhile
    this.composing = false; // an input method is composing text
    this.log = [];
    const settings /*: Settings */ = c.settings;
    this.settings = settings;
    this.userKeys = c.userKeys;
    // What this browser remembers of the workspace (session.js).
    this.session = c.session;
    // The workspace this page views (null: none is open, the page is the
    // orchestrator's dock alone), and the URL's parameters.
    this.workspace = c.workspace;
    this.params = c.params;
    const ws = listOf(c.workspace);
    this.root = ws.map((w) => w.root).at(0) ?? "";
    this.hostName = ws.map((w) => hostText(w.host)).at(0) ?? "";
    this.trusted = ws.some((w) => w.trusted);
    this.lspLanguages = new Set(ws.flatMap((w) => w.lsp));
    this.lspStatus = noStatus(); // each language's server, as `lsp.status` events say
    this.orchestrator = c.orchestrator;
    this.skins = c.skins;
    this.editing = c.editing;
    this.ui = c.ui;
    this.prompt = c.prompt;
    this.terminals = c.terminals;
    this.host = c.host;
    this.macros = c.macros;
    this.menubar = c.menubar;
    this.input = c.input;
    this.paneEditor = c.paneEditor; // what each pane asks of the editor
    const pane = new Pane(c.paneEditor);
    this.layoutTree = leaf(pane);
    this.activePane = pane;
  }

  // Wire the parts up, load the plugins and put the workspace's layout back.
  async start(plugins) {
    const ws = this.workspace;
    // The skin first: the editor's cell is measured in its fonts.
    await this.skins.load();
    document.body.style.setProperty("--editor-zoom", String(this.settings.zoom));
    this.measure();
    // The workspace's name heads the page (the orchestrator keeps it).
    if (ws) this.orchestrator.renderTitle();
    else { document.title = "Workspaces — Fresh"; $("#ws-name").textContent = "No workspace"; }
    $("#ws-name").title = "Workspaces (Alt+O)";
    connectEvents((ev) => { this.onEvent(ev); });
    if (ws) this.session.open(hostText(ws.host), ws.root);
    this.installSashes();
    this.prompt.listen();
    this.activePane.listen();
    this.listenInput();
    this.registerCoreCommands();
    for (const c of this.macros.commands()) this.commands.set(c.id, { title: c.title, run: asCommand(c.run), source: "core", hidden: false });
    this.menubar.listen();
    for (const c of this.menubar.commands()) this.commands.set(c.id, { title: c.title, run: asCommand(c.run), source: "core", hidden: true });
    for (const c of this.orchestrator.commands()) this.commands.set(c.id, { title: c.title, run: c.run, source: "core", hidden: false });
    this.orchestrator.listen();
    this.relayout();
    new ResizeObserver(() => { this.renderAll(); }).observe($("#editor-area"));
    setInterval(() => { this.autoSave(); }, 1000);
    if (!ws) return;
    // The plugins ([{name, activate}], see plugins/index.js), in order; one
    // that throws is reported and the rest still load.
    for (const { name, activate } of plugins) {
      try { activate(this); } catch (e) { this.report(`plugin ${name}: ${failure(e)}`); }
    }
    await this.terminals.adopt(ws.terminals);
    // What was open in this workspace last time, unless a file is named;
    // then an agent's terminal not open in a tab opens in one, in front.
    const open = this.params.get("open");
    if (!open) await this.restoreLayout();
    for (const id of this.terminals.agents()) if (this.terminalViews(id).length === 0) this.showTerminal(id, this.activePane);
    this.restoring = false;
    this.rememberLayout();
    window.addEventListener("pagehide", () => { this.rememberLayout(); this.session.flush(); });
    document.addEventListener("visibilitychange", () => { if (document.hidden) { this.rememberLayout(); this.session.flush(); } });
    this.host.emit("app.ready");
    if (open) await this.openFile(open, { line: null, column: 1, place: "tab", preview: null, focus: true });
    this.focusPane(this.activePane);
  }

  // The hidden input that takes the keyboard, and keys pressed with nothing
  // focused (after a click on the status bar, say), which go to the editor.
  listenInput() {
    const input = this.input;
    input.addEventListener("keydown", (e) => { this.onKey(e); });
    // The carets show only while the editor has the keyboard (not while a
    // terminal, a panel or the palette has it).
    const area = $("#editor-area");
    input.addEventListener("focus", () => { setClass(area, "keyboard", true); });
    input.addEventListener("blur", () => { setClass(area, "keyboard", false); });
    window.addEventListener("keydown", (e) => {
      if (e.target === document.body) { this.focusPane(this.activePane); this.onKey(e); return; }
      if (!e.defaultPrevented && this.panelKey(e)) e.preventDefault();
    });
    input.addEventListener("compositionstart", () => { this.composing = true; });
    input.addEventListener("compositionend", () => { this.composing = false; this.onInput(); });
    input.addEventListener("input", () => { if (!this.composing) this.onInput(); });
    input.addEventListener("paste", (e) => { e.preventDefault(); const data = e.clipboardData; if (data) this.paste(data.getData("text")); });
    input.addEventListener("copy", (e) => { this.onCopy(e, false); });
    input.addEventListener("cut", (e) => { this.onCopy(e, true); });
  }

  // ------------------------------------------------------------- the workspace's layout, remembered

  // What is open, for the next visit (session.js): the splits and their
  // ratios, each split's document tabs (a file by its path, an untitled
  // document by its number) with their cursor and scroll, the active split
  // and tab, and whether the sidebar and the terminal show.
  layoutSnapshot() {
    const tabs = (views) => views.flatMap((v) => tabRef(v.buf).map((ref) => savedTab(ref, v.primary.pos, v.top, v.preview)));
    const node = (n) => {
      const pane = paneAt(n), f = forkAt(n);
      if (pane !== null) return savedPane(tabs(pane.views), firstOf(pane.shown.flatMap((v) => tabKeys(v.buf))));
      return f !== null ? savedSplit(f.dir, f.size.ratio, node(f.a), node(f.b)) : savedPane([], null);
    };
    return { tree: node(this.layoutTree), active: panesOf(this.layoutTree).indexOf(this.activePane), sidebar: !$("#sidebar").classList.contains("hidden") };
  }
  rememberLayout() {
    if (this.restoring) return;
    this.session.setLayout(this.layoutSnapshot());
  }
  // What a tab shows, as the remembered layout names it: a file by its
  // path, an untitled document by its number, a terminal by its id (a
  // list of at most one: a plugin's buffer is not remembered).
  // Put the remembered layout back: the splits, then each split's
  // documents (one that cannot be opened any more is left out — an
  // untitled one is there only while the server keeps it for its unsaved
  // text — and a split left with none closes).
  async restoreLayout() {
    const s = this.session.layout;
    if (s === null) return;
    if (s.sidebar) $("#sidebar").classList.remove("hidden"); else $("#sidebar").classList.add("hidden");
    const specs = [];
    const build = (n) => {
      if (n.kind === "pane") { const pane = new Pane(this.paneEditor); pane.listen(); specs.push({ pane, tabs: n.tabs, active: n.active }); return leaf(pane); }
      return fork(n.dir === "v" ? "v" : "h", clamp(n.ratio || 0.5, 0.05, 0.95), build(n.a), build(n.b));
    };
    this.layoutTree = build(s.tree);
    this.activePane = specs[Math.max(0, Math.min(specs.length - 1, s.active | 0))].pane;
    this.relayout();
    for (const { pane, tabs, active: shown } of specs) {
      for (const t of tabs) {
        for (const b of await this.loadTab(t)) {
          const v = pane.show(b, t.preview);
          const pos = Math.max(0, Math.min(t.pos | 0, b.len));
          v.cursors = [cursorAt(pos)];
          v.setTop(Math.max(0, Math.min(t.top | 0, pos)), 0);
        }
      }
      for (const key of listOf(shown)) for (const v of pane.views.filter((x) => tabKeys(x.buf).includes(key)).slice(0, 1)) pane.show(v.buf, false);
    }
    for (const { pane } of specs) if (!pane.views.length && panesOf(this.layoutTree).length > 1) await this.closePane(pane);
    this.focusPane(this.activePane);
    this.renderAll();
  }

  // A remembered tab's buffer (a list of at most one: none when it cannot
  // be opened any more).
  async loadTab(t) {
    const ref = t.ref;
    try {
      if (ref.kind === "file") return [await this.loadFile(ref.path)];
      if (ref.kind === "untitled") return [await this.loadUntitled(ref.n)];
      return this.terminals.has(ref.term) ? [this.terminalBuffer(ref.term)] : [];
    } catch { return []; }
  }

  // Files opened lately in this workspace, the latest first.
  noteRecent(path) {
    this.session.setRecent([path, ...this.session.recent.filter((p) => p !== path)].slice(0, 30));
  }
  recentFiles() { return this.session.recent; }

  measure() {
    const s = document.createElement("span");
    s.className = "measure";
    s.textContent = "M".repeat(100);
    document.body.appendChild(s);
    const r = s.getBoundingClientRect();
    this.metrics = { cw: r.width / 100, lh: Math.ceil(r.height) };
    document.documentElement.style.setProperty("--lh", `${this.metrics.lh}px`);
    s.remove();
  }

  // After the editor font changes (a skin's density or fonts): measure the
  // cell again and redraw every view and terminal at it.
  remeasure() {
    const { cw, lh } = this.metrics;
    this.measure();
    this.terminals.applyFont();
    if (cw === this.metrics.cw && lh === this.metrics.lh) return;
    for (const p of panesOf(this.layoutTree)) p.clearRows();
    this.renderAll();
  }

  report(msg) {
    this.log.push(msg);
    console.error(msg);
    this.setStatus(msg.split("\n")[0]);
  }

  // ------------------------------------------------------------- panes

  panes() { return panesOf(this.layoutTree); } // for the plugin API
  relayout() {
    const area = $("#editor-area");
    area.innerHTML = "";
    const build = (n) => {
      const pane = paneAt(n), f = forkAt(n);
      if (pane !== null || f === null) return pane !== null ? pane.el : document.createElement("div");
      const size = f.size;
      const d = document.createElement("div");
      d.className = `split ${f.dir}`;
      const a = build(f.a), b = build(f.b);
      // Grow factors in thousandths: they sum past one, so a child left
      // alone (its sibling hidden by a maximized split) fills the split.
      const flex = () => { a.style.flex = `${size.ratio * 1000} 1 0`; b.style.flex = `${(1 - size.ratio) * 1000} 1 0`; };
      flex();
      // Its sash moves the line between them; the panes redraw at their
      // new sizes (each observes its own).
      const s = document.createElement("div");
      const x = f.dir === "h";
      sash(s, x ? "x" : "y", {
        begin: () => { const r = d.getBoundingClientRect(); return { at: size.ratio, size: x ? r.width : r.height }; },
        drag: (c, delta) => {
          const min = Math.min(0.45, 60 / Math.max(1, c.size));
          size.ratio = clamp(c.at + delta / Math.max(1, c.size), min, 1 - min);
          flex();
        },
        end: () => { this.rememberLayout(); },
        reset: () => { size.ratio = 0.5; flex(); this.rememberLayout(); },
      });
      d.append(a);
      d.append(s);
      d.append(b);
      return d;
    };
    const el = build(this.layoutTree);
    el.style.flex = "1 1 0";
    area.appendChild(el);
    for (const p of panesOf(this.layoutTree)) setClass(p.el, "active", p === this.activePane);
    this.showMaximized();
    this.renderAll();
    this.rememberLayout();
  }
  // The sashes of the sidebar, the right dock and the bottom dock; their
  // sizes are remembered for the workspace.
  installSashes() {
    const parts = [
      { name: "left", el: $("#left-dock"), axis: "x", dir: 1, min: 160, most: 0.4 },
      { name: "sidebar", el: $("#sidebar"), axis: "x", dir: 1, min: 120, most: 0.5 },
      { name: "right", el: $("#right-dock"), axis: "x", dir: -1, min: 160, most: 0.6 },
      { name: "dock", el: $("#dock"), axis: "y", dir: -1, min: 80, most: 0.8 },
    ];
    const sizeOf = (name) => this.session.sizes[name];
    const setSize = (name, px) => { this.session.setSizes(Object.fromEntries([...Object.entries(this.session.sizes).filter(([k]) => k !== name), [name, px]])); };
    const prop = (p) => (p.axis === "x" ? "width" : "height");
    for (const p of parts) {
      const name = p.name;
      const kept = sizeOf(name);
      if (kept) p.el.style[prop(p)] = `${kept}px`;
      for (const el of listOf(document.querySelector(`.sash[data-sash="${name}"]`))) sash(el, p.axis, {
        // The most a part takes is a share of its container.
        begin: () => {
          const r = p.el.getBoundingClientRect(), box = p.el.parentElement.getBoundingClientRect();
          return { at: p.axis === "x" ? r.width : r.height, max: (p.axis === "x" ? box.width : box.height) * p.most };
        },
        drag: (c, d) => { const px = Math.round(clamp(c.at + p.dir * d, p.min, Math.max(p.min, c.max))); p.el.style[prop(p)] = `${px}px`; setSize(name, px); },
        end: () => {},
        reset: () => {
          p.el.style[prop(p)] = "";
          this.session.setSizes(Object.fromEntries(Object.entries(this.session.sizes).filter(([k]) => k !== name)));
        },
      });
    }
  }
  renderAll() { for (const p of panesOf(this.layoutTree)) { p.renderTabs(); p.scheduleRender(); } }
  renderBuffer(id) { for (const p of panesOf(this.layoutTree)) if (p.shown.some((v) => v.buf.id === id)) p.scheduleRender(); }
  // The active view (none: the active split shows nothing).
  view() { return this.activePane.shown.length > 0 ? some(this.activePane.view) : none(); }
  // Where the active view is, to go back to (a list of at most one):
  // remembered for Go Back, or restored.
  here() {
    if (this.activePane.shown.length === 0) return [];
    const v = this.activePane.shown[0];
    const pos = v.primary.pos, top = v.top;
    return [spot(() => { this.pushHistory(v, pos); }, () => { v.setTop(top, 0); this.editing.place(v.pane, pos, false); })];
  }
  focusPane(p) {
    this.scrollDriver = [];
    const changed = this.activePane !== p;
    this.activePane = p;
    for (const x of panesOf(this.layoutTree)) setClass(x.el, "active", x === p);
    this.giveKeyboard(p);
    // A maximized split gives way to the one focused.
    if (this.maximized.length > 0 && !this.maximized.includes(p)) { this.maximized = [p]; this.showMaximized(); }
    if (changed) { this.renderAll(); for (const v of p.shown) this.bufferEvent("buffer.activated", v.buf, [], false); }
  }
  // ------------------------------------------------------------- terminals, as tabs

  // The tab buffer of terminal `id` (made the first time): kept while no
  // tab shows it (a split closed, or Terminal: Toggle hid it).
  terminalBuffer(id) {
    const had = [...this.buffers.values()].find((b) => terminalOf(b.source).includes(id));
    if (had !== undefined) return had;
    const b = terminalBuffer(id, this.terminals.label(id));
    b.keep = true;
    this.buffers.set(b.id, b);
    return b;
  }
  // The tabs showing terminal `id`.
  terminalViews(id) { return panesOf(this.layoutTree).flatMap((p) => p.views.filter((v) => terminalOf(v.buf.source).includes(id))); }
  // Show terminal `id` in a tab of split `p`, which takes the keyboard.
  showTerminal(id, p) {
    p.show(this.terminalBuffer(id), false);
    this.focusPane(p);
    this.rememberLayout();
  }
  // The split terminals go to: the active one if it shows a terminal, else
  // the last one that does, else a new one under the active split.
  terminalPane() {
    const showing = (p) => p.shown.some((v) => terminalOf(v.buf.source).length > 0);
    if (showing(this.activePane)) return this.activePane;
    return panesOf(this.layoutTree).filter(showing).at(-1) ?? this.split("v", 0.65, []);
  }
  // Terminal: Toggle. From a terminal, back to the editor (its split
  // closes, its terminals kept, when there is another); to a terminal: the
  // split showing one, else one not shown (a new one if none) in a split
  // under the editor.
  async toggleTerminal() {
    const p = this.activePane;
    if (p.shown.some((v) => terminalOf(v.buf.source).length > 0)) {
      if (panesOf(this.layoutTree).length > 1 && p.views.every((v) => terminalOf(v.buf.source).length > 0)) await this.closePane(p);
      return;
    }
    const showing = panesOf(this.layoutTree).filter((x) => x.shown.some((v) => terminalOf(v.buf.source).length > 0)).at(-1);
    if (showing !== undefined) { this.focusPane(showing); return; }
    const hidden = this.terminals.ids().filter((id) => this.terminalViews(id).length === 0);
    let id = hidden.at(0);
    if (id === undefined) {
      try { id = await this.terminals.create(null, null, null); } catch (e) { this.setStatus(`No terminal: ${failure(e)}`); return; }
    }
    this.showTerminal(id, this.terminalPane());
    for (const other of hidden.slice(1)) this.terminalPane().show(this.terminalBuffer(other), false);
  }
  // A terminal running `command` (an agent; `resume` takes it up again
  // later), in a tab of the active split.
  async runInTerminal(command, resume) {
    this.showTerminal(await this.terminals.create(null, command, resume), this.activePane);
  }
  // Terminal `id`'s label changed: its tabs say so.
  terminalChanged(id) {
    for (const b of this.buffers.values()) if (terminalOf(b.source).includes(id)) b.name = this.terminals.label(id);
    for (const p of panesOf(this.layoutTree)) if (p.views.some((v) => terminalOf(v.buf.source).includes(id))) p.renderTabs();
  }
  // Another client opened terminal `id`: it gets a tab here too, in the
  // split showing terminals (not taking the keyboard).
  terminalAppeared(id) {
    if (this.terminalViews(id).length > 0) return;
    const p = this.terminalPane();
    p.show(this.terminalBuffer(id), false);
    p.scheduleRender();
  }

  // Split the active split in direction `dir` ("h", "v"), the new one
  // `ratio` of it, showing `shown` (a list of at most one buffer).
  split(dir, ratio, shown) {
    const old = this.activePane;
    for (const v of old.shown) this.keep(v);
    const np = new Pane(this.paneEditor);
    np.listen();
    this.layoutTree = withSplit(this.layoutTree, old, np, dir, ratio);
    for (const b of shown) np.show(b, false);
    this.activePane = np;
    this.relayout();
    this.focusPane(np);
    return np;
  }
  // Split the active split in half, showing its buffer in both.
  splitActive(dir) {
    return this.split(dir, 0.5, this.activePane.shown.map((v) => v.buf));
  }
  // Close a split. Files whose last views are in it and have unsaved
  // changes are asked about first (see `mayClose`).
  // One split filling the editor area (the others hidden, not closed), or
  // none.
  showMaximized() {
    const panes = panesOf(this.layoutTree);
    this.maximized = this.maximized.filter((m) => panes.includes(m));
    setClass($("#editor-area"), "maximized", this.maximized.length > 0);
    for (const p of panes) setClass(p.el, "max", this.maximized.includes(p));
  }
  async closePane(p) {
    if (panesOf(this.layoutTree).length === 1) return;
    for (const b of new Set(p.views.map((v) => v.buf))) if (!(await this.mayClose(b, p.views))) return;
    this.layoutTree = without(this.layoutTree, p);
    for (const v of [...p.views]) { p.untrack(v); this.dropIfUnshown(v.buf); }
    this.activePane = panesOf(this.layoutTree)[0];
    this.relayout();
    this.focusPane(this.activePane);
  }
  // Close a tab; resolves false when the user cancelled.
  async closeTab(p, v) {
    if (!(await this.mayClose(v.buf, [v]))) return false;
    if (!p.views.includes(v)) return true; // closed meanwhile
    p.closeView(v);
    // A terminal's last tab closed closes the terminal.
    terminalOf(v.buf.source).forEach((id) => { if (this.terminalViews(id).length === 0) { this.buffers.delete(v.buf.id); this.terminals.close(id); } });
    this.dropIfUnshown(v.buf);
    if (!p.views.length && panesOf(this.layoutTree).length > 1) await this.closePane(p);
    this.renderAll();
    this.updateStatus();
    this.rememberLayout();
    return true;
  }
  // Whether closing `views` may let go of buffer `b`. When they are the
  // last views of a file with unsaved changes, the user picks: Save (and
  // close once saved), Discard (the document is closed with `discard`, see
  // `dropIfUnshown`), or Cancel.
  async mayClose(b, views) {
    if (!b.doc || !b.modified) return true;
    if (panesOf(this.layoutTree).some((p) => p.views.some((v) => v.buf === b && !views.includes(v)))) return true;
    const pick = await this.choose(`${b.name} has unsaved changes`, [
      { label: "Save", value: "save" },
      { label: "Discard", value: "discard" },
      { label: "Cancel", value: "cancel" },
    ]);
    if (pick === "save") { await this.saveBuffer(b, true); return !b.modified; }
    if (pick === "discard") { b.discard = true; return true; }
    return false;
  }
  // Let go of a buffer no view shows. A file's document is closed; with
  // `discard` its unsaved changes go too, unless another client holds it.
  dropIfUnshown(b) {
    if (panesOf(this.layoutTree).some((x) => x.views.some((y) => y.buf === b))) return;
    if (b.keep) return;
    this.buffers.delete(b.id);
    for (const doc of listOf(b.doc)) {
      this.docs.delete(doc);
      const discard = b.discard;
      rpc("doc.close", { doc, discard }).then((r) => {
        if (!discard) return;
        if (r.dropped) this.setStatus(`Discarded the changes to ${b.name}`);
        else if (r.others) this.setStatus(`${b.name} is open in ${r.others} other window${r.others === 1 ? "" : "s"}; its unsaved changes stay there`);
      }).catch((e) => { this.report(failure(e)); });
    }
    this.bufferEvent("buffer.closed", b, [], false);
  }
  async closeBuffer(b) {
    for (const p of panesOf(this.layoutTree)) for (const v of [...p.views]) if (v.buf === b && !(await this.closeTab(p, v))) return false;
    if (!(await this.mayClose(b, []))) return false;
    this.dropIfUnshown(b);
    return true;
  }
  // The keyboard to what split `p` shows: a terminal's tab gives it to the
  // terminal, any other to the editor.
  giveKeyboard(p) {
    const terms = p.shown.filter((v) => terminalOf(v.buf.source).length > 0);
    if (terms.length > 0) terms.forEach((v) => { terminalOf(v.buf.source).forEach((id) => { this.terminals.focus(id); }); });
    else { this.terminals.unfocus(); this.input.focus({ preventScroll: true }); }
  }
  onViewShown(pane, v) {
    this.rememberLayout();
    if (pane === this.activePane) { this.giveKeyboard(pane); this.bufferEvent("buffer.activated", v.buf, [], false); }
    this.updateStatus();
  }
  // Show a buffer: in the active pane ("tab"), a new split below ("split")
  // or to the right ("right").
  // `preview` and `focus` as for `openFile`; preview tabs are a setting.
  showBuffer(b, place, { preview, focus }) {
    if (place === "split" || place === "right") {
      const existing = panesOf(this.layoutTree).filter((p) => p.shown.some((v) => v.buf === b));
      for (const p of existing.slice(0, 1)) { this.focusPane(p); return p.shown; }
      return this.split(place === "split" ? "v" : "h", 0.6, [b]).shown;
    }
    const v = this.activePane.show(b, preview === true && this.settings.flags.preview_tabs === true);
    // Opened to stay (not as a preview): a preview tab of it is kept.
    if (preview === false) this.keep(v);
    if (focus) this.focusPane(this.activePane);
    return [v];
  }
  // A preview tab made a tab of its own (it was edited, clicked, split or
  // kept open).
  keep(v) {
    if (!v.keepOpen()) return;
    v.pane.renderTabs();
    this.rememberLayout();
  }

  // ------------------------------------------------------------- scrolling together

  // After split `pane` drew view `v`: when it leads (the focused split, or
  // one the pointer scrolled since), the other splits follow its first
  // line — those showing the same buffer while Scroll Sync is on (the
  // editor's same-buffer sync), and those showing a buffer linked to it
  // (a comparison's two sides) always, through the link's line mapping.
  syncScroll(pane, v) {
    if (pane !== (this.scrollDriver.at(0) ?? this.activePane) || v.mode !== "source") return;
    for (const L of listOf(v.buf.text)) {
      const line = L.indexOf(v.top);
      if (line < 0) return;
      for (const p of panesOf(this.layoutTree)) {
        if (p === pane) continue;
        for (const w of p.shown) {
          if (w.mode !== "source") continue;
          for (const M of listOf(w.buf.text)) {
            // Where its top goes: the same line (the same buffer), or the
            // linked one (its link's mapping, both held whole).
            const links = this.scrollLinks.filter((k) => (k.a === v.buf && k.b === w.buf) || (k.b === v.buf && k.a === w.buf));
            const tops = w.buf === v.buf
              ? (this.settings.flags.scroll_sync === true ? [v.top] : [])
              : links.slice(0, v.buf.full && w.buf.full ? 1 : 0).map((k) => M.starts[Math.max(0, Math.min(M.count - 1, k.a === v.buf ? k.aToB(line) : k.bToA(line)))]);
            for (const top of tops) if (top !== w.top) { w.setTop(top, 0); p.scheduleRender(); }
          }
        }
      }
    }
  }
  // Scroll buffers `a` and `b` together through a line mapping (each
  // function takes a line of one and gives the other's); returns the undo.
  linkScroll(a, b, aToB, bToA) {
    const link = { a, b, aToB, bToA };
    this.scrollLinks.push(link);
    return () => { this.scrollLinks = this.scrollLinks.filter((k) => k !== link); };
  }

  // ------------------------------------------------------------- tabs

  // A press on a tab shows it. Dragged a few pixels, the tab moves: along
  // its strip to where it is dropped, or to another split — onto its tabs
  // (at that place) or its text (at the end). The drop place is marked on
  // the tab it lands beside (`.drop-before` / `.drop-after`) or the split
  // (`.drop-into`).
  tabPress(e, pane, v) {
    this.keep(v);
    pane.show(v.buf, false);
    this.focusPane(pane);
    const x0 = e.clientX, y0 = e.clientY;
    // Where it would drop: a list of at most one place.
    let dragging = false, target = [];
    const unmark = () => { for (const t of target) t.el.classList.remove(`drop-${t.side}`); };
    const move = (ev) => {
      if (!dragging && Math.hypot(ev.clientX - x0, ev.clientY - y0) < 5) return;
      if (!dragging) { dragging = true; document.documentElement.classList.add("tab-dragging"); }
      unmark();
      target = this.tabDropAt(ev.clientX, ev.clientY);
      for (const t of target) t.el.classList.add(`drop-${t.side}`);
    };
    const up = (_ev) => {
      window.removeEventListener("mousemove", move);
      window.removeEventListener("mouseup", up);
      document.documentElement.classList.remove("tab-dragging");
      unmark();
      if (dragging) for (const t of target) this.moveView(v, t.pane, t.index);
    };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
  }
  // Where a tab dropped at (x, y) goes: {pane, index, el, side}, in a list
  // of at most one (none: not on a split).
  tabDropAt(x, y) {
    const el = document.elementFromPoint(x, y);
    const paneEl = el === null ? null : el.closest(".pane");
    return panesOf(this.layoutTree).filter((p) => paneEl !== null && p.el === paneEl).map((pane) => {
      const tab = el === null ? null : el.closest(".tab");
      if (tab !== null && pane.tabsEl.contains(tab)) {
        const r = tab.getBoundingClientRect();
        const after = x > r.left + r.width / 2;
        return { pane, index: Number(tab.dataset.i) + (after ? 1 : 0), el: tab, side: after ? "after" : "before" };
      }
      return { pane, index: pane.views.length, el: pane.el, side: "into" };
    });
  }
  // Move view `v` to place `index` among the tabs of `to`. In another split
  // it is a new view of the buffer there, keeping its cursors, scroll,
  // mode and folds (or that split's own view of it, when it has one); a
  // split left with no tabs closes.
  moveView(v, to, index) {
    const from = v.pane;
    if (from === to) {
      const i = from.views.indexOf(v);
      if (i < 0) return;
      from.views.splice(i, 1);
      from.views.splice(index > i ? index - 1 : index, 0, v);
      from.renderTabs();
      this.rememberLayout();
      return;
    }
    if (!to.views.some((x) => x.buf === v.buf)) copyTab(v, to, index);
    to.show(v.buf, false);
    from.closeView(v);
    if (from.views.length === 0 && panesOf(this.layoutTree).length > 1) this.closePane(from);
    this.focusPane(to);
    this.renderAll();
    this.rememberLayout();
  }
  // Close the tabs of `pane` that `which(view, index)` picks, one at a
  // time (each may ask about unsaved changes; a cancel stops there).
  async closeTabs(pane, which) {
    const list = pane.views.filter((x, i) => which(x, i));
    for (const x of list) if (pane.views.includes(x) && !(await this.closeTab(pane, x))) return;
  }
  // A tab's menu: closing it and its neighbours, splitting it, its path.
  tabMenu(pane, v, at) {
    const b = v.buf;
    const copy = (t) => navigator.clipboard?.writeText(t).then((_r) => this.setStatus(`Copied ${t}`)).catch((_e) => this.setStatus("The browser did not allow writing the clipboard"));
    this.ui.menu([
      ...(v.preview ? [action("Keep Open", null, () => this.keep(v))] : []),
      action("Close", this.keyLabel("view.closeTab"), () => this.closeTab(pane, v)),
      action("Close Others", null, () => this.closeTabs(pane, (x, _i) => x !== v)),
      actionOf("Close to the Right", null, null, pane.views.indexOf(v) === pane.views.length - 1, () => this.closeTabs(pane, (x, i) => i > pane.views.indexOf(v))),
      action("Close Saved", null, () => this.closeTabs(pane, (x, _i) => !x.buf.modified)),
      action("Close All", null, () => this.closeTabs(pane, (_x, _i) => true)),
      SEPARATOR,
      action("Split Right", this.keyLabel("view.splitRight"), () => { this.focusPane(pane); this.splitActive("h"); }),
      action("Split Down", null, () => { this.focusPane(pane); this.splitActive("v"); }),
      ...filePath(b.origin).flatMap((path) => [
        SEPARATOR,
        action("Copy Path", null, () => copy(path)),
        action("Copy Relative Path", null, () => copy(shownFrom(this.root, path))),
        action("Reveal in Explorer", null, () => this.run("explorer.reveal", undefined)),
      ]),
    ], at);
  }
  // The editor's menu: the clipboard, the language server's actions on
  // what is under the caret, and editing the selection.
  editorMenu(pane, at) {
    this.focusPane(pane);
    if (pane.shown.length === 0) return;
    const b = pane.shown[0].buf;
    const lsp = this.lspAvailable(b);
    // A command's row, its title without its group ("Edit: ").
    const row = (id) => {
      const c = this.commands.get(id);
      return rowOf(id, c ? c.title.replace(/^(Edit|LSP|File|View): /, "") : null, CLIPBOARD_KEYS.get(id) ?? null, null);
    };
    this.ui.menu(this.menubar.resolve([
      ...["edit.cut", "edit.copy", "edit.paste"].map(row),
      SEP,
      ...(lsp ? [...["lsp.definition", "lsp.references", "lsp.rename", "lsp.codeActions", "lsp.hover"].map(row), SEP, row("lsp.format")] : []),
      ...["edit.toggleComment", "edit.duplicateLine", "edit.deleteLine"].map(row),
      SEP,
      ...["select.all", "select.allOccurrences"].map(row),
      ...(b.doc ? [SEP, row("file.revert")] : []),
    ]), at);
  }

  // ------------------------------------------------------------- buffers

  watchBuffer(b) {
    b.listeners.add((told) => {
      const news /*: BufferNews */ = told;
      if (news.kind === "edit") this.afterEdit(b, news.op, news.local);
      else if (news.kind === "text") { this.bufferEvent("buffer.changed", b, [], false); this.renderBuffer(b.id); }
      else if (news.kind === "error") this.setStatus(news.message);
      else if (news.kind === "refused") { this.setStatus(news.message); this.reloadedDoc(b, news.info); }
    });
  }

  // The buffer for a file, opening its document without showing it.
  async loadFile(path) {
    const abs = path.startsWith("/") ? path : `${this.root}/${path}`;
    const open = [...this.buffers.values()].find((x) => filePath(x.origin).includes(abs));
    if (open) return open;
    return this.adoptDoc(await rpc("doc.open", { path: abs }));
  }
  // The buffer for an untitled document the server keeps (the page was
  // reloaded with its unsaved text).
  async loadUntitled(n) {
    const open = [...this.buffers.values()].find((x) => untitledNumber(x.origin).includes(n));
    if (open !== undefined) return open;
    return this.adoptDoc(await rpc("doc.open", { untitled: n }));
  }
  // A new untitled document, shown: an empty buffer bound to no file,
  // named Untitled-N, until it is saved as one.
  async newUntitled() {
    const b = await this.adoptDoc(await rpc("doc.new", {}));
    this.showBuffer(b, "tab", { preview: null, focus: true });
    return b;
  }
  // The buffer of an opened document ({doc, path, ...} from the server).
  async adoptDoc(info) {
    const known = this.docs.get(info.doc);
    if (known !== undefined) return known;
    const b = fileBuffer(info);
    this.buffers.set(b.id, b);
    this.docs.set(info.doc, b);
    this.watchBuffer(b);
    await b.ensure(0, ENSURE_AFTER);
    this.bufferEvent("buffer.opened", b, [], false);
    return b;
  }

  // Show a file, as `at` says (OpenOptions, types/fresh.d.js). A
  // navigation: Go Back returns to where the active view was. A file shown
  // in a tab of its own (`preview` false) keeps a preview tab showing it.
  // Files open in the order they were asked for (a click's preview, then
  // the double-click's keep).
  async openFile(path, at) {
    const { line, column, place, preview, focus } = at;
    const turn = this.opening;
    let done = () => {};
    this.opening = new Promise((r) => { done = () => { r(undefined); }; });
    try {
      await turn;
      const from = this.activePane.shown;
      const b = await this.loadFile(path);
      for (const f of from) this.pushHistory(f, f.primary.pos);
      const v = this.showBuffer(b, place, { preview, focus });
      for (const p of filePath(b.origin)) this.noteRecent(p);
      for (const l of listOf(line)) await this.gotoLine(l - 1, column - 1, false);
      this.updateStatus();
      return v;
    } catch (e) {
      this.setStatus(failure(e));
      return [];
    } finally {
      done();
    }
  }

  // A virtual buffer (`o`: VirtualOptions, types/fresh.d.js).
  createVirtual(o) {
    const b = virtualBuffer(o.name, o.entries, o.mode, o.readOnly);
    b.language = o.language || null;
    b.keep = o.keep;
    this.buffers.set(b.id, b);
    this.watchBuffer(b);
    if (o.place) {
      for (const v of this.showBuffer(b, o.place, { preview: null, focus: true })) if (!o.lineNumbers) v.opts.lineNumbers = false;
    }
    return b;
  }

  applyEdit(b, ops) { this.applyTyped(b, ops, "none"); }
  // `typing` ("start", "continue" or "none") groups a run of typed
  // characters into one undo step (see buffer.js).
  applyTyped(b, ops, typing) {
    if (b.readOnly) { this.setStatus("Read-only buffer"); return; }
    b.edit(ops, true, typing);
  }

  // Whether a menu's check is on: a setting, or a piece of the editor's
  // state by name.
  isOn(kind, name) { return kind === "setting" ? this.settings.flags[name] === true : this.stateOn(name); }
  stateOn(name) {
    const v = this.activePane.shown;
    if (name === "readOnly") return v.some((x) => x.buf.readOnly);
    if (name === "lineNumbers") return v.some((x) => x.opts.lineNumbers);
    if (name === "wrap") return v.some((x) => x.opts.wrap);
    if (name === "compose") return v.some((x) => x.mode === "compose");
    if (name === "orchestrator") return this.orchestrator.dockOpen();
    if (name === "explorer") return !$("#sidebar").classList.contains("hidden");
    if (name === "terminal") return panesOf(this.layoutTree).some((p) => p.shown.some((x) => terminalOf(x.buf.source).length > 0));
    if (name === "keymapDefault") return this.settings.keymap !== "emacs";
    if (name === "keymapEmacs") return this.settings.keymap === "emacs";
    return false;
  }

  // The open files, and the problems the language servers found in them.
  counts() {
    const bufs = [...this.buffers.values()];
    const diags = bufs.flatMap((b) => b.decor.diags);
    return { files: bufs.filter((b) => !b.isVirtual).length, errors: diags.filter((x) => x.severity === 1).length, warnings: diags.filter((x) => x.severity === 2).length };
  }
  // The editor theme `key`, when a theme plugin is there to put it on.
  async useTheme(key) { if (this.commands.has("theme.use")) await this.run("theme.use", key); }

  // Buffer `b`'s event to the plugins (`ops`: what changed, `local`: made
  // here), and a view's.
  bufferEvent(event, b, ops, local) { this.host.emitBuffer(event, bufferHandle(this, b), { ops, local }); }
  viewEvent(event, v) { this.host.emitView(event, viewHandle(this, v)); }

  afterEdit(b, op, local) {
    // Editing a preview tab's file keeps it.
    if (local) for (const v of this.viewsOf(b)) this.keep(v);
    this.bufferEvent("buffer.changed", b, [op], local);
    this.renderBuffer(b.id);
    for (const p of panesOf(this.layoutTree)) if (p.views.some((v) => v.buf === b)) p.renderTabs();
  }

  // Save a file. The server refuses when the file changed on disk since it
  // was opened or last saved; an explicit save (`ask`) then offers to
  // overwrite it, auto-save leaves it and says so.
  // `ask`: a file changed on disk asks whether to overwrite it.
  async saveBuffer(b, ask) {
    if (b.file.length === 0) return;
    // An untitled document is saved as a file, asked for.
    if (filePath(b.origin).length === 0) { await this.saveBufferAs(b); return; }
    try {
      // Keeping one's changes over the file's (`askChangedOnDisk`) was the
      // choice to overwrite it.
      const refused = await b.save(b.keepMine);
      b.keepMine = false;
      for (const r of refused) {
        if (!ask || r.code !== "changed_on_disk") { this.setStatus(`Not saved: ${r.detail}`); return; }
        if (!(await this.confirm(`${b.name} changed on disk since it was opened. Overwrite it?`, "Overwrite"))) { this.setStatus(`Not saved: ${b.name}`); return; }
        for (const again of await b.save(true)) { this.setStatus(`Not saved: ${again.detail}`); return; }
      }
    } catch (e) { this.setStatus(`Not saved: ${failure(e)}`); return; }
    this.setStatus(`Saved ${b.name}`);
    this.renderAll();
    this.bufferEvent("buffer.saved", b, [], false);
  }

  // Save As: a document is written to the file asked for and bound to it
  // (`doc.save_as`: an untitled one gets its file, a file's moves to the
  // new one and the old file stays as last saved); a file there is
  // overwritten once confirmed. A buffer with no document (a plugin's
  // panel) has its text written to the file, which then opens.
  async saveBufferAs(b) {
    const rel = (p) => shownFrom(this.root, p);
    const name = (await this.prompt.ask({ title: "Save as", value: filePath(b.origin).map(rel).at(0) ?? b.name, hint: "", select: true })) ?? "";
    if (name === "") return;
    const path = name.startsWith("/") ? name : `${this.root}/${name}`;
    if (filePath(b.origin).includes(path)) { await this.saveBuffer(b, true); return; }
    const st = await rpc("fs.stat", { path });
    if (st.dir) { this.setStatus(`${name} is a folder`); return; }
    if (st.exists && !(await this.confirm(`${name} exists. Overwrite it?`, "Overwrite"))) return;
    try {
      if (b.file.length > 0) {
        const f = b.file[0];
        await whenSynced(f);
        const info = await rpc("doc.save_as", { doc: b.doc, path, overwrite: st.exists });
        b.setInfo(info);
        b.modified = info.modified;
        for (const p of filePath(b.origin)) this.noteRecent(p);
        this.renderAll();
        this.updateStatus();
        this.bufferEvent("buffer.saved", b, [], false);
      } else {
        await rpc("fs.write", { path, text: await b.textAsync(0, b.len) });
        await this.openFile(path, { line: null, column: 1, place: "tab", preview: null, focus: true });
      }
    } catch (e) { this.setStatus(`Not saved: ${failure(e)}`); return; }
    this.setStatus(`Saved as ${name}`);
  }

  async askChangedOnDisk(b) {
    const pick = await this.choose(`${b.name} changed on disk, and you have unsaved changes`, [
      { label: "Reload (discard my changes)", value: "reload" },
      { label: "Keep mine (the next save overwrites the file)", value: "keep" },
    ]);
    if (pick === "reload") {
      try {
        await rpc("doc.reload", { doc: b.doc });
        this.setStatus(`Reloaded ${b.name}`);
      } catch (e) { this.setStatus(`Not reloaded: ${failure(e)}`); }
    } else if (pick === "keep") {
      b.keepMine = true;
      this.setStatus(`Keeping your changes to ${b.name}; saving overwrites the file`);
    }
  }

  autoSave() {
    if (this.settings.flags.auto_save !== true) return;
    const now = Date.now();
    for (const b of this.buffers.values()) {
      // An untitled document waits for a name.
      if (filePath(b.origin).length > 0 && b.modified && b.file.some(synced) && now - b.lastEdit > this.settings.auto_save_seconds * 1000) {
        b.lastEdit = now;
        this.saveBuffer(b, false);
      }
    }
  }

  cursorMoved(v, old) {
    this.viewEvent("cursor.moved", v);
    hidePopup();
    this.updateStatus();
  }

  // Move a view's cursor to `pos`, loading the text there first.
  // `center`: scroll it to the middle; `extend`: extend the selection;
  // `history`: remember where the view was, for Go Back.
  async moveTo(v, pos, { center, extend, history }) {
    if (history) this.pushHistory(v, v.primary.pos);
    const pane = v.pane;
    await v.buf.ensure(pos, pane.metrics().rows + 10);
    if (!pane.shown.includes(v)) pane.show(v.buf, false);
    this.editing.place(pane, pos, extend);
    if (center) pane.reveal(true);
    pane.scheduleRender();
  }

  async gotoByte(p, extend) {
    if (this.activePane.shown.length === 0) return;
    const v = this.activePane.shown[0];
    await this.moveTo(v, p, { center: !extend && Math.abs(p - v.primary.pos) > 5000, extend, history: false });
  }

  // `history`: remember where the view was, for Go Back.
  // Go to line `line` and column `col` (both from 0); `history`: remember
  // where the cursor was.
  async gotoLine(line, col, history) {
    if (this.activePane.shown.length === 0) return;
    const v = this.activePane.shown[0];
    // Only the latest navigation lands: a newer one (the palette previews a
    // jump on every keystroke) supersedes any still waiting on the server.
    this.navSeq += 1;
    const seq = this.navSeq;
    const b = v.buf;
    // Where the line starts (none: not known yet).
    let start = noBytes();
    const held = b.text;
    if (held !== null && b.full) start = [held.starts[Math.max(0, Math.min(line, held.count - 1))]];
    else for (const doc of listOf(b.doc)) {
      const r = await rpc("doc.line_start", { doc, line });
      if (seq !== this.navSeq) return;
      if (r.pos === null) { this.setStatus("The line index is still being built"); return; }
      start = [r.pos];
    }
    for (const s of start) {
      await b.ensure(s, 100);
      if (seq !== this.navSeq) return;
      for (const L of listOf(b.text)) {
        const p = s + byteLen(L.lines[L.indexOf(s)].slice(0, col));
        if (history) this.pushHistory(v, v.primary.pos);
        await this.moveTo(v, p, { center: true, extend: false, history: false });
      }
    }
  }

  // ------------------------------------------------------------- position history

  pushHistory(v, pos) {
    const h = this.history;
    const top = h.back.at(-1);
    if (top !== undefined && top.buf === v.buf && top.pos === pos) return;
    h.back.push({ buf: v.buf, pos });
    if (h.back.length > 100) h.back.shift();
    h.fwd = [];
  }
  async historyStep(dir) {
    const h = this.history;
    const from = dir < 0 ? h.back : h.fwd, to = dir < 0 ? h.fwd : h.back;
    const e = from.pop();
    if (e === undefined) return;
    const here = this.activePane.shown;
    // An entry for a closed buffer, or for where the view already is, is
    // passed over.
    if (!this.buffers.has(e.buf.id) || here.some((v) => v.buf === e.buf && v.primary.pos === e.pos)) { await this.historyStep(dir); return; }
    for (const v of here) to.push({ buf: v.buf, pos: v.primary.pos });
    for (const nv of this.showBuffer(e.buf, "tab", { preview: null, focus: true })) await this.moveTo(nv, Math.min(e.pos, e.buf.len), { center: true, extend: false, history: false });
  }

  // ------------------------------------------------------------- server events

  // The server's news, in order.
  onEvent(news) {
    const ev /*: ServerEvent */ = news;
    if (ev.ev === "doc.ops") {
      const b = this.docs.get(ev.doc);
      if (b) { b.onOps(ev, clientId()); this.renderBuffer(b.id); }
      return;
    }
    if (ev.ev === "doc.indexing") {
      const b = this.docs.get(ev.doc);
      if (b) this.setStatus(`Indexing lines of ${b.name}: ${Math.round((100 * ev.done) / Math.max(1, ev.total))}%`);
      return;
    }
    if (ev.ev === "doc.indexed") {
      // The document has a line index now: fetch the window again, and it
      // comes with line numbers.
      for (const b of found(this.docs.get(ev.doc))) {
        this.reindexed(b, ev.lines);
        this.setStatus(`${b.name}: ${ev.lines} lines`);
      }
      return;
    }
    if (ev.ev === "doc.index_failed") {
      // Its lines stay without numbers (a huge file's count failed).
      for (const b of found(this.docs.get(ev.doc))) this.setStatus(`${b.name}: lines not counted: ${ev.why}`);
      return;
    }
    if (ev.ev === "doc.saved") {
      const b = this.docs.get(ev.doc);
      // Saved, or reloaded (its encoding may be another now).
      if (b) { b.setInfo(ev.info); this.renderAll(); this.updateStatus(); }
      return;
    }
    if (ev.ev === "doc.renamed") {
      // The file was renamed or moved (here or by another client): the
      // buffer follows it.
      const b = this.docs.get(ev.doc);
      if (!b) return;
      b.setInfo(ev.info);
      this.renderAll();
      this.updateStatus();
      this.bufferEvent("buffer.renamed", b, [], false);
      return;
    }
    if (ev.ev === "doc.info") {
      // Its language, line ending or encoding changed (`doc.set`).
      const b = this.docs.get(ev.doc);
      if (!b) return;
      b.setInfo(ev.info);
      this.renderAll();
      this.updateStatus();
      this.bufferEvent("buffer.info", b, [], false);
      return;
    }
    if (ev.ev === "doc.reloaded") {
      // A huge file changed on disk and was loaded again.
      const b = this.docs.get(ev.doc);
      if (!b) return;
      this.reloadedDoc(b, ev.info);
      this.setStatus(ev.lost ? `${b.name} changed on disk and was reloaded; its unsaved changes are lost` : `${b.name} changed on disk and was reloaded`);
      return;
    }
    if (ev.ev === "doc.changed_on_disk") {
      // Changed on disk under unsaved changes: reload the file (giving
      // them up), or keep them, and the next save overwrites the file.
      const b = this.docs.get(ev.doc);
      if (b) this.askChangedOnDisk(b);
      return;
    }
    if (ev.ev === "doc.deleted") {
      // The file was deleted: the buffer keeps its text, unsaved, and
      // saving writes the file again.
      const b = this.docs.get(ev.doc);
      if (!b) return;
      b.setInfo(ev.info);
      this.renderAll();
      this.setStatus(`${b.name} was deleted; its text is kept unsaved (save to write it again)`);
      this.bufferEvent("buffer.deleted", b, [], false);
      return;
    }
    if (ev.ev === "lsp.diagnostics") {
      const b = this.docs.get(ev.doc);
      if (!b) return;
      // Anchored at the version they describe, then carried through every
      // edit applied since, ours included. Diagnostics older than the log
      // kept here can't be placed: the next ones replace them.
      const since = b.file.flatMap((f) => opsSince(f, ev.version));
      if (since.length === 0) return;
      const items = ev.items.map((d) => ({ ...d }));
      for (const ops of since) for (const op of ops) for (const d of items) shiftRange(d, op);
      b.decor.setDiagnostics(items);
      this.renderBuffer(b.id);
      this.bufferEvent("diagnostics.changed", b, [], false);
      this.updateStatus();
      return;
    }
    if (ev.ev === "term.data") { this.terminals.onEvent(ev); return; }
    if (ev.ev === "term.exit") { this.terminals.onEvent(ev); return; }
    if (ev.ev === "term.size") { this.terminals.onEvent(ev); return; }
    if (ev.ev === "proc.exit") {
      const r = procResult(ev.code, ev.stdout, ev.stderr);
      const done = this.waiting.get(ev.proc);
      if (done !== undefined) { this.waiting.delete(ev.proc); done(r); } else this.exited.set(ev.proc, r);
      return;
    }
    if (ev.ev === "lsp.status") {
      // A language server was stopped, restarted or failed to start.
      this.lspStatus = ev.status;
      this.host.emitStatus(ev.status);
      this.updateStatus();
      return;
    }
    if (ev.ev === "workspace.trusted") {
      // The language servers start now: the event names them.
      this.trusted = true;
      this.lspLanguages = new Set(ev.lsp);
      this.host.emit("workspace.trusted");
      return;
    }
    if (ev.ev === "fs.changed") {
      this.watches.get(ev.watch)?.(ev.path);
      return;
    }
    if (ev.ev === "workspace.added") { this.orchestrator.added(ev.workspace); return; }
    if (ev.ev === "workspace.changed") { this.orchestrator.changed(ev.workspace); return; }
    if (ev.ev === "workspace.removed") { this.orchestrator.removed(ev.workspace); return; }
    if (ev.ev === "workspace.status") { this.orchestrator.statusOf(ev.workspace, ev.status); return; }
    if (ev.ev === "orchestrator.folders") { this.orchestrator.foldersChanged(ev.folders); return; }
  }

  // A process on the workspace's host; resolves with {code, stdout, stderr}.
  async runProcess(program /*: String */, args /*: String[] */, opts /*: {cwd: String} */) {
    const { proc } = await rpc("proc.run", { program, args, cwd: opts.cwd });
    const early = this.exited.get(proc);
    if (early !== undefined) { this.exited.delete(proc); return early; }
    return new Promise((resolve) => { this.waiting.set(proc, resolve); });
  }

  async watch(path, f) {
    const { watch } = await rpc("fs.watch", { path });
    this.watches.set(watch, f);
    return () => { this.watches.delete(watch); rpc("fs.unwatch", { watch }); };
  }

  // A language server answers for `b`: one is installed for its language
  // and not stopped (`lsp.stop`).
  lspAvailable(b) {
    const lang = b.language;
    return b.doc !== null && lang !== null && !b.large && this.lspLanguages.has(lang) && serverUp(this.lspStatus[lang]);
  }

  // ------------------------------------------------------------- input

  onInput() {
    const t = this.input.value;
    this.input.value = "";
    if (!t) return;
    if (this.activePane.shown.length === 0) return;
    const v = this.activePane.shown[0];
    if (v.buf.readOnly) return;
    // A buffer mode may claim printable keys (a panel's single-key commands).
    const bound = t.length === 1 ? modeKey(this.modes, v.buf.mode, t) : undefined;
    if (bound !== undefined) { bound.run(); return; }
    this.macros.text(t, true);
    this.editing.type(this.activePane, t, true);
  }

  // Text into the active view (unless it is read-only): typed (one undo
  // run, as typing) or pasted.
  insertText(text, typed) {
    const pane = this.activePane;
    if (!pane.shown.some((v) => !v.buf.readOnly)) return;
    if (typed) this.editing.type(pane, text, true);
    else this.paste(text);
  }
  // A virtual buffer of `text`, shown in a tab.
  showText(name, text) {
    this.createVirtual({ name, entries: [{ text, props: {}, style: null }], mode: null, readOnly: true, language: null, keep: false, place: "tab", lineNumbers: true });
  }

  paste(text) {
    if (this.activePane.shown.length === 0) return;
    const v = this.activePane.shown[0];
    if (v.buf.readOnly) return;
    this.macros.text(text, false);
    // One line per cursor when the counts match, as editors do.
    const parts = text.split("\n");
    if (v.cursors.length > 1 && parts.length === v.cursors.length) {
      // A line to each cursor, in the order they are in the text.
      const sorted = [...v.cursors].sort((a, b) => a.pos - b.pos);
      this.editing.replace(this.activePane, (c) => { const s = sel(c) ?? { start: c.pos, end: c.pos }; return put(s.start, s.end, parts[sorted.indexOf(c)]); });
    } else this.editing.type(this.activePane, text, false);
  }

  // The browser's copy and cut (Ctrl+C, Ctrl+X): the text copied, as
  // `copied` takes it.
  onCopy(e, cut) {
    for (const text of this.copied(cut)) { e.preventDefault(); e.clipboardData.setData("text/plain", text); }
  }
  // What a copy takes from the active view (a list of at most one: none
  // without a view): its selections in document order, as paste hands
  // them out, or with nothing selected the cursor's line; a cut deletes it.
  copied(cut) {
    if (this.activePane.shown.length === 0) return [];
    const v = this.activePane.shown[0];
    const sels = v.selections().sort((x, y) => x.start - y.start);
    const texts = sels.length ? sels.map((s) => v.buf.textRange(s.start, s.end) ?? "") : [lineAt(v.buf, v.primary.pos).text + "\n"];
    if (cut && !v.buf.readOnly) {
      if (sels.length) this.editing.replace(this.activePane, (c) => { const s = sel(c); return s ? put(s.start, s.end, "") : KEEP; });
      else this.editing.deleteLine(this.activePane);
    }
    return [texts.join("\n")];
  }

  // A key pressed in a panel (a button, a list, a field) that the control
  // left alone: the editor runs it if it is bound and a chord (Ctrl, Alt
  // or an F key), except a field's own editing keys. True: it took it. A
  // terminal keeps its own (see terminalKey).
  panelKey(e) {
    const t = e.target;
    if (t === this.input || t.closest(".xterm") !== null) return false;
    const k = keyName(e);
    if (!(e.ctrlKey || e.altKey || e.metaKey || /^F\d+$/.test(e.key))) return false;
    if (t.matches("input, textarea, select, [contenteditable]") && FIELD_KEYS.has(k)) return false;
    for (const cmd of found(this.keymap().get(k))) { this.run(cmd, undefined); return true; }
    return false;
  }

  // A key as the keymap names it: "C-S-k", "M-Up", "F12".
  // A key pressed while a terminal has the keyboard: the editor runs the
  // ones it keeps (`terminalKeys`: the terminal's toggle, moving between
  // workspaces). True: it took the key.
  terminalKey(e) {
    const k = e.type === "keydown" ? keyName(e) : "";
    if (!this.terminalKeys.has(k)) return false;
    for (const cmd of found(this.keymap().get(k))) { e.preventDefault(); this.run(cmd, undefined); return true; }
    return false;
  }

  onKey(e) {
    const k = keyName(e);
    // A buffer's mode (a plugin's panel buffer) takes its keys first.
    for (const b of this.activePane.shown.flatMap((v) => found(modeKey(this.modes, v.buf.mode, k))).slice(0, 1)) { e.preventDefault(); b.run(); return; }
    for (const cmd of found(this.keymap().get(k))) { e.preventDefault(); this.run(cmd, undefined); }
  }

  bindKey(key, command) { this.keys.set(key, command); }

  // Resolve with the next key pressed, as the keymap names it.
  captureKey() {
    return new Promise((resolve) => {
      const f = (e) => {
        if (["Control", "Shift", "Alt", "Meta"].includes(e.key)) return;
        e.preventDefault();
        e.stopPropagation();
        window.removeEventListener("keydown", f, true);
        resolve(e.key === "Escape" ? null : keyName(e));
      };
      window.addEventListener("keydown", f, true);
    });
  }

  // A theme: every colour variable ({"--bg": "#1e1e1e", ...}). Everything
  // the page draws reads them; only the terminal's canvas copies them.
  applyTheme(vars) {
    for (const [k, v] of Object.entries(vars)) document.documentElement.style.setProperty(k, v);
    this.terminals.applyTheme();
  }

  // The effective keymap, key -> command: the defaults, the plugins', the
  // Emacs keys when chosen, then the user's own.
  keymap() {
    /** const base: Dict<String> */
    const base = {
      "C-s": "file.save", "C-n": "file.new", "C-M-s": "file.saveAll", "C-M-t": "skin.next", "C-z": "edit.undo", "C-y": "edit.redo", "C-S-z": "edit.redo",
      "C-p": "palette.commands", "C-S-p": "palette.commands", F1: "palette.commands", "C-o": "palette.files", "C-'": "palette.files", "C-;": "palette.buffers", "C-g": "palette.line",
      "C-`": "terminal.toggle", "C-\\": "view.splitRight", "C-w": "view.closeTab", "C-Tab": "view.nextTab", "C-S-Tab": "view.prevTab", "C-PageDown": "view.nextTab", "C-PageUp": "view.prevTab",
      "M-Left": "go.back", "M-Right": "go.forward", "M-z": "view.toggleWrap",
      Left: "cursor.left", Right: "cursor.right", Up: "cursor.up", Down: "cursor.down",
      "S-Left": "select.left", "S-Right": "select.right", "S-Up": "select.up", "S-Down": "select.down",
      "C-Left": "cursor.wordLeft", "C-Right": "cursor.wordRight", "C-S-Left": "select.wordLeft", "C-S-Right": "select.wordRight",
      Home: "cursor.home", End: "cursor.end", "S-Home": "select.home", "S-End": "select.end",
      "C-Home": "cursor.docStart", "C-End": "cursor.docEnd", "C-S-Home": "select.docStart", "C-S-End": "select.docEnd",
      PageUp: "cursor.pageUp", PageDown: "cursor.pageDown", "S-PageUp": "select.pageUp", "S-PageDown": "select.pageDown",
      "C-Up": "view.scrollUp", "C-Down": "view.scrollDown", "C-S-Up": "select.paragraphUp", "C-S-Down": "select.paragraphDown",
      Enter: "edit.newline", Backspace: "edit.backspace", "C-Backspace": "edit.deleteWordBack", Delete: "edit.delete", "C-Delete": "edit.deleteWordForward",
      Tab: "edit.indent", "S-Tab": "edit.dedent", Esc: "edit.escape",
      "C-a": "select.all", "C-l": "select.line", "C-S-k": "edit.deleteLine", "C-S-d": "edit.duplicateLine", "M-Up": "edit.moveLineUp", "M-Down": "edit.moveLineDown",
      "M-u": "edit.upperCase", "M-l": "edit.lowerCase", "C-t": "edit.transposeChars",
      "M-S-Up": "select.blockUp", "M-S-Down": "select.blockDown", "M-S-Left": "select.blockLeft", "M-S-Right": "select.blockRight",
      "C-M-Right": "select.expand", "C-M-Left": "select.shrink",
      "C-/": "edit.toggleComment", "C-d": "select.nextOccurrence", "C-S-l": "select.allOccurrences", "C-M-Up": "cursor.addAbove", "C-M-Down": "cursor.addBelow",
      "C-]": "cursor.matchingBracket", "C-{": "fold.toggle", "C-}": "fold.toggle",
      "F4": "macro.playLast", "F5": "macro.stop", "C-=": "view.zoomIn", "C-+": "view.zoomIn", "C--": "view.zoomOut", "C-0": "view.zoomReset",
      F10: "menu.open", "M-f": "menu.file", "M-e": "menu.edit", "M-v": "menu.view", "M-s": "menu.selection", "M-g": "menu.go", "M-x": "menu.explorer", "M-h": "menu.help",
    };
    /** const emacs: Dict<String> */
    const emacs = this.settings.keymap === "emacs" ? {
      "C-f": "cursor.right", "C-b": "cursor.left", "C-n": "cursor.down", "C-p": "cursor.up", "C-a": "cursor.home", "C-e": "cursor.end",
      "M-f": "cursor.wordRight", "M-b": "cursor.wordLeft", "M-<": "cursor.docStart", "M->": "cursor.docEnd", "C-v": "cursor.pageDown", "M-v": "cursor.pageUp",
      "C-d": "edit.delete", "M-d": "edit.deleteWordForward", "C-k": "edit.killLine", "C-y": "edit.yank", "C-w": "edit.killRegion", "M-w": "edit.copyRegion",
      "C-/": "edit.undo", "C-_": "edit.undo", "C-g": "edit.escape", "M-x": "palette.commands", "C-Space": "edit.setMark", "M-g": "palette.line",
      "M-}": "cursor.paragraphDown", "M-{": "cursor.paragraphUp", "C-s": "search.find", "C-r": "search.findPrevious", "M-%": "search.replace",
    } : {};
    return new Map([...Object.entries(base), ...this.keys, ...Object.entries(emacs), ...Object.entries(this.userKeys)]);
  }

  // The keys bound to command `id`, as the keymap names them.
  keyFor(id) {
    /** const keys: String[] */
    const keys = [];
    this.keymap().forEach((c, k) => { if (c === id) keys.push(k); });
    return keys.join(", ");
  }

  // The first key for command `id` as people write it ("Ctrl+Shift+K"),
  // or "".
  keyLabel(id) {
    const k = this.keyFor(id).split(", ")[0];
    const m = /^((?:[CMS]-)*)(.+)$/.exec(k);
    if (m === null) return "";
    const key = m[2].length === 1 ? m[2].toUpperCase() : m[2];
    return [...m[1].split("-").filter(Boolean).map((x) => MODIFIERS.get(x) ?? x), key].join("+");
  }

  // Run command `id`, with its one optional argument (a theme name for
  // `theme.use`, …).
  // `id`, with its one optional argument (a theme name for `theme.use`, …).
  // Resolves when it is done. A macro being recorded takes it (not the
  // commands it runs itself).
  run(id, arg) {
    const c = this.commands.get(id);
    if (c === undefined) { this.setStatus(`No command “${id}”`); return Promise.resolve(undefined); }
    this.macros.command(id, arg);
    this.macros.depth++;
    try {
      return c.run(arg).catch((e) => { this.report(`${id}: ${failure(e)}`); });
    } catch (e) {
      this.report(`${id}: ${failure(e)}`);
      return Promise.resolve(undefined);
    } finally {
      this.macros.depth--;
    }
  }

  // ------------------------------------------------------------- core commands

  registerCoreCommands() {
    const ed = () => this.editing;
    const P = () => this.activePane;
    // The active view, for the commands that run only when there is one
    // (vcmd, mv).
    // The active split's view (the commands that use it run only when it
    // has one: `hasView`).
    const V = () => {
      for (const v of this.activePane.shown) return v;
      throw new Error("the active split shows no view");
    };
    const hasView = () => this.activePane.shown.length > 0;
    const ro = () => { const v = V(); return !v || v.buf.readOnly; };
    // A command, listed in the palette or not (`hiddenCmd`). Declared
    // functions, so each takes an action of its own kind.
    const commands = this.commands;
    function cmd(id, title, run) { commands.set(id, { title, run: asCommand(run), source: "core", hidden: false }); }
    function hiddenCmd(id, title, run) { commands.set(id, { title, run: asCommand(run), source: "core", hidden: true }); }
    const toggle = (k) => () => {
      const on = this.settings.flags[k] !== true;
      this.settings.flags[k] = on;
      this.saveSettings();
      this.setStatus(`${k.replace(/_/g, " ")}: ${on ? "on" : "off"}`);
    };
    const page = (dir, extend) => () => P().page(dir, extend);
    const mutate = (f) => () => { if (!ro()) f(); };

    cmd("file.save", "File: Save", () => { for (const v of listOf(this.view())) if (v.buf.doc !== null) this.saveBuffer(v.buf, true); });
    cmd("file.saveAll", "File: Save All", async () => {
      const mod = [...this.buffers.values()].filter((b) => b.doc !== null && b.modified);
      for (const b of mod) await this.saveBuffer(b, true);
      this.setStatus(mod.length ? `Saved ${mod.length} file(s)` : "Nothing to save");
    });
    cmd("file.toggleReadOnly", "Toggle Read-Only Mode (Current Buffer)", () => { const b = V()?.buf; if (b) { b.readOnly = !b.readOnly; this.renderAll(); } });
    // Save As: the text written to another file (through the server's
    // file operations), which then opens; the original stays as it is.
    cmd("file.saveAs", "File: Save As…", () => { for (const v of listOf(this.view())) this.saveBufferAs(v.buf); });
    cmd("file.new", "File: New File", () => this.newUntitled());
    cmd("file.language", "Set Language…", () => this.pickLanguage());
    cmd("file.encoding", "Set Encoding… (reopen or save with)", () => this.pickEncoding());
    cmd("file.lineEnding", "Set Line Ending…", () => this.pickLineEnding());
    cmd("file.revert", "File: Revert (reload from disk)", async () => {
      const b = V()?.buf;
      if (!b?.doc) return;
      if (b.modified && !(await this.confirm(`Revert ${b.name}? Its unsaved changes are lost`, "Revert"))) return;
      try { await rpc("doc.reload", { doc: b.doc }); this.setStatus(`Reverted ${b.name}`); } catch (e) { this.setStatus(`Not reverted: ${failure(e)}`); }
    });
    // The clipboard, from a menu: written and read where the browser allows
    // it (the keys go through the browser's own copy and paste).
    cmd("edit.copy", "Edit: Copy", async () => { for (const t of this.copied(false)) await navigator.clipboard.writeText(t); });
    cmd("edit.cut", "Edit: Cut", async () => { for (const t of this.copied(true)) await navigator.clipboard.writeText(t); });
    cmd("edit.paste", "Edit: Paste", async () => {
      try { this.paste(await navigator.clipboard.readText()); } catch { this.setStatus("The browser did not allow reading the clipboard here: paste with Ctrl+V"); }
    });
    cmd("edit.undo", "Edit: Undo", () => { for (const v of listOf(this.view())) this.afterUndo(v, v.buf.doUndo()); });
    cmd("edit.redo", "Edit: Redo", () => { for (const v of listOf(this.view())) this.afterUndo(v, v.buf.doRedo()); });
    cmd("palette.commands", "Command Palette", () => this.prompt.palette(">"));
    cmd("palette.files", "Open File…", () => this.prompt.palette(""));
    cmd("palette.buffers", "Switch Buffer…", () => this.prompt.palette("#"));
    cmd("palette.line", "Go to Line…", () => this.prompt.palette(":"));
    commands.set("palette.open", { title: "Palette with a Prefix", run: asArgCommand((prefix) => this.prompt.palette(prefix ?? "")), source: "core", hidden: true });
    cmd("terminal.toggle", "Terminal: Toggle", () => this.toggleTerminal());
    cmd("terminal.new", "Terminal: New", async () => { this.showTerminal(await this.terminals.create(null, null, null), this.terminalPane()); });
    // The current terminal closed and a new one started in its tab's place.
    cmd("terminal.restart", "Terminal: Restart", async () => {
      const p = this.terminalPane();
      for (const v of p.shown) if (terminalOf(v.buf.source).length > 0) await this.closeTab(p, v);
      this.showTerminal(await this.terminals.create(null, null, null), p);
    });
    // The selection (or the cursor's line) typed into the terminal shown
    // last, as if pasted there; the terminal shows (and has the keys).
    cmd("terminal.sendSelection", "Terminal: Send Selection", async () => {
      const v = V();
      if (!v) return;
      const sels = v.selections().sort((x, y) => x.start - y.start);
      const text = sels.length ? sels.map((s) => v.buf.textRange(s.start, s.end) ?? "").join("\n") : lineAt(v.buf, v.primary.pos).text + "\n";
      const p = this.terminalPane();
      const id = p.shown.flatMap((v) => terminalOf(v.buf.source)).at(0) ?? await this.terminals.create(null, null, null);
      this.showTerminal(id, p);
      await this.terminals.send(id, text);
    });
    cmd("skin.select", "Select Skin…", () => this.skins.pick());
    cmd("skin.next", "Next Skin", () => this.skins.cycle(1));
    cmd("view.splitRight", "View: Split Right", () => this.splitActive("h"));
    cmd("view.splitDown", "View: Split Down", () => this.splitActive("v"));
    cmd("view.closeSplit", "View: Close Split", () => this.closePane(P()));
    cmd("view.nextSplit", "View: Next Split", () => { const ps = panesOf(this.layoutTree); this.focusPane(ps[(ps.indexOf(P()) + 1) % ps.length]); });
    cmd("view.prevSplit", "View: Previous Split", () => { const ps = panesOf(this.layoutTree); this.focusPane(ps[(ps.indexOf(P()) - 1 + ps.length) % ps.length]); });
    cmd("view.closeTab", "View: Close Tab", () => { const p = P(); for (const v of p.shown) this.closeTab(p, v); });
    cmd("view.keepOpen", "View: Keep Tab Open (preview tab)", () => { for (const v of listOf(this.view())) this.keep(v); });
    cmd("settings.previewTabs", "Settings: Toggle Preview Tabs", toggle("preview_tabs"));
    // Zoom: the editor's and the terminals' text, a tenth at a time.
    const zoom = (z) => {
      this.settings.zoom = Math.round(clamp(z, 0.5, 3) * 10) / 10;
      document.body.style.setProperty("--editor-zoom", String(this.settings.zoom));
      this.saveSettings();
      this.remeasure();
      this.setStatus(`Zoom ${Math.round(this.settings.zoom * 100)}%`);
    };
    cmd("view.zoomIn", "View: Zoom In", () => zoom(this.settings.zoom + 0.1));
    cmd("view.zoomOut", "View: Zoom Out", () => zoom(this.settings.zoom - 0.1));
    cmd("view.zoomReset", "View: Reset Zoom", () => zoom(1));
    cmd("view.toggleSidebar", "View: Toggle Sidebar", () => { $("#sidebar").classList.toggle("hidden"); this.renderAll(); this.rememberLayout(); });
    hiddenCmd("view.showSidebar", "View: Show Sidebar", () => { $("#sidebar").classList.remove("hidden"); this.renderAll(); this.rememberLayout(); });
    cmd("file.recent", "File: Open Recent…", () => {
      const rel = (p) => shownFrom(this.root, p);
      const list = this.recentFiles();
      if (!list.length) { this.setStatus("No recent files"); return; }
      this.prompt.open({
        title: "Open Recent",
        value: "",
        items: list.map((p) => ({ label: rel(p), detail: p.split("/").pop() ?? "", value: p })),
        onAccept: (it) => { this.openFile(it.value, { line: null, column: 1, place: "tab", preview: null, focus: true }); },
        onChange: (_it) => {},
        onCancel: () => {},
      });
    });
    cmd("view.maximizeSplit", "View: Toggle Maximize Split", () => {
      this.maximized = this.maximized.length > 0 || panesOf(this.layoutTree).length < 2 ? [] : [P()];
      this.showMaximized();
      this.renderAll();
    });
    cmd("view.closeOtherTabs", "View: Close Other Tabs", () => { const p = P(); for (const v of p.shown) this.closeTabs(p, (x, _i) => x !== v); });
    cmd("view.closeAllTabs", "View: Close All Tabs", async () => { for (const p of panesOf(this.layoutTree)) await this.closeTabs(p, (_x, _i) => true); });
    cmd("view.nextTab", "View: Next Tab", () => this.cycleTab(1));
    cmd("view.prevTab", "View: Previous Tab", () => this.cycleTab(-1));
    cmd("view.toggleWrap", "View: Toggle Line Wrap (Current Buffer)", () => { for (const v of listOf(this.view())) { v.opts.wrap = !v.opts.wrap; P().scheduleRender(); } });
    cmd("view.toggleLineNumbers", "View: Toggle Line Numbers (Current Buffer)", () => { for (const v of listOf(this.view())) { v.opts.lineNumbers = !v.opts.lineNumbers; P().scheduleRender(); } });
    cmd("view.toggleCurrentLine", "View: Toggle Current Line Highlight", toggle("highlight_current_line"));
    cmd("view.toggleOccurrences", "View: Toggle Occurrence Highlight", toggle("highlight_occurrences"));
    cmd("view.toggleWhitespace", "View: Toggle Whitespace Indicators", toggle("show_whitespace"));
    cmd("view.toggleIndentGuides", "View: Toggle Indentation Guides", toggle("show_indent_guides"));
    cmd("view.toggleBrackets", "View: Toggle Matching Bracket Highlight", toggle("highlight_brackets"));
    cmd("view.toggleRainbow", "View: Toggle Rainbow Brackets", toggle("rainbow_brackets"));
    cmd("view.toggleScrollSync", "View: Toggle Scroll Sync", toggle("scroll_sync"));
    hiddenCmd("view.scrollUp", "View: Scroll Up", () => P().scrollLines(-1));
    hiddenCmd("view.scrollDown", "View: Scroll Down", () => P().scrollLines(1));
    cmd("view.addRuler", "View: Add Ruler", async () => {
      const t = await this.prompt.ask({ title: "Ruler at column", value: "80", hint: "", select: true });
      const n = parseInt(t, 10);
      if (n > 0) { this.settings.rulers = [...new Set([...this.settings.rulers, n])].sort((a, b) => a - b); this.saveSettings(); }
    });
    cmd("view.removeRulers", "View: Remove Rulers", () => { this.settings.rulers = []; this.saveSettings(); });
    cmd("settings.autoSave", "Settings: Toggle Auto Save", toggle("auto_save"));
    cmd("settings.autoClose", "Settings: Toggle Auto Close Brackets", toggle("auto_close"));
    cmd("settings.keymapDefault", "Keymap: Default", () => { this.settings.keymap = "default"; this.saveSettings(); });
    cmd("settings.keymapEmacs", "Keymap: Emacs", () => { this.settings.keymap = "emacs"; this.saveSettings(); });
    cmd("go.back", "Go Back", () => this.historyStep(-1));
    cmd("go.forward", "Go Forward", () => this.historyStep(1));
    hiddenCmd("view.focusEditor", "View: Focus Editor", () => this.focusPane(P()));
    cmd("workspace.trust", "Workspace: Trust (allow processes, terminals and language servers)", async () => {
      if (this.trusted) { this.setStatus("The workspace is trusted"); return; }
      await rpc("workspace.trust", {});
    });
    cmd("help.log", "Show Log", () => this.createVirtual({
      name: "*Log*", entries: this.log.map((l) => ({ text: l + "\n", props: {}, style: null })),
      mode: null, readOnly: true, language: null, keep: false, place: "tab", lineNumbers: true,
    }));

    // Motion and selection: nothing to do without a view.
    // Commands on the active view (nothing without one); a motion's is hidden.
    function vcmd(id, title, f) { commands.set(id, { title, run: asCommand(() => (hasView() ? f() : undefined)), source: "core", hidden: false }); }
    function mv(id, title, f) { commands.set(id, { title, run: asCommand(() => (hasView() ? f() : undefined)), source: "core", hidden: true }); }
    mv("cursor.left", "Left", () => ed().left(P(), false, false)); mv("cursor.right", "Right", () => ed().right(P(), false, false));
    mv("select.left", "Select Left", () => ed().left(P(), true, false)); mv("select.right", "Select Right", () => ed().right(P(), true, false));
    mv("cursor.wordLeft", "Word Left", () => ed().left(P(), false, true)); mv("cursor.wordRight", "Word Right", () => ed().right(P(), false, true));
    mv("select.wordLeft", "Select Word Left", () => ed().left(P(), true, true)); mv("select.wordRight", "Select Word Right", () => ed().right(P(), true, true));
    mv("cursor.up", "Up", () => ed().vertical(P(), -1, false)); mv("cursor.down", "Down", () => ed().vertical(P(), 1, false));
    mv("select.up", "Select Up", () => ed().vertical(P(), -1, true)); mv("select.down", "Select Down", () => ed().vertical(P(), 1, true));
    mv("cursor.home", "Line Start", () => ed().home(P(), false)); mv("cursor.end", "Line End", () => ed().end(P(), false));
    mv("select.home", "Select to Line Start", () => ed().home(P(), true)); mv("select.end", "Select to Line End", () => ed().end(P(), true));
    mv("cursor.docStart", "Document Start", () => { for (const v of listOf(this.view())) this.pushHistory(v, v.primary.pos); return this.gotoByte(0, false); });
    mv("cursor.docEnd", "Document End", () => { for (const v of listOf(this.view())) this.pushHistory(v, v.primary.pos); return this.gotoByte(V().buf.len, false); });
    mv("select.docStart", "Select to Document Start", () => this.gotoByte(0, true));
    mv("select.docEnd", "Select to Document End", () => this.gotoByte(V().buf.len, true));
    mv("cursor.pageUp", "Page Up", page(-1, false)); mv("cursor.pageDown", "Page Down", page(1, false));
    mv("select.pageUp", "Select Page Up", page(-1, true)); mv("select.pageDown", "Select Page Down", page(1, true));
    vcmd("cursor.paragraphUp", "Go to Previous Paragraph", () => ed().paragraph(P(), -1, false));
    vcmd("cursor.paragraphDown", "Go to Next Paragraph", () => ed().paragraph(P(), 1, false));
    mv("select.paragraphUp", "Select to Previous Paragraph", () => ed().paragraph(P(), -1, true));
    mv("select.paragraphDown", "Select to Next Paragraph", () => ed().paragraph(P(), 1, true));
    vcmd("cursor.matchingBracket", "Go to Matching Bracket", () => ed().matchingBracket(P()));
    vcmd("cursor.addAbove", "Add Cursor Above", () => addCursorVertical(P(), -1));
    vcmd("cursor.addBelow", "Add Cursor Below", () => addCursorVertical(P(), 1));
    cmd("select.all", "Select All", () => { for (const v of listOf(this.view())) { v.cursors = [selecting(v.buf.len, 0)]; P().scheduleRender(); } });
    vcmd("select.line", "Select Line", () => selectLine(P()));
    vcmd("select.nextOccurrence", "Select Next Occurrence", () => ed().selectNextOccurrence(P()));
    vcmd("select.allOccurrences", "Select All Occurrences", () => ed().selectAllOccurrences(P()));
    vcmd("select.expand", "Expand Selection", () => ed().expandSelection(P()));
    vcmd("select.shrink", "Shrink Selection", () => shrinkSelection(P()));
    mv("select.blockUp", "Block Select Up", () => blockSelect(P(), -1, 0));
    mv("select.blockDown", "Block Select Down", () => blockSelect(P(), 1, 0));
    mv("select.blockLeft", "Block Select Left", () => blockSelect(P(), 0, -1));
    mv("select.blockRight", "Block Select Right", () => blockSelect(P(), 0, 1));
    // Editing.
    mv("edit.newline", "Newline", mutate(() => ed().newline(P())));
    mv("edit.backspace", "Backspace", mutate(() => ed().backspace(P(), false)));
    mv("edit.deleteWordBack", "Delete Word Backward", mutate(() => ed().backspace(P(), true)));
    mv("edit.delete", "Delete", mutate(() => ed().del(P(), false)));
    mv("edit.deleteWordForward", "Delete Word Forward", mutate(() => ed().del(P(), true)));
    mv("edit.indent", "Indent", mutate(() => ed().indent(P(), 1)));
    mv("edit.dedent", "Dedent", mutate(() => ed().indent(P(), -1)));
    mv("edit.escape", "Escape", () => { for (const v of listOf(this.view())) { v.cursors = [v.primary]; v.primary.anchor = none(); } hidePopup(); this.host.emit("escape"); P().scheduleRender(); });
    cmd("edit.deleteLine", "Delete Line", mutate(() => ed().deleteLine(P())));
    cmd("edit.duplicateLine", "Duplicate Line", mutate(() => ed().duplicateLine(P())));
    cmd("edit.moveLineUp", "Move Line Up", mutate(() => ed().moveLines(P(), -1)));
    cmd("edit.moveLineDown", "Move Line Down", mutate(() => ed().moveLines(P(), 1)));
    cmd("edit.toggleComment", "Toggle Comment", mutate(() => ed().toggleComment(P())));
    cmd("edit.upperCase", "Transform to Uppercase", mutate(() => ed().transform(P(), (t) => t.toUpperCase())));
    cmd("edit.lowerCase", "Transform to Lowercase", mutate(() => ed().transform(P(), (t) => t.toLowerCase())));
    cmd("edit.sortLines", "Sort Lines", mutate(() => ed().sortLines(P())));
    cmd("edit.trimTrailing", "Trim Trailing Whitespace", mutate(() => ed().trimTrailing(P())));
    cmd("edit.transposeChars", "Transpose Characters", mutate(() => ed().transposeChars(P())));
    cmd("edit.transposeLines", "Transpose Lines", mutate(() => ed().transposeLines(P())));
    vcmd("fold.toggle", "Toggle Fold", () => toggleFold(P()));
    vcmd("fold.all", "Fold All", () => foldAll(P()));
    vcmd("fold.none", "Unfold All", () => unfoldAll(P()));
    // Emacs: mark and kill ring.
    mv("edit.setMark", "Set Mark", () => { const v = V(); v.primary.anchor = some(v.primary.pos); this.setStatus("Mark set"); });
    mv("edit.killLine", "Kill Line", mutate(() => {
      const v = V();
      const ln = lineAt(v.buf, v.primary.pos);
      const end = v.primary.pos === ln.end ? ln.end + 1 : ln.end;
      this.killRing.unshift(v.buf.textRange(v.primary.pos, end) ?? "");
      ed().replace(P(), (c) => (c === v.primary ? { start: c.pos, end, text: "", caret: 0 } : null));
    }));
    mv("edit.killRegion", "Kill Region", mutate(() => {
      const s = V().selection();
      if (s === null) return;
      this.killRing.unshift(V().buf.textRange(s.start, s.end) ?? "");
      ed().replace(P(), (c) => (c === V().primary ? { ...s, text: "" } : null));
    }));
    mv("edit.copyRegion", "Copy Region", () => { for (const s of listOf(V().selection())) { this.killRing.unshift(V().buf.textRange(s.start, s.end) ?? ""); V().primary.anchor = none(); P().scheduleRender(); } });
    mv("edit.yank", "Yank", mutate(() => { for (const text of this.killRing.slice(0, 1)) ed().type(P(), text, false); }));
  }

  // ------------------------------------------------------------- a document's format

  // A document's language, line ending and encoding, as the server names
  // them (asked once).
  async docFormats() {
    for (const f of this.formats) return f;
    const f /*: DocFormats */ = await rpc("doc.formats", {});
    this.formats = [f];
    return f;
  }
  // The active view's buffer, when it is a document's (a list of at most
  // one).
  activeFile() { return this.activePane.shown.filter((v) => v.buf.doc !== null).map((v) => v.buf); }
  // Pick one of `choices` ({label, detail, value}): the current one is
  // marked and first selected. Resolves with the value picked (a list of at
  // most one: none when given up).
  pickFor(title, choices, current) {
    const items = choices.map((c) => (c.value === current ? { label: c.label, detail: c.detail ? `${c.detail} · current` : "current", value: c.value } : c));
    const i = Math.max(0, items.findIndex((c) => c.value === current));
    return new Promise((resolve) => {
      this.prompt.open({ title, value: "", items: [...items.slice(i), ...items.slice(0, i)], onAccept: (it) => { resolve([it.value]); }, onChange: (_it) => {}, onCancel: () => { resolve([]); } });
    });
  }
  async pickLanguage() {
    const b = this.activeFile().at(0);
    if (b === undefined) { this.setStatus("Not a file"); return; }
    const f = await this.docFormats();
    const choices = [{ label: "plain text", detail: "", value: "" }, ...f.languages.map((l) => ({ label: l, detail: "", value: l }))];
    for (const lang of await this.pickFor("Language", choices, b.language ?? "")) {
      const o = await attempt("doc.set", { doc: b.doc, language: lang === "" ? null : lang });
      if (o.kind === "refused") this.setStatus(o.detail);
    }
  }
  async pickLineEnding() {
    const b = this.activeFile().at(0);
    if (b === undefined) { this.setStatus("Not a file"); return; }
    const f = await this.docFormats();
    const choices = f.line_endings.map((l) => ({ label: LINE_ENDINGS.get(l) ?? l, detail: "", value: l }));
    for (const le of await this.pickFor("Line ending (saving converts the file)", choices, b.lineEnding ?? "")) {
      if (le === b.lineEnding) continue;
      const o = await attempt("doc.set", { doc: b.doc, line_ending: le });
      this.setStatus(o.kind === "refused" ? o.detail : `${b.name} is saved with ${le} line endings`);
    }
  }
  // An encoding, then what to do with it: read the file again in it, or
  // write the file in it on the next save.
  async pickEncoding() {
    const b = this.activeFile().at(0);
    if (b === undefined) { this.setStatus("Not a file"); return; }
    const f = await this.docFormats();
    const choices = f.encodings.map((e) => ({ label: e.name, detail: e.description, value: e.name }));
    for (const enc of await this.pickFor("Encoding", choices, b.encoding ?? "")) {
      const how = await this.choose(`${b.name} in ${enc}`, [
        ...(b.large ? [] : [{ label: `Reopen with ${enc} (read the file again in it)`, value: "reopen" }]),
        { label: `Save with ${enc} (the next save writes it)`, value: "save" },
      ]);
      if (how === "save") {
        const o = await attempt("doc.set", { doc: b.doc, encoding: enc });
        if (o.kind === "refused") this.setStatus(o.detail);
      }
      if (how === "reopen") {
        if (b.modified && !(await this.confirm(`Reopening ${b.name} gives up its unsaved changes`, "Reopen"))) return;
        const o = await attempt("doc.reload", { doc: b.doc, encoding: enc });
        this.setStatus(o.kind === "refused" ? o.detail : `Reopened ${b.name} with ${enc}`);
      }
    }
  }

  // Buffer `b`'s document has a line index (`lines` long) now: its window
  // is fetched again, with line numbers.
  async reindexed(b, lines) {
    await b.indexed(lines, this.viewsOf(b).map((v) => v.top).at(0) ?? 0);
    this.renderBuffer(b.id);
    this.updateStatus();
  }

  // Buffer `b`'s document was loaded again (a huge file changed on disk):
  // its views go to its start, and its window is fetched again.
  async reloadedDoc(b, info) {
    b.reloaded(info);
    const views = this.viewsOf(b);
    for (const v of views) {
      for (const c of v.cursors) { c.pos = Math.min(c.pos, b.len); c.anchor = none(); }
      v.setTop(0, 0);
    }
    await b.ensure(views.map((v) => v.primary.pos).at(0) ?? 0, ENSURE_AFTER);
    this.renderAll();
    this.updateStatus();
  }
  // The views of buffer `b`, in every split.
  viewsOf(b) { return panesOf(this.layoutTree).flatMap((p) => p.views.filter((v) => v.buf === b)); }

  afterUndo(v, ops) {
    const last = ops.at(-1);
    if (last === undefined) return;
    v.keepPrimary();
    this.editing.place(v.pane, last.t === "ins" ? last.pos + byteLen(last.text) : last.pos, false);
  }

  saveSettings() {
    try { localStorage.setItem(SETTINGS_KEY, JSON.stringify(this.settings)); } catch {}
    this.renderAll();
  }
  saveKeys() {
    try { localStorage.setItem("fresh-poc:keys", JSON.stringify(this.userKeys)); } catch {}
  }
  cycleTab(d) {
    const p = this.activePane;
    if (p.views.length === 0 || p.shown.length === 0) return;
    const i = (p.views.indexOf(p.shown[0]) + d + p.views.length) % p.views.length;
    p.show(p.views[i].buf, false);
  }

  // ------------------------------------------------------------- hover and popups

  hoverProvide(f) { this.hoverProviders.push(f); }

  onHover(pane, p, x, y) {
    clearTimeout(this.hoverTimer);
    if (p == null || pane.shown.length === 0) return;
    const b = pane.shown[0].buf;
    this.hoverTimer = setTimeout(async () => {
      const parts = [];
      for (const f of this.hoverProviders) {
        try { const t = await f(b, p); if (t) parts.push(t); } catch {}
      }
      if (parts.length) popup(parts.join("\n\n"), { x, y: y + this.metrics.lh });
    }, 600);
  }

  // `text` in the popup under the active view's caret.
  popupAtCaret(text) {
    const pane = this.activePane;
    const r = pane.surfaceEl.getBoundingClientRect();
    const under = pane.shown.flatMap((v) => pane.caretBox(v.primary.pos)).map((c) => ({ x: c.x, y: c.y + c.h }));
    popup(text, under.at(0) ?? { x: r.left, y: r.top });
  }

  // Ask a question with a few answers [{label, value}]: resolves with the
  // value of the one picked, or null when cancelled.
  choose(title, answers) {
    return new Promise((resolve) => this.prompt.open({
      title,
      value: "",
      items: answers.map((a) => ({ label: a.label, detail: "", value: a.value })),
      onAccept: (it) => { resolve(it.value); },
      onChange: (_it) => {},
      onCancel: () => { resolve(null); },
    }));
  }
  // Ask a yes/no question: resolves true when `yes` is picked.
  async confirm(title, yes) {
    return (await this.choose(title, [{ label: yes, value: "yes" }, { label: "Cancel", value: "cancel" }])) === "yes";
  }

  // ------------------------------------------------------------- status

  setStatus(m) { this.status = m; this.updateStatus(); }

  // A status bar item of a part's own (made the first time): `set(text,
  // {title, onClick})` shows it (onClick null: not clickable).
  statusItem(id) {
    const it = this.statusItems.get(id) ?? statusFact();
    this.statusItems.set(id, it);
    return { set: (text, o) => { it.text = text; it.title = o.title; it.onClick = o.onClick; this.updateStatus(); } };
  }

  // The status bar: the message, then the facts. A fact that has an action
  // (a command, or a plugin item's onClick) runs it when clicked; its
  // tooltip names it and its key. The bar is written only when what it
  // shows changed.
  updateStatus() {
    const left = $("#status-left"), right = $("#status-right");
    if (!left) return;
    if (left.textContent !== this.status) left.textContent = this.status;
    const items = [];
    // A fact, its tooltip and what clicking it does (none: not clickable).
    const add = (text, title, run) => { items.push(statusFactOf(text, title, run)); };
    const cmd = (id) => () => { this.run(id, undefined); };
    const titled = (what, id) => { const k = this.keyLabel(id); return k !== "" ? `${what} (${k})` : what; };
    // A document's facts (a terminal's tab has none).
    for (const v of listOf(this.view()).filter((x) => terminalOf(x.buf.source).length === 0)) {
      const b = v.buf;
      const c = v.primary;
      for (const L of listOf(b.text)) {
        const i = L.indexOf(c.pos);
        if (i < 0) continue;
        const col = [...L.lines[i].slice(0, byteToIdx(L.lines[i], c.pos - L.starts[i]))].length + 1;
        add(listOf(L.firstLine).map((f) => `Ln ${f + i + 1}, Col ${col}`).at(0) ?? `Byte ${c.pos}`, titled("Go to Line", "palette.line"), cmd("palette.line"));
      }
      if (v.cursors.length > 1) add(`${v.cursors.length} cursors`, titled("Remove the other cursors", "edit.escape"), cmd("edit.escape"));
      for (const r of this.macros.recording) add(`● REC ${r.register}`, titled("Recording a macro: Stop Recording Macro", "macro.stop"), cmd("macro.stop"));
      if (b.readOnly && !b.isVirtual) add("[RO]", "Read-only: click to allow editing", cmd("file.toggleReadOnly"));
      if (b.large) add("large file", "Read from disk as it is viewed; lines are counted in the background", null);
      if (b.doc !== null) {
        for (const e of listOf(b.lineEnding)) add(e, titled("Line ending: Set Line Ending", "file.lineEnding"), cmd("file.lineEnding"));
        for (const e of listOf(b.encoding)) add(e, titled("Encoding: Reopen or Save with Encoding", "file.encoding"), cmd("file.encoding"));
        add(b.language ?? "plain text", titled("Language: Set Language", "file.language"), cmd("file.language"));
      } else for (const l of listOf(b.language)) add(l, "", null);
      const ds = b.decor.diags;
      if (ds.length > 0) add(`● ${ds.filter((d) => d.severity === 1).length}  ▲ ${ds.filter((d) => d.severity === 2).length}`, titled("Diagnostics: Show Panel", "lsp.diagnostics"), cmd("lsp.diagnostics"));
      if (v.mode === "compose") add("compose", titled("Markdown: Toggle Compose", "markdown.toggleCompose"), cmd("markdown.toggleCompose"));
      for (const f of b.file) add(synced(f) ? `v${f.version}` : `v${f.version}…`, "Document log version", null);
    }
    for (const it of this.statusItems.values()) if (it.text !== "") add(it.text, it.title, it.onClick);
    add(this.hostName, "Workspace host", null);
    const sig = items.map((x) => `${x.text}\u0001${x.title || ""}\u0001${x.onClick ? 1 : 0}`).join("\u0002");
    this.statusRuns = items.map((x) => x.onClick);
    if (sig === this.statusSig) return;
    this.statusSig = sig;
    right.replaceChildren(...items.map((x, k) => {
      const s = document.createElement("span");
      s.textContent = x.text;
      if (x.title) s.title = x.title;
      if (x.onClick) { s.className = "click"; s.onclick = () => this.statusRuns[k]?.(); }
      return s;
    }));
  }
}

export { THEME };
