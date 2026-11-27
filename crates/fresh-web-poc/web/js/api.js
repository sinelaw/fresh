// The plugin API: functions of the application, each taking it first.
// A plugin is a module exporting `activate(app)`; it imports this one as
// `fresh` and calls `fresh.message(app, "…")`, `fresh.activeBuffer(app)`
// and so on. Buffers and views reach plugins as handles (host.js). The
// client's own features (search, git, the language-server panels,
// markdown compose, the explorer…) are written against this API.
//
// Events come on four channels: `on` (no payload: "app.ready", "escape",
// "workspace.trusted"), `onBuffer` ("buffer.opened", "buffer.closed",
// "buffer.changed" with its ops, "buffer.saved", "buffer.activated",
// "buffer.info", "buffer.renamed", "buffer.deleted",
// "diagnostics.changed"), `onView` ("cursor.moved", "view.resized") and
// `onLspStatus`.

import { rpc, failure } from "./rpc.js";
import { whenSynced, filePath } from "./buffer.js";
import { byteLen as byteLenOf } from "./utf8.js";
import { lspOffset as lspOffsetIn, lspPosition as lspPositionIn } from "./app.js";
import { found, listOf } from "./maybe.js";
import { bufferHandle, viewHandle } from "./host.js";
import { asCommand, asArgCommand } from "./commands.js";
export { insOp, delOp } from "./ot.js";
export { byteLen } from "./utf8.js";
export { baseName, parentOf, under, shownFrom } from "./paths.js";

// The held line holding byte `pos` of held lines `L`: its index (a list
// of at most one: none when `pos` is not held).
/** function heldLineAt(L: HeldLines, pos: Number) => Number[] */
export function heldLineAt(L, pos) {
  const n = L.starts.length;
  if (n === 0 || pos < L.starts[0]) return [];
  let lo = 0, hi = n - 1;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (L.starts[mid] <= pos) lo = mid;
    else hi = mid - 1;
  }
  return pos <= L.starts[lo] + byteLenOf(L.lines[lo]) ? [lo] : [];
}

// Calls settled: the function made runs `f` `ms` after the last call with
// the same `key` (each key on its own).
/** function debouncer() => (String, Number, () => Undefined) => Undefined */
export function debouncer() {
  /** const timers: Map<String, Number> */
  const timers = new Map();
  return (key, ms, f) => {
    for (const t of found(timers.get(key))) clearTimeout(t);
    timers.set(key, setTimeout(() => { timers.delete(key); f(); }, ms));
  };
}

// File line `line` (0-based) among held lines `L`: its index there (a list
// of at most one: none when the lines' place in the file is not known yet,
// or that line is not held).
/** function heldIndex(L: HeldLines, line: Number) => Number[] */
export function heldIndex(L, line) {
  const first = L.firstLine;
  if (first === null) return [];
  const i = line - first;
  return i >= 0 && i < L.starts.length ? [i] : [];
}
// A value that may be missing (null) as a list of at most one, to act on
// it if it is there.
export { listOf, found } from "./maybe.js";
export { textWidth } from "./clusters.js";

// The application's buffer a handle is of (none once it is closed).
const held = (app, buffer) => app.buffers.get(buffer.id);
// The server document of the buffer a handle names (none: it is gone, or
// virtual).
const docOfHeld = (app, buffer) => found(held(app, buffer)).flatMap((b) => listOf(b.doc)).at(0) ?? null;

// ---------------------------------------------------------------- the workspace

export const root = (app) => app.root;
export const hostName = (app) => app.hostName;
export const trusted = (app) => app.trusted;
// The settings (a plugin may change them, then saveSettings).
export const settings = (app) => app.settings;
// Remember a change a plugin made to the settings (in the browser) and redraw.
export const saveSettings = (app) => app.saveSettings();

// ---------------------------------------------------------------- commands and keys

// `spec`: {title, hidden, run}; a hidden command is left out of the
// palette. The command's source is the first part of its id ("git.log").
export const register = (app, id, spec) => {
  app.commands.set(id, { title: spec.title, run: asCommand(spec.run), source: id.split(".")[0], hidden: spec.hidden });
};
// A command whose action takes its argument (`runWith`'s).
export const registerWith = (app, id, spec) => {
  app.commands.set(id, { title: spec.title, run: asArgCommand(spec.run), source: id.split(".")[0], hidden: spec.hidden });
};
export const run = (app, id) => app.run(id, undefined);
// A command with its argument (a palette prefix: "palette.open", "@").
export const runWith = (app, id, arg) => app.run(id, arg);
export const commands = (app) => [...app.commands].map(([id, c]) => ({ id, title: c.title, source: c.source, hidden: c.hidden }));
export const bindKey = (app, key, command) => app.bindKey(key, command);
export const keyFor = (app, command) => app.keyFor(command);
// The first key for a command as people write it ("Ctrl+Shift+K").
export const keyLabel = (app, command) => app.keyLabel(command);
// The effective keymap: a Map of key -> command.
export const keymap = (app) => app.keymap();
// The user's own bindings (saved in the browser), over the defaults.
export const userKeys = (app) => new Map(Object.entries(app.userKeys));
export const setUserKey = (app, key, command) => {
  const others = Object.entries(app.userKeys).filter(([k]) => k !== key);
  app.userKeys = Object.fromEntries(command ? [...others, [key, command]] : others);
  app.saveKeys();
};
// The next key pressed, as the keymap names it.
export const captureKey = (app) => app.captureKey();
// Keys for a buffer kind: defineMode(app, "welcome", [{ key: "Enter",
// run }]) (a ModeKey each, types/fresh.d.js).
export const defineMode = (app, mode, keys) => { app.modes.set(mode, keys); };
// A row at the end of a menu bar menu: addMenuItem(app, "Go",
// fresh.menuCommand("bookmark.list", "Bookmarks…")); MENU_BAR_SEPARATOR parts.
export const addMenuItem = (app, menu, row) => app.menubar.add(menu, row);
export { cmd as menuCommand, SEP as MENU_BAR_SEPARATOR } from "./menubar.js";
// A toggle row at the end of a menu bar menu, checked while `read()` is true.
export const addMenuToggle = (app, menu, command, label, read) => app.menubar.addToggle(menu, command, label, read);
// A context menu's items (fresh.menu): an action, one with its keys, check
// and state given, or a separator.
export { action as menuAction, actionOf as menuActionOf, SEPARATOR as MENU_SEPARATOR } from "./menu.js";
// A palette mode for a prefix: paletteMode(app, "@", {hint, items(q)}).
export const paletteMode = (app, prefix, o) => app.prompt.provide(prefix, o);

// ---------------------------------------------------------------- events

export const on = (app, event, f) => app.host.on(event, () => { f(); });
export const onBuffer = (app, event, f) => app.host.onBuffer(event, (b, _change) => { f(b); });
// "buffer.changed" with what changed: its ops, and whether it was typed here.
export const onBufferChanged = (app, f) => app.host.onBuffer("buffer.changed", (b, change) => { f(b, change); });
export const onView = (app, event, f) => app.host.onView(event, (v) => { f(v); });
export const onLspStatus = (app, f) => app.host.onStatus((status) => { f(status); });

// ---------------------------------------------------------------- buffers and views

export const activeBuffer = (app) => { const v = app.view(); return v ? bufferHandle(app, v.buf) : null; };
export const buffers = (app) => [...app.buffers.values()].map((b) => bufferHandle(app, b));
export const bufferById = (app, id) => { const b = app.buffers.get(id); return b === undefined ? null : bufferHandle(app, b); };
export const bufferByPath = (app, path) => { const b = [...app.buffers.values()].find((x) => filePath(x.origin).includes(path)); return b === undefined ? null : bufferHandle(app, b); };
// `at`: OpenOptions (types/fresh.d.js).
export const open = async (app, path, at) => (await app.openFile(path, at)).map((v) => bufferHandle(app, v.buf)).at(0) ?? null;
// The buffer for a file, without showing it.
export const load = async (app, path) => bufferHandle(app, await app.loadFile(path));
// A VirtualOptions (types/fresh.d.js).
export const createVirtual = (app, o) => bufferHandle(app, app.createVirtual(o));
export const show = (app, buffer, place) => { const b = held(app, buffer); if (b !== undefined) app.showBuffer(b, place, { preview: null, focus: true }); };
export const closeBuffer = async (app, buffer) => { const b = held(app, buffer); if (b !== undefined) await app.closeBuffer(b); };
export const activeView = (app) => { const v = app.view(); return v ? viewHandle(app, v) : null; };
export const views = (app) => app.panes().flatMap((p) => p.views).map((v) => viewHandle(app, v));
// Scroll two buffers' views together through a line mapping: aToB(line of
// a) -> line of b, bToA the other way. Returns the function that unlinks
// them.
export const linkScroll = (app, a, b, aToB, bToA) => {
  const x = held(app, a), y = held(app, b);
  return x !== undefined && y !== undefined ? app.linkScroll(x, y, aToB, bToA) : () => {};
};

// ---------------------------------------------------------------- the UI

// A PanelOptions (types/fresh.d.js).
export const panel = (app, o) => app.ui.panel(o);
// The panel `id` if it is on screen, else undefined.
export const shown = (app, id) => app.ui.shown(id);
// A context menu: items [{label, run}] (null: separator) at {x, y}.
export const menu = (app, items, at) => app.ui.menu(items, at);
// Pick from a list: {title, items: [{label, detail, value}], onAccept, onChange}.
export const pick = (app, o) => app.prompt.open(o);
// Pick a file of the workspace, searched as it is typed: its path (a list
// of at most one: none when given up).
export const pickFile = (app, title) => app.prompt.pickFile(title);
// Ask for text: {title, value} -> Promise<string|null>.
export const input = (app, o) => app.prompt.ask(o);
// Ask yes or no: (title, yes label) -> Promise<boolean>.
export const confirm = (app, title, yes) => app.confirm(title, yes);
// Ask with a few answers: (title, [{label, value}]) -> Promise<value|null>.
export const choose = (app, title, answers) => app.choose(title, answers);
export const statusItem = (app, id) => app.statusItem(id);
export const message = (app, text) => app.setStatus(text);
// A failure's text in the status bar: a refused request's detail (the
// server's own words), else the error as written.
export const errorText = failure;
export const showError = (app, e) => app.setStatus(errorText(e));
// `text` in a popup under the caret.
export const popup = (app, text) => app.popupAtCaret(text);
// Colours: {"--var": css} for the page's colour variables (style.css).
export const setTheme = (app, vars) => app.applyTheme(vars);
// Text shown when the pointer rests on a position: f(buffer, pos) -> string|null.
export const hoverProvider = (app, f) => app.hoverProvide((b, p) => f(bufferHandle(app, b), p));

// ---------------------------------------------------------------- the workspace's host

export const runProcess = (app, program, args, opts) => app.runProcess(program, args, opts);
export const watch = (app, path, f) => app.watch(path, f);
export const read = async (_app, path) => (await rpc("fs.read", { path })).text;
export const write = (_app, path, text) => rpc("fs.write", { path, text });
export const list = async (_app, path) => (await rpc("fs.list", { path })).entries;
export const stat = (_app, path) => rpc("fs.stat", { path });
export const create = (_app, path, dir) => rpc("fs.create", { path, dir });
export const rename = (_app, from, to) => rpc("fs.rename", { from, to });
export const remove = (_app, path) => rpc("fs.delete", { path });
export const copy = (_app, from, to) => rpc("fs.copy", { from, to });
export const findFiles = async (_app, query) => (await rpc("fs.find", { query })).files;
// `o`: {regex, case, whole, max}.
export const searchWorkspace = (_app, query, o) => rpc("search.project", { query, regex: o.regex, case: o.case, whole: o.whole, max: o.max });
// The next (or previous) occurrence of `query` in a document, found by the
// server: how a huge file is searched.
export const searchDocument = async (app, buffer, query, from, backward) => (await rpc("doc.search", { doc: docOfHeld(app, buffer), query, from, backward })).pos;
// The editor's themes: [{key, name}], and one fully resolved.
export const themes = async (_app) => (await rpc("theme.list", {})).themes;
export const theme = (_app, key) => rpc("theme.get", { key });

// ---------------------------------------------------------------- syntax

// The syntax colours of bytes [start, end) of a held buffer once they are
// exact: {version, runs: [{start, end, key}]}, `key` a theme key
// ("syntax.string"). Rejects when the buffer has no colours or that range
// is not loaded.
export const highlights = (app, buffer, start, end) => {
  const b = held(app, buffer);
  return b === undefined ? Promise.reject(new Error("not a loaded buffer")) : app.highlightsOf(b, start, end);
};
// The last known category at byte `pos`, carried forward through edits the
// engine has not answered yet (for a decision on a keystroke): a category
// and whether it is exact, plain text, or unknown.
export const categoryAt = (app, buffer, pos) => {
  const b = held(app, buffer);
  return b === undefined ? { kind: "unknown" } : app.syntaxOf(b).categoryAt(pos);
};
// Told when a buffer's syntax colours changed, after the frame that draws
// them.
export const onColours = (app, f) => app.host.onBuffer("buffer.colours", (b, _change) => { f(b); });

// ---------------------------------------------------------------- language servers

export const lspAvailable = (app, buffer) => { const b = held(app, buffer); return b !== undefined && app.lspAvailable(b); };
// A request about `buffer` (at byte `pos` when it is about a position).
// A request about a buffer is sent once every edit made in it here is
// sequenced: the server then has the text its positions were taken from.
const edited = async (app, buffer) => { for (const b of found(held(app, buffer))) for (const f of b.file) await whenSynced(f); };
export const lspRequest = async (app, buffer, method, params) => { await edited(app, buffer); return rpc("lsp.call", { doc: docOfHeld(app, buffer), method, params, pos: null }); };
export const lspRequestAt = async (app, buffer, method, params, pos) => { await edited(app, buffer); return rpc("lsp.call", { doc: docOfHeld(app, buffer), method, params, pos }); };
// A request about the workspace (`workspace/symbol`) to `language`'s server.
export const lspWorkspaceRequest = (_app, language, method, params) => rpc("lsp.call", { language, method, params });
// LSP (line, character) to a byte offset in a held buffer, and back.
export const lspOffset = (app, buffer, line, character) => { const b = held(app, buffer); return b !== undefined ? lspOffsetIn(b, line, character) : null; };
export const lspPosition = (app, buffer, pos) => { const b = held(app, buffer); return b !== undefined ? lspPositionIn(b, pos) : null; };
// The diagnostics touching bytes [start, end) of `buffer` (an empty one
// covers the byte at its position), by start.
export const diagnostics = (app, buffer, start, end) => found(held(app, buffer)).flatMap((b) => b.decor.diagsIn(start, end)).map((d) => ({ start: d.start, end: d.end, severity: d.severity, message: d.message, source: d.source }));
// The languages with a server on the host.
export const lspLanguages = (app) => [...app.lspLanguages];
// Which servers run: {language: running}.
export const lspStatus = (_app) => rpc("lsp.status", {});
// Stop a language's server (until restarted), or start it again; both
// resolve with {language: running}.
export const lspStop = (_app, language) => rpc("lsp.stop", { language });
export const lspRestart = (_app, language) => rpc("lsp.restart", { language });
