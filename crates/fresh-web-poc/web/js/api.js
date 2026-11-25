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

import { rpc, RpcError } from "./rpc.js";
import { BufferHandle, ViewHandle } from "./host.js";
import { asCommand } from "./commands.js";
export { byteLen } from "./ot.js";
export { stringWidth as textWidth } from "./layout.js";

const bufferOf = (app, b) => (b ? new BufferHandle(app, b) : null);
const viewOf = (app, v) => (v ? new ViewHandle(app, v) : null);

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
export const run = (app, id, arg) => app.run(id, arg);
export const commands = (app) => [...app.commands].map(([id, c]) => ({ id, title: c.title, source: c.source, hidden: c.hidden }));
export const bindKey = (app, key, command) => app.bindKey(key, command);
export const keyFor = (app, command) => app.keyFor(command);
// The first key for a command as people write it ("Ctrl+Shift+K").
export const keyLabel = (app, command) => app.keyLabel(command);
// The effective keymap: {key: command}.
export const keymap = (app) => app.keymap();
// The user's own bindings (saved in the browser), over the defaults.
export const userKeys = (app) => ({ ...app.userKeys });
export const setUserKey = (app, key, command) => {
  const others = Object.entries(app.userKeys).filter(([k]) => k !== key);
  app.userKeys = Object.fromEntries(command ? [...others, [key, command]] : others);
  app.saveKeys();
};
// The next key pressed, as the keymap names it.
export const captureKey = (app) => app.captureKey();
// Keys for a buffer kind: defineMode(app, "git-log", { Enter: fn, q: fn }).
export const defineMode = (app, mode, keys) => { app.modes.set(mode, keys); };
// A row at the end of a menu bar menu: addMenuItem(app, "Go",
// fresh.menuCommand("bookmark.list", "Bookmarks…")); menuToggle's row has a
// check, `checked(app)` read when the menu opens; MENU_BAR_SEPARATOR parts.
export const addMenuItem = (app, menu, row) => app.menubar.add(menu, row);
export { cmd as menuCommand, toggle as menuToggle, SEP as MENU_BAR_SEPARATOR } from "./menubar.js";
// A context menu's items (fresh.menu): an action, one with its keys, check
// and state given, or a separator.
export { action as menuAction, actionOf as menuActionOf, SEPARATOR as MENU_SEPARATOR } from "./menu.js";
// A palette mode for a prefix: paletteMode(app, "@", {hint, items(q)}).
export const paletteMode = (app, prefix, o) => app.prompt.provide(prefix, o);

// ---------------------------------------------------------------- events

export const on = (app, event, f) => app.host.on(event, f);
export const onBuffer = (app, event, f) => app.host.onBuffer(event, (b, change) => f(new BufferHandle(app, b), change));
export const onView = (app, event, f) => app.host.onView(event, (v) => f(new ViewHandle(app, v)));
export const onLspStatus = (app, f) => app.host.onStatus(f);

// ---------------------------------------------------------------- buffers and views

export const activeBuffer = (app) => { const v = app.view(); return v ? new BufferHandle(app, v.buf) : null; };
export const buffers = (app) => [...app.buffers.values()].map((b) => new BufferHandle(app, b));
export const bufferById = (app, id) => bufferOf(app, app.buffers.get(id));
export const bufferByPath = (app, path) => bufferOf(app, [...app.buffers.values()].find((b) => b.path === path));
// `at`: OpenOptions (types/fresh.d.js).
export const open = async (app, path, at) => { const v = await app.openFile(path, at); return v ? new BufferHandle(app, v.buf) : null; };
// The buffer for a file, without showing it.
export const load = async (app, path) => bufferOf(app, await app.loadFile(path));
// A VirtualOptions (types/fresh.d.js).
export const createVirtual = (app, o) => new BufferHandle(app, app.createVirtual(o));
export const show = (app, buffer, place) => app.showBuffer(buffer.b, place, { preview: null, focus: true });
export const closeBuffer = (app, buffer) => app.closeBuffer(buffer.b);
export const activeView = (app) => viewOf(app, app.view());
export const views = (app) => app.panes().flatMap((p) => p.views).map((v) => new ViewHandle(app, v));
// Scroll two buffers' views together through a line mapping: aToB(line of
// a) -> line of b, bToA the other way. Returns the function that unlinks
// them.
export const linkScroll = (app, a, b, aToB, bToA) => app.linkScroll(a.b, b.b, aToB, bToA);

// ---------------------------------------------------------------- the UI

// A PanelOptions (types/fresh.d.js).
export const panel = (app, o) => app.ui.panel(o);
// The panel `id` if it is on screen, else undefined.
export const shown = (app, id) => app.ui.shown(id);
// A context menu: items [{label, run}] (null: separator) at {x, y}.
export const menu = (app, items, at) => app.ui.menu(items, at);
// Pick from a list: {title, items: [{label, detail, value}], onAccept, onChange}.
export const pick = (app, o) => app.prompt.open(o);
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
export const errorText = (e) => (e instanceof RpcError ? e.detail : String(e));
export const showError = (app, e) => app.setStatus(errorText(e));
export const popup = (app, text, at) => app.popup(text, at);
// Colours: {"--var": css} for the page's colour variables (style.css).
export const setTheme = (app, vars) => app.applyTheme(vars);
// Text shown when the pointer rests on a position: f(buffer, pos) -> string|null.
export const hoverProvider = (app, f) => app.hoverProvide((b, p) => f(new BufferHandle(app, b), p));

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
export const searchDocument = async (_app, buffer, query, from, backward) => (await rpc("doc.search", { doc: buffer.b.doc, query, from, backward })).pos;
// The editor's themes: [{key, name}], and one fully resolved.
export const themes = async (_app) => (await rpc("theme.list", {})).themes;
export const theme = (_app, key) => rpc("theme.get", { key });

// ---------------------------------------------------------------- language servers

export const lspAvailable = (app, buffer) => app.lspAvailable(buffer.b);
// A request about `buffer` (at byte `pos` when it is about a position).
export const lspRequest = (_app, buffer, method, params, pos) => rpc("lsp.call", { doc: buffer.b.doc, method, params, pos });
// A request about the workspace (`workspace/symbol`) to `language`'s server.
export const lspWorkspaceRequest = (_app, language, method, params) => rpc("lsp.call", { language, method, params });
// LSP (line, character) to a byte offset in a held buffer, and back.
export const lspOffset = (app, buffer, line, character) => app.lspOffset(buffer.b, line, character);
export const lspPosition = (app, buffer, pos) => app.lspPosition(buffer.b, pos);
export const diagnostics = (_app, buffer) => buffer.b.decor.diags.map((d) => ({ start: d.start, end: d.end, severity: d.severity, message: d.message, source: d.source }));
// The languages with a server on the host.
export const lspLanguages = (app) => [...app.lspLanguages];
// Which servers run: {language: running}.
export const lspStatus = (_app) => rpc("lsp.status", {});
// Stop a language's server (until restarted), or start it again; both
// resolve with {language: running}.
export const lspStop = (_app, language) => rpc("lsp.stop", { language });
export const lspRestart = (_app, language) => rpc("lsp.restart", { language });
