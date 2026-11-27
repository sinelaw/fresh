// The page's entry: loads the WebAssembly module (every buffer keeps its
// decorations' positions in it, so nothing starts without it), asks the
// server which workspace this page views, makes the application's parts
// (each holding a getter for the application, which is made last, from
// them) and starts it with the built-in plugins.

import { filePath } from "./buffer.js";
import { rpc, session, failure } from "./rpc.js";
import { App, loadSettings, loadUserKeys, scrollToFraction, lspPosition, lspOffset } from "./app.js";
import { Editing, selectWord, selectLine, setBlock, toggleFoldAt, wordAt, bracketPair } from "./editing.js";
import { Terminals } from "./terminal.js";
import { UIKit } from "./ui.js";
import { Host } from "./host.js";
import { Palette } from "./palette.js";
import { Skins } from "./skins.js";
import { Orchestrator } from "./orchestrator.js";
import { Session } from "./session.js";
import { MenuBar } from "./menubar.js";
import { Macros } from "./macros.js";
import { PLUGINS } from "../plugins/index.js";
import { loadWasm } from "./wasm.js";
import { useClusters } from "./clusters.js";

import { $ } from "./dom.js";
import { found, listOf, some, none } from "./maybe.js";

// The longest word, in UTF-16 units either side of the cursor, whose
// occurrences are highlighted (a run of letters longer than that, as CJK
// prose without spaces is, has none).
const OCCURRENCE_MAX = 256;

// The page cannot start: it says why, in place of the editor.
function cannotStart(why) {
  const p = document.createElement("p");
  p.className = "cannot-start";
  p.textContent = `Fresh cannot start: ${why}`;
  $("#app").replaceChildren(p);
}

async function main() {
  /** let wasm: Wasm */
  let wasm;
  try { wasm = await loadWasm(); } catch (e) { cannotStart(`its WebAssembly module did not load (${failure(e)}).`); return; }
  useClusters(wasm);
  // The workspace this page views: `?ws=<id>`, or the server's first. None
  // when it was closed (or none is open): the page is then the
  // orchestrator's dock alone.
  const params = new URLSearchParams(location.search);
  const asked = params.get("ws");
  const hello = await rpc("hello", { workspace: asked === null ? null : Number(asked) });
  session.client = hello.client;
  const ws = hello.workspace;
  if (ws !== null && params.get("ws") !== String(ws.id)) {
    params.set("ws", String(ws.id));
    history.replaceState(null, "", `?${params}`);
  }
  // What each part asks of the editor: the few functions it needs, not the
  // whole application. Terminals: let the editor take its own keys from a
  // focused terminal (true: it took one), and hear that a terminal's label
  // changed or that another client opened one.
  const terminalsEditor /*: TerminalsEditor */ = {
    key: (e) => app.terminalKey(e),
    changed: (id) => { app.terminalChanged(id); },
    appeared: (id) => { app.terminalAppeared(id); },
  };
  // Skins: report a failure, say something, measure the editor's cell
  // again (a skin's fonts are in), put an editor theme on, pick from a
  // list, and count the open files and their problems.
  const skinsEditor /*: SkinsEditor */ = {
    report: (text) => { app.report(text); },
    say: (text) => { app.setStatus(text); },
    remeasure: () => { app.remeasure(); },
    useTheme: (key) => app.useTheme(key),
    pick: (o) => { app.prompt.open(o); },
    counts: () => app.counts(),
  };
  // Macros: say something, a command's key and title, refresh the status
  // bar, run a command, put text in the active view, pick from a list, and
  // show a text.
  const macrosEditor /*: MacrosEditor */ = {
    say: (text) => { app.setStatus(text); },
    keyLabel: (id) => app.keyLabel(id),
    updateStatus: () => { app.updateStatus(); },
    run: (id, arg) => app.run(id, arg),
    insert: (text, typed) => { app.insertText(text, typed); },
    pick: (o) => { app.prompt.open(o); },
    titleOf: (id) => found(app.commands.get(id)).map((c) => c.title).at(0) ?? id,
    showText: (name, text) => { app.showText(name, text); },
  };
  // The menu bar: a command's title (none: no such command), its key,
  // running it, whether a check is on, and giving the keyboard back.
  const menuEditor /*: MenuEditor */ = {
    title: (id) => found(app.commands.get(id)).map((c) => c.title).at(0) ?? null,
    keyLabel: (id) => app.keyLabel(id),
    run: (id) => { app.run(id, undefined); },
    isOn: (kind, name) => app.isOn(kind, name),
    focus: () => { app.focusPane(app.activePane); },
  };
  // The palette: where the active view is (to put it back after a
  // preview), go to a line, and the commands, buffers and files it lists.
  const paletteEditor /*: PaletteEditor */ = {
    here: () => app.here(),
    goto: (line) => { app.gotoLine(line, 0, false); },
    commands: () => [...app.commands].filter(([, c]) => !c.hidden).map(([id, c]) => ({ title: c.title, keys: app.keyFor(id), source: c.source, run: () => { app.run(id, undefined); } })),
    buffers: () => [...app.buffers.values()].map((b) => ({ name: b.name, path: filePath(b.origin), modified: b.modified, stored: b.doc !== null, show: () => { app.showBuffer(b, "tab", { preview: null, focus: true }); } })),
    root: () => app.root,
    recent: () => app.recentFiles(),
    open: (path) => { app.openFile(path, { line: null, column: 1, place: "tab", preview: null, focus: true }); },
    openAt: (path, line, column) => { app.openFile(path, { line, column, place: "tab", preview: null, focus: true }); },
    focus: () => { app.focusPane(app.activePane); },
  };
  // The panel kit: give the keyboard back, and redraw the panes when the
  // dock takes or gives room.
  const uiEditor = {
    focus: () => { app.focusPane(app.activePane); },
    redraw: () => { app.renderAll(); },
  };
  // A pane: focus, settings and cell size, the editing a click or drag
  // does, the tab, menu, hover and scroll events it passes on, and a
  // terminal's screen for a terminal's tab.
  const paneEditor = {
    isActive: (pane) => app.activePane === pane,
    focus: (pane) => { app.focusPane(pane); },
    drive: (pane) => { app.scrollDriver = [pane]; },
    settings: () => app.settings,
    metrics: () => app.metrics,
    shown: (pane, v) => { app.onViewShown(pane, v); },
    dropIfUnshown: (buf) => { app.dropIfUnshown(buf); },
    resized: (v) => { app.viewEvent("view.resized", v); },
    updateStatus: () => { app.updateStatus(); },
    syncScroll: (pane, v) => { app.syncScroll(pane, v); },
    scrollTo: (pane, f) => { scrollToFraction(pane, f); },
    hover: (pane, pos, x, y) => { app.onHover(pane, pos, x, y); },
    editorMenu: (pane, at) => { app.editorMenu(pane, at); },
    tabMenu: (pane, v, at) => { app.tabMenu(pane, v, at); },
    tabPress: (e, pane, v) => { app.tabPress(e, pane, v); },
    closeTab: (pane, v) => { app.closeTab(pane, v); },
    run: (id) => { app.run(id, undefined); },
    place: (pane, pos, extend) => { app.editing.place(pane, pos, extend); },
    selectWord: (pane) => { selectWord(pane); },
    selectLine: (pane) => { selectLine(pane); },
    setBlock: (pane, anchor, head) => { setBlock(pane, anchor, head); },
    toggleFold: (pane, line) => { toggleFoldAt(pane, line); },
    mountTerminal: (id, box) => { app.terminals.mount(id, box); },
    wordAt: (buf, pos) => wordAt(buf, pos, OCCURRENCE_MAX),
    bracketPair: (buf, pos) => bracketPair(app.syntaxOf(buf), buf, pos),
    syntaxOf: (buf) => app.syntaxOf(buf),
    wantColours: (buf, from, to) => { for (const hl of app.hl) hl.want(buf, from, to); },
  };
  // Editing: say something, apply an edit (as typed, or not), hear that a
  // cursor moved, the auto-close setting, and language server positions.
  const editingEditor = {
    say: (text) => { app.setStatus(text); },
    edit: (b, ops, typing) => { app.applyTyped(b, ops, typing); },
    moved: (v, old) => { app.cursorMoved(v, old); },
    autoClose: () => app.settings.flags.auto_close === true,
    lspAvailable: (b) => app.lspAvailable(b),
    syntaxOf: (b) => app.syntaxOf(b),
    lspPosition: (b, pos) => lspPosition(b, pos),
    lspOffset: (b, line, character) => lspOffset(b, line, character),
  };
  // The orchestrator: the workspace this page views (its id, root and
  // trust, and that it was closed); saying, asking and confirming; the keys
  // it binds; its status bar item; the panels and menus it shows; and the
  // terminals it reports on and runs agents in.
  const orchestratorEditor = {
    workspaceId: () => { const ids = listOf(app.workspace).map((w) => w.id); return ids.length > 0 ? some(ids[0]) : none(); },
    root: () => app.root,
    trusted: () => app.trusted,
    closed: () => { app.workspace = none(); },
    say: (text) => { app.setStatus(text); },
    focus: () => { app.focusPane(app.activePane); },
    pick: (o) => { app.prompt.open(o); },
    ask: (o) => app.prompt.ask(o),
    confirm: (title, yes) => app.confirm(title, yes),
    bindKey: (key, id) => { app.bindKey(key, id); },
    passKey: (key) => { app.terminalKeys.add(key); },
    status: (text, o) => { app.statusItem("orchestrator").set(text, o); },
    panel: (o) => app.ui.panel(o),
    shown: (id) => app.ui.shown(id),
    menu: (items, at) => { app.ui.menu(items, at); },
    dropdown: (items, anchor) => { app.ui.dropdown(items, anchor); },
    terminals: (states) => { app.terminals.states(states); },
    runInTerminal: (command, resume) => app.runInTerminal(command, resume),
  };
  const app = new App({
    settings: loadSettings(),
    userKeys: loadUserKeys(),
    session: new Session(),
    workspace: ws,
    params,
    orchestrator: new Orchestrator(orchestratorEditor, hello.workspaces, hello.folders, hello.machines),
    skins: new Skins(skinsEditor, ws === null ? null : ws.id),
    editing: new Editing(editingEditor),
    ui: new UIKit(uiEditor),
    prompt: new Palette(paletteEditor),
    terminals: new Terminals(terminalsEditor),
    host: new Host((text) => { app.report(text); }),
    macros: new Macros(macrosEditor),
    menubar: new MenuBar(menuEditor),
    input: $("#input"),
    paneEditor,
    wasm,
  });
  await app.start(PLUGINS);
}

main();
