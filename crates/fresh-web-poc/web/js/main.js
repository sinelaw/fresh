// The page's entry: asks the server which workspace this page views, makes
// the application's parts (each holding a getter for the application, which
// is made last, from them) and starts it with the built-in plugins.

import { rpc, session } from "./rpc.js";
import { App, loadSettings, loadUserKeys } from "./app.js";
import { Pane } from "./view.js";
import { Editing } from "./editing.js";
import { TerminalPanel } from "./terminal.js";
import { UIKit } from "./ui.js";
import { Host } from "./host.js";
import { Palette } from "./palette.js";
import { Skins } from "./skins.js";
import { Orchestrator } from "./orchestrator.js";
import { Session } from "./session.js";
import { MenuBar } from "./menubar.js";
import { Macros } from "./macros.js";
import { PLUGINS } from "../plugins/index.js";

import { $ } from "./dom.js";

async function main() {
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
  // whole application. The terminal panel: say something in the status
  // bar, give the keyboard back, lay the window out again, and let the
  // editor take its own keys from a focused terminal (true: it took one).
  const terminalEditor = {
    say: (text) => { app.setStatus(text); },
    focus: () => { app.focusPane(app.activePane); },
    relayout: () => { app.relayout(); },
    key: (e) => app.terminalKey(e),
  };
  // Skins: report a failure, say something, measure the editor's cell
  // again (a skin's fonts are in), put an editor theme on, pick from a
  // list, and count the open files and their problems.
  const skinsEditor = {
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
  const macrosEditor = {
    say: (text) => { app.setStatus(text); },
    keyLabel: (id) => app.keyLabel(id),
    updateStatus: () => { app.updateStatus(); },
    run: (id, arg) => app.run(id, arg),
    insert: (text, typed) => { app.insertText(text, typed); },
    pick: (o) => { app.prompt.open(o); },
    titleOf: (id) => app.commands.get(id)?.title ?? id,
    showText: (name, text) => { app.showText(name, text); },
  };
  // The menu bar: a command's title (none: no such command), its key,
  // running it, whether a check is on, and giving the keyboard back.
  const menuEditor = {
    title: (id) => app.commands.get(id)?.title ?? null,
    keyLabel: (id) => app.keyLabel(id),
    run: (id) => { app.run(id, undefined); },
    isOn: (kind, name) => app.isOn(kind, name),
    focus: () => { app.focusPane(app.activePane); },
  };
  // The palette: where the active view is (to put it back after a
  // preview), go to a line, and the commands, buffers and files it lists.
  const paletteEditor = {
    here: () => app.here(),
    goto: (line) => { app.gotoLine(line, 0, { history: false }); },
    commands: () => [...app.commands].filter(([, c]) => !c.hidden).map(([id, c]) => ({ title: c.title, key: app.keyFor(id), source: c.source, run: () => { app.run(id, undefined); } })),
    buffers: () => [...app.buffers.values()].map((b) => ({ name: b.name, path: b.path, modified: b.modified, stored: b.doc !== null, show: () => { app.showBuffer(b, "tab", { preview: null, focus: true }); } })),
    root: () => app.root,
    recent: () => app.recentFiles(),
    open: (path, line, column) => { app.openFile(path, { line, column, place: "tab", preview: null, focus: true }); },
    focus: () => { app.focusPane(app.activePane); },
  };
  // The panel kit: give the keyboard back, and redraw the panes when the
  // dock takes or gives room.
  const uiEditor = {
    focus: () => { app.focusPane(app.activePane); },
    redraw: () => { app.renderAll(); },
  };
  // The parts not yet given their own ports each get their own getter for
  // the application: one function shared by all of them would make the
  // checker take their classes for one type (each calls it as its method,
  // `this.app()`).
  const app = new App({
    settings: loadSettings(),
    userKeys: loadUserKeys(),
    session: new Session(),
    workspace: ws,
    params,
    orchestrator: new Orchestrator(() => app, hello.workspaces, hello.folders, hello.machines),
    skins: new Skins(skinsEditor, ws === null ? null : ws.id),
    editing: new Editing(() => app),
    ui: new UIKit(uiEditor),
    prompt: new Palette(paletteEditor),
    term: new TerminalPanel(terminalEditor, $("#terminal")),
    host: new Host((text) => { app.report(text); }),
    macros: new Macros(macrosEditor),
    menubar: new MenuBar(menuEditor),
    input: $("#input"),
    pane: new Pane(() => app),
  });
  await app.start(PLUGINS);
}

main();
