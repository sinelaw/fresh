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
  // Each part gets its own getter for the application (made below): one
  // function shared by all of them would make the checker take their
  // classes for one type (each calls it as its method, `this.app()`).
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
  const app = new App({
    settings: loadSettings(),
    userKeys: loadUserKeys(),
    session: new Session(),
    workspace: ws,
    params,
    orchestrator: new Orchestrator(() => app, hello.workspaces, hello.folders, hello.machines),
    skins: new Skins(() => app),
    editing: new Editing(() => app),
    ui: new UIKit(() => app),
    prompt: new Palette(() => app),
    term: new TerminalPanel(terminalEditor, $("#terminal")),
    host: new Host((text) => { app.report(text); }),
    macros: new Macros(() => app),
    menubar: new MenuBar(() => app),
    input: $("#input"),
    pane: new Pane(() => app),
  });
  await app.start(PLUGINS);
}

main();
