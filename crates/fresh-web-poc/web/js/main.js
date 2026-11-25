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

const $ = (s) => document.querySelector(s);

async function main() {
  // The workspace this page views: `?ws=<id>`, or the server's first. None
  // when it was closed (or none is open): the page is then the
  // orchestrator's dock alone.
  const params = new URLSearchParams(location.search);
  const hello = await rpc("hello", params.has("ws") ? { workspace: Number(params.get("ws")) } : {});
  session.client = hello.client;
  const ws = hello.workspace;
  if (ws && params.get("ws") !== String(ws.id)) {
    params.set("ws", String(ws.id));
    history.replaceState(null, "", `?${params}`);
  }
  const getApp = () => app;
  const app = new App({
    settings: loadSettings(),
    userKeys: loadUserKeys(),
    session: new Session(),
    workspace: ws,
    params,
    orchestrator: new Orchestrator(getApp, hello.workspaces, hello.folders),
    skins: new Skins(getApp),
    editing: new Editing(getApp),
    ui: new UIKit(getApp),
    prompt: new Palette(getApp),
    term: new TerminalPanel(getApp, $("#terminal")),
    host: new Host(getApp),
    macros: new Macros(getApp),
    menubar: new MenuBar(getApp),
    input: $("#input"),
    pane: new Pane(getApp),
  });
  await app.start(PLUGINS);
}

main();
