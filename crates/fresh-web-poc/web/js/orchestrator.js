// The orchestrator: every workspace the server holds, with its live status,
// and moving between them.
//
// A page views one workspace (`?ws=<id>`): switching loads the page for the
// other one, so a workspace's tabs, splits, terminals and plugins never mix
// with another's, and each comes back as this browser left it (session.js).
// Two tabs can view two workspaces side by side.
//
//   Overview (Alt+O, or the workspace's name in the top bar): one row per
//     workspace — what needs you, its host and trust, branch, documents,
//     terminals and agent sessions, diagnostics — with Switch and Close;
//     Enter switches, Delete closes; New Workspace… and Start Agent Session…
//   Switch Workspace… (Ctrl+Alt+O): the same, as a quick pick.
//   New Workspace…: a folder on a host (this machine, SSH, a container, the
//     local agent), trusted or restricted, and optionally an agent session —
//     a terminal running a command (a coding agent's CLI) — started in it.
//   The status bar counts the other workspaces that need you; a click (or
//     Jump to Attention) goes to the one that has.
//
// Status comes from the server (`workspace.status`): a workspace needs you
// when one of its terminals rang the bell or an agent session stopped
// working (finished, exited, or asks a question). Viewing it clears that
// (`workspace.seen`).

import { rpc } from "./rpc.js";
import { h } from "./ui.js";
import { STATE_MARK } from "./terminal.js";

const STORE = "fresh-poc:orchestrator";
const HOSTS = [
  { value: "local", label: "This machine" },
  { value: "ssh", label: "SSH host" },
  { value: "container", label: "Container" },
  { value: "agent", label: "Remote agent (here)" },
];

// What a row shows first: what the workspace needs, at a glance.
const mark = (w) => {
  const st = w.status;
  if (st.attention) return "●";
  if (st.terminals.some((t) => t.agent && t.state === "working")) return "*";
  return " ";
};

// The New Workspace panels whose keys are wired.
const FORMS = new WeakSet();

const plural = (n, one) => `${n} ${one}${n === 1 ? "" : "s"}`;

export class Orchestrator {
  constructor(app, workspaces) {
    this.app = app;
    this.list = workspaces;
    this.panel = null;
    this.sel = 0;
    this.formGo = null; // the open New Workspace form's launch
    let saved = {};
    try { saved = JSON.parse(localStorage.getItem(STORE) || "{}") || {}; } catch {}
    this.prefs = { agent: "", recent: [], ...saved };
  }

  // The workspace this page views (null: none).
  get current() { return this.app.workspace?.id ?? null; }

  // Commands, keys, and seeing attention when this page is looked at
  // (once, after `new`).
  listen() {
    const cmd = (id, title, run) => this.app.commands.set(id, { title, run, source: "core", hidden: false });
    cmd("orchestrator.overview", "Orchestrator: Workspaces", () => this.toggle());
    cmd("orchestrator.switch", "Orchestrator: Switch Workspace…", () => this.quickSwitch());
    cmd("orchestrator.new", "Orchestrator: New Workspace…", () => this.newWorkspace());
    cmd("orchestrator.agent", "Orchestrator: Start Agent Session…", () => this.startAgent());
    cmd("orchestrator.close", "Orchestrator: Close Workspace", () => this.close(this.current));
    cmd("orchestrator.jump", "Orchestrator: Jump to Attention", () => this.jump());
    this.app.bindKey("M-o", "orchestrator.overview");
    this.app.bindKey("C-M-o", "orchestrator.switch");
    // From an agent's terminal too.
    this.app.terminalKeys.add("M-o").add("C-M-o");
    document.querySelector("#ws-name")?.addEventListener("click", () => this.show());
    const look = () => this.seeIfLooked();
    window.addEventListener("focus", look);
    document.addEventListener("visibilitychange", look);
    this.renderBeacon();
    look();
  }

  byId(id) { return this.list.find((w) => w.id === id) || null; }

  savePrefs() {
    try { localStorage.setItem(STORE, JSON.stringify(this.prefs)); } catch {}
  }

  // ----------------------------------------------------------- events

  onEvent(ev) {
    switch (ev.ev) {
      case "workspace.added":
        if (!this.byId(ev.workspace.id)) this.list.push(ev.workspace);
        break;
      case "workspace.removed":
        this.list = this.list.filter((w) => w.id !== ev.workspace);
        if (ev.workspace === this.current) {
          // Nothing more can be done here: say so, and offer the others.
          this.app.workspace = null;
          this.app.setStatus("This workspace was closed");
          document.querySelector("#ws-name").textContent = "Closed workspace";
          this.show();
        }
        break;
      case "workspace.status": {
        const w = this.byId(ev.workspace);
        if (!w) break;
        w.status = ev.status;
        if (w.id === this.current) {
          this.app.term?.states(ev.status.terminals);
          this.seeIfLooked();
        }
        break;
      }
      default:
        return;
    }
    this.render();
    this.renderBeacon();
  }

  looked() { return document.visibilityState === "visible" && document.hasFocus(); }

  // What this workspace needed, once this page is looked at: said in the
  // status bar, and cleared.
  seeIfLooked() {
    const w = this.byId(this.current);
    if (!w?.status.attention || !this.looked()) return;
    this.app.setStatus(w.status.attention);
    rpc("workspace.seen").catch(() => {});
  }

  // ----------------------------------------------------------- moving

  // View another workspace: this page loads it (its own tabs and splits
  // come back with it).
  switchTo(id) {
    if (id === this.current) { this.panel?.close(); return; }
    const url = new URL(location.href);
    url.searchParams.set("ws", String(id));
    url.searchParams.delete("open");
    location.assign(url.href);
  }

  // The other workspaces that need you, the one that has waited longest
  // (opened first) first.
  waiting() { return this.list.filter((w) => w.id !== this.current && w.status.attention); }

  jump() {
    const w = this.waiting()[0];
    if (w) this.switchTo(w.id);
    else this.app.setStatus("No other workspace needs you");
  }

  renderBeacon() {
    const n = this.waiting().length;
    this.app.statusItem("orchestrator").set(n ? `● ${n} need${n === 1 ? "s" : ""} you` : "", {
      title: this.waiting().map((w) => `${w.name}: ${w.status.attention}`).join("\n"),
      onClick: () => this.jump(),
    });
  }

  // ----------------------------------------------------------- overview

  toggle() {
    if (this.panel?.open) this.panel.close();
    else this.show();
  }

  show() {
    this.sel = Math.max(0, this.list.findIndex((w) => w.id === this.current));
    this.panel = this.app.ui.panel({ id: "orchestrator", title: "Workspaces", place: "dialog" });
    this.render();
    this.panel.focus("workspaces");
  }

  // One line per workspace, with its actions.
  row(w) {
    const st = w.status;
    const here = w.id === this.current;
    const agents = st.terminals.filter((t) => t.agent);
    const shells = st.terminals.filter((t) => !t.agent && t.state !== "exited").length;
    const where = `${w.host}:${w.root}`;
    return h("row", { class: "orch-row" + (here ? " current" : "") },
      h("text", { class: "orch-mark" + (st.attention ? " attention" : ""), title: st.attention || "" }, mark(w)),
      h("text", { bold: here, class: "orch-name", title: where }, w.name),
      st.trusted ? null : h("badge", { class: "orch-restricted" }, "restricted"),
      st.branch ? h("text", { class: "orch-branch" }, `⎇ ${st.branch}`) : null,
      st.attention ? h("text", { class: "orch-attention" }, st.attention) : h("text", { dim: true, class: "orch-where" }, where),
      h("spacer", {}),
      st.docs ? h("text", { dim: !st.modified, title: "Open files (unsaved)" }, `${st.modified ? `●${st.modified} ` : ""}${plural(st.docs, "file")}`) : null,
      shells ? h("text", { dim: true }, plural(shells, "terminal")) : null,
      ...agents.map((t) => h("text", { class: `orch-agent ${t.state}`, title: `${t.command} — ${t.state}` }, `${STATE_MARK[t.state] || ""}${t.name}`)),
      st.errors ? h("text", { class: "orch-errors", title: "Errors" }, `✖ ${st.errors}`) : null,
      st.warnings ? h("text", { class: "orch-warnings", title: "Warnings" }, `▲ ${st.warnings}`) : null,
      st.viewers ? h("text", { dim: true, title: "Pages viewing it" }, `◉ ${st.viewers}`) : null,
      h("button", { class: "orch-act", title: here ? "This page views it" : "Switch to it (Enter)", disabled: here, onClick: () => this.switchTo(w.id) }, here ? "here" : "switch"),
      h("button", { class: "orch-act", title: "Close it (Delete)", onClick: () => this.close(w.id) }, "close"));
  }

  render() {
    if (!this.panel?.open) return;
    this.sel = Math.min(this.sel, Math.max(0, this.list.length - 1));
    const here = this.byId(this.current);
    this.panel.render(h("col", { class: "orch" },
      h("row", {},
        h("text", { dim: true }, this.list.length ? `${plural(this.list.length, "workspace")} · Enter switches · Delete closes` : "No workspace is open"),
        h("spacer", {}),
        here ? h("button", { disabled: !here.status.trusted, title: here.status.trusted ? "A terminal running an agent's command, here" : "Trust the workspace to run an agent", onClick: () => this.startAgent() }, "Start Agent Session…") : null,
        h("button", { primary: true, onClick: () => this.newWorkspace() }, "New Workspace…")),
      h("list", {
        key: "workspaces",
        class: "orch-list",
        items: this.list,
        selected: this.sel,
        render: (x) => this.row(x),
        onSelect: (_x, i) => { this.sel = i; },
        onActivate: (x) => x && this.switchTo(x.id),
        onKey: (e, x) => { if (e.key === "Delete" && x) { e.preventDefault(); this.close(x.id); } },
      })));
  }

  quickSwitch() {
    this.app.prompt.open({
      title: "Switch workspace",
      items: this.list.map((w) => ({
        label: `${mark(w)} ${w.name}${w.id === this.current ? " (this one)" : ""}`,
        detail: [w.host, w.status.branch, w.status.attention].filter(Boolean).join(" · "),
        value: w.id,
      })),
      onAccept: (it) => this.switchTo(it.value),
    });
  }

  // ----------------------------------------------------------- acting

  // Close a workspace: its terminals and agents stop. Unsaved documents
  // are asked about.
  async close(id) {
    const w = this.byId(id);
    if (!w) return;
    const running = w.status.terminals.filter((t) => t.state !== "exited").length;
    const what = running ? ` Its ${plural(running, "terminal")} stop${running === 1 ? "s" : ""}.` : "";
    if (!(await this.app.confirm(`Close ${w.name}?${what}`, "Close"))) return;
    try {
      await rpc("workspace.close", { workspace: id });
    } catch (e) {
      if (e.code !== "unsaved") { this.app.setStatus(`Not closed: ${e.detail}`); return; }
      if (!(await this.app.confirm(`${e.detail}. Close it and discard them?`, "Discard and close"))) return;
      await rpc("workspace.close", { workspace: id, discard: true });
    }
    this.app.setStatus(`Closed ${w.name}`);
  }

  // An agent session here: a terminal running the command.
  async startAgent() {
    if (!this.app.workspace) return;
    if (!this.app.trusted) { this.app.setStatus("Trust the workspace to run an agent (Workspace: Trust)"); return; }
    const command = await this.app.prompt.ask({ title: "Agent command", value: this.prefs.agent, hint: "Runs in a terminal in the workspace's root, in your login shell" });
    if (!command || !command.trim()) return;
    this.prefs.agent = command;
    this.savePrefs();
    this.panel?.close();
    const term = this.app.term;
    const id = await term.create({ command });
    if (!term.visible) await term.toggle();
    term.select(id);
  }

  // The New Workspace form: where, on which host, trusted or not, and an
  // agent to start in it.
  newWorkspace() {
    this.panel?.close();
    const f = {
      root: this.prefs.recent[0] || "", host: "local", target: "", trusted: false, agent: "",
      busy: false, error: "",
    };
    const panel = this.app.ui.panel({ id: "orchestrator-new", title: "New Workspace", place: "dialog" });
    const go = async (background) => {
      if (f.busy || !f.root.trim()) return;
      const host = { kind: f.host };
      if (f.host === "ssh") host.target = f.target.trim();
      if (f.host === "container") host.id = f.target.trim();
      f.busy = true;
      f.error = "";
      render();
      try {
        const r = await rpc("workspace.open", { root: f.root.trim(), host, trusted: f.trusted, agent: f.agent.trim() || null });
        this.prefs.recent = [f.root.trim(), ...this.prefs.recent.filter((x) => x !== f.root.trim())].slice(0, 8);
        if (f.agent.trim()) this.prefs.agent = f.agent.trim();
        this.savePrefs();
        if (!this.byId(r.workspace.id)) this.list.push(r.workspace);
        panel.close();
        if (background) { this.app.setStatus(`Opened ${r.workspace.name} in the background`); this.renderBeacon(); }
        else this.switchTo(r.workspace.id);
      } catch (e) {
        f.busy = false;
        f.error = e.detail || String(e);
        render();
      }
    };
    // Ctrl+Enter / Alt+Enter anywhere in the form: an input leaves them to
    // the form.
    const launch = (e) => e.key === "Enter" && (e.ctrlKey || e.altKey);
    const keys = (e) => launch(e);
    this.formGo = go;
    if (!FORMS.has(panel)) {
      FORMS.add(panel);
      panel.el.addEventListener("keydown", (e) => { if (launch(e)) { e.preventDefault(); this.formGo(e.altKey); } });
    }
    const field = (label, control) => h("row", { class: "orch-field" }, h("text", { class: "orch-label" }, label), control);
    const render = () => panel.render(h("col", { class: "orch-form" },
      field("Folder", h("input", { key: "root", value: f.root, placeholder: "/path/to/project (~ for home)", onInput: (v) => { f.root = v; }, onEnter: () => go(false), onKey: keys })),
      field("Host", h("select", { key: "host", value: f.host, options: HOSTS, onChange: (v) => { f.host = v; render(); } })),
      f.host === "ssh" || f.host === "container"
        ? field(f.host === "ssh" ? "Target" : "Container", h("input", { key: "target", value: f.target, placeholder: f.host === "ssh" ? "[user@]host[:port]" : "name or id", onInput: (v) => { f.target = v; }, onEnter: () => go(false), onKey: keys }))
        : null,
      field("Trust", h("checkbox", { key: "trusted", checked: f.trusted, onChange: (v) => { f.trusted = v; } }, "Run processes, terminals and language servers in it")),
      field("Agent", h("input", { key: "agent", value: f.agent, placeholder: `optional, in a trusted workspace: a command to run in a terminal, e.g. ${this.prefs.agent || "claude"}`, onInput: (v) => { f.agent = v; }, onEnter: () => go(false), onKey: keys })),
      this.prefs.recent.length ? field("Recent", h("row", { class: "orch-recent" }, ...this.prefs.recent.map((r) => h("button", { onClick: () => { f.root = r; render(); panel.focus("root"); } }, r)))) : null,
      f.error ? h("text", { class: "orch-error" }, f.error) : null,
      h("row", {},
        h("text", { dim: true }, f.busy ? "Opening…" : "Ctrl+Enter opens · Alt+Enter opens in the background"),
        h("spacer", {}),
        h("button", { disabled: f.busy, onClick: () => go(true) }, "Open in Background"),
        h("button", { primary: true, disabled: f.busy, onClick: () => go(false) }, "Open"))));
    render();
    panel.focus("root");
  }
}
