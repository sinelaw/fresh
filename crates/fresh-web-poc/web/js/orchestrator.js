// The orchestrator: every workspace the server holds, with its live state,
// in a dock beside the editor, and moving between them — the editor's
// Orchestrator (docs/features/orchestrator.md) for a page that views one
// workspace at a time.
//
// A page views one workspace (`?ws=<id>`): switching loads the page for the
// other one, so a workspace's tabs, splits, terminals and plugins never mix
// with another's, and each comes back as this browser left it (session.js).
// Two tabs can view two workspaces side by side.
//
//   The dock (Alt+O moves the keyboard to it and back; Orchestrator: Toggle
//     Dock; the workspace's name in the top bar): every workspace, its
//     state at its right edge — ● needs you, ✓ finished work not looked at,
//     * working, · idle, ? no signal, ! could not be opened (Enter
//     retries) — grouped in folders (or by the day last worked in), pinned
//     ones on top; a click switches; right-click (or F2) for its menu:
//     rename, move to a folder, pin, start an agent, close (and delete its
//     worktree); `/` searches.
//   Switch Workspace… (Ctrl+Alt+O): the same as a quick pick.
//   New Workspace…: an agent (the registry in agents.js, a terminal, or a
//     command) with its first prompt, where (host and folder), git (a new
//     worktree on a new branch, or the folder as it is), trust, a name and
//     a dock folder; Launch, or Launch in background (the dock shows it
//     being made, and why it failed, with Retry).
//   The status bar counts the other workspaces that need you; a click, or
//     Jump to Attention, goes to one (Jump Back returns).
//
// State comes from the server (`workspace.status`, `src/terminal.rs`): a
// workspace's terminals are working, waiting on a question, done (finished
// work nobody has seen), idle; its attention says why it needs the user.
// Viewing it clears that (`workspace.seen`).

import { rpc } from "./rpc.js";
import { h } from "./ui.js";
import { AGENT_CHOICES, agentById, agentCommand, agentResume } from "./agents.js";
import { $ } from "./dom.js";
import { asCommand } from "./commands.js";
import { action, actionOf } from "./menu.js";

const STORE = "fresh-poc:orchestrator";
const JUMPED = "fresh-poc:orchestrator:jumped-from";
// What a New Workspace folder is, as the machine's git says: being asked
// about, not there, a folder outside git, or in a git work tree (its top).
/** type Where = {kind: "asking"} | {kind: "missing"} | {kind: "plain"} | {kind: "repo", toplevel: String} */
/** const ASKING: Where */
const ASKING = { kind: "asking" };
/** function whereOf(r: {repo: Boolean, folder: Boolean, toplevel: String}) => Where */
function whereOf(r) {
  if (!r.folder) return { kind: "missing" };
  return r.repo ? { kind: "repo", toplevel: r.toplevel } : { kind: "plain" };
}

// A host as people read it: an SSH target, a container, the agent.
const hostText = (host) => (host.kind === "ssh" ? host.target : host.kind === "container" ? `container ${host.id}` : host.kind);

// The dock's marks, as the editor's.
export const MARKS = { failed: "!", pending: "…", waiting: "●", done: "✓", working: "*", idle: "·", unknown: "?", exited: "·" };
// The order a workspace's terminals decide its state in.
const RANK = ["waiting", "done", "working", "idle", "unknown", "exited"];
// A remote host's mark beside the state.
const HOST_MARK = { ssh: "⇅", container: "⬢", agent: "⧉" };

export function stateOf(w) {
  if (w.error) return "failed";
  if (w.pending) return "pending";
  const st = w.status;
  const terms = st.terminals.map((t) => t.state);
  let best = terms.length ? "exited" : "idle";
  for (const s of terms) if (RANK.indexOf(s) < RANK.indexOf(best)) best = s;
  // A bell, or anything else that asked for the user, reads as waiting.
  if (st.attention && best !== "done") return "waiting";
  return best;
}

// Its name: the user's, else its agent's terminal title, else its folder's.
export function displayName(w) {
  if (w.label) return w.label;
  const agent = w.status?.terminals.find((t) => t.agent && t.title);
  return agent ? agent.title : w.name;
}

const plural = (n, one) => `${n} ${one}${n === 1 ? "" : "s"}`;
const hostKind = (w) => (w.host.startsWith("ssh ") ? "ssh" : w.host.startsWith("container ") ? "container" : w.host === "agent" ? "agent" : "local");
const leaf = (path) => path.split("/").pop();
const parentOf = (path) => (path.includes("/") ? path.slice(0, path.lastIndexOf("/")) : "");

export class Orchestrator {
  constructor(app, workspaces, folders, machines) {
    this.app = app;
    this.list = workspaces;
    this.folders = folders;
    // The machines the user declared (this machine is always there).
    this.machines = machines;
    this.panel = null;
    this.filter = "";
    this.pending = []; // workspaces being made in the background: {key, spec, name, error}
    this.nextPending = 0;
    this.prefs = { dock: false, group: "folders", lens: "inbox", agent: "claude", auto: false, custom: "", recent: [], machine: "local" };
    this.formGo = null; // the open New Workspace form's launch
    this.boards = []; // the open Workboard's redraw: a list of at most one
  }

  // The workspace this page views (null: none).
  get current() { return this.app().workspace?.id ?? null; }

  // Commands, keys, and seeing attention when this page is looked at
  // (once, after `new`).
  listen() {
    try { Object.assign(this.prefs, JSON.parse(localStorage.getItem(STORE) || "{}") || {}); } catch {}
    const cmd = (id, title, run) => { this.app().commands.set(id, { title, run: asCommand(run), source: "core", hidden: false }); };
    cmd("orchestrator.dock", "Orchestrator: Toggle Dock", () => this.toggleDock());
    cmd("orchestrator.focus", "Orchestrator: Focus Dock", () => this.focusDock());
    cmd("orchestrator.switch", "Orchestrator: Switch Workspace…", () => this.quickSwitch());
    cmd("orchestrator.new", "Orchestrator: New Workspace…", () => this.newWorkspace());
    cmd("orchestrator.agent", "Orchestrator: Run Agent…", () => this.startAgent());
    cmd("orchestrator.close", "Orchestrator: Close Workspace", () => this.close(this.byId(this.current), false));
    cmd("orchestrator.rename", "Orchestrator: Rename Workspace…", () => this.rename(this.byId(this.current)));
    cmd("orchestrator.move", "Orchestrator: Move to Folder…", () => this.move(this.byId(this.current)));
    cmd("orchestrator.pin", "Orchestrator: Pin / Unpin Workspace", () => this.pin(this.byId(this.current)));
    cmd("orchestrator.newFolder", "Orchestrator: New Folder…", () => this.newFolder(""));
    cmd("orchestrator.import", "Orchestrator: Import Sessions…", () => this.importSessions());
    cmd("workboard.open", "Workboard: Open", () => this.workboard());
    cmd("workboard.next", "Workboard: Next Waiting Agent", () => this.jump());
    cmd("orchestrator.group", "Orchestrator: Group by Folders / by Date", () => this.setGroup(this.prefs.group === "folders" ? "date" : "folders"));
    cmd("orchestrator.jump", "Orchestrator: Jump to Attention", () => this.jump());
    cmd("orchestrator.jumpBack", "Orchestrator: Jump Back", () => this.jumpBack());
    this.app().bindKey("M-o", "orchestrator.focus");
    this.app().bindKey("C-M-o", "orchestrator.switch");
    // From an agent's terminal too.
    this.app().terminalKeys.add("M-o").add("C-M-o");
    $("#ws-name").addEventListener("click", () => this.toggleDock());
    window.addEventListener("focus", (_e) => { this.seeIfLooked(); });
    document.addEventListener("visibilitychange", (_e) => { this.seeIfLooked(); });
    if (this.prefs.dock || !this.current) this.openDock();
    this.renderBeacon();
    this.seeIfLooked();
  }

  byId(id) { return this.list.find((w) => w.id === id) || null; }

  savePrefs() {
    try { localStorage.setItem(STORE, JSON.stringify(this.prefs)); } catch {}
  }

  // ----------------------------------------------------------- events

  // The server's news of the workspaces (app.js passes each on).
  added(w) {
    if (!this.byId(w.id)) this.list.push(w);
    this.redraw();
  }
  changed(w) {
    const i = this.list.findIndex((x) => x.id === w.id);
    if (i >= 0) this.list[i] = w;
    else this.list.push(w);
    if (w.id === this.current) this.renderTitle();
    this.redraw();
  }
  removed(id) {
    this.list = this.list.filter((w) => w.id !== id);
    if (id === this.current) {
      // Nothing more can be done here: say so, and offer the others.
      this.app().workspace = null;
      this.app().setStatus("This workspace was closed");
      $("#ws-name").textContent = "Closed workspace";
      this.openDock();
    }
    this.redraw();
  }
  statusOf(id, status) {
    const w = this.byId(id);
    if (!w) return;
    w.status = status;
    if (w.id === this.current) {
      this.app().term.states(status.terminals);
      this.renderTitle();
      this.seeIfLooked();
    }
    this.redraw();
  }
  foldersChanged(folders) {
    this.folders = folders;
    this.redraw();
  }
  redraw() {
    this.renderDock();
    this.renderBeacon();
    for (const redraw of this.boards) redraw();
  }

  looked() { return document.visibilityState === "visible" && document.hasFocus(); }

  // What this workspace needed, once this page is looked at: said in the
  // status bar, and cleared (finished work is seen, too).
  seeIfLooked() {
    const w = this.byId(this.current);
    if (!w?.status || !this.looked()) return;
    const done = w.status.terminals.some((t) => t.state === "done");
    if (!w.status.attention && !done) return;
    if (w.status.attention) this.app().setStatus(w.status.attention);
    rpc("workspace.seen", {}).catch(() => {});
  }

  renderTitle() {
    const w = this.byId(this.current);
    if (!w) return;
    document.title = `${displayName(w)} — Fresh`;
    $("#ws-name").textContent = `${displayName(w)} · ${w.host}${w.status.trusted ? "" : " · restricted"}`;
  }

  // ----------------------------------------------------------- moving

  // View another workspace: this page loads it (its own tabs and splits
  // come back with it).
  switchTo(id) {
    if (id === this.current) { this.app().focusPane(this.app().activePane); return; }
    const url = new URL(location.href);
    url.searchParams.set("ws", String(id));
    url.searchParams.delete("open");
    location.assign(url.href);
  }

  // The other workspaces that need you: those waiting first, then those
  // with finished work; each the one opened first first.
  needing() {
    const others = this.list.filter((w) => w.id !== this.current && w.status);
    return [...others.filter((w) => stateOf(w) === "waiting"), ...others.filter((w) => stateOf(w) === "done")];
  }

  jump() {
    const w = this.needing()[0];
    if (!w) { this.app().setStatus("Nothing needs you"); return; }
    try { sessionStorage.setItem(JUMPED, String(this.current)); } catch {}
    this.switchTo(w.id);
  }

  jumpBack() {
    let id = null;
    try { id = Number(sessionStorage.getItem(JUMPED)); } catch {}
    if (!id || !this.byId(id) || id === this.current) { this.app().setStatus("No workspace to jump back to"); return; }
    try { sessionStorage.setItem(JUMPED, String(this.current)); } catch {}
    this.switchTo(id);
  }

  renderBeacon() {
    const n = this.needing();
    const waiting = n.filter((w) => stateOf(w) === "waiting").length, done = n.length - waiting;
    const text = [waiting ? `● ${waiting} need${waiting === 1 ? "s" : ""} you` : "", done ? `✓ ${done} done` : ""].filter(Boolean).join("  ");
    this.app().statusItem("orchestrator").set(text, {
      title: n.map((w) => `${displayName(w)}: ${w.status.attention || "finished"}`).join("\n") + "\n(click: Jump to Attention)",
      onClick: () => this.jump(),
    });
  }

  quickSwitch() {
    this.app().prompt.open({
      title: "Switch workspace",
      items: this.list.filter((w) => w.status).map((w) => ({
        label: `${MARKS[stateOf(w)]} ${displayName(w)}${w.id === this.current ? " (this one)" : ""}`,
        detail: [w.folder, w.host, w.status.branch, w.status.attention].filter(Boolean).join(" · "),
        value: w.id,
      })),
      onAccept: (it) => this.switchTo(it.value),
    });
  }

  // ----------------------------------------------------------- the dock

  openDock() {
    this.panel = this.app().ui.panel({ id: "orchestrator", title: "Orchestrator", place: "left", onClose: () => this.dockClosed() });
    if (!this.prefs.dock) { this.prefs.dock = true; this.savePrefs(); }
    this.renderDock();
  }

  dockClosed() {
    this.panel = null;
    this.prefs.dock = false;
    this.savePrefs();
  }

  // Whether the dock is on screen.
  dockOpen() { const p = this.app().ui.shown("orchestrator"); return p !== undefined && p.open; }

  toggleDock() {
    if (this.panel?.open) this.panel.close();
    else { this.openDock(); this.panel.focus("dock"); }
  }

  // Alt+O: the keyboard to the dock (opening it), and back to the editor.
  focusDock() {
    if (this.panel?.open && this.panel.el.contains(document.activeElement)) { this.app().focusPane(this.app().activePane); return; }
    if (!this.panel?.open) this.openDock();
    this.panel.focus("dock");
  }

  setGroup(g) {
    this.prefs.group = g;
    this.savePrefs();
    this.renderDock();
  }

  // Whether a workspace answers the search.
  matches(w) {
    const q = this.filter.trim().toLowerCase();
    if (!q) return true;
    return [displayName(w), w.root, w.status?.branch || "", w.folder].some((x) => x.toLowerCase().includes(q));
  }

  // One workspace's row: its name, branch, host, and state at the edge.
  row(w) {
    const state = stateOf(w);
    const here = w.id === this.current;
    const kind = hostKind(w);
    const st = w.status;
    const title = [w.root, w.host, w.error || st?.attention || "", w.worktree ? `worktree of ${w.worktree.repo}` : ""].filter(Boolean).join("\n");
    return h("row", { class: "orch-row" + (here ? " current" : "") }, [
      h("text", { class: "orch-name", bold: here, title }, [displayName(w)]),
      st?.branch ? h("text", { dim: true, class: "orch-branch" }, [st.branch]) : null,
      st && st.modified ? h("text", { class: "orch-modified", title: "Unsaved files" }, [`●${st.modified}`]) : null,
      h("spacer", {}),
      st && !st.trusted ? h("text", { class: "orch-restricted", title: "Restricted: no processes, terminals or language servers" }, ["⊘"]) : null,
      HOST_MARK[kind] ? h("text", { dim: true, title: w.host }, [HOST_MARK[kind]]) : null,
      h("text", { class: `orch-mark ${state}`, title: state }, [MARKS[state]]),
    ]);
  }

  pendingRow(p) {
    const state = p.error ? "failed" : "pending";
    return h("row", { class: "orch-row" }, [
      h("text", { class: "orch-name keep", title: p.error || "" }, [p.name]),
      h("text", { dim: !p.error, class: p.error ? "orch-why" : "", title: p.error || "" }, [p.error ? p.error : "Creating…"]),
      h("spacer", {}),
      h("text", { class: `orch-mark ${state}` }, [MARKS[state]]),
    ]);
  }

  // A folder's row: its name, and how many in it need you.
  folderRow(label, items) {
    const waiting = items.filter((w) => stateOf(w) === "waiting").length;
    const done = items.filter((w) => stateOf(w) === "done").length;
    return h("row", { class: "orch-row orch-folder" }, [
      h("text", { bold: true }, [label]),
      h("spacer", {}),
      waiting ? h("text", { class: "orch-mark waiting" }, [`● ${waiting}`]) : null,
      done ? h("text", { class: "orch-mark done" }, [`✓ ${done}`]) : null,
    ]);
  }

  wsNode(w) { return { id: `w${w.id}`, label: this.row(w), data: { kind: "ws", w } }; }

  // The dock's tree: pinned workspaces, then folders (or days), then the
  // rest; workspaces being made last.
  nodes() {
    const shown = this.list.filter((w) => this.matches(w));
    const pinned = shown.filter((w) => w.pinned);
    const rest = shown.filter((w) => !w.pinned);
    const out = [];
    if (pinned.length) out.push({ id: "g:pinned", label: this.folderRow("Pinned", pinned), children: pinned.map((w) => this.wsNode(w)), open: true, data: { kind: "group" } });
    if (this.prefs.group === "date") {
      const day = new Date();
      day.setHours(0, 0, 0, 0);
      const week = new Date(day);
      week.setDate(day.getDate() - ((day.getDay() + 6) % 7));
      const when = (w) => w.status?.worked || w.worked || w.created || 0;
      const groups = [
        ["Today", rest.filter((w) => when(w) >= day.getTime())],
        ["This week", rest.filter((w) => when(w) < day.getTime() && when(w) >= week.getTime())],
        ["Older", rest.filter((w) => when(w) < week.getTime())],
      ];
      for (const [label, items] of groups) {
        if (!items.length) continue;
        items.sort((a, b) => when(b) - when(a));
        out.push({ id: `g:${label}`, label: this.folderRow(label, items), children: items.map((w) => this.wsNode(w)), open: true, data: { kind: "group" } });
      }
    } else {
      const searching = !!this.filter.trim();
      const folder = (path) => {
        const subs = this.folders.filter((f) => parentOf(f) === path).map(folder).filter(Boolean);
        const own = rest.filter((w) => w.folder === path).map((w) => this.wsNode(w));
        const all = rest.filter((w) => w.folder === path || w.folder.startsWith(`${path}/`));
        if (searching && !all.length) return null;
        return { id: `f:${path}`, label: this.folderRow(leaf(path), all), children: [...subs, ...own], open: true, data: { kind: "folder", path } };
      };
      for (const f of this.folders.filter((x) => !x.includes("/"))) {
        const n = folder(f);
        if (n) out.push(n);
      }
      for (const w of rest.filter((x) => !x.folder)) out.push(this.wsNode(w));
    }
    for (const p of this.pending.filter((x) => !this.filter.trim() || x.name.toLowerCase().includes(this.filter.trim().toLowerCase()))) out.push({ id: `p${p.key}`, label: this.pendingRow(p), data: { kind: "pending", p } });
    return out;
  }

  renderDock() {
    if (!this.panel?.open) return;
    const others = this.needing();
    const waiting = others.filter((w) => stateOf(w) === "waiting").length, done = others.length - waiting;
    const total = this.list.length, shown = this.list.filter((w) => this.matches(w)).length;
    this.panel.render(h("col", { class: "orch-dock" }, [
      h("row", { class: "orch-dock-head" }, [
        h("button", { primary: true, title: "New Workspace…", onClick: () => this.newWorkspace() }, ["+ New"]),
        h("spacer", {}),
        h("button", { title: "View, group, folders", onClick: () => this.dockMenu() }, ["Menu ▾"]),
      ]),
      h("row", {}, [h("input", {
        key: "q", value: this.filter, placeholder: "/ search workspaces", list: "dock",
        onInput: (v) => { this.filter = v; this.renderDock(); },
        onKey: (e) => { if (e.key === "Escape" && this.filter) { this.filter = ""; this.renderDock(); this.panel.focus("dock"); return true; } return false; },
        onEnter: () => this.panel.focus("dock"),
      })]),
      waiting || done
        ? h("row", { class: "orch-attention-line" }, [
          waiting ? h("text", { class: "orch-mark waiting" }, [`● ${waiting} need${waiting === 1 ? "s" : ""} you`]) : null,
          done ? h("text", { class: "orch-mark done" }, [`✓ ${done} done`]) : null,
        ])
        : null,
      this.filter.trim() ? h("text", { dim: true }, [`${shown} of ${total}`]) : null,
      h("tree", {
        key: "dock",
        nodes: this.nodes(),
        onActivate: (d) => this.activate(d),
        onContext: (d, _nd, at) => this.ui().menu(this.rowMenu(d), at),
        onKey: (e, d) => {
          if (e.key === "/") { e.preventDefault(); this.panel.focus("q"); }
          else if (d && (e.key === "F2" || e.key === "ContextMenu")) { e.preventDefault(); this.ui().menu(this.rowMenu(d), this.menuAt()); }
          else if (d?.kind === "ws" && e.key === "Delete") { e.preventDefault(); this.close(d.w, false); }
        },
      }),
    ]));
  }

  ui() { return this.app().ui; }

  // Where a menu opened from the keyboard goes: by the dock's head.
  menuAt() {
    const r = this.panel.el.getBoundingClientRect();
    return { x: r.left + 12, y: r.top + 56 };
  }

  activate(d) {
    if (d.kind === "ws") { if (d.w.error) this.retry(d.w); else this.switchTo(d.w.id); }
    else if (d.kind === "pending" && d.p.error) this.launch(d.p.spec, true, d.p);
  }

  rowMenu(d) {
    if (d.kind === "pending") {
      return d.p.error ? [{ label: "Retry", run: () => this.launch(d.p.spec, true, d.p) }, { label: "Dismiss", run: () => this.dropPending(d.p) }] : [{ label: "Creating…", run: () => {}, disabled: true }];
    }
    if (d.kind === "folder") {
      return [
        { label: "New Subfolder…", run: () => this.newFolder(d.path) },
        { label: "Rename…", run: () => this.renameFolder(d.path) },
        { label: "Delete Folder", run: () => this.deleteFolder(d.path) },
      ];
    }
    if (d.kind !== "ws") return [{ label: "New Folder…", run: () => this.newFolder("") }];
    const w = d.w;
    if (w.error) {
      return [{ label: "Retry", run: () => this.retry(w) }, { label: "Rename…", run: () => this.rename(w) }, { label: "Move to Folder…", run: () => this.move(w) }, null, { label: "Dismiss", run: () => this.dismiss(w) }];
    }
    const here = w.id === this.current;
    return [
      { label: here ? "This workspace" : "Switch to It", run: () => this.switchTo(w.id), disabled: here },
      { label: "Rename…", run: () => this.rename(w) },
      { label: "Move to Folder…", run: () => this.move(w) },
      { label: w.pinned ? "Unpin" : "Pin to Top", run: () => this.pin(w) },
      here ? { label: "Run Agent…", run: () => this.startAgent(), disabled: !w.status.trusted } : false,
      null,
      { label: "Close Workspace…", run: () => this.close(w, false) },
      w.worktree ? { label: "Close and Delete Worktree…", run: () => this.close(w, true) } : false,
    ].filter((x) => x !== false); // false: not offered here; null: a separator
  }

  dockMenu() {
    const g = this.prefs.group;
    this.ui().menu([
      { label: "Group: Folders", checked: g === "folders", run: () => this.setGroup("folders") },
      { label: "Group: By Date", checked: g === "date", run: () => this.setGroup("date") },
      null,
      { label: "New Workspace…", run: () => this.newWorkspace() },
      { label: "New Folder…", run: () => this.newFolder("") },
      { label: "Import Sessions…", run: () => this.importSessions() },
      { label: "Workboard", run: () => this.workboard() },
      null,
      { label: "Jump to Attention", run: () => this.jump() },
      { label: "Jump Back", run: () => this.jumpBack() },
    ], this.menuAt());
  }

  // ----------------------------------------------------------- acting

  async set(w, change) {
    try { await rpc("workspace.set", { workspace: w.id, ...change }); } catch (e) { this.app().setStatus(`Not changed: ${e.detail || e}`); }
  }

  async rename(w) {
    if (!w) return;
    const name = await this.app().prompt.ask({ title: "Workspace name", value: displayName(w), hint: "Empty: named after its agent's terminal, or its folder" });
    if (name !== null) await this.set(w, { name });
  }

  move(w) {
    if (!w) return;
    const items = [{ label: "Top level", value: "" }, ...this.folders.map((f) => ({ label: f, value: f })), { label: "New folder…", value: null }];
    this.app().prompt.open({
      title: `Move ${displayName(w)} to`,
      items,
      onAccept: async (it) => {
        let folder = it.value;
        if (folder === null) folder = await this.newFolder("");
        if (folder !== null) await this.set(w, { folder });
      },
    });
  }

  pin(w) { if (w) this.set(w, { pinned: !w.pinned }); }

  // A new folder under `parent`; resolves with its path (null: cancelled).
  async newFolder(parent) {
    const name = await this.app().prompt.ask({ title: parent ? `New folder in ${parent}` : "New folder", value: "" });
    if (!name || !name.trim()) return null;
    const path = parent ? `${parent}/${name.trim()}` : name.trim();
    try { await rpc("folder.create", { path }); return path; } catch (e) { this.app().setStatus(`No folder: ${e.detail || e}`); return null; }
  }

  async renameFolder(path) {
    const name = await this.app().prompt.ask({ title: `Rename folder ${path}`, value: leaf(path) });
    if (!name || !name.trim()) return;
    const to = parentOf(path) ? `${parentOf(path)}/${name.trim()}` : name.trim();
    try { await rpc("folder.rename", { from: path, to }); } catch (e) { this.app().setStatus(`Not renamed: ${e.detail || e}`); }
  }

  async deleteFolder(path) {
    if (!(await this.app().confirm(`Delete folder ${path}? What is in it moves up a level.`, "Delete Folder"))) return;
    try { await rpc("folder.delete", { path }); } catch (e) { this.app().setStatus(`Not deleted: ${e.detail || e}`); }
  }

  async retry(w) {
    this.app().setStatus(`Opening ${displayName(w)}…`);
    try { await rpc("workspace.retry", { workspace: w.id }); this.app().setStatus(`Opened ${displayName(w)}`); } catch (e) { this.app().setStatus(`Not opened: ${e.detail || e}`); }
  }

  async dismiss(w) {
    if (!(await this.app().confirm(`Forget ${displayName(w)}? Its folder stays where it is.`, "Dismiss"))) return;
    await rpc("workspace.close", { workspace: w.id }).catch((e) => this.app().setStatus(String(e.detail || e)));
  }

  // Close a workspace: its terminals and agents stop. Unsaved documents
  // are asked about; with `worktree`, its git worktree goes too (its
  // branch stays).
  async close(w, worktree) {
    if (!w) return;
    if (w.error) { await this.dismiss(w); return; }
    const running = w.status.terminals.filter((t) => t.state !== "exited").length;
    const what = running ? ` Its ${plural(running, "terminal")} stop${running === 1 ? "s" : ""}.` : "";
    const wt = worktree ? ` Its worktree ${w.root} is deleted, with any uncommitted changes; the branch ${w.worktree.branch} stays.` : "";
    if (!(await this.app().confirm(`Close ${displayName(w)}?${what}${wt}`, worktree ? "Close and Delete Worktree" : "Close"))) return;
    const args = { workspace: w.id, remove_worktree: worktree };
    try {
      await rpc("workspace.close", args);
    } catch (e) {
      if (e.code !== "unsaved") { this.app().setStatus(`Not closed: ${e.detail}`); return; }
      if (!(await this.app().confirm(`${e.detail}. Close it and discard them?`, "Discard and Close"))) return;
      await rpc("workspace.close", { ...args, discard: true }).catch((e2) => this.app().setStatus(`Not closed: ${e2.detail}`));
    }
    this.app().setStatus(`Closed ${displayName(w)}`);
  }

  // The command line for an agent choice: the registry's agent with its
  // auto mode and first prompt, a custom command, or none (a terminal).
  commandFor(choice, auto, prompt, custom) {
    if (choice === "custom") return custom.trim() || null;
    const agent = agentById(choice);
    return agent ? agentCommand(agent, auto, prompt) : null;
  }

  // The command line that takes that agent up again after the server
  // restarts: the registry's agent continues its latest conversation; a
  // command of one's own is run again only when it says (an imported
  // session's resume).
  resumeFor(choice, auto, resume) {
    const agent = agentById(choice);
    if (agent) return agentResume(agent, auto);
    return choice === "custom" && resume ? resume : null;
  }

  // Run Agent here: pick the agent, give it a task; a terminal running it.
  startAgent() {
    if (!this.app().workspace) return;
    if (!this.app().trusted) { this.app().setStatus("Trust the workspace to run an agent (Workspace: Trust)"); return; }
    this.app().prompt.open({
      title: "Run agent",
      items: AGENT_CHOICES.filter((c) => c.value !== "terminal").map((c) => ({ label: c.label, value: c.value })),
      onAccept: async (it) => {
        const agent = agentById(it.value);
        const custom = it.value === "custom" ? await this.app().prompt.ask({ title: "Command", value: this.prefs.custom, hint: "Runs in a terminal in the workspace's root, in your login shell" }) : "";
        if (custom === null) return;
        const prompt = agent ? await this.app().prompt.ask({ title: `${agent.label}: the task (optional)`, value: "" }) : "";
        if (prompt === null) return;
        const command = this.commandFor(it.value, this.prefs.auto, prompt, custom);
        if (!command) return;
        if (it.value === "custom") this.prefs.custom = custom;
        this.prefs.agent = it.value;
        this.savePrefs();
        const term = this.app().term;
        const id = await term.create(null, command, this.resumeFor(it.value, this.prefs.auto, ""));
        if (!term.visible) await term.toggle();
        term.select(id);
      },
    });
  }

  // ----------------------------------------------------------- workboard

  // The Workboard (docs/features/workboard.md): every workspace on one
  // board, by what it needs from you. Inbox: Needs you, Ready to review,
  // Working, Parked. Board: Backlog, Needs me, Working, Review. Tab or 1–2
  // switch the lens, Enter opens, n goes to the next one that needs you.
  workboard() {
    const panel = this.app().ui.panel({ id: "workboard", title: "Workboard", place: "dialog", onClose: null });
    // Where each workspace goes, from its state; idle with unsaved files
    // is work to review.
    const column = (w) => {
      const st = stateOf(w);
      if (st === "waiting" || st === "failed") return "needs";
      if (st === "done" || (st === "idle" && w.status?.modified)) return "review";
      if (st === "working" || st === "pending") return "working";
      return "parked";
    };
    const lenses = {
      inbox: [["needs", "Needs you"], ["review", "Ready to review"], ["working", "Working"], ["parked", "Parked"]],
      board: [["parked", "Backlog"], ["needs", "Needs me"], ["working", "Working"], ["review", "Review"]],
    };
    const SAYS = { waiting: "waiting for you", done: "finished, not looked at", working: "working", idle: "idle", unknown: "no signal", exited: "ended", pending: "being made" };
    const why = (w) => w.error || w.status?.attention || SAYS[stateOf(w)];
    const lensKey = (e) => {
      if (e.key === "Tab" || e.key === "1" || e.key === "2") {
        e.preventDefault();
        this.prefs.lens = e.key === "1" ? "inbox" : e.key === "2" ? "board" : this.prefs.lens === "board" ? "inbox" : "board";
        this.savePrefs();
        render();
        panel.focus(this.prefs.lens === "board" ? "" : "cards");
        return true;
      }
      if (e.key === "n") { e.preventDefault(); panel.close(); this.jump(); return true; }
      return false;
    };
    // A workspace on the board: its state, name, and what it needs.
    const card = (w) => h("row", { class: "orch-row" }, [
      h("text", { class: `orch-mark ${stateOf(w)}` }, [MARKS[stateOf(w)]]),
      h("text", { class: "orch-name keep", bold: w.id === this.current, title: [w.root, w.folder, w.status?.branch].filter(Boolean).join(" · ") }, [displayName(w)]),
      h("text", { class: "wb-why" }, [why(w)]),
    ]);
    const render = () => {
      const lens = this.prefs.lens === "board" ? "board" : "inbox";
      const ws = this.list;
      const tabs = h("row", { class: "wb-lenses" }, [
        h("toggle", { on: lens === "inbox", onChange: () => { this.prefs.lens = "inbox"; this.savePrefs(); render(); } }, ["1 Inbox"]),
        h("toggle", { on: lens === "board", onChange: () => { this.prefs.lens = "board"; this.savePrefs(); render(); } }, ["2 Board"]),
        h("spacer", {}),
        h("text", { dim: true }, ["Tab: lens · Enter: open · n: next that needs you · Esc: close"]),
        h("button", { onClick: () => panel.close() }, ["Close"]),
      ]);
      if (lens === "inbox") {
        const items = [];
        for (const [key, label] of lenses.inbox) {
          const group = ws.filter((w) => column(w) === key);
          if (!group.length) continue;
          items.push({ header: true, label: `${label}  ${group.length}` });
          for (const w of group) items.push({ w });
        }
        panel.render(h("col", { class: "wb" }, [tabs, h("list", {
          key: "cards", class: "wb-list", items,
          render: (it) => (it.header ? h("text", { bold: true }, [it.label]) : h("row", { class: "orch-row" }, [
            card(it.w),
            h("text", { dim: true }, [[it.w.folder, it.w.status?.branch].filter(Boolean).join(" · ")]),
          ])),
          onActivate: (it) => it && !it.header && this.switchTo(it.w.id),
        })]));
      } else {
        panel.render(h("col", { class: "wb" }, [tabs, h("row", { class: "wb-board" }, lenses.board.map(([key, label]) => {
          const group = ws.filter((w) => column(w) === key);
          return h("col", { class: "wb-column" }, [
            h("text", { bold: true }, [`${label}  ${group.length}`]),
            h("list", { key: `column-${key}`, items: group, render: card, onActivate: (w) => w && this.switchTo(w.id) }),
          ]);
        }))]));
      }
    };
    if (!WIRED.has(panel)) {
      WIRED.add(panel);
      panel.el.addEventListener("keydown", (e) => { if (e.target.tagName !== "INPUT") lensKey(e); });
    }
    this.boards = [() => { if (panel.open) render(); }];
    render();
    panel.focus(this.prefs.lens === "board" ? "" : "cards");
  }

  // ----------------------------------------------------------- machines

  // The machine choices: this machine, the declared ones, the hosts in
  // ~/.ssh/config (they need no declaring) and (`withAdd`) "Add…", which
  // declares another.
  machineOptions(withAdd) {
    return [
      { value: "local", label: "This machine" },
      ...this.machines.map((m) => ({ value: `m:${m.name}`, label: m.ssh_config ? `${m.name} (~/.ssh/config)` : m.name === hostText(m.host) ? m.name : `${m.name} (${hostText(m.host)})` })),
      ...(withAdd ? [{ value: "add", label: "Add…" }] : []),
    ];
  }
  // A remembered choice, while its machine is still declared.
  machineChoice(choice) {
    return choice && this.machines.some((m) => `m:${m.name}` === choice) ? choice : "local";
  }
  // The host a choice names.
  hostOf(choice) {
    const m = this.machines.find((x) => `m:${x.name}` === choice);
    return m ? m.host : { kind: "local" };
  }

  // ----------------------------------------------------------- import

  // Agent sessions run outside the editor (Claude Code and Codex
  // transcripts on a host): pick one, and New Workspace opens filled in to
  // resume it in its folder.
  importSessions() {
    const st = { machine: this.machineChoice(this.prefs.machine), sessions: null, error: "", busy: false };
    const panel = this.app().ui.panel({ id: "orchestrator-import", title: "Import Sessions", place: "dialog", onClose: null });
    const scan = async () => {
      const host = this.hostOf(st.machine);
      st.busy = true;
      st.error = "";
      render();
      try { st.sessions = (await rpc("agents.scan", { host })).sessions; } catch (e) { st.error = e.detail || String(e); }
      st.busy = false;
      render();
      panel.focus("sessions");
    };
    const open = (x) => {
      if (!x) return;
      panel.close();
      this.newWorkspace({
        root: x.cwd, machine: st.machine, git: "folder", agent: "terminal", prompt: x.resume, resume: x.resume,
        name: x.title ? x.title.slice(0, 40) : "",
      });
    };
    const row = (x) => {
      const open = this.list.find((w) => w.root === x.cwd);
      return h("row", { class: "orch-row" }, [
        h("text", { class: "orch-agent-name" }, [x.agent]),
        h("text", { class: "orch-name", title: x.title || "" }, [x.title || x.id]),
        h("spacer", {}),
        open ? h("text", { dim: true }, [`open as ${displayName(open)}`]) : null,
        h("text", { dim: true, class: "orch-branch", title: x.cwd }, [x.cwd]),
        h("text", { dim: true }, [ago(x.mtime)]),
      ]);
    };
    const render = () => {
      const list = st.sessions || [];
      panel.render(h("col", { class: "orch-import" }, [
        h("row", {}, [
          h("text", { class: "orch-label" }, ["Machine"]),
          h("select", { key: "machine", value: st.machine, options: this.machineOptions(false), onChange: (v) => { st.machine = v; render(); } }),
          h("button", { primary: true, disabled: st.busy, onClick: scan }, [st.busy ? "Scanning…" : "Scan"]),
          h("spacer", {}),
          h("button", { onClick: () => panel.close() }, ["Cancel"]),
        ]),
        st.error ? h("text", { class: "orch-error" }, [st.error]) : null,
        st.sessions
          ? h("text", { dim: true }, [list.length ? `${plural(list.length, "session")} of Claude Code and Codex, newest first · Enter opens New Workspace to resume it` : "No Claude Code or Codex sessions on this machine"])
          : h("text", { dim: true }, ["Finds Claude Code (~/.claude/projects) and Codex (~/.codex/sessions) sessions on the machine."]),
        h("list", { key: "sessions", class: "orch-import-list", items: list, render: row, onActivate: open }),
      ]));
    };
    render();
    scan();
  }

  // ----------------------------------------------------------- new workspaces

  dropPending(p) {
    this.pending = this.pending.filter((x) => x !== p);
    this.renderDock();
  }

  // Open a workspace from a New Workspace spec: switched to, or made in
  // the background — shown in the dock as it is made, and why it failed
  // (Retry sends it again).
  async launch(spec, background, again) {
    const name = spec.name || leaf(spec.root.replace(/\/+$/, "")) || spec.root;
    const p = again || { key: ++this.nextPending, spec, name, error: "" };
    if (background) {
      p.error = "";
      if (!this.pending.includes(p)) this.pending.push(p);
      this.renderDock();
    }
    try {
      const r = await rpc("workspace.open", spec);
      if (!this.byId(r.workspace.id)) this.list.push(r.workspace);
      this.dropPending(p);
      if (background) this.app().setStatus(`Opened ${displayName(r.workspace)} in the background`);
      else this.switchTo(r.workspace.id);
      return null;
    } catch (e) {
      p.error = e.detail || String(e);
      this.renderDock();
      return p.error;
    }
  }

  // The New Workspace dialog: a prompt for the agent, and under it the
  // agent, the machine and the folder (the ones used last; the folder at
  // first the current workspace's) and how git is used (a new worktree, offered when git says
  // the folder is in a work tree); Launch, or Launch in Background from
  // its ▾. The workspace is trusted and the agent runs in its auto mode.
  // `preset`: fields filled in (an imported session's folder and resume
  // command, and its name).
  newWorkspace(preset) {
    const f = {
      // A command of one's own is the terminal's (the prompt box is it).
      agent: this.prefs.agent === "custom" ? "terminal" : this.prefs.agent, prompt: "",
      machine: this.machineChoice(this.prefs.machine), root: this.prefs.recent[0] || this.app().root || "",
      git: this.prefs.git || "worktree", name: "",
      resume: "", // an imported session's: taken up again after a restart
      busy: false, error: "",
      ...(preset || {}),
    };
    // What the folder is (Where), and the question last asked about it.
    /** let where: Where */
    let where = ASKING;
    let whereTimer = 0, asked = "";
    // Declaring a machine (the choice "Add…"): its SSH details, and the
    // choice to go back to on Cancel.
    const add = { back: f.machine, name: "", target: "", port: "", identity: "", options: "", busy: false, error: "" };
    // The folder browser: the folders under `base` on the chosen machine,
    // loaded as they are opened; the text box filters them by name, or (a
    // path, absolute or from ~) moves `base` there.
    const browse = { shown: false, base: "", text: "", dirs: new Map(), open: new Set(), sel: "", error: "", timer: 0 };
    const panel = this.app().ui.panel({ id: "orchestrator-new", title: "New Workspace", place: "dialog", onClose: null });
    const worktree = () => f.git === "worktree" && where.kind === "repo";
    const go = async (background) => {
      if (f.busy || f.machine === "add" || !f.root.trim() || where.kind === "missing") return;
      const host = this.hostOf(f.machine);
      // The terminal runs the prompt box's command (none: a shell); an
      // agent gets the prompt.
      const command = f.prompt.trim();
      const agent = f.agent === "terminal" ? (command || null) : this.commandFor(f.agent, true, f.prompt, "");
      const spec = {
        root: f.root.trim(), host, trusted: true, agent,
        agent_resume: f.agent === "terminal" ? (f.resume && command === f.resume ? f.resume : null) : this.resumeFor(f.agent, true, ""),
        worktree: worktree() ? { branch: null, base: null } : null,
        name: f.name.trim() || null, folder: "",
      };
      const root = f.root.trim();
      Object.assign(this.prefs, {
        agent: f.agent, machine: f.machine, git: f.git,
        recent: [root, ...this.prefs.recent.filter((x) => x !== root)].slice(0, 8),
      });
      this.savePrefs();
      if (background) { panel.close(); this.launch(spec, true, null); return; }
      f.busy = true;
      f.error = "";
      render();
      const error = await this.launch(spec, false, null);
      if (error) { f.busy = false; f.error = error; render(); }
    };
    // Ask git (on the chosen machine) whether the folder is in a work tree,
    // a moment after the folder or the machine last changed.
    const checkGit = () => {
      clearTimeout(whereTimer);
      whereTimer = setTimeout(async () => {
        const path = f.root.trim();
        const key = `${f.machine}\n${path}`;
        asked = key;
        where = ASKING;
        render();
        /** let next: Where */
        let next = { kind: "missing" };
        if (path && f.machine !== "add") {
          try { const r = await rpc("machine.git", { host: this.hostOf(f.machine), path }); next = whereOf({ repo: r.repo, folder: r.folder, toplevel: r.toplevel || "" }); } catch {}
        }
        if (asked !== key) return;
        where = next;
        render();
      }, 250);
    };
    const addMachine = async () => {
      const target = add.target.trim();
      if (!target) { add.error = "A host is needed"; render(); return; }
      const host = {
        kind: "ssh",
        target: add.port.trim() ? `${target}:${add.port.trim()}` : target,
        identity: add.identity.trim() || null,
        options: add.options.trim() ? add.options.trim().split(/\s+/) : [],
      };
      add.busy = true;
      add.error = "";
      render();
      try {
        // Unnamed, the machine is called by its host.
        const name = add.name.trim() || host.target;
        this.machines = (await rpc("machine.add", { name, host })).machines;
        f.machine = `m:${name}`;
        Object.assign(add, { name: "", target: "", port: "", identity: "", options: "" });
        checkGit();
      } catch (e) { add.error = e.detail || String(e); }
      add.busy = false;
      render();
    };
    // Load the folders in `path` on the chosen machine; with `asBase`, the
    // browser starts there.
    const load = async (path, asBase) => {
      try {
        const r = await rpc("machine.browse", { host: this.hostOf(f.machine), path });
        browse.dirs.set(r.path, r.dirs);
        if (asBase) { browse.base = r.path; browse.sel = r.path; }
        browse.error = "";
      } catch (e) { browse.error = e.detail || String(e); }
      render();
    };
    const openBrowser = () => {
      Object.assign(browse, { shown: true, text: "", dirs: new Map(), open: new Set(), error: "" });
      render();
      panel.focus("browse-q");
      load(f.root.trim() || "~", true);
    };
    // The folders used last, in a menu under the ▾ beside the folder box.
    const recentMenu = (e) => {
      const items = this.prefs.recent.length
        ? this.prefs.recent.map((x) => action(x, null, () => { setRoot(x); panel.focus("root"); }))
        : [actionOf("No recent folders", null, null, true, () => {})];
      this.app().ui.dropdown(items, e.currentTarget);
    };
    // Launch in the background, from the ▾ beside Launch.
    const launchMenu = (e) => {
      this.app().ui.dropdown([action("Launch in Background", "Alt+Enter", () => { go(true); })], e.currentTarget);
    };
    // The browser's rows: `base`'s parent, then its folders as a tree.
    const browseRows = () => {
      const rows = [];
      const walk = (path, depth) => {
        for (const d of browse.dirs.get(path) || []) {
          rows.push({ name: d.name, path: d.path, depth, open: browse.open.has(d.path) });
          if (browse.open.has(d.path)) walk(d.path, depth + 1);
        }
      };
      walk(browse.base, 0);
      const q = browse.text.trim().toLowerCase();
      const isPath = q.startsWith("/") || q.startsWith("~");
      return q && !isPath ? rows.filter((r) => r.name.toLowerCase().includes(q)) : rows;
    };
    const toggle = (row) => {
      browse.sel = row.path;
      if (browse.open.has(row.path)) browse.open.delete(row.path);
      else { browse.open.add(row.path); if (!browse.dirs.has(row.path)) { load(row.path, false); return; } }
      render();
    };
    const parentOf = (path) => (path === "/" ? "/" : path.slice(0, path.lastIndexOf("/")) || "/");
    const setRoot = (path) => { f.root = path; render(); checkGit(); };
    const choose = () => { browse.shown = false; setRoot(browse.sel || browse.base); panel.focus("root"); };
    // Ctrl+Enter / Alt+Enter anywhere in the dialog: an input leaves them to
    // the dialog.
    const launchKey = (e) => e.key === "Enter" && (e.ctrlKey || e.altKey);
    this.formGo = go;
    if (!FORMS.has(panel)) {
      FORMS.add(panel);
      panel.el.addEventListener("keydown", (e) => { if (launchKey(e)) { e.preventDefault(); this.formGo(e.altKey); } });
    }
    const labelled = (label, control) => h("row", { class: "orch-opt" }, [h("text", { dim: true }, [label]), control]);
    const field = (label, controls) => h("row", { class: "orch-field" }, [h("text", { class: "orch-label" }, [label]), ...controls]);
    const input = (key, placeholder) => h("input", { key, value: f[key], placeholder, onInput: (v) => { f[key] = v; }, onEnter: () => go(false), onKey: launchKey });
    const addInput = (key, placeholder) => h("input", { key: `add-${key}`, value: add[key], placeholder, onInput: (v) => { add[key] = v; }, onEnter: addMachine });
    const machineChanged = (v) => {
      if (v === "add") add.back = f.machine;
      f.machine = v;
      browse.shown = false;
      render();
      if (v === "add") panel.focus("add-target");
      else checkGit();
    };
    const addSection = () => h("col", { class: "orch-add-machine" }, [
      field("SSH host", [addInput("target", "[user@]host, or a Host of your ~/.ssh/config"), addInput("port", "port (22)")]),
      field("Name", [addInput("name", "optional: the host")]),
      field("Identity", [addInput("identity", "identity file (optional)")]),
      field("Options", [addInput("options", "more ssh options, e.g. -J jump (optional)")]),
      add.error ? h("text", { class: "orch-error" }, [add.error]) : null,
      field("", [
        h("button", { primary: true, disabled: add.busy, onClick: addMachine }, [add.busy ? "Connecting…" : "Add"]),
        h("button", { disabled: add.busy, onClick: () => { f.machine = add.back; add.error = ""; render(); } }, ["Cancel"]),
      ]),
    ]);
    const browser = () => h("col", { class: "orch-browse" }, [
      h("input", {
        key: "browse-q", value: browse.text, placeholder: `filter, or a path (/… or ~/…) on ${hostText(this.hostOf(f.machine))}`,
        onInput: (v) => {
          browse.text = v;
          clearTimeout(browse.timer);
          const t = v.trim();
          if (t.startsWith("/") || t.startsWith("~")) browse.timer = setTimeout(() => { load(t, true); }, 300);
          else render();
        },
        onEnter: choose,
      }),
      h("text", { dim: true }, [browse.error || browse.base]),
      h("list", {
        key: "browse-tree", class: "orch-browse-list",
        items: [{ name: "..", path: parentOf(browse.base), depth: 0, open: false, up: true }, ...browseRows()],
        render: (r) => h("text", { mono: true }, [r.up ? "↑ .." : `${"  ".repeat(r.depth)}${r.open ? "▾" : "▸"} ${r.name}`]),
        onSelect: (r) => { browse.sel = r.up ? browse.base : r.path; },
        onActivate: (r, _i, how) => {
          if (r.up) { load(r.path, true); return; }
          if (how === "double") { browse.sel = r.path; choose(); return; }
          toggle(r);
        },
        activateOnClick: true,
      }),
      h("row", {}, [
        h("text", { dim: true }, [`Selected: ${browse.sel || browse.base}`]),
        h("spacer", {}),
        h("button", { onClick: () => { browse.shown = false; render(); } }, ["Cancel"]),
        h("button", { primary: true, onClick: choose }, ["Choose"]),
      ]),
    ]);
    const render = () => {
      const agent = agentById(f.agent);
      const adding = f.machine === "add";
      const w = where;
      const missing = w.kind === "missing" && f.root.trim() !== "";
      panel.render(h("col", { class: "orch-form orch-prompt-form" }, [
        h("textarea", {
          key: "prompt", rows: 6, value: f.prompt,
          placeholder: agent ? `What should ${agent.label} do? (optional)` : "Command to run in the terminal… (none: a shell)",
          onInput: (v) => { f.prompt = v; },
        }),
        h("row", { class: "orch-opts" }, [
          labelled("Agent", h("select", { key: "agent", value: f.agent, options: AGENT_CHOICES.filter((c) => c.value !== "custom"), onChange: (v) => { f.agent = v; render(); } })),
          labelled("Machine", h("select", { key: "machine", value: f.machine, options: this.machineOptions(true), onChange: machineChanged })),
          adding ? null : h("checkbox", {
            key: "worktree", checked: worktree(), disabled: w.kind !== "repo",
            title: w.kind === "repo" ? `A new git worktree of ${w.toplevel}, on a new branch` : w.kind === "asking" ? "Asking git about the folder…" : w.kind === "missing" ? "No such folder" : "The folder is not in a git work tree",
            onChange: (v) => { f.git = v ? "worktree" : "folder"; render(); },
          }, ["New worktree"]),
        ]),
        adding ? addSection() : null,
        adding ? null : field("Folder", [
          h("row", { class: "orch-combo ui-joined" }, [
            h("input", { key: "root", value: f.root, placeholder: "/path/to/project (~ for home)", onInput: (v) => { f.root = v; checkGit(); }, onEnter: () => go(false), onKey: launchKey }),
            h("button", { field: true, title: "Recent folders", onClick: recentMenu }, ["▾"]),
          ]),
          h("button", { onClick: () => { if (browse.shown) { browse.shown = false; render(); } else openBrowser(); } }, ["Browse…"]),
        ]),
        missing ? field("", [h("text", { class: "orch-error" }, [`No folder ${f.root.trim()} on ${f.machine === "local" ? "this machine" : f.machine.slice(2)}`])]) : null,
        !adding && browse.shown ? browser() : null,
        f.error ? h("text", { class: "orch-error" }, [f.error]) : null,
        h("row", {}, [
          h("text", { dim: true }, [f.busy ? "Opening…" : "Ctrl+Enter launches · Alt+Enter in the background"]),
          h("spacer", {}),
          h("button", { onClick: () => panel.close() }, ["Cancel"]),
          h("row", { class: "orch-split-button ui-joined" }, [
            h("button", { primary: true, disabled: f.busy || adding || missing, onClick: () => go(false) }, ["Launch"]),
            h("button", { primary: true, disabled: f.busy || adding || missing, title: "More ways to launch", onClick: launchMenu }, ["▾"]),
          ]),
        ]),
      ]));
    };
    render();
    checkGit();
    panel.focus(f.machine === "add" ? "add-target" : "prompt");
  }
}

const ago = (ms) => {
  const m = Math.round((Date.now() - ms) / 60000);
  if (m < 60) return `${m}m ago`;
  if (m < 48 * 60) return `${Math.round(m / 60)}h ago`;
  return `${Math.round(m / 1440)}d ago`;
};

// The New Workspace panels whose keys are wired.
const FORMS = new WeakSet();
// The Workboard panels whose keys are wired.
const WIRED = new WeakSet();
