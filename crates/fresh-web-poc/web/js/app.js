// The client application shell: panes and tabs, the palette, the status
// bar, the keymap and command registry, terminals, and the glue between the
// server's events, the buffers and the plugins. Features with UI of their
// own (explorer, search, git, language-server panels, markdown, tours,
// keybindings, themes) are plugins in `web/plugins/`.

import { rpc, session, connectEvents } from "./rpc.js";
import { byteLen, shiftPos } from "./ot.js";
import { fileBuffer, virtualBuffer, byteToIdx } from "./buffer.js";
import { Pane, View } from "./view.js";
import { Editing } from "./editing.js";
import { TerminalPanel } from "./terminal.js";
import { UIKit } from "./ui.js";
import { Host } from "./host.js";
import { Palette } from "./palette.js";
import { Skins } from "./skins.js";
import { Orchestrator } from "./orchestrator.js";
import { THEME } from "./highlight.js";
import { sash, clamp } from "./sash.js";
import { Session } from "./session.js";
import { MenuBar } from "./menubar.js";
import { Macros } from "./macros.js";
import { PLUGINS } from "../plugins/index.js";

const $ = (s) => document.querySelector(s);


// The clipboard's keys are the browser's own (not in the keymap); menus
// show them.
const CLIPBOARD_KEYS = { "edit.cut": "Ctrl+X", "edit.copy": "Ctrl+C", "edit.paste": "Ctrl+V" };

const DEFAULT_SETTINGS = {
  auto_close: true, auto_save: false, auto_save_seconds: 30, highlight_current_line: true, highlight_occurrences: true,
  show_whitespace: false, show_indent_guides: true, preview_tabs: true, inlay_hints: true, highlight_brackets: true, rainbow_brackets: true, scroll_sync: false, rulers: [], page_width: 80, keymap: "default", theme: "dark", zoom: 1,
};

class App {
  constructor() {
    this.buffers = new Map();
    this.docs = new Map();
    this.commands = new Map(); // id -> {title, run, source, hidden}
    this.modes = new Map(); // buffer mode -> {key: fn}
    this.keys = {}; // key -> command, from plugins
    this.terminalKeys = new Set(["C-`"]); // keys a focused terminal leaves to the editor
    this.statusItems = new Map();
    this.procs = new Map();
    this.watches = new Map();
    this.hoverProviders = [];
    this.history = { back: [], fwd: [] };
    this.opening = Promise.resolve(); // the file being opened (openFile)
    this.scrollDriver = null; // the split the pointer scrolled last (syncScroll)
    this.scrollLinks = []; // [{a, b, aToB, bToA}]: buffers scrolled together
    this.killRing = [];
    this.metrics = { cw: 8, lh: 18 };
    this.status = "";
    this.statusSig = ""; // what the status bar shows, to write it only when it changes
    this.statusRuns = []; // its items' actions
    this.formats = null; // the server's languages, encodings and line endings
    this.maximized = null; // the split filling the editor area, if one does
    this.restoring = true; // putting the remembered layout back: nothing is remembered meanwhile
    this.log = [];
    let saved = {};
    try { saved = JSON.parse(localStorage.getItem("fresh-poc:settings") || "{}"); } catch {}
    this.settings = { ...DEFAULT_SETTINGS, ...saved };
    let userKeys = {};
    try { userKeys = JSON.parse(localStorage.getItem("fresh-poc:keys") || "{}"); } catch {}
    this.userKeys = userKeys;
    // What this browser remembers of the workspace (session.js).
    this.session = new Session();
  }

  async start() {
    // The workspace this page views: `?ws=<id>`, or the server's first.
    // None when it was closed (or none is open): the page is then the
    // orchestrator's dock alone.
    const params = new URLSearchParams(location.search);
    const hello = await rpc("hello", params.has("ws") ? { workspace: Number(params.get("ws")) } : {});
    session.client = hello.client;
    const ws = (this.workspace = hello.workspace);
    if (ws && params.get("ws") !== String(ws.id)) {
      params.set("ws", String(ws.id));
      history.replaceState(null, "", `?${params}`);
    }
    this.orchestrator = new Orchestrator(this, hello.workspaces, hello.folders);
    // The skin first: the editor's cell is measured in its fonts.
    this.skins = new Skins(this);
    await this.skins.load();
    document.body.style.setProperty("--editor-zoom", String(this.settings.zoom));
    this.measure();
    this.root = ws?.root ?? "";
    this.host = null;
    this.hostName = ws?.host ?? "";
    this.trusted = !!ws?.trusted;
    this.lspLanguages = new Set(ws?.lsp ?? []);
    this.lspStatus = {}; // {language: running}, as `lsp.status` events say
    document.title = ws ? `${ws.name} — Fresh` : "Workspaces — Fresh";
    $("#ws-name").textContent = ws ? `${ws.name} · ${ws.host}${ws.trusted ? "" : " · restricted"}` : "No workspace";
    $("#ws-name").title = "Workspaces (Alt+O)";
    connectEvents((ev) => this.onEvent(ev));
    if (ws) this.session.open(ws.host, ws.root);
    this.installSashes();
    this.editing = new Editing(this);
    this.ui = new UIKit(this);
    this.prompt = new Palette(this);
    this.prompt.listen();
    this.term = new TerminalPanel(this, $("#terminal"));
    this.term.listen();
    this.host = new Host(this);
    this.input = $("#input");
    this.input.addEventListener("keydown", (e) => this.onKey(e));
    // A key pressed with nothing focused (after a click on the status bar,
    // say) goes to the editor.
    window.addEventListener("keydown", (e) => {
      if (e.target !== document.body) return;
      this.focusPane(this.activePane);
      this.onKey(e);
    });
    this.input.addEventListener("compositionstart", () => { this.composing = true; });
    this.input.addEventListener("compositionend", () => { this.composing = false; this.onInput(); });
    this.input.addEventListener("input", () => { if (!this.composing) this.onInput(); });
    this.input.addEventListener("paste", (e) => { e.preventDefault(); this.paste(e.clipboardData.getData("text")); });
    this.input.addEventListener("copy", (e) => this.onCopy(e, false));
    this.input.addEventListener("cut", (e) => this.onCopy(e, true));
    const first = new Pane(this);
    first.listen();
    this.layoutTree = { pane: first };
    this.activePane = this.layoutTree.pane;
    this.registerCoreCommands();
    this.macros = new Macros(this);
    this.macros.register();
    this.menubar = new MenuBar(this);
    this.menubar.listen();
    this.orchestrator.listen();
    this.relayout();
    new ResizeObserver(() => this.renderAll()).observe($("#editor-area"));
    setInterval(() => this.autoSave(), 1000);
    if (!ws) return;
    await this.host.load(PLUGINS);
    await this.term.adopt(ws.terminals);
    // What was open in this workspace last time, unless a file is named.
    const open = params.get("open");
    if (!open) await this.restoreLayout();
    this.restoring = false;
    this.rememberLayout();
    window.addEventListener("pagehide", () => { this.rememberLayout(); this.session.flush(); });
    document.addEventListener("visibilitychange", () => { if (document.hidden) { this.rememberLayout(); this.session.flush(); } });
    this.host.emit("app.ready", {});
    if (open) await this.openFile(open);
    this.focusPane(this.activePane);
  }

  // ------------------------------------------------------------- the workspace's layout, remembered

  // What is open, for the next visit (session.js): the splits and their
  // ratios, each split's document tabs (a file by its path, an untitled
  // document by its number) with their cursor and scroll, the active split
  // and tab, and whether the sidebar and the terminal show.
  layoutSnapshot() {
    const tab = (v) => (this.tabKey(v.buf) ? { path: v.buf.path || null, untitled: v.buf.untitled, pos: v.primary.pos, top: v.top, preview: v.preview } : null);
    const node = (n) => (n.pane
      ? { tabs: n.pane.views.map(tab).filter(Boolean), active: n.pane.view ? this.tabKey(n.pane.view.buf) : null }
      : { dir: n.dir, ratio: n.ratio, a: node(n.a), b: node(n.b) });
    return { tree: node(this.layoutTree), active: this.panes().indexOf(this.activePane), sidebar: !$("#sidebar").classList.contains("hidden"), terminal: !!this.term?.visible };
  }
  rememberLayout() {
    if (this.restoring || !this.layoutTree) return;
    this.session.set("layout", this.layoutSnapshot());
  }
  // A document buffer as the remembered layout names it: its path, or
  // `untitled:N`; null for any other buffer.
  tabKey(b) { return b.path || (b.untitled ? `untitled:${b.untitled}` : null); }
  // Put the remembered layout back: the splits, then each split's
  // documents (one that cannot be opened any more is left out — an
  // untitled one is there only while the server keeps it for its unsaved
  // text — and a split left with none closes). Resolves false when there
  // was nothing to restore.
  async restoreLayout() {
    const s = this.session.get("layout");
    if (!s || !s.tree) return false;
    $("#sidebar").classList.toggle("hidden", s.sidebar === false);
    const specs = [];
    const build = (n) => {
      if (n.tabs) { const pane = new Pane(this); pane.listen(); specs.push({ pane, n }); return { pane }; }
      return { dir: n.dir === "v" ? "v" : "h", ratio: clamp(+n.ratio || 0.5, 0.05, 0.95), a: build(n.a), b: build(n.b) };
    };
    this.layoutTree = build(s.tree);
    this.activePane = specs[Math.max(0, Math.min(specs.length - 1, s.active | 0))].pane;
    this.relayout();
    for (const { pane, n } of specs) {
      for (const t of n.tabs) {
        let b;
        try { b = t.untitled ? await this.loadUntitled(t.untitled) : await this.loadFile(t.path); } catch { continue; }
        const v = pane.show(b, t.preview ? true : null);
        const pos = Math.max(0, Math.min(t.pos | 0, b.len));
        v.cursors = [{ pos, anchor: null, goalX: null }];
        v.setTop(Math.max(0, Math.min(t.top | 0, pos)));
      }
      const active = pane.views.find((v) => this.tabKey(v.buf) === n.active);
      if (active) pane.show(active.buf);
    }
    for (const { pane } of specs) if (!pane.views.length && this.panes().length > 1) await this.closePane(pane);
    if (s.terminal && !this.term.visible) await this.term.toggle();
    this.focusPane(this.activePane);
    this.renderAll();
    return true;
  }

  // Files opened lately in this workspace, the latest first.
  noteRecent(path) {
    const list = (this.session.get("recent") || []).filter((p) => p !== path);
    list.unshift(path);
    this.session.set("recent", list.slice(0, 30));
  }
  recentFiles() { return this.session.get("recent") || []; }

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
    this.term?.applyFont();
    if (!this.editing || (cw === this.metrics.cw && lh === this.metrics.lh)) return;
    for (const p of this.panes()) p.clearRows();
    this.renderAll();
  }

  report(msg) {
    this.log.push(msg);
    console.error(msg);
    this.setStatus(msg.split("\n")[0]);
  }

  // ------------------------------------------------------------- panes

  panes(node = this.layoutTree, out = []) {
    if (node.pane) out.push(node.pane);
    else { this.panes(node.a, out); this.panes(node.b, out); }
    return out;
  }
  relayout() {
    const area = $("#editor-area");
    area.innerHTML = "";
    const build = (n) => {
      if (n.pane) return n.pane.el;
      const d = document.createElement("div");
      d.className = `split ${n.dir}`;
      const a = build(n.a), b = build(n.b);
      // Grow factors in thousandths: they sum past one, so a child left
      // alone (its sibling hidden by a maximized split) fills the split.
      const flex = () => { a.style.flex = `${n.ratio * 1000} 1 0`; b.style.flex = `${(1 - n.ratio) * 1000} 1 0`; };
      flex();
      // Its sash moves the line between them; the panes redraw at their
      // new sizes (each observes its own).
      const s = document.createElement("div");
      const x = n.dir === "h";
      sash(s, x ? "x" : "y", {
        begin: () => { const r = d.getBoundingClientRect(); return { at: n.ratio, size: x ? r.width : r.height }; },
        drag: (c, delta) => {
          const min = Math.min(0.45, 60 / Math.max(1, c.size));
          n.ratio = clamp(c.at + delta / Math.max(1, c.size), min, 1 - min);
          flex();
        },
        end: () => this.rememberLayout(),
        reset: () => { n.ratio = 0.5; flex(); this.rememberLayout(); },
      });
      d.append(a, s, b);
      return d;
    };
    const el = build(this.layoutTree);
    el.style.flex = "1 1 0";
    area.appendChild(el);
    for (const p of this.panes()) p.el.classList.toggle("active", p === this.activePane);
    this.showMaximized();
    this.renderAll();
    this.rememberLayout();
  }
  // The sashes of the sidebar, the right dock, the bottom dock and the
  // terminal; their sizes are remembered for the workspace.
  installSashes() {
    const parts = {
      left: { el: $("#left-dock"), axis: "x", dir: 1, min: 160, most: 0.4 },
      sidebar: { el: $("#sidebar"), axis: "x", dir: 1, min: 120, most: 0.5 },
      right: { el: $("#right-dock"), axis: "x", dir: -1, min: 160, most: 0.6 },
      dock: { el: $("#dock"), axis: "y", dir: -1, min: 80, most: 0.8 },
      terminal: { el: $("#terminal"), axis: "y", dir: -1, min: 80, most: 0.8 },
    };
    const sizes = { ...(this.session.get("sizes") || {}) };
    const prop = (p) => (p.axis === "x" ? "width" : "height");
    for (const [name, p] of Object.entries(parts)) {
      if (sizes[name]) p.el.style[prop(p)] = `${sizes[name]}px`;
      sash(document.querySelector(`.sash[data-sash="${name}"]`), p.axis, {
        // The most a part takes is a share of its container.
        begin: () => {
          const r = p.el.getBoundingClientRect(), box = p.el.parentElement.getBoundingClientRect();
          return { at: p.axis === "x" ? r.width : r.height, max: (p.axis === "x" ? box.width : box.height) * p.most };
        },
        drag: (c, d) => { sizes[name] = Math.round(clamp(c.at + p.dir * d, p.min, Math.max(p.min, c.max))); p.el.style[prop(p)] = `${sizes[name]}px`; },
        end: () => this.session.set("sizes", sizes),
        reset: () => { delete sizes[name]; p.el.style[prop(p)] = ""; this.session.set("sizes", sizes); },
      });
    }
  }
  renderAll() { for (const p of this.panes()) { p.renderTabs(); p.scheduleRender(); } }
  renderBuffer(id) { for (const p of this.panes()) if (p.view?.buf.id === id) p.scheduleRender(); }
  view() { return this.activePane?.view || null; }
  focusPane(p) {
    if (!p) return;
    this.scrollDriver = null;
    const changed = this.activePane !== p;
    this.activePane = p;
    for (const x of this.panes()) x.el.classList.toggle("active", x === p);
    this.input.focus({ preventScroll: true });
    // A maximized split gives way to the one focused.
    if (this.maximized && this.maximized !== p) { this.maximized = p; this.showMaximized(); }
    if (changed) { this.renderAll(); if (p.view) this.host?.emit("buffer.activated", { buffer: p.view.buf, view: p.view }); }
  }
  split(dir, ratio = 0.5, buf = null) {
    const old = this.activePane;
    if (old.view) this.keep(old.view);
    const np = new Pane(this);
    np.listen();
    const replace = (n) => {
      if (n.pane === old) return { dir, ratio, a: { pane: old }, b: { pane: np } };
      if (n.pane) return n;
      return { ...n, a: replace(n.a), b: replace(n.b) };
    };
    this.layoutTree = replace(this.layoutTree);
    const b = buf || old.view?.buf;
    if (b) np.show(b);
    this.activePane = np;
    this.relayout();
    this.focusPane(np);
    return np;
  }
  // Close a split. Files whose last views are in it and have unsaved
  // changes are asked about first (see `mayClose`).
  // One split filling the editor area (the others hidden, not closed), or
  // none.
  showMaximized() {
    if (this.maximized && !this.panes().includes(this.maximized)) this.maximized = null;
    $("#editor-area").classList.toggle("maximized", !!this.maximized);
    for (const p of this.panes()) p.el.classList.toggle("max", p === this.maximized);
  }
  async closePane(p) {
    if (this.panes().length === 1) return;
    for (const b of new Set(p.views.map((v) => v.buf))) if (!(await this.mayClose(b, p.views))) return;
    const remove = (n) => {
      if (n.pane) return n;
      if (n.a.pane === p) return n.b;
      if (n.b.pane === p) return n.a;
      return { ...n, a: remove(n.a), b: remove(n.b) };
    };
    this.layoutTree = remove(this.layoutTree);
    for (const v of [...p.views]) { v.dispose(); this.dropIfUnshown(v.buf); }
    this.activePane = this.panes()[0];
    this.relayout();
    this.focusPane(this.activePane);
  }
  // Close a tab; resolves false when the user cancelled.
  async closeTab(p, v) {
    if (!(await this.mayClose(v.buf, [v]))) return false;
    if (!p.views.includes(v)) return true; // closed meanwhile
    p.closeView(v);
    this.dropIfUnshown(v.buf);
    if (!p.views.length && this.panes().length > 1) await this.closePane(p);
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
    if (this.panes().some((p) => p.views.some((v) => v.buf === b && !views.includes(v)))) return true;
    const pick = await this.choose(`${b.name} has unsaved changes`, [
      { label: "Save", value: "save" },
      { label: "Discard", value: "discard" },
      { label: "Cancel", value: null },
    ]);
    if (pick === "save") { await this.saveBuffer(b); return !b.modified; }
    if (pick === "discard") { b.discard = true; return true; }
    return false;
  }
  // Let go of a buffer no view shows. A file's document is closed; with
  // `discard` its unsaved changes go too, unless another client holds it.
  dropIfUnshown(b) {
    if (this.panes().some((x) => x.views.some((y) => y.buf === b))) return;
    if (b.keep) return;
    this.buffers.delete(b.id);
    if (b.doc) {
      this.docs.delete(b.doc);
      const discard = !!b.discard;
      rpc("doc.close", { doc: b.doc, discard }).then((r) => {
        if (!discard) return;
        if (r.dropped) this.setStatus(`Discarded the changes to ${b.name}`);
        else if (r.others) this.setStatus(`${b.name} is open in ${r.others} other window${r.others === 1 ? "" : "s"}; its unsaved changes stay there`);
      }).catch((e) => this.report(String(e.message || e)));
    }
    this.host.emit("buffer.closed", { buffer: b });
  }
  async closeBuffer(b) {
    for (const p of this.panes()) for (const v of [...p.views]) if (v.buf === b && !(await this.closeTab(p, v))) return false;
    if (!(await this.mayClose(b, []))) return false;
    this.dropIfUnshown(b);
    return true;
  }
  onViewShown(pane, v) {
    this.rememberLayout();
    if (pane === this.activePane) this.host?.emit("buffer.activated", { buffer: v.buf, view: v });
    this.updateStatus();
  }
  // Show a buffer: in the active pane ("tab"), a new split below ("split")
  // or to the right ("right").
  // `preview` and `focus` as for `openFile`; preview tabs are a setting.
  showBuffer(b, place = "tab", { preview = null, focus = true } = {}) {
    if (place === "split" || place === "right") {
      const existing = this.panes().find((p) => p.view?.buf === b);
      if (existing) { this.focusPane(existing); return existing.view; }
      return this.split(place === "split" ? "v" : "h", 0.6, b).view;
    }
    const v = this.activePane.show(b, preview && !this.settings.preview_tabs ? false : preview);
    if (focus) this.focusPane(this.activePane);
    return v;
  }
  // A preview tab made a tab of its own (it was edited, clicked, split or
  // kept open).
  keep(v) {
    if (!v.preview) return;
    v.preview = false;
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
    if (pane !== (this.scrollDriver || this.activePane) || v.mode !== "source") return;
    const L = v.buf.text;
    const line = L ? L.indexOf(v.top) : -1;
    if (line < 0) return;
    for (const p of this.panes()) {
      const w = p.view;
      if (p === pane || !w || w.mode !== "source" || !w.buf.text) continue;
      let top = null;
      if (w.buf === v.buf) { if (this.settings.scroll_sync) top = v.top; }
      else {
        const link = this.scrollLinks.find((k) => (k.a === v.buf && k.b === w.buf) || (k.b === v.buf && k.a === w.buf));
        if (link && v.buf.full && w.buf.full) {
          const M = w.buf.text;
          const to = link.a === v.buf ? link.aToB(line) : link.bToA(line);
          top = M.starts[Math.max(0, Math.min(M.count - 1, to))];
        }
      }
      if (top != null && top !== w.top) { w.setTop(top); p.scheduleRender(); }
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
    pane.show(v.buf, false);
    this.focusPane(pane);
    const x0 = e.clientX, y0 = e.clientY;
    let dragging = false, target = null, marked = null;
    const unmark = () => { if (marked) marked.classList.remove("drop-before", "drop-after", "drop-into"); marked = null; };
    const move = (ev) => {
      if (!dragging && Math.hypot(ev.clientX - x0, ev.clientY - y0) < 5) return;
      if (!dragging) { dragging = true; document.documentElement.classList.add("tab-dragging"); }
      target = this.tabDropAt(ev.clientX, ev.clientY);
      unmark();
      if (target) { marked = target.el; marked.classList.add(`drop-${target.side}`); }
    };
    const up = () => {
      window.removeEventListener("mousemove", move);
      window.removeEventListener("mouseup", up);
      document.documentElement.classList.remove("tab-dragging");
      unmark();
      if (dragging && target) this.moveView(v, target.pane, target.index);
    };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
  }
  // Where a tab dropped at (x, y) goes: {pane, index, el, side}, or null.
  tabDropAt(x, y) {
    const el = document.elementFromPoint(x, y);
    const paneEl = el && el.closest(".pane");
    const pane = paneEl && this.panes().find((p) => p.el === paneEl);
    if (!pane) return null;
    const tab = el.closest(".tab");
    if (tab && pane.tabsEl.contains(tab)) {
      const r = tab.getBoundingClientRect();
      const after = x > r.left + r.width / 2;
      return { pane, index: +tab.dataset.i + (after ? 1 : 0), el: tab, side: after ? "after" : "before" };
    }
    return { pane, index: pane.views.length, el: pane.el, side: "into" };
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
    let nv = to.views.find((x) => x.buf === v.buf);
    if (!nv) {
      nv = new View(to, v.buf);
      v.buf.views.add(nv);
      nv.cursors = v.cursors.map((c) => ({ ...c }));
      nv.setTop(v.top, v.topPx);
      nv.mode = v.mode;
      nv.opts = { ...v.opts };
      nv.folds = v.folds.map((f) => ({ ...f }));
      to.views.splice(Math.min(index, to.views.length), 0, nv);
    }
    to.show(v.buf);
    from.closeView(v);
    if (!from.views.length && this.panes().length > 1) this.closePane(from);
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
    const rel = b.path && b.path.startsWith(this.root + "/") ? b.path.slice(this.root.length + 1) : b.path;
    const copy = (t) => navigator.clipboard?.writeText(t).then(() => this.setStatus(`Copied ${t}`), () => this.setStatus("The browser did not allow writing the clipboard"));
    this.ui.menu([
      ...(v.preview ? [{ label: "Keep Open", run: () => this.keep(v) }] : []),
      { label: "Close", keys: this.keyLabel("view.closeTab"), run: () => this.closeTab(pane, v) },
      { label: "Close Others", run: () => this.closeTabs(pane, (x) => x !== v) },
      { label: "Close to the Right", disabled: pane.views.indexOf(v) === pane.views.length - 1, run: () => this.closeTabs(pane, (x, i) => i > pane.views.indexOf(v)) },
      { label: "Close Saved", run: () => this.closeTabs(pane, (x) => !x.buf.modified) },
      { label: "Close All", run: () => this.closeTabs(pane, () => true) },
      null,
      { label: "Split Right", keys: this.keyLabel("view.splitRight"), run: () => { this.focusPane(pane); this.split("h"); } },
      { label: "Split Down", run: () => { this.focusPane(pane); this.split("v"); } },
      ...(b.path ? [
        null,
        { label: "Copy Path", run: () => copy(b.path) },
        { label: "Copy Relative Path", run: () => copy(rel) },
        { label: "Reveal in Explorer", run: () => this.run("explorer.reveal") },
      ] : []),
    ], at);
  }
  // The editor's menu: the clipboard, the language server's actions on
  // what is under the caret, and editing the selection.
  editorMenu(pane, at) {
    this.focusPane(pane);
    const b = pane.view.buf;
    const lsp = this.lspAvailable(b);
    this.ui.menu(this.menubar.resolve([
      "edit.cut", "edit.copy", "edit.paste",
      null,
      ...(lsp ? ["lsp.definition", "lsp.references", "lsp.rename", "lsp.codeActions", "lsp.hover", null, "lsp.format"] : []),
      "edit.toggleComment", "edit.duplicateLine", "edit.deleteLine",
      null,
      "select.all", "select.allOccurrences",
      ...(b.doc ? [null, "file.revert"] : []),
    ].map((id) => (id ? { command: id, label: this.commands.get(id)?.title.replace(/^(Edit|LSP|File|View): /, ""), keys: CLIPBOARD_KEYS[id] } : null))), at);
  }

  // ------------------------------------------------------------- buffers

  watchBuffer(b) {
    b.listeners.add((what, data) => {
      if (what === "edit") this.afterEdit(b, data);
      if (what === "text") { this.host.emit("buffer.changed", { buffer: b, ops: [], local: false }); this.renderBuffer(b.id); }
      if (what === "error") this.setStatus(String(data));
    });
  }

  // The buffer for a file, opening its document without showing it.
  async loadFile(path) {
    const abs = path.startsWith("/") ? path : `${this.root}/${path}`;
    let b = [...this.buffers.values()].find((x) => x.path === abs);
    if (b) return b;
    return this.adoptDoc(await rpc("doc.open", { path: abs }));
  }
  // The buffer for an untitled document the server keeps (the page was
  // reloaded with its unsaved text).
  async loadUntitled(n) {
    return [...this.buffers.values()].find((x) => x.untitled === n) || this.adoptDoc(await rpc("doc.open", { untitled: n }));
  }
  // A new untitled document, shown: an empty buffer bound to no file,
  // named Untitled-N, until it is saved as one.
  async newUntitled() {
    const b = await this.adoptDoc(await rpc("doc.new"));
    this.showBuffer(b);
    return b;
  }
  // The buffer of an opened document ({doc, path, ...} from the server).
  async adoptDoc(info) {
    let b = this.docs.get(info.doc);
    if (b) return b;
    b = fileBuffer(info);
    this.buffers.set(b.id, b);
    this.docs.set(b.doc, b);
    this.watchBuffer(b);
    await b.ensure(0);
    this.host.emit("buffer.opened", { buffer: b });
    return b;
  }

  // Show a file, at a line and (UTF-16) column when given. A navigation:
  // Go Back returns to where the active view was.
  // `preview`: in the split's preview tab (true), in a tab of its own
  // (false: a preview tab showing it is kept), or as it is shown already
  // (left out). `focus: false` leaves the keyboard where it is (a list
  // previewing what it selects). Files open in the order they were asked
  // for (a click's preview, then the double-click's keep).
  async openFile(path, { line, column, place, preview, focus } = {}) {
    const turn = this.opening;
    let done = () => {};
    this.opening = new Promise((r) => { done = r; });
    try {
      await turn;
      const from = this.view();
      const b = await this.loadFile(path);
      if (from) this.pushHistory(from);
      const v = this.showBuffer(b, place, { preview, focus });
      this.noteRecent(b.path);
      if (line != null) await this.gotoLine(line - 1, column != null ? column - 1 : 0, { history: false });
      this.updateStatus();
      return v;
    } catch (e) {
      this.setStatus(String(e.message || e));
      return null;
    } finally {
      done();
    }
  }

  createVirtual(o) {
    const b = virtualBuffer(o.name, o.entries || [{ text: o.text || "" }], { mode: o.mode, readOnly: o.readOnly ?? true });
    b.language = o.language || null;
    b.keep = !!o.keep;
    this.buffers.set(b.id, b);
    this.watchBuffer(b);
    if (o.place) {
      const v = this.showBuffer(b, o.place);
      if (o.lineNumbers === false) v.opts.lineNumbers = false;
    }
    return b;
  }

  // `typing` groups a run of typed characters into one undo step (see
  // buffer.js).
  applyEdit(b, ops, typing = null) {
    if (b.readOnly) { this.setStatus("Read-only buffer"); return; }
    b.edit(ops, { typing });
  }

  afterEdit(b, data) {
    // Editing a preview tab's file keeps it.
    if (data.local) for (const v of b.views) this.keep(v);
    this.host.emit("buffer.changed", { buffer: b, ops: [data.op], local: data.local });
    this.renderBuffer(b.id);
    for (const p of this.panes()) if (p.views.some((v) => v.buf === b)) p.renderTabs();
  }

  // Save a file. The server refuses when the file changed on disk since it
  // was opened or last saved; an explicit save (`ask`) then offers to
  // overwrite it, auto-save leaves it and says so.
  async saveBuffer(b, { ask = true } = {}) {
    if (!b.doc) return;
    // An untitled document is saved as a file, asked for.
    if (!b.path) { await this.saveBufferAs(b); return; }
    try {
      // Keeping one's changes over the file's (`askChangedOnDisk`) was the
      // choice to overwrite it.
      await b.save({ force: !!b.keepMine });
      b.keepMine = false;
    } catch (e) {
      if (!ask || e.code !== "changed_on_disk") { this.setStatus(`Not saved: ${e.detail || e.message || e}`); return; }
      if (!(await this.confirm(`${b.name} changed on disk since it was opened. Overwrite it?`, "Overwrite"))) { this.setStatus(`Not saved: ${b.name}`); return; }
      await b.save({ force: true });
    }
    this.setStatus(`Saved ${b.name}`);
    this.renderAll();
    this.host.emit("buffer.saved", { buffer: b });
  }

  // Save As: a document is written to the file asked for and bound to it
  // (`doc.save_as`: an untitled one gets its file, a file's moves to the
  // new one and the old file stays as last saved); a file there is
  // overwritten once confirmed. A buffer with no document (a plugin's
  // panel) has its text written to the file, which then opens.
  async saveBufferAs(b) {
    const rel = (p) => (p && p.startsWith(this.root + "/") ? p.slice(this.root.length + 1) : p || "");
    const name = await this.prompt.ask({ title: "Save as", value: rel(b.path) || b.name });
    if (!name) return;
    const path = name.startsWith("/") ? name : `${this.root}/${name}`;
    if (path === b.path) { await this.saveBuffer(b); return; }
    const st = await rpc("fs.stat", { path });
    if (st.dir) { this.setStatus(`${name} is a folder`); return; }
    if (st.exists && !(await this.confirm(`${name} exists. Overwrite it?`, "Overwrite"))) return;
    try {
      if (b.doc) {
        while (!b.synced) await new Promise((r) => setTimeout(r, 20));
        const info = await rpc("doc.save_as", { doc: b.doc, path, overwrite: st.exists });
        b.setInfo(info);
        b.modified = info.modified;
        this.noteRecent(b.path);
        this.renderAll();
        this.updateStatus();
        this.host.emit("buffer.saved", { buffer: b });
      } else {
        await rpc("fs.write", { path, text: await b.textAsync(0, b.len) });
        await this.openFile(path);
      }
    } catch (e) { this.setStatus(`Not saved: ${e.detail || e.message || e}`); return; }
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
      } catch (e) { this.setStatus(`Not reloaded: ${e.detail || e.message || e}`); }
    } else if (pick === "keep") {
      b.keepMine = true;
      this.setStatus(`Keeping your changes to ${b.name}; saving overwrites the file`);
    }
  }

  autoSave() {
    if (!this.settings.auto_save) return;
    const now = Date.now();
    for (const b of this.buffers.values()) {
      // An untitled document waits for a name.
      if (b.path && b.modified && b.synced && now - b.lastEdit > this.settings.auto_save_seconds * 1000) {
        b.lastEdit = now;
        this.saveBuffer(b, { ask: false });
      }
    }
  }

  cursorMoved(v, old) {
    this.host.emit("cursor.moved", { view: v, buffer: v.buf, old });
    this.hidePopup();
    this.updateStatus();
  }

  // Move a view's cursor to `pos`, loading the text there first.
  async moveTo(v, pos, { reveal = true, center = false, extend = false, history = false } = {}) {
    if (history) this.pushHistory(v);
    const pane = v.pane;
    await v.buf.ensure(pos, pane.metrics().rows + 10);
    if (pane.view !== v) pane.show(v.buf);
    this.editing.place(pane, pos, extend);
    if (center) pane.reveal(true);
    pane.scheduleRender();
  }

  async gotoByte(p, extend) {
    const v = this.view();
    if (!v) return;
    await this.moveTo(v, p, { extend, center: !extend && Math.abs(p - v.primary.pos) > 5000 });
  }

  // `history`: remember where the view was, for Go Back.
  async gotoLine(line, col = 0, { history = true } = {}) {
    const v = this.view();
    if (!v) return;
    // Only the latest navigation lands: a newer one (the palette previews a
    // jump on every keystroke) supersedes any still waiting on the server.
    const seq = (this.navSeq = (this.navSeq || 0) + 1);
    const b = v.buf;
    let p;
    if (b.text && b.full) p = b.text.starts[Math.max(0, Math.min(line, b.text.count - 1))];
    else if (b.doc) {
      const r = await rpc("doc.line_start", { doc: b.doc, line });
      if (seq !== this.navSeq) return;
      if (r.pos == null) { this.setStatus("The line index is still being built"); return; }
      p = r.pos;
    }
    if (p == null) return;
    await b.ensure(p, 100);
    if (seq !== this.navSeq) return;
    const L = b.text;
    const i = L.indexOf(p);
    p += byteLen(L.lines[i].slice(0, col || 0));
    if (history) this.pushHistory(v);
    await this.moveTo(v, p, { center: true });
  }

  async scrollToFraction(pane, f) {
    const v = pane.view;
    if (!v) return;
    const b = v.buf;
    if (b.full) {
      const L = b.text;
      v.setTop(L.starts[Math.min(L.count - 1, Math.floor(f * L.count))]);
    } else if (b.totalLines) {
      const r = await rpc("doc.line_start", { doc: b.doc, line: Math.floor(f * b.totalLines) });
      if (r.pos != null) { await b.ensure(r.pos, 200); v.setTop(r.pos); }
    } else {
      const p = Math.floor(f * b.len);
      await b.ensure(p, 200);
      const L = b.text;
      v.setTop(L.starts[Math.max(0, L.indexOf(p))]);
    }
    pane.scheduleRender();
  }

  // ------------------------------------------------------------- position history

  pushHistory(v, pos = v.primary.pos) {
    const h = this.history;
    const top = h.back[h.back.length - 1];
    if (top && top.buf === v.buf && top.pos === pos) return;
    h.back.push({ buf: v.buf, pos });
    if (h.back.length > 100) h.back.shift();
    h.fwd = [];
  }
  async historyStep(dir) {
    const h = this.history;
    const from = dir < 0 ? h.back : h.fwd, to = dir < 0 ? h.fwd : h.back;
    const e = from.pop();
    if (!e) return;
    const v = this.view();
    // An entry for a closed buffer, or for where the view already is, is
    // passed over.
    if (!this.buffers.has(e.buf.id) || (v && v.buf === e.buf && v.primary.pos === e.pos)) return this.historyStep(dir);
    if (v) to.push({ buf: v.buf, pos: v.primary.pos });
    const nv = this.showBuffer(e.buf);
    await this.moveTo(nv, Math.min(e.pos, e.buf.len), { center: true });
  }

  // ------------------------------------------------------------- server events

  onEvent(ev) {
    switch (ev.ev) {
      case "doc.ops": {
        const b = this.docs.get(ev.doc);
        if (b) { b.onOps(ev, session.client); this.renderBuffer(b.id); }
        break;
      }
      case "doc.indexing": {
        const b = this.docs.get(ev.doc);
        if (b) this.setStatus(`Indexing lines of ${b.name}: ${Math.round((100 * ev.done) / Math.max(1, ev.total))}%`);
        break;
      }
      case "doc.indexed": {
        // The document has a line index now: fetch the window again, and it
        // comes with line numbers.
        const b = this.docs.get(ev.doc);
        if (!b) break;
        b.totalLines = ev.lines;
        const v = [...b.views][0];
        b.text = null;
        b.ensure(v ? v.top : 0).then(() => { this.renderBuffer(b.id); this.updateStatus(); });
        this.setStatus(`${b.name}: ${ev.lines} lines`);
        break;
      }
      case "doc.saved": {
        const b = this.docs.get(ev.doc);
        // Saved, or reloaded (its encoding may be another now).
        if (b) { b.setInfo(ev.info); this.renderAll(); this.updateStatus(); }
        break;
      }
      case "doc.renamed": {
        // The file was renamed or moved (here or by another client): the
        // buffer follows it.
        const b = this.docs.get(ev.doc);
        if (!b) break;
        b.setInfo(ev.info);
        this.renderAll();
        this.updateStatus();
        this.host.emit("buffer.renamed", { buffer: b, from: ev.from });
        break;
      }
      case "doc.info": {
        // Its language, line ending or encoding changed (`doc.set`).
        const b = this.docs.get(ev.doc);
        if (!b) break;
        b.setInfo(ev.info);
        this.renderAll();
        this.updateStatus();
        this.host.emit("buffer.info", { buffer: b });
        break;
      }
      case "doc.reloaded": {
        // A huge file changed on disk and was loaded again.
        const b = this.docs.get(ev.doc);
        if (!b) break;
        b.reloaded(ev.info).then(() => { this.renderAll(); this.updateStatus(); });
        this.setStatus(ev.lost ? `${b.name} changed on disk and was reloaded; its unsaved changes are lost` : `${b.name} changed on disk and was reloaded`);
        break;
      }
      case "doc.changed_on_disk": {
        // Changed on disk under unsaved changes: reload the file (giving
        // them up), or keep them, and the next save overwrites the file.
        const b = this.docs.get(ev.doc);
        if (b) this.askChangedOnDisk(b);
        break;
      }
      case "doc.deleted": {
        // The file was deleted: the buffer keeps its text, unsaved, and
        // saving writes the file again.
        const b = this.docs.get(ev.doc);
        if (!b) break;
        b.setInfo(ev.info);
        this.renderAll();
        this.setStatus(`${b.name} was deleted; its text is kept unsaved (save to write it again)`);
        this.host.emit("buffer.deleted", { buffer: b });
        break;
      }
      case "lsp.diagnostics": {
        const b = this.docs.get(ev.doc);
        if (!b) break;
        // Anchored at the version they describe, then carried through every
        // edit applied since, ours included.
        const items = ev.items.map((d) => ({ ...d }));
        const move = (op) => { for (const d of items) { d.start = shiftPos(d.start, op, true); d.end = Math.max(d.start, shiftPos(d.end, op, false)); } };
        for (const op of b.opsSince(ev.version)) move(op);
        b.decor.diags = items;
        b.decor.gen++;
        this.renderBuffer(b.id);
        this.host.emit("diagnostics.changed", { buffer: b });
        this.updateStatus();
        break;
      }
      case "term.data": case "term.exit": case "term.size":
        this.term.onEvent(ev);
        break;
      case "proc.exit": {
        const done = this.procs.get(ev.proc);
        if (done) { this.procs.delete(ev.proc); done({ code: ev.code, stdout: ev.stdout, stderr: ev.stderr }); }
        else this.procs.set(ev.proc, ev); // arrived before the request returned
        break;
      }
      case "lsp.status":
        // A language server was stopped or restarted: {language: running}.
        this.lspStatus = ev.status || {};
        this.host.emit("lsp.status", { status: { ...this.lspStatus } });
        this.updateStatus();
        break;
      case "workspace.trusted":
        // The language servers start now: the event names them.
        this.trusted = true;
        this.lspLanguages = new Set(ev.lsp);
        document.querySelector("#ws-name").textContent = document.querySelector("#ws-name").textContent.replace(" · restricted", "");
        this.host.emit("workspace.trusted", {});
        break;
      case "fs.changed":
        this.watches.get(ev.watch)?.(ev.path);
        break;
      case "workspace.added": case "workspace.removed": case "workspace.status": case "workspace.changed": case "orchestrator.folders":
        this.orchestrator.onEvent(ev);
        break;
    }
  }

  // A process on the workspace's host; resolves with {code, stdout, stderr}.
  async runProcess(program, args, { cwd } = {}) {
    const { proc } = await rpc("proc.run", { program, args, cwd });
    const early = this.procs.get(proc);
    if (early) { this.procs.delete(proc); return { code: early.code, stdout: early.stdout, stderr: early.stderr }; }
    return new Promise((resolve) => this.procs.set(proc, resolve));
  }

  async watch(path, f) {
    const { watch } = await rpc("fs.watch", { path });
    this.watches.set(watch, f);
    return () => { this.watches.delete(watch); rpc("fs.unwatch", { watch }); };
  }

  // A language server answers for `b`: one is installed for its language
  // and not stopped (`lsp.stop`).
  lspAvailable(b) { return !!(b.doc && b.language && !b.large && this.lspLanguages?.has(b.language) && this.lspStatus[b.language] !== false); }

  // A language server's (line, UTF-16 column) as a byte offset in a held buffer.
  lspOffset(b, line, character) {
    const L = b.text;
    if (!L || L.firstLine == null) return null;
    const i = line - L.firstLine;
    if (i < 0 || i >= L.count) return null;
    return L.starts[i] + byteLen(L.lines[i].slice(0, character));
  }

  lspPosition(b, pos) {
    const L = b.text;
    const i = L ? L.indexOf(pos) : -1;
    if (i < 0) return null;
    return { line: (L.firstLine ?? 0) + i, character: byteToIdx(L.lines[i], pos - L.starts[i]) };
  }

  // ------------------------------------------------------------- input

  onInput() {
    const t = this.input.value;
    this.input.value = "";
    if (!t) return;
    const v = this.view();
    if (!v || v.buf.readOnly) return;
    // A buffer mode may claim printable keys (a panel's single-key commands).
    const mode = v.buf.mode && this.modes.get(v.buf.mode);
    if (mode && t.length === 1 && mode[t]) { mode[t](); return; }
    this.macros.text(t, true);
    this.editing.type(this.activePane, t, true);
  }

  paste(text) {
    const v = this.view();
    if (!v || v.buf.readOnly) return;
    this.macros.text(text, false);
    // One line per cursor when the counts match, as editors do.
    const parts = text.split("\n");
    if (v.cursors.length > 1 && parts.length === v.cursors.length) {
      const order = v.cursors.map((c, i) => [c, i]).sort((a, b) => a[0].pos - b[0].pos);
      const byCursor = new Map(order.map(([c], k) => [c, parts[k]]));
      this.editing.replace(this.activePane, (c) => { const s = c.anchor != null && c.anchor !== c.pos ? { start: Math.min(c.anchor, c.pos), end: Math.max(c.anchor, c.pos) } : { start: c.pos, end: c.pos }; return { ...s, text: byCursor.get(c) }; });
    } else this.editing.type(this.activePane, text);
  }

  onCopy(e, cut) {
    const v = this.view();
    if (!v) return;
    // In document order, as paste hands them out.
    const sels = v.selections().sort((x, y) => x.start - y.start);
    // With nothing selected, copy (or cut) the cursor's line.
    const texts = sels.length ? sels.map((s) => v.buf.textRange(s.start, s.end)) : [this.editing.lineAt(v.buf, v.primary.pos).text + "\n"];
    e.preventDefault();
    e.clipboardData.setData("text/plain", texts.join("\n"));
    if (cut && !v.buf.readOnly) {
      if (sels.length) this.editing.replace(this.activePane, (c) => { const s = c.anchor != null && c.anchor !== c.pos ? { start: Math.min(c.anchor, c.pos), end: Math.max(c.anchor, c.pos) } : null; return s ? { ...s, text: "" } : null; });
      else this.editing.deleteLine(this.activePane);
    }
  }

  // A key as the keymap names it: "C-S-k", "M-Up", "F12".
  keyName(e) {
    const names = { ArrowUp: "Up", ArrowDown: "Down", ArrowLeft: "Left", ArrowRight: "Right", " ": "Space", Escape: "Esc" };
    let k = names[e.key] || e.key;
    // A digit is named by its key, shifted or not (Shift+1 is "S-1", not "!").
    if (/^Digit\d$/.test(e.code)) k = e.code.slice(5);
    if (k.length === 1) k = k.toLowerCase();
    const shift = e.shiftKey && (k.length > 1 || /[a-z0-9]/.test(k));
    return (e.ctrlKey || e.metaKey ? "C-" : "") + (e.altKey ? "M-" : "") + (shift ? "S-" : "") + k;
  }

  onKey(e) {
    const v = this.view();
    const k = this.keyName(e);
    // A buffer's mode (a plugin's panel buffer) takes its keys first.
    const mode = v?.buf.mode && this.modes.get(v.buf.mode);
    if (mode && mode[k]) { e.preventDefault(); mode[k](); return; }
    const cmd = this.keymap()[k];
    if (cmd) { e.preventDefault(); this.run(cmd); }
  }

  bindKey(key, command) { this.keys[key] = command; }

  // Resolve with the next key pressed, as the keymap names it.
  captureKey() {
    return new Promise((resolve) => {
      const f = (e) => {
        if (["Control", "Shift", "Alt", "Meta"].includes(e.key)) return;
        e.preventDefault();
        e.stopPropagation();
        window.removeEventListener("keydown", f, true);
        resolve(e.key === "Escape" ? null : this.keyName(e));
      };
      window.addEventListener("keydown", f, true);
    });
  }

  // A theme: every colour variable ({"--bg": "#1e1e1e", ...}). Everything
  // the page draws reads them; only the terminal's canvas copies them.
  applyTheme(vars) {
    for (const [k, v] of Object.entries(vars)) document.documentElement.style.setProperty(k, v);
    this.term.applyTheme();
  }

  keymap() {
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
    const emacs = this.settings.keymap === "emacs" ? {
      "C-f": "cursor.right", "C-b": "cursor.left", "C-n": "cursor.down", "C-p": "cursor.up", "C-a": "cursor.home", "C-e": "cursor.end",
      "M-f": "cursor.wordRight", "M-b": "cursor.wordLeft", "M-<": "cursor.docStart", "M->": "cursor.docEnd", "C-v": "cursor.pageDown", "M-v": "cursor.pageUp",
      "C-d": "edit.delete", "M-d": "edit.deleteWordForward", "C-k": "edit.killLine", "C-y": "edit.yank", "C-w": "edit.killRegion", "M-w": "edit.copyRegion",
      "C-/": "edit.undo", "C-_": "edit.undo", "C-g": "edit.escape", "M-x": "palette.commands", "C-Space": "edit.setMark", "M-g": "palette.line",
      "M-}": "cursor.paragraphDown", "M-{": "cursor.paragraphUp", "C-s": "search.find", "C-r": "search.findPrevious", "M-%": "search.replace",
    } : {};
    return { ...base, ...this.keys, ...emacs, ...this.userKeys };
  }

  keyFor(id) {
    const km = this.keymap();
    return Object.keys(km).filter((k) => km[k] === id).join(", ");
  }

  // The first key for command `id` as people write it ("Ctrl+Shift+K"),
  // or "".
  keyLabel(id) {
    const k = this.keyFor(id).split(", ")[0];
    if (!k) return "";
    const m = /^((?:[CMS]-)*)(.+)$/.exec(k);
    const mods = { C: "Ctrl", M: "Alt", S: "Shift" };
    const key = m[2].length === 1 ? m[2].toUpperCase() : m[2];
    return [...m[1].split("-").filter(Boolean).map((x) => mods[x]), key].join("+");
  }

  // Run command `id`, with its one optional argument (a theme name for
  // `theme.use`, …).
  // `id`, with its one optional argument (a theme name for `theme.use`, …).
  // Resolves when it is done. A macro being recorded takes it (not the
  // commands it runs itself).
  run(id, arg) {
    const c = this.commands.get(id);
    if (!c) { this.setStatus(`No command “${id}”`); return Promise.resolve(); }
    this.macros?.command(id, arg);
    if (this.macros) this.macros.depth++;
    try {
      const r = c.run(arg);
      return Promise.resolve(r).catch((e) => this.report(`${id}: ${e.stack || e}`));
    } catch (e) {
      this.report(`${id}: ${e.stack || e}`);
      return Promise.resolve();
    } finally {
      if (this.macros) this.macros.depth--;
    }
  }

  // ------------------------------------------------------------- core commands

  registerCoreCommands() {
    const ed = () => this.editing;
    const P = () => this.activePane;
    const V = () => this.view();
    const ro = () => { const v = V(); return !v || v.buf.readOnly; };
    const cmd = (id, title, run, hidden = false) => this.commands.set(id, { title, run, source: "core", hidden });
    const toggle = (k) => () => { this.settings[k] = !this.settings[k]; this.saveSettings(); this.setStatus(`${k.replace(/_/g, " ")}: ${this.settings[k] ? "on" : "off"}`); };
    const page = (dir, extend) => () => P().page(dir, extend);
    const mutate = (f) => () => { if (!ro()) f(); };

    cmd("file.save", "File: Save", () => V()?.buf.doc && this.saveBuffer(V().buf));
    cmd("file.saveAll", "File: Save All", async () => {
      const mod = [...this.buffers.values()].filter((b) => b.doc && b.modified);
      for (const b of mod) await this.saveBuffer(b);
      this.setStatus(mod.length ? `Saved ${mod.length} file(s)` : "Nothing to save");
    });
    cmd("file.toggleReadOnly", "Toggle Read-Only Mode (Current Buffer)", () => { const b = V()?.buf; if (b) { b.readOnly = !b.readOnly; this.renderAll(); } });
    // Save As: the text written to another file (through the server's
    // file operations), which then opens; the original stays as it is.
    cmd("file.saveAs", "File: Save As…", () => V() && this.saveBufferAs(V().buf));
    cmd("file.new", "File: New File", () => this.newUntitled());
    cmd("file.language", "Set Language…", () => this.pickLanguage());
    cmd("file.encoding", "Set Encoding… (reopen or save with)", () => this.pickEncoding());
    cmd("file.lineEnding", "Set Line Ending…", () => this.pickLineEnding());
    cmd("file.revert", "File: Revert (reload from disk)", async () => {
      const b = V()?.buf;
      if (!b?.doc) return;
      if (b.modified && !(await this.confirm(`Revert ${b.name}? Its unsaved changes are lost`, "Revert"))) return;
      try { await rpc("doc.reload", { doc: b.doc }); this.setStatus(`Reverted ${b.name}`); } catch (e) { this.setStatus(`Not reverted: ${e.detail || e.message || e}`); }
    });
    // The clipboard, from a menu: copy and cut go through the browser's own
    // copy (onCopy); paste reads the clipboard where the browser allows it.
    cmd("edit.copy", "Edit: Copy", () => { this.input.focus({ preventScroll: true }); document.execCommand("copy"); });
    cmd("edit.cut", "Edit: Cut", () => { this.input.focus({ preventScroll: true }); document.execCommand("cut"); });
    cmd("edit.paste", "Edit: Paste", async () => {
      try { this.paste(await navigator.clipboard.readText()); } catch { this.setStatus("The browser did not allow reading the clipboard here: paste with Ctrl+V"); }
    });
    cmd("edit.undo", "Edit: Undo", () => { const v = V(); if (v) this.afterUndo(v, v.buf.doUndo()); });
    cmd("edit.redo", "Edit: Redo", () => { const v = V(); if (v) this.afterUndo(v, v.buf.doRedo()); });
    cmd("palette.commands", "Command Palette", () => this.prompt.palette(">"));
    cmd("palette.files", "Open File…", () => this.prompt.palette(""));
    cmd("palette.buffers", "Switch Buffer…", () => this.prompt.palette("#"));
    cmd("palette.line", "Go to Line…", () => this.prompt.palette(":"));
    cmd("palette.open", "Palette with a Prefix", (prefix) => this.prompt.palette(prefix || ""), true);
    cmd("terminal.toggle", "Terminal: Toggle", () => this.term.toggle());
    cmd("terminal.new", "Terminal: New", async () => { if (!this.term.visible) await this.term.toggle(); await this.term.create(); });
    // The current terminal closed and a new one started in its place.
    cmd("terminal.restart", "Terminal: Restart", async () => {
      if (!this.term.visible) await this.term.toggle();
      const id = this.term.current;
      if (id != null) this.term.close(id);
      await this.term.create();
    });
    // The selection (or the cursor's line) typed into the current
    // terminal, as if pasted there; the terminal shows (and has the keys).
    cmd("terminal.sendSelection", "Terminal: Send Selection", async () => {
      const v = V();
      if (!v) return;
      const sels = v.selections().sort((x, y) => x.start - y.start);
      const text = sels.length ? sels.map((s) => v.buf.textRange(s.start, s.end)).join("\n") : this.editing.lineAt(v.buf, v.primary.pos).text + "\n";
      if (!this.term.visible) await this.term.toggle();
      if (this.term.current == null) await this.term.create();
      await rpc("term.input", { term: this.term.current, data: text });
    });
    cmd("skin.select", "Select Skin…", () => this.skins.pick());
    cmd("skin.next", "Next Skin", () => this.skins.cycle(1));
    cmd("view.splitRight", "View: Split Right", () => this.split("h"));
    cmd("view.splitDown", "View: Split Down", () => this.split("v"));
    cmd("view.closeSplit", "View: Close Split", () => this.closePane(P()));
    cmd("view.nextSplit", "View: Next Split", () => { const ps = this.panes(); this.focusPane(ps[(ps.indexOf(P()) + 1) % ps.length]); });
    cmd("view.prevSplit", "View: Previous Split", () => { const ps = this.panes(); this.focusPane(ps[(ps.indexOf(P()) - 1 + ps.length) % ps.length]); });
    cmd("view.closeTab", "View: Close Tab", () => { const p = P(); if (p.view) this.closeTab(p, p.view); });
    cmd("view.keepOpen", "View: Keep Tab Open (preview tab)", () => { const v = V(); if (v) this.keep(v); });
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
    cmd("file.recent", "File: Open Recent…", () => {
      const rel = (p) => (p.startsWith(this.root + "/") ? p.slice(this.root.length + 1) : p);
      const list = this.recentFiles();
      this.prompt.open({
        title: "Open Recent",
        items: list.length ? list.map((p) => ({ label: rel(p), detail: p.split("/").pop(), value: p })) : [{ label: "No recent files", value: null }],
        onAccept: (it) => it.value && this.openFile(it.value),
      });
    });
    cmd("view.maximizeSplit", "View: Toggle Maximize Split", () => {
      this.maximized = this.maximized || this.panes().length < 2 ? null : P();
      this.showMaximized();
      this.renderAll();
    });
    cmd("view.closeOtherTabs", "View: Close Other Tabs", () => { const p = P(); if (p.view) this.closeTabs(p, (x) => x !== p.view); });
    cmd("view.closeAllTabs", "View: Close All Tabs", async () => { for (const p of this.panes()) await this.closeTabs(p, () => true); });
    cmd("view.nextTab", "View: Next Tab", () => this.cycleTab(1));
    cmd("view.prevTab", "View: Previous Tab", () => this.cycleTab(-1));
    cmd("view.toggleWrap", "View: Toggle Line Wrap (Current Buffer)", () => { const v = V(); if (v) { v.opts.wrap = !v.opts.wrap; P().scheduleRender(); } });
    cmd("view.toggleLineNumbers", "View: Toggle Line Numbers (Current Buffer)", () => { const v = V(); if (v) { v.opts.lineNumbers = !v.opts.lineNumbers; P().scheduleRender(); } });
    cmd("view.toggleCurrentLine", "View: Toggle Current Line Highlight", toggle("highlight_current_line"));
    cmd("view.toggleOccurrences", "View: Toggle Occurrence Highlight", toggle("highlight_occurrences"));
    cmd("view.toggleWhitespace", "View: Toggle Whitespace Indicators", toggle("show_whitespace"));
    cmd("view.toggleIndentGuides", "View: Toggle Indentation Guides", toggle("show_indent_guides"));
    cmd("view.toggleBrackets", "View: Toggle Matching Bracket Highlight", toggle("highlight_brackets"));
    cmd("view.toggleRainbow", "View: Toggle Rainbow Brackets", toggle("rainbow_brackets"));
    cmd("view.toggleScrollSync", "View: Toggle Scroll Sync", toggle("scroll_sync"));
    cmd("view.scrollUp", "View: Scroll Up", () => P().scrollLines(-1), true);
    cmd("view.scrollDown", "View: Scroll Down", () => P().scrollLines(1), true);
    cmd("view.addRuler", "View: Add Ruler", async () => {
      const t = await this.prompt.ask({ title: "Ruler at column", value: "80" });
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
    cmd("view.focusEditor", "View: Focus Editor", () => this.focusPane(P()), true);
    cmd("workspace.trust", "Workspace: Trust (allow processes, terminals and language servers)", async () => {
      if (this.trusted) { this.setStatus("The workspace is trusted"); return; }
      await rpc("workspace.trust");
    });
    cmd("help.log", "Show Log", () => this.createVirtual({ name: "*Log*", entries: this.log.map((l) => ({ text: l + "\n" })), place: "tab" }));

    // Motion and selection: nothing to do without a view.
    const vcmd = (id, title, f, hidden = false) => cmd(id, title, () => (V() ? f() : undefined), hidden);
    const mv = (id, title, f) => vcmd(id, title, f, true);
    mv("cursor.left", "Left", () => ed().left(P(), false)); mv("cursor.right", "Right", () => ed().right(P(), false));
    mv("select.left", "Select Left", () => ed().left(P(), true)); mv("select.right", "Select Right", () => ed().right(P(), true));
    mv("cursor.wordLeft", "Word Left", () => ed().left(P(), false, true)); mv("cursor.wordRight", "Word Right", () => ed().right(P(), false, true));
    mv("select.wordLeft", "Select Word Left", () => ed().left(P(), true, true)); mv("select.wordRight", "Select Word Right", () => ed().right(P(), true, true));
    mv("cursor.up", "Up", () => ed().vertical(P(), -1, false)); mv("cursor.down", "Down", () => ed().vertical(P(), 1, false));
    mv("select.up", "Select Up", () => ed().vertical(P(), -1, true)); mv("select.down", "Select Down", () => ed().vertical(P(), 1, true));
    mv("cursor.home", "Line Start", () => ed().home(P(), false)); mv("cursor.end", "Line End", () => ed().end(P(), false));
    mv("select.home", "Select to Line Start", () => ed().home(P(), true)); mv("select.end", "Select to Line End", () => ed().end(P(), true));
    mv("cursor.docStart", "Document Start", () => { this.pushHistory(V()); return this.gotoByte(0); });
    mv("cursor.docEnd", "Document End", () => { this.pushHistory(V()); return this.gotoByte(V().buf.len); });
    mv("select.docStart", "Select to Document Start", () => this.gotoByte(0, true));
    mv("select.docEnd", "Select to Document End", () => this.gotoByte(V().buf.len, true));
    mv("cursor.pageUp", "Page Up", page(-1, false)); mv("cursor.pageDown", "Page Down", page(1, false));
    mv("select.pageUp", "Select Page Up", page(-1, true)); mv("select.pageDown", "Select Page Down", page(1, true));
    vcmd("cursor.paragraphUp", "Go to Previous Paragraph", () => ed().paragraph(P(), -1, false));
    vcmd("cursor.paragraphDown", "Go to Next Paragraph", () => ed().paragraph(P(), 1, false));
    mv("select.paragraphUp", "Select to Previous Paragraph", () => ed().paragraph(P(), -1, true));
    mv("select.paragraphDown", "Select to Next Paragraph", () => ed().paragraph(P(), 1, true));
    vcmd("cursor.matchingBracket", "Go to Matching Bracket", () => ed().matchingBracket(P()));
    vcmd("cursor.addAbove", "Add Cursor Above", () => ed().addCursorVertical(P(), -1));
    vcmd("cursor.addBelow", "Add Cursor Below", () => ed().addCursorVertical(P(), 1));
    cmd("select.all", "Select All", () => { const v = V(); if (v) { v.cursors = [{ pos: v.buf.len, anchor: 0, goalX: null }]; P().scheduleRender(); } });
    vcmd("select.line", "Select Line", () => ed().selectLine(P()));
    vcmd("select.nextOccurrence", "Select Next Occurrence", () => ed().selectNextOccurrence(P()));
    vcmd("select.allOccurrences", "Select All Occurrences", () => ed().selectAllOccurrences(P()));
    vcmd("select.expand", "Expand Selection", () => ed().expandSelection(P()));
    vcmd("select.shrink", "Shrink Selection", () => ed().shrinkSelection(P()));
    mv("select.blockUp", "Block Select Up", () => ed().blockSelect(P(), -1, 0));
    mv("select.blockDown", "Block Select Down", () => ed().blockSelect(P(), 1, 0));
    mv("select.blockLeft", "Block Select Left", () => ed().blockSelect(P(), 0, -1));
    mv("select.blockRight", "Block Select Right", () => ed().blockSelect(P(), 0, 1));
    // Editing.
    mv("edit.newline", "Newline", mutate(() => ed().newline(P())));
    mv("edit.backspace", "Backspace", mutate(() => ed().backspace(P(), false)));
    mv("edit.deleteWordBack", "Delete Word Backward", mutate(() => ed().backspace(P(), true)));
    mv("edit.delete", "Delete", mutate(() => ed().del(P(), false)));
    mv("edit.deleteWordForward", "Delete Word Forward", mutate(() => ed().del(P(), true)));
    mv("edit.indent", "Indent", mutate(() => ed().indent(P(), 1)));
    mv("edit.dedent", "Dedent", mutate(() => ed().indent(P(), -1)));
    mv("edit.escape", "Escape", () => { const v = V(); if (!v) return; v.cursors = [v.primary]; v.primary.anchor = null; this.hidePopup(); this.host.emit("escape", {}); P().scheduleRender(); });
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
    vcmd("fold.toggle", "Toggle Fold", () => ed().toggleFold(P()));
    vcmd("fold.all", "Fold All", () => ed().foldAll(P()));
    vcmd("fold.none", "Unfold All", () => ed().unfoldAll(P()));
    // Emacs: mark and kill ring.
    mv("edit.setMark", "Set Mark", () => { const v = V(); v.primary.anchor = v.primary.pos; this.setStatus("Mark set"); });
    mv("edit.killLine", "Kill Line", mutate(() => {
      const v = V();
      const ln = ed().lineAt(v.buf, v.primary.pos);
      const end = v.primary.pos === ln.end ? ln.end + 1 : ln.end;
      this.killRing.unshift(v.buf.textRange(v.primary.pos, end) || "");
      ed().replace(P(), (c) => (c === v.primary ? { start: c.pos, end, text: "", caret: 0 } : null));
    }));
    mv("edit.killRegion", "Kill Region", mutate(() => {
      const s = V().selection();
      if (!s) return;
      this.killRing.unshift(V().buf.textRange(s.start, s.end) || "");
      ed().replace(P(), (c) => (c === V().primary ? { ...s, text: "" } : null));
    }));
    mv("edit.copyRegion", "Copy Region", () => { const s = V().selection(); if (s) { this.killRing.unshift(V().buf.textRange(s.start, s.end) || ""); V().primary.anchor = null; P().scheduleRender(); } });
    mv("edit.yank", "Yank", mutate(() => this.killRing.length && ed().type(P(), this.killRing[0])));
  }

  // ------------------------------------------------------------- a document's format

  // A document's language, line ending and encoding, as the server names
  // them (asked once).
  async docFormats() {
    if (!this.formats) this.formats = await rpc("doc.formats");
    return this.formats;
  }
  // Pick one of `choices` ({label, detail, value}) for the active file;
  // the current one is marked and first selected. Resolves with the value,
  // or null.
  async pickFor(title, choices, current) {
    const b = this.view()?.buf;
    if (!b?.doc) { this.setStatus("Not a file"); return null; }
    const items = choices.map((c) => (c.value === current ? { ...c, detail: c.detail ? `${c.detail} · current` : "current" } : c));
    const i = Math.max(0, items.findIndex((c) => c.value === current));
    return new Promise((resolve) => this.prompt.open({ title, items: [...items.slice(i), ...items.slice(0, i)], onAccept: (it) => resolve(it.value), onCancel: () => resolve(null) }));
  }
  async setFormat(b, change) {
    try { await rpc("doc.set", { doc: b.doc, ...change }); } catch (e) { this.setStatus(String(e.detail || e.message || e)); }
  }
  async pickLanguage() {
    const b = this.view()?.buf;
    const f = await this.docFormats();
    const lang = await this.pickFor("Language", [{ label: "plain text", value: "" }, ...f.languages.map((l) => ({ label: l, value: l }))], b?.language || "");
    if (lang != null) await this.setFormat(b, { language: lang || null });
  }
  async pickLineEnding() {
    const b = this.view()?.buf;
    const f = await this.docFormats();
    const names = { LF: "LF — Unix, macOS", CRLF: "CRLF — Windows", CR: "CR — classic Mac" };
    const le = await this.pickFor("Line ending (saving converts the file)", f.line_endings.map((l) => ({ label: names[l] || l, value: l })), b?.lineEnding);
    if (le && le !== b.lineEnding) { await this.setFormat(b, { line_ending: le }); this.setStatus(`${b.name} is saved with ${le} line endings`); }
  }
  // An encoding, then what to do with it: read the file again in it, or
  // write the file in it on the next save.
  async pickEncoding() {
    const b = this.view()?.buf;
    const f = await this.docFormats();
    const enc = await this.pickFor("Encoding", f.encodings.map((e) => ({ label: e.name, detail: e.description, value: e.name })), b?.encoding);
    if (!enc) return;
    const how = await this.choose(`${b.name} in ${enc}`, [
      ...(b.large ? [] : [{ label: `Reopen with ${enc} (read the file again in it)`, value: "reopen" }]),
      { label: `Save with ${enc} (the next save writes it)`, value: "save" },
    ]);
    if (how === "save") await this.setFormat(b, { encoding: enc });
    if (how === "reopen") {
      if (b.modified && !(await this.confirm(`Reopening ${b.name} gives up its unsaved changes`, "Reopen"))) return;
      try { await rpc("doc.reload", { doc: b.doc, encoding: enc }); this.setStatus(`Reopened ${b.name} with ${enc}`); } catch (e) { this.setStatus(String(e.detail || e.message || e)); }
    }
  }

  afterUndo(v, ops) {
    if (!ops || !ops.length) return;
    const last = ops[ops.length - 1];
    v.cursors = [v.primary];
    this.editing.place(v.pane, last.t === "ins" ? last.pos + byteLen(last.text) : last.pos, false);
  }

  saveSettings() {
    try { localStorage.setItem("fresh-poc:settings", JSON.stringify(this.settings)); } catch {}
    this.renderAll();
  }
  saveKeys() {
    try { localStorage.setItem("fresh-poc:keys", JSON.stringify(this.userKeys)); } catch {}
  }
  cycleTab(d) {
    const p = this.activePane;
    if (!p.views.length) return;
    const i = (p.views.indexOf(p.view) + d + p.views.length) % p.views.length;
    p.show(p.views[i].buf);
  }

  // ------------------------------------------------------------- hover and popups

  hoverProvide(f) { this.hoverProviders.push(f); }

  onHover(pane, p, x, y) {
    clearTimeout(this.hoverTimer);
    if (p == null || !pane.view) return;
    this.hoverTimer = setTimeout(async () => {
      const b = pane.view.buf;
      const parts = [];
      for (const f of this.hoverProviders) {
        try { const t = await f(b, p); if (t) parts.push(t); } catch {}
      }
      if (parts.length) this.popup(parts.join("\n\n"), { x, y: y + this.metrics.lh });
    }, 600);
  }

  popup(text, at) {
    const h = $("#popup");
    h.textContent = text;
    if (!at) {
      const pane = this.activePane;
      const c = pane.caretBox(pane.view.primary.pos);
      const r = pane.surfaceEl.getBoundingClientRect();
      at = c ? { x: c.x, y: c.y + c.h } : { x: r.left, y: r.top };
    }
    h.style.left = `${Math.min(at.x, innerWidth - 420)}px`;
    h.style.top = `${at.y}px`;
    h.classList.remove("hidden");
  }
  hidePopup() { $("#popup").classList.add("hidden"); }

  // Ask a question with a few answers [{label, value}]: resolves with the
  // value of the one picked, or null when cancelled.
  choose(title, answers) {
    return new Promise((resolve) => this.prompt.open({
      title,
      items: answers,
      onAccept: (it) => resolve(it.value),
      onCancel: () => resolve(null),
    }));
  }
  // Ask a yes/no question: resolves true when `yes` is picked.
  async confirm(title, yes) {
    return (await this.choose(title, [{ label: yes, value: true }, { label: "Cancel", value: false }])) === true;
  }

  // ------------------------------------------------------------- status

  setStatus(m) { this.status = m; this.updateStatus(); }

  statusItem(id) {
    const app = this;
    if (!this.statusItems.has(id)) this.statusItems.set(id, { text: "", title: "", onClick: null });
    const it = this.statusItems.get(id);
    return { set(text, o = {}) { it.text = text; it.title = o.title || ""; it.onClick = o.onClick || null; app.updateStatus(); } };
  }

  // The status bar: the message, then the facts. A fact that has an action
  // (a command, or a plugin item's onClick) runs it when clicked; its
  // tooltip names it and its key. The bar is written only when what it
  // shows changed.
  updateStatus() {
    const v = this.view();
    const left = $("#status-left"), right = $("#status-right");
    if (!left) return;
    if (left.textContent !== this.status) left.textContent = this.status;
    const items = [];
    const add = (text, title, run) => items.push({ text, title, run });
    const cmd = (id) => () => this.run(id);
    const titled = (what, id) => { const k = this.keyLabel(id); return k ? `${what} (${k})` : what; };
    if (v) {
      const b = v.buf;
      const L = b.text;
      const c = v.primary;
      const i = L ? L.indexOf(c.pos) : -1;
      if (i >= 0) {
        const col = [...L.lines[i].slice(0, byteToIdx(L.lines[i], c.pos - L.starts[i]))].length + 1;
        add(L.firstLine != null ? `Ln ${L.firstLine + i + 1}, Col ${col}` : `Byte ${c.pos}`, titled("Go to Line", "palette.line"), cmd("palette.line"));
      }
      if (v.cursors.length > 1) add(`${v.cursors.length} cursors`, titled("Remove the other cursors", "edit.escape"), cmd("edit.escape"));
      if (this.macros?.recording) add(`● REC ${this.macros.recording.register}`, titled("Recording a macro: Stop Recording Macro", "macro.stop"), cmd("macro.stop"));
      if (b.readOnly && !b.isVirtual) add("[RO]", "Read-only: click to allow editing", cmd("file.toggleReadOnly"));
      if (b.large) add("large file", "Read from disk as it is viewed; lines are counted in the background");
      if (b.doc) {
        if (b.lineEnding) add(b.lineEnding, titled("Line ending: Set Line Ending", "file.lineEnding"), cmd("file.lineEnding"));
        if (b.encoding) add(b.encoding, titled("Encoding: Reopen or Save with Encoding", "file.encoding"), cmd("file.encoding"));
        add(b.language || "plain text", titled("Language: Set Language", "file.language"), cmd("file.language"));
      } else if (b.language) add(b.language);
      const ds = b.decor.diags;
      if (ds.length) add(`● ${ds.filter((d) => d.severity === 1).length}  ▲ ${ds.filter((d) => d.severity === 2).length}`, titled("Diagnostics: Show Panel", "lsp.diagnostics"), cmd("lsp.diagnostics"));
      if (v.mode === "compose") add("compose", titled("Markdown: Toggle Compose", "markdown.toggleCompose"), cmd("markdown.toggleCompose"));
      if (b.doc) add(b.synced ? `v${b.version}` : `v${b.version}…`, "Document log version");
    }
    for (const it of this.statusItems.values()) if (it.text) add(it.text, it.title, it.onClick);
    add(this.hostName, "Workspace host");
    const sig = items.map((x) => `${x.text}\u0001${x.title || ""}\u0001${x.run ? 1 : 0}`).join("\u0002");
    this.statusRuns = items.map((x) => x.run);
    if (sig === this.statusSig) return;
    this.statusSig = sig;
    right.replaceChildren(...items.map((x, k) => {
      const s = document.createElement("span");
      s.textContent = x.text;
      if (x.title) s.title = x.title;
      if (x.run) { s.className = "click"; s.onclick = () => this.statusRuns[k]?.(); }
      return s;
    }));
  }
}

window.app = new App();
window.app.start();
export { THEME };
