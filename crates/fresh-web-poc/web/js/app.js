// The client application shell: panes and tabs, the palette, the status
// bar, the keymap and command registry, terminals, and the glue between the
// server's events, the buffers and the plugins. Features with UI of their
// own (explorer, search, git, language-server panels, markdown, tours,
// keybindings, themes) are plugins in `web/plugins/`.

import { rpc, session, connectEvents } from "./rpc.js";
import { byteLen, shiftPos } from "./ot.js";
import { FileBuffer, VirtualBuffer, byteToIdx } from "./buffer.js";
import { Pane, resetStyleCaches } from "./view.js";
import { Editing } from "./editing.js";
import { TerminalPanel } from "./terminal.js";
import { UIKit } from "./ui.js";
import { Host } from "./host.js";
import { Palette } from "./palette.js";
import { THEME } from "./highlight.js";

const $ = (s) => document.querySelector(s);

const PLUGINS = ["welcome", "explorer", "search", "lsp", "git", "markdown", "compare", "tours", "keybindings", "themes"];

const DEFAULT_SETTINGS = {
  auto_close: true, auto_save: false, auto_save_seconds: 30, highlight_current_line: true, highlight_occurrences: true,
  show_whitespace: false, rulers: [], page_width: 80, keymap: "default", theme: "dark",
};

class App {
  constructor() {
    this.buffers = new Map();
    this.docs = new Map();
    this.commands = new Map(); // id -> {title, run, source, hidden}
    this.modes = new Map(); // buffer mode -> {key: fn}
    this.keys = {}; // key -> command, from plugins
    this.statusItems = new Map();
    this.procs = new Map();
    this.watches = new Map();
    this.hoverProviders = [];
    this.history = { back: [], fwd: [] };
    this.killRing = [];
    this.metrics = { cw: 8, lh: 18 };
    this.status = "";
    this.log = [];
    let saved = {};
    try { saved = JSON.parse(localStorage.getItem("fresh-poc:settings") || "{}"); } catch {}
    this.settings = { ...DEFAULT_SETTINGS, ...saved };
    try { this.userKeys = JSON.parse(localStorage.getItem("fresh-poc:keys") || "{}"); } catch { this.userKeys = {}; }
  }

  async start() {
    this.measure();
    const hello = await rpc("hello");
    session.client = hello.client;
    this.root = hello.root;
    this.host = null;
    this.hostName = hello.host;
    this.trusted = hello.trusted;
    this.lspLanguages = new Set(hello.lsp);
    document.title = `${hello.name} — Fresh`;
    $("#ws-name").textContent = `${hello.name} · ${hello.host}${hello.trusted ? "" : " · restricted"}`;
    connectEvents((ev) => this.onEvent(ev));
    this.editing = new Editing(this);
    this.ui = new UIKit(this);
    this.prompt = new Palette(this);
    this.term = new TerminalPanel(this, $("#terminal"));
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
    this.layoutTree = { pane: new Pane(this) };
    this.activePane = this.layoutTree.pane;
    this.registerCoreCommands();
    this.relayout();
    new ResizeObserver(() => this.renderAll()).observe($("#editor-area"));
    setInterval(() => this.autoSave(), 1000);
    await this.host.load(PLUGINS);
    await this.term.adopt(hello.terminals);
    this.host.emit("app.ready", {});
    const open = new URLSearchParams(location.search).get("open");
    if (open) await this.openFile(open);
    this.focusPane(this.activePane);
  }

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
      a.style.flex = `${n.ratio} 1 0`;
      b.style.flex = `${1 - n.ratio} 1 0`;
      d.append(a, b);
      return d;
    };
    const el = build(this.layoutTree);
    el.style.flex = "1 1 0";
    area.appendChild(el);
    for (const p of this.panes()) p.el.classList.toggle("active", p === this.activePane);
    this.renderAll();
  }
  renderAll() { for (const p of this.panes()) { p.renderTabs(); p.scheduleRender(); } }
  renderBuffer(id) { for (const p of this.panes()) if (p.view?.buf.id === id) p.scheduleRender(); }
  view() { return this.activePane?.view || null; }
  focusPane(p) {
    if (!p) return;
    const changed = this.activePane !== p;
    this.activePane = p;
    for (const x of this.panes()) x.el.classList.toggle("active", x === p);
    this.input.focus({ preventScroll: true });
    if (changed) { this.renderAll(); if (p.view) this.host?.emit("buffer.activated", { buffer: p.view.buf, view: p.view }); }
  }
  split(dir, ratio = 0.5, buf = null) {
    const old = this.activePane;
    const np = new Pane(this);
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
  closePane(p) {
    if (this.panes().length === 1) return;
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
  closeTab(p, v) {
    p.closeView(v);
    this.dropIfUnshown(v.buf);
    if (!p.views.length && this.panes().length > 1) this.closePane(p);
    this.renderAll();
    this.updateStatus();
  }
  dropIfUnshown(b) {
    if (this.panes().some((x) => x.views.some((y) => y.buf === b))) return;
    if (b.keep) return;
    this.buffers.delete(b.id);
    if (b.doc) { this.docs.delete(b.doc); rpc("doc.close", { doc: b.doc }).catch(() => {}); }
    this.host.emit("buffer.closed", { buffer: b });
  }
  closeBuffer(b) {
    for (const p of this.panes()) for (const v of [...p.views]) if (v.buf === b) this.closeTab(p, v);
    this.dropIfUnshown(b);
  }
  onViewShown(pane, v) {
    if (pane === this.activePane) this.host?.emit("buffer.activated", { buffer: v.buf, view: v });
    this.updateStatus();
  }
  // Show a buffer: in the active pane ("tab"), a new split below ("split")
  // or to the right ("right").
  showBuffer(b, place = "tab") {
    if (place === "split" || place === "right") {
      const existing = this.panes().find((p) => p.view?.buf === b);
      if (existing) { this.focusPane(existing); return existing.view; }
      return this.split(place === "split" ? "v" : "h", 0.6, b).view;
    }
    const v = this.activePane.show(b);
    this.focusPane(this.activePane);
    return v;
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
    const info = await rpc("doc.open", { path: abs });
    b = this.docs.get(info.doc);
    if (b) return b;
    b = new FileBuffer(info);
    this.buffers.set(b.id, b);
    this.docs.set(b.doc, b);
    this.watchBuffer(b);
    await b.ensure(0);
    this.host.emit("buffer.opened", { buffer: b });
    return b;
  }

  // Show a file, at a line and (UTF-16) column when given. A navigation:
  // Go Back returns to where the active view was.
  async openFile(path, { line, column, place } = {}) {
    try {
      const from = this.view();
      const b = await this.loadFile(path);
      if (from) this.pushHistory(from);
      const v = this.showBuffer(b, place);
      if (line != null) await this.gotoLine(line - 1, column != null ? column - 1 : 0, { history: false });
      this.updateStatus();
      return v;
    } catch (e) {
      this.setStatus(String(e.message || e));
      return null;
    }
  }

  createVirtual(o) {
    const b = new VirtualBuffer(o.name, { mode: o.mode, readOnly: o.readOnly ?? true });
    b.setEntries(o.entries || [{ text: o.text || "" }]);
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
    this.host.emit("buffer.changed", { buffer: b, ops: [data.op], local: data.local });
    this.renderBuffer(b.id);
    for (const p of this.panes()) if (p.views.some((v) => v.buf === b)) p.renderTabs();
  }

  // Save a file. The server refuses when the file changed on disk since it
  // was opened or last saved; an explicit save (`ask`) then offers to
  // overwrite it, auto-save leaves it and says so.
  async saveBuffer(b, { ask = true } = {}) {
    if (!b.doc) return;
    try {
      await b.save();
    } catch (e) {
      const msg = String(e.message || e).replace(/^doc\.save: /, "");
      if (!ask || !/changed on disk/.test(msg)) { this.setStatus(`Not saved: ${msg}`); return; }
      if (!(await this.confirm(`${b.name} changed on disk since it was opened. Overwrite it?`, "Overwrite"))) { this.setStatus(`Not saved: ${b.name}`); return; }
      await b.save({ force: true });
    }
    this.setStatus(`Saved ${b.name}`);
    this.renderAll();
    this.host.emit("buffer.saved", { buffer: b });
  }

  autoSave() {
    if (!this.settings.auto_save) return;
    const now = Date.now();
    for (const b of this.buffers.values()) {
      if (b.doc && b.modified && !b.inflight && !b.pending.length && now - (b.lastEdit || 0) > this.settings.auto_save_seconds * 1000) {
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
      v.top = L.starts[Math.min(L.count - 1, Math.floor(f * L.count))];
    } else if (b.totalLines) {
      const r = await rpc("doc.line_start", { doc: b.doc, line: Math.floor(f * b.totalLines) });
      if (r.pos != null) { await b.ensure(r.pos, 200); v.top = r.pos; }
    } else {
      const p = Math.floor(f * b.len);
      await b.ensure(p, 200);
      const L = b.text;
      v.top = L.starts[Math.max(0, L.indexOf(p))];
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
        if (b) { b.modified = ev.info.modified; this.renderAll(); }
        break;
      }
      case "lsp.diagnostics": {
        const b = this.docs.get(ev.doc);
        if (!b) break;
        // Anchored at the version they describe, then carried through every
        // edit applied since, ours included.
        const items = ev.items.map((d) => ({ ...d }));
        const move = (op) => { for (const d of items) { d.start = shiftPos(d.start, op, true); d.end = Math.max(d.start, shiftPos(d.end, op, false)); } };
        for (const e of b.recent) if (e.version > ev.version) for (const op of e.ops) move(op);
        for (const op of [...(b.inflight || []), ...b.pending]) move(op);
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

  lspAvailable(b) { return !!(b.doc && b.language && !b.large && this.lspLanguages?.has(b.language)); }

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
    this.editing.type(this.activePane, t, true);
  }

  paste(text) {
    const v = this.view();
    if (!v || v.buf.readOnly) return;
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
    const sels = v.selections();
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
    if (k.length === 1) k = k.toLowerCase();
    const shift = e.shiftKey && (k.length > 1 || /[a-z]/.test(k));
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

  applyTheme(t) {
    for (const [k, v] of Object.entries(t)) {
      if (k.startsWith("--")) document.documentElement.style.setProperty(k, v);
      else THEME[k] = v;
    }
    resetStyleCaches();
    for (const p of this.panes()) p.clearRows();
    this.ui.redraw();
    this.term.applyTheme();
    this.renderAll();
  }

  keymap() {
    const base = {
      "C-s": "file.save", "C-M-s": "file.saveAll", "C-z": "edit.undo", "C-y": "edit.redo", "C-S-z": "edit.redo",
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
      "C-/": "edit.toggleComment", "C-d": "select.nextOccurrence", "C-S-l": "select.allOccurrences", "C-M-Up": "cursor.addAbove", "C-M-Down": "cursor.addBelow",
      "C-]": "cursor.matchingBracket", "C-{": "fold.toggle", "C-}": "fold.toggle",
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

  run(id, ...args) {
    const c = this.commands.get(id);
    if (!c) { this.setStatus(`No command “${id}”`); return; }
    try {
      const r = c.run(...args);
      if (r && r.catch) r.catch((e) => this.report(`${id}: ${e.stack || e}`));
    } catch (e) { this.report(`${id}: ${e.stack || e}`); }
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
    cmd("file.toggleReadOnly", "Toggle Read-Only Mode (Current Buffer)", () => { const b = V()?.buf; if (b) { b._readOnly = !b._readOnly; this.renderAll(); } });
    cmd("edit.undo", "Edit: Undo", () => { const v = V(); if (v) this.afterUndo(v, v.buf.doUndo()); });
    cmd("edit.redo", "Edit: Redo", () => { const v = V(); if (v) this.afterUndo(v, v.buf.doRedo()); });
    cmd("palette.commands", "Command Palette", () => this.prompt.palette(">"));
    cmd("palette.files", "Open File…", () => this.prompt.palette(""));
    cmd("palette.buffers", "Switch Buffer…", () => this.prompt.palette("#"));
    cmd("palette.line", "Go to Line…", () => this.prompt.palette(":"));
    cmd("terminal.toggle", "Terminal: Toggle", () => this.term.toggle());
    cmd("terminal.new", "Terminal: New", async () => { if (!this.term.visible) await this.term.toggle(); await this.term.create(); });
    cmd("view.splitRight", "View: Split Right", () => this.split("h"));
    cmd("view.splitDown", "View: Split Down", () => this.split("v"));
    cmd("view.closeSplit", "View: Close Split", () => this.closePane(P()));
    cmd("view.nextSplit", "View: Next Split", () => { const ps = this.panes(); this.focusPane(ps[(ps.indexOf(P()) + 1) % ps.length]); });
    cmd("view.prevSplit", "View: Previous Split", () => { const ps = this.panes(); this.focusPane(ps[(ps.indexOf(P()) - 1 + ps.length) % ps.length]); });
    cmd("view.closeTab", "View: Close Tab", () => { const p = P(); if (p.view) this.closeTab(p, p.view); });
    cmd("view.nextTab", "View: Next Tab", () => this.cycleTab(1));
    cmd("view.prevTab", "View: Previous Tab", () => this.cycleTab(-1));
    cmd("view.toggleWrap", "View: Toggle Line Wrap (Current Buffer)", () => { const v = V(); if (v) { v.opts.wrap = !v.opts.wrap; P().scheduleRender(); } });
    cmd("view.toggleLineNumbers", "View: Toggle Line Numbers (Current Buffer)", () => { const v = V(); if (v) { v.opts.lineNumbers = !v.opts.lineNumbers; P().scheduleRender(); } });
    cmd("view.toggleCurrentLine", "View: Toggle Current Line Highlight", toggle("highlight_current_line"));
    cmd("view.toggleOccurrences", "View: Toggle Occurrence Highlight", toggle("highlight_occurrences"));
    cmd("view.toggleWhitespace", "View: Toggle Whitespace Indicators", toggle("show_whitespace"));
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

  // Ask a yes/no question: resolves true when `yes` is picked.
  confirm(title, yes) {
    return new Promise((resolve) => this.prompt.open({
      title,
      items: [{ label: yes, value: true }, { label: "Cancel", value: false }],
      onAccept: (it) => resolve(it.value),
      onCancel: () => resolve(false),
    }));
  }

  // ------------------------------------------------------------- status

  setStatus(m) { this.status = m; this.updateStatus(); }

  statusItem(id) {
    const app = this;
    if (!this.statusItems.has(id)) this.statusItems.set(id, { text: "", title: "", onClick: null });
    const it = this.statusItems.get(id);
    return { set(text, o = {}) { it.text = text; it.title = o.title || ""; it.onClick = o.onClick || null; app.updateStatus(); } };
  }

  updateStatus() {
    const v = this.view();
    const left = $("#status-left"), right = $("#status-right");
    if (!left) return;
    left.textContent = this.status;
    right.innerHTML = "";
    const add = (t, title, onClick) => {
      const s = document.createElement("span");
      s.textContent = t;
      if (title) s.title = title;
      if (onClick) { s.className = "click"; s.onclick = onClick; }
      right.appendChild(s);
    };
    if (v) {
      const b = v.buf;
      const L = b.text;
      const c = v.primary;
      const i = L ? L.indexOf(c.pos) : -1;
      if (i >= 0) {
        const col = [...L.lines[i].slice(0, byteToIdx(L.lines[i], c.pos - L.starts[i]))].length + 1;
        add(L.firstLine != null ? `Ln ${L.firstLine + i + 1}, Col ${col}` : `Byte ${c.pos}`);
      }
      if (v.cursors.length > 1) add(`${v.cursors.length} cursors`);
      if (b.readOnly && !b.isVirtual) add("[RO]");
      if (b.large) add("large file");
      if (b.encoding) add(b.encoding);
      if (b.language) add(b.language);
      const ds = b.decor.diags;
      if (ds.length) add(`● ${ds.filter((d) => d.severity === 1).length}  ▲ ${ds.filter((d) => d.severity === 2).length}`, "Diagnostics", () => this.run("lsp.diagnostics"));
      if (v.mode === "compose") add("compose");
      if (b.doc) add(b.inflight || b.pending.length ? `v${b.version}…` : `v${b.version}`, "Document log version");
    }
    for (const it of this.statusItems.values()) if (it.text) add(it.text, it.title, it.onClick);
    add(this.hostName);
  }
}

window.app = new App();
window.app.start();
export { THEME };
