// The client application: panes and tabs, the file explorer, prompts and
// the command palette, the status bar, terminals, and the glue between the
// server's events, the buffers and the plugins.

import { rpc, session, connectEvents } from "./rpc.js";
import { byteLen } from "./ot.js";
import { FileBuffer, VirtualBuffer, byteToIdx, nl } from "./buffer.js";
import { Pane } from "./view.js";
import { Plugins } from "./plugins.js";
import { TerminalPanel } from "./terminal.js";
import { transformSeqs } from "./ot.js";

const $ = (s) => document.querySelector(s);

class Prompt {
  constructor(app) {
    this.app = app;
    this.el = $("#prompt");
    this.label = this.el.querySelector(".label");
    this.input = this.el.querySelector("input");
    this.list = this.el.querySelector(".list");
    this.items = [];
    this.sel = 0;
    this.spec = null;
    this.input.addEventListener("input", () => this.changed());
    this.input.addEventListener("keydown", (e) => this.key(e));
    this.input.addEventListener("blur", () => setTimeout(() => { if (this.spec && document.activeElement !== this.input) this.cancel(); }, 150));
  }
  get isOpen() { return !!this.spec; }
  // spec: {label, type, initial, source(query) -> [{text, description, value}] | Promise, accept(item, input), plugin}
  open(spec) {
    if (this.spec) this.cancel(true);
    this.spec = spec;
    this.label.textContent = spec.label || "";
    this.input.value = spec.initial || "";
    this.items = [];
    this.sel = 0;
    this.el.classList.remove("hidden");
    this.input.focus();
    this.changed();
  }
  setSuggestions(items, sel) {
    this.items = (items || []).map((s) => ({ text: s.text, description: s.description, value: s.value, disabled: s.disabled }));
    this.allItems = this.items;
    this.sel = sel ?? 0;
    this.filter();
  }
  filter() {
    if (this.spec?.plugin && this.allItems) {
      const q = this.input.value.toLowerCase();
      this.items = this.allItems.filter((i) => !q || i.text.toLowerCase().includes(q));
    }
    this.renderList();
  }
  async changed() {
    const s = this.spec;
    if (!s) return;
    if (s.plugin) {
      this.app.plugins.fire("prompt_changed", { prompt_type: s.type, input: this.input.value });
      this.filter();
      return;
    }
    if (s.source) {
      const q = this.input.value;
      const r = await s.source(q);
      if (this.spec !== s || this.input.value !== q) return;
      this.items = r || [];
      this.sel = 0;
      this.renderList();
    }
  }
  renderList() {
    this.list.innerHTML = "";
    this.items.slice(0, 200).forEach((it, i) => {
      const d = document.createElement("div");
      d.className = "item" + (i === this.sel ? " sel" : "");
      d.innerHTML = `<span class="t"></span><span class="d"></span>`;
      d.querySelector(".t").textContent = it.text;
      d.querySelector(".d").textContent = it.description || "";
      d.onmousedown = (e) => { e.preventDefault(); this.sel = i; this.accept(); };
      this.list.appendChild(d);
    });
    this.list.querySelector(".sel")?.scrollIntoView({ block: "nearest" });
  }
  key(e) {
    if (e.key === "Escape") { e.preventDefault(); this.cancel(); }
    else if (e.key === "Enter") { e.preventDefault(); this.accept(); }
    else if (e.key === "ArrowDown") { e.preventDefault(); this.sel = Math.min(this.items.length - 1, this.sel + 1); this.renderList(); }
    else if (e.key === "ArrowUp") { e.preventDefault(); this.sel = Math.max(0, this.sel - 1); this.renderList(); }
  }
  close() {
    this.spec = null;
    this.allItems = null;
    this.el.classList.add("hidden");
    this.app.focusPane(this.app.activePane);
  }
  accept() {
    const s = this.spec;
    if (!s) return;
    const item = this.items[this.sel];
    const input = this.input.value;
    this.close();
    if (s.plugin) {
      const idx = item ? (this.allItems || this.items).indexOf(item) : null;
      this.app.plugins.fire("prompt_confirmed", { prompt_type: s.type, input: item && item.value != null ? String(item.value) : input, selected_index: idx != null && idx >= 0 ? idx : null });
    } else s.accept?.(item, input);
  }
  cancel(silent) {
    const s = this.spec;
    this.close();
    if (s?.plugin && !silent) this.app.plugins.fire("prompt_cancelled", { prompt_type: s.type, input: this.input.value });
  }
}

class App {
  constructor() {
    this.buffers = new Map();
    this.docs = new Map();
    this.metrics = { cw: 8, lh: 18 };
    this.config = { editor: { page_width: 80, large_file_threshold_bytes: 10 * 1024 * 1024 }, languages: {} };
    this.status = "";
    this.searchHit = null;
    this.lastSearch = "";
  }

  async start() {
    this.measure();
    const hello = await rpc("hello");
    session.client = hello.client;
    this.root = hello.root;
    this.host = hello.host;
    document.title = `${hello.name} — Fresh`;
    $("#ws-name").textContent = `${hello.name} · ${hello.host}`;
    connectEvents((ev) => this.onEvent(ev));
    this.prompt = new Prompt(this);
    this.plugins = new Plugins(this);
    this.term = new TerminalPanel(this, $("#terminal"));
    this.input = $("#input");
    this.input.addEventListener("keydown", (e) => this.onKey(e));
    this.input.addEventListener("input", () => this.onInput());
    this.input.addEventListener("paste", (e) => { e.preventDefault(); this.insertText(e.clipboardData.getData("text")); });
    this.input.addEventListener("copy", (e) => this.onCopy(e, false));
    this.input.addEventListener("cut", (e) => this.onCopy(e, true));
    this.root_pane = new Pane(this);
    this.layoutTree = { pane: this.root_pane };
    this.activePane = this.root_pane;
    this.relayout();
    new ResizeObserver(() => this.renderAll()).observe($("#editor-area"));
    this.explorer();
    await this.plugins.loadAll();
    this.welcome();
    await this.term.adopt(hello.terminals);
    this.plugins.fire("editor_initialized", {});
    this.plugins.fire("focus_gained", {});
    const open = new URLSearchParams(location.search).get("open");
    if (open) this.openFile(open);
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
  focusPane(p) {
    if (!p) return;
    const changed = this.activePane !== p;
    this.activePane = p;
    for (const x of this.panes()) x.el.classList.toggle("active", x === p);
    this.input.focus({ preventScroll: true });
    if (changed) { this.renderAll(); if (p.view) this.plugins.fire("buffer_activated", { buffer_id: p.view.buf.id, window_id: 1 }); }
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
    np.show(buf || old.view?.buf || this.welcomeBuf);
    this.activePane = np;
    this.relayout();
    this.focusPane(np);
    return np;
  }
  splitWith(buf, ratio) { return this.split("v", ratio, buf); }
  closePane(p) {
    if (this.panes().length === 1) return;
    const remove = (n) => {
      if (n.pane) return n;
      if (n.a.pane === p) return n.b;
      if (n.b.pane === p) return n.a;
      return { ...n, a: remove(n.a), b: remove(n.b) };
    };
    this.layoutTree = remove(this.layoutTree);
    for (const v of [...p.views]) v.dispose();
    this.activePane = this.panes()[0];
    this.relayout();
    this.focusPane(this.activePane);
  }
  closeTab(p, v) {
    p.closeView(v);
    const b = v.buf;
    const stillShown = this.panes().some((x) => x.views.some((y) => y.buf === b));
    if (!stillShown) this.closeBuffer(b.id, true);
    if (!p.views.length) { if (this.panes().length > 1) this.closePane(p); else p.show(this.welcomeBuf); }
    this.updateStatus();
  }
  closeBuffer(id, fromTab) {
    const b = this.buffers.get(id);
    if (!b) return;
    if (!fromTab) for (const p of this.panes()) for (const v of [...p.views]) if (v.buf === b) this.closeTab(p, v);
    if (b === this.welcomeBuf) return;
    this.buffers.delete(id);
    if (b.doc) { this.docs.delete(b.doc); rpc("doc.close", { doc: b.doc }).catch(() => {}); }
    this.plugins.fire("buffer_closed", { buffer_id: id, window_id: 1 });
  }
  onViewShown(pane, v) {
    if (pane === this.activePane) this.plugins?.fire("buffer_activated", { buffer_id: v.buf.id, window_id: 1 });
    this.updateStatus();
  }

  // ------------------------------------------------------------- buffers

  bufferInfo(b) {
    const views = [...b.views];
    const v = this.activePane?.view?.buf === b ? this.activePane.view : views[0];
    return {
      id: b.id, window_id: 1, path: b.path || "", name: b.name, modified: !!b.modified, length: b.len,
      line_count: b.totalLines ?? null, is_virtual: b.isVirtual,
      view_mode: v?.compose ? "compose" : "source",
      is_composing_in_any_split: views.some((x) => x.compose),
      compose_width: v?.composeWidth ?? null,
      language: b.language || null,
      splits: this.panes().filter((p) => p.views.some((x) => x.buf === b)).map((p) => p.id),
    };
  }

  watchBuffer(b) {
    b.listeners.add((what, data) => {
      if (what === "edit") this.afterEdit(b, data);
      if (what === "text") this.renderBuffer(b.id);
      if (what === "error") this.setStatus(String(data));
    });
  }

  async openFile(path, { line, column, pane } = {}) {
    try {
      const abs = path.startsWith("/") ? path : `${this.root}/${path}`;
      let b = [...this.buffers.values()].find((x) => x.path === abs);
      if (!b) {
        const info = await rpc("doc.open", { path: abs });
        b = this.docs.get(info.doc);
        if (!b) {
          b = new FileBuffer(info);
          this.buffers.set(b.id, b);
          this.docs.set(b.doc, b);
          this.watchBuffer(b);
          await b.ensure(0);
          this.plugins.fire("after_file_open", { path: b.path, buffer_id: b.id, window_id: 1 });
        }
      }
      const p = pane || this.activePane;
      const v = p.show(b);
      this.focusPane(p);
      if (line != null) await this.gotoLine(line - 1, column != null ? column - 1 : 0);
      this.updateStatus();
      return v;
    } catch (e) {
      this.setStatus(String(e.message || e));
    }
  }

  createVirtualBuffer(o) {
    const b = new VirtualBuffer(o.name, { mode: o.mode, readOnly: o.readOnly });
    b.setEntries(o.entries || []);
    this.buffers.set(b.id, b);
    this.watchBuffer(b);
    if (o.showLineNumbers === false) b.noLineNumbers = true;
    return b;
  }

  welcome() {
    const keys = [
      ["Ctrl+P", "open a file (searched on the server)"], ["Ctrl+Shift+P / F1", "command palette (built-in and plugin commands)"],
      ["Ctrl+S", "save"], ["Ctrl+Z / Ctrl+Y", "undo / redo (this client's own edits)"], ["Ctrl+F, F3", "find (server-side, works on huge files)"],
      ["Ctrl+G", "go to line"], ["Ctrl+`", "terminal (xterm.js over the server's PTY)"], ["Ctrl+\\", "split right"],
      ["F12 / Ctrl+Space", "go to definition / completion (language server)"], ["Ctrl+W", "close tab"],
    ];
    const entries = [
      { text: "Fresh — server for data, browser for everything else\n\n", style: { fg: "syntax.keyword", bold: true } },
      { text: "The server holds the workspace's files, each open document's ordered edit log,\nterminals, processes and one connection per language server. This page does the\nrest: layout, editing, undo, decorations, and the editor's own plugins, unchanged.\n\n" },
      ...keys.map(([k, d]) => ({ text: `  ${k.padEnd(20)} ${d}\n`, properties: { key: k } })),
      { text: "\nOpen two browser tabs on the same file to see concurrent editing.\nOpen a .md file and run “Markdown: Toggle Compose” from the palette.\n" },
    ];
    this.welcomeBuf = this.createVirtualBuffer({ name: "Welcome", readOnly: true, entries });
    this.welcomeBuf.noLineNumbers = true;
    this.activePane.show(this.welcomeBuf);
    this.activePane.view.lineNumbers = false;
  }

  // ------------------------------------------------------------- edits

  applyEdit(b, ops) {
    if (b.readOnly) { this.setStatus("Read-only buffer"); return; }
    b.edit(ops);
  }

  afterEdit(b, { op, gone }) {
    const L = b.text;
    const lineOf = (p) => { const i = L ? L.indexOf(p) : -1; return i >= 0 ? (L.firstLine ?? 0) + i : 0; };
    if (op.t === "ins") {
      const len = byteLen(op.text);
      this.plugins.fire("after_insert", {
        buffer_id: b.id, window_id: 1, position: op.pos, text: op.text, affected_start: op.pos, affected_end: op.pos + len,
        start_line: lineOf(op.pos), end_line: lineOf(op.pos + len), lines_added: nl(op.text),
      });
    } else {
      this.plugins.fire("after_delete", {
        buffer_id: b.id, window_id: 1, start: op.pos, end: op.pos + op.len, deleted_text: gone ?? "", affected_start: op.pos,
        deleted_len: op.len, start_line: lineOf(op.pos), end_line: lineOf(op.pos), lines_removed: gone ? nl(gone) : 0,
      });
    }
    this.plugins.fire("buffer_modified", { buffer_id: b.id, window_id: 1 });
    this.renderBuffer(b.id);
    for (const p of this.panes()) if (p.views.some((v) => v.buf === b)) p.renderTabs();
  }

  cursorMoved(v, old) {
    const b = v.buf;
    const L = b.text;
    this.plugins.fire("cursor_moved", {
      buffer_id: b.id, window_id: 1, cursor_id: 0, old_position: old, new_position: v.cursor,
      line: L ? (L.firstLine ?? 0) + Math.max(0, L.indexOf(v.cursor)) : 0,
      text_properties: b.propsAt ? b.propsAt(v.cursor) : [],
    });
    this.hideHover();
    this.updateStatus();
  }

  // ------------------------------------------------------------- server events

  onEvent(ev) {
    switch (ev.ev) {
      case "doc.ops": {
        const b = this.docs.get(ev.doc);
        if (b) { b.onOps(ev, session.client); this.renderBuffer(b.id); }
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
        // edit applied since.
        let items = ev.items.map((d) => ({ ...d }));
        for (const e of b.recent) {
          if (e.version <= ev.version) continue;
          for (const op of e.ops) for (const d of items) {
            d.start = shift(d.start, op, true);
            d.end = Math.max(d.start, shift(d.end, op, false));
          }
        }
        for (const op of [...(b.inflight || []), ...b.pending]) for (const d of items) {
          d.start = shift(d.start, op, true);
          d.end = Math.max(d.start, shift(d.end, op, false));
        }
        b.decor.diags = items;
        b.decor.touch();
        this.renderBuffer(b.id);
        this.updateStatus();
        break;
      }
      case "term.data": case "term.exit": case "term.size":
        this.term.onEvent(ev);
        break;
      default:
        this.plugins.onEvent(ev);
    }
  }

  // ------------------------------------------------------------- text input

  view() { return this.activePane?.view; }

  insertText(text) {
    const v = this.view();
    if (!v) return;
    if (v.buf.readOnly) return;
    const sel = v.selection();
    const ops = [];
    let at = v.cursor;
    if (sel) { ops.push({ t: "del", pos: sel.start, len: sel.end - sel.start }); at = sel.start; }
    ops.push({ t: "ins", pos: at, text });
    v.anchor = null;
    this.applyEdit(v.buf, ops);
    if (v.extraCursors.length) {
      const extra = [...v.extraCursors].sort((a, b) => b - a);
      for (const c of extra) this.applyEdit(v.buf, [{ t: "ins", pos: c, text }]);
    }
    this.activePane.reveal();
    this.activePane.scheduleRender();
  }

  onInput() {
    const t = this.input.value;
    this.input.value = "";
    if (t && !this.composing) this.insertText(t);
  }

  onCopy(e, cut) {
    const v = this.view();
    const sel = v?.selection();
    if (!sel) return;
    const t = v.buf.textRange(sel.start, sel.end);
    if (t == null) return;
    e.preventDefault();
    e.clipboardData.setData("text/plain", t);
    if (cut && !v.buf.readOnly) { v.anchor = null; this.applyEdit(v.buf, [{ t: "del", pos: sel.start, len: sel.end - sel.start }]); }
  }

  charBefore(b, p) {
    const L = b.text;
    if (!L || p <= 0) return null;
    const i = L.indexOf(p);
    if (i < 0) return null;
    const s = L.lines[i];
    const idx = byteToIdx(s, p - L.starts[i]);
    if (idx === 0) return "\n";
    const c = s.codePointAt(idx - 1);
    const ch = idx >= 2 && c >= 0xdc00 && c < 0xe000 ? s.slice(idx - 2, idx) : s[idx - 1];
    return ch;
  }
  charAt(b, p) {
    const L = b.text;
    if (!L || p >= b.len) return null;
    const i = L.indexOf(p);
    if (i < 0) return null;
    const s = L.lines[i];
    const idx = byteToIdx(s, p - L.starts[i]);
    if (idx >= s.length) return "\n";
    return String.fromCodePoint(s.codePointAt(idx));
  }
  wordEdge(b, p, dir) {
    const isW = (c) => c != null && /[\p{L}\p{N}_]/u.test(c);
    let q = p;
    const step = (x) => (dir < 0 ? this.charBefore(b, x) : this.charAt(b, x));
    while (step(q) != null && !isW(step(q)) && step(q) !== "\n") q += dir * byteLen(step(q));
    while (isW(step(q))) q += dir * byteLen(step(q));
    if (q === p && step(q) != null) q += dir * byteLen(step(q));
    return q;
  }
  selectWord(pane) {
    const v = pane.view;
    const s = this.wordEdge(v.buf, v.cursor, -1), e = this.wordEdge(v.buf, s, 1);
    v.anchor = s;
    v.cursor = e;
    pane.scheduleRender();
  }
  lineBounds(b, p) {
    const L = b.text;
    const i = L.indexOf(p);
    return { i, start: L.starts[i], end: L.lineEnd(i), text: L.lines[i] };
  }

  onKey(e) {
    const pane = this.activePane;
    const v = pane?.view;
    const k = (e.ctrlKey || e.metaKey ? "C-" : "") + (e.altKey ? "M-" : "") + (e.shiftKey && e.key.length > 1 ? "S-" : "") + e.key;
    // A plugin mode's bindings come first.
    const mode = v?.buf.mode && this.plugins.modes.get(v.buf.mode);
    if (mode) {
      const name = { Enter: "Return", Escape: "Escape", Tab: "Tab", " ": "Space" }[e.key] || e.key;
      const h = mode.bindings.get((e.ctrlKey ? "C-" : "") + name) || mode.bindings.get(name);
      if (h) { e.preventDefault(); this.plugins.call(mode.plugin, h); return; }
    }
    const act = this.keymap[k];
    if (act) { e.preventDefault(); this.runAction(act, e); return; }
    if (!v) return;
    const b = v.buf;
    const ext = e.shiftKey;
    switch (e.key) {
      case "ArrowLeft": e.preventDefault();
        if (e.ctrlKey) return pane.setCursor(this.wordEdge(b, v.cursor, -1), ext);
        if (!ext && v.selection()) return pane.setCursor(v.selection().start, false);
        return pane.setCursor(v.cursor - byteLen(this.charBefore(b, v.cursor) || ""), ext);
      case "ArrowRight": e.preventDefault();
        if (e.ctrlKey) return pane.setCursor(this.wordEdge(b, v.cursor, 1), ext);
        if (!ext && v.selection()) return pane.setCursor(v.selection().end, false);
        return pane.setCursor(v.cursor + byteLen(this.charAt(b, v.cursor) || ""), ext);
      case "ArrowUp": e.preventDefault(); return pane.moveRows(-1, ext);
      case "ArrowDown": e.preventDefault(); return pane.moveRows(1, ext);
      case "PageUp": e.preventDefault(); pane.scrollLines(-pane.metrics().rows); return pane.moveRows(-pane.metrics().rows, ext);
      case "PageDown": e.preventDefault(); pane.scrollLines(pane.metrics().rows); return pane.moveRows(pane.metrics().rows, ext);
      case "Home": {
        e.preventDefault();
        if (e.ctrlKey) return this.gotoByte(0, ext);
        const lb = this.lineBounds(b, v.cursor);
        const indent = lb.start + byteLen(/^\s*/.exec(lb.text)[0]);
        return pane.setCursor(v.cursor === indent ? lb.start : indent, ext);
      }
      case "End": {
        e.preventDefault();
        if (e.ctrlKey) return this.gotoByte(b.len, ext);
        return pane.setCursor(this.lineBounds(b, v.cursor).end, ext);
      }
      case "Escape": v.anchor = null; v.extraCursors = []; this.hideHover(); pane.scheduleRender(); return;
    }
    if (b.readOnly) return;
    switch (e.key) {
      case "Enter": {
        e.preventDefault();
        const lb = this.lineBounds(b, v.cursor);
        this.insertText("\n" + /^[ \t]*/.exec(lb.text)[0]);
        return;
      }
      case "Tab": e.preventDefault(); this.insertText("    "); return;
      case "Backspace": {
        e.preventDefault();
        const sel = v.selection();
        if (sel) { v.anchor = null; return this.applyEdit(b, [{ t: "del", pos: sel.start, len: sel.end - sel.start }]); }
        const c = this.charBefore(b, v.cursor);
        if (c == null) return;
        const n = e.ctrlKey ? v.cursor - this.wordEdge(b, v.cursor, -1) : byteLen(c);
        return this.applyEdit(b, [{ t: "del", pos: v.cursor - n, len: n }]);
      }
      case "Delete": {
        e.preventDefault();
        const sel = v.selection();
        if (sel) { v.anchor = null; return this.applyEdit(b, [{ t: "del", pos: sel.start, len: sel.end - sel.start }]); }
        const c = this.charAt(b, v.cursor);
        if (c == null) return;
        return this.applyEdit(b, [{ t: "del", pos: v.cursor, len: byteLen(c) }]);
      }
    }
  }

  get keymap() {
    return {
      "C-s": "save", "C-z": "undo", "C-y": "redo", "C-Z": "redo", "C-p": "quick_open", "C-P": "command_palette", F1: "command_palette",
      "C-f": "find", F3: "find_next", "S-F3": "find_previous", "C-g": "goto_line", "C-`": "toggle_terminal", "C-\\": "split_right",
      "C-w": "close_tab", F12: "goto_definition", "C- ": "completion", "C-a": "select_all", "C-k": "hover", "C-Tab": "next_tab",
      "C-PageDown": "next_tab", "C-PageUp": "prev_tab", "M-z": "toggle_wrap",
    };
  }

  // ------------------------------------------------------------- actions

  get actions() {
    const A = {
      save: async () => {
        const b = this.view()?.buf;
        if (!b || !b.doc) return;
        this.plugins.fire("before_file_save", { path: b.path, buffer_id: b.id, window_id: 1 });
        await b.save();
        this.setStatus(`Saved ${b.name}`);
        this.renderAll();
        this.plugins.fire("after_file_save", { path: b.path, buffer_id: b.id, window_id: 1 });
      },
      undo: () => { const v = this.view(); if (v) { const ops = v.buf.doUndo(); if (ops) this.cursorAfter(v, ops); } },
      redo: () => { const v = this.view(); if (v) { const ops = v.buf.doRedo(); if (ops) this.cursorAfter(v, ops); } },
      quick_open: () => this.prompt.open({
        label: "Open file:",
        source: async (q) => (await rpc("fs.find", { query: q })).files.map((f) => ({ text: f, value: f })),
        accept: (it, input) => this.openFile(it ? it.value : input),
      }),
      command_palette: () => this.prompt.open({
        label: "Command:",
        source: (q) => this.commandList().filter((c) => c.text.toLowerCase().includes(q.toLowerCase())),
        accept: (it) => it && it.run(),
      }),
      find: () => this.prompt.open({
        label: "Find:", initial: this.lastSearch,
        accept: (_it, input) => { this.lastSearch = input; this.find(input, false); },
      }),
      find_next: () => this.lastSearch && this.find(this.lastSearch, false),
      find_previous: () => this.lastSearch && this.find(this.lastSearch, true),
      goto_line: () => this.prompt.open({ label: "Go to line:", accept: (_i, input) => { const n = parseInt(input, 10); if (n > 0) this.gotoLine(n - 1, 0); } }),
      toggle_terminal: () => this.term.toggle(),
      new_terminal: async () => { if (!this.term.visible) await this.term.toggle(); await this.term.create(); },
      split_right: () => this.split("h"),
      split_down: () => this.split("v"),
      close_split: () => this.closePane(this.activePane),
      close_tab: () => { const p = this.activePane; if (p.view) this.closeTab(p, p.view); },
      next_tab: () => this.cycleTab(1),
      prev_tab: () => this.cycleTab(-1),
      select_all: () => { const v = this.view(); if (v) { v.anchor = 0; v.cursor = v.buf.len; this.activePane.scheduleRender(); } },
      goto_definition: () => this.gotoDefinition(),
      completion: () => this.completion(),
      hover: () => { const v = this.view(); const at = v && this.activePane.caretXY(v.cursor); if (at) { const r = this.activePane.textEl.getBoundingClientRect(); this.showHoverAt(this.activePane, v.cursor, r.left + at.x * this.metrics.cw, r.top + (at.r + 1) * this.metrics.lh, true); } },
      toggle_wrap: () => { const v = this.view(); if (v) { v.wrap = !v.wrap; this.activePane.scheduleRender(); } },
      toggle_line_numbers: () => { const v = this.view(); if (v) { v.lineNumbers = !(v.lineNumbers ?? true); this.activePane.scheduleRender(); } },
      show_help: () => this.activePane.show(this.welcomeBuf),
      show_plugin_log: () => {
        const b = this.createVirtualBuffer({ name: "*Plugin Log*", readOnly: true, entries: this.plugins.log.map((l) => ({ text: l + "\n" })) });
        this.activePane.show(b);
      },
      move_left: () => this.onKey(new KeyboardEvent("keydown", { key: "ArrowLeft" })),
      move_right: () => this.onKey(new KeyboardEvent("keydown", { key: "ArrowRight" })),
      move_up: () => this.activePane.moveRows(-1, false),
      move_down: () => this.activePane.moveRows(1, false),
      select_left: () => this.onKey(new KeyboardEvent("keydown", { key: "ArrowLeft", shiftKey: true })),
      select_right: () => this.onKey(new KeyboardEvent("keydown", { key: "ArrowRight", shiftKey: true })),
      move_line_start: () => this.onKey(new KeyboardEvent("keydown", { key: "Home" })),
      move_line_end: () => this.onKey(new KeyboardEvent("keydown", { key: "End" })),
    };
    return A;
  }
  runAction(name, e) {
    const f = this.actions[name];
    if (!f) { this.setStatus(`Action “${name}” is not available in the proof of concept`); return false; }
    f(e);
    return true;
  }
  commandList() {
    const builtin = [
      ["File: Save", "save"], ["File: Open…", "quick_open"], ["Edit: Undo", "undo"], ["Edit: Redo", "redo"], ["Find…", "find"],
      ["Go to Line…", "goto_line"], ["Terminal: Toggle", "toggle_terminal"], ["Terminal: New", "new_terminal"], ["View: Split Right", "split_right"],
      ["View: Split Down", "split_down"], ["View: Close Split", "close_split"], ["View: Toggle Line Wrap", "toggle_wrap"],
      ["View: Toggle Line Numbers", "toggle_line_numbers"], ["LSP: Go to Definition", "goto_definition"], ["LSP: Completion", "completion"],
      ["LSP: Hover", "hover"], ["Help: Welcome", "show_help"], ["Plugins: Show Log", "show_plugin_log"],
    ].map(([text, a]) => ({ text, description: "built-in", run: () => this.runAction(a) }));
    const fromPlugins = [...this.plugins.commands].map(([name, c]) => ({ text: name, description: `${c.description || ""} · ${c.source}`, run: () => this.plugins.runCommand(name) }));
    return [...builtin, ...fromPlugins].sort((a, b) => a.text.localeCompare(b.text));
  }
  cycleTab(d) {
    const p = this.activePane;
    if (!p.views.length) return;
    const i = (p.views.indexOf(p.view) + d + p.views.length) % p.views.length;
    p.show(p.views[i].buf);
  }
  cursorAfter(v, ops) {
    const last = ops[ops.length - 1];
    if (last) { v.anchor = null; this.activePane.setCursor(last.t === "ins" ? last.pos + byteLen(last.text) : last.pos); }
  }

  // ------------------------------------------------------------- navigation

  async gotoByte(p, extend) {
    const pane = this.activePane;
    const v = pane.view;
    await v.buf.ensure(p, pane.metrics().rows + 10);
    if (v.buf.text.indexOf(v.top) < 0 || Math.abs(v.top - p) > 0) {
      const i = v.buf.text.indexOf(p);
      if (i >= 0 && (p < v.top || p > v.top + 4096)) v.top = v.buf.text.starts[Math.max(0, i - 5)];
    }
    pane.setCursor(p, extend);
  }

  async gotoLine(line, col) {
    const v = this.view();
    if (!v) return;
    const b = v.buf;
    let p;
    if (b.text && b.full) p = b.text.starts[Math.min(line, b.text.count - 1)];
    else if (b.doc) {
      const r = await rpc("doc.line_start", { doc: b.doc, line });
      if (r.pos == null) { this.setStatus("Line numbers are not available for this huge file"); return; }
      p = r.pos;
    }
    if (p == null) return;
    await b.ensure(p, 100);
    const L = b.text;
    const i = L.indexOf(p);
    p += byteLen(L.lines[i].slice(0, col || 0));
    v.top = L.starts[Math.max(0, i - 5)];
    this.activePane.setCursor(p, false);
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

  async find(q, backward) {
    const v = this.view();
    if (!v || !q) return;
    const b = v.buf;
    let pos;
    const from = backward ? v.cursor : (this.searchHit && this.searchHit.pos === v.cursor ? v.cursor + 1 : v.cursor);
    if (b.doc && !b.full) pos = (await rpc("doc.search", { doc: b.doc, query: q, from, backward })).pos;
    else {
      const L = b.text;
      const all = L.lines.join("\n");
      const idx = byteToIdx(all, from - L.start);
      let i = backward ? all.lastIndexOf(q, idx - 1) : all.indexOf(q, idx);
      if (i < 0) i = backward ? all.lastIndexOf(q) : all.indexOf(q);
      pos = i < 0 ? null : L.start + byteLen(all.slice(0, i));
    }
    if (pos == null) { this.setStatus(`Not found: ${q}`); this.searchHit = null; return; }
    this.searchHit = { buf: b, pos, len: byteLen(q) };
    await this.gotoByte(pos);
    this.setStatus(`Found “${q}”`);
  }

  // ------------------------------------------------------------- language server

  async gotoDefinition() {
    const v = this.view();
    if (!v?.buf.doc) return;
    try {
      const r = await rpc("lsp.definition", { doc: v.buf.doc, pos: v.cursor });
      const loc = r.locations[0];
      if (!loc) { this.setStatus("No definition found"); return; }
      await this.openFile(loc.path);
      await this.gotoLine(loc.line, loc.character);
    } catch (e) { this.setStatus(e.message); }
  }

  async completion() {
    const v = this.view();
    if (!v?.buf.doc) return;
    const pane = this.activePane;
    try {
      const r = await rpc("lsp.completion", { doc: v.buf.doc, pos: v.cursor });
      const start = this.wordEdge(v.buf, v.cursor, -1);
      const prefix = v.buf.textRange(start, v.cursor) || "";
      this.prompt.open({
        label: "Complete:", initial: prefix,
        source: (q) => r.items.filter((i) => String(i.label).toLowerCase().startsWith(q.toLowerCase())).map((i) => ({ text: String(i.label), description: i.detail || "", value: i.insert })),
        accept: (it) => {
          if (!it) return;
          const s = this.wordEdge(v.buf, v.cursor, -1);
          this.applyEdit(v.buf, [{ t: "del", pos: s, len: v.cursor - s }, { t: "ins", pos: s, text: it.value }]);
          pane.scheduleRender();
        },
      });
    } catch (e) { this.setStatus(e.message); }
  }

  onHover(pane, p, x, y) {
    clearTimeout(this.hoverTimer);
    if (p == null) return;
    this.hoverTimer = setTimeout(() => this.showHoverAt(pane, p, x, y + this.metrics.lh, false), 600);
  }
  async showHoverAt(pane, p, x, y, force) {
    const b = pane.view?.buf;
    if (!b) return;
    const msgs = b.decor.diags.filter((d) => p >= d.start && p <= Math.max(d.end, d.start + 1)).map((d) => `${["", "error", "warning", "info", "hint"][d.severity] || "error"}: ${d.message}`);
    let text = msgs.join("\n");
    if (b.doc && (force || !text)) {
      try {
        const r = await rpc("lsp.hover", { doc: b.doc, pos: p });
        if (r.text) text = text ? `${text}\n\n${r.text}` : r.text;
      } catch {}
    }
    if (!text) return;
    const h = $("#hover");
    h.textContent = text;
    h.style.left = `${x}px`;
    h.style.top = `${y}px`;
    h.classList.remove("hidden");
  }
  hideHover() { $("#hover").classList.add("hidden"); }

  // ------------------------------------------------------------- status

  setStatus(m) { this.status = m; this.updateStatus(); }
  updateStatus() {
    const v = this.view();
    const left = $("#status-left"), right = $("#status-right");
    if (!left) return;
    left.textContent = this.status;
    const parts = [];
    if (v) {
      const b = v.buf;
      const L = b.text;
      const i = L ? L.indexOf(v.cursor) : -1;
      if (i >= 0) {
        const col = [...L.lines[i].slice(0, byteToIdx(L.lines[i], v.cursor - L.starts[i]))].length + 1;
        parts.push(L.firstLine != null ? `Ln ${L.firstLine + i + 1}, Col ${col}` : `Byte ${v.cursor}`);
      }
      if (b.large) parts.push("huge file (windowed)");
      if (b.language) parts.push(b.language);
      if (b.decor.diags.length) parts.push(`⚠ ${b.decor.diags.length}`);
      if (v.compose) parts.push("compose");
      for (const [, it] of this.plugins?.statusItems || []) { const val = it.values.get(b.id); if (val) parts.push(val); }
      if (b.doc) parts.push(b.inflight || b.pending.length ? `v${b.version}…` : `v${b.version}`);
    }
    parts.push(this.host);
    right.textContent = parts.join("  ·  ");
  }

  // ------------------------------------------------------------- explorer

  explorer() {
    const root = $("#explorer-tree");
    const node = (entry, depth) => {
      const d = document.createElement("div");
      d.className = "node";
      const row = document.createElement("div");
      row.className = "entry" + (entry.dir ? " dir" : "");
      row.style.paddingLeft = `${depth * 12 + 6}px`;
      row.textContent = `${entry.dir ? "▸ " : "  "}${entry.name}`;
      row.title = entry.path;
      d.appendChild(row);
      let open = false, kids = null;
      row.onmousedown = async (e) => {
        e.preventDefault();
        if (!entry.dir) { this.openFile(entry.path); return; }
        open = !open;
        row.textContent = `${open ? "▾ " : "▸ "}${entry.name}`;
        if (open) {
          kids = document.createElement("div");
          const { entries } = await rpc("fs.list", { path: entry.path });
          for (const c of entries) kids.appendChild(node(c, depth + 1));
          d.appendChild(kids);
        } else if (kids) { kids.remove(); kids = null; }
      };
      return d;
    };
    rpc("fs.list", {}).then(({ entries }) => { for (const e of entries) root.appendChild(node(e, 0)); });
  }
}

function shift(p, op, right) {
  if (op.t === "ins") return p > op.pos || (p === op.pos && right) ? p + byteLen(op.text) : p;
  if (p >= op.pos + op.len) return p - op.len;
  return p > op.pos ? op.pos : p;
}

export { transformSeqs };
window.app = new App();
window.app.start();
