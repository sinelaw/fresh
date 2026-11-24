// Terminals, drawn by xterm.js in this client from the byte stream the
// server's PTY produces. The server owns the process (on whatever host the
// workspace's authority is) and a replay buffer; every client runs its own
// emulator, so the terminal behaves exactly as xterm.js does anywhere.
//
// Output chunks carry their offset in the stream. A terminal attached late
// replays the server's buffer, which ends at a known offset, and then takes
// only the bytes after it, so nothing is lost or written twice.

import { rpc } from "./rpc.js";

// The terminal's colours, from the theme's variables (xterm draws on a
// canvas, so it takes values, not variables). Its ANSI colours are kept
// readable on any background by xterm's contrast floor.
const xtermTheme = () => {
  const css = getComputedStyle(document.documentElement);
  const v = (name) => css.getPropertyValue(name).trim();
  return { background: v("--terminal-bg"), foreground: v("--terminal-fg"), cursor: v("--caret"), selectionBackground: v("--selection") };
};

// The terminal's font: the editor's (the skin sets its size).
const termFont = () => {
  const css = getComputedStyle(document.body);
  // The size as the editor's text has it (the skin's density, the zoom).
  const surface = document.querySelector(".surface");
  return { fontFamily: css.getPropertyValue("--mono"), fontSize: parseFloat(getComputedStyle(surface || document.body).fontSize) || 13 };
};

// How an agent session's tab shows what it is doing (the server reads it
// from the terminal's stream: `src/terminal.rs`).
export const STATE_MARK = { working: "* ", waiting: "● ", done: "✓ ", idle: "· ", unknown: "? ", exited: "✕ " };

const decode = (b64) => Uint8Array.from(atob(b64), (c) => c.charCodeAt(0));

class Term {
  constructor(panel, id) {
    this.panel = panel;
    this.id = id;
    this.end = null; // stream offset written so far; null until attached
    this.queue = []; // chunks that arrived before the replay
    this.el = document.createElement("div");
    this.el.className = "xterm-host";
    panel.screens.appendChild(this.el);
    this.xt = new Terminal({
      ...termFont(),
      cursorBlink: true,
      allowProposedApi: true,
      scrollback: 5000,
      theme: xtermTheme(),
      minimumContrastRatio: 4.5,
    });
    this.fit = new FitAddon.FitAddon();
    this.xt.loadAddon(this.fit);
    this.xt.open(this.el);
  }

  // Wire the terminal to the server and the panel (once, after `new`).
  listen() {
    const panel = this.panel;
    this.xt.onData((data) => rpc("term.input", { term: this.id, data }));
    this.xt.onBinary((data) => rpc("term.input", { term: this.id, data }));
    this.xt.onResize(({ cols, rows }) => rpc("term.resize", { term: this.id, cols, rows }).catch(() => {}));
    this.xt.attachCustomKeyEventHandler((e) => {
      // The keys the editor keeps while a terminal has the keyboard (the
      // terminal's toggle, moving between workspaces): `app.terminalKeys`.
      const app = panel.app;
      const k = e.type === "keydown" ? app.keyName(e) : null;
      if (!k || !app.terminalKeys.has(k)) return true;
      e.preventDefault();
      app.run(app.keymap()[k]);
      return false;
    });
    this.xt.onTitleChange((t) => { this.title = t; panel.renderTabs(); });
  }

  async attach() {
    const r = await rpc("term.attach", { term: this.id });
    // What it is: its name, and whether it runs an agent's command.
    this.name = r.name;
    this.agent = r.agent;
    this.state = r.state;
    this.xt.write(decode(r.data));
    this.end = r.end;
    for (const ev of this.queue) this.data(ev);
    this.queue = [];
    if (r.exited) this.exited();
  }

  data(ev) {
    if (this.end == null) { this.queue.push(ev); return; }
    let bytes = decode(ev.data);
    const at = ev.at;
    if (at + bytes.length <= this.end) return; // already in the replay
    if (at < this.end) bytes = bytes.subarray(this.end - at);
    this.xt.write(bytes);
    this.end = Math.max(this.end, at + decode(ev.data).length);
  }

  exited() {
    this.done = true;
    this.xt.write("\r\n\x1b[2m[process exited]\x1b[0m\r\n");
    this.panel.renderTabs();
  }

  show(on) {
    this.el.style.display = on ? "block" : "none";
    if (on) requestAnimationFrame(() => { try { this.fit.fit(); } catch {} this.xt.focus(); });
  }
}

export class TerminalPanel {
  constructor(app, el) {
    this.app = app;
    this.el = el;
    this.tabsEl = el.querySelector(".term-tabs");
    this.screens = el.querySelector(".term-screens");
    this.terms = new Map();
    this.current = null;
    this.visible = false;
    this.closed = new Set(); // ids closed here: their last output is not a new terminal
  }

  // Refit the terminals as the panel resizes (once, after `new`).
  listen() {
    new ResizeObserver(() => this.refit()).observe(this.screens);
  }

  // Terminals the server already runs (from another client, or before a
  // reload) are attached as they are.
  async adopt(terms) {
    for (const t of terms) await this.add(t.id);
  }

  // The terminals' states from the workspace's status: an agent session's
  // tab shows its own.
  states(terms) {
    let changed = false;
    for (const s of terms) {
      const t = this.terms.get(s.id);
      if (t && t.state !== s.state) { t.state = s.state; changed = true; }
    }
    if (changed) this.renderTabs();
  }

  async add(id) {
    if (this.terms.has(id)) return this.terms.get(id);
    const t = new Term(this, id);
    t.listen();
    this.terms.set(id, t);
    await t.attach();
    if (!this.current) this.select(id);
    this.renderTabs();
    return t;
  }

  // A new terminal: the shell, or `command` in it (an agent session).
  async create({ name, command } = {}) {
    const id = (await rpc("term.open", { cols: 100, rows: 24, name, command })).term;
    await this.add(id);
    this.select(id);
    return id;
  }

  select(id) {
    this.current = id;
    for (const [k, t] of this.terms) t.show(k === id && this.visible);
    this.renderTabs();
  }

  renderTabs() {
    this.tabsEl.innerHTML = "";
    for (const [id, t] of this.terms) {
      const b = document.createElement("div");
      b.className = "tab" + (id === this.current ? " active" : "");
      const label = document.createElement("span");
      label.className = "label";
      const mark = t.done ? STATE_MARK.exited : t.agent ? STATE_MARK[t.state] || "" : "";
      label.textContent = `${mark}${t.title || (t.agent ? t.name : `Terminal ${id}`)}`;
      b.title = label.textContent;
      b.appendChild(label);
      b.onmousedown = (e) => { e.preventDefault(); this.select(id); };
      const x = document.createElement("span");
      x.className = "x";
      x.textContent = "×";
      x.onmousedown = (e) => { e.preventDefault(); e.stopPropagation(); this.close(id); };
      b.appendChild(x);
      this.tabsEl.appendChild(b);
    }
    const plus = document.createElement("div");
    plus.className = "tab plus";
    plus.textContent = "+";
    plus.title = "New terminal";
    plus.onmousedown = (e) => { e.preventDefault(); this.create(); };
    this.tabsEl.appendChild(plus);
  }

  close(id) {
    const t = this.terms.get(id);
    if (!t) return;
    this.closed.add(id);
    rpc("term.close", { term: id }).catch(() => {});
    t.xt.dispose();
    t.el.remove();
    this.terms.delete(id);
    if (this.current === id) this.current = this.terms.keys().next().value ?? null;
    if (this.current != null) this.select(this.current);
    this.renderTabs();
  }

  async toggle() {
    // A first terminal before the panel opens: when the workspace refuses
    // one (restricted), the panel stays closed.
    if (!this.visible && !this.terms.size) {
      try { await this.create(); } catch (e) { this.app.setStatus(`No terminal: ${e.detail || e.message || e}`); return; }
    }
    this.visible = !this.visible;
    this.el.classList.toggle("hidden", !this.visible);
    if (this.visible) {
      this.select(this.current);
    } else this.app.focusPane(this.app.activePane);
    this.app.relayout();
  }

  applyTheme() {
    const theme = xtermTheme();
    for (const t of this.terms.values()) t.xt.options.theme = theme;
  }

  applyFont() {
    const f = termFont();
    for (const t of this.terms.values()) Object.assign(t.xt.options, f);
    this.refit();
  }

  refit() {
    const t = this.terms.get(this.current);
    if (t && this.visible) try { t.fit.fit(); } catch {}
  }

  onEvent(ev) {
    if (ev.ev === "term.data") {
      const t = this.terms.get(ev.term);
      if (t) t.data(ev);
      else if (!this.closed.has(ev.term)) this.add(ev.term); // another client opened it: show it here too
    } else if (ev.ev === "term.exit") {
      this.terms.get(ev.term)?.exited();
    }
  }
}
