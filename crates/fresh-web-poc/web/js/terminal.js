// Terminals, drawn by xterm.js in this client from the byte stream the
// server's PTY produces. The server owns the process (on whatever host the
// workspace's authority is) and a replay buffer; every client runs its own
// emulator, so the terminal behaves exactly as xterm.js does anywhere.
//
// Output chunks carry their offset in the stream. A terminal attached late
// replays the server's buffer, which ends at a known offset, and then takes
// only the bytes after it, so nothing is lost or written twice.

import { setClass, escapeHtml } from "./dom.js";
import { rpc } from "./rpc.js";
import { found } from "./maybe.js";

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
export const STATE_MARK = new Map([["working", "* "], ["waiting", "● "], ["done", "✓ "], ["idle", "· "], ["unknown", "? "], ["exited", "✕ "]]);

// Base64 as bytes.
/** function decode(b64: String) => Uint8Array */
function decode(b64) {
  const text = atob(b64);
  const out = new Uint8Array(text.length);
  for (let i = 0; i < text.length; i++) out[i] = text.charCodeAt(i);
  return out;
}

// What a chunk of a terminal's stream (`bytes` from offset `at`) does,
// given the offset written so far (`end`; null before the replay came):
// wait in the queue, nothing (it is in the replay already), or write
// its new part and move the offset on.
/** type Arrival = {kind: "queue"} | {kind: "skip"} | {kind: "write", bytes: Uint8Array, end: Number} */
/** function arrival(end: Number | Null, at: Number, bytes: Uint8Array) => Arrival */
function arrival(end, at, bytes) {
  if (end === null) return { kind: "queue" };
  if (at + bytes.length <= end) return { kind: "skip" };
  return { kind: "write", bytes: at < end ? bytes.subarray(end - at) : bytes, end: at + bytes.length };
}
// A terminal's stream written up to `end`.
/** function streamAt(end: Number | Null) => {end: Number | Null} */
function streamAt(end) { return { end }; }
// Nothing written yet (a terminal's stream before its replay).
/** const NOT_ATTACHED: Number | Null */
const NOT_ATTACHED = null;

class Term {
  // `screens` holds it; `changed()` redraws the tabs (its name, title or
  // state changed).
  constructor(id, screens, changed) {
    this.changed = changed;
    this.id = id;
    this.stream = streamAt(NOT_ATTACHED); // the stream offset written so far
    this.queue = []; // chunks that arrived before the replay
    // What it is (the server says when it is attached): its name, whether
    // it runs an agent's command, the agent's state; the title its program
    // set; whether it has exited.
    this.name = "";
    this.agent = false;
    this.state = "";
    this.title = "";
    this.done = false;
    this.el = document.createElement("div");
    this.el.className = "xterm-host";
    screens.appendChild(this.el);
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

  // Wire the terminal to the server and the panel (once, after `new`):
  // `editorKey(e)` gives the editor its own keys (true: it took the key).
  listen(editorKey) {
    this.xt.onData((data) => { rpc("term.input", { term: this.id, data }); });
    this.xt.onBinary((data) => { rpc("term.input", { term: this.id, data }); });
    this.xt.onResize(({ cols, rows }) => { rpc("term.resize", { term: this.id, cols, rows }).catch(() => {}); });
    // The keys the editor keeps while a terminal has the keyboard.
    this.xt.attachCustomKeyEventHandler((e) => !editorKey(e));
    this.xt.onTitleChange((t) => { this.title = t; this.changed(); });
  }

  async attach() {
    const r = await rpc("term.attach", { term: this.id });
    // What it is: its name, and whether it runs an agent's command.
    this.name = r.name;
    this.agent = r.agent;
    this.state = r.state;
    this.xt.write(decode(r.data));
    this.stream = streamAt(r.end);
    for (const ev of this.queue) this.data(ev);
    this.queue = [];
    if (r.exited) this.exited();
  }

  data(ev) {
    const a = arrival(this.stream.end, ev.at, decode(ev.data));
    if (a.kind === "queue") this.queue.push(ev);
    if (a.kind !== "write") return;
    this.xt.write(a.bytes);
    this.stream = streamAt(a.end);
  }

  exited() {
    this.done = true;
    this.xt.write("\r\n\x1b[2m[process exited]\x1b[0m\r\n");
    this.changed();
  }

  show(on) {
    this.el.style.display = on ? "block" : "none";
    if (on) requestAnimationFrame(() => { try { this.fit.fit(); } catch {} this.xt.focus(); });
  }
}

export class TerminalPanel {
  // `editor`: what the panel asks of the editor (js/main.js).
  constructor(editor, el) {
    this.editor = editor;
    this.el = el;
    this.tabsEl = el.querySelector(".term-tabs");
    this.screens = el.querySelector(".term-screens");
    this.terms = new Map(); // its terminals, by id
    this.selected = []; // the terminal shown: a list of at most one id
    this.visible = false;
    this.closed = new Set(); // ids closed here: their last output is not a new terminal
  }

  // The terminal shown, if any.
  get current() { return this.selected.at(0); }

  // Refit the terminals as the panel resizes (once, after `new`).
  listen() {
    new ResizeObserver(() => this.refit()).observe(this.screens);
    this.tabsEl.addEventListener("mousedown", (e) => { this.tabPress(e); });
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
      for (const t of found(this.terms.get(s.id))) if (t.state !== s.state) { t.state = s.state; changed = true; }
    }
    if (changed) this.renderTabs();
  }

  async add(id) {
    for (const had of found(this.terms.get(id))) return had;
    const t = new Term(id, this.screens, () => { this.renderTabs(); });
    t.listen(this.editor.key);
    this.terms.set(id, t);
    await t.attach();
    if (!this.selected.length) this.select(id);
    this.renderTabs();
    return t;
  }

  // A new terminal: the shell, or `command` in it (an agent session;
  // `resume` takes it up again later), named `name` (null: its own).
  async create(name, command, resume) {
    const id = (await rpc("term.open", { cols: 100, rows: 24, name, command, resume })).term;
    await this.add(id);
    this.select(id);
    return id;
  }

  select(id) {
    this.selected = [id];
    this.terms.forEach((t, k) => { t.show(k === id && this.visible); });
    this.renderTabs();
  }

  renderTabs() {
    const html = [];
    this.terms.forEach((t, id) => {
      const mark = (t.done ? STATE_MARK.get("exited") : t.agent ? STATE_MARK.get(t.state) : undefined) ?? "";
      const label = escapeHtml(`${mark}${t.title || (t.agent ? t.name : `Terminal ${id}`)}`);
      html.push(`<div class="tab${this.selected.includes(id) ? " active" : ""}" data-term="${id}" title="${label}"><span class="label">${label}</span><span class="x">×</span></div>`);
    });
    html.push(`<div class="tab plus" title="New terminal">+</div>`);
    this.tabsEl.innerHTML = html.join("");
  }

  // A press on the tabs: a tab shows its terminal, its × closes it, + makes
  // a new one.
  tabPress(e) {
    const tab = e.target.closest(".tab");
    if (tab === null) return;
    e.preventDefault();
    const term = tab.dataset.term;
    if (term === undefined) { this.create(null, null, null); return; }
    if (e.target.closest(".x") !== null) { e.stopPropagation(); this.close(Number(term)); } else this.select(Number(term));
  }

  close(id) {
    for (const t of found(this.terms.get(id))) {
      this.closed.add(id);
      rpc("term.close", { term: id }).catch((_e) => {});
      t.xt.dispose();
      t.el.remove();
      this.terms.delete(id);
    }
    if (this.selected.includes(id)) this.selected = [...this.terms.keys()].slice(0, 1);
    for (const s of this.selected) this.select(s);
    this.renderTabs();
  }

  async toggle() {
    // A first terminal before the panel opens: when the workspace refuses
    // one (restricted), the panel stays closed.
    if (!this.visible && !this.terms.size) {
      try { await this.create(null, null, null); } catch (e) { this.editor.say(`No terminal: ${String(e)}`); return; }
    }
    this.visible = !this.visible;
    setClass(this.el, "hidden", !this.visible);
    if (this.visible) {
      for (const s of this.selected) this.select(s);
    } else this.editor.focus();
    this.editor.relayout();
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
    for (const s of this.selected) {
      if (this.visible) for (const t of found(this.terms.get(s))) try { t.fit.fit(); } catch {}
    }
  }

  onEvent(ev) {
    if (ev.ev === "term.data") {
      const t = found(this.terms.get(ev.term));
      for (const x of t) x.data(ev);
      if (!t.length && !this.closed.has(ev.term)) this.add(ev.term); // another client opened it: show it here too
    } else if (ev.ev === "term.exit") {
      for (const x of found(this.terms.get(ev.term))) x.exited();
    }
  }
}
