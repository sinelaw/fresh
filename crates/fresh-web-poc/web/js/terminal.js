// Terminals, drawn by xterm.js in this client from the byte stream the
// server's PTY produces. The server owns the process (on whatever host the
// workspace's authority is) and a replay buffer; every client runs its own
// emulator, so the terminal behaves exactly as xterm.js does anywhere.
//
// Output chunks carry their offset in the stream. A terminal attached late
// replays the server's buffer, which ends at a known offset, and then takes
// only the bytes after it, so nothing is lost or written twice.

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
/** type Arrival =
 *   | {kind: "queue"}
 *   | {kind: "skip"}
 *   | {kind: "write", bytes: Uint8Array, end: Number}
 */
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
  // `changed()`: its label changed (its title, its agent's state, or it
  // exited).
  constructor(id, changed) {
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
    // Its screen, put in a tab's box (`mount`); it fits itself to it.
    this.el = document.createElement("div");
    this.el.className = "xterm-host";
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

  // Wire the terminal to the server (once, after `new`): `editorKey(e)`
  // gives the editor its own keys (true: it took the key).
  listen(editorKey) {
    this.xt.onData((data) => { rpc("term.input", { term: this.id, data }); });
    this.xt.onBinary((data) => { rpc("term.input", { term: this.id, data }); });
    this.xt.onResize(({ cols, rows }) => { rpc("term.resize", { term: this.id, cols, rows }).catch(() => {}); });
    // The keys the editor keeps while a terminal has the keyboard.
    this.xt.attachCustomKeyEventHandler((e) => !editorKey(e));
    this.xt.onTitleChange((t) => { this.title = t; this.changed(); });
    new ResizeObserver(() => { this.refit(); }).observe(this.el);
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

  // What its tab says: the agent's state or that it exited, then the
  // title its program set (else its name).
  get label() {
    const mark = (this.done ? STATE_MARK.get("exited") : this.agent ? STATE_MARK.get(this.state) : undefined) ?? "";
    return `${mark}${this.title || (this.agent ? this.name : `Terminal ${this.id}`)}`;
  }

  // Fit it to its box, while it is on screen.
  refit() {
    if (this.el.offsetParent === null) return;
    try { this.fit.fit(); } catch {}
  }
}

// The workspace's terminals. Each is shown as a tab (a terminal buffer,
// buffer.js): `mount` puts its screen in the box of the split showing it.
export class Terminals {
  // `editor`: what terminals ask of the editor (js/main.js).
  constructor(editor) {
    this.editor = editor;
    this.terms = new Map(); // by id
    this.closed = new Set(); // ids closed here: their last output is not a new terminal
    this.wanted = []; // the one to take the keyboard once on screen: a list of at most one id
  }

  // Terminals the server already runs (from another client, or before a
  // reload) are attached as they are.
  async adopt(terms) {
    for (const t of terms) await this.add(t.id);
  }

  ids() { return [...this.terms.keys()]; }
  // The ids of those running an agent's command.
  agents() { return [...this.terms.values()].filter((t) => t.agent).map((t) => t.id); }
  has(id) { return this.terms.has(id); }
  label(id) { return found(this.terms.get(id)).map((t) => t.label).at(0) ?? `Terminal ${id}`; }

  // The terminals' states from the workspace's status: an agent session's
  // tab shows its own.
  states(terms) {
    const ed /*: TerminalsEditor */ = this.editor;
    for (const s of terms) {
      for (const t of found(this.terms.get(s.id))) if (t.state !== s.state) { t.state = s.state; ed.changed(t.id); }
    }
  }

  async add(id) {
    for (const had of found(this.terms.get(id))) return had;
    const ed /*: TerminalsEditor */ = this.editor;
    const t = new Term(id, () => { ed.changed(id); });
    t.listen((e) => ed.key(e));
    this.terms.set(id, t);
    await t.attach();
    return t;
  }

  // A new terminal: the shell, or `command` in it (an agent session;
  // `resume` takes it up again later), named `name` (null: its own).
  async create(name, command, resume) {
    const id = (await rpc("term.open", { cols: 100, rows: 24, name, command, resume })).term;
    await this.add(id);
    return id;
  }

  // Put terminal `id`'s screen in `box` (taking it from wherever it was);
  // it takes the keyboard if it was asked to.
  mount(id, box) {
    for (const t of found(this.terms.get(id))) {
      if (t.el.parentElement !== box) box.replaceChildren(t.el);
      requestAnimationFrame(() => {
        t.refit();
        if (this.wanted.includes(id)) { this.wanted = []; t.xt.focus(); }
      });
    }
  }
  // Give terminal `id` the keyboard: now if it is on screen, else when it
  // is mounted.
  focus(id) {
    this.wanted = [id];
    for (const t of found(this.terms.get(id))) {
      if (t.el.offsetParent !== null) requestAnimationFrame(() => { if (this.wanted.includes(id)) { this.wanted = []; t.refit(); t.xt.focus(); } });
    }
  }
  // None is to take the keyboard (the editor has it).
  unfocus() { this.wanted = []; }
  // Type `data` into it, as if pasted.
  send(id, data) { return rpc("term.input", { term: id, data }); }

  close(id) {
    for (const t of found(this.terms.get(id))) {
      this.closed.add(id);
      rpc("term.close", { term: id }).catch((_e) => {});
      t.xt.dispose();
      t.el.remove();
      this.terms.delete(id);
    }
  }

  applyTheme() {
    const theme = xtermTheme();
    for (const t of this.terms.values()) t.xt.options.theme = theme;
  }

  applyFont() {
    const f = termFont();
    for (const t of this.terms.values()) { Object.assign(t.xt.options, f); t.refit(); }
  }

  onEvent(ev /*: TermEvent */) {
    const ed /*: TerminalsEditor */ = this.editor;
    if (ev.ev === "term.data") {
      const t = found(this.terms.get(ev.term));
      for (const x of t) x.data(ev);
      // Another client opened it: it shows here too.
      if (t.length === 0 && !this.closed.has(ev.term)) this.add(ev.term).then((_t) => { ed.appeared(ev.term); });
    } else if (ev.ev === "term.exit") {
      for (const x of found(this.terms.get(ev.term))) x.exited();
    }
  }
}
