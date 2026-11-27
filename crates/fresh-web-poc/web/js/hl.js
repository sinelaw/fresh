// Syntax highlighting: fresh-editor's grammar engine (syntect, the same
// grammars and the same scope-to-category table as the terminal editor),
// compiled to WebAssembly (crates/fresh-highlight-wasm).
//
//  * The engine keeps its own copy of each buffer's held text, fed only by
//    the buffer's change stream (Buffer.followText) through a channel. The
//    channel is in-process and delivers on the next slice; moving the
//    engine into a Web Worker swaps the channel and nothing else.
//  * It parses in slices of a fixed number of bytes: the lines a view asked
//    for first, then on to the end of each buffer.
//  * Its answers are category runs per line, for the version of the text
//    it had. Each buffer's Colours keeps the runs on the page and carries
//    them forward through changes the engine has not answered yet: text
//    typed since takes the category of the place it was typed into.
//  * A category is a theme key ("syntax.keyword", "editor.diff_add_bg"), so
//    a theme change is a CSS change.

import { listOf, found } from "./maybe.js";
import { byteLen } from "./utf8.js";
import { THEME } from "./highlight.js";
import { bracketsIn } from "./brackets.js";
import { errorNews } from "./buffer.js";
import { failure } from "./rpc.js";

// The categories brackets inside do not count.
const LITERAL = ["syntax.string", "syntax.comment"];
// Bytes parsed per slice.
const SLICE_BYTES = 32 * 1024;

// A run of one category on a line: byte offsets [s, e) from the line's start.
/** type Run = {s: Number, e: Number, c: Number} */
// What the page knows of a line's colours: exact (parsed from the true
// state), provisional (parsed from a guessed state, until the background
// pass reaches it), or carried (moved through an edit not yet answered).
/** type LineColours =
 *   | {state: "exact", runs: Run[]}
 *   | {state: "provisional", runs: Run[]}
 *   | {state: "carried", runs: Run[]}
 *   | {state: "unknown"} */
/** const UNKNOWN: LineColours */
const UNKNOWN = { state: "unknown" };
/** function carried(runs: Run[]) => LineColours */
function carried(runs) { return { state: "carried", runs }; }
/** function parsed(exact: Boolean, runs: Run[]) => LineColours */
function parsed(exact, runs) { return exact ? { state: "exact", runs } : { state: "provisional", runs }; }
// The category at a position, as a feature on the keystroke reads it.
/** type CategoryAt =
 *   | {kind: "category", key: String, exact: Boolean}
 *   | {kind: "plain", exact: Boolean}
 *   | {kind: "unknown"} */
/** function categoryOf(key: String, exact: Boolean) => CategoryAt */
function categoryOf(key, exact) { return { kind: "category", key, exact }; }
/** function plainAt(exact: Boolean) => CategoryAt */
function plainAt(exact) { return { kind: "plain", exact }; }

// ---------------------------------------------------------------- the module

const enc = new TextEncoder();

// The syntax engine's calls in the module (wasm.js; the `hl_*` ABI).
class Engine {
  constructor(wasm) {
    /** this.w: Wasm */
    this.w = wasm;
    this.x = wasm.x;
  }
  init(packdump) { this.w.withBytes(packdump, (p, n) => this.w.check(this.x.hl_init(p, n))); }
  categories() { return JSON.parse(this.w.str(this.x.hl_categories() >>> 0)); }
  open(doc, syntax, text) {
    this.w.withBytes(enc.encode(syntax), (sp, sn) => this.w.withBytes(enc.encode(text), (tp, tn) => this.w.check(this.x.hl_open(doc, sp, sn, tp, tn))));
  }
  close(doc) { this.w.check(this.x.hl_close(doc)); }
  edit(doc, at, removed, text) { this.w.withBytes(enc.encode(text), (p, n) => this.w.check(this.x.hl_edit(doc, at, removed, p, n))); }
  want(doc, from, to) { this.w.check(this.x.hl_want(doc, from, to)); }
  // Parse up to `budget` bytes: whether work remains. A grammar that failed
  // closes its document (take then fails for it).
  work(budget) { return this.x.hl_work(budget) === 1; }
  // The lines whose runs changed since the last take.
  take(doc) {
    const ptr = this.x.hl_take(doc) >>> 0;
    if (ptr === 0) throw this.w.failed();
    const w = (k) => this.w.u32(ptr + 4 * k);
    /** const out: {line: Number, exact: Boolean, runs: Run[]}[] */
    const out = [];
    let i = 2;
    for (let k = 0; k < w(1); k++) {
      const line = w(i), exact = (w(i + 1) & 1) === 1, n = w(i + 2);
      i += 3;
      /** const runs: Run[] */
      const runs = [];
      for (let r = 0; r < n; r++, i += 3) runs.push({ s: w(i), e: w(i) + w(i + 1), c: w(i + 2) });
      out.push({ line, exact, runs });
    }
    return out;
  }
}

// ---------------------------------------------------------------- the channel

// What travels to the engine: a buffer's text change, its syntax, or the
// lines a view wants.
/** type ToEngine =
 *   | {kind: "open", doc: Number, version: Number, syntax: String, text: String}
 *   | {kind: "edit", doc: Number, version: Number, at: Number, removed: Number, text: String}
 *   | {kind: "close", doc: Number, version: Number}
 *   | {kind: "want", doc: Number, from: Number, to: Number} */
/** function openMsg(doc: Number, version: Number, syntax: String, text: String) => ToEngine */
function openMsg(doc, version, syntax, text) { return { kind: "open", doc, version, syntax, text }; }
/** function editMsg(
 *   doc: Number,
 *   version: Number,
 *   at: Number,
 *   removed: Number,
 *   text: String,
 * ) => ToEngine */
function editMsg(doc, version, at, removed, text) { return { kind: "edit", doc, version, at, removed, text }; }
/** function closeMsg(doc: Number, version: Number) => ToEngine */
function closeMsg(doc, version) { return { kind: "close", doc, version }; }
/** function wantMsg(doc: Number, from: Number, to: Number) => ToEngine */
function wantMsg(doc, from, to) { return { kind: "want", doc, from, to }; }

// A change seen on the page, by line, for carrying late answers forward.
/** type LineChange =
 *   | {kind: "lines", version: Number, line: Number, removed: Number, added: Number}
 *   | {kind: "reset", version: Number} */
/** function resetChange(version: Number) => LineChange */
function resetChange(version) { return { kind: "reset", version }; }
/** function linesChange(
 *   version: Number,
 *   line: Number,
 *   removed: Number,
 *   added: Number,
 * ) => LineChange */
function linesChange(version, line, removed, added) { return { kind: "lines", version, line, removed, added }; }

// ---------------------------------------------------------------- a buffer's colours

// The colours the page has for one buffer's held lines.
export class Colours {
  constructor(categories) {
    this.keys = categories; // theme key by category index
    /** this.lines: LineColours[] */
    this.lines = [];
    /** this.lens: Number[] */
    this.lens = [];
    this.version = 0;
    /** this.pending: LineChange[] */
    this.pending = [];
    this.gen = 0; // bumped on every change to what is known
    this.spanCache = new WeakMap();
    this.bracketCache = new WeakMap();
  }

  // The held text replaced (none: not held): nothing is known of it yet.
  reset(version, L) {
    this.lines = L === null ? [] : L.lens.map(() => UNKNOWN);
    this.lens = L === null ? [] : [...L.lens];
    this.version = version;
    this.gen++;
    this.pending.push(resetChange(version));
  }

  // An edit to the held text (`L`, as it is after it): carry the colours
  // through it.
  edit(c, L) {
    const line = L.indexOf(L.start + c.at);
    const col = c.at - (L.starts[line] - L.start);
    // The old lines the removed bytes reach, and where in the last one.
    let k = 0, endCol = col + c.removed;
    while (line + k < this.lens.length && endCol > this.lens[line + k]) { endCol -= this.lens[line + k] + 1; k++; }
    const segs = c.text.split("\n");
    const added = segs.length - 1;
    const first = this.lines[line] ?? UNKNOWN, last = this.lines[line + k] ?? UNKNOWN;
    const cat = categoryBefore(first, col);
    const typed = (s, len) => (cat === null || len === 0 ? [] : [{ s, e: s + len, c: cat }]);
    const head = runsOf(first).flatMap((r) => (r.s < col ? [{ s: r.s, e: Math.min(r.e, col), c: r.c }] : []));
    const tail = (shift) => runsOf(last).flatMap((r) => (r.e > endCol ? [{ s: Math.max(r.s, endCol) + shift, e: r.e + shift, c: r.c }] : []));
    const known = first.state !== "unknown" && last.state !== "unknown";
    /** const made: LineColours[] */
    const made = [];
    if (added === 0) {
      const len = byteLen(c.text);
      made.push(known ? carried([...head, ...typed(col, len), ...tail(col + len - endCol)]) : UNKNOWN);
    } else {
      made.push(known ? carried([...head, ...typed(col, byteLen(segs[0]))]) : UNKNOWN);
      for (const s of segs.slice(1, -1)) made.push(known ? carried(typed(0, byteLen(s))) : UNKNOWN);
      const lastLen = byteLen(segs[added]);
      made.push(known ? carried([...typed(0, lastLen), ...tail(lastLen - endCol)]) : UNKNOWN);
    }
    this.lines.splice(line, k + 1, ...made);
    this.lens.splice(line, k + 1, ...L.lens.slice(line, line + added + 1));
    this.version = c.version;
    this.gen++;
    this.pending.push(linesChange(c.version, line, k, added));
  }

  // The engine's answer for `lines` at `version`: each line moved through
  // the changes made since (dropped when one of them touched it: the
  // engine answers it again).
  apply(version, lines) {
    const after = this.pending.filter((p) => p.version > version);
    this.pending = after;
    for (const r of lines) {
      for (const li of moved(r.line, after)) this.lines[li] = parsed(r.exact, r.runs);
    }
    this.gen++;
  }

  // Line `li` of held lines `L`: its syntax spans (UTF-16 indices into its
  // text) with their style.
  spans(li, L) {
    const lc = this.lines[li] ?? UNKNOWN;
    if (lc.state === "unknown") return [];
    const text = L.lines[li];
    const hit = this.spanCache.get(lc);
    if (hit !== undefined && hit.text === text) return hit.spans;
    const spans = utf16Runs(lc.runs, L, li).map((r) => ({ s: r.s, e: r.e, style: styleOf(this.keys[r.c]) }));
    this.spanCache.set(lc, { text, spans });
    return spans;
  }
  // Line `li`'s ranges (UTF-16) of the categories `keys` (e.g. strings and
  // comments).
  rangesOf(li, L, keys) {
    const lc = this.lines[li] ?? UNKNOWN;
    if (lc.state === "unknown") return [];
    return utf16Runs(lc.runs.filter((r) => keys.includes(this.keys[r.c])), L, li).map((r) => ({ s: r.s, e: r.e }));
  }
  // Line `li`'s brackets outside its strings and comments, in its
  // coloured part (past the cut, the text is plain: no bracket counts).
  brackets(li, L) {
    const lc = this.lines[li] ?? UNKNOWN;
    const text = L.lines[li];
    const hit = this.bracketCache.get(lc);
    if (hit !== undefined && hit.text === text) return hit.brackets;
    const brackets = bracketsIn(text, this.rangesOf(li, L, LITERAL), colouredUnits(L, li));
    this.bracketCache.set(lc, { text, brackets });
    return brackets;
  }
  // The category at byte `b` of line `li`.
  categoryAt(li, b) {
    const lc = this.lines[li] ?? UNKNOWN;
    if (lc.state === "unknown") return UNKNOWN_AT;
    const exact = lc.state === "exact";
    for (const x of lc.runs) if (b >= x.s && b < x.e) return categoryOf(this.keys[x.c], exact);
    return plainAt(exact);
  }
  // Whether every line in [from, to) is exact.
  exact(from, to) { return this.lines.slice(from, to).every((l) => l.state === "exact"); }
}
/** const UNKNOWN_AT: CategoryAt */
const UNKNOWN_AT = { kind: "unknown" };

// A line's runs (none while unknown).
/** function runsOf(lc: LineColours) => Run[] */
function runsOf(lc) { return lc.state === "unknown" ? [] : lc.runs; }
// The category typed text at byte `col` takes: the character's before it
// (or at it, at a line's start). None for plain text or an unknown line.
/** function categoryBefore(lc: LineColours, col: Number) => Number | Null */
function categoryBefore(lc, col) {
  const at = col > 0 ? col - 1 : col;
  return runsOf(lc).filter((r) => at >= r.s && at < r.e).map((r) => r.c).at(0) ?? null;
}
// Line `li` at an earlier version moved through `changes` (none: one of
// them touched it, or replaced the text).
/** function moved(li: Number, changes: LineChange[]) => Number[] */
function moved(li, changes) {
  let at = li;
  for (const p of changes) {
    if (p.kind === "reset") return [];
    if (at < p.line) continue;
    if (at <= p.line + p.removed) return [];
    at += p.added - p.removed;
  }
  return [at];
}
// Line `li`'s runs in bytes as UTF-16 index ranges into its text (held
// lines `L` convert them), in order.
/** function utf16Runs(runs: Run[], L: Lines, li: Number) => Run[] */
function utf16Runs(runs, L, li) {
  return [...runs].sort((x, y) => x.s - y.s).map((r) => ({ s: L.idxAt(li, r.s), e: L.idxAt(li, r.e), c: r.c }));
}
// The bytes of a line the engine colours (fresh-highlight's
// MAX_LINE_BYTES); past them a line is plain text.
const COLOURED_BYTES = 20000;
// How much of line `li` is coloured, in UTF-16 units: all of it, or up to
// the last character boundary within COLOURED_BYTES.
/** function colouredUnits(L: Lines, li: Number) => Number */
function colouredUnits(L, li) { return L.idxBefore(li, COLOURED_BYTES); }
// The style a category's theme key draws with: a syntax key colours the
// text; a diff key fills the row behind it.
/** const styles: Map<String, Drawn> */
const styles = new Map();
/** function styleOf(key: String) => Drawn */
function styleOf(key) {
  const hit = styles.get(key);
  if (hit !== undefined) return hit;
  /** const c: String */
  const c = THEME[key] ?? "";
  const fill = key.startsWith("editor.");
  const st = Object.freeze({ fg: fill ? "" : c, bg: fill ? c : "", bold: false, italic: false, underline: false, strike: false, mono: false, markup: false, wavy: "" });
  styles.set(key, st);
  return st;
}

// What the editor's features read of a buffer's syntax, over its held
// lines `L`: its colours (a list of at most one), or plain text without.
export class LineSyntax {
  constructor(colours, L) { this.colours = colours; this.L = L; }
  // Bumped whenever what it answers may change.
  get gen() { return this.colours.map((c) => c.gen).at(0) ?? 0; }
  // Line `li`'s spans: its colours' own (not a copy: a layout asks for
  // them at every cell).
  spans(li) {
    for (const c of this.colours) return c.spans(li, this.L);
    return [];
  }
  brackets(li) {
    for (const c of this.colours) return c.brackets(li, this.L);
    return bracketsIn(this.L.lines[li], [], colouredUnits(this.L, li));
  }
  // Line `li`'s UTF-16 ranges coloured as one of `keys`.
  ranges(li, keys) { return this.colours.flatMap((c) => c.rangesOf(li, this.L, keys)); }
  // The category at byte `p`.
  categoryAt(p) {
    const li = this.L.indexOf(p);
    for (const c of this.colours) if (li >= 0) return c.categoryAt(li, p - this.L.starts[li]);
    return this.colours.length === 0 ? plainAt(true) : UNKNOWN_AT;
  }
}

// The runs of bytes [start, end) of buffer `b` in byte offsets, with their
// theme keys, and the version they are for.
function runsIn(b, colours, start, end) {
  const L = b.held;
  const out = [];
  for (let li = L.indexOf(start); li >= 0 && li < L.count && L.starts[li] < end; li++) {
    const lc = colours.lines[li] ?? UNKNOWN;
    for (const r of runsOf(lc)) {
      const s = Math.max(start, L.starts[li] + r.s), e = Math.min(end, L.starts[li] + r.e);
      if (s < e) out.push({ start: s, end: e, key: colours.keys[r.c] });
    }
  }
  return { version: b.textGen, runs: out };
}

// ---------------------------------------------------------------- the host

// The engine and the buffers it colours. `onColours(buffer)` is told when a
// buffer's colours changed.
export class Highlighting {
  constructor(engine, onColours) {
    this.engine = engine;
    this.keys = engine.categories();
    this.onColours = onColours;
    /** this.queue: ToEngine[] */
    this.queue = [];
    this.docs = new Map(); // buffer id → {buf, colours, applied, open, unfollow}
    // Who waits for a range to be exact: {doc, from, to, resolve}.
    this.waiting = [];
    this.scheduled = false;
    // Slices run as separate tasks, so input and drawing go between them.
    this.channel = new MessageChannel();
  }
  tick() { this.channel.port2.postMessage(null); }

  // Start the engine in module `wasm` (wasm.js) with the grammars the
  // server serves.
  static async load(wasm, onColours) {
    const packdump = await fetch("/hl/syntaxes.packdump");
    if (!packdump.ok) throw new Error(`highlighting: the server did not serve the grammars (${packdump.status})`);
    const engine = new Engine(wasm);
    engine.init(new Uint8Array(await packdump.arrayBuffer()));
    const h = new Highlighting(engine, onColours);
    h.channel.port1.onmessage = () => { h.slice(); };
    return h;
  }

  // Colour buffer `b` (its `syntax`, none: plain text) from now on. Returns
  // its colours.
  attach(b) {
    const colours = new Colours(this.keys);
    const d = { buf: b, colours, applied: 0, open: false, unfollow: () => {} };
    this.docs.set(b.id, d);
    this.reopen(d);
    d.unfollow = b.followText((c) => {
      if (c.kind === "edit") {
        for (const L of listOf(b.text)) colours.edit(c, L);
        if (d.open) this.post(editMsg(b.id, c.version, c.at, c.removed, c.text));
      } else this.reopen(d);
    });
    return colours;
  }
  // Buffer `b`'s syntax changed: parse it again.
  resyntax(b) { for (const d of found(this.docs.get(b.id))) this.reopen(d); }
  // Start `d`'s buffer afresh: its held text, in its syntax (plain text
  // without one).
  reopen(d) {
    const b = d.buf;
    d.colours.reset(b.textGen, b.text);
    if (d.open) this.post(closeMsg(b.id, b.textGen));
    d.open = false;
    for (const syntax of listOf(b.syntax)) {
      for (const L of listOf(b.text)) {
        this.post(openMsg(b.id, b.textGen, syntax, L.lines.join("\n")));
        d.open = true;
      }
    }
  }
  detach(b) {
    for (const d of found(this.docs.get(b.id))) {
      d.unfollow();
      this.docs.delete(b.id);
      if (d.open) this.post(closeMsg(b.id, b.textGen));
    }
  }
  colours(b) { return found(this.docs.get(b.id)).map((d) => d.colours); }
  // Lines [from, to) of `b` are about to be drawn.
  want(b, from, to) { for (const d of found(this.docs.get(b.id))) if (d.open) this.post(wantMsg(b.id, from, to)); }

  // The colours of bytes [start, end) of `b`'s held text once they are
  // exact for its text as it is then: {version, runs: [{start, end, key}]}
  // in byte offsets. Rejects when `b` is not coloured or not held there.
  exactly(b, start, end) {
    const d = this.docs.get(b.id);
    if (d === undefined || !d.open) return Promise.reject(new Error(`${b.name} has no syntax colours`));
    const L = b.held;
    const from = L.indexOf(start), to = L.indexOf(Math.max(start, end - 1));
    if (from < 0 || to < 0) return Promise.reject(new Error(`${b.name}: bytes ${start}..${end} are not loaded`));
    return new Promise((resolve) => {
      this.waiting.push({ doc: b.id, from, to: to + 1, resolve: () => { resolve(runsIn(b, d.colours, start, end)); } });
      this.want(b, from, to + 1);
    });
  }
  // Answer whoever waits on a range that is now exact.
  answer() {
    const still = [];
    for (const w of this.waiting) {
      const d = this.docs.get(w.doc);
      if (d !== undefined && d.colours.pending.length === 0 && d.colours.exact(w.from, w.to)) w.resolve();
      else if (d !== undefined) still.push(w);
    }
    this.waiting = still;
  }

  post(m) {
    this.queue.push(m);
    if (!this.scheduled) { this.scheduled = true; this.tick(); }
  }

  // One slice: deliver what was posted, parse SLICE_BYTES, hand out answers.
  slice() {
    this.scheduled = false;
    const queue = this.queue;
    this.queue = [];
    for (const m of queue) this.deliver(m);
    const more = this.engine.work(SLICE_BYTES);
    for (const d of [...this.docs.values()].filter((x) => x.open)) {
      try {
        const lines = this.engine.take(d.buf.id);
        if (lines.length === 0) continue;
        d.colours.apply(d.applied, lines);
        this.onColours(d.buf);
      } catch (e) { this.refused(d, e); }
    }
    this.answer();
    if (more || this.queue.length > 0) { this.scheduled = true; this.tick(); }
  }
  /** method deliver(m: ToEngine) => Undefined */
  deliver(m) {
    const d = this.docs.get(m.doc);
    try {
      if (m.kind === "open") { this.engine.open(m.doc, m.syntax, m.text); for (const x of found(d)) x.applied = m.version; }
      else if (m.kind === "edit") { this.engine.edit(m.doc, m.at, m.removed, m.text); for (const x of found(d)) x.applied = m.version; }
      else if (m.kind === "close") { this.engine.close(m.doc); for (const x of found(d)) x.applied = m.version; }
      else this.engine.want(m.doc, m.from, m.to);
    } catch (e) { for (const x of found(d)) this.refused(x, e); }
  }
  // The engine refused `d`'s buffer (an unknown syntax, an edit it cannot
  // place, a grammar that failed): it shows plain text from now on, and
  // says why.
  refused(d, e) {
    d.open = false;
    this.detach(d.buf);
    d.buf.emit(errorNews(`${d.buf.name}: no syntax colours (${failure(e)})`));
    this.onColours(d.buf);
  }
}
