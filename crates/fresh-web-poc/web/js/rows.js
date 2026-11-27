// Where rows start, for scrolling, motions and the scrollbar: rows found
// by walking the layout's own rows (layout.js) from a place a row is known
// to start, never by a wrap rule of their own.
//
// A short line is walked from its start. A long one keeps a row index: the
// byte where every ROWS_PER_MARK-th row starts (a wrap checkpoint), so rows
// near any byte are found by walking forward from the nearest checkpoint.
// Without wrapping, it keeps the column of a cluster boundary now and then
// instead, so the columns on screen are drawn without laying the line out
// from its start.
//
// The index is built lazily: the rows a view needs first, the rest in
// background slices bounded in bytes (`work`). It is kept under one layout
// key (width, tab size, the generation of layout-changing decorations):
// another key starts it afresh. An edit inside a long line keeps the
// checkpoints before it and holds those after it aside, moved through the
// edit; the next walk past the edit re-wraps from a checkpoint before it
// until a row starts where a held one did, and takes the rest back with
// their row numbers moved (a line with no break opportunities never
// realigns, so its rows after the edit are found again in the background).

import { blockRows, blockOf, blockLast, nextShown, prevShown, ignoreMark } from "./layout.js";
import { byteLen } from "./utf8.js";
import { found } from "./maybe.js";
import { clustersAround } from "./clusters.js";

// A line this long, in bytes, is indexed.
export const LONG_LINE = 16 * 1024;
// Rows between wrap checkpoints.
const ROWS_PER_MARK = 32;
// Bytes walked by one background slice.
const WORK_BYTES = 64 * 1024;

// A checkpoint: a row's number in its line and the byte (from the line's
// start) where it starts; without wrapping, a column and the byte of the
// cluster boundary there.
/** type Mark = {ord: Number, at: Number} */
/** function markOf(ord: Number, at: Number) => Mark */
function markOf(ord, at) { return { ord, at }; }
// A long line's index: its length in bytes; its checkpoints, in order;
// the last row start (or marked column) walked to, every one before it
// known; whether the walk reached the line's end. `held`: checkpoints from
// before an edit, past `upto`, moved through it, to take back once a walk
// meets one; `heldDone` whether they reached the line's end.
/** type LineRows = {
 *   len: Number,
 *   marks: Mark[],
 *   upto: Mark,
 *   done: Boolean,
 *   held: Mark[],
 *   heldDone: Boolean,
 * } */
/** function freshRows(len: Number) => LineRows */
function freshRows(len) { return { len, marks: [markOf(0, 0)], upto: markOf(0, 0), done: false, held: [], heldDone: false }; }

// The last checkpoint of `marks` at or before byte `at`.
/** function markBefore(marks: Mark[], at: Number) => Mark */
function markBefore(marks, at) {
  let lo = 0, hi = marks.length - 1;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (marks[mid].at <= at) lo = mid; else hi = mid - 1;
  }
  return marks[lo];
}

// A view's row index, for its layout key; its lines by where they start,
// in bytes from the held text's start (an edit before the held window
// moves neither).
export class RowIndex {
  constructor() {
    this.key = "";
    this.wrap = false;
    /** this.lines: Map<Number, LineRows> */
    this.lines = new Map();
  }
  // Rows laid out under `key` from now on (another key: start afresh).
  use(key, wrap) {
    if (key === this.key) return;
    this.key = key;
    this.wrap = wrap;
    this.lines = new Map();
  }
  // The held text changed (a buffer's TextChange): the lines an edit
  // touched keep what is still true of them.
  change(c) {
    if (c.kind === "held") { this.lines = new Map(); return; }
    const n = byteLen(c.text), at = c.at, gone = c.removed;
    /** const kept: Map<Number, LineRows> */
    const kept = new Map();
    for (const [start, e] of this.lines) {
      const r = at - start;
      // Before the line (and not into it), after it, or in it; an edit
      // that makes it other lines (a newline typed, or its own removed)
      // leaves it to be indexed again.
      if (start > at + gone) kept.set(start + n - gone, e);
      else if (r > e.len) kept.set(start, e);
      else if (r >= 0 && !c.text.includes("\n") && r + gone <= e.len) kept.set(start, this.wrap ? edited(e, r, gone, n) : cutAt(e, r, n - gone));
    }
    this.lines = kept;
  }

  // Line `li`'s index, when it has one (a long line on its own): made when
  // first asked for. A list of at most one.
  /** method of(x: Layout, li: Number) => LineRows[] */
  of(x, li) {
    const L = x.L;
    if (L.lens[li] < LONG_LINE || blockOf(x, li) !== li || blockLast(x, li) !== li) return [];
    const key = L.starts[li] - L.start;
    const have = this.lines.get(key);
    if (have !== undefined) return [have];
    const made = freshRows(L.lens[li]);
    this.lines.set(key, made);
    return [made];
  }

  // Walk line `li`'s rows (an indexed one, `e`) from checkpoint `from`:
  // `visit(row, ord)` for each, until it answers false or the line ends.
  // What the walk passes beyond `upto` goes into the index.
  /** method walk(x: Layout, e: LineRows, li: Number, from: Mark, visit: (Row, Number) => Boolean) => Undefined */
  walk(x, e, li, from, visit) {
    const ls = x.L.starts[li];
    let ord = from.ord;
    for (const row of blockRows(x, li, ls + from.at, 0, Infinity, ignoreMark)) {
      if (row.kind !== "text") continue;
      const at = (row.start ?? ls) - ls;
      if (at > e.upto.at) {
        if (ord - e.marks[e.marks.length - 1].ord >= ROWS_PER_MARK) e.marks.push(markOf(ord, at));
        e.upto = markOf(ord, at);
        rejoin(e, ord, at);
      }
      if (endsLine(x, li, row)) e.done = true;
      if (!visit(row, ord)) return;
      ord++;
    }
  }
  // Index line `li` (`e`) up to byte `at` from its start (or its end).
  /** method reach(x: Layout, e: LineRows, li: Number, at: Number) => Undefined */
  reach(x, e, li, at) {
    if (e.done || e.upto.at >= at) return;
    if (this.wrap) this.walk(x, e, li, e.upto, () => !e.done && e.upto.at < at);
    // A cluster takes at most four columns a byte (a tab), so a walk to
    // that column has passed `at`.
    else this.reachColumn(x, e, li, e.upto.ord + 4 * (at - e.upto.at) + 1);
  }
  // Without wrapping: index line `li` (`e`) up to column `col`.
  /** method reachColumn(x: Layout, e: LineRows, li: Number, col: Number) => Undefined */
  reachColumn(x, e, li, col) {
    if (e.done || e.upto.ord >= col) return;
    const ls = x.L.starts[li];
    const mark = (c, b) => {
      e.upto = markOf(c, b - ls);
      e.marks.push(e.upto);
    };
    for (const row of blockRows(x, li, ls + e.upto.at, e.upto.ord, col, mark)) {
      if (row.kind === "text" && endsLine(x, li, row)) e.done = true;
    }
  }

  // Index the long lines not done yet a slice further: whether any is
  // still not done.
  /** method work(x: Layout) => Boolean */
  work(x) {
    const L = x.L;
    for (const [start, e] of this.lines) {
      if (e.done) continue;
      const li = L.indexOf(L.start + start);
      if (li < 0 || L.starts[li] !== L.start + start) { this.lines.delete(start); continue; }
      this.reach(x, e, li, e.upto.at + WORK_BYTES);
      return true;
    }
    return false;
  }
}

// Whether text row `r` is the last of line `li`: it ends the line, or
// the held part of a line a window cuts.
/** function endsLine(x: Layout, li: Number, r: Row) => Boolean */
function endsLine(x, li, r) { return r.eol !== null || r.end === x.L.lineEnd(li); }

// Checkpoints `e` through an edit at byte `r` of its line that removed
// `gone` bytes and inserted `n`: those before the row holding the edit
// kept; those after it held aside, moved (any held from an edit before
// are dropped: their row numbers are another edit's).
/** function edited(e: LineRows, r: Number, gone: Number, n: Number) => LineRows */
function edited(e, r, gone, n) {
  // The row holding the edit may start elsewhere now: keep the checkpoints
  // before the last one before it.
  const before = e.marks.filter((m) => m.at < r);
  const marks = before.length > 1 ? before.slice(0, -1) : [markOf(0, 0)];
  const held = e.marks.filter((m) => m.at >= r + gone && m.at > 0).map((m) => markOf(m.ord, m.at + n - gone));
  return { len: e.len + n - gone, marks, upto: marks[marks.length - 1], done: false, held, heldDone: e.done };
}
// Column checkpoints `e` cut at byte `r` (the line `d` bytes longer):
// those before it kept.
/** function cutAt(e: LineRows, r: Number, d: Number) => LineRows */
function cutAt(e, r, d) {
  const kept = e.marks.filter((m) => m.at < r);
  const marks = kept.length > 0 ? kept : [markOf(0, 0)];
  return { len: e.len + d, marks, upto: marks[marks.length - 1], done: false, held: [], heldDone: false };
}
// A walk reached row `ord` at byte `at`: a checkpoint held from before an
// edit there realigns the rows, and the held ones after it come back,
// their row numbers moved; those it passed are gone.
/** function rejoin(e: LineRows, ord: Number, at: Number) => Undefined */
function rejoin(e, ord, at) {
  if (e.held.length === 0) return;
  const k = e.held.findIndex((m) => m.at >= at);
  if (k < 0) { e.held = []; return; }
  const m = e.held[k];
  if (m.at !== at) { e.held = e.held.slice(k); return; }
  const d = ord - m.ord;
  const rest = e.held.slice(k + 1).map((h) => markOf(h.ord + d, h.at));
  e.marks.push(...rest);
  e.upto = rest.length > 0 ? rest[rest.length - 1] : e.upto;
  e.done = e.heldDone;
  e.held = [];
  e.heldDone = false;
}

// ---------------------------------------------------------------- walking rows

// Where a walk of rows cuts a row short: with wrapping, nowhere; without,
// at once (each block is one row, and only its place matters).
/** function walkCol(x: Layout) => Number */
function walkCol(x) { return x.o.wrap ? Infinity : 0; }

// A text row's address: the byte it starts at.
/** function startOf(r: Row) => Number */
function startOf(r) { return r.start ?? 0; }

// The rows of block `li` from where `from` is, as far as the row holding
// byte `to` (`to` past the block: all of them). From the block's start, the
// virtual lines above it come first.
/** function rowsTo(x: Layout, idx: RowIndex, li: Number, from: Number, to: Number) => Row[] */
function rowsTo(x, idx, li, from, to) {
  /** const out: Row[] */
  const out = [];
  for (const r of blockRows(x, li, from, 0, walkCol(x), ignoreMark)) {
    if (r.kind === "text" && startOf(r) > to) break;
    out.push(r);
  }
  return out;
}
// The checkpoint to walk line `li`'s rows from to reach byte `p`: the
// line's start, or its index's nearest checkpoint (indexed that far).
/** function walkFrom(x: Layout, idx: RowIndex, li: Number, p: Number) => Number */
function walkFrom(x, idx, li, p) {
  const ls = x.L.starts[li];
  for (const e of idx.of(x, li)) {
    idx.reach(x, e, li, p - ls);
    return ls + markBefore(e.marks, p - ls).at;
  }
  return ls;
}

// The byte where the row holding byte `p` starts (wrapping).
/** function rowStart(x: Layout, idx: RowIndex, p: Number) => Number */
export function rowStart(x, idx, p) {
  const L = x.L;
  const li = blockOf(x, L.indexOf(p));
  let at = L.starts[li];
  for (const r of rowsTo(x, idx, li, walkFrom(x, idx, li, p), p)) if (r.kind === "text") at = startOf(r);
  return at;
}

// Where a view's top row starts, for the byte the view was scrolled to
// (`top`): the start of the row holding it, when the rows there are known
// (a short line, or a long one indexed that far); else `top` itself,
// trusted as a row start until they are (a long line drawn first where the
// view is, after a width change or a jump, its index built behind).
/** function settledTop(x: Layout, idx: RowIndex, top: Number) => Number */
export function settledTop(x, idx, top) {
  const L = x.L;
  const li = blockOf(x, L.indexOf(top));
  if (!x.o.wrap) return L.starts[li];
  for (const e of idx.of(x, li)) if (!e.done && e.upto.at < top - L.starts[li]) return top;
  return rowStart(x, idx, top);
}

// A walk of rows: where it landed (a text row's start) and how many rows
// it moved.
/** type Landed = {top: Number, moved: Number} */

// From the row starting at `top`, `n` rows down, as drawn from there
// (`text`: counting only text rows, from the text row at `top`; else
// virtual ones too, landing on the text row at or after them). It stops
// at the last text row.
/** function rowsDown(x: Layout, idx: RowIndex, top: Number, n: Number, text: Boolean) => Landed */
export function rowsDown(x, idx, top, n, text) {
  const L = x.L;
  let li = blockOf(x, L.indexOf(top));
  let from = top, moved = 0, last = top, lastMoved = 0;
  while (li < L.count) {
    for (const r of blockRows(x, li, from, 0, walkCol(x), ignoreMark)) {
      if (text && r.kind !== "text") continue;
      if (r.kind === "text") {
        if (moved >= n) return { top: startOf(r), moved };
        last = startOf(r);
        lastMoved = moved;
      }
      moved++;
    }
    li = nextShown(x, li, blockLast(x, li));
    if (li < L.count) from = L.starts[li];
  }
  return { top: last, moved: lastMoved };
}

// A row before a view's top, and where a top that shows it is: its own
// start, or for a virtual row the text row it goes with (above a line:
// the line's start, which draws it; below: the line's last row).
/** type Before = {row: Row, top: Number} */

// The rows before the one starting at `top`, nearest first: a checkpoint's
// worth of a long line at a time.
/** function rowsBefore(x: Layout, idx: RowIndex, top: Number) => Iterator<Before> */
function* rowsBefore(x, idx, top) {
  const L = x.L;
  let li = blockOf(x, L.indexOf(top));
  let hi = top;
  for (;;) {
    const ls = L.starts[li];
    if (hi === Infinity || hi > ls) {
      const lo = hi === Infinity ? walkFrom(x, idx, li, L.lineEnd(blockLast(x, li))) : walkFrom(x, idx, li, hi - 1);
      const rows = rowsTo(x, idx, li, lo, hi).filter((r) => r.kind !== "text" || startOf(r) < hi);
      const texts = rows.filter((r) => r.kind === "text");
      const first = texts.map(startOf).at(0) ?? ls, lastText = texts.map(startOf).at(-1) ?? ls;
      for (let k = rows.length - 1; k >= 0; k--) {
        const r = rows[k];
        yield { row: r, top: r.kind === "text" ? startOf(r) : k < rows.indexOf(texts[0] ?? r) ? first : lastText };
      }
      hi = lo;
      if (lo > ls) continue;
    }
    li = prevShown(x, li);
    if (li < 0) return;
    hi = Infinity;
  }
}

// From the row starting at `top`, `n` rows up (`text` as for rowsDown). It
// stops at the first row.
/** function rowsUp(x: Layout, idx: RowIndex, top: Number, n: Number, text: Boolean) => Landed */
export function rowsUp(x, idx, top, n, text) {
  let moved = 0, at = top;
  if (n <= 0) return { top, moved };
  for (const b of rowsBefore(x, idx, top)) {
    if (text && b.row.kind !== "text") continue;
    moved++;
    at = b.top;
    if (moved >= n) break;
  }
  return { top: at, moved };
}

// Without wrapping: where block `li` is drawn from to show column `col`
// on: the line's start, or for a long line the nearest marked column
// before it (its byte and column).
/** function columnStart(
 *   x: Layout,
 *   idx: RowIndex,
 *   li: Number,
 *   col: Number,
 * ) => {at: Number, col: Number} */
export function columnStart(x, idx, li, col) {
  const ls = x.L.starts[li];
  for (const e of idx.of(x, li)) {
    idx.reachColumn(x, e, li, col);
    let m = e.marks[0];
    for (const k of e.marks) if (k.ord <= col) m = k;
    return { at: ls + m.at, col: m.ord };
  }
  return { at: ls, col: 0 };
}
// Without wrapping: the column byte `p` is drawn at in its line.
/** function columnOf(x: Layout, idx: RowIndex, p: Number) => Number */
export function columnOf(x, idx, p) {
  const L = x.L;
  const li = blockOf(x, L.indexOf(p));
  const ls = L.starts[li];
  let from = { at: ls, col: 0 };
  for (const e of idx.of(x, li)) {
    idx.reach(x, e, li, p - ls);
    const m = markBefore(e.marks, p - ls);
    from = { at: ls + m.at, col: m.ord };
  }
  // A cluster takes at most four columns a byte (a tab): a row cut there
  // has passed `p`.
  for (const r of blockRows(x, li, from.at, from.col, from.col + 4 * (p - from.at) + 1, ignoreMark)) {
    if (r.kind !== "text") continue;
    let col = r.col0;
    for (const c of r.cells) {
      if (c.src !== null && c.src >= p) return col;
      col += c.w;
    }
    return col;
  }
  return 0;
}

// ---------------------------------------------------------------- the scrollbar

// Where row `top` is in the held text, from 0 to 1: without wrapping by
// lines (each a row); with it by bytes, as rows are not all counted (a
// screen shows `shown` bytes).
/** function fractionOf(
 *   x: Layout,
 *   top: Number,
 *   rows: Number,
 *   shown: Number,
 * ) => {at: Number, size: Number} */
export function fractionOf(x, top, rows, shown) {
  const L = x.L;
  if (!x.o.wrap) {
    const n = Math.max(1, L.count);
    return { at: L.indexOf(top) / n, size: Math.min(1, rows / n) };
  }
  const held = Math.max(1, L.lineEnd(L.count - 1) - L.start);
  return { at: (top - L.start) / held, size: Math.min(1, shown / held) };
}
// The top for fraction `f` of the held text (the inverse of fractionOf):
// a line's start, or with wrapping the start of the row holding the byte
// there.
/** function topAtFraction(x: Layout, idx: RowIndex, f: Number) => Number */
export function topAtFraction(x, idx, f) {
  const L = x.L;
  const t = Math.max(0, Math.min(1, f));
  if (!x.o.wrap) return L.starts[Math.min(L.count - 1, Math.floor(t * L.count))];
  const p = L.start + Math.floor(t * (L.lineEnd(L.count - 1) - L.start));
  const li = L.indexOf(p);
  const ls = L.starts[li];
  const text = L.lines[li];
  // The character there (in a line's text: a newline's byte is its end).
  const n = L.idxBefore(li, p - ls);
  const around = clustersAround(text, n);
  return rowStart(x, idx, L.posAt(li, around.on || n >= text.length ? n : around.prev));
}
