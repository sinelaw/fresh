// Editing commands over every cursor of a view.
//
// Every edit is a set of replacements, one per cursor, applied as one batch
// (one undo step) from the last position to the first so earlier positions
// stay valid; each caret is then placed where its replacement says.

import { listOf, found, some, none } from "./maybe.js";
import { insOp, delOp } from "./ot.js";
import { byteLen } from "./utf8.js";
import { sel, cursorAt, selecting } from "./view.js";
import { rpc } from "./rpc.js";
import { clustersOf, clustersAround, unitsOf, widthOf } from "./clusters.js";
import { crBefore } from "./layout.js";
import { foldRange as docFoldRange } from "./doc.js";
import { CLOSE, partnerOf, openerBefore } from "./brackets.js";

const PAIRS = new Map([["(", ")"], ["[", "]"], ["{", "}"], ['"', '"'], ["'", "'"], ["`", "`"]]);
const INDENT = "    ";

// No block folds there.
/** function noFold() => Range[] */
function noFold() { return []; }

// No language server's selection ranges (none runs, or it has no answer).
/** const NO_RANGES: Range[][] | Null */
const NO_RANGES = null;

const TAB = 4;

// The screen column of UTF-16 index `idx` in `line` (tabs to the next stop,
// each cluster as wide as it is drawn), and the index of the cluster at or
// before column `col`.
/** function colOf(line: String, idx: Number) => Number */
function colOf(line, idx) {
  const c = clustersOf(line);
  let col = 0, i = 0;
  for (let k = 0; k < c.n && i < idx; k++) {
    col += line.charCodeAt(i) === 9 ? TAB - (col % TAB) : widthOf(c, k);
    i += unitsOf(c, k);
  }
  return col;
}
/** function idxAt(line: String, col: Number) => Number */
function idxAt(line, col) {
  const c = clustersOf(line);
  let at = 0, i = 0;
  for (let k = 0; k < c.n; k++) {
    const w = line.charCodeAt(i) === 9 ? TAB - (at % TAB) : widthOf(c, k);
    if (at + w > col) break;
    at += w;
    i += unitsOf(c, k);
  }
  return i;
}
// The cluster boundary at or before UTF-16 index `i` of `line`.
/** function clusterAt(line: String, i: Number) => Number */
function clusterAt(line, i) {
  const at = Math.min(i, line.length);
  const around = clustersAround(line, at);
  return around.on ? at : around.prev;
}

// The line holding a position: its index among the held lines, its start
// and end, its text, and the next line's start (null: the last).
/** type LineAt = {i: Number, start: Number, end: Number, text: String, next: Number | Null} */
// The held line of buffer `b` holding byte `p`: its index, start, end,
// text, and the next line's start (none: it is the last held). A list of
// at most one: none when `p` is outside the held text.
export function linesAt(b, p) {
  const L = b.held;
  const i = L.indexOf(p);
  return i < 0 ? noLine() : [lineIn(L, i)];
}
// The held line holding byte `p`, where the caller knows it is held (a
// cursor's position): it throws when it is not.
export function lineAt(b, p) {
  for (const ln of linesAt(b, p)) return ln;
  throw new Error(`${b.name}: byte ${p} is not held`);
}
// Held line `i` of `L`.
function lineIn(L, i) {
  /** const ln: LineAt */
  const ln = { i, start: L.starts[i], end: L.lineEnd(i), text: L.lines[i], next: i + 1 < L.count ? some(L.starts[i + 1]) : none() };
  return ln;
}
/** function noLine() => LineAt[] */
function noLine() { return []; }

// What a cursor's text becomes: [start, end) replaced by `text`, the caret
// `caret` into it (null: at its end) and the selection's anchor `anchor`
// into it (null: no selection).
/** type Replacement = {
 *   start: Number,
 *   end: Number,
 *   text: String,
 *   caret: Number | Null,
 *   anchor: Number | Null,
 * } */
/** function putWith(
 *   start: Number,
 *   end: Number,
 *   text: String,
 *   caret: Number | Null,
 *   anchor: Number | Null,
 * ) => Replacement | Null */
export function putWith(start, end, text, caret, anchor) { return { start, end, text, caret, anchor }; }
// [start, end) replaced by `text`, the caret after it.
/** function put(start: Number, end: Number, text: String) => Replacement | Null */
export function put(start, end, text) { return putWith(start, end, text, null, null); }
// A cursor's text left alone.
/** const KEEP: Replacement | Null */
export const KEEP = null;

// What an edit says when it needs text the buffer does not hold.
const UNHELD = "The selection reaches past the loaded part of the file";

// A range and its text.
/** type Held = {start: Number, end: Number, text: String} */
/** function heldOf(start: Number, end: Number, text: String) => Held */
function heldOf(start, end, text) { return { start, end, text }; }

// The held text of each cursor's range (`rangeOf`: null for a cursor
// without one), by cursor: a list of at most one, none when a range
// reaches past the text buffer `b` holds.
/** function heldTexts(
 *   b: Buffer,
 *   cursors: Cursor[],
 *   rangeOf: (c: Cursor) => {start: Number, end: Number} | Null,
 * ) => Map<Cursor, Held>[] */
export function heldTexts(b, cursors, rangeOf) {
  /** const held: Map<Cursor, Held> */
  const held = new Map();
  let ranged = 0;
  for (const c of cursors) {
    for (const r of listOf(rangeOf(c))) {
      ranged += 1;
      for (const text of listOf(b.textRange(r.start, r.end))) held.set(c, heldOf(r.start, r.end, text));
    }
  }
  return held.size === ranged ? [held] : [];
}

// Each cursor's replacement, last in the text first.
/** function replacements(
 *   cursors: Cursor[],
 *   fn: (Cursor) => Replacement | Null,
 * ) => {cursor: Cursor, r: Replacement}[] */
function replacements(cursors, fn) {
  /** const out: {cursor: Cursor, r: Replacement}[] */
  const out = [];
  for (const cursor of cursors) {
    const r = fn(cursor);
    if (r) out.push({ cursor, r });
  }
  return out.sort((x, y) => y.r.start - x.r.start);
}

// The edits making `reps` (last in the text first, so each applies where it
// was asked).
/** function editsOf(reps: {cursor: Cursor, r: Replacement}[]) => Op[] */
function editsOf(reps) {
  /** const ops: Op[] */
  const ops = [];
  for (const { r } of reps) {
    if (r.end > r.start) ops.push(delOp(r.start, r.end - r.start));
    if (r.text !== "") ops.push(insOp(r.start, r.text));
  }
  return ops;
}

// Where each replacement leaves its cursor once all are made.
/** function placesAfter(
 *   reps: {cursor: Cursor, r: Replacement}[],
 * ) => {cursor: Cursor, at: Cursor}[] */
function placesAfter(reps) {
  let delta = 0;
  return [...reps].reverse().map(({ cursor, r }) => {
    const s = r.start + delta;
    const len = byteLen(r.text);
    delta += len - (r.end - r.start);
    const anchor = r.anchor;
    return { cursor, at: selecting(s + (r.caret ?? len), anchor === null ? null : s + anchor) };
  });
}

// The character (grapheme cluster) before byte `p` of buffer `b` (a line
// break between lines, CRLF as one; none at the start, or outside the held
// text).
function charBefore(b, p) {
  if (p <= 0) return none();
  for (const ln of linesAt(b, p)) {
    const L = b.held;
    const idx = L.idxOf(ln.i, p);
    if (idx === 0) {
      if (ln.i === 0 && L.start >= ln.start) return none();
      return some(ln.i > 0 && L.lines[ln.i - 1].endsWith("\r") ? "\r\n" : "\n");
    }
    const around = clustersAround(ln.text, idx);
    return some(ln.text.slice(around.prev, idx));
  }
  return none();
}

// The character (grapheme cluster) at byte `p` of buffer `b` (a line
// break at a line's end, CRLF as one; none at the end, or outside the
// held text).
function charAtPos(b, p) {
  if (p >= b.len) return none();
  for (const ln of linesAt(b, p)) {
    const idx = b.held.idxOf(ln.i, p);
    if (idx >= ln.text.length) return some("\n");
    if (idx === ln.text.length - 1 && ln.text.endsWith("\r") && ln.next !== null) return some("\r\n");
    return some(ln.text.slice(idx, clustersAround(ln.text, idx).next));
  }
  return none();
}

/** function isWord(c: String | Null) => Boolean */
function isWord(c) { return listOf(c).some((x) => /[\p{L}\p{N}_]/u.test(x)); }
// A character's length in bytes (none: 0).
/** function lenOf(c: String | Null) => Number */
function lenOf(c) { return listOf(c).map(byteLen).at(0) ?? 0; }

// The unmatched opening bracket before `p` (in the held lines), or null.
/** function openerAt(syn: LineSyntax, b: Buffer, p: Number) => Number | Null */
function openerAt(syn, b, p) {
  const L = b.held;
  const li = L.indexOf(p);
  if (li < 0) return null;
  return openerBefore((k) => syn.brackets(k), L, li, L.idxOf(li, p));
}

export function shrinkSelection(pane) {
  const v = pane.view;
  const key = v.cursors.map((c) => `${c.anchor}:${c.pos}`).join();
  const prev = v.expansions.key === key ? v.expansions.stack.pop() : undefined;
  if (prev !== undefined) {
    v.cursors = prev.map((c) => selecting(c.pos, c.anchor));
    v.expansions.key = v.cursors.map((c) => `${c.anchor}:${c.pos}`).join();
  } else for (const c of v.cursors) c.anchor = none();
  pane.reveal(false);
  pane.scheduleRender();
}

function blockKey(v) { return v.cursors.map((c) => `${c.anchor}:${c.pos}`).join(); }

// Where a position is as a block's corner: {line, col}.
function cornerAt(b, p) {
  const L = b.text;
  const li = L.indexOf(p);
  return { line: li, col: colOf(L.lines[li], L.idxOf(li, p)) };
}

// The block that starts at window line `li`: the language server's range
// when it gave one, else the lines indented deeper than it. A compose
// view folds its document's blocks (a heading's section, a list's deeper
// items, a code block's lines).
function foldRangeAt(v, li) {
  if (v.mode === "compose") return listOf(docFoldRange(v, li));
  const b = v.buf;
  const L = b.held;
  // The language server's range (its lines are the file's: known once the
  // window's place in the file is), else the lines indented past this one.
  const lsp = listOf(L.firstLine).flatMap((first) => b.foldRanges.filter((r) => r.start === li + first && r.end > r.start).slice(0, 1).map((r) => ({ start: li, end: Math.min(L.count - 1, r.end - first) })));
  if (lsp.length > 0) return lsp;
  const ind = (t) => t.length - t.trimStart().length;
  if (L.lines[li].trim() === "") return noFold();
  const base = ind(L.lines[li]);
  let end = li;
  for (let k = li + 1; k < L.count; k++) {
    if (L.lines[k].trim() === "") continue;
    if (ind(L.lines[k]) <= base) break;
    end = k;
  }
  return end > li ? [{ start: li, end }] : noFold();
}

export function unfoldAll(pane) { pane.view.folds = []; pane.scheduleRender(); }

// Lines touched by a cursor, its selection included: [first, last] indices.
function lineSpan(b, c) {
  const s = sel(c) ?? { start: c.pos, end: c.pos };
  const L = b.held;
  let last = L.indexOf(s.end);
  if (s.end > s.start && s.end === L.starts[last] && last > L.indexOf(s.start)) last--;
  return [L.indexOf(s.start), last];
}

export function addCursorVertical(pane, dir) {
  const v = pane.view;
  const b = v.buf;
  const L = b.held;
  const edge = v.cursors.reduce((m, c) => (dir < 0 ? c.pos < m.pos : c.pos > m.pos) ? c : m, v.primary);
  const ln = lineAt(b, edge.pos);
  const j = ln.i + dir;
  if (j < 0 || j >= L.count) return;
  const col = L.idxOf(ln.i, edge.pos);
  v.cursors.push(cursorAt(L.posAt(j, clusterAt(L.lines[j], L.idxFrom(j, col)))));
  v.normalize();
  pane.scheduleRender();
}

// Characters of words: letters (with their marks), digits and `_`.
const WORD = /[\p{L}\p{N}\p{M}_]/u;
/** function wordChar(text: String, i: Number) => Boolean */
function wordChar(text, i) { return WORD.test(String.fromCodePoint(text.codePointAt(i) ?? 0)); }
// Where a run of characters that are (`word`) or are not words, from
// UTF-16 index `i` of `text` in direction `dir`, ends, at most `max` units
// on.
/** function runEnd(text: String, i: Number, dir: Number, word: Boolean, max: Number) => Number */
function runEnd(text, i, dir, word, max) {
  let k = i;
  while (Math.abs(k - i) < max) {
    if (dir > 0) {
      if (k >= text.length || wordChar(text, k) !== word) return k;
      k += (text.codePointAt(k) ?? 0) > 0xffff ? 2 : 1;
    } else {
      if (k <= 0) return k;
      const at = k >= 2 && isLowUnit(text.charCodeAt(k - 1)) ? k - 2 : k - 1;
      if (wordChar(text, at) !== word) return k;
      k = at;
    }
  }
  return k;
}
/** function isLowUnit(u: Number) => Boolean */
function isLowUnit(u) { return u >= 0xdc00 && u < 0xe000; }
// UTF-16 index `i` of `text` on a cluster boundary: `i`, or the boundary
// before it (`dir` < 0) or after it.
/** function onCluster(text: String, i: Number, dir: Number) => Number */
function onCluster(text, i, dir) {
  const around = clustersAround(text, i);
  return around.on ? i : dir < 0 ? around.prev : around.next;
}

// Where a word motion from byte `p` of buffer `b` goes in direction `dir`:
// past what is not a word on the line, then the word after it (or a
// character, the line break at a line's edge).
function wordEdge(b, p, dir) {
  for (const ln of linesAt(b, p)) {
    const L = b.held;
    const text = ln.text;
    const i = L.idxOf(ln.i, p);
    if (dir > 0 ? i >= text.length : i === 0) return p + dir * lenOf(dir > 0 ? charAtPos(b, p) : charBefore(b, p));
    const k = runEnd(text, runEnd(text, i, dir, false, Infinity), dir, true, Infinity);
    const to = k === i ? (dir > 0 ? clustersAround(text, i).next : clustersAround(text, i).prev) : onCluster(text, k, dir);
    return L.posAt(ln.i, to);
  }
  return p;
}

// The word at byte `p` of buffer `b` ({start, end}; none: not in or next
// to a word), looked for at most `max` UTF-16 units either side (a word
// reaching further: none).
export function wordAt(b, p, max) {
  for (const ln of linesAt(b, p)) {
    const L = b.held;
    const text = ln.text;
    const i = L.idxOf(ln.i, p);
    const s = runEnd(text, i, -1, true, max), e = runEnd(text, i, 1, true, max);
    if (i - s >= max || e - i >= max || s === e) return null;
    return { start: L.posAt(ln.i, onCluster(text, s, -1)), end: L.posAt(ln.i, onCluster(text, e, 1)) };
  }
  return null;
}

// A rectangle of text, from screen column and line `anchor` to `head`
// ({line, col}, window line indices): one cursor a line, each selecting
// the line's part between the two columns (a short line gives an empty
// selection at its end). The view remembers it, so Alt+Shift+arrows go
// on growing it while its cursors are the rectangle's.
export function setBlock(pane, anchor, head) {
  const v = pane.view;
  const L = v.buf.held;
  const lo = Math.min(anchor.line, head.line), hi = Math.max(anchor.line, head.line);
  // A cursor on each line, the head's line's first (the primary).
  const lines = [head.line];
  for (let k = lo; k <= hi; k++) if (k !== head.line) lines.push(k);
  v.cursors = lines.map((k) => {
    const line = L.lines[k];
    const at = (col) => L.posAt(k, idxAt(line, col));
    return selecting(at(head.col), at(anchor.col));
  });
  v.block = [{ anchor, head, key: blockKey(v) }];
  pane.reveal(false);
  pane.scheduleRender();
}

export function toggleFoldAt(pane, li) {
  const v = pane.view;
  const range = foldRangeAt(v, li);
  // The fold starting on the line, or the one this line's range starts.
  const starts = [li, ...range.map((r) => r.start)];
  const i = v.folds.findIndex((f) => starts.includes(f.start));
  if (i >= 0) v.folds.splice(i, 1);
  else {
    if (range.length === 0) return;
    const r = range[0];
    v.folds = v.folds.filter((f) => f.start < r.start || f.start > r.end);
    v.folds.push(r);
    // A cursor the fold hides moves to the end of the last line it keeps.
    const L = v.buf.held;
    for (const c of v.cursors) for (const h of pane.foldHiding(v, L.indexOf(c.pos))) { c.pos = L.lineEnd(h.keep); c.anchor = none(); }
  }
  pane.scheduleRender();
}

export function foldAll(pane) {
  const v = pane.view;
  const L = v.buf.held;
  v.folds = [];
  for (let k = 0; k < L.count; k++) {
    if (v.folds.some((f) => k >= f.start && k <= f.end)) continue;
    v.folds.push(...foldRangeAt(v, k));
  }
  pane.scheduleRender();
}

// The bracket at `p` (else the one just before it) and its partner:
// {at, match} byte offsets, or null (neither has one in the held lines).
// Brackets in strings and comments do not count.
/** function bracketPair(
 *   syn: LineSyntax,
 *   b: Buffer,
 *   p: Number,
 * ) => {at: Number, match: Number} | Null */
export function bracketPair(syn, b, p) {
  for (const L of listOf(b.text)) {
    const li = L.indexOf(p);
    if (li < 0) return null;
    // The character at `p`, then the one before it on its line (a bracket
    // is one byte).
    const k = L.idxOf(li, p);
    for (const [at, i] of [[p, k], [p - 1, k - 1]]) {
      if (i < 0) continue;
      const match = partnerOf((n) => syn.brackets(n), L, li, i);
      if (match !== null) return { at, match };
    }
  }
  return null;
}

export function selectLine(pane) {
  const b = pane.view.buf;
  const L = b.held;
  for (const c of pane.view.cursors) {
    const [a, z] = lineSpan(b, c);
    c.anchor = some(L.starts[a]);
    c.pos = z + 1 < L.count ? L.starts[z + 1] : L.lineEnd(z);
  }
  pane.scheduleRender();
}

// The ranges that hold `s` ({start, end}), smallest first, from the
// text's structure: the word, a string's inside and the string, each
// bracket pair's inside and the pair outwards, the line's text and the
// line, each indentation block outwards (its lines), the document.
function structureAround(syn, b, s) {
  const L = b.held;
  const out = [];
  const add = (start, end) => { if (start <= s.start && end >= s.end && end > start) out.push({ start, end }); };
  const w = wordAt(b, s.start, Infinity);
  if (w) add(w.start, w.end);
  const li = L.indexOf(s.start);
  if (li < 0) return out;
  const line = L.lines[li], ls = L.starts[li];
  // The closed strings on the line (as the grammar coloured them): the
  // inside and the string.
  const closed = (x) => x.e - x.s >= 2 && line[x.e - 1] === line[x.s];
  for (const x of syn.ranges(li, ["syntax.string"]).filter(closed)) {
    const a = L.posAt(li, x.s), z = L.posAt(li, x.e);
    add(a + 1, z - 1);
    add(a, z);
  }
  // Bracket pairs around it, outwards.
  for (let open = openerAt(syn, b, s.start); open !== null; open = openerAt(syn, b, open)) {
    for (const pair of listOf(bracketPair(syn, b, open))) {
      if (pair.match >= s.end) {
        add(open + 1, pair.match);
        add(open, pair.match + 1);
      }
    }
  }
  // The line's text, then the line with its break.
  const text0 = L.posAt(li, (/^\s*/.exec(line) ?? [""])[0].length);
  add(text0, L.posAt(li, line.replace(/\s+$/, "").length));
  add(ls, li + 1 < L.count ? L.starts[li + 1] : L.lineEnd(li));
  // Indentation blocks: the lines around it indented at least as deep,
  // then with the line that opens them, outwards.
  const indent = (k) => (L.lines[k].trim() === "" ? Infinity : L.lines[k].length - L.lines[k].trimStart().length);
  let lo = li, hi = L.indexOf(s.end);
  if (hi < 0) hi = L.count - 1;
  for (let guard = 0; guard < 64; guard++) {
    let depth = Infinity;
    for (let k = lo; k <= hi; k++) depth = Math.min(depth, indent(k));
    if (depth === Infinity) depth = 0;
    while (lo > 0 && indent(lo - 1) >= depth) lo--;
    while (hi < L.count - 1 && indent(hi + 1) >= depth) hi++;
    add(L.starts[lo], hi + 1 < L.count ? L.starts[hi + 1] : L.lineEnd(hi));
    if (lo === 0 && hi === L.count - 1) break;
    if (lo > 0) lo--;
    else if (hi < L.count - 1) hi++;
  }
  if (b.full) add(0, b.len);
  return out.sort((x, y) => (x.end - x.start) - (y.end - y.start));
}

// Grow the rectangle by a line or a column (Alt+Shift+arrows); a new one
// starts at the primary cursor.
export function blockSelect(pane, dl, dc) {
  const v = pane.view;
  for (const L of listOf(v.buf.text)) {
    // The rectangle kept for this view's cursors, else one at the primary.
    let anchor = cornerAt(v.buf, v.primary.pos);
    let head = { ...anchor };
    for (const k of v.block.filter((x) => x.key === blockKey(v)).slice(0, 1)) { anchor = k.anchor; head = { ...k.head }; }
    head.line = Math.max(0, Math.min(L.count - 1, head.line + dl));
    head.col = Math.max(0, head.col + dc);
    setBlock(pane, anchor, head);
  }
}

export function selectWord(pane) {
  const v = pane.view;
  const w = wordAt(v.buf, v.primary.pos, Infinity);
  for (const x of listOf(w)) { v.primary.anchor = some(x.start); v.primary.pos = x.end; }
  pane.scheduleRender();
}

export function toggleFold(pane) {
  const v = pane.view;
  const L = v.buf.held;
  const li = L.indexOf(v.primary.pos);
  // Unfold the fold the cursor's line heads, else fold the innermost
  // block holding the cursor.
  if (v.folds.some((f) => f.start === li)) return toggleFoldAt(pane, li);
  for (let k = li; k >= 0; k--) {
    if (foldRangeAt(v, k).some((r) => r.end >= li)) return toggleFoldAt(pane, k);
  }
}

export class Editing {
  // `editor` (main.js): say something in the status bar, apply an edit
  // (as typed, or not), tell it a cursor moved, the auto-close setting, and
  // a buffer's language server positions.
  constructor(editor) {
    this.editor = editor;
    // The view and carets a run of typing left (the next keystroke there
    // continues it, for undo): a list of at most one.
    this.typed = [];
  }

  // ---------------------------------------------------------------- text queries

  // ---------------------------------------------------------------- the core

  // `fn(cursor)` gives each cursor's Replacement (`put`, `putWith`), or
  // KEEP to leave its text alone. One batch, one undo step; each cursor
  // ends where its replacement puts it.
  replace(pane, fn) { return this.replaceTyped(pane, fn, "none"); }
  // The held text of each cursor's range (see heldTexts); none, said so,
  // when a range reaches past the held text.
  held(b, cursors, rangeOf) {
    const held = heldTexts(b, cursors, rangeOf);
    if (held.length === 0) this.editor.say(UNHELD);
    return held;
  }
  // The same, typed text (`typing`: "start" or "continue" a run, which is
  // one undo step, or "none"; see buffer.js).
  replaceTyped(pane, fn, typing) {
    const v = pane.view;
    const b = v.buf;
    if (b.readOnly) { this.editor.say("Read-only buffer"); return false; }
    v.normalize();
    const reps = replacements(v.cursors, fn);
    if (!reps.length) return false;
    const old = v.primary.pos;
    const ops = editsOf(reps);
    if (ops.length) this.editor.edit(b, ops, typing);
    // Each cursor where its replacement put it (the others' selections
    // go), shifted by every replacement before it.
    /** const placed: Map<Cursor, Cursor> */
    const placed = new Map();
    for (const x of placesAfter(reps)) placed.set(x.cursor, x.at);
    v.cursors = v.cursors.map((c) => placed.get(c) ?? cursorAt(c.pos));
    v.normalize();
    pane.reveal(false);
    if (old !== v.primary.pos) this.editor.moved(v, old);
    pane.scheduleRender();
    return true;
  }

  // Move every cursor with `fn(cursor) -> position`.
  move(pane, fn, extend) {
    const v = pane.view;
    const old = v.primary.pos;
    for (const c of v.cursors) {
      const to = Math.max(0, Math.min(v.buf.len, fn(c)));
      const p = pane.settle(v, to, Math.sign(to - c.pos));
      if (extend) c.anchor = some(c.anchor ?? c.pos);
      else c.anchor = none();
      c.pos = p;
      c.goalX = none();
    }
    v.normalize();
    pane.reveal(false);
    if (old !== v.primary.pos) this.editor.moved(v, old);
    pane.scheduleRender();
  }

  // Put the primary cursor at `p` (one cursor), extending its selection.
  place(pane, p, extend) {
    const v = pane.view;
    const c = v.primary;
    const old = c.pos;
    v.cursors = [c];
    if (extend) c.anchor = some(c.anchor ?? old);
    else c.anchor = none();
    c.pos = Math.max(0, Math.min(v.buf.len, p));
    c.goalX = none();
    pane.reveal(false);
    if (old !== c.pos) this.editor.moved(v, old);
    pane.scheduleRender();
  }

  // ---------------------------------------------------------------- expand and shrink

  // A language server's selection ranges for each cursor ([[{start,
  // end}], ...] innermost first), or null when there is none or it has no
  // answer.
  async lspSelectionRanges(v) {
    const b = v.buf;
    if (!this.editor.lspAvailable(b)) return NO_RANGES;
    const positions = v.cursors.map((c) => this.editor.lspPosition(b, c.pos));
    if (positions.some((x) => !x)) return NO_RANGES;
    try {
      // Each position's ranges, innermost first (src/lsp_shapes.rs).
      /** const r: LspRange[][] */
      const r = await rpc("lsp.call", { doc: b.doc, method: "textDocument/selectionRange", params: { positions } });
      if (r.length !== v.cursors.length) return NO_RANGES;
      return r.map((ranges) => ranges.flatMap((x) => {
        const start = this.editor.lspOffset(b, x.start.line, x.start.character);
        const end = this.editor.lspOffset(b, x.end.line, x.end.character);
        return start != null && end != null ? [{ start, end }] : [];
      }));
    } catch { return NO_RANGES; }
  }

  // Expand each cursor's selection to the next range that holds it: the
  // language server's selection ranges where it gives them, else the text's
  // structure (`structureAround`). Shrink goes back the way it came.
  async expandSelection(pane) {
    const v = pane.view;
    const b = v.buf;
    if (!b.text) return;
    const before = v.cursors.map((c) => ({ pos: c.pos, anchor: c.anchor }));
    const lsp = await this.lspSelectionRanges(v);
    if (!pane.shown.includes(v)) return;
    let grew = false;
    v.cursors.forEach((c, i) => {
      const s = sel(c) ?? { start: c.pos, end: c.pos };
      const mine = listOf(lsp).map((l) => l.at(i) ?? []).at(0) ?? [];
      const ranges = (mine.length > 0 ? mine.concat(b.full ? [{ start: 0, end: b.len }] : []) : structureAround(this.editor.syntaxOf(b), b, s))
        .filter((r) => r.start <= s.start && r.end >= s.end && r.end - r.start > s.end - s.start);
      for (const r of ranges.sort((x, y) => (x.end - x.start) - (y.end - y.start)).slice(0, 1)) {
        c.anchor = some(r.start);
        c.pos = r.end;
        c.goalX = none();
        grew = true;
      }
    });
    if (!grew) return;
    const key = (cs) => cs.map((c) => `${c.anchor}:${c.pos}`).join();
    if (v.expansions.key !== key(before)) v.expansions = { stack: [], key: "" };
    v.expansions.stack.push(before);
    v.expansions.key = key(v.cursors);
    v.normalize();
    pane.reveal(false);
    pane.scheduleRender();
  }

  // ---------------------------------------------------------------- transpose

  // Swap the characters on either side of each caret and move past them;
  // at the end of a line, the two before it (as the editor and Emacs).
  transposeChars(pane) {
    const b = pane.view.buf;
    this.replace(pane, (c) => {
      if (sel(c)) return KEEP;
      const ln = lineAt(b, c.pos);
      const i = b.held.idxOf(ln.i, c.pos);
      const chars = [...ln.text];
      // The character index of the caret.
      const ci = [...ln.text.slice(0, i)].length;
      const at = ci >= chars.length ? ci - 1 : ci;
      if (at < 1 || chars.length < 2) return KEEP;
      const start = ln.start + byteLen(chars.slice(0, at - 1).join(""));
      const text = chars[at] + chars[at - 1];
      return put(start, start + byteLen(text), text);
    });
  }

  // Swap each caret's line with the line above it; the caret goes down
  // with it to the next line (as Emacs's transpose-lines).
  transposeLines(pane) {
    const b = pane.view.buf;
    const L = b.held;
    this.replace(pane, (c) => {
      const li = L.indexOf(c.pos);
      if (li < 1) return KEEP;
      const start = L.starts[li - 1], end = L.lineEnd(li);
      const text = L.lines[li] + "\n" + L.lines[li - 1];
      return putWith(start, end, text, byteLen(text) + (li + 1 < L.count ? 1 : 0), null);
    });
  }

  // ---------------------------------------------------------------- block selection

  // ---------------------------------------------------------------- folding

  // ---------------------------------------------------------------- motion

  left(pane, extend, word) {
    const b = pane.view.buf;
    this.move(pane, (c) => {
      if (!extend && !word) for (const s of listOf(sel(c))) return s.start;
      return word ? wordEdge(b, c.pos, -1) : c.pos - lenOf(charBefore(b, c.pos));
    }, extend);
  }
  right(pane, extend, word) {
    const b = pane.view.buf;
    this.move(pane, (c) => {
      if (!extend && !word) for (const s of listOf(sel(c))) return s.end;
      return word ? wordEdge(b, c.pos, 1) : c.pos + lenOf(charAtPos(b, c.pos));
    }, extend);
  }
  // Home: on a wrapped line's later rows, the start of the caret's row
  // (from there, the row before's); on a line's first row, its text's
  // start, then the line's.
  home(pane, extend) {
    const v = pane.view;
    const b = v.buf;
    this.move(pane, (c) => {
      for (const p of v.opts.wrap && v.mode === "source" ? pane.rowHome(v, c.pos) : []) return p;
      const ln = lineAt(b, c.pos);
      const indent = b.held.posAt(ln.i, ln.text.length - ln.text.trimStart().length);
      return c.pos === indent ? ln.start : indent;
    }, extend);
  }
  // End: the end of the caret's row of a wrapped line (from there, the next
  // row's), else of its line.
  end(pane, extend) {
    const v = pane.view;
    const b = v.buf;
    this.move(pane, (c) => {
      if (v.opts.wrap && v.mode === "source") return pane.rowEnd(v, c.pos);
      const ln = lineAt(b, c.pos);
      return crBefore(ln.text, ln.next !== null) ? ln.end - 1 : ln.end;
    }, extend);
  }
  vertical(pane, dir, extend) {
    const v = pane.view;
    if (v.cursors.length === 1) return pane.moveRows(dir, extend);
    // Several cursors: each moves a source line, keeping its column.
    const b = v.buf;
    this.move(pane, (c) => {
      const ln = lineAt(b, c.pos);
      const L = b.held;
      const j = ln.i + dir;
      if (j < 0 || j >= L.count) return c.pos;
      const col = L.idxOf(ln.i, c.pos);
      return L.posAt(j, clusterAt(L.lines[j], L.idxFrom(j, col)));
    }, extend);
  }
  paragraph(pane, dir, extend) {
    const L = pane.view.buf.held;
    this.move(pane, (c) => {
      let i = L.indexOf(c.pos) + dir;
      while (i > 0 && i < L.count - 1 && L.lines[i].trim() !== "") i += dir;
      i = Math.max(0, Math.min(L.count - 1, i));
      return L.starts[i];
    }, extend);
  }

  matchingBracket(pane) {
    const b = pane.view.buf;
    const syn = this.editor.syntaxOf(b);
    this.move(pane, (c) => listOf(bracketPair(syn, b, c.pos)).map((x) => x.match).at(0) ?? c.pos, false);
  }

  // ---------------------------------------------------------------- typing

  // Insert `text` at every cursor. `typed`: it was typed (not pasted or
  // inserted by a command), so a run of it is one undo step: the run goes
  // on while each insert starts at the carets the last one left.
  type(pane, text, typed) {
    const v = pane.view;
    const b = v.buf;
    const lang = b.language;
    const pairsOn = this.editor.autoClose() && lang !== "markdown";
    const carets = () => v.cursors.map((c) => c.pos).join();
    const run = typed && !text.includes("\n");
    const last = this.typed.at(0);
    const typing = !run ? "none" : last !== undefined && last.v === v && last.carets === carets() ? "continue" : "start";
    const done = () => { this.typed = run ? [{ v, carets: carets() }] : []; };
    if (text.length === 1) {
      const closer = PAIRS.get(text);
      // Overtype a closer that is already there.
      if ((CLOSE.has(text) || text === '"' || text === "'" || text === "`") && v.cursors.every((c) => !sel(c) && charAtPos(b, c.pos) === text) && pairsOn) {
        this.move(pane, (c) => c.pos + 1, false);
        return done();
      }
      if (closer && pairsOn) {
        const held = this.held(b, v.cursors, sel);
        if (held.length === 0) return done();
        // A quote typed inside a string or a comment (as the colours known
        // now say) is just a character.
        const syn = this.editor.syntaxOf(b);
        const literal = (p) => { const at = syn.categoryAt(p); return at.kind === "category" && (at.key === "syntax.string" || at.key === "syntax.comment"); };
        this.replaceTyped(pane, (c) => {
          // Surround the selection.
          for (const s of held.flatMap((h) => found(h.get(c)))) return putWith(s.start, s.end, text + s.text + closer, 1 + byteLen(s.text), 1);
          const next = charAtPos(b, c.pos);
          const prev = charBefore(b, c.pos);
          const quote = text === closer;
          const ok = (next == null || /\s/.test(next) || CLOSE.has(next) || next === ",") && !(quote && (isWord(prev) || (c.pos > 0 && literal(c.pos - 1))));
          return ok ? putWith(c.pos, c.pos, text + closer, 1, null) : put(c.pos, c.pos, text);
        }, typing);
        return done();
      }
    }
    this.replaceTyped(pane, (c) => {
      const s = sel(c);
      return s ? put(s.start, s.end, text) : put(c.pos, c.pos, text);
    }, typing);
    done();
  }

  newline(pane) {
    const b = pane.view.buf;
    const lang = b.language;
    this.replace(pane, (c) => {
      const s = sel(c) ?? { start: c.pos, end: c.pos };
      const ln = lineAt(b, s.start);
      const before = ln.text.slice(0, b.held.idxOf(ln.i, s.start));
      const indent = ln.text.slice(0, ln.text.length - ln.text.replace(/^[ \t]+/, "").length);
      if (lang === "markdown") {
        // Continue a list item.
        for (const m of listOf(/^(\s*)([-*+]|\d+[.)])\s+(\[[ x]\]\s+)?/.exec(before))) {
          if (before.trim() === m[0].trim()) return put(ln.start, s.end, "");
          const bullet = /\d/.test(m[2]) ? `${parseInt(m[2], 10) + 1}${m[2].slice(-1)}` : m[2];
          return put(s.start, s.end, `\n${m[1]}${bullet} ${m[3] !== undefined ? "[ ] " : ""}`);
        }
        return put(s.start, s.end, "\n" + indent);
      }
      const t = before.trimEnd();
      const opens = /[{([]$/.test(t) || (lang === "python" && t.endsWith(":"));
      const next = charAtPos(b, s.end);
      if (opens && next != null && CLOSE.has(next) && PAIRS.get(t.slice(-1)) === next) {
        const inner = "\n" + indent + INDENT;
        return putWith(s.start, s.end, inner + "\n" + indent, byteLen(inner), null);
      }
      return put(s.start, s.end, "\n" + indent + (opens ? INDENT : ""));
    });
  }

  backspace(pane, word) {
    const b = pane.view.buf;
    this.replace(pane, (c) => {
      const s = sel(c);
      if (s) return put(s.start, s.end, "");
      if (c.pos === 0) return KEEP;
      if (word) return put(wordEdge(b, c.pos, -1), c.pos, "");
      const ln = lineAt(b, c.pos);
      const before = ln.text.slice(0, b.held.idxOf(ln.i, c.pos));
      // In leading whitespace, remove one indent level.
      if (before.length > 0 && /^ +$/.test(before)) {
        const n = before.length % 4 === 0 ? 4 : before.length % 4;
        return put(c.pos - n, c.pos, "");
      }
      const prev = charBefore(b, c.pos), next = charAtPos(b, c.pos);
      if (listOf(prev).some((p) => PAIRS.get(p) === next) && this.editor.autoClose()) return put(c.pos - 1, c.pos + 1, "");
      return put(c.pos - lenOf(prev), c.pos, "");
    });
  }

  del(pane, word) {
    const b = pane.view.buf;
    this.replace(pane, (c) => {
      const s = sel(c);
      if (s) return put(s.start, s.end, "");
      const next = charAtPos(b, c.pos);
      if (next == null) return KEEP;
      return putWith(c.pos, word ? wordEdge(b, c.pos, 1) : c.pos + byteLen(next), "", 0, null);
    });
  }

  indent(pane, dir) {
    const v = pane.view;
    const b = v.buf;
    const L = b.held;
    if (dir > 0 && v.cursors.every((c) => !sel(c))) return this.type(pane, INDENT, false);
    const sels = v.cursors.map((c) => ({ c, s: sel(c), span: lineSpan(b, c) }));
    const lines = new Set();
    for (const x of sels) for (let i = x.span[0]; i <= x.span[1]; i++) lines.add(i);
    const ops = [];
    for (const i of [...lines].sort((a, z) => z - a)) {
      if (dir > 0) ops.push(insOp(L.starts[i], INDENT));
      else {
        // An indent's spaces (up to four), else a tab.
        const line = L.lines[i];
        const spaces = Math.min(4, line.length - line.replace(/^ +/, "").length);
        const n = spaces > 0 ? spaces : line.startsWith("\t") ? 1 : 0;
        if (n > 0) ops.push(delOp(L.starts[i], n));
      }
    }
    if (ops.length) this.editor.edit(b, ops, "none");
    pane.scheduleRender();
  }

  // ---------------------------------------------------------------- line commands

  deleteLine(pane) {
    const b = pane.view.buf;
    const L = b.held;
    this.replace(pane, (c) => {
      const [a, z] = lineSpan(b, c);
      const end = z + 1 < L.count ? L.starts[z + 1] : L.lineEnd(z);
      const start = z + 1 < L.count || a === 0 ? L.starts[a] : L.starts[a] - 1;
      return putWith(start, end, "", 0, null);
    });
  }

  duplicateLine(pane) {
    const b = pane.view.buf;
    const L = b.held;
    this.replace(pane, (c) => {
      const [a, z] = lineSpan(b, c);
      const text = L.lines.slice(a, z + 1).join("\n");
      const col = c.pos - L.starts[a];
      return putWith(L.lineEnd(z), L.lineEnd(z), "\n" + text, 1 + col, null);
    });
  }

  // ---------------------------------------------------------------- transforms

  // Each cursor's selection (or the word at it) through `f`, still
  // selected afterwards.
  transform(pane, f) {
    const b = pane.view.buf;
    for (const held of this.held(b, pane.view.cursors, (c) => sel(c) ?? wordAt(b, c.pos, Infinity))) {
      this.replace(pane, (c) => {
        for (const s of found(held.get(c))) {
          const text = f(s.text);
          return putWith(s.start, s.end, text, byteLen(text), 0);
        }
        return KEEP;
      });
    }
  }

  // The lines each selection covers, sorted (case-sensitively, as the
  // editor's Sort Lines) and selected.
  sortLines(pane) {
    const b = pane.view.buf;
    const L = b.held;
    this.replace(pane, (c) => {
      const [a, z] = lineSpan(b, c);
      if (z <= a) return KEEP;
      const text = L.lines.slice(a, z + 1).sort((x, y) => (x < y ? -1 : x > y ? 1 : 0)).join("\n");
      return putWith(L.starts[a], L.lineEnd(z), text, byteLen(text), 0);
    });
  }

  // Remove the spaces and tabs at the end of every line (a file held whole).
  trimTrailing(pane) {
    const b = pane.view.buf;
    const L = b.held;
    if (!b.full) { this.editor.say("Only a file held whole can be trimmed"); return; }
    const ops = [];
    for (let i = L.count - 1; i >= 0; i--) {
      const line = L.lines[i];
      const kept = line.replace(/[ \t]+$/, "");
      if (kept.length < line.length) ops.push(delOp(L.starts[i] + byteLen(kept), byteLen(line) - byteLen(kept)));
    }
    if (ops.length > 0) this.editor.edit(b, ops, "none");
    this.editor.say(ops.length > 0 ? `Trimmed ${ops.length} line${ops.length === 1 ? "" : "s"}` : "No trailing whitespace");
    pane.scheduleRender();
  }

  moveLines(pane, dir) {
    const v = pane.view;
    const b = v.buf;
    const L = b.held;
    if (v.cursors.length > 1) return;
    const c = v.cursors[0];
    const [a, z] = lineSpan(b, c);
    if ((dir < 0 && a === 0) || (dir > 0 && z >= L.count - 1)) return;
    const block = L.lines.slice(a, z + 1).join("\n");
    const off = c.pos - L.starts[a];
    // The anchor's offset into the block (none: no selection).
    const anchorOff = listOf(c.anchor).map((x) => x - L.starts[a]);
    // The block and the line beside it swap: [start, end) becomes `text`,
    // the block then starting at `newStart`.
    const up = dir < 0;
    const start = up ? L.starts[a - 1] : L.starts[a];
    const end = up ? L.lineEnd(z) : L.lineEnd(z + 1);
    const text = up ? block + "\n" + L.lines[a - 1] : L.lines[z + 1] + "\n" + block;
    const newStart = up ? start : start + byteLen(L.lines[z + 1]) + 1;
    this.editor.edit(b, [delOp(start, end - start), insOp(start, text)], "none");
    c.pos = newStart + off;
    c.anchor = anchorOff.length > 0 ? some(newStart + anchorOff[0]) : none();
    pane.reveal(false);
    pane.scheduleRender();
  }

  // Comment the lines the cursors are on with the language's line comment,
  // or uncomment them when they all are.
  toggleComment(pane) {
    const b0 = pane.view.buf;
    for (const prefix of listOf(b0.comment)) { this.toggleLineComment(pane, prefix); return; }
    this.editor.say(`No comments in ${b0.language ?? "plain text"}`);
  }
  toggleLineComment(pane, prefix) {
    const v = pane.view;
    const b = v.buf;
    const L = b.held;
    const lines = new Set();
    for (const c of v.cursors) { const [a, z] = lineSpan(b, c); for (let i = a; i <= z; i++) lines.add(i); }
    const idx = [...lines].filter((i) => L.lines[i].trim() !== "");
    if (idx.length === 0) return;
    const commented = idx.every((i) => L.lines[i].trimStart().startsWith(prefix));
    const minIndent = idx.map((i) => L.lines[i].length - L.lines[i].trimStart().length).reduce((m, n) => Math.min(m, n), Infinity);
    const ops = [];
    for (const i of idx.sort((a, z) => z - a)) {
      const t = L.lines[i];
      if (commented) {
        const at = t.indexOf(prefix);
        const n = byteLen(prefix) + (t.charAt(at + prefix.length) === " " ? 1 : 0);
        ops.push(delOp(L.starts[i] + byteLen(t.slice(0, at)), n));
      } else ops.push(insOp(L.starts[i] + byteLen(t.slice(0, minIndent)), prefix + " "));
    }
    // Ops above are per line, last line first, so each stays valid.
    this.editor.edit(b, ops, "none");
    pane.scheduleRender();
  }

  // ---------------------------------------------------------------- multiple cursors

  selectNextOccurrence(pane) {
    const v = pane.view;
    const b = v.buf;
    const prim = v.cursors[v.cursors.length - 1];
    const s = sel(v.cursors[0]);
    if (!s) {
      const w = wordAt(b, v.primary.pos, Infinity);
      for (const x of listOf(w)) { v.primary.anchor = some(x.start); v.primary.pos = x.end; }
      pane.scheduleRender();
      return;
    }
    for (const needle of listOf(b.textRange(s.start, s.end))) this.selectNext(pane, needle, sel(prim) ?? s);
  }

  // A cursor added selecting the next `needle` after `lastSel`.
  selectNext(pane, needle, lastSel) {
    const v = pane.view;
    const L = v.buf.held;
    const found = nextIn(L, needle, lastSel.end);
    const wrapped = found.length > 0 ? found : nextIn(L, needle, L.start);
    if (wrapped.length === 0) return;
    const start = wrapped[0];
    if (v.cursors.some((c) => Math.min(c.pos, c.anchor ?? c.pos) === start)) { this.editor.say("All occurrences selected"); return; }
    v.cursors.push(selecting(start + byteLen(needle), start));
    v.normalize();
    pane.reveal(false);
    pane.scheduleRender();
  }

  selectAllOccurrences(pane) {
    const v = pane.view;
    const b = v.buf;
    const found = [...listOf(sel(v.cursors[0])), ...listOf(wordAt(b, v.primary.pos, Infinity))];
    if (found.length === 0) return;
    const s = found[0];
    for (const needle of listOf(b.textRange(s.start, s.end))) this.selectAll(pane, needle, s.start);
  }

  // Every `needle` selected, the one at `from` first.
  selectAll(pane, needle, from) {
    const v = pane.view;
    const L = v.buf.held;
    const out = [];
    const n = byteLen(needle);
    for (let at = nextIn(L, needle, L.start); at.length > 0; at = nextIn(L, needle, at[0] + n)) out.push(selecting(at[0] + n, at[0]));
    if (out.length === 0) return;
    const first = out.findIndex((c) => c.anchor === from);
    if (first > 0) out.unshift(out.splice(first, 1)[0]);
    v.cursors = out;
    this.editor.say(`${out.length} occurrences selected`);
    pane.scheduleRender();
  }
}

