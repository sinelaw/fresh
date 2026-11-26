// Editing commands over every cursor of a view.
//
// Every edit is a set of replacements, one per cursor, applied as one batch
// (one undo step) from the last position to the first so earlier positions
// stay valid; each caret is then placed where its replacement says.

import { listOf, found, some, none } from "./maybe.js";
import { byteLen, insOp, delOp } from "./ot.js";
import { byteToIdx } from "./buffer.js";
import { sel, cursorAt, selecting } from "./view.js";
import { rpc } from "./rpc.js";
import { charWidth } from "./layout.js";
import { foldRange as docFoldRange } from "./doc.js";

const PAIRS = new Map([["(", ")"], ["[", "]"], ["{", "}"], ['"', '"'], ["'", "'"], ["`", "`"]]);
const CLOSERS = new Set([")", "]", "}"]);
const COMMENT = new Map([
  ["rust", "//"], ["javascript", "//"], ["typescript", "//"], ["go", "//"], ["c", "//"], ["cpp", "//"], ["java", "//"], ["css", "/*"],
  ["python", "#"], ["shell", "#"], ["toml", "#"], ["yaml", "#"], ["ruby", "#"], ["markdown", "<!--"], ["html", "<!--"],
]);
const INDENT = "    ";

// Each bracket's partner.
const OPEN = new Map([["(", ")"], ["[", "]"], ["{", "}"]]), CLOSE = new Map([[")", "("], ["]", "["], ["}", "{"]]);
/** function isBracket(c: String | Null) => Boolean */
function isBracket(c) { return c !== null && (OPEN.has(c) || CLOSE.has(c)); }

// The unmatched opening bracket before index `k` of line `li` (scanning
// back at most BRACKET_SCAN lines): its byte offset, or null.
/** function openerIn(lines: String[], starts: Number[], li: Number, k: Number) => Number | Null */
function openerIn(lines, starts, li, k) {
  /** const depth: Map<String, Number> */
  const depth = new Map();
  let line = li, i = k;
  for (let n = 0; n < BRACKET_SCAN && line >= 0; n++) {
    const text = lines[line];
    for (; i >= 0; i--) {
      const c = text[i];
      const opener = CLOSE.get(c);
      if (opener !== undefined) depth.set(opener, (depth.get(opener) ?? 0) + 1);
      else if (OPEN.has(c)) {
        const d = depth.get(c) ?? 0;
        if (d === 0) return starts[line] + byteLen(text.slice(0, i));
        depth.set(c, d - 1);
      }
    }
    line--;
    if (line >= 0) i = lines[line].length - 1;
  }
  return null;
}

// The partner of bracket `ch` at index `k` of line `li` (byte offset `at`),
// scanning forward from an opener and back from a closer.
/** function partnerOf(lines: String[], starts: Number[], li: Number, k: Number, ch: String, at: Number) => {at: Number, match: Number} | Null */
function partnerOf(lines, starts, li, k, ch, at) {
  const opener = OPEN.get(ch), closer = CLOSE.get(ch);
  const fwd = opener !== undefined, want = opener ?? closer ?? ch;
  let line = li, i = k, depth = 0;
  for (let n = 0; n < BRACKET_SCAN && line >= 0 && line < lines.length; n++) {
    const text = lines[line];
    for (; i >= 0 && i < text.length; i += fwd ? 1 : -1) {
      if (text[i] === ch) depth++;
      else if (text[i] === want && --depth === 0) return { at, match: starts[line] + byteLen(text.slice(0, i)) };
    }
    line += fwd ? 1 : -1;
    if (line >= 0 && line < lines.length) i = fwd ? 0 : lines[line].length - 1;
  }
  return null;
}
// No block folds there.
/** function noFold() => Range[] */
function noFold() { return []; }

// No language server's selection ranges (none runs, or it has no answer).
/** const NO_RANGES: Range[][] | Null */
const NO_RANGES = null;

// How many lines a bracket's match is looked for in.
const BRACKET_SCAN = 5000;
const TAB = 4;

// The screen column of UTF-16 index `idx` in `line` (tabs to the next stop,
// wide characters two columns), and the index at or before column `col`.
/** function colOf(line: String, idx: Number) => Number */
function colOf(line, idx) {
  let c = 0;
  for (let i = 0; i < idx && i < line.length; i++) {
    const u = line.charCodeAt(i);
    if (u >= 0xdc00 && u < 0xe000) continue; // the second half of a pair
    c = line[i] === "\t" ? c + TAB - (c % TAB) : c + charWidth(line.codePointAt(i));
  }
  return c;
}
/** function idxAt(line: String, col: Number) => Number */
function idxAt(line, col) {
  let c = 0, i = 0;
  while (i < line.length) {
    const w = line[i] === "\t" ? TAB - (c % TAB) : charWidth(line.codePointAt(i));
    if (c + w > col) break;
    c += w;
    i += line.codePointAt(i) > 0xffff ? 2 : 1;
  }
  return i;
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
/** type Replacement = {start: Number, end: Number, text: String, caret: Number | Null, anchor: Number | Null} */
/** function putWith(start: Number, end: Number, text: String, caret: Number | Null, anchor: Number | Null) => Replacement | Null */
export function putWith(start, end, text, caret, anchor) { return { start, end, text, caret, anchor }; }
// [start, end) replaced by `text`, the caret after it.
/** function put(start: Number, end: Number, text: String) => Replacement | Null */
export function put(start, end, text) { return putWith(start, end, text, null, null); }
// A cursor's text left alone.
/** const KEEP: Replacement | Null */
export const KEEP = null;

// Each cursor's replacement, last in the text first.
/** function replacements(cursors: Cursor[], fn: (Cursor) => Replacement | Null) => {cursor: Cursor, r: Replacement}[] */
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
/** function placesAfter(reps: {cursor: Cursor, r: Replacement}[]) => {cursor: Cursor, at: Cursor}[] */
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

// The character before byte `p` of buffer `b` (a newline between lines;
// none at the start, or outside the held text).
function charBefore(b, p) {
  if (p <= 0) return none();
  for (const ln of linesAt(b, p)) {
    const idx = byteToIdx(ln.text, p - ln.start);
    if (idx === 0) return ln.i > 0 || b.held.start < ln.start ? some("\n") : none();
    const c = ln.text.charCodeAt(idx - 1);
    return some(idx >= 2 && c >= 0xdc00 && c < 0xe000 ? ln.text.slice(idx - 2, idx) : ln.text.charAt(idx - 1));
  }
  return none();
}

// The character at byte `p` of buffer `b` (a newline at a line's end;
// none at the end, or outside the held text).
function charAtPos(b, p) {
  if (p >= b.len) return none();
  for (const ln of linesAt(b, p)) {
    const idx = byteToIdx(ln.text, p - ln.start);
    return some(idx >= ln.text.length ? "\n" : String.fromCodePoint(ln.text.codePointAt(idx)));
  }
  return none();
}

/** function isWord(c: String | Null) => Boolean */
function isWord(c) { return listOf(c).some((x) => /[\p{L}\p{N}_]/u.test(x)); }
// A character's length in bytes (none: 0).
/** function lenOf(c: String | Null) => Number */
function lenOf(c) { return listOf(c).map(byteLen).at(0) ?? 0; }

function languageOf(b) { return b.language ?? (/\.(md|markdown)$/.test(b.path) ? "markdown" : null); }

// The unmatched opening bracket before `p` (in the held lines), or null.
function openerBefore(b, p) {
  const L = b.held;
  const li = L.indexOf(p);
  if (li < 0) return null;
  return openerIn(L.lines, L.starts, li, byteToIdx(L.lines[li], p - L.starts[li]) - 1);
}

export function shrinkSelection(pane) {
  const v = pane.view;
  const key = v.cursors.map((c) => `${c.anchor}:${c.pos}`).join();
  const prev = v.expansions.key === key ? v.expansions.stack.pop() : undefined;
  if (prev !== undefined) {
    v.cursors = prev.map((c) => selecting(c.pos, c.anchor));
    v.expansions.key = v.cursors.map((c) => `${c.anchor}:${c.pos}`).join();
  } else for (const c of v.cursors) c.anchor = null;
  pane.reveal(false);
  pane.scheduleRender();
}

function blockKey(v) { return v.cursors.map((c) => `${c.anchor}:${c.pos}`).join(); }

// Where a position is as a block's corner: {line, col}.
function cornerAt(b, p) {
  const L = b.text;
  const li = L.indexOf(p);
  return { line: li, col: colOf(L.lines[li], byteToIdx(L.lines[li], p - L.starts[li])) };
}

// The block that starts at window line `li`: the language server's range
// when it gave one, else the lines indented deeper than it. A compose
// view folds its document's blocks (a heading's section, a list's deeper
// items, a code block's lines).
function foldRangeAt(v, li) {
  if (v.mode === "compose") return listOf(docFoldRange(v, li));
  const b = v.buf;
  const L = b.held;
  const first = L.firstLine ?? 0;
  // The language server's range, else the lines indented past this one.
  const lsp = (b.foldRanges ?? []).filter((r) => r.start === li + first && r.end > r.start).slice(0, 1);
  if (lsp.length > 0) return lsp.map((r) => ({ start: li, end: Math.min(L.count - 1, r.end - first) }));
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
  const edge = dir < 0 ? v.cursors.reduce((m, c) => (c.pos < m.pos ? c : m)) : v.cursors.reduce((m, c) => (c.pos > m.pos ? c : m));
  const ln = lineAt(b, edge.pos);
  const j = ln.i + dir;
  if (j < 0 || j >= L.count) return;
  const col = byteToIdx(ln.text, edge.pos - ln.start);
  v.cursors.push(cursorAt(L.starts[j] + byteLen(L.lines[j].slice(0, Math.min(col, L.lines[j].length)))));
  v.normalize();
  pane.scheduleRender();
}

function wordEdge(b, p, dir) {
  const step = (x) => (dir < 0 ? charBefore(b, x) : charAtPos(b, x));
  let q = p;
  while (step(q) !== null && !isWord(step(q)) && step(q) !== "\n") q += dir * lenOf(step(q));
  while (isWord(step(q))) q += dir * lenOf(step(q));
  if (q === p && step(q) !== null) q += dir * lenOf(step(q));
  return q;
}

export function wordAt(b, p) {
  let s = p, e = p;
  while (isWord(charBefore(b, s))) s -= lenOf(charBefore(b, s));
  while (isWord(charAtPos(b, e))) e += lenOf(charAtPos(b, e));
  return s < e ? { start: s, end: e } : null;
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
    const at = (col) => L.starts[k] + byteLen(line.slice(0, idxAt(line, col)));
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
    for (const c of v.cursors) for (const h of pane.foldHiding(v, L.indexOf(c.pos))) { c.pos = L.lineEnd(h.keep); c.anchor = null; }
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

// The bracket at `p` (or just before it) and its match: {at, match}
// byte offsets, or null. Scans the held lines out from it, at most
// `BRACKET_SCAN` lines, a line at a time.
export function bracketPair(b, p) {
  const L = b.text;
  if (!L) return null;
  // The bracket at the caret, else the one before it.
  const here = charAtPos(b, p), before = charBefore(b, p);
  const onHere = isBracket(here);
  const ch = onHere ? here : before, at = onHere ? p : p - 1;
  if (ch === null || !isBracket(ch)) return null;
  const li = L.indexOf(at);
  if (li < 0) return null;
  return partnerOf(L.lines, L.starts, li, byteToIdx(L.lines[li], at - L.starts[li]), ch, at);
}

export function selectLine(pane) {
  const b = pane.view.buf;
  const L = b.held;
  for (const c of pane.view.cursors) {
    const [a, z] = lineSpan(b, c);
    c.anchor = L.starts[a];
    c.pos = z + 1 < L.count ? L.starts[z + 1] : L.lineEnd(z);
  }
  pane.scheduleRender();
}

// The ranges that hold `s` ({start, end}), smallest first, from the
// text's structure: the word, a string's inside and the string, each
// bracket pair's inside and the pair outwards, the line's text and the
// line, each indentation block outwards (its lines), the document.
function structureAround(b, s) {
  const L = b.held;
  const out = [];
  const add = (start, end) => { if (start <= s.start && end >= s.end && end > start) out.push({ start, end }); };
  const w = wordAt(b, s.start);
  if (w) add(w.start, w.end);
  const li = L.indexOf(s.start);
  if (li < 0) return out;
  const line = L.lines[li], ls = L.starts[li];
  // Strings on the line: quote pairs, left to right.
  for (let i = 0; i < line.length; i++) {
    const q = line[i];
    if (q !== '"' && q !== "'" && q !== "`") continue;
    let j = i + 1;
    while (j < line.length && line[j] !== q) j += line[j] === "\\" ? 2 : 1;
    if (j >= line.length) break;
    const a = ls + byteLen(line.slice(0, i)), z = ls + byteLen(line.slice(0, j + 1));
    add(a + 1, z - 1);
    add(a, z);
    i = j;
  }
  // Bracket pairs around it, outwards.
  let from = s.start;
  for (let guard = 0; guard < 64; guard++) {
    const open = openerBefore(b, from);
    if (open == null) break;
    const pair = bracketPair(b, open);
    if (!pair || pair.match < s.end) { from = open; continue; }
    add(open + 1, pair.match);
    add(open, pair.match + 1);
    from = open;
  }
  // The line's text, then the line with its break.
  const text0 = ls + byteLen((/^\s*/.exec(line) ?? [""])[0]);
  add(text0, ls + byteLen(line.replace(/\s+$/, "")));
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
  const L = v.buf.text;
  if (!L) return;
  const kept = v.block.find((b) => b.key === blockKey(v));
  const anchor = kept === undefined ? cornerAt(v.buf, v.primary.pos) : kept.anchor;
  const head = kept === undefined ? { ...anchor } : { ...kept.head };
  head.line = Math.max(0, Math.min(L.count - 1, head.line + dl));
  head.col = Math.max(0, head.col + dc);
  setBlock(pane, anchor, head);
}

export function selectWord(pane) {
  const v = pane.view;
  const w = wordAt(v.buf, v.primary.pos);
  if (w) { v.primary.anchor = w.start; v.primary.pos = w.end; }
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
      if (extend) { if (c.anchor == null) c.anchor = c.pos; }
      else c.anchor = null;
      c.pos = p;
      c.goalX = null;
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
    if (extend) { if (c.anchor == null) c.anchor = old; }
    else c.anchor = null;
    c.pos = Math.max(0, Math.min(v.buf.len, p));
    c.goalX = null;
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
    if (pane.view !== v) return;
    let grew = false;
    v.cursors.forEach((c, i) => {
      const s = sel(c) ?? { start: c.pos, end: c.pos };
      const mine = lsp === null ? [] : lsp.at(i) ?? [];
      const ranges = (mine.length > 0 ? mine.concat(b.full ? [{ start: 0, end: b.len }] : []) : structureAround(b, s))
        .filter((r) => r.start <= s.start && r.end >= s.end && r.end - r.start > s.end - s.start);
      const r = ranges.sort((x, y) => (x.end - x.start) - (y.end - y.start))[0];
      if (!r) return;
      c.anchor = r.start;
      c.pos = r.end;
      c.goalX = null;
      grew = true;
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
      const i = byteToIdx(ln.text, c.pos - ln.start);
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
      const s = sel(c);
      if (!extend && !word && s) return s.start;
      return word ? wordEdge(b, c.pos, -1) : c.pos - lenOf(charBefore(b, c.pos));
    }, extend);
  }
  right(pane, extend, word) {
    const b = pane.view.buf;
    this.move(pane, (c) => {
      const s = sel(c);
      if (!extend && !word && s) return s.end;
      return word ? wordEdge(b, c.pos, 1) : c.pos + lenOf(charAtPos(b, c.pos));
    }, extend);
  }
  home(pane, extend) {
    const b = pane.view.buf;
    this.move(pane, (c) => {
      const ln = lineAt(b, c.pos);
      const indent = ln.start + byteLen(ln.text.slice(0, ln.text.length - ln.text.trimStart().length));
      return c.pos === indent ? ln.start : indent;
    }, extend);
  }
  end(pane, extend) {
    const b = pane.view.buf;
    this.move(pane, (c) => lineAt(b, c.pos).end, extend);
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
      const col = byteToIdx(ln.text, c.pos - ln.start);
      return L.starts[j] + byteLen(L.lines[j].slice(0, col));
    }, extend);
  }
  paragraph(pane, dir, extend) {
    const b = pane.view.buf;
    const L = b.text;
    this.move(pane, (c) => {
      let i = L.indexOf(c.pos) + dir;
      while (i > 0 && i < L.count - 1 && L.lines[i].trim() !== "") i += dir;
      i = Math.max(0, Math.min(L.count - 1, i));
      return L.starts[i];
    }, extend);
  }

  matchingBracket(pane) {
    const b = pane.view.buf;
    this.move(pane, (c) => listOf(bracketPair(b, c.pos)).map((x) => x.match).at(0) ?? c.pos, false);
  }

  // ---------------------------------------------------------------- typing

  // Insert `text` at every cursor. `typed`: it was typed (not pasted or
  // inserted by a command), so a run of it is one undo step: the run goes
  // on while each insert starts at the carets the last one left.
  type(pane, text, typed = false) {
    const v = pane.view;
    const b = v.buf;
    const lang = languageOf(b);
    const pairsOn = this.editor.autoClose() && lang !== "markdown";
    const carets = () => v.cursors.map((c) => c.pos).join();
    const run = typed && !text.includes("\n");
    const last = this.typed.at(0);
    const typing = !run ? "none" : last !== undefined && last.v === v && last.carets === carets() ? "continue" : "start";
    const done = () => { this.typed = run ? [{ v, carets: carets() }] : []; };
    if (text.length === 1) {
      const closer = PAIRS.get(text);
      // Overtype a closer that is already there.
      if ((CLOSERS.has(text) || text === '"' || text === "'" || text === "`") && v.cursors.every((c) => !sel(c) && charAtPos(b, c.pos) === text) && pairsOn) {
        this.move(pane, (c) => c.pos + 1, false);
        return done();
      }
      if (closer && pairsOn) {
        this.replaceTyped(pane, (c) => {
          const s = sel(c);
          if (s) {
            // Surround the selection.
            const inner = b.textRange(s.start, s.end) ?? "";
            return putWith(s.start, s.end, text + inner + closer, 1 + byteLen(inner), 1);
          }
          const next = charAtPos(b, c.pos);
          const prev = charBefore(b, c.pos);
          const quote = text === closer;
          const ok = (next == null || /\s/.test(next) || CLOSERS.has(next) || next === ",") && !(quote && isWord(prev));
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
    const lang = languageOf(b);
    this.replace(pane, (c) => {
      const s = sel(c) ?? { start: c.pos, end: c.pos };
      const ln = lineAt(b, s.start);
      const before = ln.text.slice(0, byteToIdx(ln.text, s.start - ln.start));
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
      if (opens && next != null && CLOSERS.has(next) && PAIRS.get(t.slice(-1)) === next) {
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
      const before = ln.text.slice(0, byteToIdx(ln.text, c.pos - ln.start));
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
    if (dir > 0 && v.cursors.every((c) => !sel(c))) return this.type(pane, INDENT);
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
    this.replace(pane, (c) => {
      const s = sel(c) ?? wordAt(b, c.pos);
      if (!s) return KEEP;
      const text = f(b.textRange(s.start, s.end) ?? "");
      return putWith(s.start, s.end, text, byteLen(text), 0);
    });
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
    const anchorAt = c.anchor;
    const anchorOff = anchorAt !== null ? anchorAt - L.starts[a] : null;
    let start, end, text, newStart;
    if (dir < 0) {
      const prev = L.lines[a - 1];
      start = L.starts[a - 1]; end = L.lineEnd(z);
      text = block + "\n" + prev;
      newStart = start;
    } else {
      const next = L.lines[z + 1];
      start = L.starts[a]; end = L.lineEnd(z + 1);
      text = next + "\n" + block;
      newStart = start + byteLen(next) + 1;
    }
    this.editor.edit(b, [delOp(start, end - start), insOp(start, text)], "none");
    c.pos = newStart + off;
    c.anchor = anchorOff != null ? newStart + anchorOff : null;
    pane.reveal(false);
    pane.scheduleRender();
  }

  toggleComment(pane) {
    const v = pane.view;
    const b = v.buf;
    const L = b.held;
    const prefix = listOf(languageOf(b)).flatMap((l) => found(COMMENT.get(l))).at(0) ?? "//";
    const close = prefix === "<!--" ? " -->" : prefix === "/*" ? " */" : "";
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
        if (close !== "" && t.trimEnd().endsWith(close.trim())) {
          const e = byteLen(t.trimEnd()) - byteLen(close.trim());
          const sp = t.charAt(byteToIdx(t, e) - 1) === " " ? 1 : 0;
          ops.push(delOp(L.starts[i] + e - sp, byteLen(close.trim()) + sp));
        }
        const at = t.indexOf(prefix);
        const n = prefix.length + (t.charAt(at + prefix.length) === " " ? 1 : 0);
        ops.push(delOp(L.starts[i] + byteLen(t.slice(0, at)), n));
      } else {
        if (close !== "") ops.push(insOp(L.lineEnd(i), close));
        ops.push(insOp(L.starts[i] + byteLen(t.slice(0, minIndent)), prefix + " "));
      }
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
      const w = wordAt(b, v.primary.pos);
      if (w) { v.primary.anchor = w.start; v.primary.pos = w.end; }
      pane.scheduleRender();
      return;
    }
    const needle = b.textRange(s.start, s.end) ?? "";
    if (needle === "") return;
    const L = b.held;
    const all = L.lines.join("\n");
    const lastSel = sel(prim) ?? s;
    let i = all.indexOf(needle, byteToIdx(all, lastSel.end - L.start));
    if (i < 0) i = all.indexOf(needle);
    if (i < 0) return;
    const start = L.start + byteLen(all.slice(0, i));
    if (v.cursors.some((c) => Math.min(c.pos, c.anchor ?? c.pos) === start)) { this.editor.say("All occurrences selected"); return; }
    v.cursors.push(selecting(start + byteLen(needle), start));
    v.normalize();
    pane.reveal(false);
    pane.scheduleRender();
  }

  selectAllOccurrences(pane) {
    const v = pane.view;
    const b = v.buf;
    const found = [...listOf(sel(v.cursors[0])), ...listOf(wordAt(b, v.primary.pos))];
    if (found.length === 0) return;
    const s = found[0];
    const needle = b.textRange(s.start, s.end) ?? "";
    if (needle === "") return;
    const L = b.held;
    const all = L.lines.join("\n");
    const out = [];
    for (let i = all.indexOf(needle); i >= 0; i = all.indexOf(needle, i + needle.length)) {
      const start = L.start + byteLen(all.slice(0, i));
      out.push(selecting(start + byteLen(needle), start));
    }
    if (out.length === 0) return;
    const first = out.findIndex((c) => c.anchor === s.start);
    if (first > 0) out.unshift(out.splice(first, 1)[0]);
    v.cursors = out;
    this.editor.say(`${out.length} occurrences selected`);
    pane.scheduleRender();
  }
}

