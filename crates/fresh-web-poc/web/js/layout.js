// Visual layout of a buffer for a source view: source lines and decorations
// to rows of monospace cells, at the view's own width. (A compose view is
// drawn as a document instead: doc.js.)
//
//  * A conceal draws its text instead of [start, end); one that covers a
//    newline joins the next source line onto the same row. With `reveal`, it
//    is inactive while a cursor is inside that range, so the source shows
//    where the user is editing.
//  * Inline text is drawn before or after the character at its position, or
//    with `edge` at the right edge of the row it falls in.
//  * Lines are whole rows above or below their position's line.
//  * With wrapping on, a row breaks at the last space that fits (else at the
//    width).
//  * A view's folds hide whole lines after a fold's first line.
// A decoration with `when` applies only to a view in that mode.

import { byteLen, byteToIdx } from "./utf8.js";
import { EMPTY, restyler, drawnOf } from "./paint.js";

// A folded line's marker; a shown tab or space.
const FOLD_MARK = drawnOf({ fg: "editor.line_number_fg", bg: null, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false });
const WHITESPACE = restyler({ fg: "editor.whitespace_indicator_fg", bg: null, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false });

export function charWidth(cp) {
  if (cp === 0) return 0;
  if (cp < 0x1100) return 1;
  if (
    (cp >= 0x1100 && cp <= 0x115f) || (cp >= 0x2e80 && cp <= 0xa4cf && cp !== 0x303f) ||
    (cp >= 0xac00 && cp <= 0xd7a3) || (cp >= 0xf900 && cp <= 0xfaff) || (cp >= 0xfe30 && cp <= 0xfe4f) ||
    (cp >= 0xff00 && cp <= 0xff60) || (cp >= 0xffe0 && cp <= 0xffe6) ||
    (cp >= 0x1f300 && cp <= 0x1f64f) || (cp >= 0x1f900 && cp <= 0x1f9ff) || (cp >= 0x20000 && cp <= 0x3fffd)
  ) return 2;
  if ((cp >= 0x300 && cp <= 0x36f) || cp === 0x200b || (cp >= 0xfe00 && cp <= 0xfe0f)) return 0;
  return 1;
}
export function stringWidth(s) {
  let w = 0;
  for (const ch of s) w += charWidth(ch.codePointAt(0));
  return w;
}

export const applies = (d, mode) => d.when === "always" || d.when === mode;
export const cursorIn = (cursors, r) => cursors.some((c) => c >= r.start && c < Math.max(r.end, r.start + 1));


// A row's cell: its character, the byte it shows (none: text that is not
// the buffer's), its style and its width in columns.
/** type Cell = {ch: String, src: Number | Null, style: Drawn, w: Number} */
// A row: of source text ("text": its line, whether it continues a wrapped
// line, the byte where its line ends when it does (none: it wraps or a
// conceal runs on), the background its last overlay carries to the edge)
// or a plugin's whole row ("virtual", no line). `edge`: inline texts held
// for the row's right edge.
/** type Row = {
 *   kind: String,
 *   cells: Cell[],
 *   col: Number,
 *   lineIdx: Number | Null,
 *   cont: Boolean,
 *   eol: Number | Null,
 *   fill: String | Null,
 *   edge: InlineText[],
 * } */
/** function textRow(lineIdx: Number, cont: Boolean) => Row */
function textRow(lineIdx, cont) { return { kind: "text", cells: [], col: 0, lineIdx, cont, eol: null, fill: null, edge: [] }; }
/** function virtualRow() => Row */
function virtualRow() { return { kind: "virtual", cells: [], col: 0, lineIdx: null, cont: false, eol: null, fill: null, edge: [] }; }
// Whether a conceal is revealed (a cursor is in its `reveal` range).
/** function revealed(c: Conceal, cursors: Number[]) => Boolean */
function revealed(c, cursors) { const rv = c.reveal; return rv !== null && cursorIn(cursors, rv); }

// Decorations by position, typed.
/** function noConceals() => Map<Number, Conceal> */
function noConceals() { return new Map(); }
/** function noInlines() => Map<Number, InlineText[]> */
function noInlines() { return new Map(); }
/** function noFolds() => Map<Number, Range> */
function noFolds() { return new Map(); }
// A line's active decorations: its conceals and inline texts by position,
// and the virtual lines at it in their order.
/** type LineDecor = {
 *   conceals: Map<Number, Conceal>,
 *   inlines: Map<Number, InlineText[]>,
 *   vlines: VirtualLine[],
 * } */
/** function noLineDecor() => Map<Number, LineDecor> */
function noLineDecor() { return new Map(); }

// `L`: a buffer's held lines; `d`: its decorations (decor.js), asked for
// the lines the layout reaches.
// o: {mode, wrap, width, tab, cursors, folds:[{start,end}] (line indices),
//     styleAt(b, li, idx), styleAtByte(b), whitespace, extendAt(b)}
export function layout(L, d, top, maxRows, o) {
  const rows = [];
  const consumed = [];
  if (top < 0) return { rows, consumed, next: top };
  const width = Math.max(8, o.width);

  // Line `li`'s active decorations, gathered when the layout first reaches
  // it.
  const lineDecor = noLineDecor();
  /** const decorOf: (Number) => LineDecor */
  const decorOf = (li) => {
    const hit = lineDecor.get(li);
    if (hit !== undefined) return hit;
    const ls = L.starts[li], le = L.lineEnd(li) + 1;
    const conceals = noConceals();
    for (const c of d.concealsIn(ls, le)) {
      if (c.end <= c.start || !applies(c, o.mode)) continue;
      if (revealed(c, o.cursors)) continue;
      const prev = conceals.get(c.start);
      if (prev === undefined || prev.end < c.end) conceals.set(c.start, c);
    }
    const inlines = noInlines();
    for (const v of d.inlinesIn(ls, le)) {
      if (applies(v, o.mode)) inlines.set(v.pos, [...(inlines.get(v.pos) ?? []), v]);
    }
    const vlines = d.linesIn(ls, le).filter((v) => applies(v, o.mode)).sort((a, b) => a.order - b.order);
    const x = { conceals, inlines, vlines };
    lineDecor.set(li, x);
    return x;
  };
  const foldAt = noFolds();
  for (const f of o.folds) foldAt.set(f.start, f);

  const newRow = textRow;
  let row = newRow(top, false);
  const put = (ch, src, style, w) => { row.cells.push({ ch, src, style, w }); row.col += w; };
  const putText = (text, src, style) => {
    for (const ch of text) put(ch, src, style, charWidth(ch.codePointAt(0)));
  };
  // Edge text waits for the end of its row, then sits against the width.
  const finishRow = () => {
    for (const v of row.edge) {
      const pad = width - row.col - stringWidth(v.text);
      if (pad > 0) putText(" ".repeat(pad), null, EMPTY);
      putText(v.text, null, drawnOf(v.style));
    }
    row.edge = [];
    rows.push(row);
  };
  // The inline texts at byte `b` of line `li`.
  const inlineAt = (li, b, side) => {
    for (const v of decorOf(li).inlines.get(b) ?? []) {
      if (v.side !== side) continue;
      if (v.edge) row.edge.push(v);
      else putText(v.text, null, drawnOf(v.style));
    }
  };
  const addVirtual = (v) => {
    const r = virtualRow();
    const prev = row;
    row = r;
    putText(v.text, null, drawnOf(v.style));
    rows.push(r);
    row = prev;
  };
  // Continue on a new row.
  const wrapRow = (cur) => {
    const carry = row.edge;
    row.edge = [];
    rows.push(row);
    row = newRow(cur, true);
    row.edge = carry;
  };

  let li = top;
  while (rows.length < maxRows && li < L.count) {
    for (const v of decorOf(li).vlines) if (v.above) addVirtual(v);
    row = newRow(li, false);
    consumed.push(li);
    let cur = li, text = L.lines[cur], i = 0, b = L.starts[cur], last = li;
    // Where the row may break: the cell index after the last space (a
    // list of at most one: none yet).
    /** let breakAt: Number[] */
    let breakAt = [];
    const fold = foldAt.get(li);
    for (;;) {
      if (i >= text.length) {
        // End of a source line; b is its newline (or the end of the text).
        inlineAt(cur, b, "before");
        const c = decorOf(cur).conceals.get(b);
        if (c !== undefined && L.hasNewline(cur)) {
          putText(c.text, b, restyler(c.style)(o.styleAtByte(b)));
          const j = L.indexOf(c.end);
          if (j < 0) { row.eol = b; break; }
          for (let k = last + 1; k <= j; k++) consumed.push(k);
          last = cur = j;
          text = L.lines[cur];
          i = byteToIdx(text, c.end - L.starts[cur]);
          b = c.end;
          breakAt = [row.cells.length];
          continue;
        }
        if (fold !== undefined) putText(" ⋯", null, FOLD_MARK);
        inlineAt(cur, b, "after");
        row.eol = b;
        row.fill = o.extendAt(b);
        break;
      }
      inlineAt(cur, b, "before");
      const c = decorOf(cur).conceals.get(b);
      if (c !== undefined) {
        putText(c.text, b, restyler(c.style)(o.styleAt(b, cur, i)));
        const j = L.indexOf(c.end);
        if (j < 0) { row.eol = b; break; }
        if (j !== cur) {
          for (let k = last + 1; k <= j; k++) consumed.push(k);
          last = cur = j;
          text = L.lines[cur];
        }
        i = byteToIdx(text, c.end - L.starts[cur]);
        b = c.end;
        continue;
      }
      const cp = text.codePointAt(i);
      const ch = String.fromCodePoint(cp);
      const w = cp === 9 ? o.tab - (row.col % o.tab) : charWidth(cp);
      if (o.wrap && row.col + w > width && row.cells.some((x) => x.src != null)) {
        // Move the word that does not fit onto the next row (none to move:
        // the row just ends).
        const at = breakAt.filter((k) => k > 0 && k < row.cells.length);
        for (const k of at) {
          const tail = row.cells.splice(k);
          row.col = row.cells.reduce((n, x) => n + x.w, 0);
          wrapRow(cur);
          for (const x of tail) put(x.ch, x.src, x.style, x.w);
        }
        if (at.length === 0) wrapRow(cur);
        breakAt = [];
      }
      const st = o.styleAt(b, cur, i);
      if (cp === 9) {
        put(o.whitespace ? "→" : " ", b, o.whitespace ? WHITESPACE(st) : st, 1);
        for (let k = 1; k < w; k++) put(" ", null, st, 1);
      } else if (cp === 32 && o.whitespace) put("·", b, WHITESPACE(st), 1);
      else put(ch, b, st, w);
      inlineAt(cur, b, "after");
      if (cp === 32 || cp === 9) breakAt = [row.cells.length];
      i += ch.length;
      b += byteLen(ch);
    }
    finishRow();
    // A fold hides the lines after its first.
    if (fold !== undefined) last = Math.max(last, fold.end);
    const folded = fold !== undefined;
    for (let k = li; k <= last; k++) if (!(folded && k > li)) for (const v of decorOf(k).vlines) if (!v.above) addVirtual(v);
    li = last + 1;
  }
  return { rows, consumed, next: li };
}
