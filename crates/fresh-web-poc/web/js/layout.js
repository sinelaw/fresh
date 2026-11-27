// Visual layout of a buffer for a source view: source lines and decorations
// to rows of monospace cells, at the view's own width. (A compose view is
// drawn as a document instead: doc.js.) This is the one wrap rule: painting,
// scrolling, motions, clicks and the scrollbar all read rows from here
// (rows.js keeps where the rows of long lines start), so they agree.
//
//  * A cell is a grapheme cluster (clusters.js), as many columns wide as
//    the terminal editor measures it; a tab reaches the next tab stop.
//  * A conceal draws its text instead of [start, end); one that covers a
//    newline joins the next source line onto the same row. With `reveal`, it
//    is inactive while a cursor is inside that range, so the source shows
//    where the user is editing.
//  * Inline text is drawn before or after the character at its position, or
//    with `edge` at the right edge of the row it falls in.
//  * Lines are whole rows above or below their position's line.
//  * With wrapping on, a row breaks after the last space or tab that fits
//    (else before the cluster that does not fit). A row's state at its
//    start is its byte alone, so rows can be laid out from any row start
//    and come out as they would from the line's start.
//  * A view's folds hide whole lines after a fold's first line.
// A decoration with `when` applies only to a view in that mode.
//
// A block is what is laid out as one: a source line, with the lines that
// conceals covering its newlines join onto it.

import { listOf } from "./maybe.js";
import { EMPTY, restyler, drawnOf } from "./paint.js";
import { clustersIn, unitsOf, bytesOf, widthOf, widthFrom, textWidth } from "./clusters.js";

// A folded line's marker; a shown tab or space.
const FOLD_MARK = drawnOf({ fg: "editor.line_number_fg", bg: null, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false });
const WHITESPACE = restyler({ fg: "editor.whitespace_indicator_fg", bg: null, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false });

// UTF-16 units of a line read for clusters at a time.
const SLICE_UNITS = 4096;

export const applies = (d, mode) => d.when === "always" || d.when === mode;
export const cursorIn = (cursors, r) => cursors.some((c) => c >= r.start && c < Math.max(r.end, r.start + 1));

// A row's cell: its cluster, the byte it shows (none: text that is not the
// buffer's), its style and its width in columns.
/** type Cell = {ch: String, src: Number | Null, style: Drawn, w: Number} */
// A row: of source text ("text": its block's first line, whether it
// continues a wrapped line, the byte where its line ends when it does
// (none: it wraps, a conceal runs on, or it is cut short), the background
// its last overlay carries to the edge) or a plugin's whole row ("virtual",
// no line). A text row records the source bytes it shows, [start, end):
// one row's end is the next one's start. `col0`: the column its first cell
// is at (a long line drawn without wrapping is drawn from the columns on
// screen); `col`: the column after its last. `edge`: inline texts held for
// the row's right edge, each with the index of the cell it goes with (a
// word moved to the next row takes those of its cells along).
/** type Row = {
 *   kind: String,
 *   cells: Cell[],
 *   col0: Number,
 *   col: Number,
 *   lineIdx: Number | Null,
 *   cont: Boolean,
 *   eol: Number | Null,
 *   fill: String | Null,
 *   edge: {text: InlineText, at: Number}[],
 *   start: Number | Null,
 *   end: Number | Null,
 * } */
/** function textRow(lineIdx: Number, cont: Boolean, start: Number, col0: Number) => Row */
function textRow(lineIdx, cont, start, col0) { return { kind: "text", cells: [], col0, col: col0, lineIdx, cont, eol: null, fill: null, edge: [], start, end: start }; }
/** function virtualRow() => Row */
function virtualRow() { return { kind: "virtual", cells: [], col0: 0, col: 0, lineIdx: null, cont: false, eol: null, fill: null, edge: [], start: null, end: null }; }
// Whether a conceal is revealed (a cursor is in its `reveal` range).
/** function revealed(c: Conceal, cursors: Number[]) => Boolean */
function revealed(c, cursors) { const rv = c.reveal; return rv !== null && cursorIn(cursors, rv); }

// Decorations by position, typed.
/** function noConceals() => Map<Number, Conceal> */
function noConceals() { return new Map(); }
/** function noInlines() => Map<Number, InlineText[]> */
function noInlines() { return new Map(); }
// A line's active decorations: its conceals and inline texts by position,
// and the virtual lines at it in their order.
/** type LineDecor = {
 *   conceals: Map<Number, Conceal>,
 *   inlines: Map<Number, InlineText[]>,
 *   vlines: VirtualLine[],
 * } */
/** function noLineDecor() => Map<Number, LineDecor> */
function noLineDecor() { return new Map(); }

// What rows are laid out from: a buffer's held lines `L`, its decorations
// `d` (decor.js), asked for the lines the layout reaches, and the view's
// options. `o`: {mode, wrap, width, tab, cursors, folds:[{start,end}]
// (line indices), styleAt(b, li, idx), styleAtByte(b), whitespace,
// extendAt(b)}.
/** type Layout = {L: Lines, d: Decor, o: LayoutOptions, decor: Map<Number, LineDecor>} */
/** function layoutOf(L: Lines, d: Decor, o: LayoutOptions) => Layout */
export function layoutOf(L, d, o) { return { L, d, o, decor: noLineDecor() }; }
// The same layout without styles: for walking rows, which only their
// places matter for.
/** function measuring(x: Layout) => Layout */
export function measuring(x) {
  const o = x.o;
  return { L: x.L, d: x.d, decor: x.decor, o: { ...o, styleAt: () => EMPTY, styleAtByte: () => EMPTY, extendAt: () => null } };
}
// What a layout's rows depend on, but for the text itself: rows laid out
// under one key can be kept while the text changes (rows.js).
/** function layoutKey(x: Layout, gen: Number) => String */
export function layoutKey(x, gen) {
  const o = x.o;
  return `${o.mode}:${o.wrap ? o.width : "-"}:${o.tab}:${gen}`;
}

// Line `li`'s active decorations, gathered when a layout first reaches it.
/** function decorOf(x: Layout, li: Number) => LineDecor */
function decorOf(x, li) {
  const hit = x.decor.get(li);
  if (hit !== undefined) return hit;
  const { L, d, o } = x;
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
  const made = { conceals, inlines, vlines };
  x.decor.set(li, made);
  return made;
}

// The active conceal covering line `li`'s newline (a list of at most one):
// what joins the next line onto its block.
/** function joinerOf(x: Layout, li: Number) => Conceal[] */
function joinerOf(x, li) {
  const { L, d, o } = x;
  if (!L.hasNewline(li)) return [];
  const nl = L.lineEnd(li);
  return d.concealsIn(nl, nl + 1).filter((c) => c.start <= nl && c.end > nl && applies(c, o.mode) && !revealed(c, o.cursors)).slice(0, 1);
}
// The first line of the block holding line `li`.
/** function blockOf(x: Layout, li: Number) => Number */
export function blockOf(x, li) {
  let k = li;
  while (k > 0 && joinerOf(x, k - 1).length > 0) k--;
  return k;
}
// The last line of the block starting at line `li`.
/** function blockLast(x: Layout, li: Number) => Number */
export function blockLast(x, li) {
  let k = li;
  for (;;) {
    const j = joinerOf(x, k).map((c) => x.L.indexOf(c.end)).at(0) ?? -1;
    if (j <= k) return k;
    k = j;
  }
}

// The fold whose first line is `li` (a list of at most one).
/** function foldAt(x: Layout, li: Number) => Range[] */
function foldAt(x, li) { return x.o.folds.filter((f) => f.start === li).slice(0, 1); }
// The line drawn after block [li, last]: past a fold starting at `li`.
/** function nextShown(x: Layout, li: Number, last: Number) => Number */
export function nextShown(x, li, last) {
  return foldAt(x, li).reduce((m, f) => Math.max(m, f.end), last) + 1;
}
// The first line of the block drawn before line `li` (-1: none): before a
// fold hiding that line, its first line.
/** function prevShown(x: Layout, li: Number) => Number */
export function prevShown(x, li) {
  if (li <= 0) return -1;
  let k = blockOf(x, li - 1);
  for (const f of x.o.folds) if (k > f.start && k <= f.end) k = blockOf(x, f.start);
  return k;
}

// The rows of the block starting at line `li`, from byte `from` (its start,
// or where one of its rows starts), in order. Rows from the block's start
// begin with the virtual lines above it; the last ones are those below.
//
// Without wrapping, a row starts at column `col0` (the column of `from`)
// and is cut short once it reaches column `stopCol`; `mark(col, b)` is told
// the column of a cluster boundary now and then on the way (rows.js keeps
// them for long lines).
/** function blockRows(
 *   x: Layout,
 *   li: Number,
 *   from: Number,
 *   col0: Number,
 *   stopCol: Number,
 *   mark: (Number, Number) => Undefined,
 * ) => Iterator<Row> */
export function* blockRows(x, li, from, col0, stopCol, mark) {
  const { L, o } = x;
  const width = Math.max(8, o.width);
  const atStart = from === L.starts[li] && L.startsLine(li);
  if (atStart) for (const v of decorOf(x, li).vlines) if (v.above) yield virtualOf(v);
  let row = textRow(li, !atStart, from, col0);
  let hasSrc = false;
  const put = (ch, src, style, w) => {
    row.cells.push({ ch, src, style, w });
    row.col += w;
    if (src !== null) hasSrc = true;
  };
  const putText = (text, src, style) => { putCells(row, text, src, style); if (src !== null) hasSrc = true; };
  const inlinesAt = (k, b, side) => (decorOf(x, k).inlines.get(b) ?? []).filter((v) => v.side === side);
  // An edge text belongs to the cell it is before, or after (the one
  // before it).
  const putInlines = (vs) => {
    for (const v of vs) {
      if (v.edge) row.edge.push({ text: v, at: v.side === "after" ? row.cells.length - 1 : row.cells.length });
      else putText(v.text, null, drawnOf(v.style));
    }
  };
  // A row is done: the next starts at `next` with `cells` carried over (a
  // word moved down).
  const nextRow = (next, cells) => {
    const done = row;
    const kept = done.cells.length;
    const moved = done.edge.filter((e) => e.at >= kept && cells.length > 0).map((e) => ({ text: e.text, at: e.at - kept }));
    done.edge = done.edge.filter((e) => !(e.at >= kept && cells.length > 0));
    done.end = next;
    done.col = done.cells.reduce((n, c) => n + c.w, done.col0);
    finishRow(done, width);
    row = textRow(li, true, next, 0);
    hasSrc = false;
    for (const c of cells) put(c.ch, c.src, c.style, c.w);
    row.edge = moved;
    return done;
  };

  let cur = L.indexOf(from), last = li;
  let text = L.lines[cur], i = L.idxOf(cur, from), b = from;
  // The clusters of `text` read from `clFrom`, and the one at `i` (k).
  let cl = clustersIn(text, i, i + SLICE_UNITS), clFrom = i, k = 0, ci = i;
  const seek = (to) => { cl = clustersIn(text, to, to + SLICE_UNITS); clFrom = to; k = 0; ci = to; };
  // Where the row may break: the cell index after the last space, and the
  // byte there (a list of at most one: none yet).
  /** let breakAt: {cell: Number, byte: Number}[] */
  let breakAt = [];
  let sinceMark = 0;
  const fold = foldAt(x, li);
  let cut = false;
  for (;;) {
    if (i >= text.length) {
      // End of a source line; b is its newline (or the end of the text).
      putInlines(inlinesAt(cur, b, "before"));
      const c = decorOf(x, cur).conceals.get(b);
      if (c !== undefined && L.hasNewline(cur)) {
        putText(c.text, b, restyler(c.style)(o.styleAtByte(b)));
        const j = L.indexOf(c.end);
        if (j < 0) { row.eol = b; break; }
        last = cur = j;
        text = L.lines[cur];
        i = L.idxOf(cur, c.end);
        b = c.end;
        seek(i);
        breakAt = [{ cell: row.cells.length, byte: b }];
        continue;
      }
      if (fold.length > 0) putText(" ⋯", null, FOLD_MARK);
      putInlines(inlinesAt(cur, b, "after"));
      // The end of a window cut inside the line is not the line's end; a
      // CRLF's carriage return is the line break's, so the caret's end of
      // the line is before it.
      if (L.hasNewline(cur) || cur < L.count - 1 || !L.tailCut) {
        row.eol = crBefore(text, L.hasNewline(cur)) ? b - 1 : b;
        row.fill = o.extendAt(b);
      }
      break;
    }
    const before = inlinesAt(cur, b, "before");
    const c = decorOf(x, cur).conceals.get(b);
    if (c !== undefined) {
      putInlines(before);
      putText(c.text, b, restyler(c.style)(o.styleAt(b, cur, i)));
      const j = L.indexOf(c.end);
      if (j < 0) { row.eol = b; break; }
      if (j !== cur) {
        last = cur = j;
        text = L.lines[cur];
      }
      i = L.idxOf(cur, c.end);
      b = c.end;
      seek(i);
      continue;
    }
    if (ci !== i) seek(i);
    if (k >= cl.n) { seek(i); }
    const units = unitsOf(cl, k), bytes = bytesOf(cl, k);
    const code = text.charCodeAt(i);
    const tab = code === 9;
    // A carriage return ending a line draws no cell (CRLF is one break).
    const crlf = code === 13 && i + 1 === text.length && crBefore(text, L.hasNewline(cur));
    let w = tab ? o.tab - (row.col % o.tab) : crlf ? 0 : widthOf(cl, k);
    const shown = before.filter((v) => !v.edge).reduce((n, v) => n + textWidth(v.text), 0);
    while (o.wrap && hasSrc && row.col + shown + w > width) {
      // Move the word that does not fit onto the next row (none to move:
      // the row just ends here; a moved word this cluster still does not
      // fit after ends its row too, as a row started there would).
      const at = breakAt.filter((p) => p.cell > 0 && p.cell < row.cells.length);
      if (at.length > 0) yield nextRow(at[0].byte, row.cells.splice(at[0].cell));
      else yield nextRow(b, []);
      breakAt = [];
      if (tab) w = o.tab - (row.col % o.tab);
    }
    putInlines(before);
    const st = o.styleAt(b, cur, i);
    if (tab) {
      put(o.whitespace ? "→" : " ", b, o.whitespace ? WHITESPACE(st) : st, 1);
      for (let n = 1; n < w; n++) put(" ", null, st, 1);
    } else if (code === 32 && o.whitespace) put("·", b, WHITESPACE(st), 1);
    else put(text.slice(i, i + units), b, st, w);
    putInlines(inlinesAt(cur, b, "after"));
    i += units;
    b += bytes;
    k++;
    ci = i;
    if (code === 32 || tab) breakAt = [{ cell: row.cells.length, byte: b }];
    if (!o.wrap) {
      if (++sinceMark >= MARK_EVERY) { sinceMark = 0; mark(row.col, b); }
      if (row.col >= stopCol) { cut = true; break; }
    }
  }
  row.end = b;
  finishRow(row, width);
  yield row;
  // The rest of a block cut short: its last line, for what follows it.
  if (cut) last = blockLast(x, li);
  // A fold hides the lines after its first.
  if (fold.length > 0) return;
  for (let n = li; n <= last; n++) for (const v of decorOf(x, n).vlines) if (!v.above) yield virtualOf(v);
}
// Whether a line of text `text` ends in a CRLF's carriage return (it has
// a newline, held after it).
/** function crBefore(text: String, newline: Boolean) => Boolean */
export function crBefore(text, newline) { return newline && text.endsWith("\r"); }

// How many clusters apart, without wrapping, columns are marked.
const MARK_EVERY = 1024;

// A virtual line's row.
/** function virtualOf(v: VirtualLine) => Row */
function virtualOf(v) {
  const r = virtualRow();
  putCells(r, v.text, null, drawnOf(v.style));
  return r;
}
// The cells of `text` (not the buffer's: `src` is that of what it stands
// for, or none) at the end of row `r`.
/** function putCells(r: Row, text: String, src: Number | Null, style: Drawn) => Undefined */
function putCells(r, text, src, style) {
  const c = clustersIn(text, 0, text.length);
  let i = 0;
  for (let k = 0; k < c.n; k++) {
    const u = unitsOf(c, k);
    const ch = text.slice(i, i + u);
    const w = ch === "\t" ? 4 - (r.col % 4) : widthOf(c, k);
    r.cells.push({ ch, src, style, w });
    r.col += w;
    i += u;
  }
}
// Edge text waits for the end of its row, then sits against the width.
/** function finishRow(r: Row, width: Number) => Undefined */
function finishRow(r, width) {
  for (const { text: v } of r.edge) {
    const pad = width - r.col - widthFrom(v.text, r.col, 4);
    if (pad > 0) putCells(r, " ".repeat(pad), null, EMPTY);
    putCells(r, v.text, null, drawnOf(v.style));
  }
  r.edge = [];
}

// The rows from byte `top` (where a row starts) on, at most `maxRows`,
// across blocks and folds. Without wrapping, `from(li)` says where each
// block's row starts drawing: its byte and column (a long line is drawn
// from the columns on screen), and `stopCol` where it may stop.
/** function rowsFrom(
 *   x: Layout,
 *   top: Number,
 *   maxRows: Number,
 *   from: (Number) => {at: Number, col: Number},
 *   stopCol: Number,
 * ) => Row[] */
export function rowsFrom(x, top, maxRows, from, stopCol) {
  const { L } = x;
  /** const rows: Row[] */
  const rows = [];
  const li0 = L.indexOf(top);
  if (li0 < 0 || maxRows <= 0) return rows;
  let li = blockOf(x, li0);
  let at = { at: top, col: 0 };
  if (!x.o.wrap) at = from(li);
  for (;;) {
    for (const r of blockRows(x, li, at.at, at.col, stopCol, ignoreMark)) {
      rows.push(r);
      if (rows.length >= maxRows) return rows;
    }
    li = nextShown(x, li, blockLast(x, li));
    if (li >= L.count) return rows;
    at = x.o.wrap ? { at: L.starts[li], col: 0 } : from(li);
  }
}
// No columns to mark.
/** function ignoreMark(col: Number, b: Number) => Undefined */
export function ignoreMark(_col, _b) {}

// ---------------------------------------------------------------- a row's cells

// The column byte `p` is drawn at in row `r` (its end: past the row).
/** function columnIn(r: Row, p: Number) => Number */
export function columnIn(r, p) {
  let col = r.col0;
  for (const c of r.cells) {
    if (c.src !== null && c.src >= p) return col;
    col += c.w;
  }
  return col;
}
// The byte drawn at column `x` of row `r`: the cell there (its nearer
// edge), else the row's line end, else its last cell (a wrapped row keeps
// the caret on it).
/** function byteAtColumn(r: Row, x: Number) => Number */
export function byteAtColumn(r, x) {
  let col = r.col0;
  let last = listOf(r.start);
  for (const c of r.cells) {
    for (const b of listOf(c.src)) {
      if (col + c.w / 2 > x) return b;
      last = [b];
    }
    col += c.w;
  }
  for (const b of listOf(r.eol)) return b;
  for (const b of last) return b;
  throw new Error("a text row shows no byte");
}
// Where End leaves the caret on row `r`: its line's end, or before its
// last cluster (a wrapped row's next row starts right after it).
/** function rowEndOf(r: Row) => Number */
export function rowEndOf(r) {
  for (const b of listOf(r.eol)) return b;
  const srcs = r.cells.flatMap((c) => listOf(c.src));
  return srcs.at(-1) ?? r.start ?? 0;
}
