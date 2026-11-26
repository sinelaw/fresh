// A view in compose mode, drawn as a web document: headings, paragraphs the
// browser wraps, lists, quotes, code blocks and tables, in a proportional
// font. The structure comes from `nodes` decorations (a plugin declares it;
// nothing here knows Markdown); a line in no node is a paragraph of its own.
//
// Every drawn piece of source text is a span carrying its first byte
// (`data-p`) whose text is that source substring (a joined line break draws
// as a space: one byte either way). A concealed range draws as a span with
// its range (`data-at`, `data-e`) and its replacement; a line with nothing
// to show gets a zero-width anchor (`data-a`). The spans, in byte order, are
// the map between bytes and the page: a caret is a collapsed Range at a
// byte's text offset, and a point is a byte through the browser's
// caret-from-point.
//
// Scrolling is a source line (`v.top`) plus a pixel offset from that line's
// drawn top (`v.topPx`). A frame draws the blocks from one above the top's
// block down to what fills the view, measures each line's top once after
// writing, and translates the document so the scroll position is at the top.
//
// Folds are the view's, as in a source view: a fold hides the lines after
// its first. A block offers one over what belongs to it: a heading its
// section, an item or quote the deeper ones after it, a code block or table
// its lines after the first shown. A joined block holding a fold's first
// line is drawn whole (its lines read as one).

import { byteLen } from "./ot.js";
import { byteToIdx } from "./buffer.js";
import { applies, cursorIn } from "./layout.js";
import { EMPTY, spansFor, synAt, fenceLanguage, esc, cssFor, styleKey, drawnOf, asMarkup, cssOf } from "./paint.js";
import { within } from "./dom.js";
import { poolOf, take, sameDrawn } from "./pool.js";
import { some, listOf, found } from "./maybe.js";

// Node types that live inside a block rather than being one.
const PART = new Set(["row", "cell"]);
// Block types that keep their lines (the others are joined into one run).
const LINES = new Set(["code", "table"]);
// Pixels kept clear left (and right) of the document for fold toggles.
const FOLD_MARGIN = 40;

const PLAIN = styleKey(EMPTY);
const anchor = (p) => `<span data-a="${p}">​</span>`;

// A table's row nodes and cell nodes by line, typed.
/** function noNodeAt() => Map<Number, DocNode> */
function noNodeAt() { return new Map(); }
/** function noNodesAt() => Map<Number, DocNode[]> */
function noNodesAt() { return new Map(); }

// A drawn piece of the document: text (`p`, its bytes), an anchor (`a`,
// a position drawing nothing) or a concealed range drawn as other text
// (`at`).
/** type Piece = {el: El, kind: String, start: Number, end: Number} */
/** function noPieces() => Piece[] */
function noPieces() { return []; }
/** const NO_PIECE: Piece | Null */
const NO_PIECE = null;
// No byte there.
/** const NO_POS: Number | Null */
const NO_POS = null;
// The DOM point at client (x, y), if the browser finds one.
function caretAt(x, y) {
  if (typeof document.caretPositionFromPoint === "function") {
    return listOf(document.caretPositionFromPoint(x, y)).map((c) => ({ node: c.offsetNode, off: c.offset }));
  }
  return listOf(document.caretRangeFromPoint(x, y)).map((r) => ({ node: r.startContainer, off: r.startOffset }));
}
// The element a DOM point's node is in: a text node's parent, or the node.
/** function elementAt(node: El) => El[] */
function elementAt(node) { return node.nodeType === 3 ? listOf(node.parentElement) : [node]; }
// A caret's box (none: not drawn).
/** type CaretBox = {left: Number, top: Number, height: Number} */
/** const NO_RECT: CaretBox | Null */
const NO_RECT = null;

// No syntax colours (a line's spans, for text that is not code).
/** const NO_SYNTAX: (Number) => Span[] */
const NO_SYNTAX = (_li) => [];

// What was drawn (pool.js); each drawn line's top.
/** function noDrawn() => {el: El, html: String}[] */
function noDrawn() { return []; }
/** type LineTop = {line: Number, y: Number} */
/** function noLineTops() => LineTop[] */
function noLineTops() { return []; }

// No fold there; no document node there.
/** const NO_FOLD: Range | Null */
const NO_FOLD = null;
/** const NO_NODE: DocNode | Null */
const NO_NODE = null;

// Decorations by line, and inline texts by position and side, typed.
/** function noLines() => Map<Number, VirtualLine[]> */
function noLines() { return new Map(); }
/** function noInlines() => Map<String, InlineText[]> */
function noInlines() { return new Map(); }

// The fold hiding line `li` (of `folds`, keeping lines up to `keeps`
// each) and the last line it keeps (none: the line shows).
/** function hiddenBy(folds: Range[], keeps: Number[], li: Number) => Hidden[] */
function hiddenBy(folds, keeps, li) {
  const i = folds.findIndex((f, k) => li > keeps[k] && li <= f.end);
  return i < 0 ? [] : [{ fold: folds[i], keep: keeps[i] }];
}

// The next line drawn after (dir 1) or before (dir -1) line `k`, past the
// folds: -1 or L.count at the ends.
function shownFrom(ctx, k, dir) {
  while (k >= 0 && k < ctx.L.count) {
    const h = ctx.hidden(k);
    if (h.length === 0) break;
    k = dir > 0 ? h[0].fold.end + 1 : h[0].keep;
  }
  return k;
}

// The block holding line `li`: its node (or none) and its lines.
function blockAt(ctx, li) {
  const { L, d } = ctx;
  const nodes = d.nodesIn(L.starts[li], L.lineEnd(li) + 1).filter((n) => !PART.has(n.type) && applies(n, ctx.mode));
  // The one starting first (the first of those at one start).
  for (const best of [...nodes].sort((a, b) => a.start - b.start).slice(0, 1)) {
    const f = L.indexOf(best.start), l = L.indexOf(best.end);
    return { node: some(best), first: f < 0 ? 0 : Math.min(f, li), last: l < 0 ? L.count - 1 : Math.max(l, li) };
  }
  return { node: NO_NODE, first: li, last: li };
}

// A block of the document: its Markdown node (none: plain lines) and its
// first and last line.
/** type DocBlock = {node: DocNode | Null, first: Number, last: Number} */
// Its node, as a list of at most one.
/** function nodeOf(b: DocBlock) => DocNode[] */
function nodeOf(b) { return listOf(b.node); }

// A height the block surely has at least, from line `from` on: a line
// draws at least a line height; a code block's fences and a table's
// delimiter row may draw nothing.
function atLeastHeight(ctx, blk, lh, from) {
  for (const node of nodeOf(blk)) {
    let n = 0;
    for (let k = from; k <= blk.last; k++) if (ctx.hidden(k).length === 0) n++;
    if (node.type === "code") n -= 2;
    if (node.type === "table") n -= 1;
    return Math.max(0, n) * lh;
  }
  return from > blk.last ? 0 : lh;
}

function vline(x) {
  const st = cssOf(drawnOf(x.style));
  return `<div class="vline"${st ? ` style="${st}"` : ""}>${esc(x.text || "")}​</div>`;
}

function concealHTML(c) {
  const st = cssOf(drawnOf(c.style));
  const body = c.text === "\n" ? "<br>" : esc(c.text);
  return `<span class="cc" data-at="${c.start}" data-e="${c.end}"${st ? ` style="${st}"` : ""}>${body}</span>`;
}

// hidden(li): the fold hiding line `li` and the last line it keeps
// ({fold, keep}; none: the line shows).
function hider(ctx) {
  const keeps = ctx.v.folds.map((f) => {
    const sb = blockAt(ctx, f.start);
    return nodeOf(sb).some((n) => !LINES.has(n.type)) ? Math.max(f.start, sb.last) : f.start;
  });
  return (li) => hiddenBy(ctx.v.folds, keeps, li);
}

// `probe`: only whether there is one (looks no further than needed).
function foldOf(ctx, blk, probe) {
  const { L } = ctx;
  const nodes = nodeOf(blk);
  if (nodes.length === 0) return NO_FOLD;
  const n = nodes[0];
  if (n.type === "code") return blk.last - blk.first >= 3 ? some({ start: blk.first + 1, end: blk.last }) : NO_FOLD;
  if (n.type === "table") return blk.last - blk.first >= 2 ? some({ start: blk.first, end: blk.last }) : NO_FOLD;
  // A heading's section runs to the next heading as high; an item or a
  // quote holds the deeper ones after it. Blank lines in between belong.
  const child = (b) => n.type === "heading"
    ? !nodeOf(b).some((x) => x.type === "heading" && x.level <= n.level)
    : nodeOf(b).some((x) => x.type === n.type && x.depth > n.depth);
  let end = -1;
  for (let k = blk.last + 1; k < L.count;) {
    const b = blockAt(ctx, k);
    if (nodeOf(b).length === 0 && L.lines[k].trim() === "") { k++; continue; }
    if (!child(b)) break;
    end = b.last;
    k = b.last + 1;
    if (probe) break;
  }
  return end < 0 ? NO_FOLD : some({ start: blk.first, end });
}

// The HTML of source [a, z): runs of equally styled text as spans, each
// with its first byte; concealed ranges as their replacement; inline text.
// A line break inside the range draws as a space (or, concealed with
// "\n", as a line break). `syn(li)`: syntax spans for a line.
function walk(ctx, a, z, syn) {
  const { L, o } = ctx;
  let out = "", shown = false;
  let li = L.indexOf(a), b = a;
  while (li >= 0 && li < L.count && b <= z) {
    const text = L.lines[li], ls = L.starts[li], le = L.lineEnd(li);
    const { act, rev, inl } = ctx.lineInfo(li);
    const spans = syn(li);
    let run = "", at = 0, key = "", style = "";
    const flush = () => {
      if (!run) return;
      out += `<span data-p="${at}"${style ? ` style="${style}"` : ""}>${esc(run)}</span>`;
      shown = true;
      run = "";
    };
    const put = (ch, p, st) => {
      const k = styleKey(st);
      if (k !== key || !run) { flush(); key = k; style = k === PLAIN ? "" : cssFor(st, k); at = p; }
      run += ch;
    };
    const inline = (p, side) => {
      for (const t of inl.get(`${p}${side}`) ?? []) {
        flush();
        const st = cssOf(drawnOf(t.style));
        out += `<span class="vt${t.edge ? " edge" : ""}"${st ? ` style="${st}"` : ""}>${esc(t.text)}</span>`;
      }
    };
    const styleAt = (p, idx) => {
      let st = o.decoStyle(p, li, synAt(spans, idx));
      for (const c of rev) if (p >= c.start && p < c.end) { st = asMarkup(st); break; }
      return st;
    };
    let ci = 0;
    const concealAt = (p) => {
      while (ci < act.length && act[ci].end <= p) ci++;
      return ci < act.length && act[ci].start <= p ? act[ci] : null;
    };
    let i = byteToIdx(text, b - ls);
    let jumped = false;
    const stop = Math.min(z, le);
    while (b < stop) {
      inline(b, "b");
      const c = concealAt(b);
      if (c) {
        flush();
        out += concealHTML(c);
        if (c.text) shown = true;
        b = c.end;
        if (b > le) { jumped = true; break; }
        i = byteToIdx(text, b - ls);
        continue;
      }
      const cp = text.codePointAt(i);
      const ch = String.fromCodePoint(cp);
      put(ch, b, styleAt(b, i));
      inline(b, "a");
      b += byteLen(ch);
      i += ch.length;
    }
    if (jumped) { flush(); li = L.indexOf(b); continue; }
    if (b === le) {
      inline(le, "b");
      inline(le, "a");
      if (z > le && L.hasNewline(li)) {
        // The line break inside a joined block.
        const c = concealAt(le);
        if (c) {
          flush();
          out += concealHTML(c);
          if (c.text) shown = true;
          b = c.end;
          li = L.indexOf(b);
          continue;
        }
        put(" ", le, styleAt(le, i));
      }
    }
    flush();
    b = le + 1;
    li++;
  }
  return { html: out, shown };
}

// What drawing needs for the whole frame: decorations by line, styles.
function context(v) {
  const buf = v.buf;
  const L = buf.held;
  const d = buf.decor;
  const cursors = v.cursors.map((c) => c.pos);
  const o = v.pane.layoutOpts(v, 0);
  const vlines = noLines();
  for (const x of d.lines) {
    if (!applies(x, v.mode)) continue;
    const i = L.indexOf(x.pos);
    if (i >= 0) vlines.set(i, [...(vlines.get(i) ?? []), x]);
  }
  const lineCache = new Map();
  // A line's active conceals (sorted, the longest first at one start),
  // the revealed ones (drawn dim) and its inline texts by position.
  const lineInfo = (li) => {
    const hit = lineCache.get(li);
    if (hit !== undefined) return hit;
    const ls = L.starts[li], le = L.lineEnd(li);
    const act = [], rev = [];
    for (const c of d.concealsIn(ls, le + 1)) {
      if (c.end <= c.start || !applies(c, v.mode)) continue;
      if (listOf(c.reveal).some((r) => cursorIn(cursors, r))) rev.push(c);
      else act.push(c);
    }
    act.sort((a, b) => (a.start !== b.start ? a.start - b.start : b.end - a.end));
    const inl = noInlines();
    for (const t of d.inlinesIn(ls, le + 1)) {
      if (!applies(t, v.mode) || t.pos < ls || t.pos > le) continue;
      const k = `${t.pos}${t.side === "after" ? "a" : "b"}`;
      inl.set(k, [...(inl.get(k) ?? []), t]);
    }
    const x = { act, rev, inl };
    lineCache.set(li, x);
    return x;
  };
  // The folds hide lines by the blocks they fold, which takes only the
  // view, its text and its decorations (as `hiding` asks).
  const base = { v, L, d, mode: v.mode };
  const hidden = hider(base);
  return { base, v, L, d, o, cursors, vlines, lineInfo, mode: v.mode, hidden };
}

// The fold hiding line `li` of view `v`, as `hidden` gives it.
export function hiding(v, li) {
  return hider({ v, L: v.buf.held, d: v.buf.decor, mode: v.mode })(li);
}

// The fold the block holding line `li` offers: {start, end} lines, or null.
export function foldRange(v, li) {
  const ctx = { v, L: v.buf.held, d: v.buf.decor, mode: v.mode };
  return foldOf(ctx, blockAt(ctx, li), false);
}

// A table block `blk` of node `n`; `tog`: its fold toggle.
function tableHTML(ctx, blk, n, tog) {
  const { L, d } = ctx;
  const parts = d.nodesIn(n.start, n.end + 1).filter((x) => PART.has(x.type) && applies(x, ctx.mode));
  const rows = noNodeAt(), cells = noNodesAt();
  for (const x of parts) {
    const li = L.indexOf(x.start);
    if (x.type === "row") rows.set(li, x);
    else cells.set(li, [...(cells.get(li) ?? []), x]);
  }
  let ncol = 1;
  for (const list of cells.values()) { list.sort((a, b) => a.start - b.start); ncol = Math.max(ncol, list.length); }
  let html = `<table class="doc-table"><tbody>`;
  for (let k = blk.first; k <= blk.last; k++) {
    if (ctx.hidden(k).length > 0) continue;
    const ls = L.starts[k], le = L.lineEnd(k);
    const list = cells.get(k) ?? [];
    // The fold toggle sits in the first cell drawn, placed by the table.
    const t = tog;
    tog = "";
    if (!list.length) {
      // A row without cells (the delimiter row): its text across the table.
      const r = walk(ctx, ls, le, NO_SYNTAX);
      if (!r.shown && L.lines[k] !== "") continue;
      html += `<tr class="raw"><td class="l" colspan="${ncol}">${t}${r.html}${r.shown ? "" : anchor(le)}</td></tr>`;
      continue;
    }
    const tag = found(rows.get(k)).some((r) => r.header) ? "th" : "td";
    html += "<tr>";
    list.forEach((c, i) => {
      const a = i === 0 ? ls : c.start, z = i + 1 < list.length ? list[i + 1].start : le;
      const r = walk(ctx, a, z, NO_SYNTAX);
      const al = n.align.slice(i, i + 1).flatMap((x) => listOf(x)).map((x) => ` style="text-align:${x}"`).at(0) ?? "";
      html += `<${tag} class="l"${al}>${i === 0 ? t : ""}${r.html}${r.shown ? "" : anchor(z)}</${tag}>`;
    });
    for (let i = list.length; i < ncol; i++) html += `<${tag}></${tag}>`;
    html += "</tr>";
  }
  return html + "</tbody></table>";
}

function blockHTML(ctx, blk) {
  const { L } = ctx;
  const nodes = nodeOf(blk);
  const ls = L.starts[blk.first], le = L.lineEnd(blk.last);
  // The fold toggle (shown on hover, or while folded) and, while folded,
  // an ellipsis where the hidden part was.
  const folded = ctx.v.folds.some((f) => f.start >= blk.first && f.start <= blk.last);
  const tog = folded || foldOf(ctx.base, blk, true) !== null ? `<span class="doc-fold${folded ? " folded" : ""}" data-fold="${blk.first}">${folded ? "▸" : "▾"}</span>` : "";
  const more = folded ? `<span class="doc-more" data-fold="${blk.first}">⋯</span>` : "";
  const joined = () => { const r = walk(ctx, ls, le, NO_SYNTAX); return tog + r.html + (r.shown ? "" : anchor(le)) + more; };
  if (nodes.length === 0) return `<p class="l">${joined()}</p>`;
  const n = nodes[0];
  const type = n.type;
  if (type === "heading") {
    const lv = Math.max(1, Math.min(6, n.level || 1));
    return `<h${lv} class="l">${joined()}</h${lv}>`;
  }
  if (type === "item") {
    const m = n.marker || "•";
    const cls = m === "☑" ? " done" : m === "☐" ? " todo" : /\d/.test(m) ? " num" : "";
    return `<div class="doc-li" style="--depth:${n.depth || 0}"><div class="l"><span class="mk${cls}">${esc(m)}</span>${joined()}</div></div>`;
  }
  if (type === "quote") return `<blockquote class="l" style="--depth:${n.depth || 1}">${joined()}</blockquote>`;
  if (type === "rule") return `<div class="doc-hr l">${joined()}</div>`;
  if (type === "code") {
      const lang = fenceLanguage(n.lang);
      const syn = lang ? (li) => spansFor(lang, L.lines[li]) : NO_SYNTAX;
      let html = `<div class="doc-code"${n.lang ? ` data-lang="${esc(n.lang)}"` : ""}>${tog}`;
      for (let k = blk.first; k <= blk.last; k++) {
        if (ctx.hidden(k).length > 0) continue;
        const r = walk(ctx, L.starts[k], L.lineEnd(k), syn);
        // A line whose text is all concealed draws nothing.
        if (!r.shown && L.lines[k] !== "") continue;
        html += `<div class="ln l">${r.html}${r.shown ? "" : anchor(L.lineEnd(k))}</div>`;
      }
      return html + (folded ? `<div class="ln">${more}</div>` : "") + "</div>";
  }
  if (type === "table") return tableHTML(ctx, blk, n, tog);
  return `<p class="l">${joined()}</p>`;
}

// Where a caret at byte `p` of piece `e` is drawn, in client coordinates
// (`range`: one to measure text with): as tall as its line's box (the
// line height of the line element holding it), centred on the glyphs.
function caretRectOf(range, e, p) {
  // Inside text: the range at the byte; else the piece's box.
  const rects = [];
  for (const tn of e.kind === "p" ? listOf(e.el.firstChild) : []) {
    const idx = byteToIdx(tn.textContent, p - e.start);
    range.setStart(tn, idx);
    range.setEnd(tn, idx);
    const rs = range.getClientRects();
    if (rs.length > 0) rects.push(rs[rs.length > 1 && idx > 0 ? rs.length - 1 : 0]);
  }
  const b = e.el.getBoundingClientRect();
  const r = rects.map((x) => ({ left: x.left, top: x.top, height: x.height })).at(0) ?? { left: e.kind === "at" && p > e.start ? b.right : b.left, top: b.top, height: b.height };
  const box = e.el.closest(".l");
  const lh = box !== null ? parseFloat(getComputedStyle(box).lineHeight) : NaN;
  return lh > 0 ? { left: r.left, top: r.top + r.height / 2 - lh / 2, height: lh } : r;
}

export class DocView {
  constructor() {
    this.el = document.createElement("div");
    this.el.className = "doc";
    this.el.innerHTML = `<div class="doc-content"><div class="doc-body"></div><div class="doc-carets"></div></div>`;
    this.content = within(this.el, ".doc-content");
    this.body = within(this.el, ".doc-body");
    this.caretsEl = within(this.el, ".doc-carets");
    this.drawn = noDrawn(); // the drawn blocks and rows, with their HTML
    this.caretsHtml = ""; // the carets drawn
    this.map = noPieces(); // drawn pieces in byte order
    this.dom = noPieces(); // the same, in document order
    this.lineY = noLineTops(); // each drawn line's top, in order, in document px
    this.sy = 0; // the scroll position, in document px
    this.want = "none"; // keep the primary cursor in view: "keep", "center" or "none"
    this.moved = 0; // how far the last frame went past the asked scroll, in px
    this.range = document.createRange();
  }

  clear() {
    this.body.textContent = "";
    this.caretsEl.textContent = "";
    this.caretsHtml = "";
    this.drawn = [];
    this.map = [];
    this.dom = [];
    this.lineY = [];
  }

  // ---------------------------------------------------------------- frame

  render(v) {
    const pane = v.pane;
    const { cw, lh } = pane.metrics();
    const g = pane.geometry(v);
    // The compose width, leaving a margin on each side for fold toggles.
    const room = (pane.size.w || pane.surfaceEl.clientWidth) - 12 - 2 * FOLD_MARGIN;
    const w = `${Math.max(10 * cw, Math.min(g.width * cw, room))}px`;
    if (this.content.style.width !== w) this.content.style.width = w;
    const ctx = context(v);
    // A frame is normally one pass; a jump (keeping a far cursor in view)
    // or an offset beyond what was drawn takes another, from the new top.
    let last;
    this.moved = 0;
    for (let pass = 0; pass < 4; pass++) {
      last = this.pass(v, ctx, lh);
      if (!last.again) break;
    }
    // Writes only from here: the translation and the carets.
    this.content.style.transform = `translateY(${-this.sy}px)`;
    let html = "";
    if (pane.active) {
      for (const c of last.carets) html += `<div class="caret" style="left:${c.x}px;top:${c.y}px;height:${c.h}px"></div>`;
    }
    if (this.caretsHtml !== html) { this.caretsEl.innerHTML = html; this.caretsHtml = html; }
  }

  pass(v, ctx, lh) {
    const { L } = ctx;
    const h = v.pane.size.h > 0 ? v.pane.size.h : v.pane.surfaceEl.clientHeight;
    for (const k of ctx.hidden(L.indexOf(v.top))) v.setTop(L.starts[k.keep], 0);
    const ti = L.indexOf(v.top);
    // The blocks to draw: one above the top's block (more when the offset
    // is negative), and down to what surely fills the view.
    const top = blockAt(ctx.base, ti);
    const blocks = [top];
    let cur = top, above = 0;
    for (let k; (k = shownFrom(ctx, cur.first - 1, -1)) >= 0 && (blocks.length === 1 || above < -v.topPx);) {
      cur = blockAt(ctx.base, k);
      blocks.unshift(cur);
      above += atLeastHeight(ctx, cur, lh, cur.first);
    }
    let below = atLeastHeight(ctx, top, lh, ti);
    cur = top;
    for (let k; below < h + Math.max(0, v.topPx) + lh && (k = shownFrom(ctx, cur.last + 1, 1)) < L.count;) {
      cur = blockAt(ctx.base, k);
      blocks.push(cur);
      below += atLeastHeight(ctx, cur, lh, cur.first);
    }

    // Write: each block's HTML, its element reused while that is unchanged.
    const pool = poolOf(this.drawn);
    const drawn = [];
    const blockEls = [];
    for (const blk of blocks) {
      for (let k = blk.first; k <= blk.last; k++) for (const x of ctx.vlines.get(k) ?? []) if (x.above) drawn.push(take(pool, vline(x)));
      const b = take(pool, blockHTML(ctx, blk));
      blockEls.push(b.el);
      drawn.push(b);
      for (let k = blk.first; k <= blk.last; k++) for (const x of ctx.vlines.get(k) ?? []) if (!x.above) drawn.push(take(pool, vline(x)));
    }
    if (!sameDrawn(drawn, this.drawn)) this.body.replaceChildren(...drawn.map((d) => d.el));
    this.drawn = drawn;

    // Read: the pieces, and each line's top (a block's first line: the
    // block's top; others: their first piece; a line drawing nothing: the
    // line before it).
    const cr = this.content.getBoundingClientRect();
    const y0 = cr.top;
    const dom = noPieces();
    for (const el of this.body.querySelectorAll("[data-p],[data-a],[data-at]")) {
      const ds = el.dataset;
      const p = ds.p, a = ds.a;
      if (p !== undefined) { const s = Number(p); dom.push({ el, kind: "p", start: s, end: s + byteLen(el.textContent) }); }
      else if (a !== undefined) dom.push({ el, kind: "a", start: Number(a), end: Number(a) });
      else dom.push({ el, kind: "at", start: Number(ds.at), end: Number(ds.e) });
    }
    this.dom = dom;
    this.map = [...dom].sort((a, b) => a.start - b.start);
    const ys = new Map();
    blocks.forEach((blk, i) => { ys.set(blk.first, blockEls[i].getBoundingClientRect().top - y0); });
    for (const e of dom) {
      const li = L.indexOf(e.start);
      if (li >= 0 && !ys.has(li)) ys.set(li, e.el.getBoundingClientRect().top - y0);
    }
    const first = blocks[0].first, lastLine = blocks[blocks.length - 1].last;
    // Each drawn line's top (a folded line has none).
    const lineY = noLineTops(), yAt = new Map();
    let prev = ys.get(first) ?? 0;
    for (let k = first; k <= lastLine; k++) {
      if (ctx.hidden(k).length > 0) continue;
      const y = Math.max(prev, ys.get(k) ?? prev);
      lineY.push({ line: k, y });
      yAt.set(k, y);
      prev = y;
    }
    this.lineY = lineY;
    const yOf = (li) => yAt.get(li) ?? 0;
    const atEnd = shownFrom(ctx, lastLine + 1, 1) >= L.count;
    const bottom = this.body.getBoundingClientRect().bottom - y0;

    // The scroll position, kept on the primary cursor when asked.
    const asked = yOf(ti) + v.topPx;
    let sy = asked;
    const carets = [];
    for (const c of v.cursors) {
      const r = this.caretRect(c.pos);
      for (const b of listOf(r)) carets.push({ x: b.left - cr.left, y: b.top - y0, h: b.height || lh, c });
    }
    if (this.want !== "none") {
      // A cursor in a fold is kept in view at the line before the fold.
      const folded = ctx.hidden(L.indexOf(v.primary.pos));
      const ci = folded.length > 0 ? folded[0].keep : L.indexOf(v.primary.pos);
      // Where the primary caret is drawn (none: not drawn).
      const drawnCaret = carets.find((x) => x.c === v.primary);
      const pcs = drawnCaret !== undefined ? [{ y: drawnCaret.y, h: drawnCaret.h }] : folded.length > 0 && yAt.has(ci) ? [{ y: yOf(ci), h: lh }] : [];
      for (const pc of pcs) {
        if (this.want === "center") sy = pc.y - h / 2;
        else if (pc.y < sy) sy = pc.y;
        else if (pc.y + pc.h > sy + h) sy = pc.y + pc.h - h;
      }
      if (!pcs.length) {
        // Not drawn: start again from its line.
        const back = this.want === "center" ? h / 2 : ci < ti ? lh : h - 3 * lh;
        v.setTop(L.starts[ci], -Math.max(0, back));
        return { again: true, carets: [] };
      }
      this.want = "none";
    }
    // At the document's ends: its end stops at the view's bottom; there is
    // no space above its start.
    if (atEnd && L.eof) sy = Math.min(sy, cr.height - h);
    if (first === 0 && L.bof) sy = Math.max(sy, 0);
    // The new top: the last line starting at or above the scroll position.
    let k = 0;
    while (k + 1 < lineY.length && lineY[k + 1].y <= sy) k++;
    v.setTop(L.starts[lineY[k].line], sy - lineY[k].y);
    this.sy = sy;
    // How far the view went past what was asked (held at an end, or kept
    // on the cursor), in pixels.
    this.moved += sy - asked;
    // Drawn too little above or below (an offset past what was estimated)?
    const again = (sy < lineY[0].y && first > 0) || (sy + h > bottom && !atEnd);
    return { again, carets };
  }

  // ---------------------------------------------------------------- blocks

  // ---------------------------------------------------------------- bytes and points

  // The drawn piece for a caret at `p`: inside text, then at its end, an
  // anchor, then beside a concealed range.
  find(p) {
    const m = this.map;
    let lo = 0, hi = m.length;
    while (lo < hi) { const mid = (lo + hi) >> 1; if (m[mid].start <= p) lo = mid + 1; else hi = mid; }
    // The candidates by rank, the first found winning a tie.
    const ranked = [];
    for (let i = lo - 1; i >= 0 && i >= lo - 8; i--) {
      const e = m[i];
      if (p < e.start || p > e.end) continue;
      ranked.push({ rank: e.kind === "p" ? (p < e.end ? 0 : 1) : e.kind === "a" ? 2 : p === e.start ? 3 : p === e.end ? 4 : 5, e });
    }
    return ranked.sort((x, y) => x.rank - y.rank).map((x) => x.e).at(0) ?? NO_PIECE;
  }

  // Where a caret at byte `p` is drawn, in client coordinates, or null when
  // it is not drawn: as tall as its line's box (the line height of the line
  // element holding it), centred on the glyphs there.
  caretRect(p) {
    for (const e of listOf(this.find(p))) return some(caretRectOf(this.range, e, p));
    return NO_RECT;
  }

  // The byte at client point (x, y), held to the document's box.
  posFromPoint(v, x0, y0) {
    const cr = this.content.getBoundingClientRect();
    const sr = v.pane.surfaceEl.getBoundingClientRect();
    let x = Math.max(cr.left + 1, Math.min(cr.right - 1, x0));
    let y = Math.max(sr.top + 1, Math.min(sr.bottom - 1, y0));
    // Off the text (right of a short table, between blocks): into the
    // nearest line box, vertically first.
    const hit = document.elementFromPoint(x, y);
    if (hit === null || hit.closest(".l") === null || !this.body.contains(hit)) {
      const lines = this.drawn.flatMap((d) => (d.el.matches(".l") ? [d.el] : [...d.el.querySelectorAll(".l")]));
      const near = lines.map((l) => {
        const r = l.getBoundingClientRect();
        return { r, dy: y < r.top ? r.top - y : y >= r.bottom ? y - r.bottom + 1 : 0, dx: x < r.left ? r.left - x : x > r.right ? x - r.right : 0 };
      }).sort((a, b) => a.dy - b.dy || a.dx - b.dx);
      for (const n of near.slice(0, 1)) {
        x = Math.max(n.r.left + 1, Math.min(n.r.right - 1, x));
        y = Math.max(n.r.top + 1, Math.min(n.r.bottom - 1, y));
      }
    }
    for (const c of caretAt(x, y)) {
      if (this.body.contains(c.node)) return this.posAt(c.node, c.off);
    }
    return NO_POS;
  }

  // The byte at DOM point (`node`, `off`) in the body.
  posAt(node, off) {
    const text = node.nodeType === 3;
    const within = elementAt(node);
    if (within.length === 0) return NO_POS;
    const el = within[0];
    const ent = el.closest("[data-p],[data-a],[data-at]");
    if (ent !== null && this.body.contains(ent) && (ent.dataset.at === undefined || ent.firstChild !== null)) {
      const ds = ent.dataset;
      const p = ds.p, a = ds.a;
      if (p !== undefined) return some(Number(p) + byteLen(text ? node.textContent.slice(0, off) : off ? ent.textContent : ""));
      if (a !== undefined) return some(Number(a));
      return some(text && off * 2 > node.textContent.length ? Number(ds.e) : Number(ds.at));
    }
    // Between pieces (a list marker, a cell's padding) or on markup drawn as
    // nothing: the shown piece after the point in its line, else the one
    // before it.
    this.range.setStart(node, off);
    this.range.setEnd(node, off);
    const dom = this.dom;
    let lo = 0, hi = dom.length;
    while (lo < hi) { const mid = (lo + hi) >> 1; if (this.range.comparePoint(dom[mid].el, 0) < 0) lo = mid + 1; else hi = mid; }
    const box = el.closest(".l");
    const inBox = (i) => i >= 0 && i < dom.length && (box === null || box.contains(dom[i].el));
    // From piece `i` on in direction `d`, past empty concealed ranges.
    const near = (i, d) => {
      let k = i;
      while (dom[k].kind === "at" && dom[k].el.firstChild === null && inBox(k + d)) k += d;
      return dom[k];
    };
    if (inBox(lo)) return some(near(lo, 1).start);
    if (inBox(lo - 1)) return some(near(lo - 1, -1).end);
    return NO_POS;
  }

  // ---------------------------------------------------------------- motion

  // The byte one visual line above or below `p`, at x `goal` (document px):
  // probe points beyond the caret's line until one lands on another line,
  // scrolling when the probe leaves the view.
  lineFrom(v, p, dir, goal) {
    const lh = v.pane.metrics().lh;
    const r = listOf(this.caretRect(p)).at(0);
    if (r === undefined) return NO_POS;
    let top = r.top, bot = r.top + (r.height > 0 ? r.height : lh);
    let y = dir > 0 ? bot : top;
    for (let guard = 0; guard < 2000; guard++) {
      y += dir * 3;
      const sr = v.pane.surfaceEl.getBoundingClientRect();
      if (y <= sr.top + 1 || y >= sr.bottom - 1) {
        const shift = this.scrollBy(v, dir * Math.max(lh, (v.pane.size.h > 0 ? v.pane.size.h : sr.height) / 2));
        if (shift === 0) return NO_POS;
        y -= shift; top -= shift; bot -= shift;
        continue;
      }
      for (const q of listOf(this.posFromPoint(v, this.content.getBoundingClientRect().left + goal, y))) {
        const below = listOf(this.caretRect(q)).some((b) => (dir > 0 ? b.top >= bot - 2 : b.top + (b.height > 0 ? b.height : lh) <= top + 2));
        if (below) return some(q);
      }
    }
    return NO_POS;
  }

  // Scroll by `dy` pixels and draw: how far the view really moved (less at
  // the document's ends).
  scrollBy(v, dy) {
    v.setTop(v.top, v.topPx + dy);
    this.render(v);
    return dy + this.moved;
  }

  // The caret's x in document px.
  caretX(p) {
    return listOf(this.caretRect(p)).map((r) => r.left - this.content.getBoundingClientRect().left).at(0) ?? NO_POS;
  }
}

