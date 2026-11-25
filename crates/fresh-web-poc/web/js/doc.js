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

// Node types that live inside a block rather than being one.
const PART = new Set(["row", "cell"]);
// Block types that keep their lines (the others are joined into one run).
const LINES = new Set(["code", "table"]);
// Pixels kept clear left (and right) of the document for fold toggles.
const FOLD_MARGIN = 40;

const PLAIN = styleKey(EMPTY);
const anchor = (p) => `<span data-a="${p}">​</span>`;

export class DocView {
  constructor() {
    this.el = document.createElement("div");
    this.el.className = "doc";
    this.el.innerHTML = `<div class="doc-content"><div class="doc-body"></div><div class="doc-carets"></div></div>`;
    this.content = this.el.firstChild;
    this.body = this.content.firstChild;
    this.caretsEl = this.content.lastChild;
    this.els = []; // the drawn block elements
    this.map = []; // drawn pieces in byte order: {el, start, end, kind: "p"|"a"|"at"}
    this.dom = []; // the same, in document order
    this.lineY = []; // [line, y] for each drawn line, in order, y in document px
    this.sy = 0; // the scroll position, in document px
    this.want = null; // keep the primary cursor in view: "keep" | "center"
    this.range = document.createRange();
  }

  clear() {
    this.body.textContent = "";
    this.caretsEl.textContent = "";
    this.caretsEl._html = "";
    this.els = [];
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
    const ctx = this.context(v);
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
    if (pane.app().activePane === pane) {
      for (const c of last.carets) html += `<div class="caret" style="left:${c.x}px;top:${c.y}px;height:${c.h}px"></div>`;
    }
    if (this.caretsEl._html !== html) { this.caretsEl.innerHTML = html; this.caretsEl._html = html; }
  }

  // What drawing needs for the whole frame: decorations by line, styles.
  context(v) {
    const buf = v.buf;
    const L = buf.text;
    const d = buf.decor;
    const cursors = v.cursors.map((c) => c.pos);
    const o = v.pane.layoutOpts(v, 0);
    const vlines = new Map();
    for (const x of d.lines) {
      if (!applies(x, v.mode)) continue;
      const i = L.indexOf(x.pos);
      if (i >= 0) (vlines.get(i) || vlines.set(i, []).get(i)).push(x);
    }
    const lineCache = new Map();
    // A line's active conceals (sorted, the longest first at one start),
    // the revealed ones (drawn dim) and its inline texts by position.
    const lineInfo = (li) => {
      let x = lineCache.get(li);
      if (x) return x;
      const ls = L.starts[li], le = L.lineEnd(li);
      const act = [], rev = [];
      for (const c of d.query("conceals", ls, le + 1)) {
        if (c.end <= c.start || !applies(c, v.mode)) continue;
        if (c.reveal && cursorIn(cursors, c.reveal)) rev.push(c);
        else act.push(c);
      }
      act.sort((a, b) => a.start - b.start || b.end - a.end);
      const inl = new Map();
      for (const t of d.query("inlines", ls, le + 1)) {
        if (!applies(t, v.mode) || t.pos < ls || t.pos > le) continue;
        const k = t.pos + (t.side === "after" ? "a" : "b");
        (inl.get(k) || inl.set(k, []).get(k)).push(t);
      }
      x = { act, rev, inl };
      lineCache.set(li, x);
      return x;
    };
    // The folds hide lines by the blocks they fold, which takes only the
    // view, its text and its decorations (as `hiding` asks).
    const hidden = this.hider({ v, L, d, mode: v.mode });
    return { v, L, d, o, cursors, vlines, lineInfo, mode: v.mode, hidden };
  }

  // hidden(li): the fold hiding line `li` and the last line it keeps
  // ({fold, keep}), or null.
  hider(ctx) {
    const keeps = ctx.v.folds.map((f) => {
      const sb = this.blockAt(ctx, f.start);
      return sb.node && !LINES.has(sb.node.type) ? Math.max(f.start, sb.last) : f.start;
    });
    return (li) => {
      const folds = ctx.v.folds;
      for (let i = 0; i < folds.length; i++) if (li > keeps[i] && li <= folds[i].end) return { fold: folds[i], keep: keeps[i] };
      return null;
    };
  }

  // The fold hiding line `li` of view `v`, as `hidden` gives it.
  hiding(v, li) {
    return this.hider({ v, L: v.buf.text, d: v.buf.decor, mode: v.mode })(li);
  }

  // The fold the block holding line `li` offers: {start, end} lines, or null.
  foldRange(v, li) {
    const ctx = { v, L: v.buf.text, d: v.buf.decor, mode: v.mode };
    return this.foldOf(ctx, this.blockAt(ctx, li));
  }

  // `probe`: only whether there is one (looks no further than needed).
  foldOf(ctx, blk, probe = false) {
    const { L } = ctx;
    const n = blk.node;
    if (!n) return null;
    if (n.type === "code") return blk.last - blk.first >= 3 ? { start: blk.first + 1, end: blk.last } : null;
    if (n.type === "table") return blk.last - blk.first >= 2 ? { start: blk.first, end: blk.last } : null;
    // A heading's section runs to the next heading as high; an item or a
    // quote holds the deeper ones after it. Blank lines in between belong.
    const child = (b) => n.type === "heading"
      ? !(b.node?.type === "heading" && b.node.level <= n.level)
      : b.node?.type === n.type && (b.node.depth ?? 0) > (n.depth ?? 0);
    let end = null;
    for (let k = blk.last + 1; k < L.count;) {
      const b = this.blockAt(ctx, k);
      if (!b.node && L.lines[k].trim() === "") { k++; continue; }
      if (!child(b)) break;
      end = b.last;
      k = b.last + 1;
      if (probe) break;
    }
    return end == null ? null : { start: blk.first, end };
  }

  // The next line drawn after (dir 1) or before (dir -1) line `k`, past the
  // folds: -1 or L.count at the ends.
  shownFrom(ctx, k, dir) {
    for (let h; k >= 0 && k < ctx.L.count && (h = ctx.hidden(k));) k = dir > 0 ? h.fold.end + 1 : h.keep;
    return k;
  }

  // The block holding line `li`: its node (or none) and its lines.
  blockAt(ctx, li) {
    const { L, d } = ctx;
    let best = null;
    for (const n of d.query("nodes", L.starts[li], L.lineEnd(li) + 1)) {
      if (PART.has(n.type) || !applies(n, ctx.mode)) continue;
      if (!best || n.start < best.start) best = n;
    }
    if (!best) return { node: null, first: li, last: li };
    const f = L.indexOf(best.start), l = L.indexOf(best.end);
    return { node: best, first: f < 0 ? 0 : Math.min(f, li), last: l < 0 ? L.count - 1 : Math.max(l, li) };
  }

  // A height the block surely has at least, from line `from` on: a line
  // draws at least a line height; a code block's fences and a table's
  // delimiter row may draw nothing.
  low(ctx, blk, lh, from = blk.first) {
    if (!blk.node) return from > blk.last ? 0 : lh;
    let n = 0;
    for (let k = from; k <= blk.last; k++) if (!ctx.hidden(k)) n++;
    if (blk.node.type === "code") n -= 2;
    if (blk.node.type === "table") n -= 1;
    return Math.max(0, n) * lh;
  }

  pass(v, ctx, lh) {
    const { L } = ctx;
    const h = v.pane.size.h || v.pane.surfaceEl.clientHeight;
    const hid = ctx.hidden(L.indexOf(v.top));
    if (hid) v.setTop(L.starts[hid.keep], 0);
    const ti = L.indexOf(v.top);
    // The blocks to draw: one above the top's block (more when the offset
    // is negative), and down to what surely fills the view.
    const top = this.blockAt(ctx, ti);
    const blocks = [top];
    let cur = top, above = 0;
    for (let k; (k = this.shownFrom(ctx, cur.first - 1, -1)) >= 0 && (blocks.length === 1 || above < -v.topPx);) {
      cur = this.blockAt(ctx, k);
      blocks.unshift(cur);
      above += this.low(ctx, cur, lh);
    }
    let below = this.low(ctx, top, lh, ti);
    cur = top;
    for (let k; below < h + Math.max(0, v.topPx) + lh && (k = this.shownFrom(ctx, cur.last + 1, 1)) < L.count;) {
      cur = this.blockAt(ctx, k);
      blocks.push(cur);
      below += this.low(ctx, cur, lh);
    }

    // Write: each block's HTML, its element reused while that is unchanged.
    const pool = new Map();
    for (const el of this.els) (pool.get(el._html) || pool.set(el._html, []).get(el._html)).push(el);
    const take = (html) => {
      const list = pool.get(html);
      if (list && list.length) return list.pop();
      const t = document.createElement("template");
      t.innerHTML = html;
      const el = t.content.firstChild;
      el._html = html;
      return el;
    };
    const els = [];
    for (const blk of blocks) {
      for (let k = blk.first; k <= blk.last; k++) for (const x of ctx.vlines.get(k) || []) if (x.above) els.push(take(this.vline(x)));
      blk.el = take(this.blockHTML(ctx, blk));
      els.push(blk.el);
      for (let k = blk.first; k <= blk.last; k++) for (const x of ctx.vlines.get(k) || []) if (!x.above) els.push(take(this.vline(x)));
    }
    if (els.length !== this.els.length || els.some((x, i) => x !== this.els[i])) this.body.replaceChildren(...els);
    this.els = els;

    // Read: the pieces, and each line's top (a block's first line: the
    // block's top; others: their first piece; a line drawing nothing: the
    // line before it).
    const cr = this.content.getBoundingClientRect();
    const y0 = cr.top;
    const dom = [];
    for (const el of this.body.querySelectorAll("[data-p],[data-a],[data-at]")) {
      const ds = el.dataset;
      if (ds.p != null) { const s = +ds.p; dom.push({ el, kind: "p", start: s, end: s + byteLen(el.textContent) }); }
      else if (ds.a != null) dom.push({ el, kind: "a", start: +ds.a, end: +ds.a });
      else dom.push({ el, kind: "at", start: +ds.at, end: +ds.e });
    }
    this.dom = dom;
    this.map = [...dom].sort((a, b) => a.start - b.start);
    const ys = new Map();
    for (const blk of blocks) ys.set(blk.first, blk.el.getBoundingClientRect().top - y0);
    for (const e of dom) {
      const li = L.indexOf(e.start);
      if (li >= 0 && !ys.has(li)) ys.set(li, e.el.getBoundingClientRect().top - y0);
    }
    const first = blocks[0].first, lastLine = blocks[blocks.length - 1].last;
    // Each drawn line's top (a folded line has none).
    const lineY = [], yAt = new Map();
    let prev = ys.get(first);
    for (let k = first; k <= lastLine; k++) {
      if (ctx.hidden(k)) continue;
      const y = Math.max(prev, ys.get(k) ?? prev);
      lineY.push([k, y]);
      yAt.set(k, y);
      prev = y;
    }
    this.lineY = lineY;
    const yOf = (li) => yAt.get(li);
    const atEnd = this.shownFrom(ctx, lastLine + 1, 1) >= L.count;
    const bottom = this.body.getBoundingClientRect().bottom - y0;

    // The scroll position, kept on the primary cursor when asked.
    const asked = yOf(ti) + v.topPx;
    let sy = asked;
    const carets = [];
    for (const c of v.cursors) {
      const r = this.caretRect(c.pos);
      if (r) carets.push({ x: r.left - cr.left, y: r.top - y0, h: r.height || lh, c });
    }
    if (this.want) {
      // A cursor in a fold is kept in view at the line before the fold.
      let ci = L.indexOf(v.primary.pos);
      const fh = ctx.hidden(ci);
      if (fh) ci = fh.keep;
      const pc = carets.find((x) => x.c === v.primary) || (fh && yAt.has(ci) && { y: yOf(ci), h: lh });
      if (!pc) {
        // Not drawn: start again from its line.
        const back = this.want === "center" ? h / 2 : ci < ti ? lh : h - 3 * lh;
        v.setTop(L.starts[ci], -Math.max(0, back));
        return { again: true };
      }
      if (this.want === "center") sy = pc.y - h / 2;
      else if (pc.y < sy) sy = pc.y;
      else if (pc.y + pc.h > sy + h) sy = pc.y + pc.h - h;
      this.want = null;
    }
    // At the document's ends: its end stops at the view's bottom; there is
    // no space above its start.
    if (atEnd && L.eof) sy = Math.min(sy, cr.height - h);
    if (first === 0 && L.bof) sy = Math.max(sy, 0);
    // The new top: the last line starting at or above the scroll position.
    let k = 0;
    while (k + 1 < lineY.length && lineY[k + 1][1] <= sy) k++;
    v.setTop(L.starts[lineY[k][0]], sy - lineY[k][1]);
    this.sy = sy;
    // How far the view went past what was asked (held at an end, or kept
    // on the cursor), in pixels.
    this.moved += sy - asked;
    // Drawn too little above or below (an offset past what was estimated)?
    const again = (sy < lineY[0][1] && first > 0) || (sy + h > bottom && !atEnd);
    return { again, carets };
  }

  vline(x) {
    const st = cssOf(drawnOf(x.style));
    return `<div class="vline"${st ? ` style="${st}"` : ""}>${esc(x.text || "")}​</div>`;
  }

  // ---------------------------------------------------------------- blocks

  blockHTML(ctx, blk) {
    const { L } = ctx;
    const n = blk.node;
    const ls = L.starts[blk.first], le = L.lineEnd(blk.last);
    // The fold toggle (shown on hover, or while folded) and, while folded,
    // an ellipsis where the hidden part was.
    const folded = ctx.v.folds.some((f) => f.start >= blk.first && f.start <= blk.last);
    const tog = folded || this.foldOf(ctx, blk, true) ? `<span class="doc-fold${folded ? " folded" : ""}" data-fold="${blk.first}">${folded ? "▸" : "▾"}</span>` : "";
    const more = folded ? `<span class="doc-more" data-fold="${blk.first}">⋯</span>` : "";
    const joined = () => { const r = this.walk(ctx, ls, le); return tog + r.html + (r.shown ? "" : anchor(le)) + more; };
    if (!n) return `<p class="l">${joined()}</p>`;
    switch (n.type) {
      case "heading": {
        const lv = Math.max(1, Math.min(6, n.level || 1));
        return `<h${lv} class="l">${joined()}</h${lv}>`;
      }
      case "item": {
        const m = String(n.marker ?? "•");
        const cls = m === "☑" ? " done" : m === "☐" ? " todo" : /\d/.test(m) ? " num" : "";
        return `<div class="doc-li" style="--depth:${n.depth || 0}"><div class="l"><span class="mk${cls}">${esc(m)}</span>${joined()}</div></div>`;
      }
      case "quote": return `<blockquote class="l" style="--depth:${n.depth || 1}">${joined()}</blockquote>`;
      case "rule": return `<div class="doc-hr l">${joined()}</div>`;
      case "code": {
        const lang = fenceLanguage(n.lang);
        const syn = lang ? (li) => spansFor(lang, L.lines[li]) : null;
        let html = `<div class="doc-code"${n.lang ? ` data-lang="${esc(n.lang)}"` : ""}>${tog}`;
        for (let k = blk.first; k <= blk.last; k++) {
          if (ctx.hidden(k)) continue;
          const r = this.walk(ctx, L.starts[k], L.lineEnd(k), syn);
          // A line whose text is all concealed draws nothing.
          if (!r.shown && L.lines[k] !== "") continue;
          html += `<div class="ln l">${r.html}${r.shown ? "" : anchor(L.lineEnd(k))}</div>`;
        }
        return html + (folded ? `<div class="ln">${more}</div>` : "") + "</div>";
      }
      case "table": return this.tableHTML(ctx, blk, tog);
      default: return `<p class="l">${joined()}</p>`;
    }
  }

  tableHTML(ctx, blk, tog) {
    const { L, d } = ctx;
    const n = blk.node;
    const parts = d.query("nodes", n.start, n.end + 1).filter((x) => PART.has(x.type) && applies(x, ctx.mode));
    const rows = new Map(), cells = new Map();
    for (const x of parts) {
      const li = L.indexOf(x.start);
      if (x.type === "row") rows.set(li, x);
      else (cells.get(li) || cells.set(li, []).get(li)).push(x);
    }
    let ncol = 1;
    for (const list of cells.values()) { list.sort((a, b) => a.start - b.start); ncol = Math.max(ncol, list.length); }
    let html = `<table class="doc-table"><tbody>`;
    for (let k = blk.first; k <= blk.last; k++) {
      if (ctx.hidden(k)) continue;
      const ls = L.starts[k], le = L.lineEnd(k);
      const list = cells.get(k);
      // The fold toggle sits in the first cell drawn, placed by the table.
      const t = tog;
      tog = "";
      if (!list) {
        // A row without cells (the delimiter row): its text across the table.
        const r = this.walk(ctx, ls, le);
        if (!r.shown && L.lines[k] !== "") continue;
        html += `<tr class="raw"><td class="l" colspan="${ncol}">${t}${r.html}${r.shown ? "" : anchor(le)}</td></tr>`;
        continue;
      }
      const tag = rows.get(k)?.header ? "th" : "td";
      html += "<tr>";
      list.forEach((c, i) => {
        const a = i === 0 ? ls : c.start, z = i + 1 < list.length ? list[i + 1].start : le;
        const r = this.walk(ctx, a, z);
        const al = n.align?.[i];
        html += `<${tag} class="l"${al ? ` style="text-align:${al}"` : ""}>${i === 0 ? t : ""}${r.html}${r.shown ? "" : anchor(z)}</${tag}>`;
      });
      for (let i = list.length; i < ncol; i++) html += `<${tag}></${tag}>`;
      html += "</tr>";
    }
    return html + "</tbody></table>";
  }

  // The HTML of source [a, z): runs of equally styled text as spans, each
  // with its first byte; concealed ranges as their replacement; inline text.
  // A line break inside the range draws as a space (or, concealed with
  // "\n", as a line break). `syn(li)`: syntax spans for a line.
  walk(ctx, a, z, syn) {
    const { L, o } = ctx;
    let out = "", shown = false;
    let li = L.indexOf(a), b = a;
    while (li >= 0 && li < L.count && b <= z) {
      const text = L.lines[li], ls = L.starts[li], le = L.lineEnd(li);
      const { act, rev, inl } = ctx.lineInfo(li);
      const spans = syn ? syn(li) : null;
      let run = "", at = 0, key = null, style = "";
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
        for (const t of inl.get(p + side) || []) {
          flush();
          const st = cssOf(drawnOf(t.style));
          out += `<span class="vt${t.edge ? " edge" : ""}"${st ? ` style="${st}"` : ""}>${esc(t.text)}</span>`;
        }
      };
      const styleAt = (p, idx) => {
        let st = o.decoStyle(p, li, spans ? synAt(spans, idx) : EMPTY);
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
          out += this.concealHTML(c);
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
            out += this.concealHTML(c);
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

  concealHTML(c) {
    const st = cssOf(drawnOf(c.style));
    const body = c.text === "\n" ? "<br>" : esc(c.text);
    return `<span class="cc" data-at="${c.start}" data-e="${c.end}"${st ? ` style="${st}"` : ""}>${body}</span>`;
  }

  // ---------------------------------------------------------------- bytes and points

  // The drawn piece for a caret at `p`: inside text, then at its end, an
  // anchor, then beside a concealed range.
  find(p) {
    const m = this.map;
    let lo = 0, hi = m.length;
    while (lo < hi) { const mid = (lo + hi) >> 1; if (m[mid].start <= p) lo = mid + 1; else hi = mid; }
    let best = null, rank = 9;
    for (let i = lo - 1; i >= 0 && i >= lo - 8; i--) {
      const e = m[i];
      if (p < e.start || p > e.end) continue;
      const r = e.kind === "p" ? (p < e.end ? 0 : 1) : e.kind === "a" ? 2 : p === e.start ? 3 : p === e.end ? 4 : 5;
      if (r < rank) { rank = r; best = e; }
    }
    return best;
  }

  // Where a caret at byte `p` is drawn, in client coordinates, or null when
  // it is not drawn: as tall as its line's box (the line height of the line
  // element holding it), centred on the glyphs there.
  caretRect(p) {
    const e = this.find(p);
    if (!e) return null;
    let r = null;
    if (e.kind === "p") {
      const tn = e.el.firstChild;
      const idx = byteToIdx(tn.data, p - e.start);
      this.range.setStart(tn, idx);
      this.range.setEnd(tn, idx);
      const rs = this.range.getClientRects();
      if (rs.length) r = rs[rs.length > 1 && idx > 0 ? rs.length - 1 : 0];
    }
    if (!r) {
      const b = e.el.getBoundingClientRect();
      r = { left: e.kind === "at" && p > e.start ? b.right : b.left, top: b.top, height: b.height };
    }
    const box = e.el.closest(".l");
    const lh = box ? parseFloat(getComputedStyle(box).lineHeight) : NaN;
    if (!(lh > 0)) return { left: r.left, top: r.top, height: r.height };
    return { left: r.left, top: r.top + r.height / 2 - lh / 2, height: lh };
  }

  // The byte at client point (x, y), held to the document's box.
  posFromPoint(v, x, y) {
    const cr = this.content.getBoundingClientRect();
    const sr = v.pane.surfaceEl.getBoundingClientRect();
    x = Math.max(cr.left + 1, Math.min(cr.right - 1, x));
    y = Math.max(sr.top + 1, Math.min(sr.bottom - 1, y));
    // Off the text (right of a short table, between blocks): into the
    // nearest line box, vertically first.
    const hit = document.elementFromPoint(x, y);
    if (!hit || !hit.closest(".l") || !this.body.contains(hit)) {
      const dist = (r) => [y < r.top ? r.top - y : y >= r.bottom ? y - r.bottom + 1 : 0, x < r.left ? r.left - x : x > r.right ? x - r.right : 0];
      let best = null, bd = [Infinity, Infinity];
      for (const blk of this.els) {
        if (dist(blk.getBoundingClientRect())[0] > bd[0]) continue;
        for (const l of blk.matches(".l") ? [blk] : blk.querySelectorAll(".l")) {
          const lr = l.getBoundingClientRect();
          const d = dist(lr);
          if (d[0] < bd[0] || (d[0] === bd[0] && d[1] < bd[1])) { bd = d; best = lr; }
        }
      }
      if (best) {
        x = Math.max(best.left + 1, Math.min(best.right - 1, x));
        y = Math.max(best.top + 1, Math.min(best.bottom - 1, y));
      }
    }
    let node = null, off = 0;
    if (document.caretPositionFromPoint) {
      const c = document.caretPositionFromPoint(x, y);
      if (c) { node = c.offsetNode; off = c.offset; }
    } else if (document.caretRangeFromPoint) {
      const r = document.caretRangeFromPoint(x, y);
      if (r) { node = r.startContainer; off = r.startOffset; }
    }
    if (!node || !this.body.contains(node)) return null;
    const el = node.nodeType === 3 ? node.parentElement : node;
    const ent = el.closest("[data-p],[data-a],[data-at]");
    if (ent && this.body.contains(ent) && (ent.dataset.at == null || ent.firstChild)) {
      const ds = ent.dataset;
      if (ds.p != null) return +ds.p + byteLen(node.nodeType === 3 ? node.data.slice(0, off) : off ? ent.textContent : "");
      if (ds.a != null) return +ds.a;
      return node.nodeType === 3 && off * 2 > node.data.length ? +ds.e : +ds.at;
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
    const near = (i, d) => {
      const inBox = (e) => e && (!box || box.contains(e.el));
      if (!inBox(dom[i])) return null;
      while (dom[i].kind === "at" && !dom[i].el.firstChild && inBox(dom[i + d])) i += d;
      return dom[i];
    };
    const next = near(lo, 1), prev = near(lo - 1, -1);
    if (next) return next.start;
    if (prev) return prev.end;
    return null;
  }

  // Lines [first, last] drawn on screen.
  visibleLines(v) {
    const h = v.pane.size.h;
    const on = this.lineY.filter(([, y], i) => y < this.sy + h && (this.lineY[i + 1]?.[1] ?? Infinity) > this.sy);
    return on.length ? { first: on[0][0], last: on[on.length - 1][0] } : null;
  }

  // ---------------------------------------------------------------- motion

  // The byte one visual line above or below `p`, at x `goal` (document px):
  // probe points beyond the caret's line until one lands on another line,
  // scrolling when the probe leaves the view.
  lineFrom(v, p, dir, goal) {
    const lh = v.pane.metrics().lh;
    let r = this.caretRect(p);
    if (!r) return null;
    let top = r.top, bot = r.top + (r.height || lh);
    let y = dir > 0 ? bot : top;
    for (let guard = 0; guard < 2000; guard++) {
      y += dir * 3;
      const sr = v.pane.surfaceEl.getBoundingClientRect();
      if (y <= sr.top + 1 || y >= sr.bottom - 1) {
        const shift = this.scrollBy(v, dir * Math.max(lh, (v.pane.size.h || sr.height) / 2));
        if (!shift) return null;
        y -= shift; top -= shift; bot -= shift;
        continue;
      }
      const q = this.posFromPoint(v, this.content.getBoundingClientRect().left + goal, y);
      if (q == null) continue;
      const qr = this.caretRect(q);
      if (qr && (dir > 0 ? qr.top >= bot - 2 : qr.top + (qr.height || lh) <= top + 2)) return q;
    }
    return null;
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
    const r = this.caretRect(p);
    return r ? r.left - this.content.getBoundingClientRect().left : null;
  }
}

