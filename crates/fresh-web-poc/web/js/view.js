// A pane and the views in it: one view per buffer tab, each with its own
// scroll, cursors, options and folds — all of it this client's.

import { layout, applies, cursorIn } from "./layout.js";
import { byteLen } from "./ot.js";
import { byteToIdx } from "./buffer.js";
import { color, THEME } from "./highlight.js";
import { classify } from "./mdstruct.js";
import { EMPTY, NO_SPANS, spansFor, synAt, fenceLanguage, esc, cssFor, styleKey } from "./paint.js";
import { DocView } from "./doc.js";


let nextPaneId = 1;

export const sel = (c) => (c.anchor == null || c.anchor === c.pos ? null : { start: Math.min(c.anchor, c.pos), end: Math.max(c.anchor, c.pos) });

// One buffer as shown in one pane.
export class View {
  constructor(pane, buf) {
    this.pane = pane;
    this.buf = buf;
    // The scroll position: the first visible source line's start, and (in a
    // compose view) how many pixels of that line are scrolled past.
    this._top = 0;
    this.topPx = 0;
    // Every cursor: position, selection anchor (or null) and the column
    // vertical motion aims for. The first is the primary.
    this.cursors = [{ pos: 0, anchor: null, goalX: null }];
    this.mode = "source"; // or "compose"
    this.opts = { composeWidth: null, wrap: false, lineNumbers: !buf.isVirtual };
    this.folds = []; // [{start, end}] window line indices; start's line stays
  }
  get primary() { return this.cursors[0]; }
  get top() { return this._top; }
  // Show the line starting at `b` at the top, `px` pixels into it.
  setTop(b, px = 0) { this._top = b; this.topPx = px; }
  shift(op) {
    const sp = (p, r) => {
      if (op.t === "ins") return p > op.pos || (p === op.pos && r) ? p + byteLen(op.text) : p;
      if (p >= op.pos + op.len) return p - op.len;
      return p > op.pos ? op.pos : p;
    };
    for (const c of this.cursors) {
      c.pos = sp(c.pos, true);
      if (c.anchor != null) c.anchor = sp(c.anchor, false);
    }
    this._top = sp(this._top, false);
  }
  selection() { return sel(this.cursors[0]); }
  selections() { return this.cursors.map(sel).filter(Boolean); }
  // Sort cursors by position (primary kept first) and merge any that met.
  normalize() {
    const prim = this.cursors[0];
    const lo = (x) => Math.min(x.pos, x.anchor ?? x.pos), hi = (x) => Math.max(x.pos, x.anchor ?? x.pos);
    const out = [];
    for (const c of [...this.cursors].sort((a, b) => lo(a) - lo(b))) {
      const prev = out[out.length - 1];
      if (prev && (prev.pos === c.pos || lo(c) < hi(prev))) { if (c === prim) out[out.length - 1] = c; continue; }
      out.push(c);
    }
    const i = out.indexOf(prim);
    if (i > 0) { out.splice(i, 1); out.unshift(prim); }
    this.cursors = out.length ? out : [prim];
  }
  dispose() { this.buf.views.delete(this); }
}

export class Pane {
  constructor(app) {
    this.app = app;
    this.id = nextPaneId++;
    this.views = []; // tabs
    this.view = null;
    this.el = document.createElement("div");
    this.el.className = "pane";
    this.el.innerHTML = `<div class="tabs"></div><div class="surface"><div class="rulers"></div><div class="gutter"></div><div class="text"></div><div class="carets"></div><div class="scrollbar"><div class="thumb"></div></div></div>`;
    const el = this.el;
    const part = (k) => el.querySelector("." + k);
    this.tabsEl = part("tabs");
    this.surfaceEl = part("surface");
    this.rulersEl = part("rulers");
    this.gutterEl = part("gutter");
    this.textEl = part("text");
    this.caretsEl = part("carets");
    this.scrollbarEl = part("scrollbar");
    this.thumbEl = part("thumb");
    // A view in compose mode is drawn as a document instead of rows.
    this.doc = new DocView();
    this.surfaceEl.insertBefore(this.doc.el, this.scrollbarEl);
    this.rows = [];
    this.lastWidth = new Map();
    // Sizes come from a ResizeObserver, never read while drawing: reading
    // layout after writing rows would force a synchronous reflow.
    this.size = { w: 0, h: 0, sb: 0 };
  }

  // Take the surface's resizes and mouse (once, after `new`).
  listen() {
    new ResizeObserver(() => {
      this.size = { w: this.surfaceEl.clientWidth, h: this.surfaceEl.clientHeight, sb: this.scrollbarEl.clientHeight };
      this.scheduleRender();
    }).observe(this.surfaceEl);
    this.surfaceEl.addEventListener("mousedown", (e) => this.onMouseDown(e));
    this.surfaceEl.addEventListener("wheel", (e) => { e.preventDefault(); this.scrollLines(Math.sign(e.deltaY) * 3); }, { passive: false });
    this.surfaceEl.addEventListener("mousemove", (e) => this.onHover(e));
    this.scrollbarEl.addEventListener("mousedown", (e) => this.onScrollbar(e));
    this.gutterEl.addEventListener("mousedown", (e) => this.onGutter(e));
    this.surfaceEl.addEventListener("contextmenu", (e) => { e.preventDefault(); if (this.view) this.app.editorMenu(this, { x: e.clientX, y: e.clientY }); });
    // The wheel over the tabs scrolls them sideways.
    this.tabsEl.addEventListener("wheel", (e) => { if (!e.deltaX) this.tabsEl.scrollLeft += e.deltaY; }, { passive: true });
  }

  // ---------------------------------------------------------------- tabs

  show(buf) {
    let v = this.views.find((x) => x.buf === buf);
    if (!v) {
      v = new View(this, buf);
      buf.views.add(v);
      this.views.push(v);
    }
    this.view = v;
    this.app.onViewShown(this, v);
    this.renderTabs();
    this.scheduleRender();
    return v;
  }
  closeView(v) {
    const i = this.views.indexOf(v);
    if (i < 0) return;
    this.views.splice(i, 1);
    v.dispose();
    if (this.view === v) this.view = this.views[Math.max(0, i - 1)] || null;
    if (this.view) this.app.onViewShown(this, this.view);
    this.renderTabs();
    this.scheduleRender();
  }
  renderTabs() {
    this.tabsEl.innerHTML = "";
    const names = this.views.map((v) => v.buf.name);
    for (const [i, v] of this.views.entries()) {
      const t = document.createElement("div");
      t.className = "tab" + (v === this.view ? " active" : "");
      t.dataset.i = String(i);
      // Tabs of files with one name show their parent folder.
      const dup = names.filter((n) => n === v.buf.name).length > 1 && v.buf.path;
      const label = dup ? v.buf.path.split("/").slice(-2).join("/") : v.buf.name;
      t.textContent = (v.buf.modified ? "● " : "") + label + (v.buf.readOnly && !v.buf.isVirtual ? " [RO]" : "");
      t.title = v.buf.path || v.buf.name;
      // A press shows it (and may drag it: app.tabPress); the middle button
      // closes it; the right one opens its menu.
      t.onmousedown = (e) => { e.preventDefault(); if (e.button === 1) this.app.closeTab(this, v); else if (e.button === 0) this.app.tabPress(e, this, v); };
      t.oncontextmenu = (e) => { e.preventDefault(); this.show(v.buf); this.app.focusPane(this); this.app.tabMenu(this, v, { x: e.clientX, y: e.clientY }); };
      const x = document.createElement("span");
      x.className = "x";
      x.textContent = "×";
      x.onmousedown = (e) => { e.stopPropagation(); e.preventDefault(); this.app.closeTab(this, v); };
      t.appendChild(x);
      this.tabsEl.appendChild(t);
    }
  }

  // ---------------------------------------------------------------- geometry

  metrics() {
    const m = this.app.metrics;
    const w = (this.size.w || this.surfaceEl.clientWidth) - 12;
    const h = this.size.h || this.surfaceEl.clientHeight;
    return { cols: Math.max(10, Math.floor(w / m.cw)), rows: Math.max(1, Math.floor(h / m.lh)), cw: m.cw, lh: m.lh };
  }

  geometry(v) {
    const { cols, rows } = this.metrics();
    const L = v.buf.text;
    const compose = v.mode === "compose";
    const nums = v.opts.lineNumbers && !compose && L && L.firstLine != null;
    const digits = nums ? String(L.firstLine + L.count).length : 0;
    const gutter = (nums ? digits + 1 : 0) + 2;
    const avail = cols - gutter;
    const width = compose ? Math.min(v.opts.composeWidth || this.app.settings.page_width, avail) : avail;
    const margin = compose ? Math.max(0, Math.floor((avail - width) / 2)) : 0;
    return { cols, rows, gutter, digits, width, margin, nums };
  }

  scheduleRender() {
    if (this.pending) return;
    this.pending = requestAnimationFrame(() => { this.pending = null; this.render(); });
  }

  // ---------------------------------------------------------------- render

  layoutOpts(v, width) {
    const buf = v.buf;
    const L = buf.text;
    const lang = buf.language || (/\.(md|markdown)$/.test(buf.path || "") && "markdown");
    const sels = v.selections();
    const s = this.app.settings;
    // Fenced blocks in markdown are highlighted in the fence's language.
    let struct = null;
    if (lang === "markdown" && buf.full) {
      if (buf.structGen !== buf.textGen) { buf.struct = classify(L.lines); buf.structGen = buf.textGen; }
      struct = buf.struct;
    }
    const fenceLang = (li) => {
      let k = li;
      while (k > 0 && struct.region[k] !== "open") k--;
      return fenceLanguage((/^\s*(?:`{3,}|~{3,})\s*([\w+-]*)/.exec(L.lines[k]) || [])[1]);
    };
    const synFor = (li) => {
      const region = struct && struct.region[li];
      const l = region === "body" ? fenceLang(li) : lang;
      return region === "body" && !l ? NO_SPANS : spansFor(l, L.lines[li]);
    };
    // Each line's decorations, gathered when the layout first reaches the
    // line (the interval index makes that cost what the line has), in order
    // of precedence: overlays, occurrences, diagnostics, selection.
    let occ = null;
    if (s.highlight_occurrences) {
      const w = v.selection() || this.app.editing.wordAt(buf, v.primary.pos);
      const t = w ? buf.textRange(w.start, w.end) : null;
      if (t && t.length >= 2 && !t.includes("\n")) occ = t;
    }
    const occBg = { bg: THEME["editor.occurrence_bg"] };
    const selBg = THEME["editor.selection_bg"];
    const DIAG = ["", THEME["diagnostic.error"], THEME["diagnostic.warning"], THEME["diagnostic.info"], THEME["diagnostic.hint"]];
    const overlayFn = (o) => {
      const y = o.style || {};
      const fg = y.fg != null ? color(y.fg) : null, bg = y.bg != null ? color(y.bg) : null;
      return (st) => ({
        ...st, ...(fg ? { fg } : {}), ...(bg ? { bg } : {}),
        ...(y.bold ? { bold: true } : {}), ...(y.italic ? { italic: true } : {}), ...(y.underline ? { underline: true } : {}), ...(y.strike ? { strike: true } : {}), ...(y.mono ? { mono: true } : {}),
      });
    };
    const lineCache = new Map();
    const lineDecos = (li) => {
      let out = lineCache.get(li);
      if (out) return out;
      out = [];
      const ls = L.starts[li], le = L.lineEnd(li) + 1;
      for (const o of buf.decor.query("overlays", ls, le)) if (applies(o, v.mode)) out.push({ s: o.start, e: Math.max(o.end, o.start), f: overlayFn(o) });
      if (occ) {
        const line = L.lines[li];
        for (let k = line.indexOf(occ); k >= 0; k = line.indexOf(occ, k + occ.length)) {
          if ((k === 0 || !/\w/.test(line[k - 1])) && !/\w/.test(line[k + occ.length] || "")) {
            const st0 = ls + byteLen(line.slice(0, k));
            out.push({ s: st0, e: st0 + byteLen(occ), f: (st) => ({ ...st, ...occBg }) });
          }
        }
      }
      for (const g of buf.decor.query("diags", ls, le)) { const wavy = DIAG[g.severity] || DIAG[1]; out.push({ s: g.start, e: Math.max(g.end, g.start + 1), f: (st) => ({ ...st, wavy }) }); }
      for (const x of sels) if (x.start < le && x.end > ls) out.push({ s: x.start, e: x.end, f: (st) => ({ ...st, bg: selBg }) });
      lineCache.set(li, out);
      return out;
    };
    // Decorations over style `st` (the syntax style) at byte b of line li.
    const decoStyle = (b, li, st) => {
      for (const x of lineDecos(li)) if (b >= x.s && x.e > b) st = x.f(st);
      return st;
    };
    return {
      mode: v.mode,
      wrap: v.opts.wrap,
      width,
      tab: 4,
      cursors: v.cursors.map((c) => c.pos),
      folds: v.folds,
      whitespace: s.show_whitespace,
      extendAt: (b) => {
        for (const o of buf.decor.query("overlays", b, b + 1)) if (o.style?.extendToLineEnd && o.style.bg) return color(o.style.bg);
        return null;
      },
      decoStyle,
      styleAt: (b, li, idx) => {
        if (li == null) { li = L.indexOf(b); idx = li >= 0 ? byteToIdx(L.lines[li], b - L.starts[li]) : 0; }
        return li >= 0 ? decoStyle(b, li, synAt(synFor(li), idx)) : EMPTY;
      },
    };
  }

  computeRows(v, extra = 1) {
    const L = v.buf.text;
    const g = this.geometry(v);
    const r = layout(v.buf, Math.max(0, L.indexOf(v.top)), g.rows + extra, this.layoutOpts(v, g.width));
    return { ...r, g };
  }

  // Window line indices [first, last] on screen.
  visibleLineRange(v) {
    if (!v.buf.text || this.view !== v) return null;
    if (v.mode === "compose") return this.doc.visibleLines(v);
    const { consumed } = this.computeRows(v, 0);
    return consumed.length ? { first: consumed[0], last: consumed[consumed.length - 1] } : null;
  }

  ready(v) {
    const L = v.buf.text;
    return !!L && L.indexOf(v.top) >= 0 && (v.buf.full || L.eof || L.count - L.indexOf(v.top) > this.metrics().rows + 2);
  }

  // Clear the drawn rows (a pane with nothing to show, or still loading).
  clearRows() {
    this.textEl.textContent = "";
    this.gutterEl.textContent = "";
    this.caretsEl.textContent = "";
    this.rowEls = [];
    this.gutEls = [];
    this.rows = [];
    this.doc.clear();
  }

  // Draw now, not at the next frame (motion that measures what is drawn).
  renderNow() {
    if (this.pending) { cancelAnimationFrame(this.pending); this.pending = null; }
    this.render();
  }

  render() {
    const v = this.view;
    if (!v) { this.clearRows(); return; }
    const buf = v.buf;
    if (!this.ready(v)) {
      buf.ensure(v.top, this.metrics().rows + 50).then(() => {
        const L2 = buf.text;
        if (L2 && L2.indexOf(v.top) < 0) v.setTop(L2.starts[0]);
        if (this.ready(v) || (L2 && L2.eof)) this.render();
      });
      if (!buf.text || buf.text.indexOf(v.top) < 0) { this.clearRows(); this.textEl.textContent = "Loading…"; return; }
    }
    const L = buf.text;
    v.setTop(L.starts[L.indexOf(v.top)], v.topPx);
    const compose = v.mode === "compose";
    if (this.surfaceEl.classList.contains("compose") !== compose) { this.clearRows(); this.surfaceEl.classList.toggle("compose", compose); }
    if (compose) { this.renderDoc(v); return; }
    const { rows, g } = this.computeRows(v, 0);
    this.rows = rows;
    this.geom = g;
    const { cw, lh } = this.metrics();
    const s = this.app.settings;
    this.gutterEl.style.width = `${g.gutter * cw}px`;
    const left = (g.gutter + g.margin) * cw;
    this.textEl.style.left = this.caretsEl.style.left = this.rulersEl.style.left = `${left}px`;
    // Gutter marks and the worst diagnostic, by line.
    const marks = new Map();
    for (const m of buf.decor.gutters) { const i = L.indexOf(m.start); if (i >= 0) marks.set(i, m); }
    const dline = new Map();
    for (const d of buf.decor.diags) { const i = L.indexOf(d.start); if (i >= 0 && !(dline.get(i) <= d.severity)) dline.set(i, d.severity); }
    const curLines = new Set(v.cursors.map((c) => L.indexOf(c.pos)));
    const folded = new Set(v.folds.map((f) => f.start));
    // Rows are drawn from HTML strings (as Monaco draws its view lines) into
    // a pool of row elements; a row's DOM is written only when its HTML
    // changed, so a keystroke touches the rows it changed and no others.
    // Rows are drawn from HTML strings (as Monaco draws its view lines) and
    // kept by content: a row whose HTML is already on screen is reused, just
    // moved (scrolling moves every row but rewrites only the new ones), and
    // the children are swapped in with one replaceChildren.
    if (!this.rowEls || this.textEl.firstChild?.nodeType === 3) this.clearRows();
    const reuse = (pool, html) => {
      const list = pool.get(html);
      if (list && list.length) return list.pop();
      const t = document.createElement("template");
      t.innerHTML = html;
      const el = t.content.firstChild;
      el._html = html;
      return el;
    };
    const textPool = new Map(), gutPool = new Map();
    for (const el of this.rowEls) (textPool.get(el._html) || textPool.set(el._html, []).get(el._html)).push(el);
    for (const el of this.gutEls) (gutPool.get(el._html) || gutPool.set(el._html, []).get(el._html)).push(el);
    const textEls = [], gutEls = [];
    let lastLine = -1;
    for (const r of rows) {
      const current = s.highlight_current_line && r.kind === "text" && curLines.has(r.lineIdx) && !v.selection();
      const cls = "row" + (r.kind === "virtual" ? " virtual" : "") + (current && !r.fill ? " current" : "");
      let html = `<div class="${cls}"${r.fill ? ` style="background:${r.fill}"` : ""}>`;
      let run = "", key = null, cssText = "";
      for (const c of r.cells) {
        const k2 = c.style === EMPTY ? "" : styleKey(c.style);
        if (k2 !== key) {
          if (key !== null) html += cssText ? `<span style="${cssText}">${esc(run)}</span>` : esc(run);
          key = k2; cssText = k2 ? cssFor(c.style, k2) : ""; run = "";
        }
        run += c.ch;
      }
      if (key !== null) html += cssText ? `<span style="${cssText}">${esc(run)}</span>` : esc(run);
      if (!r.cells.length) html += "\u200b";
      html += "</div>";
      textEls.push(reuse(textPool, html));
      // The gutter: line number and one mark (fold, diagnostic or plugin).
      let ghtml;
      if (r.kind === "text" && !r.cont && r.lineIdx !== lastLine) {
        lastLine = r.lineIdx;
        const m = marks.get(r.lineIdx);
        const sev = dline.get(r.lineIdx);
        const mark = folded.has(r.lineIdx) ? "▸" : sev ? (sev === 1 ? "●" : "▲") : m ? m.glyph : " ";
        const mcol = sev ? THEME[sev === 1 ? "diagnostic.error" : "diagnostic.warning"] : m && m.color ? color(m.color) : "";
        const num = g.nums ? String(L.firstLine + r.lineIdx + 1).padStart(g.digits) + " " : "";
        ghtml = `<div class="row${current ? " current" : ""}" data-line="${r.lineIdx}"${m && !sev && m.title ? ` title="${esc(m.title)}"` : ""}>${num}<span class="mark"${mcol ? ` style="color:${mcol}"` : ""}>${esc(mark)}</span></div>`;
      } else ghtml = `<div class="row">\u200b</div>`;
      gutEls.push(reuse(gutPool, ghtml));
    }
    const same = (a, b) => a.length === b.length && a.every((x, i) => x === b[i]);
    if (!same(textEls, this.rowEls)) this.textEl.replaceChildren(...textEls);
    if (!same(gutEls, this.gutEls)) this.gutterEl.replaceChildren(...gutEls);
    this.rowEls = textEls;
    this.gutEls = gutEls;
    this.rulersEl.textContent = "";
    for (const col of s.rulers) {
      const r = document.createElement("div");
      r.className = "ruler";
      r.style.left = `${(col - 1) * cw}px`;
      this.rulersEl.appendChild(r);
    }
    // Carets: absolutely placed, so moving one never touches the rows.
    let html = "";
    if (this.app.activePane === this) {
      for (const c of v.cursors) {
        const at = this.caretXY(c.pos);
        if (at) html += `<div class="caret" style="left:${at.x * cw}px;top:${at.r * lh}px;height:${lh}px"></div>`;
      }
    }
    if (this.caretsEl._html !== html) { this.caretsEl.innerHTML = html; this.caretsEl._html = html; }
    this.finishRender(v, g);
  }

  // A view in compose mode: a document (doc.js).
  renderDoc(v) {
    const g = this.geometry(v);
    this.geom = g;
    this.doc.render(v);
    this.finishRender(v, g);
  }

  finishRender(v, g) {
    this.renderScrollbar(v, g);
    if (this.lastWidth.get(v) !== g.width) {
      this.lastWidth.set(v, g.width);
      this.app.host.emit("view.resized", { view: v });
    }
    this.app.updateStatus();
  }

  renderScrollbar(v, g) {
    const L = v.buf.text;
    const buf = v.buf;
    let frac, size;
    if (L.firstLine != null && buf.totalLines) {
      frac = (L.firstLine + L.indexOf(v.top)) / Math.max(1, buf.totalLines);
      size = Math.min(1, g.rows / Math.max(1, buf.totalLines));
    } else {
      frac = v.top / Math.max(1, buf.len);
      size = 0.02;
    }
    const h = this.size.sb;
    this.thumbEl.style.top = `${frac * h}px`;
    this.thumbEl.style.height = `${Math.max(16, size * h)}px`;
  }

  // Where a caret at byte `p` is drawn, in client pixels: {x, y, h}, or
  // null when it is not on screen.
  caretBox(p) {
    if (this.view?.mode === "compose") {
      const r = this.doc.caretRect(p);
      return r ? { x: r.left, y: r.top, h: r.height || this.metrics().lh } : null;
    }
    const at = this.caretXY(p);
    if (!at) return null;
    const { cw, lh } = this.metrics();
    const rect = this.textEl.getBoundingClientRect();
    return { x: rect.left + at.x * cw, y: rect.top + at.r * lh, h: lh };
  }

  // The byte drawn at client point (x, y).
  posFromPoint(x, y) {
    const v = this.view;
    if (v && v.mode === "compose") return this.doc.posFromPoint(v, x, y);
    const { cw, lh } = this.metrics();
    const rect = this.textEl.getBoundingClientRect();
    return this.posAt(Math.floor((y - rect.top) / lh), Math.max(0, (x - rect.left) / cw));
  }

  // The fold hiding line `li`, with the last line it keeps: {fold, keep}.
  foldHiding(v, li) {
    if (v.mode === "compose") return this.doc.hiding(v, li);
    const f = v.folds.find((f) => li > f.start && li <= f.end);
    return f ? { fold: f, keep: f.start } : null;
  }

  // Where a caret can stand at or beyond `p` (in direction `dir`): not on
  // a line a fold hides, nor inside text a conceal hides (one revealed by a
  // caret at `p` shows).
  settle(v, p, dir) {
    const L = v.buf.text;
    const h = L && this.foldHiding(v, L.indexOf(p));
    if (h) p = dir < 0 || h.fold.end + 1 >= L.count ? L.lineEnd(h.keep) : L.starts[h.fold.end + 1];
    for (let guard = 0; guard < 64; guard++) {
      const c = v.buf.decor.query("conceals", p, p + 1).find((c) => c.start < p && p < c.end && applies(c, v.mode) && !(c.reveal && cursorIn([p], c.reveal)));
      if (!c) return p;
      p = dir < 0 ? c.start : c.end;
    }
    return p;
  }

  // Where byte `p` is drawn: {r, x} in rows and columns.
  caretXY(p) {
    let best = null;
    for (let r = 0; r < this.rows.length; r++) {
      const row = this.rows[r];
      if (row.kind !== "text") continue;
      let x = 0;
      for (const c of row.cells) {
        if (c.src === p) return { r, x };
        if (c.src != null && c.src < p) best = { r, x: x + c.w };
        x += c.w;
      }
      if (row.eol === p) return { r, x: row.col };
      if (row.eol != null && row.eol > p && best && best.r === r) return best;
    }
    return null;
  }

  // The byte under row `r`, column `x`.
  posAt(r, x) {
    r = Math.max(0, Math.min(this.rows.length - 1, r));
    let row = this.rows[r];
    while (row && row.kind !== "text" && r < this.rows.length - 1) row = this.rows[++r];
    if (!row || row.kind !== "text") return null;
    let col = 0, last = null;
    for (const c of row.cells) {
      if (c.src != null) {
        if (col + c.w / 2 > x) return c.src;
        last = c.src;
      }
      col += c.w;
    }
    if (row.eol != null) return row.eol;
    const next = this.rows[r + 1];
    const first = next && next.cells.find((c) => c.src != null);
    return first ? first.src : last;
  }

  // ---------------------------------------------------------------- motion

  scrollLines(n) {
    const v = this.view;
    if (!v || !v.buf.text) return;
    if (v.mode === "compose") { v.setTop(v.top, v.topPx + n * this.metrics().lh); this.scheduleRender(); return; }
    const L = v.buf.text;
    const i = L.indexOf(v.top);
    if (i < 0) return;
    let j = Math.max(0, Math.min(L.count - 1, i + n));
    for (const f of v.folds) if (j > f.start && j <= f.end) j = n > 0 ? Math.min(L.count - 1, f.end + 1) : f.start;
    if (j === i && n < 0 && !L.bof) {
      v.buf.ensure(Math.max(0, L.start - 1)).then(() => this.scrollLines(n));
      return;
    }
    v.setTop(L.starts[j]);
    if (!v.buf.full && ((!L.eof && L.count - j < 400) || (!L.bof && j < 50))) v.buf.ensure(v.top, 400);
    this.scheduleRender();
  }

  // Keep the primary cursor on screen; `center` puts it mid-screen.
  reveal(center = false) {
    const v = this.view;
    if (!v || !v.buf.text) return;
    // A document keeps the cursor in view as it draws (it measures there).
    if (v.mode === "compose") { this.doc.want = center ? "center" : this.doc.want || "keep"; this.scheduleRender(); return; }
    const L = v.buf.text;
    const ci = L.indexOf(v.primary.pos);
    if (ci < 0) return;
    if (center) { v.setTop(L.starts[Math.max(0, ci - Math.floor(this.metrics().rows / 2))]); return; }
    if (ci < L.indexOf(v.top)) { v.setTop(L.starts[ci]); return; }
    for (let guard = 0; guard < 2000; guard++) {
      const { rows, g } = this.computeRows(v, 0);
      this.rows = rows;
      const at = this.caretXY(v.primary.pos);
      if (at && at.r < g.rows) return;
      const ti = L.indexOf(v.top);
      if (ti >= ci || ti >= L.count - 1) return;
      v.setTop(L.starts[ti + 1]);
    }
  }

  // Move the primary cursor by visual rows.
  moveRows(n, extend) {
    const v = this.view;
    if (!v) return;
    if (v.mode === "compose") {
      const c = v.primary;
      this.renderNow();
      const goal = c.goalX ?? this.doc.caretX(c.pos);
      if (goal == null) return;
      let p = c.pos;
      for (let k = 0; k < Math.abs(n); k++) {
        const q = this.doc.lineFrom(v, p, Math.sign(n), goal);
        if (q == null) break;
        p = q;
      }
      this.app.editing.place(this, p, extend);
      c.goalX = goal;
      return;
    }
    const L = v.buf.text;
    const c = v.primary;
    this.reveal();
    let { rows } = this.computeRows(v, Math.abs(n) + 2);
    this.rows = rows;
    let at = this.caretXY(c.pos);
    if (!at) return;
    const goal = c.goalX ?? at.x;
    let r = at.r, left = Math.abs(n);
    while (left > 0) {
      let t = r + Math.sign(n);
      while (t >= 0 && t < rows.length && rows[t].kind !== "text") t += Math.sign(n);
      if (t < 0) {
        const ti = L.indexOf(v.top);
        if (ti === 0) break;
        v.setTop(L.starts[ti - 1]);
        ({ rows } = this.computeRows(v, Math.abs(n) + 2));
        this.rows = rows;
        at = this.caretXY(c.pos);
        r = at ? at.r : 0;
        continue;
      }
      if (t >= rows.length) break;
      r = t;
      left--;
    }
    const p = this.posAt(r, goal);
    if (p != null) { this.app.editing.place(this, p, extend); c.goalX = goal; }
  }

  // A page up or down: the view scrolls a screen and the cursor keeps its
  // place on it.
  page(dir, extend) {
    const v = this.view;
    if (!v) return;
    if (v.mode !== "compose") {
      const n = this.metrics().rows;
      this.scrollLines(dir * n);
      this.moveRows(dir * n, extend);
      return;
    }
    const c = v.primary;
    this.renderNow();
    const at = this.caretBox(c.pos);
    const sr = this.surfaceEl.getBoundingClientRect();
    const dist = dir * Math.max(this.metrics().lh, sr.height - 2 * this.metrics().lh);
    const goal = c.goalX ?? this.doc.caretX(c.pos) ?? 0;
    // The caret moves a page through the document; the view moved `shift`.
    const shift = this.doc.scrollBy(v, dist);
    this.finishRender(v, this.geometry(v));
    const y = (at ? at.y + at.h / 2 : sr.top + sr.height / 2) + dist - shift;
    const p = this.posFromPoint(this.doc.content.getBoundingClientRect().left + goal, y);
    if (p != null) { this.app.editing.place(this, p, extend); c.goalX = goal; }
  }

  // ---------------------------------------------------------------- mouse

  onMouseDown(e) {
    if (e.target === this.scrollbarEl || e.target === this.thumbEl || this.gutterEl.contains(e.target)) return;
    e.preventDefault();
    this.app.focusPane(this);
    const v = this.view;
    if (!v) return;
    // A document's fold toggle.
    const fold = e.target.closest?.("[data-fold]");
    if (fold) { this.app.editing.toggleFoldAt(this, +fold.dataset.fold); return; }
    const toPos = (ev) => this.posFromPoint(ev.clientX, ev.clientY);
    const p = toPos(e);
    if (p == null) return;
    // A right press moves the caret there unless it is in the selection
    // (the context menu acts on it); the menu opens on `contextmenu`.
    if (e.button === 2) {
      if (!v.selections().some((s) => p >= s.start && p <= s.end)) this.app.editing.place(this, p, false);
      return;
    }
    if (e.button !== 0) return;
    if (e.altKey && e.button === 0) { v.cursors.push({ pos: p, anchor: null, goalX: null }); v.normalize(); this.scheduleRender(); return; }
    this.app.editing.place(this, p, e.shiftKey);
    if (e.ctrlKey && e.button === 0) { this.app.run("lsp.definition"); return; }
    if (e.detail === 2) this.app.editing.selectWord(this);
    if (e.detail === 3) this.app.editing.selectLine(this);
    const extend = (ev) => { const q = toPos(ev); if (q != null) this.app.editing.place(this, q, true); };
    // Held past the top or bottom edge, the view scrolls each frame (faster
    // the farther out) and the selection follows to the edge.
    let last = null, frame = null;
    const tick = () => {
      frame = null;
      const sr = this.surfaceEl.getBoundingClientRect();
      const d = last.clientY < sr.top ? last.clientY - sr.top : last.clientY > sr.bottom ? last.clientY - sr.bottom : 0;
      if (!d) return;
      this.scrollLines(Math.sign(d) * Math.min(10, 1 + Math.floor(Math.abs(d) / this.metrics().lh)));
      this.renderNow();
      extend(last);
      frame = requestAnimationFrame(tick);
    };
    const move = (ev) => { last = ev; extend(ev); if (!frame) frame = requestAnimationFrame(tick); };
    const up = () => { cancelAnimationFrame(frame); window.removeEventListener("mousemove", move); window.removeEventListener("mouseup", up); };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
  }

  onGutter(e) {
    e.preventDefault();
    this.app.focusPane(this);
    const row = e.target.closest(".row");
    if (row?.dataset.line != null) this.app.editing.toggleFoldAt(this, +row.dataset.line);
  }

  onHover(e) {
    const v = this.view;
    if (!v || !v.buf.text) return;
    this.app.onHover(this, this.posFromPoint(e.clientX, e.clientY), e.clientX, e.clientY);
  }

  onScrollbar(e) {
    e.preventDefault();
    const go = (ev) => {
      const r = this.scrollbarEl.getBoundingClientRect();
      this.app.scrollToFraction(this, Math.max(0, Math.min(1, (ev.clientY - r.top) / r.height)));
    };
    go(e);
    const up = () => { window.removeEventListener("mousemove", go); window.removeEventListener("mouseup", up); };
    window.addEventListener("mousemove", go);
    window.addEventListener("mouseup", up);
  }
}
