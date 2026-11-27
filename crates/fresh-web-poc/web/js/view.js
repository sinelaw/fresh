// A pane and the views in it: one view per buffer tab, each with its own
// scroll, cursors, options and folds — all of it this client's.

import { listOf, found, some } from "./maybe.js";

import { setClass, escapeHtml } from "./dom.js";
import { layoutOf, measuring, layoutKey, rowsFrom, blockRows, blockOf, ignoreMark, applies, cursorIn, columnIn, byteAtColumn, rowEndOf } from "./layout.js";
import { RowIndex, rowStart, settledTop, rowsDown, rowsUp, columnStart, columnOf, fractionOf, topAtFraction } from "./rows.js";
import { byteLen } from "./utf8.js";
import { clustersAround } from "./clusters.js";
import { shiftPos } from "./ot.js";
import { ENSURE_AFTER, terminalOf, filePath } from "./buffer.js";
import { color, THEME } from "./highlight.js";
import { OPEN, balanceOf } from "./brackets.js";
import { EMPTY, synAt, cssFor, styleKey, restyler, withWavy } from "./paint.js";
import { DocView, hiding } from "./doc.js";
import { poolOf, take, sameDrawn } from "./pool.js";


let nextPaneId = 1;

// What a view draws over the text's own style: a bracket in the colour of
// its depth, the occurrences of the word at the cursor, the bracket pair at
// the cursor, the selection.
const RAINBOW = [1, 2, 3, 4, 5, 6].map((n) => restyler({ fg: `editor.bracket_rainbow_${n}`, bg: null, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false }));
const OCCURRENCE = restyler({ fg: null, bg: "editor.occurrence_bg", bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false });
const BRACKET = restyler({ fg: "editor.bracket_match_fg", bg: null, bold: true, italic: false, underline: true, strike: false, mono: false, extendToLineEnd: false });
const SELECTED = restyler({ fg: null, bg: "editor.selection_bg", bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false });
// Each buffer's bracket depth at every line start, for what its syntax
// says (`syn`: its text and colours as of `syn.gen`).
const depthCache = new WeakMap();
function rainbowDepths(buf, syn) {
  const hit = depthCache.get(buf);
  if (hit !== undefined && hit.text === buf.textGen && hit.colours === syn.gen) return hit.depths;
  const L = buf.held;
  const depths = new Int32Array(L.count + 1);
  for (let i = 0; i < L.count; i++) depths[i + 1] = depths[i] + balanceOf(syn.brackets(i));
  depthCache.set(buf, { text: buf.textGen, colours: syn.gen, depths });
  return depths;
}

// `make()` for `key`, remembered while `key` is the last one asked.
/** function remember<K, V>(memo: Map<K, V>, key: K, make: () => V) => V */
function remember(memo, key, make) {
  if (memo.has(key)) {
    const hit = memo.get(key);
    if (hit !== undefined) return hit;
  }
  const v = make();
  memo.clear();
  memo.set(key, v);
  return v;
}

// A scrollbar mark: its rank (lower wins a row) and colour.
/** type Mark = {rank: Number, color: String} */
// A view's folds: none at first (each its first and last line, as window
// line indices).
/** function noFoldSpans() => Range[] */
function noFoldSpans() { return []; }
/** function noMarks() => Map<Number, Mark> */
function noMarks() { return new Map(); }
// A diagnostic's theme key by severity (1 error … 4 hint).
// The theme key of a diagnostic's severity (an unknown one: an error's).
/** function diagKey(severity: Number) => String */
function diagKey(severity) { return ["diagnostic.error", "diagnostic.warning", "diagnostic.info", "diagnostic.hint"].at(severity - 1) ?? "diagnostic.error"; }

// A row's HTML: its cells in runs of one style, each run a span (a run
// of no style, key "", plain text); `fill`: the background to its edge;
// `col0`: the column its first cell is at. A cell of anything but one
// printable ASCII character is drawn in a box exactly its columns wide, so
// a fallback font that draws it wider cannot move the columns after it.
/** type Run = {key: String, style: Drawn, html: String} */
/** function rowHtml(cls: String, fill: String | Null, cells: Cell[], col0: Number) => String */
function rowHtml(cls, fill, cells, col0) {
  /** const runs: Run[] */
  const runs = [];
  for (const c of cells) {
    const key = c.style === EMPTY ? "" : styleKey(c.style);
    const html = PRINTABLE.test(c.ch) ? escapeHtml(c.ch) : `<span class="cl w${Math.min(c.w, 2)}">${escapeHtml(c.ch)}</span>`;
    const last = runs.at(-1);
    if (last !== undefined && last.key === key) last.html += html;
    else runs.push({ key, style: c.style, html });
  }
  const spans = runs.map((x) => (x.key !== "" ? `<span style="${cssFor(x.style, x.key)}">${x.html}</span>` : x.html)).join("");
  const css = [fill === null ? "" : `background:${fill}`, col0 > 0 ? `margin-left:calc(${col0} * var(--cw))` : ""].filter((x) => x !== "").join(";");
  return `<div class="${cls}"${css === "" ? "" : ` style="${css}"`}>${cells.length > 0 ? spans : "\u200b"}</div>`;
}
// One printable ASCII character: a cell the editor's font draws one
// column wide.
const PRINTABLE = /^[\x20-\x7e]$/;

// No screen place.
/** function noXY() => XY[] */
function noXY() { return []; }
/** function noByte() => Number | Null */
function noByte() { return null; }
/** function noCorner() => Corner[] */
function noCorner() { return []; }
/** function noFrame() => Number[] */
function noFrame() { return []; }
/** function noBytes() => Number[] */
function noBytes() { return []; }

// Presses in a row (a double or triple click): the last one's time and
// place, and how many came each close after the one before.
/** function nextClicks(last: Clicks, t: Number, x: Number, y: Number) => Clicks */
function nextClicks(last, t, x, y) {
  const close = t - last.t < 500 && Math.abs(x - last.x) < 4 && Math.abs(y - last.y) < 4;
  return { t, x, y, n: close ? last.n + 1 : 1 };
}


// A line's decorations are gathered this many UTF-16 units at a time.
const DECO_UNITS = 4096;
// Text held past a view's top, in bytes, enough for a screen of a huge
// line (fewer: the window is fetched again around the top).
const READY_BYTES = 64 * 1024;

// The scrollbar's width, in px (style.css `.scrollbar`): text stops short
// of it.
const SCROLLBAR_WIDTH = 12;

// No draw asked for yet (a pane's `pending` frame).
/** const NO_FRAME: Number | Null */
const NO_FRAME = null;

// A <div> of class `cls` holding `kids`.
function div(cls, kids) {
  const d = document.createElement("div");
  d.className = cls;
  for (const k of kids) d.appendChild(k);
  return d;
}

/** function cursorWith(pos: Number, anchor: Number | Null, goalX: Number | Null) => Cursor */
export function cursorWith(pos, anchor, goalX) { return { pos, anchor, goalX }; }
// `c` moved by an edit: `move(p, after)` is where position `p` goes
// (`after`: a position at the edit's place goes after its insertion).
/** function shiftCursor(c: Cursor, move: (Number, Boolean) => Number) => Cursor */
export function shiftCursor(c, move) {
  const a = c.anchor;
  return cursorWith(move(c.pos, true), a === null ? null : move(a, false), c.goalX);
}
/** function cursorAt(pos: Number) => Cursor */
export function cursorAt(pos) { return { pos, anchor: null, goalX: null }; }
// A cursor at `pos` selecting from `anchor` (none when they meet).
/** function selecting(pos: Number, anchor: Number | Null) => Cursor */
export function selecting(pos, anchor) { return { pos, anchor: anchor === pos ? null : anchor, goalX: null }; }

// A cursor's selection, or null for none.
/** function sel(c: Cursor) => {start: Number, end: Number} | Null */
export function sel(c) {
  const a = c.anchor;
  return a === null || a === c.pos ? null : { start: Math.min(a, c.pos), end: Math.max(a, c.pos) };
}


// Copies of a view's cursors, options and folds (for another split's view).
/** function copyCursors(cs: Cursor[]) => Cursor[] */
function copyCursors(cs) { return cs.map((c) => ({ pos: c.pos, anchor: c.anchor, goalX: c.goalX })); }
/** function copyOptions(o: ViewOptions) => ViewOptions */
function copyOptions(o) { return { composeWidth: o.composeWidth, wrap: o.wrap, lineNumbers: o.lineNumbers }; }
/** function copyFolds(fs: Range[]) => Range[] */
function copyFolds(fs) { return fs.map((f) => ({ start: f.start, end: f.end })); }

// A view's options: its compose width (none: the setting's), wrapping,
// and line numbers.
/** function viewOptions(lineNumbers: Boolean) => ViewOptions */
function viewOptions(lineNumbers) { return { composeWidth: null, wrap: false, lineNumbers }; }

// One buffer as shown in one pane.
export class View {
  constructor(pane, buf) {
    this.pane = pane;
    this.buf = buf;
    // The scroll position: the byte where the top row starts (any byte of
    // a line: a row of a wrapped line starts inside it), and (in a compose
    // view, whose top is a line's start) how many pixels of that line are
    // scrolled past.
    this._top = 0;
    this.topPx = 0;
    // The byte the view was scrolled to: the top row is the one holding it
    // (at another width, another row; a resize keeps it at the top).
    this.scrolledTo = 0;
    // Where the rows of its long lines start, at its width (rows.js).
    this.index = new RowIndex();
    // Every cursor: position, selection anchor (or null) and the column
    // vertical motion aims for. The first is the primary.
    this.cursors = [cursorAt(0)];
    this.mode = "source"; // or "compose"
    this.opts = viewOptions(!buf.isVirtual);
    this.folds = noFoldSpans(); // window line indices; start's line stays
    this.left = 0; // columns scrolled sideways (no wrap)
    this.preview = false; // the split's preview tab: the next preview replaces it
    // The selections Expand Selection grew from (Shrink goes back), while
    // the cursors are as it left them (`key`): none at first.
    this.expansions = { stack: [], key: "" };
    // The rectangle the cursors are (Alt+Shift+arrows grow it), while they
    // are as it left them: a list of at most one.
    this.block = [];
  }
  get primary() { return this.cursors[0]; }
  get top() { return this._top; }
  // Show the row starting at `b` at the top (in a compose view, the line
  // starting there, `px` pixels into it).
  setTop(b, px) { this._top = b; this.scrolledTo = b; this.topPx = px; }
  // The top row found to start at `b` (the view still scrolled to the
  // byte it was).
  rowTop(b) { this._top = b; }
  // The primary cursor alone (the others dropped).
  keepPrimary() { this.cursors = [this.primary]; }
  // A preview tab made a tab of its own: whether it was a preview.
  keepOpen() {
    const was = this.preview;
    this.preview = false;
    return was;
  }
  shift(op) {
    const sp = (p, r) => shiftPos(p, op, r);
    this.cursors = this.cursors.map((c) => shiftCursor(c, sp));
    this._top = sp(this._top, false);
    this.scrolledTo = sp(this.scrolledTo, false);
  }
  selection() { return sel(this.cursors[0]); }
  selections() { return this.cursors.flatMap((c) => listOf(sel(c))); }
  // Sort cursors by position (primary kept first) and merge any that met.
  normalize() {
    const prim = this.cursors[0];
    const lo = (x) => Math.min(x.pos, x.anchor ?? x.pos), hi = (x) => Math.max(x.pos, x.anchor ?? x.pos);
    const out = [];
    for (const c of [...this.cursors].sort((a, b) => lo(a) - lo(b))) {
      const last = out.length - 1;
      if (last >= 0 && (out[last].pos === c.pos || lo(c) < hi(out[last]))) { if (c === prim) out[last] = c; continue; }
      out.push(c);
    }
    const i = out.indexOf(prim);
    if (i > 0) { out.splice(i, 1); out.unshift(prim); }
    this.cursors = out.length ? out : [prim];
  }
}

// A tab of split `to` at `index` that is a copy of view `v` (of another
// split): its cursors, scroll, mode, options and folds.
export function copyTab(v, to, index) {
  const nv = new View(to, v.buf);
  to.track(nv);
  nv.cursors = copyCursors(v.cursors);
  nv.setTop(v.top, v.topPx);
  nv.mode = v.mode;
  nv.opts = copyOptions(v.opts);
  nv.folds = copyFolds(v.folds);
  to.views.splice(Math.min(index, to.views.length), 0, nv);
}

export class Pane {
  // `editor` (main.js): what a pane asks of the editor it is in — focus,
  // the settings and cell size, the editing a click or drag does, and the
  // tab, menu, hover and scroll events it passes on.
  constructor(editor) {
    this.editor = editor;
    this.id = nextPaneId++;
    this.views = []; // tabs
    this.shown = []; // the view shown: one of `views`, or none
    this.unfollow = new Map(); // each view's unfollow of its buffer's edits
    // The tabs over the surface: rulers, gutter, text and carets, then (in
    // a compose view) the document, then the scrollbar.
    this.tabsEl = div("tabs", []);
    this.rulersEl = div("rulers", []);
    this.gutterEl = div("gutter", []);
    this.textEl = div("text", []);
    this.caretsEl = div("carets", []);
    this.marksEl = div("marks", []);
    this.thumbEl = div("thumb", []);
    this.scrollbarEl = div("scrollbar", [this.marksEl, this.thumbEl]);
    // A view in compose mode is drawn as a document instead of rows.
    this.doc = new DocView();
    this.surfaceEl = div("surface", [this.rulersEl, this.gutterEl, this.textEl, this.caretsEl, this.doc.el, this.scrollbarEl]);
    // A terminal's tab shows the terminal's screen here instead.
    this.termEl = div("term-box", []);
    this.el = div("pane", [this.tabsEl, this.surfaceEl, this.termEl]);
    this.marksSig = "";
    this.rows = [];
    // The rows and gutter rows on screen (pool.js), and the rulers' and
    // carets' HTML.
    this.rowsDrawn = [];
    this.gutterDrawn = [];
    this.drawn = { rulers: "", carets: "", cw: 0 };
    this.lastWidth = new Map();
    // Sizes come from a ResizeObserver, never read while drawing: reading
    // layout after writing rows would force a synchronous reflow.
    this.size = { w: 0, h: 0, sb: 0 };
    this.wantX = false; // keep the cursor in view sideways at the next draw
    // The bracket pair at the cursor, by the cursor position and text it
    // was found for (the last one only).
    this.brackets = new Map();
    this.maxCols = 0; // the widest row drawn, in columns
    // The edits to the shown buffer since the rows were drawn: what a click
    // on them is mapped through (rows drawn before an edit name the bytes
    // as they were).
    /** this.sinceDrawn: Op[] */
    this.sinceDrawn = [];
    // A slice of row index work asked for (none: none).
    /** this.indexing: Number[] */
    this.indexing = [];
    this.frame = { id: NO_FRAME }; // the frame the next draw is asked for
    this.clicks = { t: -Infinity, x: 0, y: 0, n: 0 }; // presses in a row
  }

  // The view shown, if any.
  // The view shown, where the caller knows there is one (the commands on
  // a view run only then): it throws when the split shows none.
  get view() {
    for (const v of this.shown) return v;
    throw new Error("the split shows no view");
  }

  // Take the surface's resizes and mouse (once, after `new`).
  // Whether the editor's keys go to this pane.
  get active() { return this.editor.isActive(this); }

  listen() {
    new ResizeObserver(() => {
      this.size = { w: this.surfaceEl.clientWidth, h: this.surfaceEl.clientHeight, sb: this.scrollbarEl.clientHeight };
      this.scheduleRender();
    }).observe(this.surfaceEl);
    this.surfaceEl.addEventListener("mousedown", (e) => this.onMouseDown(e));
    // The middle button drags a block, and does not paste (as X does).
    this.surfaceEl.addEventListener("mouseup", (e) => { if (e.button === 1) e.preventDefault(); });
    this.surfaceEl.addEventListener("auxclick", (e) => { if (e.button === 1) e.preventDefault(); });
    // The wheel scrolls lines; sideways (a tilt, or with Shift) it scrolls
    // columns.
    this.surfaceEl.addEventListener("wheel", (e) => {
      e.preventDefault();
      // The split scrolled leads the ones that scroll with it.
      this.editor.drive(this);
      const dx = e.shiftKey ? e.deltaY || e.deltaX : Math.abs(e.deltaX) > Math.abs(e.deltaY) ? e.deltaX : 0;
      if (dx) this.scrollCols(Math.sign(dx) * 6);
      else this.scrollRows(Math.sign(e.deltaY) * 3);
    }, { passive: false });
    this.surfaceEl.addEventListener("mousemove", (e) => this.onHover(e));
    this.scrollbarEl.addEventListener("mousedown", (e) => this.onScrollbar(e));
    this.gutterEl.addEventListener("mousedown", (e) => this.onGutter(e));
    this.surfaceEl.addEventListener("contextmenu", (e) => { e.preventDefault(); if (this.shown.length > 0) this.editor.editorMenu(this, { x: e.clientX, y: e.clientY }); });
    // The wheel over the tabs scrolls them sideways.
    this.tabsEl.addEventListener("wheel", (e) => { if (!e.deltaX) this.tabsEl.scrollLeft += e.deltaY; }, { passive: true });
  }

  // ---------------------------------------------------------------- tabs

  // Show `buf` in its tab here, made if there is none. `preview`: a new
  // tab is the split's preview tab, in the place of the one there was
  // (unless that one holds unsaved changes: it is kept).
  show(buf, preview) {
    const v = this.views.find((x) => x.buf === buf) ?? this.newTab(buf, preview);
    this.shown = [v];
    this.editor.shown(this, v);
    this.renderTabs();
    this.scheduleRender();
    return v;
  }
  // A view of this split follows its buffer's edits (its cursors and
  // scroll move through them) until it is closed.
  track(v) {
    const offOps = v.buf.follow((op) => {
      v.shift(op);
      if (this.shown.includes(v)) this.sinceDrawn.push(op);
    });
    const offText = v.buf.followText((c) => { v.index.change(c); });
    this.unfollow.set(v, () => { offOps(); offText(); });
  }
  untrack(v) {
    for (const off of found(this.unfollow.get(v))) off();
    this.unfollow.delete(v);
  }
  // A new tab of `buf`: the preview tab (in place of the one there was,
  // unless that one holds unsaved changes: it is kept), or a kept one.
  newTab(buf, preview) {
    const v = new View(this, buf);
    this.track(v);
    const old = preview ? this.views.find((x) => x.preview) : undefined;
    if (old !== undefined && !old.buf.modified) {
      this.views.splice(this.views.indexOf(old), 1, v);
      this.untrack(old);
      this.editor.dropIfUnshown(old.buf);
    } else {
      if (old !== undefined) old.preview = false;
      this.views.push(v);
    }
    v.preview = preview;
    return v;
  }
  closeView(v) {
    const i = this.views.indexOf(v);
    if (i < 0) return;
    this.views.splice(i, 1);
    this.untrack(v);
    if (this.shown.includes(v)) this.shown = this.views.slice(Math.max(0, i - 1), Math.max(0, i - 1) + 1);
    for (const s of this.shown) this.editor.shown(this, s);
    this.renderTabs();
    this.scheduleRender();
  }
  renderTabs() {
    this.tabsEl.innerHTML = "";
    const names = this.views.map((v) => v.buf.name);
    this.views.forEach((v, i) => {
      const t = document.createElement("div");
      t.className = "tab" + (this.shown.includes(v) ? " active" : "") + (v.preview ? " preview" : "");
      t.dataset.i = String(i);
      // Tabs of files with one name show their parent folder.
      // Two tabs of one name: each says its folder too.
      const dup = names.filter((n) => n === v.buf.name).length > 1;
      const label = (dup ? filePath(v.buf.origin).map((p) => p.split("/").slice(-2).join("/")).at(0) : undefined) ?? v.buf.name;
      t.textContent = (v.buf.modified ? "● " : "") + label + (v.buf.readOnly && !v.buf.isVirtual ? " [RO]" : "");
      t.title = (filePath(v.buf.origin).at(0) ?? v.buf.name) + (v.preview ? " (preview: the next preview replaces it; edit, double-click or Keep Open to keep it)" : "");
      // A press shows it (and may drag it: app.tabPress); the middle button
      // closes it; the right one opens its menu.
      t.onmousedown = (e) => { e.preventDefault(); if (e.button === 1) this.editor.closeTab(this, v); else if (e.button === 0) this.editor.tabPress(e, this, v); };
      t.oncontextmenu = (e) => { e.preventDefault(); this.show(v.buf, false); this.editor.focus(this); this.editor.tabMenu(this, v, { x: e.clientX, y: e.clientY }); };
      const x = document.createElement("span");
      x.className = "x";
      x.textContent = "×";
      x.onmousedown = (e) => { e.stopPropagation(); e.preventDefault(); this.editor.closeTab(this, v); };
      t.appendChild(x);
      this.tabsEl.appendChild(t);
    });
  }

  // ---------------------------------------------------------------- geometry

  // The width text may take, in px: the surface's, less the scrollbar.
  textWidth() { return (this.size.w > 0 ? this.size.w : this.surfaceEl.clientWidth) - SCROLLBAR_WIDTH; }

  metrics() {
    const m = this.editor.metrics();
    const w = this.textWidth();
    const h = this.size.h > 0 ? this.size.h : this.surfaceEl.clientHeight;
    return { cols: Math.max(10, Math.floor(w / m.cw)), rows: Math.max(1, Math.floor(h / m.lh)), cw: m.cw, lh: m.lh };
  }

  geometry(v) {
    const { cols, rows } = this.metrics();
    const compose = v.mode === "compose";
    // Line numbers, when the text's place in the file is known: the last
    // one's digits.
    const last = v.opts.lineNumbers && !compose ? listOf(v.buf.text).flatMap((L) => listOf(L.firstLine).map((f) => f + L.count)) : [];
    const nums = last.length > 0;
    const digits = last.map((n) => String(n).length).at(0) ?? 0;
    const gutter = (nums ? digits + 1 : 0) + 2;
    const avail = cols - gutter;
    const width = compose ? Math.min(v.opts.composeWidth ?? this.editor.settings().page_width, avail) : avail;
    const margin = compose ? Math.max(0, Math.floor((avail - width) / 2)) : 0;
    return { cols, rows, gutter, digits, width, margin, nums };
  }

  scheduleRender() {
    if (this.frame.id !== null) return;
    this.frame.id = requestAnimationFrame(() => { this.frame.id = null; this.render(); });
  }

  // ---------------------------------------------------------------- render

  layoutOpts(v, width) {
    const buf = v.buf;
    const L = buf.held;
    const sels = v.selections();
    const s = this.editor.settings();
    // The syntax colours (fenced blocks in Markdown come in their own
    // language: the grammar embeds it).
    const syn = this.editor.syntaxOf(buf);
    const synFor = (li) => syn.spans(li);
    // Each line's decorations, gathered a stretch of DECO_UNITS at a time
    // where the layout reaches it (the interval index makes that cost what
    // the stretch has), in order of precedence: overlays, occurrences,
    // diagnostics, selection, then the overlays drawn above the selection.
    const w = s.flags.highlight_occurrences === true ? v.selection() ?? this.editor.wordAt(buf, v.primary.pos) : null;
    const t /*: String | Null */ = w !== null ? buf.textRange(w.start, w.end) : null;
    /** const occ: String | Null */
    const occ = t !== null && t.length >= 2 && !t.includes("\n") ? t : null;
    // The bracket at the cursor and its match, drawn bold in the theme's
    // colour for them (found once per cursor position and text).
    const brackets = s.flags.highlight_brackets === true
      ? remember(this.brackets, `${buf.id}:${buf.textGen}:${v.primary.pos}`, () => this.editor.bracketPair(buf, v.primary.pos))
      : null;
    // Rainbow brackets: each bracket in the colour of its nesting depth,
    // counted from the start of the file (a buffer held whole), strings
    // and comments left out.
    const depths = s.flags.rainbow_brackets === true && buf.full ? rainbowDepths(buf, syn) : null;
    const stretches = new Map();
    const decosAt = (li, idx) => {
      const n = Math.floor(idx / DECO_UNITS);
      const key = `${li}:${n}`;
      const hit = stretches.get(key);
      if (hit !== undefined) return hit;
      const text = L.lines[li];
      // The stretch [i0, i1) of the line's text, and its bytes.
      const i0 = L.idxFrom(li, n * DECO_UNITS), i1 = L.idxFrom(li, (n + 1) * DECO_UNITS);
      const ls = L.posAt(li, i0), le = i1 >= text.length ? L.lineEnd(li) + 1 : L.posAt(li, i1);
      const out = [];
      const above = [];
      for (const o of buf.decor.overlaysIn(ls, le)) if (applies(o, v.mode)) (o.above ? above : out).push({ s: o.start, e: Math.max(o.end, o.start), f: restyler(o.style) });
      for (const ds of listOf(depths)) {
        let depth = ds[li];
        for (const x of syn.brackets(li)) {
          const opens = OPEN.has(x.c);
          if (!opens) depth--;
          if (x.k >= i0 && x.k < i1) {
            const at = L.posAt(li, x.k);
            out.push({ s: at, e: at + 1, f: RAINBOW[Math.max(0, depth) % RAINBOW.length] });
          }
          if (opens) depth++;
        }
      }
      for (const word of listOf(occ)) {
        // Occurrences that start in the stretch (read a word's length past it).
        const from = Math.max(0, i0 - 1), part = text.slice(from, Math.min(text.length, i1 + word.length + 1));
        for (let k = part.indexOf(word, i0 - from); k >= 0 && from + k < i1; k = part.indexOf(word, k + word.length)) {
          const at = from + k;
          if ((at === 0 || !/\w/.test(text.charAt(at - 1))) && !/\w/.test(text.charAt(at + word.length))) {
            const st0 = L.posAt(li, at);
            out.push({ s: st0, e: st0 + byteLen(word), f: OCCURRENCE });
          }
        }
      }
      if (brackets) for (const x of [brackets.at, brackets.match]) if (x >= ls && x < le) out.push({ s: x, e: x + 1, f: BRACKET });
      for (const g of buf.decor.diagsIn(ls, le)) { const wavy = THEME[diagKey(g.severity)] ?? ""; out.push({ s: g.start, e: Math.max(g.end, g.start + 1), f: (st) => withWavy(st, wavy) }); }
      for (const x of sels) if (x.start < le && x.end > ls) out.push({ s: x.start, e: x.end, f: SELECTED });
      out.push(...above);
      stretches.set(key, out);
      return out;
    };
    // Decorations over style `st` (the syntax style) at byte b, UTF-16
    // index `idx` of line li.
    const decoStyle = (b, li, idx, st) => {
      for (const x of decosAt(li, idx)) if (b >= x.s && x.e > b) st = x.f(st);
      return st;
    };
    return {
      // Line `li`'s syntax spans.
      syntax: synFor,
      mode: v.mode,
      wrap: v.opts.wrap,
      width,
      tab: 4,
      cursors: v.cursors.map((c) => c.pos),
      folds: v.folds,
      whitespace: s.flags.show_whitespace === true,
      // The background an overlay at `b` carries to the row's end, else
      // the syntax's row fill there (a diff's added or removed line).
      extendAt: (b) => {
        const o = buf.decor.overlaysIn(b, b + 1).find((x) => x.style.extendToLineEnd && x.style.bg !== null);
        if (o !== undefined) return color(o.style.bg);
        const li = L.indexOf(b);
        return li < 0 ? null : synFor(li).map((x) => x.style.bg).find((bg) => bg !== "") ?? null;
      },
      decoStyle,
      // The style at byte `b`, the `idx`th UTF-16 unit of line `li`.
      styleAt: (b, li, idx) => (li >= 0 ? decoStyle(b, li, idx, synAt(synFor(li), idx)) : EMPTY),
      // The style at byte `b`, its line found.
      styleAtByte: (b) => {
        const li = L.indexOf(b);
        if (li < 0) return EMPTY;
        const idx = L.idxOf(li, Math.min(b, L.lineEnd(li)));
        return decoStyle(b, li, idx, synAt(synFor(li), idx));
      },
    };
  }

  // The layout of view `v` as it is now (its rows' index keyed to it).
  layoutFor(v) {
    const buf = v.buf;
    const g = this.geometry(v);
    const x = layoutOf(buf.held, buf.decor, this.layoutOpts(v, g.width));
    v.index.use(layoutKey(x, buf.decor.layoutGen), v.opts.wrap);
    return x;
  }

  // The rows from view `v`'s top, at most `n` (a long line drawn without
  // wrapping from the columns on screen, `left` on).
  rowsOf(v, x, n) {
    const g = this.geometry(v);
    const from = (li) => columnStart(x, v.index, li, v.left);
    return rowsFrom(x, v.top, n, from, v.left + g.width + 1);
  }

  // The rows view `v` draws (layout `x`), its colours asked for.
  computeRows(v, x) {
    const L = v.buf.held;
    const g = this.geometry(v);
    const top = Math.max(0, L.indexOf(v.top));
    this.editor.wantColours(v.buf, top, Math.min(L.count, top + g.rows));
    return { rows: this.rowsOf(v, x, g.rows), g };
  }

  // Whether the text at view `v`'s top is held, with a screen's worth past
  // it (in lines, or in a huge line READY_BYTES).
  ready(v) {
    return listOf(v.buf.text).some((L) => {
      const i = L.indexOf(v.top);
      return i >= 0 && (v.buf.full || L.eof || L.count - i > this.metrics().rows + 2 || L.heldEnd() - v.top > READY_BYTES);
    });
  }

  // Fetch the text at view `v`'s top (and a screen past it), then draw.
  async fetch(v) {
    await v.buf.ensure(v.top, this.metrics().rows + 50);
    for (const L of listOf(v.buf.text)) {
      if (L.indexOf(v.top) < 0) v.setTop(L.starts[0], 0);
      if (this.ready(v) || L.eof) this.render();
    }
  }
  // Fetch the text from byte `p` (before the held window), then scroll `n`
  // rows.
  async scrollPast(v, p, n) {
    await v.buf.ensure(Math.max(0, p), ENSURE_AFTER);
    this.scrollRows(n);
  }

  // Clear the drawn rows (a pane with nothing to show, or still loading).
  clearRows() {
    this.textEl.textContent = "";
    this.gutterEl.textContent = "";
    this.caretsEl.textContent = "";
    this.rowsDrawn = [];
    this.gutterDrawn = [];
    this.rows = [];
    this.doc.clear();
  }

  // Draw now, not at the next frame (motion that measures what is drawn).
  renderNow() {
    const asked = this.frame.id;
    if (asked !== null) { cancelAnimationFrame(asked); this.frame.id = null; }
    this.render();
  }

  render() {
    setClass(this.el, "terminal", this.shown.some((x) => terminalOf(x.buf.source).length > 0));
    if (this.shown.length === 0) { this.clearRows(); return; }
    const v = this.shown[0];
    const buf = v.buf;
    for (const id of terminalOf(buf.source)) { this.editor.mountTerminal(id, this.termEl); this.editor.updateStatus(); return; }
    if (!this.ready(v)) {
      this.fetch(v);
      if (!listOf(buf.text).some((L) => L.indexOf(v.top) >= 0)) { this.clearRows(); this.textEl.textContent = "Loading…"; return; }
    }
    const L = buf.held;
    const compose = v.mode === "compose";
    if (this.surfaceEl.classList.contains("compose") !== compose) { this.clearRows(); setClass(this.surfaceEl, "compose", compose); }
    if (compose) { v.setTop(L.starts[L.indexOf(v.top)], v.topPx); this.renderDoc(v); return; }
    const { cw, lh } = this.metrics();
    if (this.drawn.cw !== cw) { this.surfaceEl.style.setProperty("--cw", `${cw}px`); this.drawn.cw = cw; }
    const g0 = this.geometry(v);
    const x = this.layoutFor(v);
    // The top: where the row holding the byte scrolled to starts, once the
    // rows there are known.
    v.rowTop(settledTop(measuring(x), v.index, v.scrolledTo));
    // Sideways: a cursor that moved is kept in view (a few columns in from
    // the edge), and the widest row on screen bounds the scroll.
    if (v.opts.wrap) v.left = 0;
    else if (this.wantX) {
      const col = columnOf(measuring(x), v.index, v.primary.pos);
      if (col < v.left) v.left = Math.max(0, col - 4);
      else if (col > v.left + g0.width - 2) v.left = col - g0.width + 6;
    }
    this.wantX = false;
    let { rows, g } = this.computeRows(v, x);
    this.maxCols = rows.reduce((m, r) => Math.max(m, r.col), 0);
    if (!v.opts.wrap && v.left > Math.max(0, this.maxCols - 1)) {
      v.left = Math.max(0, this.maxCols - 1);
      ({ rows, g } = this.computeRows(v, x));
    }
    this.rows = rows;
    this.sinceDrawn = [];
    const s = this.editor.settings();
    this.gutterEl.style.width = `${g.gutter * cw}px`;
    const left = (g.gutter + g.margin - v.left) * cw;
    this.textEl.style.left = this.caretsEl.style.left = this.rulersEl.style.left = `${left}px`;
    // Gutter marks and the worst diagnostic, by line, over the lines drawn.
    const marks = new Map();
    const dline = new Map();
    const drawnLines = rows.flatMap((r) => (r.kind === "text" ? listOf(r.lineIdx) : []));
    /** const drawnSpan: Range[] */
    const drawnSpan = drawnLines.length === 0 ? [] : [{ start: L.starts[Math.min(...drawnLines)], end: L.lineEnd(Math.max(...drawnLines)) + 1 }];
    for (const r of drawnSpan) {
      for (const m of buf.decor.guttersIn(r.start, r.end)) { const i = L.indexOf(m.start); if (i >= 0) marks.set(i, m); }
      for (const d of buf.decor.diagsIn(r.start, r.end)) {
        const i = L.indexOf(d.start), was = dline.get(i);
        if (i >= 0 && (was === undefined || d.severity < was)) dline.set(i, d.severity);
      }
    }
    const curLines = new Set(v.cursors.map((c) => L.indexOf(c.pos)));
    const folded = new Set(v.folds.map((f) => f.start));
    // Rows are drawn from HTML strings and kept by content (pool.js): a
    // keystroke rewrites the rows it changed, scrolling only the new ones,
    // and the children are swapped in with one replaceChildren.
    if (this.textEl.firstChild?.nodeType === 3) this.clearRows();
    const textPool = poolOf(this.rowsDrawn), gutPool = poolOf(this.gutterDrawn);
    const textEls = [], gutEls = [];
    let lastLine = -1;
    for (const r of rows) {
      // A text row's line (a virtual row has none).
      const line = r.kind === "text" ? listOf(r.lineIdx) : [];
      const current = s.flags.highlight_current_line === true && line.some((li) => curLines.has(li)) && v.selection() === null;
      const cls = "row" + (r.kind === "virtual" ? " virtual" : "") + (current && !r.fill ? " current" : "");
      textEls.push(take(textPool, rowHtml(cls, r.fill, r.cells, r.col0)));
      // The gutter: line number and one mark (fold, diagnostic or plugin).
      let ghtml = `<div class="row">\u200b</div>`;
      for (const li of line) {
        if (r.cont || li === lastLine) continue;
        lastLine = li;
        const m = marks.get(li);
        const sev = dline.get(li);
        const mark = folded.has(li) ? "▸" : sev ? (sev === 1 ? "●" : "▲") : m !== undefined ? m.glyph : " ";
        const mcol = sev ? THEME[sev === 1 ? "diagnostic.error" : "diagnostic.warning"] ?? "" : m !== undefined ? color(m.color) ?? "" : "";
        const num = g.nums && L.firstLine !== null ? String(L.firstLine + li + 1).padStart(g.digits) + " " : "";
        const title = m !== undefined && !sev && m.title ? ` title="${escapeHtml(m.title)}"` : "";
        ghtml = `<div class="row${current ? " current" : ""}" data-line="${li}"${title}>${num}<span class="mark"${mcol ? ` style="color:${mcol}"` : ""}>${escapeHtml(mark)}</span></div>`;
      }
      gutEls.push(take(gutPool, ghtml));
    }
    if (!sameDrawn(textEls, this.rowsDrawn)) this.textEl.replaceChildren(...textEls.map((d) => d.el));
    if (!sameDrawn(gutEls, this.gutterDrawn)) this.gutterEl.replaceChildren(...gutEls.map((d) => d.el));
    this.rowsDrawn = textEls;
    this.gutterDrawn = gutEls;
    // Rulers, and indentation guides: a line at each indent level inside a
    // line's leading whitespace (a blank line takes the next line's).
    let guides = "";
    for (const col of s.rulers) guides += `<div class="ruler" style="left:${(col - 1) * cw}px"></div>`;
    if (s.flags.show_indent_guides === true) {
      const indentOf = (li) => {
        const lead = /^[ \t]*/.exec(L.lines[li]);
        const m = lead ? lead[0] : "";
        if (m.length === L.lines[li].length) return -1;
        let w = 0;
        for (const ch of m) w = ch === "\t" ? w + 4 - (w % 4) : w + 1;
        return w;
      };
      let next = -1;
      const ind = rows.map(() => 0);
      for (let r = rows.length - 1; r >= 0; r--) {
        const row = rows[r];
        if (row.kind !== "text") { ind[r] = next < 0 ? 0 : next; continue; }
        const w = indentOf(row.lineIdx);
        ind[r] = w < 0 ? Math.max(0, next) : w;
        if (w >= 0) next = w;
      }
      rows.forEach((row, r) => {
        for (let c = 0; c < ind[r]; c += 4) guides += `<div class="guide" style="left:${c * cw}px;top:${r * lh}px;height:${lh}px"></div>`;
      });
    }
    if (this.drawn.rulers !== guides) { this.rulersEl.innerHTML = guides; this.drawn.rulers = guides; }
    // Carets: absolutely placed, so moving one never touches the rows.
    let html = "";
    if (this.active) {
      for (const c of v.cursors) {
        for (const at of this.caretXY(c.pos)) html += `<div class="caret" style="left:${at.x * cw}px;top:${at.r * lh}px;height:${lh}px"></div>`;
      }
    }
    if (this.drawn.carets !== html) { this.caretsEl.innerHTML = html; this.drawn.carets = html; }
    this.finishRender(v, g);
    this.indexLater(v);
  }

  // Index the long lines view `v` reached, a slice at a time between
  // frames.
  indexLater(v) {
    if (this.indexing.length > 0) return;
    this.indexing = [setTimeout(() => {
      this.indexing = [];
      if (!this.shown.includes(v) || v.mode !== "source" || v.buf.text === null) return;
      if (v.index.work(measuring(this.layoutFor(v)))) this.indexLater(v);
    }, 0)];
  }

  // A view in compose mode: a document (doc.js).
  renderDoc(v) {
    const g = this.geometry(v);
    this.doc.render(v);
    this.finishRender(v, g);
  }

  finishRender(v, g) {
    this.renderScrollbar(v, g);
    this.editor.syncScroll(this, v);
    if (this.lastWidth.get(v) !== g.width) {
      this.lastWidth.set(v, g.width);
      this.editor.resized(v);
    }
    this.editor.updateStatus();
  }

  renderScrollbar(v, g) {
    const L = v.buf.held;
    const buf = v.buf;
    // Where the view is: without wrapping, by lines (in a window, when the
    // file's are counted and the window's place among them is known),
    // else by bytes; with it, by bytes (rows.js).
    const counted = listOf(buf.totalLines).filter((n) => n > 0).flatMap((n) => listOf(L.firstLine).map((first) => ({ n, first })));
    const shown = this.rows.map((r) => r.end ?? v.top).reduce((m, e) => Math.max(m, e), v.top) - v.top;
    const wrapped = v.opts.wrap && v.mode === "source";
    const whole = buf.full && v.mode === "source" ? [fractionOf(this.layoutFor(v), v.top, g.rows, shown)] : wrapped ? [{ at: v.top / Math.max(1, buf.len), size: Math.min(1, shown / Math.max(1, buf.len)) }] : [];
    const frac = whole.map((f) => f.at).at(0) ?? counted.map((c) => (c.first + L.indexOf(v.top)) / c.n).at(0) ?? v.top / Math.max(1, buf.len);
    const size = whole.map((f) => f.size).at(0) ?? counted.map((c) => Math.min(1, g.rows / c.n)).at(0) ?? 0.02;
    const h = this.size.sb;
    this.thumbEl.style.top = `${frac * h}px`;
    this.thumbEl.style.height = `${Math.max(16, size * h)}px`;
    this.renderMarks(v, h);
  }

  // View `v`'s top for byte `p` of its held text: the cluster boundary at
  // or before it (trusted as where a row starts until the rows there are
  // known).
  topNear(v, p) {
    const L = v.buf.held;
    const li = L.indexOf(p);
    const i = L.idxBefore(li, p - L.starts[li]);
    const around = clustersAround(L.lines[li], i);
    return L.posAt(li, around.on ? i : around.prev);
  }

  // View `v`'s top for fraction `f` of a buffer held whole (the
  // scrollbar's place).
  topAtFraction(v, f) {
    if (v.mode !== "source") return v.buf.held.starts[Math.min(v.buf.held.count - 1, Math.floor(f * v.buf.held.count))];
    return topAtFraction(measuring(this.layoutFor(v)), v.index, f);
  }

  // Marks on the scrollbar, by where their line is in the file: git
  // changes (the gutter's marks) in the left lane, search matches in the
  // middle, diagnostics in the right, the cursor's line across. One mark a
  // pixel row and lane, the most severe; redrawn only when what they show
  // changed. Only a buffer held whole has them (a huge file's lines are
  // not all known).
  renderMarks(v, h) {
    const b = v.buf;
    const d = b.decor;
    const sig = b.full ? `${b.id}:${d.gen}:${b.textGen}:${b.held.count}:${b.held.indexOf(v.primary.pos)}:${h}` : "";
    if (sig === this.marksSig) return;
    this.marksSig = sig;
    if (sig === "" || h === 0) { this.marksEl.replaceChildren(); return; }
    const L = b.held;
    const cursor = L.indexOf(v.primary.pos);
    const n = Math.max(1, L.count);
    const lanes = [noMarks(), noMarks(), noMarks()]; // y -> its mark
    // A mark in `lane` at the row of byte `pos` (one of a lower rank wins).
    /** const put: (Number, Number, Number, String | Null) => Undefined */
    const put = (lane, pos, rank, col) => {
      const i = L.indexOf(pos);
      if (i < 0 || col === null || col === "") return;
      const y = Math.min(h - 2, Math.floor((i / n) * h));
      const at = lanes[lane].get(y);
      if (at === undefined || at.rank > rank) lanes[lane].set(y, { rank, color: col });
    };
    // The whole text is held: its marks are those in [0, len].
    const marks = d.marksIn(0, b.len + 1);
    for (const m of marks.gutters) put(0, m.start, 1, color(m.color));
    for (const o of marks.overlays) { const bg = o.style.bg; if (bg !== null) put(1, o.start, 1, color(bg)); }
    for (const g of marks.diags) put(2, g.start, g.severity, THEME[diagKey(g.severity)] ?? null);
    const els = [];
    lanes.forEach((m, lane) => {
      m.forEach((mark, y) => {
        const e = document.createElement("div");
        e.className = `mark lane${lane}`;
        e.style.top = `${y}px`;
        e.style.background = mark.color;
        els.push(e);
      });
    });
    const c = document.createElement("div");
    c.className = "mark cursor";
    c.style.top = `${Math.min(h - 2, Math.floor((cursor / n) * h))}px`;
    els.push(c);
    this.marksEl.replaceChildren(...els);
  }

  // Where a caret at byte `p` is drawn, in client pixels: {x, y, h} (none
  // when it is not on screen).
  caretBox(p) {
    if (this.shown.some((v) => v.mode === "compose")) {
      return listOf(this.doc.caretRect(p)).map((r) => ({ x: r.left, y: r.top, h: r.height || this.metrics().lh }));
    }
    const { cw, lh } = this.metrics();
    const rect = this.textEl.getBoundingClientRect();
    return this.caretXY(p).map((at) => ({ x: rect.left + at.x * cw, y: rect.top + at.r * lh, h: lh }));
  }

  // The byte drawn at client point (x, y).
  posFromPoint(x, y) {
    for (const v of this.shown) if (v.mode === "compose") return this.doc.posFromPoint(v, x, y);
    const { cw, lh } = this.metrics();
    const rect = this.textEl.getBoundingClientRect();
    return this.posAt(Math.floor((y - rect.top) / lh), Math.max(0, (x - rect.left) / cw));
  }

  // The fold hiding line `li`, with the last line it keeps: {fold, keep}
  // (none: the line shows).
  foldHiding(v, li) {
    if (v.mode === "compose") return hiding(v, li);
    return v.folds.filter((f) => li > f.start && li <= f.end).slice(0, 1).map((f) => ({ fold: f, keep: f.start }));
  }

  // Where a caret can stand at or beyond `p` (in direction `dir`): not on
  // a line a fold hides, nor inside text a conceal hides (one revealed by a
  // caret at `p` shows).
  settle(v, p, dir) {
    const L = v.buf.text;
    if (L !== null) {
      for (const h of this.foldHiding(v, L.indexOf(p))) p = dir < 0 || h.fold.end + 1 >= L.count ? L.lineEnd(h.keep) : L.starts[h.fold.end + 1];
    }
    for (let guard = 0; guard < 64; guard++) {
      const c = v.buf.decor.concealsIn(p, p + 1).find((c) => c.start < p && p < c.end && applies(c, v.mode) && !(c.reveal && cursorIn([p], c.reveal)));
      if (!c) return p;
      p = dir < 0 ? c.start : c.end;
    }
    return p;
  }

  // Where byte `p` is drawn: {r, x} in rows and columns (none when it is
  // not on screen).
  caretXY(p) {
    // The end of the last cell before `p` (at most one).
    let best = noXY();
    for (let r = 0; r < this.rows.length; r++) {
      const row = this.rows[r];
      if (row.kind !== "text") continue;
      let x = row.col0;
      for (const c of row.cells) {
        if (c.src === p) return [{ r, x }];
        for (const src of listOf(c.src)) if (src < p) best = [{ r, x: x + c.w }];
        x += c.w;
      }
      if (row.eol === p) return [{ r, x: row.col }];
      for (const eol of listOf(row.eol)) if (eol > p) for (const b of best) if (b.r === r) return [b];
    }
    return noXY();
  }

  // The byte under row `r`, column `x` of the rows drawn, as the text is
  // now (moved through the edits made since they were drawn).
  posAt(r0, x) {
    const from = Math.max(0, Math.min(this.rows.length - 1, r0));
    const r = this.rows.findIndex((row, k) => k >= from && row.kind === "text");
    if (r < 0) return noByte();
    const at = byteAtColumn(this.rows[r], x);
    return some(this.sinceDrawn.reduce((p, op) => shiftPos(p, op, true), at));
  }

  // The text row of view `v` (layout `x`) starting at byte `top` (without
  // wrapping: its line's row, drawn from near column `col`).
  textRowAt(v, x, top, col) {
    const L = x.L;
    const li = blockOf(x, L.indexOf(top));
    const from = x.o.wrap ? { at: top, col: 0 } : columnStart(x, v.index, li, col);
    for (const r of blockRows(x, li, from.at, from.col, x.o.wrap ? Infinity : col + 2, ignoreMark)) if (r.kind === "text") return r;
    throw new Error(`no text row at byte ${top}`);
  }
  // The start of the row holding byte `p` of view `v` (without wrapping,
  // its line's start).
  rowOf(v, x, p) {
    const L = x.L;
    return x.o.wrap ? rowStart(x, v.index, p) : L.starts[blockOf(x, L.indexOf(p))];
  }

  // Where Home goes from byte `p` in a wrapped line, by rows: the start of
  // the row holding it, or from there the row before's (a list of at most
  // one: none on a line's first row, where Home is the line's).
  rowHome(v, p) {
    if (v.buf.held.indexOf(p) < 0) return noBytes();
    const x = measuring(this.layoutFor(v));
    const row = this.textRowAt(v, x, this.rowOf(v, x, p), 0);
    const start = row.start ?? p;
    if (!row.cont) return noBytes();
    return [p === start ? rowsUp(x, v.index, start, 1, true).top : start];
  }
  // Where End goes from byte `p` in a wrapped line, by rows: the end of the
  // row holding it (its line's end, or before the row's last cluster), or
  // from there the next row's.
  rowEnd(v, p) {
    if (v.buf.held.indexOf(p) < 0) return p;
    const x = measuring(this.layoutFor(v));
    const row = this.textRowAt(v, x, this.rowOf(v, x, p), 0);
    const end = rowEndOf(row);
    if (p !== end || row.eol !== null) return end;
    return rowEndOf(this.textRowAt(v, x, rowsDown(x, v.index, row.start ?? p, 1, true).top, 0));
  }


  // ---------------------------------------------------------------- motion

  // Scroll `n` rows (a compose view: `n` line heights).
  scrollRows(n) {
    if (this.shown.length === 0) return;
    const v = this.shown[0];
    if (v.buf.text === null) return;
    if (v.mode === "compose") { v.setTop(v.top, v.topPx + n * this.metrics().lh); this.scheduleRender(); return; }
    const L = v.buf.held;
    if (L.indexOf(v.top) < 0) return;
    const x = measuring(this.layoutFor(v));
    const to = n > 0 ? rowsDown(x, v.index, v.top, n, false) : rowsUp(x, v.index, v.top, -n, false);
    v.setTop(to.top, 0);
    // Past the start of a window: the text before it, then the rest.
    if (n < 0 && to.moved < -n && !L.bof) { this.scrollPast(v, L.start - 1, n + to.moved); return; }
    const li = L.indexOf(v.top);
    const near = (!L.eof && (L.count - li < 400 || L.heldEnd() - v.top < READY_BYTES * 4)) || (!L.bof && (li < 50 || v.top - L.start < READY_BYTES * 4));
    if (!v.buf.full && near) v.buf.ensure(v.top, 400);
    this.scheduleRender();
  }

  // Scroll `n` columns sideways (no wrap), as far as the widest row on
  // screen goes.
  scrollCols(n) {
    if (this.shown.length === 0) return;
    const v = this.shown[0];
    if (v.opts.wrap || v.mode === "compose") return;
    const width = this.geometry(v).width;
    v.left = Math.max(0, Math.min(v.left + n, (this.maxCols || 0) - width + 4));
    this.scheduleRender();
  }

  // Keep the primary cursor on screen; `center` puts it mid-screen.
  // Sideways, the next draw keeps it in view (it measures the rows).
  reveal(center) {
    if (this.shown.length === 0) return;
    const v = this.shown[0];
    if (v.buf.text === null) return;
    this.wantX = true;
    // A document keeps the cursor in view as it draws (it measures there).
    if (v.mode === "compose") { this.doc.want = center ? "center" : this.doc.want === "none" ? "keep" : this.doc.want; this.scheduleRender(); return; }
    const L = v.buf.held;
    if (L.indexOf(v.primary.pos) < 0) return;
    const x = measuring(this.layoutFor(v));
    const rows = this.metrics().rows;
    const caret = this.rowOf(v, x, v.primary.pos);
    // Mid-screen, as asked or from a top no longer held (a jump far into a
    // huge file).
    if (center || L.indexOf(v.top) < 0) { v.setTop(rowsUp(x, v.index, caret, Math.floor(rows / 2), false).top, 0); return; }
    if (caret < v.top) { v.setTop(caret, 0); return; }
    // On screen: at or before the last row drawn from the top.
    if (caret <= rowsDown(x, v.index, v.top, rows - 1, false).top) return;
    v.setTop(rowsUp(x, v.index, caret, rows - 1, false).top, 0);
  }

  // Move the primary cursor by visual rows.
  moveRows(n, extend) {
    if (this.shown.length === 0) return;
    const v = this.shown[0];
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
      this.editor.place(this, p, extend);
      c.goalX = goal;
      return;
    }
    const c = v.primary;
    if (v.buf.held.indexOf(c.pos) < 0) return;
    const x = measuring(this.layoutFor(v));
    const here = this.rowOf(v, x, c.pos);
    const goal = c.goalX ?? (v.opts.wrap ? columnIn(this.textRowAt(v, x, here, 0), c.pos) : columnOf(x, v.index, c.pos));
    const to = n > 0 ? rowsDown(x, v.index, here, n, true) : rowsUp(x, v.index, here, -n, true);
    if (to.moved === 0) return;
    this.editor.place(this, byteAtColumn(this.textRowAt(v, x, to.top, goal), goal), extend);
    c.goalX = goal;
  }

  // A page up or down: the view scrolls a screen and the cursor keeps its
  // place on it.
  page(dir, extend) {
    if (this.shown.length === 0) return;
    const v = this.shown[0];
    if (v.mode !== "compose") {
      const n = this.metrics().rows;
      this.scrollRows(dir * n);
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
    const y = (at.map((a) => a.y + a.h / 2).at(0) ?? sr.top + sr.height / 2) + dist - shift;
    const p = this.posFromPoint(this.doc.content.getBoundingClientRect().left + goal, y);
    if (p != null) { this.editor.place(this, p, extend); c.goalX = goal; }
  }

  // ---------------------------------------------------------------- mouse

  onMouseDown(e) {
    if (e.target === this.scrollbarEl || e.target === this.thumbEl || this.gutterEl.contains(e.target)) return;
    e.preventDefault();
    this.editor.focus(this);
    if (this.shown.length === 0) return;
    const v = this.shown[0];
    // A document's fold toggle.
    for (const fold of listOf(e.target.closest("[data-fold]"))) { this.editor.toggleFold(this, Number(fold.dataset.fold)); return; }
    const p = this.posFromPoint(e.clientX, e.clientY);
    if (p == null) return;
    // A right press moves the caret there unless it is in the selection
    // (the context menu acts on it); the menu opens on `contextmenu`.
    if (e.button === 2) {
      if (!v.selections().some((s) => p >= s.start && p <= s.end)) this.editor.place(this, p, false);
      return;
    }
    // Alt+drag or a middle-button drag selects a rectangle (a block); an
    // Alt+click without a drag adds a cursor.
    if (e.button === 1 || (e.button === 0 && e.altKey)) { this.blockDrag(v, e, p); return; }
    if (e.button !== 0) return;
    this.clicks = nextClicks(this.clicks, e.timeStamp, e.clientX, e.clientY);
    this.editor.place(this, p, e.shiftKey);
    if (e.ctrlKey) { this.editor.run("lsp.definition"); return; }
    if (this.clicks.n === 2) this.editor.selectWord(this);
    if (this.clicks.n === 3) this.editor.selectLine(this);
    const extend = (at) => { const q = this.posFromPoint(at.x, at.y); if (q != null) this.editor.place(this, q, true); };
    // Held past the top or bottom edge, the view scrolls each frame (faster
    // the farther out) and the selection follows to the edge.
    // Where the mouse was last, and the frame asked for (at most one).
    let last = { x: e.clientX, y: e.clientY };
    let frame = noFrame();
    const tick = (_t) => {
      frame = noFrame();
      const sr = this.surfaceEl.getBoundingClientRect();
      const d = last.y < sr.top ? last.y - sr.top : last.y > sr.bottom ? last.y - sr.bottom : 0;
      if (d === 0) return;
      this.scrollRows(Math.sign(d) * Math.min(10, 1 + Math.floor(Math.abs(d) / this.metrics().lh)));
      this.renderNow();
      extend(last);
      frame = [requestAnimationFrame(tick)];
    };
    const move = (ev) => { last = { x: ev.clientX, y: ev.clientY }; extend(last); if (frame.length === 0) frame = [requestAnimationFrame(tick)]; };
    const up = (_ev) => { for (const f of frame) cancelAnimationFrame(f); window.removeEventListener("mousemove", move); window.removeEventListener("mouseup", up); };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
  }

  // The screen line and column at client point (x, y), for a block's
  // corner: {line, col} (the column may be past the line's end); none
  // off the text.
  cornerFromPoint(v, x, y) {
    const L = v.buf.text;
    const p = this.posFromPoint(x, y);
    if (p == null || L === null) return noCorner();
    const rect = this.textEl.getBoundingClientRect();
    return [{ line: L.indexOf(p), col: Math.max(0, Math.round((x - rect.left) / this.metrics().cw)) }];
  }
  blockDrag(v, e, p) {
    if (v.mode === "compose") return;
    const x0 = e.clientX, y0 = e.clientY;
    const anchor = this.cornerFromPoint(v, x0, y0);
    let dragged = false;
    const move = (ev) => {
      if (!dragged && Math.hypot(ev.clientX - x0, ev.clientY - y0) < 4) return;
      dragged = true;
      const head = this.cornerFromPoint(v, ev.clientX, ev.clientY);
      for (const a of anchor) for (const h of head) this.editor.setBlock(this, a, h);
    };
    const up = (_ev) => {
      window.removeEventListener("mousemove", move);
      window.removeEventListener("mouseup", up);
      if (!dragged && e.button === 0) { v.cursors.push(cursorAt(p)); v.normalize(); this.scheduleRender(); }
    };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
  }

  onGutter(e) {
    e.preventDefault();
    this.editor.focus(this);
    for (const row of listOf(e.target.closest(".row"))) {
      for (const line of found(row.dataset.line)) this.editor.toggleFold(this, Number(line));
    }
  }

  onHover(e) {
    if (this.shown.length === 0) return;
    const v = this.shown[0];
    if (v.buf.text === null) return;
    this.editor.hover(this, this.posFromPoint(e.clientX, e.clientY), e.clientX, e.clientY);
  }

  onScrollbar(e) {
    e.preventDefault();
    this.editor.drive(this);
    const go = (ev) => {
      const r = this.scrollbarEl.getBoundingClientRect();
      this.editor.scrollTo(this, Math.max(0, Math.min(1, (ev.clientY - r.top) / r.height)));
    };
    go(e);
    const up = (_ev) => { window.removeEventListener("mousemove", go); window.removeEventListener("mouseup", up); };
    window.addEventListener("mousemove", go);
    window.addEventListener("mouseup", up);
  }
}
