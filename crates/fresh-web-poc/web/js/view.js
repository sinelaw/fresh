// A pane and the views in it: one view per buffer tab, each with its own
// scroll, cursors, view mode and width — all of it this client's.

import { layout, stringWidth } from "./layout.js";
import { byteLen } from "./ot.js";
import { byteToIdx } from "./buffer.js";
import { color, highlightLine, THEME } from "./highlight.js";
import { classify } from "./mdstruct.js";

let nextSplitId = 1;
const FLOW_RUN_MAX = 200;

// One buffer as shown in one pane.
export class View {
  constructor(pane, buf) {
    this.pane = pane;
    this.buf = buf;
    this.top = 0; // byte: start of the first visible source line
    this.cursor = 0;
    this.anchor = null; // selection anchor, or null
    this.compose = false;
    this.composeWidth = null;
    this.wrap = false;
    this.lineNumbers = null; // plugin default; null = on
    this.leftCol = 0;
    this.goalX = null;
    this.extraCursors = [];
    buf.views.add(this);
  }
  shift(op) {
    const sp = (p, r) => {
      if (op.t === "ins") return p > op.pos || (p === op.pos && r) ? p + byteLen(op.text) : p;
      if (p >= op.pos + op.len) return p - op.len;
      return p > op.pos ? op.pos : p;
    };
    this.cursor = sp(this.cursor, true);
    if (this.anchor != null) this.anchor = sp(this.anchor, false);
    this.top = sp(this.top, false);
    this.extraCursors = this.extraCursors.map((c) => sp(c, true));
  }
  selection() {
    if (this.anchor == null || this.anchor === this.cursor) return null;
    return { start: Math.min(this.anchor, this.cursor), end: Math.max(this.anchor, this.cursor) };
  }
  dispose() { this.buf.views.delete(this); }
}

export class Pane {
  constructor(app) {
    this.app = app;
    this.id = nextSplitId++;
    this.views = []; // tabs
    this.view = null;
    this.el = document.createElement("div");
    this.el.className = "pane";
    this.el.innerHTML = `<div class="tabs"></div><div class="surface"><div class="gutter"></div><div class="text"></div><div class="carets"></div><div class="scrollbar"><div class="thumb"></div></div></div>`;
    this.tabsEl = this.el.querySelector(".tabs");
    this.surface = this.el.querySelector(".surface");
    this.gutterEl = this.el.querySelector(".gutter");
    this.textEl = this.el.querySelector(".text");
    this.caretsEl = this.el.querySelector(".carets");
    this.scrollEl = this.el.querySelector(".scrollbar");
    this.thumbEl = this.el.querySelector(".thumb");
    this.rows = [];
    this.lastViewport = "";
    this.surface.addEventListener("mousedown", (e) => this.onMouseDown(e));
    this.surface.addEventListener("wheel", (e) => { e.preventDefault(); this.scrollLines(Math.sign(e.deltaY) * 3); }, { passive: false });
    this.surface.addEventListener("mousemove", (e) => this.onHover(e));
    this.scrollEl.addEventListener("mousedown", (e) => this.onScrollbar(e));
  }

  // ---------------------------------------------------------------- tabs

  show(buf) {
    let v = this.views.find((x) => x.buf === buf);
    if (!v) {
      v = new View(this, buf);
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
    for (const v of this.views) {
      const t = document.createElement("div");
      t.className = "tab" + (v === this.view ? " active" : "");
      t.textContent = (v.buf.modified ? "● " : "") + v.buf.name;
      t.title = v.buf.path || v.buf.name;
      t.onmousedown = (e) => { e.preventDefault(); if (e.button === 1) this.app.closeTab(this, v); else { this.show(v.buf); this.app.focusPane(this); } };
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
    const w = this.surface.clientWidth - 12;
    const h = this.surface.clientHeight;
    return { cols: Math.max(10, Math.floor(w / m.cw)), rows: Math.max(1, Math.floor(h / m.lh)), cw: m.cw, lh: m.lh };
  }

  scheduleRender() {
    if (this.pendingRender) return;
    this.pendingRender = requestAnimationFrame(() => { this.pendingRender = null; this.render(); });
  }

  // ---------------------------------------------------------------- render

  layoutOpts(v, width) {
    const buf = v.buf;
    const L = buf.text;
    const lang = buf.language || (buf.path || "").endsWith(".md") && "markdown";
    const sel = v.selection();
    const cursors = [v.cursor, ...v.extraCursors];
    const syn = new Map();
    // Inside a fenced block, highlight in the fence's language.
    const struct = buf.structGen === buf.textGen ? buf.struct : null;
    const FENCE_LANG = { bash: "shell", sh: "shell", shell: "shell", console: "shell", rust: "rust", rs: "rust", js: "javascript", javascript: "javascript", ts: "typescript", typescript: "typescript", python: "python", py: "python", json: "json", toml: "toml", go: "go", c: "c", cpp: "cpp" };
    const fenceLang = (li) => {
      let k = li;
      while (k > 0 && struct.region[k] !== "open") k--;
      const info = (/^\s*(?:`{3,}|~{3,})\s*([\w+-]*)/.exec(L.lines[k]) || [])[1] || "";
      return FENCE_LANG[info.toLowerCase()] || null;
    };
    const synFor = (li) => {
      if (!syn.has(li)) {
        const region = struct && struct.region[li];
        const l = region === "body" ? fenceLang(li) : lang;
        syn.set(li, region === "body" && !l ? [] : highlightLine(l, L.lines[li]));
      }
      return syn.get(li);
    };
    const overlays = buf.decor.overlays;
    const diags = buf.decor.diags;
    const search = this.app.searchHit && this.app.searchHit.buf === buf ? this.app.searchHit : null;
    return {
      compose: v.compose,
      wrap: v.wrap || v.compose,
      width,
      tab: 4,
      cursors,
      extendAt: (b) => {
        for (const o of overlays) if (o.style?.extendToLineEnd && o.start <= b && o.end >= b && o.style.bg) return color(o.style.bg);
        return null;
      },
      styleAt: (b, li, idx) => {
        let st = {};
        if (li != null) for (const s of synFor(li)) if (idx >= s.s && idx < s.e) { st = { fg: THEME[s.k], bold: s.bold, italic: s.italic }; break; }
        for (const o of overlays) {
          if (b >= o.start && b < o.end) {
            const s = o.style || {};
            st = { ...st };
            if (s.fg != null) st.fg = color(s.fg);
            if (s.bg != null) st.bg = color(s.bg);
            if (s.bold) st.bold = true;
            if (s.italic) st.italic = true;
            if (s.underline) st.underline = true;
            if (s.strikethrough) st.strike = true;
          }
        }
        for (const g of diags) if (b >= g.start && b < Math.max(g.end, g.start + 1)) st = { ...st, wavy: ["", THEME["diagnostic.error"], THEME["diagnostic.warning"], THEME["diagnostic.info"], THEME["diagnostic.hint"]][g.severity] || THEME["diagnostic.error"] };
        if (search && b >= search.pos && b < search.pos + search.len) st = { ...st, bg: THEME["search.match_bg"] };
        if (sel && b >= sel.start && b < sel.end) st = { ...st, bg: THEME["editor.selection_bg"] };
        return st;
      },
    };
  }

  geometry(v) {
    const { cols, rows } = this.metrics();
    const buf = v.buf;
    const L = buf.text;
    const showNums = v.lineNumbers ?? !buf.isVirtual;
    // A huge file has no line numbers to show (no line index), as in the editor.
    const maxLine = L && L.firstLine != null ? L.firstLine + L.count : null;
    const gutter = showNums && maxLine != null ? String(maxLine).length + 2 : 1;
    const avail = cols - gutter;
    const width = v.compose ? Math.min(v.composeWidth || 80, avail) : avail;
    const margin = v.compose ? Math.max(0, Math.floor((avail - width) / 2)) : 0;
    return { cols, rows, gutter, width, margin, showNums };
  }

  computeRows(v, extra = 1) {
    const L = v.buf.text;
    const g = this.geometry(v);
    const topIdx = L.indexOf(v.top);
    const r = layout(v.buf, Math.max(0, topIdx), g.rows + extra, this.layoutOpts(v, g.width));
    return { ...r, g, topIdx };
  }

  render() {
    const v = this.view;
    this.textEl.innerHTML = "";
    this.gutterEl.innerHTML = "";
    this.caretsEl.innerHTML = "";
    if (!v) return;
    const buf = v.buf;
    const L = buf.text;
    if (!L || L.indexOf(v.top) < 0 || (!L.eof && L.count - L.indexOf(v.top) < this.metrics().rows + 2 && !buf.full)) {
      if (buf.ensure) {
        this.textEl.textContent = "Loading…";
        buf.ensure(v.top, this.metrics().rows + 50).then(() => {
          if (buf.text && buf.text.indexOf(v.top) < 0) v.top = buf.text.starts[0];
          if (buf.text && buf.text.indexOf(v.top) >= 0) this.render();
        });
      }
      if (!L || L.indexOf(v.top) < 0) return;
    }
    // Snap the top to a line start.
    const ti = L.indexOf(v.top);
    v.top = L.starts[ti];
    const { rows, consumed, g } = this.computeRows(v, 0);
    this.rows = rows;
    this.geom = g;
    this.offerLines(v, consumed);
    const { cw, lh } = this.metrics();
    this.gutterEl.style.width = `${g.gutter * cw}px`;
    const textLeft = (g.gutter + g.margin) * cw;
    this.textEl.style.left = `${textLeft}px`;
    this.caretsEl.style.left = `${textLeft}px`;
    const frag = document.createDocumentFragment();
    const gfrag = document.createDocumentFragment();
    let lastLine = -1;
    for (const r of rows) {
      const div = document.createElement("div");
      div.className = "row" + (r.kind === "virtual" ? " virtual" : "");
      if (r.fill) div.style.background = r.fill;
      let run = null, runStyle = null;
      const flush = () => {
        if (run == null) return;
        const s = document.createElement("span");
        s.textContent = run;
        applyStyle(s, runStyle);
        div.appendChild(s);
        run = null;
      };
      for (const c of r.cells) {
        const key = styleKey(c.style);
        if (run != null && key === runStyle.__k) run += c.ch;
        else { flush(); run = c.ch; runStyle = { ...c.style, __k: key }; }
      }
      flush();
      if (!r.cells.length) div.appendChild(document.createTextNode("​"));
      frag.appendChild(div);
      const gd = document.createElement("div");
      gd.className = "row";
      if (g.showNums && r.kind === "text" && !r.cont && r.lineIdx !== lastLine) {
        gd.textContent = String(L.firstLine != null ? L.firstLine + r.lineIdx + 1 : "").padStart(g.gutter - 1) + " ";
        lastLine = r.lineIdx;
      } else gd.textContent = "​";
      gfrag.appendChild(gd);
    }
    this.textEl.appendChild(frag);
    this.gutterEl.appendChild(gfrag);
    // Carets.
    if (this.app.activePane === this) {
      for (const p of [v.cursor, ...v.extraCursors]) {
        const at = this.caretXY(p);
        if (!at) continue;
        const c = document.createElement("div");
        c.className = "caret";
        c.style.left = `${at.x * cw}px`;
        c.style.top = `${at.r * lh}px`;
        c.style.height = `${lh}px`;
        this.caretsEl.appendChild(c);
      }
    }
    this.renderScrollbar(v, g);
    // viewport_changed
    const vp = `${buf.id}:${v.top}:${g.cols}:${g.rows}`;
    if (vp !== this.lastViewport) {
      this.lastViewport = vp;
      this.app.plugins.fire("viewport_changed", {
        split_id: this.id, buffer_id: buf.id, window_id: 1, top_byte: v.top,
        top_line: L.firstLine != null ? L.firstLine + ti : null, width: g.cols, height: g.rows,
      });
    }
    this.app.updateStatus();
  }

  renderScrollbar(v, g) {
    const L = v.buf.text;
    const buf = v.buf;
    let frac, size;
    if (L.firstLine != null && buf.totalLines) {
      const top = L.firstLine + L.indexOf(v.top);
      frac = top / Math.max(1, buf.totalLines);
      size = Math.min(1, g.rows / Math.max(1, buf.totalLines));
    } else {
      frac = v.top / Math.max(1, buf.len);
      size = 0.02;
    }
    const h = this.scrollEl.clientHeight;
    this.thumbEl.style.top = `${frac * h}px`;
    this.thumbEl.style.height = `${Math.max(16, size * h)}px`;
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

  // ---------------------------------------------------------------- plugins

  offerLines(v, consumed) {
    const buf = v.buf;
    const L = buf.text;
    if (!consumed.length) return;
    const composing = [...buf.views].some((x) => x.compose);
    let a = consumed[0], b = consumed[consumed.length - 1];
    if (composing) {
      // Walk out to the blank lines around, so a reflowing pass sees whole
      // paragraphs, as the editor offers them.
      let k = 0;
      while (a > 0 && L.lines[a - 1].trim() !== "" && k++ < FLOW_RUN_MAX) a--;
      k = 0;
      while (b < L.count - 1 && L.lines[b].trim() !== "" && k++ < FLOW_RUN_MAX) b++;
    }
    const seen = (buf.offered ||= new Set());
    let struct = null;
    const isMd = /\.(md|markdown)$/.test(buf.path || buf.name || "");
    if (isMd && buf.full) {
      if (buf.structGen !== buf.textGen) {
        buf.struct = classify(L.lines);
        buf.structGen = buf.textGen;
      }
      struct = buf.struct;
    }
    const keyOf = (i) => `${L.starts[i]}\u0000${L.lines[i]}${L.hasNewline(i) ? "\n" : ""}`;
    const send = [];
    for (let i = a; i <= b; i++) send.push(!seen.has(keyOf(i)));
    if (composing) {
      // A run (lines between blank lines) is offered whole when any of it
      // is new, so a pass that flows a paragraph sees all of it.
      let i = a;
      while (i <= b) {
        if (L.lines[i].trim() === "") { i++; continue; }
        const s0 = i;
        while (i <= b && L.lines[i].trim() !== "") i++;
        if (send.slice(s0 - a, i - a).some(Boolean)) for (let k = s0; k < i; k++) send[k - a] = true;
      }
    }
    const lines = [];
    for (let i = a; i <= b; i++) {
      if (!send[i - a]) continue;
      seen.add(keyOf(i));
      const content = L.lines[i] + (L.hasNewline(i) ? "\n" : "");
      const line = {
        line_number: L.firstLine != null ? L.firstLine + i : Math.floor(L.starts[i] / 80),
        byte_start: L.starts[i],
        byte_end: L.starts[i] + byteLen(content),
        content,
      };
      if (struct) {
        if (struct.region[i]) line.region = struct.region[i];
        if (struct.table[i]) line.table = struct.table[i];
      }
      lines.push(line);
    }
    if (!lines.length) return;
    this.app.plugins.fire("lines_changed", {
      buffer_id: buf.id, lines, epoch: buf.version || 0, is_composing_in_any_split: composing,
    });
  }

  // ---------------------------------------------------------------- motion

  scrollLines(n) {
    const v = this.view;
    if (!v || !v.buf.text) return;
    const L = v.buf.text;
    let i = L.indexOf(v.top);
    if (i < 0) return;
    const j = Math.max(0, Math.min(L.count - 1, i + n));
    if (j === i && n < 0 && !L.bof) {
      v.buf.ensure(Math.max(0, L.start - 1)).then(() => { this.scrollLines(n); });
      return;
    }
    v.top = L.starts[j];
    if (!L.eof && L.count - j < 400) v.buf.ensure(v.top, 400);
    if (!L.bof && j < 50) v.buf.ensure(v.top, 400);
    this.scheduleRender();
  }

  // Keep the caret on screen.
  reveal() {
    const v = this.view;
    if (!v || !v.buf.text) return;
    const L = v.buf.text;
    let ci = L.indexOf(v.cursor);
    if (ci < 0) return;
    const ti = L.indexOf(v.top);
    if (ci < ti) { v.top = L.starts[ci]; return; }
    // Lay out from the top and see whether the caret is within the rows.
    for (let guard = 0; guard < 2000; guard++) {
      const { rows, g } = this.computeRows(v, 0);
      this.rows = rows;
      const at = this.caretXY(v.cursor);
      if (at && at.r < g.rows) return;
      const ti2 = L.indexOf(v.top);
      if (ti2 >= ci || ti2 >= L.count - 1) return;
      v.top = L.starts[ti2 + 1];
    }
  }

  // Move the caret by visual rows.
  moveRows(n, extend) {
    const v = this.view;
    if (!v) return;
    const L = v.buf.text;
    this.reveal();
    let { rows } = this.computeRows(v, Math.abs(n) + 2);
    this.rows = rows;
    let at = this.caretXY(v.cursor);
    if (!at) return;
    if (v.goalX == null) v.goalX = at.x;
    let r = at.r;
    let left = Math.abs(n);
    while (left > 0) {
      let t = r + Math.sign(n);
      while (t >= 0 && t < rows.length && rows[t].kind !== "text") t += Math.sign(n);
      if (t < 0) {
        const ti = L.indexOf(v.top);
        if (ti === 0) break;
        v.top = L.starts[ti - 1];
        ({ rows } = this.computeRows(v, Math.abs(n) + 2));
        this.rows = rows;
        at = this.caretXY(v.cursor);
        r = at ? at.r : 0;
        continue;
      }
      if (t >= rows.length) break;
      r = t;
      left--;
    }
    const p = this.posAt(r, v.goalX);
    if (p != null) this.setCursor(p, extend, true);
  }

  setCursor(p, extend, keepGoal) {
    const v = this.view;
    const old = v.cursor;
    if (extend) { if (v.anchor == null) v.anchor = old; }
    else v.anchor = null;
    v.cursor = Math.max(0, Math.min(v.buf.len, p));
    if (!keepGoal) v.goalX = null;
    this.reveal();
    if (old !== v.cursor) this.app.cursorMoved(v, old);
    this.scheduleRender();
  }

  // ---------------------------------------------------------------- mouse

  onMouseDown(e) {
    if (e.target === this.scrollEl || e.target === this.thumbEl) return;
    e.preventDefault();
    this.app.focusPane(this);
    const v = this.view;
    if (!v) return;
    const { cw, lh } = this.metrics();
    const rect = this.textEl.getBoundingClientRect();
    const toPos = (ev) => this.posAt(Math.floor((ev.clientY - rect.top) / lh), Math.max(0, (ev.clientX - rect.left) / cw));
    const p = toPos(e);
    if (p == null) return;
    if (e.altKey && e.button === 0) { v.extraCursors.push(p); this.scheduleRender(); return; }
    this.setCursor(p, e.shiftKey);
    if (e.detail === 2) this.app.selectWord(this);
    const move = (ev) => { const q = toPos(ev); if (q != null) this.setCursor(q, true); };
    const up = () => { window.removeEventListener("mousemove", move); window.removeEventListener("mouseup", up); };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
  }

  onHover(e) {
    const v = this.view;
    if (!v || !this.rows.length) return;
    const { cw, lh } = this.metrics();
    const rect = this.textEl.getBoundingClientRect();
    const p = this.posAt(Math.floor((e.clientY - rect.top) / lh), Math.max(0, (e.clientX - rect.left) / cw));
    this.app.onHover(this, p, e.clientX, e.clientY);
  }

  onScrollbar(e) {
    e.preventDefault();
    const go = (ev) => {
      const r = this.scrollEl.getBoundingClientRect();
      const f = Math.max(0, Math.min(1, (ev.clientY - r.top) / r.height));
      this.app.scrollToFraction(this, f);
    };
    go(e);
    const up = () => { window.removeEventListener("mousemove", go); window.removeEventListener("mouseup", up); };
    window.addEventListener("mousemove", go);
    window.addEventListener("mouseup", up);
  }
}

function styleKey(s) {
  return `${s.fg || ""}|${s.bg || ""}|${s.bold ? 1 : 0}${s.italic ? 1 : 0}${s.underline ? 1 : 0}${s.strike ? 1 : 0}|${s.wavy || ""}`;
}
const css = (x) => (typeof x === "string" && (x.startsWith("#") || x.startsWith("rgb")) ? x : color(x));
function applyStyle(el, s) {
  if (!s) return;
  if (s.fg) el.style.color = css(s.fg);
  if (s.bg) el.style.background = css(s.bg);
  if (s.bold) el.style.fontWeight = "bold";
  if (s.italic) el.style.fontStyle = "italic";
  const deco = [];
  if (s.underline) deco.push("underline");
  if (s.strike) deco.push("line-through");
  if (s.wavy) { el.style.textDecoration = `underline wavy ${s.wavy}`; return; }
  if (deco.length) el.style.textDecoration = deco.join(" ");
}

export { stringWidth, byteToIdx };
