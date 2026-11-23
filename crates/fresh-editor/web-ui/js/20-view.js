// Editor views: the client's windows onto the core's panes. The core lays
// the buffer out at the view's size (wrapping, folds, virtual text, syntax,
// selections, diagnostics) and sends the visual lines; the client draws them
// and sends back presses, drags and scrolls in cells.
//
// A row arrives as the cells a terminal would draw, one entry per column
// (`""` for a column the grapheme before it covers). Each grapheme is drawn at
// its own column, so where text lands never depends on the browser font's
// widths: a wide character, an emoji sequence or a fallback glyph sits on the
// columns the core gave it, and the text after it on its own.

/** Cell metrics of the editor font, measured once it has loaded: a cell is
 *  the font's advance plus a little air (CELL_AIR), and a row is the font's
 *  size times ROW_AIR, so the buffer reads like a terminal rather than a
 *  page of tightly set code. */
const CELL_AIR = 1.0477, ROW_AIR = 18 / 13;
let CW = 8, CH = 18;
function measureCell() {
  const probe = el("span", "mono", "M".repeat(100));
  probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
  document.body.appendChild(probe);
  const adv = probe.getBoundingClientRect().width / 100;
  const size = parseFloat(getComputedStyle(probe).fontSize) || 15;
  probe.remove();
  CW = Math.round((adv || 8) * CELL_AIR * 100) / 100;
  CH = Math.round(size * ROW_AIR * 100) / 100;
  document.documentElement.style.setProperty("--cw", CW + "px");
  document.documentElement.style.setProperty("--ch", CH + "px");
}

/** The views on screen, by the leaf they show. */
const views = new Map();
/** And by the core's view id, for routing `view` events. */
const viewById = new Map();

class EditorView {
  constructor(leaf) {
    this.leaf = leaf;
    this.id = null;
    this.size = null;
    this.data = null;
    this.drawn = [];
    this.root = div("view");
    this.root.tabIndex = -1;
    this.root.setAttribute("role", "textbox");
    this.root.setAttribute("aria-multiline", "true");
    this.root.setAttribute("aria-label", "Editor");
    this.lines = el("canvas", "lines");
    this.ctx = this.lines.getContext("2d");
    this.caret = div("caret");
    this.bar = div("vbar");
    this.thumb = div("thumb");
    this.bar.appendChild(this.thumb);
    this.root.append(this.lines, this.caret, this.bar);
    this.pointer();
    this.resizer = new ResizeObserver(() => this.fit());
    this.resizer.observe(this.root);
  }

  /** Tell the core this view's size, opening it the first time. */
  fit() {
    const r = this.root.getBoundingClientRect();
    if (r.width < 4 || r.height < 4) return;
    const width = Math.max(1, Math.floor((r.width - 10) / CW));
    const height = Math.max(1, Math.floor(r.height / CH));
    if (this.size && this.size[0] === width && this.size[1] === height) return;
    this.size = [width, height];
    if (this.id === null) {
      if (this.opening) return;
      this.opening = true;
      api.call("view.open", { leaf: this.leaf, width, height }).then((v) => {
        this.opening = false;
        this.id = v.view;
        viewById.set(this.id, this);
        const [w, h] = this.size;
        if (w !== width || h !== height) api.send("view.resize", { view: this.id, width: w, height: h });
      }, () => { this.opening = false; });
    } else {
      api.send("view.resize", { view: this.id, width, height });
    }
  }

  close() {
    this.resizer.disconnect();
    if (this.id !== null) {
      api.send("view.close", { view: this.id });
      viewById.delete(this.id);
    }
    this.root.remove();
  }

  /** Apply a view event: its facts, and the rows that changed. */
  update(ev) {
    // The rows kept so far, updated with the ones that changed. (`ev.lines`
    // is the buffer's line count, for the scrollbar.)
    const rows = this.data ? this.data.rows : [];
    for (const [y, row] of ev.rows) rows[y] = row;
    rows.length = ev.count;
    for (let y = 0; y < ev.count; y++) if (!rows[y]) rows[y] = [];
    this.data = { ...ev, rows };
    this.paint();
  }

  repaint() {
    this.drawn = [];
    this.paint();
  }

  /** Size the canvas to the view's cells, at the screen's pixel density. */
  sizeCanvas(cols, rows) {
    const dpr = window.devicePixelRatio || 1;
    const w = cols * CW, h = rows * CH;
    const key = `${w}x${h}@${dpr}`;
    if (this.canvasKey === key) return;
    this.canvasKey = key;
    this.lines.width = Math.ceil(w * dpr);
    this.lines.height = Math.ceil(h * dpr);
    this.lines.style.width = w + "px";
    this.lines.style.height = h + "px";
    this.ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    this.drawn = [];
  }

  paint() {
    const d = this.data;
    if (!d) return;
    this.sizeCanvas(d.width, d.count);
    const look = textLook(this.root);
    if (this.look !== look.key) { this.look = look.key; this.drawn = []; }
    for (let y = 0; y < d.count; y++) {
      const key = JSON.stringify(d.rows[y]);
      if (this.drawn[y] === key) continue;
      this.drawn[y] = key;
      drawRow(this.ctx, d.rows[y], y, d.width, look);
    }
    this.drawn.length = d.count;
    this.placeCaret();
    this.placeBar();
    this.announce();
  }

  /** The scrollbar: by line when the core knows the buffer's lines (the
   *  window's share of them is the thumb), else by byte. */
  placeBar() {
    const d = this.data;
    const h = this.root.clientHeight;
    let frac, share;
    if (d.lines !== null && d.lines !== undefined && d.topLine !== null && d.topLine !== undefined) {
      const total = Math.max(1, d.lines);
      frac = Math.min(1, d.topLine / total);
      share = Math.min(1, d.count / total);
      this.bar.hidden = d.lines <= d.count && d.topLine === 0;
    } else {
      const len = Math.max(1, d.len);
      frac = Math.min(1, d.top / len);
      share = Math.min(1, (d.count * 60) / len);
      this.bar.hidden = false;
    }
    // No shorter than the skin's thumb allows (its min-height), so a
    // skin's slider keeps its grip on a long file.
    const th = Math.max(parseFloat(getComputedStyle(this.thumb).minHeight) || 18, share * h);
    this.thumb.style.height = th + "px";
    this.thumb.style.top = Math.min(h - th, frac * h) + "px";
  }

  /** Tell a screen reader the caret's line when the caret changes line. */
  announce() {
    const d = this.data;
    if (!d || !d.caret || !core.layout || core.layout.active !== this.leaf) return;
    const text = rowText(d.rows[d.caret[1]], d.gutter || 0).trimEnd();
    const key = `${this.leaf}:${d.caret[1]}:${text}`;
    if (key === this.announced) return;
    this.announced = key;
    const live = document.getElementById("live");
    if (live) live.textContent = text || "blank";
  }

  placeCaret() {
    const d = this.data;
    const active = core.layout && core.layout.active === this.leaf;
    const show = !!(d && d.caret && active && !core.ask);
    this.caret.hidden = !show;
    this.root.classList.toggle("active", !!active);
    if (!show) return;
    this.caret.style.left = d.caret[0] * CW + "px";
    this.caret.style.top = d.caret[1] * CH + "px";
    // The keyboard's text field sits on the caret, so an input method opens
    // its candidates there.
    const r = this.lines.getBoundingClientRect();
    kb.style.left = r.left + d.caret[0] * CW + "px";
    kb.style.top = r.top + d.caret[1] * CH + "px";
    // Restart the blink on every move.
    this.caret.classList.remove("blink");
    void this.caret.offsetWidth;
    this.caret.classList.add("blink");
  }

  /** The caret's position on the page, for anchoring popups. */
  caretRect() {
    const d = this.data;
    if (!d || !d.caret) return null;
    const r = this.lines.getBoundingClientRect();
    return { x: r.left + d.caret[0] * CW, y: r.top + d.caret[1] * CH, h: CH };
  }

  cellAt(e) {
    const r = this.lines.getBoundingClientRect();
    return {
      col: Math.max(0, Math.floor((e.clientX - r.left) / CW)),
      row: Math.max(0, Math.floor((e.clientY - r.top) / CH)),
    };
  }

  pointer() {
    let dragging = false, pending = null;
    this.lines.addEventListener("mousedown", (e) => {
      if (e.button !== 0 || this.id === null) return;
      e.preventDefault();
      focusKeyboard();
      const c = this.cellAt(e);
      api.send("view.press", {
        view: this.id, col: c.col, row: c.row, clicks: e.detail || 1,
        shift: e.shiftKey, ctrl: e.ctrlKey || e.metaKey, alt: e.altKey,
      });
      dragging = true;
      const move = (m) => {
        if (!dragging) return;
        const cell = this.cellAt(m);
        if (pending) { pending = cell; return; }
        pending = cell;
        requestAnimationFrame(() => {
          api.send("view.drag", { view: this.id, col: pending.col, row: pending.row });
          pending = null;
        });
      };
      const up = () => {
        dragging = false;
        window.removeEventListener("mousemove", move);
        window.removeEventListener("mouseup", up);
      };
      window.addEventListener("mousemove", move);
      window.addEventListener("mouseup", up);
    });
    let acc = 0;
    this.root.addEventListener("wheel", (e) => {
      if (this.id === null) return;
      e.preventDefault();
      acc += e.deltaMode === 1 ? e.deltaY * CH : e.deltaY;
      const lines = Math.trunc(acc / CH);
      if (lines !== 0) {
        acc -= lines * CH;
        api.send("view.scroll", { view: this.id, lines });
      }
    }, { passive: false });
    // The editor's own menu: the clipboard, then what the language server
    // offers here. A press outside any selection first puts the caret there.
    this.root.addEventListener("contextmenu", (e) => {
      e.preventDefault();
      if (this.id === null || !this.lines.contains(e.target)) return;
      if (!(this.data && this.data.selection)) {
        const c = this.cellAt(e);
        api.send("view.press", { view: this.id, col: c.col, row: c.row, clicks: 1 });
      }
      const act = (name) => () => { api.send("action", { name, view: this.id }); focusKeyboard(); };
      contextMenu(e.clientX, e.clientY, [
        { label: "Cut", run: act("cut") },
        { label: "Copy", run: act("copy") },
        { label: "Paste", run: () => pasteFromSystem(this) },
        { label: "Select All", run: act("select_all") },
        null,
        { label: "Go to Definition", run: act("lsp_goto_definition") },
        { label: "Find References", run: act("lsp_references") },
        { label: "Rename Symbol…", run: act("lsp_rename") },
        { label: "Code Actions…", run: act("lsp_code_actions") },
        null,
        { label: "Command Palette…", run: act("command_palette") },
      ]);
    });
    // The scrollbar: drag the thumb, or press the track to jump there.
    this.bar.addEventListener("mousedown", (e) => {
      if (this.id === null) return;
      e.preventDefault();
      e.stopPropagation();
      const r = this.bar.getBoundingClientRect();
      const th = this.thumb.getBoundingClientRect();
      const grab = e.target === this.thumb ? e.clientY - th.top : th.height / 2;
      let pending = null;
      const to = (y) => {
        const f = Math.max(0, Math.min(1, (y - grab - r.top) / Math.max(1, r.height)));
        if (pending !== null) { pending = f; return; }
        pending = f;
        requestAnimationFrame(() => { api.send("view.scrollto", { view: this.id, fraction: pending }); pending = null; });
      };
      to(e.clientY);
      const move = (m) => to(m.clientY);
      const up = () => { window.removeEventListener("mousemove", move); window.removeEventListener("mouseup", up); };
      window.addEventListener("mousemove", move);
      window.addEventListener("mouseup", up);
    });
    // Touch: a drag scrolls (the tap itself arrives as a press).
    let touchY = null, touchAcc = 0;
    this.root.addEventListener("touchstart", (e) => {
      if (e.touches.length === 1) { touchY = e.touches[0].clientY; touchAcc = 0; }
    }, { passive: true });
    this.root.addEventListener("touchmove", (e) => {
      if (touchY === null || this.id === null) return;
      e.preventDefault();
      const y = e.touches[0].clientY;
      touchAcc += touchY - y;
      touchY = y;
      const lines = Math.trunc(touchAcc / CH);
      if (lines) { touchAcc -= lines * CH; api.send("view.scroll", { view: this.id, lines }); }
    }, { passive: false });
    this.root.addEventListener("touchend", () => { touchY = null; }, { passive: true });
  }
}

/** The editor font and default colours as the page styles them now. */
function textLook(node) {
  const cs = getComputedStyle(node);
  const root = getComputedStyle(document.documentElement);
  const look = {
    family: cs.fontFamily,
    size: cs.fontSize,
    fg: root.getPropertyValue("--ed-fg").trim() || "#ddd",
    bg: root.getPropertyValue("--bg").trim() || "#111",
  };
  look.key = `${look.family}|${look.size}|${look.fg}|${look.bg}|${CW}|${CH}`;
  return look;
}

/** Draw one row: each run's background over its columns, then each
 *  grapheme centred on the columns it covers (itself and the `""` columns
 *  after it), then the run's lines. Edges are snapped to device pixels, so
 *  neighbouring cells meet without a seam. */
function drawRow(ctx, runs, y, cols, look) {
  const dpr = window.devicePixelRatio || 1;
  const snap = (v) => Math.round(v * dpr) / dpr;
  const top = snap(y * CH), h = snap((y + 1) * CH) - top;
  ctx.clearRect(0, top, cols * CW, h);
  ctx.textBaseline = "middle";
  ctx.textAlign = "center";
  for (const r of runs || []) {
    let fg = r.fg || look.fg, bg = r.bg || null;
    if (r.r) { const f = fg; fg = bg || look.bg; bg = f; }
    const left = snap(r.x * CW), right = snap((r.x + r.t.length) * CW);
    if (bg) { ctx.fillStyle = bg; ctx.fillRect(left, top, right - left, h); }
    ctx.globalAlpha = r.d ? 0.6 : 1;
    ctx.fillStyle = fg;
    ctx.font = `${r.i ? "italic " : ""}${r.b ? "700 " : ""}${look.size} ${look.family}`;
    for (let i = 0; i < r.t.length; i++) {
      if (!r.t[i]) continue;
      let span = 1;
      while (i + span < r.t.length && r.t[i + span] === "") span++;
      ctx.fillText(r.t[i], (r.x + i + span / 2) * CW, top + h / 2);
    }
    if (r.u) ctx.fillRect(left, top + h - 2, right - left, 1);
    if (r.s) ctx.fillRect(left, top + Math.floor(h / 2), right - left, 1);
    ctx.globalAlpha = 1;
  }
}

/** A row's text from column `from`: its graphemes in column order. */
function rowText(runs, from = 0) {
  const cols = [];
  for (const r of runs || []) r.t.forEach((g, i) => { cols[r.x + i] = g; });
  return cols.slice(from).map((g) => g || "").join("");
}

api.on("view", (v) => {
  const view = viewById.get(v.view);
  if (view) view.update(v);
});
api.on("view.closed", (v) => {
  const view = viewById.get(v.view);
  if (view) { view.id = null; view.data = null; viewById.delete(v.view); view.size = null; view.fit(); }
});
api.on("close", () => {
  // The core forgot every view with the connection; reopen them.
  for (const v of views.values()) { v.id = null; v.data = null; v.size = null; v.opening = false; }
  viewById.clear();
});
api.on("open", () => { for (const v of views.values()) v.fit(); });
