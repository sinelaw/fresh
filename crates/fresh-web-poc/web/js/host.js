// What plugins hold of the application (the API itself is api.js): handles
// on buffers and views, a buffer's decoration sets, and the plugins' event
// handlers, on four channels: plain events, buffer events (with the ops of a
// change), view events and the language servers' status.

// A plugin's decorations in one buffer, cleared and replaced as a unit or by
// range. Positions are byte offsets; every one moves with edits.
class DecorationSet {
  constructor(buffer, ns) {
    this.buffer = buffer;
    this.ns = ns;
  }
  get d() { return this.buffer.decor; }
  clear() { this.d.clearNamespace(this.ns); }
  clearRange(start, end) { this.d.clearNamespaceRange(this.ns, start, end); }
  // Style [start, end) (`style`: a Style; `opts`: OverlayOptions, see
  // types/fresh.d.js).
  overlay(start, end, style, opts) { this.d.add("overlays", { ns: this.ns, start, end, style, when: opts.when, mark: opts.mark, above: opts.above }); }
  // Draw `opts.text` instead of [start, end) (ConcealOptions). A conceal
  // over a newline joins the next line onto this row; in a compose view,
  // text "\n" is a line break, and a revealed range is drawn dimmed.
  conceal(start, end, opts) {
    this.d.add("conceals", { ns: this.ns, start, end, text: opts.text, style: opts.style, reveal: opts.reveal, when: opts.when });
  }
  // `text` drawn at `pos` (InlineOptions).
  inline(pos, text, opts) {
    this.d.add("inlines", { ns: this.ns, pos, text, style: opts.style, side: opts.side, edge: opts.edge, when: opts.when });
  }
  // A whole row of text above or below the line holding `pos` (LineOptions).
  line(pos, opts) {
    this.d.add("lines", { ns: this.ns, pos, above: opts.above, text: opts.text, style: opts.style, when: opts.when, order: opts.order });
  }
  // Document structure over the lines of [start, end), drawn by a compose
  // view as typography (`block`: a Block). Inside a table, "cell" nodes
  // split their row at their starts. Paragraph, item and quote lines are
  // joined (each line break drawn as a space) and wrapped; code keeps its
  // lines. A line in no node is a paragraph of its own.
  node(start, end, block, when) {
    this.d.add("nodes", { ns: this.ns, start, end, ...block, when });
  }
  // A mark in the gutter on the line holding `pos`.
  // (GutterOptions; the git gutter's glyph is "▎").
  gutter(pos, opts) { this.d.add("gutters", { ns: this.ns, start: pos, end: pos, glyph: opts.glyph, color: opts.color, title: opts.title }); }
  // This set's gutter marks where they are now (edits move them): [{pos, glyph}].
  gutters() { return this.d.gutters.filter((g) => g.ns === this.ns).map((g) => ({ pos: g.start, glyph: g.glyph })); }
  // This set's overlays where they are now: [{start, end}].
  overlays() { return this.d.overlays.filter((o) => o.ns === this.ns).map((o) => ({ start: o.start, end: o.end })); }
}

export class BufferHandle {
  constructor(app, buf) {
    this.app = app;
    this.b = buf;
  }
  get id() { return this.b.id; }
  get path() { return this.b.path; }
  get name() { return this.b.name; }
  get language() { return this.b.language; }
  get length() { return this.b.len; }
  get modified() { return !!this.b.modified; }
  get isVirtual() { return this.b.isVirtual; }
  // The whole text is held (an ordinary file, or a virtual buffer).
  get whole() { return !!this.b.full; }
  // Held text in [start, end), or null when it is not held.
  text(start = 0, end = this.b.len) { return this.b.textRange(start, end); }
  textAsync(start = 0, end = this.b.len) { return this.b.textAsync(start, end); }
  // The held lines: {lines, starts, firstLine}.
  lines() { const L = this.b.text; return L ? { lines: L.lines, starts: L.starts, firstLine: L.firstLine } : null; }
  edit(ops) { this.app.applyEdit(this.b, ops); }
  insert(pos, text) { this.edit([{ t: "ins", pos, text }]); }
  delete(start, end) { if (end > start) this.edit([{ t: "del", pos: start, len: end - start }]); }
  replace(start, end, text) { const ops = []; if (end > start) ops.push({ t: "del", pos: start, len: end - start }); if (text) ops.push({ t: "ins", pos: start, text }); this.edit(ops); }
  save() { return this.app.saveBuffer(this.b); }
  // The decoration set `name` of this buffer (each plugin names its own).
  decorations(name) { return new DecorationSet(this.b, name); }
  views() { return [...this.b.views].map((v) => new ViewHandle(this.app, v)); }
  setEntries(entries) { this.b.setEntries(entries); this.app.renderBuffer(this.b.id); }
  propsAt(pos) { return this.b.propsAt(pos); }
  // Foldable blocks, as document line ranges [{start, end}] (a language
  // server's folding ranges); folding uses indentation where none are given.
  setFoldRanges(ranges) { this.b.foldRanges = ranges; }
  render() { this.app.renderBuffer(this.b.id); }
}

export class ViewHandle {
  constructor(app, v) {
    this.app = app;
    this.v = v;
  }
  get buffer() { return new BufferHandle(this.app, this.v.buf); }
  get cursor() { return this.v.cursors[0].pos; }
  get cursors() { return this.v.cursors.map((c) => ({ pos: c.pos, anchor: c.anchor })); }
  get selection() { return this.v.selection(); }
  get mode() { return this.v.mode; }
  get options() { return { ...this.v.opts }; }
  // {mode: "source"|"compose", composeWidth, wrap, lineNumbers}
  setOptions(o) { Object.assign(this.v.opts, o); if (o.mode) this.v.mode = o.mode; this.app.renderBuffer(this.v.buf.id); }
  // Both resolve once the text there is loaded and the view shows it.
  // (`o`: CursorOptions, types/fresh.d.js.)
  setCursor(pos, o) { return this.app.moveTo(this.v, pos, { center: o.center, extend: false, history: false }); }
  // Select [start, end), the caret at `end`.
  async select(start, end, o) {
    await this.app.moveTo(this.v, start, { center: false, extend: false, history: false });
    await this.app.moveTo(this.v, end, { center: o.center, extend: true, history: false });
  }
  // Columns of text the view shows across (its own width).
  get width() { return this.v.pane.geometry(this.v).width; }
  get visibleLines() { return this.v.pane.visibleLineRange(this.v); }
}

export class Host {
  constructor(app) {
    this.app = app;
    this.plain = new Map(); // event -> [() => …]
    this.bufferEvents = new Map(); // event -> [(buffer, {ops, local}) => …]
    this.viewEvents = new Map(); // event -> [(view) => …]
    this.statusHandlers = []; // [(status) => …]
    this.plugins = [];
  }

  // A handler that threw is reported; the others still run.
  failed(event, e) { this.app().report(`${event}: ${String(e)}`); }

  on(event, f) {
    const list = this.plain.get(event);
    if (list) list.push(f);
    else this.plain.set(event, [f]);
  }
  onBuffer(event, f) {
    const list = this.bufferEvents.get(event);
    if (list) list.push(f);
    else this.bufferEvents.set(event, [f]);
  }
  onView(event, f) {
    const list = this.viewEvents.get(event);
    if (list) list.push(f);
    else this.viewEvents.set(event, [f]);
  }
  onStatus(f) { this.statusHandlers.push(f); }

  emit(event) {
    for (const f of this.plain.get(event) || []) {
      try { f(); } catch (e) { this.failed(event, e); }
    }
  }
  // `ops`: what changed ("buffer.changed"), `local`: typed here.
  emitBuffer(event, b, ops, local) {
    for (const f of this.bufferEvents.get(event) || []) {
      try { f(b, { ops, local }); } catch (e) { this.failed(event, e); }
    }
  }
  emitView(event, v) {
    for (const f of this.viewEvents.get(event) || []) {
      try { f(v); } catch (e) { this.failed(event, e); }
    }
  }
  emitStatus(status) {
    for (const f of this.statusHandlers) {
      try { f(status); } catch (e) { this.failed("lsp.status", e); }
    }
  }

  // Activate `plugins` ([{name, activate}], see plugins/index.js) in order;
  // one that throws is reported and the rest still load.
  async load(plugins) {
    for (const { name, activate } of plugins) {
      try {
        await activate(this.app());
        this.plugins.push(name);
      } catch (e) {
        this.failed(`plugin ${name}`, e);
      }
    }
  }
}
