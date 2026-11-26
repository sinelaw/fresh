// What plugins hold of the application (the API itself is api.js): handles
// on buffers and views, a buffer's decoration sets, and the plugins' event
// handlers, on four channels: plain events, buffer events (with the ops of a
// change), view events and the language servers' status.

import { insOp, delOp } from "./ot.js";

// A plugin's decorations in one buffer, cleared and replaced as a unit or by
// range (`ns` names the set). Positions are byte offsets; every one moves
// with edits. Each function is described with the DecorationSet type
// (types/fresh.d.js).
/** function asDecorations(d: DecorationSet) => DecorationSet */
function asDecorations(d) { return d; }
function decorationSet(buf, ns) {
  return asDecorations({
    clear: () => { buf.decor.clearNamespace(ns); },
    clearRange: (start, end) => { buf.decor.clearNamespaceRange(ns, start, end); },
    overlay: (start, end, style, opts) => { buf.decor.addOverlay({ ns, start, end, style, when: opts.when, mark: opts.mark, above: opts.above }); },
    conceal: (start, end, opts) => { buf.decor.addConceal({ ns, start, end, text: opts.text, style: opts.style, reveal: opts.reveal, when: opts.when }); },
    inline: (pos, text, opts) => { buf.decor.addInline({ ns, pos, text, style: opts.style, side: opts.side, edge: opts.edge, when: opts.when }); },
    line: (pos, opts) => { buf.decor.addLine({ ns, pos, above: opts.above, text: opts.text, style: opts.style, when: opts.when, order: opts.order }); },
    node: (start, end, block, when) => { buf.decor.addNode({ ns, start, end, type: block.type, level: block.level, depth: block.depth, marker: block.marker, lang: block.lang, align: block.align, header: block.header, delimiter: block.delimiter, when }); },
    gutter: (pos, opts) => { buf.decor.addGutter({ ns, start: pos, end: pos, glyph: opts.glyph, color: opts.color, title: opts.title }); },
    gutters: () => buf.decor.gutters.filter((g) => g.ns === ns).map((g) => ({ pos: g.start, glyph: g.glyph })),
    overlays: () => buf.decor.overlays.filter((o) => o.ns === ns).map((o) => ({ start: o.start, end: o.end })),
  });
}


// What a plugin holds of a buffer and of a view (types/fresh.d.js:
// BufferHandle, ViewHandle): functions over the application's own.
/** function asBuffer(h: BufferHandle) => BufferHandle */
function asBuffer(h) { return h; }
/** function asView(h: ViewHandle) => ViewHandle */
function asView(h) { return h; }

export function bufferHandle(app, buf) {
  const edit = (ops) => { app.applyEdit(buf, ops); };
  return asBuffer({
    id: buf.id,
    isVirtual: buf.isVirtual,
    path: () => buf.path,
    name: () => buf.name,
    language: () => buf.language,
    length: () => buf.len,
    modified: () => buf.modified,
    whole: () => buf.full,
    text: (start, end) => buf.textRange(start, end),
    textAsync: (start, end) => buf.textAsync(start, end),
    lines: () => { const L = buf.text; return L ? { lines: L.lines, starts: L.starts, firstLine: L.firstLine } : null; },
    edit,
    insert: (pos, text) => { edit([insOp(pos, text)]); },
    delete: (start, end) => { if (end > start) edit([delOp(start, end - start)]); },
    replace: (start, end, text) => { edit([...(end > start ? [delOp(start, end - start)] : []), ...(text ? [insOp(start, text)] : [])]); },
    save: () => app.saveBuffer(buf, true),
    decorations: (ns) => decorationSet(buf, ns),
    views: () => [...buf.views].map((v) => viewHandle(app, v)),
    setEntries: (entries) => { buf.setEntries(entries); app.renderBuffer(buf.id); },
    propsAt: (pos) => buf.propsAt(pos),
    setFoldRanges: (ranges) => { buf.foldRanges = ranges; },
    render: () => { app.renderBuffer(buf.id); },
  });
}

export function viewHandle(app, v) {
  const options = () => { app.renderBuffer(v.buf.id); };
  return asView({
    buffer: bufferHandle(app, v.buf),
    cursor: () => v.cursors[0].pos,
    selection: () => v.selection(),
    mode: () => v.mode,
    composeWidth: () => v.opts.composeWidth,
    setMode: (mode) => { v.mode = mode; options(); },
    setComposeWidth: (n) => { v.opts.composeWidth = n; options(); },
    // Both resolve once the text there is loaded and the view shows it.
    setCursor: (pos, o) => app.moveTo(v, pos, { center: o.center, extend: false, history: false }),
    // Select [start, end), the caret at `end`.
    select: async (start, end, o) => {
      await app.moveTo(v, start, { center: false, extend: false, history: false });
      await app.moveTo(v, end, { center: o.center, extend: true, history: false });
    },
  });
}

// No handlers yet, typed.
/** function noPlain() => Map<String, (() => Undefined)[]> */
function noPlain() { return new Map(); }
/** function noBufferHandlers() => Map<String, ((BufferHandle, BufferChange) => Undefined)[]> */
function noBufferHandlers() { return new Map(); }
/** function noViewHandlers() => Map<String, ((ViewHandle) => Undefined)[]> */
function noViewHandlers() { return new Map(); }
/** function noStatusHandlers() => ((Dict<Boolean>) => Undefined)[] */
function noStatusHandlers() { return []; }

export class Host {
  // `report(text)`: a failure for the user (a handler that threw).
  constructor(report) {
    this.report = report;
    this.plain = noPlain();
    this.bufferEvents = noBufferHandlers();
    this.viewEvents = noViewHandlers();
    this.statusHandlers = noStatusHandlers();
  }

  // A handler that threw is reported; the others still run.
  failed(event, what) { this.report(`${event}: ${what}`); }

  on(event, f) { this.plain.set(event, [...(this.plain.get(event) ?? []), f]); }
  onBuffer(event, f) { this.bufferEvents.set(event, [...(this.bufferEvents.get(event) ?? []), f]); }
  onView(event, f) { this.viewEvents.set(event, [...(this.viewEvents.get(event) ?? []), f]); }
  onStatus(f) { this.statusHandlers = [...this.statusHandlers, f]; }

  emit(event) {
    for (const f of this.plain.get(event) ?? []) {
      try { f(); } catch (e) { this.failed(event, String(e)); }
    }
  }
  // A buffer's event (`change`: what changed, for "buffer.changed").
  emitBuffer(event, b, change) {
    for (const f of this.bufferEvents.get(event) ?? []) {
      try { f(b, change); } catch (e) { this.failed(event, String(e)); }
    }
  }
  emitView(event, v) {
    for (const f of this.viewEvents.get(event) ?? []) {
      try { f(v); } catch (e) { this.failed(event, String(e)); }
    }
  }
  emitStatus(status) {
    for (const f of this.statusHandlers) {
      try { f(status); } catch (e) { this.failed("lsp.status", String(e)); }
    }
  }
}
