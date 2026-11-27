// A buffer's decorations, held by this client (each kind's fields are
// typed in types/fresh.d.js):
//   overlays  styling over source text
//   conceals  source drawn as other text
//   inlines   text spliced into a row
//   lines     whole rows between lines
//   nodes     document structure: what a compose view draws as headings,
//             lists, tables...
//   gutters   marks in the gutter
// plus the language server's diagnostics. Each item belongs to a namespace
// (plugin and set).
//
// Positions live in the buffer's marker list (markers.js), one marker per
// item (and one more for a conceal's reveal range); what else an item says
// is kept here by its marker's id. Every edit the buffer applies, local or
// remote, in log order, moves every marker in one call, so the decorations
// always describe the text they are drawn on; a lookup asks the list for a
// range and costs what it finds there.
//
// How each kind's positions move: a range's start goes after text inserted
// at it and its end before (the range keeps to the text it covers); inline
// text drawn before its character moves with that character, inline text
// drawn after stays; lines and gutter marks stay.

import { shiftPos } from "./ot.js";
import { found } from "./maybe.js";
import { Markers, LEFT, RIGHT } from "./markers.js";

// A range (`o`'s start and end) moved through `op` by the rule ranges here
// follow, for positions not yet in a marker list (diagnostics carried from
// the version they describe).
export function shiftRange(o, op) {
  o.start = shiftPos(o.start, op, true);
  o.end = Math.max(o.start, shiftPos(o.end, op, false));
}

// Whether a decoration placed at `p` starts in [start, end).
/** function startsIn(p: Placed, start: Number, end: Number) => Boolean */
function startsIn(p, start, end) { return p.start >= start && p.start < end; }

// No decorations yet, typed: each kind's items by marker id. An item keeps
// the positions it was placed at; it is read placed where its marker is now.
/** function noOverlays() => Map<Number, Overlay> */
function noOverlays() { return new Map(); }
/** function noConceals() => Map<Number, Conceal> */
function noConceals() { return new Map(); }
/** function noInlines() => Map<Number, InlineText> */
function noInlines() { return new Map(); }
/** function noLines() => Map<Number, VirtualLine> */
function noLines() { return new Map(); }
/** function noNodes() => Map<Number, DocNode> */
function noNodes() { return new Map(); }
/** function noGutters() => Map<Number, GutterMark> */
function noGutters() { return new Map(); }
/** function noDiagnostics() => Map<Number, Diagnostic> */
function noDiagnostics() { return new Map(); }
// A conceal's reveal range's marker, by the conceal's.
/** function noReveals() => Map<Number, Number> */
function noReveals() { return new Map(); }
// The items of each namespace, by marker id.
/** function noNamespaces() => Map<String, Set<Number>> */
function noNamespaces() { return new Map(); }

export class Decor {
  // Decorations whose markers live in module `wasm` (wasm.js).
  constructor(wasm) {
    this.markers = new Markers(wasm);
    this.overlays = noOverlays();
    this.conceals = noConceals();
    this.reveals = noReveals();
    this.inlines = noInlines();
    this.lines = noLines();
    this.nodes = noNodes();
    this.gutters = noGutters();
    this.diags = noDiagnostics();
    this.namespaces = noNamespaces();
    this.gen = 0; // bumped on every change
  }

  // Move every position through primitive `op` (ot.js).
  shift(op) {
    this.markers.shift(op);
    this.gen++;
  }

  // ---- lookups: the items of a kind touching [start, end) (an empty one
  // covers the byte at its position), by start, then by when they were
  // added; placed where their markers are now.

  /** method overlaysIn(start: Number, end: Number) => Overlay[] */
  overlaysIn(start, end) {
    return this.markers.query(start, end).flatMap((p) => found(this.overlays.get(p.id)).map((o) => ({ ...o, start: p.start, end: p.end })));
  }
  /** method concealsIn(start: Number, end: Number) => Conceal[] */
  concealsIn(start, end) {
    return this.markers.query(start, end).flatMap((p) => found(this.conceals.get(p.id)).map((c) => ({ ...c, start: p.start, end: p.end, reveal: this.revealOf(p.id) })));
  }
  /** method inlinesIn(start: Number, end: Number) => InlineText[] */
  inlinesIn(start, end) {
    return this.markers.query(start, end).flatMap((p) => found(this.inlines.get(p.id)).map((t) => ({ ...t, pos: p.start })));
  }
  /** method linesIn(start: Number, end: Number) => VirtualLine[] */
  linesIn(start, end) {
    return this.markers.query(start, end).flatMap((p) => found(this.lines.get(p.id)).map((l) => ({ ...l, pos: p.start })));
  }
  /** method nodesIn(start: Number, end: Number) => DocNode[] */
  nodesIn(start, end) {
    return this.markers.query(start, end).flatMap((p) => found(this.nodes.get(p.id)).map((n) => ({ ...n, start: p.start, end: p.end })));
  }
  /** method guttersIn(start: Number, end: Number) => GutterMark[] */
  guttersIn(start, end) {
    return this.markers.query(start, end).flatMap((p) => found(this.gutters.get(p.id)).map((g) => ({ ...g, start: p.start, end: p.start })));
  }
  /** method diagsIn(start: Number, end: Number) => Diagnostic[] */
  diagsIn(start, end) {
    return this.markers.query(start, end).flatMap((p) => found(this.diags.get(p.id)).map((d) => ({ ...d, start: p.start, end: p.end })));
  }

  // The gutter marks, the overlays the scrollbar marks (`mark`) and the
  // diagnostics in [start, end), found in one lookup.
  /** method marksIn(start: Number, end: Number) => {gutters: GutterMark[], overlays: Overlay[], diags: Diagnostic[]} */
  marksIn(start, end) {
    /** const out: {gutters: GutterMark[], overlays: Overlay[], diags: Diagnostic[]} */
    const out = { gutters: [], overlays: [], diags: [] };
    for (const p of this.markers.query(start, end)) {
      for (const g of found(this.gutters.get(p.id))) out.gutters.push({ ...g, start: p.start, end: p.start });
      for (const o of found(this.overlays.get(p.id))) if (o.mark) out.overlays.push({ ...o, start: p.start, end: p.end });
      for (const d of found(this.diags.get(p.id))) out.diags.push({ ...d, start: p.start, end: p.end });
    }
    return out;
  }

  // Conceal `id`'s reveal range as it is now (null: it has none).
  /** method revealOf(id: Number) => Range | Null */
  revealOf(id) {
    for (const r of found(this.reveals.get(id))) { const p = this.markers.get(r); return { start: p.start, end: p.end }; }
    return null;
  }

  // Namespace `ns`'s overlays and gutter marks, as they are now.
  /** method overlaysOf(ns: String) => Overlay[] */
  overlaysOf(ns) {
    return this.idsOf(ns).flatMap((id) => found(this.overlays.get(id)).map((o) => { const p = this.markers.get(id); return { ...o, start: p.start, end: p.end }; }));
  }
  /** method guttersOf(ns: String) => GutterMark[] */
  guttersOf(ns) {
    return this.idsOf(ns).flatMap((id) => found(this.gutters.get(id)).map((g) => { const p = this.markers.get(id); return { ...g, start: p.start, end: p.start }; }));
  }
  // Namespace `ns`'s items' markers, in the order they were added.
  /** method idsOf(ns: String) => Number[] */
  idsOf(ns) { return [...(this.namespaces.get(ns) ?? [])]; }

  // The severity of each diagnostic.
  /** method severities() => Number[] */
  severities() { return [...this.diags.values()].map((d) => d.severity); }

  // ---- changes

  // Item `id` joins namespace `ns`.
  joined(ns, id) {
    const ids = this.namespaces.get(ns) ?? new Set();
    ids.add(id);
    this.namespaces.set(ns, ids);
    this.gen++;
  }
  // A range's marker: start after text inserted at it, end before.
  range(start, end) { return this.markers.span(start, Math.max(start, end), RIGHT, LEFT); }

  addOverlay(o) { const id = this.range(o.start, o.end); this.overlays.set(id, o); this.joined(o.ns, id); }
  addConceal(c) {
    const id = this.range(c.start, c.end);
    this.conceals.set(id, c);
    const rv = c.reveal;
    if (rv !== null) this.reveals.set(id, this.range(rv.start, rv.end));
    this.joined(c.ns, id);
  }
  addInline(t) { const id = this.markers.point(t.pos, t.side === "before" ? RIGHT : LEFT); this.inlines.set(id, t); this.joined(t.ns, id); }
  addLine(l) { const id = this.markers.point(l.pos, LEFT); this.lines.set(id, l); this.joined(l.ns, id); }
  addNode(n) { const id = this.range(n.start, n.end); this.nodes.set(id, n); this.joined(n.ns, id); }
  addGutter(g) { const id = this.markers.point(g.start, LEFT); this.gutters.set(id, g); this.joined(g.ns, id); }

  // The language server's diagnostics, replacing the last.
  setDiagnostics(items) {
    for (const id of this.diags.keys()) this.markers.delete(id);
    this.diags = noDiagnostics();
    for (const d of items) this.diags.set(this.range(d.start, d.end), d);
    this.gen++;
  }

  // Remove item `id` of namespace `ns`, with its markers.
  remove(ns, id) {
    this.markers.delete(id);
    for (const r of found(this.reveals.get(id))) this.markers.delete(r);
    this.reveals.delete(id);
    this.overlays.delete(id);
    this.conceals.delete(id);
    this.inlines.delete(id);
    this.lines.delete(id);
    this.nodes.delete(id);
    this.gutters.delete(id);
    for (const ids of found(this.namespaces.get(ns))) ids.delete(id);
  }

  clearNamespace(ns) {
    for (const id of this.idsOf(ns)) this.remove(ns, id);
    this.namespaces.delete(ns);
    this.gen++;
  }

  // Remove the namespace's items that start in [start, end).
  clearNamespaceRange(ns, start, end) {
    const ids = this.namespaces.get(ns) ?? new Set();
    for (const p of this.markers.query(start, end)) if (ids.has(p.id) && startsIn(p, start, end)) this.remove(ns, p.id);
    this.gen++;
  }
}
