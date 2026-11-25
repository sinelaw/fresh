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
// (plugin and set). Every position moves through every edit the buffer
// applies, local or remote, in log order, so the decorations always describe
// the text they are drawn on.

import { shiftPos } from "./ot.js";

// An interval index of `items` (each with a start and an end): sorted by
// start, with the running maximum end, searched by bisection, so a lookup
// costs the items it returns, not the size of the set.
function indexOf(items) {
  const sorted = [...items].sort((a, b) => a.start - b.start);
  let m = -Infinity;
  const maxEnd = sorted.map((d) => { m = Math.max(m, Math.max(d.end, d.start + 1)); return m; });
  return { items: sorted, maxEnd };
}
// The indexed items overlapping [start, end).
function overlapping(ix, start, end) {
  const { items, maxEnd } = ix;
  // The last item starting before `end`.
  let lo = 0, hi = items.length;
  while (lo < hi) { const mid = (lo + hi) >> 1; if (items[mid].start < end) lo = mid + 1; else hi = mid; }
  const out = [];
  for (let i = lo - 1; i >= 0 && maxEnd[i] > start; i--) {
    const d = items[i];
    if (Math.max(d.end, d.start + 1) > start) out.push(d);
  }
  return out.reverse();
}

// No decorations yet, typed.
/** function noOverlays() => Overlay[] */
function noOverlays() { return []; }
/** function noConceals() => Conceal[] */
function noConceals() { return []; }
/** function noInlines() => InlineText[] */
function noInlines() { return []; }
/** function noLines() => VirtualLine[] */
function noLines() { return []; }
/** function noNodes() => DocNode[] */
function noNodes() { return []; }
/** function noGutters() => GutterMark[] */
function noGutters() { return []; }
/** function noDiagnostics() => Diagnostic[] */
function noDiagnostics() { return []; }

// The ranges conceals reveal.
/** function revealsOf(cs: Conceal[]) => Range[] */
function revealsOf(cs) { return cs.flatMap((c) => { const rv = c.reveal; return rv === null ? [] : [rv]; }); }

// Whether an item placed at `p` is in [start, end).
/** function startsIn(p: Number, start: Number, end: Number) => Boolean */
function startsIn(p, start, end) { return p >= start && p < end; }

export class Decor {
  constructor() {
    this.overlays = noOverlays();
    this.conceals = noConceals();
    this.inlines = noInlines();
    this.lines = noLines();
    this.nodes = noNodes();
    this.gutters = noGutters();
    this.diags = noDiagnostics();
    this.gen = 0; // bumped on every change
    // Each queried kind's index, as of a generation: a list of at most one.
    this.overlayIx = [];
    this.concealIx = [];
    this.nodeIx = [];
    this.diagIx = [];
  }

  // The items of a kind overlapping [start, end).
  overlaysIn(start, end) {
    const ix = this.overlayIx.find((x) => x.gen === this.gen) ?? { gen: this.gen, ix: indexOf(this.overlays) };
    this.overlayIx = [ix];
    return overlapping(ix.ix, start, end);
  }
  concealsIn(start, end) {
    const ix = this.concealIx.find((x) => x.gen === this.gen) ?? { gen: this.gen, ix: indexOf(this.conceals) };
    this.concealIx = [ix];
    return overlapping(ix.ix, start, end);
  }
  nodesIn(start, end) {
    const ix = this.nodeIx.find((x) => x.gen === this.gen) ?? { gen: this.gen, ix: indexOf(this.nodes) };
    this.nodeIx = [ix];
    return overlapping(ix.ix, start, end);
  }
  diagsIn(start, end) {
    const ix = this.diagIx.find((x) => x.gen === this.gen) ?? { gen: this.gen, ix: indexOf(this.diags) };
    this.diagIx = [ix];
    return overlapping(ix.ix, start, end);
  }

  // The inline texts at positions in [start, end).
  inlinesIn(start, end) { return this.inlines.filter((t) => startsIn(t.pos, start, end)); }

  addOverlay(o) { this.overlays = [...this.overlays, o]; this.gen++; }
  addConceal(c) { this.conceals = [...this.conceals, c]; this.gen++; }
  addInline(t) { this.inlines = [...this.inlines, t]; this.gen++; }
  addLine(l) { this.lines = [...this.lines, l]; this.gen++; }
  addNode(n) { this.nodes = [...this.nodes, n]; this.gen++; }
  addGutter(g) { this.gutters = [...this.gutters, g]; this.gen++; }
  // The language server's diagnostics, replacing the last.
  setDiagnostics(items) { this.diags = items; this.gen++; }

  clearNamespace(ns) {
    this.overlays = this.overlays.filter((d) => d.ns !== ns);
    this.conceals = this.conceals.filter((d) => d.ns !== ns);
    this.inlines = this.inlines.filter((d) => d.ns !== ns);
    this.lines = this.lines.filter((d) => d.ns !== ns);
    this.nodes = this.nodes.filter((d) => d.ns !== ns);
    this.gutters = this.gutters.filter((d) => d.ns !== ns);
    this.gen++;
  }

  // Remove the namespace's items that start in [start, end).
  clearNamespaceRange(ns, start, end) {
    this.overlays = this.overlays.filter((d) => d.ns !== ns || !startsIn(d.start, start, end));
    this.conceals = this.conceals.filter((d) => d.ns !== ns || !startsIn(d.start, start, end));
    this.inlines = this.inlines.filter((d) => d.ns !== ns || !startsIn(d.pos, start, end));
    this.lines = this.lines.filter((d) => d.ns !== ns || !startsIn(d.pos, start, end));
    this.nodes = this.nodes.filter((d) => d.ns !== ns || !startsIn(d.start, start, end));
    this.gutters = this.gutters.filter((d) => d.ns !== ns || !startsIn(d.start, start, end));
    this.gen++;
  }

  shift(op) {
    const r = (p) => shiftPos(p, op, true);
    const l = (p) => shiftPos(p, op, false);
    const range = (o) => { o.start = r(o.start); o.end = Math.max(o.start, l(o.end)); };
    for (const o of this.overlays) range(o);
    for (const o of this.conceals) range(o);
    for (const rv of revealsOf(this.conceals)) range(rv);
    for (const o of this.inlines) o.pos = o.side === "before" ? r(o.pos) : l(o.pos);
    for (const o of this.lines) o.pos = l(o.pos);
    for (const o of this.nodes) range(o);
    for (const o of this.gutters) { o.start = l(o.start); o.end = o.start; }
    for (const o of this.diags) range(o);
    this.gen++;
  }
}
