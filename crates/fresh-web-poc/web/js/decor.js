// A buffer's decorations, held by this client. Kinds:
//   overlays  {start, end, style}                 styling over source text
//   conceals  {start, end, text, reveal}          source drawn as other text
//   inlines   {pos, text, side, edge}             text spliced into a row
//   lines     {pos, above, text | rule}           whole rows between lines
//   blocks    {start, end, indent, prefix}        hanging indent for wraps
//   gutters   {start, glyph, color}               marks in the gutter
// plus the language server's diagnostics. Each item belongs to a namespace
// (plugin and set). Every position moves through every edit the buffer
// applies, local or remote, in log order, so the decorations always describe
// the text they are drawn on.

import { shiftPos } from "./ot.js";

const KINDS = ["overlays", "conceals", "inlines", "lines", "blocks", "gutters"];

export class Decor {
  constructor() {
    for (const k of KINDS) this[k] = [];
    this.diags = [];
    this.gen = 0;
  }

  // The items of `kind` overlapping [start, end): an interval index (sorted
  // by start, with the running maximum end) built once per change of the
  // decorations and searched by bisection, so a lookup costs the items it
  // returns, not the size of the set.
  query(kind, start, end) {
    let ix = this.ix?.[kind];
    if (!ix || ix.gen !== this.gen) {
      const items = [...(kind === "diags" ? this.diags : this[kind])].sort((a, b) => a.start - b.start);
      const maxEnd = new Array(items.length);
      let m = -Infinity;
      items.forEach((d, i) => { m = Math.max(m, Math.max(d.end, d.start + 1)); maxEnd[i] = m; });
      ix = { gen: this.gen, items, maxEnd };
      (this.ix ||= {})[kind] = ix;
    }
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

  add(kind, item) {
    this[kind].push(item);
    this.gen++;
  }

  clearNamespace(ns) {
    for (const k of KINDS) this[k] = this[k].filter((d) => d.ns !== ns);
    this.gen++;
  }

  // Remove the namespace's items that start in [start, end).
  clearNamespaceRange(ns, start, end) {
    for (const k of KINDS) {
      this[k] = this[k].filter((d) => {
        if (d.ns !== ns) return true;
        const p = d.start ?? d.pos;
        return !(p >= start && p < end);
      });
    }
    this.gen++;
  }

  shift(op) {
    const r = (p) => shiftPos(p, op, true);
    const l = (p) => shiftPos(p, op, false);
    const range = (o) => { o.start = r(o.start); o.end = Math.max(o.start, l(o.end)); };
    for (const o of this.overlays) range(o);
    for (const o of this.conceals) { range(o); if (o.reveal) range(o.reveal); }
    for (const o of this.inlines) o.pos = o.side === "before" ? r(o.pos) : l(o.pos);
    for (const o of this.lines) o.pos = l(o.pos);
    for (const o of this.blocks) range(o);
    for (const o of this.gutters) { o.start = l(o.start); o.end = o.start; }
    for (const o of this.diags) range(o);
    this.gen++;
  }
}
