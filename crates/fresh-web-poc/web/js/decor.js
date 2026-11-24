// A buffer's decorations, held by this client: overlays, conceals, soft
// breaks, inline virtual text, virtual lines, plugin markers and
// diagnostics. Every position moves through every edit the buffer applies,
// local or remote, in log order (`shift`), so the set is always about the
// text it is drawn on.

import { shiftPos } from "./ot.js";

let nextHandle = 1;

export class Decor {
  constructor() {
    this.overlays = [];
    this.conceals = [];
    this.softBreaks = [];
    this.vtexts = new Map(); // id -> vt
    this.vlines = [];
    this.markers = new Map(); // key -> {start, end, payload}
    this.diags = [];
    this.gen = 0; // bumped on every change, for render caching
  }

  touch() { this.gen++; }

  shift(op) {
    const r = (p) => shiftPos(p, op, true);
    const l = (p) => shiftPos(p, op, false);
    const range = (o) => { o.start = r(o.start); o.end = Math.max(o.start, l(o.end)); };
    for (const o of this.overlays) range(o);
    for (const o of this.conceals) {
      range(o);
      if (o.scopeStart != null) { o.scopeStart = r(o.scopeStart); o.scopeEnd = Math.max(o.scopeStart, l(o.scopeEnd)); }
    }
    for (const o of this.softBreaks) {
      o.pos = r(o.pos);
      if (o.scopeStart != null) { o.scopeStart = r(o.scopeStart); o.scopeEnd = Math.max(o.scopeStart, l(o.scopeEnd)); }
    }
    for (const o of this.vtexts.values()) o.pos = o.before ? r(o.pos) : l(o.pos);
    for (const o of this.vlines) o.pos = l(o.pos);
    for (const o of this.markers.values()) range(o);
    for (const o of this.diags) range(o);
    this.gen++;
  }

  // ---- overlays
  addOverlay(ns, start, end, style) {
    const handle = `ov${nextHandle++}`;
    this.overlays.push({ handle, ns, start, end, style });
    this.gen++;
    return handle;
  }
  clearNamespace(ns) { this.overlays = this.overlays.filter((o) => o.ns !== ns); this.gen++; }
  clearOverlaysInRange(start, end, ns) {
    this.overlays = this.overlays.filter((o) => (ns != null && o.ns !== ns) || !(o.start < end && o.end > start) && !(o.start === o.end && o.start >= start && o.start < end));
    this.gen++;
  }
  removeOverlay(handle) { this.overlays = this.overlays.filter((o) => o.handle !== handle); this.gen++; }

  // ---- conceals
  addConceal(c) { this.conceals.push(c); this.gen++; }
  clearConcealNamespace(ns) { this.conceals = this.conceals.filter((c) => c.ns !== ns); this.gen++; }
  clearConcealsInRange(start, end) {
    this.conceals = this.conceals.filter((c) => !(c.start < end && c.end > start) && !(c.start === c.end && c.start >= start && c.start < end));
    this.gen++;
  }

  // ---- soft breaks
  addSoftBreak(b) { this.softBreaks.push(b); this.gen++; }
  clearSoftBreakNamespace(ns) { this.softBreaks = this.softBreaks.filter((b) => b.ns !== ns); this.gen++; }
  clearSoftBreaksInRange(start, end) { this.softBreaks = this.softBreaks.filter((b) => !(b.pos >= start && b.pos < end)); this.gen++; }

  // ---- inline virtual text
  addVirtualText(vt) { this.vtexts.set(vt.id, vt); this.gen++; }
  removeVirtualText(id) { this.vtexts.delete(id); this.gen++; }
  removeVirtualTextsByPrefix(prefix) {
    for (const id of [...this.vtexts.keys()]) if (id.startsWith(prefix)) this.vtexts.delete(id);
    this.gen++;
  }
  clearVirtualTextsInRange(prefix, start, end) {
    for (const [id, v] of [...this.vtexts]) if (id.startsWith(prefix) && v.pos >= start && v.pos < end) this.vtexts.delete(id);
    this.gen++;
  }
  clearVirtualTextNamespace(ns) {
    for (const [id, v] of [...this.vtexts]) if (v.ns === ns) this.vtexts.delete(id);
    this.vlines = this.vlines.filter((v) => v.ns !== ns);
    this.gen++;
  }
  clearVirtualTexts() { this.vtexts.clear(); this.vlines = []; this.gen++; }

  // ---- virtual lines
  addVirtualLine(v) { this.vlines.push(v); this.gen++; }
  clearVirtualLinesInRange(ns, start, end) {
    this.vlines = this.vlines.filter((v) => v.ns !== ns || !(v.pos >= start && v.pos < end));
    this.gen++;
  }
}
