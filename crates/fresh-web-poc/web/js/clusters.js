// Grapheme clusters: what a person sees as one character (é, a flag, a
// family emoji, a Hangul syllable), and so what the caret steps over,
// what a row never breaks inside and what one cell is drawn for. Each has
// a width in columns. Both come from the WebAssembly module
// (crates/fresh-highlight-wasm, `uc_clusters`): the terminal editor's own
// measure (unicode-segmentation, unicode-width), so the browser and the
// terminal agree on every row break.
//
// Positions stay UTF-8 bytes; clusters are asked for a slice of a line's
// text at a time, never a whole long line on the keystroke.

import { listOf } from "./maybe.js";

const enc = new TextEncoder();

// The module clusters are measured by (main.js sets it once, before
// anything is drawn): a list of at most one.
/** let measurer: Wasm[] */
let measurer = [];
/** function useClusters(wasm: Wasm) => Undefined */
export function useClusters(wasm) { measurer = [wasm]; }
/** function moduleOf() => Wasm */
function moduleOf() {
  for (const w of measurer) return w;
  throw new Error("clusters are measured before the WebAssembly module is loaded");
}

// A text's clusters: `n` of them, each with its UTF-16 units, UTF-8 bytes
// and columns (`ascii`: every one is a printable ASCII character, one unit,
// one byte and one column, so `words` is empty).
/** type Clusters = {n: Number, ascii: Boolean, words: Uint32Array} */
/** function asciiClusters(n: Number) => Clusters */
function asciiClusters(n) { return { n, ascii: true, words: new Uint32Array(0) }; }
/** function measuredClusters(words: Uint32Array) => Clusters */
function measuredClusters(words) { return { n: words.length / 3, ascii: false, words }; }

// Printable ASCII: each character a cluster of one column (what the module
// answers for it, without the trip).
const PRINTABLE = /^[\x20-\x7e]*$/;

// The clusters of `text`, the last of which may continue past its end.
/** function clustersOf(text: String) => Clusters */
export function clustersOf(text) {
  if (PRINTABLE.test(text)) return asciiClusters(text.length);
  const w = moduleOf();
  // UTF-16 to UTF-8 is at most three bytes a unit.
  const cap = text.length * 3;
  const p = w.x.hl_alloc(cap) >>> 0;
  if (p === 0 && cap > 0) throw w.failed();
  try {
    const r = enc.encodeInto(text, w.bytesAt(p, cap));
    return measuredClusters(w.words(w.x.uc_clusters(p, r.written)));
  } finally { w.x.hl_free(p, cap); }
}
// Cluster `k`'s UTF-16 units, UTF-8 bytes and columns.
/** function unitsOf(c: Clusters, k: Number) => Number */
export function unitsOf(c, k) { return c.ascii ? 1 : c.words[3 * k + 1]; }
/** function bytesOf(c: Clusters, k: Number) => Number */
export function bytesOf(c, k) { return c.ascii ? 1 : c.words[3 * k]; }
/** function widthOf(c: Clusters, k: Number) => Number */
export function widthOf(c, k) { return c.ascii ? 1 : c.words[3 * k + 2]; }

// The clusters of `text` from UTF-16 index `from` (a cluster boundary) to
// about `to`: those that end before `to`, or at the text's end. The last
// one a slice reads may continue past it, so it is left for the next
// slice, which starts where it does (and reads further when one cluster
// is longer than the slice).
/** function clustersIn(text: String, from: Number, to: Number) => Clusters */
export function clustersIn(text, from, to) {
  for (let end = Math.min(text.length, to); ; end = Math.min(text.length, from + 2 * (end - from) + 16)) {
    if (end >= text.length) return clustersOf(text.slice(from));
    // A slice never ends between the halves of a surrogate pair.
    const cut = isHigh(text.charCodeAt(end - 1)) ? end - 1 : end;
    const c = clustersOf(text.slice(from, cut));
    if (c.n > 1) return c.ascii ? asciiClusters(c.n - 1) : measuredClusters(c.words.subarray(0, c.words.length - 3));
  }
}
/** function isHigh(u: Number) => Boolean */
function isHigh(u) { return u >= 0xd800 && u < 0xdc00; }

// The columns `text` takes, laid out from column 0 (a tab to the next stop
// of `tab`).
/** function textWidth(text: String) => Number */
export function textWidth(text) { return widthFrom(text, 0, 4); }
/** function widthFrom(text: String, col: Number, tab: Number) => Number */
export function widthFrom(text, col, tab) {
  const c = clustersOf(text);
  if (c.ascii && !text.includes("\t")) return text.length;
  let at = col, i = 0;
  for (let k = 0; k < c.n; k++) {
    at += text.charCodeAt(i) === 9 ? tab - (at % tab) : widthOf(c, k);
    i += unitsOf(c, k);
  }
  return at - col;
}

// How far a cluster boundary can be looked for on either side of a
// position, in UTF-16 units, when the line's start is farther: a cluster
// boundary depends only on the characters next to it, but for runs of
// regional indicators (flags pair up from the run's start), which are
// followed back to where they begin.
const NEAR = 128;

// The cluster boundaries nearest UTF-16 index `i` of `text` (a code point
// boundary) on either side: the last one before it and the first one after
// it (`i` itself at the text's start or end), and whether `i` is one. Where
// `i` is inside a cluster, they are that cluster's start and end.
/** type Around = {prev: Number, next: Number, on: Boolean} */
/** function clustersAround(text: String, i: Number) => Around */
export function clustersAround(text, i) {
  let from = Math.max(0, i - NEAR);
  while (from > 0 && isLow(text.charCodeAt(from))) from--;
  while (from > 0 && isRegional(text, from)) from -= 2;
  const c = clustersIn(text, from, Math.min(text.length, i + NEAR));
  let at = from, prev = from < i ? from : i;
  for (let k = 0; k < c.n; k++) {
    const next = at + unitsOf(c, k);
    if (at < i) prev = at;
    if (next > i) return { prev, next, on: at === i };
    at = next;
  }
  return { prev, next: at, on: at === i };
}
/** function isLow(u: Number) => Boolean */
function isLow(u) { return u >= 0xdc00 && u < 0xe000; }
// Whether a regional indicator (a flag's half) ends just before index `i`.
/** function isRegional(text: String, i: Number) => Boolean */
function isRegional(text, i) {
  const cp = i >= 2 ? text.codePointAt(i - 2) : undefined;
  return listOf(cp ?? null).some((x) => x >= 0x1f1e6 && x <= 0x1f1ff);
}
