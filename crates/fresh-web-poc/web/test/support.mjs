// What the client's tests share: the WebAssembly module (clusters and
// markers come from it), a seeded random generator, and text laid out
// the way a source view lays it out.
//
// The module is the one the server serves, built on its own with
//   cargo build -p fresh-highlight-wasm --profile wasm --target wasm32-unknown-unknown
// and found at target/wasm32-unknown-unknown/wasm/fresh_highlight_wasm.wasm
// under the workspace, or where FRESH_HIGHLIGHT_WASM says.

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { Wasm } from "../js/wasm.js";
import { useClusters } from "../js/clusters.js";
import { Lines, wholeText } from "../js/buffer.js";
import { Decor } from "../js/decor.js";
import { layoutOf } from "../js/layout.js";
import { EMPTY } from "../js/paint.js";

const workspace = fileURLToPath(new URL("../../../../", import.meta.url));

// The module, loaded once; clusters are measured by it from then on.
/** let loaded: Wasm[] */
let loaded = [];
export async function wasmModule() {
  for (const w of loaded) return w;
  const path = process.env.FRESH_HIGHLIGHT_WASM ?? `${workspace}target/wasm32-unknown-unknown/wasm/fresh_highlight_wasm.wasm`;
  const { instance, module } = await WebAssembly.instantiate(readFileSync(path), {});
  const w = new Wasm(instance.exports, module);
  useClusters(w);
  loaded = [w];
  return w;
}

// A generator of numbers in [0, 1) from `seed` (mulberry32): the same
// seed, the same run, so a failure names its seed and comes back.
export function random(seed) {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
// One of `xs`, by `rnd`.
export function pick(rnd, xs) { return xs[Math.floor(rnd() * xs.length)]; }

// Pieces text is made of: words, spaces, tabs and what moves widths and
// clusters (combining marks, wide characters, emoji sequences, flags,
// Hangul jamo, a CRLF's carriage return).
export const PIECES = [
  "a", "word", "lorem", "ipsum", "x", " ", " ", "  ", "\t", "é", "é", "ñ",
  "你", "好", "한", "각", "\u{1F468}‍\u{1F469}‍\u{1F467}", "\u{1F1EB}\u{1F1F7}",
  "\u{1F600}", "{", "}", "\"", ",", "0123456789",
];
// A random line of about `n` pieces.
export function randomLine(rnd, n) {
  let s = "";
  for (let k = 0; k < n; k++) s += pick(rnd, PIECES);
  return s;
}

// Layout options as a source view's, styles left out.
export function options(wrap, width) {
  return {
    mode: "source", wrap, width, tab: 4, cursors: [], folds: [], whitespace: false,
    styleAt: () => EMPTY, styleAtByte: () => EMPTY, extendAt: () => null,
  };
}
// `text` held whole, laid out at `width` (wrapping or not).
export function laidOut(w, text, wrap, width) {
  const L = new Lines(wholeText(text));
  const d = new Decor(w);
  return layoutOf(L, d, options(wrap, width));
}
