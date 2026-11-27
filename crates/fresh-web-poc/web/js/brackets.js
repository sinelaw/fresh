// Brackets: the ones on a line that count (outside its strings and
// comments, as the highlighter finds them), and each one's partner.

import { highlightLine } from "./highlight.js";
import { byteLen } from "./utf8.js";

// Each bracket's partner.
/** const OPEN: Map<String, String> */
export const OPEN = new Map([["(", ")"], ["[", "]"], ["{", "}"]]);
/** const CLOSE: Map<String, String> */
export const CLOSE = new Map([[")", "("], ["]", "["], ["}", "{"]]);

// How many lines a bracket's partner is looked for in.
const SCAN_LINES = 5000;
// How many lines' brackets are remembered (by language and text).
const CACHE_LINES = 100000;

// A bracket on a line: its UTF-16 index and character.
/** type Bracket = {k: Number, c: String} */
/** function bracket(k: Number, c: String) => Bracket */
function bracket(k, c) { return { k, c }; }

/** const cache: Map<String, Bracket[]> */
const cache = new Map();

// The brackets of `line` (in language `lang`) that count, in order.
/** function bracketsOf(lang: String | Null, line: String) => Bracket[] */
export function bracketsOf(lang, line) {
  const key = `${lang}\u0000${line}`;
  const hit = cache.get(key);
  if (hit !== undefined) return hit;
  const literal = highlightLine(lang, line).filter((x) => x.k === "syntax.string" || x.k === "syntax.comment");
  /** const out: Bracket[] */
  const out = [];
  for (let k = 0; k < line.length; k++) {
    const c = line[k];
    if ((OPEN.has(c) || CLOSE.has(c)) && !literal.some((x) => k >= x.s && k < x.e)) out.push(bracket(k, c));
  }
  if (cache.size >= CACHE_LINES) cache.clear();
  cache.set(key, out);
  return out;
}

// A line's bracket balance: its openers less its closers.
/** function balanceOf(lang: String | Null, line: String) => Number */
export function balanceOf(lang, line) {
  return bracketsOf(lang, line).reduce((n, x) => n + (OPEN.has(x.c) ? 1 : -1), 0);
}

// The lines held: their text and byte offsets.
/** type Held = {lines: String[], starts: Number[]} */

// The partner of the bracket at index `k` of line `li` (looked for
// forward from an opener, back from a closer, at most SCAN_LINES lines):
// its byte offset, or null (none, or that is no bracket that counts).
/** function partnerOf(lang: String | Null, L: Held, li: Number, k: Number) => Number | Null */
export function partnerOf(lang, L, li, k) {
  const at = bracketsOf(lang, L.lines[li]).find((x) => x.k === k);
  if (at === undefined) return null;
  const fwd = OPEN.has(at.c);
  const want = fwd ? OPEN.get(at.c) : CLOSE.get(at.c);
  let depth = 0;
  for (let n = 0, line = li; n < SCAN_LINES && line >= 0 && line < L.lines.length; n++, line += fwd ? 1 : -1) {
    const all = bracketsOf(lang, L.lines[line]);
    const here = line !== li ? all : all.filter((x) => (fwd ? x.k >= k : x.k <= k));
    for (const x of fwd ? here : [...here].reverse()) {
      if (x.c === at.c) depth++;
      else if (x.c === want && --depth === 0) return L.starts[line] + byteLen(L.lines[line].slice(0, x.k));
    }
  }
  return null;
}

// The unmatched opening bracket before index `k` of line `li` (looked
// for at most SCAN_LINES lines back): its byte offset, or null.
/** function openerBefore(lang: String | Null, L: Held, li: Number, k: Number) => Number | Null */
export function openerBefore(lang, L, li, k) {
  /** const depth: Map<String, Number> */
  const depth = new Map();
  for (let n = 0, line = li; n < SCAN_LINES && line >= 0; n++, line--) {
    const all = bracketsOf(lang, L.lines[line]);
    const here = line !== li ? all : all.filter((x) => x.k < k);
    for (const x of [...here].reverse()) {
      const opener = CLOSE.get(x.c);
      if (opener !== undefined) depth.set(opener, (depth.get(opener) ?? 0) + 1);
      else {
        const d = depth.get(x.c) ?? 0;
        if (d === 0) return L.starts[line] + byteLen(L.lines[line].slice(0, x.k));
        depth.set(x.c, d - 1);
      }
    }
  }
  return null;
}
