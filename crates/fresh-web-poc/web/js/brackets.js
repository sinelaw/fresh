// Brackets: the ones on a line that count (outside its strings and
// comments, as the grammar engine coloured them: hl.js), and each one's
// partner.

import { byteLen } from "./utf8.js";

// Each bracket's partner.
/** const OPEN: Map<String, String> */
export const OPEN = new Map([["(", ")"], ["[", "]"], ["{", "}"]]);
/** const CLOSE: Map<String, String> */
export const CLOSE = new Map([[")", "("], ["]", "["], ["}", "{"]]);

// How many lines a bracket's partner is looked for in.
const SCAN_LINES = 5000;

// A bracket on a line: its UTF-16 index and character.
/** type Bracket = {k: Number, c: String} */
/** function bracket(k: Number, c: String) => Bracket */
function bracket(k, c) { return { k, c }; }

// The brackets of `line` outside the UTF-16 ranges `literal` (its strings
// and comments), in order.
/** function bracketsIn(line: String, literal: {s: Number, e: Number}[]) => Bracket[] */
export function bracketsIn(line, literal) {
  /** const out: Bracket[] */
  const out = [];
  let j = 0;
  for (let k = 0; k < line.length; k++) {
    while (j < literal.length && literal[j].e <= k) j++;
    const c = line[k];
    if ((OPEN.has(c) || CLOSE.has(c)) && !(j < literal.length && literal[j].s <= k)) out.push(bracket(k, c));
  }
  return out;
}

// A line's bracket balance: its openers less its closers.
/** function balanceOf(brackets: Bracket[]) => Number */
export function balanceOf(brackets) {
  return brackets.reduce((n, x) => n + (OPEN.has(x.c) ? 1 : -1), 0);
}

// The lines held: their text and byte offsets.
/** type Held = {lines: String[], starts: Number[]} */

// The partner of the bracket at index `k` of line `li` (looked for
// forward from an opener, back from a closer, at most SCAN_LINES lines;
// `at(li)` gives line `li`'s brackets): its byte offset, or null (none, or
// that is no bracket that counts).
/** function partnerOf(
 *   at: (li: Number) => Bracket[],
 *   L: Held,
 *   li: Number,
 *   k: Number,
 * ) => Number | Null */
export function partnerOf(at, L, li, k) {
  const here = at(li).find((x) => x.k === k);
  if (here === undefined) return null;
  const fwd = OPEN.has(here.c);
  const want = fwd ? OPEN.get(here.c) : CLOSE.get(here.c);
  let depth = 0;
  for (let n = 0, line = li; n < SCAN_LINES && line >= 0 && line < L.lines.length; n++, line += fwd ? 1 : -1) {
    const all = at(line);
    const on = line !== li ? all : all.filter((x) => (fwd ? x.k >= k : x.k <= k));
    for (const x of fwd ? on : [...on].reverse()) {
      if (x.c === here.c) depth++;
      else if (x.c === want && --depth === 0) return L.starts[line] + byteLen(L.lines[line].slice(0, x.k));
    }
  }
  return null;
}

// The unmatched opening bracket before index `k` of line `li` (looked
// for at most SCAN_LINES lines back): its byte offset, or null.
/** function openerBefore(
 *   at: (li: Number) => Bracket[],
 *   L: Held,
 *   li: Number,
 *   k: Number,
 * ) => Number | Null */
export function openerBefore(at, L, li, k) {
  /** const depth: Map<String, Number> */
  const depth = new Map();
  for (let n = 0, line = li; n < SCAN_LINES && line >= 0; n++, line--) {
    const all = at(line);
    const on = line !== li ? all : all.filter((x) => x.k < k);
    for (const x of [...on].reverse()) {
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
