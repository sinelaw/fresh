// Text measured as the server measures it: in UTF-8 bytes. A code point
// is one UTF-16 unit, or a surrogate pair (4 bytes); a lone surrogate is
// written as U+FFFD (3 bytes), as TextEncoder writes it.

// Whether `s` has a surrogate pair at `i`.
/** function pairAt(s: String, i: Number) => Boolean */
function pairAt(s, i) {
  const c = s.charCodeAt(i);
  if (c < 0xd800 || c >= 0xdc00 || i + 1 >= s.length) return false;
  const d = s.charCodeAt(i + 1);
  return d >= 0xdc00 && d < 0xe000;
}
// The bytes of one unit that is not half of a pair.
/** function unitBytes(c: Number) => Number */
function unitBytes(c) { return c < 0x80 ? 1 : c < 0x800 ? 2 : 3; }

// The UTF-8 length of `s`.
/** function byteLen(s: String) => Number */
export function byteLen(s) {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    if (pairAt(s, i)) { n += 4; i++; }
    else n += unitBytes(s.charCodeAt(i));
  }
  return n;
}

// The UTF-16 index of byte offset `b` within `s`.
/** function byteToIdx(s: String, b: Number) => Number */
export function byteToIdx(s, b) {
  let n = 0, i = 0;
  while (i < s.length && n < b) {
    if (pairAt(s, i)) { n += 4; i += 2; }
    else { n += unitBytes(s.charCodeAt(i)); i++; }
  }
  return i;
}
