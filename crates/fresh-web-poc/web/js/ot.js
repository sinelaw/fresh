// Operational transform over byte offsets: the same rules as the server's
// `src/ot.rs`. A primitive is {t:"ins", pos, text} or {t:"del", pos, len};
// an edit is a sequence of them, each relative to the document after the
// ones before it. Ties between inserts at one position go to the one
// sequenced first.

export const enc = new TextEncoder();
/** const byteLen: (String) => Number */
export const byteLen = (s) => {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    if (c < 0x80) n += 1;
    else if (c < 0x800) n += 2;
    else if (c >= 0xd800 && c < 0xdc00) { n += 4; i++; }
    else n += 3;
  }
  return n;
};

/** function len(p: Op) => Number */
function len(p) { return p.t === "ins" ? byteLen(p.text) : p.len; }
/** const noop: (Op) => Boolean */
const noop = (p) => (p.t === "ins" ? p.text.length === 0 : p.len === 0);

// `a` made against the same document as `b`, moved to apply after `b`.
/** function transform(a: Op, b: Op, aFirst: Boolean) => Op[] */
export function transform(a, b, aFirst) {
  return moved(a, b, aFirst).filter((p) => !noop(p));
}

/** function moved(a: Op, b: Op, aFirst: Boolean) => Op[] */
function moved(a, b, aFirst) {
  if (a.t === "ins") {
    if (b.t === "ins") return a.pos < b.pos || (a.pos === b.pos && aFirst) ? [a] : [{ ...a, pos: a.pos + len(b) }];
    const p = a.pos <= b.pos ? a.pos : a.pos >= b.pos + b.len ? a.pos - b.len : b.pos;
    return [{ ...a, pos: p }];
  }
  if (b.t === "ins") {
    const bl = len(b);
    if (b.pos <= a.pos) return [{ ...a, pos: a.pos + bl }];
    if (b.pos >= a.pos + a.len) return [a];
    const before = b.pos - a.pos;
    return [{ t: "del", pos: a.pos, len: before }, { t: "del", pos: a.pos + bl, len: a.len - before }];
  }
  const a0 = a.pos, a1 = a.pos + a.len, b0 = b.pos, b1 = b.pos + b.len;
  if (a1 <= b0) return [a];
  if (a0 >= b1) return [{ ...a, pos: a0 - b.len }];
  const overlap = Math.min(a1, b1) - Math.max(a0, b0);
  return [{ t: "del", pos: Math.min(a0, b0), len: a.len - overlap }];
}

// Returns [a', b']: a' applies after b, b' after a.
/** function transformSeqs(a: Op[], b: Op[], aFirst: Boolean) => [Op[], Op[]] */
export function transformSeqs(a, b, aFirst) {
  if (a.length === 0 || b.length === 0) return [a, b];
  if (a.length === 1 && b.length === 1) return [transform(a[0], b[0], aFirst), transform(b[0], a[0], !aFirst)];
  if (a.length > 1) {
    const [a1, b1] = transformSeqs(a.slice(0, 1), b, aFirst);
    const [rest, b2] = transformSeqs(a.slice(1), b1, aFirst);
    return [a1.concat(rest), b2];
  }
  const [a1, b1] = transformSeqs(a, b.slice(0, 1), aFirst);
  const [a2, rest] = transformSeqs(a1, b.slice(1), aFirst);
  return [a2, b1.concat(rest)];
}

// Where position `p` goes through primitive `op`. `right` decides an insert
// exactly at `p`: true moves p past it.
/** function shiftPos(p: Number, op: Op, right: Boolean) => Number */
export function shiftPos(p, op, right) {
  if (op.t === "ins") {
    return p > op.pos || (p === op.pos && right) ? p + len(op) : p;
  }
  if (p >= op.pos + op.len) return p - op.len;
  if (p > op.pos) return op.pos;
  return p;
}
