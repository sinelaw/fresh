// A buffer's marker list: positions that follow its edits, kept in the
// WebAssembly module (wasm.js; crates/fresh-highlight-wasm/src/markers.rs)
// in an interval tree, so an edit costs O(log n) and a range query
// O(log n + k) rather than a pass over every position.
//
// A marker is a point or a span; each end has a gravity, which decides an
// insertion exactly there the way ot.js `shiftPos(p, op, right)` does:
// RIGHT moves the end past the inserted text, LEFT leaves it before. A
// deletion moves a position inside it to where it starts, and an end never
// precedes its start. Positions are byte offsets.

import { byteLen } from "./utf8.js";

/** const LEFT: Number */
export const LEFT = 0;
/** const RIGHT: Number */
export const RIGHT = 1;

// A marker where it is now.
/** type Placed = {id: Number, start: Number, end: Number} */

// Answer words `w` as Placed, three words each.
/** function placedOf(w: Uint32Array) => Placed[] */
function placedOf(w) {
  /** const out: Placed[] */
  const out = [];
  for (let i = 0; i < w.length; i += 3) out.push({ id: w[i], start: w[i + 1], end: w[i + 2] });
  return out;
}

// A list goes with the Markers that made it: once that is collected, the
// module lets the list go (`drop` is a list's mk_drop).
/** const collected: FinalizationRegistry<() => Undefined> */
const collected = new FinalizationRegistry((drop) => { drop(); });

export class Markers {
  // A new, empty list in module `wasm` (a Wasm).
  constructor(wasm) {
    this.wasm = wasm;
    const list = wasm.words(wasm.x.mk_new())[0];
    this.list = list;
    collected.register(this, () => { wasm.check(wasm.x.mk_drop(list)); });
  }
  // A point at `pos`; returns its id.
  point(pos, gravity) { return this.wasm.words(this.wasm.x.mk_point(this.list, pos, gravity))[0]; }
  // A span over [start, end); returns its id.
  span(start, end, startGravity, endGravity) {
    return this.wasm.words(this.wasm.x.mk_span(this.list, start, end, startGravity, endGravity))[0];
  }
  delete(id) { this.wasm.check(this.wasm.x.mk_delete(this.list, id)); }
  // Move every marker through primitive `op` (ot.js).
  shift(op) {
    if (op.t === "ins") this.wasm.check(this.wasm.x.mk_edit(this.list, op.pos, 0, byteLen(op.text)));
    else this.wasm.check(this.wasm.x.mk_edit(this.list, op.pos, op.len, 0));
  }
  /** method get(id: Number) => Placed */
  get(id) { return placedOf(this.wasm.words(this.wasm.x.mk_get(this.list, id)))[0]; }
  // The markers touching [start, end) (an empty one covers the byte at its
  // position), by start, then by creation.
  /** method query(start: Number, end: Number) => Placed[] */
  query(start, end) { return placedOf(this.wasm.words(this.wasm.x.mk_query(this.list, start, end))); }
}
