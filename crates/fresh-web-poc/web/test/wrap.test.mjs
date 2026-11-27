// Wrapping and the row index, against one oracle: a fresh wrap of the
// text as it is, from its line's start. Each test runs many seeded random
// cases; a failure names its seed.
//
// Run (after building the WebAssembly module, see support.mjs):
//   node --test crates/fresh-web-poc/web/test/

import { test, before } from "node:test";
import assert from "node:assert/strict";
import { wasmModule, random, pick, randomLine, laidOut, options } from "./support.mjs";
import { blockRows, measuring, ignoreMark, columnIn, byteAtColumn, layoutOf } from "../js/layout.js";
import { RowIndex, rowStart, rowsDown, rowsUp, LONG_LINE } from "../js/rows.js";
import { clustersOf, clustersIn, clustersAround, unitsOf, bytesOf } from "../js/clusters.js";
import { Lines, wholeText } from "../js/buffer.js";
import { Decor } from "../js/decor.js";
import { byteLen } from "../js/utf8.js";
import { insOp, delOp } from "../js/ot.js";

/** let w: Wasm */
let w;
before(async () => { w = await wasmModule(); });

const SEEDS = 40;

// The text rows of line `li` of layout `x`, wrapped fresh from its start.
function freshRows(x, li) {
  return [...blockRows(x, li, x.L.starts[li], 0, Infinity, ignoreMark)].filter((r) => r.kind === "text");
}
// The byte offsets of `text`'s cluster boundaries, its end included.
function boundaries(text) {
  const c = clustersOf(text);
  const out = [0];
  let b = 0;
  for (let k = 0; k < c.n; k++) { b += bytesOf(c, k); out.push(b); }
  return out;
}
// A seeded case: a line of random pieces, a width.
function lineCase(seed, pieces) {
  const rnd = random(seed);
  return { rnd, text: randomLine(rnd, pieces), width: 8 + Math.floor(rnd() * 40) };
}

test("rows cover each line exactly, break only between clusters, and fit the width", () => {
  for (let seed = 1; seed <= SEEDS; seed++) {
    const { text, width } = lineCase(seed, 200);
    const x = measuring(laidOut(w, text, true, width));
    const rows = freshRows(x, 0);
    const ok = new Set(boundaries(text));
    assert.equal(rows[0].start, 0, `seed ${seed}: the first row starts the line`);
    rows.forEach((r, k) => {
      const next = k + 1 < rows.length ? rows[k + 1].start : null;
      if (next !== null) assert.equal(r.end, next, `seed ${seed}: row ${k} ends where row ${k + 1} starts`);
      else assert.equal(r.eol, byteLen(text), `seed ${seed}: the last row ends the line`);
      assert.ok(ok.has(r.start), `seed ${seed}: row ${k} starts at byte ${r.start}, inside a cluster`);
      assert.ok(r.col <= width, `seed ${seed}: row ${k} is ${r.col} columns wide, past ${width}`);
      // Its cells are its source range's clusters, in order.
      const shown = r.cells.filter((c) => c.src !== null).map((c) => c.src);
      assert.deepEqual(shown, [...ok].filter((b) => b >= r.start && b < r.end), `seed ${seed}: row ${k} shows every cluster of its range once`);
    });
    // Joined, the rows' source ranges are the line (a wrap drops nothing).
    const joined = rows.map((r) => x.L.text(r.start ?? 0, r.end ?? 0)).join("");
    assert.equal(joined, text, `seed ${seed}`);
  }
});

test("rows walked from any wrap checkpoint equal a fresh wrap's from there", () => {
  for (let seed = 1; seed <= SEEDS / 4; seed++) {
    const { text, width } = lineCase(seed, 8000);
    const x = measuring(laidOut(w, text, true, width));
    assert.ok(byteLen(text) >= LONG_LINE, "the line is long enough to be indexed");
    const idx = new RowIndex();
    idx.use("k", true);
    const fresh = freshRows(x, 0).map((r) => r.start);
    const [e] = idx.of(x, 0);
    idx.reach(x, e, 0, Infinity);
    assert.ok(e.done, `seed ${seed}: the index reached the line's end`);
    for (const m of e.marks) {
      assert.equal(fresh[m.ord], m.at, `seed ${seed}: checkpoint of row ${m.ord}`);
      const walked = [...blockRows(x, 0, m.at, 0, Infinity, ignoreMark)].filter((r) => r.kind === "text").map((r) => r.start);
      assert.deepEqual(walked, fresh.slice(m.ord), `seed ${seed}: rows from row ${m.ord}`);
    }
  }
});

test("after random edits the repaired row index equals a fresh wrap", () => {
  for (let seed = 1; seed <= SEEDS / 2; seed++) {
    const rnd = random(seed);
    const width = 10 + Math.floor(rnd() * 30);
    // A long line with spaces, or one with no break opportunities.
    const spaced = rnd() < 0.7;
    let text = spaced ? randomLine(rnd, 6000) : "x".repeat(20000) + "你好".repeat(500);
    const L = new Lines(wholeText(text));
    const x = measuring(layoutOf(L, new Decor(w), options(true, width)));
    const idx = new RowIndex();
    idx.use("k", true);
    for (let step = 0; step < 12; step++) {
      // Index part of the line (or all of it), then edit it.
      for (const e of idx.of(x, 0)) idx.reach(x, e, 0, Math.floor(rnd() * L.lens[0]));
      const cuts = boundaries(text);
      const at = pick(rnd, cuts.slice(0, -1));
      const op = rnd() < 0.6
        ? insOp(at, pick(rnd, [" ", "word ", "é", "你", "\u{1F600}", "ab cd", "\t"]))
        : delOp(at, pick(rnd, cuts.filter((b) => b > at).slice(0, 4)) - at);
      const removed = op.t === "del" ? op.len : 0, inserted = op.t === "ins" ? op.text : "";
      L.splice(op);
      text = L.lines[0];
      idx.change({ kind: "edit", version: step, at, removed, text: inserted });
      x.decor.clear();
      const fresh = freshRows(x, 0).map((r) => r.start);
      for (const e of idx.of(x, 0)) {
        idx.reach(x, e, 0, Infinity);
        for (const m of e.marks) assert.equal(fresh[m.ord], m.at, `seed ${seed} step ${step}: checkpoint of row ${m.ord}`);
      }
      // The row holding any cluster boundary is the fresh wrap's.
      const now = boundaries(text);
      for (let k = 0; k < 20; k++) {
        const p = pick(rnd, now);
        const want = fresh.filter((s) => s <= p).at(-1);
        assert.equal(rowStart(x, idx, p), want, `seed ${seed} step ${step}: the row holding byte ${p}`);
      }
    }
  }
});

test("scrolling down n rows then up n returns to the same top byte", () => {
  for (let seed = 1; seed <= SEEDS; seed++) {
    const rnd = random(seed);
    const lines = [];
    for (let k = 0; k < 6 + Math.floor(rnd() * 10); k++) lines.push(rnd() < 0.15 ? randomLine(rnd, 4000) : randomLine(rnd, Math.floor(rnd() * 40)));
    const x = measuring(laidOut(w, lines.join("\n"), rnd() < 0.8, 10 + Math.floor(rnd() * 40)));
    const idx = new RowIndex();
    idx.use("k", x.o.wrap);
    let top = 0;
    for (let k = 0; k < 30; k++) {
      const n = 1 + Math.floor(rnd() * 50);
      const down = rowsDown(x, idx, top, n, false);
      if (down.moved < n) { top = down.top; continue; }
      const up = rowsUp(x, idx, down.top, n, false);
      assert.equal(up.moved, n, `seed ${seed}`);
      assert.equal(up.top, top, `seed ${seed}: down ${n} from ${top} to ${down.top}, then up ${n}`);
      top = down.top;
    }
  }
});

test("byte to cell and back is the identity on every cluster boundary", () => {
  for (let seed = 1; seed <= SEEDS; seed++) {
    const { text, width } = lineCase(seed, 150);
    for (const wrap of [true, false]) {
      const x = measuring(laidOut(w, text, wrap, width));
      const rows = freshRows(x, 0);
      for (const b of boundaries(text)) {
        const r = rows.filter((row) => (row.start ?? 0) <= b).at(-1);
        const col = columnIn(r, b);
        // A wrapped row's end is the next row's start: drawn there.
        assert.equal(byteAtColumn(r, col), b, `seed ${seed} wrap ${wrap}: byte ${b} at column ${col}`);
      }
    }
  }
});

// A text's cluster boundaries in UTF-16 units, its end included.
function unitBoundaries(text) {
  const c = clustersOf(text);
  const out = [0];
  let i = 0;
  for (let k = 0; k < c.n; k++) { i += unitsOf(c, k); out.push(i); }
  return out;
}

test("clusters read a slice at a time are the whole text's", () => {
  for (let seed = 1; seed <= SEEDS; seed++) {
    const rnd = random(seed);
    const text = randomLine(rnd, 120);
    const whole = unitBoundaries(text);
    assert.equal(whole.at(-1), text.length, `seed ${seed}: the clusters cover the text`);
    // From any boundary, slices of any size: the boundaries they find are
    // the whole text's.
    for (let k = 0; k < 20; k++) {
      let from = pick(rnd, whole);
      const size = 1 + Math.floor(rnd() * 12);
      while (from < text.length) {
        const c = clustersIn(text, from, from + size);
        assert.ok(c.n > 0, `seed ${seed}: a slice from ${from} reads a cluster`);
        for (let j = 0; j < c.n; j++) {
          from += unitsOf(c, j);
          assert.ok(whole.includes(from), `seed ${seed}: ${from} is no cluster boundary`);
        }
      }
    }
    // Around any code point boundary: the boundaries either side.
    for (let i = 0; i <= text.length; i++) {
      if (i > 0 && i < text.length && text.charCodeAt(i) >= 0xdc00 && text.charCodeAt(i) < 0xe000) continue;
      const a = clustersAround(text, i);
      assert.equal(a.on, whole.includes(i), `seed ${seed}: is ${i} a boundary`);
      assert.equal(a.prev, whole.filter((b) => b < i).at(-1) ?? i, `seed ${seed}: the boundary before ${i}`);
      assert.equal(a.next, whole.find((b) => b > i) ?? i, `seed ${seed}: the boundary after ${i}`);
    }
  }
});
