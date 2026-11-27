// Held text: UTF-16 indices and UTF-8 bytes through a long line's offset
// checkpoints, kept through edits; windows cut inside a huge line. The
// oracle is the text as one string, measured from its start.
//
// Run: node --test crates/fresh-web-poc/web/test/

import { test } from "node:test";
import assert from "node:assert/strict";
import { random, pick, PIECES } from "./support.mjs";
import { Lines, wholeText } from "../js/buffer.js";
import { byteLen } from "../js/utf8.js";
import { insOp, delOp } from "../js/ot.js";

const SEEDS = 30;

// A random text of `n` pieces, with a newline now and then (`lines`).
function randomText(rnd, n, lines) {
  let s = "";
  for (let k = 0; k < n; k++) s += lines && rnd() < 0.002 ? "\n" : pick(rnd, PIECES);
  return s;
}
// The code point boundaries of `s` in UTF-16 units, with their bytes.
function codePointBoundaries(s) {
  const out = [{ idx: 0, b: 0 }];
  let b = 0;
  for (let i = 0; i < s.length;) {
    const cp = s.codePointAt(i) ?? 0;
    b += byteLen(String.fromCodePoint(cp));
    i += cp > 0xffff ? 2 : 1;
    out.push({ idx: i, b });
  }
  return out;
}
// Every line of `L` converts each code point boundary both ways, and
// refuses an index or byte inside a character.
function assertOffsets(L, seed) {
  L.lines.forEach((s, i) => {
    const bs = codePointBoundaries(s);
    const bytes = new Set(bs.map((x) => x.b)), idxs = new Set(bs.map((x) => x.idx));
    for (const { idx, b } of bs) {
      assert.equal(L.byteAt(i, idx), b, `seed ${seed} line ${i}: byte of index ${idx}`);
      assert.equal(L.idxAt(i, b), idx, `seed ${seed} line ${i}: index of byte ${b}`);
    }
    for (let b = 0; b <= L.lens[i]; b += 7) if (!bytes.has(b)) assert.throws(() => L.idxAt(i, b), `seed ${seed}: byte ${b} is inside a character`);
    for (let idx = 0; idx <= s.length; idx += 5) if (!idxs.has(idx)) assert.throws(() => L.byteAt(i, idx), `seed ${seed}: index ${idx} is inside a character`);
  });
}

test("offsets convert both ways on every code point boundary of long lines", () => {
  for (let seed = 1; seed <= SEEDS; seed++) {
    const rnd = random(seed);
    const L = new Lines(wholeText(randomText(rnd, 3000, true)));
    assertOffsets(L, seed);
  }
});

test("offsets and text stay right through random edits", () => {
  for (let seed = 1; seed <= SEEDS; seed++) {
    const rnd = random(seed);
    let text = randomText(rnd, 2500, true);
    const L = new Lines(wholeText(text));
    // Convert once, so the long lines have their checkpoints to keep.
    assertOffsets(L, seed);
    for (let step = 0; step < 25; step++) {
      const bs = codePointBoundaries(text);
      const at = pick(rnd, bs);
      const op = rnd() < 0.6
        ? insOp(at.b, pick(rnd, [...PIECES, "\n", "ab\ncd"]))
        : delOp(at.b, pick(rnd, bs.filter((x) => x.b > at.b).slice(0, 6).map((x) => x.b - at.b).concat([0])));
      L.splice(op);
      text = op.t === "ins" ? text.slice(0, at.idx) + op.text + text.slice(at.idx) : text.slice(0, at.idx) + text.slice((bs.find((x) => x.b === at.b + op.len) ?? at).idx);
      assert.equal(L.lines.join("\n"), text, `seed ${seed} step ${step}: the held text`);
      assert.deepEqual(L.lens, L.lines.map((s) => byteLen(s)), `seed ${seed} step ${step}: line lengths`);
    }
    assertOffsets(L, seed);
  }
});

test("a window cut inside a line holds no newline at its cut ends", () => {
  const text = "x".repeat(50) + "\n" + "y".repeat(30) + "\n" + "z".repeat(40);
  // Bytes [20, 100): inside the first line to inside the third.
  const w = { start: 20, text: text.slice(20, 100), bof: false, eof: false, firstLine: 0, headCut: true, tailCut: true };
  const L = new Lines(w);
  assert.equal(L.count, 3);
  assert.equal(L.startsLine(0), false);
  assert.equal(L.startsLine(1), true);
  assert.equal(L.hasNewline(1), true);
  assert.equal(L.hasNewline(2), false, "the last held line goes on past the window");
  assert.equal(L.heldEnd(), 100);
  assert.equal(L.indexOf(100), 2);
  assert.equal(L.indexOf(101), -1);
  assert.equal(L.text(45, 85), text.slice(45, 85));
  // An edit at the cut end goes on the held part.
  L.splice(insOp(100, "!!"));
  assert.equal(L.lines[2], "z".repeat(18) + "!!");
});
