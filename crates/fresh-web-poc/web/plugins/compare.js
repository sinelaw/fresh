// Compare files: two files side by side, both editable, differences marked
// in both and kept current as either is edited. Or a file against its last
// commit.

import { changeBlocks } from "../js/diff.js";
import * as fresh from "../js/api.js";

// A changed block (diff.js): `del` lines of a from line `a` replaced by
// `ins` lines of b from line `b`.
/** type Change = {a: Number, b: Number, del: Number, ins: Number} */
// One side of the comparison as the blocks see it: where a block starts on
// it and how many of its lines the block holds.
/** type Side = {start: (Change) => Number, size: (Change) => Number} */
/** const SIDE_A: Side */
const SIDE_A = { start: (k) => k.a, size: (k) => k.del };
/** const SIDE_B: Side */
const SIDE_B = { start: (k) => k.b, size: (k) => k.ins };
// The two buffers compared (by id), their changed blocks, and how to stop
// their scrolling together.
/** type Pair = {a: Number, b: Number, blocks: Change[], unlink: () => Undefined} */

// A line of side `from` to side `to`, past the changed blocks before it (a
// line in a changed block goes to the start of its counterpart).
/** function across(blocks: Change[], line: Number, from: Side, to: Side) => Number */
function across(blocks, line, from, to) {
  let off = 0;
  for (const k of blocks) {
    if (line < from.start(k)) break;
    if (line < from.start(k) + from.size(k)) return to.start(k);
    off = to.start(k) + to.size(k) - (from.start(k) + from.size(k));
  }
  return line + off;
}

// A whole buffer's lines, without the empty one after a final newline.
/** function held(L: Lines) => String[] */
function held(L) { return L.lines[L.lines.length - 1] === "" ? L.lines.slice(0, -1) : L.lines; }

export function activate(app) {
  /** let pair: Pair | Null */
  let pair = null;
  const markLines = (d, L, first, n, bg, glyph, color) => {
    for (let i = first; i < first + n && i < L.starts.length; i++) {
      d.overlay(L.starts[i], L.starts[i] + fresh.byteLen(L.lines[i]) + 1, { fg: null, bg, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: true }, { when: "always", mark: false, above: false });
      d.gutter(L.starts[i], { glyph, color, title: "" });
    }
  };
  const mark = () => {
    const now = pair;
    const a = now ? fresh.bufferById(app, now.a) : null, b = now ? fresh.bufferById(app, now.b) : null;
    const A = a ? a.lines() : null, B = b ? b.lines() : null;
    if (!now || !a || !b || !A || !B) return;
    const da = a.decorations("compare"), db = b.decorations("compare");
    da.clear(); db.clear();
    const found = changeBlocks(held(A), held(B));
    const blocks = found || [];
    pair = { a: now.a, b: now.b, blocks, unlink: now.unlink };
    for (const k of blocks) {
      markLines(da, A, k.a, k.del, "editor.diff_remove_bg", "-", "git.deleted");
      markLines(db, B, k.b, k.ins, "editor.diff_add_bg", "+", "git.added");
    }
    a.render(); b.render();
    const n = blocks.length;
    fresh.message(app, found ? `${n} difference${n === 1 ? "" : "s"}` : "Too many differences to compare");
  };
  const blocksNow = () => { const now = pair; return now ? now.blocks : []; };
  const open = (a, b) => {
    fresh.show(app, a, "tab");
    fresh.show(app, b, "right");
    const before = pair;
    if (before) before.unlink();
    // The two sides scroll together, line for line past the differences.
    const unlink = fresh.linkScroll(app, a, b,
      (line) => across(blocksNow(), line, SIDE_A, SIDE_B),
      (line) => across(blocksNow(), line, SIDE_B, SIDE_A));
    pair = { a: a.id, b: b.id, blocks: [], unlink };
    mark();
  };
  const compareFiles = async () => {
    const files = await fresh.findFiles(app, "");
    const pick = (title) => new Promise((resolve) => fresh.pick(app, { title, items: files.map((f) => ({ label: f, value: f })), onAccept: (it) => resolve(it.value), onCancel: () => resolve(null) }));
    const a = await pick("Compare: first file");
    if (!a) return;
    const b = await pick("Compare: second file");
    const A = await fresh.load(app, a), B = b ? await fresh.load(app, b) : null;
    if (A && B) open(A, B);
  };
  const compareHead = async () => {
    const cur = fresh.activeBuffer(app);
    if (!cur?.path) return;
    const rel = cur.path.slice(fresh.root(app).length + 1);
    const r = await fresh.runProcess(app, "git", ["show", `HEAD:./${rel}`], { cwd: fresh.root(app) });
    if (r.code !== 0) return fresh.message(app, "Not in the last commit");
    const head = fresh.createVirtual(app, {
      name: `${cur.name} (HEAD)`, entries: [{ text: r.stdout, props: {}, style: null }],
      mode: null, readOnly: true, language: cur.language, keep: false, place: null, lineNumbers: true,
    });
    open(head, cur);
  };
  // The next (or previous) changed block, on whichever side is active.
  const next = (dir) => {
    const v = fresh.activeView(app);
    const now = pair;
    if (!now || !v) return;
    const id = v.buffer.id;
    const side = id === now.a ? SIDE_A : id === now.b ? SIDE_B : null;
    if (!side) return fresh.message(app, "Not a compared file");
    if (!now.blocks.length) return fresh.message(app, "No differences");
    const L = v.buffer.lines();
    if (!L) return;
    const cur = L.starts.findIndex((s, k) => v.cursor >= s && (k + 1 >= L.starts.length || v.cursor < L.starts[k + 1]));
    const lines = now.blocks.map(side.start);
    const line = dir > 0 ? lines.find((i) => i > cur) ?? lines[0] : [...lines].reverse().find((i) => i < cur) ?? lines[lines.length - 1];
    v.setCursor(L.starts[Math.min(line, L.starts.length - 1)], { center: true });
  };
  // Marked again a moment after the edits stop.
  /** let pending: Number | Null */
  let pending = null;
  fresh.onBuffer(app, "buffer.changed", (buffer) => {
    const now = pair;
    if (!now || (buffer.id !== now.a && buffer.id !== now.b)) return;
    const before = pending;
    if (before !== null) clearTimeout(before);
    pending = setTimeout(() => { pending = null; mark(); }, 200);
  });
  fresh.register(app, "compare.files", { title: "Compare Files…", hidden: false, run: compareFiles });
  fresh.register(app, "compare.head", { title: "Compare with Last Commit", hidden: false, run: compareHead });
  fresh.register(app, "compare.next", { title: "Compare: Next Difference", hidden: false, run: () => { next(1); } });
  fresh.register(app, "compare.prev", { title: "Compare: Previous Difference", hidden: false, run: () => { next(-1); } });
  fresh.bindKey(app, "F7", "compare.next");
  fresh.bindKey(app, "S-F7", "compare.prev");
}
