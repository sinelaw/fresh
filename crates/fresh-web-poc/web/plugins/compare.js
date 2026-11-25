// Compare files: two files side by side, both editable, differences marked
// in both and kept current as either is edited. Or a file against its last
// commit.

import { changeBlocks } from "../js/diff.js";
import * as fresh from "../js/api.js";

export function activate(app) {
  let pair = null; // {a, b, blocks, unlink}
  // A line of one side to the other's, past the changed blocks before it
  // (a line in a changed block goes to the start of its counterpart).
  const across = (line, from, to, len) => {
    let off = 0;
    for (const k of pair ? pair.blocks : []) {
      if (line < k[from]) break;
      if (line < k[from] + k[len.from]) return k[to];
      off = k[to] + k[len.to] - (k[from] + k[len.from]);
    }
    return line + off;
  };
  // A whole buffer's lines, without the empty one after a final newline.
  const held = (L) => (L.lines[L.lines.length - 1] === "" ? L.lines.slice(0, -1) : L.lines);
  const markLines = (d, L, first, n, bg, glyph, color) => {
    for (let i = first; i < first + n && i < L.starts.length; i++) {
      d.overlay(L.starts[i], L.starts[i] + fresh.byteLen(L.lines[i]) + 1, { fg: null, bg, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: true }, { when: "always", mark: false, above: false });
      d.gutter(L.starts[i], { glyph, color, title: "" });
    }
  };
  const mark = () => {
    if (!pair) return;
    const { a, b } = pair;
    const A = a.lines(), B = b.lines();
    if (!A || !B) return;
    const da = a.decorations("compare"), db = b.decorations("compare");
    da.clear(); db.clear();
    const blocks = changeBlocks(held(A), held(B));
    pair.blocks = blocks || [];
    for (const k of pair.blocks) {
      markLines(da, A, k.a, k.del, "editor.diff_remove_bg", "-", "git.deleted");
      markLines(db, B, k.b, k.ins, "editor.diff_add_bg", "+", "git.added");
    }
    a.render(); b.render();
    const n = pair.blocks.length;
    fresh.message(app, blocks ? `${n} difference${n === 1 ? "" : "s"}` : "Too many differences to compare");
  };
  const open = async (pathA, bufB) => {
    const a = typeof pathA === "string" ? await fresh.load(app, pathA) : pathA;
    const b = typeof bufB === "string" ? await fresh.load(app, bufB) : bufB;
    fresh.show(app, a, "tab");
    fresh.show(app, b, "right");
    pair?.unlink();
    // The two sides scroll together, line for line past the differences.
    const unlink = fresh.linkScroll(app, a, b,
      (line) => across(line, "a", "b", { from: "del", to: "ins" }),
      (line) => across(line, "b", "a", { from: "ins", to: "del" }));
    pair = { a, b, blocks: [], unlink };
    mark();
  };
  const compareFiles = async () => {
    const files = await fresh.findFiles(app, "");
    const pick = (title) => new Promise((resolve) => fresh.pick(app, { title, items: files.map((f) => ({ label: f, value: f })), onAccept: (it) => resolve(it.value), onCancel: () => resolve(null) }));
    const a = await pick("Compare: first file");
    if (!a) return;
    const b = await pick("Compare: second file");
    if (b) open(a, b);
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
    if (!pair || !v) return;
    const side = v.buffer.id === pair.a.id ? "a" : v.buffer.id === pair.b.id ? "b" : null;
    if (!side) return fresh.message(app, "Not a compared file");
    if (!pair.blocks.length) return fresh.message(app, "No differences");
    const L = v.buffer.lines();
    const cur = L.starts.findIndex((s, k) => v.cursor >= s && (k + 1 >= L.starts.length || v.cursor < L.starts[k + 1]));
    const lines = pair.blocks.map((k) => k[side]);
    const line = dir > 0 ? lines.find((i) => i > cur) ?? lines[0] : [...lines].reverse().find((i) => i < cur) ?? lines[lines.length - 1];
    v.setCursor(L.starts[Math.min(line, L.starts.length - 1)], { center: true });
  };
  let t = null;
  fresh.onBuffer(app, "buffer.changed", (buffer) => {
    if (pair && (buffer.id === pair.a.id || buffer.id === pair.b.id)) { clearTimeout(t); t = setTimeout(mark, 200); }
  });
  fresh.register(app, "compare.files", { title: "Compare Files…", hidden: false, run: compareFiles });
  fresh.register(app, "compare.head", { title: "Compare with Last Commit", hidden: false, run: compareHead });
  fresh.register(app, "compare.next", { title: "Compare: Next Difference", hidden: false, run: () => next(1) });
  fresh.register(app, "compare.prev", { title: "Compare: Previous Difference", hidden: false, run: () => next(-1) });
  fresh.bindKey(app, "F7", "compare.next");
  fresh.bindKey(app, "S-F7", "compare.prev");
}
