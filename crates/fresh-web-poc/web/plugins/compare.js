// Compare files: two files side by side, both editable, differences marked
// in both and kept current as either is edited. Or a file against its last
// commit.

import { changeBlocks } from "../js/diff.js";

export function activate(fresh) {
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
      d.overlay(L.starts[i], L.starts[i] + fresh.text.byteLen(L.lines[i]) + 1, { fg: null, bg, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: true }, { when: "always", mark: false, above: false });
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
    fresh.ui.message(blocks ? `${n} difference${n === 1 ? "" : "s"}` : "Too many differences to compare");
  };
  const open = async (pathA, bufB) => {
    const a = typeof pathA === "string" ? await fresh.buffers.load(pathA) : pathA;
    const b = typeof bufB === "string" ? await fresh.buffers.load(bufB) : bufB;
    fresh.buffers.show(a, "tab");
    fresh.buffers.show(b, "right");
    pair?.unlink();
    // The two sides scroll together, line for line past the differences.
    const unlink = fresh.views.linkScroll(a, b,
      (line) => across(line, "a", "b", { from: "del", to: "ins" }),
      (line) => across(line, "b", "a", { from: "ins", to: "del" }));
    pair = { a, b, blocks: [], unlink };
    mark();
  };
  const compareFiles = async () => {
    const files = await fresh.host.find("");
    const pick = (title) => new Promise((resolve) => fresh.ui.pick({ title, items: files.map((f) => ({ label: f, value: f })), onAccept: (it) => resolve(it.value), onCancel: () => resolve(null) }));
    const a = await pick("Compare: first file");
    if (!a) return;
    const b = await pick("Compare: second file");
    if (b) open(a, b);
  };
  const compareHead = async () => {
    const cur = fresh.buffers.active();
    if (!cur?.path) return;
    const rel = cur.path.slice(fresh.workspace.root.length + 1);
    const r = await fresh.host.run("git", ["show", `HEAD:./${rel}`], { cwd: fresh.workspace.root });
    if (r.code !== 0) return fresh.ui.message("Not in the last commit");
    const head = fresh.buffers.createVirtual({ name: `${cur.name} (HEAD)`, entries: [{ text: r.stdout }], language: cur.language });
    open(head, cur);
  };
  // The next (or previous) changed block, on whichever side is active.
  const next = (dir) => {
    const v = fresh.views.active();
    if (!pair || !v) return;
    const side = v.buffer.id === pair.a.id ? "a" : v.buffer.id === pair.b.id ? "b" : null;
    if (!side) return fresh.ui.message("Not a compared file");
    if (!pair.blocks.length) return fresh.ui.message("No differences");
    const L = v.buffer.lines();
    const cur = L.starts.findIndex((s, k) => v.cursor >= s && (k + 1 >= L.starts.length || v.cursor < L.starts[k + 1]));
    const lines = pair.blocks.map((k) => k[side]);
    const line = dir > 0 ? lines.find((i) => i > cur) ?? lines[0] : [...lines].reverse().find((i) => i < cur) ?? lines[lines.length - 1];
    v.setCursor(L.starts[Math.min(line, L.starts.length - 1)], { center: true });
  };
  let t = null;
  fresh.events.on("buffer.changed", ({ buffer }) => {
    if (pair && (buffer.id === pair.a.id || buffer.id === pair.b.id)) { clearTimeout(t); t = setTimeout(mark, 200); }
  });
  fresh.commands.register("compare.files", { title: "Compare Files…", run: compareFiles });
  fresh.commands.register("compare.head", { title: "Compare with Last Commit", run: compareHead });
  fresh.commands.register("compare.next", { title: "Compare: Next Difference", run: () => next(1) });
  fresh.commands.register("compare.prev", { title: "Compare: Previous Difference", run: () => next(-1) });
  fresh.keys.bind("F7", "compare.next");
  fresh.keys.bind("S-F7", "compare.prev");
}
