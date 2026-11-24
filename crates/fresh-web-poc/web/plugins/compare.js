// Compare files: two files side by side, both editable, differences marked
// in both and kept current as either is edited. Or a file against its last
// commit.

import { diffLines } from "../js/diff.js";

export function activate(fresh) {
  let pair = null; // {a, b} buffers
  const mark = () => {
    if (!pair) return;
    const { a, b } = pair;
    const A = a.lines(), Bl = b.lines();
    if (!A || !Bl) return;
    const da = a.decorations("compare"), db = b.decorations("compare");
    da.clear(); db.clear();
    const edits = diffLines(A.lines, Bl.lines) || [];
    pair.hunks = [];
    for (const e of edits) {
      if (e.kind === "-" && e.a < A.starts.length) {
        da.overlay(A.starts[e.a], A.starts[e.a] + fresh.text.byteLen(A.lines[e.a]) + 1, { bg: "#4b1818", extendToLineEnd: true });
        da.gutter(A.starts[e.a], { glyph: "-", color: "git.deleted" });
      } else if (e.kind === "+" && e.b < Bl.starts.length) {
        db.overlay(Bl.starts[e.b], Bl.starts[e.b] + fresh.text.byteLen(Bl.lines[e.b]) + 1, { bg: "#1e3a1e", extendToLineEnd: true });
        db.gutter(Bl.starts[e.b], { glyph: "+", color: "git.added" });
        pair.hunks.push(e.b);
      }
    }
    a.render(); b.render();
    fresh.ui.message(`${edits.length} line${edits.length === 1 ? "" : "s"} differ`);
  };
  const open = async (pathA, bufB) => {
    const a = typeof pathA === "string" ? await fresh.buffers.load(pathA) : pathA;
    const b = typeof bufB === "string" ? await fresh.buffers.load(bufB) : bufB;
    fresh.buffers.show(a, "tab");
    fresh.buffers.show(b, "right");
    pair = { a, b, hunks: [] };
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
  const next = (dir) => {
    if (!pair || !pair.hunks.length) return;
    const v = fresh.views.active();
    const L = pair.b.lines();
    const cur = L.starts.findIndex((s, k) => v.cursor >= s && (k + 1 >= L.starts.length || v.cursor < L.starts[k + 1]));
    const h = dir > 0 ? pair.hunks.find((i) => i > cur) ?? pair.hunks[0] : [...pair.hunks].reverse().find((i) => i < cur) ?? pair.hunks[pair.hunks.length - 1];
    v.setCursor(L.starts[h], { center: true });
  };
  let t = null;
  fresh.events.on("buffer.changed", ({ buffer }) => {
    if (pair && (buffer.id === pair.a.id || buffer.id === pair.b.id)) { clearTimeout(t); t = setTimeout(mark, 200); }
  });
  fresh.commands.register("compare.files", { title: "Compare Files…", run: compareFiles });
  fresh.commands.register("compare.head", { title: "Compare with Last Commit", run: compareHead });
  fresh.commands.register("compare.next", { title: "Compare: Next Difference", run: () => next(1) });
  fresh.commands.register("compare.prev", { title: "Compare: Previous Difference", run: () => next(-1) });
}
