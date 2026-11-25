// Bookmarks, as in the editor: Ctrl+Shift+0…9 sets bookmark N at the
// cursor (again on the same line clears it), Alt+0…9 jumps to it. A
// bookmark is a gutter mark (its digit) in its own decoration set, so its
// position moves with every edit, anyone's; when its file was closed, the
// place it was set at is opened instead. Bookmarks: List picks one.

import * as fresh from "../js/api.js";

// A bookmark as it was set: its file and place (for when the file is
// closed), and where it is now in an open buffer (by id).
/** type Placed = {path: String, pos: Number} */
/** type At = {buffer: Number, pos: Number} */

export function activate(app) {
  /** const marks: Map<Number, Placed> */
  const marks = new Map();
  const set = (n) => (b) => b.decorations(`bm${n}`);
  // Where bookmark `n` is now, in an open buffer.
  /** const where: (Number) => At | Null */
  const where = (n) => {
    for (const b of fresh.buffers(app)) {
      const g = set(n)(b).gutters()[0];
      if (g) return { buffer: b.id, pos: g.pos };
    }
    return null;
  };
  const toggle = (n) => {
    const v = fresh.activeView(app);
    if (!v) return;
    const b = v.buffer;
    const now = where(n);
    const line = (buf, p) => { const L = buf.lines(); return L ? L.starts.findLastIndex((s) => s <= p) : -1; };
    const was = now ? fresh.bufferById(app, now.buffer) : null;
    if (now && was) {
      set(n)(was).clear();
      was.render();
      if (was.id === b.id && line(b, now.pos) === line(b, v.cursor())) { marks.delete(n); fresh.message(app, `Bookmark ${n} cleared`); return; }
    }
    const L = b.lines();
    const i = line(b, v.cursor());
    const pos = L && i >= 0 ? L.starts[i] : v.cursor();
    set(n)(b).gutter(pos, { glyph: String(n), color: "ui.accent_fg", title: `Bookmark ${n}` });
    b.render();
    // A virtual buffer's bookmark goes with it.
    const path = b.path();
    if (path) marks.set(n, { path, pos });
    else marks.delete(n);
    fresh.message(app, `Bookmark ${n} set`);
  };
  const jump = async (n) => {
    const at = where(n);
    const open = at ? fresh.bufferById(app, at.buffer) : null;
    if (at && open) {
      fresh.show(app, open, "tab");
      const v = fresh.activeView(app);
      if (v) await v.setCursor(at.pos, { center: true });
      return;
    }
    const m = marks.get(n);
    if (!m) { fresh.message(app, `No bookmark ${n}`); return; }
    const b = await fresh.open(app, m.path, { line: null, column: 1, place: "tab", preview: null, focus: true });
    if (!b) return;
    set(n)(b).gutter(m.pos, { glyph: String(n), color: "ui.accent_fg", title: `Bookmark ${n}` });
    const v = fresh.activeView(app);
    if (v) await v.setCursor(Math.min(m.pos, b.length()), { center: true });
  };
  for (let n = 0; n <= 9; n++) {
    fresh.register(app, `bookmark.set${n}`, { title: `Bookmark: Toggle ${n}`, hidden: true, run: () => toggle(n) });
    fresh.register(app, `bookmark.jump${n}`, { title: `Bookmark: Jump to ${n}`, hidden: true, run: () => jump(n) });
    fresh.bindKey(app, `C-S-${n}`, `bookmark.set${n}`);
    fresh.bindKey(app, `M-${n}`, `bookmark.jump${n}`);
  }
  fresh.register(app, "bookmark.list", {
    title: "Bookmarks: List…",
    hidden: false,
    run: () => {
      // Each bookmark: in an open buffer, or in a closed file.
      /** const items: {label: String, detail: String, value: Number | Null}[] */
      const items = [];
      for (let n = 0; n <= 9; n++) {
        const at = where(n);
        const open = at ? fresh.bufferById(app, at.buffer) : null;
        const m = marks.get(n);
        const name = open ? open.name() : m ? m.path.slice(m.path.lastIndexOf("/") + 1) : null;
        if (name !== null) items.push({ label: `${n}  ${name}`, detail: open ? "open" : "closed file", value: n });
      }
      if (!items.length) items.push({ label: "No bookmarks (Ctrl+Shift+0…9 sets one)", detail: "", value: null });
      fresh.pick(app, { title: "Bookmarks", items, onAccept: (it) => { const n = it.value; if (n !== null) jump(n); } });
    },
  });
  fresh.addMenuItem(app, "Go", fresh.MENU_BAR_SEPARATOR);
  fresh.addMenuItem(app, "Go", fresh.menuCommand("bookmark.list", "Bookmarks…"));
}
