// Bookmarks, as in the editor: Ctrl+Shift+0…9 sets bookmark N at the
// cursor (again on the same line clears it), Alt+0…9 jumps to it. A
// bookmark is a gutter mark (its digit) in its own decoration set, so its
// position moves with every edit, anyone's; when its file was closed, the
// place it was set at is opened instead. Bookmarks: List picks one.

import * as fresh from "../js/api.js";

export function activate(app) {
  const marks = new Map(); // n -> {path, pos} as set (for a closed file)
  const set = (n) => (b) => b.decorations(`bm${n}`);
  // Where bookmark `n` is now: {buffer, pos}, or null.
  const where = (n) => {
    for (const b of fresh.buffers(app)) {
      const g = set(n)(b).gutters()[0];
      if (g) return { buffer: b, pos: g.pos };
    }
    return null;
  };
  const toggle = (n) => {
    const v = fresh.activeView(app);
    if (!v) return;
    const b = v.buffer;
    const now = where(n);
    const line = (buf, p) => { const L = buf.lines(); return L ? L.starts.findLastIndex((s) => s <= p) : -1; };
    if (now) {
      set(n)(now.buffer).clear();
      now.buffer.render();
      if (now.buffer.id === b.id && line(b, now.pos) === line(b, v.cursor)) { marks.delete(n); fresh.message(app, `Bookmark ${n} cleared`); return; }
    }
    const L = b.lines();
    const i = line(b, v.cursor);
    const pos = i >= 0 ? L.starts[i] : v.cursor;
    set(n)(b).gutter(pos, { glyph: String(n), color: "ui.accent_fg", title: `Bookmark ${n}` });
    b.render();
    marks.set(n, { path: b.path, pos });
    fresh.message(app, `Bookmark ${n} set`);
  };
  const jump = async (n) => {
    const at = where(n);
    if (at) {
      fresh.show(app, at.buffer, "tab");
      await fresh.activeView(app)?.setCursor(at.pos, { center: true });
      return;
    }
    const m = marks.get(n);
    if (!m || !m.path) { fresh.message(app, `No bookmark ${n}`); return; }
    const b = await fresh.open(app, m.path, { line: null, column: 1, place: "tab", preview: null, focus: true });
    if (!b) return;
    set(n)(b).gutter(m.pos, { glyph: String(n), color: "ui.accent_fg", title: `Bookmark ${n}` });
    await fresh.activeView(app)?.setCursor(Math.min(m.pos, b.length), { center: true });
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
      const items = [];
      for (let n = 0; n <= 9; n++) {
        const at = where(n);
        const m = marks.get(n);
        if (!at && !m) continue;
        const name = at ? at.buffer.name : m.path.split("/").pop();
        items.push({ label: `${n}  ${name}`, detail: at ? "open" : "closed file", value: n });
      }
      fresh.pick(app, { title: "Bookmarks", items: items.length ? items : [{ label: "No bookmarks (Ctrl+Shift+0…9 sets one)", value: null }], onAccept: (it) => it.value != null && jump(it.value) });
    },
  });
  fresh.addMenuItem(app, "Go", null);
  fresh.addMenuItem(app, "Go", { command: "bookmark.list", label: "Bookmarks…" });
}
