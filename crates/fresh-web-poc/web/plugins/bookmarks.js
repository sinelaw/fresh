// Bookmarks, as in the editor: Ctrl+Shift+0…9 sets bookmark N at the
// cursor (again on the same line clears it), Alt+0…9 jumps to it. A
// bookmark is a gutter mark (its digit) in its own decoration set, so its
// position moves with every edit, anyone's; when its file was closed, the
// place it was set at is opened instead. Bookmarks: List picks one.

export function activate(fresh) {
  const marks = new Map(); // n -> {path, pos} as set (for a closed file)
  const set = (n) => (b) => b.decorations(`bm${n}`);
  // Where bookmark `n` is now: {buffer, pos}, or null.
  const where = (n) => {
    for (const b of fresh.buffers.all()) {
      const g = set(n)(b).gutters()[0];
      if (g) return { buffer: b, pos: g.pos };
    }
    return null;
  };
  const toggle = (n) => {
    const v = fresh.views.active();
    if (!v) return;
    const b = v.buffer;
    const now = where(n);
    const line = (buf, p) => { const L = buf.lines(); return L ? L.starts.findLastIndex((s) => s <= p) : -1; };
    if (now) {
      set(n)(now.buffer).clear();
      now.buffer.render();
      if (now.buffer.id === b.id && line(b, now.pos) === line(b, v.cursor)) { marks.delete(n); fresh.ui.message(`Bookmark ${n} cleared`); return; }
    }
    const L = b.lines();
    const i = line(b, v.cursor);
    const pos = i >= 0 ? L.starts[i] : v.cursor;
    set(n)(b).gutter(pos, { glyph: String(n), color: "ui.accent_fg", title: `Bookmark ${n}` });
    b.render();
    marks.set(n, { path: b.path, pos });
    fresh.ui.message(`Bookmark ${n} set`);
  };
  const jump = async (n) => {
    const at = where(n);
    if (at) {
      fresh.buffers.show(at.buffer, "tab");
      await fresh.views.active()?.setCursor(at.pos, { center: true });
      return;
    }
    const m = marks.get(n);
    if (!m || !m.path) { fresh.ui.message(`No bookmark ${n}`); return; }
    const b = await fresh.buffers.open(m.path);
    if (!b) return;
    set(n)(b).gutter(m.pos, { glyph: String(n), color: "ui.accent_fg", title: `Bookmark ${n}` });
    await fresh.views.active()?.setCursor(Math.min(m.pos, b.length), { center: true });
  };
  for (let n = 0; n <= 9; n++) {
    fresh.commands.register(`bookmark.set${n}`, { title: `Bookmark: Toggle ${n}`, hidden: true, run: () => toggle(n) });
    fresh.commands.register(`bookmark.jump${n}`, { title: `Bookmark: Jump to ${n}`, hidden: true, run: () => jump(n) });
    fresh.keys.bind(`C-S-${n}`, `bookmark.set${n}`);
    fresh.keys.bind(`M-${n}`, `bookmark.jump${n}`);
  }
  fresh.commands.register("bookmark.list", {
    title: "Bookmarks: List…",
    run: () => {
      const items = [];
      for (let n = 0; n <= 9; n++) {
        const at = where(n);
        const m = marks.get(n);
        if (!at && !m) continue;
        const name = at ? at.buffer.name : m.path.split("/").pop();
        items.push({ label: `${n}  ${name}`, detail: at ? "open" : "closed file", value: n });
      }
      fresh.ui.pick({ title: "Bookmarks", items: items.length ? items : [{ label: "No bookmarks (Ctrl+Shift+0…9 sets one)", value: null }], onAccept: (it) => it.value != null && jump(it.value) });
    },
  });
  fresh.menus.add("Go", null);
  fresh.menus.add("Go", { command: "bookmark.list", label: "Bookmarks…" });
}
