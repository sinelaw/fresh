// The keybinding editor: every command with its keys, searchable; select
// one and press Enter (or click Change) to record a new key; your own
// bindings are kept in the browser and take precedence over the defaults.

export function activate(fresh) {
  const { h } = fresh;
  // The command whose new key is being recorded ("" when none).
  let query = "", recording = "", selected = 0;
  const rows = () => {
    const km = fresh.keys.all();
    const user = fresh.keys.user();
    const byCmd = new Map();
    for (const [k, c] of Object.entries(km)) { if (!byCmd.has(c)) byCmd.set(c, []); byCmd.get(c).push(k); }
    const q = query.toLowerCase();
    return fresh.commands.all()
      .filter((c) => !q || c.title.toLowerCase().includes(q) || c.id.includes(q) || (byCmd.get(c.id) || []).some((k) => k.toLowerCase().includes(q)))
      .sort((a, b) => a.title.localeCompare(b.title))
      .map((c) => ({ ...c, keys: byCmd.get(c.id) || [], custom: (byCmd.get(c.id) || []).some((k) => user[k] === c.id) }));
  };
  const record = async (row) => {
    if (!row) return;
    recording = row.id;
    refresh();
    const key = await fresh.keys.capture();
    recording = "";
    if (key) {
      fresh.keys.setUser(key, row.id);
      fresh.ui.message(`${key} → ${row.title}`);
    }
    refresh();
  };
  const remove = (row) => {
    if (!row) return;
    const user = fresh.keys.user();
    for (const k of row.keys) if (user[k] === row.id) fresh.keys.setUser(k, null);
    refresh();
  };
  // Redraw the editor if it is on screen.
  const refresh = () => {
    const panel = fresh.ui.shown("keys");
    if (panel) render(panel);
  };
  const render = (panel) => {
    panel.render(h("col", {}, [
      h("row", {}, [
        h("input", { key: "q", value: query, placeholder: "Search commands or keys", grow: true, list: "keys", onInput: (q) => { query = q; selected = 0; refresh(); }, onEnter: () => record(rows()[selected]) }),
        h("text", { dim: true }, [recording ? "Press the new key (Esc cancels)…" : "Enter: add a key · Delete in the list: remove yours"]),
        h("button", { onClick: () => record(rows()[selected]) }, ["Add Key"]),
        h("button", { onClick: () => remove(rows()[selected]) }, ["Remove Mine"]),
      ]),
      h("list", {
        key: "keys",
        items: rows(),
        selected,
        onSelect: (_r, i) => { selected = i; },
        render: (r) => h("row", {}, [
          h("text", { bold: r.custom }, [r.title]),
          h("spacer", {}),
          h("text", { mono: true, color: "ui.accent_fg" }, [recording === r.id ? "…" : r.keys.join(", ")]),
          h("text", { dim: true }, [r.source !== "core" ? r.source : ""]),
        ]),
        onActivate: record,
        onKey: (e, r) => { if (e.key === "Delete") { e.preventDefault(); remove(r); } },
      }),
    ]));
  };
  fresh.commands.register("keys.editor", {
    title: "Keybindings: Open Editor",
    hidden: false,
    run: () => {
      const panel = fresh.ui.panel({ id: "keys", title: "Keybindings", place: "dialog", onClose: null });
      render(panel);
      panel.focus("q");
    },
  });
  fresh.keys.bind("C-M-k", "keys.editor");
}
