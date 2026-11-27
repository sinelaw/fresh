// The keybinding editor: every command with its keys, searchable; select
// one and press Enter (or click Change) to record a new key; your own
// bindings are kept in the browser and take precedence over the defaults.

import * as fresh from "../js/api.js";
import * as ui from "../js/ui.js";

export function activate(app) {
  // The command whose new key is being recorded ("" when none).
  let query = "", selected = 0;
  // The command whose new key is being recorded, if any.
  /** let recording: String | Null */
  let recording = null;
  const rows = () => {
    const km = fresh.keymap(app);
    const user = fresh.userKeys(app);
    const byCmd = new Map();
    km.forEach((c, k) => { byCmd.set(c, [...(byCmd.get(c) ?? []), k]); });
    const q = query.toLowerCase();
    const all /*: CommandInfo[] */ = fresh.commands(app);
    return all
      .filter((c) => !q || c.title.toLowerCase().includes(q) || c.id.includes(q) || (byCmd.get(c.id) || []).some((k) => k.toLowerCase().includes(q)))
      .sort((a, b) => a.title.localeCompare(b.title))
      .map((c) => ({ ...c, keys: byCmd.get(c.id) || [], custom: (byCmd.get(c.id) || []).some((k) => user.get(k) === c.id) }));
  };
  const record = async (row) => {
    if (!row) return;
    recording = row.id;
    refresh();
    const key = await fresh.captureKey(app);
    recording = null;
    if (key) {
      fresh.setUserKey(app, key, row.id);
      fresh.message(app, `${key} → ${row.title}`);
    }
    refresh();
  };
  const remove = (row) => {
    if (!row) return;
    const user = fresh.userKeys(app);
    for (const k of row.keys) if (user.get(k) === row.id) fresh.setUserKey(app, k, null);
    refresh();
  };
  // Redraw the editor if it is on screen.
  const refresh = () => {
    const panel = fresh.shown(app, "keys");
    if (panel) render(panel);
  };
  const render = (panel) => {
    panel.render(ui.col(ui.BOX, [
      ui.row(ui.BOX, [
        ui.input({ ...ui.INPUT, key: "q", value: query, placeholder: "Search commands or keys", grow: true, list: ["keys"], onInput: (q) => { query = q; selected = 0; refresh(); }, onEnter: () => { record(rows()[selected]); } }),
        ui.text({ ...ui.TEXT, dim: true }, recording !== null ? "Press the new key (Esc cancels)…" : "Enter: add a key · Delete in the list: remove yours"),
        ui.button({ ...ui.BUTTON, onClick: () => record(rows()[selected]) }, "Add Key"),
        ui.button({ ...ui.BUTTON, onClick: () => remove(rows()[selected]) }, "Remove Mine"),
        ui.button({ ...ui.BUTTON, onClick: () => panel.close() }, "Close"),
      ]),
      ui.list({
        ...ui.listProps("keys", rows(), (r) => ui.row(ui.BOX, [
          ui.text({ ...ui.TEXT, bold: r.custom }, r.title),
          ui.spacer(),
          ui.text({ ...ui.TEXT, mono: true, color: ["ui.accent_fg"] }, recording === r.id ? "…" : r.keys.join(", ")),
          ui.text({ ...ui.TEXT, dim: true }, r.source !== "core" ? r.source : ""),
        ])),
        selected: [selected],
        onSelect: (_r, i) => { selected = i; },
        onActivate: (r) => { record(r); },
        onKey: (e, r) => { if (e.key === "Delete") { e.preventDefault(); for (const x of r) remove(x); } },
      }),
    ]));
  };
  fresh.register(app, "keys.editor", {
    title: "Keybindings: Open Editor",
    hidden: false,
    run: () => {
      const panel = fresh.panel(app, { id: "keys", title: "Keybindings", place: "dialog", onClose: null });
      render(panel);
      panel.focus("q");
    },
  });
  fresh.bindKey(app, "C-M-k", "keys.editor");
}
