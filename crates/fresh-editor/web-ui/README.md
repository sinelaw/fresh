# Fresh — web client

The browser front end of Fresh: an ordinary client-side web application on the
editor's core. The design is [docs/internal/web-ui.md](../../../docs/internal/web-ui.md);
this file is how the code is laid out and how to run it.

The core is a headless editor engine. It sends the client data — the visual
lines of each editor view at the client's size, the split layout, open
buffers, menus, commands, the status facts, prompts and confirmations,
popups, plugin panels, settings, files — and change events, and takes
commands. The client owns all of its UI: layout, menus, palette, explorer,
tabs, dialogs, settings screen, focus, hover, scrolling and theming. It is free
to look and behave differently from the terminal; editing behaves the same,
because editing is the core's.

## Running it

```sh
cargo run --release --features web --example webui_server -- 127.0.0.1:8137 path/to/file
```

then open <http://127.0.0.1:8137>. `fresh --web` serves the same client from
the session daemon, so a browser and `fresh -a` terminals share one editor.

## Layout

- `index.html` — the document. `build.rs` concatenates `css/*.css` and
  `js/*.js` in filename order into its `/*@CSS@*/` and `/*@JS@*/` markers; the
  result is embedded in the binary, so the page is one self-contained file.
- `js/00-api.js` — the WebSocket (`/api`), requests and events, and `core`, the
  last value of each topic.
- `js/10-theme.js` — the client's colours, seeded from the editor theme.
- `js/15-skins.js` — skins: reading, choosing, installing and applying them,
  their furniture and live values, and the picker
  ([docs/features/web-ui-skins.md](../../../docs/features/web-ui-skins.md)).
- `js/20-view.js` — editor views: open one per pane at its size, draw the
  lines the core sends, send presses, drags and wheel scrolls in cells.
- `js/30-layout.js` — splits, panes and tabs from the core's layout.
- `js/40-menubar.js` — the menu bar from the core's menus.
- `js/45-explorer.js` — the file tree, over the core's filesystem.
- `js/50-status.js` — the status bar from the core's facts.
- `js/55-ask.js` — the core's questions: prompts (with the file browser
  for file prompts), confirmations, workspace trust; the client's own small
  dialogs and context menus.
- `js/60-popups.js` — completion, hover and action popups at the caret.
- `js/65-widgets.js`, `js/70-panels.js` — plugin panels: a `WidgetSpec` drawn
  as controls; presses go back as `panel.event`.
- `js/80-settings.js` — the settings screen, from the schema and the values.
- `js/85-keybindings.js` — keyboard shortcuts, over the core's keybinding
  model.
- `js/90-keys.js` — the keyboard: the client's own keys, everything else to
  the core; paste and clipboard; boot.
- `css/` — the client's styles (`99-app.css`), the widget renderer's, and
  what every skin builds on (`95-skin-kit.css`: the frames, the bezel kit,
  the picker).
- `skins/` — the built-in skins, bundled into the page by `build.rs`;
  `example-skins/` — skins to copy into a skins folder.

## The protocol

`crates/fresh-editor/src/webui/api.rs` is the reference. In brief:

- Requests `{"id", "op", …}` get a reply `{"re", "ok": true, …}` or
  `{"re", "error"}`; without an `id`, none.
- A view's event carries its facts (`width`, `count` rows, `gutter`,
  `caret`, `top` byte and `len`, and `topLine` and `lines` when the buffer
  knows its lines) and only the rows that changed since the last one, as
  `rows: [[index, runs], …]`; the client keeps the rest. A row is the cells
  a terminal backend draws (`Buffer::diff`), as runs `{x, t, fg, bg, b, i,
  u, r, d, s}` where `t` has one entry per column from `x`: the grapheme
  drawn there, or `""` for a column the grapheme before it covers. The
  client draws each grapheme at its own column on a canvas, so no width is
  ever computed from a font.
- Events `{"ev": topic, …}` arrive when a topic changed: `root` (the project
  root, the active workspace's), `layout`,
  `buffers`, `status`, `menus`, `ask`, `popups`, `panels`, `settings`,
  `theme`, `skinValues`, `clipboard`, `keybindings` (while the shortcuts
  screen is open), and `view` for each open view.
- Ops: `view.open|resize|scroll|scrollto|press|drag|close`, `key`, `paste`,
  `action`, `command.run`, `commands.list`, `menu.run`, `split.focus`,
  `split.ratio`, `tab.show`, `tab.close`, `tab.drop`, `buffer.new`,
  `menu.rows|choose` (a tab's, the "+"'s or an explorer entry's menu: the
  terminal's, plugins' items included),
  `file.open|rename|create|delete`, `dir.list`,
  `prompt.set|select|confirm|cancel`, `browser.select|cd|hidden`,
  `confirm.choose`, `trust.select|confirm|decline`,
  `keys.open|set|delete|disable|conflicts|save|close`,
  `popup.select|confirm|cancel`, `panel.event`, `settings.schema`,
  `settings.set`, `skin.theme`, `quit`.
- Besides `/api`, the host serves the page, its font, and the skins folder
  (`/skins/index.json`, `/skins/<id>/<file>`).

A command that does not name a view acts in the client's active view (its
own cursors). Payload fields never use the name `id`, which is the
request's.

## Tests

- `cargo test -p fresh-editor --features web --lib webui::` — the host and
  the API.
- `web-ui/test/run.sh` — builds the host, starts it on a throwaway project and
  drives the client in headless Chromium (`drive.mjs`).
