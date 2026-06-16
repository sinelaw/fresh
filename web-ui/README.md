# Fresh — web UI (wired to the real editor, no mocks)

A non-terminal UI for Fresh, driven **end-to-end by the real Rust `Editor`** — no
mock model. See `docs/internal/UNIFIED_SCENE_DESIGN.md` (design + phased plan) and
`docs/internal/NON_TERMINAL_UI_RESEARCH.md` (original research).

The guiding principle: **the TUI and the web must not re-implement the same
logic.** Everything semantic — which menus/items exist, what's enabled/checked,
accelerators, status segments, the settings tree, etc. — is derived **once** in
the core; each frontend only renders it.

- **Chrome is native DOM/CSS**, rendered from the editor's **semantic
  projections** in `crates/fresh-editor/src/view/scene.rs` (`Editor::menu_view()`,
  `tab_bar_view()`, `status_view()`, `palette_view()`, `popups_view()`,
  `file_explorer_view()`, `trust_dialog_view()`, `widgets_view()`,
  `context_menu_view()`, `keybinding_editor_view()`, `settings_view()`): menu bar
  + dropdowns, tabs, status bar, command palette, popups, file explorer, trust
  dialog, context menus, plugin widgets/dock, the keybinding editor and the full
  Settings modal.
- **Buffer interior is SVG** — the pipeline's real, syntax-highlighted cells. The
  line-number gutter is emitted as its own block (kept out of the buffer-text
  flow), and every glyph is pinned to its exact cell column.
- **Input is real** — key/mouse/wheel are POSTed and run through the real
  `Editor::handle_key` / `handle_mouse` (and shared hit→action dispatch for
  settings/widgets/keybindings); the page re-renders from the editor's new state.

## Architecture (taps the real render pipeline)

```
browser (web-ui/index.html)  ──HTTP──►  fresh::webui bridge  ──►  real Editor
  chrome  = native HTML from   GET /state    runs Editor::render   (piece tree,
  scene.rs projections         POST /key     into a cell buffer,   highlighter,
  buffer  = real highlighted   POST /mouse   reads the pipeline's   handle_key, …)
  CELLS (SVG)                  POST /action  layout caches + cells
  key/mouse ─► POST            POST /resize
```

The bridge (`crates/fresh-editor/src/webui/mod.rs`) runs the **actual**
`Editor::render` once into an in-memory `Buffer`. `Editor::suppress_chrome_cells`
makes the pipeline compute chrome *layout/geometry/semantics* but **not draw**
chrome cells, so the cell buffer carries pane interiors only. The bridge then
serializes the `scene.rs` projections (chrome) and slices the rendered cells
(buffer interiors). **Nothing is re-implemented** — layout, highlighting, tabs,
scrollbars, split borders and item state all come from the core; only the final
drawing is re-targeted. The TUI keeps `suppress_chrome_cells = false`, so its
rendering is unchanged.

Each poll runs `editor_tick` (drains async LSP/plugin/file events, steps
animations), so frames advance without user input, exactly like the TUI loop.

## Run it

```sh
cargo run -p fresh-editor --example webui_server -- 127.0.0.1:8137 \
  crates/fresh-editor/src/view/scene.rs   # or any file(s)
# then open http://127.0.0.1:8137  and type — edits go through the real editor.
```

> ⚠️ The bridge binds plain localhost HTTP and hosts a live editor with
> filesystem access. It's a local-development prototype, **not** for exposure on
> a shared interface.

## Headless test (Playwright)

`test/drive.mjs` drives the **real** bridge in headless Chromium: it asserts the
buffer interior is the pipeline's real syntax-highlighted cells while all chrome
is native HTML (no cell-drawn chrome), and that key / mouse / menu / palette /
settings / widget interactions run through the real `Editor`. **50 assertions**
across the chrome surfaces, plus screenshots.

```sh
# 1) start the bridge (see above) on :8141
# 2) run the driver
CHROMIUM=/path/to/chrome UI_URL=http://127.0.0.1:8141 node web-ui/test/drive.mjs
```

(Defaults: `CHROMIUM=/opt/pw-browsers/chromium-1194/chrome-linux/chrome`,
`UI_URL=http://127.0.0.1:8141`, `SHOTS=/tmp/pw/shots`.)

A Rust web/TUI parity test (`crates/fresh-editor/tests/scene_parity.rs`) drives
one `Editor` and asserts the chrome the web scene reports also appears in the
TUI's cell rendering — so the two renderers can't diverge on what the chrome is.
