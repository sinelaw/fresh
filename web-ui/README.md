# Fresh — web UI (wired to the real editor, no mocks)

A non-terminal UI for Fresh, driven **end-to-end by the real Rust `Editor`** — no
mock model. See `docs/internal/NON_TERMINAL_UI_RESEARCH.md` (Direction A).

- **Chrome is DOM/CSS** — menu bar, tabs, split panes via CSS grid + dividers,
  status bar — rendered from the editor's real `ChromeSnapshot`.
- **Text body is SVG `<text>`/`<tspan>`** — gutter + syntax-colored runs from
  real buffer contents.
- **Editing is real** — keystrokes are POSTed to `/key` and run through the real
  `Editor::handle_key`; the page re-renders from the editor's new state.

## Architecture (taps the real render pipeline)

```
browser (web-ui/index.html)  ──HTTP──►  fresh::webui bridge  ──►  real Editor
  chrome = UI/DOM @ rects     GET /state   runs Editor::render    (piece tree,
  buffer interior = real      POST /key    into a cell buffer,    highlighter,
  highlighted CELLS (SVG)      POST /resize reads the pipeline's   handle_key, …)
  keydown ─► POST /key                      layout caches + cells
```

The bridge (`crates/fresh-editor/src/webui/mod.rs`) runs the **actual**
`Editor::render` once into an in-memory `Buffer`, then reads the geometry the
pipeline already aggregated for the frame — `WindowLayoutCache`
(`split_areas` = per-pane content_rect + scrollbar + thumb, `separator_areas`,
`tab_layouts`, `file_explorer_area`) and `ChromeLayout` — and slices the rendered
cells. **Nothing is re-implemented:** layout, syntax highlighting, tabs,
scrollbars and split borders all come from the pipeline. Only the final drawing is
re-targeted: buffer interiors are emitted as the real highlighted cells (drawn as
SVG), and chrome (menu bar, status bar, tabs, scrollbars, split borders, file
explorer) is emitted as semantic region rects rendered as UI/DOM elements.

It is the same `Backend` seam a Tauri build would use (`invoke`/event), over
localhost so it runs headless. (Next: ship only the visible-window cell diff
rather than the whole frame, per the xi-editor lesson.)

## Run it

```sh
cargo run -p fresh-editor --example webui_server -- 127.0.0.1:8139 \
  crates/fresh-editor/src/view/chrome_snapshot.rs   # or any file(s)
# then open http://127.0.0.1:8139  and type — edits go through the real editor.
```

## Status (partial wire, more coming)

Live now: render real chrome + real buffer text; keyboard editing (printable keys,
Enter, Backspace/Delete, arrows, Home/End, Page keys) through `handle_key`.

Next: live caret/selection from the editor's cursor, mouse → `handle_mouse`, tab &
split & palette clicks mapped to real editor actions, real syntax highlighting
(reuse the editor's highlighter instead of the toy JS one), and the visible-window
line-cache diff.

## Headless test (Playwright)

`test/drive.mjs` drives the **real** UI in headless Chromium: asserts the page
renders genuine editor state (menubar, on-disk file contents) and that typing
mutates the real buffer (cross-checked against the server's `/state`). 10 assertions
+ screenshots.

```sh
# 1) start the bridge (see above) on :8139
# 2) run the driver
CHROMIUM=/path/to/chrome UI_URL=http://127.0.0.1:8139 node web-ui/test/drive.mjs
```

(Defaults: `CHROMIUM=/opt/pw-browsers/chromium-1194/chrome-linux/chrome`,
`UI_URL=http://127.0.0.1:8139`, `SHOTS=/tmp/pw/shots`.)
