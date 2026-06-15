# Fresh — web UI (wired to the real editor, no mocks)

A non-terminal UI for Fresh, driven **end-to-end by the real Rust `Editor`** — no
mock model. See `docs/internal/NON_TERMINAL_UI_RESEARCH.md` (Direction A).

- **Chrome is DOM/CSS** — menu bar, tabs, split panes via CSS grid + dividers,
  status bar — rendered from the editor's real `ChromeSnapshot`.
- **Text body is SVG `<text>`/`<tspan>`** — gutter + syntax-colored runs from
  real buffer contents.
- **Editing is real** — keystrokes are POSTed to `/key` and run through the real
  `Editor::handle_key`; the page re-renders from the editor's new state.

## Architecture (the seam)

```
browser (web-ui/index.html)  ──HTTP──►  fresh::webui bridge  ──►  real Editor
  DOM/CSS chrome + SVG text   GET /state  (single-threaded,        (piece tree,
  keydown ─► POST /key        POST /key    hosts the Editor)        handle_key, …)
```

The bridge (`crates/fresh-editor/src/webui/mod.rs`) is the same `Backend` seam a
Tauri build would use (`invoke`/event) — just over localhost so it runs headless.
Per the xi-editor lesson it should ship only the visible-window line diff; the PoC
currently sends whole-buffer text (bounded to 1000 lines).

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
