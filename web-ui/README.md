# Fresh — web UI prototype

An interactive, self-contained browser prototype of Fresh's **non-terminal UI**
(see `docs/internal/NON_TERMINAL_UI_RESEARCH.md`, Direction A / §A.1).

- **Chrome is DOM/CSS** — menu bar, tabs (with close), split panes via CSS grid +
  draggable dividers, status bar, command palette. Accessible (real DOM/ARIA).
- **Text body is SVG `<text>`/`<tspan>`** — line-number gutter, syntax-colored runs,
  blinking caret. Mirrors the Rust reference renderer
  `crates/fresh-editor/src/view/chrome_html.rs`.
- **Interactive**: type, Backspace/Delete/Enter, arrows, Home/End; click tabs to
  switch, `×` to close; drag the divider to resize; `Cmd/Ctrl+P` for the command
  palette; click a menu; `Esc` to dismiss.

## Run it

Just open `index.html` in any browser. No build step.

## The backend seam (path to the real core)

The UI talks to a `Backend` object that currently applies events to an **in-browser
mock model** (`Mock`). In a Tauri build, `Backend.mode === "tauri"` and `Backend.send`
calls `window.__TAURI__.invoke("apply_event", …)`, with a `chrome-update` event
re-rendering from the **real Rust core** (piece tree, LSP, plugins, real/GB files).
The UI code does not change — only the `Backend` implementation. Per the xi-editor
lesson, the backend pushes only the **visible-window line diff**, never the whole buffer.

## Headless test (Playwright)

`test/drive.mjs` drives the UI in headless Chromium and asserts the interactions
(typing/editing, tab switch, palette open→filter→select, divider drag, tab close,
menu) — 22 assertions, plus screenshots.

```sh
# needs `playwright` installed and a Chromium binary
CHROMIUM=/path/to/chrome SHOTS=/tmp/shots node web-ui/test/drive.mjs
```

Defaults: `CHROMIUM=/opt/pw-browsers/chromium-1194/chrome-linux/chrome`,
`FRESH_UI=<this dir>/index.html`, `SHOTS=/tmp/pw/shots`.
