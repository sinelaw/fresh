# Fresh — web UI (wired to the real editor, no mocks)

A non-terminal UI for Fresh, driven **end-to-end by the real Rust `Editor`** — no
mock model. See `docs/internal/web-ui.md` (architecture, design gaps, and the
roadmap to desktop-grade polish); the original design/research notes
(`UNIFIED_SCENE_DESIGN.md`, `NON_TERMINAL_UI_RESEARCH.md`) live in git history.

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
  flow), and every glyph is pinned to its exact cell column. The cell size is
  **measured, not hardcoded** (canvas `measureText` of the same monospace stack,
  at boot and on zoom / `devicePixelRatio` change), and **Ctrl+= / Ctrl+- /
  Ctrl+0** (plus Ctrl+wheel) zoom the editor view — these chords are
  frontend-owned and never reach the editor, which only sees the resulting
  cols/rows re-fit; the zoom factor persists in localStorage as a pure view
  preference.
- **Input is real** — key/mouse/wheel ride the WebSocket as tagged JSON
  messages and run through the real `Editor::handle_key` / `handle_mouse`
  (and shared hit→action dispatch for settings/widgets/keybindings); the page
  re-renders from the editor's pushed state. IME/dead-key text lands in a
  hidden input and is forwarded on commit; mouse downs carry the browser's
  click count for the editor's double/triple-click path. On touch devices a
  one-finger pan scrolls the buffer through the same wheel forwarding
  (vertical and horizontal), while taps keep the ordinary click path. OS
  clipboard works both ways: DOM `paste` → a `paste` message → the editor's
  bracketed-paste path, and editor copies surface in the scene
  (`clipboard: {seq, text}`) for `navigator.clipboard`.

## Architecture (taps the real render pipeline)

```
browser (web-ui/index.html)  ══WS /ws══►  fresh::webui bridge  ──►  real Editor
  chrome  = native HTML from  ◄══ push:     runs Editor::render    (piece tree,
  scene.rs projections          hello (full scene)  into a cell     highlighter,
  buffer  = real highlighted    frame (region diffs) buffer, reads   handle_key, …)
  CELLS (SVG)                 input ══► {type:key|mouse|action|…}
                              ──HTTP──►  GET /state, POST /key … (curl + harness)
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

Transport: **one WebSocket, server push** (docs/internal/web-ui.md §3.1). On
connect the client gets a full-scene `hello`; afterwards the server's event
loop ticks the editor (drains async LSP/plugin/file events, steps animations —
~40 ms while active, ~250 ms idle, exactly like the TUI loop, with or without
a client) and pushes a `frame` of **region diffs** only when the scene
changed — typing resends only the changed pane, an idle editor sends nothing,
and the frontend rebuilds only the DOM region containers whose paths changed
(per-region patching, docs §3.4).
One client at a time (a second `/ws` gets `409`; foreign `Origin` gets `403`);
on disconnect the page retries with backoff and resyncs from the next hello.
Every HTTP route (`GET /state`, `POST /key` `/mouse` `/action` `/widget`
`/settings` `/kbedit` `/paste` `/resize` `/step` `/reset`) still answers with
the full scene as before — curl and the parity harness are untouched, and an
HTTP-side mutation is pushed to the connected browser as a diff immediately.

## Run it

The bridge ships in the main `fresh` binary behind the opt-in `web` feature,
which also embeds this `index.html` so the build is self-contained. Build with
that feature and launch with `--web [ADDR]` (address optional, default
`127.0.0.1:8137`); any files given are opened in the served editor:

```sh
cargo run --release --features web -p fresh-editor -- \
  --web 127.0.0.1:8137 crates/fresh-editor/src/view/scene.rs   # or any file(s)
# then open http://127.0.0.1:8137  and type — edits go through the real editor.
```

For interactive use serve a **release** build — the debug scene render dominates
the key→frame round-trip (see docs/internal/web-ui.md §3.1 for the measured
debug vs release numbers). A debug build works for development iteration too
(same command without `--release`), just with visibly higher typing latency.

The `webui_server` example is the equivalent entry point for the parity harness
and headless suite (`cargo run --features web -p fresh-editor --example
webui_server -- [ADDR] [FILES…]`). Both it and `fresh --web` serve the same
compile-time-embedded `index.html` — there is no on-disk fallback, so editing
the frontend requires a rebuild.

> ⚠️ The bridge binds plain localhost HTTP and hosts a live editor with
> filesystem access. It's a local-development prototype, **not** for exposure on
> a shared interface.

## Headless test (Playwright)

`test/drive.mjs` drives the **real** bridge in headless Chromium: it asserts the
buffer interior is the pipeline's real syntax-highlighted cells while all chrome
is native HTML (no cell-drawn chrome), that key / mouse / menu / palette /
settings / widget interactions run through the real `Editor` (over the
WebSocket input path), and that the push transport behaves: server-pushed
frames without page input, region diffs on typing, idle silence, and the
single-client 409 — plus per-region DOM patching (a typing frame rebuilds
only its pane), measured metrics + app zoom (Ctrl+= / Ctrl+0, hit-testing
while zoomed), and touch pan/tap in a `hasTouch` mobile context.
**113 assertions** across the chrome surfaces, plus screenshots.

One command runs the whole thing — build the bridge, install the Playwright
deps (`test/package.json`) on first use, start the server, run the suite,
tear down:

```sh
web-ui/test/run.sh
```

Env knobs: `PORT` (default `8141`) picks the bridge port; `PROFILE` (default
`debug`, keeps CI cheap) selects the cargo profile — `PROFILE=release
web-ui/test/run.sh` builds with `--release` and runs the suite against
`target/release/examples/webui_server`; `CHROMIUM=/path/to/chrome`
uses an existing Chromium binary and skips playwright's browser download
(otherwise `run.sh` fetches Chromium via `npx playwright install chromium
--with-deps` on first use). `SHOTS` (default `/tmp/pw/shots`) is where
screenshots land. To run the driver against an already-running bridge:

```sh
UI_URL=http://127.0.0.1:8141 node web-ui/test/drive.mjs
```

CI runs this suite plus the parity test via `.github/workflows/web-ui.yml` on
changes touching `web-ui/` or the webui/scene code.

A Rust web/TUI parity test (`crates/fresh-editor/tests/scene_parity.rs`) drives
one `Editor` and asserts the chrome the web scene reports also appears in the
TUI's cell rendering — so the two renderers can't diverge on what the chrome is.
