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
- **Text is selectable everywhere.** Buffer selection is the editor's own
  (drag → real selection). Live terminals select through the core too: a
  drag on the grid drops the split into read-only scrollback (the
  Ctrl+Space view, pixel-identical) and starts a real editor selection —
  Ctrl+C copies it, Ctrl+Space resumes the shell; a bare click still just
  focuses. Programs that enabled mouse reporting (DECSET 1000/1002/1003)
  keep receiving the mouse; Shift+drag bypasses them to select anyway.
  And **holding Alt** suspends all forwarding and lets the *browser* own
  the mouse: drag/double-click build a native selection over any text on
  the page — terminals, file explorer, menus, dialogs — with Ctrl+C
  copying it (SVG grid selections are rebuilt row-aware so multi-line
  copies keep their newlines). Every piece is opt-out: **Ctrl+Alt+S**
  toggles the Alt-selection mode (a frontend view preference, persisted
  in localStorage like zoom), and `terminal.mouse_drag_selects` /
  `terminal.mouse_forwarding` in the editor config govern the
  terminal-side behavior.

## Source layout — one page, split by concern

The served page is **one fully self-contained HTML file**, but it is
*authored* here as separate sources:

```
web-ui/
  shell.html    document skeleton: <head>, body markup, style/script markers
  css/          NN-name.css — concatenated (in filename order) into <style>
  js/           NN-name.js  — concatenated (in filename order) into <script>
```

`crates/fresh-editor/build.rs` (`assemble_webui`) concatenates each directory
in **filename order** and splices the results into `shell.html`'s marker
comments, writing `$OUT_DIR/webui-index.html`; `webui/mod.rs` embeds that via
`include_str!`.

**Order is load-bearing.**

- **CSS**: later files deliberately override earlier ones (the polish pass,
  the navy/teal skin, and the COSMOS shell each re-skin rules that precede
  them). Renaming a file so it sorts differently *changes the cascade*.
- **JS**: all files are concatenated into a single classic `<script>` and
  share one top-level scope; `function` declarations hoist across file
  boundaries, but `const`/`let` bindings must be declared in a file that
  sorts before their first top-level *use*.

Pick a numeric prefix that places a new file where its concern belongs; gaps
(10, 20, 35…) are left so insertions don't require renames.

| CSS | concern |
|---|---|
| `10-base.css` | typography, design tokens, resets, `#app` geometry |
| `20-chrome.css` | menu bar/dropdowns, tabs, status bar |
| `30-palette.css` | command palette / picker |
| `35-editor.css` | file explorer, pane surfaces, separators, scrollbars, caret |
| `40-widgets.css` | trust dialog, plugin widgets, Settings, keybinding editor, aux modals |
| `50-popups.css` | completion/hover/action popups, cell SVG, `#err` |
| `60-polish.css` | visual polish pass: radii, hairlines, hovers, selection pills |
| `70-mobile.css` | mobile / portrait touch shell |
| `80-skin.css` | navy/teal orchestrator skin |
| `90-cosmos.css` | COSMOS shell (the Cosmos web theme): wallpaper, device bezel, glass dock, motion |
| `91-theme-switch.css` | the web-theme switcher pill + drop menu (token-driven, re-skinned per theme) |
| `92-theme-macos.css` | macOS web themes (Light + Dark): title bar + traffic lights, vibrancy, SF font — one token-driven structural pass, gated on `body.macfam` |
| `94-theme-compact.css` | Compact web theme: full-bleed, dense spacing, flat surfaces |

| JS | concern |
|---|---|
| `10-core.js` | cell↔px metrics, zoom, web-theme density scale, shell geometry, DOM helpers |
| `15-theme.js` | web-theme system: registry, `applyWebTheme`, `setWebTheme`, the switcher |
| `20-cells.js` | icon set, cell-grid SVG renderer, TUI theme → CSS variables |
| `30-render.js` | per-region DOM patching, motion (FX), `render()` |
| `40-menu.js` / `45-tabs.js` / `55-status.js` | native chrome builders |
| `50-palette.js` | palette / picker / prompts + file-browser band |
| `60-popups.js` | native popups |
| `65-widgets.js` | plugin widget tree + Settings / keybinding editor / aux modals |
| `70-panels.js` | trust dialog, file explorer, border drag handles |
| `75-app.js` | mobile shell + transport (WS frames, resize, clipboard) |
| `80-input.js` | keyboard/mouse/touch input, native selection, boot |

## Web themes (chrome look) vs the TUI colour theme

The editor's **TUI colour theme** owns every *buffer* cell (syntax colours,
selection, cursor) and is piped into the chrome's CSS variables by
`applyTheme()` (js/20-cells.js) — it is editor state, shared with the TUI.

Layered on top is a **web theme**: a purely frontend choice of *chrome* look,
in the same class as zoom / palette placement / Alt-selection — a view
preference persisted in `localStorage` (`fresh.webtheme`), never sent to the
editor. Four ship:

- **Cosmos** (default) — the abstract wallpaper, `COSMOS-991` hardware bezel and
  frosted-glass dock (`90-cosmos.css`). Unchanged from before the theme system.
- **macOS Light / macOS Dark** — a native-feeling desktop app: a title bar with
  traffic lights + the document name, the SF system (proportional) chrome font,
  light or dark vibrancy panels, the system accent blue and a Finder-style
  source-list dock. Both variants are one token-driven structural pass gated on
  `body.macfam`; only their colour tokens differ.
- **Compact** — a dense, chrome-light IDE: no wallpaper/bezel, a ~8% smaller
  measured grid, tight paddings, hairline rules and flat surfaces.

`js/15-theme.js` is the whole mechanism. `applyWebTheme()` (called from
`render()` right after `applyTheme()`) does two things: it sets the
`theme-<name>` class on `<body>` (plus `macfam` for the two macOS variants) —
which drives all *structural* CSS (`92/94-theme-*.css`, plus the Cosmos-gated
wallpaper/bezel/dock rules in `90-cosmos.css`) — and it layers the theme's
*chrome colour tokens* inline over `applyTheme()`'s TUI-derived values (`--bg`
is never overridden, so the buffer stays the TUI theme's). The buffer's
`svg.cells` are pinned to `--mono-family`, so a theme may repoint the chrome's
`--font-family` at a proportional stack without disturbing the monospace grid.

Users switch it three ways, all frontend-owned: the floating **theme pill** in
the top-right corner (a menu of the four), the **Ctrl/Cmd+Alt+T** chord
(Shift reverses), and the mobile **⋮** sheet's "Theme:" row. The choice is
exposed on `window.fresh` (`setWebTheme` / `cycleWebTheme` / `webTheme` /
`webThemes`) for drivers and tests.

When building or reviewing a theme, walk
`docs/internal/web-ui-theme-checklist.md` — an element-by-element visual
checklist of every chrome surface (dock, explorer, buffer, menus, palette,
popups, settings, …) and the dimensions to verify (margins, colours, padding,
text, icons, gaps, borders, selection/hover/focus states), plus the macOS
reference values and the gotchas (never inherit `--bg` on a control; keep the
buffer monospace; inset menu highlights via `::before`; selection language per
surface).

## Architecture (taps the real render pipeline)

```
browser (assembled web-ui page) ══WS /ws══► fresh::webui bridge ──► real Editor
  chrome  = native HTML from  ◄══ push:     runs Editor::render    (piece tree,
  scene.rs projections          hello (full scene)  into a cell     highlighter,
  buffer  = real highlighted    frame (region diffs) buffer, reads   handle_key, …)
  CELLS (SVG)                 input ══► {type:key|mouse|action|…}
                              ──HTTP──►  GET /state, POST /action … (curl + harness; origin-gated)
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
**Many clients, one editor (shared-view mirroring).** Every `/ws` upgrade joins
the session — a second tab, another device, or the page you load after a server
restart all connect and stay live (a stale/half-open tab can no longer lock out
a new page, the old first-come-`409` failure mode). The scene is built once per
tick and pushed to each client as its own region diff; input is accepted from
all of them and applied to the one editor. Because the clients have different
window sizes but there is one grid, the render is fit to the **smallest**
client's viewport (`effective_size`) so it fits every window — bigger windows
letterbox. A liveness heartbeat (ping every 15 s, reap after 45 s of silence;
browsers auto-pong) reaps a half-open peer so it can't linger or pin the grid to
a dead small window's size. A foreign `Origin` still gets `403`; on disconnect
the page retries with backoff and resyncs from the next hello. Independent
per-client cursors/viewports are the deeper §3.7 work, still PLANNED.
The frontend's input rides the WebSocket, but every HTTP route still answers as
before (full scene): `GET /` (page), `GET /state` (used by the frontend's manual
`refresh()` resync, `run.sh`'s readiness poll, and curl), and the `POST` input
routes (`/key` `/mouse` `/action` `/widget` `/settings` `/kbedit` `/paste`
`/resize`) plus the parity-harness `/step` `/reset` — the Playwright suite and
curl drive the editor through these. State-mutating `POST`s are gated by the
**same** same-origin/Host check as the `/ws` upgrade, so a cross-origin browser
page can't drive the editor over HTTP; non-browser callers send no `Origin` and
pass. An HTTP-side mutation is pushed to the connected browser as a diff.

## Run it

The bridge ships in the main `fresh` binary behind the opt-in `web` feature,
which embeds the assembled page so the build is self-contained. Build with
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
compile-time-embedded page — there is no on-disk fallback, so editing the
frontend requires a rebuild.

> ⚠️ The bridge binds plain localhost HTTP and hosts a live editor with
> filesystem access. It's a local-development prototype, **not** for exposure on
> a shared interface.

## Headless test (Playwright)

`test/drive.mjs` drives the **real** bridge in headless Chromium: it asserts the
buffer interior is the pipeline's real syntax-highlighted cells while all chrome
is native HTML (no cell-drawn chrome), that key / mouse / menu / palette /
settings / widget interactions run through the real `Editor` (over the
WebSocket input path), and that the push transport behaves: server-pushed
frames without page input, region diffs on typing, idle silence, and
shared-view mirroring (a second WebSocket joins and gets its own hello, both
clients receive broadcast frames from either one's input, and the shared grid
shrinks to the smallest client's viewport then grows back when it leaves) —
plus per-region DOM patching (a typing frame rebuilds
only its pane), measured metrics + app zoom (Ctrl+= / Ctrl+0, hit-testing
while zoomed), touch pan/tap in a `hasTouch` mobile context, and the
selection model: a drag on a live terminal grid becomes a real editor
selection in read-only scrollback (Ctrl+C copies it through the editor
clipboard, Ctrl+Space resumes, a bare click only focuses), and Alt-hold
native browser selection over the SVG grid. It also drives the **web-theme
switch** (Cosmos ↔ macOS Light ↔ macOS Dark ↔ Compact): the body class, the bezel/title-bar swap,
the buffer staying monospace under a proportional chrome font, the denser
Compact grid, `localStorage` persistence, and the switcher menu.
**171 assertions** across the chrome surfaces, plus screenshots.

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
