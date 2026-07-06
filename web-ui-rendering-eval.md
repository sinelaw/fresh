# Web UI Rendering Evaluation

**Subject:** `web-ui/index.html` + `crates/fresh-editor/src/webui/mod.rs` (the "real render pipeline" web bridge)
**Date:** 2026-07-06
**Method:** Code review of the bridge, the scene projections and the frontend; full build of `webui_server`; live bridge on `127.0.0.1:8141` driven with headless Chromium (Playwright 1194); the bundled `web-ui/test/drive.mjs` suite; custom probes (splits, selection, unicode, themes, mobile breakpoint, latency/payload measurements); the Rust `scene_parity` test.

---

## Summary

The web UI is in **very good shape for what it claims to be** — a local-development, single-client, no-mocks frontend driven end-to-end by the real Rust `Editor`. Every automated check passes (50/50 Playwright assertions, `scene_parity` green), and visual inspection of eleven chrome surfaces plus six custom probes found rendering to be correct, theme-faithful, and surprisingly polished (the Settings and Keybindings modals look like a real desktop app, and there is a working mobile touch shell).

**Overall rating: 8/10 as a prototype.** The rendering layer is essentially done and architecturally sound. The remaining gaps are input-side (no IME path on desktop, no OS clipboard), scalability-by-design limits of the bridge (one client, full-scene JSON per keystroke, full DOM rebuild per frame), and some documentation rot.

---

## Architecture (verified against the code)

```
browser (web-ui/index.html)  ──HTTP──►  webui bridge (single thread)  ──►  real Editor
  chrome  = native DOM/CSS      GET  /state      Editor::render into an
  from scene.rs projections     POST /key        in-memory ratatui Buffer;
  buffer  = the pipeline's      POST /mouse      suppress_chrome_cells=true
  real cells, drawn as SVG      POST /action     so cells carry pane
  text pinned per cell column   POST /resize …   interiors only
```

The design promise — *nothing semantic is re-implemented* — holds up under inspection:

- `crates/fresh-editor/src/view/scene.rs` (~1,500 lines) derives twelve serializable projections (`menu_view`, `tab_bar_view`, `status_view`, `palette_view`, `popups_view`, `file_explorer_view`, `trust_dialog_view`, `widgets_view`, `context_menu_view`, `aux_modals_view`, `keybinding_editor_view`, `settings_view`) that the TUI renderer *also* consumes. Enabled/checked state, accelerators, visibility conditions all come from the same functions the TUI uses.
- The bridge (`webui/mod.rs`, ~820 lines) runs the **actual** `Editor::render` into a `TestBackend` buffer each request, slices pane interiors (gutter emitted as a separate block from buffer text), and serializes the projections. Input goes through the real `Editor::handle_key` / `handle_mouse` / `dispatch_settings_hit` — the same code paths as the terminal.
- Every request runs `editor_tick`, so async LSP/plugin/file events and animations advance exactly like the TUI loop; the scene carries a `poll.active` pacing hint and the frontend polls at 40ms when active, 500ms when idle (verified: idle polling is throttled, not a busy loop).
- `tests/scene_parity.rs` drives one editor through both renderers and asserts the web scene's chrome also appears in the TUI cells. **Passes** (`1 passed; 0 failed`, ~1s).

This is a genuinely strong foundation: the classic failure mode of second frontends (drift from the primary) is structurally prevented rather than tested after the fact.

## Automated test results

- `web-ui/test/drive.mjs` against a live bridge in headless Chromium: **50 passed, 0 failed**, zero JS page errors. It verifies buffer interiors are real highlighted cells (≥3 syntax colors), all eleven chrome surfaces are native HTML with no SVG/cell leakage, and that menus, palette, popups, explorer, trust dialog, plugin toolbar/dock widgets, keybinding editor and Settings all round-trip through the real editor.
- `cargo test -p fresh-editor --test scene_parity`: **passes**.

## Visual inspection (screenshots, 1280×800 @2x)

| Surface | Verdict |
|---|---|
| Buffer + gutter | Correct. Real syntax highlighting, line numbers in a separate cell block, fold arrows, crisp vector rules instead of dashed `│` glyphs, live caret from the pipeline's cursor cell. Per-glyph x-pinning keeps columns exact — no drift when highlight runs re-split. |
| Menu bar + dropdowns | Native, clean. Accelerators right-aligned, separators, disabled items greyed (e.g. Detach), submenu arrows. Dropdown rows sit at the exact cell rects the editor hit-tests. |
| Command palette | Native card with count (`1 / 265`), filtering via the real suggestion engine, key hints as chips. Live-grep toolbar renders plugin `WidgetSpec` toggles that actually flip plugin state. |
| Popups (status-bar Remote indicator) | Native card, correct anchor position near the status bar. |
| File explorer | Native tree, selection highlight, disclosure chevrons, bold directories. Async dir scan arrives via the frame pump. |
| Trust dialog | Native modal, 3 options. |
| Settings modal | The most impressive surface: category tree, per-item controls (toggles, dropdowns, steppers, map/list editors), layered entry dialog on top, footer buttons. All native, all dispatched through `SettingsHit` — the same enum a TUI cell click produces. |
| Keybinding editor | Full table (923 bindings), grouping header, Add Binding dialog. Minor: the footer hint row is slightly clipped by the modal's bottom edge. |
| Orchestrator dock | Native left panel (cards, buttons, filter field) beside the file explorer; editor pane correctly re-wraps at the narrower width. |
| Split panes | `split_vertical` → two panes, each with its own tab bar, gutter, soft-wrap and scrollbar; separator drawn between. |
| Mobile shell (<480px) | Real touch chrome: header (search/palette/keyboard/overflow), Termux-style sticky modifier row, symbol row, nav bar, status strip; grid re-fits (58×35). Hidden-input trick summons the soft keyboard; Android `beforeinput` fallback exists. |
| Themes | Switching to `light` restyles **all** native chrome via CSS variables seeded from the real `Theme` (with `color-mix` derived surfaces/hairlines, and luminance-picked text on accent fills). No stuck dark remnants observed. |
| Selection | Mouse drag produces real selection background runs across lines (~6,000 bg cells reported), matching the TUI. |
| Wide glyphs | `你 好 x🎉 yéz` typed via `/key` renders with correct 2-cell spacing in the SVG (verified visually — no overlap or drift). |

## Measurements (debug build, localhost)

- `/state` payload: **~41 KB** at 140×44; ~31 ms/request average over 10 requests.
- Key round-trip (POST `/key` → JSON scene): **min 31 ms / avg 36 ms / max 72 ms** from the page. Usable but noticeable; a release build would improve the render slice of this.
- 30 wheel events dispatch in ~1.8 s including coalescing (frontend batches `deltaY` into `n` steps — good).
- Idle: 2 polls/s (500 ms), each a full editor tick + render + 41 KB JSON + `JSON.stringify` comparison client-side; DOM untouched when the scene is unchanged (string-compare short-circuit) — caret blink doesn't restart, scroll positions of natively-scrolled panels are snapshot/restored across rebuilds.

## Gaps and issues (ranked)

### 1. No desktop IME / composition input path (Medium)
`document.keydown` is the only text-input path on desktop. Composed input — CJK via IME, dead-key accents — never produces usable `keydown.key` values (`"Process"`/`"Unidentified"`), and there are no `compositionend`/`beforeinput` handlers outside the mobile hidden-input. Typing 你好 into the desktop web UI is currently impossible even though the editor and the SVG renderer handle wide glyphs perfectly (verified via direct `/key` POSTs). The mobile path (`beforeinput` on the hidden input) already contains 90% of the needed logic; wiring a hidden input (or `contenteditable` sink) on desktop would close this.

### 2. No OS clipboard bridge (Medium)
There are no `paste`/`copy` event handlers. Ctrl+V forwards the literal key to the editor, which pastes from the *editor's internal* clipboard — text copied in another app can't be pasted into the web UI, and text copied in the editor can't leave it (`user-select:none` also blocks manual selection-copy of the SVG text). A `paste` listener feeding `ev.clipboardData` through the existing `/key`-per-char (or a new `/paste` route into the editor's paste action) would fix the inbound half.

### 3. Documentation rot (Low, cheap to fix)
- `web-ui/README.md` and `scene.rs`/`webui/mod.rs` doc comments reference `docs/internal/UNIFIED_SCENE_DESIGN.md` and `docs/internal/NON_TERMINAL_UI_RESEARCH.md` — both were deleted in c5e076b ("replace sprawling internal docs with code-verified architecture set"). The architecture set (`docs/internal/rendering-and-layout.md` etc.) doesn't cover the web UI at all; the only surviving design description is the web-ui README itself.
- `drive.mjs`'s header tells you to start the bridge with `crates/fresh-editor/src/view/chrome_snapshot.rs` — a file that doesn't exist (README correctly says `scene.rs`).

### 4. Test harness has an undeclared dependency (Low)
`drive.mjs` imports `playwright`, but no `package.json` declares it anywhere (the root one is the VitePress docs site), so the documented test command fails out of the box with `ERR_MODULE_NOT_FOUND`. A `web-ui/test/package.json` with `playwright` pinned (browsers are already pre-provisioned at `/opt/pw-browsers`) plus a `npm test`-style script would make it reproducible. There is also no CI wiring for either `drive.mjs` or (as far as the workflow files show) a regular `scene_parity` run.

### 5. By-design scalability ceilings (informational)
Acceptable for the stated "local development prototype" scope, but worth listing because they bound where this can go:
- **One client, single thread, `Connection: close`** — every poll is a fresh TCP connection; two clients interleave on one editor.
- **Full scene per interaction** — every keystroke serializes the whole 41 KB scene; every changed frame rebuilds the entire `#app` DOM (`innerHTML=""`) and re-emits every SVG glyph with a per-character `x` list. At 140×44 this is fine; at 4K-fullscreen grid sizes or with several splits it will scale linearly and start to hurt. Incremental per-region diffing (the scene is already region-structured) is the obvious next step if this graduates beyond a prototype.
- **Fixed cell metrics** (`CW=8.2`, `CH=18`, font-size 13) — no zoom/font-size control; browser zoom works but blurs the caret/cell alignment assumptions baked into `cellAt()` hit-testing (clicks are mapped by `clientX/CW`, which assumes 1:1 CSS-pixel scale).

### 6. Cosmetic nits (Low)
- Keybinding editor: bottom hint row clips mid-glyph against the modal's rounded edge.
- The status message area shows transient plugin noise on boot ("Activating direnv environment…") in every screenshot until something overwrites it — cosmetic, but it's the first thing a new user reads.

## What's notably good

- **The parity discipline.** `scene_parity.rs` + shared projections make TUI/web divergence a compile-or-test failure, not a bug class. This is the right architecture and it is actually enforced, not just documented.
- **Theme fidelity with taste.** Chrome colors are seeded from the live `Theme`, but hairlines/surfaces are derived via `color-mix` instead of piping raw terminal colors into borders — high-contrast themes stay readable (`--on-accent` luminance picking).
- **Careful DOM hygiene.** Scene string-compare to skip re-renders, scroll-position preservation across rebuilds, native scrolling for tall panels (settings/dock) with wheel events deliberately *not* forwarded to the editor for those, hover forwarding de-duped by cell.
- **The gutter/text split** in the bridge (separate cell blocks) is forward-looking — it's exactly what a future native selection layer needs.
- **Security posture is conscious**: same-origin only (no CORS header, with a comment explaining why), README warns the bridge is not for shared interfaces.
- **The mobile shell** goes far beyond what a prototype needed: sticky one-shot modifiers, soft-keyboard summoning, safe-area handling, breakpoint re-fit on rotation.

## Suggested next steps (in order of value per effort)

1. Fix the stale doc pointers (README, `scene.rs`, `drive.mjs` header) and add `web-ui/test/package.json` so the documented test command works from a clean checkout.
2. Add a desktop text-input sink (hidden input reusing the existing mobile `beforeinput` logic) → unlocks IME and dead keys.
3. Add a `paste` handler (and a copy path — e.g. write the editor selection to `navigator.clipboard` on copy actions) → real clipboard interop.
4. Wire `drive.mjs` and `scene_parity` into CI so the 50-assertion suite runs on PRs (the bridge builds in one cargo command; Chromium is a standard Playwright install).
5. When/if this outgrows "prototype": per-region DOM diffing and a persistent connection (SSE or WebSocket) before any multi-client work.
