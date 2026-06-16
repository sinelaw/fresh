# Fresh — Unified Scene rendering (TUI + web/Tauri from one model)

**Status:** Design + in-progress implementation. Branch `claude/non-terminal-ui-research-fir1y9`.
**Date:** 2026-06

## Problem

Today the view layer has a *double flow*:

1. `Editor::render(frame)` draws **everything** — buffer interiors **and** chrome (menu
   bar, menu dropdown, tabs, status bar, command palette / suggestions, popups) — into a
   ratatui **cell** buffer.
2. The web bridge (`crates/fresh-editor/src/webui`) renders buffer interiors from those
   cells (as SVG), but renders chrome as **native HTML** from semantic models it
   *re-extracts* from the per-frame layout caches.

So chrome is produced **twice** — once as cells (for the TUI) and once as a semantic model
(for the web) — and because the pipeline still paints chrome into the cells, the web
frontend has had to *hide* the cell-drawn chrome (cover panels, blank rects). That's the
"hack" we're removing.

## Goal

One **semantic Scene** that both backends consume:

```
Editor state ──► build Scene ──► { panes: cells ,  chrome: model }
                                   │                 │
                  TUI/GUI ◄────────┘                 └────────► web/Tauri
                  (chrome model → cells,            (panes → SVG,
                   composited over panes)            chrome model → HTML)
```

- **Buffer/pane interiors** stay as the cell pipeline's output. This is *not* a double
  render — it is one cell render consumed by both (TUI shows the cells; web draws the same
  cells as SVG text). Re-implementing the syntax/wrap/view-line engine as a semantic model
  is a separate, much larger effort and is explicitly **out of scope** here.
- **Chrome** (menu bar, dropdown, tabs, status bar, palette/suggestions, popups) becomes a
  typed `ChromeModel`. Both backends *render that model*: the TUI/GUI renders it to cells;
  the web renders it to HTML. Single source of truth, no double-render, no hiding.

## Architecture

```rust
// core (crates/fresh-editor)
pub struct Scene {
    pub panes:  Vec<PaneView>,   // buffer interiors as cell regions (+ scrollbars)
    pub chrome: ChromeModel,     // semantic; rendered by each backend
}
pub struct ChromeModel {
    pub menubar:  MenuBarModel,  // labels + open/highlight/submenu + dropdown items
    pub tabs:     Vec<TabBarModel>,
    pub statusbar: StatusBarModel,   // ordered labeled segments
    pub palette:  Option<PaletteModel>,
    pub popups:   Vec<PopupModel>,
}
```

The cell pass draws **only** panes (+ scrollbars/separators). Chrome is emitted as
`ChromeModel`, never into cells, when in "scene" mode.

- **TUI/GUI compositor**: `render_chrome_model_to_cells(frame, &ChromeModel)` — the menu /
  tab / status / palette renderers move *behind* the model (they take the model, not raw
  editor state). The terminal main loop becomes: draw panes → composite chrome model.
- **Web bridge**: serialises `ChromeModel` to JSON (it already does, ad-hoc, in
  `scene_json`); the typed model replaces the hand-rolled extraction.

## Phased plan (each phase keeps the TUI working + tests green)

- **Phase 1 — suppression seam (in progress).** Add a render flag so the cell pass can
  *skip drawing* the chrome we render natively while still populating the existing layout
  caches/models. The web bridge turns it on, so `render_to_buffer` yields **pane-only
  cells** (no chrome bleed) and the frontend cover/blank hacks are deleted. The TUI leaves
  the flag off → unchanged. This is the exact "panes cells + chrome model" seam the Scene
  formalises.
- **Phase 2 — typed `ChromeModel` in core.** Lift the bridge's ad-hoc JSON extraction into
  typed structs + `Editor::chrome_model()`. Bridge consumes the typed model (behaviour
  identical). Web tests green.
- **Phase 3 — TUI renders chrome from the model.** Refactor MenuRenderer / TabBar /
  StatusBar / Suggestions to take `ChromeModel` parts and render them to cells. The TUI now
  draws chrome *from the same model the web uses* → the double flow is gone; Phase-1
  suppression + this compositor are the only chrome paths.
- **Phase 4 — `Scene` umbrella + Tauri.** Wrap panes + chrome in `Scene`; the Tauri shell
  consumes the same model over IPC instead of HTTP.

## Status

- [x] Phase 0: web bridge renders chrome as native HTML from re-extracted models;
  frame-pump parity with the TUI loop (`editor_tick`); plugin runtime enabled
  (git, orchestrator, …) so the web build is as full-featured as the TUI.
- [x] Phase 1: `suppress_chrome_cells` render flag threaded through MenuRenderer /
  SuggestionsRenderer; pipeline records chrome layout but skips drawing it to
  cells; web frontend cover/blank hacks deleted. TUI unchanged (flag off).
- [x] Phase 2: shared semantic projections in the core (`view/scene.rs`).
  `Editor::menu_view()`, `tab_bar_view()`, `status_view()`, `palette_view()` and
  `popups_view()` are the single derivations of the menu tree / tabs / status
  segments / palette / popups (completion, hover, action, list, text). The web
  bridge only *serializes* them — every bespoke chrome builder is gone, and the
  frontend has ZERO cell-drawn chrome (buffer interiors only). Geometry comes
  from the pipeline's layout caches so clicks/scroll route back through the
  existing `handle_mouse` hit-testers.
- [x] Phase 1b — comprehensive layout-vs-draw seam (the "don't draw what this
  frontend doesn't need" model). Every chrome renderer now computes its
  layout/geometry/semantic model but paints cells only when the active frontend
  draws chrome itself; the web pipeline yields **pane-only cells** with no
  draw-then-cover. Done for: menu, suggestions, overlay prompt (frame/title/
  toolbar/separator/footer/preview), file-explorer sidebar, popups (local +
  global), workspace-trust dialog, and the tab bar (threaded through
  SplitRenderer). The **status bar** was the last cell-scraper: `status_view`
  read text back out of the drawn cells; now `StatusBarRenderer` captures a
  semantic `StatusBarLayout.segments` model (name/text/position per element)
  that `status_view` reads directly, and its draw is gated like the rest. For the
  TUI every guard is a no-op (draw=true) so cell rendering is byte-identical;
  verified across menu/overlay/explorer/popup/tab/status e2e suites + the web
  suite. Settings + remaining widget surfaces (panels/dock) still draw to cells
  pending their own native projections.
- [~] Phase 3: TUI renderers consume the same shared content as the projections,
  so the content logic lives in exactly one place (geometry/`MenuLayout` stays the
  renderer's output). Done for the **menu**: `MenuRenderer::render` now takes the
  expanded menu list from `Editor::all_menus_expanded()` — the single source
  shared with `menu_view()` — and item state goes through the shared
  `is_menu_item_enabled`/`is_checkbox_checked` helpers + the same
  `find_keybinding_for_action`. Verified byte-identical via the
  `menu_render_golden` TUI snapshot (e2e). The cell vs HTML rendering itself
  legitimately differs per frontend (the intended boundary). Tabs/status/palette/
  popups already read their content from a single source (buffer metadata /
  prompt state / popup structs / render output), so no second derivation remains.
- [~] Phase 5: remaining cell-drawn surfaces → native semantic UI. The goal is
  ZERO cell-drawn chrome of any kind. Each gets an `Editor::*_view()` projection
  in `view/scene.rs`, native rendering in the frontend, and interactions routed
  back through the existing `handle_mouse`/`handle_key` at the cached cell rects.
  - [x] **File explorer** sidebar — `file_explorer_view()` (tree rows: name,
    depth, is_dir, expanded + selection/scroll); native tree, click/scroll route
    to the existing file-explorer hit-test. (e2e: explorer suite + drive.)
  - [x] **Workspace-trust dialog** — `trust_dialog_view()` (title/path/triggers,
    3 radio options + selection, OK/Quit) from `TrustDialogLayout`; native modal
    with a scrim; options/OK/Quit route to `handle_workspace_trust_mouse` at the
    cached rects, keyboard via `handle_key`. (e2e: trust suite + drive.)
  - [x] **Plugin widgets API** — done. `WidgetSpec` is serde-serializable, so:
    (a) the overlay **prompt toolbar** is projected on `PaletteView.toolbar` and
    rendered by a recursive `widgetEl` (Row/Col/Toggle/Button/Spacer/Divider/
    HintBar/LabeledSection/Text/List/Raw); Toggle/Button clicks route via
    `/widget` → `toggle_overlay_toolbar_widget`. (b) **Floating + dock panels**
    (e.g. the orchestrator session dock) project via `Editor::widgets_view()`
    (raw spec + instance state + on-screen rect + the panel's `HitArea` list);
    the frontend renders them natively and forwards a clicked hit's *index* to
    `/widget` → `deliver_widget_hit_by_index`, which runs the **same**
    `deliver_widget_hit` path (focus / tree-expand / list-select / fire
    `widget_event`) as a TUI cell click — extracted from `click_handlers` so both
    share it. `render_floating_widget_panel` obeys the layout-vs-draw seam
    (computes `last_inner_rect`, paints nothing when suppressed). **Mounted
    virtual-buffer panels** need no work: they render as ordinary buffer (pane)
    cells and clicks already route through the buffer widget hit-test. Verified:
    live-grep toolbar 7/7, dock 8/8 (button + toggle flip through the real
    plugin), drive 41/41; TUI dock/widget e2e (orchestrator_dock 40,
    dock_panel_routing 2, widget_panel_ownership 1) unchanged.
  - [ ] **Settings UI** — independent of widgets (overlap check: they share only
    `view/controls/*`; Settings has 5 bespoke controls — DualList/Map/TextList/
    Json/Complex — plus nullable/layer/category-tree/search/entry-dialogs that
    widgets lack, so it can't ride on widgets). `Editor::settings_view()` from
    `settings_state` + `active_chrome().settings_layout` (cached). Tagged
    `ControlView` mirrors `SettingControl` with per-control sub-rects from
    `ControlLayoutInfo`; clicks route to `handle_settings_mouse` at sub-rect
    centers, text edit forwards keystrokes to `handle_settings_input`. First cut:
    toggle/number/dropdown/text fully interactive; map/duallist/json/objectarray
    + entry dialogs read-only fallback, then iterate. Large.
  - [~] **Calibration wizard** — deprecate (not converting).
- [ ] Phase 4: `Scene` umbrella + Tauri transport.
