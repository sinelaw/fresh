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
- [ ] Phase 2: typed `ChromeModel` + `Editor::chrome_model()`.
- [ ] Phase 3: TUI chrome renderers consume the model.
- [ ] Phase 4: `Scene` umbrella + Tauri transport.
