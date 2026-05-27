# Orchestrator Global Dock — Remaining Gaps

> **Status**: Tracking doc for the global dock (branch
> `claude/elegant-fermat-vKK5G`). The dock ships as a non-modal,
> full-height left column: toggle via "Orchestrator: Toggle Dock";
> ↑↓ live-switch the active window (30ms debounce) with a directional
> whole-window wipe; Enter/Esc/editor-click blur to the editor with the
> dock pinned; mouse click selects+activates a row; wheel over the dock
> is consumed.

## Open gaps

### Rendering / z-order (core)
- ~~Full-screen modals overlap the dock.~~ **Fixed**: Settings,
  calibration wizard, keybinding editor, and event-debug now dim +
  render against `chrome_area`, so they sit beside the dock instead of
  being overpainted by it. The workspace-trust dialog still uses `size`
  — it's a startup gate that can't be concurrent with the dock.
- ~~Some global popups still use full-screen coords.~~ **Fixed**: the
  command-palette suggestions, Live Grep overlay, and menu dropdowns
  (chrome layout), plus `render_top_global_popup` (global popups) and the
  per-buffer LSP hover/completion popups now clamp to `chrome_area`, so
  they can't overrun the dock column. (The popup fixes mirror the
  verified overlay fixes; the LSP/global popups couldn't be triggered
  interactively here — no LSP server in the sandbox — but the change is
  the same `chrome_area` clamp and renders without regression.)
- **`last_frame_width/height` store full `size`,** not `chrome_area`, so
  macro-replay / `recompute_layout` lays the chrome at the wrong width
  while the dock is up.

### Dock chrome (core)
- ~~Right-edge-only border + drag-resize.~~ **Fixed**: the dock draws
  only a right border (no top/left/bottom — content reclaims those
  rows/cols); the right border is draggable to resize the width, and the
  chosen width persists across hide/show within the session (`Editor.
  dock_width`). Cross-session persistence (config) is still TODO.

### Dock UX (plugin)
- ~~No "show empty/1-file sessions" toggle in the dock.~~ **Fixed**: the
  dock now renders a "show empty/1-file" toggle (default off — hides
  trivial sessions), wired to the same `hide-trivial` filter as the
  modal.
- **Diving into a *switched* session focuses the file explorer, not the
  buffer.** When you arrow to a different session and press Enter, the
  window activates with its file-explorer pane focused, so the first
  keystrokes filter the tree instead of editing — you must Ctrl+E / click
  into the buffer first. (Editing the *current* session works seamlessly:
  open a file → dock → dive → type all flows into the buffer.) Likely in
  the window-activation focus-restore layer rather than the dock; verify
  whether `set_active_window` should land focus on the last editor pane.
- **Attention glyph (⚑).** No reliable per-session "agent waiting /
  exited" signal exists in the session model yet, so the wireframe's
  attention indicator is not implemented. Needs a real state source
  (e.g. track `terminal_exit` / idle) before adding the glyph.
- **Project grouping.** The dock shows a flat list with a per-row
  project tag (like the modal), not collapsible project group headers
  (the `list` widget is flat; grouping needs interleaved header rows +
  selection-index remapping).
- **New-session from the dock closes it** (`openForm` →
  `closeOpenDialog`); arguably the dock should persist / reopen after.
- **Detail strip is one line** (branch only). The richer
  age / pgid / last-terminal-line detail (`buildPreviewEntries`) is not
  surfaced in the dock to keep the list-fill height maths exact.

### Misc
- **Toggle keybinding unbound** (intentional — "decide later"). Only
  reachable via the command palette today.
- **No automated tests** for `FloatingPanelControl`,
  `SetActiveWindowAnimated`, or the dock behaviours.

## Done
Non-modal dock placement + layout carve; focus/blur key + mouse routing;
list fills height with pinned hint; live-switch + whole-window
directional wipe; worktree toggle, scope, filter, inline
Stop/Archive/Delete + in-place Delete confirm; wheel consumption.
