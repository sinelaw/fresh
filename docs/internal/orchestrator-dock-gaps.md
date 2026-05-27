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
- **Full-screen modals overlap the dock.** Settings, calibration
  wizard, keybinding editor, and the workspace-trust dialog still
  render against the whole screen and dim it (`render.rs` ~1482–1574
  use `size`, not `chrome_area`). With the dock visible the dock column
  draws over their left edge (dock is painted last). Either confine
  these to `chrome_area` or suppress the dock while one is open.
- **Centered/anchored popups use full-screen coords.** Command-palette
  suggestions and global popups (`render_prompt_popups`,
  `render_top_global_popup`) are positioned/clamped against `size`, so
  with the dock up they can be offset or overrun the dock column.
- **`last_frame_width/height` store full `size`,** not `chrome_area`, so
  macro-replay / `recompute_layout` lays the chrome at the wrong width
  while the dock is up.

### Dock chrome (core)
- **Right-edge-only border + drag-resize.** The dock currently draws a
  full box (all four borders). It should draw only a **right** border
  (no top/left/bottom — reclaim those rows/cols for content), and that
  right border should be **draggable** to resize the dock width (persist
  the chosen width). Today the width is a fixed constant.

### Dock UX (plugin)
- **No "show empty/1-file sessions" toggle in the dock.** Master added
  this filter to the modal picker (default off); the dock shares
  `filterSessions` so it hides empty/1-file sessions too, but
  `buildDockSpec` has no control to reveal them — a freshly-launched
  session with no edits never appears in the dock. Add the toggle.
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
