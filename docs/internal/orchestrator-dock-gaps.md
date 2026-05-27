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
- **New-session from the dock closes it** — *Fixed (P1).* The host now
  holds two independent widget-panel slots (`PanelSlot::Dock` +
  `PanelSlot::Floating`, see `app/mod.rs`); the dock mounts into the Dock
  slot (`mountFloatingWidget(..., asDock=true)`) and a centered modal
  (the New-Session form) into the Floating slot, so `+ New` / `Alt+N`
  leaves the dock visible beside the form. Input/mouse/wheel route to the
  focused slot, with the centered modal taking precedence over the dock.
  The dock has its own `KeyContext::Dock`. See
  [Design: dock + modal coexistence](#design-dock--modal-coexistence).
- **Detail strip is one line** (branch only). The richer
  age / pgid / last-terminal-line detail (`buildPreviewEntries`) is not
  surfaced in the dock to keep the list-fill height maths exact.

### Misc
- **Toggle keybinding unbound** (intentional — "decide later"). Only
  reachable via the command palette today.

## Design: dock + modal coexistence

We only want *principled, final* solutions here — no slot-juggling or
reopen-after-close workarounds. All correct options start from the same
ontology: **the dock is persistent, non-modal, owns a fixed layout
region → it is chrome, not an overlay.** Today's bug exists only because
it was built as a modal-ish floating panel sharing one host slot with the
orchestrator's centered form/picker. (Settings, the keybinding editor,
popups, and menus live in *separate* state and already coexist with the
dock — the conflict is isolated to the orchestrator's own panels.)

### Invariant that must be preserved (this is what we have today)
The two-tier chrome must keep working exactly as it does now:

- **Editor-global carve, once per frame** (`compute_dock_split` →
  `dock_area` + `chrome_area`), independent of `active_window`.
- The dock is **editor-global** (owned by `Editor`, shows *all*
  sessions, persists across switches) — *not* per-window like the file
  explorer. It renders into `dock_area`.
- The active window (menu / tabs / explorer / splits / status) renders
  **independently** into `chrome_area`; switching sessions only swaps
  what's in `chrome_area`.
- The **whole-window wipe** on switch runs on the *newly-active window's*
  animation manager, **scoped to `chrome_area`**. Because `dock_area` and
  `chrome_area` are disjoint, the dock stays static while the window
  slides; the animation's "before" snapshot is bounded to the animated
  rect, so the dock is never captured/moved.
- **Hard rule:** the dock region must never be inside any window's
  animation rect. Falls out naturally as long as the dock stays
  editor-level and the carve runs first.

Any solution below must preserve all of the above — they change only
*how the dock is hosted/focused* and *how a centered modal coexists*,
never the carve or the animation scoping.

### Option P1 — dock as first-class editor-global chrome (recommended)
Make the dock an editor-level chrome region with its own
`KeyContext::Dock` (modeled on `KeyContext::FileExplorer`, but
editor-global, not per-window), hosting a declarative widget spec.
Centered modals stay as the single overlay layer *above* chrome.

- **Why correct:** the dock genuinely *is* chrome; the explorer proves
  the "persistent, non-modal, focusable region that coexists with every
  modal/popup" pattern. Coexistence becomes automatic and uniform.
- **Focus model:** reuses the established `KeyContext` precedence
  (Settings > Menu > Prompt > Popup > … > Dock/FileExplorer/Normal). No
  new focus subsystem, no per-call-site `focused` flags; dock blur/dive
  is just a context transition back to the editor split.
- **Preserves today:** same carve, same `chrome_area`-scoped animation;
  only dock hosting + focus change. A centered modal then renders over
  `chrome_area` while the dock keeps rendering in `dock_area`.
- **Bonus:** also resolves two other open gaps — "dive focuses explorer
  not buffer" (becomes a normal context transition) and the
  `last_frame_width` macro-replay bug (dock width is computed once,
  authoritatively, as part of the layout).
- **Cost:** needs a layout-region abstraction that hosts a widget spec
  (the explorer is bespoke; splits host buffers); re-plumb the dock's
  render/mouse/scrollbars off the floating path. Medium-large, but it
  *removes* the "dock as floating panel" code rather than adding slots.

### Option P2 — unified layer compositor
Replace the ad-hoc overlay/focus zoo (`floating_widget_panel`,
`settings_state`, `keybinding_editor`, the global/buffer popup stacks,
`menu_state`, the prompt) with one ordered **layer stack**. Each layer
declares a *region* (full-screen-centred, left-dock column,
cursor-anchored, …), a *focus policy* (modal / non-modal / passive), and
*paint order*; a single `focused_layer` pointer is the keyboard target,
and input + mouse dispatch walk layers top-down by region.

- **Why correct:** the genuinely general model — focus precedence,
  z-order, and hit-testing become *properties of layers* instead of
  conditionals scattered across `render.rs` / `input.rs` /
  `mouse_input.rs`. Dock + modal + popup coexistence is intrinsic, and it
  also fixes the existing per-slot inconsistencies (each current overlay
  reinvents dim / focus / mouse).
- **Preserves today:** the editor content is the bottom layer, the dock a
  non-modal `LeftDock`-region layer, modals modal layers, popups passive
  anchored layers. The window-switch wipe is still scoped to the window's
  region (the dock layer's region is disjoint), so persistent dock +
  independent animated window is intrinsic to the model.
- **Cost / risk:** the largest change; touches the whole overlay + input
  subsystem; high migration risk; must define the focus policy
  rigorously (the original design avoided a stack for exactly this
  reason). It also introduces a focus model that must *subsume* — not sit
  beside — the established `KeyContext` precedence, or it's less
  principled than P1.

### Recommendation
**P1** is the correct minimal-principled answer (dock = chrome, reuse
`KeyContext`, delete the floating-panel dock code, coexistence by
construction, today's carve+animation preserved). **P2** is the eventual
destination only if a future need demands arbitrary layered panels and we
choose to consolidate the overlay zoo; if so, P1's dock-as-chrome should
be a layer within it. A hybrid (P1 for the dock now + a later
`OverlayManager` unifying just the modal/popup zoo) is a reasonable
staged path to P2.

## Done
Non-modal dock placement + layout carve; focus/blur key + mouse routing;
list fills height with pinned hint; live-switch + whole-window
directional wipe; worktree toggle, scope, filter, inline
Stop/Archive/Delete + in-place Delete confirm; wheel consumption.
