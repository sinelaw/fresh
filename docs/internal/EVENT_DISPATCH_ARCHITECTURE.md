# Event Dispatch Architecture

This document describes the current state of mouse/keyboard event handling in Fresh, identifies architectural issues, and proposes improvements.

## Background

Fresh uses [ratatui](https://ratatui.rs/) for terminal UI rendering. Ratatui is intentionally a **rendering library only** - it does not handle input events. This is by design, as the maintainers want it to remain a library rather than a framework.

This means Fresh must implement its own event dispatch system to map screen coordinates back to UI components.

## Current State

### How It Works Now

1. **Layout** is calculated in `render.rs` using ratatui's constraint system
2. **Mouse events** are handled in `mouse_input.rs` with a mix of approaches:
   - **Cached layout lookups** (good): `cached_layout.tab_areas`, `cached_layout.status_bar_area`, etc.
   - **Hardcoded coordinates** (bad): `if row == 0` for menu bar

### What Works Well

Some components properly use `cached_layout` for hit testing:

| Component | Cached Layout Field | Notes |
|-----------|---------------------|-------|
| Tab bar | `tab_areas: Vec<(split_id, buffer_id, row, start_col, end_col, close_start)>` | Per-tab positions |
| Status bar | `status_bar_area: Option<(row, x, width)>` | With sub-indicator positions |
| File explorer | `file_explorer_area: Option<Rect>` | Area and resize border |
| Split separators | `get_separator_areas()` | For resize dragging |

### What's Broken

**Issue #832** exposed a fundamental problem: the menu bar check used hardcoded `row == 0`:

```rust
// BUG: Assumes menu bar is always at row 0
if row == 0 {
    // Handle menu bar click...
}
```

When `menu_bar_visible` is false, row 0 becomes the tab bar, but clicks were still being intercepted by the menu bar handler.

**Similar risks exist for:**
- Tab context menu positioning (uses `row + 1` without checking menu bar visibility)
- Any future component that assumes fixed row positions

## The Core Problem

There's no unified system for:
1. **Layout-to-event coordination**: Rendering produces positions, but event handling must independently know those positions
2. **Component visibility**: When UI elements show/hide, event handlers must be updated
3. **Hit testing**: No central "what's at this coordinate?" function

## Recommended Improvements

### Phase 1: Immediate Fixes (Low Risk)

Add `menu_bar_row` to cached layout for consistency:

```rust
pub struct CachedLayout {
    // Existing fields...
    pub tab_areas: Vec<(SplitId, BufferId, u16, u16, u16, u16)>,
    pub status_bar_area: Option<(u16, u16, u16)>,

    // NEW: Menu bar position (None when hidden)
    pub menu_bar_row: Option<u16>,
}
```

Then change hardcoded checks:
```rust
// Before
if row == 0 { ... }

// After
if self.cached_layout.menu_bar_row == Some(row) { ... }
```

### Phase 2: Unified Hit Testing (Medium Effort)

Create a central hit-test structure built during rendering:

```rust
/// A clickable/hoverable region on screen
pub struct HitArea {
    pub rect: Rect,
    pub target: HitTarget,
    pub z_index: u8,  // Higher = on top (for overlapping popups)
}

/// What can be clicked
pub enum HitTarget {
    MenuBarItem(usize),
    Tab { split_id: SplitId, buffer_id: BufferId, close_button: bool },
    EditorContent { split_id: SplitId },
    Scrollbar { split_id: SplitId },
    StatusBarIndicator(StatusIndicator),
    FileExplorer { item_index: Option<usize> },
    SplitSeparator { split_id: SplitId },
    Dialog { dialog_id: DialogId, element: DialogElement },
    // ...
}

impl CachedLayout {
    /// Find the topmost hit target at (col, row)
    pub fn hit_test(&self, col: u16, row: u16) -> Option<&HitTarget> {
        self.hit_areas
            .iter()
            .filter(|area| area.rect.contains(Position { x: col, y: row }))
            .max_by_key(|area| area.z_index)
            .map(|area| &area.target)
    }
}
```

Benefits:
- Single source of truth for "what's at this position"
- Rendering and event handling use the same data
- Z-index naturally handles popups/dialogs overlaying content
- Easy to debug (can dump hit areas for testing)

### Phase 3: Compositor Pattern (Larger Refactor)

For more complex UI (nested dialogs, transient popups, etc.), consider a compositor pattern like Helix:

```rust
pub trait Component {
    fn render(&mut self, area: Rect, frame: &mut Frame, hit_areas: &mut Vec<HitArea>);
    fn handle_event(&mut self, event: Event) -> EventResult;
}

pub enum EventResult {
    Consumed,
    Ignored,
    Callback(Box<dyn FnOnce(&mut Compositor)>),
}

pub struct Compositor {
    layers: Vec<Box<dyn Component>>,  // Back to front
}

impl Compositor {
    fn handle_event(&mut self, event: Event) {
        // Events propagate front-to-back until consumed
        for layer in self.layers.iter_mut().rev() {
            match layer.handle_event(event.clone()) {
                EventResult::Consumed => return,
                EventResult::Callback(cb) => { cb(self); return; }
                EventResult::Ignored => continue,
            }
        }
    }
}
```

Benefits:
- Modal dialogs naturally block events to layers beneath
- Each component handles its own events
- Push/pop layers for transient UI (menus, tooltips, dialogs)
- Clean separation of concerns

## How Other TUI Apps Solve This

### Helix Editor
- **Compositor with layer stack**: Events propagate front-to-back until consumed
- **Component trait**: Each UI element implements `handle_event()` and `render()`
- **Callbacks for state changes**: Components return closures that modify compositor state

### GitUI
- **Manual Rect tracking**: Stores rendered positions, checks coordinates in handlers
- **Focus-based routing**: Active component receives keyboard events

### rat-focus / rat-event (Ratatui Ecosystem)
- **FocusBuilder**: Reconstructs focusable widget list each frame during render
- **HandleEvent trait**: Standardized event processing with `Outcome` return type
- **Automatic hit testing**: `focus_at(col, row)` finds component at position

### tui-realm
- **React/Elm inspired**: Components with props, state, and message passing
- **View abstraction**: Manages mounting/unmounting, focus, event forwarding
- **Subscription system**: Route events to components even when not focused

## Decision Matrix

| Approach | Complexity | Risk | Benefit |
|----------|------------|------|---------|
| Phase 1: Add menu_bar_row | Low | Low | Fixes immediate bug pattern |
| Phase 2: Unified HitArea | Medium | Medium | Eliminates coordinate bugs |
| Phase 3: Compositor | High | High | Scalable for complex UI |

## Recommendation

1. **Now**: Complete Phase 1 for consistency (the fix for #832 was a minimal version of this)
2. **Next few months**: Implement Phase 2 when adding new interactive UI elements
3. **Future**: Evaluate Phase 3 if/when the UI complexity warrants it (multiple nested dialogs, complex popups, etc.)

## References

- [Ratatui Event Handling Docs](https://ratatui.rs/concepts/event-handling/)
- [Ratatui Component Architecture](https://ratatui.rs/concepts/application-patterns/component-architecture/)
- [GitHub Issue #1050 - Mouse events on Rect](https://github.com/ratatui/ratatui/issues/1050)
- [Helix Architecture](https://github.com/helix-editor/helix/blob/master/docs/architecture.md)
- [rat-focus crate](https://docs.rs/rat-focus)
- [tui-realm](https://github.com/veeso/tui-realm)

## Appendix: Current Mouse Handling Locations

Key files involved in mouse event handling:

- `crates/fresh-editor/src/app/mouse_input.rs` - Main mouse event dispatch
- `crates/fresh-editor/src/app/render.rs` - Layout calculation
- `crates/fresh-editor/src/app/mod.rs` - `CachedLayout` struct definition
- `crates/fresh-editor/src/view/ui/split_rendering.rs` - Tab bar rendering and position tracking
