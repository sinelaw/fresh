# Unified Hit-Test with Theme Key Provenance

## Context

The theme inspector popup (Ctrl+Right-Click) needs to know which theme keys
styled each screen cell. Currently this is solved by a per-cell theme key map
(`cell_theme_map`) on `CachedLayout`, populated during rendering. This works
but has drawbacks:

1. **Redundant storage**: The cell map duplicates region info that `CachedLayout`
   already tracks via `split_areas`, `status_bar_area`, `tab_layouts`, etc.
2. **Per-frame cost**: The map is allocated and filled every frame (~240KB for a
   200x50 terminal) even though it's only queried on the rare Ctrl+Right-Click.
3. **Scattered hit testing**: Mouse clicks, hover detection, and theme inspection
   each re-implement region detection logic in different files (`mouse_input.rs`,
   `theme_inspect.rs`, `render.rs`).

## Proposal

Introduce a unified `CachedLayout::region_at(col, row) -> Option<RegionInfo>`
method that replaces all per-component hit testing with a single lookup. The
`RegionInfo` struct carries both interaction semantics (for mouse handling) and
theme key provenance (for the inspector), eliminating the separate `cell_theme_map`.

## Design

### RegionInfo struct

```rust
/// Describes the UI region at a screen position.
/// Used by mouse input, theme inspector, and accessibility.
pub struct RegionInfo {
    /// What kind of region this is
    pub kind: RegionKind,
    /// Theme key for the foreground color
    pub fg_theme_key: Option<&'static str>,
    /// Theme key for the background color
    pub bg_theme_key: Option<&'static str>,
    /// Syntax highlight category (editor content only)
    pub syntax_category: Option<&'static str>,
}

pub enum RegionKind {
    /// Editor content area (text)
    EditorContent {
        split_id: LeafId,
        buffer_id: BufferId,
        byte_pos: Option<usize>,
    },
    /// Line number gutter
    Gutter {
        split_id: LeafId,
        buffer_id: BufferId,
    },
    /// Tab bar
    Tab {
        split_id: LeafId,
        buffer_id: BufferId,
        is_active: bool,
        is_close_button: bool,
    },
    /// Status bar
    StatusBar {
        sub_region: StatusBarSubRegion,
    },
    /// Menu bar
    MenuBar {
        menu_index: Option<usize>,
    },
    /// File explorer
    FileExplorer,
    /// Scrollbar
    Scrollbar {
        is_thumb: bool,
    },
    /// Split separator
    SplitSeparator,
    /// Popup / overlay UI
    Popup {
        popup_index: usize,
    },
    /// Empty / unoccupied
    Empty,
}
```

### Where region_at lives

`CachedLayout::region_at(col, row)` checks regions in priority order (popups
first, then menu, status bar, tabs, split content, file explorer). This is the
same priority order used by mouse input today, but consolidated into one place.

For **editor content cells**, the per-cell theme key info still needs to come
from rendering (because overlays, syntax highlights, selection, and cursor
state determine the final keys). The key insight is:

- **Non-editor regions**: Theme keys are deterministic from the region type.
  `region_at` returns them directly from the `RegionKind`.
- **Editor content cells**: Theme keys depend on the specific byte position's
  overlays and syntax. These still need to be recorded during rendering.

### Hybrid approach for editor content

Keep the per-cell recording only for editor content areas (gutter + text),
not for the entire terminal. This reduces the map from `width * height` to
just the editor content rectangles.

```rust
/// Per-split editor cell theme info, indexed by (row_offset, col_offset)
/// within the split's content rectangle.
pub struct SplitCellThemeMap {
    /// Theme info per cell, indexed as row * width + col
    /// (relative to the split's content_rect origin)
    cells: Vec<CellThemeInfo>,
    width: u16,
}
```

Store as `HashMap<LeafId, SplitCellThemeMap>` on `CachedLayout`. The rendering
pipeline already has the split_id, so recording is straightforward.

### Lookup flow

```
region_at(col, row) ->
  1. Check popups (highest z-order)
  2. Check menu bar
  3. Check status bar
  4. Check tab bars (per split)
  5. Check split separators
  6. Check scrollbars (per split)
  7. Check split content areas:
     a. Determine split_id from split_areas
     b. If in gutter: return Gutter region with theme keys
     c. If in content: look up SplitCellThemeMap[split_id]
        -> returns CellThemeInfo with exact fg/bg theme keys
  8. Check file explorer
  9. Return Empty
```

### Consumers

#### Mouse input (`mouse_input.rs`)
Replace scattered `if point_in_rect(...)` / `if row == bar_row` checks with:
```rust
match self.cached_layout.region_at(col, row) {
    Some(RegionInfo { kind: RegionKind::Tab { buffer_id, .. }, .. }) => { /* switch tab */ }
    Some(RegionInfo { kind: RegionKind::StatusBar { sub_region }, .. }) => { /* status click */ }
    Some(RegionInfo { kind: RegionKind::EditorContent { split_id, byte_pos, .. }, .. }) => { /* editor click */ }
    ...
}
```

#### Theme inspector (`theme_inspect.rs`)
Replace `resolve_theme_key_at` with:
```rust
fn resolve_theme_key_at(&self, col: u16, row: u16) -> Option<ThemeKeyInfo> {
    let region = self.cached_layout.region_at(col, row)?;
    let fg_color = region.fg_theme_key.and_then(|k| self.theme.resolve_theme_key(k));
    let bg_color = region.bg_theme_key.and_then(|k| self.theme.resolve_theme_key(k));
    Some(ThemeKeyInfo { fg_key: region.fg_theme_key.map(Into::into), ... })
}
```

#### Hover detection
Cursor style changes and tooltip display can also use `region_at` to determine
what's under the mouse.

## Implementation plan

### Phase 1: Extract region_at for non-editor regions

1. Add `RegionInfo`, `RegionKind` structs to `types.rs`
2. Implement `CachedLayout::region_at()` for non-editor regions (status bar,
   menu, tabs, scrollbar, file explorer, separators) using existing cached
   layout fields
3. Replace `record_non_editor_theme_regions()` with `region_at` — the theme
   inspector calls `region_at` instead of reading the cell map for these regions
4. Remove the full-terminal `cell_theme_map` allocation; keep per-split maps
   only for editor content

### Phase 2: Migrate mouse input

5. Replace region checks in `mouse_input.rs` with `region_at` calls
6. Remove duplicated `point_in_rect` checks scattered across mouse handlers
7. Consolidate hover detection to use `region_at`

### Phase 3: Per-split editor theme maps

8. Replace the flat `Vec<CellThemeInfo>` with `HashMap<LeafId, SplitCellThemeMap>`
9. Only allocate/fill for visible splits' content areas
10. Thread the per-split map through `render_view_lines` (already done, just
    change the indexing to be split-relative instead of screen-absolute)

### Phase 4: Unify with UNIFIED_UI_FRAMEWORK_PLAN

11. The `RegionKind` enum aligns with the `HitTest` trait proposed in
    `UNIFIED_UI_FRAMEWORK_PLAN.md`. Each component's `*Layout` struct already
    returns typed hit results; `region_at` is the top-level dispatcher that
    delegates to them.
12. Plugin-exposed hit testing can use the same `region_at` infrastructure.

## Relationship to existing plans

- **UNIFIED_UI_FRAMEWORK_PLAN.md**: Proposes a `HitTest` trait per component.
  This plan adds the top-level dispatcher (`region_at`) and the theme key
  provenance that the framework plan doesn't address.
- **event-dispatch-architecture.md**: Documents the scattered hit-testing
  problem. This plan is the concrete solution.

## Tradeoffs

**Pros:**
- Single source of truth for "what's at this screen position"
- Eliminates per-frame full-terminal allocation when inspector isn't active
- Mouse input, theme inspector, hover, and accessibility all share one code path
- Editor content cells still get exact theme keys from rendering (no heuristics)

**Cons:**
- `region_at` introduces a priority-ordered cascade that must stay in sync with
  rendering z-order (popups > menu > status bar > content)
- Per-split maps add a `HashMap` lookup vs the current flat array index
- Migration touches mouse_input.rs extensively (high-risk file)

**Why not do it now:**
- The current `cell_theme_map` approach works correctly and has no heuristics
- The main cost (per-frame allocation) is negligible for typical terminal sizes
- mouse_input.rs is a large, complex file — refactoring it needs careful testing
- This plan can be executed incrementally, phase by phase
