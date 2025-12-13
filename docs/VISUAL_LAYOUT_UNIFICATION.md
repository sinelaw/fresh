# Visual Layout Unification Plan

## Problem Statement

The codebase has inconsistent handling of visual column calculations across different flows:

1. **Rendering/cursor detection** uses `ViewLine.char_mappings` indexed by visual column
   - Zero-width chars (ANSI escapes) have no entries, breaking cursor detection

2. **Mouse clicks** use `char_mappings[visual_col]` for O(1) lookup
   - Works for visible chars, but same zero-width issue

3. **MoveUp/MoveDown** use `str_width()` and `byte_offset_at_visual_column()` on raw buffer
   - Doesn't understand ANSI escape sequences (counts `[`, `2`, `m` as width 1 each!)
   - Doesn't handle tab expansion consistently with rendering

## Solution: Unified Visual Layout Module

### Core Data Model

```rust
// Per-line mappings that support all operations
pub struct LineMappings {
    // Per-CHARACTER (indexed by char position in text)
    // Length == text.chars().count()
    pub char_source_bytes: Vec<Option<usize>>,

    // Per-VISUAL-COLUMN (indexed by visual column)
    // Length == visual width of line
    pub visual_to_char: Vec<usize>,
}
```

### Shared Utilities

```rust
// primitives/visual_layout.rs

/// Calculate visual width handling ANSI escapes, tabs, zero-width chars
pub fn visual_width(s: &str, start_col: usize) -> usize;

/// Convert byte offset to visual column (ANSI-aware, tab-aware)
pub fn byte_to_visual_col(s: &str, byte_offset: usize) -> usize;

/// Convert visual column to byte offset (ANSI-aware, tab-aware)
pub fn visual_col_to_byte(s: &str, visual_col: usize) -> usize;

/// Build complete per-char and per-visual-col mappings
pub fn build_line_mappings(text: &str, source_bytes: &[Option<usize>]) -> LineMappings;
```

### O(1) Operations

| Operation | How | Complexity |
|-----------|-----|------------|
| Mouse click at visual col V | `char_idx = visual_to_char[V]`<br>`byte = char_source_bytes[char_idx]` | O(1) |
| Cursor render at char I | `byte = char_source_bytes[I]` | O(1) |
| MoveUp/Down | Use shared `byte_to_visual_col()` and `visual_col_to_byte()` | O(n) per line |

Note: MoveUp/Down is O(n) but only processes one line at a time, and navigation is infrequent compared to rendering.

## Implementation Steps

### 1. Create `primitives/visual_layout.rs`
- Move/refactor ANSI-aware width calculation from display_width.rs
- Add `build_line_mappings()` function
- Add `byte_to_visual_col()` and `visual_col_to_byte()` with ANSI/tab support

### 2. Update `ViewLine` in `view_pipeline.rs`
- Replace `char_mappings: Vec<Option<usize>>` (per visual col)
- With `char_source_bytes: Vec<Option<usize>>` (per char)
- Add `visual_to_char: Vec<usize>` (per visual col)
- Keep `char_styles` but make it per-char instead of per-visual-col

### 3. Update `view_pipeline.rs` iterator
- Build new mappings structure during iteration
- One entry in char_source_bytes per character (including zero-width)
- One entry in visual_to_char per visual column

### 4. Update `split_rendering.rs`
- Cursor detection: use `char_source_bytes[char_index]` instead of `char_mappings[col_offset]`
- Track char_index separately from col_offset during iteration
- `push_span_with_map`: update to build per-char mappings

### 5. Update `input.rs` mouse click handling
- `screen_to_buffer_position`: use `visual_to_char[col]` then `char_source_bytes[char_idx]`
- Update `ViewLineMapping` struct to use new format

### 6. Update `actions.rs` MoveUp/MoveDown
- Replace `str_width()` with ANSI-aware `byte_to_visual_col()`
- Replace `byte_offset_at_visual_column()` with ANSI-aware `visual_col_to_byte()`

## Migration Notes

- The existing `display_width.rs` functions can remain for simple cases
- New visual_layout.rs handles the complex ANSI/tab cases
- Tests will need updating to reflect new data structures
