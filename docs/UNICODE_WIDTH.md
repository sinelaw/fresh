# Unicode Width and Multi-byte Character Support

This document lists all the places in the codebase where visual display width must be accounted for, particularly for:
- CJK (Chinese, Japanese, Korean) characters that occupy 2 terminal columns
- Emoji that occupy 2 terminal columns
- Zero-width characters (combining marks, zero-width space)
- Multi-byte UTF-8 characters (which may not correspond to visual width)

## Core Utility Module

### `src/primitives/display_width.rs`

The central utility module for all visual width calculations. Uses the `unicode-width` crate.

**Key functions:**
- `char_width(c: char) -> usize` - Returns visual width of a character (0, 1, or 2)
- `str_width(s: &str) -> usize` - Returns total visual width of a string
- `DisplayWidth` trait - Extension trait for convenient `.display_width()` method

**Usage:** Import from `crate::primitives::display_width::{char_width, str_width}`

## Critical Rendering Code

### `src/primitives/line_wrapping.rs`

Line wrapping must wrap based on visual width, not character count.

**Key locations:**
- `wrap_line()` function (lines 111-127) - Tracks `segment_visual_width` using `char_width(c)`
- Uses visual width to determine when to break lines
- Property tests verify visual width correctness

### `src/view/ui/split_rendering.rs`

The main buffer rendering code that positions characters and cursors on screen.

**Key locations:**
- Line ~2150: `visible_char_count += char_width(ch)` - Tracks visual column during character iteration
- Line ~2927: `str_width(&span.content)` - Counts span width for cursor position detection (in test code)

**Important:** The `visible_char_count` variable tracks visual columns, not character indices.

### `src/view/viewport.rs`

Viewport scrolling calculations for horizontal scroll.

**Key locations:**
- Lines ~381-394: Calculates cursor visual column by walking through characters and summing widths
- Line ~394: `str_width(line_text)` for line visual width
- `ensure_column_visible_simple()` uses visual widths for scroll calculations

### `src/primitives/ansi.rs`

ANSI escape code handling must exclude invisible escape sequences from width.

**Key locations:**
- `visible_char_count()` function (lines ~302-319) - Returns visual width excluding ANSI codes
- Uses `str_width()` for fast path (no ANSI) and `char_width()` for character-by-character parsing

## UI Components

### `src/view/ui/status_bar.rs`

Status bar text truncation must account for visual width.

**Key locations:**
- Lines ~504-529: Truncates left status by visual width using `str_width()` and `char_width()`
- Lines ~538: Calculates padding using `str_width(&displayed_left)`
- Lines ~602-639: Similar truncation logic for narrow terminal case

**Truncation pattern:**
```rust
let visual_width = str_width(&text);
if visual_width > max_width {
    let mut width = 0;
    let truncated: String = text
        .chars()
        .take_while(|ch| {
            let w = char_width(*ch);
            if width + w <= truncate_at {
                width += w;
                true
            } else {
                false
            }
        })
        .collect();
    format!("{}...", truncated)
}
```

### `src/view/ui/tabs.rs`

Tab bar width calculations.

**Key locations:**
- Lines ~200-205: `str_width(&tab_name_text)` and `str_width(close_text)` for tab sizing

### `src/view/ui/file_explorer.rs`

File explorer tree view layout.

**Key locations:**
- Line ~176: `str_width(&node.entry.name)` for name column width
- Line ~249: `str_width(&size_text)` for file size alignment

### `src/view/ui/file_browser.rs`

File browser popup for Open File dialog.

**Key locations:**
- Line ~567: `str_width(label) + 2` for shortcut label width in navigation bar

### `src/view/ui/suggestions.rs`

Autocomplete/command palette suggestion rendering.

**Key locations (all use visual-width-aware truncation):**
- Lines ~139-162: Command name column
- Lines ~188-213: Keybinding column
- Lines ~235-271: Description column
- Lines ~310-331: Source column

Each column uses the truncation pattern shown above.

## Locations That DON'T Need Visual Width

Some `.chars().count()` usages are intentionally character-based:

### Character Indexing into Data Structures

- `char_mappings` vectors in `split_rendering.rs` (lines ~1003, ~2162, ~2271)
  - These index into character-indexed data structures, not visual positions
  - The vectors have one element per character, not per visual column

### Test Assertions

- Test code that verifies character count vs byte count (e.g., `fancy_quote.chars().count() == 1`)

## Common Pitfalls

1. **Byte length vs character count vs visual width:**
   - `s.len()` - byte count (wrong for multi-byte UTF-8)
   - `s.chars().count()` - character count (wrong for double-width)
   - `str_width(s)` - visual width (correct for display)

2. **Truncation must use visual width:**
   ```rust
   // WRONG: truncates by character count
   text.chars().take(max_width).collect()

   // RIGHT: truncates by visual width
   let mut width = 0;
   text.chars().take_while(|ch| {
       let w = char_width(*ch);
       if width + w <= max_width { width += w; true } else { false }
   }).collect()
   ```

3. **Cursor positioning:**
   - Screen X coordinate = sum of visual widths of characters before cursor
   - NOT the character index

4. **Line wrapping:**
   - Wrap when visual width exceeds terminal width
   - NOT when character count exceeds

## Testing

### Unit Tests

- `src/primitives/display_width.rs` - Tests for width functions
- `src/primitives/line_wrapping.rs` - Tests including:
  - `test_visual_width_calculation`
  - `test_wrap_line_double_width_characters`
  - `test_wrap_line_emoji_visual_width`
  - `test_wrap_line_mixed_ascii_and_cjk`
  - `test_chars_count_vs_visual_width_bug` (regression test)

### Property Tests

- `src/primitives/line_wrapping.rs::proptests` module
- Tests wrap_line with various Unicode character combinations

### E2E Tests

- `tests/e2e/multibyte_characters.rs` - End-to-end tests for multi-byte character handling

## Dependencies

- `unicode-width = "0.2"` in `Cargo.toml`
- Uses UAX#11 (Unicode Standard Annex #11) East Asian Width property
