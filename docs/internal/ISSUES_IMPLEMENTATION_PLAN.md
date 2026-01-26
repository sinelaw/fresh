# Fresh Editor Issues Implementation Plan

This document provides a step-by-step implementation plan for 14 bug fixes and feature improvements, following the pattern: reproduce in tmux, add e2e test, implement fix.

## Overview

| # | Issue | Priority | Complexity | Key Files |
|---|-------|----------|------------|-----------|
| 1 | Buffer focus history (close returns to previous) | P0 | Medium | `split.rs`, `buffer_management.rs` |
| 2 | Search not finding `__` or `plugin_name<` | P0 | Low | `render.rs` |
| 3 | Multi-cursor cut not working | P0 | Medium | `clipboard.rs` |
| 4 | Rainbow bracket matching | P1 | High | New file, `theme/types.rs` |
| 5 | Async formatter (rustfmt freezing UI) | P1 | High | `on_save_actions.rs`, async handling |
| 6 | Indent selection cursor position | P1 | Medium | `input.rs` |
| 7 | Library files read-only (go-to-definition) | P1 | Low | `lsp_requests.rs`, `buffer_management.rs` |
| 8 | Tab bar scroll buttons | P1 | Medium | `tabs.rs`, `mouse_input.rs` |
| 9 | Explorer menu visibility | P2 | Low | `menu.rs`, `render.rs` |
| 10 | Shift+click selection | P1 | Low | `mouse_input.rs` |
| 11 | Whitespace cleanup on save | P2 | Medium | `on_save_actions.rs`, `config.rs` |
| 12 | LSP file filtering by project root | P2 | Medium | `lsp_requests.rs`, `manager.rs` |
| 13 | Virtual buffer cursor/status hiding | P2 | Low | `render.rs`, status bar |
| 14 | Movement keys disabled in virtual buffers | P2 | Low | `input.rs`, `buffer_mode.rs` |

---

## Issue 1: Buffer Focus History

### Problem
When closing a buffer, the editor switches to an adjacent tab instead of the previously focused buffer.

### Current State
- `SplitViewState` in `src/view/split.rs:107` has `previous_buffer: Option<BufferId>` - only tracks ONE previous
- `buffer_management.rs` uses index-based replacement in `close_buffer_internal`

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/view/split.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/buffer_management.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/mod.rs`

**Step 1.1:** Change `SplitViewState` to use a focus history stack
```rust
// In split.rs, replace:
pub previous_buffer: Option<BufferId>,
// With:
pub focus_history: Vec<BufferId>,  // Most recent at end
```

**Step 1.2:** Add helper methods to `SplitViewState`
```rust
impl SplitViewState {
    pub fn push_focus(&mut self, buffer_id: BufferId) {
        // Remove if already in history (LRU-style)
        self.focus_history.retain(|&id| id != buffer_id);
        self.focus_history.push(buffer_id);
        // Limit to 50 entries
        if self.focus_history.len() > 50 {
            self.focus_history.remove(0);
        }
    }

    pub fn pop_focus(&mut self) -> Option<BufferId> {
        self.focus_history.pop()
    }

    pub fn remove_from_history(&mut self, buffer_id: BufferId) {
        self.focus_history.retain(|&id| id != buffer_id);
    }
}
```

**Step 1.3:** Update `set_active_buffer` in `mod.rs` to push to history before switching

**Step 1.4:** Update `close_buffer_internal` to use `pop_focus()` instead of index-based selection

### E2E Test
Create `tests/e2e/buffer_focus_history.rs`:
```rust
#[test]
fn test_close_returns_to_previous_focused() {
    // Open A, B, C
    // Focus order: A -> B -> C -> A -> B
    // Close B -> should return to A (most recent before B)
}
```

### Verification
1. `cargo test -p fresh-editor buffer_focus`
2. Manual: Open 3+ files, switch between them, close and verify correct return

---

## Issue 2: Search Not Finding Special Characters

### Problem
Ctrl+F search doesn't find `__` or `plugin_name<` even though they exist in the file.

### Current State
- Search in `render.rs` uses `regex::escape()` when `use_regex` is false
- Need to verify escaping is applied correctly

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/render.rs`

**Step 2.1:** Debug and verify regex escaping in `perform_search()`
- Ensure `<` and `_` are properly escaped
- `_` should NOT need escaping in regex (it's literal)
- `<` should be literal in non-regex mode

**Step 2.2:** Add debug logging if needed to trace search pattern construction

### E2E Test
Add to `tests/e2e/search.rs`:
```rust
#[test]
fn test_search_double_underscore() {
    let mut harness = EditorTestHarness::create(80, 24, HarnessOptions::new());
    harness.type_text("def __init__(self): pass");
    harness.send_key(KeyCode::Char('f'), KeyModifiers::CONTROL);
    harness.type_text("__");
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE);
    // Verify match found
}

#[test]
fn test_search_angle_bracket() {
    let mut harness = EditorTestHarness::create(80, 24, HarnessOptions::new());
    harness.type_text("let x: Vec<String>");
    harness.send_key(KeyCode::Char('f'), KeyModifiers::CONTROL);
    harness.type_text("Vec<");
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE);
    // Verify match found
}
```

### Verification
1. `cargo test -p fresh-editor search`
2. Manual: Open Python file, search for `__init__`

---

## Issue 3: Multi-Cursor Cut Not Working

### Problem
Using cut with multiple cursors selecting text in multiple locations doesn't work correctly.

### Current State
- `cut_selection()` in `clipboard.rs` handles multi-cursor
- Processes cursors in reverse order to maintain offsets
- Issue may be with position adjustment during batch deletion

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/clipboard.rs`

**Step 3.1:** Investigate the cut flow
- Trace through `cut_selection()` with multiple cursors
- Verify all selections are captured before deletion
- Check if positions shift incorrectly

**Step 3.2:** Fix - likely need to:
- Copy all selections first (before any deletions)
- Then delete in reverse order with proper offset tracking
- Use `apply_events_as_bulk_edit()` for atomic operation

### E2E Test
Add to `tests/e2e/multicursor.rs`:
```rust
#[test]
fn test_multicursor_cut() {
    let mut harness = EditorTestHarness::create(80, 24, HarnessOptions::new());
    harness.type_text("hello world hello world");
    // Add cursor at both "world" positions
    // Select "world" at each cursor
    // Cut
    harness.assert_buffer_content("hello  hello ");
    // Paste should give "world\nworld"
}
```

### Verification
1. `cargo test -p fresh-editor multicursor`
2. Manual: Select same text at multiple locations, cut, verify all removed

---

## Issue 4: Rainbow Bracket Matching

### Problem
No highlighting for matching parentheses, and no rainbow colors for nested brackets.

### Current State
- `goto_matching_bracket()` exists in `render.rs` but doesn't highlight
- Reference highlighter in `reference_highlight_overlay.rs` shows overlay pattern

### Implementation

**Files to create/modify:**
- Create: `/home/noam/repos/fresh/crates/fresh-editor/src/view/bracket_highlight_overlay.rs`
- Modify: `/home/noam/repos/fresh/crates/fresh-editor/src/view/theme/types.rs`
- Modify: `/home/noam/repos/fresh/crates/fresh-editor/src/config.rs`
- Modify: `/home/noam/repos/fresh/crates/fresh-editor/src/app/render.rs`

**Step 4.1:** Add rainbow colors to theme
```rust
// In types.rs
pub bracket_match_colors: Vec<Color>,  // Cycle through for nested brackets
```

**Step 4.2:** Create bracket highlight overlay module
- On cursor movement, find bracket at cursor
- Find matching bracket using existing logic
- Calculate nesting depth
- Apply overlay with depth-based color

**Step 4.3:** Add config options
```rust
pub highlight_matching_brackets: bool,
pub rainbow_brackets: bool,
```

### E2E Test
```rust
#[test]
fn test_bracket_highlight_on_cursor() {
    // Create buffer with nested brackets
    // Move cursor to opening bracket
    // Verify overlay on matching closing bracket
}
```

### Verification
1. `cargo test -p fresh-editor bracket`
2. Manual: Open Rust file, move cursor to brackets, verify highlighting

---

## Issue 5: Async Formatter Execution

### Problem
rustfmt timing out freezes the entire UI because formatter runs synchronously.

### Current State
- `on_save_actions.rs` has a blocking polling loop (10ms sleep in try_wait loop)
- This blocks the main thread

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/on_save_actions.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/async_messages.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/mod.rs`

**Step 5.1:** Create async formatter task type
```rust
pub struct FormatterTask {
    pub buffer_id: BufferId,
    pub receiver: oneshot::Receiver<FormatterResult>,
}
```

**Step 5.2:** Move formatter execution to spawned thread
- Spawn thread for formatter process
- Send result via channel when complete

**Step 5.3:** Add formatter state tracking
```rust
// In Editor
pub formatting_in_progress: Option<FormatterTask>,
```

**Step 5.4:** Handle completion in event loop
- Check for formatter result in main loop
- Apply changes to buffer when complete
- Show "Formatting..." in status bar during operation

### E2E Test
```rust
#[test]
fn test_formatter_nonblocking() {
    // Start format on buffer
    // Immediately send input events
    // Verify input events processed (UI not blocked)
}
```

### Verification
1. `cargo test -p fresh-editor format`
2. Manual: Format large file, verify UI responds during formatting

---

## Issue 6: Indent Selection Cursor Position

### Problem
After pressing Tab with text selected, selection moves or changes unexpectedly.

### Current State
- `InsertTab` action in `input.rs` indents selected lines
- Cursor position may not be adjusted correctly after indentation

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/input.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/input/buffer_mode.rs`

**Step 6.1:** Investigate current indent behavior
- Trace through Tab with selection active
- Identify where cursor position goes wrong

**Step 6.2:** Fix cursor adjustment
- Store cursor column before indent
- After indent, adjust cursor by indent amount
- Maintain selection spanning same logical range

### E2E Test
Add to `tests/e2e/tab_indent_selection.rs`:
```rust
#[test]
fn test_indent_preserves_relative_cursor() {
    // Type multiline text
    // Select multiple lines
    // Record cursor position relative to line
    // Indent
    // Verify cursor at original_column + indent_size
}
```

### Verification
1. `cargo test -p fresh-editor tab_indent`
2. Manual: Select lines, indent, verify cursor position

---

## Issue 7: Library Files Read-Only

### Problem
When using go-to-definition and it opens a library source file (outside project), it should be read-only.

### Current State
- `BufferMetadata` has `read_only: bool` field
- LSP go-to-definition opens files via `open_file()` without checking location

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/lsp_requests.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/buffer_management.rs`

**Step 7.1:** Add library file detection
```rust
fn is_library_file(path: &Path, project_root: &Path) -> bool {
    // Check if outside project root
    if !path.starts_with(project_root) {
        return true;
    }
    // Check common library paths
    let path_str = path.to_string_lossy();
    path_str.contains(".cargo") ||
    path_str.contains("node_modules") ||
    path_str.contains("site-packages")
}
```

**Step 7.2:** Update `handle_goto_definition_response` to set read-only
```rust
// After opening file, if it's a library file:
if is_library_file(&path, &self.project_root) {
    self.buffer_metadata_mut(buffer_id).read_only = true;
}
```

### E2E Test
```rust
#[test]
fn test_goto_definition_library_readonly() {
    // Mock LSP response pointing outside project
    // Execute goto definition
    // Verify buffer has read_only = true
}
```

### Verification
1. `cargo test -p fresh-editor lsp`
2. Manual: Go to std library definition, verify read-only indicator

---

## Issue 8: Tab Bar Scroll Buttons

### Problem
Left/right buttons on tabs bar don't work - left button overlaps separator, clicking either does nothing.

### Current State
- `tabs.rs` renders `<` and `>` scroll indicators
- No click handlers for these buttons in mouse handling

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/view/ui/tabs.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/mouse_input.rs`

**Step 8.1:** Add scroll button hit areas to TabLayout
```rust
pub struct TabLayout {
    // existing fields...
    pub left_scroll_area: Option<Rect>,
    pub right_scroll_area: Option<Rect>,
}
```

**Step 8.2:** Extend TabHit enum
```rust
pub enum TabHit {
    // existing variants...
    ScrollLeft,
    ScrollRight,
}
```

**Step 8.3:** Handle clicks in mouse_input.rs
- On ScrollLeft: decrease `tab_scroll_offset` by scroll amount
- On ScrollRight: increase `tab_scroll_offset` by scroll amount
- Clamp to valid range

**Step 8.4:** Fix overlap with file explorer separator
- Ensure scroll button positioning accounts for explorer width

### E2E Test
```rust
#[test]
fn test_tab_scroll_buttons() {
    // Open 10+ files to overflow tab bar
    // Simulate click on right scroll button
    // Verify tab_scroll_offset increased
    // Verify different tabs now visible
}
```

### Verification
1. `cargo test -p fresh-editor tabs`
2. Manual: Open many files, click scroll buttons, verify scrolling

---

## Issue 9: Explorer Menu Visibility

### Problem
"Explorer" menu should only be visible when File Explorer is in focus.

### Current State
- Menu visibility uses MenuContext with `when` conditions
- Need to add file_explorer_focused context

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/view/ui/menu.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/render.rs`

**Step 9.1:** Add "file_explorer_focused" to MenuContext
**Step 9.2:** Update Explorer menu items with `when: "file_explorer_focused"`
**Step 9.3:** Set context based on current focus in render

### E2E Test
```rust
#[test]
fn test_explorer_menu_visibility() {
    // Focus editor area
    // Verify Explorer menu not visible
    // Focus file explorer
    // Verify Explorer menu visible
}
```

### Verification
1. `cargo test -p fresh-editor menu`
2. Manual: Toggle file explorer focus, verify menu visibility

---

## Issue 10: Shift+Click Selection Extension

### Problem
Shift+click should extend selection to clicked point like other apps.

### Current State
- `mouse_input.rs` handles clicks but doesn't check Shift modifier for selection

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/mouse_input.rs`

**Step 10.1:** In `handle_mouse_click`, check for Shift modifier
```rust
if modifiers.contains(KeyModifiers::SHIFT) {
    // Use current cursor position as anchor
    // Calculate clicked position
    // Extend selection from anchor to clicked position
    state.cursors.primary_mut().move_to(clicked_pos, true); // true = extend
    return;
}
```

### E2E Test
Add to `tests/e2e/selection.rs`:
```rust
#[test]
fn test_shift_click_extends_selection() {
    let mut harness = EditorTestHarness::create(80, 24, HarnessOptions::new());
    harness.type_text("hello world test");
    // Click at position 0
    harness.send_mouse_event(0, 2, MouseButton::Left, MouseEventKind::Down);
    // Shift+click at position 10
    harness.send_mouse_event_with_modifiers(10, 2, MouseButton::Left,
        MouseEventKind::Down, KeyModifiers::SHIFT);
    // Verify selection from 0 to 10
}
```

### Verification
1. `cargo test -p fresh-editor selection`
2. Manual: Click, then shift+click elsewhere, verify selection

---

## Issue 11: Whitespace Cleanup

### Problem
Need command and on-save option to clean up trailing whitespace.

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/on_save_actions.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/config.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/input/commands.rs`

**Step 11.1:** Add config options
```rust
// In EditorConfig
pub trim_trailing_whitespace_on_save: bool,
pub ensure_final_newline: bool,
```

**Step 11.2:** Implement cleanup function
```rust
fn trim_trailing_whitespace(&mut self) -> Vec<Event> {
    // Iterate all lines
    // Trim trailing whitespace from each
    // Return edit events
}
```

**Step 11.3:** Add as action
```rust
Action::TrimTrailingWhitespace
```

**Step 11.4:** Call in `run_on_save_actions` if enabled

### E2E Test
```rust
#[test]
fn test_trim_whitespace_on_save() {
    // Create buffer with trailing spaces
    // Enable config
    // Save
    // Verify spaces removed
}
```

### Verification
1. `cargo test -p fresh-editor whitespace`
2. Manual: Create file with trailing spaces, save, verify cleaned

---

## Issue 12: LSP File Filtering by Project Root

### Problem
LSP sends irrelevant files (outside project root, wrong language).

### Current State
- `lsp_requests.rs` has filters but may not check project root

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/lsp_requests.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/types.rs`

**Step 12.1:** Add project root check in `with_lsp_for_buffer`
```rust
// Check if file is within project root
let file_path = metadata.file_path()?;
if !file_path.starts_with(&self.project_root) {
    metadata.disable_lsp("File outside project root");
    return None;
}
```

### E2E Test
```rust
#[test]
fn test_lsp_disabled_outside_project() {
    // Open file outside project
    // Verify LSP disabled for that buffer
}
```

### Verification
1. `cargo test -p fresh-editor lsp`
2. Manual: Open external file, verify no LSP features

---

## Issue 13: Virtual Buffer Line/Col Status Hidden

### Problem
In virtual buffers, line/column status should be hidden.

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/render.rs`

**Step 13.1:** In status bar rendering, check if buffer is virtual
```rust
if !metadata.is_virtual() {
    // Render line:column
} else {
    // Show buffer mode instead (e.g., "Diagnostics")
}
```

### E2E Test
```rust
#[test]
fn test_virtual_buffer_hides_line_col() {
    // Open diagnostics panel (virtual buffer)
    // Verify status bar doesn't show line:col
}
```

---

## Issue 14: Virtual Buffer Movement Keys Disabled

### Problem
In virtual buffers where cursor is hidden, movement keys should have no effect.

### Implementation

**Files to modify:**
- `/home/noam/repos/fresh/crates/fresh-editor/src/app/input.rs`
- `/home/noam/repos/fresh/crates/fresh-editor/src/input/buffer_mode.rs`

**Step 14.1:** Check `show_cursors` before handling movement
```rust
fn handle_movement_action(&mut self, action: Action) {
    if !self.active_state().show_cursors {
        return; // Ignore movement in virtual buffers
    }
    // Normal movement handling
}
```

### E2E Test
```rust
#[test]
fn test_virtual_buffer_ignores_movement() {
    // Create virtual buffer with hidden cursor
    // Send movement keys
    // Verify cursor position unchanged
}
```

---

## Verification Plan

### Unit Tests
```bash
cargo test -p fresh-editor
```

### E2E Tests
```bash
cargo test -p fresh-editor --test '*'
```

### Manual Testing Checklist
1. [ ] Open 3+ files, switch between them, close tabs - verify correct focus return
2. [ ] Search for `__` and `<` characters - verify found
3. [ ] Multi-cursor cut operation - verify all selections cut
4. [ ] Bracket highlighting with rainbow colors
5. [ ] Format large Rust file - verify UI responsive
6. [ ] Indent selection - verify cursor position correct
7. [ ] Go to std library definition - verify read-only
8. [ ] Tab bar scroll buttons - verify clicking works
9. [ ] Explorer menu visibility based on focus
10. [ ] Shift+click selection extension
11. [ ] Trailing whitespace cleanup on save
12. [ ] LSP not active for external files
13. [ ] Virtual buffer status bar (no line:col)
14. [ ] Virtual buffer ignores movement keys

---

## Implementation Order

1. **Phase 1 - High Impact Bug Fixes:**
   - Issue 2 (Search special chars)
   - Issue 3 (Multi-cursor cut)
   - Issue 10 (Shift+click)

2. **Phase 2 - UX Improvements:**
   - Issue 1 (Focus history)
   - Issue 6 (Indent cursor)
   - Issue 8 (Tab scroll buttons)

3. **Phase 3 - Features:**
   - Issue 5 (Async formatter)
   - Issue 4 (Rainbow brackets)
   - Issue 11 (Whitespace cleanup)

4. **Phase 4 - Polish:**
   - Issue 7 (Library read-only)
   - Issue 9 (Explorer menu)
   - Issues 12-14 (LSP/Virtual buffers)
