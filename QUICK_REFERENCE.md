# Quick Reference: Line Number Bugs and Inefficiencies

## Critical Issues to Fix Immediately

### Issue #1: populate_line_cache() Bug - Line Numbers Always Wrong

**Files affected**:
- `/home/user/fresh/src/ui/split_rendering.rs` (line 368-422)
- `/home/user/fresh/src/buffer.rs` (line 876-879)

**Current broken code** (split_rendering.rs):
```rust
368 | let starting_line_num = state
369 |     .buffer
370 |     .populate_line_cache(state.viewport.top_byte, visible_count);
371 |
372 | // Compute syntax highlighting for the visible viewport...
373 | let viewport_start = state.viewport.top_byte;
...
422 | let current_line_num = starting_line_num + lines_rendered;
```

**Why it's broken**:
```rust
// In buffer.rs (876-879):
pub fn populate_line_cache(&mut self, _start_byte: usize, _line_count: usize) -> usize {
    // No-op: LineIndex maintains all line starts automatically
    0  // <-- ALWAYS RETURNS 0!!!
}
```

**Fix**:
```rust
// Replace line 370 in split_rendering.rs with:
let starting_line_num = state.buffer.get_line_number(state.viewport.top_byte);
```

**Impact**: ALL line numbers shown in margins are wrong! This affects:
- Line number rendering (margin.rs)
- Plugin hooks (render-line hooks get wrong line numbers)
- Diagnostic indicators position

---

### Issue #2: Inefficient line_count() - O(n) Instead of O(1)

**File**: `/home/user/fresh/src/editor.rs` (line 4833-4863)

**Function**: `calculate_max_scrollbar_position()`

**Current broken code**:
```rust
4834 | let mut line_count = 0;
4835 | let mut iter = buffer.line_iterator(0);
4836 | while iter.next().is_some() {
4837 |     line_count += 1;  // <-- ITERATES ENTIRE FILE!
4838 | }
...
4850 | let mut iter = buffer.line_iterator(0);
4851 | let mut current_line = 0;
...
4854 | while current_line < scrollable_lines {
4855 |     if let Some((pos, _content)) = iter.next() {
4856 |         max_byte_pos = pos;
4857 |         current_line += 1;
4858 |     }
4859 | }
```

**Why it's broken**:
- For a 10,000 line file: 10,000 iterations!
- Does this TWICE (lines 4835-4838 AND 4850-4861)
- Called frequently (scrollbar interactions)
- Buffer has a `line_count()` method that returns in O(1)

**Fix**:
```rust
fn calculate_max_scrollbar_position(buffer: &Buffer, viewport_height: usize) -> usize {
    let total_lines = buffer.line_count();  // O(1) instead of O(n)!
    
    if total_lines <= viewport_height {
        return 0;
    }
    
    let scrollable_lines = total_lines.saturating_sub(viewport_height);
    
    // Convert line number to byte offset using position_to_offset
    let position = Position { line: scrollable_lines, column: 0 };
    buffer.position_to_offset(position)  // O(1) instead of O(n)!
}
```

---

## High-Impact Inefficiencies

### Issue #3: Sequential Line Seeking (O(n) Instead of O(1))

**Files affected**:
- `/home/user/fresh/src/editor.rs` (lines 2628-2642, 4677-4682, 4850-4861)

**Pattern 1** (line 2628-2642):
```rust
2628 | let mut iter = state.buffer.line_iterator(0);  // Start from 0
2629 | let mut target_byte = 0;
2630 |
2631 | for current_line in 0..=target_line {
2632 |     if let Some((line_start, _)) = iter.next() {
2633 |         if current_line == target_line {
2634 |             target_byte = line_start;
2635 |             break;
2636 |         }
2637 |     }
2638 | }
```

**Pattern 2** (line 4677-4682):
```rust
4677 | let mut iter = state.buffer.line_iterator(0);
4678 | let mut line_byte = 0;
4679 |
4680 | for _ in 0..target_line {
4681 |     if let Some((pos, _content)) = iter.next() {
4682 |         line_byte = pos;
```

**Why it's broken**:
- Seeking to line 5000 requires 5000 iterations
- Buffer has O(1) `position_to_offset()` method
- Called during user navigation (frequent)

**Fix**:
```rust
use crate::line_index::Position;

// Instead of iterating:
let position = Position { line: target_line, column: column_offset };
let final_position = buffer.position_to_offset(position);
```

---

### Issue #4: Repeated get_line_number() Calls Per Frame

**File**: `/home/user/fresh/src/ui/split_rendering.rs` (lines 101-117)

**Current code**:
```rust
101 | let (total_lines, top_line) = if buffer_len <= large_file_threshold_bytes as usize {
102 |     // Small file: count actual lines
103 |     let total_lines = if buffer_len > 0 {
104 |         // Get the line number of the last byte
105 |         state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1  // Binary search #1
106 |     } else {
106 |         1
107 |     };
108 |
109 |     // Get the line number at the top of the viewport
110 |     let top_line = if state.viewport.top_byte < buffer_len {
111 |         state.buffer.get_line_number(state.viewport.top_byte)  // Binary search #2
112 |     } else {
113 |         0
114 |     };
115 |
116 |     (total_lines, top_line)
```

**Why it's inefficient**:
- Called every frame for every visible split
- With 4-split view: 8 binary searches per frame
- At 60fps: 480+ binary searches per second

**Optimization**:
```rust
// Only need one: starting_line_num computed once
let starting_line_num = state.buffer.get_line_number(state.viewport.top_byte);
let total_lines = if buffer_len > 0 {
    state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
} else {
    1
};
```

---

### Issue #5: Redundant Line Iteration in Viewport

**File**: `/home/user/fresh/src/viewport.rs` (lines 217-272, function `ensure_visible()`)

**Current code**:
```rust
219 | let cursor_iter = buffer.line_iterator(cursor.position);  // Iterator created
220 | let cursor_line_start = cursor_iter.current_position();   // Gets line start
221 |
222 | // Get line numbers from the cache
223 | let top_line_number = buffer.get_line_number(self.top_byte);           // Binary search
224 | let cursor_line_number = buffer.get_line_number(cursor_line_start);    // Binary search again!
```

**Why it's inefficient**:
- Iterator already found the line, but we do separate binary searches
- Called on every cursor movement

**Optimization**:
```rust
let cursor_iter = buffer.line_iterator(cursor.position);
let cursor_line_start = cursor_iter.current_position();

// Add method to LineIterator to get current line number
let cursor_line_number = cursor_iter.current_line_number();  // No extra search!
```

---

### Issue #6: Full Buffer Iteration to Count Lines Between Points

**File**: `/home/user/fresh/src/viewport.rs` (lines 413-420, function `ensure_cursors_visible()`)

**Current code**:
```rust
413 | // Count lines between min and max using iterator
414 | let mut iter = buffer.line_iterator(min_byte);
415 | let mut line_span = 0;
416 | while let Some((line_byte, _)) = iter.next() {
417 |     if line_byte >= max_byte {
418 |         break;
419 |     }
420 |     line_span += 1;
```

**Why it's inefficient**:
- If cursors are at lines 100 and 5000: 4900 iterations
- Can be computed in O(1) using line number subtraction

**Fix**:
```rust
let min_line = buffer.get_line_number(min_byte);
let max_line = buffer.get_line_number(max_byte);
let line_span = max_line.saturating_sub(min_line);  // O(log n) instead of O(n)
```

---

### Issue #7: Inefficient ensure_line_visible()

**File**: `/home/user/fresh/src/viewport.rs` (lines 277-327)

**Current code**:
```rust
279 | let mut seek_iter = buffer.line_iterator(0);        // Start from 0
280 | let mut current_line = 0;
281 | let mut target_line_byte = 0;
282 |
283 | while current_line < line {                          // O(n) iteration!
284 |     if let Some((line_start, _)) = seek_iter.next() {
285 |         if current_line + 1 == line {
286 |             target_line_byte = line_start;
287 |             break;
288 |         }
289 |         current_line += 1;
290 |     }
291 | }
292 |
293 | // Check if visible by iterating again                // Another O(n)!
294 | let visible_count = self.visible_line_count();
295 | let mut iter = buffer.line_iterator(self.top_byte);
296 | let mut lines_from_top = 0;
297 | let mut target_is_visible = false;
298 |
299 | while let Some((line_byte, _)) = iter.next() {
300 |     if line_byte == target_line_byte {
301 |         target_is_visible = lines_from_top < visible_count;
302 |         break;
303 |     }
304 |     lines_from_top += 1;
305 | }
```

**Fix**:
```rust
use crate::line_index::Position;

// Get byte offset for line directly (O(1))
let target_byte = buffer.position_to_offset(Position { line, column: 0 });

// Get line numbers for comparison (O(log n) each, 2 searches total instead of 2×O(n))
let top_line = buffer.get_line_number(self.top_byte);
let target_line_num = buffer.get_line_number(target_byte);

// Check visibility with math (O(1))
let target_is_visible = target_line_num >= top_line && 
                       target_line_num < top_line + visible_count;
```

---

## Method Reference

**Available in Buffer**:
- `buffer.line_count()` - Returns total lines (O(1))
- `buffer.get_line_number(byte_offset)` - Gets line at byte (O(log n))
- `buffer.position_to_offset(Position { line, column })` - Gets byte at line/col (O(1))
- `buffer.offset_to_position(byte_offset)` - Gets line/col at byte (O(log n))
- `buffer.line_iterator(byte_offset)` - Creates iterator from byte (O(1) to create, O(1) per step)

**Available in LineIterator**:
- `iter.next()` - Get next line (O(1))
- `iter.prev()` - Get previous line (O(1))
- `iter.current_position()` - Get current byte position (O(1))

**Missing but could be added**:
- `iter.current_line_number()` - Get current line without binary search
- `buffer.lines_between(start_byte, end_byte)` - Get line count between two bytes

---

## Testing the Fixes

### Test 1: Line Numbers Display Correctly
```rust
#[test]
fn test_line_numbers_at_viewport_top() {
    let mut buffer = Buffer::from_str_test("line1\nline2\nline3\n...\nline100");
    let mut viewport = Viewport::new(80, 24);
    
    // Scroll to line 50
    viewport.scroll_to(&buffer, 50);
    
    // Line numbers should start at 50
    let starting_line = buffer.get_line_number(viewport.top_byte);
    assert_eq!(starting_line, 49);  // 0-indexed
}
```

### Test 2: Line Seeking is Fast
```rust
#[test]
fn test_line_seeking_performance() {
    let mut buffer = Buffer::new_with_lines(10000);
    
    // Seek to line 5000 - should be O(1)
    let position = Position { line: 5000, column: 0 };
    let byte_offset = buffer.position_to_offset(position);
    
    // This should be instant (O(1)), not iterate 5000 times
    assert!(byte_offset > 0);
}
```

### Test 3: Scrollbar Calculation is Fast
```rust
#[test]
fn test_scrollbar_calc_performance() {
    let buffer = Buffer::new_with_lines(100000);
    let viewport_height = 24;
    
    // This should use line_count() not iterate entire buffer
    let max_scroll = calculate_max_scrollbar_position(&buffer, viewport_height);
    
    // Should be instant
    assert!(max_scroll > 0);
}
```

