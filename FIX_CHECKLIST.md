# Line Number Implementation - Fix Checklist

## Critical Issues to Fix

### CRITICAL: Line Numbers Display Wrong (Issue #1)
- [ ] Location: `src/ui/split_rendering.rs` line 370
- [ ] Replace: `state.buffer.populate_line_cache(state.viewport.top_byte, visible_count)`
- [ ] With: `state.buffer.get_line_number(state.viewport.top_byte)`
- [ ] Test: Line numbers should match actual viewport position
- [ ] Impact: Fixes all displayed line numbers, plugin hooks, diagnostic indicators
- [ ] Time: 5 minutes

---

## High Priority: Performance Issues

### Issue #2: Buffer Iteration Instead of line_count()
- [ ] Location: `src/editor.rs` lines 4833-4863
- [ ] Function: `calculate_max_scrollbar_position()`
- [ ] Replace entire function with O(1) implementation using:
  - `buffer.line_count()` instead of iterator loop
  - `buffer.position_to_offset()` instead of iterator seek
- [ ] Test: Scrollbar interaction should be instant even on large files
- [ ] Time: 10 minutes
- [ ] Performance gain: 10,000x faster (no iteration for 10k lines)

### Issue #3: Sequential Line Seeking (Instance 1)
- [ ] Location: `src/editor.rs` lines 2628-2642
- [ ] Replace loop-based seeking with `buffer.position_to_offset()`
- [ ] Add import: `use crate::line_index::Position`
- [ ] Test: Navigation to arbitrary line numbers should be instant
- [ ] Time: 5 minutes

### Issue #4: Sequential Line Seeking (Instance 2)
- [ ] Location: `src/editor.rs` lines 4677-4682
- [ ] Same fix as Issue #3
- [ ] Test: Scrollbar position calculation should be instant
- [ ] Time: 5 minutes

### Issue #5: Sequential Line Seeking (Instance 3)
- [ ] Location: `src/editor.rs` lines 4850-4861
- [ ] Can be eliminated by fixing Issue #2
- [ ] Should already be resolved after fixes #2, #3, #4
- [ ] Time: 0 minutes (resolved by other fixes)

---

## Medium Priority: Optimizations

### Issue #6: Repeated get_line_number() Calls
- [ ] Location: `src/ui/split_rendering.rs` lines 101-117
- [ ] Option A: Cache the result in a local variable
- [ ] Option B: Compute once at start of function and reuse
- [ ] Test: Verify scrollbar calculation still works correctly
- [ ] Time: 10 minutes
- [ ] Performance gain: Reduces binary searches by 50%

### Issue #7: Redundant Binary Search in ensure_visible()
- [ ] Location: `src/viewport.rs` lines 217-272
- [ ] Add `current_line_number()` method to LineIterator
- [ ] Or: Cache line number after iterator call
- [ ] Test: Cursor movement should work correctly
- [ ] Time: 15 minutes

### Issue #8: Double Iteration in ensure_line_visible()
- [ ] Location: `src/viewport.rs` lines 277-327
- [ ] Replace iteration with:
  - `position_to_offset()` to get byte position
  - `get_line_number()` to get line numbers
  - Line number arithmetic to check visibility
- [ ] Test: Ensure line visibility works correctly
- [ ] Time: 15 minutes

### Issue #9: Iteration for Line Span
- [ ] Location: `src/viewport.rs` lines 413-420
- [ ] Replace with: `max_line.saturating_sub(min_line)`
- [ ] Test: Multi-cursor operations should work correctly
- [ ] Time: 5 minutes

---

## Testing Checklist

### Functional Testing
- [ ] Line numbers display correctly at top of viewport
- [ ] Line numbers update correctly after scrolling
- [ ] Line numbers correct after jumping to line
- [ ] Plugin render-line hooks receive correct line numbers
- [ ] Diagnostic indicators appear at correct lines

### Performance Testing
- [ ] Scrolling is smooth (no stutters)
- [ ] Scrollbar clicking is responsive
- [ ] Navigation to arbitrary lines is instant
- [ ] Opening large files (10k+ lines) is not slow
- [ ] Rendering performance improved (profile with flamegraph)

### Edge Cases
- [ ] Empty buffer (0 lines)
- [ ] Single line buffer
- [ ] Buffer without final newline
- [ ] Large files (100k+ lines)
- [ ] Multi-cursor operations
- [ ] Split panes (1, 2, 4 splits)

---

## Validation Commands

Run these to verify fixes:

```bash
# Verify compilation
cargo build

# Run all tests
cargo test

# Run specific test files
cargo test viewport
cargo test buffer
cargo test editor

# Performance test (if flamegraph available)
cargo flamegraph -- --test-threads=1

# Check line number display
# (Manual: Open a file and verify line numbers match position)
```

---

## Documentation Updates

After fixes:
- [ ] Add comments explaining why we use `position_to_offset()` instead of iteration
- [ ] Document performance characteristics of line-related methods
- [ ] Update architecture documentation if needed
- [ ] Add regression test for populate_line_cache bug

---

## Sign-off

- [ ] All fixes implemented
- [ ] All tests passing
- [ ] Performance verified (5-10x improvement)
- [ ] Code reviewed
- [ ] Merged to main branch

---

## Time Estimate

| Phase | Tasks | Time |
|-------|-------|------|
| Critical | Issue #1 | 5 min |
| High Priority | Issues #2-5 | 30 min |
| Medium Priority | Issues #6-9 | 50 min |
| Testing | Full test suite | 20 min |
| **Total** | | **105 minutes** |

---

## Files Modified

- [ ] `src/ui/split_rendering.rs`
- [ ] `src/editor.rs`
- [ ] `src/viewport.rs`
- [ ] `src/buffer.rs` (possibly add helper methods)

## Files Reviewed (No Changes)

- [ ] `src/margin.rs`
- [ ] `src/line_index.rs`

