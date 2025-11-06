# FIXED: Critical Bug in Marker-Based Overlay System

## Summary

**Bug:** Overlays didn't render colors in real UI or E2E tests
**Root Cause:** Marker list not initialized with buffer size when loading files
**Fix:** Initialize marker list in `EditorState::from_file()`
**Result:** All tests pass ✅, overlays render correctly ✅

## The Bug Investigation Journey

### User Report
> "in real ui (no test harness) i don't see any colors at all"

This led to discovering that E2E tests were also failing - TODO overlays showed `Reset` color instead of RGB.

### Initial Hypothesis (WRONG)
We thought this was a test harness or rendering pipeline issue because:
- Overlay unit tests passed
- FIXME rendered correctly
- TODO didn't render

### The Smoking Gun

Added extensive tracing and found:
```rust
Overlay todo_TODO_25 at position 25: range=0..0, contains=false
```

The overlay created for range 25..29 was returning **0..0**! The markers were broken.

### Root Cause Analysis

```rust
// Marker creation
Creating marker MarkerId(74) at position 25 with right affinity
Failed to find gap for position 25! Cumulative pos reached: 22, total entries: 3
```

The marker list only knew about **22 bytes**, but we were trying to create a marker at position **25**!

#### Why?

1. **Test loads file:** `"// TODO: Original comment\n"` (26 bytes)
   - Buffer: 26 bytes ✅
   - Marker list: `Gap(0)` ❌

2. **Test types at position 0:** `"// FIXME: New comment\n"` (23 bytes)
   - Each character triggers `adjust_for_insert(pos, 1)`
   - Marker list grows: `Gap(0)` → `Gap(1)` → ... → `Gap(22)` ✅

3. **Plugin creates overlay for TODO at 25..29:**
   - Buffer has 49 bytes total (23 + 26) ✅
   - Marker list only knows about 22 bytes ❌
   - **Marker creation FAILS**
   - Markers return position 0 (default)
   - Overlay has range 0..0
   - Overlay doesn't render ❌

### The Fix

**File:** `src/state.rs:90-98`

```rust
// Initialize marker list with buffer size
let mut marker_list = MarkerList::new();
if buffer.len() > 0 {
    tracing::debug!(
        "Initializing marker list for file with {} bytes",
        buffer.len()
    );
    marker_list.adjust_for_insert(0, buffer.len());
}
```

Now when a file is loaded:
- Buffer: 26 bytes ✅
- Marker list: `Gap(26)` ✅

When text is inserted at position 0:
- Marker list: `Gap(22), [markers for FIXME], Gap(26)` ✅
- Total bytes: 48 ✅

When overlay is created at position 25:
- Marker list knows about all 48 bytes ✅
- Markers created successfully at 25 and 29 ✅
- Overlay has correct range ✅
- Overlay renders with RGB color ✅

## Test Results

### Before Fix
```
test test_todo_highlighter_updates_on_edit ... FAILED
test test_todo_highlighter_updates_on_delete ... FAILED

Found TODO at (13, 2) with background: Reset  ❌
```

### After Fix
```
test test_todo_highlighter_plugin ... ok
test test_todo_highlighter_toggle ... ok
test test_todo_highlighter_disable ... ok
test test_todo_highlighter_updates_on_edit ... ok
test test_todo_highlighter_updates_on_delete ... ok

test result: ok. 5 passed; 0 failed

Found TODO at (13, 1) with background: Rgb(255, 165, 0) ✅
Found FIXME at (13, 1) with background: Rgb(255, 0, 0) ✅
Found TODO on line 2 at (13, 2) with background: Rgb(255, 165, 0) ✅
```

## Impact

This bug affected **all files loaded from disk**:
- Opening existing files ❌
- TODO highlighter plugin ❌
- LSP diagnostic overlays ❌
- Any overlay-based feature ❌

The fix ensures marker lists are properly initialized, making the marker-based overlay system **production-ready**.

## Lessons Learned

1. **Event-driven != Initialization**: The marker list properly handled `Insert` events but wasn't initialized for content already in the buffer.

2. **State invariants**: When creating state from existing data (files), all related state must be initialized consistently.

3. **Comprehensive tracing**: Adding detailed tracing at multiple levels (creation, querying, rendering) was essential to finding the root cause.

## Related Files

- `src/state.rs`: The fix (marker list initialization)
- `src/marker.rs`: Added extensive tracing
- `src/overlay.rs`: Added debugging for position queries
- `tests/e2e/plugin.rs`: Tests that exposed the bug

## Verification

To verify the fix works:

1. Build editor: `cargo build --release`
2. Open a file with TODO comments
3. Run command: "TODO Highlighter: Enable"
4. Observe: Keywords highlighted with correct colors ✅

Or run tests:
```bash
cargo test test_todo_highlighter --test e2e_tests
# All 5 tests pass ✅
```
