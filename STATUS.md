# LSP Performance Optimization Status

## Summary

Significant progress has been made on LSP diagnostic performance, but additional bottlenecks remain in repeated diagnostic applications during typing.

## Completed (Already Committed)

✅ **Initial O(N²) → O(N) optimization** (commit 260c8c4)
- Fixed critical bug where `line_char_to_byte` iterated from line 0 for every diagnostic
- Added line cache pre-population before processing diagnostics
- Sort diagnostics by line number for sequential processing
- Result: 200 diagnostics went from >60s timeout to ~280ms

## Current Performance Issues (Not Yet Committed)

### Issue 1: Repeated Diagnostic Application Still Slow

**Problem:** When LSP sends diagnostics on every keystroke (which is normal), re-applying the same diagnostics takes ~236ms average, nearly as long as the first application (252ms).

**Root Cause Analysis from flame3.svg:**
- `apply_diagnostics_to_state`: 35.02% of total time
  - `populate_line_cache`: 24.19% - Still expensive even with cache
  - `OverlayManager::at_position`: 22.89% - Frequent overlay lookups during rendering
  - `marker_tree::IntervalTree::get_position`: 22.15% - Marker tree queries
  - `lsp_position_to_byte`: 10.20% - Position conversion still has overhead

**Current Optimization Attempt (Uncommitted):**
- Added `max_cached_line` tracking to LineCache
- Optimize `populate_line_cache` to skip if target line already cached
- Uses `get_cached_byte_offset_for_line(target_line)` check before iterating

**Remaining Issues:**
1. **Cache coherence concern**: The optimization assumes contiguous cache coverage. After edits, cache may have gaps (e.g., lines 0-10 and 50-60 cached, but 11-49 missing). Current check may give false positives.

2. **get_cached_byte_offset_for_line is O(N)**: Iterates through all cache entries to find a line number. This partially defeats the caching benefit.

3. **Still iterating when cache is full**: Even with optimizations, re-application still takes 236ms. The cache should be fully populated after first application, so subsequent calls should be nearly instant but aren't.

### Issue 2: Overlay/Marker Tree Overhead

**Problem:**
- `OverlayManager::at_position`: 22.89% of execution time
- `marker_tree::IntervalTree::get_position`: 22.15% of execution time

These are called during rendering for every character position to check for overlays (diagnostics, highlights, etc.).

**Potential Solutions:**
- Cache overlay queries within a single render pass
- Use spatial indexing for overlay lookups
- Defer overlay resolution until actually needed for display

## Test Results

Performance test (`test_lsp_typing_performance_with_many_diagnostics`):
- First application: 252ms for 200 diagnostics ✅
- Re-application (avg of 10): 236ms ⚠️ (should be much faster with cache)
- Test now reports times without hard limits (per user request about slow machines)

## Recommendations

### Short-term (High Priority)
1. **Investigate why cache isn't helping on re-application**
   - Add instrumentation to see if cache is being invalidated
   - Check if we're actually hitting the fast path on re-application
   - Profile specifically the re-application case

2. **Fix cache coherence**
   - Either ensure contiguous cache coverage (fill gaps)
   - Or implement proper range checking that handles discontiguous cache
   - Consider a bitmap or interval tree for efficient range queries

3. **Optimize get_cached_byte_offset_for_line**
   - Maintain reverse index: `HashMap<line_number, byte_offset>`
   - Or use the BTreeMap more efficiently (currently keyed by byte_offset)

### Medium-term
4. **Reduce overlay lookup overhead**
   - Profile overlay/marker tree queries during rendering
   - Consider caching overlay query results per render frame
   - Investigate if we're doing redundant lookups

5. **Cache diagnostic state**
   - Compare incoming diagnostics to previous diagnostics
   - Skip re-application if diagnostics haven't changed
   - Hash diagnostic set for quick comparison

### Files Modified (Uncommitted)
- `src/buffer.rs` - Added cache check optimization in `populate_line_cache`
- `src/line_cache.rs` - Added `max_cached_line` tracking and helper methods
- `tests/e2e/lsp.rs` - Enhanced test to measure re-application performance

## Performance Comparison

| Scenario | Before Fix | After Commit | Current Uncommitted |
|----------|-----------|--------------|-------------------|
| First application (200 diags) | >60s timeout | ~280ms | ~252ms |
| Re-application (avg) | N/A | N/A | ~236ms |
| Target (with full caching) | N/A | N/A | <50ms |

## Notes

- The uncommitted changes show promise but have correctness concerns around cache coherence
- Need to balance optimization complexity vs. correctness
- Current state is functional but not optimal for typing performance
