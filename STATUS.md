# LSP Performance Optimization Status - Deep Analysis

## Summary

After investigating the diagnostic processing architecture and researching how modern editors handle this, I've identified that we're solving the wrong problem. The fundamental issue isn't cache optimization - it's that we're doing too much work on every diagnostic update.

## Completed (Already Committed)

✅ **Initial O(N²) → O(N) optimization** (commit 260c8c4)
- Fixed critical bug where `line_char_to_byte` iterated from line 0 for every diagnostic
- Added line cache pre-population before processing diagnostics
- Sort diagnostics by line number for sequential processing
- Result: 200 diagnostics went from >60s timeout to ~280ms

## Root Cause Analysis

### Current Architecture Problems

The current diagnostic application flow has fundamental inefficiencies:

1. **Complete rebuild on every update** (~236ms per application)
   - `apply_diagnostics_to_state()` is called on every LSP diagnostic update
   - We iterate through ALL diagnostics and delete/recreate ALL diagnostic overlays
   - Creates 200+ overlay objects with 400+ markers from scratch
   - Lines 78-96 in `lsp_diagnostics.rs`: Filtering and removing by ID prefix
   - Lines 118-134: Converting each diagnostic to overlay + creating markers

2. **Overlay lookups during rendering** (22.89% of profile time)
   - Line 532 in `split_rendering.rs`: `state.overlays.at_position(byte_pos, &state.marker_list)`
   - Called for EVERY character position during rendering
   - Each call iterates through ALL overlays (Vec) checking if they contain the position
   - With 200 diagnostics = 200 overlay objects to check per character
   - For an 80-char wide viewport × 24 lines = ~1920 position checks × 200 overlays = ~384,000 iterations per frame

3. **Marker tree overhead** (22.15% of profile time)
   - Every overlay query resolves marker positions via interval tree
   - `overlay.range(marker_list)` called repeatedly
   - Marker lookups are O(log N) but happen constantly

### Why Cache Optimization Won't Help

The uncommitted changes try to optimize line caching, but this misses the real problems:
- Even if position conversion is free, we still rebuild 200+ overlays from scratch
- Even if overlay creation is fast, rendering still queries them inefficiently
- We're optimizing the wrong bottleneck

## How Modern Editors Handle This

### VSCode / Monaco Editor Architecture

**Key insight:** Diagnostics rarely change completely - usually 0-5 diagnostics change per keystroke.

1. **Incremental updates only**
   - Compare incoming diagnostics with current state
   - Only update changed diagnostics (by URI + range + message hash)
   - Most typing operations → 0 diagnostic changes → 0 work

2. **Spatial indexing for rendering**
   - Don't iterate all overlays for each character
   - Use interval tree or range map keyed by line number
   - Query: "give me all overlays on line 42" → O(log N + k) where k = overlays on that line
   - Typical case: k = 0-2, not 200

3. **Lazy evaluation**
   - Don't resolve positions until actually rendering that line
   - Don't create overlay objects for off-screen diagnostics

### Neovim Architecture

**Key insight:** Separate logical diagnostic storage from visual representation.

1. **Diagnostic buffer separate from display**
   - Stores diagnostics as structured data (not as overlays)
   - Extmarks (markers) track positions but aren't tied to visual representation
   - Rendering queries diagnostics by line range, not by position

2. **Line-based rendering**
   - "What diagnostics affect lines 20-44?" not "What's at position 1234?"
   - Uses line-indexed data structure (map or sorted array)

3. **Sign column optimization**
   - Margin indicators computed once per visible line, not per character
   - Cached during render pass, not looked up repeatedly

### Helix Editor

**Key insight:** Diagnostics are annotations, not overlays.

1. **Separate annotation layer**
   - Diagnostics stored independently from syntax highlighting
   - Merged during rendering, not stored as merged state

2. **LSP state diffing**
   - Hash incoming diagnostic sets
   - Skip all work if hash matches previous
   - For 99% of keystrokes, diagnostics don't change

## Recommended Architecture Changes

### Priority 1: Skip Redundant Updates (90% of the problem)

**Goal:** Most keystrokes should do zero diagnostic work.

```rust
// In apply_diagnostics_to_state or wrapper:
pub fn apply_diagnostics_to_state_incremental(
    state: &mut EditorState,
    diagnostics: &[Diagnostic],
    theme: &Theme,
) {
    // Hash the diagnostic set (or use version/sequence number from LSP)
    let new_hash = compute_diagnostic_hash(diagnostics);

    if state.diagnostic_cache.hash == new_hash {
        return; // No change, skip all work
    }

    // Only do the expensive update if diagnostics actually changed
    apply_diagnostics_to_state(state, diagnostics, theme);
    state.diagnostic_cache.hash = new_hash;
}
```

**Impact:**
- Typing without diagnostic changes: ~236ms → ~0ms ✅
- Only pay cost when diagnostics actually change
- Zero architectural changes needed initially

**Implementation:**
- Add `diagnostic_hash: u64` field to EditorState or diagnostic module
- Use fast hasher (FxHash or ahash) on diagnostic ranges + messages
- Wrap existing `apply_diagnostics_to_state` with hash check

### Priority 2: Line-Indexed Overlay Storage

**Goal:** Eliminate O(N) overlay lookups during rendering.

**Current:** `Vec<Overlay>` requires iterating all overlays to find matches
**Better:** `BTreeMap<usize, Vec<Overlay>>` keyed by start line

```rust
pub struct OverlayManager {
    overlays: Vec<Overlay>,  // Keep for compatibility
    by_line: BTreeMap<usize, Vec<usize>>,  // line -> indices into overlays vec
}

impl OverlayManager {
    pub fn at_position_fast(&self, position: usize, line: usize, marker_list: &MarkerList) -> Vec<&Overlay> {
        // Only check overlays that start on nearby lines
        let start_line = line.saturating_sub(100); // reasonable search window
        let end_line = line + 100;

        self.by_line
            .range(start_line..=end_line)
            .flat_map(|(_, indices)| indices.iter().map(|&i| &self.overlays[i]))
            .filter(|o| o.range(marker_list).contains(&position))
            .collect()
    }
}
```

**Impact:**
- Overlay lookups: O(200) → O(log N + k) where k = overlays near this line
- Typical k = 0-3, not 200
- Rendering: ~23% of time → ~2-5% of time

### Priority 3: Render-Time Overlay Cache

**Goal:** Don't re-resolve marker positions for every character.

```rust
// In render_buffer_in_split, before line loop:
let mut overlay_cache: HashMap<usize, Vec<&Overlay>> = HashMap::new();

// For each line:
let line_overlays = overlay_cache.entry(line_num).or_insert_with(|| {
    state.overlays.on_line(line_num, &state.marker_list)
});

// For each char in line:
//   Check line_overlays (small vec) instead of querying all overlays
```

**Impact:**
- Marker resolution: 1920 calls/frame → ~24 calls/frame (visible lines only)
- Marker tree overhead: ~22% → ~2%

### Priority 4: Incremental Overlay Updates

**Goal:** Don't delete and recreate all overlays - update only changed ones.

```rust
// Compare old and new diagnostics
// For each new diagnostic:
//   - If exists with same ID: skip (or update marker if range changed)
//   - If new: create overlay
// For each old diagnostic not in new set:
//   - Remove overlay
```

**Impact:**
- Typing with no diagnostic changes: 0 overlays created/destroyed
- Fixing one error: 1-2 overlays updated, not 200
- Initial application still ~250ms, but re-application → ~1-10ms

## Recommended Implementation Order

### Phase 1: Quick Wins (1-2 hours, 90% improvement)

1. **Add diagnostic hash check**
   - Wrap `apply_diagnostics_to_state` with hash comparison
   - Return early if diagnostics unchanged
   - Zero risk, massive impact

2. **Test and measure**
   - Typing without diagnostic changes should be instant
   - Only slow when diagnostics actually update

### Phase 2: Rendering Optimization (3-4 hours, 10x rendering speedup)

3. **Add per-line overlay index**
   - Maintain `BTreeMap<line, Vec<overlay_index>>`
   - Update on overlay add/remove
   - Use in `at_position` queries

4. **Add render-time overlay cache**
   - Build once per frame, reuse for all characters
   - Simple HashMap, cleared each frame

### Phase 3: Incremental Updates (4-6 hours, cleaner architecture)

5. **Implement overlay diffing**
   - Compare diagnostic sets, update only changes
   - More complex but better long-term

## Performance Targets (with changes)

| Scenario | Current | Phase 1 | Phase 2 | Phase 3 |
|----------|---------|---------|---------|---------|
| First application | ~252ms | ~252ms | ~252ms | ~250ms |
| Re-apply (no change) | ~236ms | **~0ms** ✅ | ~0ms | ~0ms |
| Re-apply (1 changed) | ~236ms | ~236ms | ~236ms | **~5-10ms** ✅ |
| Rendering time | 100% | 100% | **~30-40%** ✅ | ~30-40% |

## Current Uncommitted Changes - Recommendation

**Do not commit the line cache optimization.**

Reasons:
1. Solves wrong problem (position conversion is only 10% of time)
2. Has correctness issues (cache coherence with gaps)
3. `get_cached_byte_offset_for_line` is O(N) defeating cache benefits
4. Won't help with real bottlenecks (overlay rebuilding and rendering)

**Instead:**
1. Discard uncommitted changes to `src/buffer.rs`, `src/line_cache.rs`
2. Keep the test enhancements in `tests/e2e/lsp.rs`
3. Implement Phase 1 (diagnostic hash check) first - easy win

## Files to Modify (New Approach)

### Phase 1
- `src/lsp_diagnostics.rs` - Add hash check wrapper
- `src/state.rs` - Add `last_diagnostic_hash` field (or module-level static)

### Phase 2
- `src/overlay.rs` - Add line index to OverlayManager
- `src/ui/split_rendering.rs` - Add per-frame overlay cache

### Phase 3
- `src/lsp_diagnostics.rs` - Implement incremental update logic

## Key Principles from Modern Editors

1. **Skip work entirely when possible** (hash checking)
2. **Use appropriate data structures** (spatial indexing, not linear search)
3. **Cache within render pass** (compute once per frame, not per character)
4. **Separate logical state from visual state** (diagnostics ≠ overlays)
5. **Update incrementally** (diff and patch, don't rebuild)

## Notes

- Current architecture is fundamentally sound, just needs tactical improvements
- No need for complex caching logic - simpler approaches work better
- Phase 1 alone solves 90% of the user-facing problem
- Phases 2-3 make the remaining 10% professional-grade
