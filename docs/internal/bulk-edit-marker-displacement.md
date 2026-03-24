# Bulk Edit Marker Displacement Bug

## Objective

Fix virtual text (inlay hints, inline diagnostics, etc.) becoming visually displaced after any bulk edit operation — toggle comment, cut, paste, indent/dedent, multi-cursor edits.

## Problem

### Symptom

After toggling a line comment (inserting `// ` on line 1), inlay hints on subsequent lines render at incorrect column positions — shifted left by the number of inserted bytes. For example:

```
Before:  int x = add(a: 1, b: 2);    ← hints "a:" and "b:" correctly placed
After:   int x = aa: dd(b: 1, 2);    ← hints displaced 3 chars left into the text
```

The displacement equals the byte length of the inserted text (3 bytes for `// `).

### Root Cause

`apply_events_as_bulk_edit` (`app/mod.rs:2501-2741`) modifies the buffer but does **not** adjust the marker list or margins.

The function:
1. Converts events to edit tuples (line 2542-2554)
2. Calls `state.buffer.apply_bulk_edits(&edit_refs)` — modifies only the piece tree (line 2566)
3. Manually adjusts cursor positions (lines 2571-2696)
4. Invalidates the highlighter (line 2703)
5. **Never calls `marker_list.adjust_for_insert/delete`**
6. **Never calls `margins.adjust_for_insert/delete`**

Compare with the normal single-edit path in `state.rs:331-332`:

```rust
self.marker_list.adjust_for_insert(position, text.len());
self.margins.adjust_for_insert(position, text.len());
```

### Why Rendering Breaks

The rendering pipeline builds two things from the same buffer state:

1. **`char_source_bytes`** (in `ViewLine`) — absolute byte offsets read from the buffer after the edit. These are correct.
2. **`virtual_text_lookup`** (HashMap keyed by byte position) — built by calling `marker_list.get_position()` for each virtual text marker. Since markers were never adjusted, these return **stale** byte positions.

The rendering loop (`split_rendering.rs:4752`) matches `byte_pos` from (1) against keys from (2). With stale marker positions, the lookup hits at the wrong character, displacing the virtual text.

### Affected Features

All callers of `apply_events_as_bulk_edit`:

| Feature | File | Line |
|---------|------|------|
| Toggle comment | `app/render.rs` | 3253, 3467 |
| Cut | `app/clipboard.rs` | 424, 482 |
| Paste | `app/clipboard.rs` | 605 |
| Indent/Dedent | `app/render.rs` | 3985 |
| Multi-cursor typing | `app/input.rs` | 1078 |
| Auto-close pairs | `app/input.rs` | 3698, 3770 |

### Affected Marker-Based Features

Any feature storing positions via the marker system:

- **Virtual text** (inlay hints, ghost text, inline diagnostics)
- **Overlays** (semantic tokens, search highlights, diagnostic underlines) — each overlay has `start_marker` and `end_marker`
- **Margins** (breakpoints, line annotations)

Syntax highlights are NOT affected — they are recomputed fresh each frame from the buffer.

## Discussion: How Other Features Compare

### Diagnostic Highlights (Overlays)

Diagnostic underlines and semantic tokens are stored as **Overlay** objects, each backed by two markers (start + end). They use `query_viewport()` which resolves marker positions at render time. Since bulk edits skip marker adjustment, overlays are equally affected — they just haven't been reported because:

1. Diagnostics are re-fetched from the LSP shortly after the edit
2. Semantic tokens also get refreshed
3. The transient misalignment is brief and less visually obvious than inlay hint displacement

### Syntax Highlights

Not affected. `highlight_viewport()` re-scans the buffer text each frame. No markers involved.

### Search Highlights

Stored as overlays (markers). The code at `app/mod.rs:2717` explicitly notes: "Do NOT clear search overlays — markers track through edits for F3/Shift+F3." But since markers aren't adjusted for bulk edits, F3 after a bulk edit would jump to the wrong position.

### Cursor Positions

Manually adjusted with custom shift logic in `apply_events_as_bulk_edit` (lines 2571-2696). This is correct but redundant — cursors could use the marker system too.

## Alternatives

### Option A: Add marker/margin adjustments to `apply_events_as_bulk_edit`

After `state.buffer.apply_bulk_edits()`, iterate through the sorted edit list and call `marker_list.adjust_for_insert/adjust_for_delete` for each edit.

```rust
// After line 2566: state.buffer.apply_bulk_edits(&edit_refs)
// Edits are sorted descending by position — apply marker adjustments in same order
for (pos, del_len, text) in &edits {
    if *del_len > 0 {
        state.marker_list.adjust_for_delete(*pos, *del_len);
        state.margins.adjust_for_delete(*pos, *del_len);
    }
    if !text.is_empty() {
        state.marker_list.adjust_for_insert(*pos, text.len());
        state.margins.adjust_for_insert(*pos, text.len());
    }
}
```

| Pro | Con |
|-----|-----|
| Minimal change (~10 lines) | Must maintain correct edit ordering |
| Fixes all callers at once | Sequential marker adjustments (O(k log n) for k edits) |
| Consistent with single-edit path | Undo/redo uses buffer snapshots — need to verify marker consistency |
| Easy to reason about correctness | |

### Option B: Batch marker adjustment via `marker_list.apply_bulk_edits()`

Add a new method to `MarkerList` that accepts a list of edits and applies all adjustments in a single tree pass, mirroring `buffer.apply_bulk_edits()`.

| Pro | Con |
|-----|-----|
| Most efficient for large edit batches | Significant implementation complexity in IntervalTree |
| Single atomic operation | Lazy propagation already makes sequential calls O(log n) each |
| Clean API symmetry with buffer | Over-engineered for the current problem |

### Option C: Re-request virtual text from LSP after bulk edit

The LSP is already notified at line 2737. Just wait for the response to fix positions.

| Pro | Con |
|-----|-----|
| No marker code changes | Doesn't fix transient displacement (user sees flicker) |
| | Doesn't fix non-LSP virtual text (plugins, etc.) |
| | Doesn't fix overlay displacement (search, diagnostics) |
| | Treats symptom, not cause |

### Option D: Track pending edits and correct in `build_lookup`

Maintain a per-frame "pending displacement" list; apply corrections when building the virtual text lookup.

| Pro | Con |
|-----|-----|
| Targeted to virtual text only | Duplicates the marker system's purpose |
| | Doesn't fix overlays, margins, search |
| | Complex and error-prone |
| | Violates the design of the marker system |

## Undo/Redo Analysis

### How BulkEdit undo works today

BulkEdit uses **snapshot-based** undo, not event-replay:

1. Before edit: `old_snapshot = buffer.snapshot_buffer_state()` — clones piece tree + string buffers
2. After edit: `new_snapshot = buffer.snapshot_buffer_state()`
3. Undo: `inverse()` swaps the two snapshots (`event.rs:452-454`), then `state.apply()` calls `buffer.restore_buffer_state(old_snapshot)` — wholesale replaces the piece tree (`buffer.rs:2076`)
4. Redo: Same mechanism with the swapped event

**Snapshots only cover the buffer (piece tree + string buffers). They do NOT snapshot or restore the marker list or margins.**

### BulkEdit undo is already broken today

Currently, since `apply_events_as_bulk_edit` never adjusts markers, the forward and reverse paths are symmetrically broken — markers never move, so undo "accidentally" leaves them at the right positions for the restored buffer. But this breaks as soon as the LSP refreshes markers in between:

```
1. Markers at [53, 68]
2. BulkEdit inserts "// "  → buffer +3, markers stay at [53, 68]   ← wrong but "stable"
3. LSP responds             → clears markers, creates new at [56, 71]  ← now correct
4. Undo                     → buffer restored, markers still at [56, 71]  ← WRONG
```

After step 4, markers are displaced by +3 until the LSP responds again. **This is an existing bug.**

### Single-edit undo also has a pre-existing limitation

For normal Insert/Delete events, undo works via event inversion — the inverse event goes through `apply_insert`/`apply_delete` which properly calls `marker_list.adjust_for_insert/delete`. This is correct for simple cases.

However, **delete operations destroy marker position information irreversibly:**

```
1. Markers at [10, 12, 14] inside a range
2. Delete [10, 15)  → all three markers collapse to position 10
3. Undo (Insert)    → markers at 10 all shift to 15 (right affinity) or stay at 10 (left affinity)
```

After undo, markers are bunched at one end, not restored to [10, 12, 14]. The original spread within the deleted range is lost. This is a **fundamental limitation** of marker-based position tracking shared by all editors (VSCode, Neovim, etc.).

### Why this doesn't block our fix

The marker system was never designed to be undo-correct on its own. It relies on external providers (LSP) refreshing positions after every edit. This is true today for:

- Single-edit undo (delete collapses markers irreversibly)
- BulkEdit undo (snapshot doesn't cover markers)
- Any sequence where LSP refreshes markers between edits

Our fix (Option A) doesn't change this contract. It makes the **forward path** correct — markers track through bulk edits just like they track through single edits. The undo path has the same transient-incorrectness window it always had, which the LSP resolves on its next response.

**Net effect:** Strictly better. The forward path (which users see more and which is currently visibly broken) becomes correct. The undo path remains as correct/incorrect as it already is.

## Recommendation

**Option A: Add marker/margin adjustments to `apply_events_as_bulk_edit`.**

This is the right fix because:

1. **It's the minimal change** (~10 lines) that fixes the root cause
2. **It fixes all callers** — toggle comment, cut, paste, indent/dedent, multi-cursor
3. **It fixes all marker-based features** — virtual text, overlays, search highlights, margins
4. **It's consistent** with the single-edit path (`state.rs:331-332`)
5. **Undo is not a concern** — the marker system already relies on LSP re-requests for undo correctness in both the single-edit and bulk-edit paths; this fix doesn't change that contract
6. **Performance is fine** — lazy propagation in the IntervalTree makes each `adjust_for_edit` O(log n); for k sub-edits this is O(k log n), acceptable for typical bulk edits (< 50 sub-edits)

Option B (batch marker API) is a valid future optimization if profiling shows bulk edits with many sub-edits are slow, but it's unnecessary complexity for now.

## Implementation Plan

### Step 1: Add marker and margin adjustments

In `app/mod.rs`, after the `state.buffer.apply_bulk_edits(&edit_refs)` call at line 2566, add marker and margin adjustments for each sub-edit. The edits are already sorted descending by position (line 2557), which is the correct order — later positions first avoids cascading shifts.

```rust
// Adjust markers and margins for each edit (descending position order)
for (pos, del_len, text) in &edits {
    if *del_len > 0 {
        state.marker_list.adjust_for_delete(*pos, *del_len);
        state.margins.adjust_for_delete(*pos, *del_len);
    }
    if !text.is_empty() {
        state.marker_list.adjust_for_insert(*pos, text.len());
        state.margins.adjust_for_insert(*pos, text.len());
    }
}
```

### Step 2: Add test coverage

Add a test that:
1. Sets up a buffer with virtual text markers at known positions
2. Applies a bulk edit (e.g., insert `// ` at line start)
3. Verifies marker positions are correctly adjusted
4. Verifies `build_lookup` returns correct byte positions
5. Optionally: verifies rendering output shows hints at correct columns

### Step 3: Verify search overlay correctness

After the fix, verify that F3 (find next) works correctly after a bulk edit by checking that search overlay markers track correctly through toggle-comment and indent/dedent.

### Step 4: Edge cases to test

- **Multi-cursor edits**: Multiple inserts at different positions. Each sub-edit must adjust markers correctly without double-shifting.
- **Mixed insert + delete**: Cut operations that delete ranges. Both `adjust_for_delete` and `adjust_for_insert` must be called in the right order.
- **Undo after bulk edit**: Verify that undo + LSP refresh produces correct marker positions (same behavior as today's single-edit undo).
- **Empty edits**: No-op sub-edits (0-length insert or delete) should be harmless.
- **Edits at buffer boundaries**: Insert at position 0, delete at end of buffer.
