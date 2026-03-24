# Bulk Edit Marker Displacement — Post-Fix Analysis

## Status

**The forward-path bug is FIXED** (commit `abd52d7`). BulkEdit operations now adjust markers
and margins correctly. This document is updated to reflect the fix and analyze the **remaining
undo/redo gaps**.

## What Was Fixed

### Original Problem (Issue #1263)

`apply_events_as_bulk_edit` (`app/mod.rs`) modified the buffer via `apply_bulk_edits()` but
never called `marker_list.adjust_for_insert/delete` or `margins.adjust_for_insert/delete`.
This caused virtual text (inlay hints, ghost text, inline diagnostics), overlays (search
highlights, diagnostic underlines), and margins (breakpoints) to render at stale byte positions
after toggle-comment, cut, paste, indent/dedent, and multi-cursor edits.

### The Fix (`app/mod.rs:2568-2580`)

```rust
// Adjust markers and margins for each edit (descending position order,
// matching the order used by apply_bulk_edits — later positions first
// so earlier edits don't shift positions of later ones)
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

### Test Coverage

E2E test `test_comment_does_not_displace_inlay_hints` (`crates/fresh-editor/tests/e2e/lsp.rs`)
verifies that toggling a comment does not displace inlay hints on subsequent lines.

---

## Remaining Gap: BulkEdit Undo/Redo Does Not Adjust Markers

### The Problem

BulkEdit undo uses **snapshot-based** restoration (`state.rs:688-711`):

1. `buffer.restore_buffer_state(snapshot)` — replaces piece tree wholesale
2. Cursor positions updated explicitly
3. **No marker or margin adjustment**

This means after a BulkEdit + LSP refresh + undo sequence, markers are displaced:

```
1. Markers at [53, 68]
2. BulkEdit inserts "// "  → markers adjusted to [56, 71]     ✅ (fixed)
3. LSP responds             → clears old markers, creates new at [56, 71]  ✅
4. Undo                     → buffer reverts, markers stay at [56, 71]     ❌ WRONG
5. LSP responds again       → markers corrected to [53, 68]               ✅ (eventual)
```

Between steps 4 and 5, markers are displaced. The user sees a brief flash of misaligned
hints/diagnostics until the LSP refreshes. This is the **same transient incorrectness** that
existed before the fix — the fix didn't introduce it, but it didn't resolve it either.

### Single-Edit Undo Comparison

Single-edit undo/redo works correctly because event inversion produces a Delete/Insert event
that goes through `apply_delete`/`apply_insert` in `state.rs`, which calls the marker
adjustment methods. The inverse edit naturally produces the correct marker shift.

The one exception: **delete operations destroy intra-range marker spread irreversibly.** If
markers at [10, 12, 14] are inside a deleted range, they collapse to position 10. Undoing the
delete (re-inserting text) shifts them all to one end, not back to [10, 12, 14]. This is a
fundamental limitation shared by all editors.

### Why BulkEdit Undo Can't Simply Replay Marker Adjustments

The snapshot approach bypasses the edit-by-edit path entirely. `restore_buffer_state()` replaces
the piece tree atomically — there are no individual inserts/deletes to trigger marker adjustment.

To fix this properly, the BulkEdit undo path would need to either:

1. **Store the edit list alongside the snapshot** and replay inverse marker adjustments on undo
2. **Snapshot markers too** — but this has proven problematic (see Atom's failed `maintainHistory`)
3. **Switch BulkEdit undo from snapshots to event replay** — more correct but harder to implement

---

## How Other Editors Handle This

Cross-editor research reveals three industry patterns:

### Pattern 1: "Decorations are ephemeral" (VSCode/Monaco, Helix, Kakoune)

Accept that undo doesn't restore decorations. Source systems (LSP, plugins) re-push them.

- **VSCode/Monaco**: Decorations live in an IntervalTree, adjusted by `acceptReplace` for all
  edits including undo. But decorations deleted/modified between edits are not restored. No
  built-in mechanism for decoration undo. [Monaco issue #4949](https://github.com/microsoft/monaco-editor/issues/4949)
  documents this as a known limitation.
- **Helix**: Undo preserves only text + selections. Marks, diagnostics, gutter indicators are
  external to the revision tree.
- **Kakoune**: `range-specs` break silently on undo — stale positions are simply wrong until
  updated.

**Fresh is currently in this camp.** The LSP refresh after undo resolves markers eventually.

### Pattern 2: "Record adjustments in undo entries" (Emacs, Neovim)

Store per-marker position data alongside undo entries.

- **Emacs**: Text properties are fully undoable via `(nil PROPERTY VALUE BEG . END)` undo list
  entries. Markers have `(marker . adjustment)` entries. But **overlays** (the main decoration
  mechanism) are explicitly NOT part of undo. This clean split means most visual decorations
  (flycheck, company, hl-line) get zero undo integration.
- **Neovim**: Extmarks have an opt-in `undo_restore` flag. When true, mark positions are saved
  to the undo header and restored. However: redo is still broken (issue #30331),
  `invalidate+undo_restore` has bugs (#29509), and deleted extmarks are never restored.
  Described by maintainers as a "typical Vim whack-a-mole game."

**This is the most promising direction for Fresh** — store the BulkEdit's edit list in the
event so the undo path can replay inverse marker adjustments.

### Pattern 3: "Composable effect inversion" (CodeMirror 6)

The most architecturally sophisticated approach. CM6's `invertedEffects` facet lets each
decoration consumer define inverse effects registered alongside the history module:

```typescript
const invertHighlight = invertedEffects.of(tr => {
  let found = []
  for (let e of tr.effects) {
    if (e.is(addHighlight)) found.push(removeHighlight.of(e.value))
    if (e.is(removeHighlight)) found.push(addHighlight.of(e.value))
  }
  return found
})
```

On undo, both text inversion and effect inversions are applied. Effects carry position data
mapped through document changes. This is opt-in, composable, and doesn't require snapshotting.

**This is a longer-term aspiration for Fresh** — a plugin hook system where overlay consumers
participate in undo — but it requires significant architectural work.

### Pattern 4: "Snapshot markers at checkpoints" (Atom — failed)

Atom's `MarkerLayer` had an experimental `maintainHistory` option that snapshotted marker
positions at undo checkpoints. It had significant edge-case bugs: markers destroyed between
checkpoints were not restorable by undo. The snippet system eventually moved to a "history
provider" callback pattern instead.

**This approach should be avoided.** The edge cases around markers created/destroyed between
checkpoints are what killed Atom's implementation.

### Summary Table

| Editor | Markers in undo? | Mechanism | Redo works? | Key limitation |
|---|---|---|---|---|
| **VSCode/Monaco** | No | N/A | N/A | No API to hook decoration undo |
| **Neovim** | Opt-in | Undo header | No (#30331) | Redo broken; invalidate+undo_restore buggy |
| **Emacs** | Text props yes, overlays no | Undo list entries | Yes (text props) | Overlays (most decorations) excluded |
| **CodeMirror 6** | Opt-in | `invertedEffects` | Yes | Must manually define inverse effects |
| **Helix** | No | N/A | N/A | No plugin hook for auxiliary state |
| **Kakoune** | No | N/A | N/A | Range-specs break silently |
| **Atom** | Opt-in (experimental) | Checkpoint snapshots | Partially | Failed; destroyed markers not restorable |
| **ProseMirror** | Doc marks yes, decorations no | Steps with `invert()` | Yes (marks) | No decoration undo mechanism |

---

## Recommendations for BulkEdit Undo/Redo

### Short-term: Store edit list in BulkEdit event (Pattern 2)

Add the sorted edit list to the `BulkEdit` event variant so the undo path can replay inverse
marker adjustments:

```rust
Event::BulkEdit {
    old_snapshot, new_snapshot,
    old_cursors, new_cursors,
    edits: Vec<(usize, usize, String)>,  // NEW: the edit tuples
}
```

In `state.rs` BulkEdit application, after `restore_buffer_state()`:

```rust
// Replay inverse marker adjustments (ascending order for undo = reverse of
// the descending order used in the forward path)
for (pos, del_len, text) in edits.iter().rev() {
    // Undo: inverse of insert is delete, inverse of delete is insert
    if !text.is_empty() {
        self.marker_list.adjust_for_delete(*pos, text.len());
        self.margins.adjust_for_delete(*pos, text.len());
    }
    if *del_len > 0 {
        self.marker_list.adjust_for_insert(*pos, *del_len);
        self.margins.adjust_for_insert(*pos, *del_len);
    }
}
```

| Pro | Con |
|-----|-----|
| Eliminates transient displacement after undo | Edit list stored twice (once as snapshot diff, once as tuples) |
| Consistent with single-edit undo behavior | Doesn't solve the "delete collapses markers" fundamental limitation |
| Minimal architecture change | Need to handle redo direction too (forward edits, not inverse) |

### Long-term: Effect inversion hooks (Pattern 3)

Design a system where overlay consumers (LSP diagnostics, search highlights, reference
highlights) can register inverse effect handlers, similar to CM6's `invertedEffects`. This
would let each subsystem participate in undo without the core needing to know about them.

This is significantly more architectural work and should only be pursued if the short-term
fix proves insufficient.

### What NOT to do

- **Don't snapshot the entire marker tree** — Atom tried this and failed. Markers created/destroyed
  between checkpoints produce irrecoverable edge cases.
- **Don't try to make undo "perfect"** — Even Neovim's opt-in system has redo bugs after years.
  The LSP refresh provides an eventual-consistency safety net that makes transient marker
  incorrectness tolerable.
