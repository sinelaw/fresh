# Undo/Redo and Markers: Comprehensive Analysis

## Overview

Fresh's marker system (IntervalTree-backed `MarkerList`) underpins three visual features:
- **Virtual text** (inlay hints, ghost text, inline diagnostics) — via `VirtualTextManager`
- **Overlays** (diagnostic underlines, search highlights, semantic tokens) — via `OverlayManager`, each overlay backed by start+end markers
- **Margins** (breakpoints, line annotations) — via `MarginManager`, using a separate `MarkerList`

All three rely on `adjust_for_insert`/`adjust_for_delete` to track byte positions through edits.
This document catalogs every marker-related undo/redo issue and recommends fixes.

---

## Issue Inventory

### Issue 1: BulkEdit forward path — FIXED

**Status**: Fixed in `abd52d7`.

`apply_events_as_bulk_edit` now calls `marker_list.adjust_for_insert/delete` and
`margins.adjust_for_insert/delete` after `apply_bulk_edits()` (`app/mod.rs:2568-2580`).

### Issue 2: BulkEdit undo/redo does not adjust markers

**Status**: Open. Severity: Low-medium (transient flicker until LSP refreshes).

**Mechanism**: BulkEdit undo restores the buffer via `restore_buffer_state(snapshot)` which
replaces the piece tree atomically (`state.rs:699-700`). No `adjust_for_insert/delete` calls
are made. Markers remain at their post-edit (or post-LSP-refresh) positions while the buffer
reverts.

**Sequence**:
```
1. Markers at [53, 68]
2. BulkEdit inserts "// "  → markers adjusted to [56, 71]     ✅
3. LSP responds            → clears markers, creates new at [56, 71]  ✅
4. Undo                    → buffer reverts, markers stay at [56, 71]  ❌
5. LSP responds again      → markers corrected to [53, 68]            ✅ (eventual)
```

The displacement between steps 4-5 is visible as a brief flash of misaligned hints.

**Why this is hard**: The snapshot approach bypasses the edit-by-edit path. There are no
individual inserts/deletes to trigger marker adjustment. The BulkEdit event
(`event.rs:205-218`) stores only `old_snapshot`, `new_snapshot`, and cursor states — not the
edit list.

### Issue 3: Delete collapses markers irreversibly (single-edit AND bulk-edit)

**Status**: Open. Severity: Low (fundamental limitation, shared by all editors).

**Mechanism**: When a range is deleted, all markers within that range collapse to the deletion
start (`marker_tree.rs` `adjust_for_edit` with negative delta). Undoing the delete (inserting
text back) shifts markers at the insertion point, but they all end up at one position — the
original spread within the deleted range is permanently lost.

```
1. Markers at [10, 12, 14] inside range [10, 15)
2. Delete [10, 15)  → all three collapse to position 10
3. Undo (Insert)    → markers shift to 15 (right affinity) or stay at 10 (left affinity)
   LOST: original positions [10, 12, 14] are gone
```

**Why this is fundamental**: The marker system stores positions, not "position within range"
offsets. Once collapsed, the relative spacing is lost. Every editor that uses position-based
markers (VSCode, Neovim, Emacs, Helix) has this same limitation. The only workaround is
snapshot-based restoration, which Atom tried and abandoned due to edge cases.

### Issue 4: Overlays are explicitly non-undoable

**Status**: By design. Severity: None (correct behavior).

`AddOverlay`, `RemoveOverlay`, and `ClearNamespace` events return `None` from `inverse()`
(`event.rs:420-431`). This means undo never re-creates or removes overlays. This is correct
because overlays are ephemeral decorations managed by external systems (LSP diagnostics,
search, semantic tokens) that re-push them on demand.

However, overlay **positions** are affected by issues 2 and 3 above — overlay markers
(start+end) are subject to the same displacement and collapse problems as virtual text markers.

### Issue 5: Virtual text markers are cleared and recreated by LSP

**Status**: By design. Severity: None (this is the safety net).

`apply_inlay_hints_to_state` (`lsp_requests.rs:720-780`) calls `virtual_texts.clear()` then
recreates all markers from scratch using fresh LSP positions. This happens after every edit
that triggers an LSP notification. This means:

- After any edit (including undo/redo), the LSP will eventually send fresh inlay hints
- All stale marker positions are wiped and replaced with correct ones
- The window of incorrectness is bounded by LSP response latency (typically <100ms)

This is the **eventual consistency** safety net that makes issues 2 and 3 tolerable.

### Issue 6: Margin markers during BulkEdit undo

**Status**: Open. Same root cause as Issue 2.

Margins (breakpoints, line annotations) use the same marker system. BulkEdit undo restores
the buffer but doesn't adjust margin marker positions. Unlike virtual text, margins are NOT
refreshed by the LSP — they persist until explicitly removed. This means margin displacement
after BulkEdit undo is **permanent** until the user manually re-adds the margin.

This is the **most impactful remaining issue** because there's no automatic recovery path.

---

## Cross-Editor Context

Research across 8 editors reveals three industry patterns:

### Pattern 1: "Ephemeral decorations" (VSCode/Monaco, Helix, Kakoune)

Accept that undo doesn't restore decorations. Source systems re-push them.
- VSCode: No decoration undo API. Extensions re-push via `deltaDecorations()`.
- Helix: Undo preserves only text + selections. Everything else is external.
- Kakoune: `range-specs` break silently on undo.

**Fresh is currently here** for virtual text (with the LSP safety net).

### Pattern 2: "Record in undo entries" (Emacs, Neovim)

Store per-marker data alongside undo entries.
- Emacs: Text properties undoable, markers have adjustment entries, overlays excluded.
- Neovim: Opt-in `undo_restore` on extmarks. Still has bugs: redo broken (#30331),
  invalidate+undo_restore buggy (#29509). Years of whack-a-mole.

**This is the pragmatic next step for Fresh** — specifically for BulkEdit.

### Pattern 3: "Composable effect inversion" (CodeMirror 6)

The gold standard. `invertedEffects` facet lets each decoration consumer register inverse
effects. History module stores and replays them alongside text inversions. Fully composable,
fully opt-in, no snapshotting.

**This is the long-term aspiration** if Fresh develops a richer plugin ecosystem.

---

## Recommendations

### Tier 1: Fix BulkEdit undo/redo marker adjustment (Medium effort, High value)

**Problem**: Issues 2 and 6.

**Approach**: Store the edit list in the `BulkEdit` event, replay inverse marker adjustments
during undo/redo.

**Changes to `Event::BulkEdit`** (`event.rs:205-218`):
```rust
BulkEdit {
    old_snapshot: Option<Arc<BufferSnapshot>>,
    new_snapshot: Option<Arc<BufferSnapshot>>,
    old_cursors: Vec<(CursorId, usize, Option<usize>)>,
    new_cursors: Vec<(CursorId, usize, Option<usize>)>,
    description: String,
    edits: Vec<(usize, usize, String)>,  // NEW: (position, delete_len, insert_text)
}
```

**Changes to `state.rs` BulkEdit application** (around line 700):
```rust
Event::BulkEdit { new_snapshot, new_cursors, edits, .. } => {
    if let Some(snapshot) = new_snapshot {
        self.buffer.restore_buffer_state(snapshot);
    }

    // Determine direction: are we undoing or redoing?
    // For undo: apply inverse of original edits (ascending order)
    // For redo: apply original edits (descending order, same as forward)
    //
    // Since inverse() swaps old/new snapshots, we can detect direction by
    // comparing against current buffer state. But simpler: store a direction
    // flag, or always store the correct edit list for this direction in
    // inverse().

    // Apply marker adjustments to match the restored buffer state
    for (pos, del_len, text) in edits.iter().rev() {
        if !text.is_empty() {
            self.marker_list.adjust_for_delete(*pos, text.len());
            self.margins.adjust_for_delete(*pos, text.len());
        }
        if *del_len > 0 {
            self.marker_list.adjust_for_insert(*pos, *del_len);
            self.margins.adjust_for_insert(*pos, *del_len);
        }
    }

    // Update cursors...
}
```

**Changes to `inverse()`** (`event.rs:443-458`): When swapping snapshots and cursors, also
invert the edit list. Each `(pos, del_len, text)` becomes `(pos, text.len(), deleted_text)`
— but we don't have the deleted text in the edit tuple. Two options:

- **Option A**: Store deleted text in the edit tuple (increases memory but is correct)
- **Option B**: Don't invert edits — instead store a `is_inverse: bool` flag and have the
  application code flip the adjustment direction

Option B is simpler:
```rust
BulkEdit {
    // ... existing fields ...
    edits: Vec<(usize, usize, String)>,
    is_undo: bool,  // false = forward edits, true = inverse edits
}
```

The `inverse()` method just flips `is_undo`. The application code in `state.rs` checks
`is_undo` to decide whether to apply edits forward (adjust for insert/delete) or backward
(adjust for inverse insert/delete).

**Trade-offs**:

| Pro | Con |
|-----|-----|
| Eliminates transient marker displacement after BulkEdit undo/redo | Edit list stored alongside snapshots (modest memory increase) |
| Fixes margin displacement permanently (no LSP safety net for margins) | Must handle direction correctly in application code |
| Consistent with single-edit undo behavior | Doesn't solve the "delete collapses markers" limitation (Issue 3) |
| No architectural change needed | |

**Why this is worth doing**: Margins have no automatic recovery path. A displaced breakpoint
after undo is a real user-facing bug, not a transient flicker.

---

### Tier 2: Accept fundamental marker collapse limitation (No effort)

**Problem**: Issue 3.

**Recommendation**: Do nothing. This is a fundamental limitation of position-based marker
tracking shared by every editor. The LSP safety net (Issue 5) handles virtual text. Overlays
are re-pushed by their source systems.

The only scenario where collapse is user-visible is:
1. User selects a multi-line range containing multiple markers
2. Deletes the range (markers collapse)
3. Immediately undoes (markers bunched at one end)
4. LSP hasn't responded yet

This is a <100ms window in typical usage. Not worth the complexity of snapshot-based marker
restoration (which Atom tried and abandoned).

---

### Tier 3: Accept overlays as non-undoable (No effort)

**Problem**: Issue 4.

**Recommendation**: Do nothing. The current design is correct. Overlays are ephemeral
decorations driven by external systems. Making them undoable would require:
- Tracking which overlays were added/removed between undo points
- Storing overlay state (face, priority, message, namespace) in the undo log
- Handling namespace clears across undo boundaries

This complexity is not justified. The external systems (LSP, search) already re-push overlays
when the buffer changes, including after undo.

---

### Tier 4: Long-term — Effect inversion hooks (Large effort, speculative value)

**Problem**: Future-proofing for a plugin ecosystem.

If Fresh develops plugins that create decorations with undo semantics (e.g., a code review
tool that lets users undo annotation placement), the current architecture has no hook for it.

**Approach**: Inspired by CodeMirror 6's `invertedEffects`, add a registration mechanism where
subsystems can provide inverse effects for their state changes:

```rust
// Conceptual API — not a concrete proposal yet
editor.register_undo_participant("lsp-diagnostics", |transaction| {
    // Given a transaction (set of events), return events to undo our state
    vec![Event::ClearNamespace { namespace: "diagnostics".into() }]
});
```

**Recommendation**: Don't build this now. Revisit if/when Fresh has a plugin API and plugins
request undo integration. The current ephemeral-overlay + LSP-refresh model handles all
existing use cases.

---

## Summary

| Issue | Severity | Recommendation | Effort |
|-------|----------|----------------|--------|
| BulkEdit forward markers | Fixed | — | Done |
| BulkEdit undo/redo markers | Medium (margins permanent, vtext transient) | Store edit list in BulkEdit event | Medium |
| Delete collapses markers | Low (fundamental, <100ms window) | Accept; LSP safety net sufficient | None |
| Overlays non-undoable | None (correct by design) | Accept | None |
| Virtual text LSP refresh | None (safety net) | Accept | None |
| Effect inversion hooks | Speculative | Defer until plugin API exists | None (for now) |

**Priority**: Tier 1 (BulkEdit undo marker adjustment) is the only action item. It has clear
value because margin displacement after undo has no automatic recovery. Everything else is
either already correct, has an adequate safety net, or is a fundamental limitation shared by
the entire industry.
