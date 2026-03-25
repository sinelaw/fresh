# Implementation Plan: BulkEdit Undo/Redo Marker Handling

## Strategy

Two-pronged approach per subsystem:

| Subsystem | On BulkEdit undo/redo | Recovery |
|-----------|----------------------|----------|
| Virtual text (inlay hints) | Clear all | LSP re-pushes ~100ms |
| LSP overlays (diagnostics) | Clear `lsp-diagnostic` namespace | LSP re-pushes |
| Reference highlights | Clear `reference-highlight` namespace | Re-computed on cursor move |
| Bracket highlights | Clear `bracket-highlight` namespace | Re-computed on cursor move |
| Search overlays | Leave alone | Search re-runs on buffer change |
| Margins (breakpoints, indicators) | Replay marker adjustments from stored edit list | Permanent fix, no external recovery |

### Key invariant: redo = forward

Both `state.apply(BulkEdit)` during redo and the forward path use the **same marker
adjustment logic**. This works because:

- **Forward path**: `apply_events_as_bulk_edit` applies edits + adjusts markers, then stores
  the edit list `[(pos, del_len, ins_len)]` in the BulkEdit event. The event is logged but
  never applied via `state.apply()`.
- **Redo**: EventLog replays the **original** event. `state.apply(BulkEdit)` restores the
  snapshot and replays the **same** edit list → same marker adjustments as forward.
- **Undo**: EventLog calls `inverse()` which swaps snapshots AND swaps `del_len`/`ins_len`
  in each edit tuple. `state.apply(BulkEdit)` restores the old snapshot and replays the
  **inverted** edit list → correct reverse marker adjustments.

The adjustment code in `state.apply(BulkEdit)` is identical for undo and redo — it just
processes whatever edit list the event carries. The `inverse()` transform ensures the list
describes the correct A→B transformation for that direction.

## Changes

### Step 1: Add edit list to `BulkEdit` event

**File**: `crates/fresh-editor/src/model/event.rs` (lines 205-218)

Add an `edits` field storing only lengths (not text — we only need lengths for
`adjust_for_insert`/`adjust_for_delete`):

```rust
BulkEdit {
    old_snapshot: Option<Arc<BufferSnapshot>>,
    new_snapshot: Option<Arc<BufferSnapshot>>,
    old_cursors: Vec<(CursorId, usize, Option<usize>)>,
    new_cursors: Vec<(CursorId, usize, Option<usize>)>,
    description: String,
    /// Edit operations as (position, delete_len, insert_len), sorted descending by position.
    /// Used to replay marker adjustments on undo/redo.
    /// - On redo: replayed as-is (same adjustments as the forward path)
    /// - On undo: inverse() swaps del_len/ins_len (reverse adjustments)
    edits: Vec<(usize, usize, usize)>,
}
```

### Step 2: Store edits when creating BulkEdit

**File**: `crates/fresh-editor/src/app/mod.rs` (around line 2580)

The edit list is already computed at line 2542-2554. Convert to lengths when constructing
the BulkEdit event:

```rust
let edit_lengths: Vec<(usize, usize, usize)> = edits
    .iter()
    .map(|(pos, del_len, text)| (*pos, *del_len, text.len()))
    .collect();

let bulk_event = Event::BulkEdit {
    old_snapshot: Some(Arc::new(old_snapshot)),
    new_snapshot: Some(Arc::new(new_snapshot)),
    old_cursors,
    new_cursors,
    description,
    edits: edit_lengths,
};
```

### Step 3: Invert the edit list in `inverse()`

**File**: `crates/fresh-editor/src/model/event.rs` (lines 443-458)

`inverse()` swaps `del_len` and `ins_len` so undo replays the reverse adjustments:

```rust
Self::BulkEdit {
    old_snapshot, new_snapshot, old_cursors, new_cursors, description, edits,
} => {
    // Invert edits: swap delete_len and insert_len
    // This makes undo apply the reverse marker adjustments
    let inverted_edits: Vec<(usize, usize, usize)> = edits
        .iter()
        .map(|(pos, del_len, ins_len)| (*pos, *ins_len, *del_len))
        .collect();

    Some(Self::BulkEdit {
        old_snapshot: new_snapshot.clone(),
        new_snapshot: old_snapshot.clone(),
        old_cursors: new_cursors.clone(),
        new_cursors: old_cursors.clone(),
        description: format!("Undo: {}", description),
        edits: inverted_edits,
    })
}
```

### Step 4: Apply marker adjustments + clear ephemeral decorations in BulkEdit handler

**File**: `crates/fresh-editor/src/state.rs` (lines 688-721)

This code runs for both undo and redo. The edit list already carries the correct
adjustments for the direction (original for redo, inverted for undo):

```rust
Event::BulkEdit {
    new_snapshot,
    new_cursors,
    edits,
    ..
} => {
    // Restore buffer state
    if let Some(snapshot) = new_snapshot {
        self.buffer.restore_buffer_state(snapshot);
    }

    // Replay marker adjustments from the edit list.
    // For redo: same adjustments as the forward path.
    // For undo: inverse() has swapped del/ins, so adjustments are reversed.
    // Edits are in descending position order — process as-is so later
    // positions are adjusted first (no cascading shift errors).
    for &(pos, del_len, ins_len) in edits {
        if del_len > 0 {
            self.margins.adjust_for_delete(pos, del_len);
        }
        if ins_len > 0 {
            self.margins.adjust_for_insert(pos, ins_len);
        }
    }

    // Clear ephemeral decorations — their source systems will re-push correct positions.
    // Virtual text (inlay hints): LSP re-sends after edit notification
    self.virtual_texts.clear(&mut self.marker_list);

    // LSP-managed overlays: LSP re-sends diagnostics after edit notification
    self.overlays.clear_namespace(
        &OverlayNamespace::from_string("lsp-diagnostic".to_string()),
        &mut self.marker_list,
    );

    // Cursor-position-dependent overlays: re-computed on next cursor event
    self.overlays.clear_namespace(
        &OverlayNamespace::from_string("reference-highlight".to_string()),
        &mut self.marker_list,
    );
    self.overlays.clear_namespace(
        &OverlayNamespace::from_string("bracket-highlight".to_string()),
        &mut self.marker_list,
    );

    // Update cursor positions
    for (cursor_id, position, anchor) in new_cursors {
        if let Some(cursor) = cursors.get_mut(*cursor_id) {
            cursor.position = *position;
            cursor.anchor = *anchor;
        }
    }

    // Invalidate highlight cache for entire buffer
    self.highlighter.invalidate_all();

    // Update primary cursor line number
    let primary_pos = cursors.primary().position;
    self.primary_cursor_line_number =
        match self.buffer.offset_to_position(primary_pos) {
            Some(pos) => crate::model::buffer::LineNumber::Absolute(pos.line),
            None => crate::model::buffer::LineNumber::Absolute(0),
        };
}
```

Note: the forward path never hits this code. `apply_events_as_bulk_edit` applies edits
directly to the buffer and adjusts markers itself. The BulkEdit event is only logged,
not applied via `state.apply()`. So `state.apply(BulkEdit)` only runs during undo/redo —
no guard needed to skip clearing on first application.

### Step 5: Update all BulkEdit construction sites

Search for all places that construct `Event::BulkEdit { ... }` and add the `edits` field.
Only `apply_events_as_bulk_edit` has the real edit list. All others pass `edits: vec![]`.

**Files to update** (search for `Event::BulkEdit`):
- `crates/fresh-editor/src/app/mod.rs` — main construction site (real edit list)
- `crates/fresh-editor/src/model/event.rs` — `inverse()` (already covered in Step 3)
- Any test files constructing BulkEdit events (`edits: vec![]`)

Passing `edits: vec![]` is safe — it means no marker adjustment on undo/redo, same as today.

## Files Changed (Summary)

| File | Change |
|------|--------|
| `crates/fresh-editor/src/model/event.rs` | Add `edits: Vec<(usize, usize, usize)>` to BulkEdit; update `inverse()` to swap del/ins |
| `crates/fresh-editor/src/state.rs` | In BulkEdit handler: adjust margins from edit list, clear ephemeral decorations |
| `crates/fresh-editor/src/app/mod.rs` | Store edit lengths in BulkEdit event construction |
| Test files constructing BulkEdit | Add `edits: vec![]` field |
| `crates/fresh-editor/tests/e2e/undo_redo_roundtrip.rs` | **New**: comprehensive forward/undo/redo roundtrip tests |

## Testing: Forward/Undo/Redo Roundtrip Verification

### Core properties

For every buffer-modifying operation, two things must hold:

**Property 1: Buffer content roundtrip**
```
original_content = buffer.to_string()
apply(operation)
post_edit_content = buffer.to_string()
apply(undo)
assert buffer.to_string() == original_content    // undo restores original
apply(redo)
assert buffer.to_string() == post_edit_content    // redo matches forward result
```

**Property 2: Marker positions roundtrip**
```
original_marker_positions = snapshot_marker_positions()
apply(operation)
post_edit_marker_positions = snapshot_marker_positions()
apply(undo)
assert snapshot_marker_positions() == original_marker_positions   // markers restored
apply(redo)
assert snapshot_marker_positions() == post_edit_marker_positions  // markers match forward
```

Property 2 applies to margins (which must survive undo/redo exactly). For virtual text and
LSP overlays, the property is weaker: after undo/redo they should be **cleared** (not stale),
then the LSP re-pushes them. So the virtual text assertion after undo/redo is:
```
assert virtual_texts.is_empty()   // cleared, not stale at wrong positions
```

### Helper: `snapshot_marker_positions`

Each test that validates markers will use a helper to capture marker state:

```rust
/// Snapshot all marker-based state for comparison.
/// Returns (margin_indicator_positions, virtual_text_positions, overlay_count).
fn snapshot_markers(state: &EditorState) -> MarkerSnapshot {
    let buf_len = state.buffer.len();

    // Margin indicator positions: Vec<(byte_offset, namespace)>
    let margin_positions: Vec<(usize, String)> = state.margins
        .line_indicators.iter()
        .flat_map(|(&marker_id, indicators)| {
            let pos = state.margins.indicator_markers
                .get_position(MarkerId(marker_id))
                .unwrap_or(0);
            indicators.keys().map(move |ns| (pos, ns.clone()))
        })
        .collect();

    // Virtual text positions: Vec<usize>
    let vtext_positions: Vec<usize> = state.virtual_texts
        .query_range(&state.marker_list, 0, buf_len.max(1))
        .into_iter()
        .map(|(pos, _)| pos)
        .collect();

    // Overlay positions: Vec<(start, end, namespace)>
    // Read via overlays that have namespace set
    let overlay_count = state.overlays.len();

    MarkerSnapshot { margin_positions, vtext_positions, overlay_count }
}
```

### Test level: Integration vs E2E tradeoffs

#### Option A: Integration tests (`EditorState + EventLog + Cursors` directly)

**Pros:**
- Fast (~ms per test), no terminal simulation overhead
- Deterministic — no keybinding mapping, no command palette fuzzy matching
- Tests exactly the code path that matters (`state.apply()` + `EventLog`)
- Easy to construct precise BulkEdit events with known edit lists

**Cons:**
- Must manually construct BulkEdit events (snapshot before/after, compute edit list)
- Doesn't exercise `apply_events_as_bulk_edit` which is the real forward path
- If `apply_events_as_bulk_edit` changes how it constructs BulkEdit events, the
  integration test's manual construction might diverge from reality
- Doesn't test toggle-comment, indent, move-line — the actual user operations

#### Option B: E2E tests (`EditorTestHarness` with `send_key`)

**Pros:**
- Exercises the full pipeline: keybinding → action → `apply_events_as_bulk_edit` →
  event log → undo/redo → `state.apply()`
- Tests real operations (toggle comment, indent, move line) not synthetic BulkEdits
- If the BulkEdit event construction changes, tests still work because they go through
  the real path
- More confidence that the fix works for actual user workflows

**Cons:**
- Slower (~100ms+ per test due to harness setup, rendering)
- Requires file-backed buffers with correct extensions for language detection
- More brittle — depends on keybindings, command palette, language configs
- Harder to isolate: if a test fails, is it the marker logic or the toggle-comment logic?

#### Option C: Both (recommended)

Split tests into two groups:

1. **Integration tests** for the core mechanism:
   - `Event::BulkEdit` inverse correctly swaps edit lengths
   - `state.apply(BulkEdit)` with edit list adjusts margins correctly
   - `state.apply(BulkEdit)` clears virtual text
   - Single-edit (Insert/Delete) marker roundtrip through undo/redo
   - These are fast, precise, and test the new code directly.

2. **E2E tests** for real operations:
   - Toggle comment + undo/redo with markers
   - Indent/dedent + undo/redo with markers
   - Move line up/down + undo/redo with markers
   - Interleaved single-edit and BulkEdit chains
   - These give confidence that the full pipeline works end-to-end.

This is the approach used below. Integration tests (Tests 1-10) use direct `EditorState`
manipulation. E2E tests (Tests 11-13) use `EditorTestHarness` for realistic operations.

### Integration tests: `crates/fresh-editor/tests/integration_tests.rs`

Use `EditorState + EventLog + Cursors` directly. For BulkEdit: manually construct the
scenario by applying edits to the buffer directly, snapshotting before and after, then
constructing an `Event::BulkEdit` with the correct edit list.

### E2E tests: `crates/fresh-editor/tests/e2e/undo_redo_roundtrip.rs`

Use `EditorTestHarness` with `send_key` for realistic BulkEdit operations (toggle comment,
indent). Add markers via `harness.editor_mut().active_state_mut()` before the operations.
Verify marker state via `harness.editor().active_state()` after undo/redo.

---

#### Tests 1-5: Single-edit operations (Insert/Delete)

These verify Property 1 + 2 for single edits. Undo uses `event.inverse()` which produces
Insert↔Delete, so markers adjust naturally via `apply_insert`/`apply_delete`.

All tests use `EditorState::new()` + `Cursors::new()` + `EventLog::new()` directly.
Markers are added via `state.virtual_texts.add()` and `state.margins.set_line_indicator()`.

**Test 1: Insert after markers — forward/undo/redo**
```rust
// Setup: buffer = "hello", vtext at 3, margin at 0
// Insert 'X' at end (pos 5)
let event = Event::Insert { position: 5, text: "X".into(), cursor_id };
log.append(event.clone());
state.apply(&mut cursors, &event);
assert_eq!(buffer, "helloX");
assert_eq!(vtext_pos, 3);  // unchanged (insert after)
assert_eq!(margin_pos, 0); // unchanged

// Undo
for e in log.undo() { state.apply(&mut cursors, &e); }
assert_eq!(buffer, "hello");
assert_eq!(vtext_pos, 3);
assert_eq!(margin_pos, 0);

// Redo
for e in log.redo() { state.apply(&mut cursors, &e); }
assert_eq!(buffer, "helloX");
assert_eq!(vtext_pos, 3);
assert_eq!(margin_pos, 0);
```

**Test 2: Insert before markers — forward/undo/redo**
```
Setup: buffer = "hello", vtext at 3, margin at 2
Insert "XX" at position 1 → "hXXello"
Verify: vtext at 5, margin at 4 (both shifted by 2)
Undo → "hello", vtext at 3, margin at 2
Redo → "hXXello", vtext at 5, margin at 4
```

**Test 3: Delete after markers — forward/undo/redo**
```
Setup: buffer = "hello", vtext at 1, margin at 0
Delete range 3..5 → "hel"
Verify: vtext at 1, margin at 0 (unchanged, delete after)
Undo → "hello", vtext at 1, margin at 0
Redo → "hel", vtext at 1, margin at 0
```

**Test 4: Delete before markers — forward/undo/redo**
```
Setup: buffer = "hello world", vtext at 8, margin at 6
Delete range 0..6 → "world"
Verify: vtext at 2, margin at 0 (both shifted back by 6)
Undo → "hello world", vtext at 8, margin at 6
Redo → "world", vtext at 2, margin at 0
```

**Test 5: Newline insert before margin — forward/undo/redo**
```
Setup: buffer = "hello", margin at 0
Insert "\n" at position 3 → "hel\nlo"
Verify: margin still at 0
Undo → "hello", margin at 0
Redo → "hel\nlo", margin at 0
```

---

#### Tests 6-10: BulkEdit operations

These verify both Property 1 AND Property 2. BulkEdit undo/redo uses snapshot restore +
edit list replay for margins, and clearing for virtual text.

To construct BulkEdit events at the integration level:
1. Set up initial buffer content via Insert events
2. Add markers (virtual text + margins)
3. Snapshot buffer state (old_snapshot)
4. Apply individual edits via `buffer.apply_bulk_edits()` + `marker_list.adjust_for_*`
5. Snapshot again (new_snapshot)
6. Construct `Event::BulkEdit { old_snapshot, new_snapshot, edits, ... }` and log it
7. Then test undo/redo via `log.undo()`/`log.redo()` → `state.apply()`

**Test 6: BulkEdit insert-only (simulated comment prefix) + margins — forward/undo/redo**
```
Setup: buffer = "aaa\nbbb\nccc", margin at byte 0 (line 1), margin at byte 4 (line 2)
BulkEdit: insert "// " at positions [8, 4, 0] (descending order)
  → "// aaa\n// bbb\n// ccc"
  edits = [(8, 0, 3), (4, 0, 3), (0, 0, 3)]
Verify: margins shifted to [0→3, 4→10] (each shifted by cumulative "// " insertions)
Undo → "aaa\nbbb\nccc", margins back at [0, 4]
Redo → "// aaa\n// bbb\n// ccc", margins at [3, 10]
```

**Test 7: BulkEdit delete-only (simulated uncomment) + margins — forward/undo/redo**
```
Setup: buffer = "// aaa\n// bbb\n// ccc", margin at byte 3, margin at byte 10
BulkEdit: delete 3 bytes at positions [8, 4, 0] (descending order)
  → "aaa\nbbb\nccc"
  edits = [(8, 3, 0), (4, 3, 0), (0, 3, 0)]
Verify: margins shifted to [3→0, 10→4]
Undo → "// aaa\n// bbb\n// ccc", margins at [3, 10]
Redo → "aaa\nbbb\nccc", margins at [0, 4]
```

**Test 8: BulkEdit clears virtual text — forward/undo/redo**
```
Setup: buffer = "aaa\nbbb", add virtual text at pos 2
BulkEdit: insert "// " at positions [4, 0]
  → "// aaa\n// bbb"
Undo → verify virtual_texts.is_empty() (cleared, not stale at wrong position)
Redo → verify virtual_texts.is_empty() (cleared again)
```

**Test 9: BulkEdit insert+delete (simulated indent replace) + margins**
```
Setup: buffer = "\taaa\n\tbbb", margin at byte 1 (after tab on line 1)
BulkEdit: delete 1 byte + insert 4 bytes at positions [5, 0] (replace tab with spaces)
  → "    aaa\n    bbb"
  edits = [(5, 1, 4), (0, 1, 4)]
Verify: margin shifted from 1 to 4 (tab replaced by 4 spaces)
Undo → "\taaa\n\tbbb", margin back at 1
Redo → "    aaa\n    bbb", margin at 4
```

**Test 10: BulkEdit with no markers — baseline content roundtrip**
```
Setup: buffer = "line1\nline2\nline3", no markers
BulkEdit: insert "// " at each line start
Undo → verify original content
Redo → verify commented content
(Baseline: confirms BulkEdit undo/redo works for content even without marker changes)
```

---

#### Tests 11-13: E2E tests — real operations via `EditorTestHarness`

These use `send_key` for realistic BulkEdit operations, testing the full pipeline.
Markers are added via `harness.editor_mut().active_state_mut()`.

**Test 11: Toggle comment + undo/redo with margin indicators (E2E)**
```
1. Open .rs file with "fn main() {\n    return;\n}"
2. Add margin indicator at byte 0 and byte 14 (start of "return" line)
3. Select all (Ctrl+A)
4. Run "Toggle Comment" via command palette
5. Verify: content is commented, margins shifted by "// " (3 bytes per line)
6. Ctrl+Z (undo) → verify original content + original margin positions
7. Ctrl+Y (redo) → verify commented content + shifted margin positions
```

**Test 12: Tab indent + undo/redo with margin indicators (E2E)**
```
1. Open .py file with "line1\nline2\nline3"
2. Add margin indicator at byte 6 (start of "line2")
3. Select lines 2-3 (Down, Shift+Down, Shift+Down)
4. Press Tab to indent
5. Verify: margin shifted by 4 spaces
6. Ctrl+Z → original content + original margin position
7. Ctrl+Y → indented content + shifted margin position
```

**Test 13: Toggle comment clears virtual text on undo/redo (E2E)**
```
1. Open .rs file with "fn main() {}"
2. Add virtual text at pos 3 (simulating inlay hint)
3. Run "Toggle Comment"
4. Ctrl+Z → verify virtual_texts.is_empty() (cleared, not at stale position)
5. Ctrl+Y → verify virtual_texts.is_empty() (cleared again)
```

---

### Implementation notes

- Each test follows the pattern: set up content + markers → capture original state →
  apply operation → capture post-edit state → undo → assert == original → redo →
  assert == post-edit.
- **Setup**: `EditorState::new(80, 24, LARGE_FILE_THRESHOLD, test_fs())` + `Cursors::new()`
  + `EventLog::new()`. Insert initial content via `Event::Insert`, applied + logged.
- **Buffer content**: `state.buffer.to_string().unwrap()`.
- **Margin positions**: `state.margins.indicator_markers.get_position(marker_id)`.
  Store the `MarkerId` returned by `state.margins.set_line_indicator()`.
- **Virtual text state**: `state.virtual_texts.query_range(&state.marker_list, 0, buf_len)`
  for positions, or `state.virtual_texts.is_empty()` for cleared check.
- **Adding test markers**: `state.virtual_texts.add(&mut state.marker_list, pos, ...)` and
  `state.margins.set_line_indicator(byte_offset, namespace, indicator)`.
- **Undo/redo**: `log.undo()` / `log.redo()` → iterate returned events and apply each via
  `state.apply(&mut cursors, &event)`. Same pattern as existing `test_undo_redo_cursor_positions`.
- **BulkEdit construction**: Manually snapshot before/after, compute edit lengths, construct
  `Event::BulkEdit { ... }` and `log.append()` it. Don't call `state.apply()` for the
  forward path (the edits were already applied directly). This mirrors what
  `apply_events_as_bulk_edit` does.
- **No file I/O needed**: Integration tests work with in-memory buffers. No temp files,
  no language detection, no command palette.

## Risks

- **Memory**: Storing `Vec<(usize, usize, usize)>` per BulkEdit is negligible (24 bytes per
  edit, typically <100 edits).
- **Correctness of edit inversion**: The `inverse()` swap is straightforward since we only
  store lengths. The position field stays the same because edits are in descending order and
  we restore the full buffer snapshot (positions don't need recalculation).
- **Namespace strings**: Hardcoded `"lsp-diagnostic"`, `"reference-highlight"`,
  `"bracket-highlight"`. If new namespaces are added, they'll need to be added here too.
  Consider adding an `is_ephemeral()` method to `OverlayNamespace` or using constants.
