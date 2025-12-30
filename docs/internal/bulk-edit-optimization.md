# Bulk Edit Optimization Design

## Problem Statement

Several operations in the editor suffer from O(n²) performance when handling many edits:

| Operation | Trigger | Current Complexity |
|-----------|---------|-------------------|
| Multi-cursor edits | Typing with 100+ cursors | O(cursors × tree_size) |
| Toggle Comment | Ctrl+/ on 1000 lines | O(lines × tree_size) |
| Indent Selection | Tab on 1000 lines | O(lines × tree_size) |
| Dedent Selection | Shift+Tab on 1000 lines | O(lines × tree_size) |
| Replace All | 1500 replacements | O(matches × tree_size) ✅ Fixed |
| LSP Rename | Rename symbol with 100 refs | O(refs × tree_size) |

### Root Cause

Current implementation wraps individual `Event::Insert`/`Event::Delete` in `Event::Batch`:

```rust
for each_edit in edits {
    events.push(Event::Delete { ... });
    events.push(Event::Insert { ... });
}
let batch = Event::Batch { events, ... };
apply_event_to_active_buffer(&batch);  // Applies each event sequentially
```

Each event triggers:
1. `collect_leaves_with_split` - O(pieces)
2. `build_balanced` - O(pieces)
3. `check_and_rebalance` - O(pieces)

For N edits: O(N × pieces) = O(N²) as pieces grow with edits.

## Proposed Solution

### 1. New PieceTree Method: `apply_bulk_edits`

```rust
impl PieceTree {
    /// Apply multiple edits in a single tree traversal + rebuild
    ///
    /// # Arguments
    /// * `edits` - Vec of (position, delete_len, insert_text), sorted descending by position
    /// * `buffers` - Mutable reference to string buffers for new text
    ///
    /// # Complexity
    /// O(pieces + edits) instead of O(pieces × edits)
    pub fn apply_bulk_edits(
        &mut self,
        edits: &[(usize, usize, &str)],
        buffers: &mut Vec<StringBuffer>,
    ) {
        if edits.is_empty() {
            return;
        }

        // 1. Collect all split points
        let mut split_points: Vec<usize> = Vec::with_capacity(edits.len() * 2);
        for (pos, del_len, _) in edits {
            split_points.push(*pos);
            if *del_len > 0 {
                split_points.push(*pos + *del_len);
            }
        }
        split_points.sort_unstable();
        split_points.dedup();

        // 2. Single traversal: collect leaves split at all points
        let leaves = self.collect_leaves_with_splits(&split_points, buffers);

        // 3. Build new leaf list with edits applied
        let new_leaves = self.apply_edits_to_leaves(leaves, edits, buffers);

        // 4. Single balanced tree rebuild
        self.root = Self::build_balanced(&new_leaves);
        self.recompute_metadata();
    }

    /// Collect leaves, splitting at multiple points in one traversal
    fn collect_leaves_with_splits(
        &self,
        split_points: &[usize],
        buffers: &[StringBuffer],
    ) -> Vec<Piece> {
        // Single DFS traversal, split pieces at each point
        // Returns pieces with start/end offsets for easy filtering
        todo!()
    }

    /// Apply edits to collected leaves
    fn apply_edits_to_leaves(
        &self,
        leaves: Vec<Piece>,
        edits: &[(usize, usize, &str)],
        buffers: &mut Vec<StringBuffer>,
    ) -> Vec<Piece> {
        let mut result = Vec::new();
        let mut leaf_iter = leaves.into_iter().peekable();
        let mut current_offset = 0;

        for (pos, del_len, insert_text) in edits.iter() {
            // Copy leaves before this edit position
            while let Some(leaf) = leaf_iter.peek() {
                let leaf_end = current_offset + leaf.length;
                if leaf_end <= *pos {
                    result.push(leaf_iter.next().unwrap());
                    current_offset = leaf_end;
                } else {
                    break;
                }
            }

            // Skip leaves in deleted range
            let del_end = pos + del_len;
            while let Some(leaf) = leaf_iter.peek() {
                let leaf_end = current_offset + leaf.length;
                if current_offset < del_end {
                    leaf_iter.next(); // discard
                    current_offset = leaf_end;
                } else {
                    break;
                }
            }

            // Insert new piece for inserted text
            if !insert_text.is_empty() {
                let buffer_id = self.allocate_in_add_buffer(insert_text, buffers);
                result.push(Piece::new(buffer_id, 0, insert_text.len(), ...));
            }
        }

        // Copy remaining leaves
        result.extend(leaf_iter);
        result
    }
}
```

### 2. New Event Type: `BulkEdit`

```rust
/// Efficient bulk edit that stores tree snapshot for O(1) undo
BulkEdit {
    /// Previous tree state for undo (Arc clone = O(1))
    old_tree: PieceTree,
    /// Previous cursor states
    old_cursors: Vec<(CursorId, usize, Option<usize>)>,
    /// New cursor states after edit
    new_cursors: Vec<(CursorId, usize, Option<usize>)>,
    /// Human-readable description
    description: String,
}
```

**Key insight**: The piece tree uses `Arc<PieceTreeNode>` - it's a persistent data structure. Storing `old_tree` for undo is O(1) (Arc clone), not O(n) (content copy).

### 3. EditorState Apply Handler

```rust
Event::BulkEdit { old_tree, new_cursors, .. } => {
    // Tree already modified by apply_bulk_edits before event creation
    // Just update cursor positions
    for (cursor_id, position, anchor) in new_cursors {
        if let Some(cursor) = self.cursors.get_mut(*cursor_id) {
            cursor.position = *position;
            cursor.anchor = *anchor;
        }
    }
    self.highlighter.invalidate_all();
}
```

### 4. Inverse for Undo

```rust
Event::BulkEdit { old_tree, old_cursors, new_cursors, description } => {
    Some(Event::BulkEdit {
        old_tree: current_tree.clone(),  // Snapshot current for redo
        old_cursors: new_cursors.clone(),
        new_cursors: old_cursors.clone(),
        description: format!("Undo: {}", description),
    })
}
```

Undo application:
```rust
Event::BulkEdit { old_tree, new_cursors, .. } => {
    self.buffer.piece_tree = old_tree.clone();  // Restore old tree
    // Restore cursor positions...
}
```

## Unified Edit Collection

All batch operations reduce to `Vec<(position, delete_len, insert_text)>`:

```rust
fn collect_edits(operation: &Operation) -> Vec<(usize, usize, String)> {
    match operation {
        // Multi-cursor: insert same char at each position
        MultiCursorInsert { cursors, char } => {
            cursors.iter()
                .map(|c| (c.position, c.selection_len(), char.to_string()))
                .collect()
        }

        // Toggle comment: insert/delete at line starts
        ToggleComment { lines, adding } => {
            lines.iter()
                .map(|l| if adding {
                    (l.start, 0, "// ".to_string())
                } else {
                    (l.start, 3, String::new())
                })
                .collect()
        }

        // Indent: insert whitespace at line starts
        IndentLines { lines, indent } => {
            lines.iter()
                .map(|l| (l.start, 0, indent.clone()))
                .collect()
        }

        // Replace all now uses BulkEdit with Delete+Insert pairs
        // (Event::ReplaceAll has been removed from the codebase)
    }
}
```

## Integration Points

### Current Batch Path (to be replaced)

Location: `src/app/input.rs:2340-2348`

```rust
// BEFORE (O(n²))
if let Some(events) = self.action_to_events(action) {
    if events.len() > 1 {
        let batch = Event::Batch { events, description };
        self.apply_event_to_active_buffer(&batch);
    }
}

// AFTER (O(n))
if let Some(events) = self.action_to_events(action) {
    if events.len() > 1 && has_buffer_modifications(&events) {
        let bulk_edit = self.create_bulk_edit(events, description);
        self.active_event_log_mut().append(bulk_edit.clone());
        self.apply_event_to_active_buffer(&bulk_edit);
    }
}
```

### Operations to Migrate

1. **Multi-cursor edits** (`src/app/input.rs`)
   - `handle_insert_char_editor`
   - `Action::DeleteBackward`
   - `apply_action_as_events`

2. **Toggle Comment** (`src/app/render.rs:2612-2755`)
   - `toggle_comment()`

3. **Indent/Dedent** (`src/input/actions.rs`)
   - `Action::Tab` with selection (lines 917-959)
   - `Action::DedentSelection` (lines 779-843)

4. **LSP Rename** (`src/app/lsp_requests.rs:1170-1227`)
   - `apply_lsp_changes()`

5. **Replace All** - ✅ Migrated to `BulkEdit` (Event::ReplaceAll removed)

## Complexity Analysis

| Metric | Before (Batch) | After (BulkEdit) |
|--------|----------------|------------------|
| Tree traversals | O(N) | O(1) |
| Tree rebuilds | O(N) | O(1) |
| Memory for undo | O(N) event objects | O(1) Arc clone |
| Time complexity | O(N × pieces) | O(pieces + N) |

For 500 cursors on a 10,000 line file:
- Before: ~500 × 10,000 = 5,000,000 operations per keystroke
- After: ~10,000 + 500 = 10,500 operations per keystroke
- **~500x improvement**

## Implementation Plan

1. **Phase 1**: Implement `PieceTree::apply_bulk_edits`
   - Add `collect_leaves_with_splits` method
   - Add `apply_edits_to_leaves` method
   - Unit tests for bulk operations

2. **Phase 2**: Add `BulkEdit` event type
   - Add to `Event` enum
   - Implement `inverse()`
   - Implement `modifies_buffer()`, `is_write_action()`
   - Handle in `EditorState::apply()`

3. **Phase 3**: Migrate multi-cursor path
   - Create helper to convert events to edits
   - Replace Batch creation with BulkEdit
   - Integration tests

4. **Phase 4**: Migrate other operations
   - Toggle Comment
   - Indent/Dedent
   - LSP Rename
   - ✅ ReplaceAll migrated and removed

## Testing Strategy

1. **Unit tests** for `apply_bulk_edits`:
   - Empty edits
   - Single edit
   - Multiple non-overlapping edits
   - Adjacent edits
   - Edits at document boundaries

2. **Property tests**:
   - Bulk edit result matches sequential apply
   - Undo restores exact previous state
   - Cursor positions correctly adjusted

3. **Performance benchmarks**:
   - 100, 500, 1000 cursors typing
   - Compare before/after timing

4. **E2E tests**:
   - Multi-cursor editing on large files
   - Toggle comment on 1000+ lines
   - Replace all with 1000+ matches
