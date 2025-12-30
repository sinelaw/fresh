# Bulk Edit Optimization

## Summary

Batch operations (multi-cursor, toggle comment, indent, replace-all) previously used `Event::Batch` which applied edits sequentially, causing O(n²) performance.

The `BulkEdit` event type solves this by:
1. Applying all edits in a single tree traversal via `PieceTree::apply_bulk_edits`
2. Storing tree snapshot for O(1) undo (Arc clone, not content copy)
3. Tracking cursor positions before/after for proper undo/redo

## Performance

| Metric | Before (Batch) | After (BulkEdit) |
|--------|----------------|------------------|
| Tree traversals | O(N) | O(1) |
| Tree rebuilds | O(N) | O(1) |
| Memory for undo | O(N) event objects | O(1) Arc clone |
| Time complexity | O(N × pieces) | O(pieces + N) |

For 500 cursors: ~500x improvement.

## Migration Status

| Operation | Status | Location |
|-----------|--------|----------|
| Multi-cursor edits | ✅ Done | `src/app/mod.rs` |
| Replace All | ✅ Done | `src/app/render.rs` (ReplaceAll event removed) |
| Toggle Comment | ✅ Done | `src/app/render.rs` |
| Indent/Dedent | ✅ Done | `src/app/mod.rs` |
| LSP Rename | ✅ Done | `src/app/lsp_requests.rs` |
| Multi-cursor Paste | ✅ Done | `src/app/clipboard.rs` |

## Key Functions

- `PieceTree::apply_bulk_edits()` - Apply multiple edits in single traversal
- `Editor::apply_events_as_bulk_edit()` - Convert Insert/Delete events to BulkEdit
- `Event::BulkEdit` - Stores old_tree, old_cursors, new_cursors for undo/redo
