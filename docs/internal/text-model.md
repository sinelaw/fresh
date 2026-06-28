# Text Model Architecture

Purpose: explain how Fresh stores, edits, indexes, snapshots, anchors, and persists text — the `model/` layer under `crates/fresh-editor/src/model/` — and the decisions, trade-offs, and concessions behind each part. Code is ground truth; doc/code discrepancies are flagged inline. All `path:line` refs are relative to `crates/fresh-editor/src/`.

---

## 1. Layer map

Three "buffer" concepts are easy to conflate; they are distinct:

| Type | File | Role | Owns text? |
|---|---|---|---|
| `PieceTree` | `model/piece_tree.rs:784` | persistent (path-copying) piece-table tree | structure only; bytes live in `StringBuffer`s |
| `TextBuffer` (alias `Buffer`) | `model/buffer/mod.rs:154`, alias `:3226` | the document store: piece tree + buffer pool + format/persistence metadata | yes |
| `CompositeBuffer` | `model/composite_buffer.rs:13` | synthesized multi-source view (diff/merge/side-by-side) | no — references `TextBuffer`s by id |

`DocumentModel` (`model/document_model.rs:177`) is a **trait**, the editor-facing abstraction over a buffer; it is implemented by `EditorState` (`state.rs:1453`), not by `TextBuffer`. (The module diagram at `document_model.rs:26-32` shows `TextBuffer` as the implementer; the prose at `:25` correctly says `EditorState` — the diagram is slightly misleading.)

Cursors and markers are **not** owned by `TextBuffer`. It owns only `piece_tree`, the `buffers` pool, and `persistence`/`file_kind`/`format`/`version`/`config` (`buffer/mod.rs:154`). Cursors live in `model/cursor.rs`; markers in `model/marker.rs` + `marker_tree.rs`, held by editor/view state.

---

## 2. The piece tree (piece table)

### 2.1 Why a piece table, not a rope / gap buffer / array

A piece table never mutates the original file bytes. The document is a sequence of *pieces*, each a `(buffer, offset, length)` slice into one of two append-only byte pools:

- **Stored** content — the original file (or, in large-file mode, an *unloaded* reference to a byte range on disk).
- **Added** content — everything the user has typed since load, appended to a growing buffer; existing bytes are never moved.

`BufferLocation` (`piece_tree.rs:274`) tags each piece `Stored(id)` or `Added(id)`. This buys three properties an editor specifically wants:

1. **Cheap O(1) snapshots for undo/redo.** Because edited bytes are never overwritten — they are split off into new pieces — a snapshot is just a reference to the current tree root (see §4 and §5). A rope can also share structure, but the piece table's "original buffer is immutable" invariant additionally makes the **save diff** (§7) and **structural diff** (`piece_tree_diff.rs`) trivial: unchanged regions are literally the same `Arc` nodes.
2. **Huge files with negligible memory.** A `Stored` piece can point at a byte range that is *not in memory* (`BufferData::Unloaded`, §3). The whole multi-GB file is one piece referencing the file on disk; bytes are pulled in lazily per viewport. A rope or gap buffer would have to materialize the entire file.
3. **Negligible per-edit cost.** Inserts/deletes split at most one leaf and append to the Added buffer — no large memmove (gap buffer's weakness on multi-cursor scatter) and no array shift.

The trade-off vs a rope: a piece table degrades into many small pieces under pathological edit patterns, and line/column lookup is not naturally O(log n) unless line counts are tracked per node. Fresh addresses both (§2.4 line tracking, §2.5 rebalancing).

### 2.2 Node shape

`PieceTreeNode` (`piece_tree.rs:293`) is a binary tree:

```rust
enum PieceTreeNode {
    Internal { left_bytes: usize, lf_left: Option<usize>,
               left: Arc<PieceTreeNode>, right: Arc<PieceTreeNode> },
    Leaf     { location: BufferLocation, offset: usize, bytes: usize,
               line_feed_cnt: Option<usize> },
}
```

`PieceTree` (`piece_tree.rs:784`) is just `{ root: Arc<PieceTreeNode>, total_bytes: usize }` and derives `Clone` — so cloning a tree is one `Arc::clone` of the root. Children are `Arc<PieceTreeNode>`, making the tree a **persistent (immutable) data structure**: every edit produces a new root that shares all untouched subtrees with the old root.

Internal nodes carry **augmented order statistics**: `left_bytes` (total bytes in the left subtree) drives O(log n) offset→piece lookup (`find_by_offset`, `:379`); `lf_left` (line feeds in the left subtree) drives O(log n) line↔offset lookup (`find_byte_offset_for_line`, `:613`). Both are summary fields recomputed on the path-copy path so they stay consistent.

### 2.3 Edits: path-copying insert/delete

Insert (`PieceTree::insert`, `:1212`) builds an `insert_leaf` and calls `path_copy_insert` (`:869`): it walks root→target leaf, splits only the leaf at the insertion point, and `Arc::clone`s every sibling subtree along the way (`:900`, `:914`). Delete (`:1457` → `path_copy_delete`, `:992`) is symmetric, trimming or dropping the affected leaves. Newline counts for the new sub-pieces are computed by scanning the underlying buffer bytes (`compute_line_feeds_static`, `:1378`) — or left `None` when the buffer is unloaded.

Because only the root-to-leaf path is cloned, an edit is **O(log n) new nodes**; the old root remains valid and untouched. That is the property that makes both undo snapshots (§5) and the structural diff (§7) cheap.

### 2.4 Line indexing (two-tier)

Line/column navigation needs newline counts. Fresh tracks them at two levels:

- **Per leaf:** `line_feed_cnt: Option<usize>`, summed into `lf_left` on internal nodes. `None` means "not yet scanned" — line-based navigation is unavailable for that subtree and methods like `find_byte_offset_for_line` bail to `None` (`:629`).
- **Per `StringBuffer`:** `line_starts: Option<Vec<usize>>` (`piece_tree.rs:18`). `Some` for small/loaded buffers (computed by `compute_line_starts`, `:224`); **`None` = large-file mode**, where building a full line index would defeat the negligible-memory goal.

This `Option` is the central large-file concession: with no line index, the editor works in byte-offset space (see `DocumentPosition::ByteOffset`, `document_model.rs:100`) and fills in `line_feed_cnt` incrementally as chunks are scanned (`update_leaf_line_feeds`, `:1527`; path-copy variant `update_leaf_line_feeds_path_copy`, `:1547`, which preserves `Arc::ptr_eq` for unscanned subtrees).

### 2.5 Balancing: rebuild, not red-black

Fresh does **not** use a self-balancing rotation tree (no red-black / AVL in the piece tree — that lives in `marker_tree.rs`, §6). Instead, after each insert it calls `check_and_rebalance` (`:1176`): if `depth > 2 * ceil(log2(leaf_count))` it collapses the whole tree to a flat leaf list and rebuilds a perfectly balanced tree via `build_balanced` (`:831`, recursive midpoint split), in `rebalance` (`:1169`).

Trade-off: a full O(n) rebuild is asymptotically worse per-trigger than O(log n) rotations, but (a) it is dramatically simpler and avoids the subtle parent-pointer/rotation bugs an RB-tree invites, (b) it triggers rarely (only when depth roughly doubles past optimal), so amortized cost stays low, and (c) the rebuilt tree is *optimally* balanced rather than merely within an RB factor. The concession is accepted deliberately: editor edit rates are low relative to what a rebuild costs, and the code-simplicity win is large. `build_balanced` is also reused as the construction primitive for `from_leaves` (`:822`), chunk splitting (§3), and pristine-root rebuild (§7).

### 2.6 Iteration & range reads

`PieceRangeIter` (`piece_tree.rs:2094`) walks pieces for a byte range; `count_lines_in_byte_range` (`:556`) and `offset_to_position` (`:1601`) provide line math. Range reads that may touch unloaded bytes go through `TextBuffer::get_text_range_mut` (`buffer/mod.rs:1250`), which lazy-loads (§3) — the low-level `StringBuffer::get_data` is `pub(crate)` precisely to force callers through that lazy path (`piece_tree.rs:101`).

---

## 3. Lazy loading for multi-GB files

Backs the README claim of "multi-gigabyte files with negligible memory overhead" (`README.md:15`, linking the *how Fresh loads huge files fast* blog post).

### 3.1 Load strategy

`load_from_file_internal` (`buffer/mod.rs:411`) branches on size against `DEFAULT_LARGE_FILE_THRESHOLD = 100 MB` (`:86`):

- **< threshold** → `load_small_file` (`:459`): read fully, detect encoding/binary, compute line index (`line_starts: Some`).
- **>= threshold** → `load_large_file_internal` (`:551`): for **UTF-8/ASCII** files, create one `StringBuffer` with `BufferData::Unloaded { file_path, file_offset: 0, bytes: file_size }` (`:614`) and a single-leaf piece tree with `line_feed_cnt = None` and **no line index** (`:626`). Nothing but an 8 KB detection sample is read.

`BufferData` (`piece_tree.rs:14`):

```rust
enum BufferData {
    Loaded   { data: Vec<u8>, line_starts: Option<Vec<usize>> },
    Unloaded { file_path: PathBuf, file_offset: usize, bytes: usize },
}
```

### 3.2 Chunked on-demand loading

When a viewport needs bytes inside an unloaded region, `get_text_range_mut` (`buffer/mod.rs:1250`) isolates and loads a chunk of at most `LOAD_CHUNK_SIZE = 1 MB` (`:89`). `StringBuffer::create_chunk_buffer` (`piece_tree.rs:192`) carves a sub-range as a new unloaded buffer and stamps `stored_file_offset = Some(absolute_offset)` — the marker that lets the save path (§7) recognize a loaded chunk as *original file content* rather than a user edit. `StringBuffer::load` (`:142`) reads the range via the `FileSystem` trait and asserts the returned length matches the contract (`:157`, defense against a remote FS short read that would corrupt save).

`split_leaves_to_chunk_size(max)` (`piece_tree.rs:1576`) pre-splits oversized leaves so each is ≤ chunk size, then rebuilds once. Used before line scanning (`prepare_line_scan`, `buffer/mod.rs:1734`) and search (§ search, `:1925`).

### 3.3 Concessions for non-resynchronizable encodings

Lazy chunking requires being able to find character boundaries when jumping mid-file. CJK encodings (Shift-JIS, GB18030, GBK, EUC-KR) are **non-resynchronizable** (`encoding.rs:165-218`). For these, large-file mode is abandoned: `check_large_file_encoding` (`buffer/mod.rs:497`) returns a `LargeFileEncodingConfirmation` and, once confirmed, the entire file is loaded and converted (`:596-607`). Non-UTF-8/ASCII large files always fall back to full load. So the negligible-memory guarantee holds specifically for large **UTF-8/ASCII** files (the common case); other encodings trade memory for correctness, with a user prompt.

### 3.4 The `file_kind` flags

`BufferFileKind` (`buffer/file_kind.rs:11`) carries three booleans: `large_file` (lazy mode, no line index), `line_feeds_scanned` (per-leaf `line_feed_cnt` are accurate), `is_binary` (opened read-only, no encoding conversion on save). These flags, not the encoding detector, gate the line-index and save behaviors.

---

## 4. Markers / interval tree (anchoring)

Everything that must stick to a logical text position across edits — search highlights, selections, folds, diagnostics, inlay/ghost virtual text, margin indicators (breakpoints) — is anchored by a **marker** whose byte position the system shifts automatically on every edit.

### 4.1 Structure: augmented AVL interval tree

The real storage is `IntervalTree` (`marker_tree.rs:84`): a node-based (`Rc<RefCell<Node>>`) **self-balancing AVL** tree, augmented as an interval tree, with VSCode-style lazy delta propagation. (Note this is the opposite balancing choice from the piece tree — markers are far more numerous and queried by overlap, so per-node rotation + augmentation wins here.)

```rust
struct Node {
    marker: Marker, height: i32,          // AVL height
    max_end: u64,                          // interval augmentation (subtree max end)
    lazy_delta: i64,                       // deferred shift for node + descendants
    parent: WeakNodePtr, left: NodePtr, right: NodePtr,
}                                          // marker_tree.rs:67
```

- **AVL** via `balance`/`rotate_left`/`rotate_right` (`:727-797`).
- **`max_end`** lets `query_recursive` (`:697`) prune subtrees → O(log n + k) overlap queries.
- **Lazy deltas**: an edit pushes an `i64` onto subtree roots rather than touching every node; `get_position` (`:268`) reconstructs a true position by summing ancestors' unpushed deltas walking to the root. This keeps edit-time shifting O(log n).
- BST key is `(start, id)` (`:589`); `marker_map: HashMap<MarkerId, Node>` gives O(1) id→node.

`MarkerList` (`marker.rs:43`) is a thin wrapper owning an `IntervalTree`, exposing point markers as zero-length intervals `[pos,pos]`. The `MarkerEntry` enum (`marker.rs:22`) and `MarkerList::entries()` are **vestigial** remnants of the old Vec/gap-buffer implementation; `entries()` returns `&[]`.

### 4.2 What anchors via markers

`MarkerType` (`marker_tree.rs:21`) has only two variants — `Position` and `LineAnchor { estimated_line, confidence }`. There is no `Bookmark`/`Fold`/`Diagnostic` variant: **feature semantics live in the consumers**, each a higher-level manager creating plain `Position` markers (or start/end pairs):

| Feature | Consumer | Gravity used |
|---|---|---|
| Search-match highlight | `OverlayManager` (`view/overlay.rs:222`) | start right, **end left** |
| Selection / diff line highlight | `OverlayManager` (`view/overlay.rs:180`) | start left, end right |
| Folds | `view/folding.rs:82` | start+end |
| Inlay/ghost/inline-diagnostic virtual text | `VirtualTextManager` (`view/virtual_text.rs`) | single point |
| Margin indicators (breakpoints) | `MarginManager` own `MarkerList` (`view/margin.rs:311`) | point |

### 4.3 Insertion gravity

Gravity decides what happens to a boundary when text is inserted *exactly at* it. Insertions strictly before always shift the marker; strictly after never do; gravity breaks the tie. `right_gravity = true` (default) pushes the marker forward (ends after inserted text); `right_gravity = false` (left) leaves it in place. Motivated by issue #2053 — search highlights must not grow when you type right after a match (`marker_tree.rs:48-58`).

Displacement on insert/delete is `adjust_recursive` (`:621`). Load-bearing bits: `stay_put = left_gravity && delta > 0 && pos == start` (`:643`); deletions clamp start to the deletion point: `(start + delta).max(pos)` (`:648`) — this is the "markers inside a deleted range collapse to the deletion start" behavior. An insertion strictly before a node bumps the right child's `lazy_delta` instead of recursing (`:662`), preserving O(log n).

**Caveat:** `MarkerList::create(pos, left_affinity)` always inserts a *right*-gravity marker, storing `left_affinity` only in a side map; true left gravity requires `create_left_gravity` (`marker.rs:102`). So callers passing `left_affinity = true` (margins, overlay starts) do **not** get sticky-left behavior — only search-match ends do (which call `create_left_gravity`).

### 4.4 AnchorConfidence

`AnchorConfidence` (`marker_tree.rs:33`) = `Exact | Estimated | Relative(MarkerId)`, a field of `LineAnchor`. Intent: byte offsets stay precise under edits (the tree keeps the interval correct), but a *derived line number* may be stale/guessed, so it carries a confidence tag that callers can upgrade by re-scanning (`update_line_anchor`, `:539`). **Status: dormant.** `LineAnchor`/`AnchorConfidence` have a full unit-tested API but zero production callers, and no code path actually degrades confidence on edit (`adjust_recursive` never touches `marker_type`). This is designed-but-unwired scaffolding — consistent with the markers' real clients being overlays/virtual-text/margins (`undo-redo-markers-analysis.md:5-9`).

### 4.5 Edit displacement & bulk edits

All edits funnel through `IntervalTree::adjust_for_edit(pos, delta)` (`marker_tree.rs:439`); `MarkerList::adjust_for_insert/delete` call it with ±len. Two subtleties added in commit `41d173fdf`:

- **Gravity-reversal repair** (`:455-486`): an insertion shared by a left-gravity stayer and a right-gravity mover reverses their `(start,id)` order, which the in-place BST can't represent. Fix: delete the stayers, run `adjust_recursive`, re-insert them so the BST is rebuilt valid.
- **Identity-based delete** (`:301`): marker removal routes via `marker_map` (identity), not the `(start,id)` key, because clamping can leave two markers sharing a position with order-contradicting ids.

Bulk-edit forward marker adjustment lives at `app/event_apply.rs:344` (`apply_events_as_bulk_edit`) and merges same-position delete+insert into a net delta (`:366`) — a refinement absent from the docs. On undo/redo, `replay_bulk_marker_adjustments` + `restore_displaced_markers` (`state.rs:941`, `:1000`) re-apply and restore markers; markers that collapsed into a deleted range are snapshotted *before* deletion and restored, so collapse is recoverable on undo (contradicting both undo-redo docs, which call it a permanent limitation).

> **Doc discrepancies (markers):** `bulk-edit-marker-displacement.md:20` and `undo-redo-markers-analysis.md:21` cite the forward fix at `app/mod.rs:2568-2580`; it now lives at `app/event_apply.rs:344-383` after the god-function decomposition. Both undo-redo docs present BulkEdit-undo marker handling and irreversible collapse as *open*; they are implemented (`event.rs` + `state.rs`).

---

## 5. Event model & undo/redo

### 5.1 Event vs BulkEdit

`Event` (`event.rs:11`) is the editor's *undoable* event type (distinct from `ControlEvent` in `control_event.rs`, a fire-and-forget `(name, data)` notification not in the undo system, and from `edit.rs`'s lightweight version-history `Edit`). It has ~40 variants: text (`Insert` `:13`, `Delete` `:20`), cursors (`MoveCursor`, `AddCursor`, `RemoveCursor`, anchors), viewport, overlays, popups, margins, splits, and two aggregate forms:

- **`Batch { events: Vec<Event>, .. }`** (`:199`) — applies/undoes N child events sequentially. This is the naive multi-cursor path: one `Insert`/`Delete` per cursor, each triggering its own tree traversal → **O(n²)** for n cursors.
- **`BulkEdit { old_snapshot, new_snapshot, old/new_cursors, edits, displaced_markers, .. }`** (`:210`) — the optimized path. Snapshots are `#[serde(skip)]` (runtime-only).

### 5.2 The O(n²)→O(n) multi-cursor optimization (design-decisions #2)

Problem (`design-decisions.md:63-79`): a sequential `Batch` did one tree path-copy per cursor. Solution: collapse all N edits into one `Vec<(pos, del_len, text)>` sorted descending by position and apply them in a **single** tree pass — `TextBuffer::apply_bulk_edits` (`buffer/mod.rs:1157`) → `PieceTree::apply_bulk_edits` (`piece_tree.rs:1833`), which gathers all split points, sorts/dedups once, and rebuilds in one pass (`collect_leaves_with_multi_split`, `:1975`). Reported ~500× speedup for multi-cursor ops. `Batch` still exists as the fallback when no event mutates the buffer (`event_apply.rs:258`).

### 5.3 O(1) undo via Arc snapshots

Because the piece tree is persistent (§2.2), capturing pre/post state is just Arc clones, not a content copy:

```rust
// buffer/mod.rs:1147
fn snapshot_buffer_state(&self) -> Arc<BufferSnapshot> {
    Arc::new(BufferSnapshot {
        piece_tree: self.piece_tree.clone(),   // = Arc::clone of root, O(1)
        buffers: self.buffers.clone(),
        next_buffer_id: self.next_buffer_id,
    })
}
```

`BufferSnapshot` (`buffer/mod.rs:191`) bundles the tree **and** the `buffers` pool. Restore (`restore_buffer_state`, `:1135`) swaps both back in. `BulkEdit::inverse` (`event.rs:474`) just swaps `old_snapshot`/`new_snapshot`, the cursor lists, and `del_len`/`ins_len` in each `edits` tuple — no recomputation, so undo/redo is **O(1) restore**.

> **Cost caveat:** the tree clone is truly O(1), but `BufferSnapshot` also clones `buffers: Vec<StringBuffer>` (O(number of string buffers) + their byte payloads). This is required for correctness against `consolidate_after_save()`, which can replace the string buffers a snapshot's tree references (`buffer/mod.rs:1144-1146`; commit `89df8ff64` "Fix undo corruption after save"). So a BulkEdit snapshot is O(1) in the *tree* but not strictly O(1) overall.

### 5.4 EventLog

`EventLog` (`event.rs:620`) is a single append-only `entries: Vec<LogEntry>` plus a `current_index` cursor — left of the cursor is "done", right is "redoable" (no separate stacks). `can_undo` = `current_index > 0`; `undo` (`:902`) walks backward applying `inverse()` of each entry, stopping after the first *write* action (one Undo = one logical edit); `redo` (`:945`) walks forward. `append` (`:806`) truncates redo history only on write actions and **does not log** non-write events (e.g. `MoveCursor`) so navigation after undo preserves the redo chain (matches VS Code/Sublime).

- **Undo groups** (`:672`, walked at `:915`) tag entries with a shared `group_id` so a macro replay reverts atomically (#2062, commit `ef2b27600`).
- **Modified-since-saved:** `saved_at_index: Option<usize>`; `mark_saved` (`:692`) records the current index; `is_at_saved_position` (`:706`) is true if the index matches *or* every event between is non-mutating — so undoing/redoing across cursor moves still reports "not modified". A real v0.1.77 truncation panic is guarded by `test_is_at_saved_position_after_truncate` (`:1456`).

> **Planned/stub:** `EventLog::snapshots` + `snapshot_interval` (periodic seek snapshots) are declared but never populated (`Snapshot::buffer_state: ()`, `:607`; creation is a no-op, `:854`). Only per-BulkEdit snapshots are real. Also: `old_snapshot`/`new_snapshot` and `group_id` are `#[serde(skip)]`, so a **reloaded** JSON-Lines log loses BulkEdit-undo state and group atomicity, falling back to per-entry undo.

---

## 6. Composite buffers

`CompositeBuffer` (`composite_buffer.rs:13`) synthesizes one logical view from multiple source `TextBuffer`s, enabling side-by-side diff, unified diff, 3-way merge, and code review. It owns **no** text and is stored per-window (`app/window/mod.rs:319`), not in the buffer pool.

```rust
struct CompositeBuffer {
    id, name, layout: CompositeLayout,
    sources: Vec<SourcePane>,        // each -> a TextBuffer by id (+ optional byte range)
    alignment: LineAlignment,        // display_row -> per-pane source lines
    active_pane: usize, mode, initial_focus_hunk,
}                                     // composite_buffer.rs:13
```

`SourcePane` (`:134`) references a `TextBuffer` by `BufferId` with an optional sub-range. `LineAlignment` (`:230`) is `rows: Vec<AlignedRow>`; each `AlignedRow` (`:442`) has `pane_lines: Vec<Option<SourceLineRef>>` (`None` = padding/blank for that pane) and a `RowType` (`:500`: `Context | Deletion | Addition | Modification | HunkHeader`). `LineAlignment::from_hunks` (`:262`) builds alignment from git-style `DiffHunk` ops, keeping unchanged lines paired (`:282-324`) and falling back to positional pairing when per-line ops are absent.

Layouts (`CompositeLayout`, `:106`): `SideBySide` (default), `Stacked` (notebook cells), `Unified`. Hunk navigation: `next_hunk_row`/`prev_hunk_row` (`:419`). Wiring: created via `EditorState::create_composite_buffer` (`app/composite_buffer_actions.rs:481`) and the plugin path (`:1086`); rendered by `view/ui/split_rendering/orchestration/render_composite.rs`; input via `input/composite_router.rs`; scroll sync via `app/scroll_sync.rs`. The diff/review feature drives the heavy usage (review-diff commits).

> **Planned:** Buffer Groups (`buffer-groups-design.md`) — `BufferGroup`/`GroupLayout`/`createBufferGroup` to host a composite as one leaf of a multi-split tab — is design-stage; no `BufferGroup` struct exists. Composite buffers ship and stand alone today.

---

## 7. Encoding detection & the save path

Design philosophy (design-decisions #5, `:127-144`): **normalize on load** — convert to UTF-8 with LF immediately, remember the original encoding/line-ending/BOM, convert back on save.

### 7.1 Detection

`Encoding` (`encoding.rs:30`) has 13 variants (UTF-8, UTF-8-BOM, UTF-16 LE/BE, ASCII, Latin-1, Windows-1250/1251/1252, GB18030, GBK, Shift-JIS, EUC-KR). `detect_encoding_or_binary` (`:253`) samples the first 8 KB and runs four priority phases, returning at the first verdict: BOM (`:281`, definitive), strict UTF-8 validation with truncation tolerance (`:297`), BOM-less UTF-16 via null-byte alternation >50% (`:340`), then a legacy phase (`:374`) that rejects binary, runs `chardetng`, and disambiguates Windows-125x via `encoding_heuristics.rs` (1250 at `:39`, 1251 at `:99`). The internal 8 KB clamp counts as truncation (`:263`, fix #1635) so a multi-byte sequence straddling the cut isn't misclassified. Line endings: `detect_line_ending` (`format.rs:123`) majority-votes CRLF/CR/LF over 8 KB, defaulting LF. Load converts to UTF-8 LF (`detect_and_convert`, `:534`).

### 7.2 Format state & save

`BufferFormat` (`format.rs:50`) stores **both** current and original `encoding`/`line_ending`; `*_changed_since_load()` compares them, and `promote_current_to_original` (`:111`) rebaselines after a successful save. `save::build_write_recipe` (`save.rs:136`) computes `needs_line_ending_conversion` and `needs_encoding_conversion` (the latter true whenever the encoding isn't plain UTF-8/ASCII, since storage is always UTF-8, `:157`), prepends the original BOM as the first insert (`:176`), and for each piece either emits a zero-copy `Copy{offset,len}` for unchanged `Stored` regions from the same file or re-encodes/re-line-ends the bytes into an `Insert`. Binary files skip conversion, preserving raw bytes. Robustness: atomic temp-file write, in-place write with crash-recovery metadata, and a `SudoSaveRequired` path that preserves ownership/permissions (`save.rs:278`, `:392`, `:26`).

### 7.3 Pristine-saved-root rebuild

Two "saved root" mechanisms:

- **`Persistence::saved_root: Arc<PieceTreeNode>`** (`persistence.rs:37`) — the tree snapshot at last save. `diff_since_saved` (`:243`) short-circuits via `Arc::ptr_eq` on roots (`:259`) then falls to the structural diff. `finalize_save` calls `mark_saved_snapshot` + `promote_current_to_original` (`buffer/mod.rs:781`).
- **`rebuild_with_pristine_saved_root`** (`buffer/mod.rs:2110`) — run after an incremental line-feed scan. It rebuilds a *pristine* tree (the whole original file as one `Stored(0)` piece, chunk-split, with scanned line counts), sets it as `saved_root` (`:2178`), then replays the user's deletions/insertions onto a clone. The point: the live tree and `saved_root` **share Arc pointers for unedited subtrees**, so `diff_since_saved` and the structural diff (`piece_tree_diff.rs:24`, `Arc::ptr_eq` fast path) cost O(edited region), not O(file size). `stored_file_offset` on chunk buffers (§3.2) is what lets the rebuild tell loaded-but-unedited chunks from real edits (`:2144`).

> **Doc discrepancies (encoding):** `design-decisions.md` lists fewer example encodings than shipped; the `Gbk` variant exists but detection always folds GBK into `Gb18030` (`encoding.rs:414`); invalid bytes are silently lossy on convert; mixed-encoding files are not handled (single whole-file encoding) — all acknowledged open questions in the doc.

---

## 8. Supporting types

- **`piece_tree_diff.rs`** — `diff_piece_trees` (`:24`) computes changed byte ranges between two roots, using `Arc::ptr_eq` to skip identical subtrees in O(1), so it costs O(changed path). Drives modified-line gutter marks and save diffing.
- **`line_diff.rs`** — LCS line diff (`ChangeType::{Inserted, Modified, Deleted}`) for saved-vs-current comparison.
- **`cursor.rs`** — `Cursor { position, anchor, sticky_column, selection_mode, block_anchor, deselect_on_move }` (`:24`); positions are **byte offsets**; `Cursors` (`:190`) is a `HashMap<CursorId, Cursor>` with a primary, supporting multi-cursor merge/dedupe and per-edit adjustment.
- **`buffer_position.rs`** — free functions (`byte_to_2d`, `pos_2d_to_byte`) converting byte↔(line,col) over a `&Buffer`, kept as functions to avoid growing `TextBuffer`'s surface (`:4-7`).
- **`document_model.rs`** — the `DocumentModel` trait + `DocumentPosition` (`LineColumn` or `ByteOffset`, the dual coordinate system for huge files) and viewport types.
- **`filesystem.rs`** — `FileSystem` trait (`:424`): `read_range` (the lazy-load primitive), `write_patched` (zero-copy save), `metadata`, `search_file` (server-side search). Local and remote backends implement it, so the entire model is FS-agnostic (enables remote editing).

---

## 9. Implemented vs planned (summary)

**Implemented:** persistent path-copying piece tree with rebuild-balancing; two-tier line indexing with large-file `None` mode; chunked lazy loading of UTF-8/ASCII multi-GB files; AVL interval-tree markers with gravity, lazy deltas, gravity-reversal repair, identity delete, and undo-time marker restore; single-pass `BulkEdit` with O(1) Arc-snapshot undo and undo groups; `EventLog` index-cursor undo/redo + modified-since-saved; composite buffers (side-by-side/unified/stacked) wired into diff/review; encoding+line-ending+BOM detection and preservation with pristine-saved-root structural-sharing diff; full buffer-refactor submodule split (`format`/`file_kind`/`persistence`/`save`/`search`).

**Planned / dormant:** `LineAnchor` + `AnchorConfidence` (no production callers, no degradation logic); `EventLog` periodic seek snapshots (stubbed); Buffer Groups; `MarkerEntry`/`entries()` (vestigial). Non-UTF-8/large CJK files concede lazy loading for a full-load prompt. Reloaded undo logs lose BulkEdit/group state (`#[serde(skip)]`).
