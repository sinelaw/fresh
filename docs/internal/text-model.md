# Text Model Architecture

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: explain how Fresh stores, edits, indexes, snapshots, anchors, and persists text — the `model/` layer — and the decisions, trade-offs, and concessions behind each part.

---

## 1. Layer map

Three "buffer" concepts are easy to conflate; they are distinct:

| Type | Role | Owns text? |
|---|---|---|
| `PieceTree` | persistent (path-copying) piece-table tree | structure only; bytes live in `StringBuffer`s |
| `TextBuffer` (alias `Buffer`) | the document store: piece tree + buffer pool + format/persistence metadata | yes |
| `CompositeBuffer` | synthesized multi-source view (diff/merge/side-by-side) | no — references `TextBuffer`s by id |

`DocumentModel` is a **trait**, the editor-facing abstraction over a buffer; it is implemented by `EditorState`, not by `TextBuffer`.

Cursors and markers are **not** owned by `TextBuffer`. It owns only the piece tree, the buffer pool, and persistence/file-kind/format/version/config state. Cursors and markers are held by editor/view state.

---

## 2. The piece tree (piece table)

### 2.1 Why a piece table, not a rope / gap buffer / array

A piece table never mutates the original file bytes. The document is a sequence of *pieces*, each a `(buffer, offset, length)` slice into one of two append-only byte pools:

- **Stored** content — the original file (or, in large-file mode, an *unloaded* reference to a byte range on disk).
- **Added** content — everything the user has typed since load, appended to a growing buffer; existing bytes are never moved.

Each piece is tagged `Stored` or `Added`. This buys three properties an editor specifically wants:

1. **Cheap O(1) snapshots for undo/redo.** Because edited bytes are never overwritten — they are split off into new pieces — a snapshot is just a reference to the current tree root (see §4 and §5). A rope can also share structure, but the piece table's "original buffer is immutable" invariant additionally makes the **save diff** (§7) and structural diff trivial: unchanged regions are literally the same `Arc` nodes.
2. **Huge files with negligible memory.** A `Stored` piece can point at a byte range that is *not in memory* (`BufferData::Unloaded`, §3). The whole multi-GB file is one piece referencing the file on disk; bytes are pulled in lazily per viewport. A rope or gap buffer would have to materialize the entire file.
3. **Negligible per-edit cost.** Inserts/deletes split at most one leaf and append to the Added buffer — no large memmove (gap buffer's weakness on multi-cursor scatter) and no array shift.

The trade-off vs a rope: a piece table degrades into many small pieces under pathological edit patterns, and line/column lookup is not naturally O(log n) unless line counts are tracked per node. Fresh addresses both (§2.4 line tracking, §2.5 rebalancing).

### 2.2 Node shape

A piece tree node is a binary tree of internal nodes and leaves. An internal node carries summary fields for its left subtree (left byte count and left line-feed count) plus `Arc` references to its two children. A leaf carries a buffer location (`Stored`/`Added`), an offset, a byte length, and an optional line-feed count.

A `PieceTree` is the tree root plus a total byte count, and derives `Clone` — so cloning a tree is one `Arc::clone` of the root. Children are held behind `Arc`, making the tree a **persistent (immutable) data structure**: every edit produces a new root that shares all untouched subtrees with the old root.

Internal nodes carry **augmented order statistics**: the left-subtree byte count drives O(log n) offset→piece lookup; the left-subtree line-feed count drives O(log n) line↔offset lookup. Both are summary fields recomputed on the path-copy path so they stay consistent.

### 2.3 Edits: path-copying insert/delete

Insert builds an insert leaf and path-copies: it walks root→target leaf, splits only the leaf at the insertion point, and `Arc::clone`s every sibling subtree along the way. Delete is symmetric, trimming or dropping the affected leaves. Newline counts for the new sub-pieces are computed by scanning the underlying buffer bytes — or left unset when the buffer is unloaded.

Because only the root-to-leaf path is cloned, an edit is **O(log n) new nodes**; the old root remains valid and untouched. That is the property that makes both undo snapshots (§5) and the structural diff (§7) cheap.

### 2.4 Line indexing (two-tier)

Line/column navigation needs newline counts. Fresh tracks them at two levels:

- **Per leaf:** an optional line-feed count, summed into the internal nodes' left line-feed summaries. An unset count means "not yet scanned" — line-based navigation is unavailable for that subtree and line↔offset lookups bail out.
- **Per `StringBuffer`:** an optional `line_starts` index. Present for small/loaded buffers; **absent = large-file mode**, where building a full line index would defeat the negligible-memory goal.

This optional index is the central large-file concession: with no line index, the editor works in byte-offset space (see `DocumentPosition::ByteOffset`) and fills in per-leaf line-feed counts incrementally as chunks are scanned. The incremental update has a path-copy variant that preserves `Arc::ptr_eq` for unscanned subtrees.

### 2.5 Balancing: rebuild, not red-black

Fresh does **not** use a self-balancing rotation tree in the piece tree (no red-black / AVL — that lives in the marker tree, §6). Instead, after each insert it checks depth: when depth grows past roughly twice the optimum for the leaf count, it collapses the whole tree to a flat leaf list and rebuilds a perfectly balanced tree via recursive midpoint split.

Trade-off: a full O(n) rebuild is asymptotically worse per-trigger than O(log n) rotations, but (a) it is dramatically simpler and avoids the subtle parent-pointer/rotation bugs an RB-tree invites, (b) it triggers rarely (only when depth roughly doubles past optimal), so amortized cost stays low, and (c) the rebuilt tree is *optimally* balanced rather than merely within an RB factor. The concession is accepted deliberately: editor edit rates are low relative to what a rebuild costs, and the code-simplicity win is large. The balanced-build primitive is also reused for construction from a leaf list, chunk splitting (§3), and pristine-root rebuild (§7).

### 2.6 Iteration & range reads

A piece-range iterator walks pieces for a byte range; helper routines provide line math (line counts within a byte range, offset→position). Range reads that may touch unloaded bytes go through the buffer's range-read entry point, which lazy-loads (§3) — the low-level `StringBuffer` data accessor is crate-private precisely to force callers through that lazy path.

---

## 3. Lazy loading for multi-GB files

Backs the project claim of multi-gigabyte files with negligible memory overhead.

### 3.1 Load strategy

The internal load routine branches on file size against a large-file threshold:

- **Below threshold** → small-file load: read fully, detect encoding/binary, compute the line index.
- **At or above threshold** → large-file load: for **UTF-8/ASCII** files, create one `StringBuffer` with `BufferData::Unloaded` spanning the whole file and a single-leaf piece tree with no per-leaf line-feed count and **no line index**. Nothing but a small detection sample is read.

`BufferData` distinguishes a `Loaded` variant (an in-memory byte vector plus optional line-starts index) from an `Unloaded` variant (a file path, file offset, and byte length).

### 3.2 Chunked on-demand loading

When a viewport needs bytes inside an unloaded region, the range-read path isolates and loads a bounded chunk. A chunk buffer carves a sub-range as a new unloaded buffer and stamps its absolute file offset — the marker that lets the save path (§7) recognize a loaded chunk as *original file content* rather than a user edit. The buffer load reads the range via the `FileSystem` trait and asserts the returned length matches the requested contract — defense against a remote FS short read that would corrupt save.

A split-to-chunk-size operation pre-splits oversized leaves so each is within chunk size, then rebuilds once. It is used before line scanning and before search.

### 3.3 Concessions for non-resynchronizable encodings

Lazy chunking requires being able to find character boundaries when jumping mid-file. Several CJK encodings (Shift-JIS, GB18030, GBK, EUC-KR) are **non-resynchronizable**. For these, large-file mode is abandoned: the encoding check returns a confirmation prompt and, once confirmed, the entire file is loaded and converted. Non-UTF-8/ASCII large files always fall back to full load. So the negligible-memory guarantee holds specifically for large **UTF-8/ASCII** files (the common case); other encodings trade memory for correctness, with a user prompt.

### 3.4 The `file_kind` flags

`BufferFileKind` carries three booleans: `large_file` (lazy mode, no line index), `line_feeds_scanned` (per-leaf line-feed counts are accurate), and `is_binary` (opened read-only, no encoding conversion on save). These flags, not the encoding detector, gate the line-index and save behaviors.

---

## 4. Markers / interval tree (anchoring)

Everything that must stick to a logical text position across edits — search highlights, selections, folds, diagnostics, inlay/ghost virtual text, margin indicators (breakpoints) — is anchored by a **marker** whose byte position the system shifts automatically on every edit.

### 4.1 Structure: augmented AVL interval tree

The real storage is an `IntervalTree`: a node-based, **self-balancing AVL** tree, augmented as an interval tree, with VSCode-style lazy delta propagation. This is the opposite balancing choice from the piece tree — markers are far more numerous and queried by overlap, so per-node rotation plus augmentation wins here.

Each node holds a marker, its AVL height, a subtree max-end (the interval augmentation), a lazy delta (a deferred shift for the node and its descendants), and parent/child links.

- **AVL** via balance plus left/right rotation.
- **Subtree max-end** lets overlap queries prune subtrees → O(log n + k) overlap queries.
- **Lazy deltas**: an edit pushes a shift onto subtree roots rather than touching every node; reconstructing a true position sums ancestors' unpushed deltas walking to the root. This keeps edit-time shifting O(log n).
- The BST key is `(start, id)`; an id→node map gives O(1) lookup by id.

`MarkerList` is a thin wrapper owning an `IntervalTree`, exposing point markers as zero-length intervals. The `MarkerEntry` enum and the list's `entries()` accessor are **vestigial** remnants of the old Vec/gap-buffer implementation; `entries()` returns an empty slice.

### 4.2 What anchors via markers

`MarkerType` has only two variants — `Position` and `LineAnchor`. There is no `Bookmark`/`Fold`/`Diagnostic` variant: **feature semantics live in the consumers**, each a higher-level manager creating plain `Position` markers (or start/end pairs):

| Feature | Consumer | Gravity used |
|---|---|---|
| Search-match highlight | `OverlayManager` | start right, **end left** |
| Selection / diff line highlight | `OverlayManager` | start left, end right |
| Folds | folding manager | start+end |
| Inlay/ghost/inline-diagnostic virtual text | `VirtualTextManager` | single point |
| Margin indicators (breakpoints) | `MarginManager`'s own `MarkerList` | point |

### 4.3 Insertion gravity

Gravity decides what happens to a boundary when text is inserted *exactly at* it. Insertions strictly before always shift the marker; strictly after never do; gravity breaks the tie. Right gravity (the default) pushes the marker forward (ends after inserted text); left gravity leaves it in place. The motivating case: search highlights must not grow when text is typed right after a match.

Displacement on insert/delete is handled by the tree's adjust routine. Load-bearing behaviors: a left-gravity marker whose start equals the insertion point stays put on insert; deletions clamp a marker's start to the deletion point — this is the "markers inside a deleted range collapse to the deletion start" behavior. An insertion strictly before a node bumps the right child's lazy delta instead of recursing, preserving O(log n).

**Caveat:** the default `MarkerList` create path always inserts a *right*-gravity marker, storing the requested affinity only in a side map; true left gravity requires the dedicated left-gravity create path. So callers passing left affinity (margins, overlay starts) do **not** get sticky-left behavior — only search-match ends do (which call the left-gravity path).

### 4.4 AnchorConfidence

`AnchorConfidence` is `Exact | Estimated | Relative(MarkerId)`, a field of `LineAnchor`. Intent: byte offsets stay precise under edits (the tree keeps the interval correct), but a *derived line number* may be stale or estimated, so it carries a confidence tag that callers can upgrade by re-scanning. **Status: dormant.** `LineAnchor`/`AnchorConfidence` have a full unit-tested API but no production callers, and no code path degrades confidence on edit (the adjust routine never touches the marker type). This is designed-but-unwired scaffolding — consistent with the markers' real clients being overlays/virtual-text/margins.

### 4.5 Edit displacement & bulk edits

All edits funnel through the interval tree's adjust-for-edit entry point (position, delta); the `MarkerList` insert/delete helpers call it with the signed length. Two subtleties:

- **Gravity-reversal repair**: an insertion shared by a left-gravity stayer and a right-gravity mover reverses their `(start, id)` order, which the in-place BST cannot represent. Fix: delete the stayers, run the adjust, re-insert them so the BST is rebuilt valid.
- **Identity-based delete**: marker removal routes via the id→node map (identity), not the `(start, id)` key, because clamping can leave two markers sharing a position with order-contradicting ids.

Bulk-edit forward marker adjustment merges a same-position delete+insert into a net delta. On undo/redo, the bulk marker adjustments are replayed and displaced markers restored; markers that collapsed into a deleted range are snapshotted *before* deletion and restored, so collapse is recoverable on undo.

---

## 5. Event model & undo/redo

### 5.1 Event vs BulkEdit

`Event` is the editor's *undoable* event type, distinct from `ControlEvent` (a fire-and-forget `(name, data)` notification not in the undo system) and from the lightweight version-history `Edit`. It has roughly forty variants: text (`Insert`, `Delete`), cursors (move/add/remove, anchors), viewport, overlays, popups, margins, splits, and two aggregate forms:

- **`Batch`** — applies/undoes N child events sequentially. This is the naive multi-cursor path: one `Insert`/`Delete` per cursor, each triggering its own tree traversal → **O(n²)** for n cursors.
- **`BulkEdit`** — the optimized path, carrying old/new snapshots, old/new cursor lists, the edit list, and displaced markers. The snapshots are runtime-only (not serialized).

### 5.2 The O(n²)→O(n) multi-cursor optimization

Problem: a sequential `Batch` did one tree path-copy per cursor. Solution: collapse all N edits into one list of `(position, delete length, text)`, sort it descending by position, and apply it in a **single** tree pass — the buffer's bulk-edit path gathers all split points, sorts/dedups once, and rebuilds in one pass. The reported result is roughly a 500× speedup for multi-cursor operations. `Batch` still exists as the fallback when no event mutates the buffer.

### 5.3 O(1) undo via Arc snapshots

Because the piece tree is persistent (§2.2), capturing pre/post state is just Arc clones, not a content copy. A buffer snapshot bundles the piece tree (an `Arc::clone` of the root, O(1)), the buffer pool, and the next-buffer-id counter. Restore swaps both the tree and the buffer pool back in. `BulkEdit::inverse` swaps the old/new snapshots, the cursor lists, and the delete/insert lengths in each edit tuple — no recomputation, so undo/redo is **O(1) restore**.

> **Cost caveat:** the tree clone is truly O(1), but a buffer snapshot also clones the buffer pool (O(number of string buffers) plus their byte payloads). This is required for correctness against post-save consolidation, which can replace the string buffers a snapshot's tree references. So a BulkEdit snapshot is O(1) in the *tree* but not strictly O(1) overall.

### 5.4 EventLog

`EventLog` is a single append-only entry list plus a current-index cursor — left of the cursor is "done", right is "redoable" (no separate stacks). Undo is possible when the index is past the start; undo walks backward applying each entry's inverse, stopping after the first *write* action (one Undo = one logical edit); redo walks forward. Append truncates redo history only on write actions and **does not log** non-write events (such as cursor moves) so navigation after undo preserves the redo chain (matching VS Code/Sublime).

- **Undo groups** tag entries with a shared group id so a macro replay reverts atomically.
- **Modified-since-saved:** a saved-at-index field records the index at save time; the buffer reports unmodified if the current index matches that saved index *or* every event between is non-mutating — so undoing/redoing across cursor moves still reports "not modified". A regression test guards a truncation panic that previously occurred here.

> **Planned/stub:** `EventLog`'s periodic seek snapshots and snapshot interval are declared but never populated (snapshot creation is a no-op). Only per-`BulkEdit` snapshots are real. Also: the snapshots and group id are not serialized, so a **reloaded** JSON-Lines log loses BulkEdit-undo state and group atomicity, falling back to per-entry undo.

---

## 6. Composite buffers

`CompositeBuffer` synthesizes one logical view from multiple source `TextBuffer`s, enabling side-by-side diff, unified diff, 3-way merge, and code review. It owns **no** text and is stored per-window, not in the buffer pool.

A composite buffer holds an id, name, layout, a list of source panes, a line alignment, the active pane, a mode, and an initial focus hunk. A `SourcePane` references a `TextBuffer` by id with an optional sub-range. A `LineAlignment` is a list of aligned rows; each aligned row holds per-pane line references (a missing reference means padding/blank for that pane) and a row type (`Context | Deletion | Addition | Modification | HunkHeader`). Alignment is built from git-style diff hunk ops, keeping unchanged lines paired and falling back to positional pairing when per-line ops are absent.

Layouts: `SideBySide` (default), `Stacked` (notebook cells), and `Unified`. Hunk navigation provides next/previous hunk rows. Composites are created via `EditorState` and a plugin path, rendered by the split-rendering composite path, routed for input through a composite input router, and scroll-synced across panes. The diff/review feature drives the heavy usage.

> **Planned:** Buffer Groups — a `BufferGroup`/`GroupLayout`/`createBufferGroup` mechanism to host a composite as one leaf of a multi-split tab — is design-stage; no `BufferGroup` struct exists. Composite buffers ship and stand alone today.

---

## 7. Encoding detection & the save path

Design philosophy: **normalize on load** — convert to UTF-8 with LF immediately, remember the original encoding/line-ending/BOM, convert back on save.

### 7.1 Detection

`Encoding` has thirteen variants (UTF-8, UTF-8-BOM, UTF-16 LE/BE, ASCII, Latin-1, Windows-1250/1251/1252, GB18030, GBK, Shift-JIS, EUC-KR). Detection samples the first several kilobytes and runs four priority phases, returning at the first verdict: BOM (definitive), strict UTF-8 validation with truncation tolerance, BOM-less UTF-16 via null-byte alternation over half, then a legacy phase that rejects binary, runs `chardetng`, and disambiguates the Windows-125x family via dedicated heuristics. The internal sample clamp counts as truncation so a multi-byte sequence straddling the cut is not misclassified. Line endings are chosen by majority vote of CRLF/CR/LF over the sample, defaulting to LF. Load converts to UTF-8 LF.

### 7.2 Format state & save

`BufferFormat` stores **both** current and original encoding/line-ending; change-detection compares them, and a promote step rebaselines after a successful save. The save recipe builder computes whether line-ending and encoding conversion are needed (the latter true whenever the encoding is not plain UTF-8/ASCII, since storage is always UTF-8), prepends the original BOM as the first insert, and for each piece either emits a zero-copy copy for unchanged `Stored` regions from the same file or re-encodes/re-line-ends the bytes into an insert. Binary files skip conversion, preserving raw bytes. Robustness: atomic temp-file write, in-place write with crash-recovery metadata, and a sudo-save path that preserves ownership/permissions.

### 7.3 Pristine-saved-root rebuild

Two "saved root" mechanisms:

- **Persistence's saved root** — the tree snapshot at last save. The since-saved diff short-circuits via `Arc::ptr_eq` on roots, then falls to the structural diff. Finalizing a save marks the saved snapshot and promotes current format to original.
- **Pristine-saved-root rebuild** — run after an incremental line-feed scan. It rebuilds a *pristine* tree (the whole original file as one `Stored` piece, chunk-split, with scanned line counts), sets it as the saved root, then replays the user's deletions/insertions onto a clone. The point: the live tree and the saved root **share Arc pointers for unedited subtrees**, so the since-saved diff and the structural diff (which has an `Arc::ptr_eq` fast path) cost O(edited region), not O(file size). The stored file offset on chunk buffers (§3.2) is what lets the rebuild tell loaded-but-unedited chunks from real edits.

> **Known limitations (encoding):** the `Gbk` variant exists but detection always folds GBK into `Gb18030`; invalid bytes are silently lossy on convert; mixed-encoding files are not handled (a single whole-file encoding is assumed).

---

## 8. Supporting types

- **Piece-tree diff** — computes changed byte ranges between two roots, using `Arc::ptr_eq` to skip identical subtrees in O(1), so it costs O(changed path). Drives modified-line gutter marks and save diffing.
- **Line diff** — an LCS line diff (inserted/modified/deleted) for saved-vs-current comparison.
- **Cursor** — a cursor holds a position, anchor, sticky column, selection mode, block anchor, and a deselect-on-move flag; positions are **byte offsets**. The cursor collection is a map from cursor id to cursor with a designated primary, supporting multi-cursor merge/dedupe and per-edit adjustment.
- **Buffer position** — free functions converting byte↔(line, col) over a buffer reference, kept as functions to avoid growing `TextBuffer`'s surface.
- **Document model** — the `DocumentModel` trait plus `DocumentPosition` (`LineColumn` or `ByteOffset`, the dual coordinate system for huge files) and viewport types.
- **Filesystem** — a `FileSystem` trait: range read (the lazy-load primitive), patched write (zero-copy save), metadata, and server-side file search. Local and remote backends implement it, so the entire model is FS-agnostic (enabling remote editing).

---

## 9. Implemented vs planned (summary)

**Implemented:** persistent path-copying piece tree with rebuild-balancing; two-tier line indexing with a large-file no-index mode; chunked lazy loading of UTF-8/ASCII multi-GB files; AVL interval-tree markers with gravity, lazy deltas, gravity-reversal repair, identity delete, and undo-time marker restore; single-pass `BulkEdit` with O(1) Arc-snapshot undo and undo groups; `EventLog` index-cursor undo/redo plus modified-since-saved; composite buffers (side-by-side/unified/stacked) wired into diff/review; encoding plus line-ending plus BOM detection and preservation with pristine-saved-root structural-sharing diff; the buffer-module split into format/file-kind/persistence/save/search submodules.

**Planned / dormant:** `LineAnchor` plus `AnchorConfidence` (no production callers, no degradation logic); `EventLog` periodic seek snapshots (stubbed); Buffer Groups; `MarkerEntry`/`entries()` (vestigial). Non-UTF-8/large CJK files concede lazy loading for a full-load prompt. Reloaded undo logs lose BulkEdit/group state (snapshots and group id are not serialized).
