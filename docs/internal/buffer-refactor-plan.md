# `model/buffer.rs` Refactor Plan

Target: break up the monolithic `crates/fresh-editor/src/model/buffer.rs`
into concern-scoped modules, extracting coherent field clusters from
`TextBuffer` into owned sub-structs so that the single 3,896-line
`impl TextBuffer` block is redistributed across smaller owned
subsystems. Behaviourally a no-op.

## 1. Context & measurements

The file is 8,029 lines. A majority of the surface area is in a single
struct with a single impl block, with tests inlined.

### 1.1 File composition

| L# | Item | Lines |
|---|---|---|
| 1–310 | Small helper types (`SudoSaveRequired`, `LargeFileEncodingConfirmation`, `LineScanChunk`, `ChunkedSearchState`, `BufferConfig`, `LineEnding`, `WriteRecipe`, `LineNumber`) | ~310 |
| 315–383 | **`pub struct TextBuffer` — 17 fields** | 68 |
| 391–395 | `pub struct BufferSnapshot` | 5 |
| **397–4293** | **`impl TextBuffer` — single block, ~120 methods** | **~3,896** |
| 4293–4719 | `ChunkInfo`, `OverlappingChunks`, `HybridSearchPlan` | ~430 |
| 4722–4778 | Free `search_boundary_overlap` | ~55 |
| 4779–7866 | Tests (two `#[cfg(test)] mod tests` blocks) | **~3,090** |
| 7867–8029 | `LineData`, `TextBufferLineIterator` | ~160 |

**Production code: ~4,940 lines. Test code: ~3,090 lines.**

### 1.2 Scattered-`impl` audit

```
$ rg -n '^impl.*TextBuffer\b' crates/fresh-editor/src/
crates/fresh-editor/src/model/buffer.rs:397:impl TextBuffer {
```

**Exactly one file contains `impl TextBuffer`.** The headline
anti-pattern from the editor-modules plan (scattered impls across many
files) **does not apply here**. What applies is the sibling shape: one
giant impl block, one giant file, many concerns fused.

### 1.3 External call-site blast radius

```
$ rg -n 'TextBuffer::' --type=rust crates/ | rg -v '/model/buffer\.rs' | wc -l
112
```

Touched by 18 files outside `model/buffer.rs`, including
`state.rs`, `input/actions.rs`, `app/search_scan.rs`,
`app/scan_orchestrators.rs`, `services/recovery/mod.rs`,
`model/document_model.rs`, `wasm/mod.rs`, and seven test files.
Public-API preservation is load-bearing.

### 1.4 Field-access frequency

`grep -c 'self\.<field>' model/buffer.rs` (one row per field):

| Accesses | Field | Cluster candidate |
|---:|---|---|
| 51 | `piece_tree` | Storage (core) |
| 43 | `buffers` | Storage (core) |
| 38 | `fs` | Persistence |
| 16 | `next_buffer_id` | Storage (core) |
| 13 | `file_path` | Persistence |
| 10 | `saved_root` | Persistence |
| 9 | `modified` | Persistence |
| 9 | `line_feeds_scanned` | FileKind |
| 9 | `encoding` | Format |
| 7 | `line_ending` | Format |
| 5 | `saved_file_size` | Persistence |
| 4 | `original_line_ending` | Format |
| 4 | `original_encoding` | Format |
| 3 | `recovery_pending` | Persistence |
| 3 | `large_file` | FileKind |
| 2 | `version` | (stays on `TextBuffer`) |
| 2 | `is_binary` | FileKind |
| 2 | `config` | (stays on `TextBuffer`) |

Total: 230 `self.<field>` accesses across 18 fields. Three tight
clusters (Storage, Persistence, Format+FileKind) absorb 16 of 18 fields;
`version` and `config` remain on the top-level struct.

### 1.5 Largest methods (by line count)

| Method | Approx lines | Concern |
|---|---:|---|
| `build_write_recipe` | ~130 | Save |
| `save_with_inplace_write` | ~70 | Save |
| `load_large_file_internal` | ~110 | Load |
| `diff_since_saved` | ~90 | Snapshot diff |
| `rebuild_with_pristine_saved_root` | ~130 | Save consolidation |
| `search_hybrid_plan` | ~90 | Search |
| `search_scan_next_chunk` | ~100 | Search |
| `chunk_split_and_load` | ~155 | Storage chunking |
| `HybridSearchPlan::execute` | ~175 | Search (free) |
| `get_text_range_mut` | ~120 | Storage read-with-load |

No single method rivals the cross-cutting mega-methods in the
editor-modules plan (`render` ~1,100 lines, `handle_action` ~1,162
lines). The scale issue here is breadth, not depth.

### 1.6 Invariant choke-point (pre-existing)

One method already enforces the key mutation invariant:

```rust
fn mark_content_modified(&mut self) {
    self.modified = true;
    self.recovery_pending = true;
    self.bump_version();
}
```

Called from 10 sites, all of which mutate buffer content. This is the
template for the post-refactor orchestrator shape — keep it and make it
the *only* path that flips these flags.

## 2. Why the current shape is wrong (diagnosis)

The file's problem is not scale per se (4,940 production lines isn't
enormous) but **concern fusion inside a single namespace**.

**Ten distinct concerns share the `impl TextBuffer` namespace.** A
reader looking for "how does save work" and a reader looking for "how
does search-scan resume across chunks" navigate the same ~120-method
list. The concerns are genuinely independent:

1. Construction / loading (~15 methods, L400–L887)
2. Saving + write-recipe building (~18 methods, L888–L1521)
3. Snapshot / diff-vs-saved (~10 methods, L1535–L1856)
4. Edits (~10 methods, L1870–L2136)
5. Viewport preparation and chunk loading (~5 methods, L2308–L2608)
6. Flag / metadata accessors (~25 methods, L2528–L3326)
7. Encoding & line-ending detection (~7 static methods, L3327–L3480)
8. Line operations (~10 methods, L3481–L3504, L4144–L4265)
9. Search (~12 methods, L2677–L2952, L3512–L3682)
10. Replace (~4 methods, L3683–L3772)
11. Position / boundary conversions (~15 methods, L1857–L1869, L3774–L4142)
12. Line cache (~7 no-op or near-no-op methods, L4232–L4265)

A search-concern change and an encoding-concern change land in the same
file, the same struct, and the same free-for-all mutable view of 17
fields. Code review has no way to assert "this PR touches only
persistence".

**The 17 fields are two clean clusters plus one hot core plus two
leftovers.** §3 makes this precise. Crucially, the "hot core"
(`piece_tree`, `buffers`, `next_buffer_id` — 110 of 230 field
accesses) is what most methods *actually* need; the rest is ambient
context most methods ignore. Splitting format and persistence off
shrinks what the majority of methods have to see.

**Tests are 3,090 lines in two blocks.** Inline tests multiply the
cost of any re-organisation because moving a method requires moving its
tests. The tests mirror the concern split above (search tests, save
tests, line-ending tests, binary detection tests) but are currently
fused into two monolithic `mod tests` blocks.

**External surface is wide but shallow.** 112 call sites across 18
files, but nearly all are simple accessors (`buffer.len()`,
`buffer.file_path()`, `buffer.insert(...)`). Signature preservation is
achievable; the refactor is internal.

**What this is not.** It is not a god-object refactor in the
editor-modules sense. There is no scattered `impl TextBuffer` to
consolidate. The mechanism (extract field clusters into sub-structs;
move methods to the struct that owns the fields they touch) is the
same, but the motivating measurement is the method-count-per-namespace
(~120), not the impl-files-per-type (1).

## 3. Proposed field clusters (the core of the plan)

Every one of `TextBuffer`'s 17 fields is accounted for below. Three
new sub-structs absorb 14 fields; three fields remain on `TextBuffer`
with a stated reason.

### 3.1 New sub-structs

| Sub-struct | Fields absorbed | Field accesses today | Concern |
|---|---|---:|---|
| `BufferFormat` | `line_ending`, `original_line_ending`, `encoding`, `original_encoding` | 24 | Text encoding + line-ending tracking and conversion |
| `BufferFileKind` | `large_file`, `line_feeds_scanned`, `is_binary` | 14 | Is-this-a-large/binary-file-and-has-its-line-scan-run |
| `Persistence` | `fs`, `file_path`, `modified`, `recovery_pending`, `saved_root`, `saved_file_size` | 78 | Filesystem handle, save-state tracking, saved-root snapshot |

### 3.2 Fields that remain on `TextBuffer`

| Field | Accesses | Why it stays |
|---|---:|---|
| `piece_tree` | 51 | The core storage. Every edit, read, search, and line-op touches it; extracting it into a wrapper adds indirection for zero gain. |
| `buffers` | 43 | String-buffer pool is piece-tree-coupled (the pieces reference these buffers by id). Must stay colocated with `piece_tree`. |
| `next_buffer_id` | 16 | Allocates ids for `buffers`. Trivially coupled to `buffers`. |
| `version` | 2 | Top-level monotonic counter. Read by external callers (change tracking, LSP). Belongs at the outer type for the same reason a database has one sequence. |
| `config` | 2 | Top-level tuning knobs (`estimated_line_length`). Already its own `BufferConfig` type; just a field. |

The three "stays" fields `piece_tree` / `buffers` / `next_buffer_id`
are the **storage core** (110 of 230 accesses). They do not deserve
a sub-struct of their own — they *are* the buffer. Wrapping them in
`BufferStorage` would create a pass-through struct with no distinct
concern. Leave them flat.

### 3.3 Before / after struct diff

**Before (17 flat fields):**

```rust
pub struct TextBuffer {
    fs: Arc<dyn FileSystem + Send + Sync>,
    piece_tree: PieceTree,
    saved_root: Arc<PieceTreeNode>,
    buffers: Vec<StringBuffer>,
    next_buffer_id: usize,
    file_path: Option<PathBuf>,
    modified: bool,
    recovery_pending: bool,
    large_file: bool,
    line_feeds_scanned: bool,
    is_binary: bool,
    line_ending: LineEnding,
    original_line_ending: LineEnding,
    encoding: Encoding,
    original_encoding: Encoding,
    saved_file_size: Option<usize>,
    version: u64,
    config: BufferConfig,
}
```

**After (8 fields, composed):**

```rust
pub struct TextBuffer {
    // Storage core — stays flat, see §3.2
    piece_tree:     PieceTree,
    buffers:        Vec<StringBuffer>,
    next_buffer_id: usize,

    // Extracted sub-structs (§3.1)
    persistence:    Persistence,
    format:         BufferFormat,
    file_kind:      BufferFileKind,

    // Top-level trackers (§3.2)
    version:        u64,
    config:         BufferConfig,
}
```

17 flat fields → 8 top-level fields, 3 of which are sub-structs
composing the remaining 13 fields. The shrinkage isn't the point; the
point is that a method touching `encoding` cannot accidentally touch
`modified`, and vice versa.

### 3.4 Where each sub-struct lives

```
crates/fresh-editor/src/model/buffer/
    mod.rs              // pub struct TextBuffer + orchestrators (save, load, edits)
    format.rs           // pub struct BufferFormat + detection free fns
    file_kind.rs        // pub struct BufferFileKind
    persistence.rs      // pub struct Persistence + save-state transitions
    ...                 // concern modules (see §7)
```

Outside `model/buffer/`, callers still see `TextBuffer` with accessor
methods (`buffer.encoding()`, `buffer.is_modified()`, `buffer.file_path()`).
The public API is preserved; the internal ownership is not.

## 4. Architectural principles (the hard rules)

**Rule 1 (hard, grep-enforceable).** Only `model/buffer/mod.rs` may
contain `impl TextBuffer`. Every other file in `model/buffer/` puts
methods on *its own* sub-struct (`impl Persistence`, `impl BufferFormat`,
`impl BufferFileKind`). Audit:

```
$ rg -n '^impl TextBuffer\b' crates/fresh-editor/src/model/buffer/
crates/fresh-editor/src/model/buffer/mod.rs:<line>:impl TextBuffer {
```

must return exactly one line. This rule prevents the next refactor from
re-scattering the god-object into sibling files.

**Rule 2 (hard).** A sub-struct method takes `&mut self` meaning the
sub-struct. No sub-struct method signature contains `TextBuffer`. If a
method needs `piece_tree` plus `format`, it becomes a `TextBuffer`
orchestrator, not a sub-struct method with a back-pointer.

**Rule 3 (hard).** `mark_content_modified` (and any future equivalent)
is the only path that flips `modified` / `recovery_pending` / `version`.
Post-refactor it lives on `TextBuffer` (since it touches both
`Persistence` and the top-level `version`); sub-structs never mutate
those flags directly. Audit: `rg 'persistence\.modified\s*=' model/`
returns only lines inside `mark_content_modified`.

**Rule 4.** Static/pure helpers (`detect_line_ending`, `detect_encoding*`,
`convert_to_encoding`, `normalize_line_endings`, `is_utf8_continuation_byte`,
`find_in_bytes`) are **free functions**, not methods. They take bytes,
return bytes; they never touch a struct.

**Rule 5.** Public API surface is preserved. Every method currently
callable as `buffer.foo()` remains callable as `buffer.foo()`. If a
method moves to `Persistence`, `TextBuffer` gains a one-line delegator
(`pub fn foo(&self) -> ... { self.persistence.foo() }`). Delegators are
cheap; call-site churn across 18 external files is expensive.

**Rule 6.** No flag days. Every commit compiles and every commit
passes `cargo test -p fresh-editor`. Sub-structs are introduced
behind delegators before old fields are removed, so the two
representations coexist across commits inside a phase until the old
one is deleted.

## 5. Target shape

### 5.1 Directory layout

```
crates/fresh-editor/src/model/buffer/
├── mod.rs                   TextBuffer struct + orchestrators + delegators (~800)
├── format.rs                BufferFormat + detect_* free fns               (~450)
├── file_kind.rs             BufferFileKind + is_binary static helpers      (~100)
├── persistence/
│   ├── mod.rs               Persistence struct + save-state transitions    (~250)
│   ├── load.rs              load_from_file, load_small/large, encoding ck  (~500)
│   ├── save.rs              save, save_to_file, finalize_*, consolidate_*  (~550)
│   ├── write_recipe.rs      WriteRecipe + build/stream/write_recipe_*      (~450)
│   ├── inplace.rs           should_use_inplace_write, write_data_inplace,
│   │                        recovery-meta, stream_file_to_writer           (~350)
│   └── snapshot.rs          saved_root management, diff_since_saved,
│                            rebuild_with_pristine_saved_root               (~450)
├── edits.rs                 insert_bytes/insert_at_position/delete*/replace_content,
│                            apply_bulk_edits, restore/snapshot_buffer_state (~450)
├── storage/
│   ├── mod.rs               get_text_range/_mut, slice_bytes, get_all_text,
│   │                        len, is_empty, to_string                       (~250)
│   ├── chunks.rs            prepare_viewport, chunk_split_and_load,
│   │                        ensure_chunk_loaded_at, OverlappingChunks,
│   │                        ChunkInfo, extend_streaming                    (~500)
│   └── line_scan.rs         prepare_line_scan, apply_scan_updates,
│                            piece_tree_leaves, scan_leaf, leaf_io_params,
│                            LineScanChunk                                  (~300)
├── search.rs                find_next*, find_regex*, find_pattern,
│                            search_scan_*, search_hybrid*, HybridSearchPlan,
│                            ChunkedSearchState, search_boundary_overlap    (~700)
├── replace.rs               replace_range/next/all/all_regex               (~130)
├── position.rs              offset_to_position, position_to_offset,
│                            position_to_line_col, line_col_to_position,
│                            lsp position ↔ byte, char/grapheme/word
│                            boundary, snap_to_char_boundary                (~550)
├── lines.rs                 get_line, line_start_offset, piece_info_at_offset,
│                            stats, resolve_line_byte_offset, line_iterator,
│                            iter_lines_from, get_line_number,
│                            estimated_line_length, LineNumber, LineData,
│                            TextBufferLineIterator                         (~400)
├── line_cache.rs            populate/get_cached/invalidate/handle_/clear   (~60)
└── tests/
    ├── mod.rs               shared fixtures: test_fs, text_with_newlines
    ├── edits.rs
    ├── position.rs
    ├── save_load.rs
    ├── search_replace.rs
    ├── line_endings.rs
    ├── binary_detection.rs
    └── property.rs          proptest scenarios (the Operation strategy)
```

17 new source files plus 8 test modules. No file exceeds ~700 lines.
The old monolithic `model/buffer.rs` no longer exists.

### 5.2 `TextBuffer` after composition

```rust
// model/buffer/mod.rs — the ONLY file with `impl TextBuffer`
pub struct TextBuffer {
    piece_tree:     PieceTree,
    buffers:        Vec<StringBuffer>,
    next_buffer_id: usize,

    persistence:    Persistence,
    format:         BufferFormat,
    file_kind:      BufferFileKind,

    version:        u64,
    config:         BufferConfig,
}

impl TextBuffer {
    // Construction (delegated to Persistence::load_* + small wrapping)
    pub fn new(_large_file_threshold: usize, fs: Arc<dyn FileSystem + ...>) -> Self { ... }
    pub fn from_bytes(content: Vec<u8>, fs: Arc<dyn FileSystem + ...>) -> Self { ... }
    pub fn load_from_file<P: AsRef<Path>>(path: P, fs: ...) -> Result<Self> { ... }

    // Orchestrators (touch 2+ sub-structs)
    pub fn save(&mut self) -> Result<()> { ... }
    pub fn insert_bytes(&mut self, offset: usize, text: Vec<u8>) -> Cursor { ... }
    pub fn apply_bulk_edits(&mut self, edits: &[(usize, usize, &str)]) -> isize { ... }

    // The one invariant choke-point
    fn mark_content_modified(&mut self) {
        self.persistence.mark_dirty();          // sets modified + recovery_pending
        self.version += 1;
    }

    // Delegators (public API preserved; one line each)
    pub fn encoding(&self) -> Encoding                { self.format.encoding() }
    pub fn set_encoding(&mut self, e: Encoding)       { self.mark_content_modified(); self.format.set_encoding(e) }
    pub fn is_modified(&self) -> bool                 { self.persistence.is_modified() }
    pub fn file_path(&self) -> Option<&Path>          { self.persistence.file_path() }
    pub fn is_binary(&self) -> bool                   { self.file_kind.is_binary() }
    // ... ~40 more one-line delegators
}
```

### 5.3 A representative sub-struct

```rust
// model/buffer/format.rs
pub struct BufferFormat {
    line_ending: LineEnding,
    original_line_ending: LineEnding,
    encoding: Encoding,
    original_encoding: Encoding,
}

impl BufferFormat {
    pub fn new(line_ending: LineEnding, encoding: Encoding) -> Self {
        Self { line_ending, original_line_ending: line_ending,
               encoding, original_encoding: encoding }
    }
    pub fn encoding(&self) -> Encoding { self.encoding }
    pub fn line_ending(&self) -> LineEnding { self.line_ending }
    pub fn set_encoding(&mut self, e: Encoding) { self.encoding = e; }
    pub fn set_line_ending(&mut self, le: LineEnding) { self.line_ending = le; }
    pub fn set_default_encoding(&mut self, e: Encoding) {
        self.encoding = e;
        self.original_encoding = e;
    }
    pub fn set_default_line_ending(&mut self, le: LineEnding) {
        self.line_ending = le;
        self.original_line_ending = le;
    }
    pub fn encoding_changed_since_load(&self) -> bool {
        self.encoding != self.original_encoding
    }
    pub fn line_ending_changed_since_load(&self) -> bool {
        self.line_ending != self.original_line_ending
    }
}

// Pure helpers — free functions, Rule 4
pub fn detect_line_ending(bytes: &[u8]) -> LineEnding { ... }
pub fn detect_encoding(bytes: &[u8]) -> Encoding { ... }
pub fn detect_encoding_or_binary(bytes: &[u8], truncated: bool) -> (Encoding, bool) { ... }
pub fn detect_and_convert_encoding(bytes: &[u8]) -> (Encoding, Vec<u8>) { ... }
pub fn convert_to_encoding(utf8: &[u8], target: Encoding) -> Vec<u8> { ... }
pub fn normalize_line_endings(bytes: Vec<u8>) -> Vec<u8> { ... }
pub(super) fn convert_line_endings_to(bytes: &[u8], target: LineEnding) -> Vec<u8> { ... }
```

No `TextBuffer` in any signature. All four `*_changed_since_load`
semantics, previously expressed inline in `build_write_recipe`, become
named methods on `BufferFormat`. Unit-testable without a filesystem.

### 5.4 Visibility table

| File set | May import | May NOT import |
|---|---|---|
| `format.rs`, `file_kind.rs` | stdlib, encoding crate | `TextBuffer`, `Persistence`, `PieceTree` |
| `persistence/*` | stdlib, `FileSystem` trait, `PieceTree` (only for `saved_root` type), `format` (read-only) | `TextBuffer` |
| `storage/*`, `search.rs`, `lines.rs`, `position.rs`, `replace.rs`, `line_cache.rs` | `PieceTree`, `StringBuffer`, the top-level struct fields they need as `&mut` args | `Persistence` mutation methods |
| `edits.rs` | everything above | — |
| `mod.rs` | everything under `model/buffer/` | — |

Enforced by one grep per row:

```
rg 'TextBuffer' crates/fresh-editor/src/model/buffer/format.rs  # → 0 hits
rg 'TextBuffer' crates/fresh-editor/src/model/buffer/persistence/  # → 0 hits
rg 'persistence::' crates/fresh-editor/src/model/buffer/storage/  # → 0 hits
```

## 6. Coordination mechanisms

Pick one of these three patterns per cross-sub-struct case. Don't add
a fourth.

### (a) Orchestrator with split borrows

For the handful of operations whose *whole purpose* is to combine
sub-structs. Lives on `TextBuffer`:

```rust
impl TextBuffer {
    pub fn save(&mut self) -> anyhow::Result<()> {
        let bytes = storage::serialize_all(
            &self.piece_tree, &self.buffers,
            self.format.line_ending(), self.format.encoding(),
        );
        let path = self.persistence.require_file_path()?;
        self.persistence.save_bytes(&path, bytes, &self.config)?;
        self.persistence.promote_to_saved(
            &self.piece_tree, self.file_kind.is_large_file(),
        );
        Ok(())
    }
}
```

Each line reads one sub-struct; the orchestrator is the only place
that touches several. Use this for: `save`, `load_from_file`,
`insert_bytes`, `delete`, `replace_content`, `apply_bulk_edits`,
`mark_content_modified`, `extend_streaming`, `restore_buffer_state`,
`snapshot_buffer_state`, `rebuild_with_pristine_saved_root`.

### (b) Borrowed parameters for read-many paths

When a sub-struct method needs *read-only* knowledge of another
sub-struct's state — common in save/load paths that need to know the
encoding and line-ending. Pass by `&`:

```rust
impl Persistence {
    pub fn build_write_recipe(
        &self,
        piece_tree: &PieceTree,
        buffers: &[StringBuffer],
        format: &BufferFormat,
        large_file: bool,
    ) -> io::Result<WriteRecipe> {
        // ...inline branches on format.line_ending_changed_since_load()...
    }
}
```

No back-pointer; every dependency visible in the signature. Use this
for every save/load helper that needs format/file-kind context.

### (c) Post-mutation notifications

When a sub-struct finishes work that implies a top-level invariant
change — e.g. `Persistence::save_bytes` completed, so the version
should bump. The sub-struct does not touch the flag; the orchestrator
does, *after* the sub-struct method returns:

```rust
pub fn save(&mut self) -> Result<()> {
    self.persistence.save_bytes(...)?;
    // Persistence has already updated its own modified/recovery_pending flags
    // internally; TextBuffer bumps version because that's a top-level concern.
    self.version += 1;
    Ok(())
}
```

Cross-sub-struct side-effects are always the orchestrator's
responsibility. Sub-structs do not reach.

### Decision rule

- Two or more sub-structs' **mutable** state involved → mechanism (a),
  an orchestrator on `TextBuffer`.
- One sub-struct mutates, another contributes **read-only** context →
  mechanism (b), `&` parameter.
- A sub-struct mutates and the top-level struct needs to react →
  mechanism (c), the orchestrator reacts after the call.
- Only one sub-struct's state involved → it's not cross-cutting; put
  the method on that sub-struct.

No `Rc<RefCell<TextBuffer>>`, no event bus, no `&mut self` where
`self` is the outer `TextBuffer` appearing inside a sub-struct method
signature. That's the shortlist.

## 7. Method-by-method mapping

Every method currently on `impl TextBuffer` appears in exactly one
row. Top-level types and free fns appear at the end of their section.

### 7.1 → `format.rs` (BufferFormat)

| Currently | Moves to |
|---|---|
| `line_ending`, `set_line_ending`, `set_default_line_ending` | `impl BufferFormat` methods |
| `encoding`, `set_encoding`, `set_default_encoding` | `impl BufferFormat` methods |
| `detect_line_ending` | free `pub fn` |
| `detect_encoding`, `detect_encoding_or_binary`, `detect_and_convert_encoding` | free `pub fn` |
| `convert_to_encoding`, `normalize_line_endings` | free `pub fn` |
| `convert_line_endings_to` | free `pub(super) fn` |
| `LineEnding` enum (currently L187–L215) | moves here |
| `set_encoding` / `set_line_ending` callers of `mark_content_modified` | delegator on `TextBuffer` flips the flag before/after |

### 7.2 → `file_kind.rs` (BufferFileKind)

| Currently | Moves to |
|---|---|
| `is_binary` | `impl BufferFileKind` |
| `is_large_file` | `impl BufferFileKind` |
| `has_line_feed_scan` | `impl BufferFileKind` |
| Internal setters for these three flags during load | `impl BufferFileKind` (`pub(super)`) |

### 7.3 → `persistence/` (Persistence sub-struct and its submodules)

**`persistence/mod.rs`** — struct + small state methods:

| Currently | Moves to |
|---|---|
| `filesystem`, `set_filesystem` | `impl Persistence` |
| `file_path`, `rename_file_path`, `clear_file_path` | `impl Persistence` |
| `is_modified`, `clear_modified`, `set_modified` | `impl Persistence` |
| `is_recovery_pending`, `set_recovery_pending` | `impl Persistence` |
| `original_file_size` | `impl Persistence` |
| `mark_dirty` (new — flips modified + recovery_pending) | `impl Persistence` |

**`persistence/load.rs`**:

| Currently | Moves to |
|---|---|
| `from_bytes_raw`, `from_bytes`, `from_bytes_with_encoding`, `from_str`, `empty` | `impl TextBuffer` constructors that call `persistence::load::*` helpers |
| `load_from_file`, `load_from_file_with_encoding` | `impl TextBuffer` (public) that call `persistence::load::*` helpers |
| `load_small_file` | free `pub(super) fn load_small_file(...)` |
| `load_large_file`, `load_large_file_confirmed`, `load_large_file_internal` | free `pub(super) fn` in `persistence/load.rs` |
| `check_large_file_encoding` | free `pub fn` (used by app layer) |
| `LargeFileEncodingConfirmation` type | moves here |

**`persistence/save.rs`**:

| Currently | Moves to |
|---|---|
| `save`, `save_to_file`, `finalize_external_save` | `impl TextBuffer` orchestrators (mechanism a) |
| `finalize_save` | `impl Persistence` |
| `consolidate_after_save`, `consolidate_large_file`, `consolidate_small_file` | `impl Persistence` (take `&mut PieceTree, &mut Vec<StringBuffer>` as args — mechanism b) |
| `make_sudo_error`, `SudoSaveRequired` | moves here |

**`persistence/write_recipe.rs`**:

| Currently | Moves to |
|---|---|
| `WriteRecipe`, `RecipeAction`, `to_write_ops`, `has_copy_ops`, `flatten_inserts` | moves here verbatim |
| `build_write_recipe` | free `pub(super) fn build_write_recipe(piece_tree, buffers, format, file_kind, saved_*)` |
| `write_recipe_to_file` | free `pub(super) fn` |

**`persistence/inplace.rs`**:

| Currently | Moves to |
|---|---|
| `should_use_inplace_write` | free `pub(super) fn` |
| `create_temp_file`, `create_recovery_temp_file` | free `pub(super) fn` |
| `inplace_recovery_meta_path`, `write_inplace_recovery_meta` | free `pub(super) fn` |
| `save_with_inplace_write`, `write_data_inplace`, `stream_file_to_writer` | free `pub(super) fn` |

**`persistence/snapshot.rs`**:

| Currently | Moves to |
|---|---|
| `mark_saved_snapshot`, `refresh_saved_root_if_unmodified` | `impl Persistence` (take `&PieceTree` arg) |
| `apply_chunk_load_to_saved_root` | `impl Persistence` |
| `diff_since_saved`, `verify_content_differs_in_ranges`, `extract_range_from_tree`, `collect_range_from_node`, `tree_total_bytes`, `diff_trees_by_structure` | `impl Persistence` (read-only, take `&PieceTree`) |
| `get_recovery_chunks` | `impl Persistence` |
| `rebuild_with_pristine_saved_root` | `impl TextBuffer` orchestrator (touches `Persistence` + `piece_tree` + `buffers`) |

### 7.4 → `edits.rs`

`edits.rs` contains orchestrators on `impl TextBuffer` (mechanism a).
Every edit calls `mark_content_modified`.

| Currently | Moves to |
|---|---|
| `insert_bytes`, `try_append_to_existing_buffer`, `insert`, `insert_at_position` | `impl TextBuffer` (orchestrators) |
| `delete_bytes`, `delete`, `delete_range` | `impl TextBuffer` |
| `replace_content` | `impl TextBuffer` |
| `apply_bulk_edits` | `impl TextBuffer` |
| `restore_buffer_state`, `snapshot_buffer_state` | `impl TextBuffer` (touches storage + persistence) |
| `BufferSnapshot` struct | stays near, in `edits.rs` or moves to `mod.rs` |

### 7.5 → `storage/`

**`storage/mod.rs`**:

| Currently | Moves to |
|---|---|
| `get_text_range` | `impl TextBuffer` (`pub(crate)` or delegator) |
| `get_text_range_mut` | `impl TextBuffer` (orchestrator — may need `chunk_split_and_load`) |
| `get_all_text`, `get_all_text_string`, `slice_bytes`, `to_string` | `impl TextBuffer` (all pure reads over `piece_tree`+`buffers`) |
| `len`, `is_empty`, `total_bytes`, `line_count` | `impl TextBuffer` |
| `buffer_slice` | `impl TextBuffer` |

**`storage/chunks.rs`**:

| Currently | Moves to |
|---|---|
| `prepare_viewport`, `chunk_split_and_load`, `ensure_chunk_loaded_at` | `impl TextBuffer` (orchestrators — touch persistence for `fs`) |
| `extend_streaming` | `impl TextBuffer` orchestrator |
| `ChunkInfo`, `OverlappingChunks` (L4293–L4540) | moves here |

**`storage/line_scan.rs`**:

| Currently | Moves to |
|---|---|
| `prepare_line_scan`, `piece_tree_leaves`, `scan_leaf`, `leaf_io_params`, `apply_scan_updates` | `impl TextBuffer` |
| `LineScanChunk` type | moves here |

### 7.6 → `search.rs`

| Currently | Moves to |
|---|---|
| `find_next`, `find_next_in_range`, `find_pattern`, `find_in_bytes` | `impl TextBuffer` + free `find_in_bytes` |
| `find_next_regex`, `find_next_regex_in_range`, `find_regex` | `impl TextBuffer` |
| `search_scan_init`, `search_scan_next_chunk`, `search_scan_all` | `impl TextBuffer` |
| `search_hybrid_plan`, `search_hybrid` | `impl TextBuffer` |
| `HybridSearchPlan`, `HybridSearchPlan::execute` | moves here |
| `ChunkedSearchState` | moves here |
| free `search_boundary_overlap` (L4722) | moves here |

### 7.7 → `replace.rs`

| Currently | Moves to |
|---|---|
| `replace_range`, `replace_next`, `replace_all`, `replace_all_regex` | `impl TextBuffer` orchestrators (delete + insert, bump modified) |

### 7.8 → `position.rs`

| Currently | Moves to |
|---|---|
| `offset_to_position`, `position_to_offset` | `impl TextBuffer` |
| `position_to_line_col`, `line_col_to_position` | `impl TextBuffer` |
| `position_to_lsp_position`, `lsp_position_to_byte` | `impl TextBuffer` |
| `prev_char_boundary`, `next_char_boundary`, `snap_to_char_boundary`, `is_utf8_continuation_byte` | `impl TextBuffer` + one free pure fn |
| `prev_grapheme_boundary`, `next_grapheme_boundary` | `impl TextBuffer` |
| `prev_word_boundary`, `next_word_boundary` | `impl TextBuffer` |

### 7.9 → `lines.rs`

| Currently | Moves to |
|---|---|
| `get_line`, `line_start_offset`, `piece_info_at_offset`, `stats` | `impl TextBuffer` |
| `resolve_line_byte_offset` | `impl TextBuffer` |
| `line_iterator`, `iter_lines_from` | `impl TextBuffer` |
| `get_line_number`, `estimated_line_length` | `impl TextBuffer` |
| `LineNumber` enum (L276–L313) | moves here |
| `LineData` (L7867), `TextBufferLineIterator` (L7880–end) | moves here |

### 7.10 → `line_cache.rs`

| Currently | Moves to |
|---|---|
| `populate_line_cache`, `get_cached_byte_offset_for_line`, `invalidate_line_cache_from`, `handle_line_cache_insertion`, `handle_line_cache_deletion`, `clear_line_cache` | `impl TextBuffer` — these are ~all no-ops today (remnants of an earlier cache). Consider a follow-up to delete them outright, but not in this refactor. |

### 7.11 → `mod.rs` (stays on `TextBuffer`)

| Currently | Stays on `impl TextBuffer` |
|---|---|
| `new`, `new_with_path` | Constructor (composes all sub-structs) |
| `version`, `bump_version` | Top-level counter (§3.2) |
| `mark_content_modified` | Invariant choke-point (Rule 3) |
| `from_str_test`, `new_test` | Test helpers, stay near struct |
| `BufferConfig`, `Default for BufferConfig` | stays (or moves to `mod.rs`) |

### 7.12 Tests

The inline test mods at L4779–L7866 split roughly as follows. Each
`tests/<topic>.rs` is `#[cfg(test)] mod <topic>` with `use super::*;`:

| Current test range | New file |
|---|---|
| Buffer-empty / multiline / insert / delete basics (L4790–L5600 area) | `tests/edits.rs` |
| Offset↔position, LSP position, line iterator cross-ref (L5638–L5904 area) | `tests/position.rs` |
| Line-ending detect / normalize (L5904–L5940 area) | `tests/line_endings.rs` |
| `get_all_text` returns empty for unloaded (L5940 area) | `tests/save_load.rs` |
| Proptest `operation_strategy`, `text_with_newlines` (L7443 onward) | `tests/property.rs` (+ shared helpers in `tests/mod.rs`) |
| Binary detection (L7780–L7866) | `tests/binary_detection.rs` |
| `apply_recipe` helper (L7756–L7779) | `tests/mod.rs` as shared helper |

Shared helpers (`test_fs`, fixtures) consolidate into `tests/mod.rs`.

## 8. Handling the realities

Four aspects of the current code need an explicit plan, because a
naive move will either not compile or will silently break invariants.

### 8.1 Borrow checker

Most orchestrators need `&mut` to two sub-structs at once. Rust's
split-borrow rules permit this when the outer type is destructured:

```rust
pub fn save(&mut self) -> Result<()> {
    let TextBuffer { persistence, piece_tree, buffers,
                     format, file_kind, config, .. } = self;
    let recipe = persistence::write_recipe::build(
        piece_tree, buffers, format, file_kind,
        persistence.saved_root(), persistence.saved_file_size(),
    )?;
    persistence.write_recipe_to_disk(recipe, config)?;
    persistence.promote_to_saved(piece_tree, file_kind.is_large_file());
    Ok(())
}
```

Two cases where this doesn't work and need extra care:

- **Chunked load during read.** `get_text_range_mut` currently calls
  `ensure_chunk_loaded_at`, which mutates both `piece_tree` and
  `buffers` *and* reads `persistence.fs`. It can't be a
  `Persistence::load_chunk(&mut self, piece_tree: &mut PieceTree,
  buffers: &mut Vec<StringBuffer>)` because `fs` lives in `Persistence`
  and the same method needs `&` access to it while mutating two
  externals. Resolution: `fn load_chunk(fs: &dyn FileSystem,
  piece_tree: &mut PieceTree, buffers: &mut Vec<StringBuffer>, ...)`
  as a free function in `persistence/load.rs` — take `fs` as a borrow
  off the caller's destructured `TextBuffer`.
- **Consolidate after save.** `consolidate_after_save` both mutates
  `persistence` (updates `saved_root`, `saved_file_size`, `modified`)
  and resets `piece_tree` + `buffers`. Same destructure pattern as
  above.

### 8.2 Cross-cutting orchestrators

Three methods touch several sub-structs and deserve individual
plans.

**`save` (+ `save_to_file`, `finalize_external_save`).** Current:
~70 lines; builds `WriteRecipe`, picks inplace vs temp, writes,
finalises, consolidates. Target shape (§6a): ~15-line orchestrator
that destructures once and calls four `persistence::*` free functions
in sequence. Size gain is in the move, not a rewrite.

**`load_from_file` (+ `_with_encoding` variants).** Current:
`load_from_file` dispatches to `load_small_file` or
`load_large_file`, both of which construct a `TextBuffer` from
scratch. Target: `load_from_file` is still a `TextBuffer::`
constructor that calls `persistence::load::small` or
`persistence::load::large`, each of which returns a tuple
`(PieceTree, Vec<StringBuffer>, BufferFormat, BufferFileKind,
Persistence)` to assemble. No behaviour change.

**`insert_bytes` (and its siblings `insert_at_position`, `delete_bytes`,
`replace_content`).** Current: mutates `piece_tree`, possibly mutates
`buffers` (new string-buffer insertion), calls `mark_content_modified`.
Target: same, but `mark_content_modified` now flips `persistence.*`
flags (via `Persistence::mark_dirty()`) + bumps `self.version`, and
the storage-touching part destructures out `piece_tree` and `buffers`.

No method exceeds the complexity of the orchestrators in the
editor-modules plan. This is the easy part of the refactor.

### 8.3 Implicit invariants that must survive

The monolithic impl holds four invariants implicitly. Each must have
exactly one named choke-point after the refactor.

- **"Any mutation flips modified + recovery_pending + bumps version."**
  Today: `mark_content_modified()` (called from 10 sites). Post-refactor:
  same method, now on `impl TextBuffer` in `mod.rs`, calls
  `self.persistence.mark_dirty()` + `self.version += 1`. No sub-struct
  may flip `modified` or `recovery_pending` on its own. Enforced by
  grep (Rule 3).
- **"`original_*` formats are snapshots from load time."** Set by
  `load_*` and `save` (via `consolidate_after_save`). Post-refactor:
  `BufferFormat::new` takes both the current and original in the
  initialiser; `consolidate_after_save` calls
  `format.promote_current_to_original()`. Every other format-setter
  leaves `original_*` alone.
- **"`saved_root` + `saved_file_size` + `buffers` are consistent after
  save."** Today: `consolidate_large_file` / `consolidate_small_file`
  reconstruct `buffers` atomically. Post-refactor: one method
  `Persistence::promote_to_saved(&mut piece_tree, &mut buffers,
  is_large: bool)`, called once from `save()`.
- **"`version` is monotonic."** Today: `bump_version` is private and
  only called by `mark_content_modified`. Post-refactor: `version`
  stays on `TextBuffer` and is touched only by
  `mark_content_modified`. Enforced by grep.

Make each invariant's choke-point a `#[doc(hidden)]`-style comment
that names the invariant by name, so a future reader can't
accidentally duplicate the logic.

### 8.4 Coexistence during migration

Each phase introduces a sub-struct behind delegators before removing
the old fields. Example within Phase 2 (extract `BufferFormat`):

1. **Commit A.** Add `format: BufferFormat` to `TextBuffer`
   initialised from the four existing fields. Delegators
   (`buffer.encoding()`) return `self.format.encoding()`; but the
   four raw fields also still exist and are kept in sync in the
   constructor. File compiles, tests pass, behaviour unchanged.
2. **Commit B.** Move every internal read of
   `self.encoding`/`self.line_ending`/`self.original_*` to
   `self.format.encoding()` etc. Still compiles with redundant state.
3. **Commit C.** Move every internal write of those fields to
   `self.format.set_*()`. Still redundant.
4. **Commit D.** Delete the four raw fields from `TextBuffer`.
   Constructor only initialises `self.format`. Tests pass.

Between commits A and D the two representations coexist on `main`.
That's the price of not having a flag day.

## 9. Phased execution

Nine phases. Each lands as a single PR. Every commit within a phase
compiles and passes `cargo test -p fresh-editor`. Phases can be
reordered slightly, but Phase 1 must come first and Phase 9 must
come last.

### Phase 1 — Convert file to directory; no behaviour change

- `git mv crates/fresh-editor/src/model/buffer.rs
         crates/fresh-editor/src/model/buffer/mod.rs`
- Verify build + tests green.

**Risk:** trivial. **Blast radius:** module resolution only.

### Phase 2 — Extract `BufferFormat` (easiest cluster)

4 fields, 24 accesses, minimal coupling. Establishes the
sub-struct + delegator pattern.

- Commit A: Add `format: BufferFormat` field, initialised to same
  values; keep the four raw fields in sync in the constructor.
- Commit B: Route internal reads of `self.{line_ending, encoding,
  original_*}` through `self.format.*()`.
- Commit C: Route internal writes through `self.format.set_*()`.
- Commit D: Delete the four raw fields.
- Commit E: Move `detect_line_ending`, `detect_encoding*`,
  `convert_*`, `normalize_line_endings`, `convert_line_endings_to`,
  `LineEnding` enum to `model/buffer/format.rs` as free fns.
- Commit F: Move the `BufferFormat` struct + delegators into
  `model/buffer/format.rs`.

**Risk:** low. **Blast radius:** none external (accessors preserved).
**Test coverage:** existing `test_detect_crlf`, `test_detect_lf`,
`test_normalize_*`.

### Phase 3 — Extract `BufferFileKind`

3 fields, 14 accesses. Same A-through-F shape as Phase 2 but smaller.

**Risk:** trivial. **Blast radius:** none external.

### Phase 4 — Extract `Persistence` (largest cluster)

6 fields, 78 accesses, plus the `mark_content_modified` invariant.
Do this one field at a time:

- Commit A: Introduce `Persistence` with only `fs`. Route accesses.
  Delete raw `fs`.
- Commit B: Add `file_path`. Route. Delete raw.
- Commit C: Add `modified` + `recovery_pending` together (they move
  as a unit because `mark_dirty` touches both). Introduce
  `Persistence::mark_dirty`. Route `mark_content_modified` to it.
  Delete raw.
- Commit D: Add `saved_root` + `saved_file_size` together. Route.
  Delete raw.
- Commit E: Now that all six fields live on `Persistence`, split the
  impl across `persistence/{mod,load,save,write_recipe,inplace,
  snapshot}.rs`. Free fns where they belong; methods where they
  belong. Orchestrators stay on `impl TextBuffer`.

**Risk:** medium. **Blast radius:** internal only — but the biggest
internal move in the plan. **Test coverage:** recovery integration
tests (`tests/e2e/recovery.rs`), large-file tests
(`tests/e2e/large_file_*.rs`). Run these between every commit.

### Phase 5 — Split leaf concerns (no new sub-structs)

With the three sub-structs in place, carve off the pure-storage-core
concerns into their own files. These stay as `impl TextBuffer`
methods but in dedicated files:

- Commit A: `position.rs` (position/boundary methods; largest cluster
  that's self-contained).
- Commit B: `lines.rs` + move `LineNumber`, `LineData`,
  `TextBufferLineIterator`.
- Commit C: `replace.rs` (4 methods).
- Commit D: `line_cache.rs` (6 near-no-op methods).
- Commit E: `search.rs` + move `HybridSearchPlan`, `ChunkedSearchState`,
  `search_boundary_overlap`.

Each commit moves one file's worth of methods and re-runs tests.

**Risk:** low. **Blast radius:** internal only. Public API preserved
since these are still methods on `TextBuffer`.

### Phase 6 — Split storage concerns

- Commit A: `storage/mod.rs` — pure reads (`get_all_text`, `len`, etc.).
- Commit B: `storage/chunks.rs` + move `ChunkInfo`,
  `OverlappingChunks`, `extend_streaming`.
- Commit C: `storage/line_scan.rs` + move `LineScanChunk`.

**Risk:** low-medium. `chunk_split_and_load` is the trickiest method
in the file (~155 lines). **Test coverage:**
`tests/e2e/large_file_inplace_write_bug.rs` is the canary.

### Phase 7 — Consolidate edits

- Commit: Move edit orchestrators (`insert_bytes`, `insert_at_position`,
  `delete_bytes`, `delete`, `delete_range`, `replace_content`,
  `apply_bulk_edits`, `restore_buffer_state`, `snapshot_buffer_state`,
  `BufferSnapshot`) into `edits.rs`.

**Risk:** low. **Blast radius:** internal — but edits are the hottest
path, so run the property-based tests.

### Phase 8 — Redistribute tests

Inline tests move to `tests/` submodules in the order below. Each
commit moves one topic and its fixtures:

- Commit A: `tests/mod.rs` with shared helpers (`test_fs`, `apply_recipe`,
  fixture generators).
- Commit B: `tests/edits.rs` (largest topic).
- Commit C: `tests/position.rs`.
- Commit D: `tests/save_load.rs`.
- Commit E: `tests/line_endings.rs`.
- Commit F: `tests/binary_detection.rs`.
- Commit G: `tests/property.rs` (proptest).
- Commit H: Delete the now-empty inline `mod tests` blocks.

**Risk:** trivial per commit. **Blast radius:** none (tests are
internal).

### Phase 9 — Audit and cleanup

- Run each grep audit from §4 and §5.4; fix any stragglers.
- Confirm every file is under 700 lines.
- Delete any `pub` that should be `pub(super)` after the move.
- Consider following up on the near-no-op `line_cache` methods in a
  separate PR (not in this refactor).

**Risk:** trivial. **Blast radius:** none.

### Expected outcome

| | Before | After |
|---|---:|---:|
| Files in `model/buffer*` | 1 (buffer.rs) | ~17 source + 8 test |
| Largest single source file | 8,029 | ≤ 700 |
| `TextBuffer` field count | 17 flat | 8 composed |
| Methods in one `impl TextBuffer` block | ~120 | ~40 (orchestrators + delegators) |
| `impl TextBuffer` blocks crate-wide | 1 | 1 (unchanged — Rule 1) |
| Test blocks | 2 inline monoliths | 8 topic files |

## 10. Success criteria

Mechanically checkable. Each of these must pass before merging the
final phase.

**A. Single `impl TextBuffer` (Rule 1).**

```
$ rg -n '^impl TextBuffer\b' crates/fresh-editor/src/
crates/fresh-editor/src/model/buffer/mod.rs:<line>:impl TextBuffer {
```

Exactly one line. Nothing else.

**B. No raw-field leakage per extracted sub-struct.**

For each `(field, owner)` pair, the raw field name may appear only
inside the owner's module:

```
$ rg -n '\.line_ending\b|\.encoding\b|\.original_line_ending\b|\.original_encoding\b' \
      crates/fresh-editor/src/model/buffer/ \
      | rg -v '^crates/fresh-editor/src/model/buffer/format\.rs'
# → empty

$ rg -n '\.large_file\b|\.line_feeds_scanned\b|\.is_binary\b' \
      crates/fresh-editor/src/model/buffer/ \
      | rg -v '^crates/fresh-editor/src/model/buffer/file_kind\.rs'
# → empty

$ rg -n '\.(fs|file_path|modified|recovery_pending|saved_root|saved_file_size)\b' \
      crates/fresh-editor/src/model/buffer/ \
      | rg -v '^crates/fresh-editor/src/model/buffer/persistence/'
# → empty (except the destructure pattern in mod.rs)
```

**C. Only `mark_content_modified` writes `modified`, `recovery_pending`,
`version` (Rule 3).**

```
$ rg -n '\.modified\s*=|\.recovery_pending\s*=|self\.version\s*(\+=|=)' \
      crates/fresh-editor/src/model/buffer/
```

Every hit must be inside `mod.rs::mark_content_modified` or
`persistence/mod.rs::mark_dirty` (the latter is the only path
`mark_content_modified` takes). No other hits.

**D. No `TextBuffer` in sub-struct signatures (Rule 2).**

```
$ rg -n '\bTextBuffer\b' crates/fresh-editor/src/model/buffer/format.rs
# → empty

$ rg -n '\bTextBuffer\b' crates/fresh-editor/src/model/buffer/file_kind.rs
# → empty

$ rg -n '\bTextBuffer\b' crates/fresh-editor/src/model/buffer/persistence/
# → empty
```

**E. File-size cap.** No file in `model/buffer/` exceeds 700 lines:

```
$ find crates/fresh-editor/src/model/buffer -name '*.rs' -exec wc -l {} + \
    | awk '$1 > 700 { print }'
# → empty
```

**F. Public API preserved.** Every method that was callable as
`TextBuffer::foo` / `buffer.foo()` before the refactor is still
callable with the same signature. The 18 external files under
`crates/fresh-editor/src/` and the 7 external test files must
compile without modification.

Acceptance: `cargo check -p fresh-editor && cargo test -p fresh-editor`
passes without any change outside `crates/fresh-editor/src/model/buffer/`.

**G. Tests green at every phase boundary.** Not just at the end. Each
phase's final commit must be a green CI run.

## 11. Risks & mitigations

**R1: `consolidate_after_save` silently corrupts recovery state.**
The current method reconstructs `buffers` and updates `saved_root`
atomically. If the new `Persistence::promote_to_saved` is split across
multiple calls or the ordering changes, a crash between calls could
leave recovery in a torn state. **Mitigation:** keep the method atomic
— it's one call from `save()` that takes `&mut PieceTree,
&mut Vec<StringBuffer>` as arguments. Run
`tests/e2e/recovery.rs` and `tests/e2e/large_file_inplace_write_bug.rs`
between every commit in Phase 4.

**R2: `mark_content_modified` is skipped by a new path.** Today every
mutating method explicitly calls it. Post-refactor, if a sub-struct
gains a setter and forgets the delegator pattern, an edit can land
without bumping `version` — and LSP change-tracking would go stale.
**Mitigation:** Rule 3's grep audit. Plus: sub-structs never expose
public mutators for the flagged fields; only `TextBuffer`'s
orchestrators mutate them.

**R3: Borrow checker stalls in orchestrators.** Some of the
edit-with-chunk-load methods need simultaneous `&mut` on three things
and read on a fourth. **Mitigation:** the destructure-and-free-fn
pattern (§8.1) is the escape hatch. If it can't be made to work for a
specific method, convert that method to a free function taking all
its deps by mutable reference and call it from the orchestrator.

**R4: Tests break silently when split.** Inline `mod tests` can
reference private fields via `use super::*`. After extraction, a test
that relied on `buffer.encoding` (field access) instead of
`buffer.encoding()` (method) will fail to compile. **Mitigation:** run
tests after each commit in Phase 2-4 and fix any test-side
field-access on the spot, not in a later phase.

**R5: Line-cache methods are near-no-ops and hide a half-finished
feature.** The 6 `*_line_cache_*` methods mostly do nothing
(`populate_line_cache` takes args it doesn't use). Extracting them
into a file preserves dead code. **Mitigation:** do the extraction
mechanically in Phase 5 (preserve behaviour), and file a follow-up
issue "remove or finish line cache" outside this refactor. Don't
conflate cleanup with restructuring.

**R6: Encoding detection's `detect_encoding_or_binary` sets the
`is_binary` flag that `BufferFileKind` will own.** Today the caller in
`from_bytes_raw` sets `self.is_binary` directly. Post-refactor the
detection is a free function (returns `(Encoding, bool)`), and the
constructor sets `file_kind.is_binary` from the returned bool.
**Mitigation:** verified by the binary-detection test suite
(`test_detect_binary_*`), which must pass after Phase 3.

**R7: The refactor balloons beyond scope.** The `storage::chunks`
module touches the piece-tree API; it's tempting to "clean up" the
piece-tree interface while here. **Mitigation:** refuse all grooming.
If a method's body can be moved byte-for-byte, do that; otherwise
defer to a follow-up PR.
