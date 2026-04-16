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
