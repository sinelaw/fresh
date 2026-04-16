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
