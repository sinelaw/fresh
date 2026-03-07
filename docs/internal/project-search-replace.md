# Project-Wide Search & Replace

> **Status**: Implementation Plan
> **Date**: March 2026

## Motivation

The current `search_replace` plugin shells out to `git grep` for search and uses `std::fs::read_to_string` / `std::fs::write` for replacement. This has several critical problems:

1. **Bypasses the `FileSystem` trait** — both search and replace use local-only I/O. The editor's SSH/remote filesystem abstraction is completely sidestepped. The plugin is broken on remote connections.
2. **Bypasses the buffer model** — replacements read/write raw files instead of editing through the piece tree. Open buffers with unsaved changes are silently overwritten. There is no undo. Encoding and line endings are mangled (`split("\n")` destroys CRLF).
3. **Search results can be stale** — `git grep` searches on-disk content. If a buffer has unsaved edits, the results show the saved state, not what the user sees. Line numbers and byte offsets don't match the live buffer, so replacements target the wrong locations.
4. **No large-file support** — unopened files are read entirely into memory for replacement. There is no streaming, no chunked processing.

The editor already has all the machinery to solve these problems. The buffer's piece tree supports chunked streaming search (`OverlappingChunks`, `process_search_scan_batch`), lazy loading through the `FileSystem` trait, and incremental non-blocking scanning. The goal is to reuse this infrastructure for project-wide search, adding only the file-tree dispatch layer.

## Architectural Principles

**1. Everything goes through existing abstractions**

The `FileSystem` trait handles local, remote (SSH), and test filesystem backends. The `TextBuffer` handles encoding, line endings, lazy loading, piece tree editing, and undo. The plugin API exposes `deleteRange`, `insertText`, and `openFile`. New code should compose these — not reimplement them.

**2. Search the content the user sees**

If a file is open with dirty edits, search must run against the buffer's piece tree (which reflects the unsaved state). If a file is not open, search runs against the on-disk content via the `FileSystem` trait. This matches VS Code's behavior and eliminates stale-result bugs.

**3. Large files are not special-cased**

The same code path handles small and large files. `TextBuffer::load_large_file` creates a piece tree with unloaded chunks (file offset references, no I/O). `get_text_range_mut` lazy-loads chunks on demand through the `FileSystem` trait. `process_search_scan_batch` processes a few chunks per `editor_tick`. The file is never fully in memory. This already works — project-wide search just needs to create these lightweight buffers for unopened files.

**4. The plugin is UI-only**

The plugin handles the panel, keybindings, selection checkboxes, and user prompts. Search and replace logic lives in Rust, exposed as two plugin API methods. The plugin never touches file I/O or buffer internals.

## Design

### Search: `editor.grepProject(pattern, opts)`

New Rust-side API exposed to plugins.

**Input:**
```typescript
interface GrepOpts {
  fixedString?: boolean;   // -F (literal) vs regex
  caseSensitive?: boolean;
  maxResults?: number;
  // Future: glob include/exclude filters
}
```

**Output:**
```typescript
interface GrepMatch {
  file: string;        // Absolute path
  bufferId: number;    // >0 if file is open in a buffer, 0 if not
  byteOffset: number;  // Match start in file/buffer content
  length: number;      // Match length in bytes
  line: number;        // 1-indexed line number
  column: number;      // 1-indexed column
  context: string;     // The matched line (for display)
}
```

**Implementation (Rust side):**

1. Walk the project file tree via `FileSystem::read_dir` (recursive). Respect `.gitignore` if available (reuse existing ignore infrastructure or call `git ls-files` as a fast path for git repos).

2. For each file path, dispatch:

| File state | Search strategy |
|---|---|
| Open in a buffer (dirty or clean) | Search the existing `TextBuffer`'s piece tree. Use `prepare_line_scan` + the same chunk-based regex scan that `process_search_scan_batch` uses. Dirty edits are automatically included. |
| Not open, small file (< large-file threshold) | Read via `FileSystem::read_file`. Search the raw bytes using `TextBuffer::find_in_bytes` (literal) or `regex::find_iter` (regex). No buffer created. |
| Not open, large file (>= threshold) | Create a lightweight `TextBuffer` via `load_large_file` — records piece tree references to the file, no I/O. Run `prepare_line_scan` + incremental chunk scan. Chunks are lazy-loaded via `FileSystem::read_range`, searched, and discarded. File is never fully in memory. Drop the temporary buffer after scanning. |

3. Collect `GrepMatch` results up to `maxResults`. For each match, compute line/column by counting newlines in the scanned content (already available from the chunk processing).

4. The scan is incremental and non-blocking — process a batch of files/chunks per `editor_tick`, report progress to the plugin via a callback or status update. This matches the existing single-buffer `SearchScanState` pattern.

### Replace: `editor.replaceInBuffer(bufferId, matches, replacement)`

New Rust-side API exposed to plugins.

**Input:**
```typescript
interface ReplaceMatch {
  byteOffset: number;
  length: number;
}
```

**Implementation (Rust side):**

1. If the file is not open, `openFile` to create a buffer (lazy-loads via FS trait).

2. Sort matches by `byteOffset` descending — editing from the end backwards prevents earlier edits from shifting later offsets.

3. For each match: `delete_range(offset, offset + length)` then `insert_text(offset, replacement)`. These are the existing `TextBuffer` methods that update the piece tree, markers, and overlays.

4. Group all edits as a single undo action (the buffer's undo system already supports grouping — same mechanism used by multi-cursor edits).

5. Save the buffer via `executeAction("save")`, which goes through `FileSystem::write_patched` — the optimized path that sends only changed chunks to the remote host over SSH.

### Plugin changes

The `search_replace.ts` plugin becomes a thin UI layer:

```
User triggers command
  → prompt for search pattern
  → prompt for replacement text
  → call editor.grepProject(pattern, opts)
  → display results in virtual buffer panel (existing code)
  → user selects/deselects matches, presses Enter
  → for each file with selected matches:
      call editor.replaceInBuffer(bufferId, selectedMatches, replacement)
  → close panel, report results
```

The plugin no longer calls `spawnProcess`, `readFile`, or `writeFile`. It only manages the UI.

## What's reused vs new

### Reused (zero modifications)

| Component | Location | What it does |
|---|---|---|
| `TextBuffer::find_in_bytes` | `buffer.rs:3287` | Literal byte pattern search |
| `TextBuffer::find_pattern` | `buffer.rs:3256` | Chunked literal search via `OverlappingChunks` |
| `TextBuffer::find_regex` | `buffer.rs:3338` | Chunked regex search via `OverlappingChunks` |
| `OverlappingChunks` | `buffer.rs:3991` | Streaming piece tree iterator with cross-boundary overlap |
| `TextBuffer::get_text_range_mut` | `buffer.rs:2175` | Lazy-loads unloaded chunks on demand via `FileSystem` trait |
| `TextBuffer::load_large_file` | `buffer.rs:683` | Creates piece tree with unloaded chunk references (no I/O) |
| `prepare_line_scan` | `buffer.rs:2638` | Splits piece tree into scan chunks |
| `process_search_scan_batch` pattern | `buffer_management.rs:2724` | Incremental chunk-based regex scan with overlap handling |
| `FileSystem::read_dir` | `filesystem.rs` | Directory listing (local + SSH) |
| `FileSystem::read_file` | `filesystem.rs:314` | File reading (local + SSH) |
| `FileSystem::read_range` | `filesystem.rs:317` | Partial file reading for lazy loading |
| `FileSystem::write_patched` | `filesystem.rs:363` | Optimized save — sends only diffs to remote |
| `TextBuffer::insert_text` | `buffer.rs` | Piece tree insert |
| `TextBuffer::delete_range` | `buffer.rs` | Piece tree delete |
| Buffer undo grouping | `buffer.rs` | Groups multiple edits as single undo action |

### New code

| Component | Estimated size | What it does |
|---|---|---|
| `grep_project` dispatcher | ~120 lines | Walks file tree, checks open buffers, dispatches to appropriate search strategy per file |
| `GrepProjectScanState` | ~40 lines | Tracks incremental multi-file scan progress (analogous to existing `SearchScanState`) |
| `replace_in_buffer` | ~30 lines | Sorts matches descending, applies `delete_range` + `insert_text` in undo group |
| Plugin API bindings | ~60 lines | `ts_export.rs` + `quickjs_backend.rs` glue for the two new methods |
| Plugin rewrite | ~-100 lines (net reduction) | Remove `git grep`/`readFile`/`writeFile` logic, call new APIs instead |

## Comparison with other editors

| | VS Code | Vim/Neovim | JetBrains | fresh (proposed) |
|---|---|---|---|---|
| Search engine | ripgrep on extension host | External tool | PSI index | Reuse existing `TextBuffer` search infra via `FileSystem` trait |
| Dirty buffer handling | Search in-memory document | Doesn't (user saves first) | PSI tree is always current | Search piece tree (dirty edits included) |
| Large file search | ripgrep streams | External tool streams | Pre-built index | `load_large_file` + incremental chunk scan (existing code) |
| Remote/SSH | Extension host runs on remote | Neovim remote | Gateway on remote | `FileSystem` trait (existing abstraction) |
| Replace mechanism | `WorkspaceEdit` on document model | `:cdo s///` on buffer | Write action on PSI | `deleteRange` + `insertText` on buffer (existing ops) |
| Undo | Single undo group | Per-buffer | Single action | Per-buffer undo group (existing infra) |
| Encoding safety | Document model | Buffer `fileformat` | VFS | Buffer handles encoding on load (existing) |
| Net new code | Large (WorkspaceEdit, TextSearchProvider) | Minimal (`:cdo` is built-in) | Large (PSI, index) | ~250 lines of Rust + plugin simplification |
