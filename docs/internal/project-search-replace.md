# Project-Wide Search & Replace

> **Status**: Feature-complete — needs tests and polish
> **Date**: March 2026

## Motivation

The original `search_replace` plugin shelled out to `git grep` for search and used raw `std::fs` for replacement. This bypassed the `FileSystem` trait (broken on SSH), bypassed the buffer model (no undo, stale results, encoding mangling), and had no large-file support.

The editor already has all the machinery to solve these problems: piece tree chunked search, lazy loading through the `FileSystem` trait, and incremental non-blocking scanning.

## Architecture

### Principles

1. **Everything goes through existing abstractions** — `FileSystem` trait for I/O, `TextBuffer` for search/edit, plugin API for UI.
2. **Search the content the user sees** — dirty buffers search the piece tree; unopened files search via `FileSystem`.
3. **Large files are not special-cased** — same `TextBuffer` code path handles small and large files. `load_large_file` creates lazy piece tree nodes; `get_text_range_mut` loads chunks on demand; chunks are searched and can be discarded.
4. **The plugin is UI-only** — panel, keybindings, selection, display. No file I/O.

### Search flow

```
Plugin: editor.grepProjectStreaming(pattern, opts, progressCallback)
  → Rust: snapshot dirty buffers on main thread
  → Rust: spawn tokio task
    → Walker: ignore::WalkBuilder respects .gitignore
    → Per file (8 parallel via semaphore):
        - If dirty snapshot exists → wrap in TextBuffer, search_scan_all()
        - Else → fs.read_file(), wrap in TextBuffer, search_scan_all()
        - SearchMatch results (byte_offset, length, line, column, context)
          → convert to GrepMatch JSON → send via AsyncBridge
  → Plugin receives streaming progress callbacks + final resolution
```

### Replace flow

```
Plugin: editor.replaceInFile(filePath, matches, replacement)
  → Rust: open file as buffer if needed (hidden_from_tabs)
  → Sort matches descending by byte_offset
  → Apply all edits as single bulk operation (single undo)
  → Save via FileSystem trait
```

## What's Done

### Search unified behind TextBuffer

All search — built-in single-buffer, synchronous project grep, and streaming project grep — now goes through the same code path:

- **`SearchMatch`** struct — `byte_offset`, `length`, `line`, `column`, `context`
- **`ChunkedSearchState`** — mutable scan state with incremental `running_line` counter
- **`search_scan_init(regex, max_matches, query_len)`** — creates state from `prepare_line_scan()`
- **`search_scan_next_chunk(&mut state)`** — processes one chunk, computes line/col/context on the fly. O(chunk_size) line counting via incremental cursor.
- **`search_scan_all(regex, max_matches, query_len)`** — synchronous variant for `spawn_blocking`

Editor's `SearchScanState` wraps `ChunkedSearchState` + editor-specific fields. `process_search_scan_batch` delegates to `buffer.search_scan_next_chunk()`.

### Project grep migrated

`handle_grep_project` and `handle_grep_project_streaming` create temporary `TextBuffer` instances and call `search_scan_all()`. The old `collect_matches_from_bytes` (with its duplicated regex/case-folding/whole-word logic) is deleted. Whole-word matching uses `\b...\b` in the regex, same as built-in search.

### Plugin UI (`search_replace.ts`)

- Compact inline-editing UX with search/replace fields, toggle buttons (case/regex/whole-word)
- Virtual-scrolled hierarchical results tree with per-match checkboxes
- Debounced search (150ms via `editor.delay()`)
- Streaming results via `grepProjectStreaming` progress callback
- Replace via `replaceInFile` — groups by file, applies edits, saves
- i18n via `editor.t()` (~140 messages in `search_replace.i18n.json`)

### Plugin API (`quickjs_backend.rs`)

- `grepProjectStreaming` with custom JS wrapper, auto-generated d.ts via `ts_raw` proc macro attribute
- `replaceInFile` returns `ReplaceResult { replacements, buffer_id }`
- `GrepMatch` type with file, buffer_id, byte_offset, length, line, column, context

## Known Limitations

### Context truncation on very long lines

The overlap window between chunks is `max(query_len, 256)` bytes. If a match sits on a line longer than the overlap, the reported `column` will be relative to the overlap start (not the true line start) and `context` will be truncated. This affects < 1% of real code. Increasing the overlap would increase redundant scanning.

### Soft match cap in streaming grep

The 8 parallel searchers check `match_count` with relaxed atomics, so the total can slightly exceed `max_results`. This is acceptable — it's a UI responsiveness limit, not a hard contract.

### `\b` word boundaries are ASCII-centric

Rust's `\b` matches ASCII word boundaries. For non-ASCII identifiers (e.g., `café`), whole-word matching may miss boundaries. Same limitation as built-in search.

## Remaining Work

### 1. Tests (high priority)

**Unit tests for `search_scan_next_chunk`:**
- Correct line/column/context for matches within a single chunk
- Correct line numbers across multiple chunks (running_line tracking)
- Overlap deduplication: matches in overlap region are skipped
- Matches at chunk boundaries (spanning overlap)
- Max matches cap triggers correctly

**Unit tests for project grep handlers:**
- `handle_grep_project` with open dirty buffers vs unopened files
- Binary file skipping
- Max results limit

**E2E test gaps:**
- Verify line/column accuracy in search results panel
- Multi-file replace with some files failing

### 2. Plugin polish (medium priority)

- Invalid regex patterns silently fail — show error message to user
- Replace errors are debug-logged but not surfaced in status bar
- Remove leftover `editor.debug()` calls (search_replace_enter, search_replace_tab)
- Selection state is lost when search re-runs (option toggle, pattern change)

### 3. Future / nice-to-have

- Search history (cycle through previous patterns)
- "Replace next" — replace one match and advance to next
- Replace preview — show replacement text inline before confirming
- Glob include/exclude filters for file paths
- Configurable max results (currently 200 in plugin)
- Large file lazy loading for project grep (currently reads small files fully; could use `load_large_file` for files above a project-grep-specific threshold)
- Concurrent search: chunks would need chunk-relative line numbers fixed up after completion
