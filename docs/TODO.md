# TODO

## Completed Features ✅

**Core Functionality**: Event-driven architecture with unlimited undo/redo, multi-cursor editing, clipboard operations (copy/cut/paste), position history navigation (Alt+Left/Right), line wrapping, large file support (1GB+), instant startup. Advanced prompt editing (word deletion Ctrl+Backspace/Delete, copy/paste/cut Ctrl+C/X/V in all input prompts).

**UI & Layout**: Split views (horizontal/vertical), scrollbar, tab bar, command palette (Ctrl+P), help system (Ctrl+H), file explorer with lazy loading and gitignore support, status bar, line numbers, themes (dark/light/high-contrast).

**LSP Integration**: Diagnostics (errors/warnings), code completion (Ctrl+Space), go-to-definition, rename refactoring (F2), multi-language support, process resource limits.

**File Operations**: Open/save/close, multiple buffers, file explorer (create/delete files/dirs, show/hide hidden, respect gitignore, auto-expand on focus, unsaved indicators), async I/O.

**Git Integration**: Git grep (Ctrl+Shift+G), git find file (Ctrl+Shift+P).

**Plugin System**: Lua 5.4 runtime, plugin manager, command registration, event hooks, async process spawning, buffer query API, overlay system. Example: TODO Highlighter plugin.

**Testing**: 400+ unit tests, 59 E2E tests, property-based tests, visual regression testing framework.

**Recent Fixes**: Scrollbar height when no scrolling needed, cursor rendering at buffer end, keybinding conflicts, file explorer scrolling and focus sync, viewport scrolling on Enter key, marker-based overlay system.

**Performance (Jan 2025)**: Massive improvements for huge files (61MB, 789K lines):
- **ChunkTree optimization**: Fixed chunk size from 64 bytes to 4KB → 38x speedup (file loading: 3.2s → 83ms, reduced tree from 1M to 15K nodes)
- **Scroll limit simplification**: O(n) → O(viewport_height), preventing problem at source rather than fixing afterward
- **Buffer cache removal**: Eliminated `buffer.to_string()` calls (3.9s for 61MB!), added TODO for proper incremental cache
- **render-line hook**: Plugins now inspect visible content during rendering (no duplicate iteration, scales to 1GB+ files)
- **Test performance**: `test_line_numbers_absolute_after_jump_to_beginning` improved from 158s → 0.10s (~1,580x speedup!)

---

## Remaining Work

### High Priority: Core Editor Features

#### Search & Replace
- [x] Basic text search (forward/backward with F3/Shift+F3)
- [x] Search wrap-around at document boundaries
- [x] Search highlighting (viewport-optimized for huge files)
- [x] Incremental search (as-you-type highlighting in prompt)
- [ ] Search with regex support
- [ ] Replace (single occurrence)
- [ ] Replace all
- [ ] Search in selection
- [ ] Case-sensitive/insensitive toggle
- [ ] Whole word matching
- [ ] Search history
- [ ] Multi-file search integration (via git grep)

#### Auto-Indent & Smart Editing
- [ ] Auto-indent on newline (language-aware)
- [ ] Smart home key (toggle between line start and first non-whitespace)
- [ ] Bracket matching (highlight matching bracket)
- [ ] Auto-close brackets/quotes
- [ ] Auto-pair deletion (delete both opening and closing)
- [ ] Electric indent (auto-adjust indentation)
- [ ] Toggle comment (language-aware, Ctrl+/)
- [ ] Block comment support
- [ ] Indent/dedent selection (Tab/Shift+Tab)

#### Advanced Selection
- [ ] Rectangular/block selection (Alt+drag or Ctrl+Alt+arrows)
- [ ] Multiple rectangular selections
- [ ] Column editing mode
- [ ] Expand selection to scope (by AST nodes)

#### Macros
- [ ] Record macro (q + key)
- [ ] Play macro (@ + key)
- [ ] Named macros
- [ ] Macro editing
- [ ] Macro persistence

### High Priority: LSP Features

Complete the LSP integration to match VS Code/Neovim capabilities:

- [ ] Hover documentation (show on keybinding or hover)
- [ ] Code actions (quick fixes, refactorings)
- [ ] Find references (show all usages)
- [ ] Document symbols (outline/breadcrumb)
- [ ] Workspace symbols (find symbol across project)
- [ ] Signature help (parameter hints while typing)
- [ ] Inlay hints (type annotations, parameter names)
- [ ] Call hierarchy
- [ ] Type hierarchy
- [ ] Document formatting (format entire file)
- [ ] Range formatting (format selection)
- [ ] Semantic tokens (advanced syntax highlighting)
- [ ] Code lens (inline actions)
- [ ] Folding ranges (code folding)

### High Priority: File Explorer Polish

- [ ] Input dialog system for custom file/directory names
- [ ] Rename with custom name (currently limited)
- [ ] Copy/move operations (Ctrl+C, Ctrl+X, Ctrl+V in explorer)
- [ ] Duplicate file/directory
- [ ] File watching for auto-refresh
- [ ] Search/filter within explorer
- [ ] Sort options (name, date, size, type)
- [ ] Show file permissions/metadata
- [ ] Bulk operations (multi-select)

### Medium Priority: Editor Experience

#### Navigation & Jumps
- [ ] Go to line number (Ctrl+G)
- [ ] Go to matching bracket
- [ ] Jump to next/previous error (F8/Shift+F8)
- [ ] Jump to next/previous search result (F3/Shift+F3)
- [ ] Jump to beginning/end of block
- [ ] Bookmark system (set/clear/jump)
- [ ] Mark ring (Emacs-style)

#### Visual Improvements
- [ ] Minimap (optional, like VS Code)
- [ ] Indent guides
- [ ] Current line highlighting
- [ ] Whitespace visualization (spaces, tabs, line endings)
- [ ] Color column (vertical ruler at 80/120 chars)
- [ ] Rainbow brackets
- [ ] Git gutter (show added/modified/deleted lines)
- [ ] Smooth scrolling

#### Themes & Appearance
- [ ] More built-in themes (Solarized, Monokai, Dracula, Nord, etc.)
- [ ] Theme customization UI
- [ ] Font configuration (size, family)
- [ ] Ligature support
- [ ] Custom color overrides
- [ ] Per-language theme adjustments

#### Snippets & Templates
- [ ] Snippet system (define snippets in config)
- [ ] Snippet expansion (Tab trigger)
- [ ] Tabstops and placeholders
- [ ] Mirror/transform placeholders
- [ ] Snippet variables ($1, $2, $TM_FILENAME, etc.)
- [ ] Snippet file templates (new file from template)
- [ ] Language-specific snippets

#### Command Palette Improvements
- [ ] Fuzzy matching (currently substring)
- [ ] Command history
- [ ] Command aliases
- [ ] Recently used commands at top
- [ ] Command categories/grouping
- [ ] Show keyboard shortcuts in palette

### Medium Priority: Advanced Features

#### Project Management
- [ ] Project/workspace concept
- [ ] Project-specific configuration
- [ ] Project switching
- [ ] Workspace root detection improvements
- [ ] Multiple workspace folders
- [ ] Project templates

#### Advanced Git Integration
- [ ] Git status in file explorer
- [ ] Git blame (show commit info for line)
- [ ] Git diff view (side-by-side or unified)
- [ ] Stage/unstage hunks
- [ ] Commit UI
- [ ] Branch switching
- [ ] Git log viewer
- [ ] Merge conflict resolution UI
- [ ] Rebase interactive UI (stretch goal: Magit-style)

#### Terminal Integration
- [ ] Embedded terminal (Ctrl+`)
- [ ] Multiple terminals
- [ ] Split terminal
- [ ] Send selection to terminal
- [ ] Terminal history
- [ ] Shell integration

#### Debugger Integration
- [ ] Debug adapter protocol (DAP) support
- [ ] Breakpoints (toggle, conditional)
- [ ] Debug toolbar (continue, step, etc.)
- [ ] Variables view
- [ ] Call stack
- [ ] Watch expressions
- [ ] Debug console/REPL

### Medium Priority: Plugin System (Phase 3)

#### Interactive UI API
- [ ] Virtual buffers (`editor.create_virtual_buffer(name, content)`)
- [ ] Set buffer content (`editor.set_buffer_content(buffer_id, content)`)
- [ ] Read-only buffers (`editor.set_buffer_read_only(buffer_id, bool)`)
- [ ] Selection lists (`editor.show_selection_list(items, callback)`)
- [ ] Input dialogs (`editor.show_input(prompt, default, callback)`)
- [ ] Generic popups (`editor.show_popup(options)`)

#### Modal Interaction & Navigation
- [ ] Define custom modes (`editor.define_mode(mode_name, options)`)
- [ ] Set buffer mode (`editor.set_mode(buffer_id, mode_name)`)
- [ ] Dynamic keybindings (`editor.bind_key(mode, key, callback)`)
- [ ] Goto line/position (`editor.goto_line(line_num)`, `editor.goto_position(offset)`)
- [ ] Set selection (`editor.set_selection(start, end)`)
- [ ] Scroll control (`editor.scroll_to_line(line_num)`)

#### Enhanced Hooks & Integration
- [ ] More hooks: `on_buffer_open`, `on_selection_change`, `on_key_press`
- [ ] State persistence: `editor.get/set_plugin_data(key, value)`
- [ ] Plugin configuration support
- [ ] LSP access: `editor.lsp_call(...)`
- [ ] Search/Replace API: `editor.search(...)`
- [ ] Undo history API: `editor.get_undo_history(...)`
- [ ] Custom syntax definitions
- [ ] Process cancellation/kill support
- [ ] Async Lua execution: `editor.async(function)`

#### Overlay Lifecycle Management
**Priority: High** (blocks TODO highlighter plugin from working correctly with text edits)

**Problem**: Marker-based overlays automatically adjust positions when text changes, but stale overlays aren't automatically removed. When text is inserted/deleted before existing keywords:
1. Old overlays persist with stale IDs (e.g., `todo_TODO_L1_O1`)
2. Markers move these overlays to new byte positions (correct!)
3. New overlays are created for the same keywords with new IDs (e.g., `todo_TODO_L2_O1`)
4. Result: Stale overlay highlights wrong content, new overlay highlights correct content

**Test failures**:
- `test_todo_highlighter_updates_on_edit` - inserting line before TODO leaves old overlay at wrong position
- `test_todo_highlighter_updates_on_delete` - deleting lines causes similar issue

**Solutions** (implement at least one):
- [ ] `editor.remove_overlays_by_prefix(buffer_id, prefix)` - Bulk remove plugin overlays by ID prefix
- [ ] `editor.clear_all_overlays(buffer_id)` - Clear all overlays for a buffer
- [ ] Automatic overlay cleanup based on marker validity (detect when marker points to deleted text)
- [ ] Overlay update API: `editor.update_overlay(buffer_id, overlay_id, new_range)` to reuse existing overlay

**Recommended approach**: Implement `remove_overlays_by_prefix()`. Plugins can then:
- On insert/delete events: `editor.remove_overlays_by_prefix(buffer_id, "todo_")`
- On next render-line: recreate overlays for visible keywords
- Still leverages markers for viewport scrolling (no recreation needed!)
- Only recreates when buffer content actually changes

#### Target Plugins (Showcase)
- [ ] Magit-style Git interface
- [ ] Telescope-style fuzzy finder
- [ ] Undo tree visualizer
- [ ] Project search & replace
- [ ] LSP code actions menu
- [ ] Advanced snippet system

### Low Priority: Polish & UX

#### User Experience
- [ ] Welcome screen (first run, tips, keybindings)
- [ ] Onboarding tutorial
- [ ] Configuration UI (settings editor)
- [ ] Keybinding customization UI
- [ ] Better error messages
- [ ] User-friendly error reporting
- [ ] Crash recovery (restore unsaved files)
- [ ] Session persistence (restore open files on restart)

#### Dialogs & Prompts
- [ ] Confirmation dialogs (delete, close unsaved, etc.)
- [ ] Progress indicators (file loading, LSP initialization)
- [ ] Status messages with timeout
- [ ] Toast notifications
- [ ] Modal dialogs

#### Performance & Optimization
- [ ] Incremental LSP sync (send only changed ranges)
- [ ] Syntax highlighting cache
- [ ] File explorer caching improvements
- [ ] Lazy plugin loading
- [ ] Startup time optimization
- [ ] Memory usage profiling
- [ ] Benchmark suite

#### Accessibility
- [ ] Screen reader support
- [ ] High contrast themes
- [ ] Keyboard-only navigation (no mouse required)
- [ ] Configurable UI scale
- [ ] Color-blind friendly themes

### Low Priority: Advanced/Future Features

#### Remote Editing
- [ ] SSH file editing
- [ ] SFTP support
- [ ] Remote workspace
- [ ] Remote LSP servers
- [ ] Remote terminal

#### Collaboration
- [ ] Collaborative editing (CRDT-based)
- [ ] Share session (read-only or collaborative)
- [ ] Presence indicators (show other cursors)
- [ ] Chat/comments

#### Extensions & Marketplace
- [ ] Plugin marketplace/registry
- [ ] Plugin discovery UI
- [ ] One-click plugin installation
- [ ] Plugin auto-updates
- [ ] Plugin ratings/reviews

#### Other
- [ ] Diff editor (compare two files side-by-side)
- [ ] Hex editor mode
- [ ] Binary file viewer
- [ ] Image preview in editor
- [ ] PDF preview
- [ ] Markdown preview (live)
- [ ] Org-mode support
- [ ] Vi/Vim emulation mode
- [ ] Emacs keybinding mode
- [ ] Multiple cursor shapes (block, underline, etc.)

---

## Technical Debt & Refactoring

### Line Wrapping Refactoring
- [ ] **Unify wrapping and no-wrapping code paths**: Treat no-wrapping as infinite-width wrapping
  - Modify rendering to always use `wrap_line()` with `WrapConfig::new(usize::MAX, gutter_width, false)` for no-wrap mode
  - Remove all `if line_wrap` branches in `split_rendering.rs::render_buffer_in_split()`
  - Handle horizontal scrolling as post-processing on the single segment returned for infinite-width lines

- [ ] **Move cursor position calculation into rendering traversal**: Eliminate duplicate line iteration
  - In `split_rendering.rs::render_buffer_in_split()`, track cursor screen position during the existing line rendering loop
  - As each line is rendered, check if it contains the primary cursor position
  - Use the already-computed `segments` from `wrap_line()` to calculate position via `char_position_to_segment()`
  - After loop completes, use tracked position instead of calling `viewport.cursor_screen_position()`
  - Delete `viewport.rs::cursor_screen_position()` entirely

- [ ] **Fix style preservation during wrapping**: Currently loses syntax highlighting/selection styles when wrapping
  - In wrapping section, preserve the original `line_spans` styling instead of using only first span's style
  - Track character-to-span mapping to apply correct styles to each character in wrapped segments
  - Ensure selections, syntax highlighting, and overlays render correctly across wrapped segments

**Benefits**: Single source of truth for wrapping, single line traversal (better performance), cursor positioning and rendering always agree by construction, massive code deduplication.

### Code Organization
- [x] Extract UI rendering (~430 lines → 6 modules)
- [x] Extract commands & prompts (~335 lines → 2 modules)
- [ ] Create BufferView abstraction (~500 lines)
- [ ] Extract multi-cursor operations (~200 lines)
- [ ] Refactor Editor into smaller components
- [ ] Split large modules (editor.rs is ~3000 lines)

### Test Infrastructure
- [ ] **Fix async file loading in test harness**: Currently 6 tests ignored due to async file loading not working properly
  - `test_file_explorer_displays_opened_file_content`
  - `test_git_find_file_actually_opens_file`
  - `test_git_grep_opens_correct_file_and_jumps_to_line`
  - `test_git_grep_cursor_position_accuracy`
  - `test_git_grep_shows_results`
  - Test harness needs way to wait for/force async file operations to complete

- [ ] **Fix BIG.txt generation timing**: 2 scrolling tests fail when run with other tests
  - `test_jump_to_eof_large_file`
  - `test_line_numbers_absolute_after_jump_to_beginning`
  - Issue: BIG.txt (61MB test file) generation interferes with other tests
  - Solution: Better test isolation or pre-generated fixtures

- [ ] **Support independent buffers per split**: Currently architectural limitation
  - `test_margin_per_buffer_in_split_view` expects different files in different splits
  - Current behavior: All splits display the same active buffer
  - Need to implement per-split buffer management if this is desired functionality

- [ ] Add more E2E tests for complex workflows
- [ ] Performance regression tests
- [ ] Memory leak detection tests

---

## Comparison: Feature Parity with Major Editors

### ✅ Features on Par with Emacs/Neovim/VS Code/Zed
- Multi-cursor editing
- LSP integration (diagnostics, completion, go-to-definition, rename)
- Split views
- File explorer
- Syntax highlighting (tree-sitter)
- Command palette
- Themes
- Large file support (better than most)
- Plugin system (comparable to early Vim/Emacs plugins)
- Async I/O
- Unlimited undo/redo

### 🚧 Features Partially Implemented
- Line wrapping (implemented but needs refactoring)
- Git integration (grep/find, but missing status/blame/diff)
- Clipboard (basic, but missing system clipboard on all platforms)
- LSP (core features done, missing hover/actions/references/hints)

### ❌ Major Missing Features (vs Emacs/Neovim/VS Code/Zed)
- **Search & Replace** (critical gap)
- **Auto-indent** (critical gap)
- **Bracket matching/auto-close** (critical gap)
- **Snippets** (critical gap for productivity)
- **Debugger integration** (DAP)
- **Terminal integration** (embedded terminal)
- **Git UI** (beyond grep/find)
- **Project management** (workspace concept)
- **Hover documentation** (LSP)
- **Code actions** (LSP)
- **Find references** (LSP)
- **Advanced navigation** (go to line, bookmarks)
- **Macros**
- **Minimap/indent guides**
- **Remote editing** (SSH/SFTP)
- **Collaborative editing**

---

## Milestones

### Milestone 1: Essential Editing (Target: MVP+)
*Goal: Match basic productivity of other editors*
- [x] Core editing (insert, delete, move, select)
- [x] Multi-cursor
- [x] Undo/redo
- [x] Clipboard
- [ ] **Search & replace** ← HIGHEST PRIORITY
- [ ] **Auto-indent**
- [ ] **Bracket matching/auto-close**
- [ ] **Go to line**

### Milestone 2: Developer Experience (Target: Competitive)
*Goal: Be a viable daily driver for developers*
- [x] LSP (diagnostics, completion, go-to-definition, rename)
- [ ] LSP (hover, code actions, find references)
- [x] File explorer
- [ ] File explorer (rename with custom name, copy/move)
- [x] Git (grep, find file)
- [ ] Git (blame, status, diff)
- [ ] Snippets
- [ ] Toggle comment

### Milestone 3: Advanced Features (Target: Best-in-Class)
*Goal: Unique features that set us apart*
- [x] Large file support (1GB+)
- [x] Plugin system (Lua)
- [ ] Plugin system (Phase 3 APIs)
- [ ] Magit-style git interface (via plugin)
- [ ] Telescope-style fuzzy finder (via plugin)
- [ ] Terminal integration
- [ ] Debugger integration
- [ ] Advanced theming

### Milestone 4: Polish & Ecosystem (Target: Production-Ready)
*Goal: Ready for 1.0 release*
- [ ] Welcome screen & onboarding
- [ ] Configuration UI
- [ ] Error handling & crash recovery
- [ ] Session persistence
- [ ] Plugin marketplace
- [ ] Comprehensive documentation
- [ ] Video tutorials

---

## Notes

- **Current focus**: Search & replace is the #1 missing feature for daily use
- **Plugin system**: Core infrastructure is solid, need Phase 3 APIs for advanced plugins
- **LSP**: Basic features work well, need advanced features (hover, actions, references)
- **File explorer**: Functional but needs polish (rename, copy/move, file watching)
- **Testing**: Strong test coverage (400+ unit, 59 E2E), need to fix 8 ignored tests
- **Performance**: Excellent (large file support, instant startup), continue monitoring
- **Code quality**: Needs refactoring (line wrapping, large modules), but stable

---

## Architectural Analysis: Lazy-Edit Approach for Large Files

**Date:** 2025-11-09
**Status:** Analysis Complete - **NOT RECOMMENDED**
**Context:** Proposal to use Write-Ahead Log (WAL) with lazy application of edits for instant editing on huge files

### Proposal Summary

Store edits in an event log and only apply them when sections are brought into view (viewport-based lazy materialization). Include persistence via WAL alongside the actual file, with background application and atomic rename for undo/redo.

### Critical Problems Identified

#### 1. Position Tracking Cascade Failure 🔴

**Current Architecture Relies On:**
- Dozens of position-dependent structures: cursors, selections, markers, overlays, search matches, syntax ranges, line cache
- `VirtualBuffer` with automatic position adjustment via edit log
- O(log n) position queries via ChunkTree

**Lazy-Edit Problem:**
```
File: "Hello World\nFoo Bar"
Replace "o" → "XXX" (3 occurrences)

Visible region: "HellXXX WXXXrld\n..." (applied)
Hidden region: "FXXXXXXBar" (pending)

Cursor at position 20 means:
- 20 if unapplied region not visited
- 26 if applied (6 extra chars)

Every position needs: (base_pos, pending_offset, applied_regions_bitmap)
Position queries become: O(log n) → O(n × regions)
```

**Current `adjust_for_edit()` assumes immediate application:**
```rust
// src/cursor.rs:86-99
if edit_pos <= self.position {
    self.position = (self.position as isize + delta).max(0) as usize;
}
```
With lazy edits: requires complex region-tracking state machine.

#### 2. Multi-Cursor Consistency Nightmare 🔴

With cursors at positions: 100, 5000, 10000, 50000
- Viewport applies edits only in 0-5000 range
- Cursor at 10000 points to logical position but unknown physical position
- When user types at cursor 10000, must materialize ALL pending edits up to that point
- Or maintain shadow positions with pending_edits_hash for every cursor

#### 3. Line Cache Invalidation Chaos 🔴

**Current system** (src/line_cache.rs:62-110): Iterates from nearest cached point to build line numbers.

**With lazy edits:**
```
1GB file, 10M lines, 1000 scattered edits
User jumps to line 7500

Current: O(distance from nearest cache entry)
Lazy: Must apply all pending edits between cache and target first
      OR maintain logical vs physical line number mappings
      "Line 7500" becomes ambiguous: before or after edits?
```

**Invalidation** is currently simple:
```rust
// src/line_cache.rs:150+
pub fn invalidate_from(&mut self, byte_offset: usize) {
    self.entries.retain(|&offset, _| offset < byte_offset);
}
```
With lazy edits: need per-region tracking of which edits are applied.

#### 4. Syntax Highlighting Corruption 🔴

Tree-sitter parsers are context-sensitive. A change at position 100 affects syntax at position 100,000:
```rust
// Position 0-100
fn process() {
    let x = "unclosed string

// Position 10,000
    println!("code");  // Actually inside the string!
}
```

**Current:** Simple invalidation on edit, re-parse viewport. Works because edits are immediately applied.

**Lazy edits:** Viewport shows incorrect syntax until all edits materialized, or must re-parse on every viewport change.

#### 5. Memory Overhead Explosion 🟡

**Current ChunkTree:**
```
1GB file = ~250,000 chunks (4KB each)
1000 edits = 1000 new chunk versions
Memory: ~1GB (original) + ~4MB (modified chunks via Arc sharing)
```

**Lazy edit approach:**
```
Event log: 1M edits × ~100 bytes avg = 100MB
Applied regions bitmap: 1 bit per 4KB = 31KB
Region version map: 250K entries × 8 bytes = 2MB
Position translation cache: Variable, potentially huge

Total overhead: ~100-200MB
Worse than current persistent ChunkTree!
```

#### 6. Cascading Materialization 🟡

Operations requiring sequential iteration force full materialization:
- **Save file:** Must apply ALL edits
- **Search:** Must search actual text, not event log
- **Jump to line N:** Must count newlines through all edits
- **LSP position conversion:** Must know actual text
- **Calculate file size:** Must account for all edits

**Result:**
```
User saves file:
  Original ChunkTree: 1GB
  Event log: 100MB
  Materialized ChunkTree: 1.2GB
  Peak memory: 2.3GB for a 1GB file!
```

**Current system:** Direct edits with Arc-sharing. Peak memory: ~1.05GB.

#### 7. WAL Complexity and Crash Inconsistency 🔴

**Proposed WAL:**
```
File: document.txt (1GB)
WAL: document.txt.wal (event log)
Background thread applies edits to real file

Crash scenarios:
1. Crash during background write:
   - WAL has events
   - File partially updated
   - Which events applied? Unknown!

2. WAL + File desync:
   - 1000 edits in WAL
   - Background applied 500
   - Crash
   - On restart: Which 500 were applied?
```

**Required for correctness:**
```rust
struct WalEntry {
    sequence_number: u64,
    event: Event,
    applied_to_file: bool,  // Requires fsync after each!
    checksum: u64,
}
```

**Current approach:** Events in memory, file writes are atomic, no partial-state on disk.

#### 8. File Watching Conflicts 🔴

```
Lazy system with WAL:
  User loads 1GB file
  User makes 1000 edits (in WAL, not yet applied)
  External tool modifies file (e.g., git checkout)

Now:
  WAL positions relative to old file content
  File content changed underneath
  All position-based edits in WAL now INVALID
  Must detect, invalidate WAL, reload = LOST WORK!
```

**Current system:** Detect external change, ask user: Reload or Keep. Simple binary choice.

### Performance Impact on Actual Bottlenecks

**Current bottlenecks identified:**

1. **Search** (src/buffer.rs:227): `let text = self.to_string();` materializes entire file
2. **Save** (src/buffer.rs:135): Reads entire buffer to save
3. **Load** (src/buffer.rs:106): One-time cost, already fast

**Lazy-edits would NOT help** because these operations require full materialization anyway!

### Better Alternatives

#### Alternative 1: Streaming Search ✅ (Low Effort, High Benefit)

**Current problem:**
```rust
pub fn find_next(&self, pattern: &str, start_pos: usize) -> Option<usize> {
    let text = self.to_string();  // Materializes entire 1GB file!
    // ...
}
```

**Solution:** Use existing ChunkTree iterators:
```rust
pub fn find_next_streaming(&self, pattern: &str, start_pos: usize) -> Option<usize> {
    let mut iter = self.persistence.byte_iterator(start_pos);
    let mut buffer = Vec::with_capacity(pattern.len() * 2);

    // Process 4KB chunks at a time
    while let Some(chunk) = iter.next_chunk() {
        buffer.extend_from_slice(chunk);
        if let Some(offset) = search_in_buffer(&buffer, pattern) {
            return Some(current_pos + offset);
        }
        // Keep overlap for cross-chunk matches
        if buffer.len() >= pattern.len() {
            buffer.drain(0..buffer.len() - pattern.len() + 1);
        }
    }
    None
}
```

**Benefits:**
- Works on 1GB files without materializing entire content
- Memory: O(chunk_size) = 8KB
- Time: Same O(n) but no memory spike
- **Effort:** 1-2 days
- **Risk:** Low

#### Alternative 2: Async Operations with Progress ✅ (Medium Effort, High Benefit)

**Wrong metric:** "Time to complete operation"
**Right metric:** "Time until user can continue editing"

```rust
pub async fn replace_all_progressive(
    pattern: &str,
    replacement: &str,
    progress_callback: impl Fn(usize, usize)
) -> Result<()> {
    let chunk_size = 1024 * 1024; // 1MB chunks

    for offset in (0..total_size).step_by(chunk_size) {
        let edits = find_and_replace_in_range(offset, offset + chunk_size);
        apply_edits(edits);

        progress_callback(processed, total);

        // Yield to UI thread every 100ms
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
}
```

**Benefits:**
- UI stays responsive
- Progress bar shows completion
- User can cancel operation
- No complex position tracking
- All edits eventually applied correctly
- **Effort:** 3-5 days
- **Risk:** Low-Medium

#### Alternative 3: Optimized Bulk Operations ✅ (Medium Effort, Medium Benefit)

```rust
impl ChunkTree {
    pub fn bulk_replace(&self, positions: &[(usize, usize, &str)]) -> Self {
        // Optimized batch insert/delete
        // Create minimal new chunks
        // Maximum Arc sharing
    }
}
```

**Benefits:**
- Faster multi-cursor operations
- Maintains existing architecture
- **Effort:** 3-5 days
- **Risk:** Low

### The Industry-Standard Solution: Rope/Piece Table (Already Implemented!)

The problems identified above aren't hypothetical - they're the **exact problems** that Piece Tables (VS Code) and Ropes (Xi Editor, Kakoune) were invented to solve.

**Current Architecture - Layered Design:**

The codebase uses a **Rope** with multiple layers for different responsibilities:

#### Layer 1: ChunkTree (src/chunk_tree.rs) - The Rope
```rust
// Persistent tree structure
enum Node {
    Leaf(data: &[u8]),           // Actual bytes (up to 4KB)
    Gap(size: usize),            // Efficient empty space
    Internal(children: Vec<Arc<Node>>)  // Tree with Arc-sharing
}
```

**Responsibility:** Persistent immutable data structure for text storage
- Insert/delete = O(log n) tree operations creating new nodes
- Old versions stay valid via Arc-sharing (structural sharing)
- No copying of unchanged subtrees

#### Layer 2: VirtualBuffer (src/virtual_buffer.rs) - Caching + Iterator Support
```rust
struct VirtualBuffer {
    persistence: Box<dyn PersistenceLayer>,  // Usually ChunkTreePersistence
    cache: Cache,                            // 16MB LRU cache
    edit_log: Vec<Edit>,                     // For iterator position adjustment
    edit_version: AtomicU64,                 // Version counter
}
```

**Responsibility:** Performance optimizations on top of persistence layer
- 16MB cache for frequently accessed regions
- Edit log tracks changes so **active iterators** can adjust their positions
- Version tracking for iterator lifecycle management
- **NOT for undo/redo** (that's EventLog in event.rs)

#### Layer 3: Buffer (src/buffer.rs) - High-Level Text Operations
```rust
pub struct Buffer {
    virtual_buffer: VirtualBuffer,
    line_cache: LineCache,      // Byte offset → line number mapping
    file_path: Option<PathBuf>,
    modified: bool,
}
```

**Responsibility:** Text editor semantics
- Line-based operations (line iterator, line cache)
- File I/O (load, save)
- High-level operations (find, slice, character boundaries)
- Modified state tracking

#### Layer 4: EditorState (src/state.rs) - Undo/Redo + UI State
```rust
pub struct EditorState {
    buffer: Buffer,
    cursors: Cursors,
    viewport: Viewport,
    // ... UI state
}

// Separate from VirtualBuffer!
pub struct EventLog {
    entries: Vec<LogEntry>,     // Full event history with data
    current_index: usize,        // For undo/redo
    snapshots: Vec<Snapshot>,   // Periodic checkpoints
}
```

**Responsibility:** Application-level state and undo/redo
- EventLog stores full editing history (with text content)
- Undo/redo via event replay from snapshots
- Cursors, viewport, overlays, markers

#### Layer 5: Rendering - Iterator-Based Display
```rust
// Rendering walks the buffer using iterators
let mut iter = buffer.line_iterator(viewport.top_byte);
for (byte_offset, line_content) in iter.take(viewport_height) {
    render_line(line_content, syntax_highlighting);
}
```

**Responsibility:** Display visible content only
- Uses iterators from Buffer/VirtualBuffer
- O(viewport) not O(file_size)
- Iterators read from ChunkTree snapshot (efficient chunk traversal)

**How it works (same principle as Piece Table):**

```
Initial: Load 1GB file
  ChunkTree: [Leaf(file_data, 0..1GB)]
  VirtualBuffer: empty cache, empty edit_log
  Buffer: empty line_cache

User types "Hello" at beginning:
  ChunkTree: [Leaf("Hello"), Leaf(file_data, 0..1GB)]  // O(log n) insert
  VirtualBuffer: edit_log += [Insert(0, 5)]            // Track for iterators
  Buffer: line_cache invalidated from byte 0
  EventLog: append Insert event (for undo)

User replaces "Chapter 1" → "Introduction" at 1000:
  ChunkTree: [Leaf("Hello"), Leaf(file_data, 0..1000),
              Leaf("Introduction"), Leaf(file_data, 1009..1GB)]
  VirtualBuffer: edit_log += [Delete(1000, 9), Insert(1000, 12)]
  Buffer: line_cache invalidated from byte 1000
  EventLog: append Delete + Insert events (for undo)
```

**Why this layered approach solves all the problems:**

1. ✅ **No Coordinate Mapping Problem:** ChunkTree IS the current virtual state (not computed on-demand)
2. ✅ **No JIT Replay:** Rendering iterates ChunkTree directly - O(viewport) chunk traversal
3. ✅ **Global Features Work:** Search/lint iterate the actual ChunkTree content (not event log)
4. ✅ **No Concurrency Hell:** Persistent ChunkTree with Arc-sharing = old versions stay valid
5. ✅ **Instant Edits:** O(log n) ChunkTree operations, not file copies
6. ✅ **Undo is Free:** EventLog stores history, ChunkTree versions are Arc-shared

**Two separate "logs" with different purposes:**
- **VirtualBuffer.edit_log:** Lightweight position tracking (offset, length only) for **iterator adjustment**
- **EventLog:** Complete history (with text content) for **undo/redo**

**Potential Optimization: ChunkTree Root-Based Undo**

There's currently a **redundancy** between event-based undo and the ChunkTree's persistent structure:

**Current implementation** (src/event.rs:476-482):
```rust
pub struct Snapshot {
    pub log_index: usize,
    pub buffer_state: (),  // Placeholder! Intended for ChunkTree root
    pub cursor_positions: Vec<(CursorId, usize, Option<usize>)>,
}
```

**How undo/redo currently works:**
- EventLog stores all events with full text content
- Undo: Generate inverse events and replay them (e.g., Insert → Delete)
- Snapshots every 100 events (to avoid replaying from beginning)
- **Snapshot.buffer_state is currently unused** (just `()`)

**Alternative approach using ChunkTree roots:**
```rust
pub struct Snapshot {
    pub log_index: usize,
    pub buffer_state: Arc<ChunkTree>,  // Store root pointer
    pub cursor_positions: Vec<(CursorId, usize, Option<usize>)>,
}
```

**Undo via ChunkTree roots:**
- ChunkTree is already persistent (Arc-shared nodes)
- Every edit creates a new root, old root stays valid
- Undo = restore previous root pointer (O(1))
- No event replay needed!

**Trade-offs:**

| Approach | Memory | Undo Speed | Implementation |
|----------|--------|------------|----------------|
| Event replay (current) | O(events × text_size) | O(events_to_replay) | Complex inverse logic |
| ChunkTree roots | O(modified_chunks) via Arc | **O(1)** | Simple pointer swap |

**Why event replay might still be preferred:**
1. **Cross-buffer operations:** Events can include viewport, cursor, overlay changes
2. **Granular undo:** Can undo by "write action" groups, not just buffer edits
3. **Event stream debugging:** Events can be logged to disk for debugging
4. **Already implemented and working**

**Hybrid approach (best of both):**
```rust
pub struct Snapshot {
    pub buffer_state: Arc<ChunkTree>,     // Fast buffer restore
    pub events_since_snapshot: Vec<Event>, // For non-buffer state
}
```

This would give O(1) buffer undo while keeping event replay for UI state (cursors, viewport, etc.).

**The lazy-edit proposal would replace this proven solution with a worse one:**
- Current: ChunkTree (Rope) IS the virtual state, edits immediately applied ✅
- Proposed: WAL with deferred application, virtual state computed on-demand ❌

### Recommendation: **DO NOT IMPLEMENT LAZY EDITS**

The complexity cost far exceeds any benefit.

**Why the idea is appealing:**
- Instant feedback on huge operations sounds great
- WAL approach sounds robust

**Why it fails:**
- **The codebase already uses the industry-standard solution (Rope)**
- Lazy-edit would be a regression from proven Rope architecture to problematic WAL architecture
- Optimizes wrong metric (completion time vs continuation time)
- Current ChunkTree already achieves instant edits via:
  - O(log n) edits via persistent tree structure
  - O(viewport) rendering via chunked iteration
  - O(1) undo/redo via structural sharing (Arc<Node>)
- Missing pieces (streaming search, progress feedback) solvable without architectural overhaul

### Recommended Implementation Path

**Phase 1: Fix Immediate Bottlenecks** (1-2 weeks)
1. ✅ Streaming search/replace - Use existing ChunkTree iterators
   - Replace `to_string()` materialization in `find_next()`
   - Effort: 1-2 days
   - Benefit: GB file searching without OOM

2. ✅ Async bulk operations - Add progress bars and cancellation
   - Implement for save, load, replace-all
   - Effort: 3-5 days
   - Benefit: Responsive UI during long operations

3. ✅ Optimized bulk edits - Batch operations in ChunkTree
   - Special-case multi-cursor bulk operations
   - Effort: 3-5 days
   - Benefit: Faster replace-all

**Phase 2: Polish** (1 week)
- Progress indicators
- Cancellable operations
- Better error messages during long operations

**Total effort:** 2-3 weeks
**Complexity:** Low-Medium (builds on existing architecture)
**Benefit:** GB file editing with responsive UI
**Risk:** Low (incremental improvements, easily testable)

### Conclusion

The current architecture is **well-designed** for large files. The perceived performance problem is limited to specific operations (`to_string()` materialization) that can be fixed with targeted improvements, not architectural overhaul.

**Fundamental insight:** The lazy-edit approach solves a problem the architecture doesn't have, while introducing dozens of new problems that don't currently exist.

**Files referenced:**
- `src/buffer.rs` - Buffer operations, search, save/load
- `src/cursor.rs` - Position adjustment logic
- `src/line_cache.rs` - Line number caching and invalidation
- `src/chunk_tree.rs` - Persistent data structure for large files
- `src/virtual_buffer.rs` - Edit log and position tracking
- `src/highlighter.rs` - Viewport-only syntax highlighting
