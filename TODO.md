# Editor Implementation Plan

## Overview

Building a high-performance terminal text editor from scratch with:
- **Arbitrary large file support** (GB+ files)
- **Ultra-low latency** (<1ms per operation)
- **Multiple cursors** (Sublime/VSCode style)
- **Best-effort syntax highlighting** (tree-sitter)
- **Event-driven architecture** (lossless history, undo/redo)
- **JSON configuration** (hot reload, schema validation)

## Architecture Documents

- [NEW_ARCHITECTURE.md](NEW_ARCHITECTURE.md) - Core design and data structures
- [EVENT_LOG_ARCHITECTURE.md](EVENT_LOG_ARCHITECTURE.md) - Event system and smart scrolling
- [CONFIG_SYSTEM.md](CONFIG_SYSTEM.md) - Configuration and keybindings
- [ARCHITECTURE_ANALYSIS.md](ARCHITECTURE_ANALYSIS.md) - Analysis of old code (reference)
- [DETAILS.md](DETAILS.md) - Technical summary of original codebase

## Phase 0: Foundation & Architecture (3-4 days)

### 0.1 Clean Slate
- [ ] Review all architecture documents and finalize design decisions
- [ ] Delete obsolete modules:
  - `src/virtual_file.rs` (replaced by buffer.rs)
  - `src/lines.rs` (replaced by buffer.rs)
  - `src/memstore.rs` (replaced by buffer.rs)
  - `src/logs.rs` (using proper logging if needed)
  - Old `src/main.rs` content (keep placeholder for now)
- [ ] Update `Cargo.toml` dependencies:
  - Add: `serde`, `serde_json` (config)
  - Add: `anyhow`, `thiserror` (error handling)
  - Keep: `crossterm`, `ratatui`, `tree-sitter-*`
  - Remove unused dependencies

### 0.2 Event System (`event.rs`)
- [ ] Define `Event` enum with all event types:
  - `Insert { position, text, cursor_id }`
  - `Delete { range, deleted_text, cursor_id }`
  - `AddCursor { cursor }`
  - `RemoveCursor { cursor_id }`
  - `MoveCursor { cursor_id, old_position, new_position }`
  - `SetSelection { cursor_id, anchor }`
  - `Scroll { old_position, new_position }`
  - `Batch { events, description }`
- [ ] Implement `EventLog` struct:
  - `record(&mut self, event: Event) -> EventId`
  - `get(&self, id: EventId) -> Option<&LogEntry>`
  - `history(&self) -> &[LogEntry]`
  - `undo(&mut self) -> Option<EventId>`
  - `redo(&mut self) -> Option<EventId>`
- [ ] Implement event persistence:
  - `persist_event()` - append to log file
  - `load()` - load from log file
- [ ] Implement snapshots for performance:
  - `snapshots: Vec<(EventId, EditorState)>`
  - `rebuild_state()` - start from nearest snapshot
  - `maybe_snapshot()` - snapshot every N events
- [ ] Add serialization support (derive Serialize/Deserialize)
- [ ] Write unit tests for EventLog

### 0.3 Configuration System (`config.rs`)
- [ ] Define `Config` struct with all settings:
  - `theme: ThemeConfig` (colors, syntax colors)
  - `editor: EditorConfig` (tab_size, line_numbers, etc.)
  - `keybindings: Vec<Keybinding>`
  - `languages: HashMap<String, LanguageConfig>`
- [ ] Implement config loading/saving:
  - `Config::load() -> Result<Self>` (from ~/.config/editor/config.json)
  - `Config::save(&self) -> Result<()>`
  - `Config::config_path() -> Result<PathBuf>`
- [ ] Implement validation:
  - `validate(&self) -> Result<()>` (hex colors, tab_size range, etc.)
- [ ] Implement defaults:
  - `impl Default for Config`
  - Default keybindings
  - Default languages (Rust, JS, Python, JSON)
- [ ] Add hot reload support (notify crate, watch file changes)
- [ ] Generate default config on first run
- [ ] Write unit tests for config loading/validation

### 0.4 Keybinding System (`keybindings.rs`)
- [ ] Define `Action` enum (all possible actions):
  - Editing: Insert, Delete, Backspace, Undo, Redo
  - Navigation: MoveLeft, MoveRight, MoveUp, MoveDown, etc.
  - Selection: SelectLeft, SelectAll, SelectNextOccurrence
  - Multi-cursor: AddCursorAbove, AddCursorBelow
  - File: Save, Open, Close, Quit
  - Line ops: InsertLineAbove, DeleteLine, ToggleComment, etc.
  - Custom: Custom { name, args }
- [ ] Implement `KeybindingResolver`:
  - `new(config: &Config) -> Self` (build binding map)
  - `resolve(&self, key: KeyEvent) -> Option<Action>`
  - `parse_action(action: &str, args: &HashMap) -> Action`
- [ ] Support modifier keys (Ctrl, Alt, Shift, Super)
- [ ] Support context conditions (when: "editorTextFocus")
- [ ] Write unit tests for keybinding resolution

### 0.5 Buffer & Line Cache (`buffer.rs`)
- [ ] Define `Buffer` struct:
  - `content: Rope` (alias for ChunkTree<'static>)
  - `line_cache: LineCache`
  - `file_path: Option<PathBuf>`
  - `modified: bool`
- [ ] Define `LineCache` struct:
  - `line_starts: Vec<usize>` (byte offsets)
  - `valid: bool`
  - `line_count: usize`
- [ ] Implement core editing operations:
  - `insert(&mut self, pos: usize, text: &str)` - O(log n)
  - `delete(&mut self, range: Range<usize>)` - O(log n)
  - `slice(&self, range: Range<usize>) -> String` - O(log n)
- [ ] Implement line cache operations:
  - `rebuild_line_cache(&mut self)` - scan content, find newlines
  - `line_to_byte(&self, line: usize) -> usize` - O(1)
  - `byte_to_line(&self, byte: usize) -> usize` - O(log lines)
  - `line_content(&self, line: usize) -> &str` - O(log n)
  - `line_count(&self) -> usize` - O(1)
- [ ] Implement efficient range queries:
  - `lines_in_range(&self, start: usize, count: usize) -> Vec<&str>`
- [ ] Implement file operations:
  - `load_from_file(path: &Path) -> Result<Buffer>`
  - `save(&mut self) -> Result<()>`
- [ ] Implement incremental cache updates:
  - `invalidate_cache_from(&mut self, byte_offset: usize)`
  - `rebuild_cache_from(&mut self, byte_offset: usize)`
- [ ] Handle UTF-8 boundary detection:
  - `is_char_boundary(&self, pos: usize) -> bool`
  - `next_char_boundary(&self, pos: usize) -> usize`
  - `prev_char_boundary(&self, pos: usize) -> usize`
- [ ] Write unit tests for Buffer operations
- [ ] Write performance tests (1GB file benchmark)

## Phase 1: Core Editor (2-3 days)

### 1.1 Cursor System (`cursor.rs`)
- [ ] Define `Cursor` struct:
  - `position: usize` (byte offset)
  - `anchor: Option<usize>` (for selections)
  - `sticky_column: usize` (for up/down navigation)
- [ ] Define `CursorId(u64)` type
- [ ] Implement cursor methods:
  - `new(position: usize) -> Self`
  - `with_selection(start: usize, end: usize) -> Self`
  - `collapsed(&self) -> bool`
  - `selection_range(&self) -> Option<Range<usize>>`
- [ ] Write unit tests for cursor operations

### 1.2 Viewport & Smart Scrolling (`viewport.rs`)
- [ ] Define `Viewport` struct:
  - `scroll_pos: usize` (byte offset of top-left)
  - `scroll_col: usize` (horizontal scroll)
  - `width: u16`
  - `height: u16`
  - `visible_lines: Range<usize>` (cached)
- [ ] Implement smart scrolling:
  - `ensure_visible(&mut self, buffer: &Buffer, cursor: &Cursor)` - keep cursor visible
  - `ensure_all_visible(&mut self, buffer: &Buffer, cursors: &HashMap)` - multi-cursor
  - `scroll_to_line(&mut self, buffer: &Buffer, line: usize)`
- [ ] Implement viewport queries:
  - `visible_content(&self, buffer: &Buffer) -> Vec<(usize, &str)>`
  - `cursor_to_screen(&self, buffer: &Buffer, cursor: &Cursor) -> (u16, u16)`
- [ ] Implement scroll animations (future enhancement):
  - `smooth_scroll_to(&mut self, target_line: usize)`
- [ ] Write unit tests for viewport scrolling

### 1.3 Editor State (`state.rs`)
- [ ] Define `EditorState` struct:
  - `buffer: Buffer`
  - `cursors: HashMap<CursorId, Cursor>`
  - `primary_cursor: CursorId`
  - `viewport: Viewport`
  - `next_cursor_id: CursorId`
- [ ] Implement `apply(&mut self, event: &Event)`:
  - Handle `Event::Insert` - insert text, move cursor, adjust others, smart scroll
  - Handle `Event::Delete` - delete text, move cursor, adjust others, smart scroll
  - Handle `Event::AddCursor` - add cursor, make primary, smart scroll
  - Handle `Event::RemoveCursor` - remove cursor
  - Handle `Event::MoveCursor` - move cursor, smart scroll
  - Handle `Event::SetSelection` - set selection anchor
  - Handle `Event::Scroll` - update viewport
  - Handle `Event::Batch` - apply all events in order
- [ ] Implement cursor adjustment logic:
  - After insert: shift cursors after position
  - After delete: shift cursors after range
  - After batch: normalize cursors (merge overlapping)
- [ ] Implement `rebuild_from_log(&mut self, log: &EventLog)`:
  - Reset to initial state
  - Apply all events up to current
- [ ] Write unit tests for event application
- [ ] Write tests for cursor adjustment

### 1.4 Editor Event Loop (`editor.rs`)
- [ ] Define `Editor` struct:
  - `state: EditorState`
  - `log: EventLog`
  - `highlighter: Highlighter` (stub for now)
  - `terminal: Terminal`
  - `config: Config`
  - `keybindings: KeybindingResolver`
- [ ] Implement constructor:
  - `new(file: Option<PathBuf>) -> Result<Self>`
  - Load config
  - Create keybinding resolver
  - Load file if provided
- [ ] Implement main event loop:
  - `run(&mut self) -> Result<()>`
  - Loop: render → read event → handle event
- [ ] Implement event handling:
  - `handle_event(&mut self, event: crossterm::Event) -> Result<bool>`
  - `handle_key_event(&mut self, key: KeyEvent) -> Result<bool>`
  - Convert KeyEvent → Action → Vec<Event>
  - Record and apply events
- [ ] Implement action conversion:
  - `action_to_events(&self, action: &Action) -> Result<Vec<Event>>`
  - Convert each Action to appropriate Event(s)
  - Handle multi-cursor scenarios
- [ ] Implement undo/redo:
  - `handle_undo(&mut self)` - call log.undo(), rebuild state
  - `handle_redo(&mut self)` - call log.redo(), rebuild state
- [ ] Write integration tests (simulate keystrokes)

### 1.5 Basic Rendering (`render.rs`)
- [ ] Implement rendering without syntax highlighting:
  - `render(&mut self) -> Result<()>`
  - Get visible lines from viewport
  - Render line numbers (if enabled in config)
  - Render line content
  - Render cursors (primary highlighted differently)
  - Render selections (if any)
  - Render status bar (filename, line:col, mode, modified flag)
- [ ] Implement cursor rendering:
  - Block cursor at primary position
  - Thin vertical lines for secondary cursors
  - Selection highlighting
- [ ] Implement status bar:
  - Left: filename, modified flag
  - Center: status messages
  - Right: line:col, cursor count
- [ ] Apply theme colors from config
- [ ] Handle terminal resize events
- [ ] Optimize rendering (only redraw changed regions if possible)

### 1.6 Basic Keybindings
- [ ] Implement essential actions:
  - Character insertion (typing)
  - Backspace, Delete
  - Arrow keys (Left, Right, Up, Down)
  - Ctrl+Left/Right (word movement)
  - Home, End (line start/end)
  - Ctrl+Home/End (file start/end)
  - PageUp, PageDown
  - Ctrl+S (save)
  - Ctrl+Q (quit)
  - Ctrl+Z (undo)
  - Ctrl+Shift+Z or Ctrl+Y (redo)
- [ ] Test each keybinding individually
- [ ] Test multi-cursor editing (if secondary cursors added manually)

### 1.7 Milestone: Basic Editor Works
- [ ] **Test**: Open a text file
- [ ] **Test**: Navigate with arrow keys
- [ ] **Test**: Type text and see it appear
- [ ] **Test**: Delete text with backspace
- [ ] **Test**: Save file (Ctrl+S)
- [ ] **Test**: Undo changes (Ctrl+Z)
- [ ] **Test**: Redo changes (Ctrl+Shift+Z)
- [ ] **Test**: Quit editor (Ctrl+Q)
- [ ] **Test**: File modified flag updates correctly
- [ ] **Test**: Smart scroll keeps cursor visible

## Phase 2: Multi-Cursor Support (1 day)

### 2.1 Multi-Cursor Events
- [ ] Implement AddCursor event handling in state.rs
- [ ] Implement RemoveCursor event handling
- [ ] Implement cursor normalization:
  - `normalize_cursors(&mut self)` - merge overlapping cursors
  - Called after any multi-cursor operation
- [ ] Test cursor adjustment with multiple cursors

### 2.2 Multi-Cursor Keybindings
- [ ] Implement Ctrl+D (select next occurrence):
  - Find current selection text
  - Search for next occurrence
  - Add cursor at match
- [ ] Implement Ctrl+Click (add cursor at mouse position)
- [ ] Implement Alt+Up/Down (add cursor above/below)
- [ ] Implement Escape (reduce to single cursor)
- [ ] Implement Ctrl+A (select all, single cursor)

### 2.3 Multi-Cursor Rendering
- [ ] Render all cursors (primary different color)
- [ ] Render all selections
- [ ] Show cursor count in status bar
- [ ] Ensure all cursors visible when possible (smart scroll)

### 2.4 Multi-Cursor Editing
- [ ] Test: Add multiple cursors with Ctrl+D
- [ ] Test: Type with multiple cursors
- [ ] Test: Delete with multiple cursors
- [ ] Test: Move all cursors with arrow keys
- [ ] Test: Undo/redo with multiple cursors
- [ ] Test: Cursors normalize after overlapping edits

### 2.5 Milestone: Multi-Cursor Works
- [ ] **Test**: Select word, Ctrl+D multiple times, type to replace all
- [ ] **Test**: Alt+Click to place cursors, type at all positions
- [ ] **Test**: Vertical editing (cursors in column, type)
- [ ] **Test**: Undo reverts all multi-cursor changes
- [ ] **Benchmark**: 100 cursors, type character, measure latency (<2ms)

## Phase 3: Syntax Highlighting (1 day)

### 3.1 Highlighter Module (`highlighter.rs`)
- [ ] Define `Highlighter` struct:
  - `parser: tree_sitter::Parser`
  - `config: HighlightConfiguration`
  - `cache: HighlightCache`
- [ ] Define `HighlightCache`:
  - `range: Range<usize>` (cached byte range)
  - `spans: Vec<(usize, usize, u8)>` (start, end, style_id)
  - `valid: bool`
- [ ] Implement language detection:
  - `detect_language(path: &Path, config: &Config) -> Option<String>`
  - Match by file extension using config.languages
- [ ] Implement highlighter:
  - `new(config: &Config) -> Result<Self>`
  - `highlight(&mut self, buffer: &Buffer, range: Range<usize>) -> Vec<(usize, usize, Style)>`
  - Use tree-sitter to parse visible range
  - Return styled spans
- [ ] Implement caching:
  - `invalidate(&mut self, edited_range: Range<usize>)`
  - Only invalidate if edit overlaps cached range
  - Prefer to show stale highlights than block
- [ ] Implement timeout:
  - If parsing takes >5ms, abort and return unstyled
  - Retry on next render
- [ ] Add support for multiple languages:
  - Rust (tree-sitter-rust)
  - JavaScript (tree-sitter-javascript)
  - Python (tree-sitter-python)
  - JSON (tree-sitter-json)
  - More as needed

### 3.2 Integrate Highlighting into Rendering
- [ ] Update render.rs to use highlighter:
  - Get visible range from viewport
  - Call `highlighter.highlight(buffer, range)`
  - Apply styles to rendered text
- [ ] Map tree-sitter highlight names to theme colors:
  - comment → theme.syntax.comment
  - keyword → theme.syntax.keyword
  - string → theme.syntax.string
  - etc.
- [ ] Handle highlighting errors gracefully:
  - Show unstyled text if parsing fails
  - Log error for debugging
- [ ] Invalidate highlight cache on edits:
  - Call `highlighter.invalidate(edited_range)` after event application

### 3.3 Milestone: Syntax Highlighting Works
- [ ] **Test**: Open Rust file, see syntax colors
- [ ] **Test**: Edit code, highlighting updates correctly
- [ ] **Test**: Scroll through file, no visible lag
- [ ] **Test**: Open large file (1MB+), highlighting doesn't block
- [ ] **Benchmark**: Highlight 50 lines <5ms
- [ ] **Test**: Multiple languages (Rust, JS, Python, JSON)

## Phase 4: Optimization & Large Files (1 day)

### 4.1 Lazy Line Cache
- [ ] Make line cache build lazily:
  - Only compute visible range on first access
  - Expand cache as user scrolls
  - Trade initial scroll speed for memory
- [ ] Implement cache expansion:
  - `ensure_lines_cached(&mut self, start: usize, end: usize)`
  - Only parse lines if not in cache
- [ ] Test with large files (100MB+):
  - Open should be instant
  - First scroll might be slower
  - Subsequent scrolls fast

### 4.2 Event Log Snapshots
- [ ] Implement snapshot creation:
  - `maybe_snapshot(&mut self, state: &EditorState)`
  - Create snapshot every 1000 events
- [ ] Implement fast state rebuild:
  - `rebuild_state(&self) -> EditorState`
  - Find nearest snapshot before current
  - Apply events since snapshot
- [ ] Test undo performance:
  - 10,000 edits, undo should be instant
  - No need to replay all 10,000 events

### 4.3 Event Log Persistence
- [ ] Implement log file format:
  - JSON lines (one event per line)
  - Store in ~/.local/share/editor/sessions/
- [ ] Implement session saving:
  - `enable_persistence(&mut self, path: &Path)`
  - Append each event to log file
- [ ] Implement session loading:
  - `EventLog::load(path: &Path) -> Result<Self>`
  - Replay all events to restore session
- [ ] Add --resume flag to CLI:
  - Resume last editing session
  - Show list of recent sessions

### 4.4 Performance Profiling
- [ ] Profile critical paths:
  - Keystroke → render latency
  - Event application time
  - Line cache lookup time
  - Syntax highlighting time
- [ ] Identify bottlenecks
- [ ] Optimize hot paths:
  - Use flamegraph or perf
  - Target <1ms for common operations
- [ ] Add performance tests:
  - `bench_keystroke_latency()`
  - `bench_multi_cursor_edit()`
  - `bench_scroll_1gb_file()`

### 4.5 Large File Testing
- [ ] Generate test files:
  - 10MB, 100MB, 1GB, 10GB
  - Various content types (code, text, binary)
- [ ] Test opening large files:
  - Should be instant (<100ms)
  - Memory usage reasonable (<50MB)
- [ ] Test editing large files:
  - Insert at beginning, middle, end
  - Delete large ranges
  - Undo/redo large operations
- [ ] Test scrolling large files:
  - Scroll to end instantly
  - Scroll back to start instantly
  - No lag at any position
- [ ] Test syntax highlighting with large files:
  - Only highlight visible range
  - No full-file parsing

### 4.6 Milestone: Production Ready
- [ ] **Test**: Open 1GB file in <100ms
- [ ] **Test**: Edit 1GB file with <1ms latency
- [ ] **Test**: Memory usage <50MB for any file size
- [ ] **Test**: 100 cursors edit at 60fps
- [ ] **Test**: Undo 10,000 edits instantly
- [ ] **Benchmark**: All operations meet latency targets
- [ ] **Test**: Save/resume session works correctly

## Phase 5: Polish & Features (Ongoing)

### 5.1 Search & Replace
- [ ] Implement find:
  - Ctrl+F to open search
  - Highlight all matches
  - F3/Shift+F3 to navigate matches
- [ ] Implement replace:
  - Ctrl+H to open replace
  - Replace current match
  - Replace all matches
- [ ] Implement regex search:
  - Support basic regex patterns
  - Show match count
- [ ] Add search-related events to log

### 5.2 Line Operations
- [ ] Implement toggle comment:
  - Ctrl+/ to comment/uncomment lines
  - Use language comment token from config
  - Works with multi-cursor
- [ ] Implement move line up/down:
  - Alt+Up/Down to move lines
  - Works with multiple selections
- [ ] Implement duplicate line:
  - Ctrl+D to duplicate current line
- [ ] Implement delete line:
  - Ctrl+Shift+K to delete entire line

### 5.3 Advanced Multi-Cursor
- [ ] Implement select all occurrences:
  - Ctrl+Shift+L to add cursor at each occurrence
- [ ] Implement split selection to lines:
  - Alt+Shift+I to split multi-line selection to cursors per line
- [ ] Implement cursor history:
  - Alt+Left/Right to navigate cursor history

### 5.4 File Operations
- [ ] Implement open file:
  - Ctrl+O to open file picker
  - Fuzzy search file names
- [ ] Implement save as:
  - Ctrl+Shift+S to save as new file
- [ ] Implement close file:
  - Ctrl+W to close current file
  - Prompt if unsaved changes
- [ ] Implement multi-file tabs:
  - Switch between open files
  - Ctrl+Tab to cycle tabs

### 5.5 Clipboard
- [ ] Implement copy:
  - Ctrl+C to copy selections
  - Store in system clipboard
- [ ] Implement cut:
  - Ctrl+X to cut selections
- [ ] Implement paste:
  - Ctrl+V to paste at cursors
  - Works with multi-cursor

### 5.6 Visual Enhancements
- [ ] Implement minimap (optional):
  - Small overview of entire file
  - Show visible region
  - Click to jump
- [ ] Implement indent guides:
  - Vertical lines showing indentation
- [ ] Implement bracket matching:
  - Highlight matching brackets
- [ ] Implement whitespace visualization:
  - Toggle to show spaces/tabs

### 5.7 Configuration Enhancements
- [ ] Add command palette:
  - Ctrl+Shift+P to open palette
  - Fuzzy search all actions
  - Show keybindings
- [ ] Add theme picker:
  - Built-in themes (dark, light, etc.)
  - Custom themes in config
- [ ] Add config editor:
  - Ctrl+, to open config.json
  - Validate on save
- [ ] Add keybinding recorder:
  - Record macro sequences
  - Save to config

### 5.8 Documentation
- [ ] Write user manual:
  - Installation instructions
  - Keybinding reference
  - Configuration guide
- [ ] Write developer guide:
  - Architecture overview
  - Event system explanation
  - How to add new features
- [ ] Create example configs:
  - Vim-style keybindings
  - Emacs-style keybindings
  - VSCode-style keybindings
- [ ] Add inline help:
  - F1 to show help
  - Context-sensitive help

## Testing Strategy

### Unit Tests
- [ ] chunk_tree.rs - All operations (already exists)
- [ ] buffer.rs - Insert, delete, line cache
- [ ] cursor.rs - Cursor operations
- [ ] event.rs - Event log, undo/redo
- [ ] config.rs - Loading, validation
- [ ] keybindings.rs - Key resolution
- [ ] state.rs - Event application

### Integration Tests
- [ ] Edit workflow: open → edit → save → quit
- [ ] Multi-cursor workflow: add cursors → edit → undo
- [ ] Search workflow: find → replace → navigate
- [ ] Session workflow: edit → save session → resume

### Performance Tests
- [ ] Keystroke latency benchmark (<1ms)
- [ ] Multi-cursor benchmark (100 cursors <2ms)
- [ ] Large file benchmark (1GB opens <100ms)
- [ ] Syntax highlighting benchmark (50 lines <5ms)
- [ ] Undo/redo benchmark (10k edits instant)

### Manual Testing
- [ ] Test on Linux
- [ ] Test on macOS
- [ ] Test on Windows
- [ ] Test with various terminal emulators
- [ ] Test with various file sizes
- [ ] Test with various languages

## Dependencies (Cargo.toml)

```toml
[dependencies]
# Terminal UI
crossterm = "0.28"
ratatui = "0.29"

# Text processing
tree-sitter = "0.22"
tree-sitter-rust = "0.23"
tree-sitter-javascript = "0.23"
tree-sitter-python = "0.23"
tree-sitter-json = "0.23"
tree-sitter-highlight = "0.24"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Error handling
anyhow = "1.0"
thiserror = "1.0"

# File watching (hot reload)
notify = "6.0"

# Async runtime (if needed)
# tokio = { version = "1.0", features = ["full"] }

[dev-dependencies]
tempfile = "3.15"
criterion = "0.5"  # For benchmarks
```

## Project Structure

```
src/
├── main.rs           # CLI entry point, arg parsing
├── editor.rs         # Editor struct, event loop, rendering
├── event.rs          # Event enum, EventLog, snapshots
├── state.rs          # EditorState, apply() implementation
├── buffer.rs         # Buffer, line cache, file I/O
├── cursor.rs         # Cursor, CursorId
├── viewport.rs       # Viewport, smart scrolling
├── highlighter.rs    # Syntax highlighting, tree-sitter
├── config.rs         # Config loading/saving, validation
├── keybindings.rs    # Action enum, KeybindingResolver
├── theme.rs          # Theme application, color conversion
├── render.rs         # Rendering helpers
├── chunk_tree.rs     # (existing) Rope/ChunkTree implementation
└── lib.rs            # Library exports (for testing)

tests/
├── integration_tests.rs
├── performance_tests.rs
└── fixtures/
    ├── test_files/
    └── configs/

benches/
└── editor_bench.rs

examples/
└── example_config.json
```

## Success Criteria

### Phase 0 Complete
- ✅ All architecture docs reviewed and approved
- ✅ Old code deleted, fresh start
- ✅ Event system implemented and tested
- ✅ Config system loads/saves/validates
- ✅ Buffer with line cache works for large files

### Phase 1 Complete
- ✅ Can open, edit, and save text files
- ✅ Basic navigation works (arrows, home, end, page up/down)
- ✅ Typing appears instantly (<1ms)
- ✅ Undo/redo works correctly
- ✅ Smart scroll keeps cursor visible
- ✅ Status bar shows file info

### Phase 2 Complete
- ✅ Multiple cursors work (Ctrl+D, Alt+Click)
- ✅ Multi-cursor editing is fast (100 cursors <2ms)
- ✅ All cursors visible when possible
- ✅ Undo/redo works with multi-cursor

### Phase 3 Complete
- ✅ Syntax highlighting works (Rust, JS, Python, JSON)
- ✅ Colors from theme config applied
- ✅ Highlighting doesn't block editing (<5ms)
- ✅ Cache invalidation works correctly

### Phase 4 Complete
- ✅ 1GB files open instantly (<100ms)
- ✅ Edit latency <1ms for any file size
- ✅ Memory usage <50MB
- ✅ Undo 10k edits instantly
- ✅ Session save/resume works

### Phase 5 Complete
- ✅ Search/replace works
- ✅ Line operations work
- ✅ Clipboard works
- ✅ Documentation complete
- ✅ All tests passing
- ✅ Ready for users

## Timeline Estimate

- **Phase 0**: 3-4 days (foundation)
- **Phase 1**: 2-3 days (core editor)
- **Phase 2**: 1 day (multi-cursor)
- **Phase 3**: 1 day (syntax highlighting)
- **Phase 4**: 1 day (optimization)
- **Phase 5**: Ongoing (features & polish)

**Total**: ~10 days for production-ready editor, then ongoing improvements

## Notes

- This is a complete rewrite - no backwards compatibility with old code
- Event log is the **only** way to modify state - no exceptions
- Smart scroll is automatic - triggered by all edit events
- Config is hot-reloadable - changes apply immediately
- Performance is critical - profile and optimize continuously
- Tests are essential - write them as you go
- Document as you build - future you will thank you

---

**Ready to build the best terminal editor ever? Let's go! 🚀**
