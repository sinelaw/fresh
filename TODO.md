# Editor Implementation Plan

## Overview

Building a high-performance terminal text editor from scratch with:
- **Arbitrary large file support** (GB+ files)
- **Ultra-low latency** (<1ms per operation)
- **Multiple cursors** (Sublime/VSCode style)
- **Best-effort syntax highlighting** (tree-sitter)
- **Event-driven architecture** (lossless history, undo/redo)
- **JSON configuration** (hot reload, schema validation)
- **Multiple file support** (tabs/buffers)
- **Clipboard** (copy/paste between buffers)
- **LSP integration** (completion, diagnostics, go-to-definition)

## Architecture Documents

- [NEW_ARCHITECTURE.md](NEW_ARCHITECTURE.md) - Core design and data structures
- [EVENT_LOG_ARCHITECTURE.md](EVENT_LOG_ARCHITECTURE.md) - Event system and smart scrolling
- [CONFIG_SYSTEM.md](CONFIG_SYSTEM.md) - Configuration and keybindings
- [LSP_ARCHITECTURE.md](LSP_ARCHITECTURE.md) - LSP client integration
- [ARCHITECTURE_ANALYSIS.md](ARCHITECTURE_ANALYSIS.md) - Analysis of old code (reference)
- [DETAILS.md](DETAILS.md) - Technical summary of original codebase

## Progress Summary

**Current Status**: Phase 0 Complete ✅ (86 tests passing)

**Commits**:
- ✅ Phase 0: Foundational modules complete (c74b15d)
- ✅ Add EditorState with event application logic (a4404ab)
- ✅ Update architecture: Add LSP, multi-buffer, and clipboard support (6729178)

---

## Phase 0: Foundation & Architecture ✅ COMPLETE

### 0.1 Clean Slate ✅
- ✅ Review all architecture documents and finalize design decisions
- ✅ Delete obsolete modules: virtual_file.rs, lines.rs, memstore.rs, logs.rs
- ✅ Update `Cargo.toml` dependencies:
  - ✅ Add: `serde`, `serde_json` (config)
  - ✅ Keep: `crossterm`, `ratatui`, `tree-sitter-*`

### 0.2 Event System (`event.rs`) ✅
- ✅ Define `Event` enum with all event types
- ✅ Implement `EventLog` struct with undo/redo
- ✅ Implement event persistence (save/load JSON Lines format)
- ✅ Implement `LogEntry` with timestamps
- ✅ Add serialization support
- ✅ Write unit tests for EventLog (5 tests passing)

### 0.3 Configuration System (`config.rs`) ✅
- ✅ Define `Config` struct with theme, editor, keybindings, languages
- ✅ Implement config loading/saving (JSON)
- ✅ Implement validation
- ✅ Implement defaults with sensible keybindings
- ✅ Write unit tests (4 tests passing)

### 0.4 Keybinding System (`keybindings.rs`) ✅
- ✅ Define `Action` enum (40+ actions)
- ✅ Implement `KeybindingResolver`
- ✅ Support modifier keys (Ctrl, Alt, Shift)
- ✅ Parse action strings from config
- ✅ Write unit tests (4 tests passing)

### 0.5 Buffer & Line Cache (`buffer.rs`) ✅
- ✅ Implement `Buffer` struct with ChunkTree backend
- ✅ Implement `LineCache` for O(log n) line lookups
- ✅ Implement insert/delete operations
- ✅ Implement line<->byte conversion
- ✅ Implement file load/save
- ✅ Implement word boundary navigation
- ✅ Write unit tests (9 tests passing)

### 0.6 Cursor System (`cursor.rs`) ✅
- ✅ Implement `Cursor` struct (position, anchor, sticky_column)
- ✅ Implement `Cursors` collection for multi-cursor
- ✅ Implement cursor adjustment after edits
- ✅ Implement normalization (merge overlapping)
- ✅ Support selections
- ✅ Write unit tests (7 tests passing)

### 0.7 Viewport & Smart Scrolling (`viewport.rs`) ✅
- ✅ Implement `Viewport` struct
- ✅ Implement ensure_visible (smart scroll)
- ✅ Implement multi-cursor aware scrolling
- ✅ Implement cursor screen position calculation
- ✅ Write unit tests (6 tests passing)

### 0.8 Editor State (`state.rs`) ✅
- ✅ Implement `EditorState` struct (Buffer + Cursors + Viewport)
- ✅ Implement `apply(event)` - THE ONLY WAY TO MODIFY STATE
- ✅ Integrate smart scrolling into event application
- ✅ Implement cursor adjustment on Insert/Delete
- ✅ Write unit tests (7 tests passing)

**Phase 0 Milestone**: All foundation complete. 86 tests passing. ✅

---

## Phase 1: Core Editor (2-3 days) 🔄 IN PROGRESS

### 1.1 Editor Structure (`editor.rs`)
- [ ] Define `Editor` struct:
  - `buffers: HashMap<BufferId, EditorState>` (multiple open files)
  - `active_buffer: BufferId`
  - `event_logs: HashMap<BufferId, EventLog>` (per-buffer undo/redo)
  - `config: Config`
  - `keybindings: KeybindingResolver`
  - `clipboard: String` (shared across buffers)
  - `terminal: Terminal` (ratatui)
  - `should_quit: bool`
- [ ] Implement `Editor::new(config: Config) -> Result<Self>`
- [ ] Implement buffer management:
  - `open_file(&mut self, path: &Path) -> Result<BufferId>`
  - `close_buffer(&mut self, id: BufferId) -> Result<()>`
  - `switch_buffer(&mut self, id: BufferId)`
  - `next_buffer(&mut self)`, `prev_buffer(&mut self)`
- [ ] Implement clipboard operations:
  - `copy_selection(&mut self, buffer_id: BufferId)`
  - `cut_selection(&mut self, buffer_id: BufferId)`
  - `paste(&mut self, buffer_id: BufferId)`

### 1.2 Event Loop
- [ ] Implement main event loop:
  - `run(&mut self) -> Result<()>`
  - Poll for keyboard events (crossterm)
  - Handle resize events
  - Timeout for periodic tasks (diagnostics refresh)
- [ ] Handle terminal setup/cleanup:
  - Enter raw mode
  - Enable mouse support (optional)
  - Restore terminal on exit
- [ ] Handle panics gracefully (cleanup terminal)

### 1.3 Action to Events Conversion
- [ ] Implement `action_to_events()`:
  - Map each `Action` to one or more `Event`s
  - Handle cursor-based actions (move, select)
  - Handle edit actions (insert char, delete)
  - Handle multi-cursor actions (apply to all cursors)
- [ ] Examples:
  - `Action::InsertChar('a')` → `Event::Insert { position, text: "a", cursor_id }`
  - `Action::MoveLeft` → `Event::MoveCursor { cursor_id, position: pos-1, ... }`
  - `Action::SelectAll` → Multiple events for cursor position and anchor
  - `Action::DeleteSelection` → Multiple `Event::Delete` for each cursor
- [ ] Apply events through EventLog:
  - `log.append(event)`
  - `state.apply(&event)`
- [ ] Implement undo/redo:
  - `Action::Undo` → `log.undo()`, apply inverse events
  - `Action::Redo` → `log.redo()`, reapply events

### 1.4 Basic Rendering (no syntax highlighting)
- [ ] Implement main render function:
  - `render(&mut self, frame: &mut Frame)`
- [ ] Render buffer content:
  - Get visible lines from viewport
  - Draw line numbers (if enabled)
  - Draw text content
- [ ] Render cursors:
  - Calculate cursor screen positions
  - Draw cursor blocks/lines
- [ ] Render selections:
  - Highlight selected text
- [ ] Render status bar:
  - File name, line/col, dirty indicator
  - Mode (insert/normal if modal)
- [ ] Render multiple buffers (tabs):
  - Tab bar showing open files
  - Active tab highlighted
- [ ] Handle long lines (horizontal scroll)

### 1.5 Basic File Operations
- [ ] Implement file open from CLI args:
  - `editor file.txt` → open file
  - `editor` → empty buffer
- [ ] Implement save:
  - `Action::Save` → `buffer.save()`
  - Show confirmation in status bar
  - Handle errors (permission denied, etc.)
- [ ] Implement save-as:
  - Prompt for file name (simple input)
  - Save to new path
- [ ] Implement quit:
  - Check for unsaved changes
  - Prompt user (Save/Discard/Cancel)

**Phase 1 Milestone**: Can open file, edit text, move cursor, save, quit. Basic usable editor.

---

## Phase 2: Multi-Cursor & Advanced Editing (1-2 days)

### 2.1 Multi-Cursor Keybindings
- [ ] Implement Ctrl+D (add cursor at next match):
  - Find next occurrence of selected text
  - Add cursor there
  - Normalize cursors
- [ ] Implement Ctrl+Alt+Up/Down (add cursor above/below):
  - Add cursor at same column on adjacent line
- [ ] Implement Esc (remove secondary cursors):
  - Keep only primary cursor
- [ ] Test multi-cursor editing:
  - Type with multiple cursors
  - Delete with multiple cursors
  - Move all cursors together

### 2.2 Advanced Selection
- [ ] Implement select word (double-click or Ctrl+W)
- [ ] Implement select line (Ctrl+L)
- [ ] Implement expand selection (Ctrl+Shift+→)
- [ ] Implement rectangular selection (Alt+drag)

### 2.3 Smart Editing
- [ ] Implement auto-indent on newline
- [ ] Implement bracket matching/auto-close
- [ ] Implement smart home (toggle between line start and first non-whitespace)
- [ ] Implement toggle comment (language-aware)

**Phase 2 Milestone**: Full multi-cursor support. Advanced selection and editing.

---

## Phase 3: Syntax Highlighting (1 day)

### 3.1 Highlighter (`highlighter.rs`)
- [ ] Implement `Highlighter` struct:
  - `parser: tree_sitter::Parser`
  - `config: HighlightConfiguration`
  - `cache: HighlightCache`
- [ ] Implement highlighting:
  - `highlight(&mut self, text: &str, range: Range<usize>) -> Vec<Span>`
  - Parse only visible range
  - Use 5ms timeout (best-effort)
  - Cache results
- [ ] Implement cache invalidation:
  - Invalidate on edits
  - Keep cache for visible range only
- [ ] Integrate into rendering:
  - Apply syntax colors when drawing text

### 3.2 Language Detection
- [ ] Implement language detection from file extension
- [ ] Load appropriate tree-sitter grammar
- [ ] Configure in config.json:
  - Map extensions to languages
  - Map languages to grammars
- [ ] Support multiple languages:
  - Rust (rust-analyzer grammar)
  - JavaScript/TypeScript
  - Python
  - JSON
  - Markdown

**Phase 3 Milestone**: Pretty colored code with tree-sitter.

---

## Phase 4: LSP Integration (2-3 days)

### 4.1 LSP Client (`lsp.rs`)
- [ ] Implement JSON-RPC protocol over stdin/stdout
- [ ] Implement `LspClient` struct:
  - `spawn(command: &str) -> Result<Self>`
  - `initialize(root_uri: &str) -> Result<()>`
  - `did_open(uri: &str, text: &str, language_id: &str)`
  - `did_change(uri: &str, changes: Vec<Change>)`
  - `shutdown() -> Result<()>`
- [ ] Implement request/response handling:
  - Track pending requests by ID
  - Handle server notifications (diagnostics)
  - Timeout long-running requests
- [ ] Handle server lifecycle:
  - Detect server crash
  - Offer to restart
  - Graceful degradation

### 4.2 Basic LSP Features
- [ ] Implement diagnostics:
  - Request on file open
  - Request after edits (debounced 500ms)
  - Store per-buffer
  - Render inline (squiggly underlines)
- [ ] Implement completion:
  - Trigger on `.` `:` `>` or Ctrl+Space
  - Show popup below cursor
  - Navigate with arrow keys
  - Insert on Enter/Tab
  - Fuzzy filter as user types
- [ ] Convert events to LSP changes:
  - `Event::Insert` → `TextDocumentContentChangeEvent`
  - `Event::Delete` → `TextDocumentContentChangeEvent`
  - Track document versions

### 4.3 Advanced LSP Features
- [ ] Implement go-to-definition:
  - Keybinding (Ctrl+B or F12)
  - Jump to definition location
  - Track jump history for back navigation
- [ ] Implement hover:
  - Show on keybinding (Ctrl+K Ctrl+I)
  - Render popup with documentation
- [ ] Implement code actions:
  - Show lightbulb when available
  - Menu to select action
  - Apply workspace edit

### 4.4 LSP Manager
- [ ] Implement `LspManager`:
  - Spawn one server per language
  - Route requests to appropriate server
  - Handle multiple files per server
- [ ] Configure in config.json:
  - Map languages to LSP commands
  - Enable/disable per language

**Phase 4 Milestone**: Full IDE-like experience with autocomplete and diagnostics.

---

## Phase 5: Polish & Optimization (1-2 days)

### 5.1 Search & Replace
- [ ] Implement search (Ctrl+F):
  - Input field for search query
  - Highlight all matches
  - Navigate with F3/Shift+F3
  - Regex support
- [ ] Implement replace (Ctrl+H):
  - Replace single occurrence
  - Replace all
  - Preview before replace

### 5.2 Command Palette
- [ ] Implement command palette (Ctrl+Shift+P):
  - Fuzzy search all actions
  - Show keybindings
  - Execute action

### 5.3 File Explorer
- [ ] Implement simple file tree:
  - Show in sidebar
  - Navigate with arrows
  - Open file on Enter
  - Toggle with Ctrl+B

### 5.4 Performance Optimization
- [ ] Profile hot paths:
  - Rendering
  - Line cache rebuilding
  - Syntax highlighting
- [ ] Optimize for large files:
  - Lazy line cache (only visible range)
  - Incremental highlighting
  - Virtual scrolling
- [ ] Test with 1GB+ files
- [ ] Measure keystroke latency (<1ms target)

### 5.5 User Experience
- [ ] Improve error messages
- [ ] Add confirmation dialogs (quit without saving)
- [ ] Add progress indicators (loading large files)
- [ ] Add welcome screen (first run)
- [ ] Generate default config on first run
- [ ] Improve status bar (show more info)

**Phase 5 Milestone**: Production-ready editor. Fast, stable, feature-complete.

---

## Phase 6: Advanced Features (Future)

### 6.1 Themes
- [ ] Load themes from JSON
- [ ] Support multiple color schemes
- [ ] Theme preview

### 6.2 Macros
- [ ] Record macro (q + key)
- [ ] Play macro (@ + key)
- [ ] Store macros in config

### 6.3 Split Views
- [ ] Horizontal split
- [ ] Vertical split
- [ ] Navigate between splits

### 6.4 Git Integration
- [ ] Show git status in gutter
- [ ] Git blame
- [ ] Stage/unstage hunks

### 6.5 More LSP Features
- [ ] Find references
- [ ] Rename refactoring
- [ ] Document formatting
- [ ] Signature help
- [ ] Inlay hints
- [ ] Code lens

---

## Testing Strategy

### Unit Tests (Continuous)
- Event system (5 tests ✅)
- Config loading (4 tests ✅)
- Keybindings (4 tests ✅)
- Buffer operations (9 tests ✅)
- Cursor management (7 tests ✅)
- Viewport scrolling (6 tests ✅)
- State application (7 tests ✅)
- **Target: 90%+ coverage**

### Integration Tests
- [ ] Open file, edit, save, verify on disk
- [ ] Multi-cursor editing across multiple lines
- [ ] Undo/redo complex edit sequences
- [ ] LSP completion and diagnostics
- [ ] Large file performance (1GB file)

### Manual Testing
- [ ] Test with various file types (Rust, JS, Python, etc.)
- [ ] Test error recovery (LSP crash, disk full, etc.)
- [ ] Test on different terminals (iTerm, Alacritty, Terminal.app)
- [ ] Test on different platforms (Linux, macOS)

---

## Dependencies

```toml
[dependencies]
# Terminal
crossterm = "0.28"
ratatui = "0.29"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Syntax highlighting
tree-sitter = "0.24"
tree-sitter-highlight = "0.24"
tree-sitter-rust = "0.23"
tree-sitter-javascript = "0.23"
tree-sitter-typescript = "0.23"
tree-sitter-python = "0.23"

# LSP (Phase 4)
lsp-types = "0.95"
serde_json = "1.0"  # Already included

# Utilities
anyhow = "1.0"      # Error handling
thiserror = "1.0"   # Custom errors
tempfile = "3.15"   # For tests

# Optional
notify = "6.0"      # File watching (config hot reload)
```

---

## Success Criteria

### Phase 1
- ✅ 86 tests passing
- [ ] Can open, edit, save, quit
- [ ] Responsive (no lag)
- [ ] Handles errors gracefully

### Phase 2
- [ ] Multi-cursor editing works smoothly
- [ ] Undo/redo with multiple cursors
- [ ] All 100+ tests passing

### Phase 3
- [ ] Syntax highlighting for Rust, JS, Python
- [ ] No performance degradation
- [ ] Graceful fallback if parsing fails

### Phase 4
- [ ] Working autocomplete
- [ ] Inline diagnostics
- [ ] Go-to-definition
- [ ] Works with rust-analyzer

### Phase 5
- [ ] Opens 1GB+ files instantly
- [ ] <1ms keystroke latency
- [ ] No crashes in normal use
- [ ] All 150+ tests passing

---

## Timeline Estimate

- **Phase 0**: ✅ Complete (3 days)
- **Phase 1**: 2-3 days
- **Phase 2**: 1-2 days
- **Phase 3**: 1 day
- **Phase 4**: 2-3 days
- **Phase 5**: 1-2 days
- **Total**: ~10-14 days to production-ready

---

## Current Focus

**Next Task**: Implement Editor struct and basic event loop (Phase 1.1-1.2)
