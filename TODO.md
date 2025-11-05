# Editor Implementation Plan

> **Maintenance Note**: When features are completed, remove detailed implementation sections and keep only a single-line summary in "Implemented Features" at the top. This keeps the TODO file focused on what's next, not what's done.

## Implemented Features ✅

- **Event-driven architecture**: Lossless history with undo/redo
- **Multiple cursors**: Ctrl+D (next match), Ctrl+Alt+Up/Down (add above/below), Esc (remove secondary)
- **Smart scrolling**: Both vertical (with scroll offset) and horizontal (for long lines)
- **File operations**: Open, edit, save with dirty tracking
- **Multiple buffers**: Tab-based interface
- **Clipboard**: Copy/paste between buffers and cursors
- **Help system**: Ctrl+H shows all keybindings
- **Minibuffer/Prompt system**: Ctrl+O for file open, Escape to cancel, typing support
- **Command palette**: Ctrl+P with fuzzy matching, autocomplete, and all editor commands
- **Advanced selection**: Ctrl+W (select word), Ctrl+L (select line), Ctrl+Shift+→ (expand selection)
- **Configuration**: JSON-based config with keybindings, theme, editor settings
- **High performance**: ChunkTree buffer, line cache, <1ms operations
- **Testing**: 59 E2E tests, property tests, benchmarks
- **LSP Integration (Basic)**: JSON-RPC client, LspManager, rust-analyzer support, didOpen/didChange notifications
- **Core UI Primitives**: Overlay system (decorations), Popup system (floating windows) - Emacs-style general-purpose building blocks
- **Split view system**: Horizontal/vertical splits, navigation, nested splits - foundation for diagnostics panels
- **Theme system**: Comprehensive theming with 3 built-in themes (dark, light, high-contrast), JSON config support

## Current Status

**Phase**: 4.2.5 Complete - Async I/O Architecture ✅
**Tests**: 165 unit tests passing
**Completed**: Tokio async architecture ✅, LSP async client ✅, Async message bridge ✅, LSP diagnostics via async notifications ✅
**Architecture**: Hybrid sync/async - main loop stays synchronous (16ms polling ~60fps), I/O runs in tokio tasks
**Next**: Test with real LSP server (rust-analyzer), then continue with more LSP features

---

## Phase 2: Multi-Cursor & Advanced Editing

### 2.3 Advanced Selection
- [x] Implement select word (Ctrl+W) ✅
- [x] Implement select line (Ctrl+L) ✅
- [x] Implement expand selection (Ctrl+Shift+→) ✅
- [ ] Implement rectangular selection (Alt+drag) - requires mouse event handling infrastructure

### 2.4 Smart Editing
- [ ] Implement auto-indent on newline
- [ ] Implement bracket matching/auto-close
- [ ] Implement smart home (toggle between line start and first non-whitespace)
- [ ] Implement toggle comment (language-aware)

---

## Phase 3: Syntax Highlighting

### 3.1 Highlighter (`highlighter.rs`)
- [ ] Implement `Highlighter` struct with tree-sitter parser
- [ ] Implement best-effort highlighting with 5ms timeout
- [ ] Implement cache with invalidation on edits
- [ ] Integrate into rendering pipeline

### 3.2 Language Detection
- [ ] Implement language detection from file extension
- [ ] Load appropriate tree-sitter grammar
- [ ] Support Rust, JavaScript/TypeScript, Python, JSON, Markdown

---

## Phase 3.5: Core UI Primitives (Emacs Philosophy)

**Goal**: Build general-purpose UI primitives that are LSP-agnostic and reusable

### 3.5.1 Overlay System (`overlay.rs`) ✅
- [x] Overlay struct: position range, priority, face (styling) ✅
- [x] OverlayManager: add, remove, query overlays by position ✅
- [x] Support multiple overlay types: underline (wavy/dotted/dashed), background, foreground ✅
- [x] Z-ordering by priority for overlapping overlays ✅
- [x] Helper methods for common cases (error, warning, info, hint, selection) ✅
- [x] Integrated into EditorState ✅
- [x] Event-driven API (AddOverlay, RemoveOverlay, ClearOverlays) ✅
- [ ] Render overlays in viewport (underlines, highlights, backgrounds) - deferred to Phase 4.2

### 3.5.2 Popup/Floating Window System (`popup.rs`) ✅
- [x] Popup struct: position, size, content, border style ✅
- [x] PopupManager: show, hide, position relative to cursor/point ✅
- [x] Render popup with border and scrolling support ✅
- [x] Handle popup input/navigation (arrow keys, page up/down) ✅
- [x] Auto-positioning to keep popup on screen ✅
- [x] List support with selection and icons ✅
- [x] Integrated into EditorState ✅
- [x] Event-driven API (ShowPopup, HidePopup, PopupSelectNext/Prev, etc.) ✅
- [x] Integrated into Editor::render() ✅
- [x] Keybindings for navigation (Up/Down/PageUp/PageDown/Esc/Enter) ✅
- [x] E2E tests for popups ✅

### 3.5.3 Annotation/Margin System (`margin.rs`)
- [ ] Left margin support for line numbers, symbols, etc.
- [ ] Right margin for additional metadata
- [ ] Gutter annotations (breakpoints, errors, warnings, info)
- [ ] Configurable margin width and content

---

## Phase 4: LSP Integration (Built on Core UI)

### 4.1 LSP Client (`lsp.rs`) ✅
- [x] Implement JSON-RPC protocol over stdin/stdout ✅
- [x] Implement initialize, did_open, did_change, shutdown ✅
- [x] Handle request/response tracking ✅
- [x] Handle server lifecycle (crash detection, restart) ✅

### 4.2 Basic LSP Features (Using Core UI Primitives)
- [x] Async I/O architecture with Tokio ✅
- [x] AsyncBridge for sync/async communication ✅
- [x] Async LSP client (lsp_async.rs) - refactored to use lsp-types properly ✅
- [x] LspManager with async handles ✅
- [x] Diagnostics receiving (via async notifications) ✅
- [x] Diagnostics display via overlays (colored text + underlines) ✅
- [x] LSP diagnostic to overlay conversion (lsp_diagnostics.rs) ✅
- [x] Overlay rendering in viewport (red/yellow/blue colors with underline) ✅
- [x] E2E test for visual diagnostic rendering ✅
- [x] didSave notification support (diagnostics appear after Ctrl+S) ✅
- [x] Split view support (vertical/horizontal) ✅
  - [x] Generic split view system that allows multiple buffers displayed simultaneously ✅
  - [x] Support both vertical and horizontal splits ✅
  - [x] Navigation between splits (Ctrl+o / Ctrl+Shift+o) ✅
  - [x] Keybindings (Alt+h horizontal, Alt+v vertical, Alt+x close, Alt+o navigate) ✅
  - [x] Nested splits (arbitrary depth) ✅
  - [x] 7 E2E tests covering all split functionality ✅
  - [ ] Dedicated diagnostics buffer type (can display in split) - future enhancement
- [ ] Diagnostics in gutter (error/warning icons) - deferred
- [ ] Completion via popup system ← NEXT
- [x] Convert events to LSP changes (full document sync) ✅

### 4.3 Advanced LSP Features
- [ ] Go-to-definition (Ctrl+B or F12)
- [ ] Hover documentation via popup
- [ ] Code actions via popup menu

### 4.4 LSP Manager ✅
- [x] One server per language ✅
- [x] Route requests to appropriate server ✅
- [x] Configure in config.json ✅
- [x] Integrated into Editor with didOpen/didChange/didSave ✅

---

## Phase 5: Polish & Optimization

### 5.1 Search & Replace
- [ ] Search (Ctrl+F) with regex support
- [ ] Replace (Ctrl+H) with preview

### 5.2 Command Palette
- [x] Fuzzy search all actions (Ctrl+P) ✅ Complete
- [x] Show keybindings ✅ Complete (via Show Help command)

### 5.3 Keybinding System Refactoring
- [ ] Design context-aware keybinding system with priority/overlays (Emacs-style)
  - Different keybinding contexts based on UI state (help mode, prompt mode, popup visible, normal mode)
  - Priority-based keybinding resolution (popup keys override normal keys when popup visible)
  - Inspiration: Emacs minor/major modes, modal keymaps
- [ ] Replace hardcoded key event handlers in `Editor::handle_key()` with `KeybindingResolver`
- [ ] Eliminate duplicated key matching logic
- [ ] Make all keybindings customizable via config.json

### 5.4 File Explorer
- [ ] Simple file tree in sidebar (Ctrl+B)

### 5.5 Performance Optimization
- [ ] Profile hot paths
- [ ] Test with 1GB+ files
- [ ] Measure keystroke latency (<1ms target)

### 5.6 User Experience
- [ ] Improve error messages
- [ ] Confirmation dialogs (quit without saving)
- [ ] Progress indicators (loading large files)
- [ ] Welcome screen and default config generation

---

## Phase 6: Advanced Features (Future)

### 6.1 Theme System ✅
- [x] Create Theme struct with all color definitions (background, foreground, selection, cursor, status bar, prompt, suggestions, etc.) ✅
- [x] Replace all hardcoded Color/Style references throughout codebase with theme lookups ✅
- [x] Make theme replaceable at runtime (store in Editor context) ✅
- [x] Load themes from JSON configuration ✅
- [x] Support multiple built-in themes (dark, light, high contrast) ✅

### 6.2 Other Advanced Features
- [ ] Macros (record/play)
- [ ] Git integration (status, blame, stage hunks)
- [ ] More LSP features (find references, rename, format, signature help, inlay hints)

---

## Architecture Documents

- [NEW_ARCHITECTURE.md](NEW_ARCHITECTURE.md) - Core design and data structures
- [EVENT_LOG_ARCHITECTURE.md](EVENT_LOG_ARCHITECTURE.md) - Event system and smart scrolling
- [CONFIG_SYSTEM.md](CONFIG_SYSTEM.md) - Configuration and keybindings
- [LSP_ARCHITECTURE.md](LSP_ARCHITECTURE.md) - LSP client integration
- [TESTING.md](TESTING.md) - Testing strategy

---

---

## Phase 7: Code Organization & Refactoring

### 7.1 Editor.rs Refactoring Plan
**Goal**: Refactor `editor.rs` (3,535 lines) into focused modules separating single-buffer concerns from multi-buffer orchestration.

#### Target Architecture
1. **`editor.rs`** (~1000 lines) - Multi-buffer orchestrator
   - Buffer lifecycle (open, close, switch)
   - Active buffer tracking
   - Global state (clipboard, quit flag, help)
   - Config and keybindings
   - Event logs (one per buffer)
   - Buffer metadata
   - LSP manager
   - Tokio runtime and async bridge
   - Split manager coordination

2. **`buffer_view.rs`** (~500 lines) - Single buffer display & operations
   - Single buffer rendering
   - Cursor position calculations
   - Action-to-events conversion
   - Word boundary detection
   - Selection operations for single buffer

3. **`ui/mod.rs`** and submodules (~800 lines total)
   - `ui/tabs.rs` - Tab bar rendering
   - `ui/status_bar.rs` - Status bar and prompt display
   - `ui/suggestions.rs` - Autocomplete/command palette UI
   - `ui/help.rs` - Help page rendering
   - `ui/split_rendering.rs` - Split layout and separators

4. **`commands.rs`** (~300 lines) - Command system
   - Command definitions
   - Command filtering
   - Command execution routing

5. **`prompt.rs`** (~300 lines) - Prompt/minibuffer system
   - Prompt lifecycle
   - Input handling
   - Suggestion management

6. **`multi_cursor.rs`** (~200 lines) - Multi-cursor operations
   - Add cursor at next match
   - Add cursor above/below

#### Implementation Phases

**Phase 7.1.1: Extract Rendering (~Week 1)** ✅ COMPLETE
- [x] Create `ui/mod.rs` structure
- [x] Create `ui/tabs.rs` - Move `render_tabs`
- [x] Create `ui/status_bar.rs` - Move `render_status_bar`
- [x] Create `ui/suggestions.rs` - Move `render_suggestions`
- [x] Create `ui/help.rs` - Move `render_help`, `scroll_help`, help state
- [x] Create `ui/split_rendering.rs` - Move `render_content`, `render_separator`
- [x] Update `editor.rs` to use new UI modules
- [x] Test rendering still works
**Result**: Extracted ~430 lines of rendering code into 6 focused UI modules

**Phase 7.1.2: Extract Commands & Prompts (~Week 1)** ✅ COMPLETE
- [x] Create `commands.rs`
- [x] Move `Command` struct
- [x] Move `get_all_commands`
- [x] Move `filter_commands`
- [x] Create `prompt.rs`
- [x] Move `Prompt`, `PromptType`, `Suggestion` structs
- [x] Move prompt methods (start, cancel, confirm, update)
- [x] Update `editor.rs` to use new modules
- [x] Test command palette and prompts work
**Result**: Extracted ~335 lines into commands.rs and prompt.rs modules

**Phase 7.1.3: Create BufferView (~Week 2)**
- [ ] Create `buffer_view.rs`
- [ ] Define `BufferView` struct
- [ ] Move `action_to_events` to BufferView
- [ ] Move word boundary helpers (find_word_start, find_word_end, etc.)
- [ ] Move `render_buffer_in_split_static` as BufferView method
- [ ] Update Editor to work with BufferViews
- [ ] Test single buffer operations
- [ ] Test multi-buffer switching

**Phase 7.1.4: Polish & Additional Extractions (~Week 1)**
- [ ] Create `multi_cursor.rs`
- [ ] Move multi-cursor operations
- [ ] Consider extracting LSP integration to `lsp_integration.rs`
- [ ] Update all tests
- [ ] Update documentation
- [ ] Remove dead code
- [ ] Final verification

---

## Timeline Estimate

- **Phase 0-2.2**: ✅ Complete
- **Phase 2.3**: ✅ Complete (Advanced Selection)
- **Phase 2.4**: 1 day (Smart Editing - deferred)
- **Phase 3**: 1 day (Syntax Highlighting - deferred)
- **Phase 3.5**: ✅ Complete (Core UI Primitives)
- **Phase 4.1**: ✅ Complete (LSP Client & Basic Integration)
- **Phase 4.2**: ✅ Complete (LSP Diagnostics via overlays)
- **Phase 4.2.5**: ✅ Complete (Async I/O Architecture)
- **Phase 4.2.6**: ✅ Complete (Split View System)
- **Phase 4.3**: 1-2 days (Advanced LSP features - deferred)
- **Phase 5**: 1-2 days (Polish & Optimization - deferred)
- **Phase 7**: 2-4 weeks (Code Organization & Refactoring) **← CURRENT**
- **Total to production**: ~3-6 weeks remaining
