# Editor Implementation Plan

## Implemented Features (Phase 0-2.2 Complete)

- **Event-driven architecture**: Lossless history with undo/redo
- **Multiple cursors**: Ctrl+D (next match), Ctrl+Alt+Up/Down (add above/below), Esc (remove secondary)
- **Smart scrolling**: Both vertical (with scroll offset) and horizontal (for long lines)
- **File operations**: Open, edit, save with dirty tracking
- **Multiple buffers**: Tab-based interface
- **Clipboard**: Copy/paste between buffers and cursors
- **Help system**: Ctrl+H shows all keybindings
- **Configuration**: JSON-based config with keybindings, theme, editor settings
- **High performance**: ChunkTree buffer, line cache, <1ms operations
- **Testing**: 34 E2E tests, property tests, benchmarks

## Current Status

**Phase**: 2.2.1 Complete ✅
**Tests**: 40 passing (40 E2E + unit + property tests)
**Next**: Phase 2.3 - Advanced Selection (or Phase 3 - Syntax Highlighting)

---

## Phase 2.2.1: Minibuffer/Prompt Infrastructure ✅ COMPLETE

**Implemented**: Full minibuffer/prompt system following Emacs philosophy.

### What Was Built

**Core Infrastructure:**
- ✅ `PromptType` enum with OpenFile, SaveFileAs, Search, Replace, Command
- ✅ `Prompt` struct with message, input, cursor_pos, prompt_type
- ✅ `prompt: Option<Prompt>` field in Editor
- ✅ Prompt control methods (start, cancel, confirm, is_prompting, prompt_input, prompt_mut)
- ✅ Prompt rendering with yellow background in status bar
- ✅ Prompt input handling (typing, backspace, cursor movement, Home/End)

**Key Refactoring:**
- ✅ Centralized `Editor::handle_key()` method consolidates all key handling
- ✅ Main.rs and test harness now delegate to Editor, ensuring identical behavior
- ✅ Eliminated ~200 lines of duplicated code

**Interactive Commands:**
- ✅ Open file (Ctrl+O) - prompts for filename, opens on Enter, cancels on Escape
- ✅ Error handling for non-existent files

**Tests:**
- ✅ test_prompt_rendering - verifies yellow background and message display
- ✅ test_prompt_input_handling - tests typing, backspace, Home/End, cursor movement
- ✅ test_prompt_cancel - ensures Escape cancels and shows "Canceled" message
- ✅ test_open_file_workflow - end-to-end test of Ctrl+O → type path → Enter → file opens
- ✅ test_open_nonexistent_file - verifies error message for invalid paths

**Future Commands (TODO):**
- [ ] Save as (Ctrl+Shift+S) - save active buffer to new path
- [ ] Search (Ctrl+F) - find text in buffer
- [ ] Replace (Ctrl+H) - find and replace text
- [ ] Command palette (Ctrl+Shift+P) - fuzzy search all commands

---

## Phase 2: Multi-Cursor & Advanced Editing

### 2.3 Advanced Selection
- [ ] Implement select word (double-click or Ctrl+W)
- [ ] Implement select line (Ctrl+L)
- [ ] Implement expand selection (Ctrl+Shift+→)
- [ ] Implement rectangular selection (Alt+drag)

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

## Phase 4: LSP Integration

### 4.1 LSP Client (`lsp.rs`)
- [ ] Implement JSON-RPC protocol over stdin/stdout
- [ ] Implement initialize, did_open, did_change, shutdown
- [ ] Handle request/response tracking
- [ ] Handle server lifecycle (crash detection, restart)

### 4.2 Basic LSP Features
- [ ] Diagnostics (inline squiggly underlines)
- [ ] Completion (popup with fuzzy filter)
- [ ] Convert events to LSP changes

### 4.3 Advanced LSP Features
- [ ] Go-to-definition (Ctrl+B or F12)
- [ ] Hover documentation (Ctrl+K Ctrl+I)
- [ ] Code actions (lightbulb menu)

### 4.4 LSP Manager
- [ ] One server per language
- [ ] Route requests to appropriate server
- [ ] Configure in config.json

---

## Phase 5: Polish & Optimization

### 5.1 Search & Replace
- [ ] Search (Ctrl+F) with regex support
- [ ] Replace (Ctrl+H) with preview

### 5.2 Command Palette
- [ ] Fuzzy search all actions (Ctrl+Shift+P)
- [ ] Show keybindings

### 5.3 File Explorer
- [ ] Simple file tree in sidebar (Ctrl+B)

### 5.4 Performance Optimization
- [ ] Profile hot paths
- [ ] Test with 1GB+ files
- [ ] Measure keystroke latency (<1ms target)

### 5.5 User Experience
- [ ] Improve error messages
- [ ] Confirmation dialogs (quit without saving)
- [ ] Progress indicators (loading large files)
- [ ] Welcome screen and default config generation

---

## Phase 6: Advanced Features (Future)

- [ ] Themes (load from JSON)
- [ ] Macros (record/play)
- [ ] Split views (horizontal/vertical)
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

## Timeline Estimate

- **Phase 0-2.2**: ✅ Complete
- **Phase 2.3-2.4**: 1-2 days (next)
- **Phase 3**: 1 day
- **Phase 4**: 2-3 days
- **Phase 5**: 1-2 days
- **Total to production**: ~7-10 days remaining
