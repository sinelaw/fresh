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

## Current Status

**Phase**: 4.1 Complete ✅ (LSP Client - Basic Integration)
**Tests**: 59 passing (59 E2E + unit + property tests)
**Next**: Phase 4.2 - Diagnostics Display & Completion UI
**LSP**: rust-analyzer integration working - didOpen/didChange notifications functional

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

## Phase 4: LSP Integration

### 4.1 LSP Client (`lsp.rs`) ✅
- [x] Implement JSON-RPC protocol over stdin/stdout ✅
- [x] Implement initialize, did_open, did_change, shutdown ✅
- [x] Handle request/response tracking ✅
- [x] Handle server lifecycle (crash detection, restart) ✅

### 4.2 Basic LSP Features
- [x] Diagnostics receiving (stored in LspClient) ✅
- [ ] Diagnostics display (inline squiggly underlines) - rendering not yet implemented
- [ ] Completion (popup with fuzzy filter) - not yet implemented
- [x] Convert events to LSP changes (full document sync) ✅

### 4.3 Advanced LSP Features
- [ ] Go-to-definition (Ctrl+B or F12)
- [ ] Hover documentation (Ctrl+K Ctrl+I)
- [ ] Code actions (lightbulb menu)

### 4.4 LSP Manager ✅
- [x] One server per language ✅
- [x] Route requests to appropriate server ✅
- [x] Configure in config.json ✅
- [x] Integrated into Editor with didOpen/didChange ✅

---

## Phase 5: Polish & Optimization

### 5.1 Search & Replace
- [ ] Search (Ctrl+F) with regex support
- [ ] Replace (Ctrl+H) with preview

### 5.2 Command Palette
- [x] Fuzzy search all actions (Ctrl+P) ✅ Complete
- [x] Show keybindings ✅ Complete (via Show Help command)

### 5.3 Keybinding System Refactoring
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

### 6.1 Theme System
- [ ] Create Theme struct with all color definitions (background, foreground, selection, cursor, status bar, prompt, suggestions, etc.)
- [ ] Replace all hardcoded Color/Style references throughout codebase with theme lookups
- [ ] Make theme replaceable at runtime (store in Editor context)
- [ ] Load themes from JSON configuration
- [ ] Support multiple built-in themes (dark, light, high contrast)

### 6.2 Other Advanced Features
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
- **Phase 2.3**: ✅ Complete (Advanced Selection)
- **Phase 2.4**: 1 day (Smart Editing - next for multi-cursor features)
- **Phase 3**: 1 day (Syntax Highlighting)
- **Phase 4.1**: ✅ Complete (LSP Client & Basic Integration)
- **Phase 4.2-4.3**: 2-3 days (Diagnostics UI, Completion, Advanced LSP)
- **Phase 5**: 1-2 days (Polish & Optimization)
- **Total to production**: ~5-7 days remaining
