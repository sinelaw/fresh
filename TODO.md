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

**Phase**: 2.2 Complete ✅
**Tests**: 35 passing (all E2E, unit, property tests)
**Next**: Phase 2.2.1 - Minibuffer/Prompt Infrastructure (CRITICAL - blocks many features)

---

## Phase 2.2.1: Minibuffer/Prompt Infrastructure (CRITICAL)

**Goal**: Implement the minibuffer (input prompt at bottom) - the foundation for all interactive commands following Emacs philosophy.

**Why Critical**: Blocks interactive file opening, search, replace, command palette, save-as, and many other features.

### Architecture
Following Emacs philosophy, the minibuffer is a unified input mechanism:
- Single reusable component for all user input
- Separates core primitives (in Editor) from interactive commands (in event loop/future plugins)
- Commands use primitives + prompt system

### Primitives Needed in Editor Core

**1. Prompt State Management**
- [ ] Add `prompt: Option<Prompt>` field to Editor struct
- [ ] Implement `Prompt` struct with:
  - `message: String` - The prompt text (e.g., "Find file: ")
  - `input: String` - User's current input
  - `cursor_pos: usize` - Cursor position in input
  - `callback_type: PromptType` - What to do when user confirms
- [ ] Implement `PromptType` enum:
  - `OpenFile`
  - `SaveFileAs`
  - `Search`
  - `Replace`
  - `Command` (for M-x command palette)

**2. Prompt Control Methods**
- [ ] `Editor::start_prompt(message, prompt_type)` - Enter prompt mode
- [ ] `Editor::cancel_prompt()` - Cancel and return to normal mode
- [ ] `Editor::confirm_prompt()` - Execute the callback with input
- [ ] `Editor::is_prompting() -> bool` - Check if in prompt mode
- [ ] `Editor::prompt_input() -> Option<&str>` - Get current input

**3. Prompt Rendering**
- [ ] Modify `render_status_bar()` to show prompt when active
- [ ] Format: `[prompt_message][user_input]|` (with cursor)
- [ ] Different styling to distinguish from status bar

**4. Prompt Input Handling**
- [ ] In event loop: Route input to prompt when `is_prompting()`
- [ ] Handle regular chars -> append to input
- [ ] Handle backspace -> remove char
- [ ] Handle Enter -> confirm prompt
- [ ] Handle Escape -> cancel prompt
- [ ] Handle Left/Right -> move cursor in input

### Commands Built on Primitives (in event loop for now)

**Once primitives are ready, implement these commands:**
- [ ] Open file (Ctrl+X Ctrl+F or Ctrl+O)
  - Calls `editor.start_prompt("Find file: ", PromptType::OpenFile)`
  - On confirm: calls `editor.open_file(Path::new(input))`
- [ ] Save as (Ctrl+X Ctrl+W)
  - Calls `editor.start_prompt("Save as: ", PromptType::SaveFileAs)`
  - On confirm: saves active buffer to new path
- [ ] Search (Ctrl+S or Ctrl+F)
  - Calls `editor.start_prompt("Search: ", PromptType::Search)`
  - On confirm: finds and highlights matches
- [ ] Command palette (M-x / Ctrl+Shift+P)
  - Calls `editor.start_prompt("M-x ", PromptType::Command)`
  - On confirm: executes command by name

### Testing Plan
- [ ] Unit tests for Prompt struct
- [ ] E2E test for prompt rendering
- [ ] E2E test for prompt input handling
- [ ] E2E test for open file workflow
- [ ] E2E test for cancel prompt

### Future Enhancement (with plugin system)
When we add Lua/Rhai scripting, these commands move to scripts:
```lua
function find_file()
    editor.prompt("Find file: ", function(input)
        editor.open_file(input)
    end)
end
```

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
