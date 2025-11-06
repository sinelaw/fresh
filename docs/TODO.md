# TODO

## Completed Features ✅

Core editing, multi-cursor, event-driven architecture, LSP integration (diagnostics, completion, go-to-def), file explorer with gitignore support, split views, syntax highlighting, command palette, configuration system, themes, position history, comprehensive testing.

**Plugin System (Phase 1)**: Lua 5.4 runtime, plugin manager, command registration, hook system (event-driven), callback execution, debug logging (auto-opens in background tab), basic editor API (insert, set_status, overlays).

## Current Focus

### File Explorer Polish
- [ ] Input dialog system for custom file/directory names
- [ ] Copy/move operations
- [ ] File watching for auto-refresh
- [ ] Search/filter within explorer

### LSP Features
- [ ] Hover documentation
- [ ] Code actions
- [ ] Find references
- [ ] Rename refactoring
- [ ] Signature help
- [ ] Inlay hints

### Editing Features
- [ ] Search & replace with regex
- [ ] Rectangular selection (Alt+drag)
- [ ] Auto-indent on newline
- [ ] Bracket matching/auto-close
- [ ] Smart home key
- [ ] Toggle comment (language-aware)

### Test Infrastructure
- [ ] **Fix async file loading in test harness**: Currently 6 tests are ignored due to async file loading not working properly in tests:
  - `test_file_explorer_displays_opened_file_content` - file explorer doesn't load file content synchronously
  - `test_git_find_file_actually_opens_file` - git find file doesn't load buffer content
  - `test_git_grep_opens_correct_file_and_jumps_to_line` - git grep doesn't load file
  - `test_git_grep_cursor_position_accuracy` - git grep doesn't load file
  - `test_git_grep_shows_results` - git grep doesn't show file content
  - The test harness needs a way to properly wait for/force async file operations to complete

- [ ] **Fix BIG.txt generation timing**: 2 scrolling tests fail when run with other tests:
  - `test_jump_to_eof_large_file` - passes individually, fails in suite
  - `test_line_numbers_absolute_after_jump_to_beginning` - passes individually, fails in suite
  - Issue: BIG.txt (61MB test file) generation interferes with other tests
  - Solution: Better test isolation or pre-generated fixtures

- [ ] **Support independent buffers per split**: Currently architectural limitation:
  - `test_margin_per_buffer_in_split_view` - expects different files in different splits
  - Current behavior: All splits display the same active buffer
  - Need to implement per-split buffer management if this is desired functionality

### Code Organization
- [x] Extract UI rendering (~430 lines → 6 modules)
- [x] Extract commands & prompts (~335 lines → 2 modules)
- [ ] Create BufferView (~500 lines)
- [ ] Extract multi-cursor operations (~200 lines)

### Polish
- [ ] Improve error messages
- [ ] Confirmation dialogs
- [ ] Progress indicators
- [ ] Welcome screen
- [ ] More themes

## Plugin System Roadmap

The primary goal of the plugin system is to enable powerful, interactive, and asynchronous plugins similar to **Emacs' Magit** (Advanced Git Interface) and **Neovim's Telescope** (Fuzzy Finder), as identified in our design research. The roadmap is now prioritized to deliver the core APIs for these high-impact plugins first.

### Phase 1: Core Infrastructure ✅ COMPLETE
- [x] Core plugin infrastructure (PluginManager, HookRegistry, CommandRegistry)
- [x] Lua 5.4 runtime integration
- [x] Basic plugin API (set_status, insert, register_command, overlays)
- [x] Command registration and palette integration
- [x] Event-driven hooks and Lua callback execution

### Target Plugin Capabilities
- **Magit-style Git Interface:** Interactive git workflow (status, log, diff, commit, rebase) using virtual buffers, async processes, and modal keybindings.
- **Telescope-style Fuzzy Finder:** A unified, high-performance fuzzy finder for files, buffers, commands, and git branches, using a flexible popup UI and async finders.
- **Undo Tree Visualizer:** A graphical, branching undo history, requiring virtual buffers and access to the undo tree.

---

### **Phase 2: Core APIs for Advanced Plugins**
*Consolidates the essential features required to build initial versions of Magit and Telescope.*

#### **High Priority: Buffer & Editor State API (Querying)**
*Required for almost all plugin functionality.*
- [ ] `editor.get_buffer_content(buffer_id)` & `editor.get_line(buffer_id, line_num)`
- [ ] `editor.get_selection()` & `editor.get_all_cursors()`
- [ ] `editor.get_active_buffer_id()` & `editor.get_buffer_info(buffer_id)`
- [ ] `editor.get_viewport()`

#### **High Priority: Async Task & Process API**
*Essential for git operations and external tools (`fd`, `rg`).*
- [ ] `editor.spawn(command, args, callback)` - Spawn async process with stdout/stderr streaming.
- [ ] Process cancellation/kill support.
- [ ] Working directory control.
- [ ] `editor.async(function)` for running Lua asynchronously.

#### **High Priority: Interactive UI API**
*Required for interactive selection, dialogs, and custom views.*
- **Virtual Buffers:**
    - [ ] `editor.create_virtual_buffer(name, content)` - For `*magit-status*`.
    - [ ] `editor.set_buffer_content(buffer_id, content)`.
    - [ ] `editor.set_buffer_read_only(buffer_id, read_only)`.
- **Popups:**
    - [ ] `editor.show_selection_list(items, callback)` - For fuzzy finders.
    - [ ] `editor.show_input(prompt, default, callback)` - For commit messages.
    - [ ] `editor.show_popup(options)` - Generic popup for custom UIs.

#### **High Priority: Modal Interaction & Navigation**
*Needed for mode-specific behavior (Magit) and buffer manipulation.*
- **Modes & Keybindings:**
    - [ ] `editor.define_mode(mode_name, options)` & `editor.set_mode(buffer_id, mode_name)`.
    - [ ] `editor.bind_key(mode, key, callback)`.
- **Navigation & Manipulation:**
    - [ ] `editor.goto_line(line_num)` / `editor.goto_position(byte_offset)`.
    - [ ] `editor.set_selection(start, end)`.
    - [ ] `editor.scroll_to_line(line_num)`.

---

### **Phase 3: Nice-to-Have & Future Enhancements**
*Features to improve power, robustness, and developer experience.*

#### **Medium Priority: Enhanced Hooks, State, and Integration**
- [ ] **More Hooks:** `on_buffer_open`, `on_selection_change`, `on_key_press`.
- [ ] **State Persistence:** `editor.get/set_plugin_data(key, value)` with persistence.
- [ ] **Plugin Configuration:** Support for user config files.
- [ ] **Advanced Editor Integration:**
    - [ ] LSP access: `editor.lsp_call(...)`.
    - [ ] Search/Replace API: `editor.search(...)`.
    - [ ] Undo History API: `editor.get_undo_history(...)`.
    - [ ] Custom Syntax: `editor.add_syntax_definition(...)`.

#### **Low Priority: Optimization & Debugging**
- [ ] **Performance:** Incremental/stream-based buffer APIs, caching.
- [ ] **Debugging:** Lua REPL, improved error handling, hot-reloading.

#### **Future: WASM Plugin Support**
- [ ] WASM runtime integration as an alternative to Lua.

### Example Target Plugins

Once the above features are complete, these should be possible:

**1. Undo Tree Visualizer** - Buffer query, virtual buffers, custom keybindings, undo history access. Shows branching undo history graphically, navigate and jump to any state.

**2. Magit (Git Interface)** - Async processes, popups, custom modes, virtual buffers. Git status, log, diff, commit, rebase, etc. Full interactive git workflow.

**3. Project Search & Replace** - Async search, virtual buffers, multi-buffer operations. Ripgrep integration, preview and apply changes.

**4. LSP Code Actions Menu** - LSP integration, popups, buffer modification. Custom code action UI, refactoring tools.

**5. Snippet System** - Buffer modification, keybindings, state management. Tabstops and placeholders, custom snippet definitions.

**6. File Tree / Explorer** - Virtual buffers, custom keybindings, async file operations. Navigate file system, file operations (create, delete, rename).

### Testing & Documentation Strategy

For each new API phase:
- [ ] Unit tests for Rust implementation
- [ ] Integration tests with actual Lua plugins
- [ ] Example plugins demonstrating features
- [ ] Performance benchmarks for critical paths
- [ ] Complete API reference documentation
- [ ] Plugin development guide
- [ ] Best practices document

---

## Future Ideas

- Macros (record/play)
- Git integration (status, blame, stage hunks) - *may be implemented as plugins*
- Remote file editing (SSH, SFTP)
- Collaborative editing
