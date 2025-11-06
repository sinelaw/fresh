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

### Line Wrapping Refactoring
- [ ] **Unify wrapping and no-wrapping code paths**: Treat no-wrapping as infinite-width wrapping
  - Modify rendering to always use `wrap_line()` with `WrapConfig::new(usize::MAX, gutter_width, false)` for no-wrap mode
  - Remove all `if line_wrap` branches in `split_rendering.rs::render_buffer_in_split()`
  - Handle horizontal scrolling as post-processing on the single segment returned for infinite-width lines

- [ ] **Move cursor position calculation into rendering traversal**: Eliminate duplicate line iteration
  - In `split_rendering.rs::render_buffer_in_split()`, track cursor screen position during the existing line rendering loop (lines 344-629)
  - As each line is rendered, check if it contains the primary cursor position
  - Use the already-computed `segments` from `wrap_line()` to calculate position via `char_position_to_segment()`
  - After loop completes, use tracked position instead of calling `viewport.cursor_screen_position()`
  - Delete `viewport.rs::cursor_screen_position()` entirely

- [ ] **Fix style preservation during wrapping**: Currently loses syntax highlighting/selection styles when wrapping
  - In wrapping section (lines 586-620), preserve the original `line_spans` styling instead of using only first span's style
  - Track character-to-span mapping to apply correct styles to each character in wrapped segments
  - Ensure selections, syntax highlighting, and overlays render correctly across wrapped segments

**Benefits**: Single source of truth for wrapping (line_wrapping.rs), single line traversal (better performance), cursor positioning and rendering always agree by construction, massive code deduplication.

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

#### **High Priority: Buffer & Editor State API (Querying)** ✅ COMPLETE
*Required for almost all plugin functionality.*
- [x] `editor.get_buffer_content(buffer_id)` & `editor.get_line(buffer_id, line_num)`
- [x] `editor.get_selection()` & `editor.get_all_cursors()` (implemented as `editor.get_primary_cursor()` & `editor.get_all_cursors()`)
- [x] `editor.get_active_buffer_id()` & `editor.get_buffer_info(buffer_id)` & `editor.list_buffers()`
- [x] `editor.get_viewport()`

#### **High Priority: Async Task & Process API** ✅ COMPLETE
*Essential for git operations and external tools (`fd`, `rg`).*
- [x] Core infrastructure: `spawn_plugin_process()` function with tokio
- [x] `AsyncMessage::PluginProcessOutput` for result delivery
- [x] Process callback execution in PluginManager
- [x] Lua binding for `editor.spawn(command, args, callback)`
- [x] Working directory control via `{cwd = "/path"}` option
- [x] Full test coverage (2 Lua tests + 4 process tests)
- [x] Example plugin: `async_demo.lua` with 7 commands
- [ ] Process cancellation/kill support (TODO: Phase 3)
- [ ] `editor.async(function)` for running Lua asynchronously (TODO: Phase 3)

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

### Implemented Example Plugins ✅

**1. TODO Highlighter** (`plugins/todo_highlighter.lua`) - **COMPLETE**
   - Uses: Buffer query API, overlay API, command registration
   - Features: Pattern matching for TODO/FIXME/HACK/NOTE/XXX/BUG in comments
   - Color-coded highlights, multiple comment styles, toggle/refresh commands
   - Demonstrates: Real-world useful plugin with Phase 2 APIs
   - Status: Fully functional, tested, documented

### Example Target Plugins (Future)

Once Phase 3 features are complete, these advanced plugins should be possible:

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
