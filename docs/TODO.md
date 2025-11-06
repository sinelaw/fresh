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

### Editor Features
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

### Phase 1: Core Infrastructure ✅ COMPLETE

- [x] Core plugin infrastructure (PluginManager, HookRegistry, CommandRegistry)
- [x] Lua 5.4 runtime integration
- [x] Basic plugin API (set_status, insert, register_command)
- [x] Command registration and palette integration
- [x] Lua callback execution
- [x] Debug logging system (debug() function with auto-opened log file)
- [x] Event-driven hooks (automatic hook invocation from events)
- [x] Visual overlays API

### Phase 2: Buffer Inspection & Query API

**Priority: HIGH** - Required for most interactive plugins

#### Buffer State Query
- [ ] `editor.get_buffer_content(buffer_id)` - Get full buffer text
- [ ] `editor.get_line(buffer_id, line_num)` - Get specific line
- [ ] `editor.get_line_count(buffer_id)` - Get total line count
- [ ] `editor.get_selection()` - Get current selection range
- [ ] `editor.get_cursor_position()` - Get cursor position (line, column, byte offset)
- [ ] `editor.get_all_cursors()` - Get all cursor positions (multi-cursor support)
- [ ] `editor.get_active_buffer_id()` - Get current buffer ID
- [ ] `editor.get_buffer_info(buffer_id)` - Get buffer metadata (file path, modified, language, etc.)
- [ ] `editor.get_viewport()` - Get visible region (top line, height)

#### Buffer Navigation
- [ ] `editor.goto_line(line_num)` - Move cursor to line
- [ ] `editor.goto_position(byte_offset)` - Move cursor to byte offset
- [ ] `editor.set_selection(start, end)` - Set selection range
- [ ] `editor.scroll_to_line(line_num)` - Scroll viewport to line

### Phase 3: Async Task & Process Management

**Priority: HIGH** - Essential for git operations and external tools

#### Process Spawning
- [ ] `editor.spawn(command, args, callback)` - Spawn async process
- [ ] `editor.spawn_shell(command, callback)` - Spawn shell command
- [ ] Process stdout/stderr streaming to callback
- [ ] Process exit code handling
- [ ] Process cancellation/kill support
- [ ] Working directory control
- [ ] Environment variable passing

#### Async Operations
- [ ] `editor.async(function)` - Run Lua function asynchronously
- [ ] Promise/Future-style API for chaining operations
- [ ] Timeout support
- [ ] Progress reporting for long operations

**Example: Magit Log**
```lua
editor.spawn("git", {"log", "--oneline", "-n", "50"}, function(stdout, stderr, exit_code)
    if exit_code == 0 then
        local commits = parse_git_log(stdout)
        display_in_magit_buffer(commits)
    end
end)
```

### Phase 4: Custom UI & Popup System

**Priority: HIGH** - Required for interactive selection and dialogs

#### Popup API
- [ ] `editor.show_popup(options)` - Show custom popup/dialog
- [ ] `editor.show_selection_list(items, callback)` - Show selectable list
- [ ] `editor.show_input(prompt, default, callback)` - Show input dialog
- [ ] `editor.show_menu(items, callback)` - Show context menu
- [ ] Popup positioning control (cursor, center, bottom)
- [ ] Multi-column popup support
- [ ] Custom rendering in popups (colors, icons)
- [ ] Popup navigation keybindings

#### Virtual Buffers
- [ ] `editor.create_virtual_buffer(name, content)` - Create non-file buffer
- [ ] `editor.set_buffer_content(buffer_id, content)` - Replace buffer content
- [ ] `editor.set_buffer_read_only(buffer_id, read_only)` - Lock buffer editing
- [ ] `editor.set_buffer_syntax(buffer_id, syntax)` - Set syntax highlighting
- [ ] Custom buffer names (e.g., "*magit-log*", "*undo-tree*")

**Example: Undo History Visualization**
```lua
editor.create_virtual_buffer("*undo-tree*", render_undo_tree())
editor.set_buffer_read_only(buffer_id, true)
```

### Phase 5: Mode System & Custom Keybindings

**Priority: MEDIUM-HIGH** - Needed for mode-specific behavior

#### Plugin Modes
- [ ] `editor.define_mode(mode_name, options)` - Define custom mode
- [ ] `editor.set_mode(buffer_id, mode_name)` - Activate mode for buffer
- [ ] `editor.get_mode(buffer_id)` - Get current mode
- [ ] Mode-specific keybindings
- [ ] Mode activation/deactivation hooks
- [ ] Mode inheritance (extend existing modes)

#### Keybinding Registration
- [ ] `editor.bind_key(mode, key, callback)` - Register keybinding
- [ ] `editor.unbind_key(mode, key)` - Remove keybinding
- [ ] Keybinding priority/shadowing
- [ ] Multi-key sequences (e.g., "C-x C-s")
- [ ] Conditional keybindings (context-aware)

**Example: Magit Rebase Mode**
```lua
editor.define_mode("magit-rebase", {
    keybindings = {
        ["p"] = function() set_action("pick") end,
        ["s"] = function() set_action("squash") end,
        ["r"] = function() set_action("reword") end,
        ["d"] = function() set_action("drop") end,
        ["C-c C-c"] = function() finish_rebase() end,
        ["C-c C-k"] = function() abort_rebase() end,
    },
    read_only = false,
    syntax = "git-rebase"
})
```

### Phase 6: Enhanced Hooks & Events

**Priority: MEDIUM** - Better plugin lifecycle and reactivity

#### Additional Hook Types
- [ ] `on_buffer_open` - When buffer is opened
- [ ] `on_buffer_close` - When buffer is closed
- [ ] `on_buffer_switch` - When active buffer changes
- [ ] `on_mode_change` - When buffer mode changes
- [ ] `on_selection_change` - When selection changes
- [ ] `on_cursor_move` - When cursor moves (throttled)
- [ ] `on_key_press` - Before key is processed
- [ ] `on_command_execute` - Before command runs

#### Hook Improvements
- [ ] Pass more context to hooks (buffer_id, event details)
- [ ] Hook priority/ordering
- [ ] Async hooks (non-blocking)
- [ ] Hook error handling (don't crash editor)

### Phase 7: State Management & Persistence

**Priority: MEDIUM** - For plugin state across sessions

#### Plugin State
- [ ] `editor.set_plugin_data(key, value)` - Store plugin-specific data
- [ ] `editor.get_plugin_data(key)` - Retrieve plugin data
- [ ] Persist plugin state to disk (JSON file in plugin dir)
- [ ] Session-specific vs. global state
- [ ] Buffer-local state (attach data to buffers)

#### Configuration
- [ ] Plugin configuration file support (`.lua` config files)
- [ ] `editor.get_config(key, default)` - Get config value
- [ ] Hot-reload configuration changes
- [ ] User vs. project-level config

### Phase 8: Advanced Editor Integration

**Priority: MEDIUM-LOW** - Polish and power features

#### LSP Integration
- [ ] `editor.lsp_call(method, params, callback)` - Call LSP methods from plugins
- [ ] Access LSP diagnostics
- [ ] Custom LSP providers (plugins can act as LSP servers)

#### Syntax & Highlighting
- [ ] `editor.add_syntax_definition(name, patterns)` - Custom syntax highlighting
- [ ] `editor.set_highlight_region(buffer_id, range, color)` - Temporary highlights
- [ ] Semantic highlighting API

#### Search & Replace
- [ ] `editor.search(pattern, callback)` - Search in buffer
- [ ] `editor.replace(pattern, replacement)` - Replace in buffer
- [ ] Regex support with capture groups
- [ ] Multi-buffer search

#### Undo/Redo Access
- [ ] `editor.get_undo_history(buffer_id)` - Get undo tree
- [ ] `editor.undo_to_state(buffer_id, state_id)` - Jump to specific state
- [ ] `editor.get_redo_history(buffer_id)` - Get redo history

### Phase 9: Performance & Optimization

**Priority: LOW-MEDIUM** - Important for large buffers/files

#### Incremental Operations
- [ ] Stream-based API for large buffers (don't copy entire buffer)
- [ ] `editor.get_line_range(start, end)` - Get range of lines efficiently
- [ ] Lazy evaluation for expensive operations
- [ ] Buffer change notifications (delta updates instead of full content)

#### Caching
- [ ] Plugin result caching API
- [ ] Cache invalidation on buffer changes
- [ ] Memoization utilities

### Phase 10: Error Handling & Debugging

**Priority: MEDIUM** - Better developer experience

#### Error Handling
- [ ] `pcall`/`xpcall` wrapper with editor integration
- [ ] Stack traces in debug log
- [ ] Error popup notifications
- [ ] Plugin crash recovery (sandbox plugins)

#### Debugging Tools
- [ ] `editor.inspect(value)` - Pretty-print Lua values to debug log
- [ ] Lua REPL in editor (eval Lua expressions)
- [ ] Plugin reload without restart
- [ ] Performance profiling API

### Phase 11: WASM Plugin Support (Future)

**Priority: LOW** - Alternative to Lua for performance-critical plugins

- [ ] WASM runtime integration (wasmtime or wasmer)
- [ ] WASM<->Rust FFI bindings
- [ ] Sandboxed WASM execution
- [ ] Shared API surface with Lua plugins

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

### Next Phase Priority Order

1. **Buffer Query API** (Phase 2) - Foundation for most interactive plugins
2. **Async Process API** (Phase 3) - Unblock git operations and external tools
3. **Popup/Selection API** (Phase 4) - Enable interactive UIs
4. **Mode System** (Phase 5) - Allow mode-specific behavior
5. **Enhanced Hooks** (Phase 6) - Better plugin lifecycle

---

## Future Ideas

- Macros (record/play)
- Git integration (status, blame, stage hunks) - *may be implemented as plugins*
- Remote file editing (SSH, SFTP)
- Collaborative editing
