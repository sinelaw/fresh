# TODO

## Completed Features ✅

**Core Functionality**: Event-driven architecture with unlimited undo/redo, multi-cursor editing, clipboard operations (copy/cut/paste), position history navigation (Alt+Left/Right), line wrapping, large file support (1GB+), instant startup.

**UI & Layout**: Split views (horizontal/vertical), scrollbar, tab bar, command palette (Ctrl+P), help system (Ctrl+H), file explorer with lazy loading and gitignore support, status bar, line numbers, themes (dark/light/high-contrast).

**LSP Integration**: Diagnostics (errors/warnings), code completion (Ctrl+Space), go-to-definition, rename refactoring (F2), multi-language support, process resource limits.

**File Operations**: Open/save/close, multiple buffers, file explorer (create/delete files/dirs, show/hide hidden, respect gitignore, auto-expand on focus, unsaved indicators), async I/O.

**Git Integration**: Git grep (Ctrl+Shift+G), git find file (Ctrl+Shift+P).

**Plugin System**: Lua 5.4 runtime, plugin manager, command registration, event hooks, async process spawning, buffer query API, overlay system. Example: TODO Highlighter plugin.

**Testing**: 400+ unit tests, 59 E2E tests, property-based tests, visual regression testing framework.

**Recent Fixes**: Scrollbar height when no scrolling needed, cursor rendering at buffer end, keybinding conflicts, file explorer scrolling and focus sync, viewport scrolling on Enter key, marker-based overlay system.

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
