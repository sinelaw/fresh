# TODO

## Completed Features ✅

### Core Functionality
- Event-driven architecture with unlimited undo/redo
- Multi-cursor editing
- Clipboard operations (copy/cut/paste)
- Position history navigation (Alt+Left/Right)
- Line wrapping
- Large file support (1GB+) with instant startup
- Advanced prompt editing (word deletion, copy/paste/cut in all input prompts)
- **Auto-indent (Jan 2025)** - Tree-sitter based, hybrid heuristic approach, supports all languages

### UI & Layout
- Split views (horizontal/vertical)
- Scrollbar, tab bar, status bar, line numbers
- Command palette (Ctrl+P), help system (Ctrl+H)
- File explorer (lazy loading, gitignore support, create/delete, unsaved indicators)
- Themes (dark/light/high-contrast)

### LSP Integration
- Diagnostics (errors/warnings)
- Code completion (Ctrl+Space)
- Go-to-definition, rename refactoring (F2)
- Multi-language support, process resource limits

### Search & Replace
- ✅ **Streaming search** (Nov 2025) - Literal & regex, efficient on GB+ files with overlapping chunks
- ✅ **Replace operations** (Nov 2025) - replace_range(), replace_next(), replace_all(), replace_all_regex() with capture groups
- ✅ **Replace UI** (Ctrl+R) - Emacs-style two-step prompts with incremental highlighting
- ✅ **Interactive replace** (Ctrl+Alt+R) - Query-replace with y/n/!/q prompts, proper undo/redo
- ✅ **Search in selection** (Nov 2025) - Limit search to selected range
- ✅ **Search history** (Nov 2025) - Up/Down navigation, bash-like, 100 items per history
- Basic text search UI (F3/Shift+F3), wrap-around, highlighting, incremental search

### File Operations
- Open/save/close, multiple buffers, async I/O
- File explorer (create/delete files/dirs, show/hide hidden, respect gitignore, auto-expand on focus)

### Git Integration
- Git grep (Ctrl+Shift+G)
- Git find file (Ctrl+Shift+P)

### Plugin System
- ✅ **Lua 5.4 runtime** - Fully integrated plugin manager, lifecycle management
- ✅ **Dynamic hooks** - 16+ hook types (render-line, after-save, etc.)
- ✅ **Command registration** - Plugins can register custom commands
- ✅ **Async process spawning** - Non-blocking external commands
- ✅ **Buffer query API** - Metadata queries, streaming content access via render-line hook
- ✅ **Overlay lifecycle** - clear_all_overlays(), remove_overlays_by_prefix()
- ✅ **Example plugins** - TODO Highlighter (optimized for GB+ files), async demos

### Performance & Optimization
- ✅ **Marker system (IntervalTree)** - O(log n) marker operations, lazy delta propagation for position tracking
- ✅ **ChunkTree optimization** (Jan 2025) - 4KB chunks → 38x speedup (file loading: 3.2s → 83ms)
- ✅ **Scroll optimization** - O(n) → O(viewport_height)
- ✅ **Buffer cache removal** - Eliminated expensive `buffer.to_string()` calls
- 400+ unit tests, 59 E2E tests, property-based tests, visual regression testing

---

## Remaining Work

### Priority 1: Critical Editor Features

#### Search & Replace Enhancements
- [ ] Case-sensitive/insensitive toggle
- [ ] Whole word matching
- [ ] Persist search history across sessions
- [ ] Multi-file search/replace (integrate with git grep)
- [ ] Progress bar for replace_all on huge files

#### Smart Editing
- [ ] Smart home key (toggle between line start and first non-whitespace)
- [ ] Bracket matching & auto-close
- [ ] Auto-pair deletion (delete both opening and closing)
- [ ] Electric indent (auto-adjust indentation)
- [ ] Toggle comment (Ctrl+/, language-aware)
- [ ] Indent/dedent selection (Tab/Shift+Tab)

#### Navigation
- [ ] Go to line number (Ctrl+G)
- [ ] Go to matching bracket
- [ ] Jump to next/previous error (F8/Shift+F8)
- [ ] Bookmark system (set/clear/jump)

#### Selection & Editing
- [ ] Rectangular/block selection (Alt+drag or Ctrl+Alt+arrows)
- [ ] Expand selection to scope (by AST nodes)

#### Macros
- [ ] Record macro (q + key)
- [ ] Play macro (@ + key)
- [ ] Macro persistence

### Priority 2: LSP & Developer Tools

### LSP Support (Maturity Roadmap)

This plan aims to evolve the LSP client to be performant, full-featured, and robust, based on the principles for building a mature LSP client.

#### Priority 1: Performance & Stability Foundation

- [ ] **Implement Incremental Synchronization (Deltas):**
    - **Problem:** The editor currently sends the entire file content on every keystroke, causing significant UI lag in large files.
    - **Solution:** Modify the `didChange` notification to send only the changed text (deltas). This requires checking for the `TextDocumentSyncKind::Incremental` capability from the server and calculating the text diffs to send. This is the highest priority performance fix.
- [ ] **Implement Request Cancellation:**
    - **Problem:** Slow or outdated results (e.g., from code completion) can appear after the user has already moved on, creating UI "jank."
    - **Solution:** Implement support for sending `$/cancelRequest` notifications when a new request is issued before an old one completes (e.g., typing more characters while a completion menu is visible).
- [ ] **Robust Server Lifecycle Management:**
    - **Problem:** A crashed or hung LSP server can leave the editor in a broken state with no feedback.
    - **Solution:** Implement robust error handling to detect when the LSP process dies. Notify the user and offer to restart the server.
- [ ] **Harden JSON-RPC Message Parsing:**
    - **Problem:** A malformed or partial message from the LSP server could crash the editor's message handling loop.
    - **Solution:** Improve the robustness of the JSON-RPC transport layer to gracefully handle framing errors, corrupt headers, or invalid JSON, preventing panics.

#### Priority 2: Core UX Features

- [ ] **Dedicated Diagnostics Panel:**
    - **Problem:** Diagnostics are only visible as squiggles in the text. There is no way to see a full list of problems in the current file or project.
    - **Solution:** Create a new UI panel that lists all diagnostics from `textDocument/publishDiagnostics`, allowing users to quickly navigate to each error location.
- [ ] **Hover Documentation:** Show documentation for the symbol under the cursor in a popup window on `textDocument/hover`.
- [ ] **Code Actions:** Query for `textDocument/codeAction` and allow the user to apply quick fixes and refactorings (e.g., via a menu).
- [ ] **Find References:** Implement `textDocument/references` and display the results in a list or quickfix window.
- [ ] **Signature Help:** Show function/method parameter hints as the user is typing, triggered by `textDocument/signatureHelp`.

#### Priority 3: Advanced Features & Polish

- [ ] **Semantic Tokens:** Implement `textDocument/semanticTokens` for more advanced and accurate syntax highlighting.
- [ ] **Document & Workspace Symbols:** Implement `textDocument/documentSymbol` for an outline/breadcrumb view and `workspace/symbol` for project-wide symbol search.
- [ ] **Inlay Hints:** Display inlay hints (`textDocument/inlayHint`) for type annotations and parameter names.
- [ ] **Progress Reporting:** Handle `$/progress` notifications from the server to show activity indicators in the UI (e.g., for indexing).
- [ ] **Server Communication & Logging:**
    - [ ] Handle `window/logMessage` to display server logs for debugging.
    - [ ] Handle `window/showMessage` and `window/showMessageRequest` to show info/warnings and ask questions.
- [ ] **Document Formatting:** Add commands for `textDocument/formatting` and `textDocument/rangeFormatting`.
- [ ] **Call Hierarchy / Type Hierarchy:** Implement `callHierarchy/incomingCalls` and `typeHierarchy/supertypes`.
- [ ] **Code Lens / Folding Ranges:** Implement `textDocument/codeLens` and `textDocument/foldingRange`.

#### Priority 4: Project & Configuration

- [ ] **Multi-Root Workspace Support:** Support `workspace/workspaceFolders` to correctly handle projects with multiple sub-projects, potentially launching separate LSP instances per folder.
- [ ] **Configuration Synchronization:** Send `workspace/didChangeConfiguration` notifications when editor settings (like tab size or diagnostics settings) change.

#### File Explorer Polish
- [ ] Input dialog system for custom names
- [ ] Rename with custom name
- [ ] Copy/move operations (Ctrl+C/X/V in explorer)
- [ ] File watching for auto-refresh
- [ ] Search/filter within explorer
- [ ] Sort options (name, date, size, type)

### Priority 3: Visual Enhancements & UX

#### Visual Improvements
- [ ] Indent guides
- [ ] Current line highlighting
- [ ] Whitespace visualization
- [ ] Color column (vertical ruler)
- [ ] Git gutter (show added/modified/deleted lines)
- [ ] Minimap (optional)

#### Themes & Appearance
- [ ] More built-in themes (Solarized, Monokai, Dracula, Nord)
- [ ] Theme customization UI
- [ ] Font configuration (size, family)
- [ ] Ligature support

#### Command Palette Improvements
- [ ] Fuzzy matching (currently substring)
- [ ] Command history
- [ ] Recently used commands at top
- [ ] Show keyboard shortcuts in palette

#### Snippets & Templates
- [ ] Snippet system with Tab expansion
- [ ] Tabstops and placeholders
- [ ] Snippet variables ($1, $2, $TM_FILENAME, etc.)
- [ ] Language-specific snippets

#### User Experience
- [ ] Welcome screen & onboarding
- [ ] Configuration UI (settings editor)
- [ ] Better error messages
- [ ] Crash recovery (restore unsaved files)
- [ ] Session persistence (restore open files)

### Priority 4: Advanced Features

#### Git Integration
- [ ] Git status in file explorer
- [ ] Git blame
- [ ] Git diff view (side-by-side or unified)
- [ ] Stage/unstage hunks
- [ ] Commit UI / Branch switching
- [ ] Git log viewer
- [ ] Merge conflict resolution UI
- [ ] Magit-style interface (via plugin)

#### Terminal & Debugger
- [ ] Embedded terminal (Ctrl+`)
- [ ] Multiple terminals / split terminal
- [ ] Debug adapter protocol (DAP) support
- [ ] Breakpoints (toggle, conditional)
- [ ] Debug toolbar / Variables view / Call stack

#### Project Management
- [ ] Project/workspace concept
- [ ] Project-specific configuration
- [ ] Multiple workspace folders

### Priority 5: Plugin System (Advanced APIs)

#### Interactive UI API
- [ ] Virtual buffers / selection lists / input dialogs
- [ ] Read-only buffers
- [ ] Generic popups

#### Modal Interaction & Navigation
- [ ] Define custom modes
- [ ] Dynamic keybindings
- [ ] Goto line/position, set selection, scroll control

#### Enhanced Hooks & Integration
- [ ] More hooks: `on_buffer_open`, `on_selection_change`, `on_key_press`
- [ ] State persistence API
- [ ] LSP access / Search API / Undo history API
- [ ] Process cancellation support

#### Target Showcase Plugins
- [ ] Magit-style Git interface
- [ ] Telescope-style fuzzy finder
- [ ] Undo tree visualizer
- [ ] Project search & replace

### Priority 6: Future Enhancements

#### Performance & Optimization
- [ ] Syntax highlighting cache
- [ ] Lazy plugin loading
- [ ] Memory usage profiling

#### Dialogs & Progress
- [ ] Confirmation dialogs
- [ ] Progress indicators
- [ ] Toast notifications

#### Accessibility
- [ ] Screen reader support
- [ ] Configurable UI scale
- [ ] Color-blind friendly themes

#### Advanced/Future Features
- [ ] Remote editing (SSH/SFTP)
- [ ] Collaborative editing (CRDT-based)
- [ ] Plugin marketplace
- [ ] Diff editor (compare files)
- [ ] Markdown preview
- [ ] Vi/Vim emulation mode

---

## Technical Debt & Refactoring

### Line Wrapping Refactoring
- [ ] Unify wrapping and no-wrapping code paths (treat no-wrap as infinite-width)
- [ ] Move cursor position calculation into rendering traversal (eliminate duplicate iteration)
- [ ] Fix style preservation during wrapping (currently loses syntax highlighting)

**Benefits**: Single source of truth, better performance, massive code deduplication

### Code Organization
- [x] Extract UI rendering (~430 lines → 6 modules)
- [x] Extract commands & prompts (~335 lines → 2 modules)
- [ ] Create BufferView abstraction (~500 lines)
- [ ] Extract multi-cursor operations (~200 lines)
- [ ] Split large modules (editor.rs is ~3000 lines)

### Test Infrastructure
- [ ] Fix async file loading in test harness (6 tests ignored)
- [ ] Fix BIG.txt generation timing (2 scrolling tests fail)
- [ ] Support independent buffers per split (if desired)
- [ ] Add more E2E tests for complex workflows
- [ ] Performance regression tests

---

## Summary

### Current Status (January 2025)
**Strengths**: Multi-cursor editing, search & replace, auto-indent, LSP basics, large file support (1GB+), fully integrated Lua plugin system, IntervalTree marker system, strong test coverage (400+ tests)

**Recent Major Completions**:
- ✅ Search & Replace (Nov 2025) - Complete with interactive replace, history, search in selection
- ✅ Auto-indent (Jan 2025) - Tree-sitter based with hybrid heuristics
- ✅ Plugin System (Nov 2025) - Fully integrated with Lua runtime, hooks, and overlay management
- ✅ Marker System (Nov 2025) - O(log n) IntervalTree implementation with lazy delta propagation

**Critical Gaps**: Advanced LSP features (hover, code actions, find references), bracket matching, snippets, terminal integration

**Next Steps**: Focus on LSP advanced features and smart editing (bracket matching, toggle comment)

### Milestones

**M1: Essential Editing** ✅ **COMPLETE**
- [x] Core editing, multi-cursor, undo/redo
- [x] Search & replace
- [x] Auto-indent, go to line

**M2: Developer Experience** (Target: Daily Driver)
- [x] LSP basics (diagnostics, completion, go-to-def, rename)
- [ ] LSP advanced (hover, code actions, find references) ← **CURRENT FOCUS**
- [ ] Snippets, toggle comment, bracket matching

**M3: Advanced Features** (Target: Best-in-Class)
- [x] Large file support, plugin system (Lua)
- [x] Performance optimization (marker system, ChunkTree)
- [ ] Advanced plugin APIs (custom modes, virtual buffers)
- [ ] Magit/Telescope-style plugins
- [ ] Terminal & debugger integration

**M4: Polish & Ecosystem** (Target: Production-Ready)
- [ ] Welcome screen, configuration UI
- [ ] Crash recovery, session persistence
- [ ] Plugin marketplace, comprehensive docs
