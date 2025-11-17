# TODO

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

---

### Priority 2: LSP & Developer Tools

#### LSP Core Robustness (P0)

- [ ] **Auto-Restart on Crash**
  - Detect server process death, track restart attempts
  - Exponential backoff (1s, 2s, 4s, 8s delays)
  - Give up after 5 restarts in 3 minutes
  - Notify user on crash with option to manually restart
  - **Effort:** 4-6 hours

#### LSP Architecture Improvements (P1)

- [ ] **Feature Registration System**
  - Abstract features: `trait LspFeature { initialize(), clear() }`
  - Modular completion, hover, diagnostics (separate files)
  - Enables dynamic capability registration (LSP 3.16+)
  - **Effort:** 8-12 hours

- [ ] **Pull Diagnostics** (LSP 3.17+)
  - Implement `textDocument/diagnostic`
  - Track `resultId` for incremental updates
  - **Effort:** 8-12 hours

- [ ] **Multi-Root Workspaces**
  - Support `Vec<WorkspaceFolder>` instead of single `root_uri`
  - Send `workspace/didChangeWorkspaceFolders` on add/remove
  - **Effort:** 4-6 hours

#### LSP Core UX Features (P1)

- [ ] **Hover Documentation**
  - Request `textDocument/hover` on Ctrl+K or hover
  - Show documentation popup with markdown rendering
  - Cache results, cancel on cursor move
  - **Effort:** 4-6 hours

- [ ] **Code Actions**
  - Query `textDocument/codeAction` for quick fixes
  - Show menu/popup with available actions
  - Apply `WorkspaceEdit` changes
  - **Effort:** 6-8 hours

- [ ] **Find References**
  - Request `textDocument/references`
  - Display results in quickfix/location list
  - Jump to reference on selection
  - **Effort:** 4-6 hours

- [ ] **Signature Help**
  - Request `textDocument/signatureHelp` on `(` and `,`
  - Show parameter hints in popup
  - Highlight active parameter
  - **Effort:** 4-6 hours

#### LSP Developer Experience (P2)

- [ ] **Middleware System**
  - Intercept requests/notifications for logging, metrics
  - **Effort:** 6-8 hours

- [ ] **Document Selectors**
  - Match by language, scheme, glob pattern
  - Don't send unnecessary files to language servers
  - **Effort:** 2-3 hours

#### Deferred (Lower Priority)

- Semantic Tokens - Advanced highlighting
- Inlay Hints - Type annotations
- Call/Type Hierarchy - Advanced navigation
- Log Viewer Panel - UI polish

#### File Explorer Polish
- [ ] Input dialog system for custom names
- [ ] Rename with custom name
- [ ] Copy/move operations (Ctrl+C/X/V in explorer)
- [ ] File watching for auto-refresh
- [ ] Search/filter within explorer
- [ ] Sort options (name, date, size, type)

---

### Priority 3: Virtual Buffers & Advanced Plugin UIs

**Goal:** Enable plugins to create rich UIs (Magit-style git interface, grep results, undo tree visualization) following Emacs' special buffer philosophy.

**Current Status:**
- ✅ BufferMode system implemented (keybindings, inheritance, read-only flag)
- ✅ TextProperty system implemented (metadata embedding in text ranges)
- ✅ Plugin API commands defined (CreateVirtualBuffer, SetContent, etc.)
- ❌ BufferKind enum not yet implemented (file vs virtual distinction)
- ❌ TypeScript ops for virtual buffers not exposed

#### Remaining Infrastructure Tasks

- [ ] **BufferKind Enum**
  - Add `BufferKind::File { path }` vs `BufferKind::Virtual { name, mode }`
  - Virtual buffers skip file I/O, dirty-checking, LSP notifications
  - `*Name*` convention signals special buffer (Emacs style)

- [ ] **TypeScript Virtual Buffer Ops**
  - `op_fresh_create_virtual_buffer(name, mode, read_only)`
  - `op_fresh_create_virtual_buffer_in_split(name, mode, entries, ratio, ...)`
  - `op_fresh_set_virtual_buffer_content(buffer_id, entries)`
  - `op_fresh_get_text_properties_at_cursor(buffer_id)`
  - `op_fresh_define_mode(name, config)`

- [ ] **Next-Error Navigation Pattern**
  - Global `next-error` / `previous-error` commands (M-g n / M-g p)
  - Plugin API: `editor.set_next_error_source(buf_id, callback)`
  - Navigation works from any buffer (jumps to source location)

- [ ] **Revert Buffer Mechanism**
  - `revert-buffer` command (g in special mode) calls buffer's revert function
  - Plugin API: `editor.set_revert_function(buf_id, callback)`

#### Example Plugin: Diagnostics Panel (TypeScript)

Once infrastructure is complete:
- Show diagnostics from current file with severity icons
- Filter by severity (errors only, warnings only, all)
- Jump to location on RET
- Auto-refresh on diagnostic updates
- Integrate with next-error navigation

#### Future Plugin Examples
- Magit-style git interface
- Telescope-style fuzzy finder
- Undo tree visualizer
- Test runner results
- Grep/search results browser

---

### Priority 4: Visual Enhancements & UX

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

---

### Priority 5: Advanced Features

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

---

### Priority 6: Unified Event System

**Goal**: Create a coherent event architecture unifying hooks, control events, and script control mode.

#### Phase 1: Editor Emits Control Events
- [ ] Add `EventBroadcaster` to Editor
- [ ] Emit FileOpened, FileSaved, FileClosed events
- [ ] Emit LspStatusChanged, DiagnosticsUpdated events
- [ ] Emit PopupShown/Hidden, CompletionReceived events

#### Phase 2: Plugin Event API
- [ ] `editor.emit_event(event_type, data)` - Plugins emit custom events
- [ ] `editor.on_event(pattern, callback)` - Subscribe to events
- [ ] `editor.wait_for_event(pattern, timeout)` - Async wait for events
- [ ] Event namespacing to avoid collisions

#### Phase 3: Unify Hooks and Control Events
- [ ] Hooks automatically emit corresponding ControlEvents
- [ ] Single source of truth: hooks define what happens, events broadcast

#### Phase 4: Script Mode Integration
- [ ] Script mode subscribes to EventBroadcaster
- [ ] Event-based waiting instead of polling

---

### Priority 7: Future Enhancements

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

### Unified Line Cache Architecture (High Priority)

**Problem**: Line number ↔ byte offset conversions are a major performance bottleneck (61.95% of diagnostic processing time).

**Solution**: Unify line tracking into the existing IntervalTree marker system. Lines are intervals between newlines, reusing lazy delta propagation for O(log N) edits.

**Expected Performance Gain**:
- LSP diagnostic processing: 61.95% reduction
- Edit performance: 10-100x faster for files with large caches

**Implementation Plan**:
- [ ] Phase 1: Extend IntervalTree with `MarkerType` enum and line marker methods
- [ ] Phase 2: Add `line_to_byte` / `byte_to_line` to unified tree
- [ ] Phase 3: Migrate `lsp_position_to_byte` to use new system
- [ ] Phase 4: Remove `LineCache` struct and eager update logic
- [ ] Phase 5: Add lazy line marker rescanning for edits with newlines
- [ ] Phase 6: Implement viewport-based line population strategy
- [ ] Phase 7: Benchmark with large files (1GB+) and many diagnostics (10k+)

### Line Wrapping Refactoring
- [ ] Unify wrapping and no-wrapping code paths
- [ ] Move cursor position calculation into rendering traversal
- [ ] Fix style preservation during wrapping

### Code Organization
- [ ] Create BufferView abstraction (~500 lines)
- [ ] Extract multi-cursor operations (~200 lines)
- [ ] Split large modules (editor.rs is ~3000 lines)

### Split View Improvements

**Current Status**: Basic split view implemented with Emacs-style shared buffers. Each split has independent cursors and viewports, edits are synchronized.

**Remaining Work**:
- [ ] Splitting a split that already has minimal size (1-2 lines)
- [ ] Nested splits (3+ levels deep) maintain correct hierarchy
- [ ] Rapid split/close operations don't leak memory
- [ ] Resizing terminal window redistributes space proportionally

### Test Infrastructure
- [ ] TypeScript plugin testing infrastructure (unit tests, mocking, test helpers)
- [ ] Fix async file loading in test harness (6 tests ignored)
- [ ] Fix BIG.txt generation timing (2 scrolling tests fail)
- [ ] Add more E2E tests for complex workflows
- [ ] Performance regression tests

---

## Completed Work (Summary)

### TypeScript Plugin System Migration (Nov 2024)
Full migration from Lua to TypeScript as the sole plugin runtime:
- Embedded Deno Core (V8 engine) with 44 native ops
- Async/await support via native Promises
- TypeScriptPluginManager with load/unload/reload/hot-reload
- Event/hook system (editor.on/off with emit infrastructure)
- 9 production TypeScript plugins (git-grep, git-find-file, todo-highlighter, etc.)
- Complete removal of mlua and all Lua code

### Menu Bar System
Full keyboard/mouse navigation with F10 toggle, arrow key navigation, Alt+letter mnemonics, keybinding display in dropdowns, JSON configuration.

### Core LSP Features
- Client state machine (Initial→Starting→Initializing→Running→Stopping→Stopped→Error)
- Request cancellation with $/cancelRequest notifications
- Deferred document open (queue commands until init completes)
- Diagnostics, completion, go-to-definition, rename refactoring
- Progress notifications, window messages, UTF-16 position encoding
- CPU optimization (eliminated 46% busy-wait loop)

### Search & Replace
Streaming search on GB+ files, regex support, interactive query-replace, search in selection, search history.

### Plugin Infrastructure
BufferMode system (keybindings with inheritance), TextProperty system (metadata embedding), hook-based prompt API, command registration, async process spawning.

### Performance Optimizations
ChunkTree (38x speedup), IntervalTree marker system (O(log n)), viewport-based rendering (O(viewport_height)), eliminated expensive buffer.to_string() calls.

### UI & Layout
Split views (horizontal/vertical with shared buffers), file explorer (lazy loading, gitignore, create/delete), command palette, themes, scrollbars, tab bar.

### Core Editing
Multi-cursor editing, unlimited undo/redo, position history navigation, auto-indent (tree-sitter based), large file support (1GB+).

---

## Next Steps

1. **High Priority**: LSP advanced features (hover, code actions, find references)
2. **Medium Priority**: Smart editing (bracket matching, toggle comment)
3. **Medium Priority**: Complete virtual buffer TypeScript ops for advanced plugin UIs
