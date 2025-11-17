# TODO

## Remaining Work

### Priority 1: Critical Editor Features

#### Search & Replace Enhancements
- [x] Case-sensitive/insensitive toggle ✅
- [x] Whole word matching ✅
- [ ] Persist search history across sessions
- [ ] Multi-file search/replace (integrate with git grep)
- [ ] Progress bar for replace_all on huge files

#### Smart Editing
- [x] Smart home key (toggle between line start and first non-whitespace) ✅
- [x] Bracket matching & auto-close (auto-inserts closing bracket/quote) ✅
- [x] Auto-pair deletion (delete both opening and closing when between pairs) ✅
- [x] Electric indent (auto-adjust indentation for closing delimiters) ✅
- [x] Toggle comment (Ctrl+/, language-aware) ✅
- [x] Indent/dedent selection (Tab/Shift+Tab) ✅

#### Navigation
- [x] Go to line number (Ctrl+G) ✅
- [x] Go to matching bracket (Ctrl+]) ✅
- [x] Jump to next/previous error (F8/Shift+F8) ✅
- [x] Bookmark system (Ctrl+Shift+0-9 to set, Alt+0-9 to jump) ✅

#### Selection & Editing
- [ ] Rectangular/block selection (Alt+drag or Ctrl+Alt+arrows)
- [ ] Expand selection to scope (by AST nodes)

#### Macros
- [x] Record macro (Alt+Shift+0-9 to toggle recording, F5 to stop) ✅
- [x] Play macro (Ctrl+Alt+0-9 to play) ✅
- [ ] Macro persistence (save/load across sessions)

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

#### Example Plugin: Diagnostics Panel (TypeScript) ✅ IMPLEMENTED

Fully functional diagnostics panel with virtual buffer split view:
- [x] Show diagnostics with severity icons ([E], [W], [I], [H])
- [x] Horizontal split view with 70/30 ratio
- [x] Navigation keybindings (j/k/n/p, RET to jump, q to close)
- [x] Selection marker for current diagnostic
- [x] Summary footer with error/warning counts
- [ ] Filter by severity (errors only, warnings only, all)
- [ ] Auto-refresh on diagnostic updates
- [ ] Integrate with next-error navigation

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

**Problem**: Line number ↔ byte offset conversions are a major performance bottleneck.

**Solution**: Unify line tracking into the existing IntervalTree marker system. Lines are intervals between newlines, reusing lazy delta propagation for O(log N) edits.

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
- [ ] Create BufferView abstraction
- [ ] Extract multi-cursor operations
- [ ] Split large modules (editor.rs)

### Split View Improvements

**Current Status**: Basic split view implemented with Emacs-style shared buffers. Each split has independent cursors and viewports, edits are synchronized.

**Remaining Work**:
- [ ] Splitting a split that already has minimal size (1-2 lines)
- [ ] Nested splits (3+ levels deep) maintain correct hierarchy
- [ ] Rapid split/close operations don't leak memory
- [ ] Resizing terminal window redistributes space proportionally

### Test Infrastructure
- [ ] TypeScript plugin testing infrastructure (unit tests, mocking, test helpers)
- [ ] Fix async file loading in test harness
- [ ] Fix BIG.txt generation timing for scrolling tests
- [ ] Add more E2E tests for complex workflows
- [ ] Performance regression tests

---

## Completed Work (Summary)

### TypeScript Plugin System Migration
Full migration from Lua to TypeScript as the sole plugin runtime:
- Embedded Deno Core (V8 engine) with native ops
- Async/await support via native Promises
- TypeScriptPluginManager with load/unload/reload/hot-reload
- Event/hook system (editor.on/off with emit infrastructure)
- Production plugins (git-grep, git-find-file, todo-highlighter, etc.)
- Complete removal of mlua and all Lua code

### Plugin Thread Architecture (CRITICAL - Required for Async Ops)

**Problem**: Current `run_hook_blocking()` creates a new tokio runtime for each hook call. When plugins call async ops (like `spawnProcess`), the runtime is destroyed after the hook returns, orphaning pending async work and causing hangs.

**Solution**: Dedicated thread for TypeScript plugin execution with message passing.

**Architecture**:
```
Main Thread (UI)                    Plugin Thread
     │                                    │
     ├─────── HookRequest ──────────────>│
     │        (hook_name, args)           │
     │                                    ├── JsRuntime lives here (not Send/Sync)
     │                                    ├── Persistent tokio runtime
     │                                    ├── Executes JS/TS code
     │                                    ├── Drives async ops to completion
     │                                    │
     │<─────── PluginCommand ────────────┤
     │         (setPromptSuggestions,     │
     │          setStatus, etc.)          │
     │                                    │
```

**Implementation Plan**:
- [ ] Phase 1: Create PluginThread struct
  - Owns JsRuntime and tokio Runtime
  - Spawned on dedicated thread at editor startup
  - Event loop: receive requests, execute, send results

- [ ] Phase 2: Define message types
  - `HookRequest { hook_name, args, response_channel }`
  - `ActionRequest { action_name, response_channel }`
  - `LoadPluginRequest { path }`
  - `ShutdownRequest`

- [ ] Phase 3: Refactor TypeScriptPluginManager
  - Remove `run_hook_blocking()` and other `*_blocking()` methods
  - Replace with async message sending to plugin thread
  - `run_hook()` becomes fire-and-forget (results via PluginCommand channel)

- [ ] Phase 4: Update Editor integration
  - Editor calls `ts_manager.send_hook(name, args)` (non-blocking)
  - Plugin thread executes hook asynchronously
  - Results come back via existing `PluginCommand` channel
  - `process_async_messages()` already handles these

- [ ] Phase 5: Handle plugin loading
  - `load_plugin()` sends request to plugin thread
  - Blocks on response (only during startup)
  - Or: make plugin loading fully async

**Benefits**:
- Complete isolation: Plugins can't block UI thread
- Stable async runtime: Single tokio runtime for all async ops
- Natural async model: Plugins use async/await without restrictions
- No race conditions: Sequential execution within plugin thread
- Matches industry standard: Similar to VSCode's Extension Host

**Files to modify**:
- `src/ts_runtime.rs`: Extract runtime into PluginThread, add message passing
- `src/editor.rs`: Update hook calls to be non-blocking
- `src/plugin_api.rs`: May need adjustments for thread safety

### Menu Bar System
Full keyboard/mouse navigation with F10 toggle, arrow key navigation, Alt+letter mnemonics, keybinding display in dropdowns, JSON configuration.

### Core LSP Features
- Client state machine with validated transitions
- Request cancellation with $/cancelRequest notifications
- Deferred document open (queue commands until init completes)
- Diagnostics, completion, go-to-definition, rename refactoring
- Progress notifications, window messages, UTF-16 position encoding
- CPU optimization (eliminated busy-wait loop)

### Search & Replace
Streaming search on GB+ files, regex support, interactive query-replace, search in selection, search history.

### Plugin Infrastructure
BufferMode system (keybindings with inheritance), TextProperty system (metadata embedding), hook-based prompt API, command registration, async process spawning.

### Performance Optimizations
ChunkTree, IntervalTree marker system (O(log n)), viewport-based rendering, eliminated expensive buffer.to_string() calls.

### UI & Layout
Split views (horizontal/vertical with shared buffers), file explorer (lazy loading, gitignore, create/delete), command palette, themes, scrollbars, tab bar.

### Core Editing
Multi-cursor editing, unlimited undo/redo, position history navigation, auto-indent (tree-sitter based), large file support (1GB+).

---

## Next Steps

1. **High Priority**: LSP advanced features (hover, code actions, find references)
2. **High Priority**: Macros (record, play, persistence)
3. **Medium Priority**: Complete virtual buffer infrastructure (BufferKind, TypeScript ops)
4. **Medium Priority**: Rectangular/block selection
5. **Lower Priority**: Search enhancements (persist history, multi-file search)
