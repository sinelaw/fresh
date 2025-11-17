# TODO

## Implementation Guidelines

Features are categorized as:
- **🦀 Core (Rust)** - Performance-critical, deep integration, fundamental editor operations
- **📦 Plugin (TypeScript)** - UI/UX features, domain-specific, built on existing APIs
- **🔧 Infrastructure (Rust)** - Enables plugins, provides APIs for plugin features

---

## Remaining Work

### Priority 1: Critical Editor Features

#### Search & Replace Enhancements
- [x] Case-sensitive/insensitive toggle ✅
- [x] Whole word matching ✅
- [ ] Persist search history across sessions - **🦀 Core** (file I/O, startup/shutdown)
- [ ] Multi-file search/replace (integrate with git grep) - **📦 Plugin** (uses existing search APIs)
- [ ] Progress bar for replace_all on huge files - **🦀 Core** (rendering pipeline)

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
- [x] Rectangular/block selection data structures and keybindings (Ctrl+Alt+arrows) ✅
- [ ] Rectangular/block selection rendering and operations - **🦀 Core** (rendering, multi-cursor ops)
- [ ] Expand selection to scope (by AST nodes) - **🦀 Core** (tree-sitter integration)

#### Macros
- [x] Record macro (Alt+Shift+0-9 to toggle recording, F5 to stop) ✅
- [x] Play macro (Ctrl+Alt+0-9 to play) ✅
- [ ] Macro persistence (save/load across sessions) - **🦀 Core** (file I/O, startup)

---

### Priority 2: LSP & Developer Tools

#### LSP Core Robustness (P0) - **🦀 Core**

- [ ] **Auto-Restart on Crash**
  - Detect server process death, track restart attempts
  - Exponential backoff (1s, 2s, 4s, 8s delays)
  - Give up after 5 restarts in 3 minutes
  - Notify user on crash with option to manually restart
  - **Effort:** 4-6 hours

#### LSP Architecture Improvements (P1) - **🦀 Core**

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

#### LSP Core UX Features (P1) - **🦀 Core**

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

#### LSP Developer Experience (P2) - **🦀 Core**

- [ ] **Middleware System**
  - Intercept requests/notifications for logging, metrics
  - **Effort:** 6-8 hours

- [ ] **Document Selectors**
  - Match by language, scheme, glob pattern
  - Don't send unnecessary files to language servers
  - **Effort:** 2-3 hours

#### Deferred (Lower Priority)

- Semantic Tokens - **🦀 Core** (highlighting integration)
- Inlay Hints - **🦀 Core** (rendering pipeline)
- Call/Type Hierarchy - **🦀 Core** (LSP protocol)
- Log Viewer Panel - **📦 Plugin** (UI display)

#### File Explorer Polish
- [ ] Input dialog system for custom names - **🦀 Core** (UI primitive)
- [ ] Rename with custom name - **🦀 Core** (uses dialog system)
- [ ] Copy/move operations (Ctrl+C/X/V in explorer) - **🦀 Core** (file operations)
- [ ] File watching for auto-refresh - **🦀 Core** (OS integration)
- [ ] Search/filter within explorer - **📦 Plugin** (filter UI)
- [ ] Sort options (name, date, size, type) - **📦 Plugin** (sort logic)

---

### Priority 3: Virtual Buffers & Advanced Plugin UIs

**Goal:** Enable plugins to create rich UIs (Magit-style git interface, grep results, undo tree visualization) following Emacs' special buffer philosophy.

**Current Status:**
- ✅ BufferMode system implemented (keybindings, inheritance, read-only flag)
- ✅ TextProperty system implemented (metadata embedding in text ranges)
- ✅ Plugin API commands defined (CreateVirtualBuffer, SetContent, etc.)
- ❌ BufferKind enum not yet implemented (file vs virtual distinction)
- ❌ TypeScript ops for virtual buffers not exposed

#### Remaining Infrastructure Tasks - **🔧 Infrastructure**

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

#### Example Plugin: Diagnostics Panel (TypeScript) ✅ IMPLEMENTED - **📦 Plugin**

Fully functional diagnostics panel with virtual buffer split view:
- [x] Show diagnostics with severity icons ([E], [W], [I], [H])
- [x] Horizontal split view with 70/30 ratio
- [x] Navigation keybindings (j/k/n/p, RET to jump, q to close)
- [x] Selection marker for current diagnostic
- [x] Summary footer with error/warning counts
- [ ] Filter by severity (errors only, warnings only, all)
- [ ] Auto-refresh on diagnostic updates
- [ ] Integrate with next-error navigation

#### Future Plugin Examples - **📦 Plugin**
- Magit-style git interface
- Telescope-style fuzzy finder
- Undo tree visualizer
- Test runner results
- Grep/search results browser

---

### Priority 4: Visual Enhancements & UX

#### Visual Improvements
- [ ] Indent guides - **🦀 Core** (rendering pipeline)
- [ ] Current line highlighting - **🦀 Core** (rendering)
- [ ] Whitespace visualization - **🦀 Core** (rendering)
- [ ] Color column (vertical ruler) - **🦀 Core** (rendering)
- [ ] Git gutter (show added/modified/deleted lines) - **📦 Plugin** (git diff parsing)
- [ ] Minimap (optional) - **🦀 Core** (separate rendering view)

#### Themes & Appearance
- [ ] More built-in themes (Solarized, Monokai, Dracula, Nord) - **📦 Plugin** (JSON configs)
- [ ] Theme customization UI - **📦 Plugin** (settings editor)
- [ ] Font configuration (size, family) - **🦀 Core** (terminal setup)
- [ ] Ligature support - **🦀 Core** (rendering)

#### Command Palette Improvements
- [ ] Fuzzy matching (currently substring) - **🦀 Core** (search algorithm)
- [ ] Command history - **🦀 Core** (persistence)
- [ ] Recently used commands at top - **🦀 Core** (sorting logic)

#### Snippets & Templates
- [ ] Snippet system with Tab expansion - **📦 Plugin** (can use existing insert APIs)
- [ ] Tabstops and placeholders - **📦 Plugin** (cursor management via API)
- [ ] Snippet variables ($1, $2, $TM_FILENAME, etc.) - **📦 Plugin** (variable expansion)
- [ ] Language-specific snippets - **📦 Plugin** (JSON configs)

#### User Experience
- [ ] Welcome screen & onboarding - **📦 Plugin** (virtual buffer UI)
- [ ] Configuration UI (settings editor) - **📦 Plugin** (virtual buffer UI)
- [ ] Better error messages - **🦀 Core** (error handling)
- [ ] Crash recovery (restore unsaved files) - **🦀 Core** (file I/O, startup)
- [ ] Session persistence (restore open files) - **🦀 Core** (file I/O, startup)

---

### Priority 5: Advanced Features

#### Git Integration
- [ ] Git status in file explorer - **📦 Plugin** (git commands)
- [ ] Git blame - **📦 Plugin** (git blame parsing, virtual buffer)
- [ ] Git diff view (side-by-side or unified) - **📦 Plugin** (virtual buffer UI)
- [ ] Stage/unstage hunks - **📦 Plugin** (git commands)
- [ ] Commit UI / Branch switching - **📦 Plugin** (git commands, prompts)
- [ ] Git log viewer - **📦 Plugin** (git log parsing, virtual buffer)
- [ ] Merge conflict resolution UI - **📦 Plugin** (virtual buffer, markers)
- [ ] Magit-style interface (via plugin) - **📦 Plugin** (virtual buffer UI)

#### Terminal & Debugger
- [ ] Embedded terminal (Ctrl+`) - **🦀 Core** (PTY integration, rendering)
- [ ] Multiple terminals / split terminal - **🦀 Core** (split view management)
- [ ] Debug adapter protocol (DAP) support - **🦀 Core** (protocol implementation)
- [ ] Breakpoints (toggle, conditional) - **🦀 Core** (margin markers, persistence)
- [ ] Debug toolbar / Variables view / Call stack - **📦 Plugin** (virtual buffer UIs)

#### Project Management
- [ ] Project/workspace concept - **🦀 Core** (multi-root workspace)
- [ ] Project-specific configuration - **🦀 Core** (config loading)
- [ ] Multiple workspace folders - **🦀 Core** (LSP multi-root)

---

### Priority 6: Unified Event System - **🔧 Infrastructure**

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

#### Performance & Optimization - **🦀 Core**
- [ ] Syntax highlighting cache
- [ ] Lazy plugin loading
- [ ] Memory usage profiling

#### Dialogs & Progress
- [ ] Confirmation dialogs - **🦀 Core** (UI primitive)
- [ ] Progress indicators - **🦀 Core** (rendering)
- [ ] Toast notifications - **🦀 Core** (transient UI)

#### Accessibility - **🦀 Core**
- [ ] Screen reader support
- [ ] Configurable UI scale
- [ ] Color-blind friendly themes - **📦 Plugin** (theme configs)

#### Advanced/Future Features
- [ ] Remote editing (SSH/SFTP) - **🦀 Core** (network I/O)
- [ ] Collaborative editing (CRDT-based) - **🦀 Core** (data structures)
- [ ] Plugin marketplace - **📦 Plugin** (package management UI)
- [ ] Diff editor (compare files) - **📦 Plugin** (virtual buffer, diff parsing)
- [ ] Markdown preview - **📦 Plugin** (markdown rendering, virtual buffer)
- [ ] Vi/Vim emulation mode - **📦 Plugin** (keybinding modes, state machine)

---

## Technical Debt & Refactoring

### Unified Line Cache Architecture (High Priority) - **🦀 Core**

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

### Line Wrapping Refactoring - **🦀 Core**
- [ ] Unify wrapping and no-wrapping code paths
- [ ] Move cursor position calculation into rendering traversal
- [ ] Fix style preservation during wrapping

### Code Organization - **🦀 Core**
- [ ] Create BufferView abstraction
- [ ] Extract multi-cursor operations
- [ ] Split large modules (editor.rs)

### Split View Improvements - **🦀 Core**

**Current Status**: Basic split view implemented with Emacs-style shared buffers. Each split has independent cursors and viewports, edits are synchronized.

**Remaining Work**:
- [ ] Splitting a split that already has minimal size (1-2 lines)
- [ ] Nested splits (3+ levels deep) maintain correct hierarchy
- [ ] Rapid split/close operations don't leak memory
- [ ] Resizing terminal window redistributes space proportionally

### Test Infrastructure
- [ ] TypeScript plugin testing infrastructure (unit tests, mocking, test helpers) - **🔧 Infrastructure**
- [ ] Fix async file loading in test harness - **🦀 Core**
- [ ] Fix BIG.txt generation timing for scrolling tests - **🦀 Core**
- [ ] Add more E2E tests for complex workflows - **🦀 Core**
- [ ] Performance regression tests - **🦀 Core**

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

1. **High Priority**: LSP advanced features (hover, code actions, find references) - **🦀 Core**
2. **High Priority**: Rectangular/block selection rendering - **🦀 Core**
3. **Medium Priority**: Complete virtual buffer infrastructure - **🔧 Infrastructure**
4. **Medium Priority**: Macro persistence (save/load) - **🦀 Core**
5. **Lower Priority**: Git integration plugins - **📦 Plugin**

### Recent Completions (This Session)
- ✅ Bracket auto-close and auto-pair deletion
- ✅ Jump to next/previous error (F8/Shift+F8)
- ✅ Macro recording and playback system (Alt+Shift+0-9 / Ctrl+Alt+0-9)
- ✅ Comprehensive E2E tests for smart editing features (25 tests)
- ✅ Block selection infrastructure (SelectionMode, Position2D, keybindings)
- ✅ Feature categorization (Core vs Plugin analysis)
