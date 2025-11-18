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
- [x] Persist search history across sessions (~/.local/share/fresh/) ✅
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
- [x] Rectangular/block selection data structures and keybindings (Alt+Shift+arrows) ✅
- [x] Rectangular/block selection rendering (visual rectangle highlighting) ✅
- [ ] Block selection operations (insert, delete, copy on rectangle) - **🦀 Core** (multi-cursor ops)
- [ ] Expand selection to scope (by AST nodes) - **🦀 Core** (tree-sitter integration)

#### Macros
- [x] Record macro (Alt+Shift+0-9 to toggle recording, F5 to stop) ✅
- [x] Play macro (Ctrl+Alt+0-9 to play) ✅
- [ ] Macro persistence (save/load across sessions) - **🦀 Core** (file I/O, startup)

---

### Priority 2: LSP & Developer Tools

#### LSP Core Robustness (P0) - **🦀 Core**

- [x] **Auto-Restart on Crash** ✅
  - Detect server process death, exponential backoff (1s, 2s, 4s, 8s)
  - Give up after 5 restarts in 3 minutes, manual restart command available

#### LSP Next Priority Features (P1) - **🦀 Core**

- [x] **Pull Diagnostics** (LSP 3.17+) ✅
  - Implement `textDocument/diagnostic` request
  - Track `resultId` for incremental updates
  - Auto-trigger after didOpen/didChange

- [ ] **Inlay Hints** (LSP 3.17+)
  - Implement `textDocument/inlayHint`
  - Render inline type/parameter hints (e.g., `let x/*: i32*/ = 5;`)
  - High visibility feature expected by modern editor users
  - **Effort:** 12-16 hours (includes virtual text infrastructure)

  **Missing Infrastructure: Inline Virtual Text Rendering**

  The current Overlay system (`src/overlay.rs`) handles visual decorations (underlines, backgrounds, foreground colors) but cannot inject virtual text. The rendering loop in `src/ui/split_rendering.rs` assumes 1:1 character-to-span mapping - each buffer character maps to exactly one Ratatui Span.

  **Recommended Architecture: Extend Overlay System**

  Add a `VirtualText` variant to `OverlayFace`:

  ```rust
  pub enum OverlayFace {
      Underline { color: Color, style: UnderlineStyle },
      Background { color: Color },
      Foreground { color: Color },
      Style { style: Style },
      // NEW: Virtual text injection
      VirtualText {
          text: String,
          style: Style,
          position: VirtualTextPosition,  // Before/After char
      },
  }

  pub enum VirtualTextPosition {
      BeforeChar,   // Inline, before the character at this position
      AfterChar,    // Inline, after the character (typical for type hints)
  }
  ```

  **Why This Works:**
  - Reuses IntervalTree (`src/marker_tree.rs`) for O(log n) position tracking
  - Markers auto-adjust on edits via `adjust_for_edit()`
  - Viewport-based querying via `query_viewport()` scales to hundreds of hints
  - No buffer modification - virtual text is purely synthetic

  **Implementation Phases:**

  *Phase 1: Virtual Text Infrastructure* (~4-6 hours)
  - [ ] Add `VirtualText` variant to `OverlayFace` enum
  - [ ] Create `InlayHintManager` in new `src/inlay_hint.rs`
  - [ ] Add `inlay_hints: InlayHintManager` to `EditorState`
  - [ ] Extend rendering loop (`split_rendering.rs` ~line 810):
    ```rust
    // After adding the regular character Span:
    if let Some(virt) = check_virtual_text_after(byte_pos) {
        line_spans.push(Span::styled(virt.text, virt.style));
    }
    ```

  *Phase 2: LSP Integration* (~3-4 hours)
  - [ ] Add `textDocument/inlayHint` request to `LspHandle`
  - [ ] Fetch hints on viewport change (debounced) and after `didChange`
  - [ ] Convert LSP `InlayHint` to internal representation
  - [ ] Cache hints with `resultId` for incremental updates

  *Phase 3: Cursor & Interaction* (~3-4 hours)
  - [ ] Ensure cursor positioning skips virtual text (cursor column != display column)
  - [ ] Handle selection across virtual text
  - [ ] Support toggle keybinding (Ctrl+Alt to temporarily hide)
  - [ ] Configuration: `editor.inlayHints.enabled`

  *Phase 4: Polish* (~2 hours)
  - [ ] Hover tooltips for hints
  - [ ] Double-click to insert (if server provides `textEdit`)
  - [ ] Padding options (`padding_left`, `padding_right`)

  **Key Files:**
  - `src/marker_tree.rs` (880 lines) - IntervalTree with lazy deltas
  - `src/overlay.rs` (588 lines) - Extend with VirtualText face
  - `src/ui/split_rendering.rs` (1223 lines) - Modify rendering loop ~line 810
  - `src/lsp_diagnostics.rs` - Follow pattern for LSP → Overlay conversion

  **Alternative: Plugin-Based LSP Orchestration**

  If virtual text rendering is implemented as Core infrastructure with a plugin API:
  ```typescript
  editor.setInlayHints(bufferId, [
    { position: 42, text: ": i32", kind: "type" },
    { position: 100, text: "count:", kind: "parameter" }
  ]);
  ```
  Then the LSP protocol handling (request/response, caching, viewport tracking) could be done in a TypeScript plugin, similar to how VSCode separates Monaco rendering from extension-based providers.

#### LSP Architecture Improvements (P2) - **🦀 Core**

- [ ] **Feature Registration System**
  - Abstract features: `trait LspFeature { initialize(), clear() }`
  - Enables dynamic capability registration (LSP 3.16+)
  - **Effort:** 8-12 hours

- [ ] **Multi-Root Workspaces**
  - Support `Vec<WorkspaceFolder>` instead of single `root_uri`
  - **Effort:** 4-6 hours

#### LSP Core UX Features (P1) - **🦀 Core**

- [x] **Hover Documentation** ✅
  - Request `textDocument/hover` on Ctrl+K
  - Show documentation popup with symbol highlighting
  - Cancel on cursor move, keyboard dismissal (Esc)
  - **Note:** Markdown rendering not yet implemented (displays as plain text)
  - **Effort:** 4-6 hours

- [x] **Code Actions** ✅
  - Query `textDocument/codeAction` for quick fixes
  - Show menu/popup with available actions
  - Apply `WorkspaceEdit` changes (selection not yet implemented)
  - Keybinding: Ctrl+. to trigger code actions
  - **Effort:** 6-8 hours

- [x] **Find References** ✅
  - Request `textDocument/references` (Shift+F12 or command palette)
  - Display results in virtual buffer split view (plugin-based)
  - Jump to reference on Enter, navigate with n/p/j/k
  - **Effort:** 4-6 hours

- [x] **Signature Help** ✅
  - Request `textDocument/signatureHelp` on `(` and `,` (auto-trigger)
  - Show parameter hints in popup with active parameter highlighting
  - Keybinding: Ctrl+Shift+Space for manual trigger
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
- [ ] **Automatic split closure for plugin panels** - When a plugin creates a split (e.g., Find References, Diagnostics), the split should automatically close when the buffer is closed, rather than requiring each plugin to track and close the split manually. API suggestion: `createVirtualBufferInSplit` option `close_split_on_buffer_close: true`

### Test Infrastructure
- [ ] TypeScript plugin testing infrastructure (unit tests, mocking, test helpers) - **🔧 Infrastructure**
- [ ] Fix async file loading in test harness - **🦀 Core**
- [ ] Fix BIG.txt generation timing for scrolling tests - **🦀 Core**
- [ ] Add more E2E tests for complex workflows - **🦀 Core**
- [ ] Performance regression tests - **🦀 Core**

---

## Plugin API Gap Analysis: Fresh vs VSCode

> **Full comparison:** See [PLUGIN_API_COMPARISON.md](./PLUGIN_API_COMPARISON.md) for detailed method-by-method analysis.

### Executive Summary

**Overall Parity: ~25-30%** of VSCode's core extension capabilities.

- Fresh provides **~60 methods** via single `editor` API object
- VSCode provides **hundreds of methods** across 15+ namespaces
- Fresh's strength: Hybrid architecture (Rust core + TypeScript plugins for UI/workflows)
- Fresh's gap: No plugin-based language services (IntelliSense is not extensible by plugins)

### Architecture Difference

Fresh uses a **different model** than VSCode:
- **VSCode**: Everything extensible via plugins (language services, debuggers, SCM, tests)
- **Fresh**: Core features in Rust (LSP, syntax highlighting, debugging) + Plugins for UI/workflows

This is intentional - Fresh prioritizes performance and simplicity over infinite extensibility.

### Top 10 Critical Feature Gaps (for VSCode-like plugin ecosystem)

| Priority | Gap | Impact | Effort |
|----------|-----|--------|--------|
| **1** | Cursor/Selection Write | Cannot set cursor position programmatically | 4-6h |
| **2** | Execute Command | Cannot invoke commands from plugins | 2-3h |
| **3** | Basic Dialogs | No user confirmation/input dialogs | 4-6h |
| **4** | File Find (glob) | No `findFiles()` for project search | 3-4h |
| **5** | File Watchers | Cannot react to external file changes | 6-8h |
| **6** | Workspace Edit | No multi-file refactoring | 8-12h |
| **7** | Tree View Provider | No hierarchical UI panels | 12-16h |
| **8** | Plugin Configuration | Plugins have no settings API | 6-8h |
| **9** | Progress Indicator | No long-running task feedback | 4-6h |
| **10** | Clipboard Access | Cannot read/write clipboard | 2-3h |

### What Fresh DOES Well

- ✅ **Buffer Queries** - Full read access to buffers, cursors, selections
- ✅ **Basic Mutations** - Insert/delete text operations
- ✅ **Visual Overlays** - RGB highlighting with prefix management
- ✅ **File System** - Read/write files, stat, directory listing
- ✅ **Process Spawning** - Async external command execution
- ✅ **Event Hooks** - Non-blocking hooks for file/buffer/cursor events
- ✅ **Virtual Buffers** - Custom UI panels with embedded properties
- ✅ **Mode Definition** - Custom keybindings per plugin
- ✅ **Path Utilities** - Join, dirname, basename, etc.

### What Fresh CANNOT Do (Plugin Perspective)

- ❌ **Set cursor/selection** - Read-only access to cursor state
- ❌ **Execute commands** - Cannot invoke commands programmatically
- ❌ **Show dialogs** - No info/warning/error modals
- ❌ **Language providers** - No IntelliSense, hover, definition providers
- ❌ **Tree views** - No hierarchical data UI
- ❌ **Webviews** - No custom HTML/JS panels
- ❌ **Output channels** - No streaming log panels
- ❌ **Terminals** - No integrated terminal creation
- ❌ **Debug adapters** - No debugging infrastructure
- ❌ **SCM providers** - No git integration abstraction
- ❌ **Test controllers** - No test framework integration

### Recommended Implementation Roadmap

#### Phase 1: Essential Plugin APIs (Enables basic plugin authoring)

```typescript
// Priority additions to EditorAPI
setCursorPosition(buffer_id: number, position: number): boolean;
setSelections(selections: SelectionRange[]): boolean;
revealRange(buffer_id: number, start: number, end: number): boolean;
executeCommand(name: string, ...args: any[]): any;
getCommands(): string[];
showMessage(level: "info" | "warning" | "error", message: string): void;
findFiles(pattern: string, maxResults?: number): Promise<string[]>;
getClipboard(): Promise<string>;
setClipboard(text: string): Promise<void>;
```

**Effort: ~30-40 hours** | **Impact: High** - Enables navigation, refactoring, and interactive plugins.

#### Phase 2: Enhanced UI & Configuration

```typescript
// Configuration API
getConfiguration(section: string): Record<string, any>;
setConfiguration(section: string, key: string, value: any): void;
onConfigurationChange(callback: (section: string) => void): void;

// Progress API
withProgress(title: string, task: (report: (message: string) => void) => Promise<void>): Promise<void>;

// File Watching
watchFile(path: string, callback: (event: "change" | "delete") => void): Disposable;
```

**Effort: ~40-50 hours** | **Impact: Medium** - Plugin settings, progress feedback, reactive file updates.

#### Phase 3: Advanced Features (For full IDE-like plugins)

- Tree view providers (file explorer plugins, outline views)
- Output channels (build logs, LSP logs)
- Webview panels (custom HTML UIs)
- Workspace edit (multi-file refactoring)

**Effort: ~100+ hours** | **Impact: High** - Enables complex IDE features.

### Strategic Decision: Hybrid vs Full Extension Model

Fresh's current architecture is intentionally **hybrid**:
- **Core Rust** handles performance-critical features (LSP, rendering, editing)
- **TypeScript plugins** handle UI workflows (git integration, diagnostics panels)

**Pros of current approach:**
- Simpler plugin API surface
- Better performance (core features in Rust)
- Easier to maintain consistency
- Lower barrier for plugin authors

**Cons vs VSCode model:**
- Less extensible (can't replace core language services)
- Fewer potential plugins (no IntelliSense providers)
- Limited ecosystem growth

**Recommendation:** Stay with hybrid model but close the Phase 1 gaps to enable useful plugins like:
- Git status/blame integration
- Code navigation tools
- Project search/replace
- Custom formatters (via external tools)
- Bookmark/snippet managers

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

### Plugin Thread Architecture ✅ COMPLETED

**Solution**: Dedicated thread for TypeScript plugin execution with message passing.

**Architecture**:
```
Main Thread (UI)                    Plugin Thread
     │                                    │
     ├─────── PluginRequest ─────────────>│
     │        (RunHook, LoadPlugin, etc.) │
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

**Implementation** (src/plugin_thread.rs):
- [x] Phase 1: Created PluginThreadHandle with dedicated thread
  - JsRuntime and tokio Runtime owned by plugin thread
  - Spawned at editor startup via PluginThreadHandle::spawn()
  - Event loop receives requests and executes JS/TS code

- [x] Phase 2: Defined PluginRequest message types
  - `RunHook { hook_name, args }` - fire-and-forget
  - `ExecuteAction { action_name, response }` - oneshot channel response
  - `LoadPlugin { path, response }` - blocking load
  - `LoadPluginsFromDir { dir, response }` - load all plugins
  - `ProcessCommands { response }` - retrieve pending commands
  - `Shutdown` - clean termination

- [x] Phase 3: Refactored TypeScriptPluginManager
  - Replaced with PluginThreadHandle in Editor
  - `run_hook()` is non-blocking (fire-and-forget via channel)
  - `execute_action()` blocks with oneshot channel response
  - `load_plugins_from_dir()` blocks during startup only

- [x] Phase 4: Updated Editor integration
  - Editor uses `ts_manager.run_hook(name, args)` (non-blocking)
  - Removed all `run_hook_blocking()` calls
  - Results come via PluginCommand channel (setPromptSuggestions, etc.)

- [x] Phase 5: Plugin loading via message passing
  - `load_plugin()` sends request and blocks on response
  - `load_plugins_from_dir()` blocks during startup
  - Unload/reload supported via messages

**Additional cleanup completed**:
- Removed HookRegistry from Editor (TypeScript uses its own event system)
- Removed Lua plugin manager module
- Removed render-line hook system (can be reimplemented if needed)
- Simplified SplitRenderer (no hook parameters)

**Benefits achieved**:
- ✅ Complete isolation: Plugins can't block UI thread
- ✅ Stable async runtime: Single tokio runtime for all async ops
- ✅ Natural async model: Plugins use async/await without restrictions
- ✅ No race conditions: Sequential execution within plugin thread
- ✅ Matches industry standard: Similar to VSCode's Extension Host

**Limitation**: Hooks are now non-blocking, so plugins cannot intercept/cancel operations (e.g., before-insert to reject input). This could be addressed by adding a blocking hook variant with timeout for critical hooks if needed.

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

1. **High Priority**: Inlay Hints (LSP 3.17+) - **🦀 Core**
2. **Medium Priority**: Block selection operations (insert/delete/copy) - **🦀 Core**
3. **Medium Priority**: Complete virtual buffer infrastructure - **🔧 Infrastructure**
4. **Lower Priority**: Macro persistence, Git integration plugins
5. **Lower Priority**: Multi-Root Workspaces - **🦀 Core**

### Recent Completions (This Session)
- ✅ Pull Diagnostics (LSP 3.17+ textDocument/diagnostic, auto-trigger on open/change, result_id tracking)
- ✅ LSP Auto-Restart on Crash (exponential backoff, cooldown, manual restart command)
- ✅ Bracket auto-close and auto-pair deletion
- ✅ Jump to next/previous error (F8/Shift+F8)
- ✅ Macro recording and playback system (Alt+Shift+0-9 / Ctrl+Alt+0-9)
- ✅ Comprehensive E2E tests for smart editing features (25 tests)
- ✅ Block selection infrastructure (SelectionMode, Position2D, keybindings)
- ✅ Feature categorization (Core vs Plugin analysis)
- ✅ Block selection rendering (Alt+Shift+arrows highlight rectangular regions)
- ✅ Block selection E2E tests (5 tests covering all directions)
- ✅ Search history persistence (save/load to ~/.local/share/fresh/ with Drop impl)
- ✅ **Plugin Thread Architecture** (PluginThreadHandle with dedicated thread, non-blocking hooks, removed Lua/HookRegistry)
- ✅ **LSP Hover Documentation** (Ctrl+K shows docs, symbol highlighting, keyboard dismissal)
- ✅ LSP position calculation fixes (proper UTF-16 encoding for multi-byte chars)
- ✅ Hover popup positioning (2 lines below cursor to avoid obscuring symbol)
- ✅ TypeScript syntax highlighting improvements (full JavaScript support)
- ✅ Plugin API gap analysis documentation (Fresh vs VSCode comparison)
- ✅ **Find References** (Shift+F12 shows references in virtual buffer split, plugin-based display)
