# TODO

## Remaining Work

### Priority 0: TypeScript Plugin System Migration 🔥

**Goal:** Replace Lua plugin system with TypeScript/Deno for native async/await support, solving the fundamental async command execution problem.

**Why TypeScript over Lua:**
- Native async/await (no coroutine hacks)
- Massive ecosystem (npm packages)
- Type safety at compile time
- Better developer familiarity
- Solves the async command problem elegantly

**Binary Size Impact:** +10-20MB (V8 engine)
**Memory Impact:** +10-50MB runtime
**Startup Impact:** +50-100ms

---

#### 🚀 Progress Summary (as of latest commits)

**Phase 1 Status: ~90% Complete**
- ✅ **1.1 Deno Core Dependency** - DONE (deno_core 0.272.0 integrated)
- ✅ **1.2 TypeScript Runtime** - DONE (TypeScriptRuntime struct with JsRuntime wrapper)
- ✅ **1.3 Editor Ops** - 19 ops implemented (including async spawn_process!)
- ✅ **1.4 Type Definitions** - DONE (auto-generated + manual async types)

**Key Achievements:**
- V8 engine successfully embedded in Fresh
- Native async/await working (Promise-based ops)
- State sharing via Arc<RwLock<EditorStateSnapshot>>
- Commands sent via mpsc channel (PluginCommand enum)
- 20 passing tests covering runtime, ops, state, actions, and async ops
- Auto-generated TypeScript types from Rust code (19 ops)
- Sample TypeScript plugins created (hello_world.ts, bookmarks.ts)
- Command registration working (PluginAction for global functions)
- File opening with line/column positioning
- Split view operations (get active split, open file in split)
- **Async process spawning via native Promise** (spawn_process op)

**Remaining Phase 1 Work:**
- Hook registration ops (on, off, emit) - complex, requires JS→Rust callback mechanism
- Mode definition ops (optional for Phase 1)
- Async ops (create_virtual_buffer_in_split) - spawn_process DONE!

**Commits:**
1. `1eae5c8` - feat: Add TypeScript plugin runtime with deno_core
2. `80cb50b` - feat: Add comprehensive editor ops to TypeScript runtime
3. `d535dfa` - feat: Add auto-generated TypeScript types and additional editor ops
4. `2549fa5` - feat: Add command registration, file opening, and split view ops
5. `c610f9e` - test: Add focused unit tests for TypeScript runtime ops (16 tests total)
6. `f0b825c` - feat: Add comprehensive bookmarks plugin example using new ops
7. (pending) - feat: Add async spawn_process op with native Promise support

---

#### Phase 1: Core Infrastructure (Foundation)

##### 1.1 Add Deno Core Dependency ✅ **COMPLETE**
```toml
# Cargo.toml
[dependencies]
deno_core = "0.272.0"  # Or latest stable
tokio = { version = "1", features = ["full"] }
```

- [x] Add `deno_core` to Cargo.toml
- [x] Configure V8 platform initialization in main.rs
- [x] Set up basic JsRuntime with minimal ops
- [x] Test "hello world" TypeScript execution
- **Effort:** 2-4 hours ✅ **DONE**

##### 1.2 Create TypeScript Plugin Runtime ✅ **COMPLETE**
```rust
// src/ts_runtime.rs (NEW)
pub struct TypeScriptRuntime {
    js_runtime: JsRuntime,
    command_sender: mpsc::Sender<PluginCommand>,
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,
}

impl TypeScriptRuntime {
    pub fn new(...) -> Self { ... }
    pub async fn load_plugin(&mut self, path: &Path) -> Result<()> { ... }
    pub async fn execute_action(&mut self, name: &str) -> Result<()> { ... }
}
```

- [x] Create `src/ts_runtime.rs` module
- [x] Implement JsRuntime wrapper with Fresh-specific configuration
- [x] Set up module loader for TypeScript files
- [ ] Configure snapshot for faster startup (optional, can defer)
- **Effort:** 4-6 hours ✅ **DONE**

##### 1.3 Define Editor API as Deno Ops ✅ **PARTIALLY COMPLETE**
```rust
// src/ts_runtime.rs (ops defined inline)
#[op2(fast)]
fn op_fresh_set_status(state: &mut OpState, #[string] message: String) {
    // Sends PluginCommand::SetStatus via channel
}

#[op2(fast)]
fn op_fresh_add_overlay(
    state: &mut OpState,
    buffer_id: u32,
    #[string] overlay_id: String,
    start: u32,
    end: u32,
    r: u8, g: u8, b: u8,
    underline: bool,
) -> bool {
    // Sends PluginCommand::AddOverlay via channel
}

// 14 ops total implemented
```

- [x] Define ops inline in `src/ts_runtime.rs` (no separate module)
- [x] Implement synchronous ops (18 total):
  - `op_fresh_get_active_buffer_id` → returns current buffer ID ✅
  - `op_fresh_get_cursor_position` → returns cursor position ✅
  - `op_fresh_get_buffer_path` → returns buffer file path ✅
  - `op_fresh_get_buffer_length` → returns buffer length ✅
  - `op_fresh_is_buffer_modified` → returns modified status ✅
  - `op_fresh_set_status` → sets status message ✅
  - `op_fresh_debug` → debug logging ✅
  - `op_fresh_insert_text` → inserts text at position ✅
  - `op_fresh_delete_range` → deletes text range ✅
  - `op_fresh_insert_at_cursor` → convenience for cursor insert ✅
  - `op_fresh_add_overlay` → adds overlay ✅
  - `op_fresh_remove_overlay` → removes overlay ✅
  - `op_fresh_remove_overlays_by_prefix` → batch remove ✅
  - `op_fresh_clear_all_overlays` → clear all overlays ✅
  - `op_fresh_register_command` → registers command with PluginAction ✅
  - `op_fresh_open_file` → opens file at location (line/column) ✅
  - `op_fresh_get_active_split_id` → returns split ID ✅
  - `op_fresh_open_file_in_split` → opens file in specific split ✅
  - `op_define_mode` → defines buffer mode (TODO - optional for Phase 1)
- [x] Implement async ops (for I/O operations):
  - `op_fresh_spawn_process` → spawns external command with Promise ✅
  - `op_create_virtual_buffer_in_split` → creates buffer (TODO)
  - `op_fetch_url` → HTTP fetch (future)
- [x] Wire ops into JsRuntime extension via `extension!` macro
- **Effort:** 8-12 hours (8-10 hours done, 2-4 hours remaining)

##### 1.4 TypeScript Type Definitions ✅ **COMPLETE (Auto-Generated)**
```typescript
// types/fresh.d.ts (AUTO-GENERATED by build.rs)
declare global {
  const editor: EditorAPI;
}

interface EditorAPI {
  // Status and Logging
  setStatus(message: string): void;
  debug(message: string): void;

  // Buffer Queries
  getActiveBufferId(): number;
  getCursorPosition(): number;
  getBufferPath(buffer_id: number): string;
  getBufferLength(buffer_id: number): number;
  isBufferModified(buffer_id: number): boolean;

  // Buffer Mutations
  insertText(buffer_id: number, position: number, text: string): boolean;
  deleteRange(buffer_id: number, start: number, end: number): boolean;
  insertAtCursor(text: string): boolean;

  // Overlay Operations
  addOverlay(buffer_id: number, overlay_id: string, start: number, end: number, r: number, g: number, b: number, underline: boolean): boolean;
  removeOverlay(buffer_id: number, overlay_id: string): boolean;
  removeOverlaysByPrefix(buffer_id: number, prefix: string): boolean;
  clearAllOverlays(buffer_id: number): boolean;
}
```

- [x] Create `types/fresh.d.ts` with complete API definitions ✅
- [x] **Auto-generated via build.rs** - Types stay in sync with Rust ops automatically! ✅
- [x] Build script parses `src/ts_runtime.rs` and generates TypeScript types ✅
- [ ] Document all function signatures with JSDoc comments (TODO - enhance build.rs)
- [ ] Include example usage in comments (TODO)
- [x] Ship with editor binary (embedded via build process) ✅
- **Effort:** 2-3 hours ✅ **DONE** (build.rs codegen complete)

---

#### Phase 2: Plugin Manager Rewrite

##### 2.1 Replace PluginManager
```rust
// src/plugin_manager.rs (REWRITE)
pub struct PluginManager {
    runtime: TypeScriptRuntime,
    plugins: HashMap<String, PluginInfo>,
    // REMOVED: lua: Lua
    // REMOVED: command_receiver: mpsc::Receiver<PluginCommand>
}

impl PluginManager {
    pub fn new(api: PluginApi) -> Result<Self> {
        let runtime = TypeScriptRuntime::new(api)?;
        Ok(Self { runtime, plugins: HashMap::new() })
    }

    pub async fn load_plugin(&mut self, path: &Path) -> Result<()> {
        self.runtime.load_plugin(path).await
    }

    pub async fn execute_action(&mut self, action: &str) -> Result<()> {
        self.runtime.execute_action(action).await
    }

    // NO MORE process_commands() - commands execute immediately!
}
```

- [ ] Rewrite `src/plugin_manager.rs` to use TypeScriptRuntime
- [ ] Remove all mlua dependencies
- [ ] Remove command queue/receiver (commands are synchronous now!)
- [ ] Update plugin loading to handle `.ts` files
- [ ] Implement action execution via TypeScript runtime
- **Effort:** 6-8 hours

##### 2.2 Update Editor Integration
```rust
// src/editor.rs changes
impl Editor {
    pub async fn run_plugin_action(&mut self, action: &str) -> Result<()> {
        // Execute TypeScript action (may be async)
        self.plugin_manager.execute_action(action).await?;
        // State is already updated - no need to process command queue!
        Ok(())
    }

    // REMOVE: process_plugin_commands() - no longer needed
}
```

- [ ] Update `Editor` to use async plugin execution
- [ ] Remove `process_commands()` call from main loop
- [ ] Ensure plugin state snapshot is updated in real-time
- [ ] Handle async plugin actions in event loop (tokio integration)
- **Effort:** 4-6 hours

##### 2.3 Hook System Migration
```typescript
// TypeScript hook registration
editor.on("after-file-save", (args) => {
  editor.setStatus(`Saved: ${args.path}`);
  return true;
});

editor.on("render-line", (args) => {
  // Process line for highlighting
  return true;
});
```

- [ ] Implement hook registration in TypeScript ops
- [ ] Store hook callbacks in JsRuntime state
- [ ] Call TypeScript hooks from Rust (cross-language invocation)
- [ ] Ensure hook return values are properly handled
- **Effort:** 4-6 hours

---

#### Phase 3: Rewrite All Plugins in TypeScript

##### 3.1 Diagnostics Panel Plugin
```typescript
// plugins/diagnostics-panel.ts
const panelState = {
  open: false,
  currentIndex: 1,
  diagnostics: [] as Diagnostic[],
  bufferId: null as number | null,
  sourceSplitId: null as number | null,
};

// Define mode with keybindings
editor.defineMode("diagnostics-list", {
  parent: "special",
  bindings: {
    "RET": "goto_diagnostic",
    "n": "diagnostics_next",
    "p": "diagnostics_prev",
    "Down": "diagnostics_next",
    "Up": "diagnostics_prev",
  },
  readOnly: true,
});

// Show panel - ASYNC with proper buffer_id return!
async function showPanel() {
  panelState.sourceSplitId = editor.getActiveSplitId();
  panelState.diagnostics = getDiagnostics();
  panelState.currentIndex = 1;

  const entries = buildEntries();

  // THIS IS THE KEY: await returns buffer_id immediately!
  const bufferId = await editor.createVirtualBufferInSplit({
    name: "*Diagnostics*",
    mode: "diagnostics-list",
    readOnly: true,
    entries,
    ratio: 0.7,
    panelId: "diagnostics",
    showLineNumbers: false,
    showCursors: false,
  });

  // Now we have the correct buffer_id!
  panelState.bufferId = bufferId;
  panelState.open = true;

  // Apply overlays with correct buffer_id
  applyOverlays();

  editor.setStatus(`Diagnostics: ${panelState.diagnostics.length} items`);
}

function applyOverlays() {
  if (!panelState.bufferId) return;

  editor.removeOverlaysByPrefix(panelState.bufferId, "diag_");

  let offset = 0;
  for (let i = 0; i < panelState.diagnostics.length; i++) {
    const diag = panelState.diagnostics[i];
    const config = severityConfig[diag.severity];
    const lineText = formatDiagnostic(diag, i, i === panelState.currentIndex - 1);

    editor.addOverlay(
      panelState.bufferId,
      `diag_${i}_line`,
      offset,
      offset + lineText.length - 1,
      config.color[0], config.color[1], config.color[2],
      false
    );

    offset += lineText.length;
  }
}

// Register command
editor.registerCommand({
  name: "Show Diagnostics",
  description: "Show LSP diagnostics in a panel",
  action: "toggle_diagnostics_panel",
  contexts: ["normal"],
});

// Global function for action
globalThis.toggle_diagnostics_panel = async () => {
  if (panelState.open) {
    editor.setStatus("Diagnostics panel already open");
    return;
  }
  await showPanel();
};
```

- [ ] Rewrite `plugins/diagnostics-panel.lua` → `plugins/diagnostics-panel.ts`
- [ ] Use async/await for buffer creation
- [ ] Apply overlays immediately after buffer creation
- [ ] Test that colors render on first open (the original problem!)
- **Effort:** 3-4 hours

##### 3.2 Git Grep Plugin
```typescript
// plugins/git-grep.ts
editor.registerCommand({
  name: "Git Grep",
  description: "Search for text in git-tracked files",
  action: "start_git_grep",
  contexts: ["normal"],
});

globalThis.start_git_grep = async () => {
  // Use prompt API (needs to be implemented)
  const query = await editor.prompt("Git grep: ");
  if (!query) return;

  const result = await editor.spawn("git", ["grep", "-n", query]);
  if (result.exitCode !== 0) {
    editor.setStatus(`Git grep failed: ${result.stderr}`);
    return;
  }

  // Parse results and display
  const lines = result.stdout.split("\n").filter(Boolean);
  editor.setStatus(`Found ${lines.length} matches`);
  // ... show results in virtual buffer
};
```

- [ ] Rewrite `plugins/git-grep.lua` → `plugins/git-grep.ts`
- [ ] Rewrite `plugins/git-find-file.lua` → `plugins/git-find-file.ts`
- [ ] Rewrite `plugins/welcome.lua` → `plugins/welcome.ts`
- [ ] Rewrite `plugins/todo_highlighter.lua` → `plugins/todo_highlighter.ts`
- **Effort:** 4-6 hours total

##### 3.3 Remove Lua Infrastructure
- [ ] Remove `mlua` from Cargo.toml
- [ ] Delete old Lua plugin files (`.lua`)
- [ ] Remove Lua-specific code from plugin_api.rs
- [ ] Update documentation to reference TypeScript
- [ ] Remove Lua syntax highlighting configs (if any)
- **Effort:** 2-3 hours

---

#### Phase 4: Async Event Loop Integration

##### 4.1 Tokio Integration in Main Loop
```rust
// src/main.rs
#[tokio::main]
async fn main() {
    let mut editor = Editor::new().await?;

    loop {
        // Handle terminal events
        if let Some(event) = poll_event() {
            editor.handle_event(event).await?;
        }

        // Process async plugin tasks
        editor.plugin_manager.poll_pending().await?;

        // Render
        editor.render()?;

        // Sleep
        tokio::time::sleep(Duration::from_millis(16)).await;
    }
}
```

- [ ] Convert main.rs to use `#[tokio::main]`
- [ ] Make Editor methods async where needed
- [ ] Ensure TypeScript async ops integrate with tokio runtime
- [ ] Handle plugin Promise resolution in event loop
- **Effort:** 4-6 hours

##### 4.2 Command Execution Model
```rust
// NEW: Commands execute synchronously within ops
// No more command queue!

// Old (Lua):
// 1. Plugin calls editor.create_virtual_buffer()
// 2. Command queued
// 3. Plugin calls editor.add_overlay() with WRONG buffer_id
// 4. Commands processed LATER

// New (TypeScript):
// 1. Plugin calls await editor.createVirtualBufferInSplit()
// 2. Op executes IMMEDIATELY, creates buffer
// 3. Op returns buffer_id to TypeScript
// 4. Plugin calls editor.addOverlay() with CORRECT buffer_id
// 5. Op executes IMMEDIATELY
```

- [ ] Ensure all ops execute immediately (no queuing)
- [ ] Update EditorStateSnapshot in real-time after each op
- [ ] Verify buffer_id is returned correctly from async ops
- [ ] Test end-to-end: create buffer → add overlay → colors appear immediately
- **Effort:** 2-3 hours (mostly testing)

---

#### Phase 5: Testing & Documentation

##### 5.1 Test Suite
- [ ] Unit tests for TypeScript ops (Rust side)
- [ ] Integration tests for plugin loading
- [ ] E2E tests for diagnostics panel with colors
- [ ] Performance benchmarks (startup time, memory usage)
- [ ] Verify no memory leaks in JsRuntime
- **Effort:** 6-8 hours

##### 5.2 Documentation Updates
- [ ] Update `docs/PLUGINS.md` for TypeScript API
- [ ] Update `docs/ARCHITECTURE.md` to reflect new runtime
- [ ] Create `docs/TYPESCRIPT_MIGRATION.md` explaining the change
- [ ] Update README.md with new plugin development instructions
- [ ] Add TypeScript plugin examples
- **Effort:** 4-6 hours

##### 5.3 Developer Experience
- [ ] Ship `types/fresh.d.ts` with editor binary
- [ ] Add `tsconfig.json` template for plugins
- [ ] Provide plugin starter template
- [ ] Consider bundling TypeScript compiler or requiring pre-compilation
- **Effort:** 2-3 hours

---

#### Summary

**Total Estimated Effort:** 60-80 hours (2-3 weeks full-time)

**Key Benefits:**
1. ✅ Native async/await solves command timing problem
2. ✅ `await createVirtualBuffer()` returns buffer_id immediately
3. ✅ No more stale snapshot issues
4. ✅ Massive ecosystem (npm packages)
5. ✅ Type safety catches errors at compile time
6. ✅ Better developer familiarity

**Key Costs:**
1. ❌ +10-20MB binary size (V8 engine)
2. ❌ +10-50MB memory usage
3. ❌ +50-100ms startup time
4. ❌ More complex embedding (Deno ops vs mlua)
5. ❌ Rewrite all existing plugins
6. ❌ Lose Lua ecosystem (small but specialized)

**Critical Path:**
1. Phase 1.3 (Deno Ops) - Most complex, core of new system
2. Phase 2.1 (PluginManager Rewrite) - Replaces old architecture
3. Phase 3.1 (Diagnostics Plugin) - Validates the solution works
4. Phase 4.1 (Tokio Integration) - Makes async actually work

**Risk Mitigation:**
- Start with Phase 1.1-1.2 to validate Deno embedding works
- Build minimal proof-of-concept before full rewrite
- Keep old Lua code in git history (can revert if needed)
- Consider phased rollout with feature flag (but adds complexity)

---

### Priority 0.5: Menu Bar System (COMPLETE ✅)

**Completed**: Full menu bar implementation with F10/keyboard navigation, mouse interaction, Alt+letter mnemonics, keybinding display in dropdowns, JSON configuration, and Lua plugin API for runtime menu modification.

**Remaining Polish**:
- [ ] Test plugin menu augmentation
- [ ] Performance: test with many menus/items

---

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

### LSP Support (Robustness & Foundation)

**Goal:** Build a production-grade LSP client with solid foundations, inspired by VS Code's battle-tested architecture.

#### Phase 1: Core Robustness (P0 - Foundation) 🔥

- [x] **Client State Machine** (`lsp_async.rs:LspHandle`) ✅
    - Implemented `enum LspClientState { Initial, Starting, Initializing, Running, Stopping, Stopped, Error }`
    - Prevents invalid transitions with `can_transition_to()` and `transition_to()`
    - Status reporting to UI via `to_server_status()`
    - Fixed race condition allowing init from Starting state (commit a5c071a)
    - **Completed:** Full state machine with validation

- [ ] **Auto-Restart on Crash** (`lsp_error_handler.rs` - new file)
    - Detect server process death, track restart attempts with time window
    - Exponential backoff (1s, 2s, 4s, 8s delays)
    - Give up after 5 restarts in 3 minutes to prevent infinite loops
    - Notify user on crash with option to manually restart
    - **Impact:** High - resilient to transient server failures
    - **Effort:** Medium (4-6 hours)

- [x] **Request Cancellation** (`lsp_async.rs`) ✅
    - Added `CancelRequest` command and request tracking (`active_requests` HashMap)
    - Cancel stale completions when user types more characters
    - Send `$/cancelRequest` notification to server via `handle_cancel_request()`
    - LspHandle exposes `cancel_request()` for editor to call
    - Editor cancels pending requests on text changes
    - **Completed:** Full request cancellation with tracking and server notification

- [x] **Deferred Document Open** (`lsp_async.rs:LspHandle`) ✅
    - Queue pending DidOpen/DidChange/DidSave commands until initialization completes
    - Replay pending commands after successful init
    - Commands silently queued if server not initialized
    - **Completed:** Full command queueing with replay

#### Phase 2: Architecture Improvements (P1 - Scalability)

- [ ] **Feature Registration System** (`lsp_features.rs` - new file)
    - Abstract features: `trait LspFeature { initialize(), clear() }`
    - Dynamic features: `trait DynamicFeature: LspFeature { register(), unregister() }`
    - Modular completion, hover, diagnostics, etc. (separate files)
    - Enables dynamic capability registration (LSP 3.16+)
    - **Impact:** High - maintainability, extensibility
    - **Effort:** High (8-12 hours)

- [ ] **Pull Diagnostics** (`lsp_diagnostic_pull.rs` - new file)
    - Implement `textDocument/diagnostic` (LSP 3.17+)
    - Track `resultId` for incremental diagnostic updates
    - Background scheduler for inter-file dependencies (500ms interval)
    - Server can return "unchanged" instead of resending all diagnostics
    - **Impact:** High - reduces bandwidth, better for large projects
    - **Effort:** High (8-12 hours)

- [ ] **Multi-Root Workspaces** (`lsp_manager.rs`)
    - Support `Vec<WorkspaceFolder>` instead of single `root_uri`
    - Send `workspace/didChangeWorkspaceFolders` on add/remove
    - Essential for monorepos and multi-package projects
    - **Impact:** Medium - modern LSP clients expect this
    - **Effort:** Medium (4-6 hours)

#### Phase 3: Core UX Features (P1 - User-Facing)

- [ ] **Hover Documentation** (`editor.rs`, `lsp_async.rs`)
    - Request `textDocument/hover` on Ctrl+K or hover
    - Show documentation popup with markdown rendering
    - Cache results, cancel on cursor move
    - **Impact:** High - essential IDE feature
    - **Effort:** Medium (4-6 hours)

- [ ] **Code Actions** (`editor.rs`, `lsp_async.rs`)
    - Query `textDocument/codeAction` for quick fixes
    - Show menu/popup with available actions
    - Apply `WorkspaceEdit` changes
    - **Impact:** High - quick fixes are essential
    - **Effort:** Medium (6-8 hours)

- [ ] **Find References** (`editor.rs`, `lsp_async.rs`)
    - Request `textDocument/references`
    - Display results in quickfix/location list
    - Jump to reference on selection
    - **Impact:** High - navigation feature
    - **Effort:** Medium (4-6 hours)

- [ ] **Signature Help** (`editor.rs`, `lsp_async.rs`)
    - Request `textDocument/signatureHelp` on `(` and `,`
    - Show parameter hints in popup
    - Highlight active parameter
    - **Impact:** Medium - helpful for function calls
    - **Effort:** Medium (4-6 hours)

- [ ] **Diagnostics Panel** (See "Virtual Buffers & Diagnostic Panel" section below)
    - Requires virtual buffer infrastructure (Phase 1)
    - Plugin-implementable diagnostic list view
    - Follows Emacs special buffer philosophy
    - **Impact:** High - foundational for advanced plugin UIs
    - **Effort:** High (16-24 hours total for foundation + panel)

#### Phase 4: Developer Experience (P2 - Polish)

- [ ] **Middleware System** (`lsp_middleware.rs` - new file)
    - `trait Middleware { intercept_request(), intercept_notification() }`
    - Logging, metrics, request transformation
    - Better debugging and extensibility
    - **Impact:** Medium - helpful for debugging and testing
    - **Effort:** High (6-8 hours)

- [ ] **Document Selectors** (`lsp_document_selector.rs` - new file)
    - Match by language, scheme (`file`, `untitled`), glob pattern
    - Don't send `.rs` files in `/target/` or `/docs/` to rust-analyzer
    - More precise document routing
    - **Impact:** Medium - prevents unnecessary server load
    - **Effort:** Low (2-3 hours)

#### Already Complete ✅

- [x] Incremental text sync (sends ranges, not full documents)
- [x] Two-task architecture (command processor + stdout reader)
- [x] Request/response matching via shared HashMap
- [x] Command queueing before initialization (deferred document open)
- [x] Progress notifications (`$/progress`) with Begin/Report/End phases
- [x] Window messages (`window/showMessage`, `window/logMessage`)
- [x] Server status tracking with full state machine
- [x] UTF-16 position encoding with line cache
- [x] Client state machine with validated transitions (Initial→Starting→Initializing→Running→Stopping→Stopped→Error)
- [x] workDoneProgress capability enabled
- [x] CPU optimization (eliminated 46% busy-wait loop)
- [x] Request cancellation with $/cancelRequest notifications and request tracking

#### Deferred (Lower Priority)

- **Semantic Tokens** - Advanced highlighting (nice-to-have)
- **Inlay Hints** - Type annotations (nice-to-have)
- **Call/Type Hierarchy** - Advanced navigation (nice-to-have)
- **Log Viewer Panel** - UI polish (can use external tools)

---

**Next Steps:** Phase 1 is mostly complete (state machine ✅, deferred opens ✅). Focus on remaining P0 items (auto-restart, request cancellation) then move to Phase 3 user-facing features (hover, code actions, find references).

---

### Virtual Buffers & Diagnostic Panel (Emacs Philosophy)

**Goal:** Implement a diagnostic panel using an architecture that enables plugins to create rich UIs (Magit-style git interface, grep results, undo tree visualization, etc.) following Emacs' special buffer philosophy.

**Core Principle:** Everything is a buffer. Special buffers are regular buffers with specific modes that define keybindings and behavior.

#### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Emacs-Style Special Buffer Architecture           │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   Data Source (LSP Diagnostics, Git Status, Search Results, etc.)   │
│              │                                                       │
│              ▼                                                       │
│   ┌─────────────────┐                                               │
│   │  Virtual Buffer │  ← NOT backed by file                        │
│   │  (Read-Only)    │  ← Custom major mode                         │
│   │  *Diagnostics*  │  ← Text with embedded properties             │
│   └────────┬────────┘                                               │
│            │                                                         │
│   ┌────────┴────────┐                                               │
│   │ Buffer-Local    │  ← Mode-specific keybindings                 │
│   │   Keybindings   │  ← RET: goto, g: refresh, q: quit            │
│   └────────┬────────┘                                               │
│            │                                                         │
│            ▼                                                         │
│   ┌─────────────────┐                                               │
│   │  next-error     │  ← Global navigation (M-g n / M-g p)         │
│   │   Integration   │  ← Jump to source location                   │
│   └─────────────────┘                                               │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

#### Phase 1: Core Virtual Buffer Infrastructure

**Files to modify/create:**
- `src/buffer_kind.rs` - New buffer type enum
- `src/buffer_mode.rs` - Mode-based keybindings
- `src/text_property.rs` - Emacs-style text properties
- `src/state.rs` - Extend EditorState for virtual buffers
- `src/editor.rs` - Handle virtual buffer creation
- `src/plugin_api.rs` - Expose APIs to Lua

##### 1.1 Buffer Kind Distinction

```rust
// src/buffer_kind.rs
pub enum BufferKind {
    File {
        path: PathBuf,
        uri: Option<lsp_types::Uri>,
    },
    Virtual {
        name: String,           // e.g., "*Diagnostics*"
        mode: String,           // e.g., "diagnostics-list"
        read_only: bool,        // Usually true for special buffers
    },
}
```

- [ ] Add `BufferKind` enum to distinguish file vs virtual buffers
- [ ] Update `BufferMetadata` to use `BufferKind`
- [ ] Virtual buffers skip file I/O, dirty-checking, LSP notifications
- [ ] `*Name*` convention signals special buffer (Emacs style)

##### 1.2 Buffer Mode System (Buffer-Local Keybindings)

```rust
// src/buffer_mode.rs
pub struct BufferMode {
    pub name: String,                           // "diagnostics-list"
    pub parent: Option<String>,                 // "special" for inheritance
    pub keybindings: HashMap<KeyEvent, String>, // Key → command name
    pub read_only: bool,                        // Default read-only?
}

// Built-in "special" mode (base for all special buffers)
fn special_mode() -> BufferMode {
    BufferMode {
        name: "special".into(),
        parent: None,
        keybindings: hashmap! {
            key!('q') => "close-buffer".into(),
            key!('g') => "revert-buffer".into(),
        },
        read_only: true,
    }
}
```

- [ ] Create `BufferMode` struct with keybindings and inheritance
- [ ] Implement mode lookup with inheritance chain (child → parent → global)
- [ ] Add built-in `special` mode (q=quit, g=refresh)
- [ ] Mode registry: `HashMap<String, BufferMode>`
- [ ] When dispatching keypress, check buffer's mode keybindings first

##### 1.3 Text Properties (Metadata on Text Ranges)

```rust
// src/text_property.rs
pub struct TextProperty {
    pub start: usize,                           // Byte offset
    pub end: usize,                             // Byte offset
    pub properties: HashMap<String, serde_json::Value>,
}

// Example: diagnostic entry with location metadata
TextProperty {
    start: 0,
    end: 45,  // "Error at src/main.rs:42:10: undefined var\n"
    properties: hashmap! {
        "location" => json!({
            "file": "src/main.rs",
            "line": 42,
            "column": 10,
        }),
        "severity" => json!("error"),
        "diagnostic_id" => json!("lsp-diagnostic-L42C10-abc123"),
    },
}
```

- [ ] Add `TextProperty` struct for embedding metadata in text
- [ ] Store properties in `EditorState` for virtual buffers
- [ ] Query properties at cursor position
- [ ] Properties preserved during buffer content updates

##### 1.4 Virtual Buffer Rendering

- [ ] Virtual buffers render like file buffers (same rendering pipeline)
- [ ] Read-only buffers block text insertion/deletion
- [ ] Show `[RO]` indicator in status bar for read-only buffers
- [ ] Optional: different background color for special buffers

##### 1.5 Plugin API Extensions

```lua
-- Create virtual buffer (Emacs: get-buffer-create)
local buf_id = editor.create_virtual_buffer("*Diagnostics*", {
    mode = "diagnostics-list",
    read_only = true,
})

-- Set buffer content with text properties
editor.set_buffer_content(buf_id, {
    {
        text = "Error at src/main.rs:42:10: undefined variable 'foo'\n",
        properties = {
            location = { file = "src/main.rs", line = 42, column = 10 },
            severity = "error",
        }
    },
    {
        text = "Warning at src/lib.rs:100:5: unused variable\n",
        properties = {
            location = { file = "src/lib.rs", line = 100, column = 5 },
            severity = "warning",
        }
    },
})

-- Get properties at current cursor position
local props = editor.get_text_properties_at_point()
-- Returns: { location = {...}, severity = "error" }

-- Define custom mode with keybindings
editor.define_mode("diagnostics-list", {
    parent = "special",  -- Inherits q=quit, g=refresh
    bindings = {
        ["Return"] = "diagnostics:goto",  -- Custom command
        ["n"] = "next-line",              -- Standard movement
        ["p"] = "previous-line",
        ["e"] = "diagnostics:filter-errors",
        ["w"] = "diagnostics:filter-warnings",
    },
})

-- Show buffer in current split
editor.show_buffer(buf_id)

-- Switch to buffer without changing split layout
editor.switch_to_buffer(buf_id)
```

- [ ] `editor.create_virtual_buffer(name, options)` - Create special buffer
- [ ] `editor.set_buffer_content(buf_id, rich_text)` - Set content with properties
- [ ] `editor.get_text_properties_at_point()` - Query properties at cursor
- [ ] `editor.define_mode(name, config)` - Define buffer mode with keybindings
- [ ] `editor.show_buffer(buf_id)` - Display buffer in current split
- [ ] `editor.get_buffer_kind(buf_id)` - Check if file or virtual

##### 1.6 Next-Error Navigation Pattern

```rust
// Global next-error state
pub struct NextErrorState {
    pub source_buffer: Option<BufferId>,  // Which buffer provides locations
    pub current_index: usize,              // Current position in list
}

// Keybindings (global, not buffer-local)
// M-g n → next-error
// M-g p → previous-error
// M-g M-g → first-error
```

```lua
-- Plugin registers as next-error source
editor.set_next_error_source(diagnostics_buffer, function(direction, reset)
    -- direction: 1 for next, -1 for previous
    -- reset: true to start from beginning
    if reset then
        editor.goto_line(diagnostics_buffer, 1)
    else
        local current_line = editor.get_cursor_line(diagnostics_buffer)
        editor.goto_line(diagnostics_buffer, current_line + direction)
    end

    local props = editor.get_text_properties_at_point(diagnostics_buffer)
    if props.location then
        return props.location  -- {file, line, column}
    end
end)
```

- [ ] Add `NextErrorState` to Editor for global navigation
- [ ] Implement `next-error` and `previous-error` commands
- [ ] Plugin API: `editor.set_next_error_source(buf_id, callback)`
- [ ] Navigation works from any buffer (jumps to source location)
- [ ] Bind M-g n / M-g p globally

##### 1.7 Revert Buffer Mechanism

```lua
-- Plugin defines how to refresh buffer content
editor.set_revert_function(diagnostics_buffer, function()
    -- Re-collect diagnostics and update buffer
    local entries = collect_current_diagnostics()
    editor.set_buffer_content(diagnostics_buffer, entries)
end)
```

- [ ] `revert-buffer` command (g in special mode) calls buffer's revert function
- [ ] Plugin API: `editor.set_revert_function(buf_id, callback)`
- [ ] For virtual buffers: regenerate content (not re-read from disk)

#### Phase 2: Diagnostic Panel Plugin

With Phase 1 infrastructure, the diagnostic panel becomes a Lua plugin:

**File:** `plugins/diagnostics-panel.lua`

```lua
-- plugins/diagnostics-panel.lua
local diagnostics_buffer = nil
local current_filter = "all"  -- "all", "errors", "warnings", "info"

-- Register mode for diagnostic list
editor.define_mode("diagnostics-list", {
    parent = "special",
    bindings = {
        ["Return"] = "diagnostics:goto",
        ["n"] = "next-line",
        ["p"] = "previous-line",
        ["g"] = "revert-buffer",
        ["q"] = "close-buffer",
        ["e"] = "diagnostics:filter-errors",
        ["w"] = "diagnostics:filter-warnings",
        ["a"] = "diagnostics:show-all",
    },
})

-- Collect diagnostics from current buffer's overlays
local function collect_diagnostics()
    local entries = {}
    local overlays = editor.get_overlays({ prefix = "lsp-diagnostic-" })

    for _, overlay in ipairs(overlays) do
        local severity = parse_severity(overlay.priority)
        if matches_filter(severity, current_filter) then
            table.insert(entries, {
                text = format_diagnostic_line(overlay),
                properties = {
                    location = {
                        file = editor.get_active_buffer_path(),
                        line = overlay.line,
                        column = overlay.column,
                    },
                    severity = severity,
                    message = overlay.message,
                }
            })
        end
    end

    -- Sort by severity (errors first), then by line number
    table.sort(entries, function(a, b)
        if a.properties.severity ~= b.properties.severity then
            return severity_rank(a.properties.severity) < severity_rank(b.properties.severity)
        end
        return a.properties.location.line < b.properties.location.line
    end)

    return entries
end

local function format_diagnostic_line(overlay)
    local icon = severity_icon(overlay.priority)
    local file = editor.get_relative_path(overlay.file)
    return string.format("%s %s:%d:%d: %s\n",
                         icon, file, overlay.line, overlay.column, overlay.message)
end

-- Main command to show diagnostics panel
editor.register_command("diagnostics:show", function()
    if not diagnostics_buffer then
        diagnostics_buffer = editor.create_virtual_buffer("*Diagnostics*", {
            mode = "diagnostics-list",
            read_only = true,
        })

        -- Set up revert (refresh) function
        editor.set_revert_function(diagnostics_buffer, function()
            local entries = collect_diagnostics()
            if #entries == 0 then
                editor.set_buffer_content(diagnostics_buffer, {
                    { text = "No diagnostics.\n", properties = {} }
                })
            else
                editor.set_buffer_content(diagnostics_buffer, entries)
            end
        end)

        -- Register as next-error source
        editor.set_next_error_source(diagnostics_buffer, function(direction, reset)
            -- Navigate within diagnostics buffer
            local line = editor.get_cursor_line(diagnostics_buffer)
            if reset then
                line = 1
            else
                line = line + direction
            end
            editor.goto_line(diagnostics_buffer, line)
            return editor.get_text_properties_at_point(diagnostics_buffer).location
        end)
    end

    -- Refresh and show
    editor.revert_buffer(diagnostics_buffer)
    editor.show_buffer(diagnostics_buffer)
end)

-- Jump to diagnostic location
editor.register_command("diagnostics:goto", function()
    local props = editor.get_text_properties_at_point()
    if props and props.location then
        editor.open_file(props.location.file, {
            line = props.location.line,
            column = props.location.column,
        })
    end
end)

-- Filter commands
editor.register_command("diagnostics:filter-errors", function()
    current_filter = "errors"
    editor.revert_buffer(diagnostics_buffer)
end)

editor.register_command("diagnostics:filter-warnings", function()
    current_filter = "warnings"
    editor.revert_buffer(diagnostics_buffer)
end)

editor.register_command("diagnostics:show-all", function()
    current_filter = "all"
    editor.revert_buffer(diagnostics_buffer)
end)

-- Auto-refresh when diagnostics change
editor.on_hook("diagnostics-published", function(args)
    if diagnostics_buffer and editor.buffer_is_visible(diagnostics_buffer) then
        editor.revert_buffer(diagnostics_buffer)
    end
end)

-- Add menu item
editor.add_menu_item("View", {
    label = "Diagnostics Panel",
    command = "diagnostics:show",
    keybinding = "Ctrl+Shift+M",
})
```

- [ ] Create `plugins/diagnostics-panel.lua` as reference implementation
- [ ] Show diagnostics from current file with severity icons (✗, ⚠, ℹ, ●)
- [ ] Filter by severity (errors only, warnings only, all)
- [ ] Jump to location on RET
- [ ] Refresh on 'g' or automatically on diagnostic updates
- [ ] Integrate with next-error navigation
- [ ] Add command palette entry and keybinding
- [ ] Add to View menu

#### Phase 3: Enhanced Features (Future)

##### 3.1 Workspace-Wide Diagnostics
- [ ] Collect diagnostics from ALL open buffers
- [ ] Group by file or show flat list
- [ ] Support LSP workspace diagnostics (not just open files)

##### 3.2 Tabulated List Mode (Emacs-style)
```lua
-- Column-based display with sorting
editor.set_tabulated_format(diagnostics_buffer, {
    { name = "Sev", width = 3, sortable = true },
    { name = "File", width = 30, sortable = true },
    { name = "Line", width = 6, sortable = true },
    { name = "Message", width = 0, sortable = false },  -- 0 = fill remaining
})
```
- [ ] Column headers with clickable sorting
- [ ] Automatic alignment
- [ ] Consistent appearance across all list buffers

##### 3.3 Interactive Elements
- [ ] Clickable file paths (mouse support)
- [ ] Expandable details (show full diagnostic message)
- [ ] Quick-fix actions inline (if available)

#### Benefits of This Architecture

1. **Emacs-Aligned Philosophy**:
   - Special buffers are regular buffers with modes
   - Text as universal interface with embedded metadata
   - next-error pattern for global navigation
   - Composition over monolithic UIs

2. **Plugin-First Design**:
   - Core provides primitives (virtual buffers, modes, properties)
   - Diagnostic panel is just one plugin using these primitives
   - Same infrastructure enables: Magit, grep results, test results, undo tree

3. **Minimal Core Changes**:
   - BufferKind enum (file vs virtual)
   - Mode-based keybindings (natural extension)
   - Text properties (straightforward metadata)
   - Reuses existing buffer rendering and management

4. **Separation of Concerns**:
   - **Data Source**: LSP diagnostics (existing overlay system)
   - **Storage**: Virtual buffer with text properties (new infrastructure)
   - **Presentation**: Plugin-controlled formatting (Lua code)
   - **Navigation**: next-error pattern (global, reusable)

5. **Future Extensibility**:
   - Git Magit interface (same pattern: virtual buffer + custom mode)
   - Grep/search results (virtual buffer with location properties)
   - Undo tree visualizer (virtual buffer with tree data)
   - Test runner results (virtual buffer with pass/fail markers)

#### Files to Modify

**New Files:**
- `src/buffer_kind.rs` - BufferKind enum
- `src/buffer_mode.rs` - BufferMode struct and registry
- `src/text_property.rs` - TextProperty struct
- `plugins/diagnostics-panel.lua` - Reference implementation

**Modified Files:**
- `src/state.rs` - Extend EditorState for virtual buffers, properties
- `src/editor.rs` - Handle virtual buffer creation, mode switching
- `src/keybindings.rs` - Check buffer mode keybindings before global
- `src/plugin_api.rs` - New Lua API functions
- `src/hooks.rs` - Add DiagnosticsPublished hook
- `src/rendering.rs` - Handle read-only indicator in status bar
- `src/lsp_diagnostics.rs` - Emit hook after applying diagnostics

#### Estimated Effort

- **Phase 1 (Core Infrastructure)**: 12-16 hours
  - BufferKind enum: 2h
  - BufferMode system: 4h
  - TextProperty: 3h
  - Plugin API extensions: 4h
  - Next-error navigation: 3h

- **Phase 2 (Diagnostic Panel Plugin)**: 4-8 hours
  - Basic panel: 3h
  - Filtering: 2h
  - Auto-refresh: 1h
  - Polish & testing: 2h

- **Total**: 16-24 hours

This investment pays dividends: the same infrastructure enables many advanced plugin UIs (Magit, Telescope, grep results, etc.) with no additional core work.

---

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

### Priority 4.5: Unified Event System for Control & Observation

**Goal**: Create a coherent event architecture that unifies hooks (plugin callbacks), control events (observable state changes), and script control mode waiting into a single elegant system.

**Current State**:
- **Hooks** (`HookArgs`) - Internal plugin callbacks that can intercept/cancel operations
- **Edit Events** (`Event`) - Undo/redoable buffer changes
- **Control Events** (`ControlEvent`) - Observable notifications for external systems (new)
- **Script Control Mode** - External automation via JSON commands

**Problem**: These systems have overlapping concerns but aren't unified:
- Hooks fire for plugins but aren't observable externally
- Control events exist but editor doesn't emit them yet
- Script mode has hardcoded state polling instead of waiting for semantic events
- Plugins can't emit custom events for other plugins to observe

**Proposed Architecture**:

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Event Flow Architecture                       │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   User Action / Editor Operation                                     │
│              │                                                       │
│              ▼                                                       │
│   ┌─────────────────┐                                               │
│   │   Pre-Hooks     │  ← Plugins can intercept & cancel             │
│   │ (Before* hooks) │                                               │
│   └────────┬────────┘                                               │
│            │ continue?                                               │
│            ▼                                                         │
│   ┌─────────────────┐                                               │
│   │  Execute Action │  ← Core editor operation                      │
│   └────────┬────────┘                                               │
│            │                                                         │
│            ▼                                                         │
│   ┌─────────────────┐                                               │
│   │   Post-Hooks    │  ← Plugins react to completion                │
│   │ (After* hooks)  │                                               │
│   └────────┬────────┘                                               │
│            │                                                         │
│            ▼                                                         │
│   ┌─────────────────┐                                               │
│   │  Emit Control   │  ← Observable by external systems             │
│   │     Event       │    (Script Mode, other plugins)               │
│   └────────┬────────┘                                               │
│            │                                                         │
│            ├─────────────────┬──────────────────┐                   │
│            ▼                 ▼                  ▼                   │
│   ┌──────────────┐  ┌──────────────┐  ┌───────────────┐            │
│   │ Event Stream │  │   Plugin     │  │  Script Mode  │            │
│   │  Subscribers │  │  Listeners   │  │   wait_for    │            │
│   └──────────────┘  └──────────────┘  └───────────────┘            │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

**Implementation Tasks**:

#### Phase 1: Editor Emits Control Events
- [ ] Add `EventBroadcaster` to `Editor` struct
- [ ] Emit `FileOpened` after `open_file()` succeeds
- [ ] Emit `FileSaved` after `save_buffer()` succeeds
- [ ] Emit `FileClosed` after buffer close
- [ ] Emit `LspStatusChanged` when LSP server state changes
- [ ] Emit `PopupShown`/`PopupHidden` when popups toggle
- [ ] Emit `SearchCompleted` after find operations
- [ ] Emit `CompletionReceived` when LSP completions arrive
- [ ] Emit `DiagnosticsUpdated` when LSP diagnostics change

#### Phase 2: Plugin Event API
- [ ] Lua API: `editor.emit_event(event_type, data)` - Plugins emit custom events
- [ ] Lua API: `editor.on_event(pattern, callback)` - Subscribe to events
- [ ] Lua API: `editor.wait_for_event(pattern, timeout)` - Async wait for events
- [ ] Event namespacing: `plugin:my_plugin:custom_event` to avoid collisions
- [ ] Event filtering: Subscribe to specific patterns, not all events

```lua
-- Plugin A: Emits events
local function on_git_status_ready(status)
  editor.emit_event("plugin:git:status_changed", {
    branch = status.branch,
    modified = status.modified_count,
    staged = status.staged_count
  })
end

-- Plugin B: Listens for events
editor.on_event("plugin:git:status_changed", function(data)
  update_status_line(data.branch)
end)

-- Plugin C: Waits for event
local event = editor.wait_for_event("plugin:lsp:ready", 5000)
if event then
  -- LSP is ready, do something
end
```

#### Phase 3: Unify Hooks and Control Events
- [ ] Hooks automatically emit corresponding ControlEvents after completion
- [ ] `AfterFileOpen` hook → emits `FileOpened` control event
- [ ] `AfterFileSave` hook → emits `FileSaved` control event
- [ ] `PostCommand` hook → emits `CommandExecuted` control event
- [ ] Single source of truth: hooks define what happens, events broadcast that it happened

```rust
// In editor.rs - after running hooks, emit control event
fn save_file(&mut self) -> io::Result<()> {
    let path = self.get_current_path();

    // Pre-hook (can cancel)
    if !self.hooks.run_hooks("before-file-save", &HookArgs::BeforeFileSave { ... }) {
        return Ok(()); // Cancelled
    }

    // Do the actual save
    self.write_to_disk()?;

    // Post-hook (inform plugins)
    self.hooks.run_hooks("after-file-save", &HookArgs::AfterFileSave { ... });

    // Emit control event (broadcast to external observers)
    self.event_broadcaster.emit(ControlEvent::FileSaved {
        path: path.to_string()
    });

    Ok(())
}
```

#### Phase 4: Script Mode Integration
- [ ] Script mode subscribes to `EventBroadcaster`
- [ ] `wait_for` uses event stream instead of polling (where applicable)
- [ ] Event-based waiting is more reliable than screen scraping
- [ ] Backwards compatible: state-based polling still available as fallback

```json
// Wait for LSP to be ready (event-based, clean)
{"type": "wait_for", "condition": {
  "type": "event_match",
  "pattern": {"pattern": "lsp_status", "language": "rust", "status": "running"}
}}

// Wait for completion popup (event-based)
{"type": "wait_for", "condition": {
  "type": "event_match",
  "pattern": {"pattern": "completion_received"}
}}

// Fallback: screen contains text (state-based polling)
{"type": "wait_for", "condition": {
  "type": "screen_contains",
  "text": "Error"
}}
```

#### Phase 5: Advanced Event Features
- [ ] Event replay for debugging/testing
- [ ] Event filtering/routing (some events only to certain subscribers)
- [ ] Event history with timestamps for debugging
- [ ] Event serialization for test generation
- [ ] Rate limiting for high-frequency events (cursor moves, typing)

**Benefits**:

1. **Coherent Architecture**: Single event flow for all observation needs
2. **Plugin Interoperability**: Plugins can communicate via events
3. **External Automation**: Script mode waits for semantic events, not screen scraping
4. **Debugging**: Event stream provides audit trail of what happened
5. **Test Generation**: Record events to generate reproducible tests
6. **Extensibility**: New event types don't require core changes

**Design Principles**:

- **Hooks are for interception**: Can cancel operations, synchronous, internal
- **Events are for observation**: Cannot cancel, broadcast after completion, external
- **Unidirectional flow**: Operations → Hooks → Events → Observers
- **No hardcoded conditions**: Script mode uses event patterns, not string matching
- **Namespace isolation**: Plugin events prefixed to avoid collisions

**Example: Complete LSP Completion Flow**

```
User presses Ctrl+Space
        │
        ▼
PreCommand hook (Action::LspCompletion)
        │
        ▼
Editor requests completion from LSP
        │
        ▼
LSP async handler receives items
        │
        ▼
Emit ControlEvent::CompletionReceived { item_count: 15 }
        │
        ├──────────────────────┬────────────────────┐
        ▼                      ▼                    ▼
Script mode sees event   Plugin logs "15 items"   Status bar updates
wait_for completes       received
```

**Files Involved**:
- `src/control_event.rs` - ControlEvent enum and EventBroadcaster
- `src/hooks.rs` - HookArgs and HookRegistry
- `src/editor.rs` - Emit events after operations
- `src/script_control.rs` - Wait for events
- `src/plugin_api.rs` - Lua bindings for emit/subscribe

### Priority 5: Plugin System (Advanced APIs) ✅ **Git Refactoring Complete**

**Completed:** Git grep and git find file converted to pure Lua plugins, removing ~465 lines of Rust code.
**Implementation:** Hook-based prompt API (prompt-changed/confirmed/cancelled), string-based function mapping, file opening API.
**Result:** Demonstrated plugin system power, reduced core complexity, made git features fully customizable.

**Remaining Advanced APIs:**

#### Interactive UI API
- [ ] Virtual buffers / selection lists / input dialogs (partially addressed by Prompt API in Priority 5)
- [ ] Read-only buffers
- [ ] Generic popups (custom content rendering)
- [ ] Tree/list widgets for structured data

#### Modal Interaction & Navigation
- [ ] Define custom modes/contexts (needed for git grep results, magit)
- [ ] Buffer-local keybindings (partially addressed in Priority 5)
- [ ] Goto line/position API, set selection, scroll control
- [ ] Cursor manipulation API

#### Enhanced Hooks & Integration
- [ ] More hooks: `on_buffer_open`, `on_selection_change`, `on_key_press`, `on_cursor_moved`
- [ ] State persistence API (plugin configuration, session state)
- [ ] LSP access API (query diagnostics, trigger completion from plugins)
- [ ] Search API (invoke search from plugins, get search state)
- [ ] Undo history API (query undo tree, create undo boundaries)
- [ ] Process cancellation support (cancel long-running spawned processes)

#### Target Showcase Plugins
- [ ] Git grep & find files (Priority 5 - in progress)
- [ ] Magit-style Git interface (needs virtual buffers + custom contexts)
- [ ] Telescope-style fuzzy finder (reuses prompt API from Priority 5)
- [ ] Undo tree visualizer (needs virtual buffers + undo history API)
- [ ] Project search & replace (needs search API + prompt API)
- [ ] Git blame (line annotations + async git commands)

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

**Current Problem**: Line number ↔ byte offset conversions are a major performance bottleneck:
- `populate_line_cache()` takes **61.95%** of diagnostic processing time
- Line cache uses eager updates on edits: O(K log K) where K = cached lines (can be 1000+)
- Separate systems for markers (interval tree with lazy deltas) and lines (BTreeMap with eager updates)

**Proposed Solution**: Unify line tracking into the existing IntervalTree marker system

#### Architecture Overview

Lines ARE intervals! A line is just the interval between two newlines:
- Line 0: `[0, first_\n)`
- Line 1: `[first_\n, second_\n)`
- Line N: `[nth_\n, (n+1)th_\n)`

**Key Insight**: The marker tree already has lazy delta propagation for edits. We can represent lines as special markers and get O(log N) edits for free!

```
Current:
┌──────────┐  ┌──────────────┐  ┌─────────────┐
│  Buffer  │  │  MarkerList  │  │  LineCache  │
│          │  │              │  │             │
│ Virtual  │  │ IntervalTree │  │  BTreeMap   │
│ Buffer   │  │ (lazy Δ) ✅  │  │ (eager) ❌  │
└──────────┘  └──────────────┘  └─────────────┘
     │              │                    │
     └─────Edit─────┴──────────────────┘
           │                             │
      O(chunks)                    O(K log K) SLOW!

Proposed:
┌──────────┐  ┌──────────────────────────────┐
│  Buffer  │  │  UnifiedMarkerTree           │
│          │  │                              │
│ Virtual  │  │  IntervalTree with:          │
│ Buffer   │  │  - Position markers          │
│          │  │  - LINE markers (newlines)   │
│          │  │  Both use lazy Δ! ✅         │
└──────────┘  └──────────────────────────────┘
     │                     │
     └─────Edit────────────┘
           │
      Both O(log N) FAST!
```

#### How It Works

**1. Initialization (File Load)**
```rust
// Scan buffer ONCE to find all newlines
let mut byte = 0;
let mut line_num = 0;
for newline_pos in buffer.find_all_newlines() {
    marker_tree.insert_line_marker(byte..newline_pos, line_num);
    byte = newline_pos;
    line_num += 1;
}
```
**Cost**: O(N) scan + O(L log L) insertions where L = line count
**When**: Only on file load, NOT on every diagnostic update!

**2. Edits (Every Keystroke)**
```rust
Event::Insert { position, text } => {
    // Adjust ALL markers (positions + lines) with lazy deltas
    marker_tree.adjust_for_edit(position, +text.len());  // O(log N) ✅

    // If text contains newlines, invalidate and rescan affected region
    if text.contains('\n') {
        marker_tree.invalidate_lines(position..position+text.len());
        rescan_lines(affected_region);  // O(M) where M = affected lines
    }

    buffer.insert(position, text);
}
```
**Cost for edit WITHOUT newlines**: O(log N) - just lazy delta! ✅
**Cost for edit WITH newlines**: O(log N) + O(M) where M = affected lines (usually 1-5) ✅

**3. Query: Line Number → Byte Offset**
```rust
fn line_to_byte(&self, line_num: usize) -> usize {
    // Query marker tree for line marker
    if let Some(marker) = self.marker_tree.get_line_marker(line_num) {
        return marker.start + marker.pending_delta;  // O(log N)
    }
    // Not cached - scan from nearest known line
    scan_from_nearest(line_num)  // O(M) where M = distance
}
```

**4. Query: Byte Offset → Line Number**
```rust
fn byte_to_line(&self, byte_offset: usize) -> usize {
    // Use interval tree range query - lines ARE intervals!
    let markers = self.marker_tree.query_lines(byte_offset, byte_offset+1);
    markers.first().map(|m| m.line_number)  // O(log N + k) where k=1
}
```

#### Marker Types

```rust
enum MarkerType {
    Position {
        overlay_id: Option<String>,
        affinity: bool,
    },
    Line {
        line_number: usize,
        // interval.start = line start (after previous \n)
        // interval.end = line end (at next \n)
    },
}

struct Marker {
    id: MarkerId,
    interval: Range<u64>,
    marker_type: MarkerType,
}
```

#### Huge File Strategy: Anchor-Based Line Numbering

**Problem**: For huge files (1GB+, 10M lines), there's no "nearest cached line" for random access:
- LSP diagnostic at line 8,500,000
- No cached lines nearby
- Scanning from line 0 or even "nearest" line (could be millions of lines away) is unacceptable

**Solution: Estimated Anchors + Sparse Network**

Instead of exact line numbers everywhere, use **byte-anchored positions with estimated line numbers**:

```rust
struct LineAnchor {
    byte_offset: usize,           // Known: exact byte position
    estimated_line: usize,        // May be estimated from avg line length
    confidence: AnchorConfidence,
}

enum AnchorConfidence {
    Exact,                  // Scanned from known position
    Estimated,              // Calculated from avg line length
    Relative(MarkerId),     // Relative to parent anchor
}
```

**Key Operations:**

1. **Create Anchor at Line N (no long scan)**
```rust
// Need line 8,500,000 but no nearby anchors
let estimated_byte = 8_500_000 * avg_line_length;  // ~850MB
let line_start = scan_to_prev_newline(estimated_byte);  // O(100 bytes)
create_anchor(line_start, 8_500_000, Estimated);
// Cost: O(avg_line_length) not O(millions of lines)! ✅
```

2. **Relative Anchoring for Nearby Lines**
```rust
// Diagnostic at line 8,500,050, anchor exists at 8,500,000
let parent = nearest_anchor_before(8_500_050);
scan_forward_n_lines(parent, 50);  // O(50 * avg_line_length)
create_anchor(..., 8_500_050, Relative(parent.id));
// Cost: O(5000 bytes) not O(8.5M lines)! ✅
```

3. **Lazy Refinement**
```rust
// When exact position discovered (e.g., viewport scroll from top):
let exact_line = scan_from_zero_to(byte);
if anchor.confidence == Estimated {
    let error = exact_line - anchor.estimated_line;
    refine_anchor_and_children(anchor, exact_line, error);
    anchor.confidence = Exact;
}
```

**Properties:**
- **Maximum scan**: Never scan more than max(100 lines, 10KB) between anchors
- **Sparse network**: ~50-200 anchors for 1GB file (viewport + diagnostics + search hits)
- **Self-correcting**: Anchors refine from Estimated→Exact as file is navigated
- **Local errors**: Wrong estimate at line 8.5M doesn't affect line 9.2M
- **Byte positions always exact**: Overlays/diagnostics appear correctly regardless of line number estimates

**When Estimation Matters:**
- Line number gutter display (acceptable to be slightly off until scrolled to)
- "Go to line N" command (refine on navigation)

**When Estimation Doesn't Matter:**
- Diagnostics (use byte positions for rendering)
- Hover/go-to-def (LSP returns byte positions)
- Overlays (anchored to bytes via markers)

**Fallback: Byte-Based LSPs**
- If LSP supports `PositionEncodingKind::Utf8`, skip line conversion entirely
- Work directly with byte offsets (no line numbers needed)

#### Performance Comparison

| Operation | Current (BTreeMap) | Proposed (Unified Tree) |
|-----------|-------------------|-------------------------|
| File load | O(1) - no cache | O(L log L) optional pre-scan OR O(1) lazy |
| Edit (no \n) | O(K log K) 😱 | O(log N) ✅ |
| Edit (with \n) | O(K log K) 😱 | O(log N + M) ✅ |
| Line→byte | O(log K) or O(M) scan | O(log N) or O(M) scan |
| Byte→line | O(log K) or O(M) scan | O(log N + k) query |
| LSP diagnostics | O(L) scan + O(D) converts | O(D log N) ✅ |

Where:
- N = total markers (positions + lines)
- L = total lines in file
- K = cached lines (can be 1000+)
- M = lines to scan (distance to nearest cached)
- D = new diagnostics to convert

**Current bottleneck**: `populate_line_cache` takes 61.95% of time (53B samples in flame graph)

#### Benefits

1. **Single Source of Truth**: ONE tree for ALL position tracking
2. **Efficient Edits**: O(log N) for everything, not O(K log K)
3. **Memory Efficiency**: Sparse cache, only accessed lines
4. **Code Simplification**: Remove `line_cache.rs`, `handle_insertion/deletion`
5. **Viewport Query Synergy**: Same `query_viewport` works for both overlays AND lines
6. **Huge File Support**: Lazy population scales to GB+ files

#### Implementation Plan

- [ ] **Phase 1**: Extend IntervalTree with `MarkerType` enum and line marker methods
- [ ] **Phase 2**: Add `line_to_byte` / `byte_to_line` to unified tree (parallel with old cache)
- [ ] **Phase 3**: Migrate `lsp_position_to_byte` to use new system
- [ ] **Phase 4**: Remove `LineCache` struct and eager update logic from Buffer
- [ ] **Phase 5**: Add lazy line marker rescanning for edits with newlines
- [ ] **Phase 6**: Implement viewport-based line population strategy
- [ ] **Phase 7**: Benchmark with large files (1GB+) and many diagnostics (10k+)

**Expected Performance Gain**:
- LSP diagnostic processing: 61.95% reduction (remove populate_line_cache bottleneck)
- Edit performance: 10-100x faster for files with large caches
- Memory: Proportional to accessed lines, not total lines

---

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

### Split View Behavioral Expectations

**Current Status**: Split view has basic functionality implemented but exhibits incorrect behaviors. This section documents the expected behaviors that must be validated and fixed.

#### Core Data Model
- Split system uses an Emacs-style tree structure (arbitrary nesting depth)
- Each split (leaf) displays exactly one buffer
- Multiple splits can display the same buffer simultaneously
- Active split determines which split receives keyboard input
- Active buffer determines which buffer is being edited

**Architectural Principle (Emacs-style):**
- **SHARED across splits**: Buffer content (text), undo history, overlays/diagnostics, file path
- **PER-SPLIT (independent)**: Cursors (positions + selections), viewport (scroll position), multi-cursor state

This means each split is essentially a "view" into the buffer with its own cursor and scroll position. Edits in one split are immediately visible in all splits showing that buffer, but cursor movements and scrolling are isolated to the active split.

#### Expected Behaviors

##### 1. Split Creation
- [ ] **Horizontal Split** (`split_horizontal`):
  - Creates a new split below the active split
  - New split displays the SAME buffer as the original (shared content)
  - Split ratio defaults to 50/50
  - Focus moves to the new (bottom) split
  - Both splits show the same buffer content
  - Status line shows buffer for the focused split

- [ ] **Vertical Split** (`split_vertical`):
  - Creates a new split to the right of the active split
  - New split displays the SAME buffer as the original (shared content)
  - Split ratio defaults to 50/50
  - Focus moves to the new (right) split
  - Both splits show the same buffer content
  - Status line shows buffer for the focused split

##### 2. Buffer Assignment (Shared Buffer Model)
- [ ] Multiple splits can show the SAME buffer simultaneously
- [ ] Typing in one split modifies the shared buffer (visible in all splits showing it)
- [ ] Opening a file in a split changes that split's buffer reference (not other splits)
- [ ] Each split has independent cursor, selection, AND scroll position for its buffer view
- [ ] Edits are synchronized across all splits viewing the same buffer
- [ ] ONLY buffer content is shared; all view state is per-split

##### 3. Cursor Management (Per-Split Cursors for Same Buffer)
- [ ] Each split maintains its OWN cursor set (Cursors struct), independent of other splits
- [ ] Multi-cursor operations are per-split (adding cursors in split A doesn't add them in split B)
- [ ] Cursor movements in one split do NOT affect cursor positions in other splits
- [ ] Primary cursor shows as hardware cursor ONLY in active split
- [ ] Secondary cursors (multi-cursor) render with REVERSED style
- [ ] When editing, cursor positions in OTHER splits viewing the same buffer adjust for insertions/deletions
- [ ] Selections (anchors) are per-split, not shared across splits

##### 4. Scrolling (Per-Split Viewport)
- [ ] Each split maintains independent scroll position (Viewport.top_byte)
- [ ] Scrolling in one split does NOT affect other splits (even showing same buffer)
- [ ] Page Up/Down affects ONLY the active split's viewport
- [ ] Horizontal scroll (left_column) is per-split, not per-buffer
- [ ] After split creation, new split's viewport starts at same position as parent
- [ ] Viewport automatically resizes when split dimensions change

##### 5. Focus Navigation
- [ ] `next_split` cycles through splits in order (circular navigation)
- [ ] `prev_split` cycles in reverse order
- [ ] Focus change updates both active_split and active_buffer
- [ ] Status bar reflects the focused split's buffer information
- [ ] Tab bar highlights the active buffer (if tabs shown)
- [ ] Clicking a split focuses it immediately

##### 6. Split Closing
- [ ] Closing a split removes it from the tree
- [ ] Parent split expands to fill the vacated space
- [ ] Cannot close the last remaining split (error message)
- [ ] Focus moves to a sibling split after closing
- [ ] Buffer associated with closed split may remain open (if shown elsewhere) or be closed
- [ ] Unsaved changes warning before closing split with modified buffer

##### 7. Split Resizing
- [ ] `increase_split_size` grows the active split by 5% (ratio adjustment)
- [ ] `decrease_split_size` shrinks the active split by 5%
- [ ] Ratio clamped between 0.1 and 0.9 (prevents invisible splits)
- [ ] Resizing adjusts the PARENT split container's ratio, not the leaf
- [ ] Content in both splits re-renders to fit new dimensions

##### 8. Visual Rendering
- [ ] Horizontal splits show separator line (`─` characters)
- [ ] Vertical splits show separator line (`│` characters)
- [ ] Each split renders its own line numbers (gutter)
- [ ] Each split has its own scrollbar
- [ ] Scrollbar color differs for active vs inactive splits
- [ ] Buffer content respects split boundaries (no overflow)
- [ ] Status bar shows information for the active split's buffer

##### 9. Text Editing Across Splits (Shared Buffer)
- [ ] Insert/delete in one split affects the shared buffer (visible in all splits showing it)
- [ ] Cursor positions in other splits adjust automatically for insertions/deletions
- [ ] Undo/redo operates on the buffer (affects all splits showing that buffer)
- [ ] Copy/paste uses single system clipboard (shared across splits)
- [ ] Find/replace operates on the active split's buffer view
- [ ] LSP features (completion, diagnostics) work in active split

##### 10. Edge Cases
- [ ] Splitting a split that already has minimal size (1-2 lines)
- [ ] Nested splits (3+ levels deep) maintain correct hierarchy
- [ ] Rapid split/close operations don't leak memory
- [ ] Focus tracking remains correct after complex split operations
- [ ] Resizing terminal window redistributes space proportionally

#### Implementation Plan

**Phase 1: Data Structure Changes**
```rust
// NEW: Per-split view state (independent of buffer)
pub struct SplitViewState {
    pub cursors: Cursors,        // Per-split cursor set (including multi-cursor)
    pub viewport: Viewport,      // Per-split scroll position
}

// MODIFIED: Editor struct
pub struct Editor {
    buffers: HashMap<BufferId, EditorState>,           // Shared buffer content
    split_view_states: HashMap<SplitId, SplitViewState>, // Per-split view state
    // ... rest unchanged
}

// MODIFIED: EditorState (remove view-specific state)
pub struct EditorState {
    pub buffer: Buffer,          // Shared content
    pub overlays: OverlayList,   // Shared overlays/diagnostics
    pub marker_list: MarkerList, // Shared markers
    // REMOVE: pub cursors: Cursors    (move to SplitViewState)
    // REMOVE: pub viewport: Viewport  (move to SplitViewState)
}
```

**Phase 2: Split Operations**
- [ ] `split_pane_horizontal/vertical`: Clone current split's `SplitViewState` for new split (same buffer, same cursor/scroll initially)
- [ ] `close_split`: Remove `SplitViewState` entry
- [ ] `next_split/prev_split`: Just update active split ID (view states already stored)

**Phase 3: Rendering Changes**
- [ ] `render_content`: Fetch `SplitViewState` for each split, not from buffer
- [ ] `render_buffer_in_split`: Use split's viewport/cursors, not buffer's
- [ ] Scrollbar: Use split's viewport for thumb position

**Phase 4: Event Handling**
- [ ] All cursor operations use `active_split`'s `SplitViewState`
- [ ] All scroll operations use `active_split`'s viewport
- [ ] Buffer edits: Apply to shared `EditorState`, then adjust cursors in ALL splits showing that buffer

**Phase 5: Cursor Adjustment on Edits**
- [ ] When buffer is edited, iterate all `SplitViewState` entries pointing to that buffer
- [ ] Adjust cursor positions for insertions (shift forward) and deletions (shift backward/clamp)
- [ ] This ensures cursors in other splits remain valid after edits

---

### Test Infrastructure
- [ ] **Lua Plugin Testing Infrastructure** - Need comprehensive testing infra/API/best practices for testing Lua scripts, preferably tests that could be written in the Lua environment itself. Currently, plugin tests require copying plugin files to test directories and setting up editor harnesses. Ideally, we'd have:
  - Unit testing framework for Lua plugins (similar to busted or luaunit)
  - Integration testing API that allows plugins to be tested in isolation
  - Mock/stub support for editor APIs (editor.spawn, editor.open_file, etc.)
  - Test helpers for common patterns (setting up test buffers, simulating user input)
  - Documentation and examples for plugin testing best practices
- [ ] Fix async file loading in test harness (6 tests ignored)
- [ ] Fix BIG.txt generation timing (2 scrolling tests fail)
- [ ] Support independent buffers per split (if desired)
- [ ] Add more E2E tests for complex workflows
- [ ] Performance regression tests

---

## Summary

### Current Status
**Strengths**: Multi-cursor editing, search & replace, auto-indent, LSP basics, large file support (1GB+), fully integrated Lua plugin system with hook-based prompt API, IntervalTree marker system, strong test coverage

**Recent Major Completions**:
- ✅ **Menu Bar System** - Full implementation with keyboard navigation (F10, arrows, Enter, Esc), mouse interaction (click menu, click item, click outside to close), Alt+letter mnemonics with underlined characters, keybinding display in dropdowns, JSON configuration, and 16 unit tests
- ✅ **Git Plugin Refactoring** - Converted git operations to pure Lua plugins, removed ~465 lines of Rust code
- ✅ **Hook-Based Prompt API** - Interactive UI for plugins via prompt-changed/confirmed/cancelled hooks
- ✅ **String-Based Function Mapping** - Commands call global Lua functions by name
- ✅ Search & Replace - Complete with interactive replace, history, search in selection
- ✅ Auto-indent - Tree-sitter based with hybrid heuristics
- ✅ Plugin System - Fully integrated with Lua runtime, hooks, and overlay management
- ✅ Marker System - O(log n) IntervalTree implementation with lazy delta propagation

**Critical Gaps**:
- Advanced LSP features (hover, code actions, find references)
- Bracket matching, snippets
- Terminal integration
- Virtual buffers & custom contexts (for Magit-style plugins)

**Next Steps**:
1. **High Priority**: LSP advanced features (hover, code actions, find references)
2. **Medium Priority**: Smart editing (bracket matching, toggle comment)
3. **Medium Priority**: Virtual buffers API (for advanced plugin UIs like Magit)

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

## Completed Work

## Recent Major Completions ✅

- **Menu Bar System** - Discoverable menus (File/Edit/View/Selection/Go/Help) with keyboard/mouse navigation, Alt+letter mnemonics, keybinding display, JSON config, and Lua plugin API
- **Git Plugin Refactoring** - Converted git-grep and git-find-file to pure Lua plugins (~465 lines of Rust removed)
- **Plugin Prompt API** - Hook-based interactive UI (prompt-changed/confirmed/cancelled)
- **LSP State Machine** - Full client lifecycle management with validated state transitions

## Completed Features ✅

### Core Functionality
- Event-driven architecture with unlimited undo/redo
- Multi-cursor editing
- Clipboard operations (copy/cut/paste)
- Position history navigation (Alt+Left/Right)
- Line wrapping
- Large file support (1GB+) with instant startup
- Advanced prompt editing (word deletion, copy/paste/cut in all input prompts)
- **Auto-indent** - Tree-sitter based, hybrid heuristic approach, supports all languages

### UI & Layout
- Split views (horizontal/vertical)
- Scrollbar, tab bar, status bar, line numbers
- Command palette (Ctrl+P), help system (Ctrl+H)
- File explorer (lazy loading, gitignore support, create/delete, unsaved indicators)
- Themes (dark/light/high-contrast)
- **Menu bar** - Discoverable menus with keybinding display and Alt+letter mnemonics

### LSP Integration
- Diagnostics (errors/warnings)
- Code completion (Ctrl+Space)
- Go-to-definition, rename refactoring (F2)
- Multi-language support, process resource limits

### Search & Replace
- ✅ **Streaming search** - Literal & regex, efficient on GB+ files with overlapping chunks
- ✅ **Replace operations** - replace_range(), replace_next(), replace_all(), replace_all_regex() with capture groups
- ✅ **Replace UI** (Ctrl+R) - Emacs-style two-step prompts with incremental highlighting
- ✅ **Interactive replace** (Ctrl+Alt+R) - Query-replace with y/n/!/q prompts, proper undo/redo
- ✅ **Search in selection** - Limit search to selected range
- ✅ **Search history** - Up/Down navigation, bash-like, 100 items per history
- Basic text search UI (F3/Shift+F3), wrap-around, highlighting, incremental search

### File Operations
- Open/save/close, multiple buffers, async I/O
- File explorer (create/delete files/dirs, show/hide hidden, respect gitignore, auto-expand on focus)

### Git Integration
- ✅ **Git grep** - Implemented as pure Lua plugin using hook-based prompt API
- ✅ **Git find file** - Implemented as pure Lua plugin with fuzzy matching
- Plugins accessible via command palette ("Git Grep", "Git Find File")

### Plugin System
- ✅ **Lua 5.4 runtime** - Fully integrated plugin manager, lifecycle management
- ✅ **Dynamic hooks** - 20+ hook types (render-line, after-save, prompt-changed, etc.)
- ✅ **String-based function mapping** - Commands call global Lua functions by name
- ✅ **Hook-based prompt API** - Interactive UI via prompt-changed/confirmed/cancelled hooks
- ✅ **File opening API** - `editor.open_file({path, line, column})`
- ✅ **Command registration** - Plugins can register custom commands
- ✅ **Async process spawning** - Non-blocking external commands
- ✅ **Buffer query API** - Metadata queries, streaming content access via render-line hook
- ✅ **Overlay lifecycle** - clear_all_overlays(), remove_overlays_by_prefix()
- ✅ **Full plugins** - git-grep, git-find-file, TODO highlighter (optimized for GB+ files)

### Performance & Optimization
- ✅ **Marker system (IntervalTree)** - O(log n) marker operations, lazy delta propagation for position tracking
- ✅ **ChunkTree optimization** - 4KB chunks → 38x speedup (file loading: 3.2s → 83ms)
- ✅ **Scroll optimization** - O(n) → O(viewport_height)
- ✅ **Buffer cache removal** - Eliminated expensive `buffer.to_string()` calls
- 400+ unit tests, E2E tests, property-based tests, visual regression testing
