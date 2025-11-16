# TODO

## Remaining Work

### Priority 0: Menu Bar System (COMPLETE ✅)

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

- [ ] **Request Cancellation** (`lsp_async.rs`)
    - Add `CancellationToken` to completion/hover requests
    - Cancel stale completions when user types more characters
    - Send `$/cancelRequest` notification to server
    - Reduces server load, prevents outdated UI results
    - **Impact:** High - better UX and performance
    - **Effort:** Medium (4-6 hours)

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

- [ ] **Diagnostics Panel** (new file: `diagnostics_panel.rs`)
    - List view of all diagnostics in current file/workspace
    - Filter by severity (errors, warnings, hints)
    - Jump to diagnostic location on click
    - **Impact:** Medium - better error overview
    - **Effort:** Medium (6-8 hours)

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

#### Deferred (Lower Priority)

- **Semantic Tokens** - Advanced highlighting (nice-to-have)
- **Inlay Hints** - Type annotations (nice-to-have)
- **Call/Type Hierarchy** - Advanced navigation (nice-to-have)
- **Log Viewer Panel** - UI polish (can use external tools)

---

**Next Steps:** Phase 1 is mostly complete (state machine ✅, deferred opens ✅). Focus on remaining P0 items (auto-restart, request cancellation) then move to Phase 3 user-facing features (hover, code actions, find references).

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
