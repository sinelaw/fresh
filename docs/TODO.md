# TODO

## Remaining Work

### Priority 0: Menu Bar System (IN PROGRESS)

#### Feature: Top-Level Menu Bar with Keybinding Integration

**Goal**: Add a discoverable menu bar above tabs that helps new users find features via standard conventions (File, Edit, View, etc.)

**Architecture Overview**:

```
UI Hierarchy (Updated):
Main Frame
├── Menu Bar (1 line) ← NEW
├── File Explorer + Tabs + Content
├── Suggestions
└── Status Bar
```

**Core Requirements**:
1. **Menu bar positioning**: Render at top of view, above tabs
2. **Action binding**: Menu items trigger existing Action enum values (no duplication)
3. **Keybinding display**: Show keyboard shortcuts next to menu items, pulled from keybinding config
4. **JSON configuration**: Menu structure (sections, items, order, separators) defined via JSON
5. **Plugin augmentation**: Runtime menu modification API for plugins
6. **Conventional defaults**: File/Edit/View/Selection/Go/Help menus following industry standards

**Data Structures**:

```rust
// config.rs - Menu configuration
pub struct MenuConfig {
    pub menus: Vec<Menu>,
}

pub struct Menu {
    pub label: String,
    pub items: Vec<MenuItem>,
}

pub enum MenuItem {
    Action {
        label: String,
        action: String,        // References Action enum (e.g., "save", "undo")
        args: Option<HashMap<String, serde_json::Value>>,
        when: Option<String>,  // Optional context filter
    },
    Separator,
    Submenu {
        label: String,
        items: Vec<MenuItem>,
    },
}
```

**Default Menu Structure**:

```
File                    Edit                    View                    Selection
├─ New File             ├─ Undo       Ctrl+Z    ├─ Toggle Explorer Ctrl+B   ├─ Select All      Ctrl+A
├─ Open File... Ctrl+O  ├─ Redo       Ctrl+Y    ├─ ─────────────            ├─ Select Word     Ctrl+D
├─ ──────────           ├─ ─────────────        ├─ Split Horizontal         ├─ Select Line     Ctrl+L
├─ Save         Ctrl+S  ├─ Cut        Ctrl+X    ├─ Split Vertical           ├─ ─────────────
├─ Save As...   Ctrl+Shift+S  ├─ Copy       Ctrl+C    ├─ Close Split              ├─ Add Cursor Above    Ctrl+Alt+Up
├─ ──────────           ├─ Paste      Ctrl+V    ├─ Focus Next Split Ctrl+K  ├─ Add Cursor Below    Ctrl+Alt+Down
├─ Close Buffer Ctrl+W  ├─ ─────────────        └─ ─────────────            ├─ Add Cursor at Match Ctrl+D
└─ Quit         Ctrl+Q  ├─ Find...    Ctrl+F                                └─ ─────────────
                        └─ Replace... Ctrl+H

Go                      Help
├─ Go to Definition F12 ├─ Show Help  F1
├─ Go to Line...  Ctrl+G├─ About
├─ Go to Symbol.. Ctrl+T
├─ ─────────────
├─ Next Error     F8
└─ Previous Error Shift+F8
```

**Component Architecture**:

1. **MenuRenderer** (`src/ui/menu.rs`):
   - Renders horizontal menu bar with labels
   - Handles dropdown rendering on activation
   - Queries keybinding system for shortcuts
   - Calculates layout for dropdown positioning

2. **Menu State** (in `Editor`):
   ```rust
   pub struct MenuState {
       active_menu: Option<usize>,      // Index of open menu
       highlighted_item: Option<usize>, // Index within dropdown
       plugin_menus: Vec<Menu>,         // Runtime additions from plugins
   }
   ```

3. **Keybinding Integration**:
   ```rust
   // Given an action, find the primary keybinding
   fn find_keybinding_for_action(
       &self,
       action: &str,
       context: KeyContext,
   ) -> Option<String> {
       // Returns formatted string like "Ctrl+S" or "F12"
   }
   ```

4. **Plugin API Extensions**:
   ```rust
   enum PluginCommand {
       // Existing commands...

       AddMenuItem {
           menu_label: String,        // Which menu ("File", "Edit", etc.)
           item: MenuItem,            // Item to add
           position: MenuPosition,    // Top, Bottom, Before(label), After(label)
       },
       AddMenu {
           menu: Menu,                // New top-level menu
           position: MenuPosition,    // Position in menu bar
       },
       RemoveMenuItem {
           menu_label: String,
           item_label: String,
       },
   }

   enum MenuPosition {
       Top,
       Bottom,
       Before(String),
       After(String),
   }
   ```

5. **Lua Plugin API**:
   ```lua
   -- Add custom menu item
   editor.add_menu_item("Tools", {
       label = "Run Tests",
       action = "run_custom_tests",  -- Calls global Lua function
   }, "bottom")

   -- Add entire menu
   editor.add_menu("Git", {
       { label = "Status", action = "git_status" },
       { label = "Commit...", action = "git_commit" },
       "separator",
       { label = "Push", action = "git_push" },
   }, "after:View")
   ```

**Interaction Model**:

1. **Keyboard Navigation**:
   - `Alt` or `F10`: Activate menu bar (highlight first menu)
   - `Left/Right`: Navigate between menus
   - `Up/Down`: Navigate within dropdown
   - `Enter`: Execute highlighted action
   - `Esc`: Close menu

2. **Mouse Interaction**:
   - Click menu label: Open dropdown
   - Click menu item: Execute action
   - Click outside: Close menu
   - Hover: Highlight item (visual feedback only)

3. **Keybinding Display Logic**:
   - For each menu item action, query `KeybindingResolver`
   - Find the shortest/primary keybinding for current context
   - Format as human-readable string (Ctrl+S, Alt+Enter, F12, etc.)
   - Right-align in dropdown for clean visual presentation

**Rendering Details**:

```rust
// Menu bar layout (1 line at top)
"File  Edit  View  Selection  Go  Help"

// Dropdown layout (overlay positioned below menu label)
┌──────────────────────────┐
│ Undo              Ctrl+Z │
│ Redo              Ctrl+Y │
│ ──────────────────────── │
│ Cut               Ctrl+X │
│ Copy              Ctrl+C │
│ Paste             Ctrl+V │
│ ──────────────────────── │
│ Find...           Ctrl+F │
│ Replace...        Ctrl+H │
└──────────────────────────┘
```

**Configuration File Format** (`~/.config/fresh/config.json`):

```json
{
  "menu": {
    "menus": [
      {
        "label": "File",
        "items": [
          { "label": "New File", "action": "new_file" },
          { "label": "Open File...", "action": "prompt_open_file" },
          "separator",
          { "label": "Save", "action": "save" },
          { "label": "Save As...", "action": "save_as" },
          "separator",
          { "label": "Close Buffer", "action": "close_buffer" },
          { "label": "Quit", "action": "quit" }
        ]
      },
      {
        "label": "Edit",
        "items": [
          { "label": "Undo", "action": "undo" },
          { "label": "Redo", "action": "redo" },
          "separator",
          { "label": "Cut", "action": "cut" },
          { "label": "Copy", "action": "copy" },
          { "label": "Paste", "action": "paste" }
        ]
      }
    ]
  }
}
```

**Implementation Tasks**:

- [ ] **Phase 1: Core Data Structures**
  - [ ] Add `MenuConfig`, `Menu`, `MenuItem` to `config.rs`
  - [ ] Add `MenuState` to `Editor` struct
  - [ ] Implement JSON serialization/deserialization
  - [ ] Create default menu configuration

- [ ] **Phase 2: UI Rendering**
  - [ ] Create `src/ui/menu.rs` with `MenuRenderer`
  - [ ] Implement menu bar rendering (horizontal labels)
  - [ ] Implement dropdown rendering (positioned overlay)
  - [ ] Update `Editor::render()` to include menu bar above tabs
  - [ ] Add theme colors for menu (menu_bar, menu_dropdown, menu_highlight, etc.)

- [ ] **Phase 3: Keybinding Integration**
  - [ ] Add `find_keybinding_for_action()` to `KeybindingResolver`
  - [ ] Implement reverse lookup (Action → Keybinding)
  - [ ] Format keybindings for display (Ctrl+S, Alt+Enter, etc.)
  - [ ] Display keybindings in dropdown (right-aligned)

- [ ] **Phase 4: Interaction Handlers**
  - [ ] Add keyboard handlers (Alt, F10, arrows, Enter, Esc)
  - [ ] Add mouse handlers (click menu, click item, click outside)
  - [ ] Implement menu state transitions (open/close, navigate)
  - [ ] Execute actions on selection

- [ ] **Phase 5: Plugin API**
  - [ ] Add `PluginCommand::AddMenuItem` and `AddMenu`
  - [ ] Implement runtime menu modification in `Editor`
  - [ ] Add Lua bindings (`editor.add_menu_item()`, `editor.add_menu()`)
  - [ ] Document plugin menu API in `docs/PLUGINS.md`

- [ ] **Phase 6: Testing & Polish**
  - [ ] Unit tests for menu configuration parsing
  - [ ] E2E tests for keyboard/mouse navigation
  - [ ] Test plugin menu augmentation
  - [ ] Accessibility: ensure keyboard-only navigation works perfectly
  - [ ] Performance: test with many menus/items

**Benefits**:
- **Discoverability**: New users can explore features via menus
- **Convention**: Familiar File/Edit/View structure from other editors
- **No duplication**: Reuses existing Action enum and keybinding config
- **Extensibility**: Plugins can add custom menus for domain-specific features
- **Accessibility**: Keyboard-only navigation for power users

**Migration Path**:
- Menu system is additive (no breaking changes)
- All existing keybindings continue to work
- Menu is optional UI chrome (can be toggled off if desired)
- Plugins can gradually adopt menu API

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

- [ ] **Client State Machine** (`lsp_async.rs:LspHandle`)
    - Replace `bool` with `enum LspClientState { Initial, Starting, Initializing, Running, Stopping, Stopped, Error }`
    - Prevent invalid transitions (e.g., can't initialize twice, can't send requests when Stopped)
    - Better status reporting to UI ("Initializing..." vs "Running" vs "Error")
    - **Impact:** Prevents bugs, better UX, clearer debugging
    - **Effort:** Low (2-3 hours)

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

- [ ] **Deferred Document Open** (`lsp_async.rs:LspHandle`)
    - Don't send `didOpen` for non-visible documents immediately
    - Queue pending opens, send when document becomes visible
    - Faster startup for projects with many files
    - **Impact:** Medium - improves startup performance
    - **Effort:** Low (2-3 hours)

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
- [x] Command queueing before initialization
- [x] Progress notifications (`$/progress`)
- [x] Window messages (`window/showMessage`, `window/logMessage`)
- [x] Server status tracking
- [x] UTF-16 position encoding with line cache

#### Deferred (Lower Priority)

- **Semantic Tokens** - Advanced highlighting (nice-to-have)
- **Inlay Hints** - Type annotations (nice-to-have)
- **Call/Type Hierarchy** - Advanced navigation (nice-to-have)
- **Log Viewer Panel** - UI polish (can use external tools)

---

**Next Steps:** Start with Phase 1 (robustness). These are quick wins with high impact that make the LSP client production-ready.

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

- **Git Plugin Refactoring** - Removed ~465 lines of hardcoded Rust git code (src/git.rs, Action::GitGrep, Action::GitFindFile, etc.) and replaced with pure Lua plugins using hook-based prompt API and string-based function mapping
- **Plugin Prompt API** - Hook-based design (prompt-changed, prompt-confirmed, prompt-cancelled) for interactive UI in plugins
- **String-Based Function Mapping** - Commands call global Lua functions by name (e.g., `action = "start_git_grep"`)
- **File Opening API** - `editor.open_file({path, line, column})` for precise navigation

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
