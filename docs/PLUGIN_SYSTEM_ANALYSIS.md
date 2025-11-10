# Plugin System Analysis: Emacs-Style Architecture

**Date:** 2025-11-06
**Status:** Design Analysis
**Goal:** Enable powerful plugins capable of implementing Magit-like features, non-LSP major modes, and complex editor extensions

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Emacs Plugin System Overview](#emacs-plugin-system-overview)
3. [Current Architecture Assessment](#current-architecture-assessment)
4. [Identified Gaps](#identified-gaps)
5. [Scripting Language Options](#scripting-language-options)
6. [Required Infrastructure](#required-infrastructure)
7. [Implementation Challenges](#implementation-challenges)
8. [Proposed Architecture](#proposed-architecture)
9. [Example Use Cases](#example-use-cases)
10. [Roadmap](#roadmap)

---

## Executive Summary

The editor has an **excellent foundation** for a plugin system with event-driven architecture, overlay/popup primitives, and async message passing. However, to achieve Emacs-level extensibility, we need:

### Critical Gaps
1. **No scripting runtime** - Currently Rust-only, no dynamic loading
2. **Limited event hooks** - Events exist but no plugin subscription mechanism
3. **No plugin API surface** - Core primitives (overlays, popups) not exposed
4. **Missing buffer/window introspection** - Plugins can't query editor state
5. **No command definition API** - Can't register custom commands dynamically

### Recommended Approach
**Phase 1:** Lua scripting with FFI bindings (similar to Neovim)
**Phase 2:** WASM plugins for sandboxing and multi-language support
**Phase 3:** Native Rust plugins with dynamic loading

### Strengths to Build On
- Event log architecture (perfect for undo/redo in plugins)
- Overlay/popup system (Emacs-style visual primitives)
- AsyncBridge pattern (plugins can spawn async tasks)
- Command palette infrastructure (easy to extend)
- Context-aware keybindings (already supports "when" clauses)

---

## Emacs Plugin System Overview

### Core Emacs Plugin Capabilities

#### 1. **Buffer/Window/Frame Manipulation**
```elisp
;; Create buffers, split windows, manage frames
(with-current-buffer (get-buffer-create "*magit*")
  (erase-buffer)
  (insert "Status: ")
  (pop-to-buffer (current-buffer)))
```

**Editor Equivalent Needs:**
- `create_buffer(name)` → BufferId
- `get_active_buffer()` → BufferId
- `switch_to_buffer(buffer_id)` → void
- `split_window(direction, ratio)` → SplitId
- `buffer_insert(buffer_id, position, text)` → void
- `buffer_erase(buffer_id)` → void

#### 2. **Overlays (Text Properties)**
```elisp
;; Add visual decorations
(let ((overlay (make-overlay start end)))
  (overlay-put overlay 'face '(:foreground "red" :underline t))
  (overlay-put overlay 'help-echo "Error: undefined variable"))
```

**Editor Status:** ✅ **Already exists!** (`Event::AddOverlay`)
**Missing:** API to create/manage from plugins

#### 3. **Interactive Commands**
```elisp
;; Define commands with prompts
(defun my-search-project (query)
  (interactive "sSearch: ")
  (async-shell-command (concat "rg " query)))
```

**Editor Equivalent Needs:**
- `register_command(name, callback, contexts)` → void
- `prompt_user(type, label, callback)` → void (async)
- `register_keybinding(key, mods, action, context)` → void

#### 4. **Hooks (Lifecycle Events)**
```elisp
;; Subscribe to editor events
(add-hook 'after-save-hook 'my-format-on-save)
(add-hook 'before-change-functions 'my-syntax-check)
```

**Editor Status:** ❌ **Missing entirely**
**Needs:** Hook registry system

#### 5. **Async Processes**
```elisp
;; Run external commands asynchronously
(make-process
  :name "git-status"
  :command '("git" "status" "--porcelain")
  :filter 'my-process-output)
```

**Editor Status:** 🟡 **Partial** (AsyncBridge exists, not exposed)
**Needs:** Plugin process management API

#### 6. **Keymaps (Modal Bindings)**
```elisp
;; Context-specific keybindings
(defvar magit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'magit-stage)
    (define-key map (kbd "c") 'magit-commit)
    map))
```

**Editor Status:** 🟡 **Partial** (KeyContext exists)
**Needs:** Dynamic context creation for plugins

---

## Current Architecture Assessment

### Strengths ✅

#### 1. **Event-Driven Core** (src/event.rs)
```rust
pub enum Event {
    Insert { position, text, cursor_id },
    Delete { range, deleted_text, cursor_id },
    MoveCursor { cursor_id, position, anchor },
    AddOverlay { overlay_id, range, face, priority, message },
    ShowPopup { popup },
    // ... 20+ event types
}
```

**Why This Is Great:**
- All state changes are serializable and replayable
- Perfect foundation for plugin event subscription
- Already supports undo/redo (plugins get this for free)
- Can stream events to plugins for reactive programming

**What's Missing:**
- No event subscription API
- Events are applied directly, not broadcast to listeners

#### 2. **Overlay System** (Event::AddOverlay)
```rust
AddOverlay {
    overlay_id: String,
    range: Range<usize>,
    face: OverlayFace,  // Underline/Background/Foreground
    priority: i32,
    message: Option<String>,
}
```

**Why This Is Great:**
- Emacs-style text properties
- Z-ordering with priority
- Can represent diagnostics, selections, search results, etc.

**What's Missing:**
- No query API (get overlays in range)
- No overlay-specific hooks (on-hover, on-click)

#### 3. **Popup System** (Event::ShowPopup)
```rust
pub struct PopupData {
    pub title: Option<String>,
    pub content: PopupContentData,  // Text, List, Custom
    pub position: PopupPositionData, // AtCursor, Centered, Fixed
    pub width: u16,
    pub max_height: u16,
}
```

**Why This Is Great:**
- Already supports custom content
- Position strategies (perfect for tooltips, menus)
- List popups with navigation (completion, file picker)

**What's Missing:**
- Custom rendering callbacks
- Event handling per popup (key dispatch)

#### 4. **AsyncBridge Pattern** (src/async_bridge.rs)
```rust
pub enum AsyncMessage {
    LspDiagnostics { uri, diagnostics },
    LspCompletion { request_id, items },
    GitGrepResults { query, results },
    // Extensible!
}
```

**Why This Is Great:**
- Clean separation of sync (UI) and async (I/O)
- Non-blocking message passing
- Can easily add plugin-specific messages

**What's Missing:**
- No plugin registration for custom message types
- No dynamic message routing

#### 5. **Command Palette** (src/commands.rs)
```rust
pub struct Command {
    pub name: String,
    pub description: String,
    pub action: Action,
    pub contexts: Vec<KeyContext>,
}

pub fn get_all_commands() -> Vec<Command>  // Hardcoded list
```

**Why This Is Great:**
- Already has fuzzy search
- Context-aware filtering
- Clean command abstraction

**What's Missing:**
- Dynamic command registration
- Currently returns hardcoded Vec, not registry

#### 6. **Context System** (src/keybindings.rs)
```rust
pub enum KeyContext {
    Normal,
    Help,
    Prompt,
    Popup,
    FileExplorer,
}
```

**Why This Is Great:**
- Modal keybinding support
- Extensible to plugin-specific modes (e.g., MagitMode)

**What's Missing:**
- Hardcoded enum (can't add dynamic contexts)
- No context stack (Emacs has major + minor modes)

### Weaknesses ❌

#### 1. **No Scripting Runtime**
- Currently Rust-only
- No dynamic code loading
- Plugins would require recompilation

**Impact:** Cannot implement dynamic plugins without this

#### 2. **No Hook System**
```rust
// Desired API:
editor.register_hook("before-save", |buffer_id| {
    format_buffer(buffer_id);
});
```

**Current Reality:** No subscription mechanism exists

#### 3. **No Buffer Introspection API**
```rust
// Desired API:
let content = editor.get_buffer_content(buffer_id, range)?;
let cursor_pos = editor.get_cursor_position(buffer_id)?;
let file_path = editor.get_buffer_file_path(buffer_id)?;
```

**Current Reality:** Editor struct is opaque, no query methods

#### 4. **Hardcoded Action Enum**
```rust
pub enum Action {
    InsertChar(char),
    MoveLeft,
    Save,
    // ... 70+ variants
}
```

**Problem:** Can't add custom actions without modifying core

#### 5. **No Plugin Lifecycle Management**
- No plugin discovery/loading
- No dependency resolution
- No enable/disable mechanism
- No plugin configuration

---

## Identified Gaps

### Gap 1: Scripting Runtime (CRITICAL)

**What's Missing:**
- Embedded language interpreter (Lua/Rhai/WASM)
- FFI bindings to Rust core
- Sandboxing/resource limits

**Why Critical:**
Without this, plugins require recompiling the entire editor.

**Options:**
1. **Lua** (mlua crate) - Proven (Neovim), fast, lightweight
2. **Rhai** - Pure Rust, easier FFI, smaller ecosystem
3. **WASM** - Multi-language, sandboxed, overhead for FFI calls

### Gap 2: Plugin API Surface (CRITICAL)

**What's Missing:**
- Public API module (`src/plugin_api.rs`)
- Trait-based plugin interface
- Version compatibility guarantees

**Example Needed API:**
```rust
pub trait Plugin {
    fn name(&self) -> &str;
    fn version(&self) -> &str;
    fn on_load(&mut self, ctx: &mut PluginContext);
    fn on_unload(&mut self, ctx: &mut PluginContext);
}

pub struct PluginContext<'a> {
    editor: &'a mut Editor,
}

impl<'a> PluginContext<'a> {
    // Buffer API
    pub fn create_buffer(&mut self, name: &str) -> BufferId;
    pub fn get_buffer_content(&self, id: BufferId) -> String;
    pub fn insert_text(&mut self, id: BufferId, pos: usize, text: &str);

    // Event API
    pub fn subscribe(&mut self, event_type: EventType, callback: Box<dyn Fn(&Event)>);
    pub fn emit_event(&mut self, event: Event);

    // Command API
    pub fn register_command(&mut self, cmd: Command);
    pub fn execute_action(&mut self, action: Action);

    // UI API
    pub fn add_overlay(&mut self, overlay: Overlay);
    pub fn show_popup(&mut self, popup: Popup);

    // Async API
    pub fn spawn_task<F>(&self, task: F) where F: Future<Output = AsyncMessage>;
}
```

### Gap 3: Hook/Event System (HIGH PRIORITY)

**What's Missing:**
- Event subscription registry
- Hook invocation points in core editor loop
- Before/after hooks for operations

**Desired Hooks:**
```rust
// File lifecycle
"before-file-open"     → (path: &Path) -> bool  // can cancel
"after-file-open"      → (buffer_id: BufferId)
"before-file-save"     → (buffer_id: BufferId) -> bool
"after-file-save"      → (buffer_id: BufferId)
"file-closed"          → (buffer_id: BufferId)

// Buffer changes
"before-insert"        → (buffer_id, position, text) -> bool
"after-insert"         → (buffer_id, position, text)
"before-delete"        → (buffer_id, range) -> bool
"after-delete"         → (buffer_id, range)

// Cursor movement
"cursor-moved"         → (buffer_id, old_pos, new_pos)

// View events
"buffer-activated"     → (buffer_id)
"buffer-deactivated"   → (buffer_id)
"split-created"        → (split_id)

// Editor lifecycle
"editor-initialized"   → ()
"pre-command"          → (action: &Action)
"post-command"         → (action: &Action)
"idle"                 → ()  // No input for N ms
```

**Implementation Approach:**
```rust
pub struct HookRegistry {
    hooks: HashMap<String, Vec<Box<dyn Fn(&HookArgs) -> bool>>>,
}

impl HookRegistry {
    pub fn add_hook(&mut self, name: &str, callback: Box<dyn Fn(&HookArgs) -> bool>) {
        self.hooks.entry(name.to_string())
            .or_insert_with(Vec::new)
            .push(callback);
    }

    pub fn run_hooks(&self, name: &str, args: &HookArgs) -> bool {
        if let Some(hooks) = self.hooks.get(name) {
            for hook in hooks {
                if !hook(args) {
                    return false;  // Hook cancelled operation
                }
            }
        }
        true
    }
}
```

### Gap 4: Dynamic Command Registration ✅ (IMPLEMENTED)

**Status:** Fully implemented in Phase 1

**Implementation:**
```rust
// src/command_registry.rs
pub struct CommandRegistry {
    commands: Vec<Command>,
}

impl CommandRegistry {
    pub fn register(&mut self, command: Command);
    pub fn unregister(&mut self, name: &str);
    pub fn unregister_by_prefix(&mut self, prefix: &str);  // For plugin cleanup
    pub fn get_all(&self) -> &[Command];
}

// In Lua plugins:
editor.register_command({
    name = "My Command",
    description = "Does something",
    action = "my_action",
    contexts = {"normal"},
    callback = function()
        -- Custom Lua code
    end
})
```

### Gap 5: Custom Action Handling (MEDIUM PRIORITY)

**Current Problem:**
```rust
// src/keybindings.rs - closed enum
pub enum Action {
    InsertChar(char),
    MoveLeft,
    // ... can't add plugin actions
}
```

**Solution 1: Add Custom Variant**
```rust
pub enum Action {
    // ... existing variants
    Custom(String),  // Plugin-defined action names
}
```

**Solution 2: Trait-Based Dispatch**
```rust
pub trait ActionHandler {
    fn handle(&mut self, editor: &mut Editor) -> io::Result<()>;
}

pub struct ActionRegistry {
    handlers: HashMap<String, Box<dyn ActionHandler>>,
}
```

### Gap 6: Buffer Query API ✅ (IMPLEMENTED - Nov 2025)

**Status:** Implemented with architectural constraints

**Available APIs:**
```rust
// Query metadata (implemented)
editor.get_buffer_info(buffer_id) -> BufferInfo  // path, modified, length
editor.get_active_buffer_id() -> BufferId
editor.list_buffers() -> Vec<BufferInfo>
editor.get_primary_cursor() -> CursorInfo
editor.get_all_cursors() -> Vec<CursorInfo>
editor.get_viewport() -> ViewportInfo

// Access content via hooks (recommended approach)
editor.on("render-line", function(args)
    // args: buffer_id, line_number, byte_start, byte_end, content
end)
```

**Intentionally NOT Implemented:**
- `get_buffer_content()` - Would materialize entire buffer, killing performance on GB+ files
- Plugins should use `render-line` hook for line-by-line access instead
- For file-level operations, use `editor.spawn()` with external tools

### Gap 7: Plugin Configuration (LOW PRIORITY)

**What's Missing:**
- Per-plugin config section
- Plugin settings UI
- Config reload notifications

**Desired Config:**
```json
{
  "plugins": {
    "magit": {
      "enabled": true,
      "keybindings": {
        "status": "Ctrl-x g"
      }
    },
    "git-gutter": {
      "enabled": true,
      "sign_column": true,
      "update_interval_ms": 500
    }
  }
}
```

### Gap 8: Context Stack (LOW PRIORITY)

**Current:** Single active context (KeyContext enum)
**Emacs:** Major mode + multiple minor modes

**Desired:**
```rust
pub struct ContextStack {
    major: String,           // "rust-mode", "magit-mode"
    minor: Vec<String>,      // ["line-numbers", "git-gutter"]
}

// Keybinding resolution:
// 1. Check minor modes (top to bottom)
// 2. Check major mode
// 3. Fall back to global
```

---

## Scripting Language Options

### Option 1: Lua (via mlua crate) ⭐ RECOMMENDED

**Pros:**
- ✅ Proven in Neovim (massive plugin ecosystem)
- ✅ Fast JIT compilation (LuaJIT)
- ✅ Small memory footprint (~200KB)
- ✅ Easy FFI with mlua crate
- ✅ Synchronous model (matches editor's event loop)
- ✅ Good error messages and debugging

**Cons:**
- ❌ 1-indexed arrays (different from Rust)
- ❌ Dynamic typing (runtime errors)
- ❌ Requires learning Lua

**Example Plugin:**
```lua
-- ~/.config/editor/plugins/magit.lua

local M = {}

function M.setup(editor)
  -- Register command
  editor:register_command({
    name = "Magit Status",
    action = function()
      M.show_status(editor)
    end,
    keybinding = { key = "g", mods = {"ctrl"}, context = "normal" }
  })

  -- Subscribe to hooks
  editor:on("after-file-save", function(buffer_id)
    M.refresh_git_status(editor, buffer_id)
  end)
end

function M.show_status(editor)
  -- Create buffer
  local buf = editor:create_buffer("*magit-status*")
  editor:switch_to_buffer(buf)

  -- Run git status async
  editor:spawn_async(function()
    local status = run_command("git", {"status", "--porcelain"})
    editor:insert_text(buf, 0, status)

    -- Add overlays for modified files
    for line in status:gmatch("[^\n]+") do
      if line:match("^M") then
        local range = editor:line_to_range(buf, line_num)
        editor:add_overlay({
          range = range,
          face = { foreground = {255, 165, 0} },  -- Orange
          priority = 10
        })
      end
    end
  end)
end

return M
```

**Integration Code (Rust):**
```rust
use mlua::{Lua, UserData, UserDataMethods};

struct EditorApi {
    editor: Arc<Mutex<Editor>>,
}

impl UserData for EditorApi {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method("create_buffer", |_, this, name: String| {
            let mut editor = this.editor.lock().unwrap();
            let id = editor.create_buffer(&name);
            Ok(id.0)  // Return buffer ID as number
        });

        methods.add_method("insert_text", |_, this, (buf_id, pos, text): (usize, usize, String)| {
            let mut editor = this.editor.lock().unwrap();
            editor.insert_text(BufferId(buf_id), pos, &text)?;
            Ok(())
        });

        // ... more methods
    }
}

// Plugin loading
fn load_lua_plugin(lua: &Lua, path: &Path, editor: Arc<Mutex<Editor>>) -> mlua::Result<()> {
    let api = EditorApi { editor };
    lua.globals().set("editor", api)?;
    lua.load(path).exec()?;
    Ok(())
}
```

### Option 2: Rhai (Pure Rust Scripting)

**Pros:**
- ✅ Pure Rust (no C dependencies)
- ✅ Rust-like syntax (familiar to editor developers)
- ✅ Easy integration with serde
- ✅ Built-in sandboxing

**Cons:**
- ❌ Smaller ecosystem (fewer examples)
- ❌ No JIT (slower than LuaJIT)
- ❌ Less proven for editor plugins

**Example Plugin:**
```rhai
// magit.rhai

fn show_status() {
    let buf = editor.create_buffer("*magit-status*");
    editor.switch_to_buffer(buf);

    let status = run_command("git", ["status", "--porcelain"]);
    editor.insert_text(buf, 0, status);
}

editor.register_command(#{
    name: "Magit Status",
    action: show_status,
    keybinding: #{ key: "g", mods: ["ctrl"] }
});
```

### Option 3: WASM (Multi-Language)

**Pros:**
- ✅ Multi-language support (Rust, AssemblyScript, Go, etc.)
- ✅ Sandboxed (security/stability)
- ✅ Near-native performance
- ✅ Can compile existing libraries

**Cons:**
- ❌ Complex FFI (serialize across boundary)
- ❌ Larger plugins (~100KB+ per plugin)
- ❌ Async model mismatch (WASM is blocking)
- ❌ More complex tooling (compilation step)

**Example (AssemblyScript):**
```typescript
// magit.ts (compiled to WASM)

import { editor } from "./editor-api";

export function showStatus(): void {
  const buf = editor.createBuffer("*magit-status*");
  editor.switchToBuffer(buf);

  const status = editor.runCommand("git", ["status", "--porcelain"]);
  editor.insertText(buf, 0, status);
}

export function onLoad(): void {
  editor.registerCommand({
    name: "Magit Status",
    action: showStatus,
    keybinding: { key: "g", mods: ["ctrl"] }
  });
}
```

### Option 4: Native Rust Plugins (Dynamic Loading)

**Pros:**
- ✅ Full Rust performance
- ✅ Type safety
- ✅ Can use any crate

**Cons:**
- ❌ Requires compilation per OS/arch
- ❌ ABI instability (requires exact Rust version match)
- ❌ Unsafe code (dynamic loading is unsafe)
- ❌ Hard to sandbox

**Example:**
```rust
// magit/src/lib.rs

use editor_plugin_api::*;

#[no_mangle]
pub extern "C" fn plugin_init(ctx: &mut PluginContext) {
    ctx.register_command(Command {
        name: "Magit Status",
        description: "Open Magit status buffer",
        action: Action::Custom("magit-status".into()),
        contexts: vec![KeyContext::Normal],
    });

    ctx.subscribe(EventType::AfterSave, Box::new(|event| {
        // Refresh git status
        true
    }));
}
```

### Recommendation: Hybrid Approach

**Phase 1: Lua** (Quick wins, proven model)
- Implement Lua runtime with core API
- Port simple plugins (git-gutter, line numbers, themes)
- Learn from Neovim ecosystem

**Phase 2: WASM** (Advanced features, multi-language)
- Add WASM runtime alongside Lua
- Enable CPU-intensive plugins (syntax highlighting, formatters)
- Allow users to write in Rust/TypeScript/Go

**Phase 3: Native Plugins** (Performance-critical)
- Optional for plugins that need raw performance
- Require signing/sandboxing
- Use for LSP servers, tree-sitter parsers

---

## Required Infrastructure

### Infrastructure 1: Plugin Manager (CRITICAL)

**Components:**
```rust
// src/plugin_manager.rs

pub struct PluginManager {
    plugins: HashMap<String, Box<dyn Plugin>>,
    hooks: HookRegistry,
    commands: CommandRegistry,
    lua: Option<Lua>,
}

impl PluginManager {
    pub fn load_plugin(&mut self, path: &Path) -> Result<()> {
        // Detect plugin type (Lua, WASM, native)
        // Load and initialize
        // Register hooks/commands
    }

    pub fn unload_plugin(&mut self, name: &str) -> Result<()> {
        // Call plugin.on_unload()
        // Remove hooks/commands
        // Drop plugin instance
    }

    pub fn reload_plugin(&mut self, name: &str) -> Result<()> {
        self.unload_plugin(name)?;
        // Find plugin path
        self.load_plugin(path)
    }

    pub fn emit_hook(&self, name: &str, args: &HookArgs) -> bool {
        self.hooks.run_hooks(name, args)
    }
}
```

**Plugin Discovery:**
```
~/.config/editor/plugins/
├── magit/
│   ├── plugin.toml          # Metadata
│   └── init.lua             # Entry point
├── git-gutter/
│   ├── plugin.toml
│   └── init.lua
└── tree-sitter-custom/
    ├── plugin.toml
    └── plugin.wasm
```

**plugin.toml:**
```toml
[plugin]
name = "magit"
version = "0.1.0"
description = "Git interface inspired by Emacs Magit"
author = "Your Name"
entry = "init.lua"
type = "lua"

[dependencies]
editor_version = ">=0.1.0"
plugins = []

[keybindings]
"Ctrl-x g" = "magit-status"

[hooks]
after_save = true
```

### Infrastructure 2: Hook Registry (CRITICAL)

**Implementation:**
```rust
// src/hooks.rs

pub enum HookArgs {
    BeforeFileOpen { path: PathBuf },
    AfterFileOpen { buffer_id: BufferId },
    BeforeInsert { buffer_id: BufferId, position: usize, text: String },
    AfterInsert { buffer_id: BufferId, position: usize, text: String },
    CursorMoved { buffer_id: BufferId, old_pos: usize, new_pos: usize },
    // ... all hook types
}

type HookCallback = Box<dyn Fn(&HookArgs) -> bool + Send>;

pub struct HookRegistry {
    hooks: HashMap<String, Vec<HookCallback>>,
}

impl HookRegistry {
    pub fn add_hook(&mut self, name: &str, callback: HookCallback) {
        self.hooks.entry(name.to_string())
            .or_insert_with(Vec::new)
            .push(callback);
    }

    pub fn remove_hooks(&mut self, name: &str) {
        self.hooks.remove(name);
    }

    pub fn run_hooks(&self, name: &str, args: &HookArgs) -> bool {
        if let Some(hooks) = self.hooks.get(name) {
            for hook in hooks {
                if !hook(args) {
                    return false;  // Hook cancelled operation
                }
            }
        }
        true
    }
}
```

**Integration into Event Loop:**
```rust
// In src/editor.rs - handle_key()

pub fn handle_key(&mut self, key: KeyCode, modifiers: KeyModifiers) -> io::Result<()> {
    let action = self.keybindings.resolve(key, modifiers, self.key_context);

    // PRE-COMMAND HOOK
    if !self.plugin_manager.emit_hook("pre-command", &HookArgs::PreCommand { action: &action }) {
        return Ok(());  // Hook cancelled
    }

    self.handle_action(action)?;

    // POST-COMMAND HOOK
    self.plugin_manager.emit_hook("post-command", &HookArgs::PostCommand { action: &action });

    Ok(())
}
```

### Infrastructure 3: Plugin API Module (CRITICAL)

**Public API Design:**
```rust
// src/plugin_api.rs

pub struct PluginContext<'a> {
    editor: &'a mut Editor,
    plugin_name: String,
}

impl<'a> PluginContext<'a> {
    // === BUFFER API ===

    pub fn create_buffer(&mut self, name: &str) -> BufferId {
        self.editor.create_buffer(name)
    }

    pub fn get_buffer_content(&self, id: BufferId, range: Range<usize>) -> Option<String> {
        self.editor.get_buffer_content(id, range)
    }

    pub fn insert_text(&mut self, id: BufferId, position: usize, text: &str) -> Result<()> {
        let events = vec![Event::Insert {
            position,
            text: text.to_string(),
            cursor_id: CursorId(0),
        }];
        self.editor.apply_events(id, events)
    }

    pub fn delete_range(&mut self, id: BufferId, range: Range<usize>) -> Result<()> {
        let deleted_text = self.get_buffer_content(id, range.clone())?;
        let event = Event::Delete {
            range,
            deleted_text,
            cursor_id: CursorId(0),
        };
        self.editor.apply_events(id, vec![event])
    }

    pub fn get_cursor_position(&self, id: BufferId) -> Option<usize> {
        self.editor.get_cursor_position(id)
    }

    pub fn move_cursor(&mut self, id: BufferId, position: usize) -> Result<()> {
        let event = Event::MoveCursor {
            cursor_id: CursorId(0),
            position,
            anchor: None,
        };
        self.editor.apply_events(id, vec![event])
    }

    // === OVERLAY API ===

    pub fn add_overlay(&mut self, id: BufferId, overlay: Overlay) -> Result<()> {
        let event = Event::AddOverlay {
            overlay_id: overlay.id,
            range: overlay.range,
            face: overlay.face,
            priority: overlay.priority,
            message: overlay.message,
        };
        self.editor.apply_events(id, vec![event])
    }

    pub fn remove_overlay(&mut self, id: BufferId, overlay_id: &str) -> Result<()> {
        let event = Event::RemoveOverlay {
            overlay_id: overlay_id.to_string(),
        };
        self.editor.apply_events(id, vec![event])
    }

    pub fn get_overlays_in_range(&self, id: BufferId, range: Range<usize>) -> Vec<Overlay> {
        self.editor.get_overlays_in_range(id, range)
    }

    // === POPUP API ===

    pub fn show_popup(&mut self, popup: Popup) -> Result<()> {
        let event = Event::ShowPopup { popup: popup.into() };
        self.editor.apply_global_event(event)
    }

    pub fn hide_popup(&mut self) -> Result<()> {
        let event = Event::HidePopup;
        self.editor.apply_global_event(event)
    }

    // === COMMAND API ===

    pub fn register_command(&mut self, command: Command) {
        self.editor.command_registry.register(command);
    }

    pub fn execute_action(&mut self, action: Action) -> Result<()> {
        self.editor.handle_action(action)
    }

    // === HOOK API ===

    pub fn on<F>(&mut self, hook_name: &str, callback: F)
    where
        F: Fn(&HookArgs) -> bool + Send + 'static,
    {
        self.editor.hook_registry.add_hook(hook_name, Box::new(callback));
    }

    // === ASYNC API ===

    pub fn spawn_async<F>(&self, task: F)
    where
        F: Future<Output = AsyncMessage> + Send + 'static,
    {
        let sender = self.editor.async_bridge.sender();
        self.editor.tokio_runtime.spawn(async move {
            let result = task.await;
            let _ = sender.send(result);
        });
    }

    pub fn run_command(&self, cmd: &str, args: &[&str]) -> Result<String> {
        // Execute external command and return stdout
        use std::process::Command;
        let output = Command::new(cmd).args(args).output()?;
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    // === KEYBINDING API ===

    pub fn register_keybinding(&mut self, binding: Keybinding) {
        self.editor.keybinding_registry.register(binding);
    }

    // === QUERY API ===

    pub fn get_buffer_file_path(&self, id: BufferId) -> Option<PathBuf> {
        self.editor.get_buffer_file_path(id)
    }

    pub fn get_active_buffer(&self) -> BufferId {
        self.editor.active_buffer
    }

    pub fn get_all_buffers(&self) -> Vec<BufferId> {
        self.editor.buffers.keys().copied().collect()
    }
}
```

### Infrastructure 4: Command Registry (HIGH PRIORITY)

**Implementation:**
```rust
// src/command_registry.rs

pub struct CommandRegistry {
    commands: Vec<Command>,
}

impl CommandRegistry {
    pub fn new() -> Self {
        Self {
            commands: get_builtin_commands(),
        }
    }

    pub fn register(&mut self, command: Command) {
        // Remove existing command with same name
        self.commands.retain(|c| c.name != command.name);
        self.commands.push(command);
    }

    pub fn unregister(&mut self, name: &str) {
        self.commands.retain(|c| c.name != name);
    }

    pub fn get_all(&self) -> &[Command] {
        &self.commands
    }

    pub fn filter(&self, query: &str, context: KeyContext) -> Vec<Suggestion> {
        filter_commands(&self.commands, query, context)
    }
}

// Update src/commands.rs
pub fn get_builtin_commands() -> Vec<Command> {
    // Return default commands (currently in get_all_commands)
}
```

### Infrastructure 5: Async Task Management (MEDIUM PRIORITY)

**Current AsyncBridge is Good, but Add:**
```rust
// src/plugin_async.rs

pub struct PluginTaskHandle {
    id: u64,
    cancel_sender: tokio::sync::oneshot::Sender<()>,
}

impl PluginTaskHandle {
    pub fn cancel(self) {
        let _ = self.cancel_sender.send(());
    }
}

pub struct AsyncTaskManager {
    next_id: u64,
    tasks: HashMap<u64, PluginTaskHandle>,
}

impl AsyncTaskManager {
    pub fn spawn<F>(&mut self, task: F) -> u64
    where
        F: Future<Output = AsyncMessage> + Send + 'static,
    {
        let id = self.next_id;
        self.next_id += 1;

        let (cancel_tx, cancel_rx) = tokio::sync::oneshot::channel();

        tokio::spawn(async move {
            tokio::select! {
                result = task => {
                    // Send result to async bridge
                }
                _ = cancel_rx => {
                    // Task cancelled
                }
            }
        });

        self.tasks.insert(id, PluginTaskHandle { id, cancel_sender: cancel_tx });
        id
    }

    pub fn cancel_task(&mut self, id: u64) {
        if let Some(handle) = self.tasks.remove(&id) {
            handle.cancel();
        }
    }
}
```

### Infrastructure 6: Context Stack (LOW PRIORITY)

**Extend KeyContext:**
```rust
// src/context.rs

pub struct ContextStack {
    major: String,              // e.g., "rust-mode", "magit-mode"
    minor: Vec<String>,         // e.g., ["line-numbers", "git-gutter"]
}

impl ContextStack {
    pub fn push_minor(&mut self, context: String) {
        if !self.minor.contains(&context) {
            self.minor.push(context);
        }
    }

    pub fn pop_minor(&mut self, context: &str) {
        self.minor.retain(|c| c != context);
    }

    pub fn set_major(&mut self, context: String) {
        self.major = context;
    }

    pub fn contexts(&self) -> Vec<&str> {
        let mut contexts = vec![self.major.as_str()];
        contexts.extend(self.minor.iter().map(|s| s.as_str()));
        contexts
    }
}

// Keybinding resolution checks all contexts in order
```

---

## Implementation Challenges

### Challenge 1: Ownership & Borrowing in Plugin API

**Problem:**
Plugins need mutable access to Editor, but Rust's borrow checker prevents multiple mutable references.

**Solution 1: Interior Mutability**
```rust
pub struct Editor {
    buffers: Arc<RwLock<HashMap<BufferId, EditorState>>>,
    // ... other fields wrapped in Arc<RwLock<>>
}

// Allows plugins to hold references without preventing other access
```

**Solution 2: Message Passing**
```rust
// Plugins send commands, don't directly mutate
pub enum PluginCommand {
    InsertText { buffer_id: BufferId, position: usize, text: String },
    AddOverlay { buffer_id: BufferId, overlay: Overlay },
    // ...
}

// Plugin sends commands to queue, editor processes them
```

**Solution 3: Capability-Based API**
```rust
// Each plugin operation gets a short-lived mutable reference
impl PluginContext<'a> {
    pub fn with_buffer<F, R>(&mut self, id: BufferId, f: F) -> Option<R>
    where
        F: FnOnce(&mut EditorState) -> R,
    {
        self.editor.buffers.get_mut(&id).map(f)
    }
}
```

### Challenge 2: Synchronous vs Asynchronous Execution

**Problem:**
Main loop is synchronous (60fps), but plugins may need async I/O (network, file system).

**Current Solution (Keep):**
AsyncBridge pattern - plugins spawn Tokio tasks, send results via channel

**Enhancement Needed:**
```rust
// Add plugin-specific async message types
pub enum AsyncMessage {
    // ... existing variants
    PluginMessage {
        plugin_name: String,
        data: serde_json::Value,  // Or Box<dyn Any>
    },
}

// Plugin registers handler
ctx.on_async_message(|data| {
    // Process async result in main loop
});
```

### Challenge 3: Error Handling & Plugin Crashes

**Problem:**
Plugin errors shouldn't crash the editor.

**Solution: Catch Panics**
```rust
impl PluginManager {
    pub fn run_hook_safe(&self, name: &str, args: &HookArgs) -> bool {
        let result = std::panic::catch_unwind(|| {
            self.hooks.run_hooks(name, args)
        });

        match result {
            Ok(continue_) => continue_,
            Err(e) => {
                eprintln!("Plugin panic in hook '{}': {:?}", name, e);
                // Optionally disable the plugin
                true  // Continue execution
            }
        }
    }
}
```

**For Lua:**
```rust
// mlua already provides error handling
match lua.load(code).exec() {
    Ok(_) => {},
    Err(e) => {
        eprintln!("Lua error: {}", e);
        // Show error in status bar
        self.set_status_message(format!("Plugin error: {}", e));
    }
}
```

### Challenge 4: Plugin Performance & Resource Limits

**Problem:**
Malicious or poorly written plugins could freeze the editor.

**Solution 1: Timeout Hooks**
```rust
use std::time::{Duration, Instant};

pub fn run_hooks_with_timeout(&self, name: &str, args: &HookArgs, timeout: Duration) -> bool {
    let start = Instant::now();

    if let Some(hooks) = self.hooks.get(name) {
        for hook in hooks {
            if start.elapsed() > timeout {
                eprintln!("Hook '{}' timeout exceeded", name);
                return true;  // Continue, but warn
            }

            if !hook(args) {
                return false;
            }
        }
    }
    true
}
```

**Solution 2: WASM Sandboxing**
```rust
// WASM provides built-in sandboxing
// Can limit:
// - Memory usage
// - CPU time (instruction count)
// - System calls (no file system access without permission)
```

**Solution 3: Plugin Metrics**
```rust
pub struct PluginMetrics {
    pub total_cpu_time: Duration,
    pub hook_call_count: HashMap<String, usize>,
    pub avg_hook_duration: HashMap<String, Duration>,
}

// Track and display in :plugins command
// Warn user if plugin is slow
```

### Challenge 5: Plugin Versioning & Compatibility

**Problem:**
Editor updates may break plugins.

**Solution: Semantic Versioning**
```rust
pub struct PluginApi {
    pub version: semver::Version,  // e.g., "0.1.0"
}

// Plugin manifest specifies required version
[dependencies]
editor_version = ">=0.1.0,<0.2.0"

// Check on load
fn load_plugin(&mut self, manifest: &PluginManifest) -> Result<()> {
    if !manifest.editor_version.matches(&PLUGIN_API_VERSION) {
        return Err(Error::IncompatibleVersion);
    }
    // ...
}
```

**Solution: Deprecated API Warnings**
```rust
#[deprecated(since = "0.2.0", note = "Use `insert_text_at_cursor` instead")]
pub fn insert_text(&mut self, text: &str) { }
```

### Challenge 6: Plugin Dependencies

**Problem:**
Plugin A depends on Plugin B's functionality.

**Solution: Load Order**
```toml
# plugin.toml
[dependencies]
plugins = ["common-utils", "git-integration"]

# Topological sort on load
```

**Solution: Service Registration**
```rust
// Plugin B provides service
ctx.register_service("git", Box::new(GitService { ... }));

// Plugin A uses service
let git = ctx.get_service::<GitService>("git")?;
git.get_status();
```

---

## Proposed Architecture

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                        EDITOR CORE                          │
│  ┌────────────┐  ┌─────────────┐  ┌──────────────────┐     │
│  │  Event     │  │  Editor     │  │  Plugin          │     │
│  │  Loop      │→ │  State      │← │  Manager         │     │
│  └────────────┘  └─────────────┘  └──────────────────┘     │
│                         ↕                   ↕                │
│                  ┌─────────────┐    ┌──────────────┐        │
│                  │  Event Log  │    │  Hook        │        │
│                  │             │    │  Registry    │        │
│                  └─────────────┘    └──────────────┘        │
└─────────────────────────────────────────────────────────────┘
                              ↕
┌─────────────────────────────────────────────────────────────┐
│                      PLUGIN API LAYER                       │
│  ┌────────────────────────────────────────────────────┐     │
│  │              PluginContext (FFI Bridge)            │     │
│  │  • Buffer API    • Overlay API   • Command API     │     │
│  │  • Hook API      • Async API     • Query API       │     │
│  └────────────────────────────────────────────────────┘     │
└─────────────────────────────────────────────────────────────┘
                              ↕
┌─────────────────────────────────────────────────────────────┐
│                    SCRIPTING RUNTIMES                       │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │  Lua         │  │  WASM        │  │  Native      │      │
│  │  (mlua)      │  │  (wasmtime)  │  │  (.so/.dylib)│      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                              ↕
┌─────────────────────────────────────────────────────────────┐
│                          PLUGINS                            │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐   │
│  │  Magit   │  │  Git     │  │  Tree    │  │  Custom  │   │
│  │          │  │  Gutter  │  │  Sitter  │  │  Theme   │   │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘   │
└─────────────────────────────────────────────────────────────┘
```

### Data Flow Example: Magit Status

```
1. User presses Ctrl-x g
   ↓
2. KeybindingResolver → Action::Custom("magit-status")
   ↓
3. Editor checks ActionHandlerRegistry
   ↓
4. Calls Lua function magit.show_status()
   ↓
5. Lua calls editor:create_buffer("*magit-status*")
   ↓ FFI
6. PluginContext::create_buffer()
   ↓
7. Editor creates new buffer, returns BufferId
   ↓ FFI
8. Lua spawns async task: editor:spawn_async(...)
   ↓
9. Tokio task runs: git status --porcelain
   ↓
10. Task sends AsyncMessage::PluginMessage { data: status_output }
    ↓
11. Main loop receives message in process_async_messages()
    ↓
12. Calls Lua callback: on_git_status_result(data)
    ↓ FFI
13. PluginContext::insert_text() + add_overlay()
    ↓
14. Events applied: Event::Insert, Event::AddOverlay
    ↓
15. Next frame: render() displays magit buffer with colored overlays
```

### File Structure (Proposed)

```
src/
├── plugin_api.rs          # Public plugin API (PluginContext)
├── plugin_manager.rs      # Plugin lifecycle management
├── hooks.rs               # Hook registry and types
├── command_registry.rs    # Dynamic command registration
├── lua_bindings.rs        # Lua FFI implementation
├── wasm_bindings.rs       # WASM FFI implementation (future)
├── plugin_async.rs        # Async task management for plugins
└── ...

~/.config/editor/
├── config.json
└── plugins/
    ├── magit/
    │   ├── plugin.toml
    │   ├── init.lua
    │   └── git.lua
    ├── git-gutter/
    │   ├── plugin.toml
    │   └── init.lua
    └── custom-theme/
        ├── plugin.toml
        └── theme.lua
```

---

## Example Use Cases

### Use Case 1: Magit-Style Git Interface

**Goal:** Complex, interactive git buffer with keybindings

**Implementation:**
```lua
-- ~/.config/editor/plugins/magit/init.lua

local M = {}

function M.setup(editor)
  -- Create custom context for magit buffer
  editor:create_context("magit")

  -- Register command
  editor:register_command({
    name = "Magit Status",
    action = "magit-status",
    keybinding = { key = "g", mods = {"ctrl", "x"}, context = "normal" }
  })

  -- Register magit-specific keybindings
  editor:register_keybinding({
    key = "s",
    context = "magit",
    action = function() M.stage_file(editor) end
  })

  editor:register_keybinding({
    key = "c",
    context = "magit",
    action = function() M.commit(editor) end
  })

  editor:register_keybinding({
    key = "P",
    context = "magit",
    action = function() M.push(editor) end
  })

  -- Hook into save to refresh status
  editor:on("after-file-save", function(args)
    if M.status_buffer then
      M.refresh_status(editor)
    end
  end)
end

function M.show_status(editor)
  -- Create or reuse buffer
  M.status_buffer = editor:find_buffer("*magit-status*")
    or editor:create_buffer("*magit-status*")

  editor:switch_to_buffer(M.status_buffer)
  editor:set_buffer_context(M.status_buffer, "magit")

  -- Run git status async
  M.refresh_status(editor)
end

function M.refresh_status(editor)
  editor:spawn_async(function()
    local status = run_command("git", {"status", "--porcelain"})
    local diff = run_command("git", {"diff", "--stat"})

    return {
      type = "magit-status-result",
      status = status,
      diff = diff
    }
  end, function(result)
    -- Clear buffer
    editor:clear_buffer(M.status_buffer)

    -- Insert header
    editor:insert_text(M.status_buffer, 0, "Magit Status\n\n")

    -- Parse and format status
    local pos = editor:get_buffer_length(M.status_buffer)

    -- Unstaged files
    editor:insert_text(M.status_buffer, pos, "Unstaged changes:\n")
    for line in result.status:gmatch("[^\n]+") do
      if line:match("^ M") then
        local file = line:sub(4)
        local start = editor:get_buffer_length(M.status_buffer)
        editor:insert_text(M.status_buffer, start, "  " .. file .. "\n")

        -- Add overlay for modified marker
        local range = { start = start, end = start + 2 }
        editor:add_overlay({
          buffer = M.status_buffer,
          range = range,
          face = { foreground = {255, 165, 0} },  -- Orange
          priority = 10
        })

        -- Store file info for staging
        M.file_positions[start] = file
      end
    end

    -- Insert diff stats
    pos = editor:get_buffer_length(M.status_buffer)
    editor:insert_text(M.status_buffer, pos, "\n" .. result.diff)
  end)
end

function M.stage_file(editor)
  local cursor_pos = editor:get_cursor_position(M.status_buffer)
  local file = M.file_positions[cursor_pos]

  if file then
    editor:spawn_async(function()
      run_command("git", {"add", file})
      return { type = "magit-stage-complete" }
    end, function()
      M.refresh_status(editor)
      editor:set_status("Staged: " .. file)
    end)
  end
end

function M.commit(editor)
  editor:prompt("Commit message: ", function(message)
    editor:spawn_async(function()
      run_command("git", {"commit", "-m", message})
      return { type = "magit-commit-complete" }
    end, function()
      M.refresh_status(editor)
      editor:set_status("Committed: " .. message)
    end)
  end)
end

return M
```

**What This Demonstrates:**
- ✅ Custom buffer with special keybindings
- ✅ Async command execution (git status, add, commit)
- ✅ Overlays for visual decoration
- ✅ Interactive prompts
- ✅ Hook subscription (refresh on save)

### Use Case 2: Git Gutter (Line-by-Line Diff)

**Goal:** Show +/- markers in margin for changed lines

**Implementation:**
```lua
-- ~/.config/editor/plugins/git-gutter/init.lua

local M = {
  cache = {}  -- file_path → diff hunks
}

function M.setup(editor)
  -- Enable for all git-tracked files
  editor:on("after-file-open", function(args)
    M.update_gutter(editor, args.buffer_id)
  end)

  editor:on("after-insert", function(args)
    M.schedule_update(editor, args.buffer_id)
  end)

  editor:on("after-delete", function(args)
    M.schedule_update(editor, args.buffer_id)
  end)

  -- Periodic refresh
  editor:on_idle(500, function()  -- 500ms idle
    local buf = editor:get_active_buffer()
    M.update_gutter(editor, buf)
  end)
end

function M.update_gutter(editor, buffer_id)
  local file_path = editor:get_buffer_file_path(buffer_id)
  if not file_path then return end

  editor:spawn_async(function()
    local diff = run_command("git", {"diff", "--unified=0", file_path})
    return {
      type = "git-gutter-result",
      buffer_id = buffer_id,
      hunks = M.parse_diff(diff)
    }
  end, function(result)
    -- Clear old margin annotations
    editor:clear_margin_position(result.buffer_id, "left")

    -- Add new markers
    for _, hunk in ipairs(result.hunks) do
      if hunk.type == "added" then
        for line = hunk.start_line, hunk.end_line do
          editor:add_margin_annotation({
            buffer = result.buffer_id,
            line = line,
            position = "left",
            content = { text = "+", face = { foreground = {0, 255, 0} } }
          })
        end
      elseif hunk.type == "deleted" then
        editor:add_margin_annotation({
          buffer = result.buffer_id,
          line = hunk.start_line,
          position = "left",
          content = { text = "-", face = { foreground = {255, 0, 0} } }
        })
      elseif hunk.type == "modified" then
        for line = hunk.start_line, hunk.end_line do
          editor:add_margin_annotation({
            buffer = result.buffer_id,
            line = line,
            position = "left",
            content = { text = "~", face = { foreground = {255, 165, 0} } }
          })
        end
      end
    end
  end)
end

return M
```

**What This Demonstrates:**
- ✅ Margin annotations (git gutter markers)
- ✅ Hook-based reactivity (update on edit/save)
- ✅ Idle callback (periodic refresh)
- ✅ Async git operations

### Use Case 3: Custom Language Mode (Non-LSP)

**Goal:** TOML mode with custom syntax highlighting and formatting

**Implementation:**
```lua
-- ~/.config/editor/plugins/toml-mode/init.lua

local M = {}

function M.setup(editor)
  -- Register for .toml files
  editor:on("after-file-open", function(args)
    local path = editor:get_buffer_file_path(args.buffer_id)
    if path and path:match("%.toml$") then
      M.activate(editor, args.buffer_id)
    end
  end)

  -- Register commands
  editor:register_command({
    name = "Format TOML",
    action = function()
      M.format(editor, editor:get_active_buffer())
    end,
    contexts = {"normal"}
  })
end

function M.activate(editor, buffer_id)
  -- Set buffer-local keybindings
  editor:add_buffer_keybinding(buffer_id, {
    key = "f",
    mods = {"ctrl"},
    action = function() M.format(editor, buffer_id) end
  })

  -- Add syntax highlighting overlays
  M.highlight(editor, buffer_id)

  -- Auto-format on save
  editor:on("before-file-save", function(args)
    if args.buffer_id == buffer_id then
      M.format(editor, buffer_id)
    end
  end)
end

function M.highlight(editor, buffer_id)
  local content = editor:get_buffer_content(buffer_id)

  -- Clear existing overlays
  editor:clear_overlays(buffer_id)

  -- Regex-based syntax highlighting
  -- (In real implementation, use tree-sitter or similar)

  -- Highlight section headers: [section]
  for match in content:gmatch("%[([^%]]+)%]") do
    local start = content:find("%[" .. match .. "%]")
    editor:add_overlay({
      buffer = buffer_id,
      range = { start = start, end = start + #match + 2 },
      face = { foreground = {100, 150, 255}, bold = true },
      priority = 5
    })
  end

  -- Highlight keys
  for key in content:gmatch("([%w_]+)%s*=") do
    local start = content:find(key .. "%s*=")
    editor:add_overlay({
      buffer = buffer_id,
      range = { start = start, end = start + #key },
      face = { foreground = {255, 200, 100} },
      priority = 5
    })
  end
end

function M.format(editor, buffer_id)
  local content = editor:get_buffer_content(buffer_id)

  editor:spawn_async(function()
    -- Run external formatter (e.g., taplo)
    local formatted = run_command("taplo", {"format", "-"}, content)
    return { type = "toml-format-result", formatted = formatted }
  end, function(result)
    -- Replace buffer content
    local len = editor:get_buffer_length(buffer_id)
    editor:delete_range(buffer_id, { start = 0, end = len })
    editor:insert_text(buffer_id, 0, result.formatted)

    -- Re-highlight
    M.highlight(editor, buffer_id)
  end)
end

return M
```

**What This Demonstrates:**
- ✅ File type detection and mode activation
- ✅ Buffer-local keybindings
- ✅ Custom syntax highlighting via overlays
- ✅ Format-on-save hook
- ✅ External tool integration

---

## Roadmap

### Phase 1: Foundation (2-3 months)

**Goal:** Basic Lua plugin support with core API

**Deliverables:**
1. **Hook System**
   - [ ] Implement HookRegistry
   - [ ] Add hook invocation points in editor.rs
   - [ ] Document all available hooks

2. **Plugin Manager**
   - [ ] Basic plugin discovery (scan ~/.config/editor/plugins/)
   - [ ] Lua runtime integration (mlua)
   - [ ] Plugin load/unload lifecycle

3. **Core Plugin API**
   - [ ] Buffer API (create, insert, delete, query)
   - [ ] Overlay API (add, remove, query)
   - [ ] Popup API (show, hide)
   - [ ] Command registration
   - [ ] Keybinding registration

4. **Example Plugins**
   - [ ] Line numbers (demonstrate margin API)
   - [ ] Git gutter (demonstrate hooks + async)
   - [ ] Custom theme (demonstrate config)

**Success Criteria:**
- ✅ Can load and run Lua plugins
- ✅ Plugins can register commands and keybindings
- ✅ Plugins can modify buffers and add overlays
- ✅ Plugin errors don't crash editor

### Phase 2: Advanced Features (2-3 months)

**Goal:** Powerful plugin capabilities (Magit-level complexity)

**Deliverables:**
1. **Async Task Management**
   - [ ] PluginTaskHandle for cancellation
   - [ ] Task timeout and resource limits
   - [ ] Progress reporting API

2. **Context Stack**
   - [ ] Dynamic context creation
   - [ ] Context switching (major/minor modes)
   - [ ] Context-aware keybinding resolution

3. **Advanced UI APIs**
   - [ ] Custom popup content (rendering callbacks)
   - [ ] Popup event handling (key dispatch)
   - [ ] Multi-pane layouts

4. **Query API**
   - [ ] Buffer introspection (content, cursors, overlays)
   - [ ] Editor state queries (active buffer, splits)
   - [ ] File system queries

5. **Example Plugins**
   - [ ] Magit (complex git interface)
   - [ ] File browser (custom UI)
   - [ ] Snippet expansion

**Success Criteria:**
- ✅ Can implement Magit-level plugins
- ✅ Plugins can create custom UIs
- ✅ Plugins can manage async tasks

### Phase 3: Performance & Polish (1-2 months)

**Goal:** Production-ready plugin system

**Deliverables:**
1. **WASM Support**
   - [ ] WASM runtime integration (wasmtime)
   - [ ] WASM FFI bindings
   - [ ] Example plugin in Rust/AssemblyScript

2. **Plugin Tooling**
   - [ ] Plugin template generator
   - [ ] Plugin debugger
   - [ ] Plugin testing framework

3. **Performance**
   - [ ] Hook timeout enforcement
   - [ ] Plugin metrics and profiling
   - [ ] Resource limit enforcement

4. **Documentation**
   - [ ] Plugin API reference
   - [ ] Plugin development guide
   - [ ] Example plugin cookbook

**Success Criteria:**
- ✅ WASM plugins work alongside Lua
- ✅ Plugin performance is monitored
- ✅ Comprehensive documentation

### Phase 4: Ecosystem (Ongoing)

**Goal:** Grow plugin ecosystem

**Deliverables:**
1. **Plugin Registry**
   - [ ] Central plugin repository
   - [ ] Plugin discovery/installation
   - [ ] Version management

2. **More APIs**
   - [ ] Network API (HTTP requests)
   - [ ] Database API (SQLite)
   - [ ] IPC API (communicate with other processes)

3. **Native Plugin Support**
   - [ ] Dynamic library loading
   - [ ] ABI stability layer
   - [ ] Plugin signing/sandboxing

**Success Criteria:**
- ✅ 20+ community plugins
- ✅ Plugin installation is one command
- ✅ Plugin API is stable (1.0.0)

---

## Conclusion

The editor has an **excellent foundation** for an Emacs-style plugin system:

### Existing Strengths
- ✅ Event-driven architecture (perfect for plugin reactivity)
- ✅ Overlay/popup primitives (Emacs-style UI components)
- ✅ AsyncBridge pattern (clean async integration)
- ✅ Command palette infrastructure (easy to extend)
- ✅ Context-aware keybindings (modal support)

### Critical Next Steps
1. **Add Lua runtime** (mlua crate) for dynamic scripting
2. **Implement hook system** for plugin event subscription
3. **Create plugin API module** with FFI bindings
4. **Build 2-3 example plugins** to validate API design

### Timeline Estimate
- **Phase 1 (Foundation):** 2-3 months → Basic Lua plugins working
- **Phase 2 (Advanced):** 2-3 months → Magit-level complexity achievable
- **Phase 3 (Polish):** 1-2 months → Production-ready with WASM support
- **Phase 4 (Ecosystem):** Ongoing → Community growth

### Recommended Starting Point

**Week 1-2: Hook System**
```rust
// src/hooks.rs
pub struct HookRegistry { ... }
pub enum HookArgs { ... }
```

**Week 3-4: Lua Integration**
```rust
// src/lua_bindings.rs
use mlua::Lua;
struct EditorApi { ... }
```

**Week 5-6: Plugin Manager**
```rust
// src/plugin_manager.rs
pub struct PluginManager { ... }
```

**Week 7-8: First Plugin**
```lua
-- ~/.config/editor/plugins/git-gutter/init.lua
```

The architecture is sound, the primitives exist, and the path forward is clear. This can become a truly Emacs-level extensible editor.
