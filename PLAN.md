# Design: "Load Plugin from Buffer" Command

## Goal

Allow users to take the code in the current editor buffer and run it as a plugin — with the full plugin API available (editor commands, hooks, overlays, etc.) — without saving it to the plugins directory or "installing" it. This streamlines plugin development and enables ad-hoc scripting.

## Current Architecture (Summary)

The existing plugin loading pipeline:

1. **Discovery**: Scan `~/.config/fresh/plugins/` for `.ts`/`.js` files
2. **Transform**: `load_module_with_source()` reads the file from disk, then:
   - If ES imports → `bundle_module()` (resolves local deps, bundles into IIFE)
   - If ES exports only → `strip_imports_and_exports()` + `transpile_typescript()` if `.ts`
   - Plain code → `transpile_typescript()` if `.ts`, else run directly
3. **Execution**: `execute_js(code, source_name)` creates a per-plugin QuickJS `Context`, sets up the editor API (`getEditor()`, hooks, commands), wraps code in IIFE, and evals it
4. **Registration**: Plugin is stored in `HashMap<String, TsPluginInfo>` with name, path, enabled flag

Key types/channels:
- `PluginRequest` enum sent over `mpsc::UnboundedSender` to plugin thread
- `PluginThreadHandle` provides blocking methods (`load_plugin`, `unload_plugin`, `reload_plugin`)
- `PluginManager` (in `fresh-editor`) wraps `PluginThreadHandle`
- `PluginCommand` enum (in `fresh-core`) for plugin→editor communication

## Design Alternatives

### Alternative A: New `PluginRequest::LoadPluginFromSource` (Recommended)

Add a new variant to `PluginRequest` that accepts source code directly instead of a file path:

```
PluginRequest::LoadPluginFromSource {
    source: String,          // The buffer contents
    name: String,            // Synthetic plugin name (e.g. "buffer-plugin" or derived from buffer)
    source_type: SourceType, // Ts or Js, inferred from buffer language
    response: oneshot::Sender<Result<()>>,
}
```

**New function** `load_plugin_from_source_internal()` — mirrors `load_plugin_internal()` but:
- Accepts source code as a `String` instead of reading from disk
- Skips i18n file loading (no companion files for a buffer)
- Runs the same transform pipeline (transpile TS, strip exports, etc.)
- Calls `execute_js()` with a synthetic source name like `"<buffer>"` or `"buffer-plugin.ts"`
- Registers in the plugins HashMap so it can be unloaded/reloaded later

**Editor side**: New `Action::LoadPluginFromBuffer` triggers:
1. Read current buffer contents via `self.active_state().buffer.slice_bytes(0..total_bytes)`
2. Detect language (TS vs JS) from buffer's language mode or file extension
3. Send `PluginRequest::LoadPluginFromSource` to plugin thread
4. Show status message on success/failure

**Command palette entry**: Register as "Load Plugin from Buffer" in `COMMAND_DEFS`.

**Pros**:
- Clean separation: new code path purpose-built for source-from-memory
- No temp files, no filesystem side effects
- Plugin can be unloaded cleanly (it's registered with a name)
- Follows existing patterns exactly

**Cons**:
- Some code duplication with `load_plugin_internal` (mitigated by extracting shared transform logic)
- Bundling (`bundle_module`) won't work for buffer plugins with local imports since there's no filesystem path to resolve relative imports from

### Alternative B: Write to temp file, then `load_plugin()`

Save the buffer to a temp file in a known location (e.g. `/tmp/fresh-buffer-plugin.ts`), then call the existing `load_plugin()` path.

**Pros**:
- Zero new code in the plugin runtime — reuses everything
- Bundling works (temp file has a real path for import resolution)

**Cons**:
- Filesystem side effects (temp files to manage/clean up)
- Race conditions if user runs it multiple times quickly
- Leaks implementation detail (temp paths show up in error messages, stack traces)
- Feels hacky — the buffer *is* the source, we shouldn't round-trip through disk

### Alternative C: Plugin API method `editor.loadPluginFromSource()`

Expose this as a JS API so plugins themselves can load other plugin source code. The command palette command would then be a thin wrapper.

**Pros**:
- Composable: other plugins can use it (e.g., a "plugin marketplace" plugin)
- Consistent with the API-first design

**Cons**:
- Security concern: arbitrary code injection from plugin to plugin
- More surface area than needed for the immediate goal
- Can be added later on top of Alternative A

## Recommended Approach: Alternative A

Alternative A is the cleanest. It follows the existing architecture patterns, avoids filesystem hacks, and is straightforward to implement. Alternative C is a nice follow-up but not needed for v1.

## Implementation Status

All items below are **implemented and tested**:

- [x] `PluginRequest::LoadPluginFromSource` variant added to `thread.rs`
- [x] `load_plugin_from_source_internal()` function with hot-reload (unload-then-load)
- [x] `QuickJsBackend::execute_source()` — transpile + execute source code without file I/O
- [x] `PluginThreadHandle::load_plugin_from_source()` — blocking public API
- [x] `PluginManager::load_plugin_from_source()` — editor-level API with `#[cfg(feature = "plugins")]`
- [x] `Action::LoadPluginFromBuffer` in both `fresh-core` and `fresh-editor` Action enums
- [x] Handler in `app/input.rs` — reads buffer content, detects TS/JS, calls plugin manager
- [x] Command palette entry: `cmd.load_plugin_from_buffer` in `COMMAND_DEFS`
- [x] `from_str` mapping: `"load_plugin_from_buffer"` in keybindings
- [x] **Hot-reload cleanup (Phase 1)**: `QuickJsBackend::cleanup_plugin()` cleans up plugin context, event handlers, registered actions, callback contexts
- [x] **Hot-reload cleanup (Phase 2)**: `PluginTrackedState` tracks namespaces/IDs per plugin; compensating `PluginCommand`s sent on unload for overlays, conceals, soft breaks, line indicators, virtual text, file explorer decorations, and custom contexts
- [x] **Hot-reload cleanup (Phase 3)**: Resource cleanup on unload — kills background processes, closes virtual/composite buffers, closes terminals, removes scroll sync groups. Uses shared `AsyncResourceOwners` map for tracking async resource IDs across threads.
- [x] **E2E tests**: `test_load_plugin_from_buffer_registers_command` and `test_load_plugin_from_buffer_hot_reload_cleanup` in `tests/e2e/plugins/load_from_buffer.rs`

## Detailed Design

### 1. Plugin Runtime Layer (`fresh-plugin-runtime`)

**`thread.rs`** — Add to `PluginRequest` enum:
```rust
LoadPluginFromSource {
    source: String,
    name: String,
    is_typescript: bool,
    response: oneshot::Sender<Result<()>>,
}
```

Add `PluginThreadHandle::load_plugin_from_source()` method (blocking, like `load_plugin()`).

Add `load_plugin_from_source_internal()` async fn:
```rust
async fn load_plugin_from_source_internal(
    runtime: Rc<RefCell<QuickJsBackend>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    source: &str,
    name: &str,
    is_typescript: bool,
) -> Result<()> {
    // If plugin with this name already loaded, unload first (hot-reload semantics)
    if plugins.contains_key(name) {
        unload_plugin_internal(Rc::clone(&runtime), plugins, name)?;
    }

    let js_code = if is_typescript {
        // Strip exports if present, then transpile
        let cleaned = if has_es_module_syntax(source) {
            strip_imports_and_exports(source)
        } else {
            source.to_string()
        };
        transpile_typescript(&cleaned, &format!("{}.ts", name))?
    } else {
        if has_es_module_syntax(source) {
            strip_imports_and_exports(source)
        } else {
            source.to_string()
        }
    };

    // Note: ES imports (import ... from ...) are NOT supported for buffer plugins
    // since there's no filesystem path to resolve relative imports from.
    if has_es_imports(source) {
        tracing::warn!("Buffer plugin '{}' has ES imports which cannot be resolved. Stripping them.", name);
    }

    let source_name = format!("<buffer:{}>", name);
    runtime.borrow_mut().execute_js(&js_code, &source_name)?;

    plugins.insert(name.to_string(), TsPluginInfo {
        name: name.to_string(),
        path: PathBuf::from(source_name), // synthetic path
        enabled: true,
    });

    Ok(())
}
```

Handle the new variant in `handle_request()`.

**`quickjs_backend.rs`** — Make `execute_js` `pub(crate)` (currently private) so `load_plugin_from_source_internal` can call it. Or extract the transform+execute logic into a shared helper.

### 2. Plugin Manager Layer (`fresh-editor/src/services/plugins/manager.rs`)

Add `PluginManager::load_plugin_from_source()`:
```rust
pub fn load_plugin_from_source(&self, source: &str, name: &str, is_typescript: bool) -> Result<()>
```

### 3. Editor Action (`fresh-core` + `fresh-editor`)

Add `Action::LoadPluginFromBuffer` to the Action enum.

In `input.rs` handler:
```rust
Action::LoadPluginFromBuffer => {
    let state = self.active_state();
    let buffer = &state.buffer;
    let content = String::from_utf8_lossy(&buffer.slice_bytes(0..buffer.total_bytes())).to_string();

    // Determine if TypeScript based on file extension or language mode
    let is_ts = buffer.file_path()
        .and_then(|p| p.extension())
        .and_then(|e| e.to_str())
        .map(|e| e == "ts" || e == "tsx")
        .unwrap_or(true); // default to TS (superset of JS)

    // Generate plugin name from filename or use generic name
    let name = buffer.file_path()
        .and_then(|p| p.file_stem())
        .and_then(|s| s.to_str())
        .map(|s| format!("buffer-{}", s))
        .unwrap_or_else(|| "buffer-plugin".to_string());

    match self.plugin_manager.load_plugin_from_source(&content, &name, is_ts) {
        Ok(()) => self.set_status(format!("Plugin '{}' loaded from buffer", name)),
        Err(e) => self.set_status(format!("Failed to load plugin: {}", e)),
    }
}
```

### 4. Command Palette Entry (`commands.rs`)

```rust
CommandDef {
    name_key: "command.load_plugin_from_buffer",
    desc_key: "command.load_plugin_from_buffer.desc",
    action: || Action::LoadPluginFromBuffer,
    contexts: &[KeyContext::Normal],
    custom_contexts: &[],
}
```

## Key Design Decisions & Tradeoffs

### Hot-reload semantics
When the user runs "Load Plugin from Buffer" on a buffer they've already loaded, we **unload the previous version first** then load the new one. This is critical for the dev workflow — edit, re-run, see changes. The alternative (error on duplicate name) would be frustrating.

### Plugin naming
We derive the name from the buffer's filename (e.g., `buffer-my_plugin` from `my_plugin.ts`). For unsaved buffers, we use `buffer-plugin`. This means:
- Named buffers get stable identities across reloads (good for hot-reload)
- Multiple unnamed buffers would collide — acceptable tradeoff for v1

### No import resolution
Buffer plugins can't use `import ... from './helper'` because there's no filesystem context for relative path resolution. This is an inherent limitation of in-memory evaluation. Workarounds for the future:
- If the buffer has a file path, we could use its directory for resolution
- We could support a "save and load" variant that saves first

### TypeScript default
If we can't determine the language, we default to TypeScript since TS is a superset of JS and the transpiler handles plain JS fine.

### No i18n support
Buffer plugins skip `.i18n.json` loading — ad-hoc plugins don't need localization. This simplifies the implementation.

## Hot-Reload: Plugin State Cleanup Audit

Hot-reload = unload previous version, then load new version. The current `unload_plugin_internal()` (`thread.rs:1270-1294`) only cleans up **2 of 20+** state types. This section catalogs every piece of plugin-created state, whether it's cleaned up, and what we need to fix.

### Current unload implementation

```rust
fn unload_plugin_internal(...) {
    plugins.remove(name);                          // ✅ plugin registry
    runtime.services.unregister_plugin_strings();  // ✅ i18n strings
    runtime.services.unregister_commands_by_plugin(); // ✅ commands (CommandRegistry)
}
```

That's it. Everything else leaks.

### Complete state inventory

#### TIER 1 — Plugin runtime state (QuickJsBackend-owned, in-process)

These are `Rc<RefCell<HashMap<...>>>` fields on `QuickJsBackend`. Cleanup is easy — just filter/remove by plugin name.

| State | Storage | Cleaned? | Hot-reload impact if leaked |
|---|---|---|---|
| **Plugin JS Context** | `plugin_contexts: HashMap<String, Context>` | ❌ | Memory leak. On reload, `execute_js` checks `plugin_contexts` by name — if found, **reuses the old context**. Old globals, old closures, old handler references all persist. New code runs in old context. **This is the worst leak** — it means hot-reload doesn't actually start fresh. |
| **Event handlers** | `event_handlers: HashMap<String, Vec<PluginHandler>>` | ❌ | Old handlers still fire on hooks. After reload, both old AND new handlers run. Duplicate side effects. |
| **Registered actions** | `registered_actions: HashMap<String, PluginHandler>` | ❌ | Old action handlers still reference old plugin name. Commands registered by `registerCommand()` are cleaned (CommandRegistry), but actions registered by `defineMode()` bindings leak. Stale action dispatch. |
| **Callback contexts** | `callback_contexts: HashMap<u64, String>` | ❌ | Old in-flight async callbacks (delay, spawnProcess, etc.) still reference old plugin. If they resolve after unload, they try to dispatch into the old (still-alive due to context leak) context. Minor — callbacks drain naturally. |
| **Background process handles** | `background_process_handles` (if tracked) | ❌ | Processes keep running. On reload, plugin spawns new processes. Old ones are orphaned. |

#### TIER 2 — Editor-side state (sent via PluginCommand channel, owned by editor)

These are created by sending `PluginCommand` variants. The plugin runtime doesn't own them — the editor does. Cleanup requires either (a) sending compensating `PluginCommand`s during unload, or (b) the editor tracking plugin ownership.

**Namespace-based state** — Plugins pass an arbitrary `namespace: String` to these APIs. The namespace is NOT automatically prefixed with the plugin name; it's whatever the plugin chooses (e.g., `"git-gutter"`, `"diagnostics"`). This means we can't generically "clear all state for plugin X" on the editor side without either:
- Convention: auto-prefix namespace with plugin name at the API level
- Tracking: maintain a `plugin_name → Vec<namespace>` map in the runtime

| State | Creation API | Cleanup API exists? | Cleaned on unload? | Impact |
|---|---|---|---|---|
| **Overlays** | `addOverlay(bufferId, namespace, ...)` | `clearNamespace(bufferId, ns)` exists | ❌ | Stale syntax highlighting, diagnostics highlights persist visually |
| **Conceals** | `addConceal(bufferId, namespace, ...)` | `clearConcealNamespace(bufferId, ns)` exists | ❌ | Text remains hidden/replaced. Content appears corrupted. |
| **Soft breaks** | `addSoftBreak(bufferId, namespace, ...)` | `clearSoftBreakNamespace(bufferId, ns)` exists | ❌ | Line wrapping artifacts |
| **Virtual text** | `addVirtualText(bufferId, id, ...)` | `removeVirtualText(bufferId, id)` exists | ❌ | Stale inline hints/swatches remain |
| **Virtual lines** | `addVirtualLine(bufferId, ..., namespace, ...)` | `clearVirtualLineNamespace(bufferId, ns)` exists | ❌ | Stale git blame, inline docs remain |
| **Line indicators** | `setLineIndicator(bufferId, line, namespace, ...)` | `clearLineIndicators(bufferId, ns)` exists | ❌ | Stale gutter markers (git, breakpoints) |
| **View transforms** | `submitViewTransform(bufferId, splitId, ...)` | `clearViewTransform(bufferId, splitId)` exists | ❌ | Corrupted custom rendering persists |
| **Layout hints** | `setLayoutHints(bufferId, splitId, ...)` | No explicit clear | ❌ | Stale layout config |
| **File explorer decorations** | `setFileExplorerDecorations(namespace, ...)` | Overwrite with empty = clear | ❌ | Stale file tree icons/colors |
| **Custom contexts** | `setContext(name, active)` | `setContext(name, false)` | ❌ | Stale keybinding conditions; commands visible/hidden incorrectly |
| **Modes** | `defineMode(name, ...)` | No explicit undefine | ❌ | Mode persists; if plugin re-registers same name, likely overwrites (ok for hot-reload) |
| **Menu items** | `addMenu(...)`, `addMenuItem(...)` | `removeMenuItem(...)` exists | ❌ | Stale menu items; clicking runs nonexistent handler |
| **Scroll sync groups** | `createScrollSyncGroup(groupId, ...)` | `removeScrollSyncGroup(groupId)` exists | ❌ | Phantom scroll syncing persists |
| **Grammars** | `registerGrammar(language, ...)` | No explicit unregister | ❌ | Grammar persists; re-register on reload overwrites (likely ok) |
| **Language configs** | `registerLanguageConfig(language, ...)` | No explicit unregister | ❌ | Config persists; re-register overwrites (likely ok) |
| **LSP server configs** | `registerLspServer(language, ...)` | No explicit unregister | ❌ | Config persists; re-register overwrites (likely ok) |

**Buffer-creating APIs** — These create real editor buffers. No plugin ownership tracking exists.

| State | Creation API | Cleaned on unload? | Impact |
|---|---|---|---|
| **Virtual buffers** | `createVirtualBuffer(...)` | ❌ | Buffers persist as open tabs. On reload, plugin creates new ones → tab accumulation. |
| **Composite buffers** | `createCompositeBuffer(...)` | ❌ | Same as virtual buffers |
| **Terminals** | `createTerminal(...)` | ❌ | Terminal processes keep running; new ones created on reload |

### Recommended cleanup strategy

#### Phase 1: Must-have for hot-reload (runtime-side, easy)

These are all in-process, no editor coordination needed. Fix `unload_plugin_internal` to also:

```rust
fn unload_plugin_internal(...) {
    // ... existing cleanup ...

    let rt = runtime.borrow();

    // 1. Remove plugin's JS context (CRITICAL — without this, reload reuses old context)
    rt.plugin_contexts.borrow_mut().remove(name);

    // 2. Remove event handlers for this plugin
    for handlers in rt.event_handlers.borrow_mut().values_mut() {
        handlers.retain(|h| h.plugin_name != name);
    }

    // 3. Remove registered actions for this plugin
    rt.registered_actions.borrow_mut().retain(|_, h| h.plugin_name != name);

    // 4. Remove callback contexts for this plugin
    rt.callback_contexts.borrow_mut().retain(|_, pname| pname != name);
}
```

**Without at least items 1-3, hot-reload is fundamentally broken.** Item 1 is the most critical — if the old context survives, `execute_js` will reuse it and new code runs alongside old closures/handlers.

#### Phase 2: Important for visual correctness (namespace tracking)

Add a `plugin_namespaces: Rc<RefCell<HashMap<String, Vec<String>>>>` to `JsEditorApi` that records every namespace string a plugin uses. In each `addOverlay`, `addConceal`, `addSoftBreak`, `addVirtualLine`, `setLineIndicator`, `setFileExplorerDecorations` call, record `plugin_name → namespace`.

On unload, send compensating commands for each tracked namespace:
- `ClearNamespace` for overlays
- `ClearConcealNamespace` for conceals
- `ClearSoftBreakNamespace` for soft breaks
- `ClearVirtualLineNamespace` for virtual lines
- `ClearLineIndicators` (per namespace) for line indicators
- `SetFileExplorerDecorations` with empty list for file explorer

Also track `plugin_name → Vec<virtual_text_id>` for virtual text cleanup, and `plugin_name → Vec<context_name>` for context deactivation.

This requires the cleanup to send `PluginCommand`s, which means the unload function needs access to the `command_sender`. Currently `unload_plugin_internal` has access to the runtime (which has `command_sender` on `JsEditorApi`), so this is feasible.

#### Phase 3: Nice-to-have (resource cleanup)

- Kill background processes on unload (track `plugin_name → Vec<process_id>`)
- Close virtual/composite buffers created by plugin (track `plugin_name → Vec<BufferId>`)
- Close terminals created by plugin
- Remove scroll sync groups (track `plugin_name → Vec<group_id>`)
- Remove menu items (requires menu tracking infrastructure)

These are less critical because:
- Processes/terminals are visible to the user (they can close them manually)
- Menu items and scroll sync groups are relatively rare in ad-hoc plugins
- Grammars/language configs/LSP configs overwrite on re-registration (idempotent)

### Summary: What's safe to defer

| State | Safe to defer? | Why |
|---|---|---|
| Grammars, language configs, LSP configs | ✅ Yes | Re-registration overwrites; idempotent |
| Modes | ✅ Yes | `defineMode` with same name overwrites bindings |
| View state | ✅ Yes | Persisted per-buffer; doesn't cause errors |
| Terminals, virtual buffers | ⚠️ Mostly | User can close manually; accumulation is annoying but not broken |
| Menu items | ⚠️ Mostly | Stale items are confusing but rare for ad-hoc plugins |
| Everything in Phase 1 | ❌ No | Hot-reload is broken without these |
| Overlays/conceals/virtual text | ⚠️ Depends on plugin | If plugin re-clears its namespaces on init (common pattern), it self-heals. But if it doesn't, visual corruption persists. |

## Future Enhancements

1. **"Save and Load Plugin"** variant that saves the buffer first, then loads via the file-based path (enabling import resolution)
2. **Plugin API exposure** (`editor.loadPluginFromSource()`) for programmatic use
3. **Auto-reload on save** — watch the buffer for saves and auto-reload the plugin
4. **Plugin REPL** — evaluate selected text as plugin code (even more ad-hoc)
5. **Error overlay** — show transpile/runtime errors inline in the buffer
