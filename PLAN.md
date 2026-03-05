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

## Future Enhancements

1. **"Save and Load Plugin"** variant that saves the buffer first, then loads via the file-based path (enabling import resolution)
2. **Plugin API exposure** (`editor.loadPluginFromSource()`) for programmatic use
3. **Auto-reload on save** — watch the buffer for saves and auto-reload the plugin
4. **Plugin REPL** — evaluate selected text as plugin code (even more ad-hoc)
5. **Error overlay** — show transpile/runtime errors inline in the buffer
