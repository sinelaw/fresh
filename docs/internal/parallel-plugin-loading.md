# Parallel Plugin Loading & Plugin Dependencies

**Status:** Design Plan
**Scope:** `fresh-plugin-runtime`, `fresh-parser-js`, `fresh-core`, `fresh-editor`

## Problem

Fresh loads all plugins serially during startup. Each plugin goes through three
phases sequentially: file I/O → TypeScript transpilation → QuickJS execution.
With many plugins (20–80+), startup becomes noticeably slow. Additionally, there
is no way to declare inter-plugin dependencies, and collisions on shared
registries (commands, contexts, grammars) are silently accepted — last writer
wins — which creates subtle, order-dependent bugs.

## Goals

1. **Parallel I/O and transpilation** — read files and transpile TS→JS
   concurrently while keeping QuickJS execution serial.
2. **Plugin dependency declaration** — allow plugins to declare dependencies on
   other plugins using standard TypeScript `import` syntax.
3. **Deterministic, dependency-respecting execution order** — topological sort
   of plugins before serial execution.
4. **First-writer-wins semantics** — collisions on shared registries
   (`registerCommand`, `register_grammar`, `set_context`, etc.) must fail
   loudly rather than silently overwrite.
5. **Transpile caching** — avoid re-transpiling unchanged plugin files.

## Non-Goals

- Lazy/on-demand plugin activation (valuable but a separate effort).
- Running plugin JS execution in parallel (QuickJS is single-threaded by
  design, and plugins share mutable editor state).
- Plugin sandboxing / per-plugin QuickJS contexts.

---

## Architecture

### Current Flow (Serial)

```
for each plugin_dir:
    for each .ts/.js file (filesystem order):
        1. read_to_string(path)           # blocking I/O
        2. read i18n JSON                  # blocking I/O
        3. transpile_typescript(source)    # CPU-bound (oxc)
           or bundle_module(path)          # CPU-bound (oxc + dependency resolution)
        4. execute_js(js_code)             # QuickJS execution (side effects)
```

Total time: `N × (IO + transpile + exec)`.

### Proposed Flow (Two-Phase)

```
Phase 1 — Parallel Preparation (thread pool / rayon):
    discover all plugin files across all directories
    for each plugin IN PARALLEL:
        1. read_to_string(path)
        2. read i18n JSON
        3. parse imports to extract dependency metadata
        4. transpile/bundle to JS
        5. compute content hash, check transpile cache
    collect Vec<PreparedPlugin>

Phase 2 — Serial Execution (plugin thread, single-threaded QuickJS):
    topological sort by declared dependencies
    for each plugin in topo order:
        1. register i18n strings
        2. execute_js(prepared.js_code)
        3. register in plugins HashMap
```

Total time: `max(IO + transpile) + N × exec`.

### Data Structures

```rust
/// Result of Phase 1 for a single plugin.
struct PreparedPlugin {
    name: String,
    path: PathBuf,
    js_code: String,                    // transpiled/bundled JS, ready to execute
    i18n: Option<HashMap<String, HashMap<String, String>>>,
    dependencies: Vec<String>,          // extracted from import statements
    content_hash: u64,                  // for transpile cache invalidation
}
```

---

## Dependency Declaration via TypeScript Imports

### Syntax

Plugins declare dependencies using a `fresh:plugin/` import scheme:

```typescript
// my-plugin.ts
import type { SomeType } from "fresh:plugin/utility-plugin";
```

This is **parsed at transpile time** to extract `"utility-plugin"` as a
dependency. The import statement itself is **stripped** before execution (just
like existing export stripping). The actual runtime inter-plugin API uses
explicit editor methods:

```typescript
// utility-plugin.ts — exporting
editor.registerPluginExport("utility-plugin", {
    formatDate: (d: Date) => d.toISOString(),
});

// my-plugin.ts — importing (at runtime)
const utils = editor.getPluginExport("utility-plugin");
if (!utils) throw new Error("utility-plugin not loaded");
```

### Why `import type` + runtime API (not real ES imports)?

1. All plugins execute in the same QuickJS global scope — there are no real
   ES module boundaries at runtime.
2. `import type` is erased by the TypeScript transpiler, so it has zero runtime
   cost and zero bundling complexity.
3. The `fresh:plugin/` scheme is unambiguous — it cannot collide with local
   file imports (which use `./` relative paths).
4. TypeScript tooling (LSP, IDE) can resolve `fresh:plugin/*` via a
   `paths` entry in `tsconfig.json` or a generated `.d.ts`, giving full
   autocompletion and type checking.
5. Runtime `getPluginExport` makes the dependency explicit and allows for
   graceful handling (null check = soft dependency).

### Dependency Extraction

Extend `fresh-parser-js` to extract `fresh:plugin/*` imports:

```rust
/// Extract plugin dependency names from `import ... from "fresh:plugin/NAME"`.
pub fn extract_plugin_dependencies(source: &str) -> Vec<String> { ... }
```

This is a lightweight parse — regex or a single-pass line scan suffices since
the `fresh:plugin/` scheme is syntactically unambiguous. For robustness, reuse
the existing oxc AST parse that already happens during transpilation.

### Topological Sort

Use Kahn's algorithm on the dependency graph. On cycle detection, report the
full cycle path and **refuse to load any plugin in the cycle**:

```
Error: Plugin dependency cycle detected: A → B → C → A
  Plugins A, B, C will not be loaded.
```

Plugins with no dependency relationships maintain alphabetical order for
determinism (matching current behavior).

---

## First-Writer-Wins: Collision Detection on Shared Registries

### Problem

Currently, `CommandRegistry::register` (line 75 of `command_registry.rs`)
silently replaces existing commands:

```rust
// Current: silent overwrite
commands.retain(|c| c.name != command.name);
commands.push(command);
```

Similar silent-overwrite behavior exists for:
- `registered_actions` HashMap in `quickjs_backend.rs` (line 804)
- `set_context` (keybinding contexts)
- `register_grammar` / `register_language_config` / `register_lsp_server`

This makes plugin behavior dependent on load order — a bug source that becomes
worse with parallel preparation (where the order of Phase 1 completion is
non-deterministic).

### Design

**First-writer-wins**: the first plugin to register a name owns it. Subsequent
attempts to register the same name **fail with an exception** thrown back to
the calling plugin's JS context.

#### Command Registration

```rust
// command_registry.rs
pub fn try_register(&self, command: Command) -> Result<(), CommandCollisionError> {
    let mut commands = self.plugin_commands.write().unwrap();
    if commands.iter().any(|c| c.name == command.name) {
        return Err(CommandCollisionError {
            name: command.name,
            existing_plugin: commands.iter()
                .find(|c| c.name == command.name)
                .map(|c| c.source.clone()),
        });
    }
    commands.push(command);
    Ok(())
}
```

The existing `register` method is kept for internal use (built-in commands that
legitimately override) but the plugin-facing path goes through `try_register`.

#### Plugin-Side Error

In `quickjs_backend.rs`, `register_command` currently returns `bool`. Change it
to throw a JS exception on collision:

```rust
pub fn register_command<'js>(
    &self,
    ctx: rquickjs::Ctx<'js>,
    name: String,
    ...
) -> rquickjs::Result<bool> {
    // ... existing code ...
    match self.command_sender.send(PluginCommand::TryRegisterCommand { command, response_tx }) {
        Ok(()) => {
            match response_rx.recv() {
                Ok(Ok(())) => Ok(true),
                Ok(Err(collision)) => Err(ctx.throw(
                    rquickjs::String::from_str(ctx.clone(),
                        &format!("Command '{}' already registered by {}",
                            collision.name, collision.existing_plugin)
                    )?.into_value()
                )),
                Err(_) => Ok(false),
            }
        }
        Err(_) => Ok(false),
    }
}
```

#### Affected Registries

| Registry | Current behavior | New behavior |
|----------|-----------------|--------------|
| `registerCommand` | Silent replace | Throw exception |
| `registered_actions` (handler map) | Silent replace | Throw exception |
| `set_context` | Silent replace | Allowed (contexts are meant to be toggled by anyone) |
| `register_grammar` | Silent replace | Throw exception (first grammar for a scope wins) |
| `register_language_config` | Silent replace | Throw exception |
| `register_lsp_server` | Silent replace | Throw exception |

`set_context` is intentionally excluded — contexts are boolean flags that
multiple plugins may legitimately toggle (e.g., `"panel_visible"`).

#### Unregister + Re-register

A plugin that calls `unregisterCommand(name)` first and then
`registerCommand(name, ...)` should succeed. The unregister clears the
ownership, allowing re-registration. This supports hot-reload workflows.

---

## Transpile Cache

### Design

Store transpiled JS alongside a content hash in a cache directory:

```
~/.config/fresh/cache/plugins/
    <plugin-name>.<content-hash>.js
```

On startup:
1. Compute hash of `.ts` source (e.g., xxhash64).
2. Check if `<name>.<hash>.js` exists in cache.
3. If hit: read cached JS (skip transpilation).
4. If miss: transpile, write to cache, use result.

Cache entries are cheap (a few KB each). Prune entries older than 30 days on
startup.

### Integration with Parallel Prep

The cache check happens inside Phase 1 (parallel), so cache hits make that
phase nearly instant.

---

## Implementation Plan

### Milestone 1: First-Writer-Wins Collision Detection

**Files:**
- `crates/fresh-editor/src/input/command_registry.rs` — add `try_register`
- `crates/fresh-core/src/api.rs` — add `TryRegisterCommand` variant with
  response channel
- `crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs` — change
  `register_command` to use synchronous collision check, throw on failure
- `crates/fresh-editor/src/services/plugins/bridge.rs` — implement
  `try_register_command`

**Tests:**
- Unit test in `command_registry.rs`: register same command name twice →
  second call returns `Err(CommandCollisionError)`
- Unit test in `quickjs_backend.rs` (`test_api_register_command` area):
  two `registerCommand` calls with same name → second throws JS exception
- E2E test: two plugins both register `"My Command"` → editor starts, only
  first plugin's handler is active, second plugin logs error via
  `editor.debug()`
- E2E test: plugin A registers command, plugin B unregisters it, plugin B
  re-registers → succeeds

Repeat analogous tests for `register_grammar`, `register_language_config`,
`register_lsp_server`, and the `registered_actions` handler map.

### Milestone 2: Dependency Declaration & Topological Sort

**Files:**
- `crates/fresh-parser-js/src/lib.rs` — add `extract_plugin_dependencies()`
  function that parses `import ... from "fresh:plugin/NAME"`
- `crates/fresh-plugin-runtime/src/thread.rs` — integrate dependency
  extraction into plugin loading; add topological sort before execution
- `crates/fresh-core/src/api.rs` — add `dependencies: Vec<String>` to
  `PluginConfig` (informational, for UI/debugging)

**Tests:**
- Unit test for `extract_plugin_dependencies`: various import styles
  (`import type`, `import { X }`, `import * as`, multiline) correctly
  extract dependency names
- Unit test for topological sort: basic ordering, diamond dependencies,
  independent plugins retain alphabetical order
- Unit test for cycle detection: reports cycle path, affected plugins not
  loaded
- E2E test: plugin A depends on plugin B; plugin B registers an export;
  plugin A reads the export in `editor_initialized` → works correctly
- E2E test: plugin declares dependency on non-existent plugin → startup
  error, plugin not loaded, editor still starts
- E2E test: circular dependency between two plugins → both skipped with
  error, remaining plugins load fine

### Milestone 3: Parallel I/O and Transpilation

**Files:**
- `crates/fresh-plugin-runtime/src/thread.rs` — split
  `load_plugins_from_dir_with_config_internal` into Phase 1 (parallel
  preparation) and Phase 2 (serial execution)
- `crates/fresh-plugin-runtime/Cargo.toml` — add `rayon` dependency (or
  use `tokio::task::spawn_blocking` pool)
- `crates/fresh-parser-js/src/lib.rs` — ensure `transpile_typescript` and
  `bundle_module` are `Send` (no `Rc`, no thread-local state) so they can
  run on a thread pool

**Approach:**
- Use `rayon::par_iter` for Phase 1. The oxc allocator is per-invocation
  and does not share state, so parallel transpilation is safe.
- `bundle_module` does recursive file reads — each invocation builds its own
  `visited` set and module list, so it is safe to parallelize across
  different entry points. However, two plugins that both import the same
  local helper will each independently bundle it. This is fine — the
  bundled output is per-plugin and the duplication is in-memory only.
- Phase 2 receives `Vec<PreparedPlugin>` already sorted by the topo sort
  from Milestone 2.

**Tests:**
- Unit test: prepare N plugins in parallel, verify all PreparedPlugins
  have correct `js_code` and `dependencies`
- E2E test: 10+ test plugins loaded with parallel prep, verify all
  commands registered correctly and hooks fire
- E2E stress test: 50 trivial plugins → measure startup time, assert it
  is below a threshold (regression guard)
- Test: plugin with bundled local imports (multi-file plugin) prepared in
  parallel → executes correctly

### Milestone 4: Transpile Cache

**Files:**
- `crates/fresh-plugin-runtime/src/cache.rs` (new) — transpile cache logic
  (hash, read, write, prune)
- `crates/fresh-plugin-runtime/src/thread.rs` — integrate cache into Phase 1
- `crates/fresh-core/src/config.rs` — add `plugin_cache_dir` to
  `DirContext`

**Tests:**
- Unit test: cache miss → transpile → cache write → second load → cache hit
  (no transpile call)
- Unit test: modify plugin source → hash changes → cache miss → re-transpile
- Unit test: corrupted cache file → graceful fallback to transpile
- Unit test: prune removes entries older than threshold
- E2E test: start editor twice with same plugins → second startup measurably
  faster (or at minimum, cache files exist on disk)

### Milestone 5: Runtime Plugin Export API

**Files:**
- `crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs` — add
  `register_plugin_export` and `get_plugin_export` methods to `JsEditorApi`
- `crates/fresh-core/src/api.rs` — add corresponding `PluginCommand` variants
- Plugin TypeScript definitions auto-regenerated via the proc macro

**Tests:**
- Unit test: register export, get export → returns correct value
- Unit test: get export for unregistered plugin → returns null
- Unit test: register export twice from same plugin → replaces (allowed,
  it's the plugin's own namespace)
- Unit test: register export with name of another plugin → throw (namespace
  squatting prevention)
- E2E test: utility plugin exports a function, consumer plugin calls it →
  correct result

---

## Comparison to Other Editors

| Aspect | Fresh (proposed) | VS Code | Neovim (lazy.nvim) | Zed |
|--------|-----------------|---------|-------------------|-----|
| **Parallelism** | Parallel I/O+transpile, serial exec | Parallel (separate processes) | Serial (single Lua) | Parallel (WASM isolates) |
| **Dependency syntax** | `import type` from `fresh:plugin/*` | `extensionDependencies` in `package.json` | `dependencies = {}` in Lua config | None (isolated) |
| **Collision handling** | First-writer-wins, exception | Last-writer-wins (silent) | Varies by manager | N/A (isolated) |
| **Lazy loading** | Future work | Activation events | `event`, `cmd`, `ft` triggers | Language-based |
| **Isolation** | Shared context | Separate processes | Shared Lua state | WASM sandbox |
| **Cache** | Transpile cache | Built-in VSIX cache | Lockfile-based | WASM binary cache |

### Key Takeaways from Other Editors

- **VS Code's activation events** are the gold standard for lazy loading but
  require a manifest — we get a similar benefit with lower complexity by using
  `import` syntax for deps and deferring lazy activation to a future milestone.
- **Neovim's lazy.nvim** proves that serial execution with a dependency DAG
  and smart lazy loading can handle 80+ plugins with fast startup — the
  bottleneck is usually I/O and parse time, not execution.
- **Zed's WASM isolation** prevents collisions entirely but sacrifices
  inter-plugin communication — Fresh's shared-context model is more flexible
  but demands explicit collision detection (Milestone 1).

---

## Risks and Mitigations

| Risk | Mitigation |
|------|-----------|
| Parallel transpilation introduces non-determinism in execution order | Topo sort + alphabetical tiebreaker makes order fully deterministic regardless of Phase 1 completion order |
| `bundle_module` does recursive file reads — parallel invocations could hit filesystem contention | Unlikely bottleneck; OS-level page cache handles this. Monitor with benchmarks. |
| First-writer-wins breaks plugins that intentionally override commands | Provide `editor.overrideCommand()` as an explicit, opt-in override API for this use case. Document the change in release notes. |
| Dependency on non-existent plugin silently accepted | Fail loudly at startup: "Plugin X depends on Y, but Y is not installed" |
| Plugin export API introduces new shared mutable state | Exports are keyed by plugin name, and only the owning plugin can write to its own namespace — no cross-plugin mutation |
| Cache grows unbounded | Prune old entries on startup (30-day TTL). Cache dir is in config, user can clear it. |
