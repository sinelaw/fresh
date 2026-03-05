# Design: LSP Support for Plugin Buffer Development

## Problem

When a user writes plugin code in a buffer (via "Load Plugin from Buffer"), they get **zero editor intelligence** — no autocomplete for `getEditor()`, `registerHandler()`, `EditorAPI` methods, no type checking, no hover docs. This makes the plugin development experience significantly worse than editing a `.ts` file in the plugins directory.

The core challenge: the Fresh plugin API is defined in `fresh.d.ts` (1,368 lines, auto-generated from Rust), and an LSP server needs to see both this type definition and the buffer contents to provide completions. But the buffer may be unsaved, unnamed, or ephemeral.

## Current State

### How LSP works today in Fresh

- `LspManager` manages one LSP server per language (e.g., `typescript-language-server --stdio` for TypeScript)
- `with_lsp_for_buffer()` is the central helper — it requires `metadata.file_uri()` to return `Some(Uri)`
- **Virtual buffers and unnamed buffers return `None` for `file_uri()`**, so LSP is completely disabled for them
- `BufferMetadata::new_unnamed()` explicitly sets `lsp_enabled: false`
- LSP URIs are always `file://` scheme — no support for `untitled:` or virtual document schemes

### How fresh.d.ts is available today

- Lives at `crates/fresh-editor/plugins/lib/fresh.d.ts`
- Plugins reference it via `/// <reference path="./lib/fresh.d.ts" />`
- When `embed-plugins` feature is on, entire `plugins/` dir (including `lib/fresh.d.ts`) is compiled into the binary via `include_dir!()` and extracted to `~/.cache/fresh/embedded-plugins/{hash}/`
- So `fresh.d.ts` IS embedded in the binary and extractable at runtime

### How LoadPluginFromBuffer works today

- Reads buffer content as a string
- Derives plugin name from buffer filename (or "buffer-plugin" if unnamed)
- Sends source to QuickJS runtime for transpilation + execution
- No LSP integration whatsoever

## Design Alternatives

### Alternative A: Write temp file + point TS LSP at it (Recommended)

**Approach**: When a buffer is being used for plugin development, write its content to a temporary `.ts` file alongside a `tsconfig.json` that includes `fresh.d.ts`. Point the LSP at this temp file.

**Mechanics**:
1. Create a temp directory: `~/.cache/fresh/plugin-dev/{session}/`
2. Write `fresh.d.ts` there (copy from embedded plugins cache or from source)
3. Write a `tsconfig.json`:
   ```json
   {
     "compilerOptions": {
       "target": "ES2020",
       "module": "ES2020",
       "strict": true,
       "noEmit": true,
       "lib": ["ES2020"]
     },
     "files": ["plugin.ts", "fresh.d.ts"]
   }
   ```
4. Write buffer contents to `plugin.ts` in that directory
5. Give the buffer a `file://` URI pointing to this temp file
6. On every buffer change (`didChange`), also update the temp file (or rely on LSP's in-memory document sync)

**Pros**:
- Works with standard `typescript-language-server` — no custom LSP needed
- `tsconfig.json` can be precisely configured for the plugin environment (no DOM, correct target, etc.)
- `fresh.d.ts` is naturally discoverable by the TS compiler
- Works for unnamed/unsaved buffers — the temp file acts as backing store
- Buffer can still be saved to a "real" path later without disruption

**Cons**:
- Requires writing files to disk (but only to a cache dir)
- Need to sync buffer changes to the temp file (but LSP `didChange` already handles in-memory; the temp file is only needed for initial `didOpen`)
- Need cleanup on buffer close / editor exit
- Two-file indirection (buffer content ≠ file on disk) could confuse "go to definition" results

**Important details**:
- The temp `tsconfig.json` must NOT include `"dom"` lib since plugins run in QuickJS, not a browser
- Should include `"skipLibCheck": true` to avoid checking `fresh.d.ts` itself
- The `fresh.d.ts` uses `declare function getEditor(): EditorAPI` at global scope — this is exactly what plugins see at runtime, so type checking will be accurate
- `typescript-language-server` respects `tsconfig.json` found in the file's directory hierarchy

### Alternative B: Virtual document scheme + custom LSP middleware

**Approach**: Use `untitled:` URI scheme for the buffer. Implement a middleware or wrapper around `typescript-language-server` that intercepts `textDocument/didOpen` for `untitled:` URIs and injects `fresh.d.ts` types.

**Mechanics**:
1. Assign buffer a URI like `untitled:buffer-plugin.ts`
2. Create a custom LSP proxy that:
   - Forwards most requests to `typescript-language-server`
   - On `didOpen` for `untitled:` docs, also opens a virtual `fresh.d.ts`
   - Maps workspace to a virtual project with the right `tsconfig`

**Pros**:
- No temp files on disk
- Cleaner URI semantics

**Cons**:
- `typescript-language-server` has limited support for `untitled:` URIs — it needs a workspace root to find `tsconfig.json` and `node_modules`
- Would require building an LSP proxy in Rust, which is significant complexity
- Non-standard behavior — debugging LSP issues becomes much harder
- `untitled:` documents can't resolve relative imports or type references

**Verdict**: Too complex for the benefit. LSP servers are designed around the filesystem.

### Alternative C: Deno LSP instead of typescript-language-server

**Approach**: Use `deno lsp` which has built-in TypeScript support and can work with virtual documents.

**Mechanics**:
1. Start `deno lsp` with `--unstable` flag
2. Configure initialization options to enable TypeScript
3. Use Deno's support for `deno:` or data URIs for type injection

**Pros**:
- Deno LSP is a single binary with built-in TypeScript compiler
- No need for `npm install -g typescript-language-server typescript`
- Better support for standalone scripts (no `node_modules` needed)

**Cons**:
- Requires Deno to be installed (Fresh is a Rust editor, not a Deno project)
- Deno LSP has different module resolution semantics (URL imports, import maps)
- Getting `fresh.d.ts` types visible requires Deno-specific configuration (`deno.json` with `compilerOptions.types`)
- May confuse the "regular" TypeScript LSP if user also has TS/JS project files open
- Deno LSP initialization is different from standard `typescript-language-server`

**Verdict**: Introducing a Deno dependency for plugin development is heavy-handed. Could be offered as an alternative configuration, but shouldn't be the default.

### Alternative D: Embedded TypeScript service (in-process)

**Approach**: Bundle a TypeScript type-checker (or a subset) directly into Fresh's Rust binary, avoiding external LSP servers entirely.

**Pros**:
- Zero external dependencies
- Instant startup, no process management
- Full control over the type-checking environment

**Cons**:
- Enormous engineering effort — TypeScript's type system is complex
- Would need to re-implement or bind to TypeScript's compiler API
- Ongoing maintenance burden as TypeScript evolves
- Binary size increase

**Verdict**: Not practical.

## Recommended Approach: Alternative A (temp file + tsconfig)

### Implementation Plan

#### Phase 1: Infrastructure — temp workspace for plugin buffers

1. **Create `PluginDevWorkspace` struct** in `fresh-editor/src/services/plugins/`:
   ```rust
   pub struct PluginDevWorkspace {
       /// Path to the temp directory
       dir: PathBuf,
       /// Path to the temp plugin.ts file
       plugin_file: PathBuf,
       /// Whether fresh.d.ts has been written
       types_ready: bool,
   }
   ```

2. **On "Load Plugin from Buffer" or explicit "Enable Plugin LSP" action**:
   - Create `~/.cache/fresh/plugin-dev/` directory
   - Copy `fresh.d.ts` from embedded plugins dir (already extracted at `~/.cache/fresh/embedded-plugins/{hash}/lib/fresh.d.ts`)
   - Write `tsconfig.json` with plugin-appropriate settings
   - Write buffer content to `plugin.ts`

3. **Update buffer metadata**:
   - Set `kind` to `BufferKind::File { path: temp_plugin_path, uri: file_uri }`
   - Or: add a new variant `BufferKind::PluginDev { backing_file, original_name }` to track the association
   - Set `lsp_enabled: true`
   - This makes `file_uri()` return `Some(...)`, enabling the entire LSP pipeline

#### Phase 2: LSP activation for plugin buffers

4. **Ensure TypeScript LSP auto-starts for plugin buffers**:
   - The default config has `auto_start: false` for TypeScript LSP
   - When a plugin dev workspace is created, call `lsp.allow_language("typescript")` to enable it
   - The existing `with_lsp_for_buffer()` → `try_spawn()` flow will then work normally

5. **Buffer change sync**:
   - The existing `didChange` notifications already handle in-memory updates to the LSP
   - The temp file on disk is only needed for initial project discovery
   - No need to write every keystroke to disk — LSP works with in-memory document state after `didOpen`

#### Phase 3: Lifecycle management

6. **Cleanup on buffer close**:
   - When the plugin buffer is closed, clean up the temp directory
   - Remove from `PluginDevWorkspace` tracking

7. **Cleanup on editor exit**:
   - Delete all `~/.cache/fresh/plugin-dev/` contents
   - Or: use session-specific subdirectories that are cleaned up

#### Phase 4: UX Polish

8. **Auto-detection**: When a buffer's content starts with `/// <reference path` or calls `getEditor()`, offer to enable plugin LSP mode
9. **Status bar indicator**: Show that plugin development LSP is active
10. **Command**: "Enable Plugin Development Mode" in command palette — activates LSP for current buffer

### Edge Cases

#### Unnamed / unsaved buffers
- The temp file approach handles this naturally — the buffer doesn't need a "real" path
- The plugin name is derived from the buffer display name ("buffer-plugin" by default)
- The temp file path acts as the LSP's view of the document

#### Multiple plugin buffers simultaneously
- Each gets its own temp directory: `~/.cache/fresh/plugin-dev/{buffer_id}/`
- Each has its own copy of `fresh.d.ts` and `tsconfig.json`
- LSP sees them as separate projects (different `tsconfig.json` roots)
- Alternatively, share a single workspace with multiple `.ts` files — but this could cause cross-contamination of types between plugins

#### User saves buffer to a real file
- If the user saves the buffer to e.g. `~/.config/fresh/plugins/my_plugin.ts`, the metadata should update to point to the real file
- The temp workspace can be cleaned up
- LSP continues working via the real file path (assuming `fresh.d.ts` is findable — it is, via `/// <reference path>`)

#### fresh.d.ts not available (non-embedded build)
- Fall back: check if the plugins directory is known and `lib/fresh.d.ts` exists there
- If not found, LSP still works but without Fresh API types — user gets completions for standard TS but not `getEditor()` etc.
- Show a warning: "Fresh API types not available for plugin development"

#### TypeScript LSP not installed
- The existing `typescript-lsp.ts` plugin already handles this case with a helpful popup
- No additional work needed — the same error handling applies

### What fresh.d.ts needs (if anything)

The current `fresh.d.ts` uses `declare function` at global scope, which is exactly how the QuickJS runtime exposes these APIs. This means:
- **No changes needed to fresh.d.ts** — it already correctly types the global plugin environment
- The `tsconfig.json` just needs to include it in `"files"` and it will provide global type augmentation
- `getEditor()`, `registerHandler()`, `ProcessHandle<T>`, `EditorAPI` — all correctly typed as globals

### tsconfig.json for plugin development

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ES2020",
    "moduleResolution": "node",
    "strict": true,
    "noEmit": true,
    "skipLibCheck": true,
    "lib": ["ES2020"],
    "types": []
  },
  "files": ["fresh.d.ts", "plugin.ts"]
}
```

Key decisions:
- **No `"dom"` lib**: Plugins run in QuickJS, not a browser — no `window`, `document`, `fetch`
- **`"types": []`**: Prevents picking up `@types/node` or other ambient types that don't exist in QuickJS
- **`"strict": true`**: Helps catch bugs in plugin code
- **`ES2020` target**: Matches QuickJS's capability level
- **`"skipLibCheck": true`**: Don't waste time checking `fresh.d.ts` itself
- **`"files"` not `"include"`**: Explicit file list prevents picking up stray `.ts` files

### Risks and Mitigations

| Risk | Mitigation |
|------|-----------|
| Temp file accumulation | Session-scoped dirs + cleanup on exit + periodic cache cleanup |
| LSP startup latency | TypeScript LSP is slow to initialize (~2-5s); show loading indicator |
| Stale type definitions | `fresh.d.ts` comes from the same binary that's running — always in sync |
| Disk space | Each workspace is ~50KB (fresh.d.ts + tsconfig.json); negligible |
| Cross-platform temp paths | Use `dirs::cache_dir()` (already used by embedded plugins) |
