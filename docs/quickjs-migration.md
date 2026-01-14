# QuickJS Plugin Backend

## Overview

Fresh uses QuickJS for its JavaScript plugin runtime, replacing the previous deno_core (V8) backend.

**Benefits:**
- Reduced dependencies (~315 → ~183 crates)
- Faster compilation (no V8 snapshot generation)
- Lighter runtime (~700KB vs multi-MB V8)
- Simple single backend (QuickJS + oxc)

## Status: In Progress

| Component | Status |
|-----------|--------|
| QuickJS runtime (rquickjs 0.11) | ✅ Complete |
| TypeScript transpilation (oxc 0.108) | ✅ Complete |
| ES module bundling | ✅ Complete |
| Proc macro TypeScript generation | ✅ Complete |
| ts-rs type export integration | ✅ Complete |
| Plugin API methods | ⚠️ 75/122 methods (61%) |
| Async operations (tokio) | ✅ Complete |

**Test coverage:** 52 unit tests + 23 e2e tests passing

## Architecture

### Class-Based API

The plugin API is exposed via `JsEditorApi` using rquickjs class bindings with automatic camelCase conversion.

**Key patterns:**
- `#[rquickjs::class]` - Expose struct to JS
- `#[rquickjs::methods(rename_all = "camelCase")]` - Auto-convert method names
- `rquickjs::function::Opt<T>` - Optional parameters
- `rquickjs::function::Rest<T>` - Variadic arguments
- `rquickjs_serde::to_value()` - Rust → JS conversion

### Async Pattern

Async methods use a callback-based pattern:
1. JS calls `_xxxStart()` → returns callbackId
2. Rust sends `PluginCommand` to app
3. App executes operation, calls `resolve_callback(id, result)`
4. JS Promise resolves

## File Structure

```
src/services/plugins/
├── backend/quickjs_backend.rs  # JsEditorApi implementation
├── api.rs                      # PluginCommand, EditorStateSnapshot
├── transpile.rs                # TypeScript → JS
└── thread.rs                   # Plugin thread runner

crates/fresh-plugin-api-macros/ # TypeScript definition generation
plugins/lib/fresh.d.ts          # Generated TypeScript definitions
```

## Dependencies

- `rquickjs` 0.11 - QuickJS bindings
- `rquickjs-serde` 0.4 - Serde integration
- `oxc_*` 0.108 - TypeScript transpilation
- `fresh-plugin-api-macros` - Proc macros

## Next Steps: API Completion

### Missing Methods (47 methods)

The following methods need to be added to `JsEditorApi` in `quickjs_backend.rs`:

**Priority 1 - Core Functionality:**
- `getBufferInfo(buffer_id)` → BufferInfo | null
- `getPrimaryCursor()` → CursorInfo | null
- `getAllCursors()` → CursorInfo[]
- `getViewport()` → ViewportInfo | null
- `getCursorLine()` → number
- `getAllCursorPositions()` → number[]
- `findBufferByPath(path)` → number
- `getBufferSavedDiff(buffer_id)` → TsBufferSavedDiff | null

**Priority 2 - Virtual Text/Overlays:**
- `addVirtualLine(buffer_id, position, text, fg_rgb, bg_rgb, above, namespace, priority)`
- `addVirtualText(buffer_id, id, position, text, rgb, before, use_bg)`
- `removeVirtualText(buffer_id, id)`
- `removeVirtualTextsByPrefix(buffer_id, prefix)`
- `clearVirtualTexts(buffer_id)`
- `clearVirtualTextNamespace(buffer_id, namespace)`
- `removeOverlay(buffer_id, handle)`
- `clearOverlaysInRange(buffer_id, start, end)`

**Priority 3 - View Transforms:**
- `submitViewTransform(buffer_id, split_id, start, end, tokens, layout_hints)`
- `clearViewTransform(buffer_id, split_id)`

**Priority 4 - Composite Buffers:**
- `createCompositeBuffer(options)` → Promise<number>
- `updateCompositeAlignment(buffer_id, hunks)`
- `closeCompositeBuffer(buffer_id)`

**Priority 5 - Scroll Sync:**
- `createScrollSyncGroup(group_id, left_split, right_split)`
- `setScrollSyncAnchors(group_id, anchors)`
- `removeScrollSyncGroup(group_id)`

**Priority 6 - Split Operations:**
- `createVirtualBufferInExistingSplit(options)` → Promise<number>
- `setSplitScroll(split_id, top_byte)`
- `setSplitRatio(split_id, ratio)`
- `distributeSplitsEvenly()`
- `setLineNumbers(buffer_id, enabled)`

**Priority 7 - File Explorer:**
- `setFileExplorerDecorations(namespace, decorations)`
- `clearFileExplorerDecorations(namespace)`

**Priority 8 - Diagnostics/LSP:**
- `getAllDiagnostics()` → TsDiagnostic[]
- `getHighlights(buffer_id, start, end)` → Promise<TsHighlightSpan[]>
- `disableLspForLanguage(language)`

**Priority 9 - Process Management:**
- `isProcessRunning(process_id)` → boolean
- `spawnProcessWait(process_id)` → Promise<SpawnResult>
- `killProcess(process_id)` → Promise<boolean>

**Priority 10 - UI:**
- `showActionPopup(options)` → Promise<ActionPopupResult>
- `deleteTheme(name)` → Promise<void>

**Priority 11 - Misc:**
- `executeActions(actions)` → boolean (batch execution)
- `getHandlers(event_name)` → string[]
- `pluginTranslate(plugin_name, key, args)` → string
- `fileStat(path)` → FileStat

### Missing Types (27 types)

Add to `api.rs` with `#[derive(TS)]` and register in `ts_export.rs`:

**Core:** BufferInfo, CursorInfo, ViewportInfo, SelectionRange, LayoutHints
**Virtual Buffers:** CreateVirtualBufferOptions, CreateVirtualBufferInCurrentSplitOptions, CreateVirtualBufferInExistingSplitOptions, CreateVirtualBufferResult, TextPropertyEntry
**Composite:** TsCompositeLayoutConfig, TsCompositeSourceConfig, TsCompositePaneStyle, TsCompositeHunk, CreateCompositeBufferOptions
**View Transform:** ViewTokenWire, ViewTokenWireKind
**Diagnostics:** TsDiagnostic, TsDiagnosticPosition, TsDiagnosticRange, TsBufferSavedDiff
**UI:** TsActionPopupAction, TsActionPopupOptions, PromptSuggestion, ActionSpecJs
**File System:** DirEntry, FileStat, FileExplorerDecoration

### Signature Fixes

These methods exist but have incompatible signatures:

1. **addOverlay** - Fix parameter order (underline should come after bg colors)
2. **defineMode** - Add `read_only` parameter
3. **openFile** - Make line/column non-nullable (use 0 for default)
4. **t()** - Change from variadic to `args?: Record<string, string>`
5. **registerCommand** - Match origin/master signature
6. **setPromptSuggestions** - Use typed `PromptSuggestion[]`
7. **setVirtualBufferContent** - Use typed `TextPropertyEntry[]`
8. **createVirtualBuffer/InSplit** - Use typed options objects
9. **spawnProcess/spawnBackgroundProcess** - Make args optional
10. **sendLspRequest** - Make params optional with `unknown | null`

## Future: Native Async

rquickjs supports native async via `AsyncRuntime`/`AsyncContext` and the `Promised` wrapper. This could replace the `_xxxStart` + JS wrapper pattern but would require architectural changes. The current callback-based pattern works well.

## References

- [rquickjs docs](https://docs.rs/rquickjs/)
- [QuickJS engine](https://bellard.org/quickjs/)
- [oxc project](https://oxc-project.github.io/)
