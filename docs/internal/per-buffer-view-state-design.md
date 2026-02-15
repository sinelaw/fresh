# Design: Keyed Per-Buffer Per-View State

## Status

Core architecture: **Implemented** (February 14-15, 2026)
Plugin API & migration: **Not started**

## What Was Done

The core per-buffer-per-view state system is fully in place:

- **`BufferViewState` struct** (`view/split.rs`) — holds `cursors`, `viewport`, `view_mode`, `compose_width`, `compose_column_guides`, `view_transform`, and related fields per buffer per split.
- **`SplitViewState` refactored** — buffer-specific fields replaced with `active_buffer: BufferId` and `keyed_states: HashMap<BufferId, BufferViewState>`. Accessors `active_state()` / `active_state_mut()` plus helpers (`buffer_state`, `ensure_buffer_state`, `remove_buffer_state`, `switch_buffer`). `Deref`/`DerefMut` impls proxy to the active state for backward compatibility.
- **Cursor migration** — `EditorState.cursors` removed. Cursors live exclusively in `BufferViewState`; editing operations access them through the split view state. All cursor sync calls (`save_current_split_view_state` / `restore_current_split_view_state`) eliminated.
- **`ComposeState` removed from `EditorState`** — `view_mode`, `compose_width`, and related fields now live only in `BufferViewState`.
- **Workspace persistence** — `workspace.rs` stores per-file state (`view_mode`, `compose_width`, cursor, scroll) in `file_states: HashMap<PathBuf, SerializedFileState>` within each `SerializedSplitViewState`.

## Remaining Work

### 1. Plugin state API

Expose `setViewState` / `getViewState` methods to the JS plugin runtime so plugins can store per-buffer-per-split state that persists across sessions.

```typescript
// Proposed additions to EditorAPI
editor.setViewState(bufferId: number, key: string, value: unknown): void
editor.getViewState(bufferId: number, key: string): unknown | undefined
```

Rust side: add a `plugin_state: HashMap<String, serde_json::Value>` field to `BufferViewState` and wire up host function handlers.

### 2. Migrate `markdown_compose` plugin

The plugin currently uses global JS-side structures that don't persist and aren't per-split:

- `composeBuffers: Set<number>` — tracks which buffers are in compose mode
- `tableColumnWidths: Map<number, Map<number, TableWidthInfo>>` — caches table column widths per buffer

Migration steps:
- Replace `composeBuffers` Set with `editor.getViewState(bufferId, "compose-active")` checks — compose activation is already tracked by `view_mode` in `BufferViewState`, so this Set may simply be removed.
- Replace `tableColumnWidths` Map with `editor.setViewState(bufferId, "table-widths", ...)` so widths persist across sessions and are independent per split.
- Remove the global state variables.

### 3. Plugin state persistence

Extend `SerializedFileState` in `workspace.rs` to include the `plugin_state` map so plugin-managed state round-trips through save/restore. Add a `buffer_view_restored` hook so plugins can react to restored state.

### 4. Content-dependent state invalidation

When a buffer is edited from one split, other splits viewing the same buffer should invalidate content-dependent plugin state (e.g., `tableColumnWidths`). Plugins can listen for `buffer_changed` events and mark cached state stale. This needs implementation and testing.

## Open Questions

- **Undo interaction**: View mode changes are not undoable (undo only covers buffer content). Recommendation: keep it this way, revisit if users report it as a problem.

- **Same buffer, multiple splits**: Content is shared but view state is per-split. The `buffer_changed` hook for invalidating content-dependent plugin state needs testing once the plugin API is in place.
