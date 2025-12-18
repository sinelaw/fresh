# Fresh Architecture

Fresh is a high-performance terminal-based text editor written in Rust. This document describes the
runtime structure and the core “flow” concepts: event loop, input handling, actions vs events,
state ownership, rendering, and plugins.

## Runtime Model

Fresh runs a synchronous main thread and communicates with background workers:
- **Main thread:** terminal input, frame loop, state mutation, rendering.
- **Tokio runtime / background tasks:** LSP, file I/O, terminal PTY I/O, directory refresh.
- **Plugin thread (TypeScript runtime):** executes hooks/actions and sends `PluginCommand`s back to
  the editor.

Key entrypoint: `src/main.rs`

## Main Event Loop

The main loop is a fixed-timestep-ish render loop (~60fps target) that interleaves:
1. Drain async work/results (`Editor::process_async_messages`)
2. Time-based checks (hover timers, warning log, auto-save, polling file changes)
3. Render when needed (`Editor::render`)
4. Poll terminal input (keyboard/mouse/resize)

Key file: `src/main.rs`

## Input Handling

### Terminal Input Events

The terminal produces `crossterm::event::Event` values:
- key press events
- mouse events (click/drag/move/scroll)
- resize events

The main loop routes these into `Editor`:
- keys → `Editor::handle_key` (`src/app/input.rs`)
- mouse → `Editor::handle_mouse` (`src/app/mouse_input.rs`)

### Modal Dispatch (Settings/Menu/Prompt/Popup)

Keyboard input has a strict priority order for “modal” UI:
1. Settings
2. Menu
3. Prompt (including file browser prompts)
4. Popup
5. Normal/FileExplorer/Terminal contexts

Modal components implement a small hierarchical `InputHandler` trait and return deferred work
(`DeferredAction`) that the `Editor` executes after dispatch.

Key files:
- Dispatch glue: `src/app/input_dispatch.rs`
- Input handler primitives: `src/input/handler.rs`
- Prompt: `src/view/prompt_input.rs`
- Menu: `src/view/ui/menu_input.rs`
- Popup: `src/view/popup_input.rs`

### Key Contexts and Keybindings

When no modal consumes input, keys resolve to an editor `Action`:
- `KeyContext` determines which keymap applies (global vs normal vs prompt vs popup, etc.)
- chord sequences are supported (multi-key bindings)
- context fallthrough is limited to “application-wide” actions to prevent leakage from modals

Key file: `src/input/keybindings.rs`

## Actions vs Events (Core Concepts)

Fresh has two distinct layers that are easy to conflate:

### `Action` (Intent)

`crate::input::keybindings::Action` is the “what the user wants” layer:
- examples: `Save`, `CommandPalette`, `MoveLeft`, `InsertChar('a')`, `LspHover`, `PluginAction(...)`
- produced by keybindings, menus, command palette, and some UI handlers

Execution entrypoint: `Editor::handle_action` (`src/app/input.rs`)

### `Event` (State Change + Undo/Redo)

`crate::model::event::Event` is the event-sourced “what changed” layer for undoable mutations:
- examples: `Insert`, `Delete`, `MoveCursor`, `AddCursor`, `Batch`, plus some view events
- stored in a per-buffer `EventLog` for undo/redo and “modified since saved” tracking

Key files:
- Event definitions + event log: `src/model/event.rs`
- Undo/redo application: `src/app/undo_actions.rs`

### Action → Event Conversion

Many editing/navigation actions convert into one or more `Event`s via:
- `src/input/actions.rs` (pure conversion logic)
- `Editor::action_to_events` (`src/app/render.rs`) as a convenience wrapper

Multi-cursor edits typically become `Event::Batch` so undo is atomic.

### Centralized Event Application

All undoable buffer mutations should go through:
- `Editor::apply_event_to_active_buffer` (`src/app/mod.rs`)

This method centralizes cross-cutting concerns:
- apply to `EditorState`
- sync cursor state into active split view state
- invalidate layouts for splits viewing the buffer
- adjust cursors in other splits viewing the same buffer
- clear/update search highlights appropriately
- fire plugin hooks for edits
- send LSP change notifications using pre-computed positions

Key file: `src/app/mod.rs`

## State Ownership: Buffer vs View

Fresh separates shared buffer state from per-split view state:

### Buffer State (shared per buffer)

`EditorState` owns “the document” and content-anchored decorations:
- text buffer
- cursors (authoritative positions)
- overlays, margins, virtual text
- syntax/semantic highlighting caches

Key file: `src/state.rs`

### View State (per split)

`SplitViewState` owns “how it’s displayed in this split”:
- viewport (scroll position, wrap mode, dimensions)
- a copy of cursors for hit testing / render bookkeeping
- optional `view_transform` payload (plugin-provided token stream)
- compose settings (width, column guides)

Key file: `src/view/split.rs`

View-only events (scrolling, recentering, set viewport) are applied at the `Editor`/split layer;
buffer events (insert/delete/etc.) are applied to `EditorState`.

## Async Messages (LSP, Plugins, File Watching, Terminals)

Every main-loop iteration drains async results via:
- `Editor::process_async_messages` (`src/app/mod.rs`)

This processes:
- LSP results/diagnostics (via the async bridge)
- plugin commands (`PluginCommand`) from the plugin thread
- terminal output/exits, file-open directory loads, file tree refresh, etc.

Key files:
- Message handling: `src/app/async_messages.rs`
- LSP handlers: `src/app/async_messages.rs` and `src/app/lsp_actions.rs`

## Rendering Pipeline (Overview)

Rendering is designed to preserve source-byte → screen-cell mappings for cursors and hit testing:
1. Determine viewport per split (scroll + size)
2. Build base tokens for visible bytes
3. (Optional) apply per-split `view_transform` tokens if present
4. Generate view lines + mappings
5. Apply styling layers (syntax/semantic, selection, overlays, etc.)
6. Emit ratatui widgets

Key files:
- High-level render + hook emission: `src/app/render.rs`
- Split rendering and tokenization: `src/view/ui/split_rendering.rs`
- View line generation: `src/view/ui/view_pipeline.rs`

## Plugins (Updated Timing Model)

Plugins run on a separate thread. The editor interacts with plugins through:
- **Hooks:** `plugin_manager.run_hook(...)` queues work to the plugin thread (non-blocking).
- **Commands:** plugins send `PluginCommand`s back to the editor, which are applied when the main
  thread drains them during `process_async_messages()`.

Implications:
- Hooks do not block rendering.
- Effects like `SubmitViewTransform`, overlays, virtual text, etc. may become visible on a later
  frame (typically the next frame).

Key files:
- Plugin manager facade: `src/services/plugins/manager.rs`
- Plugin thread interface: `src/services/plugins/thread.rs`
- Hook definitions: `src/services/plugins/hooks.rs`
- Plugin command handling: `src/app/mod.rs`, `src/app/plugin_commands.rs`, `src/app/async_messages.rs`

