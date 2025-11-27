# Proposal for Source Code Reorganization

> **STATUS: COMPLETED** - This reorganization has been fully implemented. The `src/` directory now follows the structure outlined below. This document is retained for historical reference.

This document outlines a plan to reorganize the `src/` directory to better reflect the application's architecture. The goal is to make the codebase easier to navigate and understand for new and existing developers.

## Motivation

The current structure of the `src/` directory is mostly flat. While this is simple, it doesn't expose the logical layers of the application. Key architectural components like the core data model, the view layer, and asynchronous services are intermingled.

By grouping files into directories based on their role, we can make the architecture explicit, reduce cognitive overhead, and make it easier to find relevant code.

## Guiding Principle

The proposed reorganization is based on the architectural analysis of the codebase, which identifies several distinct layers:

1.  **Core/Orchestration**: The main application state and editor orchestration.
2.  **Pure Model**: The pure data representation of a document (no external dependencies).
3.  **View & UI**: The presentation layer responsible for rendering.
4.  **Input Pipeline**: The system that translates user input into commands.
5.  **Services**: Asynchronous peripherals that communicate with the outside world.
6.  **Primitives**: Low-level utilities used by other layers.

The new directory structure will mirror these layers.

## Architectural Analysis Results

After analyzing the actual module dependencies, several critical findings informed this plan:

### Key Findings

1. **`state.rs` is NOT a pure model** - It imports from view modules (overlay, viewport, virtual_text), primitives (highlighter, indent), and orchestrates the entire editor. It should stay at root level or be part of the app/ controller layer, NOT in model/.

2. **`multi_cursor.rs` has dependency inversion** - It imports `state::EditorState`, which is backwards for a "model" module. Models should not import from orchestrators. This file belongs in `input/` since it's about cursor manipulation actions.

3. **`event.rs` has view coupling** - Imports `overlay::{OverlayHandle, OverlayNamespace}`. This is acceptable since these are just identifiers, but noted for future cleanup.

4. **`lsp_diagnostics.rs` has mixed concerns** - It directly manipulates editor state and overlays. Ideally should be split, but for pragmatic reasons kept in services/ for now.

### Circular Dependency Risks

The following potential circular dependencies exist and must be carefully managed:
- `state.rs` ↔ `multi_cursor.rs` (FIXED by moving multi_cursor to input/)
- View modules ↔ State (acceptable - state is the orchestrator)

## Proposed Directory Structure

```
src/
├── app/                  // The main application controller and entry point
│   ├── mod.rs            // Formerly editor/mod.rs - the Editor struct
│   ├── input.rs
│   ├── render.rs
│   ├── file_explorer.rs
│   ├── types.rs
│   └── script_control.rs // Script control mode for programmatic interaction
├── model/                // PURE data model for a "document" (minimal dependencies)
│   ├── mod.rs
│   ├── buffer.rs         // Formerly text_buffer.rs - pure data structure
│   ├── piece_tree.rs     // Pure data structure
│   ├── cursor.rs         // Pure cursor data
│   ├── edit.rs           // Edit operations in buffer history
│   ├── event.rs          // Core event types
│   ├── marker.rs         // Pure marker data
│   ├── marker_tree.rs    // Pure marker tree structure
│   ├── control_event.rs  // Observable notifications
│   └── document_model.rs // Document abstraction trait
├── view/                 // UI components and rendering logic
│   ├── mod.rs
│   ├── split.rs
│   ├── viewport.rs
│   ├── popup.rs
│   ├── prompt.rs
│   ├── overlay.rs
│   ├── virtual_text.rs
│   ├── margin.rs
│   ├── theme.rs
│   ├── stream.rs         // Formerly view.rs - View stream representation
│   ├── file_tree/        // File tree navigation component
│   └── ui/               // Existing ui/ directory
│       ├── mod.rs
│       ├── menu.rs
│       ├── status_bar.rs
│       ├── suggestions.rs
│       ├── file_explorer.rs
│       ├── split_rendering.rs
│       ├── tabs.rs
│       └── view_pipeline.rs
├── input/                // The input-to-action-to-event pipeline
│   ├── mod.rs
│   ├── actions.rs
│   ├── commands.rs
│   ├── keybindings.rs
│   ├── command_registry.rs
│   ├── input_history.rs
│   ├── position_history.rs
│   ├── buffer_mode.rs    // Buffer mode system for buffer-local keybindings
│   └── multi_cursor.rs   // Cursor manipulation actions (moved from model/)
├── services/             // Asynchronous peripherals (LSP, plugins, FS)
│   ├── mod.rs
│   ├── lsp/
│   │   ├── mod.rs
│   │   ├── manager.rs    // Formerly lsp_manager.rs
│   │   ├── client.rs     // Formerly lsp.rs
│   │   ├── async_handler.rs // Formerly lsp_async.rs
│   │   └── diagnostics.rs // Formerly lsp_diagnostics.rs
│   ├── plugins/
│   │   ├── mod.rs
│   │   ├── thread.rs     // Formerly plugin_thread.rs
│   │   ├── api.rs        // Formerly plugin_api.rs
│   │   ├── process.rs    // Formerly plugin_process.rs
│   │   ├── runtime.rs    // Formerly ts_runtime.rs
│   │   ├── hooks.rs      // Hook system for plugins
│   │   └── event_hooks.rs // Event-to-hook mapping
│   ├── fs/               // Existing fs/ directory
│   ├── async_bridge.rs
│   ├── clipboard.rs
│   ├── signal_handler.rs
│   └── process_limits.rs // Process resource limiting
├── primitives/           // Low-level syntax and rendering utilities
│   ├── mod.rs
│   ├── highlighter.rs
│   ├── semantic_highlight.rs
│   ├── ansi.rs
│   ├── ansi_background.rs
│   ├── indent.rs
│   ├── text_property.rs
│   ├── word_navigation.rs
│   ├── line_wrapping.rs
│   └── line_iterator.rs
├── state.rs              // Main editor state orchestrator (stays at root)
├── config.rs             // Configuration (stays at root)
├── lib.rs                // Crate root, re-exporting main components
└── main.rs               // Application entry point
```

## Reorganization Plan

This reorganization can be done incrementally.

### Phase 1: Create New Core Directories

Create the following new directories in `src/`:
- `src/app/`
- `src/model/`
- `src/view/`
- `src/input/`
- `src/services/`
- `src/primitives/`

### Phase 2: Relocate the Pure Model

Move the files defining PURE document data into `src/model/`. These files should have minimal external dependencies.

- **Move:**
  - `text_buffer.rs` -> `model/buffer.rs`
  - `piece_tree.rs` -> `model/piece_tree.rs`
  - `cursor.rs` -> `model/cursor.rs`
  - `marker.rs` -> `model/marker.rs`
  - `marker_tree.rs` -> `model/marker_tree.rs`
  - `event.rs` -> `model/event.rs`
  - `edit.rs` -> `model/edit.rs`
  - `control_event.rs` -> `model/control_event.rs`
  - `document_model.rs` -> `model/document_model.rs`
- **Keep at root:** `state.rs` (it's the main orchestrator, not a pure model)
- **Action:** Create `src/model/mod.rs` to expose the public structs and update `use` paths across the codebase.

### Phase 3: Consolidate the Controller

Rename `src/editor/` to `src/app/` to better reflect its role as the central application controller.

- **Move:**
  - `editor/mod.rs` -> `app/mod.rs`
  - `editor/input.rs` -> `app/input.rs`
  - `editor/render.rs` -> `app/render.rs`
  - `editor/file_explorer.rs` -> `app/file_explorer.rs`
  - `editor/types.rs` -> `app/types.rs`
  - `script_control.rs` -> `app/script_control.rs`
- **Action:** Update `use` paths. The `Editor` struct will now be `crate::app::Editor`.

### Phase 4: Relocate the Input Pipeline

Move the input handling chain into `src/input/`.

- **Move:**
  - `actions.rs` -> `input/actions.rs`
  - `commands.rs` -> `input/commands.rs`
  - `keybindings.rs` -> `input/keybindings.rs`
  - `command_registry.rs` -> `input/command_registry.rs`
  - `input_history.rs` -> `input/input_history.rs`
  - `position_history.rs` -> `input/position_history.rs`
  - `buffer_mode.rs` -> `input/buffer_mode.rs`
  - `multi_cursor.rs` -> `input/multi_cursor.rs` (moved here due to state dependency)
- **Action:** Create `src/input/mod.rs` and update `use` paths.

### Phase 5: Relocate the View & UI Layer

Group all components responsible for presentation into `src/view/`.

- **Move:**
  - `ui/` -> `view/ui/` (includes menu.rs, status_bar.rs, suggestions.rs, file_explorer.rs, split_rendering.rs, tabs.rs, view_pipeline.rs)
  - `split.rs` -> `view/split.rs`
  - `viewport.rs` -> `view/viewport.rs`
  - `popup.rs` -> `view/popup.rs`
  - `prompt.rs` -> `view/prompt.rs`
  - `overlay.rs` -> `view/overlay.rs`
  - `virtual_text.rs` -> `view/virtual_text.rs`
  - `margin.rs` -> `view/margin.rs`
  - `file_tree/` -> `view/file_tree/`
  - `theme.rs` -> `view/theme.rs`
  - `view.rs` -> `view/stream.rs` (renamed to avoid conflict with view/mod.rs)
- **Action:** Create `src/view/mod.rs` and update `use` paths.

### Phase 6: Group Asynchronous Services

Group all modules that deal with external processes and I/O into `src/services/`.

- **Move LSP modules:**
  - `lsp_manager.rs` -> `services/lsp/manager.rs`
  - `lsp.rs` -> `services/lsp/client.rs`
  - `lsp_async.rs` -> `services/lsp/async_handler.rs`
  - `lsp_diagnostics.rs` -> `services/lsp/diagnostics.rs`
- **Move Plugin modules:**
  - `plugin_thread.rs` -> `services/plugins/thread.rs`
  - `plugin_api.rs` -> `services/plugins/api.rs`
  - `plugin_process.rs` -> `services/plugins/process.rs`
  - `ts_runtime.rs` -> `services/plugins/runtime.rs`
  - `hooks.rs` -> `services/plugins/hooks.rs`
  - `event_hooks.rs` -> `services/plugins/event_hooks.rs`
- **Move other services:**
  - `fs/` -> `services/fs/`
  - `async_bridge.rs` -> `services/async_bridge.rs`
  - `clipboard.rs` -> `services/clipboard.rs`
  - `signal_handler.rs` -> `services/signal_handler.rs`
  - `process_limits.rs` -> `services/process_limits.rs`
- **Action:** Create `mod.rs` files for the new subdirectories and update `use` paths.

### Phase 7: Group Primitives & Utilities

Move the remaining low-level, reusable utilities into `src/primitives/`.

- **Move:**
  - `highlighter.rs` -> `primitives/highlighter.rs`
  - `semantic_highlight.rs` -> `primitives/semantic_highlight.rs`
  - `ansi.rs` -> `primitives/ansi.rs`
  - `ansi_background.rs` -> `primitives/ansi_background.rs`
  - `indent.rs` -> `primitives/indent.rs`
  - `text_property.rs` -> `primitives/text_property.rs`
  - `word_navigation.rs` -> `primitives/word_navigation.rs`
  - `line_wrapping.rs` -> `primitives/line_wrapping.rs`
  - `line_iterator.rs` -> `primitives/line_iterator.rs`
- **Keep at root:**
  - `config.rs` -> stays at root (used globally)
  - `state.rs` -> stays at root (main orchestrator)
- **Action:** Create `src/primitives/mod.rs` and update `use` paths.

## Implementation Notes

### Automated Migration Script

The reorganization can be automated using a shell script that:
1. Creates the new directory structure
2. Moves files to their new locations using `git mv`
3. Updates `mod.rs` files to declare the new module structure
4. Uses `sed` or similar tools to update `use` and `crate::` paths

### Path Update Patterns

Key patterns to find and replace:
- `crate::editor::` -> `crate::app::`
- `crate::text_buffer` -> `crate::model::buffer`
- `crate::cursor` -> `crate::model::cursor`
- `crate::piece_tree` -> `crate::model::piece_tree`
- `crate::marker` -> `crate::model::marker`
- `crate::event` -> `crate::model::event`
- `crate::lsp` -> `crate::services::lsp::client`
- `crate::lsp_manager` -> `crate::services::lsp::manager`
- `crate::lsp_async` -> `crate::services::lsp::async_handler`
- `crate::lsp_diagnostics` -> `crate::services::lsp::diagnostics`
- `crate::plugin_thread` -> `crate::services::plugins::thread`
- `crate::plugin_api` -> `crate::services::plugins::api`
- `crate::plugin_process` -> `crate::services::plugins::process`
- `crate::ts_runtime` -> `crate::services::plugins::runtime`
- `crate::hooks` -> `crate::services::plugins::hooks`
- `crate::event_hooks` -> `crate::services::plugins::event_hooks`
- `crate::highlighter` -> `crate::primitives::highlighter`
- `crate::semantic_highlight` -> `crate::primitives::semantic_highlight`
- `crate::ansi` -> `crate::primitives::ansi`
- `crate::indent` -> `crate::primitives::indent`
- etc.

## Follow-up Work

After each phase of moving files, a crucial step will be to update all `use` statements and module declarations (`mod.rs` and `lib.rs`) to reflect the new paths. This can be a tedious process but is essential for the compiler. Tools like `sed`, `grep`, or IDE-based find-and-replace will be necessary.

This phased approach allows the refactoring to be done in manageable chunks, reducing the risk of breaking the build for an extended period.

## Files Audit

All modules in `lib.rs` have been accounted for in this reorganization plan:

| Current Location | Target Location | Notes |
|-----------------|-----------------|-------|
| `actions.rs` | `input/actions.rs` | |
| `ansi.rs` | `primitives/ansi.rs` | |
| `ansi_background.rs` | `primitives/ansi_background.rs` | |
| `async_bridge.rs` | `services/async_bridge.rs` | |
| `buffer_mode.rs` | `input/buffer_mode.rs` | |
| `clipboard.rs` | `services/clipboard.rs` | |
| `command_registry.rs` | `input/command_registry.rs` | |
| `commands.rs` | `input/commands.rs` | |
| `config.rs` | `config.rs` | Stays at root |
| `control_event.rs` | `model/control_event.rs` | |
| `cursor.rs` | `model/cursor.rs` | Pure data |
| `document_model.rs` | `model/document_model.rs` | |
| `edit.rs` | `model/edit.rs` | |
| `editor/` | `app/` | |
| `event.rs` | `model/event.rs` | Has overlay dep (acceptable) |
| `event_hooks.rs` | `services/plugins/event_hooks.rs` | |
| `file_tree/` | `view/file_tree/` | |
| `fs/` | `services/fs/` | |
| `highlighter.rs` | `primitives/highlighter.rs` | |
| `hooks.rs` | `services/plugins/hooks.rs` | |
| `indent.rs` | `primitives/indent.rs` | |
| `input_history.rs` | `input/input_history.rs` | |
| `keybindings.rs` | `input/keybindings.rs` | |
| `line_iterator.rs` | `primitives/line_iterator.rs` | |
| `line_wrapping.rs` | `primitives/line_wrapping.rs` | |
| `lsp.rs` | `services/lsp/client.rs` | |
| `lsp_async.rs` | `services/lsp/async_handler.rs` | |
| `lsp_diagnostics.rs` | `services/lsp/diagnostics.rs` | Has mixed concerns (noted) |
| `lsp_manager.rs` | `services/lsp/manager.rs` | |
| `margin.rs` | `view/margin.rs` | |
| `marker.rs` | `model/marker.rs` | |
| `marker_tree.rs` | `model/marker_tree.rs` | |
| `multi_cursor.rs` | `input/multi_cursor.rs` | Moved from model/ (has state dep) |
| `overlay.rs` | `view/overlay.rs` | |
| `piece_tree.rs` | `model/piece_tree.rs` | |
| `plugin_api.rs` | `services/plugins/api.rs` | |
| `plugin_process.rs` | `services/plugins/process.rs` | |
| `plugin_thread.rs` | `services/plugins/thread.rs` | |
| `popup.rs` | `view/popup.rs` | |
| `position_history.rs` | `input/position_history.rs` | |
| `process_limits.rs` | `services/process_limits.rs` | |
| `prompt.rs` | `view/prompt.rs` | |
| `script_control.rs` | `app/script_control.rs` | |
| `semantic_highlight.rs` | `primitives/semantic_highlight.rs` | |
| `signal_handler.rs` | `services/signal_handler.rs` | |
| `split.rs` | `view/split.rs` | |
| `state.rs` | `state.rs` | Stays at root (orchestrator) |
| `text_buffer.rs` | `model/buffer.rs` | |
| `text_property.rs` | `primitives/text_property.rs` | |
| `theme.rs` | `view/theme.rs` | |
| `ts_runtime.rs` | `services/plugins/runtime.rs` | |
| `ui/` | `view/ui/` | |
| `view.rs` | `view/stream.rs` | |
| `viewport.rs` | `view/viewport.rs` | |
| `virtual_text.rs` | `view/virtual_text.rs` | |
| `word_navigation.rs` | `primitives/word_navigation.rs` | |
