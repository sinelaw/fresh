# Input Processing, Layout, and Rendering Architecture

This document summarizes the architecture for input processing, layout, and rendering in `fresh2`, covering the main buffer flow, split/composite views, Settings UI, plugin system, and specific UI elements.

## 1. Input Processing / Propagation / Bubbling

The input system follows a hierarchical, bubbling model defined primarily in `crates/fresh-editor/src/input/handler.rs`.

### Core Mechanism
*   **Trait:** The `InputHandler` trait defines the contract for any component that handles input.
    *   `handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult`: Handles the event at the current level.
    *   `focused_child_mut(&mut self) -> Option<&mut dyn InputHandler>`: Returns the currently focused child component.
    *   `dispatch_input(...) -> InputResult`: The main entry point. It implements the **Leaf-first, bubble-up** logic.
*   **Propagation Logic:**
    1.  **Leaf-First:** `dispatch_input` recursively calls itself on `focused_child_mut()`.
    2.  **Bubble Up:** If the child returns `InputResult::Ignored`, the parent attempts to handle the event via `handle_key_event`.
    3.  **Consumption:** If a handler returns `InputResult::Consumed`, propagation stops immediately.
*   **Modals:** Handlers can override `is_modal()` to return `true`. A modal handler forces `dispatch_input` to return `Consumed` for *all* events, even those it didn't explicitly handle, preventing input leakage to parents (used in Settings and Prompts).
*   **Context:** `InputContext` is threaded through the call stack. It allows handlers to queue `DeferredAction`s (e.g., `CloseSettings`, `PasteToSettings`, `ExecuteAction`) to be performed after the immutable borrow of the input loop is released.

### Specific Implementations
*   **Composite View (Splits):** `CompositeInputRouter` (`crates/fresh-editor/src/input/composite_router.rs`) handles input for split buffers.
    *   It routes input based on the `focused_pane` in `CompositeViewState`.
    *   It handles navigation keys (e.g., `Tab` to switch panes, `h`/`j`/`k`/`l` for movement) and routes editing keys to the active source buffer if editable.
    *   It blocks editing on read-only panes.
*   **Settings UI:** `SettingsState` (`crates/fresh-editor/src/view/settings/state.rs`) implements `InputHandler`.
    *   It manages focus between panels (`Categories`, `Settings`, `Footer`).
    *   It delegates specific input handling to sub-handlers like `handle_text_editing_input`, `handle_dropdown_input`, etc., depending on the active control state.
*   **Plugins:** Plugins (e.g., `pkg.ts`) define custom modes (e.g., `"pkg-manager"`) via `editor.defineMode`.
    *   Key bindings in these modes map to global JavaScript functions (e.g., `globalThis.pkg_nav_up`).
    *   The core editor dispatches these actions when the virtual buffer has focus.

## 2. Layout and Rendering

Rendering uses `ratatui` for TUI primitives and a custom viewport system for buffer content.

### Viewport & Buffer Rendering
*   **Viewport Logic:** `Viewport` (`crates/fresh-editor/src/view/viewport.rs`) manages the visible window into a `TextBuffer`.
    *   **State:** Tracks `top_byte` (source of truth for vertical scroll), `left_column`, `width`, `height`, and wrapping state.
    *   **Synchronization:** `ensure_visible` ensures the cursor stays in view, calculating line positions. It handles both wrapped and unwrapped lines.
    *   **Lazy Loading:** `prepare_viewport` pre-loads chunks of data from `TextBuffer` (which uses a `PieceTree`) before rendering to ensure performance with large files.
*   **Text Buffer:** `TextBuffer` (`crates/fresh-editor/src/model/buffer.rs`) is the data model.
    *   Uses a `PieceTree` for efficient edits.
    *   Supports lazy loading for large files.
    *   Provides iterators (`iter_pieces_in_range`) for rendering.

### Composite / Split Views
*   **Data Model:** `CompositeBuffer` (`crates/fresh-editor/src/model/composite_buffer.rs`) aggregates multiple `SourcePane`s.
    *   **Alignment:** Uses `LineAlignment` to map display rows to source lines (e.g., for side-by-side diffs). Rows can be `Context`, `Addition`, `Deletion`, etc.
*   **View State:** `CompositeViewState` (`crates/fresh-editor/src/view/composite_view.rs`) manages the presentation.
    *   **Layout:** Calculates `pane_widths` based on configured ratios (e.g., 50/50) and computes `Rect`s for each pane via `compute_pane_rects`.
    *   **Scrolling:** Maintains a single `scroll_row` for synchronized scrolling of all panes.
    *   **Cursors:** Maintains separate `pane_cursors` for each pane.

### Settings UI Rendering
*   **Renderer:** `render_settings` (`crates/fresh-editor/src/view/settings/render.rs`) draws the UI.
    *   **Layout:** Dynamically chooses between Vertical (narrow) and Horizontal (wide) layouts based on available width.
    *   **Hit Testing:** `SettingsLayout` tracks screen areas (`Rect`s) for items and controls to support mouse interaction (`SettingsHit`).
    *   **Controls:** Specialized renderers for `Toggle`, `Number`, `Dropdown`, `TextList`, `Map`, etc., handle the visual state of each setting.
    *   **ScrollablePanel:** Used to render the list of settings items, handling its own scroll offset and clipping.

## 3. UI Elements Implementation

### Editor Chrome
*   **Tabs:** `TabsRenderer` (`crates/fresh-editor/src/view/ui/tabs.rs`) renders the tab bar.
    *   **Scrolling:** `compute_tab_scroll_offset` calculates horizontal scrolling to keep the active tab visible.
    *   **Rendering:** Iterates through buffers in the split, applying styles for active/inactive/hovered states. Renders close buttons ("×") and modified indicators ("*"). Returns hit areas for mouse interaction.
*   **Status Bar:** `StatusBarRenderer` (`crates/fresh-editor/src/view/ui/status_bar.rs`) handles the bottom bar.
    *   **Layout:** Renders left side (filename, position, diagnostics) and right side (line ending, language, LSP status). Truncates long paths/messages intelligently.
    *   **Prompt:** The prompt/minibuffer is rendered within the status bar area. It handles cursor positioning and text selection styling.
*   **Menu Bar:** `MenuRenderer` (`crates/fresh-editor/src/view/ui/menu.rs`) renders the top menu.
    *   **State:** `MenuState` tracks active menu, highlighted item, and nested submenu path.
    *   **Dropdowns:** Renders recursive dropdown levels, adjusting position to stay within screen bounds (flipping left/right if needed). Handles keyboard navigation and mnemonics (Alt+Letter).
*   **Scrollbars:** `render_scrollbar` (`crates/fresh-editor/src/view/ui/scrollbar.rs`) is a reusable widget.
    *   **Logic:** Calculates thumb size and position based on viewport ratio (`thumb_geometry`). Supports click-to-scroll on the track (`click_to_offset`).
    *   **Styling:** Uses different colors for track, thumb, and hover states.

### Settings UI Components
*   **ScrollablePanel:** (`crates/fresh-editor/src/view/ui/scroll_panel.rs`) A generic widget for scrolling variable-height items.
    *   **Row-based Scrolling:** Manages `ScrollState` with row offsets (not item index offsets) to handle items of different heights.
    *   **Focus Visibility:** `ensure_focused_visible` handles scrolling to keep focused items (and sub-regions within items) in view.
*   **Buttons:** `render_button` (`crates/fresh-editor/src/view/settings/render.rs`) renders footer buttons (Save, Cancel, etc.).
    *   **States:** Handles focused (bold/highlighted) and hovered styles.
    *   **Layout:** Dynamically positions buttons based on available width, hiding less critical buttons (Layer, Reset) in narrow modes.

### Plugin UI (pkg.ts)
*   **Virtual Buffer:** The package manager uses a read-only virtual buffer created via `createVirtualBufferInExistingSplit`.
*   **Text-based UI:** UI elements are "drawn" using text characters:
    *   **Buttons:** Rendered as text like `[ Install ]` or `[ Update ]`.
    *   **Lists:** Rendered as lines of text with prefixes like `▸` for selection.
    *   **Layout:** Manually pads strings to create columns (e.g., `padEnd(LIST_WIDTH)`).
*   **Styling (Overlays):** `applyPkgManagerHighlighting` applies colors using `editor.addOverlay`.
    *   It parses the text content (e.g., finding `[ ]` brackets) or uses the `entries` structure to apply specific styles (colors) to ranges of bytes.
*   **Interaction:**
    *   **Navigation:** Global functions (`pkg_nav_up`, `pkg_nav_down`) mapped to keys update the internal selection state index.
    *   **Action:** `pkg_activate` checks the current `focus` state (list, filter button, action button) and executes the corresponding logic.

## 4. Code Flow & Responsibility Split

This section details how responsibilities (State, Render, Input, Mouse) are distributed for each component category.

### Buffer / Editor View
**Architecture:** Integrated Pipeline
*   **State:** Held in `EditorState` and `TextBuffer` (content) + `Viewport` (scroll position).
*   **Rendering (`view_pipeline.rs`):** A strict pipeline converts `TextBuffer` → `ViewTokenWire` (tokens) → `ViewLine` (display lines). This preserves semantic info (like source byte offsets) through the entire process.
*   **Layout (`Layout` in `view_pipeline.rs`):** The pipeline produces a `Layout` object containing `ViewLine`s and `byte_to_line` mappings. This object acts as the "hit test" database.
*   **Input (`handler.rs`):** Dispatches keys to the active buffer.
*   **Mouse:** Uses the `Layout` object produced during rendering to map screen coordinates `(x, y)` back to source byte offsets in `O(1)` or `O(log n)` time. This decouples rendering logic from hit-testing logic while keeping them consistent.

### Settings UI
**Architecture:** Separated Modules with "Retained Mode" Hit Testing
*   **State (`state.rs`):** `SettingsState` is the single source of truth for navigation, pending changes, and active pages.
*   **Rendering (`render.rs`):** Pure function `render_settings`. Crucially, it returns a `SettingsLayout` object.
*   **Hit Testing (`layout.rs`):** The `SettingsLayout` contains computed `Rect`s for every interactive element (buttons, checkboxes, items). It persists until the next render.
*   **Mouse (`mouse.rs`):** Queries the cached `SettingsLayout` to determine what was clicked (`SettingsHit`). It modifies `SettingsState` directly.
*   **Input (`input.rs`):** A dedicated `InputHandler` implementation for `SettingsState` that routes keys based on the `FocusPanel` (Categories vs Settings vs Footer).

### UI Chrome (Tabs, Status Bar, Menu)
**Architecture:** Immediate Mode Rendering + Hit Area Return
*   **Tabs (`tabs.rs`):** `TabsRenderer::render_for_split` draws tabs and *immediately returns* a `Vec` of hit areas (`buffer_id`, `start_col`, `end_col`, `close_col`). The editor stores this ephemeral layout for the next mouse click.
*   **Status Bar (`status_bar.rs`):** `StatusBarRenderer::render_status_bar` returns a `StatusBarLayout` struct containing coordinates for clickable indicators (LSP, Line Ending, Language).
*   **Menu (`menu.rs`):** `MenuRenderer` draws the menu. Unlike Settings, it doesn't return a full layout object. Instead, `MenuState` has logic (`get_menu_at_position`) to re-calculate hit testing on-the-fly based on string widths.
*   **Scrollbars (`scrollbar.rs`):** `ScrollbarState` holds the logic. `render_scrollbar` draws it. Input handling uses `ScrollbarState::click_to_offset` to map clicks back to scroll positions mathmatically, without needing a cached layout.

### Plugin UI (pkg.ts)
**Architecture:** Monolithic Script
*   **State:** Global `pkgState` object in TypeScript holds everything (search query, selection index, installed list).
*   **Rendering:** `updatePkgManagerView` rebuilds the entire virtual buffer content (strings) from `pkgState` every time something changes. It manually reapplies overlays.
*   **Input:** Global functions (`pkg_nav_up`) modify `pkgState` and call `updatePkgManagerView`.
*   **Mouse:** Not explicitly handled in the script (relies on editor's basic buffer clicking), or handled via basic cursor positioning. The UI is primarily keyboard-driven.

## 5. Review: Manual Flows and Broken Abstractions

A review of the implementation reveals several areas where abstractions are "leaky" or where logic is manually duplicated rather than using a shared framework.

### 5.1 Manual Hit Testing (Menu vs Settings)
There is a significant inconsistency in how interactive areas are tracked:
*   **Settings UI** uses a robust `SettingsLayout` object produced by the renderer. This is a clean, retained-mode approach to hit testing.
*   **Menu System** uses "manual" calculation logic in `MenuState::get_menu_at_position`. It re-calculates widths based on label strings on-the-fly when a click occurs. This means the logic for "how a menu is drawn" is duplicated between `MenuRenderer` and `MenuState`.
*   **Tabs & Status Bar** use a hybrid approach where the renderer returns a list of areas. This is better than the Menu system but less organized than the Settings system.

### 5.2 Broken Prompt Abstraction
The `Prompt` system (`crates/fresh-editor/src/view/prompt.rs`) is designed to be a generic input mechanism. However:
*   The `StatusBarRenderer` contains a specialized `render_file_open_prompt` method.
*   This method has hardcoded knowledge of `FileOpenState` and performs specialized path truncation that only exists for this one prompt type.
*   This breaks the "Generic Prompt" abstraction by forcing the Status Bar to know about specific application-level features. Ideally, path truncation should be a service or a specific "Input Transform" applied to the prompt state.

### 5.3 Virtual Buffer "UI" Complexity
Virtual buffers (used by `pkg.ts` and others) are a low-level abstraction that places a heavy burden on the creator:
*   **Manual Layout:** `pkg.ts` manually calculates padding (e.g., `padEnd(LIST_WIDTH)`) and joins strings to create a "grid".
*   **Manual Styling:** The plugin must calculate byte offsets for every color it wants to apply. If the text content changes, all offsets must be manually re-calculated and re-applied via `editor.addOverlay`.
*   **Missing Components:** There is no "UI Framework" for virtual buffers. Every plugin must re-invent buttons, lists, and focus management from raw bytes and text properties.

### 5.4 The "God Module": Split Rendering
`crates/fresh-editor/src/view/ui/split_rendering.rs` is a massive module (~5.5k lines) that handles:
*   Buffer rendering logic (cursors, selection, syntax highlighting).
*   Split pane layout calculations.
*   Coordination of Tabs, Scrollbars, and the Status Bar.
*   Mapping screen coordinates to buffer positions.
*   The high level of manual coordination here makes it difficult to reason about the rendering lifecycle. Many responsibilities (like scrollbar logic) are "mixed in" rather than being handled by isolated components.

### 5.5 Composite Buffer Input Complexity
`CompositeInputRouter` is a giant, manual match statement for keys. It has to decide whether to scroll the composite view, switch panes, or forward the key to a source buffer. This "routing logic" is hardcoded and doesn't easily allow for plugin-defined behaviors in composite views without modifying the core Rust code.