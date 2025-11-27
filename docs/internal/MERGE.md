# 3-Way Merge Conflict Resolution: Specification

> **STATUS: IMPLEMENTED** - This feature has been implemented as `plugins/merge_conflict.ts`. This document is retained as the design specification.

## 1. Implementation Strategy: Plugin-First

To keep the editor core lean and encourage extensibility, this feature **SHOULD** be implemented as a self-contained **TypeScript plugin**. This approach decouples the complex UI and state management of the merge tool from the editor's core functionality.

The plugin will be responsible for:
*   Detecting conflicts.
*   Running `git` to fetch the different file versions.
*   Calculating diffs.
*   Rendering the entire custom merge UI.
*   Managing all user interactions (keyboard and mouse) within the merge view.

This strategy depends on critical additions to the core editor's plugin API, as detailed in the technical requirements section.

## 2. User Experience (UX) and Behavior

The UX must be intuitive and efficient, supporting both keyboard-first and mouse-assisted workflows.

### 2.1. Invocation

*   **Automatic Detection:** The editor MUST provide a plugin event hook (e.g., `on_buffer_load`) that allows the plugin to inspect buffer content upon file opening. The plugin will use this hook to detect Git conflict markers.
*   **Mode Transition:** Upon detection, the plugin will take over the view, replacing the standard editor with the 3-Way Merge UI. It will not display the raw marker text.

### 2.2. Visual Layout

The merge interface will be a custom view rendered by the plugin, appearing as a multi-panel layout.

```
┌───────────────────────────┬───────────────────────────┐
│ OURS (Read-only)          │ THEIRS (Read-only)        │
│  Changes from your branch.│  Incoming changes.        │
│  [  Conflict 1  ]         │  [  Conflict 1  ]         │
├───────────────────────────┴───────────────────────────┤
│ RESULT (Editable)                                     │
│  Resolved code appears here. This is a fully          │
│  functional editor pane.                              │
│  << Accept Ours | Accept Theirs >>                    │
├───────────────────────────────────────────────────────┤
│ [n] Next [p] Prev | [u] Use Ours [t] Take Theirs [b] Both | [s] Save & Exit [q] Abort │
└───────────────────────────────────────────────────────┘
```
*(Note: A true three-panel view showing `BASE` is a potential future enhancement).*

### 2.3. Keyboard Shortcut Display

To ensure clarity, a help bar displaying the available keyboard shortcuts **MUST** be visible at the bottom of the screen at all times while the merge view is active. This bar provides context-sensitive commands for the merge workflow.

**Example:**
`[n] Next [p] Prev | [u] Use Ours [t] Take Theirs [b] Both | [s] Save & Exit [q] Abort`

### 2.4. Mouse Support

To provide a familiar experience for users of GUI-based merge tools, key actions **MUST** be clickable.

*   **Clickable Actions:** The text prompts rendered as virtual text (e.g., `<< Accept Ours`, `Accept Theirs >>`) are interactive.
*   **Behavior:** Clicking on one of these text "buttons" **MUST** trigger the exact same action as its corresponding keyboard shortcut. For example, clicking on `<< Accept Ours` is equivalent to pressing the `u` key.

### 2.5. Visual Elements & Highlighting

The plugin will use the `editor.addOverlay` and `editor.addVirtualText` APIs to render all visual elements.

*   **Conflict Blocks:** Each conflicting block should be highlighted with a distinct background color.
*   **Intra-line Diffing:** The plugin MUST perform word-level diffing.
    *   **Additions:** Highlight with a **green** background.
    *   **Deletions:** Show a shaded block in **red**.
    *   **Modifications:** Use a **blue** background.
*   **Actionable "Buttons":** The clickable text prompts (`<< Accept Ours`, etc.) will be rendered using `editor.addVirtualText`.
*   **Status Bar:** The editor's main status bar MUST be updated by the plugin with merge-specific information (e.g., `Merge: 3 of 5 conflicts remaining.`).

### 2.6. Core Interactions & Behavior

*   **Navigation:**
    *   `n` / `j` / `Down Arrow`: Jump to the **next** conflict block.
    *   `p` / `k` / `Up Arrow`: Jump to the **previous** conflict block.
*   **Resolving Conflicts:**
    *   `u` (Use): Accept the "Ours" hunk.
    *   `t` (Take): Accept the "Theirs" hunk.
    *   `b` (Both): Copy both hunks into the result pane for manual resolution.
*   **Manual Editing:** The `RESULT` pane is a fully editable `DocumentModel`. Any edit within a conflict block marks it as resolved.
*   **Completion & Exit:**
    *   `s` (Save & Exit): Saves the `RESULT` buffer and exits. Requires confirmation if conflicts remain.
    *   `q` (Abort): Discards all merge changes and exits. Requires confirmation.

### 2.7. Intelligent Auto-Resolution (Leveraging `git-mediate` Logic)

*   **Non-Conflicting Changes:** The `RESULT` buffer should be pre-populated with all non-conflicting changes.
*   **`git-mediate` Style Trivial Conflict Resolution:** The plugin SHOULD implement an auto-resolution strategy inspired by `git-mediate`. This means:
    1.  For each block marked as a conflict, the tool will compare the "Ours" version against the common ancestor ("Base").
    2.  Simultaneously, it will compare the "Theirs" version against "Base".
    3.  If only one side has modified the block relative to "Base" (i.e., the other side's version for that block is identical to "Base"), then that unilateral change is automatically accepted.
    4.  Only truly ambiguous conflicts, where both "Ours" and "Theirs" have different modifications from "Base" in the same region, will be presented for manual resolution.
*   **Default Activation:** This `git-mediate` inspired auto-resolution mechanism **MUST** be active by default to minimize user effort and streamline the merge process.

## 3. Plugin-Based Technical Implementation

### 3.1. Core API Requirements

To enable a plugin-based implementation, the core editor **MUST** provide the following:

1.  **File Open Hook:** An event hook like `editor.on("buffer_load", "my_handler")` that is triggered after a file's content is loaded. This is **critical** for automatic conflict detection.
2.  **Mouse Event Hook:** An event hook like `editor.on("mouse_click", "my_mouse_handler")` that provides the screen coordinates (row, column) and button type for any mouse click within the editor viewport.
3.  **Custom View Transformation API:** An API like `editor.submitViewTransform` powerful enough for a plugin to render a completely custom UI.

### 3.2. Plugin Logic Flow

1.  **Registration:** The plugin registers handlers for `buffer_load` and `mouse_click` events.
2.  **Detection:** The `buffer_load` handler checks for conflict markers.
3.  **Activation:** If markers are found, the plugin activates the `'merge-conflict'` mode and initiates data fetching.
4.  **Mouse Handling:** The `mouse_click` handler checks if the click coordinates fall within the screen bounds of any clickable elements rendered by the plugin. If so, it invokes the corresponding resolution action.

### 3.3. Data Fetching

*   The plugin will use `await editor.spawnProcess("git", ...)` to get the `base`, `ours`, and `theirs` content.

### 3.4. Custom View Rendering

*   The complex layout will be built using the **`editor.submitViewTransform`** API, not editor splits.
*   The plugin will construct `ViewTokenWire` objects representing the entire UI, drawing text from its in-memory state.
*   During this process, the plugin **must** store the screen coordinates (bounding boxes) for all clickable virtual text elements it renders.

### 3.5. State Management

*   The plugin will maintain its own state in TypeScript, including the content of `ours`, `theirs`, `base`, the list of conflicts, and the **screen positions of clickable UI elements**.

### 3.6. Keybindings and Mode

*   The plugin will call `editor.defineMode("merge-conflict", "normal", ...)` to register all keyboard shortcuts.
*   The plugin's `mouse_click` handler will provide the functionality for the mouse-based interactions.

## 4. Future Enhancements

*   **Show Base:** Add a command to toggle a three-panel view that includes the `BASE` document.
*   **Ignore Whitespace:** Add a toggle to ignore whitespace changes during diffing.
*   **Plugin Hooks:** The merge plugin could, in turn, expose its own hooks for other plugins to provide custom resolution strategies.
