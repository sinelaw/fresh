# File Explorer

Fresh includes a built-in file explorer.

*   **Toggle Sidebar:** Use `Ctrl+B` to show/hide the file explorer sidebar. When a nested file is active, toggling on expands the tree and reveals the file.
*   **Focus:** Use `Ctrl+E` to switch focus between the file explorer and editor.
*   **Navigation:** Use the arrow keys to move up and down the file tree.

## Opening Files

- **Enter** opens the selected file and focuses the editor.
- **Arrow Up/Down** also opens the highlighted file in a preview tab automatically as you move — so you can scan files without leaving the keyboard.
- **Single-click** opens a file in an ephemeral *preview* tab — the next single-click on another file replaces it instead of piling up tabs. Any real commitment — editing the file, pressing Enter, double-clicking, clicking the tab itself, or a layout action like splitting — promotes the preview to a permanent tab.
- **Double-click** opens the file in a permanent tab and focuses the editor.

Preview tabs are enabled by default. Turn them off in the Settings UI if you prefer every click to open a permanent tab.

## Cut / Copy / Paste and Multi-Selection

- **`Ctrl+C` / `Ctrl+X` / `Ctrl+V`** — copy, cut, or paste the selection. Same-directory copy auto-appends ` copy` / ` copy 2` etc. Same-directory cut is a no-op. Paste into a different directory with a name conflict prompts per-file: (o)verwrite, (O) all, (s)kip, (S) all, (c)ancel.
- **Cut-pending** items are visually dimmed. Cancel a pending cut with Escape or by pasting back into the same directory.
- **`Shift+Up` / `Shift+Down`** extend a multi-select range from the current anchor; all clipboard operations (and delete) act on the whole selection.
- **Buffers follow files** — renaming or moving a file (via cut+paste) relocates any open buffers pointing at it; deleting a file closes its buffer. Renaming a directory relocates buffers for every file inside it.

## Width

The sidebar's width is configurable via `file_explorer.width` in settings. It accepts either form:

- A **percent** of the terminal width, e.g. `"30%"`.
- An **absolute** number of columns, e.g. `"24"`.

Dragging the divider preserves whichever form you configured — a sidebar set up as a percent stays a percent after you drag it.

## Visibility and .gitignore

- The file explorer respects your `.gitignore` by default, and auto-reloads when `.gitignore` changes on disk.
- A file is shown only if it isn't hidden by **any** active filter — so if a file is both a dotfile and gitignored, it takes enabling both toggles to see it.
- Use **Toggle Hidden Files** and **Toggle Gitignored Files** from the command palette to flip either filter. Both settings persist to config across sessions.

See it in action: [Preview Tabs in the 0.3.0 blog](/blog/fresh-0.3.0/#preview-tabs-in-file-explorer).
