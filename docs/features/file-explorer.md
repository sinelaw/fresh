# File Explorer

Fresh includes a built-in file explorer.

*   **Toggle Sidebar:** Use `Ctrl+B` to show/hide the file explorer sidebar. When a nested file is active, toggling on expands the tree and reveals the file.
*   **Focus:** Use `Ctrl+E` to switch focus between the file explorer and editor.
*   **Sidebar sections:** Plugins can add sections under the file tree — the Markdown **Contents** outline, for example. `Alt+Shift+N` cycles the keyboard through the sidebar (file explorer, then each section, then back to the editor) and `Alt+Shift+P` runs the cycle the other way; both work from inside the tree, a section, the dock or a terminal (in a terminal these two chords go to the editor, not to the program running in it). A hidden sidebar is shown first. A section belongs to the window — or the file — it was made for, so another workspace's outline never appears in yours.
*   **Reveal:** Showing or focusing the explorer selects the current file in the tree. When it cannot — an unnamed buffer, or a file outside the project — the status bar says so.
*   **Navigation:** Use the arrow keys to move up and down the file tree.
*   **Sticky parents:** When you scroll through a nested folder, its expanded ancestor folders remain visible at the top of the sidebar.

## Opening Files

- **Enter** opens the selected file and focuses the editor.
- **Arrow Up/Down** also opens the highlighted file in a preview tab automatically as you move — so you can scan files without leaving the keyboard.
- **Single-click** opens a file in an ephemeral *preview* tab — the next single-click on another file replaces it instead of piling up tabs. Any real commitment — editing the file, pressing Enter, double-clicking, clicking the tab itself, or a layout action like splitting — promotes the preview to a permanent tab.
- **Double-click** opens the file in a permanent tab and focuses the editor.

Preview tabs are enabled by default. Turn them off in the Settings UI if you prefer every click to open a permanent tab.

## Cut / Copy / Paste and Multi-Selection

- **`Ctrl+C` / `Ctrl+X` / `Ctrl+V`** — copy, cut, or paste the selection. Same-directory copy auto-appends ` copy` / ` copy 2` etc. Same-directory cut is a no-op. Paste into a different directory with a name conflict asks per file, in a confirmation dialog offering Overwrite, Overwrite All, Skip, Skip All and Cancel (each button's own underlined letter works as an accelerator).
- **Cut-pending** items are visually dimmed. Cancel a pending cut with Escape or by pasting back into the same directory.
- **`Shift+Up` / `Shift+Down`** extend a multi-select range from the current anchor; all clipboard operations (and delete) act on the whole selection.
- **Buffers follow files** — renaming or moving a file (via cut+paste) relocates any open buffers pointing at it; deleting a file closes its buffer. Renaming a directory relocates buffers for every file inside it.

## Following the Active File

`file_explorer.follow_active_buffer` (off by default) keeps the tree pointed at
whatever file you are editing: switch tabs, or jump to a definition in another
file, and the sidebar expands to that file and moves its selection onto it.

It stays out of your way while you are using the tree yourself — with the
keyboard inside the sidebar, the selection is yours and nothing moves it. It
also does nothing while the sidebar is hidden, or for a file outside the
project root.

Toggling the sidebar on, or focusing it with `Ctrl+E`, reveals the active file
either way; that is the explicit "show me where I am", and this setting does
not govern it.

## Width

The sidebar's width is configurable via `file_explorer.width` in settings. It accepts either form:

- A **percent** of the terminal width, e.g. `"30%"`.
- An **absolute** number of columns, e.g. `"24"`.

Dragging the divider preserves whichever form you configured — a sidebar set up as a percent stays a percent after you drag it.

## Visibility and .gitignore

- The file explorer respects your `.gitignore` by default, and auto-reloads when `.gitignore` changes on disk.
- A file is shown only if it isn't hidden by **any** active filter — so if a file is both a dotfile and gitignored, it takes enabling both toggles to see it.
- Use **Toggle Hidden Files** and **Toggle Gitignored Files** from the command palette to flip either filter. Both persist across restarts.

## Sorting

Entries sort in natural order: directories first, then files, with digit runs compared numerically (so `chapter-2` comes before `chapter-10`). Sorting is case-insensitive.

See it in action: [Preview Tabs in the 0.3.0 blog](/blog/fresh-0.3.0/#preview-tabs-in-file-explorer).
