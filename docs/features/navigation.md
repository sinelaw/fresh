# Navigation

*   **Go to Definition:** Use the command palette (`Ctrl+P >`) and search for "Go to Definition" to jump to the definition of a symbol under the cursor (requires LSP).
*   **Position History:** Navigate back and forward through your edit locations using `Alt+Left` and `Alt+Right`.
*   **Open File Jump:** The Open File prompt and Quick Open (`Ctrl+O`) support `path:line[:col]` syntax to jump directly to a location after opening (e.g. `src/main.rs:42:10`).
*   **Hidden files:** The Open File prompt hides dotfiles by default. Start your filter with `.` to reveal them (e.g. `.env` surfaces `.envrc`).
*   **New tab:** The tab bar's **+** button opens a popup to create a New Terminal or New File.
*   **Recent Files:** **File → Recent Files…** (or "Recent Files" in the command palette) reopens a file you closed in this workspace. The list is kept with the workspace, so it survives a restart.

## Tabs

*   Tabs for files with the same name show as much of their parent folder as it takes to tell them apart (`model/mod.rs`, `view/mod.rs`).
*   Middle-click a tab to close it. Scroll the mouse wheel over the tab bar to pan through tabs that don't fit.
*   Turn on **Auto Hide Tab Bar** in Settings to hide the tab bar while a window shows a single file; it comes back as soon as a second tab or split opens.

## Large Files

A file counts as large when it is over 10 MB; change the threshold in Settings under **Large File Threshold Bytes**. Large files open instantly because Fresh loads them lazily, a piece at a time, and they don't start a language server.

When opening a large file, the gutter shows **byte offsets** instead of line numbers. To get exact line numbers, use "Go to Line" from the command palette — Fresh will offer to scan the file. Only the line index is kept in memory, not the file contents. Over SSH, the scan runs server-side and only the index is transferred. You can also trigger this directly with "Scan Line Index" from the command palette.

**Tip:** for minified or single-line files (a large JSON or JS bundle on one line), turn on line wrap with **Toggle Line Wrap (Current Buffer)** so the text fits the window instead of running off to the right.

## Split View

Use the command palette for "Split Vertical", "Split Horizontal", "Close Split", "Next Split", and "Previous Split".

"Next Pane" and "Previous Pane" step through every tab in every split as one list. They have no key by default; run them from the command palette or bind them in the [Keybinding Editor](./keybinding-editor.md).

**Scroll Sync** — same-buffer splits can scroll together. Toggle via "Toggle Scroll Sync" in the command palette.
