# Navigation

*   **Go to Definition:** Use the command palette (`Ctrl+P >`) and search for "Go to Definition" to jump to the definition of a symbol under the cursor (requires LSP).
*   **Position History:** Navigate back and forward through your edit locations using `Alt+Left` and `Alt+Right`.
*   **Open File Jump:** The Open File prompt and Quick Open (`Ctrl+O`) support `path:line[:col]` syntax to jump directly to a location after opening (e.g. `src/main.rs:42:10`).

## Large Files

When opening a large file, the gutter shows **byte offsets** instead of line numbers. To get exact line numbers, use "Go to Line" from the command palette — Fresh will offer to scan the file. Only the line index is kept in memory, not the file contents. Over SSH, the scan runs server-side and only the index is transferred. You can also trigger this directly with "Scan Line Index" from the command palette.

## Split View

Use the command palette for "Split Vertical", "Split Horizontal", "Close Split", "Next Split", and "Previous Split".

**Scroll Sync** — same-buffer splits can scroll together. Toggle via "Toggle Scroll Sync" in the command palette.
