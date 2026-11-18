# Command Palette

Press `Ctrl+P` to open the command palette. Use prefix characters to switch modes:

| Prefix | Mode | Description |
|--------|------|-------------|
| *(none)* | File finder | Fuzzy search for files in your project |
| `>` | Commands | Search and run editor commands |
| `#` | Buffers | Switch between open buffers by name |
| `:` | Go to line | Jump to a specific line number |

**Tips:**
- A hints line at the bottom shows available prefixes
- Press `Tab` to accept the top suggestion. In file finder mode, `Tab` instead completes the path as far as all the matches agree, like shell completion
- Type `>` to access commands, or `#` followed by a buffer name to switch files
- Space-separated terms match independently (e.g., "feat group" matches "features/groups/view.tsx") — so `etc hosts` finds `/etc/hosts`, `save file` finds `save_file.rs`
- In file finder mode, use `path:line[:col]` syntax to jump to a location after opening (e.g. `src/main.rs:42:10`)
- In go-to-line mode (`:`) and in file-finder mode when you append `:<N>` to a file, the cursor previews the target line live as you type and commits when you press Enter. If you move the mouse or hit Escape, the preview is reverted.

## File Finder on Large and Remote Trees

File enumeration runs in the background, so results stream in as soon as they're found — you can start typing the moment the palette opens, even on very large repositories or over SSH. Typing a path like `etc/hosts` also produces instant filesystem-confirmed matches without waiting for enumeration. Ranking prefers contiguous matches, so `results` finds `results.json` first.

The file finder follows the same hidden and ignored rules as the [file explorer](./file-explorer.md#visibility-and-gitignore): it leaves out gitignored files and anything matching your ignore patterns, and lists hidden files when **Show Hidden Files** is on. Typing a leading `.` (e.g. `.env`) finds dotfiles either way.
