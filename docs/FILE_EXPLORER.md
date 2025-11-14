# File Explorer

A VS Code-style file explorer with lazy loading, gitignore support, and async I/O.

## Status: ~95% Complete

**Working features:**
- Toggle show/hide (Ctrl+B)
- Navigate directory tree (Alt+J/K)
- Expand/collapse directories (Alt+L/Alt+Shift+H or Enter on directory)
- Open files in editor (Alt+Enter or Enter on file, auto-switches focus to editor)
- Project directory auto-expands on initialization
- Editor tabs positioned above editor area only (when file explorer is open)
- Auto-expand and select open file when switching focus to file explorer
- Refresh directory contents (Alt+R)
- Create files/directories (Alt+N, Alt+Shift+N)
- Delete files/directories (Alt+Shift+D)
- Gitignore filtering (Alt+I)
- Hidden file toggle (Alt+.)
- Unsaved change indicators (●) for modified files
- Symmetric scrolling behavior (cursor reaches viewport edges before scrolling)
- 15+ keybindings
- E2E tests (all passing in parallel)

**In progress:**
- Rename (needs input dialog system)
- Copy/move operations
- File watching for auto-refresh

## Architecture

### Filesystem Layer (`src/fs/`)
- **FsBackend** trait - Pluggable filesystem (local implemented, network-ready)
- **LocalFsBackend** - Async I/O with tokio::fs, LRU metadata cache
- **FsManager** - Request deduplication, batch operations

### Tree Model (`src/file_tree/`)
- **FileTree** - Lazy-loading tree, path-to-node HashMap for O(1) lookup
- **TreeNode** - State machine (Collapsed, Loading, Expanded, Error, Leaf)
- **FileTreeView** - Navigation, filtering, scroll management
- **IgnorePatterns** - Gitignore support via `ignore` crate

### UI (`src/ui/file_explorer.rs`)
- Ratatui-based rendering
- Tree structure visualization with expand/collapse indicators (▼/▶)
- Unsaved change indicators (●) for modified files
- Color-coded entries (directories, hidden files, selected items)
- Focus-aware rendering

### Integration (`src/editor.rs`)
- File explorer as special buffer type
- Split view integration (30% explorer | 70% editor)
- Focus management
- Async operations via AsyncBridge

## Key Design Decisions

1. **Lazy Loading** - Only expanded directories are in memory. Collapse frees memory immediately.
2. **Async-First** - All filesystem operations are non-blocking (essential for network filesystems).
3. **Pluggable Backend** - FsBackend trait allows local, NFS, SSHFS, etc.
4. **Gitignore Support** - Uses `ignore` crate (same as ripgrep) for git-compatible patterns.
5. **Request Batching** - Duplicate concurrent requests are deduplicated.

## Performance

- List 10,000 file directory: < 100ms
- Parallel metadata fetch (100 files): < 1s
- Tree expansion: O(n) where n = direct children
- Node lookup: O(1) via HashMap
- Memory per node: ~200 bytes

## Configuration

In `config.json`:

```json
{
  "file_explorer": {
    "show_hidden": false,
    "show_gitignored": false,
    "respect_gitignore": true,
    "custom_ignore_patterns": [],
    "width": 0.3
  }
}
```

## Tests

- Tests in `src/fs/` (filesystem layer)
- Tests in `src/file_tree/` (tree model, including expand_to_path tests)
- E2E tests in `tests/e2e/file_explorer.rs` (+ 1 ignored for unrelated issue)
  - Enter key behavior (directories and files)
  - Auto-expansion on initialization
  - Auto-expand and select open file on focus switch
  - Unsaved change indicators
  - Tab positioning with file explorer
  - Symmetric scrolling behavior
  - Focus switching, navigation, and more
- All tests hermetic (isolated temp directories)

## Future Enhancements

- Input dialog system for custom names
- File watching for auto-refresh
- Search/filter within explorer
- Preview on selection
- Bulk operations
- Network filesystem backends (SSH, SFTP)
