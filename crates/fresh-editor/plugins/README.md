# Plugins

This directory contains production-ready plugins for the editor. Plugins are written in **TypeScript** and run in a sandboxed Deno environment. They are automatically loaded when the editor starts.

## Available Plugins

### Core Plugins

| Plugin | Description |
|--------|-------------|
| `welcome.ts` | Displays welcome message on startup |
| `manual_help.ts` | Manual page and keyboard shortcuts display |
| `diagnostics_panel.ts` | LSP diagnostics panel with navigation |
| `search_replace.ts` | Search and replace functionality |
| `path_complete.ts` | Path completion in prompts |

### Git Integration

| Plugin | Description |
|--------|-------------|
| `git_grep.ts` | Interactive search through git-tracked files |
| `git_find_file.ts` | Fuzzy file finder for git repositories |
| `git_blame.ts` | Git blame view with commit navigation |
| `git_log.ts` | Git log viewer with history browsing |

### Code Enhancement

| Plugin | Description |
|--------|-------------|
| `todo_highlighter.ts` | Highlights TODO/FIXME/HACK keywords in comments |
| `color_highlighter.ts` | Highlights color codes with their actual colors |
| `find_references.ts` | Find references across the codebase |
| `clangd_support.ts` | Clangd-specific LSP features (switch header/source) |

### Editing Modes

| Plugin | Description |
|--------|-------------|
| `markdown_compose.ts` | Semi-WYSIWYG markdown editing with soft breaks |
| `merge_conflict.ts` | 3-way merge conflict resolution |

### Development/Testing

| Plugin | Description |
|--------|-------------|
| `test_view_marker.ts` | Testing utilities for view markers |

---

## Example Plugins

The `examples/` directory contains educational examples demonstrating specific API features:

| Example | Description |
|---------|-------------|
| `hello_world.ts` | Minimal plugin demonstrating command registration |
| `async_demo.ts` | Async process spawning |
| `buffer_query_demo.ts` | Buffer state querying |
| `virtual_buffer_demo.ts` | Creating virtual buffers with text properties |
| `bookmarks.ts` | Bookmark management example |
| `git_grep.ts` | Git grep implementation example |

---

## Plugin Development

For plugin development guides, see:
- **Getting Started:** [`docs/PLUGIN_DEVELOPMENT.md`](../docs/PLUGIN_DEVELOPMENT.md)
- **API Reference:** [`docs/plugin-api.md`](../docs/plugin-api.md)
- **Examples:** [`examples/README.md`](examples/README.md)
- **Clangd Plugin:** [`clangd_support.md`](clangd_support.md)

