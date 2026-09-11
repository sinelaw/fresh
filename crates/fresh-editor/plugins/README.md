# Plugins

This directory contains production-ready plugins for the editor. Plugins are written in **TypeScript** and run in a sandboxed QuickJS environment (transpiled via oxc_transformer). They are automatically loaded when the editor starts.

## Available Plugins

### Core Plugins

| Plugin | Description |
|--------|-------------|
| `welcome_screen.ts` | The welcome screen: a scrollable onboarding buffer shown when there is nothing else open |
| `welcome.ts` | Plugin-API demo commands (say hello, insert time) |
| `manual_help.ts` | Manual page and keyboard shortcuts display |
| `diagnostics_panel.ts` | LSP diagnostics panel with navigation |
| `dap.ts` | Debug Adapter Protocol client (launch, breakpoints, stepping) |
| `lsp_help.ts` | Install instructions when a language server is missing (all languages whose helper only offers install help) |
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
| `markdown_toc.ts` | Markdown table of contents as a sidebar section (headings tree, cursor sync, click to jump) |
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
- **Getting Started:** [`docs/development/plugin-development.md`](../docs/development/plugin-development.md)
- **API Reference:** [`docs/development/plugin-api.md`](../docs/development/plugin-api.md)
- **Examples:** [`examples/README.md`](examples/README.md)
- **Clangd Plugin:** [`clangd_support.md`](clangd_support.md)

## Debug adapters

The `dap.ts` plugin reads VS Code-compatible launch configurations from
`.vscode/launch.json`. Configure the executable for each adapter type in
`config.json`; the adapter itself must already be installed:

```json
{
  "plugins": {
    "dap": {
      "settings": {
        "adapters": [
          {
            "type": "python",
            "command": "python",
            "args": ["-m", "debugpy.adapter"]
          }
        ]
      }
    }
  }
}
```

Use the command palette actions beginning with `Debug:` to start or stop a
session, toggle breakpoints, continue, pause, and step. Set
`plugins.dap.settings.configuration` when `launch.json` contains more than one
configuration; otherwise the first entry is used.
