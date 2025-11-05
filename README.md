# Editor

A fast, lightweight terminal text editor written in Rust. Handles files of any size with instant startup, low memory usage, and modern IDE features.

## Why This Editor?

- **Instant load of huge files** - Open and edit 1GB+ files instantly with syntax highlighting
- **Lightweight** - Minimal memory footprint, fast startup
- **No dependencies** - Single binary, works in any terminal
- **IDE features** - LSP support for completion, diagnostics, go-to-definition
- **Powerful editing** - Multiple cursors, split views, unlimited undo
- **Responsive** - Never freezes, even with slow file systems or network drives

## Performance

- **Instant startup** - No initialization delay, even for large files
- **Large file support** - Efficiently handles files of arbitrary size (tested with 1GB+)
- **Low memory** - Lazy loading and efficient data structures keep memory usage minimal
- **60fps** - Responsive UI with predictable latency
- **Fast operations** - Sub-millisecond insert/delete/navigation

## Features

### Editing
- **Multiple cursors** - Ctrl+D to select next occurrence, edit multiple locations at once
- **Unlimited undo/redo** - Complete edit history
- **Advanced selection** - Select word, line, or expand selection incrementally
- **Split views** - Work on multiple files side-by-side with nested splits
- **Smart scrolling** - Viewport automatically follows your cursor

### Language Support
- **LSP integration** - Native support for Language Server Protocol
  - Real-time diagnostics (errors, warnings)
  - Code completion
  - Go-to-definition
  - Works with rust-analyzer, typescript-language-server, pyright, etc.
- **Syntax highlighting** - Tree-sitter based, supports Rust, JavaScript, TypeScript, Python, and more
- **Multiple languages** - Concurrent language servers for different file types

### File Management
- **File explorer** - Built-in tree view with gitignore support
- **Multiple buffers** - Tab-based interface for multiple files
- **Position history** - Navigate back/forward through your edit locations (Alt+Left/Right)
- **Fast file operations** - Non-blocking I/O, works smoothly even on network drives

### Developer Experience
- **Command palette** - Ctrl+P for fuzzy command search
- **Help system** - Ctrl+H shows all keybindings
- **Fully configurable** - JSON config for keybindings, themes, LSP servers
- **Multiple themes** - Dark, light, and high-contrast themes included

## Requirements

- A terminal emulator (any modern terminal works)
- Rust toolchain (for building from source)

That's it. No other dependencies required.

## Quick Start

### Build
```bash
cargo build --release
```

### Run
```bash
./target/release/editor [file]
```

Open any file, including large files:
```bash
./target/release/editor large_log_file.txt
./target/release/editor src/main.rs
```

## Essential Keybindings

| Action | Key |
|--------|-----|
| **Getting Help** |
| Command palette | Ctrl+P |
| Show all keybindings | Ctrl+H |
| **File Operations** |
| Open file | Ctrl+O |
| Save | Ctrl+S |
| File explorer | Ctrl+B |
| **Editing** |
| Undo/Redo | Ctrl+Z / Ctrl+Y |
| Select next occurrence | Ctrl+D |
| Select word | Ctrl+W |
| Select line | Ctrl+L |
| **Navigation** |
| Go to definition | Ctrl+B |
| Navigate back/forward | Alt+Left / Alt+Right |
| **Code** |
| Completion | Ctrl+Space |
| **Layout** |
| Split horizontal | Alt+H |
| Split vertical | Alt+V |
| Next split | Alt+O |

Press **Ctrl+H** inside the editor to see all keybindings.

## Configuration

Configuration file: `~/.config/editor/config.json`

```json
{
  "theme": {
    "name": "dark"
  },
  "editor": {
    "tab_size": 4,
    "line_numbers": true
  },
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "enabled": true
    }
  },
  "file_explorer": {
    "show_hidden": false,
    "respect_gitignore": true
  }
}
```

All keybindings, colors, and LSP servers are configurable.

## Large File Support

This editor is designed to handle files of any size:

- **Instant loading** - Files open immediately regardless of size
- **Viewport-only parsing** - Only highlights visible text (enables instant load of huge files)
- **Efficient data structure** - Rope-based buffer with O(log n) operations
- **Lazy loading** - File explorer only loads directories when expanded
- **Streaming I/O** - Non-blocking file operations

Tested and works smoothly with multi-GB log files.

## Architecture

Built with Rust for performance and reliability. Key design choices:

- **Hybrid async/sync** - Main loop is synchronous for low latency, I/O is async for responsiveness
- **Event-driven** - All edits go through an event log (enables unlimited undo, future collaboration)
- **Pluggable backends** - Filesystem and LSP abstracted for extensibility
- **Zero-copy where possible** - Efficient memory usage via Arc and structural sharing

See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for implementation details.

## Documentation

- [ARCHITECTURE.md](docs/ARCHITECTURE.md) - Implementation details
- [TODO.md](docs/TODO.md) - Roadmap and planned features
- [TESTING.md](docs/TESTING.md) - Testing strategy
- [LSP_ARCHITECTURE.md](docs/LSP_ARCHITECTURE.md) - LSP integration
- [FILE_EXPLORER.md](docs/FILE_EXPLORER.md) - File explorer details

## Testing

Comprehensive test suite:
- 165 unit tests
- 59 end-to-end tests
- Property-based tests

```bash
cargo test                    # Run all tests
cargo test --lib              # Unit tests only
cargo test --test e2e_tests   # E2E tests only
```

## License

Copyright (c) Noam Lewis
