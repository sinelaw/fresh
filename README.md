# Editor

A high-performance terminal text editor written in Rust with async I/O, LSP integration, and sophisticated buffer management.

## Features

### Core Editing
- **Multiple cursors** - Ctrl+D for next match, full multi-cursor support
- **Event-driven architecture** - Lossless edit history with undo/redo
- **Advanced selection** - Word, line, and expand-selection operations
- **Smart scrolling** - Automatic viewport management for cursors
- **Split views** - Nested horizontal/vertical splits

### Language Features
- **LSP integration** - Diagnostics, completion, go-to-definition (async, non-blocking)
- **Syntax highlighting** - Tree-sitter based, viewport-only parsing for large files
- **Multiple language servers** - One per language, managed concurrently

### File Management
- **File explorer** - Lazy-loading tree with gitignore support
- **Multiple buffers** - Tab-based interface
- **Position history** - Alt+Left/Right navigation like VS Code

### Developer Experience
- **Command palette** - Ctrl+P with fuzzy matching
- **Configurable** - JSON-based config for keybindings, themes, LSP
- **Themeable** - Dark, light, and high-contrast themes included

## Architecture Highlights

### Hybrid Async/Sync
Main loop runs synchronously at ~60fps for predictable latency. All I/O (LSP, file operations) runs in async Tokio tasks. Communication via non-blocking message channels - no locks in main loop.

### Iterator Edit Resilience
Buffer iterators automatically adjust their position when edits occur. Two-level caching (ChunkTree snapshot + 4KB buffer) achieves ~4096x fewer lock operations.

### Viewport-Only Parsing
Syntax highlighting only parses ~50 visible lines. This allows instant loading of 1GB+ files while providing real-time highlighting.

### Gap-Aware Rope
ChunkTree is a persistent rope-like structure with gap support. Inserting far beyond EOF creates gaps efficiently without allocating intervening space.

### Lazy File Tree
File explorer only loads expanded directories. Collapse operations immediately free memory. Path-to-node HashMap provides O(1) lookups.

### Edit Log Garbage Collection
Event history tracks active iterator versions. After each edit, finds minimum version (low-water mark) and prunes older events, bounding memory usage.

## Building

```bash
cargo build --release
```

## Running

```bash
cargo run --release -- [file]
```

## Testing

```bash
cargo test                           # All tests
cargo test --lib                     # Unit tests
cargo test --test e2e_tests          # E2E tests
```

- **165 unit tests** - Core data structures
- **59 E2E tests** - Full integration via virtual terminal
- **Property tests** - Invariants and round-trips
- **Hermetic E2E** - Isolated temp directories per test

## Key Keybindings

| Action | Key |
|--------|-----|
| Command palette | Ctrl+P |
| Help | Ctrl+H |
| Save | Ctrl+S |
| Open file | Ctrl+O |
| Next occurrence | Ctrl+D |
| Undo/Redo | Ctrl+Z / Ctrl+Y |
| Go to definition | Ctrl+B |
| Completion | Ctrl+Space |
| Split horizontal | Alt+H |
| Split vertical | Alt+V |
| File explorer | Ctrl+B |
| Navigate back/forward | Alt+Left/Right |

## Configuration

Edit `~/.config/editor/config.json`:

```json
{
  "theme": {
    "name": "dark",
    "background": "#1e1e1e",
    "foreground": "#d4d4d4"
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
  }
}
```

See `docs/` for detailed architecture and testing documentation.

## Documentation

- [ARCHITECTURE.md](docs/ARCHITECTURE.md) - Design and implementation details
- [TODO.md](docs/TODO.md) - Roadmap and future work
- [TESTING.md](docs/TESTING.md) - Testing strategy
- [LSP_ARCHITECTURE.md](docs/LSP_ARCHITECTURE.md) - LSP integration details
- [FILE_EXPLORER.md](docs/FILE_EXPLORER.md) - File explorer implementation

## License

Copyright (c) Noam Lewis
