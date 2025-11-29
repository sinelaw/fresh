# Fresh

[Visit the official Fresh website](https://sinelaw.github.io/fresh/)

A terminal-based text editor.

## Discovery & Ease of Use

Fresh is designed for discovery. It features native UIs, a full Menu system, and a powerful Command Palette. With full mouse support, transitioning from graphical editors is seamless.

## Modern Extensibility

Extend Fresh easily using modern tools. Plugins are written in TypeScript and run securely in a sandboxed Deno environment, providing access to a modern JavaScript ecosystem without compromising stability.

## Zero-Latency Performance

Fresh is engineered for speed. It delivers a near zero-latency experience, with text appearing instantly. The editor is designed to be light and fast, reliably opening and editing huge files up to multi-gigabyte sizes without slowdown.

## Comprehensive Feature Set

- **File Management**: open/save/new/close, file explorer, tabs, auto-revert, git file finder
- **Editing**: undo/redo, multi-cursor, block selection, smart indent, comments, clipboard
- **Search & Replace**: incremental search, find in selection, query replace, git grep
- **Navigation**: go to line/bracket, word movement, position history, bookmarks, error navigation
- **Views & Layout**: split panes, line numbers, line wrap, backgrounds, markdown preview
- **Language Server (LSP)**: go to definition, references, hover, code actions, rename, diagnostics, autocompletion
- **Productivity**: command palette, menu bar, keyboard macros, git log, diagnostics panel
- **Plugins & Extensibility**: TypeScript plugins, color highlighter, TODO highlighter, merge conflicts, path complete, keymaps

![Fresh Screenshot](docs/screenshot1.png)
![Fresh Screenshot](docs/screenshot2.png)
![Fresh Screenshot](docs/screenshot3.png)

## Installation

### Via npm (recommended)

```bash
npm install -g @fresh-editor/fresh-editor
```

### Via npx (for a quick test)

```bash
npx @fresh-editor/fresh-editor
```

### Pre-built binaries

Download the latest release for your platform from the [releases page](httpss://github.com/sinelaw/fresh/releases).

### From crates.io

```bash
cargo install fresh-editor
```

### From source

```bash
git clone https://github.com/sinelaw/fresh.git
cd fresh
cargo build --release
./target/release/fresh [file]
```

## Documentation

- [User Guide](docs/USER_GUIDE.md)
- [Plugin Development](docs/PLUGIN_DEVELOPMENT.md)
- [Architecture](docs/ARCHITECTURE.md)

## License

Copyright (c) Noam Lewis

This project is licensed under the GNU General Public License v2.0 (GPL-2.0).
