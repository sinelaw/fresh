# Fresh

A terminal-based text editor.

This is a completely free and open source project, not owned by any commerical company.

**Status:** Alpha preview, early adopters version -0.1

![Fresh Screenshot](docs/screenshot1.png)

## Features

- **Ease of use** - Command palette and discoverable menus.
- **Huge file support** - Opens multi-gigabyte files in milliseconds
- **TypeScript plugins** - Extend the editor with plugins that run in a sandboxed Deno environment
- **LSP integration** - Diagnostics, completion, and go-to-definition out of the box
- **Powerful editing** - Multi-cursor support, macros, split views, etc.

## Installation

```bash
cargo build --release
./target/release/fresh [file]
```

## Documentation

- [User Guide](docs/USER_GUIDE.md)
- [Plugin Development](docs/PLUGIN_DEVELOPMENT.md)
- [Architecture](docs/ARCHITECTURE.md)
