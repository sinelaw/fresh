# Fresh User Guide

Welcome to Fresh, a fast, extensible, and powerful terminal text editor. This guide will walk you through the core features of Fresh and help you get started with using and configuring the editor.

## Getting Started

### Installation

To get started with Fresh, you'll need to have the Rust toolchain installed on your system. You can then build the editor from source:

```bash
cargo build --release
```

### Running Fresh

To run Fresh, you can either open it without a file, or specify a file to open:

```bash
# Open an empty buffer
./target/release/fresh

# Open a file
./target/release/fresh src/main.rs
```

### Core Concepts

*   **The Command Palette:** The command palette is your central hub for accessing all of Fresh's features. Press `Ctrl+P` to open it, and then start typing to search for commands.
*   **Buffers:** Each open file is represented as a buffer. You can have multiple buffers open at once and switch between them.
*   **Splits:** You can split your editor view horizontally or vertically to view multiple buffers at once.
*   **The Status Bar:** The status bar at the bottom of the screen displays information about the current buffer, including the file name, cursor position, and Git branch.

## Core Features

### Editing

Fresh provides a powerful set of editing features to help you be more productive.

*   **Multiple Cursors:** Use `Ctrl+D` to select the next occurrence of the current word and create a new cursor. This allows you to edit multiple places in your code at once.
*   **Advanced Selection:** Fresh provides a variety of ways to select text, including word selection (`Ctrl+W`), line selection (`Ctrl+L`), and expanding the selection incrementally.
*   **Unlimited Undo/Redo:** Fresh has a complete edit history, so you can undo and redo changes as much as you need to.

### Navigation

*   **Go to Definition:** Use `Ctrl+B` to jump to the definition of a symbol under the cursor (requires LSP).
*   **Position History:** Navigate back and forward through your edit locations using `Alt+Left` and `Alt+Right`.

### File Explorer

Fresh includes a built-in file explorer to help you navigate your project's files.

*   **Toggle:** Use `Ctrl+B` to open and close the file explorer.
*   **Navigation:** Use the arrow keys to move up and down the file tree.
*   **Open Files:** Press `Enter` to open the selected file.
*   **Gitignore Support:** The file explorer respects your `.gitignore` file, hiding ignored files by default.

### Search and Replace

Fresh provides a powerful search and replace feature with support for regular expressions and interactive replacement.

*   **Search:** Press `Ctrl+F` to open the search prompt.
*   **Replace:** Press `Ctrl+R` to open the search and replace prompt.

### LSP Integration

Fresh has native support for the Language Server Protocol (LSP), providing features like:

*   **Real-time diagnostics:** See errors and warnings in your code as you type.
*   **Code completion:** Get intelligent code completion suggestions.
*   **Go-to-definition:** Quickly jump to the definition of a symbol.

You can configure LSP servers for your favorite languages in the `config.json` file.

## Plugins

Fresh's functionality can be extended with plugins written in TypeScript. Fresh comes with a few useful plugins out of the box:

*   **TODO Highlighter:** Highlights `TODO`, `FIXME`, and other keywords in your comments.
*   **Git Grep:** Interactively search through your Git repository.
*   **Git Find File:** Quickly find and open files in your Git repository.

## Configuration

Fresh is configured using a `config.json` file located in `~/.config/fresh/config.json`.

### Example Configuration

```json
{
  "theme": "dark",
  "editor": {
    "tab_size": 4
  },
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "enabled": true
    }
  }
}
```

### Process Resource Limits

To prevent LSP servers from consuming too many resources, Fresh can limit their memory and CPU usage. This is configured in the `process_limits` section of your `config.json` file.

```json
{
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "enabled": true,
      "process_limits": {
        "max_memory_mb": 4096,
        "max_cpu_percent": 200
      }
    }
  }
}
```

For more information on how to configure resource limits, see the `docs/PROCESS_LIMITS.md` file.

## Advanced Topics

### Script Control Mode

Fresh can be launched in a special "script control mode" that allows external programs to control the editor. This is useful for integrations with tools like AI assistants. For more information, see `docs/SCRIPT_CONTROL_MODE.md`.

### Visual Regression Testing

Fresh uses a visual regression testing system to ensure that UI changes are intentional. For more information, see `docs/VISUAL_REGRESSION_TESTING.md`.

## Keybindings

| Action                 | Key                   |
| ---------------------- | --------------------- |
| **General**            |
| Command Palette        | `Ctrl+P`              |
| Show Keybindings       | `Ctrl+H`              |
| **File**               |
| Open File              | `Ctrl+O`              |
| Save File              | `Ctrl+S`              |
| **Editing**            |
| Undo                   | `Ctrl+Z`              |
| Redo                   | `Ctrl+Y`              |
| Select Next Occurrence | `Ctrl+D`              |
| **Navigation**         |
| Go to Definition       | `Ctrl+B`              |
| Back                   | `Alt+Left`            |
| Forward                | `Alt+Right`           |
| **Layout**             |
| Split Horizontal       | `Alt+H`               |
| Split Vertical         | `Alt+V`               |
| Next Split             | `Alt+O`               |
| File Explorer          | `Ctrl+B`              |
