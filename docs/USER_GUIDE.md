# Fresh User Guide

Welcome to Fresh, a fast, extensible, and powerful terminal text editor. This guide will walk you through the core features of Fresh and help you get started with using and configuring the editor.

## Getting Started

### Installation

See the [Installation section in the README](../README.md#installation) for all available installation methods, including Homebrew, AUR, .deb/.rpm packages, npm, crates.io, and building from source.

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

*   **Go to Definition:** Use the command palette (`Ctrl+P`) and search for "Go to Definition" to jump to the definition of a symbol under the cursor (requires LSP).
*   **Position History:** Navigate back and forward through your edit locations using `Alt+Left` and `Alt+Right`.

### File Explorer

Fresh includes a built-in file explorer to help you navigate your project's files.

*   **Toggle:** Use `Ctrl+E` to open and close the file explorer.
*   **Navigation:** Use the arrow keys to move up and down the file tree.
*   **Open Files:** Press `Enter` to open the selected file.
*   **Gitignore Support:** The file explorer respects your `.gitignore` file, hiding ignored files by default.

### Search and Replace

Fresh provides a powerful search and replace feature with support for regular expressions and interactive replacement.

*   **Search:** Press `Ctrl+F` to open the search prompt.
*   **Replace:** Press `Ctrl+R` to open the search and replace prompt.

### Integrated Terminal

Fresh includes a built-in terminal emulator that lets you run shell commands without leaving the editor.

#### Opening a Terminal

*   **Command Palette:** Press `Ctrl+P` and search for "Open Terminal"
*   **Multiple Terminals:** You can open multiple terminal tabs and switch between them like regular file buffers

#### Terminal Modes

The terminal has two modes, indicated in the status bar:

1.  **Terminal Mode** (status bar shows "Terminal"): Your keyboard input goes directly to the shell. Use this for typing commands and interacting with programs.

2.  **Scrollback Mode** (status bar shows "Terminal (read only)"): The terminal output becomes a read-only buffer that you can scroll through, search, and copy text from.

#### Switching Between Modes

*   **`Ctrl+Space`**: Toggle between terminal mode and scrollback mode
*   **`Ctrl+]`**: Exit terminal mode (same as `Ctrl+Space`)

#### Keyboard Capture

By default, most editor keybindings (like `Ctrl+P` for command palette) still work in terminal mode. If you need to send these keys to the terminal instead:

*   **`F9`**: Toggle keyboard capture mode
*   When keyboard capture is enabled (status bar shows "Terminal [capture]"), all keys except `F9` are sent to the terminal
*   **Visual indicator:** The UI dims (menu bar, status bar, other splits) to clearly show focus is exclusively on the terminal

#### Scrollback Navigation

In scrollback mode, you can use standard editor navigation:

*   **Arrow keys / Page Up / Page Down**: Scroll through output
*   **`Ctrl+Home`**: Jump to the beginning of scrollback history
*   **`Ctrl+End`**: Jump to the end
*   **`Ctrl+F`**: Search through terminal output

#### Tips and Quirks

*   **Session Persistence:** Terminal sessions are preserved when you close and reopen Fresh. Your scrollback history and running processes are maintained.
*   **Automatic Scroll:** When new output arrives while you're in scrollback mode, the terminal automatically returns to terminal mode to show the latest output. Disable this with the `terminal.jump_to_end_on_output` config option.
*   **Resizing:** The terminal automatically resizes when you resize the editor or split panes.

### LSP Integration

Fresh has native support for the Language Server Protocol (LSP), providing features like:

*   **Real-time diagnostics:** See errors and warnings in your code as you type.
*   **Code completion:** Get intelligent code completion suggestions.
*   **Go-to-definition:** Quickly jump to the definition of a symbol.

#### Configuring LSP for a New Language

To add LSP support for a language, you need to configure two sections in your `~/.config/fresh/config.json`:

1. **`languages`**: Define the file extensions for the language
2. **`lsp`**: Configure the language server command

For example, to add C# support:

```json
{
  "languages": {
    "csharp": {
      "extensions": ["cs"],
      "grammar": "c_sharp",
      "comment_prefix": "//",
      "auto_indent": true
    }
  },
  "lsp": {
    "csharp": {
      "command": "/path/to/csharp-language-server",
      "args": [],
      "enabled": true
    }
  }
}
```

The language name (e.g., `"csharp"`) must match in both sections. Fresh includes built-in language definitions for Rust, JavaScript, TypeScript, and Python, but you can add any language by configuring it in your config file.

## Plugins

Fresh's functionality can be extended with plugins written in TypeScript. Fresh comes with a few useful plugins out of the box:

*   **TODO Highlighter:** Highlights `TODO`, `FIXME`, and other keywords in your comments.
*   **Git Grep:** Interactively search through your Git repository.
*   **Git Find File:** Quickly find and open files in your Git repository.

> On macOS, plugins folder needs to live either in the same directory as the binary OR in the directory that fresh is run from. If installed via homebrew, the binary lives in ```/opt/homebrew/bin/fresh```. The simplest, cleanest way to to create a symbolic link in that folder pointing to your plugins. i.e. ``` ln -s /Users/username/freshplugins /opt/homebrew/bin/plugins```

### Clangd helper plugin

Fresh ships `plugins/clangd_support.ts` with the source tree; see `plugins/clangd_support.md` for an overview of the plugin commands and how it surfaces clangd-specific notifications in the status bar.

## Configuration

Fresh is configured using a `config.json` file located in:

* `~/.config/fresh/config.json` on Unix-like systems, like Linux and macOS.
* `%APPDATA%\fresh\config.json` on Windows.

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
| Go to Definition       | Command Palette       |
| Back                   | `Alt+Left`            |
| Forward                | `Alt+Right`           |
| **Layout**             |
| Split Horizontal       | `Alt+H`               |
| Split Vertical         | `Alt+V`               |
| Next Split             | `Alt+O`               |
| File Explorer          | `Ctrl+E`              |
| **Terminal**           |
| Toggle Terminal Mode   | `Ctrl+Space`          |
| Exit Terminal Mode     | `Ctrl+]`              |
| Toggle Keyboard Capture| `F9`                  |
| Paste in Terminal      | `Ctrl+V`              |
