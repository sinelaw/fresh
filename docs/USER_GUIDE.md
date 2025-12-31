# Fresh User Guide

Welcome to Fresh, a fast, extensible, and powerful terminal text editor. This guide will walk you through the core features of Fresh and help you get started with using and configuring the editor.

## Getting Started

### Installation

See the [Installation section in the README](../README.md#installation) for all available installation methods, including Homebrew, AUR, .deb/.rpm packages, npm, crates.io, and building from source.

### Running Fresh

To run Fresh, you can either open it without a file, or specify a file to open:

```bash
# Open an empty buffer
fresh

# Open a file
fresh src/main.rs
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

#### Configuring Language Detection via Settings UI

You can also configure language detection using the Settings UI instead of editing `config.json` directly:

1. **Open Settings**: Press `Ctrl+,` or use the command palette (`Ctrl+P`) and search for "Settings"
2. **Navigate to Languages**: Go to the **Languages** section
3. **Add or Edit a Language**: Click on an existing language to edit it, or add a new one
4. **Configure Detection**: Set the following fields:
   - **Extensions**: File extensions that should use this language (e.g., `cs` for C#, `rs` for Rust)
   - **Filenames**: Specific filenames without extensions (e.g., `Makefile`, `.bashrc`, `.zshrc`)
   - **Grammar**: The syntax highlighting grammar to use (must match a grammar name from syntect)

##### Example: Adding Shell Script Detection for Dotfiles

To make Fresh recognize `.bashrc`, `.zshrc`, and similar files as shell scripts:

1. Open Settings (`Ctrl+,`)
2. Go to **Languages** → **bash** (or create a new `bash` entry)
3. Add filenames: `.bashrc`, `.zshrc`, `.bash_profile`, `.profile`
4. The grammar should be set to `Bourne Again Shell (bash)` or similar

Fresh checks filenames first, then extensions, allowing dotfiles without traditional extensions to get proper syntax highlighting.

## Plugins

Fresh's functionality can be extended with plugins written in TypeScript. Fresh comes with a few useful plugins out of the box:

*   **TODO Highlighter:** Highlights `TODO`, `FIXME`, and other keywords in your comments.
*   **Git Grep:** Interactively search through your Git repository.
*   **Git Find File:** Quickly find and open files in your Git repository.

> On macOS, plugins folder needs to live either in the same directory as the binary OR in the directory that fresh is run from. If installed via homebrew, the binary lives in ```/opt/homebrew/bin/fresh```. The simplest, cleanest way to to create a symbolic link in that folder pointing to your plugins. i.e. ``` ln -s /Users/username/freshplugins /opt/homebrew/bin/plugins```

### Clangd helper plugin

Fresh ships `plugins/clangd_support.ts` with the source tree; see `plugins/clangd_support.md` for an overview of the plugin commands and how it surfaces clangd-specific notifications in the status bar.

## Configuration

Fresh uses a layered configuration system that allows you to customize settings at different levels of scope.

### Configuration Layers

Settings are loaded from multiple layers, with higher layers overriding lower ones:

| Layer | Location | Scope | Use Case |
|-------|----------|-------|----------|
| **System** | Built-in defaults | Global | Factory defaults (read-only) |
| **User** | `~/.config/fresh/config.json` | All projects | Personal preferences |
| **Project** | `.fresh/config.json` in project root | Single project | Project-specific settings |
| **Session** | `.fresh/session.json` (temporary) | Current session | Temporary overrides |

**Path Notes:**
- On Windows, User config is at `%APPDATA%\fresh\config.json`
- Project config is found by searching up from the current directory for `.fresh/config.json`

### How Layers Are Merged

When Fresh loads configuration, it merges all layers together. The merge behavior depends on the type of setting:

#### Simple Values (strings, numbers, booleans)

Higher layers completely override lower layers. If a setting is not specified in a higher layer, it falls through to the next lower layer.

```
System: theme = "default"    ← Base default
User:   theme = "dark"       ← Overrides system
Project: (not set)           ← Falls through
Session: theme = "light"     ← Final value: "light"
```

#### Nested Objects (editor, terminal, file_explorer)

Nested objects are **deep-merged** field by field. Each field follows the same "higher wins" rule independently.

**Example:** If User sets `editor.tab_size = 4` and Project sets `editor.line_wrap = true`:
```json
// User config
{ "editor": { "tab_size": 4, "line_numbers": true } }

// Project config
{ "editor": { "line_wrap": true } }

// Result: All fields merged
{ "editor": { "tab_size": 4, "line_numbers": true, "line_wrap": true } }
```

#### Languages Map (deep merge)

The `languages` map uses **deep merging with field-level override**:
- Entries from all layers are combined (you can add new languages at any layer)
- For the same language key, individual fields are merged (not replaced entirely)

**Example:** Extending built-in Rust settings in your project:
```json
// System (built-in): rust has extensions, grammar, etc.
// Project config - only need to specify what you're changing:
{
  "languages": {
    "rust": {
      "tab_size": 2,
      "format_on_save": true
    }
  }
}
// Result: Rust keeps all system defaults, with tab_size and format_on_save overridden
```

#### LSP Map (shallow merge)

The `lsp` map uses **shallow merging**:
- Entries from all layers are combined
- For the same language key, the **entire config is replaced** (not field-merged)

**Example:** To override just one LSP setting, you must specify the complete config:
```json
{
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "args": [],
      "enabled": true,
      "initialization_options": { "checkOnSave": { "command": "clippy" } }
    }
  }
}
```

#### Lists (keybindings, on_save actions)

Lists are **replaced entirely** by higher layers - they are not merged or appended.

**Example:** If you define `keybindings` in your Project config, it completely replaces User keybindings (not extends them).

#### Removing/Unsetting Values

There is currently no explicit mechanism to "remove" or "unset" a value defined in a lower layer. You can only override values with different settings. For boolean settings, you can set them to `false` to disable a feature enabled in a lower layer.

### Using the Settings UI

The easiest way to configure Fresh is through the Settings UI:

1. **Open Settings**: Press `Ctrl+,` or use Command Palette → "Open Settings"
2. **Browse Categories**: Use arrow keys or click to navigate
3. **Change Values**: Toggle booleans, adjust numbers, select from dropdowns
4. **Choose Target Layer**: Click the layer button (e.g., `[ User ]`) to switch between User/Project/Session
5. **Save**: Press Enter on the Save button or use `Ctrl+S`

**Advanced: Edit Config File Directly**

For complex configurations (like LSP args or custom keybindings), click the `[ Edit ]` button in the Settings footer to open the raw JSON config file for the selected layer.

### Example Configurations

**User config** (`~/.config/fresh/config.json`) - your personal defaults:
```json
{
  "version": 1,
  "theme": "dark",
  "editor": {
    "tab_size": 4,
    "line_numbers": true
  }
}
```

**Project config** (`.fresh/config.json`) - project-specific overrides:
```json
{
  "version": 1,
  "editor": {
    "tab_size": 2
  },
  "languages": {
    "javascript": {
      "formatter": "prettier --write"
    }
  }
}
```

### Common Configuration Tasks

#### Add a Custom Language

To add syntax highlighting and LSP support for a new language:

```json
{
  "languages": {
    "mylang": {
      "extensions": ["ml", "myl"],
      "grammar": "mylang",
      "comment_prefix": "#",
      "auto_indent": true
    }
  },
  "lsp": {
    "mylang": {
      "command": "mylang-lsp",
      "args": ["--stdio"],
      "enabled": true
    }
  }
}
```

#### Customize LSP Settings

Configure initialization options for a language server:

```json
{
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "enabled": true,
      "initialization_options": {
        "checkOnSave": { "command": "clippy" }
      }
    }
  }
}
```

#### Project-Specific Tab Size

Create `.fresh/config.json` in your project:
```json
{
  "version": 1,
  "editor": {
    "tab_size": 2
  }
}
```

### Layer Source Indicators

In the Settings UI, each setting shows where its current value comes from:
- **(user)** - Set in your User config
- **(project)** - Set in the Project config
- **(session)** - Temporary session override
- *(no indicator)* - Using system default

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

## Keyboard Config

Many OSes, window managers and terminal applications capture keys and filter them out so that applications like Fresh, running in the terminal, don't actually have a chance to handle those keys.

### Linux: XFCE window manager Ctrl + Alt + Up/Down keys - Disabling Workspace Switching Shortcuts

Follow these steps to clear the **Ctrl + Alt + Up** and **Ctrl + Alt + Down** shortcuts so they can be used in other applications (like `fresh`).

---

#### Step-by-Step Instructions

1.  **Open Settings**: Open the XFCE Application Menu and go to **Settings** > **Window Manager**.
2.  **Navigate to Keyboard**: Click on the **Keyboard** tab.
3.  **Find Workspace Shortcuts**: Scroll through the list of actions to find:
    * `Upper workspace`
    * `Bottom workspace`
4.  **Clear First Shortcut (Up)**:
    * Select the row for **Upper workspace** (usually mapped to `Ctrl+Alt+Up`).
    * Click the **Clear** button (or double-click the row and press **Backspace**).
5.  **Clear Second Shortcut (Down)**:
    * Select the row for **Bottom workspace** (usually mapped to `Ctrl+Alt+Down`).
    * Click the **Clear** button.
6.  **Close**: Click **Close** to save the changes.

---

#### Configuration Summary

| Action | Default Shortcut | New Setting |
| :--- | :--- | :--- |
| **Upper workspace** | `Ctrl + Alt + Up` | *Cleared / None* |
| **Bottom workspace** | `Ctrl + Alt + Down` | *Cleared / None* |

*Note: If you still experience issues, check **Settings** > **Keyboard** > **Application Shortcuts** to ensure no custom commands are overriding these keys.*

### macOS: Shift + Arrow Key Shortcuts in Terminal.app

Follow these steps to map **Shift + Up** and **Shift + Down** to specific escape sequences in your macOS Terminal.

---

#### Step-by-Step Instructions

1.  **Open Settings**: Launch Terminal and go to **Terminal** > **Settings** (or press `Cmd + ,`).
2.  **Navigate to Keyboard**: Click the **Profiles** tab, then select the **Keyboard** sub-tab.
3.  **Add First Shortcut (Cursor Up)**:
    * Click the **Plus (+)** icon at the bottom left of the list.
    * **Key**: Select `Cursor Up`.
    * **Modifier**: Select `Shift`.
    * **Action**: Select `Send Text`.
    * **Input**: Type `\033[1;2A`
    * Click **OK**.
4.  **Add Second Shortcut (Cursor Down)**:
    * Click the **Plus (+)** icon again.
    * **Key**: Select `Cursor Down`.
    * **Modifier**: Select `Shift`.
    * **Action**: Select `Send Text`.
    * **Input**: Type `\033[1;2B`
    * Click **OK**.

---

#### Configuration Summary

| Shortcut | Key | Modifier | Action | Escape Sequence |
| :--- | :--- | :--- | :--- | :--- |
| **Shift + Up** | Cursor Up | Shift | Send Text | `\033[1;2A` |
| **Shift + Down** | Cursor Down | Shift | Send Text | `\033[1;2B` |

## Troubleshooting

### Terminal Color Support

Fresh automatically detects your terminal's color capability and converts theme colors accordingly. Most modern terminals support 24-bit "truecolor", but some terminals and multiplexers have limited support.

#### Color Modes

- **Truecolor (24-bit)**: Full RGB color support (16 million colors). Used by modern terminals like Kitty, Alacritty, iTerm2, and most others with `COLORTERM=truecolor`.
- **256 colors**: Extended palette. Used by xterm-256color and similar terminals.
- **16 colors**: Basic ANSI colors. Used by the Linux console and very old terminals.

#### Terminal Multiplexers

GNU Screen and tmux add a layer between your terminal and Fresh, which can affect color rendering:

- **GNU Screen**: Does not support truecolor. Fresh automatically uses 256 colors when `TERM` starts with `screen`.
- **tmux**: Supports 256 colors by default. Some configurations support truecolor with `TERM=tmux-direct`.

#### Manual Override

If colors look wrong, you can force a specific color mode with the `FRESH_COLOR_MODE` environment variable:

```bash
# Force 256-color mode (recommended for GNU Screen)
FRESH_COLOR_MODE=256 fresh

# Force 16-color mode
FRESH_COLOR_MODE=16 fresh

# Force truecolor (if auto-detection is wrong)
FRESH_COLOR_MODE=truecolor fresh
```

#### Common Issues

| Symptom | Likely Cause | Solution |
| :--- | :--- | :--- |
| Colors look completely wrong | Truecolor detected but not supported | Use `FRESH_COLOR_MODE=256` |
| Weird artifacts/rendering issues | Terminal multiplexer interference | Try `FRESH_COLOR_MODE=256` or check TERM |
| Very limited/ugly colors | 16-color mode detected | Check your terminal supports 256 colors |

#### Checking Your Terminal

```bash
# Check TERM variable
echo $TERM

# Check COLORTERM (if set, indicates truecolor support)
echo $COLORTERM
```

## Advanced Topics

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
