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

# Open a file at a specific line
fresh src/main.rs:42

# Open a file at a specific line and column
fresh src/main.rs:42:10

# Open multiple files (with optional line:col)
fresh Cargo.toml src/lib.rs:100:5
```

The `file:line:col` syntax is useful for jumping directly to compiler errors or search results.

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

#### Built-in LSP Support

Fresh includes built-in LSP configurations for many popular languages. Simply install the language server and Fresh will use it automatically:

| Language | LSP Server | Install Command |
|----------|-----------|-----------------|
| Rust | rust-analyzer | `rustup component add rust-analyzer` |
| Go | gopls | `go install golang.org/x/tools/gopls@latest` |
| TypeScript/JavaScript | typescript-language-server | `npm install -g typescript-language-server typescript` |
| Python | pylsp | `pip install python-lsp-server` |
| Java | jdtls | `brew install jdtls` |
| Zig | zls | `brew install zls` |
| LaTeX | texlab | `brew install texlab` |
| Markdown | marksman | `brew install marksman` |
| C/C++ | clangd | `brew install llvm` |

#### Python LSP Configuration

Fresh includes built-in support for Python with `pylsp` (Python Language Server). However, you can also use alternative Python language servers:

**Using pyright (recommended for type checking):**

```json
{
  "lsp": {
    "python": {
      "command": "pyright-langserver",
      "args": ["--stdio"],
      "enabled": true
    }
  }
}
```

Install pyright with: `npm install -g pyright` or `pip install pyright`

**Using basedpyright (enhanced pyright fork):**

```json
{
  "lsp": {
    "python": {
      "command": "basedpyright-langserver",
      "args": ["--stdio"],
      "enabled": true
    }
  }
}
```

Install basedpyright with: `pip install basedpyright` or `uv pip install basedpyright`

**Using pylsp with plugins:**

pylsp supports various plugins for enhanced functionality:

```json
{
  "lsp": {
    "python": {
      "command": "pylsp",
      "args": [],
      "enabled": true,
      "initialization_options": {
        "pylsp": {
          "plugins": {
            "pycodestyle": { "enabled": true },
            "pylint": { "enabled": true }
          }
        }
      }
    }
  }
}
```

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

## Themes

Fresh supports customizable color themes for the editor, UI elements, and syntax highlighting.

### Selecting a Theme

Use the command palette (`Ctrl+P`) and search for "Select Theme" to choose from available themes. Built-in themes and user themes are both shown.

### Creating and Editing Themes

Fresh includes a visual Theme Editor for creating and customizing themes:

1. **Open the Theme Editor**: Press `Ctrl+P` and search for "Edit Theme"

2. **The Theme Editor Interface**:
   - Color fields show a preview swatch next to each value
   - Sections can be collapsed/expanded with `Enter`
   - Navigate with `Up/Down` arrows or `Tab/Shift+Tab`

3. **Editing Colors**:
   - Press `Enter` on any color field to edit it
   - Enter a hex color (`#RRGGBB`) or named color (e.g., `red`, `blue`)
   - Colors are applied immediately as you edit

4. **Theme Editor Shortcuts**:
   | Action | Key |
   | ------ | --- |
   | Open theme | `Ctrl+O` |
   | Save | `Ctrl+S` |
   | Save As | `Ctrl+Shift+S` |
   | Delete theme | `Ctrl+D` |
   | Close | `Ctrl+Q` or `Escape` |
   | Help | `F1` |

5. **Working with Built-in Themes**:
   - Built-in themes cannot be modified directly
   - Use "Save As" (`Ctrl+Shift+S`) to create a copy that you can customize
   - Your custom themes are saved to `~/.config/fresh/themes/`

6. **Theme Structure**:
   - **Editor**: Main editor colors (background, foreground, cursor, selection)
   - **UI Elements**: Interface colors (tabs, menus, status bar)
   - **Search**: Search result highlighting
   - **Diagnostics**: LSP diagnostic colors (errors, warnings)
   - **Syntax Highlighting**: Code colors (keywords, strings, comments)

### Theme File Format

Themes are stored as JSON files. You can also edit them directly at `~/.config/fresh/themes/`. Example:

```json
{
  "name": "my-theme",
  "editor": {
    "bg": [30, 30, 30],
    "fg": [212, 212, 212],
    "cursor": [82, 139, 255],
    "selection_bg": [38, 79, 120]
  },
  "syntax": {
    "keyword": [86, 156, 214],
    "string": [206, 145, 120],
    "comment": [106, 153, 85]
  }
}
```

Colors are specified as `[R, G, B]` arrays with values from 0-255.

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

#### LSP Map (deep merge)

The `lsp` map uses **deep merging with field-level override**:
- Entries from all layers are combined
- For the same language key, individual fields are merged (not replaced entirely)
- Unspecified fields inherit from lower layers (you only need to specify what you're changing)

**Example:** To disable an LSP while preserving its default command:
```json
{
  "lsp": {
    "rust": {
      "enabled": false
    }
  }
}
// Result: rust-analyzer command preserved from defaults, just disabled
```

**Example:** To add initialization options without repeating the command:
```json
{
  "lsp": {
    "rust": {
      "initialization_options": { "checkOnSave": { "command": "clippy" } }
    }
  }
}
// Result: command="rust-analyzer" (from defaults) + your initialization_options
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

## Privacy & Telemetry

Fresh checks for new versions to notify you when upgrades are available. Alongside this, it sends basic anonymous telemetry to help understand usage patterns. Both are part of the same daily check.

The data collected includes:

- Fresh version
- Operating system and architecture (e.g., `linux-x86_64`, `macos-aarch64`)
- Terminal type (the `TERM` environment variable)

No personal data, file contents, or usage behavior is collected. The check runs once on startup and then once daily.

### Disabling Upgrade Checks and Telemetry

You can disable both upgrade checking and telemetry with the same flag:

**Command line flag:**
```bash
fresh --no-upgrade-check
```

**Configuration file** (`~/.config/fresh/config.json`):
```json
{
  "check_for_updates": false
}
```

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

### macOS Terminal Tips

**TL;DR: Recommended Terminals**
- **Kitty**: Best experience out of the box. Add `macos_option_as_alt left` to config.
- **Ghostty**: Best experience out of the box. Add `macos-option-as-alt = left` to config.
- **Terminal.app**: [Import Fresh.terminal profile](../scripts/macOS/Fresh.terminal) to fix keybindings.
- **iTerm2**: Follow the [configuration instructions](#iterm2-setup) below.

Fresh works best on macOS when you understand the interaction between the operating system, your terminal emulator, and the editor. This section covers common issues and recommended configurations.

#### Using the macOS Keymap

Fresh includes a dedicated macOS keymap that addresses terminal-specific challenges. To use it, add to your `~/.config/fresh/config.json`:

```json
{
  "keymap": "macos"
}
```

The macOS keymap is designed around these constraints:

**Ctrl+Shift combinations don't work.** Some macOS terminals cannot reliably send Ctrl+Shift sequences. For example, Ctrl+Shift+Z produces a caron character (ˇ) instead of being recognized as a key chord. The macOS keymap uses Ctrl+Alt as an alternative modifier.

**Some Ctrl keys are ASCII control characters.** In terminal protocols, Ctrl+J is Line Feed (newline), Ctrl+M is Carriage Return (Enter), and Ctrl+I is Tab. Binding actions to these keys causes erratic behavior. The macOS keymap avoids these collisions.

**International keyboards use Alt for essential characters.** On German, French, and other ISO layouts, Alt (Option) combined with letters produces characters like @, [, ], {, and }. The macOS keymap avoids Alt+letter combinations that would block character input.

**Unix readline conventions are preserved.** Terminal users expect Ctrl+Y to "yank" (paste from the kill ring), Ctrl+K to kill to end of line, and Ctrl+U to kill to start of line. The macOS keymap respects these conventions rather than overriding them with GUI editor shortcuts.

Use the **Command Palette** (Ctrl+P) or **Show Keybindings** (Ctrl+H) to discover the actual key bindings, or view the keymap file directly at `keymaps/macos.json`.

#### Recommended Terminal Emulators

For the best experience with Fresh on macOS, use a terminal that supports the **Kitty Keyboard Protocol (KKP)** or **CSI u** for unambiguous key reporting:

| Terminal | KKP Support | Notes |
| :--- | :--- | :--- |
| **Kitty** | Full | Set `macos_option_as_alt left` in config |
| **Ghostty** | Full | Set `macos-option-as-alt = left` in config |
| **iTerm2** | CSI u | Requires configuration (see below) |
| **Terminal.app** | None | Requires manual key mappings (see below) |

#### iTerm2 Setup

To get the best experience with iTerm2, you need to enable CSI u support and configure the Option key.

1.  Go to **Settings** > **Profiles** > **Keys**.
2.  **General** tab:
    *   Check **Report keys using CSI u**. This allows Fresh to distinguish between combinations like `Ctrl+I` and `Tab`.
    *   Set **Left Option key** to **Esc+**. This treats the Option key as Meta/Alt.
    *   Set **Right Option key** to **Normal** if you use it for special characters (or Esc+ if you want it as Alt too).

![iTerm2 Profile Keys](macos-iterm-profile-keys.png)

#### Apple Terminal.app Setup

Apple's built-in Terminal requires manual configuration to work well with modern terminal editors.

**Option as Meta:**
1.  Go to **Settings** > **Profiles** > **Keyboard**.
2.  Check **Use Option as Meta key**.

![Terminal Option as Meta](mac-terminal-option-as-meta.png)

**Key Mappings:**
Fresh relies on Shift+Arrow keys for selection, but Terminal.app often doesn't send these by default.

**Easier Method: Import Profile**
We provide a pre-configured profile that sets up colors and key mappings for you.
1.  Locate `scripts/macOS/Fresh.terminal` in the repository.
2.  In Terminal.app, go to **Settings** > **Profiles**.
3.  Click the gear icon at the bottom of the sidebar and select **Import...**.
4.  Select the `Fresh.terminal` file.

**Manual Configuration:**
If you prefer to configure it manually:
1.  In **Settings** > **Profiles** > **Keyboard**, click the `+` button.
2.  Map **Shift + Cursor Up** to send text `\033[1;2A` (press Esc then type `[1;2A`).
3.  Map **Shift + Cursor Down** to send text `\033[1;2B`.

The full list of keys:

- Control + Option

    Up: \033[1;7A

    Down: \033[1;7B

    Right: \033[1;7C

    Left: \033[1;7D

- Control + Shift

    Up: \033[1;6A

    Down: \033[1;6B

    Right: \033[1;6C

    Left: \033[1;6D

- Shift

    Up: \033[1;2A

    Down: \033[1;2B

    Right: \033[1;2C

    Left: \033[1;2D


![Terminal Keymaps](mac-terminal-keymaps.png)

#### Keyboard Enhancement Flags

Fresh can use the Kitty Keyboard Protocol to get more accurate key reporting from supported terminals. You can configure which features to enable in your config file:

```json
{
  "editor": {
    "keyboard_disambiguate_escape_codes": true,
    "keyboard_report_event_types": false,
    "keyboard_report_alternate_keys": true,
    "keyboard_report_all_keys_as_escape_codes": false
  }
}
```

| Option | Default | Description |
| :--- | :--- | :--- |
| `keyboard_disambiguate_escape_codes` | `true` | Use CSI-u sequences for unambiguous escape/modifier key reading |
| `keyboard_report_event_types` | `false` | Report key repeat and release events (not just press) |
| `keyboard_report_alternate_keys` | `true` | Send alternate keycodes in addition to base keycodes |
| `keyboard_report_all_keys_as_escape_codes` | `false` | Report all keys (including plain text) as escape sequences |

These flags only take effect if your terminal supports the Kitty Keyboard Protocol. Fresh automatically detects support and falls back gracefully if the protocol is unavailable. If you experience keyboard issues, try disabling all flags by setting them to `false`.

#### Home and End Keys

On macOS, the Home and End keys scroll the terminal buffer by default instead of moving the cursor. Fresh's macOS keymap works around this by binding:

- **Ctrl+A** → Move to line start
- **Ctrl+E** → Move to line end
- **Ctrl+Shift+A** → Select to line start
- **Ctrl+Shift+E** → Select to line end

If you prefer using the actual Home/End keys, configure your terminal to send the proper escape sequences:

**iTerm2:**
1. Preferences → Profiles → Keys → Key Mappings
2. Add: Home → Send Escape Sequence → `[H`
3. Add: End → Send Escape Sequence → `[F`

#### Mission Control Conflicts

macOS uses **Ctrl+Arrow** keys for Mission Control desktop switching by default, which prevents these shortcuts from reaching terminal applications.

To use Ctrl+Arrow in Fresh for word movement or multi-cursor:

1. Open **System Settings** → **Keyboard** → **Keyboard Shortcuts** → **Mission Control**
2. Disable or rebind:
   - "Move left a space" (Ctrl+Left)
   - "Move right a space" (Ctrl+Right)
   - "Mission Control" (Ctrl+Up)
   - "Application windows" (Ctrl+Down)

Alternatively, Fresh's macOS keymap provides **Alt+Arrow** as the primary word movement binding, which doesn't conflict with Mission Control.

#### Option Key on International Keyboards

If you use Option to type special characters (like @ on German layouts), you should configure your terminal to treat only the **Left Option** as Meta/Alt, and keep the **Right Option** for character input. iTerm2 supports this configuration (see above).

#### International Keyboard Layouts

The macOS keymap disables Alt+0-9 bindings because these key combinations are used to type essential characters on many international keyboard layouts:

- **German**: Alt+L = @, Alt+5 = [, Alt+6 = ]
- **French**: Alt+( = {, Alt+) = }
- **Spanish**: Alt+2 = @, Alt+3 = #

If you find that certain Alt combinations insert characters instead of triggering editor commands, ensure your terminal's Option key is configured as Meta (see above).

## Internationalization (i18n)

Fresh supports multiple languages for its user interface. The editor automatically detects your system locale, but you can also set your preferred language manually.

### Supported Languages

See the [`locales/`](../locales/) directory for the full list of supported languages. Each `.json` file represents a supported locale (e.g., `en.json` for English, `es.json` for Spanish, `ja.json` for Japanese).

### Setting Your Language

You can configure your preferred language in `~/.config/fresh/config.json`:

```json
{
  "locale": "es"
}
```

Or use the Settings UI (`Ctrl+,`) and navigate to the **General** section to select your language.

### Plugin Translations

Plugins can provide their own translations. If a plugin supports i18n, it will automatically use your configured locale. Plugin translations are stored in `.i18n.json` files alongside the plugin.

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

Open command palette (Ctrl+P or ^P) and type the name of the command you want to run - if any keybinding is assigned, it will also be shown.

Alternatively, use Help -> Keyboard Shortcuts to view the full list.
