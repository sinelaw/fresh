# Configuration

- [Overview](./) - Layered configuration system
- [Startup Script (init.ts)](./init.md) - Run TypeScript at startup for environment-dependent setup
- [Keyboard](./keyboard.md) - Keyboard shortcuts and keybinding configuration

---

Fresh uses layered configuration.

## Configuration Layers

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

## How Layers Are Merged

Fresh merges all layers. Merge behavior depends on the setting type:

### Simple Values (strings, numbers, booleans)

Higher layers override lower layers. If a setting is not specified in a higher layer, it falls through to the next lower layer.

```
System: theme = "default"    ← Base default
User:   theme = "dark"       ← Overrides system
Project: (not set)           ← Falls through
Session: theme = "light"     ← Final value: "light"
```

### Nested Objects (editor, terminal, file_explorer)

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

### Languages Map (deep merge)

The `languages` map uses **deep merging with field-level override**:
- Entries from all layers are combined (you can add new languages at any layer)
- For the same language key, individual fields are merged (not replaced entirely)
- Editor settings including `line_wrap`, `wrap_column`, `page_view`, and `page_width` can be set per-language — e.g. wrap Markdown at 80 columns while leaving code unwrapped

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

### LSP Map (deep merge)

> For the LSP feature itself (multi-server config, root markers, formatters, `only_features` / `except_features`, etc.), see [LSP Integration](../features/lsp.md). This section only covers how the `lsp` map is merged across config layers.

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

### Lists (keybindings, on_save actions)

Lists are **replaced entirely** by higher layers - they are not merged or appended.

**Example:** If you define `keybindings` in your Project config, it completely replaces User keybindings (not extends them).

### Removing/Unsetting Values

You cannot remove or unset a value from a lower layer — only override it. For boolean settings, you can set them to `false` to disable a feature enabled in a lower layer.

## Using the Settings UI

To configure Fresh through the Settings UI:

1. **Open Settings**: Use **Edit → Settings...** or Command Palette (`Ctrl+P`) → "Open Settings"
2. **Browse Categories**: Use arrow keys or click to navigate
3. **Change Values**: Toggle booleans, adjust numbers, select from dropdowns
4. **Choose Target Layer**: Click the layer button (e.g., `[ User ]`) to switch between User/Project/Session
5. **Save**: Press Enter on the Save button or use `Ctrl+S`

**Advanced: Edit Config File Directly**

For complex configurations (like LSP args or custom keybindings), click the `[ Edit ]` button in the Settings footer to open the raw JSON config file for the selected layer.

## Example Configurations

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

## Common Configuration Tasks

### Add a Custom Language

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

The `grammar` field accepts a short name like `"bash"` or `"rust"` as well as the full display name. To see every grammar available in your environment — including built-in grammars, user-installed grammars, language packs, bundles, and plugin-registered grammars — run:

```
fresh --cmd grammar list
```

### Customize Auto-Indentation

Fresh auto-indents new lines when you press Enter. Most languages work out of
the box, but you can tune the rules — or add them for a language Fresh doesn't
recognize — with an `indent` block on a language entry. No tree-sitter grammar
is required.

```json
{
  "languages": {
    "kotlin": {
      "extensions": ["kt", "kts"],
      "indent": {
        "increase_indent_pattern": "[\\{\\[\\(]\\s*$",
        "decrease_indent_pattern": "^\\s*[\\}\\]\\)]"
      }
    }
  }
}
```

#### How it works

When you press Enter, Fresh looks at the line you're splitting (the **reference
line**) and the text that moves down to the new line, then applies your
patterns to choose the new line's indent.

Each pattern is a regular expression
([regex crate syntax](https://docs.rs/regex/latest/regex/#syntax) — linear, with
no look-around or back-references). Before matching, Fresh blanks out comment
and string spans on the line (replacing them with spaces), so a bracket or
keyword **inside a string or comment never triggers indentation**.

Every pattern is optional. Any pattern you omit keeps the language's built-in
behavior, so you can override just one thing.

| Field | When it matches | Effect |
|-------|-----------------|--------|
| `increase_indent_pattern` | the reference line | the new line is **one level deeper** |
| `decrease_indent_pattern` | the new line's leading text | that line is **one level shallower** |
| `indent_next_line_pattern` | the reference line | the **next line only** is one level deeper (one-shot; doesn't persist) |
| `dedent_next_line_pattern` | the reference line | the **following line** is one level shallower (one-shot) |
| `self_close_pattern` | the reference line | cancels `increase_indent_pattern` for that line (stops one-liners like `def f; end` from over-indenting) |

The indent step is one unit of the language's `tab_size` (tabs or spaces per
your `use_tabs` setting).

#### Examples

Brace-delimited language (indent after a line ending in an open bracket; outdent
a line that starts with a close bracket):

```json
"indent": {
  "increase_indent_pattern": "[\\{\\[\\(]\\s*$",
  "decrease_indent_pattern": "^\\s*[\\}\\]\\)]"
}
```

Python-like, layout-defined language (indent after a `:`; dedent the line after
a flow-exit statement):

```json
"indent": {
  "increase_indent_pattern": ":\\s*$",
  "dedent_next_line_pattern": "^\\s*(return|pass|raise|break|continue)\\b"
}
```

`begin`/`end`-style language, using `self_close_pattern` so `begin … end` on one
line doesn't indent the next line:

```json
"indent": {
  "increase_indent_pattern": "^\\s*begin\\b",
  "decrease_indent_pattern": "^\\s*end\\b",
  "self_close_pattern": "\\bend\\b"
}
```

> Note: patterns are written in a JSON string, so backslashes must be escaped
> (`\\s`, `\\{`).

These patterns are the same mechanism Fresh's built-in languages use, grouped
into **families** (curly-brace, Python, Ruby-like, …). To add a language to
Fresh's source tree — or to understand which family yours resembles — see
[Adding a Built-in Language](/development/adding-languages).

### Set a Default Language for Unrecognized Files

When Fresh opens a file whose type it cannot detect (no matching extension, filename, or glob pattern), it shows it as "Plain Text" with no syntax highlighting. Set `default_language` to the name of any entry in the `languages` map and unrecognized files will use that language's full configuration — useful for `.conf`, `.rc`, `.rules`, and other config files that Fresh doesn't recognize.

```json
{ "default_language": "bash" }
```

This tells Fresh: "When you don't know what language a file is, treat it as bash." The file picks up bash syntax highlighting, `#` comments, indent rules, and anything else defined for bash in `languages`.

Any language name works — try `yaml`, `json`, `toml`, or a custom entry of your own. To disable (the default), leave `default_language` unset.

### Customize LSP Settings

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

### Project-Specific Tab Size

Create `.fresh/config.json` in your project:
```json
{
  "version": 1,
  "editor": {
    "tab_size": 2
  }
}
```

## Layer Source Indicators

In the Settings UI, each setting shows where its current value comes from:
- **(user)** - Set in your User config
- **(project)** - Set in the Project config
- **(session)** - Temporary session override
- *(no indicator)* - Using system default

## Status Bar

The left and right sides of the status bar are configurable through the Settings UI. Each side uses a **DualList** picker: items live in an **Available** column or an **Included** column, and you move them back and forth to show or hide them. Use the arrow buttons next to the Included list to reorder. Elements include the filename, cursor position, encoding, LSP indicator, git branch, warning counts, palette hint, a `{clock}` element that shows `HH:MM` with a blinking colon, and a `{remote}` indicator that lights up when you're attached to an SSH remote or a devcontainer.

The `{remote}` indicator is clickable — activate it to open a context-aware menu for the current authority (detach, show container logs, retry attach, etc.). It also reflects connection state: `Connecting`, `Connected`, or `FailedAttach`.

## Save Behavior

If the target directory doesn't exist when you save a file, Fresh prompts to create it for you instead of failing. This applies to both brand-new files and to saving an existing buffer under a new path.

## Editor Settings Reference

All settings can be changed via the Settings UI (run **Open Settings** from the palette).

### Display

| Setting | Description | Default |
|---------|-------------|---------|
| Line numbers | Show line numbers in gutter | on |
| Line wrap | Soft-wrap long lines | off |
| Rulers | Column positions for vertical ruler lines | none |
| Vertical scrollbar | Show vertical scrollbar | on |
| Horizontal scrollbar | Show horizontal scrollbar | off |
| Terminal background | Let terminal background show through | off |
| Bracket matching | Highlight matching bracket pairs | on |
| Status bar | Show/hide the status bar | on |
| Whitespace indicators | Show space/tab characters (leading, inner, trailing) | off |
| Diagnostics inline text | Show diagnostics at end of line | off |
| Show tilde | Show `~` markers after end of file | on |
| Menu bar mnemonics | Enable Alt+key shortcuts for menu bar | on |

### Editing

| Setting | Description | Default |
|---------|-------------|---------|
| Auto-close | Auto-close brackets and quotes | on |
| Auto-surround | Wrap selection when typing a delimiter | on |
| Trim trailing whitespace on save | Remove trailing whitespace when saving | off |
| Ensure final newline on save | Add trailing newline when saving | off |

### Auto-Save

| Setting | Description | Default |
|---------|-------------|---------|
| Auto-save | Save modified buffers to disk automatically | off |
| Auto-save interval | Seconds between auto-saves (when enabled) | 30 |
| Recovery save interval | Seconds between crash-recovery saves | 2 |
| Hot exit | Persist all buffers (including scratch) across sessions | on |

### Indentation

| Setting | Description | Default |
|---------|-------------|---------|
| Tab size | Spaces per indent level | 4 |
| Use tabs | Indent with tabs instead of spaces | off |

### UI

| Setting | Description | Default |
|---------|-------------|---------|
| Show prompt line | Show the prompt line at the bottom | on |

### Clipboard

| Setting | Description | Default |
|---------|-------------|---------|
| OSC 52 | Use OSC 52 escape sequence for clipboard | on |
| System clipboard | Use system clipboard | on |

If copy/paste hangs (common with PuTTY), try disabling one or both of these.

## Process Resource Limits

To prevent LSP servers from consuming too many resources, Fresh can limit their memory and CPU usage.

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

The `max_memory_mb` limit is enforced via platform-specific mechanisms. `max_cpu_percent` is relative to one core (e.g. 200 = two full cores).
