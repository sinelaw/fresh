# Editing

::: tip Platform Note
Some keybindings may not work or may differ on your system due to differences in keyboard layouts, terminal emulators, and operating systems. Terminals capture and report key events differently, and some key combinations may be intercepted by your OS or terminal before reaching Fresh. If a keybinding doesn't work, check the command palette (`Ctrl+P`) for alternative bindings, use the [keyboard configuration](../configuration/keyboard.md) to customize bindings, or browse all available shortcuts in the [Keybinding Editor](./keybinding-editor.md).
:::

## Smart Editing

- **Smart Home** — Home toggles between first non-whitespace character and column 0.
- **Smart Backspace** — Backspace in leading whitespace removes one indent level instead of a single character.
- **Auto-indent** — Enter preserves the current indentation level. After `{`, `(`, or `:`, an extra indent level is added.
- **Auto-close** — Typing an opening bracket or quote inserts the closing pair. Controlled by `auto_close` (default: on), independent of `auto_indent`. Per-language overrides via `languages.<lang>.auto_close`.
- **Surround selection** — With text selected, typing an opening delimiter wraps the selection (e.g. select `hello`, type `(` → `(hello)`). Controlled by `auto_surround` (default: on) with per-language overrides.
- **Smart quote suppression** — Quotes typed inside an existing string don't auto-close.
- **Bracket matching** — Matching brackets are highlighted. Use "Go to Matching Bracket" from the command palette to jump. Enabled by default; toggle via `highlight_matching_brackets` in settings.

## Vertical Rulers

Add column rulers at any position via "Add Ruler" from the command palette. Useful for enforcing line length limits. Remove with "Remove Ruler". Rulers are per-buffer. The `rulers` config setting can also set default rulers (e.g. `[80, 120]`).

## Auto-Save

Enable `auto_save_enabled` in settings to automatically save modified buffers to disk at a configurable interval (default 30 seconds). This is separate from the crash-recovery auto-save, which runs independently every 2 seconds to a recovery directory.

## Code Folding

Fold and unfold code blocks via gutter indicators or "Toggle Fold" from the command palette. Up/Down navigation skips folded regions. Each split view maintains its own fold state. Folding works in two modes:

- **LSP folding** — uses `foldingRange` from the language server when available.
- **Indent-based folding** — fallback for files without LSP support and large file mode. Fold from any line within an indented block.

## Read-Only Mode

Files without write permission and known library paths (rustup toolchains, `/usr/include`, `/nix/store`, Homebrew Cellar, `.nuget`, Xcode SDKs) open as read-only automatically. The status bar shows `[RO]`. Use "Toggle Read Only" from the command palette to override.

## Whitespace Indicators

Control visibility of space (`·`) and tab (`→`) characters. Configure independently for leading, inner, and trailing positions via the Settings UI or `whitespace_indicators` in config. A master toggle and per-language overrides are supported. Theme color: `whitespace_indicator_fg`.

## Inline Diagnostics

Diagnostic messages can be displayed at the end of each line, right-aligned, with version-aware staleness dimming. Disabled by default — enable "diagnostics inline text" in the Settings UI or set `diagnostics_inline_text` in config.

## Line Wrap

When line wrap is enabled (`line_wrap` in settings), wrapped continuation lines preserve the indentation of their parent line (hanging indent).

## Multiple Cursors

Edit multiple locations simultaneously:

| Shortcut | Action |
|----------|--------|
| `Ctrl+D` | Add cursor at next occurrence of selection |
| `Ctrl+Alt+↑` | Add cursor above |
| `Ctrl+Alt+↓` | Add cursor below |
| `Esc` | Remove secondary cursors |

## Selection

| Shortcut | Action |
|----------|--------|
| `Ctrl+W` | Select word under cursor |
| `Ctrl+L` | Select current line |
| `Ctrl+A` | Select all |
| `Shift+Arrow` | Extend selection in direction |
| `Ctrl+Shift+←/→` | Select word left/right |
| `Shift+Home/End` | Select to line start/end |
| `Ctrl+Shift+Home/End` | Select to document start/end |
| `Shift+PgUp/PgDn` | Select page up/down |

### Block Selection

| Shortcut | Action |
|----------|--------|
| `Alt+Shift+↑/↓` | Block select up/down |
| `Alt+Shift+←/→` | Block select left/right |

## Basic Editing

| Shortcut | Action |
|----------|--------|
| `Ctrl+C` | Copy |
| `Ctrl+X` | Cut |
| `Ctrl+V` | Paste |
| `Ctrl+Z` | Undo |
| `Ctrl+Y` | Redo |
| `Tab` | Indent |
| `Shift+Tab` | Dedent |
| `Ctrl+/` | Toggle comment |
| `Ctrl+T` | Transpose characters |

### Deletion

| Shortcut | Action |
|----------|--------|
| `Backspace` | Delete backward |
| `Del` | Delete forward |
| `Ctrl+Backspace` | Delete word backward |
| `Ctrl+Del` | Delete word forward |
| `Ctrl+K` | Delete to end of line |

### Sort and Transform

Available from the command palette:

- **Sort Lines** — sort selected lines alphabetically
- **Trim Trailing Whitespace** — remove trailing whitespace from all lines

Configure `trim_trailing_whitespace_on_save` and `ensure_final_newline_on_save` in settings to run these automatically on save.

### Case Conversion

| Shortcut | Action |
|----------|--------|
| `Alt+U` | Convert to uppercase |
| `Alt+L` | Convert to lowercase |

## Search and Replace

| Shortcut | Action |
|----------|--------|
| `Ctrl+F` | Search in buffer |
| `Ctrl+R` | Replace in buffer |
| `Ctrl+Alt+R` | Interactive replace (y/n/!/q for each match) |
| `F3` | Find next match |
| `Shift+F3` | Find previous match **TODO - use command palette**|
| `Alt+N` / `Ctrl+F3` | Find next occurrence of selection |
| `Alt+P` / `Ctrl+Shift+F3` | Find previous occurrence of selection **TODO - use command palette**|

See [Search and Replace](./search-replace.md) for more details.

## Macros

Record and replay sequences of keystrokes:

| Shortcut | Action |
|----------|--------|
| `F5` | Stop macro recording |
| `F4` | Play last recorded macro |

Use the command palette (`Ctrl+P`) to access **Record Macro**, **Play Macro**, **Play Last Macro**, and **List Macros** commands.

To bind a custom key to play a macro, follow the example below to add a `keybindings` section to your local `config.json`. 

This example binds `alt+shift+!` to play macro 1 and `alt+shift+@` to play macro 2.

```json
{
  "theme": "dracula",
  "keybindings": [
    {
      "key": "!",
      "modifiers": ["alt"],
      "action": "play_macro",
      "args": {"char": "1"},
      "when": "normal"
    },
    {
      "key": "@",
      "modifiers": ["alt"],
      "action": "play_macro",
      "args": {"char": "2"},
      "when": "normal"
    }
  ]
}
```

## Bookmarks

Jump quickly between locations in your code:

| Shortcut | Action |
|----------|--------|
| `Ctrl+Shift+0-9` | Set bookmark 0-9 |
| `Alt+0-9` | Jump to bookmark 0-9 |

## Markdown Editing

Smart editing for Markdown files (provided by the built-in `markdown_source` plugin, enabled by default):

- Enter continues list items (bullets, ordered lists, checkboxes)
- Enter on an empty list marker removes it
- Tab indents list items and cycles the bullet style
- Single-quote auto-close is disabled so apostrophes don't interfere

### Compose Mode (experimental)

"Markdown: Toggle Compose" from the command palette enables a distraction-free mode that conceals markup (`**`, `*`, `[]()`), applies soft line breaks at a configurable width, and renders tables. Use "Markdown: Set Compose Width" to adjust the width. Open the same file in a vertical split to see source and composed views side by side.

## Shell Integration

Run shell commands on your buffer or selection:

| Shortcut | Action |
|----------|--------|
| `Alt+\|` | Run shell command on buffer/selection (output shown) |
| `Alt+Shift+\|` | Run shell command and replace selection with output |

## Navigation

| Shortcut | Action |
|----------|--------|
| `Ctrl+Home` | Move to document start |
| `Ctrl+End` | Move to document end |
| `Ctrl+G` | Go to line number |
| `F8` | Jump to next error/diagnostic |
| `Shift+F8` | Jump to previous error/diagnostic |
| `Alt+←` | Navigate back in history |
| `Alt+→` | Navigate forward in history |

See [Navigation](./navigation.md) for more details.
