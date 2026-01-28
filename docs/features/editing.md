# Editing

Fresh provides a powerful set of editing features with intuitive keybindings.

::: tip Platform Note
Some keybindings may not work or may differ on your system due to differences in keyboard layouts, terminal emulators, and operating systems. Terminals capture and report key events differently, and some key combinations may be intercepted by your OS or terminal before reaching Fresh. If a keybinding doesn't work, check the command palette (`Ctrl+P`) for alternative bindings or use the [keyboard configuration](../configuration/keyboard.md) to customize bindings.
:::

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
| `Shift+F3` | Find previous match |
| `Alt+N` / `Ctrl+F3` | Find next occurrence of selection |
| `Alt+P` / `Ctrl+Shift+F3` | Find previous occurrence of selection |

See [Search and Replace](./search-replace.md) for more details.

## Macros

Record and replay sequences of keystrokes:

| Shortcut | Action |
|----------|--------|
| `Alt+Shift+0-9` | Toggle macro recording for slot 0-9 |
| `Ctrl+0-9` | Play macro from slot 0-9 |
| `F5` | Stop macro recording |

## Bookmarks

Jump quickly between locations in your code:

| Shortcut | Action |
|----------|--------|
| `Ctrl+Shift+0-9` | Set bookmark 0-9 |
| `Alt+0-9` | Jump to bookmark 0-9 |

## Shell Integration

Run shell commands on your buffer or selection:

| Shortcut | Action |
|----------|--------|
| `Alt+\|` | Run shell command on buffer/selection (output shown) |
| `Alt+Shift+\|` | Run shell command and replace selection with output |

## Navigation

| Shortcut | Action |
|----------|--------|
| `Ctrl+G` | Go to line number |
| `Ctrl+]` | Go to matching bracket |
| `F8` | Jump to next error/diagnostic |
| `Shift+F8` | Jump to previous error/diagnostic |
| `Alt+←` | Navigate back in history |
| `Alt+→` | Navigate forward in history |

See [Navigation](./navigation.md) for more details.
