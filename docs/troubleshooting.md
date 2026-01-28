# Troubleshooting

## Terminal Color Support

Fresh automatically detects your terminal's color capability and converts theme colors accordingly. Most modern terminals support 24-bit "truecolor", but some terminals and multiplexers have limited support.

### Color Modes

- **Truecolor (24-bit)**: Full RGB color support (16 million colors). Used by modern terminals like Kitty, Alacritty, iTerm2, and most others with `COLORTERM=truecolor`.
- **256 colors**: Extended palette. Used by xterm-256color and similar terminals.
- **16 colors**: Basic ANSI colors. Used by the Linux console and very old terminals.

### Terminal Multiplexers

GNU Screen and tmux add a layer between your terminal and Fresh, which can affect color rendering:

- **GNU Screen**: Does not support truecolor. Fresh automatically uses 256 colors when `TERM` starts with `screen`.
- **tmux**: Supports 256 colors by default. Some configurations support truecolor with `TERM=tmux-direct`.

### Manual Override

If colors look wrong, you can force a specific color mode with the `FRESH_COLOR_MODE` environment variable:

```bash
# Force 256-color mode (recommended for GNU Screen)
FRESH_COLOR_MODE=256 fresh

# Force 16-color mode
FRESH_COLOR_MODE=16 fresh

# Force truecolor (if auto-detection is wrong)
FRESH_COLOR_MODE=truecolor fresh
```

### Common Issues

| Symptom | Likely Cause | Solution |
| :--- | :--- | :--- |
| Colors look completely wrong | Truecolor detected but not supported | Use `FRESH_COLOR_MODE=256` |
| Weird artifacts/rendering issues | Terminal multiplexer interference | Try `FRESH_COLOR_MODE=256` or check TERM |
| Very limited/ugly colors | 16-color mode detected | Check your terminal supports 256 colors |

### Checking Your Terminal

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

### Debug Keyboard Events

If a keybinding isn't working as expected, use **Help → Debug Keyboard Events** to see exactly what key codes your terminal sends to Fresh. This shows raw terminal events before any translation, helping diagnose issues like:

- Missing modifier keys (e.g., Ctrl+Shift+Home arriving as just Ctrl+Home)
- Terminal or OS intercepting keys before they reach Fresh
- Incorrect escape sequences from your terminal

Press any key to see its code, modifiers, and event type. Press `c` to clear history, `q` or `Esc` to close.
