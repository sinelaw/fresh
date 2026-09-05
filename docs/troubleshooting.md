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

### 256-Color Contrast

When running in a 256-color terminal, Fresh automatically adjusts foreground colors to maintain readable contrast against their background.

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

## Corrupted Display

If something outside Fresh scribbles over the TUI — a stray shell message, an external program's output, a paste with unbalanced escape sequences, or a terminal that got wedged during a resize — the screen can end up with ghost text or misaligned cells. Run **Redraw Screen** from the command palette (`Ctrl+P`) to clear the terminal and repaint the UI from scratch.

## Advanced Topics

### Visual Regression Testing

Fresh uses visual regression testing to catch unintentional UI changes. See `docs/VISUAL_REGRESSION_TESTING.md`.

## Keybindings

Open command palette (Ctrl+P or ^P) and type the name of the command you want to run - if any keybinding is assigned, it will also be shown.

Alternatively, use Help -> Keyboard Shortcuts to view the full list.

### Debug Keyboard Events

If a keybinding isn't working as expected, use **Help → Debug Keyboard Events** to see exactly what key codes your terminal sends to Fresh. This shows raw terminal events before any translation, helping diagnose issues like:

- Missing modifier keys (e.g., Ctrl+Shift+Home arriving as just Ctrl+Home)
- Terminal or OS intercepting keys before they reach Fresh
- Incorrect escape sequences from your terminal

Press any key to see its code, modifiers, and event type. Press `c` to clear history, `q` or `Esc` to close.

## Updating

### `fresh --cmd update` fails with a 403 from GitHub

`api.github.com` allows an unauthenticated caller 60 requests an hour **per IP
address** — shared with everyone else behind the same router, VPN or corporate
NAT — so a 403 from that host usually means somebody else spent the budget.

Fresh stays off that API almost entirely: the version comes from GitHub's
release redirect, and the archive, the package and their checksums come from
`github.com`, exactly as `install.sh` fetches them. Checking for an update
therefore costs nothing at all.

Installing spends one API request: the release attestation, which is verified
against a second origin on purpose and so cannot be served from the same host
as the download. If that one is refused:

- **Wait** for the window named in the message (at most an hour).
- **Use a token** — a GitHub personal access token needs no scopes at all for a
  public repository and raises the limit to 5000/hour:

  ```bash
  export FRESH_GITHUB_TOKEN=ghp_...   # or GITHUB_TOKEN / GH_TOKEN
  fresh --cmd update
  ```

  It is sent to `api.github.com` and nowhere else.
- **Re-run the installer**, which fetches the release directly from
  `github.com` and verifies its published checksum.
- **Skip the attestation** for this one run — `fresh --cmd update --yes
  --skip-attestation`. The download is still checked against its published
  SHA-256, but that checksum shares an origin with the artifact, so it catches
  a corrupted download and not a tampered release. Fresh says so whenever the
  flag is used.

`fresh --cmd update --pre` also needs the API, because pre-releases are only
listed there.
