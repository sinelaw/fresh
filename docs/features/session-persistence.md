# Session Persistence (Experimental)

> **Warning**: This feature is experimental. The API and behavior may change.

Detach from Fresh and reattach later, similar to tmux. Your editor state persists even after closing the terminal.

## Quick Start

```bash
# Start or attach to a session for the current directory
fresh -a

# Detach: press Ctrl+Shift+D (or use Command Palette > "Detach")
# Terminal closes, but Fresh keeps running in the background

# Reattach later from the same directory
fresh -a

# List all running sessions
fresh --cmd session list
```

## Direct vs Session Mode

| Command | Mode | Description |
|---------|------|-------------|
| `fresh myfile.txt` | Direct | No server. Closing quits everything. |
| `fresh -a` | Session | Background server. Supports detach/reattach. |

Use session mode for long-running tasks or SSH sessions where connection may drop.

## How It Works

With `-a`, Fresh starts a background server. The terminal is a lightweight client relaying input/output.

```
Terminal (Client)  ←→  Unix Socket  ←→  Fresh Server (Background)
     ↓                                        ↓
  Your keyboard                         Editor state
  Your screen                           Open files
                                        Running terminals
```

Detaching exits only the client; the server keeps running.

## Commands

| Command | Description |
|---------|-------------|
| `fresh -a` | Attach to session for current directory (starts server if needed) |
| `fresh -a <name>` | Attach to named session |
| `fresh --cmd session list` | List running sessions |
| `fresh --cmd session new <name>` | Start a new named session |
| `fresh --cmd session kill` | Kill session for current directory |
| `fresh --cmd session kill <name>` | Kill named session |
| `fresh --cmd session kill --all` | Kill all sessions |

### Named Sessions

For multiple sessions in the same directory:

```bash
fresh --cmd session new feature-work
fresh --cmd session list
fresh -a feature-work
```

### Detaching

- `Ctrl+Shift+D` or Command Palette → "Detach" or File → Detach Session
- **Detach**: Client exits, server keeps running
- **Quit** (`Ctrl+Q`): Both client and server exit

## Limitations and Pitfalls

### Resource Usage

Each session consumes memory for open files, terminal scrollback, and LSP servers. Use `fresh --cmd session list` periodically to check for forgotten sessions.

### Terminal State

When reattaching, terminal size may differ and some applications may not render correctly after resize. Scrollback is preserved but limited by buffer size.

### Platform Differences

| Platform | IPC Mechanism |
|----------|---------------|
| Linux/macOS | Unix domain sockets |
| Windows | Named pipes |

### Known Issues

1. **Stale sockets**: If Fresh crashes, socket files may remain. See [Socket Locations](#socket-locations) for cleanup.
2. **Single client**: Only one client can attach at a time.
3. **Signal handling**: Some signals don't propagate to server terminals.

## Troubleshooting

### "Connection refused"

Server may have crashed. Run `fresh --cmd session kill` to clean up, then `fresh -a` again.

### Session not in list

Sessions are keyed by working directory. `~/project` and `/home/user/project` create different sessions—use consistent paths.

### High memory usage

Check for forgotten sessions with `fresh --cmd session list`.

## Socket Locations

| Platform | Location |
|----------|----------|
| Linux | `$XDG_RUNTIME_DIR/fresh/` or `/tmp/fresh-$UID/` |
| macOS | `/tmp/fresh-$UID/` |
| Windows | `%LOCALAPPDATA%\fresh\sockets\` |
