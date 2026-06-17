# Daemon Mode

> **Formerly "Session Mode" / "Session Persistence".** Daemon mode was
> previously called *session mode*, and this page was titled *Session
> Persistence*. The terminology changed to avoid the overloaded word
> "session"; the old CLI form `fresh --cmd session …` still works as an
> alias, and this page keeps its `/features/session-persistence` URL so
> existing links and bookmarks continue to work.

> **Palette:** `Detach`. **CLI:** `fresh -a`, `fresh --cmd daemon list|new|kill`, `fresh --restore`, `fresh --no-restore`. **Config:** `hot_exit`, `editor.restore_previous_session`.

Detach from Fresh and reattach later, similar to tmux. The Fresh daemon keeps running in the background, so your editor state survives even after you close the terminal.

See also: [Remote Editing (SSH)](./ssh.md) for pairing daemon mode with remote backends, and [Devcontainers](./devcontainer.md) for routing through a container.

## Hot Exit

All buffers — including unnamed scratch buffers — persist automatically. When you quit Fresh, unsaved changes are preserved and restored on next startup. Configurable via the `hot_exit` setting (default: on).

## Workspace Storage

Each workspace's state (open files, split layout, plugin state) is restored on startup by default. Control this with:

- **`editor.restore_previous_session`** (config, default `true`) — when set to `false`, Fresh skips restoring tabs and splits but still brings back unsaved "hot-exit" content (dirty files and unnamed buffers).
- **`--no-restore`** (CLI) — one-shot skip equivalent to the config flag being off.
- **`--restore`** (CLI) — force a full workspace restore even when the config flag is off. Mutually exclusive with `--no-restore`.

## Quick Start

```bash
# Start or attach to a daemon for the current directory
fresh -a

# Detach: press Ctrl+Shift+D (or use Command Palette > "Detach")
# Terminal closes, but the Fresh daemon keeps running in the background

# Reattach later from the same directory
fresh -a

# List all running daemons
fresh --cmd daemon list
```

The `daemon` subcommand is an alias for the older `session` subcommand; both forms work (e.g. `fresh --cmd session list` is equivalent to `fresh --cmd daemon list`).

## Direct vs Daemon Mode

| Command | Mode | Description |
|---------|------|-------------|
| `fresh myfile.txt` | Direct | No background process. Closing quits everything. |
| `fresh -a` | Daemon | Background daemon. Supports detach/reattach. |

Use daemon mode for long-running tasks or remote work where the connection may drop.

## How It Works

With `-a`, Fresh starts a background daemon. The terminal is a lightweight client relaying input/output.

```
Terminal (Client)  ←→  Unix Socket  ←→  Fresh Daemon (Background)
     ↓                                        ↓
  Your keyboard                         Editor state
  Your screen                           Open files
                                        Running terminals
```

Detaching exits only the client; the daemon keeps running.

## Commands

| Command | Description |
|---------|-------------|
| `fresh -a` | Attach to the daemon for the current directory (starts one if needed) |
| `fresh -a <name>` | Attach to a named daemon |
| `fresh --cmd daemon list` | List running daemons |
| `fresh --cmd daemon new <name>` | Start a new named daemon |
| `fresh --cmd daemon open-file <name> <files> [--wait]` | Open files in a daemon (starts and attaches if needed) |
| `fresh --cmd daemon kill` | Kill the daemon for the current directory |
| `fresh --cmd daemon kill <name>` | Kill a named daemon |
| `fresh --cmd daemon kill --all` | Kill all daemons |

### Named Daemons

For multiple daemons in the same directory:

```bash
fresh --cmd daemon new feature-work
fresh --cmd daemon list
fresh -a feature-work
```

### Opening Files in a Daemon

Open files in a running daemon without attaching to it. If no daemon is running, one is started and the client attaches interactively:

```bash
# Open file in the current directory's daemon (use "." for the name)
fresh --cmd daemon open-file . src/main.rs

# Open file at specific line and column
fresh --cmd daemon open-file myproject src/lib.rs:42:10

# Open multiple files
fresh --cmd daemon open-file . file1.rs file2.rs
```

This is useful for integrating Fresh with file managers or other tools—files open in the existing editor without starting a new terminal.

### Blocking Until Done (`--wait`)

The `--wait` flag keeps the CLI process alive until the user is done with the file. The process exits when:

- The **popup is dismissed** (press Escape) — if the file was opened with an `@"message"`
- The **buffer is closed** — if no message was given

```bash
# Open a file and block until the user closes the buffer
fresh --cmd daemon open-file . src/main.rs --wait

# Open at a line with a popup message — blocks until popup is dismissed
fresh --cmd daemon open-file . 'src/main.rs:42@"Review this function"' --wait
```

If no daemon is running, one is started automatically and the client attaches interactively (`--wait` is ignored in this case — quit or detach normally).

#### Use as Git's Editor

Set Fresh as git's editor so `git commit`, `git rebase -i`, etc. open in your running daemon and block until you close the buffer:

```bash
git config --global core.editor 'fresh --cmd daemon open-file . --wait'
```

Git appends the filename, so the final command becomes e.g. `fresh --cmd daemon open-file . --wait .git/COMMIT_EDITMSG`. The `--wait` flag can appear anywhere after the daemon name — files after it are collected normally.

#### Annotated Code Walkthroughs

Combine `--wait` with [range selection and popup messages](../getting-started/#running-fresh) to walk a user through code one location at a time. Each command blocks until the user presses Escape, then the next location opens:

```bash
fresh --cmd daemon open-file . 'src/parse.rs:10-25@"Step 1: The parser entry point"' --wait
fresh --cmd daemon open-file . 'src/eval.rs:80-95@"Step 2: Expression evaluation"' --wait
fresh --cmd daemon open-file . 'src/emit.rs:5@"Step 3: Code generation starts here"' --wait
```

Popup messages support markdown. Use `$'...'` quoting for multi-line messages:

```bash
fresh --cmd daemon open-file . \
  $'src/main.rs:1-15@"**Overview**\n\nThis is the entry point.\nNote the error handling on line 12."' --wait
```

#### Programmatic Integration

The `--wait` blocking behavior makes `daemon open-file` composable with any tool that needs to present files to a user and wait for acknowledgement:

```bash
# Code review script
for file in $(git diff --name-only HEAD~1); do
  fresh --cmd daemon open-file . "$file@\"Review this file\"" --wait
done

# Step through grep matches
grep -rn "TODO" src/ | while IFS=: read -r file line _; do
  fresh --cmd daemon open-file . "$file:$line@\"TODO found here\"" --wait
done
```

### Detaching

- `Ctrl+Shift+D` or Command Palette → "Detach" or File → Detach
- **Detach**: Client exits, daemon keeps running
- **Quit** (`Ctrl+Q`): Both client and daemon exit

## Limitations and Pitfalls

### Resource Usage

Each daemon consumes memory for open files, terminal scrollback, and LSP servers. Use `fresh --cmd daemon list` periodically to check for forgotten daemons.

### Terminal State

When reattaching, terminal size may differ and some applications may not render correctly after resize. Scrollback is preserved but limited by buffer size.

### Platform Differences

| Platform | IPC Mechanism |
|----------|---------------|
| Linux/macOS | Unix domain sockets |
| Windows | Named pipes |

### Known Issues

1. **Stale sockets**: If Fresh crashes, socket files may remain. See [Socket Locations](#socket-locations) for cleanup.
2. **Signal handling**: Some signals don't propagate to daemon terminals.

## Troubleshooting

### "Connection refused"

The daemon may have crashed. Run `fresh --cmd daemon kill` to clean up, then `fresh -a` again.

### Daemon not in list

Daemons are keyed by working directory. `~/project` and `/home/user/project` create different daemons—use consistent paths.

### High memory usage

Check for forgotten daemons with `fresh --cmd daemon list`.

## Socket Locations

| Platform | Location |
|----------|----------|
| Linux | `$XDG_RUNTIME_DIR/fresh/` or `/tmp/fresh-$UID/` |
| macOS | `/tmp/fresh-$UID/` |
| Windows | `%LOCALAPPDATA%\fresh\sockets\` |
