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
| `fresh --cmd session open-file <name> <files> [--wait]` | Open files in a running session |
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

### Opening Files in a Running Session

Open files in an existing session without attaching to it:

```bash
# Open file in current directory session (use "." for session name)
fresh --cmd session open-file . src/main.rs

# Open file at specific line and column
fresh --cmd session open-file myproject src/lib.rs:42:10

# Open multiple files
fresh --cmd session open-file . file1.rs file2.rs
```

This is useful for integrating Fresh with file managers or other tools—files open in the existing editor without starting a new terminal session.

### Blocking Until Done (`--wait`)

The `--wait` flag keeps the CLI process alive until the user is done with the file. The process exits when:

- The **popup is dismissed** (press Escape) — if the file was opened with an `@"message"`
- The **buffer is closed** — if no message was given

```bash
# Open a file and block until the user closes the buffer
fresh --cmd session open-file . src/main.rs --wait

# Open at a line with a popup message — blocks until popup is dismissed
fresh --cmd session open-file . 'src/main.rs:42@"Review this function"' --wait
```

If no session is running, one is started automatically.

#### Use as Git's Editor

Set Fresh as git's editor so `git commit`, `git rebase -i`, etc. open in your running session and block until you close the buffer:

```bash
git config --global core.editor 'fresh --cmd session open-file . --wait'
```

Git appends the filename, so the final command becomes e.g. `fresh --cmd session open-file . --wait .git/COMMIT_EDITMSG`. The `--wait` flag can appear anywhere after the session name — files after it are collected normally.

#### Annotated Code Walkthroughs

Combine `--wait` with [range selection and popup messages](../../#file-location-syntax) to walk a user through code one location at a time. Each command blocks until the user presses Escape, then the next location opens:

```bash
fresh --cmd session open-file . 'src/parse.rs:10-25@"Step 1: The parser entry point"' --wait
fresh --cmd session open-file . 'src/eval.rs:80-95@"Step 2: Expression evaluation"' --wait
fresh --cmd session open-file . 'src/emit.rs:5@"Step 3: Code generation starts here"' --wait
```

Popup messages support markdown. Use `$'...'` quoting for multi-line messages:

```bash
fresh --cmd session open-file . \
  $'src/main.rs:1-15@"**Overview**\n\nThis is the entry point.\nNote the error handling on line 12."' --wait
```

#### Programmatic Integration

The `--wait` blocking behavior makes `session open-file` composable with any tool that needs to present files to a user and wait for acknowledgement:

```bash
# Code review script
for file in $(git diff --name-only HEAD~1); do
  fresh --cmd session open-file . "$file@\"Review this file\"" --wait
done

# Step through grep matches
grep -rn "TODO" src/ | while IFS=: read -r file line _; do
  fresh --cmd session open-file . "$file:$line@\"TODO found here\"" --wait
done
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
2. **Signal handling**: Some signals don't propagate to server terminals.

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
