# Getting Started

## Installation

The quickest way to install Fresh is the install script:

```bash
curl -fsSL https://raw.githubusercontent.com/sinelaw/fresh/refs/heads/master/scripts/install.sh | sh
```

On Linux this installs the **universal build**, the recommended way to run Fresh there: a single static binary that works on every distro and needs no root. On macOS the script uses Homebrew.

To upgrade the universal build, run `fresh --update` (it asks before installing), or click the update indicator that appears in the status bar when a new release is out. Re-running the install script works too. Other install methods upgrade through their own package manager.

See the [Installation section in the README](https://github.com/sinelaw/fresh#installation) for every other method, including Homebrew, winget, AUR, .deb/.rpm packages, npm, crates.io, Nix and building from source.

## Running Fresh

Run Fresh from the command line:

```bash
# Open this directory's workspace
fresh

# Open a file
fresh src/main.rs

# Open a file at a specific line
fresh src/main.rs:42
fresh +42 src/main.rs

# Open a file at a specific line and column
fresh src/main.rs:42:10

# Open multiple files (with optional line:col)
fresh Cargo.toml src/lib.rs:100:5

# Open a directory as a workspace, with the file explorer
fresh ~/projects/app

# Read piped input into a new buffer
cargo build 2>&1 | fresh
fresh - < notes.txt

# Just an editor in this terminal: no workspace, no background editor
fresh --simple

# Open a remote file or directory via SSH
fresh user@host:/path/to/file.txt
fresh user@host:~/projects
```

The `file:line:col` syntax is useful for jumping directly to compiler errors or search results.

A bare `fresh` opens the workspace for the directory you are in. With **Auto Attach** on (the default), it runs in a shared background editor that outlives the terminal: you can detach and come back, and a later `fresh` in another directory switches that same editor to its workspace. Turn Auto Attach off in the Settings UI to get a plain editor in the terminal instead. See [Daemon Mode](../features/session-persistence.md) for details. `fresh FILE` is unaffected: it just opens that file, here.

### Selecting a range and showing a message

A location can also be a range, which opens the file with those lines selected, and can carry a message that pops up next to it:

```bash
# Select lines 10 to 25
fresh 'src/main.rs:10-25'

# Select from line 10, column 5 to line 12, column 3
fresh 'src/main.rs:10:5-12:3'

# Open at a line with a popup message (Markdown is supported)
fresh 'src/main.rs:42@"Check this error path"'
```

Quote the argument so the shell keeps the `@"..."` part intact. Together with `--wait`, this lets scripts walk someone through code one location at a time; see [Annotated Code Walkthroughs](../features/session-persistence.md#annotated-code-walkthroughs).

## First Launch: the Welcome Screen

When you start Fresh with a bare `fresh`, a **Welcome** tab opens behind your other tabs (opening a file, or `fresh --simple`, skips it). It shows what's new in this release, the essential keys (command palette, open and new file, file explorer, settings, terminal, help, quit), a switch for Nerd Font icons, and the Auto Attach setting.

![The Welcome tab: what's new in this release, a Nerd Font switch, the essential keys and the startup options](/images/welcome-screen.png)

Close it with `Esc` or the tab's ×, and run **Welcome** from the command palette to bring it back. To stop it from opening at startup, uncheck **Show this screen on startup** at the bottom of the page, or turn it off in the Settings UI under **Plugins → welcome_screen**.

## Core Concepts

*   **The Command Palette:** The command palette provides quick access to commands and features. Press `Ctrl+P` to open it, and then start typing to search for commands.
*   **Buffers:** Each open file is represented as a buffer. You can have multiple buffers open at once and switch between them.
*   **Splits:** You can split your editor view horizontally or vertically to view multiple buffers at once.
*   **The Status Bar:** The status bar at the bottom of the screen displays information about the current buffer, including the file name, cursor position, and Git branch.
