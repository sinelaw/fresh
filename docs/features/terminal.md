# Integrated Terminal

Fresh includes a built-in terminal emulator.

## Opening a Terminal

All from the command palette (`Ctrl+P`):

*   **Open Terminal** — a terminal in the current split.
*   **Open Terminal to the Right** — a terminal in a new vertical split beside the active pane.
*   **Open Terminal Below** — a terminal in a new horizontal split below the active pane.
*   **Open Terminal in Utility Dock** (`Alt`+backtick) — a terminal in the shared bottom dock, creating the dock if needed.

You can open multiple terminal tabs and switch between them like regular file buffers. The tab bar's **+** button also offers New Terminal / New File.

## Send to Terminal

**Send Selection to Terminal** runs the selected text — or the current line if there's no selection — in the most recently used terminal, and switches focus to it.

## Clickable Paths

`Ctrl+Click` (or `Ctrl`-hover, which underlines the target) opens file paths from terminal output, including scrollback. Paths resolve as absolute (with `~` expansion), then relative to the terminal's working directory, then relative to Fresh's working directory. The shell's working directory is tracked via OSC 7, so relative paths resolve correctly after `cd` — and resolution works over SSH.

## Terminal Modes

The terminal has two modes, indicated in the status bar:

1.  **Terminal Mode** (status bar shows "Terminal"): Your keyboard input goes directly to the shell. The scrollbar is hidden and the grid uses the full split width.

2.  **Scrollback Mode** (status bar shows "Terminal (read only)"): The terminal output becomes a read-only buffer that you can scroll through, search, and copy text from.

**Per-split:** The mode is a property of each split, not the terminal as a whole. If you show the same terminal in two splits, one can sit in scrollback (frozen at some point in history) while the other keeps streaming the live output — and each split keeps its own mode even when it isn't focused. Disable **Automatic Scroll** (below) to hold a split in scrollback while output keeps arriving.

## Switching Between Modes

*   **`Ctrl+Space`**: Toggle between terminal mode and scrollback mode
*   **`Ctrl+]`**: Exit terminal mode (same as `Ctrl+Space`)

## Keyboard Capture

By default, most editor keybindings (like `Ctrl+P` for command palette) still work in terminal mode. If you need to send these keys to the terminal instead:

*   **`F9`**: Toggle keyboard capture mode
*   When keyboard capture is enabled (status bar shows "Terminal [capture]"), all keys except `F9` are sent to the terminal
*   **Visual indicator:** The UI dims (menu bar, status bar, other splits) to clearly show focus is exclusively on the terminal

## Scrollback Navigation

In scrollback mode, you can use standard editor navigation:

*   **Arrow keys / Page Up / Page Down**: Scroll through output
*   **`Ctrl+Home`**: Jump to the beginning of scrollback history
*   **`Ctrl+End`**: Jump to the end
*   **`Ctrl+F`**: Search through terminal output

## Shell Override

By default the integrated terminal runs `$SHELL` (or the platform default if `$SHELL` is empty). Override this with `terminal.shell` in config without changing `$SHELL` for the whole process — handy when you want a wrapper that forces an interactive shell, or a different shell inside a container:

```json
{
  "terminal": {
    "shell": { "command": "/usr/bin/bash", "args": ["--login", "-i"] }
  }
}
```

The override applies to host-shell terminals; wrappers that re-parent the shell keep their own spawn contract and ignore `terminal.shell`. This includes `docker exec` when attached to a devcontainer and the `ssh -t` wrapper used when [editing a remote over SSH](./ssh.md) — in those cases the terminal opens the shell *inside the container / on the remote host*, rooted at the workspace.

## Tips and Quirks

*   **Workspace Persistence:** Terminal scrollback is preserved across editor restarts, but running processes are terminated.
*   **Daemon Mode:** Use `fresh -a` to start in daemon mode, then detach with `Ctrl+Shift+D` to keep terminal processes running in the background. Reattach with `fresh -a`. See [Daemon Mode](./session-persistence.md) for details.
*   **Automatic Scroll:** When new output arrives while the focused split is in scrollback mode, it automatically returns to terminal mode to show the latest output. Disable this with the `terminal.jump_to_end_on_output` config option — handy when you want to read scrollback in one split while another split (or the same one) keeps streaming. The jump always yields while a text selection is active, so new output can't destroy a selection you're about to copy.
*   **Mouse Selection:** Dragging on a live terminal selects text — the split drops into read-only scrollback (`Ctrl+Space` resumes) with a real selection that `Ctrl+C` copies; a bare click still just focuses the terminal, and double-click selects words in scrollback. Set `terminal.mouse_drag_selects = false` to make drags on the live grid inert again.
*   **Mouse Forwarding:** Mouse events reach the program inside the terminal only when it enabled mouse reporting (DECSET 1000/1002/1003), and `Shift`+drag bypasses it so you can always select. Set `terminal.mouse_forwarding = "alt_screen"` to restore the legacy rule (forward everything to any alternate-screen program, no `Shift` bypass).
*   **Resizing:** The terminal automatically resizes when you resize the editor or split panes.
*   **Suspend (Unix):** Run **Suspend Process** from the palette to send the foreground Fresh process to the background (like Ctrl+Z in a shell). In daemon mode the suspend is routed through the client so the daemon stays up.

## Windows Support

Fresh supports full terminal emulation on Windows 10 (1809+) via ConPTY. PowerShell is preferred over cmd.exe. Stdin piping works with `type file | fresh`.
