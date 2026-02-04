# Integrated Terminal

Fresh includes a built-in terminal emulator that lets you run shell commands without leaving the editor.

## Opening a Terminal

*   **Command Palette:** Press `Ctrl+P` and search for "Open Terminal"
*   **Multiple Terminals:** You can open multiple terminal tabs and switch between them like regular file buffers

## Terminal Modes

The terminal has two modes, indicated in the status bar:

1.  **Terminal Mode** (status bar shows "Terminal"): Your keyboard input goes directly to the shell. Use this for typing commands and interacting with programs.

2.  **Scrollback Mode** (status bar shows "Terminal (read only)"): The terminal output becomes a read-only buffer that you can scroll through, search, and copy text from.

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

## Tips and Quirks

*   **Session Persistence:** Terminal scrollback is preserved when you close and reopen Fresh. Your scrollback history is maintained, but all running processes are terminated and lost.
*   **Automatic Scroll:** When new output arrives while you're in scrollback mode, the terminal automatically returns to terminal mode to show the latest output. Disable this with the `terminal.jump_to_end_on_output` config option.
*   **Resizing:** The terminal automatically resizes when you resize the editor or split panes.

## Windows Support

Fresh supports full terminal emulation on Windows 10 (1809+) via ConPTY. PowerShell is preferred over cmd.exe. Stdin piping works with `type file | fresh`.
