
# Keyboard Config

Many OSes, window managers and terminal applications capture keys and filter them out so that applications like Fresh, running in the terminal, don't actually have a chance to handle those keys.

## Linux: XFCE window manager Ctrl + Alt + Up/Down keys - Disabling Workspace Switching Shortcuts

Follow these steps to clear the **Ctrl + Alt + Up** and **Ctrl + Alt + Down** shortcuts so they can be used in other applications (like `fresh`).

---

### Step-by-Step Instructions

1.  **Open Settings**: Open the XFCE Application Menu and go to **Settings** > **Window Manager**.
2.  **Navigate to Keyboard**: Click on the **Keyboard** tab.
3.  **Find Workspace Shortcuts**: Scroll through the list of actions to find:
    * `Upper workspace`
    * `Bottom workspace`
4.  **Clear First Shortcut (Up)**:
    * Select the row for **Upper workspace** (usually mapped to `Ctrl+Alt+Up`).
    * Click the **Clear** button (or double-click the row and press **Backspace**).
5.  **Clear Second Shortcut (Down)**:
    * Select the row for **Bottom workspace** (usually mapped to `Ctrl+Alt+Down`).
    * Click the **Clear** button.
6.  **Close**: Click **Close** to save the changes.

---

### Configuration Summary

| Action | Default Shortcut | New Setting |
| :--- | :--- | :--- |
| **Upper workspace** | `Ctrl + Alt + Up` | *Cleared / None* |
| **Bottom workspace** | `Ctrl + Alt + Down` | *Cleared / None* |

*Note: If you still experience issues, check **Settings** > **Keyboard** > **Application Shortcuts** to ensure no custom commands are overriding these keys.*

## macOS Terminal Tips

**TL;DR: Recommended Terminals**
- **Kitty**: Best experience out of the box. Add `macos_option_as_alt left` to config.
- **Ghostty**: Best experience out of the box. Add `macos-option-as-alt = left` to config.
- **Terminal.app**: [Import Fresh.terminal profile](https://github.com/sinelaw/fresh/blob/master/crates/fresh-editor/scripts/macOS/Fresh.terminal) to fix keybindings.
- **iTerm2**: Follow the [configuration instructions](#iterm2-setup) below.

Fresh works best on macOS when you understand the interaction between the operating system, your terminal emulator, and the editor. This section covers common issues and recommended configurations.

### Using the macOS Keymap

Fresh includes a dedicated macOS keymap that addresses terminal-specific challenges. To use it, add to your `~/.config/fresh/config.json`:

```json
{
  "keymap": "macos"
}
```

The macOS keymap is designed around these constraints:

**Ctrl+Shift combinations don't work.** Some macOS terminals cannot reliably send Ctrl+Shift sequences. For example, Ctrl+Shift+Z produces a caron character (ˇ) instead of being recognized as a key chord. The macOS keymap uses Ctrl+Alt as an alternative modifier.

**Some Ctrl keys are ASCII control characters.** In terminal protocols, Ctrl+J is Line Feed (newline), Ctrl+M is Carriage Return (Enter), and Ctrl+I is Tab. Binding actions to these keys causes erratic behavior. The macOS keymap avoids these collisions.

**International keyboards use Alt for essential characters.** On German, French, and other ISO layouts, Alt (Option) combined with letters produces characters like @, [, ], {, and }. The macOS keymap avoids Alt+letter combinations that would block character input.

**Unix readline conventions are preserved.** Terminal users expect Ctrl+Y to "yank" (paste from the kill ring), Ctrl+K to kill to end of line, and Ctrl+U to kill to start of line. The macOS keymap respects these conventions rather than overriding them with GUI editor shortcuts.

Use the **Command Palette** (Ctrl+P) or **Show Keybindings** (Ctrl+H) to discover the actual key bindings, or view the keymap file directly at `keymaps/macos.json`.

#### Recommended Terminal Emulators

For the best experience with Fresh on macOS, use a terminal that supports the **Kitty Keyboard Protocol (KKP)** or **CSI u** for unambiguous key reporting:

| Terminal | KKP Support | Notes |
| :--- | :--- | :--- |
| **Kitty** | Full | Set `macos_option_as_alt left` in config |
| **Ghostty** | Full | Set `macos-option-as-alt = left` in config |
| **iTerm2** | CSI u | Requires configuration (see below) |
| **Terminal.app** | None | Requires manual key mappings (see below) |

### iTerm2 Setup

To get the best experience with iTerm2, you need to enable CSI u support and configure the Option key.

1.  Go to **Settings** > **Profiles** > **Keys**.
2.  **General** tab:
    *   Check **Report keys using CSI u**. This allows Fresh to distinguish between combinations like `Ctrl+I` and `Tab`.
    *   Set **Left Option key** to **Esc+**. This treats the Option key as Meta/Alt.
    *   Set **Right Option key** to **Normal** if you use it for special characters (or Esc+ if you want it as Alt too).

![iTerm2 Profile Keys](/images/macos-iterm-profile-keys.png)

### Apple Terminal.app Setup

Apple's built-in Terminal requires manual configuration to work well with modern terminal editors.

**Option as Meta:**
1.  Go to **Settings** > **Profiles** > **Keyboard**.
2.  Check **Use Option as Meta key**.

![Terminal Option as Meta](/images/mac-terminal-option-as-meta.png)

**Key Mappings:**
Fresh relies on Shift+Arrow keys for selection, but Terminal.app often doesn't send these by default.

**Easier Method: Import Profile**
We provide a pre-configured profile that sets up colors and key mappings for you.
1.  Locate `scripts/macOS/Fresh.terminal` in the repository.
2.  In Terminal.app, go to **Settings** > **Profiles**.
3.  Click the gear icon at the bottom of the sidebar and select **Import...**.
4.  Select the `Fresh.terminal` file.

**Manual Configuration:**
If you prefer to configure it manually:
1.  In **Settings** > **Profiles** > **Keyboard**, click the `+` button.
2.  Map **Shift + Cursor Up** to send text `\033[1;2A` (press Esc then type `[1;2A`).
3.  Map **Shift + Cursor Down** to send text `\033[1;2B`.

The full list of keys:

- Control + Option

    Up: \033[1;7A

    Down: \033[1;7B

    Right: \033[1;7C

    Left: \033[1;7D

- Control + Shift

    Up: \033[1;6A

    Down: \033[1;6B

    Right: \033[1;6C

    Left: \033[1;6D

- Shift

    Up: \033[1;2A

    Down: \033[1;2B

    Right: \033[1;2C

    Left: \033[1;2D


![Terminal Keymaps](/images/mac-terminal-keymaps.png)

### Keyboard Enhancement Flags

Fresh can use the Kitty Keyboard Protocol to get more accurate key reporting from supported terminals. You can configure which features to enable in your config file:

```json
{
  "editor": {
    "keyboard_disambiguate_escape_codes": true,
    "keyboard_report_event_types": false,
    "keyboard_report_alternate_keys": true,
    "keyboard_report_all_keys_as_escape_codes": false
  }
}
```

| Option | Default | Description |
| :--- | :--- | :--- |
| `keyboard_disambiguate_escape_codes` | `true` | Use CSI-u sequences for unambiguous escape/modifier key reading |
| `keyboard_report_event_types` | `false` | Report key repeat and release events (not just press) |
| `keyboard_report_alternate_keys` | `true` | Send alternate keycodes in addition to base keycodes |
| `keyboard_report_all_keys_as_escape_codes` | `false` | Report all keys (including plain text) as escape sequences |

These flags only take effect if your terminal supports the Kitty Keyboard Protocol. Fresh automatically detects support and falls back gracefully if the protocol is unavailable. If you experience keyboard issues, try disabling all flags by setting them to `false`.

### Home and End Keys

On macOS, the Home and End keys scroll the terminal buffer by default instead of moving the cursor. Fresh's macOS keymap works around this by binding:

- **Ctrl+A** → Move to line start
- **Ctrl+E** → Move to line end
- **Ctrl+Shift+A** → Select to line start
- **Ctrl+Shift+E** → Select to line end

If you prefer using the actual Home/End keys, configure your terminal to send the proper escape sequences:

**iTerm2:**
1. Preferences → Profiles → Keys → Key Mappings
2. Add: Home → Send Escape Sequence → `[H`
3. Add: End → Send Escape Sequence → `[F`

### Mission Control Conflicts

macOS uses **Ctrl+Arrow** keys for Mission Control desktop switching by default, which prevents these shortcuts from reaching terminal applications.

To use Ctrl+Arrow in Fresh for word movement or multi-cursor:

1. Open **System Settings** → **Keyboard** → **Keyboard Shortcuts** → **Mission Control**
2. Disable or rebind:
   - "Move left a space" (Ctrl+Left)
   - "Move right a space" (Ctrl+Right)
   - "Mission Control" (Ctrl+Up)
   - "Application windows" (Ctrl+Down)

Alternatively, Fresh's macOS keymap provides **Alt+Arrow** as the primary word movement binding, which doesn't conflict with Mission Control.

### Option Key on International Keyboards

If you use Option to type special characters (like @ on German layouts), you should configure your terminal to treat only the **Left Option** as Meta/Alt, and keep the **Right Option** for character input. iTerm2 supports this configuration (see above).

### International Keyboard Layouts

The macOS keymap disables Alt+0-9 bindings because these key combinations are used to type essential characters on many international keyboard layouts:

- **German**: Alt+L = @, Alt+5 = [, Alt+6 = ]
- **French**: Alt+( = {, Alt+) = }
- **Spanish**: Alt+2 = @, Alt+3 = #

If you find that certain Alt combinations insert characters instead of triggering editor commands, ensure your terminal's Option key is configured as Meta (see above).

## Debugging Keyboard Issues

If keybindings aren't working as expected, use **Help → Debug Keyboard Events** to see exactly what your terminal sends. See [Troubleshooting: Debug Keyboard Events](/troubleshooting#debug-keyboard-events) for details.