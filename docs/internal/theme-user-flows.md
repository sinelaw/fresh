# Theme System User Flows

This document describes the user flows for adding and customizing themes in Fresh.

## Overview

Fresh has a robust theming system with 5 color categories:
- **Editor Colors** - background, cursor, selection, line highlighting
- **UI Colors** - tabs, menus, status bar, popups, scrollbars
- **Search Colors** - match highlighting
- **Diagnostic Colors** - errors, warnings, info, hints
- **Syntax Colors** - keywords, strings, comments, functions, etc.

## User Flow 1: Quick Theme Selection

**Goal:** Switch between available themes

1. Press `Ctrl+P` to open command palette
2. Type "select theme" and press Enter
3. Browse/filter themes (current theme marked with "(current)")
4. Press Enter to apply - takes effect immediately
5. Theme preference saved to `~/.config/fresh/config.json`

**Files involved:**
- `src/app/input.rs:1932-1975` - `start_select_theme_prompt()`
- `src/app/input.rs:1978-1995` - `apply_theme()`

## User Flow 2: Interactive Theme Editor

**Goal:** Create or modify themes visually

### Opening the Editor
1. Press `Ctrl+P` to open command palette
2. Type "edit theme" and press Enter
3. Theme Editor opens in current split

**Optional: Add a keyboard shortcut**

To open the theme editor with a single keystroke, add a custom keybinding in `~/.config/fresh/config.json`:

```json
{
  "keybindings": {
    "normal": [
      ["C-S-t", "open_theme_editor"]
    ]
  }
}
```

This binds `Ctrl+Shift+T` to open the theme editor. You can choose any key combination.

### Creating a New Theme
1. Press `o` to open a theme (built-in or user) as starting point
2. Navigate with arrow keys to color fields
3. Press `Enter` or `Space` to edit a color
4. Input color as: `#FF0000` (hex), `[255,0,0]` (RGB), or `Red` (named)
5. Press `s` to save (prompts for name if built-in) - theme is automatically applied

### Key Bindings

| Key | Action |
|-----|--------|
| `Up/Down/j/k` | Navigate between selectable fields and sections |
| `Tab` | Navigate to next field/section (with wrapping) |
| `Shift+Tab` | Navigate to previous field/section (with wrapping) |
| `Enter/Space` | Edit color field or toggle section expand/collapse |
| `o` | Open theme (built-in or user) |
| `s` | Save theme (requires Save As for built-in, auto-applies) |
| `S` | Save as (new name, auto-applies) |
| `x` | Delete current user theme |
| `r` | Reload theme from file |
| `?` | Show help |
| `q/Esc` | Close editor |

All commands are also available via the command palette (`Ctrl+P`) when the theme editor is open.

### Color Input Formats
- **Hex:** `#FF8000` or `#ff8000`
- **RGB array:** `[255, 128, 0]` or `255,128,0`
- **Named colors:** `Red`, `Green`, `Blue`, `DarkGray`, `White`, etc.
- **Special:** `Default` or `Reset` (uses terminal's native color for transparency)

**Files involved:**
- `plugins/theme_editor.ts` - Full interactive editor implementation

## User Flow 3: Manual JSON File Creation

**Goal:** Create themes by writing JSON directly

### Steps
1. Create file at `~/.config/fresh/themes/my-theme.json`
2. Use the required JSON structure (see below)
3. Theme appears automatically in theme selector
4. Select via `Ctrl+P` → "select theme"

### Theme JSON Structure

```json
{
  "name": "my-theme",
  "editor": {
    "bg": [30, 30, 30],
    "fg": [212, 212, 212],
    "cursor": [255, 255, 255],
    "inactive_cursor": [100, 100, 100],
    "selection_bg": [38, 79, 120],
    "current_line_bg": [40, 40, 40],
    "line_number_fg": [100, 100, 100],
    "line_number_bg": [30, 30, 30],
    "diff_add_bg": [35, 60, 35],
    "diff_remove_bg": [70, 35, 35],
    "diff_modify_bg": [40, 38, 30]
  },
  "ui": {
    "tab_active_fg": [255, 255, 255],
    "tab_active_bg": [30, 30, 30],
    "tab_inactive_fg": [150, 150, 150],
    "tab_inactive_bg": [45, 45, 45],
    "tab_separator_bg": [30, 30, 30],
    "menu_bar_bg": [51, 51, 51],
    "menu_bar_fg": [255, 255, 255],
    "status_bar_bg": [0, 122, 204],
    "status_bar_fg": [255, 255, 255],
    "prompt_fg": [255, 255, 255],
    "prompt_bg": [60, 60, 60],
    "prompt_selection_fg": [255, 255, 255],
    "prompt_selection_bg": [0, 122, 204],
    "popup_border_fg": [100, 100, 100],
    "popup_bg": [45, 45, 45],
    "popup_selection_bg": [0, 122, 204],
    "popup_text_fg": [255, 255, 255],
    "suggestion_bg": [45, 45, 45],
    "suggestion_selected_bg": [0, 122, 204],
    "help_bg": [30, 30, 30],
    "help_fg": [255, 255, 255],
    "help_key_fg": [86, 156, 214],
    "help_separator_fg": [100, 100, 100],
    "help_indicator_fg": [255, 100, 100],
    "help_indicator_bg": [30, 30, 30],
    "split_separator_fg": [100, 100, 100]
  },
  "search": {
    "match_bg": [255, 215, 0],
    "match_fg": [0, 0, 0]
  },
  "diagnostic": {
    "error_fg": [255, 85, 85],
    "error_bg": [50, 30, 30],
    "warning_fg": [255, 200, 100],
    "warning_bg": [50, 45, 30],
    "info_fg": [100, 200, 255],
    "info_bg": [30, 40, 50],
    "hint_fg": [150, 150, 150],
    "hint_bg": [30, 30, 30]
  },
  "syntax": {
    "keyword": [197, 134, 192],
    "string": [206, 145, 120],
    "comment": [106, 153, 85],
    "function": [220, 220, 170],
    "type": [78, 201, 176],
    "variable": [156, 220, 254],
    "constant": [100, 150, 200],
    "operator": [212, 212, 212]
  }
}
```

## User Flow 4: Override Built-in Theme

**Goal:** Customize a built-in theme without renaming

1. Copy a built-in theme from `themes/` directory
2. Place it at `~/.config/fresh/themes/<same-name>.json`
3. User themes take precedence over built-ins with same name

**Example:**
```bash
cp themes/dark.json ~/.config/fresh/themes/dark.json
# Edit ~/.config/fresh/themes/dark.json
```

## User Flow 5: Set Default Theme via Config

**Goal:** Change default theme without UI

Edit `~/.config/fresh/config.json`:
```json
{
  "theme": "nord"
}
```

Default theme is `high-contrast` if not specified.

## Built-in Themes

| Theme | Description |
|-------|-------------|
| `dark` | VSCode Dark+ inspired |
| `light` | VSCode Light+ inspired |
| `high-contrast` | Accessibility-focused (default) |
| `nostalgia` | Turbo Pascal/DOS inspired |
| `nord` | Nord color scheme (JSON only) |
| `dracula` | Dracula color scheme (JSON only) |
| `solarized-dark` | Solarized dark scheme (JSON only) |

## Theme Storage Locations

| Location | Purpose |
|----------|---------|
| `themes/*.json` | Built-in themes (shipped with Fresh) |
| `~/.config/fresh/themes/` | User custom themes |
| `~/.config/fresh/config.json` | Stores current theme preference |

## Key Files Reference

| File | Purpose |
|------|---------|
| `src/view/theme.rs` | Core theme system, color definitions, loading |
| `src/config.rs` | Theme configuration (ThemeName type) |
| `src/app/input.rs` | Theme selection and application logic |
| `plugins/theme_editor.ts` | Interactive theme editor UI |
| `plugins/theme_editor.i18n.json` | Theme editor translations |
