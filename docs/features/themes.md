# Themes


Fresh supports customizable color themes.

## Selecting a Theme

Use the command palette (`Ctrl+P`) and search for "Select Theme" to choose from available themes. Built-in themes and user themes are both shown.

## Setting a Theme in `config.json`

The `theme` field in `config.json` accepts several forms, so you can point at a built-in, a local file, or a theme hosted somewhere else (Fresh's config parser accepts JSONC, so `//` comments are fine):

```jsonc
{ "theme": "dark" }                              // built-in, by name
{ "theme": "builtin://dark" }                    // same thing, explicit

{ "theme": "my-theme.json" }                     // relative to ~/.config/fresh/themes
{ "theme": "subdir/dark.json" }                  // nested is fine

{ "theme": "file://${HOME}/themes/x.json" }      // absolute path; ${HOME} and
                                                  // ${XDG_CONFIG_HOME} are expanded

{ "theme": "https://github.com/foo/themes#dark" } // URL-packaged theme; fragment
                                                   // picks one theme from the repo
```

The relative form is convenient for sharing a Fresh `config.json` in a dotfiles repo alongside the theme files themselves — the path resolves the same way on every machine.

## Creating and Editing Themes

Fresh includes a visual Theme Editor for creating and customizing themes:

1. **Open the Theme Editor**: Press `Ctrl+P` and search for "Edit Theme"

2. **The Theme Editor Interface**:
   - Color fields show a preview swatch next to each value
   - Sections can be collapsed/expanded with `Enter`
   - Navigate with `Up/Down` arrows, `Tab/Shift+Tab`, or mouse scroll
   - Click color swatches to edit them directly

3. **Editing Colors**:
   - Press `Enter` on any color field to edit it
   - Enter a hex color (`#RRGGBB`) or named color (e.g., `red`, `blue`)
   - Colors are applied immediately as you edit

4. **Theme Editor Shortcuts**:
   | Action | Key |
   | ------ | --- |
   | Open theme | `Ctrl+O` |
   | Save | `Ctrl+S` |
   | Save As | `Ctrl+Shift+S` |
   | Delete theme | `Ctrl+D` |
   | Close | `Ctrl+Q` or `Escape` |
   | Help | `F1` |

5. **Working with Built-in Themes**:
   - Built-in themes cannot be modified directly
   - Use "Save As" (`Ctrl+Shift+S`) to create a copy that you can customize
   - Your custom themes are saved to `~/.config/fresh/themes/`

6. **Theme Structure**:
   - **Editor**: Main editor colors (background, foreground, cursor, selection)
   - **UI Elements**: Interface colors (tabs, menus, status bar)
   - **Search**: Search result highlighting
   - **Diagnostics**: LSP diagnostic colors (errors, warnings)
   - **Syntax Highlighting**: Code colors (keywords, strings, comments)

## Theme File Format

Themes are stored as JSON files. You can also edit them directly at `~/.config/fresh/themes/`. Example:

```json
{
  "name": "my-theme",
  "editor": {
    "bg": [30, 30, 30],
    "fg": [212, 212, 212],
    "cursor": [82, 139, 255],
    "selection_bg": [38, 79, 120]
  },
  "syntax": {
    "keyword": [86, 156, 214],
    "string": [206, 145, 120],
    "comment": [106, 153, 85]
  }
}
```

Colors are specified as `[R, G, B]` arrays with values from 0-255.

Only `name` is required. Any section or field you omit is filled in from a
**base theme** (see [Inheritance](#inheritance) below), so a partial theme
only needs to spell out the colors that differ from its base.

## Inheritance

A theme can build on top of another theme instead of restating every color.
Any field you don't set is taken from the base; the fields you do set
override the base.

```jsonc
{
  "name": "my-light-tweak",
  "extends": "builtin://light",        // pick the base
  "editor": { "cursor": [255, 105, 180] }  // change one thing
}
```

`extends` accepts a built-in name in either form: `"builtin://light"` or
just `"light"`. The available built-ins are `dark`, `light`, and
`high-contrast` (plus any others shipped in your install — see the
**Select Theme** menu for the full list). Inheriting from another *user*
theme is not supported in this version.

If `extends` is omitted, Fresh tries to pick a sensible base for you:

- If your theme sets `editor.bg`, Fresh looks at the relative luminance of
  that color and picks `builtin://light` for bright backgrounds and
  `builtin://dark` for dim ones. So a custom theme that only sets a cream
  background gets light-flavored UI chrome automatically.
- If your theme doesn't set `editor.bg` either, every unset field falls
  back to a per-field hardcoded default.

This means the partial example at the top of this section works without
needing to spell out every UI/diagnostic color — Fresh fills the rest in
from the matching built-in.

## Inspecting Theme Colors

Use "Inspect Theme at Cursor" from the command palette to see which theme colors apply at the cursor position. You can also `Ctrl+Right-Click` on any text to see theme info in a popup.

