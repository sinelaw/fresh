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
   - The editor opens below your code and the whole editor (menu, explorer,
     tabs, gutter, status bar) shows each edit immediately, before you save.
     Closing without saving puts your theme back
   - `chrome` options are on/off toggles: press `Enter` to flip one
   - Each color has an `Attributes` row accepting comma-separated text
     attributes: `bold`, `italic`, `underlined`, `dim`, and `reversed`

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
    "keyword": { "color": [86, 156, 214], "modifier": ["bold"] },
    "string": [206, 145, 120],
    "comment": { "color": [106, 153, 85], "modifier": ["italic"] }
  }
}
```

Colors are specified as `[R, G, B]` arrays with values from 0-255, or by
name (`"Red"`, `"LightBlue"`, …; `"Default"` is the terminal's own color).

### File explorer colors

Ordinary folder names in the file explorer use `ui.file_explorer_directory_fg`.
A theme that leaves it out keeps the old look, with folders in the
`syntax.keyword` color. Set it to color folders without changing how
keywords look in code:

```json
{
  "name": "neutral-folders",
  "extends": "builtin://dark",
  "syntax": { "keyword": [237, 65, 65] },
  "ui": { "file_explorer_directory_fg": [200, 209, 217] }
}
```

Git status colors (`ui.file_status_*_fg`) and the selected row are still drawn
on top of it.

The explorer panel has its own colors too: `ui.file_explorer_bg` and
`ui.file_explorer_fg` (the panel's ground and file names, default
`editor.bg` / `editor.fg`), `ui.file_explorer_border_fg` and
`ui.file_explorer_border_focused_fg` (the box, unfocused and focused), and
`ui.file_explorer_indicator_fg` (the `>` / `▼` folder arrows).

### Gutter colors

- `editor.line_number_bg` is the gutter's background. When it differs from
  `editor.bg` the gutter is painted in it, which marks where the text begins.
- `editor.current_line_number_fg` styles the cursor's line number. Bundle
  attributes to make it stand out:
  `{"color": [255, 204, 102], "modifier": ["bold"]}`. Leave it out to keep
  the default look.
- `editor.gutter_separator_fg` colors the `│` between the numbers and the text.

### Removing borders and separators (`chrome`)

Colors cannot give a cell back: a border drawn in its background's color still
takes its column. The `chrome` section says which pieces exist at all; a piece
that is off takes no cells and the content moves into the space.

```json
{
  "name": "borderless",
  "extends": "builtin://dark",
  "editor": { "line_number_bg": [22, 22, 22] },
  "ui": { "file_explorer_bg": [36, 36, 40] },
  "chrome": { "gutter_separator": false, "file_explorer_border": false }
}
```

| Option | Default | Off |
| ------ | ------- | --- |
| `gutter_separator` | `true` | The gutter ends in a one-cell pad instead of ` │ `. |
| `file_explorer_border` | `true` | The explorer keeps its title row; its walls and bottom border go and its rows take the full width. The edge facing the editor stays as one blank column you can drag to resize. |

When a border goes, mark the edge with a background instead:
`editor.line_number_bg` for the gutter, `ui.file_explorer_bg` for the
explorer. The built-in `flat` and `quiet` themes do both.

### More syntax categories

`syntax.number` (numbers and booleans), `syntax.property` (fields, object keys,
JSON keys, HTML tags) and `syntax.attribute` (attributes, decorators,
annotations) can be colored separately. A theme that leaves them out keeps the
old look: `number` and `attribute` follow `syntax.constant`, `property`
follows `syntax.variable`.

### Text attributes

Every color in a theme — in `editor`, `ui`, `search`, `diagnostic` and
`syntax` alike — is either a bare color or an object bundling that color with
a `modifier` list of `bold`, `italic`, `underlined`, `dim`, or `reversed`. Use
a bare color (or omit `modifier`) for no attributes. A cell gets the
attributes of every color it is drawn with, foreground and background, so an
attribute on a `*_bg` color applies to the text drawn over it.

For example, to show LSP errors and warnings as underlines rather than as a
background wash (`"Default"` leaves the background as it is):

```json
{
  "name": "underlined-diagnostics",
  "extends": "builtin://dark",
  "diagnostic": {
    "error_bg": { "color": "Default", "modifier": ["underlined"] },
    "warning_bg": { "color": "Default", "modifier": ["underlined"] }
  }
}
```

Attributes mean nothing on colors that are not drawn behind or as text, such
as the scrollbar colors; those colors use only the color.

The older attribute-only keys `editor.selection_modifier` and
`ui.semantic_highlight_modifier` still work, and win over attributes bundled
with `editor.selection_bg` / `ui.semantic_highlight_bg` when a theme gives
both.

A key Fresh does not know (a typo, or a key from another editor's theme
format) is ignored, with a warning naming it. A theme with a value Fresh
cannot read is not loaded, with a warning naming the file and the key.

Only `name` is required. A color you omit comes from another color, never
from a value built into Fresh: from a **base theme** (see
[Inheritance](#inheritance) below), or, in a complete theme, from the color
it falls back to (see [Fallbacks](#fallbacks)). So a partial theme only needs
to spell out the colors that differ from its base.

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

If `extends` is omitted and the theme leaves out any of the
[required colors](#fallbacks), Fresh picks a base for you:

- If your theme sets `editor.bg`, Fresh looks at the relative luminance of
  that color and picks `builtin://light` for bright backgrounds and
  `builtin://dark` for dim ones. So a custom theme that only sets a cream
  background gets light-flavored UI chrome automatically.
- If your theme doesn't set `editor.bg` either, it extends `builtin://dark`.

This means the partial example at the top of this section works without
needing to spell out every UI/diagnostic color — Fresh fills the rest in
from the matching built-in.

## Fallbacks

A theme without `extends` that sets every **required** color stands on its
own, with no base theme. The required colors are the 49 that theme files have
had since the first theme format: the `editor` background, text, cursor,
selection, current line and line numbers; the tab, status bar, prompt, popup,
suggestion, help and split-separator colors in `ui`; `search.match_bg` and
`match_fg`; the eight `diagnostic` colors; and eight `syntax` categories.

Every other color names a fallback color, and a standalone theme that leaves
it out takes that color's value (and text attributes), following the chain
to the first color the theme sets. For example `ui.menu_hover_bg` falls back
to `ui.menu_highlight_bg`, which falls back to the required
`ui.popup_selection_bg`. Every chain ends at a required color.

`extends` takes precedence: in a theme with a base, a color you leave out is
the base's, even if you changed the color it would fall back to.

## Inspecting Theme Colors

Use "Inspect Theme at Cursor" from the command palette to see which theme colors apply at the cursor position. You can also `Ctrl+Right-Click` on any text to see theme info in a popup.
