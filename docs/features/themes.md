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
   - A color's text attributes (`bold`, `italic`, …) are on its picker
     panel: `Tab` to the panel, `↑` to **Attributes**, `Enter`. The tree shows
     them after the color (`#6E7080 +italic`)
   - The editor opens below your code and the whole editor (menu, explorer,
     tabs, gutter, status bar) shows each edit immediately, before you save.
     Closing without saving puts your theme back
   - **Chrome** holds the on/off options (toggle with `Enter`) and the glyphs,
     with their **nerd_font** set beside them; a glyph prompt offers a list of
     separators and caps to pick from
   - **File Icons** lists the theme's icon overrides. `Enter` on **Add…**
     adds one (the built-in names and extensions are suggested); an entry's
     `glyph` and `color` show the built-in value until you set them
   - An unset chrome or icon value shows what it draws — the option's
     default, or the built-in icon — and edits start from it
   - `Delete` resets a chrome or file-icon value to Fresh's default, or
     removes a whole icon entry
   - Attributes are comma-separated: `bold`, `italic`, `underlined`, `dim`,
     `reversed`

4. **Theme Editor Shortcuts**:
   | Action | Key |
   | ------ | --- |
   | Open theme | `Ctrl+O` |
   | Save | `Ctrl+S` |
   | Save As | `Ctrl+Shift+S` |
   | Delete theme | `Ctrl+D` |
   | Reset value / remove icon entry | `Delete` |
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

| `tab_separator` | `" "` | Text between two tabs, in `ui.tab_separator_fg` on `ui.tab_separator_bg`. Any glyph: `"│"`, `"▏"`, `"·"`; `""` puts tabs side by side. |
| `status_bar_separator` | `""` | Text between status bar items, in `ui.status_separator_fg`/`_bg`. Items already have a one-space margin, so `"·"` shows as `LF · UTF-8`. A `status_bar.separator` in your config wins over the theme's. |

```json
"chrome": { "tab_separator": "▏", "status_bar_separator": "·" }
```

#### Glyphs

Every glyph the chrome draws can be changed. Defaults are what Fresh has
always drawn; an empty string draws nothing and takes no space.

| Glyph | Default | What it is |
| ----- | ------- | ---------- |
| `tab_cap_left`, `tab_cap_right` | none | Each tab's ends, in the tab's own background on the strip's: rounded or slanted tabs |
| `tab_close` / `tab_modified` | `×` / `*` | A tab's close button / unsaved marker |
| `status_bar_section_left`, `status_bar_section_right` | none | Where two status items' backgrounds meet: drawn in one's color over the other's |
| `status_bar_cap_left`, `status_bar_cap_right` | none | The bar's outer ends, when the outermost item has its own background |
| `fold_collapsed` / `fold_expanded` | `▸` / `▾` | The gutter's fold markers |
| `diagnostic_marker` | `●` | The gutter's mark on a line with a diagnostic |
| `folder_collapsed` / `folder_expanded` | `>` / `▼` | The explorer's folder arrows (your `file_explorer.tree_indicator_*` setting wins once you change it) |
| `section_collapsed` / `section_expanded` | `▶` / `▼` | A sidebar section's header marker (with more than one section) |
| `panel_close` / `panel_refresh` | `×` / `⟳` | The sidebar header's close and refresh buttons |

#### Nerd Font glyphs and status bar sections

Status lines in Neovim (lualine) and tab lines (bufferline) get their look from
Powerline glyphs drawn *between colors*: a section separator is painted in one
segment's background over the next one's, so segments read as shapes. Fresh's
status bar does the same:

- `ui.status_outer_fg` / `_bg` color the outermost item on each side
  (lualine's sections *a* and *z*), `ui.status_inner_fg` / `_bg` the next one
  in (*b* and *y*). Both default to the bar's own colors.
- `status_bar_section_left` / `_right` draw where two items' backgrounds
  differ, `status_bar_cap_left` / `_right` round off the ends.

Glyphs that need a [Nerd Font](https://www.nerdfonts.com) go under
`nerd_font`. They are drawn only while `editor.nerd_font_icons` is on, so the
same theme still looks right in a terminal without the font:

```json
"chrome": {
  "status_bar_separator": "·",
  "folder_collapsed": "›", "folder_expanded": "⌄",
  "nerd_font": {
    "status_bar_cap_left": "\ue0b6", "status_bar_section_left": "\ue0b4",
    "status_bar_section_right": "\ue0b6", "status_bar_cap_right": "\ue0b4",
    "tab_cap_left": "\ue0b6", "tab_cap_right": "\ue0b4",
    "tab_close": "\udb80\udd56",
    "folder_collapsed": "\uf460 \ue5ff", "folder_expanded": "\uf47c \ue5fe"
  }
}
```

The built-in `quiet` theme is a complete example.

### File-type icons

With `editor.nerd_font_icons` on, the file explorer and the tabs show a
file-type icon before each name — the Rust crab for `.rs`, the npm logo for
`package.json`, open and closed folders. Fresh has built-in icons in each
type's familiar color; a theme's `file_icons` section replaces or recolors
any of them and adds its own:

```json
{
  "name": "my-icons",
  "extends": "builtin://quiet",
  "file_icons": {
    "by_extension": {
      "md":  { "color": [255, 120, 160] },
      "gif": { "glyph": "\udb83\udd78", "color": [80, 220, 120] }
    },
    "by_name": { "index.md": { "glyph": "\uf015" } },
    "default": { "color": [110, 115, 130] },
    "folder": { "color": [230, 190, 90] },
    "folder_open": { "color": [230, 190, 90] }
  }
}
```

- An entry sets a `glyph`, a `color`, or both; what it leaves out comes from
  the built-in icon it replaces.
- The most specific match wins: a whole file name (case-insensitive), then
  the extension (without the dot), then `default`. At each level the theme's
  entry wins over the built-in one.
- A theme that `extends` another inherits the entries it does not name.
- `ui.file_icon_fg` draws every icon in one color; folders otherwise use
  `ui.file_explorer_directory_fg`.
- A plugin that sets an explorer row's leading slot (`setFileExplorerSlots`)
  owns that row's slot.

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
