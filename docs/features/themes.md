# Themes


Fresh supports customizable color themes.

## Selecting a Theme

Use the command palette (`Ctrl+P`) and search for "Select Theme" to choose from available themes. Built-in themes and user themes are both shown.

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

## Inspecting Theme Colors

Use "Inspect Theme at Cursor" from the command palette to see which theme colors apply at the cursor position. You can also `Ctrl+Right-Click` on any text to see theme info in a popup.

