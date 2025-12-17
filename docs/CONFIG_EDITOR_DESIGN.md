# Config Editor Design

> Built-in settings editor for Fresh, replacing the plugin-based config_editor.ts

## Research Summary

### Zed Editor Settings UI
- **Layout**: Separate window with categories on left, controls on right
- **Philosophy**: "Files as the organizing principle" - settings.json always available for power users
- **Controls**: Form-based structure with strongly-typed settings
- **Navigation**: Tab groups for keyboard navigability, focus management
- **State**: Direct UI tree state (similar to React hooks)
- **Source**: [How We Rebuilt Settings in Zed](https://zed.dev/blog/settings-ui)

### IntelliJ IDEA Settings
- **Layout**: Hierarchical tree navigation on left, settings panel on right
- **Search**: Prominent search field at top-left for filtering settings
- **Indicators**: Icon marks project-specific vs IDE-wide settings
- **Actions**: OK (apply + close), Apply (apply + stay open), Cancel
- **Theme**: New "Islands" theme (2025) with rounded corners, balanced spacing
- **Source**: [IntelliJ Settings Documentation](https://www.jetbrains.com/help/idea/settings-preferences-dialog.html)

### VS Code Settings
- **Indicators**: Colored bar on left shows modified settings (like editor gutter)
- **Search**: Powerful Bing-powered search with extension discovery
- **Controls**: Type-aware inputs (checkboxes for bools, dropdowns for enums)
- **Context Menu**: Gear icon for reset to default, copy setting ID
- **Validation**: Inline validation errors
- **Source**: [VS Code Settings Editor UI](https://dev.to/vscode/all-new-vscode-settings-editor-ui-----3j48)

---

## Design Goals

1. **Terminal-Native**: Designed for TUI, not a web GUI port
2. **Keyboard-First**: Full navigation without mouse
3. **Discoverable**: Users can browse and find settings without reading docs
4. **Efficient**: Common settings are quick to change
5. **Safe**: Changes can be previewed and reverted
6. **JSON Accessible**: Power users can still edit config.json directly

---

## UI Layout

The config editor opens as a full-screen modal (like the file picker), not a separate buffer.

### Main Screen Structure

```
┌─ Settings ─────────────────────────────────────────────────────────────────┐
│ [/] Search settings...                              [?] Help  [Esc] Close  │
├────────────────────────────────────────────────────────────────────────────┤
│ ▸ Editor                     │ ╔══════════════════════════════════════════╗│
│   Appearance                 │ ║ Line Numbers                      [✓]  ●║│
│   Behavior                   │ ║ Show line numbers in the gutter         ║│
│   Performance                │ ╠══════════════════════════════════════════╣│
│ ▸ File Explorer              │ ║ Relative Line Numbers             [ ]   ║│
│ ▸ Terminal                   │ ║ Show line numbers relative to cursor    ║│
│ ▸ LSP / Language Servers     │ ╠══════════════════════════════════════════╣│
│ ▸ Languages                  │ ║ Tab Size                         [4  ]  ║│
│ ▸ Keybindings                │ ║ Number of spaces per tab character      ║│
│ ▸ Theme                      │ ╠══════════════════════════════════════════╣│
│ ▸ Updates                    │ ║ Line Wrap                         [✓]   ║│
│                              │ ║ Wrap long lines to fit window width     ║│
│                              │ ╠══════════════════════════════════════════╣│
│                              │ ║ Scroll Offset                    [3  ]  ║│
│                              │ ║ Minimum lines above/below cursor        ║│
│                              │ ╚══════════════════════════════════════════╝│
├────────────────────────────────────────────────────────────────────────────┤
│ Tab:Edit  ↑↓:Navigate  Enter:Toggle/Expand  /:Search  Ctrl+S:Save  Esc:Exit│
└────────────────────────────────────────────────────────────────────────────┘
```

Key elements:
- **Left Panel**: Category tree (collapsible sections)
- **Right Panel**: Settings for selected category
- **Status Bar**: Keyboard shortcuts cheat sheet
- **Modified Indicator**: Dot (●) next to changed settings

---

## Category Organization

### Hierarchy

```
├── Editor
│   ├── Appearance
│   │   ├── Line Numbers
│   │   ├── Relative Line Numbers
│   │   └── Syntax Highlighting
│   ├── Behavior
│   │   ├── Tab Size
│   │   ├── Auto Indent
│   │   └── Line Wrap
│   ├── Mouse
│   │   ├── Mouse Hover Enabled
│   │   ├── Mouse Hover Delay
│   │   └── Double Click Time
│   └── Performance
│       ├── Highlight Timeout
│       ├── Large File Threshold
│       └── Highlight Context Bytes
│
├── File Explorer
│   ├── Show Hidden Files
│   ├── Show Gitignored Files
│   ├── Respect Gitignore
│   ├── Width
│   └── Custom Ignore Patterns
│
├── Terminal
│   └── Jump to End on Output
│
├── LSP / Language Servers
│   ├── [rust]
│   │   ├── Command
│   │   ├── Args
│   │   ├── Enabled
│   │   └── Auto Start
│   ├── [python]
│   │   └── ...
│   └── [Add Server...]
│
├── Languages
│   ├── [rust]
│   │   ├── Extensions
│   │   ├── Grammar
│   │   ├── Comment Prefix
│   │   ├── Highlighter
│   │   └── TextMate Grammar
│   └── [Add Language...]
│
├── Keybindings
│   ├── Active Map (dropdown: default/emacs/vscode)
│   ├── [View Current Bindings...]
│   └── [Edit keybindings JSON...]
│
├── Theme
│   └── Theme Name (dropdown with preview)
│
├── Recovery
│   ├── Recovery Enabled
│   └── Auto Save Interval
│
└── Updates
    └── Check for Updates
```

---

## Control Types

### Boolean Toggle
```
┌──────────────────────────────────────────────────────────────┐
│ Line Numbers                                          [✓]  ●│
│ Show line numbers in the gutter                              │
│                                                              │
│ Default: On                                                  │
└──────────────────────────────────────────────────────────────┘
```
- `[✓]` = enabled, `[ ]` = disabled
- Press `Space` or `Enter` to toggle
- `●` indicates modified from default

### Number Input
```
┌──────────────────────────────────────────────────────────────┐
│ Tab Size                                             [4   ] │
│ Number of spaces per tab character                           │
│                                                              │
│ Default: 4  |  Valid range: 1-16                             │
└──────────────────────────────────────────────────────────────┘
```
- Shows current value in editable field
- Press `Enter` to edit, type number, `Enter` to confirm
- Validation shown inline

### Dropdown / Enum
```
┌──────────────────────────────────────────────────────────────┐
│ Active Keybinding Map                         [▼ default  ] │
│ Choose your preferred keybinding style                       │
│ ┌─────────────────────────────────────┐                      │
│ │ ● default                           │                      │
│ │   emacs                             │                      │
│ │   vscode                            │                      │
│ │   custom-vim (user defined)         │                      │
│ └─────────────────────────────────────┘                      │
└──────────────────────────────────────────────────────────────┘
```
- Press `Enter` to open dropdown
- Arrow keys to navigate, `Enter` to select
- Shows current selection in button

### Theme Selector (with Preview)
```
┌──────────────────────────────────────────────────────────────┐
│ Theme                                    [▼ high-contrast  ] │
│ Color theme for the editor                                   │
│ ┌─────────────────────────────────────┐ ┌──────────────────┐ │
│ │ ● high-contrast                     │ │ ▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄ │ │
│ │   monokai                           │ │ █ fn main() {   █ │ │
│ │   solarized-dark                    │ │ █   println!(); █ │ │
│ │   solarized-light                   │ │ █ }             █ │ │
│ │   dracula                           │ │ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ │ │
│ └─────────────────────────────────────┘ └──────────────────┘ │
└──────────────────────────────────────────────────────────────┘
```
- Live preview of theme colors in mini code sample
- Preview updates as you navigate options

### String Input
```
┌──────────────────────────────────────────────────────────────┐
│ Custom Ignore Patterns                                       │
│ Patterns to ignore in file explorer (in addition to gitignore)
│ ┌──────────────────────────────────────────────────────────┐ │
│ │ *.log                                                    │ │
│ │ node_modules/                                            │ │
│ │ target/                                                  │ │
│ │ [+ Add pattern...]                                       │ │
│ └──────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
```
- List of strings with add/remove capability
- Press `Enter` on item to edit, `Delete` to remove

### LSP Server Configuration
```
┌──────────────────────────────────────────────────────────────┐
│ ▾ rust                                              [Default]│
├──────────────────────────────────────────────────────────────┤
│ Command        [rust-analyzer                              ] │
│ Args           [--log-file, /tmp/ra.log                    ] │
│ Enabled        [✓]                                           │
│ Auto Start     [ ]                                           │
│                                                              │
│ [Test Connection]  [Reset to Default]                        │
└──────────────────────────────────────────────────────────────┘
```
- Collapsible sections for each language
- "Default" badge when using built-in config
- Test button to verify LSP server is reachable

---

## Search Interface

When user presses `/` to search:

```
┌─ Settings ─────────────────────────────────────────────────────────────────┐
│ [/] line numb█                                      [?] Help  [Esc] Close  │
├────────────────────────────────────────────────────────────────────────────┤
│                              │ Search Results (2 matches)                  │
│ ▸ Editor                     │ ╔══════════════════════════════════════════╗│
│   Appearance ←────────────── │ ║ Line Numbers                      [✓]  ●║│
│   Behavior                   │ ║ Editor > Appearance                      ║│
│   Performance                │ ║ Show line numbers in the gutter         ║│
│ ▸ File Explorer              │ ╠══════════════════════════════════════════╣│
│ ▸ Terminal                   │ ║ Relative Line Numbers             [ ]   ║│
│ ...                          │ ║ Editor > Appearance                      ║│
│                              │ ║ Show line numbers relative to cursor    ║│
│                              │ ╚══════════════════════════════════════════╝│
│                              │                                             │
│                              │ [↑↓ Navigate results]  [Enter: Jump to]    │
├────────────────────────────────────────────────────────────────────────────┤
│ Esc:Clear search  Enter:Go to result  Tab:Next result                      │
└────────────────────────────────────────────────────────────────────────────┘
```

Features:
- Fuzzy matching on setting names and descriptions
- Results show breadcrumb path (Editor > Appearance)
- Left panel highlights matching categories
- Press `Enter` to jump to setting
- `Esc` clears search and returns to browse mode

---

## Modified Settings Tracking

Visual indicators for changes:

```
┌──────────────────────────────────────────────────────────────┐
│ Line Numbers                                          [✓]  ●│  ← Modified
│ Relative Line Numbers                                 [ ]   │  ← Default
│ Tab Size                                             [2   ]●│  ← Modified
└──────────────────────────────────────────────────────────────┘
```

The header shows pending changes:

```
┌─ Settings ──────────────────────────────────────────── 3 unsaved changes ──┐
```

---

## Actions Bar

At the bottom of the settings panel, contextual actions:

```
┌──────────────────────────────────────────────────────────────┐
│                                                              │
│  [Reset to Default]     [Revert All Changes]     [Save]      │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

- **Reset**: Reset current setting to default
- **Save**: Save changes and close dialog
- **Cancel**: Discard changes and close dialog (shows confirmation if unsaved changes exist)

---

## Keyboard Navigation

| Key | Action |
|-----|--------|
| `↑` `↓` | Navigate settings list |
| `←` `→` | Switch between category tree and settings panel (or increment/decrement values) |
| `Enter` | Toggle bool, open dropdown, edit value, or activate footer button |
| `Space` | Toggle boolean (alternative) |
| `Tab` | Cycle between panels: categories → settings → footer buttons |
| `Shift+Tab` | Cycle panels in reverse |
| `/` | Focus search field |
| `Esc` | Clear search / close dropdown / exit settings |
| `?` | Show help overlay |

### Category Tree Navigation
| Key | Action |
|-----|--------|
| `Enter` | Expand/collapse section |
| `→` | Expand and enter section |
| `←` | Collapse or go to parent |

---

## Edit Mode for Complex Values

For arrays and objects (like keybindings), switch to a JSON editor:

```
┌─ Editing: keybindings ─────────────────────────────────────────────────────┐
│                                                                            │
│   1│ [                                                                     │
│   2│   {                                                                   │
│   3│     "key": "s",                                                       │
│   4│     "modifiers": ["ctrl"],                                            │
│   5│     "action": "save"                                                  │
│   6│   },                                                                  │
│   7│   {                                                                   │
│   8│     "key": "q",                                                       │
│   9│     "modifiers": ["ctrl"],                                            │
│  10│     "action": "quit"                                                  │
│  11│   }                                                                   │
│  12│ ]                                                                     │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│ Ctrl+Enter:Save and close  Esc:Cancel  (JSON syntax highlighted)          │
└────────────────────────────────────────────────────────────────────────────┘
```

- Full JSON editor with syntax highlighting
- Validation before save
- Schema tooltips on hover (if mouse enabled)

---

## Help Overlay

Press `?` to show help:

```
┌─ Settings Help ────────────────────────────────────────────────────────────┐
│                                                                            │
│  NAVIGATION                      EDITING                                   │
│  ──────────                      ───────                                   │
│  ↑/↓    Navigate settings        Enter   Toggle/edit value                 │
│  ←/→    Switch panels            Space   Toggle checkbox                   │
│  Tab    Next field               Backsp  Clear field                       │
│  /      Search settings          Ctrl+Z  Undo change                       │
│                                                                            │
│  ACTIONS                         FILES                                     │
│  ───────                         ─────                                     │
│  Ctrl+S Save changes             Config: ~/.config/fresh/config.json       │
│  Esc    Close / Cancel                                                     │
│  ?      Toggle this help                                                   │
│                                                                            │
│  Settings are saved to config.json. The JSON file can be edited directly   │
│  for advanced configuration. Some settings require restart to take effect. │
│                                                                            │
│                                              [Press any key to close]      │
└────────────────────────────────────────────────────────────────────────────┘
```

---

## Confirmation Dialogs

### Unsaved Changes on Exit
```
┌─ Unsaved Changes ───────────────────────────────────────────┐
│                                                             │
│  You have 3 unsaved changes:                                │
│                                                             │
│    • editor.tab_size: 4 → 2                                 │
│    • editor.line_numbers: true → false                      │
│    • theme: "high-contrast" → "monokai"                     │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐    │
│  │   [Save and Exit]    [Discard]    [Cancel]          │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Reset Confirmation
```
┌─ Reset to Default ──────────────────────────────────────────┐
│                                                             │
│  Reset "tab_size" to its default value?                     │
│                                                             │
│  Current: 2                                                 │
│  Default: 4                                                 │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐    │
│  │        [Reset]              [Cancel]                │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## LSP Server Management

Special UI for adding/configuring LSP servers:

```
┌─ Add Language Server ───────────────────────────────────────────────────────┐
│                                                                             │
│  Language:  [go                                                           ] │
│                                                                             │
│  Command:   [gopls                                                        ] │
│  Arguments: [                                                             ] │
│                                                                             │
│  ┌─ Common Servers ─────────────────────────────────────────────────────┐   │
│  │ rust       → rust-analyzer                                           │   │
│  │ python     → pylsp                                                   │   │
│  │ typescript → typescript-language-server --stdio                      │   │
│  │ go         → gopls                                                   │   │
│  │ c/cpp      → clangd                                                  │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │        [Add Server]              [Cancel]                           │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Responsive Layout

For narrow terminals (< 80 columns), use stacked layout:

```
┌─ Settings ─────────────────────────────────────────┐
│ [/] Search...                           [?] [Esc] │
├────────────────────────────────────────────────────┤
│ ◀ Editor > Appearance                              │
├────────────────────────────────────────────────────┤
│ Line Numbers                              [✓]    ● │
│ Show line numbers in the gutter                    │
├────────────────────────────────────────────────────┤
│ Relative Line Numbers                     [ ]      │
│ Show relative line numbers                         │
├────────────────────────────────────────────────────┤
│ Tab Size                                  [4   ]   │
│ Spaces per tab                                     │
├────────────────────────────────────────────────────┤
│ ↑↓:Nav  Enter:Edit  ◀:Back  Ctrl+S:Save           │
└────────────────────────────────────────────────────┘
```

- Category shown as breadcrumb at top
- `←` / `Backspace` returns to category list
- Full-width settings panel

---

## Architectural Integration

### Current Fresh Rendering Patterns

Fresh uses a consistent architecture across all UI components:

1. **Renderer Pattern**: Static structs with `render()` methods (e.g., `FileBrowserRenderer`, `MenuRenderer`, `SplitRenderer`)
2. **State Separation**: State structs are separate from renderer structs (e.g., `FileOpenState` vs `FileBrowserRenderer`)
3. **Layout Structs**: Renderers return layout info for mouse hit testing (e.g., `FileBrowserLayout`)
4. **Direct Ratatui Usage**: No custom Widget trait - renders directly with ratatui primitives
5. **Input Context Routing**: `KeyContext` enum determines which component handles input

### Existing Reusable Components

Fresh already has these reusable building blocks:

| Component | Location | Purpose |
|-----------|----------|---------|
| `ScrollbarState` / `render_scrollbar()` | `view/ui/scrollbar.rs` | Scrollbar with state calculation and hit testing |
| `PopupManager` / `Popup` | `view/popup.rs` | Stack-based popup system with positioning |
| `PopupListItem` | `view/popup.rs` | List items with icon, text, detail |
| `parse_markdown()` | `view/popup.rs` | Markdown → styled lines for terminal |

### Integration Strategy

The settings editor will follow Fresh's established patterns while introducing reusable form controls.

#### Module Structure

```
src/
├── bin/
│   └── generate_schema.rs  # Schema generation binary (uses schemars)
│
└── view/
    ├── settings/
    │   ├── mod.rs          # Public exports, SettingsView coordinator
    │   ├── schema.rs       # Load and parse JSON Schema, build SettingsTree
    │   ├── state.rs        # SettingsState, focus, change tracking
    │   ├── render.rs       # SettingsRenderer (static render method)
    │   ├── layout.rs       # SettingsLayout for hit testing
    │   └── search.rs       # Fuzzy search over settings (titles + descriptions)
    │
    └── controls/           # NEW: Reusable form controls
        ├── mod.rs          # Control enum and common traits
        ├── toggle.rs       # Boolean checkbox control
        ├── number_input.rs # Numeric input with validation
        ├── dropdown.rs     # Enum/list selector
        ├── text_input.rs   # Single-line text field
        ├── text_list.rs    # List of strings (add/remove)
        └── button.rs       # Clickable button
```

#### New KeyContext

Add `KeyContext::Settings` to the input routing system, with priority between `Prompt` and `Normal`.

#### Control Abstraction

The `controls/` module provides reusable form primitives following the scrollbar pattern:

**ControlState structs** - Hold mutable state for each control type:
- `ToggleState`: value, focused
- `NumberInputState`: value, editing, cursor_pos, validation_error
- `DropdownState`: selected_index, options, expanded, scroll_offset
- `TextInputState`: value, cursor_pos, selection
- `TextListState`: items, selected_index, editing_index
- `ButtonState`: focused, pressed

**Render functions** - Static functions that render controls:
- Take `frame`, `area`, `state`, `theme` parameters
- Return layout info for hit testing
- Follow the `render_scrollbar()` pattern

**ControlLayout structs** - Enable mouse interaction:
- Hit testing regions
- Click-to-action mapping

#### Reuse Opportunities

These controls can be reused across Fresh:

| Control | Settings Editor | Other Uses |
|---------|-----------------|------------|
| Toggle | Boolean settings | View menu checkboxes, confirmation dialogs |
| NumberInput | Numeric settings | Goto line dialog, width/height inputs |
| Dropdown | Enum settings | Theme selector, keybinding map selector |
| TextInput | String settings | Search field, rename dialog |
| TextList | Array settings | Custom ignore patterns |
| Button | Action buttons | Confirmation dialogs, wizard navigation |

### State Management Design

#### SettingsState Structure

- `original_config`: Snapshot of config at open time
- `working_config`: Current config with pending changes
- `pending_changes`: Map of setting paths to their changed values (for modified indicators)
- `selected_category`: Current category/subcategory path
- `selected_setting`: Index within current category
- `focus`: Which panel has focus (Tree, Settings, Search, Control)
- `control_states`: Map of setting paths to their control states
- `search_query`: Current search text
- `search_results`: Filtered settings matching query
- `scroll_offsets`: Per-category scroll positions

#### Change Detection

- Compare `working_config` field values against `original_config`
- Track which settings have `pending_changes` for the `●` modified indicator
- On save: write `working_config` to disk, update `original_config`
- On cancel: restore from `original_config`

### JSON Schema-Based Config Integration

Use JSON Schema as the standard, well-understood format for describing config structure. The schema is already generated from Rust types via `#[derive(JsonSchema)]`, making it the obvious choice.

#### Research: How Zed Does It

Zed's settings UI uses a **field accessor pattern** ([source](https://zed.dev/blog/settings-ui)):
- `SettingField<T>` with function pointers for pick/write operations
- Type-based renderer registry mapping `TypeId` to control renderers
- Manual `SettingItem` definitions with title, description, field accessors

**Key insight**: Zed initially tried a **macro-based approach** where settings were annotated with UI metadata, but abandoned it because it "glued UI concerns into non-UI crates."

#### Our Approach: JSON Schema

We already have `#[derive(JsonSchema)]` on Config types. Rather than building custom accessor infrastructure, use the schema directly:

**Why JSON Schema is better for Fresh:**
- **Standard format**: Well-known, tooling exists, self-documenting
- **Already generated**: We have `schemars` derive, just need proper generation
- **Obvious mapping**: Schema types → UI controls is mechanical
- **External tooling**: LSP can validate config files, editors get autocomplete
- **Less code**: No `SettingField<T>`, no `field!` macro, no registry

**Schema → UI Mapping:**

| JSON Schema | Control Type |
|-------------|--------------|
| `type: "boolean"` | Toggle |
| `type: "integer"` | NumberInput |
| `type: "number"` | NumberInput |
| `type: "string"` + `enum` | Dropdown |
| `type: "string"` | TextInput |
| `type: "object"` | Section (recurse into properties) |
| `type: "array"` | TextList |
| `$ref` | Resolve and recurse |

**Information from schema:**

| UI Need | Schema Source |
|---------|---------------|
| Label | Property name → Title Case |
| Description | `description` field |
| Default value | `default` field |
| Control type | `type` field |
| Enum options | `enum` array |
| Categories | Object nesting / `$ref` structure |

#### Read/Write via JSON Pointers

Use serde_json's pointer API for dynamic access:

```rust
// Read
let value = serde_json::to_value(&config)?;
let tab_size = value.pointer("/editor/tab_size");

// Write
let mut value = serde_json::to_value(&config)?;
*value.pointer_mut("/editor/tab_size").unwrap() = json!(2);
let config: Config = serde_json::from_value(value)?;
```

This trades compile-time field access safety for simplicity. Errors are caught at:
1. Schema validation (structure matches)
2. Serde deserialization (types match)

#### Category Structure (from schema)

Derived automatically from schema's object nesting:

```json
{
  "$defs": {
    "Config": {
      "properties": {
        "theme": { "type": "string", "description": "Color theme..." },
        "editor": { "$ref": "#/$defs/EditorConfig" }
      }
    },
    "EditorConfig": {
      "properties": {
        "tab_size": { "type": "integer", "description": "Spaces per tab..." },
        "line_numbers": { "type": "boolean", "description": "Show line numbers..." }
      }
    }
  }
}
```

UI walks this structure to build:
- Top-level properties → Top-level settings
- `$ref` to object types → Collapsible sections
- Nested properties → Settings within sections

### Input Handling Integration

#### New Actions

Add to the `Action` enum:
- `OpenSettings` - Open settings modal
- `SettingsClose` - Close settings (with unsaved check)
- `SettingsSave` - Save and stay open
- `SettingsSearch` - Focus search field
- `SettingsNavigate(Direction)` - Move between settings
- `SettingsToggle` - Toggle current boolean / expand dropdown
- `SettingsEdit` - Enter edit mode for current control
- `SettingsReset` - Reset current setting to default

#### Keybinding Defaults

Register these in the `default` keymap for `KeyContext::Settings`:
- `Escape` → `CloseSettings`
- `/` → `SettingsSearch`
- `Up/Down` → Navigate settings
- `Left/Right` → Increment/decrement values, or navigate footer buttons
- `Enter` → Toggle/activate current control
- `Tab` → Cycle focus between panels (categories → settings → footer)
- `?` → Show help overlay

### Rendering Integration

#### Entry Point

Add settings rendering in `Editor.render()` after popups but before menu:
- Check if `settings_state` is `Some`
- Call `SettingsRenderer::render()` with full-screen area
- Settings modal overlays everything except menu bar

#### Two-Panel Layout

The renderer calculates layout based on terminal width:
- Wide mode (≥80 cols): Side-by-side panels
- Narrow mode (<80 cols): Stacked panels with breadcrumb

#### Render Order

Within the settings modal:
1. Clear background
2. Render border and title with change count
3. Render category tree (left panel)
4. Render settings list (right panel)
5. Render active control overlay (dropdown menu, etc.)
6. Render footer with keyboard shortcuts
7. Render search overlay if active
8. Render help overlay if active

### Mouse Support

Following the `FileBrowserLayout` pattern:

**SettingsLayout** tracks hit regions:
- `tree_area`: Category tree panel
- `settings_area`: Settings list panel
- `search_area`: Search field
- `per_setting_areas`: Vec of (Rect, SettingPath) for each visible setting
- `control_layouts`: Map of active control layouts

**HoverTarget variants** (add to existing enum):
- `SettingsCategory(CategoryPath)`
- `SettingRow(SettingPath)`
- `SettingsControl(SettingPath, ControlRegion)`
- `SettingsScrollbar`

---

## Migration from Plugin ✅ COMPLETE

The `plugins/config_editor.ts` plugin has been fully removed. The built-in Settings UI is now the only way to edit configuration (besides directly editing config.json).

### What was done:

1. ✅ Implemented native settings view in Rust
2. ✅ Added `OpenSettings` action to command palette
3. ✅ Added `Ctrl+,` keybinding to open Settings
4. ✅ Added "Settings..." to Edit menu
5. ✅ Removed `config_editor.ts` plugin and its e2e tests
6. ✅ Removed `s` and `Ctrl+S` keybindings (previously used by plugin)
7. ✅ Settings dialog now consumes all keyboard input (global shortcuts don't leak through)
8. ✅ Theme and keybinding changes are applied immediately after saving

### Advantages of Native Implementation

- Faster startup (no JavaScript evaluation)
- Direct access to Config struct and schema
- Better keyboard handling (consumes all input when modal is open)
- Consistent with other native UI components
- No TextEncoder/TextDecoder polyfill issues
- Runtime state updates (theme, keybindings) applied immediately

---

## Implementation Plan

### Status Summary

| Phase | Status | Notes |
|-------|--------|-------|
| Phase 1: Controls Module | ✅ DONE | All controls implemented with tests |
| Phase 2: Schema Generation | ✅ DONE | 5-line binary replaces 620-line build.rs |
| Phase 3: Settings UI | ✅ DONE | Basic modal with navigation working |
| Phase 4: Search & Polish | ✅ DONE | Help overlay, confirmation dialog, search UI all working |
| Phase 5: Migration | ✅ DONE | Plugin removed, Settings UI is now the only config editor |

### Known Bugs (found during testing)

| Bug | Severity | Status | Description |
|-----|----------|--------|-------------|
| Keybindings not editable | High | Open | Shows `<Complex - edit in config.toml>`. Need table UI for key/modifiers/action entries. |
| Menus not editable | High | Open | Shows `<Complex - edit in config.toml>`. Need hierarchical tree UI for menu structure. |
| Map entry values not editable | High | Open | Map entries show `{N fields}` but expanding/editing individual fields not implemented. |
| Dropdown editing doesn't work | High | ✅ Fixed | Enter/arrows on dropdown now work correctly. |
| Number input editing not implemented | High | ✅ Fixed | Enter to edit, type value, Enter to confirm. Left/Right also increment/decrement. |
| No settings item selection indicator | Medium | Open | Can't see which setting is selected in the settings panel. This makes navigation nearly unusable. |
| View doesn't scroll to selection | Medium | Open | After search jump, view doesn't scroll to show the selected item. |
| Search text input broken | High | ✅ Fixed | Settings actions weren't routed to handle_action. |
| Confirmation dialog empty | Medium | ✅ Fixed | Dialog height calculation was off by 1, causing changes to overlap with separator. |
| No button selection indicator | Medium | ✅ Fixed | Added ▶ indicator and bold styling for selected button. |
| No panel focus indicator | Low | Open | Can't visually tell if categories or settings panel has focus. |
| Terminal captures input when Settings opens | High | ✅ Fixed | Added Settings to popup/prompt check in input routing. Also added OpenSettings to terminal UI actions. |
| Footer buttons inaccessible via keyboard | High | ✅ Fixed | Added FocusPanel enum with Categories/Settings/Footer states. Tab now cycles through all three panels. Footer buttons now navigable with Left/Right arrows. |
| Global shortcuts leak through Settings | High | ✅ Fixed | Ctrl+P (palette), Ctrl+Q (quit), etc. were not consumed when Settings was open. Fixed by consuming all unhandled keys in Settings context. |
| Search results unrelated to query | Medium | Open | Searching "font" returns 14 results with no "font" matches. Fuzzy matching too aggressive or broken. |
| "●" indicator unexplained | Low | Open | Some categories show ● with no explanation. Users don't know if it means unsaved changes, errors, etc. |
| Left/Right for +/- undiscoverable | Low | Open | Arrow keys increment/decrement number fields but help text only shows "↑↓:Navigate". |
| Cancel in Unsaved Changes dialog closes everything | Medium | Open | "Cancel" should return to Settings, but instead closes the entire dialog like Discard. |
| [+] Add new buttons don't respond | Medium | Open | Pressing Enter on "[+] Add new" for Keybinding Maps/Languages produces no response. |
| Empty Unsaved Changes dialog persists | Medium | Open | "You have unsaved changes" dialog appears with no changes listed, persists across Settings reopens. |
| Dropdown options have no selection indicator | Low | Open | When dropdown is open, no visible highlight shows which option is selected (only preview updates). |
| Escape doesn't close Settings directly | Low | Open | Help text says "Esc:Close" but Escape only triggers unsaved changes flow, doesn't close directly. |
| Theme not applied after save | High | ✅ Fixed | save_settings() now updates runtime state (theme, keybindings) after saving config. |

### Phase 1: Core Controls Module ✅

Create reusable form controls that can be used independently of settings.

**New files:**
- `src/view/controls/mod.rs` - Control types enum and common rendering utilities
- `src/view/controls/toggle.rs` - ToggleState, render_toggle(), ToggleLayout
- `src/view/controls/number_input.rs` - NumberInputState, render_number_input()
- `src/view/controls/dropdown.rs` - DropdownState, render_dropdown()
- `src/view/controls/text_input.rs` - TextInputState, render_text_input()
- `src/view/controls/button.rs` - ButtonState, render_button()

**Design principles:**
- Each control is self-contained with state, render function, and layout
- Controls follow the scrollbar pattern: state struct + render function + layout struct
- Theme-aware: all colors from Theme struct
- Keyboard and mouse support in layouts
- No dependencies on settings-specific code

### Phase 2: Robust Schema Generation

The current `build.rs` uses ~600 lines of custom Rust parsing to extract config structure. This is fragile and duplicates what `schemars` already provides. Replace with a proper approach.

#### Current Problem

```
build.rs (fragile):
├── Custom regex-like parsing of Rust source
├── Manually extracts structs, fields, doc comments
├── Reimplements serde attribute parsing
├── Breaks on edge cases
└── Hard to maintain
```

#### Solution: Schema Generation Binary

Add a simple binary that uses schemars properly:

```
src/bin/
└── generate_schema.rs  # Uses schemars::schema_for!(Config)
```

**Schema generation is trivial (5 lines):**
```rust
fn main() {
    let schema = schemars::schema_for!(Config);
    println!("{}", serde_json::to_string_pretty(&schema).unwrap());
}
```

**Usage:**
```bash
cargo run --bin generate_schema > plugins/config-schema.json
```

**CI verification** (in `.github/workflows/ci.yml`):
```yaml
- name: Generate schema
  run: cargo run --bin generate_schema > /tmp/config-schema.json
- name: Check schema is up-to-date
  run: diff -u plugins/config-schema.json /tmp/config-schema.json
```

#### Benefits

- **Standard tooling**: Uses schemars as intended, no custom parsing
- **Correct by construction**: Schema always matches types exactly
- **Maintainable**: Deleted ~620 lines from build.rs, replaced with 5-line binary
- **CI verified**: Schema drift is caught in CI
- **No restructuring needed**: Config types stay in main crate

### Phase 3: Settings UI with Schema

Build the settings UI that reads from JSON Schema.

**New files:**
- `src/view/settings/schema.rs` - Parse schema, build category tree
- `src/view/settings/items.rs` - `SettingItem`, `SettingsPage` from schema
- `src/view/settings/state.rs` - SettingsState, change tracking
- `src/view/settings/render.rs` - SettingsRenderer

**Implementation approach:**
- Load schema at startup (from embedded file or runtime generation)
- Walk schema `$defs` and `properties` to build UI structure
- Map JSON Schema types to controls:
  - `type: "boolean"` → Toggle
  - `type: "integer"` / `type: "number"` → NumberInput
  - `type: "string"` + `enum` → Dropdown
  - `type: "string"` → TextInput
  - `type: "object"` → Section (recurse)
  - `type: "array"` → TextList
- Use `description` field for setting descriptions
- Use `default` field for reset functionality
- Use JSON pointer paths for read/write (`/editor/tab_size`)

**Read/write via serde_json:**
```rust
// Read current value
let config_value = serde_json::to_value(&config)?;
let tab_size = config_value.pointer("/editor/tab_size");

// Write new value
let mut config_value = serde_json::to_value(&config)?;
*config_value.pointer_mut("/editor/tab_size").unwrap() = new_val;
let config: Config = serde_json::from_value(config_value)?;
```

**Additional files:**
- `src/view/settings/layout.rs` - SettingsLayout for hit testing

**Modifications:**
- `src/app/mod.rs` - Add `settings_state: Option<SettingsState>` to Editor
- `src/app/render.rs` - Render settings modal when active
- `src/input/keybindings.rs` - Add `KeyContext::Settings` and related actions
- `src/app/input.rs` - Handle settings actions

### Phase 4: Search & Polish

Add search functionality and UX polish.

**New files:**
- `src/view/settings/search.rs` - Fuzzy search over settings

**Features:**
- Fuzzy matching on setting names and descriptions
- Highlight matching categories in tree
- Search results panel with breadcrumbs
- Help overlay (?)
- Unsaved changes confirmation dialog

### Phase 5: Migration & Cleanup ✅

Replace the plugin-based config editor.

**Completed steps:**
1. ✅ Added command palette entry "Settings" → `OpenSettings` action
2. ✅ Added "Settings..." to Edit menu
3. ✅ Added keyboard shortcut `Ctrl+,`
4. ✅ Removed `config_editor.ts` plugin entirely
5. ✅ Removed `s` and `Ctrl+S` keybindings from default keymap
6. ✅ Settings dialog consumes all keyboard input (no global shortcut leakage)
7. ✅ Runtime state (theme, keybindings) updated immediately on save

---

## Design Decisions

### Why Not Extend PopupManager?

The settings editor is too complex for the popup system:
- Needs two-panel layout with independent scrolling
- Requires persistent state across focus changes
- Has multiple layers of controls (dropdown over settings panel)
- Needs full keyboard navigation with context switching

Instead, settings is a modal view like the file picker, but with richer UI.

### Why Separate Controls Module?

Form controls are generally useful beyond settings:
- Goto line dialog could use NumberInput
- Rename dialog could use TextInput
- Confirmation dialogs could use Button
- Menu items with toggles could use Toggle

Extracting controls enables gradual adoption without modifying existing code.

### Why JSON Schema Over Other Approaches?

Alternatives considered:

1. **Derive macro on Config types**: `#[derive(SettingsUI)]` generates metadata
   - Con: Zed tried this and abandoned it - "glued UI concerns into non-UI crates"
   - Con: Harder to control ordering, grouping, custom descriptions

2. **Visitor pattern**: Config types implement trait to describe themselves
   - Con: Still couples UI metadata to data types
   - Con: Requires manual trait impls or proc macro

3. **Field accessor pattern** (Zed's approach):
   - Pro: Type-safe via function pointers
   - Con: Requires defining every setting manually in UI code
   - Con: More infrastructure to build (`SettingField<T>`, registry, etc.)

4. **JSON Schema** (chosen):
   - Pro: Standard format, well-understood
   - Pro: Already generated via `#[derive(JsonSchema)]`
   - Pro: Automatic - new Config fields appear in UI automatically
   - Pro: External tooling (LSP validation, editor autocomplete)
   - Pro: Less custom code - just walk the schema
   - Con: Runtime type safety only (caught by serde on deserialize)

**The key insight**: We already have the schema. The current problem is *how* we generate it (fragile build.rs parsing), not *whether* to use it. Fix the generation, keep the schema.

---

## ScrollablePanel Abstraction

The settings panel scrolling logic should be extracted into a reusable `ScrollablePanel` component. This follows patterns from established UI frameworks.

### Inspiration from Other Frameworks

| Framework | Component | Key Patterns |
|-----------|-----------|--------------|
| Flutter | `ListView` / `Sliver` | Items report size, `Scrollable.ensureVisible()` |
| WPF/WinUI | `ScrollViewer` | `BringIntoView()`, pixel-based scrolling |
| Qt | `QAbstractScrollArea` | Viewport + scroll position, `ensureWidgetVisible()` |
| Web/React | `react-window` | Virtual scrolling, dynamic measurement |

### Core Design

#### ScrollState - Pure State Management

```rust
/// Pure scroll state - knows nothing about content
pub struct ScrollState {
    /// Scroll offset in rows (not items)
    pub offset: u16,
    /// Viewport height
    pub viewport: u16,
    /// Total content height
    pub content_height: u16,
}

impl ScrollState {
    /// Create new scroll state
    pub fn new(viewport: u16) -> Self;

    /// Update content height (call when items change)
    pub fn set_content_height(&mut self, height: u16);

    /// Scroll to ensure a region is visible
    /// If region is taller than viewport, shows the top
    pub fn ensure_visible(&mut self, y: u16, height: u16) {
        if y < self.offset {
            // Region is above viewport - scroll up
            self.offset = y;
        } else if y + height > self.offset + self.viewport {
            // Region is below viewport - scroll down
            if height > self.viewport {
                // Oversized item - show top
                self.offset = y;
            } else {
                self.offset = y + height - self.viewport;
            }
        }
    }

    /// Scroll by delta rows (positive = down, negative = up)
    pub fn scroll_by(&mut self, delta: i16);

    /// Scroll to a ratio (0.0 = top, 1.0 = bottom)
    pub fn scroll_to_ratio(&mut self, ratio: f32);

    /// Get scrollbar thumb ratio (size relative to track)
    pub fn thumb_ratio(&self) -> f32 {
        self.viewport as f32 / self.content_height as f32
    }

    /// Get scrollbar thumb position (0.0 to 1.0)
    pub fn thumb_position(&self) -> f32 {
        self.offset as f32 / (self.content_height - self.viewport) as f32
    }

    /// Check if scrolling is needed
    pub fn needs_scrollbar(&self) -> bool {
        self.content_height > self.viewport
    }
}
```

#### ScrollItem Trait - Item Size Reporting

```rust
/// Trait for items that can be displayed in a scrollable panel
pub trait ScrollItem {
    /// Total height of this item in terminal rows
    fn height(&self) -> u16;

    /// Optional: sub-focus regions within this item
    /// Used for items with internal navigation (e.g., TextList rows)
    fn focus_regions(&self) -> &[FocusRegion] {
        &[]
    }
}

/// A focusable region within an item
pub struct FocusRegion {
    /// Identifier for this region
    pub id: usize,
    /// Y offset within the parent item
    pub y_offset: u16,
    /// Height of this region
    pub height: u16,
}
```

#### ScrollablePanel - Rendering Coordinator

```rust
/// Manages scrolling for a list of items
pub struct ScrollablePanel {
    scroll: ScrollState,
}

impl ScrollablePanel {
    pub fn new(viewport_height: u16) -> Self;

    /// Update scroll state for new viewport size
    pub fn set_viewport(&mut self, height: u16);

    /// Calculate total content height from items
    pub fn update_content_height<I: ScrollItem>(&mut self, items: &[I]) {
        let height: u16 = items.iter().map(|i| i.height()).sum();
        self.scroll.set_content_height(height);
    }

    /// Ensure focused item (and optional sub-region) is visible
    pub fn ensure_focused_visible<I: ScrollItem>(
        &mut self,
        items: &[I],
        focused_index: usize,
        sub_focus: Option<usize>,
    ) {
        // Calculate Y offset of focused item
        let item_y: u16 = items[..focused_index].iter().map(|i| i.height()).sum();
        let item = &items[focused_index];

        // If sub-focus specified, use that region
        let (focus_y, focus_h) = if let Some(sub_id) = sub_focus {
            if let Some(region) = item.focus_regions().iter().find(|r| r.id == sub_id) {
                (item_y + region.y_offset, region.height)
            } else {
                (item_y, item.height())
            }
        } else {
            (item_y, item.height())
        };

        self.scroll.ensure_visible(focus_y, focus_h);
    }

    /// Render visible items and scrollbar
    /// Returns layout info for hit testing
    pub fn render<I, F, L>(
        &self,
        frame: &mut Frame,
        area: Rect,
        items: &[I],
        render_item: F,
        theme: &Theme,
    ) -> ScrollablePanelLayout<L>
    where
        I: ScrollItem,
        F: Fn(&mut Frame, Rect, &I, usize) -> L,
    {
        let scrollbar_width = if self.scroll.needs_scrollbar() { 1 } else { 0 };
        let content_area = Rect::new(
            area.x,
            area.y,
            area.width.saturating_sub(scrollbar_width),
            area.height,
        );

        let mut layouts = Vec::new();
        let mut current_y = 0u16;
        let mut render_y = area.y;

        for (idx, item) in items.iter().enumerate() {
            let item_h = item.height();

            // Skip items before scroll offset
            if current_y + item_h <= self.scroll.offset {
                current_y += item_h;
                continue;
            }

            // Stop if past viewport
            if render_y >= area.y + area.height {
                break;
            }

            // Calculate visible portion of item
            let skip_top = self.scroll.offset.saturating_sub(current_y);
            let available_h = (area.y + area.height).saturating_sub(render_y);
            let visible_h = (item_h - skip_top).min(available_h);

            let item_area = Rect::new(content_area.x, render_y, content_area.width, visible_h);
            let layout = render_item(frame, item_area, item, idx);
            layouts.push((idx, layout));

            render_y += visible_h;
            current_y += item_h;
        }

        // Render scrollbar if needed
        if self.scroll.needs_scrollbar() {
            let scrollbar_area = Rect::new(
                area.x + content_area.width,
                area.y,
                1,
                area.height,
            );
            let scrollbar_state = ScrollbarState::new(
                self.scroll.content_height as usize,
                self.scroll.viewport as usize,
                self.scroll.offset as usize,
            );
            render_scrollbar(frame, scrollbar_area, &scrollbar_state, &ScrollbarColors::from_theme(theme));
        }

        ScrollablePanelLayout {
            content_area,
            scrollbar_area: if self.scroll.needs_scrollbar() {
                Some(Rect::new(area.x + content_area.width, area.y, 1, area.height))
            } else {
                None
            },
            item_layouts: layouts,
        }
    }

    // Delegate scroll operations
    pub fn scroll_up(&mut self, rows: u16) { self.scroll.scroll_by(-(rows as i16)); }
    pub fn scroll_down(&mut self, rows: u16) { self.scroll.scroll_by(rows as i16); }
    pub fn scroll_to_ratio(&mut self, ratio: f32) { self.scroll.scroll_to_ratio(ratio); }
}

/// Layout info returned by ScrollablePanel::render
pub struct ScrollablePanelLayout<L> {
    /// Content area (excluding scrollbar)
    pub content_area: Rect,
    /// Scrollbar area (if visible)
    pub scrollbar_area: Option<Rect>,
    /// Per-item layouts with their indices
    pub item_layouts: Vec<(usize, L)>,
}
```

### Handling Oversized Items

When an item is taller than the viewport (e.g., a TextList with 20 rows):

1. **Show top on first focus**: When navigating to an oversized item, show its top
2. **Sub-focus tracking**: Track which internal region is focused
3. **Scroll within item**: Use `ensure_visible()` with sub-focus region

```rust
// Example: TextList with 10 visible rows, user focuses row 15
panel.ensure_focused_visible(
    &items,
    focused_item_idx,
    Some(15), // Sub-focus: row 15 within the TextList
);
// This scrolls to show row 15, even if the TextList item is only partially visible
```

### Integration with Settings UI

The settings panel would use `ScrollablePanel` like this:

```rust
impl SettingsState {
    fn render_settings_panel(&mut self, frame: &mut Frame, area: Rect, theme: &Theme) {
        let page = self.current_page().unwrap();

        // Update content height
        self.scroll_panel.update_content_height(&page.items);

        // Ensure focused item visible
        if let Some(selected) = self.selected_item {
            self.scroll_panel.ensure_focused_visible(
                &page.items,
                selected,
                self.sub_focus, // For TextList/Map internal focus
            );
        }

        // Render
        let layout = self.scroll_panel.render(
            frame,
            area,
            &page.items,
            |frame, area, item, idx| render_setting_item(frame, area, item, idx, theme),
            theme,
        );

        // Store layout for hit testing
        self.panel_layout = Some(layout);
    }
}
```

### Benefits

1. **Reusable**: Same component for settings, file picker, popup lists, etc.
2. **Row-based scrolling**: Pixel/row precision instead of item-based
3. **Partial visibility**: Natural handling of items at viewport edges
4. **Sub-item focus**: Built-in support for navigating within large items
5. **Consistent behavior**: Same scrolling semantics everywhere
6. **Testable**: Pure state management can be unit tested

### Implementation Plan

1. Create `src/view/ui/scroll_panel.rs` with `ScrollState` and `ScrollablePanel`
2. Add `ScrollItem` trait
3. Implement `ScrollItem` for `SettingItem`
4. Refactor `render_settings_panel` to use `ScrollablePanel`
5. Add sub-focus support to `SettingsState` for TextList/Map navigation
6. Consider adopting for file picker and popup lists

---

## Future Enhancements

1. **Project Settings**: Per-project config overrides (`.fresh/config.json` in project root)
2. **Settings Profiles**: Named setting profiles (e.g., "Writing", "Coding") with quick switching
3. **Plugin Settings**: Plugin-contributed settings sections via the schema
4. **Import/Export**: Share settings snippets as JSON
5. **Search History**: Remember recent searches
6. **Keyboard Shortcut Editor**: Visual keybinding configuration
7. **Live Preview**: Immediate application of non-destructive settings
