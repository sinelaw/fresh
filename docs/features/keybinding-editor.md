# Keybinding Editor

The Keybinding Editor provides a visual interface for browsing, searching, and customizing keyboard shortcuts. Open it from **Edit → Keybinding Editor...** or via the Command Palette (`Ctrl+P` → "Keybinding Editor").

## Overview

The editor displays all active keybindings in a searchable, filterable table. Sections are collapsible, and plugin mode bindings are shown as first-class entries alongside built-in bindings. The table has five columns:

| Column | Description |
|--------|-------------|
| **Key** | The key combination (e.g., `Ctrl+S`) |
| **Action** | The machine-readable action name (e.g., `save`) |
| **Description** | Human-readable description (e.g., "Save file") |
| **Context** | When the binding is active (`normal`, `global`, `prompt`, etc.) |
| **Source** | Whether the binding comes from the active keymap or custom user config |

## Navigation

| Shortcut | Action |
|----------|--------|
| `↑` / `↓` | Move selection up/down |
| `PgUp` / `PgDn` | Page up/down |
| `Home` / `End` | Jump to first/last binding |

## Searching

Two search modes are available:

### Text Search

Press `/` to start a text search. Type to filter bindings by action name, description, key display, or context. Plugin-registered command names are included in search results. Press `Esc` to cancel.

### Key Recording Search

Press `r` to start a key recording search. Press any key combination to find all bindings that match that exact key. Press `Tab` to switch between text and key recording modes.

## Filtering

| Shortcut | Action |
|----------|--------|
| `c` | Cycle context filter (All → global → normal → prompt → ...) |
| `s` | Cycle source filter (All → Custom → Keymap) |

The current filter state is shown in the header bar.

## Adding and Editing Bindings

| Shortcut | Action |
|----------|--------|
| `Enter` | Edit the selected binding |
| `a` | Add a new binding |
| `d` or `Delete` | Delete a custom binding |

### The Edit Dialog

When adding or editing a binding, a dialog appears with three fields:

1. **Key** — Press the desired key combination. The dialog starts in key recording mode. To bind a special key like **Esc**, **Tab**, or **Enter**, press **Enter** on the key field first to enter recording, then press the key you want to bind; press **Escape** to cancel recording.
2. **Action** — Type an action name. An autocomplete popup shows matching actions as you type. Use `↑`/`↓` to navigate suggestions and `Tab` or `Enter` to accept. Only valid action names are accepted.
3. **Context** — Use `←`/`→` to cycle through available contexts (global, normal, prompt, searchPrompt, popup, completion, file\_explorer, dock, menu, terminal, settings, compositeBuffer).

Use `Tab` to move between fields. The dialog shows a read-only description of the selected action and warns about conflicting bindings.

Press **Save** to apply or **Cancel** to discard.

## Saving Changes

Press `Ctrl+S` to save all pending changes to your user config file. Changes are written to the `keybindings` array in your `config.json`.

If you try to close the editor with unsaved changes, a confirmation dialog offers three options: **Save**, **Discard**, or **Cancel**.

## How Keybindings Work

Fresh uses a layered keybinding system:

1. **Keymap** — A named set of default bindings (`default`, `emacs`, `vscode`, `macos`, `macos-gui`). Set via `"active_keybinding_map"` in your config.
2. **Custom bindings** — User overrides defined in the `"keybindings"` array of your config file. These take precedence over keymap bindings.

Custom bindings added through the editor are appended to the `keybindings` array. To switch the base keymap, use **View → Keybinding Style** or set `"active_keybinding_map"` in your config file:

```json
{
  "active_keybinding_map": "emacs"
}
```

Every built-in keymap other than `default` inherits from another one and
overrides only the keys it cares about, so anything a keymap doesn't mention
behaves like its parent. `vscode`, `macos`, and `emacs` inherit `default`;
`macos-gui` inherits `macos`.

### Binding Format

Each binding in `config.json` has this structure:

```json
{
  "keybindings": [
    {
      "key": "s",
      "modifiers": ["ctrl"],
      "action": "save",
      "when": "normal"
    }
  ]
}
```

| Field | Description |
|-------|-------------|
| `key` | The key name (e.g., `"s"`, `"Enter"`, `"F1"`, `"Up"`) |
| `modifiers` | Array of modifier keys: `"ctrl"`, `"alt"`, `"shift"`, `"super"` |
| `action` | The action to trigger (see action list via autocomplete in the editor) |
| `when` | Context when this binding is active (optional, defaults to `"normal"`) |

### Contexts

| Context | When Active |
|---------|-------------|
| `global` | Always active, regardless of focus |
| `normal` | When the text editor is focused |
| `prompt` | When an input prompt is active |
| `searchPrompt` | When the find/replace prompt is active — a narrowing of `prompt`, adding the match-mode toggles |
| `popup` | When a popup (completion, hover) is open |
| `completion` | When the completion popup is open — takes precedence over `popup` |
| `file_explorer` | When the file explorer has focus |
| `dock` | When the utility dock has focus |
| `menu` | When a menu is open |
| `terminal` | When the integrated terminal has focus |
| `settings` | When the settings editor is open |
| `compositeBuffer` | When a composite (diff / git log) view is focused |

When the same key is bound in more than one applicable context, the most
specific context wins: a `normal` or `prompt` binding outranks a `global` one
for the same key. `global` is the fallback that applies wherever no narrower
binding claims the key. Your custom bindings always outrank keymap defaults,
whatever context either uses.

### Chord (multi-key) bindings

A binding can be a *sequence* of keypresses instead of a single one — Emacs's
`C-x C-s` is one. Use `keys` instead of `key`/`modifiers`:

```json
{
  "keybindings": [
    {
      "keys": [
        { "key": "x", "modifiers": ["ctrl"] },
        { "key": "s", "modifiers": ["ctrl"] }
      ],
      "action": "save",
      "when": "normal"
    }
  ]
}
```

The keybinding editor lists chords (shown as `Ctrl+X Ctrl+S`), and `d` disables
one the same way it disables any other keymap binding. Recording a *new* chord
from the editor's add/edit dialog is not supported yet — the key field captures
a single combination — so write those in `config.json` by hand.
