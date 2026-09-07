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
| `d` or `Delete` | Delete the selected binding. A custom binding is dropped from `config.json`; a keymap or plugin binding is removed for good with an `unbind` entry, and the key falls through to whatever else binds it |
| `x` | Disable the selected binding: a `noop` override, so the key does nothing in that context |

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
| `key` | The key name (e.g., `"s"`, `"Enter"`, `"F1"`, `"Up"`) — see [Key Names](../configuration/keyboard.md#key-names) for every accepted spelling |
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

### Removing vs. disabling a built-in binding

The built-in keymaps are read-only, so the editor records what you do to
their bindings in `config.json`, and the two operations leave different
things behind:

* **Delete (`d`)** writes an `unbind` entry. The keymap binding is gone: it
  no longer appears in the editor, and the key falls through to whatever else
  binds it — a `global` entry, the parent keymap, or nothing.

  ```json
  { "key": "g", "modifiers": ["alt"], "action": "unbind", "when": "normal" }
  ```

  To bring the binding back, delete that entry from `config.json`.

* **Disable (`x`)** writes a `noop` override. That is a real binding: the key
  does nothing in that context, and nothing underneath it fires either. Use it
  when you want a key dead rather than free — `Ctrl+Q → quit`, everywhere,
  say. It shows in the editor as a custom `noop` row, and `d` on that row
  removes the override again.

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

The keybinding editor lists chords (shown as `Ctrl+X Ctrl+S`), and `d` removes
one the same way it removes any other keymap binding. Recording a *new* chord
from the editor's add/edit dialog is not supported yet — the key field captures
a single combination — so write those in `config.json` by hand.

A chord holds its first key only while it can still fire. Remove (or disable)
every keymap chord that starts with a key and that key is free: on the emacs
keymap, delete `Alt+G G` and `Alt+G Alt+G` and `Alt+G` opens the Go menu, the
`global` binding the chords were hiding. You don't have to remove them first,
though — a single-key binding you add outranks any keymap chord that starts
with the same key, so binding `Alt+G` to `goto_line` fires as soon as you
save. Only a chord you wrote yourself keeps its prefix over your own
single-key binding.
