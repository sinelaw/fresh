# Unified Keybinding Resolution

## Goals

- **Single resolution path**: All keybindings (builtin, keymap, plugin mode,
  user custom) resolve through KeybindingResolver. No separate ModeRegistry
  lookup.
- **Drop mode inheritance**: The `parent` parameter in `defineMode()` is
  removed. Unmatched keys fall through to normal dispatch, which already handles
  cursor movement, Ctrl shortcuts, etc. for buffer-local modes.
- **Simpler plugin API**: `defineMode()` takes `(name, bindings, readOnly?,
  allowTextInput?)`. Plugins declare only the keys they handle.
- **User overrides work uniformly**: A user binding with `"when": "mode:git-log"`
  overrides a plugin mode binding through the same mechanism as any other
  keybinding override.

## Background

When a plugin calls `defineMode("git-log", "normal", bindings)`, the bindings
are stored in two places:

1. **ModeRegistry** — `BufferMode.keybindings` HashMap, walked with parent
   chain inheritance
2. **KeybindingResolver** — `plugin_defaults` tier, keyed by `Mode("git-log")`
   context

The dispatch path in `input.rs` checks both: KeybindingResolver first (for user
overrides), then ModeRegistry (for plugin defaults + inheritance). This is
redundant — the same bindings exist in both systems.

Mode inheritance (`parent: "normal"`) copies ~60 keybindings from the parent
into the child mode. But buffer-local read-only modes already fall through to
normal dispatch for unmatched keys (lines 174-212 of `input.rs`), so the
inherited bindings never actually fire — the fallthrough handles them.

## Steps

### 1. Add explicit bindings where inheritance was load-bearing

Only 3 bindings across 2 files actually depend on inheritance:

- **`diagnostics_panel.ts`**: `diagnostics-extra` inherits `Enter` and `Escape`
  from `diagnostics-results`. Add these 2 bindings explicitly.
- **`lib/finder.ts`**: Preview mode inherits `Escape → close` from `"special"`.
  Add 1 explicit binding.

### 2. Strip keybinding storage from ModeRegistry

`BufferMode` becomes metadata-only:

```rust
struct BufferMode {
    name: String,
    read_only: bool,
    allow_text_input: bool,
    plugin_name: Option<String>,
}
```

Remove from `BufferMode`: `parent`, `keybindings`, `chord_keybindings`, and
builder methods (`with_parent`, `with_binding`, `with_chord_binding`,
`with_bindings`).

Remove from `ModeRegistry`: `resolve_keybinding`, `resolve_chord_keybinding`,
`is_chord_prefix`, `get_all_keybindings`, `normalize_key`.

Keep: `register`, `get`, `list_modes`, `has_mode`, `is_read_only`,
`allows_text_input`.

Remove the built-in "special" mode registration in `ModeRegistry::new()`.

### 3. Register mode chords into KeybindingResolver

Add `plugin_chord_defaults: HashMap<KeyContext, HashMap<Vec<(KeyCode, KeyModifiers)>, Action>>`
to `KeybindingResolver`.

Add `load_plugin_chord_default(context, sequence, action)` method, mirroring
`load_plugin_default` for single keys.

Add the new tier to `resolve_chord`'s search order:

```
1. chord_bindings          (custom)  — Global, then context
2. default_chord_bindings  (keymap)  — Global, then context
3. plugin_chord_defaults             — context only
```

In `handle_define_mode`: register multi-key bindings via
`load_plugin_chord_default` with `Mode("name")` context.

### 4. Simplify dispatch in input.rs

Replace the current mode dispatch block (lines 120-212) with:

```rust
if let Some(ref mode_name) = effective_mode {
    let mode_ctx = KeyContext::Mode(mode_name.to_string());
    let key_event = KeyEvent::new(code, modifiers);

    // Mode chords (via KeybindingResolver)
    let chord_result = self.keybindings.resolve_chord(
        &self.chord_state, &key_event, mode_ctx.clone()
    );
    match chord_result {
        ChordResolution::Complete(action) => {
            self.chord_state.clear();
            return self.handle_action(action);
        }
        ChordResolution::Partial => {
            self.chord_state.push((code, modifiers));
            return Ok(());
        }
        ChordResolution::NoMatch => {
            if !self.chord_state.is_empty() {
                self.chord_state.clear();
            }
        }
    }

    // Mode single-key (via KeybindingResolver: custom > keymap > plugin)
    let action = self.keybindings.resolve(&key_event, mode_ctx);
    if action != Action::None {
        return self.handle_action(action);
    }

    // Fallthrough behavior (unchanged)
    // - allow_text_input: chars → mode_text_input, others → block
    // - global editor mode + read_only: block
    // - otherwise: fall through to normal dispatch
}
```

Delete `resolve_mode_keybinding` from `mod.rs`.

### 5. Update keybinding editor

Change `KeybindingEditor::new` and `resolve_all_bindings` to read from
`keybindings.get_plugin_defaults()` instead of
`mode_registry.get_all_keybindings()`.

### 6. Update keybinding labels

In `handle_define_mode`, populate `keybinding_labels` from
`keybindings.get_plugin_defaults()` for the `Mode("name")` context instead of
`mode_registry.get_all_keybindings()`.

### 7. Remove `parent` from defineMode API

New signature:

```ts
defineMode(name: string, bindingsArr: string[][], readOnly?: boolean, allowTextInput?: boolean): boolean;
```

Update all ~25 plugin call sites to remove the parent argument.

Update Rust side: `PluginCommand::DefineMode`, `PluginApi`, QuickJS backend.

Update `fresh.d.ts`.

## Result

- One resolution path for all keybindings (mode and non-mode)
- ModeRegistry is ~30 lines of metadata storage
- `input.rs` dispatch shrinks by ~30 lines
- No dual registration
- Mode chords go through the same chord system as everything else
