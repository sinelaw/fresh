# Settings Modified Indicator Design

## Problem Statement

The current settings UI has several UX issues related to the "modified" indicator:

1. **Incorrect baseline for comparison**: The current implementation compares settings values against the schema default. This causes:
   - Auto-discovered content (like plugins) to show as "modified" on initial load because they differ from the empty schema default `{}`
   - The "Reset" button clears plugins entirely because it resets to schema default

2. **Section-level indicators are misleading**: The dot indicators on sections (General, Editor, Plugins) show "modified" based on comparison to schema defaults, not based on what the user has actually configured in the current layer.

3. **No visibility into individual item modifications**: Users cannot see which specific items have been modified at the current layer vs inherited from a lower layer.

## Goals

Design a UX similar to IntelliJ IDEA's settings:
- Show which items are explicitly defined in the current target layer (User/Project)
- "Reset" should remove the value from the current layer, falling back to inherited
- Auto-managed content (plugins) should not show as "modified" and should not be resettable

## Layered Configuration Architecture

Fresh uses a 4-layer configuration system (highest precedence first):
1. **Session** - Temporary runtime overrides (not persisted)
2. **Project** - Project-specific settings (`.fresh/config.toml`)
3. **User** - User-global settings (`~/.config/fresh/config.toml`)
4. **System** - Built-in defaults (schema defaults)

Values cascade: higher layers override lower layers. The final config is the merge of all layers.

## Design

### Definition of "Modified"

**Current behavior**: `modified = (current_value != schema_default)`

**Proposed behavior**: `modified = (value is defined in target_layer)`

For example, when editing User layer settings:
- An item is "modified" if it has a value defined in the User layer
- An item is NOT modified if it comes from System defaults or is undefined

This aligns with the UX concept: "modified" means "the user explicitly configured this in the current layer."

### Section-Level Indicators

The dot indicator next to category names (e.g., "General", "Editor") should show:
- **Filled dot (●)**: At least one item in this section is defined in the target layer
- **Empty (space)**: No items in this section are defined in the target layer

This is computed by aggregating: `category_modified = any(item.modified for item in category.items)`

### Individual Item Indicators

Each setting item should display:
1. **Layer source badge**: Shows which layer the current value comes from (User, Project, System)
2. **Modified indicator**: Shows if the item is defined in the current target layer

### Reset Behavior

**Current behavior**: Reset sets the value to schema default.

**Proposed behavior**: Reset removes the value from the current layer's delta.

This means:
- If User layer defines `tab_size: 2`, clicking Reset removes it from User layer
- The value then falls back to System default (or Project layer if editing Session)
- Items not defined in the current layer have nothing to reset

### Auto-Managed Content (Maps with `x-no-add`)

Plugins and other auto-discovered content use `x-no-add` schema extension:
- These Maps are populated automatically, not by user configuration
- They should **never** show as "modified" (even though they differ from empty default)
- They should **never** be resettable (Reset has no meaning for auto-discovered content)
- They should skip the modified calculation entirely

### Implementation Changes

#### 1. `build_item` / `build_page` functions

Add parameters:
- `layer_sources: &HashMap<String, ConfigLayer>` - Maps paths to their source layer
- `target_layer: ConfigLayer` - The layer being edited

Calculate modified as:
```rust
// For regular items
let modified = layer_sources.get(&schema.path) == Some(&target_layer);

// For Maps with no_add
let modified = false; // Auto-managed, never "modified"
```

#### 2. `reset_current_to_default` function

Change from:
```rust
// Set value to schema default
self.set_pending_change(&path, default.clone());
```

To:
```rust
// Remove value from delta (fall back to inherited)
// Only if the item is defined in the current layer
if item.modified {
    self.remove_pending_change(&path);
}
```

#### 3. Section indicator calculation

Already correct: `page.items.iter().any(|i| i.modified)`

Once `modified` is calculated correctly per-item, section indicators will automatically work.

## Migration Path

1. Update `build_item` signature to accept layer info
2. Update all callers (`build_page`, `build_pages`)
3. Pass `layer_sources` and `target_layer` from `SettingsState`
4. Update `reset_current_to_default` to remove from delta
5. Update tests that rely on old "modified" semantics

## Future Considerations

- Visual distinction between "inherited from User" vs "inherited from System"
- Ability to view/edit different layers (currently only User layer is editable)
- Diff view showing what's defined at each layer
