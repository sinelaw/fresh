# Fresh Editor Configuration System Design

## Overview
This document outlines the design for the next generation of the Fresh editor's configuration system. The current implementation relies on a single merged `Config` struct loaded from JSON, which lacks the flexibility of multi-layered overrides (System vs User vs Project) and robust "dual-writer" support.

The new design implements a **4-Level Overlay Architecture** with **Recursive Merging**, utilizing standard **JSON** as the primary configuration format.

## Architectural Goals
1.  **4-Level Hierarchy**: Clearly distinguish between System (defaults), User (global), Project (local), and Session (volatile) settings.
2.  **Deep Merging**: recursive merging of configuration objects (maps/lists) rather than simple replacement.
3.  **Minimal Persistence**: Only save the *delta* (changes) from the layer below.
4.  **Simplicity**: Use standard JSON to ensure maximum compatibility and ease of implementation.
5.  **Schema Evolution**: Robust versioning and migration strategy.

## 1. The 4-Level Overlay Semantics

The configuration will be resolved by merging layers in the following order (lowest to highest precedence):

| Level | Source | Path (Linux/macOS) | Purpose | Mutability |
| :--- | :--- | :--- | :--- | :--- |
| **1. System** | Embedded Binary | `src/config.rs` (Hardcoded) | Immutable defaults for all users. | Read-Only |
| **2. User** | Global File | `~/.config/fresh/config.json` | User preferences (theme, keymaps). | Read/Write |
| **3. Project** | Local File | `$PROJECT_ROOT/.fresh/config.json` | Project-specific overrides (indentation, build commands). | Read/Write |
| **4. Session** | Runtime/Volatile | Memory / `.fresh/session.json` | Temporary state (open files, cursor pos). | Read/Write |

**Resolution Logic:**
`EffectiveConfig = Merge(System, Merge(User, Merge(Project, Session)))`

### Multi-Root Workspaces (Future)
For multi-root workspaces, a 5th level "Workspace" can be inserted between User and Project.

## 2. Merging Strategy

The system will employ a **Deep Merge** strategy:

*   **Scalars (Int, Bool, String)**: Higher precedence overwrites lower precedence.
*   **Maps (HashMap/Objects)**: Recursively merged. Keys present in higher precedence override keys in lower. New keys are added.
*   **Lists (Arrays)**: Replace by default. (A new list in Project replaces the User list).

### Rust Implementation Pattern
We will split the current `Config` struct into:
1.  `PartialConfig`: A struct where all fields are `Option<T>`, representing a layer that *might* define values.
2.  `ResolvedConfig`: The final struct (similar to current `Config`) where all fields are concrete types.

```rust
// Represents a single layer (User, Project, etc.)
#[derive(Deserialize, Serialize)]
struct PartialConfig {
    theme: Option<String>,
    editor: Option<PartialEditorConfig>,
    // ...
}

// Represents the final merged state used by the editor
struct Config {
    theme: String,
    editor: EditorConfig,
    // ...
}
```

## 3. Format Selection: JSON

We will use standard **JSON** as the configuration format.

*   **Primary Format**: `config.json` (User/Project).
*   **Ecosystem**: Universal support for syntax highlighting, linting, and automated tools.

**Programmatic Edits:**
Since standard JSON does not support comments, we can safely use `serde_json` to serialize the `PartialConfig` layers back to disk when settings are changed via the UI.

## 4. Minimal Persistence

To avoid "setting drift" (where user config accumulates defaults), we implement **Delta Serialization**:

When saving a setting (e.g., changing `tab_size` to 2 in Project scope):
1.  **Calculate Parent Value**: Resolve `System + User`. Say the result is `4`.
2.  **Compare**: The new value `2` differs from `4`.
3.  **Write Delta**: We write `{"editor": {"tab_size": 2}}` to the Project layer file.

If the user sets `tab_size` back to `4` (the parent value):
1.  **Compare**: New value `4` equals parent `4`.
2.  **Prune**: We *remove* the `tab_size` key from the Project layer file, letting it inherit again.

## 5. Migration Strategy

We will use **Sequential Programmatic Migrations** handled at load time.

1.  **Version Field**: Every config file has a `version` field (default 0).
2.  **Migrators**: A chain of functions `fn migrate_v0_to_v1(serde_json::Value) -> serde_json::Value`.
3.  **Process**:
    *   Load raw JSON file.
    *   Apply `v0 -> v1`, `v1 -> v2`, etc., until `CURRENT_VERSION` is reached.
    *   Deserialization into `PartialConfig` happens *after* migration.

## 6. Conditional Configuration Layers

To support advanced scenarios like platform-specific keybindings or language-specific indentation, we introduce **Conditional Layers** that are injected into the merge stack dynamically.

### Platform Overrides
The editor will automatically look for and load platform-specific config files if they exist. These are merged *after* the main User config but *before* the Project config.

*   `config_linux.json`
*   `config_macos.json`
*   `config_windows.json`

**Resolution:** `System -> User -> User(Platform) -> Project -> Session`

### Syntax-Specific Overrides
When a buffer with a specific language ID (e.g., "python") is active, the editor calculates an "Effective Configuration" for that buffer by injecting a language-specific layer.

This layer is derived from the `languages` key in the resolved config.

```json
// config.json
{
  "editor": { "tab_size": 4 },
  "languages": {
    "python": {
      "editor": { "tab_size": 4 } // Explicit language override
    },
    "ruby": {
      "editor": { "tab_size": 2 }
    }
  }
}
```

**Resolution for a Buffer:**
`EffectiveBufferConfig = Merge(GlobalConfig, LanguageConfig)`

## Implementation Plan

1.  **Refactor Config Structs**: Split `Config` into `PartialConfig` (for layers) and `ResolvedConfig`.
2.  **Implement Layer Loading**: Update `config_io.rs` to load `System`, `User`, `Project` independently.
3.  **Implement Merge Logic**: Write a recursive merge function for `PartialConfig`.
4.  **Update Save Logic**: Ensure `save_to_file` calculates the delta against the merged parent layers.
5.  **UI Integration**: Update the Settings UI to modify the appropriate layer.

## Schema Validation
We will continue to use `schemars` to generate JSON Schema. This provides out-of-the-box autocomplete and validation in most modern text editors.