# Configuration System Implementation Plan

This document provides a step-by-step implementation plan for the 4-Level Overlay Configuration System described in `CONFIG_DESIGN.md`.

## Current State Analysis

### What Already Exists
| Feature | Status | Location |
|---------|--------|----------|
| System defaults (hardcoded) | ✅ Complete | `Config::default()` in `src/config.rs` |
| User config loading | ✅ Partial | `~/.config/fresh/config.json` |
| Project config loading | ✅ Partial | `{working_dir}/config.json` |
| HashMap merging (lsp, languages) | ✅ Complete | `config.rs:987-1007` |
| Version field for migrations | ✅ Field exists | `Config::version: u32` |
| Delta serialization (vs defaults) | ✅ Partial | `config_io.rs:131-173` |
| Language-specific config | ✅ Complete | `Config::languages` HashMap |
| JSON Schema generation | ✅ Complete | `schemars` integration |

### What Needs Implementation
| Feature | Priority | Complexity |
|---------|----------|------------|
| `PartialConfig` structs | P0 | High |
| Deep merge for all nested objects | P0 | Medium |
| Layer-aware loading (all 4 layers) | P0 | Medium |
| Session layer (volatile) | P1 | Medium |
| Delta serialization (vs parent layers) | P1 | Medium |
| Platform-specific config files | P2 | Low |
| Migration chain system | P2 | Medium |
| Settings UI layer selection | P3 | High |
| Buffer-specific effective config | P3 | Medium |

---

## Phase 1: Core Type System Refactoring

### Step 1.1: Create PartialConfig Module
**Files to create/modify:** `src/partial_config.rs` (new), `src/lib.rs`

Create mirror structs where all fields are `Option<T>`:

```rust
// src/partial_config.rs

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents a configuration layer where all fields are optional.
/// Used for User, Project, and Session layers that may only define some values.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialConfig {
    pub version: Option<u32>,
    pub theme: Option<ThemeName>,
    pub check_for_updates: Option<bool>,
    pub editor: Option<PartialEditorConfig>,
    pub file_explorer: Option<PartialFileExplorerConfig>,
    pub file_browser: Option<PartialFileBrowserConfig>,
    pub terminal: Option<PartialTerminalConfig>,
    pub keybindings: Option<Vec<Keybinding>>,
    pub keybinding_maps: Option<HashMap<String, KeymapConfig>>,
    pub active_keybinding_map: Option<KeybindingMapName>,
    pub languages: Option<HashMap<String, PartialLanguageConfig>>,
    pub lsp: Option<HashMap<String, LspServerConfig>>,
    pub menu: Option<MenuConfig>,
    pub warnings: Option<PartialWarningsConfig>,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialEditorConfig {
    pub tab_size: Option<usize>,
    pub auto_indent: Option<bool>,
    pub line_numbers: Option<bool>,
    pub relative_line_numbers: Option<bool>,
    // ... all 27 fields as Option<T>
}

// Similar for all nested config structs...
```

**Structs to create partial versions for:**
1. `PartialConfig` (main)
2. `PartialEditorConfig` (27 fields)
3. `PartialFileExplorerConfig` (7 fields)
4. `PartialFileBrowserConfig` (4 fields)
5. `PartialTerminalConfig` (6 fields)
6. `PartialLanguageConfig` (12 fields)
7. `PartialWarningsConfig` (4 fields)
8. `PartialFormatterConfig` (4 fields)

### Step 1.2: Implement Merge Trait
**Files to modify:** `src/partial_config.rs`

```rust
/// Trait for merging configuration layers.
/// Higher precedence values (self) override lower precedence (other).
pub trait Merge {
    fn merge_from(&mut self, other: &Self);
}

impl<T: Clone> Merge for Option<T> {
    fn merge_from(&mut self, other: &Self) {
        if self.is_none() {
            *self = other.clone();
        }
    }
}

impl Merge for PartialConfig {
    fn merge_from(&mut self, other: &Self) {
        self.version.merge_from(&other.version);
        self.theme.merge_from(&other.theme);
        self.check_for_updates.merge_from(&other.check_for_updates);

        // For nested structs, merge recursively
        match (&mut self.editor, &other.editor) {
            (Some(a), Some(b)) => a.merge_from(b),
            (None, Some(b)) => self.editor = Some(b.clone()),
            _ => {}
        }

        // For HashMaps, merge entries
        if let Some(ref mut self_langs) = self.languages {
            if let Some(ref other_langs) = other.languages {
                for (key, value) in other_langs {
                    self_langs.entry(key.clone()).or_insert_with(|| value.clone());
                }
            }
        } else {
            self.languages = other.languages.clone();
        }

        // ... continue for all fields
    }
}
```

### Step 1.3: Implement Resolution to Concrete Config
**Files to modify:** `src/partial_config.rs`, `src/config.rs`

```rust
impl PartialConfig {
    /// Resolve a partial config to a concrete Config by filling in defaults.
    /// This should only be called after all layers have been merged.
    pub fn resolve(self) -> Config {
        let defaults = Config::default();
        Config {
            version: self.version.unwrap_or(defaults.version),
            theme: self.theme.unwrap_or(defaults.theme),
            check_for_updates: self.check_for_updates.unwrap_or(defaults.check_for_updates),
            editor: self.editor
                .map(|e| e.resolve(&defaults.editor))
                .unwrap_or(defaults.editor),
            // ... all fields
        }
    }
}
```

---

## Phase 2: Layer-Aware Loading System

### Step 2.1: Define ConfigLayer Enum
**Files to modify:** `src/config_io.rs`

```rust
/// Represents a configuration layer in the 4-level hierarchy.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConfigLayer {
    /// Hardcoded defaults embedded in binary (lowest precedence)
    System,
    /// User-global settings (~/.config/fresh/config.json)
    User,
    /// Project-local settings ($PROJECT_ROOT/.fresh/config.json)
    Project,
    /// Runtime/volatile session state (highest precedence)
    Session,
}

impl ConfigLayer {
    pub fn precedence(&self) -> u8 {
        match self {
            ConfigLayer::System => 0,
            ConfigLayer::User => 1,
            ConfigLayer::Project => 2,
            ConfigLayer::Session => 3,
        }
    }
}
```

### Step 2.2: Create ConfigResolver
**Files to create:** `src/config_resolver.rs`

```rust
/// Manages loading and merging of all configuration layers.
pub struct ConfigResolver {
    dir_context: DirectoryContext,
    working_dir: PathBuf,
}

impl ConfigResolver {
    pub fn new(working_dir: PathBuf) -> Self {
        Self {
            dir_context: DirectoryContext::new(),
            working_dir,
        }
    }

    /// Load all layers and merge them into a resolved Config.
    pub fn resolve(&self) -> Result<Config, ConfigError> {
        // Start with empty partial config
        let mut merged = PartialConfig::default();

        // Layer 1: System (hardcoded) - represented as PartialConfig
        // Skip - defaults are applied during resolution

        // Layer 2: User global config
        if let Some(user_partial) = self.load_user_layer()? {
            merged.merge_from(&user_partial);
        }

        // Layer 3: Project local config
        if let Some(project_partial) = self.load_project_layer()? {
            merged.merge_from(&project_partial);
        }

        // Layer 4: Session volatile config
        if let Some(session_partial) = self.load_session_layer()? {
            merged.merge_from(&session_partial);
        }

        // Resolve to concrete Config (applies system defaults for missing values)
        Ok(merged.resolve())
    }

    fn user_config_path(&self) -> PathBuf {
        self.dir_context.config_path()
    }

    fn project_config_path(&self) -> PathBuf {
        self.working_dir.join(".fresh").join("config.json")
    }

    fn session_config_path(&self) -> PathBuf {
        self.working_dir.join(".fresh").join("session.json")
    }

    fn load_user_layer(&self) -> Result<Option<PartialConfig>, ConfigError> {
        let path = self.user_config_path();
        if path.exists() {
            let content = std::fs::read_to_string(&path)?;
            let partial: PartialConfig = serde_json::from_str(&content)?;
            Ok(Some(partial))
        } else {
            Ok(None)
        }
    }

    // Similar for load_project_layer, load_session_layer...
}
```

### Step 2.3: Update Config Loading Entry Points
**Files to modify:** `src/config.rs`, `src/config_io.rs`

Replace current `Config::load_for_working_dir`:

```rust
impl Config {
    /// Load configuration with full 4-layer resolution.
    pub fn load_for_working_dir(working_dir: &Path) -> Result<Self, ConfigError> {
        let resolver = ConfigResolver::new(working_dir.to_path_buf());
        resolver.resolve()
    }

    /// Load from a specific layer only (for debugging/testing).
    pub fn load_layer(layer: ConfigLayer, working_dir: &Path) -> Result<PartialConfig, ConfigError> {
        let resolver = ConfigResolver::new(working_dir.to_path_buf());
        match layer {
            ConfigLayer::System => Ok(PartialConfig::from_defaults()),
            ConfigLayer::User => resolver.load_user_layer(),
            ConfigLayer::Project => resolver.load_project_layer(),
            ConfigLayer::Session => resolver.load_session_layer(),
        }
    }
}
```

---

## Phase 3: Delta Serialization

### Step 3.1: Implement Config Diff
**Files to modify:** `src/config_io.rs`

```rust
impl PartialConfig {
    /// Calculate the delta between this config and a parent config.
    /// Returns a PartialConfig containing only the differences.
    pub fn diff_from(&self, parent: &PartialConfig) -> PartialConfig {
        let mut delta = PartialConfig::default();

        // For each field, only include if different from parent
        if self.theme != parent.theme {
            delta.theme = self.theme.clone();
        }

        if self.check_for_updates != parent.check_for_updates {
            delta.check_for_updates = self.check_for_updates;
        }

        // For nested structs, diff recursively
        if let (Some(self_editor), Some(parent_editor)) = (&self.editor, &parent.editor) {
            let editor_diff = self_editor.diff_from(parent_editor);
            if !editor_diff.is_empty() {
                delta.editor = Some(editor_diff);
            }
        } else if self.editor.is_some() {
            delta.editor = self.editor.clone();
        }

        // ... continue for all fields
        delta
    }

    /// Check if this partial config has any set values.
    pub fn is_empty(&self) -> bool {
        self.theme.is_none()
            && self.check_for_updates.is_none()
            && self.editor.as_ref().map_or(true, |e| e.is_empty())
            // ... all fields
    }
}
```

### Step 3.2: Update Save Logic
**Files to modify:** `src/config_io.rs`

```rust
impl ConfigResolver {
    /// Save a config to a specific layer, writing only the delta from parent layers.
    pub fn save_to_layer(
        &self,
        config: &Config,
        layer: ConfigLayer
    ) -> Result<(), ConfigError> {
        // Calculate parent config (merge all layers below target)
        let parent = self.resolve_up_to_layer(layer)?;

        // Convert current config to partial
        let current = PartialConfig::from_config(config);

        // Calculate delta
        let delta = current.diff_from(&parent);

        // Get path for target layer
        let path = match layer {
            ConfigLayer::User => self.user_config_path(),
            ConfigLayer::Project => self.project_config_path(),
            ConfigLayer::Session => self.session_config_path(),
            ConfigLayer::System => return Err(ConfigError::ReadOnly),
        };

        // Write delta to file
        let json = serde_json::to_string_pretty(&delta)?;
        std::fs::write(&path, json)?;

        Ok(())
    }

    /// Resolve config by merging layers up to (but not including) the target layer.
    fn resolve_up_to_layer(&self, layer: ConfigLayer) -> Result<PartialConfig, ConfigError> {
        let mut merged = PartialConfig::default();

        if layer.precedence() > ConfigLayer::User.precedence() {
            if let Some(user) = self.load_user_layer()? {
                merged.merge_from(&user);
            }
        }

        if layer.precedence() > ConfigLayer::Project.precedence() {
            if let Some(project) = self.load_project_layer()? {
                merged.merge_from(&project);
            }
        }

        Ok(merged)
    }
}
```

---

## Phase 4: Platform-Specific Configuration

### Step 4.1: Add Platform Config Loading
**Files to modify:** `src/config_resolver.rs`

```rust
impl ConfigResolver {
    /// Load platform-specific user config overlay.
    fn load_user_platform_layer(&self) -> Result<Option<PartialConfig>, ConfigError> {
        let platform_file = if cfg!(target_os = "linux") {
            "config_linux.json"
        } else if cfg!(target_os = "macos") {
            "config_macos.json"
        } else if cfg!(target_os = "windows") {
            "config_windows.json"
        } else {
            return Ok(None);
        };

        let path = self.dir_context.config_dir().join(platform_file);
        if path.exists() {
            let content = std::fs::read_to_string(&path)?;
            let partial: PartialConfig = serde_json::from_str(&content)?;
            Ok(Some(partial))
        } else {
            Ok(None)
        }
    }

    /// Updated resolution with platform layer.
    pub fn resolve(&self) -> Result<Config, ConfigError> {
        let mut merged = PartialConfig::default();

        // Layer 2a: User global config
        if let Some(user_partial) = self.load_user_layer()? {
            merged.merge_from(&user_partial);
        }

        // Layer 2b: User platform-specific config (NEW)
        if let Some(platform_partial) = self.load_user_platform_layer()? {
            merged.merge_from(&platform_partial);
        }

        // Layer 3: Project local config
        if let Some(project_partial) = self.load_project_layer()? {
            merged.merge_from(&project_partial);
        }

        // Layer 4: Session volatile config
        if let Some(session_partial) = self.load_session_layer()? {
            merged.merge_from(&session_partial);
        }

        Ok(merged.resolve())
    }
}
```

---

## Phase 5: Migration System

### Step 5.1: Create Migration Framework
**Files to create:** `src/config_migration.rs`

```rust
use serde_json::Value;

/// Current config schema version
pub const CURRENT_VERSION: u32 = 1;

/// Type alias for migration functions
type MigrationFn = fn(Value) -> Result<Value, ConfigError>;

/// Get the chain of migrations from a version to current.
fn get_migrations() -> Vec<(u32, MigrationFn)> {
    vec![
        (0, migrate_v0_to_v1),
        // Future: (1, migrate_v1_to_v2),
    ]
}

/// Apply all necessary migrations to bring config to current version.
pub fn migrate_to_current(mut value: Value) -> Result<Value, ConfigError> {
    let version = value.get("version")
        .and_then(|v| v.as_u64())
        .unwrap_or(0) as u32;

    for (from_version, migration_fn) in get_migrations() {
        if version <= from_version {
            value = migration_fn(value)?;
        }
    }

    // Update version field
    if let Value::Object(ref mut map) = value {
        map.insert("version".to_string(), Value::Number(CURRENT_VERSION.into()));
    }

    Ok(value)
}

/// Migration from v0 (implicit) to v1.
fn migrate_v0_to_v1(mut value: Value) -> Result<Value, ConfigError> {
    // Example migration: rename "tabSize" to "tab_size" if exists
    if let Value::Object(ref mut map) = value {
        if let Some(editor) = map.get_mut("editor") {
            if let Value::Object(ref mut editor_map) = editor {
                if let Some(tab_size) = editor_map.remove("tabSize") {
                    editor_map.insert("tab_size".to_string(), tab_size);
                }
            }
        }
    }
    Ok(value)
}
```

### Step 5.2: Integrate Migration into Loading
**Files to modify:** `src/config_resolver.rs`

```rust
use crate::config_migration::migrate_to_current;

impl ConfigResolver {
    fn load_and_migrate(&self, path: &Path) -> Result<Option<PartialConfig>, ConfigError> {
        if !path.exists() {
            return Ok(None);
        }

        let content = std::fs::read_to_string(path)?;

        // Parse as raw JSON first
        let mut value: serde_json::Value = serde_json::from_str(&content)?;

        // Apply migrations
        value = migrate_to_current(value)?;

        // Now deserialize to PartialConfig
        let partial: PartialConfig = serde_json::from_value(value)?;
        Ok(Some(partial))
    }
}
```

---

## Phase 6: Session Layer Implementation

### Step 6.1: Define Session-Specific Fields
**Files to modify:** `src/partial_config.rs`

```rust
/// Session-specific configuration (volatile, not persisted across editor restarts).
/// This is a subset of PartialConfig used for runtime overrides.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct SessionConfig {
    /// Currently active theme (may differ from persisted preference)
    pub theme: Option<ThemeName>,

    /// Temporary editor overrides
    pub editor: Option<PartialEditorConfig>,

    /// Buffer-specific overrides (keyed by file path)
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub buffer_overrides: HashMap<PathBuf, PartialEditorConfig>,
}

impl SessionConfig {
    pub fn to_partial_config(&self) -> PartialConfig {
        PartialConfig {
            theme: self.theme.clone(),
            editor: self.editor.clone(),
            ..Default::default()
        }
    }
}
```

### Step 6.2: Session Persistence (Optional)
**Files to modify:** `src/config_resolver.rs`

```rust
impl ConfigResolver {
    /// Save session state to .fresh/session.json
    pub fn save_session(&self, session: &SessionConfig) -> Result<(), ConfigError> {
        let path = self.session_config_path();

        // Ensure .fresh directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let json = serde_json::to_string_pretty(session)?;
        std::fs::write(&path, json)?;
        Ok(())
    }

    /// Clear session config on editor exit (optional cleanup)
    pub fn clear_session(&self) -> Result<(), ConfigError> {
        let path = self.session_config_path();
        if path.exists() {
            std::fs::remove_file(&path)?;
        }
        Ok(())
    }
}
```

---

## Phase 7: Settings UI Integration

### Step 7.1: Add Layer Selection to Settings UI
**Files to modify:** `src/view/settings/state.rs`

```rust
pub struct SettingsState {
    // Existing fields...

    /// Which layer the user is currently editing
    pub target_layer: ConfigLayer,

    /// Loaded partial configs for each layer (for display/comparison)
    pub layer_configs: HashMap<ConfigLayer, PartialConfig>,
}

impl SettingsState {
    pub fn set_target_layer(&mut self, layer: ConfigLayer) {
        self.target_layer = layer;
        // Reload pending changes for the new layer
        self.pending_changes.clear();
    }

    /// Check if a setting is overridden in a specific layer
    pub fn is_overridden_in_layer(&self, path: &str, layer: ConfigLayer) -> bool {
        if let Some(partial) = self.layer_configs.get(&layer) {
            partial.has_value_at_path(path)
        } else {
            false
        }
    }

    /// Get the effective value and which layer it comes from
    pub fn get_value_source(&self, path: &str) -> (serde_json::Value, ConfigLayer) {
        // Walk layers from highest to lowest precedence
        for layer in [ConfigLayer::Session, ConfigLayer::Project, ConfigLayer::User, ConfigLayer::System] {
            if let Some(partial) = self.layer_configs.get(&layer) {
                if let Some(value) = partial.get_value_at_path(path) {
                    return (value, layer);
                }
            }
        }
        // Fall back to system default
        (Config::default().get_value_at_path(path), ConfigLayer::System)
    }
}
```

### Step 7.2: Update Save Logic in Settings Actions
**Files to modify:** `src/app/settings_actions.rs`

```rust
pub fn save_settings(editor: &mut Editor) -> Result<(), ConfigError> {
    let settings = editor.settings_state_mut();
    let target_layer = settings.target_layer;

    // Apply pending changes to get new config
    let new_config = settings.apply_changes(&editor.config)?;

    // Save to the target layer using delta serialization
    let resolver = ConfigResolver::new(editor.working_dir().to_path_buf());
    resolver.save_to_layer(&new_config, target_layer)?;

    // Update in-memory config
    editor.set_config(new_config);

    Ok(())
}
```

---

## Phase 8: Buffer-Specific Effective Config

### Step 8.1: Create BufferConfig Resolution
**Files to modify:** `src/config.rs` or new `src/buffer_config.rs`

```rust
/// Resolved configuration for a specific buffer, including language overrides.
pub struct BufferConfig {
    pub tab_size: usize,
    pub use_tabs: bool,
    pub auto_indent: bool,
    pub show_whitespace_tabs: bool,
    pub formatter: Option<FormatterConfig>,
    pub on_save: Vec<OnSaveAction>,
    // ... other buffer-relevant settings
}

impl BufferConfig {
    /// Resolve effective config for a buffer given its language.
    pub fn resolve(global_config: &Config, language_id: Option<&str>) -> Self {
        let editor = &global_config.editor;

        // Start with global editor settings
        let mut config = BufferConfig {
            tab_size: editor.tab_size,
            use_tabs: false, // global default
            auto_indent: editor.auto_indent,
            show_whitespace_tabs: editor.show_whitespace_tabs,
            formatter: None,
            on_save: Vec::new(),
        };

        // Apply language-specific overrides
        if let Some(lang_id) = language_id {
            if let Some(lang_config) = global_config.languages.get(lang_id) {
                if let Some(tab_size) = lang_config.tab_size {
                    config.tab_size = tab_size;
                }
                if let Some(use_tabs) = lang_config.use_tabs {
                    config.use_tabs = use_tabs;
                }
                if let Some(auto_indent) = lang_config.auto_indent {
                    config.auto_indent = auto_indent;
                }
                if let Some(show_tabs) = lang_config.show_whitespace_tabs {
                    config.show_whitespace_tabs = show_tabs;
                }
                config.formatter = lang_config.formatter.clone();
                config.on_save = lang_config.on_save.clone();
            }
        }

        config
    }
}
```

---

## Phase 9: Testing Strategy

### Step 9.1: Unit Tests for Merge Logic
**Files to create/modify:** `src/partial_config.rs` (tests module)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_merge_scalars_higher_precedence_wins() {
        let mut higher = PartialConfig {
            theme: Some(ThemeName::Dark),
            ..Default::default()
        };
        let lower = PartialConfig {
            theme: Some(ThemeName::Light),
            check_for_updates: Some(true),
            ..Default::default()
        };

        higher.merge_from(&lower);

        assert_eq!(higher.theme, Some(ThemeName::Dark)); // Higher wins
        assert_eq!(higher.check_for_updates, Some(true)); // Filled from lower
    }

    #[test]
    fn test_merge_nested_structs_recursive() {
        let mut higher = PartialConfig {
            editor: Some(PartialEditorConfig {
                tab_size: Some(2),
                ..Default::default()
            }),
            ..Default::default()
        };
        let lower = PartialConfig {
            editor: Some(PartialEditorConfig {
                tab_size: Some(4),
                line_numbers: Some(true),
                ..Default::default()
            }),
            ..Default::default()
        };

        higher.merge_from(&lower);

        let editor = higher.editor.unwrap();
        assert_eq!(editor.tab_size, Some(2)); // Higher wins
        assert_eq!(editor.line_numbers, Some(true)); // Filled from lower
    }

    #[test]
    fn test_merge_hashmaps_combine_entries() {
        let mut higher = PartialConfig {
            languages: Some(hashmap! {
                "rust".to_string() => PartialLanguageConfig {
                    tab_size: Some(4),
                    ..Default::default()
                }
            }),
            ..Default::default()
        };
        let lower = PartialConfig {
            languages: Some(hashmap! {
                "python".to_string() => PartialLanguageConfig {
                    tab_size: Some(4),
                    ..Default::default()
                }
            }),
            ..Default::default()
        };

        higher.merge_from(&lower);

        let langs = higher.languages.unwrap();
        assert!(langs.contains_key("rust"));
        assert!(langs.contains_key("python"));
    }

    #[test]
    fn test_resolution_fills_defaults() {
        let partial = PartialConfig {
            theme: Some(ThemeName::Dark),
            // Everything else None
            ..Default::default()
        };

        let resolved = partial.resolve();

        assert_eq!(resolved.theme, ThemeName::Dark);
        assert_eq!(resolved.editor.tab_size, 4); // Default
        assert!(resolved.editor.line_numbers); // Default true
    }
}
```

### Step 9.2: Integration Tests for Layer Loading
**Files to create:** `tests/e2e/config_layers.rs`

```rust
#[test]
fn test_project_config_overrides_user_config() {
    // Setup: Create user config with theme=light
    // Setup: Create project config with theme=dark
    // Load config
    // Assert: theme is dark (project wins)
}

#[test]
fn test_missing_project_config_falls_back_to_user() {
    // Setup: Create user config with theme=light
    // Setup: No project config
    // Load config
    // Assert: theme is light (user value used)
}

#[test]
fn test_delta_serialization_only_writes_differences() {
    // Setup: User config has tab_size=4
    // Modify: Set tab_size=2 in project scope
    // Save to project layer
    // Assert: Project file contains only {"editor": {"tab_size": 2}}
}

#[test]
fn test_platform_specific_config_loaded() {
    // Setup: Create config_linux.json with specific theme
    // Load config on Linux
    // Assert: Platform theme is applied
}
```

---

## Implementation Order Summary

| Step | Description | Estimated Files | Dependencies |
|------|-------------|-----------------|--------------|
| 1.1 | Create PartialConfig structs | 1 new file | None |
| 1.2 | Implement Merge trait | Same file | 1.1 |
| 1.3 | Implement resolve() | Same file + config.rs | 1.2 |
| 2.1 | Define ConfigLayer enum | config_io.rs | None |
| 2.2 | Create ConfigResolver | 1 new file | 1.3, 2.1 |
| 2.3 | Update loading entry points | config.rs, config_io.rs | 2.2 |
| 3.1 | Implement diff_from | partial_config.rs | 1.1 |
| 3.2 | Update save logic | config_io.rs | 3.1, 2.2 |
| 4.1 | Platform config loading | config_resolver.rs | 2.2 |
| 5.1 | Migration framework | 1 new file | None |
| 5.2 | Integrate migration | config_resolver.rs | 5.1, 2.2 |
| 6.1 | Session config fields | partial_config.rs | 1.1 |
| 6.2 | Session persistence | config_resolver.rs | 6.1, 2.2 |
| 7.1 | Settings UI layer selection | view/settings/state.rs | 2.2 |
| 7.2 | Update settings save | app/settings_actions.rs | 7.1, 3.2 |
| 8.1 | BufferConfig resolution | config.rs or new file | 1.3 |
| 9.1 | Unit tests | partial_config.rs | All phase 1-3 |
| 9.2 | Integration tests | tests/e2e/ | All phases |

---

## Risk Mitigation

### Backward Compatibility
- Keep existing `Config` struct API stable
- Add new methods alongside existing ones during transition
- Existing config files will be loaded via migration system

### Performance
- Cache resolved config; only re-resolve on layer file change
- Use file watchers for hot-reload instead of polling
- Lazy-load platform and session layers

### Testing
- Run full test suite after each phase
- Create snapshot tests for config file format
- Test migration path from current config files

---

## Success Criteria

1. **All existing tests pass** after refactoring
2. **4-layer hierarchy** works correctly with proper precedence
3. **Deep merge** correctly handles nested objects and HashMaps
4. **Delta serialization** produces minimal config files
5. **Settings UI** allows editing specific layers
6. **Migration** handles version 0 → 1 transition smoothly
7. **Platform configs** are loaded and applied correctly
8. **Session layer** provides runtime override capability
