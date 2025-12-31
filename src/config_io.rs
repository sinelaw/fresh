//! Runtime configuration I/O operations.
//!
//! This module contains system directory detection and config loading utilities
//! that require runtime dependencies (dirs, tracing).
//! These are separated from config.rs to allow schema-only builds.

use crate::config::{Config, ConfigError};
use crate::partial_config::{Merge, PartialConfig};
use serde_json::Value;
use std::path::{Path, PathBuf};

// ============================================================================
// Configuration Migration System
// ============================================================================

/// Current config schema version.
/// Increment this when making breaking changes to config structure.
pub const CURRENT_CONFIG_VERSION: u32 = 1;

/// Apply all necessary migrations to bring a config JSON to the current version.
pub fn migrate_config(mut value: Value) -> Result<Value, ConfigError> {
    let version = value.get("version").and_then(|v| v.as_u64()).unwrap_or(0) as u32;

    // Apply migrations sequentially
    if version < 1 {
        value = migrate_v0_to_v1(value)?;
    }
    // Future migrations:
    // if version < 2 { value = migrate_v1_to_v2(value)?; }

    Ok(value)
}

/// Migration from v0 (implicit/missing version) to v1.
/// This is the initial migration that establishes the version field.
fn migrate_v0_to_v1(mut value: Value) -> Result<Value, ConfigError> {
    if let Value::Object(ref mut map) = value {
        // Set version to 1
        map.insert("version".to_string(), Value::Number(1.into()));

        // Example: rename camelCase keys to snake_case if they exist
        if let Some(editor) = map.get_mut("editor") {
            if let Value::Object(ref mut editor_map) = editor {
                // tabSize -> tab_size (hypothetical legacy format)
                if let Some(val) = editor_map.remove("tabSize") {
                    editor_map.entry("tab_size").or_insert(val);
                }
                // lineNumbers -> line_numbers
                if let Some(val) = editor_map.remove("lineNumbers") {
                    editor_map.entry("line_numbers").or_insert(val);
                }
            }
        }
    }
    Ok(value)
}

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
    /// Get the precedence level (higher = takes priority)
    pub fn precedence(self) -> u8 {
        match self {
            ConfigLayer::System => 0,
            ConfigLayer::User => 1,
            ConfigLayer::Project => 2,
            ConfigLayer::Session => 3,
        }
    }
}

/// Manages loading and merging of all configuration layers.
///
/// Resolution order: System → User → Project → Session
/// Higher precedence layers override lower precedence layers.
pub struct ConfigResolver {
    dir_context: DirectoryContext,
    working_dir: PathBuf,
}

impl ConfigResolver {
    /// Create a new ConfigResolver for a working directory.
    pub fn new(dir_context: DirectoryContext, working_dir: PathBuf) -> Self {
        Self {
            dir_context,
            working_dir,
        }
    }

    /// Load all layers and merge them into a resolved Config.
    ///
    /// Layers are merged from highest to lowest precedence:
    /// Session > Project > UserPlatform > User > System
    ///
    /// Each layer fills in values missing from higher precedence layers.
    pub fn resolve(&self) -> Result<Config, ConfigError> {
        // Start with highest precedence layer (Session)
        let mut merged = self.load_session_layer()?.unwrap_or_default();

        // Merge in Project layer (fills missing values)
        if let Some(project_partial) = self.load_project_layer()? {
            tracing::debug!("Loaded project config layer");
            merged.merge_from(&project_partial);
        }

        // Merge in User Platform layer (e.g., config_linux.json)
        if let Some(platform_partial) = self.load_user_platform_layer()? {
            tracing::debug!("Loaded user platform config layer");
            merged.merge_from(&platform_partial);
        }

        // Merge in User layer (fills remaining missing values)
        if let Some(user_partial) = self.load_user_layer()? {
            tracing::debug!("Loaded user config layer");
            merged.merge_from(&user_partial);
        }

        // Resolve to concrete Config (applies system defaults for any remaining None values)
        Ok(merged.resolve())
    }

    /// Get the path to user config file.
    pub fn user_config_path(&self) -> PathBuf {
        self.dir_context.config_path()
    }

    /// Get the path to project config file.
    /// Checks new location first (.fresh/config.json), falls back to legacy (config.json).
    pub fn project_config_path(&self) -> PathBuf {
        let new_path = self.working_dir.join(".fresh").join("config.json");
        if new_path.exists() {
            return new_path;
        }
        // Fall back to legacy location for backward compatibility
        let legacy_path = self.working_dir.join("config.json");
        if legacy_path.exists() {
            return legacy_path;
        }
        // Return new path as default for new projects
        new_path
    }

    /// Get the preferred path for writing project config (new location).
    pub fn project_config_write_path(&self) -> PathBuf {
        self.working_dir.join(".fresh").join("config.json")
    }

    /// Get the path to session config file.
    pub fn session_config_path(&self) -> PathBuf {
        self.working_dir.join(".fresh").join("session.json")
    }

    /// Get the platform-specific config filename.
    fn platform_config_filename() -> Option<&'static str> {
        if cfg!(target_os = "linux") {
            Some("config_linux.json")
        } else if cfg!(target_os = "macos") {
            Some("config_macos.json")
        } else if cfg!(target_os = "windows") {
            Some("config_windows.json")
        } else {
            None
        }
    }

    /// Get the path to platform-specific user config file.
    pub fn user_platform_config_path(&self) -> Option<PathBuf> {
        Self::platform_config_filename().map(|filename| self.dir_context.config_dir.join(filename))
    }

    /// Load the user layer from disk.
    pub fn load_user_layer(&self) -> Result<Option<PartialConfig>, ConfigError> {
        self.load_layer_from_path(&self.user_config_path())
    }

    /// Load the platform-specific user layer from disk.
    pub fn load_user_platform_layer(&self) -> Result<Option<PartialConfig>, ConfigError> {
        if let Some(path) = self.user_platform_config_path() {
            self.load_layer_from_path(&path)
        } else {
            Ok(None)
        }
    }

    /// Load the project layer from disk.
    pub fn load_project_layer(&self) -> Result<Option<PartialConfig>, ConfigError> {
        self.load_layer_from_path(&self.project_config_path())
    }

    /// Load the session layer from disk.
    pub fn load_session_layer(&self) -> Result<Option<PartialConfig>, ConfigError> {
        self.load_layer_from_path(&self.session_config_path())
    }

    /// Load a layer from a specific path, applying migrations if needed.
    fn load_layer_from_path(&self, path: &Path) -> Result<Option<PartialConfig>, ConfigError> {
        if !path.exists() {
            return Ok(None);
        }

        let content = std::fs::read_to_string(path)
            .map_err(|e| ConfigError::IoError(format!("{}: {}", path.display(), e)))?;

        // Parse as raw JSON first
        let value: Value = serde_json::from_str(&content)
            .map_err(|e| ConfigError::ParseError(format!("{}: {}", path.display(), e)))?;

        // Apply migrations
        let migrated = migrate_config(value)?;

        // Now deserialize to PartialConfig
        let partial: PartialConfig = serde_json::from_value(migrated)
            .map_err(|e| ConfigError::ParseError(format!("{}: {}", path.display(), e)))?;

        Ok(Some(partial))
    }

    /// Save a config to a specific layer, writing only the delta from parent layers.
    pub fn save_to_layer(&self, config: &Config, layer: ConfigLayer) -> Result<(), ConfigError> {
        if layer == ConfigLayer::System {
            return Err(ConfigError::ValidationError(
                "Cannot write to System layer".to_string(),
            ));
        }

        // Calculate parent config (merge all layers below target)
        let parent = self.resolve_up_to_layer(layer)?;

        // Convert current config to partial
        let current = PartialConfig::from(config);

        // Calculate delta
        let delta = diff_partial_config(&current, &parent);

        // Get path for target layer (use write paths for new configs)
        let path = match layer {
            ConfigLayer::User => self.user_config_path(),
            ConfigLayer::Project => self.project_config_write_path(),
            ConfigLayer::Session => self.session_config_path(),
            ConfigLayer::System => unreachable!(),
        };

        // Ensure parent directory exists
        if let Some(parent_dir) = path.parent() {
            std::fs::create_dir_all(parent_dir)
                .map_err(|e| ConfigError::IoError(format!("{}: {}", parent_dir.display(), e)))?;
        }

        // Write delta to file
        let json = serde_json::to_string_pretty(&delta)
            .map_err(|e| ConfigError::SerializeError(e.to_string()))?;
        std::fs::write(&path, json)
            .map_err(|e| ConfigError::IoError(format!("{}: {}", path.display(), e)))?;

        Ok(())
    }

    /// Resolve config by merging layers below the target layer.
    /// Used to calculate the "parent" config for delta serialization.
    fn resolve_up_to_layer(&self, layer: ConfigLayer) -> Result<PartialConfig, ConfigError> {
        let mut merged = PartialConfig::default();

        // Merge from highest precedence (just below target) to lowest
        // Session layer: parent includes Project + UserPlatform + User
        // Project layer: parent includes UserPlatform + User
        // User layer: parent is empty (system defaults applied during resolve)

        if layer == ConfigLayer::Session {
            // Session's parent is Project + UserPlatform + User
            if let Some(project) = self.load_project_layer()? {
                merged = project;
            }
            if let Some(platform) = self.load_user_platform_layer()? {
                merged.merge_from(&platform);
            }
            if let Some(user) = self.load_user_layer()? {
                merged.merge_from(&user);
            }
        } else if layer == ConfigLayer::Project {
            // Project's parent is UserPlatform + User
            if let Some(platform) = self.load_user_platform_layer()? {
                merged = platform;
            }
            if let Some(user) = self.load_user_layer()? {
                merged.merge_from(&user);
            }
        }
        // User layer's parent is empty (defaults handled during resolve)

        Ok(merged)
    }
}

/// Calculate the delta between a partial config and its parent.
/// Returns a PartialConfig containing only values that differ from parent.
fn diff_partial_config(current: &PartialConfig, parent: &PartialConfig) -> PartialConfig {
    // Convert both to JSON values and diff them
    let current_json = serde_json::to_value(current).unwrap_or_default();
    let parent_json = serde_json::to_value(parent).unwrap_or_default();

    let diff = json_diff(&parent_json, &current_json);

    // Convert diff back to PartialConfig
    serde_json::from_value(diff).unwrap_or_default()
}

impl Config {
    /// Get the system config file paths (without local/working directory).
    ///
    /// On macOS, prioritizes `~/.config/fresh/config.json` if it exists.
    /// Then checks the standard system config directory.
    fn system_config_paths() -> Vec<PathBuf> {
        let mut paths = Vec::with_capacity(2);

        // macOS: Prioritize ~/.config/fresh/config.json
        #[cfg(target_os = "macos")]
        if let Some(home) = dirs::home_dir() {
            let path = home.join(".config").join("fresh").join(Config::FILENAME);
            if path.exists() {
                paths.push(path);
            }
        }

        // Standard system paths (XDG on Linux, AppSupport on macOS, Roaming on Windows)
        if let Some(config_dir) = dirs::config_dir() {
            let path = config_dir.join("fresh").join(Config::FILENAME);
            if !paths.contains(&path) && path.exists() {
                paths.push(path);
            }
        }

        paths
    }

    /// Get all config search paths, checking local (working directory) first.
    ///
    /// Search order:
    /// 1. `{working_dir}/config.json` (project-local config)
    /// 2. System config paths (see `system_config_paths()`)
    ///
    /// Only returns paths that exist on disk.
    fn config_search_paths(working_dir: &Path) -> Vec<PathBuf> {
        let local = Self::local_config_path(working_dir);
        let mut paths = Vec::with_capacity(3);

        if local.exists() {
            paths.push(local);
        }

        paths.extend(Self::system_config_paths());
        paths
    }

    /// Find the first existing config file, checking local directory first.
    ///
    /// Returns `None` if no config file exists anywhere.
    pub fn find_config_path(working_dir: &Path) -> Option<PathBuf> {
        Self::config_search_paths(working_dir).into_iter().next()
    }

    /// Load configuration, checking working directory first, then system paths.
    ///
    /// Falls back to defaults if no config file is found or all fail to load.
    pub fn load_for_working_dir(working_dir: &Path) -> Self {
        for path in Self::config_search_paths(working_dir) {
            match Self::load_from_file(&path) {
                Ok(config) => {
                    tracing::info!("Loaded config from {}", path.display());
                    return config;
                }
                Err(e) => {
                    tracing::warn!(
                        "Failed to load config from {}: {}, trying next option",
                        path.display(),
                        e
                    );
                }
            }
        }
        tracing::debug!("No config file found, using defaults");
        Self::default()
    }

    /// Load configuration using the 4-level layer system.
    ///
    /// Merges layers in precedence order: Session > Project > User > System
    /// Falls back to defaults for any unspecified values.
    pub fn load_with_layers(dir_context: &DirectoryContext, working_dir: &Path) -> Self {
        let resolver = ConfigResolver::new(dir_context.clone(), working_dir.to_path_buf());
        match resolver.resolve() {
            Ok(config) => {
                tracing::info!("Loaded layered config for {}", working_dir.display());
                config
            }
            Err(e) => {
                tracing::warn!("Failed to load layered config: {}, using defaults", e);
                Self::default()
            }
        }
    }

    /// Read the raw user config file content as JSON.
    ///
    /// This returns the sparse user config (only what's in the file, not merged
    /// with defaults). Useful for plugins that need to distinguish between
    /// user-set values and defaults.
    ///
    /// Checks working directory first, then system paths.
    pub fn read_user_config_raw(working_dir: &Path) -> serde_json::Value {
        for path in Self::config_search_paths(working_dir) {
            if let Ok(contents) = std::fs::read_to_string(&path) {
                match serde_json::from_str(&contents) {
                    Ok(value) => return value,
                    Err(e) => {
                        tracing::warn!("Failed to parse config from {}: {}", path.display(), e);
                    }
                }
            }
        }
        serde_json::Value::Object(serde_json::Map::new())
    }

    /// Save configuration to a JSON file, only saving fields that differ from defaults.
    ///
    /// This keeps user config files minimal and clean - only user customizations are saved.
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), ConfigError> {
        let current =
            serde_json::to_value(self).map_err(|e| ConfigError::SerializeError(e.to_string()))?;
        let defaults = serde_json::to_value(Self::default())
            .map_err(|e| ConfigError::SerializeError(e.to_string()))?;

        // Compute diff - only values that differ from defaults
        let diff = json_diff(&defaults, &current);

        let contents = serde_json::to_string_pretty(&diff)
            .map_err(|e| ConfigError::SerializeError(e.to_string()))?;

        std::fs::write(path.as_ref(), contents).map_err(|e| ConfigError::IoError(e.to_string()))?;

        Ok(())
    }
}

/// Compute the difference between two JSON values.
/// Returns only the parts of `current` that differ from `defaults`.
fn json_diff(defaults: &serde_json::Value, current: &serde_json::Value) -> serde_json::Value {
    use serde_json::Value;

    match (defaults, current) {
        // Both are objects - recursively diff
        (Value::Object(def_map), Value::Object(cur_map)) => {
            let mut result = serde_json::Map::new();

            for (key, cur_val) in cur_map {
                if let Some(def_val) = def_map.get(key) {
                    // Key exists in both - recurse
                    let diff = json_diff(def_val, cur_val);
                    // Only include if there's an actual difference
                    if !is_empty_diff(&diff) {
                        result.insert(key.clone(), diff);
                    }
                } else {
                    // Key only in current - include it entirely
                    result.insert(key.clone(), cur_val.clone());
                }
            }

            Value::Object(result)
        }
        // For arrays and primitives, include if different
        _ => {
            if defaults == current {
                Value::Object(serde_json::Map::new()) // Empty object signals "no diff"
            } else {
                current.clone()
            }
        }
    }
}

/// Check if a diff result represents "no changes"
fn is_empty_diff(value: &serde_json::Value) -> bool {
    match value {
        serde_json::Value::Object(map) => map.is_empty(),
        _ => false,
    }
}

/// Directory paths for editor state and configuration
///
/// This struct holds all directory paths that the editor needs.
/// Only the top-level `main` function should use `dirs::*` to construct this;
/// all other code should receive it by construction/parameter passing.
///
/// This design ensures:
/// - Tests can use isolated temp directories
/// - Parallel tests don't interfere with each other
/// - No hidden global state dependencies
#[derive(Debug, Clone)]
pub struct DirectoryContext {
    /// Data directory for persistent state (recovery, sessions, history)
    /// e.g., ~/.local/share/fresh on Linux, ~/Library/Application Support/fresh on macOS
    pub data_dir: std::path::PathBuf,

    /// Config directory for user configuration
    /// e.g., ~/.config/fresh on Linux, ~/Library/Application Support/fresh on macOS
    pub config_dir: std::path::PathBuf,

    /// User's home directory (for file open dialog shortcuts)
    pub home_dir: Option<std::path::PathBuf>,

    /// User's documents directory (for file open dialog shortcuts)
    pub documents_dir: Option<std::path::PathBuf>,

    /// User's downloads directory (for file open dialog shortcuts)
    pub downloads_dir: Option<std::path::PathBuf>,
}

impl DirectoryContext {
    /// Create a DirectoryContext from the system directories
    /// This should ONLY be called from main()
    pub fn from_system() -> std::io::Result<Self> {
        let data_dir = dirs::data_dir()
            .ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "Could not determine data directory",
                )
            })?
            .join("fresh");

        #[allow(unused_mut)] // mut needed on macOS only
        let mut config_dir = dirs::config_dir()
            .ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "Could not determine config directory",
                )
            })?
            .join("fresh");

        // macOS: Prioritize ~/.config/fresh if it exists
        #[cfg(target_os = "macos")]
        if let Some(home) = dirs::home_dir() {
            let xdg_config = home.join(".config").join("fresh");
            if xdg_config.exists() {
                config_dir = xdg_config;
            }
        }

        Ok(Self {
            data_dir,
            config_dir,
            home_dir: dirs::home_dir(),
            documents_dir: dirs::document_dir(),
            downloads_dir: dirs::download_dir(),
        })
    }

    /// Create a DirectoryContext for testing with a temp directory
    /// All paths point to subdirectories within the provided temp_dir
    pub fn for_testing(temp_dir: &std::path::Path) -> Self {
        Self {
            data_dir: temp_dir.join("data"),
            config_dir: temp_dir.join("config"),
            home_dir: Some(temp_dir.join("home")),
            documents_dir: Some(temp_dir.join("documents")),
            downloads_dir: Some(temp_dir.join("downloads")),
        }
    }

    /// Get the recovery directory path
    pub fn recovery_dir(&self) -> std::path::PathBuf {
        self.data_dir.join("recovery")
    }

    /// Get the sessions directory path
    pub fn sessions_dir(&self) -> std::path::PathBuf {
        self.data_dir.join("sessions")
    }

    /// Get the search history file path
    pub fn search_history_path(&self) -> std::path::PathBuf {
        self.data_dir.join("search_history.json")
    }

    /// Get the replace history file path
    pub fn replace_history_path(&self) -> std::path::PathBuf {
        self.data_dir.join("replace_history.json")
    }

    /// Get the terminals root directory
    pub fn terminals_dir(&self) -> std::path::PathBuf {
        self.data_dir.join("terminals")
    }

    /// Get the terminal directory for a specific working directory
    pub fn terminal_dir_for(&self, working_dir: &std::path::Path) -> std::path::PathBuf {
        let encoded = crate::session::encode_path_for_filename(working_dir);
        self.terminals_dir().join(encoded)
    }

    /// Get the config file path
    pub fn config_path(&self) -> std::path::PathBuf {
        self.config_dir.join(Config::FILENAME)
    }

    /// Get the themes directory path
    pub fn themes_dir(&self) -> std::path::PathBuf {
        self.config_dir.join("themes")
    }

    /// Get the grammars directory path
    pub fn grammars_dir(&self) -> std::path::PathBuf {
        self.config_dir.join("grammars")
    }

    /// Get the plugins directory path
    pub fn plugins_dir(&self) -> std::path::PathBuf {
        self.config_dir.join("plugins")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_resolver() -> (TempDir, ConfigResolver) {
        let temp_dir = TempDir::new().unwrap();
        let dir_context = DirectoryContext::for_testing(temp_dir.path());
        let working_dir = temp_dir.path().join("project");
        std::fs::create_dir_all(&working_dir).unwrap();
        let resolver = ConfigResolver::new(dir_context, working_dir);
        (temp_dir, resolver)
    }

    #[test]
    fn resolver_returns_defaults_when_no_config_files() {
        let (_temp, resolver) = create_test_resolver();
        let config = resolver.resolve().unwrap();

        // Should have system defaults
        assert_eq!(config.editor.tab_size, 4);
        assert!(config.editor.line_numbers);
    }

    #[test]
    fn resolver_loads_user_layer() {
        let (temp, resolver) = create_test_resolver();

        // Create user config
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(&user_config_path, r#"{"editor": {"tab_size": 2}}"#).unwrap();

        let config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 2);
        assert!(config.editor.line_numbers); // Still default
        drop(temp);
    }

    #[test]
    fn resolver_project_overrides_user() {
        let (temp, resolver) = create_test_resolver();

        // Create user config with tab_size=2
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(
            &user_config_path,
            r#"{"editor": {"tab_size": 2, "line_numbers": false}}"#,
        )
        .unwrap();

        // Create project config with tab_size=8
        let project_config_path = resolver.project_config_path();
        std::fs::create_dir_all(project_config_path.parent().unwrap()).unwrap();
        std::fs::write(&project_config_path, r#"{"editor": {"tab_size": 8}}"#).unwrap();

        let config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 8); // Project wins
        assert!(!config.editor.line_numbers); // User value preserved
        drop(temp);
    }

    #[test]
    fn resolver_session_overrides_all() {
        let (temp, resolver) = create_test_resolver();

        // Create user config
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(&user_config_path, r#"{"editor": {"tab_size": 2}}"#).unwrap();

        // Create project config
        let project_config_path = resolver.project_config_path();
        std::fs::create_dir_all(project_config_path.parent().unwrap()).unwrap();
        std::fs::write(&project_config_path, r#"{"editor": {"tab_size": 4}}"#).unwrap();

        // Create session config
        let session_config_path = resolver.session_config_path();
        std::fs::write(&session_config_path, r#"{"editor": {"tab_size": 16}}"#).unwrap();

        let config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 16); // Session wins
        drop(temp);
    }

    #[test]
    fn layer_precedence_ordering() {
        assert!(ConfigLayer::Session.precedence() > ConfigLayer::Project.precedence());
        assert!(ConfigLayer::Project.precedence() > ConfigLayer::User.precedence());
        assert!(ConfigLayer::User.precedence() > ConfigLayer::System.precedence());
    }

    #[test]
    fn save_to_system_layer_fails() {
        let (_temp, resolver) = create_test_resolver();
        let config = Config::default();
        let result = resolver.save_to_layer(&config, ConfigLayer::System);
        assert!(result.is_err());
    }

    #[test]
    fn resolver_loads_legacy_project_config() {
        let (temp, resolver) = create_test_resolver();

        // Create legacy project config at {working_dir}/config.json
        let working_dir = temp.path().join("project");
        let legacy_path = working_dir.join("config.json");
        std::fs::write(&legacy_path, r#"{"editor": {"tab_size": 3}}"#).unwrap();

        let config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 3);
        drop(temp);
    }

    #[test]
    fn resolver_prefers_new_config_over_legacy() {
        let (temp, resolver) = create_test_resolver();

        // Create both legacy and new project configs
        let working_dir = temp.path().join("project");

        // Legacy: tab_size=3
        let legacy_path = working_dir.join("config.json");
        std::fs::write(&legacy_path, r#"{"editor": {"tab_size": 3}}"#).unwrap();

        // New: tab_size=5
        let new_path = working_dir.join(".fresh").join("config.json");
        std::fs::create_dir_all(new_path.parent().unwrap()).unwrap();
        std::fs::write(&new_path, r#"{"editor": {"tab_size": 5}}"#).unwrap();

        let config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 5); // New path wins
        drop(temp);
    }

    #[test]
    fn load_with_layers_works() {
        let temp = TempDir::new().unwrap();
        let dir_context = DirectoryContext::for_testing(temp.path());
        let working_dir = temp.path().join("project");
        std::fs::create_dir_all(&working_dir).unwrap();

        // Create user config
        std::fs::create_dir_all(&dir_context.config_dir).unwrap();
        std::fs::write(dir_context.config_path(), r#"{"editor": {"tab_size": 2}}"#).unwrap();

        let config = Config::load_with_layers(&dir_context, &working_dir);
        assert_eq!(config.editor.tab_size, 2);
    }

    #[test]
    fn platform_config_overrides_user() {
        let (temp, resolver) = create_test_resolver();

        // Create user config with tab_size=2
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(&user_config_path, r#"{"editor": {"tab_size": 2}}"#).unwrap();

        // Create platform config with tab_size=6
        if let Some(platform_path) = resolver.user_platform_config_path() {
            std::fs::write(&platform_path, r#"{"editor": {"tab_size": 6}}"#).unwrap();

            let config = resolver.resolve().unwrap();
            assert_eq!(config.editor.tab_size, 6); // Platform overrides user
        }
        drop(temp);
    }

    #[test]
    fn project_overrides_platform() {
        let (temp, resolver) = create_test_resolver();

        // Create user config
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(&user_config_path, r#"{"editor": {"tab_size": 2}}"#).unwrap();

        // Create platform config
        if let Some(platform_path) = resolver.user_platform_config_path() {
            std::fs::write(&platform_path, r#"{"editor": {"tab_size": 6}}"#).unwrap();
        }

        // Create project config with tab_size=10
        let project_config_path = resolver.project_config_path();
        std::fs::create_dir_all(project_config_path.parent().unwrap()).unwrap();
        std::fs::write(&project_config_path, r#"{"editor": {"tab_size": 10}}"#).unwrap();

        let config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 10); // Project overrides platform
        drop(temp);
    }

    #[test]
    fn migration_adds_version() {
        let input = serde_json::json!({
            "editor": {"tab_size": 2}
        });

        let migrated = migrate_config(input).unwrap();

        assert_eq!(migrated.get("version"), Some(&serde_json::json!(1)));
    }

    #[test]
    fn migration_renames_camelcase_keys() {
        let input = serde_json::json!({
            "editor": {
                "tabSize": 8,
                "lineNumbers": false
            }
        });

        let migrated = migrate_config(input).unwrap();

        let editor = migrated.get("editor").unwrap();
        assert_eq!(editor.get("tab_size"), Some(&serde_json::json!(8)));
        assert_eq!(editor.get("line_numbers"), Some(&serde_json::json!(false)));
        assert!(editor.get("tabSize").is_none());
        assert!(editor.get("lineNumbers").is_none());
    }

    #[test]
    fn migration_preserves_existing_snake_case() {
        let input = serde_json::json!({
            "version": 1,
            "editor": {"tab_size": 4}
        });

        let migrated = migrate_config(input).unwrap();

        let editor = migrated.get("editor").unwrap();
        assert_eq!(editor.get("tab_size"), Some(&serde_json::json!(4)));
    }

    #[test]
    fn resolver_loads_legacy_camelcase_config() {
        let (temp, resolver) = create_test_resolver();

        // Create config with legacy camelCase keys
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(
            &user_config_path,
            r#"{"editor": {"tabSize": 3, "lineNumbers": false}}"#,
        )
        .unwrap();

        let config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 3);
        assert!(!config.editor.line_numbers);
        drop(temp);
    }
}
