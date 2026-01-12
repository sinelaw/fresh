//! Runtime configuration I/O operations.
//!
//! This module contains system directory detection and config loading utilities
//! that require runtime dependencies (dirs, tracing).
//! These are separated from config.rs to allow schema-only builds.

use crate::config::{Config, ConfigError};
use crate::partial_config::{Merge, PartialConfig, SessionConfig};
use serde_json::Value;
use std::path::{Path, PathBuf};

// ============================================================================
// JSON Utilities
// ============================================================================

/// Recursively strip null values and empty objects from a JSON value.
/// This ensures that config layer files only contain the actual overridden values,
/// not null placeholders for inherited fields.
fn strip_nulls(value: Value) -> Option<Value> {
    match value {
        Value::Null => None,
        Value::Object(map) => {
            let filtered: serde_json::Map<String, Value> = map
                .into_iter()
                .filter_map(|(k, v)| strip_nulls(v).map(|v| (k, v)))
                .collect();
            if filtered.is_empty() {
                None
            } else {
                Some(Value::Object(filtered))
            }
        }
        Value::Array(arr) => {
            let filtered: Vec<Value> = arr.into_iter().filter_map(strip_nulls).collect();
            Some(Value::Array(filtered))
        }
        other => Some(other),
    }
}

/// Recursively strip default values (empty strings, empty arrays) from a JSON value.
/// This ensures that fields with default serde values don't get saved to config files.
fn strip_empty_defaults(value: Value) -> Option<Value> {
    match value {
        Value::Null => None,
        Value::String(s) if s.is_empty() => None,
        Value::Array(arr) if arr.is_empty() => None,
        Value::Object(map) => {
            let filtered: serde_json::Map<String, Value> = map
                .into_iter()
                .filter_map(|(k, v)| strip_empty_defaults(v).map(|v| (k, v)))
                .collect();
            if filtered.is_empty() {
                None
            } else {
                Some(Value::Object(filtered))
            }
        }
        Value::Array(arr) => {
            let filtered: Vec<Value> = arr.into_iter().filter_map(strip_empty_defaults).collect();
            if filtered.is_empty() {
                None
            } else {
                Some(Value::Array(filtered))
            }
        }
        other => Some(other),
    }
}

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
        if let Some(Value::Object(ref mut editor_map)) = map.get_mut("editor") {
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
            Self::System => 0,
            Self::User => 1,
            Self::Project => 2,
            Self::Session => 3,
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
        let parent_partial = self.resolve_up_to_layer(layer)?;

        // Resolve parent to full config and convert back to get all values populated.
        // This ensures proper comparison - both current and parent have all fields set,
        // so the diff will correctly identify only the actual differences.
        let parent = PartialConfig::from(&parent_partial.resolve());

        // Convert current config to partial
        let current = PartialConfig::from(config);

        // Calculate delta - now both are fully populated, so only actual differences are captured
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

        // Write delta to file, stripping null values and empty defaults to keep configs minimal
        let delta_value =
            serde_json::to_value(&delta).map_err(|e| ConfigError::SerializeError(e.to_string()))?;
        let stripped_nulls = strip_nulls(delta_value).unwrap_or(Value::Object(Default::default()));
        let clean_delta =
            strip_empty_defaults(stripped_nulls).unwrap_or(Value::Object(Default::default()));
        let json = serde_json::to_string_pretty(&clean_delta)
            .map_err(|e| ConfigError::SerializeError(e.to_string()))?;
        std::fs::write(&path, json)
            .map_err(|e| ConfigError::IoError(format!("{}: {}", path.display(), e)))?;

        Ok(())
    }

    /// Save a SessionConfig to the session layer file.
    pub fn save_session(&self, session: &SessionConfig) -> Result<(), ConfigError> {
        let path = self.session_config_path();

        // Ensure .fresh directory exists
        if let Some(parent_dir) = path.parent() {
            std::fs::create_dir_all(parent_dir)
                .map_err(|e| ConfigError::IoError(format!("{}: {}", parent_dir.display(), e)))?;
        }

        let json = serde_json::to_string_pretty(session)
            .map_err(|e| ConfigError::SerializeError(e.to_string()))?;
        std::fs::write(&path, json)
            .map_err(|e| ConfigError::IoError(format!("{}: {}", path.display(), e)))?;

        tracing::debug!("Saved session config to {}", path.display());
        Ok(())
    }

    /// Load the session config from disk, or return an empty one if it doesn't exist.
    pub fn load_session(&self) -> Result<SessionConfig, ConfigError> {
        match self.load_session_layer()? {
            Some(partial) => Ok(SessionConfig::from(partial)),
            None => Ok(SessionConfig::new()),
        }
    }

    /// Clear the session config file on editor exit.
    pub fn clear_session(&self) -> Result<(), ConfigError> {
        let path = self.session_config_path();
        if path.exists() {
            std::fs::remove_file(&path)
                .map_err(|e| ConfigError::IoError(format!("{}: {}", path.display(), e)))?;
            tracing::debug!("Cleared session config at {}", path.display());
        }
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

    /// Determine which layer each setting value comes from.
    /// Returns a map of JSON pointer paths to their source layer.
    pub fn get_layer_sources(
        &self,
    ) -> Result<std::collections::HashMap<String, ConfigLayer>, ConfigError> {
        use std::collections::HashMap;

        let mut sources: HashMap<String, ConfigLayer> = HashMap::new();

        // Load each layer and mark which paths come from it
        // Check layers in precedence order (highest first)
        // Session layer takes priority, then Project, then User, then System defaults

        if let Some(session) = self.load_session_layer()? {
            let json = serde_json::to_value(&session).unwrap_or_default();
            collect_paths(&json, "", &mut |path| {
                sources.insert(path, ConfigLayer::Session);
            });
        }

        if let Some(project) = self.load_project_layer()? {
            let json = serde_json::to_value(&project).unwrap_or_default();
            collect_paths(&json, "", &mut |path| {
                sources.entry(path).or_insert(ConfigLayer::Project);
            });
        }

        if let Some(user) = self.load_user_layer()? {
            let json = serde_json::to_value(&user).unwrap_or_default();
            collect_paths(&json, "", &mut |path| {
                sources.entry(path).or_insert(ConfigLayer::User);
            });
        }

        // Any path not in the map comes from System defaults (implicitly)

        Ok(sources)
    }
}

/// Recursively collect all non-null leaf paths in a JSON value.
fn collect_paths<F>(value: &Value, prefix: &str, collector: &mut F)
where
    F: FnMut(String),
{
    match value {
        Value::Object(map) => {
            for (key, val) in map {
                let path = if prefix.is_empty() {
                    format!("/{}", key)
                } else {
                    format!("{}/{}", prefix, key)
                };
                collect_paths(val, &path, collector);
            }
        }
        Value::Null => {} // Skip nulls (unset in partial config)
        _ => {
            // Leaf value - collect this path
            collector(prefix.to_string());
        }
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
                    // Key only in current - include it, but strip empty defaults
                    if let Some(stripped) = strip_empty_defaults(cur_val.clone()) {
                        result.insert(key.clone(), stripped);
                    }
                }
            }

            Value::Object(result)
        }
        // For arrays and primitives, include if different
        _ => {
            // Treat empty string as "not set" - don't include in diff
            if let Value::String(s) = current {
                if s.is_empty() {
                    return Value::Object(serde_json::Map::new()); // No diff
                }
            }
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

    /// Get the history file path for a specific prompt type
    /// This is the generic method used by prompt_histories HashMap.
    /// history_name can be: "search", "replace", "goto_line", "plugin:custom_name", etc.
    pub fn prompt_history_path(&self, history_name: &str) -> std::path::PathBuf {
        // Sanitize the name for filesystem safety (replace : with _)
        let safe_name = history_name.replace(':', "_");
        self.data_dir.join(format!("{}_history.json", safe_name))
    }

    /// Get the search history file path (legacy, calls generic method)
    pub fn search_history_path(&self) -> std::path::PathBuf {
        self.prompt_history_path("search")
    }

    /// Get the replace history file path (legacy, calls generic method)
    pub fn replace_history_path(&self) -> std::path::PathBuf {
        self.prompt_history_path("replace")
    }

    /// Get the goto line history file path (legacy, calls generic method)
    pub fn goto_line_history_path(&self) -> std::path::PathBuf {
        self.prompt_history_path("goto_line")
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

    #[test]
    fn save_and_load_session() {
        let (_temp, resolver) = create_test_resolver();

        let mut session = SessionConfig::new();
        session.set_theme(crate::config::ThemeName::from("dark"));
        session.set_editor_option(|e| e.tab_size = Some(2));

        // Save session
        resolver.save_session(&session).unwrap();

        // Load session
        let loaded = resolver.load_session().unwrap();
        assert_eq!(loaded.theme, Some(crate::config::ThemeName::from("dark")));
        assert_eq!(loaded.editor.as_ref().unwrap().tab_size, Some(2));
    }

    #[test]
    fn clear_session_removes_file() {
        let (_temp, resolver) = create_test_resolver();

        let mut session = SessionConfig::new();
        session.set_theme(crate::config::ThemeName::from("dark"));

        // Save then clear
        resolver.save_session(&session).unwrap();
        assert!(resolver.session_config_path().exists());

        resolver.clear_session().unwrap();
        assert!(!resolver.session_config_path().exists());
    }

    #[test]
    fn load_session_returns_empty_when_no_file() {
        let (_temp, resolver) = create_test_resolver();

        let session = resolver.load_session().unwrap();
        assert!(session.is_empty());
    }

    #[test]
    fn session_affects_resolved_config() {
        let (_temp, resolver) = create_test_resolver();

        // Save a session with tab_size=16
        let mut session = SessionConfig::new();
        session.set_editor_option(|e| e.tab_size = Some(16));
        resolver.save_session(&session).unwrap();

        // Resolve should pick up session value
        let config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 16);
    }

    #[test]
    fn save_to_layer_writes_minimal_delta() {
        let (temp, resolver) = create_test_resolver();

        // Create user config with tab_size=2
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(
            &user_config_path,
            r#"{"editor": {"tab_size": 2, "line_numbers": false}}"#,
        )
        .unwrap();

        // Resolve the full config (inherits user values)
        let mut config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 2);
        assert!(!config.editor.line_numbers);

        // Change only tab_size in the project layer
        config.editor.tab_size = 8;

        // Save to project layer
        resolver
            .save_to_layer(&config, ConfigLayer::Project)
            .unwrap();

        // Read the project config file and verify it contains ONLY the delta
        let project_config_path = resolver.project_config_write_path();
        let content = std::fs::read_to_string(&project_config_path).unwrap();
        let json: serde_json::Value = serde_json::from_str(&content).unwrap();

        // Should only have editor.tab_size = 8, nothing else
        assert_eq!(
            json.get("editor").and_then(|e| e.get("tab_size")),
            Some(&serde_json::json!(8)),
            "Project config should contain tab_size override"
        );

        // Should NOT have line_numbers (inherited from user, not changed)
        assert!(
            json.get("editor")
                .and_then(|e| e.get("line_numbers"))
                .is_none(),
            "Project config should NOT contain line_numbers (it's inherited from user layer)"
        );

        // Should NOT have other editor fields like scroll_offset (system default)
        assert!(
            json.get("editor")
                .and_then(|e| e.get("scroll_offset"))
                .is_none(),
            "Project config should NOT contain scroll_offset (it's a system default)"
        );

        drop(temp);
    }

    #[test]
    fn save_to_layer_removes_inherited_values() {
        let (temp, resolver) = create_test_resolver();

        // Create user config with tab_size=2
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(&user_config_path, r#"{"editor": {"tab_size": 2}}"#).unwrap();

        // Create project config with tab_size=8
        let project_config_path = resolver.project_config_write_path();
        std::fs::create_dir_all(project_config_path.parent().unwrap()).unwrap();
        std::fs::write(&project_config_path, r#"{"editor": {"tab_size": 8}}"#).unwrap();

        // Resolve config
        let mut config = resolver.resolve().unwrap();
        assert_eq!(config.editor.tab_size, 8);

        // Set tab_size back to the user value (2)
        config.editor.tab_size = 2;

        // Save to project layer
        resolver
            .save_to_layer(&config, ConfigLayer::Project)
            .unwrap();

        // Read the project config - tab_size should be removed (same as parent)
        let content = std::fs::read_to_string(&project_config_path).unwrap();
        let json: serde_json::Value = serde_json::from_str(&content).unwrap();

        // Should not have editor.tab_size since it matches the user value
        assert!(
            json.get("editor").and_then(|e| e.get("tab_size")).is_none(),
            "Project config should NOT contain tab_size when it matches user layer"
        );

        drop(temp);
    }

    /// Issue #630 FIX: save_to_layer saves only the delta, defaults are inherited.
    ///
    /// The save_to_layer method correctly:
    /// 1. Saves only settings that differ from defaults
    /// 2. Loads correctly because defaults are applied during resolve()
    ///
    /// This test verifies that modifying a config and saving works correctly.
    #[test]
    fn issue_630_save_to_file_strips_settings_matching_defaults() {
        let (_temp, resolver) = create_test_resolver();

        // Create a config with some non-default settings
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(
            &user_config_path,
            r#"{
                "theme": "dracula",
                "editor": {
                    "tab_size": 2
                }
            }"#,
        )
        .unwrap();

        // Load the config
        let mut config = resolver.resolve().unwrap();
        assert_eq!(config.theme.0, "dracula");
        assert_eq!(config.editor.tab_size, 2);

        // User disables LSP via UI
        if let Some(lsp_config) = config.lsp.get_mut("python") {
            lsp_config.enabled = false;
        }

        // Save using save_to_layer
        resolver.save_to_layer(&config, ConfigLayer::User).unwrap();

        // Read back the saved config file
        let content = std::fs::read_to_string(&user_config_path).unwrap();
        let json: serde_json::Value = serde_json::from_str(&content).unwrap();

        eprintln!(
            "Saved config:\n{}",
            serde_json::to_string_pretty(&json).unwrap()
        );

        // Verify the delta contains what we changed
        assert_eq!(
            json.get("theme").and_then(|v| v.as_str()),
            Some("dracula"),
            "Theme should be saved (differs from default)"
        );
        assert_eq!(
            json.get("editor")
                .and_then(|e| e.get("tab_size"))
                .and_then(|v| v.as_u64()),
            Some(2),
            "tab_size should be saved (differs from default)"
        );
        assert_eq!(
            json.get("lsp")
                .and_then(|l| l.get("python"))
                .and_then(|p| p.get("enabled"))
                .and_then(|v| v.as_bool()),
            Some(false),
            "lsp.python.enabled should be saved (differs from default)"
        );

        // Reload and verify the full config is correct
        let reloaded = resolver.resolve().unwrap();
        assert_eq!(reloaded.theme.0, "dracula");
        assert_eq!(reloaded.editor.tab_size, 2);
        assert!(!reloaded.lsp["python"].enabled);
        // Command should come from defaults
        assert_eq!(reloaded.lsp["python"].command, "pylsp");
    }

    /// Test that toggling LSP enabled/disabled preserves the command field.
    ///
    /// 1. Start with empty config (defaults apply, python has command "pylsp")
    /// 2. Disable python LSP, save
    /// 3. Load, enable python LSP, save
    /// 4. Load and verify command is still the default
    #[test]
    fn toggle_lsp_preserves_command() {
        let (_temp, resolver) = create_test_resolver();
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();

        // Step 1: Empty config - defaults apply (python has command "pylsp")
        std::fs::write(&user_config_path, r#"{}"#).unwrap();

        // Load and verify default command
        let config = resolver.resolve().unwrap();
        let original_command = config.lsp["python"].command.clone();
        assert!(
            !original_command.is_empty(),
            "Default python LSP should have a command"
        );

        // Step 2: Disable python LSP, save
        let mut config = resolver.resolve().unwrap();
        config.lsp.get_mut("python").unwrap().enabled = false;
        resolver.save_to_layer(&config, ConfigLayer::User).unwrap();

        // Verify saved file only has enabled:false, not empty command/args
        let saved_content = std::fs::read_to_string(&user_config_path).unwrap();
        assert!(
            !saved_content.contains(r#""command""#),
            "Saved config should not contain 'command' field. File content: {}",
            saved_content
        );
        assert!(
            !saved_content.contains(r#""args""#),
            "Saved config should not contain 'args' field. File content: {}",
            saved_content
        );

        // Step 3: Load again, enable python LSP, save
        let mut config = resolver.resolve().unwrap();
        assert!(!config.lsp["python"].enabled);
        config.lsp.get_mut("python").unwrap().enabled = true;
        resolver.save_to_layer(&config, ConfigLayer::User).unwrap();

        // Step 4: Load and verify command is still the same
        let config = resolver.resolve().unwrap();
        assert_eq!(
            config.lsp["python"].command, original_command,
            "Command should be preserved after toggling enabled. Got: '{}'",
            config.lsp["python"].command
        );
    }

    /// Issue #631 REPRODUCTION: Config with disabled LSP (no command) should be valid.
    ///
    /// Users write configs like:
    /// ```json
    /// { "lsp": { "python": { "enabled": false } } }
    /// ```
    /// This SHOULD be valid - a disabled LSP doesn't need a command.
    /// But currently it FAILS because `command` is required.
    ///
    /// THIS TEST WILL FAIL until the bug is fixed.
    #[test]
    fn issue_631_disabled_lsp_without_command_should_be_valid() {
        let (_temp, resolver) = create_test_resolver();

        // Create the exact config from issue #631 - disabled LSP without command field
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();
        std::fs::write(
            &user_config_path,
            r#"{
                "lsp": {
                    "json": { "enabled": false },
                    "python": { "enabled": false },
                    "toml": { "enabled": false }
                },
                "theme": "dracula"
            }"#,
        )
        .unwrap();

        // Try to load this config - it SHOULD succeed
        let result = resolver.resolve();

        // THIS ASSERTION FAILS - demonstrating bug #631
        // A disabled LSP config should NOT require a command field
        assert!(
            result.is_ok(),
            "BUG #631: Config with disabled LSP should be valid even without 'command' field. \
             Got parse error: {:?}",
            result.err()
        );

        // Verify the theme was loaded (config parsed correctly)
        let config = result.unwrap();
        assert_eq!(
            config.theme.0, "dracula",
            "Theme should be 'dracula' from config file"
        );
    }

    /// Test that loading a config without command field uses the default command.
    #[test]
    fn loading_lsp_without_command_uses_default() {
        let (_temp, resolver) = create_test_resolver();
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();

        // Write config with rust LSP but no command field
        std::fs::write(
            &user_config_path,
            r#"{ "lsp": { "rust": { "enabled": false } } }"#,
        )
        .unwrap();

        // Load and check that command comes from defaults
        let config = resolver.resolve().unwrap();
        assert_eq!(
            config.lsp["rust"].command, "rust-analyzer",
            "Command should come from defaults when not in file. Got: '{}'",
            config.lsp["rust"].command
        );
        assert!(
            !config.lsp["rust"].enabled,
            "enabled should be false from file"
        );
    }

    /// Test simulating the Settings UI flow:
    /// 1. Load config with defaults
    /// 2. Apply change (toggle enabled) via JSON pointer (like Settings UI does)
    /// 3. Save via save_to_layer
    /// 4. Reload and verify command is preserved
    #[test]
    fn settings_ui_toggle_lsp_preserves_command() {
        let (_temp, resolver) = create_test_resolver();
        let user_config_path = resolver.user_config_path();
        std::fs::create_dir_all(user_config_path.parent().unwrap()).unwrap();

        // Step 1: Start with empty config
        std::fs::write(&user_config_path, r#"{}"#).unwrap();

        // Load resolved config - should have rust with command="rust-analyzer"
        let config = resolver.resolve().unwrap();
        assert_eq!(
            config.lsp["rust"].command, "rust-analyzer",
            "Default rust command should be rust-analyzer"
        );
        assert!(
            config.lsp["rust"].enabled,
            "Default rust enabled should be true"
        );

        // Step 2: Simulate Settings UI applying a change to disable rust LSP
        // (This mimics what SettingsState::apply_changes does)
        let mut config_json = serde_json::to_value(&config).unwrap();
        *config_json
            .pointer_mut("/lsp/rust/enabled")
            .expect("path should exist") = serde_json::json!(false);
        let modified_config: crate::config::Config =
            serde_json::from_value(config_json).expect("should deserialize");

        // Verify command is still present after JSON round-trip
        assert_eq!(
            modified_config.lsp["rust"].command, "rust-analyzer",
            "Command should be preserved after JSON modification"
        );

        // Step 3: Save via save_to_layer
        resolver
            .save_to_layer(&modified_config, ConfigLayer::User)
            .unwrap();

        // Check what was saved to file
        let saved_content = std::fs::read_to_string(&user_config_path).unwrap();
        eprintln!("After disable, file contains:\n{}", saved_content);

        // Note: File may contain extra fields like auto_start, process_limits due to
        // how json_diff handles nested objects. The important thing is that command
        // is NOT in the file (it matches defaults) and reloading works correctly.

        // Step 4: Reload and verify command is preserved
        let reloaded = resolver.resolve().unwrap();
        assert_eq!(
            reloaded.lsp["rust"].command, "rust-analyzer",
            "Command should be preserved after save/reload (disabled). Got: '{}'",
            reloaded.lsp["rust"].command
        );
        assert!(!reloaded.lsp["rust"].enabled, "rust should be disabled");

        // Step 5: Re-enable rust LSP (simulating Settings UI)
        let mut config_json = serde_json::to_value(&reloaded).unwrap();
        *config_json
            .pointer_mut("/lsp/rust/enabled")
            .expect("path should exist") = serde_json::json!(true);
        let modified_config: crate::config::Config =
            serde_json::from_value(config_json).expect("should deserialize");

        // Step 6: Save via save_to_layer
        resolver
            .save_to_layer(&modified_config, ConfigLayer::User)
            .unwrap();

        // Check what was saved to file
        let saved_content = std::fs::read_to_string(&user_config_path).unwrap();
        eprintln!("After re-enable, file contains:\n{}", saved_content);

        // Step 7: Reload and verify command is STILL preserved
        let final_config = resolver.resolve().unwrap();
        assert_eq!(
            final_config.lsp["rust"].command, "rust-analyzer",
            "Command should be preserved after toggle cycle. Got: '{}'",
            final_config.lsp["rust"].command
        );
        assert!(final_config.lsp["rust"].enabled, "rust should be enabled");
    }
}
