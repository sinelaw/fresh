//! Runtime configuration I/O operations.
//!
//! This module contains system directory detection and config loading utilities
//! that require runtime dependencies (dirs, tracing).
//! These are separated from config.rs to allow schema-only builds.

use crate::config::Config;
use std::path::{Path, PathBuf};

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
