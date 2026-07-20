use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Filesystem operations exposed to plugins.
///
/// Plugins must perform *all* file I/O through this handle rather than touching
/// `std::fs` directly, so their reads and writes follow the active window's
/// authority — the local host for a local session, the remote host for an
/// SSH/container session. There is no local fallback: an implementation routes
/// every call to exactly one filesystem backend and nothing else.
pub trait PluginFilesystem: Send + Sync {
    /// Read a file's raw bytes. `None` if it can't be read.
    fn read_file(&self, path: &Path) -> Option<Vec<u8>>;
    /// Write bytes to a file, creating parent directories as needed. Returns
    /// whether the write succeeded.
    fn write_file(&self, path: &Path, contents: &[u8]) -> bool;
    /// Whether a path exists.
    fn exists(&self, path: &Path) -> bool;
    /// List a directory's entries (empty on error).
    fn read_dir(&self, path: &Path) -> Vec<crate::api::DirEntry>;
    /// Create a directory and all parents. Returns whether it exists afterwards.
    fn create_dir_all(&self, path: &Path) -> bool;
    /// Remove a file or directory (recursively for directories).
    fn remove_path(&self, path: &Path) -> bool;
    /// Rename/move a path. Returns whether it succeeded.
    fn rename(&self, from: &Path, to: &Path) -> bool;
    /// Copy a file or directory (recursively for directories).
    fn copy(&self, from: &Path, to: &Path) -> bool;
    /// Stat a path.
    fn stat(&self, path: &Path) -> Option<PluginFileStat>;
    /// Canonicalize a path (resolve symlinks / `..`). `None` if it can't be
    /// resolved (e.g. the path does not exist on the backend).
    fn canonicalize(&self, path: &Path) -> Option<PathBuf>;
}

/// Metadata about a path, as surfaced to plugins via `fileStat`.
#[derive(Debug, Clone, serde::Serialize)]
pub struct PluginFileStat {
    /// Whether the path is a regular file.
    pub is_file: bool,
    /// Whether the path is a directory.
    pub is_dir: bool,
    /// Size in bytes.
    pub size: u64,
    /// Whether the path is read-only.
    pub readonly: bool,
}

/// A [`PluginFilesystem`] that does nothing — every read fails and every
/// mutation is a no-op. Used by the no-op service bridge in headless/test
/// contexts. It never touches any real filesystem.
pub struct NoopPluginFilesystem;

impl PluginFilesystem for NoopPluginFilesystem {
    fn read_file(&self, _path: &Path) -> Option<Vec<u8>> {
        None
    }
    fn write_file(&self, _path: &Path, _contents: &[u8]) -> bool {
        false
    }
    fn exists(&self, _path: &Path) -> bool {
        false
    }
    fn read_dir(&self, _path: &Path) -> Vec<crate::api::DirEntry> {
        Vec::new()
    }
    fn create_dir_all(&self, _path: &Path) -> bool {
        false
    }
    fn remove_path(&self, _path: &Path) -> bool {
        false
    }
    fn rename(&self, _from: &Path, _to: &Path) -> bool {
        false
    }
    fn copy(&self, _from: &Path, _to: &Path) -> bool {
        false
    }
    fn stat(&self, _path: &Path) -> Option<PluginFileStat> {
        None
    }
    fn canonicalize(&self, _path: &Path) -> Option<PathBuf> {
        None
    }
}

/// Trait for the editor to provide services to the plugin runtime
/// without the runtime depending directly on UI or complex system logic.
pub trait PluginServiceBridge: Send + Sync + 'static {
    /// Support downcasting for tests
    fn as_any(&self) -> &dyn std::any::Any;

    /// The filesystem plugins use for authority-scoped file I/O — a window's
    /// backend (local for a local window, remote for an SSH/container window).
    /// `window` selects which window; `None` means the active window (where a
    /// bare string path resolves). A `None` return of an operation on a window
    /// that no longer exists is treated as a failed op, never a silent fallback.
    fn authority_filesystem(&self, window: Option<u64>) -> Arc<dyn PluginFilesystem>;

    /// The filesystem plugins use for explicitly local file I/O — always the
    /// editor host, regardless of the active authority. This is where a
    /// `LocalPath` (built via `editor.localPath(...)`) resolves; the package
    /// manager and other plugins that persist editor-owned state under the
    /// config/data dirs use it so their files stay on the host during remote
    /// sessions.
    fn local_filesystem(&self) -> Arc<dyn PluginFilesystem>;

    /// Translate a string for a plugin
    fn translate(&self, plugin_name: &str, key: &str, args: &HashMap<String, String>) -> String;

    /// Get the current locale
    fn current_locale(&self) -> String;

    /// Update the current JavaScript execution state (for debugging/signal handlers)
    fn set_js_execution_state(&self, state: String);

    /// Clear the JavaScript execution state
    fn clear_js_execution_state(&self);

    /// Get the JSON schema for themes
    fn get_theme_schema(&self) -> serde_json::Value;

    /// Get a list of builtin theme names
    fn get_builtin_themes(&self) -> serde_json::Value;

    /// Full theme registry (builtins + user + packages + bundles) as a JSON
    /// object keyed by canonical registry key. Each value is the parsed theme
    /// with `_key` / `_pack` metadata fields (see `ThemeRegistry::to_json_map`).
    fn get_all_themes(&self) -> serde_json::Value;

    /// Register custom i18n strings for a plugin
    fn register_plugin_strings(
        &self,
        _plugin_name: &str,
        _strings: HashMap<String, HashMap<String, String>>,
    ) {
    }

    /// Unregister custom i18n strings for a plugin
    fn unregister_plugin_strings(&self, _plugin_name: &str) {}

    /// Register a plugin command
    fn register_command(&self, command: crate::command::Command);

    /// Unregister a command by name
    fn unregister_command(&self, name: &str);

    /// Unregister all commands with a given prefix
    fn unregister_commands_by_prefix(&self, prefix: &str);

    /// Unregister all commands registered by a specific plugin
    fn unregister_commands_by_plugin(&self, plugin_name: &str);

    /// Get the plugins directory path
    fn plugins_dir(&self) -> std::path::PathBuf;

    /// Get the config directory path
    fn config_dir(&self) -> std::path::PathBuf;

    /// Get the persistent data directory path (DirectoryContext::data_dir).
    /// Used for long-lived plugin state such as review-diff comment history.
    fn data_dir(&self) -> std::path::PathBuf;

    /// Directory holding terminal scrollback backing files for the given
    /// working directory (project root / worktree). Each root gets its own
    /// subdir, so Universal Search's terminal scope can stay scoped to the
    /// active project. Default falls back to the shared `terminals` root
    /// (covers all roots); the editor bridge overrides with the per-root
    /// subdir (`DirectoryContext::terminal_dir_for`).
    fn terminal_dir(&self, working_dir: &std::path::Path) -> std::path::PathBuf {
        let _ = working_dir;
        self.data_dir().join("terminals")
    }

    /// Per-working-directory data root for plugin state that should be scoped
    /// to a single project root / worktree (e.g. `<data_dir>/workdirs/
    /// <encoded-cwd>/`). Default falls back to the shared parent; the editor
    /// bridge overrides with the per-root subdir
    /// (`DirectoryContext::working_data_dir_for`).
    fn working_data_dir(&self, working_dir: &std::path::Path) -> std::path::PathBuf {
        let _ = working_dir;
        self.data_dir().join("workdirs")
    }

    /// Get theme data (JSON) by name from the in-memory cache.
    fn get_theme_data(&self, _name: &str) -> Option<serde_json::Value> {
        None
    }

    /// Save a theme file to the user themes directory.
    /// Returns the path where the file was written.
    fn save_theme_file(&self, _name: &str, _content: &str) -> Result<String, String> {
        Err("not implemented".to_string())
    }

    /// Check if a user theme file exists (for overwrite confirmation).
    fn theme_file_exists(&self, _name: &str) -> bool {
        false
    }
}

/// A no-op implementation of the service bridge for testing
pub struct NoopServiceBridge;

impl PluginServiceBridge for NoopServiceBridge {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn authority_filesystem(&self, _window: Option<u64>) -> Arc<dyn PluginFilesystem> {
        Arc::new(NoopPluginFilesystem)
    }
    fn local_filesystem(&self) -> Arc<dyn PluginFilesystem> {
        Arc::new(NoopPluginFilesystem)
    }
    fn translate(&self, _plugin_name: &str, key: &str, _args: &HashMap<String, String>) -> String {
        key.to_string()
    }
    fn current_locale(&self) -> String {
        "en".to_string()
    }
    fn set_js_execution_state(&self, _state: String) {}
    fn clear_js_execution_state(&self) {}
    fn get_theme_schema(&self) -> serde_json::Value {
        serde_json::Value::Null
    }
    fn get_builtin_themes(&self) -> serde_json::Value {
        serde_json::Value::Null
    }
    fn get_all_themes(&self) -> serde_json::Value {
        serde_json::Value::Null
    }
    fn register_plugin_strings(
        &self,
        _plugin_name: &str,
        _strings: HashMap<String, HashMap<String, String>>,
    ) {
    }
    fn unregister_plugin_strings(&self, _plugin_name: &str) {}
    fn register_command(&self, _command: crate::command::Command) {}
    fn unregister_command(&self, _name: &str) {}
    fn unregister_commands_by_prefix(&self, _prefix: &str) {}
    fn unregister_commands_by_plugin(&self, _plugin_name: &str) {}
    fn plugins_dir(&self) -> std::path::PathBuf {
        std::path::PathBuf::from("/tmp/plugins")
    }
    fn config_dir(&self) -> std::path::PathBuf {
        std::path::PathBuf::from("/tmp/config")
    }
    fn data_dir(&self) -> std::path::PathBuf {
        std::path::PathBuf::from("/tmp/data")
    }
}
