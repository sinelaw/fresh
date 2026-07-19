use crate::config_io::DirectoryContext;
use crate::i18n;
use crate::input::command_registry::CommandRegistry;
use crate::model::filesystem::FileSystem;
use crate::services::signal_handler;
use crate::view::theme;
use fresh_core::api::DirEntry as PluginDirEntry;
use fresh_core::services::{PluginFileStat, PluginFilesystem, PluginServiceBridge};
use std::any::Any;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

/// Shared, live handle to the active window's filesystem backend.
///
/// The editor updates this whenever the active window (and therefore its
/// authority) changes, so plugin file I/O always follows the currently focused
/// session — the local host for a local window, the remote host for an
/// SSH/container window — with no snapshotting and no local fallback.
pub struct ActiveFilesystem(RwLock<Arc<dyn FileSystem + Send + Sync>>);

impl ActiveFilesystem {
    pub fn new(fs: Arc<dyn FileSystem + Send + Sync>) -> Self {
        Self(RwLock::new(fs))
    }

    /// The filesystem backend currently in effect.
    pub fn get(&self) -> Arc<dyn FileSystem + Send + Sync> {
        self.0.read().unwrap().clone()
    }

    /// Point plugin file I/O at a new backend (called on active-window change).
    pub fn set(&self, fs: Arc<dyn FileSystem + Send + Sync>) {
        *self.0.write().unwrap() = fs;
    }
}

/// [`PluginFilesystem`] that routes every operation to the active window's
/// authority filesystem via [`ActiveFilesystem`]. This is the *only* filesystem
/// plugins touch, so their file access follows remote/SSH backends exactly like
/// the editor core does.
pub struct AuthorityPluginFilesystem {
    active: Arc<ActiveFilesystem>,
}

impl AuthorityPluginFilesystem {
    pub fn new(active: Arc<ActiveFilesystem>) -> Self {
        Self { active }
    }

    /// Ensure `path`'s parent directory exists, creating it if necessary.
    /// Returns false only when creation was required and failed.
    fn ensure_parent(fs: &dyn FileSystem, path: &Path) -> bool {
        if let Some(parent) = path.parent() {
            if !parent.as_os_str().is_empty()
                && !fs.exists(parent)
                && fs.create_dir_all(parent).is_err()
            {
                return false;
            }
        }
        true
    }
}

impl PluginFilesystem for AuthorityPluginFilesystem {
    fn read_file(&self, path: &Path) -> Option<Vec<u8>> {
        self.active.get().read_file(path).ok()
    }

    fn write_file(&self, path: &Path, contents: &[u8]) -> bool {
        let fs = self.active.get();
        Self::ensure_parent(fs.as_ref(), path) && fs.write_file(path, contents).is_ok()
    }

    fn exists(&self, path: &Path) -> bool {
        self.active.get().exists(path)
    }

    fn read_dir(&self, path: &Path) -> Vec<PluginDirEntry> {
        match self.active.get().read_dir(path) {
            Ok(entries) => entries
                .into_iter()
                .map(|e| PluginDirEntry {
                    name: e.name.clone(),
                    is_file: e.is_file(),
                    is_dir: e.is_dir(),
                })
                .collect(),
            Err(_) => Vec::new(),
        }
    }

    fn create_dir_all(&self, path: &Path) -> bool {
        let fs = self.active.get();
        fs.is_dir(path).unwrap_or(false) || fs.create_dir_all(path).is_ok()
    }

    fn remove_path(&self, path: &Path) -> bool {
        let fs = self.active.get();
        if fs.is_dir(path).unwrap_or(false) {
            fs.remove_dir_all(path).is_ok()
        } else {
            fs.remove_file(path).is_ok()
        }
    }

    fn rename(&self, from: &Path, to: &Path) -> bool {
        let fs = self.active.get();
        if fs.rename(from, to).is_ok() {
            return true;
        }
        // Same-backend cross-device fallback: copy then remove the source.
        let is_dir = fs.is_dir(from).unwrap_or(false);
        let copied = if is_dir {
            fs.copy_dir_all(from, to).is_ok()
        } else {
            fs.copy(from, to).is_ok()
        };
        if !copied {
            return false;
        }
        if is_dir {
            fs.remove_dir_all(from).is_ok()
        } else {
            fs.remove_file(from).is_ok()
        }
    }

    fn copy(&self, from: &Path, to: &Path) -> bool {
        let fs = self.active.get();
        if fs.is_dir(from).unwrap_or(false) {
            fs.copy_dir_all(from, to).is_ok()
        } else {
            Self::ensure_parent(fs.as_ref(), to) && fs.copy(from, to).is_ok()
        }
    }

    fn stat(&self, path: &Path) -> Option<PluginFileStat> {
        let fs = self.active.get();
        let md = fs.metadata(path).ok()?;
        Some(PluginFileStat {
            is_file: fs.is_file(path).unwrap_or(false),
            is_dir: fs.is_dir(path).unwrap_or(false),
            size: md.size,
            readonly: md.is_readonly,
        })
    }

    fn canonicalize(&self, path: &Path) -> Option<PathBuf> {
        self.active.get().canonicalize(path).ok()
    }
}

pub struct EditorServiceBridge {
    pub command_registry: Arc<RwLock<CommandRegistry>>,
    pub dir_context: DirectoryContext,
    pub theme_cache: Arc<RwLock<std::collections::HashMap<String, serde_json::Value>>>,
    /// The plugin filesystem, routing to the active window's authority.
    pub plugin_fs: Arc<dyn PluginFilesystem>,
}

impl PluginServiceBridge for EditorServiceBridge {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn filesystem(&self) -> Arc<dyn PluginFilesystem> {
        Arc::clone(&self.plugin_fs)
    }

    fn translate(&self, plugin_name: &str, key: &str, args: &HashMap<String, String>) -> String {
        i18n::translate_plugin_string(plugin_name, key, args)
    }

    fn current_locale(&self) -> String {
        i18n::current_locale()
    }

    fn set_js_execution_state(&self, state: String) {
        signal_handler::set_js_execution_state(state);
    }

    fn clear_js_execution_state(&self) {
        signal_handler::clear_js_execution_state();
    }

    fn get_theme_schema(&self) -> serde_json::Value {
        theme::get_theme_schema()
    }

    fn get_builtin_themes(&self) -> serde_json::Value {
        theme::get_builtin_themes()
    }

    fn get_all_themes(&self) -> serde_json::Value {
        let cache = self.theme_cache.read().unwrap();
        let map: serde_json::Map<String, serde_json::Value> =
            cache.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        serde_json::Value::Object(map)
    }

    fn register_plugin_strings(
        &self,
        plugin_name: &str,
        strings: HashMap<String, HashMap<String, String>>,
    ) {
        i18n::register_plugin_strings(plugin_name, strings);
    }

    fn unregister_plugin_strings(&self, plugin_name: &str) {
        i18n::unregister_plugin_strings(plugin_name);
    }

    fn register_command(&self, command: fresh_core::command::Command) {
        // Convert fresh_core::command::Command to crate::input::commands::Command
        use crate::input::commands::{Command as EditorCommand, CommandSource};
        use crate::input::keybindings::{Action, KeyContext};

        let editor_command = EditorCommand {
            name: command.name,
            description: command.description,
            action: Action::PluginAction(command.action_name),
            contexts: vec![KeyContext::Global],
            custom_contexts: command.custom_contexts,
            source: CommandSource::Plugin(command.plugin_name),
            terminal_bypass: command.terminal_bypass,
        };
        self.command_registry
            .read()
            .unwrap()
            .register(editor_command);
    }

    fn unregister_command(&self, name: &str) {
        self.command_registry.read().unwrap().unregister(name);
    }

    fn unregister_commands_by_prefix(&self, prefix: &str) {
        self.command_registry
            .read()
            .unwrap()
            .unregister_by_prefix(prefix);
    }

    fn unregister_commands_by_plugin(&self, plugin_name: &str) {
        self.command_registry
            .read()
            .unwrap()
            .unregister_by_plugin(plugin_name);
    }

    fn plugins_dir(&self) -> PathBuf {
        self.dir_context.plugins_dir()
    }

    fn config_dir(&self) -> PathBuf {
        self.dir_context.config_dir.clone()
    }

    fn data_dir(&self) -> PathBuf {
        self.dir_context.data_dir.clone()
    }

    fn terminal_dir(&self, working_dir: &std::path::Path) -> PathBuf {
        self.dir_context.terminal_dir_for(working_dir)
    }

    fn working_data_dir(&self, working_dir: &std::path::Path) -> PathBuf {
        self.dir_context.working_data_dir_for(working_dir)
    }

    fn get_theme_data(&self, key_or_name: &str) -> Option<serde_json::Value> {
        let cache = self.theme_cache.read().unwrap();
        // Exact key match
        if let Some(v) = cache.get(key_or_name) {
            return Some(v.clone());
        }
        // Fallback: match by theme name inside the cached values
        let normalized = key_or_name.to_lowercase().replace(['_', ' '], "-");
        cache
            .values()
            .find(|v| {
                v.get("name")
                    .and_then(|n| n.as_str())
                    .is_some_and(|n| n.to_lowercase().replace(['_', ' '], "-") == normalized)
            })
            .cloned()
    }

    fn save_theme_file(&self, name: &str, content: &str) -> Result<String, String> {
        let themes_dir = self.dir_context.themes_dir();
        if !themes_dir.exists() {
            std::fs::create_dir_all(&themes_dir).map_err(|e| e.to_string())?;
        }
        let path = themes_dir.join(format!("{}.json", name));
        std::fs::write(&path, content).map_err(|e| e.to_string())?;
        Ok(path.to_string_lossy().to_string())
    }

    fn theme_file_exists(&self, name: &str) -> bool {
        let themes_dir = self.dir_context.themes_dir();
        themes_dir.join(format!("{}.json", name)).exists()
    }
}
