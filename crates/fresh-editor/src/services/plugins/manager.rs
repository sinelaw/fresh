//! Unified Plugin Manager
//!
//! This module provides a unified interface for the plugin system that works
//! regardless of whether the `plugins` feature is enabled. When plugins are
//! disabled, all methods are no-ops, avoiding the need for cfg attributes
//! scattered throughout the codebase.

use crate::config_io::DirectoryContext;
use crate::input::command_registry::CommandRegistry;
use fresh_core::config::PluginConfig;
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, RwLock};

#[cfg(feature = "plugins")]
use super::bridge::EditorServiceBridge;
#[cfg(feature = "plugins")]
use fresh_plugin_runtime::PluginThreadHandle;

/// Unified plugin manager that abstracts over the plugin system.
///
/// When the `plugins` feature is enabled, this wraps `PluginThreadHandle`.
/// When disabled, all methods are no-ops.
pub struct PluginManager {
    #[cfg(feature = "plugins")]
    inner: Option<PluginThreadHandle>,
    #[cfg(not(feature = "plugins"))]
    _phantom: std::marker::PhantomData<()>,
    /// Test-only side channel: commands pushed via
    /// [`Self::test_inject_command`] are returned by the next
    /// `process_commands()` call as if they had come from the plugin
    /// thread. Always present (zero overhead — empty `Vec`) so
    /// integration tests in `tests/` can use it without an extra
    /// feature flag.
    pending_injected_commands: Vec<super::api::PluginCommand>,
}

impl PluginManager {
    /// Create a new plugin manager.
    ///
    /// When `plugins` feature is enabled and `enable` is true, spawns the plugin thread.
    /// Otherwise, creates a no-op manager.
    pub fn new(
        enable: bool,
        command_registry: Arc<RwLock<CommandRegistry>>,
        dir_context: DirectoryContext,
        theme_cache: Arc<RwLock<HashMap<String, serde_json::Value>>>,
    ) -> Self {
        #[cfg(feature = "plugins")]
        {
            if enable {
                let services = Arc::new(EditorServiceBridge {
                    command_registry: command_registry.clone(),
                    dir_context,
                    theme_cache,
                });
                match PluginThreadHandle::spawn(services) {
                    Ok(handle) => {
                        return Self {
                            inner: Some(handle),
                            pending_injected_commands: Vec::new(),
                        }
                    }
                    Err(e) => {
                        tracing::error!("Failed to spawn TypeScript plugin thread: {}", e);
                        #[cfg(debug_assertions)]
                        panic!("TypeScript plugin thread creation failed: {}", e);
                    }
                }
            } else {
                tracing::info!("Plugins disabled via --no-plugins flag");
            }
            Self {
                inner: None,
                pending_injected_commands: Vec::new(),
            }
        }

        #[cfg(not(feature = "plugins"))]
        {
            let _ = command_registry; // Suppress unused warning
            let _ = dir_context; // Suppress unused warning
            let _ = theme_cache; // Suppress unused warning
            if enable {
                tracing::warn!("Plugins requested but compiled without plugin support");
            }
            Self {
                _phantom: std::marker::PhantomData,
                pending_injected_commands: Vec::new(),
            }
        }
    }

    /// Inject a [`PluginCommand`](super::api::PluginCommand) into the
    /// manager's pending queue as if it had arrived from the plugin
    /// thread. Returned by the next `process_commands()` call.
    ///
    /// Intended for tests that need to deterministically reproduce
    /// renderer/plugin races (e.g. the mid-render `process_commands`
    /// path in `Editor::render`) without spinning up the real plugin
    /// runtime. Production code should not call this.
    pub fn test_inject_command(&mut self, command: super::api::PluginCommand) {
        self.pending_injected_commands.push(command);
    }

    /// Check if the plugin system is active (has a running plugin thread,
    /// or — in tests — has commands queued via [`Self::test_inject_command`]).
    pub fn is_active(&self) -> bool {
        if !self.pending_injected_commands.is_empty() {
            return true;
        }
        #[cfg(feature = "plugins")]
        {
            self.inner.is_some()
        }
        #[cfg(not(feature = "plugins"))]
        {
            false
        }
    }

    /// Check if the plugin thread is still alive
    pub fn is_alive(&self) -> bool {
        #[cfg(feature = "plugins")]
        {
            self.inner.as_ref().map(|h| h.is_alive()).unwrap_or(false)
        }
        #[cfg(not(feature = "plugins"))]
        {
            false
        }
    }

    /// Check thread health and panic if the plugin thread died due to a panic.
    /// This propagates plugin thread panics to the calling thread.
    /// Call this periodically (e.g., in wait loops) to fail fast on plugin errors.
    pub fn check_thread_health(&mut self) {
        #[cfg(feature = "plugins")]
        {
            if let Some(ref mut handle) = self.inner {
                handle.check_thread_health();
            }
        }
    }

    /// Load plugins from a directory.
    pub fn load_plugins_from_dir(&self, dir: &Path) -> Vec<String> {
        #[cfg(feature = "plugins")]
        {
            if let Some(ref manager) = self.inner {
                return manager.load_plugins_from_dir(dir);
            }
            Vec::new()
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = dir;
            Vec::new()
        }
    }

    /// Load plugins from a directory with config support.
    /// Returns (errors, discovered_plugins) where discovered_plugins is a map of
    /// plugin name -> PluginConfig with paths populated.
    #[cfg(feature = "plugins")]
    pub fn load_plugins_from_dir_with_config(
        &self,
        dir: &Path,
        plugin_configs: &HashMap<String, PluginConfig>,
    ) -> (Vec<String>, HashMap<String, PluginConfig>) {
        if let Some(ref manager) = self.inner {
            return manager.load_plugins_from_dir_with_config(dir, plugin_configs);
        }
        (Vec::new(), HashMap::new())
    }

    /// Load plugins from a directory with config support (no-op when plugins disabled).
    #[cfg(not(feature = "plugins"))]
    pub fn load_plugins_from_dir_with_config(
        &self,
        dir: &Path,
        plugin_configs: &HashMap<String, PluginConfig>,
    ) -> (Vec<String>, HashMap<String, PluginConfig>) {
        let _ = (dir, plugin_configs);
        (Vec::new(), HashMap::new())
    }

    /// Unload a plugin by name.
    pub fn unload_plugin(&self, name: &str) -> anyhow::Result<()> {
        #[cfg(feature = "plugins")]
        {
            self.inner
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("Plugin system not active"))?
                .unload_plugin(name)
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = name;
            Ok(())
        }
    }

    /// Load a single plugin by path.
    pub fn load_plugin(&self, path: &Path) -> anyhow::Result<()> {
        #[cfg(feature = "plugins")]
        {
            self.inner
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("Plugin system not active"))?
                .load_plugin(path)
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = path;
            Ok(())
        }
    }

    /// Load a plugin from source code directly (no file I/O).
    ///
    /// If a plugin with the same name is already loaded, it will be unloaded first
    /// (hot-reload semantics). This is used for "Load Plugin from Buffer".
    pub fn load_plugin_from_source(
        &self,
        source: &str,
        name: &str,
        is_typescript: bool,
    ) -> anyhow::Result<()> {
        #[cfg(feature = "plugins")]
        {
            self.inner
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("Plugin system not active"))?
                .load_plugin_from_source(source, name, is_typescript)
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = (source, name, is_typescript);
            Ok(())
        }
    }

    /// Run a hook (fire-and-forget).
    pub fn run_hook(&self, hook_name: &str, args: super::hooks::HookArgs) {
        #[cfg(feature = "plugins")]
        {
            if let Some(ref manager) = self.inner {
                manager.run_hook(hook_name, args);
            }
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = (hook_name, args);
        }
    }

    /// Deliver a response to a pending async plugin operation.
    pub fn deliver_response(&self, response: super::api::PluginResponse) {
        #[cfg(feature = "plugins")]
        {
            if let Some(ref manager) = self.inner {
                manager.deliver_response(response);
            }
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = response;
        }
    }

    /// Process pending plugin commands (non-blocking).
    pub fn process_commands(&mut self) -> Vec<super::api::PluginCommand> {
        // Drain any test-injected commands first so they appear at the
        // front of the returned batch — matching the order the real
        // plugin thread would have produced if the inject call were a
        // genuine plugin response.
        let mut commands = std::mem::take(&mut self.pending_injected_commands);
        #[cfg(feature = "plugins")]
        {
            if let Some(ref mut manager) = self.inner {
                commands.extend(manager.process_commands());
            }
        }
        commands
    }

    /// Process commands, blocking until `HookCompleted` for the given hook arrives.
    /// See [`PluginThreadHandle::process_commands_until_hook_completed`] for details.
    ///
    // TODO: This method is currently unused (dead code). Either wire it into the
    // render path to synchronously wait for plugin responses (e.g. conceals from
    // lines_changed), or remove it along with PluginThreadHandle's implementation
    // and the HookCompleted sentinel if the non-blocking drain approach is sufficient.
    pub fn process_commands_until_hook_completed(
        &mut self,
        hook_name: &str,
        timeout: std::time::Duration,
    ) -> Vec<super::api::PluginCommand> {
        #[cfg(feature = "plugins")]
        {
            if let Some(ref mut manager) = self.inner {
                return manager.process_commands_until_hook_completed(hook_name, timeout);
            }
            Vec::new()
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = (hook_name, timeout);
            Vec::new()
        }
    }

    /// Get the state snapshot handle for updating editor state.
    #[cfg(feature = "plugins")]
    pub fn state_snapshot_handle(&self) -> Option<Arc<RwLock<super::api::EditorStateSnapshot>>> {
        self.inner.as_ref().map(|m| m.state_snapshot_handle())
    }

    /// Execute a plugin action asynchronously.
    #[cfg(feature = "plugins")]
    pub fn execute_action_async(
        &self,
        action_name: &str,
    ) -> Option<anyhow::Result<fresh_plugin_runtime::thread::oneshot::Receiver<anyhow::Result<()>>>>
    {
        self.inner
            .as_ref()
            .map(|m| m.execute_action_async(action_name))
    }

    /// List all loaded plugins.
    #[cfg(feature = "plugins")]
    pub fn list_plugins(
        &self,
    ) -> Vec<fresh_plugin_runtime::backend::quickjs_backend::TsPluginInfo> {
        self.inner
            .as_ref()
            .map(|m| m.list_plugins())
            .unwrap_or_default()
    }

    /// Collect the isolated-declarations `.d.ts` emit of every loaded
    /// plugin that produced one. Returns `(plugin_name, d_ts_source)`
    /// pairs — callers use this to assemble `plugins.d.ts`.
    ///
    /// Available in all builds: without the `plugins` feature it
    /// returns an empty vec, letting `editor_init` call this
    /// unconditionally.
    pub fn plugin_declarations(&self) -> Vec<(String, String)> {
        #[cfg(feature = "plugins")]
        {
            self.list_plugins()
                .into_iter()
                .filter_map(|info| info.declarations.map(|d| (info.name, d)))
                .collect()
        }
        #[cfg(not(feature = "plugins"))]
        {
            Vec::new()
        }
    }

    /// Reload a plugin by name.
    #[cfg(feature = "plugins")]
    pub fn reload_plugin(&self, name: &str) -> anyhow::Result<()> {
        self.inner
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Plugin system not active"))?
            .reload_plugin(name)
    }

    /// Submit a "load plugins from dir with config" request without
    /// blocking. Returns `None` when the plugin runtime is inactive (no
    /// thread), or when the request couldn't be submitted. Used by the
    /// startup async-load path.
    #[cfg(feature = "plugins")]
    pub fn load_plugins_from_dir_with_config_request(
        &self,
        dir: &Path,
        plugin_configs: &HashMap<String, PluginConfig>,
    ) -> Option<
        fresh_plugin_runtime::thread::oneshot::Receiver<
            fresh_plugin_runtime::thread::PluginsDirLoadResult,
        >,
    > {
        self.inner.as_ref().and_then(|m| {
            m.load_plugins_from_dir_with_config_request(dir, plugin_configs)
                .ok()
        })
    }

    /// Submit a "load plugin from source" request without blocking.
    /// Returns `None` when the plugin runtime is inactive.
    #[cfg(feature = "plugins")]
    pub fn load_plugin_from_source_request(
        &self,
        source: &str,
        name: &str,
        is_typescript: bool,
    ) -> Option<fresh_plugin_runtime::thread::oneshot::Receiver<anyhow::Result<()>>> {
        self.inner.as_ref().and_then(|m| {
            m.load_plugin_from_source_request(source, name, is_typescript)
                .ok()
        })
    }

    /// Submit a "list plugins" request without blocking. Submitted after
    /// a batch of dir-load requests, this guarantees the response covers
    /// every plugin loaded by that batch (FIFO request channel).
    #[cfg(feature = "plugins")]
    pub fn list_plugins_request(
        &self,
    ) -> Option<
        fresh_plugin_runtime::thread::oneshot::Receiver<
            Vec<fresh_plugin_runtime::backend::quickjs_backend::TsPluginInfo>,
        >,
    > {
        self.inner
            .as_ref()
            .and_then(|m| m.list_plugins_request().ok())
    }

    /// Check if any handlers are registered for a hook.
    pub fn has_hook_handlers(&self, hook_name: &str) -> bool {
        #[cfg(feature = "plugins")]
        {
            self.inner
                .as_ref()
                .map(|m| m.has_hook_handlers(hook_name))
                .unwrap_or(false)
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = hook_name;
            false
        }
    }

    /// Resolve an async callback in the plugin runtime
    #[cfg(feature = "plugins")]
    pub fn resolve_callback(&self, callback_id: super::api::JsCallbackId, result_json: String) {
        if let Some(inner) = &self.inner {
            inner.resolve_callback(callback_id, result_json);
        }
    }

    /// Resolve an async callback in the plugin runtime (no-op when plugins disabled)
    #[cfg(not(feature = "plugins"))]
    pub fn resolve_callback(
        &self,
        callback_id: fresh_core::api::JsCallbackId,
        result_json: String,
    ) {
        let _ = (callback_id, result_json);
    }

    /// Reject an async callback in the plugin runtime
    #[cfg(feature = "plugins")]
    pub fn reject_callback(&self, callback_id: super::api::JsCallbackId, error: String) {
        if let Some(inner) = &self.inner {
            inner.reject_callback(callback_id, error);
        }
    }

    /// Reject an async callback in the plugin runtime (no-op when plugins disabled)
    #[cfg(not(feature = "plugins"))]
    pub fn reject_callback(&self, callback_id: fresh_core::api::JsCallbackId, error: String) {
        let _ = (callback_id, error);
    }

    /// Call a streaming callback with partial data (does not consume the callback).
    /// When `done` is true, the JS side cleans up.
    #[cfg(feature = "plugins")]
    pub fn call_streaming_callback(
        &self,
        callback_id: fresh_core::api::JsCallbackId,
        result_json: String,
        done: bool,
    ) {
        if let Some(inner) = &self.inner {
            inner.call_streaming_callback(callback_id, result_json, done);
        }
    }

    /// Call a streaming callback (no-op when plugins disabled)
    #[cfg(not(feature = "plugins"))]
    pub fn call_streaming_callback(
        &self,
        callback_id: fresh_core::api::JsCallbackId,
        result_json: String,
        done: bool,
    ) {
        let _ = (callback_id, result_json, done);
    }
}
