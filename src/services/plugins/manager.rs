//! Unified Plugin Manager
//!
//! This module provides a unified interface for the plugin system that works
//! regardless of whether the `plugins` feature is enabled. When plugins are
//! disabled, all methods are no-ops, avoiding the need for cfg attributes
//! scattered throughout the codebase.

use crate::config::PluginConfig;
use crate::input::command_registry::CommandRegistry;
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, RwLock};

#[cfg(feature = "plugins")]
use super::thread::PluginThreadHandle;

/// Unified plugin manager that abstracts over the plugin system.
///
/// When the `plugins` feature is enabled, this wraps `PluginThreadHandle`.
/// When disabled, all methods are no-ops.
pub struct PluginManager {
    #[cfg(feature = "plugins")]
    inner: Option<PluginThreadHandle>,
    #[cfg(not(feature = "plugins"))]
    _phantom: std::marker::PhantomData<()>,
}

impl PluginManager {
    /// Create a new plugin manager.
    ///
    /// When `plugins` feature is enabled and `enable` is true, spawns the plugin thread.
    /// Otherwise, creates a no-op manager.
    pub fn new(
        enable: bool,
        command_registry: Arc<RwLock<CommandRegistry>>,
        dir_context: crate::config_io::DirectoryContext,
    ) -> Self {
        #[cfg(feature = "plugins")]
        {
            if enable {
                match PluginThreadHandle::spawn(command_registry, dir_context) {
                    Ok(handle) => {
                        return Self {
                            inner: Some(handle),
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
            Self { inner: None }
        }

        #[cfg(not(feature = "plugins"))]
        {
            let _ = command_registry; // Suppress unused warning
            let _ = dir_context; // Suppress unused warning
            if enable {
                tracing::warn!("Plugins requested but compiled without plugin support");
            }
            Self {
                _phantom: std::marker::PhantomData,
            }
        }
    }

    /// Check if the plugin system is active (has a running plugin thread).
    pub fn is_active(&self) -> bool {
        #[cfg(feature = "plugins")]
        {
            self.inner.is_some()
        }
        #[cfg(not(feature = "plugins"))]
        {
            false
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
    pub fn load_plugins_from_dir_with_config(
        &self,
        dir: &Path,
        plugin_configs: &HashMap<String, PluginConfig>,
    ) -> (Vec<String>, HashMap<String, PluginConfig>) {
        #[cfg(feature = "plugins")]
        {
            if let Some(ref manager) = self.inner {
                return manager.load_plugins_from_dir_with_config(dir, plugin_configs);
            }
            (Vec::new(), HashMap::new())
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = (dir, plugin_configs);
            (Vec::new(), HashMap::new())
        }
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
        #[cfg(feature = "plugins")]
        {
            if let Some(ref mut manager) = self.inner {
                return manager.process_commands();
            }
            Vec::new()
        }
        #[cfg(not(feature = "plugins"))]
        {
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
    ) -> Option<anyhow::Result<super::thread::oneshot::Receiver<anyhow::Result<()>>>> {
        self.inner
            .as_ref()
            .map(|m| m.execute_action_async(action_name))
    }

    /// List all loaded plugins.
    #[cfg(feature = "plugins")]
    pub fn list_plugins(&self) -> Vec<super::runtime::TsPluginInfo> {
        self.inner
            .as_ref()
            .map(|m| m.list_plugins())
            .unwrap_or_default()
    }

    /// Reload a plugin by name.
    #[cfg(feature = "plugins")]
    pub fn reload_plugin(&self, name: &str) -> anyhow::Result<()> {
        self.inner
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Plugin system not active"))?
            .reload_plugin(name)
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
}
