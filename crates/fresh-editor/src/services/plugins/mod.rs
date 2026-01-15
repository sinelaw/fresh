//! Plugin system
//!
//! The plugin system provides TypeScript/JavaScript plugin support using QuickJS + oxc.
//! When the `plugins` feature is disabled, only the type definitions (api, hooks, event_hooks)
//! are available - the actual runtime is excluded.
//!
//! Use `PluginManager` as the main interface - it handles both enabled and disabled cases.

pub mod api {
    pub use fresh_core::api::*;
}
pub mod bridge;
pub mod event_hooks;
pub mod hooks;
pub mod manager;

#[cfg(feature = "embed-plugins")]
pub mod embedded;

// Re-export the main interface
pub use manager::PluginManager;

// Re-export thread module for oneshot channels used by plugin action execution
#[cfg(feature = "plugins")]
pub use fresh_plugin_runtime::thread;
