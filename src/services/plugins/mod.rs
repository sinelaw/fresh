//! Plugin system
//!
//! The plugin system provides TypeScript/JavaScript plugin support using QuickJS + oxc.
//! When the `plugins` feature is disabled, only the type definitions (api, hooks, event_hooks)
//! are available - the actual runtime is excluded.
//!
//! Use `PluginManager` as the main interface - it handles both enabled and disabled cases.

pub mod api;
pub mod event_hooks;
pub mod hooks;
pub mod manager;

#[cfg(feature = "plugins")]
pub mod backend;
#[cfg(feature = "plugins")]
pub mod process;
#[cfg(feature = "plugins")]
pub mod thread;
#[cfg(feature = "plugins")]
pub mod transpile;
#[cfg(feature = "plugins")]
pub mod ts_export;

#[cfg(feature = "embed-plugins")]
pub mod embedded;

// Re-export the main interface
pub use manager::PluginManager;
