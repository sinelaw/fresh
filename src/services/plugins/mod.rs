//! Plugin system
//!
//! The plugin system provides TypeScript/JavaScript plugin support using deno_core.
//! When the `plugins` feature is disabled, only the type definitions (api, hooks, event_hooks)
//! are available - the actual runtime is excluded to avoid deno dependencies.
//!
//! Use `PluginManager` as the main interface - it handles both enabled and disabled cases.

pub mod api;
pub mod event_hooks;
pub mod hooks;
pub mod manager;

#[cfg(feature = "plugins")]
pub mod process;
#[cfg(feature = "plugins")]
pub mod runtime;
#[cfg(feature = "plugins")]
pub mod thread;

#[cfg(feature = "embed-plugins")]
pub mod embedded;

// Re-export the main interface
pub use manager::PluginManager;
