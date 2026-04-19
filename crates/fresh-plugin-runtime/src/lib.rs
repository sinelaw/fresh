pub mod backend;
pub mod process;
pub mod thread;
pub mod ts_export;

pub use thread::{PluginConfig, PluginThreadHandle};

/// Stable plugin name for the user's `~/.config/fresh/init.ts`.
/// Shared with `fresh-editor/src/init_script.rs` so cleanup_plugin can
/// gate the ClearRuntimeOverlay command on init.ts specifically.
pub const INIT_PLUGIN_NAME: &str = "init.ts";
