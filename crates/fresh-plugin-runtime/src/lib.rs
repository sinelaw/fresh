#[cfg(feature = "oxc")]
pub mod api_docs;
pub mod backend;
pub mod config_types;
pub mod process;
pub mod thread;
#[cfg(feature = "oxc")]
pub mod ts_export;

pub use thread::{PluginConfig, PluginThreadHandle};

/// The TypeScript/JavaScript toolchain the runtime compiles plugins with, for
/// the editor's own uses (syntax checks, scripts, the precompiled registry).
pub use fresh_parser_js;
