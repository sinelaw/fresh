//! Asynchronous services and external integrations
//!
//! This module contains all code that deals with external processes,
//! I/O, and async operations.

pub mod async_bridge;
pub mod authority;
pub mod clipboard;
pub mod completion;
pub use fresh_editor_core::counters;
pub mod editorconfig;
pub mod env_provider;
pub mod file_watcher;
pub mod fs;
#[cfg(target_os = "linux")]
pub mod gpm;
/// Outbound HTTP(S); the only place `ureq`/TLS is used (gated by `http`).
pub mod http;
pub mod live_grep_state;
pub use fresh_editor_core::log_dirs;
pub mod lsp;
pub use fresh_editor_core::packages;
pub mod plugins;
pub use fresh_editor_core::process_hidden;
// Lives in `fresh-editor-core`: the impl blocks hang off `types::ProcessLimits`,
// which is defined down there.
pub use fresh_editor_core::process_limits;
pub mod recovery;
pub mod release_checker;
pub mod remote;
pub mod signal_handler;
pub mod status_log;
pub mod stdin_spool;
pub mod styled_html;
pub mod telemetry;
pub mod terminal;
pub mod terminal_modes;
pub mod terminal_title;
pub mod time_source;
pub mod tracing_setup;
#[cfg(all(unix, feature = "runtime"))]
pub mod tty_input;
pub mod warning_log;
pub mod workspace_trust;
