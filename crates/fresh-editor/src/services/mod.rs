//! Asynchronous services and external integrations
//!
//! This module contains all code that deals with external processes,
//! I/O, and async operations.

pub mod async_bridge;
pub mod authority;
pub mod clipboard;
pub mod completion;
pub mod counters;
pub mod editorconfig;
pub mod env_provider;
pub mod file_watcher;
pub mod fs;
#[cfg(target_os = "linux")]
pub mod gpm;
/// Outbound HTTP(S); the only place `ureq`/TLS is used (gated by `http`).
pub mod http;
pub mod live_grep_state;
pub mod log_dirs;
pub mod lsp;
pub mod packages;
pub mod plugins;
pub mod process_hidden;
pub mod process_limits;
pub mod recovery;
pub mod release_checker;
pub mod remote;
pub mod signal_handler;
pub mod status_log;
pub mod styled_html;
pub mod telemetry;
pub mod terminal;
pub mod terminal_modes;
/// In-place self-update engine for self-contained installs (`fresh update`).
#[cfg(feature = "self-update")]
pub mod updater;
// Unix host-input reader (raw stdin -> InputParser). Only needed by the
// interactive binary, which requires `runtime`.
pub mod terminal_title;
pub mod time_source;
pub mod tracing_setup;
#[cfg(all(unix, feature = "runtime"))]
pub mod tty_input;
pub mod warning_log;
pub mod workspace_trust;
