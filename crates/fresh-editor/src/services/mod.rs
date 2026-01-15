//! Asynchronous services and external integrations
//!
//! This module contains all code that deals with external processes,
//! I/O, and async operations.

pub mod async_bridge;
pub mod clipboard;
pub mod fs;
#[cfg(target_os = "linux")]
pub mod gpm;
pub mod log_dirs;
pub mod lsp;
pub mod plugins;
pub mod process_limits;
pub mod recovery;
pub mod release_checker;
pub mod signal_handler;
pub mod styled_html;
pub mod telemetry;
pub mod terminal;
pub mod terminal_modes;
pub mod time_source;
pub mod tracing_setup;
pub mod warning_log;
