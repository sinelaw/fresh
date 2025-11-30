//! Asynchronous services and external integrations
//!
//! This module contains all code that deals with external processes,
//! I/O, and async operations.

pub mod async_bridge;
pub mod clipboard;
pub mod fs;
pub mod lsp;
pub mod plugins;
pub mod process_limits;
pub mod recovery;
pub mod signal_handler;
