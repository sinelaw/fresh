//! GPM (General Purpose Mouse) support for Linux console
//!
//! This module provides mouse support when running in a Linux virtual console (TTY)
//! where the GPM daemon is available. GPM provides mouse events in environments
//! where standard terminal mouse protocols (xterm, SGR) are not available.
//!
//! # Usage
//!
//! GPM support is only available on Linux and requires the GPM daemon to be running.
//! The module automatically detects if GPM is available and connects to it.
//!
//! # Architecture
//!
//! - `ffi.rs` - Raw FFI bindings to libgpm
//! - `types.rs` - Rust types for GPM events, buttons, modifiers
//! - `client.rs` - High-level GPM client for connecting and reading events
//! - `convert.rs` - Conversion from GPM events to crossterm events

mod client;
mod convert;
mod ffi;
mod types;

pub use client::GpmClient;
pub use convert::gpm_to_crossterm;
pub use types::{GpmButtons, GpmEvent, GpmEventType, GpmModifiers};
