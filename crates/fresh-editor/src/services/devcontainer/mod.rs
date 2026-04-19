//! Devcontainer support
//!
//! This module provides detection, lifecycle management, and connection
//! to development containers via the `devcontainer` CLI.

pub mod cli;
pub mod detect;

pub use cli::{DevcontainerCli, DevcontainerError, UpResult};
pub use detect::detect_devcontainer;
