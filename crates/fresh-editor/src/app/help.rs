//! Built-in help manual support
//!
//! This module provides the embedded help manual that is bundled into the binary
//! at compile time using `include_str!()`.

use std::sync::LazyLock;

/// The embedded help manual content (bundled at compile time).
///
/// Normalized to LF-only line endings regardless of the source file's
/// line-ending style (the file may be checked out with CRLF on Windows).
pub static HELP_MANUAL_CONTENT: LazyLock<String> =
    LazyLock::new(|| include_str!("../../docs/fresh.txt").replace('\r', ""));

/// The name of the help manual buffer
pub const HELP_MANUAL_BUFFER_NAME: &str = "*Fresh Manual*";

/// The name of the keyboard shortcuts buffer
pub const KEYBOARD_SHORTCUTS_BUFFER_NAME: &str = "*Keyboard Shortcuts*";
