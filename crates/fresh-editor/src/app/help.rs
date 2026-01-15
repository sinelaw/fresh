//! Built-in help manual support
//!
//! This module provides the embedded help manual that is bundled into the binary
//! at compile time using `include_str!()`.

/// The embedded help manual content (bundled at compile time)
pub const HELP_MANUAL_CONTENT: &str = include_str!("../../../../docs/fresh.txt");

/// The name of the help manual buffer
pub const HELP_MANUAL_BUFFER_NAME: &str = "*Fresh Manual*";

/// The name of the keyboard shortcuts buffer
pub const KEYBOARD_SHORTCUTS_BUFFER_NAME: &str = "*Keyboard Shortcuts*";
