//! Keybinding Editor
//!
//! A modal dialog for browsing, searching, and editing keybindings.
//! Provides a table view of all resolved bindings with search, filter,
//! key recording, conflict detection, and keymap management.

mod editor;
mod helpers;
mod types;

pub use editor::KeybindingEditor;
pub use types::*;
