//! Keybinding Editor
//!
//! A modal dialog for browsing, searching, and editing keybindings.
//! Provides a table view of all resolved bindings with search, filter,
//! key recording, conflict detection, and keymap management.

mod editor;
// `pub(crate)` for the config-name serialiser: `keybindings::parse_key` is its
// exact inverse, and `config_names_round_trip` holds the two together.
pub(crate) mod helpers;
mod types;

pub use editor::KeybindingEditor;
pub use types::*;
