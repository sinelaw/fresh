//! Settings UI module
//!
//! Provides a built-in settings editor that reads from JSON Schema
//! and renders appropriate controls for each setting type.
//!
//! ## Architecture
//!
//! - `schema.rs` - Parse JSON Schema into setting definitions
//! - `items.rs` - Convert schema to renderable items with control states
//! - `state.rs` - Manage settings UI state and pending changes
//! - `render.rs` - Render the settings modal
//! - `layout.rs` - Hit testing for mouse interaction
//! - `entry_dialog.rs` - Dialog for editing complex map entries

pub mod entry_dialog;
pub mod input;
pub mod items;
pub mod layout;
pub mod mouse;
pub mod render;
pub mod schema;
pub mod search;
pub mod state;

pub use entry_dialog::EntryDialogState;
pub use layout::{SettingsHit, SettingsLayout};
pub use render::render_settings;
pub use search::{search_settings, SearchResult};
pub use state::{FocusPanel, SettingsState};
