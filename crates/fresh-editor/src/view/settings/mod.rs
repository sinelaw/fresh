//! Settings UI module
//!
//! Provides a built-in settings editor that reads from JSON Schema
//! and renders appropriate controls for each setting type.
//!
//! ## Architecture
//!
//! - `schema.rs` - Parse JSON Schema into setting definitions (WASM-compatible)
//! - `items.rs` - Convert schema to renderable items with control states
//! - `state.rs` - Manage settings UI state and pending changes
//! - `render.rs` - Render the settings modal
//! - `layout.rs` - Hit testing for mouse interaction
//! - `entry_dialog.rs` - Dialog for editing complex map entries

// Schema is WASM-compatible (pure data types)
pub mod schema;

// Runtime-only modules (depend on config_io, state, etc.)
#[cfg(feature = "runtime")]
pub mod entry_dialog;
#[cfg(feature = "runtime")]
pub mod input;
#[cfg(feature = "runtime")]
pub mod items;
#[cfg(feature = "runtime")]
pub mod layout;
#[cfg(feature = "runtime")]
pub mod mouse;
#[cfg(feature = "runtime")]
pub mod render;
#[cfg(feature = "runtime")]
pub mod search;
#[cfg(feature = "runtime")]
pub mod state;

#[cfg(feature = "runtime")]
pub use entry_dialog::EntryDialogState;
#[cfg(feature = "runtime")]
pub use layout::{SettingsHit, SettingsLayout};
#[cfg(feature = "runtime")]
pub use render::render_settings;
#[cfg(feature = "runtime")]
pub use search::{search_settings, SearchResult};
#[cfg(feature = "runtime")]
pub use state::{FocusPanel, SettingsState};
