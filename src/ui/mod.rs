//! UI rendering modules
//!
//! This module contains all rendering logic for the editor UI,
//! separated into focused submodules:
//! - `menu` - Menu bar rendering
//! - `tabs` - Tab bar rendering for multiple buffers
//! - `status_bar` - Status bar and prompt/minibuffer display
//! - `suggestions` - Autocomplete and command palette UI
//! - `split_rendering` - Split pane layout and rendering
//! - `file_explorer` - File tree explorer rendering

pub mod file_explorer;
pub mod menu;
pub mod split_rendering;
pub mod status_bar;
pub mod suggestions;
pub mod tabs;

// Re-export main types for convenience
pub use file_explorer::FileExplorerRenderer;
pub use menu::{CheckboxStates, MenuRenderer, MenuState};
pub use split_rendering::SplitRenderer;
pub use status_bar::StatusBarRenderer;
pub use suggestions::SuggestionsRenderer;
pub use tabs::TabsRenderer;
