//! UI rendering modules
//!
//! This module contains all rendering logic for the editor UI,
//! separated into focused submodules:
//! - `tabs` - Tab bar rendering for multiple buffers
//! - `status_bar` - Status bar and prompt/minibuffer display
//! - `suggestions` - Autocomplete and command palette UI
//! - `help` - Help page rendering and navigation
//! - `split_rendering` - Split pane layout and rendering

pub mod help;
pub mod split_rendering;
pub mod status_bar;
pub mod suggestions;
pub mod tabs;

// Re-export main types for convenience
pub use help::HelpRenderer;
pub use split_rendering::SplitRenderer;
pub use status_bar::StatusBarRenderer;
pub use suggestions::SuggestionsRenderer;
pub use tabs::TabsRenderer;
