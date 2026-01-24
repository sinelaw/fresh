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
//! - `scrollbar` - Reusable scrollbar widget
//! - `scroll_panel` - Reusable scrollable panel for variable-height items
//! - `file_browser` - File open dialog popup

// WASM-compatible modules (pure rendering, no runtime deps)
pub mod scroll_panel;
pub mod scrollbar;
pub mod text_edit;
pub mod view_pipeline;

// Runtime-only modules (depend on state, services, input, etc.)
#[cfg(feature = "runtime")]
pub mod file_browser;
#[cfg(feature = "runtime")]
pub mod file_explorer;
#[cfg(feature = "runtime")]
pub mod menu;
#[cfg(feature = "runtime")]
pub mod menu_input;
#[cfg(feature = "runtime")]
pub mod split_rendering;
#[cfg(feature = "runtime")]
pub mod status_bar;
#[cfg(feature = "runtime")]
pub mod suggestions;
#[cfg(feature = "runtime")]
pub mod tabs;

// Re-export main types for convenience
#[cfg(feature = "runtime")]
pub use file_browser::{FileBrowserLayout, FileBrowserRenderer};
#[cfg(feature = "runtime")]
pub use file_explorer::FileExplorerRenderer;
#[cfg(feature = "runtime")]
pub use menu::{context_keys, MenuContext, MenuRenderer, MenuState};
#[cfg(feature = "runtime")]
pub use menu_input::MenuInputHandler;
pub use scroll_panel::{
    FocusRegion, RenderInfo, ScrollItem, ScrollState, ScrollablePanel, ScrollablePanelLayout,
};
pub use scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};
#[cfg(feature = "runtime")]
pub use split_rendering::SplitRenderer;
#[cfg(feature = "runtime")]
pub use status_bar::{truncate_path, StatusBarLayout, StatusBarRenderer, TruncatedPath};
#[cfg(feature = "runtime")]
pub use suggestions::SuggestionsRenderer;
#[cfg(feature = "runtime")]
pub use tabs::TabsRenderer;
pub use text_edit::TextEdit;
