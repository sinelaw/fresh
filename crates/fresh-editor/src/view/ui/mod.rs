//! UI rendering modules
//!
//! This module contains all rendering logic for the editor UI,
//! separated into focused submodules:
//! - `menu` - Menu bar rendering
//! - `tabs` - Tab bar rendering for multiple buffers
//! - `status_bar` - Status bar and prompt/minibuffer display
//! - `split_rendering` - Split pane layout and rendering
//! - `file_explorer` - File tree explorer rendering
//! - `scrollbar` - Reusable scrollbar widget
//! - `scroll_panel` - Reusable scrollable panel for variable-height items
//! - `file_browser` - File open dialog popup

// WASM-compatible modules (pure rendering, no runtime deps)
pub mod layout;
pub mod scroll_panel;
pub mod scrollbar;
pub mod view_pipeline;

/// `TextEdit` lives in `crate::primitives::text_edit` now (shared
/// between the legacy Settings UI and the plugin widget framework).
/// Re-exported here under the historical `crate::view::ui::text_edit`
/// path so existing call sites keep compiling unchanged.
pub use crate::primitives::text_edit;

// Runtime-only modules (depend on state, services, input, etc.)
#[cfg(feature = "runtime")]
pub mod expanded_menus_cache;
#[cfg(feature = "runtime")]
pub mod file_browser;
#[cfg(feature = "runtime")]
pub mod file_explorer;
#[cfg(feature = "runtime")]
pub mod menu;
#[cfg(feature = "runtime")]
#[cfg(feature = "runtime")]
pub mod split_rendering;
#[cfg(feature = "runtime")]
pub mod status_bar;
#[cfg(feature = "runtime")]
pub mod tabs;

// Re-export main types for convenience
pub use crate::primitives::text_edit::TextEdit;
#[cfg(feature = "runtime")]
pub use expanded_menus_cache::ExpandedMenusCache;
#[cfg(feature = "runtime")]
pub use file_browser::{
    FileBrowserLayout, FileBrowserRenderer, FileBrowserToggle, FileBrowserToggleSpan,
};
#[cfg(feature = "runtime")]
pub use file_explorer::FileExplorerRenderer;
pub use layout::point_in_rect;
pub(crate) use menu::MenuRowStyle;
#[cfg(feature = "runtime")]
pub use menu::{context_keys, MenuContext, MenuLayout, MenuRenderer, MenuState};
#[cfg(feature = "runtime")]
pub use scroll_panel::{FocusRegion, ScrollItem, ScrollState, ScrollablePanel};
pub use scrollbar::{render_scrollbar, ScrollbarColors, ScrollbarState};
#[cfg(feature = "runtime")]
pub use split_rendering::{EditorRenderConfig, RenderStyle, SplitRenderer};
#[cfg(feature = "runtime")]
pub use status_bar::{truncate_path, StatusBarRenderer, TruncatedPath};
#[cfg(feature = "runtime")]
pub use tabs::{TabHit, TabHitArea, TabLayout, TabsRenderer};
