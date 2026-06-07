mod buffer_group;
mod buffer_meta;
mod context_menu;
mod drag;
mod hover;
mod layout;
mod lsp_state;
mod lsp_uri;
mod mouse;
mod search_state;
mod theme;

pub const DEFAULT_BACKGROUND_FILE: &str = "scripts/landscape-wide.txt";

// buffer_group re-exports
pub use buffer_group::{BufferGroup, BufferGroupId, GroupLayoutNode};

// buffer_meta re-exports
pub use buffer_meta::{BufferKind, BufferMetadata};

// context_menu re-exports
pub use context_menu::FILE_EXPLORER_CONTEXT_MENU_WIDTH;
pub use context_menu::NEW_TAB_MENU_WIDTH;
pub use context_menu::{
    FileExplorerContextMenu, FileExplorerContextMenuItem, NewTabMenu, NewTabMenuItem,
    TabContextMenu, TabContextMenuItem,
};

// drag re-exports
pub use drag::{TabDragState, TabDropZone};

// hover re-exports
pub use hover::HoverTarget;

// layout re-exports
pub(crate) use layout::{ChromeLayout, WindowLayoutCache};
pub use layout::{OverlayPreviewState, ViewLineMapping};

// lsp_state re-exports
pub use lsp_state::LspMenuItem;
pub(crate) use lsp_state::{LspMessageEntry, LspProgressInfo};

// lsp_uri re-exports
pub use lsp_uri::{file_path_to_lsp_uri, file_path_to_lsp_uri_with_translation, LspUri};

// mouse re-exports
pub(crate) use mouse::MouseState;

// search_state re-exports
pub(super) use search_state::EventLineInfo;
pub(crate) use search_state::{InteractiveReplaceState, SearchState};

// theme re-exports
pub use theme::{CellThemeInfo, ThemeInfoPopup, ThemeKeyInfo};
