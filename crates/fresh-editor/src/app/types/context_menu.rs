use crate::model::event::{BufferId, LeafId};
use rust_i18n::t;

pub const FILE_EXPLORER_CONTEXT_MENU_WIDTH: u16 = 24;

/// Width of the "+" new-tab popup menu (fits "New Terminal" + padding).
pub const NEW_TAB_MENU_WIDTH: u16 = 18;

/// Width of the tab right-click context menu (fits
/// "Extract to New Workspace" + padding).
pub const TAB_CONTEXT_MENU_WIDTH: u16 = 28;

/// Tab context menu items
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TabContextMenuItem {
    /// Close this tab
    Close,
    /// Close all other tabs
    CloseOthers,
    /// Close tabs to the right
    CloseToRight,
    /// Close tabs to the left
    CloseToLeft,
    /// Close all tabs
    CloseAll,
    /// Copy the tab's file path relative to the workspace root
    CopyRelativePath,
    /// Copy the tab's absolute file path
    CopyFullPath,
    /// Move the tab into its own new orchestrator workspace over the same
    /// project root (a co-tenant window)
    ExtractToNewWorkspace,
}

impl TabContextMenuItem {
    /// Get all menu items in order
    pub fn all() -> &'static [Self] {
        // Append-only: existing e2e tests address items by their index in
        // this list, so new entries go after the older ones.
        &[
            Self::Close,
            Self::CloseOthers,
            Self::CloseToRight,
            Self::CloseToLeft,
            Self::CloseAll,
            Self::CopyRelativePath,
            Self::CopyFullPath,
            Self::ExtractToNewWorkspace,
        ]
    }

    /// Get the display label for this menu item
    pub fn label(&self) -> String {
        match self {
            Self::Close => t!("tab.close").to_string(),
            Self::CloseOthers => t!("tab.close_others").to_string(),
            Self::CloseToRight => t!("tab.close_to_right").to_string(),
            Self::CloseToLeft => t!("tab.close_to_left").to_string(),
            Self::CloseAll => t!("tab.close_all").to_string(),
            Self::CopyRelativePath => t!("tab.copy_relative_path").to_string(),
            Self::CopyFullPath => t!("tab.copy_full_path").to_string(),
            Self::ExtractToNewWorkspace => t!("tab.extract_to_new_workspace").to_string(),
        }
    }
}

/// State for tab context menu (right-click popup on tabs)
#[derive(Debug, Clone)]
pub struct TabContextMenu {
    /// The buffer ID this context menu is for
    pub buffer_id: BufferId,
    /// The split ID where the tab is located
    pub split_id: LeafId,
    /// Screen position where the menu should appear (x, y)
    pub position: (u16, u16),
    /// Currently highlighted menu item index
    pub highlighted: usize,
}

impl TabContextMenu {
    /// Create a new tab context menu
    pub fn new(buffer_id: BufferId, split_id: LeafId, x: u16, y: u16) -> Self {
        Self {
            buffer_id,
            split_id,
            position: (x, y),
            highlighted: 0,
        }
    }

    /// Get the currently highlighted item
    pub fn highlighted_item(&self) -> TabContextMenuItem {
        TabContextMenuItem::all()[self.highlighted]
    }

    /// Menu height including the top/bottom borders.
    pub fn height(&self) -> u16 {
        TabContextMenuItem::all().len() as u16 + 2
    }

    /// Anchor position shifted so the menu fits on screen — the single
    /// source of truth shared by rendering, hover, and click hit-testing
    /// (diverging copies would let clicks land on a menu drawn elsewhere).
    pub fn clamped_position(&self, screen_width: u16, screen_height: u16) -> (u16, u16) {
        let x = if self.position.0 + TAB_CONTEXT_MENU_WIDTH > screen_width {
            screen_width.saturating_sub(TAB_CONTEXT_MENU_WIDTH)
        } else {
            self.position.0
        };
        let h = self.height();
        let y = if self.position.1 + h > screen_height {
            screen_height.saturating_sub(h)
        } else {
            self.position.1
        };
        (x, y)
    }

    /// Move highlight down
    pub fn next_item(&mut self) {
        let items = TabContextMenuItem::all();
        self.highlighted = (self.highlighted + 1) % items.len();
    }

    /// Move highlight up
    pub fn prev_item(&mut self) {
        let items = TabContextMenuItem::all();
        self.highlighted = if self.highlighted == 0 {
            items.len() - 1
        } else {
            self.highlighted - 1
        };
    }
}

/// Items in the "+" new-tab popup menu (shown when clicking the `+`
/// button at the end of the tab bar).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NewTabMenuItem {
    /// Open a new terminal in the split
    NewTerminal,
    /// Create a new empty file buffer
    NewFile,
}

impl NewTabMenuItem {
    /// Get all menu items in order.
    pub fn all() -> &'static [Self] {
        &[Self::NewTerminal, Self::NewFile]
    }

    /// Get the display label for this menu item.
    pub fn label(&self) -> String {
        match self {
            Self::NewTerminal => t!("tab.new_terminal").to_string(),
            Self::NewFile => t!("tab.new_file").to_string(),
        }
    }
}

/// State for the "+" new-tab popup menu (left-click on the tab bar's
/// trailing `+` button).
#[derive(Debug, Clone)]
pub struct NewTabMenu {
    /// The split whose tab bar's `+` button was clicked.
    pub split_id: LeafId,
    /// Screen position where the menu should appear (x, y).
    pub position: (u16, u16),
    /// Currently highlighted menu item index.
    pub highlighted: usize,
}

impl NewTabMenu {
    /// Create a new "+" popup menu anchored at the given screen position.
    pub fn new(split_id: LeafId, x: u16, y: u16) -> Self {
        Self {
            split_id,
            position: (x, y),
            highlighted: 0,
        }
    }

    /// Move highlight down.
    pub fn next_item(&mut self) {
        let items = NewTabMenuItem::all();
        self.highlighted = (self.highlighted + 1) % items.len();
    }

    /// Move highlight up.
    pub fn prev_item(&mut self) {
        let items = NewTabMenuItem::all();
        self.highlighted = if self.highlighted == 0 {
            items.len() - 1
        } else {
            self.highlighted - 1
        };
    }
}

/// File explorer context menu items
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileExplorerContextMenuItem {
    NewFile,
    NewDirectory,
    Rename,
    Cut,
    Copy,
    Paste,
    Duplicate,
    Delete,
    CopyFullPath,
    CopyRelativePath,
}

impl FileExplorerContextMenuItem {
    pub fn all() -> &'static [Self] {
        // Order matters: existing e2e tests address items by their index in
        // this list (e.g. Delete is index 6 in the single-selection menu).
        // Append-only changes here keep the older tests stable; the new
        // entries (Duplicate, CopyFullPath, CopyRelativePath) live after
        // Delete for that reason.
        &[
            Self::NewFile,
            Self::NewDirectory,
            Self::Rename,
            Self::Cut,
            Self::Copy,
            Self::Paste,
            Self::Delete,
            Self::Duplicate,
            Self::CopyFullPath,
            Self::CopyRelativePath,
        ]
    }

    pub fn multi_selection() -> &'static [Self] {
        &[
            Self::Cut,
            Self::Copy,
            Self::Paste,
            Self::Delete,
            Self::Duplicate,
            Self::CopyFullPath,
            Self::CopyRelativePath,
        ]
    }

    pub fn root_single_selection() -> &'static [Self] {
        // The root menu is intentionally narrow (VS Code parity): only
        // creation + paste actions. Copy-path on the project root is left
        // off because the workspace path is already exposed via other
        // commands and adding it here would surface a "Copy …" entry on
        // a menu that's supposed to hide destructive/copy-style actions.
        &[Self::NewFile, Self::NewDirectory, Self::Paste]
    }

    pub fn label(&self) -> String {
        match self {
            Self::NewFile => t!("explorer.context.new_file").to_string(),
            Self::NewDirectory => t!("explorer.context.new_directory").to_string(),
            Self::Rename => t!("explorer.context.rename").to_string(),
            Self::Cut => t!("explorer.context.cut").to_string(),
            Self::Copy => t!("explorer.context.copy").to_string(),
            Self::Paste => t!("explorer.context.paste").to_string(),
            Self::Duplicate => t!("explorer.context.duplicate").to_string(),
            Self::Delete => t!("explorer.context.delete").to_string(),
            Self::CopyFullPath => t!("explorer.context.copy_full_path").to_string(),
            Self::CopyRelativePath => t!("explorer.context.copy_relative_path").to_string(),
        }
    }
}

/// State for file explorer context menu (right-click popup in the file explorer)
#[derive(Debug, Clone)]
pub struct FileExplorerContextMenu {
    /// Screen position where the menu should appear (x, y)
    pub position: (u16, u16),
    /// Currently highlighted menu item index
    pub highlighted: usize,
    /// Whether the menu was opened with multiple items selected
    pub is_multi_selection: bool,
    /// Whether the sole selected node is the project root
    pub is_root_selected: bool,
}

impl FileExplorerContextMenu {
    pub fn new(x: u16, y: u16, is_multi_selection: bool, is_root_selected: bool) -> Self {
        Self {
            position: (x, y),
            highlighted: 0,
            is_multi_selection,
            is_root_selected,
        }
    }

    pub fn items(&self) -> &'static [FileExplorerContextMenuItem] {
        if self.is_multi_selection {
            FileExplorerContextMenuItem::multi_selection()
        } else if self.is_root_selected {
            FileExplorerContextMenuItem::root_single_selection()
        } else {
            FileExplorerContextMenuItem::all()
        }
    }

    pub fn height(&self) -> u16 {
        self.items().len() as u16 + 2
    }

    pub fn clamped_position(&self, screen_width: u16, screen_height: u16) -> (u16, u16) {
        let x = if self.position.0 + FILE_EXPLORER_CONTEXT_MENU_WIDTH > screen_width {
            screen_width.saturating_sub(FILE_EXPLORER_CONTEXT_MENU_WIDTH)
        } else {
            self.position.0
        };
        let h = self.height();
        let y = if self.position.1 + h > screen_height {
            screen_height.saturating_sub(h)
        } else {
            self.position.1
        };
        (x, y)
    }

    pub fn next_item(&mut self) {
        let len = self.items().len();
        self.highlighted = (self.highlighted + 1) % len;
    }

    pub fn prev_item(&mut self) {
        let len = self.items().len();
        self.highlighted = if self.highlighted == 0 {
            len - 1
        } else {
            self.highlighted - 1
        };
    }
}
