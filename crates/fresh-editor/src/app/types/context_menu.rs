use crate::model::event::{BufferId, LeafId};
use rust_i18n::t;

pub const FILE_EXPLORER_CONTEXT_MENU_WIDTH: u16 = 24;

/// Width of the "+" new-tab popup menu (fits "New Terminal" + padding).
pub const NEW_TAB_MENU_WIDTH: u16 = 18;

/// Width of the tab right-click context menu (fits
/// "Extract to New Workspace" + padding).
pub const TAB_CONTEXT_MENU_WIDTH: u16 = 28;

/// Width of the close-split confirmation popup (fits "Close split" + padding).
pub const CLOSE_SPLIT_MENU_WIDTH: u16 = 16;

/// Shared geometry + navigation + hit-testing core for the native context
/// menus.
///
/// `fresh` has three native right-click / popup menus — the tab context
/// menu, the "+" new-tab popup, and the file-explorer context menu. They
/// are visually and behaviourally identical bordered item lists; only their
/// fixed width and their item source differ. This core owns everything that
/// was previously hand-copied across all three (box height, edge-clamping,
/// highlight navigation, and the "which item is at (col,row)?" hit-test), so
/// each concrete menu is just this core plus its own payload (which
/// buffer/split it acts on, which selection mode it was opened in).
///
/// The `item_count` is captured when the menu opens and is fixed for its
/// lifetime — the item source never changes while a menu is on screen — so
/// navigation and hit-testing need no access to the concrete item slice.
#[derive(Debug, Clone)]
pub struct ContextMenu {
    /// Screen position where the menu's top-left corner is anchored (x, y),
    /// before edge-clamping.
    pub position: (u16, u16),
    /// Currently highlighted item index (0-based).
    pub highlighted: usize,
    /// Menu box width in cells (fixed per menu kind).
    pub width: u16,
    /// Number of selectable items (fixed while the menu is open).
    pub item_count: usize,
}

/// Result of hit-testing a screen position against a [`ContextMenu`] box.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContextMenuHit {
    /// The position is outside the (clamped) menu box entirely.
    Outside,
    /// The position is on the top or bottom border row — inert.
    Border,
    /// The position is on item row `idx` (0-based).
    Item(usize),
}

impl ContextMenu {
    /// Anchor a fresh menu of `item_count` items at the given screen
    /// position, highlight cleared to the first item.
    pub fn new(x: u16, y: u16, width: u16, item_count: usize) -> Self {
        Self {
            position: (x, y),
            highlighted: 0,
            width,
            item_count,
        }
    }

    /// Total box height: one row per item plus the top and bottom borders.
    pub fn height(&self) -> u16 {
        self.item_count as u16 + 2
    }

    /// The anchor position clamped so the whole box stays on screen.
    pub fn clamped_position(&self, screen_width: u16, screen_height: u16) -> (u16, u16) {
        let x = if self.position.0 + self.width > screen_width {
            screen_width.saturating_sub(self.width)
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

    /// Move the highlight down one item, wrapping at the end.
    pub fn next_item(&mut self) {
        if self.item_count == 0 {
            return;
        }
        self.highlighted = (self.highlighted + 1) % self.item_count;
    }

    /// Move the highlight up one item, wrapping at the start.
    pub fn prev_item(&mut self) {
        if self.item_count == 0 {
            return;
        }
        self.highlighted = if self.highlighted == 0 {
            self.item_count - 1
        } else {
            self.highlighted - 1
        };
    }

    /// Classify a screen position against the (edge-clamped) menu box: an
    /// item row, a border row, or outside. This is the single hit-test both
    /// mouse hover and click routing consult, so the drawn box and the
    /// clickable box can never disagree.
    pub fn hit(&self, col: u16, row: u16, screen_width: u16, screen_height: u16) -> ContextMenuHit {
        let (menu_x, menu_y) = self.clamped_position(screen_width, screen_height);
        let menu_height = self.height();
        if col < menu_x || col >= menu_x + self.width || row < menu_y || row >= menu_y + menu_height
        {
            return ContextMenuHit::Outside;
        }
        // The first and last rows are the borders — inert.
        if row == menu_y || row == menu_y + menu_height - 1 {
            return ContextMenuHit::Border;
        }
        let idx = (row - menu_y - 1) as usize;
        if idx < self.item_count {
            ContextMenuHit::Item(idx)
        } else {
            // Unreachable given the bounds above (an interior row always maps
            // to a valid item), but treat any gap as inert rather than
            // fabricating an out-of-range index.
            ContextMenuHit::Border
        }
    }
}

/// Discriminates which concrete native context menu is currently open, so a
/// single generic handler can route activation to the right `execute_*`
/// dispatch after the shared geometry/navigation has done its work.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContextMenuKind {
    /// The tab right-click context menu.
    Tab,
    /// The "+" new-tab popup menu.
    NewTab,
    /// The file-explorer right-click context menu.
    FileExplorer,
    /// The close-split confirmation popup (clicking the split's `×` button).
    CloseSplit,
}

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
    /// Shared geometry + navigation core (position, highlight, width, items).
    pub menu: ContextMenu,
}

impl TabContextMenu {
    /// Create a new tab context menu
    pub fn new(buffer_id: BufferId, split_id: LeafId, x: u16, y: u16) -> Self {
        Self {
            buffer_id,
            split_id,
            menu: ContextMenu::new(
                x,
                y,
                TAB_CONTEXT_MENU_WIDTH,
                TabContextMenuItem::all().len(),
            ),
        }
    }

    /// The items this menu presents, in display order.
    pub fn items(&self) -> &'static [TabContextMenuItem] {
        TabContextMenuItem::all()
    }

    /// Get the currently highlighted item
    pub fn highlighted_item(&self) -> TabContextMenuItem {
        TabContextMenuItem::all()[self.menu.highlighted]
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
    /// Shared geometry + navigation core (position, highlight, width, items).
    pub menu: ContextMenu,
}

impl NewTabMenu {
    /// Create a new "+" popup menu anchored at the given screen position.
    pub fn new(split_id: LeafId, x: u16, y: u16) -> Self {
        Self {
            split_id,
            menu: ContextMenu::new(x, y, NEW_TAB_MENU_WIDTH, NewTabMenuItem::all().len()),
        }
    }

    /// The items this menu presents, in display order.
    pub fn items(&self) -> &'static [NewTabMenuItem] {
        NewTabMenuItem::all()
    }

    /// Get the currently highlighted item.
    pub fn highlighted_item(&self) -> NewTabMenuItem {
        NewTabMenuItem::all()[self.menu.highlighted]
    }
}

/// Items in the close-split confirmation popup (shown when clicking the split
/// tab bar's `×` button). Closing a split is not immediately reversible, so the
/// click pops this two-item confirm instead of acting at once.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CloseSplitMenuItem {
    /// Confirm: close the split.
    CloseSplit,
    /// Dismiss without closing.
    Cancel,
}

impl CloseSplitMenuItem {
    /// Get all menu items in order (confirm first, then cancel).
    pub fn all() -> &'static [Self] {
        &[Self::CloseSplit, Self::Cancel]
    }

    /// Get the display label for this menu item.
    pub fn label(&self) -> String {
        match self {
            Self::CloseSplit => t!("action.close_split").to_string(),
            Self::Cancel => t!("confirm.cancel").to_string(),
        }
    }
}

/// State for the close-split confirmation popup (left-click on a split tab
/// bar's `×` button).
#[derive(Debug, Clone)]
pub struct CloseSplitMenu {
    /// The split whose `×` button was clicked (the split to close on confirm).
    pub split_id: LeafId,
    /// Shared geometry + navigation core (position, highlight, width, items).
    pub menu: ContextMenu,
}

impl CloseSplitMenu {
    /// Create a new close-split confirmation popup anchored at the given
    /// screen position.
    pub fn new(split_id: LeafId, x: u16, y: u16) -> Self {
        Self {
            split_id,
            menu: ContextMenu::new(
                x,
                y,
                CLOSE_SPLIT_MENU_WIDTH,
                CloseSplitMenuItem::all().len(),
            ),
        }
    }

    /// The items this menu presents, in display order.
    pub fn items(&self) -> &'static [CloseSplitMenuItem] {
        CloseSplitMenuItem::all()
    }

    /// Get the currently highlighted item.
    pub fn highlighted_item(&self) -> CloseSplitMenuItem {
        CloseSplitMenuItem::all()[self.menu.highlighted]
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
    /// Whether the menu was opened with multiple items selected
    pub is_multi_selection: bool,
    /// Whether the sole selected node is the project root
    pub is_root_selected: bool,
    /// Shared geometry + navigation core (position, highlight, width, items).
    pub menu: ContextMenu,
}

impl FileExplorerContextMenu {
    pub fn new(x: u16, y: u16, is_multi_selection: bool, is_root_selected: bool) -> Self {
        let item_count = Self::items_for(is_multi_selection, is_root_selected).len();
        Self {
            is_multi_selection,
            is_root_selected,
            menu: ContextMenu::new(x, y, FILE_EXPLORER_CONTEXT_MENU_WIDTH, item_count),
        }
    }

    /// The item set for a given selection mode. The menu's item source is
    /// fixed at open time (see [`ContextMenu`]), so this is pure.
    fn items_for(
        is_multi_selection: bool,
        is_root_selected: bool,
    ) -> &'static [FileExplorerContextMenuItem] {
        if is_multi_selection {
            FileExplorerContextMenuItem::multi_selection()
        } else if is_root_selected {
            FileExplorerContextMenuItem::root_single_selection()
        } else {
            FileExplorerContextMenuItem::all()
        }
    }

    pub fn items(&self) -> &'static [FileExplorerContextMenuItem] {
        Self::items_for(self.is_multi_selection, self.is_root_selected)
    }

    /// Get the currently highlighted item.
    pub fn highlighted_item(&self) -> FileExplorerContextMenuItem {
        self.items()[self.menu.highlighted]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::SplitId;

    #[test]
    fn close_split_menu_presents_confirm_then_cancel() {
        // The confirm popup offers exactly two choices, "Close split" first
        // (the default highlight) and "Cancel" second. The order is what the
        // renderer and hit-test rely on (fresh#2768).
        assert_eq!(
            CloseSplitMenuItem::all(),
            &[CloseSplitMenuItem::CloseSplit, CloseSplitMenuItem::Cancel]
        );
        assert!(!CloseSplitMenuItem::CloseSplit.label().is_empty());
        assert!(!CloseSplitMenuItem::Cancel.label().is_empty());

        let split_id = LeafId(SplitId(7));
        let menu = CloseSplitMenu::new(split_id, 10, 4);
        assert_eq!(menu.split_id, split_id);
        assert_eq!(menu.menu.item_count, 2);
        // Default highlight is the first item — "Close split".
        assert_eq!(menu.highlighted_item(), CloseSplitMenuItem::CloseSplit);
        assert_eq!(menu.items().len(), 2);
    }

    #[test]
    fn close_split_menu_navigation_reaches_cancel() {
        let mut menu = CloseSplitMenu::new(LeafId(SplitId(1)), 0, 0);
        menu.menu.next_item();
        assert_eq!(menu.highlighted_item(), CloseSplitMenuItem::Cancel);
        menu.menu.next_item();
        // Wraps back to the confirm item.
        assert_eq!(menu.highlighted_item(), CloseSplitMenuItem::CloseSplit);
    }
}
