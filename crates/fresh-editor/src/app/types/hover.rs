use crate::app::file_open::SortMode;
use crate::model::event::{ContainerId, LeafId, SplitDirection};

/// Types of UI elements that can be hovered over
#[derive(Debug, Clone, PartialEq)]
pub enum HoverTarget {
    /// Hovering over a split separator (container_id, direction)
    SplitSeparator(ContainerId, SplitDirection),
    /// Hovering over a scrollbar thumb (split_id)
    ScrollbarThumb(LeafId),
    /// Hovering over a scrollbar track (split_id, relative_row)
    ScrollbarTrack(LeafId, u16),
    /// Hovering over a menu bar item (menu_index)
    MenuBarItem(usize),
    /// Hovering over a menu dropdown item (menu_index, item_index)
    MenuDropdownItem(usize, usize),
    /// Hovering over a submenu item (depth, item_index) - depth 1+ for nested submenus
    SubmenuItem(usize, usize),
    /// Hovering over a popup list item (popup_index in stack, item_index)
    PopupListItem(usize, usize),
    /// Hovering over a suggestion item (item_index)
    SuggestionItem(usize),
    /// Hovering over the file explorer border (for resize)
    FileExplorerBorder,
    /// Hovering over a file browser navigation shortcut
    FileBrowserNavShortcut(usize),
    /// Hovering over a file browser file/directory entry
    FileBrowserEntry(usize),
    /// Hovering over a file browser column header
    FileBrowserHeader(SortMode),
    /// Hovering over the file browser scrollbar
    FileBrowserScrollbar,
    /// Hovering over the file browser "Show Hidden" checkbox
    FileBrowserShowHiddenCheckbox,
    /// Hovering over the file browser "Detect Encoding" checkbox
    FileBrowserDetectEncodingCheckbox,
    /// Hovering over a tab name (target, split_id) - for non-active tabs
    TabName(crate::view::split::TabTarget, LeafId),
    /// Hovering over a tab close button (target, split_id)
    TabCloseButton(crate::view::split::TabTarget, LeafId),
    /// Hovering over a close split button (split_id)
    CloseSplitButton(LeafId),
    /// Hovering over a maximize/unmaximize split button (split_id)
    MaximizeSplitButton(LeafId),
    /// Hovering over the file explorer close button
    FileExplorerCloseButton,
    /// Hovering over a file explorer item's status indicator (path)
    FileExplorerStatusIndicator(std::path::PathBuf),
    /// Hovering over the status bar LSP indicator
    StatusBarLspIndicator,
    /// Hovering over the status bar remote-authority indicator
    StatusBarRemoteIndicator,
    /// Hovering over the status bar warning badge
    StatusBarWarningBadge,
    /// Hovering over the status bar line ending indicator
    StatusBarLineEndingIndicator,
    /// Hovering over the status bar encoding indicator
    StatusBarEncodingIndicator,
    /// Hovering over the status bar language indicator
    StatusBarLanguageIndicator,
    /// Hovering over the search options "Case Sensitive" checkbox
    SearchOptionCaseSensitive,
    /// Hovering over the search options "Whole Word" checkbox
    SearchOptionWholeWord,
    /// Hovering over the search options "Regex" checkbox
    SearchOptionRegex,
    /// Hovering over the search options "Confirm Each" checkbox
    SearchOptionConfirmEach,
    /// Hovering over a tab context menu item (item_index)
    TabContextMenuItem(usize),
    /// Hovering over a file explorer context menu item (item_index)
    FileExplorerContextMenuItem(usize),
    /// Hovering over a "+" new-tab popup menu item (item_index)
    NewTabMenuItem(usize),
}
