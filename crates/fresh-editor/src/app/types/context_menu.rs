use crate::model::event::{BufferId, LeafId};
use fresh_i18n::t;

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

/// A plugin's item in an open right-click menu (see
/// `addFileExplorerMenuItem` / `addTabMenuItem`): its label, and the
/// handler that choosing it calls.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PluginMenuEntry {
    pub label: String,
    pub handler: String,
    /// Whether it can be chosen: the context its `when` names is set in the
    /// window (or it names none). A disabled item is shown greyed out.
    pub enabled: bool,
}

/// One row of a menu, as both frontends draw it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContextMenuRow {
    pub label: String,
    /// A stable name for the row, for the web (`data-id`).
    pub id: String,
    pub destructive: bool,
    /// Shown greyed out, and choosing it does nothing.
    pub disabled: bool,
}

impl ContextMenuRow {
    fn plugin(entry: &PluginMenuEntry) -> Self {
        Self {
            label: entry.label.clone(),
            // A handler name belongs to one plugin (the runtime refuses
            // another's), so this names one row.
            id: format!("plugin:{}", entry.handler),
            destructive: false,
            disabled: !entry.enabled,
        }
    }
}

/// A menu's width: its own, or wider to fit a plugin's label. A row is
/// drawn as a space, the label and at least one space more, inside the
/// border.
fn width_with_plugin_items(width: u16, plugin_items: &[PluginMenuEntry]) -> u16 {
    plugin_items
        .iter()
        .map(|entry| {
            let label = crate::primitives::display_width::str_width(&entry.label);
            u16::try_from(label.saturating_add(4)).unwrap_or(u16::MAX)
        })
        .fold(width, u16::max)
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

    /// A stable name for the item, for the web (`data-id`).
    pub fn id(&self) -> &'static str {
        match self {
            Self::Close => "close",
            Self::CloseOthers => "closeOthers",
            Self::CloseToRight => "closeToRight",
            Self::CloseToLeft => "closeToLeft",
            Self::CloseAll => "closeAll",
            Self::CopyRelativePath => "copyRelativePath",
            Self::CopyFullPath => "copyFullPath",
            Self::ExtractToNewWorkspace => "extractToNewWorkspace",
        }
    }

    /// Whether the item cannot be undone: none, as closing an unsaved tab asks first.
    pub fn destructive(&self) -> bool {
        false
    }
}

/// State for tab context menu (right-click popup on tabs)
#[derive(Debug, Clone)]
pub struct TabContextMenu {
    /// The buffer ID this context menu is for
    pub buffer_id: BufferId,
    /// The split ID where the tab is located
    pub split_id: LeafId,
    /// Plugins' items for this tab, shown after the editor's own.
    pub plugin_items: Vec<PluginMenuEntry>,
    /// Shared geometry + navigation core (position, highlight, width, items).
    pub menu: ContextMenu,
}

/// What a tab menu row does when chosen.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TabMenuChoice {
    Builtin(TabContextMenuItem),
    /// Call this plugin handler with the tab.
    Plugin {
        handler: String,
    },
}

impl TabContextMenu {
    /// Create a new tab context menu
    pub fn new(
        buffer_id: BufferId,
        split_id: LeafId,
        x: u16,
        y: u16,
        plugin_items: Vec<PluginMenuEntry>,
    ) -> Self {
        let item_count = TabContextMenuItem::all().len() + plugin_items.len();
        let width = width_with_plugin_items(TAB_CONTEXT_MENU_WIDTH, &plugin_items);
        Self {
            buffer_id,
            split_id,
            plugin_items,
            menu: ContextMenu::new(x, y, width, item_count),
        }
    }

    /// The editor's own items, in display order (before the plugins').
    pub fn items(&self) -> &'static [TabContextMenuItem] {
        TabContextMenuItem::all()
    }

    /// Every row, in display order.
    pub fn rows(&self) -> Vec<ContextMenuRow> {
        self.items()
            .iter()
            .map(|i| ContextMenuRow {
                label: i.label(),
                id: i.id().to_string(),
                destructive: i.destructive(),
                disabled: false,
            })
            .chain(self.plugin_items.iter().map(ContextMenuRow::plugin))
            .collect()
    }

    /// What the highlighted row does.
    pub fn highlighted_item(&self) -> Option<TabMenuChoice> {
        let builtin = self.items();
        if let Some(item) = builtin.get(self.menu.highlighted) {
            return Some(TabMenuChoice::Builtin(*item));
        }
        let entry = self
            .plugin_items
            .get(self.menu.highlighted - builtin.len())
            .filter(|entry| entry.enabled)?;
        Some(TabMenuChoice::Plugin {
            handler: entry.handler.clone(),
        })
    }

    /// Whether the highlighted row is a disabled one, which choosing
    /// leaves the menu open.
    pub fn highlighted_is_disabled(&self) -> bool {
        self.menu
            .highlighted
            .checked_sub(self.items().len())
            .and_then(|i| self.plugin_items.get(i))
            .is_some_and(|entry| !entry.enabled)
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

    /// A stable name for the item, for the web (`data-id`).
    pub fn id(&self) -> &'static str {
        match self {
            Self::NewTerminal => "newTerminal",
            Self::NewFile => "newFile",
        }
    }

    /// Whether the item does something that cannot be undone.
    pub fn destructive(&self) -> bool {
        false
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

    /// A stable name for the item, for the web (`data-id`).
    pub fn id(&self) -> &'static str {
        match self {
            Self::CloseSplit => "closeSplit",
            Self::Cancel => "cancel",
        }
    }

    /// Whether the item cannot be undone.
    pub fn destructive(&self) -> bool {
        matches!(self, Self::CloseSplit)
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

    /// A stable name for the item, for the web (`data-id`).
    pub fn id(&self) -> &'static str {
        match self {
            Self::NewFile => "newFile",
            Self::NewDirectory => "newDirectory",
            Self::Rename => "rename",
            Self::Cut => "cut",
            Self::Copy => "copy",
            Self::Paste => "paste",
            Self::Duplicate => "duplicate",
            Self::Delete => "delete",
            Self::CopyFullPath => "copyFullPath",
            Self::CopyRelativePath => "copyRelativePath",
        }
    }

    /// Whether the item does something that cannot be undone.
    pub fn destructive(&self) -> bool {
        matches!(self, Self::Delete)
    }
}

/// State for file explorer context menu (right-click popup in the file explorer)
#[derive(Debug, Clone)]
pub struct FileExplorerContextMenu {
    /// Whether the menu was opened with multiple items selected
    pub is_multi_selection: bool,
    /// Whether the sole selected node is the project root
    pub is_root_selected: bool,
    /// Plugins' items for the entry the menu is about, shown after the
    /// editor's own. `None` for a menu about several entries or the root.
    pub plugin_items: Option<ExplorerPluginItems>,
    /// Shared geometry + navigation core (position, highlight, width, items).
    pub menu: ContextMenu,
}

/// Plugins' items in a file explorer menu, and the entry they act on.
#[derive(Debug, Clone)]
pub struct ExplorerPluginItems {
    pub target: fresh_core::api::FileExplorerMenuTarget,
    pub items: Vec<PluginMenuEntry>,
}

/// What a file explorer menu row does when chosen.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FileExplorerMenuChoice {
    Builtin(FileExplorerContextMenuItem),
    /// Call this plugin handler with the entry.
    Plugin {
        handler: String,
        target: fresh_core::api::FileExplorerMenuTarget,
    },
}

impl FileExplorerContextMenu {
    pub fn new(
        x: u16,
        y: u16,
        is_multi_selection: bool,
        is_root_selected: bool,
        plugin_items: Option<ExplorerPluginItems>,
    ) -> Self {
        let plugin_entries = plugin_items.as_ref().map_or(&[][..], |p| &p.items[..]);
        let item_count =
            Self::items_for(is_multi_selection, is_root_selected).len() + plugin_entries.len();
        let width = width_with_plugin_items(FILE_EXPLORER_CONTEXT_MENU_WIDTH, plugin_entries);
        Self {
            is_multi_selection,
            is_root_selected,
            plugin_items,
            menu: ContextMenu::new(x, y, width, item_count),
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

    /// The editor's own items, in display order (before the plugins').
    pub fn items(&self) -> &'static [FileExplorerContextMenuItem] {
        Self::items_for(self.is_multi_selection, self.is_root_selected)
    }

    fn plugin_entries(&self) -> &[PluginMenuEntry] {
        self.plugin_items.as_ref().map_or(&[], |p| &p.items)
    }

    /// Every row, in display order.
    pub fn rows(&self) -> Vec<ContextMenuRow> {
        self.items()
            .iter()
            .map(|i| ContextMenuRow {
                label: i.label(),
                id: i.id().to_string(),
                destructive: i.destructive(),
                disabled: false,
            })
            .chain(self.plugin_entries().iter().map(ContextMenuRow::plugin))
            .collect()
    }

    /// What the highlighted row does.
    pub fn highlighted_item(&self) -> Option<FileExplorerMenuChoice> {
        let builtin = self.items();
        if let Some(item) = builtin.get(self.menu.highlighted) {
            return Some(FileExplorerMenuChoice::Builtin(*item));
        }
        let plugin = self.plugin_items.as_ref()?;
        let entry = plugin
            .items
            .get(self.menu.highlighted - builtin.len())
            .filter(|entry| entry.enabled)?;
        Some(FileExplorerMenuChoice::Plugin {
            handler: entry.handler.clone(),
            target: plugin.target.clone(),
        })
    }

    /// Whether the highlighted row is a disabled one, which choosing
    /// leaves the menu open.
    pub fn highlighted_is_disabled(&self) -> bool {
        self.menu
            .highlighted
            .checked_sub(self.items().len())
            .and_then(|i| self.plugin_entries().get(i))
            .is_some_and(|entry| !entry.enabled)
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

    fn entry(label: &str, handler: &str) -> PluginMenuEntry {
        PluginMenuEntry {
            label: label.to_string(),
            handler: handler.to_string(),
            enabled: true,
        }
    }

    #[test]
    fn tab_menu_lists_plugin_items_after_its_own() {
        let menu = TabContextMenu::new(
            BufferId(1),
            LeafId(SplitId(1)),
            0,
            0,
            vec![entry("Plugin Item", "plugin_tab_item")],
        );
        let rows = menu.rows();
        let own = TabContextMenuItem::all().len();
        assert_eq!(rows.len(), own + 1);
        assert_eq!(menu.menu.item_count, own + 1);
        assert_eq!(rows[own].label, "Plugin Item");
        assert_eq!(rows[own].id, "plugin:plugin_tab_item");

        let mut menu = menu;
        menu.menu.highlighted = own;
        assert_eq!(
            menu.highlighted_item(),
            Some(TabMenuChoice::Plugin {
                handler: "plugin_tab_item".to_string()
            })
        );
        menu.menu.highlighted = 0;
        assert_eq!(
            menu.highlighted_item(),
            Some(TabMenuChoice::Builtin(TabContextMenuItem::Close))
        );
    }

    #[test]
    fn explorer_menu_hands_its_entry_to_plugin_items() {
        let target = fresh_core::api::FileExplorerMenuTarget {
            kind: fresh_core::api::FileExplorerEntryKind::File,
            path: fresh_core::api::WindowPath {
                window: fresh_core::WindowId(1),
                value: "/p/a.png".to_string(),
            },
        };
        let mut menu = FileExplorerContextMenu::new(
            0,
            0,
            false,
            false,
            Some(ExplorerPluginItems {
                target: target.clone(),
                items: vec![entry("Plugin Item", "plugin_explorer_item")],
            }),
        );
        menu.menu.highlighted = FileExplorerContextMenuItem::all().len();
        assert_eq!(
            menu.highlighted_item(),
            Some(FileExplorerMenuChoice::Plugin {
                handler: "plugin_explorer_item".to_string(),
                target,
            })
        );
    }

    #[test]
    fn a_disabled_plugin_item_is_shown_but_not_chosen() {
        let mut menu = TabContextMenu::new(
            BufferId(1),
            LeafId(SplitId(1)),
            0,
            0,
            vec![PluginMenuEntry {
                enabled: false,
                ..entry("Plugin Item", "plugin_tab_item")
            }],
        );
        let own = TabContextMenuItem::all().len();
        assert!(menu.rows()[own].disabled);
        menu.menu.highlighted = own;
        assert!(menu.highlighted_is_disabled());
        assert_eq!(menu.highlighted_item(), None);
        menu.menu.highlighted = 0;
        assert!(!menu.highlighted_is_disabled());
    }

    #[test]
    fn a_long_plugin_label_widens_the_menu() {
        let narrow = TabContextMenu::new(BufferId(1), LeafId(SplitId(1)), 0, 0, vec![]);
        assert_eq!(narrow.menu.width, TAB_CONTEXT_MENU_WIDTH);
        let label = "A label much longer than any of the tab menu's own";
        let wide = TabContextMenu::new(
            BufferId(1),
            LeafId(SplitId(1)),
            0,
            0,
            vec![entry(label, "h")],
        );
        // A space before the label and one after, inside the border.
        assert_eq!(wide.menu.width as usize, label.len() + 4);
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
