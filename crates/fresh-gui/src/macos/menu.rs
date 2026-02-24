//! Native macOS menu bar integration.
//!
//! This module creates a native macOS menu bar using the `muda` crate,
//! mirroring the editor's built-in menu structure. Menu item activations
//! are translated back into editor actions via a channel.
//!
//! The menu bar includes the standard macOS application menu (Fresh),
//! plus File, Edit, View, Selection, Go, LSP, and Help menus that
//! correspond to the editor's internal menu definitions.

use muda::{
    AboutMetadata, Menu as MudaMenu, MenuEvent, MenuItem as MudaMenuItem, PredefinedMenuItem,
    Submenu,
};
use std::collections::HashMap;
use std::sync::mpsc;

/// An action triggered by a native menu item click.
#[derive(Debug, Clone)]
pub struct NativeMenuAction {
    /// The editor action name (e.g. "save", "open", "quit").
    pub action: String,
    /// Optional action arguments.
    pub args: HashMap<String, serde_json::Value>,
}

/// Receiver end for native menu events.
pub type NativeMenuReceiver = mpsc::Receiver<NativeMenuAction>;

/// Build the native macOS menu bar and return the menu + event receiver.
///
/// The returned `MudaMenu` must be attached to the window via
/// `menu.init_for_nsapp()` on macOS.
///
/// The receiver yields `NativeMenuAction` values whenever the user clicks
/// a menu item. The GUI event loop should poll this receiver and dispatch
/// the actions to the editor.
pub fn build_native_menu_bar() -> (MudaMenu, NativeMenuReceiver) {
    let (tx, rx) = mpsc::channel();
    let menu = MudaMenu::new();

    // -- App menu (macOS only: "Fresh" menu) --
    let app_menu = Submenu::new("Fresh", true);
    let about_item = PredefinedMenuItem::about(
        Some("About Fresh"),
        Some(AboutMetadata {
            name: Some("Fresh".to_string()),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
            copyright: Some("GPL-2.0".to_string()),
            ..Default::default()
        }),
    );
    let _ = app_menu.append(&about_item);
    let _ = app_menu.append(&PredefinedMenuItem::separator());

    let settings_item = MudaMenuItem::new("Settings...", true, None);
    register_action(&tx, &settings_item, "open_settings", HashMap::new());
    let _ = app_menu.append(&settings_item);

    let _ = app_menu.append(&PredefinedMenuItem::separator());
    let _ = app_menu.append(&PredefinedMenuItem::services(None));
    let _ = app_menu.append(&PredefinedMenuItem::separator());
    let _ = app_menu.append(&PredefinedMenuItem::hide(None));
    let _ = app_menu.append(&PredefinedMenuItem::hide_others(None));
    let _ = app_menu.append(&PredefinedMenuItem::show_all(None));
    let _ = app_menu.append(&PredefinedMenuItem::separator());

    let quit_item = MudaMenuItem::new("Quit Fresh", true, None);
    register_action(&tx, &quit_item, "quit", HashMap::new());
    let _ = app_menu.append(&quit_item);

    let _ = menu.append(&app_menu);

    // -- File menu --
    let file_menu = Submenu::new("File", true);
    add_action_item(&tx, &file_menu, "New File", "new");
    add_action_item(&tx, &file_menu, "Open...", "open");
    let _ = file_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &file_menu, "Save", "save");
    add_action_item(&tx, &file_menu, "Save As...", "save_as");
    add_action_item(&tx, &file_menu, "Revert File", "revert");
    let _ = file_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &file_menu, "Close Buffer", "close");
    let _ = file_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &file_menu, "Switch Project...", "switch_project");
    let _ = menu.append(&file_menu);

    // -- Edit menu --
    let edit_menu = Submenu::new("Edit", true);
    add_action_item(&tx, &edit_menu, "Undo", "undo");
    add_action_item(&tx, &edit_menu, "Redo", "redo");
    let _ = edit_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &edit_menu, "Cut", "cut");
    add_action_item(&tx, &edit_menu, "Copy", "copy");
    add_action_item(&tx, &edit_menu, "Paste", "paste");
    let _ = edit_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &edit_menu, "Select All", "select_all");
    let _ = edit_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &edit_menu, "Find...", "search");
    add_action_item(&tx, &edit_menu, "Find Next", "find_next");
    add_action_item(&tx, &edit_menu, "Find Previous", "find_previous");
    add_action_item(&tx, &edit_menu, "Replace...", "query_replace");
    let _ = edit_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &edit_menu, "Delete Line", "delete_line");
    add_action_item(&tx, &edit_menu, "Toggle Comment", "toggle_comment");
    let _ = edit_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &edit_menu, "Settings...", "open_settings");
    add_action_item(&tx, &edit_menu, "Keybinding Editor...", "open_keybinding_editor");
    let _ = menu.append(&edit_menu);

    // -- View menu --
    let view_menu = Submenu::new("View", true);
    add_action_item(&tx, &view_menu, "File Explorer", "toggle_file_explorer");
    let _ = view_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &view_menu, "Line Numbers", "toggle_line_numbers");
    add_action_item(&tx, &view_menu, "Word Wrap", "toggle_line_wrap");
    let _ = view_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &view_menu, "Select Theme...", "select_theme");
    add_action_item(&tx, &view_menu, "Select Language...", "select_locale");
    let _ = view_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &view_menu, "Split Horizontal", "split_horizontal");
    add_action_item(&tx, &view_menu, "Split Vertical", "split_vertical");
    add_action_item(&tx, &view_menu, "Close Split", "close_split");
    let _ = view_menu.append(&PredefinedMenuItem::separator());

    // Terminal submenu
    let terminal_sub = Submenu::new("Terminal", true);
    add_action_item(&tx, &terminal_sub, "Open Terminal", "open_terminal");
    add_action_item(&tx, &terminal_sub, "Close Terminal", "close_terminal");
    let _ = view_menu.append(&terminal_sub);

    let _ = menu.append(&view_menu);

    // -- Selection menu --
    let sel_menu = Submenu::new("Selection", true);
    add_action_item(&tx, &sel_menu, "Select All", "select_all");
    add_action_item(&tx, &sel_menu, "Select Word", "select_word");
    add_action_item(&tx, &sel_menu, "Select Line", "select_line");
    add_action_item(&tx, &sel_menu, "Expand Selection", "expand_selection");
    let _ = sel_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &sel_menu, "Add Cursor Above", "add_cursor_above");
    add_action_item(&tx, &sel_menu, "Add Cursor Below", "add_cursor_below");
    add_action_item(
        &tx,
        &sel_menu,
        "Add Next Occurrence",
        "add_cursor_next_match",
    );
    let _ = menu.append(&sel_menu);

    // -- Go menu --
    let go_menu = Submenu::new("Go", true);
    add_action_item(&tx, &go_menu, "Go to Line...", "goto_line");
    add_action_item(&tx, &go_menu, "Go to Definition", "lsp_goto_definition");
    add_action_item(&tx, &go_menu, "Find References", "lsp_references");
    let _ = go_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &go_menu, "Next Buffer", "next_buffer");
    add_action_item(&tx, &go_menu, "Previous Buffer", "prev_buffer");
    let _ = go_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &go_menu, "Command Palette...", "command_palette");
    let _ = menu.append(&go_menu);

    // -- LSP menu --
    let lsp_menu = Submenu::new("LSP", true);
    add_action_item(&tx, &lsp_menu, "Show Hover Info", "lsp_hover");
    add_action_item(&tx, &lsp_menu, "Go to Definition", "lsp_goto_definition");
    add_action_item(&tx, &lsp_menu, "Find References", "lsp_references");
    add_action_item(&tx, &lsp_menu, "Rename Symbol...", "lsp_rename");
    let _ = lsp_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &lsp_menu, "Show Completions", "lsp_completion");
    add_action_item(&tx, &lsp_menu, "Code Actions...", "lsp_code_actions");
    let _ = lsp_menu.append(&PredefinedMenuItem::separator());
    add_action_item(&tx, &lsp_menu, "Restart Server", "lsp_restart");
    add_action_item(&tx, &lsp_menu, "Stop Server", "lsp_stop");
    let _ = menu.append(&lsp_menu);

    // -- Help menu --
    let help_menu = Submenu::new("Help", true);
    add_action_item(&tx, &help_menu, "Show Manual", "show_help");
    add_action_item(&tx, &help_menu, "Keyboard Shortcuts", "keyboard_shortcuts");
    let _ = menu.append(&help_menu);

    // -- Window menu (macOS standard) --
    let window_menu = Submenu::new("Window", true);
    let _ = window_menu.append(&PredefinedMenuItem::minimize(None));
    let _ = window_menu.append(&PredefinedMenuItem::maximize(None));
    let _ = window_menu.append(&PredefinedMenuItem::separator());
    let _ = window_menu.append(&PredefinedMenuItem::fullscreen(None));
    let _ = window_menu.append(&PredefinedMenuItem::bring_all_to_front(None));
    let _ = menu.append(&window_menu);

    (menu, rx)
}

/// Poll for native menu events and return any pending action.
///
/// This should be called from the GUI event loop's `about_to_wait` or
/// similar tick function. Returns `None` if no menu events are pending.
pub fn poll_menu_event(rx: &NativeMenuReceiver) -> Option<NativeMenuAction> {
    rx.try_recv().ok()
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Thread-local map from muda menu item IDs to editor actions.
/// We use this to look up the action when a MenuEvent fires.
thread_local! {
    static MENU_ACTION_MAP: std::cell::RefCell<HashMap<muda::MenuId, NativeMenuAction>> =
        std::cell::RefCell::new(HashMap::new());
}

/// Register a menu item ID → action mapping and set up the global event handler
/// (once).
fn register_action(
    _tx: &mpsc::Sender<NativeMenuAction>,
    item: &MudaMenuItem,
    action: &str,
    args: HashMap<String, serde_json::Value>,
) {
    let native_action = NativeMenuAction {
        action: action.to_string(),
        args,
    };
    MENU_ACTION_MAP.with(|map| {
        map.borrow_mut().insert(item.id().clone(), native_action);
    });
}

/// Convenience: create a MudaMenuItem, append it to a submenu, and register
/// its action.
fn add_action_item(tx: &mpsc::Sender<NativeMenuAction>, submenu: &Submenu, label: &str, action: &str) {
    let item = MudaMenuItem::new(label, true, None);
    register_action(tx, &item, action, HashMap::new());
    let _ = submenu.append(&item);
}

/// Look up the editor action for a given `MenuEvent`.
///
/// Call this from the GUI event loop when `MenuEvent::receiver()` yields
/// an event.
pub fn resolve_menu_event(event: &MenuEvent) -> Option<NativeMenuAction> {
    MENU_ACTION_MAP.with(|map| map.borrow().get(event.id()).cloned())
}
