//! Native macOS menu bar — builds `muda` menus from the editor's
//! [`fresh_core::menu::Menu`] / [`MenuItem`] model.
//!
//! The public entry point is [`build_from_model`], which takes a `&[Menu]`
//! (the same structure the editor uses for its built-in TUI menu bar) and
//! returns a `muda::Menu` ready to be attached to NSApp.
//!
//! Menu item clicks are resolved back to editor actions via
//! [`resolve_menu_event`].

use fresh_core::menu::{Menu, MenuItem};
use muda::{
    AboutMetadata, Menu as MudaMenu, MenuEvent, MenuItem as MudaMenuItem, PredefinedMenuItem,
    Submenu,
};
use std::collections::HashMap;

use crate::native_menu::MenuAction;

// ---------------------------------------------------------------------------
// Thread-local action map
// ---------------------------------------------------------------------------

thread_local! {
    /// Maps muda menu-item IDs → editor actions so we can resolve clicks.
    static ACTION_MAP: std::cell::RefCell<HashMap<muda::MenuId, MenuAction>> =
        std::cell::RefCell::new(HashMap::new());
}

/// Clear the action map (called before a full rebuild so stale entries don't
/// accumulate).
fn clear_action_map() {
    ACTION_MAP.with(|map| map.borrow_mut().clear());
}

/// Register a `MudaMenuItem` → `MenuAction` mapping.
fn register(item: &MudaMenuItem, action: &str, args: &HashMap<String, serde_json::Value>) {
    ACTION_MAP.with(|map| {
        map.borrow_mut().insert(
            item.id().clone(),
            MenuAction {
                action: action.to_string(),
                args: args.clone(),
            },
        );
    });
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Build a `muda::Menu` from the editor's menu model.
///
/// The returned menu contains:
/// 1. A standard macOS **app menu** (About, Settings, Services, Hide, Quit)
/// 2. All menus from `menus` converted recursively
/// 3. A standard **Window** menu (Minimize, Maximize, Fullscreen)
pub fn build_from_model(menus: &[Menu], app_name: &str) -> MudaMenu {
    clear_action_map();
    let muda_menu = MudaMenu::new();

    // -- App menu (macOS-only: application name menu) -------------------------
    let app_submenu = Submenu::new(app_name, true);
    let _ = app_submenu.append(&PredefinedMenuItem::about(
        Some(&format!("About {app_name}")),
        Some(AboutMetadata {
            name: Some(app_name.to_string()),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
            copyright: Some("GPL-2.0".to_string()),
            ..Default::default()
        }),
    ));
    let _ = app_submenu.append(&PredefinedMenuItem::separator());

    // "Settings…" wired to the editor action
    let settings = MudaMenuItem::new("Settings\u{2026}", true, None);
    register(&settings, "open_settings", &HashMap::new());
    let _ = app_submenu.append(&settings);

    let _ = app_submenu.append(&PredefinedMenuItem::separator());
    let _ = app_submenu.append(&PredefinedMenuItem::services(None));
    let _ = app_submenu.append(&PredefinedMenuItem::separator());
    let _ = app_submenu.append(&PredefinedMenuItem::hide(None));
    let _ = app_submenu.append(&PredefinedMenuItem::hide_others(None));
    let _ = app_submenu.append(&PredefinedMenuItem::show_all(None));
    let _ = app_submenu.append(&PredefinedMenuItem::separator());

    let quit = MudaMenuItem::new(&format!("Quit {app_name}"), true, None);
    register(&quit, "quit", &HashMap::new());
    let _ = app_submenu.append(&quit);
    let _ = muda_menu.append(&app_submenu);

    // -- Editor-defined menus -------------------------------------------------
    for menu in menus {
        let sub = convert_menu(menu);
        let _ = muda_menu.append(&sub);
    }

    // -- Standard Window menu -------------------------------------------------
    let window_menu = Submenu::new("Window", true);
    let _ = window_menu.append(&PredefinedMenuItem::minimize(None));
    let _ = window_menu.append(&PredefinedMenuItem::maximize(None));
    let _ = window_menu.append(&PredefinedMenuItem::separator());
    let _ = window_menu.append(&PredefinedMenuItem::fullscreen(None));
    let _ = window_menu.append(&PredefinedMenuItem::bring_all_to_front(None));
    let _ = muda_menu.append(&window_menu);

    muda_menu
}

/// Resolve a `muda::MenuEvent` to the editor [`MenuAction`] that was
/// registered when the menu was built.
pub fn resolve_menu_event(event: &MenuEvent) -> Option<MenuAction> {
    ACTION_MAP.with(|map| map.borrow().get(event.id()).cloned())
}

// ---------------------------------------------------------------------------
// Recursive model → muda conversion
// ---------------------------------------------------------------------------

/// Convert a top-level `Menu` to a `muda::Submenu`.
fn convert_menu(menu: &Menu) -> Submenu {
    let sub = Submenu::new(&menu.label, true);
    for item in &menu.items {
        append_item(&sub, item);
    }
    sub
}

/// Append a single `MenuItem` (recursively for submenus) to a `muda::Submenu`.
fn append_item(parent: &Submenu, item: &MenuItem) {
    match item {
        MenuItem::Separator { .. } => {
            let _ = parent.append(&PredefinedMenuItem::separator());
        }

        MenuItem::Action {
            label,
            action,
            args,
            ..
        } => {
            let muda_item = MudaMenuItem::new(label, true, None);
            register(&muda_item, action, args);
            let _ = parent.append(&muda_item);
        }

        MenuItem::Submenu { label, items } => {
            let child = Submenu::new(label, true);
            for sub_item in items {
                append_item(&child, sub_item);
            }
            let _ = parent.append(&child);
        }

        MenuItem::DynamicSubmenu { label, .. } => {
            // Dynamic submenus should be resolved at the editor layer before
            // being passed here. If one slips through unresolved, show a
            // placeholder.
            let child = Submenu::new(label, true);
            let _ = parent.append(&child);
        }

        MenuItem::Label { info } => {
            // Informational labels are shown as disabled items.
            let muda_item = MudaMenuItem::new(info, false, None);
            let _ = parent.append(&muda_item);
        }
    }
}
