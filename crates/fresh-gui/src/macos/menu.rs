//! Native macOS menu bar — builds `muda` menus from the editor's
//! [`fresh_core::menu::Menu`] / [`MenuItem`] model.
//!
//! The public entry point is [`build_from_model`], which takes a `&[Menu]`
//! (the same structure the editor uses for its built-in TUI menu bar) and
//! returns a `muda::Menu` ready to be attached to NSApp.
//!
//! Menu item clicks are resolved back to editor actions via
//! [`resolve_menu_event`].
//!
//! ## Dynamic features
//!
//! * **`when` conditions** — Action items with a `when` field are
//!   enabled/disabled based on the [`MenuContext`].  The initial state is
//!   set at build time; subsequent updates are applied incrementally by
//!   [`sync_tracked_items`].
//!
//! * **`checkbox` items** — Action items with a `checkbox` field are
//!   rendered as `muda::CheckMenuItem`.  The checked state comes from
//!   [`MenuContext`].
//!
//! * **`DynamicSubmenu`** — Should be expanded to `Submenu` by the
//!   application *before* passing to [`build_from_model`].  Any unresolved
//!   `DynamicSubmenu` is rendered as an empty placeholder.

use fresh_core::menu::{Menu, MenuContext, MenuItem};
use muda::{
    AboutMetadata, CheckMenuItem, Menu as MudaMenu, MenuEvent, MenuItem as MudaMenuItem,
    PredefinedMenuItem, Submenu,
};
use std::collections::HashMap;

use crate::native_menu::MenuAction;

// ---------------------------------------------------------------------------
// Thread-local tracking state
// ---------------------------------------------------------------------------

/// A tracked native menu item that may need state updates.
enum TrackedItem {
    /// A regular action item that may be enabled/disabled via a `when` condition.
    Regular {
        item: MudaMenuItem,
        when_condition: Option<String>,
    },
    /// A checkbox action item whose checked and enabled states depend on context.
    Check {
        item: CheckMenuItem,
        when_condition: Option<String>,
        checkbox_condition: String,
    },
}

/// A tracked submenu (top-level menu) with a visibility condition.
struct TrackedSubmenu {
    submenu: Submenu,
    when_condition: Option<String>,
}

thread_local! {
    /// Maps muda menu-item IDs → editor actions so we can resolve clicks.
    static ACTION_MAP: std::cell::RefCell<HashMap<muda::MenuId, MenuAction>> =
        std::cell::RefCell::new(HashMap::new());

    /// Items whose state (enabled / checked) depends on `MenuContext`.
    static TRACKED_ITEMS: std::cell::RefCell<Vec<TrackedItem>> =
        std::cell::RefCell::new(Vec::new());

    /// Top-level submenus with `when` conditions (for visibility toggling).
    static TRACKED_SUBMENUS: std::cell::RefCell<Vec<TrackedSubmenu>> =
        std::cell::RefCell::new(Vec::new());
}

/// Clear all thread-local tracking state (called before a full rebuild).
fn clear_tracking() {
    ACTION_MAP.with(|map| map.borrow_mut().clear());
    TRACKED_ITEMS.with(|items| items.borrow_mut().clear());
    TRACKED_SUBMENUS.with(|subs| subs.borrow_mut().clear());
}

/// Register a regular `MudaMenuItem` → `MenuAction` mapping.
fn register_regular(
    item: &MudaMenuItem,
    action: &str,
    args: &HashMap<String, serde_json::Value>,
    when_condition: Option<String>,
) {
    ACTION_MAP.with(|map| {
        map.borrow_mut().insert(
            item.id().clone(),
            MenuAction {
                action: action.to_string(),
                args: args.clone(),
            },
        );
    });
    TRACKED_ITEMS.with(|items| {
        items.borrow_mut().push(TrackedItem::Regular {
            item: item.clone(),
            when_condition,
        });
    });
}

/// Register a `CheckMenuItem` → `MenuAction` mapping.
fn register_check(
    item: &CheckMenuItem,
    action: &str,
    args: &HashMap<String, serde_json::Value>,
    when_condition: Option<String>,
    checkbox_condition: String,
) {
    ACTION_MAP.with(|map| {
        map.borrow_mut().insert(
            item.id().clone(),
            MenuAction {
                action: action.to_string(),
                args: args.clone(),
            },
        );
    });
    TRACKED_ITEMS.with(|items| {
        items.borrow_mut().push(TrackedItem::Check {
            item: item.clone(),
            when_condition,
            checkbox_condition,
        });
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
pub fn build_from_model(menus: &[Menu], app_name: &str, context: &MenuContext) -> MudaMenu {
    clear_tracking();
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
    register_regular(&settings, "open_settings", &HashMap::new(), None);
    let _ = app_submenu.append(&settings);

    let _ = app_submenu.append(&PredefinedMenuItem::separator());
    let _ = app_submenu.append(&PredefinedMenuItem::services(None));
    let _ = app_submenu.append(&PredefinedMenuItem::separator());
    let _ = app_submenu.append(&PredefinedMenuItem::hide(None));
    let _ = app_submenu.append(&PredefinedMenuItem::hide_others(None));
    let _ = app_submenu.append(&PredefinedMenuItem::show_all(None));
    let _ = app_submenu.append(&PredefinedMenuItem::separator());

    let quit = MudaMenuItem::new(&format!("Quit {app_name}"), true, None);
    register_regular(&quit, "quit", &HashMap::new(), None);
    let _ = app_submenu.append(&quit);
    let _ = muda_menu.append(&app_submenu);

    // -- Editor-defined menus -------------------------------------------------
    for menu in menus {
        let visible = match &menu.when {
            Some(condition) => context.get(condition),
            None => true,
        };
        let sub = convert_menu(menu, context);
        TRACKED_SUBMENUS.with(|subs| {
            subs.borrow_mut().push(TrackedSubmenu {
                submenu: sub.clone(),
                when_condition: menu.when.clone(),
            });
        });
        if !visible {
            // Build it but hide it — sync_tracked_items can show it later.
            let _ = sub.set_enabled(false);
        }
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

/// Incrementally update tracked menu item states from the current context.
///
/// This is called each frame (when the context actually changed) and avoids
/// the cost of a full menu rebuild.  It iterates all tracked items and:
/// - Sets enabled/disabled based on `when` conditions
/// - Sets checked/unchecked based on `checkbox` conditions
/// - Shows/hides top-level submenus based on their `when` conditions
pub fn sync_tracked_items(context: &MenuContext) {
    TRACKED_ITEMS.with(|items| {
        for tracked in items.borrow().iter() {
            match tracked {
                TrackedItem::Regular {
                    item,
                    when_condition,
                } => {
                    let enabled = match when_condition.as_deref() {
                        Some(cond) => context.get(cond),
                        None => true,
                    };
                    let _ = item.set_enabled(enabled);
                }
                TrackedItem::Check {
                    item,
                    when_condition,
                    checkbox_condition,
                } => {
                    let enabled = match when_condition.as_deref() {
                        Some(cond) => context.get(cond),
                        None => true,
                    };
                    let _ = item.set_enabled(enabled);
                    let _ = item.set_checked(context.get(checkbox_condition));
                }
            }
        }
    });

    TRACKED_SUBMENUS.with(|subs| {
        for tracked in subs.borrow().iter() {
            if let Some(ref cond) = tracked.when_condition {
                let _ = tracked.submenu.set_enabled(context.get(cond));
            }
        }
    });
}

// ---------------------------------------------------------------------------
// Recursive model → muda conversion
// ---------------------------------------------------------------------------

/// Convert a top-level `Menu` to a `muda::Submenu`.
fn convert_menu(menu: &Menu, context: &MenuContext) -> Submenu {
    let sub = Submenu::new(&menu.label, true);
    for item in &menu.items {
        append_item(&sub, item, context);
    }
    sub
}

/// Append a single `MenuItem` (recursively for submenus) to a `muda::Submenu`.
fn append_item(parent: &Submenu, item: &MenuItem, context: &MenuContext) {
    match item {
        MenuItem::Separator { .. } => {
            let _ = parent.append(&PredefinedMenuItem::separator());
        }

        MenuItem::Action {
            label,
            action,
            args,
            when,
            checkbox,
        } => {
            let enabled = match when.as_deref() {
                Some(cond) => context.get(cond),
                None => true,
            };

            if let Some(checkbox_cond) = checkbox {
                // Checkbox item — use CheckMenuItem.
                let checked = context.get(checkbox_cond);
                let check_item = CheckMenuItem::new(label, enabled, checked, None);
                register_check(
                    &check_item,
                    action,
                    args,
                    when.clone(),
                    checkbox_cond.clone(),
                );
                let _ = parent.append(&check_item);
            } else {
                // Regular action item.
                let muda_item = MudaMenuItem::new(label, enabled, None);
                register_regular(&muda_item, action, args, when.clone());
                let _ = parent.append(&muda_item);
            }
        }

        MenuItem::Submenu { label, items } => {
            let child = Submenu::new(label, true);
            for sub_item in items {
                append_item(&child, sub_item, context);
            }
            let _ = parent.append(&child);
        }

        MenuItem::DynamicSubmenu { label, .. } => {
            // Dynamic submenus should be resolved at the editor layer before
            // being passed here. If one slips through unresolved, show a
            // placeholder.
            tracing::warn!(
                "Unresolved DynamicSubmenu '{}' passed to native menu",
                label
            );
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
