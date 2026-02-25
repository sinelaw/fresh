//! Platform-native menu bar abstraction.
//!
//! Provides [`NativeMenuBar`] — a thin wrapper that builds and updates a
//! platform-native menu bar from the editor's [`Menu`] / [`MenuItem`] model.
//!
//! * On **macOS** it delegates to [`super::macos::menu`] which uses the `muda`
//!   crate to drive the AppKit (Cocoa) menu bar.
//! * On **other platforms** it is a no-op stub so the rest of the GUI code
//!   compiles without `#[cfg]` sprinkled everywhere.

use fresh_core::menu::{Menu, MenuContext};
use std::collections::HashMap;

/// An action triggered by a native menu item click.
#[derive(Debug, Clone)]
pub struct MenuAction {
    /// The editor action name (e.g. `"save"`, `"open"`, `"quit"`).
    pub action: String,
    /// Optional action arguments.
    pub args: HashMap<String, serde_json::Value>,
}

// =========================================================================
// macOS implementation (delegates to macos::menu)
// =========================================================================

#[cfg(target_os = "macos")]
pub struct NativeMenuBar {
    /// Keep the muda `Menu` alive — dropping it removes it from the menu bar.
    _menu: muda::Menu,
    /// Last known context — used to avoid redundant state syncs.
    last_context: MenuContext,
}

#[cfg(target_os = "macos")]
impl NativeMenuBar {
    /// Build a native menu bar from the editor's menu model and attach it
    /// to the running NSApplication.
    pub fn build(menus: &[Menu], app_name: &str, context: &MenuContext) -> Self {
        // Install notification observers for menu tracking detection (idempotent-safe:
        // called once, leaks observers intentionally for process lifetime).
        super::macos::menu_tracking::install_tracking_observers();

        let muda_menu = super::macos::menu::build_from_model(menus, app_name, context);
        muda_menu.init_for_nsapp();
        Self {
            _menu: muda_menu,
            last_context: context.clone(),
        }
    }

    /// Returns `true` if the native menu bar is currently being tracked
    /// (user is hovering over menus).  Callers should avoid consuming
    /// pending menu model updates when this returns `true`.
    pub fn is_tracking(&self) -> bool {
        super::macos::menu_tracking::is_menu_tracking()
    }

    /// Rebuild the native menu bar from an updated model.
    pub fn update(&mut self, menus: &[Menu], app_name: &str, context: &MenuContext) {
        self.do_update(menus, app_name, context);
    }

    fn do_update(&mut self, menus: &[Menu], app_name: &str, context: &MenuContext) {
        // Remove old menu from NSApp, build a fresh one.
        self._menu.remove_for_nsapp();
        self._menu = super::macos::menu::build_from_model(menus, app_name, context);
        self._menu.init_for_nsapp();
        self.last_context = context.clone();
    }

    /// Incrementally sync enabled/disabled and checkbox states from the
    /// application's current [`MenuContext`].  This is cheap — it only
    /// iterates tracked items and calls `set_enabled` / `set_checked`
    /// when values differ from the last sync.
    ///
    /// The caller (`about_to_wait`) is responsible for not calling this
    /// while the menu bar is being tracked.
    pub fn sync_state(&mut self, context: &MenuContext) {
        if *context == self.last_context {
            return; // Nothing changed.
        }
        super::macos::menu::sync_tracked_items(context);
        self.last_context = context.clone();
    }

    /// Poll for a pending menu action.  Returns `None` if the user has not
    /// clicked any menu item since the last poll.
    pub fn poll_action(&self) -> Option<MenuAction> {
        match muda::MenuEvent::receiver().try_recv() {
            Ok(event) => super::macos::menu::resolve_menu_event(&event),
            Err(_) => None,
        }
    }
}

// =========================================================================
// Stub implementation for non-macOS platforms
// =========================================================================

#[cfg(not(target_os = "macos"))]
pub struct NativeMenuBar;

#[cfg(not(target_os = "macos"))]
impl NativeMenuBar {
    pub fn build(_menus: &[Menu], _app_name: &str, _context: &MenuContext) -> Self {
        Self
    }

    pub fn is_tracking(&self) -> bool {
        false
    }

    pub fn update(&mut self, _menus: &[Menu], _app_name: &str, _context: &MenuContext) {}

    pub fn sync_state(&mut self, _context: &MenuContext) {}

    pub fn poll_action(&self) -> Option<MenuAction> {
        None
    }
}
