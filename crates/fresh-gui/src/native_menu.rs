//! Platform-native menu bar abstraction.
//!
//! Provides [`NativeMenuBar`] — a thin wrapper that builds and updates a
//! platform-native menu bar from the editor's [`Menu`] / [`MenuItem`] model.
//!
//! * On **macOS** it delegates to [`super::macos::menu`] which uses the `muda`
//!   crate to drive the AppKit (Cocoa) menu bar.
//! * On **other platforms** it is a no-op stub so the rest of the GUI code
//!   compiles without `#[cfg]` sprinkled everywhere.

use fresh_core::menu::Menu;
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
}

#[cfg(target_os = "macos")]
impl NativeMenuBar {
    /// Build a native menu bar from the editor's menu model and attach it
    /// to the running NSApplication.
    pub fn build(menus: &[Menu], app_name: &str) -> Self {
        let muda_menu = super::macos::menu::build_from_model(menus, app_name);
        muda_menu.init_for_nsapp();
        Self { _menu: muda_menu }
    }

    /// Rebuild the native menu bar from an updated model.
    pub fn update(&mut self, menus: &[Menu], app_name: &str) {
        // Remove old menu from NSApp, build a fresh one.
        self._menu.remove_for_nsapp();
        self._menu = super::macos::menu::build_from_model(menus, app_name);
        self._menu.init_for_nsapp();
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
    pub fn build(_menus: &[Menu], _app_name: &str) -> Self {
        Self
    }

    pub fn update(&mut self, _menus: &[Menu], _app_name: &str) {}

    pub fn poll_action(&self) -> Option<MenuAction> {
        None
    }
}
