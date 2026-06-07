//! Help-buffer orchestrators.
//!
//! `open_help_manual` and `open_keyboard_shortcuts` create read-only
//! virtual buffers populated with the manual text or keybinding listing.
//! Both check for an existing help buffer first to avoid duplicates.
//! All bodies live on `impl Window` — they operate on this window's
//! buffer storage and use window-side `create_virtual_buffer` +
//! `set_active_buffer`.

use crossterm::event::{KeyCode, KeyModifiers};

use super::help;
use crate::app::window::Window;
use crate::app::Editor;
use crate::input::buffer_mode::BufferMode;
use crate::input::keybindings::{Action, KeyContext};

/// Mode name carried on the help/manual virtual buffers. Referenced
/// both when creating the buffer (so input dispatch resolves keys
/// against this context) and when installing the close binding.
pub(super) const HELP_PANEL_MODE: &str = "special";

impl Editor {
    /// Register the built-in `"special"` buffer mode used by the help
    /// manual and keyboard-shortcuts viewers. Both viewers document
    /// "Press q to close" inline, so we bind `q -> close_tab` here.
    /// `inherit_normal_bindings` keeps cursor motion / copy / search
    /// usable even though `editing_disabled` blocks edits.
    ///
    /// Idempotent: clears any prior binding for this mode context
    /// before installing, so it's safe to call on every viewer open
    /// (also restores the binding after a config reload, which
    /// reinitializes `KeybindingResolver`).
    ///
    /// Avoids calling into `handle_define_mode` because that path
    /// lives under `#[cfg(feature = "plugins")]`; we only need the
    /// keybinding + mode-registry slice here, which is always built.
    pub(super) fn ensure_help_panel_mode_registered(&mut self) {
        let mode_ctx = KeyContext::Mode(HELP_PANEL_MODE.to_string());
        {
            let mut kb = self.keybindings.write().unwrap();
            kb.clear_plugin_defaults_for_mode(HELP_PANEL_MODE);
            kb.set_mode_inherits_normal_bindings(HELP_PANEL_MODE, true);
            kb.load_plugin_default(
                mode_ctx,
                KeyCode::Char('q'),
                KeyModifiers::NONE,
                Action::CloseTab,
            );
        }
        self.mode_registry.register(
            BufferMode::new(HELP_PANEL_MODE)
                .with_read_only(true)
                .with_inherit_normal_bindings(true),
        );
    }
}

impl Window {
    /// Open the built-in help manual in a read-only buffer.
    ///
    /// If a help manual buffer already exists, switch to it instead of
    /// creating a new one.
    pub fn open_help_manual(&mut self) {
        // Check if help buffer already exists.
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::HELP_MANUAL_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            self.set_active_buffer(buffer_id);
            return;
        }

        // Create new help buffer with "special" mode (has 'q' to close).
        let buffer_id = self.create_virtual_buffer(
            help::HELP_MANUAL_BUFFER_NAME.to_string(),
            HELP_PANEL_MODE.to_string(),
            true,
        );

        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, help::HELP_MANUAL_CONTENT);
            state.buffer.clear_modified();
            state.editing_disabled = true;
            state.margins.configure_for_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }

    /// Open the keyboard shortcuts viewer in a read-only buffer.
    ///
    /// If a keyboard shortcuts buffer already exists, switch to it
    /// instead of creating a new one. The shortcuts are dynamically
    /// generated from the current keybindings configuration.
    pub fn open_keyboard_shortcuts(&mut self) {
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::KEYBOARD_SHORTCUTS_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            self.set_active_buffer(buffer_id);
            return;
        }

        // Get all keybindings from this window's resources.
        let bindings = self
            .resources
            .keybindings
            .read()
            .unwrap()
            .get_all_bindings();

        // Format the keybindings as readable text.
        let mut content = String::from("Keyboard Shortcuts\n");
        content.push_str("==================\n\n");
        content.push_str("Press 'q' to close this buffer.\n\n");

        let mut current_context = String::new();
        for (key, action) in &bindings {
            let (context, action_name) = if let Some(bracket_end) = action.find("] ") {
                let ctx = &action[1..bracket_end];
                let name = &action[bracket_end + 2..];
                (ctx.to_string(), name.to_string())
            } else {
                ("Normal".to_string(), action.clone())
            };

            if context != current_context {
                if !current_context.is_empty() {
                    content.push('\n');
                }
                content.push_str(&format!("── {} Mode ──\n\n", context));
                current_context = context;
            }

            content.push_str(&format!("  {:20} {}\n", key, action_name));
        }

        let buffer_id = self.create_virtual_buffer(
            help::KEYBOARD_SHORTCUTS_BUFFER_NAME.to_string(),
            HELP_PANEL_MODE.to_string(),
            true,
        );

        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, &content);
            state.buffer.clear_modified();
            state.editing_disabled = true;
            state.margins.configure_for_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }
}
