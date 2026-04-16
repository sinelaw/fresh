//! Help-buffer orchestrators on `Editor`.
//!
//! `open_help_manual` and `open_keyboard_shortcuts` create read-only
//! virtual buffers populated with the manual text or keybinding listing.
//! Both check for an existing help buffer first to avoid duplicates.

use rust_i18n::t;

use crate::model::event::{BufferId, EventLog};
use crate::state::EditorState;

use super::help;
use super::Editor;

impl Editor {
    /// Open the built-in help manual in a read-only buffer
    ///
    /// If a help manual buffer already exists, switch to it instead of creating a new one.
    pub fn open_help_manual(&mut self) {
        // Check if help buffer already exists
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::HELP_MANUAL_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            // Switch to existing help buffer
            self.set_active_buffer(buffer_id);
            return;
        }

        // Create new help buffer with "special" mode (has 'q' to close)
        let buffer_id = self.create_virtual_buffer(
            help::HELP_MANUAL_BUFFER_NAME.to_string(),
            "special".to_string(),
            true,
        );

        // Set the content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, help::HELP_MANUAL_CONTENT);
            state.buffer.clear_modified();
            state.editing_disabled = true;

            // Disable line numbers for cleaner display
            state.margins.configure_for_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }

    /// Open the keyboard shortcuts viewer in a read-only buffer
    ///
    /// If a keyboard shortcuts buffer already exists, switch to it instead of creating a new one.
    /// The shortcuts are dynamically generated from the current keybindings configuration.
    pub fn open_keyboard_shortcuts(&mut self) {
        // Check if keyboard shortcuts buffer already exists
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::KEYBOARD_SHORTCUTS_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            // Switch to existing buffer
            self.set_active_buffer(buffer_id);
            return;
        }

        // Get all keybindings
        let bindings = self.keybindings.read().unwrap().get_all_bindings();

        // Format the keybindings as readable text
        let mut content = String::from("Keyboard Shortcuts\n");
        content.push_str("==================\n\n");
        content.push_str("Press 'q' to close this buffer.\n\n");

        // Group bindings by context (Normal, Prompt, etc.)
        let mut current_context = String::new();
        for (key, action) in &bindings {
            // Check if action starts with a context prefix like "[Prompt] "
            let (context, action_name) = if let Some(bracket_end) = action.find("] ") {
                let ctx = &action[1..bracket_end];
                let name = &action[bracket_end + 2..];
                (ctx.to_string(), name.to_string())
            } else {
                ("Normal".to_string(), action.clone())
            };

            // Print context header when it changes
            if context != current_context {
                if !current_context.is_empty() {
                    content.push('\n');
                }
                content.push_str(&format!("── {} Mode ──\n\n", context));
                current_context = context;
            }

            // Format: "  Ctrl+S          Save"
            content.push_str(&format!("  {:20} {}\n", key, action_name));
        }

        // Create new keyboard shortcuts buffer with "special" mode (has 'q' to close)
        let buffer_id = self.create_virtual_buffer(
            help::KEYBOARD_SHORTCUTS_BUFFER_NAME.to_string(),
            "special".to_string(),
            true,
        );

        // Set the content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, &content);
            state.buffer.clear_modified();
            state.editing_disabled = true;

            // Disable line numbers for cleaner display
            state.margins.configure_for_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }
}
