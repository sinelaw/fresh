//! View mode action handlers.
//!
//! This module contains handlers for view-related actions like compose mode toggling.

use super::Editor;
use crate::state::ViewMode;
use rust_i18n::t;

impl Editor {
    /// Toggle between Compose and Source view modes.
    pub fn handle_toggle_compose_mode(&mut self) {
        let default_wrap = self.config.editor.line_wrap;
        let default_line_numbers = self.config.editor.line_numbers;
        let active_split = self.split_manager.active_split();

        let view_mode = {
            let current = self
                .split_view_states
                .get(&active_split)
                .map(|vs| vs.view_mode.clone())
                .unwrap_or(ViewMode::Source);
            match current {
                ViewMode::Compose => ViewMode::Source,
                _ => ViewMode::Compose,
            }
        };

        // Update split view state (source of truth for view mode and line numbers)
        if let Some(vs) = self.split_view_states.get_mut(&active_split) {
            // Save the current per-split line number state before switching
            let current_line_numbers = vs.show_line_numbers;
            vs.view_mode = view_mode.clone();
            // In Compose mode, disable builtin line wrap - the plugin handles
            // wrapping by inserting Break tokens in the view transform pipeline.
            // In Source mode, respect the user's default_wrap preference.
            vs.viewport.line_wrap_enabled = match view_mode {
                ViewMode::Compose => false,
                ViewMode::Source => default_wrap,
            };
            match view_mode {
                ViewMode::Compose => {
                    vs.compose_prev_line_numbers = Some(current_line_numbers);
                    vs.show_line_numbers = false;
                }
                ViewMode::Source => {
                    // Clear compose width to remove margins
                    vs.compose_width = None;
                    vs.view_transform = None;
                    let restore = vs
                        .compose_prev_line_numbers
                        .take()
                        .unwrap_or(default_line_numbers);
                    vs.show_line_numbers = restore;
                }
            }
        }

        let mode_label = match view_mode {
            ViewMode::Compose => t!("view.compose").to_string(),
            ViewMode::Source => "Source".to_string(),
        };
        self.set_status_message(t!("view.mode", mode = mode_label).to_string());
    }
}
