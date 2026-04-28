//! Macro record & playback orchestrators on `Editor`.
//!
//! Cross-cutting effects — status messages, action replay through
//! `handle_action`, virtual buffer creation for `show_macro_in_buffer` /
//! `list_macros_in_buffer` — for the macro subsystem. Plain data state
//! lives in `super::macros::MacroState`; these methods drive it.

use rust_i18n::t;

use crate::input::keybindings::Action;
use crate::model::event::{BufferId, EventLog};
use crate::state::EditorState;

use super::types::{BufferKind, BufferMetadata};
use super::Editor;

impl Editor {
    /// Toggle macro recording for the given register
    pub(super) fn toggle_macro_recording(&mut self, key: char) {
        match self.macros.recording_key() {
            Some(k) if k == key => self.stop_macro_recording(),
            Some(_) => {
                self.stop_macro_recording();
                self.start_macro_recording(key);
            }
            None => self.start_macro_recording(key),
        }
    }

    /// Start recording a macro
    pub(super) fn start_macro_recording(&mut self, key: char) {
        self.macros.start_recording(key);

        // Build the stop hint dynamically from keybindings
        let stop_hint = self.build_macro_stop_hint(key);
        self.set_status_message(
            t!(
                "macro.recording_with_hint",
                key = key,
                stop_hint = stop_hint
            )
            .to_string(),
        );
    }

    /// Build a hint message for how to stop macro recording
    fn build_macro_stop_hint(&self, _key: char) -> String {
        let mut hints = Vec::new();

        // Check for F5 (stop_macro_recording)
        if let Some(stop_key) = self.get_keybinding_for_action("stop_macro_recording") {
            hints.push(stop_key);
        }

        // Get command palette keybinding
        let palette_key = self
            .get_keybinding_for_action("command_palette")
            .unwrap_or_else(|| "Ctrl+P".to_string());

        if hints.is_empty() {
            // No keybindings found, just mention command palette
            format!("{} → Stop Recording Macro", palette_key)
        } else {
            // Show keybindings and command palette
            format!("{} or {} → Stop Recording", hints.join("/"), palette_key)
        }
    }

    /// Stop recording and save the macro
    pub(super) fn stop_macro_recording(&mut self) {
        let Some((key, action_count)) = self.macros.stop_recording() else {
            self.set_status_message(t!("macro.not_recording").to_string());
            return;
        };

        let play_hint = self.build_macro_play_hint();
        self.set_status_message(
            t!(
                "macro.saved",
                key = key,
                count = action_count,
                play_hint = play_hint
            )
            .to_string(),
        );
    }

    /// Build a hint message for how to play a macro
    fn build_macro_play_hint(&self) -> String {
        // Check for play_last_macro keybinding (e.g. F4)
        if let Some(play_key) = self.get_keybinding_for_action("play_last_macro") {
            return format!("{} → Play Last Macro", play_key);
        }

        // Fall back to command palette hint
        let palette_key = self
            .get_keybinding_for_action("command_palette")
            .unwrap_or_else(|| "Ctrl+P".to_string());

        format!("{} → Play Macro", palette_key)
    }
    /// Play back a recorded macro synchronously.
    ///
    /// All actions are executed in a tight loop. Between each action,
    /// `recompute_layout` is called so that visual-line movements
    /// (MoveLineEnd, etc.) see correct, up-to-date layout information.
    /// Drawing is deferred until the next render cycle.
    pub(super) fn play_macro(&mut self, key: char) {
        // Prevent recursive macro playback
        if self.macros.is_playing() {
            return;
        }

        let Some(actions) = self.macros.get(key).map(<[_]>::to_vec) else {
            self.set_status_message(t!("macro.not_found", key = key).to_string());
            return;
        };
        if actions.is_empty() {
            self.set_status_message(t!("macro.empty", key = key).to_string());
            return;
        }

        self.macros.begin_play();
        let action_count = actions.len();
        let width = self.cached_layout.last_frame_width;
        let height = self.cached_layout.last_frame_height;
        for action in actions {
            if let Err(e) = self.handle_action(action) {
                tracing::warn!("Macro action failed: {}", e);
            }
            self.recompute_layout(width, height);
        }
        self.macros.end_play();

        self.set_status_message(t!("macro.played", key = key, count = action_count).to_string());
    }

    /// Record an action to the current macro (if recording).
    ///
    /// PromptConfirm is special-cased here because the action itself doesn't
    /// carry the prompt text — we must snapshot the text now so replay gets
    /// the user's original input rather than whatever the prompt happens to
    /// contain at replay time. Everything else is forwarded unchanged to the
    /// subsystem, which applies its own control-action filter.
    pub(super) fn record_macro_action(&mut self, action: &Action) {
        if let Action::PromptConfirm = action {
            if let Some(prompt) = &self.prompt {
                let text = prompt.get_text().to_string();
                self.macros
                    .record_transformed(Action::PromptConfirmWithText(text));
                return;
            }
        }
        self.macros.record_if_recording(action);
    }

    /// Show a macro in a buffer as JSON
    pub(super) fn show_macro_in_buffer(&mut self, key: char) {
        // Get macro data and cache what we need before any mutable borrows
        let (json, actions_len) = match self.macros.get(key) {
            Some(actions) => {
                let json = match serde_json::to_string_pretty(actions) {
                    Ok(json) => json,
                    Err(e) => {
                        self.set_status_message(
                            t!("macro.serialize_failed", error = e.to_string()).to_string(),
                        );
                        return;
                    }
                };
                (json, actions.len())
            }
            None => {
                self.set_status_message(t!("macro.not_found", key = key).to_string());
                return;
            }
        };

        // Create header with macro info
        let content = format!(
            "// Macro '{}' ({} actions)\n// This buffer can be saved as a .json file for persistence\n\n{}",
            key,
            actions_len,
            json
        );

        // Create a new buffer for the macro
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.authority.filesystem),
        );
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Set buffer content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer = crate::model::buffer::Buffer::from_str(
                &content,
                self.config.editor.large_file_threshold_bytes as usize,
                std::sync::Arc::clone(&self.authority.filesystem),
            );
        }

        // Set metadata
        let metadata = BufferMetadata {
            kind: BufferKind::Virtual {
                mode: "macro-view".to_string(),
            },
            display_name: format!("*Macro {}*", key),
            lsp_enabled: false,
            lsp_disabled_reason: Some("Virtual macro buffer".to_string()),
            read_only: false, // Allow editing for saving
            binary: false,
            lsp_opened_with: std::collections::HashSet::new(),
            hidden_from_tabs: false,
            synthetic_placeholder: false,
            is_preview: false,
            recovery_id: None,
        };
        self.buffer_metadata.insert(buffer_id, metadata);

        // Switch to the new buffer
        self.set_active_buffer(buffer_id);
        self.set_status_message(
            t!("macro.shown_buffer", key = key, count = actions_len).to_string(),
        );
    }

    /// List all recorded macros in a buffer
    pub(super) fn list_macros_in_buffer(&mut self) {
        if self.macros.is_empty() {
            self.set_status_message(t!("macro.none_recorded").to_string());
            return;
        }

        // Build a summary of all macros
        let mut content =
            String::from("// Recorded Macros\n// Use ShowMacro(key) to see details\n\n");

        for key in self.macros.keys_sorted() {
            if let Some(actions) = self.macros.get(key) {
                content.push_str(&format!("Macro '{}': {} actions\n", key, actions.len()));

                // Show all actions
                for (i, action) in actions.iter().enumerate() {
                    content.push_str(&format!("  {}. {:?}\n", i + 1, action));
                }
                content.push('\n');
            }
        }

        // Create a new buffer for the macro list
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.authority.filesystem),
        );
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Set buffer content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer = crate::model::buffer::Buffer::from_str(
                &content,
                self.config.editor.large_file_threshold_bytes as usize,
                std::sync::Arc::clone(&self.authority.filesystem),
            );
        }

        // Set metadata
        let metadata = BufferMetadata {
            kind: BufferKind::Virtual {
                mode: "macro-list".to_string(),
            },
            display_name: "*Macros*".to_string(),
            lsp_enabled: false,
            lsp_disabled_reason: Some("Virtual macro list buffer".to_string()),
            read_only: true,
            binary: false,
            lsp_opened_with: std::collections::HashSet::new(),
            hidden_from_tabs: false,
            synthetic_placeholder: false,
            is_preview: false,
            recovery_id: None,
        };
        self.buffer_metadata.insert(buffer_id, metadata);

        // Switch to the new buffer
        self.set_active_buffer(buffer_id);
        self.set_status_message(t!("macro.showing", count = self.macros.count()).to_string());
    }
}
