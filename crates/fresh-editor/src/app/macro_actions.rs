//! Macro record & playback orchestrators on `Editor`.
//!
//! Cross-cutting effects — status messages, action replay through
//! `handle_action`, virtual buffer creation for `show_macro_in_buffer` /
//! `list_macros_in_buffer` — for the macro subsystem. Plain data state
//! lives in `super::macros::MacroState`; these methods drive it.

use rust_i18n::t;

use crate::input::keybindings::Action;
use crate::model::event::EventLog;
use crate::state::EditorState;

use super::macro_codegen::{generate_define_block, generate_promote_block, upsert_macro_block};
use super::types::{BufferKind, BufferMetadata};
use super::Editor;

impl Editor {
    /// Toggle macro recording for the given register
    pub(super) fn toggle_macro_recording(&mut self, key: char) {
        match self.active_window_mut().macros.recording_key() {
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
        self.active_window_mut().macros.start_recording(key);

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
        let Some((key, action_count)) = self.active_window_mut().macros.stop_recording() else {
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
        if self.active_window_mut().macros.is_playing() {
            return;
        }

        let Some(actions) = self.active_window_mut().macros.get(key).map(<[_]>::to_vec) else {
            self.set_status_message(t!("macro.not_found", key = key).to_string());
            return;
        };
        if actions.is_empty() {
            self.set_status_message(t!("macro.empty", key = key).to_string());
            return;
        }

        self.active_window_mut().macros.begin_play();
        // Bracket the replay so the whole macro is a single undo unit: one Undo
        // reverts the entire playback (and one Redo re-applies it) rather than
        // one event per replayed write action. The group is opened and closed
        // on the same buffer's log even if the macro switches buffers mid-replay.
        let group_buffer = self.active_buffer();
        if let Some(log) = self.active_window_mut().event_logs.get_mut(&group_buffer) {
            log.begin_undo_group();
        }
        let action_count = actions.len();
        let width = self.active_chrome().last_frame_width;
        let height = self.active_chrome().last_frame_height;
        for action in actions {
            if let Err(e) = self.handle_action(action) {
                tracing::warn!("Macro action failed: {}", e);
            }
            self.recompute_layout(width, height);
        }
        if let Some(log) = self.active_window_mut().event_logs.get_mut(&group_buffer) {
            log.end_undo_group();
        }
        self.active_window_mut().macros.end_play();

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
            if let Some(prompt) = &self.active_window_mut().prompt {
                let text = prompt.get_text().to_string();
                self.active_window_mut()
                    .macros
                    .record_transformed(Action::PromptConfirmWithText(text));
                return;
            }
        }
        self.active_window_mut().macros.record_if_recording(action);
    }

    /// Show a macro in a buffer as JSON
    pub(super) fn show_macro_in_buffer(&mut self, key: char) {
        // Get macro data and cache what we need before any mutable borrows
        let (json, actions_len) = match self.active_window_mut().macros.get(key) {
            Some(actions) => {
                // Render as `ActionSpec[]` — the canonical, *loadable* form that
                // `executeActions` consumes and "Macro: Load from buffer" parses
                // back. (Not the raw `Action` serde form, which doesn't round-trip.)
                let specs: Vec<fresh_core::api::ActionSpec> =
                    actions.iter().map(|a| a.to_action_spec()).collect();
                let json = match serde_json::to_string_pretty(&specs) {
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

        // Create header with macro info. The body is an editable ActionSpec
        // array: tweak it, then run "Macro: Load from buffer" to store it back
        // into a register.
        let content = format!(
            "// Macro '{}' ({} actions) — editable ActionSpec[]\n// Edit, then run \"Macro: Load from buffer\" to store it into a register.\n\n{}",
            key,
            actions_len,
            json
        );

        // Create a new buffer for the macro
        let buffer_id = self.alloc_buffer_id();

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.authority().filesystem),
        );
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .insert(buffer_id, state);
        self.active_window_mut()
            .event_logs
            .insert(buffer_id, EventLog::new());

        // Set buffer content
        let fs = std::sync::Arc::clone(&self.authority().filesystem);
        let threshold = self.config.editor.large_file_threshold_bytes as usize;
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
            state.buffer = crate::model::buffer::Buffer::from_str(&content, threshold, fs);
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
            auto_revert_enabled: true,
            synthetic_placeholder: false,
            recovery_id: None,
        };
        self.active_window_mut()
            .buffer_metadata
            .insert(buffer_id, metadata);

        // Switch to the new buffer
        self.set_active_buffer(buffer_id);
        self.set_status_message(
            t!("macro.shown_buffer", key = key, count = actions_len).to_string(),
        );
    }

    /// List all recorded macros in a buffer
    pub(super) fn list_macros_in_buffer(&mut self) {
        if self.active_window_mut().macros.is_empty() {
            self.set_status_message(t!("macro.none_recorded").to_string());
            return;
        }

        // Build a summary of all macros
        let mut content =
            String::from("// Recorded Macros\n// Use ShowMacro(key) to see details\n\n");

        for key in self.active_window_mut().macros.keys_sorted() {
            if let Some(actions) = self.active_window_mut().macros.get(key) {
                content.push_str(&format!("Macro '{}': {} actions\n", key, actions.len()));

                // Show all actions
                for (i, action) in actions.iter().enumerate() {
                    content.push_str(&format!("  {}. {:?}\n", i + 1, action));
                }
                content.push('\n');
            }
        }

        // Create a new buffer for the macro list
        let buffer_id = self.alloc_buffer_id();

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.authority().filesystem),
        );
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .insert(buffer_id, state);
        self.active_window_mut()
            .event_logs
            .insert(buffer_id, EventLog::new());

        // Set buffer content
        let fs = std::sync::Arc::clone(&self.authority().filesystem);
        let threshold = self.config.editor.large_file_threshold_bytes as usize;
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
            state.buffer = crate::model::buffer::Buffer::from_str(&content, threshold, fs);
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
            auto_revert_enabled: true,
            synthetic_placeholder: false,
            recovery_id: None,
        };
        self.active_window_mut()
            .buffer_metadata
            .insert(buffer_id, metadata);

        // Switch to the new buffer
        self.set_active_buffer(buffer_id);
        let count = self.active_window().macros.count();
        self.set_status_message(t!("macro.showing", count = count).to_string());
    }

    /// Append register `key`'s recorded macro to `init.ts` as an editable
    /// `editor.defineMacro(...)` block, then hot-reload init.ts so it takes
    /// effect immediately. This is the persistence path: the macro survives
    /// restarts and becomes hand-editable TypeScript in a file the user owns.
    pub(super) fn save_macro_to_init(&mut self, key: char) {
        self.write_macro_to_init(key, false);
    }

    /// Append register `key`'s recorded macro to `init.ts` as an editable
    /// `registerHandler` / `registerCommand` stub — the "promote to arbitrary
    /// code" path. The recorded steps become an ordinary `executeActions` call
    /// inside a real function the user can extend with loops, conditionals, and
    /// the full plugin API. Hot-reloads init.ts when done.
    pub(super) fn promote_macro_to_command(&mut self, key: char) {
        self.write_macro_to_init(key, true);
    }

    /// Shared body for save/promote: render the macro into the requested form,
    /// upsert its sentinel-delimited block into `init.ts`, write, and reload.
    fn write_macro_to_init(&mut self, key: char, promote: bool) {
        let actions = match self.active_window().macros.get(key) {
            Some(actions) if !actions.is_empty() => actions.to_vec(),
            Some(_) => {
                self.set_status_message(t!("macro.empty", key = key).to_string());
                return;
            }
            None => {
                self.set_status_message(t!("macro.not_found", key = key).to_string());
                return;
            }
        };

        let block = if promote {
            generate_promote_block(key, &actions)
        } else {
            generate_define_block(key, &actions)
        };

        let config_dir = self.dir_context.config_dir.clone();
        // Ensure init.ts (and the type scaffolding its `/// <reference>`s need)
        // exist before we append to it.
        let path = match crate::init_script::ensure_starter(&config_dir) {
            Ok(p) => p,
            Err(e) => {
                self.set_status_message(
                    t!("macro.init_write_failed", error = e.to_string()).to_string(),
                );
                return;
            }
        };

        let existing = std::fs::read_to_string(&path).unwrap_or_default();
        let updated = upsert_macro_block(&existing, key, &block);
        if let Err(e) = std::fs::write(&path, updated) {
            self.set_status_message(
                t!("macro.init_write_failed", error = e.to_string()).to_string(),
            );
            return;
        }

        // Hot-reload init.ts so the macro is live now — mirrors Action::InitReload.
        self.load_init_script(true);
        self.fire_plugins_loaded_hook();

        let msg = if promote {
            t!("macro.promoted_to_init", key = key)
        } else {
            t!("macro.saved_to_init", key = key)
        };
        self.set_status_message(msg.to_string());
    }

    /// Parse the active buffer as an `ActionSpec[]` JSON array (e.g. a
    /// `ShowMacro` buffer the user tweaked) and store it under register `key`.
    /// The inverse of [`Self::show_macro_in_buffer`] — together they give a
    /// lightweight "edit a macro and re-run it" loop without touching init.ts.
    pub(super) fn load_macro_from_active_buffer(&mut self, key: char) {
        let Some(text) = self.active_state().buffer.to_string() else {
            self.set_status_message(t!("macro.buffer_unreadable").to_string());
            return;
        };

        let specs = match parse_action_specs(&text) {
            Ok(specs) => specs,
            Err(e) => {
                self.set_status_message(t!("macro.load_parse_failed", error = e).to_string());
                return;
            }
        };

        let mut actions = Vec::with_capacity(specs.len());
        for spec in &specs {
            if let Some(action) = Action::from_str(&spec.action, &spec.args) {
                for _ in 0..spec.count.max(1) {
                    actions.push(action.clone());
                }
            }
        }

        let count = actions.len();
        self.active_window_mut().macros.define(key, actions);
        self.set_status_message(
            t!("macro.loaded_from_buffer", key = key, count = count).to_string(),
        );
    }
}

/// Extract the `[ ... ]` JSON array from `text` (tolerating leading comment
/// lines, as a `ShowMacro` buffer has) and parse it as `Vec<ActionSpec>`.
fn parse_action_specs(text: &str) -> Result<Vec<fresh_core::api::ActionSpec>, String> {
    let start = text
        .find('[')
        .ok_or_else(|| "no '[' found — expected an ActionSpec array".to_string())?;
    let end = text
        .rfind(']')
        .ok_or_else(|| "no ']' found — expected an ActionSpec array".to_string())?;
    if end < start {
        return Err("malformed array brackets".to_string());
    }
    serde_json::from_str::<Vec<fresh_core::api::ActionSpec>>(&text[start..=end])
        .map_err(|e| e.to_string())
}
