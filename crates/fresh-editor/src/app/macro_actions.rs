//! Macro record & playback orchestrators on `Editor`.
//!
//! Cross-cutting effects — status messages, action replay through
//! `handle_action`, virtual buffer creation for `show_macro_in_buffer` /
//! `list_macros_in_buffer` — for the macro subsystem. Plain data state
//! lives in `super::macros::MacroState`; these methods drive it.

use fresh_i18n::t;

use crate::input::keybindings::Action;
use crate::model::event::EventLog;
use crate::state::EditorState;

use super::macro_codegen::{generate_define_block, generate_promote_block, upsert_macro_block};
use super::types::{BufferKind, BufferMetadata};
use super::Editor;

/// How long replay waits for a plugin step's handler to settle before going
/// on without it. A handler that is only slow still finishes; its effects
/// then just land after the steps that follow it.
#[cfg(feature = "plugins")]
const MACRO_PLUGIN_STEP_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(2);

/// A macro replay in progress (`Editor::macro_replay`).
pub(crate) struct MacroReplay {
    /// The register being played, for the closing status message.
    key: char,
    /// The window whose macro state and undo group the replay holds.
    window: fresh_core::WindowId,
    /// The buffer whose undo group brackets the replay.
    group_buffer: super::BufferId,
    /// Steps not yet run.
    steps: std::collections::VecDeque<Action>,
    total: usize,
    /// The answerable-call id the step being dispatched carries to the
    /// plugin thread; cleared by `handle_action` once it is sent.
    #[cfg(feature = "plugins")]
    pub(super) dispatch_request: Option<u64>,
    /// The plugin step the replay is parked on, if any.
    #[cfg(feature = "plugins")]
    waiting: Option<PluginStepWait>,
}

#[cfg(feature = "plugins")]
struct PluginStepWait {
    /// The id the handler's settlement is reported under.
    request_id: u64,
    /// Whether a prompt was already open when the step ran, so that only a
    /// prompt the handler opened counts as it asking the user.
    prompt_open_at_start: bool,
    since: std::time::Instant,
}

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
    /// Play back a recorded macro.
    ///
    /// Steps run in order, with `recompute_layout` between each so that
    /// visual-line movements (MoveLineEnd, etc.) see up-to-date layout. A step
    /// that runs a plugin handler is dispatched as an answerable call and the
    /// replay waits for the handler to settle before the next step: the
    /// handler runs on the plugin thread, and the step after it in the
    /// recording has to see its effect. While it waits the replay is parked in
    /// `macro_replay` and resumed from the event loop
    /// ([`Self::resume_macro_replay`]). A macro with no plugin steps finishes
    /// inside this call. Drawing is deferred until the next render cycle.
    pub(super) fn play_macro(&mut self, key: char) {
        // Prevent recursive macro playback
        if self.macro_replay.is_some() || self.active_window_mut().macros.is_playing() {
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
        self.macro_replay = Some(MacroReplay {
            key,
            window: self.active_window,
            group_buffer,
            total: actions.len(),
            steps: actions.into(),
            #[cfg(feature = "plugins")]
            dispatch_request: None,
            #[cfg(feature = "plugins")]
            waiting: None,
        });
        self.run_macro_replay();
    }

    /// Run replay steps until the macro ends or a step parks it on a plugin
    /// handler.
    fn run_macro_replay(&mut self) {
        loop {
            let Some(replay) = self.macro_replay.as_mut() else {
                return;
            };
            let Some(action) = replay.steps.pop_front() else {
                self.finish_macro_replay();
                return;
            };
            // The `PluginAction` arm of `handle_action` dispatches with this id
            // and clears it once the handler is on its way.
            #[cfg(feature = "plugins")]
            let request_id = matches!(action, Action::PluginAction(_)).then(|| {
                let id = crate::server::command_access::next_request_id();
                replay.dispatch_request = Some(id);
                id
            });

            if let Err(e) = self.handle_action(action) {
                tracing::warn!("Macro action failed: {}", e);
            }
            let width = self.active_chrome().last_frame.width;
            let height = self.active_chrome().last_frame.height;
            self.recompute_layout(width, height);

            #[cfg(feature = "plugins")]
            if let Some(request_id) = request_id {
                let prompt_open_at_start = self.active_window().prompt.is_some();
                let since = self.time_source.now();
                let Some(replay) = self.macro_replay.as_mut() else {
                    return;
                };
                // Still set means the step never reached the plugin thread
                // (no plugin manager, or the dispatch failed): nothing to wait on.
                if replay.dispatch_request.take().is_none() {
                    replay.waiting = Some(PluginStepWait {
                        request_id,
                        prompt_open_at_start,
                        since,
                    });
                    return;
                }
            }
        }
    }

    /// Resume a replay parked on a plugin step, once the step's handler has
    /// settled, has stopped to ask the user something, or has run out of time.
    ///
    /// The second case matters because the user's answer is usually the next
    /// step of the recording: a handler that opened a prompt settles only once
    /// a later step confirms it, so waiting for it to settle would stall the
    /// replay until the time limit. Called once per event-loop pass; returns
    /// whether the replay moved.
    #[cfg(feature = "plugins")]
    pub(super) fn resume_macro_replay(&mut self) -> bool {
        let Some(wait) = self.macro_replay.as_ref().and_then(|r| r.waiting.as_ref()) else {
            return false;
        };
        let (request_id, prompt_open_at_start, since) =
            (wait.request_id, wait.prompt_open_at_start, wait.since);

        let outcomes = crate::server::command_access::take_completed_where(|id| id == request_id);
        for failed in outcomes.iter().filter(|o| !o.ok) {
            tracing::warn!(
                "macro replay: plugin step failed: {}",
                failed.error.as_deref().unwrap_or("unknown error")
            );
        }
        let settled = !outcomes.is_empty();
        let window = self.active_window();
        let asks_user = (window.prompt.is_some() && !prompt_open_at_start)
            || !window.pending_next_key_callbacks.is_empty();
        let timed_out =
            self.time_source.now().saturating_duration_since(since) >= MACRO_PLUGIN_STEP_TIMEOUT;
        if !(settled || asks_user || timed_out) {
            return false;
        }
        if !settled && !asks_user {
            tracing::warn!(
                "macro replay: plugin step did not settle within {:?}; continuing",
                MACRO_PLUGIN_STEP_TIMEOUT
            );
        }

        if let Some(replay) = self.macro_replay.as_mut() {
            replay.waiting = None;
        }
        self.run_macro_replay();
        true
    }

    /// Close the replay's undo group and report it played.
    fn finish_macro_replay(&mut self) {
        let Some(replay) = self.macro_replay.take() else {
            return;
        };
        if let Some(window) = self.windows.get_mut(&replay.window) {
            if let Some(log) = window.event_logs.get_mut(&replay.group_buffer) {
                log.end_undo_group();
            }
            window.macros.end_play();
        }
        self.set_status_message(
            t!("macro.played", key = replay.key, count = replay.total).to_string(),
        );
    }

    /// Run `f` without recording what it dispatches into an in-flight macro.
    ///
    /// For actions that are a plugin's doing rather than the user's input: a
    /// timer tick, or the actions a handler issues through
    /// `editor.executeAction(s)`. The key that ran the handler is what gets
    /// recorded, and replaying it issues those actions again.
    #[cfg(feature = "plugins")]
    pub(super) fn without_macro_recording<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let was_suppressed = std::mem::replace(&mut self.suppress_macro_recording, true);
        let result = f(self);
        self.suppress_macro_recording = was_suppressed;
        result
    }

    /// Record an action to the current macro (if recording).
    ///
    /// PromptConfirm is special-cased here because the action itself doesn't
    /// carry the prompt text — we must snapshot the text now so replay gets
    /// the user's original input rather than whatever the prompt happens to
    /// contain at replay time. Everything else is forwarded unchanged to the
    /// subsystem, which applies its own control-action filter.
    pub(super) fn record_macro_action(&mut self, action: &Action) {
        #[cfg(feature = "plugins")]
        if self.suppress_macro_recording {
            return;
        }
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
                // Render as `ActionSpec[]` — the same vocabulary `executeActions`
                // and the generated `init.ts` blocks use, and more readable than
                // the raw `Action` serde form.
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

        // Create header with macro info. This is a read-only view; to persist
        // or edit a macro, run "Macro: Save to init.ts".
        let content = format!(
            "// Macro '{}' ({} actions) — ActionSpec[] view\n// Run \"Macro: Save to init.ts\" to persist this macro as editable code.\n\n{}",
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
}
