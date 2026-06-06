//! Editor-lifecycle methods: quit, restart, session/detach control,
//! focus/resize hooks, theme/settings queries, escape-sequence + clipboard
//! piping, and the should_quit confirmation flow that walks modified buffers.

use super::*;

impl Editor {
    /// Check if the editor should quit
    pub fn should_quit(&self) -> bool {
        self.should_quit
    }

    /// Check if the client should detach (keep server running)
    pub fn should_detach(&self) -> bool {
        self.should_detach
    }

    /// Clear the detach flag (after processing)
    pub fn clear_detach(&mut self) {
        self.should_detach = false;
    }

    /// Set session mode (use hardware cursor only, no REVERSED style for software cursor)
    pub fn set_session_mode(&mut self, session_mode: bool) {
        self.session_mode = session_mode;
        self.clipboard.set_session_mode(session_mode);
        // Also set custom context for command palette filtering
        if session_mode {
            self.active_window_mut()
                .active_custom_contexts
                .insert(crate::types::context_keys::SESSION_MODE.to_string());
        } else {
            self.active_window_mut()
                .active_custom_contexts
                .remove(crate::types::context_keys::SESSION_MODE);
        }
    }

    /// Check if running in session mode
    pub fn is_session_mode(&self) -> bool {
        self.session_mode
    }

    /// Mark that the backend does not render a hardware cursor.
    /// When set, the renderer always draws a software cursor indicator.
    pub fn set_software_cursor_only(&mut self, enabled: bool) {
        self.software_cursor_only = enabled;
    }

    /// Set the session name for display in status bar.
    ///
    /// When a session name is set, the recovery service is reinitialized
    /// to use a session-scoped recovery directory so each named session's
    /// recovery data is isolated.
    pub fn set_session_name(&mut self, name: Option<String>) {
        if let Some(ref session_name) = name {
            let base_recovery_dir = self.dir_context.recovery_dir();
            let scope = crate::services::recovery::RecoveryScope::Session {
                name: session_name.clone(),
            };
            let recovery_config = RecoveryConfig {
                enabled: self.recovery_service.lock().unwrap().is_enabled(),
                ..RecoveryConfig::default()
            };
            // Replace the shared service's contents in place — the
            // `Arc<Mutex>` is cloned into every window, so we must not
            // swap the `Arc` itself (that would desync the windows).
            *self.recovery_service.lock().unwrap() =
                RecoveryService::with_scope(recovery_config, &base_recovery_dir, &scope);
        }
        self.session_name = name;
    }

    /// Get the session name (for status bar display)
    pub fn session_name(&self) -> Option<&str> {
        self.session_name.as_deref()
    }

    /// Queue escape sequences to be sent to the client (session mode only)
    pub fn queue_escape_sequences(&mut self, sequences: &[u8]) {
        self.pending_escape_sequences.extend_from_slice(sequences);
    }

    /// Take pending escape sequences, clearing the queue
    pub fn take_pending_escape_sequences(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.pending_escape_sequences)
    }

    /// Take pending clipboard data queued in session mode, clearing the request
    pub fn take_pending_clipboard(
        &mut self,
    ) -> Option<crate::services::clipboard::PendingClipboard> {
        self.clipboard.take_pending_clipboard()
    }

    /// Check if the editor should restart with a new working directory
    pub fn should_restart(&self) -> bool {
        self.restart_with_dir.is_some()
    }

    /// Take the restart directory, clearing the restart request
    /// Returns the new working directory if a restart was requested
    pub fn take_restart_dir(&mut self) -> Option<PathBuf> {
        self.restart_with_dir.take()
    }

    /// Request the editor to restart with a new working directory
    /// This triggers a clean shutdown and restart with the new project root
    /// Request a full hardware terminal clear and redraw on the next frame.
    /// Used after external commands have messed up the terminal state.
    pub fn request_full_redraw(&mut self) {
        self.full_redraw_requested = true;
    }

    /// Check if a full redraw was requested, and clear the flag.
    pub fn take_full_redraw_request(&mut self) -> bool {
        let requested = self.full_redraw_requested;
        self.full_redraw_requested = false;
        requested
    }

    /// Request the event loop to suspend the editor process (SIGTSTP on Unix).
    /// The loop tears down terminal modes, raises the signal, then re-enables
    /// modes once the shell sends SIGCONT (e.g. via `fg`).
    pub fn request_suspend(&mut self) {
        self.suspend_requested = true;
    }

    /// Check if a suspend was requested, and clear the flag.
    pub fn take_suspend_request(&mut self) -> bool {
        let requested = self.suspend_requested;
        self.suspend_requested = false;
        requested
    }

    pub fn request_restart(&mut self, new_working_dir: PathBuf) {
        tracing::info!(
            "Restart requested with new working directory: {}",
            new_working_dir.display()
        );
        self.restart_with_dir = Some(new_working_dir);
        // Also signal quit so the event loop exits
        self.should_quit = true;
    }

    /// Get the active theme (read lock).
    pub fn theme(&self) -> std::sync::RwLockReadGuard<'_, crate::view::theme::Theme> {
        self.theme.read().unwrap()
    }

    /// Check if the settings dialog is open and visible
    pub fn is_settings_open(&self) -> bool {
        self.settings_state.as_ref().is_some_and(|s| s.visible)
    }

    /// Request the editor to quit
    pub fn quit(&mut self) {
        // Check for unsaved buffers (all are auto-persisted when hot_exit is enabled)
        let modified_count = self.count_modified_buffers_needing_prompt();
        if modified_count == 0 && self.config.editor.confirm_quit {
            // No dirty buffers, but the user has opted into a
            // safety-net confirmation for a stray Ctrl+Q (issue #2030).
            let msg = t!("prompt.quit_confirm").to_string();
            self.start_prompt(msg, PromptType::ConfirmQuit);
            return;
        }
        if modified_count > 0 {
            let save_key = t!("prompt.key.save").to_string();
            let cancel_key = t!("prompt.key.cancel").to_string();
            let hot_exit = self.config.editor.hot_exit;

            let discard_key = t!("prompt.key.discard").to_string();
            let msg = if hot_exit {
                // With hot exit: offer save, discard, quit-without-saving (recoverable), or cancel
                let quit_key = t!("prompt.key.quit").to_string();
                if modified_count == 1 {
                    t!(
                        "prompt.quit_modified_hot_one",
                        save_key = save_key,
                        discard_key = discard_key,
                        quit_key = quit_key,
                        cancel_key = cancel_key
                    )
                    .to_string()
                } else {
                    t!(
                        "prompt.quit_modified_hot_many",
                        count = modified_count,
                        save_key = save_key,
                        discard_key = discard_key,
                        quit_key = quit_key,
                        cancel_key = cancel_key
                    )
                    .to_string()
                }
            } else {
                // Without hot exit: offer save, discard, or cancel
                if modified_count == 1 {
                    t!(
                        "prompt.quit_modified_one",
                        save_key = save_key,
                        discard_key = discard_key,
                        cancel_key = cancel_key
                    )
                    .to_string()
                } else {
                    t!(
                        "prompt.quit_modified_many",
                        count = modified_count,
                        save_key = save_key,
                        discard_key = discard_key,
                        cancel_key = cancel_key
                    )
                    .to_string()
                }
            };
            self.start_prompt(msg, PromptType::ConfirmQuitWithModified);
        } else {
            self.should_quit = true;
        }
    }

    /// Count modified buffers that would require a save prompt on quit.
    ///
    /// When `hot_exit` is enabled, unnamed buffers are excluded (they are
    /// automatically recovered across sessions), but file-backed modified
    /// buffers still trigger a prompt with a "recoverable" option.
    /// When `auto_save_enabled` is true, file-backed buffers are excluded
    /// (they will be saved to disk on exit).
    fn count_modified_buffers_needing_prompt(&self) -> usize {
        let hot_exit = self.config.editor.hot_exit;
        let auto_save = self.config.editor.auto_save_enabled;

        self.windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .iter()
            .filter(|(buffer_id, state)| {
                if !state.buffer.is_modified() {
                    return false;
                }
                if let Some(meta) = self.active_window().buffer_metadata.get(buffer_id) {
                    if let Some(path) = meta.file_path() {
                        let is_unnamed = path.as_os_str().is_empty();
                        if is_unnamed && hot_exit {
                            return false; // unnamed buffer, auto-recovered via hot exit
                        }
                        if !is_unnamed && auto_save {
                            return false; // file-backed, will be auto-saved on exit
                        }
                    }
                }
                true
            })
            .count()
    }

    /// Handle terminal focus gained event
    pub fn focus_gained(&mut self) {
        self.plugin_manager.read().unwrap().run_hook(
            "focus_gained",
            crate::services::plugins::hooks::HookArgs::FocusGained {},
        );
    }

    /// Dispatch a raw terminal event into the editor.
    ///
    /// Async clipboard pastes are anchored in the buffer (a floating
    /// "▍" placeholder), not gated on an input queue, so input is
    /// dispatched immediately even while a paste is in flight. The
    /// pasted text lands at its anchor when it arrives without
    /// disturbing the live cursor.
    ///
    /// Returns whether the editor wants the next frame redrawn.
    pub fn handle_input_event(&mut self, event: crossterm::event::Event) -> anyhow::Result<bool> {
        use crossterm::event::{Event as Ev, KeyEventKind};

        match event {
            Ev::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                let key_code = format!("{:?}", key_event.code);
                let modifiers = format!("{:?}", key_event.modifiers);
                self.active_window_mut()
                    .log_keystroke(&key_code, &modifiers);
                let translated = self.key_translator().translate(key_event);
                self.handle_key(translated.code, translated.modifiers)?;
                // If `paste()` just took the async placeholder path,
                // skip the otherwise-automatic render for this
                // keystroke. The placeholder is sitting in the
                // buffer; the next render that fires for any other
                // reason (typing, mouse, the paste resolving, the
                // deadline expiring) will pick it up. Saves one full
                // `terminal.draw` cycle per Ctrl+V — on a slow
                // renderer that's the dominant component of the
                // user-visible paste latency.
                if self.take_paste_slow_path_armed() {
                    Ok(false)
                } else {
                    Ok(true)
                }
            }
            Ev::Mouse(mouse_event) => self.handle_mouse(mouse_event),
            Ev::Resize(w, h) => {
                self.resize(w, h);
                Ok(true)
            }
            Ev::Paste(text) => {
                // Terminal-initiated bracketed paste — no async read
                // needed, the terminal already harvested the clipboard.
                // When a floating modal / dock owns the keyboard the
                // paste belongs to its focused text field, not the
                // buffer underneath; route there first (and let a modal
                // with no text field focused swallow it) before falling
                // back to the buffer/prompt paste path.
                if !self.paste_bracketed_into_focused_panel(&text) {
                    self.paste_text(text);
                }
                Ok(true)
            }
            Ev::FocusGained => {
                self.focus_gained();
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Adopt new terminal (screen) dimensions, then re-derive the whole
    /// layout. This is the OS-terminal-resize entry point; it only
    /// records the new screen size and defers everything else to the
    /// single layout funnel, [`Editor::relayout`].
    pub fn resize(&mut self, width: u16, height: u16) {
        // Editor's canonical screen dimensions (used to seed new windows).
        self.terminal_width = width;
        self.terminal_height = height;
        self.relayout();
    }

    /// The single layout funnel. Every event that can change the on-screen
    /// geometry — an OS terminal resize, toggling/dragging the dock,
    /// toggling or dragging the file explorer, creating/closing/resizing a
    /// split — mutates only the relevant source-of-truth state and then
    /// calls this. `relayout` reads that state, derives the authoritative
    /// geometry once, and pushes it *down* (one-directional) to every
    /// consumer: split viewports, terminal PTYs (for all windows), the
    /// dock / floating-panel rerender, and the plugin `resize` hook.
    ///
    /// It is intentionally cheap to call redundantly: terminal PTY resizes
    /// are idempotent (the PTY layer drops no-op size changes) and the
    /// plugin hook is signature-deduped, so callers never need to decide
    /// "did this actually change the layout?" — they just call `relayout`.
    pub fn relayout(&mut self) {
        // Derive the dock width from its placement (the source of truth),
        // exactly as the renderer's `compute_dock_split` does, so the
        // geometry we push down matches what gets painted.
        let dock_cols = self.dock_cols();
        let (width, height) = (self.terminal_width, self.terminal_height);

        // Push the derived geometry down to every window. The dock is
        // editor-global, so every window — not just the active one —
        // sizes its terminals for the post-dock chrome, ready for a
        // dive without a stale first frame.
        for window in self.windows.values_mut() {
            window.apply_layout(width, height, dock_cols);
        }

        self.notify_layout_changed();
    }

    /// Effective width (cols) the left dock currently claims, or `0` when
    /// no dock is shown / the terminal is too narrow for one. Delegates to
    /// the renderer's `compute_dock_split` so this and the paint path can
    /// never disagree about how wide the dock is.
    pub(crate) fn dock_cols(&self) -> u16 {
        let size = ratatui::layout::Rect::new(0, 0, self.terminal_width, self.terminal_height);
        self.compute_dock_split(size)
            .0
            .map(|dock| dock.width)
            .unwrap_or(0)
    }

    /// Fire the plugin `resize` hook and rerender mounted panels, but only
    /// when the content geometry plugins observe has actually changed since
    /// the last notification. The dedupe is load-bearing: the orchestrator
    /// reacts to `resize` by re-issuing the dock's `dock_width`, which loops
    /// back through `relayout`; without the signature guard that would
    /// re-fire every frame. Once the dock width settles the signature stops
    /// changing and the cascade stops.
    fn notify_layout_changed(&mut self) {
        let dock_cols = self.dock_cols();
        // File-explorer width of the active window, measured against the
        // post-dock chrome (matches the renderer and `resize_visible_terminals`).
        let fe_cols = {
            let win = self.active_window();
            if win.file_explorer_visible {
                win.file_explorer_width
                    .to_cols(self.terminal_width.saturating_sub(dock_cols))
            } else {
                0
            }
        };
        let signature = (
            self.terminal_width,
            self.terminal_height,
            dock_cols,
            fe_cols,
        );
        if self.last_layout_signature == Some(signature) {
            return;
        }
        self.last_layout_signature = Some(signature);

        // Refresh the plugin-facing snapshot BEFORE firing the resize
        // hook. Without this, the orchestrator's resize handler reads
        // `editor.getViewport()` from a snapshot whose dimensions still
        // reflect the pre-resize size — the one-way ratchet in
        // `buildOpenSpec` then sees `old > old` and skips the update,
        // leaving the picker stuck small. Updating the snapshot here lets
        // plugins observe the new dimensions when they react to the hook.
        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();

        // Notify plugins of the layout change so they can adjust their own
        // layouts. The hook still reports the screen dimensions; plugins
        // that care about their available area read `getViewport()` (which
        // reflects the dock / file-explorer carve-out) from the snapshot
        // refreshed just above.
        self.plugin_manager.read().unwrap().run_hook(
            "resize",
            fresh_core::hooks::HookArgs::Resize {
                width: self.terminal_width,
                height: self.terminal_height,
            },
        );

        // If a floating widget panel is currently mounted (the
        // Orchestrator picker / dock, New-Session form, plugin overlays),
        // its cached `entries` were laid out against the old geometry —
        // re-render against the new one so column widths, side borders and
        // embed rects all reflect the new chrome (Bug 13). The hook above
        // lets plugins update their spec; this rerender picks up either the
        // updated spec or the existing spec at the new width.
        for panel_id in [
            self.dock.as_ref().map(|f| f.panel_id),
            self.floating_widget_panel.as_ref().map(|f| f.panel_id),
        ]
        .into_iter()
        .flatten()
        {
            self.rerender_widget_panel(panel_id);
        }
    }
}

impl crate::app::window::Window {
    /// Adopt the geometry handed down by [`Editor::relayout`]: cache the
    /// screen dimensions and the editor-global dock width, reseed every
    /// split viewport against the post-dock editor width, and resize the
    /// visible terminal PTYs. Per-split viewport dimensions are refined
    /// again at paint time by `sync_viewport_to_content`; terminals have
    /// no such paint-time sync, which is why their PTY size must be pushed
    /// here.
    pub fn apply_layout(&mut self, width: u16, height: u16, dock_cols: u16) {
        self.terminal_width = width;
        self.terminal_height = height;
        self.dock_cols = dock_cols;

        let editor_width = width.saturating_sub(dock_cols);
        if let Some(view_states) = self.split_view_states_mut() {
            for view_state in view_states.values_mut() {
                view_state.viewport.resize(editor_width, height);
            }
        }

        self.resize_visible_terminals();

        // The editor narrowed/widened (dock toggle or drag, file explorer,
        // window resize, split change). Re-pin each visible split's active
        // tab into view at its NEW width — otherwise a tab that was flush
        // against the right edge scrolls off when the pane shrinks and the
        // tab-scroll offset is never revisited. Use each split's real area
        // width (dock/explorer/split-aware), not the whole-window width, so
        // a half-width vertical split scrolls correctly too.
        let visible: Vec<(
            crate::model::event::LeafId,
            crate::model::event::BufferId,
            u16,
        )> = match self.buffers.splits() {
            Some((mgr, _)) => mgr
                .get_visible_buffers(self.editor_content_area())
                .into_iter()
                .map(|(split_id, buffer_id, area)| (split_id, buffer_id, area.width))
                .collect(),
            None => Vec::new(),
        };
        for (split_id, buffer_id, tab_width) in visible {
            self.ensure_active_tab_visible(split_id, buffer_id, tab_width);
        }
    }
}
