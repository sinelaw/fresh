use super::*;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

/// Convert a crossterm `KeyEvent` into the `KeyEventPayload` shape
/// delivered to plugin `editor.getNextKey()` callers.
///
/// `key` matches the naming used by `defineMode` bindings:
///   - named keys are lowercase (`"escape"`, `"enter"`, `"tab"`,
///     `"space"`, `"backspace"`, arrows, `"f1"`–`"f12"`, …)
///   - printable characters are returned as-is (`"a"`, `"!"`, `" "`)
///   - unsupported / unknown keys yield an empty `key` string
fn key_event_to_payload(ev: &crossterm::event::KeyEvent) -> fresh_core::api::KeyEventPayload {
    use crossterm::event::{KeyCode, KeyModifiers};
    let key = match ev.code {
        KeyCode::Char(c) => c.to_string(),
        KeyCode::Esc => "escape".to_string(),
        KeyCode::Enter => "enter".to_string(),
        KeyCode::Tab => "tab".to_string(),
        KeyCode::BackTab => "backtab".to_string(),
        KeyCode::Backspace => "backspace".to_string(),
        KeyCode::Delete => "delete".to_string(),
        KeyCode::Left => "left".to_string(),
        KeyCode::Right => "right".to_string(),
        KeyCode::Up => "up".to_string(),
        KeyCode::Down => "down".to_string(),
        KeyCode::Home => "home".to_string(),
        KeyCode::End => "end".to_string(),
        KeyCode::PageUp => "pageup".to_string(),
        KeyCode::PageDown => "pagedown".to_string(),
        KeyCode::Insert => "insert".to_string(),
        KeyCode::F(n) => format!("f{}", n),
        _ => String::new(),
    };
    fresh_core::api::KeyEventPayload {
        key,
        ctrl: ev.modifiers.contains(KeyModifiers::CONTROL),
        alt: ev.modifiers.contains(KeyModifiers::ALT),
        shift: ev.modifiers.contains(KeyModifiers::SHIFT),
        meta: ev.modifiers.contains(KeyModifiers::SUPER),
    }
}

impl Editor {
    /// If a plugin is awaiting the next keypress (via
    /// `editor.getNextKey()`), resolve the front-most pending
    /// callback with this key and return `true` so the caller can
    /// short-circuit further dispatch. The key is consumed by the
    /// resolution; mode bindings and editor actions do not see it.
    ///
    /// If no callback is pending but the plugin has declared key
    /// capture active (`editor.beginKeyCapture()`), buffer the key
    /// instead of dispatching it. The next `AwaitNextKey` will pop
    /// from the buffer immediately. This closes the race between
    /// fast typing/paste and the plugin re-arming `getNextKey`
    /// between iterations.
    fn try_resolve_next_key_callback(&mut self, key_event: &crossterm::event::KeyEvent) -> bool {
        let payload = key_event_to_payload(key_event);
        if let Some(callback_id) = self.pending_next_key_callbacks.pop_front() {
            let json = serde_json::to_string(&payload).unwrap_or_else(|_| "null".to_string());
            self.plugin_manager.resolve_callback(callback_id, json);
            return true;
        }
        if self.key_capture_active {
            self.pending_key_capture_buffer.push_back(payload);
            return true;
        }
        false
    }
}

impl Editor {
    /// Whether editor-pane popups (LSP completion, hover, signature help,
    /// global plugin popups, …) should intercept keyboard input.
    ///
    /// Returns `false` when:
    ///   - the user has focus on the file explorer pane (popups belong
    ///     to the editor pane, and the explorer must own its own
    ///     keystrokes), or
    ///   - the topmost visible popup is unfocused (LSP popups appear
    ///     unfocused so they don't silently swallow the next keystroke;
    ///     the user grabs focus explicitly with `popup_focus`,
    ///     default `Alt+T`).
    ///
    /// Buffer-switch handlers (e.g. `open_file_preview`) clear stale
    /// popups so a popup tied to the previous preview doesn't follow the
    /// user across buffers.
    ///
    /// Single source of truth for both `get_key_context` (binding resolution)
    /// and `dispatch_modal_input` (handler routing) so the two cannot drift.
    pub(crate) fn popups_capture_keys(&self) -> bool {
        use crate::input::keybindings::KeyContext;
        if matches!(self.key_context, KeyContext::FileExplorer) {
            return false;
        }
        self.topmost_popup_focused()
    }

    /// Whether the topmost visible popup (global stack first, then the
    /// active buffer's stack) has been marked focused. Returns `false`
    /// when no popup is visible — the caller is responsible for
    /// short-circuiting that case.
    pub(crate) fn topmost_popup_focused(&self) -> bool {
        if let Some(popup) = self.global_popups.top() {
            return popup.focused;
        }
        if let Some(popup) = self.active_state().popups.top() {
            return popup.focused;
        }
        // No popup → no capture. Returning `false` here is safe because
        // every caller gates on visibility before reaching this path.
        false
    }

    /// When an *unfocused* popup is on screen, resolve the key event
    /// against `KeyContext::Popup`/`Global` so the user's bound
    /// `popup_cancel` (default Esc) and `popup_focus` (default Alt+T)
    /// keys still take effect even though the popup isn't claiming the
    /// keyboard. Without this, dismissing an LSP auto-prompt with Esc
    /// would silently fall through to the buffer.
    ///
    /// Returns `None` for any other action so type-to-filter, cursor
    /// motion, etc. continue to drive the buffer.
    pub(crate) fn resolve_unfocused_popup_action(
        &self,
        event: &crossterm::event::KeyEvent,
    ) -> Option<crate::input::keybindings::Action> {
        use crate::input::keybindings::{Action, KeyContext};

        let popup_visible =
            self.global_popups.is_visible() || self.active_state().popups.is_visible();
        if !popup_visible || self.topmost_popup_focused() {
            return None;
        }

        // Higher-priority modal contexts (Settings, Menu, Prompt) own the
        // keyboard regardless of whether a buffer popup happens to be
        // visible underneath. Skip the unfocused-popup interception so
        // pressing Esc in a settings dialog still closes the dialog
        // rather than reaching past it to dismiss a stale popup.
        if self.settings_state.as_ref().is_some_and(|s| s.visible)
            || self.menu_state.active_menu.is_some()
            || self.is_prompting()
        {
            return None;
        }

        let kb = self.keybindings.read().ok()?;

        // `popup_focus` lives in the Normal/FileExplorer context defaults
        // (not Global) so a user's own binding for the same key in those
        // contexts wins at the same precedence level. If the resolution
        // here returns anything other than `PopupFocus`, it's the user's
        // override — let the normal dispatcher handle it. Don't claim
        // `popup_cancel` from Normal because Normal's default `Esc`
        // resolves to `remove_secondary_cursors`, which would shadow the
        // popup-dismiss intent here.
        let popup_focus_match = matches!(
            kb.resolve_in_context_only(event, self.key_context.clone()),
            Some(Action::PopupFocus),
        );
        if popup_focus_match {
            return Some(Action::PopupFocus);
        }

        // Fall back to the Popup context for `popup_cancel`. Esc
        // (the default `popup_cancel` binding) should still dismiss
        // an unfocused popup even though the popup itself isn't
        // claiming the keyboard — that matches every other popup-
        // dismissal affordance in the editor.
        let resolved_popup = kb.resolve_in_context_only(event, KeyContext::Popup);
        match resolved_popup {
            Some(action @ (Action::PopupCancel | Action::PopupFocus)) => Some(action),
            _ => None,
        }
    }

    /// Resolve a key event against `KeyContext::Completion` when the topmost
    /// visible popup is a completion popup. Only `CompletionAccept` and
    /// `CompletionDismiss` are recognised here — every other key falls
    /// through to the popup's own handler so type-to-filter, navigation, and
    /// the "any other key dismisses + passthrough" behaviours stay intact.
    pub(crate) fn resolve_completion_popup_action(
        &self,
        event: &crossterm::event::KeyEvent,
    ) -> Option<crate::input::keybindings::Action> {
        use crate::input::keybindings::{Action, KeyContext};
        use crate::view::popup::PopupKind;

        let topmost_kind = if self.global_popups.is_visible() {
            self.global_popups.top().map(|p| p.kind)
        } else if self.active_state().popups.is_visible() {
            self.active_state().popups.top().map(|p| p.kind)
        } else {
            None
        };

        if topmost_kind != Some(PopupKind::Completion) {
            return None;
        }

        match self
            .keybindings
            .read()
            .unwrap()
            .resolve_in_context_only(event, KeyContext::Completion)
        {
            Some(action @ (Action::CompletionAccept | Action::CompletionDismiss)) => Some(action),
            _ => None,
        }
    }

    /// Determine the current keybinding context based on UI state
    pub fn get_key_context(&self) -> crate::input::keybindings::KeyContext {
        use crate::input::keybindings::KeyContext;

        // Priority order: Settings > Menu > Prompt > Popup (only when
        // editor-pane focused) > CompositeBuffer > Current context
        // (FileExplorer or Normal).
        if self.settings_state.as_ref().is_some_and(|s| s.visible) {
            KeyContext::Settings
        } else if self.menu_state.active_menu.is_some() {
            KeyContext::Menu
        } else if self.is_prompting() {
            KeyContext::Prompt
        } else if self.popups_capture_keys()
            && (self.global_popups.is_visible() || self.active_state().popups.is_visible())
        {
            KeyContext::Popup
        } else if self.is_composite_buffer(self.active_buffer()) {
            KeyContext::CompositeBuffer
        } else {
            // Use the current context (can be FileExplorer or Normal)
            self.key_context.clone()
        }
    }

    /// Handle a key event and return whether it was handled
    /// This is the central key handling logic used by both main.rs and tests
    pub fn handle_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        use crate::input::keybindings::Action;

        let _t_total = std::time::Instant::now();

        tracing::trace!(
            "Editor.handle_key: code={:?}, modifiers={:?}",
            code,
            modifiers
        );

        // Create key event for dispatch methods
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);

        // Event debug dialog intercepts ALL key events before any other processing.
        // This must be checked here (not just in main.rs/gui) so it works in
        // client/server mode where handle_key is called directly.
        if self.is_event_debug_active() {
            self.handle_event_debug_input(&key_event);
            return Ok(());
        }

        // Try terminal input dispatch first (handles terminal mode and re-entry)
        if self.dispatch_terminal_input(&key_event).is_some() {
            return Ok(());
        }

        // If a plugin is awaiting the next keypress (`editor.getNextKey()`),
        // hand this key to the front-most pending callback and consume it.
        // This must run before any other dispatch so the awaiting plugin —
        // typically running a short input loop (flash labels, vi
        // find-char/replace-char) — can drive its own state machine
        // without binding every printable key in `defineMode`.
        if self.try_resolve_next_key_callback(&key_event) {
            return Ok(());
        }

        // Clear skip_ensure_visible flag so cursor becomes visible after key press
        // (scroll actions will set it again if needed). Use the *effective*
        // active split so this clears the flag on a focused buffer-group
        // panel's own view state, not the group host's — without this, a
        // scroll action in the panel (mouse scrollbar click, plugin
        // scrollBufferToLine, etc.) sets `skip_ensure_visible` on the panel
        // and subsequent key presses never clear it, so cursor motion stops
        // scrolling the viewport.
        let active_split = self.effective_active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.viewport.clear_skip_ensure_visible();
        }

        // Dismiss theme info popup on any key press
        if self.theme_info_popup.is_some() {
            self.theme_info_popup = None;
        }

        if self.file_explorer_context_menu.is_some() {
            if let Some(result) = self.handle_file_explorer_context_menu_key(code, modifiers) {
                return result;
            }
        }

        // Determine the current context first
        let mut context = self.get_key_context();

        // Special case: Hover and Signature Help popups should be dismissed on any key press
        // EXCEPT for Ctrl+C when the popup has a text selection (allow copy first).
        //
        // Fires for both focused and unfocused popups: an unfocused
        // hover popup that floats over the buffer must still vanish when
        // the user starts typing — otherwise it lingers indefinitely
        // because no key event reaches it. The focused-popup path also
        // covers the legacy case where a transient popup was given
        // focus (e.g. via the focus-popup keybinding).
        let popup_visible_on_screen =
            self.global_popups.is_visible() || self.active_state().popups.is_visible();
        if popup_visible_on_screen {
            // Check if the current popup is transient (hover, signature help).
            // Editor-level popups always take precedence over buffer popups
            // when both are visible — they're effectively modal overlays.
            let (is_transient_popup, has_selection) = {
                let popup = self
                    .global_popups
                    .top()
                    .or_else(|| self.active_state().popups.top());
                (
                    popup.is_some_and(|p| p.transient),
                    popup.is_some_and(|p| p.has_selection()),
                )
            };

            // Don't dismiss if popup has selection and user is pressing Ctrl+C (let them copy first)
            let is_copy_key = key_event.code == crossterm::event::KeyCode::Char('c')
                && key_event
                    .modifiers
                    .contains(crossterm::event::KeyModifiers::CONTROL);

            // Skip the dismiss when the user is *transferring* focus to
            // the popup — otherwise pressing the focus-popup key while
            // a transient popup is on screen would close the popup
            // before its handler ever sees the focus action.
            let resolved_action = self
                .keybindings
                .read()
                .ok()
                .map(|kb| kb.resolve(&key_event, context.clone()));
            let is_focus_popup_key = matches!(
                resolved_action,
                Some(crate::input::keybindings::Action::PopupFocus)
            );

            if is_transient_popup && !(has_selection && is_copy_key) && !is_focus_popup_key {
                // Dismiss the popup on any key press (except Ctrl+C with selection)
                self.hide_popup();
                tracing::debug!("Dismissed transient popup on key press");
                // Recalculate context now that popup is gone
                context = self.get_key_context();
            }
        }

        // Unfocused popup control: even though an unfocused popup
        // doesn't claim the keyboard, the user's bound popup-cancel
        // (default Esc) and popup-focus (default Alt+T) keys must
        // still affect it. Resolved here, *before* the modal
        // dispatcher routes the key to the buffer/explorer/etc.
        if let Some(action) = self.resolve_unfocused_popup_action(&key_event) {
            self.handle_action(action)?;
            return Ok(());
        }

        // Try hierarchical modal input dispatch first (Settings, Menu, Prompt, Popup)
        if self.dispatch_modal_input(&key_event).is_some() {
            return Ok(());
        }

        // If a modal was dismissed (e.g., completion popup closed and returned Ignored),
        // recalculate the context so the key is processed in the correct context.
        if context != self.get_key_context() {
            context = self.get_key_context();
        }

        // Only check buffer mode keybindings when the editor buffer has focus.
        // FileExplorer, Menu, Prompt, Popup contexts should not trigger mode bindings
        // (e.g. markdown-source's Enter handler should not fire while the explorer is focused).
        let should_check_mode_bindings =
            matches!(context, crate::input::keybindings::KeyContext::Normal);

        if should_check_mode_bindings {
            // effective_mode() returns buffer-local mode if present, else global mode.
            // This ensures virtual buffer modes aren't hijacked by global modes.
            let effective_mode = self.effective_mode().map(|s| s.to_owned());

            if let Some(ref mode_name) = effective_mode {
                let mode_ctx = crate::input::keybindings::KeyContext::Mode(mode_name.to_string());
                let key_event = crossterm::event::KeyEvent::new(code, modifiers);

                // Mode chord resolution (via KeybindingResolver)
                let (chord_result, resolved_action) = {
                    let keybindings = self.keybindings.read().unwrap();
                    let chord_result =
                        keybindings.resolve_chord(&self.chord_state, &key_event, mode_ctx.clone());
                    let resolved = keybindings.resolve(&key_event, mode_ctx);
                    (chord_result, resolved)
                };
                match chord_result {
                    crate::input::keybindings::ChordResolution::Complete(action) => {
                        tracing::debug!("Mode chord resolved to action: {:?}", action);
                        self.chord_state.clear();
                        return self.handle_action(action);
                    }
                    crate::input::keybindings::ChordResolution::Partial => {
                        tracing::debug!("Potential chord prefix in mode '{}'", mode_name);
                        self.chord_state.push((code, modifiers));
                        return Ok(());
                    }
                    crate::input::keybindings::ChordResolution::NoMatch => {
                        if !self.chord_state.is_empty() {
                            tracing::debug!("Chord sequence abandoned in mode, clearing state");
                            self.chord_state.clear();
                        }
                    }
                }

                // Mode single-key resolution (custom > keymap > plugin defaults)
                if resolved_action != Action::None {
                    return self.handle_action(resolved_action);
                }
            }

            // Handle unbound keys for modes that want to capture input.
            //
            // Buffer-local modes with allow_text_input (e.g. search-replace-list)
            // capture character keys and block other unbound keys.
            //
            // Buffer-local modes WITHOUT allow_text_input (e.g. diff-view) let
            // unbound keys fall through to normal keybinding handling so that
            // Ctrl+C, arrows, etc. still work.
            //
            // Global editor modes (e.g. vi-normal) block all unbound keys when
            // read-only.
            if let Some(ref mode_name) = effective_mode {
                if self.mode_registry.allows_text_input(mode_name) {
                    if let KeyCode::Char(c) = code {
                        let ch = if modifiers.contains(KeyModifiers::SHIFT) {
                            c.to_uppercase().next().unwrap_or(c)
                        } else {
                            c
                        };
                        if !modifiers.intersects(KeyModifiers::CONTROL | KeyModifiers::ALT) {
                            let action_name = format!("mode_text_input:{}", ch);
                            return self.handle_action(Action::PluginAction(action_name));
                        }
                    }
                    tracing::debug!("Blocking unbound key in text-input mode '{}'", mode_name);
                    return Ok(());
                }
            }
            if let Some(ref mode_name) = self.editor_mode {
                if self.mode_registry.is_read_only(mode_name) {
                    tracing::debug!("Ignoring unbound key in read-only mode '{}'", mode_name);
                    return Ok(());
                }
                tracing::debug!(
                    "Mode '{}' is not read-only, allowing key through",
                    mode_name
                );
            }
        }

        // --- Composite buffer input routing ---
        // If the active buffer is a composite buffer (side-by-side diff),
        // route remaining composite-specific keys (scroll, pane switch, close)
        // through CompositeInputRouter before falling through to regular
        // keybinding resolution. Hunk navigation (n/p/]/[) is handled by the
        // Action system via CompositeBuffer context bindings.
        {
            let active_buf = self.active_buffer();
            let active_split = self.effective_active_split();
            if self.is_composite_buffer(active_buf) {
                if let Some(handled) =
                    self.try_route_composite_key(active_split, active_buf, &key_event)
                {
                    return handled;
                }
            }
        }

        // Check for chord sequence matches first
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let (chord_result, action) = {
            let keybindings = self.keybindings.read().unwrap();
            let chord_result =
                keybindings.resolve_chord(&self.chord_state, &key_event, context.clone());
            let action = keybindings.resolve(&key_event, context.clone());
            (chord_result, action)
        };

        match chord_result {
            crate::input::keybindings::ChordResolution::Complete(action) => {
                // Complete chord match - execute action and clear chord state
                tracing::debug!("Complete chord match -> Action: {:?}", action);
                self.chord_state.clear();
                return self.handle_action(action);
            }
            crate::input::keybindings::ChordResolution::Partial => {
                // Partial match - add to chord state and wait for more keys
                tracing::debug!("Partial chord match - waiting for next key");
                self.chord_state.push((code, modifiers));
                return Ok(());
            }
            crate::input::keybindings::ChordResolution::NoMatch => {
                // No chord match - clear state and try regular resolution
                if !self.chord_state.is_empty() {
                    tracing::debug!("Chord sequence abandoned, clearing state");
                    self.chord_state.clear();
                }
            }
        }

        // Regular single-key resolution (already resolved above)
        tracing::trace!("Context: {:?} -> Action: {:?}", context, action);

        // Cancel pending LSP requests on user actions (except LSP actions themselves)
        // This ensures stale completions don't show up after the user has moved on
        match action {
            Action::LspCompletion
            | Action::LspGotoDefinition
            | Action::LspReferences
            | Action::LspHover
            | Action::None => {
                // Don't cancel for LSP actions or no-op
            }
            _ => {
                // Cancel any pending LSP requests
                self.cancel_pending_lsp_requests();
            }
        }

        // Note: Modal components (Settings, Menu, Prompt, Popup, File Browser) are now
        // handled by dispatch_modal_input using the InputHandler system.
        // All remaining actions delegate to handle_action.
        self.handle_action(action)
    }

    /// Handle an action (for normal mode and command execution).
    /// Used by the app module internally and by the GUI module for native menu dispatch.
    pub(crate) fn handle_action(&mut self, action: Action) -> AnyhowResult<()> {
        use crate::input::keybindings::Action;

        // Record action to macro if recording
        self.record_macro_action(&action);

        // Reset dabbrev cycling session on any non-dabbrev action.
        if !matches!(action, Action::DabbrevExpand) {
            self.reset_dabbrev_state();
        }

        match action {
            Action::Quit => self.quit(),
            Action::ForceQuit => {
                self.should_quit = true;
            }
            Action::Detach => {
                self.should_detach = true;
            }
            Action::Save => {
                // Check if buffer has a file path - if not, redirect to SaveAs
                if self.active_state().buffer.file_path().is_none() {
                    self.start_prompt_with_initial_text(
                        t!("file.save_as_prompt").to_string(),
                        PromptType::SaveFileAs,
                        String::new(),
                    );
                    self.init_file_open_state();
                } else if self.check_save_conflict().is_some() {
                    // Check if file was modified externally since we opened/saved it
                    self.start_prompt(
                        t!("file.file_changed_prompt").to_string(),
                        PromptType::ConfirmSaveConflict,
                    );
                } else if let Err(e) = self.save() {
                    let msg = format!("{}", e);
                    self.status_message = Some(t!("file.save_failed", error = &msg).to_string());
                }
            }
            Action::SaveAs => {
                // Get current filename as default suggestion
                let current_path = self
                    .active_state()
                    .buffer
                    .file_path()
                    .map(|p| {
                        // Make path relative to working_dir if possible
                        p.strip_prefix(&self.working_dir)
                            .unwrap_or(p)
                            .to_string_lossy()
                            .to_string()
                    })
                    .unwrap_or_default();
                self.start_prompt_with_initial_text(
                    t!("file.save_as_prompt").to_string(),
                    PromptType::SaveFileAs,
                    current_path,
                );
                self.init_file_open_state();
            }
            Action::Open => {
                self.start_prompt(t!("file.open_prompt").to_string(), PromptType::OpenFile);
                self.prefill_open_file_prompt();
                self.init_file_open_state();
            }
            Action::SwitchProject => {
                self.start_prompt(
                    t!("file.switch_project_prompt").to_string(),
                    PromptType::SwitchProject,
                );
                self.init_folder_open_state();
            }
            Action::GotoLine => {
                let has_line_index = self
                    .buffers
                    .get(&self.active_buffer())
                    .is_none_or(|s| s.buffer.line_count().is_some());
                if has_line_index {
                    self.start_prompt(
                        t!("file.goto_line_prompt").to_string(),
                        PromptType::GotoLine,
                    );
                } else {
                    self.start_prompt(
                        t!("goto.scan_confirm_prompt", yes = "y", no = "N").to_string(),
                        PromptType::GotoLineScanConfirm,
                    );
                }
            }
            Action::ScanLineIndex => {
                self.start_incremental_line_scan(false);
            }
            Action::New => {
                self.new_buffer();
            }
            Action::Close | Action::CloseTab => {
                // Both Close and CloseTab use close_tab() which handles:
                // - Closing the split if this is the last buffer and there are other splits
                // - Prompting for unsaved changes
                // - Properly closing the buffer
                self.close_tab();
            }
            Action::Revert => {
                // Check if buffer has unsaved changes - prompt for confirmation
                if self.active_state().buffer.is_modified() {
                    let revert_key = t!("prompt.key.revert").to_string();
                    let cancel_key = t!("prompt.key.cancel").to_string();
                    self.start_prompt(
                        t!(
                            "prompt.revert_confirm",
                            revert_key = revert_key,
                            cancel_key = cancel_key
                        )
                        .to_string(),
                        PromptType::ConfirmRevert,
                    );
                } else {
                    // No local changes, just revert
                    if let Err(e) = self.revert_file() {
                        self.set_status_message(
                            t!("error.failed_to_revert", error = e.to_string()).to_string(),
                        );
                    }
                }
            }
            Action::ToggleAutoRevert => {
                self.toggle_auto_revert();
            }
            Action::FormatBuffer => {
                if let Err(e) = self.format_buffer() {
                    self.set_status_message(
                        t!("error.format_failed", error = e.to_string()).to_string(),
                    );
                }
            }
            Action::TrimTrailingWhitespace => match self.trim_trailing_whitespace() {
                Ok(true) => {
                    self.set_status_message(t!("whitespace.trimmed").to_string());
                }
                Ok(false) => {
                    self.set_status_message(t!("whitespace.no_trailing").to_string());
                }
                Err(e) => {
                    self.set_status_message(
                        t!("error.trim_whitespace_failed", error = e).to_string(),
                    );
                }
            },
            Action::EnsureFinalNewline => match self.ensure_final_newline() {
                Ok(true) => {
                    self.set_status_message(t!("whitespace.newline_added").to_string());
                }
                Ok(false) => {
                    self.set_status_message(t!("whitespace.already_has_newline").to_string());
                }
                Err(e) => {
                    self.set_status_message(
                        t!("error.ensure_newline_failed", error = e).to_string(),
                    );
                }
            },
            Action::Copy => {
                // Editor-level popups take precedence over everything, including the file explorer.
                let popup = self
                    .global_popups
                    .top()
                    .or_else(|| self.active_state().popups.top());
                if let Some(popup) = popup {
                    if popup.has_selection() {
                        if let Some(text) = popup.get_selected_text() {
                            self.clipboard.copy(text);
                            self.set_status_message(t!("clipboard.copied").to_string());
                            return Ok(());
                        }
                    }
                }
                if self.key_context == crate::input::keybindings::KeyContext::FileExplorer {
                    self.file_explorer_copy();
                    return Ok(());
                }
                // Check if active buffer is a composite buffer
                let buffer_id = self.active_buffer();
                if self.is_composite_buffer(buffer_id) {
                    if let Some(_handled) = self.handle_composite_action(buffer_id, &Action::Copy) {
                        return Ok(());
                    }
                }
                self.copy_selection()
            }
            Action::CopyWithTheme(theme) => self.copy_selection_with_theme(&theme),
            Action::CopyFilePath => self.copy_active_buffer_path(false),
            Action::CopyRelativeFilePath => self.copy_active_buffer_path(true),
            Action::Cut => {
                if self.key_context == crate::input::keybindings::KeyContext::FileExplorer {
                    self.file_explorer_cut();
                    return Ok(());
                }
                if self.is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                self.cut_selection()
            }
            Action::Paste => {
                if self.key_context == crate::input::keybindings::KeyContext::FileExplorer {
                    self.file_explorer_paste();
                    return Ok(());
                }
                if self.is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                self.paste()
            }
            Action::YankWordForward => self.yank_word_forward(),
            Action::YankWordBackward => self.yank_word_backward(),
            Action::YankToLineEnd => self.yank_to_line_end(),
            Action::YankToLineStart => self.yank_to_line_start(),
            Action::YankViWordEnd => self.yank_vi_word_end(),
            Action::Undo => {
                self.handle_undo();
            }
            Action::Redo => {
                self.handle_redo();
            }
            Action::ShowHelp => {
                self.open_help_manual();
            }
            Action::ShowKeyboardShortcuts => {
                self.open_keyboard_shortcuts();
            }
            Action::ShowWarnings => {
                self.show_warnings_popup();
            }
            Action::ShowStatusLog => {
                self.open_status_log();
            }
            Action::ShowLspStatus => {
                self.show_lsp_status_popup();
            }
            Action::ShowRemoteIndicatorMenu => {
                self.show_remote_indicator_popup();
            }
            Action::ClearWarnings => {
                self.clear_warnings();
            }
            Action::CommandPalette => {
                // CommandPalette now delegates to QuickOpen (which starts with ">" prefix
                // for command mode). Toggle if already open.
                if let Some(prompt) = &self.prompt {
                    if prompt.prompt_type == PromptType::QuickOpen {
                        self.cancel_prompt();
                        return Ok(());
                    }
                }
                self.start_quick_open();
            }
            Action::QuickOpen => {
                // Toggle Quick Open: close if already open, otherwise open it
                if let Some(prompt) = &self.prompt {
                    if prompt.prompt_type == PromptType::QuickOpen {
                        self.cancel_prompt();
                        return Ok(());
                    }
                }

                // Start Quick Open with file suggestions (default mode)
                self.start_quick_open();
            }
            Action::QuickOpenBuffers => {
                if let Some(prompt) = &self.prompt {
                    if prompt.prompt_type == PromptType::QuickOpen {
                        self.cancel_prompt();
                        return Ok(());
                    }
                }
                self.start_quick_open_with_prefix("#");
            }
            Action::QuickOpenFiles => {
                if let Some(prompt) = &self.prompt {
                    if prompt.prompt_type == PromptType::QuickOpen {
                        self.cancel_prompt();
                        return Ok(());
                    }
                }
                self.start_quick_open_with_prefix("");
            }
            Action::OpenLiveGrep => {
                // Invoke the live_grep plugin's start_live_grep handler.
                // This still produces the bottom-anchored Finder UI today
                // — Phase 2/3 of issue #1796 will swap in the floating
                // overlay rendering. The Action exists now so users get a
                // direct keybinding (Alt+/) instead of palette-only access.
                #[cfg(feature = "plugins")]
                {
                    if let Some(result) =
                        self.plugin_manager.execute_action_async("start_live_grep")
                    {
                        match result {
                            Ok(receiver) => {
                                self.pending_plugin_actions
                                    .push(("start_live_grep".to_string(), receiver));
                            }
                            Err(e) => {
                                self.set_status_message(format!(
                                    "Live Grep unavailable: {}",
                                    e
                                ));
                            }
                        }
                    } else {
                        self.set_status_message(
                            "Live Grep plugin not loaded".to_string(),
                        );
                    }
                }
                #[cfg(not(feature = "plugins"))]
                {
                    self.set_status_message(
                        "Live Grep requires the plugins feature".to_string(),
                    );
                }
            }
            Action::ResumeLiveGrep => {
                // Stub: replay the last-known query through the
                // palette path until the floating overlay lands.
                let last_query = self
                    .live_grep_last_state
                    .as_ref()
                    .map(|s| s.query.clone())
                    .unwrap_or_default();
                if last_query.is_empty() {
                    self.start_quick_open_with_prefix(">live grep");
                } else {
                    // Future: open overlay seeded with `last_query` and
                    // `selected_index`. For now, surface a status hint.
                    self.set_status_message(format!(
                        "Resume Live Grep: previous query was \"{}\" (overlay not yet wired — open via Ctrl+P)",
                        last_query
                    ));
                }
            }
            Action::LiveGrepExportQuickfix => {
                // Stub: only meaningful when the active prompt is the
                // Live Grep overlay. Until the overlay lands the active
                // prompt is `QuickOpen`, so just no-op with a status.
                let in_live_grep = self
                    .prompt
                    .as_ref()
                    .map(|p| p.prompt_type == PromptType::LiveGrep)
                    .unwrap_or(false);
                if !in_live_grep {
                    self.set_status_message(
                        "Quickfix export is only available inside the Live Grep overlay".to_string(),
                    );
                    return Ok(());
                }
                // TODO(#1796 Phase 6): snapshot prompt.suggestions ->
                // Vec<GrepMatch>, dismiss prompt, push into Utility
                // Dock as a virtual buffer with role=utility_dock.
                self.set_status_message("Quickfix export not yet implemented".to_string());
            }
            Action::ToggleUtilityDock => {
                use crate::view::split::SplitRole;
                if let Some(dock_leaf) = self
                    .split_manager
                    .find_leaf_by_role(SplitRole::UtilityDock)
                {
                    let active = self.split_manager.active_split();
                    if active == dock_leaf {
                        // Already focused — no editor-leaf history yet,
                        // so just cycle to the next leaf via the
                        // existing Alt+] command. Phase 7 will track a
                        // proper "previous editor split" pointer.
                        self.next_split();
                    } else {
                        self.split_manager.set_active_split(dock_leaf);
                    }
                } else {
                    self.set_status_message(
                        "No Utility Dock open — invoke a dock-aware utility (Diagnostics, Search/Replace, …)"
                            .to_string(),
                    );
                }
            }
            Action::OpenTerminalInDock => {
                // TODO(#1796 Phase 5/7): route through the dock
                // dispatcher so the terminal lands in the dock leaf
                // (creating it if absent). Until that wiring is in
                // place, fall back to the existing standalone path.
                self.open_terminal();
            }
            Action::ToggleLineWrap => {
                let new_value = !self.config.editor.line_wrap;
                self.config_mut().editor.line_wrap = new_value;

                // Update all viewports to reflect the new line wrap setting,
                // respecting per-language overrides
                let leaf_ids: Vec<_> = self.split_view_states.keys().copied().collect();
                for leaf_id in leaf_ids {
                    let buffer_id = self
                        .split_manager
                        .get_buffer_id(leaf_id.into())
                        .unwrap_or(BufferId(0));
                    let effective_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
                    let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
                    if let Some(view_state) = self.split_view_states.get_mut(&leaf_id) {
                        view_state.viewport.line_wrap_enabled = effective_wrap;
                        view_state.viewport.wrap_indent = self.config.editor.wrap_indent;
                        view_state.viewport.wrap_column = wrap_column;
                    }
                }

                let state = if self.config.editor.line_wrap {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(t!("view.line_wrap_state", state = state).to_string());
            }
            Action::ToggleCurrentLineHighlight => {
                let new_value = !self.config.editor.highlight_current_line;
                self.config_mut().editor.highlight_current_line = new_value;

                // Update all splits
                let leaf_ids: Vec<_> = self.split_view_states.keys().copied().collect();
                for leaf_id in leaf_ids {
                    if let Some(view_state) = self.split_view_states.get_mut(&leaf_id) {
                        view_state.highlight_current_line =
                            self.config.editor.highlight_current_line;
                    }
                }

                let state = if self.config.editor.highlight_current_line {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(
                    t!("view.current_line_highlight_state", state = state).to_string(),
                );
            }
            Action::ToggleReadOnly => {
                let buffer_id = self.active_buffer();
                let is_now_read_only = self
                    .buffer_metadata
                    .get(&buffer_id)
                    .map(|m| !m.read_only)
                    .unwrap_or(false);
                self.mark_buffer_read_only(buffer_id, is_now_read_only);

                let state_str = if is_now_read_only {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(t!("view.read_only_state", state = state_str).to_string());
            }
            Action::TogglePageView => {
                self.handle_toggle_page_view();
            }
            Action::SetPageWidth => {
                let active_split = self.split_manager.active_split();
                let current = self
                    .split_view_states
                    .get(&active_split)
                    .and_then(|v| v.compose_width.map(|w| w.to_string()))
                    .unwrap_or_default();
                self.start_prompt_with_initial_text(
                    "Page width (empty = viewport): ".to_string(),
                    PromptType::SetPageWidth,
                    current,
                );
            }
            Action::SetBackground => {
                let default_path = self
                    .ansi_background_path
                    .as_ref()
                    .and_then(|p| {
                        p.strip_prefix(&self.working_dir)
                            .ok()
                            .map(|rel| rel.to_string_lossy().to_string())
                    })
                    .unwrap_or_else(|| DEFAULT_BACKGROUND_FILE.to_string());

                self.start_prompt_with_initial_text(
                    "Background file: ".to_string(),
                    PromptType::SetBackgroundFile,
                    default_path,
                );
            }
            Action::SetBackgroundBlend => {
                let default_amount = format!("{:.2}", self.background_fade);
                self.start_prompt_with_initial_text(
                    "Background blend (0-1): ".to_string(),
                    PromptType::SetBackgroundBlend,
                    default_amount,
                );
            }
            Action::LspCompletion => {
                self.request_completion();
            }
            Action::DabbrevExpand => {
                self.dabbrev_expand();
            }
            Action::LspGotoDefinition => {
                self.request_goto_definition()?;
            }
            Action::LspRename => {
                self.start_rename()?;
            }
            Action::LspHover => {
                self.request_hover()?;
            }
            Action::LspReferences => {
                self.request_references()?;
            }
            Action::LspSignatureHelp => {
                self.request_signature_help();
            }
            Action::LspCodeActions => {
                self.request_code_actions()?;
            }
            Action::LspRestart => {
                self.handle_lsp_restart();
            }
            Action::LspStop => {
                self.handle_lsp_stop();
            }
            Action::LspToggleForBuffer => {
                self.handle_lsp_toggle_for_buffer();
            }
            Action::ToggleInlayHints => {
                self.toggle_inlay_hints();
            }
            Action::DumpConfig => {
                self.dump_config();
            }
            Action::RedrawScreen => {
                self.request_full_redraw();
            }
            Action::SelectTheme => {
                self.start_select_theme_prompt();
            }
            Action::InspectThemeAtCursor => {
                self.inspect_theme_at_cursor();
            }
            Action::SelectKeybindingMap => {
                self.start_select_keybinding_map_prompt();
            }
            Action::SelectCursorStyle => {
                self.start_select_cursor_style_prompt();
            }
            Action::SelectLocale => {
                self.start_select_locale_prompt();
            }
            Action::Search => {
                // If already in a search-related prompt, Ctrl+F acts like Enter (confirm search)
                let is_search_prompt = self.prompt.as_ref().is_some_and(|p| {
                    matches!(
                        p.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch
                    )
                });

                if is_search_prompt {
                    self.confirm_prompt();
                } else {
                    self.start_search_prompt(
                        t!("file.search_prompt").to_string(),
                        PromptType::Search,
                        false,
                    );
                }
            }
            Action::Replace => {
                // Use same flow as query-replace, just with confirm_each defaulting to false
                self.start_search_prompt(
                    t!("file.replace_prompt").to_string(),
                    PromptType::ReplaceSearch,
                    false,
                );
            }
            Action::QueryReplace => {
                // Enable confirm mode by default for query-replace
                self.search_confirm_each = true;
                self.start_search_prompt(
                    "Query replace: ".to_string(),
                    PromptType::QueryReplaceSearch,
                    false,
                );
            }
            Action::FindInSelection => {
                self.start_search_prompt(
                    t!("file.search_prompt").to_string(),
                    PromptType::Search,
                    true,
                );
            }
            Action::FindNext => {
                self.find_next();
            }
            Action::FindPrevious => {
                self.find_previous();
            }
            Action::FindSelectionNext => {
                self.find_selection_next();
            }
            Action::FindSelectionPrevious => {
                self.find_selection_previous();
            }
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::NextBuffer => self.next_buffer(),
            Action::PrevBuffer => self.prev_buffer(),
            Action::SwitchToPreviousTab => self.switch_to_previous_tab(),
            Action::SwitchToTabByName => self.start_switch_to_tab_prompt(),

            // Tab scrolling (manual scroll - don't auto-adjust)
            Action::ScrollTabsLeft => {
                let active_split_id = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split_id) {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_sub(5);
                    self.set_status_message(t!("status.scrolled_tabs_left").to_string());
                }
            }
            Action::ScrollTabsRight => {
                let active_split_id = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split_id) {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_add(5);
                    self.set_status_message(t!("status.scrolled_tabs_right").to_string());
                }
            }
            Action::NavigateBack => self.navigate_back(),
            Action::NavigateForward => self.navigate_forward(),
            Action::SplitHorizontal => self.split_pane_horizontal(),
            Action::SplitVertical => self.split_pane_vertical(),
            Action::CloseSplit => self.close_active_split(),
            Action::NextSplit => self.next_split(),
            Action::PrevSplit => self.prev_split(),
            Action::IncreaseSplitSize => self.adjust_split_size(0.05),
            Action::DecreaseSplitSize => self.adjust_split_size(-0.05),
            Action::ToggleMaximizeSplit => self.toggle_maximize_split(),
            Action::ToggleFileExplorer => self.toggle_file_explorer(),
            Action::ToggleMenuBar => self.toggle_menu_bar(),
            Action::ToggleTabBar => self.toggle_tab_bar(),
            Action::ToggleStatusBar => self.toggle_status_bar(),
            Action::TogglePromptLine => self.toggle_prompt_line(),
            Action::ToggleVerticalScrollbar => self.toggle_vertical_scrollbar(),
            Action::ToggleHorizontalScrollbar => self.toggle_horizontal_scrollbar(),
            Action::ToggleLineNumbers => self.toggle_line_numbers(),
            Action::ToggleScrollSync => self.toggle_scroll_sync(),
            Action::ToggleMouseCapture => self.toggle_mouse_capture(),
            Action::ToggleMouseHover => self.toggle_mouse_hover(),
            Action::ToggleDebugHighlights => self.toggle_debug_highlights(),
            // Rulers
            Action::AddRuler => {
                self.start_prompt(t!("rulers.add_prompt").to_string(), PromptType::AddRuler);
            }
            Action::RemoveRuler => {
                self.start_remove_ruler_prompt();
            }
            // Buffer settings
            Action::SetTabSize => {
                let current = self
                    .buffers
                    .get(&self.active_buffer())
                    .map(|s| s.buffer_settings.tab_size.to_string())
                    .unwrap_or_else(|| "4".to_string());
                self.start_prompt_with_initial_text(
                    "Tab size: ".to_string(),
                    PromptType::SetTabSize,
                    current,
                );
            }
            Action::SetLineEnding => {
                self.start_set_line_ending_prompt();
            }
            Action::SetEncoding => {
                self.start_set_encoding_prompt();
            }
            Action::ReloadWithEncoding => {
                self.start_reload_with_encoding_prompt();
            }
            Action::SetLanguage => {
                self.start_set_language_prompt();
            }
            Action::ToggleIndentationStyle => {
                if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
                    state.buffer_settings.use_tabs = !state.buffer_settings.use_tabs;
                    let status = if state.buffer_settings.use_tabs {
                        "Indentation: Tabs"
                    } else {
                        "Indentation: Spaces"
                    };
                    self.set_status_message(status.to_string());
                }
            }
            Action::ToggleTabIndicators | Action::ToggleWhitespaceIndicators => {
                if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
                    state.buffer_settings.whitespace.toggle_all();
                    let status = if state.buffer_settings.whitespace.any_visible() {
                        t!("toggle.whitespace_indicators_shown")
                    } else {
                        t!("toggle.whitespace_indicators_hidden")
                    };
                    self.set_status_message(status.to_string());
                }
            }
            Action::ResetBufferSettings => self.reset_buffer_settings(),
            Action::FocusFileExplorer => self.focus_file_explorer(),
            Action::FocusEditor => self.focus_editor(),
            Action::FileExplorerUp => self.file_explorer_navigate_up(),
            Action::FileExplorerDown => self.file_explorer_navigate_down(),
            Action::FileExplorerPageUp => self.file_explorer_page_up(),
            Action::FileExplorerPageDown => self.file_explorer_page_down(),
            Action::FileExplorerExpand => self.file_explorer_toggle_expand(),
            Action::FileExplorerCollapse => self.file_explorer_collapse(),
            Action::FileExplorerOpen => self.file_explorer_open_file()?,
            Action::FileExplorerRefresh => self.file_explorer_refresh(),
            Action::FileExplorerNewFile => self.file_explorer_new_file(),
            Action::FileExplorerNewDirectory => self.file_explorer_new_directory(),
            Action::FileExplorerDelete => self.file_explorer_delete(),
            Action::FileExplorerRename => self.file_explorer_rename(),
            Action::FileExplorerToggleHidden => self.file_explorer_toggle_hidden(),
            Action::FileExplorerToggleGitignored => self.file_explorer_toggle_gitignored(),
            Action::FileExplorerSearchClear => self.file_explorer_search_clear(),
            Action::FileExplorerSearchBackspace => self.file_explorer_search_pop_char(),
            Action::FileExplorerCopy => self.file_explorer_copy(),
            Action::FileExplorerCut => self.file_explorer_cut(),
            Action::FileExplorerPaste => self.file_explorer_paste(),
            Action::FileExplorerExtendSelectionUp => self.file_explorer_extend_selection_up(),
            Action::FileExplorerExtendSelectionDown => self.file_explorer_extend_selection_down(),
            Action::FileExplorerToggleSelect => self.file_explorer_toggle_select(),
            Action::FileExplorerSelectAll => self.file_explorer_select_all(),
            Action::RemoveSecondaryCursors => {
                // Convert action to events and apply them
                if let Some(events) = self.action_to_events(Action::RemoveSecondaryCursors) {
                    // Wrap in batch for atomic undo
                    let batch = Event::Batch {
                        events: events.clone(),
                        description: "Remove secondary cursors".to_string(),
                    };
                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);

                    // Ensure the primary cursor is visible after removing secondary cursors
                    let active_split = self.split_manager.active_split();
                    let active_buffer = self.active_buffer();
                    if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                        let state = self.buffers.get_mut(&active_buffer).unwrap();
                        view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);
                    }
                }
            }

            // Menu navigation actions
            Action::MenuActivate => {
                self.handle_menu_activate();
            }
            Action::MenuClose => {
                self.handle_menu_close();
            }
            Action::MenuLeft => {
                self.handle_menu_left();
            }
            Action::MenuRight => {
                self.handle_menu_right();
            }
            Action::MenuUp => {
                self.handle_menu_up();
            }
            Action::MenuDown => {
                self.handle_menu_down();
            }
            Action::MenuExecute => {
                if let Some(action) = self.handle_menu_execute() {
                    return self.handle_action(action);
                }
            }
            Action::MenuOpen(menu_name) => {
                if self.config.editor.menu_bar_mnemonics {
                    self.handle_menu_open(&menu_name);
                }
            }

            Action::SwitchKeybindingMap(map_name) => {
                // Check if the map exists (either built-in or user-defined)
                let is_builtin =
                    matches!(map_name.as_str(), "default" | "emacs" | "vscode" | "macos");
                let is_user_defined = self.config.keybinding_maps.contains_key(&map_name);

                if is_builtin || is_user_defined {
                    // Update the active keybinding map in config
                    self.config_mut().active_keybinding_map = map_name.clone().into();

                    // Reload the keybinding resolver with the new map
                    *self.keybindings.write().unwrap() =
                        crate::input::keybindings::KeybindingResolver::new(&self.config);

                    self.set_status_message(
                        t!("view.keybindings_switched", map = map_name).to_string(),
                    );
                } else {
                    self.set_status_message(
                        t!("view.keybindings_unknown", map = map_name).to_string(),
                    );
                }
            }

            Action::SmartHome => {
                // In composite (diff) views, use LineStart movement
                let buffer_id = self.active_buffer();
                if self.is_composite_buffer(buffer_id) {
                    if let Some(_handled) =
                        self.handle_composite_action(buffer_id, &Action::SmartHome)
                    {
                        return Ok(());
                    }
                }
                self.smart_home();
            }
            Action::ToggleComment => {
                self.toggle_comment();
            }
            Action::ToggleFold => {
                self.toggle_fold_at_cursor();
            }
            Action::GoToMatchingBracket => {
                self.goto_matching_bracket();
            }
            Action::JumpToNextError => {
                self.jump_to_next_error();
            }
            Action::JumpToPreviousError => {
                self.jump_to_previous_error();
            }
            Action::SetBookmark(key) => {
                self.set_bookmark(key);
            }
            Action::JumpToBookmark(key) => {
                self.jump_to_bookmark(key);
            }
            Action::ClearBookmark(key) => {
                self.clear_bookmark(key);
            }
            Action::ListBookmarks => {
                self.list_bookmarks();
            }
            Action::ToggleSearchCaseSensitive => {
                self.search_case_sensitive = !self.search_case_sensitive;
                let state = if self.search_case_sensitive {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(
                    t!("search.case_sensitive_state", state = state).to_string(),
                );
                // Update incremental highlights if in search prompt, otherwise re-run completed search
                // Check prompt FIRST since we want to use current prompt input, not stale search_state
                if let Some(prompt) = &self.prompt {
                    if matches!(
                        prompt.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch
                    ) {
                        let query = prompt.input.clone();
                        self.update_search_highlights(&query);
                    }
                } else if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::ToggleSearchWholeWord => {
                self.search_whole_word = !self.search_whole_word;
                let state = if self.search_whole_word {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.whole_word_state", state = state).to_string());
                // Update incremental highlights if in search prompt, otherwise re-run completed search
                // Check prompt FIRST since we want to use current prompt input, not stale search_state
                if let Some(prompt) = &self.prompt {
                    if matches!(
                        prompt.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch
                    ) {
                        let query = prompt.input.clone();
                        self.update_search_highlights(&query);
                    }
                } else if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::ToggleSearchRegex => {
                self.search_use_regex = !self.search_use_regex;
                let state = if self.search_use_regex {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.regex_state", state = state).to_string());
                // Update incremental highlights if in search prompt, otherwise re-run completed search
                // Check prompt FIRST since we want to use current prompt input, not stale search_state
                if let Some(prompt) = &self.prompt {
                    if matches!(
                        prompt.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch
                    ) {
                        let query = prompt.input.clone();
                        self.update_search_highlights(&query);
                    }
                } else if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::ToggleSearchConfirmEach => {
                self.search_confirm_each = !self.search_confirm_each;
                let state = if self.search_confirm_each {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.confirm_each_state", state = state).to_string());
            }
            Action::FileBrowserToggleHidden => {
                // Toggle hidden files in file browser (handled via file_open_toggle_hidden)
                self.file_open_toggle_hidden();
            }
            Action::StartMacroRecording => {
                // This is a no-op; use ToggleMacroRecording instead
                self.set_status_message(
                    "Use Ctrl+Shift+R to start recording (will prompt for register)".to_string(),
                );
            }
            Action::StopMacroRecording => {
                self.stop_macro_recording();
            }
            Action::PlayMacro(key) => {
                self.play_macro(key);
            }
            Action::ToggleMacroRecording(key) => {
                self.toggle_macro_recording(key);
            }
            Action::ShowMacro(key) => {
                self.show_macro_in_buffer(key);
            }
            Action::ListMacros => {
                self.list_macros_in_buffer();
            }
            Action::PromptRecordMacro => {
                self.start_prompt("Record macro (0-9): ".to_string(), PromptType::RecordMacro);
            }
            Action::PromptPlayMacro => {
                self.start_prompt("Play macro (0-9): ".to_string(), PromptType::PlayMacro);
            }
            Action::PlayLastMacro => {
                if let Some(key) = self.macros.last_register() {
                    self.play_macro(key);
                } else {
                    self.set_status_message(t!("status.no_macro_recorded").to_string());
                }
            }
            Action::PromptSetBookmark => {
                self.start_prompt("Set bookmark (0-9): ".to_string(), PromptType::SetBookmark);
            }
            Action::PromptJumpToBookmark => {
                self.start_prompt(
                    "Jump to bookmark (0-9): ".to_string(),
                    PromptType::JumpToBookmark,
                );
            }
            Action::CompositeNextHunk => {
                let buf = self.active_buffer();
                self.composite_next_hunk_active(buf);
            }
            Action::CompositePrevHunk => {
                let buf = self.active_buffer();
                self.composite_prev_hunk_active(buf);
            }
            Action::None => {}
            Action::DeleteBackward => {
                if self.is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                // Normal backspace handling
                if let Some(events) = self.action_to_events(Action::DeleteBackward) {
                    if events.len() > 1 {
                        // Multi-cursor: use optimized bulk edit (O(n) instead of O(n²))
                        let description = "Delete backward".to_string();
                        if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description)
                        {
                            self.active_event_log_mut().append(bulk_edit);
                        }
                    } else {
                        for event in events {
                            self.active_event_log_mut().append(event.clone());
                            self.apply_event_to_active_buffer(&event);
                        }
                    }
                }
            }
            Action::PluginAction(action_name) => {
                tracing::debug!("handle_action: PluginAction('{}')", action_name);
                // Execute the plugin callback via TypeScript plugin thread
                // Use non-blocking version to avoid deadlock with async plugin ops
                #[cfg(feature = "plugins")]
                if let Some(result) = self.plugin_manager.execute_action_async(&action_name) {
                    match result {
                        Ok(receiver) => {
                            // Store pending action for processing in main loop
                            self.pending_plugin_actions
                                .push((action_name.clone(), receiver));
                        }
                        Err(e) => {
                            self.set_status_message(
                                t!("view.plugin_error", error = e.to_string()).to_string(),
                            );
                            tracing::error!("Plugin action error: {}", e);
                        }
                    }
                } else {
                    self.set_status_message(t!("status.plugin_manager_unavailable").to_string());
                }
                #[cfg(not(feature = "plugins"))]
                {
                    let _ = action_name;
                    self.set_status_message(
                        "Plugins not available (compiled without plugin support)".to_string(),
                    );
                }
            }
            Action::LoadPluginFromBuffer => {
                #[cfg(feature = "plugins")]
                {
                    let buffer_id = self.active_buffer();
                    let state = self.active_state();
                    let buffer = &state.buffer;
                    let total = buffer.total_bytes();
                    let content =
                        String::from_utf8_lossy(&buffer.slice_bytes(0..total)).to_string();

                    // Determine if TypeScript from file extension, default to TS
                    let is_ts = buffer
                        .file_path()
                        .and_then(|p| p.extension())
                        .and_then(|e| e.to_str())
                        .map(|e| e == "ts" || e == "tsx")
                        .unwrap_or(true);

                    // Derive plugin name from buffer filename
                    let name = buffer
                        .file_path()
                        .and_then(|p| p.file_name())
                        .and_then(|s| s.to_str())
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| "buffer-plugin".to_string());

                    match self
                        .plugin_manager
                        .load_plugin_from_source(&content, &name, is_ts)
                    {
                        Ok(()) => {
                            self.set_status_message(format!(
                                "Plugin '{}' loaded from buffer",
                                name
                            ));
                        }
                        Err(e) => {
                            self.set_status_message(format!("Failed to load plugin: {}", e));
                            tracing::error!("LoadPluginFromBuffer error: {}", e);
                        }
                    }

                    // Set up plugin dev workspace for LSP support
                    self.setup_plugin_dev_lsp(buffer_id, &content);
                }
                #[cfg(not(feature = "plugins"))]
                {
                    self.set_status_message(
                        "Plugins not available (compiled without plugin support)".to_string(),
                    );
                }
            }
            Action::InitReload => {
                // Same code path as auto-load: read init.ts and push it
                // through the existing plugin pipeline. The runtime's
                // hot-reload semantics drop prior commands / handlers /
                // event subs / settings before the new source runs.
                self.load_init_script(true);
                // Re-fire plugins_loaded so handlers expecting a "fresh"
                // post-load environment (M2) see it.
                self.fire_plugins_loaded_hook();
            }
            Action::InitEdit => {
                // Ensure the file exists (create from template if absent),
                // then open it in the editor so users can edit + reload.
                let config_dir = self.dir_context.config_dir.clone();
                match crate::init_script::ensure_starter(&config_dir) {
                    Ok(path) => {
                        // Regenerate `types/plugins.d.ts` from the live plugin
                        // set. It's written once at editor startup, but any
                        // plugin loaded/reloaded/unloaded since then would
                        // leave the aggregate stale (or missing, in builds
                        // where the plugins feature was off at boot but the
                        // user has since enabled a plugin). The user's
                        // tsconfig.json lists this file in `files`, so a
                        // stale copy is exactly when `getPluginApi("foo")`
                        // loses its typed overload.
                        let declarations = self.plugin_manager.plugin_declarations();
                        crate::init_script::write_plugin_declarations(&config_dir, &declarations);
                        match self.open_file(&path) {
                            Ok(_) => {
                                self.set_status_message(format!("init.ts: {}", path.display()));
                            }
                            Err(e) => {
                                self.set_status_message(format!("init.ts: open failed: {e}"));
                            }
                        }
                    }
                    Err(e) => {
                        self.set_status_message(format!("init.ts: create failed: {e}"));
                    }
                }
            }
            Action::InitCheck => {
                // Run the same parse check as `fresh --cmd init check` but
                // surface results in the status bar.
                let report = crate::init_script::check(&self.dir_context.config_dir);
                if report.ok && report.diagnostics.is_empty() {
                    self.set_status_message("init.ts: ok".into());
                } else if !report.ok {
                    let first = report
                        .diagnostics
                        .first()
                        .map(|d| format!("{}:{}: {}", d.line, d.column, d.message))
                        .unwrap_or_else(|| "unknown error".into());
                    self.set_status_message(format!(
                        "init.ts: {} error(s) — first: {first}",
                        report.diagnostics.len()
                    ));
                } else {
                    self.set_status_message(format!(
                        "init.ts: {} warning(s)",
                        report.diagnostics.len()
                    ));
                }
            }
            Action::OpenTerminal => {
                self.open_terminal();
            }
            Action::CloseTerminal => {
                self.close_terminal();
            }
            Action::FocusTerminal => {
                // If viewing a terminal buffer, switch to terminal mode
                if self.is_terminal_buffer(self.active_buffer()) {
                    self.terminal_mode = true;
                    self.key_context = KeyContext::Terminal;
                    self.set_status_message(t!("status.terminal_mode_enabled").to_string());
                }
            }
            Action::TerminalEscape => {
                // Exit terminal mode back to editor
                if self.terminal_mode {
                    self.terminal_mode = false;
                    self.key_context = KeyContext::Normal;
                    self.set_status_message(t!("status.terminal_mode_disabled").to_string());
                }
            }
            Action::ToggleKeyboardCapture => {
                // Toggle keyboard capture mode in terminal
                if self.terminal_mode {
                    self.keyboard_capture = !self.keyboard_capture;
                    if self.keyboard_capture {
                        self.set_status_message(
                            "Keyboard capture ON - all keys go to terminal (F9 to toggle)"
                                .to_string(),
                        );
                    } else {
                        self.set_status_message(
                            "Keyboard capture OFF - UI bindings active (F9 to toggle)".to_string(),
                        );
                    }
                }
            }
            Action::TerminalPaste => {
                // Paste clipboard contents into terminal as a single batch
                if self.terminal_mode {
                    if let Some(text) = self.clipboard.paste() {
                        self.send_terminal_input(text.as_bytes());
                    }
                }
            }
            Action::ShellCommand => {
                // Run shell command on buffer/selection, output to new buffer
                self.start_shell_command_prompt(false);
            }
            Action::ShellCommandReplace => {
                // Run shell command on buffer/selection, replace content
                self.start_shell_command_prompt(true);
            }
            Action::OpenSettings => {
                self.open_settings();
            }
            Action::CloseSettings => {
                // Check if there are unsaved changes
                let has_changes = self
                    .settings_state
                    .as_ref()
                    .is_some_and(|s| s.has_changes());
                if has_changes {
                    // Show confirmation dialog
                    if let Some(ref mut state) = self.settings_state {
                        state.show_confirm_dialog();
                    }
                } else {
                    self.close_settings(false);
                }
            }
            Action::SettingsSave => {
                self.save_settings();
            }
            Action::SettingsReset => {
                if let Some(ref mut state) = self.settings_state {
                    state.reset_current_to_default();
                }
            }
            Action::SettingsInherit => {
                if let Some(ref mut state) = self.settings_state {
                    state.set_current_to_null();
                }
            }
            Action::SettingsToggleFocus => {
                if let Some(ref mut state) = self.settings_state {
                    state.toggle_focus();
                }
            }
            Action::SettingsActivate => {
                self.settings_activate_current();
            }
            Action::SettingsSearch => {
                if let Some(ref mut state) = self.settings_state {
                    state.start_search();
                }
            }
            Action::SettingsHelp => {
                if let Some(ref mut state) = self.settings_state {
                    state.toggle_help();
                }
            }
            Action::SettingsIncrement => {
                self.settings_increment_current();
            }
            Action::SettingsDecrement => {
                self.settings_decrement_current();
            }
            Action::CalibrateInput => {
                self.open_calibration_wizard();
            }
            Action::EventDebug => {
                self.open_event_debug();
            }
            Action::SuspendProcess => {
                self.request_suspend();
            }
            Action::OpenKeybindingEditor => {
                self.open_keybinding_editor();
            }
            Action::PromptConfirm => {
                if let Some((input, prompt_type, selected_index)) = self.confirm_prompt() {
                    use super::prompt_actions::PromptResult;
                    match self.handle_prompt_confirm_input(input, prompt_type, selected_index) {
                        PromptResult::ExecuteAction(action) => {
                            return self.handle_action(action);
                        }
                        PromptResult::EarlyReturn => {
                            return Ok(());
                        }
                        PromptResult::Done => {}
                    }
                }
            }
            Action::PromptConfirmWithText(ref text) => {
                // For macro playback: set the prompt text before confirming
                if let Some(ref mut prompt) = self.prompt {
                    prompt.set_input(text.clone());
                    self.update_prompt_suggestions();
                }
                if let Some((input, prompt_type, selected_index)) = self.confirm_prompt() {
                    use super::prompt_actions::PromptResult;
                    match self.handle_prompt_confirm_input(input, prompt_type, selected_index) {
                        PromptResult::ExecuteAction(action) => {
                            return self.handle_action(action);
                        }
                        PromptResult::EarlyReturn => {
                            return Ok(());
                        }
                        PromptResult::Done => {}
                    }
                }
            }
            Action::PopupConfirm => {
                use super::popup_actions::PopupConfirmResult;
                if let PopupConfirmResult::EarlyReturn = self.handle_popup_confirm() {
                    return Ok(());
                }
            }
            Action::PopupCancel => {
                self.handle_popup_cancel();
            }
            Action::PopupFocus => {
                self.handle_popup_focus();
            }
            Action::CompletionAccept => {
                use super::popup_actions::PopupConfirmResult;
                if let PopupConfirmResult::EarlyReturn = self.handle_popup_confirm() {
                    return Ok(());
                }
            }
            Action::CompletionDismiss => {
                self.handle_popup_cancel();
            }
            Action::InsertChar(c) => {
                if self.is_prompting() {
                    return self.handle_insert_char_prompt(c);
                } else if self.key_context == KeyContext::FileExplorer {
                    self.file_explorer_search_push_char(c);
                } else {
                    self.handle_insert_char_editor(c)?;
                }
            }
            // Prompt clipboard actions
            Action::PromptCopy => {
                if let Some(prompt) = &self.prompt {
                    let text = prompt.selected_text().unwrap_or_else(|| prompt.get_text());
                    if !text.is_empty() {
                        self.clipboard.copy(text);
                        self.set_status_message(t!("clipboard.copied").to_string());
                    }
                }
            }
            Action::PromptCut => {
                if let Some(prompt) = &self.prompt {
                    let text = prompt.selected_text().unwrap_or_else(|| prompt.get_text());
                    if !text.is_empty() {
                        self.clipboard.copy(text);
                    }
                }
                if let Some(prompt) = self.prompt.as_mut() {
                    if prompt.has_selection() {
                        prompt.delete_selection();
                    } else {
                        prompt.clear();
                    }
                }
                self.set_status_message(t!("clipboard.cut").to_string());
                self.update_prompt_suggestions();
            }
            Action::PromptPaste => {
                if let Some(text) = self.clipboard.paste() {
                    if let Some(prompt) = self.prompt.as_mut() {
                        prompt.insert_str(&text);
                    }
                    self.update_prompt_suggestions();
                }
            }
            _ => {
                // TODO: Why do we have this catch-all? It seems like actions should either:
                // 1. Be handled explicitly above (like InsertChar, PopupConfirm, etc.)
                // 2. Or be converted to events consistently
                // This catch-all makes it unclear which actions go through event conversion
                // vs. direct handling. Consider making this explicit or removing the pattern.
                self.apply_action_as_events(action)?;
            }
        }

        Ok(())
    }
}
