//! Input dispatch using the hierarchical InputHandler system.
//!
//! This module provides the bridge between Editor and the InputHandler trait,
//! dispatching input to modal components and processing deferred actions.

use super::terminal_input::{should_enter_terminal_mode, TerminalModeInputHandler};
use super::Editor;
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crate::input::keybindings::{Action, KeyContext};
use crate::view::file_browser_input::FileBrowserInputHandler;
use crate::view::query_replace_input::QueryReplaceConfirmInputHandler;
use crate::view::ui::MenuInputHandler;
use anyhow::Result as AnyhowResult;
use crossterm::event::KeyEvent;
use rust_i18n::t;

impl Editor {
    /// Dispatch input when in terminal mode.
    ///
    /// Returns `Some(InputResult)` if terminal mode handled the input,
    /// `None` if not in terminal mode or if a modal is active.
    pub fn dispatch_terminal_input(&mut self, event: &KeyEvent) -> Option<InputResult> {
        // Skip if we're in a prompt/popup (those need to handle keys normally)
        // — including the floating widget panel (Orchestrator picker,
        // new-session form, plugin overlays), which is the editor-wide
        // modal owner of the keyboard while it's up. Without this skip,
        // a terminal-buffer-active window with `terminal_mode=true` would
        // route keys to the PTY child even when the user's keystrokes
        // are meant for the picker on top of it.
        let in_modal = self.is_prompting()
            || self.global_popups.is_visible()
            || self.active_state().popups.is_visible()
            || self.menu_state.active_menu.is_some()
            || self.settings_state.as_ref().is_some_and(|s| s.visible)
            || self.calibration_wizard.is_some()
            || self.keybinding_editor.is_some()
            || self.floating_widget_panel.is_some()
            // The dock blocks PTY routing only while it owns the keyboard;
            // a blurred dock leaves the dived-into terminal usable.
            || self.dock.as_ref().is_some_and(|f| f.focused);

        if in_modal {
            return None;
        }

        // Handle terminal mode input
        if self.active_window().terminal_mode {
            // If the user navigated away from the terminal buffer (e.g. opened
            // Review Diff via the command palette), the active buffer is no
            // longer a terminal. Exit terminal mode so the new buffer's
            // keybindings work.
            if !self
                .active_window()
                .is_terminal_buffer(self.active_buffer())
            {
                self.active_window_mut().terminal_mode = false;
                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::Normal;
                return None; // fall through to normal input dispatch
            }
            // Keyboard focus has been explicitly handed to the file
            // explorer (issue #2029, sub-issue 1). Skip the PTY route
            // even though `terminal_mode` is still set, so arrow keys
            // reach the explorer instead of being swallowed by the
            // shell. The `terminal_mode` flag is cleared up front by
            // `take_focus_for_file_explorer`; this is a belt-and-braces
            // guard against any async path that re-enables
            // `terminal_mode` while file-explorer focus is legitimate.
            if matches!(
                self.active_window().key_context,
                crate::input::keybindings::KeyContext::FileExplorer
            ) {
                return None;
            }
            // Plugin commands flagged `terminalBypass: true` (via
            // `editor.registerCommand(..., { terminalBypass: true })`)
            // resolve to actions that must reach the editor even
            // when a terminal pane owns the keyboard — that's how
            // bound shortcuts to commands like `Orchestrator: Open`
            // stay reachable from inside `top`/`htop`/a shell.
            // Resolve the key against the regular (Normal) context;
            // if it's a registered bypass action, dispatch it and
            // return *before* the terminal handler claims the key.
            // Builtin UI actions (CommandPalette, QuickOpen, …)
            // still flow through `TerminalModeInputHandler`'s own
            // `is_terminal_ui_action` allowlist below.
            let bypass_action = {
                let keybindings = self.keybindings.read().unwrap();
                let action = keybindings.resolve(event, KeyContext::Normal);
                if self
                    .command_registry
                    .read()
                    .unwrap()
                    .is_terminal_bypass_action(&action)
                {
                    Some(action)
                } else {
                    None
                }
            };
            if let Some(action) = bypass_action {
                if let Err(e) = self.handle_action(action) {
                    tracing::warn!("terminal-bypass action failed: {e}");
                }
                return Some(InputResult::Consumed);
            }
            let mut ctx = InputContext::new();
            let keyboard_capture = self.active_window().keyboard_capture;
            let keybindings = self.keybindings.read().unwrap();
            let mut handler = TerminalModeInputHandler::new(keyboard_capture, &keybindings);
            let result = handler.dispatch_input(event, &mut ctx);
            drop(keybindings);
            self.process_deferred_actions(ctx);
            return Some(result);
        }

        // Check for keys that should re-enter terminal mode from scrollback view.
        // Any plain character key exits scrollback and is forwarded to the terminal.
        if self
            .active_window()
            .is_terminal_buffer(self.active_buffer())
            && should_enter_terminal_mode(event)
        {
            self.enter_terminal_mode();
            // Forward the key to the terminal so the user's input isn't lost
            self.active_window_mut()
                .send_terminal_key(event.code, event.modifiers);
            return Some(InputResult::Consumed);
        }

        None
    }

    /// Dispatch input to the appropriate modal handler.
    ///
    /// Returns `Some(InputResult)` if a modal handled the input,
    /// `None` if no modal is active and input should be handled normally.
    pub fn dispatch_modal_input(&mut self, event: &KeyEvent) -> Option<InputResult> {
        let mut ctx = InputContext::new();

        // Settings has highest priority
        if let Some(ref mut settings) = self.settings_state {
            if settings.visible {
                let result = settings.dispatch_input(event, &mut ctx);
                self.process_deferred_actions(ctx);
                return Some(result);
            }
        }

        // Keybinding editor is next
        if self.keybinding_editor.is_some() {
            let result = self.handle_keybinding_editor_input(event);
            return Some(result);
        }

        // Calibration wizard is next (modal, blocks all other input)
        if self.calibration_wizard.is_some() {
            let result = self.handle_calibration_input(event);
            return Some(result);
        }

        // Menu is next
        if self.menu_state.active_menu.is_some() {
            let all_menus: Vec<crate::config::Menu> = self
                .menus
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            let mut handler = MenuInputHandler::new(&mut self.menu_state, &all_menus);
            let result = handler.dispatch_input(event, &mut ctx);
            self.process_deferred_actions(ctx);
            return Some(result);
        }

        // Prompt is next
        if self.active_window().prompt.is_some() {
            // Check for Alt+key keybindings in Prompt context first
            // Use resolve_in_context_only to bypass Global bindings (like menu mnemonics)
            // This allows Prompt-specific Alt+key bindings (like encoding toggle) to work
            if event
                .modifiers
                .contains(crossterm::event::KeyModifiers::ALT)
            {
                if let crossterm::event::KeyCode::Char(_) = event.code {
                    let prompt_action = self.keybindings.read().unwrap().resolve_in_context_only(
                        event,
                        crate::input::keybindings::KeyContext::Prompt,
                    );
                    if let Some(action) = prompt_action {
                        // For file browser actions, route to handle_file_open_action
                        if self.is_file_open_active() && self.handle_file_open_action(&action) {
                            return Some(InputResult::Consumed);
                        }
                        // For other prompt actions, use handle_action
                        if let Err(e) = self.handle_action(action) {
                            tracing::warn!("Prompt action failed: {}", e);
                        }
                        return Some(InputResult::Consumed);
                    }
                }
            }

            // File browser prompts use FileBrowserInputHandler
            if self.is_file_open_active() {
                let active_window_id = self.active_window;
                let __win = self
                    .windows
                    .get_mut(&active_window_id)
                    .expect("active window present");
                if let (Some(ref mut file_state), Some(ref mut prompt)) =
                    (&mut __win.file_open_state, &mut __win.prompt)
                {
                    let mut handler = FileBrowserInputHandler::new(file_state, prompt);
                    let result = handler.dispatch_input(event, &mut ctx);
                    self.process_deferred_actions(ctx);
                    return Some(result);
                }
            }

            // QueryReplaceConfirm prompts use QueryReplaceConfirmInputHandler
            use crate::view::prompt::PromptType;
            let is_query_replace_confirm = self
                .active_window()
                .prompt
                .as_ref()
                .is_some_and(|p| p.prompt_type == PromptType::QueryReplaceConfirm);
            if is_query_replace_confirm {
                let mut handler = QueryReplaceConfirmInputHandler::new();
                let result = handler.dispatch_input(event, &mut ctx);
                self.process_deferred_actions(ctx);
                return Some(result);
            }

            // Universal Search overlay focus ring: Tab/Shift+Tab move focus
            // between the query input and the scope toggles; Space/Enter
            // activate the focused toggle. Intercepted before the prompt's own
            // input handling so Tab doesn't fall through to other behaviour.
            if let Some(result) = self.handle_overlay_toolbar_key(event) {
                return Some(result);
            }

            if let Some(ref mut prompt) = self.active_window_mut().prompt {
                let result = prompt.dispatch_input(event, &mut ctx);
                // Only return and process deferred actions if the prompt handled the input
                // If Ignored, fall through to check global keybindings
                if result != InputResult::Ignored {
                    self.process_deferred_actions(ctx);
                    return Some(result);
                }
            }
        }

        // Editor-pane popups (global + buffer) belong to the editor pane and
        // must not capture input when the file explorer is the focused pane.
        // Mirrors the priority encoded in `get_key_context()` via the same
        // `popups_capture_keys()` predicate so the two paths cannot drift —
        // one source of truth for "is the popup eligible to eat this key?".
        if self.popups_capture_keys() {
            // Completion popups consult the keybinding resolver in the
            // `Completion` context first, so accept/dismiss can be remapped
            // via the keybinding editor. Falls through to the popup's own
            // handler for everything else (type-to-filter, navigation, etc.).
            if let Some(action) = self.resolve_completion_popup_action(event) {
                self.process_deferred_actions(ctx);
                if let Err(e) = self.handle_action(action) {
                    tracing::warn!("Completion popup action failed: {}", e);
                }
                return Some(InputResult::Consumed);
            }

            // The workspace-trust prompt is a bespoke modal with its own keys
            // (mnemonics select-and-confirm, Q quits, Esc is inert). Intercept
            // before the generic popup handler so list type-to-filter etc.
            // never swallow them.
            if self.global_popups.top().is_some_and(|p| {
                matches!(
                    p.resolver,
                    crate::view::popup::PopupResolver::WorkspaceTrust
                )
            }) {
                if let Some(result) = self.handle_workspace_trust_key(event) {
                    return Some(result);
                }
            }

            // Editor-level (global) popups take precedence over buffer popups
            // so that plugin notifications stay focused even when the active
            // buffer owns its own popup stack.
            if self.global_popups.is_visible() {
                let result = self.global_popups.dispatch_input(event, &mut ctx);
                self.process_deferred_actions(ctx);
                if result != InputResult::Ignored {
                    return Some(result);
                }
                // Re-check visibility — the dispatch may have queued a
                // ClosePopup that the deferred-action processor has now fired.
                return None;
            }

            // Popup is next
            if self.active_state().popups.is_visible() {
                let result = self
                    .active_state_mut()
                    .popups
                    .dispatch_input(event, &mut ctx);
                self.process_deferred_actions(ctx);
                // If the popup handler returned Ignored (e.g., non-word
                // character, Ctrl+key, arrow keys), fall through to normal
                // input handling. The deferred ClosePopup action was already
                // processed above.
                if result != InputResult::Ignored {
                    return Some(result);
                }
            }
        }

        None
    }

    /// Process deferred actions collected during input handling.
    pub fn process_deferred_actions(&mut self, ctx: InputContext) {
        // Set status message if provided
        if let Some(msg) = ctx.status_message {
            self.set_status_message(msg);
        }

        // Process each deferred action
        for action in ctx.deferred_actions {
            if let Err(e) = self.execute_deferred_action(action) {
                self.set_status_message(
                    t!("error.deferred_action", error = e.to_string()).to_string(),
                );
            }
        }
    }

    /// Execute a single deferred action.
    fn execute_deferred_action(&mut self, action: DeferredAction) -> AnyhowResult<()> {
        match action {
            // Settings actions
            DeferredAction::CloseSettings { save } => {
                if save {
                    self.save_settings();
                }
                self.close_settings(false);
            }
            DeferredAction::PasteToSettings => {
                if let Some(text) = self.clipboard.paste() {
                    if !text.is_empty() {
                        if let Some(settings) = &mut self.settings_state {
                            if let Some(dialog) = settings.entry_dialog_mut() {
                                dialog.insert_str(&text);
                            }
                        }
                    }
                }
            }
            DeferredAction::OpenConfigFile { layer } => {
                self.open_config_file(layer)?;
            }

            // Menu actions
            DeferredAction::CloseMenu => {
                self.close_menu_with_auto_hide();
            }
            DeferredAction::ExecuteMenuAction { action, args } => {
                // Convert menu action to keybinding Action and execute
                if let Some(kb_action) = self.menu_action_to_action(&action, args) {
                    self.handle_action(kb_action)?;
                }
            }

            // Prompt actions
            DeferredAction::ClosePrompt => {
                self.cancel_prompt();
            }
            DeferredAction::ConfirmPrompt => {
                self.handle_action(Action::PromptConfirm)?;
            }
            DeferredAction::UpdatePromptSuggestions => {
                self.update_prompt_suggestions();
            }
            DeferredAction::PromptHistoryPrev => {
                self.prompt_history_prev();
            }
            DeferredAction::PromptHistoryNext => {
                self.prompt_history_next();
            }
            DeferredAction::PreviewThemeFromPrompt => {
                if let Some(prompt) = &self.active_window_mut().prompt {
                    if matches!(
                        prompt.prompt_type,
                        crate::view::prompt::PromptType::SelectTheme { .. }
                    ) {
                        let theme_name = prompt.input.clone();
                        self.preview_theme(&theme_name);
                    }
                }
            }
            DeferredAction::PromptSelectionChanged { selected_index } => {
                // Fire hook for plugin prompts so they can update live preview
                let plugin_custom_type =
                    self.active_window()
                        .prompt
                        .as_ref()
                        .and_then(|p| match &p.prompt_type {
                            crate::view::prompt::PromptType::Plugin { custom_type } => {
                                Some(custom_type.clone())
                            }
                            _ => None,
                        });
                if let Some(custom_type) = plugin_custom_type {
                    self.plugin_manager.read().unwrap().run_hook(
                        "prompt_selection_changed",
                        crate::services::plugins::hooks::HookArgs::PromptSelectionChanged {
                            prompt_type: custom_type.clone(),
                            selected_index,
                        },
                    );
                }
            }

            // Popup actions
            DeferredAction::ClosePopup => {
                // Route through handle_popup_cancel so popup-specific
                // cleanup runs (e.g. the LSP auto-prompt needs to mark
                // the language as prompted and drop the pending queue
                // entry — otherwise the render-time drain would just
                // re-open the popup on the next frame, defeating Esc).
                self.handle_popup_cancel();
            }
            DeferredAction::ConfirmPopup => {
                self.handle_action(Action::PopupConfirm)?;
            }
            DeferredAction::PopupTypeChar(c) => {
                self.handle_popup_type_char(c);
            }
            DeferredAction::PopupBackspace => {
                self.handle_popup_backspace();
            }
            DeferredAction::CopyToClipboard(text) => {
                self.clipboard.copy(text);
                self.set_status_message(t!("clipboard.copied").to_string());
            }

            // Generic action execution
            DeferredAction::ExecuteAction(kb_action) => {
                self.handle_action(kb_action)?;
            }

            // Character insertion with suggestion update
            DeferredAction::InsertCharAndUpdate(c) => {
                if let Some(ref mut prompt) = self.active_window_mut().prompt {
                    prompt.insert_char(c);
                }
                self.update_prompt_suggestions();
            }

            // File browser actions
            DeferredAction::FileBrowserSelectPrev => {
                if let Some(state) = &mut self.active_window_mut().file_open_state {
                    state.select_prev();
                }
            }
            DeferredAction::FileBrowserSelectNext => {
                if let Some(state) = &mut self.active_window_mut().file_open_state {
                    state.select_next();
                }
            }
            DeferredAction::FileBrowserPageUp => {
                if let Some(state) = &mut self.active_window_mut().file_open_state {
                    state.page_up(10);
                }
            }
            DeferredAction::FileBrowserPageDown => {
                if let Some(state) = &mut self.active_window_mut().file_open_state {
                    state.page_down(10);
                }
            }
            DeferredAction::FileBrowserConfirm => {
                // Must call handle_file_open_action directly to get proper
                // file browser behavior (e.g., project switch triggering restart)
                self.handle_file_open_action(&Action::PromptConfirm);
            }
            DeferredAction::FileBrowserAcceptSuggestion => {
                self.handle_file_open_action(&Action::PromptAcceptSuggestion);
            }
            DeferredAction::FileBrowserGoParent => {
                // Navigate to parent directory
                let parent = self
                    .active_window_mut()
                    .file_open_state
                    .as_ref()
                    .and_then(|s| s.current_dir.parent())
                    .map(|p| p.to_path_buf());
                if let Some(parent_path) = parent {
                    self.load_file_open_directory(parent_path);
                }
            }
            DeferredAction::FileBrowserUpdateFilter => {
                self.update_file_open_filter();
            }
            DeferredAction::FileBrowserToggleHidden => {
                self.file_open_toggle_hidden();
            }

            // Interactive replace actions
            DeferredAction::InteractiveReplaceKey(c) => {
                self.handle_interactive_replace_key(c)?;
            }
            DeferredAction::CancelInteractiveReplace => {
                self.cancel_prompt();
                self.active_window_mut().interactive_replace_state = None;
            }

            // Terminal mode actions
            DeferredAction::ToggleKeyboardCapture => {
                self.active_window_mut().keyboard_capture =
                    !self.active_window_mut().keyboard_capture;
                if self.active_window_mut().keyboard_capture {
                    self.set_status_message(
                        "Keyboard capture ON - all keys go to terminal (F9 to toggle)".to_string(),
                    );
                } else {
                    self.set_status_message(
                        "Keyboard capture OFF - UI bindings active (F9 to toggle)".to_string(),
                    );
                }
            }
            DeferredAction::SendTerminalKey(code, modifiers) => {
                self.active_window_mut().send_terminal_key(code, modifiers);
            }
            DeferredAction::SendTerminalMouse {
                col,
                row,
                kind,
                modifiers,
            } => {
                self.active_window_mut()
                    .send_terminal_mouse(col, row, kind, modifiers);
            }
            DeferredAction::ExitTerminalMode { explicit } => {
                self.active_window_mut().terminal_mode = false;
                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::Normal;
                if explicit {
                    // User explicitly exited - don't auto-resume when switching back
                    let buf = self.active_buffer();
                    self.active_window_mut().terminal_mode_resume.remove(&buf);
                    {
                        let __b = self.active_buffer();
                        self.active_window_mut().sync_terminal_to_buffer(__b);
                    };
                    self.set_status_message(
                        "Terminal mode disabled - read only (Ctrl+Space to resume)".to_string(),
                    );
                }
            }
            DeferredAction::EnterScrollbackMode => {
                self.active_window_mut().terminal_mode = false;
                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::Normal;
                {
                    let __b = self.active_buffer();
                    self.active_window_mut().sync_terminal_to_buffer(__b);
                };
                self.set_status_message(
                    "Scrollback mode - use PageUp/Down to scroll (Ctrl+Space to resume)"
                        .to_string(),
                );
                // Scroll up using normal buffer scrolling
                self.handle_action(Action::MovePageUp)?;
            }
            DeferredAction::EnterTerminalMode => {
                self.enter_terminal_mode();
            }
        }

        Ok(())
    }

    /// Convert a menu action string to a keybinding Action.
    fn menu_action_to_action(
        &self,
        action_name: &str,
        args: std::collections::HashMap<String, serde_json::Value>,
    ) -> Option<Action> {
        // Try to parse as a built-in action first
        if let Some(action) = Action::from_str(action_name, &args) {
            return Some(action);
        }

        // Otherwise treat as a plugin action
        Some(Action::PluginAction(action_name.to_string()))
    }

    /// Navigate to previous history entry in prompt.
    fn prompt_history_prev(&mut self) {
        // Get the prompt type and current input
        let prompt_info = self
            .active_window()
            .prompt
            .as_ref()
            .map(|p| (p.prompt_type.clone(), p.input.clone()));

        if let Some((prompt_type, current_input)) = prompt_info {
            // Get the history key for this prompt type
            if let Some(key) = Self::prompt_type_to_history_key(&prompt_type) {
                if let Some(history) = self.active_window_mut().prompt_histories.get_mut(&key) {
                    if let Some(entry) = history.navigate_prev(&current_input) {
                        if let Some(ref mut prompt) = self.active_window_mut().prompt {
                            prompt.set_input(entry);
                        }
                    }
                }
            }
        }
    }

    /// Navigate to next history entry in prompt.
    fn prompt_history_next(&mut self) {
        let prompt_type = self
            .active_window()
            .prompt
            .as_ref()
            .map(|p| p.prompt_type.clone());

        if let Some(prompt_type) = prompt_type {
            // Get the history key for this prompt type
            if let Some(key) = Self::prompt_type_to_history_key(&prompt_type) {
                if let Some(history) = self.active_window_mut().prompt_histories.get_mut(&key) {
                    if let Some(entry) = history.navigate_next() {
                        if let Some(ref mut prompt) = self.active_window_mut().prompt {
                            prompt.set_input(entry);
                        }
                    }
                }
            }
        }
    }

    /// Ordered toggle keys of the active overlay's widget toolbar (render
    /// order). Drives the focus ring. Empty when there's no toolbar.
    fn overlay_toolbar_keys(&self) -> Vec<String> {
        self.active_chrome()
            .prompt_toolbar_hits
            .iter()
            .map(|(k, _)| k.clone())
            .collect()
    }

    /// Advance (or retreat) the overlay focus ring: input → toggle0 → … →
    /// toggleN → input. No-op (returns false) unless an overlay prompt with a
    /// toolbar is active.
    fn cycle_overlay_focus(&mut self, forward: bool) -> bool {
        if !self.overlay_prompt_active() {
            return false;
        }
        let has_toolbar = self
            .active_window()
            .prompt
            .as_ref()
            .is_some_and(|p| p.toolbar_widget.is_some());
        if !has_toolbar {
            return false;
        }
        let keys = self.overlay_toolbar_keys();
        if keys.is_empty() {
            return false;
        }
        let cur = self
            .active_window()
            .prompt
            .as_ref()
            .and_then(|p| p.toolbar_focus.clone());
        // Ring includes the input as the `None` slot.
        let next: Option<String> = match cur {
            None => Some(if forward {
                keys[0].clone()
            } else {
                keys[keys.len() - 1].clone()
            }),
            Some(k) => match keys.iter().position(|x| x == &k) {
                Some(i) if forward => keys.get(i + 1).cloned(), // None past the end → input
                Some(i) => {
                    if i == 0 {
                        None
                    } else {
                        keys.get(i - 1).cloned()
                    }
                }
                None => None, // stale key → input
            },
        };
        if let Some(p) = self.active_window_mut().prompt.as_mut() {
            p.toolbar_focus = next;
        }
        true
    }

    /// Fire the focused toolbar control's toggle. The host owns the checked
    /// state, so this flips it and emits a `widget_event` (see
    /// `toggle_overlay_toolbar_widget`); the plugin reacts.
    fn activate_focused_overlay_toggle(&mut self) {
        let key = self
            .active_window()
            .prompt
            .as_ref()
            .and_then(|p| p.toolbar_focus.clone());
        if let Some(key) = key {
            self.toggle_overlay_toolbar_widget(&key);
        }
    }

    /// Activate the overlay toolbar control with `key` and emit a
    /// `widget_event` so the plugin can react. For a `Toggle` the host owns
    /// the checked state — it flips it in place and emits `toggle`
    /// (`{checked}`). For a `Button` it emits `activate` (`{}`). Shared by
    /// mouse clicks, Space/Enter on the focused control, and the
    /// `toggleOverlayToolbarWidget` plugin API — one host path for every way
    /// a control can be triggered.
    pub(crate) fn toggle_overlay_toolbar_widget(&mut self, key: &str) {
        if key.is_empty() {
            return;
        }
        // Resolve what event to emit, flipping a toggle's checked state in
        // place. `None` → the key isn't a toggle/button (no-op).
        let event: Option<(&'static str, serde_json::Value)> = {
            let Some(prompt) = self.active_window_mut().prompt.as_mut() else {
                return;
            };
            let Some(spec) = prompt.toolbar_widget.as_mut() else {
                return;
            };
            match crate::widgets::find_widget_by_key(spec, key) {
                Some(fresh_core::api::WidgetSpec::Toggle { checked, .. }) => {
                    let nv = !*checked;
                    crate::widgets::set_toggle_checked_in_spec(spec, key, nv);
                    Some(("toggle", serde_json::json!({ "checked": nv })))
                }
                Some(fresh_core::api::WidgetSpec::Button { .. }) => {
                    Some(("activate", serde_json::json!({})))
                }
                _ => None,
            }
        };
        let Some((event_type, payload)) = event else {
            return;
        };
        #[cfg(feature = "plugins")]
        {
            let pm = self.plugin_manager.read().unwrap();
            if pm.has_hook_handlers("widget_event") {
                pm.run_hook(
                    "widget_event",
                    crate::services::plugins::hooks::HookArgs::WidgetEvent {
                        panel_id: 0,
                        widget_key: key.to_string(),
                        event_type: event_type.to_string(),
                        payload,
                    },
                );
            }
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = (event_type, payload);
        }
    }

    /// Handle a key for the overlay's toolbar focus ring. Returns
    /// `Some(Consumed)` when it owns the key, `None` to let normal prompt
    /// handling proceed (also resets focus to the input when the user starts
    /// typing, so typing always edits the query).
    fn handle_overlay_toolbar_key(&mut self, event: &KeyEvent) -> Option<InputResult> {
        use crossterm::event::{KeyCode, KeyModifiers};
        if !self.overlay_prompt_active() {
            return None;
        }
        let has_toolbar = self
            .active_window()
            .prompt
            .as_ref()
            .is_some_and(|p| p.toolbar_widget.is_some());
        if !has_toolbar {
            return None;
        }
        let focused = self
            .active_window()
            .prompt
            .as_ref()
            .is_some_and(|p| p.toolbar_focus.is_some());
        let shift = event.modifiers.contains(KeyModifiers::SHIFT);
        match event.code {
            KeyCode::BackTab => {
                self.cycle_overlay_focus(false);
                Some(InputResult::Consumed)
            }
            KeyCode::Tab => {
                self.cycle_overlay_focus(!shift);
                Some(InputResult::Consumed)
            }
            KeyCode::Char(' ') | KeyCode::Enter if focused => {
                self.activate_focused_overlay_toggle();
                Some(InputResult::Consumed)
            }
            // Navigating the result list (or typing) returns focus to the
            // query input, then falls through so the navigation / character
            // insertion happens — and Enter afterwards opens the highlighted
            // result rather than re-activating a control.
            KeyCode::Up
            | KeyCode::Down
            | KeyCode::PageUp
            | KeyCode::PageDown
            | KeyCode::Char(_)
                if focused =>
            {
                if let Some(p) = self.active_window_mut().prompt.as_mut() {
                    p.toolbar_focus = None;
                }
                None
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deferred_action_close_menu() {
        // This is a basic structure test - full integration tests
        // would require a complete Editor setup
        let action = DeferredAction::CloseMenu;
        assert!(matches!(action, DeferredAction::CloseMenu));
    }

    #[test]
    fn test_deferred_action_execute_menu_action() {
        let action = DeferredAction::ExecuteMenuAction {
            action: "save".to_string(),
            args: std::collections::HashMap::new(),
        };
        if let DeferredAction::ExecuteMenuAction { action: name, .. } = action {
            assert_eq!(name, "save");
        } else {
            panic!("Expected ExecuteMenuAction");
        }
    }
}
