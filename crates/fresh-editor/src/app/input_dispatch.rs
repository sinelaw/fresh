//! Input dispatch using the hierarchical InputHandler system.
//!
//! This module provides the bridge between Editor and the InputHandler trait,
//! dispatching input to modal components and processing deferred actions.

use super::terminal_input::{should_enter_terminal_mode, TerminalModeInputHandler};
use super::Editor;
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crate::input::keybindings::{Action, KeyContext};
use anyhow::Result as AnyhowResult;
use crossterm::event::KeyEvent;
use fresh_i18n::t;

impl Editor {
    /// Dispatch input when in terminal mode.
    ///
    /// Returns `Some(InputResult)` if terminal mode handled the input,
    /// `None` if not in terminal mode or the tree says the terminal is not
    /// taking raw input this frame.
    pub fn dispatch_terminal_input(
        &mut self,
        event: &KeyEvent,
        context: &KeyContext,
    ) -> Option<InputResult> {
        // **The PTY gate is the tree's** (design §3.7.8, §2.3(8)): the live
        // terminal's leaf takes raw input, and `Ui::raw_input` derives
        // whether such a leaf is reachable — no exclusive layer above it,
        // and the keyboard its own. Reached from the base dispatcher in the
        // context the pane's leaf settled, so a key here was routed to the
        // pane by focus; the gate is the tree's statement of the same fact.
        // Handle terminal mode input. `focused_terminal_live()` is the derived
        // gate: it is true only when the editor pane owns focus (not the file
        // explorer / a popup), the active buffer is a terminal, and the focused
        // split is not in that terminal's scrollback set. So the two former
        // belt-and-braces guards here — "active buffer is no longer a terminal"
        // and "file explorer stole focus" — are subsumed: both make this false
        // and fall through to normal dispatch.
        if *context == KeyContext::Terminal
            && self.shell_ui.as_ref().is_some_and(|ui| ui.raw_input())
            && self.active_window().focused_terminal_live()
        {
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
        // The context is the gate: a terminal parked in scroll-back is the
        // pane's plain content, and the key reaches here only when the tree
        // routed it to that content — never from under the explorer or a
        // popup, which name their own contexts (issue #2029 was that class,
        // on the live branch above).
        if *context == KeyContext::Normal
            && self
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
                            settings.paste_into_focused_text(&text);
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
                        let theme_name = prompt.input_str().to_string();
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
                if explicit {
                    // User explicitly exited (Ctrl+]/Escape): drop the focused
                    // split into read-only scrollback (recorded per-split so
                    // refocus keeps it there).
                    self.enter_terminal_scrollback();
                    self.set_status_message(
                        "Terminal mode disabled - read only (Ctrl+Space to resume)".to_string(),
                    );
                }
                // Non-explicit (split navigation): the split stays live — it is
                // never added to the scrollback set — so returning to it resumes
                // the live grid. The upcoming focus change re-derives the key
                // context for the newly focused split; nothing to do here.
            }
            DeferredAction::EnterScrollbackMode => {
                // Shift+PageUp: drop the focused split into scrollback (recorded
                // per-split) and page up through the synced read-only view.
                self.enter_terminal_scrollback();
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
    pub(crate) fn menu_action_to_action(
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
            .map(|p| (p.prompt_type.clone(), p.input_str().to_string()));

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

    /// Activate the overlay toolbar control with `key` and emit the
    /// resulting `widget_event`s so the plugin can react. Shared by the
    /// `toggleOverlayToolbarWidget` plugin API (a plugin's own Alt+… shortcut)
    /// and the web's toolbar clicks — one host path for a control triggered
    /// by name. A press on the control and Space or Enter on it when focused
    /// are the tree's, and reach the same kind through the toolbar panel
    /// (`Slot::PromptToolbar`) like any other panel's.
    ///
    /// Dispatch is generic: the control's kind answers the activation
    /// through the same `on_key` machinery every panel uses, queueing its
    /// events on `KeyFx` (a `Toggle` queues `toggle` with the flipped value,
    /// a `Button` queues `activate` — no per-kind match here). The toolbar is
    /// the plugin's panel `PROMPT_TOOLBAR_PANEL_ID`, so the events reach it
    /// tagged `panel_id: 0`, and the checked state the events carry is what
    /// the plugin re-emits its spec with.
    pub(crate) fn toggle_overlay_toolbar_widget(&mut self, key: &str) {
        if key.is_empty() {
            return;
        }
        let Some(panel_key) = self.prompt_toolbar_key() else {
            return;
        };
        let Some(widget) = self
            .widget_registry
            .get(&panel_key)
            .and_then(|p| crate::widgets::find_widget_by_key(&p.spec, key))
            .cloned()
        else {
            return;
        };
        let mut fx = crate::widgets::kinds::KeyFx::default();
        let Some(panel) = self.widget_registry.get_mut(&panel_key) else {
            return;
        };
        // Every trigger converges on the kind's activation key; Toggle and
        // Button treat Space and Enter identically. A Toggle has no window;
        // the kinds reached here never read it.
        let _ = crate::widgets::kinds::behavior(&widget).on_key(
            &widget,
            key,
            panel,
            crate::widgets::kinds::Viewport::default(),
            "Space",
            &mut fx,
        );
        self.rerender_widget_panel(&panel_key);
        for (event_type, payload) in fx.events {
            self.fire_widget_event(&panel_key, key.to_string(), event_type, payload);
        }
    }

    /// The registry key of the open overlay prompt's toolbar panel, when the
    /// plugin set one.
    pub(crate) fn prompt_toolbar_key(&self) -> Option<crate::widgets::PanelKey> {
        self.active_window().prompt.as_ref()?.toolbar.clone()
    }

    /// Whether a toolbar control has the keyboard rather than the query
    /// input: the toolbar panel's focus fact names one.
    pub(crate) fn prompt_toolbar_holds_keyboard(&self) -> bool {
        self.prompt_toolbar_key()
            .and_then(|k| self.widget_registry.focus_key(&k))
            .is_some_and(|f| !f.is_empty())
    }

    /// The query input has the keyboard back: clear the toolbar panel's focus
    /// fact. The description then marks the input's focus holder instead of
    /// the control, and the tree follows the mark.
    pub(crate) fn release_prompt_toolbar_focus(&mut self) {
        let Some(key) = self.prompt_toolbar_key() else {
            return;
        };
        if !self.prompt_toolbar_holds_keyboard() {
            return;
        }
        self.widget_registry.decide_focus(&key, String::new());
        self.shell_description_stale = true;
    }

    /// Mount or replace the toolbar panel under `key` with `spec`.
    ///
    /// A re-emitted spec keeps the panel's state and focus, clamped onto the
    /// widgets the new spec still has — the same carry every described
    /// panel's update does (`resolve_described_panel`). The toolbar never
    /// had a text projection: it is described in the card's header band, so
    /// nothing is rendered into a buffer here.
    pub(crate) fn mount_prompt_toolbar(
        &mut self,
        key: &crate::widgets::PanelKey,
        spec: fresh_core::api::WidgetSpec,
    ) {
        self.shell_description_stale = true;
        let mounted = self.widget_registry.get(key).is_some();
        if mounted {
            let states = self
                .widget_registry
                .instance_states(key)
                .cloned()
                .unwrap_or_default();
            let focus = self
                .widget_registry
                .focus_key(key)
                .map(str::to_string)
                .unwrap_or_default();
            if self
                .widget_registry
                .update(
                    key,
                    spec,
                    states,
                    focus,
                    std::collections::HashMap::new(),
                    Vec::new(),
                )
                .is_err()
            {
                tracing::warn!("prompt toolbar {key} vanished between the lookup and the update");
            }
        } else {
            self.widget_registry.mount(
                key.clone(),
                crate::app::PROMPT_TOOLBAR_BUFFER_ID,
                spec,
                std::collections::HashMap::new(),
                String::new(),
                std::collections::HashMap::new(),
                Vec::new(),
                // The query input holds the keyboard until a control is
                // given it.
                false,
                false,
            );
        }
        self.rerender_widget_panel(key);
    }

    /// Put `prompt` up in the active window, taking down the one that was
    /// up — and its toolbar panel with it.
    ///
    /// The one way a prompt opens: a prompt assigned over another left the
    /// other's toolbar mounted, and the next toolbar for that plugin found
    /// it and carried a focus the user never gave this session.
    pub(crate) fn set_prompt(&mut self, prompt: crate::view::prompt::Prompt) {
        self.drop_prompt();
        self.active_window_mut().prompt = Some(prompt);
    }

    /// Take the active window's prompt down, and its toolbar panel with it.
    ///
    /// The one way a prompt closes: the toolbar's registry entry lives
    /// exactly as long as the prompt that shows it.
    pub(crate) fn drop_prompt(&mut self) -> Option<crate::view::prompt::Prompt> {
        let prompt = self.active_window_mut().prompt.take()?;
        if let Some(key) = &prompt.toolbar {
            let _ = self.widget_registry.unmount(key);
            self.shell_description_stale = true;
        }
        Some(prompt)
    }

    /// The toolbar panel's interior for the card's header band, when the open
    /// overlay prompt has one. Built the way `panel_interior` builds the
    /// dock's, minus the slot-bound facts a toolbar does not have: it holds
    /// the keyboard whenever the prompt is up, reserves no marker gutter and
    /// carries no row budget.
    pub(crate) fn prompt_toolbar_interior(&self) -> Option<crate::view::shell::panel::Interior> {
        use std::rc::Rc;
        let key = self.prompt_toolbar_key()?;
        let spec = self.widget_registry.get(&key)?.spec.clone();
        let (hovered, hovered_item) = self.widget_registry.hover_keys(&key);
        Some(crate::view::shell::panel::Interior {
            spec: Rc::new(spec),
            states: Rc::new(
                self.widget_registry
                    .instance_states(&key)
                    .cloned()
                    .unwrap_or_default(),
            ),
            focus_key: self
                .widget_registry
                .focus_key(&key)
                .map(|s| s.to_string())
                .unwrap_or_default(),
            keyboard: true,
            page: None,
            hovered_key: Some(hovered).filter(|k| !k.is_empty()),
            hovered_item_key: hovered_item,
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
            keymap: None,
            markdown: Some(self.markdown_ink()),
        })
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
