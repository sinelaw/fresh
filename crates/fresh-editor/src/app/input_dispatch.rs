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
use rust_i18n::t;

impl Editor {
    /// Dispatch input when in terminal mode.
    ///
    /// Returns `Some(InputResult)` if terminal mode handled the input,
    /// `None` if not in terminal mode or if a modal is active.
    pub fn dispatch_terminal_input(&mut self, event: &KeyEvent) -> Option<InputResult> {
        // Skip if any overlay layer is blocking — a prompt, popup, menu,
        // settings/calibration/keybinding modal, the floating widget panel
        // (Orchestrator picker / new-session form / plugin overlays), or a
        // *focused* dock. A blurred dock leaves the dived-into terminal
        // usable, which is why this is a per-layer `blocks_terminal_input`
        // property and not just "any overlay present." See
        // `Editor::overlay_layers` for the per-layer rationale.
        if self.presents_blocking_overlay() {
            return None;
        }

        // Handle terminal mode input. `focused_terminal_live()` is the derived
        // gate: it is true only when the editor pane owns focus (not the file
        // explorer / a popup), the active buffer is a terminal, and the focused
        // split is not in that terminal's scrollback set. So the two former
        // belt-and-braces guards here — "active buffer is no longer a terminal"
        // and "file explorer stole focus" — are subsumed: both make this false
        // and fall through to normal dispatch.
        if self.active_window().focused_terminal_live() {
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
        // The focus gate matters because the active buffer is still the
        // terminal while the user is off in the file explorer — without it
        // Enter and every other plain key went to the PTY from under the
        // explorer. Issue #2029 was the same class, on the live branch above.
        if self.active_window().editor_pane_owns_keyboard()
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

    /// THE key walk: iterate the owner-stamped overlay stack
    /// ([`Editor::overlay_stack`]) top-down, offering the key to each
    /// layer's declaring component through
    /// [`crate::app::chrome::ChromeComponent::on_layer_key`] — the
    /// keyboard analogue of `dispatch_pointer` walking `hit_stack`
    /// over owner-stamped boxes. A component consuming stops the
    /// walk; a declining layer (`None`) falls through to the next
    /// layer down. Routing is DERIVED from the registered layers —
    /// no kind ladder; a new surface registers a component and its
    /// keys route with no edit here.
    ///
    /// Returns `None` when no layer claims the key, letting
    /// `handle_key` fall through to the pipeline tail (mode bindings,
    /// the composite router, chord/keybinding resolution — slice K4's
    /// migration target).
    ///
    /// Every modal band routes through this walk: the capture-all
    /// modals (Settings, KeybindingEditor, CalibrationWizard, Menu),
    /// the workspace-trust prompt, the prompt rungs
    /// (`dispatch_prompt_key`), and the popup rungs
    /// (`dispatch_popup_keys`) — each reached at its layer's declared
    /// rank, in the same stack `get_key_context()`, the
    /// terminal-input gate and the mouse modal-capture path read.
    ///
    /// RULING — why the keyboard side does NOT get the mouse side's
    /// one-derived-structure-per-event treatment: the stack below is
    /// built once for THE WALK's routing, but handlers may MUTATE
    /// state and then decline (the popup rung processes a deferred
    /// ClosePopup and falls through), so a lower handler's
    /// `get_key_context()` must re-derive against post-mutation
    /// state — a pre-walk snapshot would hand it a stale context.
    /// Same-event is not same-state mid-walk here, unlike the pointer
    /// walks (whose handlers consume whenever they mutate). The
    /// handler-level rebuilds (`chrome/base.rs`, `chrome/dock.rs`)
    /// are therefore load-bearing, not waste; folding them away needs
    /// the invalidation-aware derivation recorded for the
    /// forward-design arc (sinelaw/fresh#3024).
    pub(super) fn dispatch_layer_keyboard(
        &mut self,
        event: &KeyEvent,
    ) -> Option<AnyhowResult<InputResult>> {
        let stack = self.overlay_stack();
        for entry in &stack {
            // `owner: None` is the hardcoded event-debug head — a
            // pre-walk intercept (`handle_key` handled it already).
            let Some(owner) = entry.owner else { continue };
            if let Some(result) =
                crate::app::chrome::components()[owner].on_layer_key(self, &entry.layer, event)
            {
                return Some(result);
            }
        }
        None
    }

    /// Process deferred actions collected during input handling.
    pub fn process_deferred_actions(&mut self, ctx: InputContext) {
        // Deferred actions mutate UI state outside the handle_key/handle_action
        // funnels (they run from the event loop after a handler returns), so
        // spoil the per-generation UI memos here too.
        self.bump_ui_gen();

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

    /// The active overlay toolbar's focus ring, derived from its
    /// layout-box tree exactly the way panel rings are: document order
    /// of focusable boxes. Any focusable kind the plugin puts in the
    /// toolbar joins the ring — nothing here knows which kinds those
    /// are. Empty when there's no toolbar.
    fn overlay_toolbar_keys(&self) -> Vec<String> {
        crate::widgets::layout_box::focus_ring(&self.active_chrome().prompt_toolbar_boxes)
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

    /// Activate the overlay toolbar control with `key` and emit the
    /// resulting `widget_event`s so the plugin can react. Shared by mouse
    /// clicks, Space/Enter on the focused control, and the
    /// `toggleOverlayToolbarWidget` plugin API — one host path for every
    /// way a control can be triggered.
    ///
    /// Dispatch is generic: the control's kind answers the activation
    /// through the same `on_key` machinery registry panels use, queueing
    /// its events on `KeyFx` (a `Toggle` queues `toggle` with the flipped
    /// value, a `Button` queues `activate` — no per-kind match here, and a
    /// future focusable kind participates for free). Toolbar policy is
    /// what stays host-side: the toolbar spec is host-held, so a queued
    /// `toggle` is applied back to it (the host owns the checked state),
    /// and events broadcast with `panel_id: 0` — the toolbar isn't a
    /// registry panel, so there's no owner to target. (Kinds with
    /// per-instance state — open dropdowns, text editors — need the real
    /// registry mount, a recorded later arc; the ephemeral panel here
    /// carries no instance state across events.)
    pub(crate) fn toggle_overlay_toolbar_widget(&mut self, key: &str) {
        if key.is_empty() {
            return;
        }
        let Some(spec_node) = self
            .active_window()
            .prompt
            .as_ref()
            .and_then(|p| p.toolbar_widget.as_ref())
            .and_then(|s| crate::widgets::find_widget_by_key(s, key))
            .cloned()
        else {
            return;
        };
        let mut fx = crate::widgets::kinds::KeyFx::default();
        let mut scratch = crate::widgets::WidgetPanelState {
            buffer_id: crate::model::event::BufferId(0),
            spec: spec_node.clone(),
            hits: Vec::new(),
            instance_states: std::collections::HashMap::new(),
            focus_key: key.to_string(),
            tabbable: Vec::new(),
            effective_rows: std::collections::HashMap::new(),
            boxes: Vec::new(),
        };
        // Every trigger converges on the kind's activation key; Toggle and
        // Button treat Space and Enter identically.
        let _ = crate::widgets::kinds::behavior(&spec_node).on_key(
            &spec_node,
            key,
            &mut scratch,
            "Space",
            &mut fx,
        );
        for (event_type, payload) in fx.events {
            if event_type == "toggle" {
                if let Some(nv) = payload.get("checked").and_then(|v| v.as_bool()) {
                    if let Some(spec) = self
                        .active_window_mut()
                        .prompt
                        .as_mut()
                        .and_then(|p| p.toolbar_widget.as_mut())
                    {
                        crate::widgets::set_toggle_checked_in_spec(spec, key, nv);
                    }
                }
            }
            #[cfg(feature = "plugins")]
            {
                let pm = self.plugin_manager.read().unwrap();
                if pm.has_hook_handlers("widget_event") {
                    pm.run_hook(
                        "widget_event",
                        crate::services::plugins::hooks::HookArgs::WidgetEvent {
                            panel_id: 0,
                            widget_key: key.to_string(),
                            event_type,
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
    }

    /// Handle a key for the overlay's toolbar focus ring. Returns
    /// `Some(Consumed)` when it owns the key, `None` to let normal prompt
    /// handling proceed (also resets focus to the input when the user starts
    /// typing, so typing always edits the query).
    pub(super) fn handle_overlay_toolbar_key(&mut self, event: &KeyEvent) -> Option<InputResult> {
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
