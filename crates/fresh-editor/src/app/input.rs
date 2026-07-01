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
        if let Some(callback_id) = self
            .active_window_mut()
            .pending_next_key_callbacks
            .pop_front()
        {
            let json = serde_json::to_string(&payload).unwrap_or_else(|_| "null".to_string());
            self.plugin_manager
                .read()
                .unwrap()
                .resolve_callback(callback_id, json);
            return true;
        }
        if self.active_window_mut().key_capture_active {
            self.active_window_mut()
                .pending_key_capture_buffer
                .push_back(payload);
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
        use crate::view::popup::PopupResolver;
        // The workspace-trust prompt is an editor-wide modal shown at startup:
        // it must own the keyboard regardless of which pane is focused.
        // Opening a *directory* focuses the file-explorer pane, which would
        // otherwise short-circuit below and leave the (rendered) prompt
        // un-interactable.
        let trust_prompt_up = self
            .global_popups
            .top()
            .is_some_and(|p| p.focused && matches!(p.resolver, PopupResolver::WorkspaceTrust));
        if trust_prompt_up {
            return true;
        }
        if matches!(self.active_window().key_context, KeyContext::FileExplorer) {
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
        //
        // Ask the overlay stack directly rather than re-listing the modal
        // fields: any layer ranked *above* the popup layer that owns the
        // keyboard is exactly Settings / Menu / Prompt (the only layers
        // above Popup). `popup_visible` above guarantees a Popup layer is
        // present, so `take_while` stops before the editor base layer.
        let blocked_by_higher_modal = self
            .overlay_layers()
            .iter()
            .take_while(|l| l.kind != crate::app::overlay::LayerKind::Popup)
            .any(|l| l.owns_keyboard);
        if blocked_by_higher_modal {
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
            kb.resolve_in_context_only(event, self.active_window().key_context.clone()),
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

    /// Build the editor's overlay stack, ordered top-first (highest
    /// keyboard-focus precedence first), ending with the always-present
    /// editor base layer.
    ///
    /// This is the single source of truth for overlay precedence: focus
    /// resolution (`get_key_context`), the unfocused-popup modal guard
    /// (`resolve_unfocused_popup_action`), the terminal-input gate
    /// (`dispatch_terminal_input`), and the mouse early-capture ladder
    /// (`handle_mouse`) all read from this list rather than keeping their
    /// own conditional ladders.
    pub(crate) fn overlay_layers(&self) -> Vec<crate::app::overlay::Layer> {
        use crate::app::overlay::{Layer, LayerKind};
        use crate::input::keybindings::KeyContext;

        let mut layers = Vec::new();

        // Event-debug dialog intercepts every key event ahead of every
        // other path (see `handle_key_event`), so it sits at the top of
        // the stack. Its dispatcher is custom (no `KeyContext`).
        if self.active_window().is_event_debug_active() {
            layers.push(Layer {
                kind: LayerKind::EventDebug,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        // Full-screen modals own the keyboard whenever they are present.
        if self.settings_state.as_ref().is_some_and(|s| s.visible) {
            layers.push(Layer {
                kind: LayerKind::Settings,
                owns_keyboard: true,
                key_context: Some(KeyContext::Settings),
                blocks_terminal_input: true,
            });
        }
        // Keybinding editor and calibration wizard install their own
        // input dispatchers (see `input_dispatch.rs`), so they are
        // transparent to `KeyContext`-driven keybinding resolution
        // (`key_context: None`) — but they fully own the keyboard while
        // present and block PTY routing.
        if self.keybinding_editor.is_some() {
            layers.push(Layer {
                kind: LayerKind::KeybindingEditor,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        if self.calibration_wizard.is_some() {
            layers.push(Layer {
                kind: LayerKind::CalibrationWizard,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        // The workspace-trust prompt is a `global_popups` entry with its
        // own modal z-band, key handler and mouse handler. When it's the
        // top of the global stack it takes the place of the generic
        // `Popup` layer so the dedicated handlers can be reached by
        // top-down kind dispatch (`handle_mouse`, `input_dispatch`).
        let trust_on_top = self.global_popups.top().is_some_and(|p| {
            matches!(
                p.resolver,
                crate::view::popup::PopupResolver::WorkspaceTrust
            )
        });
        if trust_on_top {
            layers.push(Layer {
                kind: LayerKind::WorkspaceTrust,
                owns_keyboard: self.popups_capture_keys(),
                key_context: Some(KeyContext::Popup),
                blocks_terminal_input: true,
            });
        }
        if self.menu_state.active_menu.is_some() {
            layers.push(Layer {
                kind: LayerKind::Menu,
                owns_keyboard: true,
                key_context: Some(KeyContext::Menu),
                blocks_terminal_input: true,
            });
        }
        if self.is_prompting() {
            // Find/replace prompts resolve in the narrower `SearchPrompt`
            // context, which owns the match-mode toggles and otherwise falls
            // through to `Prompt`. Every other prompt stays in `Prompt`, so
            // the toggle keys (Alt+W etc.) never fire outside an actual search.
            let key_context = if self.active_prompt_has_search_options() {
                KeyContext::SearchPrompt
            } else {
                KeyContext::Prompt
            };
            layers.push(Layer {
                kind: LayerKind::Prompt,
                owns_keyboard: true,
                key_context: Some(key_context),
                blocks_terminal_input: true,
            });
        }
        // A non-trust popup is *present* whenever visible, but only *owns*
        // the keyboard while capturing (`popups_capture_keys`); a
        // merely-visible unfocused popup falls through. Either way a
        // visible popup blocks PTY routing — it covers the active buffer.
        if !trust_on_top
            && (self.global_popups.is_visible() || self.active_state().popups.is_visible())
        {
            layers.push(Layer {
                kind: LayerKind::Popup,
                owns_keyboard: self.popups_capture_keys(),
                key_context: Some(KeyContext::Popup),
                blocks_terminal_input: true,
            });
        }
        // The tab-bar popups (the "+" new-tab menu and the tab right-click
        // context menu) are modal chrome: while one is open it owns the
        // keyboard via a custom dispatcher (`handle_new_tab_menu_key` /
        // `handle_tab_context_menu_key`, run from `handle_key` ahead of
        // `KeyContext` resolution), so they expose no `KeyContext` here.
        // Like any covering overlay they block PTY routing — otherwise keys
        // would leak into an active terminal buffer underneath instead of
        // driving the menu. Ranked below `Popup` so the unfocused-popup
        // `take_while` guard above is unaffected.
        if self.active_window().new_tab_menu.is_some() {
            layers.push(Layer {
                kind: LayerKind::NewTabMenu,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        if self.active_window().tab_context_menu.is_some() {
            layers.push(Layer {
                kind: LayerKind::TabContextMenu,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        // The centered widget modal (picker / new-session form / plugin
        // overlay) owns the keyboard when focused. It resolves as `Normal`
        // regardless of the underlying buffer's (possibly stale) context so
        // mode-keybinding lookups still fire for the panel's own chords.
        // It blocks PTY routing whenever present — the modal sits on top
        // of (and obscures) the active terminal buffer.
        if let Some(f) = self.floating_widget_panel.as_ref() {
            layers.push(Layer {
                kind: LayerKind::FloatingModal,
                owns_keyboard: f.focused,
                key_context: Some(KeyContext::Normal),
                blocks_terminal_input: true,
            });
        }
        // The editor-global dock owns the keyboard only while focused; a
        // blurred dock stays visible but lets the buffer underneath keep
        // the keyboard *and* receive PTY routing (the dock lives beside
        // the chrome, not over it).
        if let Some(d) = self.dock.as_ref() {
            layers.push(Layer {
                kind: LayerKind::Dock,
                owns_keyboard: d.focused,
                key_context: Some(KeyContext::Dock),
                blocks_terminal_input: d.focused,
            });
        }
        // The editor content is the keyboard owner of last resort.
        let base_context = if self
            .active_window()
            .is_composite_buffer(self.active_buffer())
        {
            KeyContext::CompositeBuffer
        } else {
            self.active_window().key_context.clone()
        };
        layers.push(Layer {
            kind: LayerKind::Editor,
            owns_keyboard: true,
            key_context: Some(base_context),
            blocks_terminal_input: false,
        });

        layers
    }

    /// True iff any overlay layer is currently blocking key routing to a
    /// terminal buffer's PTY child. The single source of truth for the
    /// "is anything modal up?" question.
    pub(crate) fn presents_blocking_overlay(&self) -> bool {
        crate::app::overlay::any_layer_blocks_terminal_input(&self.overlay_layers())
    }

    /// Determine the current keybinding context based on UI state.
    ///
    /// Returns the `KeyContext` of the topmost overlay layer that owns the
    /// keyboard (see [`Editor::overlay_layers`]).
    pub fn get_key_context(&self) -> crate::input::keybindings::KeyContext {
        crate::app::overlay::resolve_focus_context(&self.overlay_layers())
            .expect("editor base layer always owns the keyboard")
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

        // Diagnostic for the "dock visible, buffer won't accept keys" wedge
        // (#2234, item 4): while the dock is mounted, record its host-side focus
        // plus the active window's key context for *every* key, before any
        // routing. If a repro shows `dock_focused=true` for keys the user aimed
        // at the buffer, the dock is swallowing them (line ~492) — a
        // host-focus / plugin-`dockBlurred` desync; if `dock_focused=false`,
        // the keys reached the window and the issue is in key-context routing.
        if let Some(focused) = self.dock.as_ref().map(|d| d.focused) {
            tracing::debug!(
                target: "fresh::dock",
                ?code,
                dock_focused = focused,
                key_context = ?self.active_window().key_context,
                active_window = ?self.active_window_id(),
                "handle_key: dock mounted (routing diagnostic)"
            );
        }

        // Event debug dialog intercepts ALL key events before any other processing.
        // This must be checked here (not just in main.rs/gui) so it works in
        // client/server mode where handle_key is called directly.
        if self.active_window().is_event_debug_active() {
            self.active_window_mut()
                .handle_event_debug_input(&key_event);
            return Ok(());
        }

        // Try terminal input dispatch first (handles terminal mode and re-entry).
        // Note: `dispatch_terminal_input` short-circuits to None when a floating
        // widget panel is mounted, so picker / form keys reach the panel below
        // instead of being forwarded to the PTY child of the underlying terminal.
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

        // Floating widget panel claims all keys while visible. Esc
        // unmounts + fires a `widget_event` "cancel"; smart-key names
        // (Tab/Return/Backspace/…/Up/Down) route through the widget
        // command dispatcher; printable chars feed `textInputChar` to
        // the focused TextInput. Mouse clicks outside the panel are
        // swallowed (handled in `mouse_input`).
        // A focused centered modal takes keyboard precedence over the
        // dock (e.g. the New-Session form opened on top of the dock).
        if self
            .floating_widget_panel
            .as_ref()
            .is_some_and(|f| f.focused)
            && self.dispatch_floating_widget_key(super::PanelSlot::Floating, code, modifiers)
        {
            return Ok(());
        }
        // A focused dock swallows keys in the dispatch below, so the global
        // focus-toggle (default Alt+O) would never be able to hand focus back
        // to the editor once you've dived in. Resolve it here, ahead of the
        // dock's own key handling, so the toggle is symmetric (same key in and
        // out). Only the blur-out direction needs this early hook — focusing a
        // blurred/hidden dock is handled by ordinary keybinding resolution
        // since the editor owns the keyboard in that state.
        if self.dock.as_ref().is_some_and(|f| f.focused) {
            let ctx = self.get_key_context();
            let resolved = self
                .keybindings
                .read()
                .ok()
                .map(|kb| kb.resolve(&key_event, ctx));
            if matches!(resolved, Some(Action::ToggleDockFocus)) {
                self.handle_action(Action::ToggleDockFocus)?;
                return Ok(());
            }
        }
        if self.dock.as_ref().is_some_and(|f| f.focused)
            && self.dispatch_floating_widget_key(super::PanelSlot::Dock, code, modifiers)
        {
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
        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&active_split)
        {
            view_state.viewport.clear_skip_ensure_visible();
        }

        // Dismiss theme info popup on any key press
        if self.active_window_mut().theme_info_popup.is_some() {
            self.active_window_mut().theme_info_popup = None;
        }

        if self
            .active_window_mut()
            .file_explorer_context_menu
            .is_some()
        {
            if let Some(result) = self.handle_file_explorer_context_menu_key(code, modifiers) {
                return result;
            }
        }

        // The tab-bar popups (the "+" new-tab menu and the tab right-click
        // context menu) are modal: while one is open it owns the keyboard so
        // navigation/selection work and every other key is filtered out
        // instead of leaking into the active buffer underneath.
        if let Some(result) = self.handle_new_tab_menu_key(code) {
            return result;
        }
        if let Some(result) = self.handle_tab_context_menu_key(code) {
            return result;
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
        //
        // CompositeBuffer is included so a composite buffer's plugin-defined
        // mode (e.g. the review-diff `diff-view` mode) can bind keys the core
        // composite handling leaves free — like Enter / Alt+O to open the file
        // under the cursor. Keys the mode does not bind fall through unchanged
        // to the composite router and the CompositeBuffer keymap below, so
        // built-in hunk navigation (n/p/]/[) and close (q) are unaffected.
        let should_check_mode_bindings = matches!(
            context,
            crate::input::keybindings::KeyContext::Normal
                | crate::input::keybindings::KeyContext::CompositeBuffer
        );

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
                    let chord_result = keybindings.resolve_chord(
                        &self.active_window().chord_state,
                        &key_event,
                        mode_ctx.clone(),
                    );
                    let resolved = keybindings.resolve(&key_event, mode_ctx);
                    (chord_result, resolved)
                };
                match chord_result {
                    crate::input::keybindings::ChordResolution::Complete(action) => {
                        tracing::debug!("Mode chord resolved to action: {:?}", action);
                        self.active_window_mut().chord_state.clear();
                        return self.handle_action(action);
                    }
                    crate::input::keybindings::ChordResolution::Partial => {
                        tracing::debug!("Potential chord prefix in mode '{}'", mode_name);
                        self.active_window_mut().chord_state.push((code, modifiers));
                        return Ok(());
                    }
                    crate::input::keybindings::ChordResolution::NoMatch => {
                        if !self.active_window_mut().chord_state.is_empty() {
                            tracing::debug!("Chord sequence abandoned in mode, clearing state");
                            self.active_window_mut().chord_state.clear();
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
                    // Before blocking the key, resolve it against
                    // the Normal context and forward if it's one of
                    // the clipboard / select-all actions — those
                    // legitimately belong to the focused widget
                    // Text input, not the underlying buffer. Other
                    // Ctrl-modified actions (e.g. Open / Save /
                    // SplitVertical) stay blocked so they don't
                    // hijack a focused search field.
                    let normal_ctx = crate::input::keybindings::KeyContext::Normal;
                    let resolved = {
                        let keybindings = self.keybindings.read().unwrap();
                        keybindings.resolve(&key_event, normal_ctx)
                    };
                    match resolved {
                        Action::Paste | Action::Copy | Action::Cut | Action::SelectAll => {
                            return self.handle_action(resolved);
                        }
                        _ => {}
                    }
                    // Shift+arrow / Ctrl+Shift+arrow extend the
                    // selection on the focused widget TextEdit, if
                    // any. We route these directly here instead of
                    // through the IPC `WidgetAction` path because
                    // selection ops are host-internal — the plugin's
                    // model only cares about the post-`change`
                    // value, which still fires when the selection
                    // is mutated by a subsequent edit.
                    if modifiers.contains(KeyModifiers::SHIFT) {
                        let buffer_id = self.active_buffer();
                        if let Some(panel_id) = self.focused_text_widget_panel_for_buffer(buffer_id)
                        {
                            let ctrl = modifiers.contains(KeyModifiers::CONTROL);
                            let handled = match code {
                                KeyCode::Left if ctrl => self
                                    .with_focused_text_editor(&panel_id, |e| {
                                        e.move_word_left_selecting()
                                    }),
                                KeyCode::Right if ctrl => self
                                    .with_focused_text_editor(&panel_id, |e| {
                                        e.move_word_right_selecting()
                                    }),
                                KeyCode::Left => self.with_focused_text_editor(&panel_id, |e| {
                                    e.move_left_selecting()
                                }),
                                KeyCode::Right => self.with_focused_text_editor(&panel_id, |e| {
                                    e.move_right_selecting()
                                }),
                                KeyCode::Up => self
                                    .with_focused_text_editor(&panel_id, |e| e.move_up_selecting()),
                                KeyCode::Down => self.with_focused_text_editor(&panel_id, |e| {
                                    e.move_down_selecting()
                                }),
                                KeyCode::Home => self.with_focused_text_editor(&panel_id, |e| {
                                    e.move_home_selecting()
                                }),
                                KeyCode::End => self.with_focused_text_editor(&panel_id, |e| {
                                    e.move_end_selecting()
                                }),
                                _ => false,
                            };
                            // We always consume Shift+nav on a
                            // focused widget Text — `handled=false`
                            // means the move was a no-op (e.g.
                            // already at the boundary), which is
                            // still the correct shortcut behaviour.
                            if matches!(
                                code,
                                KeyCode::Left
                                    | KeyCode::Right
                                    | KeyCode::Up
                                    | KeyCode::Down
                                    | KeyCode::Home
                                    | KeyCode::End
                            ) {
                                let _ = handled;
                                return Ok(());
                            }
                        }
                    }
                    tracing::debug!("Blocking unbound key in text-input mode '{}'", mode_name);
                    return Ok(());
                }
            }
            if let Some(ref mode_name) = self.active_window().editor_mode {
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
            if self.active_window().is_composite_buffer(active_buf) {
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
            let chord_result = keybindings.resolve_chord(
                &self.active_window().chord_state,
                &key_event,
                context.clone(),
            );
            let action = keybindings.resolve(&key_event, context.clone());
            (chord_result, action)
        };

        match chord_result {
            crate::input::keybindings::ChordResolution::Complete(action) => {
                // Complete chord match - execute action and clear chord state
                tracing::debug!("Complete chord match -> Action: {:?}", action);
                self.active_window_mut().chord_state.clear();
                return self.handle_action(action);
            }
            crate::input::keybindings::ChordResolution::Partial => {
                // Partial match - add to chord state and wait for more keys
                tracing::debug!("Partial chord match - waiting for next key");
                self.active_window_mut().chord_state.push((code, modifiers));
                return Ok(());
            }
            crate::input::keybindings::ChordResolution::NoMatch => {
                // No chord match - clear state and try regular resolution
                if !self.active_window_mut().chord_state.is_empty() {
                    tracing::debug!("Chord sequence abandoned, clearing state");
                    self.active_window_mut().chord_state.clear();
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
            | Action::LspImplementation
            | Action::LspHover
            | Action::None => {
                // Don't cancel for LSP actions or no-op
            }
            _ => {
                // Cancel any pending LSP requests
                self.active_window_mut().cancel_pending_lsp_requests();
            }
        }

        // Note: Modal components (Settings, Menu, Prompt, Popup, File Browser) are now
        // handled by dispatch_modal_input using the InputHandler system.
        // All remaining actions delegate to handle_action.
        self.handle_action(action)
    }

    /// Handle an action (for normal mode and command execution).
    /// Used by the app module internally and by the GUI module for native menu dispatch.
    /// Change the current workspace's trust level, persist it, and report it.
    /// The new policy applies live at the next authority-routed spawn (the
    /// guarding spawners read the level on every spawn) — there is NO editor
    /// restart here, deliberately: a rebuild would reset every other
    /// orchestrator session's buffers/layout (see the body). Trust-gated work
    /// re-triggers via the `trust_changed` hook instead (e.g. env-manager
    /// re-activates a now-trusted env). Already-correct selections (e.g.
    /// confirming the current level) only persist the decision.
    pub(crate) fn set_workspace_trust_level(
        &mut self,
        level: crate::services::workspace_trust::TrustLevel,
    ) {
        use crate::services::workspace_trust::TrustLevel;
        // Trust is a per-window gate: each `Window` owns its own authority +
        // `WorkspaceTrust` (issue #2280), and the guarding spawners read the
        // level live at spawn time. Writing the new level here is the whole
        // change — `set_level` itself documents "no rebuild required". The
        // next authority-routed spawn (LSP, terminal command, task, formatter,
        // plugin `spawnProcess`) honours the new level automatically.
        //
        // We deliberately do NOT `request_restart` here: that tears down and
        // rebuilds the *entire* editor — every orchestrator session window,
        // not just this one — which discarded other sessions' buffers and
        // reset the layout when toggling a single session's trust (the
        // trust-level-modal reset bug).
        let trust = &self.authority().workspace_trust;
        trust.set_level(level);
        let msg = match level {
            TrustLevel::Trusted => t!("trust.now_trusted"),
            TrustLevel::Restricted => t!("trust.now_restricted"),
            TrustLevel::Blocked => t!("trust.now_blocked"),
        }
        .to_string();
        self.active_window_mut().status_message = Some(msg);

        // Refresh the plugin-visible state snapshot so `editor.workspaceTrustLevel()`
        // reflects the new level, then notify plugins. The `trust_changed` hook
        // lets trust-gated work re-trigger inline — env-manager re-activates a
        // now-trusted env without a window switch — and it is the single signal
        // every trust-change path (modal confirm, status pill, plugin action)
        // funnels through, since they all route here. Deliberately a hook and a
        // snapshot refresh, NOT a `request_restart`: a rebuild would reset every
        // other session's buffers/layout (see the note above).
        #[cfg(feature = "plugins")]
        {
            self.update_plugin_state_snapshot();
            self.plugin_manager.read().unwrap().run_hook(
                "trust_changed",
                crate::services::plugins::hooks::HookArgs::TrustChanged {
                    level: level.as_str().to_string(),
                },
            );
        }
    }

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
            Action::WorkspaceTrustTrust => {
                self.set_workspace_trust_level(
                    crate::services::workspace_trust::TrustLevel::Trusted,
                );
            }
            Action::WorkspaceTrustRestrict => {
                self.set_workspace_trust_level(
                    crate::services::workspace_trust::TrustLevel::Restricted,
                );
            }
            Action::WorkspaceTrustBlock => {
                self.set_workspace_trust_level(
                    crate::services::workspace_trust::TrustLevel::Blocked,
                );
            }
            Action::WorkspaceTrustPrompt => {
                // Voluntarily-opened: cancellable (Esc / Cancel just closes).
                self.show_workspace_trust_popup(true);
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
                    self.active_window_mut().status_message =
                        Some(t!("file.save_failed", error = &msg).to_string());
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
                        p.strip_prefix(self.working_dir())
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
                    .buffers()
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
                if self.active_window_mut().key_context
                    == crate::input::keybindings::KeyContext::FileExplorer
                {
                    self.active_window_mut().file_explorer_copy();
                    return Ok(());
                }
                // A focused widget Text input on the active buffer
                // wins over the underlying buffer's copy path. The
                // widget's selection lives in its TextEdit; this
                // bypasses `is_editing_disabled` because widget
                // inputs are independent of the underlying virtual
                // buffer's read-only-ness.
                let buffer_id = self.active_buffer();
                if let Some(panel_id) = self.focused_text_widget_panel_for_buffer(buffer_id) {
                    if self.handle_widget_copy(&panel_id) {
                        self.set_status_message(t!("clipboard.copied").to_string());
                        return Ok(());
                    }
                }
                // Check if active buffer is a composite buffer
                if self.active_window().is_composite_buffer(buffer_id) {
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
                if self.active_window_mut().key_context
                    == crate::input::keybindings::KeyContext::FileExplorer
                {
                    self.active_window_mut().file_explorer_cut();
                    return Ok(());
                }
                // Focused widget Text wins over the buffer cut path,
                // and bypasses `is_editing_disabled` — widget inputs
                // are independent of the underlying virtual buffer.
                let buffer_id = self.active_buffer();
                if let Some(panel_id) = self.focused_text_widget_panel_for_buffer(buffer_id) {
                    if self.handle_widget_cut(&panel_id) {
                        return Ok(());
                    }
                }
                if self.active_window().is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                self.cut_selection()
            }
            Action::Paste => {
                if self.active_window_mut().key_context
                    == crate::input::keybindings::KeyContext::FileExplorer
                {
                    self.file_explorer_paste();
                    return Ok(());
                }
                // Focused widget Text wins over the buffer paste
                // path, and bypasses `is_editing_disabled`. Line
                // endings get normalised to LF before insertion
                // (multi-line `TextEdit` stores plain `\n`;
                // single-line strips them).
                let buffer_id = self.active_buffer();
                if let Some(panel_id) = self.focused_text_widget_panel_for_buffer(buffer_id) {
                    if let Some(text) = self.clipboard.paste() {
                        let normalized = text.replace("\r\n", "\n").replace('\r', "\n");
                        self.handle_widget_insert_str(&panel_id, &normalized);
                        self.set_status_message(t!("clipboard.pasted").to_string());
                    }
                    return Ok(());
                }
                if self.active_window().is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                self.paste()
            }
            Action::SelectAll => {
                // Focused widget Text wins over the buffer's
                // select-all. SelectAll on the buffer is then
                // handled by the default `apply_action_as_events`
                // catch-all path below.
                let buffer_id = self.active_buffer();
                if let Some(panel_id) = self.focused_text_widget_panel_for_buffer(buffer_id) {
                    self.handle_widget_select_all(&panel_id);
                    return Ok(());
                }
                self.apply_action_as_events(Action::SelectAll)?;
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
                self.ensure_help_panel_mode_registered();
                self.active_window_mut().open_help_manual();
            }
            Action::ShowKeyboardShortcuts => {
                self.ensure_help_panel_mode_registered();
                self.active_window_mut().open_keyboard_shortcuts();
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
            Action::ShowReadOnlyMenu => {
                self.show_read_only_popup();
            }
            Action::ClearWarnings => {
                self.active_window_mut().clear_warnings();
            }
            Action::CommandPalette => {
                // CommandPalette now delegates to QuickOpen (which starts with ">" prefix
                // for command mode). Toggle if already open.
                if self.close_quick_open_if_open() {
                    return Ok(());
                }
                self.start_quick_open();
            }
            Action::QuickOpen => {
                if self.close_quick_open_if_open() {
                    return Ok(());
                }
                self.start_quick_open();
            }
            Action::QuickOpenBuffers => {
                if self.close_quick_open_if_open() {
                    return Ok(());
                }
                self.start_quick_open_with_prefix("#");
            }
            Action::QuickOpenFiles => {
                if self.close_quick_open_if_open() {
                    return Ok(());
                }
                self.start_quick_open_with_prefix("");
            }
            Action::OpenLiveGrep => {
                self.handle_action(Action::PluginAction("start_live_grep".to_string()))?;
            }
            Action::ResumeLiveGrep => {
                self.handle_action(Action::PluginAction("resume_live_grep".to_string()))?;
            }
            Action::ToggleUtilityDock => {
                use crate::view::split::SplitRole;
                if let Some(dock_leaf) = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .find_leaf_by_role(SplitRole::UtilityDock)
                {
                    let active = self
                        .windows
                        .get(&self.active_window)
                        .and_then(|w| w.buffers.splits())
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .active_split();
                    if active == dock_leaf {
                        // Already focused — no editor-leaf history yet,
                        // so just cycle to the next leaf via the
                        // existing Alt+] command. Phase 7 will track a
                        // proper "previous editor split" pointer.
                        self.next_split();
                    } else {
                        self.windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_manager_mut())
                            .expect("active window must have a populated split layout")
                            .set_active_split(dock_leaf);
                    }
                } else {
                    self.set_status_message(
                        "No Utility Dock open — invoke a dock-aware utility (Diagnostics, Search/Replace, …)"
                            .to_string(),
                    );
                }
            }
            Action::CycleLiveGrepProvider => {
                // Only meaningful while the Live Grep overlay is open. Detect via prompt state —
                // both `PromptType::LiveGrep` (Resume's pre-seeded overlay) and
                // `Plugin{custom_type:"live-grep"}` (the live-running plugin's prompt) qualify.
                let in_live_grep = self
                    .active_window()
                    .prompt
                    .as_ref()
                    .map(|p| match &p.prompt_type {
                        PromptType::LiveGrep => true,
                        PromptType::Plugin { custom_type } => custom_type == "live-grep",
                        _ => false,
                    })
                    .unwrap_or(false);
                if !in_live_grep {
                    self.set_status_message(
                        "Cycle Live Grep provider only works inside Live Grep".to_string(),
                    );
                    return Ok(());
                }
                self.handle_action(Action::PluginAction("live_grep_cycle_provider".to_string()))?;
            }
            Action::OpenTerminalInDock => {
                self.handle_open_terminal_in_dock()?;
            }
            Action::ToggleLineWrap => {
                let new_value = !self.config.editor.line_wrap;
                self.config_mut().editor.line_wrap = new_value;
                // `resolve_line_wrap_for_buffer` below reads
                // `Window::config()`, which holds a *separate* `Arc<Config>`
                // clone from the Editor's. Without this sync the resolve
                // would return the pre-toggle value and we'd write the
                // *old* line-wrap state back into the viewport — silently
                // no-op'ing the toggle while still flipping the status
                // message. See `Editor::config_mut` for the broader rule.
                self.sync_windows_config();

                // Update all viewports to reflect the new line wrap setting,
                // respecting per-language overrides
                let leaf_ids: Vec<_> = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .keys()
                    .copied()
                    .collect();
                for leaf_id in leaf_ids {
                    let buffer_id = self
                        .split_manager_mut()
                        .get_buffer_id(leaf_id.into())
                        .unwrap_or(BufferId(0));
                    let effective_wrap =
                        self.active_window().resolve_line_wrap_for_buffer(buffer_id);
                    let wrap_column = self
                        .active_window()
                        .resolve_wrap_column_for_buffer(buffer_id);
                    if let Some(view_state) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .get_mut(&leaf_id)
                    {
                        view_state.viewport.line_wrap_enabled = effective_wrap;
                        view_state.viewport.wrap_indent = self.config.editor.wrap_indent;
                        view_state.viewport.wrap_column = wrap_column;
                        // Global toggle expresses global intent; drop the
                        // per-buffer pin so it doesn't revert this change.
                        view_state.line_wrap_override = None;
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
                let leaf_ids: Vec<_> = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .keys()
                    .copied()
                    .collect();
                for leaf_id in leaf_ids {
                    if let Some(view_state) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .get_mut(&leaf_id)
                    {
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
            Action::ToggleOccurrenceHighlight => {
                let new_value = !self.config.editor.highlight_occurrences;
                self.config_mut().editor.highlight_occurrences = new_value;

                // Update all open buffers
                for window in self.windows.values_mut() {
                    for (_, state) in &mut window.buffers {
                        state.reference_highlight_overlay.enabled = new_value;
                        if !new_value {
                            state
                                .reference_highlight_overlay
                                .clear(&mut state.overlays, &mut state.marker_list);
                        }
                    }
                }

                let state = if new_value {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(
                    t!("view.occurrence_highlight_state", state = state).to_string(),
                );
            }
            Action::ToggleReadOnly => {
                let buffer_id = self.active_buffer();
                let is_now_read_only = self
                    .active_window()
                    .buffer_metadata
                    .get(&buffer_id)
                    .map(|m| !m.read_only)
                    .unwrap_or(false);
                self.active_window_mut()
                    .mark_buffer_read_only(buffer_id, is_now_read_only);

                let state_str = if is_now_read_only {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(t!("view.read_only_state", state = state_str).to_string());
            }
            Action::TogglePageView => {
                self.active_window_mut().handle_toggle_page_view();
            }
            Action::SetPageWidth => {
                let active_split = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                let current = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
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
                        p.strip_prefix(self.working_dir())
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
            Action::LspImplementation => {
                self.request_implementation()?;
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
                let is_search_prompt = self.active_window().prompt.as_ref().is_some_and(|p| {
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
                self.active_window_mut().search_confirm_each = true;
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
            Action::ClearSearch => {
                self.active_window_mut().clear_search_highlights();
            }
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::AddCursorsToLineEnds => self.add_cursors_to_line_ends(),
            Action::NextBuffer => self.next_buffer(),
            Action::PrevBuffer => self.prev_buffer(),
            Action::SwitchToPreviousTab => self.switch_to_previous_tab(),
            Action::SwitchToTabByName => self.start_switch_to_tab_prompt(),

            // Tab scrolling (manual scroll - don't auto-adjust)
            Action::ScrollTabsLeft => {
                let active_split_id = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                if let Some(view_state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .get_mut(&active_split_id)
                {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_sub(5);
                    self.set_status_message(t!("status.scrolled_tabs_left").to_string());
                }
            }
            Action::ScrollTabsRight => {
                let active_split_id = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                if let Some(view_state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .get_mut(&active_split_id)
                {
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
            Action::NextWindow => self.next_window(),
            Action::PrevWindow => self.prev_window(),
            Action::IncreaseSplitSize => self.adjust_split_size(0.05),
            Action::DecreaseSplitSize => self.adjust_split_size(-0.05),
            Action::ToggleMaximizeSplit => self.toggle_maximize_split(),
            Action::ToggleFileExplorer => self.toggle_file_explorer(),
            Action::ToggleFileExplorerSide => self.toggle_file_explorer_side(),
            Action::ToggleMenuBar => self.toggle_menu_bar(),
            Action::ToggleTabBar => self.active_window_mut().toggle_tab_bar(),
            Action::ToggleStatusBar => self.active_window_mut().toggle_status_bar(),
            Action::TogglePromptLine => self.active_window_mut().toggle_prompt_line(),
            Action::ToggleVerticalScrollbar => self.toggle_vertical_scrollbar(),
            Action::ToggleHorizontalScrollbar => self.toggle_horizontal_scrollbar(),
            Action::ToggleLineNumbers => self.toggle_line_numbers(),
            Action::ToggleLineNumbersCurrentBuffer => self.toggle_line_numbers_current_buffer(),
            Action::ToggleLineWrapCurrentBuffer => self.toggle_line_wrap_current_buffer(),
            Action::TriggerWaveAnimation => self.trigger_wave_animation(),
            Action::ToggleScrollSync => self.active_window_mut().toggle_scroll_sync(),
            Action::ToggleMouseCapture => self.toggle_mouse_capture(),
            Action::ToggleMouseHover => self.toggle_mouse_hover(),
            Action::ToggleDebugHighlights => self.active_window_mut().toggle_debug_highlights(),
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
                    .buffers()
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
                let __buffer_id = self.active_buffer();
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&__buffer_id)
                {
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
                let __buffer_id = self.active_buffer();
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&__buffer_id)
                {
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
            Action::FocusEditor => self.active_window_mut().focus_editor(),
            Action::ToggleDockFocus => {
                // Bounce keyboard focus between the editor/explorer area and
                // the orchestrator dock. `dock` is `Some` whenever the dock is
                // mounted (focused or merely visible-but-blurred); the helpers
                // flip `focused` and fire the matching `focus`/`blur`
                // widget_event so the plugin's mirror stays in sync.
                match self.dock.as_ref().map(|d| d.focused) {
                    Some(true) => self.blur_floating_panel(super::PanelSlot::Dock),
                    Some(false) => self.refocus_floating_panel(super::PanelSlot::Dock),
                    // Dock hidden: hand off to the orchestrator plugin's
                    // show-dock command so one key both opens and focuses it.
                    None => {
                        return self.handle_action(Action::PluginAction(
                            "orchestrator_dock_toggle".to_string(),
                        ));
                    }
                }
            }
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
            Action::FileExplorerSearchClear => {
                self.active_window_mut().file_explorer_search_clear()
            }
            Action::FileExplorerSearchBackspace => {
                self.active_window_mut().file_explorer_search_pop_char()
            }
            Action::FileExplorerCopy => self.active_window_mut().file_explorer_copy(),
            Action::FileExplorerCut => self.active_window_mut().file_explorer_cut(),
            Action::FileExplorerPaste => self.file_explorer_paste(),
            Action::FileExplorerDuplicate => self.file_explorer_duplicate(),
            Action::FileExplorerCopyFullPath => self.file_explorer_copy_path(false),
            Action::FileExplorerCopyRelativePath => self.file_explorer_copy_path(true),
            Action::FileExplorerExtendSelectionUp => {
                self.active_window_mut().file_explorer_extend_selection_up()
            }
            Action::FileExplorerExtendSelectionDown => self
                .active_window_mut()
                .file_explorer_extend_selection_down(),
            Action::FileExplorerToggleSelect => {
                self.active_window_mut().file_explorer_toggle_select()
            }
            Action::FileExplorerSelectAll => self.active_window_mut().file_explorer_select_all(),
            Action::RemoveSecondaryCursors => {
                // Convert action to events and apply them
                if let Some(events) = self
                    .active_window_mut()
                    .action_to_events(Action::RemoveSecondaryCursors)
                {
                    // Wrap in batch for atomic undo
                    let batch = Event::Batch {
                        events: events.clone(),
                        description: "Remove secondary cursors".to_string(),
                    };
                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);

                    // Ensure the primary cursor is visible after removing secondary cursors
                    let active_split = self
                        .windows
                        .get(&self.active_window)
                        .and_then(|w| w.buffers.splits())
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .active_split();
                    let active_buffer = self.active_buffer();
                    self.active_window_mut()
                        .ensure_cursor_visible_for_split(active_buffer, active_split);
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
                // Delegate to the shared helper so the menu path persists the
                // choice to the user config (issue #474), matching the
                // command-palette path. This handler previously duplicated the
                // switch logic but skipped persistence, so the keybinding style
                // reset to the default on the next launch.
                self.apply_keybinding_map(&map_name);
            }

            Action::SmartHome => {
                // In composite (diff) views, use LineStart movement
                let buffer_id = self.active_buffer();
                if self.active_window().is_composite_buffer(buffer_id) {
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
                self.active_window_mut().toggle_fold_at_cursor();
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
                self.active_window_mut().set_bookmark(key);
            }
            Action::JumpToBookmark(key) => {
                self.jump_to_bookmark(key);
            }
            Action::ClearBookmark(key) => {
                self.active_window_mut().clear_bookmark(key);
            }
            Action::ListBookmarks => {
                self.active_window_mut().list_bookmarks();
            }
            Action::ToggleSearchCaseSensitive if !self.active_prompt_has_search_options() => {}
            Action::ToggleSearchWholeWord if !self.active_prompt_has_search_options() => {}
            Action::ToggleSearchRegex if !self.active_prompt_has_search_options() => {}
            Action::ToggleSearchCaseSensitive => {
                self.active_window_mut().search_case_sensitive =
                    !self.active_window().search_case_sensitive;
                let state = if self.active_window().search_case_sensitive {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(
                    t!("search.case_sensitive_state", state = state).to_string(),
                );
                self.refresh_active_search();
            }
            Action::ToggleSearchWholeWord => {
                self.active_window_mut().search_whole_word =
                    !self.active_window().search_whole_word;
                let state = if self.active_window().search_whole_word {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.whole_word_state", state = state).to_string());
                self.refresh_active_search();
            }
            Action::ToggleSearchRegex => {
                self.active_window_mut().search_use_regex = !self.active_window().search_use_regex;
                let state = if self.active_window().search_use_regex {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.regex_state", state = state).to_string());
                self.refresh_active_search();
            }
            Action::ToggleSearchConfirmEach => {
                self.active_window_mut().search_confirm_each =
                    !self.active_window().search_confirm_each;
                let state = if self.active_window().search_confirm_each {
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
                if let Some(key) = self.active_window_mut().macros.last_register() {
                    self.play_macro(key);
                } else {
                    self.set_status_message(t!("status.no_macro_recorded").to_string());
                }
            }
            Action::PromptSaveMacroToInit => {
                self.start_prompt(
                    "Save macro to init.ts (0-9): ".to_string(),
                    PromptType::SaveMacroToInit,
                );
            }
            Action::PromptPromoteMacro => {
                self.start_prompt(
                    "Promote macro to command (0-9): ".to_string(),
                    PromptType::PromoteMacro,
                );
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
                self.active_window_mut().composite_next_hunk_active(buf);
            }
            Action::CompositePrevHunk => {
                let buf = self.active_buffer();
                self.active_window_mut().composite_prev_hunk_active(buf);
            }
            Action::None => {}
            Action::DeleteBackward => {
                if self.active_window().is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                // Normal backspace handling
                if let Some(events) = self
                    .active_window_mut()
                    .action_to_events(Action::DeleteBackward)
                {
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
                {
                    let result = self
                        .plugin_manager
                        .read()
                        .unwrap()
                        .execute_action_async(&action_name);
                    if let Some(result) = result {
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
                        self.set_status_message(
                            t!("status.plugin_manager_unavailable").to_string(),
                        );
                    }
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

                    let load_result = self
                        .plugin_manager
                        .read()
                        .unwrap()
                        .load_plugin_from_source(&content, &name, is_ts);
                    match load_result {
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
                        let declarations =
                            self.plugin_manager.read().unwrap().plugin_declarations();
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
            Action::OpenTerminalRight => {
                self.open_terminal_split(crate::model::event::SplitDirection::Vertical);
            }
            Action::OpenTerminalBelow => {
                self.open_terminal_split(crate::model::event::SplitDirection::Horizontal);
            }
            Action::CloseTerminal => {
                self.close_terminal();
            }
            Action::FocusTerminal => {
                // If viewing a terminal buffer, switch to terminal mode
                if self
                    .active_window()
                    .is_terminal_buffer(self.active_buffer())
                {
                    // Mode change to live: remember it for the next focus.
                    let active = self.active_buffer();
                    self.active_window_mut().set_terminal_interaction_mode(
                        active,
                        crate::app::window::TerminalInteractionMode::Live,
                    );
                    self.active_window_mut().terminal_mode = true;
                    self.active_window_mut().key_context = KeyContext::Terminal;
                    self.set_status_message(t!("status.terminal_mode_enabled").to_string());
                }
            }
            Action::TerminalEscape => {
                // Exit terminal mode back to editor
                if self.active_window().terminal_mode {
                    // User dropped to read-only scrollback: remember that mode.
                    let active = self.active_buffer();
                    self.active_window_mut().set_terminal_interaction_mode(
                        active,
                        crate::app::window::TerminalInteractionMode::Scrollback,
                    );
                    self.active_window_mut().terminal_mode = false;
                    self.active_window_mut().key_context = KeyContext::Normal;
                    self.set_status_message(t!("status.terminal_mode_disabled").to_string());
                }
            }
            Action::ToggleKeyboardCapture => {
                // Toggle keyboard capture mode in terminal
                if self.active_window().terminal_mode {
                    self.active_window_mut().keyboard_capture =
                        !self.active_window_mut().keyboard_capture;
                    if self.active_window_mut().keyboard_capture {
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
                if self.active_window().terminal_mode {
                    if let Some(text) = self.clipboard.paste() {
                        self.active_window_mut()
                            .send_terminal_input(text.as_bytes());
                    }
                }
            }
            Action::SendSelectionToTerminal => {
                self.send_selection_to_terminal();
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
                self.active_window_mut().open_event_debug();
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
                if let Some(ref mut prompt) = self.active_window_mut().prompt {
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
                } else if self.active_window_mut().key_context == KeyContext::FileExplorer {
                    self.active_window_mut().file_explorer_search_push_char(c);
                } else {
                    self.handle_insert_char_editor(c)?;
                }
            }
            // Prompt clipboard actions
            Action::PromptCopy => {
                if let Some(prompt) = &self.active_window_mut().prompt {
                    let text = prompt.selected_text().unwrap_or_else(|| prompt.get_text());
                    if !text.is_empty() {
                        self.clipboard.copy(text);
                        self.set_status_message(t!("clipboard.copied").to_string());
                    }
                }
            }
            Action::PromptCut => {
                if let Some(prompt) = &self.active_window_mut().prompt {
                    let text = prompt.selected_text().unwrap_or_else(|| prompt.get_text());
                    if !text.is_empty() {
                        self.clipboard.copy(text);
                    }
                }
                if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
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
                    if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
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

    /// Fire a `widget_event` at the plugin owning the dock, keyed to the
    /// `sessions` widget. Used for dock-only gestures (Enter-activate,
    /// the Alt+T/Alt+I/Alt+P filter toggles) that the dialog handles via
    /// an editor mode the dock can't use — see `dispatch_floating_widget_key`.
    fn fire_dock_widget_event(&self, panel_key: &crate::widgets::PanelKey, event_type: &str) {
        self.fire_widget_event(
            panel_key,
            "sessions".to_string(),
            event_type.to_string(),
            serde_json::json!({}),
        );
    }

    /// Route a keystroke to the floating widget panel when one is
    /// mounted. Returns `true` if the key was consumed.
    ///
    /// Esc unmounts the panel and fires a `widget_event` `cancel`
    /// so the plugin can clean up its own state (clear mode, drop
    /// form state, etc.). Tab / S-Tab / Return / Space / Backspace /
    /// Delete / Home / End / Left / Right / Up / Down route through
    /// the same smart-key dispatch the bound mode handlers would
    /// use. Printable characters feed `textInputChar` to the
    /// currently focused TextInput.
    fn dispatch_floating_widget_key(
        &mut self,
        slot: super::PanelSlot,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> bool {
        use crossterm::event::{KeyCode, KeyModifiers};
        let panel_key = match self.panel(slot) {
            Some(fwp) => fwp.panel_key.clone(),
            None => {
                tracing::debug!(
                    target: "fresh::dock",
                    ?slot,
                    ?code,
                    "dispatch_floating_widget_key: no panel mounted in slot — returning false"
                );
                return false;
            }
        };
        tracing::debug!(
            target: "fresh::dock",
            panel = %panel_key,
            ?slot,
            ?code,
            modifiers = ?modifiers,
            placement = ?self.panel(slot).map(|f| f.placement),
            focused = ?self.panel(slot).map(|f| f.focused),
            "dispatch_floating_widget_key: entry"
        );
        // The left dock handles Enter / Esc / Space / "/" here, at the
        // floating-panel layer, *independent of editor modes*. Editor
        // modes (`defineMode`) resolve against the active buffer's mode,
        // which the dock floats over — so a session whose buffer has a
        // local mode would shadow any global dock mode. Up/Down fall
        // through to the generic smart-key list nav below (which fires
        // the `select` event the plugin live-switches on).
        if matches!(
            self.panel(slot).map(|f| f.placement),
            Some(super::PanelPlacement::LeftDock { .. })
        ) {
            let on_filter = self
                .widget_registry
                .focus_key(&panel_key)
                .map(|k| k == "filter")
                .unwrap_or(false);
            // The project dropdown owns the keyboard while panel focus
            // sits on one of its `project-pick:` rows (the plugin moves
            // focus there when the menu opens). In that state ↑/↓ move
            // the dropdown cursor, Enter commits it, and Esc cancels —
            // all routed to the plugin as `dock_menu_*` events. Without
            // this, those keys fell through to the generic list nav
            // below and drove the session list *under* the open menu,
            // so the dropdown was visible but un-navigable by keyboard.
            let on_project_menu = self
                .widget_registry
                .focus_key(&panel_key)
                .map(|k| k.starts_with("project-pick:"))
                .unwrap_or(false);
            if on_project_menu {
                match code {
                    KeyCode::Up => {
                        self.fire_dock_widget_event(&panel_key, "dock_menu_prev");
                        return true;
                    }
                    KeyCode::Down => {
                        self.fire_dock_widget_event(&panel_key, "dock_menu_next");
                        return true;
                    }
                    // Tab/Shift+Tab navigate the menu too, so they can't
                    // tab focus *out* of the open dropdown into the dock
                    // toolbar behind it.
                    KeyCode::Tab if modifiers.contains(KeyModifiers::SHIFT) => {
                        self.fire_dock_widget_event(&panel_key, "dock_menu_prev");
                        return true;
                    }
                    KeyCode::BackTab => {
                        self.fire_dock_widget_event(&panel_key, "dock_menu_prev");
                        return true;
                    }
                    KeyCode::Tab => {
                        self.fire_dock_widget_event(&panel_key, "dock_menu_next");
                        return true;
                    }
                    KeyCode::Enter | KeyCode::Char(' ') => {
                        self.fire_dock_widget_event(&panel_key, "dock_menu_accept");
                        return true;
                    }
                    KeyCode::Esc => {
                        self.fire_dock_widget_event(&panel_key, "dock_menu_cancel");
                        return true;
                    }
                    _ => {}
                }
            }
            match code {
                KeyCode::Esc => {
                    if on_filter {
                        // Return from the filter to the session list.
                        self.set_panel_focus_and_notify(&panel_key, "sessions".to_string());
                    } else {
                        // Leave the dock — focus the editor; dock stays visible.
                        self.blur_floating_panel(slot);
                    }
                    return true;
                }
                KeyCode::Enter => {
                    if on_filter {
                        // Return from the filter to the session list.
                        self.set_panel_focus_and_notify(&panel_key, "sessions".to_string());
                    } else if self
                        .widget_registry
                        .focus_key(&panel_key)
                        .map(|k| k == "sessions" || k.is_empty())
                        .unwrap_or(true)
                    {
                        // Enter on the session list activates the highlighted
                        // row. The plugin attaches a discovered (on-disk)
                        // worktree as a new session, or — for a row already
                        // backed by a live window — blurs to the editor (the
                        // dock stays visible). Handled plugin-side so the
                        // discovered-vs-live decision lives next to the
                        // dialog's identical `activate` logic, not split across
                        // the host (was: always blur, which silently dropped
                        // the on-disk attach in the dock).
                        self.fire_dock_widget_event(&panel_key, "dock_activate");
                    } else {
                        // A button or toggle is keyboard-focused (Tab-cycled
                        // onto "+ New", "Manage", "view", the project menu, or
                        // a checkbox). Run THAT control's action via the
                        // generic smart-key dispatcher — which fires `activate`
                        // for a Button and `toggle` for a Toggle — instead of
                        // the list's dock_activate. Without this, Enter on a
                        // focused button silently fell through to dock_activate
                        // and merely re-focused the session list, so buttons
                        // worked with the mouse but not the keyboard.
                        self.handle_widget_command(
                            &panel_key,
                            fresh_core::api::WidgetAction::Key {
                                key: "Enter".to_string(),
                            },
                        );
                    }
                    return true;
                }
                KeyCode::Char('/') if modifiers.is_empty() => {
                    self.set_panel_focus_and_notify(&panel_key, "filter".to_string());
                    return true;
                }
                KeyCode::Char('t' | 'T') if modifiers.contains(KeyModifiers::ALT) => {
                    // Alt+T toggles "show all worktrees". In the dialog this is
                    // an OPEN_MODE chord, but the dock has no editor mode (it
                    // floats over the active buffer's mode), so route it as a
                    // dock widget_event the plugin maps to the same toggle —
                    // otherwise it falls through to the generic chord path and
                    // merely blurs the dock.
                    self.fire_dock_widget_event(&panel_key, "dock_toggle_worktrees");
                    return true;
                }
                KeyCode::Char('i' | 'I') if modifiers.contains(KeyModifiers::ALT) => {
                    // Alt+I toggles "show empty/1-file sessions" — same dock
                    // routing rationale as Alt+T above.
                    self.fire_dock_widget_event(&panel_key, "dock_toggle_trivial");
                    return true;
                }
                KeyCode::Char('p' | 'P') if modifiers.contains(KeyModifiers::ALT) => {
                    // Alt+P flips the project scope (current ↔ all) — same dock
                    // routing rationale as Alt+T above.
                    self.fire_dock_widget_event(&panel_key, "dock_toggle_scope");
                    return true;
                }
                KeyCode::Char('n' | 'N') if modifiers.contains(KeyModifiers::ALT) => {
                    // Alt+N opens the new-session form. Handled here (not
                    // via an editor mode) because the dock floats over the
                    // active buffer's mode; fire a `dock_new` widget_event
                    // the plugin turns into "+ New" — and which now leaves
                    // the dock mounted (the form is a separate slot).
                    self.fire_widget_event(
                        &panel_key,
                        "sessions".to_string(),
                        "dock_new".to_string(),
                        serde_json::json!({}),
                    );
                    return true;
                }
                KeyCode::Char(' ') => {
                    // Toggle the highlighted row's multi-select checkbox
                    // (plugin owns the selection set).
                    tracing::debug!(
                        target: "fresh::dock",
                        panel = %panel_key,
                        focus_key = ?self.widget_registry.focus_key(&panel_key),
                        "dispatch_floating_widget_key: Space on LeftDock — firing dock_space widget_event"
                    );
                    self.fire_widget_event(
                        &panel_key,
                        "sessions".to_string(),
                        "dock_space".to_string(),
                        serde_json::json!({}),
                    );
                    return true;
                }
                _ => {}
            }
        }
        let key_name: Option<&str> = match code {
            KeyCode::Esc => {
                // Mode-binding precedence: a plugin's `defineMode`
                // entry for Escape wins over the default
                // "Esc closes the modal" behaviour. Mirrors the
                // same has_explicit_binding check the named-key
                // and Ctrl/Alt-char branches below already run.
                // Lets a plugin claim Esc for a nested
                // dismiss-the-dropdown gesture before the
                // outermost cancel fires.
                let mode_has_binding = self
                    .active_window()
                    .editor_mode
                    .as_ref()
                    .map(|mode_name| {
                        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
                        let mode_ctx =
                            crate::input::keybindings::KeyContext::Mode(mode_name.to_string());
                        let keybindings = self.keybindings.read().unwrap();
                        keybindings.has_explicit_binding(&key_event, &mode_ctx)
                    })
                    .unwrap_or(false);
                if mode_has_binding {
                    return false;
                }
                let widget_key = self
                    .widget_registry
                    .get(&panel_key)
                    .map(|p| p.focus_key.clone())
                    .unwrap_or_default();
                self.fire_widget_event(
                    &panel_key,
                    widget_key,
                    "cancel".to_string(),
                    serde_json::json!({}),
                );
                *self.panel_opt_mut(slot) = None;
                let _ = self.widget_registry.unmount(&panel_key);
                return true;
            }
            KeyCode::Tab => Some(if modifiers.contains(KeyModifiers::SHIFT) {
                "Shift+Tab"
            } else {
                "Tab"
            }),
            KeyCode::BackTab => Some("Shift+Tab"),
            KeyCode::Enter => Some("Enter"),
            KeyCode::Backspace => Some("Backspace"),
            KeyCode::Delete => Some("Delete"),
            KeyCode::Home => Some("Home"),
            KeyCode::End => Some("End"),
            KeyCode::Left => Some("Left"),
            KeyCode::Right => Some("Right"),
            KeyCode::Up => Some("Up"),
            KeyCode::Down => Some("Down"),
            KeyCode::PageUp => Some("PageUp"),
            KeyCode::PageDown => Some("PageDown"),
            _ => None,
        };
        if let Some(name) = key_name {
            // Mode-binding precedence: if the active editor mode has a
            // plugin-defined binding for this key, let it win instead
            // of applying the floating panel's default smart-key
            // behaviour. This is what `defineMode` exists for — a
            // plugin saying "in MY mode, Enter does X" must be
            // authoritative, not silently overridden by the host's
            // generic "Enter = focus-advance" default. The orchestrator
            // New-Session form relies on this so Enter submits the
            // form regardless of which field is focused (matching the
            // dialog's `Enter: submit` hint).
            //
            // Important: only count bindings that are *explicitly* set
            // for the mode (user / default / plugin defaults). The
            // resolver's full `resolve()` falls back to Normal-context
            // bindings for any mode, which would falsely report Enter
            // as bound everywhere (Normal's Enter inserts a newline).
            // We check the three context-scoped maps directly so the
            // Normal-fallback path doesn't taint the precedence check.
            let mode_has_binding = self
                .active_window()
                .editor_mode
                .as_ref()
                .map(|mode_name| {
                    let key_event = crossterm::event::KeyEvent::new(code, modifiers);
                    let mode_ctx =
                        crate::input::keybindings::KeyContext::Mode(mode_name.to_string());
                    let keybindings = self.keybindings.read().unwrap();
                    keybindings.has_explicit_binding(&key_event, &mode_ctx)
                })
                .unwrap_or(false);
            if mode_has_binding {
                return false;
            }
            self.handle_widget_command(
                &panel_key,
                fresh_core::api::WidgetAction::Key {
                    key: name.to_string(),
                },
            );
            return true;
        }
        if let KeyCode::Char(c) = code {
            // The active editor mode may have explicitly claimed this
            // char via `defineMode` — e.g. the Orchestrator picker
            // binds `Alt+N` (new session), `Alt+P` (scope), and `/`
            // (focus filter). Defer to that path so plugin-declared
            // modal shortcuts work. This now covers *plain* chars too
            // (not just Ctrl/Alt chords): a plugin that binds a bare
            // key like `/` gets it before the text-input fast path.
            // The trade-off is that a bound bare key can't also be
            // typed as text in that mode, which is what the plugin
            // asked for by binding it.
            {
                let mode_has_binding = self
                    .active_window()
                    .editor_mode
                    .as_ref()
                    .map(|mode_name| {
                        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
                        let mode_ctx =
                            crate::input::keybindings::KeyContext::Mode(mode_name.to_string());
                        let keybindings = self.keybindings.read().unwrap();
                        keybindings.has_explicit_binding(&key_event, &mode_ctx)
                    })
                    .unwrap_or(false);
                if mode_has_binding {
                    return false;
                }
            }
            // Ctrl/Alt-modified chords with no mode binding: a centered
            // modal swallows them (it must not leak keys to global
            // bindings like Ctrl-P). The non-modal dock does the
            // opposite — an unhandled shortcut returns focus to the
            // editor (blur) and falls through so the editor handles it
            // (e.g. Ctrl-P opens the command palette).
            if modifiers.intersects(KeyModifiers::CONTROL | KeyModifiers::ALT) {
                if matches!(
                    self.panel(slot).map(|f| f.placement),
                    Some(super::PanelPlacement::LeftDock { .. })
                ) {
                    self.blur_floating_panel(slot);
                    return false;
                }
                return true;
            }
            let ch = if modifiers.contains(KeyModifiers::SHIFT) {
                c.to_uppercase().next().unwrap_or(c)
            } else {
                c
            };
            // Space is a special case on a focused Toggle / Button:
            // the convention is "Space activates the focused
            // control", not "insert a literal space". Route it
            // through the smart-key dispatcher (which fires
            // `widget_event { event_type: "toggle" }` on a Toggle,
            // `activate` on a Button) instead of the text-input
            // fast path. For a focused Text widget the smart-key
            // dispatcher still inserts " " as a char, so typing
            // spaces into Project Path / Agent Command keeps
            // working.
            if ch == ' ' {
                self.handle_widget_command(
                    &panel_key,
                    fresh_core::api::WidgetAction::Key {
                        key: "Space".to_string(),
                    },
                );
                return true;
            }
            self.handle_widget_command(
                &panel_key,
                fresh_core::api::WidgetAction::TextInputChar {
                    text: ch.to_string(),
                },
            );
            return true;
        }
        // Any other keystroke that reaches here (function keys,
        // unhandled keycodes, etc.) is swallowed too — the modal
        // is the exclusive owner of the input channel until it
        // unmounts.
        true
    }

    /// If the Quick Open prompt is currently open, cancel it and return `true`.
    /// All four Quick Open variants (CommandPalette, QuickOpen, QuickOpenBuffers,
    /// QuickOpenFiles) toggle off when invoked while the picker is already visible.
    fn close_quick_open_if_open(&mut self) -> bool {
        if let Some(prompt) = &self.active_window_mut().prompt {
            if prompt.prompt_type == PromptType::QuickOpen {
                self.cancel_prompt();
                return true;
            }
        }
        false
    }

    /// Re-run the active search after a search-option flag is toggled.
    /// If a search prompt is open, updates incremental highlights from the
    /// prompt's current input. Otherwise re-executes the last completed search.
    fn refresh_active_search(&mut self) {
        if let Some(prompt) = &self.active_window_mut().prompt {
            if matches!(
                prompt.prompt_type,
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch
            ) {
                let query = prompt.input.clone();
                self.update_search_highlights(&query);
            }
        } else if let Some(search_state) = &self.active_window().search_state {
            let query = search_state.query.clone();
            self.perform_search(&query);
        }
    }

    /// Open a terminal in the utility dock, creating the dock split if none exists yet.
    fn handle_open_terminal_in_dock(&mut self) -> AnyhowResult<()> {
        use crate::model::event::SplitDirection;
        use crate::view::split::SplitRole;

        if let Some(dock_leaf) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .find_leaf_by_role(SplitRole::UtilityDock)
        {
            // Existing dock — focus it and let the regular open_terminal path attach a new tab.
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .set_active_split(dock_leaf);
            self.open_terminal();
            return Ok(());
        }

        // No dock yet. Spawn the PTY first so we have a real terminal buffer to seed the new
        // dock leaf with — otherwise the leaf would carry the user's previously-active buffer
        // as a placeholder and that buffer would linger as a phantom tab in the dock.
        let Some(terminal_id) = self.spawn_terminal_session() else {
            return Ok(());
        };
        let buffer_id = self.create_terminal_buffer_detached(terminal_id);

        // Split at the root so the dock spans the full width below any pre-existing side-by-side panes.
        let new_leaf = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .split_root_positioned(SplitDirection::Horizontal, buffer_id, 0.7, false)
            .map_err(|e| {
                self.set_status_message(format!("Failed to create dock for terminal: {}", e));
            });
        let Ok(new_leaf) = new_leaf else {
            return Ok(());
        };

        let mut view_state = crate::view::split::SplitViewState::with_buffer(
            self.terminal_width,
            self.terminal_height,
            buffer_id,
        );
        // Terminal-dedicated splits never show line numbers or current-line highlight.
        // (Mirrors the plugin-terminal split setup in `create_plugin_terminal`.)
        view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
            line_numbers: false,
            highlight_current_line: false,
            line_wrap: self.active_window().resolve_line_wrap_for_buffer(buffer_id),
            wrap_indent: self.config.editor.wrap_indent,
            wrap_column: self
                .active_window()
                .resolve_wrap_column_for_buffer(buffer_id),
            rulers: self.config.editor.rulers.clone(),
            scroll_offset: 0,
        });
        // Terminals don't wrap — keep escape sequences intact.
        view_state.viewport.line_wrap_enabled = false;

        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .insert(new_leaf, view_state);
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_leaf_role(new_leaf, Some(SplitRole::UtilityDock));
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_active_split(new_leaf);

        // Mirror open_terminal's post-attach bookkeeping. The buffer was
        // created via `create_terminal_buffer_detached`, so its remembered
        // mode is already Live.
        self.active_window_mut().terminal_mode = true;
        self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Terminal;
        self.active_window_mut().resize_visible_terminals();

        let exit_key = self
            .keybindings
            .read()
            .unwrap()
            .find_keybinding_for_action(
                "terminal_escape",
                crate::input::keybindings::KeyContext::Terminal,
            )
            .unwrap_or_else(|| "Ctrl+Space".to_string());
        self.set_status_message(
            rust_i18n::t!("terminal.opened", id = terminal_id.0, exit_key = exit_key).to_string(),
        );
        tracing::info!(
            "Opened terminal {:?} into new dock leaf {:?} (buffer {:?})",
            terminal_id,
            new_leaf,
            buffer_id
        );
        Ok(())
    }
}
