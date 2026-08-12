//! Editor-side input orchestration: the imperative shell of key handling.
//!
//! The layering here is deliberate. `Editor` is the *high level*: it owns
//! the pipeline order and applies effects, but the decisions — what a key
//! resolves to, which reading of a chord wins, what a keystroke aimed at
//! a widget panel means — live in Editor-free lower layers and never see
//! this struct:
//!
//!   - [`crate::input::router`] — pure decision functions over narrow
//!     views (this module builds the views and executes the outcomes);
//!   - [`crate::input::keybindings::KeybindingResolver`] — chord and
//!     binding resolution;
//!   - `app/overlay.rs` — the overlay `Layer` stack and its pure
//!     precedence functions.
//!
//! Executing a resolved [`Action`] is the editor's own high-level
//! behaviour and lives in `app/action_dispatch.rs`.

use super::*;
use crate::input::router;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

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
        use super::window::NextKeyClaim;
        // The queue/buffer state lives on the Window; the editor only
        // supplies the payload and performs the plugin resolution.
        let payload = router::key_event_to_payload(key_event);
        match self.active_window_mut().claim_next_key(payload) {
            NextKeyClaim::Resolve(callback_id, payload) => {
                let json = serde_json::to_string(&payload).unwrap_or_else(|_| "null".to_string());
                self.plugin_manager
                    .read()
                    .unwrap()
                    .resolve_callback(callback_id, json);
                true
            }
            NextKeyClaim::Buffered => true,
            NextKeyClaim::NotClaimed => false,
        }
    }

    /// Handle a key press that a terminal reported, resolving which of its two
    /// readings the keymap should see.
    ///
    /// A chord is both a physical key plus modifiers (`Ctrl+Shift+7`) and the
    /// character that key types (`&` on a US layout, `/` on a German one). The
    /// parser reports both when they disagree — see
    /// [`fresh_input_parser::KeyPress`] — because neither is right on its own:
    /// binding the physical chord leaves a German user's `Ctrl+/` firing
    /// `set_bookmark` (sinelaw/fresh#2933), and binding the typed character
    /// breaks every US `Ctrl+Shift+<digit>`.
    ///
    /// **The keymap decides.** The layout reading is tried first and used only
    /// if something is actually bound to it; otherwise the physical chord is
    /// handled exactly as before. So a US layout is unaffected — nothing binds
    /// `ctrl+&`, so `Ctrl+Shift+7` still reaches `set_bookmark` — while a German
    /// layout resolves the same keystroke to `ctrl+/`.
    ///
    /// One chord can only mean one thing, so this is a precedence, not a
    /// merge: where a keymap binds both readings, the layout one wins and the
    /// physical chord is unreachable from that key. A user who wants the other
    /// way round rebinds it.
    pub fn handle_key_press(&mut self, press: fresh_input_parser::KeyPress) -> AnyhowResult<()> {
        // The decision is the router's ([`router::layout_reading`]); this
        // shell only supplies the keymap and the current context.
        let layout = {
            let context = self.get_key_context();
            self.keybindings
                .read()
                .ok()
                .and_then(|kb| router::layout_reading(&press, &kb, context))
        };
        let (code, modifiers) = layout.unwrap_or((press.code, press.modifiers));
        self.handle_key(code, modifiers)
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

        // The native context menus (file-explorer / tab / "+" new-tab) are
        // modal: while one is open it owns the keyboard so navigation and
        // selection work and every other key is filtered out instead of
        // leaking into the active buffer or the explorer's type-ahead find
        // underneath. One handler covers all three.
        if let Some(result) = self.handle_context_menu_key(code, modifiers) {
            return result;
        }

        // Determine the current context first
        let mut context = self.get_key_context();

        // Transient popups (Hover, Signature Help) are dismissed on any
        // key press — for both focused and unfocused popups: an unfocused
        // hover popup that floats over the buffer must still vanish when
        // the user starts typing. The exceptions (Ctrl+C with a
        // selection, the focus-popup key) are the router's decision
        // ([`router::should_dismiss_transient_popup`]); this shell
        // supplies the topmost popup's state. Editor-level popups always
        // take precedence over buffer popups when both are visible.
        let popup_visible_on_screen =
            self.global_popups.is_visible() || self.active_state().popups.is_visible();
        if popup_visible_on_screen {
            let view = {
                let popup = self
                    .global_popups
                    .top()
                    .or_else(|| self.active_state().popups.top());
                router::TransientPopupView {
                    is_transient: popup.is_some_and(|p| p.transient),
                    has_selection: popup.is_some_and(|p| p.has_selection()),
                }
            };
            let dismiss = self.keybindings.read().is_ok_and(|kb| {
                router::should_dismiss_transient_popup(&view, &kb, context.clone(), &key_event)
            });
            if dismiss {
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
            if let Some(result) = self.dispatch_mode_bindings(&key_event, code, modifiers) {
                return result;
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

        // Resolve the key against the current context, chords first —
        // the decision is [`router::chord_or_key`]. An abandoned chord
        // prefix is cleared so it can't poison the next key.
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let disposition = {
            let keybindings = self.keybindings.read().unwrap();
            router::chord_or_key(
                &self.active_window().chord_state,
                &keybindings,
                &key_event,
                context.clone(),
            )
        };
        match disposition {
            router::ChordDisposition::Chord(action) => {
                // Complete chord match - execute action and clear chord state
                tracing::debug!("Complete chord match -> Action: {:?}", action);
                self.active_window_mut().chord_state.clear();
                self.handle_action(action)
            }
            router::ChordDisposition::Pending => {
                // Partial match - add to chord state and wait for more keys
                tracing::debug!("Partial chord match - waiting for next key");
                self.active_window_mut().chord_state.push((code, modifiers));
                Ok(())
            }
            router::ChordDisposition::Resolved(action) => {
                self.active_window_mut().chord_state.clear();
                tracing::trace!("Context: {:?} -> Action: {:?}", context, action);
                // Cancel pending LSP requests on user actions (except LSP
                // actions themselves) so stale completions don't show up
                // after the user has moved on.
                if router::cancels_pending_lsp(&action) {
                    self.active_window_mut().cancel_pending_lsp_requests();
                }
                // Keys the file browser ignores (its Alt+letter toggles) resolve
                // here in the Prompt context; the resulting prompt/file-browser
                // actions belong to the browser's state machine, not the generic
                // handler.
                if self.is_file_open_active() && self.handle_file_open_action(&action) {
                    return Ok(());
                }
                // Note: Modal components (Settings, Menu, Prompt, Popup, File
                // Browser) are handled by dispatch_modal_input using the
                // InputHandler system. All remaining actions delegate to
                // handle_action.
                self.handle_action(action)
            }
        }
    }

    /// Mode-binding stage of the pipeline: while the editor buffer has
    /// focus and a mode is active, a key may be claimed by the mode's
    /// chords and bindings, captured as text input, forwarded to a
    /// focused widget Text input, or blocked. The decision is
    /// [`router::mode_key_disposition`]; this shell builds the
    /// [`router::ModeKeyView`] from live state and applies the
    /// disposition. Returns `None` when no mode claims the key and the
    /// pipeline should continue.
    fn dispatch_mode_bindings(
        &mut self,
        key_event: &crossterm::event::KeyEvent,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> Option<AnyhowResult<()>> {
        use crate::input::router::{ModeKeyDisposition, WidgetSelectionMove};

        // effective_mode() returns buffer-local mode if present, else
        // global mode, so virtual buffer modes aren't hijacked by global
        // modes.
        let effective_mode = self.effective_mode().map(|s| s.to_owned());
        let allows_text_input = effective_mode
            .as_deref()
            .is_some_and(|m| self.mode_registry.allows_text_input(m));
        // Only a text-input mode routes selection keys to a focused
        // widget Text — don't pay for the panel lookup otherwise.
        let focused_widget_panel = if allows_text_input {
            let buffer_id = self.active_buffer();
            self.focused_text_widget_panel_for_buffer(buffer_id)
        } else {
            None
        };
        let view = router::ModeKeyView {
            allows_text_input,
            global_mode_read_only: self
                .active_window()
                .editor_mode
                .as_deref()
                .map(|m| self.mode_registry.is_read_only(m)),
            has_focused_text_widget: focused_widget_panel.is_some(),
            effective_mode,
        };
        let disposition = {
            let kb = self.keybindings.read().unwrap();
            router::mode_key_disposition(&view, &self.active_window().chord_state, &kb, key_event)
        };
        tracing::trace!(?disposition, mode = ?view.effective_mode, "mode-binding stage");
        // An abandoned chord prefix must not poison the next key. Only a
        // mode's own resolution clears it — with no mode active the
        // pending prefix belongs to the context-level chord stage below.
        if view.effective_mode.is_some() && !matches!(disposition, ModeKeyDisposition::ChordPending)
        {
            self.active_window_mut().chord_state.clear();
        }
        match disposition {
            ModeKeyDisposition::Run(action) => Some(self.handle_action(action)),
            ModeKeyDisposition::ChordPending => {
                self.active_window_mut().chord_state.push((code, modifiers));
                Some(Ok(()))
            }
            ModeKeyDisposition::TextInput(ch) => {
                let action_name = format!("mode_text_input:{}", ch);
                Some(self.handle_action(Action::PluginAction(action_name)))
            }
            ModeKeyDisposition::Forward(action) => Some(self.handle_action(action)),
            ModeKeyDisposition::WidgetSelection(mv) => {
                // Always consumed on a focused widget Text — a no-op move
                // (already at a boundary) is still the correct shortcut
                // behaviour.
                if let Some(panel_id) = focused_widget_panel {
                    let _ = match mv {
                        WidgetSelectionMove::WordLeft => self
                            .with_focused_text_editor(&panel_id, |e| e.move_word_left_selecting()),
                        WidgetSelectionMove::WordRight => self
                            .with_focused_text_editor(&panel_id, |e| e.move_word_right_selecting()),
                        WidgetSelectionMove::Left => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_left_selecting())
                        }
                        WidgetSelectionMove::Right => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_right_selecting())
                        }
                        WidgetSelectionMove::Up => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_up_selecting())
                        }
                        WidgetSelectionMove::Down => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_down_selecting())
                        }
                        WidgetSelectionMove::Home => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_home_selecting())
                        }
                        WidgetSelectionMove::End => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_end_selecting())
                        }
                    };
                }
                Some(Ok(()))
            }
            ModeKeyDisposition::Block => Some(Ok(())),
            ModeKeyDisposition::FallThrough => None,
        }
    }

    /// Deliver `key_event` to the buffer inside a focused, interactive
    /// `Pane` widget. Returns whether it was consumed.
    ///
    /// This is what makes a pane a *pane* rather than a picture. A
    /// terminal in a split is interactive because focusing it routes
    /// keys through `key_to_pty_bytes` to its PTY; a pane earns the same
    /// treatment by naming the same buffer, so the translation — app
    /// cursor mode, modifiers, the lot — is the shared one rather than a
    /// per-key table maintained alongside it.
    ///
    /// Terminals only. A file pane's keys would mean *editing*, which
    /// brings undo, LSP and save with it — a decision a panel should not
    /// make implicitly, so a non-terminal pane simply declines the key.
    fn focused_interactive_pane(
        &self,
        slot: super::PanelSlot,
    ) -> Option<(fresh_core::WindowId, BufferId)> {
        let panel = self.panel(slot)?;
        let focus_key = self
            .widget_registry
            .focus_key(&panel.panel_key)?
            .to_string();
        if focus_key.is_empty() {
            return None;
        }
        panel.embeds.iter().find_map(|e| {
            let key = e.key.as_deref()?;
            let buffer_id = e.buffer_id?;
            (e.interactive && key == focus_key).then_some((
                fresh_core::WindowId(e.window_id as u64),
                BufferId(buffer_id as usize),
            ))
        })
    }

    fn send_key_to_pane(
        &mut self,
        window_id: fresh_core::WindowId,
        buffer_id: BufferId,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> bool {
        let key_event = &crossterm::event::KeyEvent::new(code, modifiers);
        let Some(window) = self.windows.get(&window_id) else {
            return false;
        };
        let Some(terminal_id) = window.get_terminal_id(buffer_id) else {
            return false;
        };
        let Some(handle) = window.terminal_manager.get(terminal_id) else {
            return false;
        };
        let app_cursor = handle
            .state
            .lock()
            .map(|s| s.is_app_cursor())
            .unwrap_or(false);
        let Some(bytes) = crate::services::terminal::pty::key_to_pty_bytes(
            key_event.code,
            key_event.modifiers,
            app_cursor,
        ) else {
            return false;
        };
        handle.write(&bytes);
        true
    }

    /// When an *unfocused* popup is on screen, resolve the key event
    /// against `KeyContext::Popup`/`Global` so the user's bound
    /// `popup_cancel` (default Esc) and `popup_focus` (default Alt+T)
    /// keys still take effect even though the popup isn't claiming the
    /// keyboard. Without this, dismissing an LSP auto-prompt with Esc
    /// would silently fall through to the buffer.
    ///
    /// The state guards live here; the keybinding precedence decision is
    /// [`router::unfocused_popup_action`].
    pub(crate) fn resolve_unfocused_popup_action(
        &self,
        event: &crossterm::event::KeyEvent,
    ) -> Option<crate::input::keybindings::Action> {
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
        if crate::app::overlay::popup_blocked_by_higher_modal(&self.overlay_layers()) {
            return None;
        }

        let kb = self.keybindings.read().ok()?;
        router::unfocused_popup_action(self.active_window().key_context.clone(), &kb, event)
    }

    /// Resolve a key event against `KeyContext::Completion` when the topmost
    /// visible popup is a completion popup. The gating and precedence
    /// decision is [`router::completion_popup_action`]; this shell supplies
    /// the topmost popup kind and the keymap.
    pub(crate) fn resolve_completion_popup_action(
        &self,
        event: &crossterm::event::KeyEvent,
    ) -> Option<crate::input::keybindings::Action> {
        let topmost_kind = if self.global_popups.is_visible() {
            self.global_popups.top().map(|p| p.kind)
        } else if self.active_state().popups.is_visible() {
            self.active_state().popups.top().map(|p| p.kind)
        } else {
            None
        };
        let kb = self.keybindings.read().unwrap();
        router::completion_popup_action(topmost_kind, &kb, event)
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
    /// The decision — what the key *means* given the panel's placement,
    /// focused widget and the active editor mode — is
    /// [`router::widget_panel_key`], which is pure and Editor-free. This
    /// shell builds the [`router::WidgetPanelView`] from live state and
    /// executes the outcome it names.
    fn dispatch_floating_widget_key(
        &mut self,
        slot: super::PanelSlot,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> bool {
        use crate::input::router::WidgetKeyOutcome;
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
        let view = router::WidgetPanelView {
            is_left_dock: matches!(
                self.panel(slot).map(|f| f.placement),
                Some(super::PanelPlacement::LeftDock { .. })
            ),
            focus_key: self
                .widget_registry
                .focus_key(&panel_key)
                .map(str::to_string),
            focused_widget_is_text: self.panel_focused_widget_is_text(&panel_key),
            focused_text_completions_open: self.focused_text_completions_open(&panel_key),
            editor_mode: self.active_window().editor_mode.clone(),
        };
        let outcome = {
            let kb = self.keybindings.read().unwrap();
            router::widget_panel_key(&view, &kb, code, modifiers)
        };
        tracing::debug!(
            target: "fresh::dock",
            panel = %panel_key,
            ?slot,
            ?code,
            modifiers = ?modifiers,
            focus_key = ?view.focus_key,
            ?outcome,
            "dispatch_floating_widget_key: decision"
        );
        // An interactive `Pane` behaves like a focused terminal split: the
        // buffer inside it takes the keystroke. Anything the outcome would
        // otherwise consume as text or a smart key belongs to that PTY.
        //
        // `FallThrough` is deliberately excluded. That is how the plugin's own
        // mode bindings get a look — without it a focused pane would swallow
        // the very key that gets you back out of it.
        if !matches!(outcome, WidgetKeyOutcome::FallThrough) {
            if let Some((window_id, buffer_id)) = self.focused_interactive_pane(slot) {
                if self.send_key_to_pane(window_id, buffer_id, code, modifiers) {
                    return true;
                }
            }
        }

        match outcome {
            WidgetKeyOutcome::DockEvent(event_type) => {
                self.fire_dock_widget_event(&panel_key, event_type);
                true
            }
            WidgetKeyOutcome::FocusWidget(key) => {
                self.set_panel_focus_and_notify(&panel_key, key.to_string());
                true
            }
            WidgetKeyOutcome::Blur => {
                self.blur_floating_panel(slot);
                true
            }
            WidgetKeyOutcome::BlurUnconsumed => {
                self.blur_floating_panel(slot);
                false
            }
            WidgetKeyOutcome::CancelAndUnmount => {
                // Fire a `widget_event` `cancel` so the plugin can clean up
                // its own state (clear mode, drop form state, etc.).
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
                true
            }
            WidgetKeyOutcome::SmartKey(name) => {
                self.handle_widget_command(
                    &panel_key,
                    fresh_core::api::WidgetAction::Key {
                        key: name.to_string(),
                    },
                );
                true
            }
            WidgetKeyOutcome::TextChar(ch) => {
                self.handle_widget_command(
                    &panel_key,
                    fresh_core::api::WidgetAction::TextInputChar {
                        text: ch.to_string(),
                    },
                );
                true
            }
            WidgetKeyOutcome::Paste => {
                if let Some(text) = self.clipboard.paste() {
                    // Normalise line endings to LF, matching the
                    // Action::Paste widget branch; single-line TextEdit
                    // strips embedded newlines itself.
                    let normalized = text.replace("\r\n", "\n").replace('\r', "\n");
                    self.handle_widget_insert_str(&panel_key, &normalized);
                    self.set_status_message(t!("clipboard.pasted").to_string());
                }
                true
            }
            WidgetKeyOutcome::Copy => {
                self.handle_widget_copy(&panel_key);
                true
            }
            WidgetKeyOutcome::Cut => {
                self.handle_widget_cut(&panel_key);
                true
            }
            WidgetKeyOutcome::SelectAll => {
                self.handle_widget_select_all(&panel_key);
                true
            }
            WidgetKeyOutcome::Swallow => true,
            WidgetKeyOutcome::FallThrough => false,
        }
    }
}
