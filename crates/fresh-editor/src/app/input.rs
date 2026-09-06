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
use fresh_i18n::t;

impl Editor {
    /// Dispatch a text-input mode's typed character to its plugin
    /// through the typed fast lane (`PluginRequest::ModeTextInput`) —
    /// same ordered queue as every other dispatched plugin action, so
    /// the mode's own bindings (Backspace, Space, …) and plain
    /// characters cannot reorder. Replaces the legacy
    /// `PluginAction("mode_text_input@<mode>:<char>")` string encoding.
    pub(crate) fn dispatch_mode_text_input(&mut self, mode: Option<&str>, ch: char) {
        #[cfg(feature = "plugins")]
        {
            let text = ch.to_string();
            let result = self
                .plugin_manager
                .read()
                .unwrap()
                .mode_text_input_async(mode, &text);
            match result {
                Some(Ok(receiver)) => {
                    // Same pending-action bookkeeping as the
                    // `Action::PluginAction` arm; the label is for
                    // logging only.
                    let label = match mode {
                        Some(m) => format!("mode_text_input@{}", m),
                        None => "mode_text_input".to_string(),
                    };
                    self.pending_plugin_actions.push((label, receiver));
                }
                Some(Err(e)) => {
                    self.set_status_message(
                        t!("view.plugin_error", error = e.to_string()).to_string(),
                    );
                    tracing::error!("mode text-input dispatch error: {}", e);
                }
                None => {
                    self.set_status_message(t!("status.plugin_manager_unavailable").to_string());
                }
            }
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = (mode, ch);
        }
    }

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
        // **Input is never routed over a tree older than the facts it routes
        // over.** Every read below that asks the tree where the keyboard is
        // — the PTY gate, the key context, the unfocused-popup guard — reads
        // a tree laid out from the facts as they stand; and a key may change
        // any of them, so the description is stale once it has been handled,
        // and the next reader lays it out again.
        self.lay_out_shell_if_stale();
        let r = self.handle_key_routed(code, modifiers);
        self.shell_description_stale = true;
        r
    }

    fn handle_key_routed(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        let _t_total = std::time::Instant::now();

        // Any keystroke may change routing-relevant UI state (open/close a
        // popup, move focus, toggle a mode), so advance the coarse UI
        // generation up front. Bumping on ENTRY (not exit) still lets the
        // chrome-tree reads *within* this keystroke share one rebuild — the

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
        // at the buffer, the dock is swallowing them (its
        // `view::shell::panel::keys_layer`, whose applier runs
        // `dispatch_floating_widget_key`) — a
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

        // The pre-band's chrome keyboard grabs are gone. The stage existed
        // for two shapes a `layer_rank` cannot express, and it outranked
        // every rank by pipeline position — which is why membership was
        // restricted by ruling rather than left open. Both members have since
        // been said as something else: the context menus' navigation grab
        // became a `Layer` answered by the walk below, and the theme
        // inspector's dismiss-and-continue observer became
        // `Dismiss { any_key }.passing_through()` on its own layer, which is
        // what "a keyboard `PassAfter`" was a description of.
        //
        // A grab band with no members is a rank inversion waiting for the next
        // surface that finds it convenient, so it goes with them.

        // Transient popups (Hover, Signature Help) are dismissed on any
        // key press — for both focused and unfocused popups: an unfocused
        // hover popup that floats over the buffer must still vanish when
        // the user starts typing. The exceptions (Ctrl+C with a
        // selection, the focus-popup key) are the router's decision
        // ([`router::should_dismiss_transient_popup`]); this shell
        // supplies the topmost popup's state. Editor-level popups always
        // take precedence over buffer popups when both are visible.
        // Deliberately PRE-WALK: an observer that must see the key even
        // when a higher modal will consume it (typing under Settings
        // still dismisses a hover popup) — no first-consumer walk can
        // express that, so it stays a pre-band stage like event-debug.
        let popup_visible_on_screen =
            self.global_popups.is_visible() || self.active_state().popups.is_visible();
        if popup_visible_on_screen {
            let context = self.get_key_context();
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
                router::should_dismiss_transient_popup(&view, &kb, context, &key_event)
            });
            if dismiss {
                self.hide_popup();
                tracing::debug!("Dismissed transient popup on key press");
                // No context recalc needed here: every stage below
                // derives its context fresh from the layer stack.
            }
        }

        // The migration shell is offered the key, and answers only for
        // surfaces that have migrated their keyboard half — the context menus
        // so far, whose open layer is modal and swallows everything it does
        // not act on. Every other key falls through to the walk below.
        // `dispatch` reports whether the tree claimed it rather than the
        // caller inferring it from the messages: a modal swallow produces no
        // message at all, and inferring claim from "said something" would let
        // those keys straight through.
        self.shell_key_event = Some(key_event);
        match crate::view::shell::input::key(&fresh_input_parser::KeyPress::new(key_event)) {
            Some(input) => {
                if self.shell_dispatch(input).claimed {
                    return Ok(());
                }
            }
            // **A key the tree has no vocabulary for still cannot walk past a
            // surface that owns the keyboard.** Declining to translate it
            // costs the key its *routing*; letting it fall to the walk below
            // would cost a modal its containment, and the media key would
            // reach whatever the modal is covering. `keyboard_owned` is the
            // same containment question the claim above answers.
            None if self.shell_ui.as_ref().is_some_and(|ui| ui.keyboard_owned()) => {
                return Ok(());
            }
            None => {}
        }

        // **The pipeline tail.** Everything that used to be offered the key
        // ahead of this — the capture-all modals, the workspace-trust prompt,
        // the menu, the popups, the prompt, a focused dock or plugin panel —
        // is a layer in the shell tree now and claimed above if it wanted the
        // key. What is left is the editor content's own keyboard: mode
        // bindings, composite routing, the unfocused-popup interception and
        // chord/keybinding resolution, in `chrome::base`.
        //
        // This was `dispatch_layer_keyboard`, a walk down an owner-stamped
        // `overlay_stack()` offering each layer's component an `on_layer_key`.
        // The stack is still derived and still read, but nothing dispatches
        // through it any more, so the walk is a call and the owner stamp that
        // addressed the dispatch is gone with it. Two readers still need it
        // ORDERED — `get_key_context` and the unfocused-popup guard below;
        // the rest (the PTY gate, the LSP-hover suppressor, the caret
        // suppression) ask only whether a layer is present and read
        // `Editor::overlay_layer_set`.
        self.dispatch_base_key(code, modifiers)?;
        Ok(())
    }

    /// Mode-binding stage of the pipeline: while the editor buffer has
    /// focus and a mode is active, a key may be claimed by the mode's
    /// chords and bindings, captured as text input, forwarded to a
    /// focused widget Text input, or blocked. The decision is
    /// [`router::mode_key_disposition`]; this shell builds the
    /// [`router::ModeKeyView`] from live state and applies the
    /// disposition. Returns `None` when no mode claims the key and the
    /// pipeline should continue.
    pub(super) fn dispatch_mode_bindings(
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
                // Typed fast lane: the mode and character travel as
                // structured fields, not spliced into an action-name
                // string. The dispatch stays mode-qualified so it
                // reaches the plugin that *defined* the mode —
                // `mode_text_input` alone is one global name, and
                // several text-input modes would otherwise fight over
                // it. Deliberately still asynchronous through the same
                // ordered plugin queue: a mode's other bindings (Space,
                // Backspace, …) edit the same field through that queue,
                // and taking a host-side shortcut here would let plain
                // characters overtake them and scramble the typed text.
                let mode = view.effective_mode.clone();
                self.dispatch_mode_text_input(mode.as_deref(), ch);
                Some(Ok(()))
            }
            ModeKeyDisposition::Forward(action) => Some(self.handle_action(action)),
            ModeKeyDisposition::WidgetSelection { mv, extend } => {
                // Always consumed on a focused widget Text — a no-op move
                // (already at a boundary) is still the correct shortcut
                // behaviour. `extend` distinguishes Shift+nav, which grows
                // the selection, from a plain move, which collapses it.
                if let Some(panel_id) = focused_widget_panel {
                    let _ = match (mv, extend) {
                        (WidgetSelectionMove::WordLeft, true) => self
                            .with_focused_text_editor(&panel_id, |e| e.move_word_left_selecting()),
                        (WidgetSelectionMove::WordRight, true) => self
                            .with_focused_text_editor(&panel_id, |e| e.move_word_right_selecting()),
                        (WidgetSelectionMove::Left, true) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_left_selecting())
                        }
                        (WidgetSelectionMove::Right, true) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_right_selecting())
                        }
                        (WidgetSelectionMove::Up, true) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_up_selecting())
                        }
                        (WidgetSelectionMove::Down, true) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_down_selecting())
                        }
                        (WidgetSelectionMove::Home, true) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_home_selecting())
                        }
                        (WidgetSelectionMove::End, true) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_end_selecting())
                        }
                        (WidgetSelectionMove::WordLeft, false) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_word_left())
                        }
                        (WidgetSelectionMove::WordRight, false) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_word_right())
                        }
                        (WidgetSelectionMove::Left, false) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_left())
                        }
                        (WidgetSelectionMove::Right, false) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_right())
                        }
                        (WidgetSelectionMove::Up, false) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_up())
                        }
                        (WidgetSelectionMove::Down, false) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_down())
                        }
                        (WidgetSelectionMove::Home, false) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_home())
                        }
                        (WidgetSelectionMove::End, false) => {
                            self.with_focused_text_editor(&panel_id, |e| e.move_end())
                        }
                    };
                }
                Some(Ok(()))
            }
            ModeKeyDisposition::Block => Some(Ok(())),
            ModeKeyDisposition::FallThrough => None,
        }
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
        // pressing Esc in a settings dialog still closes the dialog rather
        // than reaching past it to dismiss a stale popup.
        //
        // **Not `Ui::focus_confined`, though it looks like the same question.**
        // A layer dismissed by this very keystroke goes on confining focus
        // until the app stops declaring it, and this guard runs inside
        // `dispatch_base_key` — reached precisely when a surface declined,
        // including one that dismissed itself passing through. It needs
        // post-mutation truth, which is what the re-derived stack gives it.
        if self.shell_ui.as_ref().is_some_and(|ui| ui.keyboard_owned()) {
            return None;
        }

        let kb = self.keybindings.read().ok()?;
        router::unfocused_popup_action(self.active_window().key_context.clone(), &kb, event)
    }

    // **The completion resolver is the popup's own now.** It asked the keymap
    // for the key as it arrived, from inside a walk the shell tree is offered
    // the key *before* — the same shape that let a `menu`-section binding be
    // swallowed before the keymap was ever consulted. The bindings are
    // enumerated instead (`Editor::popup_keys`) and declared on the open
    // popup's layer, where nothing is in front of them.

    /// Route a keystroke to the floating widget panel when one is
    /// mounted. Returns `true` if the key was consumed.
    ///
    /// The decision — what the key *means* given the panel's placement,
    /// focused widget and the active editor mode — is
    /// [`router::widget_panel_key`], which is pure and Editor-free. This
    /// shell builds the [`router::WidgetPanelView`] from live state and
    /// executes the outcome it names.
    pub(super) fn dispatch_floating_widget_key(
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
            non_modal: matches!(
                self.panel(slot).map(|f| f.placement),
                Some(super::PanelPlacement::LeftDock { .. })
            ) || matches!(slot, super::PanelSlot::Sidebar(_)),
            focus_key: self
                .widget_registry
                .focus_key(&panel_key)
                .map(str::to_string),
            focused_widget_is_text: self.panel_focused_widget_is_text(&panel_key),
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
        match outcome {
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
                if let Some(o) = self.panel_opt_mut(slot) {
                    *o = None;
                }
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
        }
    }
}
