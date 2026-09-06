//! The editor content as the keyboard owner of last resort.
//!
//! The base surface had a pointer half, and both halves of it are in the
//! shell's tree now. The **tab-menu dismissal** — a right-click anywhere
//! clears the transient menus — is a capture-phase listener on the frame
//! (`shell::splits::tab_menu_guard`), which is where "anywhere" can mean it:
//! this walk only ever ran for the events the tree declined. The **wheel's
//! drop floor** — chrome owning no scrollable content drops the notch rather
//! than handing it to the focused pane (sinelaw/fresh#2969) — is the same
//! statement made by nothing claiming: every scrollable surface claims its own
//! wheel in the tree, and a notch nothing claimed simply ends.

use anyhow::Result as AnyhowResult;

use super::Editor;

/// Behavior owned by this component: the key pipeline's tail (moved
/// verbatim from `handle_key` — the editor content's own keyboard
/// handling, reached when every layer above has declined).
impl Editor {
    /// Mode bindings, composite-buffer routing, then chord/keybinding
    /// resolution against the current context. The context is derived
    /// fresh from the layer stack here: a rung above may have
    /// dismissed a popup (completion returning `Ignored` closes it via
    /// deferred actions), and the old pipeline's post-modal recalc is
    /// exactly this.
    pub(crate) fn dispatch_base_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        use crate::input::router;
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);

        // **The unfocused-popup interception, first.** A merely-visible popup
        // holds no focus, so nothing in the tree is listening for it, and the
        // user's bound popup-cancel (default Esc) and popup-focus (default
        // Alt+T) are ordinary editor bindings that must still find it — which
        // makes this a rung of the keymap rather than a layer's dispatch. It
        // rode `chrome::Popups::on_layer_key` at `layer_rank::POPUP` only
        // because the ranked walk was the one place a rung could sit; the two
        // ranks between it and this one are off the walk now, so running it
        // here is the same order it always had.
        if let Some(r) = self.dispatch_popup_keys(&key_event) {
            return r.map(|_| ());
        }

        let context = self.get_key_context();

        // **The PTY is the leaf's.** The context is `Terminal` only when the
        // active pane's leaf — whose settled fact it is — holds the
        // keyboard, so a key that resolves here was routed to the terminal
        // by the tree; the forwarding checks the tree's own gate
        // (`Ui::raw_input`) and takes the key raw, save the bypass and UI
        // actions it lets through. A terminal parked in scroll-back is the
        // plain content (`Normal`), and a key that re-enters terminal mode
        // is answered there, by the same dispatch.
        if matches!(
            context,
            crate::input::keybindings::KeyContext::Terminal
                | crate::input::keybindings::KeyContext::Normal
        ) && self.dispatch_terminal_input(&key_event, &context).is_some()
        {
            return Ok(());
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
                // All remaining actions delegate to handle_action.
                self.handle_action(action)
            }
        }
    }
}
