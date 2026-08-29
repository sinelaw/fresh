//! Info/message popups: **the keyboard, and nothing else.**
//!
//! Everything this component used to carry is a property of the popup's own
//! layer now — the opaque rect that absorbed a stray press, the scrollbar
//! track, the wheel, the two dismiss guards, the row and close-button clicks,
//! the hover highlight. What is left is `dispatch_popup_keys` and the layer
//! rank that says a visible popup blocks the PTY, which are about who owns the
//! keyboard rather than about where anything is on screen.

use crate::input::keybindings::Action;

use super::{ChromeComponent, Editor};

pub(crate) struct Popups;

impl ChromeComponent for Popups {
    // No wheel arm. The popup's window is a viewport in the shell's tree and
    // takes its own wheel, vertical and horizontal — which is also what stops
    // a horizontal delta panning the buffer underneath, since a layer's
    // content claims the event rather than a guard absorbing it.

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        // A non-trust popup is *present* whenever visible, but only
        // *owns* the keyboard while capturing; a merely-visible
        // unfocused popup falls through. Either way a visible popup
        // blocks PTY routing — it covers the active buffer. While the
        // workspace-trust prompt tops the global stack, its dedicated
        // layer (the modals component) takes this one's place.
        if !ed.workspace_trust_on_top()
            && (ed.global_popups.is_visible() || ed.active_state().popups.is_visible())
        {
            out.push((
                super::layer_rank::POPUP,
                Layer {
                    kind: LayerKind::Popup,
                    owns_keyboard: ed.popups_capture_keys(),
                    key_context: Some(crate::input::keybindings::KeyContext::Popup),
                    blocks_terminal_input: true,
                },
            ));
        }
    }

    fn on_layer_key(
        &self,
        ed: &mut Editor,
        _layer: &crate::app::overlay::Layer,
        event: &crossterm::event::KeyEvent,
    ) -> Option<anyhow::Result<crate::input::handler::InputResult>> {
        ed.dispatch_popup_keys(event)
    }
}

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// Keyboard for the popup layer (the rungs of
    /// `dispatch_modal_input`'s popup block plus `handle_key`'s
    /// unfocused-popup interception, moved here — offered by the
    /// layer walk when it reaches the Popup layer).
    ///
    /// The unfocused rung runs first: a merely-visible popup doesn't
    /// capture the keyboard, but the user's bound popup-cancel
    /// (default Esc) and popup-focus (default Alt+T) keys must still
    /// affect it. `resolve_unfocused_popup_action` keeps its internal
    /// `popup_blocked_by_higher_modal` guard DELIBERATELY: the Prompt
    /// layer above declines keys its handler ignores (walk
    /// fall-through is broader than its `owns_keyboard` claim), and
    /// the old pipeline ran this interception before the prompt block
    /// only when no higher layer owned the keyboard — the guard is
    /// what keeps that precedence byte-identical on the walk.
    ///
    /// The capturing rungs mirror the old block exactly: completion
    /// resolver → global popups → buffer popups, with the global
    /// rung's Ignored deliberately returning `None` without trying
    /// buffer popups (its dispatch may have queued a ClosePopup that
    /// the deferred-action processor has already fired).
    pub(super) fn dispatch_popup_keys(
        &mut self,
        event: &crossterm::event::KeyEvent,
    ) -> Option<anyhow::Result<crate::input::handler::InputResult>> {
        use crate::input::handler::{InputContext, InputHandler, InputResult};

        if let Some(action) = self.resolve_unfocused_popup_action(event) {
            return Some(self.handle_action(action).map(|_| InputResult::Consumed));
        }

        if !self.popups_capture_keys() {
            return None;
        }

        let mut ctx = InputContext::new();

        // Completion popups consult the keybinding resolver in the
        // `Completion` context first, so accept/dismiss can be remapped
        // via the keybinding editor. Falls through to the popup's own
        // handler for everything else (type-to-filter, navigation, etc.).
        if let Some(action) = self.resolve_completion_popup_action(event) {
            self.process_deferred_actions(ctx);
            if let Err(e) = self.handle_action(action) {
                tracing::warn!("Completion popup action failed: {}", e);
            }
            return Some(Ok(InputResult::Consumed));
        }

        // (The workspace-trust rung lives with the WorkspaceTrust
        // component now — its 870-ranked layer replaces this one while
        // the trust prompt tops the global stack, so the walk never
        // reaches here in that state.)

        // Editor-level (global) popups take precedence over buffer popups
        // so that plugin notifications stay focused even when the active
        // buffer owns its own popup stack.
        if self.global_popups.is_visible() {
            let result = self.global_popups.dispatch_input(event, &mut ctx);
            self.process_deferred_actions(ctx);
            if result != InputResult::Ignored {
                return Some(Ok(result));
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
                return Some(Ok(result));
            }
        }

        None
    }

    /// Choose a row of the topmost popup, then confirm it.
    ///
    /// The tail of `handle_click_global_popups` and `handle_click_buffer_popups`
    /// with the hit-test taken off the front — a list row that answers its own
    /// click has an index, and asking it to report a screen position so the
    /// editor can hit-test its way back to that index is the round trip the
    /// migration removes.
    ///
    /// "Topmost" is `handle_popup_confirm`'s own rule, restated so the row that
    /// is *selected* and the popup that is *confirmed* cannot be different
    /// ones: global popups win over a buffer's while any is visible.
    pub(crate) fn select_popup_item(&mut self, index: usize) {
        let set = |p: &mut crate::view::popup::Popup| {
            if let crate::view::popup::PopupContent::List { selected, .. } = &mut p.content {
                *selected = index;
            }
        };
        if self.global_popups.is_visible() {
            if let Some(p) = self.global_popups.top_mut() {
                set(p);
            }
        } else if let Some(p) = self.active_state_mut().popups.top_mut() {
            set(p);
        }
        if let Err(e) = self.handle_action(Action::PopupConfirm) {
            tracing::warn!("popup confirm failed: {e}");
        }
    }
}
