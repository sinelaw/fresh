//! Info/message popups: **the unfocused rung.**
//!
//! Everything else this used to carry is a property of the popup's own layer
//! now — the opaque rect that absorbed a stray press, the scrollbar track,
//! the wheel, the two dismiss guards, the row and close-button clicks, the
//! hover highlight, and, last, the keyboard of the popup that holds it,
//! whose seam names its key section for `frame::key_context_of`. What is
//! left is the interception for a popup that is *visible without being
//! focused* (nothing in the tree is listening for its keys, because nothing
//! in it has focus).

use crate::input::keybindings::Action;

use super::Editor;

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// The unfocused-popup interception, which is all that is left of the
    /// popup layer's keyboard here.
    ///
    /// A merely-visible popup doesn't capture the keyboard, but the user's
    /// bound popup-cancel (default Esc) and popup-focus (default Alt+T) keys
    /// must still affect it. `resolve_unfocused_popup_action` keeps its
    /// internal `popup_blocked_by_higher_modal` guard DELIBERATELY: the
    /// Prompt layer above declines keys its handler ignores (walk
    /// fall-through is broader than its `owns_keyboard` claim), and the old
    /// pipeline ran this interception before the prompt block only when no
    /// higher layer owned the keyboard — the guard is what keeps that
    /// precedence byte-identical on the walk.
    pub(super) fn dispatch_popup_keys(
        &mut self,
        event: &crossterm::event::KeyEvent,
    ) -> Option<anyhow::Result<crate::input::handler::InputResult>> {
        use crate::input::handler::InputResult;
        self.resolve_unfocused_popup_action(event)
            .map(|action| self.handle_action(action).map(|_| InputResult::Consumed))
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

    /// The topmost popup, on `handle_popup_confirm`'s own rule: global popups
    /// win over a buffer's while any is visible. Stated once so the popup a
    /// step *moves* and the popup it *confirms* cannot be different ones.
    pub(crate) fn topmost_popup_mut(&mut self) -> Option<&mut crate::view::popup::Popup> {
        match self.global_popups.is_visible() {
            true => self.global_popups.top_mut(),
            false => self.active_state_mut().popups.top_mut(),
        }
    }

    /// Carry out one step of the open popup's keyboard.
    ///
    /// **The arms `view/popup/input/` used to hold, minus the key matching.**
    /// Four files matched `KeyCode`s and called these methods; which key means
    /// which step is the keymap's answer now, declared on the open popup as
    /// shortcuts and resolved by the tree before anything else sees the key.
    /// What is left is what each step does.
    pub(crate) fn popup_key(&mut self, k: crate::view::shell::msg::PopupKey) {
        use crate::input::handler::{DeferredAction, InputContext};
        use crate::view::shell::msg::PopupKey as K;
        let mut ctx = InputContext::new();
        let mut pick = None;
        if let Some(p) = self.topmost_popup_mut() {
            match k {
                K::Prev => p.select_prev(),
                K::Next => p.select_next(),
                K::First => p.select_first(),
                K::Last => p.select_last(),
                K::PageUp => p.page_up(),
                K::PageDown => p.page_down(),
                K::ScrollUp => p.scroll_by(-1),
                K::ScrollDown => p.scroll_by(1),
                K::Confirm => ctx.defer(DeferredAction::ConfirmPopup),
                // Selecting and confirming in one step, and only when the row
                // is really there — the number keys' own rule.
                K::Pick(i) => pick = p.select_index(i).then_some(()),
                K::Close => ctx.defer(DeferredAction::ClosePopup),
                K::Copy => {
                    if p.has_selection() {
                        if let Some(text) = p.get_selected_text() {
                            ctx.defer(DeferredAction::CopyToClipboard(text));
                        }
                    }
                }
                K::TypeChar(c) => ctx.defer(DeferredAction::PopupTypeChar(c)),
                K::Backspace => ctx.defer(DeferredAction::PopupBackspace),
            }
        }
        if pick.is_some() {
            ctx.defer(DeferredAction::ConfirmPopup);
        }
        self.process_deferred_actions(ctx);
    }
}
