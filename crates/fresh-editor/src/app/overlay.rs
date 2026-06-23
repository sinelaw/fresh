//! Unified overlay **layer** model (P2).
//!
//! The editor presents a stack of overlays on top of the editor content:
//! the event-debug dialog, full-screen modals (settings, keybinding editor,
//! calibration wizard, workspace-trust prompt), the menu, the prompt,
//! popups, the centered widget modal, and the left dock. Each one used to
//! have its own focus-precedence, terminal-blocking and mouse-capture
//! logic scattered across `input.rs`, `input_dispatch.rs`, `mouse_input.rs`
//! and `render.rs` — and the conditional ladders went out of sync (the
//! mouse handler's modal precedence didn't match the keyboard handler's,
//! `dispatch_terminal_input`'s `in_modal` predicate over-listed the same
//! fields, the unfocused-popup guard re-listed Settings/Menu/Prompt).
//!
//! This module makes the stack a first-class ordered list. Every callsite
//! that asks "which overlay is in charge?" — keyboard focus
//! (`get_key_context`), the unfocused-popup modal guard
//! (`resolve_unfocused_popup_action`), the terminal-input gate
//! (`dispatch_terminal_input`) and the mouse early-capture ladder
//! (`handle_mouse`) — reads from the *same* `Editor::overlay_layers()`
//! list, so the precedence rules live in one place.

use crate::input::keybindings::KeyContext;

/// Identifies a concrete overlay. The ordering of `overlay_layers`
/// (top-first), not this enum's declaration order, defines precedence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LayerKind {
    /// The event-debug dialog (`active_window().event_debug`) — a
    /// full-screen modal with its own input dispatcher.
    EventDebug,
    Settings,
    /// The keybinding editor (`keybinding_editor`) — a full-screen modal
    /// with its own input dispatcher; transparent to `KeyContext`-driven
    /// keybinding resolution.
    KeybindingEditor,
    /// The calibration wizard (`calibration_wizard`) — same as above.
    CalibrationWizard,
    /// The workspace-trust prompt: a global popup whose top resolver is
    /// `PopupResolver::WorkspaceTrust`, painted in the modal z-band and
    /// dispatched by a bespoke mouse/key handler. Distinct from `Popup`
    /// so its dedicated dispatchers can be located top-down by kind.
    WorkspaceTrust,
    Menu,
    Prompt,
    Popup,
    /// The tab bar's "+" new-tab popup (`active_window().new_tab_menu`). A
    /// modal chrome menu with a custom key dispatcher
    /// (`handle_new_tab_menu_key`), so it's transparent to `KeyContext`
    /// resolution but still blocks PTY routing while open.
    NewTabMenu,
    /// The tab right-click context menu (`active_window().tab_context_menu`),
    /// same treatment as `NewTabMenu`.
    TabContextMenu,
    /// The centered widget modal (`floating_widget_panel`).
    FloatingModal,
    /// The editor-global left dock (`dock`).
    Dock,
    /// The editor content / window splits — the bottom layer.
    Editor,
}

/// One entry in the overlay stack: a present overlay (or the always-present
/// editor base), with the per-layer flags the dispatchers need.
#[derive(Debug, Clone)]
pub(crate) struct Layer {
    pub kind: LayerKind,
    /// Whether this layer currently owns the keyboard. Modal layers set
    /// this whenever present; focusable layers (dock, popup) only while
    /// focused/capturing; the editor base always sets it so a top-down
    /// walk always terminates.
    pub owns_keyboard: bool,
    /// The keybinding context to resolve against when this layer is the
    /// keyboard owner. `None` for layers whose keys are intercepted by a
    /// custom dispatcher (event-debug, calibration wizard, keybinding
    /// editor) and never reach `KeyContext`-driven resolution — they are
    /// transparent to `resolve_focus_context`, which keeps walking below
    /// them.
    pub key_context: Option<KeyContext>,
    /// Whether this layer, while present, blocks routing of keys to the
    /// PTY child of a terminal buffer underneath. A blurred dock leaves
    /// the terminal usable; a merely-visible popup does not (it covers
    /// the active buffer and the user's keystrokes belong to the popup).
    pub blocks_terminal_input: bool,
}

/// Resolve the keyboard-owning `KeyContext` from an ordered (top-first)
/// layer list: the first owning layer that has a `KeyContext` wins.
/// Layers without a `KeyContext` (custom-dispatch modals) are skipped —
/// their input dispatcher has already intercepted keys upstream, so they
/// are transparent to `KeyContext`-driven resolution.
///
/// The editor base layer always owns and has a `KeyContext`, so this
/// never returns `None` for a well-formed stack.
pub(crate) fn resolve_focus_context(layers: &[Layer]) -> Option<KeyContext> {
    layers
        .iter()
        .find(|l| l.owns_keyboard && l.key_context.is_some())
        .and_then(|l| l.key_context.clone())
}

/// True iff any layer in the stack currently blocks routing to the PTY
/// child of a terminal buffer underneath.
pub(crate) fn any_layer_blocks_terminal_input(layers: &[Layer]) -> bool {
    layers.iter().any(|l| l.blocks_terminal_input)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn layer(kind: LayerKind, owns: bool, ctx: Option<KeyContext>, blocks: bool) -> Layer {
        Layer {
            kind,
            owns_keyboard: owns,
            key_context: ctx,
            blocks_terminal_input: blocks,
        }
    }

    fn base() -> Layer {
        layer(LayerKind::Editor, true, Some(KeyContext::Normal), false)
    }

    #[test]
    fn topmost_owning_layer_wins() {
        let layers = [
            layer(
                LayerKind::Settings,
                false,
                Some(KeyContext::Settings),
                false,
            ),
            layer(LayerKind::Popup, true, Some(KeyContext::Popup), true),
            layer(LayerKind::Dock, true, Some(KeyContext::Dock), true),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Popup));
    }

    #[test]
    fn falls_through_unfocused_layers_to_base() {
        let layers = [
            layer(
                LayerKind::FloatingModal,
                false,
                Some(KeyContext::Normal),
                true,
            ),
            layer(LayerKind::Dock, false, Some(KeyContext::Dock), false),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Normal));
    }

    #[test]
    fn base_layer_terminates_the_walk() {
        let layers = [base()];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Normal));
        assert!(!any_layer_blocks_terminal_input(&layers));
    }

    /// Custom-dispatch modals own the keyboard but expose no
    /// `KeyContext`. `resolve_focus_context` must walk past them and
    /// return the base context — matching the historical behavior when
    /// `get_key_context` happened to be queried while one of those
    /// modals was up.
    #[test]
    fn keycontext_walk_is_transparent_to_custom_dispatch_modals() {
        let layers = [
            layer(LayerKind::CalibrationWizard, true, None, true),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Normal));
        assert!(any_layer_blocks_terminal_input(&layers));
    }

    /// A merely-visible (unfocused) popup blocks PTY routing — it
    /// covers the active buffer. A blurred dock does not block; a
    /// focused dock does.
    #[test]
    fn terminal_blocking_differs_from_keyboard_ownership() {
        let popup_visible_not_capturing = [
            layer(LayerKind::Popup, false, Some(KeyContext::Popup), true),
            base(),
        ];
        assert_eq!(
            resolve_focus_context(&popup_visible_not_capturing),
            Some(KeyContext::Normal),
        );
        assert!(any_layer_blocks_terminal_input(
            &popup_visible_not_capturing
        ));

        let blurred_dock = [
            layer(LayerKind::Dock, false, Some(KeyContext::Dock), false),
            base(),
        ];
        assert!(!any_layer_blocks_terminal_input(&blurred_dock));
    }
}
