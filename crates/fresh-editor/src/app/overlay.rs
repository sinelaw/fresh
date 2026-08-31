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
//! list, so the precedence rules live in one place. The `impl Editor`
//! block at the bottom of this file is that source of truth: it builds
//! the layer stack from live editor state and answers the shared focus
//! queries (`popups_capture_keys`, `get_key_context`, …) every input
//! path consults.

use super::Editor;
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
    /// A native context menu — the tab right-click menu, the "+"
    /// new-tab popup, the file-explorer right-click menu, or the
    /// close-split confirmation (`Window::open_context_menu` resolves
    /// which). One kind for all four: they share the geometry core,
    /// are mutually exclusive, and get identical treatment — a modal
    /// chrome menu whose custom key dispatcher (the chrome
    /// `ContextMenu` component's `on_key`) makes it transparent to
    /// `KeyContext` resolution while it blocks PTY routing.
    ContextMenu,
    /// The centered widget modal (`floating_widget_panel`).
    FloatingModal,
    /// The editor-global left dock (`dock`).
    Dock,
    /// The editor content / window splits — the bottom layer.
    Editor,
}

/// One entry in the overlay stack: a present overlay (or the always-present
/// editor base), with the per-layer flags the dispatchers need.
#[derive(Debug, Clone, PartialEq)]
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

/// True iff a layer ranked *above* the popup layer currently owns the
/// keyboard. Used by the unfocused-popup key interception: the layers
/// ranked above `Popup` (840) are the capture-all modals (Settings /
/// KeybindingEditor / CalibrationWizard 880-900), WorkspaceTrust
/// (870), Menu (860) and Prompt (850), plus the hardcoded event-debug
/// head — the stack derives the set, so this list is documentation, not
/// an encoding. While one of those owns the keyboard the popup must not
/// intercept keys. Callers guarantee a `Popup` layer is present, so the
/// `take_while` stops before the editor base layer.
///
/// **Still load-bearing after the key walk went.** The interception is the
/// first rung of `dispatch_base_key` now, and base runs whenever the tree
/// did not claim — which includes a `Modality::Focus` surface that
/// *declined*. An open prompt still reports `owns_keyboard`, so this is
/// what keeps Esc in a prompt from reaching past it to a stale popup.
pub(crate) fn popup_blocked_by_higher_modal(layers: &[Layer]) -> bool {
    layers
        .iter()
        .take_while(|l| l.kind != LayerKind::Popup)
        .any(|l| l.owns_keyboard)
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
    /// and `dispatch_popup_keys` (handler routing) so the two cannot drift.
    /// True while the workspace-trust prompt is the TOP of the global
    /// popup stack — the state in which its dedicated mouse/key
    /// handlers (and its dedicated overlay layer) take over from the
    /// generic popup treatment.
    pub(crate) fn workspace_trust_on_top(&self) -> bool {
        self.global_popups.top().is_some_and(|p| {
            matches!(
                p.resolver,
                crate::view::popup::PopupResolver::WorkspaceTrust
            )
        })
    }

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
        let mut ranked: Vec<(u16, Layer)> = Vec::new();
        // Event-debug intercepts every key ahead of every other path
        // (see `handle_key_event`) — a debugging instrument with a
        // custom dispatcher, deliberately not a registered component.
        if self.active_window().is_event_debug_active() {
            ranked.push((
                1000,
                Layer {
                    kind: LayerKind::EventDebug,
                    owns_keyboard: true,
                    key_context: None,
                    blocks_terminal_input: true,
                },
            ));
        }
        for c in crate::app::chrome::components() {
            c.layers(self, &mut ranked);
        }
        // Stable sort: within a rank, declaration (registry) order is
        // preserved — the ordering this has always had.
        ranked.sort_by(|a, b| b.0.cmp(&a.0));
        ranked.into_iter().map(|(_, l)| l).collect()
    }

    /// True iff any overlay layer is currently blocking key routing to a
    /// terminal buffer's PTY child. The single source of truth for the
    /// "is anything modal up?" question.
    pub(crate) fn presents_blocking_overlay(&self) -> bool {
        crate::app::overlay::any_layer_blocks_terminal_input(&self.overlay_layers())
    }

    /// True iff a modal overlay — a prompt (Open File dialog, command
    /// palette, …), the menu, a full-screen modal (settings, keybinding
    /// editor, calibration wizard, workspace trust), a native context menu,
    /// or the centered widget modal — currently covers the editor content.
    ///
    /// Derived from the same [`Editor::overlay_layers`] stack the keyboard
    /// and mouse dispatchers consult, so there is a single notion of
    /// modality. The popup band is deliberately excluded: the hover and
    /// completion popups themselves live there, and mouse hover has its own
    /// popup-aware handling (`update_lsp_hover_state`). The dock and the
    /// editor base are not modal.
    ///
    /// Used to suppress mouse-hover LSP requests while a modal overlay is
    /// up: positions under the pointer map to the buffer *behind* the
    /// overlay, so a hover there would query — and render a popup for —
    /// content the user cannot see (sinelaw/fresh#2912).
    pub(crate) fn modal_overlay_active(&self) -> bool {
        use crate::app::overlay::LayerKind;
        self.overlay_layers().iter().any(|l| {
            !matches!(
                l.kind,
                LayerKind::Popup | LayerKind::Dock | LayerKind::Editor
            )
        })
    }

    /// Determine the current keybinding context based on UI state.
    ///
    /// Returns the `KeyContext` of the topmost overlay layer that owns the
    /// keyboard (see [`Editor::overlay_layers`]).
    pub fn get_key_context(&self) -> crate::input::keybindings::KeyContext {
        crate::app::overlay::resolve_focus_context(&self.overlay_layers())
            .expect("editor base layer always owns the keyboard")
    }
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
