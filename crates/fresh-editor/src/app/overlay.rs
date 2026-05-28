//! Unified overlay **layer** model (P2 — staged migration).
//!
//! The editor paints a stack of overlays on top of the editor content:
//! full-screen modals (settings, the keybinding editor, the calibration
//! wizard, …), the menu, the prompt, popups, the centered widget modal,
//! and the left dock. Historically each was an independent field with its
//! own focus-precedence, paint-order and mouse-routing logic scattered
//! across `render.rs`, `input.rs`, `input_dispatch.rs` and
//! `mouse_input.rs`. The eventual destination
//! (see `docs/internal/orchestrator-dock-gaps.md`, "Design: dock + modal
//! coexistence", option P2) is to make this stack a first-class *ordered
//! list of layers* so precedence, z-order and hit-testing are *properties
//! of layers* rather than duplicated conditionals.
//!
//! Status of the staged migration:
//! - Step 1: `get_key_context` resolved through the stack.
//! - Step 2: the duplicate "higher-priority modal owns the keyboard" guard
//!   in `resolve_unfocused_popup_action` is a stack query.
//! - Step 3 (this revision): the stack includes the full-screen modal zoo
//!   (calibration / keybinding editor) and tracks `blocks_terminal_input`
//!   per layer, letting `dispatch_terminal_input`'s `in_modal` predicate
//!   become a single stack query.
//! - Later: render paint-order and mouse hit-testing.

use crate::input::keybindings::KeyContext;

/// Where a layer is anchored on screen. Determines its hit-testing region
/// and, for modal layers, what is dimmed underneath. Step 1–3 only resolve
/// focus and terminal-blocking; it is carried on every layer so the
/// render/mouse migrations can switch onto it without reshaping the model.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LayerRegion {
    /// Covers the whole frame — really the *chrome area* beside the dock,
    /// which is the full frame when no dock is present. Full-screen modals
    /// (settings, calibration wizard, keybinding editor) and the menu/prompt
    /// live here.
    FullScreen,
    /// A centered modal box sized by a percentage of the frame (the
    /// orchestrator picker / new-session form / plugin modals).
    Centered,
    /// A full-height column pinned to the left of the chrome (the dock).
    LeftDock,
    /// Anchored near the cursor or a focused widget (popups, completions).
    Anchored,
    /// The editor content / window splits — the bottom-most layer.
    EditorContent,
}

/// How a layer participates in keyboard focus.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FocusPolicy {
    /// Owns the keyboard whenever the layer is present, blocking every
    /// layer below it (settings, calibration wizard, keybinding editor,
    /// menu, prompt, a focused centered modal).
    Modal,
    /// Focusable, but only owns the keyboard while *focused*; when blurred
    /// it coexists with the editor underneath (the dock; an unfocused
    /// popup that is merely visible).
    NonModal,
    /// Never the keyboard target — the bottom editor-content layer, which
    /// is the terminal's default key sink.
    Base,
}

/// Identifies a concrete overlay. The ordering of `overlay_layers` — not
/// this enum's declaration order — defines precedence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LayerKind {
    Settings,
    /// The keybinding editor (`keybinding_editor`) — a full-screen modal
    /// with its own input dispatcher; transparent to `KeyContext`-driven
    /// keybinding resolution.
    KeybindingEditor,
    /// The calibration wizard (`calibration_wizard`) — same as above.
    CalibrationWizard,
    Menu,
    Prompt,
    Popup,
    /// The centered widget modal (`floating_widget_panel`).
    FloatingModal,
    /// The editor-global left dock (`dock`).
    Dock,
    /// The editor content / window splits.
    Editor,
}

/// One entry in the overlay stack: a present overlay (or the always-present
/// editor base), with the data the dispatchers need to make precedence,
/// focus, terminal-blocking and (later) paint/hit-test decisions.
#[derive(Debug, Clone)]
pub(crate) struct Layer {
    pub kind: LayerKind,
    // `region` / `policy` describe the layer for the paint-order and
    // mouse-hit-test migrations (later steps of P2). Reading is via
    // `kind` / `owns_keyboard` / `key_context` / `blocks_terminal_input`.
    #[allow(dead_code)]
    pub region: LayerRegion,
    #[allow(dead_code)]
    pub policy: FocusPolicy,
    /// Whether this layer currently owns the keyboard. For `Modal` layers
    /// this is always true while present; for `NonModal` it tracks the
    /// layer's focused/capturing state; the `Base` layer sets it true so a
    /// top-down walk always terminates.
    pub owns_keyboard: bool,
    /// The keybinding context to resolve against when this layer is the
    /// keyboard owner. `None` for layers whose keys are intercepted by a
    /// custom dispatcher (calibration wizard, keybinding editor) and never
    /// reach `KeyContext`-driven resolution — they are *transparent* to
    /// `resolve_focus_context`, which keeps walking below them.
    pub key_context: Option<KeyContext>,
    /// Whether this layer, while present, blocks routing of keys to the
    /// PTY child of a terminal buffer underneath. A blurred dock leaves the
    /// terminal usable; a merely-visible popup does not (it covers the
    /// active buffer and the user's keystrokes belong to the popup).
    pub blocks_terminal_input: bool,
}

/// Resolve the keyboard-owning `KeyContext` from an ordered (top-first)
/// layer list: the first owning layer that *has* a `KeyContext` wins.
/// Layers without a `KeyContext` (custom-dispatch modals) are skipped —
/// not because they don't own the keyboard but because they don't
/// participate in `KeyContext`-driven keybinding resolution; their input
/// dispatcher has already intercepted the keys upstream.
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
/// child of a terminal buffer underneath. Used by `dispatch_terminal_input`
/// to decide whether to drop into PTY-passthrough mode.
pub(crate) fn any_layer_blocks_terminal_input(layers: &[Layer]) -> bool {
    layers.iter().any(|l| l.blocks_terminal_input)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn modal(kind: LayerKind, owns: bool, ctx: Option<KeyContext>, blocks: bool) -> Layer {
        Layer {
            kind,
            region: LayerRegion::FullScreen,
            policy: if owns {
                FocusPolicy::Modal
            } else {
                FocusPolicy::NonModal
            },
            owns_keyboard: owns,
            key_context: ctx,
            blocks_terminal_input: blocks,
        }
    }

    fn base() -> Layer {
        Layer {
            kind: LayerKind::Editor,
            region: LayerRegion::EditorContent,
            policy: FocusPolicy::Base,
            owns_keyboard: true,
            key_context: Some(KeyContext::Normal),
            blocks_terminal_input: false,
        }
    }

    #[test]
    fn topmost_owning_layer_wins() {
        let layers = [
            modal(
                LayerKind::Settings,
                false,
                Some(KeyContext::Settings),
                false,
            ),
            modal(LayerKind::Popup, true, Some(KeyContext::Popup), true),
            modal(LayerKind::Dock, true, Some(KeyContext::Dock), true),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Popup));
    }

    #[test]
    fn falls_through_unfocused_layers_to_base() {
        let layers = [
            modal(
                LayerKind::FloatingModal,
                false,
                Some(KeyContext::Normal),
                true,
            ),
            modal(LayerKind::Dock, false, Some(KeyContext::Dock), false),
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

    /// Calibration / keybinding-editor own the keyboard via custom
    /// dispatch — they have no `KeyContext`. `resolve_focus_context`
    /// must walk past them and return the base context, matching the
    /// historical behavior when `get_key_context` was queried while one
    /// of those modals happened to be up.
    #[test]
    fn keycontext_walk_is_transparent_to_custom_dispatch_modals() {
        let layers = [
            modal(LayerKind::CalibrationWizard, true, None, true),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Normal));
        // …but the custom-dispatch modal still blocks terminal routing:
        assert!(any_layer_blocks_terminal_input(&layers));
    }

    /// A merely-visible (unfocused) popup blocks PTY routing — the
    /// popup is painted over the active buffer. A blurred dock does
    /// not block; a focused dock does.
    #[test]
    fn terminal_blocking_differs_from_keyboard_ownership() {
        let popup_visible_not_capturing = [
            modal(LayerKind::Popup, false, Some(KeyContext::Popup), true),
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
            modal(LayerKind::Dock, false, Some(KeyContext::Dock), false),
            base(),
        ];
        assert!(!any_layer_blocks_terminal_input(&blurred_dock));
    }
}
