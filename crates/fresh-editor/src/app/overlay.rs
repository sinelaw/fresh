//! Unified overlay **layer** model (P2 — staged migration).
//!
//! The editor paints a stack of overlays on top of the editor content:
//! full-screen modals (settings, the keybinding editor, …), the menu, the
//! prompt, popups, the centered widget modal, and the left dock.
//! Historically each was an independent field with its own
//! focus-precedence, paint-order and mouse-routing logic scattered across
//! `render.rs`, `input.rs` and `mouse_input.rs`. The eventual destination
//! (see `docs/internal/orchestrator-dock-gaps.md`, "Design: dock + modal
//! coexistence", option P2) is to make this stack a first-class *ordered
//! list of layers* so precedence, z-order and hit-testing are *properties
//! of layers* rather than duplicated conditionals.
//!
//! This module is the seed of that model. **Step 1** makes the layer stack
//! the single source of truth for keyboard-focus precedence: `get_key_context`
//! now builds the ordered layer list (`Editor::overlay_layers`) and returns
//! the `KeyContext` of the topmost layer that owns the keyboard, instead of a
//! hand-written if/else ladder. Render and mouse dispatch are migrated onto
//! the same list in later steps.

use crate::input::keybindings::KeyContext;

/// Where a layer is anchored on screen. Determines its hit-testing region
/// and, for modal layers, what is dimmed underneath. Not all consumers use
/// this yet (step 1 only resolves keyboard focus); it is carried on every
/// layer so the render/mouse migrations can switch onto it without
/// reshaping the model.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LayerRegion {
    /// Covers the whole frame — really the *chrome area* beside the dock,
    /// which is the full frame when no dock is present. Full-screen modals
    /// (settings, keybinding editor) and the menu/prompt live here.
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
    /// layer below it (settings, menu, prompt, a focused centered modal).
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
/// focus and (later) paint/hit-test decisions.
#[derive(Debug, Clone)]
pub(crate) struct Layer {
    // `kind` / `region` / `policy` describe the layer for the paint-order
    // and mouse-hit-test migrations (later steps of P2); step 1 resolves
    // only keyboard focus, which reads `owns_keyboard` + `key_context`.
    #[allow(dead_code)]
    pub kind: LayerKind,
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
    /// keyboard owner.
    pub key_context: KeyContext,
}

/// Resolve the keyboard-owning context from an ordered (top-first) layer
/// list: the first layer that owns the keyboard wins. The editor base
/// layer always owns the keyboard, so this never returns `None` for a
/// well-formed stack.
pub(crate) fn resolve_focus_context(layers: &[Layer]) -> Option<KeyContext> {
    layers
        .iter()
        .find(|l| l.owns_keyboard)
        .map(|l| l.key_context.clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn layer(kind: LayerKind, owns: bool, ctx: KeyContext) -> Layer {
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
        }
    }

    fn base() -> Layer {
        Layer {
            kind: LayerKind::Editor,
            region: LayerRegion::EditorContent,
            policy: FocusPolicy::Base,
            owns_keyboard: true,
            key_context: KeyContext::Normal,
        }
    }

    #[test]
    fn topmost_owning_layer_wins() {
        let layers = [
            layer(LayerKind::Settings, false, KeyContext::Settings),
            layer(LayerKind::Popup, true, KeyContext::Popup),
            layer(LayerKind::Dock, true, KeyContext::Dock),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Popup));
    }

    #[test]
    fn falls_through_unfocused_layers_to_base() {
        let layers = [
            layer(LayerKind::FloatingModal, false, KeyContext::Normal),
            layer(LayerKind::Dock, false, KeyContext::Dock),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Normal));
    }

    #[test]
    fn base_layer_terminates_the_walk() {
        let layers = [base()];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Normal));
    }
}
