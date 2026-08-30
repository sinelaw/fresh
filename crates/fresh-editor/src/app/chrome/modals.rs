//! The full-screen modal band: Settings, the keybinding editor, the
//! calibration wizard, and the workspace-trust prompt.
//!
//! **Their mouse and their keyboard have both moved.** Each owned the whole
//! pointer channel through `ChromeComponent::capture_mouse`, a band ahead of
//! every walk, and the whole keyboard through `on_layer_key`, a capture-all
//! offered in `layer_rank` order. A modal is a `Modality::Exclusive` layer in
//! the shell's tree now: it owns both channels by *containment*, and the
//! facts it produces (`ModalPointer`, `ModalKey`) name the surface rather
//! than a rank.
//!
//! Their INTERIORS stay bespoke, which is the ruling that let either channel
//! cross: the tree answers which surface an event belongs to, the surface
//! answers what it means. Three of the four dispatchers moved from here to
//! `Editor` methods with one caller each; the trust prompt's is still here
//! because its layer is a popup's rather than a modal's.
//!
//! What is left is the rank entry each contributes — read now by the PTY gate
//! and `get_key_context`, not by any key walk — and they contribute no boxes,
//! and now cannot: an exclusive layer claims every event in the tree's own
//! walk, so a box here would be dead in every reachable state.

use crate::app::overlay::{Layer, LayerKind};
use crate::input::keybindings::KeyContext;

use super::{layer_rank, ChromeComponent, Editor};

fn settings_up(ed: &Editor) -> bool {
    ed.settings_state.as_ref().is_some_and(|s| s.visible)
}

/// ONE activity predicate per modal, consulted by `layers()` here and by
/// `Editor::modal_slot`, which decides whose layer claims the pointer. Pairing
/// the two on a single fn is what kept a modal's capture gate and its layer
/// gate from drifting apart while they were separate.
fn kb_editor_up(ed: &Editor) -> bool {
    ed.keybinding_editor.is_some()
}

fn calibration_up(ed: &Editor) -> bool {
    ed.calibration_wizard.is_some()
}

pub(crate) struct Settings;

impl ChromeComponent for Settings {
    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, Layer)>) {
        // Full-screen modal: owns the keyboard whenever present.
        if settings_up(ed) {
            out.push((
                layer_rank::SETTINGS,
                Layer {
                    kind: LayerKind::Settings,
                    owns_keyboard: true,
                    key_context: Some(KeyContext::Settings),
                    blocks_terminal_input: true,
                },
            ));
        }
    }
}

impl Editor {
    /// Settings' own keyboard, reached by containment rather than by rank.
    ///
    /// Capture-all: every key is this modal's while its layer is up, which is
    /// what `Modality::Exclusive` says and what the `on_layer_key` this
    /// replaces said by returning `Some` unconditionally. The interior is
    /// eleven modules of `InputHandler` and stays exactly where it is.
    pub(crate) fn dispatch_settings_key(&mut self, event: &crossterm::event::KeyEvent) {
        use crate::input::handler::{InputContext, InputHandler};
        let mut ctx = InputContext::new();
        if let Some(settings) = self.settings_state.as_mut() {
            settings.dispatch_input(event, &mut ctx);
        }
        self.process_deferred_actions(ctx);
    }
}

pub(crate) struct KeybindingEditor;

impl ChromeComponent for KeybindingEditor {
    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, Layer)>) {
        // Installs its own input dispatcher, so it is transparent to
        // `KeyContext`-driven resolution (`key_context: None`) but
        // fully owns the keyboard while present and blocks PTY routing.
        if kb_editor_up(ed) {
            out.push((
                layer_rank::KEYBINDING_EDITOR,
                Layer {
                    kind: LayerKind::KeybindingEditor,
                    owns_keyboard: true,
                    key_context: None,
                    blocks_terminal_input: true,
                },
            ));
        }
    }

    // The keyboard is `KeySlot::KeybindingEditor`'s: the layer it declares in
    // `view::shell::keybinding` owns it, and `handle_keybinding_editor_input`
    // is reached by containment rather than by this rank.
}

pub(crate) struct CalibrationWizard;

impl ChromeComponent for CalibrationWizard {
    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, Layer)>) {
        // Same custom-dispatcher treatment as the keybinding editor.
        if calibration_up(ed) {
            out.push((
                layer_rank::CALIBRATION_WIZARD,
                Layer {
                    kind: LayerKind::CalibrationWizard,
                    owns_keyboard: true,
                    key_context: None,
                    blocks_terminal_input: true,
                },
            ));
        }
    }

    // The same, for `KeySlot::Calibration`. Its interior is a *key capture*
    // wizard — the raw key is the point — so it stays a dispatcher on a
    // crossterm event and the tree only says whose key it is.
}

/// The prompt's pointer and keyboard are both the tree's — layer, scrim,
/// modality, radios, buttons and the key claim all in `view::shell::trust`.
/// What is left here is the rank entry, which the PTY gate and
/// `get_key_context` still read.
pub(crate) struct WorkspaceTrust;

impl ChromeComponent for WorkspaceTrust {
    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, Layer)>) {
        // When it's the top of the global stack it takes the place of
        // the generic `Popup` layer (the popups component skips its
        // own entry) so the dedicated handlers can be reached by
        // top-down kind dispatch.
        if ed.workspace_trust_on_top() {
            out.push((
                layer_rank::WORKSPACE_TRUST,
                Layer {
                    kind: LayerKind::WorkspaceTrust,
                    owns_keyboard: ed.popups_capture_keys(),
                    key_context: Some(KeyContext::Popup),
                    blocks_terminal_input: true,
                },
            ));
        }
    }

    // The keyboard is `KeySlot::WorkspaceTrust`'s. The gate this used to apply
    // after the key arrived — the prompt owns keys only while it captures — is
    // `Trust::captures`, read once when the layer is described: an exclusive
    // layer with nobody listening inside it would stop a key rather than route
    // it, so whether to claim has to be decided where the claim is made.
}
