//! The full-screen modal band: Settings, the keybinding editor, the
//! calibration wizard, and the workspace-trust prompt.
//!
//! **Their mouse has moved.** Each owned the whole channel through
//! `ChromeComponent::capture_mouse`, a band ahead of every walk; a modal is a
//! `Modality::Exclusive` layer in the shell's tree now, and the trust prompt's
//! controls are nodes outright. What is left here is the keyboard — a
//! capture-all with a bespoke dispatcher apiece — and the rank entry each
//! contributes while that walk is still the chrome's.
//!
//! Their INTERIORS stay bespoke (Settings is its own later phase): the
//! component is the dispatch slot, so replacing an interior never touches
//! dispatch again. They contribute no boxes, and now cannot: an exclusive
//! layer claims every event in the tree's own walk, so a box here would be
//! dead in every reachable state.

use anyhow::Result as AnyhowResult;

use crate::app::overlay::{Layer, LayerKind};
use crate::input::keybindings::KeyContext;

use super::{layer_rank, ChromeComponent, ChromeTreeBuilder, Editor};

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
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

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

    fn on_layer_key(
        &self,
        ed: &mut Editor,
        _layer: &Layer,
        event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<crate::input::handler::InputResult>> {
        use crate::input::handler::{InputContext, InputHandler};
        // Capture-all: every key is this modal's while its layer is up.
        let mut ctx = InputContext::new();
        let result = {
            let settings = ed
                .settings_state
                .as_mut()
                .expect("Settings layer implies settings_state present");
            settings.dispatch_input(event, &mut ctx)
        };
        ed.process_deferred_actions(ctx);
        Some(Ok(result))
    }
}

pub(crate) struct KeybindingEditor;

impl ChromeComponent for KeybindingEditor {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

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

    fn on_layer_key(
        &self,
        ed: &mut Editor,
        _layer: &Layer,
        event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<crate::input::handler::InputResult>> {
        // Capture-all with its own bespoke dispatcher.
        Some(Ok(ed.handle_keybinding_editor_input(event)))
    }
}

pub(crate) struct CalibrationWizard;

impl ChromeComponent for CalibrationWizard {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

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

    fn on_layer_key(
        &self,
        ed: &mut Editor,
        _layer: &Layer,
        event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<crate::input::handler::InputResult>> {
        // Capture-all with its own bespoke dispatcher.
        Some(Ok(ed.handle_calibration_input(event)))
    }
}

/// The prompt's pointer is the tree's — layer, scrim, modality, radios and
/// buttons all in `view::shell::trust`. What is left here is its keyboard and
/// the rank entry that stands in for the layer while the keyboard walk is
/// still the chrome's.
pub(crate) struct WorkspaceTrust;

impl ChromeComponent for WorkspaceTrust {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

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

    fn on_layer_key(
        &self,
        ed: &mut Editor,
        _layer: &Layer,
        event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<crate::input::handler::InputResult>> {
        // Same gate the old popup block put around its trust rung: the
        // prompt owns keys only while it captures (focused, or the
        // editor-wide startup gate). `handle_workspace_trust_key`
        // returns `Some(Consumed)` for every key — the modal swallows
        // everything — so nothing falls past it to a generic popup
        // treatment (the layer REPLACES the Popup layer while the
        // trust prompt tops the global stack).
        if !ed.popups_capture_keys() {
            return None;
        }
        ed.handle_workspace_trust_key(event).map(Ok)
    }
}
