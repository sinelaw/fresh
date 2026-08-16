//! The full-screen modal band: Settings, the keybinding editor, the
//! calibration wizard, and the workspace-trust prompt. Each owns the
//! whole mouse channel while up ([`ChromeComponent::capture_mouse`]);
//! their INTERIORS stay bespoke (Settings is its own later phase) —
//! the component is the dispatch slot, so replacing an interior never
//! touches dispatch again. They contribute no boxes yet: capture
//! preempts every gesture walk, so band geometry would be dead until
//! the modal handlers decompose onto the walks (grab slot + opacity
//! gate — this slice's recorded residue).

use anyhow::Result as AnyhowResult;

use crate::app::overlay::{Layer, LayerKind};
use crate::input::keybindings::KeyContext;

use super::{layer_rank, ChromeComponent, ChromeTreeBuilder, Editor};

fn settings_up(ed: &Editor) -> bool {
    ed.settings_state.as_ref().is_some_and(|s| s.visible)
}

pub(crate) struct Settings;

impl ChromeComponent for Settings {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

    fn capture_mouse(
        &self,
        ed: &mut Editor,
        ev: crossterm::event::MouseEvent,
        is_double_click: bool,
    ) -> Option<AnyhowResult<bool>> {
        if settings_up(ed) {
            return Some(ed.handle_settings_mouse(ev, is_double_click));
        }
        None
    }

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

pub(crate) struct KeybindingEditor;

impl ChromeComponent for KeybindingEditor {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

    fn capture_mouse(
        &self,
        ed: &mut Editor,
        ev: crossterm::event::MouseEvent,
        _is_double_click: bool,
    ) -> Option<AnyhowResult<bool>> {
        if ed.keybinding_editor.is_some() {
            return Some(ed.handle_keybinding_editor_mouse(ev));
        }
        None
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, Layer)>) {
        // Installs its own input dispatcher, so it is transparent to
        // `KeyContext`-driven resolution (`key_context: None`) but
        // fully owns the keyboard while present and blocks PTY routing.
        if ed.keybinding_editor.is_some() {
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
}

pub(crate) struct CalibrationWizard;

impl ChromeComponent for CalibrationWizard {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

    fn capture_mouse(
        &self,
        ed: &mut Editor,
        _ev: crossterm::event::MouseEvent,
        _is_double_click: bool,
    ) -> Option<AnyhowResult<bool>> {
        // The wizard owns the modal z-band but ignores every mouse
        // event (its UI is keyboard-driven). Swallowing matches the
        // previous explicit `return Ok(false)`.
        if ed.calibration_wizard.is_some() {
            return Some(Ok(false));
        }
        None
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, Layer)>) {
        // Same custom-dispatcher treatment as the keybinding editor.
        if ed.calibration_wizard.is_some() {
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
}

pub(crate) struct WorkspaceTrust;

impl ChromeComponent for WorkspaceTrust {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

    fn capture_mouse(
        &self,
        ed: &mut Editor,
        ev: crossterm::event::MouseEvent,
        _is_double_click: bool,
    ) -> Option<AnyhowResult<bool>> {
        // Capturing only while the trust prompt is the TOP of the
        // global popup stack — beneath another popup its dedicated
        // handlers must not swallow events aimed at the one above.
        if ed.workspace_trust_on_top() {
            return Some(ed.handle_workspace_trust_mouse(ev));
        }
        None
    }

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
}
