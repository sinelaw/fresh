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

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Settings;

impl ChromeComponent for Settings {
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

    fn capture_mouse(
        &self,
        ed: &mut Editor,
        ev: crossterm::event::MouseEvent,
        is_double_click: bool,
    ) -> Option<AnyhowResult<bool>> {
        if ed.settings_state.as_ref().is_some_and(|s| s.visible) {
            return Some(ed.handle_settings_mouse(ev, is_double_click));
        }
        None
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
        // global popup stack, mirroring its `overlay_layers()` entry —
        // beneath another popup its dedicated handlers must not
        // swallow events aimed at the one above.
        let trust_on_top = ed.global_popups.top().is_some_and(|p| {
            matches!(
                p.resolver,
                crate::view::popup::PopupResolver::WorkspaceTrust
            )
        });
        if trust_on_top {
            return Some(ed.handle_workspace_trust_mouse(ev));
        }
        None
    }
}
