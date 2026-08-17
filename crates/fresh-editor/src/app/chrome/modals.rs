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

use super::{in_rect, layer_rank, ChromeComponent, ChromeTreeBuilder, Editor};

fn settings_up(ed: &Editor) -> bool {
    ed.settings_state.as_ref().is_some_and(|s| s.visible)
}

/// ONE activity predicate per modal, consulted by BOTH `capture_mouse`
/// and `layers()`. The R3 capture band walks the derived overlay stack,
/// so a component whose capture gate is true while its layer gate is
/// false is never offered the capture at all — pairing the two gates on
/// a single fn makes that impossible to drift instead of a convention.
fn kb_editor_up(ed: &Editor) -> bool {
    ed.keybinding_editor.is_some()
}

fn calibration_up(ed: &Editor) -> bool {
    ed.calibration_wizard.is_some()
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

    fn capture_mouse(
        &self,
        ed: &mut Editor,
        ev: crossterm::event::MouseEvent,
        _is_double_click: bool,
    ) -> Option<AnyhowResult<bool>> {
        if kb_editor_up(ed) {
            return Some(ed.handle_keybinding_editor_mouse(ev));
        }
        None
    }

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

    fn capture_mouse(
        &self,
        ed: &mut Editor,
        _ev: crossterm::event::MouseEvent,
        _is_double_click: bool,
    ) -> Option<AnyhowResult<bool>> {
        // The wizard owns the modal z-band but ignores every mouse
        // event (its UI is keyboard-driven). Swallowing matches the
        // previous explicit `return Ok(false)`.
        if calibration_up(ed) {
            return Some(Ok(false));
        }
        None
    }

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

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// Handle every mouse event while the workspace-trust modal is up. Left
    /// clicks act on its controls (radio rows select + confirm; [ OK ] confirms
    /// the current selection; the secondary button cancels or quits); the wheel
    /// scrolls an overflowing dialog. Everything else is absorbed so nothing
    /// reaches the buffer behind the modal.
    pub(super) fn handle_workspace_trust_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};
        let col = mouse_event.column;
        let row = mouse_event.row;
        let layout = self.active_chrome().workspace_trust_dialog.clone();

        match mouse_event.kind {
            MouseEventKind::ScrollUp => {
                self.workspace_trust_scroll = self.workspace_trust_scroll.saturating_sub(2);
            }
            MouseEventKind::ScrollDown => {
                let max = layout.as_ref().map(|l| l.max_scroll).unwrap_or(0);
                self.workspace_trust_scroll = (self.workspace_trust_scroll + 2).min(max);
            }
            MouseEventKind::Down(MouseButton::Left) => {
                if let Some(layout) = layout {
                    let hit = |r: ratatui::layout::Rect| in_rect(col, row, r);
                    if hit(layout.ok) {
                        let idx = self.current_workspace_trust_selection();
                        self.confirm_workspace_trust(idx);
                    } else if hit(layout.quit) {
                        // Secondary: Cancel (close) when voluntarily opened,
                        // Quit (exit the editor) for the mandatory open-time gate.
                        self.hide_popup();
                        if !self.workspace_trust_prompt_cancellable {
                            self.should_quit = true;
                        }
                    } else if let Some(i) = layout.radios.iter().position(|r| hit(*r)) {
                        // Selecting a radio is NOT consent. A click moves the
                        // selection and leaves the dialog up; [ OK ] commits
                        // it — the same two-step the keyboard already used
                        // (`T`/`K`/`B` select, Enter/`O` confirm). Accepting
                        // on click made "Trust folder & Allow Tooling" a
                        // one-click grant of full execution rights on a
                        // security prompt, with no chance to reconsider and
                        // no way to read the option before committing to it.
                        // The web UI forwards its radio clicks to this same
                        // hit-test, so both frontends inherit the fix.
                        self.set_workspace_trust_selection(i);
                    }
                    // else: click on the dialog body or dimmed backdrop — absorb.
                }
            }
            // Drag / move / release / right-click / horizontal scroll: absorb.
            _ => {}
        }
        Ok(true)
    }
}
