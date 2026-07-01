//! `ModalScenario` — palettes, pickers, prompts, menus.
//!
//! Asserts on a [`ModalState`] observable extracted from the
//! editor's `PopupManager`. Phase 3 handles the cases where the
//! popup state is reachable via existing actions
//! (e.g. `Action::ShowCommandPalette`) plus the
//! `OpenPrompt`/`FilterPrompt`/`ConfirmPrompt`/`CancelPrompt`
//! `InputEvent` variants, which the runner translates to the
//! corresponding `Action`s.
//!
//! Today's runner is **partial**: most modal flows in production
//! drive prompts through key-routed handlers that don't have
//! direct `Action` equivalents. The skeleton here pins down the
//! data shape (so corpus scenarios serialise consistently) and the
//! observable extraction; expanding the runner to cover every
//! popup kind happens incrementally as ModalScenarios for those
//! kinds get added.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::context::PromptKind;
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::{ModalState, Observable, PopupSnapshot};
use fresh::test_api::Action;

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct ModalScenario {
    pub description: String,
    pub initial_text: String,
    pub events: Vec<InputEvent>,
    /// What the modal stack should look like at t=∞.
    pub expected_modal: ModalState,
}

pub fn check_modal_scenario(s: ModalScenario) -> Result<(), ScenarioFailure> {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(&s.initial_text)
        .expect("load_buffer_from_text failed");

    for ev in &s.events {
        dispatch_input_event(&mut harness, ev)?;
    }

    let actual = ModalState::extract(&mut harness);
    if actual != s.expected_modal {
        return Err(ScenarioFailure::ModalStateMismatch {
            description: s.description,
            expected: format!("{:?}", s.expected_modal),
            actual: format!("{actual:?}"),
        });
    }
    Ok(())
}

pub fn assert_modal_scenario(s: ModalScenario) {
    if let Err(f) = check_modal_scenario(s) {
        panic!("{f}");
    }
}

/// Translate a high-level `InputEvent` into the editor's input
/// alphabet for the prompt subset. CancelPrompt / ConfirmPrompt
/// route through `send_key` (Esc / Enter) so the production key
/// handler — which knows about the active prompt — actually
/// closes / commits it. Dispatching `Action::PromptCancel`
/// directly is a no-op outside the file-open prompt's local
/// handler, so we use the key path instead.
fn dispatch_input_event(
    harness: &mut EditorTestHarness,
    ev: &InputEvent,
) -> Result<(), ScenarioFailure> {
    use crossterm::event::{KeyCode, KeyModifiers};
    match ev {
        InputEvent::Action(a) => {
            harness.api_mut().dispatch(a.clone());
            Ok(())
        }
        InputEvent::OpenPrompt(kind) => {
            let action = match kind {
                PromptKind::CommandPalette => Action::CommandPalette,
                PromptKind::FileOpen => Action::QuickOpen,
                PromptKind::Goto => Action::GotoLine,
                PromptKind::LiveGrep => Action::OpenLiveGrep,
                _ => {
                    return Err(ScenarioFailure::InputProjectionFailed {
                        description: String::new(),
                        reason: format!(
                            "ModalScenario phase: OpenPrompt({kind:?}) has no direct Action mapping yet",
                        ),
                    });
                }
            };
            harness.api_mut().dispatch(action);
            Ok(())
        }
        InputEvent::CancelPrompt => harness
            .send_key(KeyCode::Esc, KeyModifiers::NONE)
            .map(|_| ())
            .map_err(|e| ScenarioFailure::InputProjectionFailed {
                description: String::new(),
                reason: format!("CancelPrompt send_key(Esc): {e}"),
            }),
        InputEvent::ConfirmPrompt => harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .map(|_| ())
            .map_err(|e| ScenarioFailure::InputProjectionFailed {
                description: String::new(),
                reason: format!("ConfirmPrompt send_key(Enter): {e}"),
            }),
        InputEvent::FilterPrompt(text) => {
            // Type each char into the prompt. The editor's input
            // handler routes Action::InsertChar to the active
            // prompt when one is open.
            for c in text.chars() {
                harness.api_mut().dispatch(Action::InsertChar(c));
            }
            Ok(())
        }
        InputEvent::MenuSelect(_) => Err(ScenarioFailure::InputProjectionFailed {
            description: String::new(),
            reason: "ModalScenario phase: MenuSelect needs `popup.select(idx)` accessor".into(),
        }),
        other => Err(ScenarioFailure::InputProjectionFailed {
            description: String::new(),
            reason: format!("ModalScenario does not handle {other:?} — wrong scenario type"),
        }),
    }
}

/// Convenience constructor for the common shape: open a prompt,
/// confirm, assert depth.
pub fn modal_open_then_confirm(
    description: &str,
    kind: PromptKind,
    expected_depth_after: usize,
) -> ModalScenario {
    ModalScenario {
        description: description.into(),
        initial_text: String::new(),
        events: vec![InputEvent::OpenPrompt(kind), InputEvent::ConfirmPrompt],
        expected_modal: ModalState {
            top_popup: None,
            depth: expected_depth_after,
            prompt: None,
        },
    }
}

#[allow(dead_code)]
pub fn popup(kind: &str) -> PopupSnapshot {
    PopupSnapshot {
        kind: kind.into(),
        title: None,
        items: Vec::new(),
        selected_index: None,
        query: None,
    }
}
