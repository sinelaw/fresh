//! Property-based driver for the scenario runners.
//!
//! Uses proptest to generate `Vec<Action>` and feeds them into the
//! fallible `check_*` runners. Because the runners return
//! `Result<(), ScenarioFailure>` instead of panicking, proptest can
//! shrink failures cleanly: a failing generated case is reduced to a
//! minimal counterexample without any `catch_unwind` ceremony.
//!
//! The action alphabet here is a *safe subset* — actions that are
//! pure functions on (buffer text, cursors) without triggering modal
//! UI, async work, or filesystem I/O. The full `Action` enum has
//! ~600 variants and many of them open prompts, file dialogs, etc;
//! generating those at random would crash the harness for reasons
//! unrelated to the property under test.

use crate::common::harness::EditorTestHarness;
use fresh::test_api::{Action, Caret};
use proptest::prelude::*;

/// The canonical pure-state observable for a buffer scenario.
///
/// This is the type both the live editor and any [`ShadowModel`]
/// reduce to for differential comparison. Property tests evaluate
/// scenarios into this and assert invariants on it.
///
/// [`ShadowModel`]: crate::common::scenario::shadow::ShadowModel
#[derive(Debug, Clone, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
pub struct BufferState {
    pub buffer_text: String,
    pub primary: Caret,
    pub all_carets: Vec<Caret>,
    pub selection_text: String,
}

/// Run `actions` against a fresh headless harness seeded with
/// `initial_text` and return the resulting state. Never panics on
/// dispatch — runs the actions through `dispatch_seq`.
///
/// Harness construction failures (out of disk, etc.) still panic; an
/// external driver should already trust its environment.
pub fn evaluate_actions(initial_text: &str, actions: &[Action]) -> BufferState {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(initial_text)
        .expect("load_buffer_from_text failed");
    harness.api_mut().dispatch_seq(actions);
    observe(&mut harness)
}

/// Read the pure-state observable from a live harness.
pub fn observe(harness: &mut EditorTestHarness) -> BufferState {
    let api = harness.api_mut();
    BufferState {
        buffer_text: api.buffer_text(),
        primary: api.primary_caret(),
        all_carets: api.carets(),
        selection_text: api.selection_text(),
    }
}

/// Combination driver: run several `(initial_text, actions)` workloads
/// on ONE long-lived harness, applying `reset::reset_actions(initial)`
/// before each so the previous workload's text/cursor/selection is
/// reversed (an *active* reset, not a fresh harness). Returns the
/// observed state after each workload, in `order`.
///
/// Comparing these against the per-workload fresh-harness baseline is
/// what tests isolation: a mismatch means either the reset is
/// incomplete or a workload secretly depends on ambient state left by
/// a predecessor — neither of which a fresh-harness-per-test can
/// surface.
pub fn run_with_reset_between(
    workloads: &[(&str, Vec<Action>)],
    order: &[usize],
) -> Vec<BufferState> {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    harness
        .load_buffer_from_text("")
        .expect("seed empty buffer failed");
    let mut out = Vec::with_capacity(order.len());
    for &i in order {
        let (initial_text, actions) = &workloads[i];
        let mut seq = crate::common::scenario::reset::reset_actions(initial_text);
        seq.extend(actions.iter().cloned());
        harness.api_mut().dispatch_seq(&seq);
        out.push(observe(&mut harness));
    }
    out
}

/// Safe action subset. Any action listed here is expected to:
///   - operate purely on buffer text + cursor state,
///   - never open a prompt / popup / palette,
///   - never block on async work or I/O,
///   - never depend on filesystem state.
pub fn safe_action_strategy() -> impl Strategy<Value = Action> {
    prop_oneof![
        // Character input — restrict to ASCII printable so the
        // generator is reproducible across locales.
        (32u8..=126u8).prop_map(|b| Action::InsertChar(b as char)),
        Just(Action::InsertNewline),
        // Movement
        Just(Action::MoveLeft),
        Just(Action::MoveRight),
        Just(Action::MoveUp),
        Just(Action::MoveDown),
        Just(Action::MoveLineStart),
        Just(Action::MoveLineEnd),
        Just(Action::MoveDocumentStart),
        Just(Action::MoveDocumentEnd),
        // Selection
        Just(Action::SelectLeft),
        Just(Action::SelectRight),
        Just(Action::SelectUp),
        Just(Action::SelectDown),
        Just(Action::SelectLineStart),
        Just(Action::SelectLineEnd),
        Just(Action::SelectAll),
        // Deletion
        Just(Action::DeleteBackward),
        Just(Action::DeleteForward),
        // Case ops (transformations on selection)
        Just(Action::ToUpperCase),
    ]
}

/// Insert-only subset. Used by undo-identity properties where we want
/// `actions.len()` undos to perfectly restore the initial buffer.
pub fn insert_only_action_strategy() -> impl Strategy<Value = Action> {
    prop_oneof![
        (b'a'..=b'z').prop_map(|b| Action::InsertChar(b as char)),
        (b'0'..=b'9').prop_map(|b| Action::InsertChar(b as char)),
        Just(Action::InsertChar(' ')),
    ]
}

/// Reasonable starting buffer text — short ASCII strings.
pub fn initial_text_strategy() -> impl Strategy<Value = String> {
    prop::collection::vec(
        any::<u8>().prop_filter("printable ASCII or newline", |b| {
            (32..=126).contains(b) || *b == b'\n'
        }),
        0..40,
    )
    .prop_map(|bytes| String::from_utf8(bytes).unwrap())
}
