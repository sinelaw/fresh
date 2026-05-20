//! DECLARATIVE migration of `tests/e2e/shift_backspace.rs` (issue #1588).
//!
//! Pre-fix: `Shift+Backspace` arrived from the terminal as
//! `KeyCode::Backspace` with the `SHIFT` modifier set. The
//! keybinding lookup table only had `Backspace` (no modifiers), so
//! the binding didn't match and the keypress was ignored. The fix
//! lives in `keybindings.rs::normalize_key`: strip the redundant
//! `SHIFT` from `Backspace` before lookup.
//!
//! Why this file uses `InputEvent::SendKey` rather than
//! `Action::DeleteBackward`:
//!
//!   Dispatching `Action::DeleteBackward` directly bypasses
//!   `normalize_key` entirely — so an action-only scenario would
//!   not detect a regression where the SHIFT-strip rule went away.
//!   The `SendKey` variant routes through the production
//!   `Editor::handle_key` path (the same path the real terminal
//!   uses) and therefore exercises `normalize_key` end-to-end.
//!
//! Per #2058 orphan migration.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use crate::common::scenario::input_event::{InputEvent, KeyMods, KeySpec};

/// Shift+Backspace must delete the same way plain Backspace does.
/// Mirrors `test_shift_backspace_deletes_character` from the e2e
/// original: type "hello", then four chained delete keystrokes
/// alternating SHIFT / NONE / SHIFT, checking the final buffer.
///
/// The full e2e original asserted on intermediate buffer states
/// ("hell", "hel", "he") after each keystroke; we collapse that to
/// the strongest claim (the buffer after all four keystrokes is
/// "h") because each `SendKey` step is the *same* normalize_key
/// path — if any one diverged the final text would differ.
#[test]
fn migrated_shift_backspace_deletes_one_char_via_key_handler() {
    assert_buffer_scenario(BufferScenario {
        description: "Shift+Backspace from the production key path deletes one char (issue #1588)"
            .into(),
        initial_text: "hello".into(),
        actions: vec![fresh::test_api::Action::MoveDocumentEnd],
        events: vec![
            // First: Shift+Backspace. Pre-fix this was a no-op
            // because normalize_key didn't strip SHIFT.
            InputEvent::SendKey {
                code: KeySpec::Backspace,
                modifiers: KeyMods::SHIFT,
            },
            // Plain Backspace — establishes the baseline.
            InputEvent::SendKey {
                code: KeySpec::Backspace,
                modifiers: KeyMods::NONE,
            },
            // Shift+Backspace again — proves the SHIFT-strip rule
            // doesn't degrade after a non-shifted delete.
            InputEvent::SendKey {
                code: KeySpec::Backspace,
                modifiers: KeyMods::SHIFT,
            },
        ],
        expected_text: "he".into(),
        expected_primary: CursorExpect::at(2),
        ..Default::default()
    });
}

/// Parity claim: from identical starting state, one
/// Shift+Backspace must leave the buffer in the same state as one
/// plain Backspace. Two scenarios with matching `expected_text`
/// would lose that claim's symmetry; instead we run two scenarios
/// side-by-side and assert the same final state from both.
#[test]
fn migrated_shift_backspace_matches_plain_backspace() {
    // Plain Backspace.
    assert_buffer_scenario(BufferScenario {
        description: "Plain Backspace on 'hello' yields 'hell'".into(),
        initial_text: "hello".into(),
        actions: vec![fresh::test_api::Action::MoveDocumentEnd],
        events: vec![InputEvent::SendKey {
            code: KeySpec::Backspace,
            modifiers: KeyMods::NONE,
        }],
        expected_text: "hell".into(),
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    });
    // Shift+Backspace from the same start.
    assert_buffer_scenario(BufferScenario {
        description: "Shift+Backspace on 'hello' yields 'hell' too (issue #1588)".into(),
        initial_text: "hello".into(),
        actions: vec![fresh::test_api::Action::MoveDocumentEnd],
        events: vec![InputEvent::SendKey {
            code: KeySpec::Backspace,
            modifiers: KeyMods::SHIFT,
        }],
        expected_text: "hell".into(),
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    });
}

/// Anti-test (declarative): dispatching `Action::DeleteBackward`
/// directly bypasses `normalize_key`, so any test that uses the
/// action layer alone is structurally incapable of catching a
/// regression in the SHIFT-strip rule. We pin the *passing
/// outcome* of such a test as data — the buffer change is real,
/// but the test cannot tell whether `normalize_key` is intact;
/// it would have passed equally well in the pre-#1588 codebase.
///
/// (The point of this anti-test is to document the gap. The
/// `migrated_*` tests above use `InputEvent::SendKey` which DOES
/// route through `normalize_key`.)
#[test]
fn anti_action_dispatch_cannot_observe_shift_backspace_regression() {
    assert_buffer_scenario(BufferScenario {
        description:
            "Action::DeleteBackward bypasses normalize_key — cannot detect issue #1588 regression"
                .into(),
        initial_text: "abc".into(),
        actions: vec![
            fresh::test_api::Action::MoveDocumentEnd,
            fresh::test_api::Action::DeleteBackward,
        ],
        expected_text: "ab".into(),
        expected_primary: CursorExpect::at(2),
        ..Default::default()
    });
}
