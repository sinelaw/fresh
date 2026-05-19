//! Migration of `tests/e2e/shift_backspace.rs` (issue #1588).
//!
//! Pre-fix: Shift+Backspace arrived from the terminal as
//! `KeyCode::Backspace` with the `SHIFT` modifier set. The
//! keybinding lookup table only had `Backspace` (no
//! modifiers), so the binding didn't match and the event was
//! ignored. The fix lives in `keybindings.rs::normalize_key`:
//! strip the redundant `SHIFT` from `Backspace` before lookup.
//!
//! The earlier migration in `migrated_block_selection.rs`
//! dispatched `Action::DeleteBackward` directly, entirely
//! bypassing `normalize_key` — so it could not detect a
//! regression that removed the SHIFT-strip rule. This
//! migration drives `harness.send_key(Backspace, SHIFT)`, which
//! routes through the production key handler and exercises
//! `normalize_key` end-to-end.
//!
//! Per #2058 orphan migration.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::test_api::EditorTestApi;

#[test]
fn migrated_shift_backspace_deletes_one_char_via_key_handler() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.type_text("abc").unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "abc");

    // Production key path: Shift+Backspace must route to the
    // same DeleteBackward semantics as plain Backspace. The
    // normalize_key SHIFT-strip rule is what makes this work.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::SHIFT)
        .unwrap();

    assert_eq!(harness.get_buffer_content().unwrap(), "ab");
    assert_eq!(harness.api_mut().primary_caret().position, 2);
}

#[test]
fn migrated_shift_backspace_matches_plain_backspace() {
    // Parity claim: Shift+Backspace and plain Backspace must
    // produce the same buffer state from the same starting
    // point. Pre-fix this would diverge (Shift+Backspace was a
    // no-op while plain Backspace deleted one char).
    let mut a = EditorTestHarness::new(80, 24).unwrap();
    a.type_text("hello").unwrap();
    a.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();

    let mut b = EditorTestHarness::new(80, 24).unwrap();
    b.type_text("hello").unwrap();
    b.send_key(KeyCode::Backspace, KeyModifiers::SHIFT).unwrap();

    assert_eq!(
        a.get_buffer_content().unwrap(),
        b.get_buffer_content().unwrap(),
        "Shift+Backspace must behave identically to plain Backspace"
    );
}

/// Anti-test: dispatching the action directly via the API
/// bypasses normalize_key — it cannot detect a regression in
/// the SHIFT-strip rule. This is what migrated_block_selection
/// got wrong; this anti-test pins the gap so a future migration
/// author doesn't repeat the mistake.
///
/// (The test passes because Action::DeleteBackward DOES delete
/// a char; the point is that this style of test couldn't
/// detect issue #1588 itself — which is why this file uses
/// send_key.)
#[test]
fn anti_action_dispatch_cannot_observe_shift_backspace_regression() {
    use fresh::test_api::Action;
    let mut h = EditorTestHarness::new(80, 24).unwrap();
    h.type_text("abc").unwrap();
    h.api_mut().dispatch(Action::DeleteBackward);
    // This always passes regardless of normalize_key state.
    // The migrated_shift_backspace_* tests above use send_key
    // which DOES route through normalize_key.
    assert_eq!(h.get_buffer_content().unwrap(), "ab");
}
