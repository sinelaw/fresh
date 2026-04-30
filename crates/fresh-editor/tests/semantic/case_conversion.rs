//! Phase 2 PoC: declarative rewrite of `tests/e2e/case_conversion.rs::test_to_uppercase`.
//!
//! The original is a 39-line imperative transcript that drives keys
//! through the input layer and observes the buffer through the harness.
//! The version below is a 12-line declarative *theorem*: it states what
//! the editor should do, not how a user gets it to do that.
//!
//! Notable differences from the E2E original:
//! - No `crossterm::KeyCode`. Actions are spelled by intent.
//! - No `harness.render()`. The runner never touches a terminal.
//! - No screen scraping. Assertions are on `EditorTestApi` observables.
//! - Test runs in single-digit milliseconds.
//!
//! The original `test_to_uppercase` continues to exist alongside this
//! one; the migration is additive (see CONTRIBUTING.md §2 and
//! docs/internal/e2e-test-migration-design.md §7).

use crate::common::theorem::buffer_theorem::{
    assert_buffer_theorem, repeat, BufferTheorem, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn theorem_to_uppercase_selection() {
    let mut actions = Vec::new();
    actions.extend(repeat(Action::SelectRight, 5));
    actions.push(Action::ToUpperCase);

    assert_buffer_theorem(BufferTheorem {
        description: "ToUpperCase uppercases the 5-byte selection at byte 0 \
             and collapses the selection at the selection end",
        initial_text: "hello world",
        actions,
        expected_text: "HELLO world",
        // Note: ToUpperCase collapses the selection — the original
        // imperative test was silent about this, so the theorem pins
        // it down.
        expected_primary: CursorExpect::at(5),
        expected_extra_cursors: vec![],
        expected_selection_text: Some(""),
    });
}
