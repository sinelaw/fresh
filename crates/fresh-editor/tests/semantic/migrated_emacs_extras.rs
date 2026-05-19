//! Kill-line and word-deletion Action invariants. NOT a migration
//! of `tests/e2e/emacs_actions.rs` — that file has no
//! `DeleteToLineEnd` or `DeleteWordForward` tests. The genuine
//! emacs migrations live in `migrated_emacs_full.rs` and
//! `semantic/emacs_actions.rs`.
//!
//! Kept here because the two remaining tests exercise real
//! production paths and serve as load-bearing characterisations
//! of the kill verbs.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_kill_line_partial_from_middle() {
    assert_buffer_scenario(BufferScenario {
        description: "DeleteToLineEnd from byte 5 strips ' world'".into(),
        initial_text: "hello world".into(),
        actions: vec![
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::DeleteToLineEnd,
        ],
        expected_text: "hello".into(),
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    });
}

#[test]
fn migrated_kill_word_from_word_start_removes_word() {
    // DeleteWordForward from byte 0 of "foo bar" removes "foo".
    assert_buffer_scenario(BufferScenario {
        description: "DeleteWordForward at word start removes word + following whitespace".into(),
        initial_text: "foo bar".into(),
        actions: vec![Action::DeleteWordForward],
        expected_text: "bar".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

/// Anti-test: drops `DeleteToLineEnd` from
/// `migrated_kill_line_partial_from_middle`. Without it, the
/// buffer stays "hello world" and the expected "hello" (after
/// killing " world") cannot match.
#[test]
fn anti_emacs_kill_dropping_delete_to_line_end_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: DeleteToLineEnd dropped — ' world' is never killed".into(),
        initial_text: "hello world".into(),
        actions: vec![
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
        ],
        expected_text: "hello".into(),
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without DeleteToLineEnd the buffer stays 'hello world'; \
         the post-kill 'hello' result cannot appear"
    );
}
