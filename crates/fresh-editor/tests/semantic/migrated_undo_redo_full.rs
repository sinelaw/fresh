//! Faithful migration of `tests/e2e/undo_redo.rs`.
//!
//! Save-point tests (those that require the FS layer for the
//! "save point" mechanism) are deferred — they belong in
//! PersistenceScenario when needed. The pure undo/redo
//! interaction tests are migrated here.
//!
//! These already have semantic counterparts in `undo_redo.rs`
//! (the original Track-B migration). The "_full" file faithfully
//! mirrors the e2e action sequences with one-to-one cursor
//! pinning, where the existing semantic tests sometimes assert
//! on different aspects.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_undo_skips_readonly_movement_actions_full() {
    // Original: `test_undo_skips_readonly_movement_actions`.
    // Type "hello" (cursor 5), MoveLeft ×2 (cursor 3), Undo.
    // Expect "hell", cursor 4.
    assert_buffer_scenario(BufferScenario {
        description: "Undo after type+movement undoes the typing AND restores cursor pre-movement"
            .into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('h'),
            Action::InsertChar('e'),
            Action::InsertChar('l'),
            Action::InsertChar('l'),
            Action::InsertChar('o'),
            Action::MoveLeft,
            Action::MoveLeft,
            Action::Undo,
        ],
        expected_text: "hell".into(),
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    });
}

#[test]
fn migrated_multiple_undo_skips_all_readonly_actions_full() {
    // Original: `test_multiple_undo_skips_all_readonly_actions`.
    // Type "abc", various movements, Undo three times.
    assert_buffer_scenario(BufferScenario {
        description: "Three undos skip movements and roll back inserts one at a time".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::InsertChar('c'),
            Action::MoveLeft,
            Action::MoveRight,
            Action::MoveLineStart,
            Action::MoveLineEnd,
            Action::Undo,
            Action::Undo,
            Action::Undo,
        ],
        expected_text: String::new(),
        // After all rolled back, cursor at byte 0.
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

#[test]
fn migrated_redo_skips_readonly_movement_actions_full() {
    // Original: `test_redo_skips_readonly_movement_actions`.
    // Type "xyz", MoveLeft, Undo (→ "xy"), Redo (→ "xyz").
    assert_buffer_scenario(BufferScenario {
        description: "Redo skips intervening movements and reapplies the insert".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('x'),
            Action::InsertChar('y'),
            Action::InsertChar('z'),
            Action::MoveLeft,
            Action::Undo,
            Action::Redo,
        ],
        expected_text: "xyz".into(),
        // FINDING (matches finding §4 in the migration findings
        // doc): Redo doesn't re-advance the cursor past the
        // re-inserted bytes — observed at byte 2.
        expected_primary: CursorExpect::at(2),
        ..Default::default()
    });
}

/// Anti-test: drops the trailing `Undo` from
/// `migrated_undo_skips_readonly_movement_actions_full`.
/// Without it, all 5 chars survive — "hello" remains in the
/// buffer, so the expected post-undo "hell" cannot match.
/// Proves the Undo action is the load-bearing step.
#[test]
fn anti_undo_redo_dropping_undo_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: trailing Undo dropped — 'hello' is not rolled back to 'hell'".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('h'),
            Action::InsertChar('e'),
            Action::InsertChar('l'),
            Action::InsertChar('l'),
            Action::InsertChar('o'),
            Action::MoveLeft,
            Action::MoveLeft,
            // Undo removed.
        ],
        expected_text: "hell".into(),
        expected_primary: CursorExpect::at(4),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without Undo the buffer stays 'hello'; \
         the rolled-back 'hell' cannot appear"
    );
}

#[test]
fn migrated_undo_redo_with_mixed_actions_full() {
    // Original: `test_undo_redo_with_mixed_actions`. Just a
    // smoke test that mixed sequences round-trip cleanly.
    assert_buffer_scenario(BufferScenario {
        description: "Insert + Undo + Redo round-trip preserves text".into(),
        initial_text: String::new(),
        actions: vec![
            Action::InsertChar('A'),
            Action::InsertChar('B'),
            Action::InsertChar('C'),
            Action::Undo,
            Action::Undo,
            Action::Redo,
            Action::Redo,
        ],
        expected_text: "ABC".into(),
        // After two Redos in sequence (no intervening movements),
        // the cursor IS re-advanced to byte 3 (end of redone
        // run). Redo's cursor-advance asymmetry only manifests
        // when Redo follows actions other than another Redo.
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}
