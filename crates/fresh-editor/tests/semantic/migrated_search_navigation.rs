//! Faithful migrations of `tests/e2e/search_navigation_after_move.rs`
//! (issue #1305): Find Next / Find Previous must navigate relative
//! to the *current cursor position*, not to the last visited match
//! index.
//!
//! Drives the real search-modal flow (`Action::Search` +
//! per-char `Action::InsertChar` into the prompt +
//! `Action::PromptConfirm`) followed by buffer cursor moves and
//! `Action::FindNext` / `Action::FindPrevious`. No mocks.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, repeat, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

const NEEDLE_FILE: &str = "\
line 0 filler text
line 1 filler text
line 2 NEEDLE here
line 3 filler text
line 4 filler text
line 5 NEEDLE here
line 6 filler text
line 7 filler text
line 8 NEEDLE here
line 9 filler text";

fn search_for(query: &str) -> Vec<Action> {
    let mut out = vec![Action::Search];
    out.extend(query.chars().map(Action::InsertChar));
    out.push(Action::PromptConfirm);
    out
}

#[test]
fn migrated_find_next_respects_cursor_position_after_move() {
    // Original: `test_find_next_respects_cursor_position_after_move`.
    // After searching for "NEEDLE", cursor lands on match 1 (line
    // 2). Moving Down 5 times puts the cursor on line 7 — past
    // match 2 (line 5), before match 3 (line 8). FindNext must
    // jump to match 3, not match 2 (which would be the next-by-
    // index from the last-visited).
    let match1 = NEEDLE_FILE.find("NEEDLE").unwrap();
    let match3 = {
        let m2 = NEEDLE_FILE[match1 + 1..].find("NEEDLE").unwrap() + match1 + 1;
        NEEDLE_FILE[m2 + 1..].find("NEEDLE").unwrap() + m2 + 1
    };

    let mut actions = search_for("NEEDLE");
    actions.extend(repeat(Action::MoveDown, 5));
    actions.push(Action::FindNext);

    assert_buffer_scenario(BufferScenario {
        description: "Issue #1305: FindNext after MoveDown ×5 jumps to nearest match after cursor"
            .into(),
        initial_text: NEEDLE_FILE.into(),
        actions,
        expected_text: NEEDLE_FILE.into(),
        expected_primary: CursorExpect::at(match3),
        // Search may leave the match selected — we only assert
        // cursor position pin, not selection.
        ..Default::default()
    });
}

#[test]
fn migrated_find_previous_respects_cursor_position_after_move() {
    // Original: `test_find_previous_respects_cursor_position_after_move`
    // (simplified to one direction). Search → land on match 1.
    // MoveDocumentEnd → cursor at end of file. FindPrevious must
    // jump to match 3 (the last NEEDLE), the nearest before the
    // current cursor.
    let match1 = NEEDLE_FILE.find("NEEDLE").unwrap();
    let match3 = {
        let m2 = NEEDLE_FILE[match1 + 1..].find("NEEDLE").unwrap() + match1 + 1;
        NEEDLE_FILE[m2 + 1..].find("NEEDLE").unwrap() + m2 + 1
    };

    let mut actions = search_for("NEEDLE");
    actions.push(Action::MoveDocumentEnd);
    actions.push(Action::FindPrevious);

    assert_buffer_scenario(BufferScenario {
        description: "Issue #1305: FindPrevious from EOF jumps to nearest match before cursor"
            .into(),
        initial_text: NEEDLE_FILE.into(),
        actions,
        expected_text: NEEDLE_FILE.into(),
        expected_primary: CursorExpect::at(match3),
        ..Default::default()
    });
}

/// Anti-test: drops the cursor-move actions (`MoveDown ×5`).
/// Without the move, FindNext steps from match 1 to match 2 (line
/// 5) — not match 3. The match-3 expectation must NOT match.
#[test]
fn anti_find_next_without_cursor_move_yields_check_err() {
    let match1 = NEEDLE_FILE.find("NEEDLE").unwrap();
    let match3 = {
        let m2 = NEEDLE_FILE[match1 + 1..].find("NEEDLE").unwrap() + match1 + 1;
        NEEDLE_FILE[m2 + 1..].find("NEEDLE").unwrap() + m2 + 1
    };

    let mut actions = search_for("NEEDLE");
    actions.push(Action::FindNext);

    let scenario = BufferScenario {
        description: "anti: cursor-move dropped — FindNext should land on match 2, not match 3"
            .into(),
        initial_text: NEEDLE_FILE.into(),
        actions,
        expected_text: NEEDLE_FILE.into(),
        expected_primary: CursorExpect::at(match3),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without MoveDown ×5, FindNext steps to match 2 by index; \
         the match-3 expectation must NOT match"
    );
}
