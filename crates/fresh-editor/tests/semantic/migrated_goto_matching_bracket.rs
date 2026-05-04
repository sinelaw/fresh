//! Faithful migration of `tests/e2e/goto_matching_bracket.rs`.
//!
//! Originals open a real file containing balanced brackets,
//! position the cursor with arrow keys, and press `Ctrl+]`. The
//! scenario equivalent dispatches `Action::MoveRight` to position
//! the cursor and `Action::GoToMatchingBracket` to jump.
//!
//! Issue #1258: cursor *inside* (not on) a bracket pair should
//! also jump to the nearest enclosing closing bracket.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, repeat, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

// ── Existing behavior: cursor ON a bracket ──────────────────────────

#[test]
fn migrated_goto_matching_bracket_from_opening_paren() {
    // Original: `test_goto_matching_bracket_from_opening_paren`.
    // Cursor at position 3 (on '('); should jump to position 7 (')').
    assert_buffer_scenario(BufferScenario {
        description: "GoToMatchingBracket on '(' jumps to matching ')'".into(),
        initial_text: "foo(bar)".into(),
        actions: repeat(Action::MoveRight, 3)
            .chain(std::iter::once(Action::GoToMatchingBracket))
            .collect(),
        expected_text: "foo(bar)".into(),
        expected_primary: CursorExpect::at(7),
        ..Default::default()
    });
}

#[test]
fn migrated_goto_matching_bracket_from_closing_paren() {
    // Original: `test_goto_matching_bracket_from_closing_paren`.
    // Cursor at position 7 (on ')'); should jump to position 3 ('(').
    assert_buffer_scenario(BufferScenario {
        description: "GoToMatchingBracket on ')' jumps to matching '('".into(),
        initial_text: "foo(bar)".into(),
        actions: repeat(Action::MoveRight, 7)
            .chain(std::iter::once(Action::GoToMatchingBracket))
            .collect(),
        expected_text: "foo(bar)".into(),
        expected_primary: CursorExpect::at(3),
        ..Default::default()
    });
}

// ── Issue #1258: cursor INSIDE brackets ─────────────────────────────

#[test]
fn migrated_goto_matching_bracket_from_inside_parens() {
    // Original: `test_goto_matching_bracket_from_inside_parens`.
    // Cursor at byte 4 (on 'b' inside parens); should jump to ')'.
    assert_buffer_scenario(BufferScenario {
        description: "GoToMatchingBracket inside parens jumps to nearest ')'".into(),
        initial_text: "foo(bar)".into(),
        actions: repeat(Action::MoveRight, 4)
            .chain(std::iter::once(Action::GoToMatchingBracket))
            .collect(),
        expected_text: "foo(bar)".into(),
        expected_primary: CursorExpect::at(7),
        ..Default::default()
    });
}

#[test]
fn migrated_goto_matching_bracket_from_inside_curly_braces() {
    // Original: `test_goto_matching_bracket_from_inside_curly_braces`.
    // "fn main() { hello }"; cursor at 13 (on 'h'); jumps to '}' at 18.
    assert_buffer_scenario(BufferScenario {
        description: "GoToMatchingBracket inside braces jumps to nearest '}'".into(),
        initial_text: "fn main() { hello }".into(),
        actions: repeat(Action::MoveRight, 13)
            .chain(std::iter::once(Action::GoToMatchingBracket))
            .collect(),
        expected_text: "fn main() { hello }".into(),
        expected_primary: CursorExpect::at(18),
        ..Default::default()
    });
}

#[test]
fn migrated_goto_matching_bracket_from_inside_square_brackets() {
    // Original: `test_goto_matching_bracket_from_inside_square_brackets`.
    // "arr[1, 2, 3]"; cursor at 5 (on ','); jumps to ']' at 11.
    assert_buffer_scenario(BufferScenario {
        description: "GoToMatchingBracket inside brackets jumps to nearest ']'".into(),
        initial_text: "arr[1, 2, 3]".into(),
        actions: repeat(Action::MoveRight, 5)
            .chain(std::iter::once(Action::GoToMatchingBracket))
            .collect(),
        expected_text: "arr[1, 2, 3]".into(),
        expected_primary: CursorExpect::at(11),
        ..Default::default()
    });
}

#[test]
fn migrated_goto_matching_bracket_from_inside_nested_inner() {
    // Original: `test_goto_matching_bracket_from_inside_nested`.
    // "foo(bar[baz])"; cursor at 8 (on 'b' of "baz", inside []);
    // jumps to ']' at 11 (the nearest enclosing closer).
    assert_buffer_scenario(BufferScenario {
        description: "GoToMatchingBracket inside nested brackets jumps to inner ']'".into(),
        initial_text: "foo(bar[baz])".into(),
        actions: repeat(Action::MoveRight, 8)
            .chain(std::iter::once(Action::GoToMatchingBracket))
            .collect(),
        expected_text: "foo(bar[baz])".into(),
        expected_primary: CursorExpect::at(11),
        ..Default::default()
    });
}

/// Anti-test: guards against the migration being structurally
/// inert. Reuses the same expectation as
/// [`migrated_goto_matching_bracket_from_inside_curly_braces`]
/// (cursor at byte 18) but DROPS the `GoToMatchingBracket` action.
/// Without the jump the cursor stays at byte 13, so
/// `check_buffer_scenario` must return `Err` — proving the assertion
/// pipeline is genuinely sensitive to that action being applied.
#[test]
fn anti_goto_matching_bracket_dropping_action_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: GoToMatchingBracket dropped — cursor must not reach byte 18".into(),
        initial_text: "fn main() { hello }".into(),
        actions: repeat(Action::MoveRight, 13).collect(),
        expected_text: "fn main() { hello }".into(),
        expected_primary: CursorExpect::at(18),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without GoToMatchingBracket the cursor stays at 13, \
         so the scenario must NOT meet the cursor-at-18 expectation"
    );
}

#[test]
fn migrated_goto_matching_bracket_from_inside_nested_outer() {
    // Original: `test_goto_matching_bracket_from_inside_outer_of_nested`.
    // "foo(bar[baz])"; cursor at 4 (on 'b' of "bar", inside () but
    // outside []); jumps to ')' at 12.
    assert_buffer_scenario(BufferScenario {
        description: "GoToMatchingBracket inside outer parens (skipping nested) jumps to ')'"
            .into(),
        initial_text: "foo(bar[baz])".into(),
        actions: repeat(Action::MoveRight, 4)
            .chain(std::iter::once(Action::GoToMatchingBracket))
            .collect(),
        expected_text: "foo(bar[baz])".into(),
        expected_primary: CursorExpect::at(12),
        ..Default::default()
    });
}
