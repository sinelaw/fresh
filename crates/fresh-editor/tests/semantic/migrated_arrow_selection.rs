//! Faithful migration of `tests/e2e/issue_1566_arrow_selection.rs`.
//!
//! Issue #1566: arrow keys with an active selection should
//! collapse to the corresponding selection edge, not move one
//! grapheme from the cursor's previous position.
//!
//! - **Right** arrow + selection → cursor at the right edge of the
//!   selection, selection cleared.
//! - **Left** arrow + selection → cursor at the left edge of the
//!   selection, selection cleared.
//! - **Down** arrow + selection → collapse to the bottom edge,
//!   then advance one line down (using sticky column from the bottom edge).
//! - **Up** arrow + selection → collapse to the top edge,
//!   then advance one line up.
//!
//! The originals drive `KeyCode::Right/Left/Up/Down` (with and
//! without Shift) and assert on `cursor_position()` and
//! `selection_range()`. The scenario equivalent dispatches
//! `Action::Move{Left,Right,Up,Down}` and
//! `Action::Select{Left,Right,Up,Down}` directly.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, repeat, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

// ── Horizontal: Right/Left arrow with selection ─────────────────────

#[test]
fn migrated_right_arrow_after_forward_selection_goes_to_right_edge() {
    // Original: `test_right_arrow_after_forward_selection_goes_to_right_edge`.
    // Position 2, then Shift+Right ×3 (selection 2..5, cursor at 5),
    // then Right ⇒ cursor at 5, no selection.
    assert_buffer_scenario(BufferScenario {
        description: "Right with forward selection collapses to right edge".into(),
        initial_text: "hello world".into(),
        actions: repeat(Action::MoveRight, 2)
            .chain(repeat(Action::SelectRight, 3))
            .chain(std::iter::once(Action::MoveRight))
            .collect(),
        expected_text: "hello world".into(),
        expected_primary: CursorExpect::at(5),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

#[test]
fn migrated_right_arrow_after_backward_selection_goes_to_right_edge() {
    // Original: `test_right_arrow_after_backward_selection_goes_to_right_edge`.
    // Position 5, Shift+Left ×3 (selection 2..5, cursor at 2,
    // anchor at 5). Right ⇒ cursor at 5 (the anchor / right edge),
    // not 3.
    assert_buffer_scenario(BufferScenario {
        description: "Right with backward selection jumps to right edge (issue #1566)".into(),
        initial_text: "hello world".into(),
        actions: repeat(Action::MoveRight, 5)
            .chain(repeat(Action::SelectLeft, 3))
            .chain(std::iter::once(Action::MoveRight))
            .collect(),
        expected_text: "hello world".into(),
        expected_primary: CursorExpect::at(5),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

#[test]
fn migrated_left_arrow_after_forward_selection_goes_to_left_edge() {
    // Original: `test_left_arrow_after_forward_selection_goes_to_left_edge`.
    // Position 2, Shift+Right ×3, then Left ⇒ cursor at 2 (left
    // edge), not 4 (cursor-1 from previous position 5).
    assert_buffer_scenario(BufferScenario {
        description: "Left with forward selection jumps to left edge".into(),
        initial_text: "hello world".into(),
        actions: repeat(Action::MoveRight, 2)
            .chain(repeat(Action::SelectRight, 3))
            .chain(std::iter::once(Action::MoveLeft))
            .collect(),
        expected_text: "hello world".into(),
        expected_primary: CursorExpect::at(2),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

#[test]
fn migrated_left_arrow_after_backward_selection_goes_to_left_edge() {
    // Original: `test_left_arrow_after_backward_selection_goes_to_left_edge`.
    // Position 5, Shift+Left ×3 (cursor at 2 already), then Left.
    // Expectation: cursor stays at 2 (left edge of selection,
    // already the cursor) — does NOT retreat to 1.
    assert_buffer_scenario(BufferScenario {
        description: "Left with backward selection stays at left edge".into(),
        initial_text: "hello world".into(),
        actions: repeat(Action::MoveRight, 5)
            .chain(repeat(Action::SelectLeft, 3))
            .chain(std::iter::once(Action::MoveLeft))
            .collect(),
        expected_text: "hello world".into(),
        expected_primary: CursorExpect::at(2),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

// ── Vertical: Up/Down arrow with selection ──────────────────────────
//
// Source layout (4 lines × 10 chars + LF, last line has no trailing
// newline):
//   Line 0:  "aaaaaaaaaa\n"   bytes  0..10,  newline at 10
//   Line 1:  "bbbbbbbbbb\n"   bytes 11..21,  newline at 21
//   Line 2:  "cccccccccc\n"   bytes 22..32,  newline at 32
//   Line 3:  "dddddddddd"     bytes 33..43

const MULTI_LINE_CONTENT: &str = "aaaaaaaaaa\nbbbbbbbbbb\ncccccccccc\ndddddddddd";

#[test]
fn migrated_down_arrow_after_forward_selection_collapses_to_bottom_then_moves_down() {
    // Original: `test_down_arrow_after_forward_selection_collapses_to_bottom_then_moves_down`.
    // Start at byte 15 (line 1, col 4); Shift+Down ⇒ selection
    // 15..26, cursor at 26. Down ⇒ collapse to 26 + move to byte 37.
    assert_buffer_scenario(BufferScenario {
        description: "Down with forward selection collapses to bottom, then moves down a line"
            .into(),
        initial_text: MULTI_LINE_CONTENT.into(),
        actions: repeat(Action::MoveRight, 15)
            .chain(std::iter::once(Action::SelectDown))
            .chain(std::iter::once(Action::MoveDown))
            .collect(),
        expected_text: MULTI_LINE_CONTENT.into(),
        expected_primary: CursorExpect::at(37),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

#[test]
fn migrated_down_arrow_after_backward_selection_collapses_to_bottom_then_moves_down() {
    // Original: `test_down_arrow_after_backward_selection_collapses_to_bottom_then_moves_down`.
    // Start at byte 26 (line 2, col 4); Shift+Up ⇒ selection
    // 15..26, cursor at 15 (top), anchor at 26 (bottom).
    // Down ⇒ collapse to bottom (26) + move to byte 37 (line 3, col 4).
    assert_buffer_scenario(BufferScenario {
        description: "Down with backward selection collapses to bottom (anchor), then moves down"
            .into(),
        initial_text: MULTI_LINE_CONTENT.into(),
        actions: repeat(Action::MoveRight, 26)
            .chain(std::iter::once(Action::SelectUp))
            .chain(std::iter::once(Action::MoveDown))
            .collect(),
        expected_text: MULTI_LINE_CONTENT.into(),
        expected_primary: CursorExpect::at(37),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

#[test]
fn migrated_up_arrow_after_forward_selection_collapses_to_top_then_moves_up() {
    // Original: `test_up_arrow_after_forward_selection_collapses_to_top_then_moves_up`.
    // Start at 26, Shift+Down (selection 26..37, cursor at 37);
    // Up ⇒ collapse to top (26) + move to byte 15 (line 1, col 4).
    assert_buffer_scenario(BufferScenario {
        description: "Up with forward selection collapses to top, then moves up a line".into(),
        initial_text: MULTI_LINE_CONTENT.into(),
        actions: repeat(Action::MoveRight, 26)
            .chain(std::iter::once(Action::SelectDown))
            .chain(std::iter::once(Action::MoveUp))
            .collect(),
        expected_text: MULTI_LINE_CONTENT.into(),
        expected_primary: CursorExpect::at(15),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

#[test]
fn migrated_up_arrow_after_backward_selection_collapses_to_top_then_moves_up() {
    // Original: `test_up_arrow_after_backward_selection_collapses_to_top_then_moves_up`.
    // Start at 26, Shift+Up (selection 15..26, cursor at 15);
    // Up ⇒ collapse to top (15) + move to byte 4 (line 0, col 4).
    assert_buffer_scenario(BufferScenario {
        description: "Up with backward selection collapses to top (cursor), then moves up".into(),
        initial_text: MULTI_LINE_CONTENT.into(),
        actions: repeat(Action::MoveRight, 26)
            .chain(std::iter::once(Action::SelectUp))
            .chain(std::iter::once(Action::MoveUp))
            .collect(),
        expected_text: MULTI_LINE_CONTENT.into(),
        expected_primary: CursorExpect::at(4),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    });
}

/// Anti-test: guards against the migration being structurally
/// inert. Reuses the same expectation as
/// [`migrated_right_arrow_after_backward_selection_goes_to_right_edge`]
/// (cursor lands at byte 5) but DROPS the final `MoveRight`. Without
/// the collapse step the cursor stays at byte 2, so
/// `check_buffer_scenario` must return `Err` — proving the assertion
/// actually depends on the issue-#1566 collapse semantics.
#[test]
fn anti_arrow_selection_dropping_final_move_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: MoveRight dropped — cursor must not collapse to right edge".into(),
        initial_text: "hello world".into(),
        actions: repeat(Action::MoveRight, 5)
            .chain(repeat(Action::SelectLeft, 3))
            .collect(),
        expected_text: "hello world".into(),
        expected_primary: CursorExpect::at(5),
        expected_selection_text: Some(String::new()),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without the final MoveRight, cursor stays at 2 with \
         a live selection — must NOT match the collapsed cursor-at-5 expectation"
    );
}
