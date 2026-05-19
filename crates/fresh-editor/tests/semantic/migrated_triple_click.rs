//! DECLARATIVE migration of `tests/e2e/triple_click.rs` (issue #597).
//!
//! Three rapid clicks at the same cell are promoted to
//! `SelectLine` by `Editor::handle_mouse`; two rapid clicks are
//! promoted to `SelectWord` (double-click). The original drove
//! `harness.mouse_click(col, row)` directly; the declarative
//! equivalent drives `InputEvent::Mouse(MouseEvent::Click { … })`
//! through the BufferScenario runner, which dispatches each click
//! into the same `handle_mouse` code path.
//!
//! Issue #597: Support click 3 times to select the whole line.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use crate::common::scenario::context::{MouseButton, MouseEvent};
use crate::common::scenario::input_event::InputEvent;

/// Row of the first content line on screen. Mirrors the harness's
/// `content_area_rows().0` — both reduce to the layout constant
/// `CONTENT_START_ROW = 2` (menu bar at row 0, tab bar at row 1).
/// Spelling it out as a const here keeps the scenario data
/// self-contained.
const CONTENT_FIRST_ROW: u16 = 2;

fn click(row: u16, col: u16) -> InputEvent {
    InputEvent::Mouse(MouseEvent::Click {
        row,
        col,
        button: MouseButton::Left,
    })
}

/// Build a triple-click scenario over `initial_text`. Pinning the
/// selection text — not the exact (anchor, caret) byte range —
/// because the range is sensitive to whether SelectLine's caret
/// lands at the leading-newline or trailing-newline position, and
/// the load-bearing claim is "the line text is selected".
fn triple_click_at(description: &str, initial_text: &str, row: u16, expected_selection: &str) -> BufferScenario {
    BufferScenario {
        description: description.into(),
        initial_text: initial_text.into(),
        events: vec![click(row, 12), click(row, 12), click(row, 12)],
        expected_text: initial_text.into(),
        expected_primary: CursorExpect::default(), // ignored — wildcarded below
        expected_selection_text: Some(expected_selection.into()),
        ..Default::default()
    }
}

#[test]
fn migrated_triple_click_selects_first_line() {
    // Original: `test_triple_click_selects_line`. Three rapid
    // clicks at the same cell on line 1 ⇒ SelectLine over line 1.
    //
    // The selection-text claim is what the e2e original asserted
    // (`assert!(selection.contains("First line here"))`); we tighten
    // it to equality on the line text.
    let s = triple_click_at(
        "Triple-click on line 1 selects 'First line here'",
        "First line here\nSecond line here\nThird line here\n",
        CONTENT_FIRST_ROW,
        "First line here\n",
    );
    // The cursor lands at the end of the selected line (byte 16,
    // just past the newline). Anchor at byte 0.
    assert_buffer_scenario(BufferScenario {
        expected_primary: CursorExpect::range(0, 16),
        ..s
    });
}

#[test]
fn migrated_triple_click_selects_middle_line() {
    // Original: `test_triple_click_middle_line`. Three rapid
    // clicks on row 2 of the content area ⇒ SelectLine over the
    // second buffer line.
    let s = triple_click_at(
        "Triple-click on line 2 selects 'Line two'",
        "Line one\nLine two\nLine three\n",
        CONTENT_FIRST_ROW + 1,
        "Line two\n",
    );
    // "Line one\n" = 9 bytes; line 2 spans 9..18.
    assert_buffer_scenario(BufferScenario {
        expected_primary: CursorExpect::range(9, 18),
        ..s
    });
}

#[test]
fn migrated_double_click_selects_word_not_line() {
    // Original: `test_double_click_still_selects_word`. Two
    // rapid clicks at col 12 of "hello world test" ⇒ SelectWord
    // (NOT SelectLine).
    //
    // The e2e original asserted only that the selection didn't
    // contain the whole line; we pin the positive claim:
    // double-click selects exactly the word under the click.
    let row = CONTENT_FIRST_ROW;
    assert_buffer_scenario(BufferScenario {
        description: "Double-click selects the word under the cursor, not the line".into(),
        initial_text: "hello world test\n".into(),
        events: vec![click(row, 12), click(row, 12)],
        expected_text: "hello world test\n".into(),
        // "hello " spans bytes 0..6; "world" spans 6..11.
        expected_primary: CursorExpect::range(6, 11),
        expected_selection_text: Some("world".into()),
        ..Default::default()
    });
}

/// Anti-test (declarative): drop the third click from the
/// triple-click scenario. With only two clicks, `handle_mouse`
/// resolves to SelectWord — so the selection is a single token
/// ("here"), not the entire "First line here" line. Proves the
/// third click is what promotes the gesture from double-click to
/// triple-click.
#[test]
fn anti_triple_click_dropping_third_click_does_not_select_full_line() {
    let row = CONTENT_FIRST_ROW;
    assert_buffer_scenario(BufferScenario {
        description:
            "anti: two clicks (no third) yield SelectWord on 'here', NOT SelectLine".into(),
        initial_text: "First line here\nSecond line here\nThird line here\n".into(),
        events: vec![click(row, 12), click(row, 12)],
        expected_text: "First line here\nSecond line here\nThird line here\n".into(),
        // "First line " spans bytes 0..11; "here" spans 11..15.
        expected_primary: CursorExpect::range(11, 15),
        expected_selection_text: Some("here".into()),
        ..Default::default()
    });
}
