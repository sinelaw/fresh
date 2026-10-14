//! Regression tests for issue #3412: clicking in the empty area below the
//! text put the cursor one character short of the end of the file when the
//! last line was a single character, such as the closing `}` of a JSON file.
//!
//! A click below the text is looked up on the last line, and that lookup
//! treated a row with one drawn cell as an empty line. The fix for #3351
//! (`click_geometry.rs`, which now checks the line's content end column)
//! fixed this path too, but only clicks to the right of a line were tested.
//! These tests cover clicks below the text so it stays fixed.
//!
//! The tests click below the text and read the `Ln N, Col M` readout in the
//! status bar.

use crate::common::harness::EditorTestHarness;

/// The `Ln N, Col M` readout from the rendered status bar.
fn line_col(harness: &EditorTestHarness) -> String {
    let status = harness.get_status_bar();
    let start = status
        .find("Ln ")
        .unwrap_or_else(|| panic!("no line/column readout in status bar: {status:?}"));
    let rest = &status[start..];
    let end = rest.find("  ").unwrap_or(rest.len());
    rest[..end].trim().to_string()
}

/// The screen position where `text` is drawn.
fn position_of(harness: &EditorTestHarness, text: &str) -> (u16, u16) {
    harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{text:?} not on screen:\n{}", harness.screen_to_string()))
}

/// Opens `content` and clicks a few rows below its last line, once per
/// `(columns to the right of the line start, expected readout)` pair.
///
/// A click below the text uses the click's column on the last line, and
/// stops at the end of that line once the click is past it.
fn assert_clicks_below_text(content: &str, last_line: &str, cases: &[(u16, &str)]) {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (text_col, last_row) = position_of(&harness, last_line);
    let (first_col, first_row) = position_of(&harness, "{");
    for &(col_offset, expected) in cases {
        // Put the cursor somewhere else first, so each click has to move it.
        harness.mouse_click(first_col, first_row).unwrap();

        harness
            .mouse_click(text_col + col_offset, last_row + 3)
            .unwrap();
        assert_eq!(
            line_col(&harness),
            expected,
            "click below the text, {col_offset} columns in. Screen:\n{}",
            harness.screen_to_string()
        );
    }
}

/// The case from the issue: the file ends in a lone `}` with no newline after
/// it. A click below the text and to the right of the `}` has to land after
/// it, at the end of the file, not before it.
#[test]
fn click_below_text_lands_after_one_char_last_line() {
    assert_clicks_below_text(
        "{\n  \"key\": 1\n}",
        "}",
        &[
            (0, "Ln 3, Col 1"),
            (1, "Ln 3, Col 2"),
            (5, "Ln 3, Col 2"),
            (30, "Ln 3, Col 2"),
        ],
    );
}

/// The control the maintainer tested: a longer last line already worked, and
/// still has to.
#[test]
fn click_below_text_lands_at_end_of_longer_last_line() {
    assert_clicks_below_text(
        "{\n  \"key\": 1\n}}}",
        "}}}",
        &[
            (0, "Ln 3, Col 1"),
            (2, "Ln 3, Col 3"),
            (3, "Ln 3, Col 4"),
            (30, "Ln 3, Col 4"),
        ],
    );
}

/// With a trailing newline the end of the file is the empty line after the
/// `}`, so that is where a click below the text belongs.
#[test]
fn click_below_text_with_trailing_newline_lands_on_the_empty_last_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness
        .load_buffer_from_text("{\n  \"key\": 1\n}\n")
        .unwrap();
    harness.render().unwrap();

    let (col, row) = position_of(&harness, "  \"key\"");
    harness.mouse_click(col, row + 4).unwrap();
    assert_eq!(
        line_col(&harness),
        "Ln 4, Col 1",
        "Screen:\n{}",
        harness.screen_to_string()
    );
}
