//! Regression tests for issue #3351: clicking (or ending a drag) past the
//! end of a line that is exactly one character long put the cursor *before*
//! that character (Col 1) instead of after it (Col 2).
//!
//! The click lookup treated any row with at most one rendered cell as an
//! empty line and returned the line's first byte. The newline usually draws
//! no cell, so a lone `}` or `x` has exactly one cell and was mistaken for
//! an empty line.
//!
//! The assertions read the rendered `Ln N, Col M` readout in the status bar.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

const CONTENT: &str = "a {\n  b\n}\nzzz\nx\nyy\n";

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

/// The screen row on which `text` is drawn.
fn row_of(harness: &EditorTestHarness, text: &str) -> u16 {
    harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{text:?} not on screen:\n{}", harness.screen_to_string()))
        .1
}

fn open() -> (EditorTestHarness, crate::common::fixtures::TestFixture) {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness.load_buffer_from_text(CONTENT).unwrap();
    harness.render().unwrap();
    (harness, fixture)
}

/// A click far to the right of every line lands just past its last
/// character — including the one-character lines `}` and `x`.
#[test]
fn test_click_past_end_of_one_char_line_goes_after_the_char() {
    let (mut harness, _fixture) = open();
    let first_row = row_of(&harness, "a {");

    // (line number, expected column) for every line of CONTENT.
    let expected = [(1, 4), (2, 4), (3, 2), (4, 4), (5, 2), (6, 3)];
    for (line, col) in expected {
        harness.mouse_click(60, first_row + line - 1).unwrap();
        assert_eq!(
            line_col(&harness),
            format!("Ln {line}, Col {col}"),
            "click past the end of line {line}. Screen:\n{}",
            harness.screen_to_string()
        );
    }
}

/// A drag that ends past the end of a one-character line puts the selection
/// head after that character, not before it.
#[test]
fn test_drag_ending_past_end_of_one_char_line_goes_after_the_char() {
    let (mut harness, _fixture) = open();
    let first_row = row_of(&harness, "a {");
    let brace_row = first_row + 2;
    let (col, _) = harness.find_text_on_screen("a {").unwrap();

    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: col,
            row: first_row,
            modifiers: KeyModifiers::empty(),
        })
        .unwrap();
    for kind in [
        MouseEventKind::Drag(MouseButton::Left),
        MouseEventKind::Up(MouseButton::Left),
    ] {
        harness
            .send_mouse(MouseEvent {
                kind,
                column: 60,
                row: brace_row,
                modifiers: KeyModifiers::empty(),
            })
            .unwrap();
    }
    harness.render().unwrap();

    assert_eq!(
        line_col(&harness),
        "Ln 3, Col 2",
        "drag ended past the end of `}}`. Screen:\n{}",
        harness.screen_to_string()
    );
}
