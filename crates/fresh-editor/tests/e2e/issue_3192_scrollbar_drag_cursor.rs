//! Regression tests for issue #3192: dragging the vertical scrollbar
//! relocated the text cursor.
//!
//! `handle_scrollbar_drag_relative` and `handle_scrollbar_jump` finished by
//! calling `move_cursor_to_visible_area`, which pulled the cursor to the
//! first line of the freshly scrolled viewport whenever the scroll had left
//! it off-screen. The wheel path never did that, which is the asymmetry the
//! issue reports: scrolling is a viewport operation and must not mutate the
//! buffer's cursor.
//!
//! The relocation was invisible until the next keypress — the status bar
//! still read the old `Ln`/`Col` — so the first character typed after a drag
//! landed on a line the user never navigated to, and a save wrote it there.
//! That is what these tests observe: after the scroll, type and look at
//! *which rendered line the text joined* (CONTRIBUTING.md Testing §2 — the
//! assertions read the screen, not the model). Asserting on the status bar
//! alone would pass on the broken build, because the stale status bar is
//! precisely half the bug.
//!
//! Each scroll path gets its own test — thumb drag, track jump, and the
//! mouse wheel as the control that was always correct — and all three make
//! the same assertion, which is the parity the fix restores.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Lines in the fixture — far more than the viewport, so a scroll of any
/// size leaves line 10 off-screen.
const LINES: usize = 1000;

/// Terminal geometry. The vertical scrollbar is the rightmost column.
const WIDTH: u16 = 80;
const HEIGHT: u16 = 24;
const SCROLLBAR_COL: u16 = WIDTH - 1;

/// A harness on `LINES` numbered lines with the cursor parked at the start
/// of line 10, which is where every test below expects to find it again.
fn harness_with_cursor_on_line_10() -> EditorTestHarness {
    let mut harness = EditorTestHarness::new(WIDTH, HEIGHT).unwrap();
    let content: String = (1..=LINES).map(|i| format!("line {i:04}\n")).collect();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 9)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.screen_to_string().contains("line 0010"),
        "setup: line 10 should be on screen before scrolling"
    );
    harness
}

/// The topmost row of the scrollbar thumb, which is what a user grabs.
fn thumb_top_row(harness: &EditorTestHarness) -> u16 {
    let (first_row, last_row) = harness.content_area_rows();
    (first_row..=last_row)
        .map(|r| r as u16)
        .find(|&r| harness.is_scrollbar_thumb_at(SCROLLBAR_COL, r))
        .expect("a 1000-line buffer in a 24-row terminal must render a thumb")
}

/// Type three characters and require that they joined line 10 — the line the
/// cursor was on before the scroll — and not whatever line the scroll left at
/// the top of the viewport. The keypress clears `skip_ensure_visible`, so the
/// view scrolls back to the cursor and the edited line is on screen to read.
fn assert_typing_lands_on_line_10(harness: &mut EditorTestHarness, scrolled_from: &str) {
    harness.type_text("ZZZ").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("ZZZline 0010"),
        "{scrolled_from} moved the cursor: text typed afterwards should join \
         line 10, but line 10 does not carry it. Screen:\n{screen}"
    );
    assert_eq!(
        screen.matches("ZZZ").count(),
        1,
        "{scrolled_from}: line 10 should be the only line carrying the typed \
         text. Screen:\n{screen}"
    );
    assert!(
        screen.contains("Ln 10, Col 4"),
        "{scrolled_from}: the status bar should report the cursor still on \
         line 10, one column past the typed text. Screen:\n{screen}"
    );
}

/// Require that the scroll actually left line 10 off-screen — without that,
/// the cursor was never out of the viewport and the test proves nothing.
fn assert_line_10_scrolled_away(harness: &EditorTestHarness, scrolled_from: &str) {
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("line 0010"),
        "{scrolled_from} should have scrolled line 10 out of view. Screen:\n{screen}"
    );
}

/// Dragging the thumb scrolls the view and leaves the cursor alone.
#[test]
fn scrollbar_thumb_drag_does_not_move_the_cursor() {
    let mut harness = harness_with_cursor_on_line_10();

    let (_, last_row) = harness.content_area_rows();
    let start_row = thumb_top_row(&harness);
    // `mouse_drag` interpolates one motion event per row, so this is the
    // dense event stream a real mouse produces, not a couple of jumps.
    harness
        .mouse_drag(SCROLLBAR_COL, start_row, SCROLLBAR_COL, last_row as u16)
        .unwrap();
    harness.render().unwrap();

    assert_line_10_scrolled_away(&harness, "a scrollbar thumb drag");
    assert_typing_lands_on_line_10(&mut harness, "a scrollbar thumb drag");
}

/// Clicking the track jumps the view and leaves the cursor alone — the same
/// ruling, on the other scrollbar handler that called the cursor fixup.
#[test]
fn scrollbar_track_jump_does_not_move_the_cursor() {
    let mut harness = harness_with_cursor_on_line_10();

    let (_, last_row) = harness.content_area_rows();
    // A press below the thumb lands on the track, which jumps rather than
    // drags.
    harness.mouse_click(SCROLLBAR_COL, last_row as u16).unwrap();
    harness.render().unwrap();

    assert_line_10_scrolled_away(&harness, "a scrollbar track jump");
    assert_typing_lands_on_line_10(&mut harness, "a scrollbar track jump");
}

/// The control: the wheel was always correct, and the scrollbar now matches
/// it. If this one ever fails, the ruling itself changed.
#[test]
fn mouse_wheel_scroll_does_not_move_the_cursor() {
    let mut harness = harness_with_cursor_on_line_10();

    let (first_row, _) = harness.content_area_rows();
    for _ in 0..20 {
        harness.mouse_scroll_down(10, first_row as u16).unwrap();
    }
    harness.render().unwrap();

    assert_line_10_scrolled_away(&harness, "a mouse-wheel scroll");
    assert_typing_lands_on_line_10(&mut harness, "a mouse-wheel scroll");
}
