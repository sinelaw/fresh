//! Regression tests for issue #3407: clicking the top visible row scrolled
//! the view by the scroll-off margin, and after a drag left the cursor on the
//! top row, the next key press scrolled the view by the margin even when the
//! key did not move the cursor.
//!
//! A click names a cell the user can see, so it keeps the rows where they
//! are. The scroll-off margin applies to keyboard movement: the first key
//! that moves the cursor away from where the pointer left it places the rows
//! with the margin again, and a key that leaves the cursor alone leaves the
//! view alone.
//!
//! Assertions read the rendered screen: the gutter's first line number (the
//! view's top) and the status bar's `Ln`.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// The first line number drawn in the gutter: which line the view starts at.
fn top_line(harness: &EditorTestHarness) -> usize {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .find_map(|row| row.split_once('│')?.0.trim().parse().ok())
        .unwrap_or_else(|| panic!("no line numbers on screen:\n{screen}"))
}

/// The `N` of the status bar's `Ln N, Col M`.
fn status_line(harness: &EditorTestHarness) -> usize {
    let status = harness.get_status_bar();
    let rest = &status[status
        .find("Ln ")
        .unwrap_or_else(|| panic!("no Ln in status bar: {status:?}"))
        + 3..];
    rest.chars()
        .take_while(|c| c.is_ascii_digit())
        .collect::<String>()
        .parse()
        .unwrap()
}

/// A 200-line file, with the view moved down by the keyboard so its top is
/// well past line 1.
fn scrolled_harness() -> (EditorTestHarness, crate::common::fixtures::TestFixture) {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    let content: Vec<String> = (1..=200).map(|i| format!("line {i:03} text")).collect();
    let fixture = harness.load_buffer_from_text(&content.join("\n")).unwrap();
    harness.render().unwrap();
    for _ in 0..80 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    assert!(
        top_line(&harness) > 10,
        "the keyboard should have scrolled the view"
    );
    (harness, fixture)
}

#[test]
fn test_click_on_top_row_does_not_scroll() {
    let (mut harness, _fixture) = scrolled_harness();
    let top = top_line(&harness);
    let (first_row, _) = harness.content_area_rows();

    harness.mouse_click(20, first_row as u16).unwrap();

    assert_eq!(
        status_line(&harness),
        top,
        "the click should put the cursor on the clicked line:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        top_line(&harness),
        top,
        "clicking a visible row scrolled the view:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn test_key_after_drag_to_top_row_scrolls_only_when_it_moves_the_cursor() {
    let (mut harness, _fixture) = scrolled_harness();
    let top = top_line(&harness);
    let (first_row, _) = harness.content_area_rows();
    let first_row = first_row as u16;

    harness
        .mouse_drag(20, first_row + 10, 20, first_row)
        .unwrap();
    assert_eq!(status_line(&harness), top);
    assert_eq!(top_line(&harness), top, "the drag itself must not scroll");

    // Esc drops the selection but leaves the cursor where it is.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert_eq!(status_line(&harness), top);
    assert_eq!(
        top_line(&harness),
        top,
        "a key that did not move the cursor scrolled the view:\n{}",
        harness.screen_to_string()
    );

    // Moving the cursor with the keyboard brings the scroll-off margin back.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert_eq!(status_line(&harness), top - 1);
    assert!(
        top_line(&harness) < top - 1,
        "keyboard movement to the edge should keep the scroll-off margin:\n{}",
        harness.screen_to_string()
    );
}
