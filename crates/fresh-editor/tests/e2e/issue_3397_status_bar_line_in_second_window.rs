//! Regression test for issue #3397: a file already open in one window, then
//! opened in a second window, showed its cursor on the remembered line while
//! the status bar said `Ln 1`.
//!
//! The second window restores the file's saved cursor straight into its
//! split, without a `MoveCursor`. The status bar read the line from a
//! per-buffer cache that only a `MoveCursor` (or a hand-written refresh)
//! kept current, so it showed the new buffer's initial line until the next
//! key. The line is now derived from the cursor every frame.
//!
//! Assertions read the rendered screen: the status bar's `Ln N` and the
//! terminal cursor's row.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

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

/// The gutter line number drawn on the terminal cursor's row.
fn line_under_cursor(harness: &mut EditorTestHarness) -> usize {
    let (_, y) = harness.screen_cursor_position();
    let screen = harness.screen_to_string();
    let row = screen.lines().nth(y as usize).unwrap();
    row.split_once('│')
        .and_then(|(gutter, _)| gutter.trim().parse().ok())
        .unwrap_or_else(|| panic!("no line number on the cursor's row {y}:\n{screen}"))
}

#[test]
fn test_status_bar_shows_restored_line_in_second_window() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let root = harness.project_dir().unwrap();
    let content: String = (1..=30).map(|i| format!("line {i}\n")).collect();
    let file = root.join("a.txt");
    std::fs::write(&file, content).unwrap();

    harness.open_file(&file).unwrap();
    for _ in 0..6 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    assert_eq!(status_line(&harness), 7);

    let second_root = tempfile::tempdir().unwrap();
    let second = harness
        .editor_mut()
        .create_window_at(second_root.path().to_path_buf(), "second".to_string());
    harness.editor_mut().set_active_window(second);
    harness.open_file(&file).unwrap();

    let shown_line = line_under_cursor(&mut harness);
    assert!(
        shown_line > 1,
        "precondition: the second window restores the cursor below line 1:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        status_line(&harness),
        shown_line,
        "the status bar must name the line the cursor is drawn on:\n{}",
        harness.screen_to_string()
    );
}
