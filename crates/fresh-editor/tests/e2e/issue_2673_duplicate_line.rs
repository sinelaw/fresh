//! E2E coverage for issue #2673: the `duplicate_line` action existed but had
//! no default keybinding. `Ctrl+Shift+D` (VS Code family) should duplicate the
//! current line. Drives the real key path and asserts on rendered output.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

#[test]
fn test_ctrl_shift_d_duplicates_current_line() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("dup.txt");
    std::fs::write(&file_path, "alpha\nbeta\ngamma\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Cursor starts on line 1 ("alpha"). Duplicate it.
    harness.assert_buffer_content("alpha\nbeta\ngamma\n");
    harness
        .send_key(
            KeyCode::Char('d'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // The line is duplicated below the original.
    harness.assert_buffer_content("alpha\nalpha\nbeta\ngamma\n");

    // And it is visible on screen: two "alpha" lines now render.
    let screen = harness.screen_to_string();
    let alpha_lines = screen.lines().filter(|l| l.contains("alpha")).count();
    assert!(
        alpha_lines >= 2,
        "expected two 'alpha' lines after duplicate; screen:\n{screen}"
    );
}

#[test]
fn test_ctrl_shift_d_duplicates_selected_lines() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("dup_sel.txt");
    std::fs::write(&file_path, "one\ntwo\nthree\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Select the first two full lines: two Shift+Down presses move the
    // caret to the start of line 3, so the selection spans lines 1–2.
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(
            KeyCode::Char('d'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.render().unwrap();

    // The two selected lines are duplicated as a block below.
    harness.assert_buffer_content("one\ntwo\none\ntwo\nthree\n");
}
