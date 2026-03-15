use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Issue #1261: Deleting a newline below the cursor with Delete key
/// incorrectly decrements the status bar line number.
///
/// Steps: open a 5-line file, move to line 3, press End then Delete.
/// The cursor stays on (what is still) line 3 but the status bar
/// incorrectly shows "Ln 2".
#[test]
fn test_delete_forward_does_not_decrement_status_bar_line_number() {
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("test.txt");
    std::fs::write(&file_path, "line1\nline2\nline3\nline4\nline5\n").unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move cursor to line 3 (Down twice), then to end of line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we're on line 3
    let status = harness.get_status_bar();
    assert!(
        status.contains("Ln 3"),
        "Before delete, status bar should show Ln 3, got: {status}"
    );

    // Press Delete to merge line 4 into line 3
    // This deletes the newline AFTER the cursor - cursor stays on line 3
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Buffer should now have merged line3 and line4
    harness.assert_buffer_content("line1\nline2\nline3line4\nline5\n");

    // Status bar should still show Ln 3 (cursor didn't move to a different line)
    let status = harness.get_status_bar();
    assert!(
        status.contains("Ln 3"),
        "After Delete at end of line 3, status bar should still show Ln 3, got: {status}"
    );
}

/// Issue #1262: Relative line numbers use byte offset instead of line number,
/// producing wildly wrong values.
///
/// With relative_line_numbers enabled and cursor on line 3 of a 5-line file,
/// lines 1 and 2 should show "2" and "1" (distance from cursor), but
/// instead show values based on byte offsets.
#[test]
fn test_relative_line_numbers_show_correct_distances() {
    // Enable relative line numbers
    let mut config = fresh::config::Config::default();
    config.editor.relative_line_numbers = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 24, config).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("test.txt");
    std::fs::write(&file_path, "line1\nline2\nline3\nline4\nline5\n").unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move cursor to line 3
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with relative line numbers (cursor on line 3):\n{screen}");

    // Cursor line should show absolute line number 3
    // Lines above should show relative distance: line1 -> 2, line2 -> 1
    // Lines below should show relative distance: line4 -> 1, line5 -> 2

    // The cursor line (line 3) should show "3" (absolute, 1-indexed)
    // Check that the screen contains the expected relative pattern:
    //   2 | line1
    //   1 | line2
    //   3 | line3   <- absolute for cursor line
    //   1 | line4
    //   2 | line5

    // Verify that line1 shows relative distance 2 (not some large byte-based number)
    // and line2 shows relative distance 1
    // We check for the pattern in the gutter area
    assert!(
        screen.contains("   2 │ line1") || screen.contains("   2│ line1"),
        "Line 1 should show relative distance 2 from cursor on line 3, screen:\n{screen}"
    );
    assert!(
        screen.contains("   1 │ line2") || screen.contains("   1│ line2"),
        "Line 2 should show relative distance 1 from cursor on line 3, screen:\n{screen}"
    );
    assert!(
        screen.contains("   3 │ line3") || screen.contains("   3│ line3"),
        "Cursor line (line 3) should show absolute line number 3, screen:\n{screen}"
    );
    assert!(
        screen.contains("   1 │ line4") || screen.contains("   1│ line4"),
        "Line 4 should show relative distance 1 from cursor on line 3, screen:\n{screen}"
    );
    assert!(
        screen.contains("   2 │ line5") || screen.contains("   2│ line5"),
        "Line 5 should show relative distance 2 from cursor on line 3, screen:\n{screen}"
    );
}
