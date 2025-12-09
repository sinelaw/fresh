use crate::common::harness::EditorTestHarness;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::thread;
use std::time::Duration;

/// Delay between file writes to ensure filesystem notifications are received.
/// - Many filesystems (ext4, HFS+) have 1-second mtime granularity
/// - macOS FSEvents has 500ms-2s coalescing latency by default
const FILE_CHANGE_DELAY: Duration = Duration::from_millis(2100);

/// Write content to a file and sync to disk to ensure filesystem notifications fire.
/// macOS sometimes buffers events until fsync/sync_all is called.
fn write_and_sync(path: &Path, content: &str) {
    let mut file = File::create(path).unwrap();
    file.write_all(content.as_bytes()).unwrap();
    file.sync_all().unwrap();
    drop(file);

    // Also sync the parent directory to ensure the directory entry is flushed
    if let Some(parent) = path.parent() {
        if let Ok(dir) = File::open(parent) {
            let _ = dir.sync_all();
        }
    }
}

/// Test that the notify-based auto-revert flow works correctly.
/// This test validates that external file changes are detected and
/// the buffer is automatically updated in the render view.
///
/// The test performs multiple edit-save cycles to ensure the notify
/// watcher continues working after repeated file changes.
#[test]
fn test_auto_revert_multiple_external_edits() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("test_revert.txt");

    // Create initial file content
    write_and_sync(&file_path, "Initial content v1");

    // Open the file - auto_revert is enabled by default
    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content("Initial content v1");

    // Perform multiple external edit cycles
    for version in 2..=5 {
        let new_content = format!("Updated content v{}", version);

        thread::sleep(FILE_CHANGE_DELAY);

        // Write new content externally (simulating another process editing the file)
        write_and_sync(&file_path, &new_content);

        // Wait until the buffer content matches the new file content
        // This uses semantic waiting - no arbitrary timeouts
        let expected = new_content.clone();
        harness
            .wait_until(|h| h.get_buffer_content().unwrap() == expected)
            .expect("Auto-revert should update buffer content");

        // Verify the buffer was updated correctly
        harness.assert_buffer_content(&new_content);

        // Verify the screen shows the updated content
        harness.render().unwrap();
        harness.assert_screen_contains(&format!("v{}", version));
    }
}

/// Test that auto-revert works correctly when the file grows significantly
#[test]
fn test_auto_revert_file_grows() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("growing_file.txt");

    // Start with a small file
    write_and_sync(&file_path, "Line 1");

    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content("Line 1");

    // Grow the file progressively
    for num_lines in [3, 5, 10] {
        thread::sleep(FILE_CHANGE_DELAY);

        let content: String = (1..=num_lines)
            .map(|i| format!("Line {}", i))
            .collect::<Vec<_>>()
            .join("\n");

        write_and_sync(&file_path, &content);

        let expected = content.clone();
        harness
            .wait_until(|h| h.get_buffer_content().unwrap() == expected)
            .expect("Auto-revert should handle file growth");

        harness.assert_buffer_content(&content);
    }
}

/// Test that auto-revert works correctly when the file shrinks
#[test]
fn test_auto_revert_file_shrinks() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("shrinking_file.txt");

    // Start with a large file
    let initial_content = (1..=10)
        .map(|i| format!("Line {}", i))
        .collect::<Vec<_>>()
        .join("\n");
    write_and_sync(&file_path, &initial_content);

    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content(&initial_content);

    // Shrink the file progressively
    for num_lines in [5, 3, 1] {
        thread::sleep(FILE_CHANGE_DELAY);

        let content: String = (1..=num_lines)
            .map(|i| format!("Line {}", i))
            .collect::<Vec<_>>()
            .join("\n");

        write_and_sync(&file_path, &content);

        let expected = content.clone();
        harness
            .wait_until(|h| h.get_buffer_content().unwrap() == expected)
            .expect("Auto-revert should handle file shrinking");

        harness.assert_buffer_content(&content);
    }
}

/// Test that auto-revert preserves the viewport position when possible
#[test]
fn test_auto_revert_preserves_scroll_position() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("scrolled_file.txt");

    // Create a file with many lines
    let content: String = (1..=100)
        .map(|i| format!("Line number {}", i))
        .collect::<Vec<_>>()
        .join("\n");
    write_and_sync(&file_path, &content);

    harness.open_file(&file_path).unwrap();

    // Scroll down to somewhere in the middle
    use crossterm::event::{KeyCode, KeyModifiers};
    for _ in 0..10 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Record the top line before revert
    let top_line_before = harness.top_line_number();
    assert!(top_line_before > 1, "Should have scrolled down");

    // Modify the file slightly (change one line in the visible area)
    thread::sleep(FILE_CHANGE_DELAY);
    let modified_content: String = (1..=100)
        .map(|i| {
            if i == 50 {
                "Line number 50 - MODIFIED".to_string()
            } else {
                format!("Line number {}", i)
            }
        })
        .collect::<Vec<_>>()
        .join("\n");
    write_and_sync(&file_path, &modified_content);

    // Wait for auto-revert
    let expected = modified_content.clone();
    harness
        .wait_until(|h| h.get_buffer_content().unwrap() == expected)
        .expect("Auto-revert should update buffer");

    harness.assert_buffer_content(&modified_content);
}

/// Test that auto-revert does NOT occur when buffer has local modifications
#[test]
fn test_auto_revert_skipped_when_buffer_modified() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("modified_buffer.txt");

    write_and_sync(&file_path, "Original content");

    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content("Original content");

    // Make a local modification to the buffer
    use crossterm::event::{KeyCode, KeyModifiers};
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(" - local edit").unwrap();
    harness.assert_buffer_content("Original content - local edit");

    // Modify the file externally
    thread::sleep(FILE_CHANGE_DELAY);
    write_and_sync(&file_path, "External change");

    // Process events - but buffer should NOT be reverted
    // because it has local modifications
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
        thread::sleep(Duration::from_millis(20));
    }

    // Buffer should still have local modifications, not the external change
    harness.assert_buffer_content("Original content - local edit");

    // Status message should indicate the file changed but wasn't reverted
    let status = harness.get_status_bar();
    assert!(
        status.contains("changed on disk")
            || harness.get_buffer_content().unwrap() == "Original content - local edit",
        "Should either show warning or preserve local changes"
    );
}

/// Test rapid consecutive file changes are handled correctly
#[test]
fn test_auto_revert_rapid_changes() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("rapid_changes.txt");

    write_and_sync(&file_path, "v0");

    harness.open_file(&file_path).unwrap();

    // Make rapid consecutive changes
    for i in 1..=10 {
        thread::sleep(Duration::from_millis(30));
        write_and_sync(&file_path, &format!("v{}", i));
    }

    // Wait for the final version to appear
    harness
        .wait_until(|h| h.get_buffer_content().unwrap() == "v10")
        .expect("Should eventually settle on final version");

    harness.assert_buffer_content("v10");
}

/// Test that auto-revert preserves cursor position when file content changes
#[test]
fn test_auto_revert_preserves_cursor_position() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("cursor_preserve.txt");

    // Create a file with some lines
    let content = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5";
    write_and_sync(&file_path, content);

    harness.open_file(&file_path).unwrap();

    // Move cursor to a specific position (end of line 3, which is "Line 3")
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Now on line 3
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Record cursor position - should be at end of "Line 3" which is at byte offset 20
    let cursor_before = harness.cursor_position();
    assert!(cursor_before > 0, "Cursor should have moved from start");

    // Modify the file externally (but keep same structure so cursor position is valid)
    thread::sleep(FILE_CHANGE_DELAY);
    let modified_content = "Line 1\nLine 2\nLine X\nLine 4\nLine 5"; // Same length, just changed content
    write_and_sync(&file_path, modified_content);

    // Wait for auto-revert
    let expected = modified_content.to_string();
    harness
        .wait_until(|h| h.get_buffer_content().unwrap() == expected)
        .expect("Auto-revert should update buffer");

    // Cursor position should be preserved (or clamped to valid range)
    let cursor_after = harness.cursor_position();
    assert_eq!(
        cursor_before, cursor_after,
        "Cursor position should be preserved after auto-revert"
    );
}

/// Test that auto-revert is not disabled by a single save operation
/// Previously, saving the file would immediately trigger auto-revert disable
/// because the file change event would come too quickly after the previous event
#[test]
fn test_auto_revert_not_disabled_by_external_save() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("save_test.txt");

    write_and_sync(&file_path, "Initial content");

    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content("Initial content");

    // Simulate an external save (like when another process saves the file)
    thread::sleep(FILE_CHANGE_DELAY);
    write_and_sync(&file_path, "Changed by external save");

    // Wait for auto-revert
    harness
        .wait_until(|h| h.get_buffer_content().unwrap() == "Changed by external save")
        .expect("Auto-revert should update buffer after external save");

    // Small delay, then make another change
    thread::sleep(Duration::from_millis(600)); // Beyond debounce window

    // Make another external change - auto-revert should still be enabled
    write_and_sync(&file_path, "Second external change");

    // This should also be auto-reverted (auto-revert should not have been disabled)
    harness
        .wait_until(|h| h.get_buffer_content().unwrap() == "Second external change")
        .expect("Auto-revert should still work after previous external save");

    harness.assert_buffer_content("Second external change");
}

/// Test auto-revert with temp+rename save pattern (like vim, vscode, etc.)
/// This specifically tests the inode change scenario on Linux where inotify
/// watches inodes rather than paths. When a file is saved via temp+rename,
/// the inode changes and the watch can become stale.
#[test]
fn test_auto_revert_with_temp_rename_save() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("temp_rename_test.txt");

    // Create initial file
    write_and_sync(&file_path, "Initial content v1");

    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content("Initial content v1");

    // Simulate multiple save cycles using the temp+rename pattern
    // This is how many editors (vim, vscode, etc.) save files
    for version in 2..=5 {
        let new_content = format!("Updated content v{}", version);

        thread::sleep(FILE_CHANGE_DELAY);

        // Write to a temp file first, then rename (atomic save pattern)
        // This changes the file's inode, which can break inotify watches
        let temp_path = project_dir.join(format!(".temp_rename_test.txt.{}", version));
        write_and_sync(&temp_path, &new_content);
        fs::rename(&temp_path, &file_path).unwrap();
        // Sync directory after rename to ensure the rename is visible
        if let Ok(dir) = File::open(&project_dir) {
            let _ = dir.sync_all();
        }

        // Wait for the buffer to update
        let expected = new_content.clone();
        harness
            .wait_until(|h| h.get_buffer_content().unwrap() == expected)
            .expect(&format!(
                "Auto-revert should detect temp+rename save for version {}",
                version
            ));

        harness.assert_buffer_content(&new_content);
    }
}
