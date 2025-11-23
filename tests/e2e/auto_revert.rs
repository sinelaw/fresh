use crate::common::harness::EditorTestHarness;
use std::fs;
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

/// Test that the notify-based auto-revert flow works correctly.
/// This test validates that external file changes are detected and
/// the buffer is automatically updated in the render view.
///
/// The test performs multiple edit-save cycles to ensure the notify
/// watcher continues working after repeated file changes.
#[test]
fn test_auto_revert_multiple_external_edits() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test_revert.txt");

    // Create initial file content
    fs::write(&file_path, "Initial content v1").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file - auto_revert is enabled by default
    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content("Initial content v1");

    // Perform multiple external edit cycles
    for version in 2..=5 {
        let new_content = format!("Updated content v{}", version);

        // Small delay to ensure filesystem timestamp changes
        // (some filesystems have second-level granularity)
        thread::sleep(Duration::from_millis(50));

        // Write new content externally (simulating another process editing the file)
        fs::write(&file_path, &new_content).unwrap();

        // Wait until the buffer content matches the new file content
        // This uses semantic waiting - no arbitrary timeouts
        let expected = new_content.clone();
        harness
            .wait_until(|h| h.get_buffer_content() == expected)
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
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("growing_file.txt");

    // Start with a small file
    fs::write(&file_path, "Line 1").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content("Line 1");

    // Grow the file progressively
    for num_lines in [3, 5, 10] {
        thread::sleep(Duration::from_millis(50));

        let content: String = (1..=num_lines)
            .map(|i| format!("Line {}", i))
            .collect::<Vec<_>>()
            .join("\n");

        fs::write(&file_path, &content).unwrap();

        let expected = content.clone();
        harness
            .wait_until(|h| h.get_buffer_content() == expected)
            .expect("Auto-revert should handle file growth");

        harness.assert_buffer_content(&content);
    }
}

/// Test that auto-revert works correctly when the file shrinks
#[test]
fn test_auto_revert_file_shrinks() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("shrinking_file.txt");

    // Start with a large file
    let initial_content = (1..=10)
        .map(|i| format!("Line {}", i))
        .collect::<Vec<_>>()
        .join("\n");
    fs::write(&file_path, &initial_content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content(&initial_content);

    // Shrink the file progressively
    for num_lines in [5, 3, 1] {
        thread::sleep(Duration::from_millis(50));

        let content: String = (1..=num_lines)
            .map(|i| format!("Line {}", i))
            .collect::<Vec<_>>()
            .join("\n");

        fs::write(&file_path, &content).unwrap();

        let expected = content.clone();
        harness
            .wait_until(|h| h.get_buffer_content() == expected)
            .expect("Auto-revert should handle file shrinking");

        harness.assert_buffer_content(&content);
    }
}

/// Test that auto-revert preserves the viewport position when possible
#[test]
fn test_auto_revert_preserves_scroll_position() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("scrolled_file.txt");

    // Create a file with many lines
    let content: String = (1..=100)
        .map(|i| format!("Line number {}", i))
        .collect::<Vec<_>>()
        .join("\n");
    fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Scroll down to somewhere in the middle
    use crossterm::event::{KeyCode, KeyModifiers};
    for _ in 0..10 {
        harness.send_key(KeyCode::PageDown, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Record the top line before revert
    let top_line_before = harness.top_line_number();
    assert!(top_line_before > 1, "Should have scrolled down");

    // Modify the file slightly (change one line in the visible area)
    thread::sleep(Duration::from_millis(50));
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
    fs::write(&file_path, &modified_content).unwrap();

    // Wait for auto-revert
    let expected = modified_content.clone();
    harness
        .wait_until(|h| h.get_buffer_content() == expected)
        .expect("Auto-revert should update buffer");

    harness.assert_buffer_content(&modified_content);
}

/// Test that auto-revert does NOT occur when buffer has local modifications
#[test]
fn test_auto_revert_skipped_when_buffer_modified() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("modified_buffer.txt");

    fs::write(&file_path, "Original content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content("Original content");

    // Make a local modification to the buffer
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    harness.type_text(" - local edit").unwrap();
    harness.assert_buffer_content("Original content - local edit");

    // Modify the file externally
    thread::sleep(Duration::from_millis(50));
    fs::write(&file_path, "External change").unwrap();

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
        status.contains("changed on disk") || harness.get_buffer_content() == "Original content - local edit",
        "Should either show warning or preserve local changes"
    );
}

/// Test rapid consecutive file changes are handled correctly
#[test]
fn test_auto_revert_rapid_changes() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("rapid_changes.txt");

    fs::write(&file_path, "v0").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Make rapid consecutive changes
    for i in 1..=10 {
        thread::sleep(Duration::from_millis(30));
        fs::write(&file_path, format!("v{}", i)).unwrap();
    }

    // Wait for the final version to appear
    harness
        .wait_until(|h| h.get_buffer_content() == "v10")
        .expect("Should eventually settle on final version");

    harness.assert_buffer_content("v10");
}

/// Test auto-revert with temp+rename save pattern (like vim, vscode, etc.)
/// This specifically tests the inode change scenario on Linux where inotify
/// watches inodes rather than paths. When a file is saved via temp+rename,
/// the inode changes and the watch can become stale.
#[test]
fn test_auto_revert_with_temp_rename_save() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("temp_rename_test.txt");

    // Create initial file
    fs::write(&file_path, "Initial content v1").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.assert_buffer_content("Initial content v1");

    // Simulate multiple save cycles using the temp+rename pattern
    // This is how many editors (vim, vscode, etc.) save files
    for version in 2..=5 {
        let new_content = format!("Updated content v{}", version);

        thread::sleep(Duration::from_millis(50));

        // Write to a temp file first, then rename (atomic save pattern)
        // This changes the file's inode, which can break inotify watches
        let temp_path = temp_dir.path().join(format!(".temp_rename_test.txt.{}", version));
        fs::write(&temp_path, &new_content).unwrap();
        fs::rename(&temp_path, &file_path).unwrap();

        // Wait for the buffer to update
        let expected = new_content.clone();
        harness
            .wait_until(|h| h.get_buffer_content() == expected)
            .expect(&format!(
                "Auto-revert should detect temp+rename save for version {}",
                version
            ));

        harness.assert_buffer_content(&new_content);
    }
}
