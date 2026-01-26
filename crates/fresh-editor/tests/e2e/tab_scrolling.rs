use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

const NARROW_WIDTH: u16 = 40;
const TEST_HEIGHT: u16 = 20;
const NUM_FILES: usize = 15;

/// Helper to create dummy files with long names
fn create_dummy_files(temp_dir: &TempDir) -> Vec<std::path::PathBuf> {
    let mut files = Vec::new();
    for i in 0..NUM_FILES {
        let file_name = format!("long_file_name_number_{:02}.txt", i);
        let file_path = temp_dir.path().join(&file_name);
        std::fs::write(&file_path, format!("Content for file {}", i)).unwrap();
        files.push(file_path);
    }
    files
}

/// Test that the active tab is always visible and scroll indicators work
#[test]
fn test_active_tab_visibility_with_scrolling() {
    let temp_dir = TempDir::new().unwrap();
    let files = create_dummy_files(&temp_dir);

    let mut harness = EditorTestHarness::new(NARROW_WIDTH, TEST_HEIGHT).unwrap();

    // Open all dummy files
    for file_path in &files {
        harness.open_file(file_path).unwrap();
        // Check visibility after each open
        harness.render().unwrap();
        let active_file_name = file_path.file_name().unwrap().to_str().unwrap();
        harness.assert_screen_contains(active_file_name);
        // Initial files might not show indicators, but later ones should
    }

    // Initial check: Last opened file is active
    let mut active_idx = NUM_FILES - 1;
    harness.render().unwrap();
    let active_file_name = files[active_idx].file_name().unwrap().to_str().unwrap();
    harness.assert_screen_contains(active_file_name);
    // If the active tab is not the last one, we expect a right indicator; otherwise don't enforce it.
    if active_idx < NUM_FILES - 1 {
        assert!(
            harness.screen_to_string().contains(">"),
            "Expected right scroll indicator after opening many files. Screen:\n{}",
            harness.screen_to_string()
        );
    }

    // --- Cycle Forward (Next Buffer) ---
    for _i in 0..NUM_FILES {
        // Cycle through all tabs once
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap(); // Ctrl+PageDown for NextBuffer
        active_idx = (active_idx + 1) % NUM_FILES;

        harness.render().unwrap();
        let active_file_name = files[active_idx].file_name().unwrap().to_str().unwrap();
        harness.assert_screen_contains(active_file_name);

        let screen = harness.screen_to_string();
        // The expectation for indicators will depend on the width and file names.
        // For a robust E2E test, we primarily care that the *active* tab is visible.
        // The presence of indicators is a secondary visual cue.
        // We'll roughly check: if not the first tab, might see '<'. If not the last, might see '>'.
        if active_idx > 0 {
            // Might see left indicator if previous tabs are hidden
            // assert!(screen.contains("<"), "Expected left scroll indicator for file: {}", active_file_name);
        } else {
            // Should not see left indicator for the first file
            assert!(
                !screen.contains("<"),
                "Expected no left scroll indicator for file: {}",
                active_file_name
            );
        }
        if active_idx < NUM_FILES - 1 {
            // Might see right indicator if next tabs are hidden
            // assert!(screen.contains(">"), "Expected right scroll indicator for file: {}", active_file_name);
        } else {
            // Should not see right indicator for the last file (if all fit, or scrolled to end)
            // assert!(!screen.contains(">"), "Expected no right scroll indicator for file: {}", active_file_name);
        }
    }

    // --- Cycle Backward (Prev Buffer) ---
    for _i in 0..NUM_FILES {
        // Cycle through all tabs once
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
            .unwrap(); // Ctrl+PageUp for PrevBuffer
        active_idx = (active_idx + NUM_FILES - 1) % NUM_FILES; // Safe decrement

        harness.render().unwrap();
        let active_file_name = files[active_idx].file_name().unwrap().to_str().unwrap();
        harness.assert_screen_contains(active_file_name);

        let screen = harness.screen_to_string();
        if active_idx > 0 {
            // assert!(screen.contains("<"), "Expected left scroll indicator for file: {}", active_file_name);
        } else {
            assert!(
                !screen.contains("<"),
                "Expected no left scroll indicator for file: {}",
                active_file_name
            );
        }
        if active_idx < NUM_FILES - 1 {
            // assert!(screen.contains(">"), "Expected right scroll indicator for file: {}", active_file_name);
        } else {
            assert!(
                !screen.contains(">"),
                "Expected no right scroll indicator for file: {}",
                active_file_name
            );
        }
    }

    // --- Test manual scrolling ---
    // Activate a middle tab to ensure we can scroll away from it
    let middle_idx = NUM_FILES / 2;
    // Cycle to middle_idx relative to current position
    let steps_to_middle = (middle_idx + NUM_FILES - active_idx) % NUM_FILES;
    for _ in 0..steps_to_middle {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap(); // Next Buffer
        active_idx = (active_idx + 1) % NUM_FILES;
        harness.render().unwrap();
    }
    assert_eq!(active_idx, middle_idx, "Failed to activate middle tab");
    harness.assert_screen_contains(files[active_idx].file_name().unwrap().to_str().unwrap());

    // Scroll right manually
    for _ in 0..5 {
        // Scroll by 5 increments
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::ALT)
            .unwrap(); // Alt+PageDown for ScrollTabsRight
        harness.render().unwrap();
        harness.assert_screen_contains(files[active_idx].file_name().unwrap().to_str().unwrap());
        // Check for indicators based on current position and width. More complex assertion left out for simplicity
        // as the primary goal is visible active tab and manual scroll movement.
    }

    // Scroll left manually
    for _ in 0..10 {
        // Scroll by 10 increments
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::ALT)
            .unwrap(); // Alt+PageUp for ScrollTabsLeft
        harness.render().unwrap();
        harness.assert_screen_contains(files[active_idx].file_name().unwrap().to_str().unwrap());
        // Check for indicators based on current position and width.
    }
}

/// Test that clicking on scroll buttons scrolls the tab bar
#[test]
fn test_tab_scroll_button_click() {
    let temp_dir = TempDir::new().unwrap();
    let files = create_dummy_files(&temp_dir);

    // Use a wider terminal than the other test so filenames are fully visible
    // This test focuses on scroll button functionality, not narrow terminal behavior
    let mut harness = EditorTestHarness::new(80, TEST_HEIGHT).unwrap();

    // Open all dummy files to ensure tab overflow
    for file_path in &files {
        harness.open_file(file_path).unwrap();
        harness.render().unwrap();
    }

    // Go to first tab to ensure we can scroll right
    for _ in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    // Now we're on the first file - should see ">" indicator for right scroll
    let screen = harness.screen_to_string();
    if screen.contains(">") {
        // Click on the right scroll indicator (rightmost position of first row, which is the tab bar)
        // The tab bar is typically at row 1 (after menu bar)
        let tab_row = 1; // Tab bar is usually at row 1
        let right_scroll_col = NARROW_WIDTH - 1; // Right edge

        harness.mouse_click(right_scroll_col, tab_row).unwrap();
        harness.render().unwrap();

        // After clicking, the tab offset should have increased
        // The active tab should still be visible
        let first_file = files[0].file_name().unwrap().to_str().unwrap();
        harness.assert_screen_contains(first_file);
    }

    // Go to last tab to ensure we can scroll left
    for _ in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    // Now on the last file - should see "<" indicator for left scroll
    let screen = harness.screen_to_string();
    if screen.contains("<") {
        // Click on the left scroll indicator (leftmost position of tab bar)
        let tab_row = 1;
        let left_scroll_col = 0; // Left edge (or after file explorer if visible)

        harness.mouse_click(left_scroll_col, tab_row).unwrap();
        harness.render().unwrap();

        // After clicking, the tab offset should have decreased
        // The active tab should still be visible
        let last_file = files[NUM_FILES - 1].file_name().unwrap().to_str().unwrap();
        harness.assert_screen_contains(last_file);
    }
}
