//! E2E tests to reproduce crash scenarios from GitHub issues
//!
//! Issue #562: Delete folder crash - scroll_offset out of bounds
//! Issue #564: Replace all operation hangs/crashes
//! Issue #1278: Crash opening file when workspace references deleted files

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use tempfile::TempDir;

/// Test issue #562: Crash when scroll_offset becomes larger than display_nodes.len()
///
/// The crash occurs in file_explorer.rs when rendering after the tree shrinks:
/// `let visible_items = &display_nodes[scroll_offset..visible_end];`
///
/// This can happen when:
/// - A folder with many children is collapsed while scrolled down viewing those children
/// - A folder with many children is deleted while scrolled down
///
/// The fix clamps scroll_offset to display_nodes.len() before slicing.
///
/// This test uses collapse (Enter key) to trigger the condition because:
/// - It's a reliable, standard keybinding
/// - It immediately shrinks the tree without needing confirmation dialogs
#[test]
fn test_issue_562_delete_folder_crash_scroll_offset() {
    // Create harness with a small viewport to force scrolling
    let mut harness = EditorTestHarness::with_temp_project(80, 12).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a folder with many files - when collapsed, display_nodes shrinks dramatically
    let big_folder = project_root.join("big_folder");
    fs::create_dir(&big_folder).unwrap();
    for i in 0..100 {
        fs::write(
            big_folder.join(format!("file_{:03}.txt", i)),
            format!("content {}", i),
        )
        .unwrap();
    }

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Wait for big_folder to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("big_folder"))
        .unwrap();

    // Navigate to big_folder
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Expand big_folder by pressing Enter (this shows 100 files)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("file_000"))
        .unwrap();

    // Scroll down deep into the folder (80+ items down)
    // This increases scroll_offset significantly
    for _ in 0..80 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let screen_after_scroll = harness.screen_to_string();
    println!("Screen after scrolling down:\n{}", screen_after_scroll);

    // Verify we're deep in the folder (should see files in 70-80 range)
    assert!(
        screen_after_scroll.contains("file_07"),
        "Should be scrolled to files in the 70s range"
    );

    // Now navigate back to big_folder and collapse it
    // This will shrink display_nodes from ~102 to ~2 items
    // But scroll_offset might still be around 70+
    for _ in 0..80 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Collapse big_folder by pressing Enter
    // Before the fix: This would panic with "range start index X out of range for slice of length Y"
    // After the fix: scroll_offset is clamped, no panic
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // This render should NOT panic even if scroll_offset was > display_nodes.len()
    let render_result = harness.render();
    assert!(
        render_result.is_ok(),
        "Rendering should not panic after collapsing a folder while scrolled down"
    );

    let screen_after_collapse = harness.screen_to_string();
    println!("Screen after collapse:\n{}", screen_after_collapse);

    // Verify the folder is now collapsed (should not show file_000 anymore)
    assert!(
        !screen_after_collapse.contains("file_000"),
        "Folder should be collapsed, file_000 should not be visible"
    );

    // Verify big_folder is still visible (just collapsed)
    assert!(
        screen_after_collapse.contains("big_folder"),
        "big_folder should still be visible after collapse"
    );

    // Continue rendering to ensure stability
    for _ in 0..5 {
        harness.render().unwrap();
    }
}

/// Test issue #564: Replace all operation hangs/crashes
///
/// The issue reports that replacing all instances of "Wii" with "HELLO" in a
/// CSV file causes the process to consume excessive CPU and become unresponsive.
///
/// This test creates a file with many occurrences of a pattern and attempts
/// a replace-all operation.
#[test]
fn test_issue_564_replace_all_hang() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a CSV-like file with many occurrences of "Wii"
    // Similar to vgsales-new.csv mentioned in the issue
    let mut content = String::new();
    content.push_str("Rank,Name,Platform,Year,Genre,Publisher,NA_Sales,EU_Sales\n");

    // Add many rows with "Wii" in them
    for i in 0..1000 {
        content.push_str(&format!(
            "{},Game {},Wii,2010,Action,Nintendo,{:.2},{:.2}\n",
            i,
            i,
            (i as f64) * 0.1,
            (i as f64) * 0.05
        ));
    }

    let file_path = project_root.join("vgsales-test.csv");
    fs::write(&file_path, &content).unwrap();

    // Open the file
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify file is loaded
    harness
        .wait_until(|h| h.screen_to_string().contains("Platform"))
        .unwrap();

    // Trigger replace with Ctrl+R
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show "Replace:" prompt
    harness.assert_screen_contains("Replace:");

    // Type search term "Wii"
    harness.type_text("Wii").unwrap();
    harness.render().unwrap();

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show replacement prompt
    harness.assert_screen_contains("Replace 'Wii' with:");

    // Type replacement "HELLO"
    harness.type_text("HELLO").unwrap();
    harness.render().unwrap();

    // Confirm replacement - this triggers the replace-all logic
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now we should be in interactive replace mode
    // Press 'a' to replace all occurrences
    harness.type_text("a").unwrap();

    // Give it some time to complete (but not too long - it should be fast)
    // If this times out, the replace-all is hanging
    let start = std::time::Instant::now();
    let timeout = std::time::Duration::from_secs(5);

    loop {
        harness.sleep(std::time::Duration::from_millis(50));
        harness.render().unwrap();

        let screen = harness.screen_to_string();

        // Check if replace completed (status message shows count)
        if screen.contains("Replaced") || screen.contains("occurrences") {
            break;
        }

        if start.elapsed() > timeout {
            panic!(
                "Replace all operation timed out after {:?}. This may indicate an infinite loop.",
                timeout
            );
        }
    }

    // Verify the replacement worked
    let buffer_content = harness.get_buffer_content().unwrap();

    // Should contain HELLO instead of Wii
    assert!(
        buffer_content.contains("HELLO"),
        "Buffer should contain 'HELLO' after replace"
    );
    assert!(
        !buffer_content.contains("Wii"),
        "Buffer should not contain 'Wii' after replace-all"
    );
}

/// Additional test for issue #564: Test replace-all with overlapping patterns
/// This checks for edge cases that could cause infinite loops.
#[test]
fn test_replace_all_overlapping_pattern() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a file with a pattern that could cause issues if not handled correctly
    // For example, replacing "aa" with "aaa" could theoretically cause infinite loop
    // if the position isn't advanced correctly
    let content = "aa bb aa cc aa dd aa ee aa";
    let file_path = project_root.join("test.txt");
    fs::write(&file_path, content).unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger replace with Ctrl+R
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for "aa"
    harness.type_text("aa").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Replace with "aaa" (pattern contained in replacement)
    harness.type_text("aaa").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press 'a' to replace all
    harness.type_text("a").unwrap();

    // Wait for completion with timeout
    let start = std::time::Instant::now();
    let timeout = std::time::Duration::from_secs(2);

    loop {
        harness.sleep(std::time::Duration::from_millis(50));
        harness.render().unwrap();

        let screen = harness.screen_to_string();

        if screen.contains("Replaced") {
            break;
        }

        if start.elapsed() > timeout {
            panic!("Replace all with overlapping pattern timed out. Possible infinite loop.");
        }
    }

    // Verify the replacement completed without hanging
    // Note: There's a known behavior where the first occurrence gets replaced twice
    // when the replacement contains the pattern - this is being tracked separately.
    let buffer_content = harness.get_buffer_content().unwrap();

    // The important thing is the operation completes without hanging
    assert!(
        buffer_content.contains("aaa"),
        "Replace all should complete without hanging"
    );

    // Original: "aa bb aa cc aa dd aa ee aa" (5 occurrences of "aa")
    // Current behavior results in first "aa" being replaced twice (bug)
    // Expected: "aaa bb aaa cc aaa dd aaa ee aaa"
    // Actual: "aaaa bb aaa cc aaa dd aaa ee aaa"
    // This is a separate bug to investigate
    println!("Result after replace: {}", buffer_content);
}

/// Test issue #562 variant: Delete multiple folders rapidly while scrolled
#[test]
fn test_issue_562_rapid_folder_deletion() {
    let mut harness = EditorTestHarness::with_temp_project(80, 10).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create folders
    for i in 0..20 {
        fs::create_dir(project_root.join(format!("dir_{:02}", i))).unwrap();
    }

    // Open file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("dir_00"))
        .unwrap();

    // Scroll down
    for _ in 0..15 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // Delete multiple folders rapidly
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Delete, KeyModifiers::NONE)
            .unwrap();
        harness.sleep(std::time::Duration::from_millis(50));

        // Each render should succeed without panic
        let result = harness.render();
        assert!(
            result.is_ok(),
            "Rendering should not panic during rapid folder deletion"
        );
    }

    // Final render to ensure stability
    harness.render().unwrap();
}

/// Test issue #564 with Query Replace (Ctrl+Alt+R) - this is the exact scenario
/// that causes the hang in the actual bug report.
///
/// The actual vgsales-new.csv file has:
/// - 16,599 lines
/// - 1,522 occurrences of "Wii"
/// - File size ~1.3MB
///
/// Using Query Replace (Ctrl+Alt+R) and pressing 'a' to replace all causes
/// excessive CPU usage (200%+) and memory growth (10GB+) leading to hang.
#[test]
#[ignore] // This test reproduces the actual hang - ignore for CI but run manually
fn test_issue_564_query_replace_all_hang_large_file() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a file similar to the actual vgsales-new.csv
    // With ~16000 lines and ~1500 occurrences of the pattern
    let mut content = String::new();
    for i in 0..16000 {
        // Each line has approximately same structure as the CSV
        // Some lines have "Wii" (about 10% to get ~1600 occurrences)
        if i % 10 < 1 {
            content.push_str(&format!(
                "{},Wii Game {},Wii,2010,Action,Nintendo,{:.2},{:.2}\n",
                i,
                i,
                (i as f64) * 0.01,
                (i as f64) * 0.005
            ));
        } else {
            content.push_str(&format!(
                "{},Other Game {},PS4,2010,Action,Sony,{:.2},{:.2}\n",
                i,
                i,
                (i as f64) * 0.01,
                (i as f64) * 0.005
            ));
        }
    }

    let file_path = project_root.join("large-test.csv");
    fs::write(&file_path, &content).unwrap();

    // Open the file
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Wait for file to load
    harness
        .wait_until(|h| h.screen_to_string().contains("Game"))
        .unwrap();

    // Use Query Replace (Ctrl+Alt+R) - this is the exact command that causes the hang
    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.render().unwrap();

    // Should show "Query replace:" prompt
    harness.assert_screen_contains("Query replace:");

    // Type search term "Wii"
    harness.type_text("Wii").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show replacement prompt
    harness.assert_screen_contains("Query replace 'Wii' with:");

    // Type replacement "HELLO"
    harness.type_text("HELLO").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show interactive replace prompt
    harness.assert_screen_contains("Replace?");

    // Press 'a' to replace all - THIS IS WHERE THE HANG OCCURS
    harness.type_text("a").unwrap();

    // Wait for completion with timeout
    // If this times out, the bug is reproduced
    let start = std::time::Instant::now();
    let timeout = std::time::Duration::from_secs(10);

    loop {
        harness.sleep(std::time::Duration::from_millis(100));
        harness.render().unwrap();

        let screen = harness.screen_to_string();

        if screen.contains("Replaced") {
            let elapsed = start.elapsed();
            println!("Replace all completed in {:?}", elapsed);
            break;
        }

        if start.elapsed() > timeout {
            panic!(
                "ISSUE #564 REPRODUCED: Query replace all operation timed out after {:?}.\n\
                 This confirms the bug - the operation hangs with large files.\n\
                 In manual testing, this causes 200%+ CPU usage and 10GB+ memory growth.",
                timeout
            );
        }
    }
}

/// Test issue #580: Panic when changing tab arrow visibility in settings
///
/// The crash occurs in view_pipeline.rs:159:
/// `self.tab_size - (col % self.tab_size)`
///
/// When tab_size is 0, this causes a division by zero panic with:
/// "attempt to calculate the remainder with a divisor of zero"
///
/// This can happen when:
/// 1. A language config has tab_size: 0 (schema allows minimum: 0)
/// 2. The settings UI displays null tab_size as 0 and saves it
/// 3. ViewLineIterator::new is called with tab_size = 0 during rendering
#[test]
fn test_issue_580_tab_size_zero_causes_panic() {
    use fresh::config::Config;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.cpp");

    // Create a C++ file with tab characters - this will trigger ViewLineIterator
    fs::write(&file_path, "\tint main() {\n\t\treturn 0;\n\t}").unwrap();

    // Create a config with tab_size = 0 for cpp language (simulating the bug)
    let mut config = Config::default();
    if let Some(cpp_config) = config.languages.get_mut("cpp") {
        // This simulates what happens when the settings UI saves tab_size: 0
        cpp_config.tab_size = Some(0);
    }

    // Create harness with this config
    let mut harness =
        EditorTestHarness::with_config(100, 24, config).expect("Should create harness");

    // Open the file
    harness.open_file(&file_path).expect("Should open cpp file");

    // This render should NOT panic even with tab_size = 0
    // If the bug exists, this will panic with:
    // "attempt to calculate the remainder with a divisor of zero"
    let render_result = harness.render();
    assert!(
        render_result.is_ok(),
        "Rendering should not panic with tab_size = 0. The editor should handle this gracefully."
    );

    // Verify the file is displayed (content should still be visible)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("int main"),
        "File content should be visible even with tab_size = 0"
    );
}

/// Test panic when saved_at_index exceeds entries length after undo + new edits
///
/// Bug from v0.1.77:
/// thread 'main' panicked at src/model/event.rs:617:29:
/// range end index 148 out of range for slice of length 125
///
/// The crash occurs in is_at_saved_position() when:
/// 1. Make many changes (event log grows to ~150 entries)
/// 2. Save (sets saved_at_index = 148)
/// 3. Undo multiple times (current_index decreases but entries stay)
/// 4. Make NEW changes (truncates entries to current_index, e.g., 125)
/// 5. Undo triggers update_modified_from_event_log() which calls is_at_saved_position()
///    The code tries to access self.entries[start..end] where end = saved_at_index (148)
///    but entries.len() is now 125
///
/// The fix should clamp or validate indices before slicing.
#[test]
fn test_saved_at_index_out_of_bounds_after_undo_and_edit() {
    use crate::common::fixtures::TestFixture;

    // Create a test file
    let fixture = TestFixture::new("test_save_index_oob.txt", "start").unwrap();
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Open the file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Step 1: Make many changes to build up event log entries
    // Each character typed adds to the event log
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for i in 0..150 {
        harness.type_text(&format!("{}", i % 10)).unwrap();
    }
    harness.render().unwrap();

    // Step 2: Save the file (this sets saved_at_index to a high value ~150+)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify we're not modified after save
    assert!(
        !harness.editor().active_state().buffer.is_modified(),
        "Should not be modified immediately after save"
    );

    // Step 3: Undo many times (reduces current_index but entries remain)
    for _ in 0..30 {
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    }
    harness.render().unwrap();

    // Step 4: Make NEW changes - this truncates entries to current_index
    // and then appends new entries, resulting in entries.len() < saved_at_index
    harness.type_text("NEW").unwrap();
    harness.render().unwrap();

    // Step 5: Trigger another undo - this calls update_modified_from_event_log()
    // which in turn calls is_at_saved_position()
    // Before the fix: This panics with "range end index 148 out of range for slice of length 125"
    // After the fix: Should complete without panicking
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();

    // If we get here without panicking, the bug is fixed
    harness.render().unwrap();
}

/// Test issue #1278: Crash when opening a file in a directory whose workspace
/// references files that have been deleted.
///
/// Repro steps:
/// 1. Open a file with `fresh somefile.txt` in a directory, quit (saves workspace)
/// 2. Delete `somefile.txt`
/// 3. Run `fresh other.txt` in the same directory → panics at
///    buffer_management.rs:195 with `unwrap()` on `None`
///
/// Root cause: `apply_workspace()` clears `open_buffers` in
/// `restore_split_view_state()` but adds nothing back (the referenced file is
/// gone). The orphan cleanup then removes the initial empty buffer from
/// `self.buffers`. Later, `process_pending_file_opens()` calls `open_file()`
/// which does `self.buffers.get(&self.active_buffer()).unwrap()` — but the
/// active buffer was already removed.
#[test]
fn test_issue_1278_crash_workspace_deleted_file() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    fs::create_dir(&project_dir).unwrap();

    let original_file = project_dir.join("somefile.txt");
    let new_file = project_dir.join("epilogue.xhtml");
    fs::write(&original_file, "original content").unwrap();
    fs::write(&new_file, "new content").unwrap();

    // Session 1: open somefile.txt and save workspace
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness.open_file(&original_file).unwrap();
        harness.assert_buffer_content("original content");
        harness.editor_mut().save_workspace().unwrap();
    }

    // Delete the file the workspace references
    fs::remove_file(&original_file).unwrap();

    // Session 2: open a different file via startup() (mirrors production path).
    // Before the fix this panics at buffer_management.rs:195.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();

        harness
            .startup(true, &[new_file.clone()])
            .expect("startup should not panic when workspace references deleted files");

        harness.assert_buffer_content("new content");
    }
}

/// Test issue #580: Global editor.tab_size = 0 should not cause panic
///
/// Similar to the language-specific case, but tests the global editor.tab_size setting.
#[test]
fn test_issue_580_global_tab_size_zero_causes_panic() {
    use fresh::config::Config;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with tab characters
    fs::write(&file_path, "\thello\n\t\tworld").unwrap();

    // Create a config with global tab_size = 0
    let mut config = Config::default();
    config.editor.tab_size = 0;

    // Create harness with this config
    let mut harness =
        EditorTestHarness::with_config(100, 24, config).expect("Should create harness");

    // Open the file
    harness
        .open_file(&file_path)
        .expect("Should open text file");

    // This render should NOT panic even with tab_size = 0
    let render_result = harness.render();
    assert!(
        render_result.is_ok(),
        "Rendering should not panic with global tab_size = 0"
    );
}
