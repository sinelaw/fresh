use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test Live Grep plugin - basic search and preview functionality
#[test]
#[ignore = "flaky test - times out intermittently"]
fn test_live_grep_basic_search() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create test files with searchable content
    let file1_content = "fn main() {\n    println!(\"Hello, world!\");\n}\n";
    let file2_content = "fn helper() {\n    println!(\"Helper function\");\n}\n";
    let file3_content = "// This file contains UNIQUE_MARKER for testing\nlet x = 42;\n";

    fs::write(project_root.join("main.rs"), file1_content).unwrap();
    fs::write(project_root.join("helper.rs"), file2_content).unwrap();
    fs::write(project_root.join("test.rs"), file3_content).unwrap();

    // Create a file to open initially
    let fixture = TestFixture::new("initial.txt", "Initial file content\n").unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open the initial file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Open command palette and find Live Grep
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep to appear in palette (plugin loaded)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Live Grep") || s.contains("Find in Files")
        })
        .unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now we should be in the live grep prompt
    // Type a search query
    harness.type_text("UNIQUE_MARKER").unwrap();

    // Wait for search results to appear
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("test.rs") || s.contains("UNIQUE_MARKER")
        })
        .unwrap();

    // Press Escape to cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we're back to normal state
    let final_screen = harness.screen_to_string();
    assert!(
        final_screen.contains("Initial file content")
            || final_screen.contains("Live grep cancelled"),
        "Should return to normal state after ESC. Got:\n{}",
        final_screen
    );
}

/// Test Live Grep - selecting a result opens the file
#[test]
#[ignore = "flaky test - times out intermittently"]
fn test_live_grep_select_result() {
    // Create harness with temp project directory
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create a test file with unique content
    let target_content = "// TARGET_FILE\nfn target_function() {\n    let result = 123;\n}\n";
    fs::write(project_root.join("target.rs"), target_content).unwrap();

    // Create initial file in project dir
    let start_file = project_root.join("start.txt");
    fs::write(&start_file, "Starting point\n").unwrap();

    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    // Start Live Grep via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep command to appear (plugin loaded)
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for the target
    harness.type_text("TARGET_FILE").unwrap();

    // Wait for results to appear
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("target.rs") || s.contains("TARGET_FILE")
        })
        .unwrap();

    // Press Enter to select the result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for target file to open
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("TARGET_FILE") || s.contains("target_function")
        })
        .unwrap();
}

/// Test Live Grep - preview split appears and closes on ESC
#[test]
#[ignore = "flaky test - times out intermittently"]
fn test_live_grep_preview_split() {
    // Create harness with temp project directory
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create a test file with content to search
    let search_content = "PREVIEW_TEST_CONTENT\nLine 2\nLine 3\nLine 4\nLine 5\n";
    fs::write(project_root.join("preview_test.txt"), search_content).unwrap();

    // Create initial file in project dir
    let main_file = project_root.join("main.txt");
    fs::write(&main_file, "Main file\n").unwrap();

    harness.open_file(&main_file).unwrap();
    harness.render().unwrap();

    // Start Live Grep via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep command to appear (plugin loaded)
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for content
    harness.type_text("PREVIEW_TEST").unwrap();

    // Wait for preview split to appear
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("*Preview*") || s.contains("PREVIEW_TEST_CONTENT")
        })
        .unwrap();

    // Press ESC to cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Wait for preview split to close
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Preview*"))
        .unwrap();
}

/// Test Live Grep - input is preserved when navigating results
#[test]
fn test_live_grep_input_preserved() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create multiple files with matching content
    for i in 1..=5 {
        let content = format!("MULTI_MATCH line in file {}\n", i);
        fs::write(project_root.join(format!("file{}.txt", i)), content).unwrap();
    }

    // Create initial file
    let fixture = TestFixture::new("start.txt", "Start\n").unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Start Live Grep via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep command to appear (plugin loaded)
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type search query
    harness.type_text("MULTI_MATCH").unwrap();

    // Wait for results to appear
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("file1.txt") || s.contains("MULTI_MATCH")
        })
        .unwrap();

    // Navigate down through results
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after_nav = harness.screen_to_string();
    println!("Screen after navigation:\n{}", screen_after_nav);

    // The prompt should still show "MULTI_MATCH" (input preserved)
    // This verifies our fix that plugin prompts don't overwrite input on navigation
    assert!(
        screen_after_nav.contains("MULTI_MATCH"),
        "Search input should be preserved when navigating results. Got:\n{}",
        screen_after_nav
    );

    // Clean up
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test Live Grep searches in the working directory, not the process current directory
///
/// This test verifies that when the editor's working directory is set to a path
/// different from the process's current directory, Live Grep searches in the
/// working directory (where the user's project is) rather than where fresh was launched.
#[test]
fn test_live_grep_uses_working_dir() {
    // Check if ripgrep is available (required by live_grep plugin)
    let rg_check = std::process::Command::new("rg").arg("--version").output();

    if rg_check.is_err() || !rg_check.as_ref().unwrap().status.success() {
        eprintln!("Skipping test: ripgrep (rg) is not installed or not in PATH");
        eprintln!("Live Grep plugin requires ripgrep to function");
        return;
    }

    // Create a temporary project directory - this will be our working_dir
    // It is intentionally different from std::env::current_dir()
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create a test file with a unique marker that only exists in our temp project
    // This marker should NOT exist in the fresh repo's actual directory
    let unique_marker = "WORKDIR_TEST_UNIQUE_7f3a9b2c";
    let test_content = format!(
        "// This file contains {}\n// It should be found by live grep\nlet x = 42;\n",
        unique_marker
    );
    fs::write(project_root.join("workdir_test.rs"), test_content).unwrap();

    // Create initial file in project dir
    let start_file = project_root.join("start.txt");
    fs::write(&start_file, "Starting point for workdir test\n").unwrap();

    // Create harness with working_dir set to project_root
    // This is the key: working_dir != current_dir()
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    // Start Live Grep via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep command to appear (plugin loaded)
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for our unique marker
    harness.type_text(unique_marker).unwrap();

    // Wait for results - should find our file in the working directory
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("workdir_test.rs")
        })
        .unwrap();

    // Verify the result is from our working directory
    harness.assert_screen_contains("workdir_test.rs");

    // Press Enter to open the file at the match location
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the file to open - look for the unique marker in the screen
    // (it will appear in the editor content area once the file is loaded)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(unique_marker)
        })
        .unwrap();

    // Verify the buffer content is from our working directory
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains(unique_marker),
        "Buffer should contain the unique marker from working_dir. Got: {}",
        content
    );

    // Verify we're on line 1 (where the marker is)
    // The status bar format is "Ln X, Col Y" (1-indexed)
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("Ln 1"),
        "Cursor should be on line 1 (the match line). Status bar: {}",
        status_bar
    );
}
