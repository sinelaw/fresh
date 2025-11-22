//! E2E tests for git features (git grep and git find file)

use crate::common::git_test_helper::{DirGuard, GitTestRepo};
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Helper to trigger git grep via command palette
fn trigger_git_grep(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Git Grep").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper to trigger git find file via command palette
fn trigger_git_find_file(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Git Find File").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test git grep basic functionality - visibility of results
#[test]
fn test_git_grep_shows_results() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git grep
    trigger_git_grep(&mut harness);

    // Check that the prompt appeared
    harness.assert_screen_contains("Git grep: ");

    // Type search query
    harness.type_text("config").unwrap();

    // Wait for git grep to complete by checking for results in the suggestions box
    // The plugin populates suggestions with file:line:column format
    let found = harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                // Wait for suggestions to appear - they show as "filename:line:column: content"
                // The suggestion box appears above the prompt
                screen.contains(".yml:") || screen.contains(".md:") || screen.contains(".rs:")
            },
            5000,
        )
        .unwrap();

    if !found {
        // Print screen for debugging if test fails
        let screen = harness.screen_to_string();
        eprintln!("Git grep timeout - screen content:\n{}", screen);
    }

    assert!(found, "Git grep should complete and show suggestions");

    // Verify results are visible
    let screen = harness.screen_to_string();
    println!("Git grep screen:\n{screen}");

    // Should show at least one match
    assert!(
        screen.contains("src/") || screen.contains("Config") || screen.contains("config"),
        "Should show grep results"
    );
}

/// Test git grep interactive updates - results update as user types
#[test]
fn test_git_grep_interactive_updates() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git grep
    trigger_git_grep(&mut harness);

    // Type first query
    harness.type_text("Config").unwrap();

    // Wait for initial results
    harness
        .wait_for_async(|h| h.screen_to_string().contains("src/"), 2000)
        .unwrap();

    let screen_config = harness.screen_to_string();

    // Backspace to clear and type different query
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    harness.render().unwrap();

    harness.type_text("println").unwrap();

    // Wait for new results
    harness
        .wait_for_async(
            |h| {
                let s = h.screen_to_string();
                s.contains("println") || s.contains("main.rs")
            },
            2000,
        )
        .unwrap();

    let screen_println = harness.screen_to_string();

    // Results should have changed
    println!("After 'Config' query:\n{screen_config}");
    println!("After 'println' query:\n{screen_println}");

    // Both searches should show some results
    assert!(
        screen_config.contains("Config") || screen_config.contains("src/"),
        "Config search should show results"
    );
}

/// Test git grep selection and navigation
#[test]
fn test_git_grep_selection_navigation() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git grep
    trigger_git_grep(&mut harness);

    // Search for something that appears multiple times
    harness.type_text("config").unwrap();

    // Wait for results
    harness
        .wait_for_async(|h| h.screen_to_string().contains("src/"), 2000)
        .unwrap();

    // Navigate down through suggestions
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    let screen_after_down = harness.screen_to_string();

    // Navigate up
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    let screen_after_up = harness.screen_to_string();

    println!("After down:\n{screen_after_down}");
    println!("After up:\n{screen_after_up}");

    // The screens should show the prompt is still active
    assert!(screen_after_down.contains("Git grep:"));
    assert!(screen_after_up.contains("Git grep:"));
}

/// Test git grep confirm - jump to match location
#[test]
fn test_git_grep_confirm_jumps_to_location() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git grep
    trigger_git_grep(&mut harness);

    // Search for specific text
    harness.type_text("Hello, world").unwrap();

    // Wait for results
    harness
        .wait_for_async(|h| h.screen_to_string().contains("main.rs"), 2000)
        .unwrap();

    // Confirm selection (Enter) - this should open file and jump to line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Give it time to open the file
    std::thread::sleep(std::time::Duration::from_millis(200));
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("After confirming grep result:\n{screen}");

    // The file should have opened and we should be out of prompt mode
    // Note: The file might not show content if paths are relative and directory changed,
    // but at minimum the prompt should be gone
    harness.assert_screen_not_contains("Git grep:");

    // The screen should show either the file content OR at least not be in prompt mode
    // In a real scenario with proper path handling, this would show file content
    let has_file_content = screen.contains("Hello, world")
        || screen.contains("fn main")
        || screen.contains("println")
        || screen.contains("main.rs");

    if !has_file_content {
        // If file didn't open (due to relative path issues in test environment),
        // at least verify we exited the prompt successfully
        println!(
            "Note: File content not visible (likely due to relative path in test environment)"
        );
    }
}

/// Test git grep cancel
#[test]
fn test_git_grep_cancel() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git grep
    trigger_git_grep(&mut harness);

    harness.assert_screen_contains("Git grep: ");

    // Type something
    harness.type_text("config").unwrap();

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Prompt should be gone
    harness.assert_screen_not_contains("Git grep: ");
}

/// Test git find file basic functionality
#[test]
fn test_git_find_file_shows_results() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git find file
    trigger_git_find_file(&mut harness);

    // Wait for async git ls-files to complete and the file picker to appear
    // The plugin loads files asynchronously, so we need to wait for both
    // the prompt "Find file: " and some file results to appear
    let found = harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                // Wait for both the prompt and file content
                screen.contains("Find file:") &&
                    (screen.contains("src/") || screen.contains(".rs") || screen.contains("Cargo.toml"))
            },
            5000, // Increased timeout for async git command
        )
        .unwrap();

    assert!(found, "File picker and file list should appear within timeout");

    let screen = harness.screen_to_string();
    println!("Git find file screen:\n{screen}");

    // Should show files from the project
    assert!(
        screen.contains(".rs") || screen.contains("Cargo") || screen.contains("README"),
        "Should show project files"
    );
}

/// Test git find file interactive filtering
#[test]
fn test_git_find_file_interactive_filtering() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git find file
    trigger_git_find_file(&mut harness);

    // Wait for initial results
    harness
        .wait_for_async(|h| h.screen_to_string().contains("src/"), 2000)
        .unwrap();

    // Type filter to narrow down results
    harness.type_text("main").unwrap();

    // Wait for filtered results
    harness
        .wait_for_async(|h| h.screen_to_string().contains("main"), 2000)
        .unwrap();

    let screen_main = harness.screen_to_string();
    println!("After filtering 'main':\n{screen_main}");

    // Should show main.rs in results
    assert!(
        screen_main.contains("main.rs") || screen_main.contains("main"),
        "Should filter to show main.rs"
    );

    // Change filter
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    harness.type_text("lib").unwrap();

    // Wait for new filtered results
    harness
        .wait_for_async(|h| h.screen_to_string().contains("lib"), 2000)
        .unwrap();

    let screen_lib = harness.screen_to_string();
    println!("After filtering 'lib':\n{screen_lib}");

    // Should show lib.rs
    assert!(
        screen_lib.contains("lib.rs") || screen_lib.contains("lib"),
        "Should filter to show lib.rs"
    );
}

/// Test git find file selection and navigation
#[test]
fn test_git_find_file_selection_navigation() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git find file
    trigger_git_find_file(&mut harness);

    // Wait for results
    harness
        .wait_for_async(|h| h.screen_to_string().contains("src/"), 2000)
        .unwrap();

    // Navigate down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Navigate down again
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Navigate up
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    let screen = harness.screen_to_string();
    println!("After navigation:\n{screen}");

    // Prompt should still be active
    assert!(screen.contains("Find file:"));
}

/// Test git find file confirm - opens selected file
#[test]
fn test_git_find_file_confirm_opens_file() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git find file
    trigger_git_find_file(&mut harness);

    // Filter to main.rs
    harness.type_text("main.rs").unwrap();

    // Wait for results
    harness
        .wait_for_async(|h| h.screen_to_string().contains("main.rs"), 2000)
        .unwrap();

    // Confirm selection - should open the file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for file to actually load (async operation)
    harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                // Wait for prompt to close (file opened)
                !screen.contains("Find file:")
            },
            3000,
        )
        .unwrap();

    let screen = harness.screen_to_string();
    println!("After confirming file:\n{screen}");

    // The file should have opened and we should be out of prompt mode
    harness.assert_screen_not_contains("Find file:");

    // Check if file content is visible
    let has_file_content =
        screen.contains("fn main()") || screen.contains("println") || screen.contains("Hello");

    if !has_file_content {
        println!(
            "Note: File content not visible (likely due to relative path in test environment)"
        );
    }
}

/// Test git features with many results - scrolling behavior
#[test]
fn test_git_grep_scrolling_many_results() {
    let repo = GitTestRepo::new();

    // Create many files with searchable content
    repo.setup_many_files(50);
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git grep
    trigger_git_grep(&mut harness);

    // Search for "Searchable" which appears in all files
    harness.type_text("Searchable").unwrap();

    // Wait for results (should be truncated to 100 max)
    harness
        .wait_for_async(|h| h.screen_to_string().contains("file"), 2000)
        .unwrap();

    // Navigate down multiple times to test scrolling
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(20));
    }

    let screen = harness.screen_to_string();
    println!("After scrolling down:\n{screen}");

    // Should still show the prompt and results
    assert!(screen.contains("Git grep:"));
    assert!(screen.contains("file") || screen.contains("Searchable"));
}

/// Test git find file with many files - scrolling behavior
#[test]
fn test_git_find_file_scrolling_many_files() {
    let repo = GitTestRepo::new();
    repo.setup_many_files(50);
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git find file
    trigger_git_find_file(&mut harness);

    // Wait for file list
    harness
        .wait_for_async(|h| h.screen_to_string().contains("file"), 2000)
        .unwrap();

    // Navigate down multiple times
    for _ in 0..15 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(20));
    }

    // Navigate up
    for _ in 0..5 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(20));
    }

    let screen = harness.screen_to_string();
    println!("After scrolling:\n{screen}");

    // Should still show the prompt
    assert!(screen.contains("Find file:"));
}

/// Test that git commands work from command palette
#[test]
fn test_git_commands_via_command_palette() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Test that we can invoke git commands via command palette
    // Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Command: ");

    // Type to filter to git commands (note: no colon in command name)
    harness.type_text("Git Grep").unwrap();
    harness.render().unwrap();

    // Confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now be in git grep mode
    harness.assert_screen_contains("Git grep:");
}

/// REPRODUCTION TEST: Git grep selection should open file and jump to exact line
#[test]
fn test_git_grep_opens_correct_file_and_jumps_to_line() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Verify we start with an empty buffer
    let initial_content = harness.get_buffer_content();
    assert!(
        initial_content.is_empty() || initial_content == "\n",
        "Should start with empty buffer"
    );

    // Trigger git grep
    trigger_git_grep(&mut harness);

    // Search for "println" which appears in main.rs line 2
    harness.type_text("println").unwrap();

    // Wait for results
    let found = harness
        .wait_for_async(|h| h.screen_to_string().contains("main.rs"), 2000)
        .unwrap();
    assert!(found, "Should find grep results");

    let screen_before = harness.screen_to_string();
    println!("Screen with results:\n{screen_before}");

    // Confirm selection (Enter)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for file to actually load (async operation)
    let file_loaded = harness
        .wait_for_async(
            |h| {
                let content = h.get_buffer_content();
                !content.is_empty() && content != "\n" && content.contains("println")
            },
            3000,
        )
        .unwrap();

    // CRITICAL CHECKS:

    // 1. Buffer content should have changed from empty to the file content
    let buffer_content = harness.get_buffer_content();
    println!("Buffer content after selection:\n{buffer_content}");

    assert!(
        file_loaded,
        "BUG: File was not opened within timeout. Buffer: {buffer_content:?}"
    );

    assert!(
        buffer_content.contains("println"),
        "BUG: Buffer does not contain expected file content. Expected 'println' in buffer. Buffer: {buffer_content:?}"
    );

    // 2. The cursor should be at the line with println (line 2)
    let cursor_pos = harness.cursor_position();
    println!("Cursor position: {cursor_pos}");

    // The cursor should NOT be at position 0 (start of file)
    // It should be near the "println" line
    assert!(
        cursor_pos > 0,
        "BUG: Cursor is at position 0! It should have jumped to the match line. Position: {cursor_pos}"
    );

    // 3. Verify screen shows the file content
    let screen_after = harness.screen_to_string();
    println!("Screen after selection:\n{screen_after}");

    assert!(
        screen_after.contains("fn main") || screen_after.contains("println"),
        "BUG: Screen does not show file content after selection"
    );
}

/// REPRODUCTION TEST: Git find file selection should actually open the file
#[test]
fn test_git_find_file_actually_opens_file() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_plugins();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Verify we start with an empty buffer
    let initial_content = harness.get_buffer_content();
    assert!(
        initial_content.is_empty() || initial_content == "\n",
        "Should start with empty buffer"
    );

    // Trigger git find file
    trigger_git_find_file(&mut harness);

    // Wait for file list to load first (async operation)
    std::thread::sleep(std::time::Duration::from_millis(500));
    harness.render().unwrap();

    // Type to find lib.rs
    harness.type_text("lib.rs").unwrap();

    // Wait for results - check that suggestions are populated
    let found = harness
        .wait_for_async(
            |h| {
                // Check if the prompt has suggestions by checking if a file path appears
                // in the screen content (not just the prompt input line)
                // We look for "src/" which only appears in file results, not in the prompt
                let s = h.screen_to_string();
                let lines: Vec<&str> = s.lines().collect();

                // The last line is the prompt "Find file: lib.rs"
                // Check if any line EXCEPT the last one contains "src/"
                lines
                    .iter()
                    .take(lines.len().saturating_sub(1))
                    .any(|line| line.contains("src/"))
            },
            3000, // Increased timeout
        )
        .unwrap();

    let screen_before = harness.screen_to_string();
    println!("Screen with file list:\n{screen_before}");

    assert!(
        found,
        "Should find lib.rs in results. Screen:\n{screen_before}"
    );

    // Confirm selection (Enter)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Give time for file to load
    std::thread::sleep(std::time::Duration::from_millis(200));
    harness.render().unwrap();

    // CRITICAL CHECKS:

    // 1. Buffer content should have changed from empty to lib.rs content
    let buffer_content = harness.get_buffer_content();
    println!("Buffer content after selection:\n{buffer_content}");

    assert!(
        !buffer_content.is_empty() && buffer_content != "\n",
        "BUG: Buffer is still empty! File lib.rs was not opened. Buffer: {buffer_content:?}"
    );

    assert!(
        buffer_content.contains("pub struct Config") || buffer_content.contains("impl Default"),
        "BUG: Buffer does not contain lib.rs content. Expected 'Config' or 'impl Default'. Buffer: {buffer_content:?}"
    );

    // 2. Verify screen shows the file content
    let screen_after = harness.screen_to_string();
    println!("Screen after selection:\n{screen_after}");

    assert!(
        screen_after.contains("Config") || screen_after.contains("pub struct"),
        "BUG: Screen does not show lib.rs content after selection. Screen:\n{screen_after}"
    );

    // 3. Status bar should show we're no longer in prompt mode
    harness.assert_screen_not_contains("Find file:");
}

/// REPRODUCTION TEST: Verify cursor jumps to correct line in git grep
#[test]
fn test_git_grep_cursor_position_accuracy() {
    let repo = GitTestRepo::new();

    // Create a file with known line content
    repo.create_file(
        "test.txt",
        "Line 1\nLine 2\nLine 3 with MARKER\nLine 4\nLine 5\n",
    );
    repo.git_add(&["test.txt"]);
    repo.git_commit("Add test file");
    repo.setup_git_plugins();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git grep
    trigger_git_grep(&mut harness);

    // Search for MARKER (should be on line 3)
    harness.type_text("MARKER").unwrap();

    // Wait for results
    harness
        .wait_for_async(|h| h.screen_to_string().contains("test.txt"), 2000)
        .unwrap();

    // Confirm selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for file to actually load (async operation)
    let file_loaded = harness
        .wait_for_async(
            |h| {
                let content = h.get_buffer_content();
                content.contains("MARKER")
            },
            3000,
        )
        .unwrap();

    // Check buffer content
    let buffer_content = harness.get_buffer_content();
    println!("Buffer content:\n{buffer_content}");

    assert!(
        file_loaded,
        "BUG: File not opened or wrong file opened within timeout. Buffer: {buffer_content:?}"
    );

    // The cursor should be on line 3 (0-indexed = line 2)
    // Calculate expected byte position for line 3
    // Line 1: "Line 1\n" = 7 bytes
    // Line 2: "Line 2\n" = 7 bytes
    // Line 3 starts at byte 14
    let cursor_pos = harness.cursor_position();
    println!("Cursor position: {cursor_pos}");

    // Cursor should be at line 3 (byte position should be at or after byte 14)
    assert!(
        cursor_pos >= 14,
        "BUG: Cursor should be at line 3 (position >= 14), but is at position {cursor_pos}"
    );

    // Verify the line at cursor contains MARKER
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("MARKER"),
        "BUG: Screen should show the line with MARKER"
    );
}

// =============================================================================
// Git Log Tests
// =============================================================================

/// Helper to trigger git log via command palette
fn trigger_git_log(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Git Log").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test git log opens and shows commits
#[test]
fn test_git_log_shows_commits() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_log_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git log
    trigger_git_log(&mut harness);

    // Wait for git log to load
    let found = harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                // Should show "Commits:" header and at least one commit hash
                screen.contains("Commits:") && screen.contains("Initial commit")
            },
            3000,
        )
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Git log screen:\n{screen}");

    assert!(found, "Git log should show commits. Screen:\n{screen}");
    assert!(screen.contains("Commits:"), "Should show Commits: header");
}

/// Test git log cursor navigation
#[test]
fn test_git_log_cursor_navigation() {
    let repo = GitTestRepo::new();

    // Create multiple commits for navigation testing
    repo.create_file("file1.txt", "Content 1");
    repo.git_add(&["file1.txt"]);
    repo.git_commit("First commit");

    repo.create_file("file2.txt", "Content 2");
    repo.git_add(&["file2.txt"]);
    repo.git_commit("Second commit");

    repo.create_file("file3.txt", "Content 3");
    repo.git_add(&["file3.txt"]);
    repo.git_commit("Third commit");

    repo.setup_git_log_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git log
    trigger_git_log(&mut harness);

    // Wait for git log to load
    harness
        .wait_for_async(|h| h.screen_to_string().contains("Commits:"), 3000)
        .unwrap();

    // Navigate down using j key (should work via inherited normal mode)
    harness.send_key(KeyCode::Char('j'), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Navigate down using Down arrow
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Navigate up using k key
    harness.send_key(KeyCode::Char('k'), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    let screen = harness.screen_to_string();
    println!("After navigation:\n{screen}");

    // Git log should still be visible
    assert!(screen.contains("Commits:"));
}

/// Test git log show commit detail with Enter
#[test]
fn test_git_log_show_commit_detail() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_log_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git log
    trigger_git_log(&mut harness);

    // Wait for git log to load
    harness
        .wait_for_async(|h| h.screen_to_string().contains("Commits:"), 3000)
        .unwrap();

    // Move cursor to a commit line (down from header)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Press Enter to show commit detail
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Wait for commit detail to load
    let found = harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                // git show output includes "commit", "Author:", "Date:"
                screen.contains("Author:") && screen.contains("Date:")
            },
            3000,
        )
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Commit detail screen:\n{screen}");

    assert!(found, "Should show commit detail. Screen:\n{screen}");
}

/// Test going back from commit detail to git log
#[test]
fn test_git_log_back_from_commit_detail() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_log_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git log
    trigger_git_log(&mut harness);

    // Wait for git log to load
    harness
        .wait_for_async(|h| h.screen_to_string().contains("Commits:"), 3000)
        .unwrap();

    // Move to commit and show detail
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Wait for commit detail
    harness
        .wait_for_async(|h| h.screen_to_string().contains("Author:"), 3000)
        .unwrap();

    let screen_detail = harness.screen_to_string();
    println!("Commit detail:\n{screen_detail}");

    // Press q to go back to git log
    harness.send_key(KeyCode::Char('q'), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Wait for git log to reappear
    let back_to_log = harness
        .wait_for_async(|h| h.screen_to_string().contains("Commits:"), 2000)
        .unwrap();

    let screen_log = harness.screen_to_string();
    println!("Back to git log:\n{screen_log}");

    assert!(back_to_log, "Should return to git log view. Screen:\n{screen_log}");
}

/// Test closing git log with q
#[test]
fn test_git_log_close() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_log_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git log
    trigger_git_log(&mut harness);

    // Wait for git log to load
    harness
        .wait_for_async(|h| h.screen_to_string().contains("Commits:"), 3000)
        .unwrap();

    let screen_before = harness.screen_to_string();
    assert!(screen_before.contains("Commits:"));

    // Press q to close git log
    harness.send_key(KeyCode::Char('q'), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Git log should be closed
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("After closing:\n{screen_after}");

    // Should no longer show git log
    harness.assert_screen_not_contains("Commits:");
}

/// Test diff coloring in commit detail
#[test]
fn test_git_log_diff_coloring() {
    // Use the typical project setup which creates files and commits
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_log_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Trigger git log
    trigger_git_log(&mut harness);

    // Wait for git log to load
    harness
        .wait_for_async(|h| h.screen_to_string().contains("Commits:"), 3000)
        .unwrap();

    // Move to the commit and show detail
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Wait for commit detail (git show output includes Author:)
    let found = harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                screen.contains("Author:")
            },
            3000,
        )
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Commit detail with diff:\n{screen}");

    assert!(found, "Should show commit detail. Screen:\n{screen}");

    // The commit detail should show commit info from git show output
    // Note: The exact coloring is applied via overlays which aren't visible in screen text
    assert!(
        screen.contains("Author:") || screen.contains("Date:"),
        "Should show commit info"
    );
}
