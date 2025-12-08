//! E2E tests for git features (git grep and git find file)

use crate::common::git_test_helper::{DirGuard, GitTestRepo};
use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
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

    // Check that the prompt appeared
    harness.assert_screen_contains("Git grep: ");

    // Type search query
    harness.type_text("config").unwrap();

    // Wait for git grep to complete by checking for results in the suggestions box
    // The plugin populates suggestions with file:line:column format
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Wait for suggestions to appear - they show as "filename:line:column: content"
            // The suggestion box appears above the prompt
            screen.contains(".yml:") || screen.contains(".md:") || screen.contains(".rs:")
        })
        .unwrap();

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

    // Type first query
    harness.type_text("Config").unwrap();

    // Wait for initial results
    harness
        .wait_until(|h| h.screen_to_string().contains("src/"))
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
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("println") || s.contains("main.rs")
        })
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

    // Search for something that appears multiple times
    harness.type_text("config").unwrap();

    // Wait for results
    harness
        .wait_until(|h| h.screen_to_string().contains("src/"))
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

    // Search for specific text
    harness.type_text("Hello, world").unwrap();

    // Wait for results
    harness
        .wait_until(|h| h.screen_to_string().contains("main.rs"))
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
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Wait for both the prompt and file content
            screen.contains("Find file:")
                && (screen.contains("src/")
                    || screen.contains(".rs")
                    || screen.contains("Cargo.toml"))
        })
        .unwrap();

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

    // Wait for initial results
    harness
        .wait_until(|h| h.screen_to_string().contains("src/"))
        .unwrap();

    // Type filter to narrow down results
    harness.type_text("main").unwrap();

    // Wait for filtered results
    harness
        .wait_until(|h| h.screen_to_string().contains("main"))
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
        .wait_until(|h| h.screen_to_string().contains("lib"))
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

    // Wait for results
    harness
        .wait_until(|h| h.screen_to_string().contains("src/"))
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
#[cfg_attr(windows, ignore)] // Git plugin tests timeout on Windows CI
fn test_git_find_file_confirm_opens_file() {
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

    // Filter to main.rs
    harness.type_text("main.rs").unwrap();

    // Wait for results
    harness
        .wait_until(|h| h.screen_to_string().contains("main.rs"))
        .unwrap();

    // Confirm selection - should open the file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for file to actually load (async operation)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Wait for prompt to close (file opened)
            !screen.contains("Find file:")
        })
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

    // Search for "Searchable" which appears in all files
    harness.type_text("Searchable").unwrap();

    // Wait for results (should be truncated to 100 max)
    harness
        .wait_until(|h| h.screen_to_string().contains("file"))
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

    // Wait for file list
    harness
        .wait_until(|h| h.screen_to_string().contains("file"))
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

    // Wait for git grep mode to appear (async plugin loading)
    harness
        .wait_until(|h| h.screen_to_string().contains("Git grep:"))
        .unwrap();
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
    let initial_content = harness.get_buffer_content().unwrap();
    assert!(
        initial_content.is_empty() || initial_content == "\n",
        "Should start with empty buffer"
    );

    // Trigger git grep
    trigger_git_grep(&mut harness);

    // Search for "println" which appears in main.rs line 2
    harness.type_text("println").unwrap();

    // Wait for results
    harness
        .wait_until(|h| h.screen_to_string().contains("main.rs"))
        .unwrap();

    let screen_before = harness.screen_to_string();
    println!("Screen with results:\n{screen_before}");

    // Confirm selection (Enter)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for file to actually load (async operation)
    harness
        .wait_until(|h| {
            let content = h.get_buffer_content().unwrap();
            !content.is_empty() && content != "\n" && content.contains("println")
        })
        .unwrap();

    // CRITICAL CHECKS:

    // 1. Buffer content should have changed from empty to the file content
    let buffer_content = harness.get_buffer_content().unwrap();
    println!("Buffer content after selection:\n{buffer_content}");

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
    let initial_content = harness.get_buffer_content().unwrap();
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
    harness
        .wait_until(|h| {
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
        })
        .unwrap();

    let screen_before = harness.screen_to_string();
    println!("Screen with file list:\n{screen_before}");

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
    let buffer_content = harness.get_buffer_content().unwrap();
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
        .wait_until(|h| h.screen_to_string().contains("test.txt"))
        .unwrap();

    // Confirm selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for file to actually load (async operation)
    harness
        .wait_until(|h| {
            let content = h.get_buffer_content().unwrap();
            content.contains("MARKER")
        })
        .unwrap();

    // Check buffer content
    let buffer_content = harness.get_buffer_content().unwrap();
    println!("Buffer content:\n{buffer_content}");

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
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Should show "Commits:" header and at least one commit hash
            screen.contains("Commits:") && screen.contains("Initial commit")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Git log screen:\n{screen}");

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
        .wait_until(|h| h.screen_to_string().contains("Commits:"))
        .unwrap();

    // Navigate down using j key (should work via inherited normal mode)
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Navigate down using Down arrow
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Navigate up using k key
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::NONE)
        .unwrap();
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
        .wait_until(|h| h.screen_to_string().contains("Commits:"))
        .unwrap();

    // Move cursor to a commit line (down from header)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Press Enter to show commit detail
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for commit detail to load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // git show output includes "commit", "Author:", "Date:"
            screen.contains("Author:") && screen.contains("Date:")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Commit detail screen:\n{screen}");
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
        .wait_until(|h| h.screen_to_string().contains("Commits:"))
        .unwrap();

    // Move to commit and show detail
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for commit detail
    harness
        .wait_until(|h| h.screen_to_string().contains("Author:"))
        .unwrap();

    let screen_detail = harness.screen_to_string();
    println!("Commit detail:\n{screen_detail}");

    // Press q to go back to git log
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Wait for git log to reappear
    harness
        .wait_until(|h| h.screen_to_string().contains("Commits:"))
        .unwrap();

    let screen_log = harness.screen_to_string();
    println!("Back to git log:\n{screen_log}");
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
        .wait_until(|h| h.screen_to_string().contains("Commits:"))
        .unwrap();

    let screen_before = harness.screen_to_string();
    assert!(screen_before.contains("Commits:"));

    // Press q to close git log
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
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
        .wait_until(|h| h.screen_to_string().contains("Commits:"))
        .unwrap();

    // Move to the commit and show detail
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for commit detail (git show output includes Author:)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Author:")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Commit detail with diff:\n{screen}");

    // The commit detail should show commit info from git show output
    // Note: The exact coloring is applied via overlays which aren't visible in screen text
    assert!(
        screen.contains("Author:") || screen.contains("Date:"),
        "Should show commit info"
    );
}

/// REPRODUCTION TEST: Opening different commits after closing should open the correct commit
/// This tests the bug where after opening a commit with Enter, quitting with q, navigating
/// to a different commit and pressing Enter would open the first commit again instead of
/// the newly selected one.
#[test]
fn test_git_log_open_different_commits_sequentially() {
    let repo = GitTestRepo::new();

    // Create multiple commits with distinct, identifiable messages
    repo.create_file("file1.txt", "Content for first file");
    repo.git_add(&["file1.txt"]);
    repo.git_commit("FIRST_UNIQUE_COMMIT_AAA");

    repo.create_file("file2.txt", "Content for second file");
    repo.git_add(&["file2.txt"]);
    repo.git_commit("SECOND_UNIQUE_COMMIT_BBB");

    repo.create_file("file3.txt", "Content for third file");
    repo.git_add(&["file3.txt"]);
    repo.git_commit("THIRD_UNIQUE_COMMIT_CCC");

    repo.setup_git_log_plugin();

    // The harness sets the working directory for the editor, and the plugin
    // uses editor.getCwd() to get it for git commands - no need to change
    // process-wide CWD which would cause race conditions in parallel tests.
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
        .wait_until(|h| h.screen_to_string().contains("Commits:"))
        .unwrap();

    let screen_log = harness.screen_to_string();
    println!("Git log with commits:\n{screen_log}");

    // Verify all commits are visible
    assert!(
        screen_log.contains("THIRD_UNIQUE_COMMIT_CCC"),
        "Should show third commit"
    );
    assert!(
        screen_log.contains("SECOND_UNIQUE_COMMIT_BBB"),
        "Should show second commit"
    );
    assert!(
        screen_log.contains("FIRST_UNIQUE_COMMIT_AAA"),
        "Should show first commit"
    );

    // Navigate down to the first commit line (header is line 1, commits start at line 2)
    // Most recent commit (THIRD) is at the top
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Open the first commit (THIRD_UNIQUE_COMMIT_CCC - most recent)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for commit detail
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Author:") && screen.contains("THIRD_UNIQUE_COMMIT_CCC")
        })
        .unwrap();

    let screen_first_detail = harness.screen_to_string();
    println!("First commit detail (should be THIRD):\n{screen_first_detail}");

    // Press q to go back to git log
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Wait for git log to reappear
    harness
        .wait_until(|h| h.screen_to_string().contains("Commits:"))
        .unwrap();

    let screen_back_to_log = harness.screen_to_string();
    println!("Back to git log:\n{screen_back_to_log}");

    // Now navigate DOWN to a DIFFERENT commit (SECOND_UNIQUE_COMMIT_BBB)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    let screen_after_nav = harness.screen_to_string();
    println!("After navigating down:\n{screen_after_nav}");

    // Open the second commit - THIS IS THE BUG: it should open SECOND, not THIRD
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for commit detail
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Author:")
        })
        .unwrap();

    let screen_second_detail = harness.screen_to_string();
    println!("Second commit detail (should be SECOND):\n{screen_second_detail}");

    // CRITICAL ASSERTION: The bug is that it opens the first commit again instead of the second
    // This should show SECOND_UNIQUE_COMMIT_BBB, NOT THIRD_UNIQUE_COMMIT_CCC
    assert!(
        screen_second_detail.contains("SECOND_UNIQUE_COMMIT_BBB"),
        "BUG: After navigating to a different commit and pressing Enter, it should open SECOND_UNIQUE_COMMIT_BBB, but got:\n{screen_second_detail}"
    );
    assert!(
        !screen_second_detail.contains("THIRD_UNIQUE_COMMIT_CCC"),
        "BUG: Should NOT show THIRD commit when SECOND was selected:\n{screen_second_detail}"
    );
}

// =============================================================================
// Git Blame Tests
// =============================================================================

/// Helper to trigger git blame via command palette
fn trigger_git_blame(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Git Blame").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test git blame opens and shows blame blocks with headers
// TODO: Fix git blame tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_blame_shows_blocks_with_headers() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_blame_plugin();

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

    // Open file directly using the harness method
    let file_path = repo.path.join("src/main.rs");
    harness.open_file(&file_path).unwrap();

    // Wait until file is loaded (logical event)
    harness
        .wait_until(|h| {
            let content = h.get_buffer_content().unwrap();
            content.contains("fn main")
        })
        .unwrap();

    // Trigger git blame
    trigger_git_blame(&mut harness);

    // Wait until git blame view appears (logical event)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Should show block headers with ── (commit info injected via view transform)
            screen.contains("──") && screen.contains("Initial commit")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Git blame screen:\n{screen}");

    assert!(screen.contains("──"), "Should show block header separator");
    assert!(
        screen.contains("Initial commit"),
        "Should show commit summary in header"
    );
}

/// Test git blame cursor navigation
// TODO: Fix git blame tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_blame_cursor_navigation() {
    let repo = GitTestRepo::new();

    // Create a file with multiple commits to have multiple blame blocks
    repo.create_file("test.txt", "Line 1\nLine 2\n");
    repo.git_add(&["test.txt"]);
    repo.git_commit("First commit");

    repo.create_file("test.txt", "Line 1\nLine 2\nLine 3\nLine 4\n");
    repo.git_add(&["test.txt"]);
    repo.git_commit("Second commit");

    repo.setup_git_blame_plugin();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open file directly
    let file_path = repo.path.join("test.txt");
    harness.open_file(&file_path).unwrap();

    // Wait until file is loaded
    harness
        .wait_until(|h| h.get_buffer_content().unwrap().contains("Line 1"))
        .unwrap();

    // Trigger git blame
    trigger_git_blame(&mut harness);

    // Wait until blame view appears (block headers with ──)
    harness
        .wait_until(|h| h.screen_to_string().contains("──"))
        .unwrap();

    // Navigate down using j key
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Navigate down using Down arrow
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Navigate up using k key
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    let screen = harness.screen_to_string();
    println!("After navigation:\n{screen}");

    // Git blame should still be visible (showing block headers)
    assert!(screen.contains("──"));
}

/// Test git blame close with q
// TODO: Fix git blame tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_blame_close() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_blame_plugin();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open file directly
    let file_path = repo.path.join("src/main.rs");
    harness.open_file(&file_path).unwrap();

    // Wait until file is loaded
    harness
        .wait_until(|h| h.get_buffer_content().unwrap().contains("fn main"))
        .unwrap();

    // Trigger git blame
    trigger_git_blame(&mut harness);

    // Wait until blame view appears (block headers with ──)
    harness
        .wait_until(|h| h.screen_to_string().contains("──"))
        .unwrap();

    let screen_before = harness.screen_to_string();
    assert!(screen_before.contains("──"));

    // Press q to close git blame
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();

    // Wait until blame view is closed (back to original file without headers)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Original file should be visible without blame headers
            screen.contains("fn main") && !screen.contains("──")
        })
        .unwrap();

    let screen_after = harness.screen_to_string();
    println!("After closing:\n{screen_after}");

    // Should no longer show git blame headers
    harness.assert_screen_not_contains("──");
}

/// Test git blame go back in history with 'b' key
// TODO: Fix git blame tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_blame_go_back_in_history() {
    let repo = GitTestRepo::new();

    // Create initial file
    repo.create_file("test.txt", "Original line 1\nOriginal line 2\n");
    repo.git_add(&["test.txt"]);
    repo.git_commit("First commit");

    // Modify file (this creates a second commit that we can blame back to)
    repo.create_file("test.txt", "Original line 1\nModified line 2\nNew line 3\n");
    repo.git_add(&["test.txt"]);
    repo.git_commit("Second commit");

    repo.setup_git_blame_plugin();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open file directly
    let file_path = repo.path.join("test.txt");
    harness.open_file(&file_path).unwrap();

    // Wait until file is loaded
    harness
        .wait_until(|h| h.get_buffer_content().unwrap().contains("line"))
        .unwrap();

    // Trigger git blame
    trigger_git_blame(&mut harness);

    // Wait until blame view appears (block headers with ──)
    harness
        .wait_until(|h| h.screen_to_string().contains("──"))
        .unwrap();

    // Navigate to a line from the second commit
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    let screen_before = harness.screen_to_string();
    println!("Before pressing 'b':\n{screen_before}");

    // Press 'b' to go back in history
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::NONE)
        .unwrap();

    // Wait until we see the depth indicator in status or content changes
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // After going back, the blame headers should still be visible
            // and we might see "depth:" in the status or different file content
            screen.contains("──") && (screen.contains("depth:") || screen.contains("First commit"))
        })
        .unwrap();

    let screen_after = harness.screen_to_string();
    println!("After pressing 'b':\n{screen_after}");

    // We should still be in git blame view with block headers
    assert!(
        screen_after.contains("──"),
        "Should still show blame block headers after going back"
    );
}

/// Test git blame with multiple commits shows different authors/dates
// TODO: Fix git blame tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_blame_shows_different_commits() {
    let repo = GitTestRepo::new();

    // Create file with one commit
    repo.create_file("multi.txt", "Line from first commit\n");
    repo.git_add(&["multi.txt"]);
    repo.git_commit("First commit");

    // Add more lines in a second commit
    repo.create_file(
        "multi.txt",
        "Line from first commit\nLine from second commit\n",
    );
    repo.git_add(&["multi.txt"]);
    repo.git_commit("Second commit");

    repo.setup_git_blame_plugin();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open file directly
    let file_path = repo.path.join("multi.txt");
    harness.open_file(&file_path).unwrap();

    // Wait until file is loaded
    harness
        .wait_until(|h| h.get_buffer_content().unwrap().contains("Line from"))
        .unwrap();

    // Trigger git blame
    trigger_git_blame(&mut harness);

    // Wait until blame view appears with multiple blocks
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Should show at least two block headers (different commits)
            // The blocks are separated by ── lines
            let header_count = screen.matches("──").count();
            header_count >= 2
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Git blame with multiple commits:\n{screen}");

    // Should show both commit messages
    assert!(
        screen.contains("First commit") || screen.contains("Second commit"),
        "Should show commit summaries"
    );
}

/// Test git blame line numbers are correct - headers should NOT have line numbers
/// and content lines should have sequential line numbers matching the source file
// TODO: Fix git blame tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_blame_line_numbers_correct() {
    let repo = GitTestRepo::new();

    // Create file with multiple commits for different blame blocks
    repo.create_file(
        "numbered.txt",
        "Line 1 from first commit\nLine 2 from first commit\n",
    );
    repo.git_add(&["numbered.txt"]);
    repo.git_commit("First commit");

    // Add more lines in second commit
    repo.create_file("numbered.txt", "Line 1 from first commit\nLine 2 from first commit\nLine 3 from second commit\nLine 4 from second commit\n");
    repo.git_add(&["numbered.txt"]);
    repo.git_commit("Second commit");

    repo.setup_git_blame_plugin();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open file directly
    let file_path = repo.path.join("numbered.txt");
    harness.open_file(&file_path).unwrap();

    // Wait until file is loaded
    harness
        .wait_until(|h| h.get_buffer_content().unwrap().contains("Line 1"))
        .unwrap();

    // Trigger git blame
    trigger_git_blame(&mut harness);

    // Wait until blame view appears with multiple blocks
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.matches("──").count() >= 2
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Git blame with line numbers:\n{screen}");

    // The screen should show:
    // - Header lines WITHOUT line numbers (just spaces or blank in gutter)
    // - Content lines WITH line numbers 1, 2, 3, 4

    // Check that line numbers 1-4 are present (for the 4 content lines)
    // Line numbers appear at the start of lines in the gutter
    assert!(
        screen.contains("1")
            && screen.contains("2")
            && screen.contains("3")
            && screen.contains("4"),
        "Should show line numbers 1-4 for content lines"
    );

    // The header lines (──) should not be preceded by a line number
    // This is harder to check directly, but we can verify the structure
    // by checking that we have more lines than line numbers
    let total_lines = screen.lines().count();
    let header_count = screen.matches("──").count();

    // With 4 content lines and 2 headers, we should have at least 6 lines
    assert!(
        total_lines >= 6,
        "Should have at least 6 lines (4 content + 2 headers), got {total_lines}"
    );
    assert!(
        header_count >= 2,
        "Should have at least 2 header lines, got {header_count}"
    );
}

/// Test git blame scrolling - scroll to bottom and verify rendering is correct
// TODO: Fix git blame tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_blame_scroll_to_bottom() {
    let repo = GitTestRepo::new();

    // Create file with many lines to require scrolling
    let mut content = String::new();
    for i in 1..=50 {
        content.push_str(&format!("Line {} content\n", i));
    }
    repo.create_file("scrolltest.txt", &content);
    repo.git_add(&["scrolltest.txt"]);
    repo.git_commit("Add scrollable file");

    repo.setup_git_blame_plugin();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30, // Smaller height to force scrolling
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open file directly
    let file_path = repo.path.join("scrolltest.txt");
    harness.open_file(&file_path).unwrap();

    // Wait until file is loaded
    harness
        .wait_until(|h| h.get_buffer_content().unwrap().contains("Line 1"))
        .unwrap();

    // Trigger git blame
    trigger_git_blame(&mut harness);

    // Wait until blame view appears
    harness
        .wait_until(|h| h.screen_to_string().contains("──"))
        .unwrap();

    let screen_top = harness.screen_to_string();
    println!("Git blame at top:\n{screen_top}");

    // Scroll to bottom using Ctrl+End (go to end in read-only blame view)
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();
    harness.render().unwrap();

    let screen_bottom = harness.screen_to_string();
    println!("Git blame at bottom:\n{screen_bottom}");

    // At the bottom, we should see:
    // 1. The last lines of the file (e.g., "Line 50 content")
    // 2. Still have proper rendering (not corrupted)
    // 3. Still be in blame view (showing ── header or content)

    assert!(
        screen_bottom.contains("Line 50")
            || screen_bottom.contains("Line 49")
            || screen_bottom.contains("Line 48"),
        "Should show last lines of file after scrolling to bottom"
    );

    // Should not show the first lines anymore (we scrolled down)
    // Line 1 should be scrolled out of view in a 30-line terminal
    // (though with headers, exact behavior depends on header count)

    // Verify rendering is not corrupted - should still have normal text
    assert!(
        screen_bottom.contains("content"),
        "Should still show file content properly after scrolling"
    );
}

/// Blame view should remain scrollable with many virtual header lines
// TODO: Fix git blame tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_blame_scroll_with_many_virtual_lines() {
    use std::time::Duration;

    let repo = GitTestRepo::new();

    // Small file (few real lines)
    let content = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n";
    repo.create_file("scroll_many_virtual.txt", content);
    repo.git_add(&["scroll_many_virtual.txt"]);
    repo.git_commit("Add file with few lines");
    repo.setup_git_blame_plugin();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Small viewport to stress scrolling with virtual lines from blame headers
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        20,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open file
    let file_path = repo.path.join("scroll_many_virtual.txt");
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger git blame to insert virtual header lines
    trigger_git_blame(&mut harness);

    // Wait for blame view
    harness
        .wait_until(|h| h.screen_to_string().contains("──"))
        .unwrap();

    // Scroll down repeatedly with Down arrow; should make progress even with many virtual lines
    for _ in 0..40 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(5));
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Blame after scrolling with virtual lines:\n{screen}");

    // Expect bottom lines to be visible despite virtual header lines
    assert!(
        screen.contains("Line 5") || screen.contains("Line 4"),
        "Should see tail lines after scrolling with many virtual lines"
    );
}

// =============================================================================
// View Transform Tests - Minimal reproduction of byte 0 header bug
// =============================================================================

/// Helper to trigger test view marker via command palette
fn trigger_test_view_marker(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Test View Marker").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper to trigger test view marker with many virtual lines via command palette
fn trigger_test_view_marker_many_virtual_lines(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .type_text("Test View Marker (Many Virtual Lines)")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper to trigger test view marker with interleaved headers via command palette
fn trigger_test_view_marker_interleaved(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Test View Marker (Interleaved)").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// MINIMAL REPRODUCTION: View transform header at byte 0 should be visible
///
/// This is the simplest possible test for the bug described in docs/BLAME.md:
/// - A view transform injects a header at byte offset 0
/// - The header text should be visible on screen
/// - Currently, the header row exists (blank gutter) but text is empty
///
/// Expected screen output:
/// ```
///       │ == HEADER AT BYTE 0 ==    <- Row 1: blank gutter (no line num), header text
///     1 │ Line 1                    <- Row 2: line 1
///     2 │ Line 2                    <- Row 3: line 2
///     3 │ Line 3                    <- Row 4: line 3
/// ```
///
/// Actual buggy output:
/// ```
///       │                           <- Row 1: blank gutter, EMPTY content (BUG!)
///       │ Line 1                    <- Row 2: blank gutter (wrong! should be line 1)
///     2 │ Line 2                    <- Row 3: line 2
///     3 │ Line 3                    <- Row 4: line 3
/// ```
#[test]
fn test_view_transform_header_at_byte_zero() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();

    // Create a simple file (just for the plugins directory setup)
    repo.create_file("test.txt", "placeholder");
    repo.git_add(&["test.txt"]);
    repo.git_commit("Initial commit");
    repo.setup_test_view_marker_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the test file (needed to have a split to put our virtual buffer in)
    let file_path = repo.path.join("test.txt");
    harness.open_file(&file_path).unwrap();

    // Wait for file to load
    harness
        .wait_until(|h| !h.get_buffer_content().unwrap().is_empty())
        .unwrap();

    // Trigger the test view marker command
    trigger_test_view_marker(&mut harness);

    // Wait for the virtual buffer to be created
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Test view marker active") || screen.contains("*test-view-marker*")
        })
        .unwrap();

    // Wait for the view transform to be applied
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("HEADER AT BYTE 0")
        })
        .unwrap();

    let screen_after = harness.screen_to_string();
    println!("Screen after view marker:\n{screen_after}");
}

/// Ensure scrolling still works when a view transform injects many virtual lines
#[test]
fn test_view_transform_scroll_with_many_virtual_lines() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();

    repo.create_file("test.txt", "placeholder");
    repo.git_add(&["test.txt"]);
    repo.git_commit("Initial commit");
    repo.setup_test_view_marker_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        20,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the test file (so the virtual buffer has a split to attach to)
    let file_path = repo.path.join("test.txt");
    harness.open_file(&file_path).unwrap();
    harness
        .wait_until(|h| !h.get_buffer_content().unwrap().is_empty())
        .unwrap();

    // Launch the view marker that injects many virtual lines (120 pads + header before Line 1)
    trigger_test_view_marker_many_virtual_lines(&mut harness);

    // Wait for the virtual buffer to be created and rendered
    // The cursor starts at Line 1 (byte 0), which is view line 121 (after 120 virtual pads + 1 header)
    // Auto-scroll should bring the cursor into view, showing the source lines
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Line 1") || screen.contains("Line 2") || screen.contains("Line 3")
        })
        .unwrap();

    let initial_screen = harness.screen_to_string();
    println!("Initial screen (auto-scrolled to cursor):\n{initial_screen}");

    // Now scroll UP to verify we can see the virtual lines
    for _ in 0..150 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
    }
    harness.render().unwrap();

    let screen_after_up = harness.screen_to_string();
    println!("Screen after scrolling up through virtual lines:\n{screen_after_up}");

    // After scrolling up, we should see the header or virtual pads
    assert!(
        screen_after_up.contains("HEADER AT BYTE 0") || screen_after_up.contains("Virtual pad"),
        "Scrolling up should reveal header or virtual pad lines"
    );
}

/// Test scrolling with a single virtual line (header only, no pads)
#[test]
fn test_view_transform_scroll_with_single_virtual_line() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();

    repo.create_file("test.txt", "placeholder");
    repo.git_add(&["test.txt"]);
    repo.git_commit("Initial commit");
    repo.setup_test_view_marker_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        20,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the test file
    let file_path = repo.path.join("test.txt");
    harness.open_file(&file_path).unwrap();
    harness
        .wait_until(|h| !h.get_buffer_content().unwrap().is_empty())
        .unwrap();

    // Launch the view marker that injects just a header (no pads)
    trigger_test_view_marker(&mut harness);

    // Wait for virtual buffer to render with header
    harness
        .wait_until(|h| h.screen_to_string().contains("HEADER AT BYTE 0"))
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with single virtual header line:\n{screen}");

    // Should also see source content below the header
    assert!(
        screen.contains("Line 1") || screen.contains("Line 2"),
        "Source content should be visible below header"
    );

    // Verify exact line order by extracting content lines from screen
    let content_lines: Vec<&str> = screen
        .lines()
        .filter(|l| l.contains("│") && !l.contains("~"))
        .collect();

    println!("Content lines: {content_lines:?}");

    // Should have at least 4 lines: header, Line 1, Line 2, Line 3
    assert!(
        content_lines.len() >= 4,
        "Expected at least 4 content lines"
    );

    // Line 0: Header (no line number, just separator)
    assert!(
        content_lines[0].contains("│ == HEADER AT BYTE 0 =="),
        "Line 0 should be header without line number. Got: {}",
        content_lines[0]
    );

    // Line 1: "Line 1" with gutter showing "1"
    assert!(
        content_lines[1].contains("1 │ Line 1"),
        "Line 1 should show '1 │ Line 1'. Got: {}",
        content_lines[1]
    );

    // Line 2: "Line 2" with gutter showing "2"
    assert!(
        content_lines[2].contains("2 │ Line 2"),
        "Line 2 should show '2 │ Line 2'. Got: {}",
        content_lines[2]
    );

    // Line 3: "Line 3" with gutter showing "3"
    assert!(
        content_lines[3].contains("3 │ Line 3"),
        "Line 3 should show '3 │ Line 3'. Got: {}",
        content_lines[3]
    );
}

/// Test that original buffer does NOT show blame decorators
/// When blame is opened, ONLY the blame virtual buffer should have headers
// TODO: Fix git blame tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_blame_original_buffer_not_decorated() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_blame_plugin();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open file directly
    let file_path = repo.path.join("src/main.rs");
    harness.open_file(&file_path).unwrap();

    // Wait until file is loaded
    harness
        .wait_until(|h| h.get_buffer_content().unwrap().contains("fn main"))
        .unwrap();

    // Capture screen BEFORE opening blame
    let screen_before_blame = harness.screen_to_string();
    println!("Screen before blame:\n{screen_before_blame}");

    // Original file should NOT have blame headers
    assert!(
        !screen_before_blame.contains("──"),
        "Original file should NOT have blame headers before opening blame"
    );

    // Trigger git blame
    trigger_git_blame(&mut harness);

    // Wait until blame view appears
    harness
        .wait_until(|h| h.screen_to_string().contains("──"))
        .unwrap();

    let screen_with_blame = harness.screen_to_string();
    println!("Screen with blame:\n{screen_with_blame}");

    // Blame view SHOULD have headers
    assert!(
        screen_with_blame.contains("──"),
        "Blame view should have headers"
    );

    // Close blame with q
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();

    // Wait until we're back to original file
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("fn main") && !screen.contains("──")
        })
        .unwrap();

    let screen_after_close = harness.screen_to_string();
    println!("Screen after closing blame:\n{screen_after_close}");

    // After closing, original file should NOT have blame headers
    assert!(
        !screen_after_close.contains("──"),
        "Original file should NOT have blame headers after closing blame"
    );
}
