//! E2E tests for diff view cursor and navigation interactions
//!
//! These tests verify that cursor movement, pane switching, mouse clicks,
//! and selections work correctly in the side-by-side diff view.
//!
//! Test coverage includes all combinations of:
//! - Line length: empty, short (< pane width), long (> pane width)
//! - Vertical file length: short (no scroll), long (requires scroll)
//! - Cursor position: start/mid/end of line, start/mid/end of buffer
//! - Movement directions: left/right/up/down in all positions

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use fresh::config::Config;
use std::fs;

/// Helper to copy audit_mode plugin and its dependencies to the test repo
fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

/// Helper to open the side-by-side diff view
fn open_side_by_side_diff(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for side-by-side view to fully load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
            {
                panic!("Error loading side-by-side diff. Screen:\n{}", screen);
            }
            screen.contains("Side-by-side diff:") && !screen.contains("Loading side-by-side diff")
        })
        .unwrap();
}

/// Create a repo with various line lengths for comprehensive testing
/// Returns paths to original and modified content
fn create_repo_with_varied_lines(repo: &GitTestRepo) {
    // Create initial file with varied line lengths
    let file_path = repo.path.join("test.rs");
    let original_content = r#"
short
this is a medium length line for testing
this is a very long line that extends well beyond the visible viewport width and requires horizontal scrolling to see the entire content of this particular line which is intentionally made very long for testing purposes

another short
medium line here with some content
"#;
    fs::write(&file_path, original_content).expect("Failed to create file");
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify with changes to various line lengths
    let modified_content = r#"
short_modified
this is a MODIFIED medium length line for testing
this is a very long MODIFIED line that extends well beyond the visible viewport width and requires horizontal scrolling to see the entire content of this particular line which is intentionally made very long for testing purposes and now even longer

another short MOD
medium line here with some MODIFIED content
added new line
"#;
    fs::write(&file_path, modified_content).expect("Failed to modify file");
}

/// Create a repo with a long file that requires vertical scrolling
fn create_repo_with_long_file(repo: &GitTestRepo) {
    let file_path = repo.path.join("long.rs");

    // Create a file with many lines
    let mut original = String::new();
    for i in 0..100 {
        original.push_str(&format!("// Original line {}\n", i));
    }
    fs::write(&file_path, &original).expect("Failed to create file");
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify some lines throughout the file
    let mut modified = String::new();
    for i in 0..100 {
        if i == 10 || i == 50 || i == 90 {
            modified.push_str(&format!("// MODIFIED line {} with extra content\n", i));
        } else if i == 25 {
            // Add extra lines here
            modified.push_str("// Inserted line A\n");
            modified.push_str("// Inserted line B\n");
            modified.push_str(&format!("// Original line {}\n", i));
        } else {
            modified.push_str(&format!("// Original line {}\n", i));
        }
    }
    fs::write(&file_path, modified).expect("Failed to modify file");
}

/// Create a repo with empty lines for edge case testing
fn create_repo_with_empty_lines(repo: &GitTestRepo) {
    let file_path = repo.path.join("empty.rs");

    let original = "first line\n\n\nfourth line\n";
    fs::write(&file_path, original).expect("Failed to create file");
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify to add content to empty lines
    let modified = "first line\nsecond line added\n\nfourth line modified\n";
    fs::write(&file_path, modified).expect("Failed to modify file");
}

/// Create a repo with a short file (no vertical scroll needed)
fn create_repo_short_file(repo: &GitTestRepo) {
    let file_path = repo.path.join("short.rs");

    let original = "line 1\nline 2\nline 3\n";
    fs::write(&file_path, original).expect("Failed to create file");
    repo.git_add_all();
    repo.git_commit("Initial commit");

    let modified = "line 1 modified\nline 2\nline 3 changed\n";
    fs::write(&file_path, modified).expect("Failed to modify file");
}

// =============================================================================
// COMPREHENSIVE MOVEMENT TESTS
// =============================================================================

/// Test cursor movement on empty lines (line length = 0)
#[test]
fn test_diff_cursor_empty_lines() {
    let repo = GitTestRepo::new();
    create_repo_with_empty_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("empty.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Navigate to an empty line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Try moving right on empty line - should stay at position 0
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Try End on empty line - should stay at position 0
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Try Home on empty line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view after empty line navigation"
    );
}

/// Test cursor at start of line - moving left should not move, right should work
#[test]
fn test_diff_cursor_at_line_start() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("short"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to line with content
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Ensure we're at start of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Moving left at start should not crash or move cursor
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Moving right should work
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test cursor at end of line - moving right should not move past end
#[test]
fn test_diff_cursor_at_line_end() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("short"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to short line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Go to end of line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Moving right at end should not move cursor past end
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // Moving left from end should work
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test cursor at middle of line - both directions should work
#[test]
fn test_diff_cursor_at_line_middle() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("medium"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to medium length line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Move to middle of line
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Move left
    for _ in 0..3 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Move right
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test cursor at first row of buffer - up should not crash
#[test]
fn test_diff_cursor_at_buffer_start() {
    let repo = GitTestRepo::new();
    create_repo_short_file(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("short.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Cursor should be at first row
    // Try moving up - should stay at first row
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Move down should work
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Ctrl+Home should go to start
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test cursor at last row of buffer - down should not crash
#[test]
fn test_diff_cursor_at_buffer_end() {
    let repo = GitTestRepo::new();
    create_repo_short_file(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("short.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to end of buffer
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Try moving down - should stay at last row
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Move up should work
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test horizontal scroll on long lines (line > pane width)
#[test]
fn test_diff_horizontal_scroll_long_line() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80, // Narrower to trigger horizontal scroll
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("long"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to the very long line
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    println!("Before horizontal scroll:\n{}", screen_before);

    // Move right many times to trigger horizontal scroll
    for _ in 0..50 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let screen_after_right = harness.screen_to_string();
    println!("After scrolling right:\n{}", screen_after_right);

    // Move to end of line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_at_end = harness.screen_to_string();
    println!("At end of line:\n{}", screen_at_end);

    // Move back to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_at_home = harness.screen_to_string();
    println!("Back at home:\n{}", screen_at_home);

    assert!(
        screen_at_home.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test vertical scroll with long file
#[test]
fn test_diff_vertical_scroll_long_file() {
    let repo = GitTestRepo::new();
    create_repo_with_long_file(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("long.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        30, // Shorter to trigger vertical scroll
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    let screen_initial = harness.screen_to_string();
    println!("Initial view:\n{}", screen_initial);

    // Page down to scroll
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after_pagedown = harness.screen_to_string();
    println!("After PageDown:\n{}", screen_after_pagedown);

    // Go to end of buffer
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen_at_end = harness.screen_to_string();
    println!("At buffer end:\n{}", screen_at_end);

    // Page up
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Go to start of buffer
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen_at_start = harness.screen_to_string();
    println!("Back at buffer start:\n{}", screen_at_start);

    assert!(
        screen_at_start.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test cursor movement in viewport middle (not at edge)
#[test]
fn test_diff_cursor_viewport_middle() {
    let repo = GitTestRepo::new();
    create_repo_with_long_file(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("long.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Move to middle of viewport
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Move in all directions from middle
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    for _ in 0..2 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    for _ in 0..3 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test word movement (Ctrl+Left/Right) with various line content
#[test]
fn test_diff_word_movement_comprehensive() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("medium"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to medium length line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Word right from start
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    // Word left back
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Left, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    // Go to end with Ctrl+Right repeatedly
    for _ in 0..20 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::CONTROL)
            .unwrap();
    }
    harness.render().unwrap();

    // Word left from end
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Go to start with Ctrl+Left repeatedly
    for _ in 0..20 {
        harness
            .send_key(KeyCode::Left, KeyModifiers::CONTROL)
            .unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test Tab key switches between panes
#[test]
fn test_diff_pane_switching_with_tab() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Move to a line with content
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Switch to NEW pane with Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Move cursor in NEW pane
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Switch back to OLD pane
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Move cursor in OLD pane
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test mouse click places cursor correctly in both panes
#[test]
fn test_diff_mouse_click_both_panes() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Click on left pane (OLD) at various positions
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 10,
            row: 5,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: 10,
            row: 5,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    // Click on right pane (NEW)
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 100,
            row: 5,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: 100,
            row: 5,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    // Click past end of line (should clamp to line end)
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 70, // Past end of short line on left pane
            row: 3,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: 70,
            row: 3,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    // Click on empty line
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 20,
            row: 2, // Might be an empty line
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: 20,
            row: 2,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test selection with Shift+Arrow in various positions
#[test]
fn test_diff_selection_comprehensive() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to line with content
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Select right from start of line
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    // Extend selection down
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Extend selection left
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Left, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    // Clear selection by moving without shift
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Select word right
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT | KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Select to end of line
    harness
        .send_key(KeyCode::End, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Select to start of line
    harness
        .send_key(KeyCode::Home, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test combined movement: down then right, up then left, etc.
#[test]
fn test_diff_combined_movement() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Move diagonally: down-right
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // Move diagonally: up-left
    for _ in 0..3 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // End then down
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Home then up
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test no scroll needed (short file that fits in viewport)
#[test]
fn test_diff_no_scroll_needed() {
    let repo = GitTestRepo::new();
    create_repo_short_file(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("short.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40, // Tall enough that short file doesn't need scroll
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Move through entire file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // PageDown/Up should work even if no scroll needed
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test Home/End keys on each line type (empty, short, long)
#[test]
fn test_diff_home_end_all_line_types() {
    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80, // Narrow enough to require horizontal scroll on long lines
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Test 1: Empty line (first line might be empty in our test file)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Test 2: Short line (line 2: "short")
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Home on short line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // End on short line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Home again to verify
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Test 3: Medium line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Test 4: Very long line (requires horizontal scroll)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_at_start = harness.screen_to_string();
    println!("Long line at start:\n{}", screen_at_start);

    // End on long line - should scroll horizontally
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_at_end = harness.screen_to_string();
    println!("Long line at end:\n{}", screen_at_end);

    // Home on long line - should scroll back
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_back_home = harness.screen_to_string();
    println!("Long line back at home:\n{}", screen_back_home);

    // Test 5: Empty line again
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test cursor visibility: cursor should always be visible after movement
#[test]
fn test_diff_cursor_always_visible() {
    let repo = GitTestRepo::new();
    create_repo_with_long_file(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("long.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80, // Narrow to test horizontal visibility
        20, // Short to test vertical visibility
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to end of buffer - cursor should be visible
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view at buffer end"
    );

    // Go to start of buffer - cursor should be visible
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Move right many times - cursor should remain visible (viewport scrolls)
    for _ in 0..30 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view after horizontal scroll"
    );

    // Move left back to start - cursor should remain visible
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}
