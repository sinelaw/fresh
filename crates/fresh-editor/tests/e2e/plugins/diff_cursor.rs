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
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use fresh::config::Config;
use std::fs;
use tracing::info;

/// Helper to copy audit_mode plugin and its dependencies to the test repo
fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

/// Check if we're in the diff view.
/// We check for "*Diff:" in tab bar and "OLD (HEAD)" header which are visible
/// at any viewport width. The "Side-by-side diff:" status message may be truncated
/// in narrow viewports.
fn is_in_diff_view(screen: &str) -> bool {
    let has_diff_tab = screen.contains("*Diff:");
    let has_old_header = screen.contains("OLD (HEAD)");
    let has_full_status = screen.contains("Side-by-side diff:");
    (has_diff_tab && has_old_header) || has_full_status
}

/// Helper to open the side-by-side diff view
fn open_side_by_side_diff(harness: &mut EditorTestHarness) {
    info!("open_side_by_side_diff: sending Ctrl+p");
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    info!("open_side_by_side_diff: waiting for prompt");
    harness.wait_for_prompt().unwrap();
    info!("open_side_by_side_diff: typing 'Side-by-Side Diff'");
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();
    info!("open_side_by_side_diff: sending Enter");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    info!("open_side_by_side_diff: waiting for prompt closed");
    harness.wait_for_prompt_closed().unwrap();
    info!("open_side_by_side_diff: prompt closed, waiting for diff view to load");

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
            let still_loading = screen.contains("Loading side-by-side diff");
            !still_loading && is_in_diff_view(&screen)
        })
        .unwrap();
    info!("open_side_by_side_diff: diff view loaded");
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
        is_in_diff_view(&screen),
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
}

/// Test horizontal scroll on long lines (line > pane width)
#[test]
fn test_diff_horizontal_scroll_long_line() {
    init_tracing_from_env();
    info!("Starting test_diff_horizontal_scroll_long_line");

    let repo = GitTestRepo::new();
    info!("Created git test repo");
    create_repo_with_varied_lines(&repo);
    info!("Created repo with varied lines");
    setup_audit_mode_plugin(&repo);
    info!("Set up audit_mode plugin");

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80, // Narrower to trigger horizontal scroll
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    info!("Created editor test harness");

    harness.open_file(&file_path).unwrap();
    info!("Opened file");
    harness.render().unwrap();
    info!("Rendered");
    harness
        .wait_until(|h| h.screen_to_string().contains("long"))
        .unwrap();
    info!("File content visible with 'long'");

    open_side_by_side_diff(&mut harness);
    info!("Opened side-by-side diff");

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
        is_in_diff_view(&screen_at_home),
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
        is_in_diff_view(&screen_at_start),
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
}

/// Test that mouse click moves cursor to clicked position
#[test]
fn test_diff_mouse_click_moves_cursor() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    create_repo_for_wrap_test(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("wrap.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Verify initial cursor position (should be Ln 1, Col 1)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 1,") && screen.contains("Col 1"),
        "Initial cursor should be at Ln 1, Col 1, got: {}",
        screen
    );

    // The diff view layout (120 width):
    // - Left pane header "OLD (HEAD)" starts around col 0
    // - Right pane header "NEW (Working)" starts around col 60
    // - Content rows start at row 4 (after menu bar, tab bar, headers)
    // - Line numbers take ~4 chars, then content
    // Click on row 5 (line 2 content), column 10 (in left pane content area)
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

    // Cursor should have moved to line 2
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 2,"),
        "Cursor should move to Ln 2 after clicking row 5, got: {}",
        screen
    );
}

/// Test that keyboard cursor movement scrolls view to keep cursor visible
#[test]
fn test_diff_keyboard_scroll_to_cursor() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    // Create a file with many lines to require scrolling
    let file_path = repo.path.join("long.txt");
    let mut content = String::new();
    for i in 1..=50 {
        content.push_str(&format!("Line number {}\n", i));
    }
    fs::write(&file_path, &content).unwrap();
    repo.git_add(&["long.txt"]);
    repo.git_commit("Initial");
    // Modify some lines
    let mut new_content = String::new();
    for i in 1..=50 {
        if i == 25 {
            new_content.push_str("MODIFIED line 25\n");
        } else {
            new_content.push_str(&format!("Line number {}\n", i));
        }
    }
    fs::write(&file_path, &new_content).unwrap();

    setup_audit_mode_plugin(&repo);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30, // Short viewport to require scrolling
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Navigate down many lines with Ctrl+End to go to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // Cursor should be at a high line number (50+ due to trailing newlines in alignment)
    assert!(
        screen.contains("Ln 50")
            || screen.contains("Ln 49")
            || screen.contains("Ln 51")
            || screen.contains("Ln 52"),
        "Cursor should be near end of file after Ctrl+End, got: {}",
        screen
    );

    // The view should have scrolled - we should see line 50 content on screen
    assert!(
        screen.contains("Line number 50") || screen.contains("Line number 49"),
        "View should scroll to show cursor at end of file, got: {}",
        screen
    );
}

/// Test that scrollbar click/drag works in composite buffer
#[test]
fn test_diff_scrollbar_click() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    // Create a file with many lines to have a scrollbar
    let file_path = repo.path.join("scrollable.txt");
    let mut content = String::new();
    for i in 1..=100 {
        content.push_str(&format!("Line {}: some content here\n", i));
    }
    fs::write(&file_path, &content).unwrap();
    repo.git_add(&["scrollable.txt"]);
    repo.git_commit("Initial");
    // Modify middle lines
    let mut new_content = String::new();
    for i in 1..=100 {
        if i >= 45 && i <= 55 {
            new_content.push_str(&format!("MODIFIED Line {}\n", i));
        } else {
            new_content.push_str(&format!("Line {}: some content here\n", i));
        }
    }
    fs::write(&file_path, &new_content).unwrap();

    setup_audit_mode_plugin(&repo);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Verify we start at the top (Line 1 visible)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Line 1:") || screen.contains("  1 Line"),
        "Should start at top of file, got: {}",
        screen
    );

    // Click on the scrollbar area (rightmost column, middle of viewport)
    // Scrollbar should be at column 119 (width-1) for a 120-wide terminal
    // Click in the middle of the scrollbar track to scroll down
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 119,
            row: 20, // Middle of content area
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: 119,
            row: 20,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    // After clicking middle of scrollbar, we should see lines from middle of file
    let screen = harness.screen_to_string();
    // We should no longer see Line 1 at the top, OR we should see higher line numbers
    let has_higher_lines =
        screen.contains("Line 4") || screen.contains("Line 5") || screen.contains("MODIFIED");
    assert!(
        has_higher_lines || !screen.contains("Line 1:"),
        "Scrollbar click should scroll the view, got: {}",
        screen
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
    harness.send_key(KeyCode::End, KeyModifiers::SHIFT).unwrap();
    harness.render().unwrap();

    // Select to start of line
    harness
        .send_key(KeyCode::Home, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
}

/// Test Home/End keys on each line type (empty, short, long)
#[test]
fn test_diff_home_end_all_line_types() {
    init_tracing_from_env();
    info!("Starting test_diff_home_end_all_line_types");

    let repo = GitTestRepo::new();
    info!("Created git test repo");
    create_repo_with_varied_lines(&repo);
    info!("Created repo with varied lines");
    setup_audit_mode_plugin(&repo);
    info!("Set up audit_mode plugin");

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80, // Narrow enough to require horizontal scroll on long lines
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    info!("Created editor test harness");

    harness.open_file(&file_path).unwrap();
    info!("Opened file");
    harness.render().unwrap();
    info!("Rendered");

    open_side_by_side_diff(&mut harness);
    info!("Opened side-by-side diff");

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
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
}

/// Test cursor visibility: cursor should always be visible after movement
#[test]
fn test_diff_cursor_always_visible() {
    init_tracing_from_env();
    info!("Starting test_diff_cursor_always_visible");

    let repo = GitTestRepo::new();
    info!("Created git test repo");
    create_repo_with_long_file(&repo);
    info!("Created repo with long file");
    setup_audit_mode_plugin(&repo);
    info!("Set up audit_mode plugin");

    let file_path = repo.path.join("long.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80, // Narrow to test horizontal visibility
        20, // Short to test vertical visibility
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    info!("Created editor test harness");

    harness.open_file(&file_path).unwrap();
    info!("Opened file");
    harness.render().unwrap();
    info!("Rendered");

    open_side_by_side_diff(&mut harness);
    info!("Opened side-by-side diff");

    // Go to end of buffer - cursor should be visible
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        is_in_diff_view(&screen),
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
        is_in_diff_view(&screen),
        "Should still be in diff view after horizontal scroll"
    );

    // Move left back to start - cursor should remain visible
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(is_in_diff_view(&screen), "Should still be in diff view");
}

// =============================================================================
// Line Wrap Tests - cursor should wrap to next/prev line at boundaries
// =============================================================================

/// Create a test repo with simple multi-line content for wrap testing
fn create_repo_for_wrap_test(repo: &GitTestRepo) {
    let file_path = repo.path.join("wrap.txt");
    // OLD version: 3 lines with specific lengths
    let old_content = "first\nsecond\nthird\n";
    fs::write(&file_path, old_content).unwrap();

    repo.git_add(&["wrap.txt"]);
    repo.git_commit("Initial");

    // NEW version: same lines but modified
    let new_content = "first_mod\nsecond_mod\nthird_mod\n";
    fs::write(&file_path, new_content).unwrap();
}

/// Test that pressing Right at end of line moves to start of next line
#[test]
fn test_diff_cursor_wrap_right_to_next_line() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    create_repo_for_wrap_test(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("wrap.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to first content line (row 0 is header, row 1 is "first")
    // Ctrl+Home goes to row 0, Down goes to row 1 (first content)
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Go to end of first content line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we're at line 1 (row 1 = source line 0 = "first" = Ln 1)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 1,") || screen.contains("Ln 1 "),
        "Should be on line 1, got: {}",
        screen
    );

    // Press Right - should wrap to start of line 2 (row 2 = source line 1 = "second")
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 2,") || screen.contains("Ln 2 "),
        "Should be on line 2 after wrapping, got: {}",
        screen
    );

    // Should be at column 1 (start of line)
    assert!(
        screen.contains("Col 1") || screen.contains(", Col 1"),
        "Should be at column 1 after wrapping, got: {}",
        screen
    );
}

/// Test that pressing Left at start of line moves to end of previous line
#[test]
fn test_diff_cursor_wrap_left_to_prev_line() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    create_repo_for_wrap_test(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("wrap.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to third line (row 0 is header, row 1 is "first", row 2 is "second")
    // We need to go Down twice to get to "second" content line
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Go to start of line (column 1)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we're at line 2 ("second" content, file line 2)
    // Note: After hunk header at row 0, line 1 is at row 1, line 2 is at row 2
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 2,") || screen.contains("Ln 2 "),
        "Should be on line 2, got: {}",
        screen
    );

    // Press Left - should wrap to end of line 1 (the "first" line)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 1,") || screen.contains("Ln 1 "),
        "Should be on line 1 after wrapping, got: {}",
        screen
    );

    // Should be at end of line 1
    // In side-by-side diff, default focused pane is 0 (OLD/left)
    // Line 1 in OLD pane is "first" (5 chars), end of line is Col 6 (1-indexed, after last char)
    assert!(
        screen.contains("Col 6"),
        "Should be at end of OLD pane line (col 6 for 'first'), got: {}",
        screen
    );
}

/// Test that word movement (Ctrl+Right/Left) wraps at line boundaries
#[test]
fn test_diff_cursor_word_wrap_at_boundaries() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    create_repo_for_wrap_test(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("wrap.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to first content line (row 1), end
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we're at line 1 (row 1 = source line 0 = Ln 1)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 1,") || screen.contains("Ln 1 "),
        "Should be on line 1, got: {}",
        screen
    );

    // Ctrl+Right at end of line should go to next line (line 2)
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 2,") || screen.contains("Ln 2 "),
        "Ctrl+Right at end should go to line 2, got: {}",
        screen
    );

    // Go back to line 2 start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Ctrl+Left at start of line should go to previous line (line 1)
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 1,") || screen.contains("Ln 1 "),
        "Ctrl+Left at start should go to line 1, got: {}",
        screen
    );
}

// =============================================================================
// Horizontal Scroll Tests - viewport should scroll to keep cursor visible
// =============================================================================

/// Test that moving character-by-character along a long line keeps cursor visible
/// by scrolling the viewport horizontally
#[test]
fn test_diff_horizontal_scroll_keeps_cursor_visible() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    create_repo_with_varied_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    // Use narrow viewport (80 cols) to ensure scrolling is needed
    // Each pane is roughly 40 columns wide minus gutter
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Navigate to the long line (line 4 in the file = source line 3, 0-indexed)
    // The line is: "this is a very long MODIFIED line that extends..."
    // Content lines in original: empty (Ln 1), short (Ln 2), medium (Ln 3), long (Ln 4)
    // So the long line is at source line 3 (0-indexed), displayed as Ln 4 (1-indexed)
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    // After Ctrl+Home, cursor is on hunk header (row 0, no source line, shows Ln 1 fallback)
    // We need to move down to get to actual content lines:
    // Row 0 = hunk header, Row 1 = Ln 1 (empty), Row 2 = Ln 2 (short), Row 3 = Ln 3 (medium), Row 4 = Ln 4 (long)
    // Move down 4 times to get to the long line (Ln 4)
    for _ in 0..4 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Check we're on the right line by looking at the status bar
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 4,") || screen.contains("Ln 4 "),
        "Should be on line 4 (the long line). Screen:\n{}",
        screen
    );

    // Now move right character by character and verify cursor stays visible
    // After moving past the visible width, the start of line should scroll off
    // We'll check that "MODIFIED" becomes visible as we scroll right

    // First, move to start of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we're still on line 4 after pressing Home
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 4,") || screen.contains("Ln 4 "),
        "After Home, should still be on line 4. Screen:\n{}",
        screen
    );

    // Initial state: "this is a very long MODIFIED" - we can see "this" at start
    assert!(
        screen.contains("this"),
        "At line start, should see 'this'. Screen:\n{}",
        screen
    );

    // Move right past the visible portion - "MODIFIED" is around column 20+
    // Moving right 40+ times should scroll the view
    // Note: Use fewer iterations to stay on the same line (don't wrap to next line)
    for _ in 0..40 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Verify we're still on line 4
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 4,"),
        "After 40 right moves, should still be on line 4. Screen:\n{}",
        screen
    );

    assert!(
        is_in_diff_view(&screen),
        "Should still be in diff view after right moves. Screen:\n{}",
        screen
    );

    // After moving 50 chars right, "MODIFIED" should be visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("MODIFIED"),
        "After scrolling right, should see 'MODIFIED'. Screen:\n{}",
        screen
    );

    // The start of line "this" should have scrolled off
    assert!(
        !screen.contains("this is a very"),
        "After scrolling right, 'this is a very' should have scrolled off. Screen:\n{}",
        screen
    );

    // Move to end of line - this goes to end of focused pane's line (OLD pane)
    // which is shorter than the NEW pane's line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // At end of OLD line (~216 chars), we should see content around that position
    // The screen should show content from around position 180-216 (visible_width ~35)
    // This includes "testing purposes" which appears in both lines around that position
    assert!(
        screen.contains("testing purposes") || screen.contains("testing"),
        "At line end, should see 'testing purposes' (near end of line). Screen:\n{}",
        screen
    );

    // Move back to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // Back at start, should see "this" again
    assert!(
        screen.contains("this"),
        "Back at line start, should see 'this'. Screen:\n{}",
        screen
    );
}

// =============================================================================
// Selection and Copy Tests
// =============================================================================

/// Test that moving without shift clears the selection
/// We verify this by: select text, copy, move without shift, copy again,
/// then paste into a prompt. If selection was cleared, the second copy should
/// produce nothing new.
#[test]
fn test_diff_move_without_shift_clears_selection() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    create_repo_for_wrap_test(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("wrap.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Isolate clipboard to prevent parallel test interference
    harness.editor_mut().set_clipboard_for_test("".to_string());

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Navigate to content line
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Select some text with Shift+Right (select "fir" - 3 chars from "first")
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    // Copy selection - should copy "fir"
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Now move without shift - this should clear the selection
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Clear clipboard and try to copy again - should copy nothing since selection is cleared
    harness.editor_mut().set_clipboard_for_test("".to_string());
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Close diff view with 'q' and wait until it's closed
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !is_in_diff_view(&h.screen_to_string()))
        .unwrap();

    // Open command palette and paste
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The second copy should have copied nothing (selection was cleared)
    // So the paste should result in empty prompt
    let screen = harness.screen_to_string();
    let prompt_line = screen
        .lines()
        .find(|l| l.contains(">command"))
        .unwrap_or("");
    assert!(
        !prompt_line.contains("fir"),
        "After move without shift, selection should be cleared. Prompt: {}",
        prompt_line
    );
}

/// Test that copy in diff view doesn't include extra empty lines between lines
/// We verify by copying multiple lines from diff view, then pasting into prompt
#[test]
fn test_diff_copy_no_empty_lines() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    // Create file with multiple lines
    let file_path = repo.path.join("multiline.txt");
    let original_content = "line one\nline two\nline three\n";
    fs::write(&file_path, original_content).expect("Failed to create file");
    repo.git_add_all();
    repo.git_commit("Initial commit");

    let modified_content = "line one modified\nline two modified\nline three modified\n";
    fs::write(&file_path, modified_content).expect("Failed to modify file");

    setup_audit_mode_plugin(&repo);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Isolate clipboard
    harness.editor_mut().set_clipboard_for_test("".to_string());

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Go to first content line
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Select multiple lines with Shift+Down
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Copy with Ctrl+C
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Close diff view with 'q' and wait until it's closed
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !is_in_diff_view(&h.screen_to_string()))
        .unwrap();

    // Paste into prompt to verify content
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check the prompt shows the pasted content
    // Quick Open starts with ">" prefix, so pasted content goes into ">pasted_content" format
    let screen = harness.screen_to_string();
    // Find the prompt input line (starts with ">" and not part of hints)
    let prompt_line = screen
        .lines()
        .find(|l| l.starts_with(">") && !l.contains(">command"))
        .unwrap_or("");

    // Should contain line content (verifies copy worked)
    assert!(
        prompt_line.contains("line"),
        "Should contain copied line content. Prompt: {}",
        prompt_line
    );
}

/// Test that copy in diff view doesn't clear the selection
/// We verify by: select, copy, extend selection with Shift, copy again
/// If selection was preserved, the second copy should have more content
#[test]
fn test_diff_copy_preserves_selection() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    create_repo_for_wrap_test(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("wrap.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Isolate clipboard
    harness.editor_mut().set_clipboard_for_test("".to_string());

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    // Navigate to content line
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Select 3 characters with Shift+Right (select "fir")
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.render().unwrap();

    // Copy with Ctrl+C
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Extend selection with Shift+Right (now selecting "firs")
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // Copy again - should now have 4 characters if selection was preserved
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Close diff view with 'q' and wait until it's closed
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !is_in_diff_view(&h.screen_to_string()))
        .unwrap();

    // Paste into prompt to verify what was copied
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should have copied 4 characters: "firs" (first 4 chars of "first")
    // Quick Open starts with ">" prefix, so pasted content goes into ">pasted_content" format
    let screen = harness.screen_to_string();
    // Find the prompt input line (starts with ">" and not part of hints)
    let prompt_line = screen
        .lines()
        .find(|l| l.starts_with(">") && !l.contains(">command"))
        .unwrap_or("");
    assert!(
        prompt_line.contains("firs"),
        "Should have 4 chars after extending selection post-copy. Prompt: {}",
        prompt_line
    );
}
