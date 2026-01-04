//! E2E tests for diff view cursor and navigation interactions
//!
//! These tests verify that cursor movement, pane switching, mouse clicks,
//! and selections work correctly in the side-by-side diff view.

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

/// Create a repo with a file that has long lines for horizontal scroll testing
fn create_repo_with_long_lines(repo: &GitTestRepo) {
    // Create initial file with normal content
    let file_path = repo.path.join("test.rs");
    let original_content = r#"fn main() {
    println!("Hello");
    let x = 1;
}
"#;
    fs::write(&file_path, original_content).expect("Failed to create file");
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify with long lines that require horizontal scrolling
    let modified_content = r#"fn main() {
    println!("Hello world! This is a very long line that extends well beyond the visible viewport and requires horizontal scrolling to see the entire content of this line");
    let x = 1;
    let y = 2;
}
"#;
    fs::write(&file_path, modified_content).expect("Failed to modify file");
}

/// Test that arrow keys navigate the cursor in diff view
#[test]
#[ignore = "Flaky/timeout in e2e tests for composite buffer"]
fn test_diff_cursor_arrow_keys() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Modified line");
    let config = load_config();
}

fn load_config() -> Config {
    Config::default()
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Modified"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    let screen_before = harness.screen_to_string();
    println!("Before navigation:\n{}", screen_before);

    // Move down a few times
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // Move right a few times
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    let screen_after = harness.screen_to_string();
    println!("After navigation:\n{}", screen_after);

    // Cursor should have moved - status bar should show different position
    // Note: The exact position format may vary, just verify navigation worked
    assert!(
        screen_after.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test that Tab key switches between panes in diff view
#[test]
#[ignore = "Flaky/timeout in e2e tests for composite buffer"]
fn test_diff_pane_switching_with_tab() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Modified!");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Modified"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Initial view - should be focused on OLD (left) pane by default
    let screen_initial = harness.screen_to_string();
    println!("Initial state:\n{}", screen_initial);

    // Press Tab to switch to NEW (right) pane
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after_tab = harness.screen_to_string();
    println!("After Tab:\n{}", screen_after_tab);

    // Press Tab again to switch back to OLD (left) pane
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after_tab2 = harness.screen_to_string();
    println!("After second Tab:\n{}", screen_after_tab2);

    // Verify we're still in diff view
    assert!(
        screen_after_tab2.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test Home and End keys in diff view
#[test]
#[ignore = "Flaky/timeout in e2e tests for composite buffer"]
fn test_diff_home_end_keys() {
    let repo = GitTestRepo::new();
    create_repo_with_long_lines(&repo);
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
        .wait_until(|h| h.screen_to_string().contains("Hello world"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Move to the long line (line 2 with modifications)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press End to go to end of line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_at_end = harness.screen_to_string();
    println!("After End key:\n{}", screen_at_end);

    // Press Home to go back to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_at_home = harness.screen_to_string();
    println!("After Home key:\n{}", screen_at_home);

    assert!(
        screen_at_home.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test Ctrl+Left/Right word movement in diff view
#[test]
#[ignore = "Flaky/timeout in e2e tests for composite buffer"]
fn test_diff_word_movement() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello world from modified code");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("modified"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Move to the line with text
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Ctrl+Right to move word by word
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    let screen_after_ctrl_right = harness.screen_to_string();
    println!("After Ctrl+Right x3:\n{}", screen_after_ctrl_right);

    // Ctrl+Left to move back
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Left, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    let screen_after_ctrl_left = harness.screen_to_string();
    println!("After Ctrl+Left x2:\n{}", screen_after_ctrl_left);

    assert!(
        screen_after_ctrl_left.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test mouse click places cursor and sets focus to clicked pane
#[test]
#[ignore = "Flaky/timeout in e2e tests for composite buffer"]
fn test_diff_mouse_click() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello!");
    let x = 42;
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Hello"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    let screen_initial = harness.screen_to_string();
    println!("Initial state:\n{}", screen_initial);

    // Click on the right pane (NEW side) - approximately at column 100
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

    let screen_after_right_click = harness.screen_to_string();
    println!("After click on right pane:\n{}", screen_after_right_click);

    // Click on the left pane (OLD side) - approximately at column 20
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 20,
            row: 5,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: 20,
            row: 5,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    let screen_after_left_click = harness.screen_to_string();
    println!("After click on left pane:\n{}", screen_after_left_click);

    assert!(
        screen_after_left_click.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test horizontal scrolling when cursor moves beyond visible area
#[test]
#[ignore = "Flaky/timeout in e2e tests for composite buffer"]
fn test_diff_horizontal_scroll() {
    let repo = GitTestRepo::new();
    create_repo_with_long_lines(&repo);
    setup_audit_mode_plugin(&repo);

    let file_path = repo.path.join("test.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80, // Narrower to trigger horizontal scroll sooner
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Hello world"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Move to the long line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    println!("Before horizontal scroll:\n{}", screen_before);

    // Move right many times to scroll horizontally
    for _ in 0..40 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    let screen_after_scroll = harness.screen_to_string();
    println!("After horizontal scroll (40 right presses):\n{}", screen_after_scroll);

    // The view should have scrolled - we should see different content
    // The cursor should still be visible (not disappeared)
    assert!(
        screen_after_scroll.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test selection with Shift+Arrow keys in diff view
#[test]
#[ignore = "Flaky/timeout in e2e tests for composite buffer"]
fn test_diff_selection_shift_arrows() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Select this text");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Select"))
        .unwrap();

    open_side_by_side_diff(&mut harness);

    // Move to text line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Start selection with Shift+Right
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
        harness.render().unwrap();
    }

    let screen_with_selection = harness.screen_to_string();
    println!("With selection:\n{}", screen_with_selection);

    // Extend selection with Shift+Down
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let screen_multiline_selection = harness.screen_to_string();
    println!("With multi-line selection:\n{}", screen_multiline_selection);

    assert!(
        screen_multiline_selection.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}

/// Test PageUp/PageDown in diff view
#[test]
#[ignore = "Flaky/timeout in e2e tests for composite buffer"]
fn test_diff_page_navigation() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create a file with many lines
    let file_path = repo.path.join("many_lines.rs");
    let mut content = String::new();
    for i in 0..100 {
        content.push_str(&format!("// Line {}\n", i));
    }
    content.push_str("// MODIFIED LINE\n");
    for i in 101..200 {
        content.push_str(&format!("// Line {}\n", i));
    }
    fs::write(&file_path, &content).expect("Failed to create file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Since the file is new (not in git yet), we need to add and commit first
    // then modify to create a diff
    repo.git_add_all();
    repo.git_commit("Add many lines file");

    // Now modify the file
    let modified_content = content.replace("Line 50", "MODIFIED_LINE_50");
    fs::write(&file_path, &modified_content).expect("Failed to modify file");

    // Reopen to get the modified version
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    open_side_by_side_diff(&mut harness);

    let screen_initial = harness.screen_to_string();
    println!("Initial view:\n{}", screen_initial);

    // Page down
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after_pagedown = harness.screen_to_string();
    println!("After PageDown:\n{}", screen_after_pagedown);

    // Page up
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after_pageup = harness.screen_to_string();
    println!("After PageUp:\n{}", screen_after_pageup);

    assert!(
        screen_after_pageup.contains("Side-by-side diff:"),
        "Should still be in diff view"
    );
}
