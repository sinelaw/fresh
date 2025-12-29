//! E2E tests for audit_mode (Review Diff) plugin

use crate::common::git_test_helper::{DirGuard, GitTestRepo};
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::PathBuf;

/// Helper to copy audit_mode plugin and its dependencies to the test repo
fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");

    let project_root = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .expect("CARGO_MANIFEST_DIR not set");

    // Copy audit_mode.ts plugin
    let audit_mode_src = project_root.join("plugins/audit_mode.ts");
    let audit_mode_dst = plugins_dir.join("audit_mode.ts");
    fs::copy(&audit_mode_src, &audit_mode_dst).unwrap_or_else(|e| {
        panic!(
            "Failed to copy audit_mode.ts from {:?}: {}",
            audit_mode_src, e
        )
    });

    // Copy plugins/lib directory (contains virtual-buffer-factory.ts and fresh.d.ts)
    let lib_src = project_root.join("plugins/lib");
    let lib_dst = plugins_dir.join("lib");
    if lib_src.exists() {
        fs::create_dir_all(&lib_dst).expect("Failed to create plugins/lib directory");
        for entry in fs::read_dir(&lib_src).expect("Failed to read plugins/lib") {
            let entry = entry.expect("Failed to read directory entry");
            let src_path = entry.path();
            let file_name = entry.file_name();
            let dst_path = lib_dst.join(&file_name);
            fs::copy(&src_path, &dst_path).unwrap_or_else(|e| {
                panic!("Failed to copy {:?} to {:?}: {}", src_path, dst_path, e)
            });
        }
    }
}

/// Test that opening the diff view works without errors
/// This test reproduces the addOverlay TypeError that occurred when
/// the plugin passed parameters in the wrong order
#[test]
fn test_review_diff_opens_without_error() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create an initial commit
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file to create uncommitted changes
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello, modified world!");
    let config = load_config();
    start_server(config);
    // New comment line
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the modified file
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    // Verify the file is open
    harness
        .wait_until(|h| h.screen_to_string().contains("modified world"))
        .unwrap();

    // Trigger the Review Diff command via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the Review Diff async operation to complete
    // The status bar changes from "Generating Review Diff Stream..." to showing hunk count
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Wait until we're no longer generating the diff stream
            !screen.contains("Generating Review Diff Stream")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Review Diff screen:\n{}", screen);

    // The diff view should show without errors
    // Check that we don't see an error about addOverlay
    assert!(
        !screen.contains("expected i32"),
        "Should not show addOverlay type error. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("TypeError"),
        "Should not show TypeError. Screen:\n{}",
        screen
    );

    // Should show something related to the diff - either the split view or content
    assert!(
        screen.contains("main.rs")
            || screen.contains("modified world")
            || screen.contains("OLD")
            || screen.contains("Review"),
        "Should show diff-related content. Screen:\n{}",
        screen
    );
}

/// Test that the diff view displays hunks correctly
#[test]
fn test_review_diff_shows_hunks() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create an initial commit
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file to create uncommitted changes
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello, CHANGED!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
    println!("New line added");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the modified file
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Trigger Review Diff via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the Review Diff async operation to complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Generating Review Diff Stream")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Hunks screen:\n{}", screen);

    // Should not have any TypeError
    assert!(
        !screen.contains("TypeError"),
        "Should not show any TypeError. Screen:\n{}",
        screen
    );
}

/// Test that the side-by-side diff view (drill-down) works with synchronized scrolling
/// This test verifies that setSplitScroll is available in the editor API
#[test]
fn test_review_diff_side_by_side_view() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create an initial commit
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file to create uncommitted changes
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello, CHANGED!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
    println!("New line added");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the modified file
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Trigger Review Diff via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the Review Diff async operation to complete and hunks to be displayed
    // The status bar shows hunk count when done: "Review Diff: N hunks"
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Generating Review Diff Stream") && screen.contains("hunks")
        })
        .unwrap();

    let screen_before_drill = harness.screen_to_string();
    println!("Before drill-down:\n{}", screen_before_drill);

    // Now drill down into a hunk to open the side-by-side view
    // Press Enter to drill down
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for side-by-side view to open
    // The drill-down creates a split with "[OLD ◀]" in the tab name
    // Or if the operation is async, wait a bit for it to complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Either we see the OLD marker from the split, or the file was opened
            screen.contains("[OLD") || screen.contains("main.rs ×")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Side-by-side screen:\n{}", screen);

    // Should not have any TypeError about setSplitScroll
    assert!(
        !screen.contains("setSplitScroll is not a function"),
        "setSplitScroll should be available. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("TypeError"),
        "Should not show any TypeError. Screen:\n{}",
        screen
    );
}
