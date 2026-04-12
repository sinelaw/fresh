//! E2E tests for the Review Diff files panel:
//!
//! 1. Clicking on a file entry in the left panel should select that file.
//! 2. Keyboard navigation (Up/Down) should auto-scroll the files panel
//!    to keep the selected entry visible.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

fn open_review_diff(harness: &mut EditorTestHarness) -> String {
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

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            if screen.contains("TypeError") || screen.contains("Error:") {
                panic!("Error loading review diff. Screen:\n{}", screen);
            }
            screen.contains("GIT STATUS") && screen.contains("DIFF")
        })
        .unwrap();

    harness.screen_to_string()
}

/// Create a repo with many modified files to force the files panel to scroll.
fn repo_with_many_modifications(count: usize) -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // Create and commit initial files
    let src = repo.path.join("src");
    fs::create_dir_all(&src).unwrap();
    for i in 1..=count {
        let path = src.join(format!("file_{}.txt", i));
        fs::write(
            &path,
            format!("original content of file {}\nline 2\nline 3\n", i),
        )
        .unwrap();
    }
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify all files (creates unstaged changes)
    for i in 1..=count {
        let path = src.join(format!("file_{}.txt", i));
        fs::write(
            &path,
            format!(
                "original content of file {}\nline 2\nline 3\nmodified line\n",
                i
            ),
        )
        .unwrap();
    }

    repo
}

// ---------------------------------------------------------------------------
// Bug: click on entry in files panel does not select that file
// ---------------------------------------------------------------------------

#[test]
fn test_click_on_file_entry_selects_that_file() {
    init_tracing_from_env();

    let repo = repo_with_many_modifications(5);
    let any_file = repo.path.join("src/file_1.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&any_file).unwrap();
    harness.render().unwrap();
    let _ = open_review_diff(&mut harness);

    // The first file should be selected initially.
    let screen = harness.screen_to_string();
    println!("Initial screen:\n{}", screen);

    // Find the position of a *different* file (not the first one) on screen.
    // We look for "file_3.txt" which should be a few rows down.
    let target_file = "file_3.txt";
    let (col, row) = harness
        .find_text_on_screen(target_file)
        .unwrap_or_else(|| panic!("{} not found on screen:\n{}", target_file, screen));

    // Verify this is NOT the currently selected file (diff panel shows a different file).
    assert!(
        !screen.contains(&format!("DIFF FOR src/{}", target_file)),
        "Expected {} to NOT be initially selected. Screen:\n{}",
        target_file,
        screen
    );

    // Click on the target file in the files panel.
    harness.mouse_click(col, row).unwrap();

    // Wait for the diff panel to update to the clicked file.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains(&format!("DIFF FOR src/{}", target_file))
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("After click screen:\n{}", screen);

    // The selection indicator ">" should be on the clicked file's row.
    assert!(
        screen.contains(&format!(">M  src/{}", target_file)),
        "Expected selection indicator on {}. Screen:\n{}",
        target_file,
        screen
    );
}

/// Clicking on a section header (e.g. "▸ Changes") should NOT change the
/// selected file.
#[test]
fn test_click_on_section_header_does_not_change_selection() {
    init_tracing_from_env();

    let repo = repo_with_many_modifications(3);
    let any_file = repo.path.join("src/file_1.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&any_file).unwrap();
    harness.render().unwrap();
    let _ = open_review_diff(&mut harness);

    let screen = harness.screen_to_string();
    println!("Initial screen:\n{}", screen);

    // Remember which file is currently shown in the diff panel.
    let initial_diff_header = screen
        .lines()
        .find(|l| l.contains("DIFF FOR"))
        .unwrap_or("")
        .to_string();

    // Find the "Changes" section header on screen.
    let header_text = "Changes";
    let (col, row) = harness
        .find_text_on_screen(header_text)
        .unwrap_or_else(|| panic!("'{}' not found on screen:\n{}", header_text, screen));

    // Click on the section header.
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let new_diff_header = screen
        .lines()
        .find(|l| l.contains("DIFF FOR"))
        .unwrap_or("")
        .to_string();

    // The diff header should remain unchanged — clicking a section header is a no-op.
    assert_eq!(
        initial_diff_header, new_diff_header,
        "Clicking section header should not change selection. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// Bug: keyboard navigation does not auto-scroll the files panel
// ---------------------------------------------------------------------------

#[test]
fn test_keyboard_down_scrolls_files_panel_into_view() {
    init_tracing_from_env();

    // Create enough files that the panel must scroll in a small terminal.
    let repo = repo_with_many_modifications(20);
    let any_file = repo.path.join("src/file_1.txt");

    // Use a small terminal height so the files panel cannot show all entries.
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        18,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&any_file).unwrap();
    harness.render().unwrap();
    let _ = open_review_diff(&mut harness);

    // Navigate down until we're well past the visible area.
    // With 20 files and ~14 content rows, after 19 presses the selected file
    // would be off-screen without scrolling.
    for _ in 0..19 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // The last file should now be selected (shown in diff header).
            // Alphabetically: file_1, file_10..19, file_2, file_20, file_3..9
            // Index 19 (the 20th and last file) is file_9.txt.
            s.contains("DIFF FOR src/file_9.txt")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("After navigating down screen:\n{}", screen);

    // The selected file must be visible on screen (the panel scrolled to show it).
    assert!(
        screen.contains(">M  src/file_9.txt"),
        "Selected file should be visible (panel should auto-scroll). Screen:\n{}",
        screen
    );
}

#[test]
fn test_keyboard_up_scrolls_files_panel_into_view() {
    init_tracing_from_env();

    let repo = repo_with_many_modifications(20);
    let any_file = repo.path.join("src/file_1.txt");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        18,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&any_file).unwrap();
    harness.render().unwrap();
    let _ = open_review_diff(&mut harness);

    // Navigate to the end first.
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Last file should be selected — files sort as 1,10,11,...,19,2,20,...9
            // file_9.txt is the last one alphabetically
            s.contains(">M  src/file_9.txt")
        })
        .unwrap();

    // Now navigate back to the top.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("DIFF FOR src/file_1.txt")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("After navigating back to top:\n{}", screen);

    // The first file must be visible again (panel scrolled back up).
    assert!(
        screen.contains(">M  src/file_1.txt"),
        "First file should be visible after Home. Screen:\n{}",
        screen
    );

    // The GIT STATUS header should be visible too (we're at the top).
    assert!(
        screen.contains("GIT STATUS"),
        "GIT STATUS header should be visible at top. Screen:\n{}",
        screen
    );
}
