//! E2E tests reproducing bugs from the combined UX report
//! (REVIEW_DIFF_COMBINED_UX_REPORT.md).
//!
//! Each test is named after the bug it reproduces and is expected to
//! **fail** (or demonstrate the broken behavior) until the underlying
//! bug is fixed.  Once fixed, the test becomes the regression guard.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Copy audit_mode plugin and its library into the test repo.
fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

/// Open Review Diff via command palette and wait for it to load.
/// Returns the initial screen string.
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

/// Create a standard repo with one committed file and one unstaged modification.
/// Returns (repo, path_to_modified_file).
fn repo_with_one_modification() -> (GitTestRepo, std::path::PathBuf) {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs = repo.path.join("src/main.rs");
    fs::write(&main_rs, "fn main() {\n    println!(\"CHANGED\");\n}\n").unwrap();
    (repo, main_rs)
}

/// Create a repo with multiple modified files (for multi-hunk / navigation tests).
fn repo_with_multi_hunk_file() -> (GitTestRepo, std::path::PathBuf) {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify main.rs to have multiple hunks (changes in two separate regions)
    let main_rs = repo.path.join("src/main.rs");
    let content = r#"fn main() {
    println!("Hello, HUNK_ONE!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("HUNK_TWO server started");
    println!("Extra line added");
}
"#;
    fs::write(&main_rs, content).unwrap();
    (repo, main_rs)
}

// ---------------------------------------------------------------------------
// BUG-1: CompositeInputRouter dead code — side-by-side vim keys broken
// ---------------------------------------------------------------------------

/// BUG-1: In the side-by-side diff view, pressing `j` should scroll down
/// but instead produces "Editing disabled in this buffer" because the
/// CompositeInputRouter is never wired into the key dispatch pipeline.
#[test]
fn test_bug1_side_by_side_vim_keys_produce_editing_disabled() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Drill down into side-by-side view
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Wait for the side-by-side view or the loading message to finish
            (s.contains("OLD (HEAD)") || s.contains("*Diff:"))
                && !s.contains("Loading side-by-side diff")
        })
        .unwrap();

    // Press `j` — this should scroll down, not show "Editing disabled"
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("BUG-1 screen after j:\n{}", screen);

    assert!(
        !screen.contains("Editing disabled"),
        "BUG-1: `j` in side-by-side diff should scroll, not show \
         'Editing disabled'. CompositeInputRouter is not wired into \
         key dispatch. Screen:\n{}",
        screen
    );
}

/// BUG-1 (cont): `Escape` should close the side-by-side view, but it
/// does nothing because the CompositeInputRouter is dead code.
#[test]
fn test_bug1_side_by_side_escape_does_not_close() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Drill down into side-by-side view
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            (s.contains("OLD (HEAD)") || s.contains("*Diff:"))
                && !s.contains("Loading side-by-side diff")
        })
        .unwrap();

    // Press Escape — should close the side-by-side view
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("BUG-1 escape screen:\n{}", screen);

    // The side-by-side view should be closed — no more "OLD (HEAD)" or "*Diff:" tab
    assert!(
        !screen.contains("OLD (HEAD)"),
        "BUG-1: Escape in side-by-side diff should close the composite view. \
         CompositeInputRouter is dead code. Screen:\n{}",
        screen
    );
}

/// BUG-1 (cont): `Tab` should switch panes in side-by-side view.
#[test]
fn test_bug1_side_by_side_tab_does_not_switch_pane() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Drill down
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            (s.contains("OLD (HEAD)") || s.contains("*Diff:"))
                && !s.contains("Loading side-by-side diff")
        })
        .unwrap();

    // Press Tab — should switch pane, not show "Editing disabled"
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("BUG-1 tab screen:\n{}", screen);

    assert!(
        !screen.contains("Editing disabled"),
        "BUG-1: Tab in side-by-side diff should switch pane, not show \
         'Editing disabled'. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// BUG-2: Terminal resize destroys Review Diff layout
// ---------------------------------------------------------------------------

/// BUG-2: Resizing the terminal while in Review Diff mode causes the
/// toolbar, header, separator, and content to disappear.  The layout does
/// not recover even after resizing back.
#[test]
fn test_bug2_resize_destroys_review_diff_layout() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let screen_before = open_review_diff(&mut harness);

    // Sanity: we have the full layout
    assert!(screen_before.contains("GIT STATUS"), "pre-check");
    assert!(screen_before.contains("DIFF"), "pre-check");

    // Resize down
    harness.resize(80, 24).unwrap();

    // Process async plugin commands from the resize handler
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("GIT STATUS") || s.contains("DIFF")
        })
        .unwrap();

    // Resize back to original
    harness.resize(120, 40).unwrap();

    // Wait for the layout to be rebuilt after resize-back
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("GIT STATUS") && s.contains("DIFF")
        })
        .unwrap();

    let screen_after = harness.screen_to_string();
    println!("BUG-2 screen after resize cycle:\n{}", screen_after);

    // The GIT STATUS header and DIFF panel should still be visible
    assert!(
        screen_after.contains("GIT STATUS"),
        "BUG-2: GIT STATUS header missing after resize cycle. The review \
         diff layout was destroyed. Screen:\n{}",
        screen_after
    );
    assert!(
        screen_after.contains("DIFF"),
        "BUG-2: DIFF panel missing after resize cycle. Screen:\n{}",
        screen_after
    );
}

// ---------------------------------------------------------------------------
// BUG-3: File Explorer steals focus from Review Diff on launch
// ---------------------------------------------------------------------------

/// BUG-3: When File Explorer is open, opening Review Diff does not transfer
/// focus.  `j` is captured by the File Explorer instead of navigating the
/// review diff file list.
#[test]
fn test_bug3_file_explorer_steals_review_diff_keys() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Open File Explorer with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Now open Review Diff while File Explorer is focused
    let screen = open_review_diff(&mut harness);

    // Verify review diff loaded
    assert!(screen.contains("GIT STATUS"), "Review diff should load");

    // Press j — should navigate the review diff file list,
    // NOT trigger the File Explorer quick-search
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after_j = harness.screen_to_string();
    println!("BUG-3 screen after j:\n{}", screen_after_j);

    // The File Explorer title should NOT change to "/j" (quick-search mode)
    assert!(
        !screen_after_j.contains("/j"),
        "BUG-3: File Explorer intercepted `j` key instead of review diff. \
         Review Diff should auto-focus when opened. Screen:\n{}",
        screen_after_j
    );
}

// ---------------------------------------------------------------------------
// BUG-4: Hunk navigation (n/p) non-functional in diff panel
// ---------------------------------------------------------------------------

/// BUG-4: Pressing `n` in the diff panel should jump to the next hunk
/// header, but the cursor stays at its current position.
#[test]
fn test_bug4_hunk_navigation_n_does_not_move_cursor() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_multi_hunk_file();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("HUNK_ONE"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Switch focus to the diff panel with Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Record the screen before pressing n
    let _screen_before_n = harness.screen_to_string();

    // Press Home to go to line 1 of the diff
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Helper to extract the line number from the status bar ("Ln X, Col Y")
    fn extract_ln(screen: &str) -> Option<usize> {
        screen.lines().find_map(|l| {
            if let Some(idx) = l.find("Ln ") {
                let rest = &l[idx + 3..];
                let num_str: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
                num_str.parse().ok()
            } else {
                None
            }
        })
    }

    let screen_at_home = harness.screen_to_string();
    let ln_before = extract_ln(&screen_at_home).expect("status bar should show Ln");

    // Press n to jump to next hunk — uses setBufferCursor (O(1)) to move
    // the cursor directly to the hunk header byte offset.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();

    // Give async commands time to process
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
    }

    let screen_after_n = harness.screen_to_string();
    let ln_after = extract_ln(&screen_after_n).expect("status bar should show Ln after n");

    println!("BUG-4 before n: Ln {}", ln_before);
    println!("BUG-4 after n:  Ln {}", ln_after);

    // The cursor line should have jumped forward to the next hunk header
    // (the multi-hunk file has two separate @@ regions, so Ln should
    // increase by more than 1).
    assert!(
        ln_after > ln_before,
        "BUG-4: Pressing `n` should move cursor forward to next hunk. \
         Ln before={}, Ln after={}. Screen:\n{}",
        ln_before,
        ln_after,
        screen_after_n
    );

    // Press n again to reach the second hunk, then p to go back
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
    }
    let screen_at_second = harness.screen_to_string();
    let ln_second =
        extract_ln(&screen_at_second).expect("status bar should show Ln at second hunk");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::NONE)
        .unwrap();
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
    }
    let screen_after_p = harness.screen_to_string();
    let ln_after_p = extract_ln(&screen_after_p).expect("status bar should show Ln after p");
    assert!(
        ln_after_p < ln_second,
        "BUG-4: Pressing `p` should move cursor back to previous hunk. \
         Ln at_second={}, Ln after_p={}",
        ln_second,
        ln_after_p
    );
}

/// Verify that the status bar reads cursor position from the inner panel leaf
/// (via effective_active_split) rather than the outer split. After the fix,
/// the status bar correctly shows "*diff*" as the buffer name and the cursor
/// line number from the diff panel's view state.
#[test]
fn test_set_buffer_cursor_updates_status_bar_for_panel_buffer() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_multi_hunk_file();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("HUNK_ONE"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Switch focus to the diff panel with Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The status bar should show "*diff*" (the inner panel buffer name),
    // NOT the outer split's buffer name.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("*diff*"),
        "Status bar should display the inner panel buffer name '*diff*' \
         when a buffer group panel is focused. Screen:\n{}",
        screen
    );

    // Pressing 'n' should jump to the next hunk and update the status bar
    // line number — this verifies both the cursor movement and the status
    // bar reading from effective_active_split().
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    fn extract_ln(screen: &str) -> Option<usize> {
        screen.lines().find_map(|l| {
            if let Some(idx) = l.find("Ln ") {
                let rest = &l[idx + 3..];
                let num_str: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
                num_str.parse().ok()
            } else {
                None
            }
        })
    }

    let screen_at_home = harness.screen_to_string();
    let ln_at_home = extract_ln(&screen_at_home).expect("status bar should show Ln");

    // Press n twice to navigate through hunks (first n → first hunk,
    // second n → second hunk)
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
            .unwrap();
        for _ in 0..10 {
            harness.tick_and_render().unwrap();
        }
    }

    let screen_after = harness.screen_to_string();
    let ln_after = extract_ln(&screen_after).expect("status bar should show Ln");

    println!(
        "Panel cursor test: Ln at_home={}, Ln after_2n={}",
        ln_at_home, ln_after
    );
    assert!(
        ln_after > ln_at_home,
        "Hunk navigation should move the cursor forward. \
         Ln at_home={}, Ln after_2n={}",
        ln_at_home,
        ln_after
    );
}

// ---------------------------------------------------------------------------
// BUG-5: Side-by-side drill-down fails for deleted files
// ---------------------------------------------------------------------------

/// BUG-5: Pressing Enter on a deleted file shows "Loading side-by-side
/// diff..." indefinitely because readFile returns null for deleted files.
#[test]
fn test_bug5_deleted_file_drill_down_hangs() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Delete a tracked file to create a "D" status entry
    let utils_path = repo.path.join("src/utils.rs");
    fs::remove_file(&utils_path).expect("delete file");

    // Also modify a file so there's something to open the editor with
    let main_rs = repo.path.join("src/main.rs");
    fs::write(&main_rs, "fn main() { /* modified */ }\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("modified"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Navigate to find the deleted file (utils.rs with D status)
    // Navigate down through the file list to find it
    let mut found_deleted = false;
    for _ in 0..15 {
        let s = harness.screen_to_string();
        if s.contains("utils.rs") {
            found_deleted = true;
            break;
        }
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    if !found_deleted {
        // If utils.rs isn't visible, skip — the git status may not list it
        println!("BUG-5: Deleted file not found in review diff list, skipping");
        return;
    }

    // Navigate to the deleted file entry
    for _ in 0..15 {
        let s = harness.screen_to_string();
        // Check if the diff panel shows utils.rs
        if s.contains("DIFF FOR") && s.contains("utils.rs") {
            break;
        }
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // Press Enter to drill down into side-by-side view for the deleted file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Give it a moment for the async operation
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
    }

    let screen = harness.screen_to_string();
    println!("BUG-5 deleted file drill-down:\n{}", screen);

    // The view should either show the diff or show an error — but NOT be
    // stuck on "Loading side-by-side diff..." forever
    assert!(
        !screen.contains("Loading side-by-side diff"),
        "BUG-5: Drill-down on deleted file is stuck on 'Loading side-by-side \
         diff...' — readFile returns null for deleted files. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// BUG-6: Comments added from files panel never display inline
// ---------------------------------------------------------------------------

/// BUG-6: Comments added while the files panel is focused are stored as
/// hunk-level comments with no line info, so they never render inline
/// in the diff view.
#[test]
fn test_bug6_comment_from_files_panel_not_visible_in_diff() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // With files panel focused (default), press c to add a comment
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();

    // Wait for comment prompt
    harness.wait_until(|h| h.editor().is_prompting()).unwrap();

    // Type a distinctive comment
    harness.type_text("BUG6_TEST_COMMENT").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    // Now switch to diff panel to look for the comment inline
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("BUG-6 screen after comment:\n{}", screen);

    // The comment should be visible somewhere in the diff panel
    assert!(
        screen.contains("BUG6_TEST_COMMENT"),
        "BUG-6: Comment added from files panel should be visible inline in \
         the diff view, but it is not rendered. Comments with no line info \
         are skipped by pushLineComments(). Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// BUG-7: Escape does not exit File Explorer focus
// ---------------------------------------------------------------------------

/// BUG-7: When the File Explorer has focus, pressing Escape should
/// transfer focus back to the editor/review diff, but it has no effect.
#[test]
fn test_bug7_escape_does_not_exit_file_explorer_focus() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Open File Explorer
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Verify File Explorer is focused: pressing Down should move its cursor
    let _screen_before = harness.screen_to_string();

    // Press Escape to try to leave File Explorer focus
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now press `j` — if Escape worked, this should go to the editor.
    // If Escape did NOT work, the File Explorer still has focus and `j`
    // triggers its quick-search.
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("BUG-7 screen after Escape + j:\n{}", screen_after);

    // If File Explorer still has focus, its title changes to "/j"
    assert!(
        !screen_after.contains("/j"),
        "BUG-7: Escape should exit File Explorer focus, but it did not. \
         `j` was still captured by the File Explorer quick-search. Screen:\n{}",
        screen_after
    );
}

// ---------------------------------------------------------------------------
// BUG-9: Down arrow doesn't scroll viewport in side-by-side view
// ---------------------------------------------------------------------------

/// BUG-9: In the side-by-side diff view, pressing Down updates the status
/// bar line number but the viewport doesn't scroll to follow the cursor.
#[test]
fn test_bug9_side_by_side_down_arrow_no_viewport_scroll() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create a file with many lines so we can scroll past the viewport
    let main_rs = repo.path.join("src/main.rs");
    let mut content = String::from("fn main() {\n");
    for i in 0..60 {
        content.push_str(&format!("    println!(\"Line {}\"); // LONG_FILE\n", i));
    }
    content.push_str("}\n");
    fs::write(&main_rs, &content).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        30, // Small viewport to force scrolling
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("LONG_FILE"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Drill down into side-by-side view
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            (s.contains("OLD (HEAD)") || s.contains("*Diff:"))
                && !s.contains("Loading side-by-side diff")
        })
        .unwrap();

    let screen_initial = harness.screen_to_string();

    // Press Down many times to move past the visible viewport
    for _ in 0..25 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let screen_after_down = harness.screen_to_string();
    println!("BUG-9 initial:\n{}", screen_initial);
    println!("BUG-9 after 25 Downs:\n{}", screen_after_down);

    // The viewport content should have changed (scrolled) after moving
    // the cursor 25 lines down. If the viewport didn't scroll, the visible
    // lines will be identical to the initial state.
    // Compare the content area (skip status bar which will differ)
    let initial_content: Vec<&str> = screen_initial.lines().take(20).collect();
    let after_content: Vec<&str> = screen_after_down.lines().take(20).collect();

    assert_ne!(
        initial_content, after_content,
        "BUG-9: After pressing Down 25 times in side-by-side view, the \
         viewport should have scrolled to follow the cursor, but the \
         displayed content is unchanged. Screen:\n{}",
        screen_after_down
    );
}

// ---------------------------------------------------------------------------
// BUG-10: Toolbar "Export" label truncated
// ---------------------------------------------------------------------------

/// BUG-10: With the File Explorer sidebar open (narrower viewport), the
/// toolbar's `e Export` hint is truncated.
#[test]
fn test_bug10_toolbar_export_label_truncated() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100, // Narrow enough to trigger truncation with explorer open
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Open File Explorer to narrow the review diff viewport
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Switch focus away from File Explorer so review diff keys work
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Re-open File Explorer
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    let screen = harness.screen_to_string();
    println!("BUG-10 screen:\n{}", screen);

    // Check if "Export" appears in the toolbar area
    // The toolbar shows hints like "s Stage  u Unstage  d Discard ... e Export"
    let has_full_export_label = screen.contains("e Export") || screen.contains("Export");

    // This is a cosmetic bug — we just note if the label is truncated
    assert!(
        has_full_export_label,
        "BUG-10: The toolbar 'Export' label is truncated when the File Explorer \
         narrows the viewport. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// Additional UX tests
// ---------------------------------------------------------------------------

/// After opening an embedded terminal, opening Review Diff should still work:
/// cursor movement and Tab panel switching should function correctly.
#[test]
fn test_review_diff_works_after_terminal_opened() {
    init_tracing_from_env();

    // Check PTY availability — skip test if not available
    if portable_pty::native_pty_system()
        .openpty(portable_pty::PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping test: PTY not available");
        return;
    }

    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();

    // Open an embedded terminal via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Open Terminal").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Give terminal time to start
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
    }

    // Now open Review Diff
    let screen = open_review_diff(&mut harness);
    assert!(
        screen.contains("GIT STATUS"),
        "Review Diff should open successfully after terminal. Screen:\n{}",
        screen
    );

    // Tab should switch focus between file list and diff panels
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness.tick_and_render().unwrap();
    }
    let screen_after_tab = harness.screen_to_string();
    // After Tab, either the files or diff panel should be focused.
    // Verify we're still in Review Diff (not switched to terminal tab).
    assert!(
        screen_after_tab.contains("Review Diff") || screen_after_tab.contains("GIT STATUS"),
        "Tab in Review Diff mode should toggle panels, not switch tabs. Screen:\n{}",
        screen_after_tab
    );

    // Down arrow should work for navigation (file list selection)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // Verify the screen didn't produce an error
    let screen_after_move = harness.screen_to_string();
    assert!(
        !screen_after_move.contains("TypeError") && !screen_after_move.contains("Error:"),
        "Cursor movement should not produce errors after terminal was opened. Screen:\n{}",
        screen_after_move
    );
}

/// When the file list in Review Diff has more files than the visible height,
/// moving down/up beyond the view should auto-scroll.
#[test]
fn test_review_diff_file_list_auto_scrolls() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create many modified files to exceed the viewport
    for i in 0..20 {
        let path = repo.path.join(format!("src/mod_{}.rs", i));
        fs::write(&path, format!("fn func_{}() {{}}\n", i)).unwrap();
    }

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        20, // Short viewport to force scrolling
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    let first_file = repo.path.join("src/mod_0.rs");
    harness.open_file(&first_file).unwrap();
    harness.render().unwrap();

    let _screen = open_review_diff(&mut harness);

    // Record the initial file list content
    let screen_initial = harness.screen_to_string();
    println!("File list scroll test: initial screen (20 files, 20 rows)");

    // Move down many times to go past the visible area in the files panel
    for _ in 0..15 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let screen_after = harness.screen_to_string();
    println!("File list scroll test: after 15x Down");

    // The visible file list content should have changed (scrolled)
    // Compare a few content lines — if auto-scroll works, we should see
    // different file names than initially.
    let initial_lines: Vec<&str> = screen_initial.lines().take(15).collect();
    let after_lines: Vec<&str> = screen_after.lines().take(15).collect();

    assert_ne!(
        initial_lines, after_lines,
        "File list should auto-scroll when moving cursor past visible area. \
         The visible content didn't change after 15 Down presses."
    );
}

