//! Regression tests for issue #2119: mouse-wheel scrolling is broken in the
//! File Explorer, Live Grep overlay, and Git Log panel.
//!
//! These tests are written to FAIL against the current implementation and
//! PASS once the corresponding fix lands. Each drives a real mouse-wheel
//! event and asserts on rendered output only (CONTRIBUTING §2).
//!
//! Bugs covered:
//!  1. File Explorer — the wheel moves the *selected* entry (in jumps of the
//!     wheel step), dragging the viewport, instead of scrolling the view and
//!     leaving the selection alone.
//!  2. Live Grep results list — the wheel moves the selected result (in
//!     jumps), instead of scrolling the list smoothly.
//!  3. Live Grep preview pane — the wheel over the preview moves the *results*
//!     selection instead of scrolling the preview.
//!  4. Git Log commit list — the wheel does not scroll the list at all.

use crate::common::explorer::{first_explorer_token, token_after, token_on_line_with};
use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

/// Pump async work until the screen stops changing.
fn settle(harness: &mut EditorTestHarness) {
    harness.wait_until_stable(|_| true).unwrap();
}

/// The `prefix`-token on the first screen line that carries it (reading
/// order). Local to the Live Grep cases below; the explorer-shaped readers
/// they sit beside are shared from `common::explorer`.
fn first_token(screen: &str, prefix: &str) -> Option<String> {
    screen.lines().find_map(|l| token_after(l, prefix))
}

// ---------------------------------------------------------------------------
// Bug 1: File Explorer wheel moves the selection instead of scrolling the view
// ---------------------------------------------------------------------------

/// With the explorer focused and more entries than fit on screen, one
/// wheel-down over the explorer should scroll the view while leaving the
/// selected entry unchanged. The buggy behaviour moves the selection by the
/// wheel step (so the `▌` cursor jumps to a different file) and only scrolls
/// to follow it.
#[test]
fn file_explorer_wheel_scrolls_view_without_moving_selection() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Flat list of zero-padded names so the sort order is numeric and there
    // are far more entries than the explorer viewport can show.
    for i in 0..40 {
        fs::write(project_root.join(format!("file_{i:02}.txt")), "x").unwrap();
    }

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("file_00.txt").unwrap();

    // Move the selection down into the middle of the viewport so there are
    // entries both above (to scroll off the top) and below it.
    for _ in 0..12 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let before = harness.screen_to_string();
    let selected_before = token_on_line_with(&before, "▌", "file_")
        .expect("a file should be selected (▌ marker) before scrolling");
    let top_before =
        first_explorer_token(&before, "file_").expect("at least one file visible before scrolling");

    // One wheel notch over the explorer (column 3 is inside the left sidebar).
    harness.mouse_scroll_down(3, 8).unwrap();
    harness.render().unwrap();

    let after = harness.screen_to_string();
    let selected_after = token_on_line_with(&after, "▌", "file_")
        .expect("a file should still be selected after scrolling");
    let top_after =
        first_explorer_token(&after, "file_").expect("at least one file visible after scrolling");

    assert_eq!(
        selected_after, selected_before,
        "wheel over the file explorer must NOT move the selection \
         (was {selected_before}, now {selected_after}). Screen:\n{after}"
    );
    assert_ne!(
        top_after, top_before,
        "wheel over the file explorer should scroll the view \
         (top entry should change from {top_before}). Screen:\n{after}"
    );
}

// ---------------------------------------------------------------------------
// Live Grep helpers
// ---------------------------------------------------------------------------

/// A unique token on line 1 of each grep target, shown in the *preview* pane
/// (the matched line is line 2). Identifies which result is selected/previewed
/// while the preview is at the top of the file.
const PREVIEW_MARKER: &str = "ZTOPMARKER";
/// A unique token deep inside each grep target (well below the preview's
/// initial viewport). Only becomes visible after the preview scrolls down, and
/// — being per-file — also confirms which file is still selected.
const DEEP_MARKER: &str = "ZDEEPMARKER";
const NEEDLE: &str = "NEEDLEZZ";

/// Build a git repo with `n` files, each matching NEEDLE, plus the live_grep
/// plugin, and open the Live Grep overlay on a NEEDLE search. Returns the
/// harness with results showing.
fn open_live_grep_with_results(width: u16, height: u16, n: usize) -> EditorTestHarness {
    let repo = GitTestRepo::new();
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    for i in 0..n {
        // Line 1: preview-only top marker. Line 2: the searchable needle.
        // Many filler lines, then a deep per-file marker far below the
        // preview's initial viewport so it only appears once the preview
        // scrolls down.
        let mut content = format!("{PREVIEW_MARKER}{i:02}\n{NEEDLE} on line two\n");
        for f in 0..80 {
            content.push_str(&format!("filler line {f:02}\n"));
        }
        content.push_str(&format!("{DEEP_MARKER}{i:02}\n"));
        repo.create_file(&format!("match_{i:02}.txt"), &content);
    }
    repo.git_add_all();
    repo.git_commit("seed matches");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    // Keep the repo alive for the lifetime of the harness.
    std::mem::forget(repo);

    // Open palette → run Live Grep.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Live Grep").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Search in:"))
        .unwrap();

    // Type the needle and wait for results + a preview to render.
    harness.type_text(NEEDLE).unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("match_00.txt") && s.contains(PREVIEW_MARKER)
        })
        .unwrap();
    settle(&mut harness);
    harness
}

// ---------------------------------------------------------------------------
// Bug 2: Live Grep results-list wheel moves the selection
// ---------------------------------------------------------------------------

/// Wheeling over the results list should scroll the list, not move the
/// selected result. The preview reflects the selected result, so a stable
/// preview marker before/after the wheel proves the selection did not move.
#[test]
fn live_grep_results_wheel_scrolls_without_moving_selection() {
    // More results than the result pane can show, so the list is scrollable.
    let mut harness = open_live_grep_with_results(160, 40, 60);

    let before = harness.screen_to_string();
    let selected_before = first_token(&before, PREVIEW_MARKER)
        .expect("the preview should show the selected result's marker");
    let top_result_before =
        first_token(&before, "match_").expect("the result list should show results");

    // Wheel over the results list (left pane).
    harness.mouse_scroll_down(10, 12).unwrap();
    settle(&mut harness);

    let after = harness.screen_to_string();
    let selected_after = first_token(&after, PREVIEW_MARKER)
        .expect("the preview should still show a marker after scrolling");
    let top_result_after =
        first_token(&after, "match_").expect("the result list should still show results");

    assert_eq!(
        selected_after, selected_before,
        "wheel over the Live Grep results list must NOT move the selected \
         result (preview was {selected_before}, now {selected_after}). Screen:\n{after}"
    );
    assert_ne!(
        top_result_after, top_result_before,
        "wheel over the Live Grep results list should scroll the list \
         (top result should change from {top_result_before}). Screen:\n{after}"
    );
}

// ---------------------------------------------------------------------------
// Bug 3: Live Grep preview wheel moves the results selection
// ---------------------------------------------------------------------------

/// Wheeling while the pointer is over the preview pane should scroll the
/// preview, not the results list. The bug routes the wheel to the results
/// selection, so the preview jumps to a different file. We assert the
/// previewed file (its marker) is unchanged.
#[test]
fn live_grep_preview_wheel_scrolls_preview_not_results() {
    let mut harness = open_live_grep_with_results(160, 40, 60);

    let before = harness.screen_to_string();
    // Which file is selected/previewed (from the top marker, visible before
    // any preview scroll). The deep marker must NOT be visible yet.
    let selected = first_token(&before, PREVIEW_MARKER)
        .expect("the preview should show the selected result's top marker");
    let idx = &selected[PREVIEW_MARKER.len()..]; // e.g. "00"
    assert!(
        !before.contains(DEEP_MARKER),
        "deep marker must be below the preview's initial viewport. Screen:\n{before}"
    );

    // Wheel over the preview pane (right half of the overlay), enough to bring
    // the deep marker into view.
    for _ in 0..20 {
        harness.mouse_scroll_down(140, 12).unwrap();
    }
    settle(&mut harness);

    let after = harness.screen_to_string();
    // The preview scrolled (deep marker now visible) AND it's still the SAME
    // file's deep marker — i.e. the results selection did not move. A single
    // assertion captures both: a different file would show a different index,
    // and an unscrolled preview wouldn't show any deep marker.
    let expected_deep = format!("{DEEP_MARKER}{idx}");
    assert!(
        after.contains(&expected_deep),
        "wheel over the preview should scroll the preview to reveal {expected_deep} \
         (same file, scrolled) — not move the results selection. Screen:\n{after}"
    );
}

// ---------------------------------------------------------------------------
// Bug 4: Git Log commit list does not scroll with the wheel
// ---------------------------------------------------------------------------

/// With more commits than fit on screen, wheeling over the Git Log commit
/// list (left pane) should scroll it. The bug ignores the wheel entirely, so
/// the topmost visible commit never changes.
#[test]
fn git_log_commit_list_scrolls_with_wheel() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_log_plugin();

    // Many uniquely-named commits so the list overflows the viewport.
    for i in 0..40 {
        repo.create_file("churn.txt", &format!("rev {i}\n"));
        repo.git_add_all();
        repo.git_commit(&format!("wheelcommit_{i:02}"));
    }

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        24,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&repo.path.join("src/main.rs")).unwrap();
    harness.render().unwrap();

    // Open Git Log via the command palette.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Git Log").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("switch pane") && s.contains("wheelcommit_")
        })
        .unwrap();
    settle(&mut harness);

    let before = harness.screen_to_string();
    let top_before = first_token(&before, "wheelcommit_")
        .expect("a commit should be visible in the git log list");

    // Wheel down over the commit list (left pane).
    for _ in 0..4 {
        harness.mouse_scroll_down(10, 8).unwrap();
    }
    settle(&mut harness);

    let after = harness.screen_to_string();
    let top_after = first_token(&after, "wheelcommit_")
        .expect("a commit should still be visible after scrolling");

    assert_ne!(
        top_after, top_before,
        "wheel over the Git Log commit list should scroll it \
         (top commit should change from {top_before}). Screen:\n{after}"
    );
}

// ---------------------------------------------------------------------------
// Keyboard navigation after a wheel scroll must re-reveal the selection
// ---------------------------------------------------------------------------

/// In Live Grep, after the result list is wheel-scrolled so the selected
/// result is off-screen, the next keyboard navigation (Down) must scroll the
/// list so the (newly) selected result is visible again.
#[test]
fn live_grep_keyboard_nav_scrolls_selection_back_into_view() {
    let mut harness = open_live_grep_with_results(160, 40, 60);

    // Selection starts on the first result.
    let initial = harness.screen_to_string();
    assert!(
        initial.contains("match_00.txt"),
        "first result should be visible initially. Screen:\n{initial}"
    );

    // Wheel the results down until the selected result (match_00) scrolls off.
    for _ in 0..5 {
        harness.mouse_scroll_down(10, 12).unwrap();
    }
    settle(&mut harness);
    let scrolled = harness.screen_to_string();
    assert!(
        !scrolled.contains("match_00.txt"),
        "precondition: the selected result should have scrolled out of view. \
         Screen:\n{scrolled}"
    );

    // Keyboard Down moves the selection (0 → 1) and must bring it back in view.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    settle(&mut harness);
    let after = harness.screen_to_string();
    assert!(
        after.contains("match_01.txt"),
        "keyboard navigation after a wheel scroll should scroll the selected \
         result back into view. Screen:\n{after}"
    );
}

/// In the File Explorer, after the tree is wheel-scrolled so the selected
/// entry is off-screen, the next keyboard navigation (Down) must scroll the
/// view so the (newly) selected entry is visible again (its `▌` cursor shows).
#[test]
fn file_explorer_keyboard_nav_scrolls_selection_back_into_view() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    for i in 0..60 {
        fs::write(project_root.join(format!("file_{i:02}.txt")), "x").unwrap();
    }

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("file_00.txt").unwrap();

    // Select an entry near the top of the list.
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    let selected = token_on_line_with(&harness.screen_to_string(), "▌", "file_")
        .expect("an entry should be selected before scrolling");

    // Wheel the explorer down until the selected entry scrolls off the top
    // (its `▌` cursor is no longer drawn).
    for _ in 0..10 {
        harness.mouse_scroll_down(3, 8).unwrap();
    }
    harness.render().unwrap();
    assert!(
        token_on_line_with(&harness.screen_to_string(), "▌", "file_").is_none(),
        "precondition: the selected entry should have scrolled out of view. \
         Screen:\n{}",
        harness.screen_to_string()
    );

    // Keyboard Down moves the selection and must scroll it back into view.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let after = harness.screen_to_string();
    let selected_after = token_on_line_with(&after, "▌", "file_").unwrap_or_else(|| {
        panic!(
            "keyboard navigation should scroll the selected entry back into view. Screen:\n{after}"
        )
    });
    assert_ne!(
        selected_after, selected,
        "selection should have advanced by one entry (was {selected})"
    );
}
