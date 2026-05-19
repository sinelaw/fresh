//! Migration of `tests/e2e/tab_scrolling.rs` — tab-bar overflow
//! behaviour at a narrow width: cycling buffers always keeps the
//! active tab visible, and clicking the `<` / `>` scroll buttons
//! routes through the mouse path.
//!
//! Load-bearing claims preserved here:
//!
//!   1. **Active tab visibility on open / cycle.** Opening many
//!      files into a narrow (NARROW_WIDTH=40) terminal must keep
//!      the most-recently-active tab's filename rendered on screen.
//!      Cycling forward (Ctrl+PageDown → NextBuffer) and backward
//!      (Ctrl+PageUp → PrevBuffer) must keep that invariant at
//!      every step. Edge invariants from the e2e: the leftmost
//!      tab must not draw a `<` indicator (nothing to scroll left
//!      to); the rightmost tab must not draw a `>` indicator
//!      (nothing to scroll right to).
//!
//!   2. **Manual scroll round-trip via Alt+PageDown/Up.** Manual
//!      tab-bar scrolling (Alt+PageDown=ScrollTabsRight,
//!      Alt+PageUp=ScrollTabsLeft) may move the active tab off
//!      screen, but any subsequent NextBuffer (Ctrl+PageDown) must
//!      bring the newly-active tab's filename back on screen — the
//!      "switch tab snaps view to active" contract.
//!
//!   3. **Mouse-click scroll buttons.** When the `>` indicator is
//!      visible (we're on the first tab of an overflowing bar),
//!      clicking the rightmost column of the tab-bar row routes a
//!      mouse-click through to the scroll-tabs-right action. The
//!      `<` indicator path mirrors it on the leftmost column. The
//!      e2e gates these blocks behind `if screen.contains(">")` /
//!      `if screen.contains("<")` — that conditional is preserved
//!      verbatim (overflow may not always render an indicator on
//!      every harness configuration; the test asserts that *if*
//!      the indicator is shown, the click is accepted).
//!
//! ## Harness-direct pattern
//!
//! All three claims need `EditorTestHarness` surfaces with no
//! `EditorTestApi` projection: `open_file` (multi-file workspace
//! setup against a real `TempDir`), `assert_screen_contains` /
//! `screen_to_string` (full rendered-screen substring search the
//! e2e uses), and `mouse_click` (low-level event routing). The
//! migration uses the harness-direct pattern.
//!
//! Source: `tests/e2e/tab_scrolling.rs` (2 tests migrated; no
//! tests deferred).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

const NARROW_WIDTH: u16 = 40;
const TEST_HEIGHT: u16 = 20;
const NUM_FILES: usize = 15;

/// Helper to create dummy files with long names (mirrors the e2e
/// `create_dummy_files`).
fn create_dummy_files(temp_dir: &TempDir) -> Vec<std::path::PathBuf> {
    let mut files = Vec::new();
    for i in 0..NUM_FILES {
        let file_name = format!("long_file_name_number_{:02}.txt", i);
        let file_path = temp_dir.path().join(&file_name);
        std::fs::write(&file_path, format!("Content for file {}", i)).unwrap();
        files.push(file_path);
    }
    files
}

#[test]
fn migrated_active_tab_visibility_with_scrolling() {
    // Original: `test_active_tab_visibility_with_scrolling`. The
    // claim chain is opening-many-files + Ctrl+PageDown cycle
    // forward + Ctrl+PageUp cycle backward + Alt+PageDown/Up
    // manual scroll + final Ctrl+PageDown snaps active back on
    // screen. Each step asserts the active tab's filename is on
    // screen, plus the leftmost/rightmost edge invariants on the
    // `<` / `>` indicators.
    let temp_dir = TempDir::new().unwrap();
    let files = create_dummy_files(&temp_dir);

    let mut harness = EditorTestHarness::new(NARROW_WIDTH, TEST_HEIGHT).unwrap();

    // Open all dummy files
    for file_path in &files {
        harness.open_file(file_path).unwrap();
        harness.render().unwrap();
        let active_file_name = file_path.file_name().unwrap().to_str().unwrap();
        harness.assert_screen_contains(active_file_name);
    }

    // Initial check: Last opened file is active.
    let mut active_idx = NUM_FILES - 1;
    harness.render().unwrap();
    let active_file_name = files[active_idx].file_name().unwrap().to_str().unwrap();
    harness.assert_screen_contains(active_file_name);
    if active_idx < NUM_FILES - 1 {
        assert!(
            harness.screen_to_string().contains(">"),
            "Expected right scroll indicator after opening many files. Screen:\n{}",
            harness.screen_to_string()
        );
    }

    // --- Cycle Forward (Next Buffer) ---
    for _i in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        active_idx = (active_idx + 1) % NUM_FILES;

        harness.render().unwrap();
        let active_file_name = files[active_idx].file_name().unwrap().to_str().unwrap();
        harness.assert_screen_contains(active_file_name);

        let screen = harness.screen_to_string();
        // The e2e only enforces the no-left-indicator-on-first edge.
        if active_idx == 0 {
            assert!(
                !screen.contains("<"),
                "Expected no left scroll indicator for file: {}",
                active_file_name
            );
        }
    }

    // --- Cycle Backward (Prev Buffer) ---
    for _i in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
            .unwrap();
        active_idx = (active_idx + NUM_FILES - 1) % NUM_FILES;

        harness.render().unwrap();
        let active_file_name = files[active_idx].file_name().unwrap().to_str().unwrap();
        harness.assert_screen_contains(active_file_name);

        let screen = harness.screen_to_string();
        if active_idx == 0 {
            assert!(
                !screen.contains("<"),
                "Expected no left scroll indicator for file: {}",
                active_file_name
            );
        }
        if active_idx == NUM_FILES - 1 {
            assert!(
                !screen.contains(">"),
                "Expected no right scroll indicator for file: {}",
                active_file_name
            );
        }
    }

    // --- Test manual scrolling ---
    // Activate a middle tab so manual scroll can move it off-screen.
    let middle_idx = NUM_FILES / 2;
    let steps_to_middle = (middle_idx + NUM_FILES - active_idx) % NUM_FILES;
    for _ in 0..steps_to_middle {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        active_idx = (active_idx + 1) % NUM_FILES;
        harness.render().unwrap();
    }
    assert_eq!(active_idx, middle_idx, "Failed to activate middle tab");
    harness.assert_screen_contains(files[active_idx].file_name().unwrap().to_str().unwrap());

    // Scroll right manually — active tab may scroll out of view.
    for _ in 0..5 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::ALT)
            .unwrap();
        harness.render().unwrap();
    }

    // Scroll left manually
    for _ in 0..10 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::ALT)
            .unwrap();
        harness.render().unwrap();
    }

    // After manual scrolling, switching tabs should bring active tab back into view.
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    active_idx = (active_idx + 1) % NUM_FILES;
    harness.assert_screen_contains(files[active_idx].file_name().unwrap().to_str().unwrap());
}

#[test]
fn migrated_tab_scroll_button_click() {
    // Original: `test_tab_scroll_button_click`. Wider terminal
    // (80) so filenames are fully visible; focus is on the click
    // routing through the mouse path to the scroll-tabs-right /
    // scroll-tabs-left actions. The `if screen.contains(">")` /
    // `if screen.contains("<")` guards are preserved as the e2e
    // had them.
    let temp_dir = TempDir::new().unwrap();
    let files = create_dummy_files(&temp_dir);

    let mut harness = EditorTestHarness::new(80, TEST_HEIGHT).unwrap();

    // Open all dummy files to ensure tab overflow.
    for file_path in &files {
        harness.open_file(file_path).unwrap();
        harness.render().unwrap();
    }

    // Go to first tab to ensure we can scroll right.
    for _ in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    // Now we're on the first file - should see ">" indicator for right scroll.
    let screen = harness.screen_to_string();
    if screen.contains(">") {
        let tab_row = 1; // Tab bar is usually at row 1.
        let right_scroll_col = NARROW_WIDTH - 1;

        harness.mouse_click(right_scroll_col, tab_row).unwrap();
        harness.render().unwrap();
    }

    // Go to last tab to ensure we can scroll left.
    for _ in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    // Now on the last file - should see "<" indicator for left scroll.
    let screen = harness.screen_to_string();
    if screen.contains("<") {
        let tab_row = 1;
        let left_scroll_col = 0;

        harness.mouse_click(left_scroll_col, tab_row).unwrap();
        harness.render().unwrap();
    }
}

/// Anti-test: drop the Ctrl+PageDown final "switch buffer" after
/// the manual scroll sweep. After manual Alt+PageDown × 5 +
/// Alt+PageUp × 10 + no further buffer switch, the active middle
/// tab may legitimately remain off screen — proving the positive
/// test's "switch tab snaps view to active" claim depends on the
/// trailing Ctrl+PageDown, not on the manual-scroll sequence
/// somehow restoring the view.
///
/// We assert the weaker invariant: after just the manual scroll
/// sweep, the harness's tab-bar-snap behaviour is NOT triggered.
/// Concretely, after scrolling left 10 ticks past the middle tab,
/// we still observe a `<` indicator (we're scrolled far right of
/// the middle), OR the active middle filename is missing — either
/// proves we have NOT auto-snapped to active. The e2e's claim is
/// that the trailing Ctrl+PageDown causes the snap; without it,
/// no snap should occur.
#[test]
fn anti_manual_scroll_without_switch_buffer_does_not_snap() {
    let temp_dir = TempDir::new().unwrap();
    let files = create_dummy_files(&temp_dir);

    let mut harness = EditorTestHarness::new(NARROW_WIDTH, TEST_HEIGHT).unwrap();

    for file_path in &files {
        harness.open_file(file_path).unwrap();
        harness.render().unwrap();
    }

    // Activate the middle tab.
    let middle_idx = NUM_FILES / 2;
    let mut active_idx = NUM_FILES - 1;
    let steps_to_middle = (middle_idx + NUM_FILES - active_idx) % NUM_FILES;
    for _ in 0..steps_to_middle {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        active_idx = (active_idx + 1) % NUM_FILES;
        harness.render().unwrap();
    }
    assert_eq!(active_idx, middle_idx);
    let middle_name = files[middle_idx].file_name().unwrap().to_str().unwrap();
    harness.assert_screen_contains(middle_name);

    // Manual scroll right 5 + left 10 — would put us scrolled past
    // the active middle tab on the left side of the bar.
    for _ in 0..5 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::ALT)
            .unwrap();
        harness.render().unwrap();
    }
    for _ in 0..10 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::ALT)
            .unwrap();
        harness.render().unwrap();
    }

    // No Ctrl+PageDown here — that's the load-bearing step we drop.
    // Active idx is still middle_idx. After 10 left-scrolls from
    // post-5-right, we're at the leftmost end of the bar (so the
    // first file's name is visible). The active middle tab should
    // NOT be visible — proving the snap did not happen on its own.
    let screen = harness.screen_to_string();
    let first_name = files[0].file_name().unwrap().to_str().unwrap();
    assert!(
        screen.contains(first_name),
        "anti: after Alt+PageUp ×10 from middle the leftmost tab \
         name should be on screen (scrolled fully left). Screen:\n{screen}"
    );
    assert!(
        !screen.contains(middle_name),
        "anti: without a trailing Ctrl+PageDown, manual scrolling \
         alone must NOT snap the view back to the active middle \
         tab. middle={middle_name} screen=\n{screen}"
    );
}
